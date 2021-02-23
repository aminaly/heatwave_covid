# we're working just for august temps and september mortality rn
# we want to see how counties are responding to movement 

## basic setup
ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


library(dplyr)
library(lfe)
library(ggplot2)
library(dotwhisker)zo
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)
library(ggsignif)
library(sf)


##### Work on sheltering ##### 
all_files <- list.files("heatwaves_manual/safegraph/2020/07", full.names = T, recursive = T)
all_files <- c(all_files, list.files("heatwaves_manual/safegraph/2020/08", full.names = T, recursive = T))
all_files <- c(all_files, list.files("heatwaves_manual/safegraph/2020/09", full.names = T, recursive = T))


combined <- data.frame()

for(file in all_files) {
  
  f <- read_csv(file)
  f <- f %>% select(origin_census_block_group, date_range_start, date_range_end, completely_home_device_count, device_count)
  combined <- bind_rows(combined, f)
  
}

saveRDS(combined, "heatwaves_manual/Jul-Sept_shelter_data_long.RDS")
combined$fips <- str_sub(combined$origin_census_block_group, 1,5)

sheltering <- combined %>% group_by(fips, date_range_start) %>% 
  summarise(home_device = sum(completely_home_device_count, na.rm = T), 
            all_devices = sum(device_count, na.rm = T)) %>% 
   mutate(date = as.Date(date_range_start))

sheltering$shelter_index <- (sheltering$home_device / sheltering$all_devices)*100

#just covid deaths
#covid <- read_csv("heatwaves_manual/deaths_2020-11-12.csv")

##### Work on mortality ##### 

### lets grab  the mortality for august 
#start <- read_csv("cdc covid/8_5_Death_Counts.csv")
start <- read_csv("cdc covid/9_9_Death_Counts.csv")
end <- read_csv("cdc covid/10_7_Death_Counts.csv")

combined_covid <- left_join(start, end, by = "FIPS County Code")
combined_covid <- combined_covid %>% select(date_start = `Date as of.x`, date_end = `Date as of.y`, 
                                            state = `State.x`, county = `County name.x`, fips = `FIPS County Code`,
                                            deaths_covid_start = `Deaths involving COVID-19.x`, deaths_all_start = `Deaths from All Causes.x`,
                                            deaths_covid_end = `Deaths involving COVID-19.y`, deaths_all_end = `Deaths from All Causes.y`)
combined_covid$all_deaths_aug <- combined_covid$deaths_all_end - combined_covid$deaths_all_start
combined_covid$covid_deaths_aug <- combined_covid$deaths_covid_end - combined_covid$deaths_covid_start
combined_covid <- combined_covid %>% mutate(fips = ifelse(nchar(fips) == 4, paste0(0, fips), fips))
mortality <- combined_covid

##### Work on temps ##### 
## filter temps for aug 2020
t <- read_rds("./heatwaves_manual/all_temperature_data_clean_nov.rds")

## calculate percentiles for temps in 2020 (this is written somewhere just find it)
t_zs <- t %>% group_by(fips, year) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

#filter out just 2020 just to make calculations faster
t_zs_20 <- t_zs %>% dplyr::filter(year == 2020)

# list of all counties with temperatures above 90th percentile with the date 
hot_days <- t_zs_20 %>% dplyr::filter(p_low >= .9) %>% mutate(week_before = date - 7)

# join with sheltering
hot_days_sheltering <- na.omit(left_join(hot_days, sheltering, by = c("fips", "date"))) %>% 
  select(-c(date_range_start, home_device, all_devices))
  
#match up with one week prior too
previous_sheltering <- na.omit(left_join(hot_days, sheltering, by = c("week_before" = "date", "fips"))) %>% 
  rename(shelter_index_prior = shelter_index) %>% 
  select(date, shelter_index_prior, fips)

hot_days_sheltering <- na.omit(left_join(hot_days_sheltering, previous_sheltering, by = c("fips", "date")))

# % difference in sheltering
#if the shelter_change is positive, that meanss that the sheltering increased from before
hot_days_sheltering$shelter_diff <- hot_days_sheltering$shelter_index - hot_days_sheltering$shelter_index_prior

# for each fips, get percentile for sheltering 
#quantile(hot_days_sheltering$shelter_index)
hot_days_sheltering$hot_sheltering_group <- ifelse(hot_days_sheltering$shelter_index >= 31.96, 3, 
                                     ifelse(hot_days_sheltering$shelter_index <= 23.59, 1, 2))
hot_days_sheltering$prior_sheltering_group <- ifelse(hot_days_sheltering$shelter_index_prior >= 31.96, 3, 
                                                   ifelse(hot_days_sheltering$shelter_index_prior <= 23.59, 1, 2))

#group change? here a positive in group change means that sheltering increased with heat, 
# 0 means it stayed the same, negative means less shelter when hotme
hot_days_sheltering$group_change <- hot_days_sheltering$hot_sheltering_group - hot_days_sheltering$prior_sheltering_group

group_changed <- hot_days_sheltering %>% dplyr::filter(group_change != 0)

# what percentage saw a change in sheltering (shelterb4 =/= shelterafter)

#of those, how many went from low -> mid, mid -> high, or low -> high?