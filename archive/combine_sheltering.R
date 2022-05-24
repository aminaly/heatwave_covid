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
all_files <- list.files("heatwaves_manual/safegraph/2020/08", full.names = T, recursive = T)
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
            all_devices = sum(device_count, na.rm = T))

sheltering$shelter_index <- (sheltering$home_device / sheltering$all_devices)*100
sheltering <- sheltering %>% group_by(fips) %>% summarise(shelter_index = mean(shelter_index))

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

t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high >= 0.9))
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low >= 0.9))

## What month of temperature data do you want? 
mon <- 8

## pull out and combine all the months (if more than one)
temps <- t_high %>% dplyr::filter(month == mon & year == 2020)

##### Work on combining data ##### 

## shelter and mortality combined
shelter_mort <- left_join(mortality, sheltering, by = "fips")

# for each fips, get percentile for sheltering 
#  uantile(shelter_mort$shelter_index)
shelter_mort$shelter_level <- ifelse(shelter_mort$shelter_index >= 31.29, "highshelter", 
                                        ifelse(shelter_mort$shelter_index <= 24.73, "lowshelter", "midshelter"))

## for each fips, add in population and calculate mortality per capita
m_master <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))
m_pops <- m_master %>% group_by(fips) %>% summarise(population = mean(as.numeric(population_est))) %>% arrange(desc(population))
m_pops <- m_pops %>% dplyr::filter(population >= 50000)

shelter_mort <- na.omit(left_join(shelter_mort, m_pops))
shelter_mort$percap_mort <- (shelter_mort$all_deaths_aug / shelter_mort$population) * 100000

## left join with clean temps | combined and label heat
septmort_augtemp <- left_join(shelter_mort, temps, by = "fips")
#quantile(septmort_augtemp$num_90, na.rm = T)
septmort_augtemp$heat_level <- ifelse(septmort_augtemp$num_90 >= 7, "highhotdays", "lowhotdays")

## this drops hawaii and alaska
septmort_augtemp <- na.omit(septmort_augtemp)
#septmort_augtemp <- septmort_augtemp %>% mutate(shelter_level = fct_relevel(shelter_level, "highshelter", "midshelter", "lowshelter")) 

##### Work on ANOVA ##### 

# assign groups
septmort_augtemp$group <- paste(septmort_augtemp$shelter_level, septmort_augtemp$heat_level)
septmort_augtemp <- septmort_augtemp %>% select(county = county.x, fips, all_deaths_aug, 
                               shelter_level, heat_level, percap_mort, group)

## run anova 
par(mar = c(5,25,5,5)) 

# heat and shelter
anova_julytemp <- aov(percap_mort ~ as.factor(shelter_level)*as.factor(heat_level),
                      data = septmort_augtemp)
tuk <- TukeyHSD(anova_julytemp)
plot(tuk, las = 1, col = "darkgreen")

# heat only
anova_julytemp_heatonly <- aov(percap_mort ~ as.factor(heat_level),
                      data = septmort_augtemp)
tuk_heatonly <- TukeyHSD(anova_julytemp_heatonly)
plot(tuk_heatonly, las = 1, col = "darkgreen")


# shelter only 
anova_julytemp_sheltonly <- aov(percap_mort ~ as.factor(shelter_level),
                               data = septmort_augtemp)
tuk_sheltonly <- TukeyHSD(anova_julytemp_sheltonly)
plot(tuk_sheltonly, las = 1, col = "darkgreen")

## box plot 
septmort_augtemp %>%
  mutate(shelter_level = fct_relevel(shelter_level, "lowshelter", "midshelter", "highshelter")) %>%
  ggplot() +
  aes(x=heat_level, y=percap_mort, col = heat_level) + 
  geom_boxplot() + 
  facet_grid( ~ shelter_level) +
  theme(text = element_text(size = 20)) +
  theme_light() 


###This line pot will show change in mean from high hot days (over 75% percentile) and low
## get confidence intervals 
septmort_augtemp <- septmort_augtemp %>% 
  group_by(heat_level, shelter_level) %>% 
  mutate(lower = t.test(percap_mort)$conf.int[1], upper = t.test(percap_mort)$conf.int[2]) %>%
  ungroup()

septmort_augtemp %>% 
  mutate(shelter_level = fct_relevel(shelter_level, "lowshelter", "midshelter", "highshelter")) %>%
  ggplot() +
  aes(x = shelter_level, color = heat_level, group = heat_level, y = percap_mort) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") + 
  theme_minimal()  + 
  theme(text = element_text(size = 20)) +
  coord_cartesian(ylim=c(50,140)) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper), width = 0.1) 


## plot of heat temps US 
#load in counties
counties <- st_read("heatwaves_manual/shapefiles/tl_2017_us_county.shp")
counties$fips <- counties$GEOID
county_heat <- left_join(counties, temps)

geom_sf(county_heat, aes(fill = num_90)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  theme_light()


