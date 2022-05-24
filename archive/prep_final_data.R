ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

source("./regression_functions_shelter.R")

library(dplyr)
library(lfe)
library(ggplot2)
library(dotwhisker)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)
library(broom)
library(gridExtra)
library(wesanderson)
library(SafeGraphR)
library(interactions)


######## Read in datasets ######## 
t <- read_rds(paste0(getwd(), "/heatwaves_manual/bayarea_temp_data_clean_2021.rds"))
r_master <- read_csv(paste0(getwd(), "/us_census/climate_regions.csv"))
m_master <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))
s <- read_rds(paste0(getwd(), "/heatwaves_manual/patterns_clean_bayarea.rds"))
metadata <- read.csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/metadata/cbg_geographic_data.csv"), stringsAsFactors = F, header = T)

#get income and population data
income <- read.csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b19.csv"), stringsAsFactors = F, header = T)

######## Prep Data ######## 

# get state and regional information
m_master$state <- str_sub(m_master$county, -2)
m_master <- left_join(m_master, r_master, by= "state")
m_master <- unique(m_master %>% select(fips, state, region, region_s))
s <- left_join(s, m_master, by = "fips")

#set up mortality & add in median income
income <- na.omit(income %>% dplyr::select(census_block_group, 	median_income = B19013e1))
income <- income %>% mutate("census_block_group" = ifelse(nchar(census_block_group) == 11, 
                                                          paste0("0", census_block_group), census_block_group))
income$fips <- substr(income$census_block_group, 1, 5)
income <- income %>% mutate(income_group = ntile(median_income, 5))
i <- income %>% mutate("fips" = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  dplyr::select(fips, median_income, income_group, census_block_group)

i$census_block_group <- as.character(i$census_block_group)

#combine shelter with icncome, and select region if we want it
shelter <- left_join(s, i, by = c("census_block_group", "fips"))
shelter <- shelter %>% mutate(date = as.Date(date))

## Combined temperature and sheltering by fips 
t <- t %>% filter(fips %in% unique(s$fips))
t_zs <- t %>% group_by(fips, year) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = 100* pnorm(z_score_high)) %>%
  mutate(p_low = 100* pnorm(z_score_low)) %>%
  ungroup

data <- left_join(shelter, t_zs, by = c("fips", "date", "year"))
data <- data %>% mutate(mean_low_c = mean_low-273.15, mean_high_c = mean_high-273.15)

## add monthweek and countyyear for the regression
data$countyyear <- paste0(data$fips, data$year)
data$monthweek <- paste0(month(data$date, label = T), week(data$date))

saveRDS(data, "./heatwaves_manual/data_for_regression.rds")
