ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(SafeGraphR) 

#read in raw sheltering by cbg
p <- readRDS(paste0(getwd(), "/heatwaves_manual/patterns_santaclara.rds"))

#expand out sheltering 
d <- expand_integer_json(p, "stops_by_day", index = "day",  by = c("census_block_group", "date", "state", "fips"), fun = sum)

#rename for clarity
d$date <- d$date + days(d$day - 1) 
d <- d %>% mutate(month = month(date)) %>% mutate(year = year(date))

# join with the primary residence table so we know how many live there
r <- readRDS(paste0(getwd(), "/heatwaves_manual/all_residing_raw_blockgroup.rds"))
r <- r %>% filter(fips %in% unique(d$fips))

patterns_clean_blockgroup <- left_join(d, r, by = c("census_block_group", "month", "year", "fips"))
patterns_clean_blockgroup <- patterns_clean_blockgroup %>% rename(state = state.x)

patterns_clean_blockgroup$visitors = patterns_clean_blockgroup$stops_by_day - patterns_clean_blockgroup$number_devices_residing
patterns_clean_blockgroup$visitors_percap = patterns_clean_blockgroup$visitors / patterns_clean_blockgroup$number_devices_residing


saveRDS(patterns_clean_blockgroup, paste0(getwd(),"/heatwaves_manual/patterns_clean_santaclara.rds"))
