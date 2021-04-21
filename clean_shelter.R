ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(SafeGraphR) 

#read in raw sheltering by cbg
p <- readRDS(paste0(getwd(), "/heatwaves_manual/patterns_raw_blockgroup.rds"))

#get just santa clara county
p <- p %>% filter(fips == "06085")

#expand out sheltering 
d <- expand_cat_json(p, "device_home_areas", index = "origin_census_block_group",  by = c("census_block_group", "date", "state", "fips"), fun = sum)

#rename for clarity
d <- d %>% rename(visited_block_group = census_block_group) %>%
  dplyr:filter(origin_census_block_group != visited_block_group) %>%
  rename(origin_census_block_group = census_block_group) %>%
  group_by(census_block_group, date, state, fips) %>%
  summarise(total_devices_moving = sum(device_home_areas)) %>% mutate(month = month(date)) %>% mutate(year = year(date))

# join with the primary residence table so we know how many live there
r <- readRDS("all_residing_raw_blockgroup.rds")

patterns_clean_blockgroup <- left_join(d, r, by = c("census_block_group", "month", "year"))
saveRDS(patterns_clean_blockgroup, paste0(getwd(),"/heatwaves_manual/patterns_clean_blockgroup.rds"))
