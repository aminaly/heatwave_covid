ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)

all_files <- list.files("heatwaves_manual/safegraph/social_distancing_metrics/", full.names = T, recursive = T)
combined <- data.frame()

for(file in all_files) {
  
  print(file)
  possibleError <- tryCatch(
    f <- read_csv(file), 
    error = function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  f <- f %>% select(origin_census_block_group, date_range_start, date_range_end, completely_home_device_count, device_count)
  combined <- bind_rows(combined, f)
  
}

combined$fips <- str_sub(combined$origin_census_block_group, 1,5)
sheltering <- combined %>% group_by(fips, date_range_start) %>% 
  summarise(home_device = sum(completely_home_device_count, na.rm = T), 
            all_devices = sum(device_count, na.rm = T))

sheltering$shelter_index <- (sheltering$home_device / sheltering$all_devices)*100
saveRDS(sheltering, "heatwaves_manual/all_sheltering_raw_blockgroup.rds")

sheltering <- sheltering %>% group_by(fips) %>% summarise(shelter_index = mean(shelter_index))
saveRDS(sheltering, "heatwaves_manual/all_sheltering_raw_fips.rds")
