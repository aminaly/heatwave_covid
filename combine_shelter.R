ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)

## first lets do the files with all movement data
all_files <- list.files("heatwaves_manual/safegraph/patterns/2020/12/17", full.names = T, recursive = T, pattern = "*.csv")
combined <- data.frame()

for(file in all_files) {
  
  print(file)
  possibleError <- tryCatch(
    f <- read_csv(file), 
    error = function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  saveRDS(f[1:10000,], "heatwaves_manual/neighborhood_short.rds")
  
  combined <- bind_rows(combined, f)
  
}

combined$fips <- str_sub(combined$census_block_group, 1,5)
sheltering <- combined %>% group_by(fips, date_range_start) %>% 
  summarise(home_device = sum(completely_home_device_count, na.rm = T), 
            all_devices = sum(device_count, na.rm = T))

sheltering$shelter_index <- (sheltering$home_device / sheltering$all_devices)*100
saveRDS(sheltering, "heatwaves_manual/patterns_raw_blockgroup.rds")

sheltering <- sheltering %>% group_by(fips, date = date_range_start, home_device, all_devices) %>% summarise(shelter_index = mean(shelter_index))
saveRDS(sheltering, "heatwaves_manual/patterns_raw_fips.rds")

#####################
#### now lets combine the actual neighborhood devices residing patterns by day
all_files <- list.files("heatwaves_manual/safegraph/neighborhood_home_panel_summary/2020/12/17", full.names = T, recursive = T, pattern = "*.csv")
combined <- data.frame()

for(file in all_files) {
  
  print(file)
  possibleError <- tryCatch(
    f <- read_csv(file), 
    error = function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  combined <- bind_rows(combined, f)
  
}

#save out
combined$fips <- str_sub(combined$census_block_group, 1,5)
saveRDS(combined, "heatwaves_manual/all_residing_raw_blockgroup.rds")

#save out at the FIPS level
sheltering <- combined %>% group_by(fips, year, month, state) %>% summarise(shelter_index = sum(number_devices_residing))
saveRDS(combined, "heatwaves_manual/all_residing_raw_fips.rds")
