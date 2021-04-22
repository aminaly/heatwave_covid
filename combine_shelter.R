ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)

## first lets do the files with all movement data
all_files <- list.files("heatwaves_manual/safegraph/patterns/2020/12/17", full.names = T, recursive = T, pattern = "*.csv.gz")
combined <- data.frame()

for(file in all_files) {
  
  print(file)
  possibleError <- tryCatch(
    f <- read_csv(file), 
    error = function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  f$fips <- str_sub(f$area, 1,5)
  f <- f %>% filter(fips == "06085") %>%
    select(census_block_group = area, date = date_range_start, raw_device_counts, distance_from_home,
                    distance_from_primary_daytime_location, median_dwell, state = region, device_home_areas, fips, stops_by_day)
  combined <- bind_rows(combined, f)
  
}

saveRDS(combined, "heatwaves_manual/patterns_santaclara.rds")

# #####################
# #### now lets combine the actual neighborhood devices residing patterns by day
# all_files <- list.files("heatwaves_manual/safegraph/neighborhood_home_panel_summary/2020/12/17", full.names = T, recursive = T, pattern = "*.csv")
# combined <- data.frame()
# 
# for(file in all_files) {
#   
#   print(file)
#   possibleError <- tryCatch(
#     f <- read_csv(file), 
#     error = function(e) e
#   )
#   
#   if(inherits(possibleError, "error")) next
#   
#   combined <- bind_rows(combined, f)
#   
# }
# 
# #save out
# combined$fips <- str_sub(combined$census_block_group, 1,5)
# saveRDS(combined, "heatwaves_manual/all_residing_raw_blockgroup.rds")
# 
# #save out at the FIPS level
# sheltering <- combined %>% group_by(fips, year, month, state) %>% summarise(shelter_index = sum(number_devices_residing))
# saveRDS(combined, "heatwaves_manual/all_residing_raw_fips.rds")
