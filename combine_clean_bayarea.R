ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)
library(SafeGraphR)

#### Set universal vars ----

# FIPS codes we care about
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 
today <- format(Sys.Date(), "%m_%Y")

#temperature location
temp_loc <- "heatwaves_manual/temps/bg"
RUNTEMP <- TRUE

#sheltering location
movement_loc <- "heatwaves_manual/safegraph/neighborhood-patterns/2022/02/09/release-2021-07-01/neighborhood_patterns/"
RUNMOV <- TRUE

#home devices location
home_dev_loc <- "heatwaves_manual/safegraph/neighborhood-patterns/2022/02/09/release-2021-07-01/neighborhood_home_panel_summary/"

#### Combine & Clean Temperature Data ----

if(RUNTEMP) {
  
  all_files <- list.files(temp_loc, full.names = T)
  t <- data.frame()
  
  for(i in 1:length(all_files)) {
    
    print(i)
    file <- all_files[i]
    f <- readRDS(file)
    f <- f %>% filter(fips %in% included_fips)
    t <- bind_rows(t, f)

  }
  
  #### Clean Temperature Data 
  
  #for now, select just the tx
  t <- t %>% dplyr::filter(measure == "tmmx")

  #Add additional columns and rename for ease
  t <- t %>% dplyr::select(date, county, fips, census_block_group,
                           mean_high = mean_measure) %>%
    mutate(fips = as.character(fips), month = month(date), year = year(date)) %>%
    dplyr::filter(is.finite(mean_high)) %>% 
    mutate(mean_high_c = mean_high-273.15)
  
  t$monthyear <- paste0(t$month, t$year)
  
  ## Add in zscores and percentiles of temp data
  t <- t %>% group_by(fips, year) %>%
    mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
    mutate(p_high = 100* pnorm(z_score_high)) %>%
    ungroup
  

  saveRDS(t, paste0("heatwaves_manual/bay_temperature_clean_blockgroup_", today, ".RDS"))
  
  
} else {
  t <- readRDS(paste0("heatwaves_manual/bay_temperature_clean_blockgroup_", today, ".RDS"))
}

#### Combine & Clean Patterns Data ----

if(RUNMOV) {
  
  movement_files <- list.files(movement_loc, full.names = T, recursive = T, pattern = "*.csv.gz")
  movement <- data.frame()
  
  for(file in movement_files) {
    
    print(file)
    possibleError <- tryCatch(
      f <- read_csv(file), 
      error = function(e) e
    )
    
    if(inherits(possibleError, "error")) next
    
    print(head(f))
    f <- f %>% select(census_block_group = area, date = date_range_start,
                      stops_by_day, distance_from_home)
    f$fips <- str_sub(f$census_block_group, 1,5)
    f <- f %>% filter(fips %in% included_fips)
    
    movement <- bind_rows(movement, f)
    
  }
  
  #### Clean Patterns data 
  movement <- expand_integer_json(movement, "stops_by_day", index = "day",  
                                  by = c("census_block_group", "date", "fips", "distance_from_home"), fun = sum)
  #rename for clarity
  movement$date <- movement$date + days(movement$day - 1) 
  movement <- movement %>% mutate(month = month(date)) %>% mutate(year = year(date))
  
  saveRDS(movement, paste0("heatwaves_manual/bay_patterns_clean_blockgroup_", today, ".RDS"))

} else {
  movement <- readRDS(paste0("heatwaves_manual/bay_patterns_clean_blockgroup_", today, ".RDS"))
}

#### Combine Home Devices ----
home_files <- list.files(home_dev_loc, full.names = T, recursive = T, pattern = "*.csv")
home <- data.frame()

for(file in home_files) {
  
  print(file)
  possibleError <- tryCatch(
    f <- read_csv(file),
    error = function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  f$fips <- str_sub(f$census_block_group, 1,5)
  f <- f %>% filter(fips %in% included_fips)
  home <- bind_rows(home, f)
  
}

#### Read and clean Income & Population ----
income <- read.csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b19.csv"), stringsAsFactors = F, header = T)
income <- na.omit(income %>% dplyr::select(census_block_group, 	median_income = B19013e1))
income <- income %>% mutate("census_block_group" = ifelse(nchar(census_block_group) == 11, 
                                                          paste0("0", census_block_group), census_block_group))
income$fips <- substr(income$census_block_group, 1, 5)
income <- income %>% mutate(income_group = ntile(median_income, 5)) %>% filter(fips %in% included_fips)
income$census_block_group <- as.character(income$census_block_group)

pops <- cbg_pop %>% mutate(census_block_group = as.character(poi_cbg)) %>% 
  mutate(census_block_group =  ifelse(nchar(census_block_group) == 11, 
                                      paste0("0", census_block_group), census_block_group)) %>% 
  select(census_block_group, unweighted_pop)
pops$fips <- str_sub(pops$census_block_group, 1,5)
pops <- pops %>% filter(fips %in% included_fips)

pop_income <- left_join(income, pops, by = c("census_block_group", "fips"))

#### Combine all data ----

## combine movement and home 
patterns <- left_join(movement, home, by = c("census_block_group", "fips", "year", "month"))

## add in income
print(head(patterns))
patterns <- left_join(patterns, pop_income,  by = c("census_block_group", "fips"))

## combine above with temperature
print(head(patterns))
print(head(t))
data <- left_join(patterns, t,  by = c("census_block_group", "fips", "date"))

## final additions for regressions: add monthweek and countyyear
data$countyyear <- paste0(data$fips, data$year)
data$monthweek <- paste0(month(data$date, label = T), week(data$date))

saveRDS(data, paste0("./heatwaves_manual/data_for_regression_", today, ".RDS"))

