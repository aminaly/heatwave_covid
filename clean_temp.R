ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)


temps <- read_rds("./heatwaves_manual/all_temp_data_long.rds")

#reshape by with min and max next to each other
tm <- temps %>% dplyr::filter(measure == "tmmn")
tx <- temps %>% dplyr::filter(measure == "tmmx")
t <- left_join(tm, tx, by = c("date", "fips"))

#Add additional columns and rename for ease
t <- t %>% dplyr::select(date, county = county.x, fips,
                         mean_low = mean_measure.x, 
                         mean_high = mean_measure.y) %>%
  mutate(fips = as.character(fips), month = month(date), year = year(date)) %>%
  dplyr::filter(is.finite(mean_low)) %>% 
  dplyr::filter(is.finite(mean_high))

t$monthyear <- paste0(t$month, t$year)

saveRDS(t, "./heatwaves_manual/all_temperature_data_clean_nov.rds")


