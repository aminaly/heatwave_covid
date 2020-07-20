ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)


temps <- read_rds("./heatwaves_manual/all_temperature_data.rds")

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

#Calculate a z-score based on avg high for each month
t_zs <- t %>% group_by(fips, month) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  ungroup %>%
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low))


saveRDS(t_zs, "./heatwaves_manual/all_temperature_data_clean.rds")
