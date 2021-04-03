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

##Read in datasets
t <- read_rds(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean_2021.rds"))
s <- read_rds(paste0(getwd(), "/heatwaves_manual/all_sheltering_raw_fips.rds"))

t <- t %>% dplyr::filter(year %in% c(2018, 2019, 2020))
s <- s[1:100000,]

saveRDS(t, (paste0(getwd(),"/heatwaves_manual/all_temperature_data_clean_2021s.rds")))
saveRDS(s, (paste0(getwd(),"/heatwaves_manual/all_sheltering_raw_fipss.rds")))
