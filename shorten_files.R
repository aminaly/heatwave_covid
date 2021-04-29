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

# Which FIPS codes do we want? 
# bay area this time
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area not including Sonoma, Napa, Marin, and Solano
 
##Read in datasets
p <- readRDS("heatwaves_manual/patterns_raw_blockgroup.rds")
p <- p %>% filter(fips %in% included_fips)


saveRDS(p, (paste0(getwd(),"/heatwaves_manual/patterns_bayarea.rds")))

# t <- readRDS(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean_2021.rds"))
# t <- t %>% filter(fips %in% included_fips)
# 
# saveRDS(t, (paste0(getwd(),"/heatwaves_manual/bayarea_temp_data_clean_2021.rds")))
# 
