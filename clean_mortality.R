
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

morts <- list.files("./cdc mortality", full.names = T)

all_mortality <- data.frame()

for(m in morts) {
  
  mort <- read.csv(m, sep = "", stringsAsFactors = F)
  mort$County <- as.character(mort$County)
  cleaned <- mort %>% mutate("month" = str_sub(Month, -2)) %>%
    mutate("year" = str_sub(Month, 1, 4)) %>%
    mutate("fips" = ifelse(nchar(County) == 4, paste0("0", County), County)) %>%
    drop_na("fips") %>%
    select(county = Notes, fips, month, year, 
           deaths = Drug.Alcohol.Induced.Code, cause = Month.Code)
  all_mortality <- bind_rows(all_mortality, cleaned)
  
}

saveRDS(all_mortality, "./calculated/all_mortality.RDS")
