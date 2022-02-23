
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

morts <- list.files("./heatwaves_manual/cdc mortality", full.names = T)

all_mortality <- data.frame()

for(i in  1:length(morts)) {
  
  print(i)
  m <- morts[i]
  mort <- read.csv(m, sep = "", stringsAsFactors = F)
  mort$County <- as.character(mort$County)
  mort <- na.omit(mort)
  if("UCD...Drug.Alcohol.Induced.Code" %in% colnames(mort)) mort <- mort %>% dplyr::rename(Drug.Alcohol.Induced.Code = UCD...Drug.Alcohol.Induced.Code)
  cleaned <- mort %>% mutate("month" = str_sub(Month, -2)) %>%
    mutate("year" = str_sub(Month, 1, 4)) %>%
    mutate("fips" = ifelse(nchar(County) == 4, paste0("0", County), County)) %>%
    drop_na("fips") %>%
    select(county = Notes, fips, month, year, 
           deaths = Drug.Alcohol.Induced.Code, cause = Month.Code)
  all_mortality <- bind_rows(all_mortality, cleaned)
  
}


## Now add in income and population data
income <- read.csv("./us census/income_county.csv", stringsAsFactors = F, header = T)
population <- read_rds("./calculated/fips_population.rds")

#set up mortality & add in median income
m <- all_mortality %>% mutate(month = as.numeric(month), year = as.numeric(year))
income$fips <- as.character(income$fips)
income <- income %>% mutate(income_group = ntile(median_income, 10))
i <- income %>% mutate("fips" = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  dplyr::select(fips, median_income, poverty_percent, income_group)
m <- left_join(m, i, by = "fips")

#Add in the population to all of these 
p <- population %>% mutate(year = as.numeric(year))
m <- left_join(m, p, by = c("fips", "year"))
m <- m %>% mutate(deaths_per_capita = as.numeric(deaths) / as.numeric(population_est)) %>% 
  mutate(deaths_per_10000 = deaths_per_capita * 10000)

saveRDS(m, "./calculated/all_mortality.RDS")

