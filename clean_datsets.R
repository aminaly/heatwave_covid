
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

## Make useful geocode ref
geocodes <- read_csv("./us census/geocodes.csv", col_types = cols(.default = "c"))
colnames(geocodes) <- c('sum_lev', 'state_fips', "county_fips", "area_name", 'fips')
geo_state <- geocodes %>% dplyr::filter(county_fips == "000") %>% 
  dplyr::filter(!str_detect(area_name, 'city')) %>%
  dplyr::filter(!str_detect(area_name, 'town')) %>%
  dplyr::filter(!str_detect(area_name, 'municipality')) %>%
  dplyr::filter(!str_detect(area_name, 'village')) %>%
  dplyr::filter(!str_detect(area_name, 'borough')) %>%
  dplyr::filter(!str_detect(area_name, 'government')) %>%
  dplyr::filter(!str_detect(area_name, 'County')) %>%
  dplyr::filter(!str_detect(area_name, 'corporation')) %>%
  dplyr::filter(!str_detect(area_name, 'Princeton')) %>%
  dplyr::filter(!str_detect(area_name, 'corporation')) %>%
  dplyr::filter(!str_detect(area_name, 'Urban Honolulu CDP')) %>%
  dplyr::filter(!str_detect(area_name, 'county')) %>%
  dplyr::filter(!str_detect(area_name, 'Butte-Silver Bow')) %>%
  dplyr::filter(!str_detect(area_name, 'City')) %>%
  select(state_fips, area_name)
geo_county <- geocodes %>% dplyr::filter(county_fips != "000")
geocodes <- left_join(geo_county, geo_state, by = "state_fips")
geocodes <- geocodes %>% mutate(county_full_name = paste0(area_name.x, ", ", area_name.y))

## Clean Population
pops <- list.files("./us census/", pattern = "*population", full.names = T)

first <- read_csv(pops[1])
first <- first[,4:19]
first <- first %>% select(!CENSUS2000POP) %>% select(!ESTIMATESBASE2000)
colnames(first) <- c('state', 'county', 'state_name', 'county_name', '2000', '2001',
                     '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', 'fips')
first <- first %>% mutate(fips = paste0(state, county))
first <- melt(first)
first <- first %>% select(fips, year = variable, population_est = value) %>%
  mutate(fips = as.character(fips), year = as.character(year), population_est = as.character(population_est))
first <- na.omit(first)

sec <- read_csv(pops[2])
sec <- sec %>% mutate(county_full_name = str_sub(county_full_name, 2))
sec <- left_join(sec, geocodes, by = 'county_full_name')
sec <- melt(sec)
sec <- sec %>% select(fips, year = variable, population_est = value) %>%
  mutate(fips = as.character(fips), year = as.character(year), population_est = as.character(population_est))
sec <- na.omit(sec)

populations <- bind_rows(first, sec)
saveRDS(populations, "./calculated/fips_population.rds")
