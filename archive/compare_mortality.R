# Just a quick code to get some death data in some larger counties
ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

source("./regression_functions.R")

library(dplyr)
library(lfe)
library(ggplot2)
library(dotwhisker)
library(tidyverse)
library(lubridate)
library(reshape2)
library(stringr)

##Read in datasets
t <- read_rds(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean.rds"))
m_master <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))
r_master <- read_csv(paste0(getwd(), "/us census/climate_regions.csv"))

#filter for states with heatwaves in 2020
heat_states <- c("OR", "CA", "NV", "AZ", "NM", "UT", "TX", "CO")
m_master$state <- str_sub(m_master$county, -2)
m_master <- left_join(m_master, r_master, by= "state")
m_master <- m_master %>% dplyr::filter(state %in% heat_states) 

#get counties with over 50,000 people 
m_pops <- m_master %>% group_by(fips) %>% summarise(population = mean(as.numeric(population_est))) %>% arrange(desc(population))
m_pops <- m_pops %>% dplyr::filter(population >= 50000)
m_master <- m_master %>% dplyr::filter(fips %in% m_pops$fips)
m_master <- m_master %>% filter(!is.na(region_s))


# We're looking at August deaths specifically
sd <- m_master %>% dplyr::filter(fips == "06037", month == 8, year == 2018)
sd <- sd %>% group_by(year) %>% summarise(deaths = sum(deaths, na.rm = T))
mean <- mean(sd$deaths)

test <- read_csv(paste0(getwd(), "/calculated/select_counties_long.csv"))
test <- test %>% dplyr::filter(County %in% c("San Diego County", "LA County", "Maricopa", "Santa Clara County"))

test %>% 
  mutate(Source = as.factor(Source)) %>%
  mutate(Source = fct_relevel(Source, "COVID", "Projected Heat", "Non COVID")) %>%
  ggplot() +
  geom_bar(data=test, aes(y = Mortality, x = Year, fill = Source), stat="identity",
           position='stack') +
  theme_bw() + 
  facet_grid( ~ County) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(text = element_text(size = 20))

