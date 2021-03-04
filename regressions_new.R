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
t <- read_rds(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean_2021.rds"))
m_master <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))
r_master <- read_csv(paste0(getwd(), "/us census/climate_regions.csv"))

#filter for states with heatwaves in 2020
#heat_states <- c("OR", "CA", "NV", "AZ", "NM", "UT", "TX", "CO")
m_master$state <- str_sub(m_master$county, -2)
m_master <- left_join(m_master, r_master, by= "state")
#m_master <- m_master %>% dplyr::filter(state %in% heat_states) 
m_master <- m_master %>% dplyr::filter(region_s == "West")
m_master <- m_master %>% mutate(poverty_group = ntile(poverty_percent, 10))
regions <- "West"

#get counties with over 50,000 people 
m_pops <- m_master %>% group_by(fips) %>% summarise(population = mean(as.numeric(population_est))) %>% arrange(desc(population))
m_pops <- m_pops %>% dplyr::filter(population >= 20000)
m_master <- m_master %>% dplyr::filter(fips %in% m_pops$fips)
m_master <- m_master %>% filter(!is.na(region_s))
# we want to subtract a month to all of these so that when the data are combined, there is a 1 month mortality lag
m_master <- m_master %>% mutate(month = month - 1) %>% mutate(year = ifelse(month == 0, year - 1, year)) %>% 
  mutate(month = ifelse(month == 0,12, month)) %>%
#  dplyr::filter(poverty_group >= 9)

pdf(paste0("./visuals/regressions", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
       main = title)
  text(x = 0.5, y = 0.5, paste(timestamp(), "West Coast Only - No Income"),
       cex = 1.5, col = "black")

####################
## Quick function that takes data and plots all the variations we'd want
plot_data <- function(data, plot_title, lows=FALSE) {
  
  xlab <- ifelse(lows, "#Days @ or Below percentile", "#Days @ or Above percentile")
  par(mfcol = c(2,2))
  print(plot_title)
  model <- fe_model(data, level = 2)
  boots <- bootstrap_data(data, short=T, level=2)
  plot_regs(data, boots, plot_title, level = 2, xlabel = xlab, model=model)
  
  data$deaths <- log(data$deaths)
  model <- fe_model(data, level = 2)
  boots <- bootstrap_data(data, short=T, level=2)
  plot_regs(data, boots, plot_title, level = 2,xlabel = xlab, ylabel = "Log Mortality", model = model)
}

####################

#####
##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
## Recalculate z-scores for just the summer months and add in percentile value
t_zs <- t %>% group_by(fips, year) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high >= 0.9))

data <- left_join(m_master, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

## run through and do this for each unique region
for(region in regions) {
  print(region)
  plot_title <- paste0("Deaths per 100K + #Days high >90P \n ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title)
}

  
## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low >= 0.9))

data <- left_join(m_master, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

## run through and do this for each unique region
for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days low >90P \n ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title)
}


####################
##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
## just summer months
## Recalculate z-scores for just the summer months and add in percentile value
t_zs <- t %>% group_by(fips, year) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high >= 0.9))

data <- left_join(m_master, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days high >90P \n 05-09 ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title)
}


## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low >= 0.9))

data <- left_join(m_master, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)


for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days low >90P \n 05-09 ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title)
}

################
# Same as before, except xtreme cold and winter months 

#####
##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
## Recalculate z-scores for just the summer months and add in percentile value
t_zs <- t %>% group_by(fips, year) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

## Per Capita Deaths, num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high <= 0.1))

data <- left_join(m_master, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days high <10P \n ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title, lows=T)
}

## Per Capita Deaths, num days where avg high temp in county is above 90th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low <= 0.1))

data <- left_join(m_master, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days low <10P \n ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title, lows=T)
}

####################
##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
## just winter months
## Recalculate z-scores for just the winter months and add in percentile value
t_zs <- t %>% group_by(fips, year) %>%
  dplyr::filter(month %in% c(11,12,1,2)) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high <= 0.1))

data <- left_join(m_master, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days high <10P \n 11-2 ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title, lows=T)
}

## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low <= 0.1))

data <- left_join(m_master, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, poverty_group, population_est, state, year, region_s) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)
data$stateyear <- paste0(data$state, data$year)

for(region in regions) {
  plot_title <- paste0("Deaths per 100K + #Days low <10P \n 11-2 ", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title, lows=T)
}


