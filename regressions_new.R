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

#get the 300 counties with the highest populations
m_pops <- m_master %>% group_by(fips) %>% summarise(population = mean(as.numeric(population_est))) %>% arrange(desc(population))
#m_pops <- m_pops[1:300,]
#m_master <- m_master %>% dplyr::filter(fips %in% m_pops$fips)
m_master$state <- str_sub(m_master$county, -2)
m_master <- left_join(m_master, r_master, by= "state")
m_master <- m_master %>% filter(!is.na(region))

pdf(paste0("heatwaves_manual/visuals/regressions", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
       main = title)
  text(x = 0.5, y = 0.5, paste(timestamp()),
       cex = 1.5, col = "black")

####################
## Quick function that takes data and plots all the variations we'd want
plot_data <- function(data, plot_title) {
  
  par(mfcol = c(2,2))
  print(plot_title)
  model <- fe_model(data, level = 2)
  boots <- bootstrap_data(data, short=T, level=2)
  plot_regs(data, boots, plot_title, level = 2, xlabel = "#Days @ or Above percentile", model=model)
  
  data$deaths <- log(data$deaths)
  model <- fe_model(data, level = 2)
  boots <- bootstrap_data(data, short=T, level=2)
  plot_regs(data, boots, plot_title, level = 2,xlabel = "#Days @ or Above percentile", ylabel = "Log Mortality", model = model)
}

####################

#####
## We want to loop through every region and run these regressions
for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)
  ##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
  ## and only summmer months
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
  
  data <- left_join(m, t_high, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>% 
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days high >90P \n ", r)
  plot_data(data, plot_title)
  
}

for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)
  ## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
  t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
    summarize(num_90 = sum(p_low >= 0.9))
  
  data <- left_join(m, t_low, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>%
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days low >90P \n ", r)
  plot_data(data, plot_title)
}

  ### Same as above, but 95th percentile
for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)
  ## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
  t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
    summarize(num_95 = sum(p_high >= 0.95))
  
  data <- left_join(m, t_high, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_95, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>% 
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days high >95P \n ", r)
  plot_data(data, plot_title)
}

for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)
  ## Per Capita Deaths, summer months, num days where avg high temp in county is above 95th percentile
  t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
    summarize(num_95 = sum(p_low >= 0.95))
  
  data <- left_join(m, t_low, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_95, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>%
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days low >95P \n ", r)
  plot_data(data, plot_title)
}
  ####################
  ##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
  ## and only summmer months
for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)  
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
  
  data <- left_join(m, t_high, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>% 
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days high >90P \n 05-09 ", r)
  plot_data(data, plot_title)
}

for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)
  ## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
  t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
    summarize(num_90 = sum(p_low >= 0.9))
  
  data <- left_join(m, t_low, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>%
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days low >90P \n 05-09 ", r)
  plot_data(data, plot_title)
}
  ### Same as above, but 95th percentile
  ## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r)
  t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
    summarize(num_95 = sum(p_high >= 0.95))
  
  data <- left_join(m, t_high, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_95, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>% 
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days high >95P \n 05-09 ", r)
  plot_data(data, plot_title)
}

  ## Per Capita Deaths, summer months, num days where avg high temp in county is above 95th percentile
for(r in unique(m_master$region)) {
  print(r)
  m <- m_master %>% dplyr::filter(region == r) 
  t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
    summarize(num_95 = sum(p_low >= 0.95))
  
  data <- left_join(m, t_low, by = c("fips", "month", "year"))
  data <- data %>% 
    group_by(fips, measure = num_95, monthyear, county = county.x, income_group, population_est, state, year) %>% 
    summarise(deaths = sum(deaths, na.rm = T)) %>%
    dplyr::filter(is.finite(measure)) %>%
    mutate(deaths = (deaths/as.numeric(population_est))*100000)
  data <- na.omit(data)
  data$stateyear <- paste0(data$state, data$year)
  
  plot_title <- paste0("Deaths per 100K + #Days low >95P \n 05-09 ", r)
  plot_data(data, plot_title)
  
}



