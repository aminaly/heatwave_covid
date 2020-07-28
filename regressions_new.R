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

##Read in datasets
t <- read_rds(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean.rds"))
m <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))

#get the 300 counties with the highest populations
m_pops <- m %>% group_by(fips) %>% summarise(population = mean(as.numeric(population_est))) %>% arrange(desc(population))
m_pops <- m_pops[1:300,]
m <- m %>% dplyr::filter(fips %in% m_pops$fips)

## Recalculate z-scores for just the summer months and add in percentile value
t_zs <- t %>% group_by(fips, month) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

pdf(paste0("heatwaves_manual/visuals/regressions", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run

####################
## Quick function that takes data and plots all the variations we'd want
plot_data <- function(data, plot_title) {
  
  print(plot_title)
  par(mfcol = c(3,2))
  boots <- bootstrap_data(data, short=T, level=1)
  plot_regs(data, boots, plot_title, level = 1)
  boots <- bootstrap_data(data, short=T, level=2)
  plot_regs(data, boots, plot_title, level = 2)
  boots <- bootstrap_data(data, short=T, level=3)
  plot_regs(data, boots, plot_title, level = 3)
  
  data$deaths <- log(data$deaths)
  boots <- bootstrap_data(data, short=T, level=1)
  plot_regs(data, boots, plot_title, level = 1, ylabel = "Log Mortality")
  boots <- bootstrap_data(data, short=T, level=2)
  plot_regs(data, boots, plot_title, level = 2, ylabel = "Log Mortality")
  boots <- bootstrap_data(data, short=T, level=3)
  plot_regs(data, boots, plot_title, level = 3, ylabel = "Log Mortality")
}

####################
##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
## and only summmer months

## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high >= 0.9))
  
data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + #Days high >90P"
plot_data(data, plot_title)

## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low >= 0.9))

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + #Days low >90P"
plot_data(data, plot_title)

## Per Capita Deaths, summer months, z score of num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high >= 0.9)) %>%
  group_by(fips, month) %>%
  mutate(z_score_high = (num_90 - mean(num_90)) / sd(num_90)) %>% 
  ungroup 

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_high, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + Zscore #Days high >90P"
plot_data(data, plot_title)

## Per Capita Deaths, summer months, z score of num days where avg low  temp in county is above 90th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low >= 0.9)) %>%
  group_by(fips, month) %>%
  mutate(z_score_high = (num_90 - mean(num_90)) / sd(num_90)) %>% 
  ungroup 

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_low, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + Zscore #Days low >90P"
plot_data(data, plot_title)

### Same as above, but 95th percentile
## Per Capita Deaths, summer months, num days where avg high temp in county is above 90th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_95 = sum(p_high >= 0.95))

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_95, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + #Days high >95P"
plot_data(data, plot_title)

## Per Capita Deaths, summer months, num days where avg high temp in county is above 95th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_95 = sum(p_low >= 0.95))

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_95, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + #Days low >95P"
plot_data(data, plot_title)

## Per Capita Deaths, summer months, z score of num days where avg high temp in county is above 95th percentile
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_95 = sum(p_high >= 0.95)) %>%
  group_by(fips, month) %>%
  mutate(z_score_high = (num_95 - mean(num_95)) / sd(num_95)) %>% 
  ungroup 

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_high, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + Zscore #Days high >95P"
plot_data(data, plot_title)

## Per Capita Deaths, summer months, z score of num days where avg high temp in county is above 95th percentile
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_95 = sum(p_low >= 0.9)) %>%
  group_by(fips, month) %>%
  mutate(z_score_low  = (num_95 - mean(num_95)) / sd(num_95)) %>% 
  ungroup 

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_low, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*100000)
data <- na.omit(data)

plot_title <- "Deaths per 100K + Zscore #Days low >95P"
plot_data(data, plot_title)






