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
t <- read_rds("./heatwaves_manual/all_temperature_data_clean.rds")
m <- read_rds("./calculated/all_mortality.rds")

## Recalculate z-scores for just the summer months and add in percentile value
t_zs <- t %>% group_by(fips, month) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = pnorm(z_score_high)) %>%
  mutate(p_low = pnorm(z_score_low)) %>%
  ungroup

pdf(paste0("visuals/regressions", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run

####################
##For this set of regressions, we're going to do per capita deaths, regular and log mortality,
## and only summmer months

## Per Capita Deaths, summer months, avg high temp in county, max high temp in month
t_high <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_high >= 0.9))
  
data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = num_90, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Deaths per 10K + Num Days above 90th Percentile", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Deaths per 10K + Num Days above 90th Percentile", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Deaths per 10K + Num Days above 90th Percentile", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Deaths per 10K + Num Days above 90th Percentile", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Deaths per 10K + Num Days above 90th Percentile", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Deaths per 10K + Num Days above 90th Percentile", level = 3, ylabel = "Log Mortality")

## Per Capita Deaths, summer months, avg low temp in county, max high temp in month
t_low <- t_zs %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(num_90 = sum(p_low >= 0.9))

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_low_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_low_linear")
plot_regs(data, boots, "Deaths per 10K + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_low_quad")
plot_regs(data, boots, "Deaths per 10K + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_low_cub")
plot_regs(data, boots, "Deaths per 10K + Max low in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_low_linear")
plot_regs(data, boots, "Deaths per 10K + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_low_quad")
plot_regs(data, boots, "Deaths per 10K + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_low_cub")
plot_regs(data, boots, "Deaths per 10K + Max low in Month", level = 3, ylabel = "Log Mortality")













