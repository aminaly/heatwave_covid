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
library(sjPlot)
library(sjmisc)
library(prediction)

##Read in datasets
t <- read_rds("./heatwaves_manual/all_temperature_data_clean.rds")
m <- read_rds("./calculated/all_mortality.rds")

pdf("visuals/regressions.pdf")
##Finalize datasets for regressions & run

####################
## Total Deaths, all months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T))

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_high_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Total Deaths +  Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Total Deaths +  Max High in Month", level = 3, ylabel = "Log Mortality")


## Total Deaths, summer months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T))

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_high_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 3)

data$deaths <- log(data$deaths)

boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 3, ylabel = "Log Mortality")

## PerCapita Deaths, all months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T))

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_high_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 3, ylabel = "Log Mortality")

## PerCapita Deaths, summer months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T))

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_high_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 3, ylabel = "Log Mortality")

#######################
# Same as above, but use Z-score high

## Total Deaths, all months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_high = (maximum_high_temp_in_month - mean(maximum_high_temp_in_month)) / sd(maximum_high_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_high, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Total Deaths +  Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Total Deaths + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Total Deaths +  Max High in Month", level = 3, ylabel = "Log Mortality")


## Total Deaths, summer months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_high = (maximum_high_temp_in_month - mean(maximum_high_temp_in_month)) / sd(maximum_high_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_high, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Total Deaths Summer + Max High in Month", level = 3, ylabel = "Log Mortality")

## PerCapita Deaths, all months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_high = (maximum_high_temp_in_month - mean(maximum_high_temp_in_month)) / sd(maximum_high_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_high, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>%
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_high_linear")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_high_quad")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_high_cub")
plot_regs(data, boots, "Deaths per 10K + Max High in Month", level = 3, ylabel = "Log Mortality")

## PerCapita Deaths, summer months, avg high temp in county, max high temp in month
t_high <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_high_temp_in_month = max(mean_high, na.rm = T),
            avg_high_temp_in_month = mean(mean_high, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_high = (maximum_high_temp_in_month - mean(maximum_high_temp_in_month)) / sd(maximum_high_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_high, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_high, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_high_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_high_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_high_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max High in Month", level = 3, ylabel = "Log Mortality")


########################
## Total Deaths, all months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T))

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_low_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_low_linear")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_low_quad")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_low_cub")
plot_regs(data, boots, "Total Deaths +  Max low in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_low_linear")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_low_quad")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_low_cub")
plot_regs(data, boots, "Total Deaths +  Max low in Month", level = 3, ylabel = "Log Mortality")


## Total Deaths, summer months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T))

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_low_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 3)

data$deaths <- log(data$deaths)

boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 3, ylabel = "Log Mortality")

## PerCapita Deaths, all months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T))

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

## PerCapita Deaths, summer months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T))

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = maximum_low_temp_in_month - 273.15, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 3, ylabel = "Log Mortality")

#######################
# Same as above, but use Z-score low

## Total Deaths, all months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_low = (maximum_low_temp_in_month - mean(maximum_low_temp_in_month)) / sd(maximum_low_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_low, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_low_linear")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_low_quad")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_low_cub")
plot_regs(data, boots, "Total Deaths +  Max low in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_low_linear")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_low_quad")
plot_regs(data, boots, "Total Deaths + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_low_cub")
plot_regs(data, boots, "Total Deaths +  Max low in Month", level = 3, ylabel = "Log Mortality")


## Total Deaths, summer months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_low = (maximum_low_temp_in_month - mean(maximum_low_temp_in_month)) / sd(maximum_low_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_low, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Total Deaths Summer + Max low in Month", level = 3, ylabel = "Log Mortality")

## PerCapita Deaths, all months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_low = (maximum_low_temp_in_month - mean(maximum_low_temp_in_month)) / sd(maximum_low_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_low, monthyear, county = county.x, income_group, population_est) %>% 
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

## PerCapita Deaths, summer months, avg low temp in county, max low temp in month
t_low <- t %>% group_by(county, fips, month, year, monthyear) %>%
  dplyr::filter(between(month, 5, 9)) %>%
  summarize(maximum_low_temp_in_month = max(mean_low, na.rm = T),
            avg_low_temp_in_month = mean(mean_low, na.rm = T)) %>% 
  group_by(fips, month) %>%
  mutate(z_score_low = (maximum_low_temp_in_month - mean(maximum_low_temp_in_month)) / sd(maximum_low_temp_in_month)) %>% 
  ungroup 

data <- left_join(m, t_low, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = z_score_low, monthyear, county = county.x, income_group, population_est) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure)) %>% 
  mutate(deaths = (deaths/as.numeric(population_est))*10000)
data <- na.omit(data)

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 1)
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 2)
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 3)

data$deaths <- log(data$deaths)
boots <- bootstrap_data(data, short=T, level=1, name = "total_summer_low_linear")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 1, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=2, name = "total_summer_low_quad")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 2, ylabel = "Log Mortality")
boots <- bootstrap_data(data, short=T, level=3, name = "total_summer_low_cub")
plot_regs(data, boots, "Deaths per 10K Summer + Max low in Month", level = 3, ylabel = "Log Mortality")

dev.off()
