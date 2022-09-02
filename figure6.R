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
library(gridExtra)
library(wesanderson)
library(SafeGraphR)
library(interactions)
library(quantreg)
library(jtools)

## global vars
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 

data <- readRDS("./heatwaves_manual/data_with_demo_05_2022.RDS")
data <- data %>% filter(!is.na(unweighted_pop))
td <- format(Sys.Date(), "%m_%d_%Y")
ys <- c(2020, 2021)

#### data cleaning ----
## remove smoke days
smoke_days <- c(seq(as.Date("2020-08-19"), as.Date("2020-08-24"), by = 1),
                seq(as.Date("2020-09-10"), as.Date("2020-09-14"), by = 1),
                as.Date("2020-08-31", format = "%Y-%m-%d"))
data <- data %>% filter(!(date %in% smoke_days))

quantiles <- c(.05, .5, .95)

## add in income
income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% select(census_block_group, income_group_pop)
data <- left_join(data, income, by = "census_block_group")

## add in race
race <- read_csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b02.csv"))
race <- race %>% mutate(fips = substr(census_block_group, 1, 5)) %>%
  filter(fips %in% included_fips) %>%
  select(census_block_group, fips, white = B02001e2, black = B02001e3, native = B02001e4, asian = B02001e5,
         pacificisld = B02001e6, other = B02001e7, two_or_more = B02001e8) %>%
  group_by(census_block_group, white, asian) %>%
  summarise(total = sum(white, black, native, asian, pacificisld, other, two_or_more)) %>%
  mutate(p_white = white/total, p_white_asian = (white + asian)/ total) %>% 
  mutate(p_minority = 1 - p_white, p_non_white_asian = 1 - p_white_asian)
data <- left_join(data, race, by = "census_block_group")

## mark days where one or more county is >34
hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)

data_subset <- data %>% filter(date %in% hotdays & year %in% ys & !is.na(visitors_percap) & !is.na(median_income)) %>% 
  mutate(income_group_pop = as.factor(income_group_pop))

income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop_10 = ntile(median_income, 6)) %>% select(census_block_group, income_group_pop_10)
data_subset <- left_join(data_subset, income, by = "census_block_group")

## get total number of counties experiencing extreme temperatures
a <- data_subset %>% filter(mean_high_c >= 34) %>% count(date, region)
data_subset <- left_join(data_subset, a, by = "date")

#### make some figs ----
pdf(paste0("./visuals/pub_figures/view", td, ".pdf"))

## bootstrap median income linear
coefs <- c()
for(i in 1:1000) {
  samp <- sample(1:nrow(data_subset), nrow(data_subset), replace = T)
  ds <- data_subset[samp,]
  m <- felm(visitors_percap ~ median_income | county + monthweek, data = ds)
  coefs <- rbind(m$coefficients, coefs)
}

quants <- c(.05, .5, .95)
coefs <- quantile(coefs, quants)
plot_data <- as.data.frame(x = 11000:350000)
colnames(plot_data) <- c("x")
plot_data <- plot_data %>% mutate(y = (x * coefs[2]), 
                                  upper = (x * coefs[3]),
                                  lower = (x * coefs[1])) #subtracting median of all income
plot_data <- plot_data %>% mutate(y = y - plot_data[,2][x = 88000],
                                  uppper = upper - plot_data[,3][x=88000],
                                  lower = lower - plot_data[,4][x=88000])
plot_title = "felm(visitors_percap ~ median_income | county + monthweek)"

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25) +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = plot_title) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

## bootstrap # hot counties linear
coefs <- c()
for(i in 1:1000) {
  samp <- sample(1:nrow(data_subset), nrow(data_subset), replace = T)
  ds <- data_subset[samp,]
  m <- felm(visitors_percap ~ n | census_block_group + monthweek, data = ds)
  coefs <- rbind(m$coefficients, coefs)
}

quants <- c(.05, .5, .95)
coefs <- quantile(coefs, quants)
plot_data <- as.data.frame(x = 11000:350000)
colnames(plot_data) <- c("x")
plot_data <- plot_data %>% mutate(y = (x * coefs[2]), 
                                  upper = (x * coefs[3]),
                                  lower = (x * coefs[1])) #subtracting median of all income
plot_data <- plot_data %>% mutate(y = y - plot_data[,2][x = 88000],
                                  uppper = upper - plot_data[,3][x=88000],
                                  lower = lower - plot_data[,4][x=88000])
plot_title = "visitors_percap ~ n | census_block_group + monthweek"

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25) +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = plot_title) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

dev.off()