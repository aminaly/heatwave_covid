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
quantiles <- c(.05, .5, .95)
ys <- c(2020, 2021)


data <- readRDS("./heatwaves_manual/data_with_demo_05_2022.RDS")
td <- format(Sys.Date(), "%m_%d_%Y")

#### data cleaning ----
## remove smoke days
smoke_days <- c(seq(as.Date("2020-08-19"), as.Date("2020-08-24"), by = 1),
                seq(as.Date("2020-09-10"), as.Date("2020-09-14"), by = 1),
                as.Date("2020-08-31", format = "%Y-%m-%d"))

## clean up nas 
data <- data %>% filter(!(date %in% smoke_days)) %>% 
  filter(!is.na(unweighted_pop)) %>% 
  filter(year %in% ys & !is.na(visitors_percap) & !is.na(median_income)) %>%
  mutate(monthweekyr = paste0(monthweek, year))

## select only months where we know temps hit 95th percentile 
data <- data %>% filter(month %in% c(5:9))

## add in income
income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% select(census_block_group, income_group_pop) %>% 
  mutate(income_group_pop = as.factor(income_group_pop)) 
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
data <- data %>% mutate(visitors_percap_log = log(visitors_percap + 1.01)) 

income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop_10 = ntile(median_income, 6)) %>% 
  select(census_block_group, income_group_pop_10) 

data <- left_join(data, income, by = "census_block_group")

## get total number of counties experiencing extreme temperatures
b <- data %>% filter(mean_high_c >= 34) %>% count(date, region)
data <- left_join(data, b, by = "date")
data <- data %>% mutate(n = ifelse(is.na(n), 0, n))

## mark days where one or more county is >34
hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)
data_subset <- data %>% filter(date %in% hotdays)

#### make some figs ----
pdf(paste0("./visuals/pub_figures/view", td, ".pdf"))

#### bootstrap median income linear ----
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

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=77001]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=77001]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=77001]
plot_title = "felm(visitors_percap ~ median_income | county + monthweek)"

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25) +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = plot_title) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### bootstrap # hot counties linear ----
coefs <- c()
for(i in 1:1000) {
  samp <- sample(1:nrow(data_subset), nrow(data_subset), replace = T)
  ds <- data_subset[samp,]
  m <- felm(visitors_percap ~ poly(n, 2, raw = T) | census_block_group + monthweek, data = ds)
  coefs <- cbind(m$coefficients, coefs)
}

quants <- c(.05, .5, .95)
coefs_1 <- quantile(coefs[1,], quants)
coefs_2 <- quantile(coefs[2,], quants)
plot_data <- as.data.frame(x = 1:4700)
colnames(plot_data) <- c("x")

y <- plot_data$x*coefs_1[2] + plot_data$x^2*coefs_2[2]  
plot_data$y <- y - y[x=740]

upper <- plot_data$x*coefs_1[3] + plot_data$x^2*coefs_2[3]
plot_data$upper <- upper - upper[x=740]

lower <- plot_data$x*coefs_1[1] + plot_data$x^2*coefs_2[1]
plot_data$lower <- lower - lower[x=740]

plot_title = "MI change as number of CBGs with temps > 34 increases  
visitors_percap ~ poly(n, 2, raw = T) | census_block_group + monthweek"

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25) +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = plot_title, x = "num CBGs >= 34C", y = "unit change in MI") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### binned n ----

data_subset <- data_subset %>% mutate(n_bin = ntile(n, 6))
m <- felm(visitors_percap ~ as.factor(n_bin) | census_block_group + monthweek, data=data_subset)

m <- as.data.frame(cbind(x = 1:6, y = c(0, m$coefficients)))

## plot both the 2018-19 and 2020 data
ggplot(data = m, aes(x, y))+
  geom_point(data = m, aes(x, y))+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

dev.off()


#### quantile regression ----


#### fixed effect ----

#### all data MI v Temp ----
m <- felm(visitors_percap_log ~ mean_high_c | census_block_group + monthweekyr, data = data)

## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:1000) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_log ~ mean_high_c | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 0:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=24]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=24]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=24]
plot_title = "felm(visitors_percap_log ~ mean_high_c | census_block_group + monthweekyr, data = data) \n May-Sept"

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = plot_title, x = "Temperature C", y = "log(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI v Temp : Income----
m_inter <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + monthweekyr, data = data)

coefs_interacted <- c()
for(i in 1:1000) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + monthweekyr, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=0,  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Income Group", y = "Change in MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept \n 
       felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()



