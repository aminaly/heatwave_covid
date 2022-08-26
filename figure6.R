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

#### plots ----
quantiles <- c(.25, .5, .75, .95)

income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% select(census_block_group, income_group_pop)
data <- left_join(data, income, by = "census_block_group")

hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)

data_subset <- data %>% filter(date %in% hotdays & year %in% ys & !is.na(visitors_percap) & !is.na(median_income)) %>% mutate(income_group_pop = as.factor(income_group_pop))
data_all <- data %>% filter(year %in% ys) %>% mutate(hotday = ifelse(date %in% hotdays, T, F))


run_model <- function(dta, xvar, yvar) {
  mod1 <- felm(xvar ~ poly(yvar, 2, raw = T) | county + monthweek,  data = data_subset)
  mod2 <- felm(xvar ~ yvar | county + monthweek,  data = data_subset)
  return(list(mod1, mod2))
}

model_income_bin <- felm(visitors_percap ~ income_group_pop | county + monthweek,  data = data_subset)


model_income_cont <- felm(visitors_percap ~ median_income | county + monthweek,  data = data_subset)
model_income_cont_quad <- felm(visitors_percap ~ poly(median_income, 2, raw = T) | county + monthweek,  data = data_subset)



pdf(paste0("./visuals/pub_figures/view", td, ".pdf"))

##############
x <- as.data.frame(c(11000:250000))
names(x) <- c("x")
int <- tidy(model_income_cont, conf.int = T)
x <- x %>% mutate(ycont = x * int$estimate, high = x * int$conf.high, low = x * int$conf.low) %>%
  mutate(ycont = ycont - ycont[77001], high = high - high[77001], low = low[77001] )

ggplot(x, aes(x, y_cont)) +
  geom_ribbon(data = x, aes(ymin = low, ymax = high)) +
  #geom_point(aes(x, y_cont)) +
  theme_bw()

############## 
x <- as.data.frame(c(11000:250000))
names(x) <- c("x")
int <- tidy(model_income_cont_quad, conf.int = T)
x <- x %>% mutate(ycont = x * int$estimate[1] + x^2*int$estimate[2], 
                  high = x * int$conf.high[1] + x^2*int$conf.high[2], 
                  low = x * int$conf.low[1] + x^2*int$conf.low[2]) %>%
  mutate(ycont = ycont - ycont[77001], high = high - high[77001], low = low[77001] )

ggplot(x, aes(x, y_cont)) +
  geom_ribbon(data = x, aes(ymin = low, ymax = high)) +
  #geom_point(aes(x, y_cont)) +
  theme_bw()

############## 
coef <- tidy(model_income_bin, conf.int = T)
coef <- rbind(c("income_group_pop1", 0, 0, 0, 0, 0, 0), coef)

ggplot(coef, aes(term, estimate)) +
  geom_ribbon(coef, aes(ymin = conf.low, ymax = conf.high)) +
  geom_point(data = coef, aes(term, estimate))+ 
  geom_line(data = coef, aes(term, estimate)) +
  theme_bw()

dev.off()