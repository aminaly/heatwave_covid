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

#### add in demographic data (race) ----
race <- read_csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b02.csv"))
latinx <- read_csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b03.csv"))

race <- race %>% mutate(fips = substr(census_block_group, 1, 5)) %>%
  filter(fips %in% included_fips) %>%
  select(census_block_group, fips, white = B02001e2, black = B02001e3, native = B02001e4, asian = B02001e5,
         pacificisld = B02001e6, other = B02001e7, two_or_more = B02001e8)

latinx <- latinx %>% mutate(fips = substr(census_block_group, 1, 5)) %>%
  filter(fips %in% included_fips) %>% 
  select(census_block_group, fips, not_latinx = B03003e2, latinx = B03003e3)

race <- left_join(race, latinx, by = c("census_block_group", "fips"))

demo <- left_join(data, race, by = c("census_block_group", "fips"))
ys <- c(2020, 2021)

#### set up quantiles ----
quantiles <- c(.25, .5, .75, .95)

income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% select(census_block_group, income_group_pop)
data <- left_join(data, income, by = "census_block_group")

hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)
data_subset <- data %>% filter(date %in% hotdays & year %in% ys) %>% mutate(minority = ifelse(maxdemo %in% c("p_white", "p_asian"), FALSE, TRUE))
data_all <- data %>% filter(year %in% ys) %>% mutate(hotday = ifelse(date %in% hotdays, T, F))
results <- c()

#### wilcox and ks test ----

tests <- c()
for(yr in 2020:2021) {
  
  ds <- data_subset %>% filter(year == yr)
  
  for(inc in 1:5) {
    for(comp in 1:5) {
      if(inc == comp) next
      
      tryCatch({
        w <- wilcox.test(ds %>% filter(income_group_pop == inc) %>% pull(visitors_percap), 
                         ds %>% filter(income_group_pop == comp) %>% pull(visitors_percap))
        ds_wilcox <- round(w$p.value, 10)
      }, error=function(e){ds_wilcox <- NA})
      
      ds_ks <- tryCatch({
        w <- ks.test(ds %>% filter(income_group_pop == inc) %>% pull(visitors_percap), 
                     ds %>% filter(income_group_pop == comp) %>% pull(visitors_percap))
        ds_ks <- round(w$p.value, 10)
      }, error=function(e){ds_ks <- NA})
      
      
      tests <- rbind(cbind(wilcox = round(ds_wilcox, 3),
                           ks = round(ds_ks, 3), income_grp = inc, year = yr, comparison_year = comp), tests)
      
    }
  }
}

pdf(paste0("./visuals/pub_figures/fig8_dist", td, ".pdf"))

ggplot(data = data_subset %>% filter(visitors_percap <= 20), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI <= 4.5 (95th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_subset %>% filter(visitors_percap > 4.5 & visitors_percap < 6.75), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI > 4.5 (95th percentile) \n & < 20 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()
ggplot(data = data_subset %>% filter(visitors_percap > 6.75 & visitors_percap < 20), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI > 6.75 (97.5th percentile) \n & < 20 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_subset %>% filter(visitors_percap >= 20), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI >= 20 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_subset, 
       aes(x = visitors_percap, group = as.factor(year),
           color = as_factor(year), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34") +  
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 6.75) + 
  geom_vline(xintercept = 20) +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

dev.off()
