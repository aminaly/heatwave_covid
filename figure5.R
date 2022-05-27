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

## global vars
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
arg <- as.numeric(args[1])

## read in and adjust the regression data
data <- readRDS("./heatwaves_manual/data_for_regression_03_2022.RDS")
data <- data %>% mutate(visitors_percap = (stops_by_day - number_devices_residing)/ number_devices_residing,
                        countymonth = paste0(fips,month)) %>%
  filter(!is.na(visitors_percap) & is.finite(visitors_percap))
td <- format(Sys.Date(), "%m_%d_%Y")

#### remove smoke days ----
smoke_days <- c(seq(as.Date("2020-08-19"), as.Date("2020-08-24"), by = 1),
                seq(as.Date("2020-09-10"), as.Date("2020-09-14"), by = 1),
                as.Date("2020-08-31", format = "%Y-%m-%d"))
data <- data %>% filter(!(date %in% smoke_days))

#### pull in metadata for labels ----
metadata <- read_csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/metadata/cbg_field_descriptions.csv"))

#### add in income and population data ----
income <- read.csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b19.csv"), stringsAsFactors = F, header = T)
income <- na.omit(income %>% dplyr::select(census_block_group, 	median_income = B19013e1))
income <- income %>% mutate("census_block_group" = ifelse(nchar(census_block_group) == 11, 
                                                          paste0("0", census_block_group), census_block_group))
income$fips <- substr(income$census_block_group, 1, 5)
income <- income %>% mutate(income_group = ntile(median_income, 5)) %>% filter(fips %in% included_fips)
income$census_block_group <- as.character(income$census_block_group)

pops <- cbg_pop %>% mutate(census_block_group = as.character(poi_cbg)) %>% 
  mutate(census_block_group =  ifelse(nchar(census_block_group) == 11, 
                                      paste0("0", census_block_group), census_block_group)) %>% 
  select(census_block_group, unweighted_pop)
pops$fips <- str_sub(pops$census_block_group, 1,5)
pops <- pops %>% filter(fips %in% included_fips)

pop_income <- left_join(income, pops, by = c("census_block_group", "fips"))

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

demo <- left_join(pop_income, race, by = c("census_block_group", "fips"))

demo <- demo  %>%
  mutate(p_white = white/unweighted_pop, p_black = black/unweighted_pop, p_latinx = latinx/unweighted_pop, p_asian = asian/unweighted_pop,
         p_native = native/unweighted_pop, p_two_or_more = two_or_more/unweighted_pop) %>%
  select(p_white, p_black, p_latinx, p_asian, p_native, p_two_or_more, census_block_group, fips)

demo$maxdemo <- colnames(demo[,1:6])[max.col(demo[,1:6])]

### combine with data  ----

data <- data %>% mutate(monthday = yday(date), hot = ifelse(mean_high_c >= 34, "Hot Day", "Regular Day"))
data <- left_join(data, demo %>% select(census_block_group, fips, maxdemo), by = c("census_block_group", "fips"))
data <- data %>% filter(!is.na(maxdemo)) %>% mutate(year = as.factor(year))

#### lets do some plots ----
plot_data_bin <- function(data, plot_title, xlab="Temp (C)", ylab = "# Visitors / Home Devices", summer = F) {
  
  #create the bins
  LVL <- "bin"
  if(summer) {
    BINS <- 7
    data_s <- data %>% filter(between(month, 5, 9))
    data_s <- data_s %>% mutate(xvar_bin = cut(xvar, breaks = c(-Inf, seq(16, 39, 4), Inf), labels = F)) %>% 
      mutate(xvar_bin = factor(xvar_bin, levels = as.character(1:BINS))) %>% filter(!is.na(xvar_bin))
  } else {
    BINS <- 8
    data_s <- data %>% mutate(xvar_bin = cut(xvar, breaks = c(-Inf, seq(10, 35, 4), Inf), labels = F)) %>% 
      mutate(xvar_bin = factor(xvar_bin, levels = as.character(1:BINS))) %>% filter(!is.na(xvar_bin))
  }
  
  #separate the two datasets
  data_1 <- data_s %>% filter(year == "2020")
  data_2 <- data_s %>% filter(year == "2021")
  
  #get the results of the model
  model_1 <- fe_model(data_1, level = LVL)
  model_2 <- fe_model(data_2, level = LVL)
  
  #bootstrap the data by doing the model however many times, getting the coefficient values (1)
  boots_1 <- bootstrap_data(data_1, short=T, level= LVL)
  boots_1 <- boots_1[-1,]
  boots_1 <- as_tibble(boots_1) %>% mutate(xvar_bin1 = 0) %>% relocate(xvar_bin1)
  conf_1 <- apply(boots_1,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  conf_1 <- as.data.frame(cbind(1:BINS, conf_1[1,], conf_1[2,], conf_1[3,]))
  colnames(conf_1) <- c("term", "low", "mid", "upper")   
  conf_1$term <- row.names(conf_1)
  
  coefs_1 <- tidy(model_1, conf.int = T)
  coefs_1 <- coefs_1[grepl("xvar_bin", coefs_1$term),]
  coefs1_1 <- coefs_1[1,] %>% mutate(term = "xvar_bin1", estimate = 0, std.error = 0, statistic = 0, p.value = 0, conf.low = 0, conf.high = 0)
  coefs_1 <- rbind(coefs1_1, coefs_1)
  coefs_1$grp <- "2020"
  coefs_1 <- left_join(coefs_1, conf_1, by = "term")
  
  #bootstrap the data by doing the model however many times, getting the coefficient values (2)
  boots_2 <- bootstrap_data(data_2, short=T, level= LVL)
  boots_2 <- boots_2[-1,]
  boots_2 <- as_tibble(boots_2) %>% mutate(xvar_bin1 = 0) %>% relocate(xvar_bin1)
  conf_2 <- apply(boots_2,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  conf_2 <- as.data.frame(cbind(1:BINS, conf_2[1,], conf_2[2,], conf_2[3,]))
  colnames(conf_2) <- c("term", "low", "mid", "upper")   
  conf_2$term <- row.names(conf_2)
  
  coefs_2 <- tidy(model_2, conf.int = T)
  coefs_2 <- coefs_2[grepl("xvar_bin", coefs_2$term),]
  coefs1_2 <- coefs_2[1,] %>% mutate(term = "xvar_bin1", estimate = 0, std.error = 0, statistic = 0, p.value = 0, conf.low = 0, conf.high = 0)
  coefs_2 <- rbind(coefs1_2, coefs_2)
  coefs_2$grp <- "2021"
  coefs_2 <- left_join(coefs_2, conf_2, by = "term")
  
  coefs <- rbind(coefs_1, coefs_2)
  
  ## plot  data
  ggplot(data = coefs, aes(term, estimate, group = grp))+
    geom_ribbon(data = coefs, aes(ymin = low, ymax = upper, group = grp, fill = grp), linetype=2, alpha = 0.25) +
    scale_fill_brewer(palette = "Set1") +
    geom_point(data = coefs, aes(term, estimate))+ 
    geom_line(data = coefs, aes(group = grp)) +
    labs(title = plot_title) +
    theme(axis.text.x = element_text(angle = 90)) + 
    theme_bw()
  
}

#### plot summer and non summer ----
#demo <- unique(data$maxdemo)[arg]
#data_demo <- data %>% filter(maxdemo == demo)
data <- data_demo %>% mutate(xvar = mean_high - 273.15, yvar = visitors_percap)

pdf(paste0("./visuals/pub_figures/fig5_demographic_",interacted, "_", td, ".pdf"))

## plot binned data for full year
plot_title <- paste0("Mobility Index v Avg High Year ", demo)
plot_data_bin(data, plot_title, xlab = "high_temp (0-40C)")

## plot binned data summer only 
plot_title <- paste0("Mobility Index v Avg High Summer Year ", demo)
plot_data_bin(data, plot_title, xlab = "high_temp (0-40C)", summer = T)

dev.off()


