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
data <- readRDS("./heatwaves_manual/data_with_demo_05_2022.RDS")
td <- format(Sys.Date(), "%m_%d_%Y")
data <- data %>% filter(!is.na(maxdemo)) %>% mutate(year = as.factor(year))

#### remove smoke days ----
smoke_days <- c(seq(as.Date("2020-08-19"), as.Date("2020-08-24"), by = 1),
                seq(as.Date("2020-09-10"), as.Date("2020-09-14"), by = 1),
                as.Date("2020-08-31", format = "%Y-%m-%d"))
data <- data %>% filter(!(date %in% smoke_days))


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


pdf(paste0("./visuals/pub_figures/fig6_temp", td, ".pdf"))

ggplot(data = data %>% filter(mean_high_c >= 34 & visitors_percap > quantile(visitors_percap, .95)), 
       aes(mean_high_c, visitors_percap, group = year))+
  geom_point(aes(color = year)) +
  labs(title = "MI > 95th% temp > 34") + 
  theme_bw()

ggplot(data = data %>% filter(mean_high_c >= 34 & visitors_percap > quantile(visitors_percap, .95)), 
       aes(mean_high_c, visitors_percap, group = maxdemo))+
  geom_point(aes(color = maxdemo)) +
  labs(title = "MI > 95th% temp > 34") + 
  theme_bw()

ggplot(data = data %>% filter(mean_high_c >= 34 & visitors_percap < quantile(visitors_percap, .95)), 
       aes(mean_high_c, visitors_percap, group = year))+
  geom_point(aes(color = year)) +
  labs(title = "MI < 95th% temp > 34") + 
  theme_bw()

ggplot(data = data %>% filter(mean_high_c >= 34 & visitors_percap < quantile(visitors_percap, .95)), 
       aes(mean_high_c, visitors_percap, group = maxdemo))+
  geom_point(aes(color = maxdemo)) +
  labs(title = "MI < 95th% temp > 34") + 
  theme_bw()

dev.off()
  
  


