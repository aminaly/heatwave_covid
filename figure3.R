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

## read in the regression data
data <- readRDS("./heatwaves_manual/data_for_regression.rds")

## lets do some plots
pdf(paste0("./visuals/figure3", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

plot_data_bin <- function(data, plot_title, xlab="Temp (C)", ylab = "# Visitors / Home Devices") {
  
  #create the bins
  LVL <- "bin"
  BINS <- 9
  data <- data %>% mutate(xvar_bin = cut(xvar, breaks = seq(0, 45, 5), labels = F))
  #separate the two datasets
  data_1 <- data %>% filter(year %in% c(2018,2019))
  data_2 <- data %>% filter(year == 2020)

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
  coefs_1$grp <- "2018-19"
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
  coefs_2$grp <- "2020"
  coefs_2 <- left_join(coefs_2, conf_2, by = "term")
  
  coefs <- rbind(coefs_1, coefs_2)
  
  ## plot both the 2018-19 and 2020 data
  ggplot(data = coefs, aes(term, estimate, group = grp))+
    geom_ribbon(data = coefs, aes(ymin = low, ymax = upper, group = grp, fill = grp), linetype=2, alpha = 0.25) +
    scale_fill_brewer(palette = "Set1") +
    geom_point(data = coefs, aes(term, estimate))+ 
    geom_line(data = coefs, aes(group = grp)) +
    labs(title = plot_title) +
    theme(axis.text.x = element_text(angle = 90)) + 
    theme_bw()
  
}

data <- rename(data, xvar = mean_high_c)
data <- rename(data, yvar = visitors_percap)

## plot binned data for full year
plot_title <- paste0("Mobility Index v Avg High pre + post 2020")
plot_data_bin(data, plot_title, xlab = "high_temp (0-40C)")

## plot binned data for 2018/19 summer only 
data_summer <- data %>% filter(between(month.x, 5, 9))
plot_title <- paste0("Mobility Index v Avg High Summer pre + post 2020")
plot_data_bin(data_summer, plot_title, xlab = "high_temp (0-40C)")

dev.off()

