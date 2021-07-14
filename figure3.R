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
  data <- data %>% mutate(xvar_bin = cut(xvar, BINS, labels = F)) %>% mutate(xvar_bin = factor(xvar_bin, 
                                                                                               levels = as.character(1:BINS)))
  dataset <- NA
  
  print(plot_title)
  
  #get the results of the model
  model <- fe_model(data, level = LVL)
  
  #bootstrap the data by doing the model however many times, getting the coefficient values
  boots <- bootstrap_data(data, short=T, level= LVL)
  boots <- boots[-1,]
  boots <- as_tibble(boots) %>% mutate(xvar_bin1 = 0) %>% relocate(xvar_bin1)
  conf <- apply(boots,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  conf <- as.data.frame(cbind(1:BINS, conf[1,], conf[2,], conf[3,]))
  colnames(conf) <- c("term", "low", "mid", "upper")   
  conf$term <- row.names(conf)
  
  #dataset <- build_plot_dataset(data, boots, plot_title, level = LVL, xlab = xlab, ylab = ylab, model=model)
  coefs <- tidy(model, conf.int = T)
  coefs <- coefs[grepl("xvar_bin", coefs$term),]
  coefs1 <- coefs[1,] %>% mutate(term = "xvar_bin1", estimate = 0, std.error = 0, statistic = 0, p.value = 0, conf.low = 0, conf.high = 0)
  coefs <- rbind(coefs1, coefs)
  coefs$grp <- "all"
  
  coefs <- left_join(coefs, conf, by = "term")
  
  ggplot(data = coefs, aes(term, estimate))+
    geom_ribbon(aes(ymin = low, ymax = upper, group = 1), linetype=2, alpha = 0.25, fill = "#fec44f")+
    geom_point(aes(term, estimate))+ 
    geom_line(aes(group = grp)) +
    labs(title = plot_title) +
    theme(axis.text.x = element_text(angle = 90)) + 
    theme_bw()
  
}

data <- rename(data, xvar = mean_high_c)
data <- rename(data, yvar = visitors_percap)

data_old <- data %>% filter(year %in% c(2018,2019))
plot_title <- paste0("Mobility Index v Avg High 2018-19")
plot_data_bin(data_old, plot_title, xlab = "high_temp (0-40C)")

## plot binned data for 2020
data_2020 <- data %>% filter(year == 2020)
plot_title <- paste0("Mobility Index v Avg High 2020")
plot_data_bin(data_2020, plot_title, xlab = "high_temp (0-40C)")

## plot binned data for 2018/19 summer only 
data_summer <- data %>% filter(between(month.x, 5, 9)) %>% filter(year %in% c(2018,2019))
plot_title <- paste0("Mobility Index v Avg High Summer 2018-19")
plot_data_bin(data_summer, plot_title, xlab = "high_temp (0-40C)")

## plot binned data for 2020 summer only 
data_2020_summer <- data_2020 %>% filter(between(month.x, 5, 9))
plot_title <- paste0("Mobility Index v Avg High 2020 Summer")
plot_data_bin(data_2020_summer, plot_title, xlab = "high_temp (0-40C)")

## reset xvar to normalized z_score value
data <- rename(data, mean_high_c = xvar)
data <- rename(data, xvar = z_score_high)

## plot binned data for 2018/19 
data_old <- data %>% filter(year %in% c(2018,2019))
plot_title <- paste0("Mobility Index v Avg High")
plot_data_bin(data_old, plot_title, xlab = "Z_Score of Temp")

## plot binned data for 2020
data_2020 <- data %>% filter(year == 2020)
plot_title <- paste0("Mobility Index v Avg High 2020")
plot_data_bin(data_2020, plot_title, xlab = "Z_Score of Temp")

## plot binned data for 2018/19 summer only 
data_summer <- data %>% filter(between(month.x, 5, 9)) %>% filter(year %in% c(2018,2019))
plot_title <- paste0("Mobility Index v Avg High Summer 2018-19")
plot_data_bin(data_summer, plot_title, xlab = "Z_Score of Temp")

## plot binned data for 2020 summer only 
data_2020_summer <- data_2020 %>% filter(between(month.x, 5, 9))
plot_title <- paste0("Mobility Index v Avg High 2020 Summer")
plot_data_bin(data_2020_summer, plot_title, xlab = "Z_Score of Temp")

dev.off()

