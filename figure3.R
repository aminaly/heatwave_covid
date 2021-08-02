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
  data <- data %>% mutate(xvar_bin = cut(xvar, BINS, labels = F)) %>% 
    mutate(xvar_bin = factor(xvar_bin, levels = as.character(1:BINS)))
  
  #separate the two datasets
  data_1 <- data %>% filter(year %in% c(2018,2019))
  data_2 <- data %>% filter(year == 2020)

  #get the results of the model
  model_1 <- fe_model(data_1, level = LVL)
  model_2 <- fe_model(data_2, level = LVL)
  
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

