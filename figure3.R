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
readRDS("./heatwaves_manual/data_for_regression.rds")

plot_data_bin <- function(data, plot_title, xlab="Temp (C)", ylab = "# Visitors / Home Devices", BINS = 10) {
  
  #create the bins
  LVL <- "bin"
  data <- data %>% mutate(xvar_bin = cut(xvar, BINS, labels = F)) %>% mutate(xvar_bin = factor(xvar_bin, 
                                                                                               levels = as.character(1:BINS)))
  dataset <- NA
  
  print(plot_title)
  model <- fe_model(data, level = LVL)
  boots <- bootstrap_data(data, short=T, level= LVL)
  dataset <- build_bin_plot_dataset(data, boots, plot_title, level = LVL, xlab = xlab, ylab = ylab, model=model, bins = BINS)
  dataset <- dataset[-1,]
  
  coefs <- tidy(model, conf.int = T)
  coefs <- coefs[grepl("xvar_bin", coefs$term),]
  coefs$sig <- coefs$p.value < 0.05
  
  ggplot(coefs, aes(term, estimate))+
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
    geom_point(aes(group = sig, color=as.factor(sig))) +
    labs(title = plot_title, xlab = xlab) +
    theme(axis.text.x = element_text(angle = 90)) 
  
  ggplot(data = dataset, aes(x=x, y=mid)) +
    geom_line(aes(group=bin, color=as.factor(bin))) +
    geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.1) +
    geom_rug(sides="b") +
    ggtitle(plot_title) + ylab(ylab) + xlab(xlab) +
    scale_x_continuous() +
    theme(text = element_text(size = 15)) + 
    theme_bw() 
  
  #cat_plot(model, pred = xvar_bin, geom = "line", plot.points = T)
  
}
