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

data <- data %>% mutate(tempgrp = ifelse(month.y %in% c(1:3, 11:12), "cold", "warm"))
data <- data %>% group_by(tempgrp, year) %>% 
  summarize(temp = mean(mean_high_c, na.rm = T), 
            lowest = min(mean_high_c, na.rm = T), 
            highest = max(mean_high_c, na.rm = T), 
            mi = mean(visitors_percap, na.rm = T))