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

##Read in datasets
p <- readRDS("heatwaves_manual/patterns_raw_blockgroup.rds")
p <- p[1:2000]


saveRDS(f, (paste0(getwd(),"/heatwaves_manual/short_patterns.rds")))
