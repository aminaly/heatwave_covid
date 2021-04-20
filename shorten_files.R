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
all_files <- list.files("heatwaves_manual/temps/bg/", full.names = T)
f <- readRDS(all_files[1])
f <- f[1:20000,]

saveRDS(f, (paste0(getwd(),"/heatwaves_manual/example_blockgroup.rds")))
