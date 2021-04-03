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

##Read in datasets
t <- read_rds(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean_2021s.rds"))
r_master <- read_csv(paste0(getwd(), "/us_census/climate_regions.csv"))
m_master <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))
s <- read_rds(paste0(getwd(), "/heatwaves_manual/all_sheltering_raw_fips.rds"))

#get income and population data
income <- read.csv(paste0(getwd(), "/us_census/income_county.csv"), stringsAsFactors = F, header = T)
population <- read_rds(paste0(getwd(), "/calculated/fips_population.rds"))

# get state and regional information
m_master$state <- str_sub(m_master$county, -2)
m_master <- left_join(m_master, r_master, by= "state")
m_master <- unique(m_master %>% select(fips, state, region, region_s))
s <- left_join(s, m_master, by = "fips")

#set up mortality & add in median income
income$fips <- as.character(income$fips)
income <- income %>% mutate(income_group = ntile(median_income, 10))
i <- income %>% mutate("fips" = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  dplyr::select(fips, median_income, poverty_percent, income_group)
i <- i[-1,]

#combine shelter with icncome, and select region if we want it
shelter <- left_join(s, i, by = c("fips"))
shelter <- shelter %>% ungroup(date) %>% mutate(date = as.Date(date))
shelter$stateyear <- paste0(shelter$state, year(shelter$date))

pdf(paste0("./visuals/regressions", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Sheltering April - Dec"),
     cex = 1.5, col = "black")

####################
## Quick function that takes data and plots all the variations we'd want
plot_data <- function(data, plot_title, lows=FALSE) {
  
  
  mytheme <- gridExtra::ttheme_default(base_size = 5)
  #global vars
  data$yvar <- log(data$yvar)
  xlab <- "High Temp in County"
  
  ## do 2019
  data2019 <- data %>% filter(date >= "2019-04-01" & date <= "2019-11-07")
  plot_title1 <- paste(plot_title, "2019")
  par(mfcol = c(3,2))
  print(plot_title)
  model <- fe_model(data2019, level = 2)
  boots <- bootstrap_data(data2019, short=T, level=2)
  plot_regs(data2019, boots, plot_title, level = 2, xlabel = xlab, ylabel = "Shelter Index", model=model)
  
  # #table of coefs
  grid.table(tidy(model)[1:2,], theme = mytheme)
  
  ## do 2020
  plot_title1 <- paste(plot_title, "2020")
  data2020 <- data %>% filter(date >= "2020-04-01")
  model <- fe_model(data2020, level = 2)
  boots <- bootstrap_data(data2020, short=T, level=2)
  plot_regs(data2020, boots, plot_title, level = 2,xlabel = xlab, ylabel = "Log Shelter Index", model = model)
  
  # #table of coefs
  grid.table(tidy(model)[1:2,], theme = mytheme)
}

####################
## Lets run the regressions for selected regions
regions <- unique(r_master$region_s)

## Combined temperature and sheltering by fips 
data <- left_join(shelter, t, by = c("fips", "date"))
data <- data %>% mutate(mean_low_c = mean_low-273.15, mean_high_c = mean_high-273.15)

## run regression for sheltering index as a function of average high temp in county
data <- rename(data, measure = mean_high_c)
data <- rename(data, yvar = shelter_index)

# lets start an empty df for all coefficients 
model_output <- c()

# take out dates after sheltering began
for(region in regions) {
  print(region)
  plot_title <- paste0("Shelter Index v Avg High in County \n", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title)
}

## get just summer months, and do it again
data <- data %>% dplyr::filter(between(month, 5, 9)) 

for(region in regions) {
  print(region)
  plot_title <- paste0("Shelter Index v Avg High in County \n Summer Only", region)
  data_reg <- data %>% dplyr::filter(region_s == region)
  plot_data(data_reg, plot_title)
}

saveRDS(model_output, "calculated/pre_covid_model_output.rds")

