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
library(quantreg)
library(jtools)

## global vars
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 

data <- readRDS("./heatwaves_manual/data_with_demo_05_2022.RDS")
data <- data %>% filter(!is.na(unweighted_pop))
td <- format(Sys.Date(), "%m_%d_%Y")

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

demo <- left_join(data, race, by = c("census_block_group", "fips"))
ys <- c(2020, 2021)

#### historgram of demographics within income group ----
race <- left_join(race, data %>% select(census_block_group, unweighted_pop, income_group_pop), by = "census_block_group")
race <- unique(race)
racem <- melt(race, id = c("census_block_group", "fips", "unweighted_pop", "income_group_pop"))
racem <- racem %>% group_by(income_group_pop, variable) %>% summarize(pop = sum(value, na.rm = T))

racem_nh <- racem %>% filter(!is.na(income_group_pop) & !variable %in% c("not_latinx", "latinx")) 
racem_h <- racem %>% filter(!is.na(income_group_pop) & variable %in% c("not_latinx", "latinx")) 

pdf(paste0("./visuals/pub_figures/fig6_demo", td, ".pdf"))
ggplot(data = racem_nh, aes(x = variable, y = pop, group = variable))+
  geom_bar(stat="identity", aes(fill = variable)) +
  labs(title = "Demographic by income group") + 
  facet_wrap( ~ income_group_pop) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data = racem_h, aes(x = variable, y = pop, group = variable))+
  geom_bar(stat="identity", aes(fill = variable)) +
  labs(title = "Hispanic/Latinx by income group") + 
  facet_wrap( ~ income_group_pop) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

#### mobility by income group 2020 and 2021 ----
temp_mobility_data_byday <- data %>% filter(year %in% c(2020, 2021)) %>%
  group_by(date, income_group_pop, year, monthday) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T), 
            avg_visitors = mean(visitors_percap, na.rm = T))

ggplot(data = temp_mobility_data_byday, aes(x=date, y = avg_visitors, 
                                            group = as.factor(income_group_pop), 
                                            color = as.factor(income_group_pop))) +
  geom_smooth(alpha=0.2, position="identity", show.legend = FALSE) + 
  theme_bw()

#### mobility distribution by race ----

data_all_95 <- data_all %>% filter(visitors_percap <= 4.23)
data_subset <- data_all_95 %>% group_by(income_group_pop, visitors_percap) %>%
  summarise(percgroup = ntile(visitors_percap, 4)) %>% ungroup()
ggplot(data = data_subset, aes(x=as.factor(percgroup), y = visitors_percap, 
                               group = as.factor(percgroup), 
                               color = as.factor(percgroup))) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  facet_wrap( ~ income_group_pop, nrow = 2) +
  theme_bw()

#### distribution of MI values by income group 2021 - 2020 ----

ggplot(data = cast_temp_income, aes(diff_cut, group = income_group_pop, fill = income_group_pop)) +
  geom_bar(position = position_dodge()) + 
  facet_wrap( ~ income_group_pop, nrow = 2) +
  labs(x = "Diff in MI 2021-2020", y = "Number of Instances a CBG appears",
       title = "frequency of 2021-2020 MI values \n at least one county has temp >= 34") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data = cast_temp_income, aes(diff_cut, group = income_group_pop, fill = income_group_pop)) +
  geom_bar(position=position_dodge()) + 
  labs(x = "Diff in MI 2021-2020", y = "Number of Instances a CBG appears",
       title = "frequency of 2021-2020 MI values \n at least one county has temp >= 34") +
  theme_bw()

dev.off()

pdf(paste0("./visuals/pub_figures/sup2_", td, ".pdf"))

ggplot(data = data %>% filter(year %in% c(2020, 2021)), 
       aes(x = visitors_percap, group = as.factor(year),
           color = as_factor(year), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of Mobility Index: All Days") +  
  geom_vline(xintercept = 4.5) +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

dev.off()
