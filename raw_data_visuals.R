ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

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


##Read in datasets
t <- read_rds(paste0(getwd(), "/heatwaves_manual/all_temperature_data_clean_2021.rds"))
r_master <- read_csv(paste0(getwd(), "/us_census/climate_regions.csv"))
m_master <- read_rds(paste0(getwd(), "/calculated/all_mortality.rds"))
s <- read_rds(paste0(getwd(), "/heatwaves_manual/patterns_clean_santaclara.rds"))

#get income and population data
income <- read.csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b19.csv"), stringsAsFactors = F, header = T)

# get state and regional information
m_master$state <- str_sub(m_master$county, -2)
m_master <- left_join(m_master, r_master, by= "state")
m_master <- unique(m_master %>% select(fips, state, region, region_s))
s <- left_join(s, m_master, by = "fips")

#set up mortality & add in median income
income <- na.omit(income %>% dplyr::select(census_block_group, 	median_income = B19013e1))
income <- income %>% mutate("census_block_group" = ifelse(nchar(census_block_group) == 11, 
                                                paste0("0", census_block_group), census_block_group))
income$fips <- substr(income$census_block_group, 1, 5)
income <- income %>% mutate(income_group = ntile(median_income, 5))
i <- income %>% mutate("fips" = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  dplyr::select(fips, median_income, income_group, census_block_group)

i$census_block_group <- as.character(i$census_block_group)


#combine shelter with icncome, and select region if we want it
shelter <- left_join(s, i, by = c("census_block_group", "fips"))
shelter <- shelter %>% mutate(date = as.Date(date))

## Combined temperature and sheltering by fips 
t <- t %>% filter(fips %in% unique(s$fips))
t_zs <- t %>% group_by(fips, year) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = 100* pnorm(z_score_high)) %>%
  mutate(p_low = 100* pnorm(z_score_low)) %>%
  ungroup

data <- left_join(shelter, t_zs, by = c("fips", "date", "year"))
data <- data %>% mutate(mean_low_c = mean_low-273.15, mean_high_c = mean_high-273.15)

## renaming columns for easy access to main x and y values 
data <- rename(data, measure = mean_high_c)
data <- rename(data, yvar = visitors_percap)
data <- na.omit(data)

## lets do some plots
pdf(paste0("./visuals/patterns_", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Data Overview"),
     cex = 1.5, col = "black")

## cutting up the data so that we only get the "covid" timeline
data2019 <- data %>% filter(date >= "2019-03-01" & date <= "2019-11-07") %>% mutate(day = day(date))
data2020 <- data %>% filter(date >= "2020-03-01") %>% mutate(day = day(date))
data_mar_dec <- rbind(data2019, data2020)


# line plot of outside visitors over time separated by income group, just covid timeline
ggplot(data=data_mar_dec, aes(x=date, y=yvar, group=income_group)) +
  geom_smooth(aes(group = income_group, colour=as.factor(income_group))) +
  ggtitle("Mobility Throughout Year - COVID TL") + ylab("# Visitors / Home Devices") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  facet_wrap( ~ year, scales = "free", nrow = 2) +
  scale_x_date() +
  theme(text = element_text(size = 15)) +
  labs(colour="$$ Grp (5 High)") +
  theme_bw()

# line plot of outside visitors over time separated by income group, all time
ggplot(data=data, aes(x=date, y=yvar, group=income_group)) +
  geom_smooth(aes(group=income_group, color=as.factor(income_group))) +
  ggtitle("Mobility Full Timeline") + ylab("# Visitors / Home Devices") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  facet_wrap( ~ year, scales = "free", nrow = 2) + 
  scale_x_date() +
  theme(text = element_text(size = 15)) + 
  labs(colour="$$ Grp (5 High)") +
  theme_bw()
  
# bar plot showing with num of each income group in santa clara
i_m <- na.omit(left_join(m_master, i, by = "fips"))
i_m_short <- i_m %>% group_by(region_s, income_group) %>% count(income_group)

i_m_short %>% 
  ggplot() +
  geom_bar(data=i_m_short, aes(x=income_group, y = n),
           stat="identity",
           position='dodge') +
  theme_bw() + ylab("Count") +
  #facet_wrap( ~ region_s, scales = "free") + 
  #scale_y_continuous(expand = c(0, 2), limits = c(0, NA)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(text = element_text(size = 20))

# loess plot showing how normal temps were each year
ggplot(data=data, aes(x=date, y=p_high, group=year)) +
  geom_smooth(aes(group=year, color=as.factor(year))) +
  ggtitle("Temp Z Score") + ylab("Z Score") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  #facet_wrap( ~ year, scales = "free") + 
  scale_x_date() +
  geom_hline(yintercept = 0) +
  theme(text = element_text(size = 15)) + 
  labs(colour="Year") +
  theme_bw()

# loess difference of 2020 - 2019 mobility
# data_sub <- left_join(data2019, data2020, by = c("census_block_group", "month.x", "day"))
# data_sub <- data_sub %>% mutate(yvar = yvar.y - yvar.x) %>% rename(income_group = income_group.x) %>%
#   rename(date = date.x) %>% rename(region_s = region_s.x)
# 
# ggplot(data=data_sub, aes(x=date, y=yvar, group=income_group)) +
#   geom_smooth(aes(group = income_group, colour=as.factor(income_group))) +
#   ggtitle("Mobility Change 2020 - 2019") + ylab("% Sheltering") + xlab("Date") +
#   scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
#   scale_x_date() +
#   theme(text = element_text(size = 15)) +
#   labs(colour="Income Grp (5 High)") +
#   theme_bw()





  
