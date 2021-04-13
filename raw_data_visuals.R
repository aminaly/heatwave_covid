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
income <- income %>% mutate(income_group = ntile(median_income, 5))
i <- income %>% mutate("fips" = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  dplyr::select(fips, median_income, poverty_percent, income_group)
i <- i[-1,]

#combine shelter with icncome, and select region if we want it
shelter <- left_join(s, i, by = c("fips"))
shelter <- shelter %>% ungroup(date) %>% mutate(date = as.Date(date))
shelter$stateyear <- paste0(shelter$state, year(shelter$date))

## Combined temperature and sheltering by fips 
t_zs <- t %>% group_by(fips, year) %>%
  mutate(z_score_high = (mean_high - mean(mean_high)) / sd(mean_high)) %>% 
  mutate(z_score_low = (mean_low - mean(mean_low)) / sd(mean_low)) %>% 
  mutate(p_high = 100* pnorm(z_score_high)) %>%
  mutate(p_low = 100* pnorm(z_score_low)) %>%
  ungroup

data <- left_join(shelter, t_zs, by = c("fips", "date"))
data <- data %>% mutate(mean_low_c = mean_low-273.15, mean_high_c = mean_high-273.15)

## renaming columns for easy access to main x and y values 
data <- rename(data, measure = mean_high_c)
data <- rename(data, yvar = shelter_index)

## cutting up the data so that we only get the "covid" timeline
data2019 <- data %>% filter(date >= "2019-03-01" & date <= "2019-11-07")
data2020 <- data %>% filter(date >= "2020-03-01")
data_mar_dec <- rbind(data2019, data2020)

## lets do some plots
pdf(paste0("./visuals/patterns_", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Data Overview"),
     cex = 1.5, col = "black")

# line plot of mobility over time separated by income group
ggplot(data=data_mar_dec, aes(x=date, y=yvar, group=income_group)) +
  geom_line(aes(colour=as.factor(income_group))) + 
  #geom_smooth(aes(group=income_group, color=as.factor(income_group))) +
  ggtitle("Mobility Throughout Year") + ylab("Mobility") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  facet_wrap( ~ year, scales = "free") + 
  scale_x_date() +
  theme(text = element_text(size = 15)) + 
  labs(colour="Income Grp (5 High)") +
  theme_bw()

# line plot of mobility over time separated by income group but with loess 
ggplot(data=data_mar_dec, aes(x=date, y=yvar, group=income_group)) +
  #geom_line(aes(colour=income_group)) + 
  geom_smooth(aes(group=income_group, color=as.factor(income_group))) +
  ggtitle("Mobility Throughout Year") + ylab("Mobility") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  facet_wrap( ~ year, scales = "free") + 
  scale_x_date() +
  theme(text = element_text(size = 15)) + 
  labs(colour="Income Grp (5 High)") +
  theme_bw()
  
# bar plot showing with num of each income group broken down by region
i_m <- na.omit(left_join(m_master, i, by = "fips"))
i_m_short <- i_m %>% group_by(region_s, income_group) %>% count(income_group)

i_m_short %>% 
  ggplot() +
  geom_bar(data=i_m_short, aes(x=income_group, y = n),
           stat="identity",
           position='dodge') +
  theme_bw() + 
  facet_wrap( ~ region_s, scales = "free") + 
  #scale_y_continuous(expand = c(0, 2), limits = c(0, NA)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(text = element_text(size = 20))

# line plot showing how normal temps were by region
ggplot(data=data, aes(x=date, y=z_score_high, group=region_s)) +
  geom_line(aes(colour=as.factor(region_s))) + 
  ggtitle("Temp Z Score") + ylab("Z Score") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  facet_wrap( ~ year, scales = "free") + 
  scale_x_date() +
  geom_hline(yintercept = 0) +
  theme(text = element_text(size = 15)) + 
  labs(colour="Region") +
  theme_bw()

ggplot(data=data, aes(x=date, y=z_score_high, group=region_s)) +
  geom_smooth(aes(group=region_s, color=as.factor(region_s))) +
  ggtitle("Temp Z Score") + ylab("Z Score") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  facet_wrap( ~ year, scales = "free") + 
  scale_x_date() +
  geom_hline(yintercept = 0) +
  theme(text = element_text(size = 15)) + 
  labs(colour="Region") +
  theme_bw()
  



  