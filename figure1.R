ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

source("./regression_functions_shelter.R")

library(lfe)
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
library(sf)

######## Read in datasets ######## 
data <- readRDS("./heatwaves_manual/data_with_demo_05_2022.RDS")
cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp", stringsAsFactors = F) 
td <- format(Sys.Date(), "%m_%d_%Y")

data <- data %>% filter(!is.na(visitors_percap), !is.na(income_group_pop))

## add income 
income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% select(census_block_group, income_group_pop)
data <- left_join(data, income, by = "census_block_group")

## combine temp and census block group shapefile
data <- data %>% mutate(STATEFP = substr(census_block_group, 1, 2), COUNTYFP = substr(census_block_group, 3, 5),
                                                    TRACTCE = substr(census_block_group, 1, 11))
cbg$census_block_group <- paste0(cbg$STATEFP, cbg$COUNTYFP, cbg$TRACTCE, cbg$BLKGRPCE)
cbg <- cbg %>% filter(census_block_group %in% data$census_block_group)

#filter for just summer months, and find avg for that summer by cbg
temp_mobility_sum <- data %>% filter(month %in% c(5,6,7,8,9)) %>% 
  group_by(year, census_block_group) %>%
  summarize(visitors_percap = mean(visitors_percap, na.rm = T))

hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)

temp_mobility_sum_hot <- data %>% filter(month %in% c(5,6,7,8,9) & date %in% hotdays) %>% 
  group_by(year, census_block_group) %>%
  summarize(visitors_percap = mean(visitors_percap, na.rm = T))
temp_mobility_cbg <- merge(cbg, temp_mobility_sum, by = "census_block_group")


##pull out each year, join, and find the difference
cast_temp <- dcast(temp_mobility_sum, census_block_group ~ year)
cast_temp$diff <- cast_temp$`2021` - cast_temp$`2020`
cast_temp <- cast_temp %>% mutate(diff_r= rank(diff)) %>%
  mutate(diff_log = log(diff_r)) %>% mutate(diff_cut = cut(diff, breaks = c(-Inf, seq(-3, 3, 1), Inf))) %>% 
  mutate(diff_sign = ifelse(diff > 0, 1, ifelse(diff < 0, -1, 0)))
cast_temp <- merge(cbg, cast_temp, by = "census_block_group")

cast_temp_high <- dcast(temp_mobility_sum_hot, census_block_group ~ year)
cast_temp_high$diff <- cast_temp_high$`2021` - cast_temp_high$`2020`
cast_temp_high <- cast_temp_high %>% mutate(diff_r= rank(diff)) %>%
  mutate(diff_log = log(diff_r)) %>% mutate(diff_cut = cut(diff, breaks = c(-Inf, seq(-3, 3, 1), Inf))) %>% 
  mutate(diff_sign = ifelse(diff > 0, 1, ifelse(diff < 0, -1, 0)))
cast_temp_high <- merge(cbg, cast_temp_high, by = "census_block_group")

pdf(paste0("./visuals/pub_figures/fig1_", td, ".pdf"))
##Finalize datasets for regressions & run

#### Map of mobility difference between 2020 and 2021 ----
ggplot(data = cast_temp) +
  ggtitle("Bay Area Summer Mobility Difference 2021 - 2020") +
  geom_sf(data = cast_temp, size = 0.002, aes(fill = diff_cut)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = cast_temp_high) +
  ggtitle("Bay Area Summer Mobility Difference 2021 - 2020 \n temp >= 34 in at least one county") +
  geom_sf(data = cast_temp_high, size = 0.002, aes(fill = diff_cut)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

### line plot of average visitors per cap each daily
ggplot(data=data_nona, aes(x=date, y=visitors_percap, group=income_group)) +
  geom_smooth(aes(group=income_group, color=as.factor(income_group))) +
  ggtitle("Mobility Full Timeline") + ylab("# Visitors / Home Devices") + xlab("Date") +
  scale_fill_gradient(low = "darkorange", high = "darkgreen") +
  scale_x_date() +
  theme(text = element_text(size = 15)) +
  labs(colour="$$ Grp (5 High)") +
  theme_bw()


dev.off()

#### Temperatures ----

pdf(paste0("./visuals/pub_figures/fig2_temperatures_", td, ".pdf"))

### bar chart of temperatures 2020 - 2021
data <- data %>% filter(!is.na(income_group_pop) & !is.na(maxdemo) & date %in% hotdays)

data_byday <- data %>% 
  filter(year %in% c(2020:2021)) %>%
  group_by(date) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T))

ggplot(data = data_byday, aes(x=date, y = avg_temp)) +
  geom_line(alpha=0.5, position="identity") + 
  theme_bw()

data_byday_income <- data %>% 
  filter(year %in% c(2020:2021)) %>%
  group_by(date, income_group_pop) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T),
            avg_percentile = mean(p_high, na.rm = T))

ggplot(data = data_byday_income, aes(avg_temp)) +
  geom_histogram(alpha=0.5) + 
  facet_wrap( ~ income_group_pop) +
  labs(title = "range of avg 'hot day' temperatures by income group") +
  theme_bw()

ggplot(data = data_byday_income, aes(avg_percentile)) +
  geom_histogram(alpha=0.5) + 
  facet_wrap( ~ income_group_pop) +
  labs(title = "range of percentile 'hot day' temperatures by income group") +
  theme_bw()

data_byday_race <- data %>% 
  filter(year %in% c(2020:2021)) %>%
  group_by(date, maxdemo) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T),
            avg_percentile = mean(p_high, na.rm = T))

ggplot(data = data_byday_race, aes(avg_temp)) +
  geom_histogram(alpha=0.5) + 
  facet_wrap( ~ maxdemo) +
  labs(title = "range of percentile 'hot day' temperatures ") +
  theme_bw()

ggplot(data = data_byday_race, aes(avg_percentile)) +
  geom_histogram(alpha=0.5) + 
  facet_wrap( ~ maxdemo) +
  theme_bw()

## on a map
data_cbg <- data %>% 
  filter(year %in% c(2020:2021) & date %in% hotdays) %>%
  group_by(census_block_group) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T),
            avg_percentile = mean(p_high, na.rm = T)) %>%
  mutate(avg_temp_group = cut(avg_temp, breaks = 5), 
         avg_percentile_group = cut(avg_percentile, breaks = 5))

data_cbg <- merge(data_cbg, cbg, by = "census_block_group")

ggplot(data = data_cbg, aes(geometry = geometry)) +
  ggtitle("2020-21 Temperature Average \n when at least one county experiencing temp >= 34") +
  geom_sf(data = data_cbg, size = 0.002, aes(fill = avg_temp_group)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = data_cbg, aes(geometry = geometry)) +
  ggtitle("2020-21 Avg Temperature Percentile \n  when at least one county experiencing temp >= 34") +
  geom_sf(data = data_cbg, size = 0.002, aes(fill = avg_percentile_group)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

data_cbg <- data %>% 
  filter(year %in% c(2020:2021) & mean_high_c >= 34) %>%
  group_by(census_block_group) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T),
            avg_percentile = mean(p_high, na.rm = T)) %>%
  mutate(avg_temp_group = cut(avg_temp, breaks = 5), 
         avg_percentile_group = cut(avg_percentile, breaks = 5))

data_cbg <- merge(data_cbg, cbg, by = "census_block_group")

ggplot(data = data_cbg, aes(geometry = geometry)) +
  ggtitle("2020-21 Temperature Average \n when temp >= 34") +
  geom_sf(data = data_cbg, size = 0.002, aes(fill = avg_temp_group)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = data_cbg, aes(geometry = geometry)) +
  ggtitle("2020-21 Avg Temperature Percentile \n  when temp >= 34") +
  geom_sf(data = data_cbg, size = 0.002, aes(fill = avg_percentile_group)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

data_cbg <- data %>% 
  filter(year %in% c(2020:2021) & mean_high_c >= 34) %>%
  group_by(census_block_group, income_group_pop, maxdemo) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T),
            avg_percentile = mean(p_high, na.rm = T)) %>%
  mutate(avg_temp_group = cut(avg_temp, breaks = 5), 
         avg_percentile_group = cut(avg_percentile, breaks = 5))

data_cbg <- merge(data_cbg, cbg, by = "census_block_group")


ggplot(data = data_cbg, aes(geometry = geometry)) +
  ggtitle("Income Distribution") +
  geom_sf(data = data_cbg, size = 0.002, aes(fill = income_group_pop)) +
  scale_fill_continuous(na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = data_cbg, aes(geometry = geometry)) +
  ggtitle("Majority demographic by CBG") +
  geom_sf(data = data_cbg, size = 0.002, aes(fill = maxdemo)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

dev.off()

#### distribution of 2021-2020 ----
cast_temp_income <-  left_join(cast_temp_high, 
                               data %>% select(census_block_group, income_group_pop))
cast_temp_income <- cast_temp_income %>% filter(!is.na(income_group_pop))


pdf(paste0("./visuals/pub_figures/fig9_disof_2021-2020_", td, ".pdf"))

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





