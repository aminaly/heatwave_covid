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
library(sf)

## global vars
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 

## read in the regression data
data <- readRDS("./heatwaves_manual/data_for_regression_03_2022.RDS")
data <- data %>% mutate(visitors_percap = (stops_by_day - number_devices_residing)/ number_devices_residing) %>%
  filter(!is.na(visitors_percap) & is.finite(visitors_percap))
td <- format(Sys.Date(), "%m_%Y")

## read in map data 
cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp", stringsAsFactors = F) 
cbg$census_block_group <- paste0(cbg$STATEFP, cbg$COUNTYFP, cbg$TRACTCE, cbg$BLKGRPCE)
cbg <- cbg %>% filter(census_block_group %in% unique(data$census_block_group))

## get the temperature data ready

## counts temp > 34deg
data_34 <- data %>% filter(mean_high_c >= 34) %>% count(census_block_group, year) %>% rename(num_days_34 = n)
data_34_mv <- data %>% filter(mean_high_c >= 34 & visitors_percap >= quantile(visitors_percap, .9, na.rm = T)) %>% 
  count(census_block_group, year) %>% rename(num_days_both = n)
data_34 <- left_join(data_34, data_34_mv, by = c("census_block_group", "year"))
data_34 <- data_34 %>% mutate(perc_both = num_days_both/num_days_34)
data_34_occurance <- merge(cbg, data_34, by = "census_block_group")

##counts temp > 95tile
data_95p <- data %>% filter(p_high >= 95) %>% count(census_block_group, year) %>% rename(num_days_34 = n)
data_95p_mv <- data %>% filter(p_high >= 95 & visitors_percap >= quantile(visitors_percap, .9, na.rm = T)) %>%
  count(census_block_group, year) %>% rename(num_days_both = n)
data_95p <- left_join(data_95p, data_95p_mv, by = c("census_block_group", "year"))
data_95p <- data_95p %>% mutate(perc_both = num_days_both/num_days_34)
data_95p_occurance <- merge(cbg, data_95p, by = "census_block_group")

#### pull in metadata for labels ----
metadata <- read_csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/metadata/cbg_field_descriptions.csv"))

#### add in income and population data ----
income <- read.csv(paste0(getwd(), "/heatwaves_manual/safegraph_open_census_data/data/cbg_b19.csv"), stringsAsFactors = F, header = T)
income <- na.omit(income %>% dplyr::select(census_block_group, 	median_income = B19013e1))
income <- income %>% mutate("census_block_group" = ifelse(nchar(census_block_group) == 11, 
                                                          paste0("0", census_block_group), census_block_group))
income$fips <- substr(income$census_block_group, 1, 5)
income <- income %>% mutate(income_group = ntile(median_income, 5)) %>% filter(fips %in% included_fips)
income$census_block_group <- as.character(income$census_block_group)

pops <- cbg_pop %>% mutate(census_block_group = as.character(poi_cbg)) %>% 
  mutate(census_block_group =  ifelse(nchar(census_block_group) == 11, 
                                      paste0("0", census_block_group), census_block_group)) %>% 
  select(census_block_group, unweighted_pop)
pops$fips <- str_sub(pops$census_block_group, 1,5)
pops <- pops %>% filter(fips %in% included_fips)

pop_income <- left_join(income, pops, by = c("census_block_group", "fips"))

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

demo <- left_join(pop_income, race, by = c("census_block_group", "fips"))

demo <- demo  %>%
  mutate(p_white = white/unweighted_pop, p_black = black/unweighted_pop, p_latinx = latinx/unweighted_pop, p_asian = asian/unweighted_pop,
         p_native = native/unweighted_pop, p_two_or_more = two_or_more/unweighted_pop) %>%
  select(p_white, p_black, p_latinx, p_asian, p_native, p_two_or_more, census_block_group, fips)

demo$maxdemo <- colnames(demo[,1:6])[max.col(demo[,1:6])]

### plot range of mobility of different groups on days with temps >95th percentile in the summer ----

data <- data %>% mutate(monthday = yday(date), hot = ifelse(mean_high_c >= 34, "Hot Day", "Regular Day"))
data <- left_join(data, demo %>% select(census_block_group, fips, maxdemo), by = c("census_block_group", "fips"))
data_summer <- data %>% filter(month %in% c(5:9)) # may - sept
data_summer <- data_summer %>% filter(!is.na(maxdemo)) %>% mutate(year = as.factor(year))

pdf(paste0("./visuals/pub_figures/fig4_boxplots", td, ".pdf"))
##Finalize datasets for regressions & run

ggplot(data = data_summer %>% filter(visitors_percap <= quantile(visitors_percap, .95)), 
       aes(x = maxdemo, y=visitors_percap, group=maxdemo)) + 
  geom_boxplot(aes(color = maxdemo)) +
  ggtitle("Distribution of Mobility (<95th percentile) on Regular and Hot Days") +
  #scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ hot, nrow = 2) +
  theme_bw()

ggplot(data = data_summer %>% filter(visitors_percap > quantile(visitors_percap, .95) & visitors_percap < quantile(visitors_percap, .99)), 
       aes(x = maxdemo, y=visitors_percap, group=maxdemo)) + 
  geom_boxplot(aes(color = maxdemo)) +
  ggtitle("Distribution of Most Mobile (>95th) on Regular and Hot Days") +
  #scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ hot, nrow = 2) +
  theme_bw()

ggplot(data = data_summer %>% filter(visitors_percap <= quantile(visitors_percap, .95)), 
       aes(x = maxdemo, y=visitors_percap, group=hot)) + 
  geom_boxplot(aes(color = hot)) +
  ggtitle("Distribution of Mobility (<95th percentile) on Regular and Hot Days") +
  #scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ maxdemo, nrow = 2) +
  theme_bw()

ggplot(data = data_summer %>% filter(visitors_percap > quantile(visitors_percap, .95) & visitors_percap < quantile(visitors_percap, .99)), 
       aes(x = maxdemo, y=visitors_percap, group=hot)) + 
  geom_boxplot(aes(color = hot)) +
  ggtitle("Distribution of Most Mobile (>95th) on Regular and Hot Days") +
  #scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ maxdemo, nrow = 2) +
  theme_bw()

ggplot(data = data_summer %>% filter(visitors_percap <= quantile(visitors_percap, .95)), 
       aes(x = maxdemo, y=visitors_percap, group=interaction(maxdemo, year))) + 
  geom_boxplot(aes(color = maxdemo, fill = year)) +
  ggtitle("Distribution of Mobility (<95th percentile) on Regular and Hot Days") +
  #scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ hot, nrow = 2) +
  theme_bw()

ggplot(data = data_summer %>% filter(visitors_percap > quantile(visitors_percap, .95) & visitors_percap < quantile(visitors_percap, .99)), 
       aes(x = maxdemo, y=visitors_percap, group=interaction(maxdemo, year))) + 
  geom_boxplot(aes(color = maxdemo, fill = year)) +
  ggtitle("Distribution of Most Mobile (>95th) on Regular and Hot Days") +
  #scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ hot, nrow = 2) +
  theme_bw()

dev.off()

## map of where these different 

pdf(paste0("./visuals/pub_figures/fig4_map", td, ".pdf"))
##Finalize datasets for regressions & run

demo <- merge(cbg, demo, by = "census_block_group")

print(ggplot(data = demo) +
        ggtitle("demographic map") +
        geom_sf(data = demo, size = 0.002, aes(fill = maxdemo)) +
        geom_sf(data = cbg, size = 0.002, fill = "transparent") +
        #scale_fill_discrete(l) +
        labs(colour="Mobility Metric") +
        theme_bw())

dev.off()

#### mobility loess ----

pdf(paste0("./visuals/pub_figures/demographic_loess_mobility", td, ".pdf"))
##Finalize datasets for regressions & run

data_summer_mm <- data_summer %>%  
  group_by(maxdemo, date, year, monthday) %>% 
  summarize(avg_summer_mobility = mean(visitors_percap, na.rm = T),
            med_summer_mobility = median(visitors_percap, na.rm = T),
            median_income = median(median_income, na.rm = T))

data_mm <- data %>%  filter(!is.na(maxdemo)) %>%
  group_by(maxdemo, date, year, monthday) %>% 
  summarize(avg_mobility = mean(visitors_percap, na.rm = T),
            med_mobility = median(visitors_percap, na.rm = T),
            median_income = median(median_income, na.rm = T))

ggplot(data = data_mm, 
       aes(x = date, y=avg_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Avg Mobility (Loess)") +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_summer_mm, 
       aes(x = monthday, y=avg_summer_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Avg Mobility (Loess) Summer") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_mm, 
       aes(x = date, y=med_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Median Mobility (Loess)") +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_summer_mm, 
       aes(x = monthday, y=med_summer_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Median Mobility (Loess) Summer") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

data_summer_mmh <- data_summer %>% filter(hot == "Hot Day") %>%
  group_by(maxdemo, date, year, monthday) %>% 
  summarize(avg_summer_mobility = mean(visitors_percap, na.rm = T),
            med_summer_mobility = median(visitors_percap, na.rm = T),
            median_income = median(median_income, na.rm = T))

data_mmh <- data %>%  filter(hot == "Hot Day" & !is.na(maxdemo)) %>%
  group_by(maxdemo, date, year, monthday) %>% 
  summarize(avg_mobility = mean(visitors_percap, na.rm = T),
            med_mobility = median(visitors_percap, na.rm = T),
            median_income = median(median_income, na.rm = T))

ggplot(data = data_mmh, 
       aes(x = date, y=avg_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Avg Mobility (Loess) Hot Days") +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_summer_mmh, 
       aes(x = monthday, y=avg_summer_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Avg Mobility (Loess) Summer Hot Days") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_mmh, 
       aes(x = date, y=med_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Median Mobility (Loess) Hot Days") +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_summer_mmh, 
       aes(x = monthday, y=med_summer_mobility, group=maxdemo)) + 
  geom_smooth(aes(color = maxdemo)) +
  ggtitle("Median Mobility (Loess) Summer Hot Days") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

dev.off()

#### distributions of CBG mobility for each demo group each year ----
pdf(paste0("./visuals/pub_figures/densityplots_mostmobile", td, ".pdf"))

## 2018
ggplot(data = data_summer %>% filter(year == 2018 & visitors_percap > quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=hot)) + 
  geom_density(aes(color = hot)) +
  ggtitle("Density of Mobility Hot vs Regular Days 2018") +
  facet_wrap( ~ maxdemo, nrow = 2) +
  theme_bw()

## 2019
ggplot(data = data_summer %>% filter(year == 2019 & visitors_percap > quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=hot)) + 
  geom_density(aes(color = hot)) +
  ggtitle("Density of Mobility Hot vs Regular Days 2019") +
  facet_wrap( ~ maxdemo, nrow = 2) +
  theme_bw()

## 2020
ggplot(data = data_summer %>% filter(year == 2020 & visitors_percap > quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=hot)) + 
  geom_density(aes(color = hot)) +
  ggtitle("Density of Mobility Hot vs Regular Days 2020") +
  facet_wrap( ~ maxdemo, nrow = 2) +
  theme_bw()

## 2021
ggplot(data = data_summer %>% filter(year == 2021 & visitors_percap > quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=hot)) + 
  geom_density(aes(color = hot)) +
  ggtitle("Density of Mobility Hot vs Regular Days 2021") +
  facet_wrap( ~ maxdemo, nrow = 2) +
  theme_bw()

dev.off()

#### KS Tests Hot v Reg each year ----
## get all combinations of hot v not hot for each demo

ks_results <- c()

for(i in unique(data_summer$maxdemo)) {
  
  for(y in 2018:2021) {
    
    d <- data_summer %>% filter(maxdemo == i & year == y) 
    
    ks <- ks.test(d %>% filter(hot == "Regular Day") %>% pull(visitors_percap),
                  d %>% filter(hot == "Hot Day") %>% pull(visitors_percap))
    
    ks_results <- rbind(ks_results, c(maxdemo = i, year = y, percentile_visitors = "All" , statistic = ks$statistic, pval = ks$p.value))
    
    d <- data_summer %>% filter(maxdemo == i & year == y & visitors_percap > quantile(visitors_percap, .95)) 
    
    ks <- ks.test(d %>% filter(hot == "Regular Day") %>% pull(visitors_percap),
                  d %>% filter(hot == "Hot Day") %>% pull(visitors_percap))
    
    ks_results <- rbind(ks_results, c(maxdemo = i, year = y, percentile_visitors = "95th" , statistic = ks$statistic, pval = ks$p.value))
    
    d <- data_summer %>% filter(maxdemo == i & year == y & visitors_percap > quantile(visitors_percap, .90)) 
    
    ks <- ks.test(d %>% filter(hot == "Regular Day") %>% pull(visitors_percap),
                  d %>% filter(hot == "Hot Day") %>% pull(visitors_percap))
    
    ks_results <- rbind(ks_results, c(maxdemo = i, year = y, percentile_visitors = "90th" , statistic = ks$statistic, pval = ks$p.value))
  }
}

#### distributions of CBG mobility for each demo group ----
pdf(paste0("./visuals/pub_figures/densityplots_mobilitythroughyears", td, ".pdf"))

## black
ggplot(data = data_summer %>% filter(maxdemo == "p_black" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_density(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj Black CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

## white
ggplot(data = data_summer %>% filter(maxdemo == "p_white" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_density(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj White CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

## asian
ggplot(data = data_summer %>% filter(maxdemo == "p_asian" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_density(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj Asian CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

## latinx
ggplot(data = data_summer %>% filter(maxdemo == "p_latinx" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_density(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj Latinx CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

dev.off()

#### Sig Testing 2018:2021 comp to 2021by demo ----

sig_results <- c()

for(i in unique(data_summer$maxdemo)) {
  
  for(y in 2018:2021) {
    
    d <- data_summer %>% filter(year %in% c(2021, y), maxdemo == i & hot == "Hot Day" & 
                                      visitors_percap < quantile(visitors_percap, .95)) 

    ## KS
    ks_h <- tryCatch({ks.test(d %>% filter(year == y) %>% pull(visitors_percap),
                    d %>% filter(year == 2021) %>% pull(visitors_percap))}, error=function(e) {return(NA)})
    
    if(is.na(ks_h)) {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "KS", statistic = NA, pval = NA, temp = "Hot"))
    } else {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "KS", statistic = ks_h$statistic, pval = ks_h$p.value , temp = "Hot"))
    }
    
    ## wilcox
    w_h <- tryCatch({wilcox.test(d %>% filter(year == y) %>% pull(visitors_percap),
                       d %>% filter(year == 2021) %>% pull(visitors_percap))}, error=function(e) {return(NA)})
    
    if(is.na(w_h)) {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "Wilcox", statistic = NA, pval = NA, temp = "Hot"))
    } else {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "Wilcox", statistic = w_h$statistic, pval = w_h$p.value, temp = "Hot"))
    }
    
    d <- data_summer %>% filter(year %in% c(2021, y), maxdemo == i & hot == "Regular Day" & 
                                  visitors_percap < quantile(visitors_percap, .95)) 
    
    ## KS
    ks_h <- tryCatch({ks.test(d %>% filter(year == y) %>% pull(visitors_percap),
                              d %>% filter(year == 2021) %>% pull(visitors_percap))}, error=function(e) {return(NA)})
    
    if(is.na(ks_h)) {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "KS", statistic = NA, pval = NA, temp = "Regular"))
    } else {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "KS", statistic = ks_h$statistic, pval = ks_h$p.value, temp = "Regular"))
    }
    
    ## wilcox
    w_h <- tryCatch({wilcox.test(d %>% filter(year == y) %>% pull(visitors_percap),
                                 d %>% filter(year == 2021) %>% pull(visitors_percap))}, error=function(e) {return(NA)})
    
    if(is.na(w_h)) {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "Wilcox", statistic = NA, pval = NA, temp = "Regular"))
    } else {
      sig_results <- rbind(sig_results, c(maxdemo = i, year = y, test = "Wilcox", statistic = w_h$statistic, pval = w_h$p.value, temp = "Regular"))
    }
    
  }
}

#### Box Plots reg v hot of mobility <95th
pdf(paste0("./visuals/pub_figures/boxplots_mobilitythroughyears", td, ".pdf"))

## black
ggplot(data = data_summer %>% filter(maxdemo == "p_black" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_boxplot(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj Black CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

## white
ggplot(data = data_summer %>% filter(maxdemo == "p_white" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_boxplot(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj White CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

## asian
ggplot(data = data_summer %>% filter(maxdemo == "p_asian" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_boxplot(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj Asian CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

## latinx
ggplot(data = data_summer %>% filter(maxdemo == "p_latinx" & visitors_percap < quantile(visitors_percap, .95)), 
       aes(x = visitors_percap, group=year)) + 
  geom_boxplot(aes(color = year)) +
  ggtitle("Density of Mobility Hot vs Regular Days Maj Latinx CBG") +
  facet_wrap( ~ hot) +
  theme_bw()

dev.off()

#### old ----

pdf(paste0("./visuals/pub_figures/fig4", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

ggplot(data = data_34_occurance, aes(x = year, y=num_days_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Mobility spread on hottest days of #days MI > 3 temp >= 34") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_34_occurance, aes(x = year, y=num_days_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Mobility spread on hottest days of #days MI > 3 temp >= 34") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()



#### plot days and %of days with MI >3 and hot weather ----
pdf(paste0("./visuals/pub_figures/fig4_1perpage", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

for(i in c(2018:2021)) {
  
  data_34_occurance_y <- data_34_occurance %>% filter(year == i)
  
  ### Map of hotspots for days over 34 degrees 
  print(ggplot(data = data_34_occurance_y) +
    ggtitle(paste(i, "#days MI >3 (90th percentile) on days over 34C")) +
    geom_sf(data = data_34_occurance_y, size = 0.002, aes(fill = num_days_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    #facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
  print(ggplot(data = data_34_occurance_y) +
    ggtitle(paste(i,"%days MI>3 (90th percentile) of all days over 34C")) +
    geom_sf(data = data_34_occurance_y, size = 0.002, aes(fill = perc_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    #facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
  data_95p_occurance_y <- data_95p_occurance %>% filter(year == i)
  
  ### Map of hotspots for days over 95th percentile  
  print(ggplot(data = data_95p_occurance_y) +
    ggtitle(paste(i,"#days MI >3 (90th percentile) on days w temp >95percentile")) +
    geom_sf(data = data_95p_occurance_y, size = 0.002, aes(fill = num_days_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    #facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
  print(ggplot(data = data_95p_occurance_y) +
    ggtitle(paste(i,"%days MI >3 (90th percentile) on days w temp >95percentile")) +
    geom_sf(data = data_95p_occurance_y, size = 0.002, aes(fill = perc_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    #facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
}

dev.off()

#### put all 4 years on one page ----
pdf(paste0("./visuals/pub_figures/fig4_4yrs_per_page", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

ggplot(data = data_34_occurance) +
  ggtitle(paste(i, "#days MI >3 (90th percentile) on days over 34C")) +
  geom_sf(data = data_34_occurance, size = 0.002, aes(fill = num_days_both)) +
  #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
  labs(colour="Mobility Metric") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_95p_occurance) +
  ggtitle(paste(i,"#days MI >3 (90th percentile) on days w temp >95percentile")) +
  geom_sf(data = data_95p_occurance, size = 0.002, aes(fill = num_days_both)) +
  #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
  labs(colour="Mobility Metric") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_34_occurance_y) +
  ggtitle(paste(i,"%days MI>3 (90th percentile) of all days over 34C")) +
  geom_sf(data = data_34_occurance_y, size = 0.002, aes(fill = perc_both)) +
  #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
  labs(colour="Mobility Metric") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_95p_occurance_y) +
  ggtitle(paste(i,"%days MI >3 (90th percentile) on days w temp >95percentile")) +
  geom_sf(data = data_95p_occurance_y, size = 0.002, aes(fill = perc_both)) +
  #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
  labs(colour="Mobility Metric") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

dev.off()

#### all 4 years, but loop through each county ----
pdf(paste0("./visuals/pub_figures/fig4_4yrs_per_page_eachcounty", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

for(county in included_fips) {
  
  data_34_occurance_c <- data_34_occurance %>% 
    mutate(fips = as.character(paste0(STATEFP, COUNTYFP))) %>%
    filter(fips == county)
  data_95p_occurance_c <- data_95p_occurance %>% 
    mutate(fips = as.character(paste0(STATEFP, COUNTYFP))) %>%
    filter(fips == county)
  
  print(ggplot(data = data_34_occurance_c) +
    ggtitle(paste(i, "#days MI >3 (90th percentile) on days over 34C")) +
    geom_sf(data = data_34_occurance_c, size = 0.002, aes(fill = num_days_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
  print(ggplot(data = data_95p_occurance_c) +
    ggtitle(paste(i,"#days MI >3 (90th percentile) on days w temp >95percentile")) +
    geom_sf(data = data_95p_occurance_c, size = 0.002, aes(fill = num_days_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
  print(ggplot(data = data_34_occurance_c) +
    ggtitle(paste(i,"%days MI>3 (90th percentile) of all days over 34C")) +
    geom_sf(data = data_34_occurance_c, size = 0.002, aes(fill = perc_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
  print(ggplot(data = data_95p_occurance_c) +
    ggtitle(paste(i,"%days MI >3 (90th percentile) on days w temp >95percentile")) +
    geom_sf(data = data_95p_occurance_c, size = 0.002, aes(fill = perc_both)) +
    #geom_sf(data = cbg, size = 0.002, fill = "transparent") +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e8effa") +
    labs(colour="Mobility Metric") +
    facet_wrap( ~ year, nrow = 2) +
    theme_bw())
  
}

dev.off()


#### box plot of num days both by year ----
pdf(paste0("./visuals/pub_figures/fig4_boxplots", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

ggplot(data = data_34_occurance, aes(x = year, y=num_days_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of #days MI > 3 temp >= 34") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()

ggplot(data = data_34_occurance, aes(x = year, y=perc_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of %days MI > 3 temp >= 34") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()

ggplot(data = data_95p_occurance, aes(x = year, y=num_days_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of #days MI > 3 temp > 95tile") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()
ggplot(data = data_95p_occurance, aes(x = year, y=perc_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of %days MI > 3 temp > 95tile") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()

dev.off()

#### density plots ----
pdf(paste0("./visuals/pub_figures/fig4_densityplots", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

ggplot(data = data_34_occurance, aes(x = num_days_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of #days MI > 3 & temp >= 34") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

ggplot(data = data_34_occurance, aes(x = perc_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of %days MI > 3 & temp >= 34") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

ggplot(data = data_95p_occurance, aes(x = num_days_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of #days MI > 3 & temp > 95tile") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

ggplot(data = data_95p_occurance, aes(x = perc_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of %days MI > 3 & temp > 95tile") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

dev.off()

#### CBGs w/ >75% of days where MI is high when hot ----

data_34_occurance_highmv <- data_34_occurance %>% filter(perc_both >= .75)
data_95p_occurance_highmv <- data_95p_occurance %>% filter(perc_both >= .75)

#### boxplots of high movers ----

pdf(paste0("./visuals/pub_figures/fig4_boxplots_highmovers", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

ggplot(data = data_34_occurance_highmv, aes(x = year, y=num_days_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of #days MI > 3 temp >= 34") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()

ggplot(data = data_34_occurance_highmv, aes(x = year, y=perc_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of %days MI > 3 temp >= 34") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()

ggplot(data = data_95p_occurance_highmv, aes(x = year, y=num_days_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of #days MI > 3 temp > 95tile") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()
ggplot(data = data_95p_occurance_highmv, aes(x = year, y=perc_both, group=year)) + 
  geom_boxplot() +
  ggtitle("Distribution of %days MI > 3 temp > 95tile") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  theme_bw()

dev.off()

#### density plots of high movers ----
pdf(paste0("./visuals/pub_figures/fig4_densityplots_highmovers", td, ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Mobile CBGs"),
     cex = 1.5, col = "black")

ggplot(data = data_34_occurance_highmv, aes(x = num_days_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of #days MI > 3 & temp >= 34") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

ggplot(data = data_34_occurance_highmv, aes(x = perc_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of %days MI > 3 & temp >= 34") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

ggplot(data = data_95p_occurance_highmv, aes(x = num_days_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of #days MI > 3 & temp > 95tile") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

ggplot(data = data_95p_occurance_highmv, aes(x = perc_both, group = year, color = as.factor(year))) +
  ggtitle("Distribution of %days MI > 3 & temp > 95tile") +
  geom_density() +
  scale_fill_continuous(low = "#addd8e", high = "#31a354") +
  theme_bw()

dev.off()

## boxplot of income group (weighted)

## boxplot of demographics






ggplot(data = dd_d, aes(x = as.Date(yday(date), "1970-01-01"), y=visitors_percap, group=year)) + 
  geom_line(aes(color = year)) +
  scale_x_date(date_breaks="months", date_labels="%b") +
  ggtitle("Visitors percap") +
  scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()
