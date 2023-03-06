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
library(sf)
library(gridExtra)
 
## global vars
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 
quantiles <- c(.025, .5, .975)
ys <- c(2020, 2021)

## read in data 
data_all <- readRDS("./heatwaves_manual/data_with_demo_05_2022.RDS")
data <- data_all #copy of data to be filtered/edited
cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp", stringsAsFactors = F) 
td <- format(Sys.Date(), "%m_%d_%Y")

#### data cleaning ----

## create log version of MI
data <- data %>% mutate(visitors_percap_cr =  sign(visitors_percap) * (abs(visitors_percap))^(1/3)) %>%
  mutate(visitors_percap_log = log(visitors_percap + 1.00001))

## remove smoke days
smoke_days <- c(seq(as.Date("2020-08-19"), as.Date("2020-08-24"), by = 1),
                seq(as.Date("2020-09-10"), as.Date("2020-09-14"), by = 1),
                as.Date("2020-08-31", format = "%Y-%m-%d"))

## clean up nas 
data <- data %>% filter(!(date %in% smoke_days)) %>% 
  filter(!is.na(unweighted_pop)) %>% 
  filter(year %in% ys & !is.na(visitors_percap) & !is.na(median_income)) %>%
  mutate(monthweekyr = paste0(monthweek, year))

## select only months where we know temps hit 95th percentile 
data <- data %>% filter(month %in% c(5:9))

## add in income
income <- unique(data %>% dplyr::select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% dplyr::select(census_block_group, income_group_pop) %>% 
  mutate(income_group_pop = as.factor(income_group_pop)) 
data <- left_join(data, income, by = "census_block_group")
data_all <- left_join(data_all, income, by = "census_block_group")

## get total number of counties experiencing extreme temperatures
b <- data %>% filter(p_high >= 95) %>% count(date, region)
data <- left_join(data, b, by = c("date", "region"))
data <- data %>% mutate(n = ifelse(is.na(n), 0, n)) %>% rename(n_over_their_95th = n)

perc95 <- quantile(data$mean_high_c, .95)
b <- data %>% filter(mean_high_c >= perc95) %>% count(date, region)
data <- left_join(data, b, by = c("date", "region"))
data <- data %>% mutate(n = ifelse(is.na(n), 0, n)) %>% rename(n_over_total = n)

## assign avg MI group (based on percentiles equivalent to quantile reg)
m <- data %>% group_by(census_block_group) %>% 
  summarize(median_mi = median(visitors_percap, na.rm = T)) %>% 
  mutate(mi_group = cut(median_mi, 
                        breaks = c(-Inf, quantile(median_mi, c(.25, .5, .75)), Inf), 
                        labels = F))
data <- left_join(data, m, by = "census_block_group")

## create combined categorical variable
data <- data %>% unite(mi_income_groups, c("mi_group", "income_group_pop"))

## mark days where one or more county is >34 and separate TODO: DELETE if not using
hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)
data_subset <- data %>% filter(date %in% hotdays)

## combine temp and census block group shapefile
data <- data %>% mutate(STATEFP = substr(census_block_group, 1, 2), COUNTYFP = substr(census_block_group, 3, 5),
                        TRACTCE = substr(census_block_group, 1, 11))
cbg$census_block_group <- paste0(cbg$STATEFP, cbg$COUNTYFP, cbg$TRACTCE, cbg$BLKGRPCE)
cbg <- cbg %>% filter(census_block_group %in% data$census_block_group)

## create map data
map_data <- data %>% group_by(year, census_block_group) %>%
  summarize(visitors_percap = mean(visitors_percap, na.rm = T))

##pull out each year, join, and find the difference
cast_temp <- dcast(map_data, census_block_group ~ year)
cast_temp$diff <- cast_temp$`2021` - cast_temp$`2020`
cast_temp <- cast_temp %>% mutate(diff_r= rank(diff)) %>%
  mutate(diff_log = log(diff_r)) %>% mutate(diff_cut = cut(diff, breaks = c(-Inf, seq(-3, 3, 1), Inf))) %>% 
  mutate(diff_sign = ifelse(diff > 0, 1, ifelse(diff < 0, -1, 0)))
map_data <- merge(cbg, cast_temp, by = "census_block_group")

## summarize by cbg and add to map data 
mp <- data %>% group_by(census_block_group) %>%
  summarize(income = unique(income_group_pop, na.rm = T), 
            percentile = mean(p_high, na.rm = T), 
            mean_temp = mean(mean_high_c, na.rm = T))
map_data <- merge(map_data, mp, by = "census_block_group")

days_above_34 <- data %>% filter(mean_high_c  >= 34 & year %in% c(2020,2021)) %>% 
  count(census_block_group) 
map_data <- merge(map_data, days_above_34, by = "census_block_group")
map_data <- map_data %>% mutate(percentilen = as.factor(cut(percentile, 5)))

#### make some figs ----
pdf(paste0("./visuals/pub_figures/view_", td, ".pdf"))
#### maps ---- 

## change in avg mobility 
ggplot(data = map_data) +
  ggtitle("Change in Average Mobility 2020-2021 by CBG") +
  geom_sf(data = map_data, size = 0.002, aes(fill = as.factor(diff_sign))) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey") +
  labs(colour="Sign of Mobility Change") +
  theme_bw()

## Income
ggplot(map_data, aes(geometry = geometry)) +
  ggtitle("Income Distribution in San Francisco Bay Area by CBG") +
  geom_sf(data = map_data, size = 0.002, aes(fill = as.factor(income))) +
  scale_fill_discrete(na.value = "grey") +
  labs(colour="Income Group") +
  theme_bw()

## Average Temperature
ggplot(map_data, aes(geometry = geometry)) +
  ggtitle("Average Temperature Summer 2020-21") +
  geom_sf(data = map_data, size = 0.002, aes(fill = mean_temp)) +
  scale_fill_distiller(palette = "RdOrYl", na.value = "grey") +
  labs(colour="Average Summer Temperature") +
  theme_bw()

## supplemental maps 
## Avg num days above 34C
ggplot(map_data, aes(geometry = geometry)) +
  ggtitle("Total Number of Days at or Above 34C") +
  geom_sf(data = map_data, size = 0.002, aes(fill = n)) +
  scale_fill_distiller(palette = "RdPu", direction = 1, na.value = "grey") +
  labs(colour="# Days >= 34C") +
  theme_bw()

## Average Temperature Percentile in Summer
ggplot(map_data, aes(geometry = geometry)) +
  ggtitle("Average Temperature Percentile Summer 2020-21") +
  geom_sf(data = map_data, size = 0.002, aes(fill = as.factor(percentilen))) +
  scale_fill_discrete(na.value = "grey") +
  labs(colour="Average Percentile") +
  theme_bw()


#### timelines ----

data_all <- data_all %>% 
  filter(year %in% c(2020:2021)) %>%
  group_by(date, income_group_pop) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T),
            avg_mobility = mean(visitors_percap, na.rm = T)) %>% 
  filter(!date %in% seq(as.Date("2021-10-28"), by = "day", length.out = 3))

#vertical lines 
xints <- c(min(data$date), max(data %>% filter(year == 2020) %>% pull(date)), min(data %>% filter(year == 2021) %>% pull(date)), max(data$date))
temp <- ggplot(data = data_all, aes(x=date, y = avg_temp)) +
  geom_line(alpha=0.5, position="identity") + 
  geom_vline(xintercept = xints, color = "tomato") +
  #geom_point(data = data, alpha=0.5, aes(x = date, y = avg_temp), color = "tomato") +
  labs(x = "Date", y = "Average Daily Temperature") +
  theme_bw()

mobility <- ggplot(data = data_all %>% filter(!is.na(income_group_pop)), aes(x=date, y = avg_mobility, group = income_group_pop)) +
  geom_smooth(aes(group=income_group_pop, color=as.factor(income_group_pop))) +
  geom_vline(xintercept = as.Date("2020-03-17"), color = "tomato") +
  labs(x = "Date", y = "Average Mobility", color = "Income Group") +
  theme_bw()

grid.arrange(temp, mobility, nrow = 2)

#### distribution tests ----

tests <- c()
for(yr in 2020:2021) {
  
  ds <- data %>% filter(year == yr)
  
  for(inc in 1:5) {
    for(comp in 1:5) {
      if(inc == comp) next
      
      tryCatch({
        w <- wilcox.test(ds %>% filter(income_group_pop == inc) %>% pull(visitors_percap), 
                         ds %>% filter(income_group_pop == comp) %>% pull(visitors_percap))
        ds_wilcox <- round(w$p.value, 10)
      }, error=function(e){ds_wilcox <- NA})
      
      ds_ks <- tryCatch({
        w <- ks.test(ds %>% filter(income_group_pop == inc) %>% pull(visitors_percap), 
                     ds %>% filter(income_group_pop == comp) %>% pull(visitors_percap))
        ds_ks <- round(w$p.value, 10)
      }, error=function(e){ds_ks <- NA})
      
      
      tests <- rbind(cbind(wilcox = round(ds_wilcox, 3),
                           ks = round(ds_ks, 3), income_grp = inc, year = yr, comparison_year = comp), tests)
      
    }
  }
}

write.csv(tests, "./visuals/pub_figures/dist_test_table.csv")

#### distribution graphs ----

ggplot(data = data %>% filter(visitors_percap <= 24.2), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  geom_vline(xintercept = c(4.4, 6.3)) +
  ggtitle("Distribution of MI <= 24.2 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data %>% filter(visitors_percap > 4.4 & visitors_percap < 24.2), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI > 4.4 (95th percentile) \n & < 24.2(99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data %>% filter(visitors_percap > 6.3 & visitors_percap < 24.2), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI > 6.3 (97.5th percentile) \n & < 24.2 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data %>% filter(visitors_percap >= 24.2), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI >= 24.2 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

#### suplemental distribution ----
ggplot(data = data, 
       aes(x = visitors_percap, group = as.factor(year),
           color = as_factor(year), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Full Distribution of MI") +  
  geom_vline(xintercept = quantile(data$visitors_percap, .95)) +
  geom_vline(xintercept = quantile(data$visitors_percap, .999)) +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data, 
       aes(x = visitors_percap_log, group = as.factor(year),
           color = as_factor(year), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Full Distribution of MI") +  
  geom_vline(xintercept = quantile(data$visitors_percap_log, .95)) +
  geom_vline(xintercept = quantile(data$visitors_percap_log, .999)) +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data, 
       aes(x = visitors_percap_cr, group = as.factor(year),
           color = as_factor(year), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Full Distribution of MI") +  
  geom_vline(xintercept = quantile(data$visitors_percap_cr, .95)) +
  geom_vline(xintercept = quantile(data$visitors_percap_cr, .999)) +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

#### quantiles ----
quantiles <- c(.25, .5, .75, .95)

results <- c()
for(yr in 2020:2021) {
  
  for(ig in 1:5) {
    
    print(ig)
    ds <- data %>% filter(year == yr & income_group_pop == ig)
    
    for(quantile in quantiles) {
      
      qr.b <- rq(visitors_percap ~ mean_high_c, tau = quantile, data = ds)
      sum  <- summary(qr.b)$coefficients
      
      row <- as.data.frame(cbind(year = yr, quant = qr.b$tau, group = ig, 
                                 pval = sum[2,4] , coef = sum[2,1], se = sum[2,2]))
      row <- row %>% mutate(upper = coef + (2*se), lower = coef - (2*se))
      results <- rbind(results, row)
    }
  }
  
}

results <- as.data.frame(results)

results <- results %>% mutate(quant_name = paste0(quant * 100, "th"),
                              group_name = paste("Income Group:", group))
ggplot(data = results, 
       aes(x = quant_name, y = coef, group = as.factor(year), color = as.factor(year), ymin = lower, ymax = upper)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=.5) +
  facet_wrap( ~ group_name, nrow = 2) +
  labs(x = "Income Group", y = "Change in MI per degree increase",
       title = "Quantile Regression Slope for all CBGs by Year") +
  theme_bw()

#### Fixed Effects MI v Temp ----
m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = data)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 0:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=24]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=24]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=24]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ mean_high_c | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI v Temp : Income----
m_inter <- felm(visitors_percap_cr ~ mean_high_c:as.factor(income_group_pop) | census_block_group + monthweekyr, data = data)

coefs_interacted <- c()
for(i in 1:1000) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c:as.factor(income_group_pop) | census_block_group + monthweekyr, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Income Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept \n 
       felm(visitors_percap_cr ~ mean_high_c:as.factor(income_group_pop)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()

dev.off()

#### Supplemental ----

#### interacted income log(MI) + month + week + yr ----
m_inter <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + monthweekyr, data = data)

coefs_interacted <- c()
for(i in 1:1000) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + monthweekyr, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Income Group", y = "Change in MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept \n 
       felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()

#### interacted income log(MI) + date ----
m_inter <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + date, data = data)

coefs_interacted <- c()
for(i in 1:1000) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + date, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Income Group", y = "Change in MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept \n 
       felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()


#### interacted income (MI)^(1/3) + date ----
m_inter <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + date, data = data)

coefs_interacted <- c()
for(i in 1:1000) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop) | census_block_group + date, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Income Group", y = "Change in MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept \n 
       felm(visitors_percap_log ~ mean_high_c:as.factor(income_group_pop)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()


#### break ----
pdf(paste0("./visuals/pub_figures/viewsupp_", td, ".pdf"))

#### Fixed Effects MI v Temp ----
m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = data)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 0:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=24]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=24]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=24]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ mean_high_c | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI cr vs mean_high_c  interacted with the average mobility----
m_inter <- felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group) | census_block_group + monthweekyr, data = data)

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group) | census_block_group + monthweekyr, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group \n
       based on percentile of median MI for each CBG \n 
       <5%, 5-25%, 25-50%, 75-95%, >95% \n
       felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()

#### Fixed Effects MI v Temp 2020 only ----
m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweek, data = data %>% filter(year ==2020))
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2020)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweek, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 0:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=24]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=24]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=24]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "2020 ONLY \n felm(visitors_percap_cr ~ mean_high_c | 
       census_block_group + monthweek, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI cr vs mean_high_c  interacted with the average mobility 2020 only----
m_inter <- felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group) | census_block_group + monthweek, data = data %>% filter(year == 2020))

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2020)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group) | census_block_group + monthweek, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group 2020 ONLY \n
       based on percentile of median MI for each CBG \n
       <5%, 5-25%, 25-50%, 75-95%, >95% \n
       felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group)
       | census_block_group + monthweek, data = data") +
  theme_bw()


#### Fixed Effects MI v Temp 2021 only ----
m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweek, data = data %>% filter(year ==2021))
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2021)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweek, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 0:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=24]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=24]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=24]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "2021 ONLY \n felm(visitors_percap_cr ~ mean_high_c | 
       census_block_group + monthweek, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI cr vs mean_high_c  interacted with the average mobility 2021 only----
m_inter <- felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group) | census_block_group + monthweek, data = data %>% filter(year == 2021))

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2021)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group) | census_block_group + monthweek, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group 2021 ONLY \n
       based on percentile of median MI for each CBG bottom \n
       <5%, 5-25%, 25-50%, 75-95%, >95% \n 
       felm(visitors_percap_cr ~ mean_high_c:as.factor(mi_group)
       | census_block_group + monthweek, data = data") +
  theme_bw()

#### only on days where >5% of CBGs above their own 95th percentile ----
min_num_cbg_over_95th <- length(unique(data$census_block_group)) * 0.05
data_subgroup <- data %>% filter(n_over_their_95th >= min_num_cbg_over_95th)

m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = data_subgroup)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:20) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data_subgroup %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 25:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=10]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=10]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=10]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ mean_high_c | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept >5% CBGs above their own 95th percentile ", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### only on days where >5% of CBGs above overall 95th percentile ----
min_num_cbg_over_95th <- length(unique(data$census_block_group)) * 0.05
data_subgroup <- data %>% filter(n_over_total >= min_num_cbg_over_95th)

m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = data_subgroup)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data_subgroup %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ mean_high_c | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = 25:45)
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=10]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=10]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=10]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ mean_high_c | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept >5% CBGs above Bay Area 95th percentile", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()



dev.off()


#### same after break but z scores ----
pdf(paste0("./visuals/pub_figures/viewsupp_z_scores", td, ".pdf"))

#### Fixed Effects MI v Temp ----
m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweekyr, data = data)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = seq(-2, 5, .05))
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=0]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=0]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=0]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ z_score_high | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()


#### MI cr vs mean_high_c  interacted with the average mobility----
m_inter <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweekyr, data = data)

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweekyr, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group \n
       based on percentile of median MI for each CBG \n 
       <5%, 5-25%, 25-50%, 75-95%, >95% \n
       felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()

#### Fixed Effects MI v Temp 2020 only ----
m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweek, data = data %>% filter(year ==2020))
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2020)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweek, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = seq(-2, 5, .05))
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=0]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=0]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=0]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "2020 ONLY \n felm(visitors_percap_cr ~ z_score_high | 
       census_block_group + monthweek, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI cr vs mean_high_c  interacted with the average mobility 2020 only----
m_inter <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweek, data = data %>% filter(year == 2020))

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2020)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweek, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group 2020 ONLY \n
       based on percentile of median MI for each CBG \n
       <5%, 5-25%, 25-50%, 75-95%, >95% \n
       felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group)
       | census_block_group + monthweek, data = data") +
  theme_bw()


#### Fixed Effects MI v Temp 2021 only ----
m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweek, data = data %>% filter(year ==2021))
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2021)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweek, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = seq(-2, 5, .05))
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=0]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=0]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=0]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "2021 ONLY \n felm(visitors_percap_cr ~ z_score_high | 
       census_block_group + monthweek, data = data) 
       \n May-Sept", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### MI cr vs mean_high_c  interacted with the average mobility 2021 only----
m_inter <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweek, data = data %>% filter(year == 2021))

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f & year == 2021)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweek, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group 2021 ONLY \n
       based on percentile of median MI for each CBG bottom \n
       <5%, 5-25%, 25-50%, 75-95%, >95% \n 
       felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group)
       | census_block_group + monthweek, data = data") +
  theme_bw()

#### only on days where >5% of CBGs above their own 95th percentile ----
min_num_cbg_over_95th <- length(unique(data$census_block_group)) * 0.05
data_subgroup <- data %>% filter(n_over_their_95th >= min_num_cbg_over_95th)

m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweekyr, data = data_subgroup)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:20) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data_subgroup %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = seq(-1, 5, .05))
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=0]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=0]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=0]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ z_score_high | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept >5% CBGs above their own 95th percentile ", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()

#### only on days where >5% of CBGs above overall 95th percentile ----
min_num_cbg_over_95th <- length(unique(data$census_block_group)) * 0.05
data_subgroup <- data %>% filter(n_over_total >= min_num_cbg_over_95th)

m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweekyr, data = data_subgroup)
quantiles <- c(.025, .5, .975)
## bootstrap coefficients (resample within each county)
coefs <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data_subgroup %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high | census_block_group + monthweekyr, data = ds_all)
  coefs <- cbind(m$coefficients, coefs)
}

## plot
coefs <- quantile(coefs, quantiles)
plot_data <- as.data.frame(x = seq(-1, 5, .05))
colnames(plot_data) <- c("x")

y <- plot_data$x * coefs[2]
plot_data$y <- y - y[x=0]

upper <- plot_data$x * coefs[3]
plot_data$upper <- upper - upper[x=0]

lower <- plot_data$x * coefs[1]
plot_data$lower <- lower - lower[x=0]

ggplot(data = plot_data, aes(x, y))+
  geom_ribbon(data = plot_data, aes(ymin = lower, ymax = upper), linetype=2, alpha = 0.25, fill = "purple") +
  geom_line(data = plot_data, aes(x, y))+ 
  labs(title = "felm(visitors_percap_cr ~ z_score_high | 
       census_block_group + monthweekyr, data = data) 
       \n May-Sept >5% CBGs above Bay Area 95th percentile", x = "Temperature C", y = "3√(MI)") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme_bw()



dev.off()


#### doubled interaction term (MI group AND income group)
m_inter <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_income_groups) | census_block_group + monthweekyr, data = data)

coefs_interacted <- c()
for(i in 1:100) {
  print(i)
  ds_all <- c()
  for(f in included_fips) {
    ds <- data %>% filter(fips == f)
    samp <- sample(1:nrow(ds), nrow(ds), replace = T)
    ds <- ds[samp,]
    ds_all <- rbind(ds_all, ds)
  }
  m <- felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group) | census_block_group + monthweekyr, data = ds_all)
  coefs_interacted <- cbind(m$coefficients, coefs_interacted)
}

coef_quants <- apply(coefs_interacted, 1, function(x) quantile(x, quantiles))
coef_quants <- cbind(t(as.data.frame(coef_quants)), c(1:5))
colnames(coef_quants) <- c("low", "mid", "high", "group")
coef_quants <- rbind(coef_quants, c(coefs, 6))

ggplot(data = as.data.frame(coef_quants), 
       aes(x = group, y = mid, ymin = low, ymax = high)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=coef_quants[6,2],  linetype="dashed", 
             color = "red", size=.5) +
  geom_vline(xintercept = 5.5, color = "red") +
  labs(x = "Mobility Group", y = "Change in 3√MI per degree increase C",
       title = "Fixed Effects Slope for all CBGs May-Sept by MI group \n
       based on percentile of median MI for each CBG \n 
       <5%, 5-25%, 25-50%, 75-95%, >95% \n
       felm(visitors_percap_cr ~ z_score_high:as.factor(mi_group)
       | census_block_group + monthweekyr, data = data") +
  theme_bw()

#### final ----

dev.off()


#plot_data <- as.data.frame(x = seq(-2, 5, .05))
#X = 0