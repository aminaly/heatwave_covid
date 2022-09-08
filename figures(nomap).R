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

pdf(paste0("./visuals/pub_figures/fig6_temp", td, ".pdf"))

ys <- c(2020, 2021)

#### plots ----
quantiles <- c(.25, .5, .75, .95)

income <- unique(data %>% select(census_block_group, median_income, unweighted_pop))
income <- income %>% arrange(median_income) %>% mutate(cum_population = cumsum(unweighted_pop)) %>% 
  mutate(income_group_pop = ntile(median_income, 5)) %>% select(census_block_group, income_group_pop)
data <- left_join(data, income, by = "census_block_group")

hotdays <- data %>% group_by(date) %>% 
  summarize(max_temp = max(mean_high_c, na.rm = T)) %>% 
  filter(max_temp >= 34) %>% pull(date)
data_subset <- data %>% filter(date %in% hotdays & year %in% ys) %>% mutate(minority = ifelse(maxdemo %in% c("p_white", "p_asian"), FALSE, TRUE))
data_all <- data %>% filter(year %in% ys) %>% mutate(hotday = ifelse(date %in% hotdays, T, F))
results <- c()

for(yr in 2020:2021) {
  
  for(ig in 1:5) {
    
    print(ig)
    ds <- data_subset %>% filter(year == yr & income_group_pop == ig)
    
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

pdf(paste0("./visuals/pub_figures/fig7_coefs", td, ".pdf"))
results <- results %>% mutate(quant_name = paste0(quant * 100, "th"),
                              group_name = paste("Income Group:", group))
ggplot(data = results, 
       aes(x = quant_name, y = coef, group = as.factor(year), color = as.factor(year), ymin = lower, ymax = upper)) +
  geom_point(position = position_dodge2(1)) +
  geom_errorbar(width = 1, position = position_dodge2(1)) + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=.5) +
  facet_wrap( ~ group_name, nrow = 2) +
  labs(x = "Income Group", y = "Change in MI per degree increase above 34C",
       title = "Quantile Regression Slope for all CBGs \n when at least one county >=34C ")
  theme_bw()
dev.off()

## plot the results
td <- format(Sys.Date(), "%m_%d_%Y")
pdf(paste0("./visuals/pub_figures/fig6_temp_income", td, ".pdf"))

ys <- c(2020, 2021)

ggplot(data = data_subset, aes(mean_high_c, visitors_percap, group = year)) +
  geom_quantile(aes(linetype = year, color = ..quantile..), quantiles =  c(.25, .5, .75, .95)) +
  labs(title = "temp > 34") + 
  theme_bw()

ggplot(data = data_subset, aes(mean_high_c, visitors_percap, group = year))+
  geom_quantile(aes(linetype = year, color = ..quantile..), quantiles =  c(.25, .5, .75, .95)) +
  labs(title = "MI > 99th% temp > 34") + 
  facet_wrap( ~ income_group_pop) +
  theme_bw()

dev.off()

results_minority <- c()

for(j in unique(data_subset$minority)) {
  
  print(j)
  
  quant_reg_2020 <- summary(rq(visitors_percap ~ mean_high_c, quantiles,
                               data = data_subset %>% filter(year == 2020 & minority == j)), se = "boot")
  quant_reg_2021 <- summary(rq(visitors_percap ~ mean_high_c, quantiles,
                               data = data_subset %>% filter(year == 2021 & minority == j)), se = "boot")
  
  for(i in 1:length(quantiles)) {
    
    year <- 2020
    quant <- quant_reg_2020[[i]]$tau
    coefs <- quant_reg_2020[[i]]$coefficients
    
    results_minority <- rbind(results_minority, cbind(year, quant, group = j, coefs))
    
    year <- 2021
    quant <- quant_reg_2021[[i]]$tau
    coefs <- quant_reg_2021[[i]]$coefficients
    
    results_minority <- rbind(results_minority, cbind(year, quant, group = j, coefs))
    
  }
  
  
}

write.csv(results_minority, "quantile_regression_minority.csv")

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

dev.off()


#### wilcox and ks test ----

tests <- c()
for(yr in 2020:2021) {
  
  ds <- data_subset %>% filter(year == yr)
  
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

pdf(paste0("./visuals/pub_figures/fig8_dist", td, ".pdf"))

ggplot(data = data_subset %>% filter(visitors_percap <= 20), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI <= 4.5 (95th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_subset %>% filter(visitors_percap > 4.5 & visitors_percap < 6.75), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI > 4.5 (95th percentile) \n & < 20 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()
ggplot(data = data_subset %>% filter(visitors_percap > 6.75 & visitors_percap < 20), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI > 6.75 (97.5th percentile) \n & < 20 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_subset %>% filter(visitors_percap >= 20), 
       aes(x = visitors_percap, group = as.factor(income_group_pop),
           color = as_factor(income_group_pop), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34 MI >= 20 (99.9th percentile)") +   
  facet_wrap( ~ year, nrow = 2) +
  theme_bw()

ggplot(data = data_subset, 
       aes(x = visitors_percap, group = as.factor(year),
           color = as_factor(year), alpha=0.2)) +
  geom_density(size=.75) +
  ggtitle("Distribution of MI when temp >= 34") +  
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 6.75) + 
  geom_vline(xintercept = 20) +
  #facet_wrap( ~ year, nrow = 2) +
  theme_bw()

dev.off()

pdf(paste0("./visuals/pub_figures/fig9_temp", td, ".pdf"))

temp_mobility_data_byday <- data_subset %>% group_by(date, income_group_pop, year, monthday) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T))

ggplot(data = temp_mobility_data_byday, aes(x=monthday, y = avg_temp, group = year, color = year)) +
  geom_line(alpha=0.2, position="identity") + 
  facet_wrap( ~ income_group_pop, nrow = 2) +
  theme_bw()
  
dev.off()

pdf(paste0("./visuals/pub_figures/fig10_mobility", td, ".pdf"), paper = "USr")

temp_mobility_data_byday <- data %>% filter(year %in% c(2020, 2021)) %>%
  group_by(date, income_group_pop, year, monthday) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T), 
            avg_visitors = mean(visitors_percap, na.rm = T))

ggplot(data = temp_mobility_data_byday, aes(x=date, y = avg_visitors, 
                                            group = as.factor(income_group_pop), 
                                            color = as.factor(income_group_pop))) +
  geom_smooth(alpha=0.2, position="identity", show.legend = FALSE) + 
  theme_bw()

dev.off()

data_all_95 <- data_all %>% filter(visitors_percap <= 4.23)
data_subset <- data_all_95 %>% group_by(income_group_pop, visitors_percap) %>%
  summarise(percgroup = ntile(visitors_percap, 4)) %>% ungroup()
pdf(paste0("./visuals/pub_figures/fig10_race", td, ".pdf"))
ggplot(data = data_subset, aes(x=as.factor(percgroup), y = visitors_percap, 
                                            group = as.factor(percgroup), 
                                            color = as.factor(percgroup))) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  facet_wrap( ~ income_group_pop, nrow = 2) +
  theme_bw()

dev.off()



