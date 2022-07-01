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

data_subset <- data %>% filter(mean_high_c >= 34 & year %in% ys) %>% mutate(minority = ifelse(maxdemo %in% c("p_white", "p_asian"), FALSE, TRUE))

results <- c()

for(yr in 2020:2021) {
  
  for(income_group in 1:5) {
    
    print(j)
    ds <- data_subset %>% filter(year == yr & income_group_pop == income_group)
    
    for(quantile in quantiles) {
      
      qr.b <- boot.rq(ds$mean_high_c, ds$visitors_percap, tau = quantile, R = 1000)
      conf <- t(apply(qr.b$B, 2, quantile, c(0.05, 0.5, 0.95)))
      
      row <- cbind(year = yr, quant = quantile, group = income_group, lower = conf[,1], value = conf[,2], upper = conf[,3])
      results <- rbind(results, row)
    }
  }
  
}


for(j in 1:5) {
  
  print(j)
  
  quant_reg_2020 <- summary(rq(visitors_percap ~ mean_high_c, quantiles,
                               data = data_subset %>% filter(year == 2020 & income_group_pop == j)), se = "boot", cov=TRUE)

  quant_reg_2021 <- summary(rq(visitors_percap ~ mean_high_c, quantiles,
                               data = data_subset %>% filter(year == 2021 & income_group_pop == j)), cov=TRUE)

  name <- paste("income", j, "2020_", sep = "_")
  mods <- append(mods, list(name = quant_reg_2020))
  
  name <- paste("income", j, "2021", sep = "_")
  mods <- append(mods, list(name = quant_reg_2021))
  
  for(i in 1:length(quantiles)) {
    
    year <- 2020
    quant <- quant_reg_2020[[i]]$tau
    coefs <- quant_reg_2020[[i]]$coefficients
    
    results <- rbind(results, cbind(year, quant, group = j, coefs))
    
    year <- 2021
    quant <- quant_reg_2021[[i]]$tau
    coefs <- quant_reg_2021[[i]]$coefficients
    
    results <- rbind(results, cbind(year, quant, group = j, coefs))
    
  }
  
  
}

quant_reg_2020 <- summary(rq(visitors_percap ~ mean_high_c, quantiles,
                             data = data_subset %>% filter(year == 2020)), se = "boot")
quant_reg_2021 <- summary(rq(visitors_percap ~ mean_high_c, quantiles,
                             data = data_subset %>% filter(year == 2021)), se = "boot")
 
for(i in 1:3) {
  j <- 6
  year <- 2020
  quant <- quant_reg_2020[[i]]$tau
  coefs <- quant_reg_2020[[i]]$coefficients
  
  results <- rbind(results, cbind(year, quant, group = j, coefs))
  
  year <- 2021
  quant <- quant_reg_2021[[i]]$tau
  coefs <- quant_reg_2021[[i]]$coefficients
  
  results <- rbind(results, cbind(year, quant, group = j, coefs))
  
}


write.csv(results, "quantile_regression_bydemo_income_popweighted_all.csv")


results <- as.data.frame(results)
results <- results %>% rename(stderr = `Std. Error`, tval = `t value`) %>%
  mutate(me = tval * stderr) %>% mutate(lb = Value - me, ub = Value + me)
results$res <- ifelse(grepl("mean_high_c", row.names(results)), "coef", "intercept")

pdf(paste0("./visuals/pub_figures/fig7_coefs", td, ".pdf"))
ggplot(data = results %>% filter(res == "coef"), 
       aes(x = quant, y = Value, group = year, color = year, ymin = lb, ymax = ub)) +
  geom_point(position = position_dodge2(2)) +
  geom_errorbar(width = 2, position = position_dodge2(2)) + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=1) +
  facet_wrap( ~ group) +
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
pdf(paste0("./visuals/pub_figures/fig6_demo", td, ".pdf"))
ggplot(data = racem %>% filter(!is.na(income_group_pop) & !variable %in% c("not_latinx", "latinx")), aes(x = variable, y = pop))+
  geom_bar(stat="identity", aes(fill = variable)) +
  labs(title = "Demographic by income group") + 
  facet_wrap( ~ income_group_pop) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = racem %>% filter(!is.na(income_group_pop) & variable %in% c("not_latinx", "latinx")), aes(x = variable, y = pop))+
  geom_bar(stat="identity", aes(fill = variable)) +
  labs(title = "Hispanic/Latinx by income group") + 
  facet_wrap( ~ income_group_pop) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) 
dev.off()


