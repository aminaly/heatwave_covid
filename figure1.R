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
temp_mobility_data <- read_rds("./heatwaves_manual/data_for_regression.rds")
cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp", stringsAsFactors = F) 

## combine temp and census block group shapefile
temp_mobility_data <- temp_mobility_data %>% mutate(STATEFP = substr(census_block_group, 1, 2), COUNTYFP = substr(census_block_group, 3, 5),
                                                    TRACTCE = substr(census_block_group, 1, 11))
cbg$census_block_group <- paste0(cbg$STATEFP, cbg$COUNTYFP, cbg$TRACTCE, cbg$BLKGRPCE)
cbg <- cbg %>% filter(census_block_group %in% temp_mobility_data$census_block_group)

pdf(paste0("./visuals/pub_figures/fig1", Sys.time(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

#filter for just summer months, and find avg for that summer by cbg
temp_mobility_sum <- temp_mobility_data %>% filter(month.x %in% c(5,6,7,8,9)) %>% 
  group_by(year, census_block_group) %>%
  summarize(visitors_percap = mean(visitors_percap, na.rm = T))
temp_mobility_cbg <- merge(cbg, temp_mobility_sum, by = "census_block_group")

#filter for just 2019/20 and take the log of the rank
data <- temp_mobility_cbg %>% filter(year %in% c(2019, 2020)) %>% 
  mutate(visitors_percap_r= rank(visitors_percap)) %>%
  mutate(visitors_percap_log = log(visitors_percap_r))

## avg summer mobility by year
# ggplot(data = data) +
#   ggtitle("Bay Area Summer Mobility Ranked & Normalized") +
#   geom_sf(data = data, size = .002, aes(fill = visitors_percap_log)) +
#   facet_wrap( ~ year, nrow = 1) +
#   scale_fill_gradient(low = "#5ab4ac", high = "#d8b365") +
#   labs(colour="Mobility Metric") +
#   theme_bw()

##pull out each year, join, and find the difference
cast_temp <- dcast(temp_mobility_sum, census_block_group ~ year)
cast_temp$diff <- cast_temp[,4] - cast_temp[,3]
cast_temp <- cast_temp %>% mutate(diff_r= rank(diff)) %>%
  mutate(diff_log = log(diff_r))
cast_temp <- merge(cbg, cast_temp, by = "census_block_group")

# ggplot(data = cast_temp) +
#   ggtitle("2020 - 2019 Difference in Mobility Ranked & Normalized") +
#   geom_sf(data = cast_temp, size = .002, aes(fill = diff_log)) +
#   scale_fill_gradient(low = "#5ab4ac", high = "#d8b365") +
#   labs(colour="Mobility Metric") +
#   theme_bw()

## just the sign of the difference
cast_temp <- cast_temp %>% mutate(diff_sign = ifelse(diff > 0, 1, ifelse(diff < 0, -1, 0)))
# ggplot(data = cast_temp) +
#   ggtitle("Mobility Change (Increase or Decrease") +
#   geom_sf(data = cast_temp, size = .002, aes(fill = diff_sign)) +
#   scale_fill_gradient(low = "#5ab4ac", high = "#d8b365") +
#   labs(colour="Mobility Metric Sign") +
#   theme_bw()

pdf(paste0("./visuals/pub_figures/fig1", Sys.time(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

##just the difference
ggplot(data = cast_temp) +
  ggtitle("Bay Area Summer Mobility Just Diff") +
  geom_sf(data = cast_temp, size = .002, aes(fill = cut(diff, breaks = c(-Inf, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, Inf))), na.value = "yellow") +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  labs(colour="Mobility Metric") +
  theme_bw()

dev.off()
# ordered <- cast_temp[order(abs(cast_temp$diff), decreasing = T),]
# ordered$diff[100:nrow(ordered),] <- 0
# ordered <- ordered %>% mutate(diff = ifelse(diff == 0, NA, diff))
# 
# ## just the sign of the difference
# ggplot(data = ordered) +
#   ggtitle("Top 100 changes") +
#   geom_sf(data = cast_temp, size = .002, aes(fill = diff)) +
#   scale_fill_gradient(low = "#5ab4ac", high = "#d8b365", na.value = "grey") +
#   labs(colour="Mobility Metric Magnitude") +
#   theme_bw()

dev.off()

##Now lets just try plotting without the outliers
outlier_threshold <- quantile(data$visitors_percap, prob = .95, na.rm = T)
outlier_mid <- quantile(data$visitors_percap, prob = c(.25, .75), na.rm = T)
data_nooutlier <- data %>% mutate(visitors_percap = ifelse(visitors_percap > outlier_threshold, NA, visitors_percap))
data_mid <- data %>% mutate(visitors_percap = ifelse(visitors_percap < outlier_mid[1] | visitors_percap > outlier_mid[2], NA, visitors_percap))

ggplot(data = data_nooutlier) +
  ggtitle("Bay Area Summer Mobility No Outliers \n <95% percentile") +
  geom_sf(data = data_nooutlier, size = .002, aes(fill = visitors_percap)) +
  facet_wrap( ~ year, nrow = 1) +
  scale_fill_gradient(low = "#5ab4ac", high = "#d8b365", na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = data_mid) +
  ggtitle("Bay Area Summer Mobility Middle 50% \n 25% < n < 75%") +
  geom_sf(data = data_nooutlier, size = .002, aes(fill = visitors_percap)) +
  facet_wrap( ~ year, nrow = 1) +
  scale_fill_gradient(low = "#5ab4ac", high = "#d8b365", na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()



#histogram of mobility in may
mob_may <- temp_mobility_data %>% filter(month.x == 5) %>% group_by(census_block_group, year) %>% summarize(visitors_percap = mean(visitors_percap, na.rm =T))
ggplot(mob_may, aes(x=visitors_percap, color=year)) +
  geom_histogram(fill="white", alpha=0.2, position="identity")

pdf(paste0("./visuals/figures/fig1", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

mob_jun <- temp_mobility_data %>% filter(month.x == 6) %>% group_by(census_block_group, year) %>% 
  summarize(visitors_percap = mean(visitors_percap, na.rm =T)) %>% filter(visitors_percap <= 20)
ggplot(mob_jun, aes(x=visitors_percap, group=year)) +
  geom_histogram(alpha=0.2, position="identity") +
  ggtitle("Mobility June") +
  scale_color_manual(values=wes_palette(n=3, name="Zissou1"))+
  labs(colour="Year") +
  theme_bw()

mob_aug <- temp_mobility_data %>% filter(month.x == 8) %>% group_by(census_block_group, year) %>% 
  summarize(visitors_percap = mean(visitors_percap, na.rm =T)) %>% filter(visitors_percap <= 20)
ggplot(mob_aug, aes(x=visitors_percap, group=year)) +
  geom_histogram(alpha=0.2, position="identity") +
  ggtitle("Mobility August") +
  scale_color_manual(values=wes_palette(n=3, name="Zissou1"))+
  labs(colour="Year") +
  theme_bw()

ggplot(mob_jun, aes(x=visitors_percap, group=year)) +
  geom_histogram(alpha=0.5, position="identity") +
  ggtitle("Mobility June") +
  facet_wrap( ~ year, scales = "free", nrow = 2) +
  labs(colour="Year") +
  theme_bw()

ggplot(mob_aug, aes(x=visitors_percap, group=year)) +
  geom_histogram(alpha=0.5, position="identity") +
  ggtitle("Mobility August") +
  facet_wrap( ~ year, scales = "free", nrow = 2) +
  labs(colour="Year") +
  theme_bw()
  
dev.off()

#histogram of mobility in august
mob_aug <- temp_mobility_data %>% filter(month.x == "08") %>% group_by(census_block_group) %>% summarize(mean(visitors_percap, na.rm =T))
ggplot(mob_aug, aes(x=visitors_percap, color=year)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")


dev.off()


