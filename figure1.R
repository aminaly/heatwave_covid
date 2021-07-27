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

temp_mobility_data <- temp_mobility_data %>% filter(!is.na(visitors_percap))

## combine temp and census block group shapefile
temp_mobility_data <- temp_mobility_data %>% mutate(STATEFP = substr(census_block_group, 1, 2), COUNTYFP = substr(census_block_group, 3, 5),
                                                    TRACTCE = substr(census_block_group, 1, 11))
cbg$census_block_group <- paste0(cbg$STATEFP, cbg$COUNTYFP, cbg$TRACTCE, cbg$BLKGRPCE)
cbg <- cbg %>% filter(census_block_group %in% temp_mobility_data$census_block_group)

#filter for just summer months, and find avg for that summer by cbg
temp_mobility_sum <- temp_mobility_data %>% filter(month.x %in% c(5,6,7,8,9)) %>% 
  group_by(year, census_block_group) %>%
  summarize(visitors_percap = mean(visitors_percap, na.rm = T))
temp_mobility_cbg <- merge(cbg, temp_mobility_sum, by = "census_block_group")

#filter for just 2019/20 and take the log of the rank
data <- temp_mobility_cbg %>% filter(year %in% c(2019, 2020)) %>% 
  mutate(visitors_percap_r= rank(visitors_percap)) %>%
  mutate(visitors_percap_log = log(visitors_percap_r))

##pull out each year, join, and find the difference
cast_temp <- dcast(temp_mobility_sum, census_block_group ~ year, )
cast_temp$diff <- cast_temp[,4] - cast_temp[,3]
cast_temp <- cast_temp %>% mutate(diff_r= rank(diff)) %>%
  mutate(diff_log = log(diff_r)) %>% mutate(diff_cut = cut(diff, breaks = c(-Inf, seq(-3, 3, 1), Inf))) %>% 
  mutate(diff_sign = ifelse(diff > 0, 1, ifelse(diff < 0, -1, 0)))
cast_temp <- merge(cbg, cast_temp, by = "census_block_group")

pdf(paste0("./visuals/pub_figures/fig1", Sys.time(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

### Map of mobility difference between 2019 and 2020
ggplot(data = cast_temp) +
  ggtitle("Bay Area Summer Mobility Difference 2020 - 2019") +
  geom_sf(data = cast_temp, size = .002, aes(fill = diff_cut)) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

### line plot of average visitors per cap each daily
temp_mobility_data_nona <- temp_mobility_data %>% filter(!is.na(income_group))

ggplot(data=temp_mobility_data_nona, aes(x=date, y=visitors_percap, group=income_group)) +
  geom_smooth(aes(group=income_group, color=as.factor(income_group))) +
  ggtitle("Mobility Full Timeline") + ylab("# Visitors / Home Devices") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=5, name="Zissou1")) +
  scale_x_date() +
  theme(text = element_text(size = 15)) +
  labs(colour="$$ Grp (5 High)") +
  theme_bw()

### bar chart of temperatures 
temp_mobility_data_nona_day <- temp_mobility_data_nona %>% group_by(date) %>% 
  summarize(avg_temp = mean(mean_high_c, na.rm = T))

ggplot(data = a, aes(x=date, y = avg_temp)) +
  geom_line(alpha=0.2, position="identity")

dev.off()


