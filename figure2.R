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

#### Read in files
ifelse(dir.exists("~/Box Sync/heatwave_covid/"),
       setwd("~/Box Sync/heatwave_covid/"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/"))

zoning_cbg <- read_rds("./heatwaves_manual/BayAreaZoning/data/shapefile/zoning_cbg.rds")
temp_mobility_data <- read_rds("./heatwaves_manual/data_for_regression.rds")

#### prep data
zoning_cbg$cbg <- paste0(zoning_cbg$STATEFP, zoning_cbg$COUNTYFP, zoning_cbg$TRACTCE, zoning_cbg$BLKGRPCE)
zoning_cbg_summary <- zoning_cbg %>% filter(zoning %in% c(0:3)) %>% group_by(cbg) %>%
  summarize(non_res = sum(zoning == 0)/length(zoning), 
            multi = sum(zoning == 2)/length(zoning), 
            single = sum(zoning == 1)/length(zoning),
            non_dev = sum(zoning == 3)/length(zoning))

# version without geometry for calulations across columns
zoning_cbg_nogeo <- as.data.frame(zoning_cbg_summary)[,1:4]
zcng_nocbg <- zoning_cbg_nogeo[,2:4]

# calculate which zone type is the dominant and return column name (also populate nogeo)
zoning_cbg_summary$main_zoning <- colnames(zcng_nocbg)[apply(zcng_nocbg, 1, which.max)]
zoning_cbg_nogeo$main_zoning <- colnames(zcng_nocbg)[apply(zcng_nocbg, 1, which.max)]

## combine zoning with mobility data
temp_mobility_data_sm <- temp_mobility_data %>% select(cbg = census_block_group, date, 
                                                       yvar = visitors_percap, year, 
                                                       month = month.x, fips)
zoning_mob <- merge(temp_mobility_data_sm, zoning_cbg_summary, by = "cbg")

## zoning and mobility average for 2020
temp_mobility_2020 <- temp_mobility_data_sm %>% filter(year == 2020) %>%
  group_by(cbg) %>% summarise(yvar = mean(yvar, na.rm = T))
zoning_mob_2020 <- merge(temp_mobility_2020, zoning_cbg_summary, by = "cbg")

## zoning and mobility average for summer 2020
temp_mobility_summer_2020 <- temp_mobility_data_sm %>% 
  filter(year == 2020 & month %in% c(5:9)) %>%
  group_by(cbg) %>% summarise(yvar = mean(yvar, na.rm = T))
zoning_mob_summer_2020 <- merge(temp_mobility_summer_2020, zoning_cbg_summary, by = "cbg")

#### Start PDF
pdf(paste0("./visuals/pub_figures/fig2_", Sys.time(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

#### Plot zoning allocations
ggplot(data = zoning_cbg_summary) +
  ggtitle("Main Zoning per CBG") +
  geom_sf(data = zoning_cbg_summary, aes(fill = main_zoning), color = NA) +
  scale_color_manual(values=wes_palette(n=4, name="GrandBudapest1"), na.value = "grey") +
  labs(colour="Zoning") +
  theme_bw()

#### Plot 2020 mobility for each zone in Santa Clara County
santaclara <- zoning_mob_2020 %>% mutate(fips = substr(cbg, 1, 5)) %>%
  filter(fips == "06085")

ggplot(data = santaclara) +
  ggtitle("Mobility Santa Clara County") +
  geom_sf(data = zoning_cbg_summary, aes(fill = yvar), color = NA) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  facet_wrap( ~ main_zoning, scales = "free", nrow = 2) +
  labs(colour="Zoning") +
  theme_bw()

#### Plot 2020 mobility for each zone in SF County
sf <- zoning_mob %>% mutate(fips = substr(cbg, 1, 5)) %>%
  filter(fips == "06075") 

ggplot(data = sf) +
  ggtitle("Mobility Santa Clara County") +
  geom_sf(data = zoning_cbg_summary, aes(fill = yvar), color = NA) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  facet_wrap( ~ main_zoning, scales = "free", nrow = 2) +
  labs(colour="Zoning") +
  theme_bw()


#### Plot 2020 mobility for each zone in Alameda County
alameda <- zoning_mob %>% mutate(fips = substr(cbg, 1, 5)) %>%
  filter(fips == "06001")

ggplot(data = alameda) +
  ggtitle("Mobility Santa Clara County") +
  geom_sf(data = zoning_cbg_summary, aes(fill = yvar), color = NA) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  facet_wrap( ~ main_zoning, scales = "free", nrow = 2) +
  labs(colour="Zoning") +
  theme_bw()


##over time
# # line plot of mobility visitors over time separated by main zoning, grouped by year
ggplot(data=zoning_mob, aes(x=date, y=yvar, group=main_zoning)) +
  geom_smooth(aes(group=main_zoning, color=as.factor(main_zoning))) +
  ggtitle("Mobility by Zoning") + ylab("# Visitors / Home Devices") + xlab("Date") +
  scale_color_manual(values=wes_palette(n=4, name="GrandBudapest1")) +
  facet_wrap( ~ year, scales = "free", nrow = 2) +
  scale_x_date() +
  theme(text = element_text(size = 15)) +
  labs(colour="Zoning") +
  theme_bw()

#### Shut down pdf
dev.off()