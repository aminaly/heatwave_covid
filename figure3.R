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

## simplify zoning data
zoning_cbg$cbg <- paste0(zoning_cbg$STATEFP, zoning_cbg$COUNTYFP, zoning_cbg$TRACTCE, zoning_cbg$BLKGRPCE)
zoning_cbg$fips <- paste0(zoning_cbg$STATEFP, zoning_cbg$COUNTYFP)
zoning_cbg_summary <- zoning_cbg %>% filter(zoning %in% c(0:3)) %>% group_by(cbg, fips) %>%
  summarize(non_res = sum(zoning == 0)/length(zoning), 
            multi = sum(zoning == 2)/length(zoning), 
            single = sum(zoning == 1)/length(zoning),
            non_dev = sum(zoning == 3)/length(zoning))

# version without geometry for calulations across columns
zoning_cbg_nogeo <- as.data.frame(zoning_cbg_summary)[,1:5]
zcng_nocbg <- zoning_cbg_nogeo[,3:5]

# calculate which zone type is the dominant and return column name (also populate nogeo)
zoning_cbg_summary$main_zoning <- colnames(zcng_nocbg)[apply(zcng_nocbg, 1, which.max)]
zoning_cbg_nogeo$main_zoning <- colnames(zcng_nocbg)[apply(zcng_nocbg, 1, which.max)]

## combine zoning with mobility data
temp_mobility_data_sm <- temp_mobility_data %>% 
  filter(mean_high_c >= 34) %>%
  group_by(year, cbg = census_block_group, fips, xvar = mean_high_c) %>%
  summarize(yvar = mean(visitors_percap, na.rm = T))

zoning_mob <- merge(temp_mobility_data_sm, zoning_cbg_summary, by = c("cbg", "fips"))
zoning_mob <- st_as_sf(zoning_mob)

#### Start PDF
pdf(paste0("./visuals/pub_figures/fig3_", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

#### Plot mobility on days over 30 in 2018

zoning_mob_over30_2018 <- zoning_mob %>% filter(year == 2018)
zoning_mob_over30_2019 <- zoning_mob %>% filter(year == 2019)
zoning_mob_over30_2020 <- zoning_mob %>% filter(year == 2020)

ggplot(data = zoning_mob_over30_2018) +
  ggtitle("Bay Area 2018 Summer Mobility Over 30 Degrees") +
  geom_sf(data = zoning_mob_over30_2018, size = 0.002, aes(fill = yvar, geometry = "geometry")) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = zoning_mob_over30_2019) +
  ggtitle("Bay Area 2019 Summer Mobility Over 30 Degrees") +
  geom_sf(data = zoning_mob_over30_2019, size = 0.002, aes(fill = yvar, geometry = "geometry")) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = zoning_mob_over30_2020) +
  ggtitle("Bay Area 2020 Summer Mobility Over 30 Degrees") +
  geom_sf(data = zoning_mob_over30_2020, size = 0.002, aes(fill = yvar, geometry = "geometry")) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

#### Shut down pdf
dev.off()