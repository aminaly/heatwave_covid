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
cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp", stringsAsFactors = F) 
pop <- read_csv("us_census/annual_population_2010-2019.csv")

#### prep data ####
#### Prep Zoning Data ####
## simplify zoning data
zoning_cbg$cbg <- paste0(zoning_cbg$STATEFP, zoning_cbg$COUNTYFP, zoning_cbg$TRACTCE, zoning_cbg$BLKGRPCE)
zoning_cbg$fips <- paste0(zoning_cbg$STATEFP, zoning_cbg$COUNTYFP)
zoning_cbg_summary <- zoning_cbg %>% filter(zoning %in% c(0:3)) %>% group_by(cbg, fips) %>%
  summarize(non_res = sum(zoning == 0)/length(zoning), 
            multi = sum(zoning == 2)/length(zoning), 
            single = sum(zoning == 1)/length(zoning),
            non_dev = sum(zoning == 3)/length(zoning))

# version without geometry for calulations across columns
zoning_cbg_nogeo <- st_drop_geometry(zoning_cbg_summary)
zcng_nocbg <- zoning_cbg_nogeo[,3:6]

# calculate which zone type is the dominant and return column name (also populate nogeo)
zoning_cbg_summary$main_zoning <- colnames(zcng_nocbg)[apply(zcng_nocbg, 1, which.max)]
zoning_cbg_nogeo$main_zoning <- colnames(zcng_nocbg)[apply(zcng_nocbg, 1, which.max)]

#### prep CBG data ####
temp_mobility_data <- temp_mobility_data %>% filter(!is.na(visitors_percap))

## combine temp and census block group shapefile
temp_mobility_data <- temp_mobility_data %>% mutate(STATEFP = substr(census_block_group, 1, 2), COUNTYFP = substr(census_block_group, 3, 5),
                                                    TRACTCE = substr(census_block_group, 1, 11))
cbg$cbg <- paste0(cbg$STATEFP, cbg$COUNTYFP, cbg$TRACTCE, cbg$BLKGRPCE)
cbg <- cbg %>% filter(cbg %in% temp_mobility_data$census_block_group)

## combine zoning with mobility data
temp_mobility_data_sm <- temp_mobility_data %>% 
  filter(mean_high_c >= 34) %>%
  group_by(year, cbg = census_block_group, fips) %>%
  summarize(yvar = mean(visitors_percap, na.rm = T)) %>%
  mutate(yvar_cut = cut(yvar, c(seq(-1, 5, 1), Inf)))

temp_mobility_cbg <- merge(cbg, temp_mobility_data_sm, by = "cbg")

#### Start PDF ####
pdf(paste0("./visuals/pub_figures/fig3_", Sys.Date(), ".pdf"))
##Finalize datasets for regressions & run
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
     main = title)
text(x = 0.5, y = 0.5, paste(timestamp(), "\n Bay Area Data Overview"),
     cex = 1.5, col = "black")

#### Plot mobility on days over 30 in 2018

tm_2018 <- temp_mobility_cbg %>% filter(year == 2018)
tm_2019 <- temp_mobility_cbg %>% filter(year == 2019)
tm_2020 <- temp_mobility_cbg %>% filter(year == 2020)

ggplot(data = tm_2018) +
  ggtitle("Bay Area 2018 Summer Mobility Over 34 Degrees") +
  geom_sf(data = tm_2018, size = 0.002, aes(fill = yvar_cut)) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = tm_2019) +
  ggtitle("Bay Area 2019 Summer Mobility Over 34 Degrees") +
  geom_sf(data = tm_2019, size = 0.002, aes(fill = yvar_cut)) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

ggplot(data = tm_2020) +
  ggtitle("Bay Area 2020 Summer Mobility Over 34 Degrees") +
  geom_sf(data = tm_2020, size = 0.002, aes(fill = yvar_cut)) +
  scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
  labs(colour="Mobility Metric") +
  theme_bw()

## lets look at some smaller areas 

for(fip in unique(temp_mobility_cbg$fips)) {
  tm_19_20 <- temp_mobility_cbg %>% filter(year %in% c(2019, 2020)) %>% 
    filter(fips == fip) %>% mutate(pop_density = population / ALAND)
  
  print(ggplot(data = tm_19_20) +
    ggtitle(paste(fip, "Summer Mobility Over 34 Degrees")) +
    geom_sf(data = tm_19_20, size = 0.002, aes(fill = yvar_cut)) +
    scale_fill_brewer(palette = "PiYG", direction = -1, na.value = "grey") +
    facet_wrap( ~ year, nrow = 2) +
    labs(colour="Mobility Metric") +
    theme_bw())
  
  tm_19_20_max <- tm_19_20 %>% mutate(yvar = ifelse(yvar < 3, NA, yvar))
  
  print(ggplot(data = tm_19_20_max) +
    ggtitle(paste(fip, " Summer Mobility Over 34 Degrees & MI > 3")) +
    geom_sf(data = tm_19_20_max, size = 0.002, aes(fill = yvar)) +
    scale_fill_continuous(low = "#addd8e", high = "#31a354", na.value = "#e9a3c9") +
    facet_wrap( ~ year, nrow = 2) +
    labs(colour="Mobility Metric") +
    theme_bw())
  
  tm_onlymax <- tm_19_20_max %>% filter(!is.na(yvar)) 
  
  print(ggplot(data = tm_onlymax, aes(x = pop_density)) +
          geom_histogram() +
          ggtitle("Distribution of CBGs with MI > 3") +
          theme_bw())
  
  ## rearrange and add in zoning information for this fips
  # tm_max_cast <- st_drop_geometry(tm_19_20 %>% select(cbg, fips, year, yvar, ALAND))
  # tm_max_cast <- dcast(tm_max_cast, ALAND + cbg + fips ~ year, value.var = "yvar")
  # tm_max_cast <- tm_max_cast %>% mutate(diff = `2020` - `2019`) %>%
  #   filter(!is.na(diff)) %>%
  #   mutate(diff_cut = cut(diff, c(-Inf, 0, Inf), labels = c("Decrease in 2020", "Increase in 2020"))) 
  # tm_max_cast <- left_join(tm_max_cast, zoning_cbg_nogeo, by = c("cbg", "fips"))
  # 
  # print(ggplot(data = tm_max_cast, aes(x=diff_cut, y=main_zoning, fill=main_zoning)) +
  #   geom_bar(stat="identity", position=position_dodge()) +
  #   ggtitle(paste(fip, "Count of Zoning Type by Change in Mobility")) +
  #   scale_color_manual(values=wes_palette(n=2, name="Zissou1")) +
  #   theme_bw())


}

#### Shut down pdf
dev.off()