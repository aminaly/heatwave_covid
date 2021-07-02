
#### Load Packages
library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(todor)
library(reticulate)


#### Read in files
ifelse(dir.exists("~/Box Sync/heatwave_covid/"),
       setwd("~/Box Sync/heatwave_covid/"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/"))

bayarea <- st_read("./BayAreaZoning/data/shapefile/bayarea_zoning.shp")
cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp")
cbg_cali <- cbg %>% filter(STATEFP == "06")

### get rid of problematic polygons
cbg_cali <- st_make_valid(cbg_cali)
bayarea <- st_make_valid(bayarea)

#### Join the datasets (Intersection)
cbg_cali <- st_transform(cbg_cali, crs = st_crs(bayarea)) #make sure the crs are matching. 
zoning_cbg <- merge(cbg_cali, bayarea)
zoning_cbg <- st_intersection(cbg_cali, bayarea)
write_rds(zoning_cbg, "./heatwaves_manual/BayAreaZoning/data/shapefile/zoning_cbg.rds")
st_write(zoning_cbg, "./heatwaves_manual/BayAreaZoning/data/shapefile/zoning_cbg.shp")
