
#### Load Packages
library(sf)
library(dplyr)
library(tidyverse)
library(lwgeom)
library(todor)
library(reticulate)

#### Set working directory
ifelse(dir.exists("~/Box Sync/heatwave_covid/BayAreaZoning/data/shapefile"),
       setwd("~/Box Sync/heatwave_covid/BayAreaZoning/data/shapefile"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/BayAreaZoning/data/shapefile"))

#### Get list of files to read in
city_zones <- list.files(".", pattern = "*.zip", full.names = T)

bayarea <- c()
#### Loop through all the files, select the columns we care about, and combine
for(file in city_zones) {
  unzipped <- unzip(file)
  shapefile <- str_subset(unzipped, "shp")
  if(length(shapefile > 1)) shapefile <- shapefile[!str_detect(shapefile, "xml")]
  shp <- st_read(dsn = shapefile, crs = 4326) #get the .shp file that is unzipped
  names(shp) <- tolower(names(shp)) #convert names to lowercase for inconsistencies
  shp <- shp %>% select(zoning, geometry)
  shp <- st_zm(shp)
  bayarea <- rbind(bayarea, shp)
}

#### Save out the final zoning file 
st_write(bayarea, dsn = "bayarea_zoning.shp")

#### Read in the Census Block Group File
ifelse(dir.exists("~/Box Sync/heatwave_covid/"),
       setwd("~/Box Sync/heatwave_covid/"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/"))

cbg <- st_read("./heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp")
cbg_cali <- cbg %>% filter(STATEFP == "06")

#### Join the datasets (Intersection)
cbg_cali <- st_transform(cbg_cali, crs = st_crs(bayarea)) #make sure the crs are matching. 
zoning_cbg <- st_intersection(cbg_cali, bayarea)

#### Save out the final intersecting file
st_write(bayarea, dsn = "./BayAreaZoning/data/shapefile/bayarea_zoning_cbg.shp")

