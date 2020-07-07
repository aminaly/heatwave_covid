setwd("~/Box Sync/heatwaves/")

library(ncdf4)
library(dplyr)
library(stringr)
library(sf)
library(raster)
library(velox)
library(lfe)
library(lubridate)


#get list of all precip data files
gridMET_files <- list.files("gridMET", pattern = "*.nc", full.names = T)

#load in counties
counties <- st_read("shapefiles/tl_2017_us_county.shp")

# Run through temperature brick and extract over the buffers
all_data <- c()

for(i in gridMET_files) {
  print(i)
  file <- brick(i)
  
  
  for(j in 1:length(names(file))) {
    temp <- c()
    nms <- as.numeric(substring(as.character(names(file[[j]])),2))
    temp$date <- rep(as.Date(nms, origin= "1900-01-01"), nrow(counties))
    temp <- as.data.frame(temp)
    temp$county <- counties$NAME
    temp$fips <- counties$GEOID
    temp$measure <- rep(substring(i, 9, 12), nrow(counties))
    
    velox_obj <- velox(file[[j]])
    temp_measure <- velox_obj$extract(sp = counties$geometry, small = T)
    
    temp$mean_measure <- lapply(temp_measure, function(x){mean(as.numeric(x), na.rm = T)}) %>% unlist()  
    temp$max_measure <- lapply(temp_measure, function(x){max(as.numeric(x), na.rm = T)}) %>% unlist()
    
    all_data <- bind_rows(all_data, temp)
    
  }
  
}

#save this out to make my life easier
saveRDS(all_data, paste0(getwd(), "calculated/temperature_data.rds"))
