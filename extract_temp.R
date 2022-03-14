ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

library(ncdf4)
library(dplyr)
library(stringr)
library(sf)
library(raster)
library(lfe)
library(lubridate)

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
rep <- as.numeric(args[1])

#get list of all precip data files
gridMET_files <- list.files("heatwaves_manual/gridMET", pattern = "*.nc", full.names = T)

#load in census tracts & select only those we want
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 
block_group <- st_read("heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp")
block_group$fips <- paste0(block_group$STATEFP, block_group$COUNTYFP)
block_group <- block_group %>% filter(fips %in% included_fips)
st_crs(block_group) <- 4326 #currently NAD83, but virtually the same 

file_name <- paste0("./heatwaves_manual/temps/bg/", rep, "_temperature_data.rds")

# Run through temperature brick and extract over the buffers
all_data <- c()

i <- gridMET_files[rep]

print(i)
file <- stack(i)
crs(file) <- CRS("+init=epsg:4326")
print("I got here")
 
for(j in 1:length(names(file))) {
  temp <- c()
  nms <- as.numeric(substring(as.character(names(file[[j]])),2))
  temp$date <- rep(as.Date(nms, origin= "1900-01-01"), nrow(block_group))
  temp <- as.data.frame(temp)
  temp$county <- block_group$COUNTYFP
  temp$census_block_group <- block_group$GEOID
  temp$fips <- block_group$fips
  temp$measure <- rep(substring(i, 26, 29), nrow(block_group))
  temp$year <- year(temp$date)
  
  
  extracted_vals <-  extract(file[[1]], block_group, na.rm = T)

  temp$mean_measure <- lapply(extracted_vals, function(x){mean(as.numeric(x), na.rm = T)}) %>% unlist()  
  temp$max_measure <- lapply(extracted_vals, function(x){max(as.numeric(x), na.rm = T)}) %>% unlist()
  
  all_data <- bind_rows(all_data, temp)
  
}
  
#save this out to make my life easier
saveRDS(all_data, file_name)

