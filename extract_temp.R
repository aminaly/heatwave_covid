ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

library(ncdf4)
library(dplyr)
library(stringr)
library(sf)
library(raster)
library(velox)
library(lfe)
library(lubridate)

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
rep <- as.numeric(args[1])


#get list of all precip data files
gridMET_files <- list.files("heatwaves_manual/gridMET", pattern = "*.nc", full.names = T)

#load in cencus tracts & select only those we want
block_group <- st_read("heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp")
income <- read.csv(paste0(getwd(), "/us_census/income_county.csv"), stringsAsFactors = F, header = T)
block_group$fips <- paste0(block_group$STATEFP, block_group$COUNTYFP)
block_group <- block_group %>% filter(fips %in% income$fips)

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
  temp$county <- block_group$NAME
  temp$fips <- block_group$GEOID
  temp$measure <- rep(substring(i, 26, 29), nrow(block_group))
  
  velox_obj <- velox(file[[j]])
  temp_measure <- velox_obj$extract(sp = block_group$geometry, small = T)
  
  temp$mean_measure <- lapply(temp_measure, function(x){mean(as.numeric(x), na.rm = T)}) %>% unlist()  
  temp$max_measure <- lapply(temp_measure, function(x){max(as.numeric(x), na.rm = T)}) %>% unlist()
  
  all_data <- bind_rows(all_data, temp)
  
}
  

#save this out to make my life easier
saveRDS(all_data, paste0("./heatwaves_manual/temps/bg/", rep, "_temperature_data.rds"))

