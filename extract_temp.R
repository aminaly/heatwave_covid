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
library(exactextractr)

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
rep <- as.numeric(args[1])

#get list of all precip data files
gridMET_files <- list.files("heatwaves_manual/gridMET", pattern = "*pr", full.names = T)

#load in census tracts & select only those we want
included_fips <- c("06081", "06085", "06001", "06013","06075", "06087", "06041", "06097", "06055", "06095") #bay area 
block_group <- st_read("heatwaves_manual/shapefiles/cb_2019_us_bg_500k/cb_2019_us_bg_500k.shp")
block_group$fips <- paste0(block_group$STATEFP, block_group$COUNTYFP)
block_group <- block_group %>% filter(fips %in% included_fips)
st_crs(block_group) <- 4326 #currently NAD83, but virtually the same 

file_name <- paste0("./heatwaves_manual/temps/bg/", rep + 2019, "_pr_data.rds")

# Run through temperature brick and extract over the buffers
all_data <- c()

i <- gridMET_files[rep]

print(i)
file <- stack(i)
crs(file) <- CRS("+init=epsg:4326")
print("I got here")

ev <- exact_extract(file, block_group, c("mean"))

nms <- as.numeric(substring(as.character(names(file)),2))
days <- as.Date(nms, origin= "1900-01-01")
all_data <- c()

for(d in 1:length(days)) {
  day <- days[d]
  temp <- block_group %>% dplyr::select(COUNTYFP, GEOID, fips) %>% 
    mutate(date = day, measure = substring(i, 26, 29), year = year(day))
  temp <- cbind(temp, rh = ev[[d]])
  all_data <- rbind(all_data, temp)
  print(day)
}
 
  
#save this out to make my life easier
saveRDS(all_data, file_name)

