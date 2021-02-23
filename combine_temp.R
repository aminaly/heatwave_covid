library(dplyr)

all_files <- list.files("heatwaves_manual/temps", full.names = T)
combined <- data.frame()

for(file in all_files) {

  f <- readRDS(file)
  combined <- bind_rows(combined, f)

}

saveRDS(combined, "heatwaves_manual/all_temp_data_long_Feb_2021.RDS")
