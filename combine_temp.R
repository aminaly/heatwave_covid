library(dplyr)

all_files <- list.files("heatwaves_manual/temps/bg/", full.names = T)
combined <- data.frame()

for(i in 1:length(all_files)) {

  print(i)
  file <- all_files[i]
  f <- readRDS(file)
  combined <- bind_rows(combined, f)

}

saveRDS(combined, "heatwaves_manual/all_temp_data_long_blockgroup_Apr2021.RDS")
