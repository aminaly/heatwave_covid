# heatwaves

##Data Sources

Provisional COVID-19 Deaths by County in US (includes total deaths)
https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy

US Census Median Income
https://www.census.gov/data/datasets/2018/demo/saipe/2018-state-and-county.html

NOAA US Climate Regions
https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-regions.php

Weekly COVID-19 Deaths by County
https://covidcountydata.org/data/download

To Update Temp: 
- download gridMET year from site and put into folder called heatwaves_manual
- run_extract.sh (if you only want the most recent years of dowloaded data to run, change the array)
- run_combine.sh

- locally, navigate to heatwaves manual and sync the combined file: 
rsync -a aminaly@login.sherlock.stanford.edu:/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/heatwaves_manual/all_temp_data_long_blockgroup_Apr2021.RDS.RDS .

- open and run clean_temp.R (to do this locally, sync heatwaves_manual)


To Download any Safegraph Data
ml contribs 
ml poldrack
ml awscli/2

- follow instructions on downloading in the catalog: https://catalog.safegraph.io/app/information
— aws configure --profile safegraphws
— use access keys on catalog
- list store: 
— aws s3 ls s3://sg-c19-response/neighborhood-patterns/ --profile safegraphws --endpoint https://s3.wasabisys.com
- download: 
— aws s3 sync s3://sg-c19-response/neighborhood-patterns/ ./ --profile safegraphws --endpoint https://s3.wasabisys.com
-  put into folder called heatwaves_manual/safegraph



Some RSync Commands to copy and paste if you’re lazy, Amina
rsync -a aminaly@login.sherlock.stanford.edu:/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/heatwaves_manual/all_temperature_data_clean_2021.rds . 

rsync -a “aminaly@login.sherlock.stanford.edu:/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/visuals” .

new steps
- shorten files from the large main ones (filter for tips you care about) (shorten_files)
- update names in clean_shelter and run
- 

To Download Bay Area Zoning Data
https://github.com/OtheringBelonging/BayAreaZoning