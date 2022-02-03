#!/bin/bash
#SBATCH --job-name=runViz2
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/runViz2.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/runViz2.out
#SBATCH --nodes=1
#SBATCH --time=23:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=100GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml physics gdal udunits/2.2.26 netcdf/4.4.1.1 R/3.6.1 proj geos;
module load R/3.6.1
ml curl/7.81.0

cd $OAK/group_members/aminaly/heatwave_covid
Rscript ./figure2.R
