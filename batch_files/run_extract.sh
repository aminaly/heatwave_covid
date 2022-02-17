#!/bin/bash
#SBATCH --job-name=extractTemps
#SBATCH --nodes=1
#SBATCH --array=43,86
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.out
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=100GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml curl physics gdal udunits/2.2.26 netcdf R/3.6.1 proj geos;
ml curl/7.54.0

cd $OAK/group_members/aminaly/heatwave_covid
let buffer=$SLURM_ARRAY_TASK_ID
Rscript ./extract_temp.R $buffer
