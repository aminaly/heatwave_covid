#!/bin/bash
#SBATCH --job-name=extractTemps
#SBATCH --nodes=1
#SBATCH --array=1-86%10
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.out
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=100GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml system math devel sqlite gcc
ml physics proj geos gdal udunits curl netcdf R/4.1.2;
ml gcc/9.1.0

cd $OAK/group_members/aminaly/heatwave_covid
let buffer=$SLURM_ARRAY_TASK_ID
Rscript ./extract_temp.R $buffer
