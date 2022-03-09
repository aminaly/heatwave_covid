#!/bin/bash
#SBATCH --job-name=extractTemps
#SBATCH --nodes=1
#SBATCH --array=1:86
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.out
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=100GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml system math devel sqlite/3.18.0 gcc/6.3.0
ml physics proj/4.9.3 geos gdal/2.2.1 udunits/2.2.26 curl/7.54.0 netcdf/4.4.1.1 R/3.6.1;

cd $OAK/group_members/aminaly/heatwave_covid
let buffer=$SLURM_ARRAY_TASK_ID
Rscript ./extract_temp.R $buffer
