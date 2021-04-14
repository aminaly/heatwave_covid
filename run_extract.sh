#!/bin/bash
#SBATCH --job-name=extractTemps
#SBATCH --nodes=1
#SBATCH --array=1-86
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/extractTemps.out
#SBATCH --time=48:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=90GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml physics gdal udunits/2.2.26 netcdf/4.4.1.1 R/3.6.1 proj geos;
module load R/3.6.1

cd $OAK/group_members/aminaly/heatwave_covid
let buffer=$SLURM_ARRAY_TASK_ID
Rscript ./extract_temp.R $buffer
