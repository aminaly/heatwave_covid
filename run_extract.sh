#!/bin/bash
#SBATCH --job-name=extractTemps
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/extractTemps.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/extractTemps.out
#SBATCH --nodes=1
#SBATCH --time=23:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=12GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

rasters_ml
module load R/3.6.1

cd $OAK/group_members/aminaly/heatwave_covid
Rscript ./extract_temp.R
