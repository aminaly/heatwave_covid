#!/bin/bash
#SBATCH --job-name=runRegs
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/runRegs.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/runRegs.out
#SBATCH --nodes=1
#SBATCH --time=23:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=20GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

rasters_ml
module load R/3.6.1

cd $OAK/group_members/aminaly/heatwave_covid
Rscript ./regressions_new.R
