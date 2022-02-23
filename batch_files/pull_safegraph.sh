#!/bin/bash
#SBATCH --job-name=extractTemps
#SBATCH --nodes=1
#SBATCH --error=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/combine_temp.err
#SBATCH --output=/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid/outfiles/combine_temp.out
#SBATCH --time=10:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=150GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh

ml contribs
ml poldrack
ml awscli/2

cd $OAK/group_members/aminaly/heatwave_covid/heatwaves_manual/safegraph

aws s3 ls s3://sg-c19-response/neighborhood-patterns/ --profile safegraphws --endpoint https://s3.wasabisys.com
aws s3 sync s3://sg-c19-response/neighborhood-patterns/ ./ --profile safegraphws --endpoint https://s3.wasabisys.com
