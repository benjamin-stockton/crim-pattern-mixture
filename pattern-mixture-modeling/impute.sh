#!/bin/bash 
#SBATCH --partition=general
#SBATCH --constraint='epyc128'
#SBATCH --cpus-per-task=125
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=benjamin.stockton@uconn.edu

source /etc/profile.d/modules.sh
module purge
module load r/4.3.2

time Rscript full-data-imputations.R

