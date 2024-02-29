#!/bin/bash
#SBATCH --partition=general                 # Name of Partition
#SBATCH --ntasks=124                           # Maximum CPU cores for job
#SBATCH --nodes=1                             # Ensure all cores are from the same node
#SBATCH --constraint='epyc128'                # Target the Skylake node architecture
#SBATCH --mem=32G                            # Request 32GB of available RAM on a Skylake node
#SBATCH --mail-type=ALL                       # Event(s) that triggers email notification (BEGIN,END,FAIL,ALL)
#SBATCH --mail-user=benjamin.stockton@uconn.edu      # Destination email address

cd ..

source /etc/profile.d/modules.sh
module purge
module load glpk/5.0 gmp/6.2.1 r/4.3.2

time Rscript analysis-scripts/full-data-analysis-scale-8.R

