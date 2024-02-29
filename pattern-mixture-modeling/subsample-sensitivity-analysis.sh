#!/bin/bash
#SBATCH --partition=general                   # Name of Partition
#SBATCH --ntasks=5                           # Maximum CPU cores for job
#SBATCH --nodes=1                             # Ensure all cores are from the same node
#SBATCH --constraint='skylake'                # Target the Skylake node architecture
#SBATCH --mem=32G                            # Request 32GB of available RAM on a Skylake node
#SBATCH --mail-type=ALL                       # Event(s) that triggers email notification (BEGIN,END,FAIL,ALL)
#SBATCH --mail-user=benjamin.stockton@uconn.edu      # Destination email address

Rscript subsample-sensitivity-analysis.r