#!/bin/bash
#SBATCH --job-name=GenZ
#SBATCH --time 25:00:00
#SBATCH --partition=rome
#SBATCH --output=../slurm_logs/%j.out
#SBATCH --error=../slurm_logs/%j.err

LOG_DIR="../slurm_logs"
mkdir -p "$LOG_DIR"

chmond +x run-all-form-all.sh
./run-all-form-all.sh
