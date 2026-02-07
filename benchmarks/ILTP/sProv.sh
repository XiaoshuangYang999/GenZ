#!/bin/bash
#SBATCH --job-name=GenZ-IPL-theorem
#SBATCH --time 25:00:00
#SBATCH --partition=genoa
#SBATCH --output=../slurm_logs/%j.out
#SBATCH --error=../slurm_logs/%j.err
#SBATCH --mem=0
#SBATCH --nodes=1
#SBATCH --exclusive

LOG_DIR="../slurm_logs"
mkdir -p "$LOG_DIR"


sh run-prov.sh
