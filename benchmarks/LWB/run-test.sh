#!/bin/bash
#SBATCH --job-name=test-lwb
#SBATCH --time=12:00:00
#SBATCH --partition=genoa
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --output=../slurm_logs/%A_%a.out
#SBATCH --error=../slurm_logs/%A_%a.err

set -euo pipefail

LOG_DIR="../slurm_logs"
mkdir -p "$LOG_DIR"


bash ./test.sh
echo "Finished"
