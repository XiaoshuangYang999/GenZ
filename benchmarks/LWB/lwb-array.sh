#!/bin/bash
#SBATCH --job-name=LWB-bench
#SBATCH --time=25:00:00
#SBATCH --partition=genoa
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --mem=0
#SBATCH --array=0-35
#SBATCH --output=../slurm_logs/%A_%a.out
#SBATCH --error=../slurm_logs/%A_%a.err

set -euo pipefail

LOG_DIR="../slurm_logs"
mkdir -p "$LOG_DIR"


logics=(k k4 k45 d d4 d45 t s4 gl)

groups=(kp kn s4p s4n)

logic_idx=$(( SLURM_ARRAY_TASK_ID / ${#groups[@]} ))
group_idx=$(( SLURM_ARRAY_TASK_ID % ${#groups[@]} ))

logic=${logics[$logic_idx]}
group=${groups[$group_idx]}

echo "[$(date)] Task $SLURM_ARRAY_TASK_ID: logic=$logic group=$group on $(hostname)"

bash "run-${logic}-${group}.sh"
