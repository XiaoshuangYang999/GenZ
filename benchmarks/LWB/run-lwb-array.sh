#!/bin/bash
#SBATCH --job-name=LWB-bench
#SBATCH --time=5:00:00
#SBATCH --partition=genoa
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --array=0-35%15
#SBATCH --output=../slurm_logs/%A_%a.out
#SBATCH --error=../slurm_logs/%A_%a.err

set -euo pipefail

LOG_DIR="../slurm_logs"
mkdir -p "$LOG_DIR"

TIMEOUT=300      # seconds
SIZE_LIMIT=200
logics=(K K4 K45 D D4 D45 T S4 GL)
groups=(k_p k_n s4_p s4_n)

logic_idx=$(( SLURM_ARRAY_TASK_ID / ${#groups[@]} ))
group_idx=$(( SLURM_ARRAY_TASK_ID % ${#groups[@]} ))

logic=${logics[$logic_idx]}
group=${groups[$group_idx]}

echo "[$(date)] Task $SLURM_ARRAY_TASK_ID: logic=$logic group=$group on $(hostname)"
echo "TIMEOUT=${TIMEOUT}s SIZE_LIMIT=${SIZE_LIMIT}"

bash ./run-lwb-size.sh "$group" "$logic" "$TIMEOUT" "$SIZE_LIMIT"
