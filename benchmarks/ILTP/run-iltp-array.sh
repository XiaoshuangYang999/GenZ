#!/bin/bash
#SBATCH --job-name=ILTP
#SBATCH --time=5:00:00
#SBATCH --partition=genoa
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --array=0-11
#SBATCH --output=../slurm_logs/%A_%a.out
#SBATCH --error=../slurm_logs/%A_%a.err

set -euo pipefail

LOG_DIR="../slurm_logs"
mkdir -p "$LOG_DIR"

TIMEOUT=300      # seconds
SIZE_LIMIT=200

logics=(CPL IPL)
groups=(theorem non_theorem unknown)
provers=(zip tree)

num_logics=${#logics[@]}
num_groups=${#groups[@]}
num_provers=${#provers[@]}
total=$((num_logics * num_groups * num_provers))

if (( SLURM_ARRAY_TASK_ID < 0 || SLURM_ARRAY_TASK_ID >= total )); then
    echo "Invalid SLURM_ARRAY_TASK_ID=${SLURM_ARRAY_TASK_ID}, total combinations=${total}" >&2
    exit 1
fi

# Map 1D index -> (logic_idx, group_idx, prover_idx)
logic_idx=$(( SLURM_ARRAY_TASK_ID / (num_groups * num_provers) ))
rest=$(( SLURM_ARRAY_TASK_ID % (num_groups * num_provers) ))
group_idx=$(( rest / num_provers ))
prover_idx=$(( rest % num_provers ))

logic=${logics[$logic_idx]}
group=${groups[$group_idx]}
prover=${provers[$prover_idx]}

echo "[$(date)] Task $SLURM_ARRAY_TASK_ID on $(hostname)"
echo "  logic   = $logic"
echo "  group   = $group"
echo "  prover  = $prover"
echo "  TIMEOUT = ${TIMEOUT}s"
echo "  SIZE_LIMIT = ${SIZE_LIMIT}"

bash ./run-iltp.sh "$group" "$logic" "$TIMEOUT" "$SIZE_LIMIT" "$prover"
