#!/usr/bin/env bash

# Usage:
#   ./logic_prover_stats.sh data.csv
#
# The CSV header must be:
#   group,logic,prover,formula,index,size,runtime,res
#
# Function:
#   1. For each logic, and for each prover in {zip, tree}, compute:
#        - n        (number of samples)
#        - mean     (average runtime, including timeouts)
#        - median   (median runtime, including timeouts)
#   2. Then ignore logic and compute overall statistics for all logics combined,
#      again separately for zip and tree.

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 data.csv" >&2
  exit 1
fi

FILE="$1"

if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE" >&2
  exit 1
fi

########################################
# Statistics per (logic, prover)
########################################
stats_for_logic_prover() {
  local logic="$1"
  local prov="$2"
  local file="$3"

  # Extract all runtimes for this (logic, prover), sort them,
  # then compute mean and median in awk.
  local result
  result=$(
    awk -F, -v logic="$logic" -v prov="$prov" '
      NR==1 { next }                 # skip header
      $2==logic && $3==prov {       # col 2 = logic, col 3 = prover
        print $7                    # col 7 = runtime (includes timeouts)
      }
    ' "$file" \
    | LC_ALL=C sort -n \
    | awk '
      {
        vals[NR]=$1
        sum+=$1
      }
      END {
        if (NR == 0) {
          exit 1    # no data for this (logic, prover)
        }
        n = NR
        mean = sum / n
        if (n % 2 == 1) {
          median = vals[(n+1)/2]
        } else {
          median = (vals[n/2] + vals[n/2+1]) / 2
        }
        # print: mean median n
        printf "%.6f %.6f %d\n", mean, median, n
      }
    ' || true
  )

  # If there was no data, skip printing anything.
  if [[ -z "$result" ]]; then
    return
  fi

  local mean median n
  read -r mean median n <<< "$result"

  printf "%-10s %-6s %-5d %-12.4f %-12.4f\n" \
    "$logic" "$prov" "$n" "$mean" "$median"
}

########################################
# Overall statistics per prover (all logics combined)
########################################
stats_overall_prover() {
  local prov="$1"
  local file="$2"

  # Extract all runtimes for this prover (ignoring logic),
  # sort them, then compute mean and median.
  local result
  result=$(
    awk -F, -v prov="$prov" '
      NR==1 { next }          # skip header
      $3==prov {              # col 3 = prover
        print $7              # col 7 = runtime (includes timeouts)
      }
    ' "$file" \
    | LC_ALL=C sort -n \
    | awk '
      {
        vals[NR]=$1
        sum+=$1
      }
      END {
        if (NR == 0) {
          exit 1
        }
        n = NR
        mean = sum / n
        if (n % 2 == 1) {
          median = vals[(n+1)/2]
        } else {
          median = (vals[n/2] + vals[n/2+1]) / 2
        }
        printf "%.6f %.6f %d\n", mean, median, n
      }
    ' || true
  )

  if [[ -z "$result" ]]; then
    return
  fi

  local mean median n
  read -r mean median n <<< "$result"

  # Use "ALL" in the logic column to indicate global statistics.
  printf "%-10s %-6s %-5d %-12.4f %-12.4f\n" \
    "ALL" "$prov" "$n" "$mean" "$median"
}

########################################
# Main: per-logic table + overall block
########################################

# Collect all distinct logic values (column 2).
mapfile -t LOGICS < <(
  awk -F, 'NR>1 {print $2}' "$FILE" | LC_ALL=C sort -u
)

# Header for per-logic statistics.
printf "%-10s %-6s %-5s %-12s %-12s\n" "logic" "prover" "n" "mean" "median"

# For each logic, compute stats for zip and tree.
for logic in "${LOGICS[@]}"; do
  stats_for_logic_prover "$logic" "zip"  "$FILE"
  stats_for_logic_prover "$logic" "tree" "$FILE"
done

# Empty line, then overall statistics.
echo
echo "# Overall (all logics combined)"
printf "%-10s %-6s %-5s %-12s %-12s\n" "logic" "prover" "n" "mean" "median"
stats_overall_prover "zip"  "$FILE"
stats_overall_prover "tree" "$FILE"
