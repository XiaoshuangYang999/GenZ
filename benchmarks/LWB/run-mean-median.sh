#!/usr/bin/env bash

#   ./runtime_stats.sh data.csv
#
# CSV : size,runtime,cat
# cat ： zip / tree

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 data.csv"
  exit 1
fi

FILE="$1"

if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE"
  exit 1
fi

stats_for_cat() {
  local cat="$1"
  local file="$2"

  # sum and count for mean
  read -r sum count < <(
    awk -F, -v cat="$cat" '
      NR>1 && $3==cat { sum += $2; count++ }
      END { if (count>0) printf "%.8f %d\n", sum, count; }
    ' "$file"
  )

  if [[ -z "${count:-}" || "$count" -eq 0 ]]; then
    echo "No rows for cat = $cat"
    return
  fi

  # collect all runtime values for median
  mapfile -t vals < <(
    awk -F, -v cat="$cat" 'NR>1 && $3==cat {print $2}' "$file" \
      | LC_ALL=C sort -n
  )

  local n="${#vals[@]}"
  if [[ "$n" -ne "$count" ]]; then
    echo "Warning: count mismatch for cat = $cat" >&2
  fi

  # mean = sum / count
  local mean
  mean=$(awk -v s="$sum" -v c="$count" 'BEGIN { printf "%.4f", s/c }')

  # median
  local median
  if (( n % 2 == 1 )); then
    # odd
    median="${vals[$((n/2))]}"
  else
    # even
    local v1="${vals[$((n/2 - 1))]}"
    local v2="${vals[$((n/2))]}"
    median=$(awk -v a="$v1" -v b="$v2" 'BEGIN { printf "%.4f", (a+b)/2 }')
  fi

  printf "cat=%s  n=%d  mean(average)=%.4f  median=%.4f\n" \
    "$cat" "$count" "$mean" "$median"
}

stats_for_cat "zip"  "$FILE"
stats_for_cat "tree" "$FILE"
