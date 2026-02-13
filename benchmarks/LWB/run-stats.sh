#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./logic_prover_stats_all_true_false.sh data.csv
#
# Expected CSV header (column order is important):
#   group,logic,prover,formula,index,size,runtime,res
#   1     2     3      4       5     6    7       8
#
# For each logic in the fixed order (K, K4, D, D4, T, S4, GL, K45, D45)
# and for each prover in PROVERS, this script computes, for each subset:
#   subset ∈ {all, true, false}
#   - n      : number of samples
#   - mean   : average runtime
#   - median : median runtime
#   - p75    : 75th percentile (inclusive, Excel-like)
#   - p90    : 90th percentile (inclusive, Excel-like)
#
# It also computes overall statistics across ALL logics (logic = "ALL"),
# still per prover and per subset.

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 data.csv" >&2
  exit 1
fi

FILE="$1"

if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE" >&2
  exit 1
fi

# Fixed logic order for the output
LOGIC_ORDER=(K K4 D D4 T S4 GL K45 D45)

# Provers to consider (edit if you have more)
PROVERS=(zip tree)

# Subsets of results
SUBSETS=(all true false)

########################################
# stats_for logic prover subset file
#   logic  : specific logic (e.g., "K") or "ALL" to ignore logic
#   prover : specific prover (e.g., "zip")
#   subset : "all" | "true" | "false"
########################################
stats_for() {
  local logic="$1"
  local prov="$2"
  local subset="$3"
  local file="$4"

  # First awk: filter rows by logic/prover/subset and print runtimes.
  # Then sort numerically.
  # Second awk: compute mean, median, p75, p90 from sorted runtimes.
  local result
  result=$(
    awk -F, -v logic="$logic" -v prov="$prov" -v subset="$subset" '
      NR == 1 { next }   # skip header

      {
        l  = $2
        p  = $3
        rt = $7 + 0
        r  = tolower($8)

        # Filter by logic if not ALL
        if (logic != "ALL" && l != logic) {
          next
        }

        # Filter by prover
        if (p != prov) {
          next
        }

        # Filter by subset on res
        if (subset == "true") {
          if (r != "true") next
        } else if (subset == "false") {
          if (r != "false") next
        } else if (subset == "all") {
          # keep everything
        }

        print rt
      }
    ' "$file" \
    | LC_ALL=C sort -n \
    | awk '
      # Inclusive percentile (like Excel PERCENTILE.INC):
      # expects sorted array a[1..n]
      function percentile_inc(p, n, a,   k, i, f) {
        if (n <= 0) return 0
        if (n == 1) return a[1]
        k = 1 + (n - 1) * p
        i = int(k)
        f = k - i
        if (i >= n) {
          return a[n]
        }
        return a[i] + f * (a[i+1] - a[i])
      }

      {
        vals[NR] = $1
        sum += $1
      }
      END {
        if (NR == 0) {
          exit 1    # no data for this (logic, prover, subset)
        }
        n     = NR
        mean  = sum / n

        # NR is the number of sorted values
        if (n % 2 == 1) {
          median = vals[(n+1)/2]
        } else {
          median = (vals[n/2] + vals[n/2+1]) / 2
        }

        p75 = percentile_inc(0.75, n, vals)
        p90 = percentile_inc(0.90, n, vals)

        # print: mean median p75 p90 n
        printf "%.6f %.6f %.6f %.6f %d\n", mean, median, p75, p90, n
      }
    ' || true
  )

  # If there was no data, do not output a row.
  if [[ -z "$result" ]]; then
    return
  fi

  local mean median p75 p90 n
  read -r mean median p75 p90 n <<< "$result"

  # CSV row: logic,prover,subset,n,mean,median,p75,p90
  printf "%s,%s,%s,%d,%.6f,%.6f,%.6f,%.6f\n" \
    "$logic" "$prov" "$subset" "$n" "$mean" "$median" "$p75" "$p90"
}

########################################
# Main
########################################

# CSV header
echo "logic,prover,subset,n,mean,median,p75,p90"

# Per-logic stats, in fixed logic order
for logic in "${LOGIC_ORDER[@]}"; do
  for prov in "${PROVERS[@]}"; do
    for subset in "${SUBSETS[@]}"; do
      stats_for "$logic" "$prov" "$subset" "$FILE"
    done
  done
done

# Overall (ALL logics combined), per prover and subset
for prov in "${PROVERS[@]}"; do
  for subset in "${SUBSETS[@]}"; do
    stats_for "ALL" "$prov" "$subset" "$FILE"
  done
done
