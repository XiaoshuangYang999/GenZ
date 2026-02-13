#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 INPUT.csv" >&2
  echo "Example: $0 200_300s.csv" >&2
  exit 1
fi

in="$1"
# Output file name: <input>_succ_rates.csv
out="${in%.csv}_succ_rates.csv"

awk -F',' '
function trim(s) {
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s)
  return s
}

BEGIN {
  # List provers
  provers[1] = "zip"
  provers[2] = "tree"
  nProvers   = 2

  # Fixed logic order for the output
  logicOrder[1] = "K"
  logicOrder[2] = "K4"
  logicOrder[3] = "D"
  logicOrder[4] = "D4"
  logicOrder[5] = "T"
  logicOrder[6] = "S4"
  logicOrder[7] = "GL"
  logicOrder[8] = "K45"
  logicOrder[9] = "D45"
  nLogics       = 9
}

NR == 1 {
  # Automatically detect column indices
  for (i = 1; i <= NF; i++) {
    h = trim($i)
    if (h == "group")   groupCol   = i
    if (h == "logic")   logicCol   = i
    if (h == "formula") formulaCol = i
    if (h == "index")   indexCol   = i
    if (h == "prover")  provCol    = i
    if (h == "res")     resCol     = i
  }

  if (!groupCol || !logicCol || !formulaCol || !indexCol || !provCol || !resCol) {
    print "ERROR: Missing required columns (need: group, logic, formula, index, prover, res)" > "/dev/stderr"
    exit 1
  }
  next
}

{
  g   = trim($groupCol)
  l   = trim($logicCol)
  f   = trim($formulaCol)
  idx = trim($indexCol)
  p   = trim($provCol)
  r   = trim($resCol)

  # Only consider zip / tree
  if (p != "zip" && p != "tree") next

  # Mark that this logic appears at all (not strictly needed for fixed order, but harmless)
  logicSeen[l] = 1

  # Define a global key for a formula (independent of logic/prover):
  # group + formula + index
  globalKey = g SUBSEP f SUBSEP idx

  # Mark that this formula occurs in logic l
  logicFormulaSeen[l SUBSEP globalKey] = 1

  # Record whether this prover solved this formula in this logic
  rl = tolower(r)
  if (rl != "timeout") {
    solved[p SUBSEP l SUBSEP globalKey] = 1
  }
}

END {
  # Header: prover, K, K4, D, D4, T, S4, GL, K45, D45
  printf "prover"
  for (i = 1; i <= nLogics; i++) {
    printf ",%s", logicOrder[i]
  }
  printf "\n"

  # For each prover, compute success rate per logic
  for (pi = 1; pi <= nProvers; pi++) {
    p = provers[pi]
    printf "%s", p

    for (i = 1; i <= nLogics; i++) {
      l = logicOrder[i]
      totalL  = 0
      solvedL = 0

      # Iterate over all formulas that appear in some logic
      for (k in logicFormulaSeen) {
        # k = l2 SUBSEP globalKey
        split(k, parts, SUBSEP)
        l2 = parts[1]
        if (l2 != l) continue

        # Extract globalKey part (everything after the first SUBSEP)
        pos = index(k, SUBSEP)
        globalKey = substr(k, pos+1)

        totalL++

        if ((p SUBSEP l SUBSEP globalKey) in solved) {
          solvedL++
        }
      }

      rate = (totalL > 0 ? 100.0 * solvedL / totalL : 0.0)
      printf ",%.2f", rate
    }

    printf "\n"
  }
}
' "$in" > "$out"

echo "Wrote success rates to: $out"
