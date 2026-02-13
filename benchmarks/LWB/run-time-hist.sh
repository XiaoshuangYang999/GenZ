#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./runtime_hist_zip_tree.sh data.csv > hist.csv
#
# Expected CSV header (column order is important):
#   group,logic,prover,formula,index,size,runtime,res
#   1     2     3      4       5     6    7       8
#
# Bins (in seconds) are defined below as:
#   [0.01,0.1), [0.1,1), [1,10), [10,100), [100,300), [300,400)
#
# Columns in the output CSV:
#   from,to,zip_all,tree_all,zip_pos,tree_pos,zip_neg,tree_neg

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 data.csv" >&2
  exit 1
fi

FILE="$1"

if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE" >&2
  exit 1
fi

awk -F',' '
BEGIN {
  # Define bin edges (in seconds). You can change these if needed.
  edges[1] = 0.01
  edges[2] = 0.1
  edges[3] = 1
  edges[4] = 10
  edges[5] = 100
  edges[6] = 300
  edges[7] = 400
  nEdges   = 7          # edges[1..7] => 6 bins: [1..6]

  # Nothing else to init; AWK arrays default to 0.
}

NR == 1 { next }        # skip header

{
  prov    = $3
  runtime = $7 + 0
  r       = tolower($8)

  # Find bin index: from edges[i] (inclusive) to edges[i+1] (exclusive).
  bin = -1
  for (i = 1; i < nEdges; i++) {
    if (runtime >= edges[i] && runtime < edges[i+1]) {
      bin = i
      break
    }
  }

  # If runtime is out of all bins, ignore it.
  if (bin == -1) next

  # Count "all" results for this prover & bin
  count[prov, "all", bin]++

  # Count pos / neg
  if (r == "true") {
    count[prov, "pos", bin]++
  } else if (r == "false") {
    count[prov, "neg", bin]++
  }
}

END {
  # Output header
  print "from,to,zip_all,tree_all,zip_pos,tree_pos,zip_neg,tree_neg"

  # Provers in fixed order
  provList[1] = "zip"
  provList[2] = "tree"

  for (b = 1; b < nEdges; b++) {
    from = edges[b]
    to   = edges[b+1]

    # helper to fetch count or 0
    za = (( "zip",  "all", b) in count ? count["zip", "all", b] : 0)
    ta = (( "tree", "all", b) in count ? count["tree","all", b] : 0)
    zp = (( "zip",  "pos", b) in count ? count["zip", "pos", b] : 0)
    tp = (( "tree", "pos", b) in count ? count["tree","pos", b] : 0)
    zn = (( "zip",  "neg", b) in count ? count["zip", "neg", b] : 0)
    tn = (( "tree", "neg", b) in count ? count["tree","neg", b] : 0)

    printf "%.2f,%.2f,%d,%d,%d,%d,%d,%d\n",
           from, to, za, ta, zp, tp, zn, tn
  }
}
' "$FILE"
