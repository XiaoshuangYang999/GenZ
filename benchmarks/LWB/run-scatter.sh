#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 INPUT.csv OUTPUT.csv" >&2
  echo "Example: $0 results.csv all_scatter.csv" >&2
  exit 1
fi

in="$1"
out="$2"

awk -F',' -v OUT="$out" '
function trim(s) {
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s)
  return s
}

NR == 1 {
  # find prover / size / runtime
  for (i = 1; i <= NF; i++) {
    h = trim($i)
    if (h == "prover")  pcol = i
    if (h == "size")    scol = i
    if (h == "runtime") tcol = i
  }
  if (!pcol || !scol || !tcol) {
    print "Error: header must contain prover,size,runtime" > "/dev/stderr"
    exit 1
  }

  # header
  print "size,runtime,cat" > OUT
  next
}

{
  prov = tolower(trim($pcol))   # zip / tree / ...
  if (prov != "zip" && prov != "tree") next

  # check Timeout
  is_timeout = 0
  for (i = 1; i <= NF; i++) {
    field = trim($i)
    if (tolower(field) == "timeout") {
      is_timeout = 1
      break
    }
  }
  if (is_timeout) next   # remove timeout

  size = trim($scol)
  rt   = trim($tcol)
  cat  = prov   # zip / tree

  print size "," rt "," cat >> OUT
}
' "$in"
