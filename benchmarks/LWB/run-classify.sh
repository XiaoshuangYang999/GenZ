#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "Usage: $0 INPUT_CSV [OUT_PREFIX]" >&2
  echo "Example: $0 results.csv classified" >&2
  exit 1
fi

in="$1"
prefix="${2:-classified}"

awk -F',' -v PREFIX="$prefix" '
function trim(s) {
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s)
  return s
}
BEGIN {
  outTrue  = PREFIX "_true.csv"
  outFalse = PREFIX "_false.csv"
  # header
  print "size,runtime,cat" > outTrue
  print "size,runtime,cat" > outFalse
}
NR==1 {
  # find prover / res / size / runtime columns
  for (i = 1; i <= NF; i++) {
    h = trim($i)
    if (h == "prover")   pcol = i
    if (h == "res")      rcol = i
    if (h == "size")     scol = i
    if (h == "runtime")  tcol = i
  }
  if (!pcol || !rcol || !scol || !tcol) {
    print "Error: missing required columns (prover,res,size,runtime) in header" > "/dev/stderr"
    exit 1
  }
  next
}
{
  prov = tolower(trim($pcol))   # zip / tree / ...
  res  = trim($rcol)
  resLower = tolower(res)

  # zip / tree
  if (prov != "zip" && prov != "tree") next
  # remove timeout
  if (resLower == "timeout") next
  # True / False
  if (res != "True" && res != "False") next

  size = trim($scol)
  rt   = trim($tcol)
  cat  = prov   # zip or tree

  if (res == "True") {
    print size "," rt "," cat >> outTrue
  } else if (res == "False") {
    print size "," rt "," cat >> outFalse
  }
}
' "$in"
