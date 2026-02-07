#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 INPUT.csv" >&2
  exit 1
fi

in="$1"

awk -F',' '
function trim(s) {
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s)
  return s
}

function print_row(p,   t,th,nt,un,sth,snt,sun,s,sp) {
  if (!(p in total)) {
    return
  }
  t   = total[p] + 0
  th  = total_theorem[p] + 0
  nt  = total_non[p] + 0
  un  = total_unknown[p] + 0
  sth = solved_theorem[p] + 0
  snt = solved_non[p] + 0
  sun = solved_unknown[p] + 0
  s   = solved[p] + 0
  sp  = (t > 0 ? 100.0 * s / t : 0)

  printf "%s,%d,%d,%d,%d,%d,%d,%d,%.2f\n",
         p, t, th, nt, un, sth, snt, sun, sp
}

NR == 1 {
  for (i = 1; i <= NF; i++) {
    h = trim($i)
    if (h == "group")  groupCol = i
    if (h == "prover") provCol  = i
  }
  if (!groupCol || !provCol) {
    print "Error: CSV header must contain group, prover" > "/dev/stderr"
    exit 1
  }

  print "prover,total,theorem,non_theorem,unknown,solved_theorem,solved_non_theorem,solved_unknown,success_percentage"
  next
}

{
  g = trim($groupCol)
  p = trim($provCol)

  if (p != "zip" && p != "tree") next

  is_timeout = 0
  for (i = 1; i <= NF; i++) {
    field = trim($i)
    if (tolower(field) == "timeout") {
      is_timeout = 1
      break
    }
  }

  total[p]++

  if (g == "theorem")           total_theorem[p]++
  else if (g == "non_theorem")  total_non[p]++
  else if (g == "unknown")      total_unknown[p]++

  if (!is_timeout) {
    solved[p]++
    if (g == "theorem")           solved_theorem[p]++
    else if (g == "non_theorem")  solved_non[p]++
    else if (g == "unknown")      solved_unknown[p]++
  }
}

END {
  print_row("zip")
  print_row("tree")
}
' "$in"
