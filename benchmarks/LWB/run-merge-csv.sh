#!/usr/bin/env bash

out="prov_200_300s.csv"

logics=(K K4 K45 D D4 D45 T S4 GL)
families=(k_p k_n s4_p s4_n)

first=1

{
  for L in "${logics[@]}"; do
    for F in "${families[@]}"; do
      f="${L}_${F}_200_300s.csv"
      [ -f "$f" ] || continue
      # remove header except for the first file
      if (( first )); then
        cat "$f"
        first=0
      else
        tail -n +2 "$f"
      fi
    done
  done
} > "$out"
echo "Merged results into $out"
