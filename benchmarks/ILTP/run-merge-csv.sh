#!/usr/bin/env bash

out="ILTP_200_300s_2.csv"

logics=(CPL IPL)
families=(theorem non_theorem unknown)
provers=(zip tree)

first=1

{
  for L in "${logics[@]}"; do
    for F in "${families[@]}"; do
      for P in "${provers[@]}"; do
        f="${F}_${L}_${P}_200_300s.csv"
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
  done
} > "$out"
echo "Merged results into $out"
