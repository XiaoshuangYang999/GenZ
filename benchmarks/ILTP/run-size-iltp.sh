#!/usr/bin/env bash
# This script was assisted by ChatGPT.

ALL_GROUPS="theorem non_theorem unknown"

OUT_CSV="iltp_sizes.csv"
echo "group,filename,size" > "$OUT_CSV"

for group in $ALL_GROUPS; do

  for path in "$group"/*.tptp; do
    filename=$(basename "$path")
    base=${filename%.tptp}
    size=$(printf '%s\n' "$path" | tptp-size | tail -n 1)
    echo "$group,$base,$size" >> "$OUT_CSV"
  done
done

echo "Size data written to $OUT_CSV"
