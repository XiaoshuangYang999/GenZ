#!/usr/bin/env bash
# This script measures runtime and proof size for a few selected benchmarks.
# It tests both zip and tree provers and writes a plain text summary file.

TIMEOUT=14400    # seconds, change if you want
OUT_FILE="special_time_size.txt"

# Benchmarks to test: GROUP LOGIC FORMULA INDEX
BENCHES=(
  # "k_p D  k_ph_p 2"
  # "k_p D  k_ph_p 3"
  "k_p D4 k_ph_p 2"
  "k_p D4 k_ph_p 3"
  # "k_p D45 k_dum_p 1"
  # "k_p D45 k_ph_p 2"
  # "k_n D45 k_d4_n 1"
)

echo "# group logic prover formula index size runtime result proofsize" > "$OUT_FILE"

for bench in "${BENCHES[@]}"; do
  # split the fields
  read -r GROUP LOGIC FORMULA IDX <<< "$bench"

  file="${GROUP}/${FORMULA}.txt.${IDX}.intohylo"

  if [ ! -f "$file" ]; then
    echo "# WARNING: file not found: $file" >> "$OUT_FILE"
    continue
  fi

  # compute formula size
  size=$(printf '%s\n' "$file" | stack exec form-size -- 2>/dev/null | tail -n 1)

  for prover in tree; do
    if [ "$prover" = "zip" ]; then
      GENZ_MODE=""
    else
      GENZ_MODE="-t"
    fi

    out_log=$(mktemp)
    time_log=$(mktemp)

    echo "Running: group=$GROUP logic=$LOGIC prover=$prover formula=$FORMULA n=$IDX"

    # One run that both finds the proof and prints its size
    /usr/bin/time -p timeout "${TIMEOUT}s" \
      genz $GENZ_MODE -n -d -l "$LOGIC" -f "$file" -p size \
      >"$out_log" 2>"$time_log"
    status=$?

    runtime=$(awk '/^real / {print $2}' "$time_log")
    res="-"
    proofsize="-"

    if [ "$status" -eq 124 ]; then
      res="Timeout"
    elif [ "$status" -ne 0 ]; then
      res="Error"
    else
      res="OK"
      # try to read a numeric proof size from output (last numeric line)
      proofsize=$(grep -E '^[0-9]+$' "$out_log" | tail -n 1)
      # fallback: last line if no purely numeric line
      if [ -z "$proofsize" ]; then
        proofsize=$(tail -n 1 "$out_log")
      fi
    fi

    echo "$GROUP $LOGIC $prover $FORMULA $IDX $size $runtime $res $proofsize" >> "$OUT_FILE"

    rm -f "$out_log" "$time_log"
  done
done

echo "Results written to $OUT_FILE"
