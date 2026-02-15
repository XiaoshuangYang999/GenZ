#!/usr/bin/env bash
# This script was assisted by ChatGPT.

GROUP="$1"         # e.g. theorem / non_theorem / k_p / ...
LOGIC="$2"         # e.g. IPL / CPL / ...
TIMEOUT="$3"       # e.g. 300
SIZELIMIT="$4"     # e.g. 200
PROVER="$5"   # e.g. "zip" or "tree"


# Output CSV file
OUT_CSV="${GROUP}_${LOGIC}_${PROVER}_${SIZELIMIT}_${TIMEOUT}s.csv"

# Output logs path helpers
mk_out_log() {
  local base="$1" prover="$2"
  echo "Logs/${GROUP}/${LOGIC}/${PROVER}/${SIZELIMIT}/${TIMEOUT}s.${base}.log"
}
mk_time_log() {
  local base="$1" prover="$2"
  echo "Logs/${GROUP}/${LOGIC}/${PROVER}/${SIZELIMIT}/${TIMEOUT}s.${base}.time"
}

echo "group,logic,prover,file,size,runtime,res,proofsize" > "$OUT_CSV"

if [ "$PROVER" = "zip" ]; then
    GENZ_MODE=""
else
    GENZ_MODE="-t"
fi

for path in "${GROUP}"/*.tptp; do
    # Skip if glob does not match anything
    [ -e "$path" ] || continue

    filename=$(basename "$path")
    base=${filename%.tptp}

    size=$(printf '%s\n' "$path" | tptp-size | tail -n 1)
    if [ "$size" -gt "$SIZELIMIT" ]; then
        echo "      skip ${base} (size $size > $SIZELIMIT)"
        continue
    fi
    out_log="$(mk_out_log "$base" "$PROVER")"
    time_log="$(mk_time_log "$base" "$PROVER")"

    mkdir -p "$(dirname "$out_log")"

    echo "      running genz $PROVER: logic=$LOGIC, file=$base, size=$size, timeout=${TIMEOUT}s"

    /usr/bin/time -p timeout "${TIMEOUT}s" \
        genz $GENZ_MODE -d -l "$LOGIC" -i tptp -f "$path" \
        > "$out_log" 2> "$time_log"
    status=$?

    runtime=$(awk '/^real / {print $2}' "$time_log")

    res="-"
    proofsize="-"
    if [ "$status" -eq 124 ]; then
        res="Timeout"
        runtime="$TIMEOUT"
    else
        if grep -q "True" "$out_log"; then
            res="True"
            proofsize=$(timeout 1000s genz $GENZ_MODE -d -l "$LOGIC" -i tptp -f "$path" -p size 2>/dev/null | tail -n 1)
        elif grep -q "False" "$out_log"; then
            res="False"
        fi
    fi

    echo "$GROUP,$LOGIC,$PROVER,$base,$size,$runtime,$res,$proofsize" >> "$OUT_CSV"
done

echo "Size data written to $OUT_CSV"
