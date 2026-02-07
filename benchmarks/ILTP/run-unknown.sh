#!/usr/bin/env bash
# This script was assisted by ChatGPT.

GROUP=unknown
TIMEOUT=600
SIZELIMIT=500

OUT_CSV="${SIZELIMIT}_${TIMEOUT}s_${GROUP}.csv"

# output logs path
mk_out_log() {
  local base="$1" prover="$2"
  echo "Logs/${SIZELIMIT}/${TIMEOUT}s/${GROUP}/${prover}.${base}.log"
}
mk_time_log() {
  local base="$1" prover="$2"
  echo "Logs/${SIZELIMIT}/${TIMEOUT}s/${GROUP}/${prover}.${base}.time"
}

echo "group,prover,file,size,runtime,res" > "$OUT_CSV"

for path in "${GROUP}"/*.tptp; do
    filename=$(basename "$path")
    base=${filename%.tptp}

    size=$(printf '%s\n' "$path" | tptp-size | tail -n 1)
    if [ "$size" -gt "$SIZELIMIT" ]; then
        echo "      skip ${base} (size $size > $SIZELIMIT)"
        continue
    fi

    for prover in zip tree; do
        if [ "$prover" = "zip" ]; then
          GENZ_MODE=""
        else
          GENZ_MODE="-t"
        fi

        out_log="$(mk_out_log "$base" "$prover")"
        time_log="$(mk_time_log "$base" "$prover")"

        mkdir -p "$(dirname "$out_log")"

        echo "      running genz $prover: file=$base, size=$size"

        /usr/bin/time -p timeout ${TIMEOUT}s genz $GENZ_MODE -d -l IPL -i tptp -f "$path" > "$out_log" 2> "$time_log"
        status=$?

        runtime=$(awk '/^real / {print $2}' "$time_log")

        res="-"
        if [ "$status" -eq 124 ]; then
            res="Timeout"
            runtime="$TIMEOUT"
        else
            if grep -q "True" "$out_log"; then
                res="True"
            elif grep -q "False" "$out_log"; then
                res="False"
            fi
        fi
        echo "$GROUP,$prover,$base,$size,$runtime,$res" >> "$OUT_CSV"
    done
done


echo "Size data written to $OUT_CSV"
