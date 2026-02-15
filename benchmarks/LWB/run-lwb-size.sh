#!/usr/bin/env bash
# This script was assisted by ChatGPT

GROUP="$1"
LOGIC="$2"
TIMEOUT="$3"
SIZE_LIMIT="$4"

# range of index n
START_N=1
LAST_N=21

group_formulas() {
  case "$1" in
    k_p)  echo "k_branch_p k_d4_p k_dum_p k_grz_p k_lin_p k_path_p k_ph_p k_poly_p k_t4p_p" ;;
    k_n)  echo "k_branch_n k_d4_n k_dum_n k_grz_n k_lin_n k_path_n k_ph_n k_poly_n k_t4p_n" ;;
    s4_p) echo "s4_45_p s4_branch_p s4_grz_p s4_ipc_p s4_md_p s4_path_p s4_ph_p s4_s5_p s4_t4p_p" ;;
    s4_n) echo "s4_45_n s4_branch_n s4_grz_n s4_ipc_n s4_md_n s4_path_n s4_ph_n s4_s5_n s4_t4p_n" ;;
    *)    echo "" ;;
  esac
}

FORMULAS="$(group_formulas "$GROUP")"
if [ -z "$FORMULAS" ]; then
  echo "Unknown group: $GROUP"
  exit 1
fi

# input filepath
mk_input_file() {
  local formula="$1" n="$2"
  echo "${GROUP}/${formula}.txt.${n}.intohylo"
}

# output logs path
mk_out_log() {
  local prover="$1" formula="$2" n="$3"
  echo "logs/${TIMEOUT}s/${GROUP}/${LOGIC}/${prover}.${formula}.txt.${n}.log"
}
mk_time_log() {
  local prover="$1" formula="$2" n="$3"
  echo "logs/${TIMEOUT}s/${GROUP}/${LOGIC}/${prover}.${formula}.txt.${n}.time"
}

# results file
SIZE_RAW="${LOGIC}_${GROUP}_${SIZE_LIMIT}_${TIMEOUT}s.csv"

echo "group,logic,prover,formula,index,size,runtime,res,proofsize" > "$SIZE_RAW"

echo "GROUP      = $GROUP"
echo "LOGIC      = $LOGIC"
echo "TIMEOUT    = ${TIMEOUT}s"
echo "SIZE_LIMIT = $SIZE_LIMIT"
echo "FORMULAS   = $FORMULAS"

for prover in zip tree; do
  echo "=== Prover: $prover ==="

  if [ "$prover" = "zip" ]; then
    GENZ_MODE=""
  else
    GENZ_MODE="-t"
  fi

  for formula in $FORMULAS; do
    echo "  Formula family: $formula"

    for n in $(seq "$START_N" "$LAST_N"); do
      file="$(mk_input_file "$formula" "$n")"
      out_log="$(mk_out_log "$prover" "$formula" "$n")"
      time_log="$(mk_time_log "$prover" "$formula" "$n")"

      size_n=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
      if [ "$size_n" -gt "$SIZE_LIMIT" ]; then
        echo "      skip n=$n (size $size_n > $SIZE_LIMIT)"
        break
      fi

      mkdir -p "$(dirname "$out_log")"

      echo "      $prover: logic=$LOGIC, formula=$formula, n=$n, size=$size_n, file=$file"

      /usr/bin/time -p timeout ${TIMEOUT}s genz $GENZ_MODE -d -n -l "$LOGIC" -f "$file" > "$out_log" 2> "$time_log"
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
          proofsize=$(timeout 600s genz $GENZ_MODE -d -n -l "$LOGIC" -f "$file" -p size 2>/dev/null | tail -n 1)
        fi
      fi

      if [ "$res" = "True" ]; then
        echo "$GROUP,$LOGIC,$prover,$formula,$n,$size_n,$runtime,$res,$proofsize" >> "$SIZE_RAW"
      fi
    done
  done
done
