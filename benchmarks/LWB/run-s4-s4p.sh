#!/usr/bin/env bash
# This script was assisted by ChatGPT

# group names
ALL_GROUPS="s4_p"

# logics to run for each group
group_logics() {
  case "$1" in
    # k_p) echo $LOGICS_ALL ;;
    # k_n) echo $LOGICS_ALL ;;
    s4_p) echo $LOGICS_ALL ;;
    # s4_n) echo $LOGICS_ALL ;;
    *) echo "" ;;
  esac
}

# bench families for each group
group_formulas() {
  case "$1" in
    # k_p)  echo "k_branch_p k_d4_p k_dum_p k_grz_p k_lin_p k_path_p k_ph_p k_poly_p k_t4p_p" ;;
    # k_n)  echo "k_branch_n k_d4_n k_dum_n k_grz_n k_lin_n k_path_n k_ph_n k_poly_n k_t4p_n" ;;
    s4_p) echo "s4_45_p s4_branch_p s4_grz_p s4_ipc_p s4_md_p s4_path_p s4_ph_p s4_s5_p s4_t4p_p" ;;
    # s4_n) echo "s4_45_n s4_branch_n s4_grz_n s4_ipc_n s4_md_n s4_path_n s4_ph_n s4_s5_n s4_t4p_n" ;;
    *)    echo "" ;;
  esac
}

# all logics
LOGICS_ALL="S4"

# range of index n
START_N=1
LAST_N=21

# time limit
TIMEOUT=600

# size limit
SIZE_LIMIT=300

# input filepath
mk_input_file() {
  local group="$1" formula="$2" n="$3"
  echo "${group}/${formula}.txt.${n}.intohylo"
}

# output logs path
mk_out_log() {
  local group="$1" logic="$2" prover="$3" formula="$4" n="$5"
  echo "logs/${TIMEOUT}s/${group}/${logic}/${prover}.${formula}.txt.${n}.log"
}
mk_time_log() {
  local group="$1" logic="$2" prover="$3" formula="$4" n="$5"
  echo "logs/${TIMEOUT}s/${group}/${logic}/${prover}.${formula}.txt.${n}.time"
}

# results files for size-limited runs
SIZE_RAW="S4_s4p_${SIZE_LIMIT}_${TIMEOUT}s_raw.csv"

# echo "group,logic,prover,formula,index,size,runtime,res" > "$SIZE_RAW"

# Loop：group -> logic -> prover -> formula -> index
for group in $ALL_GROUPS; do
  echo "===== Group $group ====="

  LOGICS="$(group_logics "$group")"
  FORMULAS="$(group_formulas "$group")"

  echo "  LOGICS: $LOGICS"
  echo "  FORMULAS: $FORMULAS"

  for logic in $LOGICS; do
    echo "Logic: $logic (group $group)"

    for prover in zip tree; do
      echo "  Now running ${prover} benchmarks."

      if [ "$prover" = "zip" ]; then
        GENZ_MODE=""
      else
        GENZ_MODE="-t"
      fi

      for formula in $FORMULAS; do
        echo "    $formula ($prover)"

        for n in $(seq "$START_N" "$LAST_N"); do
          file="$(mk_input_file "$group" "$formula" "$n")"
          out_log="$(mk_out_log "$group" "$logic" "$prover" "$formula" "$n")"
          time_log="$(mk_time_log "$group" "$logic" "$prover" "$formula" "$n")"

          size_n=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
          if [ "$size_n" -gt "$SIZE_LIMIT" ]; then
            echo "      skip n=$n (size $size_n > $SIZE_LIMIT)"
            break
          fi

          mkdir -p "$(dirname "$out_log")"

          echo "      running genz $prover: logic=$logic, formula=$formula, n=$n, size=$size_n, file=$file"

          /usr/bin/time -p timeout ${TIMEOUT}s genz $GENZ_MODE -d -n -l "$logic" -f "$file" > "$out_log" 2> "$time_log"
          status=$?
          runtime=$(awk '/^real / {print $2}' "$time_log")


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

          echo "$group,$logic,$prover,$formula,$n,$size_n,$runtime,$res" >> "$SIZE_RAW"
        done
      done
    done
  done
done
