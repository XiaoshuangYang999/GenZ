#!/usr/bin/env bash
# This script was assisted by ChatGPT

# group names
ALL_GROUPS="k_p k_n s4_p s4_n"

# logics to run for each group
group_logics() {
  case "$1" in
    k_p) echo "K D D4 T K4 K45 D45 GL" ;;
    k_n) echo "K" ;;
    s4_p) echo "S4" ;;
    s4_n) echo "D D4 T K4 S4" ;;
    *) echo "" ;;
  esac
}

# bench families for each group
group_formulas() {
  case "$1" in
    k_p)  echo "k_branch_p k_d4_p k_dum_p k_grz_p k_lin_p k_path_p k_ph_p k_poly_p k_t4p_p" ;;
    k_n)  echo "k_branch_n k_d4_n k_dum_n k_grz_n k_lin_n k_path_n k_ph_n k_poly_n k_t4p_n" ;;
    s4_p) echo "s4_45_p s4_branch_p s4_grz_p s4_ipc_p s4_md_p s4_path_p s4_ph_p s4_s5_p s4_t4p_p" ;;
    s4_n) echo "s4_45_n s4_branch_n s4_grz_n s4_ipc_n s4_md_n s4_path_n s4_ph_n s4_s5_n s4_t4p_n" ;;
    *)    echo "" ;;
  esac
}

# all logics
LOGICS_ALL="K D D4 T K4 K45 D45 GL S4"

# range of index n
START_N=1
LAST_N=21

# time limit
TIMEOUT=600

# input filepath
mk_input_file() {
  local group="$1" formula="$2" n="$3"
  echo "${group}/${formula}.txt.${n}.intohylo"
}

# output logs path
mk_out_log() {
  local group="$1" logic="$2" prover="$3" formula="$4" n="$5"
  echo "logs/${group}/${logic}/${prover}.${formula}.txt.${n}.log"
}
mk_time_log() {
  local group="$1" logic="$2" prover="$3" formula="$4" n="$5"
  echo "logs/${group}/${logic}/${prover}.${formula}.txt.${n}.time"
}

# results files
RAW_RESULTS="bench_raw.csv"
TABLE_RESULTS="bench_table.csv"

echo "group,logic,prover,formula,first_timeout_n,max_solved_n,size_at_timeout,size_at_max_solved_n,corectness" > "$RAW_RESULTS"

# Loop：group -> logic -> prover -> formula
for group in $ALL_GROUPS; do
  echo "===== Group $group ====="

  LOGICS="$(group_logics "$group")"
  FORMULAS="$(group_formulas "$group")"

  if [ -z "$LOGICS" ] || [ -z "$FORMULAS" ]; then
    echo "  [WARN] group $group has empty LOGICS or FORMULAS, skipping."
    continue
  fi

  echo "  LOGICS: $LOGICS"
  echo "  FORMULAS: $FORMULAS"

  for logic in $LOGICS; do
    echo "Logic: $logic (group $group)"

    # zipper
    echo "  Now running zipper benchmarks."
    for formula in $FORMULAS; do
      echo "    $formula (zip)"

      prover="zip"
      first_timeout_n=""
      size_at_timeout=""
      max_solved_n=""
      size_at_max_solved_n=""

      for n in $(seq "$START_N" "$LAST_N"); do
        file="$(mk_input_file "$group" "$formula" "$n")"
        out_log="$(mk_out_log "$group" "$logic" "$prover" "$formula" "$n")"
        time_log="$(mk_time_log "$group" "$logic" "$prover" "$formula" "$n")"

        mkdir -p "$(dirname "$out_log")"

        echo "      running genz zip: logic=$logic, formula=$formula, n=$n, file=$file"
        time (timeout ${TIMEOUT}s genz -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
        status=$?

        if [ "$status" -eq 124 ]; then
          size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
          echo "      timeout at n=${n}, size $size"

          first_timeout_n="$n"
          size_at_timeout="$size"

          if [ "$n" -eq 1 ]; then
            max_solved_n=0
            size_at_max_solved_n="-"
          else
            max_solved_n=$((n - 1))
            prev_n=$((n - 1))
            prev_file="$(mk_input_file "$group" "$formula" "$prev_n")"
            size_prev=$(printf '%s\n' "$prev_file" | stack exec form-size -- | tail -n 1)
            size_at_max_solved_n="$size_prev"
          fi

          break
        fi
      done

      if [ -z "$first_timeout_n" ]; then
        first_timeout_n="-"
        size_at_timeout="-"
        max_solved_n="$LAST_N"
        last_file="$(mk_input_file "$group" "$formula" "$LAST_N")"
        size_last=$(printf '%s\n' "$last_file" | stack exec form-size -- | tail -n 1)
        size_at_max_solved_n="$size_last"
      fi
      # correctness（Y/N/-）: whether the results all match expectation
      # Expect: *_p → True；*_n → False
      expected=""
      if [[ "$group" == *_p ]]; then
        expected="True"
      elif [[ "$group" == *_n ]]; then
        expected="False"
      fi
      # default
      correctness="-"
      # check if expectation is set
      if [ -n "$expected" ]; then
        # start with "Y", unless set "N"
        # correctness="Y"

        # if max_solved_n = 0 (timeout at n=1）, correctness="Y"
        if [ "$max_solved_n" -gt 0 ]; then
          # check each solved index m
          correctness="Y"
          for m in $(seq "$START_N" "$max_solved_n"); do
            out_log="$(mk_out_log "$group" "$logic" "$prover" "$formula" "$m")"

            if [ ! -f "$out_log" ]; then
              # not supposed to happen
              correctness="N"
              break
            fi

            # find result from log
            if grep -q "True" "$out_log"; then
              res="True"
            elif grep -q "False" "$out_log"; then
              res="False"
            else
              # not supposed to happen
              res="Unknown"
            fi

            # label N if does not match expectation
            if [ "$res" != "$expected" ]; then
              correctness="N"
              break
            fi
          done
        fi
      fi

      echo "$group,$logic,$prover,$formula,$first_timeout_n,$max_solved_n,$size_at_timeout,$size_at_max_solved_n,$correctness" >> "$RAW_RESULTS"
    done

    # tree
    echo "  Now running tree benchmarks."
    for formula in $FORMULAS; do
      echo "    $formula (tree)"

      prover="tree"
      first_timeout_n=""
      size_at_timeout=""
      max_solved_n=""
      size_at_max_solved_n=""

      for n in $(seq "$START_N" "$LAST_N"); do
        file="$(mk_input_file "$group" "$formula" "$n")"
        out_log="$(mk_out_log "$group" "$logic" "$prover" "$formula" "$n")"
        time_log="$(mk_time_log "$group" "$logic" "$prover" "$formula" "$n")"

        mkdir -p "$(dirname "$out_log")"

        echo "      running genz tree: logic=$logic, formula=$formula, n=$n, file=$file"
        time (timeout ${TIMEOUT}s genz -t -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
        status=$?

        if [ "$status" -eq 124 ]; then
          size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
          echo "      timeout at n=${n}, size $size"

          first_timeout_n="$n"
          size_at_timeout="$size"

          if [ "$n" -eq 1 ]; then
            max_solved_n=0
            size_at_max_solved_n="-"
          else
            max_solved_n=$((n - 1))
            prev_n=$((n - 1))
            prev_file="$(mk_input_file "$group" "$formula" "$prev_n")"
            size_prev=$(printf '%s\n' "$prev_file" | stack exec form-size -- | tail -n 1)
            size_at_max_solved_n="$size_prev"
          fi

          break
        fi
      done

      if [ -z "$first_timeout_n" ]; then
        first_timeout_n="-"
        size_at_timeout="-"
        max_solved_n="$LAST_N"
        last_file="$(mk_input_file "$group" "$formula" "$LAST_N")"
        size_last=$(printf '%s\n' "$last_file" | stack exec form-size -- | tail -n 1)
        size_at_max_solved_n="$size_last"
      fi
      # correctness（Y/N/-）
      expected=""
      if [[ "$group" == *_p ]]; then
        expected="True"
      elif [[ "$group" == *_n ]]; then
        expected="False"
      fi

      correctness="-"

      if [ -n "$expected" ]; then
        # correctness="Y"
        if [ "$max_solved_n" -gt 0 ]; then
          correctness="Y"
          for m in $(seq "$START_N" "$max_solved_n"); do
            out_log="$(mk_out_log "$group" "$logic" "$prover" "$formula" "$m")"

            if [ ! -f "$out_log" ]; then
              correctness="N"
              break
            fi

            if grep -q "True" "$out_log"; then
              res="True"
            elif grep -q "False" "$out_log"; then
              res="False"
            else
              res="Unknown"
            fi

            if [ "$res" != "$expected" ]; then
              correctness="N"
              break
            fi
          done
        fi
      fi
      echo "$group,$logic,$prover,$formula,$first_timeout_n,$max_solved_n,$size_at_timeout,$size_at_max_solved_n,$correctness" >> "$RAW_RESULTS"
    done
  done
done

echo "Raw results written to: $RAW_RESULTS"

# Step 2: gnerate organized table from raw results

{
  echo -n "group,formula"
  for logic in $LOGICS_ALL; do
    echo -n ",${logic}zip,size,Y/N,${logic}tree,size,Y/N"
  done
  echo
} > "$TABLE_RESULTS"
for group in $ALL_GROUPS; do
  FORMULAS="$(group_formulas "$group")"
  [ -z "$FORMULAS" ] && continue

  for formula in $FORMULAS; do
    line="$group,$formula"

    for logic in $LOGICS_ALL; do
      # max_solved_n（RAW 第 6 列）
      zip_val=$(
        awk -F, -v G="$group" -v L="$logic" -v P="zip"  -v F="$formula" '
          NR>1 && $1==G && $2==L && $3==P && $4==F { print $6; exit }
        ' "$RAW_RESULTS"
      )
      tree_val=$(
        awk -F, -v G="$group" -v L="$logic" -v P="tree" -v F="$formula" '
          NR>1 && $1==G && $2==L && $3==P && $4==F { print $6; exit }
        ' "$RAW_RESULTS"
      )

      # size_at_max_solved_n（RAW 第 8 列）
      zip_size=$(
        awk -F, -v G="$group" -v L="$logic" -v P="zip"  -v F="$formula" '
          NR>1 && $1==G && $2==L && $3==P && $4==F { print $8; exit }
        ' "$RAW_RESULTS"
      )
      tree_size=$(
        awk -F, -v G="$group" -v L="$logic" -v P="tree" -v F="$formula" '
          NR>1 && $1==G && $2==L && $3==P && $4==F { print $8; exit }
        ' "$RAW_RESULTS"
      )

      # correctness (Y/N/-)（RAW 第 9 列）
      zip_corr=$(
        awk -F, -v G="$group" -v L="$logic" -v P="zip"  -v F="$formula" '
          NR>1 && $1==G && $2==L && $3==P && $4==F { print $9; exit }
        ' "$RAW_RESULTS"
      )
      tree_corr=$(
        awk -F, -v G="$group" -v L="$logic" -v P="tree" -v F="$formula" '
          NR>1 && $1==G && $2==L && $3==P && $4==F { print $9; exit }
        ' "$RAW_RESULTS"
      )

      # write "-" for no data
      [ -z "$zip_val" ]   && zip_val="-"
      [ -z "$tree_val" ]  && tree_val="-"
      [ -z "$zip_size" ]  && zip_size="-"
      [ -z "$tree_size" ] && tree_size="-"
      [ -z "$zip_corr" ]  && zip_corr="-"
      [ -z "$tree_corr" ] && tree_corr="-"

      # 每个 logic：zip(max_n, fSize, corr), tree(max_n, fSize, corr)
      line="$line,$zip_val,$zip_size,$zip_corr,$tree_val,$tree_size,$tree_corr"
    done

    echo "$line" >> "$TABLE_RESULTS"
  done
done


echo "Organized table written to: $TABLE_RESULTS"
