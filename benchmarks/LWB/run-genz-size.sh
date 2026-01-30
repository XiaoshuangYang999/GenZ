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
TIMEOUT=1

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
  echo "logs/${group}/${logic}/${prover}.${formula}.txt.${n}.log"
}
mk_time_log() {
  local group="$1" logic="$2" prover="$3" formula="$4" n="$5"
  echo "logs/${group}/${logic}/${prover}.${formula}.txt.${n}.time"
}

# results files for size-limited runs
SMALL_RAW="bench_small_raw.csv"
SMALL_PERCENT="bench_small_percent.csv"

echo "group,logic,prover,formula,n,size,runtime,expected,res,correct" > "$SMALL_RAW"

# Loop：group -> logic -> prover -> formula -> n （仅跑 size <= SIZE_LIMIT 的）
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
        GENZ_MODE="-t "
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

          start=$(date +%s)
          time (timeout ${TIMEOUT}s genz "$GENZ_MODE"-d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
          status=$?
          end=$(date +%s)
          runtime=$((end - start))

          expected=""
          if [[ "$group" == *_p ]]; then
            expected="True"
          elif [[ "$group" == *_n ]]; then
            expected="False"
          fi

          if [ "$status" -eq 124 ]; then
            res="Timeout"
          else
            if grep -q "True" "$out_log"; then
              res="True"
            elif grep -q "False" "$out_log"; then
              res="False"
            fi
          fi

          correct="-"
          if [ "$res" = "$expected" ]; then
            correct="Y"
          fi

          # 只对 size <= SIZE_LIMIT 的实例记录一行
          echo "$group,$logic,$prover,$formula,$n,$size_n,$runtime,$expected,$res,$correct" >> "$SMALL_RAW"
        done
      done
    done
  done
done

# Step 2: aggregate percentage per logic, with zip/tree side by side,
# and split by provable / unprovable / overall
echo "logic,zip_total_all,zip_correct_all,zip_pct_all,zip_total_prov,zip_correct_prov,zip_pct_prov,zip_total_unprov,zip_correct_unprov,zip_pct_unprov,tree_total_all,tree_correct_all,tree_pct_all,tree_total_prov,tree_correct_prov,tree_pct_prov,tree_total_unprov,tree_correct_unprov,tree_pct_unprov" > "$SMALL_PERCENT"

awk -F, '
NR>1 {
  # SMALL_RAW: group,logic,prover,formula,n,size,runtime,expected,res,correct
  logic    = $2
  prover   = $3
  expected = $8
  corr     = $10  # correct 列 (Y/N)

  # 我们只关心 expected 为 True/False 的条目
  if (expected != "True" && expected != "False") {
    next
  }

  key = logic ":" prover

  # overall 统计
  total_all[key]++
  if (corr == "Y") {
    correct_all[key]++
  }

  # provable / unprovable 统计
  if (expected == "True") {
    total_prov[key]++
    if (corr == "Y") {
      correct_prov[key]++
    }
  } else if (expected == "False") {
    total_unprov[key]++
    if (corr == "Y") {
      correct_unprov[key]++
    }
  }

  has_logic[logic] = 1
}
END {
  for (logic in has_logic) {
    # 为了把 zip / tree 放在同一行，先算 zip，再算 tree，最后一起 printf

    # ----- zip -----
    key_zip = logic ":zip"

    tz_all = ((key_zip in total_all)     ? total_all[key_zip]     : 0)
    cz_all = ((key_zip in correct_all)   ? correct_all[key_zip]   : 0)
    pz_all = (tz_all > 0 ? 100.0 * cz_all / tz_all : 0)

    tz_prov = ((key_zip in total_prov)   ? total_prov[key_zip]    : 0)
    cz_prov = ((key_zip in correct_prov) ? correct_prov[key_zip]  : 0)
    pz_prov = (tz_prov > 0 ? 100.0 * cz_prov / tz_prov : 0)

    tz_unprov = ((key_zip in total_unprov)   ? total_unprov[key_zip]    : 0)
    cz_unprov = ((key_zip in correct_unprov) ? correct_unprov[key_zip]  : 0)
    pz_unprov = (tz_unprov > 0 ? 100.0 * cz_unprov / tz_unprov : 0)

    # ----- tree -----
    key_tree = logic ":tree"

    tt_all = ((key_tree in total_all)     ? total_all[key_tree]     : 0)
    ct_all = ((key_tree in correct_all)   ? correct_all[key_tree]   : 0)
    pt_all = (tt_all > 0 ? 100.0 * ct_all / tt_all : 0)

    tt_prov = ((key_tree in total_prov)   ? total_prov[key_tree]    : 0)
    ct_prov = ((key_tree in correct_prov) ? correct_prov[key_tree]  : 0)
    pt_prov = (tt_prov > 0 ? 100.0 * ct_prov / tt_prov : 0)

    tt_unprov = ((key_tree in total_unprov)   ? total_unprov[key_tree]    : 0)
    ct_unprov = ((key_tree in correct_unprov) ? correct_unprov[key_tree]  : 0)
    pt_unprov = (tt_unprov > 0 ? 100.0 * ct_unprov / tt_unprov : 0)

    # 输出一行：logic + zip(3×3列) + tree(3×3列)
    printf "%s,%d,%d,%.2f,%d,%d,%.2f,%d,%d,%.2f,%d,%d,%.2f,%d,%d,%.2f,%d,%d,%.2f\n",
      logic,
      tz_all,   cz_all,   pz_all,
      tz_prov,  cz_prov,  pz_prov,
      tz_unprov,cz_unprov,pz_unprov,
      tt_all,   ct_all,   pt_all,
      tt_prov,  ct_prov,  pt_prov,
      tt_unprov,ct_unprov,pt_unprov
  }
}' "$SMALL_RAW" >> "$SMALL_PERCENT"

echo "Size-limited percentages written to: $SMALL_PERCENT"
