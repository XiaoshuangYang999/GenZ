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
LOGICS_ALL="K,K4,D,D4,T,S4,K45,D45,GL"

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
SIZE_RAW="bench_size_raw.csv"
SIZE_PERCENT="bench_size_percent.csv"

echo "group,logic,prover,formula,index,size,runtime,expected,res,correct" > "$SIZE_RAW"

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

          # 用 time 记录带小数的运行时间，输出写到 $time_log
          /usr/bin/time -p timeout ${TIMEOUT}s genz $GENZ_MODE -d -n -l "$logic" -f "$file" > "$out_log" 2> "$time_log"
          status=$?

          # 从 time 的输出里抽取第一个字段作为运行时间（秒，可以带小数）
          # macOS 上 /usr/bin/time 默认格式是：  0.02 real   0.00 user   0.00 sys
          # 所以取第一个字段即可
                    # 从 time 的输出里抽取 real 时间（单位秒，可以带小数）
          runtime=$(awk '/^real / {print $2}' "$time_log")


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

          echo "$group,$logic,$prover,$formula,$n,$size_n,$runtime,$expected,$res,$correct" >> "$SIZE_RAW"
        done
      done
    done
  done
done

# Step 2: aggregate percentage per (logic, prover),
# with preferred logic order and zip/tree on separate rows.
echo "logic,prover,total_num,solved_num,solved_percent,_p_num,_p_solved,_p_solved_percent,_n_num,_n_solved,_n_solved_percent" > "$SIZE_PERCENT"

awk -F, '
NR>1 {
  # SIZE_RAW: group,logic,prover,formula,n,size,runtime,expected,res,correct
  logic    = $2
  prover   = $3
  expected = $8
  corr     = $10  # Y/N

  # 我们只关心 expected 为 True/False 的条目
  if (expected != "True" && expected != "False") {
    next
  }

  key = logic ":" prover

  # overall
  total_all[key]++
  if (corr == "Y") {
    solved_all[key]++
  }

  # provable (expected = True)
  if (expected == "True") {
    total_p[key]++
    if (corr == "Y") {
      solved_p[key]++
    }
  }

  # unprovable (expected = False)
  if (expected == "False") {
    total_n[key]++
    if (corr == "Y") {
      solved_n[key]++
    }
  }
}
END {
  # 固定逻辑顺序
  logic_order[1] = "K"
  logic_order[2] = "K4"
  logic_order[3] = "D"
  logic_order[4] = "D4"
  logic_order[5] = "T"
  logic_order[6] = "S4"
  logic_order[7] = "K45"
  logic_order[8] = "D45"
  logic_order[9] = "GL"

  provers[1] = "zip"
  provers[2] = "tree"

  for (i = 1; i <= 9; i++) {
    logic = logic_order[i]

    for (j = 1; j <= 2; j++) {
      prover = provers[j]
      key = logic ":" prover

      # overall
      ta = ((key in total_all)   ? total_all[key]   : 0)
      sa = ((key in solved_all)  ? solved_all[key]  : 0)
      pa = (ta > 0 ? 100.0 * sa / ta : 0)

      # provable (_p)
      tp = ((key in total_p)     ? total_p[key]     : 0)
      sp = ((key in solved_p)    ? solved_p[key]    : 0)
      pp = (tp > 0 ? 100.0 * sp / tp : 0)

      # unprovable (_n)
      tn = ((key in total_n)     ? total_n[key]     : 0)
      sn = ((key in solved_n)    ? solved_n[key]    : 0)
      pn = (tn > 0 ? 100.0 * sn / tn : 0)

      # 一行一个 (logic, prover)
      # logic,prover,total,solved,%,_p_num,_p_solved,_p_%,_n_num,_n_solved,_n_%
      printf "%s,%s,%d,%d,%.2f,%d,%d,%.2f,%d,%d,%.2f\n",
        logic, prover,
        ta, sa, pa,
        tp, sp, pp,
        tn, sn, pn
    }
  }
}' "$SIZE_RAW" >> "$SIZE_PERCENT"

echo "Size-limited percentages written to: $SIZE_PERCENT"
