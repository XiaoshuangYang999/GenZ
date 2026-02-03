#!/usr/bin/env bash
# This script was assisted by ChatGPT.

ALL_GROUPS="k_p k_n s4_p s4_n"

group_formulas() {
  case "$1" in
    k_p)  echo "k_branch_p k_d4_p k_dum_p k_grz_p k_lin_p k_path_p k_ph_p k_poly_p k_t4p_p" ;;
    k_n)  echo "k_branch_n k_d4_n k_dum_n k_grz_n k_lin_n k_path_n k_ph_n k_poly_n k_t4p_n" ;;
    s4_p) echo "s4_45_p s4_branch_p s4_grz_p s4_ipc_p s4_md_p s4_path_p s4_ph_p s4_s5_p s4_t4p_p" ;;
    s4_n) echo "s4_45_n s4_branch_n s4_grz_n s4_ipc_n s4_md_n s4_path_n s4_ph_n s4_s5_n s4_t4p_n" ;;
  esac
}

START_N=1
LAST_N=21

mk_input_file() {
  local group="$1" formula="$2" n="$3"
  echo "../${group}/${formula}.txt.${n}.intohylo"
}

OUT_CSV="formula_sizes.csv"
echo "group,formula,index,size" > "$OUT_CSV"

for group in $ALL_GROUPS; do
  formulas="$(group_formulas "$group")"
  for formula in $formulas; do
    for n in $(seq "$START_N" "$LAST_N"); do
      file=$(mk_input_file "$group" "$formula" "$n")
      size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
      echo "$group,$formula,$n,$size" >> "$OUT_CSV"
    done
  done
done

echo "Size data written to $OUT_CSV"
