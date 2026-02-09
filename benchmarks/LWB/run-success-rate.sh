#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 SIZE_LIMIT INPUT.csv" >&2
  echo "Example: $0 200 all_300_600s.csv" >&2
  exit 1
fi

size_limit="$1"
in="$2"

awk -F',' -v SIZE="$size_limit" '
function trim(s) {
  gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s)
  return s
}

BEGIN {
  # 用简单数组列出 prover，避免怪异语法
  provers[1] = "zip"
  provers[2] = "tree"
  nProvers   = 2
}

NR == 1 {
  # 自动识别各列
  for (i = 1; i <= NF; i++) {
    h = trim($i)
    if (h == "group")   groupCol   = i
    if (h == "logic")   logicCol   = i
    if (h == "formula") formulaCol = i
    if (h == "index")   indexCol   = i
    if (h == "prover")  provCol    = i
    if (h == "res")     resCol     = i
    if (h == "size")    sizeCol    = i
  }

  if (!groupCol || !logicCol || !formulaCol || !indexCol || !provCol || !resCol || !sizeCol) {
    print "ERROR: Missing required columns (need: group, logic, formula, index, prover, res, size)" > "/dev/stderr"
    exit 1
  }
  next
}

{
  g   = trim($groupCol)
  l   = trim($logicCol)
  f   = trim($formulaCol)
  idx = trim($indexCol)
  p   = trim($provCol)
  r   = trim($resCol)
  s   = trim($sizeCol) + 0

  # 只看 zip / tree
  if (p != "zip" && p != "tree") next

  # 只看 size <= SIZE 的公式
  if (s > SIZE) next

  logicSeen[l] = 1

  # 定义“一个公式”的 key（不含 logic/prover，只看 group+formula+index）
  globalKey = g SUBSEP f SUBSEP idx

  # 记录这个公式出现在 logic l 里
  logicFormulaSeen[l SUBSEP globalKey] = 1

  # 记录某 prover 在某 logic 对这个公式是否成功
  rl = tolower(r)
  if (rl != "timeout") {
    solved[p SUBSEP l SUBSEP globalKey] = 1
  }
}

END {
  # 输出表头：prover,logic1,logic2,...
  printf "prover"
  for (l in logicSeen) {
    printf ",%s", l
  }
  printf "\n"

  # 对 zip / tree 逐个算成功率
  for (pi = 1; pi <= nProvers; pi++) {
    p = provers[pi]
    printf "%s", p

    for (l in logicSeen) {
      totalL  = 0
      solvedL = 0

      # 遍历该 logic 下出现过的所有公式（去重后的 globalKey）
      for (k in logicFormulaSeen) {
        # k = l2 SUBSEP globalKey
        split(k, parts, SUBSEP)
        l2 = parts[1]
        if (l2 != l) continue

        # 从 k 中截出 globalKey（去掉前面的 l 和 SUBSEP）
        pos = index(k, SUBSEP)
        globalKey = substr(k, pos+1)

        totalL++

        # 如果这个 prover 在这个 logic 对该公式成功过，则 +1
        if ((p SUBSEP l SUBSEP globalKey) in solved) {
          solvedL++
        }
      }

      rate = (totalL > 0 ? 100.0 * solvedL / totalL : 0.0)
      printf ",%.2f", rate
    }

    printf "\n"
  }
}
' "$in"
