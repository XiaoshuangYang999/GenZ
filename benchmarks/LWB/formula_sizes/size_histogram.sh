# This script was assisted by ChatGPT.
awk -F, '
BEGIN {
  bin_width = 100
}
NR > 1 {
  size = $4
  if (size ~ /^[0-9]+$/) {
    bin = int(size / bin_width) * bin_width
    count[bin]++
    if (bin > max_bin) max_bin = bin
  }
}
END {
  print "size,count"
  for (i = 0; i <= max_bin; i += bin_width) {
    c = (i in count) ? count[i] : 0
    print i "," c
  }
}
' formula_sizes.csv > size_histogram.csv
