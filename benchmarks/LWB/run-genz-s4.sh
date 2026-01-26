#! /bin/bash

# TODO: check that genz is actually on the PATH

mkdir -p s4logsZip
mkdir -p s4logsTree

echo "Now running zipper benchmarks."
for basepath in $(ls -Sr lwb_s4/*.txt.1.intohylo); do
    base=$(basename "$basepath" .txt.1.intohylo)
    echo "Formula family : $base"
    for n in $(seq 1 10); do
        file="lwb_s4/${base}.txt.${n}.intohylo"
        echo $n
        out_log="s4logsZip/${base}.txt.${n}.intohylo_output.log"
        time_log="s4logsZip/${base}.txt.${n}.intohylo_time.log"
        time (timeout 300s genz -d -n -l S4 -f "$file" > "$out_log") &> "$time_log"
        status=$?
        if [ "$status" -eq 124 ]; then
            echo "  timeout at n=${n} for ${base}, skip remaining n."
            size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
            echo "  size of formula: $size."
            break
        fi
    done
done

echo "Now running tree benchmarks."
for basepath in $(ls -Sr lwb_s4/*.txt.1.intohylo); do
    base=$(basename "$basepath" .txt.1.intohylo)
    echo "Formula family : $base"
    for n in $(seq 1 10); do
        file="lwb_s4/${base}.txt.${n}.intohylo"
        echo $n
        out_log="s4logsTree/${base}.txt.${n}.intohylo_output.log"
        time_log="s4logsTree/${base}.txt.${n}.intohylo_time.log"
        time (timeout 300s genz -t -d -n -l S4 -f "$file" > "$out_log") &> "$time_log"
        status=$?
        if [ "$status" -eq 124 ]; then
            echo "  timeout at n=${n} for ${base}, skip remaining n."
            size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
            echo "  size of formula: $size."
            break
        fi
    done
done
