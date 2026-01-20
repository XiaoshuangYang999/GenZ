#! /bin/bash

# TODO: check that genz is actually on the PATH

mkdir -p logs
mkdir -p treelogs

echo "Now running zipper benchmarks."
for basepath in $(ls -Sr lwb_k/*.txt.1.intohylo); do
    base=$(basename "$basepath" .txt.1.intohylo)
    echo "Formula family : $base"
    for n in $(seq 1 10); do
        file="lwb_k/${base}.txt.${n}.intohylo"
        echo $n
        out_log="logs/${base}.txt.${n}.intohylo_output.log"
        time_log="logs/${base}.txt.${n}.intohylo_time.log"
        time (timeout 300s genz -d -n -l K -f "$file" > "$out_log") &> "$time_log"
        status=$?
        if [ "$status" -eq 124 ]; then
            echo "  timeout at n=${n} for ${base}, skip remaining n."
            break
        fi
    done
done

echo "Now running tree benchmarks."
for basepath in $(ls -Sr lwb_k/*.txt.1.intohylo); do
    base=$(basename "$basepath" .txt.1.intohylo)
    echo "Formula family : $base"
    for n in $(seq 1 10); do
        file="lwb_k/${base}.txt.${n}.intohylo"
        echo $n
        out_log="treelogs/${base}.txt.${n}.intohylo_output.log"
        time_log="treelogs/${base}.txt.${n}.intohylo_time.log"
        time (timeout 300s genz -t -d -n -l K -f "$file" > "$out_log") &> "$time_log"
        status=$?
        if [ "$status" -eq 124 ]; then
            echo "  timeout at n=${n} for ${base}, skip remaining n."
            break
        fi
    done
done
