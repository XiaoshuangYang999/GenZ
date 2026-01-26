#! /bin/bash

# TODO: check that genz is actually on the PATH
for logic in D D4 K4 T; do
    echo "Logic: $logic"

    echo "Now running zipper benchmarks."
    dirZip="${logic}/zipper"
    mkdir -p "$dirZip"

    echo "Now running provable benchmarks."
    for basepath in lwb_k/*_p.txt.1.intohylo; do
        base=$(basename "$basepath" _p.txt.1.intohylo)
        echo "${base}_p"
        for n in $(seq 1 10); do
            file="lwb_k/${base}_p.txt.${n}.intohylo"
            # echo "$n"
            out_log="${dirZip}/${base}_p.txt.${n}.intohylo_output.log"
            time_log="${dirZip}/${base}_p.txt.${n}.intohylo_time.log"
            time (timeout 300s genz -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
            status=$?
            if [ "$status" -eq 124 ]; then
                size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
                echo "timeout at n=${n}, size $size"
                break
            fi
        done
    done

    echo "Now running unprovable benchmarks."
    for basepath in lwb_s4/*_n.txt.1.intohylo; do
        base=$(basename "$basepath" _n.txt.1.intohylo)
        echo "${base}_n"
        for n in $(seq 1 10); do
            file="lwb_s4/${base}_n.txt.${n}.intohylo"
            # echo "$n"
            out_log="${dirZip}/${base}_n.txt.${n}.intohylo_output.log"
            time_log="${dirZip}/${base}_n.txt.${n}.intohylo_time.log"
            time (timeout 300s genz -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
            status=$?
            if [ "$status" -eq 124 ]; then
                size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
                echo "timeout at n=${n}, size $size"
                break
            fi
        done
    done

    echo "Now running tree benchmarks."
    dirTree="${logic}/tree"
    mkdir -p "$dirTree"

    echo "Now running provable benchmarks."
    for basepath in lwb_k/*_p.txt.1.intohylo; do
        base=$(basename "$basepath" _p.txt.1.intohylo)
        echo "${base}_p"
        for n in $(seq 1 10); do
            file="lwb_k/${base}_p.txt.${n}.intohylo"
            # echo "$n"
            out_log="${dirTree}/${base}_p.txt.${n}.intohylo_output.log"
            time_log="${dirTree}/${base}_p.txt.${n}.intohylo_time.log"
            time (timeout 300s genz -t -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
            status=$?
            if [ "$status" -eq 124 ]; then
                size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
                echo "timeout at n=${n}, size $size"
                break
            fi
        done
    done

    echo "Now running unprovable benchmarks."
    for basepath in lwb_s4/*_n.txt.1.intohylo; do
        base=$(basename "$basepath" _n.txt.1.intohylo)
        echo "${base}_n"
        for n in $(seq 1 10); do
            file="lwb_s4/${base}_n.txt.${n}.intohylo"
            # echo "$n"
            out_log="${dirTree}/${base}_n.txt.${n}.intohylo_output.log"
            time_log="${dirTree}/${base}_n.txt.${n}.intohylo_time.log"
            time (timeout 300s genz -t -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
            status=$?
            if [ "$status" -eq 124 ]; then
                size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
                echo "timeout at n=${n}, size $size"
                break
            fi
        done
    done
    echo "$logic Done"
done

for logic in GL, K45, D45; do
    echo "Logic: $logic"
    echo "Now running zipper benchmarks."
    dirZip="${logic}/zipper"
    mkdir -p "$dirZip"

    for basepath in lwb_k/*_p.txt.1.intohylo; do
        base=$(basename "$basepath" _p.txt.1.intohylo)
        echo "${base}_p"
        for n in $(seq 1 10); do
            file="lwb_k/${base}_p.txt.${n}.intohylo"
            # echo "$n"
            out_log="${dirZip}/${base}_p.txt.${n}.intohylo_output.log"
            time_log="${dirZip}/${base}_p.txt.${n}.intohylo_time.log"
            time (timeout 300s genz -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
            status=$?
            if [ "$status" -eq 124 ]; then
                size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
                echo "timeout at n=${n}, size $size"
                break
            fi
        done
    done
    
    echo "Now running tree benchmarks."
    dirTree="${logic}/tree"
    mkdir -p "$dirTree"
    for basepath in lwb_k/*_p.txt.1.intohylo; do
        base=$(basename "$basepath" _p.txt.1.intohylo)
        echo "${base}_p"
        for n in $(seq 1 10); do
            file="lwb_k/${base}_p.txt.${n}.intohylo"
            # echo "$n"
            out_log="${dirTree}/${base}_p.txt.${n}.intohylo_output.log"
            time_log="${dirTree}/${base}_p.txt.${n}.intohylo_time.log"
            time (timeout 300s genz -t -d -n -l "$logic" -f "$file" > "$out_log") &> "$time_log"
            status=$?
            if [ "$status" -eq 124 ]; then
                size=$(printf '%s\n' "$file" | stack exec form-size -- | tail -n 1)
                echo "timeout at n=${n}, size $size"
                break
            fi
        done
    done
    echo "$logic Done"
done
echo "All Done"