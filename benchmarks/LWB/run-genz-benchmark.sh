#! /bin/bash

# TODO: check that genz is actually on the PATH

mkdir -p logs
mkdir -p treelogs

echo "Now running zipper benchmarks."

for i in `ls lwb_k -Sr`; do
    echo $i
    time (timeout 300s genz -d -n -l K -f lwb_k/$i > logs/${i}_output.log) &> logs/${i}_time.log
done

echo "Now running tree benchmarks."

for i in `ls lwb_k -Sr`; do
    echo $i
    time (timeout 300s genz -t -d -n -l K -f lwb_k/$i > treelogs/${i}_output.log) &> treelogs/${i}_time.log
done

echo "Done."
