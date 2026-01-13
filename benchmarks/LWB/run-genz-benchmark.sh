#! /bin/bash

for i in `ls lwb_k -Sr`; do
    echo $i
    time (timeout 60s genz -n -l K -f lwb_k/$i > logs/${i}_output.log) &> logs/${i}_time.log
done
