#! /bin/bash

$PATH=lwb_k

for i in `ls lwb_k -Sr`; do
    time gtimeout 10s genz -l K -f lwb_k/$i #> ${i}_output.log >> ${i}_time.log
done

