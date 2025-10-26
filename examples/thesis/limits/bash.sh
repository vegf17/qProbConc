#!/bin/bash

mkdir ./results

# echo "---Creating files"
# i=1
# while [ $i -le 7 ]
# do
#   ./rndwalk 2 $i > rndWalk2-${i}.txt
#   ((i++))
# done

# echo "---Created $((i-1)) files"


cd ../../


echo "-- rndWalk7"
./Bench "testLimits/7i" > ./thesis/limits/results/rndWalk2-7.txt --output ./thesis/limits/results/z-rndWalk2-7.html

open -a TextEdit ./thesis/limits/results/rndWalk2-7.txt

# echo "---Running benchmark"
# i=1
# while [ $i -le 7 ]
# do
#   echo "-- rndWalk${i}i"
#   ./Bench "testLimits/${i}i" > ./thesis/limits/results/rndWalk2-${i}.txt --output ./thesis/limits/results/z-rndWalk2-${i}.html
#   ((i++))
# done  
