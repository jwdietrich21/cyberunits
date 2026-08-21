#!/bin/sh

#This script controls simulations with the demo programs

echo "Batch simulation test runner";

ScriptPath=$(cd `dirname $0` && pwd);

cd $ScriptPath;

echo "Please enter minimum number of iterations in a single run:";
read miniterations;
echo "Please enter maximum number of iterations in a single run:";
read maxiterations;
echo "Please enter step width:";
read steps;
echo "Please enter number of repetitions:";
read repetitions;

# C++:

echo "C++:";

let i=$miniterations;

let j=0;

while [ $i -le $maxiterations ]
do
  let r=0;
  until [ $r -ge $repetitions ]
  do
    let r=$r+1;
    response=$(./Simulator_FC0M_CPP -b -i $i);
    runtime=${response#*simulation: };
    result=${runtime% ms};
    echo $result;
    let j=$j+1;
  done;
  let i=$i+$steps;
done;

echo "Number of simulations performed" $j;
