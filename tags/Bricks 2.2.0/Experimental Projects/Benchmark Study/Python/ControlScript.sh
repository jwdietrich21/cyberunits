#!/bin/sh

#This script controls simulations with the demo programs

echo "Batch simulation test runner";
echo;

ScriptPath=$(cd `dirname $0` && pwd);

cd $ScriptPath;

echo "Which feedback loop is going to be analysed?";
echo "First oder linear feedback loop: L";
echo "MiMe-NoCoDI loop: M";
echo "Plase enter the letter code:";
read kind;
echo;
echo "Please enter minimum number of iterations in a single run:";
read miniterations;
echo "Please enter maximum number of iterations in a single run:";
read maxiterations;
echo "Please enter step width:";
read steps;
echo "Please enter number of repetitions:";
read repetitions;

# Python:

echo "Python:";

let i=$miniterations;

let j=0;

if [[ $kind == "L" ]]
then
  echo "Linear 1st order feedback control"
elif [[ $kind="M" ]]
then
  echo "MiMe NoCoDI loop"
fi

echo;

while [ $i -le $maxiterations ]
do
  let r=0;
  until [ $r -ge $repetitions ]
  do
    let r=$r+1;
    if [[ $kind == "L" ]]
    then
      response=$(./Simulator_FC1L_Python.py -b --iterations=$i)
    elif [[ $kind="M" ]]
    then
      response=$(./Simulator_FC0M_Python.py -b --iterations=$i)
    fi
    runtime=${response#*simulation: };
    result=${runtime% ms};
    echo $result;
    let j=$j+1;
  done;
  let i=$i+$steps;
done;

echo "Number of simulations performed" $j;
