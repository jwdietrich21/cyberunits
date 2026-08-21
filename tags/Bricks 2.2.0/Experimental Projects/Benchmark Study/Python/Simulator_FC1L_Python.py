#!/usr/bin/env python3

# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# Bricks: Basic blocks for information processing structures

# Version 2.1.0 (Foudre)

# (c) Johannes W. Dietrich, 1994 - 2023
# (c) Ludwig Maximilian University of Munich 1995 - 2002
# (c) University of Ulm Hospitals 2002 - 2004
# (c) Ruhr University of Bochum 2005 - 2023

# Python version of simulation program for purposes of benchmarking

# Source code released under the BSD License

# See the file "license.txt", included in this distribution,
# for details about the copyright.
# Current versions and additional information are available from
# http://cyberunits.sf.net

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

import time
import math
import argparse
from bricks import *

parser = argparse.ArgumentParser(description="Just an example", formatter_class = argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-b", "--benchmark", action = "store_true", help="benchmark	")
parser.add_argument("-i", "--iterations", type = int, help="iterations	")

args = parser.parse_args()
config = vars(args)
print(config)

ShowTimeSeries = True
iterations = 30

if(args.benchmark):
   ShowTimeSeries = False
   
if(args.iterations):
   iterations = args.iterations

print()
print("CyberUnits Bricks demo application")
print("Linear 1st order feedback demo (Python version)")
print()

x = 5
z = 2.0

G1 = 1.3
G2 = 0.5

print("Iterations: ", end = "")
print(iterations)

startTime = time.time()

Values = {
    "i": list(range(0, iterations)),
    "x": [x] * iterations,
    "z": [z] * iterations,
    "e": [float("NaN")] * iterations,
    "y": [float("NaN")] * iterations,
    "yr": [float("NaN")] * iterations,
    "ys": [float("NaN")] * iterations
}

prediction = {
    "x": x,
    "z": z,
    "e": float("NaN"),
    "y": float("NaN"),
    "yr": float("NaN"),
    "ys": float("NaN")
}

prediction["y"] = (G1 * x + z) / (1 + G1 * G2)
prediction["yr"] = G2 * prediction["y"]
prediction["e"] = prediction["x"] - prediction["yr"]
prediction["ys"] = G1 * prediction["e"]

blocks = {
    "G1": float("NaN"),
    "G2": float("NaN"),
    "PT1": float("NaN"),
    "Comparator": float("NaN"),
    "LoadInjection": float("NaN")
}
blocks["G1"] = TP()
blocks["G2"] = TP()
blocks["PT1"] = TPT1()
blocks["Comparator"] = TPSub()
blocks["LoadInjection"] = TPAdd()

blocks["G1"].G = G1;
blocks["G2"].G = G2;
blocks["PT1"].G = 1;
blocks["PT1"].t1 = 5;
blocks["PT1"].delta = 1;
blocks["PT1"].x1 = 0;
yr = 20;

for i in range(0, iterations):
   blocks["Comparator"].input1 = x
   blocks["Comparator"].input2 = yr
   e = blocks["Comparator"].SimAndGetOutput()
   blocks["G1"].input = e
   ys = blocks["G1"].SimAndGetOutput()
   blocks["LoadInjection"].input1 = ys
   blocks["LoadInjection"].input2 = z
   yz = blocks["LoadInjection"].SimAndGetOutput()
   blocks["PT1"].input = yz
   res = blocks["PT1"].SimAndGetOutput()
   y = blocks["PT1"].x1 = res
   blocks["G2"].input = y
   yr = blocks["G2"].SimAndGetOutput()
   Values["i"][i] = i
   Values["x"][i] = x
   Values["z"][i] = z
   Values["e"][i] = e
   Values["y"][i] = y
   Values["yr"][i] = yr
   Values["ys"][i] = ys

if (ShowTimeSeries):
   print("i\tx\tz\te\ty\tyr\tys")
   for i in range(0, iterations):
      print(Values["i"][i] + 1, "\t", Values["x"][i], "\t", Values["z"][i], "\t", round(Values["e"][i], 4), "\t", round(Values["y"][i], 4), "\t", round(Values["yr"][i], 4), "\t", round(Values["ys"][i], 4))

stopTime = time.time()

deltaTime = (stopTime - startTime) * 1000; # ms

print("\nStart time: ", end = "");
print(startTime);

print("Stop time: ", end = "");
print(stopTime);

print("Elapsed time for simulation: ", end = "");
print(deltaTime, "ms");
