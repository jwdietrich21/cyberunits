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
from lifeblocks import *

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
print("MiMe NoCoDI loop demo (CLI version)")
print()

x = 5

G1 = 2.6
G2 = 5.0
G3 = 0.3
D2 = 0.5

print("Iterations: ", end = "")
print(iterations)

startTime = time.time()

Values = {
    "i": list(range(0, iterations)),
    "x": [x] * iterations,
    "e": [float("NaN")] * iterations,
    "c": [float("NaN")] * iterations,
    "y": [float("NaN")] * iterations,
    "yr": [float("NaN")] * iterations
}

prediction1 = {
    "x": x,
    "e": float("NaN"),
    "c": float("NaN"),
    "y": float("NaN"),
    "yr": float("NaN")
}

prediction2 = {
    "x": x,
    "e": float("NaN"),
    "c": float("NaN"),
    "y": float("NaN"),
    "yr": float("NaN")
}

# Solving for y:
a = D2 * G3;
b = D2 + G1 * prediction1["x"] ;
d = -G1 * G2 * prediction1["x"] ;
prediction1["y"] = -(b + math.sqrt(b * b - 4 * a * d)) / (2 * a)
prediction1["yr"] = G3 * prediction1["y"]
prediction1["e"] = prediction1["x"] / (1 + prediction1["yr"])
prediction1["c"] = G1 * prediction1["e"]
prediction2["y"] = -(b - math.sqrt(b * b - 4 * a * d)) / (2 * a)
prediction2["yr"] = G3 * prediction2["y"]
prediction2["e"] = prediction2["x"] / (1 + prediction2["yr"])
prediction1["c"] = G1 * prediction2["e"]

blocks = {
    "G1": float("NaN"),
    "G2": float("NaN"),
    "G3": float("NaN"),
    "D2": float("NaN")
}
blocks["G1"] = TP()
blocks["G3"] = TP()
blocks["MiMe"] = TMiMe()
blocks["NoCoDI"] = TNoCoDI()

blocks["G1"].G = G1
blocks["G3"].G = G3
blocks["MiMe"].G = G2
blocks["MiMe"].D = D2
yr = 20;

for i in range(0, iterations):

   blocks["NoCoDI"].input1 = x
   blocks["NoCoDI"].input2 = yr
   e = blocks["NoCoDI"].SimAndGetOutput()
   blocks["G1"].input = e
   c = blocks["G1"].SimAndGetOutput()
   blocks["MiMe"].input = c
   y = blocks["MiMe"].SimAndGetOutput()
   blocks["G3"].input = y
   yr = blocks["G3"].SimAndGetOutput()
   Values["i"][i] = i
   Values["x"][i] = x
   Values["e"][i] = e
   Values["c"][i] = c
   Values["y"][i] = y
   Values["yr"][i] = yr

if (ShowTimeSeries):
   print("i\tx\te\tc\ty\tyr")
   for i in range(0, iterations):
      print(Values["i"][i] + 1, "\t", Values["x"][i], "\t", Values["e"][i], "\t", round(Values["c"][i], 4), "\t", round(Values["y"][i], 4), "\t", round(Values["yr"][i], 4))

stopTime = time.time()

deltaTime = (stopTime - startTime) * 1000; # ms

print("\nStart time: ", end = "");
print(startTime);

print("Stop time: ", end = "");
print(stopTime);

print("Elapsed time for simulation: ", end = "");
print(deltaTime, "ms");
