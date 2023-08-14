// CyberUnits

// Object Pascal, S, Python and Swift units for computational cybernetics

// Bricks: Basic blocks for information processing structures

// Version 2.1.0 (Foudre)

// (c) Johannes W. Dietrich, 1994 - 2023
// (c) Ludwig Maximilian University of Munich 1995 - 2002
// (c) University of Ulm Hospitals 2002 - 2004
// (c) Ruhr University of Bochum 2005 - 2023

// Swift version of simulation program for purposes of benchmarking

// Source code released under the BSD License

// See the file "license.txt", included in this distribution,
// for details about the copyright.
// Current versions and additional information are available from
// http://cyberunits.sf.net

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

import Foundation
import Darwin
import Numerics

extension String: Error {}

var ShowTimeSeries = true
var iterations = 30

print()
print("CyberUnits Bricks demo application")
print("MiMe NoCoDI loop demo (CLI version)")
print()

let argus = CommandLine.arguments
if argus.count > 1  // ignore first argument with index = 0 (path to executable)
{
    for i in 1...argus.count-1
    {
       switch argus[i]
       {
          case "-b", "--benchmark":
             ShowTimeSeries = false
          case "-i":
             if (argus.count-1 > i)
             {
                 iterations = Int(argus[i+1]) ?? 30
             } else
             {
                 print("Number of iterations missing")
                 exit(-1)
             }
         default:
             (
                 print(argus[i])
             )
         }
    }
}

var x: Double = 5

let G1 = 2.6
let G2 = 5.0
let G3 = 0.3
let D2 = 0.5

print("Iterations: \(iterations)")

let startTime = Date().timeIntervalSince1970

class TValues
{
    var i = [Int]()
    var x = [Double]()
    var e = [Double]()
    var c = [Double]()
    var y = [Double]()
    var yr = [Double]()
    
    var size: Int
    {
        get { return x.count }
        set { resizeArrays(newSize: newValue) }
    }
    
    private func resizeArrays(newSize: Int)
    {
        i = Array(i.prefix(newSize))
        x = Array(x.prefix(newSize))
        e = Array(e.prefix(newSize))
        c = Array(c.prefix(newSize))
        y = Array(y.prefix(newSize))
        yr = Array(yr.prefix(newSize))
    }
}

var prediction1: [String: Double] =
[
    "x": Double(x),
    "e": Double.nan,
    "c": Double.nan,
    "y": Double.nan,
    "yr": Double.nan
]

var prediction2: [String: Double] =
[
    "x": Double(x),
    "e": Double.nan,
    "c": Double.nan,
    "y": Double.nan,
    "yr": Double.nan
]

struct TBlocks
{
    var G1: TP
    var G3: TP
    var MiMe: TMiMe
    var NoCoDI: TNoCoDI
}

let a = D2 * G3
let b = D2 + G1 * prediction1["x"]!
let d = -G1 * G2 * prediction1["x"]! ;
prediction1["y"] = -(b + sqrt(b * b - 4 * a * d)) / (2 * a)
prediction1["yr"] = G3 * prediction1["y"]!
prediction1["e"] = prediction1["x"]! / (1 + prediction1["yr"]!)
prediction1["c"] = G1 * prediction1["e"]!
prediction2["y"] = -(b - sqrt(b * b - 4 * a * d)) / (2 * a)
prediction2["yr"] = G3 * prediction2["y"]!
prediction2["e"] = prediction2["x"]! / (1 + prediction2["yr"]!)
prediction1["c"] = G1 * prediction2["e"]!

var gValues = TValues()
var gBlocks: TBlocks = .init(G1: TP(), G3: TP(), MiMe: TMiMe(), NoCoDI: TNoCoDI())

gValues.size = iterations
gBlocks.G1.G = G1
gBlocks.G3.G = G3
gBlocks.MiMe.G = G2
gBlocks.MiMe.D = D2
var yr = 20.0

var e: Double
var c: Double
var y: Double

for i in 0..<iterations
{
    gBlocks.NoCoDI.input1 = x
    gBlocks.NoCoDI.input2 = yr
    e = gBlocks.NoCoDI.simAndGetOutput()
    gBlocks.G1.input = e
    c = gBlocks.G1.simAndGetOutput()
    gBlocks.MiMe.input = c
    y = gBlocks.MiMe.simAndGetOutput()
    gBlocks.G3.input = y
    yr = gBlocks.G3.simAndGetOutput()
    
    gValues.i.append(i)
    gValues.x.append(Double(x))
    gValues.e.append(e)
    gValues.c.append(c)
    gValues.y.append(y)
    gValues.yr.append(yr)
}

if ShowTimeSeries
{
    print("i\tx\te\tc\ty\tyr")
    for i in 0..<iterations
    {
        print(Int(gValues.i[i] + 1), "\t", gValues.x[i], "\t", round(10000 * gValues.e[i]) / 10000, "\t", round(10000 * gValues.c[i]) / 10000, "\t", round(10000 * gValues.y[i]) / 10000, "\t", round(10000 * gValues.yr[i]) / 10000)
    }
}

let stopTime = Date().timeIntervalSince1970

let deltaTime = (stopTime - startTime) * 1000 // ms

print("\nStart time: \(startTime)")
print("Stop time: \(stopTime)")
print("Elapsed time for simulation: \(deltaTime) ms")

