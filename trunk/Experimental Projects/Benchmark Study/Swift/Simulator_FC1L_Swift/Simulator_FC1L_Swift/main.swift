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
print("Linear 1st order feedback demo (Swift version)")
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

var x = 5
var z = 2.0

let G1 = 1.3
let G2 = 0.5

print("Iterations: \(iterations)")

let startTime = Date().timeIntervalSince1970

class TValues
{
    var i = [Int]()
    var x = [Double]()
    var z = [Double]()
    var e = [Double]()
    var y = [Double]()
    var yr = [Double]()
    var ys = [Double]()
    
    var size: Int
    {
        get { return x.count }
        set { resizeArrays(newSize: newValue) }
    }
    
    private func resizeArrays(newSize: Int)
    {
        x = Array(x.prefix(newSize))
        z = Array(z.prefix(newSize))
        e = Array(e.prefix(newSize))
        y = Array(y.prefix(newSize))
        yr = Array(yr.prefix(newSize))
        ys = Array(ys.prefix(newSize))
    }
}

var prediction: [String: Double] = [
    "x": Double(x),
    "z": z,
    "e": Double.nan,
    "y": Double.nan,
    "yr": Double.nan,
    "ys": Double.nan
]

struct TBlocks
{
    var G1: TP
    var G2: TP
    var PT1: TPT1
    var Comparator: TPSub
    var LoadInjection: TPAdd
}

prediction["y"] = (G1 * Double(x) + z) / (1 + G1 * G2)
prediction["yr"] = G2 * prediction["y"]!
prediction["e"] = prediction["x"]! - prediction["yr"]!
prediction["ys"] = G1 * prediction["e"]!

var gValues = TValues()
var gBlocks: TBlocks = .init(G1: TP(), G2: TP(), PT1: TPT1(), Comparator: TPSub(), LoadInjection: TPAdd())

gValues.size = iterations
gBlocks.G1.G = G1
gBlocks.G2.G = G2
gBlocks.Comparator.G = 1
gBlocks.LoadInjection.G = 1
gBlocks.PT1.G = 1
gBlocks.PT1.t1 = 5
gBlocks.PT1.delta = 1
gBlocks.PT1.x1 = 0
var yr = 20.0

var e: Double
var ys: Double
var yz: Double
var y: Double

for i in 0..<iterations
{
    gBlocks.Comparator.input1 = Double(x)
    gBlocks.Comparator.input2 = yr
    e = gBlocks.Comparator.simAndGetOutput()
    gBlocks.G1.input = e
    ys = gBlocks.G1.simAndGetOutput()
    gBlocks.LoadInjection.input1 = ys
    gBlocks.LoadInjection.input2 = z
    yz = gBlocks.LoadInjection.simAndGetOutput()
    gBlocks.PT1.input = yz
    y = gBlocks.PT1.simAndGetOutput()
    gBlocks.G2.input = y
    yr = gBlocks.G2.simAndGetOutput()
    gValues.i.append(i)
    gValues.x.append(Double(x))
    gValues.z.append(z)
    gValues.e.append(e)
    gValues.y.append(y)
    gValues.yr.append(yr)
    gValues.ys.append(ys)
}

if ShowTimeSeries
{
    print("i\tx\tz\te\ty\tyr\tys")
    for i in 0..<iterations
    {
        print(Int(gValues.i[i] + 1), "\t", gValues.x[i], "\t", gValues.z[i], "\t", round(10000 * gValues.e[i]) / 10000, "\t", round(10000 * gValues.y[i]) / 10000, "\t", round(10000 * gValues.yr[i]) / 10000, "\t", round(10000 * gValues.ys[i]) / 10000)
    }
}

let stopTime = Date().timeIntervalSince1970

let deltaTime = (stopTime - startTime) * 1000 // ms

print("\nStart time: \(startTime)")
print("Stop time: \(stopTime)")
print("Elapsed time for simulation: \(deltaTime) ms")

