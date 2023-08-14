// CyberUnits

// Object Pascal, S, Python and Swift units for computational cybernetics

// Bricks: Basic blocks for information processing structures

// Version 2.1.0 (Foudre)

// (c) Johannes W. Dietrich, 1994 - 2023
// (c) Ludwig Maximilian University of Munich 1995 - 2002
// (c) University of Ulm Hospitals 2002 - 2004
// (c) Ruhr University of Bochum 2005 - 2023

// Standard blocks for systems modelling and simulation

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

let kError101 = "Runtime error: Negative parameter(s)"
let kError102 = "Runtime error: Parameter(s) out of range"
let kError103 = "Runtime error: min > max"
let kError104 = "Runtime error: max = 0"
let kError105 = "Runtime error: Denominator is zero"
let kError210 = "Runtime error: Nil pointer"

public struct TFR
{
    var M: Double = 0.0
    var phi: Double = 0.0
}

public class TModel
{
    var delta: Double
    var time: Double
    var firstBlock: TBlock?

    init()
    {
        delta = 1
        time = 0
        firstBlock = TBlock()
    }

    func reset()
    {
        time = 0
    }
}

public class TBlock
{
    var name: String = ""
    var fOutput: Double = 0
    weak var model: TModel?
    var fr: TFR = TFR()

    func simAndGetOutput() -> Double {
        fatalError("Abstract method, should be overridden in subclasses.")
    }

    deinit {
        // Destructor
    }

    func simulate() {
        fatalError("Abstract method, should be overridden in subclasses.")
    }
}

public class TControlledBlock: TBlock
{
    var input: Double = 0.0
    var G: Double = 0.0
    var amplitude: Double = 0.0
    var omega: Double = 0.0
    
    override func simAndGetOutput() -> Double {
        fatalError("Abstract method, should be overridden in subclasses.")
    }
    
    func getFR() -> TFR {
        fatalError("Abstract method, should be overridden in subclasses.")
    }
}

public class TInvertibleBlock: TBlock
{
    var input1: Double = 0.0
    var input2: Double = 0.0
    var G: Double = 0.0
    
    override func simAndGetOutput() -> Double {
        fatalError("Abstract method, should be overridden in subclasses.")
    }
    
    func getFR() -> TFR {
        fatalError("Abstract method, should be overridden in subclasses.")
    }
}

public class TTestSignal: TBlock
{
    override func simAndGetOutput() -> Double
    {
        fatalError("Abstract method, should be overridden in subclasses.")
    }
    
    deinit
    {
        // Deinitialize test signal here
    }
    
    var simOutput: Double
    {
        return simAndGetOutput()
    }
}

public class TTHarmonic: TTestSignal
{
    var G: Double = 0.0
    var omega: Double = 0.0
    var phi: Double = 0.0
    var delta: Double = 0.0
    var updateTime: Bool = false

    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }

    override init()
    {
        super.init()
        delta = 1
    }
 
    override func simulate()
        {
        assert(model != nil, kError210)
        fOutput = (G + G * sin(omega * model!.time + phi)) / 2
        if updateTime
            {
            model!.time += delta
            }
        }
}

public class TP: TControlledBlock
{
    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }

    override func getFR() -> TFR
    {
        assert(G >= 0, kError101)
        assert(omega >= 0, kError101)
        var fr = TFR()
        fr.M = amplitude * G
        fr.phi = 0
        return fr
    }

    override func simulate()
    {
        assert(G >= 0, kError101)
        fOutput = input * G
    }
}

public class TPAdd: TInvertibleBlock
{
    func output() -> Double
    {
        return fOutput
    }

    override func simulate()
    {
        assert(G >= 0, kError101)
        fOutput = G * (input1 + input2)
    }

    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }
}

public class TPSub: TInvertibleBlock
{
    func output() -> Double
    {
        return fOutput
    }

    override func simulate()
    {
        assert(G >= 0, kError101)
        fOutput = G * (input1 - input2)
    }

    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }
}

public class TPT1: TControlledBlock
{
    var t1: Double = 0.0
    var x1: Double = 0.0
    var delta: Double = 1

    func output() -> Double
    {
        return fOutput
    }

    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }

    override func getFR() -> TFR
    {
        assert(G >= 0, kError101)
        assert(t1 >= 0, kError101)
        assert(omega >= 0, kError101)
        var fr = TFR()
        fr.M = amplitude * G / sqrt(1 + pow(omega, 2) * pow(t1, 2))
        fr.phi = -atan(omega * t1)
        return fr
    }

    override func simulate()
    {
        assert(G >= 0, kError101)
        assert(t1 >= 0, kError101)
        let f = exp(-delta / t1)
        fOutput = f * x1 + G * (1 - f) * input
        x1 = fOutput
    }
}
