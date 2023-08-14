// CyberUnits

// Object Pascal, S, Python and Swift units for computational cybernetics

// LifeBlocks: Metabricks for information processing structures in organisms

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

public class TASIA: TControlledBlock
{
    var PT1Analog: TPT1
    var alpha, beta: Double
    
    func setAlpha(_ aValue: Double)
    {
        alpha = aValue
        PT1Analog.G = alpha / beta
    }
    
    func setBeta(_ aValue: Double)
    {
        beta = aValue
        PT1Analog.G = alpha / beta
        PT1Analog.t1 = 1 / beta
    }
    
    func setDelta(_ aValue: Double)
    {
        PT1Analog.delta = aValue
    }

    func setX1(_ aValue: Double)
    {
        PT1Analog.x1 = aValue
    }
    
    func getX1() -> Double
    {
        return PT1Analog.x1
    }
    
    override init()
    {
        alpha = 1
        beta = 1
        PT1Analog = TPT1()
        super.init()
        setAlpha(1)
        setBeta(1)
        setDelta(1)
    }
    
    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }

    override func simulate()
    {
        assert(beta != 0, kError102)
        PT1Analog.input = input
        fOutput = PT1Analog.simAndGetOutput()
    }
}

public class TMiMe: TControlledBlock
{
    var D: Double
    
    override init()
    {
        self.D = 1
        super.init()
    }
    
    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }

    override func simulate()
    {
        assert(input != -D, kError102)
        fOutput = G * input / (D + input)
    }
}

public class TNoCoDI: TInvertibleBlock
{
    override init()
    {
        super.init()
    }
    
    override func simAndGetOutput() -> Double
    {
        simulate()
        return fOutput
    }

    override func simulate()
    {
        assert(input2 != -1, kError102)
        fOutput = input1 / (1 + input2)
    }
}
