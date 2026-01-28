# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# Bricks: Basic blocks for information processing structures

# Version 2.1.0 (Foudre)

# (c) Johannes W. Dietrich, 1994 - 2023
# (c) Ludwig Maximilian University of Munich 1995 - 2002
# (c) University of Ulm Hospitals 2002 - 2004
# (c) Ruhr University of Bochum 2005 - 2023

# Standard blocks for systems modelling and simulation

# Source code released under the BSD License

# See the file "license.txt", included in this distribution,
# for details about the copyright.
# Current versions and additional information are available from
# http://cyberunits.sf.net

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

import math
from typing import List

kError101 = "Runtime error: Negative parameter(s)"
kError102 = "Runtime error: Parameter(s) out of range"
kError103 = "Runtime error: min > max"
kError104 = "Runtime error: max = 0"
kError105 = "Runtime error: Denominator is zero"
kError210 = 'Runtime error: Nil pointer';

class TFR:
    def __init__(self):
        self.M = 0.0
        self.phi = 0.0
        self.F = complex()

class TModel:
    def __init__(self):
        self.delta = 0.0
        self.time = 0.0
        self.firstBlock = None

class TBlock:
    def __init__(self):
        self.name = ""
        self.model = None
        self.output = 0.0
        self.fr = TFR()

class TControlledBlock(TBlock):
    def __init__(self):
        super().__init__()
        self.input = 0.0
        self.G = 0.0
        self.amplitude = 0.0
        self.omega = 0.0

class TInvertibleBlock(TBlock):
    def __init__(self):
        super().__init__()
        self.input1 = 0.0
        self.input2 = 0.0
        self.G = 0.0

class TTestSignal(TBlock):
    def __init__(self):
        super().__init__()

class TTHarmonic(TTestSignal):
    def __init__(self):
        super().__init__()
        self.G = 0.0
        self.omega = 0.0
        self.phi = 0.0
        self.delta = 0.0
        self.updateTime = False

    def SimAndGetOutput(self):
        self.simulate()
        return self.output

    def simulate(self):
        assert self.model is not None, kError210
        self.output = (self.G + self.G * math.sin(self.omega * self.model.time + self.phi)) / 2
        if self.updateTime:
            self.model.time += self.delta

class TP(TControlledBlock):
    def __init__(self):
        super().__init__()

    def SimAndGetOutput(self):
        self.simulate()
        return self.output

    def GetFR(self):
        assert self.G >= 0, kError101
        assert self.omega >= 0, kError101
        fr = TFR()
        fr.M = self.amplitude * self.G
        fr.phi = 0
        return fr

    def simulate(self):
        assert self.G >= 0, kError101
        self.output = self.input * self.G
        
class TPAdd(TInvertibleBlock):
    def __init__(self):
        super().__init__()
        self.G = 1

    def output(self):
        return self.fOutput

    def simulate(self):
        assert self.G >= 0, kError101
        self.fOutput = self.G * (self.input1 + self.input2)

    def SimAndGetOutput(self):
        self.simulate()
        return self.fOutput

class TPSub(TInvertibleBlock):
    def __init__(self):
        super().__init__()
        self.G = 1

    def output(self):
        return self.fOutput

    def simulate(self):
        assert self.G >= 0, kError101
        self.fOutput = self.G * (self.input1 - self.input2)

    def SimAndGetOutput(self):
        self.simulate()
        return self.fOutput

class TPT1(TControlledBlock):
    def __init__(self):
        super().__init__()
        self.t1 = 0.0
        self.x1 = 0.0

    def SimAndGetOutput(self):
        self.simulate()
        return self.fOutput

    def GetFR(self):
        assert self.G >= 0, kError101
        assert self.t1 >= 0, kError101
        assert self.omega >= 0, kError101
        fr = TFR()
        fr.M = self.amplitude * self.G / math.sqrt(1 + math.pow(self.omega, 2) * math.pow(self.t1, 2))
        fr.phi = -math.atan(self.omega * self.t1)
        return fr

    def simulate(self):
        assert self.G >= 0, kError101
        assert self.t1 >= 0, kError101
        f = math.exp(-self.delta / self.t1)
        self.fOutput = f * self.x1 + self.G * (1 - f) * self.input
        self.x1 = self.fOutput
                
TestEnvironment = False # set to TRUE to enable some basic unit tests

def CheckEqual(expected, received):
   if received == expected:
      print("passed")
   else:
      print("failed")
      raise Warning("failed")


def CheckTrue(received):
    if isinstance(received, bool):
        if received:
            print("passed")
        else:
            print("failed")
            raise Warning("failed")
    else:
        print("failed")
        raise Warning("failed")

if TestEnvironment:
   print("Unit tests:")

    # TP:

   testBrick = TP()
   testBrick.G = 5
   testBrick.input = 2
   print("Test #1 for TP element:   ", end = "")
   res = testBrick.SimAndGetOutput()
   CheckEqual(5 * 2, res)
   print("Test #2 for TP element:   ", end = "")
   res = testBrick.SimAndGetOutput()
   CheckEqual(5 * 2, res)
   testBrick.amplitude = 2
   print("Test #3 for TP element:   ", end = "")
   res = testBrick.GetFR()
   CheckEqual(5 * 2, res.M)

   # TPAdd:

   testBrick = TPAdd()
   testBrick.G = 1
   testBrick.input1 = 2
   testBrick.input2 = 11
   print("Test for TPAdd element:   ", end = "")
   res = testBrick.SimAndGetOutput()
   CheckEqual(13, res)

   # TPSub:

   testBrick = TPSub()
   testBrick.input1 = 107
   testBrick.input2 = 11
   print("Test for TPSub element:   ", end = "")
   res = testBrick.SimAndGetOutput()
   CheckEqual(96, res)

   # TPT1:

   testBrick = TPT1()
   testBrick.G = 5
   testBrick.delta = 10
   testBrick.t1 = 15
   testBrick.input = 2
   #testBrick.x1 = 0
   print("Test #1 for TPT1 element: ", end = "")   
   CheckEqual(0.0, testBrick.output)
   print("Test #2 for TPT1 element: ", end = "")
   for i in range(1, 100000):
      res = testBrick.SimAndGetOutput()
      testBrick.x1 = res
   CheckTrue(testBrick.G * testBrick.input - testBrick.x1 < 1e-13)

   testBrick.G = 5
   testBrick.delta = 1
   testBrick.t1 = 15e6
   testBrick.input = 2
   testBrick.x1 = 0
   print("Test #3 for TPT1 element: ", end = "")
   CheckEqual(0, testBrick.output)
   print("Test #4 for TPT1 element: ", end = "")
   for i in range(1, 100000):
      res = testBrick.SimAndGetOutput()
      testBrick.x1 = res
   CheckTrue(testBrick.x1 < 0.07)

