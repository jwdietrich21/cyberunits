# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# LifeBlocks: Metabricks for information processing structures in organisms

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
from bricks import *
from typing import List

class TASIA(TControlledBlock):
    def __init__(self):
        super().__init__()
        self.PT1Analog = TPT1()
        self.alpha = 1.0
        self.beta = 1.0
        self.delta = 1
        self.x1 = 0.0
        
    def SetAlpha(self):
        self.G = self.alpha / self.beta
        self.PT1Analog.G = self.G
        
    def SetBeta(self):
        self.G = self.alpha / self.beta
        self.PT1Analog.G = self.G
        self.PT1Analog.t1 = 1 / self.beta
       
    def SetDelta(self):
        self.PT1Analog.delta = self.delta
        
    def Setx1(self):
        self.PT1Analog.x1 = self.x1
        
    def Getx1(self):
        self.x1 = self.PT1Analog.x1

    def SimAndGetOutput(self):
        self.PT1Analog.input = self.input
        self.simulate()
        self.PT1Analog.x1 = self.x1
        return self.output

    def simulate(self):
        assert self.G >= 0, kError101
        assert self.beta != 0, kError102
        self.output = self.PT1Analog.SimAndGetOutput()
        
        
class TMiMe(TControlledBlock):
    def __init__(self):
        super().__init__()
        self.D = 1

    def SimAndGetOutput(self):
        self.simulate()
        return self.output

    def simulate(self):
        assert self.G >= 0, kError101
        self.output = self.G * self.input / (self.D + self.input)


class TNoCoDI(TInvertibleBlock):
    def __init__(self):
        super().__init__()

    def SimAndGetOutput(self):
        self.simulate()
        return self.output

    def simulate(self):
        assert self.input2 != -1, kError101
        self.output = self.input1 / (1 + self.input2)

              
TestEnvironment = False # set to True to enable some basic unit tests

if TestEnvironment:
   print("Unit tests:")

   # TASIA:
   print("Test for TASIA element:   ", end = "")
   alpha = 10
   beta = 0.5

   testBrick = TASIA()
   testBrick.alpha = alpha
   testBrick.beta = beta
   testBrick.delta = 1
   testBrick.SetAlpha()
   testBrick.SetBeta()
   testBrick.SetDelta()
   testBrick.input = 1
   for i in range(1, 100):
      res = testBrick.SimAndGetOutput()
      testBrick.x1 = res
      testBrick.Setx1()
   CheckTrue(alpha / beta - testBrick.x1 < 0.07)

   # TMiMe:
   print("Test for TMiMe element:   ", end = "")
   G = 5
   D = 2
   testSignal = 10

   testBrick = TMiMe()
   testBrick.G = G
   testBrick.D = D
   testBrick.input = testSignal
   res = testBrick.SimAndGetOutput()
   CheckEqual(G * testSignal / (D + testSignal), testBrick.output)
   
   # TNoCoDI:
   print("Test for TNoCoDI element: ", end = "")
   xe1 = 5
   xe2 = 4

   testBrick = TNoCoDI()
   testBrick.input1 = xe1
   testBrick.input2 = xe2
   res = testBrick.SimAndGetOutput()
   CheckEqual(xe1 / (1 + xe2), testBrick.output)
   