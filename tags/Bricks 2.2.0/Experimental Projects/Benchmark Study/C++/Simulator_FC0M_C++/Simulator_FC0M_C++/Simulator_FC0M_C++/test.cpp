// CyberUnits

// Object Pascal, C++, S, Python and Swift units for computational cybernetics

// Bricks: Basic blocks for information processing structures

// Version 2.1.0 (Foudre)

// (c) Johannes W. Dietrich, 1994 - 2023
// (c) Ludwig Maximilian University of Munich 1995 - 2002
// (c) University of Ulm Hospitals 2002 - 2004
// (c) Ruhr University of Bochum 2005 - 2023

// Source code released under the BSD License

// See the file "license.txt", included in this distribution,
// for details about the copyright.
// Current versions and additional information are available from
// http://cyberunits.sf.net

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#include <iostream>
#ifndef bricks_hpp
#include "bricks.hpp"
#endif

#ifndef lifeblocks_hpp
#include "lifeblocks.hpp"
#endif

#define unittests

void CheckEqual(double expected, double received)
{
    if (received == expected)
    {
        std::cout << "passed\n";
    } else
    {
        std::cout << "failed\n";
        std::cerr << "failed";
    }
}

void CheckTrue(bool received)
{
    if (received)
    {
        std::cout << "passed\n";
    } else
    {
        std::cout << "failed\n";
        std::cerr << "failed";
    }
}

void testUnits()
{
    std::cout << "Unit tests:\n";
    
    // TASIA:
    std::cout << "TTest for TASIA element:   ";
    double alpha = 10;
    double beta = 0.5;
    double resASIA;
    TASIA testBrickTASIA;
    testBrickTASIA.setAlpha(alpha);
    testBrickTASIA.setBeta(beta);
    testBrickTASIA.setDelta(1);
    testBrickTASIA.input = 1.0;
    for (int i = 1; i <= 100; i++)
    {
        resASIA = testBrickTASIA.simAndGetOutput();
        testBrickTASIA.setX1(resASIA);
    }
    CheckTrue((alpha / beta) - testBrickTASIA.PT1Analog.x1 < 0.07);
    
    // TMiMe:
    std::cout << "TTest for TMiMe element:   ";
    double G = 5;
    double D = 2;
    double testSignal = 10.0;
    double resMiMe;
    TMiMe testBrickMiMe;
    testBrickMiMe.G = G;
    testBrickMiMe.D = D;
    testBrickMiMe.input = testSignal;
    resMiMe = testBrickMiMe.simAndGetOutput();
    CheckEqual(G * testSignal / (D + testSignal), resMiMe);

    // TNoCoDI:
    std::cout << "TTest for TNoCoDI element: ";
    double xe1 = 5;
    double xe2 = 4;
    double resNoCoDI;
    TNoCoDI testBrickNoCoDI;
    testBrickNoCoDI.input1 = xe1;
    testBrickNoCoDI.input2 = xe2;
    resNoCoDI = testBrickNoCoDI.simAndGetOutput();
    CheckEqual(xe1 / (1 + xe2), resNoCoDI);
}
