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
    // TP:
    
    double res1, res2;
    TFR res3;
    std::cout << "Unit tests:\n";
    TP testBrick{};
    testBrick.G = 5;
    testBrick.input = 2;
    std::cout << "Test #1 for TP element:   ";
    res1 = testBrick.simAndGetOutput();
    CheckEqual(5 * 2, res1);
    std::cout << "Test #2 for TP element:   ";
    res2 = testBrick.simAndGetOutput();
    CheckEqual(5 * 2, res2);
    std::cout << "Test #3 for TP element:   ";
    testBrick.amplitude = 2;
    res3 = testBrick.getFR();
    CheckEqual(5 * 2, res3.M);
    
    // TPAdd:
    
    double resAdd;
    TPAdd testBrickAdd{};
    testBrickAdd.G = 1;
    testBrickAdd.input1 = 2;
    testBrickAdd.input2 = 11;
    std::cout << "TTest for TPAdd element:  ";
    resAdd = testBrickAdd.simAndGetOutput();
    CheckEqual(13, resAdd);


    // TPSub:
    
    double resSub;
    TPSub testBrickSub{};
    testBrickSub.G = 1;
    testBrickSub.input1 = 107;
    testBrickSub.input2 = 11;
    std::cout << "TTest for TPSub element:  ";
    resSub = testBrickSub.simAndGetOutput();
    CheckEqual(96, resSub);

    // TPT1:

    double resT1;
    TPT1 testBrickT1_1{};
    testBrickT1_1.G = 5;
    testBrickT1_1.delta = 10;
    testBrickT1_1.t1 = 15;
    testBrickT1_1.input = 2;
    testBrickT1_1.x1 = 0;
    std::cout << "Test #1 for TPT1 element: ";
    resT1 = testBrickT1_1.output();
    CheckEqual(0.0, resT1);
    std::cout << "Test #2 for TPT1 element: ";
    for (int i = 1; i <= 100000; i++)
    {
        resT1 = testBrickT1_1.simAndGetOutput();
        testBrickT1_1.x1 = resT1;
    }
    CheckTrue(testBrickT1_1.G * testBrickT1_1.input - testBrickT1_1.x1 < 1e-13);

    TPT1 testBrickT1_2 = TPT1{};
    testBrickT1_2.G = 5;
    testBrickT1_2.delta = 1;
    testBrickT1_2.t1 = 15e6;
    testBrickT1_2.input = 2;
    testBrickT1_2.x1 = 0;
    std::cout << "Test #3 for TPT1 element: ";
    resT1 = testBrickT1_2.output();
    CheckEqual(0, resT1);
    std::cout << "Test #4 for TPT1 element: ";
    for (int i = 1; i <= 100000; i++)
    {
        resT1 = testBrickT1_2.simAndGetOutput();
        testBrickT1_2.x1 = resT1;
    }
    CheckTrue(testBrickT1_2.x1 < 0.07);
}
