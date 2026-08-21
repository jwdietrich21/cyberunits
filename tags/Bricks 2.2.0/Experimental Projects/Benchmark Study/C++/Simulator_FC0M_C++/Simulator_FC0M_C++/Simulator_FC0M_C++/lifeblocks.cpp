// CyberUnits

// Object Pascal, C++, S, Python and Swift units for computational cybernetics

// LifeBlocks: Metabricks for information processing structures in organisms

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

#include "lifeblocks.hpp"
#include "cassert"

TASIA::TASIA()
{
    alpha = 1;
    beta = 1;
    TPT1 PT1Analog;
    setAlpha(1);
    setBeta(1);
    setDelta(1);
}

void TASIA::setAlpha(double aValue)
{
    alpha = aValue;
    PT1Analog.G = alpha/beta;
}

void TASIA::setBeta(double aValue)
{
    beta = aValue;
    PT1Analog.G = alpha/beta;
    PT1Analog.t1 = 1 / beta;
}

void TASIA::setDelta(double aValue)
{
    PT1Analog.delta = aValue;
}

void TASIA::setX1(double aValue)
{
    PT1Analog.x1 = aValue;
}

double TASIA::getX1()
{
    return PT1Analog.x1;
}

double TASIA::simAndGetOutput()
{
    simulate();
    return fOutput;
}

void TASIA::simulate()
{
    assert(beta != 0);
    PT1Analog.input = input;
    fOutput = PT1Analog.simAndGetOutput();
}

TMiMe::TMiMe()
{
    D = 1;
}

double TMiMe::simAndGetOutput()
{
    simulate();
    return fOutput;
}

void TMiMe::simulate()
{
    assert(input != 0);
    fOutput = G * input / (D + input);
}

double TNoCoDI::simAndGetOutput()
{
    simulate();
    return fOutput;
}

void TNoCoDI::simulate()
{
    assert(input2 != -1);
    fOutput = input1 / (1 + input2);
}

TNoCoDI::TNoCoDI()
{
    
}

