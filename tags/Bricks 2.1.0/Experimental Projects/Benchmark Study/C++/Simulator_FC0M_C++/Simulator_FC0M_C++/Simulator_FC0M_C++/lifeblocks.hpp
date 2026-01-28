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

#ifndef lifeblocks_hpp
#define lifeblocks_hpp

#include <stdio.h>

#endif /* lifeblocks_hpp */

#ifndef bricks_hpp
#include "bricks.hpp"
#endif

class TASIA: public TControlledBlock
{
    double alpha, beta;
public:
    TASIA();
    TPT1 PT1Analog;
    void setAlpha(double aValue);
    void setBeta(double aValue);
    void setDelta(double aValue);
    void setX1(double aValue);
    double getX1();
    double simAndGetOutput() override;
    void simulate() override;
};

class TMiMe: public TControlledBlock
{
public:
    TMiMe();
    double D;
    double simAndGetOutput() override;
    void simulate() override;
};

class TNoCoDI: public TInvertibleBlock
{
public:
    TNoCoDI();
    double simAndGetOutput() override;
    void simulate() override;
};
