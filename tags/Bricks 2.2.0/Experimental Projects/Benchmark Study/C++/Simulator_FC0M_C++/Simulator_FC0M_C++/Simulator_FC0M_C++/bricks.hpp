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
#include <vector>
#include <cmath>
#include <cassert>

#ifndef bricks_hpp
#define bricks_hpp

#include <stdio.h>

#endif /* bricks_hpp */

using namespace std;

const std::string kError101 = "Runtime error: Negative parameter(s)";
const std::string kError102 = "Runtime error: Parameter(s) out of range";
const std::string kError103 = "Runtime error: min > max";
const std::string kError104 = "Runtime error: max = 0";
const std::string kError105 = "Runtime error: Denominator is zero";
const std::string kError210 = "Runtime error: Nil pointer";

class TModel;

struct TFR
{
    double M = 0.0;
    double phi = 0.0;
};

class TBlock
{
public:
    std::string name;
    double fOutput;
    TModel* model;
    TFR fr;

    virtual double simAndGetOutput()
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }

    virtual ~TBlock()
    {
        // Destructor
    }

    virtual void simulate()
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }
};

class TModel
{
public:
    double delta;
    double time;
    TBlock* firstBlock;

    void reset();
};

class TControlledBlock : public TBlock
{
public:
    double input = 0.0;
    double G = 0.0;
    double amplitude = 0.0;
    double omega = 0.0;

    double simAndGetOutput() override
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }

    virtual TFR getFR()
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }
};

class TInvertibleBlock : public TBlock
{
public:
    double input1 = 0.0;
    double input2 = 0.0;
    double G = 0.0;

    double simAndGetOutput() override
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }

    TFR getFR()
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }
};

class TTestSignal : public TBlock
{
public:
    double simAndGetOutput() override
    {
        throw std::runtime_error("Abstract method, should be overridden in subclasses.");
    }

    ~TTestSignal()
    {
        // Deinitialize test signal here
    }

    double simOutput()
    {
        return simAndGetOutput();
    }
};

class TTHarmonic : public TTestSignal
{
public:
    double G = 0.0;
    double omega = 0.0;
    double phi = 0.0;
    double delta = 0.0;
    bool updateTime = false;

    double simAndGetOutput() override;
    
    TTHarmonic() : delta(1) {}

    void simulate() override;
};

class TP : public TControlledBlock
{
public:
    TP();
    
    double simAndGetOutput() override;

    TFR getFR() override;

    void simulate() override;
};


class TPAdd : public TInvertibleBlock
{
public:
    double output();

    void simulate() override;

    double simAndGetOutput() override;
};

class TPSub : public TInvertibleBlock
{
public:
    double output();

    void simulate() override;

    double simAndGetOutput() override;
};

class TPT1 : public TControlledBlock
{
public:
    TPT1();
    
    double t1 = 0.0;
    double x1 = 0.0;
    double delta = 1;

    double output();

    double simAndGetOutput() override;

    TFR getFR() override;

    void simulate() override;
};
