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

#include "bricks.hpp"

#include <iostream>
#include <cmath>

void TModel::reset()
{
    time = 0;
}

double TTHarmonic::simAndGetOutput()
{
    simulate();
    return fOutput;
}

void TTHarmonic::simulate()
{
    if (model == nullptr)
        throw std::runtime_error(kError210);
    
    fOutput = (G + G * std::sin(omega * model->time + phi)) / 2;
    
    if (updateTime)
    {
        model->time += delta;
    }
}

TP::TP()
{
    G = 1;
    fOutput = 0;
}

double TP::simAndGetOutput()
{
    simulate();
    return fOutput;
}

TFR TP::getFR()
{
    if (G < 0)
        throw std::runtime_error(kError101);
    if (omega < 0)
        throw std::runtime_error(kError101);
    
    TFR fr;
    fr.M = amplitude * G;
    fr.phi = 0;
    return fr;
}

void TP::simulate()
{
    if (G < 0)
        throw std::runtime_error(kError101);
    
    fOutput = input * G;
}

double TPAdd::output()
{
    return fOutput;
}

void TPAdd::simulate()
{
    if (G < 0)
        throw std::runtime_error(kError101);
    
    fOutput = G * (input1 + input2);
}

double TPAdd::simAndGetOutput()
{
    simulate();
    return fOutput;
}

double TPSub::output()
{
    return fOutput;
}

void TPSub::simulate()
{
    if (G < 0)
        throw std::runtime_error(kError101);
    
    fOutput = G * (input1 - input2);
}

double TPSub::simAndGetOutput()
{
    simulate();
    return fOutput;
}

TPT1::TPT1()
{
    G = 1;
    x1 = 0;
    fOutput = 0;
}

double TPT1::output()
{
    return fOutput;
}

double TPT1::simAndGetOutput()
{
    simulate();
    return fOutput;
}

TFR TPT1::getFR()
{
    if (G < 0)
        throw std::runtime_error(kError101);
    if (t1 < 0)
        throw std::runtime_error(kError101);
    if (omega < 0)
        throw std::runtime_error(kError101);
    
    TFR fr;
    fr.M = amplitude * G / std::sqrt(1 + std::pow(omega, 2) * std::pow(t1, 2));
    fr.phi = -std::atan(omega * t1);
    return fr;
}

void TPT1::simulate()
{
    if (G < 0)
        throw std::runtime_error(kError101);
    if (t1 < 0)
        throw std::runtime_error(kError101);
    
    double f = std::exp(-delta / t1);
    fOutput = f * x1 + G * (1 - f) * input;
    x1 = fOutput;
}
