// CyberUnits

// Object Pascal, C++, S, Python and Swift units for computational cybernetics

// Bricks: Basic blocks for information processing structures

// Version 2.1.0 (Foudre)

// (c) Johannes W. Dietrich, 1994 - 2023
// (c) Ludwig Maximilian University of Munich 1995 - 2002
// (c) University of Ulm Hospitals 2002 - 2004
// (c) Ruhr University of Bochum 2005 - 2023

// C++ version of simulation program for purposes of benchmarking

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
#ifndef bricks_hpp
#include "bricks.hpp"
#endif

/* For testing only:
#ifndef unittests
#include "test.cpp"
#endif
*/

bool ShowTimeSeries = true;
long iterations = 30;
double deltaTime;

class TValues
{
public:
    std::vector<int> i;
    std::vector<double> x;
    std::vector<double> z;
    std::vector<double> e;
    std::vector<double> y;
    std::vector<double> yr;
    std::vector<double> ys;

    void resizeArrays(long newSize)
    {
        i.resize(newSize);
        x.resize(newSize);
        z.resize(newSize);
        e.resize(newSize);
        y.resize(newSize);
        yr.resize(newSize);
        ys.resize(newSize);
    };
};

struct Tprediction
{
    double x, z, e, y, yr, ys;
};

struct TBlocks
{
    TP G1, G2;
    TPT1 PT1;
    TPSub Comparator;
    TPAdd LoadInjection;
};

int main(int argc, const char * argv[])
{
    using namespace std::chrono;
    
    std::cout << "\n";
    std::cout << "CyberUnits Bricks demo application\n";
    std::cout << "Linear 1st order feedback demo (C++ version)";
    std::cout << "\n";

    #ifdef unittests
    testUnits();
    #endif
    
    if (argc > 1)
    {
        for (int i = 1; i < argc; i++)
        {
            if ((strcmp(argv[i], "-b") == 0) or (strcmp(argv[i], "--benchmark") == 0))
            {
                ShowTimeSeries = false;
            }
            if (strcmp(argv[i], "-i") == 0)
            {
                if ((argc - 1) > i)
                {
                    iterations = stoi(argv[i+1]);
                } else
                {
                    std::cout << "Number of iterations missing.";
                    std::cerr << "Number of iterations missing.";
                }
            }
        }
    }

    int x = 5;
    double z = 2.0;

    double G1 = 1.3;
    double G2 = 0.5;
    
    std::cout << "Iterations: " << iterations << "\n";

    uint64_t startTime = duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();
        
    Tprediction prediction;
    
    prediction.x = x;
    prediction.z = z;
    prediction.y = (G1 * x + z) / (1 + G1 * G2);
    prediction.yr = G2 * prediction.y;
    prediction.e = prediction.x - prediction.yr;
    prediction.ys = G1 * prediction.e;
    
    TValues gValues;
    TBlocks gBlocks;
    
    gValues.resizeArrays(iterations);
    
    gBlocks.G1.G = G1;
    gBlocks.G2.G = G2;
    gBlocks.Comparator.G = 1;
    gBlocks.LoadInjection.G = 1;
    gBlocks.PT1.G = 1;
    gBlocks.PT1.t1 = 5;
    gBlocks.PT1.delta = 1;
    gBlocks.PT1.x1 = 0;
    
    double yr = 20;
    
    double e, ys, yz, y;
    
    for (int i = 0; i < iterations; i++)
    {
        gBlocks.Comparator.input1 = x;
        gBlocks.Comparator.input2 = yr;
        e = gBlocks.Comparator.simAndGetOutput();
        gBlocks.G1.input = e;
        ys = gBlocks.G1.simAndGetOutput();
        gBlocks.LoadInjection.input1 = ys;
        gBlocks.LoadInjection.input2 = z;
        yz = gBlocks.LoadInjection.simAndGetOutput();
        gBlocks.PT1.input = yz;
        y = gBlocks.PT1.simAndGetOutput();
        gBlocks.G2.input = y;
        yr = gBlocks.G2.simAndGetOutput();

        gValues.i.at(i) = i;
        gValues.x.at(i) = x;
        gValues.z.at(i) = z;
        gValues.e.at(i) = e;
        gValues.y.at(i) = y;
        gValues.yr.at(i) = yr;
        gValues.ys.at(i) = ys;
    }
    
    if (ShowTimeSeries)
    {
        std::cout << "i\tx\tz\te\ty\tyr\tys\n";
        for (int i = 0; i < iterations; i++)
        {
            std::cout << gValues.i[i] << "\t" << gValues.x[i] << "\t" << gValues.z[i] << "\t"
            << gValues.e[i] << "\t" << gValues.y[i] << "\t"
            << gValues.yr[i] << "\t" << gValues.ys[i] << "\t\n";
        }
    }

    uint64_t stopTime = duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();

    deltaTime = double(stopTime - startTime) / 1000;
    
    std::cout << "\nStart time: " << startTime << "\n";
    std::cout << "Stop time: " << stopTime << "\n";
    std::cout << "Elapsed time for simulation: " << deltaTime << " ms\n";
}
