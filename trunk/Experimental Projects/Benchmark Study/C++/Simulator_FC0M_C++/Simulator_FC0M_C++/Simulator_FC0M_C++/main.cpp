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

#ifndef lifeblocks_hpp
#include "lifeblocks.hpp"
#endif

/* For testing only
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
    std::vector<double> e;
    std::vector<double> c;
    std::vector<double> y;
    std::vector<double> yr;

    void resizeArrays(long newSize)
    {
        i.resize(newSize);
        x.resize(newSize);
        e.resize(newSize);
        c.resize(newSize);
        y.resize(newSize);
        yr.resize(newSize);
    };
};

struct Tprediction
{
    double x, e, c, y, yr;
};

struct TBlocks
{
    TP G1, G3;
    TMiMe MiMe;
    TNoCoDI NoCoDI;
};

int main(int argc, const char * argv[])
{
    using namespace std::chrono;
    
    std::cout << "\n";
    std::cout << "CyberUnits Bricks demo application\n";
    std::cout << "MiMe NoCoDI loop demo (C++ version)";
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

    double G1 = 2.6;
    double G2 = 5.0;
    double G3 = 0.3;
    double D2 = 0.5;
    
    std::cout << "Iterations: " << iterations << "\n";

    uint64_t startTime = duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();
        
    Tprediction prediction1, prediction2;
    
    prediction1.x = x;
    prediction2.x = x;
    const double a = D2 * G3;
    const double b = D2 + G1 * prediction1.x;
    const double d = -G1 * G2 * prediction1.x;
    
    prediction1.y = -(b + sqrt(b * b - 4 * a * d)) / (2 * a);
    prediction1.yr = G3 * prediction1.y;
    prediction1.e = prediction1.x / (1 + prediction1.yr);
    prediction1.c = G1 * prediction1.e;
    prediction2.y = -(b - sqrt(b * b - 4 * a * d)) / (2 * a);
    prediction2.yr = G3 * prediction2.y;
    prediction2.e = prediction2.x / (1 + prediction2.yr);
    prediction1.c = G1 * prediction2.e;

    TValues gValues;
    TBlocks gBlocks;
    
    gValues.resizeArrays(iterations);
    
    gBlocks.G1.G = G1;
    gBlocks.G3.G = G3;
    gBlocks.MiMe.G = G2;
    gBlocks.MiMe.D = D2;
    double yr = 20;
    
    double e, c, y;
    
    for (int i = 0; i < iterations; i++)
    {
        gBlocks.NoCoDI.input1 = x;
        gBlocks.NoCoDI.input2 = yr;
        e = gBlocks.NoCoDI.simAndGetOutput();
        gBlocks.G1.input = e;
        c = gBlocks.G1.simAndGetOutput();
        gBlocks.MiMe.input = c;
        y = gBlocks.MiMe.simAndGetOutput();
        gBlocks.G3.input = y;
        yr = gBlocks.G3.simAndGetOutput();

        gValues.i.at(i) = i;
        gValues.x.at(i) = x;
        gValues.e.at(i) = e;
        gValues.c.at(i) = c;
        gValues.y.at(i) = y;
        gValues.yr.at(i) = yr;
    }
    
    if (ShowTimeSeries)
    {
        std::cout << "i\tx\te\tc\ty\tyr\n";
        for (int i = 0; i < iterations; i++)
        {
            std::cout << gValues.i[i] << "\t" << gValues.x[i] << "\t" << gValues.e[i] << "\t"
            << gValues.c[i] << "\t" << gValues.y[i] << "\t"
            << gValues.yr[i] << "\t" << "\t\n";
        }
    }

    uint64_t stopTime = duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();

    deltaTime = double(stopTime - startTime) / 1000;
    
    std::cout << "\nStart time: " << startTime << "\n";
    std::cout << "Stop time: " << stopTime << "\n";
    std::cout << "Elapsed time for simulation: " << deltaTime << " ms\n";
     
}
