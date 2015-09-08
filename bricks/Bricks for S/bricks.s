# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# Bricks: Basic blocks for information processing structures

# Version 1.1.0 (Corvus)

# (c) Johannes W. Dietrich, 1994 - 2015
# (c) Ludwig Maximilian University of Munich 1995 - 2002
# (c) University of Ulm Hospitals 2002-2004
# (c) Ruhr University of Bochum 2005 - 2015

# Standard blocks for systems modelling and simulation

# Source code released under the BSD License

# See the file "license.txt", included in this distribution,
# for details about the copyright.
# Current versions and additional information are available from
# http://cyberunits.sf.net

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

kError101 = "Runtime error: Negative parameter(s)";
kError102 = "Runtime error: Parameter(s) out of range";
kError103 = "Runtime error: min > max";
kError104 = "Runtime error: max = 0";
kError105 = "Runtime error: Denominator is zero";

setClass("TBlock", representation = representation(name = "character", fOutput = "numeric"), contains = "VIRTUAL");

setClass("TControlledBlock", representation = representation(input = "numeric", G = "numeric", amplitude = "numeric", omega = "numeric"), contains = "TBlock");

setClass("TInvertibleBlock", representation = representation(input1 = "numeric", input2 = "numeric", G = "numeric"), contains = "TBlock");

setGeneric(name="output", def = function(theObject)
  {
    standardGeneric("output")
  }
)

setGeneric(name="simulate", def = function(theObject)
  {
    standardGeneric("simulate")
  }
)

setGeneric(name="simOutput", def = function(theObject)
  {
    standardGeneric("simOutput")
  }
)

setClass("TP", contains = "TControlledBlock");
setMethod(f = "output", signature(theObject = "TP"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);
setMethod(f = "simulate", signature(theObject = "TP"), definition = function(theObject) 
  {
    theObject@fOutput <- theObject@input * theObject@G;
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TP"), definition = function(theObject) 
  {
    simulate(theObject);
    return(theObject@fOutput);
  }
);

#x <- new("TP");
#x@G <- 5;
#x@input <- 2;
#print(simulate(x));
#print(simOutput(x));



# References:  
#
# 1. Röhler, R., "Biologische Kybernetik", B. G. Teubner, Stuttgart 1973 
#
# 2. Neuber, H., "Simulation von Regelkreisen auf Personal Computern  
#    in Pascal und Fortran 77", IWT, Vaterstetten 1989 
#
# 3. Lutz H. and Wendt, W., "Taschenbuch der Regelungstechnik" 
#    Verlag Harri Deutsch, Frankfurt am Main 2005 