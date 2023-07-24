# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# Bricks: Basic blocks for information processing structures

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

library(methods);

TestEnvironment <- FALSE; # set to TRUE to enable some basic unit tests

kError101 <- "Runtime error: Negative parameter(s)";
kError102 <- "Runtime error: Parameter(s) out of range";
kError103 <- "Runtime error: min > max";
kError104 <- "Runtime error: max = 0";
kError105 <- "Runtime error: Denominator is zero";

setClass("TVector", representation(values = "numeric"));
setClass("TMatrix", representation(values = "matrix"));
setClass("TFR", representation(M = "numeric", phi = "numeric", F = "complex"));

setClass("TModel", representation(delta = "numeric", time = "numeric"));

setMethod("initialize", "TModel",
  function(.Object, ...) {
    .Object@delta <- 1
    .Object@time <- 0
    .Object@firstBlock <- NULL
    .Object
  }
)

setGeneric("Reset", function(object) standardGeneric("Reset"))
setMethod("Reset", "TModel",
  function(object) {
    object@time <- 0
    object
  }
)

setClass("TBlock", representation = representation(name = "character", model = "TModel",  fOutput = "numeric", fFR = "TFR"), contains = "VIRTUAL");

setClass("TControlledBlock", representation = representation(input = "numeric", G = "numeric", amplitude = "numeric", omega = "numeric"), contains = "TBlock");

setMethod("initialize", "TControlledBlock",
  function(.Object, ...) {
    .Object@G <- 1;
    .Object@fOutput <- 0;
    return(.Object);
  }
)

setClass("TInvertibleBlock", representation = representation(input1 = "numeric", input2 = "numeric", G = "numeric"), contains = "TBlock");

setMethod("initialize", "TInvertibleBlock",
  function(.Object, ...) {
    .Object@G <- 1;
    .Object@fOutput <- 0;
    return(.Object);
  }
)
setClass("TTestSignal", contains = "TBlock", representation(simOutput = "numeric"));

setClass("TTHarmonic", contains = "TTestSignal", representation(G = "numeric", omega = "numeric", phi = "numeric", delta = "numeric", updateTime = "logical"));

setGeneric(name="output", def = function(theObject)
  {
    return(standardGeneric("output"));
  }
)

setGeneric(name="simulate", def = function(theObject)
  {
    return(standardGeneric("simulate"));
  }
)

setGeneric(name="simOutput", def = function(theObject)
  {
    return(standardGeneric("simOutput"));
  }
)

setGeneric(name="GetFR", def = function(theObject)
  {
    return(standardGeneric("GetFR"));
  }
)

setGeneric("fOutput", function(theObject) standardGeneric("fOutput"));
setGeneric("fOutput<-", function(theObject, value) standardGeneric("fOutput<-"));
setMethod(f = "fOutput", "TBlock", definition = function(theObject)
{
  return(theObject@fOutput);
})
setMethod(f = "fOutput<-", "TBlock", definition = function(theObject, value) 
{
  theObject@fOutput <- value;
  validObject(theObject);
  return(theObject);
})

setClass("TP", contains = "TControlledBlock");
setMethod(f = "output", signature(theObject = "TP"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);
setMethod(f = "simulate", signature(theObject = "TP"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    theObject@fOutput <- theObject@input * theObject@G;
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TP"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);
setMethod(f = "GetFR", signature(theObject = "TP"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    theObject@fFR@M <- theObject@amplitude * theObject@G;
    theObject@fFR@phi <- 0;
    return(theObject@fFR);
  }
);

setClass("TPAdd", contains = "TInvertibleBlock")
setMethod(f = "output", signature(theObject = "TPAdd"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);
setMethod(f = "simulate", signature(theObject = "TPAdd"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    theObject@fOutput <- theObject@G * (theObject@input1 + theObject@input2);
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TPAdd"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);

setClass("TPSub", contains = "TInvertibleBlock")
setMethod(f = "output", signature(theObject = "TPSub"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);
setMethod(f = "simulate", signature(theObject = "TPSub"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    theObject@fOutput <- theObject@G * (theObject@input1 - theObject@input2);
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TPSub"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);

setClass("TPT1", representation = representation(t1 = "numeric", x1 = "numeric", delta = "numeric"), contains = "TControlledBlock")
setMethod(f = "output", signature(theObject = "TPT1"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);
setMethod(f = "simulate", signature(theObject = "TPT1"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    if (theObject@t1 < 0) stop(kError101);
    
    f <- exp(-theObject@delta / theObject@t1);
    theObject@fOutput <- f * theObject@x1 + theObject@G * (1 - f) * theObject@input;
    theObject@x1 <- theObject@fOutput;
    return(list(output = theObject@fOutput, x1 = theObject@x1));
  }
);
setMethod(f = "simOutput", signature(theObject = "TPT1"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);

CheckEqual <- function(expected, received)
{
  if (length(received) == length(expected))
    {
      if (received == expected)
        cat("passed\n") else
        {
          cat("failed\n")
          warning("failed\n");
        }
    } else
    {
      cat("failed\n")
      warning ("failed\n");
    }
}

CheckTrue <- function(received)
{
  if (is.logical(received))
    {
      if (received == TRUE)
        cat("passed\n") else
        {
          cat("failed\n")
          warning("failed\n");
        }
    } else
    {
      cat("failed\n")
      warning ("failed\n");
    }
}

if(TestEnvironment)
{
  # TP:
  
  testBrick <- new("TP");
  testBrick@G <- 5;
  testBrick@input <- 2;
  cat("Test #1 for TP element:   ");
  res <- simulate(testBrick);
  CheckEqual(5 * 2, res);
  cat("Test #2 for TP element:   ");
  res <- simOutput(testBrick);
  CheckEqual(5 * 2, res);
  testBrick@amplitude <- 2;
  cat("Test #3 for TP element:   ");
  res <- GetFR(testBrick);
  CheckEqual(5 * 2, res@M);
  
  # TPAdd:
  
  testBrick <- new("TPAdd");
  testBrick@input1 <- 2;
  testBrick@input2 <- 11;
  cat("Test for TPAdd element:   ");
  res <- simOutput(testBrick);
  CheckEqual(13, res);

  # TPSub:
  
  testBrick <- new("TPSub");
  testBrick@input1 <- 107;
  testBrick@input2 <- 11;
  cat("Test for TPSub element:   ");
  res <- simOutput(testBrick);
  CheckEqual(96, res);
  
  # TPT1:
  
  testBrick <- new("TPT1");
  testBrick@G <- 5;
  testBrick@delta <- 10;
  testBrick@t1 <- 15;
  testBrick@input <- 2;
  testBrick@x1 <- 0;
  cat("Test #1 for TPT1 element: ");
  CheckEqual(0, output(testBrick));
  cat("Test #2 for TPT1 element: ");
  for (i in 1:100000)
  {
    res <- simOutput(testBrick);
    testBrick@x1 <- res$x1;
  }
  CheckTrue(testBrick@G * testBrick@input - testBrick@x1 < 1e-13);
  
  testBrick@G <- 5;
  testBrick@delta <- 1;
  testBrick@t1 <- 15e6;
  testBrick@input <- 2;
  testBrick@x1 <- 0;
  cat("Test #3 for TPT1 element: ");
  CheckEqual(0, output(testBrick));
  cat("Test #4 for TPT1 element: ");
  for (i in 1:100000)
  {
    res <- simOutput(testBrick);
    testBrick@x1 <- res$x1;
  }
  CheckTrue(testBrick@x1 < 0.07);

}

# References:  
#
# 1. Röhler, R., "Biologische Kybernetik", B. G. Teubner, Stuttgart 1973 
#
# 2. Neuber, H., "Simulation von Regelkreisen auf Personal Computern  
#    in Pascal und Fortran 77", IWT, Vaterstetten 1989 
#
# 3. Lutz H. and Wendt, W., "Taschenbuch der Regelungstechnik" 
#    Verlag Harri Deutsch, Frankfurt am Main 2005 