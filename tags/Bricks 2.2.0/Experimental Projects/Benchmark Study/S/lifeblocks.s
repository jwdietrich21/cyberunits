# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# LifeBlocks: Metabricks for information processing structures in organisms

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

TestEnvironment <- TRUE; # set to TRUE to enable some basic unit tests

if (TestEnvironment)
{
  if (sys.nframe() > 0)
  {
    if (Sys.info()[["sysname"]] == "Darwin" | Sys.info()[["machine"]] == "Macintosh" | Sys.info()[["machine"]] == "Power Macintosh")
     {
       this.frame <- sys.frame(sys.nframe());
       base.path <- dirname(sys.frame(1)$ofile);
     } else base.path <- choose.dir(getwd());
     owd <- getwd();
     setwd(base.path);
  }
  source("bricks.s");
}

setGeneric(name="alpha", function(theObject)
  {
    return(standardGeneric("alpha"));
  }
)

setGeneric("alpha<-", function(theObject, value)
  {
    return(standardGeneric("alpha<-"));
  }
)

setGeneric(name="beta", function(theObject)
  {
    return(standardGeneric("beta"));
  }
)

setGeneric("beta<-", function(theObject, value)
  {
    return(standardGeneric("beta<-"));
  }
)

setGeneric(name="delta", function(theObject)
  {
    return(standardGeneric("delta"));
  }
)

setGeneric("delta<-", function(theObject, value)
  {
    return(standardGeneric("delta<-"));
  }
)

setClass("TASIA", slots = c(alpha = "numeric", beta = "numeric", x1 = "numeric", delta = "numeric", PT1Analog = "TPT1"), contains = "TControlledBlock");

setMethod("alpha", "TASIA", definition = function(theObject) 
  {
    return(theObject@alpha);
  }
);

setMethod("alpha<-", "TASIA", definition = function(theObject, value) 
  {
    theObject@alpha <- value;
    if (length(theObject@beta) == 0)
      {
        theObject@G <- 0;
      } else
      {
        if (theObject@beta == 0)
          {
            theObject@G <- 0;
          } else
          {
            theObject@G <- theObject@alpha / theObject@beta;
          }
      }
    theObject@PT1Analog@G = theObject@G;
    validObject(theObject);
    return(theObject);
  }
);

setMethod("beta", "TASIA", definition = function(theObject) 
  {
    return(theObject@beta);
  }
);

setMethod("beta<-", "TASIA", definition = function(theObject, value) 
  {
    theObject@beta <- value;
    if (length(theObject@beta) == 0)
      {
        theObject@G <- 0
        theObject@PT1Analog@t1 <- NA;
      } else
      {
        if (theObject@beta == 0)
          {
            theObject@G <- 0;
            theObject@PT1Analog@t1 <- NA;
          } else
          {
            theObject@G <- theObject@alpha / theObject@beta;
            theObject@PT1Analog@t1 <- 1 / theObject@beta;
          }
      }
    theObject@PT1Analog@G = theObject@G;
    validObject(theObject);
    return(theObject);
  }
);

setMethod("delta", "TASIA", definition = function(theObject) 
  {
    return(theObject@delta);
  }
);

setMethod("delta<-", "TASIA", definition = function(theObject, value) 
  {
    theObject@delta <- value;
    theObject@PT1Analog@delta = theObject@delta;
    validObject(theObject);
    return(theObject);
  }
);

setMethod(f = "output", signature(theObject = "TASIA"), definition = function(theObject) 
  {
    return(theObject@fOutput);
  }
);
setMethod(f = "simulate", signature(theObject = "TASIA"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    if (theObject@beta == 0) stop(kError102);
    theObject@PT1Analog@input <- theObject@input;
    res <- simOutput(theObject@PT1Analog);
    theObject@fOutput <- res$output;
    theObject@PT1Analog@x1 <- res$x1;
    theObject@x1 <- theObject@PT1Analog@x1;
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TASIA"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);


setClass("TMiMe", slots = c(G = "numeric", D = "numeric"), contains = "TControlledBlock");
setMethod(f = "output", signature(theObject = "TMiMe"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);

setMethod(f = "simulate", signature(theObject = "TMiMe"), definition = function(theObject) 
  {
    if (theObject@G < 0) stop(kError101);
    if (-theObject@D == theObject@input) stop(kError102);
    theObject@fOutput <- theObject@G * theObject@input / (theObject@D + theObject@input);
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TMiMe"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);


setClass("TNoCoDI", slots = c(input1 = "numeric", input2 = "numeric"), contains = "TInvertibleBlock");
setMethod(f = "output", signature(theObject = "TNoCoDI"), definition = function(theObject) 
  {
    theObject@fOutput;
  }
);

setMethod(f = "simulate", signature(theObject = "TNoCoDI"), definition = function(theObject) 
  {
    if (theObject@input2 == -1) stop(kError101);
    theObject@fOutput <- theObject@input1 / (1 + theObject@input2);
    return(theObject@fOutput);
  }
);
setMethod(f = "simOutput", signature(theObject = "TNoCoDI"), definition = function(theObject) 
  {
    resl <- simulate(theObject);
    return(resl);
  }
);


if (TestEnvironment)
{  
  # TASIA:
  
  alpha <- 10;
  beta <- 0.5;
  
  testBrick <- new("TASIA");
  cat("Test for TASIA element:   ");
  alpha(testBrick) <- alpha;
  beta(testBrick) <- beta;
  delta(testBrick) <- 1;
  testBrick@input <- 1;
  testBrick@x1 <- 0;
  testBrick@PT1Analog@x1 <- 0;
  for (i in 1:100)
    {
      res <- simOutput(testBrick);
      testBrick@PT1Analog@x1 <- res;
      testBrick@x1 <- testBrick@PT1Analog@x1
    }
  CheckTrue(alpha / beta - testBrick@x1 < 0.08);
  
  # TMiMe:
  
  G <- 5;
  D <- 2;
  testSignal <- 10;
  
  testBrick <- new("TMiMe");
  cat("Test for TMiMe element:   ");
  testBrick@G <- G;
  testBrick@D <- D;
  testBrick@input <- testSignal;
  res <- simOutput(testBrick);
  CheckEqual(G * testSignal / (D + testSignal), res);
  
  # TNoCoDI:
  
  xe1 <- 5;
  xe2 <- 4;
  
  testBrick <- new("TNoCoDI");
  cat("Test for TNoCoDI element: ");
  testBrick@input1 <- xe1;
  testBrick@input2 <- xe2;
  res <- simOutput(testBrick);
  CheckEqual(xe1 / (1 + xe2), res);

  setwd(owd);
}
