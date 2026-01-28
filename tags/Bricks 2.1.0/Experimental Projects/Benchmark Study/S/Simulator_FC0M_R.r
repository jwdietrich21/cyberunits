#!/usr/bin/env -S Rscript --vanilla

# CyberUnits

# Object Pascal, S and MATLAB units for computational cybernetics

# Bricks: Basic blocks for information processing structures

# Version 2.1.0 (Foudre)

# (c) Johannes W. Dietrich, 1994 - 2023
# (c) Ludwig Maximilian University of Munich 1995 - 2002
# (c) University of Ulm Hospitals 2002 - 2004
# (c) Ruhr University of Bochum 2005 - 2023

# R version of simulation program for purposes of benchmarking

# Source code released under the BSD License

# See the file "license.txt", included in this distribution,
# for details about the copyright.
# Current versions and additional information are available from
# http://cyberunits.sf.net

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

argus <- commandArgs(FALSE);

ShowTimeSeries <- TRUE;  # default
iterations <- 30;        # default

if (length(argus) > 1)
{
  if(length(grep("^-b$", argus) > 0) | length(grep("^--benchmark", argus)) > 0)
  ShowTimeSeries <- FALSE;
  
  if(length(grep("^-i$", argus) > 0))
  iterations <- as.numeric(argus[grep("^-i$", argus) + 1]);
  
  if(length(grep("^--iterations", argus)) > 0)
  iterations <- as.numeric(unlist(strsplit(argus[grep("^--iterations", argus)], "="))[2]);
  
  if(length(grep("^--file", argus)) > 0)
  {
    base.path <- dirname(unlist(strsplit(argus[grep("^--file", argus)], "="))[2]);
    owd <- getwd();
    setwd(base.path);
  }
}

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
source("lifeblocks.s");

cat("\n");
cat("CyberUnits Bricks demo application\n");
cat("MiMe NoCoDI loop demo (CLI version)\n");
cat("\n");

x <- 5;

G1 <- 2.6;
G2 <- 5.0;
G3 <- 0.3;
D2 <- 0.5;

cat("Iterations: ");
cat(iterations);
cat("\n");

startTime <- Sys.time();

val.mat <- matrix(rep(NA, time = 6), nrow = iterations, ncol = 6);
colnames(val.mat) <- c("i", "x", "e", "c", "y", "yr");
Values <- data.frame(val.mat);
Values$i <- 1:iterations;

Prediction1 <- list(x = x, e = NA, c = NA, y = NA, yr = NA);
Prediction2 <- list(x = x, e = NA, c = NA, y = NA, yr = NA);

a <- D2 * G3;
b <- D2 + G1 * Prediction1$x;
d <- -G1 * G2 * Prediction1$x;
Prediction1$y <- -(b + sqrt(b * b - 4 * a * d)) / (2 * a);
Prediction1$yr <- G3 * Prediction1$y;
Prediction1$e <- Prediction1$x / (1 + Prediction1$yr);
Prediction1$c <- G1 * Prediction1$e;
Prediction2$y <- -(b - sqrt(b * b - 4 * a * d)) / (2 * a);
Prediction2$yr <- G3 * Prediction2$y;
Prediction2$e <- Prediction2$x / (1 + Prediction2$yr);
Prediction2$c <- G1 * Prediction2$e;

Blocks <- list(G1 = NA, G2 = NA, MiMe = NA, D2 = NA);
Blocks$G1 <- new("TP");
Blocks$G3 <- new("TP");
Blocks$MiMe <- new("TMiMe")
Blocks$NoCoDI <- new("TNoCoDI");

Blocks$G1@G <- G1;
Blocks$G3@G <- G3;
Blocks$MiMe@G <- G2;
Blocks$MiMe@D <- D2;
yr <- 20;

for (i in 1:iterations)
{
  Blocks$NoCoDI@input1 <- x;
  Blocks$NoCoDI@input2 <- yr;
  e <- simOutput(Blocks$NoCoDI);
  Blocks$G1@input <- e;
  c <- simOutput(Blocks$G1);
  Blocks$MiMe@input <- c;
  y <- simOutput(Blocks$MiMe);
  Blocks$G3@input <- y;
  yr <- simOutput(Blocks$G3);
  Values$x[i] <- x;
  Values$e[i] <- e;
  Values$c[i] <- c;
  Values$y[i] <- y;
  Values$yr[i] <- yr;
}

if (ShowTimeSeries) print(Values);

stopTime <- Sys.time();

deltaTime <- as.numeric(stopTime - startTime) * 1000; # ms

cat("\nStart time: ");
cat(paste(startTime, "\n"));

cat("Stop time: ");
cat(paste(stopTime, "\n"));

cat("Elapsed time for simulation: ");
cat(paste(deltaTime, "ms\n"));

if (sys.nframe() > 0) setwd(owd);