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

ShowTimeSeries <- TRUE;
iterations <- 30;

if (length(argus) > 1)
{
  if(length(grep("^-b$", argus) > 0) | length(grep("^--benchmark", argus)) > 0)
  ShowTimeSeries <- FALSE;
  
  if(length(grep("^-i$", argus) > 0))
  iterations <- as.numeric(argus[grep("^-i$", argus) + 1]);
  
  if(length(grep("^--iterations", argus)) > 0)
  iterations <- as.numeric(unlist(strsplit(argus[grep("^--iterations", argus)], "="))[2]);
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

cat("\n")
cat("CyberUnits Bricks demo application\n")
cat("Linear 1st order feedback demo (R version)\n")
cat("\n")

x <- 5
z <- 2.0

G1 <- 1.3
G2 <- 0.5

cat("Iterations: ")
cat(iterations)
cat("\n")

startTime <- Sys.time();

val.mat <- matrix(rep(NA, time = 7), nrow = iterations, ncol = 7);
colnames(val.mat) <- c("i", "x", "z", "e", "y", "yr", "ys");
Values <- data.frame(val.mat);
Values$i <- 1:iterations;

Prediction <- list(x = x, z = z, e = NA, y = NA, yr = NA, ys = NA);
Prediction$y <- (G1 * x + z) / (1 + G1 * G2);
Prediction$yr <- G2 * Prediction$y;
Prediction$e <- Prediction$x - Prediction$yr;
Prediction$ys <- G1 * Prediction$e;

Blocks <- list(G1 = NA, G2 = NA, PT1 = NA, Comparator = NA, LoadInjection = NA);
Blocks$G1 <- new("TP");
Blocks$G2 <- new("TP");
Blocks$PT1 <- new("TPT1")
Blocks$Comparator <- new("TPSub");
Blocks$LoadInjection <- new("TPAdd");

Blocks$G1@G <- G1;
Blocks$G2@G <- G2;
Blocks$PT1@G <- 1;
Blocks$PT1@t1 <- 5;
Blocks$PT1@delta <- 1;
Blocks$PT1@x1 <- 0;
yr <- 20;

for (i in 1:iterations)
{
  Blocks$Comparator@input1 <- x;
  Blocks$Comparator@input2 <- yr;
  e <- simOutput(Blocks$Comparator);
  Blocks$G1@input <- e;
  ys <- simOutput(Blocks$G1);
  Blocks$LoadInjection@input1 <- ys;
  Blocks$LoadInjection@input2 <- z;
  yz <- simOutput(Blocks$LoadInjection);
  Blocks$PT1@input <- yz;
  res <- simOutput(Blocks$PT1);
  y <- Blocks$PT1@x1 <- res$x1;
  Blocks$G2@input <- y;
  yr <- simOutput(Blocks$G2);
  Values$x[i] <- x;
  Values$z[i] <- z;
  Values$e[i] <- e;
  Values$y[i] <- y;
  Values$yr[i] <- yr;
  Values$ys[i] <- ys;
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