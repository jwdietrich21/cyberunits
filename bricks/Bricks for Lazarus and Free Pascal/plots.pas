unit plots;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 1.1.0 (Corvus) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ Unit for plotting }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TAGraph, TASeries, Math, Bricks;

procedure SimBodePlot(aBrick: TControlledBlock; AmpSeries,
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended;
  var omega, M, phi: TVector; var inputSignal, outputSignal: TVector);

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; var omega, M, phi: TVector);

implementation

function FirstMaximum(timeSeries: tVector): Integer;
var
  t: Integer;
  tMax, lasty: extended;
begin
  t := 0;
  tMax := 0;
  repeat
  { search for rising values first }
   lasty := timeSeries[t];
   inc(t)
  until (timeSeries[t] > lasty) or (t = length(timeSeries));
  if t < length(timeSeries) then
  repeat
   { and then for sinking values }
   lasty := timeSeries[t];
   inc(t)
  until (timeSeries[t] < lasty) or (t = length(timeSeries));
  Result := t - 1;
end;

procedure SimBodePlot(aBrick: TControlledBlock; AmpSeries,
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended;
  var omega, M, phi: TVector; var inputSignal, outputSignal: TVector);
{ Draws extimated bode plot via simulation }
const
  RESOLUTION = 13;
  TESTLENGTH = 1000;
  INITLENGTH = 10000;
var
  y_init, diff: extended;
  x, y: TVector;
  lasty: extended;
  i, j, t, initRun: Integer;
  minI, maxI: integer;
  model: TModel;
  testSignal: TTHarmonic;
  InitBuffer: TVector;
begin
  fillchar(inputSignal, sizeOf(inputSignal), 0);
  fillchar(outputSignal, sizeOf(outputSignal), 0);
  model := TModel.Create;
  testSignal := TTHarmonic.Create;
  model.firstBlock := testSignal;
  testSignal.model := model;
  testSignal.delta := 0.1; // should not be higher to avoid aliasing
  testSignal.phi := pi / 2;
  testSignal.G := 1;
  testSignal.updateTime := true;
  SetLength(omega, RESOLUTION + 1);
  SetLength(M, RESOLUTION + 1);
  SetLength(phi, RESOLUTION + 1);
  SetLength(initBuffer, INITLENGTH + 1);
  SetLength(x, INITLENGTH + 1);
  SetLength(y, INITLENGTH + 1);
  diff := maxFreq - minFreq;
  minI := trunc(minFreq * RESOLUTION);
  maxI := trunc(maxFreq * RESOLUTION / diff);
  for i := minI to maxI do
  begin
    omega[i] := i / RESOLUTION;
    testSignal.omega := omega[i];
    model.time := 0;
    for initRun := 0 to INITLENGTH do
    { initial runs for settling the system to a new equilibrium }
    begin
      x[initRun] := testSignal.simOutput;
      if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[initRun];
        TPT1(aBrick).simulate;
      end;
    end;
    for initRun := 0 to INITLENGTH do
    { second initial run for finding the first maximum }
    begin
      x[initRun] := testSignal.simOutput;
      if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[initRun];
        InitBuffer[initRun] := TPT1(aBrick).simOutput;
      end;
    end;
    SetLength(x, TESTLENGTH);
    SetLength(y, TESTLENGTH);
    //fillchar(x, sizeOf(x), 0);
    //fillchar(y, sizeOf(y), 0);
    t := FirstMaximum(InitBuffer);
    if aBrick.ClassType = TPT1 then
      TPT1(aBrick).x1 := InitBuffer[t];
    //testSignal.time := 0;
    for t := 0 to TESTLENGTH do
    begin
      x[t] := testSignal.simOutput;
      if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[t];
        y[t] := TPT1(aBrick).simOutput;
      end;
    end;
    t := FirstMaximum(y);
    //if t = TESTLENGTH then t := 0;
    M[i] := y[i];
    phi[i] := t;
    AmpSeries.AddXY(omega[i], m[i]);
    PhaseSeries.AddXY(omega[i], phi[i]);
  end;
  testSignal.Destroy;
  model.Destroy;
  inputSignal := x;
  outputSignal := y;
end;

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; var omega, M, phi: TVector);
{ Draws exact bode plot from calculated frequency response }
const
  RESOLUTION = 13;
var
  diff: extended;
  i: Integer;
  minI, maxI: integer;
begin
  SetLength(omega, RESOLUTION + 1);
  SetLength(M, RESOLUTION + 1);
  SetLength(phi, RESOLUTION + 1);
  diff := maxFreq - minFreq;
  minI := trunc(minFreq * RESOLUTION);
  maxI := trunc(maxFreq * RESOLUTION / diff);
  for i := minI to maxI do
  begin
    omega[i] := i / RESOLUTION;
    if aBrick.ClassType = TPT0 then
    begin
      aBrick.omega := omega[i];
      M[i] := TPT0(aBrick).fr.M;
      phi[i] := TPT0(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TPT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TPT1(aBrick).fr.M;
      phi[i] := TPT1(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TDT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TDT1(aBrick).fr.M;
      phi[i] := TDT1(aBrick).fr.phi;
    end;
    AmpSeries.AddXY(omega[i], m[i]);
    PhaseSeries.AddXY(omega[i], phi[i]);
  end;
end;

end.

