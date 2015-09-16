unit plots;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 1.1.0 (Director) }

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
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended; resolution: integer;
  var omega, M, phi: TVector; var inputSignal, outputSignal: TMatrix);

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; resolution: integer; var omega, M, phi: TVector);

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
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended; resolution: integer;
  var omega, M, phi: TVector; var inputSignal, outputSignal: TMatrix);
{ Draws extimated bode plot via simulation }
const
  TESTLENGTH = 1000;
  INITLENGTH = 20000;
var
  diff: extended;
  x, y: TVector;
  i, j, t: Integer;
  minI, maxI: integer;
  model: TModel;
  testSignal: TTHarmonic;
begin
  model := TModel.Create;
  testSignal := TTHarmonic.Create;
  model.firstBlock := testSignal;
  testSignal.model := model;
  testSignal.delta := 0.1; // should not be higher to avoid aliasing
  testSignal.phi := pi / 2; // begin with maximum as in cosine function
  testSignal.G := aBrick.amplitude;
  testSignal.updateTime := true;
  SetLength(omega, resolution + 1);
  SetLength(M, resolution + 1);
  SetLength(phi, resolution + 1);
  SetLength(y, TESTLENGTH + 1);
  diff := maxFreq - minFreq;
  minI := 1;
  maxI := resolution;
  SetLength(inputSignal, maxI - minI + 2, TESTLENGTH + 1);
  SetLength(outputSignal, maxI - minI + 2, TESTLENGTH + 1);
  for i := 0 to maxI do
  begin
    SetLength(x, INITLENGTH + 1);
    model.Reset;
    omega[i] := MINFREQ + (i) * diff / resolution;
    testSignal.omega := omega[i];
    model.time := 0;
    for j := 0 to INITLENGTH do
    { initial runs for allowing the system to settle to a new equilibrium }
    begin
      x[j] := testSignal.simOutput;
      if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[j];
        TPT1(aBrick).simulate;
      end;
    end;
    SetLength(x, TESTLENGTH + 1);
    model.Reset;
    for j := 0 to TESTLENGTH do
    { simulation to deliver time series }
    begin
      x[j] := testSignal.simOutput;
      if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x[j];
        y[j] := TPT1(aBrick).simOutput;
      end;
    end;
    t := FirstMaximum(y);
    M[i] := y[t];
    phi[i] := t;
    AmpSeries.AddXY(omega[i], m[i]);
    PhaseSeries.AddXY(omega[i], phi[i]);
    inputSignal[i] := copy(x, 0, length(x));
    outputSignal[i] := copy(y, 0, length(y));
  end;
  testSignal.Destroy;
  model.Destroy;
end;

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; resolution: integer; var omega, M, phi: TVector);
{ Draws exact bode plot from calculated frequency response }
var
  diff: extended;
  i: Integer;
  minI, maxI: integer;
begin
  SetLength(omega, resolution + 1);
  SetLength(M, resolution + 1);
  SetLength(phi, resolution + 1);
  diff := maxFreq - minFreq;
  minI := 1;
  maxI := resolution;
  for i := 0 to maxI do
  begin
    omega[i] := MINFREQ + (i) * diff / resolution;
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
    else if aBrick.ClassType = TPT2 then
    begin
      aBrick.omega := omega[i];
      M[i] := TPT2(aBrick).fr.M;
      phi[i] := TPT2(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TDT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TDT1(aBrick).fr.M;
      phi[i] := TDT1(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TInt then
    begin
      aBrick.omega := omega[i];
      M[i] := TInt(aBrick).fr.M;
      phi[i] := TInt(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TIT1 then
    begin
      aBrick.omega := omega[i];
      M[i] := TIT1(aBrick).fr.M;
      phi[i] := TIT1(aBrick).fr.phi;
    end
    else if aBrick.ClassType = TIT2 then
    begin
      aBrick.omega := omega[i];
      M[i] := TIT2(aBrick).fr.M;
      phi[i] := TIT2(aBrick).fr.phi;
    end;
   AmpSeries.AddXY(omega[i], m[i]);
   PhaseSeries.AddXY(omega[i], phi[i]);
  end;
end;

end.

