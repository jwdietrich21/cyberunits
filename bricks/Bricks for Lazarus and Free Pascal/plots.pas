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
  Classes, SysUtils, TASources, TAGraph, TASeries, Bricks,
  Dialogs;

procedure SimBodePlot(aBrick: TControlledBlock; AmpSeries,
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended;
  var omega, M, phi: TVector);

procedure DrawBodePlot(aBrick: TControlledBlock; AmpSeries, PhaseSeries: TLineSeries;
  minFreq, maxFreq: extended; var omega, M, phi: TVector);

implementation

procedure SimBodePlot(aBrick: TControlledBlock; AmpSeries,
  PhaseSeries: TLineSeries; minFreq, maxFreq: extended;
  var omega, M, phi: TVector);
{ Draws extimated bode plot via simulation }
const
  RESOLUTION = 13;
  TESTLENGTH = 100;
var
  x, diff: extended;
  y: array[0..TESTLENGTH] of extended;
  tMax, yMax, lasty: extended;
  i, t: Integer;
  minI, maxI: integer;
  testSignal: TTHarmonic;
begin
  testSignal := TTHarmonic.Create;
  SetLength(omega, RESOLUTION + 1);
  SetLength(M, RESOLUTION + 1);
  SetLength(phi, RESOLUTION + 1);
  diff := maxFreq - minFreq;
  minI := trunc(minFreq * RESOLUTION);
  maxI := trunc(maxFreq * RESOLUTION / diff);
  for i := minI to maxI do
  begin
    omega[i] := i / RESOLUTION;
    testSignal.time := 0;
    testSignal.omega := omega[i];
    testSignal.delta := 0.01; // should not be higher to avoid aliasing
    testSignal.phi := pi / 2;
    testSignal.G := 1;
    for t := 0 to TESTLENGTH do
    begin
      x := testSignal.simOutput;
      if aBrick.ClassType = TPT1 then
      begin
        TPT1(aBrick).input := x;
        y[t] := TPT1(aBrick).simOutput;
      end;
    end;
    t := 0;
    yMax := y[t];
    tMax := 0;
    repeat
     lasty := y[t];
     t := t + 1;
    until (y[t] < lasty) or (t = TESTLENGTH);
    if t = TESTLENGTH then t := 0;
    M[i] := lasty;
    phi[i] := t - 1;
    AmpSeries.AddXY(omega[i], m[i]);
    PhaseSeries.AddXY(omega[i], phi[i]);
  end;
  testSignal.Destroy;
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

