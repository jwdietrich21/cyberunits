unit SimulationEngine;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Simulator for MiMe-NoCoDI loop }
{ Simulation Engine }

{ Version 1.0.0 (Corvus) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ Standard blocks for systems modelling and simulation }

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
  Classes, SysUtils, Forms, bricks, lifeblocks;

type

  { TValues }

  TValues = class
  protected
    function GetSize: integer;
    procedure SetSize(aValue: integer);
  public
    x, e, c, y, yr: array of extended;
    constructor Create;
    destructor Destroy;
    property size: integer read GetSize write SetSize;
  end;

  TBlocks = record
    G1, G2, G3: TP;
    MiMe: TMiMe;
    NoCoDI: TNoCoDI;
  end;

  TPrediction = record
    x, e, c, y, yr: extended;
  end;

var
  gValues: TValues;
  gBlocks: TBlocks;
  gPrediction: TPrediction;

procedure RunSimulation(x, G1, G2, G3, D2: extended; nmax: integer);

implementation

procedure RunSimulation(x, G1, G2, G3, D2: extended; nmax: integer);
var
  e, c, y, yr: extended;
  a, b, d: extended;
  i: integer;
begin
  if nmax > 0 then
  begin
    gPrediction.x := x;

    { Solving for c: }
    // a := 1 + G2 * G3;
    // b := D2 - G1 * gPrediction.x;
    // d := -G1 * D2 * gPrediction.x;
    // gPrediction.c := -(b - sqrt(sqr(b) - 4 * a * d)) / (2 * a);
    // gPrediction.y := G2 * gPrediction.c / (D2 + gPrediction.c);
    // gPrediction.yr := G3 * gPrediction.y;
    // gPrediction.e := gPrediction.x / (1 + gPrediction.yr); }

    { Solving for y: }
    a := D2 * G3;
    b := D2 + G1 * gPrediction.x;
    d := -G1 * G2 * gPrediction.x;
    gPrediction.y := -(b - sqrt(sqr(b) - 4 * a * d)) / (2 * a);
    gPrediction.yr := G3 * gPrediction.y;
    gPrediction.e := gPrediction.x / (1 + gPrediction.yr);
    gPrediction.c := G1 * gPrediction.e;

    gValues.size := 0; // delete content
    gValues.size := nmax;
    gBlocks.G1 := TP.Create;
    gBlocks.G3 := TP.Create;
    gBlocks.MiMe := TMiMe.Create;
    gBlocks.NoCoDI := TNoCoDI.Create;
    gBlocks.G1.G := G1;
    gBlocks.G3.G := G3;
    gBlocks.MiMe.G := G2;
    gBlocks.MiMe.D := D2;
    yr := 20;
    for i := 0 to nmax - 1 do
    begin
      gBlocks.NoCoDI.input1 := x;
      gBlocks.NoCoDI.input2 := yr;
      e := gBlocks.NoCoDI.simOutput;
      gBlocks.G1.input := e;
      c := gBlocks.G1.simOutput;
      gBlocks.MiMe.input := c;
      y := gBlocks.MiMe.simOutput;
      gBlocks.G3.input := y;
      yr := gBlocks.G3.simOutput;
      gValues.x[i] := x;
      gValues.e[i] := e;
      gValues.c[i] := c;
      gValues.y[i] := y;
      gValues.yr[i] := yr;
      application.ProcessMessages;
    end;
    gBlocks.G1.Destroy;
    gBlocks.G3.Destroy;
    gBlocks.MiMe.Destroy;
    gBlocks.NoCoDI.Destroy;
  end;
end;

{ TValues }

function TValues.GetSize: integer;
begin
  result := Length(x);
end;

procedure TValues.SetSize(aValue: integer);
begin
  SetLength(x, aValue);
  SetLength(e, aValue);
  SetLength(c, aValue);
  SetLength(y, aValue);
  SetLength(yr, aValue);
end;

constructor TValues.Create;
begin
  inherited Create;
end;

destructor TValues.Destroy;
begin
  inherited Destroy;
end;

end.

