unit SimulationEngine;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
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
  Classes, SysUtils, Forms, bricks;

type

  { TValues }

  TValues = class
  protected
    function GetSize: integer;
    procedure SetSize(aValue: integer);
  public
    x, z, e, y, yr, ys: array of extended;
    constructor Create;
    destructor Destroy;
    property size: integer read GetSize write SetSize;
  end;

  TBlocks = record
    G1, G2: TP;
    PT1: TPT1;
    Comparator: TPSub;
    LoadInjection: TPAdd;
  end;

  TPrediction = record
    x, z, e, y, yr, ys: extended;
  end;

var
  gValues: TValues;
  gBlocks: TBlocks;
  gPrediction: TPrediction;

procedure RunSimulation(x, z, G1, G2: extended; nmax: integer);

implementation

procedure RunSimulation(x, z, G1, G2: extended; nmax: integer);
var
  e, y, yz, yr, ys: extended;
  i: integer;
begin
  if nmax > 0 then
  begin
    gPrediction.x := x;
    gPrediction.z := z;
    gPrediction.y := (G1 * x + z) / (1 + G1 * G2);
    gPrediction.yr := G2 * gPrediction.y;
    gPrediction.e := gPrediction.x - gPrediction.yr;
    gPrediction.ys := G1 * gPrediction.e;
    gValues.size := 0; // delete content
    gValues.size := nmax;
    gBlocks.G1 := TP.Create;
    gBlocks.G2 := TP.Create;
    gBlocks.PT1 := TPT1.Create;
    gBlocks.Comparator := TPSub.Create;
    gBlocks.LoadInjection := TPAdd.Create;
    gBlocks.G1.G := G1;
    gBlocks.G2.G := G2;
    gBlocks.PT1.G := 1;
    gBlocks.PT1.t1 := 5;
    gBlocks.PT1.delta := 1;
    yr := 20;
    for i := 0 to nmax - 1 do
    begin
      gBlocks.Comparator.input1 := x;
      gBlocks.Comparator.input2 := yr;
      e := gBlocks.Comparator.simOutput;
      gBlocks.G1.input := e;
      ys := gBlocks.G1.simOutput;
      gBlocks.LoadInjection.input1 := ys;
      gBlocks.LoadInjection.input2 := z;
      yz := gBlocks.LoadInjection.simOutput;
      gBlocks.PT1.input := yz;
      y := gBlocks.PT1.simOutput;
      gBlocks.G2.input := y;
      yr := gBlocks.G2.simOutput;
      gValues.x[i] := x;
      gValues.z[i] := z;
      gValues.e[i] := e;
      gValues.y[i] := y;
      gValues.yr[i] := yr;
      gValues.ys[i] := ys;
      application.ProcessMessages;
    end;
    gBlocks.G1.Destroy;
    gBlocks.G2.Destroy;
    gBlocks.PT1.Destroy;
    gBlocks.Comparator.Destroy;
    gBlocks.LoadInjection.Destroy;
  end;
end;

{ TValues }

function TValues.GetSize: integer;
begin
  result := Length(x);
end;

procedure TValues.SetSize(aValue: integer);
begin
  SetLength(e, aValue);
  SetLength(x, aValue);
  SetLength(y, aValue);
  SetLength(z, aValue);
  SetLength(yr, aValue);
  SetLength(ys, aValue);
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

