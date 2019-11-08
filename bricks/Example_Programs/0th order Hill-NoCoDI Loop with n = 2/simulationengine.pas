unit SimulationEngine;

{ CyberUnitsr }

{ Object Pascal units for computational cybernetics }

{ Simulator for Hill-NoCoDI loop with n = 2 }
{ Simulation Engine }

{ Version 1.0.0 (Corvus) }

{ (c) Johannes W. Dietrich, 1994 - 2019 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2019 }

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
    x, e, c, u, y, yr: array of extended;
    constructor Create;
    destructor Destroy; override;
    property size: integer read GetSize write SetSize;
  end;

  TBlocks = record
    G1, G2, G3: TP;
    MiMe: TMiMe;
    NoCoDI: TNoCoDI;
    squarer: TPMul;
  end;

  TPrediction = record
    x, e, c, u, y, yr: extended;
  end;

var
  gValues: TValues;
  gBlocks: TBlocks;
  gPrediction1, gPrediction2: TPrediction;

procedure RunSimulation(x, G1, G2, G3, D2: extended; nmax: integer);

implementation

procedure RunSimulation(x, G1, G2, G3, D2: extended; nmax: integer);
var
  e, c, y, u, yr: extended;
  a, b, cc, d: extended;
  i: integer;
begin
  if nmax > 0 then
  begin
    gPrediction1.x := x;
    gPrediction2.x := x;

    { Solving for y: }  { TODO -oJWD : Adapt for Hill kinetics }
    a := 2 * D2 * G3;
    b := D2 + sqr(G1) * sqr(gPrediction1.x);
    cc := -sqr(G1) * G2 * sqr(gPrediction1.x);
    d := -D2 * sqr(G3);
    gPrediction1.y := -(b + sqrt(sqr(b) - 4 * a * d)) / (2 * a); // change
    gPrediction1.yr := G3 * gPrediction1.y;
    gPrediction1.e := gPrediction1.x / (1 + gPrediction1.yr);
    gPrediction1.c := G1 * gPrediction1.e;
    gPrediction1.u := sqr(gPrediction1.c);
    gPrediction2.y := -(b - sqrt(sqr(b) - 4 * a * d)) / (2 * a); // change
    gPrediction2.yr := G3 * gPrediction2.y;
    gPrediction2.e := gPrediction2.x / (1 + gPrediction2.yr);
    gPrediction2.c := G1 * gPrediction2.e;
    gPrediction1.u := sqr(gPrediction2.c);

    gValues.size := 0; // delete content
    gValues.size := nmax;
    gBlocks.G1 := TP.Create;
    gBlocks.G3 := TP.Create;
    gBlocks.MiMe := TMiMe.Create;
    gBlocks.NoCoDI := TNoCoDI.Create;
    gBlocks.squarer := TPMul.Create;
    gBlocks.G1.G := G1;
    gBlocks.G3.G := G3;
    gBlocks.MiMe.G := G2;
    gBlocks.MiMe.D := D2;
    gBlocks.squarer.G := 1;
    yr := 20;
    for i := 0 to nmax - 1 do
    begin
      gBlocks.NoCoDI.input1 := x;
      gBlocks.NoCoDI.input2 := yr;
      e := gBlocks.NoCoDI.simOutput;
      gBlocks.G1.input := e;
      c := gBlocks.G1.simOutput;
      gBlocks.squarer.input1 := c;
      gBlocks.squarer.input2 := c;
      u := gBlocks.squarer.simOutput;
      gBlocks.MiMe.input := u;
      y := gBlocks.MiMe.simOutput;
      gBlocks.G3.input := y;
      yr := gBlocks.G3.simOutput;
      gValues.x[i] := x;
      gValues.e[i] := e;
      gValues.c[i] := c;
      gValues.u[i] := u;
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
  SetLength(u, aValue);
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

