unit SimulationEngine;

{ CyberUnitsr }

{ Object Pascal units for computational cybernetics }

{ Simulator for Hill-NoCoDI loop with n = 2 }
{ Simulation Engine }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

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
  Classes, SysUtils, Forms, Math, bricks, lifeblocks;

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
    G1, G3: TP; // G2 is part of the MiMe element.
    MiMe: TMiMe;
    NoCoDI: TNoCoDI;
    squarer: TPMul;
  end;

  TRoots = record
    x1, x2, x3: extended;
  end;

  TPrediction = record
    x, e, c, u, y, yr: extended;
  end;

var
  gValues: TValues;
  gBlocks: TBlocks;
  gPrediction1, gPrediction2, gPrediction3: TPrediction;

procedure RunSimulation(x, G1, G2, G3, D2: extended; nmax: integer);

implementation

function arc(chi: real): real;
  {converts an angle from degree to radian}
begin
  arc := 2 * pi * (chi / 360);
end;

function arccosinus(cosphi: real): real;
  {calculates the arcus cosine of a number between -1 and 1}
var
  arcsin: real;
begin
  arcsin     := arctan(cosphi / sqrt(1 - sqr(cosphi)));
  arccosinus := arc(90) - arcsin;
end;

function cbrt(x: real): real;
  {calculates cubic root of x}
begin
  result := sign(x) * power(abs(x), 1/3);
end;

function SolveCubic(a, b, c, d: extended): TRoots;
var
  r, s, p, q, u, v, Det, phi, y1, y2, y3: extended;
begin
  r  := c / a - 1 / 3 * sqr(b / a);
  s  := 2 / 27 * power((b / a), 3) - 1 / 3 * c * b / sqr(a) + d / a;
  p  := r / 3;
  q  := s / 2;

  Det := p * p * p + q * q;

  if Det >= 0 then
  begin {Cardano's formula, one real solution}
    u  := cbrt(-q + sqrt(Det));
    v  := cbrt(-q - sqrt(Det));
    y1 := u + v;        {real solution of Cardano's equation}
    y2 := -(u + v) / 2; {Real part of the first complex solution}
    y3 := y2;           {Real part of the second complex solution (=y2)}
  end
  else
  begin {Casus irreducibilis, three real solutions}
    u   := -q / (sqrt(-p * sqr(-p))); {cos phi}
    phi := arccosinus(u);            {angle as radian}
    y1  := 2 * sqrt(-p) * cos(phi / 3);
    y2  := -2 * sqrt(-p) * cos(phi / 3 + arc(60));
    y3  := -2 * sqrt(-p) * cos(phi / 3 - arc(60));
  end;

  result.x1 := y1 - b / (3 * a);
  result.x2 := y2 - b / (3 * a);
  result.x3 := y3 - b / (3 * a);
end;

procedure RunSimulation(x, G1, G2, G3, D2: extended; nmax: integer);
var
  e, c, y, u, yr: extended;
  a, b, cc, d: extended;
  i: integer;
  Roots: TRoots;
begin
  if nmax > 0 then
  begin
    gPrediction1.x := x;
    gPrediction2.x := x;
    gPrediction3.x := x;

    a := D2 * sqr(G3);
    b := 2 * D2 * G3;
    cc := D2 + sqr(G1) * sqr(x);
    d := -sqr(G1) * G2 * sqr(x);

    Roots := SolveCubic(a, b, cc, d);

    gPrediction1.y := Roots.x1;
    gPrediction1.yr := G3 * gPrediction1.y;
    gPrediction1.e := gPrediction1.x / (1 + gPrediction1.yr);
    gPrediction1.c := G1 * gPrediction1.e;
    gPrediction1.u := sqr(gPrediction1.c);

    gPrediction2.y := Roots.x2;
    gPrediction2.yr := G3 * gPrediction2.y;
    gPrediction2.e := gPrediction2.x / (1 + gPrediction2.yr);
    gPrediction2.c := G1 * gPrediction2.e;
    gPrediction2.u := sqr(gPrediction2.c);

    gPrediction3.y := Roots.x3;
    gPrediction3.yr := G3 * gPrediction3.y;
    gPrediction3.e := gPrediction3.x / (1 + gPrediction3.yr);
    gPrediction3.c := G1 * gPrediction3.e;
    gPrediction3.u := sqr(gPrediction3.c);

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
    yr := gPrediction1.yr * 20; // start with distortion to show control effect
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
      application.ProcessMessages; // allow operations of GUI
    end;
    gBlocks.G1.Destroy;
    gBlocks.G3.Destroy;
    gBlocks.MiMe.Destroy;
    gBlocks.NoCoDI.Destroy;
    gBlocks.squarer.Destroy;
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

