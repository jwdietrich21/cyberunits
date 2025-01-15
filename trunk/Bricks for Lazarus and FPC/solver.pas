unit Solver;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Solver for algebraic equations defined by parameter sets }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Standard blocks for systems modelling and simulation }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TLRoot = extended;
  TQRoots = array[0..1] of extended;
  TCRoots = array[0..2] of extended;
  TRRoots = array[0..3] of extended;

function Solve(a, b: extended): TLRoot;
function Solve(a, b, c: extended): TQRoots;
function Solve(a, b, c, d: extended): TCRoots;
function Solve(a, b, c, d, e: extended): TRRoots;

implementation

function arc(chi: extended): extended;
  {converts an angle from degree to radian}
  {rechnet Winkel von Grad nach Bogenmaß um}
begin
  arc := 2 * pi * (chi / 360);
end;

function arccos(cosphi: extended): extended;
  {calculates the arcus cosine of a number between -1 and 1}
  {errechnet den Arcus-Cosinus einer Zahl zwischen -1 und 1}
var
  arcsin: extended;
begin
  arcsin     := arctan(cosphi / sqrt(1 - sqr(cosphi)));
  result := arc(90) - arcsin;
end;

function cbrt(x: extended): extended;
  {calculates cubic root of x}
  {berechnet Kubikwurzel von x}
begin
  result := sign(x) * power(abs(x), 1/3);
end;

procedure SortTwo(var x, y: extended);
var
  t: extended;
begin
  if y < x then
  begin
    t := x;
    x := y;
    y := t;
  end;
end;

procedure SortThree(var x, y, z: extended);
begin
  SortTwo(x, y);
  SortTwo(x, z);
  SortTwo(y, z);
end;

function Solve(a, b: extended): TLRoot;
{solves linear equation ax + b = 0 with parameters a and b}
{löst lineare Gleichung ax + b = 0 mit Parametern a und b}
begin
  result := -b/a;
end;

function Solve(a, b, c: extended): TQRoots;
  {solves quadratic equation ax^2 + bx + c = 0 with parameters a, b and c}
  {löst quadratische Gleichung ax^2 + bx + c = 0 mit Parametern a, b, und c}
begin
  result[0] := -(b + sqrt(sqr(b) - 4 * a * c)) / (2 * a);
  result[1] := -(b - sqrt(sqr(b) - 4 * a * c)) / (2 * a);
  SortTwo(result[0], result[1]);
end;

function Solve(a, b, c, d: extended): TCRoots;
  {solves a cubic equation with the parameters a, b, c, and d}
  {löst kubische Gleichung mit Parametern a, b, c und d}
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
    phi := arccos(u);            {angle as radian}
    y1  := 2 * sqrt(-p) * cos(phi / 3);
    y2  := -2 * sqrt(-p) * cos(phi / 3 + arc(60));
    y3  := -2 * sqrt(-p) * cos(phi / 3 - arc(60));
  end;

  result[0] := y1 - b / (3 * a);
  result[1] := y2 - b / (3 * a);
  result[2] := y3 - b / (3 * a);
  SortThree(result[0], result[1], result[2]);
end;

function Solve(a, b, c, d, e: extended): TRRoots;
  {solves a quartic equation with the parameters a, b, c, d and e}
  {löst quartische Gleichung mit Parametern a, b, c, d und e}
begin
  { #todo -oJWD : To be implemented }
end;

end.

