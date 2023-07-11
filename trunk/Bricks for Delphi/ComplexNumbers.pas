unit ComplexNumbers;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 2.0.0 (Escorpión) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Unit for basic handling of complex numbers }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

interface

uses
Math;

type complex = record
  re : real;
  im : real;
end;

const
  i: complex = (re: 0; im: 1);

function cexp (z: complex): complex;
function ctimes(z1, z2: complex): complex; overload;
function ctimes(z: complex; r: real): complex; overload;
function ctimes(r: real; z: complex): complex; overload;

implementation

function ctimes(z1, z2: complex): complex; overload;
begin
  result.re := z1.re * z2.re - z1.im * z2.im;
  result.im := z1.re * z2.im + z1.im * z2.re;
end;

function ctimes(z: complex; r: real): complex; overload;
begin
  result.re := z.re * r;
  result.im := z.im * r;
end;

function ctimes(r: real; z: complex): complex; overload;
begin
  result.re := z.re * r;
  result.im := z.im * r;
end;

function cexp (z: complex): complex;
var
  expre: real;
begin
  expre := exp(z.re);
  result.re := expre * cos(z.im);
  result.im := expre * sin(z.im);
end;

end.
