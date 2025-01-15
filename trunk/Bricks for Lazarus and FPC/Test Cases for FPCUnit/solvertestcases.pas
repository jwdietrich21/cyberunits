unit solvertestcases;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Test cases for solver }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Solver;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

{ TPTestCases }

  { TSolverTestCases }

  TSolverTestCases = class(TTestCase)
  published
    procedure TestLinear1;    { linear equation }
    procedure TestLinear2;    { linear equation }
    procedure TestQuadratic1; { quadratic equation}
    procedure TestQuadratic2; { quadratic equation}
    procedure TestQuadratic3; { quadratic equation}
    procedure TestQuadratic4; { quadratic equation}
    procedure TestCubic1;     { cubic equation }
    procedure TestCubic2;     { cubic equation }
    procedure TestCubic3;     { cubic equation }
    procedure TestCubic4;     { cubic equation }
    procedure TestCubic5;     { cubic equation }
  end;


implementation

{ TControlTestCases }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;


{ TPTestCases }

procedure TSolverTestCases.TestLinear1;
{ tests solution of linear equation }
const
  a = 5;
  b = -10;
var
  root: TLRoot;
begin
  root := Solve(a, b);
  AssertEquals(2, root);
end;

procedure TSolverTestCases.TestLinear2;
{ tests solution of linear equation }
const
  a = -2;
  b = 10;
var
  root: TLRoot;
begin
  root := Solve(a, b);
  AssertEquals(5, root);
end;

procedure TSolverTestCases.TestQuadratic1;
{ tests solutions of quadratic equation }
const
  a = 1;
  b = 3;
  c = -10;
var
  roots: TQRoots;
begin
  roots := Solve(a, b, c);
  AssertEquals(-5, roots[0]);
  AssertEquals(2, roots[1]);
end;

procedure TSolverTestCases.TestQuadratic2;
{ tests solutions of quadratic equation }
const
  a = -2;
  b = 0;
  c = 4;
var
  roots: TQRoots;
begin
  roots := Solve(a, b, c);
  AssertEquals(-sqrt(2), roots[0]);
  AssertEquals(sqrt(2), roots[1]);
end;

procedure TSolverTestCases.TestQuadratic3;
{ tests solutions of quadratic equation }
const
  a = 2;
  b = -5;
  c = 0;
var
  roots: TQRoots;
begin
  roots := Solve(a, b, c);
  AssertEquals(0, roots[0]);
  AssertEquals(2.5, roots[1]);
end;

procedure TSolverTestCases.TestQuadratic4;
{ tests solutions of quadratic equation }
const
  a = 1;
  b = 2;
  c = -24;
var
  roots: TQRoots;
begin
  roots := Solve(a, b, c);
  AssertEquals(-6, roots[0]);
  AssertEquals(4, roots[1]);
end;

procedure TSolverTestCases.TestCubic1;
{ tests solutions of cubic equation }
const
  a = 1;
  b = 0;
  c = 0;
  d = 0;
var
  roots: TCRoots;
begin
  roots := Solve(a, b, c, d);
  AssertEquals(0, roots[0]);
  AssertEquals(0, roots[1]);
  AssertEquals(0, roots[2]);
end;

procedure TSolverTestCases.TestCubic2;
{ tests solutions of cubic equation }
const
  a = -1;
  b = 0;
  c = 3;
  d = 2;
var
  roots: TCRoots;
begin
  roots := Solve(a, b, c, d);
  AssertEquals(-1, roots[0]);
  AssertEquals(-1, roots[1]);
  AssertEquals(2, roots[2]);
end;

procedure TSolverTestCases.TestCubic3;
{ tests solutions of cubic equation }
const
  a = -1;
  b = 3;
  c = 0;
  d = -4;
var
  roots: TCRoots;
begin
  roots := Solve(a, b, c, d);
  AssertEquals(-1, roots[0]);
  AssertEquals(2, roots[1]);
  AssertEquals(2, roots[2]);
end;

procedure TSolverTestCases.TestCubic4;
{ tests solutions of cubic equation }
const
  a = -0.5;
  b = -0.5;
  c = 4;
  d = 6;
var
  roots: TCRoots;
begin
  roots := Solve(a, b, c, d);
  AssertEquals(-2, roots[0]);
  AssertEquals(-2, roots[1]);
  AssertEquals(3, roots[2]);
end;

procedure TSolverTestCases.TestCubic5;
{ tests solutions of cubic equation }
const
  a = 54;
  b = -270;
  c = 432;
  d = -216;
var
  roots: TCRoots;
begin
  roots := Solve(a, b, c, d);
  AssertEquals(1, roots[0]);
  AssertEquals(2, roots[1]);
  AssertEquals(2, roots[2]);
end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TSolverTestCases);
end.

