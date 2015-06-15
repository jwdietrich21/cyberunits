unit brickstestcases;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ bricks test cases }

{ Version 0.1 }

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
  Classes, SysUtils, fpcunit, testutils, testregistry, bricks;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

{ TPTestCases }

  TPTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TPT1TestCases }

  TPT1TestCases = class(TTestCase)
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

{ TControlTestCases }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

{ TPT1TestCases }

procedure TPT1TestCases.Test1;
var
  testBrick: TPT1;
  temp: real;
  i: integer;
begin
  testBrick := TPT1.Create;
  testBrick.G := 5;
  testBrick.delta := 10;
  testBrick.t1 := 15;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 10000 do
    temp := TestBrick.simOutput;
  AssertEquals(testBrick.G * testBrick.input, TestBrick.output);
  testBrick.Destroy;
end;

procedure TPT1TestCases.Test2;
var
  testBrick: TPT1;
  temp: real;
  i: integer;
begin
  testBrick := TPT1.Create;
  testBrick.G := 5;
  testBrick.delta := 1;
  testBrick.t1 := 15e6;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 100000 do
    temp := TestBrick.simOutput;
  AssertTrue(TestBrick.output < 0.07);
  testBrick.Destroy;
end;

{ TPTestCases }

procedure TPTestCases.Test1;
var
  testBrick: TP;
begin
  testBrick := TP.Create;
  testBrick.G := 5;
  testBrick.input := 2;
  AssertEquals(10, testBrick.simOutput);
  testBrick.Destroy;
end;

initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TPTestCases);
  RegisterTest(TPT1TestCases);
end.

