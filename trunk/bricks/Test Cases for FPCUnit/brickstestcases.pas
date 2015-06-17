unit brickstestcases;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ bricks test cases }

{ Version 1.0 }

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

{ TPMulTestCases }

  TPMulTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TPDivTestCases }

  TPDivTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TPAddTestCases }

  TPAddTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TPSubTestCases }

  TPSubTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TPT0TestCases }

  TPT0TestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TPT1TestCases }

  TPT1TestCases = class(TTestCase)
  published
    procedure Test1;
    procedure Test2;
  end;

{ TPT1TestCases }

  { TPT2TestCases }

  TPT2TestCases = class(TTestCase)
  published
    procedure Test1;
  end;

{ TIntTestCases }

  TIntTestCases = class(TTestCase)
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

{ TIntTestCases }

procedure TIntTestCases.Test1;
var
  testBrick: TInt;
begin
  testBrick := TInt.Create;
  testBrick.input := 10;
  testBrick.delta := 10;
  testBrick.simulate;
  AssertEquals(100, testBrick.output);
  testBrick.Destroy;
end;

procedure TIntTestCases.Test2;
var
  testBrick: TInt;
begin
  testBrick := TInt.Create;
  testBrick.input := 10;
  testBrick.delta := 50;
  testBrick.simulate;
  testBrick.simulate;
  testBrick.simulate;
  AssertEquals(1500, testBrick.output);
  testBrick.Destroy;
end;

{ TPSubTestCases }

procedure TPSubTestCases.Test1;
var
  testBrick: TPSub;
begin
  testBrick := TPSub.Create;
  testBrick.input1 := 107;
  testBrick.input2 := 11;
  AssertEquals(96, testBrick.simOutput);
  testBrick.Destroy;
end;

{ TPAddTestCases }

procedure TPAddTestCases.Test1;
var
  testBrick: TPAdd;
begin
  testBrick := TPAdd.Create;
  testBrick.input1 := 2;
  testBrick.input2 := 11;
  AssertEquals(13, testBrick.simOutput);
  testBrick.Destroy;
end;

{ TPDivTestCases }

procedure TPDivTestCases.Test1;
var
  testBrick: TPDiv;
begin
  testBrick := TPDiv.Create;
  testBrick.G := 5;
  testBrick.input1 := 15;
  testBrick.input2 := 3;
  AssertEquals(25, testBrick.simOutput);
  testBrick.Destroy;
end;

{ TPMulTestCases }

procedure TPMulTestCases.Test1;
var
  testBrick: TPMul;
begin
  testBrick := TPMul.Create;
  testBrick.G := 10;
  testBrick.input1 := 2;
  testBrick.input2 := 5;
  AssertEquals(100, testBrick.simOutput);
  testBrick.Destroy;
end;

{ TPT0TestCases }

procedure TPT0TestCases.Test1;
const
  TEST_QUEUE_LENGTH = 7;
var
  testBrick: TPT0;
  i: integer;
begin
  testBrick := TPT0.Create;
  testBrick.nt := TEST_QUEUE_LENGTH;
  testBrick.G := 10;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 0 to TEST_QUEUE_LENGTH do
    TestBrick.simOutput;
  AssertEquals(testBrick.G * 2, TestBrick.output);
  testBrick.Destroy;
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

{ TPT2TestCases }

procedure TPT2TestCases.Test1;
var
  testBrick: TPT2;
  temp: real;
  i: integer;
begin
  testBrick := TPT2.Create;
  testBrick.G := 5;
  testBrick.delta := 0.1;
  testBrick.t2 := 0.159;
  testBrick.dmp := 0.25;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 500 do
    temp := TestBrick.simOutput;
  AssertEquals(testBrick.G * testBrick.input, TestBrick.output);
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
  RegisterTest(TPT0TestCases);
  RegisterTest(TPT1TestCases);
  RegisterTest(TPT2TestCases);
  RegisterTest(TPAddTestCases);
  RegisterTest(TPSubTestCases);
  RegisterTest(TPMulTestCases);
  RegisterTest(TPDivTestCases);
  RegisterTest(TIntTestCases);
end.

