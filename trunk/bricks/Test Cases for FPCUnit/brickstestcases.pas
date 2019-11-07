unit brickstestcases;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ bricks test cases }

{ Version 1.0.1 (Corvus) }

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
  Classes, SysUtils, fpcunit, testutils, testregistry, bricks, lifeblocks;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
  end;

{ TPTestCases }

  TPTestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in frequency domain }
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
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in frequency domain }
  end;

{ TPT1TestCases }

  TPT1TestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in time domain }
    procedure Test3;  { test response in frequency domain }
  end;

{ TIT1TestCases }

  TIT1TestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in time domain }
    procedure Test3;  { test response in frequency domain }
  end;

{ TIT2TestCases }

  TIT2TestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in time domain }
  end;

{ TDT1TestCases }

  TDT1TestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in frequency domain }
  end;

{ TPT2TestCases }

  TPT2TestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in frequency domain }
  end;

{ TIntTestCases }

  TIntTestCases = class(TTestCase)
  published
    procedure Test1;  { test response in time domain }
    procedure Test2;  { test response in time domain }
    procedure Test3;  { test response in frequency domain }
  end;

{ TASIATestCases }

  TASIATestCases = class(TTestCase)
  published
    procedure Test1;
  end;

 { TMiMeTestCases }

  TMiMeTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

 { TNoCoDITestCases }

  TNoCoDITestCases = class(TTestCase)
  published
    procedure Test1;
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

procedure TIntTestCases.Test3;
var
  testBrick: TInt;
begin
  testBrick := TInt.Create;
  testBrick.G := 10;
  testBrick.amplitude := 2;
  testBrick.omega := 10;
  AssertEquals(testBrick.G * testBrick.amplitude /
    testBrick.omega, TestBrick.fr.M);
  AssertEquals(-90 * pi / 180, TestBrick.fr.phi);
  AssertEquals(abs(TestBrick.fr.M) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-TestBrick.fr.M) * sin(testBrick.fr.phi), testBrick.fr.F.im);
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

procedure TPT0TestCases.Test2;
const
  TEST_QUEUE_LENGTH = 2;
var
  testBrick: TPT0;
begin
  testBrick := TPT0.Create;
  testBrick.nt := TEST_QUEUE_LENGTH;
  testBrick.G := 10;
  testBrick.amplitude := 2;
  testBrick.omega := 10;
  testBrick.delta := 1;
  AssertEquals(20, TestBrick.fr.M);
  AssertEquals(-20, TestBrick.fr.phi);
  AssertEquals(abs(20) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-20) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TPT1TestCases }

procedure TPT1TestCases.Test1;
var
  testBrick: TPT1;
  i: integer;
begin
  testBrick := TPT1.Create;
  testBrick.G := 5;
  testBrick.delta := 10;
  testBrick.t1 := 15;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 10000 do
    TestBrick.simulate;
  AssertEquals(testBrick.G * testBrick.input, TestBrick.output);
  testBrick.Destroy;
end;

procedure TPT1TestCases.Test2;
var
  testBrick: TPT1;
  i: integer;
begin
  testBrick := TPT1.Create;
  testBrick.G := 5;
  testBrick.delta := 1;
  testBrick.t1 := 15e6;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 100000 do
    TestBrick.simulate;
  AssertTrue(TestBrick.output < 0.07);
  testBrick.Destroy;
end;

procedure TPT1TestCases.Test3;
var
  testBrick: TPT1;
begin
  testBrick := TPT1.Create;
  testBrick.G := 10;
  testBrick.delta := 1;
  testBrick.t1 := 1;
  testBrick.amplitude := 2;
  testBrick.omega := 10;
  AssertEquals(testBrick.G * testBrick.amplitude /
    sqrt(1 + sqr(testBrick.omega) * sqr(testBrick.t1)), TestBrick.fr.M);
  AssertEquals(-arctan(testBrick.omega * testBrick.t1), TestBrick.fr.phi);
  AssertEquals(abs(TestBrick.fr.M) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-TestBrick.fr.M) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TPT2TestCases }

procedure TPT2TestCases.Test1;
var
  testBrick: TPT2;
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
    TestBrick.simulate;
  AssertEquals(testBrick.G * testBrick.input, testBrick.output);
  testBrick.Destroy;
end;

procedure TPT2TestCases.Test2;
var
  testBrick: TPT2;
begin
  testBrick := TPT2.Create;
  testBrick.G := 5;
  testBrick.delta := 1;
  testBrick.t2 := 1;
  testBrick.dmp := 0.5;
  testBrick.amplitude := 2;
  testBrick.omega := 10;
  AssertEquals(testBrick.amplitude * testBrick.G /
    sqrt(sqr(1 - sqr(testBrick.omega * testBrick.t2)) +
    sqr(2 * testBrick.dmp * testBrick.omega * testBrick.t2)),
    TestBrick.fr.M);
  AssertEquals(-pi - arctan(2 * testBrick.dmp * testBrick.omega * testBrick.t2 /
    (1 - sqr(testBrick.omega * testBrick.t2))),
    TestBrick.fr.phi);
  AssertEquals(abs(TestBrick.fr.M) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-TestBrick.fr.M) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TIT1TestCases }

procedure TIT1TestCases.Test1;
var
  testBrick: TIT1;
  i: integer;
begin
  testBrick := TIT1.Create;
  testBrick.G := 1;
  testBrick.t1 := 15e6;
  testBrick.delta := 1;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 100000 do
    TestBrick.simulate;
  AssertTrue(TestBrick.output < 0.07);
  testBrick.Destroy;
end;

procedure TIT1TestCases.Test2;
var
  testBrick: TIT1;
  i: integer;
begin
  testBrick := TIT1.Create;
  testBrick.G := 1;
  testBrick.t1 := 1.5;
  testBrick.delta := 1;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 100000 do
    TestBrick.simulate;
  AssertTrue(TestBrick.output > i);
  testBrick.Destroy;
end;

procedure TIT1TestCases.Test3;
var
  testBrick: TIT1;
begin
  testBrick := TIT1.Create;
  testBrick.G := 10;
  testBrick.delta := 1;
  testBrick.t1 := 0.1;
  testBrick.amplitude := 10;
  testBrick.omega := 10;
  AssertEquals(testBrick.G * testBrick.amplitude / testBrick.omega *
    1 / sqrt(1 + sqr(testBrick.omega) * sqr(testBrick.t1)), TestBrick.fr.M);
  AssertEquals(-pi / 2 - arctan(testBrick.omega * testBrick.t1), TestBrick.fr.phi);
  AssertEquals(abs(TestBrick.fr.M) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-TestBrick.fr.M) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TIT2TestCases }

procedure TIT2TestCases.Test1;
var
  testBrick: TIT2;
  i : integer;
begin
  testBrick := TIT2.Create;
  testBrick.G := 1.2;
  testBrick.t2 := 0.175;
  testBrick.dmp := 0.15;
  testBrick.delta := 0.1;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  for i := 1 to 50 do
    testBrick.simulate;
  AssertTrue((testBrick.output > 14) and (testBrick.output < 15));
  testBrick.Destroy;
end;

procedure TIT2TestCases.Test2;
var
  testBrick: TIT2;
begin
  testBrick := TIT2.Create;
  testBrick.G := 2;
  testBrick.delta := 1;
  testBrick.t2 := 1;
  testBrick.dmp := 0.5;
  testBrick.amplitude := 2;
  testBrick.omega := 0.0001;
  AssertTrue(TestBrick.fr.M >= testBrick.G * testBrick.amplitude / testBrick.omega);
  AssertTrue((-pi / 2) - TestBrick.fr.phi < 0.1);
  AssertEquals(abs(TestBrick.fr.M) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-TestBrick.fr.M) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TDT1TestCases }

procedure TDT1TestCases.Test1;
var
  testBrick: TDT1;
  i: integer;
begin
  testBrick := TDT1.Create;
  testBrick.G := 2;
  testBrick.t1 := 2;
  testBrick.delta := 1;
  testBrick.input := 2;
  AssertEquals(0, TestBrick.output);
  TestBrick.simulate;
  AssertEquals(4, TestBrick.output);
  for i := 1 to 100000 do
    TestBrick.simulate;
  AssertTrue(TestBrick.output < 0.01);
  testBrick.Destroy;
end;

procedure TDT1TestCases.Test2;
var
  testBrick: TDT1;
begin
  testBrick := TDT1.Create;
  testBrick.G := 2;
  testBrick.delta := 1;
  testBrick.t1 := 1;
  testBrick.amplitude := 1;
  testBrick.omega := 1;
  AssertEquals(testBrick.amplitude * testBrick.G * testBrick.omega /
    sqrt(1 + sqr(testBrick.omega) * sqr(testBrick.t1)), TestBrick.fr.M);
  AssertEquals(arctan(1 / (testBrick.omega * testBrick.t1)), TestBrick.fr.phi);
  AssertEquals(abs(TestBrick.fr.M) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(-TestBrick.fr.M) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TPTestCases }

procedure TPTestCases.Test1;
{ tests response in time domain }
var
  testBrick: TP;
begin
  testBrick := TP.Create;
  testBrick.G := 5;
  testBrick.input := 2;
  AssertEquals(10, testBrick.simOutput);
  testBrick.Destroy;
end;

procedure TPTestCases.Test2;
{ tests response in frequency domain }
var
  testBrick: TP;
begin
  testBrick := TP.Create;
  testBrick.G := 5;
  testBrick.amplitude := 2;
  AssertEquals(10, testBrick.fr.M);
  AssertEquals(0, testBrick.fr.phi);
  AssertEquals(abs(10) * cos(testBrick.fr.phi), testBrick.fr.F.re);
  AssertEquals(abs(0) * sin(testBrick.fr.phi), testBrick.fr.F.im);
  testBrick.Destroy;
end;

{ TASIATestCases }

procedure TASIATestCases.Test1;
const
  alpha = 10;
  beta = 0.5;
var
  testBrick: TASIA;
  i: integer;
begin
  testBrick := TASIA.Create;
  testBrick.alpha := alpha;
  testBrick.beta := beta;
  testBrick.delta := 1;
  testBrick.input := 1;
  for i := 1 to 100 do
    TestBrick.simulate;
  AssertEquals(alpha / beta, testBrick.output);
  testBrick.Destroy;
end;

{ TMiMeTestCases }

procedure TMiMeTestCases.Test1;
const
  G = 5;
  D = 2;
  testSignal = 10;
var
  testBrick: TMiMe;
begin
  testBrick := TMiMe.Create;
  testBrick.G := G;
  testBrick.D := D;
  testBrick.input := testSignal;
  TestBrick.simulate;
  AssertEquals(G * testSignal / (D + testSignal), testBrick.output);
  testBrick.Destroy;
end;

{ TNoCoDITestCases }

procedure TNoCoDITestCases.Test1;
const
  xe1 = 5;
  xe2 = 4;
var
  testBrick: TNoCoDI;
begin
  testBrick := TNoCoDI.Create;
  testBrick.input1 := xe1;
  testBrick.input2 := xe2;
  TestBrick.simulate;
  AssertEquals(xe1 / (1 + xe2), testBrick.output);
  testBrick.Destroy;
end;


initialization

  RegisterTest(TControlTestCases);
  RegisterTest(TPTestCases);
  RegisterTest(TPT0TestCases);
  RegisterTest(TPT1TestCases);
  RegisterTest(TPT2TestCases);
  RegisterTest(TIT1TestCases);
  RegisterTest(TIT2TestCases);
  RegisterTest(TDT1TestCases);
  RegisterTest(TPAddTestCases);
  RegisterTest(TPSubTestCases);
  RegisterTest(TPMulTestCases);
  RegisterTest(TPDivTestCases);
  RegisterTest(TIntTestCases);
  RegisterTest(TASIATestCases);
  RegisterTest(TMiMeTestCases);
  RegisterTest(TNoCoDITestCases);
end.

