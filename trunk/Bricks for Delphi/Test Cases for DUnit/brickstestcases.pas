unit brickstestcases;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ bricks test cases }

{ Version 2.1.0 (Foudre)

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

interface

uses
  Bricks, DUnitX.TestFramework;

type
  [TestFixture]

  TControlTestCases = class
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
    [Test]
    procedure PositiveCheck;
  end;

  TPTestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in frequency domain }
  end;

  TPMulTestCases = class
  public
    [Test]
    procedure Test1;
  end;

  TPDivTestCases = class
  public
    [Test]
    procedure Test1;
  end;

  TPAddTestCases = class
  public
    [Test]
    procedure Test1;
  end;

  TPSubTestCases = class
  public
    [Test]
    procedure Test1;
  end;

  TPT0TestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in frequency domain }
  end;

  TPT1TestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in time domain }
    [Test]
    procedure Test3;  { test response in frequency domain }
  end;

  TIT1TestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in time domain }
    [Test]
    procedure Test3;  { test response in frequency domain }
  end;

  TIT2TestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in time domain }
  end;

  TDT1TestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in frequency domain }
  end;

  TPT2TestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in frequency domain }
  end;

  TIntTestCases = class
  public
    [Test]
    procedure Test1;  { test response in time domain }
    [Test]
    procedure Test2;  { test response in time domain }
    [Test]
    procedure Test3;  { test response in frequency domain }
  end;


implementation

{ -- FPC adapter functions -- }
{ -- Emulate functionality of Free Pascal in Delphi -- }
function AssertTrue(ACondition: boolean): boolean;
begin
  result := ACondition;
end;

function AssertEquals(const x1, x2: real): boolean;
begin
  result := AssertTrue(x1 = x2);
end;

procedure TControlTestCases.Test1;
begin
end;

procedure TControlTestCases.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertTrue(1 = 1);
end;

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

initialization
  TDUnitX.RegisterTestFixture(TControlTestCases);
  TDUnitX.RegisterTestFixture(TPTestCases);
  TDUnitX.RegisterTestFixture(TPMulTestCases);
  TDUnitX.RegisterTestFixture(TPDivTestCases);
  TDUnitX.RegisterTestFixture(TPAddTestCases);
  TDUnitX.RegisterTestFixture(TPSubTestCases);
  TDUnitX.RegisterTestFixture(TPT0TestCases);
  TDUnitX.RegisterTestFixture(TPT1TestCases);
  TDUnitX.RegisterTestFixture(TIT1TestCases);
  TDUnitX.RegisterTestFixture(TIT2TestCases);
  TDUnitX.RegisterTestFixture(TDT1TestCases);
  TDUnitX.RegisterTestFixture(TPT2TestCases);
  TDUnitX.RegisterTestFixture(TIntTestCases);
end.
