unit brickstestcases;

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

initialization
  TDUnitX.RegisterTestFixture(TControlTestCases);
  TDUnitX.RegisterTestFixture(TPTestCases);
end.
