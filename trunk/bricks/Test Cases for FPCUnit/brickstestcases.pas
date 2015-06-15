unit brickstestcases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, bricks;

type

  TPTestCases = class(TTestCase)
  published
    procedure Test1;
  end;

implementation

procedure TPTestCases.Test1;
var
  testBrick: TP;
begin
  testBrick := TP.Create;
  testBrick.G := 5;
  testBrick.input := 2;
  AssertEquals(10, testBrick.output);
  testBrick.Destroy;
end;

initialization

  RegisterTest(TPTestCases);
end.

