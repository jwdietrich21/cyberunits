program brickstests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, brickstestcases, bricks;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

