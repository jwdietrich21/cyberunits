unit bricks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math;

type

  { TBlock }

  TBlock = class
  protected
    Foutput: real;
  public
    name: string;
    procedure simulate; virtual; abstract;
    destructor Destroy; override;
    property output: real read Foutput;
  end;

  { TP }

  TP = class(TBlock)
  protected
    function GetOutput: real;
  public
    input, G: real;
    procedure simulate; override;
    constructor Create;
    destructor Destroy; override;
    property output: real read GetOutput;
  end;

implementation

destructor TBlock.Destroy;
begin
  inherited Destroy;
end;

function TP.GetOutput: real;
begin
  simulate;
  result := Foutput;
end;

procedure TP.simulate;
begin
  Foutput := input * G;
end;

constructor TP.Create;
begin
  inherited Create;
end;

destructor TP.Destroy;
begin
  inherited Destroy;
end;

end.

