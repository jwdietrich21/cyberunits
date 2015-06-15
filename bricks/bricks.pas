unit bricks;

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

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, StrUtils, Math;

const
  kError101 = 'Runtime error: Negative parameter(s)';
  kError102 = 'Runtime error: Parameter(s) out of range';
  kError103 = 'Runtime error: min > max';
  kError104 = 'Runtime error: max = 0';

type

  { TBlock }

  TBlock = class
  protected
    Foutput: real;
    procedure simulate; virtual; abstract;
  public
    name: string;
    destructor Destroy; override;
    property output: real read Foutput;
  end;

  { TP }

  TP = class(TBlock)
  protected
    procedure simulate; override;
    function GetOutput: real;
  public
    input, G: real;
    constructor Create;
    destructor Destroy; override;
    property output: real read Foutput;
    property simOutput: real read GetOutput;
  end;

  { TPT0 }

  TPT0 = class(TBlock)
  {dead-time element, improved from Neuber 1989}
  protected
    procedure simulate; override;
    function GetQueueLength: integer;
    procedure SetQueueLength(AValue: integer);
    function GetOutput: real;
  public
    input: real;
    xt: array of real;
    constructor Create;
    destructor Destroy; override;
    property output: real read Foutput;
    property nt: integer read GetQueueLength write SetQueueLength;
    property simOutput: real read GetOutput;
  end;

  { TPT1 }

  TPT1 = class(TBlock)
  {First order delay element, changed from Neuber 1989}
  protected
    procedure simulate; override;
    function GetOutput: real;
  public
    input, G, t1, x0, delta: real;
    constructor Create;
    destructor Destroy; override;
    property output: real read Foutput;
    property simOutput: real read GetOutput;
  end;

implementation

{ TPT0 }

function TPT0.GetQueueLength: integer;
begin
  result := Length(xt)
end;

procedure TPT0.SetQueueLength(AValue: integer);
begin
  SetLength(xt, AValue);
end;

procedure TPT0.simulate;
var
  i: integer;
begin
  assert(nt >= 0, kError101);
  if nt = 0 then
    fOutput := input
  else
  begin
    for i := nt - 1 downto 1 do
      xt[i] := xt[i - 1];
    fOutput := xt[nt - 1];
    xt[0] := input;
  end;
end;

function TPT0.GetOutput: real;
begin
  simulate;
  result := fOutput;
end;

constructor TPT0.Create;
begin
  inherited Create;
  fOutput := 0;
end;

destructor TPT0.Destroy;
begin
  inherited Destroy;
end;

{ TPT1 }

function TPT1.GetOutput: real;
begin
  simulate;
  result := fOutput;
end;

procedure TPT1.simulate;
var
  f: real;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  f := exp(-delta / t1);
  fOutput := f * x0 + G * (1 - f) * input;
  x0 := fOutput;
end;

constructor TPT1.Create;
begin
  inherited Create;
  x0 := 0;
  fOutput := 0;
end;

destructor TPT1.Destroy;
begin
  inherited Destroy;
end;

  { TP }

function TP.GetOutput: real;
begin
  simulate;
  result := fOutput;
end;

procedure TP.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := input * G;
end;

constructor TP.Create;
begin
  inherited Create;
  fOutput := 0;
end;

destructor TP.Destroy;
begin
  inherited Destroy;
end;

  { TBlock }

destructor TBlock.Destroy;
begin
  inherited Destroy;
end;

end.

{References:  }
{1. Neuber, H., "Simulation von Regelkreisen auf Personal Computern  }
{   in Pascal und Fortran 77", IWT, Vaterstetten 1989  }

