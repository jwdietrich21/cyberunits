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

{$mode objfpc}{$H+}

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

  { TPT1 }

  TPT1 = class(TBlock)
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

{ TPT1 }

function TPT1.GetOutput: real;
begin
  simulate;
  result := Foutput;
end;

procedure TPT1.simulate;
var
  f: real;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  f := exp(-delta / t1);
  Foutput := f * x0 + G * (1 - f) * input;
  x0 := Foutput;
end;

constructor TPT1.Create;
begin
  inherited Create;
  x0 := 0;
  foutput := 0;
end;

destructor TPT1.Destroy;
begin
  inherited Destroy;
end;

  { TP }

function TP.GetOutput: real;
begin
  simulate;
  result := Foutput;
end;

procedure TP.simulate;
begin
  assert(G >= 0, kError101);
  Foutput := input * G;
end;

constructor TP.Create;
begin
  inherited Create;
  foutput := 0;
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

