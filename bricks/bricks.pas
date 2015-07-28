unit Bricks;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 1.0.0 (Corvus) }

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
  Classes, SysUtils, StrUtils, Math, ucomplex;

const
  kError101 = 'Runtime error: Negative parameter(s)';
  kError102 = 'Runtime error: Parameter(s) out of range';
  kError103 = 'Runtime error: min > max';
  kError104 = 'Runtime error: max = 0';
  kError105 = 'Runtime error: Denominator is zero';

type

  { TBlock }
  { Abstract base class for IPS blocks }

  TBlock = class
  protected
    Foutput: extended;
  public
    name: string;
    destructor Destroy; override;
    procedure simulate; virtual; abstract;
    property output: extended read Foutput;
  end;

  { TP }
  { Proportional block }

  TP = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input, G: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPT0 }
  { Dead-time element, improved from Neuber 1989 }

  TPT0 = class(TBlock)
  protected
    function GetQueueLength: integer;
    procedure SetQueueLength(AValue: integer);
    function SimAndGetOutput: extended;
  public
    input, G: extended;
    xt: array of extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    property nt: integer read GetQueueLength write SetQueueLength;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPT1 }
  { First order delay element, changed from Neuber 1989 }

  TPT1 = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input, G, t1, x1, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPT2 }
  { Second order delay element, changed from Neuber 1989 }

  TPT2 = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input, G, t2, dmp, x1, x2, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TInt }
  { Integrator block, changed from Neuber 1989 }

  TInt = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input, G, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TInt }
  { IT1 block, changed from Neuber 1989 }

  { TIT1 }

  TIT1 = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input, G, t1, x1, delta: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPAdd }
  { Summation block }

  TPAdd = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input1, input2, G: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPSub }
  { Substraction block, comparator }

  TPSub = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input1, input2, G: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPMul }
  { Multiplicator }

  TPMul = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input1, input2, G: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

  { TPDiv }
  { Divider}

  TPDiv = class(TBlock)
  protected
    function SimAndGetOutput: extended;
  public
    input1, input2, G: extended;
    constructor Create;
    destructor Destroy; override;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;


implementation

{ TIT1 }

function TIT1.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TIT1.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  fOutput := 0;
end;

destructor TIT1.Destroy;
begin
  inherited Destroy;
end;

procedure TIT1.simulate;
var
  a, x1n: extended;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  a := 1 - exp(-delta / t1);
  x1n := exp(-delta / t1) * x1 + G * a * input;
  fOutput := fOutput + delta * a * x1 + G * (delta - a * t1) * input;
  x1 := x1n;
end;

{ TInt }

function TInt.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TInt.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TInt.Destroy;
begin
  inherited Destroy;
end;

procedure TInt.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := fOutput + G * delta * input;
end;

{ TPSub }

procedure TPSub.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := G * (input1 - input2);
end;

function TPSub.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPSub.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPSub.Destroy;
begin
  inherited Destroy;
end;

{ TPAdd }

procedure TPAdd.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := G * (input1 + input2);
end;

function TPAdd.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPAdd.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPAdd.Destroy;
begin
  inherited Destroy;
end;

{ TPMul }

procedure TPMul.simulate;
begin
  assert(G >= 0, kError101);
  fOutput := G * input1 * input2;
end;

function TPMul.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPMul.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPMul.Destroy;
begin
  inherited Destroy;
end;

{ TPDiv }

procedure TPDiv.simulate;
begin
  assert(G >= 0, kError101);
  assert(input2 <> 0, kError105);
  fOutput := G * input1 / input2;
end;

function TPDiv.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPDiv.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPDiv.Destroy;
begin
  inherited Destroy;
end;

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
    fOutput := G * xt[nt - 1];
    xt[0] := input;
  end;
end;

function TPT0.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPT0.Create;
begin
  inherited Create;
  G := 1;
  fOutput := 0;
end;

destructor TPT0.Destroy;
begin
  inherited Destroy;
end;

{ TPT1 }

function TPT1.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

procedure TPT1.simulate;
var
  f: extended;
begin
  assert((G >= 0) and (t1 >=0), kError101);
  f := exp(-delta / t1);
  fOutput := f * x1 + G * (1 - f) * input;
  x1 := fOutput;
end;

constructor TPT1.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  fOutput := 0;
end;

destructor TPT1.Destroy;
begin
  inherited Destroy;
end;

{ TPT2 }

function TPT2.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TPT2.Create;
begin
  inherited Create;
  G := 1;
  x1 := 0;
  x2 := 0;
  fOutput := 0;
end;

destructor TPT2.Destroy;
begin
  inherited Destroy;
end;

procedure TPT2.simulate;
var
  a, b, c, d, e, f, h, o, k, omg: extended;
  xn1, xn2, x1n, x2n, tau1, tau2: extended;
begin
  if dmp < 1 then
    begin
      omg := 1 / t2;
      a := exp(-delta * delta * omg);
      b := sqrt(1 - dmp * dmp) * omg;
      c := arctan(dmp * omg / b);
      d := omg * omg;
      e := d * omg / b * a * cos(b * delta + c);
      f := d / b * a * sin(b * delta);
      k := f * 2 * dmp / omg;
      xn1 := x1 * e / d - x2 * f + f * g * input;
      xn2 := x1 * f / d + x2 * e / d + x2 * k - (e / d - 1 + k) * G * input;
      x1 := xn1;
      x2 := xn2;
      fOutput := x2;
    end
  else if dmp = 1 then
    begin
      a := exp(-delta / t2);
      x1n := a * x1 + G * (1 - a) * input;
      x2n := a * x2 + (1 - a) * x1n;
      x1 := x1n;
      x2 := x2n;
      fOutput := x2;
    end
  else
    begin
      omg := 1 / t2;
      a := sqrt(dmp * dmp - 1);
      tau1 := (dmp + a) / omg;
      tau2 := (dmp - a) / omg;
      b := exp(-delta / tau1);
      c := exp(-delta / tau2);
      d := b - c;
      e := tau1 - tau2;
      f := x2 - G * input;
      k := 2 * dmp * omg;
      h := tau1 * c - tau2 * b;
      o := omg * omg;
      xn1 := (h * x1 - d * f) / e;
      xn2 := (d * (x1 + g * f) + h * f * o) / (o * e) + G * input;
      x1 := xn1;
      x2 := xn2;
      fOutput := x2;
    end;
end;

  { TP }

function TP.SimAndGetOutput: extended;
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
  G := 1;
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
