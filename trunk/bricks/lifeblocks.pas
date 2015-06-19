unit lifeblocks;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ LifeBlocks: Metabricks for processing structures in organisms }

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
  Classes, SysUtils, bricks;

type

  { TASIA }

  TASIA = class(TBlock)
  protected
    PT1Analog: TPT1;
    FAlpha, FBeta: extended;
    function SimAndGetOutput: extended;
    procedure SetAlpha(AValue: extended);
    procedure SetBeta(AValue: extended);
    procedure SetDelta(AValue: extended);
  public
    input: extended;
    constructor Create;
    destructor Destroy; override;
    property alpha: extended read FAlpha write SetAlpha;
    property beta: extended read FBeta write SetBeta;
    property delta: extended write SetDelta;
    property output: extended read Foutput;
    procedure simulate; override;
    property simOutput: extended read SimAndGetOutput;
  end;

implementation

{ TASIA }

procedure TASIA.SetAlpha(AValue: extended);
begin
  FAlpha := AValue;
  PT1Analog.G := FAlpha / FBeta;
end;

procedure TASIA.SetBeta(AValue: extended);
begin
  FBeta := AValue;
  PT1Analog.G := FAlpha / FBeta;
  PT1Analog.t1 := 1 / FBeta;
end;

procedure TASIA.SetDelta(AValue: extended);
begin
  PT1Analog.delta := AValue;
end;

function TASIA.SimAndGetOutput: extended;
begin
  simulate;
  result := fOutput;
end;

constructor TASIA.Create;
begin
  inherited create;
  PT1Analog := TPT1.Create;
  FAlpha := 1;
  FBeta := 1;
end;

destructor TASIA.Destroy;
begin
  PT1Analog.Create;
  inherited Destroy;
end;

procedure TASIA.simulate;
begin
  PT1Analog.input := input;
  fOutput := PT1Analog.simOutput;
end;

end.

