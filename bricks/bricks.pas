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

