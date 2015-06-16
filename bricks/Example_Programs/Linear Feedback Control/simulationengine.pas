unit SimulationEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, bricks;

type

  { TValues }

  TValues = class
  protected
    procedure SetSize(aValue: integer);
  public
    x, z, e, y, yr, ys: array of extended;
    constructor Create;
    destructor Destroy;
    property size: integer write SetSize;
  end;

var
  gValues: TValues;

procedure RunSimulation(x, z: extended; nmax: integer);

implementation

procedure RunSimulation(x, z: extended; nmax: integer);
var
  e, y, yr, ys: extended;
  i: integer;
begin
  if nmax > 0 then
    for i := 0 to nmax - 1 do
    begin
      gValues.x[i] := x;
      gValues.z[i] := z;
      gValues.y[i] := y;
      gValues.e[i] := e;
      gValues.yr[i] := yr;
      gValues.ys[i] := ys;
      application.ProcessMessages;
    end;
end;

{ TValues }

procedure TValues.SetSize(aValue: integer);
begin
  SetLength(e, aValue);
  SetLength(x, aValue);
  SetLength(y, aValue);
  SetLength(z, aValue);
  SetLength(yr, aValue);
  SetLength(ys, aValue);
end;

constructor TValues.Create;
begin
  inherited Create;
end;

destructor TValues.Destroy;
begin
  inherited Destroy;
end;

end.

