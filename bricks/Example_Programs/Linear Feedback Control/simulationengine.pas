unit SimulationEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, bricks;

type

  TValues = class
  protected
    procedure SetSize(aValue: integer);
  public
    x, z, e, y, yr, ys: array of extended;
    constructor Create;
    destructor Destroy;
    property size: integer write SetSize;
  end;

  TBlocks = record
    G1, G2: TP;
    Comparator: TPSub;
    LoadInjection: TPAdd;
  end;

  TPrediction = record
    x, z, e, y, yr, ys: extended;
  end;

var
  gValues: TValues;
  gBlocks: TBlocks;
  gPrediction: TPrediction;

procedure RunSimulation(x, z, G1, G2: extended; nmax: integer);

implementation

procedure RunSimulation(x, z, G1, G2: extended; nmax: integer);
var
  e, y, yr, ys: extended;
  i: integer;
begin
  if nmax > 0 then
  begin
    gPrediction.x := x;
    gPrediction.z := z;
    gPrediction.y := (G1 * x + z) / (1 + G1 * G2);
    gPrediction.yr := G2 * gPrediction.y;
    gPrediction.e := gPrediction.x - gPrediction.yr;
    gPrediction.ys := G1 * gPrediction.e;
    gValues.size := 0; // delete content
    gValues.size := nmax;
    gBlocks.G1 := TP.Create;
    gBlocks.G2 := TP.Create;
    gBlocks.Comparator := TPSub.Create;
    gBlocks.LoadInjection := TPAdd.Create;
    gBlocks.G1.G := G1;
    gBlocks.G2.G := G2;
    yr := 20;
    for i := 0 to nmax - 1 do
    begin
      gBlocks.Comparator.input1 := x;
      gBlocks.Comparator.input2 := yr;
      e := gBlocks.Comparator.simOutput;
      gBlocks.G1.input := e;
      ys := gBlocks.G1.simOutput;
      gBlocks.LoadInjection.input1 := ys;
      gBlocks.LoadInjection.input2 := z;
      y := gBlocks.LoadInjection.simOutput;
      gBlocks.G2.input := y;
      yr := gBlocks.G2.simOutput;
      gValues.x[i] := x;
      gValues.z[i] := z;
      gValues.e[i] := e;
      gValues.y[i] := y;
      gValues.yr[i] := yr;
      gValues.ys[i] := ys;
      application.ProcessMessages;
    end;
    gBlocks.G1.Destroy;
    gBlocks.G2.Destroy;
    gBlocks.Comparator.Destroy;
    gBlocks.LoadInjection.Destroy;
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

