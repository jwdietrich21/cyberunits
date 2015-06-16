unit GUI;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for linear feedback systems }
{ GUI }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, ExtCtrls, Spin, SimulationEngine, Prediction;

type

  { TValuesForm }

  TValuesForm = class(TForm)
    IterationsSpinEdit: TSpinEdit;
    G1Label: TLabel;
    G1Edit: TFloatSpinEdit;
    zLabel: TLabel;
    xLabel: TLabel;
    xSpinEdit: TFloatSpinEdit;
    StartButton: TButton;
    ValuesGrid: TStringGrid;
    ToolBar1: TToolBar;
    IterationsLabel: TLabel;
    G2Label: TLabel;
    zSpinEdit: TFloatSpinEdit;
    G2Edit: TFloatSpinEdit;
    procedure StartButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ValuesForm: TValuesForm;

implementation

{$R *.lfm}

{ TValuesForm }

procedure TValuesForm.StartButtonClick(Sender: TObject);
var
  x, z: extended;
  i, j, iterations: integer;
begin
  x := xSpinEdit.Value;
  z := zSpinEdit.Value;
  iterations := IterationsSpinEdit.Value;
  gValues := TValues.Create;
  ValuesGrid.RowCount := 26;
  for i := 0 to ValuesGrid.ColCount - 1 do
    for j := 1 to ValuesGrid.RowCount - 1 do
      ValuesGrid.Cells[i, j] := '';
  RunSimulation(x, z, G1Edit.Value, G2Edit.Value, iterations);
  PredictionForm.DisplayPrediction(gPrediction);
  if iterations > ValuesGrid.RowCount then
    ValuesGrid.RowCount := iterations + 1;
  for i := 0 to iterations - 1 do
  begin
    ValuesGrid.Cells[0, i + 1] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 1] := FloatToStrF(gValues.x[i], ffFixed, 0, 4);
    ValuesGrid.Cells[2, i + 1] := FloatToStrF(gValues.z[i], ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 1] := FloatToStrF(gValues.e[i], ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 1] := FloatToStrF(gValues.y[i], ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 1] := FloatToStrF(gValues.ys[i], ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 1] := FloatToStrF(gValues.yr[i], ffFixed, 0, 4);
  end;
  gValues.Destroy;
end;

end.

