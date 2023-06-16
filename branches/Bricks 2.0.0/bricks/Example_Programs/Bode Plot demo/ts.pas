unit ts;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ ts: draws signals in time domain }

{ Version 2.0.0 (Escorpión) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Demo for Bode plot }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TATools, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, Grids, Math, Bricks;

type

  { TTimeSeriesForm }

  TTimeSeriesForm = class(TForm)
    OmegaUnitBox: TComboBox;
    OmegaValue: TEdit;
    InputLineSeries: TLineSeries;
    OmegaLabel: TLabel;
    OutputLineSeries: TLineSeries;
    InputTSChart: TChart;
    OutputTSChart: TChart;
    OmegaTrackBar: TTrackBar;
    tsGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure OmegaTrackBarChange(Sender: TObject);
    procedure OmegaUnitBoxChange(Sender: TObject);
    procedure Redraw(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    inputSignal, outputSignal, time: TMatrix;
    minFreq, maxFreq: extended;
    resolution: integer;
  end;

var
  TimeSeriesForm: TTimeSeriesForm;

implementation

{$R *.lfm}

{ TTimeSeriesForm }

procedure TTimeSeriesForm.Redraw(Sender: TObject);
var
  i, j, l, k: longint;
  omega, diff, lowFreq: extended;
begin
  InputLineSeries.Clear;
  OutputLineSeries.Clear;
  l := length(inputSignal);
  k := length(inputSignal[1]);
  diff := maxFreq - minFreq;
  OmegaTrackbar.Max := l - 1;
  i := OmegaTrackbar.Position;
  if (l > 0) and (k > 0) and (i <= l) then
    for j := 0 to k - 1 do
    begin
      if not isNaN(inputSignal[i, j]) then
        TimeSeriesForm.InputLineSeries.AddXY(time[i, j], inputSignal[i, j]);
      if not isNaN(outputSignal[i, j]) then
        TimeSeriesForm.OutputLineSeries.AddXY(time[i, j], outputSignal[i, j]);
    end;
  tsGrid.RowCount := k + 2;
  for j := 1 to k - 1 do
    begin
      tsGrid.Cells[0, j] := FloatToStrF(inputSignal[i, j], ffFixed, 2, 2);
      tsGrid.Cells[1, j] := FloatToStrF(outputSignal[i, j], ffFixed, 2, 2);
    end;
  if OmegaUnitBox.ItemIndex = 0 then
    begin
      lowFreq := minFreq / (2 * pi);
      omega := lowFreq + (i) * diff / resolution / (2 * pi);
    end
  else
    begin
      lowFreq := minFreq;
      omega := lowFreq + i * diff / resolution;
    end;
  OmegaValue.Text := FloatToStrF(omega, ffGeneral, 3, 2);
end;

procedure TTimeSeriesForm.OmegaTrackBarChange(Sender: TObject);
begin
  Redraw(Sender);
end;

procedure TTimeSeriesForm.OmegaUnitBoxChange(Sender: TObject);
begin
  Redraw(Sender);
end;

procedure TTimeSeriesForm.FormCreate(Sender: TObject);
begin
  tsGrid.Cells[0, 0] := 'Input';
end;

end.

