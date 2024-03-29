unit Plot;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
{ Plot unit }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TALegendPanel, Forms,
  Controls, Graphics, Dialogs, SimulationEngine, GUIServices;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    ChartLegendPanel1: TChartLegendPanel;
    zSeries: TLineSeries;
    ySeries: TLineSeries;
    eSeries: TLineSeries;
    ysSeries: TLineSeries;
    yrSeries: TLineSeries;
    xSeries: TLineSeries;
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowPlot;
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure TPlotForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
    begin
      Chart1.Color := clDefault;
      Chart1.BackColor := clDefault;
      Chart1.AxisList.Axes[0].Marks.LabelBrush.Color := clDefault;
      Chart1.AxisList.Axes[0].Marks.LabelBrush.Style := bsClear;
    end
    else
    begin
      Chart1.Color := clNone;
      Chart1.BackColor := clWhite;
      Chart1.AxisList.Axes[0].Marks.LabelBrush.Color := clWhite;
      Chart1.AxisList.Axes[0].Marks.LabelBrush.Style := bsSolid;
    end;
end;

procedure TPlotForm.ShowPlot;
var
  i: integer;
begin
  Chart1.AxisList.Axes[1].Range.Max := gValues.size - 1;
  for i := 0 to gValues.size - 1 do
  begin
    xSeries.AddXY(i, gValues.x[i]);
    zSeries.AddXY(i, gValues.z[i]);
    ySeries.AddXY(i, gValues.y[i]);
    eSeries.AddXY(i, gValues.e[i]);
    ysSeries.AddXY(i, gValues.ys[i]);
    yrSeries.AddXY(i, gValues.yr[i]);
  end;
end;

end.

