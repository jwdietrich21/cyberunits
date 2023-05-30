unit Plot;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
{ Plot unit }

{ Version 1.1.1 (Dendron) }

{ (c) Johannes W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

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
  Controls, Graphics, Dialogs, SimulationEngine;

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

