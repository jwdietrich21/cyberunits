unit Plot;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Simulator for Hill-NoCoDI loop with n = 2 }
{ Plot unit }

{ Version 1.1.0 (Dendron) }

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
  Controls, Graphics, Dialogs, Spin, StdCtrls, SimulationEngine;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    ySeries: TLineSeries;
    ChartLegendPanel1: TChartLegendPanel;
    eSeries: TLineSeries;
    cSeries: TLineSeries;
    uSeries: TLineSeries;
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
    ySeries.AddXY(i, gValues.y[i]);
    eSeries.AddXY(i, gValues.e[i]);
    cSeries.AddXY(i, gValues.c[i]);
    uSeries.AddXY(i, gValues.u[i]);
    yrSeries.AddXY(i, gValues.yr[i]);
  end;
end;

end.

