unit ts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs;

type

  { TTimeSeriesForm }

  TTimeSeriesForm = class(TForm)
    InputLineSeries: TLineSeries;
    OutputLineSeries: TLineSeries;
    TSChart: TChart;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TimeSeriesForm: TTimeSeriesForm;

implementation

{$R *.lfm}

end.

