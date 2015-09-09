unit ts;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ ts: draws signals in time domain }

{ Version 1.1.0 (Corvus) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs;

type

  { TTimeSeriesForm }

  TTimeSeriesForm = class(TForm)
    InputLineSeries: TLineSeries;
    OutputLineSeries: TLineSeries;
    InputTSChart: TChart;
    OutputTSChart: TChart;
    procedure Redraw(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TimeSeriesForm: TTimeSeriesForm;

implementation

{$R *.lfm}

{ TTimeSeriesForm }

procedure TTimeSeriesForm.Redraw(Sender: TObject);
begin
  InputLineSeries.Clear;
  OutputLineSeries.Clear;
end;

end.

