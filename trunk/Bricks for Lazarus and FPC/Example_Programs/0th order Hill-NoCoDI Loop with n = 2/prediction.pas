unit Prediction;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Simulator for Hill-NoCoDI loop with n = 2 }
{ This unit displays predicted values as delivered by equilibrium analysis }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  Grids, SimulationEngine;

type

  { TPredictionForm }

  TPredictionForm = class(TForm)
    PredictionList: TStringGrid;
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DisplayPrediction(Parameters1, Parameters2, Parameters3: TPrediction);
  end;

var
  PredictionForm: TPredictionForm;

implementation

{$R *.lfm}

procedure TPredictionForm.FormPaint(Sender: TObject);
begin
  PredictionList.Cells[0, 1] := 'x';
  PredictionList.Cells[0, 2] := 'e';
  PredictionList.Cells[0, 3] := 'c';
  PredictionList.Cells[0, 4] := 'u';
  PredictionList.Cells[0, 5] := 'y';
  PredictionList.Cells[0, 6] := 'yr';
end;

procedure TPredictionForm.DisplayPrediction(Parameters1, Parameters2, Parameters3: TPrediction);
begin
  PredictionList.Cells[1, 1] := FloatToStrF(Parameters1.x, ffFixed, 0, 4);
  PredictionList.Cells[1, 2] := FloatToStrF(Parameters1.e, ffFixed, 0, 4);
  PredictionList.Cells[1, 3] := FloatToStrF(Parameters1.c, ffFixed, 0, 4);
  PredictionList.Cells[1, 4] := FloatToStrF(Parameters1.u, ffFixed, 0, 4);
  PredictionList.Cells[1, 5] := FloatToStrF(Parameters1.y, ffFixed, 0, 4);
  PredictionList.Cells[1, 6] := FloatToStrF(Parameters1.yr, ffFixed, 0, 4);
  PredictionList.Cells[2, 1] := FloatToStrF(Parameters2.x, ffFixed, 0, 4);
  PredictionList.Cells[2, 2] := FloatToStrF(Parameters2.e, ffFixed, 0, 4);
  PredictionList.Cells[2, 3] := FloatToStrF(Parameters2.c, ffFixed, 0, 4);
  PredictionList.Cells[2, 4] := FloatToStrF(Parameters2.u, ffFixed, 0, 4);
  PredictionList.Cells[2, 5] := FloatToStrF(Parameters2.y, ffFixed, 0, 4);
  PredictionList.Cells[2, 6] := FloatToStrF(Parameters2.yr, ffFixed, 0, 4);
  PredictionList.Cells[3, 1] := FloatToStrF(Parameters3.x, ffFixed, 0, 4);
  PredictionList.Cells[3, 2] := FloatToStrF(Parameters3.e, ffFixed, 0, 4);
  PredictionList.Cells[3, 3] := FloatToStrF(Parameters3.c, ffFixed, 0, 4);
  PredictionList.Cells[3, 4] := FloatToStrF(Parameters3.u, ffFixed, 0, 4);
  PredictionList.Cells[3, 5] := FloatToStrF(Parameters3.y, ffFixed, 0, 4);
  PredictionList.Cells[3, 6] := FloatToStrF(Parameters3.yr, ffFixed, 0, 4);
end;

end.

