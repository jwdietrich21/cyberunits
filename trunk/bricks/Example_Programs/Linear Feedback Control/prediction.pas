unit Prediction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit, SimulationEngine;

type

  { TPredictionForm }

  TPredictionForm = class(TForm)
    PredictionList: TValueListEditor;
  private
    { private declarations }
  public
    { public declarations }
    procedure DisplayPrediction(theParameters: TPrediction);
  end;

var
  PredictionForm: TPredictionForm;

implementation

{$R *.lfm}

procedure TPredictionForm.DisplayPrediction(theParameters: TPrediction);
begin
  PredictionList.Cells[1, 1] := FloatToStrF(theParameters.x, ffFixed, 0, 4);
  PredictionList.Cells[1, 2] := FloatToStrF(theParameters.z, ffFixed, 0, 4);
  PredictionList.Cells[1, 3] := FloatToStrF(theParameters.y, ffFixed, 0, 4);
  PredictionList.Cells[1, 4] := FloatToStrF(theParameters.e, ffFixed, 0, 4);
  PredictionList.Cells[1, 5] := FloatToStrF(theParameters.ys, ffFixed, 0, 4);
  PredictionList.Cells[1, 6] := FloatToStrF(theParameters.yr, ffFixed, 0, 4);
end;

end.

