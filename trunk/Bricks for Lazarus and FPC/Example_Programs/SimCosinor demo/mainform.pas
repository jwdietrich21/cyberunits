unit MainForm;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ LifeBlocks: Metabricks for information processing structures in organisms }

{ Version 2.2.0 (Graffiti Street) }

{ (c) Johannes W. Dietrich, 1994 - 2026 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2026 }

{ SimCosinor: Demo program for the simulation of cosinor-based timeseries }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls, Menus,
  TAGraph, TASeries, Bricks, lifeblocks;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    TimeDomainChartLineSeries1: TLineSeries;
    TimeDomainChart: TChart;
    AmplitudeFloatSpinEdit: TFloatSpinEdit;
    AcrophaseFloatSpinEdit: TFloatSpinEdit;
    AmplitudeLabel: TLabel;
    AcrophaseLabel: TLabel;
    TauFloatSpinEdit: TFloatSpinEdit;
    TauLabel: TLabel;
    MesorLabel: TLabel;
    MesorFloatSpinEdit: TFloatSpinEdit;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure AcrophaseFloatSpinEditChange(Sender: TObject);
    procedure AmplitudeFloatSpinEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MesorFloatSpinEditChange(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure TauFloatSpinEditChange(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private

  public
    procedure Simulate;
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.MesorFloatSpinEditChange(Sender: TObject);
begin
  Simulate;
end;

procedure TMainWindow.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainWindow.TauFloatSpinEditChange(Sender: TObject);
begin
  Simulate;
end;

procedure TMainWindow.AmplitudeFloatSpinEditChange(Sender: TObject);
begin
  Simulate;
end;

procedure TMainWindow.FormShow(Sender: TObject);
begin
  Simulate;
end;

procedure TMainWindow.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('Simple demo for cosinor-based rhythm generators');
end;

procedure TMainWindow.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender)
end;

procedure TMainWindow.AcrophaseFloatSpinEditChange(Sender: TObject);
begin
  Simulate;
end;

procedure TMainWindow.Simulate;
const
  n = 500;                    // Number of samples
var
  Model: TModel;
  RhythmGenerator: TCosinor;
  totalTime, dt: extended;
  i: integer;
  ts, samplingTime: array of real;
begin
  TimeDomainChartLineSeries1.Clear;
  Model := TModel.Create;
  RhythmGenerator := TCosinor.Create;
  RhythmGenerator.amplitude := AmplitudeFloatSpinEdit.Value;
  RhythmGenerator.mesor := MesorFloatSpinEdit.Value;
  RhythmGenerator.acrophase := AcrophaseFloatSpinEdit.Value;
  RhythmGenerator.tau := TauFloatSpinEdit.Value;
  RhythmGenerator.updateTime := true;
  totalTime := RhythmGenerator.tau;
  dt := totalTime / n;
  Model.delta := dt;
  Model.firstBlock := RhythmGenerator;
  RhythmGenerator.model := Model;
  RhythmGenerator.delta := Model.delta;
  SetLength(samplingTime, n); // simulated time
  SetLength(ts, n);           // time series
  TimeDomainChartLineSeries1.BeginUpdate;
  for i := 0 to n - 1 do
  begin
    samplingTime[i] := i * dt;
    ts[i] := RhythmGenerator.simOutput;
    TimeDomainChartLineSeries1.AddXY(samplingTime[i], ts[i]);
  end;
  TimeDomainChartLineSeries1.EndUpdate;
  RhythmGenerator.Destroy;
  Model.Destroy
end;

end.

