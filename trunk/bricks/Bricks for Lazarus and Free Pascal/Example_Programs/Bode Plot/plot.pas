unit plot;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

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
  Classes, SysUtils, FileUtil, TASources, TAGraph, TASeries, TATransformations,
  Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, StdCtrls, Bricks, plots;

type

  { TForm1 }

  TForm1 = class(TForm)
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    BlockTypeComboBox: TComboBox;
    PhaseChart: TChart;
    PhaseChartLineSeries: TLineSeries;
    AppleMenu: TMenuItem;
    AmplitudeChart: TChart;
    AmplitudeChartLineSeries: TLineSeries;
    DrawButton: TButton;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure BlockTypeComboBoxChange(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure DrawButtonClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DrawButtonClick(Sender: TObject);
const
  MIN_X = 0.1;
  MAX_X = 1.5;
var
  theBlock: TControlledBlock;
begin
  if BlockTypeComboBox.Caption = 'PT0' then begin
    theBlock := TPT0.Create;
    theBlock.G := 1;
    TPT0(theBlock).nt := 100;
    TPT0(theBlock).delta := 1;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries, MIN_X, MAX_X);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'PT1' then begin
    theBlock := TPT1.Create;
    theBlock.G := 1;
    TPT1(theBlock).t1 := 1;
    TPT1(theBlock).delta := 1;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries, MIN_X, MAX_X);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end;
end;

procedure TForm1.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TForm1.BlockTypeComboBoxChange(Sender: TObject);
begin
  AmplitudeChartLineSeries.Clear;
  PhaseChartLineSeries.Clear;
end;

procedure TForm1.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

end.

