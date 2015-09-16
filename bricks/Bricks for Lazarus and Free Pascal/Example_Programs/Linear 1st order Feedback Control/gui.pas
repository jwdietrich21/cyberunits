unit GUI;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
{ GUI }

{ Version 1.1.0 (Director) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, Spin, Menus, SimulationEngine, Prediction, Plot;

type

  { TValuesForm }

  TValuesForm = class(TForm)
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
    ImageList1: TImageList;
    IterationsSpinEdit: TSpinEdit;
    G1Label: TLabel;
    G1Edit: TFloatSpinEdit;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    zLabel: TLabel;
    xLabel: TLabel;
    xSpinEdit: TFloatSpinEdit;
    StartButton: TButton;
    ValuesGrid: TStringGrid;
    ToolBar1: TToolBar;
    IterationsLabel: TLabel;
    G2Label: TLabel;
    zSpinEdit: TFloatSpinEdit;
    G2Edit: TFloatSpinEdit;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowAboutWindow(Sender: TObject);
  end;

var
  ValuesForm: TValuesForm;

implementation

{$R *.lfm}

{ TValuesForm }

procedure TValuesForm.StartButtonClick(Sender: TObject);
var
  x, z: extended;
  i, j, iterations: integer;
begin
  x := xSpinEdit.Value;
  z := zSpinEdit.Value;
  iterations := IterationsSpinEdit.Value;
  gValues := TValues.Create;
  ValuesGrid.RowCount := 26;
  for i := 0 to ValuesGrid.ColCount - 1 do
    for j := 1 to ValuesGrid.RowCount - 1 do
      ValuesGrid.Cells[i, j] := '';
  PlotForm.xSeries.Clear;
  PlotForm.zSeries.Clear;
  PlotForm.ySeries.Clear;
  PlotForm.eSeries.Clear;
  PlotForm.ysSeries.Clear;
  PlotForm.yrSeries.Clear;
  RunSimulation(x, z, G1Edit.Value, G2Edit.Value, iterations);
  PredictionForm.DisplayPrediction(gPrediction);
  if iterations > ValuesGrid.RowCount then
    ValuesGrid.RowCount := iterations + 1;
  for i := 0 to iterations - 1 do
  begin
    ValuesGrid.Cells[0, i + 1] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 1] := FloatToStrF(gValues.x[i], ffFixed, 0, 4);
    ValuesGrid.Cells[2, i + 1] := FloatToStrF(gValues.z[i], ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 1] := FloatToStrF(gValues.e[i], ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 1] := FloatToStrF(gValues.y[i], ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 1] := FloatToStrF(gValues.ys[i], ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 1] := FloatToStrF(gValues.yr[i], ffFixed, 0, 4);
  end;
  PlotForm.ShowPlot;
  gValues.Destroy;
end;

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  ValuesForm.WinAboutItem.Visible := False;
  ValuesForm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  ValuesForm.WinAboutItem.Visible := True;
  ValuesForm.AppleMenu.Visible := False;
  {$ENDIF}
  ValuesForm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  ValuesForm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  ValuesForm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  ValuesForm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  ValuesForm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  ValuesForm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  ValuesForm.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  ValuesForm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  ValuesForm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  ValuesForm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TValuesForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('Linear 1st order Feedback Control, a demonstration program for CyberUnits Bricks');
end;

procedure TValuesForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TValuesForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
end;

procedure TValuesForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

end.

