unit plot;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ plot: draws signals in frequency domain  }

{ Version 1.1.0 (Director) }

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
  TAChartExtentLink, TATools, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, StdCtrls, LCLType, Grids, Math, Bricks, plots, ts;

const
  MIN_X = 0.1;
  MAX_X = 10;
  RESOLUTION = 50;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    TSButton: TButton;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    BlockTypeComboBox: TComboBox;
    PhaseChart: TChart;
    PhaseChartLineSeries: TLineSeries;
    AppleMenu: TMenuItem;
    AmplitudeChart: TChart;
    AmplitudeChartLineSeries: TLineSeries;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ImageList: TImageList;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    frGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure MacAboutItemClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ShowAboutWindow(Sender: TObject);
    procedure BlockTypeComboBoxChange(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure DrawPlot(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure TSButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    omega, M, phi: TVector;
    inputSignal, outputSignal, time: TMatrix;
    { public declarations }
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  PlotForm.WinAboutItem.Visible := False;
  PlotForm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  PlotForm.WinAboutItem.Visible := True;
  PlotForm.AppleMenu.Visible := False;
  {$ENDIF}
  PlotForm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  PlotForm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  PlotForm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  PlotForm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  PlotForm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  PlotForm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  PlotForm.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  PlotForm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  PlotForm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  PlotForm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TPlotForm.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('Simple demonstration for Bode plot with Bricks');
end;

procedure TPlotForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender)
end;

procedure TPlotForm.MenuItem1Click(Sender: TObject);
begin

end;

procedure TPlotForm.DrawPlot(Sender: TObject);
var
  theBlock: TControlledBlock;
  i: integer;
begin
  if BlockTypeComboBox.Caption = 'PT0 (exact)' then begin
    theBlock := TPT0.Create;
    theBlock.G := 1;
    TPT0(theBlock).nt := 10;
    TPT0(theBlock).delta := 0.1;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'PT0 (simulated)' then begin
    theBlock := TPT0.Create;
    theBlock.G := 1;
    TPT0(theBlock).nt := 10;
    TPT0(theBlock).delta := 0.1;
    theBlock.amplitude := 1;
    SimBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi, inputSignal, outputSignal, time);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'PT1 (exact)' then begin
    theBlock := TPT1.Create;
    theBlock.G := 1;
    TPT1(theBlock).t1 := 5;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'PT1 (simulated)' then begin
    theBlock := TPT1.Create;
    theBlock.G := 1;
    TPT1(theBlock).t1 := 5;
    theBlock.amplitude := 1;
    SimBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi, inputSignal, outputSignal, time);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'PT2 (exact)' then begin
    theBlock := TPT2.Create;
    theBlock.G := 1;
    TPT2(theBlock).t2 := 1;
    TPT2(theBlock).dmp := 0.5;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'PT2 (simulated)' then begin
    theBlock := TPT2.Create;
    theBlock.G := 1;
    TPT2(theBlock).t2 := 1;
    TPT2(theBlock).dmp := 0.5;
    theBlock.amplitude := 1;
    SimBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi, inputSignal, outputSignal, time);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'DT1 (exact)' then begin
    theBlock := TDT1.Create;
    theBlock.G := 1;
    TDT1(theBlock).t1 := 1;
    TDT1(theBlock).delta := 1;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'Integrator (I, exact)' then begin
    theBlock := TInt.Create;
    theBlock.G := 1;
    TInt(theBlock).delta := 1;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'IT1 (exact)' then begin
    theBlock := TIT1.Create;
    theBlock.G := 1;
    TIT1(theBlock).delta := 1;
    TIT1(theBlock).t1 := 2;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end
  else if BlockTypeComboBox.Caption = 'IT2 (exact)' then begin
    theBlock := TIT2.Create;
    theBlock.G := 1;
    TIT2(theBlock).delta := 1;
    TIT2(theBlock).t2 := 2;
    TIT2(theBlock).dmp := 0.1;
    theBlock.amplitude := 1;
    DrawBodePlot(theBlock, AmplitudeChartLineSeries, PhaseChartLineSeries,
      MIN_X, MAX_X, RESOLUTION, omega, M, phi);
    AmplitudeChart.AxisList.BottomAxis.Range.Min := MIN_X;
    AmplitudeChart.AxisList.BottomAxis.Range.Max := MAX_X;
    PhaseChart.AxisList.BottomAxis.Range.Min := MIN_X;
    PhaseChart.AxisList.BottomAxis.Range.Max := MAX_X;
    theBlock.Destroy;
  end;
  frGrid.RowCount := Length(omega) + 2;
  for i := 1 to Length(omega) do
    begin
      frGrid.Cells[0, i] := FloatToStrF(omega[i - 1], ffFixed, 2, 2);
      frGrid.Cells[1, i] := FloatToStrF(M[i - 1], ffFixed, 2, 2);
      frGrid.Cells[2, i] := FloatToStrF(phi[i - 1], ffFixed, 2, 2);
    end;
end;

procedure TPlotForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
  frGrid.Cells[0, 0] := 'omega';
end;

procedure TPlotForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TPlotForm.TSButtonClick(Sender: TObject);
begin
  TimeSeriesForm.inputSignal := inputSignal;
  TimeSeriesForm.outputSignal := outputSignal;
  TimeSeriesForm.time := time;
  TimeSeriesForm.minFreq := MIN_X;
  TimeSeriesForm.maxFreq := MAX_X;
  TimeSeriesForm.resolution := RESOLUTION;
  TimeSeriesForm.Redraw(Sender);
  TimeSeriesForm.Show;
end;

procedure TPlotForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TPlotForm.BlockTypeComboBoxChange(Sender: TObject);
begin
  AmplitudeChartLineSeries.Clear;
  PhaseChartLineSeries.Clear;
  if pos('(simulated)', BlockTypeComboBox.Caption) <> 0 then
    TSButton.Enabled := true
  else
    TSButton.Enabled := false;
  DrawPlot(Sender);
end;

procedure TPlotForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

end.

