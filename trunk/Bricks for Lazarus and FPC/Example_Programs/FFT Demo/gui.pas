unit gui;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2026 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2026 }

{ Demo for FFT algorithm }

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
  Classes, SysUtils, FileUtil, TAGraph, Forms, Controls, Graphics, Dialogs,
  StdCtrls, PairSplitter, ExtCtrls, Menus, ComCtrls, SignalAnalysis, Math,
  ucomplex, TASeries;

type

  { TDemoMainForm }

  TDemoMainForm = class(TForm)
    AppleMenu: TMenuItem;
    ScopeEdit: TEdit;
    EquationComboBox: TComboBox;
    EquationLabeo: TLabel;
    TimeDomainChart: TChart;
    TimeDomainChartLineSeries1: TLineSeries;
    FrequencyDomainChart: TChart;
    FrequencyDomainChartLineSeries1: TLineSeries;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DemoButton: TButton;
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
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    ResultsMemo: TMemo;
    SaveMenuItem: TMenuItem;
    ScopeTrackBar: TTrackBar;
    UndoMenuItem: TMenuItem;
    ScopeUpDown: TUpDown;
    WinAboutItem: TMenuItem;
    procedure DemoButtonClick(Sender: TObject);
    procedure EquationComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure ScopeTrackBarChange(Sender: TObject);
    procedure ScopeUpDownChanging(Sender: TObject; var AllowChange: Boolean);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
    procedure writeMemo(theString: string);
    procedure writeMemoLine(theString: string);
    procedure writectable(l: table);
    procedure DisplayScope(Sender: TObject);
  public
    { public declarations }
  end;

var
  DemoMainForm: TDemoMainForm;

implementation

{$R *.lfm}

{ TDemoMainForm }

procedure TDemoMainForm.DemoButtonClick(Sender: TObject);
const
  time = 5;              // Duration of recording (seconds)
  n = 500;               // Number of samples
  dt = time / n;         // Resolution in seconds

  ConstantPower1 = 1; // Power of constant component (DC like, relative)
  PeriodicPower1: array[1..2] of real = (10, 4); // Power of components
  PeriodicFreq1: array[1..2] of real = (5, 20); // Frequency of components (Hz)
  PeriodicDelay1: array[1..2] of real = (0, 0); // Delay of components (radians)

  ConstantPower2 = 0;
  PeriodicPower2: array[1..2] of real = (2, 1);
  PeriodicFreq2: array[1..2] of real = (1, 10);
  PeriodicDelay2: array[1..2] of real = (0, 0);

  ConstantPower3 = 0;
  PeriodicPower3: array[1..2] of real = (1, 0.5);
  PeriodicFreq3: array[1..2] of real = (0.5, 2.5);
  PeriodicDelay3: array[1..2] of real = (0, 0);

  Omega = 2 * pi;
var
  i, j: integer;
  Scope: integer;        // Window of time series to be inspected (power of 2)
  Scaling: real;
  f, samplingTime, mag, ts: array of real;
  trajectory, cfreqVec: table;
  ConstantPower: integer;
  PeriodicPower: array[1..2] of real;
  PeriodicFreq, PeriodicDelay: array[1..2] of real;
begin
  case EquationComboBox.ItemIndex of
    0:
    begin
      ConstantPower := ConstantPower1;
      PeriodicPower := PeriodicPower1;
      PeriodicFreq := PeriodicFreq1;
      PeriodicDelay := PeriodicDelay1;
    end;
    1:
    begin
      ConstantPower := ConstantPower2;
      PeriodicPower := PeriodicPower2;
      PeriodicFreq := PeriodicFreq2;
      PeriodicDelay := PeriodicDelay2;
    end;
    2:
    begin
      ConstantPower := ConstantPower3;
      PeriodicPower := PeriodicPower3;
      PeriodicFreq := PeriodicFreq3;
      PeriodicDelay := PeriodicDelay3;
    end;
  end;
  Scope := ScopeTrackBar.Position;
  Scaling := Scope / n;
  ResultsMemo.Clear;
  TimeDomainChartLineSeries1.Clear;
  FrequencyDomainChartLineSeries1.Clear;
  SetLength(samplingTime, n); // simulated time
  SetLength(trajectory, n);   // complex time series
  SetLength(ts, n);           // real part of time series
  for i := 0 to n do
  begin
    samplingTime[i] := i * dt;
    trajectory[i].re := ConstantPower;
    for j := 1 to length(PeriodicPower) do
    begin
      trajectory[i].re :=
        trajectory[i].re + PeriodicPower[j] *
        sin(PeriodicFreq[j] * omega * samplingTime[i] + PeriodicDelay[j]);
      ts[i] := trajectory[i].re;
    end;
    trajectory[i].im := 0;
  end;

  SetLength(trajectory, Scope);
  SetLength(samplingTime, Scope);
  SetLength(f, Scope);

  for i := 0 to Scope do      // generate frequency vector
    f[i] := i / time / scaling;

  cfreqVec := fft(trajectory);

  WriteMemoLine('y := ' + IntToStr(ConstantPower) + ' + ' +
    FloatToStr(PeriodicPower[1]) + '*sin(2*pi*' +
    FloatToStr(PeriodicFreq[1]) + '*t) + ' +
    FloatToStr(PeriodicPower[2]) + '*sin(2*pi*' +
    FloatToStr(PeriodicFreq[2]) + '*t)');

  WriteCTable(cfreqVec);

  SetLength(mag, Scope);
  for i := 0 to Scope do
    mag[i] := sqrt(cfreqVec[i].re * cfreqVec[i].re + cfreqVec[i].im *
      cfreqVec[i].im) * 2 / Scope;

  TimeDomainChartLineSeries1.BeginUpdate;
  for i := 0 to Scope do
    TimeDomainChartLineSeries1.AddXY(samplingTime[i], ts[i]);
  TimeDomainChartLineSeries1.EndUpdate;

  FrequencyDomainChartLineSeries1.BeginUpdate;
  for i := 0 to Scope div 2 do
    FrequencyDomainChartLineSeries1.AddXY(f[i], mag[i]);
  FrequencyDomainChart.AxisList[1].Range.Max := 50;
  FrequencyDomainChart.AxisList[1].Range.UseMax := True;
  FrequencyDomainChartLineSeries1.EndUpdate;

  SetLength(trajectory, 0);
end;

procedure TDemoMainForm.EquationComboBoxChange(Sender: TObject);
begin
  ResultsMemo.Clear;
  TimeDomainChartLineSeries1.Clear;
  FrequencyDomainChartLineSeries1.Clear;
end;

procedure TDemoMainForm.FormCreate(Sender: TObject);
begin
  DisplayScope(Sender);
end;

procedure TDemoMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('Simple demo for FFT');
end;

procedure TDemoMainForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TDemoMainForm.ScopeTrackBarChange(Sender: TObject);
begin
  DisplayScope(Sender);
end;

procedure TDemoMainForm.ScopeUpDownChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if ScopeUpDown.Position < ScopeTrackBar.Position then
    ScopeTrackBar.Position := PrevPowerOfTwo(ScopeUpDown.Position)
  else
    ScopeTrackBar.Position := NextPowerOfTwo(ScopeUpDown.Position);
end;

procedure TDemoMainForm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure TDemoMainForm.writeMemo(theString: string);
begin
  ResultsMemo.Lines.Add(theString);
end;

procedure TDemoMainForm.writeMemoLine(theString: string);
begin
  ResultsMemo.Lines.Add(theString + LineEnding);
end;

procedure TDemoMainForm.writectable(l: table);
var
  x: integer;
  tmpString1, tmpString2, tmpString3: string;
begin
  for x := 0 to length(l) - 1 do
  begin
    WriteStr(tmpString1, format('%3.3g ', [l[x].re]));
    if (l[x].im >= 0.0) then tmpString2 := '+'
    else
      tmpString2 := '';
    WriteStr(tmpString3, format('%3.5gi', [l[x].im]));
    writeMemo(tmpString1 + tmpString2 + tmpString3);
  end;
end;

procedure TDemoMainForm.DisplayScope(Sender: TObject);
begin
  ScopeEdit.Text := IntToStr(ScopeTrackBar.Position);
  ScopeUpDown.Position := ScopeTrackBar.Position;
  if IsPowerOfTwo(ScopeTrackBar.Position) then
    ScopeEdit.Font.Color := clGreen
  else
    ScopeEdit.Font.Color := clRed;
end;


end.
