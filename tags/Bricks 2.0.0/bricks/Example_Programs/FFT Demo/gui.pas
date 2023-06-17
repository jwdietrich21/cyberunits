unit gui;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 2.0.0 (Escorpión) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

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
  StdCtrls, PairSplitter, ExtCtrls, Menus, SignalAnalysis, Math, ucomplex,
  TASeries;

type

  { TDemoMainForm }

  TDemoMainForm = class(TForm)
    AppleMenu: TMenuItem;
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
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure DemoButtonClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
    procedure writeMemo(theString: string);
    procedure writeMemoLine(theString: string);
    procedure writectable(l: table);
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

  ConstantPower = 1; // Power of constant component (DC like, relative)
  PeriodicPower: array[1..2] of real = (10, 4); // Power of components
  PeriodicFreq: array[1..2] of integer = (5, 20); // Frequency of components (Hz)
  PeriodicDelay: array[1..2] of integer = (0, 0); // Delay of components (radians)

  Scope = 256;           // Window of time series to be inspected (power of 2)
  Scaling = Scope / n;

  Omega = 2 * pi;
var
  i, j: integer;
  f, samplingTime, mag, ts: array of real;
  trajectory, cfreqVec: table;
begin
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
                IntToStr(PeriodicFreq[1]) + '*t) + ' +
                FloatToStr(PeriodicPower[2]) + '*sin(2*pi*' +
                IntToStr(PeriodicFreq[2]) + '*t)');

  WriteCTable(cfreqVec);

  SetLength(mag, Scope);
  for i := 0 to Scope do
    mag[i] := sqrt(cfreqVec[i].re * cfreqVec[i].re + cfreqVec[i].im * cfreqVec[i].im) * 2 / Scope;

  TimeDomainChartLineSeries1.BeginUpdate;
  for i := 0 to Scope do
    TimeDomainChartLineSeries1.AddXY(samplingTime[i], ts[i]);
  TimeDomainChartLineSeries1.EndUpdate;

  FrequencyDomainChartLineSeries1.BeginUpdate;
  for i := 0 to Scope div 2 do
    FrequencyDomainChartLineSeries1.AddXY(f[i], mag[i]);
  FrequencyDomainChart.AxisList[1].Range.Max := 50;
  FrequencyDomainChart.AxisList[1].Range.UseMax := true;
  FrequencyDomainChartLineSeries1.EndUpdate;

  SetLength(trajectory, 0);
end;

procedure TDemoMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('Simple demo for FFT');
end;

procedure TDemoMainForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
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
  tmpString1, tmpString2, tmpString3: String;
begin
  for x := 0 to length(l) - 1 do
  begin
    WriteStr(tmpString1, format('%3.3g ', [l[x].re]));
    if (l[x].im >= 0.0) then tmpString2 := '+' else tmpString2 := '';
    WriteStr(tmpString3, format('%3.5gi', [l[x].im]));
    writeMemo(tmpString1 + tmpString2 + tmpString3)
  end;
end;


end.
