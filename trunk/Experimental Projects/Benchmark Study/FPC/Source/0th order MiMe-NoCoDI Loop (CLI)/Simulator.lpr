program Simulator;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Simulator for MiMe-NoCoDI loop }
{ Main program }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ CLI version of simulation program for purposes of benchmarking }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, Unix,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  SimulationEngine,
  CLIServices, HighResTime;

const
  kTAB = char(9);

type

  { SimulationApp }

  SimulationApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    x: extended;
    iterations: integer;
    G1, G2, G3, D2: extended;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { SimulationApp }

  procedure SimulationApp.DoRun;
  const
    precision = 2;
    digits = 4;
  var
    errorMsg: string;
    showTimeSeries: boolean;
    resultLine: tResultContent;
    i: integer;
    startTime, stopTime, deltaTime: real;
  begin
    iterations := 30;
    // quick check parameters
    ErrorMsg := CheckOptions('hbi:', ['help', 'benchmark', 'iterations:']);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('b', 'benchmark') then
      ShowTimeSeries := False
    else
      ShowTimeSeries := True;

    if HasOption('i', 'iterations') then
      iterations := StrToIntDef(GetOptionValue('i', 'iterations'), 30);

    writeln();
    writeln('CyberUnits Bricks demo application');
    writeln('MiMe NoCoDI loop demo (CLI version)');
    writeln();

    x := 5;

    G1 := 2.6;
    G2 := 5.0;
    G3 := 0.3;
    D2 := 0.5;

    write('Iterations: ');
    writeln(iterations);

    startTime := TimeInMilliseconds;

    gValues := TValues.Create;

    RunSimulation(x, G1, G2, G3, D2, iterations);

    if ShowTimeSeries then
    begin
      write('x: ');
      writeln(FloatToStrF(x, ffFixed, precision, digits));
      WriteTableHeader;
      for i := 0 to length(gValues.x) - 1 do
      begin
        resultLine[0] := IntToStr(i);
        resultLine[1] := FormatFloat(gNumberFormat, gValues.e[i]);
        resultLine[2] := FormatFloat(gNumberFormat, gValues.c[i]);
        resultLine[3] := FormatFloat(gNumberFormat, gValues.y[i]);
        resultLine[4] := FormatFloat(gNumberFormat, gValues.yr[i]);
        WriteTableLine(resultLine);
      end;
    end;

    gValues.Destroy;

    writeln();

    stopTime := TimeInMilliseconds;

    deltaTime := stopTime - startTime;

    write('Elapsed time for simulation: ');
    writeln(FloatToStr(deltaTime) + ' ms');

    writeln();

    // stop program loop
    Terminate;
  end;

  constructor SimulationApp.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor SimulationApp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure SimulationApp.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: SimulationApp;

  {$R *.res}

begin
  Application := SimulationApp.Create(nil);
  Application.Title := 'Simulator';
  Application.Run;
  Application.Free;
end.
