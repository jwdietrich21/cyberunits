unit gui;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 1.1.0 (Director) }

 { (c) Johannes W. Dietrich, 1994 - 2015 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) University of Ulm Hospitals 2002-2004 }
 { (c) Ruhr University of Bochum 2005 - 2015 }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SignalAnalysis;

type

  { TDemoMainForm }

  TDemoMainForm = class(TForm)
    DemoButton:  TButton;
    ResultsMemo: TMemo;
    procedure DemoButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DemoMainForm: TDemoMainForm;

  Data:  dataarraytype;
  didx:  dataindextype;
  fidx:  freqindextype;
  coefficients: fouriertype;
  mixed: mixedtype;

implementation

{$R *.lfm}

{ TDemoMainForm }

procedure TDemoMainForm.DemoButtonClick(Sender: TObject);
var
  theString: String;
begin
  with mixed.dataslot do
    for didx := 1 to maxarraysize do
      rp[didx] := (23 + 13 * sin(omegat(7, didx)) +
        28 * cos(omegat(22, didx)));
  fftofreal(mixed, maxarraysize);
  with mixed.coefslot do
    begin
      WriteStr(theString, 'DC = ', dcterm: 10: 2, ' ': 5, 'Noise = ', noiseterm: 10: 2);
      ResultsMemo.Lines.Add(theString + LineEnding);
    end;
  for fidx := 1 to maxfreqsize do
  begin
    with mixed.coefslot.freqterms[fidx] do
      begin
        WriteStr(theString, fidx: 4, round(cosineterm): 4, round(sineterm): 4, ' ': 4);
        ResultsMemo.Lines.Add(theString);
      end;
    if fidx mod 4 = 0 then
      ResultsMemo.Lines.Add(LineEnding);
  end;
  ResultsMemo.Lines.Add(LineEnding);
  WriteStr(theString, 'The expected result should have been:');
  ResultsMemo.Lines.Add(theString + LineEnding);
  WriteStr(theString, '  DC = 23, noise = 0, ');
  ResultsMemo.Lines.Add(theString + LineEnding);
  WriteStr(theString, '  sine 7th harmonic = 13, cosine 22nd harmonic = 28');
  ResultsMemo.Lines.Add(theString + LineEnding);
end;

end.


