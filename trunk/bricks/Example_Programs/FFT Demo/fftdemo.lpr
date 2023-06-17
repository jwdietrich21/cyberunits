program fftdemo;

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, gui
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'FFT Demo';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDemoMainForm, DemoMainForm);
  Application.Run;
end.

