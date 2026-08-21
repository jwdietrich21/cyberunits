unit CLIServices;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
{ Simulation Engine }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ This unit provides some global functions for use by other units }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, CRT;

const
  kLength = 8;
  kScale = 160;
  minWidth = 4;
  kTAB = char(9);
  RES_MAX_COLS = 5;       {number of columns of table in log window}
  STANDARD_NUM_FORMAT = '  ###,###.0000;-###,###.0000;  ###,###.0000';
  SHORT_NUM_FORMAT = '  ###,###.0000;-###,###.0000;  ###,###.0000';

type
  tResultContent = array[0..RES_MAX_COLS - 1] of string;

var
  gLength: integer;
  gNumberFormat: string;

procedure WriteTableHeader;
procedure WriteTableLine(theLine: tResultContent);

implementation

procedure WriteTableHeader;
var
  termWidth: integer;
begin
  {$IFDEF Windows}
  termWidth := WindMaxX;
  {$ELSE}
  termWidth := ScreenWidth;
  {$ENDIF}
  if termWidth > kScale then
    begin
    gLength := kLength;
    gNumberFormat := STANDARD_NUM_FORMAT;
    end
  else
    begin
    gLength := minWidth + termWidth div kScale * kLength;
    gNumberFormat := SHORT_NUM_FORMAT;
    end;
  writeln;
  write(AddChar(' ', 'i', gLength) + kTab + AddChar(' ', 'e(t)  ', gLength) + kTab);
  write(AddChar(' ', 'y(t)  ', gLength) + kTab + AddChar(' ', 'yS(t)  ', gLength) + kTab);
  write(AddChar(' ', 'yR(t)', gLength) + kTab);
  writeln();

end;

procedure WriteTableLine(theLine: tResultContent);
var
  i: integer;
begin
  for i := 0 to length(theLine) - 1 do
  begin
    write(AddChar(' ', theLine[i], gLength));
    write(kTab);
  end;
  writeln;
end;

end.

