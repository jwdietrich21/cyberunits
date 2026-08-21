unit HighResTime;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
{ Simulation Engine }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ This unit delivers time in fractional millisecond resolution }

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
  Classes, SysUtils
  {$IFDEF UNIX}
  , cthreads, Unix
  {$ENDIF};

function TimeInMilliseconds: real;

implementation

function TimeInMilliseconds: real;
var
  Time: TTimeVal;
begin
  FPGetTimeOfDay(@Time, nil);
  Result := Int64(Time.tv_sec) * 1000 + Int64(Time.tv_usec) / 1000;
end;

end.

