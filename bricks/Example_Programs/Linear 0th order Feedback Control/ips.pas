unit IPS;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 0th order feedback system }
{ Information Processing Structure }

{ Version 1.0.0 (Corvus) }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TIPSForm }

  TIPSForm = class(TForm)
    IPSImage: TImage;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  IPSForm: TIPSForm;

implementation

{$R *.lfm}

end.

