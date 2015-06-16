unit IPS;

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

