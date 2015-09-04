unit gui;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of canvas using SystemsDiagram }
{ GUI }

{ Version 1.0.0 (Corvus) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1996 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lclintf, ColorBox, Menus, LCLType, SystemsDiagram;

type

  { TDemoForm }

  TDemoForm = class(TForm)
    AppleMenu: TMenuItem;
    BackgroundColorBox: TColorBox;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    DrawingColorBox: TColorBox;
    DemoButton: TButton;
    DemoImage: TImage;
    BackgroundColorLabel: TLabel;
    DrawingColorLabel: TLabel;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure DemoButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowAboutWindow(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    BlockDiagram: TBlockDiagram;
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.lfm}

{ TDemoForm }

procedure TDemoForm.DemoButtonClick(Sender: TObject);
var
  DemoBitmap: TBitmap;
  newObject, lastObject, sigmaElement, piElement: TIPSClass;
  ConnectorA, ConnectorD, ConnectorE, connectorH: TIPSClass;
  newConnection: TConnectionClass;
  theWidth: integer;
begin
  BlockDiagram := TBlockDiagram.Create;
  DemoBitmap := TBitmap.Create;
  try
    DemoBitmap.Height := DemoImage.Height;
    DemoBitmap.Width := DemoImage.Width;
    DemoBitmap.Canvas.Brush.Color := BackgroundColorBox.Selected;
    DemoBitmap.Canvas.Rectangle(0, 0, DemoBitmap.Width, DemoBitmap.Height);
    DemoBitmap.Canvas.Pen.Color := DrawingColorBox.Selected;
    BlockDiagram.canvas := DemoBitmap.Canvas;
    try
      newObject := TTerminalClass.Create;
      BlockDiagram.firstIPSObject := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 70, 35, 90, 46);
      newObject.title := 'Input 1';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := leftmiddle;
      newObject.Draw;

      lastObject := newObject;

      newObject := TPClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 120, 20, 220, 60);
      newObject.title := 'G1';
      newObject.font.Style := [fsItalic];
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := BlockDiagram.firstIPSObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := rightmiddle;
      newConnection.drainAnchor := leftmiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;

      newObject := TSigmaClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 270, 20, 320, 60);
      newObject.title := 'ignored';
      TInvertableClass(newObject).invertedSegments := [bottomSegment];
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := rightmiddle;
      newConnection.drainAnchor := leftmiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;
      sigmaElement := lastObject;

      newObject := TPiClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 320, 90, 370, 130);
      newObject.title := 'ignored';
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := rightmiddle;
      newConnection.drainAnchor := topmiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;
      piElement := lastObject;

      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 320, 170, 372, 190);
      newObject.title := 'Input 2';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := bottommiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := newObject;
      newConnection.drainObject := piElement;
      newConnection.sourceAnchor := topmiddle;
      newConnection.drainAnchor := bottommiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;
      lastObject := newConnection;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := piElement;
      newConnection.drainObject := sigmaElement;
      newConnection.sourceAnchor := leftmiddle;
      newConnection.drainAnchor := bottommiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject.Next := newConnection;


      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 450, 30, 500, 50);
      newObject.title := 'A';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := topmiddle;
      newObject.Draw;
      lastObject := newObject;
      ConnectorA := lastObject;

      newObject := TTerminalClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 500, 90, 550, 130);
      newObject.title := 'B';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := rightmiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := rightmiddle;
      newConnection.drainAnchor := topmiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;

      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 450, 150, 500, 190);
      newObject.title := 'C';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := bottommiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := bottommiddle;
      newConnection.drainAnchor := rightmiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;

      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 400, 90, 450, 130);
      newObject.title := 'D';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := leftmiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := leftmiddle;
      newConnection.drainAnchor := bottommiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;
      ConnectorD := lastObject;
      lastObject := newConnection;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := ConnectorD;
      newConnection.drainObject := ConnectorA;
      newConnection.sourceAnchor := topmiddle;
      newConnection.drainAnchor := leftmiddle;
      newConnection.chirality := cright;
      newConnection.Draw;

      lastObject.Next := newConnection;


      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 70, 190, 90, 210);
      newObject.title := 'E';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := leftmiddle;
      newObject.Draw;
      lastObject := newObject;
      ConnectorE := lastObject;

      newObject := TTerminalClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 150, 220, 170, 240);
      newObject.title := 'F';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := bottommiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := bottommiddle;
      newConnection.drainAnchor := leftmiddle;
      newConnection.chirality := cleft;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;

      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 230, 190, 252, 190);
      newObject.title := 'G';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := rightmiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := rightmiddle;
      newConnection.drainAnchor := bottommiddle;
      newConnection.chirality := cleft;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;

      newObject := TTerminalClass.Create;
      newConnection.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 150, 160, 170, 180);
      newObject.title := 'H';
      TTerminalClass(newObject).TextMargin := 5; // TextMargin not defined in parent class
      TTerminalClass(newObject).TextPosition := topmiddle;
      newObject.Draw;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := lastObject;
      newConnection.drainObject := newObject;
      newConnection.sourceAnchor := topmiddle;
      newConnection.drainAnchor := rightmiddle;
      newConnection.chirality := cleft;
      newConnection.Draw;

      lastObject := newObject;
      lastObject.Next := newConnection;
      ConnectorH := lastObject;
      lastObject := newConnection;

      newConnection := TConnectionClass.Create;
      newConnection.blockDiagram := BlockDiagram;
      newConnection.sourceObject := ConnectorH;
      newConnection.drainObject := ConnectorE;
      newConnection.sourceAnchor := leftmiddle;
      newConnection.drainAnchor := topmiddle;
      newConnection.chirality := cleft;
      newConnection.Draw;

      lastObject.Next := newConnection;

      newObject := TSigmaClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      SetRect(newObject.boundsRect, 455, 220, 475, 250);
      TInvertableClass(newObject).invertedSegments := [leftSegment, rightSegment];
      newObject.Draw;
      lastObject := newObject;

      newObject := TPiClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      TInvertableClass(newObject).invertedSegments := [topSegment, bottomSegment];
      SetRect(newObject.boundsRect, 480, 220, 530, 250);
      newObject.Draw;

      lastObject := newObject;
      newObject := TPT0Class.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 20, 630, 50);
      newObject.Draw;

      lastObject := newObject;
      newObject := TPT1Class.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 55, 630, 85);
      newObject.Draw;

      lastObject := newObject;
      newObject := TPT2Class.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 90, 630, 120);
      newObject.Draw;

      lastObject := newObject;
      newObject := TIntClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 125, 630, 155);
      newObject.Draw;

      lastObject := newObject;
      newObject := TIT1Class.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 160, 630, 190);
      newObject.Draw;

      lastObject := newObject;
      newObject := TDT1Class.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 195, 630, 225);
      newObject.Draw;

      lastObject := newObject;
      newObject := TIT2Class.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 570, 230, 630, 260);
      newObject.Draw;

      lastObject := newObject;
      newObject := TASIAClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 270, 220, 320, 250);
      newObject.Draw;

      lastObject := newObject;
      newObject := TMiMeClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 330, 220, 380, 250);
      newObject.Draw;

      lastObject := newObject;
      newObject := TNoCoDIClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      newObject.title := '';
      SetRect(newObject.boundsRect, 390, 220, 440, 250);
      newObject.Draw;

      BlockDiagram.canvas.Font.Height := 18;
      BlockDiagram.canvas.Font.Style := [fsBold, fsItalic];
      BlockDiagram.canvas.TextOut(28, 90, 'Block diagram samples');

      BlockDiagram.canvas.Font.Height := 14;
      BlockDiagram.canvas.Font.Style := [fsBold];
      BlockDiagram.canvas.TextOut(28, 115, 'CyberUnits');
      theWidth := BlockDiagram.canvas.TextWidth('CyberUnits ');

      BlockDiagram.canvas.Font.Height := 14;
      BlockDiagram.canvas.Font.Style := [];
      BlockDiagram.canvas.TextOut(28 + theWidth, 115, 'SystemsDiagram');

    finally
    end;
    DemoImage.Canvas.Draw(0, 0, DemoBitmap);
  finally
    DemoBitmap.Free;
    BlockDiagram.Destroy;
  end;
end;

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  DemoForm.WinAboutItem.Visible := False;
  DemoForm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  DemoForm.WinAboutItem.Visible := True;
  DemoForm.AppleMenu.Visible := False;
  {$ENDIF}
  DemoForm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  DemoForm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  DemoForm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  DemoForm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  DemoForm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  DemoForm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  DemoForm.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  DemoForm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  DemoForm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  DemoForm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TDemoForm.FormCreate(Sender: TObject);
var
  EmptyBitmap: TBitmap;
begin
  EmptyBitmap := tBitmap.Create;
  BlockDiagram := nil;
  EmptyBitmap.Height := DemoImage.Height;
  EmptyBitmap.Width := DemoImage.Width;
  EmptyBitmap.Canvas.Brush.Color := BackgroundColorBox.Selected;
  EmptyBitmap.Canvas.Pen.Color := DrawingColorBox.Selected;
  EmptyBitmap.Canvas.Rectangle(0, 0, EmptyBitmap.Width, EmptyBitmap.Height);
  AdaptMenus;
  DemoImage.Canvas.Draw(0, 0, EmptyBitmap);
  EmptyBitmap.Free;
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  //if assigned(BlockDiagram) then BlockDiagram.Destroy;
end;

procedure TDemoForm.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('Simple demo for drawing with SystemsDiagram unit');
end;

procedure TDemoForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TDemoForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TDemoForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

end.

