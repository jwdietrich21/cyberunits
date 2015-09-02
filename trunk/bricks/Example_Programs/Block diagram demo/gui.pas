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
  StdCtrls, lclintf, ColorBox, SystemsDiagram;

type

  { TDemoForm }

  TDemoForm = class(TForm)
    BackgroundColorBox: TColorBox;
    DrawingColorBox: TColorBox;
    DemoButton: TButton;
    DemoImage: TImage;
    BackgroundColorLabel: TLabel;
    DrawingColorLabel: TLabel;
    procedure DemoButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
      SetRect(newObject.boundsRect, 420, 220, 470, 250);
      TInvertableClass(newObject).invertedSegments := [leftSegment, rightSegment];
      newObject.Draw;
      lastObject := newObject;

      newObject := TPiClass.Create;
      lastObject.Next := newObject;
      newObject.blockDiagram := BlockDiagram;
      TInvertableClass(newObject).invertedSegments := [topSegment, bottomSegment];
      SetRect(newObject.boundsRect, 480, 220, 530, 250);
      newObject.Draw;

      BlockDiagram.canvas.Font.Height := 18;
      BlockDiagram.canvas.Font.Style := [fsBold, fsItalic];
      BlockDiagram.canvas.TextOut(28, 100, 'Block diagram samples');

    finally
    end;
    DemoImage.Canvas.Draw(0, 0, DemoBitmap);
  finally
    DemoBitmap.Free;
    BlockDiagram.Destroy;
  end;
end;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  BlockDiagram := nil;
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  //if assigned(BlockDiagram) then BlockDiagram.Destroy;
end;

end.

