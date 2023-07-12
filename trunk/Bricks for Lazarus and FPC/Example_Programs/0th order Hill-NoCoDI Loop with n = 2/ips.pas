unit IPS;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Simulator for Hill-NoCoDI loop with n = 2 }
{ Information Processing Structure }

{ Version 2.1.0 (Foudre) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lclintf, SystemsDiagram, SimulationEngine, GUIServices;

type

  { TIPSForm }

  TIPSForm = class(TForm)
    IPSImage: TImage;
    procedure DrawIPS(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  IPSForm: TIPSForm;

implementation

{$R *.lfm}

{ TIPSForm }

procedure TIPSForm.DrawIPS(Sender: TObject);
var
  BlockDiagram: TBlockDiagram;
  IPSBitmap: TBitmap;
  Controller, Squarer, DInjection, VTypeAllostery, MMK: TIPSClass;
  G1, G2, G3, xt, D2, x1, j1, j2, j3, j4: TIPSClass;
  cx, ce, cy, cc, cu, cyr, cD, c1, cV, c2a, c2b, c2c: TConnectionClass;
  cs1, cs2, cs3, cs4: TConnectionClass;
  DrawColour: TColor;
  oldColour: TColor;
begin
  BlockDiagram := TBlockDiagram.Create;
  IPSBitmap := TBitmap.Create;
  try
    IPSBitmap.Height := IPSImage.Height;
    IPSBitmap.Width := IPSImage.Width;
    if DarkTheme then
      begin
        IPSBitmap.Canvas.Brush.Color := self.Color;
        IPSBitmap.Canvas.Pen.Color := self.Color;
        DrawColour := clSilver;
      end
    else
      begin
        IPSBitmap.Canvas.Brush.Color := clWhite;
        IPSBitmap.Canvas.Pen.Color := clWhite;
        DrawColour := clBlack;
      end;
    IPSBitmap.Canvas.Rectangle(0, 0, IPSBitmap.Width, IPSBitmap.Height);
    IPSBitmap.Canvas.Pen.Color := DrawColour;
    BlockDiagram.canvas := IPSBitmap.Canvas;

    xt := TTerminalClass.Create;
    xt.blockDiagram := BlockDiagram;
    BlockDiagram.firstIPSObject := xt;
    SetRect(xt.boundsRect, 21, 10, 41, 30);
    xt.title := 'x(t)';
    TTerminalClass(xt).TextMargin := 5;
    TTerminalClass(xt).TextPosition := rightmiddle;
    xt.Draw;

    Controller := TPiClass.Create;
    Controller.blockDiagram := BlockDiagram;
    SetRect(Controller.boundsRect, 10, 50, 50, 90);
    TInvertableClass(Controller).invertedSegments := [bottomSegment];
    Controller.Draw;
    xt.Next := Controller;

    cx := TConnectionClass.Create;
    cx.blockDiagram := BlockDiagram;
    cx.sourceObject := xt;
    cx.sourceAnchor := bottommiddle;
    cx.drainObject := Controller;
    cx.drainAnchor := topmiddle;
    cx.Draw;
    Controller.Next := cx;

    G1 := TPClass.Create;
    G1.blockDiagram := BlockDiagram;
    SetRect(G1.boundsRect, 110, 52, 170, 88);
    G1.title := 'G1';
    G1.font.Style := [fsItalic];
    G1.simulationBlock := gBlocks.G1; // Demo for linkage only, not used here
    G1.Draw;
    cx.Next := G1;

    ce := TConnectionClass.Create;
    ce.blockDiagram := BlockDiagram;
    ce.sourceObject := Controller;
    ce.sourceAnchor := rightmiddle;
    ce.drainObject := G1;
    ce.drainAnchor := leftmiddle;
    ce.title := 'e(t)';
    ce.font.Style := [fsItalic];
    TConnectionClass(ce).TextMargin := 7;
    TConnectionClass(ce).TextPosition := topmiddle;
    ce.Draw;
    G1.Next := ce;

    DInjection := TSigmaClass.Create;
    DInjection.blockDiagram := BlockDiagram;
    SetRect(DInjection.boundsRect, 440, 110, 460, 150);
    DInjection.Draw;
    ce.Next := DInjection;

    G3 := TPClass.Create;
    G3.blockDiagram := BlockDiagram;
    SetRect(G3.boundsRect, 110, 180, 170, 216);
    G3.title := 'G3';
    G3.font.Style := [fsItalic];
    G3.Draw;
    DInjection.Next := G3;

    VTypeAllostery := TSigmaClass.Create;
    VTypeAllostery.blockDiagram := BlockDiagram;
    SetRect(VTypeAllostery.boundsRect, 10, 178, 50, 218);
    VTypeAllostery.Draw;
    G3.Next := VTypeAllostery;

    cyr := TConnectionClass.Create;
    cyr.blockDiagram := BlockDiagram;
    cyr.sourceObject := G3;
    cyr.sourceAnchor := leftmiddle;
    cyr.drainObject := VTypeAllostery;
    cyr.drainAnchor := rightmiddle;
    cyr.chirality := cright;
    cyr.title := 'yR(t)';
    cyr.font.Style := [fsItalic];
    TConnectionClass(cyr).TextMargin := 7;
    TConnectionClass(cyr).TextPosition := bottommiddle;
    cyr.Draw;
    VTypeAllostery.Next := cyr;

    cV := TConnectionClass.Create;
    cV.blockDiagram := BlockDiagram;
    cV.sourceObject := VTypeAllostery;
    cV.sourceAnchor := topmiddle;
    cV.drainObject := Controller;
    cV.drainAnchor := bottommiddle;
    cV.Draw;
    cyr.Next := cV;

    j1 := TJunctionClass.Create;
    j1.blockDiagram := BlockDiagram;
    SetRect(j1.boundsRect, 199, 53, 259, 89);
    j1.Draw;
    cV.Next := j1;

    cc := TConnectionClass.Create;
    cc.blockDiagram := BlockDiagram;
    cc.sourceObject := G1;
    cc.sourceAnchor := rightmiddle;
    cc.drainObject := j1;
    cc.drainAnchor := leftmiddle;
    cc.title := '      c(t)';
    cc.font.Style := [fsItalic];
    TConnectionClass(cc).TextMargin := 7;
    TConnectionClass(cc).TextPosition := topmiddle;
    cc.Draw;
    j1.Next := cc;

    squarer := TPiClass.Create;
    squarer.blockDiagram := BlockDiagram;
    SetRect(squarer.boundsRect, 270, 50, 310, 90);
    squarer.Draw;
    cc.Next := squarer;

    j3 := TJunctionClass.Create;
    j3.blockDiagram := BlockDiagram;
    SetRect(j3.boundsRect, 255, 20, 265, 30);
    oldColour := IPSBitmap.Canvas.Pen.Color;
    IPSBitmap.Canvas.Pen.Color := clWhite; // make j3 invisible
    j3.Draw;
    IPSBitmap.Canvas.Pen.Color := oldColour;
    squarer.Next := j3;

    j4 := TJunctionClass.Create;
    j4.blockDiagram := BlockDiagram;
    SetRect(j4.boundsRect, 255, 110, 265, 120);
    oldColour := IPSBitmap.Canvas.Pen.Color;
    IPSBitmap.Canvas.Pen.Color := clWhite; // make j4 invisible
    j4.Draw;
    IPSBitmap.Canvas.Pen.Color := oldColour;
    j3.Next := j4;

    cs1 := TConnectionClass.Create;
    cs1.blockDiagram := BlockDiagram;
    cs1.sourceObject := j1;
    cs1.sourceAnchor := topmiddle;
    cs1.drainObject := j3;
    cs1.drainAnchor := rightmiddle;
    cs1.chirality := cright;
    cs1.Draw;
    j4.Next := cs1;

    cs2 := TConnectionClass.Create;
    cs2.blockDiagram := BlockDiagram;
    cs2.sourceObject := j3;
    cs2.sourceAnchor := rightmiddle;
    cs2.drainObject := squarer;
    cs2.drainAnchor := topmiddle;
    cs2.chirality := cright;
    cs2.Draw;
    cs1.Next := cs2;

    cs3 := TConnectionClass.Create;
    cs3.blockDiagram := BlockDiagram;
    cs3.sourceObject := j1;
    cs3.sourceAnchor := topmiddle;
    cs3.drainObject := j4;
    cs3.drainAnchor := rightmiddle;
    cs3.chirality := cleft;
    cs3.Draw;
    cs2.Next := cs3;

    cs4 := TConnectionClass.Create;
    cs4.blockDiagram := BlockDiagram;
    cs4.sourceObject := j4;
    cs4.sourceAnchor := rightmiddle;
    cs4.drainObject := squarer;
    cs4.drainAnchor := bottommiddle;
    cs4.chirality := cleft;
    cs4.Draw;
    cs3.Next := cs4;

    cu := TConnectionClass.Create;
    cu.blockDiagram := BlockDiagram;
    cu.sourceObject := squarer;
    cu.sourceAnchor := rightmiddle;
    cu.drainObject := DInjection;
    cu.drainAnchor := topmiddle;
    cu.chirality := cright;
    cu.title := '      u(t)';
    cu.font.Style := [fsItalic];
    TConnectionClass(cu).TextMargin := 7;
    TConnectionClass(cu).TextPosition := topmiddle;
    cu.Draw;
    cs4.Next := cu;

    G2 := TPClass.Create;
    G2.blockDiagram := BlockDiagram;
    SetRect(G2.boundsRect, 358, 112, 418, 148);
    G2.title := 'G2';
    G2.font.Style := [fsItalic];
    G2.Draw;
    cu.Next := G2;

    j2 := TJunctionClass.Create;
    j2.blockDiagram := BlockDiagram;
    SetRect(j2.boundsRect, 359, 53, 419, 89);
    j2.Draw;
    G2.Next := j2;

    D2 := TTerminalClass.Create;
    D2.blockDiagram := BlockDiagram;
    SetRect(D2.boundsRect, 480, 111, 520, 151);
    D2.title := 'D2';
    TTerminalClass(D2).TextMargin := 5;
    TTerminalClass(D2).TextPosition := rightmiddle;
    D2.Draw;
    j2.Next := D2;

    c2a := TConnectionClass.Create;
    c2a.blockDiagram := BlockDiagram;
    c2a.sourceObject := j2;
    c2a.sourceAnchor := bottommiddle;
    c2a.drainObject := G2;
    c2a.drainAnchor := topmiddle;
    c2a.Draw;
    D2.Next := c2a;

    cD := TConnectionClass.Create;
    cD.blockDiagram := BlockDiagram;
    cD.sourceObject := D2;
    cD.sourceAnchor := leftmiddle;
    cD.drainObject := DInjection;
    cD.drainAnchor := rightmiddle;
    cD.Draw;
    c2a.Next := cD;

    MMK := TPiClass.Create;
    MMK.blockDiagram := BlockDiagram;
    SetRect(MMK.boundsRect, 363, 178, 413, 218);
    TInvertableClass(MMK).invertedSegments := [rightSegment];
    MMK.Draw;
    cD.Next := MMK;

    cy := TConnectionClass.Create;
    cy.blockDiagram := BlockDiagram;
    cy.sourceObject := MMK;
    cy.sourceAnchor := leftmiddle;
    cy.drainObject := G3;
    cy.drainAnchor := rightmiddle;
    cy.chirality := cright;
    cy.title := 'y(t)';
    cy.font.Style := [fsItalic];
    TConnectionClass(cy).TextMargin := 7;
    TConnectionClass(cy).TextPosition := bottommiddle;
    cy.Draw;
    MMK.Next := cy;

    c2b := TConnectionClass.Create;
    c2b.blockDiagram := BlockDiagram;
    c2b.sourceObject := G2;
    c2b.sourceAnchor := bottommiddle;
    c2b.drainObject := MMK;
    c2b.drainAnchor := topmiddle;
    c2b.Draw;
    cy.Next := c2b;

    c2c := TConnectionClass.Create;
    c2c.blockDiagram := BlockDiagram;
    c2c.sourceObject := DInjection;
    c2c.sourceAnchor := bottommiddle;
    c2c.drainObject := MMK;
    c2c.drainAnchor := rightmiddle;
    c2c.chirality := cright;
    c2c.Draw;
    c2b.Next := c2c;

    x1 := TTerminalClass.Create;
    x1.blockDiagram := BlockDiagram;
    SetRect(x1.boundsRect, 21, 240, 41, 250);
    x1.title := '1';
    TTerminalClass(x1).TextMargin := 5;
    TTerminalClass(x1).TextPosition := rightmiddle;
    x1.Draw;
    c2c.Next := x1;

    c1 := TConnectionClass.Create;
    c1.blockDiagram := BlockDiagram;
    c1.sourceObject := x1;
    c1.sourceAnchor := topmiddle;
    c1.drainObject := VTypeAllostery;
    c1.drainAnchor := bottommiddle;
    c1.Draw;
    x1.Next := c1;

    IPSImage.Canvas.Draw(0, 0, IPSBitmap);
  finally
    IPSBitmap.Free;
    BlockDiagram.Destroy;
  end;
end;

procedure TIPSForm.FormCreate(Sender: TObject);
begin
  DrawIPS(Sender);
end;

end.

