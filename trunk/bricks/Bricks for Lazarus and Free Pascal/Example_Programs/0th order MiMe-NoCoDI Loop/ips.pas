unit IPS;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Simulator for MiMe-NoCoDI loop }
{ Information Processing Structure }

{ Version 1.1.0 (Director) }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lclintf, SystemsDiagram;

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
  Controller, DInjection, VTypeAllostery, MMK: TIPSClass;
  G1, G2, G3, xt, D2, x1, j1: TIPSClass;
  cx, ce, cy, cc, cyr, cD, c1, cV, c2a, c2b, c2c: TConnectionClass;
begin
  BlockDiagram := TBlockDiagram.Create;
  IPSBitmap := TBitmap.Create;
  try
    IPSBitmap.Height := IPSImage.Height;
    IPSBitmap.Width := IPSImage.Width;
    IPSBitmap.Canvas.Brush.Color := clWhite;
    IPSBitmap.Canvas.Pen.Color := clWhite;
    IPSBitmap.Canvas.Rectangle(0, 0, IPSBitmap.Width, IPSBitmap.Height);
    IPSBitmap.Canvas.Pen.Color := clBlack;
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
    SetRect(DInjection.boundsRect, 330, 110, 350, 150);
    DInjection.Draw;
    ce.Next := DInjection;

    cc := TConnectionClass.Create;
    cc.blockDiagram := BlockDiagram;
    cc.sourceObject := G1;
    cc.sourceAnchor := rightmiddle;
    cc.drainObject := DInjection;
    cc.drainAnchor := topmiddle;
    cc.chirality := cright;
    cc.title := '      c(t)';
    cc.font.Style := [fsItalic];
    TConnectionClass(cc).TextMargin := 7;
    TConnectionClass(cc).TextPosition := topmiddle;
    cc.Draw;
    DInjection.Next := cc;

    G3 := TPClass.Create;
    G3.blockDiagram := BlockDiagram;
    SetRect(G3.boundsRect, 110, 180, 170, 216);
    G3.title := 'G3';
    G3.font.Style := [fsItalic];
    G3.Draw;
    cc.Next := G3;

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

    G2 := TPClass.Create;
    G2.blockDiagram := BlockDiagram;
    SetRect(G2.boundsRect, 238, 112, 298, 148);
    G2.title := 'G2';
    G2.font.Style := [fsItalic];
    G2.Draw;
    cV.Next := G2;

    j1 := TJunctionClass.Create;
    j1.blockDiagram := BlockDiagram;
    SetRect(j1.boundsRect, 239, 52, 299, 88);
    j1.Draw;
    G2.Next := j1;

    D2 := TTerminalClass.Create;
    D2.blockDiagram := BlockDiagram;
    SetRect(D2.boundsRect, 370, 111, 410, 151);
    D2.title := 'D2';
    TTerminalClass(D2).TextMargin := 5;
    TTerminalClass(D2).TextPosition := rightmiddle;
    D2.Draw;
    j1.Next := D2;

    c2a := TConnectionClass.Create;
    c2a.blockDiagram := BlockDiagram;
    c2a.sourceObject := j1;
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
    SetRect(MMK.boundsRect, 243, 178, 293, 218);
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

