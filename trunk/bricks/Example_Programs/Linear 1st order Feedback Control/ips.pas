unit IPS;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of a simple simulator for a linear 1st order feedback system }
{ Information Processing Structure }

{ Version 1.0.9 (Corvus) }

{ (c) Johannes W. Dietrich, 1994 - 2019 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2019 }

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
  Controller, LoadInjection, G1, G2, PT1, xt, zt: TIPSClass;
  cx, ce, cy, cys, cyr, cz, cy0: TConnectionClass;
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

    Controller := TSigmaClass.Create;
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
    SetRect(G1.boundsRect, 140, 52, 200, 88);
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

    LoadInjection := TSigmaClass.Create;
    LoadInjection.blockDiagram := BlockDiagram;
    SetRect(LoadInjection.boundsRect, 280, 110, 300, 150);
    LoadInjection.Draw;
    ce.Next := LoadInjection;

    cys := TConnectionClass.Create;
    cys.blockDiagram := BlockDiagram;
    cys.sourceObject := G1;
    cys.sourceAnchor := rightmiddle;
    cys.drainObject := LoadInjection;
    cys.drainAnchor := topmiddle;
    cys.chirality := cright;
    cys.title := 'ys(t)';
    cys.font.Style := [fsItalic];
    TConnectionClass(cys).TextMargin := 7;
    TConnectionClass(cys).TextPosition := topmiddle;
    cys.Draw;
    LoadInjection.Next := cys;

    PT1 := TPT1Class.Create;
    PT1.blockDiagram := BlockDiagram;
    SetRect(PT1.boundsRect, 190, 180, 250, 216);
    PT1.title := '';
    PT1.font.Style := [fsItalic];
    PT1.Draw;
    cys.Next := PT1;

    cy0 := TConnectionClass.Create;
    cy0.blockDiagram := BlockDiagram;
    cy0.sourceObject := LoadInjection;
    cy0.sourceAnchor := bottommiddle;
    cy0.drainObject := PT1;
    cy0.drainAnchor := rightmiddle;
    cy0.chirality := cright;
    cy0.Draw;
    PT1.Next := cy0;

    G2 := TPClass.Create;
    G2.blockDiagram := BlockDiagram;
    SetRect(G2.boundsRect, 70, 180, 130, 216);
    G2.title := 'G2';
    G2.font.Style := [fsItalic];
    G2.Draw;
    cy0.Next := G2;

    cy := TConnectionClass.Create;
    cy.blockDiagram := BlockDiagram;
    cy.sourceObject := PT1;
    cy.sourceAnchor := leftmiddle;
    cy.drainObject := G2;
    cy.drainAnchor := rightmiddle;
    cy.chirality := cright;
    cy.title := 'y(t)';
    cy.font.Style := [fsItalic];
    TConnectionClass(cy).TextMargin := 7;
    TConnectionClass(cy).TextPosition := bottommiddle;
    cy.Draw;
    G2.Next := cy;

    cyr := TConnectionClass.Create;
    cyr.blockDiagram := BlockDiagram;
    cyr.sourceObject := G2;
    cyr.sourceAnchor := leftmiddle;
    cyr.drainObject := Controller;
    cyr.drainAnchor := bottommiddle;
    cyr.chirality := cright;
    cyr.title := 'yr(t)';
    cyr.font.Style := [fsItalic];
    TConnectionClass(cyr).TextMargin := 7;
    TConnectionClass(cyr).TextPosition := bottommiddle;
    cyr.Draw;
    cy.Next := cyr;

    zt := TTerminalClass.Create;
    zt.blockDiagram := BlockDiagram;
    SetRect(zt.boundsRect, 330, 111, 370, 151);
    zt.title := 'z(t)';
    TTerminalClass(zt).TextMargin := 5;
    TTerminalClass(zt).TextPosition := rightmiddle;
    zt.Draw;
    cyr.Next := zt;

    cz := TConnectionClass.Create;
    cz.blockDiagram := BlockDiagram;
    cz.sourceObject := zt;
    cz.sourceAnchor := leftmiddle;
    cz.drainObject := LoadInjection;
    cz.drainAnchor := rightmiddle;
    cz.Draw;
    zt.Next := cz;

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

