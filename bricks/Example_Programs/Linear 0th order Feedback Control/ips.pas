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
  Controller, LoadInjection, G1, G2, xt, zt: TIPSClass;
  cx, ce, cy, cys, cyr, cz: TConnectionClass;
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
    SetRect(xt.boundsRect, 31, 20, 51, 40);
    xt.title := 'x(t)';
    TTerminalClass(xt).TextMargin := 5;
    TTerminalClass(xt).TextPosition := rightmiddle;
    xt.Draw;

    Controller := TSigmaClass.Create;
    Controller.blockDiagram := BlockDiagram;
    SetRect(Controller.boundsRect, 20, 60, 60, 100);
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
    SetRect(G1.boundsRect, 150, 62, 210, 98);
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
    ce.Draw;
    G1.Next := ce;

    LoadInjection := TSigmaClass.Create;
    LoadInjection.blockDiagram := BlockDiagram;
    SetRect(LoadInjection.boundsRect, 290, 120, 310, 160);
    LoadInjection.Draw;
    ce.Next := LoadInjection;

    cys := TConnectionClass.Create;
    cys.blockDiagram := BlockDiagram;
    cys.sourceObject := G1;
    cys.sourceAnchor := rightmiddle;
    cys.drainObject := LoadInjection;
    cys.drainAnchor := topmiddle;
    cys.chirality := cright;
    cys.Draw;
    LoadInjection.Next := cys;

    G2 := TPClass.Create;
    G2.blockDiagram := BlockDiagram;
    SetRect(G2.boundsRect, 150, 190, 210, 226);
    G2.title := 'G2';
    G2.font.Style := [fsItalic];
    G2.Draw;
    cys.Next := G2;

    cy := TConnectionClass.Create;
    cy.blockDiagram := BlockDiagram;
    cy.sourceObject := LoadInjection;
    cy.sourceAnchor := bottommiddle;
    cy.drainObject := G2;
    cy.drainAnchor := rightmiddle;
    cy.chirality := cright;
    cy.Draw;
    G2.Next := cy;

    cyr := TConnectionClass.Create;
    cyr.blockDiagram := BlockDiagram;
    cyr.sourceObject := G2;
    cyr.sourceAnchor := leftmiddle;
    cyr.drainObject := Controller;
    cyr.drainAnchor := bottommiddle;
    cyr.chirality := cright;
    cyr.Draw;
    cy.Next := cyr;

    zt := TTerminalClass.Create;
    zt.blockDiagram := BlockDiagram;
    SetRect(zt.boundsRect, 340, 121, 380, 161);
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

