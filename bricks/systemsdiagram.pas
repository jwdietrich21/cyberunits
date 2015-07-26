unit SystemsDiagram;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Demo of canvas using SystemsDiagram }
{ SystemsDiagram }

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
  Classes, SysUtils, Graphics, lclintf, LCLProc, FPCanvas, Math, Bricks,

  Dialogs;

type

  tAnchorposition = (leftmiddle, topmiddle, rightmiddle, bottommiddle);
  tSegmentPosition = (topSegment, rightSegment, bottomSegment, leftSegment);

  tAnchorPoint = record
    position: TPoint;
    attached: boolean;
  end;

  tChirality = (cleft, cright);

  TBlockDiagram = class;
  TIPSClass = class;

  { TIPSClass }
  { Generic class for IPS Object }

  TIPSClass = class
  public
    simulationBlock: TBlock; // Representation in Bricks
    blockDiagram: TBlockDiagram;
    Next: TIPSClass;
    boundsRect, objectRect: TRect;
    anchorPoint: array[leftmiddle..bottommiddle] of tAnchorPoint;
    title: string;
    font: TFont;
    procedure Draw; virtual; abstract;
    destructor Destroy; override;
  end;

  { TPClass }
  { Proportional block }

  TPClass = class(TIPSClass)
  public
    constructor Create;
    procedure Draw; override;
  end;

  { TTerminalClass }
  { Terminal connector-like element for constants }

  TTerminalClass = class(TIPSClass)
  public
    TextMargin: integer;
    TextPosition: tAnchorposition;
    constructor Create;
    procedure Draw; override;
  end;

  { TInvertableClass }

  TInvertableClass = class(TIPSClass)
  public
    invertedSegments:  set of tSegmentPosition;
    constructor Create;
    procedure Draw; override;
  end;

  { TSigmaClass }

  TSigmaClass = class(TInvertableClass)
  public
    constructor Create;
    procedure Draw; override;
  end;

  { TPiClass }

  TPiClass = class(TInvertableClass)
  public
    constructor Create;
    procedure Draw; override;
  end;

  { TConnectionClass }

  TConnectionClass = class(TIPSClass)
  public
    sourceObject, drainObject: TIPSClass;
    sourceAnchor, drainAnchor: tAnchorposition;
    chirality: tChirality;
    constructor Create;
    procedure Draw; override;
  end;

  { TBlockDiagram }

  TBlockDiagram = class
  public
    firstIPSObject: TIPSClass;
    canvas: TCanvas;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure CenterRect(outerRect: TRect; var innerRect: TRect);
var
  innerWidth, outerWidth, innerHeight, motherHeight: integer;
begin
  innerWidth := innerRect.right - innerRect.left;
  outerWidth := outerRect.right - outerRect.left;
  innerHeight := innerRect.bottom - innerRect.top;
  motherHeight := outerRect.bottom - outerRect.top;
  innerRect.top := (motherHeight - innerHeight) div 2 + outerRect.top;
  innerRect.bottom := innerRect.top + innerHeight;
  innerRect.left := (outerWidth - innerWidth) div 2 + outerRect.left;
  innerRect.right := innerRect.left + innerWidth;
end;

procedure CenterString(const theCanvas: TCanvas; const theString: string;
  myRect: TRect; var Start: TPoint);
begin
  Start.x := myRect.left + (myRect.right - myRect.left) div 2 -
    theCanvas.TextWidth(theString) div 2;
  Start.y := myRect.top + (myRect.bottom - myRect.top) div 2 -
    (theCanvas.TextHeight(theString)) div 2;
end;

function InsetRect(var ARect: TRect; dx, dy: integer): boolean;
begin
  with ARect do
  begin
    Left := Left + dx;
    Right := Right - dx;
    Top := Top + dy;
    Bottom := Bottom - dy;
  end;
  Result := (ARect.Left >= 0) and (ARect.Top >= 0);
end;

procedure DrawArrowHead(var theCanvas: TCanvas; const x, y: integer; const theLength, alpha, beta: real);
var
  A, B, C, D, r: real;
  gamma, epsilon, zeta, eta: real;
  thePoints: array of TPoint;
  oldStyle: TFPBrushStyle;
  oldColor: TColor;
begin
  oldColor := theCanvas.Brush.Color;
  oldStyle := theCanvas.Brush.Style;
  theCanvas.Brush.Color := clBlack;
  theCanvas.Brush.Style := bsSolid;
  SetLength(thePoints, 3);         { array for corners of arrow head }
  thePoints[0] := Point(x, y);     { position of arrow head }
  gamma := alpha + beta / 2;       { angle of right edge }
  zeta := alpha - beta / 2;        { angle of left edge }
  epsilon := pi / 2 - gamma;       { angle between vertical and right edge }
  eta := pi / 2 - zeta;            { angle between vertical and left edge }
  r := theLength * cos(beta / 2);  { length of both edges }
  A := r * cos(epsilon);           { distance to vertical position of right basal corner }
  B := r * sin(epsilon);           { distance to horizontal position of right basal corner }
  thePoints[1].x := round(x - B);  { horizontal position of right basal corner }
  thePoints[1].y := round(y + A);  { vertical position of right basal corner }
  C := r * cos(eta);               { distance to vertical position of left basal corner }
  D := r * sin(eta);               { distance to horizontal position of left basal corner }
  thePoints[2].x := round(x - D);  { horizontal position of left basal corner }
  thePoints[2].y := round(y + C);  { vertical position of left basal corner }

  theCanvas.Polygon(thePoints);    { dras arrow head with these points }
  theCanvas.Brush.Style := oldStyle;
  theCanvas.Brush.Color := oldColor;
end;

procedure pathTo(theCanvas: TCanvas; x, y: integer; chirality: tChirality);
{draws rounded connection lines}
const
  STDDiam = 20;
  BETA = 26 * pi / 180;
  ARROW_LENGTH = 13;
var
  SourcePoint, overlap, arcDiam, delta: TPoint;
  arcRect: TRect;
  oldStyle: TFPBrushStyle;
  oldColor: TColor;
  alpha: real;
begin
  oldColor := theCanvas.Brush.Color;
  oldStyle := theCanvas.Brush.Style;
  SourcePoint := TheCanvas.PenPos;
  delta.x := x - SourcePoint.x;
  delta.y := y - SourcePoint.y;
  arcDiam.x := STDDiam;
  arcDiam.y := STDDiam;
  if (delta.x <> 0) and (delta.y <> 0) then
  begin { draw rounded path, if difference in x or y }
    if abs(delta.x) < STDDiam then
      arcDiam.x := abs(delta.x);
    if abs(delta.y) < STDDiam then
      arcDiam.y := abs(delta.y);
    overlap.x := arcDiam.x div 2 + 1;
    overlap.y := arcDiam.y div 2 + 1;
    if (overlap.x <> 0) and (overlap.y <> 0) then
    begin
      if delta.y > 0 then
      begin
        if delta.x > 0 then
        begin
          if chirality = cright then
          begin
            SetRect(arcRect, x - arcDiam.x, SourcePoint.y, x, SourcePoint.y + arcDiam.y);
            TheCanvas.LineTo(arcRect.left + overlap.x, arcRect.Top);
            TheCanvas.MoveTo(arcRect.Right, arcRect.bottom - overlap.y);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 0, 90 * 16);
          end
          else
          begin
            SetRect(arcRect, SourcePoint.x, y - arcDiam.y, SourcePoint.x + arcDiam.x, y);
            TheCanvas.LineTo(arcRect.left, arcRect.Top + overlap.y);
            TheCanvas.MoveTo(arcRect.Right - overlap.x, arcRect.bottom);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 180 * 16, 90 * 16);
          end;
        end
        else
        begin
          if chirality = cright then
          begin
            SetRect(arcRect, SourcePoint.x - arcDiam.x, y - arcDiam.y, SourcePoint.x, y);
            TheCanvas.LineTo(arcRect.right, arcRect.Top + overlap.y);
            TheCanvas.MoveTo(arcRect.Left + overlap.x, arcRect.bottom);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 270 * 16, 90 * 16);
          end
          else
          begin
            SetRect(arcRect, x, SourcePoint.y, x + arcDiam.x, SourcePoint.y + arcDiam.y);
            TheCanvas.LineTo(arcRect.right - overlap.x, arcRect.Top);
            TheCanvas.MoveTo(arcRect.Left, arcRect.bottom - overlap.y);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 90 * 16, 90 * 16);
          end;
        end;
      end
      else
      begin
        if delta.x > 0 then
        begin
          if chirality = cright then
          begin
            SetRect(arcRect, SourcePoint.x, y, SourcePoint.x + arcDiam.x, y + arcDiam.y);
            TheCanvas.LineTo(arcRect.left, arcRect.bottom - overlap.y);
            TheCanvas.MoveTo(arcRect.Right - overlap.x, arcRect.top);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 90 * 16, 90 * 16);
          end
          else
          begin
            SetRect(arcRect, x - arcDiam.x, SourcePoint.y - arcDiam.y, x, SourcePoint.y);
            TheCanvas.LineTo(arcRect.left + overlap.x, arcRect.bottom);
            TheCanvas.MoveTo(arcRect.Right, arcRect.top + overlap.y);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 270 * 16, 90 * 16);
          end;
        end
        else
        begin
          if chirality = cright then
          begin
            SetRect(arcRect, x, SourcePoint.y - arcDiam.y, x + arcDiam.x, SourcePoint.y);
            TheCanvas.LineTo(arcRect.right - overlap.x, arcRect.Bottom);
            TheCanvas.MoveTo(arcRect.Left, arcRect.top + overlap.y);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 180 * 16, 90 * 16);
          end
          else
          begin
            SetRect(arcRect, SourcePoint.x - arcDiam.x, y, SourcePoint.x, y + arcDiam.y);
            TheCanvas.LineTo(arcRect.right, arcRect.Bottom - overlap.y);
            TheCanvas.MoveTo(arcRect.Left + overlap.x, arcRect.top);
            TheCanvas.Arc(arcRect.Left, arcRect.Top, arcRect.Right,
              arcRect.Bottom, 0, 90 * 16);
          end;
        end;
      end;
    end;
  end;
    SourcePoint := TheCanvas.PenPos;
    TheCanvas.LineTo(x, y);  { finishing strait }
    alpha := arctan((SourcePoint.y - y) / (x - SourcePoint.x)); { angle of finishing strait }
    if (alpha = 0) and (SourcePoint.x > x) then
      alpha := pi;           { arctan is ambiguous }
    DrawArrowHead(theCanvas, x, y, ARROW_LENGTH, alpha, BETA);
    theCanvas.Brush.Style := oldStyle;
    theCanvas.Brush.Color := oldColor;
end;

procedure pathTo(theCanvas: TCanvas; thePoint: TPoint; chirality: tChirality);
begin
  pathTo(theCanvas, thePoint.x, thePoint.y, chirality);
end;

procedure GetAnchorPoints(theIPSObject: TIPSClass; const theRect: TRect);
begin
  with theIPSObject.anchorPoint[leftmiddle] do
  begin
    position.x := theRect.left;
    position.y := theRect.top + (theRect.bottom - theRect.top) div 2;
  end;
  with theIPSObject.anchorPoint[topmiddle] do
  begin
    position.x := theRect.left + (theRect.right - theRect.left) div 2;
    position.y := theRect.top;
  end;
  with theIPSObject.anchorPoint[rightmiddle] do
  begin
    position.x := theRect.right;
    position.y := theRect.top + (theRect.bottom - theRect.top) div 2;
  end;
  with theIPSObject.anchorPoint[bottommiddle] do
  begin
    position.x := theRect.left + (theRect.right - theRect.left) div 2;
    position.y := theRect.bottom;
  end;
end;

{ TConnectionClass }

constructor TConnectionClass.Create;
begin
  inherited Create;
end;

procedure TConnectionClass.Draw;
var
  StartingPoint, GoalPoint: TPoint;
begin
  StartingPoint := sourceObject.anchorPoint[sourceAnchor].position;
  GoalPoint := drainObject.anchorPoint[drainAnchor].position;
  BlockDiagram.canvas.MoveTo(StartingPoint);
  PathTo(BlockDiagram.canvas, GoalPoint.X, GoalPoint.y, chirality);
end;

{ TPiClass }

constructor TPiClass.Create;
begin
  inherited Create;
end;

procedure TPiClass.Draw;
var
  theRect: TRect;
  theString: char;
  theStart: TPoint;
  rw, rh, tw, th: integer;
  oldStyle: TFPBrushStyle;
  oldColor: TColor;
begin
  inherited Draw;
  oldColor := blockDiagram.canvas.Brush.Color;
  oldStyle := blockDiagram.canvas.Brush.Style;
  theRect := objectRect;
  theString := '*';
  rw := theRect.Right - theRect.Left;
  rh := theRect.Bottom - theRect.Top;
  InsetRect(theRect, rw div 2 - 2, rh div 2 - 2);
  blockDiagram.canvas.Brush.Color := clBlack;
  blockDiagram.canvas.Brush.Style := bsSolid;
  blockDiagram.canvas.Ellipse(theRect);
  blockDiagram.canvas.Brush.Style := oldStyle;
  blockDiagram.canvas.Brush.Color := oldColor;
end;

{ TSigmaClass }

constructor TSigmaClass.Create;
begin
  inherited Create;
end;

procedure TSigmaClass.Draw;
var
  theRect: TRect;
  theString: char;
  theStart: TPoint;
  rw, rh, tw, th: integer;
  oldColor: TColor;
  oldStyle: TFPBrushStyle;
begin
  inherited Draw;
  oldColor := blockDiagram.canvas.Brush.Color;
  oldStyle := blockDiagram.canvas.Brush.Style;
  theRect := objectRect;
  theString := '+';
  rw := theRect.Right - theRect.Left;
  rh := theRect.Bottom - theRect.Top;
  blockDiagram.canvas.GetTextSize(theString, tw, th);
  blockDiagram.canvas.Brush.Style := bsClear;
  blockDiagram.canvas.TextRect(theRect, theRect.Left + (rw - tw) div
    2, theRect.Top + (rh - th) div 2 - 1, theString);
  blockDiagram.canvas.Brush.Style := oldStyle;
  blockDiagram.canvas.Brush.Color := oldColor;
end;

{ TInvertableClass }

constructor TInvertableClass.Create;
begin
  inherited Create;
  invertedSegments := [];
  Font := TFont.Create;
end;

procedure TInvertableClass.Draw;
var
  theRect: TRect;
  theWidth, theHeight: integer;
  theCenter, intersection1, intersection2, intersection3, intersection4: TPoint;
  connectionRingWidth: integer;
  oldColor: TColor;
  oldStyle: TFPBrushStyle;
  oldFont: TFont;
begin
  theRect := boundsRect;
  theHeight := theRect.bottom - theRect.top;
  theWidth := theRect.right - theRect.left;
  if theHeight <> theWidth then
  begin
    { compensates for non-quadratic boundsRect }
    theRect.left := theRect.left + (theWidth - theHeight) div 2;
    theRect.right := theRect.left + theHeight;
    theWidth := theRect.right - theRect.left;
  end;
  oldColor := blockDiagram.canvas.Brush.Color;
  oldStyle := blockDiagram.canvas.Brush.Style;
  oldFont := blockDiagram.canvas.Font;
  blockDiagram.canvas.Font := Font;
  objectRect := theRect;
  theCenter.x := theRect.left + theWidth div 2;
  theCenter.y := theRect.top + theHeight div 2;
  intersection1 := point(theCenter.x + trunc(0.71 * theHeight / 2),
    theCenter.y - trunc(0.71 * theHeight / 2));
  intersection2 := point(theCenter.x + trunc(0.71 * theHeight / 2),
    theCenter.y + trunc(0.71 * theHeight / 2));
  intersection3 := point(theCenter.x - trunc(0.71 * theHeight / 2),
    theCenter.y + trunc(0.71 * theHeight / 2));
  intersection4 := point(theCenter.x - trunc(0.71 * theHeight / 2),
    theCenter.y - trunc(0.71 * theHeight / 2));
  blockDiagram.canvas.Ellipse(theRect);
  blockDiagram.canvas.MoveTo(intersection4);
  blockDiagram.canvas.LineTo(intersection2);
  blockDiagram.canvas.MoveTo(intersection1);
  blockDiagram.canvas.LineTo(intersection3);
  GetAnchorPoints(self, theRect);
  blockDiagram.canvas.Brush.Style := bsSolid;
  blockDiagram.canvas.Brush.Color := clBlack;
  if topSegment in invertedSegments then
    blockDiagram.canvas.Pie(theRect.Left, theRect.Top, theRect.Right, theRect.Bottom,
    intersection1.x, intersection1.y, intersection4.x, intersection4.y);
  if rightSegment in invertedSegments then
    blockDiagram.canvas.Pie(theRect.Left, theRect.Top, theRect.Right, theRect.Bottom,
    intersection2.x, intersection2.y, intersection1.x, intersection1.y);
  if bottomSegment in invertedSegments then
    blockDiagram.canvas.Pie(theRect.Left, theRect.Top, theRect.Right, theRect.Bottom,
    intersection3.x, intersection3.y, intersection2.x, intersection2.y);
  if leftSegment in invertedSegments then
    blockDiagram.canvas.Pie(theRect.Left, theRect.Top, theRect.Right, theRect.Bottom,
    intersection4.x, intersection4.y, intersection3.x, intersection3.y);
  blockDiagram.canvas.Brush.Color := oldColor;
  connectionRingWidth := theHeight div 5;
  InsetRect(theRect, connectionRingWidth, connectionRingWidth);
  blockDiagram.canvas.Ellipse(theRect);
  objectRect := theRect;
  blockDiagram.canvas.Brush.Style := oldStyle;
  blockDiagram.canvas.Brush.Color := oldColor;
  blockDiagram.canvas.Font := oldFont;
end;

{ TTerminalClass }

constructor TTerminalClass.Create;
const
  DEFAULT_MARGIN = 3;
begin
  inherited Create;
  TextMargin := DEFAULT_MARGIN;
  TextPosition := leftmiddle;
  Font := TFont.Create;
end;

procedure TTerminalClass.Draw;
var
  outerRect, innerRect, stringRect: TRect;
  theString: string;
  theStart: TPoint;
  oldFont: TFont;
begin
  oldFont := blockDiagram.canvas.Font;
  blockDiagram.canvas.Font := Font;
  outerRect := boundsRect;
  theString := title;
  SetRect(innerRect, 0, 0, 5, 5);
  CenterRect(outerRect, innerRect);
  blockDiagram.canvas.Ellipse(innerRect);
  objectRect := innerRect;
  StringRect := innerRect;
  case TextPosition of
  leftmiddle:
    MoveRect(StringRect, objectRect.Left - blockDiagram.canvas.TextWidth(theString) - TextMargin, objectRect.Top - blockDiagram.canvas.TextHeight(theString) div 2 + 1);
  rightmiddle:
    MoveRect(StringRect, objectRect.right + TextMargin, objectRect.Top - blockDiagram.canvas.TextHeight(theString) div 2 + 1);
  topmiddle:
    MoveRect(StringRect, objectRect.Left - blockDiagram.canvas.TextWidth(theString) div 2, objectRect.Top - blockDiagram.canvas.TextHeight(theString) - TextMargin);
  bottommiddle:
    MoveRect(StringRect, objectRect.Left - blockDiagram.canvas.TextWidth(theString) div 2, objectRect.Bottom + TextMargin);
  end;
  blockDiagram.canvas.TextOut(StringRect.Left, StringRect.top, theString);
  GetAnchorPoints(self, innerRect);
  blockDiagram.canvas.Font := oldFont;
end;

{ TPClass }

constructor TPClass.Create;
begin
  inherited Create;
  Font := TFont.Create;
end;

procedure TPClass.Draw;
var
  theRect: TRect;
  theString: string;
  rw, rh, tw, th: integer;
  oldFont: TFont;
begin
  if (assigned(blockDiagram) and assigned(blockDiagram.canvas)) then
  begin
    theRect := boundsRect;
    theString := title;
    oldFont := blockDiagram.canvas.Font;
    blockDiagram.canvas.Font := Font;
    blockDiagram.canvas.Rectangle(theRect);
    rw := theRect.Right - theRect.Left;
    rh := theRect.Bottom - theRect.Top;
    blockDiagram.canvas.GetTextSize(theString, tw, th);
    blockDiagram.canvas.Brush.Style := bsClear;
    blockDiagram.canvas.TextRect(theRect, theRect.Left + (rw - tw) div
      2, theRect.Top + (rh - th) div 2, theString);
    GetAnchorPoints(self, theRect);
    blockDiagram.canvas.Font := oldFont;
  end;
end;

{ TIPSClass }

destructor TIPSClass.Destroy;
begin
  if assigned(Font) then
    font.Destroy;
  inherited Destroy;
end;

{ TBlockDiagram }

constructor TBlockDiagram.Create;
begin
  inherited Create;
end;

destructor TBlockDiagram.Destroy;
var
  curIPSObject, nextIPSObject: TIPSClass;
begin
  curIPSObject := firstIPSObject;
  while assigned(curIPSObject) do
  begin
    nextIPSObject := curIPSObject.Next;
    curIPSObject.Destroy;
    curIPSObject := nextIPSObject;
  end;
  inherited Destroy;
end;

end.
