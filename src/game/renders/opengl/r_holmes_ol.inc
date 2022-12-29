(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{.$INCLUDE ../shared/a_modes.inc}
type
  TOutliner = class
  private
    type
      TSpan = record
        x0, x1: Integer;
        next: Integer; // index
      end;
      PSpan = ^TSpan;

  private
    mWidth, mHeight: Integer;
    spans: array of TSpan;
    firstFreeSpan: Integer; // span index or -1
    usedSpans: Integer;
    lines: array of Integer; // span indicies

  private
    function allocSpan (ax0, ax1: Integer): Integer; // returns span index
    procedure freeSpan (idx: Integer);

  public
    type
      TSpanX = record
        x0, x1: Integer;
      end;

      TSpanEnumerator = record
      private
        spans: array of TSpan;
        cur: Integer;
        first: Boolean;
      public
        constructor Create (master: TOutliner; y: Integer);
        function MoveNext (): Boolean; inline;
        function getCurrent (): TSpanX; inline;
        function GetEnumerator (): TSpanEnumerator; inline;
        property Current: TSpanX read getCurrent;
      end;

      TSpanEdgeEnumerator = record
      private
        spans: array of TSpan;
        spi, usp: Integer;
        sx, ex: Integer;
        cur: TSpanX;
        doSkipUSP: Boolean;
      private
        procedure nextSPI (); inline;
      public
        constructor Create (master: TOutliner; y, dy: Integer);
        function MoveNext (): Boolean; inline;
        function GetEnumerator (): TSpanEdgeEnumerator; inline;
        property Current: TSpanX read cur;
      end;

  public
    constructor Create (aw, ah: Integer);
    destructor Destroy (); override;

    procedure clear ();
    procedure setup (aw, ah: Integer);

    procedure addSpan (ax0, ax1, y: Integer);
    procedure addRect (x, y, w, h: Integer);

    function eachSpanAtY (y: Integer): TSpanEnumerator;
    function eachSpanEdgeAtY (y, dy: Integer): TSpanEdgeEnumerator;
    //function GetEnumerator (): TValEnumerator;

  public
    property width: Integer read mWidth;
    property height: Integer read mHeight;
  end;


// ////////////////////////////////////////////////////////////////////////// //
function TOutliner.allocSpan (ax0, ax1: Integer): Integer;
begin
  result := firstFreeSpan;
  if (result = -1) then
  begin
    result := usedSpans;
    if (usedSpans = Length(spans)) then SetLength(spans, usedSpans+512);
    Inc(usedSpans);
  end
  else
  begin
    firstFreeSpan := spans[result].next;
  end;
  with (spans[result]) do
  begin
    x0 := ax0;
    x1 := ax1;
    next := -1;
  end;
end;


procedure TOutliner.freeSpan (idx: Integer);
begin
  if (idx >= 0) and (idx < usedSpans) then
  begin
    spans[idx].next := firstFreeSpan;
    firstFreeSpan := idx;
  end;
end;


constructor TOutliner.Create (aw, ah: Integer);
var
  f: Integer;
begin
  assert(aw > 0);
  assert(ah > 0);
  mWidth := aw;
  mHeight := ah;
  SetLength(lines, mHeight);
  for f := 0 to High(lines) do lines[f] := -1;
  usedSpans := 0;
  firstFreeSpan := -1;
end;


destructor TOutliner.Destroy ();
begin
  spans := nil;
  lines := nil;
  inherited;
end;


procedure TOutliner.setup (aw, ah: Integer);
var
  f: Integer;
begin
  assert(aw > 0);
  assert(ah > 0);
  if (mWidth <> aw) or (mHeight <> ah) then
  begin
    mWidth := aw;
    mHeight := ah;
    SetLength(lines, mHeight);
  end;
  for f := 0 to High(lines) do lines[f] := -1;
  usedSpans := 0;
  firstFreeSpan := -1;
end;


procedure TOutliner.clear ();
var
  f: Integer;
begin
  for f := 0 to High(lines) do lines[f] := -1;
  usedSpans := 0;
  firstFreeSpan := -1;
end;


procedure TOutliner.addSpan (ax0, ax1, y: Integer);
  procedure fixFrom (spi: Integer);
  var
    sp, sn: PSpan;
    spf: Integer;
  begin
    assert(spi <> -1);
    sp := @spans[spi];
    while true do
    begin
      spf := sp.next;
      if (spf = -1) then break;
      sn := @spans[spf];
      // join?
      if (sp.x1+1 = sn.x0) then
      begin
        //conprintfln("JOIN: sp=(%s,%s); sn=(%s,%s)", sp.x0, sp.x1, sn.x0, sn.x1);
        sp.x1 := sn.x1;
      end
      else if (sn.x0 <= sp.x1) then
      begin
        // overlaps
        //conprintfln("OVER: sp=(%s,%s); sn=(%s,%s)", sp.x0, sp.x1, sn.x0, sn.x1);
        if (sp.x1 < sn.x1) then sp.x1 := sn.x1;
      end
      else
      begin
        break;
      end;
      sp.next := sn.next;
      freeSpan(spf);
    end;
  end;

var
  sprev: Integer = -1;
  scur: Integer;
  sp: PSpan;
begin
  if (ax1 < ax0) then exit;
  if (y < 0) or (y >= mHeight) then exit;
  if (ax1 < -42) or (ax0 > mWidth+42) then exit;
  if (ax0 < -42) then ax0 := -42;
  if (ax1 > mWidth+42) then ax1 := mWidth+42;
  // new span on empty line?
  scur := lines[y];
  if (scur = -1) then
  begin
    lines[y] := allocSpan(ax0, ax1);
    exit;
  end;
  // starts before the first span?
  sp := @spans[scur];
  if (ax0 < sp.x0) then
  begin
    // insert new span as the first one
    sprev := allocSpan(ax0, ax1);
    spans[sprev].next := scur;
    lines[y] := sprev;
    // fix invalid spans (if any)
    fixFrom(sprev);
    exit;
  end;
  // find span to expand
  while (scur <> -1) do
  begin
    sp := @spans[scur];
    // join spans?
    if (sp.x1+1 = ax0) then
    begin
      sp.x1 := ax1;
      fixFrom(scur);
      exit;
    end;
    // starts in current span?
    if (ax0 >= sp.x0) and (ax0 <= sp.x1) then
    begin
      if (ax1 >= sp.x0) and (ax1 <= sp.x1) then exit; // ends in current span, nothing to do
      // extend current span, and fix bad spans
      sp.x1 := ax1;
      fixFrom(scur);
      exit;
    end;
    // starts after the current span, but before the next span?
    if (sp.next <> -1) and (ax0 > sp.x1) and (ax0 < spans[sp.next].x0) then
    begin
      // insert before next span
      sprev := allocSpan(ax0, ax1);
      spans[sprev].next := sp.next;
      sp.next := sprev;
      fixFrom(sp.next);
      exit;
    end;
    // try next span
    sprev := scur;
    scur := sp.next;
  end;
  // just append new span
  assert(sprev <> -1);
  spans[sprev].next := allocSpan(ax0, ax1);
end;


procedure TOutliner.addRect (x, y, w, h: Integer);
begin
  if (w < 1) or (h < 1) then exit;
  while (h > 0) do
  begin
    addSpan(x, x+w-1, y);
    Inc(y);
    Dec(h);
  end;
end;


function TOutliner.eachSpanAtY (y: Integer): TSpanEnumerator;
begin
  result := TSpanEnumerator.Create(self, y);
end;


function TOutliner.eachSpanEdgeAtY (y, dy: Integer): TSpanEdgeEnumerator;
begin
  result := TSpanEdgeEnumerator.Create(self, y, dy);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TOutliner.TSpanEnumerator.Create (master: TOutliner; y: Integer);
begin
  spans := master.spans;
  cur := -1;
  first := true;
  if (y < 0) or (y >= master.mHeight) then exit;
  cur := master.lines[y];
end;

function TOutliner.TSpanEnumerator.MoveNext (): Boolean; inline;
begin
       if first then first := false
  else if (cur <> -1) then cur := spans[cur].next;
  result := (cur <> -1);
end;

function TOutliner.TSpanEnumerator.getCurrent (): TSpanX; inline;
begin
  result.x0 := spans[cur].x0;
  result.x1 := spans[cur].x1;
end;

function TOutliner.TSpanEnumerator.GetEnumerator (): TSpanEnumerator; inline;
begin
  result := self;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TOutliner.TSpanEdgeEnumerator.GetEnumerator (): TSpanEdgeEnumerator; inline;
begin
  result := self;
end;

constructor TOutliner.TSpanEdgeEnumerator.Create (master: TOutliner; y, dy: Integer);
begin
  doSkipUSP := false;
  spans := master.spans;
  if (dy = 0) or (y < 0) or (y >= master.mHeight) then begin spi := -1; exit; end;

  spi := master.lines[y];
  if (spi = -1) then exit;

  if (dy < 0) then
  begin
    if (y < 1) then begin spi := -1; exit; end;
    usp := master.lines[y-1];
  end
  else
  begin
    if (y+1 >= master.mHeight) then begin spi := -1; exit; end;
    usp := master.lines[y+1];
  end;

  sx := spans[spi].x0;
  ex := spans[spi].x1;
end;

procedure TOutliner.TSpanEdgeEnumerator.nextSPI (); inline;
begin
  if (spi <> -1) then spi := spans[spi].next;
  if (spi <> -1) then
  begin
    sx := spans[spi].x0;
    ex := spans[spi].x1;
  end;
end;

function TOutliner.TSpanEdgeEnumerator.MoveNext (): Boolean; inline;
begin
  result := false;

  while true do
  begin
    if doSkipUSP then
    begin
      doSkipUSP := false;
      // skip usp (this will draw final dot)
      cur.x0 := spans[usp].x1;
      cur.x1 := cur.x0;
      sx := cur.x1+1;
      assert(sx <= ex);
      result := true;
      exit;
    end;

    if (spi = -1) then exit;

    // skip usp until sx
    while (usp <> -1) do
    begin
      if (spans[usp].x1 < sx) then begin usp := spans[usp].next; continue; end;
      break;
    end;

    // no more usps?
    if (usp = -1) then
    begin
      if (sx <= ex) then
      begin
        cur.x0 := sx;
        cur.x1 := ex;
        nextSPI();
        result := true;
      end
      else
      begin
        nextSPI();
        result := (spi <> -1);
        if result then
        begin
          cur.x0 := sx;
          cur.x1 := ex;
        end;
      end;
      exit;
    end;

    // usp covers the whole spi?
    if (sx >= spans[usp].x0) and (ex <= spans[usp].x1) then
    begin
      // yes; next spi
      nextSPI();
      continue;
    end;

    // usp starts after ex?
    if (ex < spans[usp].x0) then
    begin
      // yes; draw that part
      cur.x0 := sx;
      cur.x1 := ex;
      // next spi
      nextSPI();
      result := true;
      exit;
    end;

    // usp starts after sx?
    if (sx < spans[usp].x0) then
    begin
      // yes; draw that part
      cur.x0 := sx;
      cur.x1 := spans[usp].x0;
      // does usp covers what is left?
      if (ex <= spans[usp].x1) then
      begin
        // yes; next spi
        nextSPI();
      end
      else
      begin
        // no; skip usp
        doSkipUSP := true;
        //sx := spans[usp].x1+1;
        //assert(sx <= ex);
      end;
      result := true;
      exit;
    end
    else
    begin
      // usp starts before sx
      assert(sx >= spans[usp].x0);
      assert(ex > spans[usp].x1);
    end;

    // skip usp (this will draw final dot)
    doSkipUSP := true;
  end;
end;
