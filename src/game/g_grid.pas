(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
// universal spatial grid
{$INCLUDE ../shared/a_modes.inc}
unit g_grid;

interface


type
  TBodyProxyId = Integer;

  generic TBodyGridBase<ITP> = class(TObject)
  public
    type TGridQueryCB = function (obj: ITP; tag: Integer): Boolean is nested; // return `true` to stop
    type TGridRayQueryCB = function (obj: ITP; tag: Integer; x, y, prevx, prevy: Integer): Boolean is nested; // return `true` to stop
    type TGridAlongQueryCB = function (obj: ITP; tag: Integer): Boolean is nested; // return `true` to stop

    const TagDisabled = $40000000;
    const TagFullMask = $3fffffff;

  private
    const
      GridDefaultTileSize = 32;
      GridCellBucketSize = 8; // WARNING! can't be less than 2!

  private
    type
      PBodyProxyRec = ^TBodyProxyRec;
      TBodyProxyRec = record
      private
        mX, mY, mWidth, mHeight: Integer; // aabb
        mQueryMark: LongWord; // was this object visited at this query?
        mObj: ITP;
        mTag: Integer; // `TagDisabled` set: disabled ;-)
        nextLink: TBodyProxyId; // next free or nothing

      private
        procedure setup (aX, aY, aWidth, aHeight: Integer; aObj: ITP; aTag: Integer);
      end;

      PGridCell = ^TGridCell;
      TGridCell = record
        bodies: array [0..GridCellBucketSize-1] of Integer; // -1: end of list
        next: Integer; // in this cell; index in mCells
      end;

      TGridInternalCB = function (grida: Integer; bodyId: TBodyProxyId): Boolean of object; // return `true` to stop

  private
    //mTileSize: Integer;
    const mTileSize = GridDefaultTileSize;

  private
    mMinX, mMinY: Integer; // so grids can start at any origin
    mWidth, mHeight: Integer; // in tiles
    mGrid: array of Integer; // mWidth*mHeight, index in mCells
    mCells: array of TGridCell; // cell pool
    mFreeCell: Integer; // first free cell index or -1
    mLastQuery: LongWord;
    mUsedCells: Integer;
    mProxies: array of TBodyProxyRec;
    mProxyFree: TBodyProxyId; // free
    mProxyCount: Integer; // currently used
    mProxyMaxCount: Integer;

  public
    dbgShowTraceLog: Boolean;

  private
    function allocCell (): Integer;
    procedure freeCell (idx: Integer); // `next` is simply overwritten

    function allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: ITP; aTag: Integer): TBodyProxyId;
    procedure freeProxy (body: TBodyProxyId);

    procedure insertInternal (body: TBodyProxyId);
    procedure removeInternal (body: TBodyProxyId);

    function forGridRect (x, y, w, h: Integer; cb: TGridInternalCB; bodyId: TBodyProxyId): Boolean;

    function inserter (grida: Integer; bodyId: TBodyProxyId): Boolean;
    function remover (grida: Integer; bodyId: TBodyProxyId): Boolean;

    function getProxyEnabled (pid: TBodyProxyId): Boolean; inline;
    procedure setProxyEnabled (pid: TBodyProxyId; val: Boolean); inline;

    function getGridWidthPx (): Integer; inline;
    function getGridHeightPx (): Integer; inline;

  public
    constructor Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer{; aTileSize: Integer=GridDefaultTileSize});
    destructor Destroy (); override;

    function insertBody (aObj: ITP; ax, ay, aWidth, aHeight: Integer; aTag: Integer=-1): TBodyProxyId;
    procedure removeBody (body: TBodyProxyId); // WARNING! this WILL destroy proxy!

    procedure moveBody (body: TBodyProxyId; dx, dy: Integer);
    procedure resizeBody (body: TBodyProxyId; sx, sy: Integer);
    procedure moveResizeBody (body: TBodyProxyId; dx, dy, sx, sy: Integer);

    function insideGrid (x, y: Integer): Boolean; inline;

    // `false` if `body` is surely invalid
    function getBodyXY (body: TBodyProxyId; out rx, ry: Integer): Boolean; inline;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // no callback: return `true` on the first hit
    function forEachInAABB (x, y, w, h: Integer; cb: TGridQueryCB; tagmask: Integer=-1; allowDisabled: Boolean=false): ITP;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // no callback: return `true` on the first hit
    function forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // cb with `(nil)` will be called before processing new tile
    // no callback: return `true` on the nearest hit
    function traceRay (x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP; overload;
    function traceRay (out ex, ey: Integer; ax0, ay0, ax1, ay1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // trace line along the grid, calling `cb` for all objects in passed cells, in no particular order
    function forEachAlongLine (x0, y0, x1, y1: Integer; cb: TGridAlongQueryCB; tagmask: Integer=-1): ITP;

    procedure dumpStats ();

    //WARNING! no sanity checks!
    property proxyEnabled[pid: TBodyProxyId]: Boolean read getProxyEnabled write setProxyEnabled;

    property gridX0: Integer read mMinX;
    property gridY0: Integer read mMinY;
    property gridWidth: Integer read getGridWidthPx; // in pixels
    property gridHeight: Integer read getGridHeightPx; // in pixels
  end;


// you are not supposed to understand this
// returns `true` if there is an intersection, and enter coords
// enter coords will be equal to (x0, y0) if starting point is inside the box
// if result is `false`, `inx` and `iny` are undefined
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer; out inx, iny: Integer): Boolean;

procedure swapInt (var a: Integer; var b: Integer); inline;
function distanceSq (x0, y0, x1, y1: Integer): Integer; inline;


implementation

uses
  SysUtils, e_log;


// ////////////////////////////////////////////////////////////////////////// //
procedure swapInt (var a: Integer; var b: Integer); inline; var t: Integer; begin t := a; a := b; b := t; end;

function distanceSq (x0, y0, x1, y1: Integer): Integer; inline; begin result := (x1-x0)*(x1-x0)+(y1-y0)*(y1-y0); end;


// ////////////////////////////////////////////////////////////////////////// //
// you are not supposed to understand this
// returns `true` if there is an intersection, and enter coords
// enter coords will be equal to (x0, y0) if starting point is inside the box
// if result is `false`, `inx` and `iny` are undefined
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer; out inx, iny: Integer): Boolean;
var
  wx0, wy0, wx1, wy1: Integer; // window coordinates
  stx, sty: Integer; // "steps" for x and y axes
  dsx, dsy: Integer; // "lengthes" for x and y axes
  dx2, dy2: Integer; // "double lengthes" for x and y axes
  xd, yd: Integer; // current coord
  e: Integer; // "error" (as in bresenham algo)
  rem: Integer;
  term: Integer;
  d0, d1: PInteger;
  xfixed: Boolean;
  temp: Integer;
begin
  result := false;
  // why not
  inx := x0;
  iny := y0;
  if (bw < 1) or (bh < 1) then exit; // impossible box

  if (x0 = x1) and (y0 = y1) then
  begin
    // check this point
    result := (x0 >= bx) and (y0 >= by) and (x0 < bx+bw) and (y0 < by+bh);
    exit;
  end;

  // check if staring point is inside the box
  if (x0 >= bx) and (y0 >= by) and (x0 < bx+bw) and (y0 < by+bh) then begin result := true; exit; end;

  // clip rectange
  wx0 := bx;
  wy0 := by;
  wx1 := bx+bw-1;
  wy1 := by+bh-1;

  // horizontal setup
  if (x0 < x1) then
  begin
    // from left to right
    if (x0 > wx1) or (x1 < wx0) then exit; // out of screen
    stx := 1; // going right
  end
  else
  begin
    // from right to left
    if (x1 > wx1) or (x0 < wx0) then exit; // out of screen
    stx := -1; // going left
    x0 := -x0;
    x1 := -x1;
    wx0 := -wx0;
    wx1 := -wx1;
    swapInt(wx0, wx1);
  end;

  // vertical setup
  if (y0 < y1) then
  begin
    // from top to bottom
    if (y0 > wy1) or (y1 < wy0) then exit; // out of screen
    sty := 1; // going down
  end
  else
  begin
    // from bottom to top
    if (y1 > wy1) or (y0 < wy0) then exit; // out of screen
    sty := -1; // going up
    y0 := -y0;
    y1 := -y1;
    wy0 := -wy0;
    wy1 := -wy1;
    swapInt(wy0, wy1);
  end;

  dsx := x1-x0;
  dsy := y1-y0;

  if (dsx < dsy) then
  begin
    d0 := @yd;
    d1 := @xd;
    swapInt(x0, y0);
    swapInt(x1, y1);
    swapInt(dsx, dsy);
    swapInt(wx0, wy0);
    swapInt(wx1, wy1);
    swapInt(stx, sty);
  end
  else
  begin
    d0 := @xd;
    d1 := @yd;
  end;

  dx2 := 2*dsx;
  dy2 := 2*dsy;
  xd := x0;
  yd := y0;
  e := 2*dsy-dsx;
  term := x1;

  xfixed := false;
  if (y0 < wy0) then
  begin
    // clip at top
    temp := dx2*(wy0-y0)-dsx;
    xd += temp div dy2;
    rem := temp mod dy2;
    if (xd > wx1) then exit; // x is moved out of clipping rect, nothing to do
    if (xd+1 >= wx0) then
    begin
      yd := wy0;
      e -= rem+dsx;
      if (rem > 0) then begin Inc(xd); e += dy2; end;
      xfixed := true;
    end;
  end;

  if (not xfixed) and (x0 < wx0) then
  begin
    // clip at left
    temp := dy2*(wx0-x0);
    yd += temp div dx2;
    rem := temp mod dx2;
    if (yd > wy1) or (yd = wy1) and (rem >= dsx) then exit;
    xd := wx0;
    e += rem;
    if (rem >= dsx) then begin Inc(yd); e -= dx2; end;
  end;

  if (y1 > wy1) then
  begin
    // clip at bottom
    temp := dx2*(wy1-y0)+dsx;
    term := x0+temp div dy2;
    rem := temp mod dy2;
    if (rem = 0) then Dec(term);
  end;

  if (term > wx1) then term := wx1; // clip at right

  Inc(term); // draw last point
  //if (term = xd) then exit; // this is the only point, get out of here

  if (sty = -1) then yd := -yd;
  if (stx = -1) then begin xd := -xd; term := -term; end;
  dx2 -= dy2;

  inx := d0^;
  iny := d1^;
  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TBodyGridBase.TBodyProxyRec.setup (aX, aY, aWidth, aHeight: Integer; aObj: ITP; aTag: Integer);
begin
  mX := aX;
  mY := aY;
  mWidth := aWidth;
  mHeight := aHeight;
  mQueryMark := 0;
  mObj := aObj;
  mTag := aTag;
  nextLink := -1;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBodyGridBase.Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer{; aTileSize: Integer=GridDefaultTileSize});
var
  idx: Integer;
begin
  dbgShowTraceLog := false;
  {
  if aTileSize < 1 then aTileSize := 1;
  if aTileSize > 8192 then aTileSize := 8192; // arbitrary limit
  mTileSize := aTileSize;
  }
  if (aPixWidth < mTileSize) then aPixWidth := mTileSize;
  if (aPixHeight < mTileSize) then aPixHeight := mTileSize;
  mMinX := aMinPixX;
  mMinY := aMinPixY;
  mWidth := (aPixWidth+mTileSize-1) div mTileSize;
  mHeight := (aPixHeight+mTileSize-1) div mTileSize;
  SetLength(mGrid, mWidth*mHeight);
  SetLength(mCells, mWidth*mHeight);
  SetLength(mProxies, 8192);
  mFreeCell := 0;
  // init free list
  for idx := 0 to High(mCells) do
  begin
    mCells[idx].bodies[0] := -1;
    mCells[idx].next := idx+1;
  end;
  mCells[High(mCells)].next := -1; // last cell
  // init grid
  for idx := 0 to High(mGrid) do mGrid[idx] := -1;
  // init proxies
  for idx := 0 to High(mProxies) do mProxies[idx].nextLink := idx+1;
  mProxies[High(mProxies)].nextLink := -1;
  mLastQuery := 0;
  mUsedCells := 0;
  mProxyFree := 0;
  mProxyCount := 0;
  mProxyMaxCount := 0;
  e_WriteLog(Format('created grid with size: %dx%d (tile size: %d); pix: %dx%d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize]), MSG_NOTIFY);
end;


destructor TBodyGridBase.Destroy ();
begin
  mCells := nil;
  mGrid := nil;
  mProxies := nil;
  inherited;
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TBodyGridBase.dumpStats ();
var
  idx, mcb, cidx, cnt: Integer;
begin
  mcb := 0;
  for idx := 0 to High(mGrid) do
  begin
    cidx := mGrid[idx];
    cnt := 0;
    while cidx >= 0 do
    begin
      Inc(cnt);
      cidx := mCells[cidx].next;
    end;
    if (mcb < cnt) then mcb := cnt;
  end;
  e_WriteLog(Format('grid size: %dx%d (tile size: %d); pix: %dx%d; used cells: %d; max bodies in cell: %d; max proxies allocated: %d; proxies used: %d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize, mUsedCells, mcb, mProxyMaxCount, mProxyCount]), MSG_NOTIFY);
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.getGridWidthPx (): Integer; inline; begin result := mWidth*mTileSize; end;
function TBodyGridBase.getGridHeightPx (): Integer; inline; begin result := mHeight*mTileSize; end;


function TBodyGridBase.insideGrid (x, y: Integer): Boolean; inline;
begin
  // fix coords
  Dec(x, mMinX);
  Dec(y, mMinY);
  result := (x >= 0) and (y >= 0) and (x < mWidth*mTileSize) and (y < mHeight*mTileSize);
end;


function TBodyGridBase.getBodyXY (body: TBodyProxyId; out rx, ry: Integer): Boolean; inline;
begin
  if (body >= 0) and (body < Length(mProxies)) then
  begin
    with mProxies[body] do begin rx := mX; ry := mY; end;
    result := true;
  end
  else
  begin
    rx := 0;
    ry := 0;
    result := false;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.getProxyEnabled (pid: TBodyProxyId): Boolean; inline;
begin
  if (pid >= 0) then result := ((mProxies[pid].mTag and TagDisabled) = 0) else result := false;
end;


procedure TBodyGridBase.setProxyEnabled (pid: TBodyProxyId; val: Boolean); inline;
begin
  if (pid >= 0) then
  begin
    if val then
    begin
      mProxies[pid].mTag := mProxies[pid].mTag and not TagDisabled;
    end
    else
    begin
      mProxies[pid].mTag := mProxies[pid].mTag or TagDisabled;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.allocCell (): Integer;
var
  idx: Integer;
begin
  if (mFreeCell < 0) then
  begin
    // no free cells, want more
    mFreeCell := Length(mCells);
    SetLength(mCells, mFreeCell+32768); // arbitrary number
    for idx := mFreeCell to High(mCells) do
    begin
      mCells[idx].bodies[0] := -1;
      mCells[idx].next := idx+1;
    end;
    mCells[High(mCells)].next := -1; // last cell
  end;
  result := mFreeCell;
  mFreeCell := mCells[result].next;
  mCells[result].next := -1;
  mCells[result].bodies[0] := -1;
  Inc(mUsedCells);
  //e_WriteLog(Format('grid: allocated new cell #%d (total: %d)', [result, mUsedCells]), MSG_NOTIFY);
end;


procedure TBodyGridBase.freeCell (idx: Integer);
begin
  if (idx >= 0) and (idx < Length(mCells)) then
  begin
    //if mCells[idx].body = -1 then exit; // the thing that should not be
    mCells[idx].bodies[0] := -1;
    mCells[idx].next := mFreeCell;
    mFreeCell := idx;
    Dec(mUsedCells);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: ITP; aTag: Integer): TBodyProxyId;
var
  olen, idx: Integer;
  px: PBodyProxyRec;
begin
  if (mProxyFree = -1) then
  begin
    // no free proxies, resize list
    olen := Length(mProxies);
    SetLength(mProxies, olen+8192); // arbitrary number
    for idx := olen to High(mProxies) do mProxies[idx].nextLink := idx+1;
    mProxies[High(mProxies)].nextLink := -1;
    mProxyFree := olen;
  end;
  // get one from list
  result := mProxyFree;
  px := @mProxies[result];
  mProxyFree := px.nextLink;
  px.setup(aX, aY, aWidth, aHeight, aObj, aTag);
  // add to used list
  px.nextLink := -1;
  // statistics
  Inc(mProxyCount);
  if (mProxyMaxCount < mProxyCount) then mProxyMaxCount := mProxyCount;
end;

procedure TBodyGridBase.freeProxy (body: TBodyProxyId);
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (mProxyCount = 0) then raise Exception.Create('wutafuuuuu in grid (no allocated proxies, what i should free now?)');
  // add to free list
  mProxies[body].mObj := nil;
  mProxies[body].nextLink := mProxyFree;
  mProxyFree := body;
  Dec(mProxyCount);
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.forGridRect (x, y, w, h: Integer; cb: TGridInternalCB; bodyId: TBodyProxyId): Boolean;
const
  tsize = mTileSize;
var
  gx, gy: Integer;
  gw, gh: Integer;
begin
  result := false;
  if (w < 1) or (h < 1) or not assigned(cb) then exit;
  // fix coords
  Dec(x, mMinX);
  Dec(y, mMinY);
  // go on
  if (x+w <= 0) or (y+h <= 0) then exit;
  gw := mWidth;
  gh := mHeight;
  //tsize := mTileSize;
  if (x >= gw*tsize) or (y >= gh*tsize) then exit;
  for gy := y div tsize to (y+h-1) div tsize do
  begin
    if (gy < 0) then continue;
    if (gy >= gh) then break;
    for gx := x div tsize to (x+w-1) div tsize do
    begin
      if (gx < 0) then continue;
      if (gx >= gw) then break;
      result := cb(gy*gw+gx, bodyId);
      if result then exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.inserter (grida: Integer; bodyId: TBodyProxyId): Boolean;
var
  cidx: Integer;
  pc: Integer;
  pi: PGridCell;
  f: Integer;
begin
  result := false; // never stop
  // add body to the given grid cell
  pc := mGrid[grida];
  if (pc <> -1) then
  begin
    pi := @mCells[pc];
    f := 0;
    for f := 0 to High(TGridCell.bodies) do
    begin
      if (pi.bodies[f] = -1) then
      begin
        // can add here
        pi.bodies[f] := bodyId;
        if (f+1 < Length(TGridCell.bodies)) then pi.bodies[f+1] := -1;
        exit;
      end;
    end;
  end;
  // either no room, or no cell at all
  cidx := allocCell();
  mCells[cidx].bodies[0] := bodyId;
  mCells[cidx].bodies[1] := -1;
  mCells[cidx].next := pc;
  mGrid[grida] := cidx;
end;

procedure TBodyGridBase.insertInternal (body: TBodyProxyId);
var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, inserter, body);
end;


// absolutely not tested
function TBodyGridBase.remover (grida: Integer; bodyId: TBodyProxyId): Boolean;
var
  f: Integer;
  pidx, idx, tmp: Integer;
  pc: PGridCell;
begin
  result := false; // never stop
  // find and remove cell
  pidx := -1;
  idx := mGrid[grida];
  while (idx >= 0) do
  begin
    tmp := mCells[idx].next;
    pc := @mCells[idx];
    f := 0;
    while (f < High(TGridCell.bodies)) do
    begin
      if (pc.bodies[f] = bodyId) then
      begin
        // i found her!
        if (f = 0) and (pc.bodies[1] = -1) then
        begin
          // this cell contains no elements, remove it
          tmp := mCells[idx].next;
          if (pidx = -1) then mGrid[grida] := tmp else mCells[pidx].next := tmp;
          freeCell(idx);
        end
        else
        begin
          // remove element from bucket
          Inc(f);
          while (f < High(TGridCell.bodies)) do
          begin
            pc.bodies[f-1] := pc.bodies[f];
            if (pc.bodies[f] = -1) then break;
            Inc(f);
          end;
          pc.bodies[High(TGridCell.bodies)] := -1; // just in case
        end;
        exit; // assume that we cannot have one object added to bucket twice
      end;
      Inc(f);
    end;
    pidx := idx;
    idx := tmp;
  end;
end;

// absolutely not tested
procedure TBodyGridBase.removeInternal (body: TBodyProxyId);
var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, remover, body);
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.insertBody (aObj: ITP; aX, aY, aWidth, aHeight: Integer; aTag: Integer=-1): TBodyProxyId;
begin
  aTag := aTag and TagFullMask;
  result := allocProxy(aX, aY, aWidth, aHeight, aObj, aTag);
  insertInternal(result);
end;


procedure TBodyGridBase.removeBody (body: TBodyProxyId);
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  removeInternal(body);
  freeProxy(body);
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure TBodyGridBase.moveResizeBody (body: TBodyProxyId; dx, dy, sx, sy: Integer);
var
  px: PBodyProxyRec;
  x0, y0, w, h: Integer;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (dx = 0) and (dy = 0) and (sx = 0) and (sy = 0) then exit;
  px := @mProxies[body];
  x0 := px.mX;
  y0 := px.mY;
  w := px.mWidth;
  h := px.mHeight;
  // did any corner crossed tile boundary?
  if (x0 div mTileSize <> (x0+dx) div mTileSize) or
     (y0 div mTileSize <> (y0+dx) div mTileSize) or
     ((x0+w) div mTileSize <> (x0+w+sx) div mTileSize) or
     ((y0+h) div mTileSize <> (y0+h+sy) div mTileSize) then
  begin
    removeInternal(body);
    Inc(px.mX, dx);
    Inc(px.mY, dy);
    Inc(px.mWidth, sx);
    Inc(px.mHeight, sy);
    insertInternal(body);
  end
  else
  begin
    Inc(px.mX, dx);
    Inc(px.mY, dy);
    Inc(px.mWidth, sx);
    Inc(px.mHeight, sy);
  end;
end;

procedure TBodyGridBase.moveBody (body: TBodyProxyId; dx, dy: Integer);
var
  px: PBodyProxyRec;
  nx, ny: Integer;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (dx = 0) and (dy = 0) then exit;
  // check if tile coords was changed
  px := @mProxies[body];
  nx := px.mX+dx;
  ny := px.mY+dy;
  if (nx div mTileSize <> px.mX div mTileSize) or (ny div mTileSize <> px.mY div mTileSize) then
  begin
    // crossed tile boundary, do heavy work
    moveResizeBody(body, dx, dy, 0, 0);
  end
  else
  begin
    // nothing to do with the grid, just fix coordinates
    px.mX := nx;
    px.mY := ny;
  end;
end;

procedure TBodyGridBase.resizeBody (body: TBodyProxyId; sx, sy: Integer);
var
  px: PBodyProxyRec;
  x0, y0: Integer;
  nw, nh: Integer;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (sx = 0) and (sy = 0) then exit;
  // check if tile coords was changed
  px := @mProxies[body];
  x0 := px.mX;
  y0 := px.mY;
  nw := px.mWidth+sx;
  nh := px.mHeight+sy;
  if ((x0+px.mWidth) div mTileSize <> (x0+nw) div mTileSize) or
     ((y0+px.mHeight) div mTileSize <> (y0+nh) div mTileSize) then
  begin
    // crossed tile boundary, do heavy work
    moveResizeBody(body, 0, 0, sx, sy);
  end
  else
  begin
    // nothing to do with the grid, just fix size
    px.mWidth := nw;
    px.mHeight := nh;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the first hit
function TBodyGridBase.forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;
var
  f: Integer;
  idx, curci: Integer;
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
  ptag: Integer;
begin
  result := Default(ITP);
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  // make coords (0,0)-based
  Dec(x, mMinX);
  Dec(y, mMinY);
  if (x < 0) or (y < 0) or (x >= mWidth*mTileSize) or (y >= mHeight*mTileSize) then exit;

  curci := mGrid[(y div mTileSize)*mWidth+(x div mTileSize)];
  // restore coords
  Inc(x, mMinX);
  Inc(y, mMinY);

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for idx := 0 to High(mProxies) do mProxies[idx].mQueryMark := 0;
  end;
  lq := mLastQuery;

  while (curci <> -1) do
  begin
    cc := @mCells[curci];
    for f := 0 to High(TGridCell.bodies) do
    begin
      if (cc.bodies[f] = -1) then break;
      px := @mProxies[cc.bodies[f]];
      ptag := px.mTag;
      if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
      begin
        if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
        begin
          px.mQueryMark := lq;
          if assigned(cb) then
          begin
            if cb(px.mObj, ptag) then begin result := px.mObj; exit; end;
          end
          else
          begin
            result := px.mObj;
            exit;
          end;
        end;
      end;
    end;
    curci := cc.next;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the first hit
function TBodyGridBase.forEachInAABB (x, y, w, h: Integer; cb: TGridQueryCB; tagmask: Integer=-1; allowDisabled: Boolean=false): ITP;
const
  tsize = mTileSize;
var
  idx: Integer;
  gx, gy: Integer;
  curci: Integer;
  f: Integer;
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
  gw: Integer;
  x0, y0: Integer;
  ptag: Integer;
begin
  result := Default(ITP);
  if (w < 1) or (h < 1) then exit;
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  x0 := x;
  y0 := y;

  // fix coords
  Dec(x, mMinX);
  Dec(y, mMinY);

  gw := mWidth;
  //tsize := mTileSize;

  if (x+w <= 0) or (y+h <= 0) then exit;
  if (x >= gw*tsize) or (y >= mHeight*tsize) then exit;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for idx := 0 to High(mProxies) do mProxies[idx].mQueryMark := 0;
  end;
  //e_WriteLog(Format('grid: query #%d: (%d,%d)-(%dx%d)', [mLastQuery, minx, miny, maxx, maxy]), MSG_NOTIFY);
  lq := mLastQuery;

  // go on
  for gy := y div tsize to (y+h-1) div tsize do
  begin
    if (gy < 0) then continue;
    if (gy >= mHeight) then break;
    for gx := x div tsize to (x+w-1) div tsize do
    begin
      if (gx < 0) then continue;
      if (gx >= gw) then break;
      // process cells
      curci := mGrid[gy*gw+gx];
      while (curci <> -1) do
      begin
        cc := @mCells[curci];
        for f := 0 to High(TGridCell.bodies) do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          ptag := px.mTag;
          if (not allowDisabled) and ((ptag and TagDisabled) <> 0) then continue;
          if ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
          //if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
          //if ( ((ptag and TagDisabled) = 0) = ignoreDisabled) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
          begin
            if (x0 >= px.mX+px.mWidth) or (y0 >= px.mY+px.mHeight) then continue;
            if (x0+w <= px.mX) or (y0+h <= px.mY) then continue;
            px.mQueryMark := lq;
            if assigned(cb) then
            begin
              if cb(px.mObj, ptag) then begin result := px.mObj; exit; end;
            end
            else
            begin
              result := px.mObj;
              exit;
            end;
          end;
        end;
        curci := cc.next;
      end;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the nearest hit
function TBodyGridBase.traceRay (x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
var
  ex, ey: Integer;
begin
  result := traceRay(ex, ey, x0, y0, x1, y1, cb, tagmask);
end;


// no callback: return `true` on the nearest hit
// you are not supposed to understand this
function TBodyGridBase.traceRay (out ex, ey: Integer; ax0, ay0, ax1, ay1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
const
  tsize = mTileSize;
var
  wx0, wy0, wx1, wy1: Integer; // window coordinates
  stx, sty: Integer; // "steps" for x and y axes
  dsx, dsy: Integer; // "lengthes" for x and y axes
  dx2, dy2: Integer; // "double lengthes" for x and y axes
  xd, yd: Integer; // current coord
  e: Integer; // "error" (as in bresenham algo)
  rem: Integer;
  term: Integer;
  xptr, yptr: PInteger;
  xfixed: Boolean;
  temp: Integer;
  prevx, prevy: Integer;
  lastDistSq: Integer;
  ccidx, curci: Integer;
  hasUntried: Boolean;
  lastGA: Integer = -1;
  ga, x, y: Integer;
  lastObj: ITP;
  wasHit: Boolean = false;
  gw, gh, minx, miny, maxx, maxy: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  lq: LongWord;
  f, ptag, distSq: Integer;
  x0, y0, x1, y1: Integer;
begin
  result := Default(ITP);
  lastObj := Default(ITP);
  tagmask := tagmask and TagFullMask;
  ex := ax1; // why not?
  ey := ay1; // why not?
  if (tagmask = 0) then exit;

  if (ax0 = ax1) and (ay0 = ay1) then exit; // as the first point is ignored, just get outta here

  lastDistSq := distanceSq(ax0, ay0, ax1, ay1)+1;

  gw := mWidth;
  gh := mHeight;
  minx := mMinX;
  miny := mMinY;
  maxx := gw*tsize-1;
  maxy := gh*tsize-1;

  x0 := ax0;
  y0 := ay0;
  x1 := ax1;
  y1 := ay1;

  // offset query coords to (0,0)-based
  Dec(x0, minx);
  Dec(y0, miny);
  Dec(x1, minx);
  Dec(y1, miny);

  // clip rectange
  wx0 := 0;
  wy0 := 0;
  wx1 := maxx;
  wy1 := maxy;

  // horizontal setup
  if (x0 < x1) then
  begin
    // from left to right
    if (x0 > wx1) or (x1 < wx0) then exit; // out of screen
    stx := 1; // going right
  end
  else
  begin
    // from right to left
    if (x1 > wx1) or (x0 < wx0) then exit; // out of screen
    stx := -1; // going left
    x0 := -x0;
    x1 := -x1;
    wx0 := -wx0;
    wx1 := -wx1;
    swapInt(wx0, wx1);
  end;

  // vertical setup
  if (y0 < y1) then
  begin
    // from top to bottom
    if (y0 > wy1) or (y1 < wy0) then exit; // out of screen
    sty := 1; // going down
  end
  else
  begin
    // from bottom to top
    if (y1 > wy1) or (y0 < wy0) then exit; // out of screen
    sty := -1; // going up
    y0 := -y0;
    y1 := -y1;
    wy0 := -wy0;
    wy1 := -wy1;
    swapInt(wy0, wy1);
  end;

  dsx := x1-x0;
  dsy := y1-y0;

  if (dsx < dsy) then
  begin
    xptr := @yd;
    yptr := @xd;
    swapInt(x0, y0);
    swapInt(x1, y1);
    swapInt(dsx, dsy);
    swapInt(wx0, wy0);
    swapInt(wx1, wy1);
    swapInt(stx, sty);
  end
  else
  begin
    xptr := @xd;
    yptr := @yd;
  end;

  dx2 := 2*dsx;
  dy2 := 2*dsy;
  xd := x0;
  yd := y0;
  e := 2*dsy-dsx;
  term := x1;

  xfixed := false;
  if (y0 < wy0) then
  begin
    // clip at top
    temp := dx2*(wy0-y0)-dsx;
    xd += temp div dy2;
    rem := temp mod dy2;
    if (xd > wx1) then exit; // x is moved out of clipping rect, nothing to do
    if (xd+1 >= wx0) then
    begin
      yd := wy0;
      e -= rem+dsx;
      if (rem > 0) then begin Inc(xd); e += dy2; end;
      xfixed := true;
    end;
  end;

  if (not xfixed) and (x0 < wx0) then
  begin
    // clip at left
    temp := dy2*(wx0-x0);
    yd += temp div dx2;
    rem := temp mod dx2;
    if (yd > wy1) or (yd = wy1) and (rem >= dsx) then exit;
    xd := wx0;
    e += rem;
    if (rem >= dsx) then begin Inc(yd); e -= dx2; end;
  end;

  if (y1 > wy1) then
  begin
    // clip at bottom
    temp := dx2*(wy1-y0)+dsx;
    term := x0+temp div dy2;
    rem := temp mod dy2;
    if (rem = 0) then Dec(term);
  end;

  if (term > wx1) then term := wx1; // clip at right

  Inc(term); // draw last point
  //if (term = xd) then exit; // this is the only point, get out of here

  if (sty = -1) then yd := -yd;
  if (stx = -1) then begin xd := -xd; term := -term; end;
  dx2 -= dy2;

  // first move, to skip starting point
  if (xd = term) then exit;
  prevx := xptr^+minx;
  prevy := yptr^+miny;
  // move coords
  if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
  xd += stx;
  // done?
  if (xd = term) then exit;

  if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*tsize) and (yptr^ > mHeight*tsize) then raise Exception.Create('raycaster internal error (0)');

  //if (dbgShowTraceLog) then e_WriteLog(Format('raycast start: (%d,%d)-(%d,%d); xptr^=%d; yptr^=%d', [ax0, ay0, ax1, ay1, xptr^, yptr^]), MSG_NOTIFY);

  // restore query coords
  Inc(ax0, minx);
  Inc(ay0, miny);
  //Inc(ax1, minx);
  //Inc(ay1, miny);

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for f := 0 to High(mProxies) do mProxies[f].mQueryMark := 0;
  end;
  lq := mLastQuery;

  ccidx := -1;
  // draw it; can omit checks
  while (xd <> term) do
  begin
    // check cell(s)
    if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*tsize) and (yptr^ > mHeight*tsize) then raise Exception.Create('raycaster internal error (0)');
    // new tile?
    ga := (yptr^ div tsize)*gw+(xptr^ div tsize);
    if (ga <> lastGA) then
    begin
      // yes
      if (ccidx <> -1) then
      begin
        // signal cell completion
        if assigned(cb) then
        begin
          if cb(nil, 0, xptr^+minx, yptr^+miny, prevx, prevy) then begin result := lastObj; exit; end;
        end
        else if wasHit then
        begin
          result := lastObj;
          exit;
        end;
      end;
      lastGA := ga;
      ccidx := mGrid[lastGA];
    end;
    // has something to process in this tile?
    if (ccidx <> -1) then
    begin
      // process cell
      curci := ccidx;
      hasUntried := false; // this will be set to `true` if we have some proxies we still want to process at the next step
      // convert coords to map (to avoid ajdusting coords inside the loop)
      x := xptr^+minx;
      y := yptr^+miny;
      // process cell list
      while (curci <> -1) do
      begin
        cc := @mCells[curci];
        for f := 0 to High(TGridCell.bodies) do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          ptag := px.mTag;
          if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
          begin
            // can we process this proxy?
            if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
            begin
              px.mQueryMark := lq; // mark as processed
              if assigned(cb) then
              begin
                if cb(px.mObj, ptag, x, y, prevx, prevy) then
                begin
                  result := lastObj;
                  ex := prevx;
                  ey := prevy;
                  exit;
                end;
              end
              else
              begin
                // remember this hitpoint if it is nearer than an old one
                distSq := distanceSq(ax0, ay0, prevx, prevy);
                if (distSq < lastDistSq) then
                begin
                  wasHit := true;
                  lastDistSq := distSq;
                  ex := prevx;
                  ey := prevy;
                  lastObj := px.mObj;
                end;
              end;
            end
            else
            begin
              // this is possibly interesting proxy, set "has more to check" flag
              hasUntried := true;
            end;
          end;
        end;
        // next cell
        curci := cc.next;
      end;
      // still has something interesting in this cell?
      if not hasUntried then
      begin
        // nope, don't process this cell anymore; signal cell completion
        ccidx := -1;
        if assigned(cb) then
        begin
          if cb(nil, 0, x, y, prevx, prevy) then begin result := lastObj; exit; end;
        end
        else if wasHit then
        begin
          result := lastObj;
          exit;
        end;
      end;
    end;
    //putPixel(xptr^, yptr^);
    // move coords
    prevx := xptr^+minx;
    prevy := yptr^+miny;
    if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
    xd += stx;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
//FIXME! optimize this with real tile walking
function TBodyGridBase.forEachAlongLine (x0, y0, x1, y1: Integer; cb: TGridAlongQueryCB; tagmask: Integer=-1): ITP;
const
  tsize = mTileSize;
var
  i: Integer;
  dx, dy, d: Integer;
  xerr, yerr: Integer;
  incx, incy: Integer;
  stepx, stepy: Integer;
  x, y: Integer;
  maxx, maxy: Integer;
  gw, gh: Integer;
  ccidx: Integer;
  curci: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  lq: LongWord;
  minx, miny: Integer;
  ptag: Integer;
  lastWasInGrid: Boolean;
  tbcross: Boolean;
  f: Integer;
begin
  result := Default(ITP);
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  minx := mMinX;
  miny := mMinY;

  dx := x1-x0;
  dy := y1-y0;

  if (dx > 0) then incx := 1 else if (dx < 0) then incx := -1 else incx := 0;
  if (dy > 0) then incy := 1 else if (dy < 0) then incy := -1 else incy := 0;

  dx := abs(dx);
  dy := abs(dy);

  if (dx > dy) then d := dx else d := dy;

  // `x` and `y` will be in grid coords
  x := x0-minx;
  y := y0-miny;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for i := 0 to High(mProxies) do mProxies[i].mQueryMark := 0;
  end;
  lq := mLastQuery;

  // cache various things
  //tsize := mTileSize;
  gw := mWidth;
  gh := mHeight;
  maxx := gw*tsize-1;
  maxy := gh*tsize-1;

  // setup distance and flags
  lastWasInGrid := (x >= 0) and (y >= 0) and (x <= maxx) and (y <= maxy);

  // setup starting tile ('cause we'll adjust tile vars only on tile edge crossing)
  if lastWasInGrid then ccidx := mGrid[(y div tsize)*gw+(x div tsize)] else ccidx := -1;

  // it is slightly faster this way
  xerr := -d;
  yerr := -d;

  // now trace
  for i := 1 to d do
  begin
    // do one step
    xerr += dx;
    yerr += dy;
    // invariant: one of those always changed
    if (xerr < 0) and (yerr < 0) then raise Exception.Create('internal bug in grid raycaster (0)');
    if (xerr >= 0) then begin xerr -= d; x += incx; stepx := incx; end else stepx := 0;
    if (yerr >= 0) then begin yerr -= d; y += incy; stepy := incy; end else stepy := 0;
    // invariant: we always doing a step
    if ((stepx or stepy) = 0) then raise Exception.Create('internal bug in grid raycaster (1)');
    begin
      // check for crossing tile/grid boundary
      if (x >= 0) and (y >= 0) and (x <= maxx) and (y <= maxy) then
      begin
        // we're still in grid
        lastWasInGrid := true;
        // check for tile edge crossing
             if (stepx < 0) and ((x mod tsize) = tsize-1) then tbcross := true
        else if (stepx > 0) and ((x mod tsize) = 0) then tbcross := true
        else if (stepy < 0) and ((y mod tsize) = tsize-1) then tbcross := true
        else if (stepy > 0) and ((y mod tsize) = 0) then tbcross := true
        else tbcross := false;
        // crossed tile edge?
        if tbcross then
        begin
          // setup new cell index
          ccidx := mGrid[(y div tsize)*gw+(x div tsize)];
        end;
      end
      else
      begin
        // out of grid
        if lastWasInGrid then exit; // oops, stepped out of the grid -- there is no way to return
      end;
    end;

    // has something to process in the current cell?
    if (ccidx <> -1) then
    begin
      // process cell
      curci := ccidx;
      // convert coords to map (to avoid ajdusting coords inside the loop)
      Inc(x, minx);
      Inc(y, miny);
      // process cell list
      while (curci <> -1) do
      begin
        cc := @mCells[curci];
        for f := 0 to High(TGridCell.bodies) do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          ptag := px.mTag;
          if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
          begin
            px.mQueryMark := lq; // mark as processed
            if cb(px.mObj, ptag) then begin result := px.mObj; exit; end;
          end;
        end;
        // next cell
        curci := cc.next;
      end;
      ccidx := -1; // don't process this anymore
      // convert coords to grid
      Dec(x, minx);
      Dec(y, miny);
    end;
  end;
end;


end.
