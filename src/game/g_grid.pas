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
{$IF DEFINED(D2F_DEBUG)}
  {.$DEFINE D2F_DEBUG_RAYTRACE}
  {.$DEFINE D2F_DEBUG_XXQ}
  {.$DEFINE D2F_DEBUG_MOVER}
{$ENDIF}
{$DEFINE GRID_USE_ORTHO_ACCEL}
unit g_grid;

interface


type
  TBodyProxyId = Integer;

  generic TBodyGridBase<ITP> = class(TObject)
  public
    type TGridQueryCB = function (obj: ITP; tag: Integer): Boolean is nested; // return `true` to stop
    type TGridRayQueryCB = function (obj: ITP; tag: Integer; x, y, prevx, prevy: Integer): Boolean is nested; // return `true` to stop

    type TCellQueryCB = procedure (x, y: Integer) is nested; // top-left cell corner coords

    const TagDisabled = $40000000;
    const TagFullMask = $3fffffff;

  private
    const
      GridDefaultTileSize = 32; // must be power of two!
      GridCellBucketSize = 8; // WARNING! can't be less than 2!

  public
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

        function getTag (): Integer; inline;
        procedure setTag (v: Integer); inline;

        function getEnabled (): Boolean; inline;
        procedure setEnabled (v: Boolean); inline;

      public
        property x: Integer read mX;
        property y: Integer read mY;
        property width: Integer read mWidth;
        property height: Integer read mHeight;
        property tag: Integer read getTag write setTag;
        property enabled: Boolean read getEnabled write setEnabled;
        property obj: ITP read mObj;
      end;

  private
    type
      PGridCell = ^TGridCell;
      TGridCell = record
        bodies: array [0..GridCellBucketSize-1] of Integer; // -1: end of list
        next: Integer; // in this cell; index in mCells
      end;

      TCellArray = array of TGridCell;

      TGridInternalCB = function (grida: Integer; bodyId: TBodyProxyId): Boolean of object; // return `true` to stop

  private
    //mTileSize: Integer;
    const mTileSize = GridDefaultTileSize;
    type TGetProxyFn = function (pxidx: Integer): PBodyProxyRec of object;

  public
    const tileSize = mTileSize;

    type
      TAtPointEnumerator = record
      private
        mCells: TCellArray;
        curidx, curbki: Integer;
        getpx: TGetProxyFn;
      public
        constructor Create (acells: TCellArray; aidx: Integer; agetpx: TGetProxyFn);
        function MoveNext (): Boolean; inline;
        function getCurrent (): PBodyProxyRec; inline;
        property Current: PBodyProxyRec read getCurrent;
      end;

  private
    mMinX, mMinY: Integer; // so grids can start at any origin
    mWidth, mHeight: Integer; // in tiles
    mGrid: array of Integer; // mWidth*mHeight, index in mCells
    mCells: TCellArray; // cell pool
    mFreeCell: Integer; // first free cell index or -1
    mLastQuery: LongWord;
    mUsedCells: Integer;
    mProxies: array of TBodyProxyRec;
    mProxyFree: TBodyProxyId; // free
    mProxyCount: Integer; // currently used
    mProxyMaxCount: Integer;
    mInQuery: Boolean;

  public
    dbgShowTraceLog: Boolean;
    {$IF DEFINED(D2F_DEBUG)}
    dbgRayTraceTileHitCB: TCellQueryCB;
    {$ENDIF}

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

    function getProxyById (idx: TBodyProxyId): PBodyProxyRec; inline;

  public
    constructor Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer{; aTileSize: Integer=GridDefaultTileSize});
    destructor Destroy (); override;

    function insertBody (aObj: ITP; ax, ay, aWidth, aHeight: Integer; aTag: Integer=-1): TBodyProxyId;
    procedure removeBody (body: TBodyProxyId); // WARNING! this WILL destroy proxy!

    procedure moveBody (body: TBodyProxyId; nx, ny: Integer);
    procedure resizeBody (body: TBodyProxyId; nw, nh: Integer);
    procedure moveResizeBody (body: TBodyProxyId; nx, ny, nw, nh: Integer);

    function insideGrid (x, y: Integer): Boolean; inline;

    // `false` if `body` is surely invalid
    function getBodyXY (body: TBodyProxyId; out rx, ry: Integer): Boolean; inline;
    function getBodyWH (body: TBodyProxyId; out rw, rh: Integer): Boolean; inline;
    function getBodyDims (body: TBodyProxyId; out rx, ry, rw, rh: Integer): Boolean; inline;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // no callback: return `true` on the first hit
    function forEachInAABB (x, y, w, h: Integer; cb: TGridQueryCB; tagmask: Integer=-1; allowDisabled: Boolean=false): ITP;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // no callback: return object on the first hit or nil
    function forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1; exittag: PInteger=nil): ITP;

    function atPoint (x, y: Integer): TAtPointEnumerator;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // cb with `(nil)` will be called before processing new tile
    // no callback: return object of the nearest hit or nil
    // if `inverted` is true, trace will register bodies *exluding* tagmask
    //WARNING: don't change tags in callbacks here!
    function traceRay (const x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP; overload;
    function traceRay (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;

    //function traceOrthoRayWhileIn (const x0, y0, x1, y1: Integer; tagmask: Integer=-1): ITP; overload;
    //function traceOrthoRayWhileIn (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; tagmask: Integer=-1): ITP;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // trace line along the grid, calling `cb` for all objects in passed cells, in no particular order
    //WARNING: don't change tags in callbacks here!
    function forEachAlongLine (ax0, ay0, ax1, ay1: Integer; cb: TGridQueryCB; tagmask: Integer=-1; log: Boolean=false): ITP;

    // debug
    procedure forEachBodyCell (body: TBodyProxyId; cb: TCellQueryCB);
    function forEachInCell (x, y: Integer; cb: TGridQueryCB): ITP;
    procedure dumpStats ();

  public
    //WARNING! no sanity checks!
    property proxyEnabled[pid: TBodyProxyId]: Boolean read getProxyEnabled write setProxyEnabled;

    property gridX0: Integer read mMinX;
    property gridY0: Integer read mMinY;
    property gridWidth: Integer read getGridWidthPx; // in pixels
    property gridHeight: Integer read getGridHeightPx; // in pixels

    property proxy[idx: TBodyProxyId]: PBodyProxyRec read getProxyById;
  end;


// you are not supposed to understand this
// returns `true` if there is an intersection, and enter coords
// enter coords will be equal to (x0, y0) if starting point is inside the box
// if result is `false`, `inx` and `iny` are undefined
function lineAABBIntersects (x0, y0, x1, y1: Integer; bx, by, bw, bh: Integer; out inx, iny: Integer): Boolean;

function distanceSq (x0, y0, x1, y1: Integer): Integer; inline;

procedure swapInt (var a: Integer; var b: Integer); inline;
function minInt (a, b: Integer): Integer; inline;
function maxInt (a, b: Integer): Integer; inline;


implementation

uses
  SysUtils, e_log, g_console;


// ////////////////////////////////////////////////////////////////////////// //
procedure swapInt (var a: Integer; var b: Integer); inline; var t: Integer; begin t := a; a := b; b := t; end;
function minInt (a, b: Integer): Integer; inline; begin if (a < b) then result := a else result := b; end;
function maxInt (a, b: Integer): Integer; inline; begin if (a > b) then result := a else result := b; end;

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
  //!term: Integer;
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
  //!term := x1;

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

  (*
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
  *)

  if (sty = -1) then yd := -yd;
  if (stx = -1) then begin xd := -xd; {!term := -term;} end;
  //!dx2 -= dy2;

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


function TBodyGridBase.TBodyProxyRec.getTag (): Integer; inline;
begin
  result := mTag and TagFullMask;
end;

procedure TBodyGridBase.TBodyProxyRec.setTag (v: Integer); inline;
begin
  mTag := (mTag and TagDisabled) or (v and TagFullMask);
end;

function TBodyGridBase.TBodyProxyRec.getEnabled (): Boolean; inline;
begin
  result := ((mTag and TagDisabled) = 0);
end;

procedure TBodyGridBase.TBodyProxyRec.setEnabled (v: Boolean); inline;
begin
  if v then mTag := mTag and (not TagDisabled) else mTag := mTag or TagDisabled;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBodyGridBase.TAtPointEnumerator.Create (acells: TCellArray; aidx: Integer; agetpx: TGetProxyFn);
begin
  mCells := acells;
  curidx := aidx;
  curbki := -1;
  getpx := agetpx;
end;


function TBodyGridBase.TAtPointEnumerator.MoveNext (): Boolean; inline;
begin
  while (curidx <> -1) do
  begin
    while (curbki < GridCellBucketSize) do
    begin
      Inc(curbki);
      if (mCells[curidx].bodies[curbki] = -1) then break;
      result := true;
      exit;
    end;
    curidx := mCells[curidx].next;
    curbki := -1;
  end;
  result := false;
end;


function TBodyGridBase.TAtPointEnumerator.getCurrent (): PBodyProxyRec; inline;
begin
  result := getpx(mCells[curidx].bodies[curbki]);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBodyGridBase.Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer{; aTileSize: Integer=GridDefaultTileSize});
var
  idx: Integer;
begin
  dbgShowTraceLog := false;
  {$IF DEFINED(D2F_DEBUG)}
  dbgRayTraceTileHitCB := nil;
  {$ENDIF}
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
    mCells[idx].bodies[GridCellBucketSize-1] := -1; // "has free room" flag
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


procedure TBodyGridBase.forEachBodyCell (body: TBodyProxyId; cb: TCellQueryCB);
var
  g, f, cidx: Integer;
  cc: PGridCell;
begin
  if (body < 0) or (body > High(mProxies)) or not assigned(cb) then exit;
  for g := 0 to High(mGrid) do
  begin
    cidx := mGrid[g];
    while (cidx <> -1) do
    begin
      cc := @mCells[cidx];
      for f := 0 to GridCellBucketSize-1 do
      begin
        if (cc.bodies[f] = -1) then break;
        if (cc.bodies[f] = body) then cb((g mod mWidth)*mTileSize+mMinX, (g div mWidth)*mTileSize+mMinY);
      end;
      // next cell
      cidx := cc.next;
    end;
  end;
end;


function TBodyGridBase.forEachInCell (x, y: Integer; cb: TGridQueryCB): ITP;
var
  f, cidx: Integer;
  cc: PGridCell;
begin
  result := Default(ITP);
  if not assigned(cb) then exit;
  Dec(x, mMinX);
  Dec(y, mMinY);
  if (x < 0) or (y < 0) or (x >= mWidth*mTileSize) or (y > mHeight*mTileSize) then exit;
  cidx := mGrid[(y div mTileSize)*mWidth+(x div mTileSize)];
  while (cidx <> -1) do
  begin
    cc := @mCells[cidx];
    for f := 0 to GridCellBucketSize-1 do
    begin
      if (cc.bodies[f] = -1) then break;
      if cb(mProxies[cc.bodies[f]].mObj, mProxies[cc.bodies[f]].mTag) then begin result := mProxies[cc.bodies[f]].mObj; exit; end;
    end;
    // next cell
    cidx := cc.next;
  end;
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


function TBodyGridBase.getBodyWH (body: TBodyProxyId; out rw, rh: Integer): Boolean; inline;
begin
  if (body >= 0) and (body < Length(mProxies)) then
  begin
    with mProxies[body] do begin rw := mWidth; rh := mHeight; end;
    result := true;
  end
  else
  begin
    rw := 0;
    rh := 0;
    result := false;
  end;
end;


function TBodyGridBase.getBodyDims (body: TBodyProxyId; out rx, ry, rw, rh: Integer): Boolean; inline;
begin
  if (body >= 0) and (body < Length(mProxies)) then
  begin
    with mProxies[body] do begin rx := mX; ry := mY; rw := mWidth; rh := mHeight; end;
    result := true;
  end
  else
  begin
    rx := 0;
    ry := 0;
    rw := 0;
    rh := 0;
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


function TBodyGridBase.getProxyById (idx: TBodyProxyId): PBodyProxyRec; inline;
begin
  if (idx >= 0) and (idx < High(mProxies)) then result := @mProxies[idx] else result := nil;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.allocCell (): Integer;
var
  idx: Integer;
  pc: PGridCell;
begin
  if (mFreeCell < 0) then
  begin
    // no free cells, want more
    mFreeCell := Length(mCells);
    SetLength(mCells, mFreeCell+32768); // arbitrary number
    for idx := mFreeCell to High(mCells) do
    begin
      mCells[idx].bodies[0] := -1;
      mCells[idx].bodies[GridCellBucketSize-1] := -1; // 'has free room' flag
      mCells[idx].next := idx+1;
    end;
    mCells[High(mCells)].next := -1; // last cell
  end;
  result := mFreeCell;
  pc := @mCells[result];
  mFreeCell := pc.next;
  pc.next := -1;
  Inc(mUsedCells);
  //e_WriteLog(Format('grid: allocated new cell #%d (total: %d)', [result, mUsedCells]), MSG_NOTIFY);
end;


procedure TBodyGridBase.freeCell (idx: Integer);
begin
  if (idx >= 0) and (idx < Length(mCells)) then
  begin
    with mCells[idx] do
    begin
      bodies[0] := -1;
      bodies[GridCellBucketSize-1] := -1; // 'has free room' flag
      next := mFreeCell;
    end;
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
    {$IF DEFINED(D2F_DEBUG)}
    cidx := pc;
    while (cidx <> -1) do
    begin
      pi := @mCells[cidx];
      for f := 0 to GridCellBucketSize-1 do
      begin
        if (pi.bodies[f] = -1) then break;
        if (pi.bodies[f] = bodyId) then raise Exception.Create('trying to insert already inserted proxy');
      end;
      cidx := pi.next;
    end;
    {$ENDIF}
    cidx := pc;
    while (cidx <> -1) do
    begin
      pi := @mCells[cidx];
      // check "has room" flag
      if (pi.bodies[GridCellBucketSize-1] = -1) then
      begin
        // can add here
        for f := 0 to GridCellBucketSize-1 do
        begin
          if (pi.bodies[f] = -1) then
          begin
            pi.bodies[f] := bodyId;
            if (f+1 < GridCellBucketSize) then pi.bodies[f+1] := -1;
            exit;
          end;
        end;
        raise Exception.Create('internal error in grid inserter');
      end;
      // no room, go to next cell in list (if there is any)
      cidx := pi.next;
    end;
    // no room in cells, add new cell to list
  end;
  // either no room, or no cell at all
  cidx := allocCell();
  pi := @mCells[cidx];
  pi.bodies[0] := bodyId;
  pi.bodies[1] := -1;
  pi.next := pc;
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


// assume that we cannot have one object added to bucket twice
function TBodyGridBase.remover (grida: Integer; bodyId: TBodyProxyId): Boolean;
var
  f, c: Integer;
  pidx, cidx: Integer;
  pc: PGridCell;
begin
  result := false; // never stop
  // find and remove cell
  pidx := -1; // previous cell index
  cidx := mGrid[grida]; // current cell index
  while (cidx <> -1) do
  begin
    pc := @mCells[cidx];
    for f := 0 to GridCellBucketSize-1 do
    begin
      if (pc.bodies[f] = bodyId) then
      begin
        // i found her!
        if (f = 0) and (pc.bodies[1] = -1) then
        begin
          // this cell contains no elements, remove it
          if (pidx = -1) then mGrid[grida] := pc.next else mCells[pidx].next := pc.next;
          freeCell(cidx);
          exit;
        end;
        // remove element from bucket
        for c := f to GridCellBucketSize-2 do
        begin
          pc.bodies[c] := pc.bodies[c+1];
          if (pc.bodies[c] = -1) then break;
        end;
        pc.bodies[GridCellBucketSize-1] := -1; // "has free room" flag
        exit;
      end;
    end;
    pidx := cidx;
    cidx := pc.next;
  end;
end;

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
procedure TBodyGridBase.moveResizeBody (body: TBodyProxyId; nx, ny, nw, nh: Integer);
var
  px: PBodyProxyRec;
  x0, y0, w, h: Integer;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  x0 := px.mX;
  y0 := px.mY;
  w := px.mWidth;
  h := px.mHeight;
  {$IF DEFINED(D2F_DEBUG_MOVER)}
  e_WriteLog(Format('proxy #%d: MOVERESIZE: xg=%d;yg=%d;w=%d;h=%d;nx=%d;ny=%d;nw=%d;nh=%d', [body, x0-mMinX, y0-mMinY, w, h, nx-mMinX, ny-mMinY, nw, nh]), MSG_NOTIFY);
  {$ENDIF}
  if (nx = x0) and (ny = y0) and (nw = w) and (nh = h) then exit;
  // map -> grid
  Dec(x0, mMinX);
  Dec(y0, mMinY);
  Dec(nx, mMinX);
  Dec(ny, mMinY);
  // did any corner crossed tile boundary?
  if (x0 div mTileSize <> nx div mTileSize) or
     (y0 div mTileSize <> ny div mTileSize) or
     ((x0+w) div mTileSize <> (nx+nw) div mTileSize) or
     ((y0+h) div mTileSize <> (ny+nh) div mTileSize) then
  begin
    removeInternal(body);
    px.mX := nx+mMinX;
    px.mY := ny+mMinY;
    px.mWidth := nw;
    px.mHeight := nh;
    insertInternal(body);
  end
  else
  begin
    px.mX := nx+mMinX;
    px.mY := ny+mMinY;
    px.mWidth := nw;
    px.mHeight := nh;
  end;
end;

//TODO: optimize for horizontal/vertical moves
procedure TBodyGridBase.moveBody (body: TBodyProxyId; nx, ny: Integer);
var
  px: PBodyProxyRec;
  x0, y0: Integer;
  ogx0, ogx1, ogy0, ogy1: Integer; // old grid rect
  ngx0, ngx1, ngy0, ngy1: Integer; // new grid rect
  gx, gy: Integer;
  gw, gh: Integer;
  pw, ph: Integer;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  // check if tile coords was changed
  px := @mProxies[body];
  x0 := px.mX;
  y0 := px.mY;
  if (nx = x0) and (ny = y0) then exit;
  // map -> grid
  Dec(x0, mMinX);
  Dec(y0, mMinY);
  Dec(nx, mMinX);
  Dec(ny, mMinY);
  // check for heavy work
  pw := px.mWidth;
  ph := px.mHeight;
  ogx0 := x0 div mTileSize;
  ogy0 := y0 div mTileSize;
  ngx0 := nx div mTileSize;
  ngy0 := ny div mTileSize;
  ogx1 := (x0+pw-1) div mTileSize;
  ogy1 := (y0+ph-1) div mTileSize;
  ngx1 := (nx+pw-1) div mTileSize;
  ngy1 := (ny+ph-1) div mTileSize;
  {$IF DEFINED(D2F_DEBUG_MOVER)}
  e_WriteLog(Format('proxy #%d: checkmove: xg=%d;yg=%d;w=%d;h=%d;nx=%d;ny=%d og:(%d,%d)-(%d,%d); ng:(%d,%d)-(%d,%d)', [body, x0, y0, pw, ph, nx, ny, ogx0, ogy0, ogx1, ogy1, ngx0, ngy0, ngx1, ngy1]), MSG_NOTIFY);
  {$ENDIF}
  if (ogx0 <> ngx0) or (ogy0 <> ngy0) or (ogx1 <> ngx1) or (ogy1 <> ngy1) then
  begin
    // crossed tile boundary, do heavy work
    gw := mWidth;
    gh := mHeight;
    // cycle with old rect, remove body where it is necessary
    // optimized for horizontal moves
    {$IF DEFINED(D2F_DEBUG_MOVER)}
    e_WriteLog(Format('proxy #%d: xg=%d;yg=%d;w=%d;h=%d;nx=%d;ny=%d og:(%d,%d)-(%d,%d); ng:(%d,%d)-(%d,%d)', [body, x0, y0, pw, ph, nx, ny, ogx0, ogy0, ogx1, ogy1, ngx0, ngy0, ngx1, ngy1]), MSG_NOTIFY);
    {$ENDIF}
    // remove stale marks
    if not ((ogy0 >= gh) or (ogy1 < 0)) and
       not ((ogx0 >= gw) or (ogx1 < 0)) then
    begin
      if (ogx0 < 0) then ogx0 := 0;
      if (ogy0 < 0) then ogy0 := 0;
      if (ogx1 > gw-1) then ogx1 := gw-1;
      if (ogy1 > gh-1) then ogy1 := gh-1;
      {$IF DEFINED(D2F_DEBUG_MOVER)}
      e_WriteLog(Format(' norm og:(%d,%d)-(%d,%d)', [ogx0, ogy0, ogx1, ogy1]), MSG_NOTIFY);
      {$ENDIF}
      for gx := ogx0 to ogx1 do
      begin
        if (gx < ngx0) or (gx > ngx1) then
        begin
          // this column is completely outside of new rect
          for gy := ogy0 to ogy1 do
          begin
            {$IF DEFINED(D2F_DEBUG_MOVER)}
            e_WriteLog(Format('  remove0:(%d,%d)', [gx, gy]), MSG_NOTIFY);
            {$ENDIF}
            remover(gy*gw+gx, body);
          end;
        end
        else
        begin
          // heavy checks
          for gy := ogy0 to ogy1 do
          begin
            if (gy < ngy0) or (gy > ngy1) then
            begin
              {$IF DEFINED(D2F_DEBUG_MOVER)}
              e_WriteLog(Format('  remove1:(%d,%d)', [gx, gy]), MSG_NOTIFY);
              {$ENDIF}
              remover(gy*gw+gx, body);
            end;
          end;
        end;
      end;
    end;
    // cycle with new rect, add body where it is necessary
    if not ((ngy0 >= gh) or (ngy1 < 0)) and
       not ((ngx0 >= gw) or (ngx1 < 0)) then
    begin
      if (ngx0 < 0) then ngx0 := 0;
      if (ngy0 < 0) then ngy0 := 0;
      if (ngx1 > gw-1) then ngx1 := gw-1;
      if (ngy1 > gh-1) then ngy1 := gh-1;
      {$IF DEFINED(D2F_DEBUG_MOVER)}
      e_WriteLog(Format(' norm ng:(%d,%d)-(%d,%d)', [ngx0, ngy0, ngx1, ngy1]), MSG_NOTIFY);
      {$ENDIF}
      for gx := ngx0 to ngx1 do
      begin
        if (gx < ogx0) or (gx > ogx1) then
        begin
          // this column is completely outside of old rect
          for gy := ngy0 to ngy1 do
          begin
            {$IF DEFINED(D2F_DEBUG_MOVER)}
            e_WriteLog(Format('  insert0:(%d,%d)', [gx, gy]), MSG_NOTIFY);
            {$ENDIF}
            inserter(gy*gw+gx, body);
          end;
        end
        else
        begin
          // heavy checks
          for gy := ngy0 to ngy1 do
          begin
            if (gy < ogy0) or (gy > ogy1) then
            begin
              {$IF DEFINED(D2F_DEBUG_MOVER)}
              e_WriteLog(Format('  insert1:(%d,%d)', [gx, gy]), MSG_NOTIFY);
              {$ENDIF}
              inserter(gy*gw+gx, body);
            end;
          end;
        end;
      end;
    end;
    // done
  end
  else
  begin
    {$IF DEFINED(D2F_DEBUG_MOVER)}
    e_WriteLog(Format('proxy #%d: GRID OK: xg=%d;yg=%d;w=%d;h=%d;nx=%d;ny=%d og:(%d,%d)-(%d,%d); ng:(%d,%d)-(%d,%d)', [body, x0, y0, pw, ph, nx, ny, ogx0, ogy0, ogx1, ogy1, ngx0, ngy0, ngx1, ngy1]), MSG_NOTIFY);
    {$ENDIF}
  end;
  // update coordinates
  px.mX := nx+mMinX;
  px.mY := ny+mMinY;
end;

procedure TBodyGridBase.resizeBody (body: TBodyProxyId; nw, nh: Integer);
var
  px: PBodyProxyRec;
  x0, y0, w, h: Integer;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  // check if tile coords was changed
  px := @mProxies[body];
  x0 := px.mX-mMinX;
  y0 := px.mY-mMinY;
  w := px.mWidth;
  h := px.mHeight;
  {$IF DEFINED(D2F_DEBUG_MOVER)}
  e_WriteLog(Format('proxy #%d: RESIZE: xg=%d;yg=%d;w=%d;h=%d;nw=%d;nh=%d', [body, x0, y0, w, h, nw, nh]), MSG_NOTIFY);
  {$ENDIF}
  if ((x0+w) div mTileSize <> (x0+nw) div mTileSize) or
     ((y0+h) div mTileSize <> (y0+nh) div mTileSize) then
  begin
    // crossed tile boundary, do heavy work
    removeInternal(body);
    px.mWidth := nw;
    px.mHeight := nh;
    insertInternal(body);
  end
  else
  begin
    // nothing to do with the grid, just fix size
    px.mWidth := nw;
    px.mHeight := nh;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.atPoint (x, y: Integer): TAtPointEnumerator;
var
  cidx: Integer = -1;
begin
  Dec(x, mMinX);
  Dec(y, mMinY);
  if (x >= 0) and (y >= 0) and (x < mWidth*mTileSize) and (y < mHeight*mTileSize) then cidx := mGrid[(y div mTileSize)*mWidth+(x div mTileSize)];
  result := TAtPointEnumerator.Create(mCells, cidx, getProxyById);
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the first hit
function TBodyGridBase.forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1; exittag: PInteger=nil): ITP;
var
  f: Integer;
  idx, curci: Integer;
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
  ptag: Integer;
begin
  result := Default(ITP);
  if (exittag <> nil) then exittag^ := 0;
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  {$IF DEFINED(D2F_DEBUG_XXQ)}
  if (assigned(cb)) then e_WriteLog(Format('0: grid pointquery: (%d,%d)', [x, y]), MSG_NOTIFY);
  {$ENDIF}

  // make coords (0,0)-based
  Dec(x, mMinX);
  Dec(y, mMinY);
  if (x < 0) or (y < 0) or (x >= mWidth*mTileSize) or (y >= mHeight*mTileSize) then exit;

  curci := mGrid[(y div mTileSize)*mWidth+(x div mTileSize)];

  {$IF DEFINED(D2F_DEBUG_XXQ)}
  if (assigned(cb)) then e_WriteLog(Format('1: grid pointquery: (%d,%d) (%d,%d) %d', [x, y, (x div mTileSize), (y div mTileSize), curci]), MSG_NOTIFY);
  {$ENDIF}

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

  {$IF DEFINED(D2F_DEBUG_XXQ)}
  if (assigned(cb)) then e_WriteLog(Format('2: grid pointquery: (%d,%d); lq=%u', [x, y, lq]), MSG_NOTIFY);
  {$ENDIF}

  while (curci <> -1) do
  begin
    {$IF DEFINED(D2F_DEBUG_XXQ)}
    if (assigned(cb)) then e_WriteLog(Format(' cell #%d', [curci]), MSG_NOTIFY);
    {$ENDIF}
    cc := @mCells[curci];
    for f := 0 to GridCellBucketSize-1 do
    begin
      if (cc.bodies[f] = -1) then break;
      px := @mProxies[cc.bodies[f]];
      {$IF DEFINED(D2F_DEBUG_XXQ)}
      if (assigned(cb)) then e_WriteLog(Format('  proxy #%d; qm:%u; tag:%08x; tagflag:%d  %u', [cc.bodies[f], px.mQueryMark, px.mTag, (px.mTag and tagmask), LongWord(px.mObj)]), MSG_NOTIFY);
      {$ENDIF}
      // shit. has to do it this way, so i can change tag in callback
      if (px.mQueryMark <> lq) then
      begin
        px.mQueryMark := lq;
        ptag := px.mTag;
        if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and
           (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
        begin
          if assigned(cb) then
          begin
            if cb(px.mObj, ptag) then
            begin
              result := px.mObj;
              if (exittag <> nil) then exittag^ := ptag;
              exit;
            end;
          end
          else
          begin
            result := px.mObj;
            if (exittag <> nil) then exittag^ := ptag;
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

  if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  mInQuery := true;

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
        for f := 0 to GridCellBucketSize-1 do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          // shit. has to do it this way, so i can change tag in callback
          if (px.mQueryMark = lq) then continue;
          px.mQueryMark := lq;
          ptag := px.mTag;
          if (not allowDisabled) and ((ptag and TagDisabled) <> 0) then continue;
          if ((ptag and tagmask) = 0) then continue;
          if (x0 >= px.mX+px.mWidth) or (y0 >= px.mY+px.mHeight) then continue;
          if (x0+w <= px.mX) or (y0+h <= px.mY) then continue;
          if assigned(cb) then
          begin
            if cb(px.mObj, ptag) then begin result := px.mObj; mInQuery := false; exit; end;
          end
          else
          begin
            result := px.mObj;
            mInQuery := false;
            exit;
          end;
        end;
        curci := cc.next;
      end;
    end;
  end;

  mInQuery := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the nearest hit
function TBodyGridBase.traceRay (const x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
var
  ex, ey: Integer;
begin
  result := traceRay(ex, ey, x0, y0, x1, y1, cb, tagmask);
end;


// no callback: return `true` on the nearest hit
// you are not supposed to understand this
function TBodyGridBase.traceRay (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
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
  //swapped: Boolean = false; // true: xd is yd, and vice versa
  // horizontal walker
  {$IFDEF GRID_USE_ORTHO_ACCEL}
  wklen, wkstep: Integer;
  //wksign: Integer;
  hopt: Boolean;
  {$ENDIF}
  // skipper
  xdist, ydist: Integer;
begin
  result := Default(ITP);
  lastObj := Default(ITP);
  tagmask := tagmask and TagFullMask;
  ex := ax1; // why not?
  ey := ay1; // why not?
  if (tagmask = 0) then exit;

  if (ax0 = ax1) and (ay0 = ay1) then
  begin
    result := forEachAtPoint(ax0, ay0, nil, tagmask, @ptag);
    if (result <> nil) then
    begin
      if assigned(cb) and not cb(result, ptag, ax0, ay0, ax0, ay0) then result := Default(ITP);
    end;
    exit;
  end;

  lastDistSq := distanceSq(ax0, ay0, ax1, ay1)+1;

  gw := mWidth;
  gh := mHeight;
  minx := mMinX;
  miny := mMinY;
  maxx := gw*tsize-1;
  maxy := gh*tsize-1;

  {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
  if assigned(dbgRayTraceTileHitCB) then e_WriteLog(Format('TRACING: (%d,%d)-(%d,%d) [(%d,%d)-(%d,%d)]; maxdistsq=%d', [ax0, ay0, ax1, ay1, minx, miny, maxx, maxy, lastDistSq]), MSG_NOTIFY);
  {$ENDIF}

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
    //swapped := true;
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
  // DON'T DO THIS! loop will take care of that
  if (xd = term) then
  begin
    //FIXME!
    result := forEachAtPoint(ax0, ay0, nil, tagmask, @ptag);
    if (result <> nil) then
    begin
      if assigned(cb) then
      begin
        if cb(result, ptag, ax0, ay0, ax0, ay0) then
        begin
          ex := ax0;
          ey := ay0;
        end
        else
        begin
          result := nil;
        end;
      end
      else
      begin
        ex := ax0;
        ey := ay0;
      end;
    end;
    exit;
  end;

  prevx := xptr^+minx;
  prevy := yptr^+miny;
  (*
  // move coords
  if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
  xd += stx;
  // done?
  if (xd = term) then exit;
  *)

  {$IF DEFINED(D2F_DEBUG)}
  if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*tsize) and (yptr^ >= gh*tsize) then raise Exception.Create('raycaster internal error (0)');
  {$ENDIF}
  // DON'T DO THIS! loop will take care of that
  //lastGA := (yptr^ div tsize)*gw+(xptr^ div tsize);
  //ccidx := mGrid[lastGA];

  {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
  //if assigned(dbgRayTraceTileHitCB) then e_WriteLog('1:TRACING!', MSG_NOTIFY);
  {$ENDIF}

  //if (dbgShowTraceLog) then e_WriteLog(Format('raycast start: (%d,%d)-(%d,%d); xptr^=%d; yptr^=%d', [ax0, ay0, ax1, ay1, xptr^, yptr^]), MSG_NOTIFY);

  if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  mInQuery := true;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for f := 0 to High(mProxies) do mProxies[f].mQueryMark := 0;
  end;
  lq := mLastQuery;

  {$IFDEF GRID_USE_ORTHO_ACCEL}
  // if this is strict horizontal/vertical trace, use optimized codepath
  if (ax0 = ax1) or (ay0 = ay1) then
  begin
    // horizontal trace: walk the whole tiles, calculating mindist once for each proxy in cell
    //   stx < 0: going left, otherwise `stx` is > 0, and we're going right
    // vertical trace: walk the whole tiles, calculating mindist once for each proxy in cell
    //   stx < 0: going up, otherwise `stx` is > 0, and we're going down
    hopt := (ay0 = ay1); // horizontal?
    if (stx < 0) then begin {wksign := -1;} wklen := -(term-xd); end else begin {wksign := 1;} wklen := term-xd; end;
    {$IF DEFINED(D2F_DEBUG)}
    if dbgShowTraceLog then e_LogWritefln('optimized htrace; wklen=%d', [wklen]);
    {$ENDIF}
    ga := (yptr^ div tsize)*gw+(xptr^ div tsize);
    // one of those will never change
    x := xptr^+minx;
    y := yptr^+miny;
    //prevx := x;
    //prevy := y;
    {$IF DEFINED(D2F_DEBUG)}
    if hopt then
    begin
      if (y <> ay0) then raise Exception.Create('htrace fatal internal error');
    end
    else
    begin
      if (x <> ax0) then raise Exception.Create('vtrace fatal internal error');
    end;
    {$ENDIF}
    while (wklen > 0) do
    begin
      {$IF DEFINED(D2F_DEBUG)}
      if dbgShowTraceLog then e_LogWritefln('  htrace; ga=%d; x=%d, y=%d; y=%d; y=%d', [ga, xptr^+minx, yptr^+miny, y, ay0]);
      {$ENDIF}
      // new tile?
      if (ga <> lastGA) then
      begin
        lastGA := ga;
        ccidx := mGrid[lastGA];
        // convert coords to map (to avoid ajdusting coords inside the loop)
        if hopt then x := xptr^+minx else y := yptr^+miny;
        while (ccidx <> -1) do
        begin
          cc := @mCells[ccidx];
          for f := 0 to GridCellBucketSize-1 do
          begin
            if (cc.bodies[f] = -1) then break;
            px := @mProxies[cc.bodies[f]];
            ptag := px.mTag;
            if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) and
               // constant coord should be inside
               ((hopt and (y >= px.mY) and (y < px.mY+px.mHeight)) or
                ((not hopt) and (x >= px.mX) and (x < px.mX+px.mWidth))) then
            begin
              px.mQueryMark := lq; // mark as processed
              // inside the proxy?
              if (hopt and (x > px.mX) and (x < px.mX+px.mWidth-1)) or
                 ((not hopt) and (y > px.mY) and (y < px.mY+px.mHeight-1)) then
              begin
                // setup prev[xy]
                if assigned(cb) then
                begin
                  if cb(px.mObj, ptag, x, y, x, y) then
                  begin
                    result := px.mObj;
                    ex := x;
                    ey := y;
                    mInQuery := false;
                    exit;
                  end;
                end
                else
                begin
                  distSq := distanceSq(ax0, ay0, x, y);
                  {$IF DEFINED(D2F_DEBUG)}
                  if dbgShowTraceLog then e_LogWritefln('  EMBEDDED hhit(%d): a=(%d,%d), h=(%d,%d), distsq=%d; lastsq=%d', [cc.bodies[f], ax0, ay0, x, y, distSq, lastDistSq]);
                  {$ENDIF}
                  if (distSq < lastDistSq) then
                  begin
                    ex := x;
                    ey := y;
                    result := px.mObj;
                    mInQuery := false;
                    exit;
                  end;
                end;
                continue;
              end;
              // remember this hitpoint if it is nearer than an old one
              // setup prev[xy]
              if hopt then
              begin
                // horizontal trace
                prevy := y;
                y := yptr^+miny;
                if (stx < 0) then
                begin
                  // going left
                  if (x < px.mX+px.mWidth-1) then continue; // not on the right edge
                  prevx := px.mX+px.mWidth;
                  x := prevx-1;
                end
                else
                begin
                  // going right
                  if (x > px.mX) then continue; // not on the left edge
                  prevx := px.mX-1;
                  x := prevx+1;
                end;
              end
              else
              begin
                // vertical trace
                prevx := x;
                x := xptr^+minx;
                if (stx < 0) then
                begin
                  // going up
                  if (y < px.mY+px.mHeight-1) then continue; // not on the bottom edge
                  prevy := px.mY+px.mHeight;
                  y := prevy-1;
                end
                else
                begin
                  // going down
                  if (y > px.mY) then continue; // not on the top edge
                  prevy := px.mY-1;
                  y := prevy+1;
                end;
              end;
              if assigned(cb) then
              begin
                if cb(px.mObj, ptag, x, y, prevx, prevy) then
                begin
                  result := px.mObj;
                  ex := prevx;
                  ey := prevy;
                  mInQuery := false;
                  exit;
                end;
              end
              else
              begin
                distSq := distanceSq(ax0, ay0, prevx, prevy);
                {$IF DEFINED(D2F_DEBUG)}
                if dbgShowTraceLog then e_LogWritefln('  hhit(%d): a=(%d,%d), h=(%d,%d), p=(%d,%d), distsq=%d; lastsq=%d', [cc.bodies[f], ax0, ay0, x, y, prevx, prevy, distSq, lastDistSq]);
                {$ENDIF}
                if (distSq < lastDistSq) then
                begin
                  wasHit := true;
                  lastDistSq := distSq;
                  ex := prevx;
                  ey := prevy;
                  lastObj := px.mObj;
                end;
              end;
            end;
          end;
          // next cell
          ccidx := cc.next;
        end;
        if wasHit and not assigned(cb) then begin result := lastObj; mInQuery := false; exit; end;
        if assigned(cb) and cb(nil, 0, x, y, x, y) then begin result := lastObj; mInQuery := false; exit; end;
      end;
      // skip to next tile
      if hopt then
      begin
        if (stx > 0) then
        begin
          // to the right
          wkstep := ((xptr^ or (mTileSize-1))+1)-xptr^;
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  right step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Inc(xptr^, wkstep);
          Inc(ga);
        end
        else
        begin
          // to the left
          wkstep := xptr^-((xptr^ and (not (mTileSize-1)))-1);
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  left step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Dec(xptr^, wkstep);
          Dec(ga);
        end;
      end
      else
      begin
        if (stx > 0) then
        begin
          // to the down
          wkstep := ((yptr^ or (mTileSize-1))+1)-yptr^;
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  down step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Inc(yptr^, wkstep);
          Inc(ga, mHeight);
        end
        else
        begin
          // to the up
          wkstep := yptr^-((yptr^ and (not (mTileSize-1)))-1);
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  up step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Dec(yptr^, wkstep);
          Dec(ga, mHeight);
        end;
      end;
      Dec(wklen, wkstep);
    end;
    // we can travel less than one cell
    if wasHit and not assigned(cb) then result := lastObj else begin ex := ax1; ey := ay1; end;
    mInQuery := false;
    exit;
  end;
  {$ENDIF}

  {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
  if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB((xptr^ div tsize*tsize)+minx, (yptr^ div tsize*tsize)+miny);
  {$ENDIF}

  //e_LogWritefln('*********************', []);
  ccidx := -1;
  //  can omit checks
  while (xd <> term) do
  begin
    // check cell(s)
    {$IF DEFINED(D2F_DEBUG)}
    if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*tsize) and (yptr^ >= gh*tsize) then raise Exception.Create('raycaster internal error (0)');
    {$ENDIF}
    // new tile?
    ga := (yptr^ div tsize)*gw+(xptr^ div tsize);
    {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
    if assigned(dbgRayTraceTileHitCB) then e_WriteLog(Format(' xd=%d; term=%d; gx=%d; gy=%d; ga=%d; lastga=%d', [xd, term, xptr^, yptr^, ga, lastGA]), MSG_NOTIFY);
    {$ENDIF}
    if (ga <> lastGA) then
    begin
      // yes
      {$IF DEFINED(D2F_DEBUG)}
      if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB((xptr^ div tsize*tsize)+minx, (yptr^ div tsize*tsize)+miny);
      {$ENDIF}
      if (ccidx <> -1) then
      begin
        // signal cell completion
        if assigned(cb) then
        begin
          if cb(nil, 0, xptr^+minx, yptr^+miny, prevx, prevy) then begin result := lastObj; mInQuery := false; exit; end;
        end
        else if wasHit then
        begin
          result := lastObj;
          mInQuery := false;
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
        for f := 0 to GridCellBucketSize-1 do
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
                  result := px.mObj;
                  ex := prevx;
                  ey := prevy;
                  mInQuery := false;
                  exit;
                end;
              end
              else
              begin
                // remember this hitpoint if it is nearer than an old one
                distSq := distanceSq(ax0, ay0, prevx, prevy);
                {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
                if assigned(dbgRayTraceTileHitCB) then e_WriteLog(Format('  hit(%d): a=(%d,%d), h=(%d,%d), p=(%d,%d); distsq=%d; lastsq=%d', [cc.bodies[f], ax0, ay0, x, y, prevx, prevy, distSq, lastDistSq]), MSG_NOTIFY);
                {$ENDIF}
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
          if cb(nil, 0, x, y, prevx, prevy) then begin result := lastObj; mInQuery := false; exit; end;
        end
        else if wasHit then
        begin
          result := lastObj;
          mInQuery := false;
          exit;
        end;
      end;
    end;
    if (ccidx = -1) then
    begin
      // move to cell edge, as we have nothing to trace here anymore
      if (stx < 0) then xdist := xd and (not (mTileSize-1)) else xdist := xd or (mTileSize-1);
      if (sty < 0) then ydist := yd and (not (mTileSize-1)) else ydist := yd or (mTileSize-1);
      //e_LogWritefln('0: swapped=%d; xd=%d; yd=%d; stx=%d; sty=%d; e=%d; dx2=%d; dy2=%d; term=%d; xdist=%d; ydist=%d', [swapped, xd, yd, stx, sty, e, dx2, dy2, term, xdist, ydist]);
      while (xd <> xdist) and (yd <> ydist) do
      begin
        // step
        xd += stx;
        if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
        //e_LogWritefln('  xd=%d; yd=%d', [xd, yd]);
        if (xd = term) then break;
      end;
      //e_LogWritefln('1: swapped=%d; xd=%d; yd=%d; stx=%d; sty=%d; e=%d; dx2=%d; dy2=%d; term=%d; xdist=%d; ydist=%d', [swapped, xd, yd, stx, sty, e, dx2, dy2, term, xdist, ydist]);
      if (xd = term) then break;
    end;
    //putPixel(xptr^, yptr^);
    // move coords
    prevx := xptr^+minx;
    prevy := yptr^+miny;
    if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
    xd += stx;
  end;
  // we can travel less than one cell
  if wasHit and not assigned(cb) then
  begin
    result := lastObj;
  end
  else
  begin
    ex := ax1; // why not?
    ey := ay1; // why not?
  end;

  mInQuery := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
//FIXME! optimize this with real tile walking
function TBodyGridBase.forEachAlongLine (ax0, ay0, ax1, ay1: Integer; cb: TGridQueryCB; tagmask: Integer=-1; log: Boolean=false): ITP;
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
  ccidx, curci: Integer;
  lastGA: Integer = -1;
  ga, x, y: Integer;
  gw, gh, minx, miny, maxx, maxy: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  lq: LongWord;
  f, ptag: Integer;
  x0, y0, x1, y1: Integer;
  //swapped: Boolean = false; // true: xd is yd, and vice versa
  // horizontal walker
  {$IFDEF GRID_USE_ORTHO_ACCEL}
  wklen, wkstep: Integer;
  //wksign: Integer;
  hopt: Boolean;
  {$ENDIF}
  // skipper
  xdist, ydist: Integer;
begin
  log := false;
  result := Default(ITP);
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) or not assigned(cb) then exit;

  if (ax0 = ax1) and (ay0 = ay1) then
  begin
    result := forEachAtPoint(ax0, ay0, cb, tagmask, @ptag);
    exit;
  end;

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
    //swapped := true;
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
  // DON'T DO THIS! loop will take care of that
  if (xd = term) then
  begin
    result := forEachAtPoint(ax0, ay0, cb, tagmask, @ptag);
    exit;
  end;

  (*
  // move coords
  if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
  xd += stx;
  // done?
  if (xd = term) then exit;
  *)

  {$IF DEFINED(D2F_DEBUG)}
  if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*tsize) and (yptr^ >= gh*tsize) then raise Exception.Create('raycaster internal error (0)');
  {$ENDIF}
  // DON'T DO THIS! loop will take care of that
  //lastGA := (yptr^ div tsize)*gw+(xptr^ div tsize);
  //ccidx := mGrid[lastGA];

  if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  mInQuery := true;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for f := 0 to High(mProxies) do mProxies[f].mQueryMark := 0;
  end;
  lq := mLastQuery;

  {$IFDEF GRID_USE_ORTHO_ACCEL}
  // if this is strict horizontal/vertical trace, use optimized codepath
  if (ax0 = ax1) or (ay0 = ay1) then
  begin
    // horizontal trace: walk the whole tiles, calculating mindist once for each proxy in cell
    //   stx < 0: going left, otherwise `stx` is > 0, and we're going right
    // vertical trace: walk the whole tiles, calculating mindist once for each proxy in cell
    //   stx < 0: going up, otherwise `stx` is > 0, and we're going down
    hopt := (ay0 = ay1); // horizontal?
    if (stx < 0) then begin {wksign := -1;} wklen := -(term-xd); end else begin {wksign := 1;} wklen := term-xd; end;
    {$IF DEFINED(D2F_DEBUG)}
    if dbgShowTraceLog then e_LogWritefln('optimized htrace; wklen=%d', [wklen]);
    {$ENDIF}
    ga := (yptr^ div tsize)*gw+(xptr^ div tsize);
    // one of those will never change
    x := xptr^+minx;
    y := yptr^+miny;
    {$IF DEFINED(D2F_DEBUG)}
    if hopt then
    begin
      if (y <> ay0) then raise Exception.Create('htrace fatal internal error');
    end
    else
    begin
      if (x <> ax0) then raise Exception.Create('vtrace fatal internal error');
    end;
    {$ENDIF}
    while (wklen > 0) do
    begin
      {$IF DEFINED(D2F_DEBUG)}
      if dbgShowTraceLog then e_LogWritefln('  htrace; ga=%d; x=%d, y=%d; y=%d; y=%d', [ga, xptr^+minx, yptr^+miny, y, ay0]);
      {$ENDIF}
      // new tile?
      if (ga <> lastGA) then
      begin
        lastGA := ga;
        ccidx := mGrid[lastGA];
        // convert coords to map (to avoid ajdusting coords inside the loop)
        if hopt then x := xptr^+minx else y := yptr^+miny;
        while (ccidx <> -1) do
        begin
          cc := @mCells[ccidx];
          for f := 0 to GridCellBucketSize-1 do
          begin
            if (cc.bodies[f] = -1) then break;
            px := @mProxies[cc.bodies[f]];
            ptag := px.mTag;
            if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
            begin
              px.mQueryMark := lq; // mark as processed
              if assigned(cb) then
              begin
                if cb(px.mObj, ptag) then begin result := px.mObj; mInQuery := false; exit; end;
              end
              else
              begin
                result := px.mObj;
                mInQuery := false;
                exit;
              end;
            end;
          end;
          // next cell
          ccidx := cc.next;
        end;
      end;
      // skip to next tile
      if hopt then
      begin
        if (stx > 0) then
        begin
          // to the right
          wkstep := ((xptr^ or (mTileSize-1))+1)-xptr^;
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  right step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Inc(xptr^, wkstep);
          Inc(ga);
        end
        else
        begin
          // to the left
          wkstep := xptr^-((xptr^ and (not (mTileSize-1)))-1);
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  left step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Dec(xptr^, wkstep);
          Dec(ga);
        end;
      end
      else
      begin
        if (stx > 0) then
        begin
          // to the down
          wkstep := ((yptr^ or (mTileSize-1))+1)-yptr^;
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  down step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Inc(yptr^, wkstep);
          Inc(ga, mHeight);
        end
        else
        begin
          // to the up
          wkstep := yptr^-((yptr^ and (not (mTileSize-1)))-1);
          {$IF DEFINED(D2F_DEBUG)}
          if dbgShowTraceLog then e_LogWritefln('  up step: wklen=%d; wkstep=%d', [wklen, wkstep]);
          {$ENDIF}
          if (wkstep >= wklen) then break;
          Dec(yptr^, wkstep);
          Dec(ga, mHeight);
        end;
      end;
      Dec(wklen, wkstep);
    end;
    mInQuery := false;
    exit;
  end;
  {$ENDIF}

  {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
  if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB((xptr^ div tsize*tsize)+minx, (yptr^ div tsize*tsize)+miny);
  {$ENDIF}

  ccidx := -1;
  //  can omit checks
  while (xd <> term) do
  begin
    // check cell(s)
    {$IF DEFINED(D2F_DEBUG)}
    if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*tsize) and (yptr^ >= gh*tsize) then raise Exception.Create('raycaster internal error (0)');
    {$ENDIF}
    // new tile?
    ga := (yptr^ div tsize)*gw+(xptr^ div tsize);
    {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
    if assigned(dbgRayTraceTileHitCB) then e_WriteLog(Format(' xd=%d; term=%d; gx=%d; gy=%d; ga=%d; lastga=%d', [xd, term, xptr^, yptr^, ga, lastGA]), MSG_NOTIFY);
    {$ENDIF}
    if (ga <> lastGA) then
    begin
      // yes
      {$IF DEFINED(D2F_DEBUG)}
      if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB((xptr^ div tsize*tsize)+minx, (yptr^ div tsize*tsize)+miny);
      {$ENDIF}
      lastGA := ga;
      ccidx := mGrid[lastGA];
    end;
    // has something to process in this tile?
    if (ccidx <> -1) then
    begin
      // process cell
      curci := ccidx;
      // convert coords to map (to avoid ajdusting coords inside the loop)
      x := xptr^+minx;
      y := yptr^+miny;
      // process cell list
      while (curci <> -1) do
      begin
        cc := @mCells[curci];
        for f := 0 to GridCellBucketSize-1 do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          ptag := px.mTag;
          if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and (px.mQueryMark <> lq) then
          begin
            px.mQueryMark := lq; // mark as processed
            if assigned(cb) then
            begin
              if cb(px.mObj, ptag) then begin result := px.mObj; mInQuery := false; exit; end;
            end
            else
            begin
              result := px.mObj;
              mInQuery := false;
              exit;
            end;
          end;
        end;
        // next cell
        curci := cc.next;
      end;
      // nothing more interesting in this cell
      ccidx := -1;
    end;
    // move to cell edge, as we have nothing to trace here anymore
    if (stx < 0) then xdist := xd and (not (mTileSize-1)) else xdist := xd or (mTileSize-1);
    if (sty < 0) then ydist := yd and (not (mTileSize-1)) else ydist := yd or (mTileSize-1);
    //e_LogWritefln('0: swapped=%d; xd=%d; yd=%d; stx=%d; sty=%d; e=%d; dx2=%d; dy2=%d; term=%d; xdist=%d; ydist=%d', [swapped, xd, yd, stx, sty, e, dx2, dy2, term, xdist, ydist]);
    while (xd <> xdist) and (yd <> ydist) do
    begin
      // step
      xd += stx;
      if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
      //e_LogWritefln('  xd=%d; yd=%d', [xd, yd]);
      if (xd = term) then break;
    end;
    //e_LogWritefln('1: swapped=%d; xd=%d; yd=%d; stx=%d; sty=%d; e=%d; dx2=%d; dy2=%d; term=%d; xdist=%d; ydist=%d', [swapped, xd, yd, stx, sty, e, dx2, dy2, term, xdist, ydist]);
    if (xd = term) then break;
    //putPixel(xptr^, yptr^);
    // move coords
    if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
    xd += stx;
  end;

  mInQuery := false;
end;


end.
