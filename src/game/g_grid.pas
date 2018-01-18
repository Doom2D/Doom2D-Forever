(* Copyright (C)  Doom 2D: Forever Developers
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
{.$DEFINE GRID_USE_ORTHO_ACCEL}
{$DEFINE LINEAABB2}
unit g_grid;

interface

uses
  mempool;

(*
 * In order to make this usable for kind-of-recursive calls,
 * we'll use "frame memory pool" to return results. That is,
 * we will allocate a memory pool that will be cleared on
 * frame start, and then used as a simple "no-free" allocator.
 * Grid will put results into this pool, and will never bother
 * to free it. Caller should call "release" on result, and
 * the pool will throw away everything.
 * No more callbacks, of course.
 *)

const
  GridTileSize = 32; // must be power of two!

type
  TBodyProxyId = Integer;

  generic TBodyGridBase<ITP> = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  public
    type PITP = ^ITP;

    type TGridQueryCB = function (obj: ITP; tag: Integer): Boolean is nested; // return `true` to stop
    type TGridRayQueryCB = function (obj: ITP; tag: Integer; x, y, prevx, prevy: Integer): Boolean is nested; // return `true` to stop
    type TCellQueryCB = procedure (x, y: Integer) is nested; // top-left cell corner coords

    const TagDisabled = $40000000;
    const TagFullMask = $3fffffff;

  private
    const
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

        function getX1 (): Integer; inline;
        function getY1 (): Integer; inline;

      public
        property x: Integer read mX;
        property y: Integer read mY;
        property width: Integer read mWidth;
        property height: Integer read mHeight;
        property tag: Integer read getTag write setTag;
        property enabled: Boolean read getEnabled write setEnabled;
        property obj: ITP read mObj;

        property x0: Integer read mX;
        property y0: Integer read mY;
        property x1: Integer read getX1;
        property y1: Integer read getY1;
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
    const mTileSize = GridTileSize;
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
    //mInQuery: Boolean;

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
    //function forEachInAABB (x, y, w, h: Integer; cb: TGridQueryCB; tagmask: Integer=-1; allowDisabled: Boolean=false): ITP;
    // return number of ITP thingys put into frame pool
    function forEachInAABB (x, y, w, h: Integer; tagmask: Integer=-1; allowDisabled: Boolean=false; firstHit: Boolean=false): Integer;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // no callback: return object on the first hit or nil
    //function forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1{; exittag: PInteger=nil}): ITP;
    function forEachAtPoint (x, y: Integer; tagmask: Integer=-1; allowDisabled: Boolean=false; firstHit: Boolean=false): Integer;

    function atCellInPoint (x, y: Integer): TAtPointEnumerator;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // cb with `(nil)` will be called before processing new tile
    // no callback: return object of the nearest hit or nil
    // if `inverted` is true, trace will register bodies *exluding* tagmask
    //WARNING: don't change tags in callbacks here!
    {
    function traceRayOld (const x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP; overload;
    function traceRayOld (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
    }

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // cb with `(nil)` will be called before processing new tile
    // no callback: return object of the nearest hit or nil
    // if `inverted` is true, trace will register bodies *exluding* tagmask
    // `cb` is used unconvetionally here: if it returns `false`, tracer will ignore the object
    //WARNING: don't change tags in callbacks here!
    function traceRay (const x0, y0, x1, y1: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP; overload;
    function traceRay (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;

    // return `false` if we're still inside at the end
    // line should be either strict horizontal, or strict vertical, otherwise an exception will be thrown
    // `true`: endpoint will point at the last "inside" pixel
    // `false`: endpoint will be (ax1, ay1)
    //WARNING: don't change tags in callbacks here!
    function traceOrthoRayWhileIn (out ex, ey: Integer; ax0, ay0, ax1, ay1: Integer; tagmask: Integer=-1): Boolean;

    //WARNING: don't modify grid while any query is in progress (no checks are made!)
    //         you can set enabled/disabled flag, tho (but iterator can still return objects disabled inside it)
    // trace line along the grid, calling `cb` for all objects in passed cells, in no particular order
    //WARNING: don't change tags in callbacks here!
    function forEachAlongLine (ax0, ay0, ax1, ay1: Integer; cb: TGridQueryCB; tagmask: Integer=-1; log: Boolean=false): ITP;

    // trace box with the given velocity; return object hit (if any)
    // `cb` is used unconvetionally here: if it returns `false`, tracer will ignore the object
    //WARNING: don't change tags in callbacks here!
    function traceBox (out ex, ey: Integer; const ax0, ay0, aw, ah: Integer; const dx, dy: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;

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


type
  // common structure for all line tracers
  TLineWalker = record
  public
    const TileSize = GridTileSize;

  private
    wx0, wy0, wx1, wy1: Integer; // window coordinates
    stx, sty: Integer; // "steps" for x and y axes
    stleft: Integer; // "steps left"
    err, errinc, errmax: Integer;
    xd, yd: Integer; // current coord
    horiz: Boolean;

  public
    // call `setyp` after this
    constructor Create (minx, miny, maxx, maxy: Integer);

    procedure setClip (minx, miny, maxx, maxy: Integer); inline;

    // this will use `w[xy][01]` to clip coords
    // return `false` if the whole line was clipped away
    // on `true`, you should process first point, and go on
    function setup (x0, y0, x1, y1: Integer): Boolean;

    // call this *after* doing a step
    // WARNING! if you will do a step when this returns `true`, you will fall into limbo
    function done (): Boolean; inline;

    // as you will prolly call `done()` after doing a step anyway, this will do it for you
    // move to next point, return `true` when the line is complete (i.e. you should stop)
    function step (): Boolean; inline;

    // move to next tile; return `true` if the line is complete (and walker state is undefined then)
    function stepToNextTile (): Boolean; inline;

    procedure getXY (out ox, oy: Integer); inline;

  public
    // current coords
    property x: Integer read xd;
    property y: Integer read yd;
  end;


procedure swapInt (var a: Integer; var b: Integer); inline;
//function minInt (a, b: Integer): Integer; inline;
//function maxInt (a, b: Integer): Integer; inline;


implementation

uses
  SysUtils, e_log, g_console, geom, utils;


// ////////////////////////////////////////////////////////////////////////// //
procedure swapInt (var a: Integer; var b: Integer); inline; var t: Integer; begin t := a; a := b; b := t; end;
//procedure swapInt (var a: Integer; var b: Integer); inline; begin a := a xor b; b := b xor a; a := a xor b; end;
//function minInt (a, b: Integer): Integer; inline; begin if (a < b) then result := a else result := b; end;
//function maxInt (a, b: Integer): Integer; inline; begin if (a > b) then result := a else result := b; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TLineWalker.Create (minx, miny, maxx, maxy: Integer);
begin
  setClip(minx, miny, maxx, maxy);
end;

procedure TLineWalker.setClip (minx, miny, maxx, maxy: Integer); inline;
begin
  // clip rectange
  wx0 := minx;
  wy0 := miny;
  wx1 := maxx;
  wy1 := maxy;
end;

function TLineWalker.setup (x0, y0, x1, y1: Integer): Boolean;
var
  sx0, sy0, sx1, sy1: Single;
begin
  if (wx1 < wx0) or (wy1 < wy0) then begin stleft := 0; xd := x0; yd := y0; result := false; exit; end;

  if (x0 >= wx0) and (y0 >= wy0) and (x0 <= wx1) and (y0 <= wy1) and
     (x1 >= wx0) and (y1 >= wy0) and (x1 <= wx1) and (y1 <= wy1) then
  begin
    result := true;
  end
  else
  begin
    sx0 := x0; sy0 := y0;
    sx1 := x1; sy1 := y1;
    result := clipLine(sx0, sy0, sx1, sy1, wx0, wy0, wx1, wy1);
    if not result then begin stleft := 0; xd := x0; yd := y0; exit; end;
    x0 := trunc(sx0); y0 := trunc(sy0);
    x1 := trunc(sx1); y1 := trunc(sy1);
  end;

  // check for ortho lines
  if (y0 = y1) then
  begin
    // horizontal
    horiz := true;
    stleft := abs(x1-x0)+1;
    if (x0 < x1) then stx := 1 else stx := -1;
    sty := 0;
    errinc := 0;
    errmax := 10; // anything that is greater than zero
  end
  else if (x0 = x1) then
  begin
    // vertical
    horiz := false;
    stleft := abs(y1-y0)+1;
    stx := 0;
    if (y0 < y1) then sty := 1 else sty := -1;
    errinc := 0;
    errmax := 10; // anything that is greater than zero
  end
  else
  begin
    // diagonal
    if (abs(x1-x0) >= abs(y1-y0)) then
    begin
      // horizontal
      horiz := true;
      stleft := abs(x1-x0)+1;
      errinc := abs(y1-y0)+1;
    end
    else
    begin
      // vertical
      horiz := false;
      stleft := abs(y1-y0)+1;
      errinc := abs(x1-x0)+1;
    end;
    if (x0 < x1) then stx := 1 else stx := -1;
    if (y0 < y1) then sty := 1 else sty := -1;
    errmax := stleft;
  end;
  xd := x0;
  yd := y0;
  err := -errmax;
end;

function TLineWalker.done (): Boolean; inline; begin result := (stleft <= 0); end;

// true: done
function TLineWalker.step (): Boolean; inline;
begin
  if horiz then
  begin
    xd += stx;
    err += errinc;
    if (err >= 0) then begin err -= errmax; yd += sty; end;
  end
  else
  begin
    yd += sty;
    err += errinc;
    if (err >= 0) then begin err -= errmax; xd += stx; end;
  end;
  Dec(stleft);
  result := (stleft <= 0);
end;

// true: done
function TLineWalker.stepToNextTile (): Boolean; inline;
var
  ex, ey: Integer;
  xwalk, ywalk, wklen: Integer; // to the respective edges
  f: Integer;
begin
  result := false;

  if (stleft < 2) then begin result := true; exit; end; // max one pixel left, nothing to do

  // strictly horizontal?
  if (sty = 0) then
  begin
    // only xd
    if (stx < 0) then
    begin
      // xd: to left edge
      ex := (xd and (not (TileSize-1)))-1;
      stleft -= xd-ex;
    end
    else
    begin
      // xd: to right edge
      ex := (xd or (TileSize-1))+1;
      stleft -= ex-xd;
    end;
    result := (stleft <= 0);
    xd := ex;
    exit;
  end;

  // strictly vertical?
  if (stx = 0) then
  begin
    // only xd
    if (sty < 0) then
    begin
      // yd: to top edge
      ey := (yd and (not (TileSize-1)))-1;
      stleft -= yd-ey;
    end
    else
    begin
      // yd: to bottom edge
      ey := (yd or (TileSize-1))+1;
      stleft -= ey-yd;
    end;
    result := (stleft <= 0);
    yd := ey;
    exit;
  end;

  // diagonal

  // calculate xwalk
  if (stx < 0) then
  begin
    ex := (xd and (not (TileSize-1)))-1;
    xwalk := xd-ex;
  end
  else
  begin
    ex := (xd or (TileSize-1))+1;
    xwalk := ex-xd;
  end;

  // calculate ywalk
  if (sty < 0) then
  begin
    ey := (yd and (not (TileSize-1)))-1;
    ywalk := yd-ey;
  end
  else
  begin
    ey := (yd or (TileSize-1))+1;
    ywalk := ey-yd;
  end;

  {
  while (xd <> ex) and (yd <> ey) do
  begin
    if horiz then
    begin
      xd += stx;
      err += errinc;
      if (err >= 0) then begin err -= errmax; yd += sty; end;
    end
    else
    begin
      yd += sty;
      err += errinc;
      if (err >= 0) then begin err -= errmax; xd += stx; end;
    end;
    Dec(stleft);
    if (stleft < 1) then begin result := true; exit; end;
  end;
  }

  if (xwalk <= ywalk) then wklen := xwalk else wklen := ywalk;
  while true do
  begin
    // in which dir we want to walk?
    stleft -= wklen;
    if (stleft <= 0) then begin result := true; exit; end;
    if horiz then
    begin
      xd += wklen*stx;
      for f := 1 to wklen do
      begin
        err += errinc;
        if (err >= 0) then begin err -= errmax; yd += sty; end;
      end;
    end
    else
    begin
      yd += wklen*sty;
      for f := 1 to wklen do
      begin
        err += errinc;
        if (err >= 0) then begin err -= errmax; xd += stx; end;
      end;
    end;
    // check for walk completion
    if (xd = ex) or (yd = ey) then exit;
    wklen := 1;
  end;
end;

procedure TLineWalker.getXY (out ox, oy: Integer); inline; begin ox := xd; oy := yd; end;


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

function TBodyGridBase.TBodyProxyRec.getX1 (): Integer; inline;
begin
  result := mX+mWidth-1;
end;

function TBodyGridBase.TBodyProxyRec.getY1 (): Integer; inline;
begin
  result := mY+mHeight-1;
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
  e_WriteLog(Format('created grid with size: %dx%d (tile size: %d); pix: %dx%d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize]), TMsgType.Notify);
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
  idx, mcb, ccidx, cnt: Integer;
begin
  mcb := 0;
  for idx := 0 to High(mGrid) do
  begin
    ccidx := mGrid[idx];
    cnt := 0;
    while ccidx >= 0 do
    begin
      Inc(cnt);
      ccidx := mCells[ccidx].next;
    end;
    if (mcb < cnt) then mcb := cnt;
  end;
  e_WriteLog(Format('grid size: %dx%d (tile size: %d); pix: %dx%d; used cells: %d; max bodies in cell: %d; max proxies allocated: %d; proxies used: %d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize, mUsedCells, mcb, mProxyMaxCount, mProxyCount]), TMsgType.Notify);
end;


procedure TBodyGridBase.forEachBodyCell (body: TBodyProxyId; cb: TCellQueryCB);
var
  g, f, ccidx: Integer;
  cc: PGridCell;
begin
  if (body < 0) or (body > High(mProxies)) or not assigned(cb) then exit;
  for g := 0 to High(mGrid) do
  begin
    ccidx := mGrid[g];
    while (ccidx <> -1) do
    begin
      cc := @mCells[ccidx];
      for f := 0 to GridCellBucketSize-1 do
      begin
        if (cc.bodies[f] = -1) then break;
        if (cc.bodies[f] = body) then cb((g mod mWidth)*mTileSize+mMinX, (g div mWidth)*mTileSize+mMinY);
      end;
      // next cell
      ccidx := cc.next;
    end;
  end;
end;


function TBodyGridBase.forEachInCell (x, y: Integer; cb: TGridQueryCB): ITP;
var
  f, ccidx: Integer;
  cc: PGridCell;
begin
  result := Default(ITP);
  if not assigned(cb) then exit;
  Dec(x, mMinX);
  Dec(y, mMinY);
  if (x < 0) or (y < 0) or (x >= mWidth*mTileSize) or (y > mHeight*mTileSize) then exit;
  ccidx := mGrid[(y div mTileSize)*mWidth+(x div mTileSize)];
  while (ccidx <> -1) do
  begin
    cc := @mCells[ccidx];
    for f := 0 to GridCellBucketSize-1 do
    begin
      if (cc.bodies[f] = -1) then break;
      if cb(mProxies[cc.bodies[f]].mObj, mProxies[cc.bodies[f]].mTag) then begin result := mProxies[cc.bodies[f]].mObj; exit; end;
    end;
    // next cell
    ccidx := cc.next;
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
  if (pid >= 0) and (pid < Length(mProxies)) then result := ((mProxies[pid].mTag and TagDisabled) = 0) else result := false;
end;


procedure TBodyGridBase.setProxyEnabled (pid: TBodyProxyId; val: Boolean); inline;
begin
  if (pid >= 0) and (pid < Length(mProxies)) then
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
  if (idx >= 0) and (idx < Length(mProxies)) then result := @mProxies[idx] else result := nil;
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
var
  gw, gh: Integer;
  ex, ey: Integer;
  gx, gy: Integer;
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
  if (x >= gw*mTileSize) or (y >= gh*mTileSize) then exit;
  ex := (x+w-1) div mTileSize;
  ey := (y+h-1) div mTileSize;
  x := x div mTileSize;
  y := y div mTileSize;
  // clip rect
  if (x < 0) then x := 0 else if (x >= gw) then x := gw-1;
  if (y < 0) then y := 0 else if (y >= gh) then y := gh-1;
  if (ex < 0) then ex := 0 else if (ex >= gw) then ex := gw-1;
  if (ey < 0) then ey := 0 else if (ey >= gh) then ey := gh-1;
  if (x > ex) or (y > ey) then exit; // just in case
  // do the work
  for gy := y to ey do
  begin
    for gx := x to ex do
    begin
      result := cb(gy*gw+gx, bodyId);
      if result then exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.inserter (grida: Integer; bodyId: TBodyProxyId): Boolean;
var
  ccidx: Integer;
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
    ccidx := pc;
    while (ccidx <> -1) do
    begin
      pi := @mCells[ccidx];
      for f := 0 to GridCellBucketSize-1 do
      begin
        if (pi.bodies[f] = -1) then break;
        if (pi.bodies[f] = bodyId) then raise Exception.Create('trying to insert already inserted proxy');
      end;
      ccidx := pi.next;
    end;
    {$ENDIF}
    ccidx := pc;
    while (ccidx <> -1) do
    begin
      pi := @mCells[ccidx];
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
      ccidx := pi.next;
    end;
    // no room in cells, add new cell to list
  end;
  // either no room, or no cell at all
  ccidx := allocCell();
  pi := @mCells[ccidx];
  pi.bodies[0] := bodyId;
  pi.bodies[1] := -1;
  pi.next := pc;
  mGrid[grida] := ccidx;
end;


// assume that we cannot have one object added to bucket twice
function TBodyGridBase.remover (grida: Integer; bodyId: TBodyProxyId): Boolean;
var
  f, c: Integer;
  pidx, ccidx: Integer;
  pc: PGridCell;
begin
  result := false; // never stop
  // find and remove cell
  pidx := -1; // previous cell index
  ccidx := mGrid[grida]; // current cell index
  while (ccidx <> -1) do
  begin
    pc := @mCells[ccidx];
    for f := 0 to GridCellBucketSize-1 do
    begin
      if (pc.bodies[f] = bodyId) then
      begin
        // i found her!
        if (f = 0) and (pc.bodies[1] = -1) then
        begin
          // this cell contains no elements, remove it
          if (pidx = -1) then mGrid[grida] := pc.next else mCells[pidx].next := pc.next;
          freeCell(ccidx);
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
    pidx := ccidx;
    ccidx := pc.next;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.insertBody (aObj: ITP; aX, aY, aWidth, aHeight: Integer; aTag: Integer=-1): TBodyProxyId;
begin
  aTag := aTag and TagFullMask;
  result := allocProxy(aX, aY, aWidth, aHeight, aObj, aTag);
  //insertInternal(result);
  forGridRect(aX, aY, aWidth, aHeight, inserter, result);
end;


procedure TBodyGridBase.removeBody (body: TBodyProxyId);
var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  //removeInternal(body);
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, remover, body);
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
     ((x0+w-1) div mTileSize <> (nx+nw-1) div mTileSize) or
     ((y0+h-1) div mTileSize <> (ny+nh-1) div mTileSize) then
  begin
    //writeln('moveResizeBody: cell occupation changed! old=(', x0, ',', y0, ')-(', x0+w-1, ',', y0+h-1, '); new=(', nx, ',', ny, ')-(', nx+nw-1, ',', ny+nh-1, ')');
    //removeInternal(body);
    forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, remover, body);
    px.mX := nx+mMinX;
    px.mY := ny+mMinY;
    px.mWidth := nw;
    px.mHeight := nh;
    //insertInternal(body);
    forGridRect(px.mX, px.mY, nw, nh, inserter, body);
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
  if ((x0+w-1) div mTileSize <> (x0+nw-1) div mTileSize) or
     ((y0+h-1) div mTileSize <> (y0+nh-1) div mTileSize) then
  begin
    // crossed tile boundary, do heavy work
    //removeInternal(body);
    forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, remover, body);
    px.mWidth := nw;
    px.mHeight := nh;
    //insertInternal(body);
    forGridRect(px.mX, px.mY, nw, nh, inserter, body);
  end
  else
  begin
    // nothing to do with the grid, just fix size
    px.mWidth := nw;
    px.mHeight := nh;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.atCellInPoint (x, y: Integer): TAtPointEnumerator;
var
  ccidx: Integer = -1;
begin
  Dec(x, mMinX);
  Dec(y, mMinY);
  if (x >= 0) and (y >= 0) and (x < mWidth*mTileSize) and (y < mHeight*mTileSize) then ccidx := mGrid[(y div mTileSize)*mWidth+(x div mTileSize)];
  result := TAtPointEnumerator.Create(mCells, ccidx, getProxyById);
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the first hit
function TBodyGridBase.forEachAtPoint (x, y: Integer; tagmask: Integer=-1; allowDisabled: Boolean=false; firstHit: Boolean=false): Integer;
var
  f: Integer;
  idx, curci: Integer;
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
  ptag: Integer;
  presobj: PITP;
begin
  result := 0;
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
    //if (assigned(cb)) then e_WriteLog(Format(' cell #%d', [curci]), MSG_NOTIFY);
    {$ENDIF}
    cc := @mCells[curci];
    for f := 0 to GridCellBucketSize-1 do
    begin
      if (cc.bodies[f] = -1) then break;
      px := @mProxies[cc.bodies[f]];
      {$IF DEFINED(D2F_DEBUG_XXQ)}
      //if (assigned(cb)) then e_WriteLog(Format('  proxy #%d; qm:%u; tag:%08x; tagflag:%d  %u', [cc.bodies[f], px.mQueryMark, px.mTag, (px.mTag and tagmask), LongWord(px.mObj)]), MSG_NOTIFY);
      {$ENDIF}
      if (px.mQueryMark = lq) then continue;
      px.mQueryMark := lq;
      ptag := px.mTag;
      if (not allowDisabled) and ((ptag and TagDisabled) <> 0) then continue;
      if ((ptag and tagmask) = 0) then continue;
      if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
      begin
        presobj := PITP(framePool.alloc(sizeof(ITP)));
        Move(px.mObj, presobj^, sizeof(ITP));
        Inc(result);
        if (firstHit) then begin {mInQuery := false;} exit; end;
      end;
    end;
    curci := cc.next;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the first hit
// return number of ITP thingys put into frame pool
function TBodyGridBase.forEachInAABB (x, y, w, h: Integer; tagmask: Integer=-1; allowDisabled: Boolean=false; firstHit: Boolean=false): Integer;
var
  idx: Integer;
  gx, gy: Integer;
  sx, sy, ex, ey: Integer;
  curci: Integer;
  f: Integer;
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
  gw, gh: Integer;
  x0, y0: Integer;
  ptag: Integer;
  presobj: PITP;
begin
  result := 0;
  if (w < 1) or (h < 1) then exit;

  if (w = 1) and (h = 1) then
  begin
    result := forEachAtPoint(x, y, tagmask, allowDisabled, firstHit);
    exit;
  end;

  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  x0 := x;
  y0 := y;

  // fix coords
  Dec(x, mMinX);
  Dec(y, mMinY);

  gw := mWidth;
  gh := mHeight;

  if (x+w <= 0) or (y+h <= 0) then exit;
  if (x >= gw*mTileSize) or (y >= gh*mTileSize) then exit;

  sx := x div mTileSize;
  sy := y div mTileSize;
  ex := (x+w-1) div mTileSize;
  ey := (y+h-1) div mTileSize;

  // clip rect
  if (sx < 0) then sx := 0 else if (sx >= gw) then sx := gw-1;
  if (sy < 0) then sy := 0 else if (sy >= gh) then sy := gh-1;
  if (ex < 0) then ex := 0 else if (ex >= gw) then ex := gw-1;
  if (ey < 0) then ey := 0 else if (ey >= gh) then ey := gh-1;
  if (sx > ex) or (sy > ey) then exit; // just in case

  // has something to do
  //if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  //mInQuery := true;

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
  for gy := sy to ey do
  begin
    for gx := sx to ex do
    begin
      // process cells
      curci := mGrid[gy*gw+gx];
      while (curci <> -1) do
      begin
        cc := @mCells[curci];
        for f := 0 to GridCellBucketSize-1 do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          // shit! has to do it this way, so i can change tag in callback
          if (px.mQueryMark = lq) then continue;
          px.mQueryMark := lq;
          ptag := px.mTag;
          if (not allowDisabled) and ((ptag and TagDisabled) <> 0) then continue;
          if ((ptag and tagmask) = 0) then continue;
          if (x0 >= px.mX+px.mWidth) or (y0 >= px.mY+px.mHeight) then continue;
          if (x0+w <= px.mX) or (y0+h <= px.mY) then continue;
          presobj := PITP(framePool.alloc(sizeof(ITP)));
          Move(px.mObj, presobj^, sizeof(ITP));
          Inc(result);
          if (firstHit) then begin {mInQuery := false;} exit; end;
          (*
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
          *)
        end;
        curci := cc.next;
      end;
    end;
  end;

  //mInQuery := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.forEachAlongLine (ax0, ay0, ax1, ay1: Integer; cb: TGridQueryCB; tagmask: Integer=-1; log: Boolean=false): ITP;
var
  lw: TLineWalker;
  ccidx: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  lq: LongWord;
  f, ptag: Integer;
  gw, gh, minx, miny: Integer;
  x0, y0: Integer;
  x1, y1: Integer;
  cx, cy: Integer;
  //px0, py0, px1, py1: Integer;
begin
  log := false;
  result := Default(ITP);
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) or not assigned(cb) then exit;

  gw := mWidth;
  gh := mHeight;
  minx := mMinX;
  miny := mMinY;

  // make query coords (0,0)-based
  x0 := ax0-minx;
  y0 := ay0-miny;
  x1 := ax1-minx;
  y1 := ay1-miny;

  lw := TLineWalker.Create(0, 0, gw*mTileSize-1, gh*mTileSize-1);
  if not lw.setup(x0, y0, x1, y1) then exit; // out of screen

  //if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  //mInQuery := true;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for f := 0 to High(mProxies) do mProxies[f].mQueryMark := 0;
  end;
  lq := mLastQuery;

  repeat
    lw.getXY(cx, cy);
    // check tile
    ccidx := mGrid[(cy div mTileSize)*gw+(cx div mTileSize)];
    // process cells
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
          if cb(px.mObj, ptag) then
          begin
            result := px.mObj;
            //mInQuery := false;
            exit;
          end;
        end;
      end;
      // next cell
      ccidx := cc.next;
    end;
    // done processing cells, move to next tile
  until lw.stepToNextTile();

  //mInQuery := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
// trace box with the given velocity; return object hit (if any)
// `cb` is used unconvetionally here: if it returns `false`, tracer will ignore the object
function TBodyGridBase.traceBox (out ex, ey: Integer; const ax0, ay0, aw, ah: Integer; const dx, dy: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;
var
  gx, gy: Integer;
  ccidx: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  lq: LongWord;
  f, ptag: Integer;
  minu0: Single = 100000.0;
  u0: Single;
  cx0, cy0, cx1, cy1: Integer;
  hitpx: PBodyProxyRec = nil;
begin
  result := Default(ITP);
  ex := ax0+dx;
  ey := ay0+dy;
  if (aw < 1) or (ah < 1) then exit;

  cx0 := nmin(ax0, ax0+dx);
  cy0 := nmin(ay0, ay0+dy);
  cx1 := nmax(ax0+aw-1, ax0+aw-1+dx);
  cy1 := nmax(ay0+ah-1, ay0+ah-1+dy);

  cx0 -= mMinX; cy0 -= mMinY;
  cx1 -= mMinX; cy1 -= mMinY;

  if (cx1 < 0) or (cy1 < 0) or (cx0 >= mWidth*mTileSize) or (cy0 >= mHeight*mTileSize) then exit;

  if (cx0 < 0) then cx0 := 0;
  if (cy0 < 0) then cy0 := 0;
  if (cx1 >= mWidth*mTileSize) then cx1 := mWidth*mTileSize-1;
  if (cy1 >= mHeight*mTileSize) then cy1 := mHeight*mTileSize-1;
  // just in case
  if (cx0 > cx1) or (cy0 > cy1) then exit;

  //if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  //mInQuery := true;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for f := 0 to High(mProxies) do mProxies[f].mQueryMark := 0;
  end;
  lq := mLastQuery;

  for gy := cy0 div mTileSize to cy1 div mTileSize do
  begin
    for gx := cx0 div mTileSize to cx1 div mTileSize do
    begin
      ccidx := mGrid[gy*mWidth+gx];
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
              if not cb(px.mObj, ptag) then continue;
            end;
            if not sweepAABB(ax0, ay0, aw, ah, dx, dy, px.mX, px.mY, px.mWidth, px.mHeight, @u0) then continue;
            if (minu0 > u0) then
            begin
              hitpx := px;
              result := px.mObj;
              minu0 := u0;
              if (u0 = 0.0) then
              begin
                ex := ax0;
                ey := ay0;
                //mInQuery := false;
                exit;
              end;
            end;
          end;
        end;
        // next cell
        ccidx := cc.next;
      end;
    end;
  end;

  if (minu0 <= 1.0) then
  begin
    ex := ax0+round(dx*minu0);
    ey := ay0+round(dy*minu0);
    // just in case, compensate for floating point inexactness
    if (ex >= hitpx.mX) and (ey >= hitpx.mY) and (ex < hitpx.mX+hitpx.mWidth) and (ey < hitpx.mY+hitpx.mHeight) then
    begin
      ex := ax0+trunc(dx*minu0);
      ey := ay0+trunc(dy*minu0);
    end;
  end;

  //mInQuery := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
{.$DEFINE D2F_DEBUG_OTR}
function TBodyGridBase.traceOrthoRayWhileIn (out ex, ey: Integer; ax0, ay0, ax1, ay1: Integer; tagmask: Integer=-1): Boolean;
var
  ccidx: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  ptag: Integer;
  minx, miny: Integer;
  f, c0, c1: Integer;
  x0, y0, x1, y1: Integer;
  celly0, celly1: Integer;
  dy: Integer;
  filled: array[0..mTileSize-1] of Byte;
  {$IF DEFINED(D2F_DEBUG_OTR)}
  s: AnsiString = '';
  {$ENDIF}
  pmark: PoolMark;
begin
  result := false;
  ex := ax1;
  ey := ay1;
  if not ((ax0 = ax1) or (ay0 = ay1)) then raise Exception.Create('orthoray is not orthogonal');

  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  pmark := framePool.mark();
  if (forEachAtPoint(ax0, ay0, tagmask, false, true) = 0) then exit;
  framePool.release(pmark);

  minx := mMinX;
  miny := mMinY;

  // offset query coords to (0,0)-based
  x0 := ax0-minx;
  y0 := ay0-miny;
  x1 := ax1-minx;
  y1 := ay1-miny;

  if (x0 = x1) then
  begin
    if (x0 < 0) or (x0 >= mWidth*mTileSize) then exit; // oops
    // vertical
    if (y0 < y1) then
    begin
      // down
      if (y1 < 0) or (y0 >= mHeight*mTileSize) then exit;
      //if (ay0 < 0) then ay0 := 0;
      if (y0 < 0) then exit;
      if (y1 >= mHeight*mTileSize) then y1 := mHeight*mTileSize-1;
      dy := 1;
    end
    else
    begin
      // up
      if (y0 < 0) or (y1 >= mHeight*mTileSize) then exit;
      //if (ay1 < 0) then ay1 := 0;
      if (y1 < 0) then exit;
      if (y0 >= mHeight*mTileSize) then y0 := mHeight*mTileSize-1;
      dy := -1;
    end;
    // check tile
    while true do
    begin
      ccidx := mGrid[(y0 div mTileSize)*mWidth+(x0 div mTileSize)];
      FillChar(filled, sizeof(filled), 0);
      celly0 := y0 and (not (mTileSize-1));
      celly1 := celly0+mTileSize-1;
      while (ccidx <> -1) do
      begin
        cc := @mCells[ccidx];
        for f := 0 to GridCellBucketSize-1 do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          ptag := px.mTag;
          if ((ptag and TagDisabled) = 0) and ((ptag and tagmask) <> 0) and
             (ax0 >= px.x0) and (ax0 <= px.x1) then
          begin
            // bound c0 and c1 to cell
            c0 := nclamp(px.y0-miny, celly0, celly1);
            c1 := nclamp(px.y1-miny, celly0, celly1);
            // fill the thing
            {$IF DEFINED(D2F_DEBUG_OTR)}
            e_LogWritefln('**px.y0=%s; px.y1=%s; c0=%s; c1=%s; celly0=%s; celly1=%s; [%s..%s]', [px.y0-miny, px.y1-miny, c0, c1, celly0, celly1, c0-celly0, (c0-celly0)+(c1-c0)]);
            {$ENDIF}
            //assert(c0 <= c1);
            FillChar(filled[c0-celly0], c1-c0+1, 1);
          end;
        end;
        // next cell
        ccidx := cc.next;
      end;
      {$IF DEFINED(D2F_DEBUG_OTR)}
      s := formatstrf('  x=%s; ay0=%s; ay1=%s; y0=%s; celly0=%s; celly1=%s; dy=%s; [', [ax0, ay0, ay1, y0, celly0, celly1, dy]);
      for f := 0 to High(filled) do if (filled[f] <> 0) then s += '1' else s += '0';
      s += ']';
      e_LogWriteln(s);
      {$ENDIF}
      // now go till we hit cell boundary or empty space
      if (dy < 0) then
      begin
        // up
        while (y0 >= celly0) and (filled[y0-celly0] <> 0) do
        begin
          {$IF DEFINED(D2F_DEBUG_OTR)}
          e_LogWritefln('   filled: cdy=%s; y0=%s; celly0=%s; ay0=%s; ay1=%s', [y0-celly0, y0, celly0, ay0, ay1]);
          {$ENDIF}
          Dec(y0);
          Dec(ay0);
        end;
        {$IF DEFINED(D2F_DEBUG_OTR)}
        e_LogWritefln('   span done: cdy=%s; y0=%s; celly0=%s; ay0=%s; ay1=%s', [y0-celly0, y0, celly0, ay0, ay1]);
        {$ENDIF}
        if (ay0 <= ay1) then begin ey := ay1; result := false; exit; end;
        if (y0 >= celly0) then begin ey := ay0+1; {assert(forEachAtPoint(ex, ey, nil, tagmask) <> nil);} result := true; exit; end;
      end
      else
      begin
        // down
        while (y0 <= celly1) and (filled[y0-celly0] <> 0) do begin Inc(y0); Inc(ay0); end;
        if (ay0 >= ay1) then begin ey := ay1; result := false; exit; end;
        if (y0 <= celly1) then begin ey := ay0-1; result := true; exit; end;
      end;
    end;
  end
  else
  begin
    // horizontal
    assert(false);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.traceRay (const x0, y0, x1, y1: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;
var
  ex, ey: Integer;
begin
  result := traceRay(ex, ey, x0, y0, x1, y1, cb, tagmask);
end;


// no callback: return `true` on the nearest hit
// you are not supposed to understand this
function TBodyGridBase.traceRay (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; cb: TGridQueryCB; tagmask: Integer=-1): ITP;
var
  lw: TLineWalker;
  ccidx: Integer;
  cc: PGridCell;
  px: PBodyProxyRec;
  lq: LongWord;
  f, ptag: Integer;
  gw, gh, minx, miny: Integer;
  x0, y0: Integer;
  x1, y1: Integer;
  cx, cy: Integer;
  px0, py0, px1, py1: Integer;
  lastDistSq, distSq, hx, hy: Integer;
  firstCell: Boolean = true;
  wasHit: Boolean;
begin
  result := Default(ITP);
  tagmask := tagmask and TagFullMask;
  if (tagmask = 0) then exit;

  gw := mWidth;
  gh := mHeight;
  minx := mMinX;
  miny := mMinY;

  // make query coords (0,0)-based
  x0 := ax0-minx;
  y0 := ay0-miny;
  x1 := ax1-minx;
  y1 := ay1-miny;

  lw := TLineWalker.Create(0, 0, gw*mTileSize-1, gh*mTileSize-1);
  if not lw.setup(x0, y0, x1, y1) then exit; // out of screen

  lastDistSq := distanceSq(ax0, ay0, ax1, ay1)+1;

  {$IF DEFINED(D2F_DEBUG)}
  //if assigned(dbgRayTraceTileHitCB) then e_LogWritefln('*** traceRay: (%s,%s)-(%s,%s)', [x0, y0, x1, y1]);
  {$ENDIF}

  //if mInQuery then raise Exception.Create('recursive queries aren''t supported');
  //mInQuery := true;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for f := 0 to High(mProxies) do mProxies[f].mQueryMark := 0;
  end;
  lq := mLastQuery;

  repeat
    lw.getXY(cx, cy);
    {$IF DEFINED(D2F_DEBUG)}
    if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB(cx+mMinX, cy+mMinY);
    {$ENDIF}
    // check tile
    ccidx := mGrid[(cy div mTileSize)*gw+(cx div mTileSize)];
    // process cells
    wasHit := false;
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
            if not cb(px.mObj, ptag) then continue;
          end;
          // get adjusted proxy coords
          px0 := px.mX-minx;
          py0 := px.mY-miny;
          px1 := px0+px.mWidth-1;
          py1 := py0+px.mHeight-1;
          {$IF DEFINED(D2F_DEBUG)}
          //if assigned(dbgRayTraceTileHitCB) then e_LogWritefln(' cxy=(%s,%s); pan=(%s,%s)-(%s,%s)', [cx, cy, px0, py0, px1, py1]);
          {$ENDIF}
          // inside?
          if firstCell and (x0 >= px0) and (y0 >= py0) and (x0 <= px1) and (y0 <= py1) then
          begin
            // oops
            ex := ax0;
            ey := ay0;
            result := px.mObj;
            //mInQuery := false;
            {$IF DEFINED(D2F_DEBUG)}
            if assigned(dbgRayTraceTileHitCB) then e_LogWriteln('  INSIDE!');
            {$ENDIF}
            exit;
          end;
          // do line-vs-aabb test
          if lineAABBIntersects(x0, y0, x1, y1, px0, py0, px1-px0+1, py1-py0+1, hx, hy) then
          begin
            // hit detected
            distSq := distanceSq(x0, y0, hx, hy);
            {$IF DEFINED(D2F_DEBUG)}
            //if assigned(dbgRayTraceTileHitCB) then e_LogWritefln('  hit=(%s,%s); distSq=%s; lastDistSq=%s', [hx, hy, distSq, lastDistSq]);
            {$ENDIF}
            if (distSq < lastDistSq) then
            begin
              lastDistSq := distSq;
              ex := hx+minx;
              ey := hy+miny;
              result := px.mObj;
              wasHit := true;
            end;
          end;
        end;
      end;
      // next cell
      ccidx := cc.next;
    end;
    // done processing cells; exit if we registered a hit
    // next cells can't have better candidates, obviously
    if wasHit then begin {mInQuery := false;} exit; end;
    firstCell := false;
    // move to next tile
  until lw.stepToNextTile();

  //mInQuery := false;
end;


// ////////////////////////////////////////////////////////////////////////// //
// no callback: return `true` on the nearest hit
(*
function TBodyGridBase.traceRayOld (const x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
var
  ex, ey: Integer;
begin
  result := traceRayOld(ex, ey, x0, y0, x1, y1, cb, tagmask);
end;


// no callback: return `true` on the nearest hit
// you are not supposed to understand this
function TBodyGridBase.traceRayOld (out ex, ey: Integer; const ax0, ay0, ax1, ay1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): ITP;
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
  maxx := gw*mTileSize-1;
  maxy := gh*mTileSize-1;

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
      //if (rem > 0) then begin Inc(xd); e += dy2; end; //BUGGY
      if (xd < wx0) then begin xd += 1; e += dy2; end; //???
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
  ( *
  // move coords
  if (e >= 0) then begin yd += sty; e -= dx2; end else e += dy2;
  xd += stx;
  // done?
  if (xd = term) then exit;
  * )

  {$IF DEFINED(D2F_DEBUG)}
  if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*mTileSize) and (yptr^ >= gh*mTileSize) then raise Exception.Create('raycaster internal error (0)');
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
    ga := (yptr^ div mTileSize)*gw+(xptr^ div mTileSize);
    // one of those will never change
    x := xptr^+minx;
    y := yptr^+miny;
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
               ((hopt and (y >= px.y0) and (y <= px.y1)) or
                ((not hopt) and (x >= px.x0) and (x <= px.x1))) then
            begin
              px.mQueryMark := lq; // mark as processed
              // inside the proxy?
              if (hopt and (x > px.x0) and (x < px.x1)) or
                 ((not hopt) and (y > px.y0) and (y < px.y1)) then
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
                  if (x < px.x1) then continue; // not on the right edge
                  x := px.x1;
                  prevx := x+1;
                end
                else
                begin
                  // going right
                  if (x > px.x0) then continue; // not on the left edge
                  x := px.x0;
                  prevx := x-1;
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
                  if (y < px.y1) then continue; // not on the bottom edge
                  y := px.y1;
                  prevy := x+1;
                end
                else
                begin
                  // going down
                  if (y > px.y0) then continue; // not on the top edge
                  y := px.y0;
                  prevy := y-1;
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
          Inc(ga, mWidth);
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
          Dec(ga, mWidth);
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
  if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB((xptr^ div mTileSize*mTileSize)+minx, (yptr^ div mTileSize*mTileSize)+miny);
  {$ENDIF}

  //e_LogWritefln('*********************', []);
  ccidx := -1;
  //  can omit checks
  while (xd <> term) do
  begin
    // check cell(s)
    {$IF DEFINED(D2F_DEBUG)}
    if (xptr^ < 0) or (yptr^ < 0) or (xptr^ >= gw*mTileSize) and (yptr^ >= gh*mTileSize) then raise Exception.Create('raycaster internal error (0)');
    {$ENDIF}
    // new tile?
    ga := (yptr^ div mTileSize)*gw+(xptr^ div mTileSize);
    {$IF DEFINED(D2F_DEBUG_RAYTRACE)}
    if assigned(dbgRayTraceTileHitCB) then e_WriteLog(Format(' xd=%d; term=%d; gx=%d; gy=%d; ga=%d; lastga=%d', [xd, term, xptr^, yptr^, ga, lastGA]), MSG_NOTIFY);
    {$ENDIF}
    if (ga <> lastGA) then
    begin
      // yes
      {$IF DEFINED(D2F_DEBUG)}
      if assigned(dbgRayTraceTileHitCB) then dbgRayTraceTileHitCB((xptr^ div mTileSize*mTileSize)+minx, (yptr^ div mTileSize*mTileSize)+miny);
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
*)


end.
