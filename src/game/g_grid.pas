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
{$DEFINE grid_use_buckets}
unit g_grid;

interface


type
  TBodyProxyId = Integer;

  generic TBodyGridBase<ITP> = class(TObject)
  public
    type TGridQueryCB = function (obj: ITP; tag: Integer): Boolean is nested; // return `true` to stop
    type TGridRayQueryCB = function (obj: ITP; tag: Integer; x, y, prevx, prevy: Integer): Boolean is nested; // return `true` to stop

  private
    const
      GridDefaultTileSize = 32;
      {$IFDEF grid_use_buckets}
      GridCellBucketSize = 8; // WARNING! can't be less than 2!
      {$ENDIF}

  private
    type
      PBodyProxyRec = ^TBodyProxyRec;
      TBodyProxyRec = record
      private
        mX, mY, mWidth, mHeight: Integer; // aabb
        mQueryMark: LongWord; // was this object visited at this query?
        mObj: ITP;
        mTag: Integer;
        nextLink: TBodyProxyId; // next free or nothing

      private
        procedure setup (aX, aY, aWidth, aHeight: Integer; aObj: ITP; aTag: Integer);
      end;

      PGridCell = ^TGridCell;
      TGridCell = record
        {$IFDEF grid_use_buckets}
        bodies: array [0..GridCellBucketSize-1] of Integer; // -1: end of list
        {$ELSE}
        body: Integer;
        {$ENDIF}
        next: Integer; // in this cell; index in mCells
      end;

      TGridInternalCB = function (grida: Integer): Boolean of object; // return `true` to stop

  private
    mTileSize: Integer;
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

    mUData: TBodyProxyId; // for inserter/remover
    mTagMask: Integer; // for iterator
    mItCB: TGridQueryCB; // for iterator

  private
    function allocCell: Integer;
    procedure freeCell (idx: Integer); // `next` is simply overwritten

    function allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: ITP; aTag: Integer): TBodyProxyId;
    procedure freeProxy (body: TBodyProxyId);

    procedure insert (body: TBodyProxyId);
    procedure remove (body: TBodyProxyId);

    function forGridRect (x, y, w, h: Integer; cb: TGridInternalCB): Boolean;

    function inserter (grida: Integer): Boolean;
    function remover (grida: Integer): Boolean;

  public
    constructor Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer; aTileSize: Integer=GridDefaultTileSize);
    destructor Destroy (); override;

    function insertBody (aObj: ITP; ax, ay, aWidth, aHeight: Integer; aTag: Integer=0): TBodyProxyId;
    procedure removeBody (aObj: TBodyProxyId); // WARNING! this WILL destroy proxy!

    procedure moveBody (body: TBodyProxyId; dx, dy: Integer);
    procedure resizeBody (body: TBodyProxyId; sx, sy: Integer);
    procedure moveResizeBody (body: TBodyProxyId; dx, dy, sx, sy: Integer);

    //WARNING: can't do recursive queries
    function forEachInAABB (x, y, w, h: Integer; cb: TGridQueryCB; tagmask: Integer=-1): Boolean;

    //WARNING: can't do recursive queries
    function forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1): Boolean;

    //WARNING: can't do recursive queries
    // cb with `(nil)` will be called before processing new tile
    function traceRay (x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): Boolean; overload;

    procedure dumpStats ();
  end;


implementation

uses
  SysUtils, e_log;


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
constructor TBodyGridBase.Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer; aTileSize: Integer=GridDefaultTileSize);
var
  idx: Integer;
begin
  if aTileSize < 1 then aTileSize := 1;
  if aTileSize > 8192 then aTileSize := 8192; // arbitrary limit
  if aPixWidth < aTileSize then aPixWidth := aTileSize;
  if aPixHeight < aTileSize then aPixHeight := aTileSize;
  mTileSize := aTileSize;
  mMinX := aMinPixX;
  mMinY := aMinPixY;
  mWidth := (aPixWidth+aTileSize-1) div aTileSize;
  mHeight := (aPixHeight+aTileSize-1) div aTileSize;
  SetLength(mGrid, mWidth*mHeight);
  SetLength(mCells, mWidth*mHeight);
  SetLength(mProxies, 8192);
  mFreeCell := 0;
  // init free list
  for idx := 0 to High(mCells) do
  begin
    {$IFDEF grid_use_buckets}
    mCells[idx].bodies[0] := -1;
    {$ELSE}
    mCells[idx].body := -1;
    {$ENDIF}
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
  mUData := 0;
  mTagMask := -1;
  mItCB := nil;
  e_WriteLog(Format('created grid with size: %dx%d (tile size: %d); pix: %dx%d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize]), MSG_NOTIFY);
end;


destructor TBodyGridBase.Destroy ();
begin
  mCells := nil;
  mGrid := nil;
  mProxies := nil;
  inherited;
end;


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


function TBodyGridBase.allocCell: Integer;
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
      {$IFDEF grid_use_buckets}
      mCells[idx].bodies[0] := -1;
      {$ELSE}
      mCells[idx].body := -1;
      {$ENDIF}
      mCells[idx].next := idx+1;
    end;
    mCells[High(mCells)].next := -1; // last cell
  end;
  result := mFreeCell;
  mFreeCell := mCells[result].next;
  mCells[result].next := -1;
  {$IFDEF grid_use_buckets}
  mCells[result].bodies[0] := -1;
  {$ELSE}
  mCells[result].body := -1;
  {$ENDIF}
  Inc(mUsedCells);
  //e_WriteLog(Format('grid: allocated new cell #%d (total: %d)', [result, mUsedCells]), MSG_NOTIFY);
end;


procedure TBodyGridBase.freeCell (idx: Integer);
begin
  if (idx >= 0) and (idx < Length(mCells)) then
  begin
    //if mCells[idx].body = -1 then exit; // the thing that should not be
    {$IFDEF grid_use_buckets}
    mCells[idx].bodies[0] := -1;
    {$ELSE}
    mCells[idx].body := -1;
    {$ENDIF}
    mCells[idx].next := mFreeCell;
    mFreeCell := idx;
    Dec(mUsedCells);
  end;
end;


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


function TBodyGridBase.forGridRect (x, y, w, h: Integer; cb: TGridInternalCB): Boolean;
var
  gx, gy: Integer;
  gw, gh, tsize: Integer;
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
  tsize := mTileSize;
  if (x >= gw*tsize) or (y >= gh*tsize) then exit;
  for gy := y div tsize to (y+h-1) div tsize do
  begin
    if (gy < 0) then continue;
    if (gy >= gh) then break;
    for gx := x div tsize to (x+w-1) div tsize do
    begin
      if (gx < 0) then continue;
      if (gx >= gw) then break;
      result := cb(gy*gw+gx);
      if result then exit;
    end;
  end;
end;


function TBodyGridBase.inserter (grida: Integer): Boolean;
var
  cidx: Integer;
  pc: Integer;
  {$IFDEF grid_use_buckets}
  pi: PGridCell;
  f: Integer;
  {$ENDIF}
begin
  result := false; // never stop
  // add body to the given grid cell
  pc := mGrid[grida];
  {$IFDEF grid_use_buckets}
  if (pc <> -1) then
  begin
    pi := @mCells[pc];
    f := 0;
    for f := 0 to High(TGridCell.bodies) do
    begin
      if (pi.bodies[f] = -1) then
      begin
        // can add here
        pi.bodies[f] := mUData;
        if (f+1 < Length(TGridCell.bodies)) then pi.bodies[f+1] := -1;
        exit;
      end;
    end;
  end;
  // either no room, or no cell at all
  cidx := allocCell();
  mCells[cidx].bodies[0] := mUData;
  mCells[cidx].bodies[1] := -1;
  mCells[cidx].next := pc;
  mGrid[grida] := cidx;
  {$ELSE}
  cidx := allocCell();
  //e_WriteLog(Format('  01: allocated cell for grid coords (%d,%d), body coords:(%d,%d): #%d', [gx, gy, dx, dy, cidx]), MSG_NOTIFY);
  mCells[cidx].body := mUData;
  mCells[cidx].next := pc;
  mGrid[grida] := cidx;
  {$ENDIF}
end;


procedure TBodyGridBase.insert (body: TBodyProxyId);
var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  mUData := body;
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, inserter);
end;


function TBodyGridBase.remover (grida: Integer): Boolean;
var
  {$IFDEF grid_use_buckets}
  f: Integer;
  {$ENDIF}
  pidx, idx, tmp: Integer;
  {$IFDEF grid_use_buckets}
  pc: PGridCell;
  {$ENDIF}
begin
  result := false; // never stop
  // find and remove cell
  pidx := -1;
  idx := mGrid[grida];
  while (idx >= 0) do
  begin
    tmp := mCells[idx].next;
    {$IFDEF grid_use_buckets}
    pc := @mCells[idx];
    f := 0;
    while (f < High(TGridCell.bodies)) do
    begin
      if (pc.bodies[f] = mUData) then
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
    {$ELSE}
    if (mCells[idx].body = mUData) then
    begin
      if (pidx = -1) then mGrid[grida] := tmp else mCells[pidx].next := tmp;
      freeCell(idx);
      exit; // assume that we cannot have one object added to bucket twice
    end;
    {$ENDIF}
    pidx := idx;
    idx := tmp;
  end;
end;


// absolutely not tested
procedure TBodyGridBase.remove (body: TBodyProxyId);
var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  mUData := body;
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, remover);
end;


function TBodyGridBase.insertBody (aObj: ITP; aX, aY, aWidth, aHeight: Integer; aTag: Integer=0): TBodyProxyId;
begin
  result := allocProxy(aX, aY, aWidth, aHeight, aObj, aTag);
  insert(result);
end;


procedure TBodyGridBase.removeBody (aObj: TBodyProxyId);
begin
  if (aObj < 0) or (aObj > High(mProxies)) then exit; // just in case
  remove(aObj);
  freeProxy(aObj);
end;


procedure TBodyGridBase.moveResizeBody (body: TBodyProxyId; dx, dy, sx, sy: Integer);
var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if ((dx = 0) and (dy = 0) and (sx = 0) and (sy = 0)) then exit;
  remove(body);
  px := @mProxies[body];
  Inc(px.mX, dx);
  Inc(px.mY, dy);
  Inc(px.mWidth, sx);
  Inc(px.mHeight, sy);
  insert(body);
end;

procedure TBodyGridBase.moveBody (body: TBodyProxyId; dx, dy: Integer);
begin
  moveResizeBody(body, dx, dy, 0, 0);
end;

procedure TBodyGridBase.resizeBody (body: TBodyProxyId; sx, sy: Integer);
begin
  moveResizeBody(body, 0, 0, sx, sy);
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.forEachAtPoint (x, y: Integer; cb: TGridQueryCB; tagmask: Integer=-1): Boolean;
var
  {$IFDEF grid_use_buckets}
  f: Integer;
  {$ENDIF}
  idx, curci: Integer;
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
begin
  result := false;
  if not assigned(cb) or (tagmask = 0) then exit;

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
    {$IFDEF grid_use_buckets}
    for f := 0 to High(TGridCell.bodies) do
    begin
      if (cc.bodies[f] = -1) then break;
      px := @mProxies[cc.bodies[f]];
      if (px.mQueryMark <> lq) and ((px.mTag and tagmask) <> 0) then
      begin
        if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
        begin
          px.mQueryMark := lq;
          result := cb(px.mObj, px.mTag);
          if result then exit;
        end;
      end;
    end;
    {$ELSE}
    if (cc.body <> -1) then
    begin
      px := @mProxies[cc.body];
      if (px.mQueryMark <> lq) and ((px.mTag and tagmask) <> 0) then
      begin
        if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
        begin
          px.mQueryMark := lq;
          result := cb(px.mObj, px.mTag);
          if result then exit;
        end;
      end;
    end;
    {$ENDIF}
    curci := cc.next;
  end;
end;


function TBodyGridBase.forEachInAABB (x, y, w, h: Integer; cb: TGridQueryCB; tagmask: Integer=-1): Boolean;
var
  idx: Integer;
  gx, gy: Integer;
  curci: Integer;
  {$IFDEF grid_use_buckets}
  f: Integer;
  {$ENDIF}
  cc: PGridCell = nil;
  px: PBodyProxyRec;
  lq: LongWord;
  tsize, gw: Integer;
  x0, y0: Integer;
begin
  result := false;
  if (w < 1) or (h < 1) or not assigned(cb) then exit;

  x0 := x;
  y0 := y;

  // fix coords
  Dec(x, mMinX);
  Dec(y, mMinY);

  gw := mWidth;
  tsize := mTileSize;

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
        {$IFDEF grid_use_buckets}
        for f := 0 to High(TGridCell.bodies) do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          if (px.mQueryMark <> lq) and ((px.mTag and tagmask) <> 0) then
          begin
            if (x0 >= px.mX+px.mWidth) or (y0 >= px.mY+px.mHeight) then continue;
            if (x0+w <= px.mX) or (y0+h <= px.mY) then continue;
            px.mQueryMark := lq;
            result := cb(px.mObj, px.mTag);
            if result then exit;
          end;
        end;
        {$ELSE}
        if (cc.body <> 1) then
        begin
          px := @mProxies[cc.body];
          if (px.mQueryMark <> lq) and ((px.mTag and tagmask) <> 0) then
          begin
            if (x0 >= px.mX+px.mWidth) or (y0 >= px.mY+px.mHeight) or (x0+w <= px.mX) or (y0+h <= px.mY) then
            begin
              // no intersection
            end
            else
            begin
              px.mQueryMark := lq;
              result := cb(px.mObj, px.mTag);
              if result then exit;
            end;
          end;
        end;
        {$ENDIF}
        curci := cc.next;
      end;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TBodyGridBase.traceRay (x0, y0, x1, y1: Integer; cb: TGridRayQueryCB; tagmask: Integer=-1): Boolean;
var
  i: Integer;
  dx, dy: Integer;
  xerr: Integer = 0;
  yerr: Integer = 0;
  d: Integer;
  incx, incy: Integer;
  x, y: Integer;
  maxx, maxy: Integer;
  tsize: Integer; // tile size
  gw, gh: Integer;
  lastGA: Integer = -1;
  ga: Integer = -1; // last used grid address
  ccidx: Integer = -1;
  curci: Integer = -1;
  cc: PGridCell = nil;
  hasUntried: Boolean;
  f: Integer;
  px: PBodyProxyRec;
  lq: LongWord;
  prevX, prevY: Integer;
  minx, miny: Integer;
begin
  result := false;

  if (tagmask = 0) then exit;

  // make coords (0,0)-based
  minx := mMinX;
  miny := mMinY;
  Dec(x0, minx);
  Dec(y0, miny);
  Dec(x1, minx);
  Dec(y1, miny);

  dx := x1-x0;
  dy := y1-y0;

  if (dx > 0) then incx := 1 else if (dx < 0) then incx := -1 else incx := 0;
  if (dy > 0) then incy := 1 else if (dy < 0) then incy := -1 else incy := 0;

  dx := abs(dx);
  dy := abs(dy);

  if (dx > dy) then d := dx else d := dy;

  x := x0;
  y := y0;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for i := 0 to High(mProxies) do mProxies[i].mQueryMark := 0;
  end;
  lq := mLastQuery;

  tsize := mTileSize;
  gw := mWidth;
  gh := mHeight;
  maxx := gw*tsize-1;
  maxy := gh*tsize-1;

  for i := 1 to d do
  begin
    prevX := x+minx;
    prevY := y+miny;
    Inc(xerr, dx); if (xerr >= d) then begin Dec(xerr, d); Inc(x, incx); end;
    Inc(yerr, dy); if (yerr >= d) then begin Dec(yerr, d); Inc(y, incy); end;

    if (x >= 0) and (y >= 0) and (x <= maxx) and (y <= maxy) then
    begin
      ga := (y div tsize)*gw+(x div tsize);
      if (lastGA <> ga) then
      begin
        // new cell
        lastGA := ga;
        ccidx := mGrid[lastGA];
        {
        if (ccidx <> -1) then
        begin
          result := cb(nil, 0, x+minx, y+miny, prevX, prevY);
          if result then exit;
        end;
        }
      end;
    end
    else
    begin
      if (ccidx <> -1) then
      begin
        ccidx := -1;
        result := cb(nil, 0, x+minx, y+miny, prevX, prevY);
        if result then exit;
      end;
    end;

    if (ccidx <> -1) then
    begin
      curci := ccidx;
      hasUntried := false;
      Inc(x, minx);
      Inc(y, miny);
      while (curci <> -1) do
      begin
        cc := @mCells[curci];
        {$IFDEF grid_use_buckets}
        for f := 0 to High(TGridCell.bodies) do
        begin
          if (cc.bodies[f] = -1) then break;
          px := @mProxies[cc.bodies[f]];
          if (px.mQueryMark <> lq) and ((px.mTag and tagmask) <> 0) then
          begin
            if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
            begin
              px.mQueryMark := lq;
              result := cb(px.mObj, px.mTag, x, y, prevX, prevY);
              if result then exit;
            end
            else
            begin
              hasUntried := true;
            end;
          end;
        end;
        {$ELSE}
        if (cc.body <> -1) then
        begin
          px := @mProxies[cc.body];
          if (px.mQueryMark <> lq) and ((px.mTag and tagmask) <> 0) then
          begin
            if (x >= px.mX) and (y >= px.mY) and (x < px.mX+px.mWidth) and (y < px.mY+px.mHeight) then
            begin
              px.mQueryMark := lq;
              result := cb(px.mObj, px.mTag, x, y, prevX, prevY);
              if result then exit;
            end
            else
            begin
              hasUntried := true;
            end;
          end;
        end;
        {$ENDIF}
        curci := cc.next;
      end;
      if not hasUntried then
      begin
        // don't process this cell anymore
        ccidx := -1;
        result := cb(nil, 0, x, y, prevX, prevY);
        if result then exit;
      end;
      Dec(x, minx);
      Dec(y, miny);
    end;
  end;
end;


end.
