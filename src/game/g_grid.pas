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

const
  GridDefaultTileSize = 32;
  GridCellBucketSize = 8; // WARNING! can't be less than 2!

type
  GridQueryCB = function (obj: TObject; var proxy: PBodyProxyRec; tag: Integer): Boolean is nested; // return `true` to stop

type
  TBodyGrid = class;

  TBodyProxy = Integer;

  PBodyProxyRec = ^TBodyProxyRec;
  TBodyProxyRec = record
  private
    mX, mY, mWidth, mHeight: Integer; // aabb
    mQueryMark: DWord; // was this object visited at this query?
    mObj: TObject;
    //mGrid: TBodyGrid;
    mTag: Integer;
    nextLink: TBodyProxy; // next free or nothing

  private
    procedure setup (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);

  public
    //constructor Create (aGrid: TBodyGrid; aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);
    //destructor Destroy (); override;

    property x: Integer read mX;
    property y: Integer read mY;
    property width: Integer read mWidth;
    property height: Integer read mHeight;
    property obj: TObject read mObj;
    property tag: Integer read mTag;
    //property grid: TBodyGrid read mGrid;
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

  GridInternalCB = function (grida: Integer): Boolean is nested; // return `true` to stop

  TBodyGrid = class(TObject)
  private
    mTileSize: Integer;
    mMinX, mMinY: Integer; // so grids can start at any origin
    mWidth, mHeight: Integer; // in tiles
    mGrid: array of Integer; // mWidth*mHeight, index in mCells
    mCells: array of TGridCell; // cell pool
    mFreeCell: Integer; // first free cell index or -1
    mLastQuery: DWord;
    mUsedCells: Integer;
    mProxies: array of TBodyProxyRec;
    mProxyFree: TBodyProxy; // free
    mProxyCount: Integer; // currently used
    mProxyMaxCount: Integer;

  private
    function allocCell: Integer;
    procedure freeCell (idx: Integer); // `next` is simply overwritten

    function allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer): TBodyProxy;
    procedure freeProxy (body: TBodyProxy);

    procedure insert (body: TBodyProxy);
    procedure remove (body: TBodyProxy);

    function forGridRect (x, y, w, h: Integer; cb: GridInternalCB): Boolean;

  public
    constructor Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer; aTileSize: Integer=GridDefaultTileSize);
    destructor Destroy (); override;

    function insertBody (aObj: TObject; ax, ay, aWidth, aHeight: Integer; aTag: Integer=0): TBodyProxy;
    procedure removeBody (aObj: TBodyProxy); // WARNING! this WILL destroy proxy!

    procedure moveBody (body: TBodyProxy; dx, dy: Integer);
    procedure resizeBody (body: TBodyProxy; sx, sy: Integer);
    procedure moveResizeBody (body: TBodyProxy; dx, dy, sx, sy: Integer);

    function forEachInAABB (x, y, w, h: Integer; cb: GridQueryCB; tagmask: Integer=-1): Boolean;

    //function getProxyForBody (aObj: TObject; x, y, w, h: Integer): TBodyProxy;

    procedure dumpStats ();
  end;


implementation

uses
  SysUtils, e_log;


// ////////////////////////////////////////////////////////////////////////// //
procedure TBodyProxyRec.setup (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);
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
constructor TBodyGrid.Create (aMinPixX, aMinPixY, aPixWidth, aPixHeight: Integer; aTileSize: Integer=GridDefaultTileSize);
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
  e_WriteLog(Format('created grid with size: %dx%d (tile size: %d); pix: %dx%d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize]), MSG_NOTIFY);
end;


destructor TBodyGrid.Destroy ();
begin
  mCells := nil;
  mGrid := nil;
  mProxies := nil;
  inherited;
end;


procedure TBodyGrid.dumpStats ();
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


function TBodyGrid.allocCell: Integer;
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
  Inc(mUsedCells);
  //e_WriteLog(Format('grid: allocated new cell #%d (total: %d)', [result, mUsedCells]), MSG_NOTIFY);
end;


procedure TBodyGrid.freeCell (idx: Integer);
begin
  if (idx >= 0) and (idx < High(mCells)) then
  begin
    //if mCells[idx].body = -1 then exit; // the thing that should not be
    //mCells[idx].body := -1;
    mCells[idx].next := mFreeCell;
    mFreeCell := idx;
    Dec(mUsedCells);
  end;
end;


function TBodyGrid.allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer): TBodyProxy;
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

procedure TBodyGrid.freeProxy (body: TBodyProxy);
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (mProxyCount = 0) then raise Exception.Create('wutafuuuuu in grid (no allocated proxies, what i should free now?)');
  // add to free list
  mProxies[body].mObj := nil;
  mProxies[body].nextLink := mProxyFree;
  mProxyFree := body;
  Dec(mProxyCount);
end;


function TBodyGrid.forGridRect (x, y, w, h: Integer; cb: GridInternalCB): Boolean;
var
  gx, gy: Integer;
begin
  result := false;
  if (w < 1) or (h < 1) or not assigned(cb) then exit;
  // fix coords
  Dec(x, mMinX);
  Dec(y, mMinY);
  // go on
  if (x+w <= 0) or (y+h <= 0) then exit;
  if (x >= mWidth*mTileSize) or (y >= mHeight*mTileSize) then exit;
  for gy := y div mTileSize to (y+h-1) div mTileSize do
  begin
    if (gy < 0) then continue;
    if (gy >= mHeight) then break;
    for gx := x div mTileSize to (x+w-1) div mTileSize do
    begin
      if (gx < 0) then continue;
      if (gx >= mWidth) then break;
      if (cb(gy*mWidth+gx)) then begin result := true; exit; end;
    end;
  end;
end;


procedure TBodyGrid.insert (body: TBodyProxy);

  function inserter (grida: Integer): Boolean;
  var
    cidx: Integer;
    pc: PInteger;
    {$IFDEF grid_use_buckets}
    pi: PGridCell;
    f: Integer;
    {$ENDIF}
  begin
    result := false; // never stop
    // add body to the given grid cell
    pc := @mGrid[grida];
    {$IFDEF grid_use_buckets}
    if (pc^ <> -1) then
    begin
      pi := @mCells[pc^];
      f := 0;
      for f := 0 to High(TGridCell.bodies) do
      begin
        if (pi.bodies[f] = -1) then
        begin
          // can add here
          pi.bodies[f] := body;
          if (f+1 < Length(TGridCell.bodies)) then pi.bodies[f+1] := -1;
          exit;
        end;
      end;
    end;
    // either no room, or no cell at all
    cidx := allocCell();
    mCells[cidx].bodies[0] := body;
    mCells[cidx].bodies[1] := -1;
    mCells[cidx].next := pc^;
    pc^ := cidx;
    {$ELSE}
    cidx := allocCell();
    //e_WriteLog(Format('  01: allocated cell for grid coords (%d,%d), body coords:(%d,%d): #%d', [gx, gy, dx, dy, cidx]), MSG_NOTIFY);
    mCells[cidx].body := body;
    mCells[cidx].next := pc^;
    pc^ := cidx;
    {$ENDIF}
  end;

var
  px: PBodyProxyRec;
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, inserter);
end;


// absolutely not tested
procedure TBodyGrid.remove (body: TBodyProxy);

(*
  function remover (grida: Integer): Boolean;
  var
    pidx, idx, tmp: Integer;
  begin
    result := false; // never stop
    // find and remove cell
    pidx := -1;
    idx := mGrid[grida];
    while idx >= 0 do
    begin
      tmp := mCells[idx].next;
      if (mCells[idx].body = body) then
      begin
        if (pidx = -1) then mGrid[grida] := tmp else mCells[pidx].next := tmp;
        freeCell(idx);
        break; // assume that we cannot have one object added to bucket twice
      end
      else
      begin
        pidx := idx;
      end;
      idx := tmp;
    end;
  end;

var
  px: PBodyProxyRec;
*)
begin
(*
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  forGridRect(px.mX, px.mY, px.mWidth, px.mHeight, remover);
*)
  raise Exception.Create('TBodyGrid.remove: not yet, sorry');
end;


function TBodyGrid.insertBody (aObj: TObject; aX, aY, aWidth, aHeight: Integer; aTag: Integer=0): TBodyProxy;
begin
  result := allocProxy(aX, aY, aWidth, aHeight, aObj, aTag);
  insert(result);
end;


procedure TBodyGrid.removeBody (aObj: TBodyProxy);
begin
  if (aObj < 0) or (aObj > High(mProxies)) then exit; // just in case
  removeBody(aObj);
  freeProxy(aObj);
end;


procedure TBodyGrid.moveResizeBody (body: TBodyProxy; dx, dy, sx, sy: Integer);
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

procedure TBodyGrid.moveBody (body: TBodyProxy; dx, dy: Integer);
begin
  moveResizeBody(body, dx, dy, 0, 0);
end;

procedure TBodyGrid.resizeBody (body: TBodyProxy; sx, sy: Integer);
begin
  moveResizeBody(body, 0, 0, sx, sy);
end;


function TBodyGrid.forEachInAABB (x, y, w, h: Integer; cb: GridQueryCB; tagmask: Integer=-1): Boolean;
  function iterator (grida: Integer): Boolean;
  var
    idx: Integer;
    px: PBodyProxyRec;
    {$IFDEF grid_use_buckets}
    pi: PGridCell;
    f: Integer;
    {$ENDIF}
  begin
    result := false;
    idx := mGrid[grida];
    while (idx >= 0) do
    begin
      {$IFDEF grid_use_buckets}
      pi := @mCells[idx];
      for f := 0 to High(TGridCell.bodies) do
      begin
        if (pi.bodies[f] = -1) then break;
        px := @mProxies[pi.bodies[f]];
        if (px.mQueryMark <> mLastQuery) and ((px.mTag and tagmask) <> 0) then
        begin
          //e_WriteLog(Format('  query #%d body hit: (%d,%d)-(%dx%d) tag:%d', [mLastQuery, mCells[idx].body.mX, mCells[idx].body.mY, mCells[idx].body.mWidth, mCells[idx].body.mHeight, mCells[idx].body.mTag]), MSG_NOTIFY);
          px.mQueryMark := mLastQuery;
          if (cb(px.mObj, px.mTag)) then begin result := true; exit; end;
        end;
      end;
      idx := pi.next;
      {$ELSE}
      if (mCells[idx].body <> -1) then
      begin
        px := @mProxies[mCells[idx].body];
        if (px.mQueryMark <> mLastQuery) and ((px.mTag and tagmask) <> 0) then
        begin
          //e_WriteLog(Format('  query #%d body hit: (%d,%d)-(%dx%d) tag:%d', [mLastQuery, mCells[idx].body.mX, mCells[idx].body.mY, mCells[idx].body.mWidth, mCells[idx].body.mHeight, mCells[idx].body.mTag]), MSG_NOTIFY);
          px.mQueryMark := mLastQuery;
          if (cb(px.mObj, px.mTag)) then begin result := true; exit; end;
        end;
      end;
      idx := mCells[idx].next;
      {$ENDIF}
    end;
  end;

var
  idx: Integer;
begin
  result := false;
  if not assigned(cb) then exit;

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for idx := 0 to High(mProxies) do mProxies[idx].mQueryMark := 0;
  end;
  //e_WriteLog(Format('grid: query #%d: (%d,%d)-(%dx%d)', [mLastQuery, minx, miny, maxx, maxy]), MSG_NOTIFY);

  result := forGridRect(x, y, w, h, iterator);
end;


(*
function TBodyGrid.getProxyForBody (aObj: TObject; x, y, w, h: Integer): TBodyProxy;
var
  res: TBodyProxy = -1;

  function iterator (grida: Integer): Boolean;
  var
    idx: Integer;
  begin
    result := false;
    idx := mGrid[grida];
    while idx >= 0 do
    begin
      if (mCells[idx].body <> -1) and (mProxies[mCells[idx].body].mObj = aObj) then
      begin
        result := true;
        res := mCells[idx].body;
        exit;
      end;
      idx := mCells[idx].next;
    end;
  end;

begin
  result := -1;
  if (aObj = nil) then exit;
  forGridRect(x, y, w, h, iterator);
  result := res;
end;
*)


end.
