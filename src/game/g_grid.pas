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
{$MODE DELPHI}
{$modeswitch nestedprocvars}
unit g_grid;

interface

uses e_log;

const
  GridDefaultTileSize = 32;

type
  GridQueryCB = function (obj: TObject; tag: Integer): Boolean is nested; // return `true` to stop

type
  TBodyGrid = class;

  TBodyProxy = class(TObject)
  private
    mX, mY, mWidth, mHeight: Integer; // aabb
    mQueryMark: DWord; // was this object visited at this query?
    mObj: TObject;
    mGrid: TBodyGrid;
    mTag: Integer;

  public
    constructor Create (aGrid: TBodyGrid; aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);
    destructor Destroy (); override;

    property x: Integer read mX;
    property y: Integer read mY;
    property width: Integer read mWidth;
    property height: Integer read mHeight;
    property obj: TObject read mObj;
    property tag: Integer read mTag;
    property grid: TBodyGrid read mGrid;
  end;

  PGridCell = ^TGridCell;
  TGridCell = record
    body: TBodyProxy;
    next: Integer; // in this cell; index in mCells
  end;

  TBodyGrid = class(TObject)
  private
    mTileSize: Integer;
    mWidth, mHeight: Integer; // in tiles
    mGrid: array of Integer; // mWidth*mHeight, index in mCells
    mCells: array of TGridCell; // cell pool
    mFreeCell: Integer; // first free cell index or -1
    mLastQuery: Integer;
    mUsedCells: Integer;

  private
    function allocCell: Integer;
    procedure freeCell (idx: Integer); // `next` is simply overwritten

    procedure insert (body: TBodyProxy);
    procedure remove (body: TBodyProxy);

  public
    constructor Create (aPixWidth, aPixHeight: Integer; aTileSize: Integer=GridDefaultTileSize);
    destructor Destroy (); override;

    function insertBody (aObj: TObject; ax, ay, aWidth, aHeight: Integer; aTag: Integer=0): TBodyProxy;
    procedure moveBody (body: TBodyProxy; dx, dy: Integer);
    procedure resizeBody (body: TBodyProxy; sx, sy: Integer);
    procedure moveResizeBody (body: TBodyProxy; dx, dy, sx, sy: Integer);

    function forEachInAABB (x, y, w, h: Integer; cb: GridQueryCB): Boolean;

    function getProxyForBody (aObj: TObject; x, y, w, h: Integer): TBodyProxy;

    procedure dumpStats ();
  end;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBodyProxy.Create (aGrid: TBodyGrid; aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);
begin
  mX := aX;
  mY := aY;
  mWidth := aWidth;
  mHeight := aHeight;
  mQueryMark := 0;
  mObj := aObj;
  mGrid := aGrid;
  mTag := aTag;
end;

destructor TBodyProxy.Destroy ();
begin
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBodyGrid.Create (aPixWidth, aPixHeight: Integer; aTileSize: Integer=GridDefaultTileSize);
var
  idx: Integer;
begin
  if aTileSize < 1 then aTileSize := 1;
  if aTileSize > 8192 then aTileSize := 8192; // arbitrary limit
  if aPixWidth < aTileSize then aPixWidth := aTileSize;
  if aPixHeight < aTileSize then aPixHeight := aTileSize;
  mTileSize := aTileSize;
  mWidth := (aPixWidth+aTileSize-1) div aTileSize;
  mHeight := (aPixHeight+aTileSize-1) div aTileSize;
  SetLength(mGrid, mWidth*mHeight);
  SetLength(mCells, mWidth*mHeight);
  mFreeCell := 0;
  // init free list
  for idx := 0 to High(mCells) do
  begin
    mCells[idx].body := nil;
    mCells[idx].next := idx+1;
  end;
  mCells[High(mCells)].next := -1; // last cell
  // init grid
  for idx := 0 to High(mGrid) do mGrid[idx] := -1;
  mLastQuery := 0;
  mUsedCells := 0;
  e_WriteLog(Format('created grid with size: %dx%d (tile size: %d); pix: %dx%d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize]), MSG_NOTIFY);
end;


destructor TBodyGrid.Destroy ();
var
  idx: Integer;
begin
  // free all owned proxies
  for idx := 0 to High(mCells) do mCells[idx].body.Free;
  mCells := nil;
  mGrid := nil;
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
  e_WriteLog(Format('grid size: %dx%d (tile size: %d); pix: %dx%d; used cells: %d; max bodys in cell: %d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize, mUsedCells, mcb]), MSG_NOTIFY);
end;


function TBodyGrid.allocCell: Integer;
var
  idx: Integer;
begin
  if (mFreeCell < 0) then
  begin
    // no free cells, want more
    mFreeCell := Length(mCells);
    SetLength(mCells, mFreeCell+16384); // arbitrary number
    for idx := mFreeCell to High(mCells) do
    begin
      mCells[idx].body := nil;
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
    if mCells[idx].body = nil then exit; // the thing that should not be
    mCells[idx].body := nil;
    mCells[idx].next := mFreeCell;
    mFreeCell := idx;
    Dec(mUsedCells);
  end;
end;


procedure TBodyGrid.insert (body: TBodyProxy);
var
  dx, dy, gx, gy, cidx: Integer;
begin
  if body = nil then exit;
  if (body.mWidth < 1) or (body.mHeight < 1) then exit;
  // out of grid?
  if (body.mX+body.mWidth <= 0) or (body.mY+body.mHeight <= 0) then exit;
  if (body.mX >= mWidth*mTileSize) or (body.mY >= mHeight*mTileSize) then exit;
  //e_WriteLog(Format('grid: inserting body: (%d,%d)-(%dx%d)', [body.mX, body.mY, body.mWidth, body.mHeight]), MSG_NOTIFY);
  gy := body.mY div mTileSize;
  dy := 0;
  while (dy < body.mHeight) do
  begin
    if (gy >= 0) and (gy < mHeight) then
    begin
      dx := 0;
      gx := body.mX div mTileSize;
      while (dx < body.mWidth) do
      begin
        if (gx >= 0) and (gx < mWidth) then
        begin
          //e_WriteLog(Format('  00: allocating cell for grid coords (%d,%d), body coords:(%d,%d)', [gx, gy, dx, dy]), MSG_NOTIFY);
          cidx := allocCell();
          //e_WriteLog(Format('  01: allocated cell for grid coords (%d,%d), body coords:(%d,%d): #%d', [gx, gy, dx, dy, cidx]), MSG_NOTIFY);
          mCells[cidx].body := body;
          mCells[cidx].next := mGrid[gy*mWidth+gx];
          mGrid[gy*mWidth+gx] := cidx;
          //e_WriteLog(Format('  02: put cell for grid coords (%d,%d), body coords:(%d,%d): #%d', [gx, gy, dx, dy, cidx]), MSG_NOTIFY);
        end;
        Inc(dx, mTileSize);
        Inc(gx);
      end;
    end;
    Inc(dy, mTileSize);
    Inc(gy);
  end;
end;


// absolutely not tested
procedure TBodyGrid.remove (body: TBodyProxy);
var
  dx, dy, gx, gy, idx, pidx, tmp: Integer;
begin
  if body = nil then exit;
  if (body.mWidth < 1) or (body.mHeight < 1) then exit;
  // out of grid?
  if (body.mX+body.mWidth <= 0) or (body.mY+body.mHeight <= 0) then exit;
  if (body.mX >= mWidth*mTileSize) or (body.mY >= mHeight*mTileSize) then exit;
  gy := body.mY div mTileSize;
  dy := 0;
  pidx := -1;
  while (dy < body.mHeight) do
  begin
    if (gy >= 0) and (gy < mHeight) then
    begin
      dx := 0;
      gx := body.mX div mTileSize;
      while (dx < body.mWidth) do
      begin
        if (gx >= 0) and (gx < mWidth) then
        begin
          // find and remove cell
          pidx := -1;
          idx := mGrid[gy*mWidth+gx];
          while idx >= 0 do
          begin
            tmp := mCells[idx].next;
            if mCells[idx].body = body then
            begin
              if pidx = -1 then mGrid[gy*mWidth+gx] := tmp else mCells[pidx].next := tmp;
              freeCell(idx);
            end
            else
            begin
              pidx := idx;
            end;
            idx := tmp;
          end;
        end;
        Inc(dx, mTileSize);
        Inc(gx);
      end;
    end;
    Inc(dy, mTileSize);
    Inc(gy);
  end;
end;


function TBodyGrid.insertBody (aObj: TObject; aX, aY, aWidth, aHeight: Integer; aTag: Integer=0): TBodyProxy;
begin
  result := nil;
  if aObj = nil then exit;
  result := TBodyProxy.Create(self, aX, aY, aWidth, aHeight, aObj, aTag);
  insert(result);
end;


procedure TBodyGrid.moveResizeBody (body: TBodyProxy; dx, dy, sx, sy: Integer);
begin
  if (body = nil) or ((dx = 0) and (dy = 0) and (sx = 0) and (sy = 0)) then exit;
  remove(body);
  Inc(body.mX, dx);
  Inc(body.mY, dy);
  Inc(body.mWidth, sx);
  Inc(body.mHeight, sy);
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


function TBodyGrid.forEachInAABB (x, y, w, h: Integer; cb: GridQueryCB): Boolean;
var
  gx, gy, idx: Integer;
  minx, miny, maxx, maxy: Integer;
begin
  result := false;
  if not assigned(cb) then exit;
  if (w < 1) or (h < 1) then exit;
  minx := x;
  miny := y;
  maxx := x+w-1;
  maxy := y+h-1;
  if (minx > maxx) or (miny > maxy) then exit;
  if (maxx < 0) or (maxy < 0) then exit;
  if (minx >= mWidth*mTileSize) or (miny >= mHeight*mTileSize) then exit;
  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for idx := 0 to High(mCells) do if (mCells[idx].body <> nil) then mCells[idx].body.mQueryMark := 0;
  end;
  //e_WriteLog(Format('grid: query #%d: (%d,%d)-(%dx%d)', [mLastQuery, minx, miny, maxx, maxy]), MSG_NOTIFY);
  // process grid
  for gy := miny div mTileSize to maxy div mTileSize do
  begin
    if (gy < 0) then continue;
    if (gy >= mHeight) then break;
    for gx := minx div mTileSize to maxx div mTileSize do
    begin
      if (gx < 0) then continue;
      if (gx >= mWidth) then break;
      idx := mGrid[gy*mWidth+gx];
      while idx >= 0 do
      begin
        if (mCells[idx].body <> nil) and (mCells[idx].body.mQueryMark <> mLastQuery) then
        begin
          //e_WriteLog(Format('  query #%d body hit: (%d,%d)-(%dx%d) tag:%d', [mLastQuery, mCells[idx].body.mX, mCells[idx].body.mY, mCells[idx].body.mWidth, mCells[idx].body.mHeight, mCells[idx].body.mTag]), MSG_NOTIFY);
          mCells[idx].body.mQueryMark := mLastQuery;
          if (cb(mCells[idx].body.mObj, mCells[idx].body.mTag)) then begin result := true; exit; end;
        end;
        idx := mCells[idx].next;
      end;
    end;
  end;
end;


function TBodyGrid.getProxyForBody (aObj: TObject; x, y, w, h: Integer): TBodyProxy;

  function qq (obj: TObject; tag: Integer): Boolean;
  begin
    result := (obj = aObj);
  end;

begin
  result := nil;
  forEachInAABB(x, y, w, h, qq);
end;

end.
