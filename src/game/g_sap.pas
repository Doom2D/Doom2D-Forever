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
{$INCLUDE ../shared/a_modes.inc}
// universal sweep-and-prune broad phase
unit g_sap;

interface

type
  TSAPQueryCB = function (obj: TObject; tag: Integer): Boolean is nested; // return `true` to stop

type
  TSAPProxy = Integer;

  PSAPProxyRec = ^TSAPProxyRec;
  TSAPProxyRec = record
  private
    mX, mY, mWidth, mHeight: Integer; // aabb
    mQueryMark: DWord; // was this object visited at this query?
    mObj: TObject;
    mTag: Integer;
    //nextLink: TSAPProxy; // next free or nothing
    mIIdx: array [0..1] of Integer; // indicies in corresponding intervals

  private
    procedure setup (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);

  public
    property x: Integer read mX;
    property y: Integer read mY;
    property width: Integer read mWidth;
    property height: Integer read mHeight;
    property obj: TObject read mObj;
    property tag: Integer read mTag;
    //property grid: TBodyGrid read mGrid;
  end;


  TSweepAndPrune = class(TObject)
  private
    type
      //PInterval = ^TInterval;
      TInterval = record
      public
        min, max: Integer;
        pidx: Integer; // proxy idx

      public
        function less (var i: TInterval): Boolean; inline;
        function intersects (v0, v1: Integer): Boolean; inline;
      end;

  private
    mLastQuery: DWord;
    mIntrs: array[0..1] of array of TInterval;
    mIntrsUsed: array[0..1] of Integer;
    mProxies: array of TSAPProxyRec;
    mProxyFree: TSAPProxy; // free
    mProxyCount: Integer; // currently used
    mProxyMaxCount: Integer;
    mUpdateBlocked: Integer; // >0: updates are blocked

  private
    function allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer): TSAPProxy;
    procedure freeProxy (body: TSAPProxy);

    procedure sortIntervals ();

    procedure insert (body: TSAPProxy);
    //procedure remove (body: TSAPProxy);

  public
    constructor Create ();
    destructor Destroy (); override;

    function insertBody (aObj: TObject; ax, ay, aWidth, aHeight: Integer; aTag: Integer=0): TSAPProxy;
    //procedure removeBody (aObj: TSAPProxy); // WARNING! this WILL destroy proxy!

    //procedure moveBody (body: TSAPProxy; dx, dy: Integer);
    //procedure resizeBody (body: TSAPProxy; sx, sy: Integer);
    //procedure moveResizeBody (body: TSAPProxy; dx, dy, sx, sy: Integer);

    function forEachInAABB (x, y, w, h: Integer; cb: TSAPQueryCB): Boolean;

    //function getProxyForBody (aObj: TObject; x, y, w, h: Integer): TSAPProxy;

    // call these functions before massive update (it may, or may be not faster)
    procedure batchUpdateBegin ();
    procedure batchUpdateEnd ();

    procedure dumpStats ();
  end;


implementation

uses
  SysUtils, e_log;


// ////////////////////////////////////////////////////////////////////////// //
procedure TSAPProxyRec.setup (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer);
begin
  mX := aX;
  mY := aY;
  mWidth := aWidth;
  mHeight := aHeight;
  mQueryMark := 0;
  mObj := aObj;
  mTag := aTag;
  //nextLink := -1;
  mIIdx[0] := -1;
  mIIdx[1] := -1;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TSweepAndPrune.TInterval.less (var i: TInterval): Boolean;
var
  n: Integer;
begin
  n := min-i.min;
  if (n <> 0) then begin result := (n < 0); exit; end;
  n := max-i.max;
  if (n <> 0) then begin result := (n < 0); exit; end;
  result := (pidx < i.pidx);
end;


// v0 MUST be <= v1!
function TSweepAndPrune.TInterval.intersects (v0, v1: Integer): Boolean;
begin
  result := false;
  if (v1 < min) or (v0 > max) then exit;
  result := true;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TSweepAndPrune.Create ();
var
  idx: Integer;
begin
  mLastQuery := 0;

  // init intervals
  for idx := 0 to High(mIntrs) do
  begin
    SetLength(mIntrs[idx], 8192);
    mIntrsUsed[idx] := 0;
  end;

  // init proxies
  SetLength(mProxies, 8192);
  for idx := 0 to High(mProxies) do
  begin
    mProxies[idx].mIIdx[0] := idx+1;
    mProxies[idx].mIIdx[1] := -1;
  end;
  mProxies[High(mProxies)].mIIdx[0] := -1;

  mProxyFree := 0;
  mProxyCount := 0;
  mProxyMaxCount := 0;

  mUpdateBlocked := 0;

  //e_WriteLog(Format('created grid with size: %dx%d (tile size: %d); pix: %dx%d', [mWidth, mHeight, mTileSize, mWidth*mTileSize, mHeight*mTileSize]), MSG_NOTIFY);
end;


destructor TSweepAndPrune.Destroy ();
var
  idx: Integer;
begin
  mProxies := nil;
  for idx := 0 to High(mIntrs) do mIntrs[idx] := nil;
  inherited;
end;


procedure TSweepAndPrune.dumpStats ();
begin
  e_WriteLog(Format('used intervals: %d; max proxies allocated: %d; proxies used: %d', [mIntrsUsed[0], mProxyMaxCount, mProxyCount]), MSG_NOTIFY);
end;


procedure TSweepAndPrune.batchUpdateBegin ();
begin
  Inc(mUpdateBlocked);
end;


procedure TSweepAndPrune.batchUpdateEnd ();
begin
  if (mUpdateBlocked > 0) then
  begin
    Dec(mUpdateBlocked);
    if (mUpdateBlocked = 0) then sortIntervals();
  end;
end;


function TSweepAndPrune.allocProxy (aX, aY, aWidth, aHeight: Integer; aObj: TObject; aTag: Integer): TSAPProxy;
var
  olen, idx: Integer;
  px: PSAPProxyRec;
begin
  if (mProxyFree = -1) then
  begin
    // no free proxies, resize list
    olen := Length(mProxies);
    SetLength(mProxies, olen+8192); // arbitrary number
    for idx := olen to High(mProxies) do mProxies[idx].mIIdx[0] := idx+1;
    mProxies[High(mProxies)].mIIdx[0] := -1;
    mProxyFree := olen;
  end;
  // get one from list
  result := mProxyFree;
  px := @mProxies[result];
  mProxyFree := px.mIIdx[0];
  px.setup(aX, aY, aWidth, aHeight, aObj, aTag);
  // add to used list
  px.mIIdx[0] := -1;
  // statistics
  Inc(mProxyCount);
  if (mProxyMaxCount < mProxyCount) then mProxyMaxCount := mProxyCount;
end;

procedure TSweepAndPrune.freeProxy (body: TSAPProxy);
begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  if (mProxyCount = 0) then raise Exception.Create('wutafuuuuu in grid (no allocated proxies, what i should free now?)');
  // add to free list
  mProxies[body].mObj := nil;
  mProxies[body].mIIdx[0] := mProxyFree;
  mProxies[body].mIIdx[1] := -1;
  mProxyFree := body;
  Dec(mProxyCount);
end;


procedure TSweepAndPrune.sortIntervals ();
  procedure insSort (var arr: array of TInterval; iidx: Integer);
  var
    i, j: Integer;
    x: TInterval;
  begin
    if (Length(arr) < 2) then exit; // nothing to do
    i := 1;
    while (i < Length(arr)) do
    begin
      if (arr[i].less(arr[i-1])) then
      begin
        x := arr[i];
        j := i-1;
        while (j >= 0) and (x.less(arr[j])) do
        begin
          arr[j+1] := arr[j];
          mProxies[arr[j+1].pidx].mIIdx[iidx] := j+1;
          Dec(j);
        end;
        mProxies[x.pidx].mIIdx[iidx] := j+1;
        arr[j+1] := x;
      end;
      Inc(i);
    end;
  end;

begin
  insSort(mIntrs[0], 0);
  insSort(mIntrs[1], 1);
end;


procedure TSweepAndPrune.insert (body: TSAPProxy);
var
  px: PSAPProxyRec;

  procedure insIntr (v0, v1, iidx: Integer);
  var
    i: Integer;
  begin
    i := mIntrsUsed[iidx];
    if (i >= Length(mIntrs[iidx])) then SetLength(mIntrs[iidx], i+8192);
    mIntrs[iidx][i].min := v0;
    mIntrs[iidx][i].max := v1;
    mIntrs[iidx][i].pidx := i;
    Inc(mIntrsUsed[iidx]);
  end;

begin
  if (body < 0) or (body > High(mProxies)) then exit; // just in case
  px := @mProxies[body];
  insIntr(px.mX, px.mX+px.mWidth-1, 0);
  insIntr(px.mY, px.mY+px.mHeight-1, 1);
end;


function TSweepAndPrune.insertBody (aObj: TObject; aX, aY, aWidth, aHeight: Integer; aTag: Integer=0): TSAPProxy;
begin
  if (aObj = nil) or (aWidth < 1) or (aHeight < 1) then begin result := -1; exit; end;
  result := allocProxy(aX, aY, aWidth, aHeight, aObj, aTag);
  insert(result);
end;


function TSweepAndPrune.forEachInAABB (x, y, w, h: Integer; cb: TSAPQueryCB): Boolean;

  function walkInterval (val0, val1, lq, iidx: Integer): Boolean;
  var
    i, bot, mid, cmp: Integer;
    px: PSAPProxyRec;
  begin
    result := false;
    if (mIntrsUsed[iidx] < 1) then exit; // nothing to do
    if (mIntrsUsed[iidx] = 1) then
    begin
      // one element
      i := 0;
    end
    else
    begin
      // do search
      bot := 0;
      i := mIntrsUsed[iidx]-1;
      while (bot <> i) do
      begin
        mid := i-(i-bot) div 2;
        cmp := val0-mIntrs[iidx][mid].min;
        if (cmp = 0) then break;
        if (cmp < 0) then i := mid-1 else bot := mid;
      end;
      //return (cmpfn(lines+i) == 0 ? i : -1);
      if (i > 0) and not mIntrs[iidx][i].intersects(val0, val1) and mIntrs[iidx][i-1].intersects(val0, val1) then Dec(i);
      if (i+1 < mIntrsUsed[iidx]) and not mIntrs[iidx][i].intersects(val0, val1) and mIntrs[iidx][i+1].intersects(val0, val1) then Inc(i);
      while (i > 0) and mIntrs[iidx][i].intersects(val0, val1) do Dec(i);
      if (iidx = 0) then
      begin
        // first pass
        while (i < mIntrsUsed[iidx]) and mIntrs[iidx][i].intersects(val0, val1) do
        begin
          mProxies[mIntrs[iidx][i].pidx].mQueryMark := lq;
          Inc(i);
        end;
      end
      else
      begin
        // second pass
        while (i < mIntrsUsed[iidx]) and mIntrs[iidx][i].intersects(val0, val1) do
        begin
          px := @mProxies[mIntrs[iidx][i].pidx];
          if (px.mQueryMark = lq) then
          begin
            result := cb(px.mObj, px.mTag);
            if result then exit;
          end;
          Inc(i);
        end;
      end;
    end;
  end;

var
  idx: Integer;
begin
  result := false;
  if not assigned(cb) then exit; // no callback, not interesting
  if (w < 1) or (h < 1) then exit; // nothing to do

  // increase query counter
  Inc(mLastQuery);
  if (mLastQuery = 0) then
  begin
    // just in case of overflow
    mLastQuery := 1;
    for idx := 0 to High(mProxies) do mProxies[idx].mQueryMark := 0;
  end;

  (*
   * the algorithm is simple:
   *   find start for first interval (binary search will do)
   *   walk the interval, marking proxies with mLastQuery
   *   increment mLastQuery
   *   find start for second interval (binary search will do)
   *   walk the interval, returning proxies marked with mLastQuery
   *)
  walkInterval(x, x+w-1, mLastQuery, 0);
  result := walkInterval(x, x+w-1, mLastQuery, 1);
end;


end.
