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
{$INCLUDE a_modes.inc}
{.$DEFINE IDPOOL_CHECKS}
unit idpool;

interface

{$IFDEF USE_MEMPOOL}
uses
  mempool;
{$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
type
  //TODO: implement getting n sequential ids
  TIdPool = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  public
    const InvalidId = $ffffffff;

  private
    type
      TRange = packed record
        first, last: LongWord;
      end;

  private
    mRanges: array of TRange; // available ids; sorted
    mRangeUsed: Integer; // used elements in `mRanges`
    mMaxId: LongWord;
    mUsedIds: Integer;

  private
    function findRangeWithId (aid: LongWord): Integer;

    function getHasFreeId (aid: LongWord): Boolean;
    function getHasAllocedId (aid: LongWord): Boolean;

    function getFreeIds (): Integer; inline;
    function getCapacity (): Integer; inline;

  public
    constructor Create (amax: LongWord=$7fffffff);
    destructor Destroy (); override;

    procedure clear ();

    // returns InvalidId if there are no more free ids (or throws)
    function alloc (dothrow: Boolean=true): LongWord;

    // returns InvalidId if there are no more free ids (or throws)
    function alloc (aid: LongWord; dothrow: Boolean=true): LongWord;

    // it is NOT ok to release already released id
    procedure release (aid: LongWord);

    procedure dump ();
    procedure check ();

  public
    property hasFree[aid: LongWord]: Boolean read getHasFreeId;
    property hasAlloced[aid: LongWord]: Boolean read getHasAllocedId;
    property maxId: LongWord read mMaxId;

    property usedIds: Integer read mUsedIds;
    property freeIds: Integer read getFreeIds;

    property usedRanges: Integer read mRangeUsed;
    property capacity: Integer read getCapacity;
  end;

implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
constructor TIdPool.Create (amax: LongWord=$7fffffff);
begin
  if (amax = InvalidId) then amax := InvalidId-1;
  mMaxId := amax;
  clear();
end;


destructor TIdPool.Destroy ();
begin
  mRanges := nil;
  inherited;
end;


procedure TIdPool.dump ();
var
  f: Integer;
begin
  writeln('=== idpool: ', mRangeUsed, ' ranges ===');
  for f := 0 to mRangeUsed-1 do
  begin
    writeln('  #', f, ': [', mRanges[f].first, '-', mRanges[f].last, ']');
    if (mRanges[f].last+1 = mRanges[f].first) then raise Exception.Create('unmerged ranges');
    if (f > 0) and (mRanges[f-1].last >= mRanges[f].first) then raise Exception.Create('invalid range order');
    if (f > 0) and (mRanges[f-1].last+1 = mRanges[f].first) then raise Exception.Create('unmerged ranges');
  end;
  writeln('-----------');
end;


procedure TIdPool.check ();
var
  f: Integer;
begin
  for f := 0 to mRangeUsed-1 do
  begin
    if (mRanges[f].first > mRanges[f].last) then begin dump(); raise Exception.Create('invalid range'); end;
    if (mRanges[f].first > mMaxId) then begin dump(); raise Exception.Create('invalid range'); end;
    if (mRanges[f].last > mMaxId) then begin dump(); raise Exception.Create('invalid range'); end;
    if (f > 0) and (mRanges[f-1].last >= mRanges[f].first) then begin dump(); raise Exception.Create('invalid range order'); end;
    if (f > 0) and (mRanges[f-1].last+1 = mRanges[f].first) then begin dump(); raise Exception.Create('unmerged ranges'); end;
  end;
end;


procedure TIdPool.clear ();
begin
  SetLength(mRanges, 64);
  mRanges[0].first := 0;
  mRanges[0].last := mMaxId;
  mRangeUsed := 1;
  mUsedIds := 0;
end;


function TIdPool.getFreeIds (): Integer; inline; begin result := Integer(mMaxId+1-mUsedIds); end;
function TIdPool.getCapacity (): Integer; inline; begin result := Length(mRanges); end;


function TIdPool.findRangeWithId (aid: LongWord): Integer;
var
  len, bot, mid, i: Integer;
  ls, le: LongWord;
begin
  result := -1;
  if (aid > mMaxId) then exit;
  // -1: not found
  len := mRangeUsed;
  if (len <= 0) then exit;
  if (len = 1) then begin result := 0; exit; end;
  // yay! use binary search to find the range
  bot := 0;
  i := len-1;
  while (bot <> i) do
  begin
    mid := i-(i-bot) div 2;
    //!assert((mid >= 0) and (mid < len));
    ls := mRanges[mid].first;
    le := mRanges[mid+1].first;
    if (aid >= ls) and (aid < le) then begin result := mid; exit; end; // i found her!
    if (aid < ls) then i := mid-1 else bot := mid;
  end;
  result := i;
end;


function TIdPool.getHasFreeId (aid: LongWord): Boolean; inline;
var
  ii: Integer;
begin
  result := false;
  if (aid > mMaxId) then exit;
  ii := findRangeWithId(aid);
  if (ii < 0) then exit;
  result := (aid >= mRanges[ii].first) and (aid <= mRanges[ii].last);
end;


function TIdPool.getHasAllocedId (aid: LongWord): Boolean; inline;
var
  ii: Integer;
begin
  result := false;
  if (aid > mMaxId) then exit;
  ii := findRangeWithId(aid);
  if (ii >= 0) then result := not ((aid >= mRanges[ii].first) and (aid <= mRanges[ii].last)) else result := true;
end;


// returns InvalidId if there are no more free ids (or throws)
function TIdPool.alloc (dothrow: Boolean=true): LongWord;
var
  c: Integer;
begin
  if (mRangeUsed = 0) then
  begin
    // no more ids
    if dothrow then raise Exception.Create('TIdPool: no more free ids');
    result := InvalidId;
    exit;
  end;
  result := mRanges[0].first;
  // delete first range?
  if (mRanges[0].last = result) then
  begin
    for c := 1 to mRangeUsed-1 do mRanges[c-1] := mRanges[c];
    Dec(mRangeUsed);
  end
  else
  begin
    Inc(mRanges[0].first);
  end;
  Inc(mUsedIds);
  {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
end;


// returns InvalidId if there are no more free ids (or throws)
function TIdPool.alloc (aid: LongWord; dothrow: Boolean=true): LongWord;
var
  ii, c: Integer;
begin
  if (mRangeUsed = 0) then
  begin
    // no more ids
    if dothrow then raise Exception.Create('TIdPool: no more free ids');
    result := InvalidId;
    exit;
  end;
  // invalid?
  if (aid > mMaxId) then
  begin
    if dothrow then raise Exception.Create('TIdPool: cannot allocate invalid id');
    result := InvalidId;
    exit;
  end;
  // find range with this id
  ii := findRangeWithId(aid);
  if (ii < 0) or (aid < mRanges[ii].first) or (aid > mRanges[ii].last) then
  begin
    if dothrow then raise Exception.Create('TIdPool: cannot allocate already allocated id');
    result := InvalidId;
    exit;
  end;
  // always return requested id
  result := aid;
  // can we shrink range head?
  if (aid = mRanges[ii].first) then
  begin
    // yep; range with the only id?
    if (aid = mRanges[ii].last) then
    begin
      // delete this range
      for c := ii+1 to mRangeUsed-1 do mRanges[c-1] := mRanges[c];
      Dec(mRangeUsed);
    end
    else
    begin
      mRanges[ii].first := aid+1;
    end;
    Inc(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // can we shrink range tail?
  if (aid = mRanges[ii].last) then
  begin
    // yep; simply shrink, 'cause range with one id was processed in the previous `if`
    mRanges[ii].last := aid-1;
    Inc(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // split this range to two
  if (mRangeUsed+1 > Length(mRanges)) then SetLength(mRanges, Length(mRanges)+1024);
  for c := mRangeUsed downto ii+1 do mRanges[c] := mRanges[c-1];
  Inc(mRangeUsed);
  mRanges[ii].last := aid-1;
  mRanges[ii+1].first := aid+1;
  Inc(mUsedIds);
  {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
end;


// it is NOT ok to release already released id
procedure TIdPool.release (aid: LongWord);
var
  ii, c: Integer;
begin
  if (aid > mMaxId) then raise Exception.Create(Format('TIdPool: cannot release invalid id %u', [aid]));
  // no available ids?
  if (mRangeUsed = 0) then
  begin
    // just create new range
    if (Length(mRanges) = 0) then SetLength(mRanges, 64);
    mRanges[0].first := aid;
    mRanges[0].last := aid;
    mRangeUsed := 1;
    Dec(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // before first available id?
  if (aid < mRanges[0].first) then
  begin
    // can we grow first range?
    if (aid+1 = mRanges[0].first) then
    begin
      // yep
      mRanges[0].first := aid;
    end
    else
    begin
      // nope, insert new first range
      if (mRangeUsed+1 > Length(mRanges)) then SetLength(mRanges, Length(mRanges)+1024);
      assert(mRangeUsed < Length(mRanges));
      for c := mRangeUsed downto 1 do mRanges[c] := mRanges[c-1];
      Inc(mRangeUsed);
      mRanges[0].first := aid;
      mRanges[0].last := aid;
    end;
    Dec(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // after last available id?
  if (aid > mRanges[mRangeUsed-1].last) then
  begin
    // can we grow last range?
    if (aid-1 = mRanges[mRangeUsed-1].last) then
    begin
      // yep
      mRanges[mRangeUsed-1].last := aid;
    end
    else
    begin
      // nope, insert new last range
      if (mRangeUsed+1 > Length(mRanges)) then SetLength(mRanges, Length(mRanges)+1024);
      assert(mRangeUsed < Length(mRanges));
      mRanges[mRangeUsed].first := aid;
      mRanges[mRangeUsed].last := aid;
      Inc(mRangeUsed);
    end;
    Dec(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // alas, no more easy cases; find the nearest range
  ii := findRangeWithId(aid);
  if (ii < 0) then raise Exception.Create(Format('TIdPool: cannot release invalid id %u', [aid]));
  if (aid >= mRanges[ii].first) and (aid <= mRanges[ii].last) then raise Exception.Create(Format('TIdPool: cannot release unallocated id %u', [aid]));
  // ii should contain range where `first` is less than `aid`
  assert(mRanges[ii].first < aid);
  assert(mRanges[ii].last < aid);
  {$IF DEFINED(IDPOOL_DEBUG_DUMPS)}writeln('aid=', aid, '; ii=', ii, ': [', mRanges[ii].first, '-', mRanges[ii].last, ']');{$ENDIF}
  // can grow this range at the end?
  if (mRanges[ii].last+1 = aid) then
  begin
    {$IF DEFINED(IDPOOL_DEBUG_DUMPS)}writeln(' endgrow');{$ENDIF}
    // yep; can merge ranges?
    if (ii+1 < mRangeUsed) and (aid+1 = mRanges[ii+1].first) then
    begin
      // merge
      {$IF DEFINED(IDPOOL_DEBUG_DUMPS)}writeln('  endmerge');{$ENDIF}
      mRanges[ii].last := mRanges[ii+1].last;
      for c := ii+2 to mRangeUsed do mRanges[c-1] := mRanges[c];
      Dec(mRangeUsed);
    end
    else
    begin
      // change
      {$IF DEFINED(IDPOOL_DEBUG_DUMPS)}writeln('  endchange');{$ENDIF}
      {$IF DEFINED(IDPOOL_DEBUG_DUMPS)}if (ii+1 < mRangeUsed) then writeln('   ii+1=', ii+1, ': [', mRanges[ii+1].first, '-', mRanges[ii+1].last, ']');{$ENDIF}
      mRanges[ii].last := aid;
    end;
    Dec(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // can grow next range at the start?
  if (ii+1 < mRangeUsed) and (aid+1 = mRanges[ii+1].first) then
  begin
    // yep; can merge ranges?
    if (mRanges[ii].last+1 = mRanges[ii+1].first) then
    begin
      // merge
      mRanges[ii].last := mRanges[ii+1].last;
      for c := ii+2 to mRangeUsed do mRanges[c-1] := mRanges[c];
      Dec(mRangeUsed);
    end
    else
    begin
      // change
      mRanges[ii+1].first := aid;
    end;
    Dec(mUsedIds);
    {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
    exit;
  end;
  // cannot grow anything, insert empty range after ii
  if (mRangeUsed = Length(mRanges)) then SetLength(mRanges, Length(mRanges)+1024);
  for c := mRangeUsed downto ii do mRanges[c+1] := mRanges[c];
  Inc(ii);
  mRanges[ii].first := aid;
  mRanges[ii].last := aid;
  Inc(mRangeUsed);
  Dec(mUsedIds);
  {$IF DEFINED(IDPOOL_CHECKS)}check();{$ENDIF}
end;


end.
