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
{$INCLUDE a_modes.inc}
{.$DEFINE RBHASH_DEBUG_RESIZE}
{.$DEFINE RBHASH_DEBUG_DELETE}
{$IF DEFINED(D2F_DEBUG)}
  {$DEFINE RBHASH_SANITY_CHECKS}
{$ENDIF}
// hash table (robin hood)
unit hashtable;

interface


type
  // WARNING! don't put structures into hash, use ponters or ids!
  generic THashBase<KeyT, ValueT> = class(TObject)
  private
    const InitSize = {$IF DEFINED(D2F_DEBUG)}16{$ELSE}512{$ENDIF};
    const LoadFactorPrc = 90; // it is ok for robin hood hashes

  public
    type THashFn = function (constref o: KeyT): LongWord;
    type TEquFn = function (constref a, b: KeyT): Boolean;

  private
    type
      PEntry = ^TEntry;
      TEntry = record
        key: KeyT;
        value: ValueT;
        hash: LongWord; // key hash or 0
        nextFree: Integer;
      end;

  private
    hashfn: THashFn;
    equfn: TEquFn;
    mBuckets: array of PEntry; // entries, points to mEntries elements
    mBucketsUsed: Integer;
    mEntries: array of TEntry;
    mEntriesUsed: Integer;
    mFreeEntryHead: Integer;

  private
    function allocEntry (): PEntry;
    procedure releaseEntry (e: PEntry);

    function distToStIdx (idx: LongWord): LongWord; inline;

    procedure putEntryInternal (swpe: PEntry);

  public
    constructor Create (ahashfn: THashFn; aequfn: TEquFn);
    destructor Destroy (); override;

    procedure clear ();

    procedure rehash ();

    function get (constref akey: KeyT; out rval: ValueT): Boolean; // `true`: found
    function put (constref akey: KeyT; constref aval: ValueT): Boolean; // `true`: replaced
    function has (constref akey: KeyT): Boolean; // `true`: found
    function del (constref akey: KeyT): Boolean; // `true`: deleted

    property count: Integer read mBucketsUsed;
  end;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
constructor THashBase.Create (ahashfn: THashFn; aequfn: TEquFn);
begin
  if not assigned(ahashfn) then raise Exception.Create('cannot create hash without hash function');
  if not assigned(aequfn) then raise Exception.Create('cannot create hash without equality function');

  hashfn := ahashfn;
  equfn := aequfn;

  clear();
end;


destructor THashBase.Destroy ();
begin
  mBuckets := nil;
  mEntries := nil;
  inherited;
end;


function THashBase.allocEntry (): PEntry;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mFreeEntryHead = -1) then raise Exception.Create('internal error in hash entry allocator (0)');
  if (mEntries[mFreeEntryHead].hash <> 0) then raise Exception.Create('internal error in hash entry allocator (1)');
  {$ENDIF}
  result := @mEntries[mFreeEntryHead];
  mFreeEntryHead := result.nextFree;
  Inc(mEntriesUsed);
  result.nextFree := -1;
end;


procedure THashBase.releaseEntry (e: PEntry);
var
  idx: LongWord;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mEntriesUsed = 0) then raise Exception.Create('internal error in hash entry allocator');
  if (e = nil) then raise Exception.Create('internal error in hash entry allocator (trying to release nil entry)');
  if (e.nextFree <> -1) or (e.hash = 0) then raise Exception.Create('internal error in hash entry allocator (trying to release unallocated entry)');
  {$ENDIF}
  idx := LongWord((PtrUInt(e)-PtrUInt(@mEntries[0])) div sizeof(mEntries[0]));
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (idx >= Length(mEntries)) then raise Exception.Create('internal error in hash entry allocator (invalid calculated index)');
  {$ENDIF}
  e.hash := 0;
  e.nextFree := mFreeEntryHead;
  mFreeEntryHead := idx;
  Dec(mEntriesUsed);
end;


function THashBase.distToStIdx (idx: LongWord): LongWord; inline;
var
  stidx: LongWord;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  assert(idx < Length(mBuckets));
  assert(mBuckets[idx] <> nil);
  {$ENDIF}
  stidx := mBuckets[idx].hash mod Length(mBuckets);
  if (stidx <= idx) then result := idx-stidx else result := idx+(Length(mBuckets)-stidx);
end;


function THashBase.has (constref akey: KeyT): Boolean;
var
  khash, idx: LongWord;
  dist, pdist: LongWord;
  blen: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then exit;

  blen := Length(mBuckets);
  khash := hashfn(akey); if (khash = 0) then khash := $29a;
  idx := khash mod blen;
  if (mBuckets[idx] = nil) then exit;

  for dist := 0 to blen-1 do
  begin
    if (mBuckets[idx] = nil) then break;
    pdist := distToStIdx(idx);
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
    if result then break;
    idx := (idx+1) mod blen;
  end;
end;


function THashBase.get (constref akey: KeyT; out rval: ValueT): Boolean;
var
  khash, idx: LongWord;
  dist, pdist: LongWord;
  blen: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then begin rval := Default(ValueT); exit; end;

  blen := Length(mBuckets);
  khash := hashfn(akey); if (khash = 0) then khash := $29a;
  idx := khash mod blen;
  if (mBuckets[idx] = nil) then begin rval := Default(ValueT); exit; end;

  for dist := 0 to blen-1 do
  begin
    if (mBuckets[idx] = nil) then break;
    pdist := distToStIdx(idx);
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
    if result then
    begin
      rval := mBuckets[idx].value;
      break;
    end;
    idx := (idx+1) mod blen;
  end;

  if not result then rval := Default(ValueT); // just in case
end;


procedure THashBase.putEntryInternal (swpe: PEntry);
var
  idx, dist, pcur, pdist: LongWord;
  tmpe: PEntry; // current entry to swap (or nothing)
  blen: LongWord;
begin
  blen := Length(mBuckets);
  idx := swpe.hash mod blen;
  pcur := 0;
  for dist := 0 to blen-1 do
  begin
    if (mBuckets[idx] = nil) then
    begin
      // put entry
      mBuckets[idx] := swpe;
      Inc(mBucketsUsed);
      break;
    end;
    pdist := distToStIdx(idx);
    if (pcur > pdist) then
    begin
      // swapping the current bucket with the one to insert
      tmpe := mBuckets[idx];
      mBuckets[idx] := swpe;
      swpe := tmpe;
      pcur := pdist;
    end;
    idx := (idx+1) mod blen;
    Inc(pcur);
  end;
end;


function THashBase.put (constref akey: KeyT; constref aval: ValueT): Boolean;
var
  khash, stidx, idx, dist, pdist: LongWord;
  swpe: PEntry = nil; // current entry to swap (or nothing)
  blen: LongWord;
  newsz, eidx: Integer;
begin
  result := false;

  blen := Length(mBuckets);
  khash := hashfn(akey); if (khash = 0) then khash := $29a;
  stidx := khash mod blen;

  // check if we already have this key
  idx := stidx;
  if (mBucketsUsed <> 0) and (mBuckets[idx] <> nil) then
  begin
    for dist := 0 to blen-1 do
    begin
      if (mBuckets[idx] = nil) then break;
      pdist := distToStIdx(idx);
      if (dist > pdist) then break;
      result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
      if result then
      begin
        // replace element
        //mBuckets[idx].key := akey;
        mBuckets[idx].value := aval;
        exit;
      end;
      idx := (idx+1) mod blen;
    end;
  end;

  // need to resize hash?
  if (mBucketsUsed >= blen*LoadFactorPrc div 100) then
  begin
    newsz := Length(mBuckets);
    if (Length(mEntries) <> newsz) then raise Exception.Create('internal error in hash table (resize)');
    if (newsz <= 1024*1024*1024) then newsz *= 2 else raise Exception.Create('hash table too big');
    {$IFDEF RBHASH_DEBUG_RESIZE}
    writeln('resizing hash; used=', mBucketsUsed, '; total=', blen, '; maxload=', blen*LoadFactorPrc div 100, '; newsz=', newsz);
    {$ENDIF}
    SetLength(mBuckets, newsz);
    // resize entries array
    eidx := newsz;
    SetLength(mEntries, newsz);
    while (eidx < Length(mEntries)) do begin mEntries[eidx].hash := 0; Inc(eidx); end;
    // mFreeEntryHead will be fixed in `rehash()`
    // reinsert entries
    rehash();
  end;

  // create new entry
  swpe := allocEntry();
  swpe.key := akey;
  swpe.value := aval;
  swpe.hash := khash;

  putEntryInternal(swpe);
end;


// see http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
function THashBase.del (constref akey: KeyT): Boolean;
var
  khash, stidx, idxcur, idxnext, pdist, dist: LongWord;
  blen: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then exit;

  blen := Length(mBuckets);
  khash := hashfn(akey); if (khash = 0) then khash := $29a;
  stidx := khash mod blen;

  // find key
  if (mBuckets[stidx] = nil) then exit; // no key

  idxcur := stidx;
  for dist := 0 to blen-1 do
  begin
    if (mBuckets[idxcur] = nil) then break;
    pdist := distToStIdx(idxcur);
    if (dist > pdist) then break;
    result := (mBuckets[idxcur].hash = khash) and equfn(mBuckets[idxcur].key, akey);
    if result then break;
    idxcur := (idxcur+1) mod blen;
  end;

  if not result then
  begin
    // key not found
    {$IFDEF RBHASH_DEBUG_DELETE}
    writeln('del: key ', akey, ': not found');
    {$ENDIF}
    exit;
  end;

  {$IFDEF RBHASH_DEBUG_DELETE}
  writeln('del: key ', akey, ': found at ', idxcur, '(', stidx, '); ek=', mBuckets[idxcur].key, '; ev=', mBuckets[idxcur].value);
  {$ENDIF}
  releaseEntry(mBuckets[idxcur]);

  idxnext := (idxcur+1) mod blen;
  for dist := 0 to blen-1 do
  begin
    {$IFDEF RBHASH_DEBUG_DELETE}
    writeln(' dist=', dist, '; idxcur=', idxcur, '; idxnext=', idxnext, '; ce=', (mBuckets[idxcur] <> nil), '; ne=', (mBuckets[idxnext] <> nil));
    {$ENDIF}
    if (mBuckets[idxnext] = nil) then begin {$IFDEF RBHASH_DEBUG_DELETE}writeln('  idxnext nil');{$ENDIF} mBuckets[idxcur] := nil; break; end;
    pdist := distToStIdx(idxnext);
    if (pdist = 0) then begin {$IFDEF RBHASH_DEBUG_DELETE}writeln('  pdist is zero');{$ENDIF} mBuckets[idxcur] := nil; break; end;
    {$IFDEF RBHASH_DEBUG_DELETE}writeln('  pdist=', pdist);{$ENDIF}
    mBuckets[idxcur] := mBuckets[idxnext];
    idxcur := (idxcur+1) mod blen;
    idxnext := (idxnext+1) mod blen;
  end;

  Dec(mBucketsUsed);
end;


procedure THashBase.clear ();
var
  idx: Integer;
begin
  SetLength(mBuckets, InitSize);
  for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;

  SetLength(mEntries, Length(mBuckets));
  for idx := 0 to High(mEntries) do
  begin
    mEntries[idx].hash := 0;
    mEntries[idx].nextFree := idx+1;
  end;
  mEntries[High(mEntries)].nextFree := -1;

  mBucketsUsed := 0;
  mEntriesUsed := 0;
  mFreeEntryHead := 0;
end;


procedure THashBase.rehash ();
var
  idx, lastfree: Integer;
  e: PEntry;
begin
  // clear buckets
  for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;
  mBucketsUsed := 0;
  // reinsert entries
  mFreeEntryHead := -1;
  lastfree := -1;
  for idx := 0 to High(mEntries) do
  begin
    e := @mEntries[idx];
    if (e.hash <> 0) then
    begin
      putEntryInternal(e);
    end
    else
    begin
      if (lastfree <> -1) then mEntries[lastfree].nextFree := idx else mFreeEntryHead := idx;
      lastfree := idx;
    end;
  end;
end;


end.
