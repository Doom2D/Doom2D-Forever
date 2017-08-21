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
{.$DEFINE RBHASH_DEBUG_INSERT}
{.$DEFINE RBHASH_DEBUG_DELETE}
{.$DEFINE RBHASH_DEBUG_COMPACT}
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
    const InitSize = {$IF DEFINED(D2F_DEBUG)}16{$ELSE}512{$ENDIF}; // *MUST* be power of two
    const LoadFactorPrc = 90; // it is ok for robin hood hashes

  public
    type THashFn = function (constref o: KeyT): LongWord;
    type TEquFn = function (constref a, b: KeyT): Boolean;
    type TIteratorFn = function (constref k: KeyT; constref v: ValueT): Boolean is nested; // return `true` to stop

  private
    type
      PEntry = ^TEntry;
      TEntry = record
        key: KeyT;
        value: ValueT;
        hash: LongWord; // key hash or 0
        nextFree: PEntry; // next free entry
      end;

  private
    hashfn: THashFn;
    equfn: TEquFn;
    mBuckets: array of PEntry; // entries, points to mEntries elements
    mBucketsUsed: Integer;
    mEntries: array of TEntry;
    {$IFDEF RBHASH_SANITY_CHECKS}
    mEntriesUsed: Integer;
    {$ENDIF}
    mFreeEntryHead: PEntry;
    mFirstEntry, mLastEntry: Integer;
    mSeed: LongWord;

  private
    function allocEntry (): PEntry;
    procedure releaseEntry (e: PEntry);

    //function distToStIdx (idx: LongWord): LongWord; inline;

    procedure putEntryInternal (swpe: PEntry);

    function getCapacity (): Integer; inline;

  public
    constructor Create (ahashfn: THashFn; aequfn: TEquFn);
    destructor Destroy (); override;

    procedure clear ();
    procedure reset (); // don't shrink buckets

    procedure rehash ();
    procedure compact (); // call this instead of `rehash()` after alot of deletions

    function get (constref akey: KeyT; out rval: ValueT): Boolean; // `true`: found
    function put (constref akey: KeyT; constref aval: ValueT): Boolean; // `true`: replaced
    function has (constref akey: KeyT): Boolean; // `true`: found
    function del (constref akey: KeyT): Boolean; // `true`: deleted

    //WARNING! don't modify table in iterator (queries are ok, though)
    function forEach (it: TIteratorFn): Boolean;

    property count: Integer read mBucketsUsed;
    property capacity: Integer read getCapacity;
  end;


type
  TJoaatHasher = record
  private
    seed: LongWord; // initial seed value; MUST BE FIRST
    hash: LongWord; // current value

  public
    constructor Create (aseed: LongWord);

    procedure reset (); inline; overload;
    procedure reset (aseed: LongWord); inline; overload;

    procedure put (const buf; len: LongWord);

    // current hash value
    // you can continue putting data, as this is not destructive
    function value: LongWord; inline;
  end;


type
  THashIntInt = specialize THashBase<Integer, Integer>;

function hashNewIntInt (): THashIntInt;


function u32Hash (a: LongWord): LongWord; inline;
function fnvHash (const buf; len: LongWord): LongWord;
function joaatHash (const buf; len: LongWord): LongWord;

function nextPOT (x: LongWord): LongWord; inline;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
{$PUSH}
{$RANGECHECKS OFF}
function nextPOT (x: LongWord): LongWord; inline;
begin
  result := x;
  result := result or (result shr 1);
  result := result or (result shr 2);
  result := result or (result shr 4);
  result := result or (result shr 8);
  result := result or (result shr 16);
  // already pot?
  if (x <> 0) and ((x and (x-1)) = 0) then result := result and (not (result shr 1)) else result += 1;
end;
{$POP}


// ////////////////////////////////////////////////////////////////////////// //
function hiiequ (constref a, b: Integer): Boolean; begin result := (a = b); end;

{$PUSH}
{$RANGECHECKS OFF}
function hiihash (constref k: Integer): LongWord;
begin
  result := k;
  result -= (result shl 6);
  result := result xor (result shr 17);
  result -= (result shl 9);
  result := result xor (result shl 4);
  result -= (result shl 3);
  result := result xor (result shl 10);
  result := result xor (result shr 15);
end;
{$POP}


function hashNewIntInt (): THashIntInt;
begin
  result := THashIntInt.Create(hiihash, hiiequ);
end;


// ////////////////////////////////////////////////////////////////////////// //
{$PUSH}
{$RANGECHECKS OFF}
constructor TJoaatHasher.Create (aseed: LongWord);
begin
  reset(aseed);
end;


procedure TJoaatHasher.reset (); inline; overload;
begin
  hash := seed;
end;


procedure TJoaatHasher.reset (aseed: LongWord); inline; overload;
begin
  seed := aseed;
  hash := aseed;
end;


procedure TJoaatHasher.put (const buf; len: LongWord);
var
  bytes: PByte;
  h: LongWord;
begin
  if (len < 1) then exit;
  bytes := PByte(@buf);
  h := hash;
  while (len > 0) do
  begin
    h += bytes^;
    h += (h shl 10);
    h := h xor (h shr 6);
    Dec(len);
    Inc(bytes);
  end;
  hash := h;
end;


function TJoaatHasher.value: LongWord; inline;
begin
  result := hash;
  result += (result shl 3);
  result := result xor (result shr 11);
  result += (result shl 15);
end;
{$POP}


function joaatHash (const buf; len: LongWord): LongWord;
var
  h: TJoaatHasher;
begin
  h := TJoaatHasher.Create(0);
  h.put(buf, len);
  result := h.value;
end;


// ////////////////////////////////////////////////////////////////////////// //
{$PUSH}
{$RANGECHECKS OFF}
// fnv-1a: http://www.isthe.com/chongo/tech/comp/fnv/
function fnvHash (const buf; len: LongWord): LongWord;
var
  b: PByte;
begin
  b := @buf;
  result := 2166136261; // fnv offset basis
  while (len > 0) do
  begin
    result := result xor b^;
    result := result*16777619; // 32-bit fnv prime
    Inc(b);
    Dec(len);
  end;
end;
{$POP}


{$PUSH}
{$RANGECHECKS OFF}
function u32Hash (a: LongWord): LongWord; inline;
begin
  result := a;
  result -= (result shl 6);
  result := result xor (result shr 17);
  result -= (result shl 9);
  result := result xor (result shl 4);
  result -= (result shl 3);
  result := result xor (result shl 10);
  result := result xor (result shr 15);
end;
{$POP}


// ////////////////////////////////////////////////////////////////////////// //
constructor THashBase.Create (ahashfn: THashFn; aequfn: TEquFn);
begin
  if not assigned(ahashfn) then raise Exception.Create('cannot create hash without hash function');
  if not assigned(aequfn) then raise Exception.Create('cannot create hash without equality function');

  hashfn := ahashfn;
  equfn := aequfn;
  mSeed := u32Hash($29a);

  clear();
end;


destructor THashBase.Destroy ();
begin
  mBuckets := nil;
  mEntries := nil;
  inherited;
end;


procedure THashBase.clear ();
var
  idx: Integer;
begin
  SetLength(mBuckets, InitSize);
  for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;

  SetLength(mEntries, Length(mBuckets));
  for idx := 0 to High(mEntries)-1 do
  begin
    mEntries[idx].hash := 0;
    mEntries[idx].nextFree := @mEntries[idx+1]; //idx+1;
  end;
  mEntries[High(mEntries)].hash := 0;
  mEntries[High(mEntries)].nextFree := nil;

  mBucketsUsed := 0;
  {$IFDEF RBHASH_SANITY_CHECKS}
  mEntriesUsed := 0;
  {$ENDIF}
  mFreeEntryHead := @mEntries[0];
  mFirstEntry := -1;
  mLastEntry := -1;
end;


procedure THashBase.reset ();
var
  idx: Integer;
begin
  if (mBucketsUsed > 0) then
  begin
    for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;
    for idx := 0 to High(mEntries)-1 do
    begin
      mEntries[idx].hash := 0;
      mEntries[idx].nextFree := @mEntries[idx+1]; //idx+1;
    end;
    mEntries[High(mEntries)].hash := 0;
    mEntries[High(mEntries)].nextFree := nil;

    mBucketsUsed := 0;
    {$IFDEF RBHASH_SANITY_CHECKS}
    mEntriesUsed := 0;
    {$ENDIF}
    mFreeEntryHead := @mEntries[0];
    mFirstEntry := -1;
    mLastEntry := -1;
  end;
end;


function THashBase.getCapacity (): Integer; inline; begin result := Length(mBuckets); end;


function THashBase.allocEntry (): PEntry;
var
  idx: Integer;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mFreeEntryHead = nil) then raise Exception.Create('internal error in hash entry allocator (0)');
  if (mFreeEntryHead.hash <> 0) then raise Exception.Create('internal error in hash entry allocator (1)');
  {$ENDIF}
  result := mFreeEntryHead;
  mFreeEntryHead := result.nextFree;
  {$IFDEF RBHASH_SANITY_CHECKS}
  Inc(mEntriesUsed);
  {$ENDIF}
  result.nextFree := nil; // just in case
  // fix mFirstEntry and mLastEntry
  idx := Integer((PtrUInt(result)-PtrUInt(@mEntries[0])) div sizeof(mEntries[0]));
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (idx < 0) or (idx > High(mEntries)) then raise Exception.Create('internal error in hash entry allocator (invalid entry address)');
  if (result <> @mEntries[idx]) then raise Exception.Create('internal error in hash entry allocator (wtf?!)');
  {$ENDIF}
  if (mFirstEntry < 0) or (idx < mFirstEntry) then mFirstEntry := idx;
  if (idx > mLastEntry) then mLastEntry := idx;
end;


procedure THashBase.releaseEntry (e: PEntry);
var
  cidx, idx: Integer;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mEntriesUsed = 0) then raise Exception.Create('internal error in hash entry allocator');
  if (mEntriesUsed <> mBucketsUsed) then raise Exception.Create('internal error in hash entry allocator (entry/bucket count mismatch)');
  if (e = nil) then raise Exception.Create('internal error in hash entry allocator (trying to release nil entry)');
  if (e.hash = 0) then raise Exception.Create('internal error in hash entry allocator (trying to release unallocated entry)');
  {$ENDIF}
  idx := Integer((PtrUInt(e)-PtrUInt(@mEntries[0])) div sizeof(mEntries[0]));
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (idx < 0) or (idx > High(mEntries)) then raise Exception.Create('internal error in hash entry allocator (invalid entry address)');
  if (e <> @mEntries[idx]) then raise Exception.Create('internal error in hash entry allocator (wtf?!)');
  {$ENDIF}
  e.hash := 0;
  e.nextFree := mFreeEntryHead;
  mFreeEntryHead := e; //idx;
  {$IFDEF RBHASH_SANITY_CHECKS}
  Dec(mEntriesUsed);
  {$ENDIF}
  // fix mFirstEntry and mLastEntry
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mFirstEntry < 0) or (mLastEntry < 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 0)');
  {$ENDIF}
  if (mFirstEntry = mLastEntry) then
  begin
    {$IFDEF RBHASH_SANITY_CHECKS}
    if (mEntriesUsed <> 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 1)');
    {$ENDIF}
    mFirstEntry := -1;
    mLastEntry := -1;
  end
  else
  begin
    {$IFDEF RBHASH_SANITY_CHECKS}
    if (mEntriesUsed = 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 2)');
    {$ENDIF}
    // fix first entry index
    if (idx = mFirstEntry) then
    begin
      cidx := idx+1;
      while (mEntries[cidx].hash = 0) do Inc(cidx);
      {$IFDEF RBHASH_SANITY_CHECKS}
      if (cidx > High(mEntries)) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 3)');
      {$ENDIF}
      mFirstEntry := cidx;
    end;
    // fix last entry index
    if (idx = mLastEntry) then
    begin
      cidx := idx-1;
      while (mEntries[cidx].hash = 0) do Dec(cidx);
      {$IFDEF RBHASH_SANITY_CHECKS}
      if (cidx < 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 3)');
      {$ENDIF}
      mLastEntry := cidx;
    end;
  end;
end;


(*
function THashBase.distToStIdx (idx: LongWord): LongWord; inline;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  assert(idx < Length(mBuckets));
  assert(mBuckets[idx] <> nil);
  {$ENDIF}
  result := mBuckets[idx].hash and High(mBuckets);
  if (result <= idx) then result := idx-result else result := idx+(Length(mBuckets)-result);
end;
*)


function THashBase.has (constref akey: KeyT): Boolean;
var
  khash, idx: LongWord;
  dist, pdist: LongWord;
  bhigh: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then exit;

  bhigh := High(mBuckets);
  khash := hashfn(akey) xor mSeed; if (khash = 0) then khash := $29a;
  idx := khash and bhigh;
  if (mBuckets[idx] = nil) then exit;

  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then break;
    //pdist := distToStIdx(idx);
    pdist := mBuckets[idx].hash and bhigh;
    if (pdist <= idx) then pdist := idx-pdist else pdist := idx+((bhigh+1)-pdist);
    //
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
    if result then break;
    idx := (idx+1) and bhigh;
  end;
end;


function THashBase.get (constref akey: KeyT; out rval: ValueT): Boolean;
var
  khash, idx: LongWord;
  dist, pdist: LongWord;
  bhigh: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then begin rval := Default(ValueT); exit; end;

  bhigh := High(mBuckets);
  khash := hashfn(akey) xor mSeed; if (khash = 0) then khash := $29a;
  idx := khash and bhigh;
  if (mBuckets[idx] = nil) then begin rval := Default(ValueT); exit; end;

  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then break;
    //pdist := distToStIdx(idx);
    pdist := mBuckets[idx].hash and bhigh;
    if (pdist <= idx) then pdist := idx-pdist else pdist := idx+((bhigh+1)-pdist);
    //
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
    if result then
    begin
      rval := mBuckets[idx].value;
      break;
    end;
    idx := (idx+1) and bhigh;
  end;

  if not result then rval := Default(ValueT); // just in case
end;


procedure THashBase.putEntryInternal (swpe: PEntry);
var
  idx, dist, pcur, pdist: LongWord;
  tmpe: PEntry; // current entry to swap (or nothing)
  bhigh: LongWord;
begin
  bhigh := High(mBuckets);
  idx := swpe.hash and bhigh;
  {$IFDEF RBHASH_DEBUG_INSERT}writeln('inserting key ', swpe.key, '; value=', swpe.value, '; wantidx=', idx, '; bhigh=', bhigh);{$ENDIF}
  pcur := 0;
  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then
    begin
      // put entry
      {$IFDEF RBHASH_DEBUG_INSERT}writeln('  inserted to ', idx);{$ENDIF}
      mBuckets[idx] := swpe;
      Inc(mBucketsUsed);
      break;
    end;
    //pdist := distToStIdx(idx);
    pdist := mBuckets[idx].hash and bhigh;
    if (pdist <= idx) then pdist := idx-pdist else pdist := idx+((bhigh+1)-pdist);
    //
    if (pcur > pdist) then
    begin
      // swapping the current bucket with the one to insert
      tmpe := mBuckets[idx];
      mBuckets[idx] := swpe;
      swpe := tmpe;
      pcur := pdist;
    end;
    idx := (idx+1) and bhigh;
    Inc(pcur);
  end;
end;


function THashBase.put (constref akey: KeyT; constref aval: ValueT): Boolean;
var
  khash, idx, dist, pdist: LongWord;
  swpe: PEntry = nil; // current entry to swap (or nothing)
  bhigh: LongWord;
  newsz, eidx: Integer;
begin
  result := false;

  bhigh := High(mBuckets);
  khash := hashfn(akey) xor mSeed; if (khash = 0) then khash := $29a;
  idx := khash and bhigh;

  // check if we already have this key
  if (mBucketsUsed <> 0) and (mBuckets[idx] <> nil) then
  begin
    for dist := 0 to bhigh do
    begin
      if (mBuckets[idx] = nil) then break;
      //pdist := distToStIdx(idx);
      pdist := mBuckets[idx].hash and bhigh;
      if (pdist <= idx) then pdist := idx-pdist else pdist := idx+((bhigh+1)-pdist);
      //
      if (dist > pdist) then break;
      result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
      if result then
      begin
        // replace element
        //mBuckets[idx].key := akey;
        mBuckets[idx].value := aval;
        exit;
      end;
      idx := (idx+1) and bhigh;
    end;
  end;

  // need to resize hash?
  if (mBucketsUsed >= (bhigh+1)*LoadFactorPrc div 100) then
  begin
    newsz := Length(mBuckets);
    if (Length(mEntries) <> newsz) then raise Exception.Create('internal error in hash table (resize)');
    if (newsz <= 1024*1024*1024) then newsz *= 2 else raise Exception.Create('hash table too big');
    {$IFDEF RBHASH_DEBUG_RESIZE}
    writeln('resizing hash; used=', mBucketsUsed, '; total=', (bhigh+1), '; maxload=', (bhigh+1)*LoadFactorPrc div 100, '; newsz=', newsz);
    {$ENDIF}
    SetLength(mBuckets, newsz);
    // resize entries array
    eidx := Length(mEntries);
    SetLength(mEntries, newsz);
    while (eidx < Length(mEntries)) do begin mEntries[eidx].hash := 0; Inc(eidx); end;
    // mFreeEntryHead will be fixed in `rehash()`
    // reinsert entries
    rehash();
    // as seed was changed, recalc hash
    khash := hashfn(akey) xor mSeed; if (khash = 0) then khash := $29a;
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
  khash, idx, idxnext, pdist, dist: LongWord;
  bhigh: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then exit;

  bhigh := High(mBuckets);
  khash := hashfn(akey) xor mSeed; if (khash = 0) then khash := $29a;
  idx := khash and bhigh;

  // find key
  if (mBuckets[idx] = nil) then exit; // no key
  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then break;
    //pdist := distToStIdx(idxcur);
    pdist := mBuckets[idx].hash and bhigh;
    if (pdist <= idx) then pdist := idx-pdist else pdist := idx+((bhigh+1)-pdist);
    //
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and equfn(mBuckets[idx].key, akey);
    if result then break;
    idx := (idx+1) and bhigh;
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
  writeln('del: key ', akey, ': found at ', idx, '; ek=', mBuckets[idx].key, '; ev=', mBuckets[idx].value);
  {$ENDIF}
  releaseEntry(mBuckets[idx]);

  idxnext := (idx+1) and bhigh;
  for dist := 0 to bhigh do
  begin
    {$IFDEF RBHASH_DEBUG_DELETE}
    writeln(' dist=', dist, '; idx=', idx, '; idxnext=', idxnext, '; ce=', (mBuckets[idx] <> nil), '; ne=', (mBuckets[idxnext] <> nil));
    {$ENDIF}
    if (mBuckets[idxnext] = nil) then begin {$IFDEF RBHASH_DEBUG_DELETE}writeln('  idxnext nil');{$ENDIF} mBuckets[idx] := nil; break; end;
    //pdist := distToStIdx(idxnext);
    pdist := mBuckets[idxnext].hash and bhigh;
    if (pdist <= idxnext) then pdist := idxnext-pdist else pdist := idxnext+((bhigh+1)-pdist);
    //
    if (pdist = 0) then begin {$IFDEF RBHASH_DEBUG_DELETE}writeln('  pdist is zero');{$ENDIF} mBuckets[idx] := nil; break; end;
    {$IFDEF RBHASH_DEBUG_DELETE}writeln('  pdist=', pdist);{$ENDIF}
    mBuckets[idx] := mBuckets[idxnext];
    idx := (idx+1) and bhigh;
    idxnext := (idxnext+1) and bhigh;
  end;

  Dec(mBucketsUsed);
end;


procedure THashBase.rehash ();
var
  idx: Integer;
  lastfree: PEntry;
  e: PEntry = nil; // shut up, fpc!
  {$IFDEF RBHASH_SANITY_CHECKS}
  cnt: Integer = 0;
  {$ENDIF}
begin
  // change seed, to minimize pathological cases
  if (mSeed = 0) then mSeed := $29a;
  mSeed := u32Hash(mSeed);
  // clear buckets
  for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;
  mBucketsUsed := 0;
  // reinsert entries
  mFreeEntryHead := nil;
  lastfree := nil;
  for idx := 0 to High(mEntries) do
  begin
    e := @mEntries[idx];
    if (e.hash <> 0) then
    begin
      {$IFDEF RBHASH_SANITY_CHECKS}
      if (e.nextFree <> nil) then raise Exception.Create('internal error in rehash: inconsistent');
      if (cnt = 0) and (idx <> mFirstEntry) then raise Exception.Create('internal error in rehash: inconsistent (1)');
      Inc(cnt);
      if (cnt = mBucketsUsed) and (idx <> mLastEntry) then raise Exception.Create('internal error in rehash: inconsistent (2)');
      {$ENDIF}
      e.hash := hashfn(e.key) xor mSeed; if (e.hash = 0) then e.hash := $29a;
      putEntryInternal(e);
    end
    else
    begin
      if (lastfree <> nil) then lastfree.nextFree := e else mFreeEntryHead := e;
      lastfree := e;
    end;
  end;
  if (lastfree <> nil) then e.nextFree := nil;
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (cnt <> mBucketsUsed) then raise Exception.Create('internal error in hash table resize (invalid first/last range; 0)');
  if (cnt <> mEntriesUsed) then raise Exception.Create('internal error in hash table resize (invalid first/last range; 1)');
  {$ENDIF}
end;


procedure THashBase.compact ();
var
  newsz, didx, f: Integer;
  {$IFDEF RBHASH_SANITY_CHECKS}
  cnt: Integer;
  {$ENDIF}
begin
  newsz := nextPOT(LongWord(mBucketsUsed));
  if (newsz >= 1024*1024*1024) then exit;
  if (newsz*2 >= Length(mBuckets)) then exit;
  if (newsz*2 < 128) then exit;
  {$IFDEF RBHASH_DEBUG_COMPACT}writeln('compacting; used=', mBucketsUsed, '; oldsizePOT=', newsz, '; newsize=', newsz*2);{$ENDIF}
  newsz *= 2;
  // move all entries to top
  if (mFirstEntry >= 0) then
  begin
    {$IFDEF RBHASH_SANITY_CHECKS}
    if (mBucketsUsed < 1) then raise Exception.Create('internal error in hash table (invalid bucket count; 0)');
    {$ENDIF}
    didx := 0;
    while (didx < Length(mEntries)) do if (mEntries[didx].hash <> 0) then Inc(didx) else break;
    f := didx+1;
    // copy entries
    while true do
    begin
      if (mEntries[f].hash <> 0) then
      begin
        {$IFDEF RBHASH_SANITY_CHECKS}
        if (didx >= f) then raise Exception.Create('internal error in hash: inconsistent');
        {$ENDIF}
        mEntries[didx] := mEntries[f];
        mEntries[f].hash := 0;
        Inc(didx);
        if (f = mLastEntry) then break;
        while (didx < Length(mEntries)) do if (mEntries[didx].hash <> 0) then Inc(didx) else break;
      end;
      Inc(f);
    end;
    {$IFDEF RBHASH_SANITY_CHECKS}
    if (didx <> mBucketsUsed) then raise Exception.Create('internal error in hash table (invalid first/last range; 1)');
    {$ENDIF}
    mFirstEntry := 0;
    mLastEntry := mBucketsUsed-1;
    {$IFDEF RBHASH_SANITY_CHECKS}
    cnt := 0;
    for f := mFirstEntry to mLastEntry do
    begin
      if (mEntries[f].hash = 0) then raise Exception.Create('internal error in hash table (invalid first/last range; 2)');
      Inc(cnt);
    end;
    if (cnt <> mBucketsUsed) then raise Exception.Create('internal error in hash table (invalid first/last range; 3)');
    if (cnt <> mEntriesUsed) then raise Exception.Create('internal error in hash table (invalid first/last range; 4)');
    for f := mLastEntry+1 to High(mEntries) do
    begin
      if (mEntries[f].hash <> 0) then raise Exception.Create('internal error in hash table (invalid first/last range; 5)');
    end;
    {$ENDIF}
  end
  else
  begin
    {$IFDEF RBHASH_SANITY_CHECKS}
    if (mBucketsUsed <> 0) then raise Exception.Create('internal error in hash table (invalid bucket count; 1)');
    {$ENDIF}
  end;
  // shrink
  SetLength(mBuckets, newsz);
  SetLength(mEntries, newsz);
  // mFreeEntryHead will be fixed in `rehash()`
  // reinsert entries
  rehash();
end;


function THashBase.forEach (it: TIteratorFn): Boolean;
var
  i: Integer;
begin
  result := false;
  if not assigned(it) then exit;
  i := mFirstEntry;
  if (i < 0) then exit;
  while (i <= mLastEntry) do
  begin
    if (mEntries[i].hash <> 0) then
    begin
      result := it(mEntries[i].key, mEntries[i].value);
      if result then exit;
    end;
    Inc(i);
  end;
end;


end.
