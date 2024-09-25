(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

{$INCLUDE a_modes.inc}
{.$DEFINE RBHASH_DEBUG_RESIZE}
{.$DEFINE RBHASH_DEBUG_INSERT}
{.$DEFINE RBHASH_DEBUG_DELETE}
{.$DEFINE RBHASH_DEBUG_COMPACT}
{$IF DEFINED(D2F_DEBUG)}
  {.$DEFINE RBHASH_SANITY_CHECKS}
{$ENDIF}
// hash table (robin hood)
unit hashtable;

interface

(*
 * HashObjT: class that contains class methods:
 *   class function hash (const[ref] k: KeyT): LongWord;
 *   class function equ (const[ref] a, b: KeyT): Boolean;
 *   class procedure freekey (var k: KeyT); // this may free key
 *)
type
  // WARNING! don't put structures into hash, use ponters or ids!
  generic THashBase<KeyT, ValueT, HashObjT> = class(TObject)
  private
    const InitSize = {$IF DEFINED(RBHASH_SANITY_CHECKS)}16{$ELSE}256{$ENDIF}; // *MUST* be power of two
    const LoadFactorPrc = 90; // it is ok for robin hood hashes

  public
    type
      PEntry = ^TEntry;
      TEntry = record
      public
        key: KeyT;
        value: ValueT;
      private
        hash: LongWord; // key hash or 0
        nextFree: PEntry; // next free entry
      private
        function getEmpty (): Boolean; inline;
      public
        property empty: Boolean read getEmpty;
        property keyhash: LongWord read hash; // cannot be 0
      end;

    type TFreeValueFn = procedure (var v: ValueT); // this may free value
    type TIteratorFn = function (constref k: KeyT; constref v: ValueT): Boolean is nested; // return `true` to stop
    type TIteratorExFn = function (constref k: KeyT; constref v: ValueT; keyhash: LongWord): Boolean is nested; // return `true` to stop

  private
    type
      TEntryArray = array of TEntry;

  public
    type
      TValEnumerator = record
      private
        mEntries: TEntryArray;
        mFirstEntry, mLastEntry, cur: Integer;
      public
        constructor Create (const aents: TEntryArray; afirst, alast: Integer);
        function MoveNext (): Boolean; inline;
        function getCurrent (): ValueT; inline;
        function GetEnumerator (): TValEnumerator; inline;
        property Current: ValueT read getCurrent;
      end;

      TKeyEnumerator = record
      private
        mEntries: TEntryArray;
        mFirstEntry, mLastEntry, cur: Integer;
      public
        constructor Create (const aents: TEntryArray; afirst, alast: Integer);
        function MoveNext (): Boolean; inline;
        function getCurrent (): KeyT; inline;
        function GetEnumerator (): TKeyEnumerator; inline;
        property Current: KeyT read getCurrent;
      end;

      TKeyValEnumerator = record
      private
        mEntries: TEntryArray;
        mFirstEntry, mLastEntry, cur: Integer;
      public
        constructor Create (const aents: TEntryArray; afirst, alast: Integer);
        function MoveNext (): Boolean; inline;
        function getCurrent (): PEntry; inline;
        function GetEnumerator (): TKeyValEnumerator; inline;
        property Current: PEntry read getCurrent;
      end;

  private
    freevalfn: TFreeValueFn;
    mBuckets: array of PEntry; // entries, points to mEntries elements
    mBucketsUsed: Integer;
    mEntries: TEntryArray;
    {$IFDEF RBHASH_SANITY_CHECKS}
    mEntriesUsed: Integer;
    {$ENDIF}
    mFreeEntryHead: PEntry;
    mFirstEntry, mLastEntry: Integer;
    mSeed: LongWord;

  private
    function allocEntry (): PEntry;
    procedure releaseEntry (e: PEntry);

    function distToStIdx (idx: LongWord): LongWord; inline;

    procedure putEntryInternal (swpe: PEntry);

    function getCapacity (): Integer; inline;

    procedure freeEntries ();

  public
    constructor Create (afreevalfn: TFreeValueFn=nil);
    destructor Destroy (); override;

    procedure clear ();
    procedure reset (); // don't shrink buckets

    procedure rehash ();
    procedure compact (); // call this instead of `rehash()` after alot of deletions

    // you may pass `keyhash` to bypass hash calculation
    function get (constref akey: KeyT; out rval: ValueT; keyhashin: PLongWord=nil): Boolean; // `true`: found
    // the function may return calculated value hash in `keyhash`
    function put (constref akey: KeyT; constref aval: ValueT; keyhashout: PLongWord=nil): Boolean; // `true`: replaced
    // you may pass `keyhash` to bypass hash calculation
    function has (constref akey: KeyT; keyhashin: PLongWord=nil): Boolean; // `true`: found
    // you may pass `keyhash` to bypass hash calculation
    function del (constref akey: KeyT; keyhashin: PLongWord=nil): Boolean; // `true`: deleted

    //WARNING! don't modify table in iterator (queries are ok, though)
    function forEach (it: TIteratorFn): Boolean; overload;
    function forEach (it: TIteratorExFn): Boolean; overload;

    // default `for ... in` enums values
    function GetEnumerator (): TValEnumerator;
    function byKey (): TKeyEnumerator;
    function byValue (): TValEnumerator;
    function byKeyValue (): TKeyValEnumerator; // PEntry

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

    procedure put (constref buf; len: LongWord);

    // current hash value
    // you can continue putting data, as this is not destructive
    function value: LongWord; inline;
  end;


type
  THashKeyInt = class
  public
    class function hash (const k: Integer): LongWord; inline;
    class function equ (const a, b: Integer): Boolean; inline;
    class procedure freekey (k: Integer); inline;
  end;

  THashKeyStr = class
  public
    class function hash (const k: AnsiString): LongWord; inline;
    class function equ (const a, b: AnsiString): Boolean; inline;
    class procedure freekey (var k: AnsiString); inline;
  end;

  // case-insensitive (ansi)
  THashKeyStrAnsiCI = class
  public
    class function hash (const k: AnsiString): LongWord; inline;
    class function equ (const a, b: AnsiString): Boolean; inline;
    class procedure freekey (var k: AnsiString); inline;
  end;

type
  THashIntInt = specialize THashBase<Integer, Integer, THashKeyInt>;
  THashStrInt = specialize THashBase<AnsiString, Integer, THashKeyStr>;
  THashStrCIInt = specialize THashBase<AnsiString, Integer, THashKeyStrAnsiCI>;
  THashIntStr = specialize THashBase<Integer, AnsiString, THashKeyInt>;
  THashStrStr = specialize THashBase<AnsiString, AnsiString, THashKeyStr>;
  THashStrCIStr = specialize THashBase<AnsiString, AnsiString, THashKeyStrAnsiCI>;
  THashStrVariant = specialize THashBase<AnsiString, Variant, THashKeyStr>;
  THashStrCIVariant = specialize THashBase<AnsiString, Variant, THashKeyStrAnsiCI>;


function u32Hash (a: LongWord): LongWord; inline;
function fnvHash (constref buf; len: LongWord): LongWord;
function joaatHash (constref buf; len: LongWord; seed: LongWord=0): LongWord;
function joaatHashPtr (buf: Pointer; len: LongWord; seed: LongWord=0): LongWord;

// has to be public due to FPC generics limitation
function nextPOTU32 (x: LongWord): LongWord; inline;


implementation

uses
  SysUtils, Variants;


// ////////////////////////////////////////////////////////////////////////// //
{$PUSH}
{$RANGECHECKS OFF}
function nextPOTU32 (x: LongWord): LongWord; inline;
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

procedure TJoaatHasher.put (constref buf; len: LongWord);
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


// ////////////////////////////////////////////////////////////////////////// //
{$PUSH}
{$RANGECHECKS OFF}
function joaatHash (constref buf; len: LongWord; seed: LongWord=0): LongWord;
var
  b: PByte;
  f: LongWord;
begin
  result := seed;
  b := PByte(@buf);
  for f := 1 to len do
  begin
    result += b^;
    result += (result shl 10);
    result := result xor (result shr 6);
    Inc(b);
  end;
  // finalize
  result += (result shl 3);
  result := result xor (result shr 11);
  result += (result shl 15);
end;

function joaatHashPtr (buf: Pointer; len: LongWord; seed: LongWord=0): LongWord;
var
  b: PByte;
  f: LongWord;
begin
  result := seed;
  b := PByte(buf);
  for f := 1 to len do
  begin
    result += b^;
    result += (result shl 10);
    result := result xor (result shr 6);
    Inc(b);
  end;
  // finalize
  result += (result shl 3);
  result := result xor (result shr 11);
  result += (result shl 15);
end;
{$POP}

{$PUSH}
{$RANGECHECKS OFF}
// fnv-1a: http://www.isthe.com/chongo/tech/comp/fnv/
function fnvHash (constref buf; len: LongWord): LongWord;
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

function locase1251 (ch: AnsiChar): AnsiChar; inline;
begin
  if ch < #128 then
  begin
    if (ch >= 'A') and (ch <= 'Z') then Inc(ch, 32);
  end
  else
  begin
    if (ch >= #192) and (ch <= #223) then
    begin
      Inc(ch, 32);
    end
    else
    begin
      case ch of
        #168, #170, #175: Inc(ch, 16);
        #161, #178: Inc(ch);
      end;
    end;
  end;
  result := ch;
end;


// ////////////////////////////////////////////////////////////////////////// //
// THashKeyInt
class function THashKeyInt.hash (const k: Integer): LongWord; inline;
begin
  result := LongWord(k);
  result -= (result shl 6);
  result := result xor (result shr 17);
  result -= (result shl 9);
  result := result xor (result shl 4);
  result -= (result shl 3);
  result := result xor (result shl 10);
  result := result xor (result shr 15);
end;

class function THashKeyInt.equ (const a, b: Integer): Boolean; inline; begin result := (a = b); end;
class procedure THashKeyInt.freekey (k: Integer); inline; begin end;


// ////////////////////////////////////////////////////////////////////////// //
// THashKeyStr
class function THashKeyStr.hash (const k: AnsiString): LongWord; inline; begin if (Length(k) > 0) then result := fnvHash((@k[1])^, Length(k)) else result := 0; end;
class function THashKeyStr.equ (const a, b: AnsiString): Boolean; inline; begin result := (a = b); end;
class procedure THashKeyStr.freekey (var k: AnsiString); inline; begin k := ''; end;


// ////////////////////////////////////////////////////////////////////////// //
// case-insensitive (ansi)
{$PUSH}
{$RANGECHECKS OFF}
// fnv-1a: http://www.isthe.com/chongo/tech/comp/fnv/
function fnvHashLo (constref buf; len: LongWord): LongWord;
var
  b: PAnsiChar;
begin
  b := @buf;
  result := 2166136261; // fnv offset basis
  while (len > 0) do
  begin
    result := result xor Byte(locase1251(b^));
    result := result*16777619; // 32-bit fnv prime
    Inc(b);
    Dec(len);
  end;
end;
{$POP}

class function THashKeyStrAnsiCI.hash (const k: AnsiString): LongWord; inline; begin if (Length(k) > 0) then result := fnvHashLo((@k[1])^, Length(k)) else result := 0; end;
class function THashKeyStrAnsiCI.equ (const a, b: AnsiString): Boolean; inline;
var
  f: Integer;
begin
  result := false;
  if (Length(a) = Length(b)) then
  begin
    for f := 1 to Length(a) do if (locase1251(a[f]) <> locase1251(b[f])) then exit;
  end;
  result := true;
end;
class procedure THashKeyStrAnsiCI.freekey (var k: AnsiString); inline; begin k := ''; end;


// ////////////////////////////////////////////////////////////////////////// //
function THashBase.TEntry.getEmpty (): Boolean; inline; begin result := (hash = 0); end;


// ////////////////////////////////////////////////////////////////////////// //
function THashBase.getCapacity (): Integer; inline; begin result := Length(mBuckets); end;


constructor THashBase.Create (afreevalfn: TFreeValueFn=nil);
begin
  freevalfn := afreevalfn;
  mSeed := u32Hash($29a);

  mFreeEntryHead := nil;
  mFirstEntry := -1;
  mLastEntry := -1;
  clear();
end;


destructor THashBase.Destroy ();
begin
  freeEntries();
  mBuckets := nil;
  mEntries := nil;
  inherited;
end;


procedure THashBase.freeEntries ();
var
  f: Integer;
  e: PEntry;
begin
  if (mFirstEntry >= 0) then
  begin
    for f := mFirstEntry to mLastEntry do
    begin
      e := @mEntries[f];
      if not e.empty then
      begin
        HashObjT.freekey(e.key);
        if assigned(freevalfn) then freevalfn(e.value) else e.value := Default(ValueT);
        e.key := Default(KeyT);
        e.value := Default(ValueT);
        e.hash := 0;
      end;
    end;
  end
  else if (Length(mEntries) > 0) then
  begin
    FillChar(mEntries[0], Length(mEntries)*sizeof(mEntries[0]), 0);
  end;
  mFreeEntryHead := nil;
  mFirstEntry := -1;
  mLastEntry := -1;
  {$IFDEF RBHASH_SANITY_CHECKS}
  mEntriesUsed := 0;
  {$ENDIF}
end;


procedure THashBase.clear ();
begin
  freeEntries();
  {
  SetLength(mBuckets, InitSize);
  FillChar(mBuckets[0], InitSize*sizeof(mBuckets[0]), 0);
  SetLength(mEntries, InitSize);
  FillChar(mEntries[0], InitSize*sizeof(mEntries[0]), 0);
  }
  mFreeEntryHead := nil;
  mBuckets := nil;
  mEntries := nil;
  mFirstEntry := -1;
  mLastEntry := -1;
  mBucketsUsed := 0;
end;


procedure THashBase.reset ();
//var idx: Integer;
begin
  freeEntries();
  if (mBucketsUsed > 0) then
  begin
    //for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;
    FillChar(mBuckets[0], Length(mBuckets)*sizeof(mBuckets[0]), 0);
    mBucketsUsed := 0;
  end;
end;


function THashBase.allocEntry (): PEntry;
var
  idx: Integer;
begin
  if (mFreeEntryHead = nil) then
  begin
    // nothing was allocated, so allocate something now
    if (Length(mBuckets) = 0) then
    begin
      assert(Length(mEntries) = 0);
      assert(mFirstEntry = -1);
      assert(mLastEntry = -1);
      assert(mBucketsUsed = 0);
      {$IFDEF RBHASH_SANITY_CHECKS}
      mEntriesUsed := 0;
      {$ENDIF}
      SetLength(mBuckets, InitSize);
      FillChar(mBuckets[0], InitSize*sizeof(mBuckets[0]), 0);
      SetLength(mEntries, InitSize);
      FillChar(mEntries[0], InitSize*sizeof(mEntries[0]), 0);
    end;
    if (mLastEntry = High(mEntries)) then raise Exception.Create('internal error in hash entry allocator (0.0)');
    Inc(mLastEntry);
    if (mFirstEntry = -1) then
    begin
      if (mLastEntry <> 0) then raise Exception.Create('internal error in hash entry allocator (0.1)');
      mFirstEntry := 0;
    end;
    result := @mEntries[mLastEntry];
    result.nextFree := nil; // just in case
    {$IFDEF RBHASH_SANITY_CHECKS}
    Inc(mEntriesUsed);
    {$ENDIF}
    exit;
  end;
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mFreeEntryHead = nil) then raise Exception.Create('internal error in hash entry allocator (0)');
  if (not mFreeEntryHead.empty) then raise Exception.Create('internal error in hash entry allocator (1)');
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
  if (e.empty) then raise Exception.Create('internal error in hash entry allocator (trying to release unallocated entry)');
  {$ENDIF}
  idx := Integer((PtrUInt(e)-PtrUInt(@mEntries[0])) div sizeof(mEntries[0]));
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (idx < 0) or (idx > High(mEntries)) then raise Exception.Create('internal error in hash entry allocator (invalid entry address)');
  if (e <> @mEntries[idx]) then raise Exception.Create('internal error in hash entry allocator (wtf?!)');
  {$ENDIF}
  HashObjT.freekey(e.key);
  if assigned(freevalfn) then freevalfn(e.value) else e.value := Default(ValueT);
  {$IFDEF RBHASH_SANITY_CHECKS}
  Dec(mEntriesUsed);
  {$ENDIF}
  e.key := Default(KeyT);
  e.value := Default(ValueT);
  e.hash := 0;
  e.nextFree := mFreeEntryHead;
  mFreeEntryHead := e;
  // fix mFirstEntry and mLastEntry
  {$IFDEF RBHASH_SANITY_CHECKS}
  if (mFirstEntry < 0) or (mLastEntry < 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 0)');
  {$ENDIF}
  if (mFirstEntry = mLastEntry) then
  begin
    {$IFDEF RBHASH_SANITY_CHECKS}
    if (mEntriesUsed <> 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 1)');
    {$ENDIF}
    mFreeEntryHead := nil;
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
      while (mEntries[cidx].empty) do Inc(cidx);
      {$IFDEF RBHASH_SANITY_CHECKS}
      if (cidx > High(mEntries)) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 3)');
      {$ENDIF}
      mFirstEntry := cidx;
    end;
    // fix last entry index
    if (idx = mLastEntry) then
    begin
      cidx := idx-1;
      while (mEntries[cidx].empty) do Dec(cidx);
      {$IFDEF RBHASH_SANITY_CHECKS}
      if (cidx < 0) then raise Exception.Create('internal error in hash entry allocator (invalid first/last range; 3)');
      {$ENDIF}
      mLastEntry := cidx;
    end;
  end;
end;


function THashBase.distToStIdx (idx: LongWord): LongWord; inline;
begin
  {$IFDEF RBHASH_SANITY_CHECKS}
  assert(idx < Length(mBuckets));
  assert(mBuckets[idx] <> nil);
  {$ENDIF}
  result := (mBuckets[idx].hash xor mSeed) and High(mBuckets);
  if (result <= idx) then result := idx-result else result := idx+(Length(mBuckets)-result);
end;


function THashBase.has (constref akey: KeyT; keyhashin: PLongWord=nil): Boolean;
var
  khash, idx: LongWord;
  dist, pdist: LongWord;
  bhigh, xseed: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then exit;

  bhigh := High(mBuckets);
  xseed := mSeed;

  if (keyhashin <> nil) then
  begin
    khash := keyhashin^;
    if (khash = 0) then khash := HashObjT.hash(akey);
  end
  else
  begin
    khash := HashObjT.hash(akey);
  end;
  if (khash = 0) then khash := $29a;

  idx := (khash xor xseed) and bhigh;
  if (mBuckets[idx] = nil) then exit;

  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then break;
    pdist := distToStIdx(idx);
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and HashObjT.equ(mBuckets[idx].key, akey);
    if result then break;
    idx := (idx+1) and bhigh;
  end;
end;


function THashBase.get (constref akey: KeyT; out rval: ValueT; keyhashin: PLongWord=nil): Boolean;
var
  khash, idx: LongWord;
  dist, pdist: LongWord;
  bhigh, xseed: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then begin rval := Default(ValueT); exit; end;

  bhigh := High(mBuckets);
  xseed := mSeed;

  if (keyhashin <> nil) then
  begin
    khash := keyhashin^;
    if (khash = 0) then khash := HashObjT.hash(akey);
  end
  else
  begin
    khash := HashObjT.hash(akey);
  end;
  if (khash = 0) then khash := $29a;

  idx := (khash xor xseed) and bhigh;

  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then break;
    pdist := distToStIdx(idx);
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and HashObjT.equ(mBuckets[idx].key, akey);
    if result then begin rval := mBuckets[idx].value; break; end;
    idx := (idx+1) and bhigh;
  end;

  if not result then rval := Default(ValueT); // just in case
end;


procedure THashBase.putEntryInternal (swpe: PEntry);
var
  idx, dist, pcur, pdist: LongWord;
  tmpe: PEntry;
  bhigh, xseed: LongWord;
begin
  bhigh := High(mBuckets);
  xseed := mSeed;
  idx := (swpe.hash xor xseed) and bhigh;
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
    pdist := distToStIdx(idx);
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


function THashBase.put (constref akey: KeyT; constref aval: ValueT; keyhashout: PLongWord=nil): Boolean;
var
  khash, idx, dist, pdist: LongWord;
  swpe: PEntry = nil; // current entry to swap (or nothing)
  bhigh, xseed: LongWord;
  newsz, eidx: Integer;
begin
  result := false;

  bhigh := High(mBuckets);
  xseed := mSeed;
  khash := HashObjT.hash(akey);
  if (khash = 0) then khash := $29a;
  if (keyhashout <> nil) then keyhashout^ := khash;
  idx := (khash xor xseed) and bhigh;

  // check if we already have this key
  if (mBucketsUsed <> 0) and (mBuckets[idx] <> nil) then
  begin
    for dist := 0 to bhigh do
    begin
      if (mBuckets[idx] = nil) then break;
      pdist := distToStIdx(idx);
      if (dist > pdist) then break;
      result := (mBuckets[idx].hash = khash) and HashObjT.equ(mBuckets[idx].key, akey);
      if result then
      begin
        // replace element
        HashObjT.freekey(mBuckets[idx].key);
        if assigned(freevalfn) then freevalfn(mBuckets[idx].value) else mBuckets[idx].value := Default(ValueT);
        mBuckets[idx].key := akey;
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
  end;

  // create new entry
  swpe := allocEntry();
  swpe.key := akey;
  swpe.value := aval;
  swpe.hash := khash;

  putEntryInternal(swpe);
end;


// see http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
function THashBase.del (constref akey: KeyT; keyhashin: PLongWord=nil): Boolean;
var
  khash, idx, idxnext, pdist, dist: LongWord;
  bhigh, xseed: LongWord;
begin
  result := false;
  if (mBucketsUsed = 0) then exit;

  bhigh := High(mBuckets);
  xseed := mSeed;

  if (keyhashin <> nil) then
  begin
    khash := keyhashin^;
    if (khash = 0) then khash := HashObjT.hash(akey);
  end
  else
  begin
    khash := HashObjT.hash(akey);
  end;
  if (khash = 0) then khash := $29a;

  idx := (khash xor xseed) and bhigh;

  // find key
  if (mBuckets[idx] = nil) then exit; // no key
  for dist := 0 to bhigh do
  begin
    if (mBuckets[idx] = nil) then break;
    pdist := distToStIdx(idx);
    if (dist > pdist) then break;
    result := (mBuckets[idx].hash = khash) and HashObjT.equ(mBuckets[idx].key, akey);
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
    pdist := distToStIdx(idxnext);
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
  //TODO: use prng to generate new hash
  if (mSeed = 0) then mSeed := $29a;
  mSeed := u32Hash(mSeed);
  // clear buckets
  //for idx := 0 to High(mBuckets) do mBuckets[idx] := nil;
  FillChar(mBuckets[0], Length(mBuckets)*sizeof(mBuckets[0]), 0);
  mBucketsUsed := 0;
  // reinsert entries
  mFreeEntryHead := nil;
  lastfree := nil;
  for idx := 0 to High(mEntries) do
  begin
    e := @mEntries[idx];
    if (not e.empty) then
    begin
      {$IFDEF RBHASH_SANITY_CHECKS}
      if (e.nextFree <> nil) then raise Exception.Create('internal error in rehash: inconsistent');
      if (cnt = 0) and (idx <> mFirstEntry) then raise Exception.Create('internal error in rehash: inconsistent (1)');
      Inc(cnt);
      if (cnt = mBucketsUsed) and (idx <> mLastEntry) then raise Exception.Create('internal error in rehash: inconsistent (2)');
      {$ENDIF}
      // no need to recalculate hash
      putEntryInternal(e);
    end
    else
    begin
      if (lastfree <> nil) then lastfree.nextFree := e else mFreeEntryHead := e;
      lastfree := e;
    end;
  end;
  if (lastfree <> nil) then lastfree.nextFree := nil;
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
  newsz := nextPOTU32(LongWord(mBucketsUsed));
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
    while (didx < Length(mEntries)) do if (not mEntries[didx].empty) then Inc(didx) else break;
    f := didx+1;
    // copy entries
    while true do
    begin
      if (not mEntries[f].empty) then
      begin
        {$IFDEF RBHASH_SANITY_CHECKS}
        if (didx >= f) then raise Exception.Create('internal error in hash: inconsistent');
        {$ENDIF}
        mEntries[didx] := mEntries[f];
        mEntries[f].hash := 0;
        Inc(didx);
        if (f = mLastEntry) then break;
        while (didx < Length(mEntries)) do if (not mEntries[didx].empty) then Inc(didx) else break;
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
      if (mEntries[f].empty) then raise Exception.Create('internal error in hash table (invalid first/last range; 2)');
      Inc(cnt);
    end;
    if (cnt <> mBucketsUsed) then raise Exception.Create('internal error in hash table (invalid first/last range; 3)');
    if (cnt <> mEntriesUsed) then raise Exception.Create('internal error in hash table (invalid first/last range; 4)');
    for f := mLastEntry+1 to High(mEntries) do
    begin
      if (not mEntries[f].empty) then raise Exception.Create('internal error in hash table (invalid first/last range; 5)');
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


function THashBase.forEach (it: TIteratorFn): Boolean; overload;
var
  f: Integer;
begin
  result := false;
  if not assigned(it) or (mFirstEntry < 0) then exit;
  for f := mFirstEntry to mLastEntry do
  begin
    if (not mEntries[f].empty) then
    begin
      result := it(mEntries[f].key, mEntries[f].value);
      if result then exit;
    end;
  end;
end;

function THashBase.forEach (it: TIteratorExFn): Boolean; overload;
var
  f: Integer;
begin
  result := false;
  if not assigned(it) or (mFirstEntry < 0) then exit;
  for f := mFirstEntry to mLastEntry do
  begin
    if (not mEntries[f].empty) then
    begin
      result := it(mEntries[f].key, mEntries[f].value, mEntries[f].hash);
      if result then exit;
    end;
  end;
end;


// enumerators
function THashBase.GetEnumerator (): TValEnumerator;
begin
  if (Length(mEntries) > 0) then result := TValEnumerator.Create(mEntries, mFirstEntry, mLastEntry)
  else result := TValEnumerator.Create(nil, -1, -1);
end;

function THashBase.byKey (): TKeyEnumerator;
begin
  if (Length(mEntries) > 0) then result := TKeyEnumerator.Create(mEntries, mFirstEntry, mLastEntry)
  else result := TKeyEnumerator.Create(nil, -1, -1);
end;

function THashBase.byValue (): TValEnumerator;
begin
  if (Length(mEntries) > 0) then result := TValEnumerator.Create(mEntries, mFirstEntry, mLastEntry)
  else result := TValEnumerator.Create(nil, -1, -1);
end;

function THashBase.byKeyValue (): TKeyValEnumerator; // PEntry
begin
  if (Length(mEntries) > 0) then result := TKeyValEnumerator.Create(mEntries, mFirstEntry, mLastEntry)
  else result := TKeyValEnumerator.Create(nil, -1, -1);
end;


function THashBase.TValEnumerator.GetEnumerator (): TValEnumerator; inline; begin result.mEntries := self.mEntries; result.mFirstEntry := self.mFirstEntry; result.mLastEntry := self.mLastEntry; result.cur := self.cur; end;
function THashBase.TKeyEnumerator.GetEnumerator (): TKeyEnumerator; inline; begin result.mEntries := self.mEntries; result.mFirstEntry := self.mFirstEntry; result.mLastEntry := self.mLastEntry; result.cur := self.cur; end;
function THashBase.TKeyValEnumerator.GetEnumerator (): TKeyValEnumerator; inline; begin result.mEntries := self.mEntries; result.mFirstEntry := self.mFirstEntry; result.mLastEntry := self.mLastEntry; result.cur := self.cur; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THashBase.TValEnumerator.Create (const aents: TEntryArray; afirst, alast: Integer);
begin
  mEntries := aents;
  mFirstEntry := afirst;
  mLastEntry := alast;
  cur := mFirstEntry-1;
end;

function THashBase.TValEnumerator.MoveNext (): Boolean; inline;
begin
  Inc(cur);
  while (cur <= mLastEntry) do
  begin
    if (not mEntries[cur].empty) then begin result := true; exit; end;
  end;
  result := false;
end;

function THashBase.TValEnumerator.getCurrent (): ValueT; inline;
begin
  result := mEntries[cur].value;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THashBase.TKeyEnumerator.Create (const aents: TEntryArray; afirst, alast: Integer);
begin
  mEntries := aents;
  mFirstEntry := afirst;
  mLastEntry := alast;
  cur := mFirstEntry-1;
end;

function THashBase.TKeyEnumerator.MoveNext (): Boolean; inline;
begin
  Inc(cur);
  while (cur <= mLastEntry) do
  begin
    if (not mEntries[cur].empty) then begin result := true; exit; end;
  end;
  result := false;
end;

function THashBase.TKeyEnumerator.getCurrent (): KeyT; inline;
begin
  result := mEntries[cur].key;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor THashBase.TKeyValEnumerator.Create (const aents: TEntryArray; afirst, alast: Integer);
begin
  mEntries := aents;
  mFirstEntry := afirst;
  mLastEntry := alast;
  cur := mFirstEntry-1;
end;

function THashBase.TKeyValEnumerator.MoveNext (): Boolean; inline;
begin
  Inc(cur);
  while (cur <= mLastEntry) do
  begin
    if (not mEntries[cur].empty) then begin result := true; exit; end;
  end;
  result := false;
end;

function THashBase.TKeyValEnumerator.getCurrent (): PEntry; inline;
begin
  result := @mEntries[cur];
end;


end.
