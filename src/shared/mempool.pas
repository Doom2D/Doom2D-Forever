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
{$DEFINE MEM_DISABLE_ACCOUNTING}
unit mempool;

interface

{$IFDEF USE_MEMPOOL}
uses
  SysUtils;


type
  PMemPool = ^TMemPool;
  TMemPool = record
  private
    mName: ShortString;
    mObjSize: Integer; // not a limit, just a recommendation
    mFirstPage: Pointer;
    mLastPage: Pointer;
    mAllocTotal: Integer;
    mAllocCount: Integer;

  public
    constructor Create (const aname: AnsiString; aobjsize: Integer);

    procedure setCapacity (acount: Integer); // ensure capacity for at least `acount` objects
    procedure release (); // release all pool memory

    function alloc (len: Integer): Pointer; // throws on OOM
    procedure free (ptr: Pointer); // currently it is noop

  public
    property name: ShortString read mName;
    property allocCount: Integer read mAllocCount;
    property allocTotal: Integer read mAllocTotal;
  end;


  TPoolObject = class
    {$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
    public class function NewInstance (): TObject; override;
    public procedure FreeInstance (); override;
    {$ENDIF}
  end;
{$ENDIF}

(* Simple "mark/release" allocator *)
type
  PoolMark = Integer;

  PPoolMarkRelease = ^TPoolMarkRelease;
  TPoolMarkRelease = record
  private
    mMemory: Pointer;
    mSize: Integer;
    mUsed: Integer;

  public
    constructor Create (aInitSize: Integer);

    // free all allocated memory
    procedure kill ();

    // forget everything
    procedure reset ();

    // mark current position
    function mark (): PoolMark; inline;
    // forget everything from the given mark
    procedure release (amark: PoolMark); inline;

    // allocate some memory
    // WARNING! pool can realloc it's internal storage and invalidate all previous pointers!
    function alloc (size: Integer): Pointer; inline;

    // get pointer for the given mark
    // WARNING! pointer can become invalid after next call to `alloc()`!
    function getPtr (amark: PoolMark): Pointer; inline;
    function curPtr (): Pointer; inline;
  end;


type
  generic PoolIter<T> = record
  public
    type Ptr = ^T;
    type MyType = specialize PoolIter<T>;

  private
    mPool: PPoolMarkRelease;
    mMark: PoolMark;
    mCount: Integer;
    mCurrent: Integer;
    mFinished: Boolean;

  public
    constructor Create (var apool: TPoolMarkRelease); // idiotic FPC doesn't support arg-less ctors for rectord
    procedure finishIt (); inline; // sets count

    procedure rewind (); inline;
    function length (): Integer; inline;
    procedure release (); inline; // reset pool

    function moveNext (): Boolean; inline;
    function getCurrent (): Ptr; inline;
    function getEnumerator (): MyType; inline;

    function first (): Ptr; inline;

  public
    property current: Ptr read getCurrent;
  end;


var
  framePool: TPoolMarkRelease; // temporary per-frame allocation pool


implementation

uses
  SysUtils
{$IFDEF USE_MEMPOOL}
  , hashtable
{$ENDIF}
  ;


// ////////////////////////////////////////////////////////////////////////// //
constructor TPoolMarkRelease.Create (aInitSize: Integer);
begin
  if (aInitSize > 0) then
  begin
    mSize := aInitSize;
    GetMem(mMemory, mSize);
  end
  else
  begin
    mMemory := nil;
    mSize := 0;
  end;
  mUsed := 0;
end;


// free all allocated memory
procedure TPoolMarkRelease.kill ();
begin
  if (mMemory <> nil) then FreeMem(mMemory);
  mMemory := nil;
  mSize := 0;
  mUsed := 0;
end;


// forget everything
procedure TPoolMarkRelease.reset ();
begin
  mUsed := 0;
end;


// mark current position
function TPoolMarkRelease.mark (): PoolMark; inline;
begin
  result := mUsed;
end;


// forget everything from the given mark
procedure TPoolMarkRelease.release (amark: PoolMark); inline;
begin
  if (amark < 0) or (amark > mUsed) then raise Exception.Create('MarkReleasePool is fucked (release)');
  mUsed := amark;
end;


// allocate some memory
// WARNING! pool can realloc it's internal storage and invalidate all previous pointers!
function TPoolMarkRelease.alloc (size: Integer): Pointer; inline;
begin
  if (size < 0) then raise Exception.Create('MarkReleasePool: cannot allocate negative amount of bytes');
  if (size > 1024*1024) then raise Exception.Create('MarkReleasePool: why do you need to allocate more than 1MB?');
  // do we need to get more memory?
  if (mUsed+size > mSize) then
  begin
    if (mUsed+size > 1024*1024*64) then raise Exception.Create('MarkReleasePool: more than 64MB in MarkReleasePool is insanity!');
    while (mUsed+size > mSize) do
    begin
      // less than 256KB: 64KB steps
      if (mSize < 256*1024) then mSize += 64*1024
      // less than 1MB: 128KB steps
      else if (mSize < 1024*1024) then mSize += 128*1024
      // otherwise, 1MB steps
      else mSize += 1024*1024;
    end;
    ReallocMem(mMemory, mSize);
    if (mMemory = nil) then raise Exception.Create('MarkReleasePool: out of memory!');
  end;
  result := Pointer(PAnsiChar(mMemory)+mUsed);
  mUsed += size;
  assert(mUsed <= mSize);
end;


// get pointer for the given mark
// WARNING! pointer can become invalid after next call to `alloc()`!
function TPoolMarkRelease.getPtr (amark: PoolMark): Pointer; inline;
begin
  if (amark < 0) or (amark > mUsed) then raise Exception.Create('MarkReleasePool is fucked (getPtr)');
  result := Pointer(PAnsiChar(mMemory)+amark);
end;


function TPoolMarkRelease.curPtr (): Pointer; inline;
begin
  result := Pointer(PAnsiChar(mMemory)+mUsed);
end;



// ////////////////////////////////////////////////////////////////////////// //
constructor PoolIter.Create (var apool: TPoolMarkRelease);
begin
  mPool := @apool;
  mMark := mPool^.mark();
  mCount := 0;
  mCurrent := -1;
  mFinished := false;
end;


procedure PoolIter.finishIt (); inline; // sets count
begin
  if (mFinished) then raise Exception.Create('double fatality');
  if (mPool = nil) then raise Exception.Create('void fatality');
  mFinished := true;
  mCount := Integer(PtrUInt(mPool^.curPtr)-PtrUInt(mPool^.getPtr(mMark))) div Integer(sizeof(T));
  if (mCount < 0) then raise Exception.Create('wutafu?');
end;


procedure PoolIter.rewind (); inline;
begin
  if (mPool = nil) then raise Exception.Create('void rewind');
  mCurrent := -1;
end;


function PoolIter.length (): Integer; inline;
begin
  //if (mCurrent+1 >= 0) and (mCurrent+1 < mCount) then result := mCount-(mCurrent+1) else result := 0;
  result := mCount;
end;


procedure PoolIter.release (); inline; // reset pool
begin
  if (mPool = nil) then raise Exception.Create('double release');
  mPool^.release(mMark);
  mPool := nil;
  mCount := 0;
  mCurrent := -1;
  mFinished := false;
end;


function PoolIter.moveNext (): Boolean; inline;
begin
  if (mPool = nil) then raise Exception.Create('void moveNext()');
  if (not mFinished) then raise Exception.Create('moveNext() on unfinished');
  Inc(mCurrent);
  result := (mCurrent < mCount);
end;


function PoolIter.getCurrent (): Ptr; inline;
begin
  if (mPool = nil) then raise Exception.Create('getCurrent() on nothing');
  if (mCurrent < 0) or (mCurrent >= mCount) then raise Exception.Create('getCurrent() range error');
  result := Ptr(mPool^.getPtr(mMark+mCurrent*Integer(sizeof(T))));
end;


function PoolIter.getEnumerator (): PoolIter; inline;
begin
  result := self;
end;


function PoolIter.first (): Ptr; inline;
begin
  if (mPool = nil) then raise Exception.Create('void moveNext()');
  if (not mFinished) then raise Exception.Create('moveNext() on unfinished');
  result := Ptr(mPool^.getPtr(mMark));
end;


// ////////////////////////////////////////////////////////////////////////// //
{$IFDEF USE_MEMPOOL}
uses
  hashtable;

type
  THashKeyPtr = class
  public
    class function hash (const k: Pointer): LongWord; inline;
    class function equ (const a, b: Pointer): Boolean; inline;
    class procedure freekey (k: Pointer); inline;
  end;

  THashPtrPtr = specialize THashBase<Pointer, PMemPool, THashKeyPtr>; // key: TClass; value: PMemPool

var
  pools: THashPtrPtr = nil;


// ////////////////////////////////////////////////////////////////////////// //
class function THashKeyPtr.hash (const k: Pointer): LongWord; inline; begin result := fnvHash(PByte(@k)^, sizeof(k)); end;
class function THashKeyPtr.equ (const a, b: Pointer): Boolean; inline; begin result := (a = b); end;
class procedure THashKeyPtr.freekey (k: Pointer); inline; begin end;


function getPoolFor (c: TClass): PMemPool;
begin
  if (pools = nil) then pools := THashPtrPtr.Create();
  if not pools.get(Pointer(c), result) then
  begin
    GetMem(result, sizeof(TMemPool));
    result.Create(c.ClassName, c.InstanceSize);
    pools.put(Pointer(c), result);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TMemPool.Create (const aname: AnsiString; aobjsize: Integer);
begin
  if (aobjsize < 1) then aobjsize := 16; // arbitrary number
  mName := aname;
  mObjSize := aobjsize;
  mFirstPage := nil;
  mLastPage := nil;
  mAllocTotal := 0;
  mAllocCount := 0;
end;


procedure TMemPool.setCapacity (acount: Integer); // ensure capacity for at least `acount` objects
begin
end;


procedure TMemPool.release (); // release all pool memory
begin
end;


function TMemPool.alloc (len: Integer): Pointer; // throws on OOM
begin
  if (len > 0) then mAllocTotal += len;
  if (len < 1) then len := 1;
  GetMem(result, len);
  FillChar(PByte(result)^, len, 0);
  Inc(mAllocCount);
end;


procedure TMemPool.free (ptr: Pointer); // currently it is noop
begin
  FreeMem(ptr);
end;


// ////////////////////////////////////////////////////////////////////////// //
{$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
class function TPoolObject.NewInstance (): TObject;
var
  {$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
  pool: PMemPool;
  {$ENDIF}
  ptr: Pointer;
begin
  {$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
  pool := getPoolFor(self.ClassType);
  ptr := pool.alloc(self.InstanceSize);
  {$ELSE}
  GetMem(ptr, self.InstanceSize);
  FillChar(PByte(ptr)^, self.InstanceSize, 0); // hello, Wyoming Knott!
  {$ENDIF}
  result := TObject(ptr);
  self.InitInstance(ptr);
end;


procedure TPoolObject.FreeInstance ();
var
  pool: PMemPool;
begin
  pool := getPoolFor(self.ClassType);
  pool.free(Pointer(self));
end;
{$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
{$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
procedure dumpPools ();
var
  fo: TextFile;
  kv: THashPtrPtr.PEntry;
begin
  AssignFile(fo, 'zmemlog.txt');
  Rewrite(fo);
  for kv in pools.byKeyValue do
  begin
    writeln(fo, kv.value.name, ': count=', kv.value.allocCount, '; total=', kv.value.allocTotal);
  end;
  CloseFile(fo);
end;
{$ENDIF}


initialization
  //mpoolMap := TMemPool.Create('textmap', 64);
  framePool := TPoolMarkRelease.Create(65536);
finalization
  {$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
  dumpPools();
  {$ENDIF}
{$ENDIF} // USE_MEMPOOL
end.
