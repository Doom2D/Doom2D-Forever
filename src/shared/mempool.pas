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
{$DEFINE MEM_DISABLE_ACCOUNTING}
unit mempool;

interface

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


implementation

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
finalization
  {$IF DEFINED(D2F_DEBUG) and NOT DEFINED(MEM_DISABLE_ACCOUNTING)}
  dumpPools();
  {$ENDIF}
end.
