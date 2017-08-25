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
{$DEFINE IDPOOL_CHECKS}
uses
  SysUtils, idpool;


// ////////////////////////////////////////////////////////////////////////// //
procedure simpleTest ();
var
  ip: TIdPool;
begin
  ip := TIdPool.Create();
  writeln(ip.alloc); ip.dump();
  writeln(ip.alloc); ip.dump();
  writeln(ip.alloc); ip.dump();
  writeln(ip.alloc); ip.dump();
  writeln(ip.alloc); ip.dump();
  writeln(ip.alloc); ip.dump();
  ip.release(2); ip.dump();
  ip.release(4); ip.dump();
  ip.release(0); ip.dump();
  ip.release(1); ip.dump();
  ip.release(3); ip.dump();
  ip.release(5); ip.dump();
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure hardTest ();
var
  ip: TIdPool;
  map: array of Boolean = nil;
  f, n: Integer;
  usedIds: Integer = 0;
begin
  ip := TIdPool.Create(65535*1024);
  SetLength(map, ip.maxId+1);
  for f := 0 to High(map) do map[f] := false;
  for f := 0 to High(map) div 2 do
  begin
    if ip.hasAlloced[f] then raise Exception.Create('invalid pool(0)');
    if not ip.hasFree[f] then raise Exception.Create('invalid pool(1)');
    if (ip.alloc <> f) then raise Exception.Create('invalid alloc(2)');
    map[f] := true;
    Inc(usedIds);
    if not ip.hasAlloced[f] then raise Exception.Create('invalid pool(3)');
    if ip.hasFree[f] then raise Exception.Create('invalid pool(4)');
  end;
  for f := 0 to 10000000 do
  begin
    //if (usedIds = 0) then break;
    n := Random(ip.maxId+1);
    if map[n] then
    begin
      // allocated, remove
      if not ip.hasAlloced[n] then raise Exception.Create('invalid pool(5)');
      if ip.hasFree[n] then raise Exception.Create('invalid pool(6)');
      //ip.dump();
      ip.release(n);
      //ip.dump();
      if ip.hasAlloced[n] then raise Exception.Create(Format('invalid pool(7): %d', [n]));
      if not ip.hasFree[n] then raise Exception.Create('invalid pool(8)');
      map[n] := false;
      Dec(usedIds);
    end
    else
    begin
      // free, allocate
      //ip.dump();
      n := ip.alloc();
      //ip.dump();
      if map[n] then raise Exception.Create('invalid pool(9)');
      if not ip.hasAlloced[n] then raise Exception.Create('invalid pool(a)');
      if ip.hasFree[n] then raise Exception.Create('invalid pool(b)');
      map[n] := true;
      Inc(usedIds);
    end;
  end;
  writeln(usedIds, ' used ids; id has ', ip.usedRanges, ' used ranges out of ', ip.capacity);
  if (usedIds <> ip.usedIds) then raise Exception.Create('used ids count mismatch');
  ip.check();
  for f := 0 to High(map) do
  begin
    if map[f] then
    begin
      if not ip.hasAlloced[f] then raise Exception.Create('invalid pool(b)');
      if ip.hasFree[f] then raise Exception.Create('invalid pool(c)');
    end
    else
    begin
      if ip.hasAlloced[f] then raise Exception.Create('invalid pool(d)');
      if not ip.hasFree[f] then raise Exception.Create('invalid pool(e)');
    end;
  end;
end;


begin
  //simpleTest();
  Randomize();
  hardTest();
end.
