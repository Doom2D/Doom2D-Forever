(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
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

{$INCLUDE ../a_modes.inc}
// tests for hash table
{$DEFINE EXCESSIVE_CHECKS}
uses
  SysUtils,
  hashtbl in '../hashtable.pas';

const
  MaxItems = 16384;


var
  its: array [0..MaxItems-1] of Integer;
  marks: array [0..MaxItems-1] of Boolean;
  hash: THashIntInt;
  i, v, hv: Integer;
  del: Boolean;

procedure checkHash (dump: Boolean=false);
var
  i, v: Integer;
  flag: Boolean;
  count: Integer = 0;
begin
  if dump then writeln('====== CHECK ======');
  for i := 0 to High(its) do
  begin
    if dump then
    begin
      v := -1;
      flag := hash.get(i, v);
      writeln(' check #', i, '; v=', v, '; flag=', flag);
    end;
    if (its[i] >= 0) then
    begin
      Inc(count);
      if not hash.has(i) then raise Exception.Create('(0.0) fuuuuuuuuuuuu');
      if not hash.get(i, v) then raise Exception.Create('(0.1) fuuuuuuuuuuuu');
      if (v <> its[i]) then raise Exception.Create('(0.2) fuuuuuuuuuuuu');
    end
    else
    begin
      if hash.has(i) then raise Exception.Create('(0.3) fuuuuuuuuuuuu');
    end;
  end;
  if (count <> hash.count) then raise Exception.Create('(0.4) fuuuuuuuuuuuu');
  if dump then writeln('------');
end;


procedure testIterator ();
var
  i, count: Integer;

  function iter (constref k: Integer; constref v: Integer): Boolean;
  begin
    result := false; // don't stop
    //writeln('key=', k, '; value=', v);
    if marks[k] then raise Exception.Create('duplicate entry in iterator');
    if (its[k] <> v) then raise Exception.Create('invalid entry in iterator');
    marks[k] := true;
    Inc(count);
  end;

begin
  count := 0;
  for i := 0 to High(marks) do marks[i] := false;
  hash.forEach(iter);
  if (count <> hash.count) then
  begin
    writeln('0: count=', count, '; hash.count=', hash.count);
    //raise Exception.Create('lost entries in iterator');
  end;
  count := 0;
  for i := 0 to High(marks) do if marks[i] then Inc(count);
  if (count <> hash.count) then
  begin
    writeln('1: count=', count, '; hash.count=', hash.count);
    raise Exception.Create('lost entries in iterator');
  end;
end;


var
  xcount: Integer;
begin
  for i := 0 to High(its) do its[i] := -1;

  hash := THashIntInt.Create();

  Randomize();

  writeln('testing: insertion');
  xcount := 0;
  for i := 0 to MaxItems-1 do
  begin
    v := Random(MaxItems);
    //writeln('i=', i, '; v=', v, '; its[v]=', its[v]);
    if (its[v] >= 0) then
    begin
      if not hash.has(v) then raise Exception.Create('(1.0) fuuuuuuuuuuuu');
      if not hash.get(v, hv) then raise Exception.Create('(1.1) fuuuuuuuuuuuu');
      if (hv <> its[v]) then raise Exception.Create('(1.2) fuuuuuuuuuuuu');
    end
    else
    begin
      its[v] := i;
      if hash.put(v, i) then raise Exception.Create('(1.3) fuuuuuuuuuuuu');
      Inc(xcount);
      if (xcount <> hash.count) then raise Exception.Create('(1.4) fuuuuuuuuuuuu');
    end;
    {$IFDEF EXCESSIVE_CHECKS}checkHash();{$ENDIF}
    {$IFDEF EXCESSIVE_CHECKS}testIterator();{$ENDIF}
  end;
  if (xcount <> hash.count) then raise Exception.Create('(1.4) fuuuuuuuuuuuu');
  checkHash();
  testIterator();

  writeln('testing: deletion');
  for i := 0 to MaxItems*8 do
  begin
    v := Random(MaxItems);
    //writeln('trying to delete ', v, '; its[v]=', its[v]);
    del := hash.del(v);
    //writeln('  del=', del);
    if del then
    begin
      if (its[v] < 0) then raise Exception.Create('(2.0) fuuuuuuuuuuuu');
      Dec(xcount);
    end
    else
    begin
      if (its[v] >= 0) then raise Exception.Create('(2.1) fuuuuuuuuuuuu');
    end;
    its[v] := -1;
    if (xcount <> hash.count) then raise Exception.Create('(1.4) fuuuuuuuuuuuu');
    hash.compact();
    if (xcount <> hash.count) then raise Exception.Create('(1.4) fuuuuuuuuuuuu');
    {$IFDEF EXCESSIVE_CHECKS}checkHash();{$ENDIF}
    {$IFDEF EXCESSIVE_CHECKS}testIterator();{$ENDIF}
    if (hash.count = 0) then break;
  end;

  writeln('testing: complete');
  checkHash();
  testIterator();
end.
