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
uses
  SysUtils,
  binheap in '../binheap.pas';


var
  heap: TBinaryHeapInt;
begin
  writeln('================');
  heap := binHeapNewIntLess();
  heap.insert(666);
  heap.insert(42);
  heap.insert(69);
  heap.insert(-666);
  heap.insert(8);

  while (heap.count > 0) do
  begin
    writeln(heap.front);
    heap.popFront();
  end;

  heap.Free();

  writeln('================');
  heap := binHeapNewIntGreat();
  heap.insert(666);
  heap.insert(42);
  heap.insert(69);
  heap.insert(-666);
  heap.insert(8);

  while (heap.count > 0) do
  begin
    writeln(heap.front);
    heap.popFront();
  end;

  heap.Free();
end.
