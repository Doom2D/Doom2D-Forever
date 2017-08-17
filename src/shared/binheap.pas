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
// binary heap
unit binheap;

interface


type
  TBinaryHeapLessFn = function (a, b: TObject): Boolean;

  TBinaryHeapObj = class(TObject)
  private
    elem: array of TObject;
    elemUsed: Integer;
    lessfn: TBinaryHeapLessFn;

  private
    procedure heapify (root: Integer);

  public
    constructor Create (alessfn: TBinaryHeapLessFn);
    destructor Destroy (); override;

    procedure clear ();

    procedure insert (val: TObject);

    function front (): TObject;
    procedure popFront ();

    property count: Integer read elemUsed;
  end;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBinaryHeapObj.Create (alessfn: TBinaryHeapLessFn);
begin
  if not assigned(alessfn) then raise Exception.Create('wutafuck?!');
  lessfn := alessfn;
  SetLength(elem, 8192); // 'cause why not?
  elemUsed := 0;
end;


destructor TBinaryHeapObj.Destroy ();
begin
  elem := nil;
  inherited;
end;


procedure TBinaryHeapObj.clear ();
begin
  elemUsed := 0;
end;


procedure TBinaryHeapObj.heapify (root: Integer);
var
  smallest, right: Integer;
  tmp: TObject;
begin
  while true do
  begin
    smallest := 2*root+1; // left child
    if (smallest >= elemUsed) then break; // anyway
    right := smallest+1; // right child
    if not lessfn(elem[smallest], elem[root]) then smallest := root;
    if (right < elemUsed) and (lessfn(elem[right], elem[smallest])) then smallest := right;
    if (smallest = root) then break;
    // swap
    tmp := elem[root];
    elem[root] := elem[smallest];
    elem[smallest] := tmp;
    root := smallest;
  end;
end;


procedure TBinaryHeapObj.insert (val: TObject);
var
  i, par: Integer;
begin
  if (val = nil) then exit;
  i := elemUsed;
  if (i = Length(elem)) then SetLength(elem, Length(elem)+16384); // arbitrary number
  Inc(elemUsed);
  while (i <> 0) do
  begin
    par := (i-1) div 2; // parent
    if not lessfn(val, elem[par]) then break;
    elem[i] := elem[par];
    i := par;
  end;
  elem[i] := val;
end;

function TBinaryHeapObj.front (): TObject;
begin
  if elemUsed > 0 then result := elem[0] else result := nil;
end;


procedure TBinaryHeapObj.popFront ();
begin
  if (elemUsed > 0) then
  begin
    Dec(elemUsed);
    elem[0] := elem[elemUsed];
    heapify(0);
  end;
end;


end.
