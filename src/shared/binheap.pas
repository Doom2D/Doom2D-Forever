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
  // WARNING! don't put structures into heap, use ponters or ids!
  generic TBinaryHeapBase<ITP> = class(TObject)
  private
    type
      TBinaryHeapLessFn = function (a, b: ITP): Boolean;

  private
    elem: array of ITP;
    elemUsed: Integer;
    lessfn: TBinaryHeapLessFn;

  private
    procedure heapify (root: Integer);

  public
    constructor Create (alessfn: TBinaryHeapLessFn);
    destructor Destroy (); override;

    procedure clear ();

    procedure insert (val: ITP);

    function front (): ITP;
    procedure popFront ();

    property count: Integer read elemUsed;
  end;


type
  TBinaryHeapObj = specialize TBinaryHeapBase<TObject>;
  TBinaryHeapInt = specialize TBinaryHeapBase<Integer>;


function binHeapNewIntLess (): TBinaryHeapInt;
function binHeapNewIntGreat (): TBinaryHeapInt;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
function intLess (a, b: Integer): Boolean; begin result := (a < b); end;
function intGreat (a, b: Integer): Boolean; begin result := (a > b); end;


function binHeapNewIntLess (): TBinaryHeapInt; begin result := TBinaryHeapInt.Create(@intLess); end;
function binHeapNewIntGreat (): TBinaryHeapInt; begin result := TBinaryHeapInt.Create(@intGreat); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBinaryHeapBase.Create (alessfn: TBinaryHeapLessFn);
begin
  if not assigned(alessfn) then raise Exception.Create('wutafuck?!');
  lessfn := alessfn;
  SetLength(elem, 128); // 'cause why not?
  elemUsed := 0;
end;


destructor TBinaryHeapBase.Destroy ();
begin
  elem := nil;
  inherited;
end;


procedure TBinaryHeapBase.clear ();
begin
  elemUsed := 0;
end;


procedure TBinaryHeapBase.heapify (root: Integer);
var
  smallest, right: Integer;
  tmp: ITP;
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


procedure TBinaryHeapBase.insert (val: ITP);
var
  i, par: Integer;
begin
  //if (val = nil) then exit;
  i := elemUsed;
  // grow?
  if (i = Length(elem)) then
  begin
    if (i <= 65536) then par := i*2 else par := i+65536; // arbitrary numbers
    SetLength(elem, par);
  end;
  // increase counter
  Inc(elemUsed);
  // insert element
  while (i <> 0) do
  begin
    par := (i-1) div 2; // parent
    if not lessfn(val, elem[par]) then break;
    elem[i] := elem[par];
    i := par;
  end;
  elem[i] := val;
end;

function TBinaryHeapBase.front (): ITP;
begin
  if elemUsed > 0 then result := elem[0] else result := Default(ITP);
end;


procedure TBinaryHeapBase.popFront ();
begin
  if (elemUsed > 0) then
  begin
    Dec(elemUsed);
    elem[0] := elem[elemUsed];
    heapify(0);
  end;
end;


end.
