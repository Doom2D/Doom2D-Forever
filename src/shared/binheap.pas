(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
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


(*
 * CmpObjT: class that contains class methods:
 *   class function less (const[ref] a, b: KeyT): Boolean;
 *)
type
  // WARNING! don't put structures into heap, use ponters or ids!
  generic TBinaryHeapBase<ITP, CmpObjT> = class(TObject)
  private
    elem: array of ITP;
    elemUsed: Integer;

  private
    procedure heapify (root: Integer);

  public
    constructor Create ();
    destructor Destroy (); override;

    procedure clear ();

    procedure insert (val: ITP);

    function front (): ITP;
    procedure popFront ();

    property count: Integer read elemUsed;
  end;


type
  TBinHeapKeyIntLess = class
  public
    class function less (const a, b: Integer): Boolean; inline;
  end;

  TBinHeapKeyIntGreat = class
  public
    class function less (const a, b: Integer): Boolean; inline;
  end;


type
  TBinaryHeapIntLess = specialize TBinaryHeapBase<Integer, TBinHeapKeyIntLess>;
  TBinaryHeapIntGreat = specialize TBinaryHeapBase<Integer, TBinHeapKeyIntGreat>;


implementation

uses
  SysUtils;


// ////////////////////////////////////////////////////////////////////////// //
class function TBinHeapKeyIntLess.less (const a, b: Integer): Boolean; inline; begin result := (a < b); end;
class function TBinHeapKeyIntGreat.less (const a, b: Integer): Boolean; inline; begin result := (a > b); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TBinaryHeapBase.Create ();
begin
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
    if not CmpObjT.less(elem[smallest], elem[root]) then smallest := root;
    if (right < elemUsed) and (CmpObjT.less(elem[right], elem[smallest])) then smallest := right;
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
    if not CmpObjT.less(val, elem[par]) then break;
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
