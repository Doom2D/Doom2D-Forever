(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
unit fui_common;

interface


// ////////////////////////////////////////////////////////////////////////// //
type
  TLaySize = record
  public
    w, h: Integer;

  private
    function getIdx (idx: Integer): Integer; inline;
    procedure setIdx (idx, v: Integer); inline;

  public
    constructor Create (aw, ah: Integer);

    function toString (): AnsiString;

    function equals (constref a: TLaySize): Boolean; inline;
  public
    property item[idx: Integer]: Integer read getIdx write setIdx; default;
  end;

  TLayPos = record
  public
    x, y: Integer;

  private
    function getIdx (idx: Integer): Integer; inline;
    procedure setIdx (idx, v: Integer); inline;

  public
    constructor Create (ax, ay: Integer);

    function toString (): AnsiString;

    function equals (constref a: TLayPos): Boolean; inline;

  public
    property item[idx: Integer]: Integer read getIdx write setIdx; default;
  end;

  TLayMargins = record
  public
    top, right, bottom, left: Integer;

  public
    constructor Create (atop, aright, abottom, aleft: Integer);

    function toString (): AnsiString;

    function horiz (): Integer; inline;
    function vert (): Integer; inline;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TGxRGBA = packed record
  public
    r, g, b, a: Byte;

  public
    constructor Create (ar, ag, ab: Integer; aa: Integer=255);

    function asUInt (): LongWord; inline;
    function isOpaque (): Boolean; inline;
    function isTransparent (): Boolean; inline;

    // WARNING! This function does blending in RGB space, and RGB space is not linear!
    // alpha value of `self` doesn't matter
    // `aa` means: 255 for replace color, 0 for keep `self`
    function blend (ar, ag, ab, aa: Integer): TGxRGBA; inline;
  end;


// ////////////////////////////////////////////////////////////////////////// //
// return `false` if destination rect is empty
// modifies rect0
function intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean; inline;
procedure normRGBA (var r, g, b, a: Integer); inline;


implementation

uses
  utils;


// ////////////////////////////////////////////////////////////////////////// //
constructor TLaySize.Create (aw, ah: Integer); begin w := aw; h := ah; end;
function TLaySize.getIdx (idx: Integer): Integer; inline; begin if (idx = 0) then result := w else if (idx = 1) then result := h else result := -1; end;
procedure TLaySize.setIdx (idx, v: Integer); inline; begin if (idx = 0) then w := v else if (idx = 1) then h := v; end;
function TLaySize.toString (): AnsiString; begin result := formatstrf('[%d,%d]', [w, h]); end;
function TLaySize.equals (constref a: TLaySize): Boolean; inline; begin result := (w = a.w) and (h = a.h); end;

constructor TLayPos.Create (ax, ay: Integer); begin x := ax; y := ay; end;
function TLayPos.getIdx (idx: Integer): Integer; inline; begin if (idx = 0) then result := x else if (idx = 1) then result := y else result := -1; end;
procedure TLayPos.setIdx (idx, v: Integer); inline; begin if (idx = 0) then x := v else if (idx = 1) then y := v; end;
function TLayPos.toString (): AnsiString; begin result := formatstrf('(%d,%d)', [x, y]); end;
function TLayPos.equals (constref a: TLayPos): Boolean; inline; begin result := (x = a.x) and (y = a.y); end;

constructor TLayMargins.Create (atop, aright, abottom, aleft: Integer);
begin
  if (atop < 0) then atop := 0;
  if (aright < 0) then aright := 0;
  if (abottom < 0) then abottom := 0;
  if (aleft < 0) then aleft := 0;
  left := aleft;
  right := aright;
  top := atop;
  bottom := abottom;
end;
function TLayMargins.toString (): AnsiString; begin result := formatstrf('(%s,%s,%s,%s)', [top, right, bottom, left]); end;
function TLayMargins.horiz (): Integer; inline; begin result := left+right; end;
function TLayMargins.vert (): Integer; inline; begin result := top+bottom; end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TGxRGBA.Create (ar, ag, ab: Integer; aa: Integer=255);
begin
  if (ar < 0) then r := 0 else if (ar > 255) then r := 255 else r := Byte(ar);
  if (ag < 0) then g := 0 else if (ag > 255) then g := 255 else g := Byte(ag);
  if (ab < 0) then b := 0 else if (ab > 255) then b := 255 else b := Byte(ab);
  if (aa < 0) then a := 0 else if (aa > 255) then a := 255 else a := Byte(aa);
end;

function TGxRGBA.asUInt (): LongWord; inline; begin result := LongWord(r) or (LongWord(g) shl 8) or (LongWord(b) shl 16) or (LongWord(a) shl 24); end;

function TGxRGBA.isOpaque (): Boolean; inline; begin result := (a = 255); end;
function TGxRGBA.isTransparent (): Boolean; inline; begin result := (a = 0); end;

function TGxRGBA.blend (ar, ag, ab, aa: Integer): TGxRGBA; inline;
var
  me, it, a_tmp_, dc_tmp_, srb_tmp_, sg_tmp_, drb_tmp_, dg_tmp_, orb_tmp_, og_tmp_: LongWord;
begin
  if (aa <= 0) then begin result := self; exit; end;
  result := TGxRGBA.Create(ar, ag, ab, aa);
  if (aa >= 255) then begin result.a := a; exit; end;
  me := asUInt;
  it := result.asUInt;
  a_tmp_ := (256-(255-(it shr 24))) and (-(1-(((255-(it shr 24))+1) shr 8))); // to not loose bits, but 255 should become 0
  dc_tmp_ := me and $ffffff;
  srb_tmp_ := (it and $ff00ff);
  sg_tmp_ := (it and $00ff00);
  drb_tmp_ := (dc_tmp_ and $ff00ff);
  dg_tmp_ := (dc_tmp_ and $00ff00);
  orb_tmp_ := (drb_tmp_+(((srb_tmp_-drb_tmp_)*a_tmp_+$800080) shr 8)) and $ff00ff;
  og_tmp_ := (dg_tmp_+(((sg_tmp_-dg_tmp_)*a_tmp_+$008000) shr 8)) and $00ff00;
  me := (orb_tmp_ or og_tmp_); // or $ff000000; /* and $ffffff;*/
  result.r := Byte(me and $ff);
  result.g := Byte((me shr 8) and $ff);
  result.b := Byte((me shr 16) and $ff);
  result.a := a;
end;


// ////////////////////////////////////////////////////////////////////////// //
//TODO: overflow checks
function intersectRect (var x0, y0, w0, h0: Integer; const x1, y1, w1, h1: Integer): Boolean; inline;
var
  ex0, ey0: Integer;
  ex1, ey1: Integer;
begin
  result := false;
  if (w0 < 1) or (h0 < 1) or (w1 < 1) or (h1 < 1) then exit; // at least one rect is null
  // check for intersection
  ex0 := x0+w0;
  ey0 := y0+h0;
  ex1 := x1+w1;
  ey1 := y1+h1;
  if (ex0 <= x1) or (ey0 <= y1) or (ex1 <= x0) or (ey1 <= y0) then exit;
  if (x0 >= ex1) or (y0 >= ey1) or (x1 >= ex0) or (y1 >= ey0) then exit;
  // ok, intersects
  if (x0 < x1) then x0 := x1;
  if (y0 < y1) then y0 := y1;
  if (ex0 > ex1) then ex0 := ex1;
  if (ey0 > ey1) then ey0 := ey1;
  w0 := ex0-x0;
  h0 := ey0-y0;
  result := (w0 > 0) and (h0 > 0);
end;


procedure normRGBA (var r, g, b, a: Integer); inline;
begin
  if (a < 0) then a := 0 else if (a > 255) then a := 255;
  if (r < 0) then r := 0 else if (r > 255) then r := 255;
  if (g < 0) then g := 0 else if (g > 255) then g := 255;
  if (b < 0) then b := 0 else if (b > 255) then b := 255;
end;


end.
