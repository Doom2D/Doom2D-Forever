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
{$INCLUDE ../shared/a_modes.inc}
unit gh_ui_common;

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


end.
