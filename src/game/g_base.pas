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
{$INCLUDE ../shared/a_modes.inc}
unit g_base;

interface

  type
    TMirrorType = (None, Horizontal, Vertical);
    TBlending = (None, Blend, Filter, Invert);

    TPoint2i = record
      X, Y: Integer;
    end;

    TPoint2f = record
      X, Y: Double;
    end;

    TRect = record
      Left, Top, Right, Bottom: Integer;
    end;

    TRectWH = record
      X, Y: Integer;
      Width, Height: Word;
    end;

    TRGB = packed record
      R, G, B: Byte;
    end;

    PPoint2f = ^TPoint2f;
    PRect = ^TRect;
    PRectWH = ^TRectWH;

  function _RGB (Red, Green, Blue: Byte): TRGB;
  function _Point (X, Y: Integer): TPoint2i;
  function _Rect (X, Y: Integer; Width, Height: Word): TRectWH;
  function _TRect (L, T, R, B: LongInt): TRect;

implementation

  function _RGB (Red, Green, Blue: Byte): TRGB;
  begin
    Result.R := Red;
    Result.G := Green;
    Result.B := Blue;
  end;

  function _Point (X, Y: Integer): TPoint2i;
  begin
    Result.X := X;
    Result.Y := Y;
  end;

  function _Rect (X, Y: Integer; Width, Height: Word): TRectWH;
  begin
    Result.X := X;
    Result.Y := Y;
    Result.Width := Width;
    Result.Height := Height;
  end;

  function _TRect (L, T, R, B: LongInt): TRect;
  begin
    Result.Top := T;
    Result.Left := L;
    Result.Right := R;
    Result.Bottom := B;
  end;

end.
