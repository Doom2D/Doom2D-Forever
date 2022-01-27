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
{$INCLUDE ../../shared/a_modes.inc}
unit r_touch;

interface

  procedure r_Touch_GetKeyRect (key: Integer; out x, y, w, h: Integer; out founded: Boolean);
  procedure r_Touch_Draw;

implementation

  uses
    {$IFDEF USE_SDL2}
      SDL2,
    {$ENDIF}
    SysUtils,
    e_input, g_options, g_system,
    r_game, r_graphics
  ;

  function GetKeyName (key: Integer): String;
  begin
    case key of
      VK_SHOWKBD: result := 'KBD';
      VK_HIDEKBD: result := 'KBD';
      VK_LEFT:    result := 'LEFT';
      VK_RIGHT:   result := 'RIGHT';
      VK_UP:      result := 'UP';
      VK_DOWN:    result := 'DOWN';
      VK_FIRE:    result := 'FIRE';
      VK_OPEN:    result := 'OPEN';
      VK_JUMP:    result := 'JUMP';
      VK_CHAT:    result := 'CHAT';
      VK_ESCAPE:  result := 'ESC';
      VK_0:       result := '0';
      VK_1:       result := '1';
      VK_2:       result := '2';
      VK_3:       result := '3';
      VK_4:       result := '4';
      VK_5:       result := '5';
      VK_6:       result := '6';
      VK_7:       result := '7';
      VK_8:       result := '8';
      VK_9:       result := '9';
      VK_A:       result := '10';
      VK_B:       result := '11';
      VK_C:       result := '12';
      VK_D:       result := '13';
      VK_E:       result := '14';
      VK_F:       result := '15';
      VK_CONSOLE: result := 'CON';
      VK_STATUS:  result := 'STAT';
      VK_TEAM:    result := 'TEAM';
      VK_PREV:    result := '<PREW';
      VK_NEXT:    result := 'NEXT>';
      VK_LSTRAFE: result := '<';
      VK_RSTRAFE: result := '>';
    else
      if (key > 0) and (key < e_MaxInputKeys) then
        result := e_KeyNames[key]
      else
       result := '<' + IntToStr(key) + '>'
    end
  end;

  procedure r_Touch_GetKeyRect (key: Integer; out x, y, w, h: Integer; out founded: Boolean);
    var
      sw, sh, sz: Integer;
      dpi: Single;

    procedure S (xx, yy, ww, hh: Single);
    begin
      x := Trunc(xx);
      y := Trunc(yy);
      w := Trunc(ww);
      h := Trunc(hh);
      founded := true;
    end;

  begin
    founded := false;
    {$IF DEFINED(USE_SDL2) AND NOT DEFINED(SDL2_NODPI)}
      if SDL_GetDisplayDPI(0, @dpi, nil, nil) <> 0 then
        dpi := 96;
    {$ELSE}
      dpi := 96;
    {$ENDIF}

    sz := Trunc(g_touch_size * dpi); sw := gWinSizeX; sh := gWinSizeY;
    x := 0; y := Round(sh * g_touch_offset / 100);
    w := sz; h := sz;

    if sys_IsTextInputActive() then
      case key of
        VK_HIDEKBD: S(sw - (sz/2), 0, sz / 2, sz / 2);
      end
    else if g_touch_alt then
      case key of
        (* top ------- x ------------------------------- y  w ----- h -- *)
        VK_CONSOLE:  S(0,                                0, sz / 2, sz / 2);
        VK_ESCAPE:   S(sw - 1*(sz/2) - 1,                0, sz / 2, sz / 2);
        VK_SHOWKBD:  S(sw - 2*(sz/2) - 1,                0, sz / 2, sz / 2);
        VK_CHAT:     S(sw / 2 - (sz/2) / 2 - (sz/2) - 1, 0, sz / 2, sz / 2);
        VK_STATUS:   S(sw / 2 - (sz/2) / 2 - 1,          0, sz / 2, sz / 2);
        VK_TEAM:     S(sw / 2 - (sz/2) / 2 + (sz/2) - 1, 0, sz / 2, sz / 2);
        (* left --- x - y -------------- w - h --- *)
        VK_PREV:  S(0,  sh - 3.0*sz - 1, sz, sz / 2);
        VK_LEFT:  S(0,  sh - 2.0*sz - 1, sz, sz * 2);
        VK_RIGHT: S(sz, sh - 2.0*sz - 1, sz, sz * 2);
        (* right - x ------------ y -------------- w - h -- *)
        VK_NEXT: S(sw - 1*sz - 1, sh - 3.0*sz - 1, sz, sz / 2);
        VK_UP:   S(sw - 2*sz - 1, sh - 2.0*sz - 1, sz, sz / 2);
        VK_FIRE: S(sw - 2*sz - 1, sh - 1.5*sz - 1, sz, sz);
        VK_DOWN: S(sw - 2*sz - 1, sh - 0.5*sz - 1, sz, sz / 2);
        VK_JUMP: S(sw - 1*sz - 1, sh - 2.0*sz - 1, sz, sz);
        VK_OPEN: S(sw - 1*sz - 1, sh - 1.0*sz - 1, sz, sz);
      end
    else
      case key of
        (* left ----- x ----- y -------------- w ----- h -- *)
        VK_ESCAPE:  S(0.0*sz, y - 1*sz - sz/2, sz,     sz / 2);
        VK_LSTRAFE: S(0.0*sz, y - 0*sz - sz/2, sz / 2, sz);
        VK_LEFT:    S(0.5*sz, y - 0*sz - sz/2, sz,     sz);
        VK_RIGHT:   S(1.5*sz, y - 0*sz - sz/2, sz,     sz);
        VK_RSTRAFE: S(2.5*sz, y - 0*sz - sz/2, sz / 2, sz);
        (* right - x ------------ y --------------- w - h *)
        VK_UP:   S(sw - 1*sz - 1, y -  1*sz - sz/2, sz, sz);
        VK_FIRE: S(sw - 1*sz - 1, y -  0*sz - sz/2, sz, sz);
        VK_DOWN: S(sw - 1*sz - 1, y - -1*sz - sz/2, sz, sz);
        VK_NEXT: S(sw - 2*sz - 1, y -  1*sz - sz/2, sz, sz);
        VK_JUMP: S(sw - 2*sz - 1, y -  0*sz - sz/2, sz, sz);
        VK_PREV: S(sw - 3*sz - 1, y -  1*sz - sz/2, sz, sz);
        VK_OPEN: S(sw - 3*sz - 1, y -  0*sz - sz/2, sz, sz);
        (* bottom ---- x -------------------------- y ---------------- w ----- h -- *)
        VK_CHAT:     S(sw/2 - sz/4 -  2*(sz/2) - 1, sh - 2*(sz/2) - 1, sz / 2, sz / 2);
        VK_CONSOLE:  S(sw/2 - sz/4 -  1*(sz/2) - 1, sh - 2*(sz/2) - 1, sz / 2, sz / 2);
        VK_STATUS:   S(sw/2 - sz/4 -  0*(sz/2) - 1, sh - 2*(sz/2) - 1, sz / 2, sz / 2);
        VK_TEAM:     S(sw/2 - sz/4 - -1*(sz/2) - 1, sh - 2*(sz/2) - 1, sz / 2, sz / 2);
        VK_SHOWKBD:  S(sw/2 - sz/4 - -2*(sz/2) - 1, sh - 2*(sz/2) - 1, sz / 2, sz / 2);
        VK_0:        S(sw/2 - sz/4 -  5*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_1:        S(sw/2 - sz/4 -  4*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_2:        S(sw/2 - sz/4 -  3*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_3:        S(sw/2 - sz/4 -  2*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_4:        S(sw/2 - sz/4 -  1*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_5:        S(sw/2 - sz/4 -  0*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_6:        S(sw/2 - sz/4 - -1*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_7:        S(sw/2 - sz/4 - -2*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_8:        S(sw/2 - sz/4 - -3*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_9:        S(sw/2 - sz/4 - -4*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
        VK_A:        S(sw/2 - sz/4 - -5*(sz/2) - 1, sh - 1*(sz/2) - 1, sz / 2, sz / 2);
      end
  end;

  procedure r_Touch_Draw;
    var i, x, y, w, h: Integer; founded: Boolean;
  begin
    if g_touch_enabled then
    begin
      for i := VK_FIRSTKEY to VK_LASTKEY do
      begin
        r_Touch_GetKeyRect(i, x, y, w, h, founded);
        if founded then
        begin
          e_DrawQuad(x, y, x + w, y + h, 0, 255, 0, 31);
          e_TextureFontPrintEx(x, y, GetKeyName(i), gStdFont, 255, 255, 255, 1, True)
        end
      end
    end
  end;

end.
