(* Copyright (C)  Doom 2D: Forever Developers
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
unit g_touch;

interface

  uses
    SDL2;

  var
    g_touch_enabled: Boolean;
    g_touch_size: Single;
    g_touch_offset: Single;
    g_touch_fire: Boolean;
    g_touch_alt: Boolean;

  procedure g_Touch_Init;
  procedure g_Touch_ShowKeyboard(yes: Boolean);
  procedure g_Touch_HandleEvent(const ev: TSDL_TouchFingerEvent);
  procedure g_Touch_Draw;

implementation

  uses
    SysUtils,
    e_log, e_graphics, e_input, g_options, g_game, g_main, g_weapons, g_console;

  const
    VS_KEYBOARD     = 60000;
    VS_HIDEKEYBOARD = 60001;

  var
    angleFire: Boolean;
    keyFinger: array [VK_FIRSTKEY..VK_LASTKEY] of Integer;

  procedure GetKeyRect(key: Integer; out x, y, w, h: Integer; out founded: Boolean);
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
    if SDL_GetDisplayDPI(0, @dpi, nil, nil) <> 0 then
      dpi := 96;

    sz := Trunc(g_touch_size * dpi); sw := gScreenWidth; sh := gScreenHeight;
    x := 0; y := Round(sh * g_touch_offset / 100);
    w := sz; h := sz;

    if SDL_IsTextInputActive() = SDL_True then
      case key of
        VS_HIDEKEYBOARD: S(sw - (sz/2), 0, sz / 2, sz / 2);
      end
    else if g_touch_alt then
      case key of
        (* top ------- x ------------------------------- y  w ----- h -- *)
        VK_CONSOLE:  S(0,                                0, sz / 2, sz / 2);
        VK_ESCAPE:   S(sw - 1*(sz/2) - 1,                0, sz / 2, sz / 2);
        VS_KEYBOARD: S(sw - 2*(sz/2) - 1,                0, sz / 2, sz / 2);
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
        VS_KEYBOARD: S(sw/2 - sz/4 - -2*(sz/2) - 1, sh - 2*(sz/2) - 1, sz / 2, sz / 2);
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

  function GetKeyName(key: Integer): String;
  begin
    case key of
      VS_KEYBOARD: result := 'KBD';
      VS_HIDEKEYBOARD: result := 'KBD';
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

  function IntersectControl(ctl, xx, yy: Integer): Boolean;
    var
      x, y, w, h: Integer;
      founded: Boolean;
  begin
    GetKeyRect(ctl, x, y, w, h, founded);
    result := founded and (xx >= x) and (yy >= y) and (xx <= x + w) and (yy <= y + h);
  end;

  procedure g_Touch_Init;
  begin
{$IFNDEF HEADLESS}
    g_touch_enabled := SDL_GetNumTouchDevices() > 0
{$ENDIF}
  end;

  procedure g_Touch_ShowKeyboard(yes: Boolean);
  begin
{$IFNDEF HEADLESS}
    if not g_touch_enabled then
      Exit;

    if yes then
      SDL_StartTextInput
    else
      SDL_StopTextInput
{$ENDIF}
  end;

  procedure g_Touch_HandleEvent(const ev: TSDL_TouchFingerEvent);
    var
      x, y, i, finger: Integer;
  begin
    if not g_touch_enabled then
      Exit;

    finger := ev.fingerId + 2;
    x := Trunc(ev.x * gScreenWidth);
    y := Trunc(ev.y * gScreenHeight);

    for i := VK_FIRSTKEY to VK_LASTKEY do
    begin
      if IntersectControl(i, x, y) then
      begin
        if ev.type_ = SDL_FINGERUP then
          keyFinger[i] := 0
        else if ev.type_ = SDL_FINGERMOTION then
          keyFinger[i] := finger
        else if ev.type_ = SDL_FINGERDOWN then
          begin
            KeyPress(i); // Menu events
            keyFinger[i] := finger;
          end
      end
      else if keyFinger[i] = finger then
      begin
        if ev.type_ = SDL_FINGERUP then
          keyFinger[i] := 0
        else if ev.type_ = SDL_FINGERMOTION then
          keyFinger[i] := 0
      end;

      e_KeyUpDown(i, keyFinger[i] <> 0);
    end;

    if IntersectControl(VS_KEYBOARD, x, y) then
      g_Touch_ShowKeyboard(true);
    if IntersectControl(VS_HIDEKEYBOARD, x, y) then
      g_Touch_ShowKeyboard(false);
	
    (* emulate up+fire / donw+fire *)
    if g_touch_fire and (gGameSettings.GameType <> GT_NONE) then
    begin
      if keyFinger[VK_UP] <> 0 then
      begin
        angleFire := true;
        keyFinger[VK_FIRE] := keyFinger[VK_UP];
        e_KeyUpDown(VK_FIRE, true);
      end
      else if keyFinger[VK_DOWN] <> 0 then
      begin
        angleFire := true;
        keyFinger[VK_FIRE] := keyFinger[VK_DOWN];
        e_KeyUpDown(VK_FIRE, true);
      end
      else if angleFire then
      begin
        angleFire := false;
        keyFinger[VK_FIRE] := 0;
        e_KeyUpDown(VK_FIRE, false);
      end
    end;

    (* left/right strafe *)
    if gGameSettings.GameType <> GT_NONE then
    begin
      if keyFinger[VK_LSTRAFE] <> 0 then
      begin
        keyFinger[VK_LEFT] := finger;
        keyFinger[VK_RIGHT] := 0;
        keyFinger[VK_STRAFE] := finger;
        e_KeyUpDown(VK_LEFT, true);
        e_KeyUpDown(VK_RIGHT, false);
        e_KeyUpDown(VK_STRAFE, true);
      end
      else if keyFinger[VK_RSTRAFE] <> 0 then
      begin
        keyFinger[VK_LEFT] := 0;
        keyFinger[VK_RIGHT] := finger;
        keyFinger[VK_STRAFE] := finger;
        e_KeyUpDown(VK_LEFT, false);
        e_KeyUpDown(VK_RIGHT, true);
        e_KeyUpDown(VK_STRAFE, true);
      end
      else
      begin
        keyFinger[VK_STRAFE] := 0;
        e_KeyUpDown(VK_STRAFE, false);
      end

    end;
  end;

  procedure g_Touch_Draw;
    var i: Integer;

    procedure Draw (i: Integer);
      var x, y, w, h: Integer; founded: Boolean;
    begin
      GetKeyRect(i, x, y, w, h, founded);
      if founded then
      begin
        e_DrawQuad(x, y, x + w, y + h, 0, 255, 0, 31);
        e_TextureFontPrintEx(x, y, GetKeyName(i), gStdFont, 255, 255, 255, 1, True)
      end;
    end;

  begin
{$IFNDEF HEADLESS}
    if not g_touch_enabled then
      Exit;

    for i := VK_FIRSTKEY to VK_LASTKEY do
      Draw(i);

    Draw(VS_KEYBOARD);
    Draw(VS_HIDEKEYBOARD);
{$ENDIF}
  end;

initialization
  conRegVar('touch_enable', @g_touch_enabled, 'enable/disable virtual buttons', 'draw buttons');
  conRegVar('touch_fire', @g_touch_fire, 'enable/disable fire when press virtual up/down', 'fire when press up/down');
  conRegVar('touch_size', @g_touch_size, 0.1, 10, 'size of virtual buttons', 'button size');
  conRegVar('touch_offset', @g_touch_offset, 0, 100, '', '');
  conRegVar('touch_alt', @g_touch_alt, 'althernative virtual buttons layout', 'althernative layout');
end.
