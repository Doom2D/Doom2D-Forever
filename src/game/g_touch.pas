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

  var
    angleFire: Boolean;
    keyFinger: array [VK_FIRSTKEY..VK_LASTKEY] of Integer;

  procedure GetKeyRect(key: Word; out x, y, w, h: Integer; out founded: Boolean);
    var
      sw, sh, sz: Integer;
      dpi: Single;
  begin
    if SDL_GetDisplayDPI(0, @dpi, nil, nil) <> 0 then
      dpi := 96;

    founded := true;
    sz := Trunc(g_touch_size * dpi);
    sw := gScreenWidth; sh := gScreenHeight;
    if g_touch_alt then
    begin
      w := sz div 2; h := sz div 2;
      case key of
        VK_CONSOLE: begin x := 0; y := 0 end;
        VK_ESCAPE:  begin x := sw - w - 1; y := 0 end;
        VK_CHAT:    begin x := sw div 2 - w div 2 - w; y := 0 end;
        VK_STATUS:  begin x := sw div 2 - w div 2 + 0; y := 0 end;
        VK_TEAM:    begin x := sw div 2 - w div 2 + w; y := 0 end;
        VK_PREV:    begin x := 0; y := sh - 4*sz - 1; w := sz end;
        VK_NEXT:    begin x := sw - sz - 1; y := sh - 4*sz - 1; w := sz end;
      else
        w := sz; h := sz * 3;
        case key of
          VK_LEFT:  begin x := 0; y := sh - h - 1 end;
          VK_RIGHT: begin x := w; y := sh - h - 1 end;
        else
          w := sz; h := sz;
          case key of
            VK_UP:   begin x := sw - 2*w - 1; y := sh - 3*h - 1 end;
            VK_FIRE: begin x := sw - 2*w - 1; y := sh - 2*h - 1 end;
            VK_DOWN: begin x := sw - 2*w - 1; y := sh - 1*h - 1 end;
            VK_OPEN: begin x := sw - 1*w - 1; y := sh - 1*h - h div 2 - 1 end;
            VK_JUMP: begin x := sw - 1*w - 1; y := sh - 2*h - h div 2 - 1 end;
          else
            founded := false
          end
        end
      end
    end
    else
    begin
      x := 0; y := Round(sh * g_touch_offset / 100); w := sz; h := sz;
      case key of
        VK_LSTRAFE: begin x := 0;             y := y - h div 2; w := w div 2 end;
        VK_LEFT:    begin x := w div 2;       y := y - h div 2 end;
        VK_RIGHT:   begin x := w div 2 + 1*w; y := y - h div 2 end;
        VK_RSTRAFE: begin x := w div 2 + 2*w; y := y - h div 2; w := w div 2 end;
        VK_UP:      begin x := sw - w - 1;    y := y - h div 2 - h end;
        VK_FIRE:    begin x := sw - 1*w - 1;  y := y - h div 2 end;
        VK_DOWN:    begin x := sw - w - 1;    y := y - h div 2 + h end;
        VK_NEXT:    begin x := sw - 2*w - 1;  y := y - h div 2 - h end;
        VK_JUMP:    begin x := sw - 2*w - 1;  y := y - h div 2 end;
        VK_PREV:    begin x := sw - 3*w - 1;  y := y - h div 2 - h end;
        VK_OPEN:    begin x := sw - 3*w - 1;  y := y - h div 2 end;
      else
        x := 0; y := 0; w := sz div 2; h := sz div 2;
        case key of
          VK_0:       begin x := sw div 2 - w div 2 - 5*w - 1; y := sh - 1*h - 1 end;
          VK_1:       begin x := sw div 2 - w div 2 - 4*w - 1; y := sh - 1*h - 1 end;
          VK_2:       begin x := sw div 2 - w div 2 - 3*w - 1; y := sh - 1*h - 1 end;
          VK_3:       begin x := sw div 2 - w div 2 - 2*w - 1; y := sh - 1*h - 1 end;
          VK_4:       begin x := sw div 2 - w div 2 - 1*w - 1; y := sh - 1*h - 1 end;
          VK_5:       begin x := sw div 2 - w div 2 + 0*w - 1; y := sh - 1*h - 1 end;
          VK_6:       begin x := sw div 2 - w div 2 + 1*w - 1; y := sh - 1*h - 1 end;
          VK_7:       begin x := sw div 2 - w div 2 + 2*w - 1; y := sh - 1*h - 1 end;
          VK_8:       begin x := sw div 2 - w div 2 + 3*w - 1; y := sh - 1*h - 1 end;
          VK_9:       begin x := sw div 2 - w div 2 + 4*w - 1; y := sh - 1*h - 1 end;
          VK_A:       begin x := sw div 2 - w div 2 + 5*w - 1; y := sh - 1*h - 1 end;
          VK_CHAT:    begin x := sw div 2 - w div 2 - 2*w - 1; y := sh - 2*h - 1 end;
          VK_ESCAPE:  begin x := sw div 2 - w div 2 - 1*w - 1; y := sh - 2*h - 1 end;
          VK_CONSOLE: begin x := sw div 2 - w div 2 + 0*w - 1; y := sh - 2*h - 1 end;
          VK_STATUS:  begin x := sw div 2 - w div 2 + 1*w - 1; y := sh - 2*h - 1 end;
          VK_TEAM:    begin x := sw div 2 - w div 2 + 2*w - 1; y := sh - 2*h - 1 end;
        else
          founded := false
        end
      end
    end
  end;

  function GetKeyName(key: Word): String;
  begin
    case key of
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
    if SDL_IsTextInputActive() = SDL_True then
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
    var
      i, x, y, w, h: Integer;
      founded: Boolean;
  begin
{$IFNDEF HEADLESS}
    if not g_touch_enabled then
      Exit;
    if SDL_IsTextInputActive() = SDL_True then
      Exit;

    for i := VK_FIRSTKEY to VK_LASTKEY do
    begin
      GetKeyRect(i, x, y, w, h, founded);
      if founded then
      begin
        e_DrawQuad(x, y, x + w, y + h, 0, 255, 0, 31);
        e_TextureFontPrintEx(x, y, GetKeyName(i), gStdFont, 255, 255, 255, 1, True)
      end;
    end;
{$ENDIF}
  end;

initialization
  conRegVar('touch_enable', @g_touch_enabled, 'enable/disable virtual buttons', 'draw buttons');
  conRegVar('touch_fire', @g_touch_fire, 'enable/disable fire when press virtual up/down', 'fire when press up/down');
  conRegVar('touch_size', @g_touch_size, 0.1, 10, 'size of virtual buttons', 'button size');
  conRegVar('touch_offset', @g_touch_offset, 0, 100, '', '');
  conRegVar('touch_alt', @g_touch_alt, 'althernative virtual buttons layout', 'althernative layout');
end.
