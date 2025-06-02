(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
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

implementation

uses
  SysUtils,
  e_input, e_log, e_graphics,
  g_main, g_options, g_window, g_console, g_game, g_gui, g_weapons;

var
  angleFire: Boolean;
  keyFinger: array [VK_FIRSTKEY..VK_LASTKEY] of Integer;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

function GetKeyRect (key: Integer; out x, y, w, h: Integer): Boolean;
var
  sw, sh, sz: Integer;
  dpi: Single;

  procedure S (xx, yy, ww, hh: Single);
  begin
    x := Trunc(xx);
    y := Trunc(yy);
    w := Trunc(ww);
    h := Trunc(hh);
    Result := True;
  end;

begin
  Result := False;

{$IFDEF SDL2_NODPI}
  dpi := 96;
{$ELSE}
  if SDL_GetDisplayDPI(0, @dpi, nil, nil) <> 0 then
    dpi := 96;
{$ENDIF}

  sz := Trunc(g_touch_size * dpi);
  sw := gWinSizeX;
  sh := gWinSizeY;

  x := 0;
  y := Round(sh * g_touch_offset / 100);
  w := sz;
  h := sz;

  if SDL_IsTextInputActive() = SDL_True then
  begin
    case key of
      VK_HIDEKBD: S(sw - (sz/2), 0, sz / 2, sz / 2);
    end;
  end
  else if g_touch_alt then
  begin
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
    end;
  end
  else
  begin
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
    end;
  end;
end;

function GetKeyName (key: Integer): String;
begin
  case key of
    VK_SHOWKBD: Result := 'KBD';
    VK_HIDEKBD: Result := 'KBD';
    VK_LEFT:    Result := 'LEFT';
    VK_RIGHT:   Result := 'RIGHT';
    VK_UP:      Result := 'UP';
    VK_DOWN:    Result := 'DOWN';
    VK_FIRE:    Result := 'FIRE';
    VK_OPEN:    Result := 'OPEN';
    VK_JUMP:    Result := 'JUMP';
    VK_CHAT:    Result := 'CHAT';
    VK_ESCAPE:  Result := 'ESC';
    VK_0:       Result := '0';
    VK_1:       Result := '1';
    VK_2:       Result := '2';
    VK_3:       Result := '3';
    VK_4:       Result := '4';
    VK_5:       Result := '5';
    VK_6:       Result := '6';
    VK_7:       Result := '7';
    VK_8:       Result := '8';
    VK_9:       Result := '9';
    VK_A:       Result := '10';
    VK_B:       Result := '11';
    VK_C:       Result := '12';
    VK_D:       Result := '13';
    VK_E:       Result := '14';
    VK_F:       Result := '15';
    VK_CONSOLE: Result := 'CON';
    VK_STATUS:  Result := 'STAT';
    VK_TEAM:    Result := 'TEAM';
    VK_PREV:    Result := '<PREV';
    VK_NEXT:    Result := 'NEXT>';
    VK_LSTRAFE: Result := '<';
    VK_RSTRAFE: Result := '>';
  else
    if (key > 0) and (key < e_MaxInputKeys)
      then Result := e_KeyNames[key]
      else Result := '<' + IntToStr(key) + '>';
  end;
end;

function IntersectControl (ctl, xx, yy: Integer): Boolean;
var
  x, y, w, h: Integer;
  found: Boolean;
begin
  found := GetKeyRect(ctl, x, y, w, h);
  Result := found and (xx >= x) and (yy >= y) and (xx <= x + w) and (yy <= y + h);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure g_Touch_HandleEvent (const ev: TSDL_TouchFingerEvent);
var
  x, y, i, finger: Integer;

  procedure KeyUp (finger, i: Integer);
  begin
    if g_dbg_input then
      e_LogWritefln('Input Debug: g_touch.KeyUp, finger=%s, key=%s', [finger, i]);

    keyFinger[i] := 0;
    e_KeyUpDown(i, False);
    g_Console_ProcessBind(i, False);

    // up/down + fire hack
    if g_touch_fire and (gGameSettings.GameType <> GT_NONE) and (g_ActiveWindow = nil) and angleFire then
    begin
      if (i = VK_UP) or (i = VK_DOWN) then
      begin
        angleFire := False;
        keyFinger[VK_FIRE] := 0;
        e_KeyUpDown(VK_FIRE, False);
        g_Console_ProcessBind(VK_FIRE, False);
      end;
    end;
  end;

  procedure KeyDown (finger, i: Integer);
  begin
    if g_dbg_input then
      e_LogWritefln('Input Debug: g_touch.KeyDown, finger=%s, key=%s', [finger, i]);

    keyFinger[i] := finger;
    e_KeyUpDown(i, True);
    g_Console_ProcessBind(i, True);

    // up/down + fire hack
    if g_touch_fire and (gGameSettings.GameType <> GT_NONE) and (g_ActiveWindow = nil) then
    begin
      if (i = VK_UP) or (i = VK_DOWN) then
      begin
        angleFire := True;
        keyFinger[VK_FIRE] := -1;
        e_KeyUpDown(VK_FIRE, True);
        g_Console_ProcessBind(VK_FIRE, True);
      end;
    end;
  end;

  procedure KeyMotion (finger, i: Integer);
  begin
    if keyFinger[i] <> finger then
    begin
      KeyUp(finger, i);
      KeyDown(finger, i);
    end;
  end;

begin
  // Always enable touchscreen in menu, so that player doesn't get locked out
  if (not g_touch_enabled) and (g_ActiveWindow = nil) then
    Exit;

  finger := ev.fingerId + 2;
  x := Trunc(ev.x * gWinSizeX);
  y := Trunc(ev.y * gWinSizeY);

  for i := VK_FIRSTKEY to VK_LASTKEY do
  begin
    if IntersectControl(i, x, y) then
    begin
      if ev.type_ = SDL_FINGERUP then
        KeyUp(finger, i)
      else if ev.type_ = SDL_FINGERMOTION then
        KeyMotion(finger, i)
      else if ev.type_ = SDL_FINGERDOWN then
        keyDown(finger, i);
    end
    else if keyFinger[i] = finger then
    begin
      if ev.type_ = SDL_FINGERUP then
        KeyUp(finger, i)
      else if ev.type_ = SDL_FINGERMOTION then
        KeyUp(finger, i);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////

procedure g_Touch_Init ();
begin
{$IFNDEF HEADLESS}
  g_Touch_ShowKeyboard(False);
  g_touch_devices_detected := SDL_GetNumTouchDevices() > 0;
  // Set this now, because it may not be set anywhere else.
  // If it's set in user config, it will be overwritten. If it's not, that's a reasonable default.
  g_touch_enabled := g_touch_devices_detected;
{$ENDIF}
end;

procedure g_Touch_ShowKeyboard (yes: Boolean);
begin
{$IFNDEF HEADLESS}
  if g_dbg_input then
    e_LogWritefln('g_Touch_ShowKeyboard(%s)', [yes]);

  // on desktop we always receive text (needed for cheats)
  if yes or (SDL_HasScreenKeyboardSupport() = SDL_FALSE)
    then SDL_StartTextInput()
    else SDL_StopTextInput();
{$ENDIF}
end;

procedure g_Touch_Draw ();
var
  i, x, y, w, h: Integer;
  found: Boolean;
begin
{$IFNDEF HEADLESS}
  // Always enable touchscreen in menu, so that player doesn't get locked out
  if (not g_touch_enabled) and (g_ActiveWindow = nil) then
    Exit;

  for i := VK_FIRSTKEY to VK_LASTKEY do
  begin
    found := GetKeyRect(i, x, y, w, h);
    if found then
    begin
      e_DrawQuad(x, y, x + w, y + h, 0, 255, 0, 31);
      e_TextureFontPrintEx(x, y, GetKeyName(i), gStdFont, 255, 255, 255, 1, True);
    end;
  end;
{$ENDIF}
end;
