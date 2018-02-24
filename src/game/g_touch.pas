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

  procedure g_Touch_ShowKeyboard(yes: Boolean);
  procedure g_Touch_HandleEvent(const ev: TSDL_TouchFingerEvent);
  procedure g_Touch_Draw;

implementation

  uses
    SysUtils,
    e_log, e_graphics, e_input, g_options, g_game, g_main, g_weapons, g_console;

  const
    CTL_NONE  = 0;
    CTL_LEFT  = 1;
    CTL_RIGHT = 2;
    CTL_UP    = 3;
    CTL_DOWN  = 4;
    CTL_FIRE  = 5;
    CTL_OPEN  = 6;
    CTL_JUMP  = 7;
    CTL_CHAT  = 8;
    CTL_ESC   = 9;
    CTL_W0    = 10;
    CTL_W1    = 11;
    CTL_W2    = 12;
    CTL_W3    = 13;
    CTL_W4    = 14;
    CTL_W5    = 15;
    CTL_W6    = 16;
    CTL_W7    = 17;
    CTL_W8    = 18;
    CTL_W9    = 19;
    CTL_W10   = 20;
    CTL_CON   = 21;
    CTL_STAT  = 22;
    CTL_TCHAT = 23;
    CTL_LAST  = 23;

  var
    size: Single;
    enabled: Boolean;
    keyFinger: array [1..CTL_LAST] of Integer;

  procedure GetControlRect(control: Integer; out x, y, w, h: Integer; out founded: Boolean);
     var
       sw, sh, sz: Integer;
       dpi: Single;
  begin
    if SDL_GetDisplayDPI(0, @dpi, nil, nil) <> 0 then
      dpi := 96;

    founded := true;
    sz := Trunc(size * dpi);
    x := 0; y := 0; w := sz; h := sz;
    sw := gScreenWidth; sh := gScreenHeight;
    case control of
      CTL_LEFT:  begin x := 0;            y := sh div 2 - h div 2; end;
      CTL_RIGHT: begin x := w;            y := sh div 2 - h div 2; end;
      CTL_UP:    begin x := sw - w - 1;   y := sh div 2 - h div 2 - h; end;
      CTL_DOWN:  begin x := sw - w - 1;   y := sh div 2 - h div 2 + h; end;
      CTL_FIRE:  begin x := sw - 1*w - 1; y := sh div 2 - h div 2; end;
      CTL_OPEN:  begin x := sw - 3*w - 1; y := sh div 2 - h div 2; end;
      CTL_JUMP:  begin x := sw - 2*w - 1; y := sh div 2 - h div 2; end;
    else
      w := sz div 2; h := sz div 2;
      case control of
        CTL_W0:    begin x := sw div 2 - w div 2 - 5*w - 1; y := sh - 1*h - 1; end;
        CTL_W1:    begin x := sw div 2 - w div 2 - 4*w - 1; y := sh - 1*h - 1; end;
        CTL_W2:    begin x := sw div 2 - w div 2 - 3*w - 1; y := sh - 1*h - 1; end;
        CTL_W3:    begin x := sw div 2 - w div 2 - 2*w - 1; y := sh - 1*h - 1; end;
        CTL_W4:    begin x := sw div 2 - w div 2 - 1*w - 1; y := sh - 1*h - 1; end;
        CTL_W5:    begin x := sw div 2 - w div 2 + 0*w - 1; y := sh - 1*h - 1; end;
        CTL_W6:    begin x := sw div 2 - w div 2 + 1*w - 1; y := sh - 1*h - 1; end;
        CTL_W7:    begin x := sw div 2 - w div 2 + 2*w - 1; y := sh - 1*h - 1; end;
        CTL_W8:    begin x := sw div 2 - w div 2 + 3*w - 1; y := sh - 1*h - 1; end;
        CTL_W9:    begin x := sw div 2 - w div 2 + 4*w - 1; y := sh - 1*h - 1; end;
        CTL_W10:   begin x := sw div 2 - w div 2 + 5*w - 1; y := sh - 1*h - 1; end;
        CTL_CHAT:  begin x := sw div 2 - w div 2 - 2*w - 1; y := sh - 2*h - 1; end;
        CTL_ESC:   begin x := sw div 2 - w div 2 - 1*w - 1; y := sh - 2*h - 1; end;
        CTL_CON:   begin x := sw div 2 - w div 2 + 0*w - 1; y := sh - 2*h - 1; end;
        CTL_STAT:  begin x := sw div 2 - w div 2 + 1*w - 1; y := sh - 2*h - 1; end;
        CTL_TCHAT: begin x := sw div 2 - w div 2 + 2*w - 1; y := sh - 2*h - 1; end;
      else
        founded := false
      end
    end
  end;

  function GetMenuKey(control: Integer): Word;
  begin
    case control of
      CTL_LEFT:  result := IK_LEFT;
      CTL_RIGHT: result := IK_RIGHT;
      CTL_UP:    result := IK_UP;
      CTL_DOWN:  result := IK_DOWN;
      CTL_OPEN:  result := IK_ENTER;
      CTL_FIRE:  result := IK_ENTER;
      CTL_JUMP:  result := IK_SPACE;
      CTL_ESC:   result := IK_ESCAPE;
      CTL_W0:    result := SDL_SCANCODE_0;
      CTL_W1:    result := SDL_SCANCODE_1;
      CTL_W2:    result := SDL_SCANCODE_2;
      CTL_W3:    result := SDL_SCANCODE_3;
      CTL_W4:    result := SDL_SCANCODE_4;
      CTL_W5:    result := SDL_SCANCODE_5;
      CTL_W6:    result := SDL_SCANCODE_6;
      CTL_W7:    result := SDL_SCANCODE_7;
      CTL_W8:    result := SDL_SCANCODE_8;
      CTL_W9:    result := SDL_SCANCODE_9;
      CTL_CON:   result := IK_GRAVE;
    else
      result := IK_INVALID;
    end
  end;

  function GetPlayerKey(control: Integer): Word;
  begin
    case control of
      CTL_LEFT:  result := gGameControls.P1Control.KeyLeft;
      CTL_RIGHT: result := gGameControls.P1Control.KeyRight;
      CTL_UP:    result := gGameControls.P1Control.KeyUp;
      CTL_DOWN:  result := gGameControls.P1Control.KeyDown;
      CTL_OPEN:  result := gGameControls.P1Control.KeyOpen;
      CTL_FIRE:  result := gGameControls.P1Control.KeyFire;
      CTL_JUMP:  result := gGameControls.P1Control.KeyJump;
      CTL_CHAT:  result := gGameControls.GameControls.Chat;
      CTL_ESC:   result := IK_ESCAPE;
      CTL_W0:    result := gGameControls.P1Control.KeyWeapon[WEAPON_KASTET];
      CTL_W1:    result := gGameControls.P1Control.KeyWeapon[WEAPON_SAW];
      CTL_W2:    result := gGameControls.P1Control.KeyWeapon[WEAPON_PISTOL];
      CTL_W3:    result := gGameControls.P1Control.KeyWeapon[WEAPON_SHOTGUN1];
      CTL_W4:    result := gGameControls.P1Control.KeyWeapon[WEAPON_SHOTGUN2];
      CTL_W5:    result := gGameControls.P1Control.KeyWeapon[WEAPON_CHAINGUN];
      CTL_W6:    result := gGameControls.P1Control.KeyWeapon[WEAPON_ROCKETLAUNCHER];
      CTL_W7:    result := gGameControls.P1Control.KeyWeapon[WEAPON_PLASMA];
      CTL_W8:    result := gGameControls.P1Control.KeyWeapon[WEAPON_BFG];
      CTL_W9:    result := gGameControls.P1Control.KeyWeapon[WEAPON_SUPERPULEMET];
      CTL_W10:   result := gGameControls.P1Control.KeyWeapon[WEAPON_FLAMETHROWER];
      CTL_CON:   result := IK_GRAVE;
      CTL_STAT:  result := gGameControls.GameControls.Stat;
      CTL_TCHAT: result := gGameControls.GameControls.TeamChat;
    else
      result := IK_INVALID
    end
  end;

  function GetControlName(control: Integer): String;
  begin
    case control of
      CTL_LEFT:  result := 'LEFT';
      CTL_RIGHT: result := 'RIGHT';
      CTL_UP:    result := 'UP';
      CTL_DOWN:  result := 'DOWN';
      CTL_OPEN:  result := 'OPEN';
      CTL_FIRE:  result := 'FIRE';
      CTL_JUMP:  result := 'JUMP';
      CTL_CHAT:  result := 'CHAT';
      CTL_ESC:   result := 'ESC';
      CTL_W0:    result := '0';
      CTL_W1:    result := '1';
      CTL_W2:    result := '2';
      CTL_W3:    result := '3';
      CTL_W4:    result := '4';
      CTL_W5:    result := '5';
      CTL_W6:    result := '6';
      CTL_W7:    result := '7';
      CTL_W8:    result := '8';
      CTL_W9:    result := '9';
      CTL_W10:   result := '10';
      CTL_CON:   result := 'CON';
      CTL_STAT:  result := 'STAT';
      CTL_TCHAT: result := 'TEAM';
    else
      result := '(WAT?)'
    end
  end;

  procedure DrawRect(x, y, w, h: Integer);
  begin
    e_DrawQuad(x, y, x + w, y + h, 0, 255, 0, 127);
  end;

  function IntersectControl(ctl, xx, yy: Integer): Boolean;
    var
      x, y, w, h: Integer;
      founded: Boolean;
  begin
    GetControlRect(ctl, x, y, w, h, founded);
    result := founded and (xx >= x) and (yy >= y) and (xx <= x + w) and (yy <= y + h);
  end;

  procedure g_Touch_ShowKeyboard(yes: Boolean);
  begin
{$IFNDEF HEADLESS}
    if not enabled then
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
    if not enabled then
      Exit;
    if SDL_IsTextInputActive() = SDL_True then
      Exit;

    finger := ev.fingerId + 2;
    x := Trunc(ev.x * gScreenWidth);
    y := Trunc(ev.y * gScreenHeight);

    for i := 1 to CTL_LAST do
    begin
      if IntersectControl(i, x, y) then
      begin
        if ev.type_ = SDL_FINGERUP then
          keyFinger[i] := 0
        else if ev.type_ = SDL_FINGERMOTION then
          keyFinger[i] := finger
        else if ev.type_ = SDL_FINGERDOWN then
          begin
            KeyPress(GetMenuKey(i));
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

      e_KeyUpDown(GetPlayerKey(i), keyFinger[i] <> 0);
      e_KeyUpDown(GetMenuKey(i), keyFinger[i] <> 0);
    end;
  end;

  procedure g_Touch_Draw;
    var
      i, x, y, w, h: Integer;
      founded: Boolean;
  begin
{$IFNDEF HEADLESS}
    if not enabled then
      Exit;
    if SDL_IsTextInputActive() = SDL_True then
      Exit;

    for i := 1 to CTL_LAST do
    begin
      GetControlRect(i, x, y, w, h, founded);
      if founded then
      begin
        DrawRect(x, y, w, h);
        e_TextureFontPrint(x, y, GetControlName(i), gStdFont)
      end;
    end;
{$ENDIF}
  end;

initialization
{$IFDEF ANDROID}
  enabled := true;
{$ENDIF}
  size := 1;
  conRegVar('touch_enable', @enabled, 'enable/disable virtual buttons', 'draw buttons');
  conRegVar('touch_size', @size, 0.1, 10, 'size of virtual buttons', 'button size');
end.


