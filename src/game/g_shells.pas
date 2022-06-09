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
unit g_shells;

interface

  uses g_phys;

  const
    SHELL_BULLET   = 0;
    SHELL_SHELL    = 1;
    SHELL_DBLSHELL = 2;
    SHELL_LAST     = SHELL_DBLSHELL;

    DefaultShellTimeout = 60000;
    DefaultShellMax = 300;

  type
    PShell = ^TShell;
    TShell = record
      alive:    Boolean;
      SType:    Byte;
      RAngle:   Integer;
      Timeout:  Cardinal;
      Obj:      TObj;

      procedure getMapBox (out x, y, w, h: Integer); inline;
      procedure moveBy (dx, dy: Integer); inline;

      procedure positionChanged ();  inline; //WARNING! call this after entity position was changed, or coldet will not >
    end;

  var
    gShells: Array of TShell;

  procedure g_Shells_SetMax (Count: Word);
  function g_Shells_GetMax (): Word;

  procedure g_Shells_Create (fX, fY, dX, dY: Integer; T: Byte);
  procedure g_Shells_RemoveAll;
  procedure g_Shells_Update;

implementation

  uses SysUtils, g_base, g_game, g_sound;

  var
    SHELL_TIMEOUT: Cardinal = DefaultShellTimeout;
    MaxShells: Word = DefaultShellMax;
    CurrentShell: Integer = 0;

  procedure TShell.getMapBox (out x, y, w, h: Integer); inline;
  begin
    x := Obj.X;
    y := Obj.Y;
    w := Obj.Rect.Width;
    h := Obj.Rect.Height;
  end;

  procedure TShell.moveBy (dx, dy: Integer); inline;
  begin
    if (dx <> 0) or (dy <> 0) then
    begin
      Obj.X += dx;
      Obj.Y += dy;
      positionChanged;
    end;
  end;

  procedure TShell.positionChanged (); inline;
  begin
  end;

  procedure g_Shells_SetMax (Count: Word);
  begin
    MaxShells := Count;
    SetLength(gShells, Count);
    if CurrentShell >= Count then
      CurrentShell := 0;
  end;

  function g_Shells_GetMax (): Word;
  begin
    Result := MaxShells;
  end;

  procedure g_Shells_Create (fX, fY, dX, dY: Integer; T: Byte);
  begin
    if (gShells = nil) or (Length(gShells) = 0) then
      Exit;

    with gShells[CurrentShell] do
    begin
      g_Obj_Init(@Obj);
      Obj.Rect.X := 0;
      Obj.Rect.Y := 0;
      if T = SHELL_BULLET then
      begin
        Obj.Rect.Width := 4;
        Obj.Rect.Height := 2;
      end
      else
      begin
        Obj.Rect.Width := 7;
        Obj.Rect.Height := 3;
      end;
      SType := T;
      alive := True;
      Obj.X := fX;
      Obj.Y := fY;
      g_Obj_Push(@Obj, dX + Random(4)-Random(4), dY-Random(4));
      positionChanged(); // this updates spatial accelerators
      RAngle := Random(360);
      Timeout := gTime + SHELL_TIMEOUT;
      if CurrentShell >= High(gShells) then
        CurrentShell := 0
      else
        Inc(CurrentShell);
    end;
  end;

  procedure g_Shells_RemoveAll;
    var i: Integer;
  begin
    i := g_Shells_GetMax();
    g_Shells_SetMax(0);
    g_Shells_SetMax(i);
  end;

  procedure g_Shells_SoundBounce(X, Y: Integer; T: Byte);
    var k: Integer;
  begin
    k := 1 + Random(2);
    if T = SHELL_BULLET then
      g_Sound_PlayExAt('SOUND_PLAYER_CASING' + IntToStr(k), X, Y)
    else
      g_Sound_PlayExAt('SOUND_PLAYER_SHELL' + IntToStr(k), X, Y);
  end;

  procedure g_Shells_Update;
    var i: Integer; vel: TPoint2i; mr: Word;
  begin
    if gShells = nil then
      Exit;

    for i := 0 to High(gShells) do
    begin
      if gShells[i].alive then
      begin
        with gShells[i] do
        begin
          Obj.oldX := Obj.X;
          Obj.oldY := Obj.Y;

          vel := Obj.Vel;
          mr := g_Obj_Move(@Obj, True, False, True);
          positionChanged(); // this updates spatial accelerators

          if WordBool(mr and MOVE_FALLOUT) or (gShells[i].Timeout < gTime) then
          begin
            alive := False;
            Continue;
          end;

        // Отлетает от удара о стену/потолок/пол:
          if WordBool(mr and MOVE_HITWALL) then
          begin
            Obj.Vel.X := -(vel.X div 2);
            if not WordBool(mr and MOVE_INWATER) then
              g_Shells_SoundBounce(Obj.X, Obj.Y, SType);
          end;
          if WordBool(mr and (MOVE_HITCEIL or MOVE_HITLAND)) then
          begin
            Obj.Vel.Y := -(vel.Y div 2);
            if Obj.Vel.X <> 0 then Obj.Vel.X := Obj.Vel.X div 2;
            if (Obj.Vel.X = 0) and (Obj.Vel.Y = 0) then
            begin
              if RAngle mod 90 <> 0 then
                RAngle := (RAngle div 90) * 90;
            end
            else if not WordBool(mr and MOVE_INWATER) then
              g_Shells_SoundBounce(Obj.X, Obj.Y, SType);
          end;

          if (Obj.Vel.X >= 0) then
          begin // Clockwise
            RAngle := RAngle + Abs(Obj.Vel.X)*8 + Abs(Obj.Vel.Y);
            if RAngle >= 360 then
              RAngle := RAngle mod 360;
          end else begin // Counter-clockwise
            RAngle := RAngle - Abs(Obj.Vel.X)*8 - Abs(Obj.Vel.Y);
            if RAngle < 0 then
              RAngle := (360 - (Abs(RAngle) mod 360)) mod 360;
          end;
        end;
      end;
    end;
  end;

end.
