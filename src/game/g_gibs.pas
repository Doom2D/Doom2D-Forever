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
unit g_gibs;

interface

  uses g_phys, g_base;

  const
    DefaultGibsCount = 32;
    DefaultGibsMax = 150;

  type
    PGib = ^TGib;
    TGib = record
      alive:    Boolean;
      RAngle:   Integer;
      Color:    TRGB;
      Obj:      TObj;
      ModelID: Integer;
      GibID: Integer;

      procedure getMapBox (out x, y, w, h: Integer); inline;
      procedure moveBy (dx, dy: Integer); inline;
      procedure positionChanged; inline; //WARNING! call this after entity position was changed, or coldet will not w>
    end;

  var
    gGibsCount: Integer = DefaultGibsCount; // !!! make it private
    gGibs: Array of TGib;

  procedure g_Gibs_SetMax (Count: Word);
  function g_Gibs_GetMax (): Word;

  procedure g_Gibs_Create (fX, fY, mid: Integer; fColor: TRGB);
  procedure g_Gibs_Update;

implementation

  uses
    {$IFDEF ENABLE_GFX}
      g_gfx,
    {$ENDIF}
    {$IFNDEF HEADLESS}
      r_render,
    {$ENDIF}
    g_playermodel, g_options, g_game
  ;

  type
    TGibsArray = Array of Integer;

  var
    CurrentGib: Integer = 0;
    MaxGibs: Word = DefaultGibsMax;

  procedure TGib.getMapBox (out x, y, w, h: Integer); inline;
  begin
    x := Obj.X + Obj.Rect.X;
    y := Obj.Y + Obj.Rect.Y;
    w := Obj.Rect.Width;
    h := Obj.Rect.Height;
  end;

  procedure TGib.moveBy (dx, dy: Integer); inline;
  begin
    if (dx <> 0) or (dy <> 0) then
    begin
      Obj.X += dx;
      Obj.Y += dy;
      positionChanged;
    end;
  end;

  procedure TGib.positionChanged (); inline;
  begin
  end;

  procedure g_Gibs_SetMax (Count: Word);
  begin
    MaxGibs := Count;
    SetLength(gGibs, Count);
    if CurrentGib >= Count then
      CurrentGib := 0;
  end;

  function g_Gibs_GetMax (): Word;
  begin
    Result := MaxGibs;
  end;

  function g_Gibs_Get (ModelID: Integer; var Gibs: TGibsArray): Boolean;
    var i, b: Integer; c: Boolean;
  begin
    Gibs := nil;
    Result := False;
    if (PlayerModelsArray = nil) or (gGibsCount = 0) then
      Exit;

    c := False;
    SetLength(Gibs, gGibsCount);
    for i := 0 to High(Gibs) do
    begin
      if c and (PlayerModelsArray[ModelID].GibsCount = 1) then
      begin
        SetLength(Gibs, i);
        Break;
      end;

      repeat
        b := Random(PlayerModelsArray[ModelID].GibsCount);
      until not ((PlayerModelsArray[ModelID].GibsOnce = b + 1) and c);

      Gibs[i] := b;

      c := PlayerModelsArray[ModelID].GibsOnce = b + 1;
    end;
    Result := True;
  end;

  procedure g_Gibs_Create (fX, fY, mid: Integer; fColor: TRGB);
    var
      a: Integer;
      GibsArray: TGibsArray;
      {$IFDEF ENABLE_GFX}
        Blood: TModelBlood;
      {$ENDIF}
  begin
    if mid = -1 then
      Exit;
    if (gGibs = nil) or (Length(gGibs) = 0) then
      Exit;
    if not g_Gibs_Get(mid, GibsArray) then
      Exit;

    {$IFDEF ENABLE_GFX}
      Blood := PlayerModelsArray[mid].Blood;
    {$ENDIF}

    for a := 0 to High(GibsArray) do
    begin
      with gGibs[CurrentGib] do
      begin
        ModelID := mid;
        GibID := GibsArray[a];
        Color := fColor;
        alive := True;
        g_Obj_Init(@Obj);
        {$IFNDEF HEADLESS}
          Obj.Rect := r_Render_GetGibRect(ModelID, GibID);
        {$ELSE}
          Obj.Rect.X := 16;
          Obj.Rect.Y := 16;
          Obj.Rect.Width := 16;
          Obj.Rect.Height := 16;
        {$ENDIF}
        Obj.X := fX - Obj.Rect.X - (Obj.Rect.Width div 2);
        Obj.Y := fY - Obj.Rect.Y - (Obj.Rect.Height div 2);
        g_Obj_PushA(@Obj, 25 + Random(10), Random(361));
        positionChanged; // this updates spatial accelerators
        RAngle := Random(360);
        {$IFDEF ENABLE_GFX}
          if gBloodCount > 0 then
          begin
            g_GFX_Blood(
              fX,
              fY,
              16 * gBloodCount + Random(5 * gBloodCount),
              -16 + Random(33),
              -16 + Random(33),
              Random(48),
              Random(48),
              Blood.R,
              Blood.G,
              Blood.B,
              Blood.Kind
           );
          end;
        {$ENDIF}
        if CurrentGib >= High(gGibs) then
          CurrentGib := 0
        else
          Inc(CurrentGib);
      end;
    end;
  end;

  procedure g_Gibs_Update;
    var i: Integer; vel: TPoint2i; mr: Word;
  begin
    if gGibs = nil then
      Exit;
    for i := 0 to High(gGibs) do
      if gGibs[i].alive then
        with gGibs[i] do
        begin
          Obj.oldX := Obj.X;
          Obj.oldY := Obj.Y;

          vel := Obj.Vel;
          mr := g_Obj_Move(@Obj, True, False, True);
          positionChanged(); // this updates spatial accelerators

          if WordBool(mr and MOVE_FALLOUT) then
          begin
            alive := False;
            Continue;
          end;

        // Отлетает от удара о стену/потолок/пол:
          if WordBool(mr and MOVE_HITWALL) then
            Obj.Vel.X := -(vel.X div 2);
          if WordBool(mr and (MOVE_HITCEIL or MOVE_HITLAND)) then
            Obj.Vel.Y := -(vel.Y div 2);

          if (Obj.Vel.X >= 0) then
          begin // Clockwise
            RAngle := RAngle + Abs(Obj.Vel.X)*6 + Abs(Obj.Vel.Y);
            if RAngle >= 360 then
              RAngle := RAngle mod 360;
          end else begin // Counter-clockwise
            RAngle := RAngle - Abs(Obj.Vel.X)*6 - Abs(Obj.Vel.Y);
            if RAngle < 0 then
              RAngle := (360 - (Abs(RAngle) mod 360)) mod 360;
          end;

        // Сопротивление воздуха для куска трупа:
          if gTime mod (GAME_TICK*3) = 0 then
            Obj.Vel.X := z_dec(Obj.Vel.X, 1);
        end;
  end;

end.
