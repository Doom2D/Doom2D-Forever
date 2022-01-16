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
unit r_player;

interface

  uses g_player, g_base; // TPlayer, TRGB

  procedure r_Player_Load;
  procedure r_Player_Free;

  procedure r_Player_DrawAll;
  procedure r_Player_DrawDebug (p: TPlayer);
  procedure r_Player_DrawHealth;

  procedure r_Player_DrawCorpses;
  procedure r_Player_DrawShells;

  procedure r_Player_Draw (p: TPlayer);
  procedure r_Player_DrawIndicator (p: TPlayer; Color: TRGB);
  procedure r_Player_DrawBubble (p: TPlayer);
  procedure r_Player_DrawAim (p: TPlayer);
  procedure r_Player_DrawGUI (pl: TPlayer);
  procedure r_Player_DrawRulez (p: TPlayer);
  procedure r_Player_DrawPain (p: TPlayer);
  procedure r_Player_DrawPickup (p: TPlayer);

implementation

  uses
    SysUtils, Classes, Math,
    MAPDEF, utils,
    g_basic, g_game, g_phys, g_map, g_menu, g_language, g_weapons, g_items, g_net, g_options,
{$IFDEF ENABLE_HOLMES}
    g_holmes,
{$ENDIF}
    r_playermodel, r_graphics, r_animations, r_textures, r_items
  ;

  var
    PunchFrames: array [Boolean, 0..2] of DWORD;

  procedure r_Player_Load;
  begin
    g_Frames_CreateWAD(@PunchFrames[False, 0], 'FRAMES_PUNCH', GameWAD + ':WEAPONS\PUNCH', 64, 64, 4, False);
    g_Frames_CreateWAD(@PunchFrames[False, 1], 'FRAMES_PUNCH_UP', GameWAD + ':WEAPONS\PUNCH_UP', 64, 64, 4, False);
    g_Frames_CreateWAD(@PunchFrames[False, 2], 'FRAMES_PUNCH_DN', GameWAD + ':WEAPONS\PUNCH_DN', 64, 64, 4, False);
    g_Frames_CreateWAD(@PunchFrames[True, 0], 'FRAMES_PUNCH_BERSERK', GameWAD + ':WEAPONS\PUNCHB', 64, 64, 4, False);
    g_Frames_CreateWAD(@PunchFrames[True, 1], 'FRAMES_PUNCH_BERSERK_UP', GameWAD + ':WEAPONS\PUNCHB_UP', 64, 64, 4, False);
    g_Frames_CreateWAD(@PunchFrames[True, 2], 'FRAMES_PUNCH_BERSERK_DN', GameWAD + ':WEAPONS\PUNCHB_DN', 64, 64, 4, False);
  end;

  procedure r_Player_Free;
  begin
    g_Frames_DeleteByName('FRAMES_PUNCH');
    g_Frames_DeleteByName('FRAMES_PUNCH_UP');
    g_Frames_DeleteByName('FRAMES_PUNCH_DN');
    g_Frames_DeleteByName('FRAMES_PUNCH_BERSERK');
    g_Frames_DeleteByName('FRAMES_PUNCH_BERSERK_UP');
    g_Frames_DeleteByName('FRAMES_PUNCH_BERSERK_DN');
  end;

  procedure r_Player_DrawAll;
    var i: Integer;
  begin
    if gPlayers <> nil then
      for i := 0 to High(gPlayers) do
        if gPlayers[i] <> nil then
          r_Player_Draw(gPlayers[i])
  end;

procedure r_Player_DrawDebug (p: TPlayer);
var
  fW, fH: Byte;
begin
  if p = nil then Exit;
  if (@p.Obj) = nil then Exit;

  e_TextureFontGetSize(gStdFont, fW, fH);

  e_TextureFontPrint(0, 0     , 'Pos X: ' + IntToStr(p.Obj.X), gStdFont);
  e_TextureFontPrint(0, fH    , 'Pos Y: ' + IntToStr(p.Obj.Y), gStdFont);
  e_TextureFontPrint(0, fH * 2, 'Vel X: ' + IntToStr(p.Obj.Vel.X), gStdFont);
  e_TextureFontPrint(0, fH * 3, 'Vel Y: ' + IntToStr(p.Obj.Vel.Y), gStdFont);
  e_TextureFontPrint(0, fH * 4, 'Acc X: ' + IntToStr(p.Obj.Accel.X), gStdFont);
  e_TextureFontPrint(0, fH * 5, 'Acc Y: ' + IntToStr(p.Obj.Accel.Y), gStdFont);
  e_TextureFontPrint(0, fH * 6, 'Old X: ' + IntToStr(p.Obj.oldX), gStdFont);
  e_TextureFontPrint(0, fH * 7, 'Old Y: ' + IntToStr(p.Obj.oldY), gStdFont);
end;

procedure r_Player_DrawHealth;
var
  i: Integer;
  fW, fH: Byte;
begin
  if gPlayers = nil then Exit;
  e_TextureFontGetSize(gStdFont, fW, fH);

  for i := 0 to High(gPlayers) do
    if gPlayers[i] <> nil then
    begin
      e_TextureFontPrint(gPlayers[i].Obj.X + gPlayers[i].Obj.Rect.X,
      gPlayers[i].Obj.Y + gPlayers[i].Obj.Rect.Y + gPlayers[i].Obj.Rect.Height - fH * 2,
      IntToStr(gPlayers[i].Health), gStdFont);
      e_TextureFontPrint(gPlayers[i].Obj.X + gPlayers[i].Obj.Rect.X,
      gPlayers[i].Obj.Y + gPlayers[i].Obj.Rect.Y + gPlayers[i].Obj.Rect.Height - fH,
      IntToStr(gPlayers[i].Armor), gStdFont);
    end;
end;

  procedure r_Player_DrawCorpse (p: TCorpse);
    var fX, fY: Integer;
  begin
    if (p.State <> CORPSE_STATE_REMOVEME) and (p.Model <> nil) then
    begin
      p.Obj.lerp(gLerpFactor, fX, fY);
      r_PlayerModel_Draw(p.Model, fX, fY)
    end
  end;

  procedure r_Player_DrawCorpses;
    var i: Integer;
  begin
    if gCorpses <> nil then
      for i := 0 to High(gCorpses) do
        if gCorpses[i] <> nil then
          r_Player_DrawCorpse(gCorpses[i])
  end;

procedure r_Player_DrawShells;
var
  i, fX, fY: Integer;
  a: TDFPoint;
begin
  if gShells <> nil then
    for i := 0 to High(gShells) do
      if gShells[i].alive then
        with gShells[i] do
        begin
          if not g_Obj_Collide(sX, sY, sWidth, sHeight, @Obj) then
            Continue;

          Obj.lerp(gLerpFactor, fX, fY);

          a.X := CX;
          a.Y := CY;

          e_DrawAdv(SpriteID, fX, fY, 0, True, False, RAngle, @a, TMirrorType.None);
        end;
end;

procedure r_Player_DrawIndicator (p: TPlayer; Color: TRGB);
var
  indX, indY, fX, fY, fSlope: Integer;
  indW, indH: Word;
  indA: Single;
  a: TDFPoint;
  nW, nH: Byte;
  ID: DWORD;
  c: TRGB;
begin
  if p.Alive then
  begin
    p.Obj.lerp(gLerpFactor, fX, fY);
    fSlope := nlerp(p.SlopeOld, p.Obj.slopeUpLeft, gLerpFactor);

    case gPlayerIndicatorStyle of
      0:
        begin
          if g_Texture_Get('TEXTURE_PLAYER_INDICATOR', ID) then
          begin
            e_GetTextureSize(ID, @indW, @indH);
            a.X := indW div 2;
            a.Y := indH div 2;

            if (p.Obj.X + p.Obj.Rect.X) < 0 then
            begin
              indA := 90;
              indX := fX + p.Obj.Rect.X + p.Obj.Rect.Width;
              indY := fY + p.Obj.Rect.Y + (p.Obj.Rect.Height - indW) div 2;
            end

            else if (p.Obj.X + p.Obj.Rect.X + p.Obj.Rect.Width) > Max(gMapInfo.Width, gPlayerScreenSize.X) then
            begin
              indA := 270;
              indX := fX + p.Obj.Rect.X - indH;
              indY := fY + p.Obj.Rect.Y + (p.Obj.Rect.Height - indW) div 2;
            end

            else if (p.Obj.Y - indH) < 0 then
            begin
              indA := 180;
              indX := fX + p.Obj.Rect.X + (p.Obj.Rect.Width - indW) div 2;
              indY := fY + p.Obj.Rect.Y + p.Obj.Rect.Height;
            end

            else
            begin
              indA := 0;
              indX := fX + p.Obj.Rect.X + (p.Obj.Rect.Width - indW) div 2;
              indY := fY - indH;
            end;

            indY := indY + fSlope;
            indX := EnsureRange(indX, 0, Max(gMapInfo.Width, gPlayerScreenSize.X) - indW);
            indY := EnsureRange(indY, 0, Max(gMapInfo.Height, gPlayerScreenSize.Y) - indH);

            c := e_Colors;
            e_Colors := Color;
            e_DrawAdv(ID, indX, indY, 0, True, False, indA, @a);
            e_Colors := c;
          end;
        end;

      1:
        begin
          e_TextureFontGetSize(gStdFont, nW, nH);
          indX := fX + p.Obj.Rect.X + (p.Obj.Rect.Width - Length(p.Name) * nW) div 2;
          indY := fY - nH + fSlope;
          e_TextureFontPrintEx(indX, indY, p.Name, gStdFont, Color.R, Color.G, Color.B, 1.0, True);
        end;
    end;
  end
end;

procedure r_Player_DrawBubble (p: TPlayer);
var
  bubX, bubY, fX, fY: Integer;
  ID: LongWord;
  Rb, Gb, Bb,
  Rw, Gw, Bw: SmallInt;
  Dot: Byte;
  CObj: TObj;
begin
  CObj := p.getCameraObj();
  CObj.lerp(gLerpFactor, fX, fY);
  // NB: _F_Obj.Rect is used to keep the bubble higher; this is not a mistake
  bubX := fX + p.Obj.Rect.X + IfThen(p.Direction = TDirection.D_LEFT, -4, 18);
  bubY := fY + p.Obj.Rect.Y - 18;
  Rb := 64;
  Gb := 64;
  Bb := 64;
  Rw := 240;
  Gw := 240;
  Bw := 240;
  case gChatBubble of
    1: // simple textual non-bubble
    begin
      bubX := fX + p.Obj.Rect.X - 11;
      bubY := fY + p.Obj.Rect.Y - 17;
      e_TextureFontPrint(bubX, bubY, '[...]', gStdFont);
      Exit;
    end;
    2: // advanced pixel-perfect bubble
    begin
      if p.Team = TEAM_RED then
        Rb := 255
      else
        if p.Team = TEAM_BLUE then
          Bb := 255;
    end;
    3: // colored bubble
    begin
      Rb := p.Model.Color.R;
      Gb := p.Model.Color.G;
      Bb := p.Model.Color.B;
      Rw := Min(Rb * 2 + 64, 255);
      Gw := Min(Gb * 2 + 64, 255);
      Bw := Min(Bb * 2 + 64, 255);
      if (Abs(Rw - Rb) < 32)
      or (Abs(Gw - Gb) < 32)
      or (Abs(Bw - Bb) < 32) then
      begin
        Rb := Max(Rw div 2 - 16, 0);
        Gb := Max(Gw div 2 - 16, 0);
        Bb := Max(Bw div 2 - 16, 0);
      end;
    end;
    4: // custom textured bubble
    begin
      if g_Texture_Get('TEXTURE_PLAYER_TALKBUBBLE', ID) then
        if p.Direction = TDirection.D_RIGHT then
          e_Draw(ID, bubX - 6, bubY - 7, 0, True, False)
        else
          e_Draw(ID, bubX - 6, bubY - 7, 0, True, False, TMirrorType.Horizontal);
      Exit;
    end;
  end;

  // Outer borders
  e_DrawQuad(bubX + 1, bubY    , bubX + 18, bubY + 13, Rb, Gb, Bb);
  e_DrawQuad(bubX    , bubY + 1, bubX + 19, bubY + 12, Rb, Gb, Bb);
  // Inner box
  e_DrawFillQuad(bubX + 1, bubY + 1, bubX + 18, bubY + 12, Rw, Gw, Bw, 0);

  // Tail
  Dot := IfThen(p.Direction = TDirection.D_LEFT, 14, 5);
  e_DrawLine(1, bubX + Dot, bubY + 14, bubX + Dot, bubY + 16, Rb, Gb, Bb);
  e_DrawLine(1, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 1, Dot + 1), bubY + 13, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 1, Dot + 1), bubY + 15, Rw, Gw, Bw);
  e_DrawLine(1, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 2, Dot + 2), bubY + 13, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 2, Dot + 2), bubY + 14, Rw, Gw, Bw);
  e_DrawLine(1, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 3, Dot + 3), bubY + 13, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 3, Dot + 3), bubY + 13, Rw, Gw, Bw);
  e_DrawLine(1, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 3, Dot + 3), bubY + 14, bubX + IfThen(p.Direction = TDirection.D_LEFT, Dot - 1, Dot + 1), bubY + 16, Rb, Gb, Bb);

  // Dots
  Dot := 6;
  e_DrawFillQuad(bubX + Dot,     bubY + 8, bubX + Dot + 1, bubY + 9, Rb, Gb, Bb, 0);
  e_DrawFillQuad(bubX + Dot + 3, bubY + 8, bubX + Dot + 4, bubY + 9, Rb, Gb, Bb, 0);
  e_DrawFillQuad(bubX + Dot + 6, bubY + 8, bubX + Dot + 7, bubY + 9, Rb, Gb, Bb, 0);
end;

procedure r_Player_Draw (p: TPlayer);
var
  ID: DWORD;
  w, h: Word;
  dr: Boolean;
  Mirror: TMirrorType;
  fX, fY, fSlope: Integer;
begin
  p.Obj.lerp(gLerpFactor, fX, fY);
  fSlope := nlerp(p.SlopeOld, p.Obj.slopeUpLeft, gLerpFactor);

  if p.Alive then
  begin
    if p.Direction = TDirection.D_RIGHT then
      Mirror := TMirrorType.None
    else
      Mirror := TMirrorType.Horizontal;

    if p.PunchAnim.enabled then
    begin
      if p.FKeys[KEY_DOWN].Pressed then ID := PunchFrames[R_BERSERK in p.FRulez, 2]
      else if p.FKeys[KEY_UP].Pressed then ID := PunchFrames[R_BERSERK in p.FRulez, 1]
      else ID := PunchFrames[R_BERSERK in p.FRulez, 0];
      r_AnimationState_Draw(ID, p.PunchAnim, fX + IfThen(p.Direction = TDirection.D_LEFT, 15 - p.Obj.Rect.X, p.Obj.Rect.X - 15), fY + fSlope + p.Obj.Rect.Y - 11, Mirror);
    end;

    if (p.FMegaRulez[MR_INVUL] > gTime) and ((gPlayerDrawn <> p) or (p.SpawnInvul >= gTime)) then
      if g_Texture_Get('TEXTURE_PLAYER_INVULPENTA', ID) then
      begin
        e_GetTextureSize(ID, @w, @h);
        if p.Direction = TDirection.D_LEFT then
          e_Draw(ID, fX + p.Obj.Rect.X + (p.Obj.Rect.Width div 2) - (w div 2) + 4,
                     fY + p.Obj.Rect.Y + (p.Obj.Rect.Height div 2) - (h div 2) - 7 + fSlope, 0, True, False)
        else
          e_Draw(ID, fX + p.Obj.Rect.X + (p.Obj.Rect.Width div 2) - (w div 2) - 2,
                     fY + p.Obj.Rect.Y + (p.Obj.Rect.Height div 2) - (h div 2) - 7 + fSlope, 0, True, False)
      end;

    if p.FMegaRulez[MR_INVIS] > gTime then
    begin
      if (gPlayerDrawn <> nil) and ((p = gPlayerDrawn) or
         ((p.Team = gPlayerDrawn.Team) and (gGameSettings.GameMode <> GM_DM))) then
      begin
        if (p.FMegaRulez[MR_INVIS] - gTime) <= 2100 then
          dr := not Odd((p.FMegaRulez[MR_INVIS] - gTime) div 300)
        else
          dr := True;
        if dr then
          r_PlayerModel_Draw(p.Model, fX, fY + fSlope, 200)
        else
          r_PlayerModel_Draw(p.Model, fX, fY + fSlope);
      end
      else
        r_PlayerModel_Draw(p.Model, fX, fY + fSlope, 254);
    end
    else
      r_PlayerModel_Draw(p.Model, fX, fY + fSlope);
  end;

  if g_debug_Frames then
  begin
    e_DrawQuad(p.Obj.X + p.Obj.Rect.X,
               p.Obj.Y + p.Obj.Rect.Y,
               p.Obj.X + p.Obj.Rect.X + p.Obj.Rect.Width - 1,
               p.Obj.Y + p.Obj.Rect.Y + p.Obj.Rect.Height - 1,
               0, 255, 0);
  end;

  if (gChatBubble > 0) and (p.FKeys[KEY_CHAT].Pressed) and not p.Ghost then
    if (p.FMegaRulez[MR_INVIS] <= gTime) or ((gPlayerDrawn <> nil) and ((p = gPlayerDrawn) or
       ((p.Team = gPlayerDrawn.Team) and (gGameSettings.GameMode <> GM_DM)))) then
      r_Player_DrawBubble(p);

 // e_DrawPoint(5, 335, 288, 255, 0, 0); // DL, UR, DL, UR
  if gAimLine and p.Alive and ((p = gPlayer1) or (p = gPlayer2)) then
    r_Player_DrawAim(p);
end;

procedure r_Player_DrawAim (p: TPlayer);

  procedure drawCast (sz: Integer; ax0, ay0, ax1, ay1: Integer);
  var
    ex, ey: Integer;
  begin

{$IFDEF ENABLE_HOLMES}
    if p.isValidViewPort() and (p = gPlayer1) then
    begin
      g_Holmes_plrLaser(ax0, ay0, ax1, ay1);
    end;
{$ENDIF}

    e_DrawLine(sz, ax0, ay0, ax1, ay1, 255, 0, 0, 96);
    if (g_Map_traceToNearestWall(ax0, ay0, ax1, ay1, @ex, @ey) <> nil) then
    begin
      e_DrawLine(sz, ax0, ay0, ex, ey, 0, 255, 0, 96);
    end
    else
    begin
      e_DrawLine(sz, ax0, ay0, ex, ey, 0, 0, 255, 96);
    end;
  end;

var
  wx, wy, xx, yy: Integer;
  angle: SmallInt;
  sz, len: Word;
begin
  wx := p.Obj.X + WEAPONPOINT[p.Direction].X + IfThen(p.Direction = TDirection.D_LEFT, 7, -7);
  wy := p.Obj.Y + WEAPONPOINT[p.Direction].Y;
  angle := p.Angle_;
  len := 1024;
  sz := 2;
  case p.CurrWeap of
    0: begin // Punch
      len := 12;
      sz := 4;
    end;
    1: begin // Chainsaw
      len := 24;
      sz := 6;
    end;
    2: begin // Pistol
      len := 1024;
      sz := 2;
      if angle = ANGLE_RIGHTUP then Dec(angle, 2);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 4);
      if angle = ANGLE_LEFTUP then Inc(angle, 2);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 4);
    end;
    3: begin // Shotgun
      len := 1024;
      sz := 3;
      if angle = ANGLE_RIGHTUP then Dec(angle, 2);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 4);
      if angle = ANGLE_LEFTUP then Inc(angle, 2);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 4);
    end;
    4: begin // Double Shotgun
      len := 1024;
      sz := 4;
      if angle = ANGLE_RIGHTUP then Dec(angle, 2);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 4);
      if angle = ANGLE_LEFTUP then Inc(angle, 2);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 4);
    end;
    5: begin // Chaingun
      len := 1024;
      sz := 3;
      if angle = ANGLE_RIGHTUP then Dec(angle, 2);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 4);
      if angle = ANGLE_LEFTUP then Inc(angle, 2);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 4);
    end;
    6: begin // Rocket Launcher
      len := 1024;
      sz := 7;
      if angle = ANGLE_RIGHTUP then Inc(angle, 2);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 4);
      if angle = ANGLE_LEFTUP then Dec(angle, 2);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 4);
    end;
    7: begin // Plasmagun
      len := 1024;
      sz := 5;
      if angle = ANGLE_RIGHTUP then Inc(angle);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 3);
      if angle = ANGLE_LEFTUP then Dec(angle);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 3);
    end;
    8: begin // BFG
      len := 1024;
      sz := 12;
      if angle = ANGLE_RIGHTUP then Inc(angle, 1);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 2);
      if angle = ANGLE_LEFTUP then Dec(angle, 1);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 2);
    end;
    9: begin // Super Chaingun
      len := 1024;
      sz := 4;
      if angle = ANGLE_RIGHTUP then Dec(angle, 2);
      if angle = ANGLE_RIGHTDOWN then Inc(angle, 4);
      if angle = ANGLE_LEFTUP then Inc(angle, 2);
      if angle = ANGLE_LEFTDOWN then Dec(angle, 4);
    end;
  end;
  xx := Trunc(Cos(-DegToRad(angle)) * len) + wx;
  yy := Trunc(Sin(-DegToRad(angle)) * len) + wy;
  {$IF DEFINED(D2F_DEBUG)}
  drawCast(sz, wx, wy, xx, yy);
  {$ELSE}
  e_DrawLine(sz, wx, wy, xx, yy, 255, 0, 0, 96);
  {$ENDIF}
end;

procedure r_Player_DrawGUI (pl: TPlayer);
var
  ID: DWORD;
  X, Y, SY, a, p, m: Integer;
  tw, th: Word;
  cw, ch: Byte;
  s: string;
  stat: TPlayerStatArray;
begin
  X := gPlayerScreenSize.X;
  SY := gPlayerScreenSize.Y;
  Y := 0;

  if gShowScore and (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
  begin
    if gGameSettings.GameMode = GM_CTF then
      a := 32 + 8
    else
      a := 0;
    if gGameSettings.GameMode = GM_CTF then
    begin
      s := 'TEXTURE_PLAYER_REDFLAG';
      if gFlags[FLAG_RED].State = FLAG_STATE_CAPTURED then
        s := 'TEXTURE_PLAYER_REDFLAG_S';
      if gFlags[FLAG_RED].State = FLAG_STATE_DROPPED then
        s := 'TEXTURE_PLAYER_REDFLAG_D';
      if g_Texture_Get(s, ID) then
        e_Draw(ID, X-16-32, 240-72-4, 0, True, False);
    end;

    s := IntToStr(gTeamStat[TEAM_RED].Score);
    e_CharFont_GetSize(gMenuFont, s, tw, th);
    e_CharFont_PrintEx(gMenuFont, X-16-a-tw, 240-72-4, s, TEAMCOLOR[TEAM_RED]);

    if gGameSettings.GameMode = GM_CTF then
    begin
      s := 'TEXTURE_PLAYER_BLUEFLAG';
      if gFlags[FLAG_BLUE].State = FLAG_STATE_CAPTURED then
        s := 'TEXTURE_PLAYER_BLUEFLAG_S';
      if gFlags[FLAG_BLUE].State = FLAG_STATE_DROPPED then
        s := 'TEXTURE_PLAYER_BLUEFLAG_D';
      if g_Texture_Get(s, ID) then
        e_Draw(ID,  X-16-32, 240-32-4, 0, True, False);
    end;

    s := IntToStr(gTeamStat[TEAM_BLUE].Score);
    e_CharFont_GetSize(gMenuFont, s, tw, th);
    e_CharFont_PrintEx(gMenuFont, X-16-a-tw, 240-32-4, s, TEAMCOLOR[TEAM_BLUE]);
  end;

  if g_Texture_Get('TEXTURE_PLAYER_HUDBG', ID) then
    e_DrawFill(ID, X, 0, 1, (gPlayerScreenSize.Y div 256)+IfThen(gPlayerScreenSize.Y mod 256 > 0, 1, 0),
               0, False, False);

  if g_Texture_Get('TEXTURE_PLAYER_HUD', ID) then
    e_Draw(ID, X+2, Y, 0, True, False);

  if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
  begin
    if gShowStat then
    begin
      s := IntToStr(pl.Frags);
      e_CharFont_GetSize(gMenuFont, s, tw, th);
      e_CharFont_PrintEx(gMenuFont, X-16-tw, Y, s, _RGB(255, 0, 0));

      s := '';
      p := 1;
      m := 0;
      stat := g_Player_GetStats();
      if stat <> nil then
      begin
        p := 1;

        for a := 0 to High(stat) do
          if stat[a].Name <> pl.Name then
          begin
            if stat[a].Frags > m then m := stat[a].Frags;
            if stat[a].Frags > pl.Frags then p := p+1;
          end;
      end;

      s := IntToStr(p)+' / '+IntToStr(Length(stat))+' ';
      if pl.Frags >= m then s := s+'+' else s := s+'-';
      s := s+IntToStr(Abs(pl.Frags-m));

      e_CharFont_GetSize(gMenuSmallFont, s, tw, th);
      e_CharFont_PrintEx(gMenuSmallFont, X-16-tw, Y+32, s, _RGB(255, 0, 0));
    end;

    if gLMSRespawn > LMS_RESPAWN_NONE then
    begin
      s := _lc[I_GAME_WARMUP];
      e_CharFont_GetSize(gMenuFont, s, tw, th);
      s := s + ': ' + IntToStr((gLMSRespawnTime - gTime) div 1000);
      e_CharFont_PrintEx(gMenuFont, X-64-tw, SY-32, s, _RGB(0, 255, 0));
    end
    else if gShowLives and (gGameSettings.MaxLives > 0) then
    begin
      s := IntToStr(pl.Lives);
      e_CharFont_GetSize(gMenuFont, s, tw, th);
      e_CharFont_PrintEx(gMenuFont, X-16-tw, SY-32, s, _RGB(0, 255, 0));
    end;
  end;

  e_CharFont_GetSize(gMenuSmallFont, pl.Name, tw, th);
  e_CharFont_PrintEx(gMenuSmallFont, X+98-(tw div 2), Y+8, pl.Name, _RGB(255, 0, 0));

  if R_BERSERK in pl.FRulez then
    e_Draw(gItemsTexturesID[ITEM_MEDKIT_BLACK], X+37, Y+45, 0, True, False)
  else
    e_Draw(gItemsTexturesID[ITEM_MEDKIT_LARGE], X+37, Y+45, 0, True, False);

  if g_Texture_Get('TEXTURE_PLAYER_ARMORHUD', ID) then
    e_Draw(ID, X+36, Y+77, 0, True, False);

  s := IntToStr(IfThen(pl.Health > 0, pl.Health, 0));
  e_CharFont_GetSize(gMenuFont, s, tw, th);
  e_CharFont_PrintEx(gMenuFont, X+178-tw, Y+40, s, _RGB(255, 0, 0));

  s := IntToStr(pl.Armor);
  e_CharFont_GetSize(gMenuFont, s, tw, th);
  e_CharFont_PrintEx(gMenuFont, X+178-tw, Y+68, s, _RGB(255, 0, 0));

  s := IntToStr(pl.GetAmmoByWeapon(pl.CurrWeap));

  case pl.CurrWeap of
    WEAPON_KASTET:
    begin
      s := '--';
      ID := gItemsTexturesID[ITEM_WEAPON_KASTET];
    end;
    WEAPON_SAW:
    begin
      s := '--';
      ID := gItemsTexturesID[ITEM_WEAPON_SAW];
    end;
    WEAPON_PISTOL: ID := gItemsTexturesID[ITEM_WEAPON_PISTOL];
    WEAPON_CHAINGUN: ID := gItemsTexturesID[ITEM_WEAPON_CHAINGUN];
    WEAPON_SHOTGUN1: ID := gItemsTexturesID[ITEM_WEAPON_SHOTGUN1];
    WEAPON_SHOTGUN2: ID := gItemsTexturesID[ITEM_WEAPON_SHOTGUN2];
    WEAPON_SUPERPULEMET: ID := gItemsTexturesID[ITEM_WEAPON_SUPERPULEMET];
    WEAPON_ROCKETLAUNCHER: ID := gItemsTexturesID[ITEM_WEAPON_ROCKETLAUNCHER];
    WEAPON_PLASMA: ID := gItemsTexturesID[ITEM_WEAPON_PLASMA];
    WEAPON_BFG: ID := gItemsTexturesID[ITEM_WEAPON_BFG];
    WEAPON_FLAMETHROWER: ID := gItemsTexturesID[ITEM_WEAPON_FLAMETHROWER];
  end;

  e_CharFont_GetSize(gMenuFont, s, tw, th);
  e_CharFont_PrintEx(gMenuFont, X+178-tw, Y+158, s, _RGB(255, 0, 0));
  e_Draw(ID, X+20, Y+160, 0, True, False);

  if R_KEY_RED in pl.FRulez then
    e_Draw(gItemsTexturesID[ITEM_KEY_RED], X+78, Y+214, 0, True, False);

  if R_KEY_GREEN in pl.FRulez then
    e_Draw(gItemsTexturesID[ITEM_KEY_GREEN], X+95, Y+214, 0, True, False);

  if R_KEY_BLUE in pl.FRulez then
    e_Draw(gItemsTexturesID[ITEM_KEY_BLUE], X+112, Y+214, 0, True, False);

  if pl.JetFuel > 0 then
  begin
    if g_Texture_Get('TEXTURE_PLAYER_HUDAIR', ID) then
      e_Draw(ID, X+2, Y+116, 0, True, False);
    if g_Texture_Get('TEXTURE_PLAYER_HUDJET', ID) then
      e_Draw(ID, X+2, Y+126, 0, True, False);
    e_DrawLine(4, X+16, Y+122, X+16+Trunc(168*IfThen(pl.Air > 0, pl.Air, 0)/AIR_MAX), Y+122, 0, 0, 196);
    e_DrawLine(4, X+16, Y+132, X+16+Trunc(168*pl.JetFuel/JET_MAX), Y+132, 208, 0, 0);
  end
  else
  begin
    if g_Texture_Get('TEXTURE_PLAYER_HUDAIR', ID) then
      e_Draw(ID, X+2, Y+124, 0, True, False);
    e_DrawLine(4, X+16, Y+130, X+16+Trunc(168*IfThen(pl.Air > 0, pl.Air, 0)/AIR_MAX), Y+130, 0, 0, 196);
  end;

  if gShowPing and g_Game_IsClient then
  begin
    s := _lc[I_GAME_PING_HUD] + IntToStr(NetPeer.lastRoundTripTime) + _lc[I_NET_SLIST_PING_MS];
    e_TextureFontPrint(X + 4, Y + 242, s, gStdFont);
    Y := Y + 16;
  end;

  if pl.Spectator then
  begin
    e_TextureFontPrint(X + 4, Y + 242, _lc[I_PLAYER_SPECT], gStdFont);
    e_TextureFontPrint(X + 4, Y + 258, _lc[I_PLAYER_SPECT2], gStdFont);
    e_TextureFontPrint(X + 4, Y + 274, _lc[I_PLAYER_SPECT1], gStdFont);
    if pl.NoRespawn then
    begin
      e_TextureFontGetSize(gStdFont, cw, ch);
      s := _lc[I_PLAYER_SPECT4];
      e_TextureFontPrintEx(gScreenWidth div 2 - cw*(Length(s) div 2),
                         gScreenHeight-4-ch, s, gStdFont, 255, 255, 255, 1, True);
      e_TextureFontPrint(X + 4, Y + 290, _lc[I_PLAYER_SPECT1S], gStdFont);
    end;

  end;
end;

procedure r_Player_DrawRulez (p: TPlayer);
var
  dr: Boolean;
begin
  // При взятии неуязвимости рисуется инверсионный белый фон
  if (p.FMegaRulez[MR_INVUL] >= gTime) and (p.SpawnInvul < gTime) then
  begin
    if (p.FMegaRulez[MR_INVUL]-gTime) <= 2100 then
      dr := not Odd((p.FMegaRulez[MR_INVUL]-gTime) div 300)
    else
      dr := True;

    if dr then
      e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1,
                     191, 191, 191, 0, TBlending.Invert);
  end;

  // При взятии защитного костюма рисуется зеленоватый фон
  if p.FMegaRulez[MR_SUIT] >= gTime then
  begin
    if (p.FMegaRulez[MR_SUIT]-gTime) <= 2100 then
      dr := not Odd((p.FMegaRulez[MR_SUIT]-gTime) div 300)
    else
      dr := True;

    if dr then
      e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1,
                     0, 96, 0, 200, TBlending.None);
  end;

  // При взятии берсерка рисуется красноватый фон
  if (p.Berserk >= 0) and (LongWord(p.Berserk) >= gTime) and (gFlash = 2) then
  begin
    e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1,
                     255, 0, 0, 200, TBlending.None);
  end;
end;

procedure r_Player_DrawPain (p: TPlayer);
var
  a, h: Integer;
begin
  if p.Pain = 0 then Exit;

  a := p.Pain;

  if a < 15 then h := 0
  else if a < 35 then h := 1
  else if a < 55 then h := 2
  else if a < 75 then h := 3
  else if a < 95 then h := 4
  else h := 5;

  //if a > 255 then a := 255;

  e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1, 255, 0, 0, 255-h*50);
  //e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1, 255-min(128, a), 255-a, 255-a, 0, B_FILTER);
end;

procedure r_Player_DrawPickup (p: TPlayer);
var
  a, h: Integer;
begin
  if p.Pickup = 0 then Exit;

  a := p.Pickup;

  if a < 15 then h := 1
  else if a < 35 then h := 2
  else if a < 55 then h := 3
  else if a < 75 then h := 4
  else h := 5;

  e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1, 150, 200, 150, 255-h*50);
end;

end.
