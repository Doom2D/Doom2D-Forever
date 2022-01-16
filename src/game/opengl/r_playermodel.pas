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
unit r_playermodel;

interface

  uses g_playermodel; // TPlayerModel

  procedure r_PlayerModel_Load;
  procedure r_PlayerModel_Free;
  procedure r_PlayerModel_Draw (pm: TPlayerModel; X, Y: Integer; Alpha: Byte = 0);

implementation

  uses
    SysUtils, Classes, Math,
    MAPDEF, utils,
    ImagingTypes, Imaging, ImagingUtility,
    r_graphics, g_options, r_animations, r_textures,
    g_base, g_basic, g_map, g_weapons
  ;

  const
    WeapNames: Array [WP_FIRST + 1..WP_LAST] of String = ('csaw', 'hgun', 'sg', 'ssg', 'mgun', 'rkt', 'plz', 'bfg', 'spl', 'flm');

  type
    TDirIdx = TDirection.D_LEFT..TDirection.D_RIGHT;
    TAnimIdx = A_STAND..A_LAST;

  var
    WeaponID: Array [WP_FIRST + 1..WP_LAST, W_POS_NORMAL..W_POS_DOWN, W_ACT_NORMAL..W_ACT_FIRE] of DWORD;
    Models: Array of record
      Frames: Array [TDirIdx, TAnimIdx] of record
        base: DWORD;
        mask: DWORD;
      end;
    end;

  procedure ExtAnimFromBaseAnim(MName: String; AIdx: Integer);
    const
      CopyAnim: array [A_LASTBASE+1..A_LASTEXT] of Integer = (
        A_WALK, A_WALK, A_WALK, A_WALK, A_WALK,
        A_STAND, A_WALK, A_ATTACK, A_WALK, A_SEEUP, A_SEEDOWN,
        A_ATTACKUP, A_ATTACKDOWN
      );
    var
      OIdx: Integer;
      AName, OName: String;
  begin
    // HACK: shitty workaround to duplicate base animations
    //       in place of extended, replace with something better later

    Assert((AIdx > A_LASTBASE) and (AIdx <= A_LASTEXT));
    OIdx := CopyAnim[AIdx];

    AName := MName + '_RIGHTANIM' + IntToStr(AIdx);
    OName := MName + '_RIGHTANIM' + IntToStr(OIdx);
    Assert(g_Frames_Dup(AName, OName));
    Assert(g_Frames_Dup(AName + '_MASK', OName + '_MASK'));
    AName := MName + '_LEFTANIM' + IntToStr(AIdx);
    OName := MName + '_LEFTANIM' + IntToStr(OIdx);
    if g_Frames_Exists(AName) then
    begin
      g_Frames_Dup(AName, OName);
      g_Frames_Dup(AName + '_MASK', OName + '_MASK');
    end;
  end;

  procedure r_PlayerModel_Load;
    var ID1, ID2: DWORD; i, a, b: Integer; prefix, aname: String;
  begin
    for a := WP_FIRST + 1 to WP_LAST do
    begin
      g_Texture_CreateWAD(WeaponID[a][W_POS_NORMAL][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a]));
      g_Texture_CreateWAD(WeaponID[a][W_POS_NORMAL][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_FIRE');
      g_Texture_CreateWAD(WeaponID[a][W_POS_UP][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_UP');
      g_Texture_CreateWAD(WeaponID[a][W_POS_UP][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_UP_FIRE');
      g_Texture_CreateWAD(WeaponID[a][W_POS_DOWN][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_DN');
      g_Texture_CreateWAD(WeaponID[a][W_POS_DOWN][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_DN_FIRE');
    end;
    Models := nil;
    if PlayerModelsArray <> nil then
    begin
      SetLength(Models, Length(PlayerModelsArray));
      for i := 0 to High(PlayerModelsArray) do
      begin
        prefix := PlayerModelsArray[i].FileName + ':TEXTURES\';
        for b := A_STAND to A_LAST do
        begin
          aname := PlayerModelsArray[i].Info.Name + '_RIGHTANIM' + IntToStr(b);
          with PlayerModelsArray[i].Anim[TDirection.D_RIGHT, b] do
          begin
            if not (g_Frames_CreateWAD(@ID1, aname, prefix + Resource, 64, 64, Frames, Back) and
                    g_Frames_CreateWAD(@ID2, aname + '_MASK', prefix + Mask, 64, 64, Frames, Back)) then
            begin
              if b > A_LASTBASE then
              begin
                ExtAnimFromBaseAnim(PlayerModelsArray[i].Info.Name, b);
                continue
              end
            end;
            Models[i].Frames[TDirection.D_RIGHT, b].base := ID1;
            Models[i].Frames[TDirection.D_RIGHT, b].mask := ID2;
          end;
          with PlayerModelsArray[i].Anim[TDirection.D_LEFT, b] do
          begin
            if (Resource <> '') and (Mask <> '') then
            begin
              aname := PlayerModelsArray[i].Info.Name + '_LEFTANIM' + IntToStr(b);
              g_Frames_CreateWAD(@ID1, aname, prefix + Resource, 64, 64, Frames, Back);
              g_Frames_CreateWAD(@ID2, aname + '_MASK', prefix + Mask, 64, 64, Frames, Back);
              Models[i].Frames[TDirection.D_LEFT, b].base := ID1;
              Models[i].Frames[TDirection.D_LEFT, b].mask := ID2;
            end
          end
        end
      end
    end
  end;

  procedure r_PlayerModel_Free;
    var i, a, b, c: Integer;
  begin
    if PlayerModelsArray = nil then Exit;
    for i := 0 to High(PlayerModelsArray) do
    begin
      with PlayerModelsArray[i] do
      begin
        for a := A_STAND to A_LAST do
        begin
          g_Frames_DeleteByName(Info.Name+'_LEFTANIM'+IntToStr(a));
          g_Frames_DeleteByName(Info.Name+'_LEFTANIM'+IntToStr(a)+'_MASK');
          g_Frames_DeleteByName(Info.Name+'_RIGHTANIM'+IntToStr(a));
          g_Frames_DeleteByName(Info.Name+'_RIGHTANIM'+IntToStr(a)+'_MASK');
        end;
        if Gibs <> nil then
        begin
          for a := 0 to High(Gibs) do
          begin
            e_DeleteTexture(Gibs[a].ID);
            e_DeleteTexture(Gibs[a].MaskID);
            Gibs[a].ID := DWORD(-1);
            Gibs[a].MaskID := DWORD(-1);
          end
        end
      end
    end;
    for a := WP_FIRST + 1 to WP_LAST do
      for b := W_POS_NORMAL to W_POS_DOWN do
        for c := W_ACT_NORMAL to W_ACT_FIRE do
          e_DeleteTexture(WeaponID[a][b][c])
  end;

procedure r_PlayerModel_Draw (pm: TPlayerModel; X, Y: Integer; Alpha: Byte = 0);
var
  Mirror: TMirrorType;
  pos, act: Byte;
  p: TDFPoint;
  FramesID: DWORD;
begin
// Флаги:
  if pm.Direction = TDirection.D_LEFT then
    Mirror := TMirrorType.None
  else
    Mirror := TMirrorType.Horizontal;

  if (pm.Flag <> FLAG_NONE) and (pm.FlagAnim <> nil) and (not (pm.CurrentAnimation in [A_DIE1, A_DIE2])) then
  begin
    p.X := IfThen(pm.Direction = TDirection.D_LEFT, FLAG_BASEPOINT.X, 64 - FLAG_BASEPOINT.X);
    p.Y := FLAG_BASEPOINT.Y;

    r_Animation_DrawEx(pm.FlagAnim, X+IfThen(pm.Direction = TDirection.D_LEFT, pm.FlagPoint.X-1, 2*FLAG_BASEPOINT.X-pm.FlagPoint.X+1)-FLAG_BASEPOINT.X,
                     Y+pm.FlagPoint.Y-FLAG_BASEPOINT.Y+1, Mirror, p,
                     IfThen(pm.Direction = TDirection.D_RIGHT, pm.FlagAngle, -pm.FlagAngle));
  end;

// Оружие:
  if pm.Direction = TDirection.D_RIGHT then
    Mirror := TMirrorType.None
  else
    Mirror := TMirrorType.Horizontal;

  if PlayerModelsArray[pm.id].Info.HaveWeapon and (not (pm.CurrentAnimation in [A_DIE1, A_DIE2, A_PAIN])) and  (pm.CurrentWeapon in [WP_FIRST + 1..WP_LAST]) then
  begin
    if pm.CurrentAnimation in [A_SEEUP, A_ATTACKUP] then
      pos := W_POS_UP
    else
      if pm.CurrentAnimation in [A_SEEDOWN, A_ATTACKDOWN] then
        pos := W_POS_DOWN
      else
        pos := W_POS_NORMAL;

    if (pm.CurrentAnimation in [A_ATTACK, A_ATTACKUP, A_ATTACKDOWN]) or pm.Fire then
      act := W_ACT_FIRE
    else
      act := W_ACT_NORMAL;

    if Alpha < 201 then
      e_Draw(
        WeaponID[pm.CurrentWeapon][pos][act],
        X + PlayerModelsArray[pm.id].WeaponPoints[pm.CurrentWeapon, pm.CurrentAnimation, pm.Direction, pm.AnimState.CurrentFrame].X,
        Y + PlayerModelsArray[pm.id].WeaponPoints[pm.CurrentWeapon, pm.CurrentAnimation, pm.Direction, pm.AnimState.CurrentFrame].Y,
        0,
        True,
        False,
        Mirror
      );
  end;

// Модель:
  if (pm.Direction = TDirection.D_LEFT) and (Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].base <> 0) then
  begin
    pm.AnimState.Alpha := Alpha; // !!!
    FramesID := Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].base;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, TMirrorType.None);
  end
  else
  begin
    pm.AnimState.Alpha := Alpha; // !!!
    FramesID := Models[pm.id].Frames[TDirection.D_RIGHT, pm.CurrentAnimation].base;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, Mirror);
  end;

// Маска модели:
  e_Colors := pm.Color;

  if (pm.Direction = TDirection.D_LEFT) and (Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].mask <> 0) then
  begin
    pm.AnimState.Alpha := Alpha; // !!!
    FramesID := Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].mask;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, TMirrorType.None);
  end
  else
  begin
    pm.AnimState.Alpha := Alpha; // !!!
    FramesID := Models[pm.id].Frames[TDirection.D_RIGHT, pm.CurrentAnimation].mask;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, Mirror);
  end;

  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
end;

end.
