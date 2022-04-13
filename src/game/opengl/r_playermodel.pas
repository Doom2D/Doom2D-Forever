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

  uses g_playermodel, g_base; // TPlayerModel, TRectWH

  procedure r_PlayerModel_Initialize;
  procedure r_PlayerModel_Finalize;
  procedure r_PlayerModel_Load;
  procedure r_PlayerModel_Free;
  procedure r_PlayerModel_Update;
  procedure r_PlayerModel_Draw (pm: TPlayerModel; X, Y: Integer; Alpha: Byte = 0);

  {$IFDEF ENABLE_GIBS}
    procedure r_PlayerModel_DrawGibs;
    function r_PlayerModel_GetGibRect (m, id: Integer): TRectWH;
  {$ENDIF}

implementation

  uses
    {$IFDEF ENABLE_GIBS}
      g_gibs,
    {$ENDIF}
    SysUtils, Classes, Math,
    MAPDEF, utils, e_log, wadreader,
    ImagingTypes, Imaging, ImagingUtility,
    r_graphics, g_options, r_animations, r_textures,
    g_basic, g_map, g_weapons, g_textures, g_player, g_phys, g_game
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
      {$IFDEF ENABLE_GIBS}
        Gibs: Array of record
          base: DWORD;
          mask: DWORD;
          rect: TRectWH;
        end;
      {$ENDIF}
    end;
    RedFlagFrames: DWORD;
    BlueFlagFrames: DWORD;
    FlagAnimState: TAnimState;

{$IFDEF ENABLE_GIBS}
  function r_PlayerModel_GetGibRect (m, id: Integer): TRectWH;
  begin
    Result := Models[m].Gibs[id].rect
  end;
{$ENDIF}

  procedure r_PlayerModel_Initialize;
  begin
    FlagAnimState := TAnimState.Create(True, 8, 5);
  end;

  procedure r_PlayerModel_Finalize;
  begin
    FlagAnimState.Invalidate;
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

  procedure r_PlayerModel_LoadResource (resource: AnsiString; var pData: Pointer; var len: Integer);
    var WAD: TWADFile;
  begin
    pData := nil;
    len := 0;
    WAD := TWADFile.Create;
    WAD.ReadFile(g_ExtractWadName(resource));
    WAD.GetResource(g_ExtractFilePathName(resource), pData, len);
    WAD.Free;
  end;

  function g_PlayerModel_CalcGibSize (pData: Pointer; dataSize, x, y, w, h: Integer): TRectWH;
    var i, j: Integer; done: Boolean; img: TImageData;

    function IsVoid (i, j: Integer): Boolean;
    begin
      result := Byte((PByte(img.bits) + (y+j)*img.width*4 + (x+i)*4 + 3)^) = 0
    end;

  begin
    InitImage(img);
    assert(LoadImageFromMemory(pData, dataSize, img));

    (* trace x from right to left *)
    done := false; i := 0;
    while not done and (i < w) do
    begin
      j := 0;
      while (j < h) and IsVoid(i, j) do inc(j);
      done := (j < h) and (IsVoid(i, j) = false);
      result.x := i;
      inc(i);
    end;

    (* trace y from up to down *)
    done := false; j := 0;
    while not done and (j < h) do
    begin
      i := 0;
      while (i < w) and IsVoid(i, j) do inc(i);
      done := (i < w) and (IsVoid(i, j) = false);
      result.y := j;
      inc(j);
    end;

    (* trace x from right to left *)
    done := false; i := w - 1;
    while not done and (i >= 0) do
    begin
      j := 0;
      while (j < h) and IsVoid(i, j) do inc(j);
      done := (j < h) and (IsVoid(i, j) = false);
      result.width := i - result.x + 1;
      dec(i);
    end;

    (* trace y from down to up *)
    done := false; j := h - 1;
    while not done and (j >= 0) do
    begin
      i := 0;
      while (i < w) and IsVoid(i, j) do inc(i);
      done := (i < w) and (IsVoid(i, j) = false);
      result.height := j - result.y + 1;
      dec(j);
    end;

    FreeImage(img);
  end;

  procedure r_PlayerModel_Load;
    {$IFDEF ENABLE_GIBS}
      var base, mask: Pointer; baseLen, maskLen: Integer;
    {$ENDIF}
    var ID1, ID2: DWORD; i, a, b: Integer; prefix, aname: String;
  begin
    g_Frames_CreateWAD(@RedFlagFrames, 'FRAMES_FLAG_RED', GameWAD + ':TEXTURES\FLAGRED', 64, 64, 5, False);
    g_Frames_CreateWAD(@BlueFlagFrames, 'FRAMES_FLAG_BLUE', GameWAD + ':TEXTURES\FLAGBLUE', 64, 64, 5, False);
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
          aname := PlayerModelsArray[i].Name + '_RIGHTANIM' + IntToStr(b);
          with PlayerModelsArray[i].Anim[TDirection.D_RIGHT, b] do
          begin
            if not (g_Frames_CreateWAD(@ID1, aname, prefix + Resource, 64, 64, Frames, Back) and
                    g_Frames_CreateWAD(@ID2, aname + '_MASK', prefix + Mask, 64, 64, Frames, Back)) then
            begin
              if b > A_LASTBASE then
              begin
                ExtAnimFromBaseAnim(PlayerModelsArray[i].Name, b);
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
              aname := PlayerModelsArray[i].Name + '_LEFTANIM' + IntToStr(b);
              g_Frames_CreateWAD(@ID1, aname, prefix + Resource, 64, 64, Frames, Back);
              g_Frames_CreateWAD(@ID2, aname + '_MASK', prefix + Mask, 64, 64, Frames, Back);
              Models[i].Frames[TDirection.D_LEFT, b].base := ID1;
              Models[i].Frames[TDirection.D_LEFT, b].mask := ID2;
            end
          end
        end;
        {$IFDEF ENABLE_GIBS}
          SetLength(Models[i].Gibs, PlayerModelsArray[i].GibsCount);
          if PlayerModelsArray[i].GibsCount > 0 then
          begin
            r_PlayerModel_LoadResource(prefix + PlayerModelsArray[i].GibsResource, base, baseLen);
            r_PlayerModel_LoadResource(prefix + PlayerModelsArray[i].GibsMask, mask, maskLen);
            if (base <> nil) and (mask <> nil) then
            begin
              for a := 0 to PlayerModelsArray[i].GibsCount - 1 do
              begin
                if e_CreateTextureMemEx(base, baseLen, Models[i].Gibs[a].base, a * 32, 0, 32, 32) and
                   e_CreateTextureMemEx(mask, maskLen, Models[i].Gibs[a].mask, a * 32, 0, 32, 32) then
                begin
                  Models[i].Gibs[a].rect := g_PlayerModel_CalcGibSize(base, baseLen, a * 32, 0, 32, 32);
                  with Models[i].Gibs[a].Rect do
                    if Height > 3 then
                      Height := Height - 1 - Random(2); // ???
                end
              end
            end;
            FreeMem(mask);
            FreeMem(base);
          end
        {$ENDIF}
      end
    end
  end;

  procedure r_PlayerModel_Free;
    var i, a, b, c: Integer;
  begin
    e_DeleteTexture(RedFlagFrames);
    e_DeleteTexture(BlueFlagFrames);
    if PlayerModelsArray = nil then Exit;
    for i := 0 to High(PlayerModelsArray) do
    begin
      with PlayerModelsArray[i] do
      begin
        for a := A_STAND to A_LAST do
        begin
          g_Frames_DeleteByName(Name + '_LEFTANIM' + IntToStr(a));
          g_Frames_DeleteByName(Name + '_LEFTANIM' + IntToStr(a) + '_MASK');
          g_Frames_DeleteByName(Name + '_RIGHTANIM' + IntToStr(a));
          g_Frames_DeleteByName(Name + '_RIGHTANIM' + IntToStr(a) + '_MASK');
        end;
      end
      // !!! delete gibs textures here
    end;
    for a := WP_FIRST + 1 to WP_LAST do
      for b := W_POS_NORMAL to W_POS_DOWN do
        for c := W_ACT_NORMAL to W_ACT_FIRE do
          e_DeleteTexture(WeaponID[a][b][c])
  end;

  procedure r_PlayerModel_Update;
  begin
    FlagAnimState.Update
  end;

procedure r_PlayerModel_Draw (pm: TPlayerModel; X, Y: Integer; Alpha: Byte = 0);
var
  Mirror: TMirrorType;
  pos, act: Byte;
  fp, p: TDFPoint;
  FramesID: DWORD;
  fa: Integer;
begin
// Флаги:
  if pm.Direction = TDirection.D_LEFT then
    Mirror := TMirrorType.None
  else
    Mirror := TMirrorType.Horizontal;

  FramesID := 0;
  case pm.Flag of
    FLAG_RED: FramesID := RedFlagFrames;
    FLAG_BLUE: FramesID := BlueFlagFrames;
  end;
  if (FramesID <> 0) and (not (pm.CurrentAnimation in [A_DIE1, A_DIE2])) then
  begin
    fp := PlayerModelsArray[pm.id].FlagPoint;
    fa := PlayerModelsArray[pm.id].FlagAngle;
    p.X := IfThen(pm.Direction = TDirection.D_LEFT, FLAG_BASEPOINT.X, 64 - FLAG_BASEPOINT.X);
    p.Y := FLAG_BASEPOINT.Y;
    r_AnimState_DrawEx(
      FramesID,
      FlagAnimState,
      X + IfThen(pm.Direction = TDirection.D_LEFT, fp.X - 1, 2 * FLAG_BASEPOINT.X - fp.X + 1) - FLAG_BASEPOINT.X,
      Y + fp.Y - FLAG_BASEPOINT.Y + 1,
      0,
      Mirror,
      False,
      p,
      IfThen(pm.Direction = TDirection.D_RIGHT, fa, -fa)
    );
  end;

// Оружие:
  if pm.Direction = TDirection.D_RIGHT then
    Mirror := TMirrorType.None
  else
    Mirror := TMirrorType.Horizontal;

  if PlayerModelsArray[pm.id].HaveWeapon and (not (pm.CurrentAnimation in [A_DIE1, A_DIE2, A_PAIN])) and  (pm.CurrentWeapon in [WP_FIRST + 1..WP_LAST]) then
  begin
    if pm.CurrentAnimation in [A_SEEUP, A_ATTACKUP] then
      pos := W_POS_UP
    else
      if pm.CurrentAnimation in [A_SEEDOWN, A_ATTACKDOWN] then
        pos := W_POS_DOWN
      else
        pos := W_POS_NORMAL;

    if (pm.CurrentAnimation in [A_ATTACK, A_ATTACKUP, A_ATTACKDOWN]) or pm.GetFire() then
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
    FramesID := Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].base;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, Alpha, TMirrorType.None, False);
  end
  else
  begin
    FramesID := Models[pm.id].Frames[TDirection.D_RIGHT, pm.CurrentAnimation].base;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, Alpha, Mirror, False);
  end;

// Маска модели:
  e_Colors := pm.Color;

  if (pm.Direction = TDirection.D_LEFT) and (Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].mask <> 0) then
  begin
    FramesID := Models[pm.id].Frames[TDirection.D_LEFT, pm.CurrentAnimation].mask;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, Alpha, TMirrorType.None, False);
  end
  else
  begin
    FramesID := Models[pm.id].Frames[TDirection.D_RIGHT, pm.CurrentAnimation].mask;
    r_AnimationState_Draw(FramesID, pm.AnimState, X, Y, Alpha, Mirror, False);
  end;

  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
end;

{$IFDEF ENABLE_GIBS}
  procedure r_PlayerModel_DrawGibs;
    var i, fX, fY, m, id: Integer; a: TDFPoint; pobj: ^TObj;
  begin
    if gGibs <> nil then
    begin
      for i := 0 to High(gGibs) do
      begin
        if gGibs[i].alive then
        begin
          pobj := @gGibs[i].Obj;
          if not g_Obj_Collide(sX, sY, sWidth, sHeight, pobj) then
            Continue;
          pobj.lerp(gLerpFactor, fX, fY);
          a.X := pobj.Rect.X + (pobj.Rect.Width div 2);
          a.y := pobj.Rect.Y + (pobj.Rect.Height div 2);
          m := gGibs[i].ModelID;
          id := gGibs[i].GibID;
          e_DrawAdv(Models[m].Gibs[id].base, fX, fY, 0, True, False, gGibs[i].RAngle, @a, TMirrorType.None);
          e_Colors := gGibs[i].Color;
          e_DrawAdv(Models[m].Gibs[id].mask, fX, fY, 0, True, False, gGibs[i].RAngle, @a, TMirrorType.None);
          e_Colors.R := 255;
          e_Colors.G := 255;
          e_Colors.B := 255;
        end
      end
    end
  end;
{$ENDIF}

end.
