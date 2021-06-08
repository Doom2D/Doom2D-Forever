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
{$M+}
unit g_playermodel;

interface

uses
  MAPDEF, g_textures, g_basic, g_weapons, e_graphics, utils, g_gfx,
  ImagingTypes, Imaging, ImagingUtility;

const
  A_STAND      = 0;
  A_WALK       = 1;
  A_DIE1       = 2;
  A_DIE2       = 3;
  A_ATTACK     = 4;
  A_SEEUP      = 5;
  A_SEEDOWN    = 6;
  A_ATTACKUP   = 7;
  A_ATTACKDOWN = 8;
  A_PAIN       = 9;
  // EXTENDED
  A_WALKATTACK     = 10;
  A_WALKSEEUP      = 11;
  A_WALKSEEDOWN    = 12;
  A_WALKATTACKUP   = 13;
  A_WALKATTACKDOWN = 14;
  A_FISTSTAND      = 15;
  A_FISTWALK       = 16;
  A_FISTATTACK     = 17;
  A_FISTWALKATTACK = 18;
  A_FISTSEEUP      = 19;
  A_FISTSEEDOWN    = 20;
  A_FISTATTACKUP   = 21;
  A_FISTATTACKDOWN = 22;

  A_LASTBASE = A_PAIN;
  A_LASTEXT  = A_FISTATTACKDOWN;
  A_LAST     = A_LASTEXT;

  MODELSOUND_PAIN = 0;
  MODELSOUND_DIE  = 1;

  W_POS_NORMAL = 0;
  W_POS_UP     = 1;
  W_POS_DOWN   = 2;

  W_ACT_NORMAL = 0;
  W_ACT_FIRE   = 1;

  FLAG_BASEPOINT: TDFPoint = (X:16; Y:43);

type
  TModelInfo = record
    Name:        String;
    Author:      String;
    Description: String;
    HaveWeapon:  Boolean;
  end;

  TModelBlood = record
    R, G, B, Kind: Byte;
  end;

  TModelSound = record
    ID:    DWORD;
    Level: Byte;
  end;

  TGibSprite = record
    ID: DWORD;
    MaskID: DWORD;
    Rect: TRectWH;
    OnlyOne: Boolean;
  end;

  TModelSoundArray = Array of TModelSound;
  TGibsArray = Array of TGibSprite;
  TWeaponPoints = Array [WP_FIRST + 1..WP_LAST] of
                  Array [A_STAND..A_LAST] of
                  Array [TDirection.D_LEFT..TDirection.D_RIGHT] of Array of TDFPoint;
  TModelMatrix = Array [TDirection.D_LEFT..TDirection.D_RIGHT] of Array [A_STAND..A_LAST] of TAnimation;

  TPlayerModel = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    FName:             String;
    FDirection:        TDirection;
    FColor:            TRGB;
    FBlood:            TModelBlood;
    FCurrentAnimation: Byte;
    FAnim:             TModelMatrix;
    FMaskAnim:         TModelMatrix;
    FWeaponPoints:     TWeaponPoints;
    FPainSounds:       TModelSoundArray;
    FDieSounds:        TModelSoundArray;
    FSlopSound:        Byte;
    FCurrentWeapon:    Byte;
    FDrawWeapon:       Boolean;
    FFlag:             Byte;
    FFlagPoint:        TDFPoint;
    FFlagAngle:        SmallInt;
    FFlagAnim:         TAnimation;
    FFire:             Boolean;
    FFireCounter:      Byte;

  public
    destructor  Destroy(); override;
    procedure   ChangeAnimation(Animation: Byte; Force: Boolean = False);
    function    GetCurrentAnimation: TAnimation;
    function    GetCurrentAnimationMask: TAnimation;
    procedure   SetColor(Red, Green, Blue: Byte);
    procedure   SetWeapon(Weapon: Byte);
    procedure   SetFlag(Flag: Byte);
    procedure   SetFire(Fire: Boolean);
    function    PlaySound(SoundType, Level: Byte; X, Y: Integer): Boolean;
    procedure   Update();

  published
    property    Fire: Boolean read FFire;
    property    Direction: TDirection read FDirection write FDirection;
    property    Animation: Byte read FCurrentAnimation;
    property    Weapon: Byte read FCurrentWeapon;
    property    Name: String read FName;

  public
    property    Color: TRGB read FColor write FColor;
    property    Blood: TModelBlood read FBlood;

    property    Anim: TModelMatrix read FAnim;
    property    MaskAnim: TModelMatrix read FMaskAnim;
    property    CurrentAnimation: Byte read FCurrentAnimation;

    property    CurrentWeapon: Byte read FCurrentWeapon;
    property    DrawWeapon: Boolean read FDrawWeapon;
    property    WeaponPoints: TWeaponPoints read FWeaponPoints;

    property    Flag: Byte read FFlag;
    property    FlagAnim: TAnimation read FFlagAnim;
    property    FlagAngle: SmallInt read FFlagAngle;
    property    FlagPoint: TDFPoint read FFlagPoint;
  end;

procedure g_PlayerModel_FreeData();
function  g_PlayerModel_Load(FileName: String): Boolean;
function  g_PlayerModel_GetNames(): SSArray;
function  g_PlayerModel_GetInfo(ModelName: String): TModelInfo;
function  g_PlayerModel_GetBlood(ModelName: String): TModelBlood;
function  g_PlayerModel_Get(ModelName: String): TPlayerModel;
function  g_PlayerModel_GetAnim(ModelName: String; Anim: Byte; var _Anim, _Mask: TAnimation): Boolean;
function  g_PlayerModel_GetGibs(ModelName: String; var Gibs: TGibsArray): Boolean;


implementation

uses
  g_main, g_sound, g_console, SysUtils, g_player, CONFIG,
  e_sound, g_options, g_map, Math, e_log, wadreader;

type
  TPlayerModelInfo = record
    Info:         TModelInfo;
    ModelSpeed:   Array [A_STAND..A_PAIN] of Byte;
    FlagPoint:    TDFPoint;
    FlagAngle:    SmallInt;
    WeaponPoints: TWeaponPoints;
    Gibs:         TGibsArray;
    PainSounds:   TModelSoundArray;
    DieSounds:    TModelSoundArray;
    SlopSound:    Byte;
    Blood:        TModelBlood;
  end;

const
  FLAG_DEFPOINT:  TDFPoint = (X:32; Y:16);
  FLAG_DEFANGLE = -20;
  WEAPONBASE: Array [WP_FIRST + 1..WP_LAST] of TDFPoint =
              ((X:8; Y:4), (X:8; Y:8), (X:16; Y:16), (X:16; Y:24),
               (X:16; Y:16), (X:24; Y:24), (X:16; Y:16), (X:24; Y:24),
               (X:16; Y:16), (X:8; Y:8));

  AnimNames: Array [A_STAND..A_LASTEXT] of String =
             ('StandAnim','WalkAnim','Die1Anim','Die2Anim','AttackAnim',
              'SeeUpAnim','SeeDownAnim','AttackUpAnim','AttackDownAnim','PainAnim',
              // EXTENDED
              'WalkAttackAnim', 'WalkSeeUpAnim', 'WalkSeeDownAnim',
              'WalkAttackUpAnim', 'WalkAttackDownAnim', 'FistStandAnim', 'FistWalkAnim',
              'FistAttackAnim', 'FistWalkAttackAnim', 'FistSeeUpAnim', 'FistSeeDownAnim',
              'FistAttackUpAnim', 'FistAttackDownAnim');
  WeapNames: Array [WP_FIRST + 1..WP_LAST] of String =
             ('csaw', 'hgun', 'sg', 'ssg', 'mgun', 'rkt', 'plz', 'bfg', 'spl', 'flm');

var
  PlayerModelsArray: Array of TPlayerModelInfo;

function GetPoint(var str: String; var point: TDFPoint): Boolean;
var
  a, x, y: Integer;
  s: String;
begin
  Result := False;
  x := 0;
  y := 0;

  str := Trim(str);
  if Length(str) < 3 then
    Exit;

  for a := 1 to Length(str) do
    if (str[a] = ',') or (a = Length(str)) then
    begin
      s := Copy(str, 1, a);
      if s[Length(s)] = ',' then
        SetLength(s, Length(s)-1);
      Delete(str, 1, a);

      if (Sscanf(s, '%d:%d', [@x, @y]) < 2) or
         (x < -64) or (x > 128) or
         (y < -64) or (y > 128) then
        Exit;

      point.X := x;
      point.Y := y;

      Break;
    end;

  Result := True;
end;

function GetWeapPoints(str: String; weapon: Byte; anim: Byte; dir: TDirection;
                       frames: Word; backanim: Boolean; var wpoints: TWeaponPoints): Boolean;
var
  a, b, h: Integer;
begin
  Result := False;

  if frames = 0 then
    Exit;

  backanim := backanim and (frames > 2);

  for a := 1 to frames do
  begin
    if not GetPoint(str, wpoints[weapon, anim, dir, a-1]) then
      Exit;

    with wpoints[weapon, anim, dir, a-1] do
    begin
      X := X - WEAPONBASE[weapon].X;
      Y := Y - WEAPONBASE[weapon].Y;
      if dir = TDirection.D_LEFT then
        X := -X;
    end;
  end;

  h := High(wpoints[weapon, anim, dir]);
  if backanim then
    for b := h downto frames do
      wpoints[weapon, anim, dir, b] := wpoints[weapon, anim, dir, h-b+1];

  Result := True;
end;

procedure ExtAnimFromBaseAnim(MName: String; AIdx: Integer);
const
  CopyAnim: array [A_LASTBASE+1..A_LASTEXT] of Integer = (
    A_WALK, A_WALK, A_WALK, A_WALK, A_WALK,
    A_STAND, A_WALK, A_ATTACK, A_WALK, A_SEEUP, A_SEEDOWN,
    A_ATTACKUP, A_ATTACKDOWN
  );
var
  OIdx, W, I: Integer;
  D: TDirection;
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

  with PlayerModelsArray[High(PlayerModelsArray)] do
  begin
    for W := WP_FIRST + 1 to WP_LAST do
    begin
      for D := TDirection.D_LEFT to TDirection.D_RIGHT do
      begin
        SetLength(WeaponPoints[W, AIdx, D], Length(WeaponPoints[W, OIdx, D]));
        for I := 0 to High(WeaponPoints[W, AIdx, D]) do
          WeaponPoints[W, AIdx, D, I] := WeaponPoints[W, OIdx, D, I]
      end;
    end;
  end;
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

function g_PlayerModel_Load(FileName: string): Boolean;
var
  ID: DWORD;
  a, b, len, lenpd, lenpd2, aa, bb, f: Integer;
  cc: TDirection;
  config: TConfig;
  pData, pData2: Pointer;
  WAD: TWADFile;
  s, aname: string;
  prefix: string;
  ok, chk: Boolean;
begin
  e_WriteLog(Format('Loading player model "%s"...', [FileName]), TMsgType.Notify);

  Result := False;

  WAD := TWADFile.Create;
  WAD.ReadFile(FileName);

  if {WAD.GetLastError <> DFWAD_NOERROR} not WAD.isOpen then
  begin
    WAD.Free();
    Exit;
  end;

  if not WAD.GetResource('TEXT/MODEL', pData, len) then
  begin
    WAD.Free();
    Exit;
  end;

  config := TConfig.CreateMem(pData, len);
  FreeMem(pData);

  s := config.ReadStr('Model', 'name', '');
  if s = '' then
  begin
    config.Free();
    WAD.Free();
    Exit;
  end;

  SetLength(PlayerModelsArray, Length(PlayerModelsArray)+1);
  ID := High(PlayerModelsArray);

  prefix := FileName+':TEXTURES\';

  with PlayerModelsArray[ID].Info do
  begin
    Name := s;
    Author := config.ReadStr('Model', 'author', '');
    Description := config.ReadStr('Model', 'description', '');
  end;

  with PlayerModelsArray[ID] do
  begin
    Blood.R := MAX(0, MIN(255, config.ReadInt('Blood', 'R', 150)));
    Blood.G := MAX(0, MIN(255, config.ReadInt('Blood', 'G', 0)));
    Blood.B := MAX(0, MIN(255, config.ReadInt('Blood', 'B', 0)));
    case config.ReadStr('Blood', 'Kind', 'NORMAL') of
      'NORMAL': Blood.Kind := BLOOD_NORMAL;
      'SPARKS': Blood.Kind := BLOOD_CSPARKS;
      'COMBINE': Blood.Kind := BLOOD_COMBINE;
    else
      Blood.Kind := BLOOD_NORMAL
    end
  end;

  for b := A_STAND to A_LAST do
  begin
    aname := s+'_RIGHTANIM'+IntToStr(b);
    //e_LogWritefln('### MODEL FILE: [%s]', [prefix+config.ReadStr(AnimNames[b], 'resource', '')]);
    if not (g_Frames_CreateWAD(nil, aname,
                               prefix+config.ReadStr(AnimNames[b], 'resource', ''),
                               64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                               config.ReadBool(AnimNames[b], 'backanim', False)) and
            g_Frames_CreateWAD(nil, aname+'_MASK',
                               prefix+config.ReadStr(AnimNames[b], 'mask', ''),
                               64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                               config.ReadBool(AnimNames[b], 'backanim', False))) then
    begin
      if b <= A_LASTBASE then
      begin
        config.Free();
        WAD.Free();
        Exit;
      end
      else
      begin
        ExtAnimFromBaseAnim(s, b);
        continue;
      end;
    end;

    for aa := WP_FIRST + 1 to WP_LAST do
      for bb := A_STAND to A_LAST do
        for cc := TDirection.D_LEFT to TDirection.D_RIGHT do
        begin
          f := config.ReadInt(AnimNames[bb], 'frames', 1);
          if config.ReadBool(AnimNames[bb], 'backanim', False) then
            if f > 2 then f := 2*f-2;
          SetLength(PlayerModelsArray[ID].WeaponPoints[aa, bb, cc], f);
        end;

    if (config.ReadStr(AnimNames[b], 'resource2', '') <> '') and
       (config.ReadStr(AnimNames[b], 'mask2', '') <> '') then
    begin
      aname := s+'_LEFTANIM'+IntToStr(b);
      g_Frames_CreateWAD(nil, aname,
                         prefix+config.ReadStr(AnimNames[b], 'resource2', ''),
                         64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                         config.ReadBool(AnimNames[b], 'backanim', False));

      g_Frames_CreateWAD(nil, aname+'_MASK',
                         prefix+config.ReadStr(AnimNames[b], 'mask2', ''),
                         64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                         config.ReadBool(AnimNames[b], 'backanim', False));
    end;

    PlayerModelsArray[ID].ModelSpeed[b] := Max(1, config.ReadInt(AnimNames[b], 'waitcount', 1) div 3);
  end;

  with PlayerModelsArray[ID], config do
  begin
    prefix := FileName+':SOUNDS\';

    a := 1;
    repeat
      s := config.ReadStr('Sound', 'pain'+IntToStr(a), '');
      if s <> '' then
      begin
        SetLength(PainSounds, Length(PainSounds)+1);
        g_Sound_CreateWAD(PainSounds[High(PainSounds)].ID, prefix+s);
        PainSounds[High(PainSounds)].Level := config.ReadInt('Sound', 'painlevel'+IntToStr(a), 1);
      end;
      a := a+1;
    until s = '';

    a := 1;
    repeat
      s := config.ReadStr('Sound', 'die'+IntToStr(a), '');
      if s <> '' then
      begin
        SetLength(DieSounds, Length(DieSounds)+1);
        g_Sound_CreateWAD(DieSounds[High(DieSounds)].ID, prefix+s);
        DieSounds[High(DieSounds)].Level := config.ReadInt('Sound', 'dielevel'+IntToStr(a), 1);
      end;
      a := a+1;
    until s = '';

    SlopSound := Min(Max(config.ReadInt('Sound', 'slop', 0), 0), 2);

    SetLength(Gibs, ReadInt('Gibs', 'count', 0));

    if (Gibs <> nil) and
       (WAD.GetResource('TEXTURES/'+config.ReadStr('Gibs', 'resource', 'GIBS'), pData, lenpd)) and
       (WAD.GetResource('TEXTURES/'+config.ReadStr('Gibs', 'mask', 'GIBSMASK'), pData2, lenpd2)) then
    begin
      for a := 0 to High(Gibs) do
        if e_CreateTextureMemEx(pData, lenpd, Gibs[a].ID, a*32, 0, 32, 32) and
          e_CreateTextureMemEx(pData2, lenpd2, Gibs[a].MaskID, a*32, 0, 32, 32) then
        begin
          //Gibs[a].Rect := e_GetTextureSize2(Gibs[a].ID);
          Gibs[a].Rect := g_PlayerModel_CalcGibSize(pData, lenpd, a*32, 0, 32, 32);
          with Gibs[a].Rect do
            if Height > 3 then Height := Height-1-Random(2);
          Gibs[a].OnlyOne := config.ReadInt('Gibs', 'once', -1) = a+1;
        end;

      FreeMem(pData);
      FreeMem(pData2);
    end;

    ok := True;
    for aa := WP_FIRST + 1 to WP_LAST do
      for bb := A_STAND to A_LAST do
        if not (bb in [A_DIE1, A_DIE2, A_PAIN]) then
        begin
          chk := GetWeapPoints(config.ReadStr(AnimNames[bb], WeapNames[aa]+'_points', ''), aa, bb, TDirection.D_RIGHT,
                               config.ReadInt(AnimNames[bb], 'frames', 0),
                               config.ReadBool(AnimNames[bb], 'backanim', False),
                               WeaponPoints);
          if ok and (not chk) and (aa = WEAPON_FLAMETHROWER) then
          begin
            // workaround for flamethrower
            chk := GetWeapPoints(config.ReadStr(AnimNames[bb], WeapNames[WEAPON_PLASMA]+'_points', ''), aa, bb, TDirection.D_RIGHT,
                                 config.ReadInt(AnimNames[bb], 'frames', 0),
                                 config.ReadBool(AnimNames[bb], 'backanim', False),
                                 WeaponPoints);
            if chk then
            for f := 0 to High(WeaponPoints[aa, bb, TDirection.D_RIGHT]) do
            begin
              case bb of
                A_STAND, A_PAIN:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 6);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 8);
                end;
                A_WALKATTACK, A_WALK:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 9);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 9);
                end;
                A_ATTACK:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 5);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 8);
                end;
                A_WALKSEEUP, A_SEEUP:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 5);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 16);
                end;
                A_WALKSEEDOWN, A_SEEDOWN:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 6);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 5);
                end;
                A_WALKATTACKUP, A_ATTACKUP:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 5);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 16);
                end;
                A_WALKATTACKDOWN, A_ATTACKDOWN:
                begin
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X, 6);
                  Dec(WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y, 4);
                end;
              end;
            end;
          end;
          ok := ok and (chk or (bb > A_LASTBASE));

          if not GetWeapPoints(config.ReadStr(AnimNames[bb], WeapNames[aa]+'2_points', ''), aa, bb, TDirection.D_LEFT,
                               config.ReadInt(AnimNames[bb], 'frames', 0),
                               config.ReadBool(AnimNames[bb], 'backanim', False),
                               WeaponPoints) then
            for f := 0 to High(WeaponPoints[aa, bb, TDirection.D_RIGHT]) do
            begin
              WeaponPoints[aa, bb, TDirection.D_LEFT, f].X := -WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X;
              WeaponPoints[aa, bb, TDirection.D_LEFT, f].Y := WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y;
            end;

          if not ok then Break;
        end;
    {if ok then g_Console_Add(Info.Name+' weapon points ok')
    else g_Console_Add(Info.Name+' weapon points fail');}
    Info.HaveWeapon := ok;

    s := config.ReadStr('Model', 'flag_point', '');
    if not GetPoint(s, FlagPoint) then FlagPoint := FLAG_DEFPOINT;

    FlagAngle := config.ReadInt('Model', 'flag_angle', FLAG_DEFANGLE);
  end;

  config.Free();
  WAD.Free();

  Result := True;
end;

function g_PlayerModel_Get(ModelName: String): TPlayerModel;
var
  a: Integer;
  b: Byte;
  ID, ID2: DWORD;
begin
  Result := nil;

  if PlayerModelsArray = nil then Exit;

  for a := 0 to High(PlayerModelsArray) do
    if AnsiLowerCase(PlayerModelsArray[a].Info.Name) = AnsiLowerCase(ModelName) then
    begin
      Result := TPlayerModel.Create;

      with PlayerModelsArray[a] do
      begin
        Result.FName := Info.Name;
        Result.FBlood := Blood;

        for b := A_STAND to A_LAST do
        begin
          if not (g_Frames_Get(ID, Info.Name+'_RIGHTANIM'+IntToStr(b)) and
                  g_Frames_Get(ID2, Info.Name+'_RIGHTANIM'+IntToStr(b)+'_MASK')) then
          begin
            Result.Free();
            Result := nil;
            Exit;
          end;

          Result.FAnim[TDirection.D_RIGHT][b] := TAnimation.Create(ID, b in [A_STAND, A_WALK], ModelSpeed[b]);

          Result.FMaskAnim[TDirection.D_RIGHT][b] := TAnimation.Create(ID2, b in [A_STAND, A_WALK], ModelSpeed[b]);

          if g_Frames_Exists(Info.Name+'_LEFTANIM'+IntToStr(b)) and
             g_Frames_Exists(Info.Name+'_LEFTANIM'+IntToStr(b)+'_MASK') then
          if g_Frames_Get(ID, Info.Name+'_LEFTANIM'+IntToStr(b)) and
             g_Frames_Get(ID2, Info.Name+'_LEFTANIM'+IntToStr(b)+'_MASK') then
          begin
            Result.FAnim[TDirection.D_LEFT][b] := TAnimation.Create(ID, b in [A_STAND, A_WALK], ModelSpeed[b]);

            Result.FMaskAnim[TDirection.D_LEFT][b] := TAnimation.Create(ID2, b in [A_STAND, A_WALK], ModelSpeed[b]);
          end;
        end;

        Result.FPainSounds := PainSounds;
        Result.FDieSounds := DieSounds;
        Result.FSlopSound := SlopSound;
        Result.FDrawWeapon := Info.HaveWeapon;
        Result.FWeaponPoints := WeaponPoints;

        Result.FFlagPoint := FlagPoint;
        Result.FFlagAngle := FlagAngle;

        Break;
      end;
  end;
end;

function g_PlayerModel_GetAnim(ModelName: string; Anim: Byte; var _Anim, _Mask: TAnimation): Boolean;
var
  a: Integer;
  c: Boolean;
  ID: DWORD;
begin
  Result := False;

  if PlayerModelsArray = nil then Exit;
  for a := 0 to High(PlayerModelsArray) do
    if PlayerModelsArray[a].Info.Name = ModelName then
      with PlayerModelsArray[a] do
      begin
        if Anim in [A_STAND, A_WALK] then c := True else c := False;

        if not g_Frames_Get(ID, Info.Name+'_RIGHTANIM'+IntToStr(Anim)) then
          if not g_Frames_Get(ID, Info.Name+'_LEFTANIM'+IntToStr(Anim)) then Exit;

        _Anim := TAnimation.Create(ID, c, ModelSpeed[Anim]);
        _Anim.Speed := ModelSpeed[Anim];

        if not g_Frames_Get(ID, Info.Name+'_RIGHTANIM'+IntToStr(Anim)+'_MASK') then
          if not g_Frames_Get(ID, Info.Name+'_LEFTANIM'+IntToStr(Anim)+'_MASK') then Exit;

        _Mask := TAnimation.Create(ID, c, ModelSpeed[Anim]);
        _Mask.Speed := ModelSpeed[Anim];

        Break;
      end;

  Result := True;
end;

function g_PlayerModel_GetGibs(ModelName: string; var Gibs: TGibsArray): Boolean;
var
  a, i, b: Integer;
  c: Boolean;
begin
  Result := False;

  if PlayerModelsArray = nil then Exit;
  if gGibsCount = 0 then Exit;

  c := False;

  SetLength(Gibs, gGibsCount);

  for a := 0 to High(PlayerModelsArray) do
    if PlayerModelsArray[a].Info.Name = ModelName then
    begin
      for i := 0 to High(Gibs) do
      begin
        if c and (Length(PlayerModelsArray[a].Gibs) = 1) then
        begin
          SetLength(Gibs, i);
          Break;
        end;

        repeat
          b := Random(Length(PlayerModelsArray[a].Gibs));
        until not (PlayerModelsArray[a].Gibs[b].OnlyOne and c);

        Gibs[i] := PlayerModelsArray[a].Gibs[b];

        if Gibs[i].OnlyOne then c := True;
      end;

      Result := True;
      Break;
    end;
end;

function g_PlayerModel_GetNames(): SSArray;
var
  i: DWORD;
begin
  Result := nil;

  if PlayerModelsArray = nil then Exit;

  for i := 0 to High(PlayerModelsArray) do
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := PlayerModelsArray[i].Info.Name;
  end;
end;

function g_PlayerModel_GetInfo(ModelName: string): TModelInfo;
var
  a: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if PlayerModelsArray = nil then Exit;

  for a := 0 to High(PlayerModelsArray) do
    if PlayerModelsArray[a].Info.Name = ModelName then
    begin
      Result := PlayerModelsArray[a].Info;
      Break;
    end;
end;

function g_PlayerModel_GetBlood(ModelName: string): TModelBlood;
var
  a: Integer;
begin
  Result.R := 150;
  Result.G := 0;
  Result.B := 0;
  Result.Kind := BLOOD_NORMAL;
  if PlayerModelsArray = nil then Exit;

  for a := 0 to High(PlayerModelsArray) do
    if PlayerModelsArray[a].Info.Name = ModelName then
    begin
      Result := PlayerModelsArray[a].Blood;
      Break;
    end;
end;

procedure g_PlayerModel_FreeData();
var
  i: DWORD;
  a, b: Integer;
begin
  e_WriteLog('Releasing models...', TMsgType.Notify);

  if PlayerModelsArray = nil then Exit;

  for i := 0 to High(PlayerModelsArray) do
    with PlayerModelsArray[i] do
    begin
      for a := A_STAND to A_LAST do
      begin
        g_Frames_DeleteByName(Info.Name+'_LEFTANIM'+IntToStr(a));
        g_Frames_DeleteByName(Info.Name+'_LEFTANIM'+IntToStr(a)+'_MASK');
        g_Frames_DeleteByName(Info.Name+'_RIGHTANIM'+IntToStr(a));
        g_Frames_DeleteByName(Info.Name+'_RIGHTANIM'+IntToStr(a)+'_MASK');
      end;

      if PainSounds <> nil then
        for b := 0 to High(PainSounds) do
          e_DeleteSound(PainSounds[b].ID);

      if DieSounds <> nil then
        for b := 0 to High(DieSounds) do
          e_DeleteSound(DieSounds[b].ID);

      if Gibs <> nil then
        for b := 0 to High(Gibs) do
        begin
          e_DeleteTexture(Gibs[b].ID);
          e_DeleteTexture(Gibs[b].MaskID);
        end;
    end;

  PlayerModelsArray := nil;
end;

{ TPlayerModel }

procedure TPlayerModel.ChangeAnimation(Animation: Byte; Force: Boolean = False);
begin
  if not Force then if FCurrentAnimation = Animation then Exit;

  FCurrentAnimation := Animation;

  if (FDirection = TDirection.D_LEFT) and
     (FAnim[TDirection.D_LEFT][FCurrentAnimation] <> nil) and
     (FMaskAnim[TDirection.D_LEFT][FCurrentAnimation] <> nil) then
  begin
    FAnim[TDirection.D_LEFT][FCurrentAnimation].Reset;
    FMaskAnim[TDirection.D_LEFT][FCurrentAnimation].Reset;
  end
  else
  begin
    FAnim[TDirection.D_RIGHT][FCurrentAnimation].Reset;
    FMaskAnim[TDirection.D_RIGHT][FCurrentAnimation].Reset;
  end;
end;

destructor TPlayerModel.Destroy();
var
  a: Byte;
begin
  for a := A_STAND to A_LAST do
  begin
    FAnim[TDirection.D_LEFT][a].Free();
    FMaskAnim[TDirection.D_LEFT][a].Free();
    FAnim[TDirection.D_RIGHT][a].Free();
    FMaskAnim[TDirection.D_RIGHT][a].Free();
  end;

  inherited;
end;

function TPlayerModel.GetCurrentAnimation: TAnimation;
begin
  if (FDirection = TDirection.D_LEFT) and (FAnim[TDirection.D_LEFT][FCurrentAnimation] <> nil) then
    Result := FAnim[TDirection.D_LEFT][FCurrentAnimation]
  else
    Result := FAnim[TDirection.D_RIGHT][FCurrentAnimation];
end;

function TPlayerModel.GetCurrentAnimationMask: TAnimation;
begin
  if (FDirection = TDirection.D_LEFT) and (FMaskAnim[TDirection.D_LEFT][FCurrentAnimation] <> nil) then
    Result := FMaskAnim[TDirection.D_LEFT][FCurrentAnimation]
  else
    Result := FMaskAnim[TDirection.D_RIGHT][FCurrentAnimation];
end;

function TPlayerModel.PlaySound(SoundType, Level: Byte; X, Y: Integer): Boolean;
var
  TempArray: array of DWORD;
  a: Integer;
begin
  Result := False;
  SetLength(TempArray, 0);

  if SoundType = MODELSOUND_PAIN then
  begin
    if FPainSounds = nil then Exit;

    for a := 0 to High(FPainSounds) do
      if FPainSounds[a].Level = Level then
      begin
        SetLength(TempArray, Length(TempArray)+1);
        TempArray[High(TempArray)] := FPainSounds[a].ID;
      end;
  end
  else
  begin
    if (Level in [2, 3, 5]) and (FSlopSound > 0) then
    begin
      g_Sound_PlayExAt('SOUND_MONSTER_SLOP', X, Y);
      if FSlopSound = 1 then
      begin
        Result := True;
        Exit;
      end;
    end;
    if FDieSounds = nil then Exit;

    for a := 0 to High(FDieSounds) do
      if FDieSounds[a].Level = Level then
      begin
        SetLength(TempArray, Length(TempArray)+1);
        TempArray[High(TempArray)] := FDieSounds[a].ID;
      end;
    if (TempArray = nil) and (Level = 5) then
    begin
      g_Sound_PlayExAt('SOUND_MONSTER_SLOP', X, Y);
      Result := True;
      Exit;
    end;
  end;

  if TempArray = nil then Exit;

  g_Sound_PlayAt(TempArray[Random(Length(TempArray))], X, Y);

  Result := True;
end;

procedure TPlayerModel.SetColor(Red, Green, Blue: Byte);
begin
  FColor.R := Red;
  FColor.G := Green;
  FColor.B := Blue;
end;

procedure TPlayerModel.SetFire(Fire: Boolean);
begin
  FFire := Fire;

  if FFire then FFireCounter := FAnim[TDirection.D_RIGHT, A_ATTACK].Speed*FAnim[TDirection.D_RIGHT, A_ATTACK].TotalFrames
  else FFireCounter := 0;
end;

procedure TPlayerModel.SetFlag(Flag: Byte);
var
  id: DWORD;
begin
  FFlag := Flag;

  FFlagAnim.Free();
  FFlagAnim := nil;

  case Flag of
    FLAG_RED: g_Frames_Get(id, 'FRAMES_FLAG_RED');
    FLAG_BLUE: g_Frames_Get(id, 'FRAMES_FLAG_BLUE');
    else Exit;
  end;

  FFlagAnim := TAnimation.Create(id, True, 8);
end;

procedure TPlayerModel.SetWeapon(Weapon: Byte);
begin
  FCurrentWeapon := Weapon;
end;

procedure TPlayerModel.Update();
begin
  if (FDirection = TDirection.D_LEFT) and (FAnim[TDirection.D_LEFT][FCurrentAnimation] <> nil) then
    FAnim[TDirection.D_LEFT][FCurrentAnimation].Update else FAnim[TDirection.D_RIGHT][FCurrentAnimation].Update;

  if (FDirection = TDirection.D_LEFT) and (FMaskAnim[TDirection.D_LEFT][FCurrentAnimation] <> nil) then
    FMaskAnim[TDirection.D_LEFT][FCurrentAnimation].Update else FMaskAnim[TDirection.D_RIGHT][FCurrentAnimation].Update;

  if FFlagAnim <> nil then FFlagAnim.Update;

  if FFireCounter > 0 then Dec(FFireCounter) else FFire := False;
end;

end.
