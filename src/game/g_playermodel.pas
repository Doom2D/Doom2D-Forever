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
  MAPDEF, g_textures, g_base, g_basic, g_weapons, r_graphics, utils, g_gfx,
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
  TWeaponPoints = Array [WP_FIRST + 1..WP_LAST, A_STAND..A_LAST, TDirection.D_LEFT..TDirection.D_RIGHT] of Array of TDFPoint;

  TModelMatrix = Array [TDirection.D_LEFT..TDirection.D_RIGHT, A_STAND..A_LAST] of TAnimationState;

  TModelTextures = Array [TDirection.D_LEFT..TDirection.D_RIGHT, A_STAND..A_LAST] of record
    Resource: String;
    Mask:     String;
    Frames:   Integer;
    Back:     Boolean;
  end;

  TModelBlood = record
    R, G, B, Kind: Byte;
  end;

  TModelInfo = record
    Name:        String;
    Author:      String;
    Description: String;
    HaveWeapon:  Boolean;
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

  TPlayerModel = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    FDirection:        TDirection;
    FColor:            TRGB;
    FCurrentAnimation: Byte;
    FAnimState:        TAnimationState;
    FPainSounds:       TModelSoundArray;
    FDieSounds:        TModelSoundArray;
    FSlopSound:        Byte;
    FCurrentWeapon:    Byte;
    FFlag:             Byte;
    FFlagPoint:        TDFPoint;
    FFlagAngle:        SmallInt;
    FFlagAnim:         TAnimation; // !!! TAnimationState
    FFire:             Boolean;
    FFireCounter:      Byte;
    FID:               Integer;

  public
    destructor  Destroy(); override;
    procedure   ChangeAnimation(Animation: Byte; Force: Boolean = False);
    procedure   SetColor(Red, Green, Blue: Byte);
    procedure   SetWeapon(Weapon: Byte);
    procedure   SetFlag(Flag: Byte);
    procedure   SetFire(Fire: Boolean);
    function    PlaySound(SoundType, Level: Byte; X, Y: Integer): Boolean;
    procedure   Update();

    function GetBlood (): TModelBlood;
    function GetName (): String;

  published
    property    Fire: Boolean read FFire;
    property    Direction: TDirection read FDirection write FDirection;
    property    Animation: Byte read FCurrentAnimation;
    property    Weapon: Byte read FCurrentWeapon;

  public
    property    Color: TRGB read FColor write FColor;

    property    AnimState: TAnimationState read FAnimState;
    property    CurrentAnimation: Byte read FCurrentAnimation;

    property    CurrentWeapon: Byte read FCurrentWeapon;

    property    Flag: Byte read FFlag;
    property    FlagAnim: TAnimation read FFlagAnim;
    property    FlagAngle: SmallInt read FFlagAngle;
    property    FlagPoint: TDFPoint read FFlagPoint;

    property    ID: Integer read FID;
  end;

procedure g_PlayerModel_LoadAll;
procedure g_PlayerModel_FreeData();
function  g_PlayerModel_Load(FileName: String): Boolean;
function  g_PlayerModel_GetNames(): SSArray;
function  g_PlayerModel_GetInfo(ModelName: String): TModelInfo;
function  g_PlayerModel_GetBlood(ModelName: String): TModelBlood;
function  g_PlayerModel_Get(ModelName: String): TPlayerModel;
function  g_PlayerModel_GetAnim(ModelName: String; AnimTyp: Byte; var _Anim, _Mask: TAnimation): Boolean;
function  g_PlayerModel_GetGibs(ModelName: String; var Gibs: TGibsArray): Boolean;

(* --- private data --- *)

  type
    TPlayerModelInfo = record
      Info:         TModelInfo;
      ModelSpeed:   Array [A_STAND..A_PAIN] of Byte;
      FlagPoint:    TDFPoint;
      FlagAngle:    SmallInt;
      WeaponPoints: TWeaponPoints;
      Gibs:         TGibsArray; // !!! move to render
      PainSounds:   TModelSoundArray;
      DieSounds:    TModelSoundArray;
      SlopSound:    Byte;
      Blood:        TModelBlood;
      // =======================
      FileName:    String;
      Anim:        TModelTextures;
      GibsCount:   Integer;
      GibsResource:String;
      GibsMask:    String;
      GibsOnce:    Integer;
    end;

  var
    PlayerModelsArray: Array of TPlayerModelInfo;

implementation

uses
  g_sound, g_console, SysUtils, g_player, CONFIG, r_textures, r_animations,
  e_sound, g_options, g_map, Math, e_log, wadreader;

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

  procedure g_PlayerMode_ExtendPoints (id: Integer; AIdx: Integer);
    const
      CopyAnim: array [A_LASTBASE+1..A_LASTEXT] of Integer = (
        A_WALK, A_WALK, A_WALK, A_WALK, A_WALK,
        A_STAND, A_WALK, A_ATTACK, A_WALK, A_SEEUP, A_SEEDOWN,
        A_ATTACKUP, A_ATTACKDOWN
      );
    var W, I, OIdx: Integer; D: TDirection;
  begin
    OIdx := CopyAnim[AIdx];
    with PlayerModelsArray[id] do
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
  s: string;
  prefix: string;
  ok, chk, chk2: Boolean;
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

  PlayerModelsArray[ID].FileName := FileName;
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
    with PlayerModelsArray[ID].Anim[TDirection.D_RIGHT, b] do
    begin
      Resource := config.ReadStr(AnimNames[b], 'resource', '');
      Mask := config.ReadStr(AnimNames[b], 'mask', '');
      Frames := config.ReadInt(AnimNames[b], 'frames', 1);
      Back := config.ReadBool(AnimNames[b], 'backanim', False);
      if (Resource = '') or (Mask = '') then
      begin
        if b <= A_LASTBASE then
        begin
          config.Free();
          WAD.Free();
          Exit
        end
        else
        begin
          g_PlayerMode_ExtendPoints(ID, b);
          continue
        end
      end;
    end;

    for aa := WP_FIRST + 1 to WP_LAST do
      for bb := A_STAND to A_LAST do
        for cc := TDirection.D_LEFT to TDirection.D_RIGHT do
        begin
          f := PlayerModelsArray[ID].Anim[cc, bb].Frames;
          if PlayerModelsArray[ID].Anim[cc, bb].Back and (f > 2) then
            f := 2 * f - 2;
          SetLength(PlayerModelsArray[ID].WeaponPoints[aa, bb, cc], f);
        end;

    with PlayerModelsArray[ID].Anim[TDirection.D_LEFT, b] do
    begin
      Frames := PlayerModelsArray[ID].Anim[TDirection.D_RIGHT, b].Frames;
      Back := PlayerModelsArray[ID].Anim[TDirection.D_RIGHT, b].Back;
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

    GibsCount := config.ReadInt('Gibs', 'count', 0);
    GibsResource := config.ReadStr('Gibs', 'resource', 'GIBS');
    GibsMask := config.ReadStr('Gibs', 'mask', 'GIBSMASK');
    GibsOnce := config.ReadInt('Gibs', 'once', -1);

    SetLength(Gibs, GibsCount); // !!! remove load
    if (Gibs <> nil) and
       (WAD.GetResource('TEXTURES/' + GibsResource, pData, lenpd)) and
       (WAD.GetResource('TEXTURES/' + GibsMask, pData2, lenpd2)) then
    begin
      for a := 0 to High(Gibs) do
        if e_CreateTextureMemEx(pData, lenpd, Gibs[a].ID, a*32, 0, 32, 32) and
          e_CreateTextureMemEx(pData2, lenpd2, Gibs[a].MaskID, a*32, 0, 32, 32) then
        begin
          //Gibs[a].Rect := e_GetTextureSize2(Gibs[a].ID);
          Gibs[a].Rect := g_PlayerModel_CalcGibSize(pData, lenpd, a*32, 0, 32, 32);
          with Gibs[a].Rect do
            if Height > 3 then Height := Height-1-Random(2);
          Gibs[a].OnlyOne := GibsOnce = a + 1;
        end;

      FreeMem(pData);
      FreeMem(pData2);
    end;

    ok := True;
    for aa := WP_FIRST + 1 to WP_LAST do
      for bb := A_STAND to A_LAST do
        if not (bb in [A_DIE1, A_DIE2, A_PAIN]) then
        begin
          chk := GetWeapPoints(
            config.ReadStr(AnimNames[bb], WeapNames[aa] + '_points', ''),
            aa,
            bb,
            TDirection.D_RIGHT,
            Anim[TDirection.D_RIGHT, bb].Frames,
            Anim[TDirection.D_RIGHT, bb].Back,
            WeaponPoints
          );
          if ok and (not chk) and (aa = WEAPON_FLAMETHROWER) then
          begin
            // workaround for flamethrower
            chk := GetWeapPoints(
              config.ReadStr(AnimNames[bb], WeapNames[WEAPON_PLASMA] + '_points', ''),
              aa,
              bb,
              TDirection.D_RIGHT,
              Anim[TDirection.D_RIGHT, bb].Frames,
              Anim[TDirection.D_RIGHT, bb].Back,
              WeaponPoints
            );
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

          chk2 := GetWeapPoints(
            config.ReadStr(AnimNames[bb], WeapNames[aa] + '2_points', ''),
            aa,
            bb,
            TDirection.D_LEFT,
            Anim[TDirection.D_LEFT, bb].Frames,
            Anim[TDirection.D_LEFT, bb].Back,
            WeaponPoints
          );
          if not chk2 then
          begin
            for f := 0 to High(WeaponPoints[aa, bb, TDirection.D_RIGHT]) do
            begin
              WeaponPoints[aa, bb, TDirection.D_LEFT, f].X := -WeaponPoints[aa, bb, TDirection.D_RIGHT, f].X;
              WeaponPoints[aa, bb, TDirection.D_LEFT, f].Y := WeaponPoints[aa, bb, TDirection.D_RIGHT, f].Y;
            end;
          end;

          if not ok then Break;
        end;
    {if ok then g_Console_Add(Info.Name+' weapon points ok')
    else g_Console_Add(Info.Name+' weapon points fail');}
    Info.HaveWeapon := ok;

    s := config.ReadStr('Model', 'flag_point', '');
    if not GetPoint(s, FlagPoint) then
      FlagPoint := FLAG_DEFPOINT;

    FlagAngle := config.ReadInt('Model', 'flag_angle', FLAG_DEFANGLE);
  end;

  config.Free();
  WAD.Free();

  Result := True;
end;

function g_PlayerModel_Get(ModelName: String): TPlayerModel;
  var a: Integer;
begin
  Result := nil;

  if PlayerModelsArray = nil then Exit;

  for a := 0 to High(PlayerModelsArray) do
  begin
    if AnsiLowerCase(PlayerModelsArray[a].Info.Name) = AnsiLowerCase(ModelName) then
    begin
      Result := TPlayerModel.Create;

      with PlayerModelsArray[a] do
      begin
        Result.FPainSounds := PainSounds;
        Result.FDieSounds := DieSounds;
        Result.FSlopSound := SlopSound;

        Result.FFlagPoint := FlagPoint;
        Result.FFlagAngle := FlagAngle;
        Result.FID := a;

        Result.ChangeAnimation(A_STAND, True);

        Break;
      end;
    end;
  end;
end;

function g_PlayerModel_GetAnim(ModelName: string; AnimTyp: Byte; var _Anim, _Mask: TAnimation): Boolean;
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
        if AnimTyp in [A_STAND, A_WALK] then c := True else c := False;

        if not g_Frames_Get(ID, Info.Name+'_RIGHTANIM'+IntToStr(AnimTyp)) then
          if not g_Frames_Get(ID, Info.Name+'_LEFTANIM'+IntToStr(AnimTyp)) then Exit;

        _Anim := TAnimation.Create(ID, c, ModelSpeed[AnimTyp]);
        _Anim.Speed := ModelSpeed[AnimTyp];

        if not g_Frames_Get(ID, Info.Name+'_RIGHTANIM'+IntToStr(AnimTyp)+'_MASK') then
          if not g_Frames_Get(ID, Info.Name+'_LEFTANIM'+IntToStr(AnimTyp)+'_MASK') then Exit;

        _Mask := TAnimation.Create(ID, c, ModelSpeed[AnimTyp]);
        _Mask.Speed := ModelSpeed[AnimTyp];

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
  var i, b: Integer;
begin
  e_WriteLog('Releasing models...', TMsgType.Notify);

  if PlayerModelsArray = nil then Exit;

  for i := 0 to High(PlayerModelsArray) do
  begin
    with PlayerModelsArray[i] do
    begin
      if PainSounds <> nil then
        for b := 0 to High(PainSounds) do
          e_DeleteSound(PainSounds[b].ID);
      if DieSounds <> nil then
        for b := 0 to High(DieSounds) do
          e_DeleteSound(DieSounds[b].ID);
    end;
  end;
  PlayerModelsArray := nil;
end;

{ TPlayerModel }

  procedure TPlayerModel.ChangeAnimation (Animation: Byte; Force: Boolean = False);
    var once: Boolean; speed, count: Integer;
  begin
    if not Force then
      if FCurrentAnimation = Animation then
        Exit;
    FCurrentAnimation := Animation;
    once := FCurrentAnimation in [A_STAND, A_WALK];
    speed := PlayerModelsArray[FID].ModelSpeed[FCurrentAnimation];
    count := PlayerModelsArray[FID].Anim[FDirection, FCurrentAnimation].Frames;
    FAnimState := TAnimationState.Create(once, speed, count);
  end;

destructor TPlayerModel.Destroy();
begin
  FAnimState.Free;
  inherited;
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
  if FFire then
    FFireCounter := PlayerModelsArray[FID].ModelSpeed[A_ATTACK] * PlayerModelsArray[FID].Anim[TDirection.D_RIGHT, A_ATTACK].Frames
  else
    FFireCounter := 0
end;

procedure TPlayerModel.SetFlag(Flag: Byte);
var
  tid: DWORD;
begin
  FFlag := Flag;

  FFlagAnim.Free();
  FFlagAnim := nil;

  case Flag of
    FLAG_RED: g_Frames_Get(tid, 'FRAMES_FLAG_RED');
    FLAG_BLUE: g_Frames_Get(tid, 'FRAMES_FLAG_BLUE');
    else Exit;
  end;

  FFlagAnim := TAnimation.Create(tid, True, 8);
end;

  procedure TPlayerModel.SetWeapon(Weapon: Byte);
  begin
    FCurrentWeapon := Weapon;
  end;

  function TPlayerModel.GetBlood (): TModelBlood;
  begin
    Result := PlayerModelsArray[FID].Blood
  end;

  function TPlayerModel.GetName (): String;
  begin
    Result := PlayerModelsArray[FID].Info.Name
  end;

  procedure TPlayerModel.Update;
  begin
    if FAnimState <> nil then
      FAnimState.Update;
    if FFlagAnim <> nil then
      FFlagAnim.Update;
    if FFireCounter > 0 then
      Dec(FFireCounter)
    else
      FFire := False
  end;

  procedure g_PlayerModel_LoadAll;
    var
      SR: TSearchRec;
      knownFiles: array of AnsiString = nil;
      found: Boolean;
      wext, s: AnsiString;
      f: Integer;
  begin
    // load models from all possible wad types, in all known directories
    // this does a loosy job (linear search, ooph!), but meh
    for wext in wadExtensions do
    begin
      for f := High(ModelDirs) downto Low(ModelDirs) do
      begin
        if (FindFirst(ModelDirs[f]+DirectorySeparator+'*'+wext, faAnyFile, SR) = 0) then
        begin
          repeat
            found := false;
            for s in knownFiles do
            begin
              if (strEquCI1251(forceFilenameExt(SR.Name, ''), forceFilenameExt(ExtractFileName(s), ''))) then
              begin
                found := true;
                break;
              end;
            end;
            if not found then
            begin
              SetLength(knownFiles, length(knownFiles)+1);
              knownFiles[High(knownFiles)] := ModelDirs[f]+DirectorySeparator+SR.Name;
            end;
          until (FindNext(SR) <> 0);
        end;
        FindClose(SR);
      end;
    end;
    if (length(knownFiles) = 0) then
      raise Exception.Create('no player models found!');
    if (length(knownFiles) = 1) then
      e_LogWriteln('1 player model found.', TMsgType.Notify)
    else
      e_LogWritefln('%d player models found.', [Integer(length(knownFiles))], TMsgType.Notify);
    for s in knownFiles do
      if not g_PlayerModel_Load(s) then
        e_LogWritefln('Error loading model "%s"', [s], TMsgType.Warning);
  end;

end.
