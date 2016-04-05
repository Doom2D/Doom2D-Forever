unit g_playermodel;

interface

uses
  g_textures, g_basic, e_graphics, WADEDITOR,
  WADSTRUCT, g_weapons;

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

  MODELSOUND_PAIN = 0;
  MODELSOUND_DIE  = 1;
  
type
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
  TWeaponPoints = Array [WEAPON_SAW..WEAPON_SUPERPULEMET] of
                  Array [A_STAND..A_PAIN] of
                  Array [D_LEFT..D_RIGHT] of Array of TPoint;

  TPlayerModel = class (TObject)
  private
    FName:             String;
    FDirection:        TDirection;
    FColor:            TRGB;
    FCurrentAnimation: Byte;
    FAnim:             Array [D_LEFT..D_RIGHT] of Array [A_STAND..A_PAIN] of TAnimation;
    FMaskAnim:         Array [D_LEFT..D_RIGHT] of Array [A_STAND..A_PAIN] of TAnimation;
    FWeaponPoints:     TWeaponPoints;
    FPainSounds:       TModelSoundArray;
    FDieSounds:        TModelSoundArray;
    FSlopSound:        Byte;
    FCurrentWeapon:    Byte;
    FDrawWeapon:       Boolean;
    FFlag:             Byte;
    FFlagPoint:        TPoint;
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
    procedure   Draw(X, Y: Integer; Alpha: Byte = 0);

    property    Fire: Boolean read FFire;
    property    Direction: TDirection read FDirection write FDirection;
    property    Animation: Byte read FCurrentAnimation;
    property    Weapon: Byte read FCurrentWeapon;
    property    Name: String read FName;
    property    Color: TRGB read FColor write FColor;
  end;

procedure g_PlayerModel_LoadData();
procedure g_PlayerModel_FreeData();
function  g_PlayerModel_Load(FileName: String): Boolean;
function  g_PlayerModel_GetNames(): SArray;
function  g_PlayerModel_GetInfo(ModelName: String): TModelInfo;
function  g_PlayerModel_Get(ModelName: String): TPlayerModel;
function  g_PlayerModel_GetAnim(ModelName: String; Anim: Byte; var _Anim, _Mask: TAnimation): Boolean;
function  g_PlayerModel_GetGibs(ModelName: String; var Gibs: TGibsArray): Boolean;

implementation

uses
  g_main, g_sound, g_console, SysUtils, g_player, CONFIG,
  GL, GLExt, e_sound, g_options, g_map, Math, e_log;

type
  TPlayerModelInfo = record
    Info:         TModelInfo;
    ModelSpeed:   Array [A_STAND..A_PAIN] of Byte;
    FlagPoint:    TPoint;
    FlagAngle:    SmallInt;
    WeaponPoints: TWeaponPoints;
    Gibs:         TGibsArray;
    PainSounds:   TModelSoundArray;
    DieSounds:    TModelSoundArray;
    SlopSound:    Byte;
  end;

const
  W_POS_NORMAL = 0;
  W_POS_UP     = 1;
  W_POS_DOWN   = 2;

  W_ACT_NORMAL = 0;
  W_ACT_FIRE   = 1;

  FLAG_BASEPOINT: TPoint = (X:16; Y:43);
  FLAG_DEFPOINT:  TPoint = (X:32; Y:16);
  FLAG_DEFANGLE = -20;
  WEAPONBASE: Array [WEAPON_SAW..WEAPON_SUPERPULEMET] of TPoint =
              ((X:8; Y:4), (X:8; Y:8), (X:16; Y:16), (X:16; Y:24),
               (X:16; Y:16), (X:24; Y:24), (X:16; Y:16), (X:24; Y:24), (X:16; Y:16));

  AnimNames: Array [A_STAND..A_PAIN] of String =
             ('StandAnim','WalkAnim','Die1Anim','Die2Anim','AttackAnim',
              'SeeUpAnim','SeeDownAnim','AttackUpAnim','AttackDownAnim','PainAnim');
  WeapNames: Array [WEAPON_SAW..WEAPON_SUPERPULEMET] of String =
             ('csaw', 'hgun', 'sg', 'ssg', 'mgun', 'rkt', 'plz', 'bfg', 'spl');

var
  WeaponID: Array [WEAPON_SAW..WEAPON_SUPERPULEMET] of
            Array [W_POS_NORMAL..W_POS_DOWN] of
            Array [W_ACT_NORMAL..W_ACT_FIRE] of DWORD;
  PlayerModelsArray: Array of TPlayerModelInfo;

procedure g_PlayerModel_LoadData();
var
  a: Integer;
begin
  for a := WEAPON_SAW to WEAPON_SUPERPULEMET do
  begin
    g_Texture_CreateWAD(WeaponID[a][W_POS_NORMAL][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a]));
    g_Texture_CreateWAD(WeaponID[a][W_POS_NORMAL][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_FIRE');
    g_Texture_CreateWAD(WeaponID[a][W_POS_UP][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_UP');
    g_Texture_CreateWAD(WeaponID[a][W_POS_UP][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_UP_FIRE');
    g_Texture_CreateWAD(WeaponID[a][W_POS_DOWN][W_ACT_NORMAL], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_DN');
    g_Texture_CreateWAD(WeaponID[a][W_POS_DOWN][W_ACT_FIRE], GameWAD+':WEAPONS\'+UpperCase(WeapNames[a])+'_DN_FIRE');
  end;
end;

function GetPoint(var str: String; var point: TPoint): Boolean;
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
      if dir = D_LEFT then
        X := -X;
    end;
  end;

  h := High(wpoints[weapon, anim, dir]);
  if backanim then
    for b := h downto frames do
      wpoints[weapon, anim, dir, b] := wpoints[weapon, anim, dir, h-b+1];

  Result := True;
end;

function g_PlayerModel_Load(FileName: string): Boolean;
var
  ID: DWORD;
  a, b, len, aa, bb, f: Integer;
  cc: TDirection;
  config: TConfig;
  pData, pData2: Pointer;
  WAD: TWADEditor_1;
  s: string;
  prefix: string;
  ok: Boolean;
begin
  e_WriteLog(Format('Loading player model: %s', [ExtractFileName(FileName)]), MSG_NOTIFY);

  Result := False;

  WAD := TWADEditor_1.Create;
  WAD.ReadFile(FileName);

  if WAD.GetLastError <> DFWAD_NOERROR then
  begin
    WAD.Free();
    Exit;
  end;

  if not WAD.GetResource('TEXT', 'MODEL', pData, len) then
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

  for b := A_STAND to A_PAIN do
  begin
    if not (g_Frames_CreateWAD(nil, s+'_RIGHTANIM'+IntToStr(b),
                               prefix+config.ReadStr(AnimNames[b], 'resource', ''),
                               64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                               config.ReadBool(AnimNames[b], 'backanim', False)) and
            g_Frames_CreateWAD(nil, s+'_RIGHTANIM'+IntToStr(b)+'_MASK',
                               prefix+config.ReadStr(AnimNames[b], 'mask', ''),
                               64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                               config.ReadBool(AnimNames[b], 'backanim', False))) then
    begin
      config.Free();
      WAD.Free();
      Exit;
    end;

    for aa := WEAPON_SAW to WEAPON_SUPERPULEMET do
      for bb := A_STAND to A_PAIN do
        for cc := D_LEFT to D_RIGHT do
        begin
          f := config.ReadInt(AnimNames[bb], 'frames', 1);
          if config.ReadBool(AnimNames[bb], 'backanim', False) then
            if f > 2 then f := 2*f-2;
          SetLength(PlayerModelsArray[ID].WeaponPoints[aa, bb, cc], f);
        end;

    if (config.ReadStr(AnimNames[b], 'resource2', '') <> '') and
       (config.ReadStr(AnimNames[b], 'mask2', '') <> '') then
    begin
      g_Frames_CreateWAD(nil, s+'_LEFTANIM'+IntToStr(b),
                         prefix+config.ReadStr(AnimNames[b], 'resource2', ''),
                         64, 64, config.ReadInt(AnimNames[b], 'frames', 1),
                         config.ReadBool(AnimNames[b], 'backanim', False));

      g_Frames_CreateWAD(nil, s+'_LEFTANIM'+IntToStr(b)+'_MASK',
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
       (WAD.GetResource('TEXTURES', config.ReadStr('Gibs', 'resource', 'GIBS'), pData, len)) and
       (WAD.GetResource('TEXTURES', config.ReadStr('Gibs', 'mask', 'GIBSMASK'), pData2, len)) then
    begin
      for a := 0 to High(Gibs) do
        if e_CreateTextureMemEx(pData, Gibs[a].ID, a*32, 0, 32, 32) and
          e_CreateTextureMemEx(pData2, Gibs[a].MaskID, a*32, 0, 32, 32) then
        begin
          Gibs[a].Rect := e_GetTextureSize2(Gibs[a].ID);
          with Gibs[a].Rect do
            if Height > 3 then Height := Height-1-Random(2);
          Gibs[a].OnlyOne := config.ReadInt('Gibs', 'once', -1) = a+1;
        end;

      FreeMem(pData);
      FreeMem(pData2);
    end;

    ok := True;
    for aa := WEAPON_SAW to WEAPON_SUPERPULEMET do
      for bb := A_STAND to A_PAIN do
        if not (bb in [A_DIE1, A_DIE2, A_PAIN]) then
        begin
          ok := ok and GetWeapPoints(config.ReadStr(AnimNames[bb], WeapNames[aa]+'_points', ''), aa, bb, D_RIGHT,
                                     config.ReadInt(AnimNames[bb], 'frames', 0),
                                     config.ReadBool(AnimNames[bb], 'backanim', False),
                                     WeaponPoints);

          if not GetWeapPoints(config.ReadStr(AnimNames[bb], WeapNames[aa]+'2_points', ''), aa, bb, D_LEFT,
                               config.ReadInt(AnimNames[bb], 'frames', 0),
                               config.ReadBool(AnimNames[bb], 'backanim', False),
                               WeaponPoints) then
            for f := 0 to High(WeaponPoints[aa, bb, D_RIGHT]) do
            begin
              WeaponPoints[aa, bb, D_LEFT, f].X := -WeaponPoints[aa, bb, D_RIGHT, f].X;
              WeaponPoints[aa, bb, D_LEFT, f].Y := WeaponPoints[aa, bb, D_RIGHT, f].Y;
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

        for b := A_STAND to A_PAIN do
        begin
          if not (g_Frames_Get(ID, Info.Name+'_RIGHTANIM'+IntToStr(b)) and
                  g_Frames_Get(ID2, Info.Name+'_RIGHTANIM'+IntToStr(b)+'_MASK')) then
          begin
            Result.Free();
            Result := nil;
          Exit;
        end;

        Result.FAnim[D_RIGHT][b] := TAnimation.Create(ID, b in [A_STAND, A_WALK], ModelSpeed[b]);

        Result.FMaskAnim[D_RIGHT][b] := TAnimation.Create(ID2, b in [A_STAND, A_WALK], ModelSpeed[b]);

        if g_Frames_Exists(Info.Name+'_LEFTANIM'+IntToStr(b)) and
           g_Frames_Exists(Info.Name+'_LEFTANIM'+IntToStr(b)+'_MASK') then
        if g_Frames_Get(ID, Info.Name+'_LEFTANIM'+IntToStr(b)) and
           g_Frames_Get(ID2, Info.Name+'_LEFTANIM'+IntToStr(b)+'_MASK') then
        begin
          Result.FAnim[D_LEFT][b] := TAnimation.Create(ID, b in [A_STAND, A_WALK], ModelSpeed[b]);

          Result.FMaskAnim[D_LEFT][b] := TAnimation.Create(ID2, b in [A_STAND, A_WALK], ModelSpeed[b]);
        end;

        Result.FPainSounds := PainSounds;
        Result.FDieSounds := DieSounds;
        Result.FSlopSound := SlopSound;
      end;

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

function g_PlayerModel_GetNames(): SArray;
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

procedure g_PlayerModel_FreeData();
var
  i: DWORD;
  a, b, c: Integer;
begin
  for a := WEAPON_SAW to WEAPON_SUPERPULEMET do
    for b := W_POS_NORMAL to W_POS_DOWN do
      for c := W_ACT_NORMAL to W_ACT_FIRE do
        e_DeleteTexture(WeaponID[a][b][c]); 

  e_WriteLog('Releasing models...', MSG_NOTIFY);

  if PlayerModelsArray = nil then Exit;

  for i := 0 to High(PlayerModelsArray) do
    with PlayerModelsArray[i] do
    begin
      for a := A_STAND to A_PAIN do
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

  if (FDirection = D_LEFT) and
     (FAnim[D_LEFT][FCurrentAnimation] <> nil) and
     (FMaskAnim[D_LEFT][FCurrentAnimation] <> nil) then
  begin
    FAnim[D_LEFT][FCurrentAnimation].Reset;
    FMaskAnim[D_LEFT][FCurrentAnimation].Reset;
  end
  else
  begin
    FAnim[D_RIGHT][FCurrentAnimation].Reset;
    FMaskAnim[D_RIGHT][FCurrentAnimation].Reset;
  end;
end;

destructor TPlayerModel.Destroy();
var
  a: Byte;
begin
  for a := A_STAND to A_PAIN do
  begin
    FAnim[D_LEFT][a].Free();
    FMaskAnim[D_LEFT][a].Free();
    FAnim[D_RIGHT][a].Free();
    FMaskAnim[D_RIGHT][a].Free();
  end;

  inherited;
end;

procedure TPlayerModel.Draw(X, Y: Integer; Alpha: Byte = 0);
var
  Mirror: TMirrorType;
  pos, act: Byte;
  p: TPoint;
begin
// Флаги:
  if Direction = D_LEFT then
    Mirror := M_NONE
  else
    Mirror := M_HORIZONTAL;

  if (FFlag <> FLAG_NONE) and (FFlagAnim <> nil) and
     (not (FCurrentAnimation in [A_DIE1, A_DIE2])) then
  begin
    p.X := IfThen(Direction = D_LEFT,
                  FLAG_BASEPOINT.X,
                  64-FLAG_BASEPOINT.X);
    p.Y := FLAG_BASEPOINT.Y;

    FFlagAnim.DrawEx(X+IfThen(Direction = D_LEFT, FFlagPoint.X-1, 2*FLAG_BASEPOINT.X-FFlagPoint.X+1)-FLAG_BASEPOINT.X,
                     Y+FFlagPoint.Y-FLAG_BASEPOINT.Y+1, Mirror, p,
                     IfThen(FDirection = D_RIGHT, FFlagAngle, -FFlagAngle));
  end;

// Оружие:
  if Direction = D_RIGHT then
    Mirror := M_NONE
  else
    Mirror := M_HORIZONTAL;

  if FDrawWeapon and
    (not (FCurrentAnimation in [A_DIE1, A_DIE2, A_PAIN])) and
    (FCurrentWeapon in [WEAPON_SAW..WEAPON_SUPERPULEMET]) then
  begin
    if FCurrentAnimation in [A_SEEUP, A_ATTACKUP] then
      pos := W_POS_UP
    else
      if FCurrentAnimation in [A_SEEDOWN, A_ATTACKDOWN] then
        pos := W_POS_DOWN
      else
        pos := W_POS_NORMAL;

    if (FCurrentAnimation in [A_ATTACK, A_ATTACKUP, A_ATTACKDOWN]) or
       FFire then
      act := W_ACT_FIRE
    else
      act := W_ACT_NORMAL;

    if Alpha < 201 then
      e_Draw(WeaponID[FCurrentWeapon][pos][act],
             X+FWeaponPoints[FCurrentWeapon, FCurrentAnimation, FDirection,
                             FAnim[D_RIGHT][FCurrentAnimation].CurrentFrame].X,
             Y+FWeaponPoints[FCurrentWeapon, FCurrentAnimation, FDirection,
                             FAnim[D_RIGHT][FCurrentAnimation].CurrentFrame].Y,
             0, True, False, Mirror);
  end;

// Модель:
  if (FDirection = D_LEFT) and
     (FAnim[D_LEFT][FCurrentAnimation] <> nil) then
  begin
    FAnim[D_LEFT][FCurrentAnimation].Alpha := Alpha;
    FAnim[D_LEFT][FCurrentAnimation].Draw(X, Y, M_NONE);
  end
  else
  begin
    FAnim[D_RIGHT][FCurrentAnimation].Alpha := Alpha;
    FAnim[D_RIGHT][FCurrentAnimation].Draw(X, Y, Mirror);
  end;

// Маска модели:
  e_Colors := FColor;

  if (FDirection = D_LEFT) and
     (FMaskAnim[D_LEFT][FCurrentAnimation] <> nil) then
  begin
    FMaskAnim[D_LEFT][FCurrentAnimation].Alpha := Alpha;
    FMaskAnim[D_LEFT][FCurrentAnimation].Draw(X, Y, M_NONE);
  end
  else
  begin
    FMaskAnim[D_RIGHT][FCurrentAnimation].Alpha := Alpha;
    FMaskAnim[D_RIGHT][FCurrentAnimation].Draw(X, Y, Mirror);
  end;

  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
end;

function TPlayerModel.GetCurrentAnimation: TAnimation;
begin
  if (FDirection = D_LEFT) and (FAnim[D_LEFT][FCurrentAnimation] <> nil) then
    Result := FAnim[D_LEFT][FCurrentAnimation]
  else
    Result := FAnim[D_RIGHT][FCurrentAnimation];
end;

function TPlayerModel.GetCurrentAnimationMask: TAnimation;
begin
  if (FDirection = D_LEFT) and (FMaskAnim[D_LEFT][FCurrentAnimation] <> nil) then
    Result := FMaskAnim[D_LEFT][FCurrentAnimation]
  else
    Result := FMaskAnim[D_RIGHT][FCurrentAnimation];
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
    if (Level in [2, 3]) and (FSlopSound > 0) then
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

  if FFire then FFireCounter := FAnim[D_RIGHT, A_ATTACK].Speed*FAnim[D_RIGHT, A_ATTACK].TotalFrames
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
  if (FDirection = D_LEFT) and (FAnim[D_LEFT][FCurrentAnimation] <> nil) then
    FAnim[D_LEFT][FCurrentAnimation].Update else FAnim[D_RIGHT][FCurrentAnimation].Update;

  if (FDirection = D_LEFT) and (FMaskAnim[D_LEFT][FCurrentAnimation] <> nil) then
    FMaskAnim[D_LEFT][FCurrentAnimation].Update else FMaskAnim[D_RIGHT][FCurrentAnimation].Update;

  if FFlagAnim <> nil then FFlagAnim.Update;

  if FFireCounter > 0 then Dec(FFireCounter) else FFire := False;
end;

end.
