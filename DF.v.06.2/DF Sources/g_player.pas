unit g_player;

interface

uses windows, e_graphics, g_playermodel, g_basic, g_textures, g_weapons, g_phys,
     g_sound, g_saveload, MAPSTRUCT;

const
  KEY_LEFT       = 1;
  KEY_RIGHT      = 2;
  KEY_UP         = 3;
  KEY_DOWN       = 4;
  KEY_FIRE       = 5;
  KEY_NEXTWEAPON = 6;
  KEY_PREVWEAPON = 7;
  KEY_OPEN       = 8;
  KEY_JUMP       = 9;
  R_ITEM_BACKPACK   = 0;
  R_KEY_RED         = 1;
  R_KEY_GREEN       = 2;
  R_KEY_BLUE        = 3;
  R_BERSERK         = 4;
  MR_SUIT           = 0;
  MR_INV            = 1;
  A_BULLETS         = 0;
  A_SHELLS          = 1;
  A_ROCKETS         = 2;
  A_CELLS           = 3;
  K_SIMPLEKILL      = 0;
  K_HARDKILL        = 1;
  K_EXTRAHARDKILL   = 2;
  K_FALLKILL        = 3;
  T_RESPAWN         = 0;
  T_SWITCH          = 1;
  T_USE             = 2;
  TEAM_NONE         = 0;
  TEAM_RED          = 1;
  TEAM_BLUE         = 2;
  PLAYERNUM_1       = 1;
  PLAYERNUM_2       = 2;
  ANGLE_NONE        = Low(SmallInt);
  CORPSE_STATE_REMOVEME = 0;
  CORPSE_STATE_NORMAL   = 1;
  CORPSE_STATE_MESS     = 2;
  PLAYER_RECT: TRectWH = (X:15; Y:12; Width:33; Height:52);
  PLAYER_RECT_CX       = 15+(33 div 2);
  PLAYER_RECT_CY       = 12+(52 div 2);
  PLAYER_CORPSERECT: TRectWH = (X:15; Y:48; Width:33; Height:16);

type
  TPlayerStat = record
   Name: string;
   Team: Byte;
   Frags: SmallInt;
   Deaths: SmallInt;
   Kills: Word;
   Color: TRGB;
  end;

  TPlayerStatArray = array of TPlayerStat;

  TKeyState = record
   Pressed: Boolean;
   Time: Word;
  end;

  PPlayerSaveRec = ^TPlayerSaveRec;
  TPlayerSaveRec = packed record
   Obj: TObjRec;
   Direction: TDirection;
   UID: Word;
   Health: Integer;
   Armor: Integer;
   Air: Integer;
   Live: Boolean;
   Kills: Integer;
   MonsterKills: Integer;
   Frags: Integer;
   Death: Integer;
   Flag: Byte;
   Secrets: Integer;
   Pain: Integer;
   Ammo: array[A_BULLETS..A_CELLS] of Word;
   MaxAmmo: array[A_BULLETS..A_CELLS] of Word;
   Weapon: array[WEAPON_KASTET..WEAPON_SUPERPULEMET] of Boolean;
   Reloading: array[WEAPON_KASTET..WEAPON_SUPERPULEMET] of Word;
   Rulez: set of R_ITEM_BACKPACK..R_BERSERK;
   MegaRulez: array[MR_SUIT..MR_INV] of LongWord;
   CurrWeap: Byte;
   BFGFireCounter: SmallInt;
   DamageBuffer: Integer;
   LastSpawnerUID: Word;
   LastHit: Byte;
  end;

  TPlayer = class(TObject)
   private
    FObj:       TObj;
    FDirection: TDirection;
    FKeys:      array[KEY_LEFT..KEY_JUMP] of TKeyState;
    FUID:       Word;
    FHealth:    Integer;
    FArmor:     Integer;
    FAir:       Integer;
    FLive:      Boolean;
    FKills:     Integer;
    FMonsterKills: Integer;
    FFrags:     Integer;
    FDeath:     Integer;
    FName:      string;
    FTeam:      Byte;
    FPlayerNum: Byte;
    FFlag:      Byte;
    FSecrets:   Integer;
    FModel:     TPlayerModel;
    FPain:      Integer;
    FAmmo:      array[A_BULLETS..A_CELLS] of Word;
    FMaxAmmo:   array[A_BULLETS..A_CELLS] of Word;
    FWeapon:    array[WEAPON_KASTET..WEAPON_SUPERPULEMET] of Boolean;
    FReloading: array[WEAPON_KASTET..WEAPON_SUPERPULEMET] of Word;
    FRulez:     set of R_ITEM_BACKPACK..R_BERSERK;
    FMegaRulez: array[MR_SUIT..MR_INV] of LongWord;
    FTime:      array[T_RESPAWN..T_USE] of LongWord;
    FActionPrior:   Byte;
    FActionAnim:    Byte;
    FActionForce:   Boolean;
    FActionChanged: Boolean;
    FCurrWeap:  Byte;
    FAngle:     SmallInt;
    FFireAngle: SmallInt;
    FBFGFireCounter: SmallInt;
    FDamageBuffer:   Integer;
    FLastSpawnerUID: Word;
    FLastHit:        Byte;
    FIncCam:         Integer;
    FSawSound:     TSound;
    FSawSoundIdle: TSound;
    FSawSoundHit:  TSound;
    FSawSoundSelect: TSound;
    FGodMode:   Boolean;
    function    CollideLevel(XInc, YInc: Integer): Boolean;
    function    StayOnStep(XInc, YInc: Integer): Boolean;
    function    HeadInLiquid(XInc, YInc: Integer): Boolean;
    function    BodyInLiquid(XInc, YInc: Integer): Boolean;
    function    FullInLift(XInc, YInc: Integer): Integer;
    function    PickItem(ItemType: Byte; respawn: Boolean; var remove: Boolean): Boolean; virtual;
    procedure   CollideItem();
    procedure   FlySmoke();
    function    GetAmmoByWeapon(Weapon: Byte): Word;
    procedure   SetAction(Action: Byte; Force: Boolean = False);
    procedure   OnDamage(Angle: SmallInt); virtual;
    function    firediry(): Integer;

    procedure   Respawn(Silent: Boolean); virtual;
    procedure   Run(Direction: TDirection);
    procedure   NextWeapon();
    procedure   PrevWeapon();
    procedure   SeeUp();
    procedure   SeeDown();
    procedure   Fire();
    procedure   Jump();
    procedure   Use();
   public
    constructor Create(ModelName: string); virtual;
    destructor  Destroy(); override;
    procedure   PressKey(Key: Byte; Time: Word = 0);
    procedure   ReleaseKeys();
    procedure   SetModel(ModelName: string);
    procedure   SetColor(Color: TRGB);
    function    GetKeys(): Word;
    function    Collide(X, Y: Integer; Width, Height: Word): Boolean; overload;
    function    Collide(Rect: TRectWH): Boolean; overload;
    function    Collide(X, Y: Integer): Boolean; overload;
    procedure   SetDirection(Direction: TDirection);
    procedure   GetSecret();
    function    TeleportTo(X, Y: Integer; silent: Boolean; dir: TDirection): Boolean;
    procedure   Push(vx, vy: Integer);
    procedure   ChangeModel(ModelName: string);
    procedure   BFGHit();
    procedure   GetFlag(Flag: Byte);
    procedure   AllRulez(Health: Boolean);
    procedure   Damage(value: Word; SpawnerUID: Word; vx, vy: Integer; t: Byte); virtual;
    procedure   MakeBloodVector(Count: Word; VelX, VelY: Integer);
    procedure   MakeBloodSimple(Count: Word);
    procedure   Kill(KillType: Byte; SpawnerUID: Word; t: Byte);
    procedure   Reset(Force: Boolean);
    procedure   Draw(); virtual;
    procedure   DrawPain();
    procedure   DrawInv();
    procedure   DrawGUI();
    procedure   Update(); virtual;
    procedure   Save(Rec: PPlayerSaveRec);
    procedure   Load(Rec: PPlayerSaveRec);
    property    Name: string read FName write FName;
    property    Model: TPlayerModel read FModel;
    property    Frags: Integer read FFrags;
    property    Death: Integer read FDeath;
    property    Kills: Integer read FKills;
    property    MonsterKills: Integer read FMonsterKills write FMonsterKills;
    property    Secrets: Integer read FSecrets;
    property    GodMode: Boolean read FGodMode write FGodMode;
    property    Live: Boolean read FLive;
    property    Flag: Byte read FFlag;
    property    Team: Byte read FTeam;
    property    GameX: Integer read FObj.X;
    property    GameY: Integer read FObj.Y;
    property    Vel: TPoint2i read FObj.Vel;
    property    Obj: TObj read FObj;
    property    IncCam: Integer read FIncCam write FIncCam;
    property    UID: Word read FUID;
   end;

  TWeaponPrior = array[0..9] of Byte;
  TDifficult = record
   DiagFire: Byte;
   InvisFire: Byte;
   DiagPrecision: Byte;
   FlyPrecision: Byte;
   Cover: Byte;
   CloseJump: Byte;
   WeaponPrior: TWeaponPrior;
   CloseWeaponPrior: TWeaponPrior;
   //SafeWeaponPrior: TWeaponPrior;
  end;

  TAIFlag = record
   Name: ShortString;
   Value: ShortString;
  end;

  TBot = class(TPlayer)
   private
    FAIFlags: array of TAIFlag;
    FSelectedWeapon: Byte;
    FTargetUID: Word;
    FLastVisible: LongWord;
    FDifficult: TDifficult;
    function    GetRnd(a: Byte): Boolean;
    function    GetInterval(a: Byte; radius: SmallInt): SmallInt;
    function    RunDirection(): TDirection;
    function    FullInStep(XInc, YInc: Integer): Boolean;
    //function    NeedItem(Item: Byte): Byte;
    procedure   SelectWeapon(Dist: Integer);
    procedure   SetAIFlag(fName, fValue: ShortString);
    function    GetAIFlag(fName: ShortString): ShortString;
    procedure   RemoveAIFlag(fName: ShortString);
    function    PickItem(ItemType: Byte; force: Boolean; var remove: Boolean): Boolean; override;
    function    Healthy(): Byte;
    procedure   UpdateMove();
    procedure   UpdateCombat();
    function    KeyPressed(Key: Word): Boolean;
    procedure   ReleaseKey(Key: Byte);
    function    PlayerOnScreen(PX, PY: Integer): Boolean;
    procedure   OnDamage(Angle: SmallInt); override;
   public
    procedure   Respawn(Silent: Boolean); override;
    constructor Create(ModelName: string); override;
    destructor  Destroy(); override;
    procedure   Draw(); override;
    procedure   Update(); override;
  end;

  TGib = record
   RAngle: Integer;
   ID: DWORD;
   MaskID: DWORD;
   Color: TRGB;
   Live: Boolean;
   Obj: TObj;
  end;

  TCorpse = class(TObject)
   private
    FObj: TObj;
    FState: Byte;
    FColor: TRGB;
    FAnimation: TAnimation;
    FAnimationMask: TAnimation;
    FDamage: Byte;
    FModelName: string;
   public
    constructor Create(X, Y: Integer; ModelName: string; Mess: Boolean);
    destructor Destroy; override;
    procedure Damage(Value: Word; vx, vy: Integer);
    procedure Update();
    procedure Draw();
    property Obj: TObj read FObj;
    property State: Byte read FState;
   end;

  TTeamStat = array[TEAM_RED..TEAM_BLUE] of
  record
   Goals: SmallInt;
  end;

var
  gPlayers: array of TPlayer;
  gCorpses: array of TCorpse;
  gGibs: array of TGib;
  gTeamStat: TTeamStat;
  gFly: Boolean = False;
  MAX_RUNVEL: Integer = 8;
  VEL_JUMP: Integer = 10;

procedure g_Gibs_SetMax(Count: Word);
function g_Gibs_GetMax(): Word;
procedure g_Corpses_SetMax(Count: Word);
function g_Corpses_GetMax(): Word;

procedure g_Player_Init();
procedure g_Player_Free();
function g_Player_Create(ModelName: string; Color: TRGB; Team: Byte;
                         Bot: Boolean; PlayerNum: Byte): Word;
procedure g_Player_Remove(UID: Word);
procedure g_Player_UpdateAll();
procedure g_Player_DrawAll();
procedure g_Player_ResetAll(Force, Silent: Boolean);
function g_Player_Get(UID: Word): TPlayer;
function g_Player_GetCount(): Byte;
function g_Player_GetStats(): TPlayerStatArray;
function g_Player_ValidName(Name: string): Boolean;
procedure g_Player_CreateCorpse(Player: TPlayer);
procedure g_Player_CreateGibs(fX, fY: Integer; ModelName: string; fColor: TRGB);
procedure g_Player_UpdateCorpse();
procedure g_Player_DrawCorpses();
procedure g_Player_RemoveAllCorpses();
procedure g_Bot_Add(Team, Difficult: Byte);
procedure g_Bot_AddList(Team: Byte; lname: ShortString; num: Integer = -1);
procedure g_Bot_RemoveAll();

implementation

uses g_map, g_items, g_console, SysUtils, inter, g_gfx,
  Math, g_options, g_triggers, g_menu, MAPDEF, g_game, WADEDITOR, g_main,
  g_monsters, CONFIG;

type
  TBotProfile = record
   name: ShortString;
   model: ShortString;
   team: Byte;
   color: TRGB;
   diag_fire: Byte;
   invis_fire: Byte;
   diag_precision: Byte;
   fly_precision: Byte;
   cover: Byte;
   close_jump: Byte;
   w_prior1: TWeaponPrior;
   w_prior2: TWeaponPrior;
   w_prior3: TWeaponPrior;
  end;

const
  TIME_RESPAWN1 = 1500;
  TIME_RESPAWN2 = 2000;
  TIME_RESPAWN3 = 3000;
  AIR     = 360;
  AIR_MAX = 1091;
  PLAYER_SUIT_TIME    = 30000;
  PLAYER_INV_TIME     = 30000;
  VEL_SW  = 4;
  VEL_FLY = 4;
  ANGLE_RIGHTUP   = 55;
  ANGLE_RIGHTDOWN = -35;
  ANGLE_LEFTUP    = 125;      
  ANGLE_LEFTDOWN  = -145;
  PLAYER_HEADRECT: TRectWH = (X:36; Y:16; Width:8; Height:8);
  WEAPONPOINT: array[TDirection] of TPoint = ((X:16; Y:32), (X:47; Y:32));
  BOT_MAXJUMP = 84;
  BOT_LONGDIST   = 300;
  BOT_UNSAFEDIST = 128;
  TEAMCOLOR: array[TEAM_RED..TEAM_BLUE] of TRGB = ((R:255; G:0; B:0),
                                                   (R:0; G:0; B:255));
  DIFFICULT_EASY: TDifficult = (DiagFire: 32; InvisFire: 32; DiagPrecision: 32;
                                FlyPrecision: 32; Cover: 32; CloseJump: 32);
  DIFFICULT_MEDIUM: TDifficult = (DiagFire: 127; InvisFire: 127; DiagPrecision: 127;
                                  FlyPrecision: 127; Cover: 127; CloseJump: 127);
  DIFFICULT_HARD: TDifficult = (DiagFire: 255; InvisFire: 255; DiagPrecision: 255;
                                FlyPrecision: 255; Cover: 255; CloseJump: 255);
  WEAPON_PRIOR1: TWeaponPrior = (WEAPON_SUPERPULEMET, WEAPON_SHOTGUN2, WEAPON_SHOTGUN1,
                                 WEAPON_CHAINGUN, WEAPON_PLASMA, WEAPON_ROCKETLAUNCHER,
                                 WEAPON_BFG, WEAPON_PISTOL, WEAPON_SAW, WEAPON_KASTET);
  WEAPON_PRIOR2: TWeaponPrior = (WEAPON_SUPERPULEMET, WEAPON_BFG, WEAPON_ROCKETLAUNCHER,
                                 WEAPON_SHOTGUN2, WEAPON_PLASMA, WEAPON_SHOTGUN1,
                                 WEAPON_CHAINGUN, WEAPON_PISTOL, WEAPON_SAW, WEAPON_KASTET);
  //WEAPON_PRIOR3: TWeaponPrior = (WEAPON_SUPERPULEMET, WEAPON_BFG, WEAPON_PLASMA,
  //                               WEAPON_SHOTGUN2, WEAPON_CHAINGUN, WEAPON_SHOTGUN1,
  //                               WEAPON_SAW, WEAPON_ROCKETLAUNCHER, WEAPON_PISTOL, WEAPON_KASTET);

var
  MaxGibs: Word = 150;
  MaxCorpses: Word = 20;
  CurrentGib: Integer = 0;
  BotNames: array of string;
  BotList: array of TBotProfile;

function SameTeam(UID1, UID2: Word): Boolean;
begin
 Result := False;

 if (UID1 > UID_MAX_PLAYER) or (UID1 <= UID_MAX_GAME) or
    (UID2 > UID_MAX_PLAYER) or (UID2 <= UID_MAX_GAME) then Exit;

 if (g_Player_Get(UID1) = nil) or (g_Player_Get(UID2) = nil) then Exit;

 if ((g_Player_Get(UID1).Team = TEAM_NONE) or
     (g_Player_Get(UID2).Team = TEAM_NONE)) then Exit;

 Result := g_Player_Get(UID1).FTeam = g_Player_Get(UID2).FTeam;
end;

procedure g_Gibs_SetMax(Count: Word);
begin
 MaxGibs := Count;
 SetLength(gGibs, Count);

 if CurrentGib >= Count-1 then CurrentGib := 0;
end;

function g_Gibs_GetMax(): Word;
begin
 Result := MaxGibs;
end;

procedure g_Corpses_SetMax(Count: Word);
begin
 MaxCorpses := Count;
 SetLength(gCorpses, Count);
end;

function g_Corpses_GetMax(): Word;
begin
 Result := MaxCorpses;
end;

function g_Player_Create(ModelName: string; Color: TRGB; Team: Byte;
                         Bot: Boolean; PlayerNum: Byte): Word;
var
  a: Integer;
  ok: Boolean;
begin
 Result := 0;

 ok := False;
 a := 0;
 
 if gPlayers <> nil then
  for a := 0 to High(gPlayers) do
   if gPlayers[a] = nil then
   begin
    ok := True;
    Break;
   end;

 if not ok then
 begin
  SetLength(gPlayers, Length(gPlayers)+1);
  a := High(gPlayers);
 end;

 if Bot then gPlayers[a] := TBot.Create(ModelName)
  else gPlayers[a] := TPlayer.Create(ModelName);

 if gPlayers[a].FModel = nil then
 begin
  if Bot then gPlayers[a].Destroy;
  gPlayers[a] := nil;
  g_Console_Add(Format(I_GAME_MODELERROR, [ModelName]));
  Exit;
 end;

 if (gGameSettings.GameMode = GM_CTF) or (gGameSettings.GameMode = GM_TDM) then
  gPlayers[a].FModel.Color := TEAMCOLOR[Team] else gPlayers[a].FModel.Color := Color;

 gPlayers[a].FTeam := Team;
 gPlayers[a].FUID := g_CreateUID(UID_PLAYER);
 gPlayers[a].FPlayerNum := PlayerNum;
 gPlayers[a].FLive := False;

 Result := gPlayers[a].FUID;
end;

procedure g_Bot_Add(Team, Difficult: Byte);
var
  m: SArray;
  _name, _model: string;
  a: Integer;
begin
 if gGameSettings.GameType = GT_SINGLE then Exit;
 if gGameSettings.GameMode = GM_CTF then Exit;

 m := g_PlayerModel_GetNames();
 if m = nil then Exit;

 if gGameSettings.GameMode = GM_DM then Team := TEAM_NONE
  else if Team = TEAM_NONE then
   if Random(2) = 0 then team := TEAM_RED else team := TEAM_BLUE;

 _name := '';
 if BotNames <> nil then
  for a := 0 to High(BotNames) do
   if g_Player_ValidName(BotNames[a]) then
   begin
    _name := BotNames[a];
    Break;
   end;

 if _name = '' then
 repeat
  _name := Format('DFBOT%.2d', [Random(100)]);
 until g_Player_ValidName(_name);

 _model := m[Random(Length(m))];

 with g_Player_Get(g_Player_Create(_model, _RGB(Min(Random(9)*32, 255),
                                   Min(Random(9)*32, 255), Min(Random(9)*32, 255)),
                                   Team, True, 0)) as TBot do
 begin
  Name := _name;

  case Difficult of
   1: FDifficult := DIFFICULT_EASY;
   2: FDifficult := DIFFICULT_MEDIUM;
   else FDifficult := DIFFICULT_HARD;
  end;
  
  FDifficult.WeaponPrior := WEAPON_PRIOR1;
  FDifficult.CloseWeaponPrior := WEAPON_PRIOR2;
 end;
end;

procedure g_Bot_AddList(Team: Byte; lname: ShortString; num: Integer = -1);
var
  m: SArray;
  _name, _model: string;
  a: Integer;
begin
 if gGameSettings.GameType = GT_SINGLE then Exit;
 if gGameSettings.GameMode = GM_CTF then Exit;

 m := g_PlayerModel_GetNames();
 if m = nil then Exit;

 if gGameSettings.GameMode = GM_DM then Team := TEAM_NONE
  else if Team = TEAM_NONE then Team := BotList[num].team;

 lname := AnsiLowerCase(lname);
 if (num < 0) or (num > Length(BotList)-1) then num := -1;
 if (num = -1) and (lname <> '') and (BotList <> nil) then
  for a := 0 to High(BotList) do
   if AnsiLowerCase(BotList[a].name) = lname then
   begin
    num := a;
    Break;
   end;

 if num = -1 then Exit;

 _name := BotList[num].name;
 if not g_Player_ValidName(_name) then
 repeat
  _name := Format('DFBOT%.2d', [Random(100)]);
 until g_Player_ValidName(_name);

 _model := BotList[num].model;
 if not InSArray(_model, m) then
  _model := m[Random(Length(m))];

 with g_Player_Get(g_Player_Create(_model, BotList[num].color, Team, True, 0)) as TBot do
 begin
  Name := _name;

  FDifficult.DiagFire := BotList[num].diag_fire;
  FDifficult.InvisFire := BotList[num].invis_fire;
  FDifficult.DiagPrecision := BotList[num].diag_precision;
  FDifficult.FlyPrecision := BotList[num].fly_precision;
  FDifficult.Cover := BotList[num].cover;
  FDifficult.CloseJump := BotList[num].close_jump;

  FDifficult.WeaponPrior := BotList[num].w_prior1;
  FDifficult.CloseWeaponPrior := BotList[num].w_prior2;
  //FDifficult.SafeWeaponPrior := BotList[num].w_prior3;
 end;
end;

procedure g_Bot_RemoveAll();
var
  a: Integer;
begin
 if gPlayers = nil then Exit;

 for a := 0 to High(gPlayers) do
  if gPlayers[a] <> nil then
   if gPlayers[a] is TBot then g_Player_Remove(gPlayers[a].FUID);
end;

procedure g_Player_Remove(UID: Word);
var
  i: Integer;
begin
 if gPlayers = nil then Exit;

 for i := 0 to High(gPlayers) do
  if gPlayers[i] <> nil then
   if gPlayers[i].FUID = UID then
   begin
    if gPlayers[i] is TPlayer then gPlayers[i].Destroy()
     else TBot(gPlayers[i]).Destroy();
    gPlayers[i] := nil;
    Exit;
   end;
end;

procedure g_Player_Init();
var
  F: TextFile;
  s: string;
  a, b: Integer;
  config: TConfig;
  sa: SArray;
begin
 BotNames := nil;

 if not FileExists(DataDir+'botnames.txt') then Exit; 

 AssignFile(F, DataDir+'botnames.txt');
 Reset(F);
 while not EOF(F) do
 begin
  ReadLn(F, s);

  s := Trim(s);
  if s = '' then Continue;

  SetLength(BotNames, Length(BotNames)+1);
  BotNames[High(BotNames)] := s;
 end;
 CloseFile(F);

 if BotNames <> nil then
  for a := 0 to High(BotNames) do
  begin
   b := Random(Length(BotNames));
   s := BotNames[a];
   Botnames[a] := BotNames[b];
   BotNames[b] := s;
  end;

 BotList := nil;
 a := 0;

 config := TConfig.CreateFile(DataDir+'botlist.txt');
 while config.SectionExists(IntToStr(a)) do
 begin
  SetLength(BotList, Length(BotList)+1);
  with BotList[High(BotList)] do
  begin
   name := config.ReadStr(IntToStr(a), 'name', '');
   model := config.ReadStr(IntToStr(a), 'model', '');
   if config.ReadStr(IntToStr(a), 'team', 'red') = 'red' then
    team := TEAM_RED else team := TEAM_BLUE;
   sa := parse(config.ReadStr(IntToStr(a), 'color', ''));
   color.R := StrToIntDef(sa[0], 0);
   color.G := StrToIntDef(sa[1], 0);
   color.B := StrToIntDef(sa[2], 0);
   diag_fire := config.ReadInt(IntToStr(a), 'diag_fire', 0);
   invis_fire := config.ReadInt(IntToStr(a), 'invis_fire', 0);
   diag_precision := config.ReadInt(IntToStr(a), 'diag_precision', 0);
   fly_precision := config.ReadInt(IntToStr(a), 'fly_precision', 0);
   cover := config.ReadInt(IntToStr(a), 'cover', 0);
   close_jump := config.ReadInt(IntToStr(a), 'close_jump', 0);

   sa := parse(config.ReadStr(IntToStr(a), 'w_prior1', ''));
   if Length(sa) = 10 then
    for b := 0 to 9 do
     w_prior1[b] := EnsureRange(StrToInt(sa[b]), 0, 9);

   sa := parse(config.ReadStr(IntToStr(a), 'w_prior2', ''));
   if Length(sa) = 10 then
    for b := 0 to 9 do
     w_prior2[b] := EnsureRange(StrToInt(sa[b]), 0, 9);

   {sa := parse(config.ReadStr(IntToStr(a), 'w_prior3', ''));
   if Length(sa) = 10 then
    for b := 0 to 9 do
     w_prior3[b] := EnsureRange(StrToInt(sa[b]), 0, 9);}
  end;
  a := a+1;
 end;
 
 config.Destroy();
end;

procedure g_Player_Free();
var
  i: Integer;
begin
 if gPlayers <> nil then
 begin
  for i := 0 to High(gPlayers) do
   if gPlayers[i] <> nil then
   begin
    if gPlayers[i] is TPlayer then gPlayers[i].Destroy()
     else TBot(gPlayers[i]).Destroy();
    gPlayers[i] := nil;
   end;

  gPlayers := nil;
 end;

 gPlayer1 := nil;
 gPlayer2 := nil;
end;

procedure g_Player_UpdateAll();
var
  i: Integer;
begin
 if gPlayers = nil then Exit;

 for i := 0 to High(gPlayers) do
  if gPlayers[i] <> nil then
   if gPlayers[i] is TPlayer then gPlayers[i].Update()
    else TBot(gPlayers[i]).Update();
end;

procedure g_Player_DrawAll();
var
  i: Integer;
begin
 if gPlayers = nil then Exit;

 for i := 0 to High(gPlayers) do
  if gPlayers[i] <> nil then
   if gPlayers[i] is TPlayer then gPlayers[i].Draw()
    else TBot(gPlayers[i]).Draw();
end;

function g_Player_Get(UID: Word): TPlayer;
var
  a: Integer;
begin
 Result := nil;

 if gPlayers = nil then Exit;

 for a := 0 to High(gPlayers) do
  if gPlayers[a] <> nil then
   if gPlayers[a].FUID = UID then
   begin
    Result := gPlayers[a];
    Exit;
   end;
end;

function g_Player_GetCount(): Byte;
var
  a: Integer;
begin
 Result := 0;

 if gPlayers = nil then Exit;

 for a := 0 to High(gPlayers) do
  if gPlayers[a] <> nil then Result := Result+1;
end;

function g_Player_GetStats(): TPlayerStatArray;
var
  a: Integer;
begin
 Result := nil;

 if gPlayers = nil then Exit;

 for a := 0 to High(gPlayers) do
  if gPlayers[a] <> nil then
  begin
   SetLength(Result, Length(Result)+1);
   with Result[High(Result)] do
   begin
    Name := gPlayers[a].FName;
    Team := gPlayers[a].FTeam;
    Frags := gPlayers[a].FFrags;
    Deaths := gPlayers[a].FDeath;
    Kills := gPlayers[a].FKills;
    Color := gPlayers[a].FModel.Color;
   end;
  end;
end;

procedure g_Player_ResetAll(Force, Silent: Boolean);
var
  i: Integer;
begin
 gTeamStat[TEAM_RED].Goals := 0;
 gTeamStat[TEAM_BLUE].Goals := 0;

 if gPlayers <> nil then
 for i := 0 to High(gPlayers) do
  if gPlayers[i] <> nil then
  begin
   gPlayers[i].Reset(Force);
   if gPlayers[i] is TPlayer then gPlayers[i].Respawn(Silent)
     else TBot(gPlayers[i]).Respawn(Silent);
  end;
end;

{ TPlayer }

procedure TPlayer.BFGHit();
begin
 g_Weapon_BFGHit(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                 FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
end;

procedure TPlayer.ChangeModel(ModelName: string);
var
  Model: TPlayerModel;
begin
 Model := g_PlayerModel_Get(ModelName);
 if Model = nil then Exit;

 if FModel <> nil then FModel.Destroy;
 FModel := Model;
end;

procedure TPlayer.CollideItem();
var
  i: Integer;
  r: Boolean;
begin
 if gItems = nil then Exit;
 if not FLive then Exit;

 for i := 0 to High(gItems) do
  with gItems[i] do
  begin
   if (ItemType <> ITEM_NONE) and Live then
    if g_Obj_Collide(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width,
                     PLAYER_RECT.Height, @Obj) then
   begin
    if not PickItem(ItemType, gItems[i].Respawnable, r) then Continue;

    if ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_INV] then
     g_Sound_PlayExAt('SOUND_ITEM_GETRULEZ', 255, FObj.X, FObj.Y)
    else if ItemType in [ITEM_MEDKIT_SMALL, ITEM_MEDKIT_LARGE, ITEM_MEDKIT_BLACK] then
     g_Sound_PlayExAt('SOUND_ITEM_GETMED', 255, FObj.X, FObj.Y)
    else g_Sound_PlayExAt('SOUND_ITEM_GETITEM', 255, FObj.X, FObj.Y);

    if r and not ((ItemType in [ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE]) and
                  (gGameSettings.GameType = GT_SINGLE) and
                  LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER)) then
     if not Respawnable then g_Items_Remove(i) else g_Items_Pick(i);
   end;
  end;
end;

function TPlayer.CollideLevel(XInc, YInc: Integer): Boolean;
begin
 Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                              PLAYER_RECT.Width, PLAYER_RECT.Height, PANEL_WALL);
end;

constructor TPlayer.Create(ModelName: string);
var
  ID: DWORD;
begin
 FSawSound := TSound.Create();
 if g_Sound_Get(ID, 'SOUND_WEAPON_FIRESAW') then FSawSound.SetID(ID);

 FSawSoundIdle := TSound.Create();
 if g_Sound_Get(ID, 'SOUND_WEAPON_IDLESAW') then FSawSoundIdle.SetID(ID);

 FSawSoundHit := TSound.Create();
 if g_Sound_Get(ID, 'SOUND_WEAPON_HITSAW') then FSawSoundHit.SetID(ID);

 FSawSoundSelect := TSound.Create();
 if g_Sound_Get(ID, 'SOUND_WEAPON_SELECTSAW') then FSawSoundSelect.SetID(ID);

 SetModel(ModelName);

 g_Obj_Init(@FObj);
 FObj.Rect := PLAYER_RECT;
end;

procedure TPlayer.Damage(value: Word; SpawnerUID: Word; vx, vy: Integer; t: Byte);
var
  c: Word;
begin
 if not FLive then Exit;
 FLastSpawnerUID := SpawnerUID;

 if (t = HIT_TRAP) and (t = FLastHit) then Exit;

 FLastHit := t;

 if gBloodCount > 0 then
 begin
  c := Min(value, 200)*gBloodCount-(value div 4)+Random(Min(value, 200) div 2);

  if (t = HIT_SOME) and (vx = 0) and (vy = 0) then MakeBloodSimple(c) else
  case t of
   HIT_TRAP, HIT_ACID, HIT_FLAME: MakeBloodSimple(c);
   HIT_BFG, HIT_ROCKET, HIT_SOME: MakeBloodVector(c, vx, vy);
  end;

  if t = HIT_WATER then g_GFX_Bubbles(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                                      FObj.Y+PLAYER_RECT.Y-4, value div 2, 8, 4);
 end;

 if t = HIT_TRAP then FMegaRulez[MR_INV] := 0;

 if FMegaRulez[MR_INV] > gTime then Exit;

 if not FGodMode then
 begin
  if LongBool(gGameSettings.Options and GAME_OPTION_TEAMDAMAGE) or
     (SpawnerUID = FUID) or not SameTeam(FUID, SpawnerUID) then Inc(FDamageBuffer, value);
  if gFlash then FPain := FPain+value;
 end;
end;

destructor TPlayer.Destroy();
begin
 if FSawSound <> nil then FSawSound.Destroy();
 if FSawSoundIdle <> nil then FSawSoundIdle.Destroy();
 if FSawSoundHit <> nil then FSawSoundHit.Destroy();
 if FModel <> nil then FModel.Destroy();

 inherited;
end;

procedure TPlayer.Draw();
begin
 if not Live then Exit;

 FModel.Draw(FObj.X, FObj.Y);

 //e_DrawQuad(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y,
 //           FObj.X+PLAYER_RECT.X+PLAYER_RECT.Width,
 //           FObj.Y+PLAYER_RECT.Y+PLAYER_RECT.Height, 1, 255, 0, 0);
end;

procedure TPlayer.DrawGUI();
var
  ID: DWORD;
  X, Y, a, p, m: Integer;
  tw, th: Word;
  s: string;
  stat: TPlayerStatArray;
begin
 X := gPlayerScreenSize.X;
 Y := 0;

 if gShowGoals and (gGameSettings.GameMode in [GM_CTF, GM_TDM]) then
 begin
  s := IntToStr(gTeamStat[TEAM_RED].Goals);
  e_CharFont_GetSize(gMenuFont, s, tw, th);
  e_CharFont_PrintEx(gMenuFont, X-16-tw, 240-72, s, _RGB(255, 0, 0));

  if gGameSettings.GameMode = GM_CTF then
  begin
   if g_Texture_Get('TEXTURE_PLAYER_REDFLAG', ID) then
    e_Draw(ID, X-16-tw-40, 240-72-4, 0, True, False);
  end;

  s := IntToStr(gTeamStat[TEAM_BLUE].Goals);
  e_CharFont_GetSize(gMenuFont, s, tw, th);
  e_CharFont_PrintEx(gMenuFont, X-16-tw, 240-32, s, _RGB(0, 0, 255));

  if gGameSettings.GameMode = GM_CTF then
  begin
   if g_Texture_Get('TEXTURE_PLAYER_BLUEFLAG', ID) then
    e_Draw(ID,  X-16-tw-40, 240-32-4, 0, True, False);
  end;
 end;

 if g_Texture_Get('TEXTURE_PLAYER_HUDBG', ID) then
  e_DrawFill(ID, X, 0, 1, (gPlayerScreenSize.Y div 256)+IfThen(gPlayerScreenSize.Y mod 256 > 0, 1, 0),
             0, False, False);

 if g_Texture_Get('TEXTURE_PLAYER_HUD', ID) then
  e_Draw(ID, X+2, Y, 0, True, False);

 if gGameSettings.GameType = GT_CUSTOM then
 begin
  if gShowStat then
  begin
   s := IntToStr(Frags);
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
     if stat[a].Name <> Name then
     begin
      if stat[a].Frags > m then m := stat[a].Frags;
      if stat[a].Frags > Frags then p := p+1;
     end;
   end;

   s := IntToStr(p)+'\'+IntToStr(Length(stat))+' ';
   if Frags >= m then s := s+'+' else s := s+'-';
   s := s+IntToStr(Abs(Frags-m));

   e_CharFont_GetSize(gMenuSmallFont, s, tw, th);
   e_CharFont_PrintEx(gMenuSmallFont, X-16-tw, Y+32, s, _RGB(255, 0, 0));
  end;

  e_CharFont_GetSize(gMenuSmallFont, FName, tw, th);
  e_CharFont_PrintEx(gMenuSmallFont, X+98-(tw div 2), Y+8, FName, _RGB(255, 0, 0));
 end;

 e_Draw(gItemsTexturesID[ITEM_MEDKIT_LARGE], X+37, Y+45, 0, True, False);

 if g_Texture_Get('TEXTURE_PLAYER_ARMORHUD', ID) then
  e_Draw(ID, X+36, Y+77, 0, True, False);

 s := IntToStr(IfThen(FHealth > 0, FHealth, 0));
 e_CharFont_GetSize(gMenuFont, s, tw, th);
 e_CharFont_PrintEx(gMenuFont, X+178-tw, Y+40, s, _RGB(255, 0, 0));

 s := IntToStr(FArmor);
 e_CharFont_GetSize(gMenuFont, s, tw, th);
 e_CharFont_PrintEx(gMenuFont, X+178-tw, Y+68, s, _RGB(255, 0, 0));

 s := IntToStr(GetAmmoByWeapon(FCurrWeap));
 e_CharFont_GetSize(gMenuFont, s, tw, th);
 e_CharFont_PrintEx(gMenuFont, X+178-tw, Y+158, s,  _RGB(255, 0, 0));

 case FCurrWeap of
  WEAPON_KASTET: ID := gItemsTexturesID[ITEM_WEAPON_KASTET];
  WEAPON_SAW: ID := gItemsTexturesID[ITEM_WEAPON_SAW];
  WEAPON_PISTOL: ID := gItemsTexturesID[ITEM_WEAPON_PISTOL];
  WEAPON_CHAINGUN: ID := gItemsTexturesID[ITEM_WEAPON_CHAINGUN];
  WEAPON_SHOTGUN1: ID := gItemsTexturesID[ITEM_WEAPON_SHOTGUN1];
  WEAPON_SHOTGUN2: ID := gItemsTexturesID[ITEM_WEAPON_SHOTGUN2];
  WEAPON_SUPERPULEMET: ID := gItemsTexturesID[ITEM_WEAPON_SUPERPULEMET];
  WEAPON_ROCKETLAUNCHER: ID := gItemsTexturesID[ITEM_WEAPON_ROCKETLAUNCHER];
  WEAPON_PLASMA: ID := gItemsTexturesID[ITEM_WEAPON_PLASMA];
  WEAPON_BFG: ID := gItemsTexturesID[ITEM_WEAPON_BFG];
 end;

 e_Draw(ID, X+20, Y+160, 0, True, False);

 if R_KEY_RED in FRulez then
  e_Draw(gItemsTexturesID[ITEM_KEY_RED], X+78, Y+214, 0, True, False);

 if R_KEY_GREEN in FRulez then
  e_Draw(gItemsTexturesID[ITEM_KEY_GREEN], X+95, Y+214, 0, True, False);

 if R_KEY_BLUE in FRulez then
  e_Draw(gItemsTexturesID[ITEM_KEY_BLUE], X+112, Y+214, 0, True, False);

 e_DrawLine(4, X+16, Y+129, X+16+Trunc(169*IfThen(FAir > 0, FAir, 0)/AIR_MAX), Y+129, 0, 0, 255);
end;

procedure TPlayer.DrawInv();
begin
 if FMegaRulez[MR_INV] < gTime then Exit;

 if FMegaRulez[MR_INV]-gTime <= 300 then
  e_DrawFillQuad(0, 0, gPlayerScreenSize.X, gPlayerScreenSize.Y, 200, 200, 200,
                 90+Round((300-(FMegaRulez[MR_INV]-gTime))*0.55), B_BLEND)
 else e_DrawFillQuad(0, 0, gPlayerScreenSize.X, gPlayerScreenSize.Y, 200, 200, 200, 90, B_BLEND);
end;

procedure TPlayer.DrawPain();
var
  a, h: Integer;
begin
 if FPain = 0 then Exit;

 a := FPain;

 if a < 15 then h := 0
 else if a < 35 then h := 1
 else if a < 55 then h := 2
 else if a < 75 then h := 3
 else if a < 95 then h := 4
 else h := 5;

 //if a > 255 then a := 255;

 e_DrawFillQuad(0, 0, gPlayerScreenSize.X, gPlayerScreenSize.Y, 255, 0, 0, 255-h*50);
 //e_DrawFillQuad(0, 0, gPlayerScreenSize.X, gPlayerScreenSize.Y, 255-min(128, a), 255-a, 255-a, 0, B_FILTER);
end;

procedure TPlayer.Fire();
const
  ft: array[WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte = (5, 2, 6, 18, 36, 2, 12, 2, 0, 2);
var
  f: Boolean;
  wx, wy, xd, yd: Integer;
  obj: TObj;
begin
 if FReloading[FCurrWeap] <> 0 then Exit;

 f := False;
 wx := FObj.X+WEAPONPOINT[FDirection].X;
 wy := FObj.Y+WEAPONPOINT[FDirection].Y;
 xd := wx+IfThen(FDirection = D_LEFT, -30, 30);
 yd := wy+firediry();

 case FCurrWeap of
  WEAPON_KASTET:
  begin
   if R_BERSERK in FRulez then
   begin
    //g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 75, FUID);
    obj.X := FObj.X+FObj.Rect.X;
    obj.Y := FObj.Y+FObj.Rect.Y;
    obj.rect.X := 0;
    obj.rect.Y := 0;
    obj.rect.Width := 39;
    obj.rect.Height := 52;
    obj.Vel := _Point((xd-wx) div 2, (yd-wy) div 2);
    obj.Accel := _Point((xd-wx), (yd-wy));

    if g_Weapon_Hit(@obj, 50, FUID, HIT_SOME) <> 1 then
     g_Sound_PlayExAt('SOUND_WEAPON_HITPUNCH', 255, FObj.X, FObj.Y);

    Inc(FPain, 25);
   end else g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 3, FUID);

   FReloading[FCurrWeap] := ft[FCurrWeap];
  end;

  WEAPON_SAW:
  begin
   if g_Weapon_chainsaw(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                        IfThen(gGameSettings.GameType = GT_CUSTOM, 9, 3), FUID) <> 0 then
   begin
    FSawSoundSelect.Stop();
    FSawSound.Stop();
    FSawSoundHit.Play(127, 255);
   end
    else if not FSawSoundHit.IsPlaying() then
   begin
    FSawSoundSelect.Stop();
    FSawSound.Play(127, 255);
   end;

   FReloading[FCurrWeap] := ft[FCurrWeap];
   f := True;
  end;

  WEAPON_PISTOL:
  if FAmmo[A_BULLETS] > 0 then
  begin
   g_Weapon_pistol(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_BULLETS]);
   FFireAngle := FAngle;
   f := True;
  end;

  WEAPON_SHOTGUN1:
  if FAmmo[A_SHELLS] > 0 then
  begin
   g_Weapon_shotgun(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_SHELLS]);
   FFireAngle := FAngle;
   f := True;
  end;

  WEAPON_SHOTGUN2:
  if FAmmo[A_SHELLS] >= 2 then
  begin
   g_Weapon_dshotgun(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_SHELLS], 2);
   FFireAngle := FAngle;
   f := True;
  end;

  WEAPON_CHAINGUN:
  if FAmmo[A_BULLETS] > 0 then
  begin
   g_Weapon_mgun(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_BULLETS]);
   FFireAngle := FAngle;
   f := True;
  end;

  WEAPON_ROCKETLAUNCHER:
  if FAmmo[A_ROCKETS] > 0 then
  begin
   g_Weapon_rocket(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_ROCKETS]);
   FFireAngle := FAngle;
   f := True;
  end;

  WEAPON_PLASMA:
  if FAmmo[A_CELLS] > 0 then
  begin
   g_Weapon_plasma(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_CELLS]);
   FFireAngle := FAngle;
   f := True;
  end;

  WEAPON_BFG:
  if (FAmmo[A_CELLS] >= 40) and (FBFGFireCounter = -1) then
  begin
   FBFGFireCounter := 21;
   g_Sound_PlayExAt('SOUND_WEAPON_STARTFIREBFG', 255, FObj.X, FObj.Y);
   Dec(FAmmo[A_CELLS], 40);
  end;

  WEAPON_SUPERPULEMET:
  if FAmmo[A_SHELLS] > 0 then
  begin
   g_Weapon_shotgun(wx, wy, xd, yd, FUID);
   FReloading[FCurrWeap] := ft[FCurrWeap];
   Dec(FAmmo[A_SHELLS]);
   FFireAngle := FAngle;
   f := True;
  end;
 end;

 if not f then Exit;

 if (FAngle = 0) or (FAngle = 180) then SetAction(A_ATTACK)
  else if (FAngle = ANGLE_LEFTDOWN) or (FAngle = ANGLE_RIGHTDOWN) then SetAction(A_ATTACKDOWN)
   else if (FAngle = ANGLE_LEFTUP) or (FAngle = ANGLE_RIGHTUP) then SetAction(A_ATTACKUP);
end;

function TPlayer.GetAmmoByWeapon(Weapon: Byte): Word;
begin
 case Weapon of
  WEAPON_PISTOL, WEAPON_CHAINGUN: Result := FAmmo[A_BULLETS];
  WEAPON_SHOTGUN1, WEAPON_SHOTGUN2, WEAPON_SUPERPULEMET: Result := FAmmo[A_SHELLS];
  WEAPON_ROCKETLAUNCHER: Result := FAmmo[A_ROCKETS];
  WEAPON_PLASMA, WEAPON_BFG: Result := FAmmo[A_CELLS];
  else Result := 0;
 end;
end;

function TPlayer.HeadInLiquid(XInc, YInc: Integer): Boolean;
begin
 Result := g_Map_CollidePanel(FObj.X+PLAYER_HEADRECT.X+XInc, FObj.Y+PLAYER_HEADRECT.Y+YInc,
                              PLAYER_HEADRECT.Width, PLAYER_HEADRECT.Height,
                              PANEL_WATER or PANEL_ACID1 or PANEL_ACID2);
end;

procedure TPlayer.Jump();
begin
 if gFly then
 begin
  FObj.Vel.Y := -VEL_FLY;
  Exit;
 end;

 if CollideLevel(0, 1) or
    g_Map_CollidePanel(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y+36, PLAYER_RECT.Width,
                       PLAYER_RECT.Height-33, PANEL_STEP) then FObj.Vel.Y := -VEL_JUMP
 else
  if BodyInLiquid(0, 0) then FObj.Vel.Y := -VEL_SW;
end;

procedure TPlayer.Kill(KillType: Byte; SpawnerUID: Word; t: Byte);
var
  a: Byte;
  b: DWORD;
  s: string;
begin
 FDeath := FDeath+1;
 FLive := False;

 a := 1;
 case KillType of
  K_SIMPLEKILL: a := 1;
  K_HARDKILL: a := 2;
  K_EXTRAHARDKILL: a := 3;
  K_FALLKILL: a := 4;
 end;

 if not FModel.PlaySound(MODELSOUND_DIE, a, FObj.X, FObj.Y) then
  for b := 1 to 3 do
   if FModel.PlaySound(MODELSOUND_DIE, b, FObj.X, FObj.Y) then Break;

 case KillType of
  K_SIMPLEKILL: FTime[T_RESPAWN] := gTime+TIME_RESPAWN1;
  K_HARDKILL: FTime[T_RESPAWN] := gTime+TIME_RESPAWN2;
  K_EXTRAHARDKILL,
  K_FALLKILL: FTime[T_RESPAWN] := gTime+TIME_RESPAWN3;
 end;

 case KillType of
  K_SIMPLEKILL: SetAction(A_DIE1);
  K_HARDKILL,
  K_EXTRAHARDKILL: SetAction(A_DIE2);
 end;

 if KillType <> K_FALLKILL then g_Monsters_killedp();

 if not (gGameSettings.GameType = GT_SINGLE) then
 begin
  case t of
   HIT_FALL: g_Console_Add(Format(I_PLAYER_FALLKILL, [FName]), True);
   HIT_WATER: g_Console_Add(Format(I_PLAYER_WATERKILL, [FName]), True);
   HIT_ACID: g_Console_Add(Format(I_PLAYER_ACIDKILL, [FName]), True);
   HIT_TRAP: g_Console_Add(Format(I_PLAYER_TRAPKILL, [FName]), True);
  end;

  if SpawnerUID = FUID then
  begin
   FFrags := FFrags-1;
   g_Console_Add(Format(I_PLAYER_SELFKILL, [FName]), True);
  end
   else if g_GetUIDType(SpawnerUID) = UID_PLAYER then
  begin
   Inc(g_Player_Get(SpawnerUID).FFrags, IfThen(SameTeam(FUID, SpawnerUID), -1, 1));

   if gGameSettings.GameMode = GM_TDM then
    Inc(gTeamStat[g_Player_Get(SpawnerUID).Team].Goals, IfThen(SameTeam(FUID, SpawnerUID), -1, 1));

   if KillType = K_EXTRAHARDKILL then
    case Random(2) of
     0: g_Console_Add(Format(I_PLAYER_EXTRAHARDKILL1, [FName, g_Player_Get(SpawnerUID).FName]), True and gShowKillMsg);
     1: g_Console_Add(Format(I_PLAYER_EXTRAHARDKILL2, [FName, g_Player_Get(SpawnerUID).FName]), True and gShowKillMsg);
    end
     else g_Console_Add(Format(I_PLAYER_KILL, [FName, g_Player_Get(SpawnerUID).FName]), True and gShowKillMsg);
  end
   else if g_GetUIDType(SpawnerUID) = UID_MONSTER then
  begin
   { TODO 5 : мессаг }
  end;
 end;

 for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
  if FWeapon[a] then
  begin
   case a of
    WEAPON_SAW: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_SAW, True, False);
    WEAPON_SHOTGUN1: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_SHOTGUN1, True, False);
    WEAPON_SHOTGUN2: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_SHOTGUN2, True, False);
    WEAPON_CHAINGUN: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_CHAINGUN, True, False);
    WEAPON_ROCKETLAUNCHER: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_ROCKETLAUNCHER, True, False);
    WEAPON_PLASMA: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_PLASMA, True, False);
    WEAPON_BFG: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_BFG, True, False);
    WEAPON_SUPERPULEMET: b := g_Items_Create(FObj.X, FObj.Y, ITEM_WEAPON_SUPERPULEMET, True, False);
    else b := DWORD(-1);
   end;

   if b <> DWORD(-1) then g_Obj_Push(@gItems[b].Obj, (FObj.Vel.X div 2)-2+Random(5),
                                     (FObj.Vel.Y div 2)-2+Random(5));
  end;

 if R_ITEM_BACKPACK in FRulez then
 begin
  b := g_Items_Create(FObj.X, FObj.Y, ITEM_AMMO_BACKPACK, True, False);
  g_Obj_Push(@gItems[b].Obj, (FObj.Vel.X div 2)-2+Random(5), (FObj.Vel.Y div 2)-2+Random(5));
 end;

 if gGameSettings.GameType <> GT_CUSTOM then
 begin
  if R_KEY_RED in FRulez then
  begin
   b := g_Items_Create(FObj.X, FObj.Y, ITEM_KEY_RED, True, False);
   g_Obj_Push(@gItems[b].Obj, (FObj.Vel.X div 2)-2+Random(5), (FObj.Vel.Y div 2)-2+Random(5));
  end;

  if R_KEY_GREEN in FRulez then
  begin
   b := g_Items_Create(FObj.X, FObj.Y, ITEM_KEY_GREEN, True, False);
   g_Obj_Push(@gItems[b].Obj, (FObj.Vel.X div 2)-2+Random(5), (FObj.Vel.Y div 2)-2+Random(5));
  end;

  if R_KEY_BLUE in FRulez then
  begin
   b := g_Items_Create(FObj.X, FObj.Y, ITEM_KEY_BLUE, True, False);
   g_Obj_Push(@gItems[b].Obj, (FObj.Vel.X div 2)-2+Random(5), (FObj.Vel.Y div 2)-2+Random(5));
  end;
 end;

 if FFlag <> FLAG_NONE then
 begin
  with gFlags[FFlag] do
  begin
   Obj.X := FObj.X;
   Obj.Y := FObj.Y;
   Direction := FDirection;
   State := FLAG_STATE_DROPPED;
   Count := FLAG_TIME;
   g_Obj_Push(@Obj, (FObj.Vel.X div 2)-2+Random(5), (FObj.Vel.Y div 2)-2+Random(5));

   if FFlag = FLAG_RED then s := I_PLAYER_REDFLAG else s := I_PLAYER_BLUEFLAG;

   g_Console_Add(Format(I_PLAYER_DROPFLAG, [FName, s]), True);
   g_Game_Message(Format(I_MESSAGE_DROPFLAG, [AnsiUpperCase(s)]), 144);
  end;
 end;

 g_Player_CreateCorpse(Self);
end;

function TPlayer.BodyInLiquid(XInc, YInc: Integer): Boolean;
begin
 Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc, PLAYER_RECT.Width,
                              PLAYER_RECT.Height-20, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2);
end;

procedure TPlayer.MakeBloodSimple(Count: Word);
begin
 g_GFX_Blood(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)+8,
             FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2),
             Count div 2, 3, -1, 16, (PLAYER_RECT.Height*2 div 3));
 g_GFX_Blood(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-8,
             FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2),
             Count div 2, -3, -1, 16, (PLAYER_RECT.Height*2) div 3);
end;

procedure TPlayer.MakeBloodVector(Count: Word; VelX, VelY: Integer);
begin
 g_GFX_Blood(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
             FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2),
             Count, VelX, VelY, 16, (PLAYER_RECT.Height*2) div 3);
end;

procedure TPlayer.NextWeapon();
var
  i: Byte;
  ok: Boolean;
begin
 if FBFGFireCounter <> -1 then Exit;

 if FTime[T_SWITCH] > gTime then Exit;

 for i := WEAPON_KASTET to WEAPON_SUPERPULEMET do
  if FReloading[i] > 0 then Exit;

 ok := False;

 for i := FCurrWeap+1 to WEAPON_SUPERPULEMET do
  if FWeapon[i] then
  begin
   FCurrWeap := i;
   ok := True;
   Break;
  end;

 if not ok then
  for i := WEAPON_KASTET to FCurrWeap-1 do
  if FWeapon[i] then
  begin
   FCurrWeap := i;
   Break;
  end;

 FTime[T_SWITCH] := gTime+72;

 if FCurrWeap = WEAPON_SAW then FSawSoundSelect.Play(127, 255, True);

 FModel.SetWeapon(FCurrWeap);
end;

function TPlayer.PickItem(ItemType: Byte; respawn: Boolean; var remove: Boolean): Boolean;
var
  a: Boolean;
begin
 Result := False;
 a := LongBool(gGameSettings.Options and GAME_OPTION_WEAPONSTAY) and respawn;
 remove := not a;

 case ItemType of
  ITEM_MEDKIT_SMALL:
  if FHealth < 100 then
  begin
   IncMax(FHealth, 10, 100);
   Result := True;
   remove := True;
  end;

  ITEM_MEDKIT_LARGE:
  if FHealth < 100 then
  begin
   IncMax(FHealth, 25, 100);
   Result := True;
   remove := True;
  end;

  ITEM_ARMOR_GREEN:
  if FArmor < 100 then
  begin
   FArmor := 100;
   Result := True;
   remove := True;
  end;

  ITEM_ARMOR_BLUE:
  if (FHealth < 100) or (FArmor < 200) then
  begin
   if FArmor < 200 then FArmor := 200;
   Result := True;
   remove := True;
  end;

  ITEM_SPHERE_BLUE:
  if FHealth < 200 then
  begin
   IncMax(FHealth, 100, 200);
   Result := True;
   remove := True;
  end;

  ITEM_SPHERE_WHITE:
  if (FHealth < 200) or (FArmor < 200) then
  begin
   FHealth := 200;
   FArmor := 200;
   Result := True;
   remove := True;
  end;

  ITEM_WEAPON_SAW:
  begin
   if (not respawn) and (not gGameSettings.GameType = GT_SINGLE) then
   begin
    FWeapon[WEAPON_SAW] := True;
    remove := True;
    Result := True;
    Exit;
   end;

   if a or FWeapon[WEAPON_SAW] then Exit;

   Result := not FWeapon[WEAPON_SAW];
   FWeapon[WEAPON_SAW] := True;
  end;

  ITEM_WEAPON_SHOTGUN1:
  if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SHOTGUN1] then
  begin
   if a and FWeapon[WEAPON_SHOTGUN1] then Exit;

   IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
   FWeapon[WEAPON_SHOTGUN1] := True;
   Result := True;
  end;

  ITEM_WEAPON_SHOTGUN2:
  if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SHOTGUN2] then
  begin
   if a and FWeapon[WEAPON_SHOTGUN2] then Exit;

   IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
   FWeapon[WEAPON_SHOTGUN2] := True;
   Result := True;
  end;

  ITEM_WEAPON_CHAINGUN:
  if (FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS]) or not FWeapon[WEAPON_CHAINGUN] then
  begin
   if a and FWeapon[WEAPON_CHAINGUN] then Exit;

   IncMax(FAmmo[A_BULLETS], 50, FMaxAmmo[A_BULLETS]);
   FWeapon[WEAPON_CHAINGUN] := True;
   Result := True;
  end;

  ITEM_WEAPON_ROCKETLAUNCHER:
  if (FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS]) or not FWeapon[WEAPON_ROCKETLAUNCHER] then
  begin
   if a and FWeapon[WEAPON_ROCKETLAUNCHER] then Exit;

   IncMax(FAmmo[A_ROCKETS], 2, FMaxAmmo[A_ROCKETS]);
   FWeapon[WEAPON_ROCKETLAUNCHER] := True;
   Result := True;
  end;

  ITEM_WEAPON_PLASMA:
  if (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or not FWeapon[WEAPON_PLASMA] then
  begin
   if a and FWeapon[WEAPON_PLASMA] then Exit;

   IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
   FWeapon[WEAPON_PLASMA] := True;
   Result := True;
  end;

  ITEM_WEAPON_BFG:
  if (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or not FWeapon[WEAPON_BFG] then
  begin
   if a and FWeapon[WEAPON_BFG] then Exit;

   IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
   FWeapon[WEAPON_BFG] := True;
   Result := True;
  end;

  ITEM_WEAPON_SUPERPULEMET:
  if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SUPERPULEMET] then
  begin
   if a and FWeapon[WEAPON_SUPERPULEMET] then Exit;

   IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
   FWeapon[WEAPON_SUPERPULEMET] := True;
   Result := True;
  end;

  ITEM_AMMO_BULLETS:
  if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then
  begin
   IncMax(FAmmo[A_BULLETS], 10, FMaxAmmo[A_BULLETS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_BULLETS_BOX:
  if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then
  begin
   IncMax(FAmmo[A_BULLETS], 50, FMaxAmmo[A_BULLETS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_SHELLS:
  if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then
  begin
   IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_SHELLS_BOX:
  if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then
  begin
   IncMax(FAmmo[A_SHELLS], 25, FMaxAmmo[A_SHELLS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_ROCKET:
  if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then
  begin
   IncMax(FAmmo[A_ROCKETS], 1, FMaxAmmo[A_ROCKETS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_ROCKET_BOX:
  if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then
  begin
   IncMax(FAmmo[A_ROCKETS], 5, FMaxAmmo[A_ROCKETS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_CELL:
  if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then
  begin
   IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_CELL_BIG:
  if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then
  begin
   IncMax(FAmmo[A_CELLS], 100, FMaxAmmo[A_CELLS]);
   Result := True;
   remove := True;
  end;

  ITEM_AMMO_BACKPACK:
  if not(R_ITEM_BACKPACK in FRulez) or
     (FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS]) or
     (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or
     (FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS]) or
     (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) then
  begin
   FMaxAmmo[A_BULLETS] := 400;
   FMaxAmmo[A_SHELLS] := 100;
   FMaxAmmo[A_ROCKETS] := 100;
   FMaxAmmo[A_CELLS] := 600;

   if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then
    IncMax(FAmmo[A_BULLETS], 10, FMaxAmmo[A_BULLETS]);
   if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then
    IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
   if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then
    IncMax(FAmmo[A_ROCKETS], 1, FMaxAmmo[A_ROCKETS]);
   if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then
    IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);

   FRulez := FRulez + [R_ITEM_BACKPACK];
   Result := True;
   remove := True;
  end;

  ITEM_KEY_RED:
  if not(R_KEY_RED in FRulez) then
  begin
   Include(FRulez, R_KEY_RED);
   Result := True;
   remove := True;
  end;

  ITEM_KEY_GREEN:
  if not(R_KEY_GREEN in FRulez) then
  begin
   Include(FRulez, R_KEY_GREEN);
   Result := True;
   remove := True;
  end;

  ITEM_KEY_BLUE:
  if not(R_KEY_BLUE in FRulez) then
  begin
   Include(FRulez, R_KEY_BLUE);
   Result := True;
   remove := True;
  end;

  ITEM_SUIT:
  if FMegaRulez[MR_SUIT] < gTime+PLAYER_SUIT_TIME then
  begin
   FMegaRulez[MR_SUIT] := gTime+PLAYER_SUIT_TIME;
   Result := True;
   remove := True;
  end;

  ITEM_OXYGEN:
  if FAir < AIR_MAX then
  begin
   FAir := AIR_MAX;
   Result := True;
   remove := True;
  end;

  ITEM_MEDKIT_BLACK:
  begin
   if not (R_BERSERK in FRulez) then
   begin
    Include(FRulez, R_BERSERK);
    if FBFGFireCounter < 1 then
    begin
      FCurrWeap := WEAPON_KASTET;
      FModel.SetWeapon(WEAPON_KASTET);
    end;
    Inc(FPain, 100);
    Result := True;
    remove := True;
   end;
   if FHealth < 100 then
   begin
    FHealth := 100;
    Result := True;
    remove := True;
   end;
  end;

  ITEM_INV:
  if FMegaRulez[MR_INV] < gTime+PLAYER_INV_TIME then
  begin
   FMegaRulez[MR_INV] := gTime+PLAYER_INV_TIME;
   Result := True;
   remove := True;
  end;
 end;
end;

procedure TPlayer.PrevWeapon();
var
  i: Byte;
  ok: Boolean;
begin
 if FBFGFireCounter <> -1 then Exit;

 if FTime[T_SWITCH] > gTime then Exit;

 for i := WEAPON_KASTET to WEAPON_SUPERPULEMET do
  if FReloading[i] > 0 then Exit;
 
 ok := False;

 if FCurrWeap > 0 then
 for i := FCurrWeap-1 downto WEAPON_KASTET do
  if FWeapon[i] then
  begin
   FCurrWeap := i;
   ok := True;
   Break;
  end;

 if not ok then
  for i := WEAPON_SUPERPULEMET downto FCurrWeap+1 do
  if FWeapon[i] then
  begin
   FCurrWeap := i;
   Break;
  end;

 FTime[T_SWITCH] := gTime+72;

 if FCurrWeap = WEAPON_SAW then FSawSoundSelect.Play(127, 255, True);

 FModel.SetWeapon(FCurrWeap);
end;

procedure TPlayer.Push(vx, vy: Integer);
begin
 //g_Console_Add(Format('Push x=%d y=%d', [vx, vy]));

 FObj.Accel.X := FObj.Accel.X+vx;
 FObj.Accel.Y := FObj.Accel.Y+vy;
end;

procedure TPlayer.Reset(Force: Boolean);
begin
 if Force then FLive := False;

 FTime[T_RESPAWN] := 0;
 FGodMode := False;
 FFrags := 0;
 FKills := 0;

 if gGameSettings.GameType = GT_SINGLE then
 begin
  FMonsterKills := 0;
  FDeath := 0;
  FSecrets := 0;
 end;
end;

procedure TPlayer.Respawn(Silent: Boolean);
var
  RespawnPoint: TRespawnPoint;
  a, b, c: Byte;
  Anim: TAnimation;
  ID: DWORD;
begin
 if gGameSettings.GameType = GT_CUSTOM then
 begin
  if FTime[T_RESPAWN] > gTime then Exit;

  case FTeam of
   TEAM_RED: c := RESPAWNPOINT_RED;
   TEAM_BLUE: c := RESPAWNPOINT_BLUE;
   else c := RESPAWNPOINT_DM;
  end;

  if (c = RESPAWNPOINT_DM) and (g_Map_GetPointCount(c) = 0) then
   c := IfThen(FTeam = TEAM_RED, RESPAWNPOINT_RED, RESPAWNPOINT_BLUE);

  if (c <> RESPAWNPOINT_DM) and (g_Map_GetPointCount(RESPAWNPOINT_DM) <> 0) then
   if (g_Map_GetPointCount(c) = 0) or (Random(2) = 1) then c := RESPAWNPOINT_DM;

  FRulez := FRulez{+[R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE]}-[R_BERSERK];
 end
  else
 begin
  if FTime[T_RESPAWN] > gTime then Exit;

  if FPlayerNum = PLAYERNUM_1 then
    c := RESPAWNPOINT_PLAYER1
  else
    if FPlayerNum = PLAYERNUM_2 then
      c := RESPAWNPOINT_PLAYER2
    else
      c := RESPAWNPOINT_DM;

  FRulez := FRulez-[R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE, R_BERSERK];
 end;

 ReleaseKeys();
 FModel.SetFlag(FLAG_NONE); 

 if not FLive then
 begin
  FHealth := 100;
  FArmor := 0;
  FLive := True;
  FAir := AIR;

  for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
  begin
   FWeapon[a] := False;
   FReloading[a] := 0;
  end;

  FWeapon[WEAPON_PISTOL] := True;
  FWeapon[WEAPON_KASTET] := True;
  FCurrWeap := WEAPON_PISTOL;

  FModel.SetWeapon(FCurrWeap);

  for b := A_BULLETS to A_CELLS do FAmmo[b] := 0;

  FAmmo[A_BULLETS] := 50;

  FMaxAmmo[A_BULLETS] := 200;
  FMaxAmmo[A_SHELLS] := 50;
  FMaxAmmo[A_ROCKETS] := 50;
  FMaxAmmo[A_CELLS] := 300;

  if gGameSettings.GameType = GT_CUSTOM then FRulez := [R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE]
   else FRulez := [];
 end;

 if not g_Map_GetPoint(c, RespawnPoint) then
 begin
  g_FatalError(I_GAME_GETDMERROR);
  Exit;
 end;

 FObj.X := RespawnPoint.X-PLAYER_RECT.X;
 FObj.Y := RespawnPoint.Y-PLAYER_RECT.Y;
 FObj.Vel := _Point(0, 0);
 FObj.Accel := _Point(0, 0);

 FDirection := RespawnPoint.Direction;
 if FDirection = D_LEFT then FAngle := 180 else FAngle := 0;

 FIncCam := 0;
 FBFGFireCounter := -1;
 FFlag := 0;
 FPain := 0;
 FLastHit := 0;

 SetAction(A_STAND, True);
 FModel.Direction := FDirection;

 for a := Low(FTime) to High(FTime) do FTime[a] := 0;
 for a := Low(FMegaRulez) to High(FMegaRulez) do FMegaRulez[a] := 0;

 FDamageBuffer := 0;

 if (not gLoadGameMode) and (not Silent) then
  if g_Frames_Get(ID, 'FRAMES_TELEPORT') then
  begin
   Anim := TAnimation.Create(ID, False, 3);
   g_GFX_OnceAnim(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                  FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, Anim);
   Anim.Destroy;
  end;
end;

procedure TPlayer.Run(Direction: TDirection);
begin
 if MAX_RUNVEL > 8 then FlySmoke();

 if Direction = D_LEFT then
  (if FObj.Vel.X > -MAX_RUNVEL then FObj.Vel.X := FObj.Vel.X-(MAX_RUNVEL shr 3))
 else
  if FObj.Vel.X < MAX_RUNVEL then FObj.Vel.X := FObj.Vel.X+(MAX_RUNVEL shr 3);

 SetAction(A_WALK);
end;

procedure TPlayer.SeeDown();
begin
 SetAction(A_SEEDOWN);

 if FDirection = D_LEFT then FAngle := ANGLE_LEFTDOWN else FAngle := ANGLE_RIGHTDOWN;

 if FIncCam > -120 then DecMin(FIncCam, 5, -120);
end;

procedure TPlayer.SeeUp();
begin
 SetAction(A_SEEUP);

 if FDirection = D_LEFT then FAngle := ANGLE_LEFTUP else FAngle := ANGLE_RIGHTUP;

 if FIncCam < 120 then IncMax(FIncCam, 5, 120);
end;

procedure TPlayer.SetAction(Action: Byte; Force: Boolean = False);
var
 Prior: Byte;
begin
 case Action of
  A_WALK: Prior := 3;
  A_DIE1: Prior := 5;
  A_DIE2: Prior := 5;
  A_ATTACK: Prior := 2;
  A_SEEUP: Prior := 1;
  A_SEEDOWN: Prior := 1;
  A_ATTACKUP: Prior := 2;
  A_ATTACKDOWN: Prior := 2;
  A_PAIN: Prior := 4;
  else Prior := 0;
 end;

 if (Prior > FActionPrior) or Force then
  if not ((Prior = 2) and (FCurrWeap = WEAPON_SAW)) then
  begin
   FActionPrior := Prior;
   FActionAnim := Action;
   FActionForce := Force;
   FActionChanged := True;
  end;

 if Action in [A_ATTACK, A_ATTACKUP, A_ATTACKDOWN] then FModel.SetFire(True); 
end;

function TPlayer.StayOnStep(XInc, YInc: Integer): Boolean;
begin
 Result :=  not g_Map_CollidePanel(FObj.X+PLAYER_RECT.X, FObj.Y+YInc+PLAYER_RECT.Y+PLAYER_RECT.Height-1,
                                   PLAYER_RECT.Width, 1, PANEL_STEP)
            and g_Map_CollidePanel(FObj.X+PLAYER_RECT.X, FObj.Y+YInc+PLAYER_RECT.Y+PLAYER_RECT.Height,
                                   PLAYER_RECT.Width, 1, PANEL_STEP);
end;

function TPlayer.TeleportTo(X, Y: Integer; silent: Boolean; dir: TDirection): Boolean;
var
  Anim: TAnimation;
  ID: DWORD;
begin
 Result := False;
 
 if g_CollideLevel(X, Y, PLAYER_RECT.Width, PLAYER_RECT.Height) then Exit;

 Anim := nil;
 if not silent then
 begin
  if g_Frames_Get(ID, 'FRAMES_TELEPORT') then
  begin
   Anim := TAnimation.Create(ID, False, 3);
  end;

  g_Sound_PlayExAt('SOUND_GAME_TELEPORT', 255, FObj.X, FObj.Y);
  g_GFX_OnceAnim(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                 FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, Anim);
 end;
 
 FObj.X := X-PLAYER_RECT.X;
 FObj.Y := Y-PLAYER_RECT.Y;
 FDirection := dir;

 if not silent and (Anim <> nil) then
 begin
  g_GFX_OnceAnim(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                 FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, Anim);
  Anim.Destroy;
 end;

 Result := True;
end;

function nonz(a: Single): Single;
begin
 if a <> 0 then Result := a else Result := 1;
end;

procedure TPlayer.Update();
var
  b: Byte;
  i, ii, wx, wy, xd, yd: Integer;
  blockmon, headwater: Boolean;
  st: Word;
begin
 if gFly then FlySmoke();
 if FDirection = D_LEFT then FAngle := 180 else FAngle := 0;

 //if (FObj.Accel.X <> 0) then
 // g_Console_Add('1 Accel.X='+IntToStr(FObj.Accel.X), True);

 if FLive then
 begin
  if FKeys[KEY_UP].Pressed then SeeUp();
  if FKeys[KEY_DOWN].Pressed then SeeDown();
 end;

 if not (FKeys[KEY_UP].Pressed or FKeys[KEY_DOWN].Pressed) and (FIncCam <> 0) then
 begin
  i := g_Basic.Sign(FIncCam);
  FIncCam := Abs(FIncCam);
  DecMin(FIncCam, 5, 0);
  FIncCam := FIncCam*i;
 end;

 if gTime mod (GAME_TICK*2) <> 0 then
 begin
  if (FObj.Vel.X = 0) and FLive then
  begin
   if FKeys[KEY_LEFT].Pressed then Run(D_LEFT);
   if FKeys[KEY_RIGHT].Pressed then Run(D_RIGHT);
  end;

  //if (FObj.Accel.X <> 0) then g_Console_Add('2 Accel.X='+IntToStr(FObj.Accel.X), True);
  st := g_Obj_Move(@FObj);
  if WordBool(st and MOVE_HITWATER) then g_Obj_Splash(@FObj);
  Exit;
 end;

 FActionChanged := False;

 if FLive then
 begin
  if FKeys[KEY_LEFT].Pressed then Run(D_LEFT);
  if FKeys[KEY_RIGHT].Pressed then Run(D_RIGHT);
  if FKeys[KEY_NEXTWEAPON].Pressed then NextWeapon();
  if FKeys[KEY_PREVWEAPON].Pressed then PrevWeapon();
  if FKeys[KEY_FIRE].Pressed then Fire();
  if FKeys[KEY_OPEN].Pressed then Use();
  if FKeys[KEY_JUMP].Pressed then Jump();
 end
  else if FKeys[KEY_UP].Pressed then
 begin
  if gGameSettings.GameType = GT_CUSTOM then Respawn(False)
   else
  begin
   if (FTime[T_RESPAWN] <= gTime) and gGameOn and (not FLive) then
   begin
    if LongBool(gGameSettings.Options and GAME_OPTION_TWOPLAYER) then Respawn(False)
     else
    begin
     gExit := EXIT_RESTART;
     Exit;
    end;
   end;
  end;
 end;

 st := g_Obj_Move(@FObj);

 if WordBool(st and MOVE_HITWATER) then g_Obj_Splash(@FObj);

 blockmon := g_Map_CollidePanel(FObj.X+PLAYER_HEADRECT.X, FObj.Y+PLAYER_HEADRECT.Y,
                                PLAYER_HEADRECT.Width, PLAYER_HEADRECT.Height,
                                PANEL_BLOCKMON);
 headwater := HeadInLiquid(0, 0);

 if (not FLive) or not (FKeys[KEY_LEFT].Pressed or FKeys[KEY_RIGHT].Pressed) then
  if FObj.Vel.X <> 0 then FObj.Vel.X := z_dec(FObj.Vel.X, 1);

 if (FLastHit = HIT_TRAP) and (FPain > 90) then FPain := 90;
 DecMin(FPain, 5, 0);

 if FLive and (FObj.Y > gMapInfo.Height+128) then Kill(K_FALLKILL, 0, HIT_FALL);

 i := 9;

 if FLive then
 begin
  if FCurrWeap = WEAPON_SAW then
   if not (FSawSound.IsPlaying() or FSawSoundHit.IsPlaying() or
           FSawSoundSelect.IsPlaying()) then FSawSoundIdle.Play(127, 255);

  for b := WEAPON_KASTET to WEAPON_SUPERPULEMET do
   if FReloading[b] > 0 then Dec(FReloading[b]);

  if FBFGFireCounter > -1 then
   if FBFGFireCounter = 0 then
   begin
    wx := FObj.X+WEAPONPOINT[FDirection].X;
    wy := FObj.Y+WEAPONPOINT[FDirection].Y;
    xd := wx+IfThen(FDirection = D_LEFT, -30, 30);
    yd := wy+firediry();

    g_Weapon_bfgshot(wx, wy, xd, yd, FUID);
    FReloading[WEAPON_BFG] := 0;
    FBFGFireCounter := -1;

    if (FAngle = 0) or (FAngle = 180) then SetAction(A_ATTACK)
     else if (FAngle = ANGLE_LEFTDOWN) or (FAngle = ANGLE_RIGHTDOWN) then SetAction(A_ATTACKDOWN)
      else if (FAngle = ANGLE_LEFTUP) or (FAngle = ANGLE_RIGHTUP) then SetAction(A_ATTACKUP);
   end else FBFGFireCounter := FBFGFireCounter-1;

  if FMegaRulez[MR_SUIT] < gTime then
  begin
   b := g_GetAcidHit(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width, PLAYER_RECT.Height);

   if (b > 0) and (gTime mod (15*GAME_TICK) = 0) then Damage(b, 0, 0, 0, HIT_ACID);
  end;

  if headwater or blockmon then
  begin
   FAir := FAir-1;

   if FAir < -9 then
   begin
    Damage(10, 0, 0, 0, HIT_WATER);
    FAir := 0;
   end
    else if (FAir mod 31 = 0) and not blockmon then
   begin
    g_GFX_Bubbles(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2), FObj.Y+PLAYER_RECT.Y-4, 5+Random(6), 8, 4);
    g_Sound_PlayExAt('SOUND_GAME_BUBBLES', 255, FObj.X, FObj.Y);
   end;
  end else if FAir < AIR then FAir := AIR;

  if FDamageBuffer > 0 then
  begin
   if FDamageBuffer >= 9 then
   begin
    SetAction(A_PAIN);

    if FDamageBuffer < 30 then i := 9
     else if FDamageBuffer < 100 then i := 18
      else i := 27;
   end;

   ii := Round(FDamageBuffer*FHealth / nonz(FArmor*(3/4)+FHealth));
   FArmor := FArmor-(FDamageBuffer-ii);
   FHealth := FHealth-ii;
   if FArmor < 0 then
   begin
    FHealth := FHealth+FArmor;
    FArmor := 0;
   end;

   if FHealth <= 0 then
    if FHealth > -30 then Kill(K_SIMPLEKILL, FLastSpawnerUID, FLastHit)
     else if FHealth > -50 then Kill(K_HARDKILL, FLastSpawnerUID, FLastHit)
      else Kill(K_EXTRAHARDKILL, FLastSpawnerUID, FLastHit);

   if FLive then
   begin
    if FDamageBuffer <= 20 then FModel.PlaySound(MODELSOUND_PAIN, 1, FObj.X, FObj.Y)
    else if FDamageBuffer <= 55 then FModel.PlaySound(MODELSOUND_PAIN, 2, FObj.X, FObj.Y)
    else if FDamageBuffer <= 120 then FModel.PlaySound(MODELSOUND_PAIN, 3, FObj.X, FObj.Y)
    else FModel.PlaySound(MODELSOUND_PAIN, 4, FObj.X, FObj.Y);
   end;

   FDamageBuffer := 0;
  end;

  CollideItem();
 end;

 if (FActionAnim = A_PAIN) and (FModel.Animation <> A_PAIN) then
 begin
  FModel.ChangeAnimation(FActionAnim, FActionForce);
  FModel.GetCurrentAnimation.MinLength := i;
  FModel.GetCurrentAnimationMask.MinLength := i;
 end else FModel.ChangeAnimation(FActionAnim, FActionForce and (FModel.Animation <> A_STAND));

 if (FModel.GetCurrentAnimation.Played or ((not FActionChanged) and (FModel.Animation = A_WALK)))
    then SetAction(A_STAND, True);

 if not ((FModel.Animation = A_WALK) and (Abs(FObj.Vel.X) < 4) and not FModel.Fire) then FModel.Update;

 for b := Low(FKeys) to High(FKeys) do
  if FKeys[b].Time = 0 then FKeys[b].Pressed := False else Dec(FKeys[b].Time);
end;

procedure g_Player_CreateCorpse(Player: TPlayer);
var
  find_id: DWORD;
  ok: Boolean;
begin
 if Player.Live then Exit;
 if Player.FObj.Y >= gMapInfo.Height+128 then Exit;

 with Player do
 begin
  if (FHealth >= -50) or (gGibsCount = 0) then
  begin
   if gCorpses = nil then Exit;

   ok := False;
   for find_id := 0 to High(gCorpses) do
    if gCorpses[find_id] = nil then
    begin
     ok := True;
     Break;
    end;

   if not ok then find_id := Random(Length(gCorpses));

   gCorpses[find_id] := TCorpse.Create(FObj.X, FObj.Y, FModel.Name, FHealth < -20);
   gCorpses[find_id].FColor := FModel.Color;
   gCorpses[find_id].FObj.Vel := FObj.Vel;
   gCorpses[find_id].FObj.Accel := FObj.Accel;
  end else g_Player_CreateGibs(FObj.X+PLAYER_RECT_CX, FObj.Y+PLAYER_RECT_CY, FModel.Name, FModel.Color);
 end;
end;

procedure g_Player_CreateGibs(fX, fY: Integer; ModelName: string; fColor: TRGB);
var
  a: Integer;
  GibsArray: TGibsArray;
begin
 if gGibs = nil then Exit;
 if not g_PlayerModel_GetGibs(ModelName, GibsArray) then Exit;

 for a := 0 to High(GibsArray) do
 with gGibs[CurrentGib] do
 begin
  Color := fColor;
  ID := GibsArray[a].ID;
  MaskID := GibsArray[a].MaskID;
  Live := True;
  g_Obj_Init(@Obj);
  Obj.Rect := GibsArray[a].Rect;
  Obj.X := fX-GibsArray[a].Rect.X-(GibsArray[a].Rect.Width div 2);
  Obj.Y := fY-GibsArray[a].Rect.Y-(GibsArray[a].Rect.Height div 2);
  g_Obj_PushA(@Obj, 25+Random(10), Random(361));
  RAngle := 0;

  if gBloodCount > 0 then
   g_GFX_Blood(fX, fY, 16*gBloodCount+Random(5*gBloodCount), -16+Random(33), -16+Random(33),
               Random(48), Random(48));

  if CurrentGib >= High(gGibs) then CurrentGib := 0
   else CurrentGib := CurrentGib+1;
 end;
end;

procedure g_Player_UpdateCorpse();
var
  i: Integer;
  vel: TPoint2i;
  mr: Word;
begin
 if gGibs <> nil then
 for i := 0 to High(gGibs) do
  if gGibs[i].Live then
  with gGibs[i] do
  begin
   vel := Obj.Vel;
   mr := g_Obj_Move(@Obj);

   if WordBool(mr and MOVE_FALLOUT) then
   begin
    Live := False;
    Continue;
   end;

   if WordBool(mr and MOVE_HITWALL) then Obj.Vel.X := -(vel.X div 2);
   if WordBool(mr and (MOVE_HITCEIL or MOVE_HITLAND)) then Obj.Vel.Y := -(vel.Y div 2);

   RAngle := RAngle+Round((Abs(Obj.Vel.X)+Abs(Obj.Vel.Y)));
   if RAngle > 360 then RAngle := RAngle mod 360;

   if gTime mod (GAME_TICK*3) = 0 then Obj.Vel.X := z_dec(Obj.Vel.X, 1);
  end;

 if gCorpses <> nil then 
 for i := 0 to High(gCorpses) do
  if gCorpses[i] <> nil then
   if gCorpses[i].State = CORPSE_STATE_REMOVEME then
   begin
    gCorpses[i].Destroy();
    gCorpses[i] := nil;
   end else gCorpses[i].Update;
end;

procedure g_Player_DrawCorpses();
var
  i: Integer;
  a: TPoint;
begin
 if gGibs <> nil then
 for i := 0 to High(gGibs) do if gGibs[i].Live then
  with gGibs[i] do
  begin
   if not g_Obj_Collide(sX, sY, sWidth, sHeight, @Obj) then Continue;

   a.X := Obj.Rect.X+(Obj.Rect.Width div 2);
   a.y := Obj.Rect.Y+(Obj.Rect.Height div 2);

   e_DrawAdv(ID, Obj.X, Obj.Y, 0, True, False, RAngle, @a, M_NONE);

   e_Colors := Color;
   e_DrawAdv(MaskID, Obj.X, Obj.Y, 0, True, False, RAngle, @a, M_NONE);
   e_Colors.R := 255;
   e_Colors.G := 255;
   e_Colors.B := 255;
  end;

 if gCorpses <> nil then 
 for i := 0 to High(gCorpses) do
  if gCorpses[i] <> nil then gCorpses[i].Draw;
end;

function TPlayer.Collide(X, Y: Integer; Width, Height: Word): Boolean;
begin
 Result := g_Collide(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width,
                     PLAYER_RECT.Height, X, Y, Width, Height);
end;

function TPlayer.Collide(Rect: TRectWH): Boolean;
begin
 Result := g_Collide(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width,
                     PLAYER_RECT.Height, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TPlayer.Collide(X, Y: Integer): Boolean;
begin
 X := X-FObj.X-PLAYER_RECT.X;
 Y := Y-FObj.Y-PLAYER_RECT.Y;
 Result := (x >= 0) and (x <= PLAYER_RECT.Width) and
           (y >= 0) and (y <= PLAYER_RECT.Height);
end;

procedure g_Player_RemoveAllCorpses();
var
  i: Integer;
begin
 gGibs := nil;
 SetLength(gGibs, MaxGibs);
 CurrentGib := 0;

 if gCorpses <> nil then
  for i := 0 to High(gCorpses) do
   if gCorpses[i] <> nil then gCorpses[i].Destroy;

 gCorpses := nil;
 SetLength(gCorpses, MaxCorpses);
end;

function g_Player_ValidName(Name: string): Boolean;
var
  a: Integer;
begin
 Result := True;

 if gPlayers = nil then Exit;

 for a := 0 to High(gPlayers) do
  if gPlayers[a] <> nil then
   if LowerCase(Name) = LowerCase(gPlayers[a].FName) then
   begin
    Result := False;
    Exit;
   end;
end;

procedure TPlayer.SetDirection(Direction: TDirection);
var
  d: TDirection;
begin
 d := FModel.Direction;

 FModel.Direction := Direction;
 if d <> Direction then FModel.ChangeAnimation(FModel.Animation, True);

 FDirection := Direction;
end;

function TPlayer.GetKeys(): Word;
begin
 Result := 0;

 if R_KEY_RED in FRulez then Result := KEY_RED;
 if R_KEY_GREEN in FRulez then Result := Result or KEY_GREEN;
 if R_KEY_BLUE in FRulez then Result := Result or KEY_BLUE;

 if FTeam = TEAM_RED then Result := Result or KEY_REDTEAM;
 if FTeam = TEAM_BLUE then Result := Result or KEY_BLUETEAM;
end;

procedure TPlayer.Use();
begin
 if FTime[T_USE] > gTime then Exit;

 g_Triggers_PressR(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width,
                   PLAYER_RECT.Height, FUID, ACTIVATE_PLAYERPRESS);
 FTime[T_USE] := gTime+120;
end;

function TPlayer.FullInLift(XInc, YInc: Integer): Integer;
begin
 if g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                       PLAYER_RECT.Width, PLAYER_RECT.Height-8, PANEL_LIFTUP) then Result := -1
  else
 if g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                       PLAYER_RECT.Width, PLAYER_RECT.Height-8, PANEL_LIFTDOWN) then Result := 1
 else Result := 0;
end;

procedure TPlayer.GetFlag(Flag: Byte);
var
  s: string;
begin
 if Flag = FLAG_NONE then Exit;

 if (Flag = FTeam) and (gFlags[Flag].State = FLAG_STATE_NORMAL) and (FFlag <> FLAG_NONE) then
 begin
  if FFlag = FLAG_RED then s := I_PLAYER_REDFLAG else s := I_PLAYER_BLUEFLAG;

  g_Console_Add(Format(I_PLAYER_CAPTUREFLAG, [FName, s]), True);

  g_Map_ResetFlag(FFlag);
  g_Game_Message(Format(I_MESSAGE_CAPTUREFLAG, [AnsiUpperCase(s)]), 144);

  gTeamStat[FTeam].Goals := gTeamStat[FTeam].Goals+1;

  FFlag := FLAG_NONE;
  FModel.SetFlag(FLAG_NONE);

  Exit;
 end;

 if (Flag = FTeam) and (gFlags[Flag].State = FLAG_STATE_DROPPED) then
 begin
  if Flag = FLAG_RED then s := I_PLAYER_REDFLAG else s := I_PLAYER_BLUEFLAG;

  g_Console_Add(Format(I_PLAYER_RETURNFLAG, [FName, s]), True);

  g_Map_ResetFlag(Flag);
  g_Game_Message(Format(I_MESSAGE_RETURNFLAG, [AnsiUpperCase(s)]), 144);

  Exit;
 end;

 if Flag <> FTeam then
 begin
  FFlag := Flag;
  FModel.SetFlag(FFlag);

  if Flag = FLAG_RED then s := I_PLAYER_REDFLAG else s := I_PLAYER_BLUEFLAG;
  g_Console_Add(Format(I_PLAYER_GETFLAG, [FName, s]), True);

  g_Game_Message(Format(I_MESSAGE_GETFLAG, [AnsiUpperCase(s)]), 144);

  gFlags[Flag].State := FLAG_STATE_CAPTURED;
 end;
end;

procedure TPlayer.GetSecret();
begin
 Inc(FSecrets);
end;

procedure TPlayer.PressKey(Key: Byte; Time: Word = 0);
begin
 Assert(Key <= High(FKeys));

 FKeys[Key].Pressed := True;
 FKeys[Key].Time := Time;
end;

procedure TPlayer.ReleaseKeys();
var
  a: Integer;
begin
 for a := Low(FKeys) to High(FKeys) do
 begin
  FKeys[a].Pressed := False;
  FKeys[a].Time := 0;
 end;
end;

procedure TPlayer.OnDamage(Angle: SmallInt);
begin

end;

function TPlayer.firediry(): Integer;
begin
 if FKeys[KEY_UP].Pressed then Result := -42
 else if FKeys[KEY_DOWN].Pressed then Result := 19
 else Result := 0;
end;

procedure TPlayer.Load(Rec: PPlayerSaveRec);
begin
 loadobj(@Rec^.Obj, @FObj);
 CopyMemory(@FAmmo, @Rec^.Ammo, SizeOf(FAmmo));
 CopyMemory(@FMaxAmmo, @Rec^.MaxAmmo, SizeOf(FMaxAmmo));
 CopyMemory(@FWeapon, @Rec^.Weapon, SizeOf(FWeapon));
 CopyMemory(@FMegaRulez, @Rec^.MegaRulez, SizeOf(FMegaRulez));
 CopyMemory(@FReloading, @Rec^.Reloading, SizeOf(FReloading));
 CopyMemory(@FRulez, @Rec^.Rulez, SizeOf(FRulez));
 SetDirection(Rec^.Direction);
 FUID := Rec^.UID;
 FHealth := Rec^.Health;
 FArmor := Rec^.Armor;
 FAir := Rec^.Air;
 FLive := Rec^.Live;
 FKills := Rec^.Kills;
 FMonsterKills := Rec^.MonsterKills;
 FFrags := Rec^.Frags;
 FDeath := Rec^.Death;
 FFlag := Rec^.Flag;
 FSecrets := Rec^.Secrets;
 FPain := Rec^.Pain;
 FCurrWeap := Rec^.CurrWeap;
 FBFGFireCounter := Rec^.BFGFireCounter;
 FDamageBuffer := Rec^.DamageBuffer;
 FLastSpawnerUID := Rec^.LastSpawnerUID;
 FLastHit := Rec^.LastHit;

 FModel.SetWeapon(FCurrWeap);
end;

procedure TPlayer.Save(Rec: PPlayerSaveRec);
begin
 saveobj(@FObj, @Rec^.Obj);
 CopyMemory(@Rec^.Ammo, @FAmmo, SizeOf(FAmmo));
 CopyMemory(@Rec^.MaxAmmo, @FMaxAmmo, SizeOf(FMaxAmmo));
 CopyMemory(@Rec^.Weapon, @FWeapon, SizeOf(FWeapon));
 CopyMemory(@Rec^.MegaRulez, @FMegaRulez, SizeOf(FMegaRulez));
 CopyMemory(@Rec^.Reloading, @FReloading, SizeOf(FReloading));
 CopyMemory(@Rec^.Rulez, @FRulez, SizeOf(FRulez));
 Rec^.Direction := FDirection;
 Rec^.UID := FUID;
 Rec^.Health := FHealth;
 Rec^.Armor := FArmor;
 Rec^.Air := FAir;
 Rec^.Live := FLive;
 Rec^.Kills := FKills;
 Rec^.MonsterKills := FMonsterKills;
 Rec^.Frags := FFrags;
 Rec^.Death := FDeath;
 Rec^.Flag := FFlag;
 Rec^.Secrets := FSecrets;
 Rec^.Pain := FPain;
 Rec^.CurrWeap := FCurrWeap;
 Rec^.BFGFireCounter := FBFGFireCounter;
 Rec^.DamageBuffer := FDamageBuffer;
 Rec^.LastSpawnerUID := FLastSpawnerUID;
 Rec^.LastHit := FLastHit;
end;

procedure TPlayer.AllRulez(Health: Boolean);
var
  a: Integer;
begin
 if Health then
 begin
  FHealth := 200;
  FArmor := 200;
  Exit;
 end;

 for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do FWeapon[a] := True;
 for a := A_BULLETS to A_CELLS do FAmmo[a] := 30000;
 FRulez := FRulez+[R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE];
end;

procedure TPlayer.SetModel(ModelName: string);
var
  m: TPlayerModel;
  old_color: TRGB;
begin
 m := g_PlayerModel_Get(ModelName);
 if m = nil then Exit;

 old_color := _RGB(0, 0, 0);

 if FModel <> nil then
 begin
  old_color := FModel.Color;
  FModel.Destroy;
 end;

 FModel := m;

 FModel.Color := old_color;
 FModel.SetWeapon(FCurrWeap);
 FModel.SetFlag(FFlag);
 SetDirection(FDirection);
end;

procedure TPlayer.SetColor(Color: TRGB);
begin
 if (gGameSettings.GameMode <> GM_CTF) and (gGameSettings.GameMode <> GM_TDM) then
  if FModel <> nil then FModel.Color := Color;
end;

procedure TPlayer.FlySmoke();
var
  id: DWORD;
  Anim: TAnimation;
begin
 if BodyInLiquid(0, 0) then Exit;

 if g_Frames_Get(id, 'FRAMES_SMOKE') then
 begin
  Anim := TAnimation.Create(id, False, 3);
  Anim.Alpha := 150;
  g_GFX_OnceAnim(Obj.X+Obj.Rect.X+Random(Obj.Rect.Width)-(Anim.Width div 2),
                 Obj.Y+Obj.Rect.Height-4+Random(5), Anim, ONCEANIM_SMOKE);
  Anim.Destroy;
 end;
end;

{ TCorpse }

constructor TCorpse.Create(X, Y: Integer; ModelName: string; Mess: Boolean);
begin
 g_Obj_Init(@FObj);
 FObj.X := X;
 FObj.Y := Y;
 FObj.Rect := PLAYER_CORPSERECT;
 FModelName := ModelName;
 if Mess then
 begin
  FState := CORPSE_STATE_MESS;
  g_PlayerModel_GetAnim(ModelName, A_DIE2, FAnimation, FAnimationMask);
 end
  else
 begin
  FState := CORPSE_STATE_NORMAL;
  g_PlayerModel_GetAnim(ModelName, A_DIE1, FAnimation, FAnimationMask);
 end;
end;

procedure TCorpse.Damage(Value: Word; vx, vy: Integer);
begin
 if FState = CORPSE_STATE_REMOVEME then Exit;

 FDamage := FDamage+Value;

 if FDamage > 150 then
 begin
  if FAnimation <> nil then
  begin
   FAnimation.Destroy;
   FAnimation := nil;

   FState := CORPSE_STATE_REMOVEME;

   g_Player_CreateGibs(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                       FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                       FModelName, FColor);
  end;
 end
  else
 begin
  FObj.Vel.X := FObj.Vel.X+vx;
  FObj.Vel.Y := FObj.Vel.Y+vy;
 end;
end;

destructor TCorpse.Destroy();
begin
 if FAnimation <> nil then FAnimation.Destroy;

 inherited;
end;

procedure TCorpse.Draw();
begin
 if FState = CORPSE_STATE_REMOVEME then Exit;

 if FAnimation <> nil then FAnimation.Draw(FObj.X, FObj.Y, M_NONE);
 if FAnimationMask <> nil then
 begin
  e_Colors := FColor;
  FAnimationMask.Draw(FObj.X, FObj.Y, M_NONE);
  e_Colors.R := 255;
  e_Colors.G := 255;
  e_Colors.B := 255;
 end;
end;

procedure TCorpse.Update();
var
  st: Word;
begin
 if FState = CORPSE_STATE_REMOVEME then Exit;

 if gTime mod (GAME_TICK*2) <> 0 then
 begin
  g_Obj_Move(@FObj);
  Exit;
 end;

 FObj.Vel.X := z_dec(FObj.Vel.X, 1);

 st := g_Obj_Move(@FObj);

 if WordBool(st and MOVE_FALLOUT) then
 begin
  FState := CORPSE_STATE_REMOVEME;
  Exit;
 end;

 if WordBool(st and MOVE_HITWATER) then g_Obj_Splash(@FObj);

 if FAnimation <> nil then FAnimation.Update;
 if FAnimationMask <> nil then FAnimationMask.Update;
end;

{ TBot }

constructor TBot.Create(ModelName: string);
begin
 inherited Create(ModelName);

 FDifficult.WeaponPrior := WEAPON_PRIOR1;
 FDifficult.CloseWeaponPrior := WEAPON_PRIOR2;
 //FDifficult.SafeWeaponPrior := WEAPON_PRIOR3;
end;

destructor TBot.Destroy();
begin

 inherited Destroy();
end;

procedure TBot.Draw();
begin
 inherited Draw();

 //if FTargetUID <> 0 then e_DrawLine(1, FObj.X, FObj.Y, g_Player_Get(FTargetUID).FObj.X,
 //                                   g_Player_Get(FTargetUID).FObj.Y, 255, 0, 0);
end;

procedure TBot.Respawn(Silent: Boolean);
begin
 inherited Respawn(Silent);

 FAIFlags := nil;
 FSelectedWeapon := FCurrWeap;
 FTargetUID := 0;
end;

procedure TBot.UpdateCombat();

type
  TTarget = record
   UID: Word;
   X, Y: Integer;
   cX, cY: Integer;
   Dist: Word;
   Line: Boolean;
   Visible: Boolean;
  end;

  TTargetRecord = array of TTarget;

function Compare(a, b: TTarget): Integer;
begin
 if a.Line and not b.Line then Result := -1
 else if not a.Line and b.Line then Result := 1
 else if (a.Line and b.Line) or ((not a.Line) and (not b.Line)) then
  (if a.Dist > b.Dist then Result := 1 else Result := -1)
 else Result := -1;
end;

var
  a, x1, y1, x2, y2: Integer;
  targets: TTargetRecord;
  ammo: Word;
  Target, BestTarget: TTarget;
  firew, fireh: Integer;
  angle: SmallInt;
begin
 if FCurrWeap <> FSelectedWeapon then NextWeapon();

 if (GetAIFlag('NEEDFIRE') <> '') and (FCurrWeap = FSelectedWeapon) then
 begin
  RemoveAIFlag('NEEDFIRE');

  case FCurrWeap of
   WEAPON_PLASMA, WEAPON_SUPERPULEMET, WEAPON_CHAINGUN: PressKey(KEY_FIRE, 30);
   WEAPON_SAW, WEAPON_KASTET, WEAPON_MEGAKASTET: PressKey(KEY_FIRE, 60);
   else PressKey(KEY_FIRE, 0);
  end;
 end;

 x1 := FObj.X+WEAPONPOINT[FDirection].X;
 y1 := FObj.Y+WEAPONPOINT[FDirection].Y;

 Target.UID := FTargetUID;

 if Target.UID <> 0 then
 begin
  with g_Player_Get(Target.UID) do
  begin
   Target.X := FObj.X;
   Target.Y := FObj.Y;
  end;

  Target.cX := Target.X+PLAYER_RECT_CX;
  Target.cY := Target.Y+PLAYER_RECT_CY;
  Target.Visible := g_TraceVector(x1, y1, Target.cX, Target.cY);
  Target.Line := (y1+4 < Target.Y+PLAYER_RECT.Y+PLAYER_RECT.Height) and
                 (y1-4 > Target.Y+PLAYER_RECT.Y);
 end
  else
 begin
  Target.X := 0;
  Target.Y := 0;
  Target.cX := 0;
  Target.cY := 0;
  Target.Visible := False;
  Target.Line := False;
 end;

 targets := nil;

 if not (Target.Line and Target.Visible) then
 for a := 0 to High(gPlayers) do
  if (gPlayers[a] <> nil) and (gPlayers[a].Live) and
     (gPlayers[a].FUID <> FUID) and not SameTeam(FUID, gPlayers[a].FUID) then
  begin
   x2 := gPlayers[a].FObj.X+PLAYER_RECT_CX;
   y2 := gPlayers[a].FObj.Y+PLAYER_RECT_CY;

   if PlayerOnScreen(gPlayers[a].FObj.X, gPlayers[a].FObj.Y) and
      g_TraceVector(x1, y1, x2, y2) then
   begin
    SetLength(targets, Length(targets)+1);

    with targets[High(targets)] do
    begin
     UID := gPlayers[a].FUID;
     X := gPlayers[a].FObj.X;
     Y := gPlayers[a].FObj.Y;
     cX := x2;
     cY := y2;
     Dist := g_PatchLength(x1, y1, x2, y2);
     Line := (y1+4 < Target.Y+PLAYER_RECT.Y+PLAYER_RECT.Height) and
             (y1-4 > Target.Y+PLAYER_RECT.Y);
     Visible := True;
    end;
   end;
  end;
  
 if targets <> nil then
 begin
  BestTarget := targets[0];
  if Length(targets) > 1 then
   for a := 1 to High(targets) do
    if Compare(BestTarget, targets[a]) = 1 then
     BestTarget := targets[a];

  if ((not Target.Visible) and BestTarget.Visible and (Target.UID <> BestTarget.UID)) or
     ((not Target.Line) and BestTarget.Line and BestTarget.Visible) then
  begin
   Target := BestTarget;

   if (Healthy() = 3) or ((Healthy() = 2)) then
   begin
    if ((RunDirection() = D_LEFT) and (Target.X > FObj.X)) then SetAIFlag('GORIGHT', '1');
    if ((RunDirection() = D_RIGHT) and (Target.X < FObj.X)) then SetAIFlag('GOLEFT', '1');
   end
    else
   begin
    if ((RunDirection() = D_LEFT) and (Target.X < FObj.X)) then SetAIFlag('GORIGHT', '1');
    if ((RunDirection() = D_RIGHT) and (Target.X > FObj.X)) then SetAIFlag('GOLEFT', '1');
   end;

   SelectWeapon(Abs(x1-Target.cX));
  end;
 end;
 
 if Target.UID <> 0 then
 begin
  if not PlayerOnScreen(Target.X, Target.Y) then
  begin
   if (Healthy() = 3) or ((Healthy() = 2)) then
   begin
    if ((RunDirection() = D_LEFT) and (Target.X > FObj.X)) then SetAIFlag('GORIGHT', '1');
    if ((RunDirection() = D_RIGHT) and (Target.X < FObj.X)) then SetAIFlag('GOLEFT', '1');
   end
    else
   begin
    Target.UID := 0;
    if ((RunDirection() = D_LEFT) and (Target.X < FObj.X)) then SetAIFlag('GORIGHT', '1');
    if ((RunDirection() = D_RIGHT) and (Target.X > FObj.X)) then SetAIFlag('GOLEFT', '1');
   end;
  end
   else
  begin
   if g_TraceVector(x1, y1, Target.cX, Target.cY) then FLastVisible := gTime;

   if (Abs(FObj.Y-Target.Y) <= 128) then
   begin
    if ((RunDirection() = D_LEFT) and (Target.X > FObj.X)) then SetAIFlag('GORIGHT', '1');
    if ((RunDirection() = D_RIGHT) and (Target.X < FObj.X)) then SetAIFlag('GOLEFT', '1');
   end;
  end;

  if FDirection = D_LEFT then angle := ANGLE_LEFTUP else angle := ANGLE_RIGHTUP;

  firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
  fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

  if g_CollideLine(x1, y1, x1+firew, y1+fireh,
                   Target.X+PLAYER_RECT.X+8+GetInterval(FDifficult.DiagPrecision, 128), //96
                   Target.Y+PLAYER_RECT.Y+8+GetInterval(FDifficult.DiagPrecision, 128),
                   PLAYER_RECT.Width-16, PLAYER_RECT.Height-16) and
     g_TraceVector(x1, y1, Target.cX, Target.cY) then
  begin
   SetAIFlag('NEEDFIRE', '1');
   SetAIFlag('NEEDSEEUP', '1');
  end;

  if FDirection = D_LEFT then angle := ANGLE_LEFTDOWN else angle := ANGLE_RIGHTDOWN;

  firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
  fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

  if g_CollideLine(x1, y1, x1+firew, y1+fireh,
                   Target.X+PLAYER_RECT.X+8+GetInterval(FDifficult.DiagPrecision, 128),
                   Target.Y+PLAYER_RECT.Y+8+GetInterval(FDifficult.DiagPrecision, 128),
                   PLAYER_RECT.Width-16, PLAYER_RECT.Height-16) and
     g_TraceVector(x1, y1, Target.cX, Target.cY) then
  begin
   SetAIFlag('NEEDFIRE', '1');
   SetAIFlag('NEEDSEEDOWN', '1');
  end;

  if Target.Visible and
     (y1+4 < Target.Y+PLAYER_RECT.Y+PLAYER_RECT.Height) and
     (y1-4 > Target.Y+PLAYER_RECT.Y) then
  begin
   if ((FDirection = D_LEFT) and (Target.X < FObj.X)) or
      ((FDirection = D_RIGHT) and (Target.X > FObj.X)) then SetAIFlag('NEEDFIRE', '1');

   if Abs(FObj.X-Target.X) < Trunc(gPlayerScreenSize.X*0.75) then
    if GetRnd(FDifficult.CloseJump) then
    begin
     if Abs(FObj.X-Target.X) < 128 then a := 4 else a := 30;
     if Random(a) = 0 then SetAIFlag('NEEDJUMP', '1');
    end;
  end;

   if Target.UID <> 0 then
    if gTime-FLastVisible > 2000 then Target.UID := 0
     else if not g_Player_Get(Target.UID).Live then Target.UID := 0;
 end;

 FTargetUID := Target.UID;

 if targets = nil then
  if GetAIFlag('ATTACKLEFT') <> '' then
  begin
   RemoveAIFlag('ATTACKLEFT');

   SetAIFlag('NEEDJUMP', '1');

   if RunDirection() = D_RIGHT then
   begin
    if (Healthy() > 1) and GetRnd(FDifficult.InvisFire) then
    begin
     SetAIFlag('NEEDFIRE', '1');
     SetAIFlag('GOLEFT', '1');
    end;
   end
    else
   begin
    if GetRnd(FDifficult.InvisFire) then SetAIFlag('NEEDFIRE', '1');
    if Healthy() <= 1 then SetAIFlag('GORIGHT', '1');
   end;
  end
   else if GetAIFlag('ATTACKRIGHT') <> '' then
  begin
   RemoveAIFlag('ATTACKRIGHT');

   SetAIFlag('NEEDJUMP', '1');

   if RunDirection() = D_LEFT then
   begin
    if (Healthy() > 1) and GetRnd(FDifficult.InvisFire) then
    begin
     SetAIFlag('NEEDFIRE', '1');
     SetAIFlag('GORIGHT', '1');
    end;
   end
    else
   begin
    if GetRnd(FDifficult.InvisFire) then SetAIFlag('NEEDFIRE', '1');
    if Healthy() <= 1 then SetAIFlag('GOLEFT', '1');
   end;
  end;

 if targets <> nil then
  for a := 0 to High(targets) do
  begin
   if GetRnd(FDifficult.DiagFire) then
   begin
    if FDirection = D_LEFT then angle := ANGLE_LEFTUP else angle := ANGLE_RIGHTUP;

    firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
    fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

    if g_CollideLine(x1, y1, x1+firew, y1+fireh,
                    targets[a].X+PLAYER_RECT.X+8+GetInterval(FDifficult.DiagPrecision, 128),
                    targets[a].Y+PLAYER_RECT.Y+8+GetInterval(FDifficult.DiagPrecision, 128),
                    PLAYER_RECT.Width-16, PLAYER_RECT.Height-16) and
       g_TraceVector(x1, y1, targets[a].cX, targets[a].cY) then
    begin
     SetAIFlag('NEEDFIRE', '1');
     SetAIFlag('NEEDSEEUP', '1');
    end;

    if FDirection = D_LEFT then angle := ANGLE_LEFTDOWN else angle := ANGLE_RIGHTDOWN;

    firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
    fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

    if g_CollideLine(x1, y1, x1+firew, y1+fireh,
                    targets[a].X+PLAYER_RECT.X+8+GetInterval(FDifficult.DiagPrecision, 128),
                    targets[a].Y+PLAYER_RECT.Y+8+GetInterval(FDifficult.DiagPrecision, 128),
                    PLAYER_RECT.Width-16, PLAYER_RECT.Height-16) and
       g_TraceVector(x1, y1, targets[a].cX, targets[a].cY) then
    begin
     SetAIFlag('NEEDFIRE', '1');
     SetAIFlag('NEEDSEEDOWN', '1');
    end;
   end;

   if targets[a].Line and targets[a].Visible and
      (((FDirection = D_LEFT) and (targets[a].X < FObj.X)) or
       ((FDirection = D_RIGHT) and (targets[a].X > FObj.X))) then
   begin
    SetAIFlag('NEEDFIRE', '1');
    Break;
   end;
  end;

 if g_Weapon_Danger(FUID, FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y,
                    PLAYER_RECT.Width, PLAYER_RECT.Height,
                    40+GetInterval(FDifficult.Cover, 40)) then
  SetAIFlag('NEEDJUMP', '1');

 ammo := GetAmmoByWeapon(FCurrWeap);
 if ((FCurrWeap = WEAPON_SHOTGUN2) and (ammo < 2)) or
    ((FCurrWeap = WEAPON_BFG) and (ammo < 40)) or
    (ammo = 0) then SetAIFlag('SELECTWEAPON', '1'); 

 if GetAIFlag('SELECTWEAPON') = '1' then
 begin
  SelectWeapon(-1);
  RemoveAIFlag('SELECTWEAPON');
 end;
end;

procedure TBot.Update();
begin
 if not FLive then
 begin
  ReleaseKeys();
  PressKey(KEY_UP);
 end
  else
 begin
  UpdateMove();
  UpdateCombat();
 end;

 inherited Update();
end;

procedure TBot.ReleaseKey(Key: Byte);
begin
 with FKeys[Key] do
 begin
  Pressed := False;
  Time := 0;
 end;
end;

function TBot.KeyPressed(Key: Word): Boolean;
begin
 Result := FKeys[Key].Pressed;
end;

function TBot.GetAIFlag(fName: ShortString): ShortString;
var
  a: Integer;
begin
 Result := '';

 fName := LowerCase(fName);

 if FAIFlags <> nil then
  for a := 0 to High(FAIFlags) do
   if LowerCase(FAIFlags[a].Name) = fName then
   begin
    Result := FAIFlags[a].Value;
    Break;
   end;
end;

procedure TBot.RemoveAIFlag(fName: ShortString);
var
  a, b: Integer;
begin
 if FAIFlags = nil then Exit;

 fName := LowerCase(fName);

 for a := 0 to High(FAIFlags) do
  if LowerCase(FAIFlags[a].Name) = fName then
  begin
   if a <> High(FAIFlags) then
    for b := a to High(FAIFlags)-1 do
     FAIFlags[b] := FAIFlags[b+1];

   SetLength(FAIFlags, Length(FAIFlags)-1);
   Break;
  end;
end;

procedure TBot.SetAIFlag(fName, fValue: ShortString);
var
  a: Integer;
  ok: Boolean;
begin
 a := 0;
 ok := False;

 fName := LowerCase(fName);

 if FAIFlags <> nil then
  for a := 0 to High(FAIFlags) do
   if LowerCase(FAIFlags[a].Name) = fName then
   begin
    ok := True;
    Break;
   end;

 if ok then FAIFlags[a].Value := fValue
  else
 begin
  SetLength(FAIFlags, Length(FAIFlags)+1);
  with FAIFlags[High(FAIFlags)] do
  begin
   Name := fName;
   Value := fValue;
  end;
 end;
end;

procedure TBot.UpdateMove;

procedure GoLeft(Time: Word = 1);
begin
 ReleaseKey(KEY_LEFT);
 ReleaseKey(KEY_RIGHT);
 PressKey(KEY_LEFT, Time);
 SetDirection(D_LEFT);
end;

procedure GoRight(Time: Word = 1);
begin
 ReleaseKey(KEY_LEFT);
 ReleaseKey(KEY_RIGHT);
 PressKey(KEY_RIGHT, Time);
 SetDirection(D_RIGHT);
end;

function Rnd(a: Word): Boolean;
begin
 Result := Random(a) = 0;
end;

procedure Turn(Time: Word = 1200);
begin
 if RunDirection() = D_LEFT then GoRight(Time) else GoLeft(Time);
end;

procedure Stop();
begin
 ReleaseKey(KEY_LEFT);
 ReleaseKey(KEY_RIGHT);
end;

function CanRunLeft(): Boolean;
begin
 Result := not CollideLevel(-1, 0);
end;

function CanRunRight(): Boolean;
begin
 Result := not CollideLevel(1, 0);
end;

function CanRun(): Boolean;
begin
 if RunDirection() = D_LEFT then Result := CanRunLeft() else Result := CanRunRight();
end;

procedure Jump(Time: Word = 30);
begin
 PressKey(KEY_JUMP, Time);
end;

function NearHole(): Boolean;
var
  x, sx: Integer;
begin
 { TODO 5 : Лестницы }
 sx := IfThen(RunDirection() = D_LEFT, -1, 1);
 for x := 1 to PLAYER_RECT.Width do
  if (not StayOnStep(x*sx, 0)) and
     (not CollideLevel(x*sx, PLAYER_RECT.Height)) and
     (not CollideLevel(x*sx, PLAYER_RECT.Height*2)) then
  begin
   Result := True;
   Exit;
  end;

 Result := False;
end;

function BorderHole(): Boolean;
var
  x, sx, xx: Integer;
begin
 { TODO 5 : Лестницы }
 sx := IfThen(RunDirection() = D_LEFT, -1, 1);
 for x := 1 to PLAYER_RECT.Width do
  if (not StayOnStep(x*sx, 0)) and
     (not CollideLevel(x*sx, PLAYER_RECT.Height)) and
     (not CollideLevel(x*sx, PLAYER_RECT.Height*2)) then
  begin
   for xx := x to x+32 do
    if CollideLevel(xx*sx, PLAYER_RECT.Height) then
    begin
     Result := True;
     Exit;
    end;
  end;

 Result := False;
end;

function NearDeepHole(): Boolean;
var
  x, sx, y: Integer;
begin
 Result := False;

 sx := IfThen(RunDirection() = D_LEFT, -1, 1);
 y := 3;

 for x := 1 to PLAYER_RECT.Width do
  if (not StayOnStep(x*sx, 0)) and
     (not CollideLevel(x*sx, PLAYER_RECT.Height)) and
     (not CollideLevel(x*sx, PLAYER_RECT.Height*2)) then
  begin
   while FObj.Y+y*PLAYER_RECT.Height < gMapInfo.Height do
   begin
    if CollideLevel(x*sx, PLAYER_RECT.Height*y) then Exit;
    y := y+1;
   end;

   Result := True;
  end else Result := False;
end;

function OverDeepHole(): Boolean;
var
  y: Integer;
begin
 Result := False;

 y := 1;
 while FObj.Y+y*PLAYER_RECT.Height < gMapInfo.Height do
 begin
  if CollideLevel(0, PLAYER_RECT.Height*y) then Exit;
  y := y+1;
 end;

 Result := True;
end;

function OnGround(): Boolean;
begin
 Result := StayOnStep(0, 0) or CollideLevel(0, 1);
end;

function OnLadder(): Boolean;
begin
 Result := FullInStep(0, 0);
end;

function BelowLadder(): Boolean;
begin
 Result := (FullInStep(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height) and
            not CollideLevel(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height)) or
           (FullInStep(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP) and
            not CollideLevel(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP));
end;

function BelowLiftUp(): Boolean;
begin
 Result := ((FullInLift(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height) = -1) and
           not CollideLevel(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height)) or
           ((FullInLift(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP) = -1) and
           not CollideLevel(IfThen(RunDirection() = D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP));
end;

function OnTopLift(): Boolean;
begin
 Result := (FullInLift(0, 0) = -1) and (FullInLift(0, -32) = 0);
end;

function CanJumpOver(): Boolean;
var
  sx, y: Integer;
begin
 sx := IfThen(RunDirection() = D_LEFT, -1, 1);

 Result := False;

 if not CollideLevel(sx, 0) then Exit;

 for y := 1 to BOT_MAXJUMP do
  if CollideLevel(0, -y) then Exit else
   if not CollideLevel(sx, -y) then
   begin
    Result := True;
    Exit;
   end;
end;

function CanJumpUp(Dist: ShortInt): Boolean;
var
  y, yy: Integer;
  c: Boolean;
begin
 Result := False;

 if CollideLevel(Dist, 0) then Exit;

 c := False;
 for y := 0 to BOT_MAXJUMP do
  if CollideLevel(Dist, -y) then
  begin
   c := True;
   Break;
  end;

 if not c then Exit;

 c := False;
 for yy := y+1 to BOT_MAXJUMP do
  if not CollideLevel(Dist, -yy) then
  begin
   c := True;
   Break;
  end;

 if not c then Exit;

 c := False;
 for y := 0 to BOT_MAXJUMP do
  if CollideLevel(0, -y) then
  begin
   c := True;
   Break;
  end;

 if c then Exit;

 if y < yy then Exit;

 Result := True;
end;

begin
 if OnLadder() or ((BelowLadder() or BelowLiftUp()) and Rnd(8)) then
 begin
  ReleaseKey(KEY_LEFT);
  ReleaseKey(KEY_RIGHT);
  Jump();
 end;

 if GetAIFlag('GOLEFT') <> '' then
 begin
  RemoveAIFlag('GOLEFT');
  if CanRunLeft() then GoLeft(360);
 end;

 if GetAIFlag('GORIGHT') <> '' then
 begin
  RemoveAIFlag('GORIGHT');
  if CanRunRight() then GoRight(360);
 end;

 if FObj.X < -32 then GoRight(360)
  else if FObj.X+32 > gMapInfo.Width then GoLeft(360);

 if GetAIFlag('NEEDJUMP') <> '' then
 begin
  Jump(0);
  RemoveAIFlag('NEEDJUMP');
 end;

 if GetAIFlag('NEEDSEEUP') <> '' then
 begin
  ReleaseKey(KEY_UP);
  ReleaseKey(KEY_DOWN);
  PressKey(KEY_UP, 20);
  RemoveAIFlag('NEEDSEEUP');
 end;

 if GetAIFlag('NEEDSEEDOWN') <> '' then
 begin
  ReleaseKey(KEY_UP);
  ReleaseKey(KEY_DOWN);
  PressKey(KEY_DOWN, 20);
  RemoveAIFlag('NEEDSEEDOWN');
 end;

 if GetAIFlag('GOINHOLE') <> '' then
  if not OnGround() then
  begin
   ReleaseKey(KEY_LEFT);
   ReleaseKey(KEY_RIGHT);
   RemoveAIFlag('GOINHOLE');
   SetAIFlag('FALLINHOLE', '1');
  end;

 if GetAIFlag('FALLINHOLE') <> '' then
  if OnGround() then RemoveAIFlag('FALLINHOLE');

 if not (KeyPressed(KEY_LEFT) or KeyPressed(KEY_RIGHT)) then
  if GetAIFlag('FALLINHOLE') = '' then
   if (not OnLadder()) or (FObj.Vel.Y >= 0) or (OnTopLift()) then
    if Rnd(2) then GoLeft(360) else GoRight(360);

 if OnGround() and CanJumpUp(IfThen(RunDirection() = D_LEFT, -1, 1)*32) and Rnd(8) then Jump();

 if OnGround() and NearHole() then
  if NearDeepHole() then
   case Random(6) of
    0..3: Turn();
    4: Jump();
    5: begin
        Turn();
        Jump();
       end;
   end
  else if GetAIFlag('GOINHOLE') = '' then
   case Random(6) of
    0: Turn();
    1: Jump();
    else if BorderHole() then SetAIFlag('GOINHOLE', '1');
   end;

 if (not CanRun()) and OnGround() then
 begin
  if CanJumpOver() or OnLadder() then Jump() else Turn();
 end;
end;

function TBot.FullInStep(XInc, YInc: Integer): Boolean;
begin
 Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                              PLAYER_RECT.Width, PLAYER_RECT.Height, PANEL_STEP);
end;

{function TBot.NeedItem(Item: Byte): Byte;
begin
 Result := 4;
end;}

procedure TBot.SelectWeapon(Dist: Integer);
var
  a: Integer;
  
 function HaveAmmo(weapon: Byte): Boolean;
 begin
  case weapon of
   WEAPON_PISTOL: Result := FAmmo[A_BULLETS] >= 1;
   WEAPON_SHOTGUN1: Result := FAmmo[A_SHELLS] >= 1;
   WEAPON_SHOTGUN2: Result := FAmmo[A_SHELLS] >= 2;
   WEAPON_CHAINGUN: Result := FAmmo[A_BULLETS] >= 10;
   WEAPON_ROCKETLAUNCHER: Result := FAmmo[A_ROCKETS] >= 1;
   WEAPON_PLASMA: Result := FAmmo[A_CELLS] >= 10;
   WEAPON_BFG: Result := FAmmo[A_CELLS] >= 40;
   WEAPON_SUPERPULEMET: Result := FAmmo[A_SHELLS] >= 1;
   else Result := True;
  end;
 end;

begin
 if Dist = -1 then Dist := BOT_LONGDIST;

 if Dist > BOT_LONGDIST then
 begin
  for a := 0 to 9 do
   if FWeapon[FDifficult.WeaponPrior[a]] and HaveAmmo(FDifficult.WeaponPrior[a]) then
   begin
    FSelectedWeapon := FDifficult.WeaponPrior[a];
    Break;
   end;
 end
  else //if Dist > BOT_UNSAFEDIST then
 begin
  for a := 0 to 9 do
   if FWeapon[FDifficult.CloseWeaponPrior[a]] and HaveAmmo(FDifficult.CloseWeaponPrior[a]) then
   begin
    FSelectedWeapon := FDifficult.CloseWeaponPrior[a];
    Break;
   end;
 end;
 { else
 begin
  for a := 0 to 9 do
   if FWeapon[FDifficult.SafeWeaponPrior[a]] and HaveAmmo(FDifficult.SafeWeaponPrior[a]) then
   begin
    FSelectedWeapon := FDifficult.SafeWeaponPrior[a];
    Break;
   end;
 end;}
end;

function TBot.PickItem(ItemType: Byte; force: Boolean; var remove: Boolean): Boolean;
begin
 Result := inherited PickItem(ItemType, force, remove);

 if Result then SetAIFlag('SELECTWEAPON', '1');
end;

function TBot.Healthy(): Byte;
begin
 if FMegaRulez[MR_INV] > 0 then Result := 3
 else if (FHealth > 80) or ((FHealth > 50) and (FArmor > 20)) then Result := 3
 else if (FHealth > 50) then Result := 2
 else if (FHealth > 20) then Result := 1
 else Result := 0;
end;

function TBot.PlayerOnScreen(PX, PY: Integer): Boolean;
begin
 Result := (Abs(FObj.X-PX-PLAYER_RECT.X) <= Trunc(gPlayerScreenSize.X*0.6)) and
           (Abs(FObj.Y-PY-PLAYER_RECT.Y) <= Trunc(gPlayerScreenSize.Y*0.6));
end;

procedure TBot.OnDamage(Angle: SmallInt);
var
  p: TPlayer;
begin
 inherited;

 if ((Angle = 0) or (Angle = 180)) and (g_GetUIDType(FLastSpawnerUID) = UID_PLAYER) then
 begin
  p := g_Player_Get(FLastSpawnerUID);
  if not PlayerOnScreen(p.FObj.X, p.FObj.Y) then
   if Angle = 0 then SetAIFlag('ATTACKLEFT', '1') else SetAIFlag('ATTACKRIGHT', '1'); 
 end;
end;

function TBot.RunDirection(): TDirection;
begin
 if Abs(Vel.X) >= 1 then
 begin
  if Vel.X > 0 then Result := D_RIGHT else Result := D_LEFT;
 end else Result := FDirection;
end;

function TBot.GetRnd(a: Byte): Boolean;
begin
 if a = 0 then Result := False
  else if a = 255 then Result := True
   else Result := Random(256) > 255-a;
end;

function TBot.GetInterval(a: Byte; radius: SmallInt): SmallInt;
begin
 Result := Round((255-a)/255*radius*(Random(2)-1));
end;

end.
