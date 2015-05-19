unit g_player;

interface

uses
  windows, e_graphics, g_playermodel, g_basic, g_textures,
  g_weapons, g_phys, g_sound, g_saveload, MAPSTRUCT,
  BinEditor, g_panel;

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
  KEY_CHAT       = 10;

  R_ITEM_BACKPACK   = 0;
  R_KEY_RED         = 1;
  R_KEY_GREEN       = 2;
  R_KEY_BLUE        = 3;
  R_BERSERK         = 4;

  MR_SUIT           = 0;
  MR_INVUL          = 1;
  MR_JET            = 2;
  MR_INVIS          = 3;
  MR_MAX            = 3;

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
  TEAM_COOP         = 3;

  SHELL_BULLET      = 0;
  SHELL_SHELL       = 1;

  PLAYERNUM_1       = 1;
  PLAYERNUM_2       = 2;

  ANGLE_NONE        = Low(SmallInt);

  CORPSE_STATE_REMOVEME = 0;
  CORPSE_STATE_NORMAL   = 1;
  CORPSE_STATE_MESS     = 2;

  PLAYER_RECT: TRectWH = (X:15; Y:12; Width:34; Height:52);
  PLAYER_RECT_CX       = 15+(34 div 2);
  PLAYER_RECT_CY       = 12+(52 div 2);
  PLAYER_CORPSERECT: TRectWH = (X:15; Y:48; Width:34; Height:16);

  PLAYER1_DEF_COLOR: TRGB = (R:64; G:175; B:48);
  PLAYER2_DEF_COLOR: TRGB = (R:96; G:96; B:96);

type
  TPlayerStat = record
    Name: String;
    Team: Byte;
    Frags: SmallInt;
    Deaths: SmallInt;
    Lives: Byte;
    Kills: Word;
    Color: TRGB;
    Spectator: Boolean;
  end;

  TPlayerStatArray = Array of TPlayerStat;

  TPlayerSavedState = record
    Health:     Integer;
    Armor:      Integer;
    CurrWeap:   Byte;
    Ammo:       Array [A_BULLETS..A_CELLS] of Word;
    MaxAmmo:    Array [A_BULLETS..A_CELLS] of Word;
    Weapon:     Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Boolean;
    Rulez:      Set of R_ITEM_BACKPACK..R_BERSERK;
    WaitRecall: Boolean;
  end;

  TKeyState = record
    Pressed: Boolean;
    Time: Word;
  end;

  TPlayer = class (TObject)
  private
    FIamBot:    Boolean;
    FUID:       Word;
    FName:        String;
    FTeam:      Byte;
    FLive:      Boolean;
    FDirection: TDirection;
    FHealth:    Integer;
    FLives:     Byte;
    FArmor:     Integer;
    FAir:       Integer;
    FPain:      Integer;
    FKills:     Integer;
    FMonsterKills: Integer;
    FFrags:     Integer;
    FDeath:     Integer;
    FCanJetpack: Boolean;
    FFlag:      Byte;
    FSecrets:   Integer;
    FCurrWeap:  Byte;
    FBFGFireCounter: SmallInt;
    FLastSpawnerUID: Word;
    FLastHit:   Byte;
    FObj:       TObj;
    FXTo, FYTo: Integer;
    FSpectatePlayer: Integer;

    FSavedState: TPlayerSavedState;

    FPlayerNum: Byte;
    FModel:     TPlayerModel;
    FActionPrior:    Byte;
    FActionAnim:     Byte;
    FActionForce:    Boolean;
    FActionChanged:  Boolean;
    FAngle:     SmallInt;
    FFireAngle: SmallInt;
    FIncCam:         Integer;
    FSawSound:       TPlayableSound;
    FSawSoundIdle:   TPlayableSound;
    FSawSoundHit:    TPlayableSound;
    FSawSoundSelect: TPlayableSound;
    FJetSoundOn:     TPlayableSound;
    FJetSoundOff:    TPlayableSound;
    FJetSoundFly:    TPlayableSound;
    FGodMode:   Boolean;
    FNoTarget:   Boolean;

    function    CollideLevel(XInc, YInc: Integer): Boolean;
    function    StayOnStep(XInc, YInc: Integer): Boolean;
    function    HeadInLiquid(XInc, YInc: Integer): Boolean;
    function    BodyInLiquid(XInc, YInc: Integer): Boolean;
    function    FullInLift(XInc, YInc: Integer): Integer;
    {procedure   CollideItem();}
    procedure   FlySmoke(Times: DWORD = 1);
    function    GetAmmoByWeapon(Weapon: Byte): Word;
    procedure   SetAction(Action: Byte; Force: Boolean = False);
    procedure   OnDamage(Angle: SmallInt); virtual;
    function    firediry(): Integer;

    procedure   Run(Direction: TDirection);
    procedure   NextWeapon();
    procedure   PrevWeapon();
    procedure   SeeUp();
    procedure   SeeDown();
    procedure   Fire();
    procedure   Jump();
    procedure   Use();
    
  public
    FDamageBuffer:   Integer;
    
    FAmmo:      Array [A_BULLETS..A_CELLS] of Word;
    FMaxAmmo:   Array [A_BULLETS..A_CELLS] of Word;
    FWeapon:    Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Boolean;
    FRulez:     Set of R_ITEM_BACKPACK..R_BERSERK;
    FMegaRulez: Array [MR_SUIT..MR_MAX] of DWORD;
    FReloading: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Word;
    FTime:      Array [T_RESPAWN..T_USE] of DWORD;
    FKeys:      Array [KEY_LEFT..KEY_CHAT] of TKeyState;
    FSpectator: Boolean;
    FNoRespawn: Boolean;
    FWantsInGame: Boolean;
    FGhost:     Boolean;
    FPhysics:   Boolean;
    FJetpack:   Boolean;
    FActualModelName: string;
    FClientID:  Short;
    FDummy:     Boolean;
    
    constructor Create(); virtual;
    destructor  Destroy(); override;
    procedure   Respawn(Silent: Boolean; Force: Boolean = False); virtual;
    procedure   PressKey(Key: Byte; Time: Word = 0);
    procedure   ReleaseKeys();
    procedure   SetModel(ModelName: String);
    procedure   SetColor(Color: TRGB);
    procedure   SetWeapon(W: Byte);
    function    IsKeyPressed(K: Byte): Boolean;
    function    GetKeys(): Byte;
    function    PickItem(ItemType: Byte; respawn: Boolean; var remove: Boolean): Boolean; virtual;
    function    Collide(X, Y: Integer; Width, Height: Word): Boolean; overload;
    function    Collide(Panel: TPanel): Boolean; overload;
    function    Collide(X, Y: Integer): Boolean; overload;
    procedure   SetDirection(Direction: TDirection);
    procedure   GetSecret();
    function    TeleportTo(X, Y: Integer; silent: Boolean; dir: Byte): Boolean;
    procedure   Push(vx, vy: Integer);
    procedure   ChangeModel(ModelName: String);
    procedure   ChangeTeam;
    procedure   BFGHit();
    function    GetFlag(Flag: Byte): Boolean;
    procedure   SetFlag(Flag: Byte);
    procedure   AllRulez(Health: Boolean);
    procedure   GiveItem(ItemType: Byte);
    procedure   Damage(value: Word; SpawnerUID: Word; vx, vy: Integer; t: Byte); virtual;
    procedure   MakeBloodVector(Count: Word; VelX, VelY: Integer);
    procedure   MakeBloodSimple(Count: Word);
    procedure   Kill(KillType: Byte; SpawnerUID: Word; t: Byte);
    procedure   Reset(Force: Boolean);
    procedure   Spectate(NoMove: Boolean = False);
    procedure   SoftReset();
    procedure   Draw(); virtual;
    procedure   DrawPain();
    procedure   DrawRulez();
    procedure   DrawGUI();
    procedure   Update(); virtual;
    procedure   RememberState();
    procedure   RecallState();
    procedure   SaveState(var Mem: TBinMemoryWriter); virtual;
    procedure   LoadState(var Mem: TBinMemoryReader); virtual;
    procedure   PauseSounds(Enable: Boolean);
    procedure   NetFire(Wpn: Byte; X, Y, AX, AY: Integer; WID: Integer = -1);
    procedure   DoLerp(Level: Integer = 2);
    procedure   SetLerp(XTo, YTo: Integer);
    procedure   JetpackOn;
    procedure   JetpackOff;

    property    Name: String read FName write FName;
    property    Model: TPlayerModel read FModel;
    property    Health: Integer read FHealth write FHealth;
    property    Lives: Byte read FLives write FLives;
    property    Armor: Integer read FArmor write FArmor;
    property    Air:   Integer read FAir write FAir;
    property    Frags: Integer read FFrags write FFrags;
    property    Death: Integer read FDeath write FDeath;
    property    Kills: Integer read FKills write FKills;
    property    CurrWeap: Byte read FCurrWeap write FCurrWeap;
    property    MonsterKills: Integer read FMonsterKills write FMonsterKills;
    property    Secrets: Integer read FSecrets;
    property    GodMode: Boolean read FGodMode write FGodMode;
    property    NoTarget: Boolean read FNoTarget write FNoTarget;
    property    Live: Boolean read FLive write FLive;
    property    Flag: Byte read FFlag;
    property    Team: Byte read FTeam write FTeam;
    property    GameX: Integer read FObj.X write FObj.X;
    property    GameY: Integer read FObj.Y write FObj.Y;
    property    GameVelX: Integer read FObj.Vel.X write FObj.Vel.X;
    property    GameVelY: Integer read FObj.Vel.Y write FObj.Vel.Y;
    property    GameAccelX: Integer read FObj.Accel.X write FObj.Accel.X;
    property    GameAccelY: Integer read FObj.Accel.Y write FObj.Accel.Y;
    property    Vel: TPoint2i read FObj.Vel;
    property    Obj: TObj read FObj;
    property    IncCam: Integer read FIncCam write FIncCam;
    property    UID: Word read FUID write FUID;
    property    Direction: TDirection read FDirection;
  end;

  TDifficult = record
    DiagFire: Byte;
    InvisFire: Byte;
    DiagPrecision: Byte;
    FlyPrecision: Byte;
    Cover: Byte;
    CloseJump: Byte;
    WeaponPrior: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte;
    CloseWeaponPrior: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte;
    //SafeWeaponPrior: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte;
  end;

  TAIFlag = record
    Name: String;
    Value: String;
  end;

  TBot = class (TPlayer)
  private
    FSelectedWeapon:  Byte;
    FTargetUID:       Word;
    FLastVisible:     DWORD;
    FAIFlags:         Array of TAIFlag;
    FDifficult:       TDifficult;

    function    GetRnd(a: Byte): Boolean;
    function    GetInterval(a: Byte; radius: SmallInt): SmallInt;
    function    RunDirection(): TDirection;
    function    FullInStep(XInc, YInc: Integer): Boolean;
    //function    NeedItem(Item: Byte): Byte;
    procedure   SelectWeapon(Dist: Integer);
    procedure   SetAIFlag(fName, fValue: String20);
    function    GetAIFlag(fName: String20): String20;
    procedure   RemoveAIFlag(fName: String20);
    function    Healthy(): Byte;
    procedure   UpdateMove();
    procedure   UpdateCombat();
    function    KeyPressed(Key: Word): Boolean;
    procedure   ReleaseKey(Key: Byte);
    function    TargetOnScreen(TX, TY: Integer): Boolean;
    procedure   OnDamage(Angle: SmallInt); override;

  public
    procedure   Respawn(Silent: Boolean; Force: Boolean = False); override;
    constructor Create(); override;
    destructor  Destroy(); override;
    procedure   Draw(); override;
    function    PickItem(ItemType: Byte; force: Boolean; var remove: Boolean): Boolean; override;
    procedure   Update(); override;
    procedure   SaveState(var Mem: TBinMemoryWriter); override;
    procedure   LoadState(var Mem: TBinMemoryReader); override;
  end;

  TGib = record
    Live:   Boolean;
    ID:     DWORD;
    MaskID: DWORD;
    RAngle: Integer;
    Color:  TRGB;
    Obj:    TObj;
  end;

  TShell = record
    SpriteID: DWORD;
    Live:     Boolean;
    SType:    Byte;
    RAngle:   Integer;
    Timeout:  Cardinal;
    CX, CY:   Integer;
    Obj:      TObj;
  end;
  
  TCorpse = class (TObject)
  private
    FModelName:     String;
    FMess:          Boolean;
    FState:         Byte;
    FDamage:        Byte;
    FColor:         TRGB;
    FObj:           TObj;
    FAnimation:     TAnimation;
    FAnimationMask: TAnimation;
    
  public
    constructor Create(X, Y: Integer; ModelName: String; aMess: Boolean);
    destructor  Destroy(); override;
    procedure   Damage(Value: Word; vx, vy: Integer);
    procedure   Update();
    procedure   Draw();
    procedure   SaveState(var Mem: TBinMemoryWriter);
    procedure   LoadState(var Mem: TBinMemoryReader);
    
    property    Obj: TObj read FObj;
    property    State: Byte read FState;
    property    Mess: Boolean read FMess;
  end;

  TTeamStat = Array [TEAM_RED..TEAM_BLUE] of
    record
      Goals: SmallInt;
    end;

var
  gPlayers: Array of TPlayer;
  gCorpses: Array of TCorpse;
  gGibs: Array of TGib;
  gShells: Array of TShell;
  gTeamStat: TTeamStat;
  gFly: Boolean = False;
  gLastSpawnUsed: Byte = 0;
  MAX_RUNVEL: Integer = 8;
  VEL_JUMP: Integer = 10;
  SHELL_TIMEOUT: Cardinal = 60000;

function  Lerp(X, Y, Factor: Integer): Integer;

procedure g_Gibs_SetMax(Count: Word);
function  g_Gibs_GetMax(): Word;
procedure g_Corpses_SetMax(Count: Word);
function  g_Corpses_GetMax(): Word;
procedure g_Shells_SetMax(Count: Word);
function  g_Shells_GetMax(): Word;

procedure g_Player_Init();
procedure g_Player_Free();
function  g_Player_Create(ModelName: String; Color: TRGB; Team: Byte;
                          Bot: Boolean; PlayerNum: Byte): Word;
procedure g_Player_Remove(UID: Word);
procedure g_Player_UpdateAll();
procedure g_Player_DrawAll();
procedure g_Player_RememberAll();
procedure g_Player_ResetAll(Force, Silent: Boolean);
function  g_Player_Get(UID: Word): TPlayer;
function  g_Player_GetCount(): Byte;
function  g_Player_GetStats(): TPlayerStatArray;
function  g_Player_ValidName(Name: String): Boolean;
procedure g_Player_CreateCorpse(Player: TPlayer);
procedure g_Player_CreateGibs(fX, fY: Integer; ModelName: String; fColor: TRGB);
procedure g_Player_CreateShell(fX, fY, dX, dY: Integer; T: Byte);
procedure g_Player_UpdateCorpse();
procedure g_Player_DrawCorpses();
procedure g_Player_DrawShells();
procedure g_Player_RemoveAllCorpses();
procedure g_Player_Corpses_SaveState(var Mem: TBinMemoryWriter);
procedure g_Player_Corpses_LoadState(var Mem: TBinMemoryReader);
procedure g_Bot_Add(Team, Difficult: Byte);
procedure g_Bot_AddList(Team: Byte; lname: ShortString; num: Integer = -1);
procedure g_Bot_RemoveAll();

implementation

uses
  e_log, g_map, g_items, g_console, SysUtils, g_gfx, Math,
  g_options, g_triggers, g_menu, MAPDEF, g_game,
  WADEDITOR, g_main, g_monsters, CONFIG, g_language, g_net, g_netmsg;

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
    w_prior1: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte;
    w_prior2: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte;
    w_prior3: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte;
  end;

const
  TIME_RESPAWN1 = 1500;
  TIME_RESPAWN2 = 2000;
  TIME_RESPAWN3 = 3000;
  AIR_DEF = 360;
  AIR_MAX = 1091;
  JET_MAX = 540; // ~30 sec
  PLAYER_SUIT_TIME    = 30000;
  PLAYER_INVUL_TIME   = 30000;
  PLAYER_INVIS_TIME   = 35000;
  VEL_SW  = 4;
  VEL_FLY = 6;
  ANGLE_RIGHTUP   = 55;
  ANGLE_RIGHTDOWN = -35;
  ANGLE_LEFTUP    = 125;      
  ANGLE_LEFTDOWN  = -145;
  PLAYER_HEADRECT: TRectWH = (X:24; Y:12; Width:20; Height:12);
  WEAPONPOINT: Array [TDirection] of TPoint = ((X:16; Y:32), (X:47; Y:32));
  BOT_MAXJUMP = 84;
  BOT_LONGDIST   = 300;
  BOT_UNSAFEDIST = 128;
  TEAMCOLOR: Array [TEAM_RED..TEAM_BLUE] of TRGB = ((R:255; G:0; B:0),
                                                   (R:0; G:0; B:255));
  DIFFICULT_EASY: TDifficult = (DiagFire: 32; InvisFire: 32; DiagPrecision: 32;
                                FlyPrecision: 32; Cover: 32; CloseJump: 32);
  DIFFICULT_MEDIUM: TDifficult = (DiagFire: 127; InvisFire: 127; DiagPrecision: 127;
                                  FlyPrecision: 127; Cover: 127; CloseJump: 127);
  DIFFICULT_HARD: TDifficult = (DiagFire: 255; InvisFire: 255; DiagPrecision: 255;
                                FlyPrecision: 255; Cover: 255; CloseJump: 255);
  WEAPON_PRIOR1: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte =
                                (WEAPON_SUPERPULEMET, WEAPON_SHOTGUN2, WEAPON_SHOTGUN1,
                                 WEAPON_CHAINGUN, WEAPON_PLASMA, WEAPON_ROCKETLAUNCHER,
                                 WEAPON_BFG, WEAPON_PISTOL, WEAPON_SAW, WEAPON_KASTET);
  WEAPON_PRIOR2: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte =
                                (WEAPON_SUPERPULEMET, WEAPON_BFG, WEAPON_ROCKETLAUNCHER,
                                 WEAPON_SHOTGUN2, WEAPON_PLASMA, WEAPON_SHOTGUN1,
                                 WEAPON_CHAINGUN, WEAPON_PISTOL, WEAPON_SAW, WEAPON_KASTET);
  //WEAPON_PRIOR3: Array [WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte =
  //                              (WEAPON_SUPERPULEMET, WEAPON_BFG, WEAPON_PLASMA,
  //                               WEAPON_SHOTGUN2, WEAPON_CHAINGUN, WEAPON_SHOTGUN1,
  //                               WEAPON_SAW, WEAPON_ROCKETLAUNCHER, WEAPON_PISTOL, WEAPON_KASTET);

  PLAYER_SIGNATURE = $52594C50; // 'PLYR'
  CORPSE_SIGNATURE = $50524F43; // 'CORP'

  BOTNAMES_FILENAME = 'botnames.txt';
  BOTLIST_FILENAME = 'botlist.txt';

var
  MaxGibs: Word = 150;
  MaxCorpses: Word = 20;
  MaxShells: Word = 666;
  CurrentGib: Integer = 0;
  CurrentShell: Integer = 0;
  BotNames: Array of String;
  BotList: Array of TBotProfile;

function Lerp(X, Y, Factor: Integer): Integer;
begin
  Result := X + ((Y - X) div Factor);
end;

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

procedure g_Shells_SetMax(Count: Word);
begin
  MaxShells := Count;
  SetLength(gShells, Count);

  if CurrentShell >= Count-1 then CurrentShell := 0;
end;

function g_Shells_GetMax(): Word;
begin
  Result := MaxShells;
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

function g_Player_Create(ModelName: String; Color: TRGB; Team: Byte;
                         Bot: Boolean; PlayerNum: Byte): Word;
var
  a: Integer;
  ok: Boolean;
begin
  Result := 0;

  ok := False;
  a := 0;

// Есть ли место в gPlayers:
  if gPlayers <> nil then
    for a := 0 to High(gPlayers) do
      if gPlayers[a] = nil then
      begin
        ok := True;
        Break;
      end;

// Нет места - расширяем gPlayers:
  if not ok then
  begin
    SetLength(gPlayers, Length(gPlayers)+1);
    a := High(gPlayers);
  end;

// Создаем объект игрока:
  if Bot then
    gPlayers[a] := TBot.Create()
  else
    gPlayers[a] := TPlayer.Create();


  gPlayers[a].FActualModelName := ModelName;
  gPlayers[a].SetModel(ModelName);

// Нет модели - создание не возможно:
  if gPlayers[a].FModel = nil then
  begin
    gPlayers[a].Free();
    gPlayers[a] := nil;
    g_FatalError(Format(_lc[I_GAME_ERROR_MODEL], [ModelName]));
    Exit;
  end;

// Если командная игра - красим модель в цвет команды:
  if ((gGameSettings.GameMode = GM_CTF) or
      (gGameSettings.GameMode = GM_TDM)) and
     ((Team = TEAM_RED) or (Team = TEAM_BLUE)) then
    gPlayers[a].FModel.Color := TEAMCOLOR[Team]
  else
    gPlayers[a].FModel.Color := Color;

  gPlayers[a].FTeam := Team;
  gPlayers[a].FUID := g_CreateUID(UID_PLAYER);
  gPlayers[a].FPlayerNum := PlayerNum;
  gPlayers[a].FLive := False;

  Result := gPlayers[a].FUID;
end;

procedure g_Bot_Add(Team, Difficult: Byte);
var
  m: SArray;
  _name, _model: String;
  a, tr, tb: Integer;
begin
  if not g_Game_IsServer then Exit;

// Список названий моделей:
  m := g_PlayerModel_GetNames();
  if m = nil then
    Exit;

// Команда:
  if (gGameSettings.GameType = GT_SINGLE) or (gGameSettings.GameMode = GM_COOP) then
    Team := TEAM_COOP // COOP
  else
    if gGameSettings.GameMode = GM_DM then
      Team := TEAM_NONE // DM
    else
      if Team = TEAM_NONE then // CTF / TDM
      begin
       // Автобаланс команд:
        tr := 0;
        tb := 0;

        for a := 0 to High(gPlayers) do
          if gPlayers[a] <> nil then
          begin
            if gPlayers[a].Team = TEAM_RED then
              Inc(tr)
            else
              if gPlayers[a].Team = TEAM_BLUE then
                Inc(tb);
          end;

        if tr > tb then
          Team := TEAM_BLUE
        else
          if tb > tr then
            Team := TEAM_RED
          else // tr = tb
            if Random(2) = 0 then
              Team := TEAM_RED
            else
              Team := TEAM_BLUE;
      end;

// Выбираем боту имя:
  _name := '';
  if BotNames <> nil then
    for a := 0 to High(BotNames) do
      if g_Player_ValidName(BotNames[a]) then
      begin
        _name := BotNames[a];
        Break;
      end;

// Имени нет, задаем случайное:
  if _name = '' then
    repeat
      _name := Format('DFBOT%.2d', [Random(100)]);
    until g_Player_ValidName(_name);

// Выбираем случайную модель:
  _model := m[Random(Length(m))];

// Создаем бота:
  with g_Player_Get(g_Player_Create(_model,
                                    _RGB(Min(Random(9)*32, 255),
                                         Min(Random(9)*32, 255),
                                         Min(Random(9)*32, 255)),
                                    Team, True, 0)) as TBot do
  begin
    Name := _name;

    case Difficult of
      1: FDifficult := DIFFICULT_EASY;
      2: FDifficult := DIFFICULT_MEDIUM;
      else FDifficult := DIFFICULT_HARD;
    end;
  
    for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    begin
      FDifficult.WeaponPrior[a] := WEAPON_PRIOR1[a];
      FDifficult.CloseWeaponPrior[a] := WEAPON_PRIOR2[a];
      //FDifficult.SafeWeaponPrior[a] := WEAPON_PRIOR3[a];
    end;

    if g_Game_IsNet then MH_SEND_PlayerCreate(UID);
    if g_Game_IsServer and (gGameSettings.MaxLives > 0) then
      Spectate;
  end;
end;

procedure g_Bot_AddList(Team: Byte; lName: ShortString; num: Integer = -1);
var
  m: SArray;
  _name, _model: String;
  a: Integer;
begin
  if not g_Game_IsServer then Exit;

// Список названий моделей:
  m := g_PlayerModel_GetNames();
  if m = nil then
    Exit;

// Команда:
  if (gGameSettings.GameType = GT_SINGLE) or (gGameSettings.GameMode = GM_COOP) then
    Team := TEAM_COOP // COOP
  else
    if gGameSettings.GameMode = GM_DM then
      Team := TEAM_NONE // DM
    else
      if Team = TEAM_NONE then
        Team := BotList[num].team; // CTF / TDM

// Выбираем настройки бота из списка по номеру или имени:
  lName := AnsiLowerCase(lName);
  if (num < 0) or (num > Length(BotList)-1) then
    num := -1;
  if (num = -1) and (lName <> '') and (BotList <> nil) then
    for a := 0 to High(BotList) do
      if AnsiLowerCase(BotList[a].name) = lName then
      begin
        num := a;
        Break;
      end;
  if num = -1 then
    Exit;

// Имя бота:
  _name := BotList[num].name;
// Занято - выбираем случайное:
  if not g_Player_ValidName(_name) then
  repeat
    _name := Format('DFBOT%.2d', [Random(100)]);
  until g_Player_ValidName(_name);

// Модель:
  _model := BotList[num].model;
// Нет такой - выбираем случайную:
  if not InSArray(_model, m) then
    _model := m[Random(Length(m))];

// Создаем бота:
  with g_Player_Get(g_Player_Create(_model, BotList[num].color,
                                    Team, True, 0)) as TBot do
  begin
    Name := _name;

    FDifficult.DiagFire := BotList[num].diag_fire;
    FDifficult.InvisFire := BotList[num].invis_fire;
    FDifficult.DiagPrecision := BotList[num].diag_precision;
    FDifficult.FlyPrecision := BotList[num].fly_precision;
    FDifficult.Cover := BotList[num].cover;
    FDifficult.CloseJump := BotList[num].close_jump;

    for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    begin
      FDifficult.WeaponPrior[a] := BotList[num].w_prior1[a];
      FDifficult.CloseWeaponPrior[a] := BotList[num].w_prior2[a];
      //FDifficult.SafeWeaponPrior[a] := BotList[num].w_prior3[a];
    end;

    if g_Game_IsNet then MH_SEND_PlayerCreate(UID);
  end;
end;

procedure g_Bot_RemoveAll();
var
  a: Integer;
begin
  if not g_Game_IsServer then Exit;
  if gPlayers = nil then Exit;

  for a := 0 to High(gPlayers) do
    if gPlayers[a] <> nil then
      if gPlayers[a] is TBot then
      begin
        gPlayers[a].Lives := 0;
        gPlayers[a].Kill(K_SIMPLEKILL, 0, 0);
        g_Player_Remove(gPlayers[a].FUID);
      end;
end;

procedure g_Player_Remove(UID: Word);
var
  i: Integer;
begin
  if gPlayers = nil then Exit;

  if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_PlayerDelete(UID);

  for i := 0 to High(gPlayers) do
    if gPlayers[i] <> nil then
      if gPlayers[i].FUID = UID then
      begin
        if gPlayers[i] is TPlayer then
          TPlayer(gPlayers[i]).Free()
        else
          TBot(gPlayers[i]).Free();
        gPlayers[i] := nil;
        Exit;
      end;
end;

procedure g_Player_Init();
var
  F: TextFile;
  s: String;
  a, b: Integer;
  config: TConfig;
  sa: SArray;
begin
  BotNames := nil;

  if not FileExists(DataDir + BOTNAMES_FILENAME) then
    Exit;

// Читаем возможные имена ботов из файла:
  AssignFile(F, DataDir + BOTNAMES_FILENAME);
  Reset(F);

  while not EOF(F) do
  begin
    ReadLn(F, s);

    s := Trim(s);
    if s = '' then
      Continue;

    SetLength(BotNames, Length(BotNames)+1);
    BotNames[High(BotNames)] := s;
  end;

  CloseFile(F);

// Перемешиваем их:
  if BotNames <> nil then
    for a := 0 to High(BotNames) do
    begin
      b := Random(Length(BotNames));
      s := BotNames[a];
      Botnames[a] := BotNames[b];
      BotNames[b] := s;
    end;

// Читаем файл с параметрами ботов:
  config := TConfig.CreateFile(DataDir + BOTLIST_FILENAME);
  BotList := nil;
  a := 0;

  while config.SectionExists(IntToStr(a)) do
  begin
    SetLength(BotList, Length(BotList)+1);

    with BotList[High(BotList)] do
    begin
    // Имя бота:
      name := config.ReadStr(IntToStr(a), 'name', '');
    // Модель:
      model := config.ReadStr(IntToStr(a), 'model', '');
    // Команда:
      if config.ReadStr(IntToStr(a), 'team', 'red') = 'red' then
        team := TEAM_RED
      else
        team := TEAM_BLUE;
    // Цвет модели:
      sa := parse(config.ReadStr(IntToStr(a), 'color', ''));
      color.R := StrToIntDef(sa[0], 0);
      color.G := StrToIntDef(sa[1], 0);
      color.B := StrToIntDef(sa[2], 0);
    // Вероятность стрельбы под углом:
      diag_fire := config.ReadInt(IntToStr(a), 'diag_fire', 0);
    // Вероятность ответного огня по невидимому сопернику:
      invis_fire := config.ReadInt(IntToStr(a), 'invis_fire', 0);
    // Точность стрельбы под углом:
      diag_precision := config.ReadInt(IntToStr(a), 'diag_precision', 0);
    // Точность стрельбы в полете:
      fly_precision := config.ReadInt(IntToStr(a), 'fly_precision', 0);
    // Точность уклонения от снарядов:
      cover := config.ReadInt(IntToStr(a), 'cover', 0);
    // Вероятность прыжка при приближении соперника:
      close_jump := config.ReadInt(IntToStr(a), 'close_jump', 0);
    // Приоритеты оружия для дальнего боя:
      sa := parse(config.ReadStr(IntToStr(a), 'w_prior1', ''));
      if Length(sa) = 10 then
        for b := 0 to 9 do
          w_prior1[b] := EnsureRange(StrToInt(sa[b]), 0, 9);
    // Приоритеты оружия для ближнего боя:
      sa := parse(config.ReadStr(IntToStr(a), 'w_prior2', ''));
      if Length(sa) = 10 then
        for b := 0 to 9 do
          w_prior2[b] := EnsureRange(StrToInt(sa[b]), 0, 9);

      {sa := parse(config.ReadStr(IntToStr(a), 'w_prior3', ''));
      if Length(sa) = 10 then
        for b := 0 to 9 do
          w_prior3[b] := EnsureRange(StrToInt(sa[b]), 0, 9);}
    end;

    a := a + 1;
  end;
 
  config.Free();
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
        if gPlayers[i] is TPlayer then
          TPlayer(gPlayers[i]).Free()
        else
          TBot(gPlayers[i]).Free();
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

  if gPlayers = nil then
    Exit;

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

  if gPlayers = nil then
    Exit;

  for a := 0 to High(gPlayers) do
    if gPlayers[a] <> nil then
      Result := Result + 1;
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
        Lives := gPlayers[a].FLives;
        Spectator := gPlayers[a].FSpectator;
      end;
    end;
end;

procedure g_Player_RememberAll;
var
  i: Integer;
begin
  for i := Low(gPlayers) to High(gPlayers) do
    if (gPlayers[i] <> nil) and gPlayers[i].Live then
      gPlayers[i].RememberState;
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

        if gPlayers[i] is TPlayer then
        begin
          if (not gPlayers[i].FSpectator) or gPlayers[i].FWantsInGame then
            gPlayers[i].Respawn(Silent)
          else
            gPlayers[i].Spectate;
        end
        else
          TBot(gPlayers[i]).Respawn(Silent);
      end;
end;

procedure g_Player_CreateCorpse(Player: TPlayer);
var
  find_id: DWORD;
  ok: Boolean;
begin
  if Player.Live then
    Exit;
  if Player.FObj.Y >= gMapInfo.Height+128 then
    Exit;

  with Player do
  begin
    if (FHealth >= -50) or (gGibsCount = 0) then
      begin
        if gCorpses = nil then
          Exit;

        ok := False;
        for find_id := 0 to High(gCorpses) do
          if gCorpses[find_id] = nil then
          begin
            ok := True;
            Break;
          end;

        if not ok then
          find_id := Random(Length(gCorpses));

        gCorpses[find_id] := TCorpse.Create(FObj.X, FObj.Y, FModel.Name, FHealth < -20);
        gCorpses[find_id].FColor := FModel.Color;
        gCorpses[find_id].FObj.Vel := FObj.Vel;
        gCorpses[find_id].FObj.Accel := FObj.Accel;
      end
    else
      g_Player_CreateGibs(FObj.X + PLAYER_RECT_CX,
                          FObj.Y + PLAYER_RECT_CY,
                          FModel.Name, FModel.Color);
  end;
end;

procedure g_Player_CreateShell(fX, fY, dX, dY: Integer; T: Byte);
var
  SID: DWORD;
begin
  with gShells[CurrentShell] do
  begin
    SpriteID := 0;
    g_Obj_Init(@Obj);
    Obj.Rect.X := 0;
    Obj.Rect.Y := 0;
    if T = SHELL_BULLET then
    begin
      if g_Texture_Get('TEXTURE_SHELL_BULLET', SID) then
        SpriteID := SID;
      CX := 2;
      CY := 1;
      Obj.Rect.Width := 4;
      Obj.Rect.Height := 2;
    end
    else
    begin
      if g_Texture_Get('TEXTURE_SHELL_SHELL', SID) then
        SpriteID := SID;
      CX := 4;
      CY := 2;
      Obj.Rect.Width := 7;
      Obj.Rect.Height := 3;
    end;
    SType := T;
    Live := True;
    Obj.X := fX;
    Obj.Y := fY;
    g_Obj_Push(@Obj, dX + Random(6)-Random(6), dY-Random(6));
    RAngle := 0;
    Timeout := gTime + SHELL_TIMEOUT;

    if CurrentShell >= High(gShells) then
      CurrentShell := 0
    else
      Inc(CurrentShell);
  end;
end;

procedure g_Player_CreateGibs(fX, fY: Integer; ModelName: string; fColor: TRGB);
var
  a: Integer;
  GibsArray: TGibsArray;
begin
  if gGibs = nil then
    Exit;
  if not g_PlayerModel_GetGibs(ModelName, GibsArray) then
    Exit;

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
      g_Obj_PushA(@Obj, 25 + Random(10), Random(361));
      RAngle := 0;

      if gBloodCount > 0 then
        g_GFX_Blood(fX, fY, 16*gBloodCount+Random(5*gBloodCount), -16+Random(33), -16+Random(33),
                    Random(48), Random(48));

      if CurrentGib >= High(gGibs) then
        CurrentGib := 0
      else
        CurrentGib := CurrentGib + 1;
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
          mr := g_Obj_Move(@Obj, True, False);

          if WordBool(mr and MOVE_FALLOUT) then
          begin
            Live := False;
            Continue;
          end;

        // Отлетает от удара о стену/потолок/пол:
          if WordBool(mr and MOVE_HITWALL) then
            Obj.Vel.X := -(vel.X div 2);
          if WordBool(mr and (MOVE_HITCEIL or MOVE_HITLAND)) then
            Obj.Vel.Y := -(vel.Y div 2);

          RAngle := RAngle + Obj.Vel.X*6 + Obj.Vel.Y;
          if RAngle > 360 then
            RAngle := RAngle mod 360;

        // Сопротивление воздуха для куска трупа:
          if gTime mod (GAME_TICK*3) = 0 then
            Obj.Vel.X := z_dec(Obj.Vel.X, 1);
        end;

  if gCorpses <> nil then
    for i := 0 to High(gCorpses) do
      if gCorpses[i] <> nil then
        if gCorpses[i].State = CORPSE_STATE_REMOVEME then
          begin
            gCorpses[i].Free();
            gCorpses[i] := nil;
          end
        else
          gCorpses[i].Update();

  if gShells <> nil then
    for i := 0 to High(gShells) do
      if gShells[i].Live then
        with gShells[i] do
        begin
          vel := Obj.Vel;
          mr := g_Obj_Move(@Obj, True, False);

          if WordBool(mr and MOVE_FALLOUT) or (gShells[i].Timeout < gTime) then
          begin
            Live := False;
            Continue;
          end;

        // Отлетает от удара о стену/потолок/пол:
          if WordBool(mr and MOVE_HITWALL) then
            Obj.Vel.X := -(vel.X div 2);
          if WordBool(mr and (MOVE_HITCEIL or MOVE_HITLAND)) then
          begin
            Obj.Vel.Y := -(vel.Y div 2);
            if Obj.Vel.X <> 0 then Obj.Vel.X := Obj.Vel.X div 2;
            if (RAngle <> 0) and (RAngle <> 180) and (Obj.Vel.X = 0) and (Obj.Vel.Y = 0) then
              RAngle := (RAngle div 180) * 180;
          end;

          RAngle := RAngle + Obj.Vel.X*8 + Obj.Vel.Y;
          if RAngle > 360 then
            RAngle := RAngle mod 360;
        end;
end;

procedure g_Player_DrawCorpses();
var
  i: Integer;
  a: TPoint;
begin
  if gGibs <> nil then
    for i := 0 to High(gGibs) do
      if gGibs[i].Live then
        with gGibs[i] do
        begin
          if not g_Obj_Collide(sX, sY, sWidth, sHeight, @Obj) then
            Continue;

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
      if gCorpses[i] <> nil then
        gCorpses[i].Draw();
end;

procedure g_Player_DrawShells();
var
  i: Integer;
  a: TPoint;
begin
  if gShells <> nil then
    for i := 0 to High(gShells) do
      if gShells[i].Live then
        with gShells[i] do
        begin
          if not g_Obj_Collide(sX, sY, sWidth, sHeight, @Obj) then
            Continue;

          a.X := CX;
          a.Y := CY;

          e_DrawAdv(SpriteID, Obj.X, Obj.Y, 0, True, False, RAngle, @a, M_NONE);
        end;
end;

procedure g_Player_RemoveAllCorpses();
var
  i: Integer;
begin
  gGibs := nil;
  gShells := nil;
  SetLength(gGibs, MaxGibs);
  SetLength(gShells, MaxGibs);
  CurrentGib := 0;
  CurrentShell := 0;
  
  if gCorpses <> nil then
    for i := 0 to High(gCorpses) do
      gCorpses[i].Free();

  gCorpses := nil;
  SetLength(gCorpses, MaxCorpses);
end;

procedure g_Player_Corpses_SaveState(var Mem: TBinMemoryWriter);
var
  count, i: Integer;
  b: Boolean;
begin
// Считаем количество существующих трупов:
  count := 0;
  if gCorpses <> nil then
    for i := 0 to High(gCorpses) do
      if gCorpses[i] <> nil then
        count := count + 1;

  Mem := TBinMemoryWriter.Create((count+1) * 128);

// Количество трупов:
  Mem.WriteInt(count);

  if count = 0 then
    Exit;

// Сохраняем трупы:
  for i := 0 to High(gCorpses) do
    if gCorpses[i] <> nil then
    begin
    // Название модели:
      Mem.WriteString(gCorpses[i].FModelName);
    // Тип смерти:
      b := gCorpses[i].Mess;
      Mem.WriteBoolean(b);
    // Сохраняем данные трупа:
      gCorpses[i].SaveState(Mem);
    end;
end;

procedure g_Player_Corpses_LoadState(var Mem: TBinMemoryReader);
var
  count, i: Integer;
  str: String;
  b: Boolean;
begin
  if Mem = nil then
    Exit;

  g_Player_RemoveAllCorpses();

// Количество трупов:
  Mem.ReadInt(count);

  if count > Length(gCorpses) then
  begin
    raise EBinSizeError.Create('g_Player_Corpses_LoadState: Too Many Corpses');
  end;

  if count = 0 then
    Exit;

// Загружаем трупы:
  for i := 0 to count-1 do
  begin
  // Название модели:
    Mem.ReadString(str);
  // Тип смерти:
    Mem.ReadBoolean(b);
  // Создаем труп:
    gCorpses[i] := TCorpse.Create(0, 0, str, b);
  // Загружаем данные трупа:
    gCorpses[i].LoadState(Mem);
  end;
end;

{ T P l a y e r : }

procedure TPlayer.BFGHit();
begin
  g_Weapon_BFGHit(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                  FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
  if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                   FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                   0, NET_GFX_BFG);
end;

procedure TPlayer.ChangeModel(ModelName: string);
var
  Model: TPlayerModel;
begin
  Model := g_PlayerModel_Get(ModelName);
  if Model = nil then Exit;

  FModel.Free();
  FModel := Model;
end;

procedure TPlayer.ChangeTeam;
begin
  if not (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then Exit;
  
  if g_Game_IsServer and gGameOn and FLive then
    Kill(K_SIMPLEKILL, FUID, 0);
  
  if FTeam = TEAM_RED then
  begin
    FTeam := TEAM_BLUE;
    FModel.Color := _RGB(0, 0, 255);
    g_Console_Add(Format(_lc[I_PLAYER_CHTEAM_BLUE], [FName]), True);
  end
  else
  begin
    FTeam := TEAM_RED;
    FModel.Color := _RGB(255, 0, 0);
    g_Console_Add(Format(_lc[I_PLAYER_CHTEAM_RED], [FName]), True);
  end;
end;

{
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

    if ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_INVUL] then
     g_Sound_PlayExAt('SOUND_ITEM_GETRULEZ', FObj.X, FObj.Y)
    else if ItemType in [ITEM_MEDKIT_SMALL, ITEM_MEDKIT_LARGE, ITEM_MEDKIT_BLACK] then
     g_Sound_PlayExAt('SOUND_ITEM_GETMED', FObj.X, FObj.Y)
    else g_Sound_PlayExAt('SOUND_ITEM_GETITEM', FObj.X, FObj.Y);

    // Надо убрать с карты, если это не ключ, которым нужно поделится с другим игроком:
    if r and not ((ItemType in [ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE]) and
                  (gGameSettings.GameType = GT_SINGLE) and
                  (g_Player_GetCount() > 1)) then
     if not Respawnable then g_Items_Remove(i) else g_Items_Pick(i);
   end;
  end;
end;
}

function TPlayer.CollideLevel(XInc, YInc: Integer): Boolean;
begin
  Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                               PLAYER_RECT.Width, PLAYER_RECT.Height, PANEL_WALL,
                               False);
end;

constructor TPlayer.Create();
begin
  FIamBot := False;
  FDummy := False;

  FSawSound := TPlayableSound.Create();
  FSawSoundIdle := TPlayableSound.Create();
  FSawSoundHit := TPlayableSound.Create();
  FSawSoundSelect := TPlayableSound.Create();
  FJetSoundFly := TPlayableSound.Create();
  FJetSoundOn := TPlayableSound.Create();
  FJetSoundOff := TPlayableSound.Create();

  FSawSound.SetByName('SOUND_WEAPON_FIRESAW');
  FSawSoundIdle.SetByName('SOUND_WEAPON_IDLESAW');
  FSawSoundHit.SetByName('SOUND_WEAPON_HITSAW');
  FSawSoundSelect.SetByName('SOUND_WEAPON_SELECTSAW');
  FJetSoundFly.SetByName('SOUND_PLAYER_JETFLY');
  FJetSoundOn.SetByName('SOUND_PLAYER_JETON');
  FJetSoundOff.SetByName('SOUND_PLAYER_JETOFF');

  FSpectatePlayer := -1;
  FClientID := -1;
  FSavedState.WaitRecall := False;

  FActualModelName := 'doomer';

  g_Obj_Init(@FObj);
  FObj.Rect := PLAYER_RECT;

  FBFGFireCounter := -1;
end;

procedure TPlayer.Damage(value: Word; SpawnerUID: Word; vx, vy: Integer; t: Byte);
var
  c: Word;
begin
  if not FLive then
    Exit;

// Нет повторного урона от той же ловушки:
  if (t = HIT_TRAP) and (t = FLastHit) then
    Exit;

  FLastHit := t;

// Неуязвимость не спасает от ловушек:
  if (t = HIT_TRAP) and (not FGodMode) then
  begin
    // Обнулить действия примочек, чтобы фон пропал
    FMegaRulez[MR_SUIT] := 0;
    FMegaRulez[MR_INVUL] := 0;
  end;

// Но от остального спасает:
  if FMegaRulez[MR_INVUL] > gTime then
    Exit;

// Чит-код "ГОРЕЦ":
  if FGodMode then
    Exit;

// Если есть урон своим, или ранил сам себя, или тебя ранил противник:
  if LongBool(gGameSettings.Options and GAME_OPTION_TEAMDAMAGE) or
     (SpawnerUID = FUID) or
     (not SameTeam(FUID, SpawnerUID)) then
  begin
    FLastSpawnerUID := SpawnerUID;

  // Кровь (пузырьки, если в воде):
    if gBloodCount > 0 then
    begin
      c := Min(value, 200)*gBloodCount + Random(Min(value, 200) div 2);
      if value div 4 <= c then
        c := c - (value div 4)
      else
        c := 0;

      if (t = HIT_SOME) and (vx = 0) and (vy = 0) then
        MakeBloodSimple(c)
      else
        case t of
          HIT_TRAP, HIT_ACID, HIT_FLAME: MakeBloodSimple(c);
          HIT_BFG, HIT_ROCKET, HIT_SOME: MakeBloodVector(c, vx, vy);
        end;

      if t = HIT_WATER then
        g_GFX_Bubbles(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                      FObj.Y+PLAYER_RECT.Y-4, value div 2, 8, 4);
    end;

  // Буфер урона:
    Inc(FDamageBuffer, value);

  // Вспышка боли:
    if gFlash then
      FPain := FPain + value;
  end;

  if g_Game_IsServer and g_Game_IsNet then
  begin
    MH_SEND_PlayerDamage(FUID, t, SpawnerUID, value, vx, vy);
    MH_SEND_PlayerStats(FUID);
    MH_SEND_PlayerPos(False, FUID);
  end;
end;

destructor TPlayer.Destroy();
begin
  FSawSound.Free();
  FSawSoundIdle.Free();
  FSawSoundHit.Free();
  FJetSoundFly.Free();
  FJetSoundOn.Free();
  FJetSoundOff.Free();
  FModel.Free();

  inherited;
end;

procedure TPlayer.Draw();
var                    
  bubX, bubY: Integer;
begin
  if FLive then
    if (FMegaRulez[MR_INVIS] > gTime)  then
    begin
      if (gPlayerDrawn <> nil) and ((Self = gPlayerDrawn) or
         ((FTeam = gPlayerDrawn.Team) and (gGameSettings.GameMode <> GM_DM))) then
        FModel.Draw(FObj.X, FObj.Y, 200)
      else
        FModel.Draw(FObj.X, FObj.Y, 254);
    end
    else
      FModel.Draw(FObj.X, FObj.Y);
      
  if g_debug_Frames then
  begin
    e_DrawQuad(FObj.X+FObj.Rect.X,
               FObj.Y+FObj.Rect.Y,
               FObj.X+FObj.Rect.X+FObj.Rect.Width-1,
               FObj.Y+FObj.Rect.Y+FObj.Rect.Height-1,
               0, 255, 0);
  end;

  if (FKeys[KEY_CHAT].Pressed) and not FGhost then
    if (gPlayer1 <> nil) then
    begin
      bubX := FObj.X+FObj.Rect.X - 11;
      bubY := FObj.Y+FObj.Rect.Y - 17;
      e_TextureFontPrint(bubX, bubY, '[...]', gStdFont);
    end;
 // e_DrawPoint(5, 335, 288, 255, 0, 0); // DL, UR, DL, UR
end;

procedure TPlayer.DrawGUI();
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

  if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
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

      s := IntToStr(p)+' / '+IntToStr(Length(stat))+' ';
      if Frags >= m then s := s+'+' else s := s+'-';
      s := s+IntToStr(Abs(Frags-m));

      e_CharFont_GetSize(gMenuSmallFont, s, tw, th);
      e_CharFont_PrintEx(gMenuSmallFont, X-16-tw, Y+32, s, _RGB(255, 0, 0));
    end;

    if gShowLives and (gGameSettings.MaxLives > 0) then
    begin
      s := IntToStr(Lives);
      e_CharFont_GetSize(gMenuFont, s, tw, th);
      e_CharFont_PrintEx(gMenuFont, X-16-tw, SY-32, s, _RGB(0, 255, 0));
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

  if FMegaRulez[MR_JET] > 0 then
  begin
    if g_Texture_Get('TEXTURE_PLAYER_HUDAIR', ID) then
      e_Draw(ID, X+2, Y+116, 0, True, False);
    if g_Texture_Get('TEXTURE_PLAYER_HUDJET', ID) then
      e_Draw(ID, X+2, Y+126, 0, True, False);
    e_DrawLine(4, X+16, Y+122, X+16+Trunc(168*IfThen(FAir > 0, FAir, 0)/AIR_MAX), Y+122, 0, 0, 196);
    e_DrawLine(4, X+16, Y+132, X+16+Trunc(168*(FMegaRulez[MR_JET]/JET_MAX)), Y+132, 208, 0, 0);
  end
  else
  begin
    if g_Texture_Get('TEXTURE_PLAYER_HUDAIR', ID) then
      e_Draw(ID, X+2, Y+124, 0, True, False);
    e_DrawLine(4, X+16, Y+130, X+16+Trunc(168*IfThen(FAir > 0, FAir, 0)/AIR_MAX), Y+130, 0, 0, 196);
  end;

 if g_Game_IsClient then
  begin
    s := 'Ping: ' + IntToStr(NetPeer.lastRoundTripTime);
    e_TextureFontPrint(X + 4, Y + 242, s, gStdFont);
    Y := Y + 16;
  end;

  if FSpectator then
  begin
    e_TextureFontPrint(X + 4, Y + 242, _lc[I_PLAYER_SPECT], gStdFont);
    e_TextureFontPrint(X + 4, Y + 258, _lc[I_PLAYER_SPECT2], gStdFont);
    e_TextureFontPrint(X + 4, Y + 274, _lc[I_PLAYER_SPECT1], gStdFont);
    if FNoRespawn then
    begin
      e_TextureFontGetSize(gStdFont, cw, ch);
      s := _lc[I_PLAYER_SPECT4];
      e_TextureFontPrintEx(gScreenWidth div 2 - cw*(Length(s) div 2),
                         gScreenHeight-4-ch, s, gStdFont, 255, 255, 255, 1, True);
      e_TextureFontPrint(X + 4, Y + 290, _lc[I_PLAYER_SPECT1S], gStdFont);
    end;

  end;
end;

procedure TPlayer.DrawRulez();
var
  dr: Boolean;
begin
  // При взятии неуязвимости рисуется инверсионный белый фон
  if FMegaRulez[MR_INVUL] >= gTime then
  begin
    if (FMegaRulez[MR_INVUL]-gTime) <= 2100 then
      dr := not Odd((FMegaRulez[MR_INVUL]-gTime) div 300)
    else
      dr := True;

    if dr then
      e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1,
                     191, 191, 191, 0, B_INVERT);
  end;

  // При взятии защитного костюма рисуется зеленоватый фон
  if FMegaRulez[MR_SUIT] >= gTime then
  begin
    if (FMegaRulez[MR_SUIT]-gTime) <= 2100 then
      dr := not Odd((FMegaRulez[MR_SUIT]-gTime) div 300)
    else
      dr := True;

    if dr then
      e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1,
                     0, 96, 0, 200, B_NONE);
  end;
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

 e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1, 255, 0, 0, 255-h*50);
 //e_DrawFillQuad(0, 0, gPlayerScreenSize.X-1, gPlayerScreenSize.Y-1, 255-min(128, a), 255-a, 255-a, 0, B_FILTER);
end;

procedure TPlayer.Fire();
const
  ft: array[WEAPON_KASTET..WEAPON_SUPERPULEMET] of Byte = (5, 2, 6, 18, 36, 2, 12, 2, 0, 2);
var
  f, DidFire: Boolean;
  wx, wy, xd, yd: Integer;
  obj: TObj;
begin
  if g_Game_IsClient then Exit;
// FBFGCounter - время перед выстрелом (для BFG)
// FReloading - время после выстрела (для всего)

  if FSpectator then
  begin
    Respawn(False);
    Exit;
  end;

  if FReloading[FCurrWeap] <> 0 then Exit;

  DidFire := False;

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
        obj.Vel.X := (xd-wx) div 2;
        obj.Vel.Y := (yd-wy) div 2;
        obj.Accel.X := xd-wx;
        obj.Accel.y := yd-wy;

        if g_Weapon_Hit(@obj, 50, FUID, HIT_SOME) <> 0 then
          g_Sound_PlayExAt('SOUND_WEAPON_HITBERSERK', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_WEAPON_MISSBERSERK', FObj.X, FObj.Y);

        if gFlash then
          if FPain < 50 then
            FPain := min(FPain + 25, 50);
      end else g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 3, FUID);

      DidFire := True;
      FReloading[FCurrWeap] := ft[FCurrWeap];
    end;

    WEAPON_SAW:
    begin
      if g_Weapon_chainsaw(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                           IfThen(gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF], 9, 3), FUID) <> 0 then
      begin
        FSawSoundSelect.Stop();
        FSawSound.Stop();
        FSawSoundHit.PlayAt(FObj.X, FObj.Y);
      end
      else if not FSawSoundHit.IsPlaying() then
      begin
        FSawSoundSelect.Stop();
        FSawSound.PlayAt(FObj.X, FObj.Y);
      end;

      FReloading[FCurrWeap] := ft[FCurrWeap];
      DidFire := True;
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
        DidFire := True;
        g_Player_CreateShell(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                             GameVelX, GameVelY-3, SHELL_BULLET);
      end;

    WEAPON_SHOTGUN1:
      if FAmmo[A_SHELLS] > 0 then
      begin
        g_Weapon_shotgun(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := ft[FCurrWeap];
        Dec(FAmmo[A_SHELLS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        g_Player_CreateShell(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                             GameVelX, GameVelY-3, SHELL_SHELL);
      end;

    WEAPON_SHOTGUN2:
      if FAmmo[A_SHELLS] >= 2 then
      begin
        g_Weapon_dshotgun(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := ft[FCurrWeap];
        Dec(FAmmo[A_SHELLS], 2);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        g_Player_CreateShell(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                             GameVelX, GameVelY-3, SHELL_SHELL);
        g_Player_CreateShell(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                             GameVelX, GameVelY-3, SHELL_SHELL);
      end;

    WEAPON_CHAINGUN:
      if FAmmo[A_BULLETS] > 0 then
      begin
        g_Weapon_mgun(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := ft[FCurrWeap];
        Dec(FAmmo[A_BULLETS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        g_Player_CreateShell(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                             GameVelX, GameVelY-3, SHELL_BULLET);
      end;

    WEAPON_ROCKETLAUNCHER:
      if FAmmo[A_ROCKETS] > 0 then
      begin
        g_Weapon_rocket(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := ft[FCurrWeap];
        Dec(FAmmo[A_ROCKETS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
      end;

    WEAPON_PLASMA:
      if FAmmo[A_CELLS] > 0 then
      begin
        g_Weapon_plasma(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := ft[FCurrWeap];
        Dec(FAmmo[A_CELLS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
      end;

    WEAPON_BFG:
      if (FAmmo[A_CELLS] >= 40) and (FBFGFireCounter = -1) then
      begin
        FBFGFireCounter := 21;
        g_Sound_PlayExAt('SOUND_WEAPON_STARTFIREBFG', FObj.X, FObj.Y);
        Dec(FAmmo[A_CELLS], 40);
        DidFire := True;
      end;

    WEAPON_SUPERPULEMET:
      if FAmmo[A_SHELLS] > 0 then
      begin
        g_Weapon_shotgun(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := ft[FCurrWeap];
        Dec(FAmmo[A_SHELLS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        g_Player_CreateShell(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                             GameVelX, GameVelY-3, SHELL_SHELL);
      end;
  end;

  if g_Game_IsNet then
  begin
    if DidFire then
    begin
      if FCurrWeap <> WEAPON_BFG then
        MH_SEND_PlayerFire(FUID, FCurrWeap, wx, wy, xd, yd, LastShotID)
      else
        MH_SEND_Sound(FObj.X, FObj.Y, 'SOUND_WEAPON_STARTFIREBFG');
    end;
  
    MH_SEND_PlayerStats(FUID);
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
                               PANEL_WATER or PANEL_ACID1 or PANEL_ACID2, True);
end;

procedure TPlayer.JetpackOn;
begin
  FJetSoundFly.Stop;
  FJetSoundOff.Stop;
  FJetSoundOn.SetPosition(0);
  FJetSoundOn.PlayAt(FObj.X, FObj.Y);
  FlySmoke(8);
end;

procedure TPlayer.JetpackOff;
begin
  FJetSoundFly.Stop;
  FJetSoundOn.Stop;
  FJetSoundOff.SetPosition(0);
  FJetSoundOff.PlayAt(FObj.X, FObj.Y);
end;

procedure TPlayer.Jump();
begin
  if gFly or FJetpack then
  begin
    // Полет (чит-код или джетпак):
    if FObj.Vel.Y > -VEL_FLY then FObj.Vel.Y := FObj.Vel.Y - 3;
    if FJetpack then
    begin
      if FMegaRulez[MR_JET] > 0 then
        Dec(FMegaRulez[MR_JET]);
      if (FMegaRulez[MR_JET] < 1) and g_Game_IsServer then
      begin
        FJetpack := False;
        JetpackOff;
        if g_Game_IsNet then
          MH_SEND_PlayerStats(FUID);
      end;
    end;
    Exit;
  end;

// Прыгаем или всплываем:
  if CollideLevel(0, 1) or
    g_Map_CollidePanel(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y+36, PLAYER_RECT.Width,
                       PLAYER_RECT.Height-33, PANEL_STEP, False) then
  begin
    FObj.Vel.Y := -VEL_JUMP;
    FCanJetpack := False;
  end
  else
  begin
    if BodyInLiquid(0, 0) then
      FObj.Vel.Y := -VEL_SW
    else if (FMegaRulez[MR_JET] > 0) and FCanJetpack and g_Game_IsServer then
    begin
      FJetpack := True;
      JetpackOn;
      if g_Game_IsNet then
        MH_SEND_PlayerStats(FUID);
    end;
  end;
end;

procedure TPlayer.Kill(KillType: Byte; SpawnerUID: Word; t: Byte);
var
  a, i, k, ab, ar: Byte;
  s: String;
  mon: TMonster;
  plr: TPlayer;
  srv, netsrv: Boolean;
  dofrags, OldLR: Boolean;
  KP: TPlayer;

  procedure PushItem(t: Byte);
  var
    id: DWORD;
  begin
    id := g_Items_Create(FObj.X, FObj.Y, t, True, False);
    if KillType = K_EXTRAHARDKILL then // -7..+7; -8..0
      g_Obj_Push(@gItems[id].Obj, (FObj.Vel.X div 2)-7+Random(15),
                                  (FObj.Vel.Y div 2)-Random(9))
    else
      if KillType = K_HARDKILL then // -5..+5; -5..0
        g_Obj_Push(@gItems[id].Obj, (FObj.Vel.X div 2)-5+Random(11),
                                    (FObj.Vel.Y div 2)-Random(6))
      else // -3..+3; -3..0
        g_Obj_Push(@gItems[id].Obj, (FObj.Vel.X div 2)-3+Random(7),
                                    (FObj.Vel.Y div 2)-Random(4));

    if g_Game_IsNet and g_Game_IsServer then
      MH_SEND_ItemSpawn(True, id);
  end;

begin
  dofrags := (gGameSettings.MaxLives = 0) or (gGameSettings.GameMode = GM_COOP);
  Srv := g_Game_IsServer;
  Netsrv := g_Game_IsServer and g_Game_IsNet;
  if Srv then FDeath := FDeath + 1;
  FLive := False;

  if (gGameSettings.MaxLives > 0) and Srv and (not gLMSRespawn) then
  begin
    if FLives > 0 then FLives := FLives - 1;
    if FLives = 0 then FNoRespawn := True;
  end;

// Номер типа смерти:     
  a := 1;
  case KillType of
    K_SIMPLEKILL:    a := 1;
    K_HARDKILL:      a := 2;
    K_EXTRAHARDKILL: a := 3;
    K_FALLKILL:      a := 4;
  end;

// Звук смерти:
  if not FModel.PlaySound(MODELSOUND_DIE, a, FObj.X, FObj.Y) then
    for i := 1 to 3 do
      if FModel.PlaySound(MODELSOUND_DIE, i, FObj.X, FObj.Y) then
        Break;

// Время респауна:
  if Srv then
    case KillType of
      K_SIMPLEKILL:
        FTime[T_RESPAWN] := gTime + TIME_RESPAWN1;
      K_HARDKILL:
        FTime[T_RESPAWN] := gTime + TIME_RESPAWN2;
      K_EXTRAHARDKILL, K_FALLKILL:
        FTime[T_RESPAWN] := gTime+  TIME_RESPAWN3;
    end;

// Переключаем состояние:
  case KillType of
    K_SIMPLEKILL:
      SetAction(A_DIE1);
    K_HARDKILL, K_EXTRAHARDKILL:
      SetAction(A_DIE2);
  end;

// Реакция монстров на смерть игрока:
  if (KillType <> K_FALLKILL) and (Srv) then
    g_Monsters_killedp();

// Особые типы смерти:
  case t of
    HIT_FALL: g_Console_Add(Format(_lc[I_PLAYER_KILL_FALL], [FName]), True);
    HIT_WATER: g_Console_Add(Format(_lc[I_PLAYER_KILL_WATER], [FName]), True);
    HIT_ACID: g_Console_Add(Format(_lc[I_PLAYER_KILL_ACID], [FName]), True);
    HIT_TRAP: g_Console_Add(Format(_lc[I_PLAYER_KILL_TRAP], [FName]), True);
  end;

  if SpawnerUID = FUID then
    begin // Самоубился
      if Srv and (DoFrags or (gGameSettings.GameMode = GM_TDM)) then
        FFrags := FFrags - 1;
      g_Console_Add(Format(_lc[I_PLAYER_KILL_SELF], [FName]), True);
    end
  else
    if g_GetUIDType(SpawnerUID) = UID_PLAYER then
      begin // Убит другим игроком
        KP := g_Player_Get(SpawnerUID);
        if (KP <> nil) and Srv then
        begin
          if (DoFrags or (gGameSettings.GameMode = GM_TDM)) then
            Inc(KP.FFrags,
              IfThen(SameTeam(FUID, SpawnerUID), -1, 1));

          if (gGameSettings.GameMode = GM_TDM) and DoFrags then
            Inc(gTeamStat[KP.Team].Goals,
              IfThen(SameTeam(FUID, SpawnerUID), -1, 1));

          if netsrv then MH_SEND_PlayerStats(SpawnerUID);
        end;

        plr := g_Player_Get(SpawnerUID);
        if plr = nil then
          s := '?'
        else
          s := plr.FName;

        if KillType = K_EXTRAHARDKILL then
          case Random(2) of
            0: g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_1],
                                    [FName, s]),
                             gShowKillMsg);
            1: g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_2],
                                    [FName, s]),
                             gShowKillMsg);
          end
        else
          g_Console_Add(Format(_lc[I_PLAYER_KILL],
                               [FName, s]),
                        gShowKillMsg);
      end
    else
      if g_GetUIDType(SpawnerUID) = UID_MONSTER then
      begin // Убит монстром
        mon := g_Monsters_Get(SpawnerUID);
        if mon = nil then
          s := '?'
        else
          s := g_Monsters_GetKilledBy(mon.MonsterType);

        if KillType = K_EXTRAHARDKILL then
          case Random(2) of
            0: g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_1],
                                    [FName, s]),
                             gShowKillMsg);
            1: g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_2],
                                    [FName, s]),
                             gShowKillMsg);
          end
        else
          g_Console_Add(Format(_lc[I_PLAYER_KILL],
                               [FName, s]),
                        gShowKillMsg);
      end;


  if Srv then
  begin
// Выброс оружия:
    for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
      if FWeapon[a] then
      begin
        case a of
          WEAPON_SAW: i := ITEM_WEAPON_SAW;
          WEAPON_SHOTGUN1: i := ITEM_WEAPON_SHOTGUN1;
          WEAPON_SHOTGUN2: i := ITEM_WEAPON_SHOTGUN2;
          WEAPON_CHAINGUN: i := ITEM_WEAPON_CHAINGUN;
          WEAPON_ROCKETLAUNCHER: i := ITEM_WEAPON_ROCKETLAUNCHER;
          WEAPON_PLASMA: i := ITEM_WEAPON_PLASMA;
          WEAPON_BFG: i := ITEM_WEAPON_BFG;
          WEAPON_SUPERPULEMET: i := ITEM_WEAPON_SUPERPULEMET;
          else i := 0;
        end;

        if i <> 0 then
          PushItem(i);
      end;

// Выброс рюкзака:
    if R_ITEM_BACKPACK in FRulez then
      PushItem(ITEM_AMMO_BACKPACK);

// Выброс ракетного ранца:
    if FMegaRulez[MR_JET] > 0 then
      PushItem(ITEM_JETPACK);

// Выброс ключей:
    if not (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]) then
    begin
      if R_KEY_RED in FRulez then
        PushItem(ITEM_KEY_RED);

      if R_KEY_GREEN in FRulez then
        PushItem(ITEM_KEY_GREEN);

      if R_KEY_BLUE in FRulez then
        PushItem(ITEM_KEY_BLUE);
    end;

// Выброс флага:
    if FFlag <> FLAG_NONE then
    begin
      with gFlags[FFlag] do
      begin
        Obj.X := FObj.X;
        Obj.Y := FObj.Y;
        Direction := FDirection;
        State := FLAG_STATE_DROPPED;
        Count := FLAG_TIME;
        g_Obj_Push(@Obj, (FObj.Vel.X div 2)-2+Random(5),
                         (FObj.Vel.Y div 2)-2+Random(5));

        if FFlag = FLAG_RED then
          s := _lc[I_PLAYER_FLAG_RED]
        else
          s := _lc[I_PLAYER_FLAG_BLUE];

        g_Console_Add(Format(_lc[I_PLAYER_FLAG_DROP], [FName, s]), True);
        g_Game_Message(Format(_lc[I_MESSAGE_FLAG_DROP], [AnsiUpperCase(s)]), 144);

        if Netsrv then MH_SEND_FlagEvent(FLAG_STATE_DROPPED, Flag, FUID, False);
      end;
    end;

  end;

  g_Player_CreateCorpse(Self);

  if Srv and (gGameSettings.MaxLives > 0) and FNoRespawn and (not gLMSRespawn) then
  begin
    a := 0;
    k := 0;
    ar := 0;
    ab := 0;
    for i := Low(gPlayers) to High(gPlayers) do
    begin
      if gPlayers[i] = nil then continue;
      if (not gPlayers[i].FNoRespawn) and (not gPlayers[i].FSpectator) then
      begin
        Inc(a);
        if gPlayers[i].FTeam = TEAM_RED then Inc(ar)
        else if gPlayers[i].FTeam = TEAM_BLUE then Inc(ab);
        k := i;
      end;
    end;

    OldLR := gLMSRespawn;
    if (gGameSettings.GameMode = GM_COOP) then
    begin
      if (a = 0) then
      begin
        // everyone is dead, restart the map
        g_Game_Message(_lc[I_MESSAGE_LMS_LOSE], 144);
        if Netsrv then MH_SEND_GameEvent(NET_EV_LMS_LOSE, 'N');
        gLMSRespawn := True;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (a = 1) then
      begin
        if (gPlayers[k] <> nil) and not (gPlayers[k] is TBot) then
          if (gPlayers[k].FPlayerNum = PLAYERNUM_1) or
             (gPlayers[k].FPlayerNum = PLAYERNUM_2) then
             g_Console_Add('*** ' + _lc[I_MESSAGE_LMS_SURVIVOR] + ' ***', True)
          else if Netsrv and (gPlayers[k].FClientID >= 0) then
             MH_SEND_GameEvent(NET_EV_LMS_SURVIVOR, 'N', gPlayers[k].FClientID);
      end;
    end
    else if (gGameSettings.GameMode = GM_TDM) then
    begin
      if (ab = 0) and (ar <> 0) then
      begin
        // blu team ded
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 144);
        if Netsrv then MH_SEND_GameEvent(NET_EV_TLMS_WIN, 'r');
        Inc(gTeamStat[TEAM_RED].Goals);
        gLMSRespawn := True;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (ar = 0) and (ab <> 0) then
      begin
        // red team ded
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 144);
        if Netsrv then MH_SEND_GameEvent(NET_EV_TLMS_WIN, 'b');
        Inc(gTeamStat[TEAM_BLUE].Goals);
        gLMSRespawn := True;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (ar = 0) and (ab = 0) then
      begin
        // everyone ded
        g_Game_Message(_lc[I_GAME_WIN_DRAW], 144);
        if Netsrv then MH_SEND_GameEvent(NET_EV_LMS_DRAW, FName);
        gLMSRespawn := True;
        gLMSRespawnTime := gTime + 5000;
      end;
    end
    else if (gGameSettings.GameMode = GM_DM) then
    begin
      if (a = 1) then
      begin
        if gPlayers[k] <> nil then
          with gPlayers[k] do
          begin
            // survivor is the winner
            g_Game_Message(Format(_lc[I_MESSAGE_LMS_WIN], [AnsiUpperCase(FName)]), 144);
            if Netsrv then MH_SEND_GameEvent(NET_EV_LMS_WIN, FName);
            Inc(FFrags);
          end;
        gLMSRespawn := True;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (a = 0) then
      begin
        // everyone is dead, restart the map
        g_Game_Message(_lc[I_GAME_WIN_DRAW], 144);
        if Netsrv then MH_SEND_GameEvent(NET_EV_LMS_DRAW, FName);
        gLMSRespawn := True;
        gLMSRespawnTime := gTime + 5000;
      end;
    end;
    if srv and not OldLR and gLMSRespawn then
    begin
      if NetMode = NET_SERVER then
        MH_SEND_Chat(IntToStr((gLMSRespawnTime - gTime) div 1000) +
                     _lc[I_PLAYER_SPECT5])
      else
        g_Console_Add(IntToStr((gLMSRespawnTime - gTime) div 1000) +
                      _lc[I_PLAYER_SPECT5], True);
    end;
  end;

  if Netsrv then
  begin
    MH_SEND_PlayerStats(FUID);
    MH_SEND_PlayerDeath(FUID, KillType, t, SpawnerUID);
    if gGameSettings.GameMode = GM_TDM then MH_SEND_GameStats;
  end;

  if srv and FNoRespawn then Spectate(True);
  FWantsInGame := True;
end;

function TPlayer.BodyInLiquid(XInc, YInc: Integer): Boolean;
begin
  Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc, PLAYER_RECT.Width,
                               PLAYER_RECT.Height-20, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2, False);
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
  if g_Game_IsClient then Exit;
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

  if FCurrWeap = WEAPON_SAW then
    FSawSoundSelect.PlayAt(FObj.X, FObj.Y);

  FModel.SetWeapon(FCurrWeap);
  if g_Game_IsNet then MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.SetWeapon(W: Byte);
begin
  if FCurrWeap <> W then
    if W = WEAPON_SAW then
      FSawSoundSelect.PlayAt(FObj.X, FObj.Y);

  FCurrWeap := W;
  FModel.SetWeapon(CurrWeap);
end;

function TPlayer.PickItem(ItemType: Byte; respawn: Boolean; var remove: Boolean): Boolean;
var
  a: Boolean;
begin
  Result := False;
  if g_Game_IsClient then Exit;

  // a = true - место спавна предмета:
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
      if FArmor < 200 then
      begin
        FArmor := 200;
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
        if FHealth < 200 then
          FHealth := 200;
        if FArmor < 200 then
          FArmor := 200;
        Result := True;
        remove := True;
      end;

    ITEM_WEAPON_SAW:
      if (not FWeapon[WEAPON_SAW]) or ((not respawn) and (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF])) then
      begin
        FWeapon[WEAPON_SAW] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_SHOTGUN1:
      if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SHOTGUN1] then
      begin
        // Нужно, чтобы не взять все пули сразу:
        if a and FWeapon[WEAPON_SHOTGUN1] then Exit;

        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        FWeapon[WEAPON_SHOTGUN1] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_SHOTGUN2:
      if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SHOTGUN2] then
      begin
        if a and FWeapon[WEAPON_SHOTGUN2] then Exit;

        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        FWeapon[WEAPON_SHOTGUN2] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_CHAINGUN:
      if (FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS]) or not FWeapon[WEAPON_CHAINGUN] then
      begin
        if a and FWeapon[WEAPON_CHAINGUN] then Exit;

        IncMax(FAmmo[A_BULLETS], 50, FMaxAmmo[A_BULLETS]);
        FWeapon[WEAPON_CHAINGUN] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_ROCKETLAUNCHER:
      if (FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS]) or not FWeapon[WEAPON_ROCKETLAUNCHER] then
      begin
        if a and FWeapon[WEAPON_ROCKETLAUNCHER] then Exit;

        IncMax(FAmmo[A_ROCKETS], 2, FMaxAmmo[A_ROCKETS]);
        FWeapon[WEAPON_ROCKETLAUNCHER] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_PLASMA:
      if (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or not FWeapon[WEAPON_PLASMA] then
      begin
        if a and FWeapon[WEAPON_PLASMA] then Exit;

        IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
        FWeapon[WEAPON_PLASMA] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_BFG:
      if (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or not FWeapon[WEAPON_BFG] then
      begin
        if a and FWeapon[WEAPON_BFG] then Exit;

        IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
        FWeapon[WEAPON_BFG] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_WEAPON_SUPERPULEMET:
      if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SUPERPULEMET] then
      begin
        if a and FWeapon[WEAPON_SUPERPULEMET] then Exit;

        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        FWeapon[WEAPON_SUPERPULEMET] := True;
        Result := True;
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
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
        remove := (gGameSettings.GameMode <> GM_COOP) and (g_Player_GetCount() < 2);
        if (not remove) and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETMED');
      end;

    ITEM_KEY_GREEN:
      if not(R_KEY_GREEN in FRulez) then
      begin
        Include(FRulez, R_KEY_GREEN);
        Result := True;
        remove := (gGameSettings.GameMode <> GM_COOP) and (g_Player_GetCount() < 2);
        if (not remove) and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETMED');
      end;

    ITEM_KEY_BLUE:
      if not(R_KEY_BLUE in FRulez) then
      begin
        Include(FRulez, R_KEY_BLUE);
        Result := True;
        remove := (gGameSettings.GameMode <> GM_COOP) and (g_Player_GetCount() < 2);
        if (not remove) and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETMED');
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

    ITEM_INVUL:
      if FMegaRulez[MR_INVUL] < gTime+PLAYER_INVUL_TIME then
      begin
        FMegaRulez[MR_INVUL] := gTime+PLAYER_INVUL_TIME;
        Result := True;
        remove := True;
      end;

    ITEM_BOTTLE:
      if FHealth < 200 then
      begin
        IncMax(FHealth, 4, 200);
        Result := True;
        remove := True;
      end;

    ITEM_HELMET:
      if FArmor < 200 then
      begin
        IncMax(FArmor, 5, 200);
        Result := True;
        remove := True;
      end;

    ITEM_JETPACK:
      if FMegaRulez[MR_JET] < JET_MAX then
      begin
        FMegaRulez[MR_JET] := JET_MAX;
        Result := True;
        remove := True;
      end;

    ITEM_INVIS:
      if FMegaRulez[MR_INVIS] < gTime then
      begin
        FMegaRulez[MR_INVIS] := gTime+PLAYER_INVIS_TIME;
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
  if g_Game_IsClient then Exit;
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

  if FCurrWeap = WEAPON_SAW then
    FSawSoundSelect.PlayAt(FObj.X, FObj.Y);

  FModel.SetWeapon(FCurrWeap);

  if g_Game_IsNet then MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.Push(vx, vy: Integer);
begin
  FObj.Accel.X := FObj.Accel.X + vx;
  FObj.Accel.Y := FObj.Accel.Y + vy;
  if g_Game_IsNet and g_Game_IsServer then
    MH_SEND_PlayerPos(True, FUID, NET_EVERYONE);
end;

procedure TPlayer.Reset(Force: Boolean);
begin
  if Force then
    FLive := False;

  FTime[T_RESPAWN] := 0;
  FGodMode := False;
  FNoTarget := False;
  FFrags := 0;
  FKills := 0;
  FMonsterKills := 0;
  FDeath := 0;
  FSecrets := 0;
  if FNoRespawn then
  begin
    FSpectator := False;
    FGhost := False;
    FPhysics := True;
    FSpectatePlayer := -1;
    FNoRespawn := False;
  end;
  FLives := gGameSettings.MaxLives;

  FFlag := 0;
  FModel.SetFlag(FLAG_NONE);
end;

procedure TPlayer.SoftReset();
begin
  ReleaseKeys();
  FModel.SetFlag(FLAG_NONE);
  
  FDamageBuffer := 0;
  FIncCam := 0;
  FBFGFireCounter := -1;
  FFlag := 0;
  FPain := 0;
  FLastHit := 0;

  SetAction(A_STAND, True);
end;

procedure TPlayer.Respawn(Silent: Boolean; Force: Boolean = False);
var
  RespawnPoint: TRespawnPoint;
  a, b, c: Byte;
  Anim: TAnimation;
  ID: DWORD;
begin
  if not g_Game_IsServer then Exit;
  if FDummy then Exit;
  FWantsInGame := True;
  if Force then
  begin
    FTime[T_RESPAWN] := 0;
    FLive := False;
  end;
  // if server changes MaxLives we gotta be ready
  if gGameSettings.MaxLives = 0 then FNoRespawn := False;

  if (gGameSettings.GameType <> GT_SINGLE) and (gGameSettings.GameMode <> GM_COOP) then
    begin // "Своя игра"
    // Еще нельзя возродиться:
      if FTime[T_RESPAWN] > gTime then
        Exit;

    // Просрал все жизни:
      if FNoRespawn then
      begin
        if not FSpectator then Spectate(True);
        FWantsInGame := True;
        Exit;
      end;

      if gGameSettings.GameMode = GM_DM then
        begin // DM ...
        // Точка возрождения DM:
          c := RESPAWNPOINT_DM;

          if g_Map_GetPointCount(c) = 0 then
          begin // Точек возрождения DM нет
          // Пробуем точку начала одиночной игры:
            if Random(2) = 0 then
              c := RESPAWNPOINT_PLAYER1
            else
              c := RESPAWNPOINT_PLAYER2;

            if g_Map_GetPointCount(c) = 0 then
            begin // Этой точки нет
            // Пробуем противоположную:
              if c = RESPAWNPOINT_PLAYER1 then
                c := RESPAWNPOINT_PLAYER2
              else
                c := RESPAWNPOINT_PLAYER1;

              if g_Map_GetPointCount(c) = 0 then
              begin // Точек игроков нет
              // Пробуем командную точку:
                if Random(2) = 0 then
                  c := RESPAWNPOINT_RED
                else
                  c := RESPAWNPOINT_BLUE;

                if g_Map_GetPointCount(c) = 0 then
                begin // Этой точки нет
                // Пробуем другую команду:
                  if c = RESPAWNPOINT_RED then
                    c := RESPAWNPOINT_BLUE
                  else
                    c := RESPAWNPOINT_RED;
                end;
              end;
            end;
          end;
        end // ... DM
      else
        begin // TDM / CTF ...
        // Командная точка возрождения:
          if FTeam = TEAM_RED then
            c := RESPAWNPOINT_RED
          else
            c := RESPAWNPOINT_BLUE;

          if g_Map_GetPointCount(c) = 0 then
          begin // Точки этой команды нет
          // Пробуем другую команду:
            if c = RESPAWNPOINT_RED then
              c := RESPAWNPOINT_BLUE
            else
              c := RESPAWNPOINT_RED;

            if g_Map_GetPointCount(c) = 0 then
            begin // Командных точек возрождения нет
            // Пробуем точку возрождения DM:
              c := RESPAWNPOINT_DM;

              if g_Map_GetPointCount(c) = 0 then
              begin // Точек возрождения DM нет
              // Пробуем точку начала одиночной игры:
                if Random(2) = 0 then
                  c := RESPAWNPOINT_PLAYER1
                else
                  c := RESPAWNPOINT_PLAYER2;

                if g_Map_GetPointCount(c) = 0 then
                begin // Этой точки нет
                // Пробуем противоположную:
                  if c = RESPAWNPOINT_PLAYER1 then
                    c := RESPAWNPOINT_PLAYER2
                  else
                    c := RESPAWNPOINT_PLAYER1;
                end;
              end;
            end;
          end;
        end; // ... TDM / CTF

    // Берсерк не сохраняется между уровнями:
      FRulez := FRulez-[R_BERSERK];
    end
  else // "Одиночная игра"/"Кооп"
    begin
    // Еще нельзя возродиться:
      if FTime[T_RESPAWN] > gTime then
        Exit;

    // Просрал все жизни:
      if FNoRespawn then
      begin
        if not FSpectator then Spectate(True);
        FWantsInGame := True;
        Exit;
      end;

    // Точка появления игрока:
      if (FPlayerNum = PLAYERNUM_1) and (gGameSettings.GameMode <> GM_COOP) then
        c := RESPAWNPOINT_PLAYER1
      else
        if (FPlayerNum = PLAYERNUM_2) and (gGameSettings.GameMode <> GM_COOP) then
          c := RESPAWNPOINT_PLAYER2
        else
        begin
          if gLastSpawnUsed = RESPAWNPOINT_PLAYER1 then // Боты или кооп
            c := RESPAWNPOINT_PLAYER2
          else
            c := RESPAWNPOINT_PLAYER1;
        end;

      if g_Map_GetPointCount(c) = 0 then
      begin // Правильной точки появления нет
      // Пробуем противоположную точку:
        if c = RESPAWNPOINT_PLAYER1 then
          c := RESPAWNPOINT_PLAYER2
        else
          c := RESPAWNPOINT_PLAYER1;

        if g_Map_GetPointCount(c) = 0 then
        begin // Противоположной точки тоже нет
        // Остается только точка DM:
          c := RESPAWNPOINT_DM;

          if g_Map_GetPointCount(c) = 0 then
          begin // Точек возрождения DM нет
          // Пробуем командную точку:
            if Random(2) = 0 then
              c := RESPAWNPOINT_RED
            else
              c := RESPAWNPOINT_BLUE;

            if g_Map_GetPointCount(c) = 0 then
            begin // Этой точки нет
            // Пробуем другую команду:
              if c = RESPAWNPOINT_RED then
                c := RESPAWNPOINT_BLUE
              else
                c := RESPAWNPOINT_RED;
            end;
          end;
        end;
      end;

      gLastSpawnUsed := c;

    // Берсерк и ключи не сохраняются между уровнями:
      FRulez := FRulez-[R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE, R_BERSERK];
    end;

  ReleaseKeys();
  FModel.SetFlag(FLAG_NONE);

// Воскрешение без оружия:
  if not FLive then
  begin
    FHealth := 100;
    FArmor := 0;
    FLive := True;
    FAir := AIR_DEF;

    for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    begin
      FWeapon[a] := False;
      FReloading[a] := 0;
    end;

    FWeapon[WEAPON_PISTOL] := True;
    FWeapon[WEAPON_KASTET] := True;
    FCurrWeap := WEAPON_PISTOL;

    FModel.SetWeapon(FCurrWeap);

    for b := A_BULLETS to A_CELLS do
      FAmmo[b] := 0;

    FAmmo[A_BULLETS] := 50;

    FMaxAmmo[A_BULLETS] := 200;
    FMaxAmmo[A_SHELLS] := 50;
    FMaxAmmo[A_ROCKETS] := 50;
    FMaxAmmo[A_CELLS] := 300;

    if gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF] then
      FRulez := [R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE]
    else
      FRulez := [];
  end;

// Получаем координаты точки возрождения:
  if not g_Map_GetPoint(c, RespawnPoint) then
  begin
    g_FatalError(_lc[I_GAME_ERROR_GET_SPAWN]);
    Exit;
  end;

// Установка координат и сброс всех параметров:
  FObj.X := RespawnPoint.X-PLAYER_RECT.X;
  FObj.Y := RespawnPoint.Y-PLAYER_RECT.Y;
  FObj.Vel.X := 0;
  FObj.Vel.Y := 0;
  FObj.Accel.X := 0;
  FObj.Accel.Y := 0;

  FDirection := RespawnPoint.Direction;
  if FDirection = D_LEFT then
    FAngle := 180
  else
    FAngle := 0;

  FIncCam := 0;
  FBFGFireCounter := -1;
  FFlag := 0;
  FPain := 0;
  FLastHit := 0;

  SetAction(A_STAND, True);
  FModel.Direction := FDirection;

  for a := Low(FTime) to High(FTime) do
    FTime[a] := 0;

  for a := Low(FMegaRulez) to High(FMegaRulez) do
    FMegaRulez[a] := 0;

  FDamageBuffer := 0;
  FJetpack := False;
  FCanJetpack := False;

// Анимация возрождения:
  if (not gLoadGameMode) and (not Silent) then
    if g_Frames_Get(ID, 'FRAMES_TELEPORT') then
    begin
      Anim := TAnimation.Create(ID, False, 3);
      g_GFX_OnceAnim(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                     FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, Anim);
      Anim.Free();
    end;

  FSpectator := False;
  FGhost := False;
  FPhysics := True;
  FSpectatePlayer := -1;

  if g_Game_IsNet then
  begin
    MH_SEND_PlayerPos(True, FUID, NET_EVERYONE);
    MH_SEND_PlayerStats(FUID, NET_EVERYONE);
    if not Silent then MH_SEND_Effect(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                                      FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32,
                                      0, NET_GFX_SPAWN);
  end;
end;

procedure TPlayer.Spectate(NoMove: Boolean = False);
var
  c: Integer;
  RespawnPoint: TRespawnPoint;
begin
  if FLive then
    Kill(K_EXTRAHARDKILL, FUID, HIT_SOME)
  else if (not NoMove) then
  begin
    // Точка возрождения DM:
    c := RESPAWNPOINT_DM;

    if g_Map_GetPointCount(c) = 0 then
    begin // Точек возрождения DM нет
      // Пробуем точку начала одиночной игры:
      if Random(2) = 0 then
        c := RESPAWNPOINT_PLAYER1
      else
        c := RESPAWNPOINT_PLAYER2;

      if g_Map_GetPointCount(c) = 0 then
      begin // Этой точки нет
        // Пробуем противоположную:
        if c = RESPAWNPOINT_PLAYER1 then
          c := RESPAWNPOINT_PLAYER2
        else
          c := RESPAWNPOINT_PLAYER1;

        if g_Map_GetPointCount(c) = 0 then
        begin // Точек игроков нет
          // Пробуем командную точку:
          if Random(2) = 0 then
            c := RESPAWNPOINT_RED
          else
            c := RESPAWNPOINT_BLUE;

          if g_Map_GetPointCount(c) = 0 then
          begin // Этой точки нет
            // Пробуем другую команду:
            if c = RESPAWNPOINT_RED then
              c := RESPAWNPOINT_BLUE
            else
              c := RESPAWNPOINT_RED;
          end
          else
            c := -1;
        end;
      end;
    end;

    if c = -1 then
    begin
      GameX := gMapInfo.Width div 2;
      GameY := gMapInfo.Height div 2;
    end
    else
      if not g_Map_GetPoint(c, RespawnPoint) then
      begin
        GameX := gMapInfo.Width div 2;
        GameY := gMapInfo.Height div 2;
      end
      else
      begin
        GameX := RespawnPoint.X;
        GameY := RespawnPoint.Y;
      end;
  end;
  FXTo := GameX;
  FYTo := GameY;

  FLive := False;
  FSpectator := True;
  FGhost := True;
  FPhysics := False;
  FWantsInGame := False;

  if g_Game_IsNet then
    MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.Run(Direction: TDirection);
begin
  if MAX_RUNVEL > 8 then
    FlySmoke();

// Бежим:
  if Direction = D_LEFT then
    begin
      if FObj.Vel.X > -MAX_RUNVEL then
        FObj.Vel.X := FObj.Vel.X - (MAX_RUNVEL shr 3);
    end
  else
    if FObj.Vel.X < MAX_RUNVEL then
      FObj.Vel.X := FObj.Vel.X + (MAX_RUNVEL shr 3);

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
                                    PLAYER_RECT.Width, 1, PANEL_STEP, False)
             and g_Map_CollidePanel(FObj.X+PLAYER_RECT.X, FObj.Y+YInc+PLAYER_RECT.Y+PLAYER_RECT.Height,
                                    PLAYER_RECT.Width, 1, PANEL_STEP, False);
end;

function TPlayer.TeleportTo(X, Y: Integer; silent: Boolean; dir: Byte): Boolean;
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

    g_Sound_PlayExAt('SOUND_GAME_TELEPORT', FObj.X, FObj.Y);
    g_GFX_OnceAnim(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                   FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, Anim);
    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Effect(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                     FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, 1,
                     NET_GFX_TELE);
  end;
 
  FObj.X := X-PLAYER_RECT.X;
  FObj.Y := Y-PLAYER_RECT.Y;

  if not g_Game_IsNet then
  begin
    if dir = 1 then
    begin
      SetDirection(D_LEFT);
      FAngle := 180;
    end
    else
      if dir = 2 then
      begin
        SetDirection(D_RIGHT);
        FAngle := 0;
      end
      else
        if dir = 3 then
        begin // обратное
          if FDirection = D_RIGHT then
          begin
            SetDirection(D_LEFT);
            FAngle := 180;
          end
          else
          begin
            SetDirection(D_RIGHT);
            FAngle := 0;
          end;
        end;
  end;

  if not silent and (Anim <> nil) then
  begin
    g_GFX_OnceAnim(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                   FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, Anim);
    Anim.Free();

    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Effect(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-24,
                     FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, 0,
                     NET_GFX_TELE);
  end;

  Result := True;
end;

function nonz(a: Single): Single;
begin
  if a <> 0 then
    Result := a
  else
    Result := 1;
end;

procedure TPlayer.Update();
var
  b: Byte;
  i, ii, wx, wy, xd, yd, k: Integer;
  blockmon, headwater, dospawn: Boolean;
  st: Word;
  NetServer: Boolean;
  AnyServer: Boolean;
  SetSpect: Boolean;
begin
  NetServer := g_Game_IsNet and g_Game_IsServer;
  AnyServer := g_Game_IsServer;

  if g_Game_IsClient and (NetInterpLevel > 0) then
    DoLerp(NetInterpLevel + 1)
  else
    if FGhost then
      DoLerp(4);

  if gFly or FJetpack then
    FlySmoke();

  if FDirection = D_LEFT then
    FAngle := 180
  else
    FAngle := 0;

  if FLive then
  begin
    if FKeys[KEY_UP].Pressed then
      SeeUp();
    if FKeys[KEY_DOWN].Pressed then
      SeeDown();
  end;

  if (not (FKeys[KEY_UP].Pressed or FKeys[KEY_DOWN].Pressed)) and
     (FIncCam <> 0) then
  begin
    i := g_basic.Sign(FIncCam);
    FIncCam := Abs(FIncCam);
    DecMin(FIncCam, 5, 0);
    FIncCam := FIncCam*i;
  end;

  if gTime mod (GAME_TICK*2) <> 0 then
  begin
    if (FObj.Vel.X = 0) and FLive then
    begin
      if FKeys[KEY_LEFT].Pressed then
        Run(D_LEFT);
      if FKeys[KEY_RIGHT].Pressed then
        Run(D_RIGHT);
    end;

    if FPhysics then
      g_Obj_Move(@FObj, True, True);

    Exit;
  end;

  FActionChanged := False;

  if FLive then
    begin
      if FKeys[KEY_LEFT].Pressed then Run(D_LEFT);
      if FKeys[KEY_RIGHT].Pressed then Run(D_RIGHT);
      if FKeys[KEY_NEXTWEAPON].Pressed and AnyServer then NextWeapon();
      if FKeys[KEY_PREVWEAPON].Pressed and AnyServer then PrevWeapon();
      if FKeys[KEY_FIRE].Pressed and AnyServer then Fire();
      if FKeys[KEY_OPEN].Pressed and AnyServer then Use();
      if FKeys[KEY_JUMP].Pressed then Jump()
      else
      begin
        if AnyServer and FJetpack then
        begin
          FJetpack := False;
          JetpackOff;
          if NetServer then MH_SEND_PlayerStats(FUID);
        end;
        FCanJetpack := True;
      end;
    end
  else // Dead
  begin
    dospawn := False;
    for k := Low(FKeys) to KEY_CHAT-1 do
    begin
      if FKeys[k].Pressed then
      begin
        dospawn := True;
        break;
      end;
    end;
    if (dospawn) then
    begin
      if FGhost then
      begin
        if (not FSpectator) or (FSpectatePlayer = -1) then
          FYTo := FObj.Y - 24
      end
      else
        if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER] then
          Respawn(False)
        else // Single
          if (FTime[T_RESPAWN] <= gTime) and
            gGameOn and (not FLive) then
          begin
            if (g_Player_GetCount() > 1) then
              Respawn(False)
            else
            begin
              gExit := EXIT_RESTART;
              Exit;
            end;
          end;
    end;
    if FGhost then
    begin
      if FKeys[KEY_DOWN].Pressed then
      begin
        FYTo := FObj.Y + 32;
        FSpectatePlayer := -1;
      end;
      if FKeys[KEY_LEFT].Pressed then
      begin
        FXTo := FObj.X - 32;
        FSpectatePlayer := -1;
      end;
      if FKeys[KEY_RIGHT].Pressed then
      begin
        FXto := FObj.X + 32;
        FSpectatePlayer := -1;
      end;

      if (FXTo < 0) then
        FXTo := 0
      else
        if (FXTo > gMapInfo.Width) then FXTo := gMapInfo.Width;
      if (FYTo < 0) then
        FYTo := 0
      else
        if (FYTo > gMapInfo.Height) then FYTo := gMapInfo.Height;

      if FKeys[KEY_OPEN].Pressed and AnyServer then Fire();
      if FKeys[KEY_FIRE].Pressed and AnyServer then
      begin
        if FSpectator then
        begin
          if (FSpectatePlayer >= High(gPlayers)) then
            FSpectatePlayer := -1
          else
          begin
            SetSpect := False;
            for I := FSpectatePlayer + 1 to High(gPlayers) do
              if gPlayers[I] <> nil then
                if gPlayers[I].Live then
                  if gPlayers[I].UID <> FUID then
                  begin
                    FSpectatePlayer := I;
                    SetSpect := True;
                    break;
                  end;

            if not SetSpect then FSpectatePlayer := -1;
          end;

          ReleaseKeys;
        end;
      end;
    end;
  end;

  if FPhysics then
    g_Obj_Move(@FObj, True, True)
  else
  begin
    FObj.Vel.X := 0;
    FObj.Vel.Y := 0;
    if FSpectator then
      if (FSpectatePlayer <= High(gPlayers)) and (FSpectatePlayer >= 0) then
        if gPlayers[FSpectatePlayer] <> nil then
          if gPlayers[FSpectatePlayer].Live then
          begin
            FXTo := gPlayers[FSpectatePlayer].GameX;
            FYTo := gPlayers[FSpectatePlayer].GameY;
          end;
  end;

  blockmon := g_Map_CollidePanel(FObj.X+PLAYER_HEADRECT.X, FObj.Y+PLAYER_HEADRECT.Y,
                                 PLAYER_HEADRECT.Width, PLAYER_HEADRECT.Height,
                                 PANEL_BLOCKMON, True);
  headwater := HeadInLiquid(0, 0);

// Сопротивление воздуха:
  if (not FLive) or not (FKeys[KEY_LEFT].Pressed or FKeys[KEY_RIGHT].Pressed) then
    if FObj.Vel.X <> 0 then
      FObj.Vel.X := z_dec(FObj.Vel.X, 1);

  if (FLastHit = HIT_TRAP) and (FPain > 90) then FPain := 90;
  DecMin(FPain, 5, 0);

  if FLive and (FObj.Y > gMapInfo.Height+128) and AnyServer then
  begin
    // Обнулить действия примочек, чтобы фон пропал
    FMegaRulez[MR_SUIT] := 0;
    FMegaRulez[MR_INVUL] := 0;
    FMegaRulez[MR_INVIS] := 0;
    Kill(K_FALLKILL, 0, HIT_FALL);
  end;

  i := 9;

  if FLive then
  begin
    if FCurrWeap = WEAPON_SAW then
      if not (FSawSound.IsPlaying() or FSawSoundHit.IsPlaying() or
              FSawSoundSelect.IsPlaying()) then
        FSawSoundIdle.PlayAt(FObj.X, FObj.Y);

    if FJetpack then
      if (not FJetSoundFly.IsPlaying()) and (not FJetSoundOn.IsPlaying()) and
         (not FJetSoundOff.IsPlaying()) then
      begin
        FJetSoundFly.SetPosition(0);
        FJetSoundFly.PlayAt(FObj.X, FObj.Y);
      end;

    for b := WEAPON_KASTET to WEAPON_SUPERPULEMET do
      if FReloading[b] > 0 then Dec(FReloading[b]);

    if (FBFGFireCounter > -1) then
      if FBFGFireCounter = 0 then
      begin
        wx := FObj.X+WEAPONPOINT[FDirection].X;
        wy := FObj.Y+WEAPONPOINT[FDirection].Y;
        xd := wx+IfThen(FDirection = D_LEFT, -30, 30);
        yd := wy+firediry();

        if AnyServer then
        begin
          g_Weapon_bfgshot(wx, wy, xd, yd, FUID);
          if NetServer then MH_SEND_PlayerFire(FUID, WEAPON_BFG, wx, wy, xd, yd);
          if (FAngle = 0) or (FAngle = 180) then SetAction(A_ATTACK)
            else if (FAngle = ANGLE_LEFTDOWN) or (FAngle = ANGLE_RIGHTDOWN) then SetAction(A_ATTACKDOWN)
              else if (FAngle = ANGLE_LEFTUP) or (FAngle = ANGLE_RIGHTUP) then SetAction(A_ATTACKUP);
        end;

        FReloading[WEAPON_BFG] := 0;
        FBFGFireCounter := -1;
      end else FBFGFireCounter := FBFGFireCounter-1;

    if (FMegaRulez[MR_SUIT] < gTime) and AnyServer then
    begin
      b := g_GetAcidHit(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width, PLAYER_RECT.Height);

      if (b > 0) and (gTime mod (15*GAME_TICK) = 0) then Damage(b, 0, 0, 0, HIT_ACID);
    end;

    if (headwater or blockmon) then
    begin
      FAir := FAir-1;

      if FAir < -9 then
      begin
        if AnyServer then Damage(10, 0, 0, 0, HIT_WATER);
          FAir := 0;
      end
      else if (FAir mod 31 = 0) and not blockmon then
      begin
        g_GFX_Bubbles(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2), FObj.Y+PLAYER_RECT.Y-4, 5+Random(6), 8, 4);
        if Random(2) = 0 then
          g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
      end;
    end else if FAir < AIR_DEF then FAir := AIR_DEF;

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

      if AnyServer then
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

    {CollideItem();}
  end; // if FLive then ...

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

function TPlayer.Collide(X, Y: Integer; Width, Height: Word): Boolean;
begin
  Result := g_Collide(FObj.X+PLAYER_RECT.X,
                      FObj.Y+PLAYER_RECT.Y,
                      PLAYER_RECT.Width,
                      PLAYER_RECT.Height,
                      X, Y,
                      Width, Height);
end;

function TPlayer.Collide(Panel: TPanel): Boolean;
begin
  Result := g_Collide(FObj.X+PLAYER_RECT.X,
                      FObj.Y+PLAYER_RECT.Y,
                      PLAYER_RECT.Width,
                      PLAYER_RECT.Height,
                      Panel.X, Panel.Y,
                      Panel.Width, Panel.Height);
end;

function TPlayer.Collide(X, Y: Integer): Boolean;
begin
  X := X-FObj.X-PLAYER_RECT.X;
  Y := Y-FObj.Y-PLAYER_RECT.Y;
  Result := (x >= 0) and (x <= PLAYER_RECT.Width) and
            (y >= 0) and (y <= PLAYER_RECT.Height);
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

function TPlayer.GetKeys(): Byte;
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

procedure TPlayer.NetFire(Wpn: Byte; X, Y, AX, AY: Integer; WID: Integer = -1);
var
  Obj: TObj;
  F: Boolean;
  WX, WY, XD, YD: Integer;
begin
  F := False;
  WX := X;
  WY := Y;
  XD := AX;
  YD := AY;

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
        obj.Vel.X := (xd-wx) div 2;
        obj.Vel.Y := (yd-wy) div 2;
        obj.Accel.X := xd-wx;
        obj.Accel.y := yd-wy;

        if g_Weapon_Hit(@obj, 50, FUID, HIT_SOME) <> 0 then
          g_Sound_PlayExAt('SOUND_WEAPON_HITBERSERK', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_WEAPON_MISSBERSERK', FObj.X, FObj.Y);

        if gFlash then
          if FPain < 50 then
            FPain := min(FPain + 25, 50);
      end else
        g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 3, FUID);
    end;

    WEAPON_SAW:
    begin
      if g_Weapon_chainsaw(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                           IfThen(gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF], 9, 3), FUID) <> 0 then
      begin
        FSawSoundSelect.Stop();
        FSawSound.Stop();
        FSawSoundHit.PlayAt(FObj.X, FObj.Y);
      end
      else if not FSawSoundHit.IsPlaying() then
      begin
        FSawSoundSelect.Stop();
        FSawSound.PlayAt(FObj.X, FObj.Y);
      end;
      f := True;
    end;

    WEAPON_PISTOL:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', GameX, Gamey);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_SHOTGUN1:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', Gamex, Gamey);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_SHOTGUN2:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN2', Gamex, Gamey);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_CHAINGUN:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', Gamex, Gamey);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_ROCKETLAUNCHER:
    begin
      g_Weapon_Rocket(wx, wy, xd, yd, FUID, WID);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_PLASMA:
    begin
      g_Weapon_Plasma(wx, wy, xd, yd, FUID, WID);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_BFG:
    begin
      g_Weapon_BFGShot(wx, wy, xd, yd, FUID, WID);
      FFireAngle := FAngle;
      f := True;
    end;

    WEAPON_SUPERPULEMET:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', Gamex, Gamey);
      FFireAngle := FAngle;
      f := True;
    end;
  end;

  if not f then Exit;

  if (FAngle = 0) or (FAngle = 180) then SetAction(A_ATTACK)
    else if (FAngle = ANGLE_LEFTDOWN) or (FAngle = ANGLE_RIGHTDOWN) then SetAction(A_ATTACKDOWN)
      else if (FAngle = ANGLE_LEFTUP) or (FAngle = ANGLE_RIGHTUP) then SetAction(A_ATTACKUP);
end;

procedure TPlayer.DoLerp(Level: Integer = 2);
begin
  if FObj.X <> FXTo then FObj.X := Lerp(FObj.X, FXTo, Level);
  if FObj.Y <> FYTo then FObj.Y := Lerp(FObj.Y, FYTo, Level);
end;

procedure TPlayer.SetLerp(XTo, YTo: Integer);
var
  AX, AY: Integer;
begin
  if NetInterpLevel < 1 then
  begin
    FObj.X := XTo;
    FObj.Y := YTo;
  end
  else
  begin
    FXTo := XTo;
    FYTo := YTo;

    AX := Abs(FXTo - FObj.X);
    AY := Abs(FYTo - FObj.Y);
    if (AX > 32) or (AX <= NetInterpLevel) then
      FObj.X := FXTo;
    if (AY > 32) or (AY <= NetInterpLevel) then
      FObj.Y := FYTo;
  end;
end;

function TPlayer.FullInLift(XInc, YInc: Integer): Integer;
begin
  if g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                        PLAYER_RECT.Width, PLAYER_RECT.Height-8,
                        PANEL_LIFTUP, False) then Result := -1
  else
  if g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                        PLAYER_RECT.Width, PLAYER_RECT.Height-8,
                        PANEL_LIFTDOWN, False) then Result := 1
  else Result := 0;
end;

function TPlayer.GetFlag(Flag: Byte): Boolean;
var
  s: String;
  evtype: Byte;
begin
  Result := False;

  if Flag = FLAG_NONE then
    Exit;

  if not g_Game_IsServer then Exit;

// Принес чужой флаг на свою базу:
  if (Flag = FTeam) and
     (gFlags[Flag].State = FLAG_STATE_NORMAL) and
     (FFlag <> FLAG_NONE) then
  begin
    if FFlag = FLAG_RED then
      s := _lc[I_PLAYER_FLAG_RED]
    else
      s := _lc[I_PLAYER_FLAG_BLUE];

    evtype := FLAG_STATE_SCORED;

    g_Console_Add(Format(_lc[I_PLAYER_FLAG_CAPTURE], [FName, s]), True);

    g_Map_ResetFlag(FFlag);
    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_CAPTURE], [AnsiUpperCase(s)]), 144);

    gTeamStat[FTeam].Goals := gTeamStat[FTeam].Goals + 1;

    FFlag := FLAG_NONE;
    FModel.SetFlag(FLAG_NONE);

    Result := True;
    if g_Game_IsNet then
    begin
      MH_SEND_FlagEvent(evtype, Flag, FUID, False);
      MH_SEND_GameStats;
    end;
    Exit;
  end;

// Подобрал свой флаг - вернул его на базу:
  if (Flag = FTeam) and
     (gFlags[Flag].State = FLAG_STATE_DROPPED) then
  begin
    if Flag = FLAG_RED then
      s := _lc[I_PLAYER_FLAG_RED]
    else
      s := _lc[I_PLAYER_FLAG_BLUE];

    evtype := FLAG_STATE_RETURNED;

    g_Console_Add(Format(_lc[I_PLAYER_FLAG_RETURN], [FName, s]), True);

    g_Map_ResetFlag(Flag);
    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);

    Result := True;
    if g_Game_IsNet then
    begin
      MH_SEND_FlagEvent(evtype, Flag, FUID, False);
      MH_SEND_GameStats;
    end;
    Exit;
  end;

// Подобрал чужой флаг:
  if Flag <> FTeam then
  begin
    FFlag := Flag;
    FModel.SetFlag(FFlag);

    if Flag = FLAG_RED then
      s := _lc[I_PLAYER_FLAG_RED]
    else
      s := _lc[I_PLAYER_FLAG_BLUE];

    evtype := FLAG_STATE_CAPTURED;

    g_Console_Add(Format(_lc[I_PLAYER_FLAG_GET], [FName, s]), True);

    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_GET], [AnsiUpperCase(s)]), 144);

    gFlags[Flag].State := FLAG_STATE_CAPTURED;

    Result := True;
    if g_Game_IsNet then
    begin
      MH_SEND_FlagEvent(evtype, Flag, FUID, False);
      MH_SEND_GameStats;
    end;
  end;
end;

procedure TPlayer.SetFlag(Flag: Byte);
begin
  FFlag := Flag;
  FModel.SetFlag(FFlag);
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

function TPlayer.IsKeyPressed(K: Byte): Boolean;
begin
  Result := FKeys[K].Pressed;
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

procedure TPlayer.RememberState();
var
  i: Integer;
begin
  FSavedState.Health := FHealth;
  FSavedState.Armor := FArmor;
  FSavedState.CurrWeap := FCurrWeap;

  for i := 0 to 3 do
    FSavedState.Ammo[i] := FAmmo[i];
  for i := 0 to 3 do
    FSavedState.MaxAmmo[i] := FMaxAmmo[i];

  FSavedState.Rulez := FRulez;
  FSavedState.WaitRecall := True;
end;
procedure TPlayer.RecallState();
var
  i: Integer;
begin
  if not FSavedState.WaitRecall then Exit;
  
  FHealth := FSavedState.Health;
  FArmor := FSavedState.Armor;
  FCurrWeap := FSavedState.CurrWeap;

  for i := 0 to 3 do
    FAmmo[i] := FSavedState.Ammo[i];
  for i := 0 to 3 do
    FMaxAmmo[i] := FSavedState.MaxAmmo[i];

  FRulez := FSavedState.Rulez;
  FSavedState.WaitRecall := False;

  if gGameSettings.GameType = GT_SERVER then
    MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.SaveState(var Mem: TBinMemoryWriter);
var
  i: Integer;
  sig: DWORD;
  str: String;
  b: Byte;
begin
  if FIamBot then
    i := 512
  else
    i := 256;

  Mem := TBinMemoryWriter.Create(i);

// Сигнатура игрока:
  sig := PLAYER_SIGNATURE; // 'PLYR'
  Mem.WriteDWORD(sig);
// Бот или человек:
  Mem.WriteBoolean(FIamBot);
// UID игрока:
  Mem.WriteWord(FUID);
// Имя игрока:
  Mem.WriteString(FName, 32);
// Команда:
  Mem.WriteByte(FTeam);
// Жив ли:
  Mem.WriteBoolean(FLive);
// Израсходовал ли все жизни:
  Mem.WriteBoolean(FNoRespawn);
// Направление:
  if FDirection = D_LEFT then
    b := 1
  else // D_RIGHT
    b := 2;
  Mem.WriteByte(b);
// Здоровье:
  Mem.WriteInt(FHealth);
// Жизни:
  Mem.WriteByte(FLives);
// Броня:
  Mem.WriteInt(FArmor);
// Запас воздуха:
  Mem.WriteInt(FAir);
// Боль:
  Mem.WriteInt(FPain);
// Убил:
  Mem.WriteInt(FKills);
// Убил монстров:
  Mem.WriteInt(FMonsterKills);
// Фрагов:
  Mem.WriteInt(FFrags);
// Смертей:
  Mem.WriteInt(FDeath);
// Какой флаг несет:
  Mem.WriteByte(FFlag);
// Нашел секретов:
  Mem.WriteInt(FSecrets);
// Текущее оружие:
  Mem.WriteByte(FCurrWeap);
// Время зарядки BFG:
  Mem.WriteSmallInt(FBFGFireCounter);
// Буфер урона:
  Mem.WriteInt(FDamageBuffer);
// Последний ударивший:
  Mem.WriteWord(FLastSpawnerUID);
// Тип последнего полученного урона:
  Mem.WriteByte(FLastHit);
// Объект игрока:
  Obj_SaveState(@FObj, Mem);
// Текущее количество патронов:
  for i := A_BULLETS to A_CELLS do
    Mem.WriteWord(FAmmo[i]);
// Максимальное количество патронов:
  for i := A_BULLETS to A_CELLS do
    Mem.WriteWord(FMaxAmmo[i]);
// Наличие оружия:
  for i := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    Mem.WriteBoolean(FWeapon[i]);
// Время перезарядки оружия:
  for i := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    Mem.WriteWord(FReloading[i]);
// Наличие рюкзака:
  if R_ITEM_BACKPACK in FRulez then
    b := 1
  else
    b := 0;
  Mem.WriteByte(b);
// Наличие красного ключа:
  if R_KEY_RED in FRulez then
    b := 1
  else
    b := 0;
  Mem.WriteByte(b);
// Наличие зеленого ключа:
  if R_KEY_GREEN in FRulez then
    b := 1
  else
    b := 0;
  Mem.WriteByte(b);
// Наличие синего ключа:
  if R_KEY_BLUE in FRulez then
    b := 1
  else
    b := 0;
  Mem.WriteByte(b);
// Наличие берсерка:
  if R_BERSERK in FRulez then
    b := 1
  else
    b := 0;
  Mem.WriteByte(b);
// Время действия специальных предметов:
  for i := MR_SUIT to MR_MAX do
    Mem.WriteDWORD(FMegaRulez[i]);
// Время до повторного респауна, смены оружия, исользования:
  for i := T_RESPAWN to T_USE do
    Mem.WriteDWORD(FTime[i]);
// Название модели:
  str := FModel.Name;
  Mem.WriteString(str);
// Цвет модели:
  b := FModel.Color.R;
  Mem.WriteByte(b);
  b := FModel.Color.G;
  Mem.WriteByte(b);
  b := FModel.Color.B;
  Mem.WriteByte(b);
end;

procedure TPlayer.LoadState(var Mem: TBinMemoryReader);
var
  i: Integer;
  sig: DWORD;
  str: String;
  b: Byte;
  col: TRGB;
begin
  if Mem = nil then
    Exit;

// Сигнатура игрока:
  Mem.ReadDWORD(sig);
  if sig <> PLAYER_SIGNATURE then // 'PLYR'
  begin
    raise EBinSizeError.Create('TPlayer.LoadState: Wrong Player Signature');
  end;
// Бот или человек:
  Mem.ReadBoolean(FIamBot);
// UID игрока:
  Mem.ReadWord(FUID);
// Имя игрока:
  Mem.ReadString(str);
  if (FPlayerNum <> 1) and (FPlayerNum <> 2) then
    FName := str;
// Команда:
  Mem.ReadByte(FTeam);
// Жив ли:
  Mem.ReadBoolean(FLive);
// Израсходовал ли все жизни:
  Mem.ReadBoolean(FNoRespawn);
// Направление:
  Mem.ReadByte(b);
  if b = 1 then
    FDirection := D_LEFT
  else // b = 2
    FDirection := D_RIGHT;
// Здоровье:
  Mem.ReadInt(FHealth);
// Жизни:
  Mem.ReadByte(FLives);
// Броня:
  Mem.ReadInt(FArmor);
// Запас воздуха:
  Mem.ReadInt(FAir);
// Боль:
  Mem.ReadInt(FPain);
// Убил:
  Mem.ReadInt(FKills);
// Убил монстров:
  Mem.ReadInt(FMonsterKills);
// Фрагов:
  Mem.ReadInt(FFrags);
// Смертей:
  Mem.ReadInt(FDeath);
// Какой флаг несет:
  Mem.ReadByte(FFlag);
// Нашел секретов:
  Mem.ReadInt(FSecrets);
// Текущее оружие:
  Mem.ReadByte(FCurrWeap);
// Время зарядки BFG:
  Mem.ReadSmallInt(FBFGFireCounter);
// Буфер урона:
  Mem.ReadInt(FDamageBuffer);
// Последний ударивший:
  Mem.ReadWord(FLastSpawnerUID);
// Тип последнего полученного урона:
  Mem.ReadByte(FLastHit);
// Объект игрока:
  Obj_LoadState(@FObj, Mem);
// Текущее количество патронов:
  for i := A_BULLETS to A_CELLS do
    Mem.ReadWord(FAmmo[i]);
// Максимальное количество патронов:
  for i := A_BULLETS to A_CELLS do
    Mem.ReadWord(FMaxAmmo[i]);
// Наличие оружия:
  for i := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    Mem.ReadBoolean(FWeapon[i]);
// Время перезарядки оружия:
  for i := WEAPON_KASTET to WEAPON_SUPERPULEMET do
    Mem.ReadWord(FReloading[i]);
// Наличие рюкзака:
  Mem.ReadByte(b);
  if b = 1 then
    Include(FRulez, R_ITEM_BACKPACK);
// Наличие красного ключа:
  Mem.ReadByte(b);
  if b = 1 then
    Include(FRulez, R_KEY_RED);
// Наличие зеленого ключа:
  Mem.ReadByte(b);
  if b = 1 then
    Include(FRulez, R_KEY_GREEN);
// Наличие синего ключа:
  Mem.ReadByte(b);
  if b = 1 then
    Include(FRulez, R_KEY_BLUE);
// Наличие берсерка:
  Mem.ReadByte(b);
  if b = 1 then
    Include(FRulez, R_BERSERK);
// Время действия специальных предметов:
  for i := MR_SUIT to MR_MAX do
    Mem.ReadDWORD(FMegaRulez[i]);
// Время до повторного респауна, смены оружия, исользования:
  for i := T_RESPAWN to T_USE do
    Mem.ReadDWORD(FTime[i]);
// Название модели:
  Mem.ReadString(str);
// Цвет модели:
  Mem.ReadByte(col.R);
  Mem.ReadByte(col.G);
  Mem.ReadByte(col.B);
// Обновляем модель игрока:
  SetModel(str);
  FModel.Color := col;
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

procedure TPlayer.GiveItem(ItemType: Byte);
begin
  case ItemType of
    ITEM_SUIT:
      if FMegaRulez[MR_SUIT] < gTime+PLAYER_SUIT_TIME then
      begin
        FMegaRulez[MR_SUIT] := gTime+PLAYER_SUIT_TIME;
      end;

    ITEM_OXYGEN:
      if FAir < AIR_MAX then
      begin
        FAir := AIR_MAX;
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
        end;
        if FHealth < 100 then
        begin
          FHealth := 100;
        end;
      end;

    ITEM_INVUL:
      if FMegaRulez[MR_INVUL] < gTime+PLAYER_INVUL_TIME then
      begin
        FMegaRulez[MR_INVUL] := gTime+PLAYER_INVUL_TIME;
      end;

    ITEM_INVIS:
      if FMegaRulez[MR_INVIS] < gTime then
      begin
        FMegaRulez[MR_INVIS] := gTime+PLAYER_INVIS_TIME;
      end;

    ITEM_JETPACK:
      if FMegaRulez[MR_JET] < JET_MAX then
      begin
        FMegaRulez[MR_JET] := JET_MAX;
      end;

    else
      Exit;
  end;
  if gGameSettings.GameType = GT_SERVER then MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.SetModel(ModelName: string);
var
  m: TPlayerModel;
  old_color: TRGB;
begin
  m := g_PlayerModel_Get(ModelName);
  if m = nil then
  begin
    g_SimpleError('Could not find model: ' + ModelName + '. Using doomer instead.');
    m := g_PlayerModel_Get('doomer');
    if m = nil then
    begin
      g_FatalError('Could not find model: doomer.');
      Exit;
    end;
  end;

  old_color := _RGB(0, 0, 0);

  if FModel <> nil then
  begin
    old_color := FModel.Color;
    FModel.Free();
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

procedure TPlayer.FlySmoke(Times: DWORD = 1);
var
  id, i: DWORD;
  Anim: TAnimation;
begin
  if (Random(5) = 1) and (Times = 1) then
    Exit;

  if BodyInLiquid(0, 0) then
  begin
    g_GFX_Bubbles(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)+Random(3)-1,
                  Obj.Y+Obj.Rect.Height+8, 1, 8, 4);
    if Random(2) = 0 then
      g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
    else
      g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
    Exit;
  end;

  if g_Frames_Get(id, 'FRAMES_SMOKE') then
  begin
    for i := 1 to Times do
    begin
      Anim := TAnimation.Create(id, False, 3);
      Anim.Alpha := 150;
      g_GFX_OnceAnim(Obj.X+Obj.Rect.X+Random(Obj.Rect.Width+Times*2)-(Anim.Width div 2),
                   Obj.Y+Obj.Rect.Height-4+Random(8+Times*2), Anim, ONCEANIM_SMOKE);
      Anim.Free();
    end;
  end;
end;

procedure TPlayer.PauseSounds(Enable: Boolean);
begin
  FSawSound.Pause(Enable);
  FSawSoundIdle.Pause(Enable);
  FSawSoundHit.Pause(Enable);
  FSawSoundSelect.Pause(Enable);
end;

{ T C o r p s e : }

constructor TCorpse.Create(X, Y: Integer; ModelName: String; aMess: Boolean);
begin
  g_Obj_Init(@FObj);
  FObj.X := X;
  FObj.Y := Y;
  FObj.Rect := PLAYER_CORPSERECT;
  FModelName := ModelName;
  FMess := aMess;

  if FMess then
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

destructor TCorpse.Destroy();
begin
  FAnimation.Free();

  inherited;
end;

procedure TCorpse.Damage(Value: Word; vx, vy: Integer);
begin
  if FState = CORPSE_STATE_REMOVEME then
    Exit;

  FDamage := FDamage + Value;

  if FDamage > 150 then
    begin
      if FAnimation <> nil then
      begin
        FAnimation.Free();
        FAnimation := nil;

        FState := CORPSE_STATE_REMOVEME;

        g_Player_CreateGibs(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                            FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                            FModelName, FColor);
      end;
    end
  else
    begin
      FObj.Vel.X := FObj.Vel.X + vx;
      FObj.Vel.Y := FObj.Vel.Y + vy;
    end;
end;

procedure TCorpse.Draw();
begin
  if FState = CORPSE_STATE_REMOVEME then
    Exit;

  if FAnimation <> nil then
    FAnimation.Draw(FObj.X, FObj.Y, M_NONE);

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
  if FState = CORPSE_STATE_REMOVEME then
    Exit;

  if gTime mod (GAME_TICK*2) <> 0 then
  begin
    g_Obj_Move(@FObj, True, True);
    
    Exit;
  end;

// Сопротивление воздуха для трупа:
  FObj.Vel.X := z_dec(FObj.Vel.X, 1);

  st := g_Obj_Move(@FObj, True, True);

  if WordBool(st and MOVE_FALLOUT) then
  begin
    FState := CORPSE_STATE_REMOVEME;
    Exit;
  end;

  if FAnimation <> nil then
    FAnimation.Update();
  if FAnimationMask <> nil then
    FAnimationMask.Update();
end;

procedure TCorpse.SaveState(var Mem: TBinMemoryWriter);
var
  sig: DWORD;
  anim: Boolean;
begin
  if Mem = nil then
    Exit;

// Сигнатура трупа:
  sig := CORPSE_SIGNATURE; // 'CORP'
  Mem.WriteDWORD(sig);
// Состояние:
  Mem.WriteByte(FState);
// Накопленный урон:
  Mem.WriteByte(FDamage);
// Цвет:
  Mem.WriteByte(FColor.R);
  Mem.WriteByte(FColor.G);
  Mem.WriteByte(FColor.B);
// Объект трупа:
  Obj_SaveState(@FObj, Mem);
// Есть ли анимация:
  anim := FAnimation <> nil;
  Mem.WriteBoolean(anim);
// Если есть - сохраняем:
  if anim then
    FAnimation.SaveState(Mem);
// Есть ли маска анимации:
  anim := FAnimationMask <> nil;
  Mem.WriteBoolean(anim);
// Если есть - сохраняем:
  if anim then
    FAnimationMask.SaveState(Mem);
end;

procedure TCorpse.LoadState(var Mem: TBinMemoryReader);
var
  sig: DWORD;
  anim: Boolean;
begin
  if Mem = nil then
    Exit;

// Сигнатура трупа:
  Mem.ReadDWORD(sig);
  if sig <> CORPSE_SIGNATURE then // 'CORP'
  begin
    raise EBinSizeError.Create('TCorpse.LoadState: Wrong Corpse Signature');
  end;
// Состояние:
  Mem.ReadByte(FState);
// Накопленный урон:
  Mem.ReadByte(FDamage);
// Цвет:
  Mem.ReadByte(FColor.R);
  Mem.ReadByte(FColor.G);
  Mem.ReadByte(FColor.B);
// Объект трупа:
  Obj_LoadState(@FObj, Mem);
// Есть ли анимация:
  Mem.ReadBoolean(anim);
// Если есть - загружаем:
  if anim then
  begin
    Assert(FAnimation <> nil, 'TCorpse.LoadState: no FAnimation');
    FAnimation.LoadState(Mem);
  end;
// Есть ли маска анимации:
  Mem.ReadBoolean(anim);
// Если есть - загружаем:
  if anim then
  begin
    Assert(FAnimationMask <> nil, 'TCorpse.LoadState: no FAnimationMask');
    FAnimationMask.LoadState(Mem);
  end;
end;

{ T B o t : }

constructor TBot.Create();
var
  a: Integer;
begin
  inherited Create();

  FPhysics := True;
  FSpectator := False;
  FGhost := False;

  FIamBot := True;

  for a := WEAPON_KASTET to WEAPON_SUPERPULEMET do
  begin
    FDifficult.WeaponPrior[a] := WEAPON_PRIOR1[a];
    FDifficult.CloseWeaponPrior[a] := WEAPON_PRIOR2[a];
    //FDifficult.SafeWeaponPrior[a] := WEAPON_PRIOR3[a];
  end;
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

procedure TBot.Respawn(Silent: Boolean; Force: Boolean = False);
begin
  inherited Respawn(Silent, Force);

  FAIFlags := nil;
  FSelectedWeapon := FCurrWeap;
  FTargetUID := 0;
end;

procedure TBot.UpdateCombat();
type
  TTarget = record
    UID: Word;
    X, Y: Integer;
    Rect: TRectWH;
    cX, cY: Integer;
    Dist: Word;
    Line: Boolean;
    Visible: Boolean;
    IsPlayer: Boolean;
  end;

  TTargetRecord = array of TTarget;

  function Compare(a, b: TTarget): Integer;
  begin
    if a.Line and not b.Line then // A на линии огня
      Result := -1
   else
     if not a.Line and b.Line then // B на линии огня
       Result := 1
     else // И A, и B на линии или не на линии огня
       if (a.Line and b.Line) or ((not a.Line) and (not b.Line)) then
         begin
           if a.Dist > b.Dist then // B ближе
             Result := 1
           else // A ближе или равноудаленно с B
             Result := -1;
         end
       else // Странно -> A
         Result := -1;
  end;

var
  a, x1, y1, x2, y2: Integer;
  targets: TTargetRecord;
  ammo: Word;
  Target, BestTarget: TTarget;
  firew, fireh: Integer;
  angle: SmallInt;
  mon: TMonster;
  pla: TPlayer;
  vsPlayer, vsMonster, ok: Boolean;
begin
  vsPlayer := LongBool(gGameSettings.Options and GAME_OPTION_BOTVSPLAYER);
  vsMonster := LongBool(gGameSettings.Options and GAME_OPTION_BOTVSMONSTER);

// Если текущее оружие не то, что нужно, то меняем:
  if FCurrWeap <> FSelectedWeapon then
    NextWeapon();

// Если нужно стрелять и нужное оружие, то нажать "Стрелять":
  if (GetAIFlag('NEEDFIRE') <> '') and (FCurrWeap = FSelectedWeapon) then
    begin
      RemoveAIFlag('NEEDFIRE');

      case FCurrWeap of
        WEAPON_PLASMA, WEAPON_SUPERPULEMET, WEAPON_CHAINGUN: PressKey(KEY_FIRE, 20);
        WEAPON_SAW, WEAPON_KASTET, WEAPON_MEGAKASTET: PressKey(KEY_FIRE, 40);
        else PressKey(KEY_FIRE, 0);
      end;
    end;

// Координаты ствола:
  x1 := FObj.X + WEAPONPOINT[FDirection].X;
  y1 := FObj.Y + WEAPONPOINT[FDirection].Y;

  Target.UID := FTargetUID;

  ok := False;
  if Target.UID <> 0 then
    begin // Цель есть - настраиваем
      if (g_GetUIDType(Target.UID) = UID_PLAYER) and
          vsPlayer then
        begin // Игрок
          with g_Player_Get(Target.UID) do
            begin
              Target.X := FObj.X;
              Target.Y := FObj.Y;
            end;

          Target.cX := Target.X + PLAYER_RECT_CX;
          Target.cY := Target.Y + PLAYER_RECT_CY;
          Target.Rect := PLAYER_RECT;
          Target.Visible := g_TraceVector(x1, y1, Target.cX, Target.cY);
          Target.Line := (y1+4 < Target.Y+PLAYER_RECT.Y+PLAYER_RECT.Height) and
                         (y1-4 > Target.Y+PLAYER_RECT.Y);
          Target.IsPlayer := True;
          ok := True;
        end
      else
        if (g_GetUIDType(Target.UID) = UID_MONSTER) and
            vsMonster then
          begin // Монстр
            mon := g_Monsters_Get(Target.UID);
            if mon <> nil then
              begin
                Target.X := mon.Obj.X;
                Target.Y := mon.Obj.Y;

                Target.cX := Target.X + mon.Obj.Rect.X + (mon.Obj.Rect.Width div 2);
                Target.cY := Target.Y + mon.Obj.Rect.Y + (mon.Obj.Rect.Height div 2);
                Target.Rect := mon.Obj.Rect;
                Target.Visible := g_TraceVector(x1, y1, Target.cX, Target.cY);
                Target.Line := (y1+4 < Target.Y + mon.Obj.Rect.Y + mon.Obj.Rect.Height) and
                               (y1-4 > Target.Y + mon.Obj.Rect.Y);
                Target.IsPlayer := False;
                ok := True;
              end;
          end;
    end;

  if not ok then 
    begin // Цели нет - обнуляем
      Target.X := 0;
      Target.Y := 0;
      Target.cX := 0;
      Target.cY := 0;
      Target.Visible := False;
      Target.Line := False;
      Target.IsPlayer := False;
    end;

  targets := nil;

// Если цель не видима или не на линии огня, то ищем все возможные цели:
  if (not Target.Line) or (not Target.Visible) then
  begin
  // Игроки:
    if vsPlayer then
      for a := 0 to High(gPlayers) do
        if (gPlayers[a] <> nil) and (gPlayers[a].Live) and
           (gPlayers[a].FUID <> FUID) and
           (not SameTeam(FUID, gPlayers[a].FUID)) and
           (not gPlayers[a].NoTarget) and
           (gPlayers[a].FMegaRulez[MR_INVIS] < 1) then
          begin
            if not TargetOnScreen(gPlayers[a].FObj.X + PLAYER_RECT.X,
                                  gPlayers[a].FObj.Y + PLAYER_RECT.Y) then
              Continue;

            x2 := gPlayers[a].FObj.X + PLAYER_RECT_CX;
            y2 := gPlayers[a].FObj.Y + PLAYER_RECT_CY;

          // Если игрок на экране и не прикрыт стеной:
            if g_TraceVector(x1, y1, x2, y2) then
              begin
              // Добавляем к списку возможных целей:
                SetLength(targets, Length(targets)+1);
                with targets[High(targets)] do
                  begin
                    UID := gPlayers[a].FUID;
                    X := gPlayers[a].FObj.X;
                    Y := gPlayers[a].FObj.Y;
                    cX := x2;
                    cY := y2;
                    Rect := PLAYER_RECT;
                    Dist := g_PatchLength(x1, y1, x2, y2);
                    Line := (y1+4 < Target.Y+PLAYER_RECT.Y+PLAYER_RECT.Height) and
                            (y1-4 > Target.Y+PLAYER_RECT.Y);
                    Visible := True;
                    IsPlayer := True;
                  end;
              end;
          end;

  // Монстры:
    if vsMonster and (gMonsters <> nil) then
      for a := 0 to High(gMonsters) do
        if (gMonsters[a] <> nil) and (gMonsters[a].Live) and
           (gMonsters[a].MonsterType <> MONSTER_BARREL) then
          begin
            mon := gMonsters[a];

            if not TargetOnScreen(mon.Obj.X + mon.Obj.Rect.X,
                                  mon.Obj.Y + mon.Obj.Rect.Y) then
              Continue;

            x2 := mon.Obj.X + mon.Obj.Rect.X + (mon.Obj.Rect.Width div 2);
            y2 := mon.Obj.Y + mon.Obj.Rect.Y + (mon.Obj.Rect.Height div 2);

          // Если монстр на экране и не прикрыт стеной:
            if g_TraceVector(x1, y1, x2, y2) then
              begin
              // Добавляем к списку возможных целей:
                SetLength(targets, Length(targets)+1);
                with targets[High(targets)] do
                  begin
                    UID := mon.UID;
                    X := mon.Obj.X;
                    Y := mon.Obj.Y;
                    cX := x2;
                    cY := y2;
                    Rect := mon.Obj.Rect;
                    Dist := g_PatchLength(x1, y1, x2, y2);
                    Line := (y1+4 < Target.Y + mon.Obj.Rect.Y + mon.Obj.Rect.Height) and
                            (y1-4 > Target.Y + mon.Obj.Rect.Y);
                    Visible := True;
                    IsPlayer := False;
                  end;
              end;
          end;
  end;

// Если есть возможные цели:
// (Выбираем лучшую, меняем оружие и бежим к ней/от нее)
  if targets <> nil then
  begin
  // Выбираем наилучшую цель:
    BestTarget := targets[0];
    if Length(targets) > 1 then
      for a := 1 to High(targets) do
        if Compare(BestTarget, targets[a]) = 1 then
          BestTarget := targets[a];

  // Если лучшая цель "виднее" текущей, то текущая := лучшая:
    if ((not Target.Visible) and BestTarget.Visible and (Target.UID <> BestTarget.UID)) or
        ((not Target.Line) and BestTarget.Line and BestTarget.Visible) then
      begin
        Target := BestTarget;

        if (Healthy() = 3) or ((Healthy() = 2)) then
          begin // Если здоровы - догоняем
            if ((RunDirection() = D_LEFT) and (Target.X > FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = D_RIGHT) and (Target.X < FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end
        else
          begin // Если побиты - убегаем
            if ((RunDirection() = D_LEFT) and (Target.X < FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = D_RIGHT) and (Target.X > FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end;

      // Выбираем оружие на основе расстояния и приоритетов:
        SelectWeapon(Abs(x1-Target.cX));
      end;
  end;

// Если есть цель:
// (Догоняем/убегаем, стреляем по направлению к цели)
// (Если цель далеко, то хватит следить за ней)
  if Target.UID <> 0 then
  begin
    if not TargetOnScreen(Target.X + Target.Rect.X,
                          Target.Y + Target.Rect.Y) then
      begin // Цель сбежала с "экрана"
        if (Healthy() = 3) or ((Healthy() = 2)) then
          begin // Если здоровы - догоняем
            if ((RunDirection() = D_LEFT) and (Target.X > FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = D_RIGHT) and (Target.X < FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end
        else
          begin // Если побиты - забываем о цели и убегаем
            Target.UID := 0;
            if ((RunDirection() = D_LEFT) and (Target.X < FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = D_RIGHT) and (Target.X > FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end;
      end
    else
      begin // Цель пока на "экране"
      // Если цель не загорожена стеной, то отмечаем, когда ее видели:
        if g_TraceVector(x1, y1, Target.cX, Target.cY) then
          FLastVisible := gTime;
      // Если разница высот не велика, то догоняем:
        if (Abs(FObj.Y-Target.Y) <= 128) then
          begin
            if ((RunDirection() = D_LEFT) and (Target.X > FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = D_RIGHT) and (Target.X < FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end;
      end;

  // Выбираем угол вверх:
    if FDirection = D_LEFT then
      angle := ANGLE_LEFTUP
    else
      angle := ANGLE_RIGHTUP;

    firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
    fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

  // Если при угле вверх можно попасть в приблизительное положение цели:
    if g_CollideLine(x1, y1, x1+firew, y1+fireh,
          Target.X+Target.Rect.X+GetInterval(FDifficult.DiagPrecision, 128), //96
          Target.Y+Target.Rect.Y+GetInterval(FDifficult.DiagPrecision, 128),
          Target.Rect.Width, Target.Rect.Height) and
        g_TraceVector(x1, y1, Target.cX, Target.cY) then
      begin // то нужно стрелять вверх
        SetAIFlag('NEEDFIRE', '1');
        SetAIFlag('NEEDSEEUP', '1');
      end;

  // Выбираем угол вниз:
    if FDirection = D_LEFT then
      angle := ANGLE_LEFTDOWN
    else
      angle := ANGLE_RIGHTDOWN;

    firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
    fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

  // Если при угле вниз можно попасть в приблизительное положение цели:
    if g_CollideLine(x1, y1, x1+firew, y1+fireh,
          Target.X+Target.Rect.X+GetInterval(FDifficult.DiagPrecision, 128),
          Target.Y+Target.Rect.Y+GetInterval(FDifficult.DiagPrecision, 128),
          Target.Rect.Width, Target.Rect.Height) and
        g_TraceVector(x1, y1, Target.cX, Target.cY) then
      begin // то нужно стрелять вниз
        SetAIFlag('NEEDFIRE', '1');
        SetAIFlag('NEEDSEEDOWN', '1');
      end;

  // Если цель видно и она на такой же высоте:
    if Target.Visible and
        (y1+4 < Target.Y+Target.Rect.Y+Target.Rect.Height) and
        (y1-4 > Target.Y+Target.Rect.Y) then
      begin
      // Если идем в сторону цели, то надо стрелять:
        if ((FDirection = D_LEFT) and (Target.X < FObj.X)) or
            ((FDirection = D_RIGHT) and (Target.X > FObj.X)) then
        begin // то нужно стрелять вперед
          SetAIFlag('NEEDFIRE', '1');
          SetAIFlag('NEEDSEEDOWN', '');
          SetAIFlag('NEEDSEEUP', '');
        end;
      // Если цель в пределах "экрана" и сложность позволяет прыжки сближения:
        if Abs(FObj.X-Target.X) < Trunc(gPlayerScreenSize.X*0.75) then
          if GetRnd(FDifficult.CloseJump) then
            begin // то если повезет - прыгаем (особенно, если близко)
              if Abs(FObj.X-Target.X) < 128 then
                a := 4
              else
                a := 30;
              if Random(a) = 0 then
                SetAIFlag('NEEDJUMP', '1');
            end;
      end;

  // Если цель все еще есть:
    if Target.UID <> 0 then
      if gTime-FLastVisible > 2000 then // Если видели давно
        Target.UID := 0 // то забыть цель
      else // Если видели недавно
        begin // но цель убили
          if Target.IsPlayer then
            begin // Цель - игрок
              pla := g_Player_Get(Target.UID);
              if (pla = nil) or (not pla.Live) or pla.NoTarget or
                 (pla.FMegaRulez[MR_INVIS] > 0) then
                Target.UID := 0; // то забыть цель
            end
          else
            begin // Цель - монстр
              mon := g_Monsters_Get(Target.UID);
              if (mon = nil) or (not mon.Live) then
                Target.UID := 0; // то забыть цель
            end;
        end;
  end; // if Target.UID <> 0

  FTargetUID := Target.UID;

// Если возможных целей нет:
// (Атака чего-нибудь слева или справа)
  if targets = nil then
    if GetAIFlag('ATTACKLEFT') <> '' then
      begin // Если нужно атаковать налево
        RemoveAIFlag('ATTACKLEFT');

        SetAIFlag('NEEDJUMP', '1');

        if RunDirection() = D_RIGHT then
          begin // Идем не в ту сторону
            if (Healthy() > 1) and GetRnd(FDifficult.InvisFire) then
              begin // Если здоровы, то, возможно, стреляем бежим влево и стреляем
                SetAIFlag('NEEDFIRE', '1');
                SetAIFlag('GOLEFT', '1');
              end;
          end
        else
          begin // Идем в нужную сторону
            if GetRnd(FDifficult.InvisFire) then // Возможно, стреляем вслепую
              SetAIFlag('NEEDFIRE', '1');
            if Healthy() <= 1 then // Побиты - убегаем
              SetAIFlag('GORIGHT', '1');
          end;
      end
    else
      if GetAIFlag('ATTACKRIGHT') <> '' then
        begin // Если нужно атаковать направо
          RemoveAIFlag('ATTACKRIGHT');

          SetAIFlag('NEEDJUMP', '1');

          if RunDirection() = D_LEFT then
            begin // Идем не в ту сторону
              if (Healthy() > 1) and GetRnd(FDifficult.InvisFire) then
                begin // Если здоровы, то, возможно, бежим вправо и стреляем
                  SetAIFlag('NEEDFIRE', '1');
                  SetAIFlag('GORIGHT', '1');
                end;
            end
          else
            begin
              if GetRnd(FDifficult.InvisFire) then // Возможно, стреляем вслепую
                SetAIFlag('NEEDFIRE', '1');
              if Healthy() <= 1 then // Побиты - убегаем
                SetAIFlag('GOLEFT', '1');
            end;
        end;

// Если есть возможные цели:
// (Стреляем по направлению к целям)
  if (targets <> nil) and (GetAIFlag('NEEDFIRE') <> '') then
    for a := 0 to High(targets) do
      begin
      // Если можем стрелять по диагонали:
        if GetRnd(FDifficult.DiagFire) then
          begin
          // Ищем цель сверху и стреляем, если есть:
            if FDirection = D_LEFT then
              angle := ANGLE_LEFTUP
            else
              angle := ANGLE_RIGHTUP;

            firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
            fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

            if g_CollideLine(x1, y1, x1+firew, y1+fireh,
                  targets[a].X+targets[a].Rect.X+GetInterval(FDifficult.DiagPrecision, 128),
                  targets[a].Y+targets[a].Rect.Y+GetInterval(FDifficult.DiagPrecision, 128),
                  targets[a].Rect.Width, targets[a].Rect.Height) and
                g_TraceVector(x1, y1, targets[a].cX, targets[a].cY) then
              begin
                SetAIFlag('NEEDFIRE', '1');
                SetAIFlag('NEEDSEEUP', '1');
              end;

          // Ищем цель снизу и стреляем, если есть:
            if FDirection = D_LEFT then
              angle := ANGLE_LEFTDOWN
            else
              angle := ANGLE_RIGHTDOWN;

            firew := Trunc(Cos(DegToRad(-angle))*gPlayerScreenSize.X*0.6);
            fireh := Trunc(Sin(DegToRad(-angle))*gPlayerScreenSize.X*0.6);

            if g_CollideLine(x1, y1, x1+firew, y1+fireh,
                  targets[a].X+targets[a].Rect.X+GetInterval(FDifficult.DiagPrecision, 128),
                  targets[a].Y+targets[a].Rect.Y+GetInterval(FDifficult.DiagPrecision, 128),
                  targets[a].Rect.Width, targets[a].Rect.Height) and
                g_TraceVector(x1, y1, targets[a].cX, targets[a].cY) then
              begin
                SetAIFlag('NEEDFIRE', '1');
                SetAIFlag('NEEDSEEDOWN', '1');
              end;
          end;

      // Если цель "перед носом", то стреляем:
        if targets[a].Line and targets[a].Visible and
            (((FDirection = D_LEFT) and (targets[a].X < FObj.X)) or
            ((FDirection = D_RIGHT) and (targets[a].X > FObj.X))) then
        begin
          SetAIFlag('NEEDFIRE', '1');
          Break;
        end;
      end;

// Если летит пуля, то, возможно, подпрыгиваем:
  if g_Weapon_Danger(FUID, FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y,
                    PLAYER_RECT.Width, PLAYER_RECT.Height,
                    40+GetInterval(FDifficult.Cover, 40)) then
    SetAIFlag('NEEDJUMP', '1');

// Если кончились паторны, то нужно сменить оружие:
  ammo := GetAmmoByWeapon(FCurrWeap);
  if ((FCurrWeap = WEAPON_SHOTGUN2) and (ammo < 2)) or
      ((FCurrWeap = WEAPON_BFG) and (ammo < 40)) or
      (ammo = 0) then
    SetAIFlag('SELECTWEAPON', '1');

// Если нужно сменить оружие, то выбираем нужное:
  if GetAIFlag('SELECTWEAPON') = '1' then
  begin
    SelectWeapon(-1);
    RemoveAIFlag('SELECTWEAPON');
  end;
end;

procedure TBot.Update();
begin
  if not FLive then
  begin // Respawn
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

function TBot.GetAIFlag(fName: String20): String20;
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

procedure TBot.RemoveAIFlag(fName: String20);
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

procedure TBot.SetAIFlag(fName, fValue: String20);
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
// Возможно, нажимаем кнопку:
  if Rnd(16) then
    PressKey(KEY_OPEN, 0);

// Если под лифтом или ступеньками, то, возможно, прыгаем:
  if OnLadder() or ((BelowLadder() or BelowLiftUp()) and Rnd(8)) then
    begin
      ReleaseKey(KEY_LEFT);
      ReleaseKey(KEY_RIGHT);
      Jump();
    end;

// Идем влево, если надо было:
  if GetAIFlag('GOLEFT') <> '' then
    begin
      RemoveAIFlag('GOLEFT');
      if CanRunLeft() then
        GoLeft(360);
    end;

// Идем вправо, если надо было:
  if GetAIFlag('GORIGHT') <> '' then
    begin
      RemoveAIFlag('GORIGHT');
      if CanRunRight() then
        GoRight(360);
    end;

// Если вылетели за карту, то пробуем вернуться:
  if FObj.X < -32 then
    GoRight(360)
  else
    if FObj.X+32 > gMapInfo.Width then
      GoLeft(360);

// Прыгаем, если надо было:
  if GetAIFlag('NEEDJUMP') <> '' then
    begin
      Jump(0);
      RemoveAIFlag('NEEDJUMP');
    end;

// Смотрим вверх, если надо было:
  if GetAIFlag('NEEDSEEUP') <> '' then
    begin
      ReleaseKey(KEY_UP);
      ReleaseKey(KEY_DOWN);
      PressKey(KEY_UP, 20);
      RemoveAIFlag('NEEDSEEUP');
    end;

// Смотрим вниз, если надо было:
  if GetAIFlag('NEEDSEEDOWN') <> '' then
    begin
      ReleaseKey(KEY_UP);
      ReleaseKey(KEY_DOWN);
      PressKey(KEY_DOWN, 20);
      RemoveAIFlag('NEEDSEEDOWN');
    end;

// Если нужно было в дыру и мы не на земле, то покорно летим:
  if GetAIFlag('GOINHOLE') <> '' then
    if not OnGround() then
      begin
        ReleaseKey(KEY_LEFT);
        ReleaseKey(KEY_RIGHT);
        RemoveAIFlag('GOINHOLE');
        SetAIFlag('FALLINHOLE', '1');
      end;

// Если падали и достигли земли, то хватит падать:
  if GetAIFlag('FALLINHOLE') <> '' then
    if OnGround() then
      RemoveAIFlag('FALLINHOLE');

// Если летели прямо и сейчас не на лестнице или на вершине лифта, то отходим в сторону:
  if not (KeyPressed(KEY_LEFT) or KeyPressed(KEY_RIGHT)) then
    if GetAIFlag('FALLINHOLE') = '' then
      if (not OnLadder()) or (FObj.Vel.Y >= 0) or (OnTopLift()) then
        if Rnd(2) then
          GoLeft(360)
        else
          GoRight(360);

// Если на земле и можно подпрыгнуть, то, возможно, прыгаем:
  if OnGround() and
      CanJumpUp(IfThen(RunDirection() = D_LEFT, -1, 1)*32) and
      Rnd(8) then
    Jump();

// Если на земле и возле дыры (глубина > 2 ростов игрока):
  if OnGround() and NearHole() then
    if NearDeepHole() then // Если это бездна
      case Random(6) of
        0..3: Turn(); // Бежим обратно
        4: Jump(); // Прыгаем
        5: begin // Прыгаем обратно
             Turn();
             Jump();
           end;
      end
    else // Это не бездна и мы еще не летим туда
      if GetAIFlag('GOINHOLE') = '' then
        case Random(6) of
          0: Turn(); // Не нужно туда
          1: Jump(); // Вдруг повезет - прыгаем
          else // Если яма с границей, то при случае можно туда прыгнуть
            if BorderHole() then
              SetAIFlag('GOINHOLE', '1');
   end;

// Если на земле, но некуда идти:
  if (not CanRun()) and OnGround() then
    begin
    // Если мы на лестнице или можно перепрыгнуть, то прыгаем:
      if CanJumpOver() or OnLadder() then
        Jump()
      else // иначе попытаемся в другую сторону
        if Random(2) = 0 then
          PressKey(KEY_OPEN, 0)
        else
          Turn();
    end;
end;

function TBot.FullInStep(XInc, YInc: Integer): Boolean;
begin
  Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc,
                               PLAYER_RECT.Width, PLAYER_RECT.Height, PANEL_STEP, False);
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
  begin // Дальний бой
    for a := 0 to 9 do
      if FWeapon[FDifficult.WeaponPrior[a]] and HaveAmmo(FDifficult.WeaponPrior[a]) then
      begin
        FSelectedWeapon := FDifficult.WeaponPrior[a];
        Break;
      end;
  end
  else //if Dist > BOT_UNSAFEDIST then
  begin // Ближний бой
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
  if FMegaRulez[MR_INVUL] > 0 then Result := 3
  else if (FHealth > 80) or ((FHealth > 50) and (FArmor > 20)) then Result := 3
  else if (FHealth > 50) then Result := 2
  else if (FHealth > 20) then Result := 1
  else Result := 0;
end;

function TBot.TargetOnScreen(TX, TY: Integer): Boolean;
begin
  Result := (Abs(FObj.X-TX) <= Trunc(gPlayerScreenSize.X*0.6)) and
            (Abs(FObj.Y-TY) <= Trunc(gPlayerScreenSize.Y*0.6));
end;

procedure TBot.OnDamage(Angle: SmallInt);
var
  pla: TPlayer;
  mon: TMonster;
  ok: Boolean;
begin
  inherited;

  if (Angle = 0) or (Angle = 180) then
    begin
      ok := False;
      if (g_GetUIDType(FLastSpawnerUID) = UID_PLAYER) and
          LongBool(gGameSettings.Options and GAME_OPTION_BOTVSPLAYER) then
        begin // Игрок
          pla := g_Player_Get(FLastSpawnerUID);
          ok := not TargetOnScreen(pla.FObj.X + PLAYER_RECT.X,
                                   pla.FObj.Y + PLAYER_RECT.Y);
        end
      else
        if (g_GetUIDType(FLastSpawnerUID) = UID_MONSTER) and
           LongBool(gGameSettings.Options and GAME_OPTION_BOTVSMONSTER) then
        begin // Монстр
          mon := g_Monsters_Get(FLastSpawnerUID);
          ok := not TargetOnScreen(mon.Obj.X + mon.Obj.Rect.X,
                                   mon.Obj.Y + mon.Obj.Rect.Y);
        end;

      if ok then
        if Angle = 0 then
          SetAIFlag('ATTACKLEFT', '1')
        else
          SetAIFlag('ATTACKRIGHT', '1');
    end;
end;

function TBot.RunDirection(): TDirection;
begin
  if Abs(Vel.X) >= 1 then
  begin
    if Vel.X > 0 then Result := D_RIGHT else Result := D_LEFT;
  end else
    Result := FDirection;
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

procedure TBot.SaveState(var Mem: TBinMemoryWriter);
var
  i: Integer;
  dw: DWORD;
  p: Pointer;
begin
  inherited SaveState(Mem);

// Выбранное оружие:
  Mem.WriteByte(FSelectedWeapon);
// UID цели:
  Mem.WriteWord(FTargetUID);
// Время потери цели:
  Mem.WriteDWORD(FLastVisible);
// Количество флагов ИИ:
  dw := Length(FAIFlags);
  Mem.WriteDWORD(dw);
// Флаги ИИ:
  for i := 0 to Integer(dw)-1 do
  begin
    Mem.WriteString(FAIFlags[i].Name, 20);
    Mem.WriteString(FAIFlags[i].Value, 20);
  end;
// Настройки сложности:
  p := @FDifficult;
  Mem.WriteMemory(p, SizeOf(TDifficult));
end;

procedure TBot.LoadState(var Mem: TBinMemoryReader);
var
  i: Integer;
  dw: DWORD;
  p: Pointer;
begin
  inherited LoadState(Mem);

// Выбранное оружие:
  Mem.ReadByte(FSelectedWeapon);
// UID цели:
  Mem.ReadWord(FTargetUID);
// Время потери цели:
  Mem.ReadDWORD(FLastVisible);
// Количество флагов ИИ:
  Mem.ReadDWORD(dw);
  SetLength(FAIFlags, dw);
// Флаги ИИ:
  for i := 0 to Integer(dw)-1 do
  begin
    Mem.ReadString(FAIFlags[i].Name);
    Mem.ReadString(FAIFlags[i].Value);
  end;
// Настройки сложности:
  Mem.ReadMemory(p, dw);
  if dw <> SizeOf(TDifficult) then
  begin
    raise EBinSizeError.Create('TBot.LoadState: Wrong FDifficult Size');
  end;
  FDifficult := TDifficult(p^);
end;

end.
