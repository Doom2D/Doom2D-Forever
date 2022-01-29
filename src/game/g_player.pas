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
unit g_player;

interface

uses
  SysUtils, Classes,
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  g_base, g_playermodel, g_basic, g_textures,
  g_weapons, g_phys, g_sound, g_saveload, MAPDEF,
  g_panel;

const
  KEY_LEFT       = 1;
  KEY_RIGHT      = 2;
  KEY_UP         = 3;
  KEY_DOWN       = 4;
  KEY_FIRE       = 5;
  KEY_OPEN       = 6;
  KEY_JUMP       = 7;
  KEY_CHAT       = 8;

  WP_PREV        = 0;
  WP_NEXT        = 1;
  WP_FACT        = WP_PREV;
  WP_LACT        = WP_NEXT;

  R_ITEM_BACKPACK   = 0;
  R_KEY_RED         = 1;
  R_KEY_GREEN       = 2;
  R_KEY_BLUE        = 3;
  R_BERSERK         = 4;

  MR_SUIT           = 0;
  MR_INVUL          = 1;
  MR_INVIS          = 2;
  MR_MAX            = 2;

  A_BULLETS         = 0;
  A_SHELLS          = 1;
  A_ROCKETS         = 2;
  A_CELLS           = 3;
  A_FUEL            = 4;
  A_HIGH            = 4;

  AmmoLimits: Array [0..1] of Array [A_BULLETS..A_HIGH] of Word =
  ((200,  50,  50, 300, 100),
   (400, 100, 100, 600, 200));

  K_SIMPLEKILL      = 0;
  K_HARDKILL        = 1;
  K_EXTRAHARDKILL   = 2;
  K_FALLKILL        = 3;

  T_RESPAWN         = 0;
  T_SWITCH          = 1;
  T_USE             = 2;
  T_FLAGCAP         = 3;

  TEAM_NONE         = 0;
  TEAM_RED          = 1;
  TEAM_BLUE         = 2;
  TEAM_COOP         = 3;

  ANGLE_NONE        = Low(SmallInt);

  PLAYER_RECT: TRectWH = (X:15; Y:12; Width:34; Height:52);
  PLAYER_RECT_CX       = 15+(34 div 2);
  PLAYER_RECT_CY       = 12+(52 div 2);

  PLAYER_HP_SOFT  = 100;
  PLAYER_HP_LIMIT = 200;
  PLAYER_AP_SOFT  = 100;
  PLAYER_AP_LIMIT = 200;
  SUICIDE_DAMAGE  = 112;
  WEAPON_DELAY    = 5;

  PLAYER_BURN_TIME = 110;

  PLAYER1_DEF_COLOR: TRGB = (R:64; G:175; B:48);
  PLAYER2_DEF_COLOR: TRGB = (R:96; G:96; B:96);

  AIR_DEF = 360;
  AIR_MAX = 1091;
  JET_MAX = 540; // ~30 sec
  ANGLE_RIGHTUP   = 55;
  ANGLE_RIGHTDOWN = -35;
  ANGLE_LEFTUP    = 125;
  ANGLE_LEFTDOWN  = -145;
  WEAPONPOINT: Array [TDirection] of TDFPoint = ((X:16; Y:32), (X:47; Y:32));
  TEAMCOLOR: Array [TEAM_RED..TEAM_BLUE] of TRGB = ((R:255; G:0; B:0),
                                                   (R:0; G:0; B:255));

type
  TPlayerStat = record
    Num: Integer;
    Ping: Word;
    Loss: Byte;
    Name: String;
    Team: Byte;
    Frags: SmallInt;
    Deaths: SmallInt;
    Lives: Byte;
    Kills: Word;
    Color: TRGB;
    Spectator: Boolean;
    UID: Word;
  end;

  TPlayerStatArray = Array of TPlayerStat;

  TPlayerSavedState = record
    Health:     Integer;
    Armor:      Integer;
    Air:        Integer;
    JetFuel:    Integer;
    CurrWeap:   Byte;
    NextWeap:   WORD;
    NextWeapDelay: Byte;
    Ammo:       Array [A_BULLETS..A_HIGH] of Word;
    MaxAmmo:    Array [A_BULLETS..A_HIGH] of Word;
    Weapon:     Array [WP_FIRST..WP_LAST] of Boolean;
    Rulez:      Set of R_ITEM_BACKPACK..R_BERSERK;
    Used:       Boolean;
  end;

  TKeyState = record
    Pressed: Boolean;
    Time: Word;
  end;

  TPlayer = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    FIamBot:    Boolean;
    FUID:       Word;
    FName:      String;
    FTeam:      Byte;
    FAlive:     Boolean;
    FSpawned:   Boolean;
    FDirection: TDirection;
    FHealth:    Integer;
    FLives:     Byte;
    FArmor:     Integer;
    FAir:       Integer;
    FPain:      Integer;
    FPickup:    Integer;
    FKills:     Integer;
    FMonsterKills: Integer;
    FFrags:     Integer;
    FFragCombo: Byte;
    FLastFrag:  LongWord;
    FComboEvnt: Integer;
    FDeath:     Integer;
    FCanJetpack: Boolean;
    FJetFuel:   Integer;
    FFlag:      Byte;
    FSecrets:   Integer;
    FCurrWeap:  Byte;
    FNextWeap:  WORD;
    FNextWeapDelay: Byte; // frames
    FBFGFireCounter: SmallInt;
    FLastSpawnerUID: Word;
    FLastHit:   Byte;
    FObj:       TObj;
    FXTo, FYTo: Integer;
    FSpectatePlayer: Integer;
    FFirePainTime:   Integer;
    FFireAttacker:   Word;

    FSavedStateNum:   Integer;

    FModel:     TPlayerModel;
    FPunchAnim: TAnimationState;
    FActionPrior:    Byte;
    FActionAnim:     Byte;
    FActionForce:    Boolean;
    FActionChanged:  Boolean;
    FAngle:     SmallInt;
    FFireAngle: SmallInt;
    FIncCamOld:      Integer;
    FIncCam:         Integer;
    FSlopeOld:       Integer;
    {$IFDEF ENABLE_SHELLS}
      FShellTimer:     Integer;
      FShellType:      Byte;
    {$ENDIF}
    FSawSound:       TPlayableSound;
    FSawSoundIdle:   TPlayableSound;
    FSawSoundHit:    TPlayableSound;
    FSawSoundSelect: TPlayableSound;
    FFlameSoundOn:   TPlayableSound;
    FFlameSoundOff:  TPlayableSound;
    FFlameSoundWork: TPlayableSound;
    FJetSoundOn:     TPlayableSound;
    FJetSoundOff:    TPlayableSound;
    FJetSoundFly:    TPlayableSound;
    FGodMode:   Boolean;
    FNoTarget:  Boolean;
    FNoReload:  Boolean;
    FJustTeleported: Boolean;
    FNetTime: LongWord;
    mEDamageType: Integer;


    function CollideLevel(XInc, YInc: Integer): Boolean;
    function StayOnStep(XInc, YInc: Integer): Boolean;
    function HeadInLiquid(XInc, YInc: Integer): Boolean;
    function BodyInLiquid(XInc, YInc: Integer): Boolean;
    function BodyInAcid(XInc, YInc: Integer): Boolean;
    function FullInLift(XInc, YInc: Integer): Integer;
    {procedure CollideItem();}
    procedure FlySmoke(Times: DWORD = 1);
    procedure OnFireFlame(Times: DWORD = 1);
    procedure SetAction(Action: Byte; Force: Boolean = False);
    procedure OnDamage(Angle: SmallInt); virtual;
    function firediry(): Integer;
    procedure DoPunch();

    procedure Run(Direction: TDirection);
    procedure NextWeapon();
    procedure PrevWeapon();
    procedure SeeUp();
    procedure SeeDown();
    procedure Fire();
    procedure Jump();
    procedure Use();

    function getNextWeaponIndex (): Byte; // return 255 for "no switch"
    procedure resetWeaponQueue ();
    function hasAmmoForWeapon (weapon: Byte): Boolean;
    function hasAmmoForShooting (weapon: Byte): Boolean;
    function shouldSwitch (weapon: Byte; hadWeapon: Boolean) : Boolean;

    procedure doDamage (v: Integer);

  public
    FDamageBuffer:   Integer;

    FAmmo:      Array [A_BULLETS..A_HIGH] of Word;
    FMaxAmmo:   Array [A_BULLETS..A_HIGH] of Word;
    FWeapon:    Array [WP_FIRST..WP_LAST] of Boolean;
    FRulez:     Set of R_ITEM_BACKPACK..R_BERSERK;
    FBerserk:   Integer;
    FMegaRulez: Array [MR_SUIT..MR_MAX] of DWORD;
    FReloading: Array [WP_FIRST..WP_LAST] of Word;
    FTime:      Array [T_RESPAWN..T_FLAGCAP] of DWORD;
    FKeys:      Array [KEY_LEFT..KEY_CHAT] of TKeyState;
    FWeapSwitchMode: Byte;
    FWeapPreferences: Array [WP_FIRST .. WP_LAST+1] of Byte;
    FSwitchToEmpty: Byte;
    FSkipFist: Byte;
    FColor:     TRGB;
    FPreferredTeam: Byte;
    FSpectator: Boolean;
    FNoRespawn: Boolean;
    FWantsInGame: Boolean;
    FGhost:     Boolean;
    FPhysics:   Boolean;
    FFlaming:   Boolean;
    FJetpack:   Boolean;
    FActualModelName: string;
    FClientID:  SmallInt;
    FPing:      Word;
    FLoss:      Byte;
    FReady:     Boolean;
    FDummy:     Boolean;
    FFireTime:  Integer;
    FSpawnInvul: Integer;
    FHandicap:  Integer;
    FWaitForFirstSpawn: Boolean; // set to `true` in server, used to spawn a player on first full state request

    {$IFDEF ENABLE_CORPSES}
      FCorpse:    Integer;
    {$ENDIF}

    // debug: viewport offset
    viewPortX, viewPortY, viewPortW, viewPortH: Integer;

    function isValidViewPort (): Boolean; inline;

    constructor Create(); virtual;
    destructor  Destroy(); override;
    procedure   Respawn(Silent: Boolean; Force: Boolean = False); virtual;
    function    GetRespawnPoint(): Byte;
    procedure   PressKey(Key: Byte; Time: Word = 1);
    procedure   ReleaseKeys();
    procedure   SetModel(ModelName: String);
    procedure   SetColor(Color: TRGB);
    function    GetColor(): TRGB;
    procedure   SetWeapon(W: Byte);
    function    IsKeyPressed(K: Byte): Boolean;
    function    GetKeys(): Byte;
    function    PickItem(ItemType: Byte; arespawn: Boolean; var remove: Boolean): Boolean; virtual;
    procedure   SetWeaponPrefs(Prefs: Array of Byte);
    procedure   SetWeaponPref(Weapon, Pref: Byte);
    function    GetWeaponPref(Weapon: Byte) : Byte;
    function    GetMorePrefered() : Byte;
    function    MaySwitch(Weapon: Byte) : Boolean;
    function    Collide(X, Y: Integer; Width, Height: Word): Boolean; overload;
    function    Collide(Panel: TPanel): Boolean; overload;
    function    Collide(X, Y: Integer): Boolean; overload;
    procedure   SetDirection(Direction: TDirection);
    procedure   GetSecret();
    function    TeleportTo(X, Y: Integer; silent: Boolean; dir: Byte): Boolean;
    procedure   Touch();
    procedure   Push(vx, vy: Integer);
    procedure   ChangeModel(ModelName: String);
    procedure   SwitchTeam;
    procedure   ChangeTeam(Team: Byte);
    procedure   BFGHit();
    function    GetFlag(Flag: Byte): Boolean;
    procedure   SetFlag(Flag: Byte);
    function    DropFlag(Silent: Boolean = True; DoThrow: Boolean = False): Boolean;
    function    TryDropFlag(): Boolean;
    procedure   AllRulez(Health: Boolean);
    procedure   RestoreHealthArmor();
    procedure   FragCombo();
    procedure   GiveItem(ItemType: Byte);
    procedure   Damage(value: Word; SpawnerUID: Word; vx, vy: Integer; t: Byte); virtual;
    function    Heal(value: Word; Soft: Boolean): Boolean; virtual;
    procedure   MakeBloodVector(Count: Word; VelX, VelY: Integer);
    procedure   MakeBloodSimple(Count: Word);
    procedure   Kill(KillType: Byte; SpawnerUID: Word; t: Byte);
    procedure   Reset(Force: Boolean);
    procedure   Spectate(NoMove: Boolean = False);
    procedure   SwitchNoClip;
    procedure   SoftReset();
    procedure   PreUpdate();
    procedure   Update(); virtual;
    procedure   RememberState();
    procedure   RecallState();
    procedure   SaveState (st: TStream); virtual;
    procedure   LoadState (st: TStream); virtual;
    procedure   PauseSounds(Enable: Boolean);
    procedure   NetFire(Wpn: Byte; X, Y, AX, AY: Integer; WID: Integer = -1);
    procedure   DoLerp(Level: Integer = 2);
    procedure   SetLerp(XTo, YTo: Integer);
    procedure   ProcessWeaponAction(Action: Byte);
    procedure   QueueWeaponSwitch(Weapon: Byte);
    procedure   RealizeCurrentWeapon();
    procedure   FlamerOn;
    procedure   FlamerOff;
    procedure   JetpackOn;
    procedure   JetpackOff;
    procedure   CatchFire(Attacker: Word; Timeout: Integer = PLAYER_BURN_TIME);

    //WARNING! this does nothing for now, but still call it!
    procedure positionChanged (); //WARNING! call this after entity position was changed, or coldet will not work right!

    procedure getMapBox (out x, y, w, h: Integer); inline;
    procedure moveBy (dx, dy: Integer); inline;

    function GetAmmoByWeapon(Weapon: Byte): Word; // private state

  public
    property    Vel: TPoint2i read FObj.Vel;
    property    Obj: TObj read FObj;

    property    Name: String read FName write FName;
    property    Model: TPlayerModel read FModel;
    property    Health: Integer read FHealth write FHealth;
    property    Lives: Byte read FLives write FLives;
    property    Armor: Integer read FArmor write FArmor;
    property    Air:   Integer read FAir write FAir;
    property    JetFuel: Integer read FJetFuel write FJetFuel;
    property    Frags: Integer read FFrags write FFrags;
    property    Death: Integer read FDeath write FDeath;
    property    Kills: Integer read FKills write FKills;
    property    CurrWeap: Byte read FCurrWeap write FCurrWeap;
    property    WeapSwitchMode: Byte read FWeapSwitchMode write FWeapSwitchMode;
    property    SwitchToEmpty: Byte read FSwitchToEmpty write FSwitchToEmpty;
    property    SkipFist: Byte read FSkipFist write FSkipFist;
    property    MonsterKills: Integer read FMonsterKills write FMonsterKills;
    property    Secrets: Integer read FSecrets;
    property    GodMode: Boolean read FGodMode write FGodMode;
    property    NoTarget: Boolean read FNoTarget write FNoTarget;
    property    NoReload: Boolean read FNoReload write FNoReload;
    property    alive: Boolean read FAlive write FAlive;
    property    Flag: Byte read FFlag;
    property    Team: Byte read FTeam write FTeam;
    property    Direction: TDirection read FDirection;
    property    GameX: Integer read FObj.X write FObj.X;
    property    GameY: Integer read FObj.Y write FObj.Y;
    property    GameVelX: Integer read FObj.Vel.X write FObj.Vel.X;
    property    GameVelY: Integer read FObj.Vel.Y write FObj.Vel.Y;
    property    GameAccelX: Integer read FObj.Accel.X write FObj.Accel.X;
    property    GameAccelY: Integer read FObj.Accel.Y write FObj.Accel.Y;
    property    IncCam: Integer read FIncCam write FIncCam;
    property    IncCamOld: Integer read FIncCamOld write FIncCamOld;
    property    SlopeOld: Integer read FSlopeOld write FSlopeOld;
    property    UID: Word read FUID write FUID;
    property    JustTeleported: Boolean read FJustTeleported write FJustTeleported;
    property    NetTime: LongWord read FNetTime write FNetTime;

    (* internal state *)
    property    Angle_: SmallInt read FAngle;
    property    Spectator: Boolean read FSpectator;
    property    NoRespawn: Boolean read FNoRespawn;
    property    Berserk: Integer read FBerserk;
    property    Pain: Integer read FPain;
    property    Pickup: Integer read FPickup;
    property    PunchAnim: TAnimationState read FPunchAnim write FPunchAnim;
    property    SpawnInvul: Integer read FSpawnInvul;
    property    Ghost: Boolean read FGhost;

  published
    property eName: String read FName write FName;
    property eHealth: Integer read FHealth write FHealth;
    property eLives: Byte read FLives write FLives;
    property eArmor: Integer read FArmor write FArmor;
    property eAir:   Integer read FAir write FAir;
    property eJetFuel: Integer read FJetFuel write FJetFuel;
    property eFrags: Integer read FFrags write FFrags;
    property eDeath: Integer read FDeath write FDeath;
    property eKills: Integer read FKills write FKills;
    property eCurrWeap: Byte read FCurrWeap write FCurrWeap;
    property eMonsterKills: Integer read FMonsterKills write FMonsterKills;
    property eSecrets: Integer read FSecrets write FSecrets;
    property eGodMode: Boolean read FGodMode write FGodMode;
    property eNoTarget: Boolean read FNoTarget write FNoTarget;
    property eNoReload: Boolean read FNoReload write FNoReload;
    property eAlive: Boolean read FAlive write FAlive;
    property eFlag: Byte read FFlag;
    property eTeam: Byte read FTeam write FTeam;
    property eDirection: TDirection read FDirection;
    property eGameX: Integer read FObj.X write FObj.X;
    property eGameY: Integer read FObj.Y write FObj.Y;
    property eGameVelX: Integer read FObj.Vel.X write FObj.Vel.X;
    property eGameVelY: Integer read FObj.Vel.Y write FObj.Vel.Y;
    property eGameAccelX: Integer read FObj.Accel.X write FObj.Accel.X;
    property eGameAccelY: Integer read FObj.Accel.Y write FObj.Accel.Y;
    property eIncCam: Integer read FIncCam write FIncCam;
    property eUID: Word read FUID;
    property eJustTeleported: Boolean read FJustTeleported;
    property eNetTime: LongWord read FNetTime;

    // set this before assigning something to `eDamage`
    property eDamageType: Integer read mEDamageType write mEDamageType;
    property eDamage: Integer write doDamage;

    {$IFDEF ENABLE_CORPSES}
      property Corpse: Integer read FCorpse;
    {$ENDIF}
  end;

  TDifficult = record
  public
    DiagFire: Byte;
    InvisFire: Byte;
    DiagPrecision: Byte;
    FlyPrecision: Byte;
    Cover: Byte;
    CloseJump: Byte;
    WeaponPrior: packed array [WP_FIRST..WP_LAST] of Byte;
    CloseWeaponPrior: packed array [WP_FIRST..WP_LAST] of Byte;
    //SafeWeaponPrior: Array [WP_FIRST..WP_LAST] of Byte;

  public
    procedure save (st: TStream);
    procedure load (st: TStream);
  end;

  TAIFlag = record
    Name: String;
    Value: String;
  end;

  TBot = class(TPlayer)
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
    procedure   SetAIFlag(aName, fValue: String20);
    function    GetAIFlag(aName: String20): String20;
    procedure   RemoveAIFlag(aName: String20);
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
    function    PickItem(ItemType: Byte; force: Boolean; var remove: Boolean): Boolean; override;
    function    Heal(value: Word; Soft: Boolean): Boolean; override;
    procedure   Update(); override;
    procedure   SaveState (st: TStream); override;
    procedure   LoadState (st: TStream); override;
  end;

  TTeamStat = Array [TEAM_RED..TEAM_BLUE] of
    record
      Score: SmallInt;
    end;

var
  gPlayers: Array of TPlayer;
  gTeamStat: TTeamStat;
  gFly: Boolean = False;
  gAimLine: Boolean = False;
  gChatBubble: Integer = 0;
  gPlayerIndicator: Integer = 1;
  gPlayerIndicatorStyle: Integer = 0;
  gNumBots: Word = 0;
  gSpectLatchPID1: Word = 0;
  gSpectLatchPID2: Word = 0;
  MAX_RUNVEL: Integer = 8;
  VEL_JUMP: Integer = 10;

function  Lerp(X, Y, Factor: Integer): Integer;

procedure g_Force_Model_Set(Mode: Word);
function g_Force_Model_Get(): Word;
procedure g_Forced_Model_SetName(Model: String);
function  g_Forced_Model_GetName(): String;

procedure g_Player_Init();
procedure g_Player_Free();
function  g_Player_Create(ModelName: String; Color: TRGB; Team: Byte; Bot: Boolean): Word;
function  g_Player_CreateFromState (st: TStream): Word;
procedure g_Player_Remove(UID: Word);
procedure g_Player_ResetTeams();
procedure g_Player_PreUpdate();
procedure g_Player_UpdateAll();
procedure g_Player_RememberAll();
procedure g_Player_ResetAll(Force, Silent: Boolean);
function  g_Player_Get(UID: Word): TPlayer;
function  g_Player_GetCount(): Byte;
function  g_Player_GetStats(): TPlayerStatArray;
function  g_Player_ValidName(Name: String): Boolean;
procedure g_Player_ResetReady();
procedure g_Bot_Add(Team, Difficult: Byte; Handicap: Integer = 100);
procedure g_Bot_AddList(Team: Byte; lname: ShortString; num: Integer = -1; Handicap: Integer = 100);
procedure g_Bot_MixNames();
procedure g_Bot_RemoveAll();
function g_Bot_GetCount(): Integer;

implementation

uses
  {$IFDEF ENABLE_HOLMES}
    g_holmes,
  {$ENDIF}
  {$IFDEF ENABLE_MENU}
    g_menu,
  {$ENDIF}
  {$IFDEF ENABLE_RENDER}
    r_render,
  {$ENDIF}
  {$IFDEF ENABLE_GFX}
    g_gfx,
  {$ENDIF}
  {$IFDEF ENABLE_GIBS}
    g_gibs,
  {$ENDIF}
  {$IFDEF ENABLE_SHELLS}
    g_shells,
  {$ENDIF}
  {$IFDEF ENABLE_CORPSES}
    g_corpses,
  {$ENDIF}
  e_log, g_map, g_items, g_console, Math,
  g_options, g_triggers, g_game, g_grid, e_res,
  wadreader, g_monsters, CONFIG, g_language,
  g_net, g_netmsg,
  utils, xstreams;

const PLR_SAVE_VERSION = 0;

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
    w_prior1: Array [WP_FIRST..WP_LAST] of Byte;
    w_prior2: Array [WP_FIRST..WP_LAST] of Byte;
    w_prior3: Array [WP_FIRST..WP_LAST] of Byte;
  end;

const
  TIME_RESPAWN1 = 1500;
  TIME_RESPAWN2 = 2000;
  TIME_RESPAWN3 = 3000;
  PLAYER_SUIT_TIME    = 30000;
  PLAYER_INVUL_TIME   = 30000;
  PLAYER_INVIS_TIME   = 35000;
  FRAG_COMBO_TIME = 3000;
  VEL_SW  = 4;
  VEL_FLY = 6;
  PLAYER_HEADRECT: TRectWH = (X:24; Y:12; Width:20; Height:12);
  BOT_MAXJUMP = 84;
  BOT_LONGDIST   = 300;
  BOT_UNSAFEDIST = 128;
  DIFFICULT_EASY: TDifficult = (DiagFire: 32; InvisFire: 32; DiagPrecision: 32;
                                FlyPrecision: 32; Cover: 32; CloseJump: 32;
                                WeaponPrior:(0,0,0,0,0,0,0,0,0,0,0); CloseWeaponPrior:(0,0,0,0,0,0,0,0,0,0,0));
  DIFFICULT_MEDIUM: TDifficult = (DiagFire: 127; InvisFire: 127; DiagPrecision: 127;
                                  FlyPrecision: 127; Cover: 127; CloseJump: 127;
                                WeaponPrior:(0,0,0,0,0,0,0,0,0,0,0); CloseWeaponPrior:(0,0,0,0,0,0,0,0,0,0,0));
  DIFFICULT_HARD: TDifficult = (DiagFire: 255; InvisFire: 255; DiagPrecision: 255;
                                FlyPrecision: 255; Cover: 255; CloseJump: 255;
                                WeaponPrior:(0,0,0,0,0,0,0,0,0,0,0); CloseWeaponPrior:(0,0,0,0,0,0,0,0,0,0,0));
  WEAPON_PRIOR1: Array [WP_FIRST..WP_LAST] of Byte =
                                (WEAPON_FLAMETHROWER, WEAPON_SUPERPULEMET,
                                 WEAPON_SHOTGUN2, WEAPON_SHOTGUN1,
                                 WEAPON_CHAINGUN, WEAPON_PLASMA, WEAPON_ROCKETLAUNCHER,
                                 WEAPON_BFG, WEAPON_PISTOL, WEAPON_SAW, WEAPON_KASTET);
  WEAPON_PRIOR2: Array [WP_FIRST..WP_LAST] of Byte =
                                (WEAPON_FLAMETHROWER, WEAPON_SUPERPULEMET,
                                 WEAPON_BFG, WEAPON_ROCKETLAUNCHER,
                                 WEAPON_SHOTGUN2, WEAPON_PLASMA, WEAPON_SHOTGUN1,
                                 WEAPON_CHAINGUN, WEAPON_PISTOL, WEAPON_SAW, WEAPON_KASTET);
  //WEAPON_PRIOR3: Array [WP_FIRST..WP_LAST] of Byte =
  //                              (WEAPON_FLAMETHROWER, WEAPON_SUPERPULEMET,
  //                               WEAPON_BFG, WEAPON_PLASMA, WEAPON_SHOTGUN2,
  //                               WEAPON_CHAINGUN, WEAPON_SHOTGUN1, WEAPON_SAW,
  //                               WEAPON_ROCKETLAUNCHER, WEAPON_PISTOL, WEAPON_KASTET);
  WEAPON_RELOAD: Array [WP_FIRST..WP_LAST] of Byte =
                                (5, 2, 6, 18, 36, 2, 12, 2, 14, 2, 2);

  PLAYER_SIGNATURE = $52594C50; // 'PLYR'
  CORPSE_SIGNATURE = $50524F43; // 'CORP'

  BOTNAMES_FILENAME = 'botnames.txt';
  BOTLIST_FILENAME = 'botlist.txt';

var
  ForceModel: Word = 0;
  ForcedModelName: String = STD_PLAYER_MODEL;
  BotNames: Array of String;
  BotList: Array of TBotProfile;
  SavedStates: Array of TPlayerSavedState;


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

procedure g_Force_Model_Set(Mode: Word);
begin
  ForceModel := Mode;
end;

function g_Force_Model_Get(): Word;
begin
  Result := ForceModel;
end;

procedure g_Forced_Model_SetName(Model: String);
begin
  ForcedModelName := Model;
end;

function g_Forced_Model_GetName(): String;
begin
  Result := ForcedModelName;
end;

function g_Player_Create(ModelName: String; Color: TRGB; Team: Byte; Bot: Boolean): Word;
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
  if Bot and (g_Force_Model_Get() <> 0) then
    gPlayers[a].SetModel(g_Forced_Model_GetName());

// Нет модели - создание не возможно:
  if gPlayers[a].FModel = nil then
  begin
    gPlayers[a].Free();
    gPlayers[a] := nil;
    g_FatalError(Format(_lc[I_GAME_ERROR_MODEL], [ModelName]));
    Exit;
  end;

  if not (Team in [TEAM_RED, TEAM_BLUE]) then
    if Random(2) = 0 then
      Team := TEAM_RED
    else
      Team := TEAM_BLUE;
  gPlayers[a].FPreferredTeam := Team;

  case gGameSettings.GameMode of
    GM_DM: gPlayers[a].FTeam := TEAM_NONE;
    GM_TDM,
    GM_CTF: gPlayers[a].FTeam := gPlayers[a].FPreferredTeam;
    GM_SINGLE,
    GM_COOP: gPlayers[a].FTeam := TEAM_COOP;
  end;

// Если командная игра - красим модель в цвет команды:
  gPlayers[a].FColor := Color;
  if gPlayers[a].FTeam in [TEAM_RED, TEAM_BLUE] then
    gPlayers[a].FModel.Color := TEAMCOLOR[gPlayers[a].FTeam]
  else
    gPlayers[a].FModel.Color := Color;

  gPlayers[a].FUID := g_CreateUID(UID_PLAYER);
  gPlayers[a].FAlive := False;

  Result := gPlayers[a].FUID;
end;

function g_Player_CreateFromState (st: TStream): Word;
  var a: Integer; ok, Bot: Boolean; pos: Int64;
begin
  assert(st <> nil);

  // check signature and entity type
  pos := st.Position;
  if not utils.checkSign(st, 'PLYR') then raise XStreamError.Create('invalid player signature');
  if (utils.readByte(st) <> PLR_SAVE_VERSION) then raise XStreamError.Create('invalid player version');
  Bot := utils.readBool(st);
  st.Position := pos;

  // find free player slot
  ok := false;
  for a := 0 to High(gPlayers) do
    if gPlayers[a] = nil then
    begin
      ok := true;
      break;
    end;

  // allocate player slot
  if not ok then
  begin
    SetLength(gPlayers, Length(gPlayers)+1);
    a := High(gPlayers);
  end;

  // create entity and load state
  if Bot then
  begin
    gPlayers[a] := TBot.Create();
    if (g_Force_Model_Get() <> 0) then
      gPlayers[a].SetModel(g_Forced_Model_GetName());
  end
  else
    gPlayers[a] := TPlayer.Create();
  gPlayers[a].FPhysics := True; // ???
  gPlayers[a].LoadState(st);

  result := gPlayers[a].FUID;
end;


procedure g_Player_ResetTeams();
var
  a: Integer;
begin
  if g_Game_IsClient then
    Exit;
  if gPlayers = nil then
    Exit;
  for a := Low(gPlayers) to High(gPlayers) do
    if gPlayers[a] <> nil then
      case gGameSettings.GameMode of
        GM_DM:
          gPlayers[a].ChangeTeam(TEAM_NONE);
        GM_TDM, GM_CTF:
          if not (gPlayers[a].Team in [TEAM_RED, TEAM_BLUE]) then
            if gPlayers[a].FPreferredTeam in [TEAM_RED, TEAM_BLUE] then
              gPlayers[a].ChangeTeam(gPlayers[a].FPreferredTeam)
            else
              if a mod 2 = 0 then
                gPlayers[a].ChangeTeam(TEAM_RED)
              else
                gPlayers[a].ChangeTeam(TEAM_BLUE);
        GM_SINGLE,
        GM_COOP:
          gPlayers[a].ChangeTeam(TEAM_COOP);
      end;
end;

procedure g_Bot_Add(Team, Difficult: Byte; Handicap: Integer = 100);
var
  m: SSArray;
  _name, _model: String;
  a, tr, tb: Integer;
begin
  if not g_Game_IsServer then Exit;

  if (g_Bot_GetCount() >= gMaxBots) then Exit;

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

// Выбираем случайную модель:
  _model := m[Random(Length(m))];

// Создаем бота:
  with g_Player_Get(g_Player_Create(_model,
                                    _RGB(Min(Random(9)*32, 255),
                                         Min(Random(9)*32, 255),
                                         Min(Random(9)*32, 255)),
                                    Team, True)) as TBot do
  begin
  // Если имени нет, делаем его из UID бота
    if _name = '' then
      Name := Format('DFBOT%.5d', [UID])
    else
      Name := _name;

    case Difficult of
      1: FDifficult := DIFFICULT_EASY;
      2: FDifficult := DIFFICULT_MEDIUM;
      else FDifficult := DIFFICULT_HARD;
    end;

    for a := WP_FIRST to WP_LAST do
    begin
      FDifficult.WeaponPrior[a] := WEAPON_PRIOR1[a];
      FDifficult.CloseWeaponPrior[a] := WEAPON_PRIOR2[a];
      //FDifficult.SafeWeaponPrior[a] := WEAPON_PRIOR3[a];
    end;

    FHandicap := Handicap;

    g_Console_Add(Format(_lc[I_PLAYER_JOIN], [Name]), True);

    if g_Game_IsNet then MH_SEND_PlayerCreate(UID);
    if g_Game_IsServer and (gGameSettings.MaxLives > 0) then
      Spectate();
  end;
end;

procedure g_Bot_AddList(Team: Byte; lName: ShortString; num: Integer = -1; Handicap: Integer = 100);
var
  m: SSArray;
  _name, _model: String;
  a: Integer;
begin
  if not g_Game_IsServer then Exit;

  if (g_Bot_GetCount() >= gMaxBots) then Exit;

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
  with g_Player_Get(g_Player_Create(_model, BotList[num].color, Team, True)) as TBot do
  begin
    Name := _name;

    FDifficult.DiagFire := BotList[num].diag_fire;
    FDifficult.InvisFire := BotList[num].invis_fire;
    FDifficult.DiagPrecision := BotList[num].diag_precision;
    FDifficult.FlyPrecision := BotList[num].fly_precision;
    FDifficult.Cover := BotList[num].cover;
    FDifficult.CloseJump := BotList[num].close_jump;

    FHandicap := Handicap;

    for a := WP_FIRST to WP_LAST do
    begin
      FDifficult.WeaponPrior[a] := BotList[num].w_prior1[a];
      FDifficult.CloseWeaponPrior[a] := BotList[num].w_prior2[a];
      //FDifficult.SafeWeaponPrior[a] := BotList[num].w_prior3[a];
    end;

    g_Console_Add(Format(_lc[I_PLAYER_JOIN], [Name]), True);

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
        gPlayers[a].Kill(K_SIMPLEKILL, 0, HIT_DISCON);
        g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [gPlayers[a].Name]), True);
        g_Player_Remove(gPlayers[a].FUID);
      end;

  g_Bot_MixNames();
end;

procedure g_Bot_MixNames();
var
  s: String;
  a, b: Integer;
begin
  if BotNames <> nil then
    for a := 0 to High(BotNames) do
    begin
      b := Random(Length(BotNames));
      s := BotNames[a];
      Botnames[a] := BotNames[b];
      BotNames[b] := s;
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
  sa: SSArray;
  path: AnsiString;
begin
  BotNames := nil;

  path := BOTNAMES_FILENAME;
  if e_FindResource(DataDirs, path) = false then
    Exit;

// Читаем возможные имена ботов из файла:
  AssignFile(F, path);
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
  g_Bot_MixNames();

// Читаем файл с параметрами ботов:
  config := TConfig.CreateFile(path);
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
  SetLength(SavedStates, 0);
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
  SetLength(SavedStates, 0);
end;

procedure g_Player_PreUpdate();
var
  i: Integer;
begin
  if gPlayers = nil then Exit;
  for i := 0 to High(gPlayers) do
    if gPlayers[i] <> nil then
      gPlayers[i].PreUpdate();
end;

procedure g_Player_UpdateAll();
var
  i: Integer;
begin
  if gPlayers = nil then Exit;

  //e_WriteLog('***g_Player_UpdateAll: ENTER', MSG_WARNING);
  for i := 0 to High(gPlayers) do
  begin
    if gPlayers[i] <> nil then
    begin
      if gPlayers[i] is TPlayer then
      begin
        gPlayers[i].Update();
        gPlayers[i].RealizeCurrentWeapon(); // WARNING! DO NOT MOVE THIS INTO `Update()`!
      end
      else
      begin
        // bot updates weapons in `UpdateCombat()`
        TBot(gPlayers[i]).Update();
      end;
    end;
  end;
  //e_WriteLog('***g_Player_UpdateAll: EXIT', MSG_WARNING);
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

function g_Bot_GetCount(): Integer;
var
  a: Integer;
begin
  Result := 0;

  if gPlayers = nil then
    Exit;

  for a := 0 to High(gPlayers) do
    if (gPlayers[a] <> nil) and (gPlayers[a] is TBot) then
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
        Num := a;
        Ping := gPlayers[a].FPing;
        Loss := gPlayers[a].FLoss;
        Name := gPlayers[a].FName;
        Team := gPlayers[a].FTeam;
        Frags := gPlayers[a].FFrags;
        Deaths := gPlayers[a].FDeath;
        Kills := gPlayers[a].FKills;
        Color := gPlayers[a].FModel.Color;
        Lives := gPlayers[a].FLives;
        Spectator := gPlayers[a].FSpectator;
        UID := gPlayers[a].FUID;
      end;
    end;
end;

procedure g_Player_ResetReady();
var
  a: Integer;
begin
  if not g_Game_IsServer then Exit;
  if gPlayers = nil then Exit;

  for a := 0 to High(gPlayers) do
    if gPlayers[a] <> nil then
    begin
      gPlayers[a].FReady := False;
      if g_Game_IsNet then
        MH_SEND_GameEvent(NET_EV_INTER_READY, gPlayers[a].UID, 'N');
    end;
end;

procedure g_Player_RememberAll;
var
  i: Integer;
begin
  for i := Low(gPlayers) to High(gPlayers) do
    if (gPlayers[i] <> nil) and gPlayers[i].alive then
      gPlayers[i].RememberState;
end;

procedure g_Player_ResetAll(Force, Silent: Boolean);
var
  i: Integer;
begin
  gTeamStat[TEAM_RED].Score := 0;
  gTeamStat[TEAM_BLUE].Score := 0;

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
            gPlayers[i].Spectate();
        end
        else
          TBot(gPlayers[i]).Respawn(Silent);
      end;
end;

{ T P l a y e r : }

function TPlayer.isValidViewPort (): Boolean; inline; begin result := (viewPortW > 0) and (viewPortH > 0); end;

procedure TPlayer.BFGHit();
begin
  g_Weapon_BFGHit(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                  FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
  if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                   FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                   0, NET_GFX_BFGHIT);
end;

procedure TPlayer.ChangeModel(ModelName: string);
var
  locModel: TPlayerModel;
begin
  locModel := g_PlayerModel_Get(ModelName);
  if locModel = nil then Exit;

  FModel.Free();
  FModel := locModel;
end;

procedure TPlayer.SetModel(ModelName: string);
var
  m: TPlayerModel;
begin
  m := g_PlayerModel_Get(ModelName);
  if m = nil then
  begin
    g_SimpleError(Format(_lc[I_GAME_ERROR_MODEL_FALLBACK], [ModelName]));
    m := g_PlayerModel_Get('doomer');
    if m = nil then
    begin
      g_FatalError(Format(_lc[I_GAME_ERROR_MODEL], ['doomer']));
      Exit;
    end;
  end;

  if FModel <> nil then
    FModel.Free();

  FModel := m;

  if not (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
    FModel.Color := FColor
  else
    FModel.Color := TEAMCOLOR[FTeam];
  FModel.SetWeapon(FCurrWeap);
  FModel.SetFlag(FFlag);
  SetDirection(FDirection);
end;

procedure TPlayer.SetColor(Color: TRGB);
begin
  FColor := Color;
  if not (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
    if FModel <> nil then FModel.Color := Color;
end;



function TPlayer.GetColor(): TRGB;
begin
  result := FModel.Color;
end;

procedure TPlayer.SetWeaponPrefs(Prefs: Array of Byte);
var
  i: Integer;
begin
  for i := WP_FIRST to WP_LAST + 1 do
    begin
      if (Prefs[i] > WP_LAST + 1) then
        FWeapPreferences[i] := 0
      else
        FWeapPreferences[i] := Prefs[i];
    end;
end;

procedure TPlayer.SetWeaponPref(Weapon, Pref: Byte);
begin
  if (Weapon > WP_LAST + 1) then
    exit
  else if (Pref <= WP_LAST + 1) and (Weapon <= WP_LAST + 1) then
    FWeapPreferences[Weapon] := Pref
  else if (Weapon <= WP_LAST + 1) and (Pref > WP_LAST + 1) then
    FWeapPreferences[Weapon] := 0;
end;

function TPlayer.GetWeaponPref(Weapon: Byte) : Byte;
begin
  if (Weapon > WP_LAST + 1) then
    result := 0
  else if (FWeapPreferences[Weapon] > WP_LAST + 1) then
    result := 0
  else
    result := FWeapPreferences[Weapon];
end;

function TPlayer.GetMorePrefered() : Byte;
var
  testedWeap, i: Byte;
begin
  testedWeap := FCurrWeap;
  for i := WP_FIRST to WP_LAST do
    if FWeapon[i] and maySwitch(i) and (FWeapPreferences[i] > FWeapPreferences[testedWeap]) then
      testedWeap := i;
  if (R_BERSERK in FRulez) and (FWeapPreferences[WP_LAST + 1] > FWeapPreferences[testedWeap]) then
    testedWeap := WEAPON_KASTET;
  result := testedWeap;
end;

function TPlayer.maySwitch(Weapon: Byte) : Boolean;
begin
  result := true;
  if (Weapon = WEAPON_KASTET) and (FSkipFist <> 0) then
  begin
    if (FSkipFist = 1) and (not (R_BERSERK in FRulez)) then
      result := false;
  end
  else if (FSwitchToEmpty = 0) and (not hasAmmoForShooting(Weapon)) then
    result := false;
end;

procedure TPlayer.SwitchTeam;
begin
  if g_Game_IsClient then
    Exit;
  if not (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then Exit;

  if gGameOn and FAlive then
    Kill(K_SIMPLEKILL, FUID, HIT_SELF);

  if FTeam = TEAM_RED then
  begin
    ChangeTeam(TEAM_BLUE);
    g_Console_Add(Format(_lc[I_PLAYER_CHTEAM_BLUE], [FName]), True);
    if g_Game_IsNet then
      MH_SEND_GameEvent(NET_EV_CHANGE_TEAM, TEAM_BLUE, FName);
  end
  else
  begin
    ChangeTeam(TEAM_RED);
    g_Console_Add(Format(_lc[I_PLAYER_CHTEAM_RED], [FName]), True);
    if g_Game_IsNet then
      MH_SEND_GameEvent(NET_EV_CHANGE_TEAM, TEAM_RED, FName);
  end;
  FPreferredTeam := FTeam;
end;

procedure TPlayer.ChangeTeam(Team: Byte);
var
  OldTeam: Byte;
begin
  OldTeam := FTeam;
  FTeam := Team;
  case Team of
    TEAM_RED, TEAM_BLUE:
      FModel.Color := TEAMCOLOR[Team];
    else
      FModel.Color := FColor;
  end;
  if (FTeam <> OldTeam) and g_Game_IsNet and g_Game_IsServer then
    MH_SEND_PlayerStats(FUID);
end;

{
procedure TPlayer.CollideItem();
var
  i: Integer;
  r: Boolean;
begin
 if gItems = nil then Exit;
 if not FAlive then Exit;

 for i := 0 to High(gItems) do
  with gItems[i] do
  begin
   if (ItemType <> ITEM_NONE) and alive then
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
  viewPortX := 0;
  viewPortY := 0;
  viewPortW := 0;
  viewPortH := 0;
  mEDamageType := HIT_SOME;

  FIamBot := False;
  FDummy := False;
  FSpawned := False;

  FSawSound := TPlayableSound.Create();
  FSawSoundIdle := TPlayableSound.Create();
  FSawSoundHit := TPlayableSound.Create();
  FSawSoundSelect := TPlayableSound.Create();
  FFlameSoundOn := TPlayableSound.Create();
  FFlameSoundOff := TPlayableSound.Create();
  FFlameSoundWork := TPlayableSound.Create();
  FJetSoundFly := TPlayableSound.Create();
  FJetSoundOn := TPlayableSound.Create();
  FJetSoundOff := TPlayableSound.Create();

  FSawSound.SetByName('SOUND_WEAPON_FIRESAW');
  FSawSoundIdle.SetByName('SOUND_WEAPON_IDLESAW');
  FSawSoundHit.SetByName('SOUND_WEAPON_HITSAW');
  FSawSoundSelect.SetByName('SOUND_WEAPON_SELECTSAW');
  FFlameSoundOn.SetByName('SOUND_WEAPON_FLAMEON');
  FFlameSoundOff.SetByName('SOUND_WEAPON_FLAMEOFF');
  FFlameSoundWork.SetByName('SOUND_WEAPON_FLAMEWORK');
  FJetSoundFly.SetByName('SOUND_PLAYER_JETFLY');
  FJetSoundOn.SetByName('SOUND_PLAYER_JETON');
  FJetSoundOff.SetByName('SOUND_PLAYER_JETOFF');

  FSpectatePlayer := -1;
  FClientID := -1;
  FPing := 0;
  FLoss := 0;
  FSavedStateNum := -1;
  {$IFDEF ENABLE_SHELLS}
    FShellTimer := -1;
  {$ENDIF}
  FFireTime := 0;
  FFirePainTime := 0;
  FFireAttacker := 0;
  FHandicap := 100;

  {$IFDEF ENABLE_CORPSES}
    FCorpse := -1;
  {$ENDIF}

  FActualModelName := 'doomer';

  g_Obj_Init(@FObj);
  FObj.Rect := PLAYER_RECT;

  FBFGFireCounter := -1;
  FJustTeleported := False;
  FNetTime := 0;

  FWaitForFirstSpawn := false;
  FPunchAnim := TAnimationState.Create(False, 1, 4);
  FPunchAnim.Disable;

  resetWeaponQueue();
end;

procedure TPlayer.positionChanged (); inline;
begin
end;

procedure TPlayer.doDamage (v: Integer);
begin
  if (v <= 0) then exit;
  if (v > 32767) then v := 32767;
  Damage(v, 0, 0, 0, mEDamageType);
end;

procedure TPlayer.Damage(value: Word; SpawnerUID: Word; vx, vy: Integer; t: Byte);
var
  c: Word;
begin
  if (not g_Game_IsClient) and (not FAlive) then
    Exit;

  FLastHit := t;

// Неуязвимость не спасает от ловушек:
  if ((t = HIT_TRAP) or (t = HIT_SELF)) and (not FGodMode) then
  begin
    if not g_Game_IsClient then
    begin
      FArmor := 0;
      if t = HIT_TRAP then
      begin
        // Ловушка убивает сразу:
        FHealth := -100;
        Kill(K_EXTRAHARDKILL, SpawnerUID, t);
      end;
      if t = HIT_SELF then
      begin
        // Самоубийство:
        FHealth := 0;
        Kill(K_SIMPLEKILL, SpawnerUID, t);
      end;
    end;
    // Обнулить действия примочек, чтобы фон пропал
    FMegaRulez[MR_SUIT] := 0;
    FMegaRulez[MR_INVUL] := 0;
    FMegaRulez[MR_INVIS] := 0;
    FSpawnInvul := 0;
    FBerserk := 0;
  end;

// Но от остального спасает:
  if FMegaRulez[MR_INVUL] >= gTime then
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
          HIT_TRAP, HIT_ACID, HIT_FLAME, HIT_SELF: MakeBloodSimple(c);
          HIT_BFG, HIT_ROCKET, HIT_SOME: MakeBloodVector(c, vx, vy);
        end;

      {$IFDEF ENABLE_GFX}
        if t = HIT_WATER then
        begin
          g_GFX_Bubbles(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                        FObj.Y+PLAYER_RECT.Y-4, value div 2, 8, 4);
          if Random(2) = 0
            then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
            else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
        end;
      {$ENDIF}
    end;

  // Буфер урона:
    if FAlive then
      Inc(FDamageBuffer, value);

  // Вспышка боли:
    if gFlash <> 0 then
      FPain := FPain + value;
  end;

  if g_Game_IsServer and g_Game_IsNet then
  begin
    MH_SEND_PlayerDamage(FUID, t, SpawnerUID, value, vx, vy);
    MH_SEND_PlayerStats(FUID);
    MH_SEND_PlayerPos(False, FUID);
  end;
end;

function TPlayer.Heal(value: Word; Soft: Boolean): Boolean;
begin
  Result := False;
  if g_Game_IsClient then
    Exit;
  if not FAlive then
    Exit;

  if Soft and (FHealth < PLAYER_HP_SOFT) then
  begin
    IncMax(FHealth, value, PLAYER_HP_SOFT);
    Result := True;
  end;
  if (not Soft) and (FHealth < PLAYER_HP_LIMIT) then
  begin
    IncMax(FHealth, value, PLAYER_HP_LIMIT);
    Result := True;
  end;

  if Result and g_Game_IsServer and g_Game_IsNet then
    MH_SEND_PlayerStats(FUID);
end;

destructor TPlayer.Destroy();
begin
  if (gPlayer1 <> nil) and (gPlayer1.FUID = FUID) then
    gPlayer1 := nil;
  if (gPlayer2 <> nil) and (gPlayer2.FUID = FUID) then
    gPlayer2 := nil;

  FSawSound.Free();
  FSawSoundIdle.Free();
  FSawSoundHit.Free();
  FSawSoundSelect.Free();
  FFlameSoundOn.Free();
  FFlameSoundOff.Free();
  FFlameSoundWork.Free();
  FJetSoundFly.Free();
  FJetSoundOn.Free();
  FJetSoundOff.Free();
  FModel.Free();
  FPunchAnim.Free();

  inherited;
end;

procedure TPlayer.DoPunch();
begin
  FPunchAnim.Reset;
  FPunchAnim.Enable;
end;

procedure TPlayer.Fire();
var
  f, DidFire: Boolean;
  wx, wy, xd, yd: Integer;
  locobj: TObj;
begin
  if g_Game_IsClient then Exit;
// FBFGFireCounter - время перед выстрелом (для BFG)
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
  xd := wx+IfThen(FDirection = TDirection.D_LEFT, -30, 30);
  yd := wy+firediry();

  case FCurrWeap of
    WEAPON_KASTET:
    begin
      DoPunch();
      if R_BERSERK in FRulez then
      begin
        //g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 75, FUID);
        locobj.X := FObj.X+FObj.Rect.X;
        locobj.Y := FObj.Y+FObj.Rect.Y;
        locobj.rect.X := 0;
        locobj.rect.Y := 0;
        locobj.rect.Width := 39;
        locobj.rect.Height := 52;
        locobj.Vel.X := (xd-wx) div 2;
        locobj.Vel.Y := (yd-wy) div 2;
        locobj.Accel.X := xd-wx;
        locobj.Accel.y := yd-wy;

        if g_Weapon_Hit(@locobj, 50, FUID, HIT_SOME) <> 0 then
          g_Sound_PlayExAt('SOUND_WEAPON_HITBERSERK', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_WEAPON_MISSBERSERK', FObj.X, FObj.Y);

        if (gFlash = 1) and (FPain < 50) then FPain := min(FPain + 25, 50);
      end
      else
      begin
        g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 3, FUID);
      end;

      DidFire := True;
      FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
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

      FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
      DidFire := True;
      f := True;
    end;

    WEAPON_PISTOL:
      if FAmmo[A_BULLETS] > 0 then
      begin
        g_Weapon_pistol(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_BULLETS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        {$IFDEF ENABLE_SHELLS}
          g_Shells_Create(GameX + PLAYER_RECT_CX, GameY + PLAYER_RECT_CX, GameVelX, GameVelY - 2, SHELL_BULLET);
        {$ENDIF}
      end;

    WEAPON_SHOTGUN1:
      if FAmmo[A_SHELLS] > 0 then
      begin
        g_Weapon_shotgun(wx, wy, xd, yd, FUID);
        if not gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', wx, wy);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_SHELLS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        {$IFDEF ENABLE_SHELLS}
          FShellTimer := 10;
          FShellType := SHELL_SHELL;
        {$ENDIF}
      end;

    WEAPON_SHOTGUN2:
      if FAmmo[A_SHELLS] >= 2 then
      begin
        g_Weapon_dshotgun(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_SHELLS], 2);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        {$IFDEF ENABLE_SHELLS}
          FShellTimer := 13;
          FShellType := SHELL_DBLSHELL;
        {$ENDIF}
      end;

    WEAPON_CHAINGUN:
      if FAmmo[A_BULLETS] > 0 then
      begin
        g_Weapon_mgun(wx, wy, xd, yd, FUID);
        if not gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', wx, wy);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_BULLETS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        {$IFDEF ENABLE_SHELLS}
          g_Shells_Create(GameX + PLAYER_RECT_CX, GameY + PLAYER_RECT_CX, GameVelX, GameVelY - 2, SHELL_BULLET);
        {$ENDIF}
      end;

    WEAPON_ROCKETLAUNCHER:
      if FAmmo[A_ROCKETS] > 0 then
      begin
        g_Weapon_rocket(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_ROCKETS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
      end;

    WEAPON_PLASMA:
      if FAmmo[A_CELLS] > 0 then
      begin
        g_Weapon_plasma(wx, wy, xd, yd, FUID);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_CELLS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
      end;

    WEAPON_BFG:
      if (FAmmo[A_CELLS] >= 40) and (FBFGFireCounter = -1) then
      begin
        FBFGFireCounter := 17;
        if not FNoReload then
          g_Sound_PlayExAt('SOUND_WEAPON_STARTFIREBFG', FObj.X, FObj.Y);
        Dec(FAmmo[A_CELLS], 40);
        DidFire := True;
      end;

    WEAPON_SUPERPULEMET:
      if FAmmo[A_SHELLS] > 0 then
      begin
        g_Weapon_shotgun(wx, wy, xd, yd, FUID);
        if not gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', wx, wy);
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_SHELLS]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
        {$IFDEF ENABLE_SHELLS}
          g_Shells_Create(GameX + PLAYER_RECT_CX, GameY + PLAYER_RECT_CX, GameVelX, GameVelY - 2, SHELL_SHELL);
        {$ENDIF}
      end;

    WEAPON_FLAMETHROWER:
      if FAmmo[A_FUEL] > 0 then
      begin
        g_Weapon_flame(wx, wy, xd, yd, FUID);
        FlamerOn;
        FReloading[FCurrWeap] := WEAPON_RELOAD[FCurrWeap];
        Dec(FAmmo[A_FUEL]);
        FFireAngle := FAngle;
        f := True;
        DidFire := True;
      end
      else
      begin
        FlamerOff;
        if g_Game_IsNet and g_Game_IsServer then MH_SEND_PlayerStats(FUID);
      end;
  end;

  if g_Game_IsNet then
  begin
    if DidFire then
    begin
      if FCurrWeap <> WEAPON_BFG then
        MH_SEND_PlayerFire(FUID, FCurrWeap, wx, wy, xd, yd, LastShotID)
      else
        if not FNoReload then
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
    WEAPON_FLAMETHROWER: Result := FAmmo[A_FUEL];
    else Result := 0;
  end;
end;

function TPlayer.HeadInLiquid(XInc, YInc: Integer): Boolean;
begin
  Result := g_Map_CollidePanel(FObj.X+PLAYER_HEADRECT.X+XInc, FObj.Y+PLAYER_HEADRECT.Y+YInc,
                               PLAYER_HEADRECT.Width, PLAYER_HEADRECT.Height,
                               PANEL_WATER or PANEL_ACID1 or PANEL_ACID2, True);
end;

procedure TPlayer.FlamerOn;
begin
  FFlameSoundOff.Stop();
  FFlameSoundOff.SetPosition(0);
  if FFlaming then
  begin
    if (not FFlameSoundOn.IsPlaying()) and (not FFlameSoundWork.IsPlaying()) then
      FFlameSoundWork.PlayAt(FObj.X, FObj.Y);
  end
  else
  begin
    FFlameSoundOn.PlayAt(FObj.X, FObj.Y);
    FFlaming := True;
  end;
end;

procedure TPlayer.FlamerOff;
begin
  if FFlaming then
  begin
    FFlameSoundOn.Stop();
    FFlameSoundOn.SetPosition(0);
    FFlameSoundWork.Stop();
    FFlameSoundWork.SetPosition(0);
    FFlameSoundOff.PlayAt(FObj.X, FObj.Y);
    FFlaming := False;
  end;
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

procedure TPlayer.CatchFire(Attacker: Word; Timeout: Integer = PLAYER_BURN_TIME);
begin
  if Timeout <= 0 then
    exit;
  if (FMegaRulez[MR_SUIT] > gTime) or (FMegaRulez[MR_INVUL] > gTime) then
    exit; // Не загораемся когда есть защита
  if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
    exit; // Не подгораем в воде на всякий случай
  if FFireTime <= 0 then
    g_Sound_PlayExAt('SOUND_IGNITE', FObj.X, FObj.Y);
  FFireTime := Timeout;
  FFireAttacker := Attacker;
  if g_Game_IsNet and g_Game_IsServer then
    MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.Jump();
begin
  if gFly or FJetpack then
  begin
    // Полет (чит-код или джетпак):
    if FObj.Vel.Y > -VEL_FLY then
      FObj.Vel.Y := FObj.Vel.Y - 3;
    if FJetpack then
    begin
      if FJetFuel > 0 then
        Dec(FJetFuel);
      if (FJetFuel < 1) and g_Game_IsServer then
      begin
        FJetpack := False;
        JetpackOff;
        if g_Game_IsNet then
          MH_SEND_PlayerStats(FUID);
      end;
    end;
    Exit;
  end;

// Не включать джетпак в режиме прохождения сквозь стены
  if FGhost then
    FCanJetpack := False;

// Прыгаем или всплываем:
  if (CollideLevel(0, 1) or
      g_Map_CollidePanel(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y+36, PLAYER_RECT.Width,
                       PLAYER_RECT.Height-33, PANEL_STEP, False)
     ) and (FObj.Accel.Y = 0) then // Не прыгать, если есть вертикальное ускорение
  begin
    FObj.Vel.Y := -VEL_JUMP;
    FCanJetpack := False;
  end
  else
  begin
    if BodyInLiquid(0, 0) then
      FObj.Vel.Y := -VEL_SW
    else if (FJetFuel > 0) and FCanJetpack and
            g_Game_IsServer and (not g_Obj_CollideLiquid(@FObj, 0, 0)) then
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
  DoFrags: Boolean;
  OldLR: Byte;
  KP: TPlayer;
  it: PItem;

  procedure PushItem(t: Byte);
  var
    id: DWORD;
  begin
    id := g_Items_Create(FObj.X, FObj.Y, t, True, False);
    it := g_Items_ByIdx(id);
    if KillType = K_EXTRAHARDKILL then // -7..+7; -8..0
    begin
      g_Obj_Push(@it.Obj, (FObj.Vel.X div 2)-7+Random(15),
                          (FObj.Vel.Y div 2)-Random(9));
      it.positionChanged(); // this updates spatial accelerators
    end
    else
    begin
      if KillType = K_HARDKILL then // -5..+5; -5..0
      begin
        g_Obj_Push(@it.Obj, (FObj.Vel.X div 2)-5+Random(11),
                            (FObj.Vel.Y div 2)-Random(6));
      end
      else // -3..+3; -3..0
      begin
        g_Obj_Push(@it.Obj, (FObj.Vel.X div 2)-3+Random(7),
                            (FObj.Vel.Y div 2)-Random(4));
      end;
      it.positionChanged(); // this updates spatial accelerators
    end;

    if g_Game_IsNet and g_Game_IsServer then
      MH_SEND_ItemSpawn(True, id);
  end;

begin
  DoFrags := (gGameSettings.MaxLives = 0) or (gGameSettings.GameMode = GM_COOP);
  Srv := g_Game_IsServer;
  Netsrv := g_Game_IsServer and g_Game_IsNet;
  if Srv then FDeath := FDeath + 1;
  if FAlive then
  begin
    if FGhost then
      FGhost := False;
    if not FPhysics then
      FPhysics := True;
    FAlive := False;
  end;

  {$IFDEF ENABLE_SHELLS}
    FShellTimer := -1;
  {$ENDIF}

  if (gGameSettings.MaxLives > 0) and Srv and (gLMSRespawn = LMS_RESPAWN_NONE) then
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
        FTime[T_RESPAWN] := gTime + TIME_RESPAWN3;
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

  if SpawnerUID = FUID then
    begin // Самоубился
      if Srv then
      begin
        if gGameSettings.GameMode = GM_TDM then
          Dec(gTeamStat[FTeam].Score);
        if DoFrags or (gGameSettings.GameMode = GM_TDM) then
        begin
          Dec(FFrags);
          FLastFrag := 0;
        end;
      end;
      g_Console_Add(Format(_lc[I_PLAYER_KILL_SELF], [FName]), True);
    end
  else
    if g_GetUIDType(SpawnerUID) = UID_PLAYER then
      begin // Убит другим игроком
        KP := g_Player_Get(SpawnerUID);
        if (KP <> nil) and Srv then
        begin
          if (DoFrags or (gGameSettings.GameMode = GM_TDM)) then
            if SameTeam(FUID, SpawnerUID) then
            begin
              Dec(KP.FFrags);
              KP.FLastFrag := 0;
            end else
            begin
              Inc(KP.FFrags);
              KP.FragCombo();
            end;

          if (gGameSettings.GameMode = GM_TDM) and DoFrags then
            Inc(gTeamStat[KP.Team].Score,
              IfThen(SameTeam(FUID, SpawnerUID), -1, 1));

          if netsrv then MH_SEND_PlayerStats(SpawnerUID);
        end;

        plr := g_Player_Get(SpawnerUID);
        if plr = nil then
          s := '?'
        else
          s := plr.FName;

        case KillType of
          K_HARDKILL:
            g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_2],
                                 [FName, s]),
                          gShowKillMsg);
          K_EXTRAHARDKILL:
            g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_1],
                                 [FName, s]),
                          gShowKillMsg);
          else
            g_Console_Add(Format(_lc[I_PLAYER_KILL],
                                 [FName, s]),
                          gShowKillMsg);
        end;
      end
    else if g_GetUIDType(SpawnerUID) = UID_MONSTER then
      begin // Убит монстром
        mon := g_Monsters_ByUID(SpawnerUID);
        if mon = nil then
          s := '?'
        else
          s := g_Mons_GetKilledByTypeId(mon.MonsterType);

        case KillType of
          K_HARDKILL:
            g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_2],
                                 [FName, s]),
                          gShowKillMsg);
          K_EXTRAHARDKILL:
            g_Console_Add(Format(_lc[I_PLAYER_KILL_EXTRAHARD_1],
                                 [FName, s]),
                          gShowKillMsg);
          else
            g_Console_Add(Format(_lc[I_PLAYER_KILL],
                                 [FName, s]),
                          gShowKillMsg);
        end;
      end
    else // Особые типы смерти
      case t of
        HIT_DISCON: ;
        HIT_SELF: g_Console_Add(Format(_lc[I_PLAYER_KILL_SELF], [FName]), True);
        HIT_FALL: g_Console_Add(Format(_lc[I_PLAYER_KILL_FALL], [FName]), True);
        HIT_WATER: g_Console_Add(Format(_lc[I_PLAYER_KILL_WATER], [FName]), True);
        HIT_ACID: g_Console_Add(Format(_lc[I_PLAYER_KILL_ACID], [FName]), True);
        HIT_TRAP: g_Console_Add(Format(_lc[I_PLAYER_KILL_TRAP], [FName]), True);
        else g_Console_Add(Format(_lc[I_PLAYER_DIED], [FName]), True);
      end;

  if Srv then
  begin
// Выброс оружия:
    for a := WP_FIRST to WP_LAST do
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
          WEAPON_FLAMETHROWER: i := ITEM_WEAPON_FLAMETHROWER;
          else i := 0;
        end;

        if i <> 0 then
          PushItem(i);
      end;

// Выброс рюкзака:
    if R_ITEM_BACKPACK in FRulez then
      PushItem(ITEM_AMMO_BACKPACK);

// Выброс ракетного ранца:
    if FJetFuel > 0 then
      PushItem(ITEM_JETPACK);

// Выброс ключей:
    if (not (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF])) or
       (not LongBool(gGameSettings.Options and GAME_OPTION_DMKEYS)) then
    begin
      if R_KEY_RED in FRulez then
        PushItem(ITEM_KEY_RED);

      if R_KEY_GREEN in FRulez then
        PushItem(ITEM_KEY_GREEN);

      if R_KEY_BLUE in FRulez then
        PushItem(ITEM_KEY_BLUE);
    end;

// Выброс флага:
    DropFlag(KillType = K_FALLKILL);
  end;

  {$IFDEF ENABLE_CORPSES}
    FCorpse := g_Corpses_Create(Self);
  {$ENDIF}

  if Srv and (gGameSettings.MaxLives > 0) and FNoRespawn and
     (gLMSRespawn = LMS_RESPAWN_NONE) then
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
        if Netsrv then
          MH_SEND_GameEvent(NET_EV_LMS_LOSE);
        gLMSRespawn := LMS_RESPAWN_FINAL;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (a = 1) then
      begin
        if (gPlayers[k] <> nil) and not (gPlayers[k] is TBot) then
          if (gPlayers[k] = gPlayer1) or
             (gPlayers[k] = gPlayer2) then
            g_Console_Add('*** ' + _lc[I_MESSAGE_LMS_SURVIVOR] + ' ***', True)
          else if Netsrv and (gPlayers[k].FClientID >= 0) then
            MH_SEND_GameEvent(NET_EV_LMS_SURVIVOR, 0, 'N', gPlayers[k].FClientID);
      end;
    end
    else if (gGameSettings.GameMode = GM_TDM) then
    begin
      if (ab = 0) and (ar <> 0) then
      begin
        // blu team ded
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 144);
        if Netsrv then
          MH_SEND_GameEvent(NET_EV_TLMS_WIN, TEAM_RED);
        Inc(gTeamStat[TEAM_RED].Score);
        gLMSRespawn := LMS_RESPAWN_FINAL;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (ar = 0) and (ab <> 0) then
      begin
        // red team ded
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 144);
        if Netsrv then
          MH_SEND_GameEvent(NET_EV_TLMS_WIN, TEAM_BLUE);
        Inc(gTeamStat[TEAM_BLUE].Score);
        gLMSRespawn := LMS_RESPAWN_FINAL;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (ar = 0) and (ab = 0) then
      begin
        // everyone ded
        g_Game_Message(_lc[I_GAME_WIN_DRAW], 144);
        if Netsrv then
          MH_SEND_GameEvent(NET_EV_LMS_DRAW, 0, FName);
        gLMSRespawn := LMS_RESPAWN_FINAL;
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
            if Netsrv then
              MH_SEND_GameEvent(NET_EV_LMS_WIN, 0, FName);
            Inc(FFrags);
          end;
        gLMSRespawn := LMS_RESPAWN_FINAL;
        gLMSRespawnTime := gTime + 5000;
      end
      else if (a = 0) then
      begin
        // everyone is dead, restart the map
        g_Game_Message(_lc[I_GAME_WIN_DRAW], 144);
        if Netsrv then
          MH_SEND_GameEvent(NET_EV_LMS_DRAW, 0, FName);
        gLMSRespawn := LMS_RESPAWN_FINAL;
        gLMSRespawnTime := gTime + 5000;
      end;
    end;
    if srv and (OldLR = LMS_RESPAWN_NONE) and (gLMSRespawn > LMS_RESPAWN_NONE) then
    begin
      if NetMode = NET_SERVER then
        MH_SEND_GameEvent(NET_EV_LMS_WARMUP, gLMSRespawnTime - gTime)
      else
        g_Console_Add(Format(_lc[I_MSG_WARMUP_START], [(gLMSRespawnTime - gTime) div 1000]), True);
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

function TPlayer.BodyInAcid(XInc, YInc: Integer): Boolean;
begin
  Result := g_Map_CollidePanel(FObj.X+PLAYER_RECT.X+XInc, FObj.Y+PLAYER_RECT.Y+YInc, PLAYER_RECT.Width,
                               PLAYER_RECT.Height-20, PANEL_ACID1 or PANEL_ACID2, False);
end;

procedure TPlayer.MakeBloodSimple(Count: Word);
  {$IFDEF ENABLE_GFX}
    var Blood: TModelBlood;
  {$ENDIF}
begin
  {$IFDEF ENABLE_GFX}
    Blood := SELF.FModel.GetBlood();
    g_GFX_Blood(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)+8,
                FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2),
                Count div 2, 3, -1, 16, (PLAYER_RECT.Height*2 div 3),
                Blood.R, Blood.G, Blood.B, Blood.Kind);
    g_GFX_Blood(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-8,
                FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2),
                Count div 2, -3, -1, 16, (PLAYER_RECT.Height*2) div 3,
                Blood.R, Blood.G, Blood.B, Blood.Kind);
  {$ENDIF}
end;

procedure TPlayer.MakeBloodVector(Count: Word; VelX, VelY: Integer);
  {$IFDEF ENABLE_GFX}
    var Blood: TModelBlood;
  {$ENDIF}
begin
  {$IFDEF ENABLE_GFX}
    Blood := SELF.FModel.GetBlood();
    g_GFX_Blood(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2),
                FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2),
                Count, VelX, VelY, 16, (PLAYER_RECT.Height*2) div 3,
                Blood.R, Blood.G, Blood.B, Blood.Kind);
  {$ENDIF}
end;

procedure TPlayer.ProcessWeaponAction(Action: Byte);
begin
  if g_Game_IsClient then Exit;
  case Action of
    WP_PREV: PrevWeapon();
    WP_NEXT: NextWeapon();
  end;
end;

procedure TPlayer.QueueWeaponSwitch(Weapon: Byte);
begin
  if g_Game_IsClient then Exit;
  if Weapon > High(FWeapon) then Exit;
  FNextWeap := FNextWeap or (1 shl Weapon);
end;

procedure TPlayer.resetWeaponQueue ();
begin
  FNextWeap := 0;
  FNextWeapDelay := 0;
end;

function TPlayer.hasAmmoForWeapon (weapon: Byte): Boolean;
begin
  result := false;
  case weapon of
    WEAPON_KASTET, WEAPON_SAW: result := true;
    WEAPON_SHOTGUN1, WEAPON_SHOTGUN2, WEAPON_SUPERPULEMET: result := (FAmmo[A_SHELLS] > 0);
    WEAPON_PISTOL, WEAPON_CHAINGUN: result := (FAmmo[A_BULLETS] > 0);
    WEAPON_ROCKETLAUNCHER: result := (FAmmo[A_ROCKETS] > 0);
    WEAPON_PLASMA, WEAPON_BFG: result := (FAmmo[A_CELLS] > 0);
    WEAPON_FLAMETHROWER: result := (FAmmo[A_FUEL] > 0);
    else result := (weapon < length(FWeapon));
  end;
end;

function TPlayer.hasAmmoForShooting (weapon: Byte): Boolean;
begin
  result := false;
  case weapon of
    WEAPON_KASTET, WEAPON_SAW: result := true;
    WEAPON_SHOTGUN1, WEAPON_SUPERPULEMET: result := (FAmmo[A_SHELLS] > 0);
    WEAPON_SHOTGUN2: result := (FAmmo[A_SHELLS] > 1);
    WEAPON_PISTOL, WEAPON_CHAINGUN: result := (FAmmo[A_BULLETS] > 0);
    WEAPON_ROCKETLAUNCHER: result := (FAmmo[A_ROCKETS] > 0);
    WEAPON_PLASMA: result := (FAmmo[A_CELLS] > 0);
    WEAPON_BFG: result := (FAmmo[A_CELLS] >= 40);
    WEAPON_FLAMETHROWER: result := (FAmmo[A_FUEL] > 0);
    else result := (weapon < length(FWeapon));
  end;
end;

function TPlayer.shouldSwitch (weapon: Byte; hadWeapon: Boolean): Boolean;
begin
  result := false;
  if (weapon > WP_LAST + 1) then
    begin
      result := false;
      exit;
    end;
  if (FWeapSwitchMode = 1) and not hadWeapon then
    result := true
  else if (FWeapSwitchMode = 2) then
    result := (FWeapPreferences[weapon] > FWeapPreferences[FCurrWeap]);
end;

// return 255 for "no switch"
function TPlayer.getNextWeaponIndex (): Byte;
var
  i: Word;
  wantThisWeapon: array[0..64] of Boolean;
  wwc: Integer = 0; //HACK!
  dir, cwi: Integer;
begin
  result := 255; // default result: "no switch"
  //e_LogWriteFln('FSWITCHTOEMPTY: %s', [FSwitchToEmpty], TMsgType.Notify);
  // had weapon cycling on previous frame? remove that flag
  if (FNextWeap and $2000) <> 0 then
  begin
    FNextWeap := FNextWeap and $1FFF;
    FNextWeapDelay := 0;
  end;
  // cycling has priority
  if (FNextWeap and $C000) <> 0 then
  begin
    if (FNextWeap and $8000) <> 0 then
      dir := 1
    else
      dir := -1;
    FNextWeap := FNextWeap or $2000; // we need this
    if FNextWeapDelay > 0 then
      exit; // cooldown time
    cwi := FCurrWeap;
    for i := 0 to High(FWeapon) do
    begin
      cwi := (cwi+length(FWeapon)+dir) mod length(FWeapon);
      if FWeapon[cwi] and maySwitch(cwi)  then
      begin
        //e_LogWriteFln(' SWITCH: cur=%d; new=%d %s %s', [FCurrWeap, cwi, FSwitchToEmpty, hasAmmoForWeapon(cwi)], TMsgType.Notify);
        result := Byte(cwi);
        FNextWeapDelay := WEAPON_DELAY;
        exit;
      end;
    end;
    resetWeaponQueue();
    exit;
  end;
  // no cycling
  for i := 0 to High(wantThisWeapon) do
    wantThisWeapon[i] := false;
  for i := 0 to High(FWeapon) do
    if (FNextWeap and (1 shl i)) <> 0 then
    begin
      wantThisWeapon[i] := true;
      Inc(wwc);
    end;

  // exclude currently selected weapon from the set
  wantThisWeapon[FCurrWeap] := false;
  // slow down alterations a little
  if wwc > 1 then
  begin
    //e_WriteLog(Format(' FNextWeap=%x; delay=%d', [FNextWeap, FNextWeapDelay]), MSG_WARNING);
    // more than one weapon requested, assume "alteration" and check alteration delay
    if FNextWeapDelay > 0 then
    begin
      FNextWeap := 0;
      exit;
    end; // yeah
  end;
  // do not reset weapon queue, it will be done in `RealizeCurrentWeapon()`
  // but clear all counters if no weapon should be switched
  if wwc < 1 then
  begin
    resetWeaponQueue();
    exit;
  end;
  //e_WriteLog(Format('wwc=%d', [wwc]), MSG_WARNING);
  // try weapons in descending order
  for i := High(FWeapon) downto 0 do
  begin
    if wantThisWeapon[i] and FWeapon[i] and ((wwc = 1) or hasAmmoForWeapon(i)) then
    begin
      // i found her!
      result := Byte(i);
      resetWeaponQueue();
      FNextWeapDelay := WEAPON_DELAY * 2; // anyway, 'cause why not
      //e_LogWriteFln('FOUND %s %s %s', [result, FSwitchToEmpty, hasAmmoForWeapon(i)], TMsgType.Notify);
      exit;
    end;
  end;
  // no suitable weapon found, so reset the queue, to avoid accidental "queuing" of weapon w/o ammo
  resetWeaponQueue();
end;

procedure TPlayer.RealizeCurrentWeapon();
  function switchAllowed (): Boolean;
  var
    i: Byte;
  begin
    result := false;
    if FBFGFireCounter <> -1 then
      exit;
    if FTime[T_SWITCH] > gTime then
      exit;
    for i := WP_FIRST to WP_LAST do
      if FReloading[i] > 0 then
        exit;
    result := true;
  end;

var
  nw: Byte;
begin
  //e_WriteLog(Format('***RealizeCurrentWeapon: FNextWeap=%x; FNextWeapDelay=%d', [FNextWeap, FNextWeapDelay]), MSG_WARNING);
  //FNextWeap := FNextWeap and $1FFF;
  if FNextWeapDelay > 0 then Dec(FNextWeapDelay); // "alteration delay"

  if not switchAllowed then
  begin
    //HACK for weapon cycling
    if (FNextWeap and $E000) <> 0 then FNextWeap := 0;
    exit;
  end;

  nw := getNextWeaponIndex();
  //
  if nw = 255 then exit; // don't reset anything here
  if nw > High(FWeapon) then
  begin
    // don't forget to reset queue here!
    //e_WriteLog(' RealizeCurrentWeapon: WUTAFUUUU', MSG_WARNING);
    resetWeaponQueue();
    exit;
  end;

  if FWeapon[nw] then
  begin
    FCurrWeap := nw;
    FTime[T_SWITCH] := gTime+156;
    if FCurrWeap = WEAPON_SAW then FSawSoundSelect.PlayAt(FObj.X, FObj.Y);
    FModel.SetWeapon(FCurrWeap);
    if g_Game_IsNet then MH_SEND_PlayerStats(FUID);
  end;
end;

procedure TPlayer.NextWeapon();
begin
  if g_Game_IsClient then Exit;
  FNextWeap := $8000;
end;

procedure TPlayer.PrevWeapon();
begin
  if g_Game_IsClient then Exit;
  FNextWeap := $4000;
end;

procedure TPlayer.SetWeapon(W: Byte);
begin
  if FCurrWeap <> W then
    if W = WEAPON_SAW then
      FSawSoundSelect.PlayAt(FObj.X, FObj.Y);

  FCurrWeap := W;
  FModel.SetWeapon(CurrWeap);
  resetWeaponQueue();
end;

function TPlayer.PickItem(ItemType: Byte; arespawn: Boolean; var remove: Boolean): Boolean;
var
  a: Boolean;
  switchWeapon: Byte = 255;
  hadWeapon: Boolean = False;
begin
  Result := False;
  if g_Game_IsClient then Exit;

  // a = true - место спавна предмета:
  a := LongBool(gGameSettings.Options and GAME_OPTION_WEAPONSTAY) and arespawn;
  remove := not a;
  case ItemType of
    ITEM_MEDKIT_SMALL:
      if (FHealth < PLAYER_HP_SOFT) or (FFireTime > 0) then
      begin
        if FHealth < PLAYER_HP_SOFT then IncMax(FHealth, 10, PLAYER_HP_SOFT);
        Result := True;
        remove := True;
        FFireTime := 0;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_MEDKIT_LARGE:
      if (FHealth < PLAYER_HP_SOFT) or (FFireTime > 0) then
      begin
        if FHealth < PLAYER_HP_SOFT then IncMax(FHealth, 25, PLAYER_HP_SOFT);
        Result := True;
        remove := True;
        FFireTime := 0;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_ARMOR_GREEN:
      if FArmor < PLAYER_AP_SOFT then
      begin
        FArmor := PLAYER_AP_SOFT;
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_ARMOR_BLUE:
      if FArmor < PLAYER_AP_LIMIT then
      begin
        FArmor := PLAYER_AP_LIMIT;
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_SPHERE_BLUE:
      if (FHealth < PLAYER_HP_LIMIT) or (FFireTime > 0) then
      begin
        if FHealth < PLAYER_HP_LIMIT then IncMax(FHealth, 100, PLAYER_HP_LIMIT);
        Result := True;
        remove := True;
        FFireTime := 0;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_SPHERE_WHITE:
      if (FHealth < PLAYER_HP_LIMIT) or (FArmor < PLAYER_AP_LIMIT) or (FFireTime > 0) then
      begin
        if FHealth < PLAYER_HP_LIMIT then
          FHealth := PLAYER_HP_LIMIT;
        if FArmor < PLAYER_AP_LIMIT then
          FArmor := PLAYER_AP_LIMIT;
        Result := True;
        remove := True;
        FFireTime := 0;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_WEAPON_SAW:
      if (not FWeapon[WEAPON_SAW]) or ((not arespawn) and (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF])) then
      begin
        hadWeapon := FWeapon[WEAPON_SAW];
        switchWeapon := WEAPON_SAW;
        FWeapon[WEAPON_SAW] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_SHOTGUN1:
      if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SHOTGUN1] then
      begin
        // Нужно, чтобы не взять все пули сразу:
        if a and FWeapon[WEAPON_SHOTGUN1] then Exit;
        hadWeapon := FWeapon[WEAPON_SHOTGUN1];
        switchWeapon := WEAPON_SHOTGUN1;
        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        FWeapon[WEAPON_SHOTGUN1] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_SHOTGUN2:
      if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SHOTGUN2] then
      begin
        if a and FWeapon[WEAPON_SHOTGUN2] then Exit;
        hadWeapon := FWeapon[WEAPON_SHOTGUN2];
        switchWeapon := WEAPON_SHOTGUN2;
        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        FWeapon[WEAPON_SHOTGUN2] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_CHAINGUN:
      if (FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS]) or not FWeapon[WEAPON_CHAINGUN] then
      begin
        if a and FWeapon[WEAPON_CHAINGUN] then Exit;
        hadWeapon := FWeapon[WEAPON_CHAINGUN];
        switchWeapon := WEAPON_CHAINGUN;
        IncMax(FAmmo[A_BULLETS], 50, FMaxAmmo[A_BULLETS]);
        FWeapon[WEAPON_CHAINGUN] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_ROCKETLAUNCHER:
      if (FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS]) or not FWeapon[WEAPON_ROCKETLAUNCHER] then
      begin
        if a and FWeapon[WEAPON_ROCKETLAUNCHER] then Exit;
        switchWeapon := WEAPON_ROCKETLAUNCHER;
        hadWeapon := FWeapon[WEAPON_ROCKETLAUNCHER];
        IncMax(FAmmo[A_ROCKETS], 2, FMaxAmmo[A_ROCKETS]);
        FWeapon[WEAPON_ROCKETLAUNCHER] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_PLASMA:
      if (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or not FWeapon[WEAPON_PLASMA] then
      begin
        if a and FWeapon[WEAPON_PLASMA] then Exit;
        switchWeapon := WEAPON_PLASMA;
        hadWeapon := FWeapon[WEAPON_PLASMA];
        IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
        FWeapon[WEAPON_PLASMA] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_BFG:
      if (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or not FWeapon[WEAPON_BFG] then
      begin
        if a and FWeapon[WEAPON_BFG] then Exit;
        switchWeapon := WEAPON_BFG;
        hadWeapon := FWeapon[WEAPON_BFG];
        IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
        FWeapon[WEAPON_BFG] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_SUPERPULEMET:
      if (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or not FWeapon[WEAPON_SUPERPULEMET] then
      begin
        if a and FWeapon[WEAPON_SUPERPULEMET] then Exit;
        switchWeapon := WEAPON_SUPERPULEMET;
        hadWeapon := FWeapon[WEAPON_SUPERPULEMET];
        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        FWeapon[WEAPON_SUPERPULEMET] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_WEAPON_FLAMETHROWER:
      if (FAmmo[A_FUEL] < FMaxAmmo[A_FUEL]) or not FWeapon[WEAPON_FLAMETHROWER] then
      begin
        if a and FWeapon[WEAPON_FLAMETHROWER] then Exit;
        switchWeapon := WEAPON_FLAMETHROWER;
        hadWeapon := FWeapon[WEAPON_FLAMETHROWER];
        IncMax(FAmmo[A_FUEL], 100, FMaxAmmo[A_FUEL]);
        FWeapon[WEAPON_FLAMETHROWER] := True;
        Result := True;
        if gFlash = 2 then Inc(FPickup, 5);
        if a and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETWEAPON');
      end;

    ITEM_AMMO_BULLETS:
      if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then
      begin
        IncMax(FAmmo[A_BULLETS], 10, FMaxAmmo[A_BULLETS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_BULLETS_BOX:
      if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then
      begin
        IncMax(FAmmo[A_BULLETS], 50, FMaxAmmo[A_BULLETS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_SHELLS:
      if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then
      begin
        IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_SHELLS_BOX:
      if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then
      begin
        IncMax(FAmmo[A_SHELLS], 25, FMaxAmmo[A_SHELLS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_ROCKET:
      if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then
      begin
        IncMax(FAmmo[A_ROCKETS], 1, FMaxAmmo[A_ROCKETS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_ROCKET_BOX:
      if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then
      begin
        IncMax(FAmmo[A_ROCKETS], 5, FMaxAmmo[A_ROCKETS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_CELL:
      if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then
      begin
        IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_CELL_BIG:
      if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then
      begin
        IncMax(FAmmo[A_CELLS], 100, FMaxAmmo[A_CELLS]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_FUELCAN:
      if FAmmo[A_FUEL] < FMaxAmmo[A_FUEL] then
      begin
        IncMax(FAmmo[A_FUEL], 100, FMaxAmmo[A_FUEL]);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_AMMO_BACKPACK:
      if not(R_ITEM_BACKPACK in FRulez) or
            (FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS]) or
            (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or
            (FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS]) or
            (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or
            (FAmmo[A_FUEL] < FMaxAmmo[A_FUEL]) then
      begin
        FMaxAmmo[A_BULLETS] := AmmoLimits[1, A_BULLETS];
        FMaxAmmo[A_SHELLS] := AmmoLimits[1, A_SHELLS];
        FMaxAmmo[A_ROCKETS] := AmmoLimits[1, A_ROCKETS];
        FMaxAmmo[A_CELLS] := AmmoLimits[1, A_CELLS];
        FMaxAmmo[A_FUEL] := AmmoLimits[1, A_FUEL];

        if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then
          IncMax(FAmmo[A_BULLETS], 10, FMaxAmmo[A_BULLETS]);
        if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then
          IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then
          IncMax(FAmmo[A_ROCKETS], 1, FMaxAmmo[A_ROCKETS]);
        if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then
          IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
        if FAmmo[A_FUEL] < FMaxAmmo[A_FUEL] then
          IncMax(FAmmo[A_FUEL], 50, FMaxAmmo[A_FUEL]);

        FRulez := FRulez + [R_ITEM_BACKPACK];
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_KEY_RED:
      if not(R_KEY_RED in FRulez) then
      begin
        Include(FRulez, R_KEY_RED);
        Result := True;
        remove := (gGameSettings.GameMode <> GM_COOP) and (g_Player_GetCount() < 2);
        if gFlash = 2 then Inc(FPickup, 5);
        if (not remove) and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_KEY_GREEN:
      if not(R_KEY_GREEN in FRulez) then
      begin
        Include(FRulez, R_KEY_GREEN);
        Result := True;
        remove := (gGameSettings.GameMode <> GM_COOP) and (g_Player_GetCount() < 2);
        if gFlash = 2 then Inc(FPickup, 5);
        if (not remove) and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_KEY_BLUE:
      if not(R_KEY_BLUE in FRulez) then
      begin
        Include(FRulez, R_KEY_BLUE);
        Result := True;
        remove := (gGameSettings.GameMode <> GM_COOP) and (g_Player_GetCount() < 2);
        if gFlash = 2 then Inc(FPickup, 5);
        if (not remove) and g_Game_IsNet then MH_SEND_Sound(GameX, GameY, 'SOUND_ITEM_GETITEM');
      end;

    ITEM_SUIT:
      if FMegaRulez[MR_SUIT] < gTime+PLAYER_SUIT_TIME then
      begin
        FMegaRulez[MR_SUIT] := gTime+PLAYER_SUIT_TIME;
        Result := True;
        remove := True;
        FFireTime := 0;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_OXYGEN:
      if FAir < AIR_MAX then
      begin
        FAir := AIR_MAX;
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_MEDKIT_BLACK:
      begin
        if not (R_BERSERK in FRulez) then
        begin
          Include(FRulez, R_BERSERK);
          if (FBFGFireCounter = -1) then
          begin
            FCurrWeap := WEAPON_KASTET;
            resetWeaponQueue();
            FModel.SetWeapon(WEAPON_KASTET);
          end;
          if gFlash <> 0 then
          begin
            Inc(FPain, 100);
            if gFlash = 2 then Inc(FPickup, 5);
          end;
          FBerserk := gTime+30000;
          Result := True;
          remove := True;
          FFireTime := 0;
        end;
        if (FHealth < PLAYER_HP_SOFT) or (FFireTime > 0) then
        begin
          if FHealth < PLAYER_HP_SOFT then FHealth := PLAYER_HP_SOFT;
          FBerserk := gTime+30000;
          Result := True;
          remove := True;
          FFireTime := 0;
        end;
      end;

    ITEM_INVUL:
      if FMegaRulez[MR_INVUL] < gTime+PLAYER_INVUL_TIME then
      begin
        FMegaRulez[MR_INVUL] := gTime+PLAYER_INVUL_TIME;
        FSpawnInvul := 0;
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_BOTTLE:
      if (FHealth < PLAYER_HP_LIMIT) or (FFireTime > 0) then
      begin
        if FHealth < PLAYER_HP_LIMIT then IncMax(FHealth, 4, PLAYER_HP_LIMIT);
        Result := True;
        remove := True;
        FFireTime := 0;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_HELMET:
      if FArmor < PLAYER_AP_LIMIT then
      begin
        IncMax(FArmor, 5, PLAYER_AP_LIMIT);
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_JETPACK:
      if FJetFuel < JET_MAX then
      begin
        FJetFuel := JET_MAX;
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;

    ITEM_INVIS:
      if FMegaRulez[MR_INVIS] < gTime+PLAYER_INVIS_TIME then
      begin
        FMegaRulez[MR_INVIS] := gTime+PLAYER_INVIS_TIME;
        Result := True;
        remove := True;
        if gFlash = 2 then Inc(FPickup, 5);
      end;
  end;

  if (shouldSwitch(switchWeapon, hadWeapon)) then
    QueueWeaponSwitch(switchWeapon);
end;

procedure TPlayer.Touch();
begin
  if not FAlive then
    Exit;
  //FModel.PlaySound(MODELSOUND_PAIN, 1, FObj.X, FObj.Y);
  if FIamBot then
  begin
  // Бросить флаг товарищу:
    if gGameSettings.GameMode = GM_CTF then
      DropFlag();
  end;
end;

procedure TPlayer.Push(vx, vy: Integer);
begin
  if (not FPhysics) and FGhost then
    Exit;
  FObj.Accel.X := FObj.Accel.X + vx;
  FObj.Accel.Y := FObj.Accel.Y + vy;
  if g_Game_IsNet and g_Game_IsServer then
    MH_SEND_PlayerPos(True, FUID, NET_EVERYONE);
end;

procedure TPlayer.Reset(Force: Boolean);
begin
  if Force then
    FAlive := False;

  FSpawned := False;
  FTime[T_RESPAWN] := 0;
  FTime[T_FLAGCAP] := 0;
  FGodMode := False;
  FNoTarget := False;
  FNoReload := False;
  FFrags := 0;
  FLastFrag := 0;
  FComboEvnt := -1;
  FKills := 0;
  FMonsterKills := 0;
  FDeath := 0;
  FSecrets := 0;
  FSpawnInvul := 0;
  {$IFDEF ENABLE_CORPSES}
    FCorpse := -1;
  {$ENDIF}
  FReady := False;
  if FNoRespawn then
  begin
    FSpectator := False;
    FGhost := False;
    FPhysics := True;
    FSpectatePlayer := -1;
    FNoRespawn := False;
  end;
  FLives := gGameSettings.MaxLives;

  SetFlag(FLAG_NONE);
end;

procedure TPlayer.SoftReset();
begin
  ReleaseKeys();

  FDamageBuffer := 0;
  FSlopeOld := 0;
  FIncCamOld := 0;
  FIncCam := 0;
  FBFGFireCounter := -1;
  {$IFDEF ENABLE_SHELLS}
    FShellTimer := -1;
  {$ENDIF}
  FPain := 0;
  FLastHit := 0;
  FLastFrag := 0;
  FComboEvnt := -1;

  SetFlag(FLAG_NONE);
  SetAction(A_STAND, True);
end;

function TPlayer.GetRespawnPoint(): Byte;
var
  c: Byte;
begin
  Result := 255;
  // На будущее: FSpawn - игрок уже играл и перерождается

  // Одиночная игра/кооператив
  if gGameSettings.GameMode in [GM_COOP, GM_SINGLE] then
  begin
    if Self = gPlayer1 then
    begin
      // player 1 should try to spawn on the player 1 point
      if g_Map_GetPointCount(RESPAWNPOINT_PLAYER1) > 0 then
        Exit(RESPAWNPOINT_PLAYER1)
      else if g_Map_GetPointCount(RESPAWNPOINT_PLAYER2) > 0 then
        Exit(RESPAWNPOINT_PLAYER2);
    end
    else if Self = gPlayer2 then
    begin
      // player 2 should try to spawn on the player 2 point
      if g_Map_GetPointCount(RESPAWNPOINT_PLAYER2) > 0 then
        Exit(RESPAWNPOINT_PLAYER2)
      else if g_Map_GetPointCount(RESPAWNPOINT_PLAYER1) > 0 then
        Exit(RESPAWNPOINT_PLAYER1);
    end
    else
    begin
      // other players randomly pick either the first or the second point
      c := IfThen((Random(2) = 0), RESPAWNPOINT_PLAYER1, RESPAWNPOINT_PLAYER2);
      if g_Map_GetPointCount(c) > 0 then
        Exit(c);
        // try the other one
      c := IfThen((c = RESPAWNPOINT_PLAYER1), RESPAWNPOINT_PLAYER2, RESPAWNPOINT_PLAYER1);
      if g_Map_GetPointCount(c) > 0 then
        Exit(c);
    end;
  end;

  // Мясоповал
  if gGameSettings.GameMode = GM_DM then
  begin
    // try DM points first
    if g_Map_GetPointCount(RESPAWNPOINT_DM) > 0 then
      Exit(RESPAWNPOINT_DM);
  end;

  // Командные
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
    // try team points first
    c := RESPAWNPOINT_DM;
    if FTeam = TEAM_RED then
      c := RESPAWNPOINT_RED
    else if FTeam = TEAM_BLUE then
      c := RESPAWNPOINT_BLUE;
    if g_Map_GetPointCount(c) > 0 then
      Exit(c);
  end;

  // still haven't found a spawnpoint, try random shit
  Result := g_Map_GetRandomPointType();
end;

procedure TPlayer.Respawn(Silent: Boolean; Force: Boolean = False);
var
  RespawnPoint: TRespawnPoint;
  a, b, c: Byte;
begin
  FSlopeOld := 0;
  FIncCamOld := 0;
  FIncCam := 0;
  FBFGFireCounter := -1;
  {$IFDEF ENABLE_SHELLS}
    FShellTimer := -1;
  {$ENDIF}
  FPain := 0;
  FLastHit := 0;
  FSpawnInvul := 0;

  {$IFDEF ENABLE_CORPSES}
    FCorpse := -1;
  {$ENDIF}

  if not g_Game_IsServer then
    Exit;
  if FDummy then
    Exit;
  FWantsInGame := True;
  FJustTeleported := True;
  if Force then
  begin
    FTime[T_RESPAWN] := 0;
    FAlive := False;
  end;
  FNetTime := 0;
  // if server changes MaxLives we gotta be ready
  if gGameSettings.MaxLives = 0 then FNoRespawn := False;

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

  if (gGameSettings.GameType <> GT_SINGLE) and (gGameSettings.GameMode <> GM_COOP) then
    begin // "Своя игра"
    // Берсерк не сохраняется между уровнями:
      FRulez := FRulez-[R_BERSERK];
    end
  else // "Одиночная игра"/"Кооп"
    begin
    // Берсерк и ключи не сохраняются между уровнями:
      FRulez := FRulez-[R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE, R_BERSERK];
    end;

// Получаем точку спауна игрока:
  c := GetRespawnPoint();

  ReleaseKeys();
  SetFlag(FLAG_NONE);

// Воскрешение без оружия:
  if not FAlive then
  begin
    FHealth := Round(PLAYER_HP_SOFT * (FHandicap / 100));
    FArmor := 0;
    FAlive := True;
    FAir := AIR_DEF;
    FJetFuel := 0;

    for a := WP_FIRST to WP_LAST do
    begin
      FWeapon[a] := False;
      FReloading[a] := 0;
    end;

    FWeapon[WEAPON_PISTOL] := True;
    FWeapon[WEAPON_KASTET] := True;
    FCurrWeap := WEAPON_PISTOL;
    resetWeaponQueue();

    FModel.SetWeapon(FCurrWeap);

    for b := A_BULLETS to A_HIGH do
      FAmmo[b] := 0;

    FAmmo[A_BULLETS] := 50;

    FMaxAmmo[A_BULLETS] := AmmoLimits[0, A_BULLETS];
    FMaxAmmo[A_SHELLS] := AmmoLimits[0, A_SHELLS];
    FMaxAmmo[A_ROCKETS] := AmmoLimits[0, A_SHELLS];
    FMaxAmmo[A_CELLS] := AmmoLimits[0, A_CELLS];
    FMaxAmmo[A_FUEL] := AmmoLimits[0, A_FUEL];

    if (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]) and
       LongBool(gGameSettings.Options and GAME_OPTION_DMKEYS) then
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
  FObj.oldX := FObj.X; // don't interpolate after respawn
  FObj.oldY := FObj.Y;
  FObj.Vel.X := 0;
  FObj.Vel.Y := 0;
  FObj.Accel.X := 0;
  FObj.Accel.Y := 0;

  FDirection := RespawnPoint.Direction;
  if FDirection = TDirection.D_LEFT then
    FAngle := 180
  else
    FAngle := 0;

  SetAction(A_STAND, True);
  FModel.Direction := FDirection;

  for a := Low(FTime) to High(FTime) do
    FTime[a] := 0;

  for a := Low(FMegaRulez) to High(FMegaRulez) do
    FMegaRulez[a] := 0;

// Respawn invulnerability
  if (gGameSettings.GameType <> GT_SINGLE) and (gGameSettings.SpawnInvul > 0) then
  begin
    FMegaRulez[MR_INVUL] := gTime + gGameSettings.SpawnInvul * 1000;
    FSpawnInvul := FMegaRulez[MR_INVUL];
  end;

  FDamageBuffer := 0;
  FJetpack := False;
  FCanJetpack := False;
  FFlaming := False;
  FFireTime := 0;
  FFirePainTime := 0;
  FFireAttacker := 0;

  {$IFDEF ENABLE_GFX}
    // Анимация возрождения:
    if (not gLoadGameMode) and (not Silent) then
    begin
      g_GFX_QueueEffect(
        R_GFX_TELEPORT_FAST,
        FObj.X + PLAYER_RECT.X + (PLAYER_RECT.Width div 2) - 32,
        FObj.Y + PLAYER_RECT.Y + (PLAYER_RECT.Height div 2) - 32
      );
    end;
  {$ENDIF}

  FSpectator := False;
  FGhost := False;
  FPhysics := True;
  FSpectatePlayer := -1;
  FSpawned := True;

  if (gPlayer1 = nil) and (gSpectLatchPID1 = FUID) then
    gPlayer1 := self;
  if (gPlayer2 = nil) and (gSpectLatchPID2 = FUID) then
    gPlayer2 := self;

  if g_Game_IsNet then
  begin
    MH_SEND_PlayerPos(True, FUID, NET_EVERYONE);
    MH_SEND_PlayerStats(FUID, NET_EVERYONE);
    if not Silent then
      MH_SEND_Effect(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-32,
                     FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32,
                     0, NET_GFX_TELE);
  end;
end;

procedure TPlayer.Spectate(NoMove: Boolean = False);
begin
  if FAlive then
    Kill(K_EXTRAHARDKILL, FUID, HIT_SOME)
  else if (not NoMove) then
  begin
    GameX := gMapInfo.Width div 2;
    GameY := gMapInfo.Height div 2;
  end;
  FXTo := GameX;
  FYTo := GameY;

  FAlive := False;
  FSpectator := True;
  FGhost := True;
  FPhysics := False;
  FWantsInGame := False;
  FSpawned := False;

  {$IFDEF ENABLE_CORPSES}
    FCorpse := -1;
  {$ENDIF}

  if FNoRespawn then
  begin
    if Self = gPlayer1 then
    begin
      gSpectLatchPID1 := FUID;
      gPlayer1 := nil;
    end
    else if Self = gPlayer2 then
    begin
      gSpectLatchPID2 := FUID;
      gPlayer2 := nil;
    end;
  end;

  if g_Game_IsNet then
    MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.SwitchNoClip;
begin
  if not FAlive then
    Exit;
  FGhost := not FGhost;
  FPhysics := not FGhost;
  if FGhost then
  begin
    FXTo := FObj.X;
    FYTo := FObj.Y;
  end else
  begin
    FObj.Accel.X := 0;
    FObj.Accel.Y := 0;
  end;
end;

procedure TPlayer.Run(Direction: TDirection);
  {$IFDEF ENABLE_GIBS}
    var a, b: Integer;
  {$ENDIF}
begin
  if MAX_RUNVEL > 8 then
    FlySmoke();

// Бежим:
  if Direction = TDirection.D_LEFT then
    begin
      if FObj.Vel.X > -MAX_RUNVEL then
        FObj.Vel.X := FObj.Vel.X - (MAX_RUNVEL shr 3);
    end
  else
    if FObj.Vel.X < MAX_RUNVEL then
      FObj.Vel.X := FObj.Vel.X + (MAX_RUNVEL shr 3);

  {$IFDEF ENABLE_GIBS}
    // Возможно, пинаем куски:
    if (FObj.Vel.X <> 0) and (gGibs <> nil) then
    begin
      b := Abs(FObj.Vel.X);
      if b > 1 then b := b * (Random(8 div b) + 1);
      for a := 0 to High(gGibs) do
      begin
        if gGibs[a].alive and
           g_Obj_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y+FObj.Rect.Height-4,
                         FObj.Rect.Width, 8, @gGibs[a].Obj) and (Random(3) = 0) then
        begin
          // Пинаем куски
          if FObj.Vel.X < 0 then
          begin
            g_Obj_PushA(@gGibs[a].Obj, b, Random(61)+120) // налево
          end
          else
          begin
            g_Obj_PushA(@gGibs[a].Obj, b, Random(61));    // направо
          end;
          gGibs[a].positionChanged(); // this updates spatial accelerators
        end;
      end;
    end;
  {$ENDIF}

  SetAction(A_WALK);
end;

procedure TPlayer.SeeDown();
begin
  SetAction(A_SEEDOWN);

  if FDirection = TDirection.D_LEFT then FAngle := ANGLE_LEFTDOWN else FAngle := ANGLE_RIGHTDOWN;

  if FIncCam > -120 then DecMin(FIncCam, 5, -120);
end;

procedure TPlayer.SeeUp();
begin
  SetAction(A_SEEUP);

  if FDirection = TDirection.D_LEFT then FAngle := ANGLE_LEFTUP else FAngle := ANGLE_RIGHTUP;

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
begin
  Result := False;

  if g_CollideLevel(X, Y, PLAYER_RECT.Width, PLAYER_RECT.Height) then
  begin
    g_Sound_PlayExAt('SOUND_GAME_NOTELEPORT', FObj.X, FObj.Y);
    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Sound(FObj.X, FObj.Y, 'SOUND_GAME_NOTELEPORT');
    Exit;
  end;

  FJustTeleported := True;

  if not silent then
  begin
    g_Sound_PlayExAt('SOUND_GAME_TELEPORT', FObj.X, FObj.Y);
    {$IFDEF ENABLE_GFX}
      g_GFX_QueueEffect(
        R_GFX_TELEPORT_FAST,
        FObj.X + PLAYER_RECT.X + (PLAYER_RECT.Width div 2) - 32,
        FObj.Y + PLAYER_RECT.Y + (PLAYER_RECT.Height div 2) - 32
      );
    {$ENDIF}
    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Effect(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-32,
                     FObj.Y+PLAYER_RECT.Y+(PLAYER_RECT.Height div 2)-32, 1,
                     NET_GFX_TELE);
  end;

  FObj.X := X-PLAYER_RECT.X;
  FObj.Y := Y-PLAYER_RECT.Y;
  FObj.oldX := FObj.X; // don't interpolate after respawn
  FObj.oldY := FObj.Y;
  if FAlive and FGhost then
  begin
    FXTo := FObj.X;
    FYTo := FObj.Y;
  end;

  if not g_Game_IsNet then
  begin
    if dir = 1 then
    begin
      SetDirection(TDirection.D_LEFT);
      FAngle := 180;
    end
    else
      if dir = 2 then
      begin
        SetDirection(TDirection.D_RIGHT);
        FAngle := 0;
      end
      else
        if dir = 3 then
        begin // обратное
          if FDirection = TDirection.D_RIGHT then
          begin
            SetDirection(TDirection.D_LEFT);
            FAngle := 180;
          end
          else
          begin
            SetDirection(TDirection.D_RIGHT);
            FAngle := 0;
          end;
        end;
  end;

  if not silent then
  begin
    {$IFDEF ENABLE_GFX}
      g_GFX_QueueEffect(
        R_GFX_TELEPORT_FAST,
        FObj.X + PLAYER_RECT.X + (PLAYER_RECT.Width div 2) - 32,
        FObj.Y + PLAYER_RECT.Y + (PLAYER_RECT.Height div 2) - 32
      );
    {$ENDIF}
    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Effect(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2)-32,
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

procedure TPlayer.PreUpdate();
begin
  FSlopeOld := FObj.slopeUpLeft;
  FIncCamOld := FIncCam;
  FObj.oldX := FObj.X;
  FObj.oldY := FObj.Y;
end;

procedure TPlayer.Update();
var
  b: Byte;
  i, ii, wx, wy, xd, yd, k: Integer;
  blockmon, headwater, dospawn: Boolean;
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

  if NetServer then
    if (FClientID >= 0) and (NetClients[FClientID].Peer <> nil) then
    begin
      FPing := NetClients[FClientID].Peer^.lastRoundTripTime;
      if NetClients[FClientID].Peer^.packetsSent > 0 then
        FLoss := Round(100*NetClients[FClientID].Peer^.packetsLost/NetClients[FClientID].Peer^.packetsSent)
      else
        FLoss := 0;
    end else
    begin
      FPing := 0;
      FLoss := 0;
    end;

  if FAlive then
    FPunchAnim.Update;
  if FPunchAnim.played then
    FPunchAnim.Disable;

  if FAlive and (gFly or FJetpack) then
    FlySmoke();

  if FDirection = TDirection.D_LEFT then
    FAngle := 180
  else
    FAngle := 0;

  if FAlive and (not FGhost) then
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
    if (FObj.Vel.X = 0) and FAlive then
    begin
      if FKeys[KEY_LEFT].Pressed then
        Run(TDirection.D_LEFT);
      if FKeys[KEY_RIGHT].Pressed then
        Run(TDirection.D_RIGHT);
    end;

    if FPhysics then
    begin
      g_Obj_Move(@FObj, True, True, True);
      positionChanged(); // this updates spatial accelerators
    end;

    Exit;
  end;

  FActionChanged := False;

  if FAlive then
  begin
    // Let alive player do some actions
    if FKeys[KEY_LEFT].Pressed then Run(TDirection.D_LEFT);
    if FKeys[KEY_RIGHT].Pressed then Run(TDirection.D_RIGHT);
    if FKeys[KEY_FIRE].Pressed and AnyServer then Fire()
    else
    begin
      if AnyServer then
      begin
        FlamerOff;
        if NetServer then MH_SEND_PlayerStats(FUID);
      end;
    end;
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
    if not FGhost then
      for k := Low(FKeys) to KEY_CHAT-1 do
      begin
        if FKeys[k].Pressed then
        begin
          dospawn := True;
          break;
        end;
      end;
    if dospawn then
    begin
      if gGameSettings.GameType in [GT_CUSTOM, GT_SERVER, GT_CLIENT] then
        Respawn(False)
      else // Single
        if (FTime[T_RESPAWN] <= gTime) and
          gGameOn and (not FAlive) then
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
    // Dead spectator actions
    if FGhost then
    begin
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
                if gPlayers[I].alive then
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
  // No clipping
  if FGhost then
  begin
    if FKeys[KEY_UP].Pressed or FKeys[KEY_JUMP].Pressed then
    begin
      FYTo := FObj.Y - 32;
      FSpectatePlayer := -1;
    end;
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
      FXTo := FObj.X + 32;
      FSpectatePlayer := -1;
    end;

    if (FXTo < -64) then
      FXTo := -64
    else if (FXTo > gMapInfo.Width + 32) then
      FXTo := gMapInfo.Width + 32;
    if (FYTo < -72) then
      FYTo := -72
    else if (FYTo > gMapInfo.Height + 32) then
      FYTo := gMapInfo.Height + 32;
  end;

  if FPhysics then
  begin
    g_Obj_Move(@FObj, True, True, True);
    positionChanged(); // this updates spatial accelerators
  end
  else
  begin
    FObj.Vel.X := 0;
    FObj.Vel.Y := 0;
    if FSpectator then
      if (FSpectatePlayer <= High(gPlayers)) and (FSpectatePlayer >= 0) then
        if gPlayers[FSpectatePlayer] <> nil then
          if gPlayers[FSpectatePlayer].alive then
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
  if (not FAlive) or not (FKeys[KEY_LEFT].Pressed or FKeys[KEY_RIGHT].Pressed) then
    if FObj.Vel.X <> 0 then
      FObj.Vel.X := z_dec(FObj.Vel.X, 1);

  if (FLastHit = HIT_TRAP) and (FPain > 90) then FPain := 90;
  DecMin(FPain, 5, 0);
  DecMin(FPickup, 1, 0);

  if FAlive and (FObj.Y > Integer(gMapInfo.Height)+128) and AnyServer then
  begin
    // Обнулить действия примочек, чтобы фон пропал
    FMegaRulez[MR_SUIT] := 0;
    FMegaRulez[MR_INVUL] := 0;
    FMegaRulez[MR_INVIS] := 0;
    Kill(K_FALLKILL, 0, HIT_FALL);
  end;

  i := 9;

  if FAlive then
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

    for b := WP_FIRST to WP_LAST do
      if FReloading[b] > 0 then
        if FNoReload then
          FReloading[b] := 0
        else
          Dec(FReloading[b]);

{$IFDEF ENABLE_SHELLS}
    if FShellTimer > -1 then
      if FShellTimer = 0 then
      begin
        if FShellType = SHELL_SHELL then
          g_Shells_Create(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                               GameVelX, GameVelY-2, SHELL_SHELL)
        else if FShellType = SHELL_DBLSHELL then
        begin
          g_Shells_Create(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                               GameVelX+1, GameVelY-2, SHELL_SHELL);
          g_Shells_Create(GameX+PLAYER_RECT_CX, GameY+PLAYER_RECT_CX,
                               GameVelX-1, GameVelY-2, SHELL_SHELL);
        end;
        FShellTimer := -1;
      end else Dec(FShellTimer);
{$ENDIF}

    if (FBFGFireCounter > -1) then
      if FBFGFireCounter = 0 then
      begin
        if AnyServer then
        begin
          wx := FObj.X+WEAPONPOINT[FDirection].X;
          wy := FObj.Y+WEAPONPOINT[FDirection].Y;
          xd := wx+IfThen(FDirection = TDirection.D_LEFT, -30, 30);
          yd := wy+firediry();
          g_Weapon_bfgshot(wx, wy, xd, yd, FUID);
          if NetServer then MH_SEND_PlayerFire(FUID, WEAPON_BFG, wx, wy, xd, yd);
          if (FAngle = 0) or (FAngle = 180) then SetAction(A_ATTACK)
            else if (FAngle = ANGLE_LEFTDOWN) or (FAngle = ANGLE_RIGHTDOWN) then SetAction(A_ATTACKDOWN)
              else if (FAngle = ANGLE_LEFTUP) or (FAngle = ANGLE_RIGHTUP) then SetAction(A_ATTACKUP);
        end;

        FReloading[WEAPON_BFG] := WEAPON_RELOAD[WEAPON_BFG];
        FBFGFireCounter := -1;
      end else
        if FNoReload then
          FBFGFireCounter := 0
        else
          Dec(FBFGFireCounter);

    if (FMegaRulez[MR_SUIT] < gTime) and AnyServer then
    begin
      b := g_GetAcidHit(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width, PLAYER_RECT.Height);

      if (b > 0) and (gTime mod (15*GAME_TICK) = 0) then Damage(b, 0, 0, 0, HIT_ACID);
    end;

    if (headwater or blockmon) then
    begin
      Dec(FAir);

      if FAir < -9 then
      begin
        if AnyServer then Damage(10, 0, 0, 0, HIT_WATER);
          FAir := 0;
      end
      else if (FAir mod 31 = 0) and not blockmon then
      begin
        {$IFDEF ENABLE_GFX}
          g_GFX_Bubbles(FObj.X+PLAYER_RECT.X+(PLAYER_RECT.Width div 2), FObj.Y+PLAYER_RECT.Y-4, 5+Random(6), 8, 4);
        {$ENDIF}
        if Random(2) = 0
          then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
          else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
      end;
    end else if FAir < AIR_DEF then
      FAir := AIR_DEF;

    if FFireTime > 0 then
    begin
      if BodyInLiquid(0, 0) then
      begin
        FFireTime := 0;
        FFirePainTime := 0;
      end
      else if FMegaRulez[MR_SUIT] >= gTime then
      begin
        if FMegaRulez[MR_SUIT] = gTime then
          FFireTime := 1;
        FFirePainTime := 0;
      end
      else
      begin
        OnFireFlame(1);
        if FFirePainTime <= 0 then
        begin
          if g_Game_IsServer then
            Damage(2, FFireAttacker, 0, 0, HIT_FLAME);
          FFirePainTime := 12 - FFireTime div 12;
        end;
        FFirePainTime := FFirePainTime - 1;
        FFireTime := FFireTime - 1;
        if ((FFireTime mod 33) = 0) and (FMegaRulez[MR_INVUL] < gTime) then
          FModel.PlaySound(MODELSOUND_PAIN, 1, FObj.X, FObj.Y);
        if (FFireTime = 0) and g_Game_IsNet and g_Game_IsServer then
          MH_SEND_PlayerStats(FUID);
      end;
    end;

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

      if FAlive and ((FLastHit <> HIT_FLAME) or (FFireTime <= 0)) then
      begin
        if FDamageBuffer <= 20 then FModel.PlaySound(MODELSOUND_PAIN, 1, FObj.X, FObj.Y)
          else if FDamageBuffer <= 55 then FModel.PlaySound(MODELSOUND_PAIN, 2, FObj.X, FObj.Y)
            else if FDamageBuffer <= 120 then FModel.PlaySound(MODELSOUND_PAIN, 3, FObj.X, FObj.Y)
              else FModel.PlaySound(MODELSOUND_PAIN, 4, FObj.X, FObj.Y);
      end;

      FDamageBuffer := 0;
    end;

    {CollideItem();}
  end; // if FAlive then ...

  if (FActionAnim = A_PAIN) and (FModel.Animation <> A_PAIN) then
  begin
    FModel.ChangeAnimation(FActionAnim, FActionForce);
    FModel.AnimState.MinLength := i;
  end else FModel.ChangeAnimation(FActionAnim, FActionForce and (FModel.Animation <> A_STAND));

  if (FModel.AnimState.Played or ((not FActionChanged) and (FModel.Animation = A_WALK)))
  then SetAction(A_STAND, True);

  if not ((FModel.Animation = A_WALK) and (Abs(FObj.Vel.X) < 4) and not FModel.GetFire()) then FModel.Update;

  for b := Low(FKeys) to High(FKeys) do
    if FKeys[b].Time = 0 then FKeys[b].Pressed := False else Dec(FKeys[b].Time);
end;


procedure TPlayer.getMapBox (out x, y, w, h: Integer); inline;
begin
  x := FObj.X+PLAYER_RECT.X;
  y := FObj.Y+PLAYER_RECT.Y;
  w := PLAYER_RECT.Width;
  h := PLAYER_RECT.Height;
end;


procedure TPlayer.moveBy (dx, dy: Integer); inline;
begin
  if (dx <> 0) or (dy <> 0) then
  begin
    FObj.X += dx;
    FObj.Y += dy;
    positionChanged();
  end;
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
var
  a: Integer;
begin
  if FTime[T_USE] > gTime then Exit;

  g_Triggers_PressR(FObj.X+PLAYER_RECT.X, FObj.Y+PLAYER_RECT.Y, PLAYER_RECT.Width,
                    PLAYER_RECT.Height, FUID, ACTIVATE_PLAYERPRESS);

  for a := 0 to High(gPlayers) do
    if (gPlayers[a] <> nil) and (gPlayers[a] <> Self) and
       gPlayers[a].alive and SameTeam(FUID, gPlayers[a].FUID) and
       g_Obj_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                     FObj.Rect.Width, FObj.Rect.Height, @gPlayers[a].FObj) then
    begin
      gPlayers[a].Touch();
      if g_Game_IsNet and g_Game_IsServer then
        MH_SEND_GameEvent(NET_EV_PLAYER_TOUCH, gPlayers[a].FUID);
    end;

  FTime[T_USE] := gTime+120;
end;

procedure TPlayer.NetFire(Wpn: Byte; X, Y, AX, AY: Integer; WID: Integer = -1);
var
  locObj: TObj;
  visible: Boolean = True;
  WX, WY, XD, YD: Integer;
begin
  WX := X;
  WY := Y;
  XD := AX;
  YD := AY;

  case FCurrWeap of
    WEAPON_KASTET:
    begin
      visible := False;
      DoPunch();
      if R_BERSERK in FRulez then
      begin
        //g_Weapon_punch(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, 75, FUID);
        locobj.X := FObj.X+FObj.Rect.X;
        locobj.Y := FObj.Y+FObj.Rect.Y;
        locobj.rect.X := 0;
        locobj.rect.Y := 0;
        locobj.rect.Width := 39;
        locobj.rect.Height := 52;
        locobj.Vel.X := (xd-wx) div 2;
        locobj.Vel.Y := (yd-wy) div 2;
        locobj.Accel.X := xd-wx;
        locobj.Accel.y := yd-wy;

        if g_Weapon_Hit(@locobj, 50, FUID, HIT_SOME) <> 0 then
          g_Sound_PlayExAt('SOUND_WEAPON_HITBERSERK', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_WEAPON_MISSBERSERK', FObj.X, FObj.Y);

        if gFlash = 1 then
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
    end;

    WEAPON_PISTOL:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', GameX, Gamey);
      FFireAngle := FAngle;
      {$IFDEF ENABLE_SHELLS}
        g_Shells_Create(GameX + PLAYER_RECT_CX, GameY + PLAYER_RECT_CX, GameVelX, GameVelY - 2, SHELL_BULLET);
      {$ENDIF}
    end;

    WEAPON_SHOTGUN1:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', Gamex, Gamey);
      FFireAngle := FAngle;
      {$IFDEF ENABLE_SHELLS}
        FShellTimer := 10;
        FShellType := SHELL_SHELL;
      {$ENDIF}
    end;

    WEAPON_SHOTGUN2:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN2', Gamex, Gamey);
      FFireAngle := FAngle;
      {$IFDEF ENABLE_SHELLS}
        FShellTimer := 13;
        FShellType := SHELL_DBLSHELL;
      {$ENDIF}
    end;

    WEAPON_CHAINGUN:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', Gamex, Gamey);
      FFireAngle := FAngle;
      {$IFDEF ENABLE_SHELLS}
        g_Shells_Create(GameX + PLAYER_RECT_CX, GameY + PLAYER_RECT_CX, GameVelX, GameVelY - 2, SHELL_BULLET);
      {$ENDIF}
    end;

    WEAPON_ROCKETLAUNCHER:
    begin
      g_Weapon_Rocket(wx, wy, xd, yd, FUID, WID);
      FFireAngle := FAngle;
    end;

    WEAPON_PLASMA:
    begin
      g_Weapon_Plasma(wx, wy, xd, yd, FUID, WID);
      FFireAngle := FAngle;
    end;

    WEAPON_BFG:
    begin
      g_Weapon_BFGShot(wx, wy, xd, yd, FUID, WID);
      FFireAngle := FAngle;
    end;

    WEAPON_SUPERPULEMET:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', Gamex, Gamey);
      FFireAngle := FAngle;
      {$IFDEF ENABLE_SHELLS}
        g_Shells_Create(GameX + PLAYER_RECT_CX, GameY + PLAYER_RECT_CX, GameVelX, GameVelY - 2, SHELL_SHELL);
      {$ENDIF}
    end;

    WEAPON_FLAMETHROWER:
    begin
      g_Weapon_flame(wx, wy, xd, yd, FUID, WID);
      FlamerOn;
      FFireAngle := FAngle;
    end;
  end;

  if not visible then Exit;

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
  FXTo := XTo;
  FYTo := YTo;
  if FJustTeleported or (NetInterpLevel < 1) then
  begin
    FObj.X := XTo;
    FObj.Y := YTo;
    if FJustTeleported then
    begin
      FObj.oldX := FObj.X;
      FObj.oldY := FObj.Y;
    end;
  end
  else
  begin
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
  s, ts: String;
  evtype, a: Byte;
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

    ts := Format('%.4d', [gFlags[FFlag].CaptureTime]);
    Insert('.', ts, Length(ts) + 1 - 3);
    g_Console_Add(Format(_lc[I_PLAYER_FLAG_CAPTURE], [FName, s, ts]), True);

    g_Map_ResetFlag(FFlag);
    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_CAPTURE], [AnsiUpperCase(s)]), 144);

    if ((Self = gPlayer1) or (Self = gPlayer2)
    or ((gPlayer1 <> nil) and (gPlayer1.Team = FTeam))
    or ((gPlayer2 <> nil) and (gPlayer2.Team = FTeam))) then
      a := 0
    else
      a := 1;

    if not sound_cap_flag[a].IsPlaying() then
      sound_cap_flag[a].Play();

    gTeamStat[FTeam].Score += 1;

    Result := True;
    if g_Game_IsNet then
    begin
      MH_SEND_FlagEvent(evtype, FFlag, FUID, False);
      MH_SEND_GameStats;
    end;

    gFlags[FFlag].CaptureTime := 0;
    SetFlag(FLAG_NONE);
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
    gFlags[Flag].CaptureTime := 0;

    g_Console_Add(Format(_lc[I_PLAYER_FLAG_RETURN], [FName, s]), True);

    g_Map_ResetFlag(Flag);
    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);

    if ((Self = gPlayer1) or (Self = gPlayer2)
    or ((gPlayer1 <> nil) and (gPlayer1.Team = FTeam))
    or ((gPlayer2 <> nil) and (gPlayer2.Team = FTeam))) then
      a := 0
    else
      a := 1;

    if not sound_ret_flag[a].IsPlaying() then
      sound_ret_flag[a].Play();

    Result := True;
    if g_Game_IsNet then
    begin
      MH_SEND_FlagEvent(evtype, Flag, FUID, False);
      MH_SEND_GameStats;
    end;
    Exit;
  end;

// Подобрал чужой флаг:
  if (Flag <> FTeam) and (FTime[T_FLAGCAP] <= gTime) then
  begin
    SetFlag(Flag);

    if Flag = FLAG_RED then
      s := _lc[I_PLAYER_FLAG_RED]
    else
      s := _lc[I_PLAYER_FLAG_BLUE];

    evtype := FLAG_STATE_CAPTURED;

    g_Console_Add(Format(_lc[I_PLAYER_FLAG_GET], [FName, s]), True);

    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_GET], [AnsiUpperCase(s)]), 144);

    gFlags[Flag].State := FLAG_STATE_CAPTURED;

    if ((Self = gPlayer1) or (Self = gPlayer2)
    or ((gPlayer1 <> nil) and (gPlayer1.Team = FTeam))
    or ((gPlayer2 <> nil) and (gPlayer2.Team = FTeam))) then
      a := 0
    else
      a := 1;

    if not sound_get_flag[a].IsPlaying() then
      sound_get_flag[a].Play();

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
  if FModel <> nil then
    FModel.SetFlag(FFlag);
end;

function TPlayer.TryDropFlag(): Boolean;
begin
  if LongBool(gGameSettings.Options and GAME_OPTION_ALLOWDROPFLAG) then
    Result := DropFlag(False, LongBool(gGameSettings.Options and GAME_OPTION_THROWFLAG))
  else
    Result := False;
end;

function TPlayer.DropFlag(Silent: Boolean = True; DoThrow: Boolean = False): Boolean;
var
  s: String;
  a: Byte;
  xv, yv: Integer;
begin
  Result := False;
  if (not g_Game_IsServer) or (FFlag = FLAG_NONE) then
    Exit;
  FTime[T_FLAGCAP] := gTime + 2000;
  with gFlags[FFlag] do
  begin
    Obj.X := FObj.X;
    Obj.Y := FObj.Y;
    Direction := FDirection;
    State := FLAG_STATE_DROPPED;
    Count := FLAG_TIME;
    if DoThrow then
    begin
      xv := FObj.Vel.X + IfThen(Direction = TDirection.D_RIGHT, 10, -10);
      yv := FObj.Vel.Y - 2;
    end
    else
    begin
      xv := (FObj.Vel.X div 2);
      yv := (FObj.Vel.Y div 2) - 2;
    end;
    g_Obj_Push(@Obj, xv, yv);

    positionChanged(); // this updates spatial accelerators

    if FFlag = FLAG_RED then
      s := _lc[I_PLAYER_FLAG_RED]
    else
      s := _lc[I_PLAYER_FLAG_BLUE];

    g_Console_Add(Format(_lc[I_PLAYER_FLAG_DROP], [FName, s]), True);
    g_Game_Message(Format(_lc[I_MESSAGE_FLAG_DROP], [AnsiUpperCase(s)]), 144);

    if ((Self = gPlayer1) or (Self = gPlayer2)
    or ((gPlayer1 <> nil) and (gPlayer1.Team = FTeam))
    or ((gPlayer2 <> nil) and (gPlayer2.Team = FTeam))) then
      a := 0
    else
      a := 1;

    if (not Silent) and (not sound_lost_flag[a].IsPlaying()) then
      sound_lost_flag[a].Play();

    if g_Game_IsNet then
      MH_SEND_FlagEvent(FLAG_STATE_DROPPED, Flag, FUID, False);
  end;
  SetFlag(FLAG_NONE);
  Result := True;
end;

procedure TPlayer.GetSecret();
begin
  if (self = gPlayer1) or (self = gPlayer2) then
  begin
    g_Console_Add(Format(_lc[I_PLAYER_SECRET], [FName]), True);
    g_Sound_PlayEx('SOUND_GAME_SECRET');
  end;
  Inc(FSecrets);
end;

procedure TPlayer.PressKey(Key: Byte; Time: Word = 1);
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
  SavedState: TPlayerSavedState;
begin
  SavedState.Health := FHealth;
  SavedState.Armor := FArmor;
  SavedState.Air := FAir;
  SavedState.JetFuel := FJetFuel;
  SavedState.CurrWeap := FCurrWeap;
  SavedState.NextWeap := FNextWeap;
  SavedState.NextWeapDelay := FNextWeapDelay;
  for i := Low(FWeapon) to High(FWeapon) do
    SavedState.Weapon[i] := FWeapon[i];
  for i := Low(FAmmo) to High(FAmmo) do
    SavedState.Ammo[i] := FAmmo[i];
  for i := Low(FMaxAmmo) to High(FMaxAmmo) do
    SavedState.MaxAmmo[i] := FMaxAmmo[i];
  SavedState.Rulez := FRulez - [R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE];

  FSavedStateNum := -1;
  for i := Low(SavedStates) to High(SavedStates) do
    if not SavedStates[i].Used then
    begin
      FSavedStateNum := i;
      break;
    end;
  if FSavedStateNum < 0 then
  begin
    SetLength(SavedStates, Length(SavedStates) + 1);
    FSavedStateNum := High(SavedStates);
  end;

  SavedState.Used := True;
  SavedStates[FSavedStateNum] := SavedState;
end;

procedure TPlayer.RecallState();
var
  i: Integer;
  SavedState: TPlayerSavedState;
begin
  if(FSavedStateNum < 0) or (FSavedStateNum > High(SavedStates)) then
    Exit;

  SavedState := SavedStates[FSavedStateNum];
  SavedStates[FSavedStateNum].Used := False;
  FSavedStateNum := -1;

  FHealth := SavedState.Health;
  FArmor := SavedState.Armor;
  FAir := SavedState.Air;
  FJetFuel := SavedState.JetFuel;
  FCurrWeap := SavedState.CurrWeap;
  FNextWeap := SavedState.NextWeap;
  FNextWeapDelay := SavedState.NextWeapDelay;
  for i := Low(FWeapon) to High(FWeapon) do
    FWeapon[i] := SavedState.Weapon[i];
  for i := Low(FAmmo) to High(FAmmo) do
    FAmmo[i] := SavedState.Ammo[i];
  for i := Low(FMaxAmmo) to High(FMaxAmmo) do
    FMaxAmmo[i] := SavedState.MaxAmmo[i];
  FRulez := SavedState.Rulez;

  if gGameSettings.GameType = GT_SERVER then
    MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.SaveState (st: TStream);
var
  i: Integer;
  b: Byte;
begin
  // Сигнатура игрока
  utils.writeSign(st, 'PLYR');
  utils.writeInt(st, Byte(PLR_SAVE_VERSION)); // version
  // Бот или человек
  utils.writeBool(st, FIamBot);
  // UID игрока
  utils.writeInt(st, Word(FUID));
  // Имя игрока
  utils.writeStr(st, FName);
  // Команда
  utils.writeInt(st, Byte(FTeam));
  // Жив ли
  utils.writeBool(st, FAlive);
  // Израсходовал ли все жизни
  utils.writeBool(st, FNoRespawn);
  // Направление
  if FDirection = TDirection.D_LEFT then b := 1 else b := 2; // D_RIGHT
  utils.writeInt(st, Byte(b));
  // Здоровье
  utils.writeInt(st, LongInt(FHealth));
  // Коэффициент инвалидности
  utils.writeInt(st, LongInt(FHandicap));
  // Жизни
  utils.writeInt(st, Byte(FLives));
  // Броня
  utils.writeInt(st, LongInt(FArmor));
  // Запас воздуха
  utils.writeInt(st, LongInt(FAir));
  // Запас горючего
  utils.writeInt(st, LongInt(FJetFuel));
  // Боль
  utils.writeInt(st, LongInt(FPain));
  // Убил
  utils.writeInt(st, LongInt(FKills));
  // Убил монстров
  utils.writeInt(st, LongInt(FMonsterKills));
  // Фрагов
  utils.writeInt(st, LongInt(FFrags));
  // Фрагов подряд
  utils.writeInt(st, Byte(FFragCombo));
  // Время последнего фрага
  utils.writeInt(st, LongWord(FLastFrag));
  // Смертей
  utils.writeInt(st, LongInt(FDeath));
  // Какой флаг несет
  utils.writeInt(st, Byte(FFlag));
  // Нашел секретов
  utils.writeInt(st, LongInt(FSecrets));
  // Текущее оружие
  utils.writeInt(st, Byte(FCurrWeap));
  // Желаемое оружие
  utils.writeInt(st, Word(FNextWeap));
  // ...и пауза
  utils.writeInt(st, Byte(FNextWeapDelay));
  // Время зарядки BFG
  utils.writeInt(st, SmallInt(FBFGFireCounter));
  // Буфер урона
  utils.writeInt(st, LongInt(FDamageBuffer));
  // Последний ударивший
  utils.writeInt(st, Word(FLastSpawnerUID));
  // Тип последнего полученного урона
  utils.writeInt(st, Byte(FLastHit));
  // Объект игрока
  Obj_SaveState(st, @FObj);
  // Текущее количество патронов
  for i := A_BULLETS to A_HIGH do utils.writeInt(st, Word(FAmmo[i]));
  // Максимальное количество патронов
  for i := A_BULLETS to A_HIGH do utils.writeInt(st, Word(FMaxAmmo[i]));
  // Наличие оружия
  for i := WP_FIRST to WP_LAST do utils.writeBool(st, FWeapon[i]);
  // Время перезарядки оружия
  for i := WP_FIRST to WP_LAST do utils.writeInt(st, Word(FReloading[i]));
  // Наличие рюкзака
  utils.writeBool(st, (R_ITEM_BACKPACK in FRulez));
  // Наличие красного ключа
  utils.writeBool(st, (R_KEY_RED in FRulez));
  // Наличие зеленого ключа
  utils.writeBool(st, (R_KEY_GREEN in FRulez));
  // Наличие синего ключа
  utils.writeBool(st, (R_KEY_BLUE in FRulez));
  // Наличие берсерка
  utils.writeBool(st, (R_BERSERK in FRulez));
  // Время действия специальных предметов
  for i := MR_SUIT to MR_MAX do utils.writeInt(st, LongWord(FMegaRulez[i]));
  // Время до повторного респауна, смены оружия, исользования, захвата флага
  for i := T_RESPAWN to T_FLAGCAP do utils.writeInt(st, LongWord(FTime[i]));
  // Название модели
  utils.writeStr(st, FModel.GetName());
  // Цвет модели
  utils.writeInt(st, Byte(FColor.R));
  utils.writeInt(st, Byte(FColor.G));
  utils.writeInt(st, Byte(FColor.B));
end;


procedure TPlayer.LoadState (st: TStream);
var
  i: Integer;
  str: String;
  b: Byte;
begin
  assert(st <> nil);

  // Сигнатура игрока
  if not utils.checkSign(st, 'PLYR') then raise XStreamError.Create('invalid player signature');
  if (utils.readByte(st) <> PLR_SAVE_VERSION) then raise XStreamError.Create('invalid player version');
  // Бот или человек:
  FIamBot := utils.readBool(st);
  // UID игрока
  FUID := utils.readWord(st);
  // Имя игрока
  str := utils.readStr(st);
  if (self <> gPlayer1) and (self <> gPlayer2) then FName := str;
  // Команда
  FTeam := utils.readByte(st);
  // Жив ли
  FAlive := utils.readBool(st);
  // Израсходовал ли все жизни
  FNoRespawn := utils.readBool(st);
  // Направление
  b := utils.readByte(st);
  if b = 1 then FDirection := TDirection.D_LEFT else FDirection := TDirection.D_RIGHT; // b = 2
  // Здоровье
  FHealth := utils.readLongInt(st);
  // Коэффициент инвалидности
  FHandicap := utils.readLongInt(st);
  // Жизни
  FLives := utils.readByte(st);
  // Броня
  FArmor := utils.readLongInt(st);
  // Запас воздуха
  FAir := utils.readLongInt(st);
  // Запас горючего
  FJetFuel := utils.readLongInt(st);
  // Боль
  FPain := utils.readLongInt(st);
  // Убил
  FKills := utils.readLongInt(st);
  // Убил монстров
  FMonsterKills := utils.readLongInt(st);
  // Фрагов
  FFrags := utils.readLongInt(st);
  // Фрагов подряд
  FFragCombo := utils.readByte(st);
  // Время последнего фрага
  FLastFrag := utils.readLongWord(st);
  // Смертей
  FDeath := utils.readLongInt(st);
  // Какой флаг несет
  FFlag := utils.readByte(st);
  // Нашел секретов
  FSecrets := utils.readLongInt(st);
  // Текущее оружие
  FCurrWeap := utils.readByte(st);
  // Желаемое оружие
  FNextWeap := utils.readWord(st);
  // ...и пауза
  FNextWeapDelay := utils.readByte(st);
  // Время зарядки BFG
  FBFGFireCounter := utils.readSmallInt(st);
  // Буфер урона
  FDamageBuffer := utils.readLongInt(st);
  // Последний ударивший
  FLastSpawnerUID := utils.readWord(st);
  // Тип последнего полученного урона
  FLastHit := utils.readByte(st);
  // Объект игрока
  Obj_LoadState(@FObj, st);
  // Текущее количество патронов
  for i := A_BULLETS to A_HIGH do FAmmo[i] := utils.readWord(st);
  // Максимальное количество патронов
  for i := A_BULLETS to A_HIGH do FMaxAmmo[i] := utils.readWord(st);
  // Наличие оружия
  for i := WP_FIRST to WP_LAST do FWeapon[i] := utils.readBool(st);
  // Время перезарядки оружия
  for i := WP_FIRST to WP_LAST do FReloading[i] := utils.readWord(st);
  // Наличие рюкзака
  if utils.readBool(st) then Include(FRulez, R_ITEM_BACKPACK);
  // Наличие красного ключа
  if utils.readBool(st) then Include(FRulez, R_KEY_RED);
  // Наличие зеленого ключа
  if utils.readBool(st) then Include(FRulez, R_KEY_GREEN);
  // Наличие синего ключа
  if utils.readBool(st) then Include(FRulez, R_KEY_BLUE);
  // Наличие берсерка
  if utils.readBool(st) then Include(FRulez, R_BERSERK);
  // Время действия специальных предметов
  for i := MR_SUIT to MR_MAX do FMegaRulez[i] := utils.readLongWord(st);
  // Время до повторного респауна, смены оружия, исользования, захвата флага
  for i := T_RESPAWN to T_FLAGCAP do FTime[i] := utils.readLongWord(st);
  // Название модели
  str := utils.readStr(st);
  // Цвет модели
  FColor.R := utils.readByte(st);
  FColor.G := utils.readByte(st);
  FColor.B := utils.readByte(st);
  if (self = gPlayer1) then
  begin
    str := gPlayer1Settings.Model;
    FColor := gPlayer1Settings.Color;
  end
  else if (self = gPlayer2) then
  begin
    str := gPlayer2Settings.Model;
    FColor := gPlayer2Settings.Color;
  end;
  // Обновляем модель игрока
  SetModel(str);
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
    FModel.Color := TEAMCOLOR[FTeam]
  else
    FModel.Color := FColor;
end;


procedure TPlayer.AllRulez(Health: Boolean);
var
  a: Integer;
begin
  if Health then
  begin
    FHealth := PLAYER_HP_LIMIT;
    FArmor := PLAYER_AP_LIMIT;
    Exit;
  end;

  for a := WP_FIRST to WP_LAST do FWeapon[a] := True;
  for a := A_BULLETS to A_HIGH do FAmmo[a] := 30000;
  FRulez := FRulez+[R_KEY_RED, R_KEY_GREEN, R_KEY_BLUE];
end;

procedure TPlayer.RestoreHealthArmor();
begin
  FHealth := PLAYER_HP_LIMIT;
  FArmor := PLAYER_AP_LIMIT;
end;

procedure TPlayer.FragCombo();
var
  Param: Integer;
begin
  if (gGameSettings.GameMode in [GM_COOP, GM_SINGLE]) or g_Game_IsClient then
    Exit;
  if gTime - FLastFrag < FRAG_COMBO_TIME then
  begin
    if FFragCombo < 5 then
      Inc(FFragCombo);
    Param := FUID or (FFragCombo shl 16);
    if (FComboEvnt >= Low(gDelayedEvents)) and
       (FComboEvnt <= High(gDelayedEvents)) and
       gDelayedEvents[FComboEvnt].Pending and
       (gDelayedEvents[FComboEvnt].DEType = DE_KILLCOMBO) and
       (gDelayedEvents[FComboEvnt].DENum and $FFFF = FUID) then
    begin
      gDelayedEvents[FComboEvnt].Time := gTime + 500;
      gDelayedEvents[FComboEvnt].DENum := Param;
    end
    else
      FComboEvnt := g_Game_DelayEvent(DE_KILLCOMBO, 500, Param);
  end
  else
    FFragCombo := 1;

  FLastFrag := gTime;
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
            resetWeaponQueue();
            FModel.SetWeapon(WEAPON_KASTET);
          end;
          if gFlash <> 0 then
            Inc(FPain, 100);
          FBerserk := gTime+30000;
        end;
        if FHealth < PLAYER_HP_SOFT then
        begin
          FHealth := PLAYER_HP_SOFT;
          FBerserk := gTime+30000;
        end;
      end;

    ITEM_INVUL:
      if FMegaRulez[MR_INVUL] < gTime+PLAYER_INVUL_TIME then
      begin
        FMegaRulez[MR_INVUL] := gTime+PLAYER_INVUL_TIME;
        FSpawnInvul := 0;
      end;

    ITEM_INVIS:
      if FMegaRulez[MR_INVIS] < gTime+PLAYER_INVIS_TIME then
      begin
        FMegaRulez[MR_INVIS] := gTime+PLAYER_INVIS_TIME;
      end;

    ITEM_JETPACK:
      if FJetFuel < JET_MAX then
      begin
        FJetFuel := JET_MAX;
      end;

    ITEM_MEDKIT_SMALL: if FHealth < PLAYER_HP_SOFT then IncMax(FHealth, 10, PLAYER_HP_SOFT);
    ITEM_MEDKIT_LARGE: if FHealth < PLAYER_HP_SOFT then IncMax(FHealth, 25, PLAYER_HP_SOFT);

    ITEM_ARMOR_GREEN: if FArmor < PLAYER_AP_SOFT then FArmor := PLAYER_AP_SOFT;
    ITEM_ARMOR_BLUE: if FArmor < PLAYER_AP_LIMIT then FArmor := PLAYER_AP_LIMIT;

    ITEM_SPHERE_BLUE: if FHealth < PLAYER_HP_LIMIT then IncMax(FHealth, 100, PLAYER_HP_LIMIT);
    ITEM_SPHERE_WHITE:
      if (FHealth < PLAYER_HP_LIMIT) or (FArmor < PLAYER_AP_LIMIT) then
      begin
        if FHealth < PLAYER_HP_LIMIT then FHealth := PLAYER_HP_LIMIT;
        if FArmor < PLAYER_AP_LIMIT then FArmor := PLAYER_AP_LIMIT;
      end;

    ITEM_WEAPON_SAW: FWeapon[WEAPON_SAW] := True;
    ITEM_WEAPON_SHOTGUN1: FWeapon[WEAPON_SHOTGUN1] := True;
    ITEM_WEAPON_SHOTGUN2: FWeapon[WEAPON_SHOTGUN2] := True;
    ITEM_WEAPON_CHAINGUN: FWeapon[WEAPON_CHAINGUN] := True;
    ITEM_WEAPON_ROCKETLAUNCHER: FWeapon[WEAPON_ROCKETLAUNCHER] := True;
    ITEM_WEAPON_PLASMA: FWeapon[WEAPON_PLASMA] := True;
    ITEM_WEAPON_BFG: FWeapon[WEAPON_BFG] := True;
    ITEM_WEAPON_SUPERPULEMET: FWeapon[WEAPON_SUPERPULEMET] := True;
    ITEM_WEAPON_FLAMETHROWER: FWeapon[WEAPON_FLAMETHROWER] := True;

    ITEM_AMMO_BULLETS: if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then IncMax(FAmmo[A_BULLETS], 10, FMaxAmmo[A_BULLETS]);
    ITEM_AMMO_BULLETS_BOX: if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then IncMax(FAmmo[A_BULLETS], 50, FMaxAmmo[A_BULLETS]);
    ITEM_AMMO_SHELLS: if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
    ITEM_AMMO_SHELLS_BOX: if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then IncMax(FAmmo[A_SHELLS], 25, FMaxAmmo[A_SHELLS]);
    ITEM_AMMO_ROCKET: if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then IncMax(FAmmo[A_ROCKETS], 1, FMaxAmmo[A_ROCKETS]);
    ITEM_AMMO_ROCKET_BOX: if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then IncMax(FAmmo[A_ROCKETS], 5, FMaxAmmo[A_ROCKETS]);
    ITEM_AMMO_CELL: if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);
    ITEM_AMMO_CELL_BIG: if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then IncMax(FAmmo[A_CELLS], 100, FMaxAmmo[A_CELLS]);
    ITEM_AMMO_FUELCAN: if FAmmo[A_FUEL] < FMaxAmmo[A_FUEL] then IncMax(FAmmo[A_FUEL], 100, FMaxAmmo[A_FUEL]);

    ITEM_AMMO_BACKPACK:
      if (FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS]) or
         (FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS]) or
         (FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS]) or
         (FAmmo[A_CELLS] < FMaxAmmo[A_CELLS]) or
         (FMaxAmmo[A_FUEL] < AmmoLimits[1, A_FUEL]) then
      begin
        FMaxAmmo[A_BULLETS] := AmmoLimits[1, A_BULLETS];
        FMaxAmmo[A_SHELLS] := AmmoLimits[1, A_SHELLS];
        FMaxAmmo[A_ROCKETS] := AmmoLimits[1, A_ROCKETS];
        FMaxAmmo[A_CELLS] := AmmoLimits[1, A_CELLS];
        FMaxAmmo[A_FUEL] := AmmoLimits[1, A_FUEL];

        if FAmmo[A_BULLETS] < FMaxAmmo[A_BULLETS] then IncMax(FAmmo[A_BULLETS], 10, FMaxAmmo[A_BULLETS]);
        if FAmmo[A_SHELLS] < FMaxAmmo[A_SHELLS] then IncMax(FAmmo[A_SHELLS], 4, FMaxAmmo[A_SHELLS]);
        if FAmmo[A_ROCKETS] < FMaxAmmo[A_ROCKETS] then IncMax(FAmmo[A_ROCKETS], 1, FMaxAmmo[A_ROCKETS]);
        if FAmmo[A_CELLS] < FMaxAmmo[A_CELLS] then IncMax(FAmmo[A_CELLS], 40, FMaxAmmo[A_CELLS]);

        FRulez := FRulez + [R_ITEM_BACKPACK];
      end;

    ITEM_KEY_RED: if not (R_KEY_RED in FRulez) then Include(FRulez, R_KEY_RED);
    ITEM_KEY_GREEN: if not (R_KEY_GREEN in FRulez) then Include(FRulez, R_KEY_GREEN);
    ITEM_KEY_BLUE: if not (R_KEY_BLUE in FRulez) then Include(FRulez, R_KEY_BLUE);

    ITEM_BOTTLE: if FHealth < PLAYER_HP_LIMIT then IncMax(FHealth, 4, PLAYER_HP_LIMIT);
    ITEM_HELMET: if FArmor < PLAYER_AP_LIMIT then IncMax(FArmor, 5, PLAYER_AP_LIMIT);

    else
      Exit;
  end;
  if g_Game_IsNet and g_Game_IsServer then
    MH_SEND_PlayerStats(FUID);
end;

procedure TPlayer.FlySmoke(Times: DWORD = 1);
  var i: DWORD;
begin
  if (Random(5) = 1) and (Times = 1) then
    Exit;

  if BodyInLiquid(0, 0) then
  begin
    {$IFDEF ENABLE_GFX}
      g_GFX_Bubbles(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)+Random(3)-1,
                    Obj.Y+Obj.Rect.Height+8, 1, 8, 4);
    {$ENDIF}
    if Random(2) = 0
      then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
      else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
    Exit;
  end;

  for i := 1 to Times do
  begin
    {$IFDEF ENABLE_GFX}
      g_GFX_QueueEffect(
        R_GFX_SMOKE_TRANS,
        Obj.X+Obj.Rect.X+Random(Obj.Rect.Width+Times*2)-(R_GFX_SMOKE_WIDTH div 2),
        Obj.Y+Obj.Rect.Height-4+Random(8+Times*2)
      );
    {$ENDIF}
  end;
end;

procedure TPlayer.OnFireFlame(Times: DWORD = 1);
  var i: DWORD;
begin
  if (Random(10) = 1) and (Times = 1) then
    Exit;

  for i := 1 to Times do
  begin
    {$IFDEF ENABLE_GFX}
      g_GFX_QueueEffect(
        R_GFX_FLAME,
        Obj.X+Obj.Rect.X+Random(Obj.Rect.Width+Times*2)-(R_GFX_FLAME_WIDTH div 2),
        Obj.Y+8+Random(8+Times*2)
      );
    {$ENDIF}
  end;
end;

procedure TPlayer.PauseSounds(Enable: Boolean);
begin
  FSawSound.Pause(Enable);
  FSawSoundIdle.Pause(Enable);
  FSawSoundHit.Pause(Enable);
  FSawSoundSelect.Pause(Enable);
  FFlameSoundOn.Pause(Enable);
  FFlameSoundOff.Pause(Enable);
  FFlameSoundWork.Pause(Enable);
  FJetSoundFly.Pause(Enable);
  FJetSoundOn.Pause(Enable);
  FJetSoundOff.Pause(Enable);
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

  Inc(gNumBots);

  for a := WP_FIRST to WP_LAST do
  begin
    FDifficult.WeaponPrior[a] := WEAPON_PRIOR1[a];
    FDifficult.CloseWeaponPrior[a] := WEAPON_PRIOR2[a];
    //FDifficult.SafeWeaponPrior[a] := WEAPON_PRIOR3[a];
  end;
end;

destructor TBot.Destroy();
begin
  Dec(gNumBots);
  inherited Destroy();
end;

procedure TBot.Respawn(Silent: Boolean; Force: Boolean = False);
begin
  inherited Respawn(Silent, Force);

  FAIFlags := nil;
  FSelectedWeapon := FCurrWeap;
  resetWeaponQueue();
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
  pla, tpla: TPlayer;
  vsPlayer, vsMonster, ok: Boolean;


  function monsUpdate (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if mon.alive and (mon.MonsterType <> MONSTER_BARREL) then
    begin
      if not TargetOnScreen(mon.Obj.X+mon.Obj.Rect.X, mon.Obj.Y+mon.Obj.Rect.Y) then exit;

      x2 := mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2);
      y2 := mon.Obj.Y+mon.Obj.Rect.Y+(mon.Obj.Rect.Height div 2);

      // Если монстр на экране и не прикрыт стеной
      if g_TraceVector(x1, y1, x2, y2) then
      begin
        // Добавляем к списку возможных целей
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
        WEAPON_SAW, WEAPON_KASTET, WEAPON_FLAMETHROWER: PressKey(KEY_FIRE, 40);
        else PressKey(KEY_FIRE);
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
          tpla := g_Player_Get(Target.UID);
          if tpla <> nil then
            with tpla do
              begin
                if (@FObj) <> nil then
                begin
                  Target.X := FObj.X;
                  Target.Y := FObj.Y;
                end;
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
            mon := g_Monsters_ByUID(Target.UID);
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
        if (gPlayers[a] <> nil) and (gPlayers[a].alive) and
           (gPlayers[a].FUID <> FUID) and
           (not SameTeam(FUID, gPlayers[a].FUID)) and
           (not gPlayers[a].NoTarget) and
           (gPlayers[a].FMegaRulez[MR_INVIS] < gTime) then
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
    if vsMonster then g_Mons_ForEach(monsUpdate);
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
            if ((RunDirection() = TDirection.D_LEFT) and (Target.X > FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = TDirection.D_RIGHT) and (Target.X < FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end
        else
          begin // Если побиты - убегаем
            if ((RunDirection() = TDirection.D_LEFT) and (Target.X < FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = TDirection.D_RIGHT) and (Target.X > FObj.X)) then
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
            if ((RunDirection() = TDirection.D_LEFT) and (Target.X > FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = TDirection.D_RIGHT) and (Target.X < FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end
        else
          begin // Если побиты - забываем о цели и убегаем
            Target.UID := 0;
            if ((RunDirection() = TDirection.D_LEFT) and (Target.X < FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = TDirection.D_RIGHT) and (Target.X > FObj.X)) then
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
            if ((RunDirection() = TDirection.D_LEFT) and (Target.X > FObj.X)) then
              SetAIFlag('GORIGHT', '1');
            if ((RunDirection() = TDirection.D_RIGHT) and (Target.X < FObj.X)) then
              SetAIFlag('GOLEFT', '1');
          end;
      end;

  // Выбираем угол вверх:
    if FDirection = TDirection.D_LEFT then
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
    if FDirection = TDirection.D_LEFT then
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
        if ((FDirection = TDirection.D_LEFT) and (Target.X < FObj.X)) or
            ((FDirection = TDirection.D_RIGHT) and (Target.X > FObj.X)) then
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
              if (pla = nil) or (not pla.alive) or pla.NoTarget or
                 (pla.FMegaRulez[MR_INVIS] >= gTime) then
                Target.UID := 0; // то забыть цель
            end
          else
            begin // Цель - монстр
              mon := g_Monsters_ByUID(Target.UID);
              if (mon = nil) or (not mon.alive) then
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

        if RunDirection() = TDirection.D_RIGHT then
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

          if RunDirection() = TDirection.D_LEFT then
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

  //HACK! (does it belongs there?)
  RealizeCurrentWeapon();

// Если есть возможные цели:
// (Стреляем по направлению к целям)
  if (targets <> nil) and (GetAIFlag('NEEDFIRE') <> '') then
    for a := 0 to High(targets) do
      begin
      // Если можем стрелять по диагонали:
        if GetRnd(FDifficult.DiagFire) then
          begin
          // Ищем цель сверху и стреляем, если есть:
            if FDirection = TDirection.D_LEFT then
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
            if FDirection = TDirection.D_LEFT then
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
            (((FDirection = TDirection.D_LEFT) and (targets[a].X < FObj.X)) or
            ((FDirection = TDirection.D_RIGHT) and (targets[a].X > FObj.X))) then
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
var
  EnableAI: Boolean;
begin
  if not FAlive then
  begin // Respawn
    ReleaseKeys();
    PressKey(KEY_UP);
  end
  else
  begin
    EnableAI := True;

    // Проверяем, отключён ли AI ботов
    if (g_debug_BotAIOff = 1) and (Team = TEAM_RED) then
      EnableAI := False;
    if (g_debug_BotAIOff = 2) and (Team = TEAM_BLUE) then
      EnableAI := False;
    if g_debug_BotAIOff = 3 then
      EnableAI := False;

    if EnableAI then
    begin
      UpdateMove();
      UpdateCombat();
    end
    else
    begin
      RealizeCurrentWeapon();
    end;
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

function TBot.GetAIFlag(aName: String20): String20;
var
  a: Integer;
begin
  Result := '';

  aName := LowerCase(aName);

  if FAIFlags <> nil then
    for a := 0 to High(FAIFlags) do
      if LowerCase(FAIFlags[a].Name) = aName then
      begin
        Result := FAIFlags[a].Value;
        Break;
      end;
end;

procedure TBot.RemoveAIFlag(aName: String20);
var
  a, b: Integer;
begin
  if FAIFlags = nil then Exit;

  aName := LowerCase(aName);

  for a := 0 to High(FAIFlags) do
    if LowerCase(FAIFlags[a].Name) = aName then
    begin
      if a <> High(FAIFlags) then
        for b := a to High(FAIFlags)-1 do
          FAIFlags[b] := FAIFlags[b+1];

      SetLength(FAIFlags, Length(FAIFlags)-1);
      Break;
    end;
end;

procedure TBot.SetAIFlag(aName, fValue: String20);
var
  a: Integer;
  ok: Boolean;
begin
  a := 0;
  ok := False;

  aName := LowerCase(aName);

  if FAIFlags <> nil then
    for a := 0 to High(FAIFlags) do
      if LowerCase(FAIFlags[a].Name) = aName then
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
      Name := aName;
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
    SetDirection(TDirection.D_LEFT);
  end;

  procedure GoRight(Time: Word = 1);
  begin
    ReleaseKey(KEY_LEFT);
    ReleaseKey(KEY_RIGHT);
    PressKey(KEY_RIGHT, Time);
    SetDirection(TDirection.D_RIGHT);
  end;

  function Rnd(a: Word): Boolean;
  begin
    Result := Random(a) = 0;
  end;

  procedure Turn(Time: Word = 1200);
  begin
    if RunDirection() = TDirection.D_LEFT then GoRight(Time) else GoLeft(Time);
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
    if RunDirection() = TDirection.D_LEFT then Result := CanRunLeft() else Result := CanRunRight();
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
    sx := IfThen(RunDirection() = TDirection.D_LEFT, -1, 1);
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
    sx := IfThen(RunDirection() = TDirection.D_LEFT, -1, 1);
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

    sx := IfThen(RunDirection() = TDirection.D_LEFT, -1, 1);
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
    Result := (FullInStep(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height) and
              not CollideLevel(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height)) or
              (FullInStep(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP) and
              not CollideLevel(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP));
  end;

  function BelowLiftUp(): Boolean;
  begin
    Result := ((FullInLift(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height) = -1) and
              not CollideLevel(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -PLAYER_RECT.Height)) or
              ((FullInLift(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP) = -1) and
              not CollideLevel(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*(PLAYER_RECT.Width div 2), -BOT_MAXJUMP));
  end;

  function OnTopLift(): Boolean;
  begin
    Result := (FullInLift(0, 0) = -1) and (FullInLift(0, -32) = 0);
  end;

  function CanJumpOver(): Boolean;
  var
    sx, y: Integer;
  begin
    sx := IfThen(RunDirection() = TDirection.D_LEFT, -1, 1);

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

  function IsSafeTrigger(): Boolean;
  var
    a: Integer;
  begin
    Result := True;
    if gTriggers = nil then
      Exit;
    for a := 0 to High(gTriggers) do
      if Collide(gTriggers[a].X,
                 gTriggers[a].Y,
                 gTriggers[a].Width,
                 gTriggers[a].Height) and
         (gTriggers[a].TriggerType in [TRIGGER_EXIT, TRIGGER_CLOSEDOOR,
                                       TRIGGER_CLOSETRAP, TRIGGER_TRAP,
                                       TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF,
                                       TRIGGER_ONOFF, TRIGGER_SPAWNMONSTER,
                                       TRIGGER_DAMAGE, TRIGGER_SHOT]) then
        Result := False;
  end;

begin
// Возможно, нажимаем кнопку:
  if Rnd(16) and IsSafeTrigger() then
    PressKey(KEY_OPEN);

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
      CanJumpUp(IfThen(RunDirection() = TDirection.D_LEFT, -1, 1)*32) and
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
        begin
          if IsSafeTrigger() then
            PressKey(KEY_OPEN);
        end else
          Turn();
    end;

// Осталось мало воздуха:
  if FAir < 36 * 2 then
    Jump(20);

// Выбираемся из кислоты, если нет костюма, обожглись, или мало здоровья:
  if (FMegaRulez[MR_SUIT] < gTime) and ((FLastHit = HIT_ACID) or (Healthy() <= 1)) then
    if BodyInAcid(0, 0) then
      Jump();
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
      WEAPON_FLAMETHROWER: Result := FAmmo[A_FUEL] >= 1;
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

function TBot.Heal(value: Word; Soft: Boolean): Boolean;
begin
  Result := inherited Heal(value, Soft);
end;

function TBot.Healthy(): Byte;
begin
  if FMegaRulez[MR_INVUL] >= gTime then Result := 3
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
          mon := g_Monsters_ByUID(FLastSpawnerUID);
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
    if Vel.X > 0 then Result := TDirection.D_RIGHT else Result := TDirection.D_LEFT;
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


procedure TDifficult.save (st: TStream);
begin
  utils.writeInt(st, Byte(DiagFire));
  utils.writeInt(st, Byte(InvisFire));
  utils.writeInt(st, Byte(DiagPrecision));
  utils.writeInt(st, Byte(FlyPrecision));
  utils.writeInt(st, Byte(Cover));
  utils.writeInt(st, Byte(CloseJump));
  st.WriteBuffer(WeaponPrior[Low(WeaponPrior)], sizeof(WeaponPrior));
  st.WriteBuffer(CloseWeaponPrior[Low(CloseWeaponPrior)], sizeof(CloseWeaponPrior));
end;

procedure TDifficult.load (st: TStream);
begin
  DiagFire := utils.readByte(st);
  InvisFire := utils.readByte(st);
  DiagPrecision := utils.readByte(st);
  FlyPrecision := utils.readByte(st);
  Cover := utils.readByte(st);
  CloseJump := utils.readByte(st);
  st.ReadBuffer(WeaponPrior[Low(WeaponPrior)], sizeof(WeaponPrior));
  st.ReadBuffer(CloseWeaponPrior[Low(CloseWeaponPrior)], sizeof(CloseWeaponPrior));
end;


procedure TBot.SaveState (st: TStream);
var
  i: Integer;
  dw: Integer;
begin
  inherited SaveState(st);
  utils.writeSign(st, 'BOT0');
  // Выбранное оружие
  utils.writeInt(st, Byte(FSelectedWeapon));
  // UID цели
  utils.writeInt(st, Word(FTargetUID));
  // Время потери цели
  utils.writeInt(st, LongWord(FLastVisible));
  // Количество флагов ИИ
  dw := Length(FAIFlags);
  utils.writeInt(st, LongInt(dw));
  // Флаги ИИ
  for i := 0 to dw-1 do
  begin
    utils.writeStr(st, FAIFlags[i].Name, 20);
    utils.writeStr(st, FAIFlags[i].Value, 20);
  end;
  // Настройки сложности
  FDifficult.save(st);
end;


procedure TBot.LoadState (st: TStream);
var
  i: Integer;
  dw: Integer;
begin
  inherited LoadState(st);
  if not utils.checkSign(st, 'BOT0') then raise XStreamError.Create('invalid bot signature');
  // Выбранное оружие
  FSelectedWeapon := utils.readByte(st);
  // UID цели
  FTargetUID := utils.readWord(st);
  // Время потери цели
  FLastVisible := utils.readLongWord(st);
  // Количество флагов ИИ
  dw := utils.readLongInt(st);
  if (dw < 0) or (dw > 16384) then raise XStreamError.Create('invalid number of bot AI flags');
  SetLength(FAIFlags, dw);
  // Флаги ИИ
  for i := 0 to dw-1 do
  begin
    FAIFlags[i].Name := utils.readStr(st, 20);
    FAIFlags[i].Value := utils.readStr(st, 20);
  end;
  // Настройки сложности
  FDifficult.load(st);
end;


begin
  conRegVar('player_indicator', @gPlayerIndicator, 'Draw indicator only for current player, also for teammates, or not at all', 'Draw indicator only for current player, also for teammates, or not at all');
  conRegVar('player_indicator_style', @gPlayerIndicatorStyle, 'Visual appearance of indicator', 'Visual appearance of indicator');
end.
