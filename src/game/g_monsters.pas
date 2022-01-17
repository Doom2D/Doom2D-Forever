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
{.$DEFINE D2F_DEBUG_MONS_MOVE}
unit g_monsters;

interface

uses
  SysUtils, Classes,
  mempool,
  MAPDEF,
  g_base, g_basic, g_phys, g_textures, g_grid,
  g_saveload, g_panel, xprofiler;

const
  MONSTATE_SLEEP  = 0;
  MONSTATE_GO     = 1;
  MONSTATE_RUN    = 2;
  MONSTATE_CLIMB  = 3;
  MONSTATE_DIE    = 4;
  MONSTATE_DEAD   = 5;
  MONSTATE_ATTACK = 6;
  MONSTATE_SHOOT  = 7;
  MONSTATE_PAIN   = 8;
  MONSTATE_WAIT   = 9;
  MONSTATE_REVIVE = 10;
  MONSTATE_RUNOUT = 11;

  MON_BURN_TIME   = 100;

{ in mapdef now
  BH_NORMAL   = 0;
  BH_KILLER   = 1;
  BH_MANIAC   = 2;
  BH_INSANE   = 3;
  BH_CANNIBAL = 4;
  BH_GOOD     = 5;
}

type
  ADirectedAnim = Array of Array [TDirection.D_LEFT..TDirection.D_RIGHT] of TAnimationState;

  PMonster = ^TMonster;
  TMonster = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    FMonsterType: Byte;
    FUID: Word;
    FDirection: TDirection;
    FStartDirection: TDirection;
    FStartX, FStartY: Integer;
    FRemoved: Boolean;
    FHealth: Integer;
    FMaxHealth: Integer;
    FState: Byte;
    FCurAnim: Byte;
    FAnim: ADirectedAnim;
    FTargetUID: Word;
    FTargetTime: Integer;
    FBehaviour: Byte;
    FAmmo: Integer;
    FPain: Integer;
    FSleep: Integer;
    FPainSound: Boolean;
    FPainTicks: Integer;
    FWaitAttackAnim: Boolean;
    FChainFire: Boolean;
    tx, ty: Integer;
    FStartID: Integer;
    FObj: TObj;
    FBloodRed: Byte;
    FBloodGreen: Byte;
    FBloodBlue: Byte;
    FBloodKind: Byte;
    FShellTimer: Integer;
    FShellType: Byte;
    FFirePainTime: Integer;
    FFireAttacker: Word;
    vilefire: TAnimationState;
    mProxyId: Integer; // node in dyntree or -1
    mArrIdx: Integer; // in gMonsters

    FDieTriggers: Array of Integer;
    FSpawnTrigger: Integer;

    mNeedSend: Boolean; // for network

    mEDamageType: Integer;

    procedure Turn();
    function findNewPrey(): Boolean;
    procedure ActivateTriggers();

    procedure setGameX (v: Integer); inline;
    procedure setGameY (v: Integer); inline;

    procedure doDamage (v: Integer);

  public
    FNoRespawn: Boolean;
    FFireTime: Integer;
    trapCheckFrameId: DWord; // for `g_weapons.CheckTrap()`
    mplatCheckFrameId: LongWord;

    constructor Create(MonsterType: Byte; aID: Integer; ForcedUID: Integer = -1);
    destructor Destroy(); override;
    function Collide(X, Y: Integer; Width, Height: Word): Boolean; overload;
    function Collide(Panel: TPanel): Boolean; overload;
    function Collide(X, Y: Integer): Boolean; overload;
    function TeleportTo(X, Y: Integer; silent: Boolean; dir: Byte): Boolean;
    function alive(): Boolean;
    procedure SetHealth(aH: Integer);
    procedure Push(vx, vy: Integer);
    function Damage(aDamage: Word; VelX, VelY: Integer; SpawnerUID: Word; t: Byte): Boolean;
    function Heal(Value: Word): Boolean;
    procedure BFGHit();
    procedure PreUpdate();
    procedure Update();
    procedure ClientUpdate();
    procedure ClientAttack(wx, wy, atx, aty: Integer);
    procedure SetDeadAnim;
    procedure WakeUp();
    procedure WakeUpSound();
    procedure DieSound();
    procedure PainSound();
    procedure ActionSound();
    procedure AddTrigger(t: Integer);
    procedure ClearTriggers();
    procedure Respawn();
    procedure SaveState (st: TStream);
    procedure LoadState (st: TStream);
    procedure SetState(State: Byte; ForceAnim: Byte = 255);
    procedure MakeBloodVector(Count: Word; VelX, VelY: Integer);
    procedure MakeBloodSimple(Count: Word);
    procedure RevertAnim(R: Boolean = True);
    function  AnimIsReverse: Boolean;
    function  shoot(o: PObj; immediately: Boolean): Boolean;
    function  kick(o: PObj): Boolean;
    procedure CatchFire(Attacker: Word; Timeout: Integer = MON_BURN_TIME);
    procedure OnFireFlame(Times: DWORD = 1);

    procedure positionChanged (); //WARNING! call this after monster position was changed, or coldet will not work right!

    procedure setPosition (ax, ay: Integer; callPosChanged: Boolean=true); inline;
    procedure moveBy (dx, dy: Integer); inline;

    procedure getMapBox (out x, y, w, h: Integer); inline;

    // get-and-clear
    function gncNeedSend (): Boolean; inline;
    procedure setDirty (); inline; // why `dirty`? 'cause i may introduce property `needSend` later

  public
    property Obj: TObj read FObj;

    property proxyId: Integer read mProxyId;
    property arrIdx: Integer read mArrIdx;

    property MonsterType: Byte read FMonsterType;
    property MonsterHealth: Integer read FHealth write FHealth;
    property MonsterAmmo: Integer read FAmmo write FAmmo;
    property MonsterTargetUID: Word read FTargetUID write FTargetUID;
    property MonsterTargetTime: Integer read FTargetTime write FTargetTime;
    property MonsterBehaviour: Byte read FBehaviour write FBehaviour;
    property MonsterSleep: Integer read FSleep write FSleep;
    property MonsterState: Byte read FState write FState;
    property MonsterRemoved: Boolean read FRemoved write FRemoved;
    property MonsterPain: Integer read FPain write FPain;
    property MonsterAnim: Byte read FCurAnim write FCurAnim;

    property UID: Word read FUID write FUID;
    property SpawnTrigger: Integer read FSpawnTrigger write FSpawnTrigger;

    property GameX: Integer read FObj.X write setGameX;
    property GameY: Integer read FObj.Y write setGameY;
    property GameVelX: Integer read FObj.Vel.X write FObj.Vel.X;
    property GameVelY: Integer read FObj.Vel.Y write FObj.Vel.Y;
    property GameAccelX: Integer read FObj.Accel.X write FObj.Accel.X;
    property GameAccelY: Integer read FObj.Accel.Y write FObj.Accel.Y;
    property GameDirection: TDirection read FDirection write FDirection;

    property StartID: Integer read FStartID;

    property VileFireAnim: TAnimationState read vilefire;
    property DirAnim: ADirectedAnim read FAnim;

  published
    property eMonsterType: Byte read FMonsterType;
    property eMonsterHealth: Integer read FHealth write FHealth;
    property eMonsterAmmo: Integer read FAmmo write FAmmo;
    property eMonsterTargetUID: Word read FTargetUID write FTargetUID;
    property eMonsterTargetTime: Integer read FTargetTime write FTargetTime;
    property eMonsterBehaviour: Byte read FBehaviour write FBehaviour;
    property eMonsterSleep: Integer read FSleep write FSleep;
    property eMonsterState: Byte read FState write FState;
    property eMonsterRemoved: Boolean read FRemoved;
    property eMonsterPain: Integer read FPain write FPain;
    property eMonsterAnim: Byte read FCurAnim;

    property eUID: Word read FUID;
    property eSpawnTrigger: Integer read FSpawnTrigger;

    property eGameX: Integer read FObj.X write setGameX;
    property eGameY: Integer read FObj.Y write setGameY;
    property eGameVelX: Integer read FObj.Vel.X write FObj.Vel.X;
    property eGameVelY: Integer read FObj.Vel.Y write FObj.Vel.Y;
    property eGameAccelX: Integer read FObj.Accel.X write FObj.Accel.X;
    property eGameAccelY: Integer read FObj.Accel.Y write FObj.Accel.Y;
    property eGameDirection: TDirection read FDirection write FDirection;

    property eStartID: Integer read FStartID;

    // set this before assigning something to `eDamage`
    property eDamageType: Integer read mEDamageType write mEDamageType;
    property eDamage: Integer write doDamage;
  end;


// will be called from map loader
procedure g_Mons_InitTree (x, y, w, h: Integer);

procedure g_Monsters_LoadData ();
procedure g_Monsters_FreeData ();
procedure g_Monsters_Init ();
procedure g_Monsters_Free (clearGrid: Boolean=true);
function g_Monsters_Create (MonsterType: Byte; X, Y: Integer; Direction: TDirection;
  AdjCoord: Boolean = False; ForcedUID: Integer = -1): TMonster;
procedure g_Monsters_PreUpdate ();
procedure g_Monsters_Update ();
function  g_Monsters_ByUID (UID: Word): TMonster;
procedure g_Monsters_killedp ();
procedure g_Monsters_SaveState (st: TStream);
procedure g_Monsters_LoadState (st: TStream);

function g_Mons_SpawnAt (monType: Integer; x, y: Integer; dir: TDirection=TDirection.D_LEFT): TMonster; overload;
function g_Mons_SpawnAt (const typeName: AnsiString; x, y: Integer; dir: TDirection=TDirection.D_LEFT): TMonster; overload;

function g_Mons_TypeLo (): Integer; inline;
function g_Mons_TypeHi (): Integer; inline;

function g_Mons_TypeIdByName (const name: AnsiString): Integer;
function g_Mons_NameByTypeId (monType: Integer): AnsiString;
function g_Mons_GetKilledByTypeId (monType: Integer): AnsiString;


type
  TEachMonsterCB = function (mon: TMonster): Boolean is nested; // return `true` to stop

// throws on invalid uid
function g_Mons_ByIdx (uid: Integer): TMonster; inline;

// can return null
function g_Mons_ByIdx_NC (uid: Integer): TMonster; inline;

function g_Mons_TotalCount (): Integer; inline;

function g_Mons_IsAnyAliveAt (x, y: Integer; width, height: Integer): Boolean;

function g_Mons_ForEach (cb: TEachMonsterCB): Boolean;
function g_Mons_ForEachAlive (cb: TEachMonsterCB): Boolean;

function g_Mons_ForEachAt (x, y: Integer; width, height: Integer; cb: TEachMonsterCB): Boolean;
function g_Mons_ForEachAliveAt (x, y: Integer; width, height: Integer; cb: TEachMonsterCB): Boolean;

function g_Mons_getNewTrapFrameId (): DWord; inline;
function g_Mons_getNewMPlatFrameId (): LongWord; inline;


{
type
  TMonsAlongLineCB = function (mon: TMonster; tag: Integer): Boolean is nested;

function g_Mons_AlongLine (x0, y0, x1, y1: Integer; cb: TMonsAlongLineCB; log: Boolean=false): TMonster;
}


var
  gmon_debug_use_sqaccel: Boolean = true;


//HACK!
procedure g_Mons_ProfilersBegin ();
procedure g_Mons_ProfilersEnd ();

procedure g_Mons_LOS_Start (); inline;
procedure g_Mons_LOS_End (); inline;

var
  profMonsLOS: TProfiler = nil; //WARNING: FOR DEBUGGING ONLY!


type
  TMonsterGrid = specialize TBodyGridBase<TMonster>;

var
  monsGrid: TMonsterGrid = nil; // DO NOT USE! public for debugging only!


var
  gmon_debug_think: Boolean = true;
  gmon_debug_one_think_step: Boolean = false;

  var (* private state *)
    gMonsters: array of TMonster;

  const
    ANIM_SLEEP   = 0;
    ANIM_GO      = 1;
    ANIM_DIE     = 2;
    ANIM_MESS    = 3;
    ANIM_ATTACK  = 4;
    ANIM_ATTACK2 = 5;
    ANIM_PAIN    = 6;

// Таблица характеристик монстров:
  MONSTERTABLE: Array [MONSTER_DEMON..MONSTER_MAN] of
                  record
                    Name: String;
                    Rect: TRectWH;
                    Health: Word;
                    RunVel: Byte;
                    MinPain: Byte;
                    Pain: Byte;
                    Jump: Byte;
                  end =
   ((Name:'DEMON'; Rect:(X:7; Y:8; Width:50; Height:52); Health:60;
     RunVel: 7; MinPain: 10; Pain: 20; Jump: 10),

    (Name:'IMP'; Rect:(X:15; Y:10; Width:34; Height:50); Health:25;
     RunVel: 3; MinPain: 0; Pain: 15; Jump: 10),

    (Name:'ZOMBY'; Rect:(X:15; Y:8; Width:34; Height:52); Health:15;
     RunVel: 3; MinPain: 0; Pain: 10; Jump: 10),

    (Name:'SERG'; Rect:(X:15; Y:8; Width:34; Height:52); Health:20;
     RunVel: 3; MinPain: 0; Pain: 10; Jump: 10),

    (Name:'CYBER'; Rect:(X:24; Y:9; Width:80; Height:110); Health:500;
     RunVel: 5; MinPain: 50; Pain: 70; Jump: 10),

    (Name:'CGUN'; Rect:(X:15; Y:4; Width:34; Height:56); Health:60;
     RunVel: 3; MinPain: 10; Pain: 20; Jump: 10),

    (Name:'BARON'; Rect:(X:39; Y:32; Width:50; Height:64); Health:150;
     RunVel: 3; MinPain: 30; Pain: 40; Jump: 10),

    (Name:'KNIGHT'; Rect:(X:39; Y:32; Width:50; Height:64); Health:75;
     RunVel: 3; MinPain: 30; Pain: 40; Jump: 10),

    (Name:'CACO'; Rect:(X:34; Y:36; Width:60; Height:56); Health:100;
     RunVel: 4; MinPain: 0; Pain: 10; Jump: 4),

    (Name:'SOUL'; Rect:(X:16; Y:14; Width:32; Height:36); Health:60;
     RunVel: 4; MinPain: 0; Pain: 10; Jump: 4),

    (Name:'PAIN'; Rect:(X:34; Y:36; Width:60; Height:56); Health:100;
     RunVel: 4; MinPain: 0; Pain: 10; Jump: 4),

    (Name:'SPIDER'; Rect:(X:23; Y:14; Width:210; Height:100); Health:500;
     RunVel: 4; MinPain: 50; Pain: 70; Jump: 10),

    (Name:'BSP'; Rect:(X:14; Y:17; Width:100; Height:42); Health:150;
     RunVel: 4; MinPain: 0; Pain: 20; Jump: 10),

    (Name:'MANCUB'; Rect:(X:28; Y:34; Width:72; Height:60); Health:200;
     RunVel: 3; MinPain: 20; Pain: 40; Jump: 7),

    (Name:'SKEL'; Rect:(X:30; Y:28; Width:68; Height:72); Health:200;
     RunVel: 6; MinPain: 20; Pain: 40; Jump: 11),

    (Name:'VILE'; Rect:(X:30; Y:28; Width:68; Height:72); Health:150;
     RunVel: 7; MinPain: 10; Pain: 30; Jump: 12),

    (Name:'FISH'; Rect:(X:6; Y:11; Width:20; Height:10); Health:35;
     RunVel: 14; MinPain: 10; Pain: 20; Jump: 6),

    (Name:'BARREL'; Rect:(X:20; Y:13; Width:24; Height:36); Health:20;
     RunVel: 0; MinPain: 0; Pain: 0; Jump: 0),

    (Name:'ROBO'; Rect:(X:30; Y:26; Width:68; Height:76); Health:20;
     RunVel: 3; MinPain: 20; Pain: 40; Jump: 6),

    (Name:'MAN'; Rect:(X:15; Y:6; Width:34; Height:52); Health:400;
     RunVel: 8; MinPain: 50; Pain: 70; Jump: 10));

// Таблица параметров анимации монстров:
  MONSTER_ANIMTABLE: Array [MONSTER_DEMON..MONSTER_MAN] of
     record
       LeftAnim: Boolean;
       wX, wY: Integer; // Откуда вылетит пуля
       AnimSpeed: Array [ANIM_SLEEP..ANIM_PAIN] of Byte;
       AnimCount: Array [ANIM_SLEEP..ANIM_PAIN] of Byte;
       AnimDeltaRight: Array [ANIM_SLEEP..ANIM_PAIN] of TDFPoint;
       AnimDeltaLeft: Array [ANIM_SLEEP..ANIM_PAIN] of TDFPoint;
     end =          // SLEEP           GO              DIE             MESS            ATTACK          ATTACK2         PAIN
   ((LeftAnim: False; wX: 54; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //DEMON
     AnimCount: (2, 4, 6, 6, 3, 0, 1);
     AnimDeltaRight: ((X:  1; Y:  4), (X:  1; Y:  4), (X:  0; Y:  4), (X:  0; Y:  4), (X:  2; Y:  6), (X:  2; Y:  6), (X:  2; Y:  5));
     AnimDeltaLeft:  ((X:  1; Y:  4), (X:  1; Y:  4), (X:  0; Y:  4), (X:  0; Y:  4), (X:  2; Y:  6), (X:  2; Y:  6), (X:  2; Y:  5))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //IMP
     AnimCount: (2, 4, 5, 8, 3, 0, 1);
     AnimDeltaRight: ((X:  8; Y: -4), (X:  8; Y: -4), (X: -2; Y: -1), (X:  3; Y: -2), (X: 14; Y: -4), (X: 14; Y: -4), (X: -5; Y: -4));
     AnimDeltaLeft:  ((X:  8; Y: -4), (X:  8; Y: -4), (X: -2; Y: -1), (X:  3; Y: -2), (X: 14; Y: -4), (X: 14; Y: -4), (X: -5; Y: -4))),

    (LeftAnim: True; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //ZOMBY
     AnimCount: (2, 4, 6, 9, 2, 0, 1);
     AnimDeltaRight: ((X:  1; Y: -4), (X:  1; Y: -4), (X:  3; Y: -1), (X:  2; Y: -1), (X:  2; Y: -4), (X:  2; Y: -4), (X:  1; Y: -4));
     AnimDeltaLeft:  ((X:  1; Y: -4), (X:  1; Y: -4), (X:  3; Y: -1), (X:  2; Y: -1), (X:  2; Y: -4), (X:  2; Y: -4), (X:  1; Y: -4))),

    (LeftAnim: True; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //SERG
     AnimCount: (2, 4, 5, 9, 2, 0, 1);
     AnimDeltaRight: ((X:  0; Y: -4), (X:  0; Y: -4), (X: -3; Y: -1), (X: -4; Y: -1), (X:  1; Y: -4), (X:  1; Y: -4), (X:  0; Y: -4));
     AnimDeltaLeft:  ((X:  0; Y: -4), (X:  0; Y: -4), (X: -3; Y: -1), (X: -4; Y: -1), (X:  1; Y: -4), (X:  1; Y: -4), (X:  0; Y: -4))),

    (LeftAnim: True; wX: 70; wY: 73; AnimSpeed:(3, 3, 3, 3, 3, 4, 3);  //CYBER
     AnimCount: (2, 4, 9, 9, 2, 2, 1);
     AnimDeltaRight: ((X:  2; Y: -6), (X:  2; Y: -6), (X: -3; Y: -4), (X: -3; Y: -4), (X: 25; Y: -6), (X: 0; Y: -6), (X: -2; Y: -6));
     AnimDeltaLeft:  ((X:  3; Y: -3), (X:  3; Y: -3), (X: -3; Y: -4), (X: -3; Y: -4), (X:-26; Y: -3), (X:-1; Y: -3), (X:  1; Y: -3))),

    (LeftAnim: True; wX: 32; wY: 32; AnimSpeed:(3, 2, 2, 2, 1, 0, 4);  //CGUN
     AnimCount: (2, 4, 7, 6, 2, 0, 1);
     AnimDeltaRight: ((X: -1; Y: -2), (X: -1; Y: -2), (X: -2; Y:  0), (X: -2; Y:  0), (X:  0; Y: -3), (X:  0; Y: -3), (X: -1; Y: -2));
     AnimDeltaLeft:  ((X: -1; Y: -2), (X: -1; Y: -2), (X: -2; Y:  0), (X: -2; Y:  0), (X: -1; Y: -4), (X: -1; Y: -4), (X:  2; Y: -4))),

    (LeftAnim: True; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4);  //BARON
     AnimCount: (2, 4, 7, 7, 3, 0, 1);
     AnimDeltaRight: ((X:  4; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X:  1; Y:  0), (X:  1; Y:  0), (X: -1; Y:  0));
     AnimDeltaLeft:  ((X:  0; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X: -2; Y:  0), (X: -2; Y:  0), (X:  1; Y:  0))),

    (LeftAnim: True; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4);  //KNIGHT
     AnimCount: (2, 4, 7, 7, 3, 0, 1);
     AnimDeltaRight: ((X:  4; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X:  1; Y:  0), (X:  1; Y:  0), (X: -1; Y:  0));
     AnimDeltaLeft:  ((X:  0; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X: -2; Y:  0), (X: -2; Y:  0), (X:  1; Y:  0))),

    (LeftAnim: False; wX: 88; wY: 69; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //CACO
     AnimCount: (1, 1, 7, 7, 6, 0, 1);
     AnimDeltaRight: ((X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -5), (X:  0; Y: -5), (X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -4));
     AnimDeltaLeft:  ((X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -5), (X:  0; Y: -5), (X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -4))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 4, 1, 0, 4); //SOUL
     AnimCount: (2, 2, 7, 7, 2, 0, 1); // 10
     AnimDeltaRight: ((X:  1; Y:-10), (X:  1; Y:-10), (X:-33; Y:-34), (X:-33; Y:-34), (X:-16; Y:-10), (X:-16; Y:-10), (X: -1; Y: -7));
     AnimDeltaLeft:  ((X:  1; Y:-10), (X:  1; Y:-10), (X:-33; Y:-34), (X:-33; Y:-34), (X:-16; Y:-10), (X:-16; Y:-10), (X: -1; Y: -7))),

    (LeftAnim: False; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //PAIN
     AnimCount: (4, 4, 7, 7, 4, 0, 1);
     AnimDeltaRight: ((X: -1; Y: -3), (X: -1; Y: -3), (X: -3; Y:  0), (X: -3; Y:  0), (X: -1; Y: -3), (X: -1; Y: -3), (X: -1; Y: -4));
     AnimDeltaLeft:  ((X: -1; Y: -3), (X: -1; Y: -3), (X: -3; Y:  0), (X: -3; Y:  0), (X: -1; Y: -3), (X: -1; Y: -3), (X: -1; Y: -4))),

    (LeftAnim: True; wX: 128; wY: 64; AnimSpeed:(3, 2, 4, 4, 1, 0, 4); //SPIDER
     AnimCount: (2, 6, 10, 10, 2, 0, 1);
     AnimDeltaRight: ((X: -4; Y: -4), (X: -4; Y: -4), (X: -2; Y:  8), (X: -2; Y:  8), (X: -3; Y: -3), (X: -3; Y: -3), (X: -3; Y: -4));
     AnimDeltaLeft:  ((X: -4; Y: -4), (X: -4; Y: -4), (X: -2; Y:  8), (X: -2; Y:  8), (X: -3; Y: -3), (X: -3; Y: -3), (X: 18; Y: -5))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 2, 3, 4, 1, 0, 4);  //BSP
     AnimCount: (2, 6, 7, 7, 2, 0, 1);
     AnimDeltaRight: ((X:  0; Y: -1), (X:  0; Y: -1), (X: -3; Y:  5), (X: -3; Y:  5), (X:  7; Y: -1), (X:  7; Y: -1), (X:  1; Y: -3));
     AnimDeltaLeft:  ((X:  0; Y: -1), (X:  0; Y: -1), (X: -3; Y:  5), (X: -3; Y:  5), (X:  7; Y: -1), (X:  7; Y: -1), (X:  6; Y: -3))),

    (LeftAnim: False; wX: 64; wY: 64; AnimSpeed:(3, 2, 2, 4, 2, 0, 4); //MANCUB
     AnimCount: (2, 6, 10, 10, 3, 0, 1);
     AnimDeltaRight: ((X: -2; Y: -7), (X: -2; Y: -7), (X: -4; Y: -2), (X: -4; Y: -2), (X: -4; Y: -7), (X: -4; Y: -7), (X:-14; Y: -7));
     AnimDeltaLeft:  ((X: -2; Y: -7), (X: -2; Y: -7), (X: -4; Y: -2), (X: -4; Y: -2), (X: -4; Y: -7), (X: -4; Y: -7), (X:-14; Y: -7))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 3, 3, 3, 3, 3, 3);  //SKEL
     AnimCount: (2, 6, 5, 5, 2, 2, 1);
     AnimDeltaRight: ((X: -1; Y:  4), (X: -1; Y:  4), (X: -2; Y:  4), (X: -2; Y:  4), (X: -1; Y:  4), (X:  6; Y:  2), (X:-24; Y:  4));
     AnimDeltaLeft:  ((X:  1; Y:  4), (X: -1; Y:  4), (X: -2; Y:  4), (X: -2; Y:  4), (X: -2; Y:  2), (X: -5; Y:  4), (X: 26; Y:  4))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 3, 3, 3, 3, 3, 3);  //VILE
     AnimCount: (2, 6, 9, 9, 10, 3, 1);
     AnimDeltaRight: ((X:  5; Y:-21), (X:  5; Y:-21), (X:  1; Y:-21), (X:  1; Y:-21), (X:  8; Y:-23), (X: -1; Y:-23), (X:  4; Y:-20));
     AnimDeltaLeft:  ((X: -8; Y:-21), (X:  5; Y:-21), (X:  1; Y:-21), (X:  1; Y:-21), (X:-10; Y:-24), (X:  3; Y:-23), (X: -4; Y:-22))),

    (LeftAnim: False; wX: 8; wY: 8; AnimSpeed:(2, 2, 2, 2, 3, 0, 1);   //FISH
     AnimCount: (2, 4, 1, 1, 2, 0, 3);
     AnimDeltaRight: ((X: -1; Y:  0), (X: -1; Y:  0), (X: -2; Y: -1), (X: -2; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1));
     AnimDeltaLeft:  ((X: -1; Y:  0), (X: -1; Y:  0), (X: -2; Y: -1), (X: -2; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1 ))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 0, 3, 0, 0, 0, 5); //BARREL
     AnimCount: (3, 0, 4, 0, 0, 0, 1);
     AnimDeltaRight: ((X:  0; Y:-15), (X:  0; Y:-15), (X: -1; Y:-15), (X: -1; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15));
     AnimDeltaLeft:  ((X:  0; Y:-15), (X:  0; Y:-15), (X: -1; Y:-15), (X: -1; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15))),

    (LeftAnim: False; wX: 95; wY: 57; AnimSpeed:(1, 2, 1, 0, 1, 1, 0); //ROBO
     AnimCount: (1, 12, 1, 0, 2, 4, 0);
     AnimDeltaRight: ((X: -2; Y:-26), (X: -2; Y:-26), (X:  0; Y:-26), (X:  0; Y:-26), (X:  2; Y:-26), (X: 15; Y:-26), (X: -2; Y:-26));
     AnimDeltaLeft:  ((X: -2; Y:-26), (X: -2; Y:-26), (X:  0; Y:-26), (X:  0; Y:-26), (X:  2; Y:-26), (X: 15; Y:-26), (X: -2; Y:-26))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 2, 2, 2, 0, 5); //MAN
     AnimCount: (2, 4, 7, 9, 2, 0, 1);
     AnimDeltaRight: ((X:  0; Y: -6), (X:  0; Y: -6), (X: -2; Y:  0), (X:  2; Y:  0), (X:  1; Y: -6), (X:  1; Y: -6), (X:  0; Y: -6));
     AnimDeltaLeft:  ((X:  0; Y: -6), (X:  0; Y: -6), (X: -2; Y:  0), (X:  2; Y:  0), (X:  1; Y: -6), (X:  1; Y: -6), (X:  0; Y: -6))) );

// Таблица типов анимации монстров:
  ANIMTABLE: Array [ANIM_SLEEP..ANIM_PAIN] of
               record
                 name: String;
                 loop: Boolean;
               end = ((name: 'SLEEP'; loop: True),
                      (name: 'GO'; loop: True),
                      (name: 'DIE'; loop: False),
                      (name: 'MESS'; loop: False),
                      (name: 'ATTACK'; loop: False),
                      (name: 'ATTACK2'; loop: False),
                      (name: 'PAIN'; loop: False));
implementation

uses
  e_log, g_sound, g_gfx, g_player, g_game, r_gfx,
  g_weapons, g_triggers, g_items, g_options,
  g_console, g_map, Math, g_menu, wadreader,
  g_language, g_netmsg, idpool, utils, xstreams;


function g_Look(a, b: PObj; d: TDirection): Boolean;
begin
  if not gmon_dbg_los_enabled then begin result := false; exit; end; // always "wall hit"

  if ((b^.X > a^.X) and (d = TDirection.D_LEFT)) or
     ((b^.X < a^.X) and (d = TDirection.D_RIGHT)) then
  begin
    Result := False;
    Exit;
  end;

  Result := g_TraceVector(a^.X+a^.Rect.X+(a^.Rect.Width div 2),
                          a^.Y+a^.Rect.Y+(a^.Rect.Height div 2),
                          b^.X+b^.Rect.X+(b^.Rect.Width div 2),
                          b^.Y+b^.Rect.Y+(b^.Rect.Height div 2));
end;

// ////////////////////////////////////////////////////////////////////////// //
procedure g_Mons_ProfilersBegin ();
begin
  if (profMonsLOS = nil) then profMonsLOS := TProfiler.Create('LOS CALC', g_profile_history_size);
  if (profMonsLOS <> nil) then profMonsLOS.mainBegin(g_profile_los);
  if g_profile_los and (profMonsLOS <> nil) then
  begin
    profMonsLOS.sectionBegin('loscalc');
    profMonsLOS.sectionEnd();
  end;
end;

procedure g_Mons_ProfilersEnd ();
begin
  if (profMonsLOS <> nil) and (g_profile_los) then profMonsLOS.mainEnd();
end;

procedure g_Mons_LOS_Start (); inline;
begin
  if (profMonsLOS <> nil) then profMonsLOS.sectionBeginAccum('loscalc');
end;

procedure g_Mons_LOS_End (); inline;
begin
  if (profMonsLOS <> nil) then profMonsLOS.sectionEnd();
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  monCheckTrapLastFrameId: DWord = 0;
  monCheckMPlatLastFrameId: LongWord = 0;


procedure TMonster.getMapBox (out x, y, w, h: Integer); inline;
begin
  x := FObj.X+FObj.Rect.X;
  y := FObj.Y+FObj.Rect.Y;
  w := FObj.Rect.Width;
  h := FObj.Rect.Height;
end;

function TMonster.gncNeedSend (): Boolean; inline; begin result := mNeedSend; mNeedSend := false; end;

procedure TMonster.setDirty (); inline; begin mNeedSend := true; end;


// ////////////////////////////////////////////////////////////////////////// //
{
function g_Mons_AlongLine (x0, y0, x1, y1: Integer; cb: TMonsAlongLineCB; log: Boolean=false): TMonster;
begin
  if not assigned(cb) then begin result := nil; exit; end;
  result := monsGrid.forEachAlongLine(x0, y0, x1, y1, cb, -1, log);
end;
}


//WARNING! call this after monster position was changed, or coldet will not work right!
procedure TMonster.positionChanged ();
var
  x, y, w, h: Integer;
  nx, ny, nw, nh: Integer;
begin
  {$IF DEFINED(D2F_DEBUG_MONS_MOVE)}
  //e_WriteLog(Format('monster #%d(%u): pos=(%d,%d); rpos=(%d,%d)', [mArrIdx, UID, FObj.X, FObj.Y, FObj.Rect.X, FObj.Rect.Y]), MSG_NOTIFY);
  {$ENDIF}
  if (mProxyId = -1) then
  begin
    //mNeedSend := true;
    mProxyId := monsGrid.insertBody(self, FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width, FObj.Rect.Height);
    {$IF DEFINED(D2F_DEBUG_MONS_MOVE)}
    monsGrid.getBodyXY(mProxyId, x, y);
    e_WriteLog(Format('monster #%d:(%u): inserted into the grid; mProxyid=%d; gx=%d; gy=%d', [mArrIdx, UID, mProxyId, x-monsGrid.gridX0, y-monsGrid.gridY0]), MSG_NOTIFY);
    {$ENDIF}
  end
  else
  begin
    monsGrid.getBodyDims(mProxyId, x, y, w, h);
    getMapBox(nx, ny, nw, nh);

    if (w <> nw) or (h <> nh) then
    begin
      //mNeedSend := true;
      {$IF DEFINED(D2F_DEBUG_MONS_MOVE)}
      e_WriteLog(Format('monster #%d:(%u): resized; mProxyid=%d; gx=%d; gy=%d', [mArrIdx, UID, mProxyId, x-monsGrid.gridX0, y-monsGrid.gridY0]), MSG_NOTIFY);
      {$ENDIF}
      monsGrid.moveResizeBody(mProxyId, nx, ny, nw, nh);
    end
    else if (x <> nx) or (y <> ny) then
    begin
      //mNeedSend := true;
      {$IF DEFINED(D2F_DEBUG_MONS_MOVE)}
      e_WriteLog(Format('monster #%d:(%u): updating grid; mProxyid=%d; gx=%d; gy=%d', [mArrIdx, UID, mProxyId, x-monsGrid.gridX0, y-monsGrid.gridY0]), MSG_NOTIFY);
      {$ENDIF}
      monsGrid.moveBody(mProxyId, nx, ny);
    end
    else
    begin
      exit; // nothing to do
    end;
    {$IF DEFINED(D2F_DEBUG_MONS_MOVE)}
    monsGrid.getBodyXY(mProxyId, x, y);
    e_WriteLog(Format('monster #%d:(%u): updated grid; mProxyid=%d; gx=%d; gy=%d', [mArrIdx, UID, mProxyId, x-monsGrid.gridX0, y-monsGrid.gridY0]), MSG_NOTIFY);
    {$ENDIF}
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
const
  MONSTER_SIGNATURE = $534E4F4D; // 'MONS'

  MAX_ATM = 89; // Время ожидания после потери цели
  MAX_SOUL = 512; // Ограничение Lost_Soul'ов


// ////////////////////////////////////////////////////////////////////////// //
var
  uidMap: array [0..65535] of TMonster; // monster knows it's index
  freeInds: TIdPool = nil;


procedure clearUidMap ();
var
  idx: Integer;
begin
  for idx := 0 to High(uidMap) do uidMap[idx] := nil;
  freeInds.clear();
end;


function g_Mons_getNewTrapFrameId (): DWord; inline;
var
  f: Integer;
begin
  Inc(monCheckTrapLastFrameId);
  if (monCheckTrapLastFrameId = 0) then
  begin
    // wraparound
    monCheckTrapLastFrameId := 1;
    for f := 0 to High(gMonsters) do
    begin
      if (gMonsters[f] <> nil) then gMonsters[f].trapCheckFrameId := 0;
    end;
  end;
  result := monCheckTrapLastFrameId;
end;


function g_Mons_getNewMPlatFrameId (): LongWord; inline;
var
  f: Integer;
begin
  Inc(monCheckMPlatLastFrameId);
  if (monCheckMPlatLastFrameId = 0) then
  begin
    // wraparound
    monCheckMPlatLastFrameId := 1;
    for f := 0 to High(gMonsters) do
    begin
      if (gMonsters[f] <> nil) then gMonsters[f].mplatCheckFrameId := 0;
    end;
  end;
  result := monCheckMPlatLastFrameId;
end;


var
  pt_x: Integer = 0;
  pt_xs: Integer = 1;
  pt_y: Integer = 0;
  pt_ys: Integer = 1;
  soulcount: Integer = 0;


function allocMonster (): DWORD;
var
  f, olen: Integer;
begin
  result := freeInds.alloc();
  if (result > High(gMonsters)) then
  begin
    olen := Length(gMonsters);
    SetLength(gMonsters, result+64);
    for f := olen to High(gMonsters) do gMonsters[f] := nil;
  end;
end;


function IsFriend(a, b: Byte): Boolean;
begin
  Result := True;

// Бочка - всем друг:
  if (a = MONSTER_BARREL) or (b = MONSTER_BARREL) then
    Exit;

// Монстры одного вида:
  if a = b then
    case a of
      MONSTER_IMP, MONSTER_DEMON, MONSTER_BARON, MONSTER_KNIGHT, MONSTER_CACO,
      MONSTER_SOUL, MONSTER_PAIN, MONSTER_MANCUB, MONSTER_SKEL, MONSTER_FISH:
        Exit; // Эти не бьют своих
    end;

// Lost_Soul не может ранить Pain_Elemental'а:
  if (a = MONSTER_SOUL) and (b = MONSTER_PAIN) then
    Exit;
// Pain_Elemental не может ранить Lost_Soul'а:
  if (b = MONSTER_SOUL) and (a = MONSTER_PAIN) then
    Exit;

// В остальных случаях - будут бить друг друга:
  Result := False;
end;


function BehaviourDamage(SpawnerUID: Word; BH, SelfType: Byte): Boolean;
var
  m: TMonster;
  UIDType, MonsterType: Byte;
begin
  Result := False;
  MonsterType := 0;

  UIDType := g_GetUIDType(SpawnerUID);
  if UIDType = UID_MONSTER then
  begin
    m := g_Monsters_ByUID(SpawnerUID);
    if m = nil then Exit;
    MonsterType := m.FMonsterType;
  end;

  case BH of
    BH_NORMAL: Result := (UIDType = UID_PLAYER) or
      ((UIDType = UID_MONSTER) and (not IsFriend(MonsterType, SelfType)));

    BH_KILLER: Result := UIDType = UID_PLAYER;
    BH_MANIAC: Result := (UIDType = UID_PLAYER) or
      ((UIDType = UID_MONSTER) and (not IsFriend(MonsterType, SelfType)));

    BH_INSANE: Result := (UIDType = UID_MONSTER) and (not IsFriend(MonsterType, SelfType));
    BH_CANNIBAL: Result := (UIDType = UID_MONSTER) and (MonsterType = SelfType);
  end;
end;


function canShoot(m: Byte): Boolean;
begin
  Result := False;

  case m of
    MONSTER_DEMON, MONSTER_FISH, MONSTER_BARREL:
      Exit;
    else
      Result := True;
  end;
end;


function isCorpse (o: PObj; immediately: Boolean): Integer;

  (*
  function monsCollCheck (mon: TMonster; atag: Integer): Boolean;
  begin
    atag := atag; // shut up, fpc!
    result := false; // don't stop
    if (mon.FState = MONSTATE_DEAD) and g_Obj_Collide(o, @mon.FObj) then
    begin
      case mon.FMonsterType of // Не воскресить:
        MONSTER_SOUL, MONSTER_PAIN, MONSTER_CYBER, MONSTER_SPIDER,
        MONSTER_VILE, MONSTER_BARREL, MONSTER_ROBO: exit;
      end;
      // Остальных можно воскресить
      result := true;
    end;
  end;
  *)

var
  a: Integer;
  mon: PMonster;
  mres: TMonster = nil;
  it: TMonsterGrid.Iter;
begin
  result := -1;

  // Если нужна вероятность
  if not immediately and (Random(8) <> 0) then exit;

  // Ищем мертвых монстров поблизости
  if gmon_debug_use_sqaccel then
  begin
    //mon := monsGrid.forEachInAABB(o.X+o.Rect.X, o.Y+o.Rect.Y, o.Rect.Width, o.Rect.Height, monsCollCheck);
    //if (mon <> nil) then result := mon.mArrIdx;
    it := monsGrid.forEachInAABB(o.X+o.Rect.X, o.Y+o.Rect.Y, o.Rect.Width, o.Rect.Height);
    for mon in it do
    begin
      case mon.FMonsterType of // Не воскресить:
        MONSTER_SOUL, MONSTER_PAIN, MONSTER_CYBER, MONSTER_SPIDER,
        MONSTER_VILE, MONSTER_BARREL, MONSTER_ROBO: begin end;
        // Остальных можно воскресить
        else mres := mon^;
      end;
      if (mres <> nil) then break;
    end;
    it.release();
    if (mres <> nil) then result := mres.mArrIdx;
  end
  else
  begin
    for a := 0 to High(gMonsters) do
    begin
      if (gMonsters[a] <> nil) and (gMonsters[a].FState = MONSTATE_DEAD) and g_Obj_Collide(o, @gMonsters[a].FObj) then
      begin
        case gMonsters[a].FMonsterType of // Не воскресить:
          MONSTER_SOUL, MONSTER_PAIN, MONSTER_CYBER, MONSTER_SPIDER,
          MONSTER_VILE, MONSTER_BARREL, MONSTER_ROBO: Continue;
          else // Остальных можно воскресить
            begin
              Result := a;
              Exit;
            end;
        end;
      end;
    end;
  end;
end;

procedure g_Monsters_LoadData();
begin
  e_WriteLog('Loading monsters data...', TMsgType.Notify);

  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES], 133, False);
  g_Game_StepLoading(133);

  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_SOUNDS], 0, False);

  g_Sound_CreateWADEx('SOUND_MONSTER_BARREL_DIE', GameWAD+':MSOUNDS\BARREL_DIE');

  g_Sound_CreateWADEx('SOUND_MONSTER_PAIN', GameWAD+':MSOUNDS\PAIN');
  g_Sound_CreateWADEx('SOUND_MONSTER_PAIN2', GameWAD+':MSOUNDS\PAIN2');
  g_Sound_CreateWADEx('SOUND_MONSTER_ACTION', GameWAD+':MSOUNDS\ACTION');
  g_Sound_CreateWADEx('SOUND_MONSTER_ACTION2', GameWAD+':MSOUNDS\ACTION2');
  g_Sound_CreateWADEx('SOUND_MONSTER_ALERT_1', GameWAD+':MSOUNDS\ALERT_1');
  g_Sound_CreateWADEx('SOUND_MONSTER_ALERT_2', GameWAD+':MSOUNDS\ALERT_2');
  g_Sound_CreateWADEx('SOUND_MONSTER_ALERT_3', GameWAD+':MSOUNDS\ALERT_3');
  g_Sound_CreateWADEx('SOUND_MONSTER_DIE_1', GameWAD+':MSOUNDS\DIE_1');
  g_Sound_CreateWADEx('SOUND_MONSTER_DIE_2', GameWAD+':MSOUNDS\DIE_2');
  g_Sound_CreateWADEx('SOUND_MONSTER_DIE_3', GameWAD+':MSOUNDS\DIE_3');
  g_Sound_CreateWADEx('SOUND_MONSTER_SLOP', GameWAD+':MSOUNDS\SLOP');

  g_Sound_CreateWADEx('SOUND_MONSTER_DEMON_ATTACK', GameWAD+':MSOUNDS\DEMON_ATTACK');
  g_Sound_CreateWADEx('SOUND_MONSTER_DEMON_ALERT', GameWAD+':MSOUNDS\DEMON_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_DEMON_DIE', GameWAD+':MSOUNDS\DEMON_DIE');

  g_Sound_CreateWADEx('SOUND_MONSTER_IMP_ALERT_1', GameWAD+':MSOUNDS\IMP_ALERT_1');
  g_Sound_CreateWADEx('SOUND_MONSTER_IMP_ALERT_2', GameWAD+':MSOUNDS\IMP_ALERT_2');
  g_Sound_CreateWADEx('SOUND_MONSTER_IMP_DIE_1', GameWAD+':MSOUNDS\IMP_DIE_1');
  g_Sound_CreateWADEx('SOUND_MONSTER_IMP_DIE_2', GameWAD+':MSOUNDS\IMP_DIE_2');
  g_Sound_CreateWADEx('SOUND_MONSTER_IMP_ACTION', GameWAD+':MSOUNDS\IMP_ACTION');
  g_Sound_CreateWADEx('SOUND_MONSTER_IMP_ATTACK', GameWAD+':MSOUNDS\IMP_ATTACK');

  g_Sound_CreateWADEx('SOUND_MONSTER_MAN_PAIN', GameWAD+':MSOUNDS\MAN_PAIN');
  g_Sound_CreateWADEx('SOUND_MONSTER_MAN_ALERT', GameWAD+':MSOUNDS\MAN_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_MAN_DIE', GameWAD+':MSOUNDS\MAN_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_HAHA', GameWAD+':MSOUNDS\HAHA');
  g_Sound_CreateWADEx('SOUND_MONSTER_TRUP', GameWAD+':MSOUNDS\TRUP');

  g_Sound_CreateWADEx('SOUND_MONSTER_SOUL_ATTACK', GameWAD+':MSOUNDS\SOUL_ATTACK');
  g_Sound_CreateWADEx('SOUND_MONSTER_SOUL_DIE', GameWAD+':MSOUNDS\SOUL_DIE');

  g_Sound_CreateWADEx('SOUND_MONSTER_BSP_ACTION', GameWAD+':MSOUNDS\BSP_ACTION');
  g_Sound_CreateWADEx('SOUND_MONSTER_BSP_DIE', GameWAD+':MSOUNDS\BSP_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_BSP_ALERT', GameWAD+':MSOUNDS\BSP_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_BSP_WALK', GameWAD+':MSOUNDS\BSP_WALK');

  g_Sound_CreateWADEx('SOUND_MONSTER_VILE_ACTION', GameWAD+':MSOUNDS\VILE_ACTION');
  g_Sound_CreateWADEx('SOUND_MONSTER_VILE_PAIN', GameWAD+':MSOUNDS\VILE_PAIN');
  g_Sound_CreateWADEx('SOUND_MONSTER_VILE_DIE', GameWAD+':MSOUNDS\VILE_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_VILE_ALERT', GameWAD+':MSOUNDS\VILE_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_VILE_ATTACK', GameWAD+':MSOUNDS\VILE_ATTACK');

  g_Sound_CreateWADEx('SOUND_MONSTER_SKEL_ACTION', GameWAD+':MSOUNDS\SKEL_ACTION');
  g_Sound_CreateWADEx('SOUND_MONSTER_SKEL_DIE', GameWAD+':MSOUNDS\SKEL_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_SKEL_ALERT', GameWAD+':MSOUNDS\SKEL_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_SKEL_ATTACK', GameWAD+':MSOUNDS\SKEL_ATTACK');
  g_Sound_CreateWADEx('SOUND_MONSTER_SKEL_HIT', GameWAD+':MSOUNDS\SKEL_HIT');

  g_Sound_CreateWADEx('SOUND_MONSTER_MANCUB_PAIN', GameWAD+':MSOUNDS\MANCUB_PAIN');
  g_Sound_CreateWADEx('SOUND_MONSTER_MANCUB_DIE', GameWAD+':MSOUNDS\MANCUB_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_MANCUB_ALERT', GameWAD+':MSOUNDS\MANCUB_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_MANCUB_ATTACK', GameWAD+':MSOUNDS\MANCUB_ATTACK');

  g_Sound_CreateWADEx('SOUND_MONSTER_PAIN_PAIN', GameWAD+':MSOUNDS\PAIN_PAIN');
  g_Sound_CreateWADEx('SOUND_MONSTER_PAIN_DIE', GameWAD+':MSOUNDS\PAIN_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_PAIN_ALERT', GameWAD+':MSOUNDS\PAIN_ALERT');

  g_Sound_CreateWADEx('SOUND_MONSTER_BARON_DIE', GameWAD+':MSOUNDS\BARON_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_BARON_ALERT', GameWAD+':MSOUNDS\BARON_ALERT');

  g_Sound_CreateWADEx('SOUND_MONSTER_CACO_DIE', GameWAD+':MSOUNDS\CACO_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_CACO_ALERT', GameWAD+':MSOUNDS\CACO_ALERT');

  g_Sound_CreateWADEx('SOUND_MONSTER_CYBER_DIE', GameWAD+':MSOUNDS\CYBER_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_CYBER_ALERT', GameWAD+':MSOUNDS\CYBER_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_CYBER_WALK', GameWAD+':MSOUNDS\CYBER_WALK');

  g_Sound_CreateWADEx('SOUND_MONSTER_KNIGHT_DIE', GameWAD+':MSOUNDS\KNIGHT_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_KNIGHT_ALERT', GameWAD+':MSOUNDS\KNIGHT_ALERT');

  g_Sound_CreateWADEx('SOUND_MONSTER_SPIDER_DIE', GameWAD+':MSOUNDS\SPIDER_DIE');
  g_Sound_CreateWADEx('SOUND_MONSTER_SPIDER_ALERT', GameWAD+':MSOUNDS\SPIDER_ALERT');
  g_Sound_CreateWADEx('SOUND_MONSTER_SPIDER_WALK', GameWAD+':MSOUNDS\SPIDER_WALK');

  g_Sound_CreateWADEx('SOUND_MONSTER_FISH_ATTACK', GameWAD+':MSOUNDS\FISH_ATTACK');

  freeInds := TIdPool.Create();
  clearUidMap();
  monCheckTrapLastFrameId := 0;
  monCheckMPlatLastFrameId := 0;
end;

procedure g_Monsters_FreeData();
begin
  e_WriteLog('Releasing monsters data...', TMsgType.Notify);

  g_Sound_Delete('SOUND_MONSTER_BARREL_DIE');

  g_Sound_Delete('SOUND_MONSTER_PAIN');
  g_Sound_Delete('SOUND_MONSTER_PAIN2');
  g_Sound_Delete('SOUND_MONSTER_ACTION');
  g_Sound_Delete('SOUND_MONSTER_ACTION2');
  g_Sound_Delete('SOUND_MONSTER_ALERT_1');
  g_Sound_Delete('SOUND_MONSTER_ALERT_2');
  g_Sound_Delete('SOUND_MONSTER_ALERT_3');
  g_Sound_Delete('SOUND_MONSTER_DIE_1');
  g_Sound_Delete('SOUND_MONSTER_DIE_2');
  g_Sound_Delete('SOUND_MONSTER_DIE_3');
  g_Sound_Delete('SOUND_MONSTER_SLOP');

  g_Sound_Delete('SOUND_MONSTER_DEMON_ATTACK');
  g_Sound_Delete('SOUND_MONSTER_DEMON_ALERT');
  g_Sound_Delete('SOUND_MONSTER_DEMON_DIE');

  g_Sound_Delete('SOUND_MONSTER_IMP_ALERT_1');
  g_Sound_Delete('SOUND_MONSTER_IMP_ALERT_2');
  g_Sound_Delete('SOUND_MONSTER_IMP_DIE_1');
  g_Sound_Delete('SOUND_MONSTER_IMP_DIE_2');
  g_Sound_Delete('SOUND_MONSTER_IMP_ACTION');
  g_Sound_Delete('SOUND_MONSTER_IMP_ATTACK');

  g_Sound_Delete('SOUND_MONSTER_MAN_PAIN');
  g_Sound_Delete('SOUND_MONSTER_MAN_ALERT');
  g_Sound_Delete('SOUND_MONSTER_MAN_DIE');
  g_Sound_Delete('SOUND_MONSTER_HAHA');
  g_Sound_Delete('SOUND_MONSTER_TRUP');

  g_Sound_Delete('SOUND_MONSTER_SOUL_ATTACK');
  g_Sound_Delete('SOUND_MONSTER_SOUL_DIE');

  g_Sound_Delete('SOUND_MONSTER_BSP_ACTION');
  g_Sound_Delete('SOUND_MONSTER_BSP_DIE');
  g_Sound_Delete('SOUND_MONSTER_BSP_ALERT');
  g_Sound_Delete('SOUND_MONSTER_BSP_WALK');

  g_Sound_Delete('SOUND_MONSTER_VILE_ACTION');
  g_Sound_Delete('SOUND_MONSTER_VILE_PAIN');
  g_Sound_Delete('SOUND_MONSTER_VILE_DIE');
  g_Sound_Delete('SOUND_MONSTER_VILE_ALERT');
  g_Sound_Delete('SOUND_MONSTER_VILE_ATTACK');

  g_Sound_Delete('SOUND_MONSTER_SKEL_ACTION');
  g_Sound_Delete('SOUND_MONSTER_SKEL_DIE');
  g_Sound_Delete('SOUND_MONSTER_SKEL_ALERT');
  g_Sound_Delete('SOUND_MONSTER_SKEL_ATTACK');
  g_Sound_Delete('SOUND_MONSTER_SKEL_HIT');

  g_Sound_Delete('SOUND_MONSTER_MANCUB_PAIN');
  g_Sound_Delete('SOUND_MONSTER_MANCUB_DIE');
  g_Sound_Delete('SOUND_MONSTER_MANCUB_ALERT');
  g_Sound_Delete('SOUND_MONSTER_MANCUB_ATTACK');

  g_Sound_Delete('SOUND_MONSTER_PAIN_PAIN');
  g_Sound_Delete('SOUND_MONSTER_PAIN_DIE');
  g_Sound_Delete('SOUND_MONSTER_PAIN_ALERT');

  g_Sound_Delete('SOUND_MONSTER_BARON_DIE');
  g_Sound_Delete('SOUND_MONSTER_BARON_ALERT');

  g_Sound_Delete('SOUND_MONSTER_CACO_DIE');
  g_Sound_Delete('SOUND_MONSTER_CACO_ALERT');

  g_Sound_Delete('SOUND_MONSTER_CYBER_DIE');
  g_Sound_Delete('SOUND_MONSTER_CYBER_ALERT');
  g_Sound_Delete('SOUND_MONSTER_CYBER_WALK');

  g_Sound_Delete('SOUND_MONSTER_KNIGHT_DIE');
  g_Sound_Delete('SOUND_MONSTER_KNIGHT_ALERT');

  g_Sound_Delete('SOUND_MONSTER_SPIDER_DIE');
  g_Sound_Delete('SOUND_MONSTER_SPIDER_ALERT');
  g_Sound_Delete('SOUND_MONSTER_SPIDER_WALK');

  g_Sound_Delete('SOUND_MONSTER_FISH_ATTACK');

  freeInds.Free();
  freeInds := nil;
end;

procedure g_Monsters_Init();
begin
  soulcount := 0;
end;

procedure g_Monsters_Free (clearGrid: Boolean=true);
var
  a: Integer;
begin
  e_LogWritefln('Cleared monster data (clearGrid=%s)', [clearGrid]);
  if (clearGrid) then
  begin
    monsGrid.Free();
    monsGrid := nil;
  end;
  for a := 0 to High(gMonsters) do gMonsters[a].Free();
  gMonsters := nil;
  clearUidMap();
  monCheckTrapLastFrameId := 0;
  monCheckMPlatLastFrameId := 0;
end;


// will be called from map loader
procedure g_Mons_InitTree (x, y, w, h: Integer);
begin
  monsGrid.Free();
  monsGrid := TMonsterGrid.Create(x, y, w, h);
  //clearUidMap(); // why not?
  e_LogWritefln('%s', ['Recreated monster tree']);
end;


function g_Monsters_Create(MonsterType: Byte; X, Y: Integer;
           Direction: TDirection; AdjCoord: Boolean = False; ForcedUID: Integer = -1): TMonster;
var
  find_id: DWORD;
  mon: TMonster;
begin
  result := nil;

  // Нет такого монстра
  if (MonsterType > MONSTER_MAN) or (MonsterType = 0) then exit;

  // Соблюдаем ограничение Lost_Soul'ов
  if MonsterType = MONSTER_SOUL then
  begin
    if soulcount > MAX_SOUL then exit;
    soulcount := soulcount + 1;
  end;

  find_id := allocMonster();

  mon := TMonster.Create(MonsterType, find_id, ForcedUID);
  gMonsters[find_id] := mon;
  mon.mArrIdx := find_id;
  mon.mProxyId := -1;

  uidMap[mon.FUID] := mon;

  // Настраиваем положение
  with mon do
  begin
    if AdjCoord then
    begin
      FObj.X := X-FObj.Rect.X - (FObj.Rect.Width div 2);
      FObj.Y := Y-FObj.Rect.Y - FObj.Rect.Height;
    end
    else
    begin
      FObj.X := X-FObj.Rect.X;
      FObj.Y := Y-FObj.Rect.Y;
    end;

    FDirection := Direction;
    FStartDirection := Direction;
    FStartX := GameX;
    FStartY := GameY;
    FObj.oldX := FObj.X;
    FObj.oldY := FObj.Y;
  end;

  mon.positionChanged();

  result := mon;
end;

procedure g_Monsters_killedp();
var
  a, h: Integer;
begin
  if gMonsters = nil then
    Exit;

  // Приколист смеется над смертью игрока:
  h := High(gMonsters);
  for a := 0 to h do
  begin
    if (gMonsters[a] <> nil) then
    begin
      with gMonsters[a] do
      begin
        if (FMonsterType = MONSTER_MAN) and
           (FState <> MONSTATE_DEAD) and
           (FState <> MONSTATE_SLEEP) and
           (FState <> MONSTATE_DIE) then
        begin
          g_Sound_PlayExAt('SOUND_MONSTER_TRUP', FObj.X, FObj.Y);
          Exit;
        end;
      end;
    end;
  end;
end;

procedure g_Monsters_PreUpdate();
var
  a: Integer;
begin
  if gMonsters = nil then Exit;
  for a := 0 to High(gMonsters) do
    if (gMonsters[a] <> nil) and (not gMonsters[a].FRemoved) then
      gMonsters[a].PreUpdate();
end;

procedure g_Monsters_Update();
var
  a: Integer;
begin
  // Целеуказатель
  if gTime mod (GAME_TICK*2) = 0 then
  begin
    pt_x := pt_x+pt_xs;
    pt_y := pt_y+pt_ys;
    if abs(pt_x) > 246 then pt_xs := -pt_xs;
    if abs(pt_y) > 100 then pt_ys := -pt_ys;
  end;

  gMon := True; // Для работы BlockMon'а

  if gmon_debug_think or gmon_debug_one_think_step then
  begin
    gmon_debug_one_think_step := false;
    for a := 0 to High(gMonsters) do
    begin
      if (gMonsters[a] = nil) then continue;
      if not gMonsters[a].FRemoved then
      begin
        if g_Game_IsClient then
          gMonsters[a].ClientUpdate()
        else
          gMonsters[a].Update();
      end
      else
      begin
        gMonsters[a].Free();
        gMonsters[a] := nil;
      end;
    end;
  end;

  gMon := False;
end;

function g_Monsters_ByUID (UID: Word): TMonster;
begin
  result := uidMap[UID];
end;

procedure g_Monsters_SaveState (st: TStream);
var
  count, i: Integer;
begin
  // Считаем количество существующих монстров
  count := 0;
  for i := 0 to High(gMonsters) do
  begin
    if (gMonsters[i] <> nil) and (gMonsters[i].FMonsterType <> MONSTER_NONE) then count += 1;
  end;

  // Сохраняем информацию целеуказателя
  utils.writeInt(st, LongInt(pt_x));
  utils.writeInt(st, LongInt(pt_xs));
  utils.writeInt(st, LongInt(pt_y));
  utils.writeInt(st, LongInt(pt_ys));

  // Количество монстров
  utils.writeInt(st, LongInt(count));

  if (count = 0) then exit;

  // Сохраняем монстров
  for i := 0 to High(gMonsters) do
  begin
    if (gMonsters[i] <> nil) and (gMonsters[i].FMonsterType <> MONSTER_NONE) then
    begin
      // Тип монстра
      utils.writeInt(st, Byte(gMonsters[i].MonsterType));
      // Сохраняем данные монстра:
      gMonsters[i].SaveState(st);
    end;
  end;
end;


procedure g_Monsters_LoadState (st: TStream);
var
  count, a: Integer;
  b: Byte;
  mon: TMonster;
begin
  assert(st <> nil);

  g_Monsters_Free(false);

  // Загружаем информацию целеуказателя
  pt_x := utils.readLongInt(st);
  pt_xs := utils.readLongInt(st);
  pt_y := utils.readLongInt(st);
  pt_ys := utils.readLongInt(st);

  // Количество монстров
  count := utils.readLongInt(st);

  if (count = 0) then exit;
  if (count < 0) or (count > 1024*1024) then raise XStreamError.Create('invalid monster count');

  // Загружаем монстров
  for a := 0 to count-1 do
  begin
    // Тип монстра
    b := utils.readByte(st);
    // Создаем монстра
    mon := g_Monsters_Create(b, 0, 0, TDirection.D_LEFT);
    if (mon = nil) then raise XStreamError.Create('g_Monsters_LoadState: ID = -1 (can''t create)');
    // Загружаем данные монстра
    mon.LoadState(st);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function g_Mons_SpawnAt (monType: Integer; x, y: Integer; dir: TDirection=TDirection.D_LEFT): TMonster; overload;
begin
  result := nil;
  if (monType >= MONSTER_DEMON) and (monType <= MONSTER_MAN) then
  begin
    result := g_Monsters_Create(monType, x, y, dir);
  end;
end;


function g_Mons_SpawnAt (const typeName: AnsiString; x, y: Integer; dir: TDirection=TDirection.D_LEFT): TMonster; overload;
begin
  result := g_Mons_SpawnAt(g_Mons_TypeIdByName(typeName), x, y, dir);
end;



// ////////////////////////////////////////////////////////////////////////// //
function g_Mons_TypeLo (): Integer; inline; begin result := Low(MONSTERTABLE); end;
function g_Mons_TypeHi (): Integer; inline; begin result := High(MONSTERTABLE); end;


function g_Mons_TypeIdByName (const name: String): Integer;
var
  i: Integer;
begin
  i := MONSTER_DEMON;
  while (i <= MONSTER_MAN) do
  begin
    if (CompareText(name, MONSTERTABLE[i].Name) = 0) then
    begin
      result := i;
      exit;
    end;
    Inc(i);
  end;
  result := -1;
  // HACK!
  if (CompareText(name, 'zombie') = 0) then result := MONSTER_ZOMBY;
end;


function g_Mons_NameByTypeId (monType: Integer): AnsiString;
begin
  if (monType >= MONSTER_DEMON) and (monType <= MONSTER_MAN) then
    result := MONSTERTABLE[monType].Name
  else
    result := '?';
end;


function g_Mons_GetKilledByTypeId (monType: Integer): AnsiString;
begin
  if (monType >= MONSTER_DEMON) and (monType <= MONSTER_MAN) then
    Result := KilledByMonster[monType]
  else
    Result := '?';
end;


// ////////////////////////////////////////////////////////////////////////// //
{ T M o n s t e r : }

procedure TMonster.setGameX (v: Integer); inline; begin FObj.X := v; positionChanged(); end;
procedure TMonster.setGameY (v: Integer); inline; begin FObj.Y := v; positionChanged(); end;

procedure TMonster.setPosition (ax, ay: Integer; callPosChanged: Boolean=true); inline; begin FObj.X := ax; FObj.Y := ay; if callPosChanged then positionChanged(); end;

procedure TMonster.moveBy (dx, dy: Integer); inline;
begin
  if (dx <> 0) or (dy <> 0) then
  begin
    FObj.X += dx;
    FObj.Y += dy;
    positionChanged();
  end;
end;

procedure TMonster.doDamage (v: Integer);
begin
  if (v <= 0) then exit;
  if (v > 32767) then v := 32767;
  Damage(v, 0, 0, 0, mEDamageType);
end;

procedure TMonster.ActionSound();
begin
  case FMonsterType of
    MONSTER_IMP:
      g_Sound_PlayExAt('SOUND_MONSTER_IMP_ACTION', FObj.X, FObj.Y);
    MONSTER_ZOMBY, MONSTER_SERG, MONSTER_CGUN,
    MONSTER_MANCUB:
      g_Sound_PlayExAt('SOUND_MONSTER_ACTION', FObj.X, FObj.Y);
    MONSTER_SOUL, MONSTER_BARON, MONSTER_CACO,
    MONSTER_KNIGHT, MONSTER_PAIN, MONSTER_DEMON,
    MONSTER_SPIDER:
      g_Sound_PlayExAt('SOUND_MONSTER_ACTION2', FObj.X, FObj.Y);
    MONSTER_BSP:
      g_Sound_PlayExAt('SOUND_MONSTER_BSP_ACTION', FObj.X, FObj.Y);
    MONSTER_VILE:
      g_Sound_PlayExAt('SOUND_MONSTER_VILE_ACTION', FObj.X, FObj.Y);
    MONSTER_SKEL:
      g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ACTION', FObj.X, FObj.Y);
    MONSTER_CYBER:
      ;
    MONSTER_MAN:
      g_Sound_PlayExAt('SOUND_MONSTER_HAHA', FObj.X, FObj.Y);
  end;
end;

procedure TMonster.PainSound();
begin
  if FPainSound then
    Exit;

  FPainSound := True;
  FPainTicks := 20;

  case FMonsterType of
    MONSTER_IMP, MONSTER_ZOMBY, MONSTER_SERG,
    MONSTER_SKEL, MONSTER_CGUN:
      g_Sound_PlayExAt('SOUND_MONSTER_PAIN', FObj.X, FObj.Y);
    MONSTER_SOUL, MONSTER_BARON, MONSTER_CACO,
    MONSTER_KNIGHT, MONSTER_DEMON, MONSTER_SPIDER,
    MONSTER_BSP, MONSTER_CYBER:
      g_Sound_PlayExAt('SOUND_MONSTER_PAIN2', FObj.X, FObj.Y);
    MONSTER_VILE:
      g_Sound_PlayExAt('SOUND_MONSTER_VILE_PAIN', FObj.X, FObj.Y);
    MONSTER_MANCUB:
      g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_PAIN', FObj.X, FObj.Y);
    MONSTER_PAIN:
      g_Sound_PlayExAt('SOUND_MONSTER_PAIN_PAIN', FObj.X, FObj.Y);
    MONSTER_MAN:
      g_Sound_PlayExAt('SOUND_MONSTER_MAN_PAIN', FObj.X, FObj.Y);
  end;
end;

procedure TMonster.DieSound();
begin
  case FMonsterType of
    MONSTER_IMP:
      case Random(2) of
        0: g_Sound_PlayExAt('SOUND_MONSTER_IMP_DIE_1', FObj.X, FObj.Y);
        1: g_Sound_PlayExAt('SOUND_MONSTER_IMP_DIE_2', FObj.X, FObj.Y);
      end;
    MONSTER_ZOMBY, MONSTER_SERG, MONSTER_CGUN:
      case Random(3) of
        0: g_Sound_PlayExAt('SOUND_MONSTER_DIE_1', FObj.X, FObj.Y);
        1: g_Sound_PlayExAt('SOUND_MONSTER_DIE_2', FObj.X, FObj.Y);
        2: g_Sound_PlayExAt('SOUND_MONSTER_DIE_3', FObj.X, FObj.Y);
      end;
    MONSTER_DEMON:
      g_Sound_PlayExAt('SOUND_MONSTER_DEMON_DIE', FObj.X, FObj.Y);
    MONSTER_BARREL:
      g_Sound_PlayExAt('SOUND_MONSTER_BARREL_DIE', FObj.X, FObj.Y);
    MONSTER_SOUL:
      g_Sound_PlayExAt('SOUND_MONSTER_SOUL_DIE', FObj.X, FObj.Y);
    MONSTER_BSP:
      g_Sound_PlayExAt('SOUND_MONSTER_BSP_DIE', FObj.X, FObj.Y);
    MONSTER_VILE:
      g_Sound_PlayExAt('SOUND_MONSTER_VILE_DIE', FObj.X, FObj.Y);
    MONSTER_BARON:
      g_Sound_PlayExAt('SOUND_MONSTER_BARON_DIE', FObj.X, FObj.Y);
    MONSTER_CACO:
      g_Sound_PlayExAt('SOUND_MONSTER_CACO_DIE', FObj.X, FObj.Y);
    MONSTER_CYBER:
      g_Sound_PlayExAt('SOUND_MONSTER_CYBER_DIE', FObj.X, FObj.Y);
    MONSTER_KNIGHT:
      g_Sound_PlayExAt('SOUND_MONSTER_KNIGHT_DIE', FObj.X, FObj.Y);
    MONSTER_MANCUB:
      g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_DIE', FObj.X, FObj.Y);
    MONSTER_PAIN:
      g_Sound_PlayExAt('SOUND_MONSTER_PAIN_DIE', FObj.X, FObj.Y);
    MONSTER_SKEL:
      g_Sound_PlayExAt('SOUND_MONSTER_SKEL_DIE', FObj.X, FObj.Y);
    MONSTER_SPIDER:
      g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_DIE', FObj.X, FObj.Y);
    MONSTER_MAN:
      g_Sound_PlayExAt('SOUND_MONSTER_MAN_DIE', FObj.X, FObj.Y);
  end;
end;

procedure TMonster.WakeUpSound();
begin
  case FMonsterType of
    MONSTER_IMP:
      case Random(2) of
        0: g_Sound_PlayExAt('SOUND_MONSTER_IMP_ALERT_1', FObj.X, FObj.Y);
        1: g_Sound_PlayExAt('SOUND_MONSTER_IMP_ALERT_2', FObj.X, FObj.Y);
      end;
    MONSTER_ZOMBY, MONSTER_SERG, MONSTER_CGUN:
      case Random(3) of
        0: g_Sound_PlayExAt('SOUND_MONSTER_ALERT_1', FObj.X, FObj.Y);
        1: g_Sound_PlayExAt('SOUND_MONSTER_ALERT_2', FObj.X, FObj.Y);
        2: g_Sound_PlayExAt('SOUND_MONSTER_ALERT_3', FObj.X, FObj.Y);
      end;
    MONSTER_MAN:
      g_Sound_PlayExAt('SOUND_MONSTER_MAN_ALERT', FObj.X, FObj.Y);
    MONSTER_BSP:
      g_Sound_PlayExAt('SOUND_MONSTER_BSP_ALERT', FObj.X, FObj.Y);
    MONSTER_VILE:
      g_Sound_PlayExAt('SOUND_MONSTER_VILE_ALERT', FObj.X, FObj.Y);
    MONSTER_BARON:
      g_Sound_PlayExAt('SOUND_MONSTER_BARON_ALERT', FObj.X, FObj.Y);
    MONSTER_CACO:
      g_Sound_PlayExAt('SOUND_MONSTER_CACO_ALERT', FObj.X, FObj.Y);
    MONSTER_CYBER:
      g_Sound_PlayExAt('SOUND_MONSTER_CYBER_ALERT', FObj.X, FObj.Y);
    MONSTER_KNIGHT:
      g_Sound_PlayExAt('SOUND_MONSTER_KNIGHT_ALERT', FObj.X, FObj.Y);
    MONSTER_MANCUB:
      g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_ALERT', FObj.X, FObj.Y);
    MONSTER_PAIN:
      g_Sound_PlayExAt('SOUND_MONSTER_PAIN_ALERT', FObj.X, FObj.Y);
    MONSTER_DEMON:
      g_Sound_PlayExAt('SOUND_MONSTER_DEMON_ALERT', FObj.X, FObj.Y);
    MONSTER_SKEL:
      g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ALERT', FObj.X, FObj.Y);
    MONSTER_SPIDER:
      g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_ALERT', FObj.X, FObj.Y);
    MONSTER_SOUL:
      ;
  end;
end;

procedure TMonster.BFGHit();
begin
  if FMonsterType = MONSTER_FISH then
    Exit;

  g_Weapon_BFGHit(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                  FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
  {if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                   FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                   0, NET_GFX_BFG);}
end;

function TMonster.Collide(X, Y: Integer; Width, Height: Word): Boolean;
begin
  Result := g_Collide(FObj.X+FObj.Rect.X,
                      FObj.Y+FObj.Rect.Y,
                      FObj.Rect.Width,
                      FObj.Rect.Height,
                      X, Y,
                      Width, Height);
end;

function TMonster.Collide(Panel: TPanel): Boolean;
begin
  Result := g_Collide(FObj.X+FObj.Rect.X,
                      FObj.Y+FObj.Rect.Y,
                      FObj.Rect.Width,
                      FObj.Rect.Height,
                      Panel.X, Panel.Y,
                      Panel.Width, Panel.Height);
end;

function TMonster.Collide(X, Y: Integer): Boolean;
begin
  X := X - FObj.X - FObj.Rect.X;
  Y := Y - FObj.Y - FObj.Rect.Y;
  Result := (x >= 0) and (x <= FObj.Rect.Width) and
            (y >= 0) and (y <= FObj.Rect.Height);
end;

procedure TMonster.Respawn;
begin
  FObj.Vel.X := 0;
  FObj.Vel.Y := 0;
  FObj.Accel.X := 0;
  FObj.Accel.Y := 0;
  FDirection := FStartDirection;
  {GameX}FObj.X := FStartX;
  {GameY}FObj.Y := FStartY;
  FObj.Rect := MONSTERTABLE[FMonsterType].Rect;
  FHealth := MONSTERTABLE[FMonsterType].Health;
  FAmmo := 0;
  FPain := 0;
  FTargetUID := 0;
  FTargetTime := 0;
  FDieTriggers := nil;
  FWaitAttackAnim := False;
  FChainFire := False;
  FShellTimer := -1;

  FState := MONSTATE_SLEEP;
  FCurAnim := ANIM_SLEEP;

  positionChanged(); // this updates spatial accelerators

  if g_Game_IsNet and g_Game_IsServer then
  begin
    MH_SEND_MonsterPos(FUID);
    MH_SEND_MonsterState(FUID);
  end;
end;

constructor TMonster.Create(MonsterType: Byte; aID: Integer; ForcedUID: Integer = -1);
  var a: Integer;
begin
  if ForcedUID < 0 then
    FUID := g_CreateUID(UID_MONSTER)
  else
    FUID := ForcedUID;

  FMonsterType := MonsterType;

  g_Obj_Init(@FObj);

  FState := MONSTATE_SLEEP;
  FCurAnim := ANIM_SLEEP;
  FHealth := MONSTERTABLE[MonsterType].Health;
  FMaxHealth := FHealth;
  FObj.Rect := MONSTERTABLE[MonsterType].Rect;
  FDieTriggers := nil;
  FSpawnTrigger := -1;
  FWaitAttackAnim := False;
  FChainFire := False;
  FStartID := aID;
  FNoRespawn := False;
  FShellTimer := -1;
  FBehaviour := BH_NORMAL;
  FFireTime := 0;
  FFirePainTime := 0;
  FFireAttacker := 0;
  mEDamageType := HIT_SOME;

  mProxyId := -1;
  mArrIdx := -1;
  trapCheckFrameId := 0;
  mplatCheckFrameId := 0;
  mNeedSend := false;

  if FMonsterType in [MONSTER_ROBO, MONSTER_BARREL] then
    FBloodKind := BLOOD_SPARKS
  else
    FBloodKind := BLOOD_NORMAL;
  if FMonsterType = MONSTER_CACO then
  begin
    FBloodRed := 0;
    FBloodGreen := 0;
    FBloodBlue := 150;
  end
  else if FMonsterType in [MONSTER_BARON, MONSTER_KNIGHT] then
  begin
    FBloodRed := 0;
    FBloodGreen := 150;
    FBloodBlue := 0;
  end
  else
  begin
    FBloodRed := 150;
    FBloodGreen := 0;
    FBloodBlue := 0;
  end;

  SetLength(FAnim, Length(ANIMTABLE));
  for a := ANIM_SLEEP to ANIM_PAIN do
  begin
    FAnim[a, TDirection.D_RIGHT] := TAnimationState.Create(ANIMTABLE[a].loop, MONSTER_ANIMTABLE[MonsterType].AnimSpeed[a], MONSTER_ANIMTABLE[MonsterType].AnimCount[a]);
    FAnim[a, TDirection.D_LEFT] := TAnimationState.Create(ANIMTABLE[a].loop, MONSTER_ANIMTABLE[MonsterType].AnimSpeed[a], MONSTER_ANIMTABLE[MonsterType].AnimCount[a]);
  end;
  if MonsterType = MONSTER_VILE then
    vilefire := TAnimationState.Create(True, 2, 0); // !!! len
end;

function TMonster.Damage(aDamage: Word; VelX, VelY: Integer; SpawnerUID: Word; t: Byte): Boolean;
var
  c, it: Integer;
  p: TPlayer;
begin
  Result := False;

// Монстр статичен пока идет warmup
  if (gLMSRespawn > LMS_RESPAWN_NONE) then exit;

// Умирает, умер или воскрешается => урон делать некому:
  if (FState = MONSTATE_DEAD) or (FState = MONSTATE_DIE) or (FState = MONSTATE_REVIVE) then
    Exit;

// Рыбу в воде бьет током => паника без урона:
  if (t = HIT_ELECTRO) and (FMonsterType = MONSTER_FISH) and g_Game_IsServer then
  begin
    FSleep := 20;
    if Random(2) = 0 then FDirection := TDirection.D_RIGHT else FDirection := TDirection.D_LEFT;
    Result := True;
    SetState(MONSTATE_RUN);
    Exit;
  end;

// Арчи не горят, черепа уже горят
  if (t = HIT_FLAME) and (FMonsterType in [MONSTER_VILE, MONSTER_SOUL]) then
  begin
  // Проснуться все-таки стоит
    if FState = MONSTATE_SLEEP then
      SetState(MONSTATE_GO);
    Exit;
  end;

// Ловушка убивает сразу:
  if t = HIT_TRAP then
    FHealth := -100;

// Роботу урона нет:
  if FMonsterType = MONSTER_ROBO then
    aDamage := 0;

// Наносим урон:
  if g_Game_IsServer then Dec(FHealth, aDamage);

// Усиливаем боль монстра от урона:
  if FPain = 0 then
    FPain := 3;
  FPain := FPain+aDamage;

// Если боль существенная, то меняем состояние на болевое:
  if FState <> MONSTATE_PAIN then
    if (FPain >= MONSTERTABLE[FMonsterType].MinPain) and
       (FMonsterType <> MONSTER_BARREL) then
         SetState(MONSTATE_PAIN);

// Если разрешена кровь - создаем брызги крови:
  if (gBloodCount > 0) then
  begin
    c := Min(aDamage, 200);
    c := c*gBloodCount - (aDamage div 4) + Random(c div 2);

    if (VelX = 0) and (VelY = 0) then
      MakeBloodSimple(c)
    else
      case t of
        HIT_TRAP, HIT_ACID, HIT_ELECTRO, HIT_FLAME: MakeBloodSimple(c);
        HIT_BFG, HIT_ROCKET, HIT_SOME: MakeBloodVector(c, VelX, VelY);
      end;
  end;

// Теперь цель - ударивший, если только не сам себя:
  if (SpawnerUID <> FUID) and (BehaviourDamage(SpawnerUID, FBehaviour, FMonsterType)) then
  begin
    FTargetUID := SpawnerUID;
    FTargetTime := 0;
  end;

// Здоровье закончилось:
  if FHealth <= 0 then
    begin
    // Если это не бочка и убил игрок, то ему +1:
      if (FMonsterType <> MONSTER_BARREL) then
      begin
        if (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
        begin
          p := g_Player_Get(SpawnerUID);
          if (p <> nil) and (gLMSRespawn = LMS_RESPAWN_NONE) then
          begin
            p.MonsterKills := p.MonsterKills+1;
            if gGameSettings.GameMode = GM_COOP then
              p.Frags := p.Frags + 1;
            // Uncomment this if you want to double-kill monsters
            //p.FragCombo();
          end;
        end;
        if gLMSRespawn = LMS_RESPAWN_NONE then
        begin
          Inc(gCoopMonstersKilled);
          if g_Game_IsNet then
            MH_SEND_GameStats;
        end;
      end;

    // Выбираем лут:
      case FMonsterType of
        MONSTER_ZOMBY: c := ITEM_AMMO_BULLETS;
        MONSTER_SERG: c := ITEM_WEAPON_SHOTGUN1;
        MONSTER_CGUN: c := ITEM_WEAPON_CHAINGUN;
        MONSTER_MAN: c := ITEM_KEY_RED;
        else c := 0;
      end;

    // Бросаем лут:
      if c <> 0 then
      begin
        it := g_Items_Create(FObj.X + (FObj.Rect.Width div 2),
                             FObj.Y + (FObj.Rect.Height div 2),
                             c, True, False);
        g_Items_SetDrop(it); // mark it as monster drop
        g_Obj_Push(g_Items_ObjByIdx(it), (FObj.Vel.X div 2)-3+Random(7),
                                        (FObj.Vel.Y div 2)-Random(4));
        //positionChanged(); // this updates spatial accelerators
        if g_Game_IsServer and g_Game_IsNet then
          MH_SEND_ItemSpawn(True, it);
      end;

    // Труп дальше не идет:
      FObj.Vel.X := 0;

    // У трупа размеры меньше:
      if (FMonsterType <> MONSTER_FISH) and (FMonsterType <> MONSTER_PAIN) then
      begin
        FObj.Rect.Y := FObj.Rect.Y + FObj.Rect.Height-12;
        FObj.Rect.Height := 12;
        positionChanged();
      end;

    // Урон был сильным => слабые - в кашу:
      if (FHealth <= -30) and
         ((FMonsterType = MONSTER_IMP) or (FMonsterType = MONSTER_ZOMBY) or
          (FMonsterType = MONSTER_SERG) or (FMonsterType = MONSTER_CGUN) or
          (FMonsterType = MONSTER_MAN)) then
        begin
          g_Sound_PlayExAt('SOUND_MONSTER_SLOP', FObj.X, FObj.Y);
          SetState(MONSTATE_DIE, ANIM_MESS);
        end
      else
        begin
          DieSound();
          SetState(MONSTATE_DIE);
        end;

    // Активировать триггеры, ждущие смерти этого монстра:
      if g_Game_IsServer then ActivateTriggers();

      FHealth := 0;
    end
  else
    if FState = MONSTATE_SLEEP then
    begin // Спал, разбудили несмертельным ударом:
      FPain := MONSTERTABLE[FMonsterType].Pain;
      SetState(MONSTATE_GO);
    end;

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterState(FUID);
  Result := True;
end;

function TMonster.Heal(Value: Word): Boolean;
begin
  Result := False;
  if g_Game_IsClient then
    Exit;
  if not alive then
    Exit;

  if FHealth < FMaxHealth then
  begin
    IncMax(FHealth, Value, FMaxHealth);
    if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterState(FUID);
    Result := True;
  end;
end;

destructor TMonster.Destroy();
var
  a: Integer;
begin
  for a := 0 to High(FAnim) do
  begin
    FAnim[a, TDirection.D_LEFT].Free();
    FAnim[a, TDirection.D_RIGHT].Free();
  end;

  vilefire.Free();

  if (mProxyId <> -1) then
  begin
    if (monsGrid <> nil) then
    begin
      monsGrid.removeBody(mProxyId);
      {$IF DEFINED(D2F_DEBUG_MONS_MOVE)}
      e_WriteLog(Format('monster #%d:(%u): removed from grid; mProxyid=%d', [mArrIdx, UID, mProxyId]), MSG_NOTIFY);
      {$ENDIF}
    end;
    mProxyId := -1;
  end;

  if (mArrIdx <> -1) and (mArrIdx < Length(gMonsters)) then
  begin
    freeInds.release(mArrIdx);
    gMonsters[mArrIdx] := nil;
  end;
  mArrIdx := -1;

  uidMap[FUID] := nil;

  inherited Destroy();
end;

procedure TMonster.MakeBloodSimple(Count: Word);
begin
  g_GFX_Blood(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)+8,
              FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
              Count div 2, 3, -1, 16, (FObj.Rect.Height*2 div 3),
              FBloodRed, FBloodGreen, FBloodBlue, FBloodKind);
  g_GFX_Blood(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-8,
              FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
              Count div 2, -3, -1, 16, (FObj.Rect.Height*2) div 3,
              FBloodRed, FBloodGreen, FBloodBlue, FBloodKind);
end;

procedure TMonster.MakeBloodVector(Count: Word; VelX, VelY: Integer);
begin
  g_GFX_Blood(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
              FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
              Count, VelX, VelY, 16, (FObj.Rect.Height*2) div 3,
              FBloodRed, FBloodGreen, FBloodBlue, FBloodKind);
end;

procedure TMonster.Push(vx, vy: Integer);
begin
  FObj.Accel.X := FObj.Accel.X + vx;
  FObj.Accel.Y := FObj.Accel.Y + vy;
  if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterPos(FUID);
end;

procedure TMonster.SetState(State: Byte; ForceAnim: Byte = 255);
var
  Anim: Byte;
begin
// Если состояние = начали умирать, а этот монстр = Lost_Soul,
// то соблюдаем ограничение количества Lost_Soul'ов:
  if (State = MONSTATE_DIE) and (MonsterType = MONSTER_SOUL) then
    soulcount := soulcount-1;

// Присмерти - нельзя сразу начинать атаковать или бегать:
  case FState of
    MONSTATE_DIE, MONSTATE_DEAD, MONSTATE_REVIVE:
      if (State <> MONSTATE_DEAD) and (State <> MONSTATE_REVIVE) and
         (State <> MONSTATE_GO) then
        Exit;
  end;

// Смена состояния:
  FState := State;

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterState(FUID, ForceAnim);

// Новая анимация при новом состоянии:
  case FState of
    MONSTATE_SLEEP: Anim := ANIM_SLEEP;
    MONSTATE_PAIN: Anim := ANIM_PAIN;
    MONSTATE_WAIT: Anim := ANIM_SLEEP;
    MONSTATE_CLIMB, MONSTATE_RUN, MONSTATE_RUNOUT, MONSTATE_GO: Anim := ANIM_GO;
    MONSTATE_SHOOT: Anim := ANIM_ATTACK;
    MONSTATE_ATTACK: Anim := ANIM_ATTACK;
    MONSTATE_DIE: Anim := ANIM_DIE;
    MONSTATE_REVIVE:
      begin // начали восрешаться
        Anim := FCurAnim;
        FAnim[Anim, FDirection].Revert(True);

        FObj.Rect := MONSTERTABLE[FMonsterType].Rect;
        FHealth := MONSTERTABLE[FMonsterType].Health;
        FAmmo := 0;
        FPain := 0;
      end;
    else Exit;
  end;

// Надо сменить анимацию на нестандартную:
  if ForceAnim <> 255 then
    Anim := ForceAnim;

// Если анимация новая - перезапускаем её:
  if FCurAnim <> Anim then
    if FAnim[Anim, FDirection] <> nil then
    begin
      FAnim[Anim, FDirection].Reset();
      FCurAnim := Anim;
    end;
end;

function TMonster.TeleportTo(X, Y: Integer; silent: Boolean; dir: Byte): Boolean;
begin
  Result := False;

// В точке назначения стена:
  if g_CollideLevel(X, Y, FObj.Rect.Width, FObj.Rect.Height) then
  begin
    g_Sound_PlayExAt('SOUND_GAME_NOTELEPORT', FObj.X, FObj.Y);
    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Sound(FObj.X, FObj.Y, 'SOUND_GAME_NOTELEPORT');
    Exit;
  end;

// Эффект телепорта в позиции монстра:
  if not silent then
  begin
    g_Sound_PlayExAt('SOUND_GAME_TELEPORT', Obj.X, Obj.Y);
    r_GFX_OnceAnim(
      R_GFX_TELEPORT,
      FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
      FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32
    );

    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
                     FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, 1,
                     NET_GFX_TELE);
  end;

  FObj.X := X - FObj.Rect.X;
  FObj.Y := Y - FObj.Rect.Y;
  FObj.oldX := FObj.X; // don't interpolate after teleport
  FObj.oldY := FObj.Y;
  positionChanged();

  if dir = 1 then
    FDirection := TDirection.D_LEFT
  else
    if dir = 2 then
      FDirection := TDirection.D_RIGHT
    else
      if dir = 3 then
      begin // обратное
        if FDirection = TDirection.D_RIGHT then
          FDirection := TDirection.D_LEFT
        else
          FDirection := TDirection.D_RIGHT;
      end;

// Эффект телепорта в точке назначения:
  if not silent then
  begin
    r_GFX_OnceAnim(
      R_GFX_TELEPORT,
      FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
      FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32
    );

    if g_Game_IsServer and g_Game_IsNet then
     MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
                    FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, 0,
                    NET_GFX_TELE);
  end;

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterPos(FUID);
  Result := True;
end;

procedure TMonster.PreUpdate();
begin
  FObj.oldX := FObj.X;
  FObj.oldY := FObj.Y;
end;

procedure TMonster.Update();
var
  a, b, sx, sy, wx, wy, oldvelx: Integer;
  st: Word;
  o, co: TObj;
  fall, bubbles: Boolean;
  mon: TMonster;
  mit: PMonster;
  it: TMonsterGrid.Iter;
label
  _end;
begin
  fall := True;
  bubbles := True;

// Монстр статичен пока идет warmup
  if (gLMSRespawn > LMS_RESPAWN_NONE) then exit;

// Рыбы "летают" только в воде:
  if FMonsterType = MONSTER_FISH then
    if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
      if (FState <> MONSTATE_DIE) and (FState <> MONSTATE_DEAD) then
        fall := False;

// Летающие монтсры:
  if ((FMonsterType = MONSTER_SOUL) or
      (FMonsterType = MONSTER_PAIN) or
      (FMonsterType = MONSTER_CACO)) and
     (FState <> MONSTATE_DIE) and
     (FState <> MONSTATE_DEAD) then
    fall := False;

// Меняем скорость только по четным кадрам:
  if gTime mod (GAME_TICK*2) <> 0 then
  begin
    g_Obj_Move(@FObj, fall, True, True);
    positionChanged(); // this updates spatial accelerators
    Exit;
  end;

  if FPainTicks > 0 then
    Dec(FPainTicks)
  else
    FPainSound := False;

// Двигаемся:
  st := g_Obj_Move(@FObj, fall, True, True);
  positionChanged(); // this updates spatial accelerators

// Если горим - поджигаем других монстров, но не на 100 тиков каждый раз:
  if FFireTime > 0 then
  begin
    it := monsGrid.forEachInAABB(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width, FObj.Rect.Height);
    for mit in it do
      if mit.UID <> FUID then
        mit.CatchFire(FFireAttacker, FFireTime);
  end;

// Вылетел за карту - удаляем и запускаем триггеры:
  if WordBool(st and MOVE_FALLOUT) or (FObj.X < -1000) or
     (FObj.X > gMapInfo.Width+1000) or (FObj.Y < -1000) then
  begin
    FRemoved := True;
    if alive and (gLMSRespawn = LMS_RESPAWN_NONE) then
    begin
      Inc(gCoopMonstersKilled);
      if g_Game_IsNet then
        MH_SEND_GameStats;
    end;
    ActivateTriggers();
    Exit;
  end;

  oldvelx := FObj.Vel.X;

// Сопротивление воздуха для трупа:
  if (FState = MONSTATE_DIE) or (FState = MONSTATE_DEAD) then
    FObj.Vel.X := z_dec(FObj.Vel.X, 1);

  if FFireTime > 0 then
  begin
    if WordBool(st and MOVE_INWATER) then
      FFireTime := 0
    else
    begin
      OnFireFlame(1);
      FFireTime := FFireTime - 1;
      if (FState <> MONSTATE_DIE) and (FState <> MONSTATE_DEAD) then
        if FFirePainTime = 0 then
        begin
          Damage(5, FFireAttacker, 0, 0, HIT_FLAME);
          FFirePainTime := 18;
        end
        else
          FFirePainTime := FFirePainTime - 1;
    end;
  end;

// Мертвый ничего не делает:
  if (FState = MONSTATE_DEAD) then
    goto _end;

// AI монстров выключен:
  if g_debug_MonsterOff then
  begin
    FSleep := 1;
    if FState <> MONSTATE_SLEEP then
      SetState(MONSTATE_SLEEP);
  end;

// Возможно, создаем пузырьки в воде:
  if WordBool(st and MOVE_INWATER) and (Random(32) = 0) then
  begin
    case FMonsterType of
      MONSTER_FISH:
        if Random(4) <> 0 then bubbles := False else
          g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                        FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      MONSTER_ROBO, MONSTER_BARREL:
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                      FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      else
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width-4),
                      FObj.Y+FObj.Rect.Y + Random(4), 5, 4, 4);
    end;
    if bubbles then if Random(2) = 0
      then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
      else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
  end;

// Если прошел первый кадр анимации взрыва бочки, то взрыв:
  if FMonsterType = MONSTER_BARREL then
  begin
    if (FState = MONSTATE_DIE) and (FAnim[FCurAnim, FDirection].CurrentFrame = 1) and
       (FAnim[FCurAnim, FDirection].Counter = 0) then
      g_Weapon_Explode(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                       FObj.Y+FObj.Rect.Y+FObj.Rect.Height-16,
                       60, FUID);
  end;

// Lost_Soul вылетел из воды => ускоряется:
  if FMonsterType = MONSTER_SOUL then
    if WordBool(st and MOVE_HITAIR) then
      g_Obj_SetSpeed(@FObj, 16);

  if FAmmo < 0 then
    FAmmo := FAmmo + 1;

// Если начали всплывать, то продолжаем:
  if FObj.Vel.Y < 0 then
    if WordBool(st and MOVE_INWATER) then
      FObj.Vel.Y := -4;

// Таймер - ждем после потери цели:
  FTargetTime := FTargetTime + 1;

// Гильзы
  if FShellTimer > -1 then
    if FShellTimer = 0 then
    begin
      if FShellType = SHELL_SHELL then
        g_Player_CreateShell(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                             GameVelX, GameVelY-2, SHELL_SHELL)
      else if FShellType = SHELL_DBLSHELL then
      begin
        g_Player_CreateShell(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                             GameVelX-1, GameVelY-2, SHELL_SHELL);
        g_Player_CreateShell(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                             GameVelX+1, GameVelY-2, SHELL_SHELL);
      end;
      FShellTimer := -1;
    end else Dec(FShellTimer);

// Пробуем увернуться от летящей пули:
  if fall then
    if (FState in [MONSTATE_GO, MONSTATE_RUN, MONSTATE_RUNOUT,
                   MONSTATE_ATTACK, MONSTATE_SHOOT]) then
      if g_Weapon_Danger(FUID, FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                         FObj.Rect.Width, FObj.Rect.Height, 50) then
        if (g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj)) and
           (FObj.Accel.Y = 0) then
          FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;

  case FState of
    MONSTATE_PAIN: // Состояние - Боль
      begin
      // Боль сильная => монстр кричит:
        if FPain >= MONSTERTABLE[FMonsterType].Pain then
        begin
          FPain := MONSTERTABLE[FMonsterType].Pain;
          if gSoundEffectsDF then PainSound();
        end;
        if (not gSoundEffectsDF) and (FPain >= MONSTERTABLE[FMonsterType].MinPain) then
          PainSound();

      // Снижаем боль со временем:
        FPain := FPain - 5;

      // Боль уже не ошутимая => идем дальше:
        if FPain <= MONSTERTABLE[FMonsterType].MinPain then
        begin
          FPain := 0;
          FAmmo := -9;
          SetState(MONSTATE_GO);
        end;
      end;

    MONSTATE_SLEEP: // Состояние - Сон
      begin
      // Спим:
        FSleep := FSleep + 1;

      // Проспали достаточно:
        if FSleep >= 18 then
          FSleep := 0
        else // еще спим
          goto _end;

      // На игроков идут только обычные монстры, киллеры и маньяки
        if (FBehaviour = BH_NORMAL) or (FBehaviour = BH_KILLER) or (FBehaviour = BH_MANIAC) then
        // Если есть игрок рядом, просыпаемся и идем к нему:
          if (gPlayers <> nil) then
            for a := 0 to High(gPlayers) do
              if (gPlayers[a] <> nil) and (gPlayers[a].alive)
              and (not gPlayers[a].NoTarget) and (gPlayers[a].FMegaRulez[MR_INVIS] < gTime) then
                with gPlayers[a] do
                  if g_Look(@FObj, @Obj, FDirection) then
                  begin
                    FTargetUID := gPlayers[a].UID;
                    FTargetTime := 0;
                    WakeUpSound();
                    SetState(MONSTATE_GO);
                    Break;
                  end;

      // На монстров тянет маньяков, поехавших и каннибалов
        if (FTargetUID = 0) and ((FBehaviour = BH_MANIAC)
        or (FBehaviour = BH_INSANE) or (FBehaviour = BH_CANNIBAL)) then
        // Если есть подходящий монстр рядом:
          if gMonsters <> nil then
            for a := 0 to High(gMonsters) do
              if (gMonsters[a] <> nil) and (gMonsters[a].alive) and
                 (gMonsters[a].FUID <> FUID) then
              begin
                // Маньяки нападают на всех монстров, кроме друзей
                if (FBehaviour = BH_MANIAC) and
                (IsFriend(gMonsters[a].FMonsterType, FMonsterType)) then
                  Continue;
                // Поехавшие также, но могут обозлиться на бочку
                if (FBehaviour = BH_INSANE) and (gMonsters[a].FMonsterType <> MONSTER_BARREL) and
                (IsFriend(gMonsters[a].FMonsterType, FMonsterType)) then
                  Continue;
                // Каннибалы нападают на себе подобных
                if (FBehaviour = BH_CANNIBAL) and (gMonsters[a].FMonsterType <> FMonsterType) then
                  Continue;
                if g_Look(@FObj, @gMonsters[a].Obj, FDirection) then
                begin
                  FTargetUID := gMonsters[a].UID;
                  FTargetTime := 0;
                  WakeUpSound();
                  SetState(MONSTATE_GO);
                  Break;
                end;
              end;
      end;

    MONSTATE_WAIT: // Состояние - Ожидание
      begin
      // Ждем:
        FSleep := FSleep - 1;

      // Выждали достаточно - идем:
        if FSleep < 0 then
          SetState(MONSTATE_GO);
      end;

    MONSTATE_GO: // Состояние - Движение (с осмотром ситуации)
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          Turn();
          FSleep := 40;
          SetState(MONSTATE_RUNOUT);

          goto _end;
        end;

      // Если монстр - колдун, то пробуем воскресить кого-нибудь:
        if (FMonsterType = MONSTER_VILE) then
          if isCorpse(@FObj, False) <> -1 then
          begin
            FObj.Vel.X := 0;
            SetState(MONSTATE_ATTACK, ANIM_ATTACK2);

            goto _end;
          end;

      // Цель погибла или давно ждем:
         if (not GetPos(FTargetUID, @o)) or (FTargetTime > MAX_ATM) then
           if not findNewPrey() then
             begin // Новых целей нет
              FTargetUID := 0;
              o.X := FObj.X+pt_x;
              o.Y := FObj.Y+pt_y;
              o.Vel.X := 0;
              o.Vel.Y := 0;
              o.Accel.X := 0;
              o.Accel.Y := 0;
              o.Rect := _Rect(0, 0, 0, 1);
             end
           else // Новая цель есть - берем ее координаты
            GetPos(FTargetUID, @o);

      // Цель очень близко - пинаем:
        if g_Obj_Collide(@FObj, @o) and (FTargetUID <> 0) then
        begin
          FTargetTime := 0;
          if (FMonsterType <> MONSTER_CYBER) or (Random(2) = 0) then
          begin
            if kick(@o) then
              goto _end;
          end;
        end;

      // Расстояние до цели:
        sx := o.X+o.Rect.X+(o.Rect.Width div 2)-(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2));
        sy := o.Y+o.Rect.Y+(o.Rect.Height div 2)-(FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));

      // Поворачиваемся в сторону цели:
        if sx > 0 then
          FDirection := TDirection.D_RIGHT
        else
          FDirection := TDirection.D_LEFT;

      // Если монстр умеет стрелять и есть по кому - стреляем:
        if canShoot(FMonsterType) and (FTargetUID <> 0) then
          if Abs(sx) > Abs(sy) then // угол выстрела удобный
            if shoot(@o, False) then
              goto _end;

      // Если цель почти на одной вертикали - бегаем туда-сюда:
        if Abs(sx) < 40 then
          if FMonsterType <> MONSTER_FISH then
          begin
            FSleep := 15;
            SetState(MONSTATE_RUN);
            if Random(2) = 0 then
              FDirection := TDirection.D_LEFT
            else
              FDirection := TDirection.D_RIGHT;

            goto _end;
          end;

      // Уперлись в стену:
        if WordBool(st and MOVE_HITWALL) then
        begin
          if g_Triggers_PressR(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width,
                               FObj.Rect.Height, FUID, ACTIVATE_MONSTERPRESS) <> nil then
          begin // Смогли нажать кнопку - небольшое ожидание
            FSleep := 4;
            SetState(MONSTATE_WAIT);

            goto _end;
          end;

          case FMonsterType of
            MONSTER_CACO, MONSTER_SOUL, MONSTER_PAIN, MONSTER_FISH: ;
            else // Не летают:
              if (g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj)) and
                 (FObj.Accel.Y = 0) then
              begin // Стоим на твердом полу или ступени
              // Прыжок через стену:
                FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;
                SetState(MONSTATE_CLIMB);
              end;
          end;

          goto _end;
        end;

      // Монстры, не подверженные гравитации:
        if (FMonsterType = MONSTER_CACO) or (FMonsterType = MONSTER_SOUL) or
           (FMonsterType = MONSTER_PAIN) or (FMonsterType = MONSTER_FISH) then
          begin
            if FMonsterType = MONSTER_FISH then
              begin // Рыба
                if not WordBool(st and MOVE_INWATER) then
                  begin // Рыба вне воды:
                    if g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj) then
                    begin // "Стоит" твердо
                    // Рыба трепыхается на поверхности:
                      if FObj.Accel.Y = 0 then FObj.Vel.Y := -6;
                      FObj.Accel.X := FObj.Accel.X - 8 + Random(17);
                    end;

                  // Рыбе больно:
                    SetState(MONSTATE_PAIN);
                    FPain := FPain + 50;
                  end
                else // Рыба в воде
                  begin
                  // Плывем в сторону цели по-вертикали:
                    if Abs(sy) > 8 then
                      FObj.Vel.Y := g_basic.Sign(sy)*4
                    else
                      FObj.Vel.Y := 0;

                  // Рыба плывет вверх:
                    if FObj.Vel.Y < 0 then
                      if not g_Obj_CollideLiquid(@FObj, 0, -16) then
                      begin
                      // Всплыли до поверхности - стоп:
                        FObj.Vel.Y := 0;
                      // Плаваем туда-сюда:
                        if Random(2) = 0 then
                          FDirection := TDirection.D_LEFT
                        else
                          FDirection := TDirection.D_RIGHT;
                        FSleep := 20;
                        SetState(MONSTATE_RUN);
                      end;
                   end;
              end
            else // Летающие монстры
              begin
              // Летим в сторону цели по-вертикали:
                if Abs(sy) > 8 then
                  FObj.Vel.Y := g_basic.Sign(sy)*4
                else
                  FObj.Vel.Y := 0;
              end;
          end
        else // "Наземные" монстры
          begin
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
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61)+120); // налево
                  end
                  else
                  begin
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61));    // направо
                  end;
                end;
              end;
            end;
          // Боссы могут пинать трупы:
            if (FMonsterType in [MONSTER_CYBER, MONSTER_SPIDER, MONSTER_ROBO]) and
               (FObj.Vel.X <> 0) and (gCorpses <> nil) then
            begin
              b := Abs(FObj.Vel.X);
              if b > 1 then b := b * (Random(8 div b) + 1);
              for a := 0 to High(gCorpses) do
                if (gCorpses[a] <> nil) and (gCorpses[a].State > 0) then
                begin
                  co := gCorpses[a].Obj;
                  if g_Obj_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y+FObj.Rect.Height-4,
                                   FObj.Rect.Width, 8, @co) and (Random(3) = 0) then
                    // Пинаем трупы
                    if FObj.Vel.X < 0 then
                      gCorpses[a].Damage(b*2, FUID, -b, Random(7)) // налево
                    else
                      gCorpses[a].Damage(b*2, FUID, b, Random(7)); // направо
                end;
            end;
          // Если цель высоко, то, возможно, прыгаем:
            if sy < -40 then
              if g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj) then
              // стоит твердо
                if (Random(4) = 0) and (FObj.Accel.Y = 0) then
                  FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;
          end;

        FSleep := FSleep + 1;

      // Иногда рычим:
        if FSleep >= 8 then
        begin
          FSleep := 0;
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_RUN: // Состояние - Бег
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          Turn();
          FSleep := 40;
          SetState(MONSTATE_RUNOUT);

          goto _end;
        end;

        FSleep := FSleep - 1;

      // Пробежали достаточно или врезались в стену => переходим на шаг:
        if (FSleep <= 0) or (WordBool(st and MOVE_HITWALL) and ((FObj.Vel.Y+FObj.Accel.Y) = 0)) then
        begin
          FSleep := 0;
          SetState(MONSTATE_GO);
        // Стена - идем обратно:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
            Turn();
        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_RUNOUT: // Состояние - Убегает от чего-то
      begin
      // Вышли из БлокМона:
        if (not WordBool(st and MOVE_BLOCK)) and (FSleep > 0) then
          FSleep := 0;

        FSleep := FSleep - 1;

      // Убажели достаточно далеко => переходим на шаг:
        if FSleep <= -18 then
        begin
          FSleep := 0;
          SetState(MONSTATE_GO);
        // Стена/БлокМон - идем обратно:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
            Turn();
        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_CLIMB: // Состояние - Прыжок (чтобы обойти стену)
      begin
      // Достигли высшей точки прыжка или стена кончилась => переходим на шаг:
        if ((FObj.Vel.Y+FObj.Accel.Y) >= 0) or
           (not WordBool(st and MOVE_HITWALL)) then
        begin
          FSleep := 0;
          SetState(MONSTATE_GO);

        // Стена не кончилась => бежим от нее:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
          begin
            Turn();
            FSleep := 15;
            SetState(MONSTATE_RUN);
          end;
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_ATTACK, // Состояние - Атака
    MONSTATE_SHOOT:  // Состояние - Стрельба
      begin
      // Lost_Soul врезался в стену при атаке => переходит на шаг:
        if FMonsterType = MONSTER_SOUL then
        begin
          if WordBool(st and (MOVE_HITWALL or MOVE_HITCEIL or MOVE_HITLAND)) then
            SetState(MONSTATE_GO);

          goto _end;
        end;

      // Замедляемся при атаке:
        if FMonsterType <> MONSTER_FISH then
          FObj.Vel.X := z_dec(FObj.Vel.X, 1);

      // Нужно стрелять, а монстр - колдун:
        if (FMonsterType = MONSTER_VILE) and (FState = MONSTATE_SHOOT) then
        begin
        // Цель погибла => идем дальше:
          if not GetPos(FTargetUID, @o) then
          begin
            SetState(MONSTATE_GO);

            goto _end;
          end;

        // Цель не видно => идем дальше:
          if not g_Look(@FObj, @o, FDirection) then
          begin
            SetState(MONSTATE_GO);

            goto _end;
          end;

        // Цель в воде - не загорится => идем дальше:
          if g_Obj_CollideWater(@o, 0, 0) then
          begin
            SetState(MONSTATE_GO);

            goto _end;
          end;

        // Жарим цель:
          tx := o.X+o.Rect.X+(o.Rect.Width div 2);
          ty := o.Y+o.Rect.Y+(o.Rect.Height div 2);
          g_Weapon_HitUID(FTargetUID, 2, FUID, HIT_SOME);
        end;
      end;
  end; // case FState of ...

_end:

// Состояние - Воскрешение:
  if FState = MONSTATE_REVIVE then
    if FAnim[FCurAnim, FDirection].Played then
    begin // Обратная анимация умирания закончилась - идем дальше:
      FAnim[FCurAnim, FDirection].Revert(False);
      SetState(MONSTATE_GO);
    end;

// Если есть анимация огня колдуна - пусть она идет:
  if vilefire <> nil then
    vilefire.Update();

// Состояние - Умирает и текущая анимация проиграна:
  if (FState = MONSTATE_DIE) and
     (FAnim[FCurAnim, FDirection] <> nil) and
     (FAnim[FCurAnim, FDirection].Played) then
    begin
    // Умер:
      SetState(MONSTATE_DEAD);

    // Pain_Elemental при смерти выпускает 3 Lost_Soul'а:
      if (FMonsterType = MONSTER_PAIN) then
      begin
        mon := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-30,
                                 FObj.Y+FObj.Rect.Y+20, TDirection.D_LEFT);
        if mon <> nil then
        begin
          mon.SetState(MONSTATE_GO);
          mon.FNoRespawn := True;
          Inc(gTotalMonsters);
          if g_Game_IsNet then MH_SEND_MonsterSpawn(mon.UID);
        end;

        mon := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                                 FObj.Y+FObj.Rect.Y+20, TDirection.D_RIGHT);
        if mon <> nil then
        begin
          mon.SetState(MONSTATE_GO);
          mon.FNoRespawn := True;
          Inc(gTotalMonsters);
          if g_Game_IsNet then MH_SEND_MonsterSpawn(mon.UID);
        end;

        mon := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-15,
                                 FObj.Y+FObj.Rect.Y, TDirection.D_RIGHT);
        if mon <> nil then
        begin
          mon.SetState(MONSTATE_GO);
          mon.FNoRespawn := True;
          Inc(gTotalMonsters);
          if g_Game_IsNet then MH_SEND_MonsterSpawn(mon.UID);
        end;

        if g_Game_IsNet then MH_SEND_CoopStats();
      end;

    // У этих монстров нет трупов:
      if (FMonsterType = MONSTER_PAIN) or
         (FMonsterType = MONSTER_SOUL) or
         (FMonsterType = MONSTER_BARREL) then
        FRemoved := True;
    end;

// Совершение атаки и стрельбы:
  if (FState = MONSTATE_ATTACK) or (FState = MONSTATE_SHOOT) then
    if (FAnim[FCurAnim, FDirection] <> nil) then
    // Анимация атаки есть - можно атаковать
      if (FAnim[FCurAnim, FDirection].Played) then
        begin // Анимация атаки закончилась => переходим на шаг
          if FState = MONSTATE_ATTACK then
            begin // Состояние - Атака
            // Если монстр не Lost_Soul, то после атаки переходим на шаг:
              if FMonsterType <> MONSTER_SOUL then
                SetState(MONSTATE_GO);
            end
          else // Состояние - Стрельба
            begin
            // Переходим на шаг, если не надо стрелять еще раз:
              if not FChainFire then
                SetState(MONSTATE_GO)
              else
                begin // Надо стрелять еще
                  FChainFire := False;
                // Т.к. состояние не изменилось, и нужен
                // новый цикл ожидания завершения анимации:
                  FAnim[FCurAnim, FDirection].Reset();
                end;
            end;

          FWaitAttackAnim := False;
        end

      else // Анимация атаки еще идет (исключение - Lost_Soul):
        if (FMonsterType = MONSTER_SOUL) or
           ( (not FWaitAttackAnim) and
             (FAnim[FCurAnim, FDirection].CurrentFrame =
              (FAnim[FCurAnim, FDirection].TotalFrames div 2))
           ) then
        begin // Атаки еще не было и это середина анимации атаки
          if FState = MONSTATE_ATTACK then
            begin // Состояние - Атака
            // Если это Lost_Soul, то сбрасываем анимацию атаки:
              if FMonsterType = MONSTER_SOUL then
                FAnim[FCurAnim, FDirection].Reset();

              case FMonsterType of
                MONSTER_SOUL, MONSTER_IMP, MONSTER_DEMON:
                // Грызем первого попавшегося:
                  if g_Weapon_Hit(@FObj, 15, FUID, HIT_SOME) <> 0 then
                  // Lost_Soul укусил кого-то => переходит на шаг:
                    if FMonsterType = MONSTER_SOUL then
                      SetState(MONSTATE_GO);

                MONSTER_FISH:
                // Рыба кусает первого попавшегося со звуком:
                  if g_Weapon_Hit(@FObj, 10, FUID, HIT_SOME) <> 0 then
                    g_Sound_PlayExAt('SOUND_MONSTER_FISH_ATTACK', FObj.X, FObj.Y);

                MONSTER_SKEL, MONSTER_ROBO, MONSTER_CYBER:
                // Робот, кибер или скелет сильно пинаются:
                  if FCurAnim = ANIM_ATTACK2 then
                  begin
                    o := FObj;
                    o.Vel.X := IfThen(FDirection = TDirection.D_RIGHT, 1, -1)*IfThen(FMonsterType = MONSTER_CYBER, 60, 50);
                    if g_Weapon_Hit(@o, IfThen(FMonsterType = MONSTER_CYBER, 33, 50), FUID, HIT_SOME) <> 0 then
                      g_Sound_PlayExAt('SOUND_MONSTER_SKEL_HIT', FObj.X, FObj.Y);
                  end;

                MONSTER_VILE:
                // Колдун пытается воскрешать:
                  if FCurAnim = ANIM_ATTACK2 then
                  begin
                    sx := isCorpse(@FObj, True);
                    if sx <> -1 then
                    begin // Нашли, кого воскресить
                      gMonsters[sx].SetState(MONSTATE_REVIVE);
                      g_Sound_PlayExAt('SOUND_MONSTER_SLOP', FObj.X, FObj.Y);
                    // Воскрешать - себе вредить:
                      {g_Weapon_HitUID(FUID, 5, 0, HIT_SOME);}
                    end;
                  end;
              end;
            end

          else // Состояние - Стрельба
            begin
            // Вычисляем координаты, откуда вылетит пуля:
              wx := MONSTER_ANIMTABLE[FMonsterType].wX;

              if FDirection = TDirection.D_LEFT then
              begin
                wx := MONSTER_ANIMTABLE[FMonsterType].wX-(MONSTERTABLE[FMonsterType].Rect.X+(MONSTERTABLE[FMonsterType].Rect.Width div 2));
                wx := MONSTERTABLE[FMonsterType].Rect.X+(MONSTERTABLE[FMonsterType].Rect.Width div 2)-wx;
              end; // Это значит: wx := hitX + (hitWidth / 2) - (wx - (hitX + (hitWidth / 2)))

              wx := FObj.X + wx;
              wy := FObj.Y + MONSTER_ANIMTABLE[FMonsterType].wY;

            // Монстр не может целиться в объект за спиной, стреляя влево:
              if (FDirection = TDirection.D_LEFT) and (tx > wx) then
              begin
                tx := wx - 32;
                ty := wy + Random(11) - 5;
              end;
            // И аналогично, стреляя вправо:
              if (FDirection = TDirection.D_RIGHT) and (tx < wx) then
              begin
                tx := wx + 32;
                ty := wy + Random(11) - 5;
              end;

            // Делаем выстрел нужным оружием:
              case FMonsterType of
                MONSTER_IMP:
                  g_Weapon_ball1(wx, wy, tx, ty, FUID);
                MONSTER_ZOMBY:
                  begin
                    g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', wx, wy);
                    g_Weapon_gun(wx, wy, tx, ty, 1, 3, FUID, True);
                    g_Player_CreateShell(wx, wy, 0, -2, SHELL_BULLET);
                  end;
                MONSTER_SERG:
                  begin
                    g_Weapon_shotgun(wx, wy, tx, ty, FUID);
                    if not gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', wx, wy);
                    FShellTimer := 10;
                    FShellType := SHELL_SHELL;
                  end;
                MONSTER_MAN:
                  begin
                    g_Weapon_dshotgun(wx, wy, tx, ty, FUID);
                    FShellTimer := 13;
                    FShellType := SHELL_DBLSHELL;
                    FAmmo := -36;
                  end;
                MONSTER_CYBER:
                begin
                  g_Weapon_rocket(wx, wy, tx, ty, FUID);
                  // MH_SEND_MonsterAttack(FUID, wx, wy, tx, ty);
                end;
                MONSTER_SKEL:
                  g_Weapon_revf(wx, wy, tx, ty, FUID, FTargetUID);
                MONSTER_CGUN:
                  begin
                    g_Weapon_mgun(wx, wy, tx, ty, FUID);
                    if not gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', wx, wy);
                    g_Player_CreateShell(wx, wy, 0, -2, SHELL_BULLET);
                  end;
                MONSTER_SPIDER:
                  begin
                    g_Weapon_mgun(wx, wy, tx, ty, FUID);
                    if not gSoundEffectsDF then g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', wx, wy);
                    g_Player_CreateShell(wx, wy, 0, -2, SHELL_SHELL);
                  end;
                MONSTER_BSP:
                  g_Weapon_aplasma(wx, wy, tx, ty, FUID);
                MONSTER_ROBO:
                  g_Weapon_plasma(wx, wy, tx, ty, FUID);
                MONSTER_MANCUB:
                  g_Weapon_manfire(wx, wy, tx, ty, FUID);
                MONSTER_BARON, MONSTER_KNIGHT:
                  g_Weapon_ball7(wx, wy, tx, ty, FUID);
                MONSTER_CACO:
                  g_Weapon_ball2(wx, wy, tx, ty, FUID);
                MONSTER_PAIN:
                  begin // Создаем Lost_Soul:
                    mon := g_Monsters_Create(MONSTER_SOUL, FObj.X+(FObj.Rect.Width div 2),
                                             FObj.Y+FObj.Rect.Y, FDirection);

                    if mon <> nil then
                    begin // Цель - цель Pain_Elemental'а. Летим к ней:
                      mon.FTargetUID := FTargetUID;
                      GetPos(FTargetUID, @o);
                      mon.FTargetTime := 0;
                      mon.FNoRespawn := True;
                      mon.SetState(MONSTATE_GO);
                      mon.shoot(@o, True);
                      Inc(gTotalMonsters);

                      if g_Game_IsNet then MH_SEND_MonsterSpawn(mon.UID);
                    end;
                  end;
              end;

              if FMonsterType <> MONSTER_PAIN then
                if g_Game_IsNet then
                  MH_SEND_MonsterShot(FUID, wx, wy, tx, ty);

            // Скорострельные монстры:
              if (FMonsterType = MONSTER_CGUN) or
                 (FMonsterType = MONSTER_SPIDER) or
                 (FMonsterType = MONSTER_BSP) or
                 (FMonsterType = MONSTER_MANCUB) or
                 (FMonsterType = MONSTER_ROBO) then
                if not GetPos(FTargetUID, @o) then
                // Цель мертва - ищем новую:
                  findNewPrey()
                else // Цель жива - продолжаем стрелять:
                  if shoot(@o, False) then
                    FChainFire := True;
            end;

        // Атака только 1 раз за анимацию атаки:
          FWaitAttackAnim := True;
        end;

// Последний кадр текущей анимации:
  if FAnim[FCurAnim, FDirection].Counter = FAnim[FCurAnim, FDirection].Speed-1 then
    case FState of
      MONSTATE_GO, MONSTATE_RUN, MONSTATE_CLIMB, MONSTATE_RUNOUT:
      // Звуки при передвижении:
        case FMonsterType of
          MONSTER_CYBER:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
              g_Sound_PlayExAt('SOUND_MONSTER_CYBER_WALK', FObj.X, FObj.Y);
          MONSTER_SPIDER:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
              g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_WALK', FObj.X, FObj.Y);
          MONSTER_BSP:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
              g_Sound_PlayExAt('SOUND_MONSTER_BSP_WALK', FObj.X, FObj.Y);
          MONSTER_ROBO:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 5) then
              g_Sound_PlayExAt('SOUND_MONSTER_BSP_WALK', FObj.X, FObj.Y);
        end;
    end;

  if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_LIFTLEFT or PANEL_LIFTRIGHT) and
     not ((FState = MONSTATE_DEAD) or (FState = MONSTATE_DIE))  then
    FObj.Vel.X := oldvelx;

// Если есть анимация, то пусть она идет:
  if FAnim[FCurAnim, FDirection] <> nil then
    FAnim[FCurAnim, FDirection].Update();
end;

procedure TMonster.SetDeadAnim;
begin
  if FAnim <> nil then
    FAnim[FCurAnim, FDirection].CurrentFrame := FAnim[FCurAnim, FDirection].TotalFrames - 1;
end;

procedure TMonster.RevertAnim(R: Boolean = True);
begin
  if FAnim <> nil then
    if FAnim[FCurAnim, FDirection].IsReverse <> R then
      FAnim[FCurAnim, FDirection].Revert(R);
end;

function TMonster.AnimIsReverse: Boolean;
begin
  if FAnim <> nil then
    Result := FAnim[FCurAnim, FDirection].IsReverse
  else
    Result := False;
end;

procedure TMonster.ClientUpdate();
var
  a, b, sx, sy, oldvelx: Integer;
  st: Word;
  o, co: TObj;
  fall, bubbles: Boolean;
label
  _end;
begin
  sx := 0; // SHUT UP COMPILER
  sy := 0;
  fall := True;
  bubbles := True;

// Монстр статичен пока идет warmup
  if (gLMSRespawn > LMS_RESPAWN_NONE) then exit;

// Рыбы "летают" только в воде:
  if FMonsterType = MONSTER_FISH then
    if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
      if (FState <> MONSTATE_DIE) and (FState <> MONSTATE_DEAD) then
        fall := False;

// Летающие монтсры:
  if ((FMonsterType = MONSTER_SOUL) or
      (FMonsterType = MONSTER_PAIN) or
      (FMonsterType = MONSTER_CACO)) and
     (FState <> MONSTATE_DIE) and
     (FState <> MONSTATE_DEAD) then
    fall := False;

// Меняем скорость только по четным кадрам:
  if gTime mod (GAME_TICK*2) <> 0 then
  begin
    g_Obj_Move(@FObj, fall, True, True);
    positionChanged(); // this updates spatial accelerators
    Exit;
  end;

  if FPainTicks > 0 then
    Dec(FPainTicks)
  else
    FPainSound := False;

// Двигаемся:
  st := g_Obj_Move(@FObj, fall, True, True);
  positionChanged(); // this updates spatial accelerators

// Вылетел за карту - удаляем и запускаем триггеры:
  if WordBool(st and MOVE_FALLOUT) or (FObj.X < -1000) or
     (FObj.X > gMapInfo.Width+1000) or (FObj.Y < -1000) then
  begin
    FRemoved := True;
    Exit;
  end;

  oldvelx := FObj.Vel.X;

// Сопротивление воздуха для трупа:
  if (FState = MONSTATE_DIE) or (FState = MONSTATE_DEAD) then
    FObj.Vel.X := z_dec(FObj.Vel.X, 1);

  if FFireTime > 0 then
  begin
    if WordBool(st and MOVE_INWATER) then
      FFireTime := 0
    else
    begin
      OnFireFlame(1);
      FFireTime := FFireTime - 1;
    end;
  end;

// Мертвый ничего не делает:
  if (FState = MONSTATE_DEAD) then
    goto _end;

// Возможно, создаем пузырьки в воде:
  if WordBool(st and MOVE_INWATER) and (Random(32) = 0) then
  begin
    case FMonsterType of
      MONSTER_FISH:
        if Random(4) <> 0 then bubbles := False else
          g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                        FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      MONSTER_ROBO, MONSTER_BARREL:
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                      FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      else
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width-4),
                      FObj.Y+FObj.Rect.Y + Random(4), 5, 4, 4);
    end;
    if bubbles then if Random(2) = 0
      then g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
      else g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
  end;

// Если прошел первый кадр анимации взрыва бочки, то взрыв:
  if FMonsterType = MONSTER_BARREL then
  begin
    if (FState = MONSTATE_DIE) and (FAnim[FCurAnim, FDirection].CurrentFrame = 1) and
       (FAnim[FCurAnim, FDirection].Counter = 0) then
      g_Weapon_Explode(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                       FObj.Y+FObj.Rect.Y+FObj.Rect.Height-16,
                       60, FUID);
  end;

// Lost_Soul вылетел из воды => ускоряется:
  if FMonsterType = MONSTER_SOUL then
    if WordBool(st and MOVE_HITAIR) then
      g_Obj_SetSpeed(@FObj, 16);

  if FAmmo < 0 then
    FAmmo := FAmmo + 1;

// Если начали всплывать, то продолжаем:
  if FObj.Vel.Y < 0 then
    if WordBool(st and MOVE_INWATER) then
      FObj.Vel.Y := -4;

// Таймер - ждем после потери цели:
  FTargetTime := FTargetTime + 1;

  if FShellTimer > -1 then
    if FShellTimer = 0 then
    begin
      if FShellType = SHELL_SHELL then
        g_Player_CreateShell(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                             GameVelX, GameVelY-2, SHELL_SHELL)
      else if FShellType = SHELL_DBLSHELL then
      begin
        g_Player_CreateShell(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                             GameVelX-1, GameVelY-2, SHELL_SHELL);
        g_Player_CreateShell(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
                             GameVelX+1, GameVelY-2, SHELL_SHELL);
      end;
      FShellTimer := -1;
    end else Dec(FShellTimer);

// Пробуем увернуться от летящей пули:
  if fall then
    if (FState in [MONSTATE_GO, MONSTATE_RUN, MONSTATE_RUNOUT,
                   MONSTATE_ATTACK, MONSTATE_SHOOT]) then
      if g_Weapon_Danger(FUID, FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                         FObj.Rect.Width, FObj.Rect.Height, 50) then
        if (g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj)) and
           (FObj.Accel.Y = 0) then
          FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;

  case FState of
    MONSTATE_PAIN: // Состояние - Боль
      begin
      // Боль сильная => монстр кричит:
        if FPain >= MONSTERTABLE[FMonsterType].Pain then
        begin
          FPain := MONSTERTABLE[FMonsterType].Pain;
          if gSoundEffectsDF then PainSound();
        end;
        if (not gSoundEffectsDF) and (FPain >= MONSTERTABLE[FMonsterType].MinPain) then
          PainSound();

      // Снижаем боль со временем:
        FPain := FPain - 5;

      // Боль уже не ошутимая => идем дальше:
        if FPain <= MONSTERTABLE[FMonsterType].MinPain then
        begin
          SetState(MONSTATE_GO);
          FPain := 0;
        end;
      end;

    MONSTATE_SLEEP: // Состояние - Сон
      begin
      // Спим:
        FSleep := FSleep + 1;

      // Проспали достаточно:
        if FSleep >= 18 then
          FSleep := 0
        else // еще спим
          goto _end;
      end;

    MONSTATE_WAIT: // Состояние - Ожидание
      begin
      // Ждем:
        FSleep := FSleep - 1;
      end;

    MONSTATE_GO: // Состояние - Движение (с осмотром ситуации)
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          Turn();
          FSleep := 40;
          SetState(MONSTATE_RUNOUT);

          goto _end;
        end;

      // Если монстр - колдун, то пробуем воскресить кого-нибудь:
        if (FMonsterType = MONSTER_VILE) then
          if isCorpse(@FObj, False) <> -1 then
          begin
            SetState(MONSTATE_ATTACK, ANIM_ATTACK2);
            FObj.Vel.X := 0;

            goto _end;
          end;

      // Если цель почти на одной вертикали - бегаем туда-сюда:
        if Abs(sx) < 40 then
          if FMonsterType <> MONSTER_FISH then
          begin
            SetState(MONSTATE_RUN);
            FSleep := 15;

            goto _end;
          end;

      // Уперлись в стену:
        if WordBool(st and MOVE_HITWALL) then
        begin
          case FMonsterType of
            MONSTER_CACO, MONSTER_SOUL, MONSTER_PAIN, MONSTER_FISH: ;
            else // Не летают:
              if (g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj)) and
                 (FObj.Accel.Y = 0) then
              begin // Стоим на твердом полу или ступени
              // Прыжок через стену:
                FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;
                SetState(MONSTATE_CLIMB);
              end;
          end;

          goto _end;
        end;

      // Монстры, не подверженные гравитации:
        if (FMonsterType = MONSTER_CACO) or (FMonsterType = MONSTER_SOUL) or
           (FMonsterType = MONSTER_PAIN) or (FMonsterType = MONSTER_FISH) then
          begin
            if FMonsterType = MONSTER_FISH then
              begin // Рыба
                if not WordBool(st and MOVE_INWATER) then
                  begin // Рыба вне воды:
                    if g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj) then
                    begin // "Стоит" твердо
                    // Рыба трепыхается на поверхности:
                      if FObj.Accel.Y = 0 then
                        FObj.Vel.Y := -6;
                      FObj.Accel.X := FObj.Accel.X - 8 + Random(17);
                    end;

                  // Рыбе больно:
                    SetState(MONSTATE_PAIN);
                    FPain := FPain + 50;
                  end
                else // Рыба в воде
                  begin
                  // Плывем в сторону цели по-вертикали:
                    if Abs(sy) > 8 then
                      FObj.Vel.Y := g_basic.Sign(sy)*4
                    else
                      FObj.Vel.Y := 0;

                  // Рыба плывет вверх:
                    if FObj.Vel.Y < 0 then
                      if not g_Obj_CollideLiquid(@FObj, 0, -16) then
                      begin
                      // Всплыли до поверхности - стоп:
                        FObj.Vel.Y := 0;
                      // Плаваем туда-сюда:
                        SetState(MONSTATE_RUN);
                        FSleep := 20;
                      end;
                   end;
              end
            else // Летающие монстры
              begin
              // Летим в сторону цели по-вертикали:
                if Abs(sy) > 8 then
                  FObj.Vel.Y := g_basic.Sign(sy)*4
                else
                  FObj.Vel.Y := 0;
              end;
          end
        else // "Наземные" монстры
          begin
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
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61)+120); // налево
                  end
                  else
                  begin
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61));    // направо
                  end;
                  positionChanged(); // this updates spatial accelerators
                end;
              end;
            end;
          // Боссы могут пинать трупы:
            if (FMonsterType in [MONSTER_CYBER, MONSTER_SPIDER, MONSTER_ROBO]) and
               (FObj.Vel.X <> 0) and (gCorpses <> nil) then
            begin
              b := Abs(FObj.Vel.X);
              if b > 1 then b := b * (Random(8 div b) + 1);
              for a := 0 to High(gCorpses) do
                if (gCorpses[a] <> nil) and (gCorpses[a].State > 0) then
                begin
                  co := gCorpses[a].Obj;
                  if g_Obj_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y+FObj.Rect.Height-4,
                                   FObj.Rect.Width, 8, @co) and (Random(3) = 0) then
                    // Пинаем трупы
                    if FObj.Vel.X < 0 then
                      gCorpses[a].Damage(b*2, FUID, -b, Random(7)) // налево
                    else
                      gCorpses[a].Damage(b*2, FUID, b, Random(7)); // направо
                end;
            end;
          end;

        FSleep := FSleep + 1;

      // Иногда рычим:
        if FSleep >= 8 then
        begin
          FSleep := 0;
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_RUN: // Состояние - Бег
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          SetState(MONSTATE_RUNOUT);
          FSleep := 40;

          goto _end;
        end;

        FSleep := FSleep - 1;

      // Пробежали достаточно или врезались в стену => переходим на шаг:
        if (FSleep <= 0) or (WordBool(st and MOVE_HITWALL) and ((FObj.Vel.Y+FObj.Accel.Y) = 0)) then
        begin
          SetState(MONSTATE_GO);
          FSleep := 0;

        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_RUNOUT: // Состояние - Убегает от чего-то
      begin
      // Вышли из БлокМона:
        if (not WordBool(st and MOVE_BLOCK)) and (FSleep > 0) then
          FSleep := 0;

        FSleep := FSleep - 1;

      // Убажели достаточно далеко => переходим на шаг:
        if FSleep <= -18 then
        begin
          SetState(MONSTATE_GO);
          FSleep := 0;

        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_CLIMB: // Состояние - Прыжок (чтобы обойти стену)
      begin
      // Достигли высшей точки прыжка или стена кончилась => переходим на шаг:
        if ((FObj.Vel.Y+FObj.Accel.Y) >= 0) or
           (not WordBool(st and MOVE_HITWALL)) then
        begin
          SetState(MONSTATE_GO);
          FSleep := 0;

        // Стена не кончилась => бежим от нее:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
          begin
            SetState(MONSTATE_RUN);
            FSleep := 15;
          end;
        end;

      // Бежим в выбранную сторону:
        if FDirection = TDirection.D_RIGHT then
          FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
        else
          FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

      // Если в воде, то замедляемся:
        if WordBool(st and MOVE_INWATER) then
          FObj.Vel.X := FObj.Vel.X div 2
        else // Рыбам не нужно замедляться
          if FMonsterType = MONSTER_FISH then
            FObj.Vel.X := 0;
      end;

    MONSTATE_ATTACK, // Состояние - Атака
    MONSTATE_SHOOT:  // Состояние - Стрельба
      begin
      // Lost_Soul врезался в стену при атаке => переходит на шаг:
        if FMonsterType = MONSTER_SOUL then
        begin
          if WordBool(st and (MOVE_HITWALL or MOVE_HITCEIL or MOVE_HITLAND)) then
            SetState(MONSTATE_GO);

          goto _end;
        end;

      // Замедляемся при атаке:
        if FMonsterType <> MONSTER_FISH then
          FObj.Vel.X := z_dec(FObj.Vel.X, 1);

      // Нужно стрелять, а монстр - колдун:
        if (FMonsterType = MONSTER_VILE) and (FState = MONSTATE_SHOOT) then
        begin
        // Цель погибла => идем дальше:
          if not GetPos(FTargetUID, @o) then
          begin
            SetState(MONSTATE_GO);

            goto _end;
          end;

        // Цель не видно => идем дальше:
          if not g_Look(@FObj, @o, FDirection) then
          begin
            SetState(MONSTATE_GO);

            goto _end;
          end;

        // Цель в воде - не загорится => идем дальше:
          if g_Obj_CollideWater(@o, 0, 0) then
          begin
            SetState(MONSTATE_GO);

            goto _end;
          end;
        end;
      end;
  end; // case FState of ...

_end:

// Состояние - Воскрешение:
  if FState = MONSTATE_REVIVE then
    if FAnim[FCurAnim, FDirection].Played then
    begin // Обратная анимация умирания закончилась - идем дальше:
      FAnim[FCurAnim, FDirection].Revert(False);
      SetState(MONSTATE_GO);
    end;

// Если есть анимация огня колдуна - пусть она идет:
  if vilefire <> nil then
    vilefire.Update();

// Состояние - Умирает и текущая анимация проиграна:
  if (FState = MONSTATE_DIE) and
     (FAnim[FCurAnim, FDirection] <> nil) and
     (FAnim[FCurAnim, FDirection].Played) then
    begin
    // Умер:
      SetState(MONSTATE_DEAD);

    // У этих монстров нет трупов:
      if (FMonsterType = MONSTER_PAIN) or
         (FMonsterType = MONSTER_SOUL) or
         (FMonsterType = MONSTER_BARREL) then
        FRemoved := True
      else
        FAnim[FCurAnim, FDirection].CurrentFrame := FAnim[FCurAnim, FDirection].TotalFrames - 1;
    end;

// Совершение атаки и стрельбы:
  if (FState = MONSTATE_ATTACK) or (FState = MONSTATE_SHOOT) then
    if (FAnim[FCurAnim, FDirection] <> nil) then
    // Анимация атаки есть - можно атаковать
      if (FAnim[FCurAnim, FDirection].Played) then
        begin // Анимация атаки закончилась => переходим на шаг
          if FState = MONSTATE_ATTACK then
            begin // Состояние - Атака
            // Если монстр не Lost_Soul, то после атаки переходим на шаг:
              if FMonsterType <> MONSTER_SOUL then
                SetState(MONSTATE_GO);
            end
          else // Состояние - Стрельба
            begin
            // Переходим на шаг, если не надо стрелять еще раз:
              if not FChainFire then
                SetState(MONSTATE_GO)
              else
                begin // Надо стрелять еще
                  FChainFire := False;
                // Т.к. состояние не изменилось, и нужен
                // новый цикл ожидания завершения анимации:
                  FAnim[FCurAnim, FDirection].Reset();
                end;
            end;

          FWaitAttackAnim := False;
        end

      else // Анимация атаки еще идет (исключение - Lost_Soul):
        if (FMonsterType = MONSTER_SOUL) or
           ( (not FWaitAttackAnim) and
             (FAnim[FCurAnim, FDirection].CurrentFrame =
              (FAnim[FCurAnim, FDirection].TotalFrames div 2))
           ) then
        begin // Атаки еще не было и это середина анимации атаки
          if FState = MONSTATE_ATTACK then
            begin // Состояние - Атака
            // Если это Lost_Soul, то сбрасываем анимацию атаки:
              if FMonsterType = MONSTER_SOUL then
                FAnim[FCurAnim, FDirection].Reset();

              case FMonsterType of
                MONSTER_SOUL, MONSTER_IMP, MONSTER_DEMON:
                // Грызем первого попавшегося:
                  if g_Weapon_Hit(@FObj, 15, FUID, HIT_SOME) <> 0 then
                  // Lost_Soul укусил кого-то => переходит на шаг:
                    if FMonsterType = MONSTER_SOUL then
                      SetState(MONSTATE_GO);

                MONSTER_FISH:
                  g_Weapon_Hit(@FObj, 10, FUID, HIT_SOME);

                MONSTER_SKEL, MONSTER_ROBO, MONSTER_CYBER:
                // Робот, кибер или скелет сильно пинаются:
                  if FCurAnim = ANIM_ATTACK2 then
                  begin
                    o := FObj;
                    o.Vel.X := IfThen(FDirection = TDirection.D_RIGHT, 1, -1)*IfThen(FMonsterType = MONSTER_CYBER, 60, 50);
                    g_Weapon_Hit(@o, IfThen(FMonsterType = MONSTER_CYBER, 33, 50), FUID, HIT_SOME);
                  end;

                MONSTER_VILE:
                // Колдун пытается воскрешать:
                  if FCurAnim = ANIM_ATTACK2 then
                  begin
                    sx := isCorpse(@FObj, True);
                    if sx <> -1 then
                    begin // Нашли, кого воскресить
                      g_Sound_PlayExAt('SOUND_MONSTER_SLOP', FObj.X, FObj.Y);
                    // Воскрешать - себе вредить:
                      {g_Weapon_HitUID(FUID, 5, 0, HIT_SOME);}
                    end;
                  end;
              end;
            end

          else // Состояние - Стрельба
            begin
            // Скорострельные монстры:
              if (FMonsterType = MONSTER_CGUN) or
                 (FMonsterType = MONSTER_SPIDER) or
                 (FMonsterType = MONSTER_BSP) or
                 (FMonsterType = MONSTER_MANCUB) or
                 (FMonsterType = MONSTER_ROBO) then
                if not GetPos(FTargetUID, @o) then
                // Цель мертва - ищем новую:
                  findNewPrey()
                else // Цель жива - продолжаем стрелять:
                  if shoot(@o, False) then
                    FChainFire := True;
            end;

        // Атака только 1 раз за анимацию атаки:
          FWaitAttackAnim := True;
        end;

// Последний кадр текущей анимации:
  if FAnim[FCurAnim, FDirection].Counter = FAnim[FCurAnim, FDirection].Speed-1 then
    case FState of
      MONSTATE_GO, MONSTATE_RUN, MONSTATE_CLIMB, MONSTATE_RUNOUT:
      // Звуки при передвижении:
        case FMonsterType of
          MONSTER_CYBER:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
              g_Sound_PlayExAt('SOUND_MONSTER_CYBER_WALK', FObj.X, FObj.Y);
          MONSTER_SPIDER:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
              g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_WALK', FObj.X, FObj.Y);
          MONSTER_BSP:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
              g_Sound_PlayExAt('SOUND_MONSTER_BSP_WALK', FObj.X, FObj.Y);
          MONSTER_ROBO:
            if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
               (FAnim[FCurAnim, FDirection].CurrentFrame = 5) then
              g_Sound_PlayExAt('SOUND_MONSTER_BSP_WALK', FObj.X, FObj.Y);
        end;
    end;

// Костыль для потоков
  if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_LIFTLEFT or PANEL_LIFTRIGHT) and
     not ((FState = MONSTATE_DEAD) or (FState = MONSTATE_DIE))  then
    FObj.Vel.X := oldvelx;

// Если есть анимация, то пусть она идет:
  if FAnim[FCurAnim, FDirection] <> nil then
    FAnim[FCurAnim, FDirection].Update();
end;

procedure TMonster.ClientAttack(wx, wy, atx, aty: Integer);
begin
  case FMonsterType of
    MONSTER_ZOMBY:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', wx, wy);
      g_Player_CreateShell(wx, wy, 0, -2, SHELL_BULLET);
    end;
    MONSTER_SERG:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN', wx, wy);
      FShellTimer := 10;
      FShellType := SHELL_SHELL;
    end;
    MONSTER_MAN:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRESHOTGUN2', wx, wy);
      FShellTimer := 13;
      FShellType := SHELL_DBLSHELL;
    end;
    MONSTER_CGUN, MONSTER_SPIDER:
    begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIRECGUN', wx, wy);
      g_Player_CreateShell(wx, wy, 0, -2, SHELL_BULLET);
    end;
    MONSTER_IMP:
      g_Weapon_ball1(wx, wy, atx, aty, FUID);
    MONSTER_CYBER:
      g_Weapon_rocket(wx, wy, atx, aty, FUID);
    MONSTER_SKEL:
      g_Weapon_revf(wx, wy, atx, aty, FUID, FTargetUID);
    MONSTER_BSP:
      g_Weapon_aplasma(wx, wy, atx, aty, FUID);
    MONSTER_ROBO:
      g_Weapon_plasma(wx, wy, atx, aty, FUID);
    MONSTER_MANCUB:
      g_Weapon_manfire(wx, wy, atx, aty, FUID);
    MONSTER_BARON, MONSTER_KNIGHT:
      g_Weapon_ball7(wx, wy, atx, aty, FUID);
    MONSTER_CACO:
      g_Weapon_ball2(wx, wy, atx, aty, FUID);
  end;
end;

procedure TMonster.Turn();
begin
// Разворачиваемся:
  if FDirection = TDirection.D_LEFT then FDirection := TDirection.D_RIGHT else FDirection := TDirection.D_LEFT;

// Бежим в выбранную сторону:
  if FDirection = TDirection.D_RIGHT then
    FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
  else
    FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;
end;

function TMonster.findNewPrey(): Boolean;
var
  a: DWORD;
  l, l2: Integer;
  PlayersSee, MonstersSee: Array of DWORD;
  PlayerNear, MonsterNear: Integer;
begin
  Result := False;
  SetLength(MonstersSee, 0);
  SetLength(PlayersSee, 0);

  FTargetUID := 0;
  l := 32000;
  PlayerNear := -1;
  MonsterNear := -1;

  // Поехавшие, каннибалы, и добрые игроков не трогают
  if (gPlayers <> nil) and (FBehaviour <> BH_INSANE) and
  (FBehaviour <> BH_CANNIBAL) and (FBehaviour <> BH_GOOD) then
    for a := 0 to High(gPlayers) do
      if (gPlayers[a] <> nil) and (gPlayers[a].alive)
      and (not gPlayers[a].NoTarget) and (gPlayers[a].FMegaRulez[MR_INVIS] < gTime) then
      begin
        if g_Look(@FObj, @gPlayers[a].Obj, FDirection) then
        begin
          SetLength(PlayersSee, Length(PlayersSee) + 1);
          PlayersSee[High(PlayersSee)] := a;
        end;
        l2 := Abs(gPlayers[a].GameX-FObj.X)+
              Abs(gPlayers[a].GameY-FObj.Y);
        if l2 < l then
        begin
          l := l2;
          PlayerNear := Integer(a);
        end;
      end;

  // Киллеры и добрые не трогают монстров
  if (gMonsters <> nil) and (FBehaviour <> BH_KILLER) and (FBehaviour <> BH_GOOD) then
    for a := 0 to High(gMonsters) do
      if (gMonsters[a] <> nil) and (gMonsters[a].alive) and
         (gMonsters[a].FUID <> FUID) then
      begin
        if (FBehaviour = BH_CANNIBAL) and (gMonsters[a].FMonsterType <> FMonsterType) then
          Continue; // Каннибалы атакуют только сородичей
        if (FBehaviour = BH_INSANE) and (gMonsters[a].FMonsterType <> MONSTER_BARREL)
        and (IsFriend(gMonsters[a].FMonsterType, FMonsterType)) then
          Continue; // Поехавшие не трогают друзей, но им не нравятся бочки
        if ((FBehaviour = BH_NORMAL) or (FBehaviour = BH_MANIAC))
        and (IsFriend(gMonsters[a].FMonsterType, FMonsterType)) then
          Continue; // Оставшиеся типы, кроме каннибалов, не трогают своих друзей

        if g_Look(@FObj, @gMonsters[a].Obj, FDirection) then
        begin
          SetLength(MonstersSee, Length(MonstersSee) + 1);
          MonstersSee[High(MonstersSee)] := a;
        end;
        l2 := Abs(gMonsters[a].FObj.X-FObj.X)+
              Abs(gMonsters[a].FObj.Y-FObj.Y);
        if l2 < l then
        begin
          l := l2;
          MonsterNear := Integer(a);
        end;
      end;

  case FBehaviour of
    BH_NORMAL, BH_KILLER:
    begin
      // Обычный и киллер сначала ищут игроков в поле зрения
      if (FTargetUID = 0) and (Length(PlayersSee) > 0) then
      begin
        a := PlayersSee[Random(Length(PlayersSee))];
        FTargetUID := gPlayers[a].UID;
      end;
      // Затем поблизости
      if (FTargetUID = 0) and (PlayerNear > -1) then
      begin
        a := PlayerNear;
        FTargetUID := gPlayers[a].UID;
      end;
      // Потом обычные ищут монстров в поле зрения
      if (FTargetUID = 0) and (Length(MonstersSee) > 0) then
      begin
        a := MonstersSee[Random(Length(MonstersSee))];
        FTargetUID := gMonsters[a].UID;
      end;
      // Затем поблизости
      if (FTargetUID = 0) and (MonsterNear > -1) then
      begin
        a := MonsterNear;
        FTargetUID := gMonsters[a].UID;
      end;
    end;
    BH_MANIAC, BH_INSANE, BH_CANNIBAL:
    begin
      // Маньяки, поехавшие и каннибалы сначала истребляют всё в поле зрения
      if (FTargetUID = 0) and (Length(PlayersSee) > 0) then
      begin
        a := PlayersSee[Random(Length(PlayersSee))];
        FTargetUID := gPlayers[a].UID;
      end;
      if (FTargetUID = 0) and (Length(MonstersSee) > 0) then
      begin
        a := MonstersSee[Random(Length(MonstersSee))];
        FTargetUID := gMonsters[a].UID;
      end;
      // Затем ищут кого-то поблизости
      if (FTargetUID = 0) and (PlayerNear > -1) then
      begin
        a := PlayerNear;
        FTargetUID := gPlayers[a].UID;
      end;
      if (FTargetUID = 0) and (MonsterNear > -1) then
      begin
        a := MonsterNear;
        FTargetUID := gMonsters[a].UID;
      end;
    end;
  end;

// Если и монстров нет - начинаем ждать цель:
  if FTargetUID = 0 then
    begin
      // Поехавший пытается самоубиться
      if FBehaviour = BH_INSANE then
        FTargetUID := FUID
      else
        FTargetTime := MAX_ATM;
    end
  else
    begin // Цель нашли
      FTargetTime := 0;
      Result := True;
    end;
end;

function TMonster.kick(o: PObj): Boolean;
begin
  Result := False;

  case FMonsterType of
    MONSTER_FISH:
      begin
        SetState(MONSTATE_ATTACK);
        Result := True;
      end;
    MONSTER_DEMON:
      begin
        SetState(MONSTATE_ATTACK);
        g_Sound_PlayExAt('SOUND_MONSTER_DEMON_ATTACK', FObj.X, FObj.Y);
        Result := True;
      end;
    MONSTER_IMP:
      begin
        SetState(MONSTATE_ATTACK);
        g_Sound_PlayExAt('SOUND_MONSTER_IMP_ATTACK', FObj.X, FObj.Y);
        Result := True;
      end;
    MONSTER_SKEL, MONSTER_ROBO, MONSTER_CYBER:
      begin
        SetState(MONSTATE_ATTACK, ANIM_ATTACK2);
        g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ATTACK', FObj.X, FObj.Y);
        Result := True;
      end;
    MONSTER_BARON, MONSTER_KNIGHT,
    MONSTER_CACO, MONSTER_MANCUB:
    // Эти монстры не пинают - они стреляют в упор:
      if not g_Game_IsClient then Result := shoot(o, True);
  end;
end;

function TMonster.shoot(o: PObj; immediately: Boolean): Boolean;
var
  xd, yd, m: Integer;
begin
  Result := False;

// Стрелять рано:
  if FAmmo < 0 then
    Exit;

// Ждать времени готовности к выстрелу:
  if not immediately then
    case FMonsterType of
      MONSTER_FISH, MONSTER_BARREL, MONSTER_DEMON:
        Exit; // не стреляют
      MONSTER_CGUN, MONSTER_BSP, MONSTER_ROBO:
        begin
          FAmmo := FAmmo + 1;
        // Время выстрела упущено:
          if FAmmo >= 50 then
            FAmmo := IfThen(FMonsterType = MONSTER_ROBO, -200, -50);
        end;
      MONSTER_MAN: ;
      MONSTER_MANCUB:
        begin
          FAmmo := FAmmo + 1;
        // Время выстрела упущено:
          if FAmmo >= 5 then
            FAmmo := -50;
        end;
      MONSTER_SPIDER:
        begin
          FAmmo := FAmmo + 1;
        // Время выстрела упущено:
          if FAmmo >= 100 then
            FAmmo := -50;
        end;
      MONSTER_CYBER:
        begin
        // Стреляет не всегда:
          if Random(2) = 0 then
            Exit;
          FAmmo := FAmmo + 1;
        // Время выстрела упущено:
          if FAmmo >= 10 then
            FAmmo := -50;
        end;
      MONSTER_BARON, MONSTER_KNIGHT: if Random(8) <> 0 then Exit;
      MONSTER_SKEL: if Random(32) <> 0 then Exit;
      MONSTER_VILE: if Random(8) <> 0 then Exit;
      MONSTER_PAIN: if Random(8) <> 0 then Exit;
      else if Random(16) <> 0 then Exit;
    end;

// Цели не видно:
  if not g_Look(@FObj, o, FDirection) then
    Exit;

  FTargetTime := 0;

  tx := o^.X+o^.Rect.X+(o^.Rect.Width div 2)+((o^.Vel.X{+o^.Accel.X})*12);
  ty := o^.Y+o^.Rect.Y+(o^.Rect.Height div 2)+((o^.Vel.Y{+o^.Accel.Y})*12);

// Разница по высоте больше разницы по горизонтали
// (не может стрелять под таким большим углом):
  if Abs(tx-(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2))) <
     Abs(ty-(FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2))) then
    Exit;

  case FMonsterType of
    MONSTER_IMP, MONSTER_BARON, MONSTER_KNIGHT, MONSTER_CACO:
      begin
        SetState(MONSTATE_SHOOT);
        {nn}
      end;
    MONSTER_SKEL:
      begin
        SetState(MONSTATE_SHOOT);
        {nn}
      end;
    MONSTER_VILE:
      begin // Зажигаем огонь
        tx := o^.X+o^.Rect.X+(o^.Rect.Width div 2);
        ty := o^.Y+o^.Rect.Y;
        SetState(MONSTATE_SHOOT);

        vilefire.Reset();

        g_Sound_PlayExAt('SOUND_MONSTER_VILE_ATTACK', FObj.X, FObj.Y);
        g_Sound_PlayExAt('SOUND_FIRE', o^.X, o^.Y);
      end;
    MONSTER_SOUL:
      begin // Летит в сторону цели:
        SetState(MONSTATE_ATTACK);
        g_Sound_PlayExAt('SOUND_MONSTER_SOUL_ATTACK', FObj.X, FObj.Y);

        xd := tx-(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2));
        yd := ty-(FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
        m := Max(Abs(xd), Abs(yd));
        if m = 0 then
          m := 1;

        FObj.Vel.X := (xd*16) div m;
        FObj.Vel.Y := (yd*16) div m;
      end;
    MONSTER_MANCUB, MONSTER_ZOMBY, MONSTER_SERG, MONSTER_BSP, MONSTER_ROBO,
    MONSTER_CYBER, MONSTER_CGUN, MONSTER_SPIDER, MONSTER_PAIN, MONSTER_MAN:
      begin
      // Манкубус рявкает перед первой атакой:
        if FMonsterType = MONSTER_MANCUB then
          if FAmmo = 1 then
            g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_ATTACK', FObj.X, FObj.Y);

        SetState(MONSTATE_SHOOT);
      end;
    else Exit;
  end;

  Result := True;
end;

function TMonster.alive(): Boolean;
begin
  Result := (FState <> MONSTATE_DIE) and (FState <> MONSTATE_DEAD) and (FHealth > 0);
end;

procedure TMonster.SetHealth(aH: Integer);
begin
  if (aH > 0) and (aH < 1000000) then
  begin
    FHealth := aH;
    if FHealth > FMaxHealth then
      FMaxHealth := FHealth;
  end;
end;

procedure TMonster.WakeUp();
begin
  if g_Game_IsClient then Exit;
  SetState(MONSTATE_GO);
  FTargetTime := MAX_ATM;
  WakeUpSound();
end;

procedure TMonster.SaveState (st: TStream);
var
  i: Integer;
  b: Byte;
  anim: Boolean;
begin
  assert(st <> nil);

  // Сигнатура монстра:
  utils.writeSign(st, 'MONS');
  utils.writeInt(st, Byte(0)); // version
  // UID монстра:
  utils.writeInt(st, Word(FUID));
  // Направление
  if FDirection = TDirection.D_LEFT then b := 1 else b := 2; // D_RIGHT
  utils.writeInt(st, Byte(b));
  // Надо ли удалить его
  utils.writeBool(st, FRemoved);
  // Осталось здоровья
  utils.writeInt(st, LongInt(FHealth));
  // Состояние
  utils.writeInt(st, Byte(FState));
  // Текущая анимация
  utils.writeInt(st, Byte(FCurAnim));
  // UID цели
  utils.writeInt(st, Word(FTargetUID));
  // Время после потери цели
  utils.writeInt(st, LongInt(FTargetTime));
  // Поведение монстра
  utils.writeInt(st, Byte(FBehaviour));
  // Готовность к выстрелу
  utils.writeInt(st, LongInt(FAmmo));
  // Боль
  utils.writeInt(st, LongInt(FPain));
  // Время ожидания
  utils.writeInt(st, LongInt(FSleep));
  // Озвучивать ли боль
  utils.writeBool(st, FPainSound);
  // Была ли атака во время анимации атаки
  utils.writeBool(st, FWaitAttackAnim);
  // Надо ли стрелять на следующем шаге
  utils.writeBool(st, FChainFire);
  // Подлежит ли респавну
  utils.writeBool(st, FNoRespawn);
  // Координаты цели
  utils.writeInt(st, LongInt(tx));
  utils.writeInt(st, LongInt(ty));
  // ID монстра при старте карты
  utils.writeInt(st, LongInt(FStartID));
  // Индекс триггера, создавшего монстра
  utils.writeInt(st, LongInt(FSpawnTrigger));
  // Объект монстра
  Obj_SaveState(st, @FObj);
  // Есть ли анимация огня колдуна
  anim := (vilefire <> nil);
  utils.writeBool(st, anim);
  // Если есть - сохраняем:
  if anim then vilefire.SaveState(st);
  // Анимации
  for i := ANIM_SLEEP to ANIM_PAIN do
  begin
    // Есть ли левая анимация
    anim := (FAnim[i, TDirection.D_LEFT] <> nil);
    utils.writeBool(st, anim);
    // Если есть - сохраняем
    if anim then FAnim[i, TDirection.D_LEFT].SaveState(st);
    // Есть ли правая анимация
    anim := (FAnim[i, TDirection.D_RIGHT] <> nil);
    utils.writeBool(st, anim);
    // Если есть - сохраняем
    if anim then FAnim[i, TDirection.D_RIGHT].SaveState(st);
  end;
end;


procedure TMonster.LoadState (st: TStream);
var
  i: Integer;
  b: Byte;
  anim: Boolean;
begin
  assert(st <> nil);

  // Сигнатура монстра:
  if not utils.checkSign(st, 'MONS') then raise XStreamError.Create('invalid monster signature');
  if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid monster version');
  if (uidMap[FUID] <> nil) and (uidMap[FUID] <> self) then raise Exception.Create('internal error in monster loader (0)');
  uidMap[FUID] := nil;
  // UID монстра:
  FUID := utils.readWord(st);
  //if (arrIdx = -1) then raise Exception.Create('internal error in monster loader');
  if (uidMap[FUID] <> nil) then raise Exception.Create('internal error in monster loader (1)');
  uidMap[FUID] := self;
  // Направление
  b := utils.readByte(st);
  if b = 1 then FDirection := TDirection.D_LEFT else FDirection := TDirection.D_RIGHT; // b = 2
  // Надо ли удалить его
  FRemoved := utils.readBool(st);
  // Осталось здоровья
  FHealth := utils.readLongInt(st);
  // Состояние
  FState := utils.readByte(st);
  // Текущая анимация
  FCurAnim := utils.readByte(st);
  // UID цели
  FTargetUID := utils.readWord(st);
  // Время после потери цели
  FTargetTime := utils.readLongInt(st);
  // Поведение монстра
  FBehaviour := utils.readByte(st);
  // Готовность к выстрелу
  FAmmo := utils.readLongInt(st);
  // Боль
  FPain := utils.readLongInt(st);
  // Время ожидания
  FSleep := utils.readLongInt(st);
  // Озвучивать ли боль
  FPainSound := utils.readBool(st);
  // Была ли атака во время анимации атаки
  FWaitAttackAnim := utils.readBool(st);
  // Надо ли стрелять на следующем шаге
  FChainFire := utils.readBool(st);
  // Подлежит ли респавну
  FNoRespawn := utils.readBool(st);
  // Координаты цели
  tx := utils.readLongInt(st);
  ty := utils.readLongInt(st);
  // ID монстра при старте карты
  FStartID := utils.readLongInt(st);
  // Индекс триггера, создавшего монстра
  FSpawnTrigger := utils.readLongInt(st);
  // Объект монстра
  Obj_LoadState(@FObj, st);
  // Есть ли анимация огня колдуна
  anim := utils.readBool(st);
  // Если есть - загружаем:
  if anim then
  begin
    Assert(vilefire <> nil, 'TMonster.LoadState: no vilefire anim');
    vilefire.LoadState(st);
  end;
  // Анимации
  for i := ANIM_SLEEP to ANIM_PAIN do
  begin
    // Есть ли левая анимация
    anim := utils.readBool(st);
    // Если есть - загружаем
    if anim then
    begin
      Assert(FAnim[i, TDirection.D_LEFT] <> nil, 'TMonster.LoadState: no '+IntToStr(i)+'_left anim');
      FAnim[i, TDirection.D_LEFT].LoadState(st);
    end;
    // Есть ли правая анимация
     anim := utils.readBool(st);
    // Если есть - загружаем
    if anim then
    begin
      Assert(FAnim[i, TDirection.D_RIGHT] <> nil, 'TMonster.LoadState: no '+IntToStr(i)+'_right anim');
      FAnim[i, TDirection.D_RIGHT].LoadState(st);
    end;
  end;
  // update cache
  self.positionChanged
end;


procedure TMonster.ActivateTriggers();
var
  a: Integer;
begin
  if FDieTriggers <> nil then
    for a := 0 to High(FDieTriggers) do
      g_Triggers_Press(FDieTriggers[a], ACTIVATE_MONSTERPRESS);
  if FSpawnTrigger > -1 then
  begin
    g_Triggers_DecreaseSpawner(FSpawnTrigger);
    FSpawnTrigger := -1;
  end;
end;

procedure TMonster.AddTrigger(t: Integer);
begin
  SetLength(FDieTriggers, Length(FDieTriggers)+1);
  FDieTriggers[High(FDieTriggers)] := t;
end;

procedure TMonster.ClearTriggers();
begin
  SetLength(FDieTriggers, 0);
end;

procedure TMonster.CatchFire(Attacker: Word; Timeout: Integer = MON_BURN_TIME);
begin
  if FMonsterType in [MONSTER_SOUL, MONSTER_VILE] then
    exit; // арчи не горят, черепа уже горят
  if Timeout <= 0 then exit;
  if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
    exit; // не подгораем в воде на всякий случай
  if FFireTime <= 0 then
    g_Sound_PlayExAt('SOUND_IGNITE', FObj.X, FObj.Y);
  FFireTime := Timeout;
  FFireAttacker := Attacker;
  if g_Game_IsNet and g_Game_IsServer then MH_SEND_MonsterState(FUID);
end;

procedure TMonster.OnFireFlame(Times: DWORD = 1);
  var i: DWORD; x, y: Integer;
begin
  if (Random(10) = 1) and (Times = 1) then
    Exit;

  for i := 1 to Times do
  begin
    x := Obj.X + Obj.Rect.X + Random(Obj.Rect.Width + Times * 2) - (R_GFX_FLAME_WIDTH div 2);
    y := Obj.Y + 8 + Random(8 + Times * 2) + IfThen(FState = MONSTATE_DEAD, 16, 0);
    r_GFX_OnceAnim(R_GFX_FLAME, x, y);
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
// throws on invalid uid
function g_Mons_ByIdx (uid: Integer): TMonster; inline;
begin
  result := g_Mons_ByIdx_NC(uid);
  if (result = nil) then raise Exception.Create('g_Mons_ByIdx: invalid monster id');
end;

// can return null
function g_Mons_ByIdx_NC (uid: Integer): TMonster; inline;
begin
  if (uid < 0) or (uid > High(gMonsters)) then begin result := nil; exit; end;
  result := gMonsters[uid];
end;

function g_Mons_TotalCount (): Integer; inline;
begin
  result := Length(gMonsters);
end;


function g_Mons_ForEach (cb: TEachMonsterCB): Boolean;
var
  idx: Integer;
  mon: TMonster;
begin
  result := false;
  if (gMonsters = nil) or not assigned(cb) then exit;
  for idx := 0 to High(gMonsters) do
  begin
    mon := gMonsters[idx];
    if (mon <> nil) then
    begin
      result := cb(mon);
      if result then exit;
    end;
  end;
end;


function g_Mons_ForEachAlive (cb: TEachMonsterCB): Boolean;
var
  idx: Integer;
  mon: TMonster;
begin
  result := false;
  if (gMonsters = nil) or not assigned(cb) then exit;
  for idx := 0 to High(gMonsters) do
  begin
    mon := gMonsters[idx];
    if (mon <> nil) and mon.alive then
    begin
      result := cb(mon);
      if result then exit;
    end;
  end;
end;


function g_Mons_IsAnyAliveAt (x, y: Integer; width, height: Integer): Boolean;
  (*
  function monsCollCheck (mon: TMonster; atag: Integer): Boolean;
  begin
    result := mon.alive;// and g_Obj_Collide(x, y, width, height, @mon.Obj));
  end;
  *)
var
  idx: Integer;
  mon: TMonster;
  mit: PMonster;
  it: TMonsterGrid.Iter;
begin
  result := false;
  if (width < 1) or (height < 1) then exit;
  if gmon_debug_use_sqaccel then
  begin
    //result := (monsGrid.forEachInAABB(x, y, width, height, monsCollCheck) <> nil);
    it := monsGrid.forEachInAABB(x, y, width, height);
    for mit in it do if (mit.alive) then begin result := true; break; end;
    it.release();
  end
  else
  begin
    for idx := 0 to High(gMonsters) do
    begin
      mon := gMonsters[idx];
      if (mon <> nil) and mon.alive then
      begin
        if g_Obj_Collide(x, y, width, height, @mon.Obj) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;
end;


function g_Mons_ForEachAt (x, y: Integer; width, height: Integer; cb: TEachMonsterCB): Boolean;
  (*
  function monsCollCheck (mon: TMonster; atag: Integer): Boolean;
  begin
    result := cb(mon);
  end;
  *)
var
  idx: Integer;
  mon: TMonster;
  mit: PMonster;
  it: TMonsterGrid.Iter;
begin
  result := false;
  if (width < 1) or (height < 1) then exit;
  if gmon_debug_use_sqaccel then
  begin
    //result := (monsGrid.forEachInAABB(x, y, width, height, monsCollCheck) <> nil);
    it := monsGrid.forEachInAABB(x, y, width, height);
    for mit in it do if (cb(mit^)) then begin result := true; break; end;
    it.release();
  end
  else
  begin
    for idx := 0 to High(gMonsters) do
    begin
      mon := gMonsters[idx];
      if (mon <> nil) and mon.alive then
      begin
        if g_Obj_Collide(x, y, width, height, @mon.Obj) then
        begin
          result := cb(mon);
          if result then exit;
        end;
      end;
    end;
  end;
end;


function g_Mons_ForEachAliveAt (x, y: Integer; width, height: Integer; cb: TEachMonsterCB): Boolean;
  (*
  function monsCollCheck (mon: TMonster; atag: Integer): Boolean;
  begin
    //result := false;
    //if mon.alive and g_Obj_Collide(x, y, width, height, @mon.Obj) then result := cb(mon);
    if mon.alive then result := cb(mon) else result := false;
  end;
  *)
var
  idx: Integer;
  mon: TMonster;
  mit: PMonster;
  it: TMonsterGrid.Iter;
begin
  result := false;
  if (width < 1) or (height < 1) then exit;
  if gmon_debug_use_sqaccel then
  begin
    {
    if (width = 1) and (height = 1) then
    begin
      result := (monsGrid.forEachAtPoint(x, y, monsCollCheck) <> nil);
    end
    else
    begin
      result := (monsGrid.forEachInAABB(x, y, width, height, monsCollCheck) <> nil);
    end;
    }
    it := monsGrid.forEachInAABB(x, y, width, height);
    for mit in it do
    begin
      if (mit^.alive) then
      begin
        if (cb(mit^)) then begin result := true; break; end;
      end;
    end;
    it.release();
  end
  else
  begin
    for idx := 0 to High(gMonsters) do
    begin
      mon := gMonsters[idx];
      if (mon <> nil) and mon.alive then
      begin
        if g_Obj_Collide(x, y, width, height, @mon.Obj) then
        begin
          result := cb(mon);
          if result then exit;
        end;
      end;
    end;
  end;
end;


end.
