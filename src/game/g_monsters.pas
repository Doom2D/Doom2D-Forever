(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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
unit g_monsters;

interface

uses
  g_basic, e_graphics, g_phys, g_textures,
  g_saveload, BinEditor, g_panel;

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

  BH_NORMAL   = 0;
  BH_KILLER   = 1;
  BH_MANIAC   = 2;
  BH_INSANE   = 3;
  BH_CANNIBAL = 4;
  BH_GOOD     = 5;

type
  TMonster = Class (TObject)
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
    FAnim: Array of Array [D_LEFT..D_RIGHT] of TAnimation;
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
    vilefire: TAnimation;

    FDieTriggers: Array of Integer;
    FSpawnTrigger: Integer;

    procedure Turn();
    function findNewPrey(): Boolean;
    procedure ActivateTriggers();

  public
    FNoRespawn: Boolean;
    FFireTime: Integer;

    constructor Create(MonsterType: Byte; aID: Integer; ForcedUID: Integer = -1);
    destructor Destroy(); override;
    function Collide(X, Y: Integer; Width, Height: Word): Boolean; overload;
    function Collide(Panel: TPanel): Boolean; overload;
    function Collide(X, Y: Integer): Boolean; overload;
    function TeleportTo(X, Y: Integer; silent: Boolean; dir: Byte): Boolean;
    function Live(): Boolean;
    procedure SetHealth(aH: Integer);
    procedure Push(vx, vy: Integer);
    function Damage(aDamage: Word; VelX, VelY: Integer; SpawnerUID: Word; t: Byte): Boolean;
    function Heal(Value: Word): Boolean;
    procedure BFGHit();
    procedure Update();
    procedure ClientUpdate();
    procedure ClientAttack(wx, wy, tx, ty: Integer);
    procedure SetDeadAnim;
    procedure Draw();
    procedure WakeUp();
    procedure WakeUpSound();
    procedure DieSound();
    procedure PainSound();
    procedure ActionSound();
    procedure AddTrigger(t: Integer);
    procedure ClearTriggers();
    procedure Respawn();
    procedure SaveState(var Mem: TBinMemoryWriter);
    procedure LoadState(var Mem: TBinMemoryReader);
    procedure SetState(State: Byte; ForceAnim: Byte = 255);
    procedure MakeBloodVector(Count: Word; VelX, VelY: Integer);
    procedure MakeBloodSimple(Count: Word);
    procedure RevertAnim(R: Boolean = True);
    function  AnimIsReverse: Boolean;
    function  shoot(o: PObj; immediately: Boolean): Boolean;
    function  kick(o: PObj): Boolean;
    procedure CatchFire(Attacker: Word);
    procedure OnFireFlame(Times: DWORD = 1);

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

    property Obj: TObj read FObj;
    property UID: Word read FUID write FUID;
    property SpawnTrigger: Integer read FSpawnTrigger write FSpawnTrigger;

    property GameX: Integer read FObj.X write FObj.X;
    property GameY: Integer read FObj.Y write FObj.Y;
    property GameVelX: Integer read FObj.Vel.X write FObj.Vel.X;
    property GameVelY: Integer read FObj.Vel.Y write FObj.Vel.Y;
    property GameAccelX: Integer read FObj.Accel.X write FObj.Accel.X;
    property GameAccelY: Integer read FObj.Accel.Y write FObj.Accel.Y;
    property GameDirection: TDirection read FDirection write FDirection;

    property StartID: Integer read FStartID;
  end;

procedure g_Monsters_LoadData();
procedure g_Monsters_FreeData();
procedure g_Monsters_Init();
procedure g_Monsters_Free();
function  g_Monsters_Create(MonsterType: Byte; X, Y: Integer;
            Direction: TDirection; AdjCoord: Boolean = False; ForcedUID: Integer = -1): Integer;
procedure g_Monsters_Update();
procedure g_Monsters_Draw();
procedure g_Monsters_DrawHealth();
function  g_Monsters_Get(UID: Word): TMonster;
procedure g_Monsters_killedp();
procedure g_Monsters_SaveState(var Mem: TBinMemoryWriter);
procedure g_Monsters_LoadState(var Mem: TBinMemoryReader);
function  g_Monsters_GetIDByName(name: String): Integer;
function  g_Monsters_GetNameByID(MonsterType: Byte): String;
function  g_Monsters_GetKilledBy(MonsterType: Byte): String;

var
  gMonsters: array of TMonster;

implementation

uses
  e_log, g_main, g_sound, g_gfx, g_player, g_game,
  g_weapons, g_triggers, MAPDEF, g_items, g_options,
  g_console, g_map, Math, SysUtils, g_menu, wadreader,
  g_language, g_netmsg;

const
  ANIM_SLEEP   = 0;
  ANIM_GO      = 1;
  ANIM_DIE     = 2;
  ANIM_MESS    = 3;
  ANIM_ATTACK  = 4;
  ANIM_ATTACK2 = 5;
  ANIM_PAIN    = 6;

  STATE_SLEEP  = 0;
  STATE_GO     = 1;
  STATE_RUN    = 2;
  STATE_CLIMB  = 3;
  STATE_DIE    = 4;
  STATE_DEAD   = 5;
  STATE_ATTACK = 6;
  STATE_SHOOT  = 7;
  STATE_PAIN   = 8;
  STATE_WAIT   = 9;
  STATE_REVIVE = 10;
  STATE_RUNOUT = 11;

  MONSTER_SIGNATURE = $534E4F4D; // 'MONS'

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
       AnimDeltaRight: Array [ANIM_SLEEP..ANIM_PAIN] of TPoint;
       AnimDeltaLeft: Array [ANIM_SLEEP..ANIM_PAIN] of TPoint;
     end =          // SLEEP           GO              DIE             MESS            ATTACK          ATTACK2         PAIN
   ((LeftAnim: False; wX: 54; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //DEMON
     AnimDeltaRight: ((X:  1; Y:  4), (X:  1; Y:  4), (X:  0; Y:  4), (X:  0; Y:  4), (X:  2; Y:  6), (X:  2; Y:  6), (X:  2; Y:  5));
     AnimDeltaLeft:  ((X:  1; Y:  4), (X:  1; Y:  4), (X:  0; Y:  4), (X:  0; Y:  4), (X:  2; Y:  6), (X:  2; Y:  6), (X:  2; Y:  5))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //IMP
     AnimDeltaRight: ((X:  8; Y: -4), (X:  8; Y: -4), (X: -2; Y: -1), (X:  3; Y: -2), (X: 14; Y: -4), (X: 14; Y: -4), (X: -5; Y: -4));
     AnimDeltaLeft:  ((X:  8; Y: -4), (X:  8; Y: -4), (X: -2; Y: -1), (X:  3; Y: -2), (X: 14; Y: -4), (X: 14; Y: -4), (X: -5; Y: -4))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //ZOMBY
     AnimDeltaRight: ((X:  1; Y: -4), (X:  1; Y: -4), (X:  3; Y: -1), (X:  2; Y: -1), (X:  2; Y: -4), (X:  2; Y: -4), (X:  1; Y: -4));
     AnimDeltaLeft:  ((X:  1; Y: -4), (X:  1; Y: -4), (X:  3; Y: -1), (X:  2; Y: -1), (X:  2; Y: -4), (X:  2; Y: -4), (X:  1; Y: -4))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //SERG
     AnimDeltaRight: ((X:  0; Y: -4), (X:  0; Y: -4), (X: -3; Y: -1), (X: -4; Y: -1), (X:  1; Y: -4), (X:  1; Y: -4), (X:  0; Y: -4));
     AnimDeltaLeft:  ((X:  0; Y: -4), (X:  0; Y: -4), (X: -3; Y: -1), (X: -4; Y: -1), (X:  1; Y: -4), (X:  1; Y: -4), (X:  0; Y: -4))),

    (LeftAnim: True; wX: 70; wY: 73; AnimSpeed:(3, 3, 3, 3, 3, 4, 3);  //CYBER
     AnimDeltaRight: ((X:  2; Y: -6), (X:  2; Y: -6), (X: -3; Y: -4), (X: -3; Y: -4), (X: 25; Y: -6), (X: 0; Y: -6), (X: -2; Y: -6));
     AnimDeltaLeft:  ((X:  3; Y: -3), (X:  3; Y: -3), (X: -3; Y: -4), (X: -3; Y: -4), (X:-26; Y: -3), (X:-1; Y: -3), (X:  1; Y: -3))),

    (LeftAnim: True; wX: 32; wY: 32; AnimSpeed:(3, 2, 2, 2, 1, 0, 4);  //CGUN
     AnimDeltaRight: ((X: -1; Y: -2), (X: -1; Y: -2), (X: -2; Y:  0), (X: -2; Y:  0), (X:  0; Y: -3), (X:  0; Y: -3), (X: -1; Y: -2));
     AnimDeltaLeft:  ((X: -1; Y: -2), (X: -1; Y: -2), (X: -2; Y:  0), (X: -2; Y:  0), (X: -1; Y: -4), (X: -1; Y: -4), (X:  2; Y: -4))),

    (LeftAnim: True; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4);  //BARON
     AnimDeltaRight: ((X:  4; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X:  1; Y:  0), (X:  1; Y:  0), (X: -1; Y:  0));
     AnimDeltaLeft:  ((X:  0; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X: -2; Y:  0), (X: -2; Y:  0), (X:  1; Y:  0))),

    (LeftAnim: True; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4);  //KNIGHT
     AnimDeltaRight: ((X:  4; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X:  1; Y:  0), (X:  1; Y:  0), (X: -1; Y:  0));
     AnimDeltaLeft:  ((X:  0; Y:  0), (X:  2; Y:  0), (X: -1; Y: -1), (X: -1; Y: -1), (X: -2; Y:  0), (X: -2; Y:  0), (X:  1; Y:  0))),

    (LeftAnim: False; wX: 88; wY: 69; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //CACO
     AnimDeltaRight: ((X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -5), (X:  0; Y: -5), (X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -4));
     AnimDeltaLeft:  ((X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -5), (X:  0; Y: -5), (X:  0; Y: -4), (X:  0; Y: -4), (X:  0; Y: -4))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 4, 1, 0, 4); //SOUL
     AnimDeltaRight: ((X:  1; Y:-10), (X:  1; Y:-10), (X:-33; Y:-34), (X:-33; Y:-34), (X:-16; Y:-10), (X:-16; Y:-10), (X: -1; Y: -7));
     AnimDeltaLeft:  ((X:  1; Y:-10), (X:  1; Y:-10), (X:-33; Y:-34), (X:-33; Y:-34), (X:-16; Y:-10), (X:-16; Y:-10), (X: -1; Y: -7))),

    (LeftAnim: False; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //PAIN
     AnimDeltaRight: ((X: -1; Y: -3), (X: -1; Y: -3), (X: -3; Y:  0), (X: -3; Y:  0), (X: -1; Y: -3), (X: -1; Y: -3), (X: -1; Y: -4));
     AnimDeltaLeft:  ((X: -1; Y: -3), (X: -1; Y: -3), (X: -3; Y:  0), (X: -3; Y:  0), (X: -1; Y: -3), (X: -1; Y: -3), (X: -1; Y: -4))),

    (LeftAnim: True; wX: 128; wY: 64; AnimSpeed:(3, 2, 4, 4, 1, 0, 4); //SPIDER
     AnimDeltaRight: ((X: -4; Y: -4), (X: -4; Y: -4), (X: -2; Y:  8), (X: -2; Y:  8), (X: -3; Y: -3), (X: -3; Y: -3), (X: -3; Y: -4));
     AnimDeltaLeft:  ((X: -4; Y: -4), (X: -4; Y: -4), (X: -2; Y:  8), (X: -2; Y:  8), (X: -3; Y: -3), (X: -3; Y: -3), (X: 18; Y: -5))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 2, 3, 4, 1, 0, 4);  //BSP
     AnimDeltaRight: ((X:  0; Y: -1), (X:  0; Y: -1), (X: -3; Y:  5), (X: -3; Y:  5), (X:  7; Y: -1), (X:  7; Y: -1), (X:  1; Y: -3));
     AnimDeltaLeft:  ((X:  0; Y: -1), (X:  0; Y: -1), (X: -3; Y:  5), (X: -3; Y:  5), (X:  7; Y: -1), (X:  7; Y: -1), (X:  6; Y: -3))),

    (LeftAnim: False; wX: 64; wY: 64; AnimSpeed:(3, 2, 2, 4, 2, 0, 4); //MANCUB
     AnimDeltaRight: ((X: -2; Y: -7), (X: -2; Y: -7), (X: -4; Y: -2), (X: -4; Y: -2), (X: -4; Y: -7), (X: -4; Y: -7), (X:-14; Y: -7));
     AnimDeltaLeft:  ((X: -2; Y: -7), (X: -2; Y: -7), (X: -4; Y: -2), (X: -4; Y: -2), (X: -4; Y: -7), (X: -4; Y: -7), (X:-14; Y: -7))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 3, 3, 3, 3, 3, 3);  //SKEL
     AnimDeltaRight: ((X: -1; Y:  4), (X: -1; Y:  4), (X: -2; Y:  4), (X: -2; Y:  4), (X: -1; Y:  4), (X:  6; Y:  2), (X:-24; Y:  4));
     AnimDeltaLeft:  ((X:  1; Y:  4), (X: -1; Y:  4), (X: -2; Y:  4), (X: -2; Y:  4), (X: -2; Y:  2), (X: -5; Y:  4), (X: 26; Y:  4))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 3, 3, 3, 3, 3, 3);  //VILE
     AnimDeltaRight: ((X:  5; Y:-21), (X:  5; Y:-21), (X:  1; Y:-21), (X:  1; Y:-21), (X:  8; Y:-23), (X: -1; Y:-23), (X:  4; Y:-20));
     AnimDeltaLeft:  ((X: -8; Y:-21), (X:  5; Y:-21), (X:  1; Y:-21), (X:  1; Y:-21), (X:-10; Y:-24), (X:  3; Y:-23), (X: -4; Y:-22))),

    (LeftAnim: False; wX: 8; wY: 8; AnimSpeed:(2, 2, 2, 2, 3, 0, 1);   //FISH
     AnimDeltaRight: ((X: -1; Y:  0), (X: -1; Y:  0), (X: -2; Y: -1), (X: -2; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1));
     AnimDeltaLeft:  ((X: -1; Y:  0), (X: -1; Y:  0), (X: -2; Y: -1), (X: -2; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1 ))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 0, 3, 0, 0, 0, 5); //BARREL
     AnimDeltaRight: ((X:  0; Y:-15), (X:  0; Y:-15), (X: -1; Y:-15), (X: -1; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15));
     AnimDeltaLeft:  ((X:  0; Y:-15), (X:  0; Y:-15), (X: -1; Y:-15), (X: -1; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15), (X:  0; Y:-15))),

    (LeftAnim: False; wX: 95; wY: 57; AnimSpeed:(1, 2, 1, 0, 1, 1, 0); //ROBO
     AnimDeltaRight: ((X: -2; Y:-26), (X: -2; Y:-26), (X:  0; Y:-26), (X:  0; Y:-26), (X:  2; Y:-26), (X: 15; Y:-26), (X: -2; Y:-26));
     AnimDeltaLeft:  ((X: -2; Y:-26), (X: -2; Y:-26), (X:  0; Y:-26), (X:  0; Y:-26), (X:  2; Y:-26), (X: 15; Y:-26), (X: -2; Y:-26))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 2, 2, 2, 0, 5); //MAN
     AnimDeltaRight: ((X:  0; Y: -6), (X:  0; Y: -6), (X: -2; Y:  0), (X:  2; Y:  0), (X:  1; Y: -6), (X:  1; Y: -6), (X:  0; Y: -6));
     AnimDeltaLeft:  ((X:  0; Y: -6), (X:  0; Y: -6), (X: -2; Y:  0), (X:  2; Y:  0), (X:  1; Y: -6), (X:  1; Y: -6), (X:  0; Y: -6))) );

  MAX_ATM = 89; // Время ожидания после потери цели
  MAX_SOUL = 512; // Ограничение Lost_Soul'ов

var
  pt_x: Integer = 0;
  pt_xs: Integer = 1;
  pt_y: Integer = 0;
  pt_ys: Integer = 1;
  soulcount: Integer = 0;

function FindMonster(): DWORD;
var
  i: Integer;
begin
  if gMonsters <> nil then
    for i := 0 to High(gMonsters) do
      if gMonsters[i] = nil then
      begin
        Result := i;
        Exit;
      end;

  if gMonsters = nil then
    begin
      SetLength(gMonsters, 32);
      Result := 0;
    end
  else
    begin
      Result := High(gMonsters) + 1;
      SetLength(gMonsters, Length(gMonsters) + 32);
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
    m := g_Monsters_Get(SpawnerUID);
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

function isCorpse(o: PObj; immediately: Boolean): Integer;
var
  a: Integer;
begin
  Result := -1;

// Если нужна вероятность:
  if not immediately then
    if Random(8) <> 0 then
      Exit;

  if gMonsters = nil then
    Exit;

// Ищем мертвых монстров поблизости:
  for a := 0 to High(gMonsters) do
    if (gMonsters[a] <> nil) and (gMonsters[a].FState = STATE_DEAD) then
      if g_Obj_Collide(o, @gMonsters[a].FObj) then
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

procedure g_Monsters_LoadData();
begin
  e_WriteLog('Loading monsters data...', MSG_NOTIFY);

  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 0%', 0, False);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_SLEEP', GameWAD+':MTEXTURES\BARREL_SLEEP', 64, 64, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_DIE', GameWAD+':MTEXTURES\BARREL_DIE', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_PAIN', GameWAD+':MTEXTURES\BARREL_PAIN', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_SLEEP', GameWAD+':MTEXTURES\ZOMBY_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_GO', GameWAD+':MTEXTURES\ZOMBY_GO', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_DIE', GameWAD+':MTEXTURES\ZOMBY_DIE', 64, 64, 6);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 5%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_MESS', GameWAD+':MTEXTURES\ZOMBY_MESS', 64, 64, 9);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_ATTACK', GameWAD+':MTEXTURES\ZOMBY_ATTACK', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_PAIN', GameWAD+':MTEXTURES\ZOMBY_PAIN', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_SLEEP', GameWAD+':MTEXTURES\SERG_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_GO', GameWAD+':MTEXTURES\SERG_GO', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_DIE', GameWAD+':MTEXTURES\SERG_DIE', 64, 64, 5);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 10%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_MESS', GameWAD+':MTEXTURES\SERG_MESS', 64, 64, 9);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_ATTACK', GameWAD+':MTEXTURES\SERG_ATTACK', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_PAIN', GameWAD+':MTEXTURES\SERG_PAIN', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_SLEEP', GameWAD+':MTEXTURES\MAN_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_GO', GameWAD+':MTEXTURES\MAN_GO', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_DIE', GameWAD+':MTEXTURES\MAN_DIE', 64, 64, 7);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 15%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_MESS', GameWAD+':MTEXTURES\MAN_MESS', 64, 64, 9);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_ATTACK', GameWAD+':MTEXTURES\MAN_ATTACK', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_PAIN', GameWAD+':MTEXTURES\MAN_PAIN', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_SLEEP', GameWAD+':MTEXTURES\CGUN_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_SLEEP_L', GameWAD+':MTEXTURES\CGUN_SLEEP_L', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_GO', GameWAD+':MTEXTURES\CGUN_GO', 64, 64, 4);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 20%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_GO_L', GameWAD+':MTEXTURES\CGUN_GO_L', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_DIE', GameWAD+':MTEXTURES\CGUN_DIE', 64, 64, 7);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_MESS', GameWAD+':MTEXTURES\CGUN_MESS', 64, 64, 6);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_ATTACK', GameWAD+':MTEXTURES\CGUN_ATTACK', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_ATTACK_L', GameWAD+':MTEXTURES\CGUN_ATTACK_L', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_PAIN', GameWAD+':MTEXTURES\CGUN_PAIN', 64, 64, 1);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 25%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_PAIN_L', GameWAD+':MTEXTURES\CGUN_PAIN_L', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_SLEEP', GameWAD+':MTEXTURES\IMP_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_GO', GameWAD+':MTEXTURES\IMP_GO', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_DIE', GameWAD+':MTEXTURES\IMP_DIE', 64, 64, 5);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_MESS', GameWAD+':MTEXTURES\IMP_MESS', 64, 64, 8);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_ATTACK', GameWAD+':MTEXTURES\IMP_ATTACK', 64, 64, 3);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 30%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_PAIN', GameWAD+':MTEXTURES\IMP_PAIN', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_SLEEP', GameWAD+':MTEXTURES\DEMON_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_GO', GameWAD+':MTEXTURES\DEMON_GO', 64, 64, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_DIE', GameWAD+':MTEXTURES\DEMON_DIE', 64, 64, 6);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_ATTACK', GameWAD+':MTEXTURES\DEMON_ATTACK', 64, 64, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_PAIN', GameWAD+':MTEXTURES\DEMON_PAIN', 64, 64, 1);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 35%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_SLEEP', GameWAD+':MTEXTURES\SOUL_SLEEP', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_GO', GameWAD+':MTEXTURES\SOUL_GO', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_PAIN', GameWAD+':MTEXTURES\SOUL_PAIN', 64, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_ATTACK', GameWAD+':MTEXTURES\SOUL_ATTACK', 64, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_DIE', GameWAD+':MTEXTURES\SOUL_DIE', 128, 128, 7);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_SLEEP', GameWAD+':MTEXTURES\FISH_SLEEP', 32, 32, 2);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 40%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_GO', GameWAD+':MTEXTURES\FISH_GO', 32, 32, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_PAIN', GameWAD+':MTEXTURES\FISH_PAIN', 32, 32, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_ATTACK', GameWAD+':MTEXTURES\FISH_ATTACK', 32, 32, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_DIE', GameWAD+':MTEXTURES\FISH_DIE', 32, 32, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_SLEEP', GameWAD+':MTEXTURES\SPIDER_SLEEP', 256, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_GO', GameWAD+':MTEXTURES\SPIDER_GO', 256, 128, 6);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 45%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_PAIN', GameWAD+':MTEXTURES\SPIDER_PAIN', 256, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_PAIN_L', GameWAD+':MTEXTURES\SPIDER_PAIN_L', 256, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_ATTACK', GameWAD+':MTEXTURES\SPIDER_ATTACK', 256, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_DIE', GameWAD+':MTEXTURES\SPIDER_DIE', 256, 128, 10);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_SLEEP', GameWAD+':MTEXTURES\BSP_SLEEP', 128, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_GO', GameWAD+':MTEXTURES\BSP_GO', 128, 64, 6);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 50%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_PAIN', GameWAD+':MTEXTURES\BSP_PAIN', 128, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_PAIN_L', GameWAD+':MTEXTURES\BSP_PAIN_L', 128, 64, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_ATTACK', GameWAD+':MTEXTURES\BSP_ATTACK', 128, 64, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_DIE', GameWAD+':MTEXTURES\BSP_DIE', 128, 64, 7);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_SLEEP', GameWAD+':MTEXTURES\CACO_SLEEP', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_GO', GameWAD+':MTEXTURES\CACO_GO', 128, 128, 1);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 55%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_PAIN', GameWAD+':MTEXTURES\CACO_PAIN', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_ATTACK', GameWAD+':MTEXTURES\CACO_ATTACK', 128, 128, 6);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_DIE', GameWAD+':MTEXTURES\CACO_DIE', 128, 128, 7);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_SLEEP', GameWAD+':MTEXTURES\PAIN_SLEEP', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_GO', GameWAD+':MTEXTURES\PAIN_GO', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_PAIN', GameWAD+':MTEXTURES\PAIN_PAIN', 128, 128, 1);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 60%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_ATTACK', GameWAD+':MTEXTURES\PAIN_ATTACK', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_DIE', GameWAD+':MTEXTURES\PAIN_DIE', 128, 128, 7);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_SLEEP', GameWAD+':MTEXTURES\BARON_SLEEP', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_SLEEP_L', GameWAD+':MTEXTURES\BARON_SLEEP_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_GO', GameWAD+':MTEXTURES\BARON_GO', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_PAIN', GameWAD+':MTEXTURES\BARON_PAIN', 128, 128, 1);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 65%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_PAIN_L', GameWAD+':MTEXTURES\BARON_PAIN_L', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_ATTACK', GameWAD+':MTEXTURES\BARON_ATTACK', 128, 128, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_ATTACK_L', GameWAD+':MTEXTURES\BARON_ATTACK_L', 128, 128, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_DIE', GameWAD+':MTEXTURES\BARON_DIE', 128, 128, 7);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_SLEEP', GameWAD+':MTEXTURES\KNIGHT_SLEEP', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_SLEEP_L', GameWAD+':MTEXTURES\KNIGHT_SLEEP_L', 128, 128, 2);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 70%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_GO', GameWAD+':MTEXTURES\KNIGHT_GO', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_PAIN', GameWAD+':MTEXTURES\KNIGHT_PAIN', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_PAIN_L', GameWAD+':MTEXTURES\KNIGHT_PAIN_L', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_ATTACK', GameWAD+':MTEXTURES\KNIGHT_ATTACK', 128, 128, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_ATTACK_L', GameWAD+':MTEXTURES\KNIGHT_ATTACK_L', 128, 128, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_DIE', GameWAD+':MTEXTURES\KNIGHT_DIE', 128, 128, 7);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 75%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_SLEEP', GameWAD+':MTEXTURES\MANCUB_SLEEP', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_GO', GameWAD+':MTEXTURES\MANCUB_GO', 128, 128, 6);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_PAIN', GameWAD+':MTEXTURES\MANCUB_PAIN', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_ATTACK', GameWAD+':MTEXTURES\MANCUB_ATTACK', 128, 128, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_DIE', GameWAD+':MTEXTURES\MANCUB_DIE', 128, 128, 10);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_SLEEP', GameWAD+':MTEXTURES\SKEL_SLEEP', 128, 128, 2);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 80%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_SLEEP_L', GameWAD+':MTEXTURES\SKEL_SLEEP_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_GO', GameWAD+':MTEXTURES\SKEL_GO', 128, 128, 6);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_PAIN', GameWAD+':MTEXTURES\SKEL_PAIN', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_PAIN_L', GameWAD+':MTEXTURES\SKEL_PAIN_L', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK', GameWAD+':MTEXTURES\SKEL_ATTACK', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK_L', GameWAD+':MTEXTURES\SKEL_ATTACK_L', 128, 128, 2);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 85%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK2', GameWAD+':MTEXTURES\SKEL_ATTACK2', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK2_L', GameWAD+':MTEXTURES\SKEL_ATTACK2_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_DIE', GameWAD+':MTEXTURES\SKEL_DIE', 128, 128, 5);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_SLEEP', GameWAD+':MTEXTURES\VILE_SLEEP', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_SLEEP_L', GameWAD+':MTEXTURES\VILE_SLEEP_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_GO', GameWAD+':MTEXTURES\VILE_GO', 128, 128, 6);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 90%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_PAIN', GameWAD+':MTEXTURES\VILE_PAIN', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_PAIN_L', GameWAD+':MTEXTURES\VILE_PAIN_L', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK', GameWAD+':MTEXTURES\VILE_ATTACK', 128, 128, 10);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK_L', GameWAD+':MTEXTURES\VILE_ATTACK_L', 128, 128, 10);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK2', GameWAD+':MTEXTURES\VILE_ATTACK2', 128, 128, 3);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK2_L', GameWAD+':MTEXTURES\VILE_ATTACK2_L', 128, 128, 3);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 95%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_DIE', GameWAD+':MTEXTURES\VILE_DIE', 128, 128, 9);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_SLEEP', GameWAD+':MTEXTURES\ROBO_SLEEP', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_GO', GameWAD+':MTEXTURES\ROBO_GO', 128, 128, 12);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_ATTACK', GameWAD+':MTEXTURES\ROBO_ATTACK', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_ATTACK2', GameWAD+':MTEXTURES\ROBO_ATTACK2', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_DIE', GameWAD+':MTEXTURES\ROBO_DIE', 128, 128, 1);
  g_Game_SetLoadingText(_lc[I_LOAD_MONSTER_TEXTURES]+' 100%', 0, True);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_SLEEP', GameWAD+':MTEXTURES\CYBER_SLEEP', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_SLEEP_L', GameWAD+':MTEXTURES\CYBER_SLEEP_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_GO', GameWAD+':MTEXTURES\CYBER_GO', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_GO_L', GameWAD+':MTEXTURES\CYBER_GO_L', 128, 128, 4);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_PAIN', GameWAD+':MTEXTURES\CYBER_PAIN', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_PAIN_L', GameWAD+':MTEXTURES\CYBER_PAIN_L', 128, 128, 1);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK', GameWAD+':MTEXTURES\CYBER_ATTACK', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK_L', GameWAD+':MTEXTURES\CYBER_ATTACK_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK2', GameWAD+':MTEXTURES\CYBER_ATTACK2', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK2_L', GameWAD+':MTEXTURES\CYBER_ATTACK2_L', 128, 128, 2);
  g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_DIE', GameWAD+':MTEXTURES\CYBER_DIE', 128, 128, 9);

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
end;

procedure g_Monsters_FreeData();
begin
  e_WriteLog('Releasing monsters data...', MSG_NOTIFY);

  g_Frames_DeleteByName('FRAMES_MONSTER_BARREL_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARREL_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARREL_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_MESS');
  g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_SERG_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_SERG_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_SERG_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_SERG_MESS');
  g_Frames_DeleteByName('FRAMES_MONSTER_SERG_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_SERG_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_MAN_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_MAN_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_MAN_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_MAN_MESS');
  g_Frames_DeleteByName('FRAMES_MONSTER_MAN_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_MAN_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_SLEEP_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_GO_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_MESS');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_ATTACK_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_CGUN_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_IMP_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_IMP_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_IMP_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_IMP_MESS');
  g_Frames_DeleteByName('FRAMES_MONSTER_IMP_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_IMP_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_DEMON_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_SOUL_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_FISH_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_FISH_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_FISH_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_FISH_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_FISH_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_SPIDER_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_BSP_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_BSP_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_BSP_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_BSP_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_BSP_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_BSP_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_CACO_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_CACO_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_CACO_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_CACO_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_CACO_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_PAIN_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_SLEEP_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_ATTACK_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_BARON_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_SLEEP_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_ATTACK_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_KNIGHT_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_MANCUB_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_SLEEP_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK2');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_ATTACK2_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_SKEL_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_SLEEP_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK2');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_ATTACK2_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_VILE_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_ATTACK2');
  g_Frames_DeleteByName('FRAMES_MONSTER_ROBO_DIE');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_SLEEP');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_SLEEP_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_GO');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_GO_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_PAIN');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_PAIN_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK2');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_ATTACK2_L');
  g_Frames_DeleteByName('FRAMES_MONSTER_CYBER_DIE');

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
end;

procedure g_Monsters_Init();
begin
  soulcount := 0;
end;

procedure g_Monsters_Free();
var
  a: Integer;
begin
  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      gMonsters[a].Free();

  gMonsters := nil;
end;

function g_Monsters_Create(MonsterType: Byte; X, Y: Integer;
           Direction: TDirection; AdjCoord: Boolean = False; ForcedUID: Integer = -1): Integer;
var
  find_id: DWORD;
begin
  Result := -1;

// Нет такого монстра:
  if (MonsterType > MONSTER_MAN) or (MonsterType = 0) then
    Exit;

// Соблюдаем ограничение Lost_Soul'ов:
  if MonsterType = MONSTER_SOUL then
    if soulcount > MAX_SOUL then
      Exit
    else
      soulcount := soulcount + 1;

  find_id := FindMonster();

  gMonsters[find_id] := TMonster.Create(MonsterType, find_id, ForcedUID);

// Настраиваем положение:
  with gMonsters[find_id] do
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
  end;

  Result := find_id;
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
    if (gMonsters[a] <> nil) then
      with gMonsters[a] do
        if (FMonsterType = MONSTER_MAN) and
           (FState <> STATE_DEAD) and
           (FState <> STATE_SLEEP) and
           (FState <> STATE_DIE) then
        begin
          g_Sound_PlayExAt('SOUND_MONSTER_TRUP', FObj.X, FObj.Y);
          Exit;
        end;
end;

procedure g_Monsters_Update();
var
  a: Integer;
begin
// Целеуказатель:
  if gTime mod (GAME_TICK*2) = 0 then
  begin
    pt_x := pt_x+pt_xs;
    pt_y := pt_y+pt_ys;
    if Abs(pt_x) > 246 then
      pt_xs := -pt_xs;
    if Abs(pt_y) > 100 then
      pt_ys := -pt_ys;
  end;

  gMon := True; // Для работы BlockMon'а

  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if (gMonsters[a] <> nil) then
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

  gMon := False;
end;

procedure g_Monsters_Draw();
var
  a: Integer;
begin
  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if gMonsters[a] <> nil then
        gMonsters[a].Draw();
end;

procedure g_Monsters_DrawHealth();
var
  a: Integer;
  fW, fH: Byte;
begin
  if gMonsters = nil then Exit;
  e_TextureFontGetSize(gStdFont, fW, fH);

  for a := 0 to High(gMonsters) do
    if gMonsters[a] <> nil then
    begin
      e_TextureFontPrint(gMonsters[a].FObj.X + gMonsters[a].FObj.Rect.X,
      gMonsters[a].FObj.Y + gMonsters[a].FObj.Rect.Y + gMonsters[a].FObj.Rect.Height - fH,
      IntToStr(gMonsters[a].FHealth), gStdFont);
    end;
end;

function g_Monsters_Get(UID: Word): TMonster;
var
  a: Integer;
begin
  Result := nil;

  if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
      if (gMonsters[a] <> nil) and
         (gMonsters[a].FUID = UID) then
      begin
        Result := gMonsters[a];
        Break;
      end;
end;

procedure g_Monsters_SaveState(var Mem: TBinMemoryWriter);
var
  count, i: Integer;
  b: Byte;
begin
// Считаем количество существующих монстров:
  count := 0;
  if gMonsters <> nil then
    for i := 0 to High(gMonsters) do
      if gMonsters[i] <> nil then
        if gMonsters[i].FMonsterType <> MONSTER_NONE then
          count := count + 1;

  Mem := TBinMemoryWriter.Create((count+1) * 350);

// Сохраняем информацию целеуказателя:
  Mem.WriteInt(pt_x);
  Mem.WriteInt(pt_xs);
  Mem.WriteInt(pt_y);
  Mem.WriteInt(pt_ys);

// Количество монстров:
  Mem.WriteInt(count);

  if count = 0 then
    Exit;

// Сохраняем монстров:
  for i := 0 to High(gMonsters) do
    if gMonsters[i] <> nil then
      if gMonsters[i].FMonsterType <> MONSTER_NONE then
      begin
      // Тип монстра:
        b := gMonsters[i].MonsterType;
        Mem.WriteByte(b);
      // Сохраняем данные монстра:
        gMonsters[i].SaveState(Mem);
      end;
end;

procedure g_Monsters_LoadState(var Mem: TBinMemoryReader);
var
  count, i, a: Integer;
  b: Byte;
begin
  if Mem = nil then
    Exit;

  g_Monsters_Free();

// Загружаем информацию целеуказателя:
  Mem.ReadInt(pt_x);
  Mem.ReadInt(pt_xs);
  Mem.ReadInt(pt_y);
  Mem.ReadInt(pt_ys);

// Количество монстров:
  Mem.ReadInt(count);

  if count = 0 then
    Exit;

// Загружаем монстров:
  for a := 0 to count-1 do
  begin
  // Тип монстра:
    Mem.ReadByte(b);
  // Создаем монстра:
    i := g_Monsters_Create(b, 0, 0, D_LEFT);
    if i < 0 then
    begin
      raise EBinSizeError.Create('g_Monsters_LoadState: ID = -1 (Can''t create)');
    end;
  // Загружаем данные монстра:
    gMonsters[i].LoadState(Mem);
  end;
end;

function g_Monsters_GetIDByName(name: String): Integer;
var
  i: Integer;
begin
  name := UpperCase(name);
  i := MONSTER_DEMON;
  while (i <= MONSTER_MAN) do
  begin
    if name = MONSTERTABLE[i].Name then
    begin
      Result := i;
      Exit;
    end;
    Inc(i);
  end;

  Result := -1;
end;

function g_Monsters_GetNameByID(MonsterType: Byte): String;
begin
  if MonsterType in [MONSTER_DEMON..MONSTER_MAN] then
    Result := MONSTERTABLE[MonsterType].Name
  else
    Result := '?';
end;

function g_Monsters_GetKilledBy(MonsterType: Byte): String;
begin
  if MonsterType in [MONSTER_DEMON..MONSTER_MAN] then
    Result := KilledByMonster[MonsterType]
  else
    Result := '?';
end;

{ T M o n s t e r : }

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
  GameX := FStartX;
  GameY := FStartY;
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

  FState := STATE_SLEEP;
  FCurAnim := ANIM_SLEEP;

  if g_Game_IsNet and g_Game_IsServer then
  begin
    MH_SEND_MonsterPos(FUID);
    MH_SEND_MonsterState(FUID);
  end;
end;

constructor TMonster.Create(MonsterType: Byte; aID: Integer; ForcedUID: Integer = -1);
var
  a: Integer;
  FramesID: DWORD;
  s: String;
  res: Boolean;
begin
  if ForcedUID < 0 then
    FUID := g_CreateUID(UID_MONSTER)
  else
    FUID := ForcedUID;

  FMonsterType := MonsterType;

  g_Obj_Init(@FObj);

  FState := STATE_SLEEP;
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

  for a := 0 to High(FAnim) do
  begin
    FAnim[a, D_LEFT] := nil;
    FAnim[a, D_RIGHT] := nil;
  end;

  for a := ANIM_SLEEP to ANIM_PAIN do
    if (ANIMTABLE[a].name <> '') and
       (MONSTER_ANIMTABLE[MonsterType].AnimSpeed[a] <> 0) then
    begin
      s := 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+
           '_'+ANIMTABLE[a].name;

      res := g_Frames_Exists(s);

      if res then
        res := g_Frames_Get(FramesID, s);

    // Если нет такой анимации, то пробуем заменить ее на анимацию смерти:
      if (not res) then
      begin
      // Заменяем только ANIM_MESS на ANIM_DIE:
        if a <> ANIM_MESS then
          Continue;

        if g_Frames_Get(FramesID, 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+
                        '_'+ANIMTABLE[ANIM_DIE].name) then
        begin
          FAnim[a, D_RIGHT] := TAnimation.Create(FramesID, ANIMTABLE[ANIM_DIE].loop,
                                                 MONSTER_ANIMTABLE[MonsterType].AnimSpeed[ANIM_DIE]);
          FAnim[a, D_LEFT] := TAnimation.Create(FramesID, ANIMTABLE[ANIM_DIE].loop,
                                                MONSTER_ANIMTABLE[MonsterType].AnimSpeed[ANIM_DIE]);
          Continue;
        end;
      end;

      FAnim[a, D_RIGHT] := TAnimation.Create(FramesID, ANIMTABLE[a].loop,
                                             MONSTER_ANIMTABLE[MonsterType].AnimSpeed[a]);

    // Если есть отдельная левая анимация - загружаем:
      if MONSTER_ANIMTABLE[MonsterType].LeftAnim then
      begin
        s := 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+
             '_'+ANIMTABLE[a].name+'_L';
        if g_Frames_Exists(s) then
          g_Frames_Get(FramesID, s);
      end;

      FAnim[a, D_LEFT] := TAnimation.Create(FramesID, ANIMTABLE[a].loop,
                                            MONSTER_ANIMTABLE[MonsterType].AnimSpeed[a]);
    end;

// Для колдуна загружаем также анимацию огня:
  if MonsterType = MONSTER_VILE then
    begin
      g_Frames_Get(FramesID, 'FRAMES_FIRE');
      vilefire := TAnimation.Create(FramesID, True, 2);
    end
  else
    vilefire := nil;
end;

function TMonster.Damage(aDamage: Word; VelX, VelY: Integer; SpawnerUID: Word; t: Byte): Boolean;
var
  c, it: Integer;
  p: TPlayer;
begin
  Result := False;

// Умирает, умер или воскрешается => урон делать некому:
  if (FState = STATE_DEAD) or (FState = STATE_DIE) or (FState = STATE_REVIVE) then
    Exit;

// Рыбу в воде бьет током => паника без урона:
  if (t = HIT_ELECTRO) and (FMonsterType = MONSTER_FISH) and g_Game_IsServer then
  begin
    FSleep := 20;
    if Random(2) = 0 then
      FDirection := D_RIGHT
    else
      FDirection := D_LEFT;
    Result := True;
    SetState(STATE_RUN);
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
  if FState <> STATE_PAIN then
    if (FPain >= MONSTERTABLE[FMonsterType].MinPain) and
       (FMonsterType <> MONSTER_BARREL) then
         SetState(STATE_PAIN);

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
  if (SpawnerUID <> FUID) and
  (BehaviourDamage(SpawnerUID, FBehaviour, FMonsterType)) then
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
        g_Obj_Push(@gItems[it].Obj, (FObj.Vel.X div 2)-3+Random(7),
                                    (FObj.Vel.Y div 2)-Random(4));
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
      end;

    // Урон был сильным => слабые - в кашу:
      if (FHealth <= -30) and
         ((FMonsterType = MONSTER_IMP) or (FMonsterType = MONSTER_ZOMBY) or
          (FMonsterType = MONSTER_SERG) or (FMonsterType = MONSTER_CGUN) or
          (FMonsterType = MONSTER_MAN)) then
        begin
          g_Sound_PlayExAt('SOUND_MONSTER_SLOP', FObj.X, FObj.Y);
          SetState(STATE_DIE, ANIM_MESS);
        end
      else
        begin
          DieSound();
          SetState(STATE_DIE);
        end;

    // Активировать триггеры, ждущие смерти этого монстра:
      if g_Game_IsServer then ActivateTriggers();

      FHealth := 0;
    end
  else
    if FState = STATE_SLEEP then
    begin // Спал, разбудили несмертельным ударом:
      FPain := MONSTERTABLE[FMonsterType].Pain;
      SetState(STATE_GO);
    end;

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterState(FUID);
  Result := True;
end;

function TMonster.Heal(Value: Word): Boolean;
begin
  Result := False;
  if g_Game_IsClient then
    Exit;
  if not Live then
    Exit;

  if FHealth < FMaxHealth then
  begin
    IncMax(FHealth, Value, FMaxHealth);
    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_MonsterState(FUID);
    Result := True;
  end;
end;

destructor TMonster.Destroy();
var
  a: Integer;
begin
  for a := 0 to High(FAnim) do
  begin
    FAnim[a, D_LEFT].Free();
    FAnim[a, D_RIGHT].Free();
  end;

  vilefire.Free();

  inherited Destroy();
end;

procedure TMonster.Draw();
var
  m: TMirrorType;
  dx, dy, c: Integer;
  o: TObj;
begin
  //e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, 'TYPE: '+IntToStr(FMonsterType));
  //e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y+16, 'STATE: '+IntToStr(FState));

// Если колдун стреляет, то рисуем огонь:
  if FMonsterType = MONSTER_VILE then
    if FState = STATE_SHOOT then
      if GetPos(FTargetUID, @o) then
        vilefire.Draw(o.X+o.Rect.X+(o.Rect.Width div 2)-32,
                      o.Y+o.Rect.Y+o.Rect.Height-128, M_NONE);

// Не в области рисования не ресуем:
  if not g_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width, FObj.Rect.Height,
                   sX-128, sY-128, sWidth+256, sHeight+256) then
    Exit;

// Эти монстры, умирая, не оставляют трупов:
  if FState = STATE_DEAD then
    case FMonsterType of
      MONSTER_BARREL, MONSTER_SOUL, MONSTER_PAIN: Exit;
    end;

// Есть что рисовать при текущем поведении:
  if FAnim[FCurAnim, FDirection] <> nil then
  begin
  // Если нет левой анимации или она совпадает с правой => отражаем правую:
    if (FDirection = D_LEFT) and
       ((not MONSTER_ANIMTABLE[FMonsterType].LeftAnim) or
        (FAnim[FCurAnim, D_LEFT].FramesID = FAnim[FCurAnim, D_RIGHT].FramesID)) and
        (FMonsterType <> MONSTER_BARREL) then
      m := M_HORIZONTAL
    else
      m := M_NONE;

  // Левая анимация => меняем смещение относительно центра:
    if (FDirection = D_LEFT) and
       (FMonsterType <> MONSTER_BARREL) then
      begin
        dx := MONSTER_ANIMTABLE[FMonsterType].AnimDeltaLeft[FCurAnim].X;
        dy := MONSTER_ANIMTABLE[FMonsterType].AnimDeltaLeft[FCurAnim].Y;

        if m = M_HORIZONTAL then
        begin // Нет отдельной левой анимации
        // Расстояние от края текстуры до края визуального положения объекта на текстуре:
          c := (MONSTERTABLE[FMonsterType].Rect.X - dx) + MONSTERTABLE[FMonsterType].Rect.Width;
        // Расстояние от края хит бокса до края визуального положения объекта на текстуре:
          dx := FAnim[FCurAnim, FDirection].Width - c - MONSTERTABLE[FMonsterType].Rect.X;
        // Т.к. двигать текстуру нужно будет в противоположном направлении:
          dx := -dx;
        // Это значит: dX := -frameWidth - animDeltaX + hitX + hitWidth + hitX
        end;
      end
    else // Правая анимация
      begin
        dx := MONSTER_ANIMTABLE[FMonsterType].AnimDeltaRight[FCurAnim].X;
        dy := MONSTER_ANIMTABLE[FMonsterType].AnimDeltaRight[FCurAnim].Y;
      end;

  // Рисуем:
    FAnim[FCurAnim, FDirection].Draw(Obj.X+dx, Obj.Y+dy, m);
  end;

  if g_debug_Frames then
  begin
    e_DrawQuad(FObj.X+FObj.Rect.X,
               FObj.Y+FObj.Rect.Y,
               FObj.X+FObj.Rect.X+FObj.Rect.Width-1,
               FObj.Y+FObj.Rect.Y+FObj.Rect.Height-1,
               0, 255, 0);
  end;
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
  if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_MonsterPos(FUID);
end;

procedure TMonster.SetState(State: Byte; ForceAnim: Byte = 255);
var
  Anim: Byte;
begin
// Если состояние = начали умирать, а этот монстр = Lost_Soul,
// то соблюдаем ограничение количества Lost_Soul'ов:
  if (State = STATE_DIE) and (MonsterType = MONSTER_SOUL) then
    soulcount := soulcount-1;

// Присмерти - нельзя сразу начинать атаковать или бегать:
  case FState of
    STATE_DIE, STATE_DEAD, STATE_REVIVE:
      if (State <> STATE_DEAD) and (State <> STATE_REVIVE) and
         (State <> STATE_GO) then
        Exit;
  end;

// Смена состояния:
  FState := State;

  if g_Game_IsServer and g_Game_IsNet then MH_SEND_MonsterState(FUID, ForceAnim);

// Новая анимация при новом состоянии:
  case FState of
    STATE_SLEEP: Anim := ANIM_SLEEP;
    STATE_PAIN: Anim := ANIM_PAIN;
    STATE_WAIT: Anim := ANIM_SLEEP;
    STATE_CLIMB, STATE_RUN, STATE_RUNOUT, STATE_GO: Anim := ANIM_GO;
    STATE_SHOOT: Anim := ANIM_ATTACK;
    STATE_ATTACK: Anim := ANIM_ATTACK;
    STATE_DIE: Anim := ANIM_DIE;
    STATE_REVIVE:
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
var
  TA: TAnimation;
  FramesID: DWORD;
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

  TA := nil;

// Эффект телепорта в позиции монстра:
  if not silent then
  begin
    if g_Frames_Get(FramesID, 'FRAMES_TELEPORT') then
      TA := TAnimation.Create(FramesID, False, 6);
    g_Sound_PlayExAt('SOUND_GAME_TELEPORT', Obj.X, Obj.Y);
    g_GFX_OnceAnim(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
                   FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, TA);

    if g_Game_IsServer and g_Game_IsNet then
      MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
                     FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, 1,
                     NET_GFX_TELE);
  end;

  FObj.X := X - FObj.Rect.X;
  FObj.Y := Y - FObj.Rect.Y;

  if dir = 1 then
    FDirection := D_LEFT
  else
    if dir = 2 then
      FDirection := D_RIGHT
    else
      if dir = 3 then
      begin // обратное
        if FDirection = D_RIGHT then
          FDirection := D_LEFT
        else
          FDirection := D_RIGHT;
      end;

// Эффект телепорта в точке назначения:
  if not silent and (TA <> nil) then
  begin
    g_GFX_OnceAnim(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
                   FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, TA);
    TA.Free();

    if g_Game_IsServer and g_Game_IsNet then
     MH_SEND_Effect(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-32,
                    FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, 0,
                    NET_GFX_TELE);
  end;

  if g_Game_IsServer and g_Game_IsNet then
    MH_SEND_MonsterPos(FUID);
  Result := True;
end;

procedure TMonster.Update();
var
  a, b, sx, sy, wx, wy, oldvelx: Integer;
  st: Word;
  o, co: TObj;
  fall: Boolean;
label
  _end;
begin
  fall := True;

// Рыбы "летают" только в воде:
  if FMonsterType = MONSTER_FISH then
    if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
      if (FState <> STATE_DIE) and (FState <> STATE_DEAD) then
        fall := False;

// Летающие монтсры:
  if ((FMonsterType = MONSTER_SOUL) or
      (FMonsterType = MONSTER_PAIN) or
      (FMonsterType = MONSTER_CACO)) and
     (FState <> STATE_DIE) and
     (FState <> STATE_DEAD) then
    fall := False;

// Меняем скорость только по четным кадрам:
  if gTime mod (GAME_TICK*2) <> 0 then
  begin
    g_Obj_Move(@FObj, fall, True, True);
    Exit;
  end;

  if FPainTicks > 0 then
    Dec(FPainTicks)
  else
    FPainSound := False;

// Двигаемся:
  st := g_Obj_Move(@FObj, fall, True, True);

// Вылетел за карту - удаляем и запускаем триггеры:
  if WordBool(st and MOVE_FALLOUT) or (FObj.X < -1000) or
     (FObj.X > gMapInfo.Width+1000) or (FObj.Y < -1000) then
  begin
    FRemoved := True;
    if Live and (gLMSRespawn = LMS_RESPAWN_NONE) then
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
  if (FState = STATE_DIE) or (FState = STATE_DEAD) then
    FObj.Vel.X := z_dec(FObj.Vel.X, 1);

  if FFireTime > 0 then
  begin
    if WordBool(st and MOVE_INWATER) then
      FFireTime := 0
    else
    begin
      OnFireFlame(1);
      FFireTime := FFireTime - 1;
      if (FState <> STATE_DIE) and (FState <> STATE_DEAD) then
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
  if (FState = STATE_DEAD) then
    goto _end;

// AI монстров выключен:
  if g_debug_MonsterOff then
  begin
    FSleep := 1;
    if FState <> STATE_SLEEP then
      SetState(STATE_SLEEP);
  end;

// Возможно, создаем пузырьки в воде:
  if WordBool(st and MOVE_INWATER) and (Random(32) = 0) then
    case FMonsterType of
      MONSTER_FISH:
        if Random(4) = 0 then
          g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                        FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      MONSTER_ROBO, MONSTER_BARREL:
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                      FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      else begin
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width-4),
                      FObj.Y+FObj.Rect.Y + Random(4), 5, 4, 4);
        if Random(2) = 0 then
          g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
      end;
    end;

// Если прошел первый кадр анимации взрыва бочки, то взрыв:
  if FMonsterType = MONSTER_BARREL then
  begin
    if (FState = STATE_DIE) and (FAnim[FCurAnim, FDirection].CurrentFrame = 1) and
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
    if (FState in [STATE_GO, STATE_RUN, STATE_RUNOUT,
                   STATE_ATTACK, STATE_SHOOT]) then
      if g_Weapon_Danger(FUID, FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                         FObj.Rect.Width, FObj.Rect.Height, 50) then
        if (g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj)) and
           (FObj.Accel.Y = 0) then
          FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;

  case FState of
    STATE_PAIN: // Состояние - Боль
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
          SetState(STATE_GO);
        end;
      end;

    STATE_SLEEP: // Состояние - Сон
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
              if (gPlayers[a] <> nil) and (gPlayers[a].Live)
              and (not gPlayers[a].NoTarget) and (gPlayers[a].FMegaRulez[MR_INVIS] < gTime) then
                with gPlayers[a] do
                  if g_Look(@FObj, @Obj, FDirection) then
                  begin
                    FTargetUID := gPlayers[a].UID;
                    FTargetTime := 0;
                    WakeUpSound();
                    SetState(STATE_GO);
                    Break;
                  end;

      // На монстров тянет маньяков, поехавших и каннибалов
        if (FTargetUID = 0) and ((FBehaviour = BH_MANIAC)
        or (FBehaviour = BH_INSANE) or (FBehaviour = BH_CANNIBAL)) then
        // Если есть подходящий монстр рядом:
          if gMonsters <> nil then
            for a := 0 to High(gMonsters) do
              if (gMonsters[a] <> nil) and (gMonsters[a].Live) and
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
                  SetState(STATE_GO);
                  Break;
                end;
              end;
      end;

    STATE_WAIT: // Состояние - Ожидание
      begin
      // Ждем:
        FSleep := FSleep - 1;

      // Выждали достаточно - идем:
        if FSleep < 0 then
          SetState(STATE_GO);
      end;

    STATE_GO: // Состояние - Движение (с осмотром ситуации)
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          Turn();
          FSleep := 40;
          SetState(STATE_RUNOUT);

          goto _end;
        end;

      // Если монстр - колдун, то пробуем воскресить кого-нибудь:
        if (FMonsterType = MONSTER_VILE) then
          if isCorpse(@FObj, False) <> -1 then
          begin
            FObj.Vel.X := 0;
            SetState(STATE_ATTACK, ANIM_ATTACK2);

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
          FDirection := D_RIGHT
        else
          FDirection := D_LEFT;

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
            SetState(STATE_RUN);
            if Random(2) = 0 then
              FDirection := D_LEFT
            else
              FDirection := D_RIGHT;

            goto _end;
          end;

      // Уперлись в стену:
        if WordBool(st and MOVE_HITWALL) then
        begin
          if g_Triggers_PressR(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width,
                               FObj.Rect.Height, FUID, ACTIVATE_MONSTERPRESS) <> nil then
          begin // Смогли нажать кнопку - небольшое ожидание
            FSleep := 4;
            SetState(STATE_WAIT);

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
                SetState(STATE_CLIMB);
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
                    SetState(STATE_PAIN);
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
                      if not g_Obj_CollideWater(@FObj, 0, -16) then
                      begin
                      // Всплыли до поверхности - стоп:
                        FObj.Vel.Y := 0;
                      // Плаваем туда-сюда:
                        if Random(2) = 0 then
                          FDirection := D_LEFT
                        else
                          FDirection := D_RIGHT;
                        FSleep := 20;
                        SetState(STATE_RUN);
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
                if gGibs[a].Live and
                   g_Obj_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y+FObj.Rect.Height-4,
                                 FObj.Rect.Width, 8, @gGibs[a].Obj) and (Random(3) = 0) then
                  // Пинаем куски
                  if FObj.Vel.X < 0 then
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61)+120) // налево
                  else
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61));    // направо
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
                      gCorpses[a].Damage(b*2, -b, Random(7)) // налево
                    else
                      gCorpses[a].Damage(b*2, b, Random(7)); // направо
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
        if FDirection = D_RIGHT then
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

    STATE_RUN: // Состояние - Бег
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          Turn();
          FSleep := 40;
          SetState(STATE_RUNOUT);

          goto _end;
        end;

        FSleep := FSleep - 1;

      // Пробежали достаточно или врезались в стену => переходим на шаг:
        if (FSleep <= 0) or (WordBool(st and MOVE_HITWALL) and ((FObj.Vel.Y+FObj.Accel.Y) = 0)) then
        begin
          FSleep := 0;
          SetState(STATE_GO);
        // Стена - идем обратно:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
            Turn();
        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = D_RIGHT then
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

    STATE_RUNOUT: // Состояние - Убегает от чего-то
      begin
      // Вышли из БлокМона:
        if (not WordBool(st and MOVE_BLOCK)) and (FSleep > 0) then
          FSleep := 0;

        FSleep := FSleep - 1;

      // Убажели достаточно далеко => переходим на шаг:
        if FSleep <= -18 then
        begin
          FSleep := 0;
          SetState(STATE_GO);
        // Стена/БлокМон - идем обратно:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
            Turn();
        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = D_RIGHT then
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

    STATE_CLIMB: // Состояние - Прыжок (чтобы обойти стену)
      begin
      // Достигли высшей точки прыжка или стена кончилась => переходим на шаг:
        if ((FObj.Vel.Y+FObj.Accel.Y) >= 0) or
           (not WordBool(st and MOVE_HITWALL)) then
        begin
          FSleep := 0;
          SetState(STATE_GO);

        // Стена не кончилась => бежим от нее:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
          begin
            Turn();
            FSleep := 15;
            SetState(STATE_RUN);
          end;
        end;

      // Бежим в выбранную сторону:
        if FDirection = D_RIGHT then
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

    STATE_ATTACK, // Состояние - Атака
    STATE_SHOOT:  // Состояние - Стрельба
      begin
      // Lost_Soul врезался в стену при атаке => переходит на шаг:
        if FMonsterType = MONSTER_SOUL then
        begin
          if WordBool(st and (MOVE_HITWALL or MOVE_HITCEIL or MOVE_HITLAND)) then
            SetState(STATE_GO);

          goto _end;
        end;

      // Замедляемся при атаке:
        if FMonsterType <> MONSTER_FISH then
          FObj.Vel.X := z_dec(FObj.Vel.X, 1);

      // Нужно стрелять, а монстр - колдун:
        if (FMonsterType = MONSTER_VILE) and (FState = STATE_SHOOT) then
        begin
        // Цель погибла => идем дальше:
          if not GetPos(FTargetUID, @o) then
          begin
            SetState(STATE_GO);

            goto _end;
          end;

        // Цель не видно => идем дальше:
          if not g_Look(@FObj, @o, FDirection) then
          begin
            SetState(STATE_GO);

            goto _end;
          end;

        // Цель в воде - не загорится => идем дальше:
          if g_Obj_CollideWater(@o, 0, 0) then
          begin
            SetState(STATE_GO);

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
  if FState = STATE_REVIVE then
    if FAnim[FCurAnim, FDirection].Played then
    begin // Обратная анимация умирания закончилась - идем дальше:
      FAnim[FCurAnim, FDirection].Revert(False);
      SetState(STATE_GO);
    end;

// Если есть анимация огня колдуна - пусть она идет:
  if vilefire <> nil then
    vilefire.Update();

// Состояние - Умирает и текущая анимация проиграна:
  if (FState = STATE_DIE) and
     (FAnim[FCurAnim, FDirection] <> nil) and
     (FAnim[FCurAnim, FDirection].Played) then
    begin
    // Умер:
      SetState(STATE_DEAD);

    // Pain_Elemental при смерти выпускает 3 Lost_Soul'а:
      if (FMonsterType = MONSTER_PAIN) then
      begin
        sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-30,
                                FObj.Y+FObj.Rect.Y+20, D_LEFT);
        if sx <> -1 then
        begin
          gMonsters[sx].SetState(STATE_GO);
          gMonsters[sx].FNoRespawn := True;
          Inc(gTotalMonsters);
          if g_Game_IsNet then MH_SEND_MonsterSpawn(gMonsters[sx].UID);
        end;

        sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                                FObj.Y+FObj.Rect.Y+20, D_RIGHT);
        if sx <> -1 then
        begin
          gMonsters[sx].SetState(STATE_GO);
          gMonsters[sx].FNoRespawn := True;
          Inc(gTotalMonsters);
          if g_Game_IsNet then MH_SEND_MonsterSpawn(gMonsters[sx].UID);
        end;

        sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-15,
                                FObj.Y+FObj.Rect.Y, D_RIGHT);
        if sx <> -1 then
        begin
          gMonsters[sx].SetState(STATE_GO);
          gMonsters[sx].FNoRespawn := True;
          Inc(gTotalMonsters);
          if g_Game_IsNet then MH_SEND_MonsterSpawn(gMonsters[sx].UID);
        end;
        if g_Game_IsNet then MH_SEND_CoopStats(gMonsters[sx].UID);
      end;

    // У этих монстров нет трупов:
      if (FMonsterType = MONSTER_PAIN) or
         (FMonsterType = MONSTER_SOUL) or
         (FMonsterType = MONSTER_BARREL) then
        FRemoved := True;
    end;

// Совершение атаки и стрельбы:
  if (FState = STATE_ATTACK) or (FState = STATE_SHOOT) then
    if (FAnim[FCurAnim, FDirection] <> nil) then
    // Анимация атаки есть - можно атаковать
      if (FAnim[FCurAnim, FDirection].Played) then
        begin // Анимация атаки закончилась => переходим на шаг
          if FState = STATE_ATTACK then
            begin // Состояние - Атака
            // Если монстр не Lost_Soul, то после атаки переходим на шаг:
              if FMonsterType <> MONSTER_SOUL then
                SetState(STATE_GO);
            end
          else // Состояние - Стрельба
            begin
            // Переходим на шаг, если не надо стрелять еще раз:
              if not FChainFire then
                SetState(STATE_GO)
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
          if FState = STATE_ATTACK then
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
                      SetState(STATE_GO);

                MONSTER_FISH:
                // Рыба кусает первого попавшегося со звуком:
                  if g_Weapon_Hit(@FObj, 10, FUID, HIT_SOME) <> 0 then
                    g_Sound_PlayExAt('SOUND_MONSTER_FISH_ATTACK', FObj.X, FObj.Y);

                MONSTER_SKEL, MONSTER_ROBO, MONSTER_CYBER:
                // Робот, кибер или скелет сильно пинаются:
                  if FCurAnim = ANIM_ATTACK2 then
                  begin
                    o := FObj;
                    o.Vel.X := IfThen(FDirection = D_RIGHT, 1, -1)*IfThen(FMonsterType = MONSTER_CYBER, 60, 50);
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
                      gMonsters[sx].SetState(STATE_REVIVE);
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

              if FDirection = D_LEFT then
              begin
                wx := MONSTER_ANIMTABLE[FMonsterType].wX-(MONSTERTABLE[FMonsterType].Rect.X+(MONSTERTABLE[FMonsterType].Rect.Width div 2));
                wx := MONSTERTABLE[FMonsterType].Rect.X+(MONSTERTABLE[FMonsterType].Rect.Width div 2)-wx;
              end; // Это значит: wx := hitX + (hitWidth / 2) - (wx - (hitX + (hitWidth / 2)))

              wx := FObj.X + wx;
              wy := FObj.Y + MONSTER_ANIMTABLE[FMonsterType].wY;

            // Делаем выстрел нужным оружием:
              case FMonsterType of
                MONSTER_IMP:
                  g_Weapon_ball1(wx, wy, tx, ty, FUID);
                MONSTER_ZOMBY:
                  begin
                    g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', wx, wy);
                    g_Weapon_gun(wx, wy, tx, ty, 1, 3, FUID, False);
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
                    sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+(FObj.Rect.Width div 2),
                                            FObj.Y+FObj.Rect.Y, FDirection);

                    if sx <> -1 then
                    begin // Цель - цель Pain_Elemental'а. Летим к ней:
                      gMonsters[sx].FTargetUID := FTargetUID;
                      GetPos(FTargetUID, @o);
                      gMonsters[sx].FTargetTime := 0;
                      gMonsters[sx].FNoRespawn := True;
                      gMonsters[sx].SetState(STATE_GO);
                      gMonsters[sx].shoot(@o, True);
                      Inc(gTotalMonsters);

                      if g_Game_IsNet then MH_SEND_MonsterSpawn(gMonsters[sx].UID);
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
      STATE_GO, STATE_RUN, STATE_CLIMB, STATE_RUNOUT:
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
     not ((FState = STATE_DEAD) or (FState = STATE_DIE))  then
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
  fall: Boolean;
label
  _end;
begin
  sx := 0; // SHUT UP COMPILER
  sy := 0;
  fall := True;
// Рыбы "летают" только в воде:
  if FMonsterType = MONSTER_FISH then
    if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
      if (FState <> STATE_DIE) and (FState <> STATE_DEAD) then
        fall := False;

// Летающие монтсры:
  if ((FMonsterType = MONSTER_SOUL) or
      (FMonsterType = MONSTER_PAIN) or
      (FMonsterType = MONSTER_CACO)) and
     (FState <> STATE_DIE) and
     (FState <> STATE_DEAD) then
    fall := False;

// Меняем скорость только по четным кадрам:
  if gTime mod (GAME_TICK*2) <> 0 then
  begin
    g_Obj_Move(@FObj, fall, True, True);
    Exit;
  end;

  if FPainTicks > 0 then
    Dec(FPainTicks)
  else
    FPainSound := False;

// Двигаемся:
  st := g_Obj_Move(@FObj, fall, True, True);

// Вылетел за карту - удаляем и запускаем триггеры:
  if WordBool(st and MOVE_FALLOUT) or (FObj.X < -1000) or
     (FObj.X > gMapInfo.Width+1000) or (FObj.Y < -1000) then
  begin
    FRemoved := True;
    Exit;
  end;

  oldvelx := FObj.Vel.X;

// Сопротивление воздуха для трупа:
  if (FState = STATE_DIE) or (FState = STATE_DEAD) then
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
  if (FState = STATE_DEAD) then
    goto _end;

// Возможно, создаем пузырьки в воде:
  if WordBool(st and MOVE_INWATER) and (Random(32) = 0) then
    case FMonsterType of
      MONSTER_FISH:
        if Random(4) = 0 then
          g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                        FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      MONSTER_ROBO, MONSTER_BARREL:
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width),
                      FObj.Y+FObj.Rect.Y + Random(4), 1, 0, 0);
      else begin
        g_GFX_Bubbles(FObj.X+FObj.Rect.X + Random(FObj.Rect.Width-4),
                      FObj.Y+FObj.Rect.Y + Random(4), 5, 4, 4);
        if Random(2) = 0 then
          g_Sound_PlayExAt('SOUND_GAME_BUBBLE1', FObj.X, FObj.Y)
        else
          g_Sound_PlayExAt('SOUND_GAME_BUBBLE2', FObj.X, FObj.Y);
      end;
    end;

// Если прошел первый кадр анимации взрыва бочки, то взрыв:
  if FMonsterType = MONSTER_BARREL then
  begin
    if (FState = STATE_DIE) and (FAnim[FCurAnim, FDirection].CurrentFrame = 1) and
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
    if (FState in [STATE_GO, STATE_RUN, STATE_RUNOUT,
                   STATE_ATTACK, STATE_SHOOT]) then
      if g_Weapon_Danger(FUID, FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y,
                         FObj.Rect.Width, FObj.Rect.Height, 50) then
        if (g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj)) and
           (FObj.Accel.Y = 0) then
          FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;

  case FState of
    STATE_PAIN: // Состояние - Боль
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
          SetState(STATE_GO);
          FPain := 0;
        end;
      end;

    STATE_SLEEP: // Состояние - Сон
      begin
      // Спим:
        FSleep := FSleep + 1;

      // Проспали достаточно:
        if FSleep >= 18 then
          FSleep := 0
        else // еще спим
          goto _end;
      end;

    STATE_WAIT: // Состояние - Ожидание
      begin
      // Ждем:
        FSleep := FSleep - 1;
      end;

    STATE_GO: // Состояние - Движение (с осмотром ситуации)
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          Turn();
          FSleep := 40;
          SetState(STATE_RUNOUT);

          goto _end;
        end;

      // Если монстр - колдун, то пробуем воскресить кого-нибудь:
        if (FMonsterType = MONSTER_VILE) then
          if isCorpse(@FObj, False) <> -1 then
          begin
            SetState(STATE_ATTACK, ANIM_ATTACK2);
            FObj.Vel.X := 0;

            goto _end;
          end;

      // Если цель почти на одной вертикали - бегаем туда-сюда:
        if Abs(sx) < 40 then
          if FMonsterType <> MONSTER_FISH then
          begin
            SetState(STATE_RUN);
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
                SetState(STATE_CLIMB);
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
                    SetState(STATE_PAIN);
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
                      if not g_Obj_CollideWater(@FObj, 0, -16) then
                      begin
                      // Всплыли до поверхности - стоп:
                        FObj.Vel.Y := 0;
                      // Плаваем туда-сюда:
                        SetState(STATE_RUN);
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
                if gGibs[a].Live and
                   g_Obj_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y+FObj.Rect.Height-4,
                                 FObj.Rect.Width, 8, @gGibs[a].Obj) and (Random(3) = 0) then
                  // Пинаем куски
                  if FObj.Vel.X < 0 then
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61)+120) // налево
                  else
                    g_Obj_PushA(@gGibs[a].Obj, b, Random(61));    // направо
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
                      gCorpses[a].Damage(b*2, -b, Random(7)) // налево
                    else
                      gCorpses[a].Damage(b*2, b, Random(7)); // направо
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
        if FDirection = D_RIGHT then
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

    STATE_RUN: // Состояние - Бег
      begin
      // Если наткнулись на БлокМон - убегаем от него:
        if WordBool(st and MOVE_BLOCK) then
        begin
          SetState(STATE_RUNOUT);
          FSleep := 40;

          goto _end;
        end;

        FSleep := FSleep - 1;

      // Пробежали достаточно или врезались в стену => переходим на шаг:
        if (FSleep <= 0) or (WordBool(st and MOVE_HITWALL) and ((FObj.Vel.Y+FObj.Accel.Y) = 0)) then
        begin
          SetState(STATE_GO);
          FSleep := 0;

        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = D_RIGHT then
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

    STATE_RUNOUT: // Состояние - Убегает от чего-то
      begin
      // Вышли из БлокМона:
        if (not WordBool(st and MOVE_BLOCK)) and (FSleep > 0) then
          FSleep := 0;

        FSleep := FSleep - 1;

      // Убажели достаточно далеко => переходим на шаг:
        if FSleep <= -18 then
        begin
          SetState(STATE_GO);
          FSleep := 0;

        // Иногда рычим:
          if Random(8) = 0 then
            ActionSound();
        end;

      // Бежим в выбранную сторону:
        if FDirection = D_RIGHT then
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

    STATE_CLIMB: // Состояние - Прыжок (чтобы обойти стену)
      begin
      // Достигли высшей точки прыжка или стена кончилась => переходим на шаг:
        if ((FObj.Vel.Y+FObj.Accel.Y) >= 0) or
           (not WordBool(st and MOVE_HITWALL)) then
        begin
          SetState(STATE_GO);
          FSleep := 0;

        // Стена не кончилась => бежим от нее:
          if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
          begin
            SetState(STATE_RUN);
            FSleep := 15;
          end;
        end;

      // Бежим в выбранную сторону:
        if FDirection = D_RIGHT then
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

    STATE_ATTACK, // Состояние - Атака
    STATE_SHOOT:  // Состояние - Стрельба
      begin
      // Lost_Soul врезался в стену при атаке => переходит на шаг:
        if FMonsterType = MONSTER_SOUL then
        begin
          if WordBool(st and (MOVE_HITWALL or MOVE_HITCEIL or MOVE_HITLAND)) then
            SetState(STATE_GO);

          goto _end;
        end;

      // Замедляемся при атаке:
        if FMonsterType <> MONSTER_FISH then
          FObj.Vel.X := z_dec(FObj.Vel.X, 1);

      // Нужно стрелять, а монстр - колдун:
        if (FMonsterType = MONSTER_VILE) and (FState = STATE_SHOOT) then
        begin
        // Цель погибла => идем дальше:
          if not GetPos(FTargetUID, @o) then
          begin
            SetState(STATE_GO);

            goto _end;
          end;

        // Цель не видно => идем дальше:
          if not g_Look(@FObj, @o, FDirection) then
          begin
            SetState(STATE_GO);

            goto _end;
          end;

        // Цель в воде - не загорится => идем дальше:
          if g_Obj_CollideWater(@o, 0, 0) then
          begin
            SetState(STATE_GO);

            goto _end;
          end;
        end;
      end;
  end; // case FState of ...

_end:

// Состояние - Воскрешение:
  if FState = STATE_REVIVE then
    if FAnim[FCurAnim, FDirection].Played then
    begin // Обратная анимация умирания закончилась - идем дальше:
      FAnim[FCurAnim, FDirection].Revert(False);
      SetState(STATE_GO);
    end;

// Если есть анимация огня колдуна - пусть она идет:
  if vilefire <> nil then
    vilefire.Update();

// Состояние - Умирает и текущая анимация проиграна:
  if (FState = STATE_DIE) and
     (FAnim[FCurAnim, FDirection] <> nil) and
     (FAnim[FCurAnim, FDirection].Played) then
    begin
    // Умер:
      SetState(STATE_DEAD);

    // У этих монстров нет трупов:
      if (FMonsterType = MONSTER_PAIN) or
         (FMonsterType = MONSTER_SOUL) or
         (FMonsterType = MONSTER_BARREL) then
        FRemoved := True
      else
        FAnim[FCurAnim, FDirection].CurrentFrame := FAnim[FCurAnim, FDirection].TotalFrames - 1;
    end;

// Совершение атаки и стрельбы:
  if (FState = STATE_ATTACK) or (FState = STATE_SHOOT) then
    if (FAnim[FCurAnim, FDirection] <> nil) then
    // Анимация атаки есть - можно атаковать
      if (FAnim[FCurAnim, FDirection].Played) then
        begin // Анимация атаки закончилась => переходим на шаг
          if FState = STATE_ATTACK then
            begin // Состояние - Атака
            // Если монстр не Lost_Soul, то после атаки переходим на шаг:
              if FMonsterType <> MONSTER_SOUL then
                SetState(STATE_GO);
            end
          else // Состояние - Стрельба
            begin
            // Переходим на шаг, если не надо стрелять еще раз:
              if not FChainFire then
                SetState(STATE_GO)
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
          if FState = STATE_ATTACK then
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
                      SetState(STATE_GO);

                MONSTER_FISH:
                  g_Weapon_Hit(@FObj, 10, FUID, HIT_SOME);

                MONSTER_SKEL, MONSTER_ROBO, MONSTER_CYBER:
                // Робот, кибер или скелет сильно пинаются:
                  if FCurAnim = ANIM_ATTACK2 then
                  begin
                    o := FObj;
                    o.Vel.X := IfThen(FDirection = D_RIGHT, 1, -1)*IfThen(FMonsterType = MONSTER_CYBER, 60, 50);
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
      STATE_GO, STATE_RUN, STATE_CLIMB, STATE_RUNOUT:
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
     not ((FState = STATE_DEAD) or (FState = STATE_DIE))  then
    FObj.Vel.X := oldvelx;

// Если есть анимация, то пусть она идет:
  if FAnim[FCurAnim, FDirection] <> nil then
    FAnim[FCurAnim, FDirection].Update();
end;

procedure TMonster.ClientAttack(wx, wy, tx, ty: Integer);
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
      g_Weapon_ball1(wx, wy, tx, ty, FUID);
    MONSTER_CYBER:
      g_Weapon_rocket(wx, wy, tx, ty, FUID);
    MONSTER_SKEL:
      g_Weapon_revf(wx, wy, tx, ty, FUID, FTargetUID);
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
  end;
end;

procedure TMonster.Turn();
begin
// Разворачиваемся:
  if FDirection = D_LEFT then
    FDirection := D_RIGHT
  else
    FDirection := D_LEFT;

// Бежим в выбранную сторону:
  if FDirection = D_RIGHT then
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
      if (gPlayers[a] <> nil) and (gPlayers[a].Live)
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
      if (gMonsters[a] <> nil) and (gMonsters[a].Live) and
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
        SetState(STATE_ATTACK);
        Result := True;
      end;
    MONSTER_DEMON:
      begin
        SetState(STATE_ATTACK);
        g_Sound_PlayExAt('SOUND_MONSTER_DEMON_ATTACK', FObj.X, FObj.Y);
        Result := True;
      end;
    MONSTER_IMP:
      begin
        SetState(STATE_ATTACK);
        g_Sound_PlayExAt('SOUND_MONSTER_IMP_ATTACK', FObj.X, FObj.Y);
        Result := True;
      end;
    MONSTER_SKEL, MONSTER_ROBO, MONSTER_CYBER:
      begin
        SetState(STATE_ATTACK, ANIM_ATTACK2);
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
        SetState(STATE_SHOOT);
        {nn}
      end;
    MONSTER_SKEL:
      begin
        SetState(STATE_SHOOT);
        {nn}
      end;
    MONSTER_VILE:
      begin // Зажигаем огонь
        tx := o^.X+o^.Rect.X+(o^.Rect.Width div 2);
        ty := o^.Y+o^.Rect.Y;
        SetState(STATE_SHOOT);

        vilefire.Reset();

        g_Sound_PlayExAt('SOUND_MONSTER_VILE_ATTACK', FObj.X, FObj.Y);
        g_Sound_PlayExAt('SOUND_FIRE', o^.X, o^.Y);
      end;
    MONSTER_SOUL:
      begin // Летит в сторону цели:
        SetState(STATE_ATTACK);
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

        SetState(STATE_SHOOT);
      end;
    else Exit;
  end;

  Result := True;
end;

function TMonster.Live(): Boolean;
begin
  Result := (FState <> STATE_DIE) and (FState <> STATE_DEAD) and (FHealth > 0);
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
  SetState(STATE_GO);
  FTargetTime := MAX_ATM;
  WakeUpSound();
end;

procedure TMonster.SaveState(var Mem: TBinMemoryWriter);
var
  i: Integer;
  sig: DWORD;
  b: Byte;
  anim: Boolean;
begin
  if Mem = nil then
    Exit;

// Сигнатура монстра:
  sig := MONSTER_SIGNATURE; // 'MONS'
  Mem.WriteDWORD(sig);
// UID монстра:
  Mem.WriteWord(FUID);
// Направление:
  if FDirection = D_LEFT then
    b := 1
  else // D_RIGHT
    b := 2;
  Mem.WriteByte(b);
// Надо ли удалить его:
  Mem.WriteBoolean(FRemoved);
// Осталось здоровья:
  Mem.WriteInt(FHealth);
// Состояние:
  Mem.WriteByte(FState);
// Текущая анимация:
  Mem.WriteByte(FCurAnim);
// UID цели:
  Mem.WriteWord(FTargetUID);
// Время после потери цели:
  Mem.WriteInt(FTargetTime);
// Поведение монстра:
  Mem.WriteByte(FBehaviour);
// Готовность к выстрелу:
  Mem.WriteInt(FAmmo);
// Боль:
  Mem.WriteInt(FPain);
// Время ожидания:
  Mem.WriteInt(FSleep);
// Озвучивать ли боль:
  Mem.WriteBoolean(FPainSound);
// Была ли атака во время анимации атаки:
  Mem.WriteBoolean(FWaitAttackAnim);
// Надо ли стрелять на следующем шаге:
  Mem.WriteBoolean(FChainFire);
// Подлежит ли респавну:
  Mem.WriteBoolean(FNoRespawn);
// Координаты цели:
  Mem.WriteInt(tx);
  Mem.WriteInt(ty);
// ID монстра при старте карты:
  Mem.WriteInt(FStartID);
// Индекс триггера, создавшего монстра:
  Mem.WriteInt(FSpawnTrigger);
// Объект монстра:
  Obj_SaveState(@FObj, Mem);
// Есть ли анимация огня колдуна:
  anim := vilefire <> nil;
  Mem.WriteBoolean(anim);
// Если есть - сохраняем:
  if anim then
    vilefire.SaveState(Mem);
// Анимации:
  for i := ANIM_SLEEP to ANIM_PAIN do
  begin
  // Есть ли левая анимация:
    anim := FAnim[i, D_LEFT] <> nil;
    Mem.WriteBoolean(anim);
  // Если есть - сохраняем:
    if anim then
      FAnim[i, D_LEFT].SaveState(Mem);
  // Есть ли правая анимация:
    anim := FAnim[i, D_RIGHT] <> nil;
    Mem.WriteBoolean(anim);
  // Если есть - сохраняем:
    if anim then
      FAnim[i, D_RIGHT].SaveState(Mem);
  end;
end;

procedure TMonster.LoadState(var Mem: TBinMemoryReader);
var
  i: Integer;
  sig: DWORD;
  b: Byte;
  anim: Boolean;
begin
  if Mem = nil then
    Exit;

// Сигнатура монстра:
  Mem.ReadDWORD(sig);
  if sig <> MONSTER_SIGNATURE then // 'MONS'
  begin
    raise EBinSizeError.Create('TMonster.LoadState: Wrong Monster Signature');
  end;
// UID монстра:
  Mem.ReadWord(FUID);
// Направление:
  Mem.ReadByte(b);
  if b = 1 then
    FDirection := D_LEFT
  else // b = 2
    FDirection := D_RIGHT;
// Надо ли удалить его:
  Mem.ReadBoolean(FRemoved);
// Осталось здоровья:
  Mem.ReadInt(FHealth);
// Состояние:
  Mem.ReadByte(FState);
// Текущая анимация:
  Mem.ReadByte(FCurAnim);
// UID цели:
  Mem.ReadWord(FTargetUID);
// Время после потери цели:
  Mem.ReadInt(FTargetTime);
// Поведение монстра:
  Mem.ReadByte(FBehaviour);
// Готовность к выстрелу:
  Mem.ReadInt(FAmmo);
// Боль:
  Mem.ReadInt(FPain);
// Время ожидания:
  Mem.ReadInt(FSleep);
// Озвучивать ли боль:
  Mem.ReadBoolean(FPainSound);
// Была ли атака во время анимации атаки:
  Mem.ReadBoolean(FWaitAttackAnim);
// Надо ли стрелять на следующем шаге:
  Mem.ReadBoolean(FChainFire);
// Подлежит ли респавну
  Mem.ReadBoolean(FNoRespawn);
// Координаты цели:
  Mem.ReadInt(tx);
  Mem.ReadInt(ty);
// ID монстра при старте карты:
  Mem.ReadInt(FStartID);
// Индекс триггера, создавшего монстра:
  Mem.ReadInt(FSpawnTrigger);
// Объект монстра:
  Obj_LoadState(@FObj, Mem);
// Есть ли анимация огня колдуна:
  Mem.ReadBoolean(anim);
// Если есть - загружаем:
  if anim then
  begin
    Assert(vilefire <> nil, 'TMonster.LoadState: no vilefire anim');
    vilefire.LoadState(Mem);
  end;
// Анимации:
  for i := ANIM_SLEEP to ANIM_PAIN do
  begin
  // Есть ли левая анимация:
    Mem.ReadBoolean(anim);
  // Если есть - загружаем:
    if anim then
    begin
      Assert(FAnim[i, D_LEFT] <> nil,
        'TMonster.LoadState: no '+IntToStr(i)+'_left anim');
      FAnim[i, D_LEFT].LoadState(Mem);
    end;
  // Есть ли правая анимация:
     Mem.ReadBoolean(anim);
  // Если есть - загружаем:
    if anim then
    begin
      Assert(FAnim[i, D_RIGHT] <> nil,
        'TMonster.LoadState: no '+IntToStr(i)+'_right anim');
      FAnim[i, D_RIGHT].LoadState(Mem);
    end;
  end;
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

procedure TMonster.CatchFire(Attacker: Word);
begin
  FFireTime := 100;
  FFireAttacker := Attacker;
  if g_Game_IsNet and g_Game_IsServer then
    MH_SEND_MonsterState(FUID);
end;

procedure TMonster.OnFireFlame(Times: DWORD = 1);
var
  id, i: DWORD;
  Anim: TAnimation;
begin
  if (Random(10) = 1) and (Times = 1) then
    Exit;

  if g_Frames_Get(id, 'FRAMES_FLAME') then
  begin
    for i := 1 to Times do
    begin
      Anim := TAnimation.Create(id, False, 3);
      Anim.Alpha := 0;
      g_GFX_OnceAnim(Obj.X+Obj.Rect.X+Random(Obj.Rect.Width+Times*2)-(Anim.Width div 2),
                   Obj.Y+8+Random(8+Times*2)+IfThen(FState = STATE_DEAD, 16, 0), Anim, ONCEANIM_SMOKE);
      Anim.Free();
    end;
  end;
end;

end.
