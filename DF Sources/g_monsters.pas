unit g_monsters;

interface

uses
  g_basic, windows, e_graphics, g_phys, g_textures, g_saveload;

type
  PMonsterSysSaveRec = ^TMonsterSysSaveRec;
  TMonsterSysSaveRec = packed record
   pt_x: Integer;
   pt_xs: Integer;
   pt_y: Integer;
   pt_ys: Integer;
  end;

  PMonsterSaveRec = ^TMonsterSaveRec;
  TMonsterSaveRec = packed record
   UID: Word;
   Obj: TObjRec;
   Health: Integer;
   Direction: TDirection;
   Anim: array[0..6] of array[D_LEFT..D_RIGHT] of TAnimRec;
   CurAnim: Byte;
   State: Byte;
   TargetUID: Word;
   TargetTime: Integer;
   MonsterType: Byte;
   Removed: Boolean;
   Ammo: Integer;
   Pain: Integer;
   Sleep: Integer;
   PainSound: Boolean;
   DieTriggers: array[0..15] of Integer;
   tx, ty: Integer;
   vilefire: TAnimRec;
  end;

  TMonster = class(TObject)
   private
    FUID: Word;
    FObj: TObj;
    FHealth: Integer;
    FDirection: TDirection;
    FAnim: array of array[D_LEFT..D_RIGHT] of TAnimation;
    FCurAnim: Byte;
    FState: Byte;
    FTargetUID: Word;
    FTargetTime: Integer;
    FMonsterType: Byte;
    FRemoved: Boolean;
    FAmmo: Integer;
    FPain: Integer;
    FSleep: Integer;
    FPainSound: Boolean;
    FDieTriggers: array of Integer;
    tx, ty: Integer;
    vilefire: TAnimation;
    procedure Turn();
    function shoot(o: PObj; n: Boolean): Boolean;
    function kick(o: PObj): Boolean;
    function findnewprey(): Boolean;
    procedure SetState(State: Byte; ForceAnim: Byte = 255);
    procedure MakeBloodVector(Count: Word; VelX, VelY: Integer);
    procedure MakeBloodSimple(Count: Word);
    procedure WakeUpSound();
    procedure DieSound();
    procedure PainSound();
    procedure ActionSound();
    procedure ActivateTriggers();
   public
    constructor Create(MonsterType: Byte);
    destructor Destroy(); override;
    function Collide(X, Y: Integer; Width, Height: Word): Boolean; overload;
    function Collide(Rect: TRectWH): Boolean; overload;
    function Collide(X, Y: Integer): Boolean; overload;
    function TeleportTo(X, Y: Integer; silent: Boolean): Boolean;
    function Live(): Boolean;
    procedure Push(vx, vy: Integer);
    function Damage(Damage: Word; VelX, VelY: Integer; SpawnerUID: Word; t: Byte): Boolean;
    procedure BFGHit();
    procedure Update();
    procedure Draw();
    procedure AddTrigger(t: Integer);
    procedure Save(Rec: PMonsterSaveRec);
    procedure Load(Rec: PMonsterSaveRec);
    property MonsterType: Byte read FMonsterType;
    property Obj: TObj read FObj;
    property UID: Word read FUID;
  end;
  
procedure g_Monsters_LoadData();
procedure g_Monsters_FreeData();
procedure g_Monsters_Init();
procedure g_Monsters_Free();
function g_Monsters_Create(MonsterType: Byte; X, Y: Integer; Direction: TDirection): Integer;
procedure g_Monsters_Update();
procedure g_Monsters_Draw();
function g_Monsters_Get(UID: Word): TMonster;
procedure g_Monsters_killedp();
procedure g_Monsters_goodsnd();
function g_Monsters_Save(var p: Pointer): Integer;
procedure g_Monsters_Load(p: Pointer; len: Integer);

var
  gMonsters: array of TMonster;

implementation

uses
  e_log, g_main, g_sound, g_gfx, g_player, g_game, g_weapons, g_triggers,
  MAPDEF, g_items, g_options, g_console, g_map, Math, SysUtils, g_menu,
  WADEDITOR;

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

  ANIMTABLE: array[ANIM_SLEEP..ANIM_PAIN] of
              record
               name: string;
               loop: Boolean;
              end = ((name: 'SLEEP'; loop: True),
                     (name: 'GO'; loop: True),
                     (name: 'DIE'; loop: False),
                     (name: 'MESS'; loop: False),
                     (name: 'ATTACK'; loop: False),
                     (name: 'ATTACK2'; loop: False),
                     (name: 'PAIN'; loop: False));

  MONSTERTABLE: array[MONSTER_DEMON..MONSTER_MAN] of
                record
                 Name: string;
                 Rect: TRectWH;
                 Health: Word;
                 RunVel: Byte;
                 MinPain: Byte;
                 Pain: Byte;
                 Jump: Byte;
                end =
   ((Name:'DEMON'; Rect:(X:7; Y:1; Width:49; Height:53); Health:60;
     RunVel: 7; MinPain: 10; Pain: 20; Jump: 10),

    (Name:'IMP'; Rect:(X:11; Y:13; Width:33; Height:51); Health:25;
     RunVel: 3; MinPain: 0; Pain: 15; Jump: 10),

    (Name:'ZOMBY'; Rect:(X:18; Y:10; Width:33; Height:54); Health:15;
     RunVel: 3; MinPain: 0; Pain: 10; Jump: 10),

    (Name:'SERG'; Rect:(X:19; Y:10; Width:29; Height:54); Health:20;
     RunVel: 3; MinPain: 0; Pain: 10; Jump: 10),

    (Name:'CYBER'; Rect:(X:24; Y:12; Width:80; Height:110); Health:500;
     RunVel: 5; MinPain: 50; Pain: 70; Jump: 10),

    (Name:'CGUN'; Rect:(X:19; Y:6; Width:30; Height:56); Health:60;
     RunVel: 3; MinPain: 10; Pain: 20; Jump: 10),

    (Name:'BARON'; Rect:(X:40; Y:32; Width:48; Height:64); Health:150;
     RunVel: 3; MinPain: 30; Pain: 40; Jump: 10),

    (Name:'KNIGHT'; Rect:(X:40; Y:32; Width:48; Height:64); Health:75;
     RunVel: 3; MinPain: 30; Pain: 40; Jump: 10),

    (Name:'CACO'; Rect:(X:34; Y:39; Width:60; Height:56); Health:100;
     RunVel: 4; MinPain: 0; Pain: 10; Jump: 4),

    (Name:'SOUL'; Rect:(X:16; Y:23; Width:32; Height:36); Health:60;
     RunVel: 4; MinPain: 0; Pain: 10; Jump: 4),

    (Name:'PAIN'; Rect:(X:34; Y:39; Width:60; Height:56); Health:100;
     RunVel: 4; MinPain: 0; Pain: 10; Jump: 4),

    (Name:'SPIDER'; Rect:(X:14; Y:3; Width:211; Height:100); Health:500;
     RunVel: 4; MinPain: 50; Pain: 70; Jump: 10),

    (Name:'BSP'; Rect:(X:14; Y:9; Width:100; Height:43); Health:150;
     RunVel: 4; MinPain: 0; Pain: 20; Jump: 10),

    (Name:'MANCUB'; Rect:(X:28; Y:36; Width:72; Height:60); Health:200;
     RunVel: 3; MinPain: 20; Pain: 40; Jump: 7),

    (Name:'SKEL'; Rect:(X:30; Y:24; Width:68; Height:72); Health:200;
     RunVel: 6; MinPain: 20; Pain: 40; Jump: 11),

    (Name:'VILE'; Rect:(X:30; Y:48; Width:68; Height:72); Health:150;
     RunVel: 7; MinPain: 10; Pain: 30; Jump: 12),

    (Name:'FISH'; Rect:(X:6; Y:11; Width:20; Height:10); Health:35;
     RunVel: 14; MinPain: 10; Pain: 20; Jump: 6),

    (Name:'BARREL'; Rect:(X:18; Y:26; Width:24; Height:37); Health:20;
     MinPain: 0; Pain: 0),

    (Name:'ROBO'; Rect:(X:30; Y:52; Width:68; Height:76); Health:20;
     RunVel: 3; MinPain: 20; Pain: 40; Jump: 6),

    (Name:'MAN'; Rect:(X:18; Y:10; Width:33; Height:54); Health:400;
     RunVel: 8; MinPain: 50; Pain: 70; Jump: 10));

     MONSTERTABLE2: array[MONSTER_DEMON..MONSTER_MAN] of
     record
      LeftAnim: Boolean;
      wX, wY: Integer;
      AnimSpeed: array[ANIM_SLEEP..ANIM_PAIN] of Byte;
      AnimDelta: array[ANIM_SLEEP..ANIM_PAIN] of TPoint;
     end =
   ((LeftAnim: False; wX: 54; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4);
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:-1;Y:-3), (X:-1;Y:-3), (X:-2;Y:-1), (X:0;Y:0), (X:-1;Y:0))), //DEMON

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4);
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:-4;Y:4), (X:-3;Y:3), (X:-1;Y:2), (X:0;Y:0), (X:-5;Y:2))), //IMP

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //ZOMBY
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:4;Y:5), (X:6;Y:5), (X:6;Y:1), (X:0;Y:0), (X:4;Y:1))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 2, 3, 0, 4); //SERG
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:4;Y:2), (X:2;Y:2), (X:6;Y:1), (X:0;Y:0), (X:4;Y:1))),

    (LeftAnim: True; wX: 70; wY: 73; AnimSpeed:(3, 3, 3, 3, 3, 0, 3); //CYBER
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:24;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: True; wX: 32; wY: 32; AnimSpeed:(3, 2, 2, 2, 1, 0, 4); //CGUN
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:3;Y:0), (X:3;Y:0), (X:3;Y:0), (X:0;Y:0), (X:2;Y:0))),

    (LeftAnim: True; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //BARON
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: True; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //KNIGHT
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; wX: 88; wY: 69; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //CACO
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:2), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 3, 4, 1, 0, 4); //SOUL
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:-32;Y:-35), (X:3;Y:0), (X:0;Y:0), (X:0;Y:0), (X:2;Y:0))),

    (LeftAnim: False; wX: 64; wY: 64; AnimSpeed:(3, 2, 3, 4, 2, 0, 4); //PAIN
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: True; wX: 128; wY: 64; AnimSpeed:(3, 2, 4, 4, 1, 0, 4); //SPIDER
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:1))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 2, 3, 4, 1, 0, 4); //BSP
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:3;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; wX: 64; wY: 64; AnimSpeed:(3, 2, 2, 4, 2, 0, 4); //MANCUB
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 3, 3, 3, 3, 3, 3); //SKEL
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: True; wX: 64; wY: 32; AnimSpeed:(3, 3, 3, 3, 3, 3, 3); //VILE
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; wX: 8; wY: 8; AnimSpeed:(2, 2, 2, 2, 3, 0, 1); //FISH
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; AnimSpeed:(3, 0, 3, 0, 0, 0, 5);                  //BARREL
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; wX: 95; wY: 57; AnimSpeed:(1, 2, 1, 0, 1, 1, 0); //ROBO
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0), (X:0;Y:0))),

    (LeftAnim: False; wX: 32; wY: 32; AnimSpeed:(3, 2, 2, 2, 2, 0, 5); //MAN
     AnimDelta:((X:0;Y:0), (X:0;Y:0), (X:1;Y:3), (X:1;Y:5), (X:7;Y:1), (X:0;Y:0), (X:6;Y:1))));

  MAX_ATM = 90;
  MAX_SOUL = 500;

var
  gsnd: array[0..3] of TSound;
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

 if (a = MONSTER_BARREL) or (b = MONSTER_BARREL) then Exit;

 if a = b then
  case a of
   MONSTER_IMP, MONSTER_DEMON, MONSTER_BARON, MONSTER_KNIGHT, MONSTER_CACO,
   MONSTER_SOUL, MONSTER_MANCUB, MONSTER_SKEL, MONSTER_FISH: Exit;
  end;

 if (a = MONSTER_SOUL) and (b = MONSTER_PAIN) then Exit;
 if (b = MONSTER_SOUL) and (a = MONSTER_PAIN) then Exit;

 Result := False;
end;

function canshoot(m: Byte): Boolean;
begin
 Result := False;

 case m of
  MONSTER_DEMON, MONSTER_FISH, MONSTER_BARREL: Exit;
  else Result := True;
 end;
end;

function iscorpse(o: PObj; n: Boolean): Integer;
var
  a: Integer;
begin
 Result := -1;

 if not n then
  if Random(8) <> 0 then Exit;

 if gMonsters = nil then Exit;

 for a := 0 to High(gMonsters) do
  if (gMonsters[a] <> nil) and (gMonsters[a].FState = STATE_DEAD) then
   if g_Obj_Collide(o, @gMonsters[a].FObj) then
   case gMonsters[a].FMonsterType of
    MONSTER_SOUL, MONSTER_PAIN, MONSTER_CYBER, MONSTER_SPIDER,
    MONSTER_VILE, MONSTER_BARREL, MONSTER_ROBO: Continue;
    else
    begin
     Result := a;
     Exit;
    end;
   end;
end;

procedure g_Monsters_LoadData();
var
  ID: DWORD;
begin
 e_WriteLog('Loading monsters data...', MSG_NOTIFY);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_SLEEP', GameWAD+':MTEXTURES\BARREL_SLEEP', 64, 64, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_DIE', GameWAD+':MTEXTURES\BARREL_DIE', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_PAIN', GameWAD+':MTEXTURES\BARREL_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_SLEEP', GameWAD+':MTEXTURES\ZOMBY_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_GO', GameWAD+':MTEXTURES\ZOMBY_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_DIE', GameWAD+':MTEXTURES\ZOMBY_DIE', 64, 64, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_MESS', GameWAD+':MTEXTURES\ZOMBY_MESS', 64, 64, 9);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_ATTACK', GameWAD+':MTEXTURES\ZOMBY_ATTACK', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_PAIN', GameWAD+':MTEXTURES\ZOMBY_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_SLEEP', GameWAD+':MTEXTURES\SERG_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_GO', GameWAD+':MTEXTURES\SERG_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_DIE', GameWAD+':MTEXTURES\SERG_DIE', 64, 64, 5);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_MESS', GameWAD+':MTEXTURES\SERG_MESS', 64, 64, 9);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_ATTACK', GameWAD+':MTEXTURES\SERG_ATTACK', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SERG_PAIN', GameWAD+':MTEXTURES\SERG_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_SLEEP', GameWAD+':MTEXTURES\MAN_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_GO', GameWAD+':MTEXTURES\MAN_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_DIE', GameWAD+':MTEXTURES\MAN_DIE', 64, 64, 7);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_MESS', GameWAD+':MTEXTURES\MAN_MESS', 64, 64, 9);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_ATTACK', GameWAD+':MTEXTURES\MAN_ATTACK', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MAN_PAIN', GameWAD+':MTEXTURES\MAN_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_SLEEP', GameWAD+':MTEXTURES\CGUN_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_SLEEP_L', GameWAD+':MTEXTURES\CGUN_SLEEP_L', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_GO', GameWAD+':MTEXTURES\CGUN_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_GO_L', GameWAD+':MTEXTURES\CGUN_GO_L', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_DIE', GameWAD+':MTEXTURES\CGUN_DIE', 64, 64, 7);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_MESS', GameWAD+':MTEXTURES\CGUN_MESS', 64, 64, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_ATTACK', GameWAD+':MTEXTURES\CGUN_ATTACK', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_ATTACK_L', GameWAD+':MTEXTURES\CGUN_ATTACK_L', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_PAIN', GameWAD+':MTEXTURES\CGUN_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CGUN_PAIN_L', GameWAD+':MTEXTURES\CGUN_PAIN_L', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_SLEEP', GameWAD+':MTEXTURES\IMP_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_GO', GameWAD+':MTEXTURES\IMP_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_DIE', GameWAD+':MTEXTURES\IMP_DIE', 64, 64, 5);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_MESS', GameWAD+':MTEXTURES\IMP_MESS', 64, 64, 8);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_ATTACK', GameWAD+':MTEXTURES\IMP_ATTACK', 64, 64, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_IMP_PAIN', GameWAD+':MTEXTURES\IMP_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_SLEEP', GameWAD+':MTEXTURES\DEMON_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_GO', GameWAD+':MTEXTURES\DEMON_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_DIE', GameWAD+':MTEXTURES\DEMON_DIE', 64, 64, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_ATTACK', GameWAD+':MTEXTURES\DEMON_ATTACK', 64, 64, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_DEMON_PAIN', GameWAD+':MTEXTURES\DEMON_PAIN', 64, 64, 1);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_SLEEP', GameWAD+':MTEXTURES\SOUL_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_GO', GameWAD+':MTEXTURES\SOUL_GO', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_PAIN', GameWAD+':MTEXTURES\SOUL_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_ATTACK', GameWAD+':MTEXTURES\SOUL_ATTACK', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SOUL_DIE', GameWAD+':MTEXTURES\SOUL_DIE', 128, 128, 7);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_SLEEP', GameWAD+':MTEXTURES\FISH_SLEEP', 32, 32, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_GO', GameWAD+':MTEXTURES\FISH_GO', 32, 32, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_PAIN', GameWAD+':MTEXTURES\FISH_PAIN', 32, 32, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_ATTACK', GameWAD+':MTEXTURES\FISH_ATTACK', 32, 32, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_FISH_DIE', GameWAD+':MTEXTURES\FISH_DIE', 32, 32, 1);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_SLEEP', GameWAD+':MTEXTURES\SPIDER_SLEEP', 256, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_GO', GameWAD+':MTEXTURES\SPIDER_GO', 256, 128, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_PAIN', GameWAD+':MTEXTURES\SPIDER_PAIN', 256, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_PAIN_L', GameWAD+':MTEXTURES\SPIDER_PAIN_L', 256, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_ATTACK', GameWAD+':MTEXTURES\SPIDER_ATTACK', 256, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SPIDER_DIE', GameWAD+':MTEXTURES\SPIDER_DIE', 256, 128, 10);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_SLEEP', GameWAD+':MTEXTURES\BSP_SLEEP', 128, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_GO', GameWAD+':MTEXTURES\BSP_GO', 128, 64, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_PAIN', GameWAD+':MTEXTURES\BSP_PAIN', 128, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_PAIN_L', GameWAD+':MTEXTURES\BSP_PAIN_L', 128, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_ATTACK', GameWAD+':MTEXTURES\BSP_ATTACK', 128, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BSP_DIE', GameWAD+':MTEXTURES\BSP_DIE', 128, 64, 7);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_SLEEP', GameWAD+':MTEXTURES\CACO_SLEEP', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_GO', GameWAD+':MTEXTURES\CACO_GO', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_PAIN', GameWAD+':MTEXTURES\CACO_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_ATTACK', GameWAD+':MTEXTURES\CACO_ATTACK', 128, 128, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CACO_DIE', GameWAD+':MTEXTURES\CACO_DIE', 128, 128, 7);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_SLEEP', GameWAD+':MTEXTURES\PAIN_SLEEP', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_GO', GameWAD+':MTEXTURES\PAIN_GO', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_PAIN', GameWAD+':MTEXTURES\PAIN_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_ATTACK', GameWAD+':MTEXTURES\PAIN_ATTACK', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_PAIN_DIE', GameWAD+':MTEXTURES\PAIN_DIE', 128, 128, 7);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_SLEEP', GameWAD+':MTEXTURES\BARON_SLEEP', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_SLEEP_L', GameWAD+':MTEXTURES\BARON_SLEEP_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_GO', GameWAD+':MTEXTURES\BARON_GO', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_PAIN', GameWAD+':MTEXTURES\BARON_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_PAIN_L', GameWAD+':MTEXTURES\BARON_PAIN_L', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_ATTACK', GameWAD+':MTEXTURES\BARON_ATTACK', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_ATTACK_L', GameWAD+':MTEXTURES\BARON_ATTACK_L', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARON_DIE', GameWAD+':MTEXTURES\BARON_DIE', 128, 128, 7);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_SLEEP', GameWAD+':MTEXTURES\KNIGHT_SLEEP', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_SLEEP_L', GameWAD+':MTEXTURES\KNIGHT_SLEEP_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_GO', GameWAD+':MTEXTURES\KNIGHT_GO', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_PAIN', GameWAD+':MTEXTURES\KNIGHT_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_PAIN_L', GameWAD+':MTEXTURES\KNIGHT_PAIN_L', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_ATTACK', GameWAD+':MTEXTURES\KNIGHT_ATTACK', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_ATTACK_L', GameWAD+':MTEXTURES\KNIGHT_ATTACK_L', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_KNIGHT_DIE', GameWAD+':MTEXTURES\KNIGHT_DIE', 128, 128, 7);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_SLEEP', GameWAD+':MTEXTURES\MANCUB_SLEEP', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_GO', GameWAD+':MTEXTURES\MANCUB_GO', 128, 128, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_PAIN', GameWAD+':MTEXTURES\MANCUB_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_ATTACK', GameWAD+':MTEXTURES\MANCUB_ATTACK', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_MANCUB_DIE', GameWAD+':MTEXTURES\MANCUB_DIE', 128, 128, 10);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_SLEEP', GameWAD+':MTEXTURES\SKEL_SLEEP', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_SLEEP_L', GameWAD+':MTEXTURES\SKEL_SLEEP_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_GO', GameWAD+':MTEXTURES\SKEL_GO', 128, 128, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_PAIN', GameWAD+':MTEXTURES\SKEL_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_PAIN_L', GameWAD+':MTEXTURES\SKEL_PAIN_L', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK', GameWAD+':MTEXTURES\SKEL_ATTACK', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK_L', GameWAD+':MTEXTURES\SKEL_ATTACK_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK2', GameWAD+':MTEXTURES\SKEL_ATTACK2', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_ATTACK2_L', GameWAD+':MTEXTURES\SKEL_ATTACK2_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_SKEL_DIE', GameWAD+':MTEXTURES\SKEL_DIE', 128, 128, 5);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_SLEEP', GameWAD+':MTEXTURES\VILE_SLEEP', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_SLEEP_L', GameWAD+':MTEXTURES\VILE_SLEEP_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_GO', GameWAD+':MTEXTURES\VILE_GO', 128, 128, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_PAIN', GameWAD+':MTEXTURES\VILE_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_PAIN_L', GameWAD+':MTEXTURES\VILE_PAIN_L', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK', GameWAD+':MTEXTURES\VILE_ATTACK', 128, 128, 10);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK_L', GameWAD+':MTEXTURES\VILE_ATTACK_L', 128, 128, 10);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK2', GameWAD+':MTEXTURES\VILE_ATTACK2', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_ATTACK2_L', GameWAD+':MTEXTURES\VILE_ATTACK2_L', 128, 128, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_VILE_DIE', GameWAD+':MTEXTURES\VILE_DIE', 128, 128, 9);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_SLEEP', GameWAD+':MTEXTURES\ROBO_SLEEP', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_GO', GameWAD+':MTEXTURES\ROBO_GO', 128, 128, 12);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_ATTACK', GameWAD+':MTEXTURES\ROBO_ATTACK', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_ATTACK2', GameWAD+':MTEXTURES\ROBO_ATTACK2', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ROBO_DIE', GameWAD+':MTEXTURES\ROBO_DIE', 128, 128, 1);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_SLEEP', GameWAD+':MTEXTURES\CYBER_SLEEP', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_SLEEP_L', GameWAD+':MTEXTURES\CYBER_SLEEP_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_GO', GameWAD+':MTEXTURES\CYBER_GO', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_GO_L', GameWAD+':MTEXTURES\CYBER_GO_L', 128, 128, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_PAIN', GameWAD+':MTEXTURES\CYBER_PAIN', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_PAIN_L', GameWAD+':MTEXTURES\CYBER_PAIN_L', 128, 128, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK', GameWAD+':MTEXTURES\CYBER_ATTACK', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_ATTACK_L', GameWAD+':MTEXTURES\CYBER_ATTACK_L', 128, 128, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_CYBER_DIE', GameWAD+':MTEXTURES\CYBER_DIE', 128, 128, 9);

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

 g_Sound_CreateWADEx('SOUND_GOOD1', GameWAD+':SOUNDS\GOOD1');
 g_Sound_CreateWADEx('SOUND_GOOD2', GameWAD+':SOUNDS\GOOD2');
 g_Sound_CreateWADEx('SOUND_GOOD3', GameWAD+':SOUNDS\GOOD3');
 g_Sound_CreateWADEx('SOUND_GOOD4', GameWAD+':SOUNDS\GOOD4');

 gsnd[0] := TSound.Create();
 gsnd[1] := TSound.Create();
 gsnd[2] := TSound.Create();
 gsnd[3] := TSound.Create();

 if g_Sound_Get(ID, 'SOUND_GOOD1') then gsnd[0].SetID(ID);
 if g_Sound_Get(ID, 'SOUND_GOOD2') then gsnd[1].SetID(ID);
 if g_Sound_Get(ID, 'SOUND_GOOD3') then gsnd[2].SetID(ID);
 if g_Sound_Get(ID, 'SOUND_GOOD4') then gsnd[3].SetID(ID);
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

 g_Sound_Delete('SOUND_GOOD1');
 g_Sound_Delete('SOUND_GOOD2');
 g_Sound_Delete('SOUND_GOOD3');
 g_Sound_Delete('SOUND_GOOD4');

 gsnd[0].Destroy();
 gsnd[1].Destroy();
 gsnd[2].Destroy();
 gsnd[3].Destroy();
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
   if gMonsters[a] <> nil then gMonsters[a].Destroy();

 gMonsters := nil;
end;

function g_Monsters_Create(MonsterType: Byte; X, Y: Integer; Direction: TDirection): Integer;
var
  find_id: DWORD;
begin
 Result := -1;

 if (MonsterType > MONSTER_MAN) or (MonsterType = 0) then Exit;

 if MonsterType = MONSTER_SOUL then
  if soulcount > MAX_SOUL then Exit else soulcount := soulcount+1;

 find_id := FindMonster();

 gMonsters[find_id] := TMonster.Create(MonsterType);

 with gMonsters[find_id] do
 begin
  FObj.X := X-FObj.Rect.X;
  FObj.Y := Y-FObj.Rect.Y;
  FDirection := Direction;
 end;

 Result := find_id;
end;

procedure g_Monsters_killedp();
var
  a, h: Integer;
begin
 if gMonsters = nil then Exit;

 h := High(gMonsters);
 for a := 0 to h do
  if (gMonsters[a] <> nil) then
   with gMonsters[a] do
    if (FMonsterType = MONSTER_MAN) and (FState <> STATE_DEAD) and
       (FState <> STATE_SLEEP) and (FState <> STATE_DIE) then
    begin
     g_Sound_PlayExAt('SOUND_MONSTER_TRUP', 255, FObj.X, FObj.Y);
     Exit;
    end;
end;

procedure g_Monsters_goodsnd();
var
  a: Integer;
begin
 for a := 0 to 3 do
  if gsnd[a].IsPlaying() then Exit;

 gsnd[Random(4)].Play(127, 255);
end;

procedure g_Monsters_Update();
var
  a: Integer;
begin
 if gTime mod (GAME_TICK*2) = 0 then
 begin
  pt_x := pt_x+pt_xs;
  pt_y := pt_y+pt_ys;
  if Abs(pt_x) > 246 then pt_xs := -pt_xs;
  if Abs(pt_y) > 100 then pt_ys := -pt_ys;
 end;

 gMon := True;

 if gMonsters <> nil then
  for a := 0 to High(gMonsters) do
   if (gMonsters[a] <> nil) and (not gMonsters[a].FRemoved) then gMonsters[a].Update();

 gMon := False;
end;

procedure g_Monsters_Draw();
var
  a: Integer;
begin
 if gMonsters <> nil then
  for a := 0 to High(gMonsters) do
   if gMonsters[a] <> nil then gMonsters[a].Draw();
end;

function g_Monsters_Get(UID: Word): TMonster;
var
  a: Integer;
begin
 Result := nil;
 
 if gMonsters <> nil then
  for a := 0 to High(gMonsters) do
   if (gMonsters[a] <> nil) and (gMonsters[a].FUID = UID) then
   begin
    Result := gMonsters[a];
    Break;
   end;
end;

function g_Monsters_Save(var p: Pointer): Integer;
var
  a, b, i, s: Integer;
  msys: TMonsterSysSaveRec;
  monster: TMonsterSaveRec;
begin
 b := 0;
 if gMonsters <> nil then
  for i := 0 to High(gMonsters) do
   if gMonsters[i] <> nil then
    if gMonsters[i].FMonsterType <> MONSTER_NONE then b := b+1;

 s := SizeOf(TMonsterSysSaveRec)+SizeOf(TMonsterSaveRec)*b;
 p := GetMemory(s);
 Result := s;

 msys.pt_x := pt_x;
 msys.pt_xs := pt_xs;
 msys.pt_y := pt_y;
 msys.pt_ys := pt_ys;

 CopyMemory(p, @msys, SizeOf(TMonsterSysSaveRec));
 if b = 0 then Exit;

 a := 0;
 for i := 0 to High(gMonsters) do
  if gMonsters[i] <> nil then
    if gMonsters[i].FMonsterType <> MONSTER_NONE then
    begin
     gMonsters[i].Save(@monster);
     CopyMemory(Pointer(Integer(p)+SizeOf(TMonsterSysSaveRec)+a*SizeOf(TMonsterSaveRec)),
                @monster, SizeOf(TMonsterSaveRec));
     a := a+1;
    end;

end;

procedure g_Monsters_Load(p: Pointer; len: Integer);
var
  a, b, c: Integer;
  msys: TMonsterSysSaveRec;
  monster: TMonsterSaveRec;
begin
 g_Monsters_Free();

 if len < SizeOf(TMonsterSysSaveRec) then Exit;

 CopyMemory(@msys, p, SizeOf(TMonsterSysSaveRec));
 pt_x := msys.pt_x;
 pt_xs := msys.pt_xs;
 pt_y := msys.pt_y;
 pt_ys := msys.pt_ys;

 c := (len-SizeOf(TMonsterSysSaveRec)) div SizeOf(TMonsterSaveRec);
 if c = 0 then Exit;

 for a := 0 to c-1 do
 begin
  CopyMemory(@monster, Pointer(Integer(p)+SizeOf(TMonsterSysSaveRec)+a*SizeOf(TMonsterSaveRec)), SizeOf(TMonsterSaveRec));
  b := g_Monsters_Create(monster.MonsterType, 0, 0, D_LEFT);
  gMonsters[b].Load(@monster);
 end;
end;

{ TMonster }

procedure TMonster.ActionSound();
begin
 case FMonsterType of
  MONSTER_IMP: g_Sound_PlayExAt('SOUND_MONSTER_IMP_ACTION', 255, FObj.X, FObj.Y);
  MONSTER_ZOMBY, MONSTER_SERG, MONSTER_CGUN,
  MONSTER_MANCUB: g_Sound_PlayExAt('SOUND_MONSTER_ACTION', 255, FObj.X, FObj.Y);
  MONSTER_SOUL, MONSTER_BARON, MONSTER_CACO,
  MONSTER_KNIGHT, MONSTER_PAIN, MONSTER_DEMON,
  MONSTER_SPIDER: g_Sound_PlayExAt('SOUND_MONSTER_ACTION2', 255, FObj.X, FObj.Y);
  MONSTER_BSP: g_Sound_PlayExAt('SOUND_MONSTER_BSP_ACTION', 255, FObj.X, FObj.Y);
  MONSTER_VILE: g_Sound_PlayExAt('SOUND_MONSTER_VILE_ACTION', 255, FObj.X, FObj.Y);
  MONSTER_SKEL: g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ACTION', 255, FObj.X, FObj.Y);
  MONSTER_CYBER: ;
  MONSTER_MAN: g_Sound_PlayExAt('SOUND_MONSTER_HAHA', 255, FObj.X, FObj.Y);
 end;
end;

procedure TMonster.PainSound();
begin
 if FPainSound then Exit;

 FPainSound := True;

 case FMonsterType of
  MONSTER_IMP, MONSTER_ZOMBY, MONSTER_SERG, MONSTER_SKEL,
  MONSTER_CGUN: g_Sound_PlayExAt('SOUND_MONSTER_PAIN', 255, FObj.X, FObj.Y);
  MONSTER_SOUL, MONSTER_BARON, MONSTER_CACO,
  MONSTER_KNIGHT, MONSTER_DEMON, MONSTER_SPIDER,
  MONSTER_CYBER: g_Sound_PlayExAt('SOUND_MONSTER_PAIN2', 255, FObj.X, FObj.Y);
  MONSTER_VILE: g_Sound_PlayExAt('SOUND_MONSTER_VILE_PAIN', 255, FObj.X, FObj.Y);
  MONSTER_MANCUB: g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_PAIN', 255, FObj.X, FObj.Y);
  MONSTER_PAIN: g_Sound_PlayExAt('SOUND_MONSTER_PAIN_PAIN', 255, FObj.X, FObj.Y);
  MONSTER_MAN: g_Sound_PlayExAt('SOUND_MONSTER_MAN_PAIN', 255, FObj.X, FObj.Y);
 end;
end;

procedure TMonster.DieSound();
begin
 case FMonsterType of
  MONSTER_IMP:
   case Random(2) of
    0: g_Sound_PlayExAt('SOUND_MONSTER_IMP_DIE_1', 255, FObj.X, FObj.Y);
    1: g_Sound_PlayExAt('SOUND_MONSTER_IMP_DIE_2', 255, FObj.X, FObj.Y);
   end;
  MONSTER_ZOMBY, MONSTER_SERG, MONSTER_CGUN:
   case Random(3) of
    0: g_Sound_PlayExAt('SOUND_MONSTER_DIE_1', 255, FObj.X, FObj.Y);
    1: g_Sound_PlayExAt('SOUND_MONSTER_DIE_2', 255, FObj.X, FObj.Y);
    2: g_Sound_PlayExAt('SOUND_MONSTER_DIE_3', 255, FObj.X, FObj.Y);
   end;
  MONSTER_DEMON: g_Sound_PlayExAt('SOUND_MONSTER_DEMON_DIE', 255, FObj.X, FObj.Y);
  MONSTER_BARREL: g_Sound_PlayExAt('SOUND_MONSTER_BARREL_DIE', 255, FObj.X, FObj.Y);
  MONSTER_SOUL: g_Sound_PlayExAt('SOUND_MONSTER_SOUL_DIE', 255, FObj.X, FObj.Y);
  MONSTER_BSP: g_Sound_PlayExAt('SOUND_MONSTER_BSP_DIE', 255, FObj.X, FObj.Y);
  MONSTER_VILE: g_Sound_PlayExAt('SOUND_MONSTER_VILE_DIE', 255, FObj.X, FObj.Y);
  MONSTER_BARON: g_Sound_PlayExAt('SOUND_MONSTER_BARON_DIE', 255, FObj.X, FObj.Y);
  MONSTER_CACO: g_Sound_PlayExAt('SOUND_MONSTER_CACO_DIE', 255, FObj.X, FObj.Y);
  MONSTER_CYBER: g_Sound_PlayExAt('SOUND_MONSTER_CYBER_DIE', 255, FObj.X, FObj.Y);
  MONSTER_KNIGHT: g_Sound_PlayExAt('SOUND_MONSTER_KNIGHT_DIE', 255, FObj.X, FObj.Y);
  MONSTER_MANCUB: g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_DIE', 255, FObj.X, FObj.Y);
  MONSTER_PAIN: g_Sound_PlayExAt('SOUND_MONSTER_PAIN_DIE', 255, FObj.X, FObj.Y);
  MONSTER_SKEL: g_Sound_PlayExAt('SOUND_MONSTER_SKEL_DIE', 255, FObj.X, FObj.Y);
  MONSTER_SPIDER: g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_DIE', 255, FObj.X, FObj.Y);
  MONSTER_MAN: g_Sound_PlayExAt('SOUND_MONSTER_MAN_DIE', 255, FObj.X, FObj.Y);
 end;
end;

procedure TMonster.WakeUpSound();
begin
 case FMonsterType of
  MONSTER_IMP:
   case Random(2) of
    0: g_Sound_PlayExAt('SOUND_MONSTER_IMP_ALERT_1', 255, FObj.X, FObj.Y);
    1: g_Sound_PlayExAt('SOUND_MONSTER_IMP_ALERT_2', 255, FObj.X, FObj.Y);
   end;
  MONSTER_ZOMBY, MONSTER_SERG, MONSTER_CGUN:
   case Random(3) of
    0: g_Sound_PlayExAt('SOUND_MONSTER_ALERT_1', 255, FObj.X, FObj.Y);
    1: g_Sound_PlayExAt('SOUND_MONSTER_ALERT_2', 255, FObj.X, FObj.Y);
    2: g_Sound_PlayExAt('SOUND_MONSTER_ALERT_3', 255, FObj.X, FObj.Y);
   end;
  MONSTER_MAN: g_Sound_PlayExAt('SOUND_MONSTER_MAN_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_BSP: g_Sound_PlayExAt('SOUND_MONSTER_BSP_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_VILE: g_Sound_PlayExAt('SOUND_MONSTER_VILE_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_BARON: g_Sound_PlayExAt('SOUND_MONSTER_BARON_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_CACO: g_Sound_PlayExAt('SOUND_MONSTER_CACO_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_CYBER: g_Sound_PlayExAt('SOUND_MONSTER_CYBER_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_KNIGHT: g_Sound_PlayExAt('SOUND_MONSTER_KNIGHT_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_MANCUB: g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_PAIN: g_Sound_PlayExAt('SOUND_MONSTER_PAIN_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_DEMON: g_Sound_PlayExAt('SOUND_MONSTER_DEMON_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_SKEL: g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_SPIDER: g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_ALERT', 255, FObj.X, FObj.Y);
  MONSTER_SOUL: ;
 end;
end;

procedure TMonster.BFGHit();
begin
 if FMonsterType = MONSTER_FISH then Exit;

 g_Weapon_BFGHit(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                 FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
end;

function TMonster.Collide(X, Y: Integer; Width, Height: Word): Boolean;
begin
 Result := g_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width,
                     FObj.Rect.Height, X, Y, Width, Height);
end;

function TMonster.Collide(Rect: TRectWH): Boolean;
begin
 Result := g_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width,
                     FObj.Rect.Height, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TMonster.Collide(X, Y: Integer): Boolean;
begin
 X := X-FObj.X-FObj.Rect.X;
 Y := Y-FObj.Y-FObj.Rect.Y;
 Result := (x >= 0) and (x <= FObj.Rect.Width) and
           (y >= 0) and (y <= FObj.Rect.Height);
end;

constructor TMonster.Create(MonsterType: Byte);
var
  a: Integer;
  FramesID: DWORD;
  s: string;
begin
 FUID := g_CreateUID(UID_MONSTER);

 FMonsterType := MonsterType;

 g_Obj_Init(@FObj);

 FState := STATE_SLEEP;
 FCurAnim := ANIM_SLEEP;
 FHealth := MONSTERTABLE[MonsterType].Health;
 FObj.Rect := MONSTERTABLE[MonsterType].Rect;
 FDieTriggers := nil;

 SetLength(FAnim, Length(ANIMTABLE));
 
 for a := 0 to High(FAnim) do
 begin
  FAnim[a, D_LEFT] := nil;
  FAnim[a, D_RIGHT] := nil;
 end;

 for a := ANIM_SLEEP to ANIM_PAIN do
  if (ANIMTABLE[a].name <> '') and (MONSTERTABLE2[MonsterType].AnimSpeed[a] <> 0) then
  begin
   s := 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+'_'+ANIMTABLE[a].name;

   if (not g_Frames_Exists(s)) or (not g_Frames_Get(FramesID, s)) then
   begin
    if a <> ANIM_MESS then Continue;
    
    if g_Frames_Get(FramesID, 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+
                     '_'+ANIMTABLE[ANIM_DIE].name) then
    begin
     FAnim[a, D_RIGHT] := TAnimation.Create(FramesID, ANIMTABLE[ANIM_DIE].loop,
                                            MONSTERTABLE2[MonsterType].AnimSpeed[ANIM_DIE]);
     FAnim[a, D_LEFT] := TAnimation.Create(FramesID, ANIMTABLE[ANIM_DIE].loop,
                                           MONSTERTABLE2[MonsterType].AnimSpeed[ANIM_DIE]);
     Continue;
    end;
   end;

   FAnim[a, D_RIGHT] := TAnimation.Create(FramesID, ANIMTABLE[a].loop,
                                          MONSTERTABLE2[MonsterType].AnimSpeed[a]);

   if MONSTERTABLE2[MonsterType].LeftAnim then
   begin
    s := 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+'_'+ANIMTABLE[a].name+'_L';
    if g_Frames_Exists(s) then g_Frames_Get(FramesID, s);
   end;

   FAnim[a, D_LEFT] := TAnimation.Create(FramesID, ANIMTABLE[a].loop,
                                         MONSTERTABLE2[MonsterType].AnimSpeed[a]);
  end;

 if MonsterType = MONSTER_VILE then
 begin
  g_Frames_Get(FramesID, 'FRAMES_FIRE');
  vilefire := TAnimation.Create(FramesID, True, 2);
 end else vilefire := nil;
end;

function TMonster.Damage(Damage: Word; VelX, VelY: Integer; SpawnerUID: Word; t: Byte): Boolean;
var
  c: Integer;
  p: TPlayer;
begin
 Result := False;
 
 if (FState = STATE_DEAD) or (FState = STATE_DIE) or (FState = STATE_REVIVE) then Exit;

 //g_Console_Add('damage '+IntToStr(Damage), True);

 if (t = HIT_ELECTRO) and (FMonsterType = MONSTER_FISH) then
 begin
  SetState(STATE_RUN);
  FSleep := 20;
  if Random(2) = 0 then FDirection := D_RIGHT else FDirection := D_LEFT;
  Result := True;
  Exit;
 end;

 if t = HIT_TRAP then FHealth := -100;

 if FMonsterType = MONSTER_ROBO then Damage := 0;

 Dec(FHealth, Damage);

 if FPain = 0 then FPain := 3;
 FPain := FPain+Damage;

 if FState <> STATE_PAIN then
  if (FPain >= MONSTERTABLE[FMonsterType].MinPain) and
     (FMonsterType <> MONSTER_BARREL) then SetState(STATE_PAIN);

 if (gBloodCount > 0) and (FMonsterType <> MONSTER_BARREL) then
 begin
  c := Min(Damage, 200);
  c := c*gBloodCount-(Damage div 4)+Random(c div 2);

  if (VelX = 0) and (VelY = 0) then MakeBloodSimple(c) else
  case t of
   HIT_TRAP, HIT_ACID, HIT_ELECTRO, HIT_FLAME: MakeBloodSimple(c);
   HIT_BFG, HIT_ROCKET, HIT_SOME: MakeBloodVector(c, VelX, VelY);
  end;
 end;
 FTargetUID := SpawnerUID;
 FTargetTime := 0;

 if FHealth <= 0 then
 begin
  if (FMonsterType <> MONSTER_BARREL) and (g_GetUIDType(SpawnerUID) = UID_PLAYER) then
  begin
   p := g_Player_Get(SpawnerUID);
   if p <> nil then p.MonsterKills := p.MonsterKills+1;
  end;

  case FMonsterType of
   MONSTER_ZOMBY: c := ITEM_AMMO_BULLETS;
   MONSTER_SERG: c := ITEM_WEAPON_SHOTGUN1;
   MONSTER_CGUN: c := ITEM_WEAPON_CHAINGUN;
   MONSTER_MAN: c := ITEM_KEY_RED;
   else c := 0;
  end;

  if c <> 0 then g_Items_Create(FObj.X+(FObj.Rect.Width div 2),
                                FObj.Y+(FObj.Rect.Height div 2), c, True, False);

  FObj.Vel.X := 0;
  if (FMonsterType <> MONSTER_FISH) and (FMonsterType <> MONSTER_PAIN) then
  begin
   FObj.Rect.Y := FObj.Rect.Y+FObj.Rect.Height-12;
   FObj.Rect.Height := 12;
  end;

  if (FHealth <= -30) and
     ((FMonsterType = MONSTER_IMP) or (FMonsterType = MONSTER_ZOMBY) or
      (FMonsterType = MONSTER_SERG) or (FMonsterType = MONSTER_CGUN) or
      (FMonsterType = MONSTER_MAN)) then
  begin
   g_Sound_PlayExAt('SOUND_MONSTER_SLOP', 255, FObj.X, FObj.Y);
   SetState(STATE_DIE, ANIM_MESS)
  end
   else
  begin
   DieSound();
   SetState(STATE_DIE);
  end;

  ActivateTriggers();

  FHealth := 0;
 end
  else if FState = STATE_SLEEP then
 begin
  SetState(STATE_GO);
  FPain := MONSTERTABLE[FMonsterType].Pain;
 end;

 Result := True;
end;

destructor TMonster.Destroy();
var
  a: Integer;
begin
 for a := 0 to High(FAnim) do
 begin
  if FAnim[a, D_LEFT] <> nil then FAnim[a, D_LEFT].Destroy();
  if FAnim[a, D_RIGHT] <> nil then FAnim[a, D_RIGHT].Destroy();
 end;

 if vilefire <> nil then vilefire.Destroy();

 inherited Destroy();
end;

procedure TMonster.Draw();
var
  m: TMirrorType;
  dx, dy, c: Integer;
  o: TObj;
begin
 //e_DrawFillQuad(Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.X+Obj.Rect.X+Obj.Rect.Width,
 //               Obj.Y+Obj.Rect.Y+Obj.Rect.Height, 127, 127, 127, 0, B_NONE);
 //e_DrawPoint(2, Obj.X, Obj.Y, 255, 0, 0);
 
 //e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, 'TYPE: '+IntToStr(FMonsterType));
 //e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y+16, 'STATE: '+IntToStr(FState));

 if FMonsterType = MONSTER_VILE then
  if FState = STATE_SHOOT then
   if GetPos(FTargetUID, @o) then
    vilefire.Draw(o.X+o.Rect.X+(o.Rect.Width div 2)-32, o.Y+o.Rect.Y+o.Rect.Height-128, M_NONE);

 if not g_Collide(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width, FObj.Rect.Height,
                  sX-128, sY-128, sWidth+256, sHeight+256) then Exit;

 if FState = STATE_DEAD then
  case FMonsterType of
   MONSTER_BARREL, MONSTER_SOUL, MONSTER_PAIN: Exit;
  end;

 if FAnim[FCurAnim, FDirection] <> nil then
 begin
  if FMonsterType = MONSTER_BARREL then
  begin
   if FState <> STATE_DEAD then FAnim[FCurAnim, FDirection].Draw(Obj.X, Obj.Y, M_NONE);
  end
   else
  begin
   if (((not MONSTERTABLE2[FMonsterType].LeftAnim) or (FAnim[FCurAnim, D_LEFT].FramesID = FAnim[FCurAnim, D_RIGHT].FramesID)) and
      (FDirection = D_LEFT)) then
    m := M_HORIZONTAL else m := M_NONE;

   dx := MONSTERTABLE2[FMonsterType].AnimDelta[FCurAnim].X;
   dy := MONSTERTABLE2[FMonsterType].AnimDelta[FCurAnim].Y;

   if FDirection = D_LEFT then
   begin
    c := MONSTERTABLE[FMonsterType].Rect.X+MONSTERTABLE[FMonsterType].Rect.Width;
    dx := FAnim[FCurAnim, FDirection].Width+MONSTERTABLE2[FMonsterType].AnimDelta[FCurAnim].X-c;
    dx := dx-MONSTERTABLE[FMonsterType].Rect.X;
    dx := -dx;
   end;
   
   FAnim[FCurAnim, FDirection].Draw(Obj.X+dx, Obj.Y+dy, m);
  end;
 end;
end;

procedure TMonster.MakeBloodSimple(Count: Word);
begin
 g_GFX_Blood(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)+8,
             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
             Count div 2, 3, -1, 16, (FObj.Rect.Height*2 div 3));
 g_GFX_Blood(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-8,
             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
             Count div 2, -3, -1, 16, (FObj.Rect.Height*2) div 3);
end;

procedure TMonster.MakeBloodVector(Count: Word; VelX, VelY: Integer);
begin
 g_GFX_Blood(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
             FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2),
             Count, VelX, VelY, 16, (FObj.Rect.Height*2) div 3);
end;

procedure TMonster.Push(vx, vy: Integer);
begin
 FObj.Accel.X := FObj.Accel.X+vx;
 FObj.Accel.Y := FObj.Accel.Y+vy;
end;

procedure TMonster.SetState(State: Byte; ForceAnim: Byte = 255);
var
  Anim: Byte;
begin
 if (State = STATE_DIE) and (MonsterType = MONSTER_SOUL) then soulcount := soulcount-1;

 case FState of
  STATE_DIE, STATE_DEAD,
  STATE_REVIVE: if (State <> STATE_DEAD) and (State <> STATE_REVIVE) and
                   (State <> STATE_GO) then Exit;
 end;

 FState := State;

 case FState of
  STATE_SLEEP: Anim := ANIM_SLEEP;
  STATE_PAIN: Anim := ANIM_PAIN;
  STATE_WAIT: Anim := ANIM_SLEEP;
  STATE_CLIMB, STATE_RUN, STATE_RUNOUT, STATE_GO: Anim := ANIM_GO;
  STATE_SHOOT: Anim := ANIM_ATTACK;
  STATE_ATTACK: Anim := ANIM_ATTACK;
  STATE_DIE: Anim := ANIM_DIE;
  STATE_REVIVE:
  begin
   Anim := FCurAnim;
   FAnim[Anim, FDirection].Revert(True);

   FObj.Rect := MONSTERTABLE[FMonsterType].Rect;
   FHealth := MONSTERTABLE[FMonsterType].Health;
   FAmmo := 0;
   FPain := 0;
  end;
  else Exit;
 end;

 if ForceAnim <> 255 then Anim := ForceAnim;

 if FAnim[Anim, FDirection] <> nil then
 begin
  FAnim[Anim, FDirection].Reset();
  FCurAnim := Anim;
 end;
end;

function TMonster.TeleportTo(X, Y: Integer; silent: Boolean): Boolean;
var
  TA: TAnimation;
  FramesID: DWORD;
begin
 Result := False;

 if g_CollideLevel(X, Y, FObj.Rect.Width, FObj.Rect.Height) then Exit;

 TA := nil;
 if not silent then
 begin
  if g_Frames_Get(FramesID, 'FRAMES_TELEPORT') then
   TA := TAnimation.Create(FramesID, False, 6);
  g_Sound_PlayExAt('SOUND_GAME_TELEPORT', 255, Obj.X, Obj.Y);
  g_GFX_OnceAnim(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-24,
                 FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, TA);
 end;
 
 FObj.X := X-FObj.Rect.X;
 FObj.Y := Y-FObj.Rect.Y;

 if not silent and (TA <> nil) then
 begin
  g_GFX_OnceAnim(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-24,
                 FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2)-32, TA);
  TA.Destroy;
 end;

 Result := True;
end;

procedure TMonster.Update();
var
  a, sx, sy, wx, wy: Integer;
  st: Word;
  o: TObj;
  s: string;

label
  _end, _1, _2, _3;
begin
 if gTime mod (GAME_TICK*2) <> 0 then
 begin
  st := g_Obj_Move(@FObj);
  if WordBool(st and MOVE_HITWATER) then g_Obj_Splash(@FObj);
  Exit;
 end;

 FPainSound := False;

 case FMonsterType of
  MONSTER_FISH:
   if g_Obj_CollidePanel(@FObj, 0, 0, PANEL_WATER or PANEL_ACID1 or PANEL_ACID2) then
    if (FState <> STATE_DIE) and (FState <> STATE_DEAD) then FObj.Vel.Y := FObj.Vel.Y-1;
  MONSTER_SOUL, MONSTER_PAIN, MONSTER_CACO:
   if (FState <> STATE_DIE) and (FState <> STATE_DEAD) then FObj.Vel.Y := FObj.Vel.Y-1;
 end;

 st := g_Obj_Move(@FObj);

 if WordBool(st and MOVE_FALLOUT) or (FObj.X < -1000) or
    (FObj.X > gMapInfo.Width+1000) or (FObj.Y < -1000) then
 begin
  FRemoved := True;

  ActivateTriggers();
  
  {if (FMonsterType = MONSTER_ROBO) and
     (UpperCase(ExtractFileName(gGameSettings.WAD)) = 'DOOM2D.WAD') then
  begin
   g_ProcessResourceStr(gMapInfo.Map, nil, nil, @s);
   if s = 'MAP19' then g_Game_ExitLevel('');
  end;}

  Exit;
 end;

 if WordBool(st and MOVE_HITWATER) then g_Obj_Splash(@FObj);

 if (FState = STATE_DIE) or (FState = STATE_DEAD) then
  FObj.Vel.X := z_dec(FObj.Vel.X, 1);

 if FState = STATE_DEAD then goto _end;

 if (Random(32) = 0) and WordBool(st and MOVE_INWATER) then
  case FMonsterType of
   MONSTER_FISH: if Random(4) = 0 then
                  g_GFX_Bubbles(FObj.X+FObj.Rect.X+Random(FObj.Rect.Width),
                                FObj.Y+FObj.Rect.Y+Random(4), 1, 0, 0);
   MONSTER_ROBO,
   MONSTER_BARREL: g_GFX_Bubbles(FObj.X+FObj.Rect.X+Random(FObj.Rect.Width),
                                 FObj.Y+FObj.Rect.Y+Random(4), 1, 0, 0);
   else g_GFX_Bubbles(FObj.X+FObj.Rect.X+Random(FObj.Rect.Width-4),
                      FObj.Y+FObj.Rect.Y+Random(4), 5, 4, 4);
  end;

 if FMonsterType = MONSTER_BARREL then
 begin
  if (FState = STATE_DIE) and (FAnim[FCurAnim, FDirection].CurrentFrame = 1) and
     (FAnim[FCurAnim, FDirection].Counter = 0) then
   g_Weapon_Explode(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                    FObj.Y+FObj.Rect.Y+FObj.Rect.Height-16,
                    60, FTargetUID);
 end;

 if FMonsterType = MONSTER_SOUL then
  if WordBool(st and MOVE_HITAIR) then g_Obj_SetSpeed(@FObj, 16);

 if FAmmo < 0 then FAmmo := FAmmo+1;

 if FObj.Vel.Y < 0 then
  if WordBool(st and MOVE_INWATER) then FObj.Vel.Y := -4;

 FTargetTime := FTargetTime+1;

 case FState of
  STATE_PAIN:
  begin
   if FPain >= MONSTERTABLE[FMonsterType].Pain then
   begin
    FPain := MONSTERTABLE[FMonsterType].Pain;
    PainSound();
   end;

   FPain := FPain-5;

   if FPain <= MONSTERTABLE[FMonsterType].MinPain then
   begin
    SetState(STATE_GO);
    FPain := 0;
    FAmmo := -9;
   end;
  end;

  STATE_SLEEP:
  begin
   FSleep := FSleep+1;
   if FSleep >= 18 then FSleep := 0 else goto _end;

   if gPlayers <> nil then
    for a := 0 to High(gPlayers) do
     if (gPlayers[a] <> nil) and (gPlayers[a].Live) then
      with gPlayers[a] do
       if g_Look(@FObj, @Obj, FDirection) then
       begin
        SetState(STATE_GO);
        FTargetUID := gPlayers[a].UID;
        FTargetTime := 0;
        WakeUpSound();
        Break;
       end;
  end;

  STATE_WAIT:
  begin
   FSleep := FSleep-1;
   if FSleep < 0 then SetState(STATE_GO);
  end;

  STATE_GO:
  begin
   if WordBool(st and MOVE_BLOCK) then
   begin
    Turn();
    SetState(STATE_RUNOUT);
    FSleep := 40;
    goto _end;
   end;

   if FMonsterType = MONSTER_VILE then
    if iscorpse(@FObj, False) <> -1 then 
    begin
     SetState(STATE_ATTACK, ANIM_ATTACK2);
     FObj.Vel.X := 0;
     goto _end;
    end;

   if (not GetPos(FTargetUID, @o)) or (FTargetTime > MAX_ATM) then
    if not findnewprey() then
    begin
     FTargetUID := 0;
     o.X := FObj.X+pt_x;
     o.Y := FObj.Y+pt_y;
     o.Vel := _Point(0, 0);
     o.Accel := _Point(0, 0);
     o.Rect := _Rect(0, 0, 0, 1);
    end
     else GetPos(FTargetUID, @o);

   if g_Obj_Collide(@FObj, @o) and (FTargetUID <> 0) then
   begin
    FTargetTime := 0;
    if kick(@o) then goto _end;
   end;

   sx := o.X+o.Rect.X+(o.Rect.Width div 2)-(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2));
   sy := o.Y+o.Rect.Y+(o.Rect.Height div 2)-(FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));

   if not WordBool(st and MOVE_BLOCK) then
    if Abs(sx) < 40 then
     if FMonsterType <> MONSTER_FISH then
     begin
      SetState(STATE_RUN);
      FSleep := 15;
      if Random(2) = 0 then FDirection := D_LEFT else FDirection := D_RIGHT;
      goto _end;
     end;

   if WordBool(st and MOVE_HITWALL) then
   begin
    if g_Triggers_PressR(FObj.X+FObj.Rect.X, FObj.Y+FObj.Rect.Y, FObj.Rect.Width,
                         FObj.Rect.Height, FUID, ACTIVATE_MONSTERPRESS) <> nil then
    begin
     SetState(STATE_WAIT);
     FSleep := 4;
     goto _end;
    end;

    case FMonsterType of
     MONSTER_CACO, MONSTER_SOUL, MONSTER_PAIN, MONSTER_FISH: ;
     else if g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj) then
     begin
      FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;
      SetState(STATE_CLIMB);
     end;
    end;
    
    goto _end;
   end;

   if sx > 0 then FDirection := D_RIGHT else FDirection := D_LEFT;

   if canshoot(FMonsterType) and (FTargetUID <> 0) then
    if Abs(sx) > Abs(sy) then
     if shoot(@o, False) then goto _end;

   if FMonsterType = MONSTER_FISH then
    if not WordBool(st and MOVE_INWATER) then
    begin
     if g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj) then
     begin
      FObj.Vel.Y := -6;
      FObj.Accel.X := FObj.Accel.X-8+Random(17);
     end;

     SetState(STATE_PAIN);
     FPain := FPain+50;

     goto _1;
    end;

   if (FMonsterType = MONSTER_CACO) or (FMonsterType = MONSTER_SOUL) or
      (FMonsterType = MONSTER_PAIN) or (FMonsterType = MONSTER_FISH) then
   begin
    if Abs(sy) > 8 then FObj.Vel.Y := g_basic.Sign(sy)*4 else FObj.Vel.Y := 0;

    if FMonsterType = MONSTER_FISH then
     if FObj.Vel.Y < 0 then
      if not g_Obj_CollideWater(@FObj, 0, -16) then
      begin
       FObj.Vel.Y := 0;
       SetState(STATE_RUN);
       if Random(2) = 0 then FDirection := D_LEFT else FDirection := D_RIGHT;
       FSleep := 20;
      end;
   end
    else
   begin
    if sy < -40 then
     if g_Obj_CollideLevel(@FObj, 0, 1) or g_Obj_StayOnStep(@FObj) then
      if Random(4) = 0 then FObj.Vel.Y := -MONSTERTABLE[FMonsterType].Jump;
   end;

   _1:

   FSleep := FSleep+1;

   if FSleep >= 8 then
   begin
    FSleep := 0;
    if Random(8) = 0 then ActionSound();
   end;

   if FDirection = D_RIGHT then FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
    else FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

   if WordBool(st and MOVE_INWATER) then FObj.Vel.X := FObj.Vel.X div 2
    else if FMonsterType = MONSTER_FISH then FObj.Vel.X := 0;
  end;

  STATE_RUN:
  begin
   if WordBool(st and MOVE_BLOCK) then
   begin
    SetState(STATE_RUNOUT);
    Turn();
    FSleep := 40;

    goto _end;
   end;

   FSleep := FSleep-1;

   if (FSleep <= 0) or (WordBool(st and MOVE_HITWALL) and (FObj.Vel.Y+FObj.Accel.Y = 0)) then
   begin
    SetState(STATE_GO);
    FSleep := 0;
    if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then Turn();
    if Random(8) = 0 then ActionSound();
   end;

   if FDirection = D_RIGHT then FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
    else FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

   if WordBool(st and MOVE_INWATER) then FObj.Vel.X := FObj.Vel.X div 2
    else if FMonsterType = MONSTER_FISH then FObj.Vel.X := 0;
  end;

  STATE_RUNOUT:
  begin
   if (not WordBool(st and MOVE_BLOCK)) and (FSleep > 0) then FSleep := 0;
   FSleep := FSleep-1;

   if FSleep <= -18 then
   begin
    SetState(STATE_GO);
    FSleep := 0;
    if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then Turn();
    if Random(8) = 0 then ActionSound();
   end;

   if FDirection = D_RIGHT then FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
    else FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

   if WordBool(st and MOVE_INWATER) then FObj.Vel.X := FObj.Vel.X div 2
    else if FMonsterType = MONSTER_FISH then FObj.Vel.X := 0;
  end;

  STATE_CLIMB:
  begin
   if (FObj.Vel.Y+FObj.Accel.Y >= 0) or not WordBool(st and MOVE_HITWALL) then
   begin
    SetState(STATE_GO);
    FSleep := 0;

    if WordBool(st and (MOVE_HITWALL or MOVE_BLOCK)) then
    begin
     Turn();
     SetState(STATE_RUN);
     FSleep := 15;
    end;
   end;

   if FDirection = D_RIGHT then FObj.Vel.X := MONSTERTABLE[FMonsterType].RunVel
    else FObj.Vel.X := -MONSTERTABLE[FMonsterType].RunVel;

   if WordBool(st and MOVE_INWATER) then FObj.Vel.X := FObj.Vel.X div 2
    else if FMonsterType = MONSTER_FISH then FObj.Vel.X := 0;
  end;

  STATE_ATTACK, STATE_SHOOT:
  begin
   if FMonsterType = MONSTER_SOUL then
   begin
    if WordBool(st and (MOVE_HITWALL or MOVE_HITCEIL or MOVE_HITLAND)) then SetState(STATE_GO);
    goto _end;
   end;

   if FMonsterType <> MONSTER_FISH then FObj.Vel.X := z_dec(FObj.Vel.X, 1);
   if (FMonsterType = MONSTER_VILE) and (FState = STATE_SHOOT) then
   begin
    if not GetPos(FTargetUID, @o) then
    begin
     SetState(STATE_GO);
     goto _end;
    end;

    if not g_Look(@FObj, @o, FDirection) then
    begin
     SetState(STATE_GO);
     goto _end;
    end;

    if g_Obj_CollideWater(@o, 0, 0) then
    begin
     SetState(STATE_GO);
     goto _end;
    end;

    tx := o.X+o.Rect.X+(o.Rect.Width div 2);
    ty := o.Y+o.Rect.Y+(o.Rect.Height div 2);
    g_Weapon_HitUID(FTargetUID, 2, FUID, HIT_SOME);
   end;
  end;
 end;

 _end:

 if FState = STATE_REVIVE then
  if FAnim[FCurAnim, FDirection].Played then
  begin
   FAnim[FCurAnim, FDirection].Revert(False);
   SetState(STATE_GO);
  end;

 if vilefire <> nil then vilefire.Update();

 if FAnim[FCurAnim, FDirection] <> nil then
 if (FAnim[FCurAnim, FDirection].Played){ or
    ((FState = STATE_ATTACK) and (FAnim[FCurAnim, FDirection].CurrentFrame =
                                  FAnim[FCurAnim, FDirection].TotalFrames-1))} then
  case FState of
   STATE_ATTACK:
   begin
    if FMonsterType = MONSTER_SOUL then FAnim[FCurAnim, FDirection].Reset();
    case FMonsterType of
     MONSTER_SOUL, MONSTER_IMP, MONSTER_DEMON:
      if g_Weapon_Hit(@FObj, 15, FUID, HIT_SOME) <> 0 then
       if FMonsterType = MONSTER_SOUL then SetState(STATE_GO);

     MONSTER_FISH: if g_Weapon_Hit(@FObj, 10, FUID, HIT_SOME) <> 0 then
                    g_Sound_PlayExAt('SOUND_MONSTER_FISH_ATTACK', 255, FObj.X, FObj.Y);
     
     MONSTER_SKEL, MONSTER_ROBO:
     if FCurAnim = ANIM_ATTACK2 then
     begin
      o := FObj;
      o.Vel.X := IfThen(FDirection = D_RIGHT, 1, -1)*50;
      if g_Weapon_Hit(@o, 50, FUID, HIT_SOME) <> 0 then
       g_Sound_PlayExAt('SOUND_MONSTER_SKEL_HIT', 255, FObj.X, FObj.Y);
     end;

     MONSTER_VILE:
     if FCurAnim = ANIM_ATTACK2 then
     begin
      sx := iscorpse(@FObj, True);
      if sx = -1 then goto _2;

      gMonsters[sx].SetState(STATE_REVIVE);
      g_Sound_PlayExAt('SOUND_MONSTER_SLOP', 255, FObj.X, FObj.Y);

      g_Weapon_HitUID(gMonsters[sx].FUID, 5, 0, HIT_SOME);
     end;
    end;

    _2:

    if (FMonsterType <> MONSTER_SOUL) and (FState <> STATE_DIE) then SetState(STATE_GO);
   end;

   STATE_SHOOT:
   begin
    wx := MONSTERTABLE2[FMonsterType].wX;
    if FDirection = D_LEFT then
    begin
     wx := MONSTERTABLE2[FMonsterType].wX-(MONSTERTABLE[FMonsterType].Rect.X+(MONSTERTABLE[FMonsterType].Rect.Width div 2));
     wx := MONSTERTABLE[FMonsterType].Rect.X+(MONSTERTABLE[FMonsterType].Rect.Width div 2)-wx;
    end;
    wx := wx+FObj.X;
    wy := FObj.Y+MONSTERTABLE2[FMonsterType].wY;

    case FMonsterType of
     MONSTER_IMP: g_Weapon_ball1(wx, wy, tx, ty, FUID);
     MONSTER_ZOMBY:
     begin
      g_Sound_PlayExAt('SOUND_WEAPON_FIREPISTOL', 255, wx, wy);
      g_Weapon_gun(wx, wy, tx, ty, 1, FUID, False);
     end;
     MONSTER_SERG: g_Weapon_shotgun(wx, wy, tx, ty, FUID);
     MONSTER_MAN:
     begin
      g_Weapon_dshotgun(wx, wy, tx, ty, FUID);
      FAmmo := -36;
     end;
     MONSTER_CYBER: g_Weapon_rocket(wx, wy, tx, ty, FUID);
     MONSTER_SKEL: g_Weapon_revf(wx, wy, tx, ty, FUID, FTargetUID);
     MONSTER_CGUN,
     MONSTER_SPIDER: g_Weapon_mgun(wx, wy, tx, ty, FUID);
     MONSTER_BSP: g_Weapon_aplasma(wx, wy, tx, ty, FUID);
     MONSTER_ROBO: g_Weapon_plasma(wx, wy, tx, ty, FUID);
     MONSTER_MANCUB: g_Weapon_manfire(wx, wy, tx, ty, FUID);
     MONSTER_BARON,
     MONSTER_KNIGHT: g_Weapon_ball7(wx, wy, tx, ty, FUID);
     MONSTER_CACO: g_Weapon_ball2(wx, wy, tx, ty, FUID);
     MONSTER_PAIN:
     begin
      sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+(FObj.Rect.Width div 2), FObj.Y+FObj.Rect.Y, FDirection);
      if sx <> -1 then
      begin
       gMonsters[sx].FTargetUID := FTargetUID;
       GetPos(FTargetUID, @o);
       gMonsters[sx].FTargetTime := 0;
       gMonsters[sx].SetState(STATE_GO);
       gMonsters[sx].shoot(@o, True);
      end;
     end;
    end;

    if (FMonsterType = MONSTER_CGUN) or (FMonsterType = MONSTER_SPIDER) or
       (FMonsterType = MONSTER_BSP) or (FMonsterType = MONSTER_MANCUB) or
       (FMonsterType = MONSTER_ROBO) then
     if not GetPos(FTargetUID, @o) then findnewprey()
      else if shoot(@o, False) then goto _3;

    SetState(STATE_GO);

    _3:
   end;

   STATE_DIE:
   begin
    SetState(STATE_DEAD);

    if FMonsterType = MONSTER_PAIN then
    begin
     sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-30,
                             FObj.Y+FObj.Rect.Y+20, D_LEFT);
     if sx <> -1 then gMonsters[sx].SetState(STATE_GO);

     sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2),
                             FObj.Y+FObj.Rect.Y+20, D_RIGHT);
     if sx <> -1 then gMonsters[sx].SetState(STATE_GO);

     sx := g_Monsters_Create(MONSTER_SOUL, FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2)-15,
                             FObj.Y+FObj.Rect.Y, D_RIGHT);
     if sx <> -1 then gMonsters[sx].SetState(STATE_GO);

     FObj.Rect.Y := FObj.Rect.Y+FObj.Rect.Height-12;
     FObj.Rect.Height := 12;
    end;
   end;
  end;

 if FAnim[FCurAnim, FDirection].Counter = FAnim[FCurAnim, FDirection].Speed-1 then
 case FState of
  STATE_GO, STATE_RUN, STATE_CLIMB, STATE_RUNOUT:
   case FMonsterType of
    MONSTER_CYBER: if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
                      (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
                    g_Sound_PlayExAt('SOUND_MONSTER_CYBER_WALK', 255, FObj.X, FObj.Y);
    MONSTER_SPIDER: if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
                       (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
                     g_Sound_PlayExAt('SOUND_MONSTER_SPIDER_WALK', 255, FObj.X, FObj.Y);
    MONSTER_BSP: if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
                    (FAnim[FCurAnim, FDirection].CurrentFrame = 2) then
                  g_Sound_PlayExAt('SOUND_MONSTER_BSP_WALK', 255, FObj.X, FObj.Y);
    MONSTER_ROBO: if (FAnim[FCurAnim, FDirection].CurrentFrame = 0) or
                     (FAnim[FCurAnim, FDirection].CurrentFrame = 5) then
                   g_Sound_PlayExAt('SOUND_MONSTER_BSP_WALK', 255, FObj.X, FObj.Y);
   end;
 end;

 if FAnim[FCurAnim, FDirection] <> nil then FAnim[FCurAnim, FDirection].Update();
end;

procedure TMonster.Turn();
begin
 if FDirection = D_LEFT then FDirection := D_RIGHT else FDirection := D_LEFT;
end;

function TMonster.findnewprey(): Boolean;
var
  a: DWORD;
  l, l2: Integer;
begin
 Result := False;

 FTargetUID := 0;
 l := 32000;

 if gPlayers <> nil then
  for a := 0 to High(gPlayers) do
   if (gPlayers[a] <> nil) and (gPlayers[a].Live) then
   begin
    l2 := Abs(gPlayers[a].GameX-FObj.X)+
          Abs(gPlayers[a].GameY-FObj.Y);
    if l2 < l then
    begin
     l := l2;
     FTargetUID := gPlayers[a].UID;
    end;
   end;

 if FTargetUID = 0 then
  if gMonsters <> nil then
   for a := 0 to High(gMonsters) do
    if (gMonsters[a] <> nil) and (gMonsters[a].Live) and
       (gMonsters[a].FUID <> FUID) and
       not IsFriend(gMonsters[a].FMonsterType, FMonsterType) then
    begin
     l2 := Abs(gMonsters[a].FObj.X-FObj.X)+
           Abs(gMonsters[a].FObj.Y-FObj.Y);
     if l2 < l then
     begin
      l := l2;
      FTargetUID := gMonsters[a].FUID;
     end;
    end;

 if FTargetUID = 0 then
 begin
  FTargetTime := MAX_ATM;
  Exit;
 end;

 FTargetTime := 0;
 Result := True;
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
   g_Sound_PlayExAt('SOUND_MONSTER_DEMON_ATTACK', 255, FObj.X, FObj.Y);
   Result := True;
  end;
  MONSTER_IMP:
  begin
   SetState(STATE_ATTACK);
   g_Sound_PlayExAt('SOUND_MONSTER_IMP_ATTACK', 255, FObj.X, FObj.Y);
   Result := True;
  end;
  MONSTER_SKEL:
  begin
   SetState(STATE_ATTACK, ANIM_ATTACK2);
   g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ATTACK', 255, FObj.X, FObj.Y);
   Result := True;
  end;
  MONSTER_ROBO:
  begin
   SetState(STATE_ATTACK, ANIM_ATTACK2);
   g_Sound_PlayExAt('SOUND_MONSTER_SKEL_ATTACK', 255, FObj.X, FObj.Y);
   Result := True;
  end;
  MONSTER_BARON, MONSTER_KNIGHT,
  MONSTER_CACO, MONSTER_MANCUB: Result := shoot(o, True);
 end;
end;

function TMonster.shoot(o: PObj; n: Boolean): Boolean;
var
  xd, yd, m: Integer;
begin
 Result := False;

 if FAmmo < 0 then Exit;

 if not n then
  case FMonsterType of
   MONSTER_FISH, MONSTER_BARREL, MONSTER_DEMON: Exit;
   MONSTER_CGUN, MONSTER_BSP, MONSTER_ROBO:
   begin
    FAmmo := FAmmo+1;
    if FAmmo >= 50 then FAmmo := IfThen(FMonsterType = MONSTER_ROBO, -200, -50);
   end;
   MONSTER_MAN: ;
   MONSTER_MANCUB:
   begin
    FAmmo := FAmmo+1;
    if FAmmo >= 5 then FAmmo := -50;
   end;
   MONSTER_SPIDER:
   begin
    FAmmo := FAmmo+1;
    if FAmmo >= 100 then FAmmo := -50;
   end;
   MONSTER_CYBER:
   begin
    if Random(2) = 0 then Exit;
    FAmmo := FAmmo+1;
    if FAmmo >= 10 then FAmmo := -50;
   end;
   MONSTER_BARON, MONSTER_KNIGHT: if Random(8) <> 0 then Exit;
   MONSTER_SKEL: if Random(32) <> 0 then Exit;
   MONSTER_VILE: if Random(8) <> 0 then Exit;
   MONSTER_PAIN: if Random(8) <> 0 then Exit;
   else if Random(16) <> 0 then Exit;
  end;

 if not g_Look(@FObj, o, FDirection) then Exit;

 FTargetTime := 0;

 tx := o^.X+o^.Rect.Y+(o^.Rect.Width div 2)+(o^.Vel.X{+o^.Accel.X})*12;
 ty := o^.Y+o^.Rect.Y+(o^.Rect.Height div 2)+(o^.Vel.Y{+o^.Accel.Y})*12;

 if Abs(tx-(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2))) <
    Abs(ty-(FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2))) then Exit;

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
  begin
   tx := o^.X+o^.Rect.X+(o^.Rect.Width div 2);
   ty := o^.Y+o^.Rect.Y;
   SetState(STATE_SHOOT);

   vilefire.Reset();

   g_Sound_PlayExAt('SOUND_FIRE', 255, FObj.X, FObj.Y);
   g_Sound_PlayExAt('SOUND_MONSTER_VILE_ATTACK', 255, FObj.X, FObj.Y);
  end;
  MONSTER_SOUL:
  begin
   SetState(STATE_ATTACK);
   g_Sound_PlayExAt('SOUND_MONSTER_SOUL_ATTACK', 255, FObj.X, FObj.Y);

   xd := tx-(FObj.X+FObj.Rect.X+(FObj.Rect.Width div 2));
   yd := ty-(FObj.Y+FObj.Rect.Y+(FObj.Rect.Height div 2));
   m := Max(Abs(xd), Abs(yd));
   if m = 0 then m := 1;

   FObj.Vel.X := (xd*16) div m;
   FObj.Vel.Y := (yd*16) div m;
  end;
  MONSTER_MANCUB, MONSTER_ZOMBY, MONSTER_SERG, MONSTER_BSP, MONSTER_ROBO,
  MONSTER_CYBER, MONSTER_CGUN, MONSTER_SPIDER, MONSTER_PAIN, MONSTER_MAN:
  begin
   if FMonsterType = MONSTER_MANCUB then
    if FAmmo = 1 then g_Sound_PlayExAt('SOUND_MONSTER_MANCUB_ATTACK', 255, FObj.X, FObj.Y);

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

procedure TMonster.Load(Rec: PMonsterSaveRec);
var
  a: Integer;
begin
 FUID := Rec^.UID;
 loadobj(@Rec^.Obj, @FObj);
 FHealth := Rec^.Health;
 FDirection := Rec^.Direction;

 for a := ANIM_SLEEP to ANIM_PAIN do
 begin
  if FAnim[a, D_LEFT] <> nil then
   FAnim[a, D_LEFT].Load(@Rec^.Anim[a, D_LEFT]);
  if FAnim[a, D_RIGHT] <> nil then
   FAnim[a, D_RIGHT].Load(@Rec^.Anim[a, D_RIGHT]);
 end;

 for a := 0 to 15 do
  if Rec^.DieTriggers[a] = -1 then Break
   else AddTrigger(Rec^.DieTriggers[a]);

 FCurAnim := Rec^.CurAnim;
 FState := Rec^.State;
 FTargetUID := Rec^.TargetUID;
 FTargetTime := Rec^.TargetTime;
 FMonsterType := Rec^.MonsterType;
 FRemoved := Rec^.Removed;
 FAmmo := Rec^.Ammo;
 FPain := Rec^.Pain;
 FSleep := Rec^.Sleep;
 FPainSound := Rec^.PainSound;
 tx := Rec^.tx;
 ty := Rec^.ty;
 if vilefire <> nil then vilefire.load(@Rec^.vilefire);
end;

procedure TMonster.Save(Rec: PMonsterSaveRec);
var
  a: Integer;
begin
 Rec^.UID := FUID;
 saveobj(@FObj, @Rec^.Obj);
 Rec^.Health := FHealth;
 Rec^.Direction := FDirection;

 for a := ANIM_SLEEP to ANIM_PAIN do
 begin
  if FAnim[a, D_LEFT] <> nil then
   FAnim[a, D_LEFT].Save(@Rec^.Anim[a, D_LEFT]);
  if FAnim[a, D_RIGHT] <> nil then
   FAnim[a, D_RIGHT].Save(@Rec^.Anim[a, D_RIGHT]);
 end;

 for a := 0 to 15 do Rec^.DieTriggers[a] := -1;
 for a := 0 to Min(15, High(FDieTriggers)) do
  Rec^.DieTriggers[a] := FDieTriggers[a];

 Rec^.CurAnim := FCurAnim;
 Rec^.State := FState;
 Rec^.TargetUID := FTargetUID;
 Rec^.TargetTime := FTargetTime;
 Rec^.MonsterType := FMonsterType;
 Rec^.Removed := FRemoved;
 Rec^.Ammo := FAmmo;
 Rec^.Pain := FPain;
 Rec^.Sleep := FSleep;
 Rec^.PainSound := FPainSound;
 Rec^.tx := tx;
 Rec^.ty := ty;
 if vilefire <> nil then vilefire.Save(@Rec^.vilefire);
end;

procedure TMonster.ActivateTriggers();
var
  a: Integer;
begin
 if FDieTriggers <> nil then
  for a := 0 to High(FDieTriggers) do
   g_Triggers_Press(FDieTriggers[a]);
end;

procedure TMonster.AddTrigger(t: Integer);
begin
 SetLength(FDieTriggers, Length(FDieTriggers)+1);
 FDieTriggers[High(FDieTriggers)] := t;
end;

end.
