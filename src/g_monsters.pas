unit g_monsters;

{$DEFINE DEBUG}

interface

uses
  g_basic, windows, e_graphics, g_phys, g_textures;

const
  MONSTER_NONE   = 0;
  MONSTER_DEMON  = 1;
  MONSTER_IMP    = 2;
  MONSTER_ZOMBY  = 3;
  MONSTER_SERG   = 4;
  MONSTER_CYBER  = 5;
  MONSTER_CGUN   = 6;
  MONSTER_BARON  = 7;
  MONSTER_KNIGHT = 8;
  MONSTER_CACO   = 9;
  MONSTER_SOUL   = 10;
  MONSTER_PAIN   = 11;
  MONSTER_SPIDER = 12;
  MONSTER_BSP    = 13;
  MONSTER_MANCUB = 14;
  MONSTER_SKEL   = 15;
  MONSTER_VILE   = 16;
  MONSTER_FISH   = 17;
  MONSTER_BARREL = 18;
  MONSTER_ROBO   = 19;
  MONSTER_MAN    = 20;

  STATE_SLEEP  = 0;
  STATE_GO     = 1;
  STATE_DIE    = 2;
  STATE_DEAD   = 3;
  STATE_REVIVE = 4;
  STATE_PAIN   = 5;
  STATE_WAIT   = 6;
  STATE_ATTACK = 7;
  STATE_FIRE   = 8;

  ANIM_SLEEP  = 0;
  ANIM_GO     = 1;
  ANIM_DIE    = 2;
  ANIM_MESS   = 3;
  ANIM_REVIVE = 4;
  ANIM_ATTACK = 5;
  ANIM_PAIN   = 6;

type
  TMonster = record
   MonsterType: Byte;
   UID: Word;
   Obj: TObj;
   Direction: TDirection;
   Health: SmallInt;
   Live: Boolean;
   State: Byte;
   Ammo: Integer;
   TargetUID: Word;
   LastSpawnerUID: Word;
   ExplodeTimer: Byte;
   Anim: array[ANIM_SLEEP..ANIM_PAIN] of array[D_LEFT..D_RIGHT] of TAnimation;
   CurAnim: Byte;
   sleep: Integer;
   pain: Integer;
   atm: Word;
  end;

procedure g_Monsters_LoadData();
procedure g_Monsters_FreeData();
procedure g_Monsters_Init();
procedure g_Monsters_Free();
procedure g_Monsters_Create(fMonsterType: Byte; fX, fY: Integer; fDirection: TDirection);
procedure g_Monsters_Update();
procedure g_Monsters_Draw();
function g_Monsters_Collide(ID: DWORD; fX, fY: Integer; fWidth, fHeight: Word): Boolean;
function g_Monsters_Get(UID: Word): DWORD;
procedure g_Monsters_Damage(ID: DWORD; Damage: Word; Angle: SmallInt; SpawnerUID: Word);
procedure g_Monsters_Push(ID: DWORD; Vel: Single; Angle: SmallInt);
function g_Monsters_Teleport(ID: DWORD; X, Y: Integer): Boolean;

var
  gMonsters: array of TMonster = nil;

implementation

uses
  e_log, g_menu, SysUtils, g_game, Math, g_player, g_weapons, 
  g_main, g_sound, g_console, g_gfx, g_triggers, MAPDEF;

const
  MAX_ATM = 300;
  MONSTERTABLE: array[MONSTER_DEMON..MONSTER_MAN] of
                record
                 Name: string;
                 Rect: TRectWH;
                 Health: Word;
                 AnimSpeed: array[ANIM_SLEEP..ANIM_PAIN] of Byte;
                 LeftAnim: Boolean;
                 ClearDead: Boolean;
                 RunVel: Byte;
                 MinPain: Byte;
                 Pain: Byte;
                end =
   ((Name:'DEMON'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'IMP'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'ZOMBY'; Rect:(X:12; Y:12; Width:39; Height:52); Health:15;
     AnimSpeed:(15, 10, 10, 10, 10, 10, 10); LeftAnim: False; ClearDead: False;
     RunVel: 2; MinPain: 0; Pain: 33),
    (Name:'SERG'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'CYBER'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'CGUN'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'BARON'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'KNIGHT'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'CACO'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'SOUL'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'PAIN'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'SPIDER'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'BSP'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'MANCUB'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'SKEL'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'VILE'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'FISH'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'BARREL'; Rect:(X:18; Y:26; Width:24; Height:37); Health:20;
     AnimSpeed:(15, 0, 10, 0, 0, 0, 30); LeftAnim: False; ClearDead: True;
     RunVel: 0; MinPain: 0; Pain: 0),
    (Name:'ROBO'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0),
    (Name:'MAN'; Rect:(X:0; Y:0; Width:0; Height:0); Health:0));

  ANIMTABLE: array[ANIM_SLEEP..ANIM_PAIN] of
              record
               name: string;
               loop: Boolean;
              end = ((name: 'SLEEP'; loop: True),
                     (name: 'GO'; loop: True),
                     (name: 'DIE'; loop: False),
                     (name: 'MESS'; loop: False),
                     (name: ''; loop: False),
                     (name: 'ATTACK'; loop: True),
                     (name: 'PAIN'; loop: False));
                     
function FindMonster: DWORD;
var
  i: Integer;
begin
 if gMonsters <> nil then
 for i := 0 to High(gMonsters) do
  if gMonsters[i].MonsterType = MONSTER_NONE then
  begin
   Result := i;
   Exit;
  end;

 if gMonsters = nil then
 begin
  SetLength(gMonsters, 8);
  Result := 0;
 end
  else
 begin
  Result := High(gMonsters) + 1;
  SetLength(gMonsters, Length(gMonsters) + 8);
 end;
end;

function IsFriend(a, b: Byte): Boolean;
begin
 if (a = MONSTER_BARREL) or (b = MONSTER_BARREL) then
 begin
  Result := True;
  Exit;
 end;

 if a = b then
  case a of
   MONSTER_IMP, MONSTER_DEMON, MONSTER_BARON, MONSTER_CACO,
   MONSTER_SOUL, MONSTER_MANCUB, MONSTER_FISH:
   begin
    Result := True;
    Exit;
   end;
  end;

 if (a = MONSTER_SOUL) and (b = MONSTER_PAIN) then
 begin
  Result := True;
  Exit;
 end;
 
 if (b = MONSTER_SOUL) and (a = MONSTER_PAIN) then
 begin
  Result := True;
  Exit;
 end;

 Result := False;
end;

procedure SetDir(ID: DWORD; d: TDirection);
begin
 with gMonsters[ID] do
 begin
  if d = Direction then Exit;

  Direction := d;
  Anim[CurAnim, Direction].Reset();
 end;
end;

procedure SetState(ID: DWORD; st: Byte; anm: Byte = 255);
begin
 with gMonsters[ID] do
 begin
  if (State = st) and (anm = CurAnim) then Exit;

  State := st;

  if anm = 255 then
   case State of
    STATE_SLEEP: anm := ANIM_SLEEP;
    STATE_GO: anm := ANIM_GO;
    STATE_DIE: anm := ANIM_DIE;
    STATE_DEAD: anm := CurAnim;
    STATE_REVIVE: anm := ANIM_REVIVE;
    STATE_ATTACK: anm := ANIM_ATTACK;
    STATE_PAIN: anm := ANIM_PAIN;
   end;

  if (Anim[anm, Direction] <> nil) and (anm <> CurAnim) then
   Anim[anm, Direction].Reset();

  CurAnim := anm;
 end;
end;

function GetPos(UID: Word; var o: TObj): Boolean;
begin
 Result := False;

 case g_GetUIDType(UID) of
  UID_PLAYER:
   with g_Player_Get(UID) do
   begin
    if not Live then Exit;
    o.X := GameX;
    o.Y := GameY;
    o.Rect := PLAYER_RECT;
    o.Vel := Vel;
   end;
  UID_MONSTER:
   with gMonsters[g_Monsters_Get(UID)] do
   begin
    if Live then Exit;
    o := Obj;
   end;
  else Exit;
 end;

 Result := True;
end;

function CanShoot(t: Byte): Boolean;
begin
 case t of
  MONSTER_DEMON, MONSTER_FISH, MONSTER_BARREL: Result := False;
  else Result := True;
 end;
end;

function shoot(ID: DWORD; o: PObj; n: Integer): Boolean;
var
  xd, yd, m: Integer;
begin
 Result := False;

 if gMonsters[ID].Ammo < 0 then Exit;

 with gMonsters[ID] do
 begin
  if n <> 0 then
   case MonsterType of
    MONSTER_FISH, MONSTER_BARREL, MONSTER_DEMON: Exit;
    MONSTER_CGUN, MONSTER_BSP, MONSTER_ROBO:
    begin
     Inc(Ammo);
     if Ammo >= 50 then Ammo := IfThen(MonsterType = MONSTER_ROBO, -200, -50);
    end;
    MONSTER_MAN: ;
    MONSTER_MANCUB:
    begin
     Inc(Ammo);
	   if Ammo >= 5 then Ammo := -50;
    end;
    MONSTER_SPIDER:
	  begin
     Inc(Ammo);
     if Ammo >= 100 then Ammo := -50;
    end;
    MONSTER_CYBER:
	  begin
     if Random(2) <> 0 then Exit;
     Inc(Ammo);
	   if Ammo >= 10 then Ammo := -50;
	  end;
    MONSTER_BARON, MONSTER_KNIGHT: if Random(8) <> 0 then Exit;
    MONSTER_SKEL: if Random(32) <> 0 then Exit;
    MONSTER_VILE: if Random(8) <> 0 then Exit;
    MONSTER_PAIN: if Random(8) <> 0 then Exit;
	  else if Random(16) <> 0 then Exit;
   end;

  if not g_Look(@Obj, o, Direction) then Exit;

  atm := 0;

   //mn[i].atm=0;
   //mn[i].tx=o->x+(o->xv+o->vx)*6;
   //mn[i].ty=o->y-o->h/2+(o->yv+o->vy)*6;
   //if(abs(mn[i].tx-mn[i].o.x)<abs(mn[i].ty-mn[i].o.y+mn[i].o.h/2)) return 0;

   case MonsterType of
    MONSTER_IMP, MONSTER_BARON,
    MONSTER_KNIGHT, MONSTER_CACO:
    begin
     SetState(ID, STATE_ATTACK);
     //Z_sound(firsnd,128);
    end;
	  MONSTER_SKEL:
    begin
	   SetState(ID, STATE_ATTACK);
     //Z_sound(snd[MN_SKEL-1][2],128);
    end;
	  MONSTER_VILE:
    begin
	   //mn[i].tx=o->x;
     //mn[i].ty=o->y;
	   SetState(ID, STATE_ATTACK);
     //Z_sound(fsnd,128);
	   //Z_sound(snd[MN_VILE-1][2],128);
    end;
    MONSTER_SOUL:
    begin
	   SetState(ID, STATE_ATTACK);
     //Z_sound(snd[MN_SOUL-1][2],128);
	   //yd=mn[i].ty-mn[i].o.y+mn[i].o.h/2;
     //xd=mn[i].tx-mn[i].o.x;
	   //if(!(m=max(abs(xd),abs(yd)))) m=1;
	   //mn[i].o.xv=xd*16/m;
     //mn[i].o.yv=yd*16/m;
    end;
	  MONSTER_MANCUB: if Ammo = 1 then ;//Z_sound(snd[MN_MANCUB-1][2],128);
	  MONSTER_ZOMBY, MONSTER_SERG, MONSTER_BSP, MONSTER_ROBO,
	  MONSTER_CYBER, MONSTER_CGUN, MONSTER_SPIDER,
	  MONSTER_PAIN, MONSTER_MAN: SetState(ID, STATE_ATTACK);
	  else Exit;
   end;

  Result := True;;
 end;
end;

function kick(ID: DWORD; o: PObj): Boolean;
begin
 Result := False;
{ case gMonsters[ID].MonsterType of
	MONSTER_FISH:
   begin
    setst(ID, STATE_ATTACK);
    Result := True;
   end;
	MONSTER_DEMON:
	 begin
    setst(ID, STATE_ATTACK);
    //Z_sound(snd[0][2],128);
    Result := True;
   end;
	MONSTER_IMP:
	 begin
    setst(ID, STATE_ATTACK);
    //Z_sound(snd[1][2],128);
    Result := True;
   end;
	MONSTER_SKEL:
	 begin
    setst(ID, STATE_ATTACK);
    //Z_sound(swgsnd,128);
    Result := True;
   end;
	MONSTER_ROBO:
	 begin
    setst(ID, STATE_ATTACK);
    //Z_sound(swgsnd,128);
    Result := True;
   end;
	MONSTER_BARON, MONSTER_KNIGHT,
  MONSTER_CACO, MONSTER_MANCUB: Result := shoot(ID, o, 1);
  else Result := False;
 end;}
end;

function FindNewPrey(ID: DWORD): Boolean;
var
  a: DWORD;
  l, l2: Integer;
  b: Word;
begin
 b := UID_NONE;
 l := 32000;

 if gPlayers <> nil then
  for a := 0 to High(gPlayers) do
   if (gPlayers[a] <> nil) and (gPlayers[a].Live) then
   begin
    l2 := Abs(gPlayers[a].GameX-gMonsters[ID].Obj.X)+
          Abs(gPlayers[a].GameY-gMonsters[ID].Obj.Y);
    if l2 < l then
    begin
     l := l2;
     b := gPlayers[a].UID;
    end;
   end;

 if b = UID_NONE then
  if gMonsters <> nil then
   for a := 0 to High(gMonsters) do
    if (gMonsters[a].MonsterType <> MONSTER_NONE) and (gMonsters[a].Live) and
       (a <> ID) and not IsFriend(gMonsters[a].MonsterType, gMonsters[ID].MonsterType) then
    begin
     l2 := Abs(gMonsters[a].Obj.X-gMonsters[ID].Obj.X)+
           Abs(gMonsters[a].Obj.Y-gMonsters[ID].Obj.Y);
     if l2 < l then
     begin
      l := l2;
      b := gMonsters[a].UID;
     end;
    end;

 gMonsters[ID].UID := b;

 Result := b <> UID_NONE;

 if Result then gMonsters[ID].atm := 0 else gMonsters[ID].atm := MAX_ATM;
end;

function IsCorpse(o: PObj; force: Boolean): Integer;
var
  a: Integer;
begin
 Result := -1;
 
 if not force then if Random(7) <> 0 then Exit;

 if gMonsters <> nil then
  for a := 0 to High(gMonsters) do
   with gMonsters[a] do
    if MonsterType <> MONSTER_NONE then
     if not Live then
      if g_Obj_Collide(o, @Obj) then
       case MonsterType of
        MONSTER_SOUL, MONSTER_PAIN, MONSTER_CYBER,
        MONSTER_SPIDER, MONSTER_VILE, MONSTER_BARREL: Continue;
        else if State = STATE_DEAD then
        begin
         Result := a;
         Exit;
        end
       end;
end;

procedure g_Monsters_LoadData();
begin
 e_WriteLog('Loading monsters data...', MSG_NOTIFY);

 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_SLEEP', GameWAD+':MTEXTURES\BARREL_SLEEP', 64, 64, 3);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_DIE', GameWAD+':MTEXTURES\BARREL_DIE', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_BARREL_PAIN', GameWAD+':MTEXTURES\BARREL_PAIN', 64, 64, 1);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_SLEEP', GameWAD+':MTEXTURES\ZOMBY_SLEEP', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_GO', GameWAD+':MTEXTURES\ZOMBY_GO', 64, 64, 4);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_DIE', GameWAD+':MTEXTURES\ZOMBY_DIE', 64, 64, 6);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_MESS', GameWAD+':MTEXTURES\ZOMBY_MESS', 64, 64, 9);
 { TODO 5 : CreateRevertFrames }
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_ATTACK', GameWAD+':MTEXTURES\ZOMBY_ATTACK', 64, 64, 2);
 g_Frames_CreateWAD(nil, 'FRAMES_MONSTER_ZOMBY_PAIN', GameWAD+':MTEXTURES\ZOMBY_PAIN', 64, 64, 1);

 g_Sound_CreateWADEx('SOUND_MONSTER_BARREL_DIE', GameWAD+':MSOUNDS\BARREL_DIE');
 g_Sound_CreateWADEx('SOUND_MONSTER_ZOMBY_PAIN', GameWAD+':MSOUNDS\ZOMBY_PAIN');
 g_Sound_CreateWADEx('SOUND_MONSTER_ZOMBY_DIE', GameWAD+':MSOUNDS\ZOMBY_DIE');
 g_Sound_CreateWADEx('SOUND_MONSTER_ZOMBY_ATTACK', GameWAD+':MSOUNDS\ZOMBY_ATTACK');
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
 { TODO 5 : CreateRevertFrames }
 g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_ATTACK');
 g_Frames_DeleteByName('FRAMES_MONSTER_ZOMBY_PAIN');

 g_Sound_Delete('SOUND_MONSTER_BARREL_DIE');
 g_Sound_Delete('SOUND_MONSTER_ZOMBY_PAIN');
 g_Sound_Delete('SOUND_MONSTER_ZOMBY_DIE');
 g_Sound_Delete('SOUND_MONSTER_ZOMBY_ATTACK');
end;

procedure g_Monsters_Init();
begin
end;

procedure g_Monsters_Free();
begin
 gMonsters := nil;
 { TODO 5 : удалять анимацию }
end;

procedure g_Monsters_Create(fMonsterType: Byte; fX, fY: Integer; fDirection: TDirection);
var
  find_id, FramesID: DWORD;
  a: Integer;
  s: string;

begin
 find_id := FindMonster;

 with gMonsters[find_id] do
 begin
  MonsterType := fMonsterType;
  UID := g_CreateUID(UID_MONSTER);
  Obj.Rect := MONSTERTABLE[MonsterType].Rect;
  Obj.X := fX-Obj.Rect.X;
  Obj.Y := fY-Obj.Rect.Y;
  Obj.Options := OPTION_ENABLEPHYS;
  Direction := fDirection;
  SetState(find_id, STATE_SLEEP);
  Health := MONSTERTABLE[MonsterType].Health;
  Live := True;
  Ammo := 0;
  sleep := 0;

  for a := 0 to High(Anim) do
  begin
   Anim[a, D_LEFT] := nil;
   Anim[a, D_RIGHT] := nil;
  end;

  for a := ANIM_SLEEP to ANIM_PAIN do
   if (ANIMTABLE[a].name <> '') and (MONSTERTABLE[MonsterType].AnimSpeed[a] <> 0) then
   begin
    if not g_Frames_Get(FramesID, 'FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+
                        '_'+ANIMTABLE[a].name) then Continue;

    Anim[a, D_RIGHT] := TAnimation.Create(FramesID, ANIMTABLE[a].loop,
                                          MONSTERTABLE[MonsterType].AnimSpeed[a]);

    if MONSTERTABLE[MonsterType].LeftAnim then
     if g_Frames_Exists('FRAMES_MONSTER_'+MONSTERTABLE[MonsterType].Name+'_'+
                        ANIMTABLE[a].name+'_L') then g_Frames_Get(FramesID, s);

    Anim[a, D_LEFT] := TAnimation.Create(FramesID, ANIMTABLE[a].loop,
                                         MONSTERTABLE[MonsterType].AnimSpeed[a])
   end;
 end;
end;

{procedure DeathSound(MonsterType: Byte);
begin
end;}

procedure WakeUpSound(ID: DWORD);
begin
 case gMonsters[ID].MonsterType of
  MONSTER_DEMON: ;
  MONSTER_IMP: ;
  MONSTER_ZOMBY: ;
  MONSTER_SERG: ;
  MONSTER_CYBER: ;
  MONSTER_CGUN: ;
  MONSTER_BARON: ;
  MONSTER_KNIGHT: ;
  MONSTER_CACO: ;
  MONSTER_SOUL: ;
  MONSTER_PAIN: ;
  MONSTER_SPIDER: ;
  MONSTER_BSP: ;
  MONSTER_MANCUB: ;
  MONSTER_SKEL: ;
  MONSTER_VILE: ;
  MONSTER_FISH: ;
  MONSTER_BARREL: ;
  MONSTER_ROBO: ;
  MONSTER_MAN: ;
 end;
end;

procedure PainSound(ID: DWORD);
begin
 case gMonsters[ID].MonsterType of
  MONSTER_DEMON: ;
  MONSTER_IMP: ;
  MONSTER_ZOMBY: g_Sound_PlayEx('SOUND_MONSTER_ZOMBY_PAIN', 127, 255);
  MONSTER_SERG: ;
  MONSTER_CYBER: ;
  MONSTER_CGUN: ;
  MONSTER_BARON: ;
  MONSTER_KNIGHT: ;
  MONSTER_CACO: ;
  MONSTER_SOUL: ;
  MONSTER_PAIN: ;
  MONSTER_SPIDER: ;
  MONSTER_BSP: ;
  MONSTER_MANCUB: ;
  MONSTER_SKEL: ;
  MONSTER_VILE: ;
  MONSTER_FISH: ;
  MONSTER_BARREL: ;
  MONSTER_ROBO: ;
  MONSTER_MAN: ;
 end;
end;

procedure g_Monsters_Update();
var
  a, b, sx, sy: Integer;
  prevstate: Byte;
  s: ShortInt;
  m: Word;
  o: TObj;

label
  _end;
begin
 if gMonsters = nil then Exit;

 for a := 0 to High(gMonsters) do
  if gMonsters[a].MonsterType <> MONSTER_NONE then
  with gMonsters[a] do
  begin
   m := g_Obj_Move(@Obj);

   if WordBool(m and MOVE_FALLOUT) then
   begin
    MonsterType := MONSTER_NONE;
    Continue;
   end;

   if Anim[CurAnim, Direction] <> nil then
   begin
    prevstate := State;

    if Anim[CurAnim, Direction].Played and not Anim[CurAnim, Direction].Loop then
    begin
     case CurAnim of
      ANIM_SLEEP: ;
      ANIM_GO: ;
      ANIM_DIE,
      ANIM_MESS: SetState(a, STATE_DEAD);
      ANIM_REVIVE: SetState(a, STATE_GO);
      ANIM_ATTACK: SetState(a, STATE_FIRE);
      ANIM_PAIN: SetState(a, STATE_GO);
     end;
    end;

    if prevstate = State then Anim[CurAnim, Direction].Update;
   end;

   if MonsterType = MONSTER_BARREL then
    if ExplodeTimer = 1 then
    begin
     g_Weapon_PushK();
     K_PUSH_PLAYER := 0.3;
     g_Sound_PlayEx('SOUND_MONSTER_BARREL_DIE', 127, 255);
     g_Weapon_Explode(Obj.X+Obj.Rect.X+(Obj.Rect.Height div 2),
                      Obj.Y+Obj.Rect.Y+(Obj.Rect.Width div 2),
                      80, 20, LastSpawnerUID, UID);
     g_Weapon_PopK();

     ExplodeTimer := 0;
    end else if ExplodeTimer > 0 then Dec(ExplodeTimer);

   atm := atm+1;
   if pain > 0 then pain := pain-1;
   if sleep > 0 then sleep := sleep-1;

   if Live and (MonsterType <> MONSTER_BARREL) then
   begin
    if Ammo < 0 then Inc(Ammo);

    case State of
     STATE_SLEEP:
     begin
      if sleep = 0 then
      begin
       sleep := 60;

       if gPlayers <> nil then
        for b := 0 to High(gPlayers) do
         if (gPlayers[b] <> nil) and (gPlayers[b].Live) then
          with gPlayers[b] do
           if g_Look(@Obj, GameX+PLAYER_RECT.X, GameY+PLAYER_RECT.Y,
                     PLAYER_RECT.Width, PLAYER_RECT.Height, Direction) then
           begin
            sleep := 0;
            SetState(a, STATE_GO);
            TargetUID := gPlayers[b].UID;
            WakeUpSound(a);
            Break;
           end;
      end;
     end;

     STATE_GO:
     begin
      if (not GetPos(TargetUID, o)) or (atm > MAX_ATM) then
       if not FindNewPrey(a) then TargetUID := UID_NONE
        else GetPos(TargetUID, o);

      if g_Obj_Collide(@Obj, @o) then
      begin
       atm := 0;
       if kick(a, @o) then goto _end;
      end; 

      if WordBool(m and MOVE_HITWALL) then
       if g_Triggers_PressR(Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.Rect.Width,
                            Obj.Rect.Height, UID, ACTIVATE_MONSTERPRESS) <> nil then
       begin
        SetState(a, STATE_WAIT);
        sleep := 30;
        goto _end;
       end;

      if TargetUID = UID_NONE then
      begin
       //sx sy
      end;

      sx := o.X-Obj.X;
      sy := o.Y-Obj.Y;

      if sx > 0 then SetDir(a, D_RIGHT) else SetDir(a, D_LEFT);

      if CanShoot(MonsterType) then
       if Abs(sx) > Abs(sy) then
        if shoot(a, @o, 0) then goto _end;

      if WordBool(m and MOVE_HITWALL) then
       if Direction = D_LEFT then SetDir(a, D_RIGHT) else SetDir(a, D_LEFT);

      if Direction = D_LEFT then s := -1 else s := 1;
      Obj.Vel.X := s*MONSTERTABLE[MonsterType].RunVel;
     end;

     STATE_DIE: ;
     STATE_DEAD: ;
     STATE_REVIVE: ;
     STATE_ATTACK: ;

     STATE_FIRE:
     begin
      {if Ammo > 0 then fir
       else}
      SetState(a, STATE_GO);
     end;

     STATE_PAIN:
     begin
      if pain >= MONSTERTABLE[MonsterType].Pain then
      begin
       pain := MONSTERTABLE[MonsterType].Pain;
       PainSound(a); 
      end;

      if pain <= MONSTERTABLE[MonsterType].MinPain then
      begin
       SetState(a, STATE_GO);
       pain := 0;
       Ammo := -30;
      end;
     end;

     STATE_WAIT:
     begin
      if sleep = 0 then SetState(a, STATE_GO);
     end;
    end;

    _end:
   end;
  end;
end;

procedure g_Monsters_Draw();
var
  a: Integer;
  b: Boolean;
  m: TMirrorType;
begin
 if gMonsters = nil then Exit;

 for a := 0 to High(gMonsters) do
  if gMonsters[a].MonsterType <> MONSTER_NONE then
  with gMonsters[a] do
   if g_Collide(Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.Rect.Width, Obj.Rect.Height,
                sX, sY, sWidth, sHeight) then
   begin
    b := (State = STATE_DEAD) and (MONSTERTABLE[gMonsters[a].MonsterType].ClearDead);

    if (Anim[CurAnim, Direction] <> nil) and (not b) then
    begin
     if (not MONSTERTABLE[MONSTERTYPE].LeftAnim) and (Direction = D_LEFT) then
      m := M_HORIZONTAL else m := M_NONE;
     Anim[CurAnim, Direction].Draw(Obj.X, Obj.Y, m);
    end;
    
    { else e_DrawFillQuad(Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y,
                         Obj.X+Obj.Rect.X+Obj.Rect.Width, Obj.Y+Obj.Rect.Y+Obj.Rect.Height,
                         127, 127, 127, 0, False);}
    {e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, 'ID: '+IntToStr(a));
    e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y+16, 'TYPE: '+IntToStr(MonsterType));
    e_CharFont_Print(gMenuSmallFont, Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y+32, 'STATE: '+IntToStr(State));}
   end;
end;

function g_Monsters_Collide(ID: DWORD; fX, fY: Integer; fWidth, fHeight: Word): Boolean;
begin
 {$IFDEF DEBUG} Assert(Integer(ID) <= High(gMonsters)); {$ENDIF}

 with gMonsters[ID], Obj do
  Result := g_Collide(X+Rect.X, Y+Rect.Y, Rect.Width, Rect.Height,
                      fX, fY, fHeight, fWidth);
end;

function g_Monsters_Get(UID: Word): DWORD;
var
  a: Integer;
begin
 Result := DWORD(-1);

 if gMonsters <> nil then
  for a := 0 to High(gMonsters) do
   if (gMonsters[a].MonsterType <> MONSTER_NONE) and
      (gMonsters[a].UID = UID) then
   begin
    Result := a;
    Break;
   end;

 Assert(Result <> DWORD(-1));
end;

procedure g_Monsters_Damage(ID: DWORD; Damage: Word; Angle: SmallInt; SpawnerUID: Word);
var
  p: TPlayer;
  anm, st: Byte;
begin
 {$IFDEF DEBUG} Assert(Integer(ID) <= High(gMonsters)); {$ENDIF}

 if not gMonsters[ID].Live then Exit;

 with gMonsters[ID] do
 begin
  Dec(Health, Damage);
  LastSpawnerUID := SpawnerUID;

  if Health <= 0 then
  begin
   Live := False;

   case MonsterType of
    MONSTER_DEMON: ;
    MONSTER_IMP: ;
    MONSTER_ZOMBY: g_Sound_PlayEx('SOUND_MONSTER_ZOMBY_DIE', 127, 255);
    MONSTER_SERG: ;
    MONSTER_CYBER: ;
    MONSTER_CGUN: ;
    MONSTER_BARON: ;
    MONSTER_KNIGHT: ;
    MONSTER_CACO: ;
    MONSTER_SOUL: ;
    MONSTER_PAIN: ;
    MONSTER_SPIDER: ;
    MONSTER_BSP: ;
    MONSTER_MANCUB: ;
    MONSTER_SKEL: ;
    MONSTER_VILE: ;
    MONSTER_FISH: ;
    MONSTER_BARREL: ExplodeTimer := 5+Random(20);
    MONSTER_ROBO: ;
    MONSTER_MAN: ;
   end;

   if (Health < -30) and (Anim[ANIM_MESS, D_RIGHT] <> nil) then
    anm := ANIM_MESS else anm := ANIM_DIE;
   SetState(ID, STATE_DIE, anm);

   if g_GetUIDType(SpawnerUID) = UID_PLAYER then
   begin
    p := g_Player_Get(SpawnerUID);
    if p <> nil then p.MonsterKills := p.MonsterKills+1;
   end;
  end
   else
  begin
   if State = STATE_SLEEP then
   begin
    pain := MONSTERTABLE[MonsterType].Pain;
    SetState(ID, STATE_GO);
   end
    else
   begin
    if pain = 0 then pain := 3;
    pain := pain+Damage;

    if pain > MONSTERTABLE[MonsterType].MinPain then
     SetState(ID, STATE_PAIN);
   end;

   TargetUID := SpawnerUID;

   {if State = STATE_SLEEP then SetState(ID, STATE_GO, ANIM_PAIN)
    else SetState(ID, State, ANIM_PAIN);}
  end;
 end;
end;

procedure g_Monsters_Push(ID: DWORD; Vel: Single; Angle: SmallInt);
var
  k: Single;
begin
 {$IFDEF DEBUG} Assert(Integer(ID) <= High(gMonsters)); {$ENDIF}

 case gMonsters[ID].MonsterType of
  MONSTER_BARREL: k := 0.4;
  else k := 1.0;
 end;

 g_Obj_PushA(@gMonsters[ID].Obj, Vel*k, Angle); 
end;

function g_Monsters_Teleport(ID: DWORD; X, Y: Integer): Boolean;
var
  TA: TAnimation;
  FramesID: DWORD;
begin
 {$IFDEF DEBUG} Assert(Integer(ID) <= High(gMonsters)); {$ENDIF}

 Result := False;

 with gMonsters[ID] do
  if g_CollideLevel(Obj.X+Obj.Rect.X, Obj.Y+Obj.Rect.Y, Obj.Rect.Width, Obj.Rect.Height) then Exit;

 TA := nil;

 if g_Frames_Get(FramesID, 'FRAMES_TELEPORT') then
  TA := TAnimation.Create(FramesID, False, 6);

 g_Sound_PlayEx('SOUND_GAME_TELEPORT', 127, 255);

 with gMonsters[ID] do
 begin
  g_GFX_OnceAnim(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-24,
                 Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-32, TA);
  Obj.X := X-Obj.Rect.X;
  Obj.Y := Y-Obj.Rect.Y;
  g_GFX_OnceAnim(Obj.X+Obj.Rect.X+(Obj.Rect.Width div 2)-24,
                 Obj.Y+Obj.Rect.Y+(Obj.Rect.Height div 2)-32, TA);
 end;
 
 TA.Destroy;

 Result := True;
end;

function g_Monsters_IsLive(ID: DWORD): Boolean;
begin
 {$IFDEF DEBUG} Assert(Integer(ID) <= High(gMonsters)); {$ENDIF}

 Result := (gMonsters[ID].State <> STATE_DEAD) and
           (gMonsters[ID].State <> STATE_DIE);
end;

end.
