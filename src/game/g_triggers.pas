(* Copyright (C)  Doom 2D: Forever Developers
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
unit g_triggers;

interface

uses
  SysUtils, Variants, Classes,
  MAPDEF, e_graphics, g_basic, g_sound,
  xdynrec, hashtable, exoma;

type
  TActivator = record
    UID:     Word;
    TimeOut: Word;
  end;

  PTrigger = ^TTrigger;
  TTrigger = record
  public
    ID:               DWORD;
    ClientID:         DWORD;
    TriggerType:      Byte;
    X, Y:             Integer;
    Width, Height:    Word;
    Enabled:          Boolean;
    ActivateType:     Byte;
    Keys:             Byte;
    TexturePanelGUID: Integer;
    //TexturePanelType: Word;

    TimeOut:          Word;
    ActivateUID:      Word;
    Activators:       array of TActivator;
    PlayerCollide:    Boolean;
    DoorTime:         Integer;
    PressTime:        Integer;
    PressCount:       Integer;
    SoundPlayCount:   Integer;
    Sound:            TPlayableSound;
    AutoSpawn:        Boolean;
    SpawnCooldown:    Integer;
    SpawnedCount:     Integer;
    //ShotPanelType:    Word;
    ShotPanelTime:    Integer;
    ShotSightTime:    Integer;
    ShotSightTimeout: Integer;
    ShotSightTarget:  Word;
    ShotSightTargetN: Word;
    ShotAmmoCount:    Word;
    ShotReloadTime:   Integer;

    mapId: AnsiString; // trigger id, from map
    mapIndex: Integer; // index in fields['trigger'], used in save/load
    trigPanelGUID: Integer;

    trigDataRec: TDynRecord; // triggerdata; owned by trigger (cloned)
    exoInit, exoThink, exoCheck, exoAction: TExprBase;

    userVars: THashStrVariant;

    {$INCLUDE ../shared/mapdef_tgc_def.inc}

  public
    function trigCenter (): TDFPoint; inline;
  end;

function g_Triggers_Create (aTrigger: TTrigger; trec: TDynRecord; forceInternalIndex: Integer=-1): DWORD;
procedure g_Triggers_Update();
procedure g_Triggers_Press(ID: DWORD; ActivateType: Byte; ActivateUID: Word = 0);
function g_Triggers_PressR(X, Y: Integer; Width, Height: Word; UID: Word;
                           ActivateType: Byte; IgnoreList: DWArray = nil): DWArray;
procedure g_Triggers_PressL(X1, Y1, X2, Y2: Integer; UID: DWORD; ActivateType: Byte);
procedure g_Triggers_PressC(CX, CY: Integer; Radius: Word; UID: Word; ActivateType: Byte; IgnoreTrigger: Integer = -1);
procedure g_Triggers_OpenAll();
procedure g_Triggers_DecreaseSpawner(ID: DWORD);
procedure g_Triggers_Free();
procedure g_Triggers_SaveState (st: TStream);
procedure g_Triggers_LoadState (st: TStream);


var
  gTriggerClientID: Integer = 0;
  gTriggers: array of TTrigger;
  gSecretsCount: Integer = 0;
  gMonstersSpawned: array of LongInt = nil;


implementation

uses
  Math,
  g_player, g_map, g_panel, g_gfx, g_game, g_textures,
  g_console, g_monsters, g_items, g_phys, g_weapons,
  wadreader, g_main, e_log, g_language,
  g_options, g_net, g_netmsg, utils, xparser, xstreams;

const
  TRIGGER_SIGNATURE = $58475254; // 'TRGX'
  TRAP_DAMAGE = 1000;

{$INCLUDE ../shared/mapdef_tgc_impl.inc}


// ////////////////////////////////////////////////////////////////////////// //
type
  TTrigScope = class(TExprScope)
  private
    plrprops: TPropHash;
    monsprops: TPropHash;
    platprops: TPropHash;

  public
    me: PTrigger;

  public
    constructor Create ();
    destructor Destroy (); override;

    function getObj (const aname: AnsiString): TObject; override;
    function getField (obj: TObject; const afldname: AnsiString): Variant; override;
    procedure setField (obj: TObject; const afldname: AnsiString; var aval: Variant); override;
  end;


// ////////////////////////////////////////////////////////////////////////// //
type
  TMyConstList = class(TExprConstList)
  public
    function valid (const cname: AnsiString): Boolean; override;
    function get (const cname: AnsiString; out v: Variant): Boolean; override;
  end;


// ////////////////////////////////////////////////////////////////////////// //
function TMyConstList.valid (const cname: AnsiString): Boolean;
begin
  //writeln('CHECK: ''', cname, '''');
  result :=
    (cname = 'player') or
    (cname = 'self') or
    false;
end;

function TMyConstList.get (const cname: AnsiString; out v: Variant): Boolean;
var
  eidx: Integer;
  ebs: TDynEBS;
begin
  //if (cname = 'answer') then begin v := LongInt(42); result := true; exit; end;
  result := false;
  if (gCurrentMap = nil) then exit;
  for eidx := 0 to gCurrentMap.mapdef.ebsTypeCount-1 do
  begin
    ebs := gCurrentMap.mapdef.ebsTypeAt[eidx];
    if ebs.has[cname] then
    begin
      //writeln('FOUND: ''', cname, '''');
      v := ebs[cname];
      result := true;
      exit;
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TTrigScope.Create ();
begin
  plrprops := TPropHash.Create(TPlayer, 'e');
  monsprops := TPropHash.Create(TMonster, 'e');
  platprops := TPropHash.Create(TPanel, 'e');
  me := nil;
end;


destructor TTrigScope.Destroy ();
begin
  platprops.Free();
  monsprops.Free();
  plrprops.Free();
  inherited;
end;


function TTrigScope.getObj (const aname: AnsiString): TObject;
begin
       if (aname = 'player') then result := gPlayers[0] //FIXME
  else if (aname = 'self') or (aname = 'this') then result := TObject(Pointer(PtrUInt(1)))
  else result := inherited getObj(aname);
end;


function TTrigScope.getField (obj: TObject; const afldname: AnsiString): Variant;
begin
  if (obj = gPlayers[0]) then
  begin
    if plrprops.get(obj, afldname, result) then exit;
  end
  else if (obj = TObject(Pointer(PtrUInt(1)))) then
  begin
    if (me <> nil) and (me.userVars <> nil) then
    begin
      if me.userVars.get(afldname, result) then exit;
    end;
  end;
  result := inherited getField(obj, afldname);
end;


procedure TTrigScope.setField (obj: TObject; const afldname: AnsiString; var aval: Variant);
begin
  if (obj = gPlayers[0]) then
  begin
    if plrprops.put(obj, afldname, aval) then exit;
  end
  else if (obj = TObject(Pointer(PtrUInt(1)))) then
  begin
    if (me <> nil) then
    begin
      if (Length(afldname) > 4) and (afldname[1] = 'u') and (afldname[2] = 's') and
         (afldname[3] = 'e') and (afldname[4] = 'r') then
      begin
        if (me.userVars = nil) then me.userVars := THashStrVariant.Create();
        me.userVars.put(afldname, aval);
        exit;
      end;
    end;
  end;
  inherited setField(obj, afldname, aval);
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  tgscope: TTrigScope = nil;
  tgclist: TMyConstList = nil;


// ////////////////////////////////////////////////////////////////////////// //
function TTrigger.trigCenter (): TDFPoint; inline;
begin
  result := TDFPoint.Create(x+width div 2, y+height div 2);
end;


function FindTrigger (): DWORD;
var
  i, olen: Integer;
begin
  olen := Length(gTriggers);

  for i := 0 to olen-1 do
  begin
    if gTriggers[i].TriggerType = TRIGGER_NONE then begin result := i; exit; end;
  end;

  SetLength(gTriggers, olen+8);
  result := olen;

  for i := result to High(gTriggers) do
  begin
    gTriggers[i].TriggerType := TRIGGER_NONE;
    gTriggers[i].trigDataRec := nil;
    gTriggers[i].exoInit := nil;
    gTriggers[i].exoThink := nil;
    gTriggers[i].exoCheck := nil;
    gTriggers[i].exoAction := nil;
    gTriggers[i].userVars := nil;
  end;
end;


function tr_CloseDoor (PanelGUID: Integer; NoSound: Boolean; d2d: Boolean): Boolean;
var
  a, b, c: Integer;
  pan: TPanel;
  PanelID: Integer;
begin
  result := false;
  pan := g_Map_PanelByGUID(PanelGUID);
  if (pan = nil) or not pan.isGWall then exit; //!FIXME!TRIGANY!
  PanelID := pan.arrIdx;

  if not d2d then
  begin
    with gWalls[PanelID] do
    begin
      if g_CollidePlayer(X, Y, Width, Height) or g_Mons_IsAnyAliveAt(X, Y, Width, Height) then Exit;
      if not Enabled then
      begin
        if not NoSound then
        begin
          g_Sound_PlayExAt('SOUND_GAME_DOORCLOSE', X, Y);
          if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_DOORCLOSE');
        end;
        g_Map_EnableWallGUID(PanelGUID);
        result := true;
      end;
    end;
  end
  else
  begin
    if (gDoorMap = nil) then exit;

    c := -1;
    for a := 0 to High(gDoorMap) do
    begin
      for b := 0 to High(gDoorMap[a]) do
      begin
        if gDoorMap[a, b] = DWORD(PanelID) then
        begin
          c := a;
          break;
        end;
      end;
      if (c <> -1) then break;
    end;
    if (c = -1) then exit;

    for b := 0 to High(gDoorMap[c]) do
    begin
      with gWalls[gDoorMap[c, b]] do
      begin
        if g_CollidePlayer(X, Y, Width, Height) or g_Mons_IsAnyAliveAt(X, Y, Width, Height) then exit;
      end;
    end;

    if not NoSound then
    begin
      for b := 0 to High(gDoorMap[c]) do
      begin
        if not gWalls[gDoorMap[c, b]].Enabled then
        begin
          with gWalls[PanelID] do
          begin
            g_Sound_PlayExAt('SOUND_GAME_DOORCLOSE', X, Y);
            if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_DOORCLOSE');
          end;
          break;
        end;
      end;
    end;

    for b := 0 to High(gDoorMap[c]) do
    begin
      if not gWalls[gDoorMap[c, b]].Enabled then
      begin
        g_Map_EnableWall_XXX(gDoorMap[c, b]);
        result := true;
      end;
    end;
  end;
end;


procedure tr_CloseTrap (PanelGUID: Integer; NoSound: Boolean; d2d: Boolean);
var
  a, b, c: Integer;
  wx, wy, wh, ww: Integer;
  pan: TPanel;
  PanelID: Integer;

  function monsDamage (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if g_Obj_Collide(wx, wy, ww, wh, @mon.Obj) then mon.Damage(TRAP_DAMAGE, 0, 0, 0, HIT_TRAP);
  end;

begin
  pan := g_Map_PanelByGUID(PanelGUID);
  {
  if (pan = nil) then
  begin
    e_LogWritefln('tr_CloseTrap: pguid=%s; NO PANEL!', [PanelGUID], MSG_WARNING);
  end
  else
  begin
    e_LogWritefln('tr_CloseTrap: pguid=%s; isGWall=%s; arrIdx=%s', [PanelGUID, pan.isGWall, pan.arrIdx]);
  end;
  }
  if (pan = nil) or not pan.isGWall then exit; //!FIXME!TRIGANY!
  PanelID := pan.arrIdx;

  if not d2d then
  begin
    with gWalls[PanelID] do
    begin
      if (not NoSound) and (not Enabled) then
      begin
        g_Sound_PlayExAt('SOUND_GAME_SWITCH1', X, Y);
        if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_SWITCH1');
      end;
    end;

    wx := gWalls[PanelID].X;
    wy := gWalls[PanelID].Y;
    ww := gWalls[PanelID].Width;
    wh := gWalls[PanelID].Height;

    with gWalls[PanelID] do
    begin
      if gPlayers <> nil then
      begin
        for a := 0 to High(gPlayers) do
        begin
          if (gPlayers[a] <> nil) and gPlayers[a].alive and gPlayers[a].Collide(X, Y, Width, Height) then
          begin
            gPlayers[a].Damage(TRAP_DAMAGE, 0, 0, 0, HIT_TRAP);
          end;
        end;
      end;

      //g_Mons_ForEach(monsDamage);
      g_Mons_ForEachAliveAt(wx, wy, ww, wh, monsDamage);

      if not Enabled then g_Map_EnableWallGUID(PanelGUID);
    end;
  end
  else
  begin
    if (gDoorMap = nil) then exit;

    c := -1;
    for a := 0 to High(gDoorMap) do
    begin
      for b := 0 to High(gDoorMap[a]) do
      begin
        if gDoorMap[a, b] = DWORD(PanelID) then
        begin
          c := a;
          break;
        end;
      end;
      if (c <> -1) then break;
    end;
    if (c = -1) then exit;

    if not NoSound then
    begin
      for b := 0 to High(gDoorMap[c]) do
      begin
        if not gWalls[gDoorMap[c, b]].Enabled then
        begin
          with gWalls[PanelID] do
          begin
            g_Sound_PlayExAt('SOUND_GAME_SWITCH1', X, Y);
            if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_SWITCH1');
          end;
          Break;
        end;
      end;
    end;

    for b := 0 to High(gDoorMap[c]) do
    begin
      wx := gWalls[gDoorMap[c, b]].X;
      wy := gWalls[gDoorMap[c, b]].Y;
      ww := gWalls[gDoorMap[c, b]].Width;
      wh := gWalls[gDoorMap[c, b]].Height;

      with gWalls[gDoorMap[c, b]] do
      begin
        if gPlayers <> nil then
        begin
          for a := 0 to High(gPlayers) do
          begin
            if (gPlayers[a] <> nil) and gPlayers[a].alive and gPlayers[a].Collide(X, Y, Width, Height) then
            begin
              gPlayers[a].Damage(TRAP_DAMAGE, 0, 0, 0, HIT_TRAP);
            end;
          end;
        end;

        //g_Mons_ForEach(monsDamage);
        g_Mons_ForEachAliveAt(wx, wy, ww, wh, monsDamage);
        (*
        if gMonsters <> nil then
          for a := 0 to High(gMonsters) do
            if (gMonsters[a] <> nil) and gMonsters[a].alive and
            g_Obj_Collide(X, Y, Width, Height, @gMonsters[a].Obj) then
              gMonsters[a].Damage(TRAP_DAMAGE, 0, 0, 0, HIT_TRAP);
        *)

        if not Enabled then g_Map_EnableWall_XXX(gDoorMap[c, b]);
      end;
    end;
  end;
end;


function tr_OpenDoor (PanelGUID: Integer; NoSound: Boolean; d2d: Boolean): Boolean;
var
  a, b, c: Integer;
  pan: TPanel;
  PanelID: Integer;
begin
  result := false;
  pan := g_Map_PanelByGUID(PanelGUID);
  if (pan = nil) or not pan.isGWall then exit; //!FIXME!TRIGANY!
  PanelID := pan.arrIdx;

  if not d2d then
  begin
    with gWalls[PanelID] do
    begin
      if Enabled then
      begin
        if not NoSound then
        begin
          g_Sound_PlayExAt('SOUND_GAME_DOOROPEN', X, Y);
          if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_DOOROPEN');
        end;
        g_Map_DisableWallGUID(PanelGUID);
        result := true;
      end;
    end
  end
  else
  begin
    if (gDoorMap = nil) then exit;

    c := -1;
    for a := 0 to High(gDoorMap) do
    begin
      for b := 0 to High(gDoorMap[a]) do
      begin
        if gDoorMap[a, b] = DWORD(PanelID) then
        begin
          c := a;
          break;
        end;
      end;
      if (c <> -1) then break;
    end;
    if (c = -1) then exit;

    if not NoSound then
    begin
      for b := 0 to High(gDoorMap[c]) do
      begin
        if gWalls[gDoorMap[c, b]].Enabled then
        begin
          with gWalls[PanelID] do
          begin
            g_Sound_PlayExAt('SOUND_GAME_DOOROPEN', X, Y);
            if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_DOOROPEN');
          end;
          break;
        end;
      end;
    end;

    for b := 0 to High(gDoorMap[c]) do
    begin
      if gWalls[gDoorMap[c, b]].Enabled then
      begin
        g_Map_DisableWall_XXX(gDoorMap[c, b]);
        result := true;
      end;
    end;
  end;
end;


function tr_SetLift (PanelGUID: Integer; d: Integer; NoSound: Boolean; d2d: Boolean): Boolean;
var
  a, b, c: Integer;
  t: Integer = 0;
  pan: TPanel;
  PanelID: Integer;
begin
  result := false;
  pan := g_Map_PanelByGUID(PanelGUID);
  if (pan = nil) or not pan.isGLift then exit; //!FIXME!TRIGANY!
  PanelID := pan.arrIdx;

  if (gLifts[PanelID].PanelType = PANEL_LIFTUP) or (gLifts[PanelID].PanelType = PANEL_LIFTDOWN) then
  begin
    case d of
      0: t := LIFTTYPE_UP;
      1: t := LIFTTYPE_DOWN;
      else t := IfThen(gLifts[PanelID].LiftType = LIFTTYPE_DOWN, LIFTTYPE_UP, LIFTTYPE_DOWN);
    end
  end
  else if (gLifts[PanelID].PanelType = PANEL_LIFTLEFT) or (gLifts[PanelID].PanelType = PANEL_LIFTRIGHT) then
  begin
    case d of
      0: t := LIFTTYPE_LEFT;
      1: t := LIFTTYPE_RIGHT;
      else t := IfThen(gLifts[PanelID].LiftType = LIFTTYPE_LEFT, LIFTTYPE_RIGHT, LIFTTYPE_LEFT);
    end;
  end;

  if not d2d then
  begin
    with gLifts[PanelID] do
    begin
      if (LiftType <> t) then
      begin
        g_Map_SetLiftGUID(PanelGUID, t); //???
        //if not NoSound then g_Sound_PlayExAt('SOUND_GAME_SWITCH0', X, Y);
        result := true;
      end;
    end;
  end
  else // Как в D2d
  begin
    if (gLiftMap = nil) then exit;

    c := -1;
    for a := 0 to High(gLiftMap) do
    begin
      for b := 0 to High(gLiftMap[a]) do
      begin
        if (gLiftMap[a, b] = DWORD(PanelID)) then
        begin
          c := a;
          break;
        end;
      end;
      if (c <> -1) then break;
    end;
    if (c = -1) then exit;

    {if not NoSound then
      for b := 0 to High(gLiftMap[c]) do
        if gLifts[gLiftMap[c, b]].LiftType <> t then
        begin
          with gLifts[PanelID] do
            g_Sound_PlayExAt('SOUND_GAME_SWITCH0', X, Y);
          Break;
        end;}

    for b := 0 to High(gLiftMap[c]) do
    begin
      with gLifts[gLiftMap[c, b]] do
      begin
        if (LiftType <> t) then
        begin
          g_Map_SetLift_XXX(gLiftMap[c, b], t);
          result := true;
        end;
      end;
    end;
  end;
end;


function tr_SpawnShot (ShotType: Integer; wx, wy, dx, dy: Integer; ShotSound: Boolean; ShotTarget: Word): Integer;
var
  snd: string;
  Projectile: Boolean;
  TextureID: DWORD;
  Anim: TAnimation;
begin
  result := -1;
  TextureID := DWORD(-1);
  snd := 'SOUND_WEAPON_FIREROCKET';
  Projectile := true;

  case ShotType of
    TRIGGER_SHOT_PISTOL:
      begin
        g_Weapon_pistol(wx, wy, dx, dy, 0, True);
        snd := 'SOUND_WEAPON_FIREPISTOL';
        Projectile := False;
        if ShotSound then
        begin
          g_Player_CreateShell(wx, wy, 0, -2, SHELL_BULLET);
          if g_Game_IsNet then MH_SEND_Effect(wx, wy, 0, NET_GFX_SHELL1);
        end;
      end;

    TRIGGER_SHOT_BULLET:
      begin
        g_Weapon_mgun(wx, wy, dx, dy, 0, True);
        if gSoundEffectsDF then snd := 'SOUND_WEAPON_FIRECGUN'
        else snd := 'SOUND_WEAPON_FIREPISTOL';
        Projectile := False;
        if ShotSound then
        begin
          g_Player_CreateShell(wx, wy, 0, -2, SHELL_BULLET);
          if g_Game_IsNet then MH_SEND_Effect(wx, wy, 0, NET_GFX_SHELL1);
        end;
      end;

    TRIGGER_SHOT_SHOTGUN:
      begin
        g_Weapon_Shotgun(wx, wy, dx, dy, 0, True);
        snd := 'SOUND_WEAPON_FIRESHOTGUN';
        Projectile := False;
        if ShotSound then
        begin
          g_Player_CreateShell(wx, wy, 0, -2, SHELL_SHELL);
          if g_Game_IsNet then MH_SEND_Effect(wx, wy, 0, NET_GFX_SHELL2);
        end;
      end;

    TRIGGER_SHOT_SSG:
      begin
        g_Weapon_DShotgun(wx, wy, dx, dy, 0, True);
        snd := 'SOUND_WEAPON_FIRESHOTGUN2';
        Projectile := False;
        if ShotSound then
        begin
          g_Player_CreateShell(wx, wy, 0, -2, SHELL_SHELL);
          g_Player_CreateShell(wx, wy, 0, -2, SHELL_SHELL);
          if g_Game_IsNet then MH_SEND_Effect(wx, wy, 0, NET_GFX_SHELL3);
        end;
      end;

    TRIGGER_SHOT_IMP:
      begin
        g_Weapon_ball1(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREBALL';
      end;

    TRIGGER_SHOT_PLASMA:
      begin
        g_Weapon_Plasma(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREPLASMA';
      end;

    TRIGGER_SHOT_SPIDER:
      begin
        g_Weapon_aplasma(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREPLASMA';
      end;

    TRIGGER_SHOT_CACO:
      begin
        g_Weapon_ball2(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREBALL';
      end;

    TRIGGER_SHOT_BARON:
      begin
        g_Weapon_ball7(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREBALL';
      end;

    TRIGGER_SHOT_MANCUB:
      begin
        g_Weapon_manfire(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREBALL';
      end;

    TRIGGER_SHOT_REV:
      begin
        g_Weapon_revf(wx, wy, dx, dy, 0, ShotTarget, -1, True);
        snd := 'SOUND_WEAPON_FIREREV';
      end;

    TRIGGER_SHOT_ROCKET:
      begin
        g_Weapon_Rocket(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREROCKET';
      end;

    TRIGGER_SHOT_BFG:
      begin
        g_Weapon_BFGShot(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_WEAPON_FIREBFG';
      end;

    TRIGGER_SHOT_EXPL:
      begin
        if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_ROCKET') then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(wx-64, wy-64, Anim);
          Anim.Free();
        end;
        Projectile := False;
        g_Weapon_Explode(wx, wy, 60, 0);
        snd := 'SOUND_WEAPON_EXPLODEROCKET';
      end;

    TRIGGER_SHOT_BFGEXPL:
      begin
        if g_Frames_Get(TextureID, 'FRAMES_EXPLODE_BFG') then
        begin
          Anim := TAnimation.Create(TextureID, False, 6);
          Anim.Blending := False;
          g_GFX_OnceAnim(wx-64, wy-64, Anim);
          Anim.Free();
        end;
        Projectile := False;
        g_Weapon_BFG9000(wx, wy, 0);
        snd := 'SOUND_WEAPON_EXPLODEBFG';
      end;

    TRIGGER_SHOT_FLAME:
      begin
        g_Weapon_flame(wx, wy, dx, dy, 0, -1, True);
        snd := 'SOUND_GAME_BURNING';
      end;

    else exit;
  end;

  if g_Game_IsNet and g_Game_IsServer then
  begin
    case ShotType of
      TRIGGER_SHOT_EXPL: MH_SEND_Effect(wx, wy, Byte(ShotSound), NET_GFX_EXPLODE);
      TRIGGER_SHOT_BFGEXPL: MH_SEND_Effect(wx, wy, Byte(ShotSound), NET_GFX_BFGEXPL);
      else
      begin
        if Projectile then MH_SEND_CreateShot(LastShotID);
        if ShotSound then MH_SEND_Sound(wx, wy, snd);
      end;
    end;
  end;

  if ShotSound then g_Sound_PlayExAt(snd, wx, wy);

  if Projectile then Result := LastShotID;
end;


procedure MakeShot (var Trigger: TTrigger; wx, wy, dx, dy: Integer; TargetUID: Word);
begin
  with Trigger do
  begin
    if (tgcAmmo = 0) or ((tgcAmmo > 0) and (ShotAmmoCount > 0)) then
    begin
      if (trigPanelGUID <> -1) and (ShotPanelTime = 0) then
      begin
        g_Map_SwitchTextureGUID({ShotPanelType,} trigPanelGUID);
        ShotPanelTime := 4; // тиков на вспышку выстрела
      end;

      if (tgcSight > 0) then ShotSightTimeout := 180; // ~= 5 секунд

      if (ShotAmmoCount > 0) then Dec(ShotAmmoCount);

      dx += Random(tgcAccuracy)-Random(tgcAccuracy);
      dy += Random(tgcAccuracy)-Random(tgcAccuracy);

      tr_SpawnShot(tgcShotType, wx, wy, dx, dy, tgcShotSound, TargetUID);
    end
    else
    begin
      if (tgcReload > 0) and (ShotReloadTime = 0) then
      begin
        ShotReloadTime := tgcReload; // тиков на перезарядку пушки
      end;
    end;
  end;
end;


procedure tr_MakeEffect (X, Y, VX, VY: Integer; T, ST, CR, CG, CB: Byte; Silent, Send: Boolean);
var
  FramesID: DWORD;
  Anim: TAnimation;
begin
  if T = TRIGGER_EFFECT_PARTICLE then
  begin
    case ST of
      TRIGGER_EFFECT_SLIQUID:
      begin
             if (CR = 255) and (CG = 0) and (CB = 0) then g_GFX_SimpleWater(X, Y, 1, VX, VY, 1, 0, 0, 0)
        else if (CR = 0) and (CG = 255) and (CB = 0) then g_GFX_SimpleWater(X, Y, 1, VX, VY, 2, 0, 0, 0)
        else if (CR = 0) and (CG = 0) and (CB = 255) then g_GFX_SimpleWater(X, Y, 1, VX, VY, 3, 0, 0, 0)
        else g_GFX_SimpleWater(X, Y, 1, VX, VY, 0, 0, 0, 0);
      end;
      TRIGGER_EFFECT_LLIQUID: g_GFX_SimpleWater(X, Y, 1, VX, VY, 4, CR, CG, CB);
      TRIGGER_EFFECT_DLIQUID: g_GFX_SimpleWater(X, Y, 1, VX, VY, 5, CR, CG, CB);
      TRIGGER_EFFECT_BLOOD: g_GFX_Blood(X, Y, 1, VX, VY, 0, 0, CR, CG, CB);
      TRIGGER_EFFECT_SPARK: g_GFX_Spark(X, Y, 1, GetAngle2(VX, VY), 0, 0);
      TRIGGER_EFFECT_BUBBLE: g_GFX_Bubbles(X, Y, 1, 0, 0);
    end;
  end;

  if T = TRIGGER_EFFECT_ANIMATION then
  begin
    case ST of
      EFFECT_TELEPORT: begin
        if g_Frames_Get(FramesID, 'FRAMES_TELEPORT') then
        begin
          Anim := TAnimation.Create(FramesID, False, 3);
          if not Silent then g_Sound_PlayExAt('SOUND_GAME_TELEPORT', X, Y);
          g_GFX_OnceAnim(X-32, Y-32, Anim);
          Anim.Free();
        end;
        if Send and g_Game_IsServer and g_Game_IsNet then MH_SEND_Effect(X, Y, Byte(not Silent), NET_GFX_TELE);
      end;
      EFFECT_RESPAWN: begin
        if g_Frames_Get(FramesID, 'FRAMES_ITEM_RESPAWN') then
        begin
          Anim := TAnimation.Create(FramesID, False, 4);
          if not Silent then g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', X, Y);
          g_GFX_OnceAnim(X-16, Y-16, Anim);
          Anim.Free();
        end;
        if Send and g_Game_IsServer and g_Game_IsNet then MH_SEND_Effect(X-16, Y-16, Byte(not Silent), NET_GFX_RESPAWN);
      end;
      EFFECT_FIRE: begin
        if g_Frames_Get(FramesID, 'FRAMES_FIRE') then
        begin
          Anim := TAnimation.Create(FramesID, False, 4);
          if not Silent then g_Sound_PlayExAt('SOUND_FIRE', X, Y);
          g_GFX_OnceAnim(X-32, Y-128, Anim);
          Anim.Free();
        end;
        if Send and g_Game_IsServer and g_Game_IsNet then MH_SEND_Effect(X-32, Y-128, Byte(not Silent), NET_GFX_FIRE);
      end;
    end;
  end;
end;


function tr_Teleport (ActivateUID: Integer; TX, TY: Integer; TDir: Integer; Silent: Boolean; D2D: Boolean): Boolean;
var
  p: TPlayer;
  m: TMonster;
begin
  Result := False;
  if (ActivateUID < 0) or (ActivateUID > $FFFF) then Exit;
  case g_GetUIDType(ActivateUID) of
    UID_PLAYER:
      begin
        p := g_Player_Get(ActivateUID);
        if p = nil then Exit;
        if D2D then
        begin
          if p.TeleportTo(TX-(p.Obj.Rect.Width div 2), TY-p.Obj.Rect.Height, Silent, TDir) then result := true;
        end
        else
        begin
          if p.TeleportTo(TX, TY, Silent, TDir) then result := true;
        end;
      end;
    UID_MONSTER:
      begin
        m := g_Monsters_ByUID(ActivateUID);
        if m = nil then Exit;
        if D2D then
        begin
          if m.TeleportTo(TX-(m.Obj.Rect.Width div 2), TY-m.Obj.Rect.Height, Silent, TDir) then result := true;
        end
        else
        begin
          if m.TeleportTo(TX, TY, Silent, TDir) then result := true;
        end;
      end;
  end;
end;


function tr_Push (ActivateUID: Integer; VX, VY: Integer; ResetVel: Boolean): Boolean;
var
  p: TPlayer;
  m: TMonster;
begin
  result := true;
  if (ActivateUID < 0) or (ActivateUID > $FFFF) then exit;
  case g_GetUIDType(ActivateUID) of
    UID_PLAYER:
      begin
        p := g_Player_Get(ActivateUID);
        if p = nil then Exit;

        if ResetVel then
        begin
          p.GameVelX := 0;
          p.GameVelY := 0;
          p.GameAccelX := 0;
          p.GameAccelY := 0;
        end;

        p.Push(VX, VY);
      end;

    UID_MONSTER:
      begin
        m := g_Monsters_ByUID(ActivateUID);
        if m = nil then Exit;

        if ResetVel then
        begin
          m.GameVelX := 0;
          m.GameVelY := 0;
          m.GameAccelX := 0;
          m.GameAccelY := 0;
        end;

        m.Push(VX, VY);
      end;
  end;
end;


function tr_Message (MKind: Integer; MText: string; MSendTo: Integer; MTime: Integer; ActivateUID: Integer): Boolean;
var
  msg: string;
  p: TPlayer;
  i: Integer;
begin
  Result := True;
  if (ActivateUID < 0) or (ActivateUID > $FFFF) then Exit;
  msg := b_Text_Format(MText);
  case MSendTo of
    TRIGGER_MESSAGE_DEST_ME: // activator
      begin
        if g_GetUIDType(ActivateUID) = UID_PLAYER then
        begin
          if g_Game_IsWatchedPlayer(ActivateUID) then
          begin
                 if MKind = TRIGGER_MESSAGE_KIND_CHAT then g_Console_Add(msg, True)
            else if MKind = TRIGGER_MESSAGE_KIND_GAME then g_Game_Message(msg, MTime);
          end
          else
          begin
            p := g_Player_Get(ActivateUID);
            if g_Game_IsNet and (p.FClientID >= 0) then
            begin
                   if MKind = TRIGGER_MESSAGE_KIND_CHAT then MH_SEND_Chat(msg, NET_CHAT_SYSTEM, p.FClientID)
              else if MKind = TRIGGER_MESSAGE_KIND_GAME then MH_SEND_GameEvent(NET_EV_BIGTEXT, MTime, msg, p.FClientID);
            end;
          end;
        end;
      end;

    TRIGGER_MESSAGE_DEST_MY_TEAM: // activator's team
      begin
        if g_GetUIDType(ActivateUID) = UID_PLAYER then
        begin
          p := g_Player_Get(ActivateUID);
          if g_Game_IsWatchedTeam(p.Team) then
          begin
                 if MKind = TRIGGER_MESSAGE_KIND_CHAT then g_Console_Add(msg, True)
            else if MKind = TRIGGER_MESSAGE_KIND_GAME then g_Game_Message(msg, MTime);
          end;

          if g_Game_IsNet then
          begin
            for i := Low(gPlayers) to High(gPlayers) do
            begin
              if (gPlayers[i].Team = p.Team) and (gPlayers[i].FClientID >= 0) then
              begin
                     if MKind = TRIGGER_MESSAGE_KIND_CHAT then MH_SEND_Chat(msg, NET_CHAT_SYSTEM, gPlayers[i].FClientID)
                else if MKind = TRIGGER_MESSAGE_KIND_GAME then MH_SEND_GameEvent(NET_EV_BIGTEXT, MTime, msg, gPlayers[i].FClientID);
              end;
            end;
          end;
        end;
      end;

    TRIGGER_MESSAGE_DEST_ENEMY_TEAM: // activator's enemy team
      begin
        if g_GetUIDType(ActivateUID) = UID_PLAYER then
        begin
          p := g_Player_Get(ActivateUID);
          if g_Game_IsWatchedTeam(p.Team) then
          begin
                 if MKind = TRIGGER_MESSAGE_KIND_CHAT then g_Console_Add(msg, True)
            else if MKind = TRIGGER_MESSAGE_KIND_GAME then g_Game_Message(msg, MTime);
          end;

          if g_Game_IsNet then
          begin
            for i := Low(gPlayers) to High(gPlayers) do
            begin
              if (gPlayers[i].Team <> p.Team) and (gPlayers[i].FClientID >= 0) then
              begin
                     if MKind = TRIGGER_MESSAGE_KIND_CHAT then MH_SEND_Chat(msg, NET_CHAT_SYSTEM, gPlayers[i].FClientID)
                else if MKind = TRIGGER_MESSAGE_KIND_GAME then MH_SEND_GameEvent(NET_EV_BIGTEXT, MTime, msg, gPlayers[i].FClientID);
              end;
            end;
          end;
        end;
      end;

    TRIGGER_MESSAGE_DEST_RED_TEAM: // red team
      begin
        if g_Game_IsWatchedTeam(TEAM_RED) then
        begin
               if MKind = TRIGGER_MESSAGE_KIND_CHAT then g_Console_Add(msg, True)
          else if MKind = TRIGGER_MESSAGE_KIND_GAME then g_Game_Message(msg, MTime);
        end;

        if g_Game_IsNet then
        begin
          for i := Low(gPlayers) to High(gPlayers) do
          begin
            if (gPlayers[i].Team = TEAM_RED) and (gPlayers[i].FClientID >= 0) then
            begin
                   if MKind = TRIGGER_MESSAGE_KIND_CHAT then MH_SEND_Chat(msg, NET_CHAT_SYSTEM, gPlayers[i].FClientID)
              else if MKind = TRIGGER_MESSAGE_KIND_GAME then MH_SEND_GameEvent(NET_EV_BIGTEXT, MTime, msg, gPlayers[i].FClientID);
            end;
          end;
        end;
      end;

    TRIGGER_MESSAGE_DEST_BLUE_TEAM: // blue team
      begin
        if g_Game_IsWatchedTeam(TEAM_BLUE) then
        begin
               if MKind = TRIGGER_MESSAGE_KIND_CHAT then g_Console_Add(msg, True)
          else if MKind = TRIGGER_MESSAGE_KIND_GAME then g_Game_Message(msg, MTime);
        end;

        if g_Game_IsNet then
        begin
          for i := Low(gPlayers) to High(gPlayers) do
          begin
            if (gPlayers[i].Team = TEAM_BLUE) and (gPlayers[i].FClientID >= 0) then
            begin
                   if MKind = TRIGGER_MESSAGE_KIND_CHAT then MH_SEND_Chat(msg, NET_CHAT_SYSTEM, gPlayers[i].FClientID)
              else if MKind = TRIGGER_MESSAGE_KIND_GAME then MH_SEND_GameEvent(NET_EV_BIGTEXT, MTime, msg, gPlayers[i].FClientID);
            end;
          end;
        end;
      end;

    TRIGGER_MESSAGE_DEST_EVERYONE: // everyone
      begin
             if MKind = TRIGGER_MESSAGE_KIND_CHAT then g_Console_Add(msg, True)
        else if MKind = TRIGGER_MESSAGE_KIND_GAME then g_Game_Message(msg, MTime);

        if g_Game_IsNet then
        begin
               if MKind = TRIGGER_MESSAGE_KIND_CHAT then MH_SEND_Chat(msg, NET_CHAT_SYSTEM)
          else if MKind = TRIGGER_MESSAGE_KIND_GAME then MH_SEND_GameEvent(NET_EV_BIGTEXT, MTime, msg);
        end;
      end;
  end;
end;


function tr_ShotAimCheck (var Trigger: TTrigger; Obj: PObj): Boolean;
begin
  result := false;
  with Trigger do
  begin
    if TriggerType <> TRIGGER_SHOT then Exit;
    result := (tgcAim and TRIGGER_SHOT_AIM_ALLMAP > 0)
              or g_Obj_Collide(X, Y, Width, Height, Obj);
    if result and (tgcAim and TRIGGER_SHOT_AIM_TRACE > 0) then
    begin
      result := g_TraceVector(tgcTX, tgcTY,
                              Obj^.X + Obj^.Rect.X + (Obj^.Rect.Width div 2),
                              Obj^.Y + Obj^.Rect.Y + (Obj^.Rect.Height div 2));
    end;
  end;
end;


function ActivateTrigger (var Trigger: TTrigger; actType: Byte): Boolean;
var
  animonce: Boolean;
  p: TPlayer;
  m: TMonster;
  pan: TPanel;
  idx, k, wx, wy, xd, yd: Integer;
  iid: LongWord;
  coolDown: Boolean;
  pAngle: Real;
  FramesID: DWORD;
  Anim: TAnimation;
  UIDType: Byte;
  TargetUID: Word;
  it: PItem;
  mon: TMonster;

  function monsShotTarget (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if mon.alive and tr_ShotAimCheck(Trigger, @(mon.Obj)) then
    begin
      xd := mon.GameX + mon.Obj.Rect.Width div 2;
      yd := mon.GameY + mon.Obj.Rect.Height div 2;
      TargetUID := mon.UID;
      result := true; // stop
    end;
  end;

  function monsShotTargetMonPlr (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if mon.alive and tr_ShotAimCheck(Trigger, @(mon.Obj)) then
    begin
      xd := mon.GameX + mon.Obj.Rect.Width div 2;
      yd := mon.GameY + mon.Obj.Rect.Height div 2;
      TargetUID := mon.UID;
      result := true; // stop
    end;
  end;

  function monShotTargetPlrMon (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    if mon.alive and tr_ShotAimCheck(Trigger, @(mon.Obj)) then
    begin
      xd := mon.GameX + mon.Obj.Rect.Width div 2;
      yd := mon.GameY + mon.Obj.Rect.Height div 2;
      TargetUID := mon.UID;
      result := true; // stop
    end;
  end;

var
  tvval: Variant;
begin
  result := false;
  if g_Game_IsClient then exit;

  if not Trigger.Enabled then exit;
  if (Trigger.TimeOut <> 0) and (actType <> ACTIVATE_CUSTOM) then exit;
  if (gLMSRespawn = LMS_RESPAWN_WARMUP) then exit;

  if (Trigger.exoCheck <> nil) then
  begin
    //conwritefln('exocheck: [%s]', [Trigger.exoCheck.toString()]);
    try
      tgscope.me := @Trigger;
      tvval := Trigger.exoCheck.value(tgscope);
      tgscope.me := nil;
      if not Boolean(tvval) then exit;
    except on e: Exception do
      begin
        tgscope.me := nil;
        conwritefln('trigger exocheck error: %s [%s]', [e.message, Trigger.exoCheck.toString()]);
        exit;
      end;
    end;
  end;

  animonce := False;

  coolDown := (actType <> 0);

  if (Trigger.exoAction <> nil) then
  begin
    //conwritefln('exoactivate: [%s]', [Trigger.exoAction.toString()]);
    try
      tgscope.me := @Trigger;
      Trigger.exoAction.value(tgscope);
      tgscope.me := nil;
    except on e: Exception do
      begin
        tgscope.me := nil;
        conwritefln('trigger exoactivate error: %s [%s]', [e.message, Trigger.exoAction.toString()]);
        exit;
      end;
    end;
  end;

  with Trigger do
  begin
    case TriggerType of
      TRIGGER_EXIT:
        begin
          g_Sound_PlayEx('SOUND_GAME_SWITCH0');
          if g_Game_IsNet then MH_SEND_Sound(X, Y, 'SOUND_GAME_SWITCH0');
          gExitByTrigger := True;
          g_Game_ExitLevel(tgcMap);
          TimeOut := 18;
          Result := True;

          Exit;
        end;

      TRIGGER_TELEPORT:
        begin
          Result := tr_Teleport(ActivateUID,
                                tgcTarget.X, tgcTarget.Y,
                                tgcDirection, tgcSilent,
                                tgcD2d);
          TimeOut := 0;
        end;

      TRIGGER_OPENDOOR:
        begin
          Result := tr_OpenDoor(trigPanelGUID, tgcSilent, tgcD2d);
          TimeOut := 0;
        end;

      TRIGGER_CLOSEDOOR:
        begin
          Result := tr_CloseDoor(trigPanelGUID, tgcSilent, tgcD2d);
          TimeOut := 0;
        end;

      TRIGGER_DOOR, TRIGGER_DOOR5:
        begin
          pan := g_Map_PanelByGUID(trigPanelGUID);
          if (pan <> nil) and pan.isGWall then
          begin
            if gWalls[{trigPanelID}pan.arrIdx].Enabled then
            begin
              result := tr_OpenDoor(trigPanelGUID, tgcSilent, tgcD2d);
              if (TriggerType = TRIGGER_DOOR5) then DoorTime := 180;
            end
            else
            begin
              result := tr_CloseDoor(trigPanelGUID, tgcSilent, tgcD2d);
            end;

            if result then TimeOut := 18;
          end;
        end;

      TRIGGER_CLOSETRAP, TRIGGER_TRAP:
        begin
          tr_CloseTrap(trigPanelGUID, tgcSilent, tgcD2d);

          if TriggerType = TRIGGER_TRAP then
            begin
              DoorTime := 40;
              TimeOut := 76;
            end
          else
            begin
              DoorTime := -1;
              TimeOut := 0;
            end;

          Result := True;
        end;

      TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
        begin
          PressCount += 1;
          if PressTime = -1 then PressTime := tgcWait;
          if coolDown then TimeOut := 18 else TimeOut := 0;
          Result := True;
        end;

      TRIGGER_SECRET:
        if g_GetUIDType(ActivateUID) = UID_PLAYER then
        begin
          Enabled := False;
          Result := True;
          if gLMSRespawn = LMS_RESPAWN_NONE then
          begin
            g_Player_Get(ActivateUID).GetSecret();
            Inc(gCoopSecretsFound);
            if g_Game_IsNet then MH_SEND_GameStats();
          end;
        end;

      TRIGGER_LIFTUP:
        begin
          Result := tr_SetLift(trigPanelGUID, 0, tgcSilent, tgcD2d);
          TimeOut := 0;

          if (not tgcSilent) and Result then begin
            g_Sound_PlayExAt('SOUND_GAME_SWITCH0',
                             X + (Width div 2),
                             Y + (Height div 2));
            if g_Game_IsServer and g_Game_IsNet then
              MH_SEND_Sound(X + (Width div 2),
                            Y + (Height div 2),
                            'SOUND_GAME_SWITCH0');
          end;
        end;

      TRIGGER_LIFTDOWN:
        begin
          Result := tr_SetLift(trigPanelGUID, 1, tgcSilent, tgcD2d);
          TimeOut := 0;

          if (not tgcSilent) and Result then begin
            g_Sound_PlayExAt('SOUND_GAME_SWITCH0',
                             X + (Width div 2),
                             Y + (Height div 2));
            if g_Game_IsServer and g_Game_IsNet then
              MH_SEND_Sound(X + (Width div 2),
                            Y + (Height div 2),
                            'SOUND_GAME_SWITCH0');
          end;
        end;

      TRIGGER_LIFT:
        begin
          Result := tr_SetLift(trigPanelGUID, 3, tgcSilent, tgcD2d);

          if Result then
          begin
            TimeOut := 18;

            if (not tgcSilent) and Result then begin
              g_Sound_PlayExAt('SOUND_GAME_SWITCH0',
                               X + (Width div 2),
                               Y + (Height div 2));
              if g_Game_IsServer and g_Game_IsNet then
                MH_SEND_Sound(X + (Width div 2),
                              Y + (Height div 2),
                              'SOUND_GAME_SWITCH0');
            end;
          end;
        end;

      TRIGGER_TEXTURE:
        begin
          if tgcActivateOnce then
            begin
              Enabled := False;
              TriggerType := TRIGGER_NONE;
            end
          else
            if coolDown then
              TimeOut := 6
            else
              TimeOut := 0;

          animonce := tgcAnimateOnce;
          Result := True;
        end;

      TRIGGER_SOUND:
        begin
          if Sound <> nil then
          begin
            if tgcSoundSwitch and Sound.IsPlaying() then
              begin // Нужно выключить, если играл
                Sound.Stop();
                SoundPlayCount := 0;
                Result := True;
              end
            else // (not Data.SoundSwitch) or (not Sound.IsPlaying())
              if (tgcPlayCount > 0) or (not Sound.IsPlaying()) then
                begin
                  if tgcPlayCount > 0 then
                    SoundPlayCount := tgcPlayCount
                  else // 0 - играем бесконечно
                    SoundPlayCount := 1;
                  Result := True;
                end;
            if g_Game_IsNet then MH_SEND_TriggerSound(Trigger);
          end;
        end;

      TRIGGER_SPAWNMONSTER:
        if (tgcSpawnMonsType in [MONSTER_DEMON..MONSTER_MAN]) then
        begin
          Result := False;
          if (tgcDelay > 0) and (actType <> ACTIVATE_CUSTOM) then
          begin
            AutoSpawn := not AutoSpawn;
            SpawnCooldown := 0;
            // Автоспавнер переключен - меняем текстуру
            Result := True;
          end;

          if ((tgcDelay = 0) and (actType <> ACTIVATE_CUSTOM))
          or ((tgcDelay > 0) and (actType = ACTIVATE_CUSTOM)) then
            for k := 1 to tgcMonsCount do
            begin
              if (actType = ACTIVATE_CUSTOM) and (tgcDelay > 0) then
                SpawnCooldown := tgcDelay;
              if (tgcMax > 0) and (SpawnedCount >= tgcMax) then
                Break;

              mon := g_Monsters_Create(tgcSpawnMonsType,
                     tgcTX, tgcTY,
                     TDirection(tgcDirection), True);

              Result := True;

            // Здоровье:
              if (tgcHealth > 0) then
                mon.SetHealth(tgcHealth);
            // Устанавливаем поведение:
              mon.MonsterBehaviour := tgcBehaviour;
              mon.FNoRespawn := True;
              if g_Game_IsNet then
                MH_SEND_MonsterSpawn(mon.UID);
            // Идем искать цель, если надо:
              if tgcActive then
                mon.WakeUp();

              if tgcSpawnMonsType <> MONSTER_BARREL then Inc(gTotalMonsters);

              if g_Game_IsNet then
              begin
                SetLength(gMonstersSpawned, Length(gMonstersSpawned)+1);
                gMonstersSpawned[High(gMonstersSpawned)] := mon.UID;
              end;

              if tgcMax > 0 then
              begin
                mon.SpawnTrigger := ID;
                Inc(SpawnedCount);
              end;

              case tgcEffect of
                EFFECT_TELEPORT: begin
                  if g_Frames_Get(FramesID, 'FRAMES_TELEPORT') then
                  begin
                    Anim := TAnimation.Create(FramesID, False, 3);
                    g_Sound_PlayExAt('SOUND_GAME_TELEPORT', tgcTX, tgcTY);
                    g_GFX_OnceAnim(mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-32,
                                   mon.Obj.Y+mon.Obj.Rect.Y+(mon.Obj.Rect.Height div 2)-32, Anim);
                    Anim.Free();
                  end;
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_Effect(mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-32,
                                   mon.Obj.Y+mon.Obj.Rect.Y+(mon.Obj.Rect.Height div 2)-32, 1,
                                   NET_GFX_TELE);
                end;
                EFFECT_RESPAWN: begin
                  if g_Frames_Get(FramesID, 'FRAMES_ITEM_RESPAWN') then
                  begin
                    Anim := TAnimation.Create(FramesID, False, 4);
                    g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', tgcTX, tgcTY);
                    g_GFX_OnceAnim(mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-16,
                                   mon.Obj.Y+mon.Obj.Rect.Y+(mon.Obj.Rect.Height div 2)-16, Anim);
                    Anim.Free();
                  end;
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_Effect(mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-16,
                                   mon.Obj.Y+mon.Obj.Rect.Y+(mon.Obj.Rect.Height div 2)-16, 1,
                                   NET_GFX_RESPAWN);
                end;
                EFFECT_FIRE: begin
                  if g_Frames_Get(FramesID, 'FRAMES_FIRE') then
                  begin
                    Anim := TAnimation.Create(FramesID, False, 4);
                    g_Sound_PlayExAt('SOUND_FIRE', tgcTX, tgcTY);
                    g_GFX_OnceAnim(mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-32,
                                   mon.Obj.Y+mon.Obj.Rect.Y+mon.Obj.Rect.Height-128, Anim);
                    Anim.Free();
                  end;
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_Effect(mon.Obj.X+mon.Obj.Rect.X+(mon.Obj.Rect.Width div 2)-32,
                                   mon.Obj.Y+mon.Obj.Rect.Y+mon.Obj.Rect.Height-128, 1,
                                   NET_GFX_FIRE);
                end;
              end;
            end;
          if g_Game_IsNet then
          begin
            MH_SEND_GameStats();
            MH_SEND_CoopStats();
          end;

          if coolDown then
            TimeOut := 18
          else
            TimeOut := 0;
          // Если активирован автоспавнером, не меняем текстуру
          if actType = ACTIVATE_CUSTOM then
            Result := False;
        end;

      TRIGGER_SPAWNITEM:
        if (tgcSpawnItemType in [ITEM_MEDKIT_SMALL..ITEM_MAX]) then
        begin
          Result := False;
          if (tgcDelay > 0) and (actType <> ACTIVATE_CUSTOM) then
          begin
            AutoSpawn := not AutoSpawn;
            SpawnCooldown := 0;
            // Автоспавнер переключен - меняем текстуру
            Result := True;
          end;

          if ((tgcDelay = 0) and (actType <> ACTIVATE_CUSTOM))
          or ((tgcDelay > 0) and (actType = ACTIVATE_CUSTOM)) then
            if (not tgcDmonly) or
               (gGameSettings.GameMode in [GM_DM, GM_TDM, GM_CTF]) then
              for k := 1 to tgcItemCount do
              begin
                if (actType = ACTIVATE_CUSTOM) and (tgcDelay > 0) then
                  SpawnCooldown := tgcDelay;
                if (tgcMax > 0) and (SpawnedCount >= tgcMax) then
                  Break;

                iid := g_Items_Create(tgcTX, tgcTY,
                  tgcSpawnItemType, tgcGravity, False, True);

                Result := True;

                if tgcMax > 0 then
                begin
                  it := g_Items_ByIdx(iid);
                  it.SpawnTrigger := ID;
                  Inc(SpawnedCount);
                end;

                case tgcEffect of
                  EFFECT_TELEPORT: begin
                    it := g_Items_ByIdx(iid);
                    if g_Frames_Get(FramesID, 'FRAMES_TELEPORT') then
                    begin
                      Anim := TAnimation.Create(FramesID, False, 3);
                      g_Sound_PlayExAt('SOUND_GAME_TELEPORT', tgcTX, tgcTY);
                      g_GFX_OnceAnim(it.Obj.X+it.Obj.Rect.X+(it.Obj.Rect.Width div 2)-32,
                                     it.Obj.Y+it.Obj.Rect.Y+(it.Obj.Rect.Height div 2)-32, Anim);
                      Anim.Free();
                    end;
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_Effect(it.Obj.X+it.Obj.Rect.X+(it.Obj.Rect.Width div 2)-32,
                                     it.Obj.Y+it.Obj.Rect.Y+(it.Obj.Rect.Height div 2)-32, 1,
                                     NET_GFX_TELE);
                  end;
                  EFFECT_RESPAWN: begin
                    it := g_Items_ByIdx(iid);
                    if g_Frames_Get(FramesID, 'FRAMES_ITEM_RESPAWN') then
                    begin
                      Anim := TAnimation.Create(FramesID, False, 4);
                      g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', tgcTX, tgcTY);
                      g_GFX_OnceAnim(it.Obj.X+it.Obj.Rect.X+(it.Obj.Rect.Width div 2)-16,
                                     it.Obj.Y+it.Obj.Rect.Y+(it.Obj.Rect.Height div 2)-16, Anim);
                      Anim.Free();
                    end;
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_Effect(it.Obj.X+it.Obj.Rect.X+(it.Obj.Rect.Width div 2)-16,
                                     it.Obj.Y+it.Obj.Rect.Y+(it.Obj.Rect.Height div 2)-16, 1,
                                     NET_GFX_RESPAWN);
                  end;
                  EFFECT_FIRE: begin
                    it := g_Items_ByIdx(iid);
                    if g_Frames_Get(FramesID, 'FRAMES_FIRE') then
                    begin
                      Anim := TAnimation.Create(FramesID, False, 4);
                      g_Sound_PlayExAt('SOUND_FIRE', tgcTX, tgcTY);
                      g_GFX_OnceAnim(it.Obj.X+it.Obj.Rect.X+(it.Obj.Rect.Width div 2)-32,
                                     it.Obj.Y+it.Obj.Rect.Y+it.Obj.Rect.Height-128, Anim);
                      Anim.Free();
                    end;
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_Effect(it.Obj.X+it.Obj.Rect.X+(it.Obj.Rect.Width div 2)-32,
                                     it.Obj.Y+it.Obj.Rect.Y+it.Obj.Rect.Height-128, 1,
                                     NET_GFX_FIRE);
                  end;
                end;

                if g_Game_IsNet then
                  MH_SEND_ItemSpawn(True, iid);
              end;

          if coolDown then
            TimeOut := 18
          else
            TimeOut := 0;
          // Если активирован автоспавнером, не меняем текстуру
          if actType = ACTIVATE_CUSTOM then
            Result := False;
        end;

      TRIGGER_MUSIC:
        begin
        // Меняем музыку, если есть на что:
          if (Trigger.tgcMusicName <> '') then
          begin
            gMusic.SetByName(Trigger.tgcMusicName);
            gMusic.SpecPause := True;
            gMusic.Play();
          end;

          case Trigger.tgcMusicAction of
            TRIGGER_MUSIC_ACTION_STOP: // Выключить
              gMusic.SpecPause := True; // Пауза
            TRIGGER_MUSIC_ACTION_PLAY: // Включить
              if gMusic.SpecPause then // Была на паузе => играть
                gMusic.SpecPause := False
              else // Играла => сначала
                gMusic.SetPosition(0);
          end;

          if coolDown then
            TimeOut := 36
          else
            TimeOut := 0;
          Result := True;
          if g_Game_IsNet then MH_SEND_TriggerMusic;
        end;

      TRIGGER_PUSH:
        begin
          pAngle := -DegToRad(tgcAngle);
          Result := tr_Push(ActivateUID,
                            Floor(Cos(pAngle)*tgcForce),
                            Floor(Sin(pAngle)*tgcForce),
                            tgcResetVelocity);
          TimeOut := 0;
        end;

      TRIGGER_SCORE:
        begin
          Result := False;
          // Прибавить или отнять очко
          if (tgcScoreAction in [TRIGGER_SCORE_ACTION_ADD, TRIGGER_SCORE_ACTION_SUB]) and (tgcScoreCount > 0) then
          begin
            // Своей или чужой команде
            if (tgcScoreTeam in [TRIGGER_SCORE_TEAM_MINE_RED, TRIGGER_SCORE_TEAM_MINE_BLUE]) and (g_GetUIDType(ActivateUID) = UID_PLAYER) then
            begin
              p := g_Player_Get(ActivateUID);
              if ((tgcScoreAction = TRIGGER_SCORE_ACTION_ADD) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_RED))
              or ((tgcScoreAction = TRIGGER_SCORE_ACTION_ADD) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_BLUE)) then
              begin
                Inc(gTeamStat[TEAM_RED].Goals, tgcScoreCount); // Red Scores

                if tgcScoreCon then
                begin
                  if (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) then
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_ADD_OWN], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '+r');
                  end else
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_ADD_ENEMY], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '+re');
                  end;
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, TEAM_RED);
                end;
              end;
              if ((tgcScoreAction = TRIGGER_SCORE_ACTION_SUB) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_RED))
              or ((tgcScoreAction = TRIGGER_SCORE_ACTION_SUB) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_BLUE)) then
              begin
                Dec(gTeamStat[TEAM_RED].Goals, tgcScoreCount); // Red Fouls

                if tgcScoreCon then
                begin
                  if (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) then
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_SUB_OWN], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '-r');
                  end else
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_SUB_ENEMY], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '-re');
                  end;
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, -TEAM_RED);
                end;
              end;
              if ((tgcScoreAction = TRIGGER_SCORE_ACTION_ADD) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_BLUE))
              or ((tgcScoreAction = TRIGGER_SCORE_ACTION_ADD) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_RED)) then
              begin
                Inc(gTeamStat[TEAM_BLUE].Goals, tgcScoreCount); // Blue Scores

                if tgcScoreCon then
                begin
                  if (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) then
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_ADD_OWN], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '+b');
                  end else
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_ADD_ENEMY], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '+be');
                  end;
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, TEAM_BLUE);
                end;
              end;
              if ((tgcScoreAction = TRIGGER_SCORE_ACTION_SUB) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_BLUE))
              or ((tgcScoreAction = TRIGGER_SCORE_ACTION_SUB) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_RED)) then
              begin
                Dec(gTeamStat[TEAM_BLUE].Goals, tgcScoreCount); // Blue Fouls

                if tgcScoreCon then
                begin
                  if (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) then
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_SUB_OWN], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '-b');
                  end else
                  begin
                    g_Console_Add(Format(_lc[I_PLAYER_SCORE_SUB_ENEMY], [p.Name, tgcScoreCount, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                    if g_Game_IsServer and g_Game_IsNet then
                      MH_SEND_GameEvent(NET_EV_SCORE, p.UID or (tgcScoreCount shl 16), '-be');
                  end;
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, -TEAM_BLUE);
                end;
              end;
              Result := (p.Team = TEAM_RED) or (p.Team = TEAM_BLUE);
            end;
            // Какой-то конкретной команде
            if tgcScoreTeam in [TRIGGER_SCORE_TEAM_FORCE_RED, TRIGGER_SCORE_TEAM_FORCE_BLUE] then
            begin
              if (tgcScoreAction = TRIGGER_SCORE_ACTION_ADD) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_RED) then
              begin
                Inc(gTeamStat[TEAM_RED].Goals, tgcScoreCount); // Red Scores

                if tgcScoreCon then
                begin
                  g_Console_Add(Format(_lc[I_PLAYER_SCORE_ADD_TEAM], [_lc[I_PLAYER_SCORE_RED], tgcScoreCount]), True);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE, tgcScoreCount shl 16, '+tr');
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, TEAM_RED);
                end;
              end;
              if (tgcScoreAction = TRIGGER_SCORE_ACTION_SUB) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_RED) then
              begin
                Dec(gTeamStat[TEAM_RED].Goals, tgcScoreCount); // Red Fouls

                if tgcScoreCon then
                begin
                  g_Console_Add(Format(_lc[I_PLAYER_SCORE_SUB_TEAM], [_lc[I_PLAYER_SCORE_RED], tgcScoreCount]), True);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE, tgcScoreCount shl 16, '-tr');
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, -TEAM_RED);
                end;
              end;
              if (tgcScoreAction = TRIGGER_SCORE_ACTION_ADD) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_BLUE) then
              begin
                Inc(gTeamStat[TEAM_BLUE].Goals, tgcScoreCount); // Blue Scores

                if tgcScoreCon then
                begin
                  g_Console_Add(Format(_lc[I_PLAYER_SCORE_ADD_TEAM], [_lc[I_PLAYER_SCORE_BLUE], tgcScoreCount]), True);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE, tgcScoreCount shl 16, '+tb');
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, TEAM_BLUE);
                end;
              end;
              if (tgcScoreAction = TRIGGER_SCORE_ACTION_SUB) and (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_BLUE) then
              begin
                Dec(gTeamStat[TEAM_BLUE].Goals, tgcScoreCount); // Blue Fouls

                if tgcScoreCon then
                begin
                  g_Console_Add(Format(_lc[I_PLAYER_SCORE_SUB_TEAM], [_lc[I_PLAYER_SCORE_BLUE], tgcScoreCount]), True);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE, tgcScoreCount shl 16, '-tb');
                end;

                if tgcScoreMsg then
                begin
                  g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
                  if g_Game_IsServer and g_Game_IsNet then
                    MH_SEND_GameEvent(NET_EV_SCORE_MSG, -TEAM_BLUE);
                end;
              end;
              Result := True;
            end;
          end;
          // Выигрыш
          if (tgcScoreAction = TRIGGER_SCORE_ACTION_WIN) and (gGameSettings.GoalLimit > 0) then
          begin
            // Своей или чужой команды
            if (tgcScoreTeam in [TRIGGER_SCORE_TEAM_MINE_RED, TRIGGER_SCORE_TEAM_MINE_BLUE]) and (g_GetUIDType(ActivateUID) = UID_PLAYER) then
            begin
              p := g_Player_Get(ActivateUID);
              if ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_RED)) // Red Wins
              or ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_BLUE)) then
              begin
                if gTeamStat[TEAM_RED].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_RED].Goals := gGameSettings.GoalLimit;

                  if tgcScoreCon then
                  begin
                    if (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) then
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_OWN], [p.Name, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wr');
                    end else
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_ENEMY], [p.Name, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wre');
                    end;
                  end;

                  Result := True;
                end;
              end;
              if ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_BLUE)) // Blue Wins
              or ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_RED)) then
              begin
                if gTeamStat[TEAM_BLUE].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_BLUE].Goals := gGameSettings.GoalLimit;

                  if tgcScoreCon then
                  begin
                    if (tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) then
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_OWN], [p.Name, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wb');
                    end else
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_ENEMY], [p.Name, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wbe');
                    end;
                  end;

                  Result := True;
                end;
              end;
            end;
            // Какой-то конкретной команды
            if tgcScoreTeam in [TRIGGER_SCORE_TEAM_FORCE_RED, TRIGGER_SCORE_TEAM_FORCE_BLUE] then
            begin
              if (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_RED) then // Red Wins
              begin
                if gTeamStat[TEAM_RED].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_RED].Goals := gGameSettings.GoalLimit;
                  Result := True;
                end;
              end;
              if (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_BLUE) then // Blue Wins
              begin
                if gTeamStat[TEAM_BLUE].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_BLUE].Goals := gGameSettings.GoalLimit;
                  Result := True;
                end;
              end;
            end;
          end;
          // Проигрыш
          if (tgcScoreAction = TRIGGER_SCORE_ACTION_LOOSE) and (gGameSettings.GoalLimit > 0) then
          begin
            // Своей или чужой команды
            if (tgcScoreTeam in [TRIGGER_SCORE_TEAM_MINE_RED, TRIGGER_SCORE_TEAM_MINE_BLUE]) and (g_GetUIDType(ActivateUID) = UID_PLAYER) then
            begin
              p := g_Player_Get(ActivateUID);
              if ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_BLUE)) // Red Wins
              or ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_RED)) then
                if gTeamStat[TEAM_RED].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_RED].Goals := gGameSettings.GoalLimit;

                  if tgcScoreCon then
                    if tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED then
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_ENEMY], [p.Name, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wre');
                    end else
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_OWN], [p.Name, _lc[I_PLAYER_SCORE_TO_RED]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wr');
                    end;

                  Result := True;
                end;
              if ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED) and (p.Team = TEAM_RED)) // Blue Wins
              or ((tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_BLUE) and (p.Team = TEAM_BLUE)) then
                if gTeamStat[TEAM_BLUE].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_BLUE].Goals := gGameSettings.GoalLimit;

                  if tgcScoreCon then
                    if tgcScoreTeam = TRIGGER_SCORE_TEAM_MINE_RED then
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_ENEMY], [p.Name, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wbe');
                    end else
                    begin
                      g_Console_Add(Format(_lc[I_PLAYER_SCORE_WIN_OWN], [p.Name, _lc[I_PLAYER_SCORE_TO_BLUE]]), True);
                      if g_Game_IsServer and g_Game_IsNet then
                        MH_SEND_GameEvent(NET_EV_SCORE, p.UID, 'wb');
                    end;

                  Result := True;
                end;
            end;
            // Какой-то конкретной команды
            if tgcScoreTeam in [TRIGGER_SCORE_TEAM_FORCE_BLUE, TRIGGER_SCORE_TEAM_FORCE_RED] then
            begin
              if (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_BLUE) then // Red Wins
              begin
                if gTeamStat[TEAM_RED].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_RED].Goals := gGameSettings.GoalLimit;
                  Result := True;
                end;
              end;
              if (tgcScoreTeam = TRIGGER_SCORE_TEAM_FORCE_RED) then // Blue Wins
              begin
                if gTeamStat[TEAM_BLUE].Goals < SmallInt(gGameSettings.GoalLimit) then
                begin
                  gTeamStat[TEAM_BLUE].Goals := gGameSettings.GoalLimit;
                  Result := True;
                end;
              end;
            end;
          end;
          if Result then begin
            if coolDown then
              TimeOut := 18
            else
              TimeOut := 0;
            if g_Game_IsServer and g_Game_IsNet then
              MH_SEND_GameStats;
          end;
        end;

      TRIGGER_MESSAGE:
        begin
          Result := tr_Message(tgcKind, tgcText,
                               tgcMsgDest, tgcMsgTime,
                               ActivateUID);
          TimeOut := 18;
        end;

      TRIGGER_DAMAGE, TRIGGER_HEALTH:
        begin
          Result := False;
          UIDType := g_GetUIDType(ActivateUID);
          if (UIDType = UID_PLAYER) or (UIDType = UID_MONSTER) then
          begin
            Result := True;
            k := -1;
            if coolDown then
            begin
              // Вспоминаем, активировал ли он меня раньше
              for idx := 0 to High(Activators) do
                if Activators[idx].UID = ActivateUID then
                begin
                  k := idx;
                  Break;
                end;
              if k = -1 then
              begin // Видим его впервые
                // Запоминаем его
                SetLength(Activators, Length(Activators) + 1);
                k := High(Activators);
                Activators[k].UID := ActivateUID;
              end else
              begin // Уже видели его
                // Если интервал отключён, но он всё ещё в зоне поражения, даём ему время
                if (tgcInterval = 0) and (Activators[k].TimeOut > 0) then
                  Activators[k].TimeOut := 65535;
                // Таймаут прошёл - работаем
                Result := Activators[k].TimeOut = 0;
              end;
            end;

            if Result then
            begin
              case UIDType of
                UID_PLAYER:
                  begin
                    p := g_Player_Get(ActivateUID);
                    if p = nil then
                      Exit;

                    // Наносим урон игроку
                    if (TriggerType = TRIGGER_DAMAGE) and (tgcAmount > 0) then
                      p.Damage(tgcAmount, 0, 0, 0, HIT_SOME);

                    // Лечим игрока
                    if (TriggerType = TRIGGER_HEALTH) and (tgcAmount > 0) then
                      if p.Heal(tgcAmount, not tgcHealMax) and (not tgcSilent) then
                      begin
                        g_Sound_PlayExAt('SOUND_ITEM_GETITEM', p.Obj.X, p.Obj.Y);
                        if g_Game_IsServer and g_Game_IsNet then
                          MH_SEND_Sound(p.Obj.X, p.Obj.Y, 'SOUND_ITEM_GETITEM');
                      end;
                  end;

                UID_MONSTER:
                  begin
                    m := g_Monsters_ByUID(ActivateUID);
                    if m = nil then
                      Exit;

                    // Наносим урон монстру
                    if (TriggerType = TRIGGER_DAMAGE) and (tgcAmount > 0) then
                      m.Damage(tgcAmount, 0, 0, 0, HIT_SOME);

                    // Лечим монстра
                    if (TriggerType = TRIGGER_HEALTH) and (tgcAmount > 0) then
                      if m.Heal(tgcAmount) and (not tgcSilent) then
                      begin
                        g_Sound_PlayExAt('SOUND_ITEM_GETITEM', m.Obj.X, m.Obj.Y);
                        if g_Game_IsServer and g_Game_IsNet then
                          MH_SEND_Sound(m.Obj.X, m.Obj.Y, 'SOUND_ITEM_GETITEM');
                      end;
                  end;
              end;
              // Назначаем время следующего воздействия
              idx := tgcInterval;
              if coolDown then
                if idx > 0 then
                  Activators[k].TimeOut := idx
                else
                  Activators[k].TimeOut := 65535;
            end;
          end;
          TimeOut := 0;
        end;

      TRIGGER_SHOT:
        begin
          if ShotSightTime > 0 then
            Exit;

          // put this at the beginning so it doesn't trigger itself
          TimeOut := tgcWait + 1;

          wx := tgcTX;
          wy := tgcTY;
          pAngle := -DegToRad(tgcAngle);
          xd := wx + Round(Cos(pAngle) * 32.0);
          yd := wy + Round(Sin(pAngle) * 32.0);
          TargetUID := 0;

          case tgcShotTarget of
            TRIGGER_SHOT_TARGET_MON: // monsters
              //TODO: accelerate this!
              g_Mons_ForEachAlive(monsShotTarget);

            TRIGGER_SHOT_TARGET_PLR: // players
              if gPlayers <> nil then
                for idx := Low(gPlayers) to High(gPlayers) do
                  if (gPlayers[idx] <> nil) and gPlayers[idx].alive and
                     tr_ShotAimCheck(Trigger, @(gPlayers[idx].Obj)) then
                  begin
                    xd := gPlayers[idx].GameX + PLAYER_RECT_CX;
                    yd := gPlayers[idx].GameY + PLAYER_RECT_CY;
                    TargetUID := gPlayers[idx].UID;
                    break;
                  end;

            TRIGGER_SHOT_TARGET_RED: // red team
              if gPlayers <> nil then
                for idx := Low(gPlayers) to High(gPlayers) do
                  if (gPlayers[idx] <> nil) and gPlayers[idx].alive and
                     (gPlayers[idx].Team = TEAM_RED) and
                     tr_ShotAimCheck(Trigger, @(gPlayers[idx].Obj)) then
                  begin
                    xd := gPlayers[idx].GameX + PLAYER_RECT_CX;
                    yd := gPlayers[idx].GameY + PLAYER_RECT_CY;
                    TargetUID := gPlayers[idx].UID;
                    break;
                  end;

            TRIGGER_SHOT_TARGET_BLUE: // blue team
              if gPlayers <> nil then
                for idx := Low(gPlayers) to High(gPlayers) do
                  if (gPlayers[idx] <> nil) and gPlayers[idx].alive and
                     (gPlayers[idx].Team = TEAM_BLUE) and
                     tr_ShotAimCheck(Trigger, @(gPlayers[idx].Obj)) then
                  begin
                    xd := gPlayers[idx].GameX + PLAYER_RECT_CX;
                    yd := gPlayers[idx].GameY + PLAYER_RECT_CY;
                    TargetUID := gPlayers[idx].UID;
                    break;
                  end;

            TRIGGER_SHOT_TARGET_MONPLR: // monsters then players
            begin
              //TODO: accelerate this!
              g_Mons_ForEachAlive(monsShotTargetMonPlr);

              if (TargetUID = 0) and (gPlayers <> nil) then
                for idx := Low(gPlayers) to High(gPlayers) do
                  if (gPlayers[idx] <> nil) and gPlayers[idx].alive and
                     tr_ShotAimCheck(Trigger, @(gPlayers[idx].Obj)) then
                  begin
                    xd := gPlayers[idx].GameX + PLAYER_RECT_CX;
                    yd := gPlayers[idx].GameY + PLAYER_RECT_CY;
                    TargetUID := gPlayers[idx].UID;
                    break;
                  end;
            end;

            TRIGGER_SHOT_TARGET_PLRMON: // players then monsters
            begin
              if gPlayers <> nil then
                for idx := Low(gPlayers) to High(gPlayers) do
                  if (gPlayers[idx] <> nil) and gPlayers[idx].alive and
                     tr_ShotAimCheck(Trigger, @(gPlayers[idx].Obj)) then
                  begin
                    xd := gPlayers[idx].GameX + PLAYER_RECT_CX;
                    yd := gPlayers[idx].GameY + PLAYER_RECT_CY;
                    TargetUID := gPlayers[idx].UID;
                    break;
                  end;
              if TargetUID = 0 then
              begin
                //TODO: accelerate this!
                g_Mons_ForEachAlive(monShotTargetPlrMon);
              end;
            end;

            else begin
              if (tgcShotTarget <> TRIGGER_SHOT_TARGET_NONE) or
                 (tgcShotType <> TRIGGER_SHOT_REV) then
                TargetUID := ActivateUID;
            end;
          end;

          if (tgcShotTarget = TRIGGER_SHOT_TARGET_NONE) or (TargetUID > 0) or
            ((tgcShotTarget > TRIGGER_SHOT_TARGET_NONE) and (TargetUID = 0)) then
          begin
            Result := True;
            if (tgcSight = 0) or
               (tgcShotTarget = TRIGGER_SHOT_TARGET_NONE) or
               (TargetUID = ShotSightTarget) then
              MakeShot(Trigger, wx, wy, xd, yd, TargetUID)
            else
            begin
              ShotSightTime := tgcSight;
              ShotSightTargetN := TargetUID;
              if tgcShotType = TRIGGER_SHOT_BFG then
              begin
                g_Sound_PlayExAt('SOUND_WEAPON_STARTFIREBFG', wx, wy);
                if g_Game_IsNet and g_Game_IsServer then
                  MH_SEND_Sound(wx, wy, 'SOUND_WEAPON_STARTFIREBFG');
              end;
            end;
          end;
        end;

      TRIGGER_EFFECT:
        begin
          idx := tgcFXCount;

          while idx > 0 do
          begin
            case tgcFXPos of
              TRIGGER_EFFECT_POS_CENTER:
              begin
                wx := X + Width div 2;
                wy := Y + Height div 2;
              end;
              TRIGGER_EFFECT_POS_AREA:
              begin
                wx := X + Random(Width);
                wy := Y + Random(Height);
              end;
              else begin
                wx := X + Width div 2;
                wy := Y + Height div 2;
              end;
            end;
            xd := tgcVelX;
            yd := tgcVelY;
            if tgcSpreadL > 0 then xd -= Random(tgcSpreadL+1);
            if tgcSpreadR > 0 then xd += Random(tgcSpreadR+1);
            if tgcSpreadU > 0 then yd -= Random(tgcSpreadU+1);
            if tgcSpreadD > 0 then yd += Random(tgcSpreadD+1);
            tr_MakeEffect(wx, wy, xd, yd,
                       tgcFXType, tgcFXSubType,
                       tgcFXRed, tgcFXGreen, tgcFXBlue, True, False);
            Dec(idx);
          end;
          TimeOut := tgcWait;
          result := true;
        end;
    end;
  end;

  if Result {and (Trigger.TexturePanel <> -1)} then
  begin
    g_Map_SwitchTextureGUID({Trigger.TexturePanelType,} Trigger.TexturePanelGUID, IfThen(animonce, 2, 1));
  end;
end;


function g_Triggers_CreateWithMapIndex (aTrigger: TTrigger; arridx, mapidx: Integer): DWORD;
var
  triggers: TDynField;
begin
  triggers := gCurrentMap['trigger'];
  if (triggers = nil) then raise Exception.Create('LOAD: map has no triggers');
  if (mapidx < 0) or (mapidx >= triggers.count) then raise Exception.Create('LOAD: invalid map trigger index');
  aTrigger.mapIndex := mapidx;
  result := g_Triggers_Create(aTrigger, triggers.itemAt[mapidx], arridx);
end;


function g_Triggers_Create (aTrigger: TTrigger; trec: TDynRecord; forceInternalIndex: Integer=-1): DWORD;
var
  find_id: DWORD;
  fn, mapw: AnsiString;
  f, olen: Integer;
  ptg: PTrigger;
begin
  if (tgscope = nil) then tgscope := TTrigScope.Create();
  if (tgclist = nil) then tgclist := TMyConstList.Create();

  // Не создавать выход, если игра без выхода
  if (aTrigger.TriggerType = TRIGGER_EXIT) and
     (not LongBool(gGameSettings.Options and GAME_OPTION_ALLOWEXIT)) then
  begin
    aTrigger.TriggerType := TRIGGER_NONE;
  end;

  // Если монстры запрещены, отменяем триггер
  if (aTrigger.TriggerType = TRIGGER_SPAWNMONSTER) and
     (not LongBool(gGameSettings.Options and GAME_OPTION_MONSTERS)) and
     (gGameSettings.GameType <> GT_SINGLE) then
  begin
    aTrigger.TriggerType := TRIGGER_NONE;
  end;

  // Считаем количество секретов на карте
  if (aTrigger.TriggerType = TRIGGER_SECRET) then gSecretsCount += 1;

  if (forceInternalIndex < 0) then
  begin
    find_id := FindTrigger();
  end
  else
  begin
    olen := Length(gTriggers);
    if (forceInternalIndex >= olen) then
    begin
      SetLength(gTriggers, forceInternalIndex+1);
      for f := olen to High(gTriggers) do
      begin
        gTriggers[f].TriggerType := TRIGGER_NONE;
        gTriggers[f].trigDataRec := nil;
        gTriggers[f].exoInit := nil;
        gTriggers[f].exoThink := nil;
        gTriggers[f].exoCheck := nil;
        gTriggers[f].exoAction := nil;
        gTriggers[f].userVars := nil;
      end;
    end;
    f := forceInternalIndex;
    gTriggers[f].trigDataRec.Free();
    gTriggers[f].exoInit.Free();
    gTriggers[f].exoThink.Free();
    gTriggers[f].exoCheck.Free();
    gTriggers[f].exoAction.Free();
    gTriggers[f].userVars.Free();
    gTriggers[f].trigDataRec := nil;
    gTriggers[f].exoInit := nil;
    gTriggers[f].exoThink := nil;
    gTriggers[f].exoCheck := nil;
    gTriggers[f].exoAction := nil;
    gTriggers[f].userVars := nil;
    find_id := DWORD(forceInternalIndex);
  end;
  gTriggers[find_id] := aTrigger;
  ptg := @gTriggers[find_id];

  ptg.mapId := trec.id;
  // clone trigger data
  if (trec.trigRec = nil) then
  begin
    ptg.trigDataRec := nil;
    //HACK!
    if (ptg.TriggerType <> TRIGGER_SECRET) then
    begin
      e_LogWritefln('trigger of type %s has no triggerdata; wtf?!', [ptg.TriggerType], TMsgType.Warning);
    end;
  end
  else
  begin
    ptg.trigDataRec := trec.trigRec.clone(nil);
  end;

  with ptg^ do
  begin
    ID := find_id;
    // if this type of trigger exists both on the client and on the server
    // use an uniform numeration
    ClientID := 0;
    if (ptg.TriggerType = TRIGGER_SOUND) then
    begin
      Inc(gTriggerClientID);
      ClientID := gTriggerClientID;
    end;
    TimeOut := 0;
    ActivateUID := 0;
    PlayerCollide := False;
    DoorTime := -1;
    PressTime := -1;
    PressCount := 0;
    SoundPlayCount := 0;
    Sound := nil;
    AutoSpawn := False;
    SpawnCooldown := 0;
    SpawnedCount := 0;
  end;

  // update cached trigger variables
  trigUpdateCacheData(ptg^, ptg.trigDataRec);

  ptg.userVars := nil;

  try
    ptg.exoThink := TExprBase.parseStatList(tgclist, VarToStr(trec.user['exoma_think']));
  except
    on e: TExomaParseException do
      begin
        conwritefln('*** ERROR parsing exoma_think (%s,%s): %s [%s]', [e.tokLine, e.tokCol, e.message, VarToStr(trec.user['exoma_think'])]);
        ptg.exoThink := nil;
      end;
    else
      raise;
  end;
  try
    ptg.exoCheck := TExprBase.parse(tgclist, VarToStr(trec.user['exoma_check']));
  except
    on e: TExomaParseException do
      begin
        conwritefln('*** ERROR parsing exoma_check (%s,%s): %s [%s]', [e.tokLine, e.tokCol, e.message, VarToStr(trec.user['exoma_check'])]);
        ptg.exoCheck := nil;
      end;
    else
      raise;
  end;
  try
    ptg.exoAction := TExprBase.parseStatList(tgclist, VarToStr(trec.user['exoma_action']));
  except
    on e: TExomaParseException do
      begin
        conwritefln('*** ERROR parsing exoma_action (%s,%s): %s [%s]', [e.tokLine, e.tokCol, e.message, VarToStr(trec.user['exoma_action'])]);
        ptg.exoAction := nil;
      end;
    else
      raise;
  end;
  try
    ptg.exoInit := TExprBase.parseStatList(tgclist, VarToStr(trec.user['exoma_init']));
  except
    on e: TExomaParseException do
      begin
        conwritefln('*** ERROR parsing exoma_init (%s,%s): %s [%s]', [e.tokLine, e.tokCol, e.message, VarToStr(trec.user['exoma_init'])]);
        ptg.exoInit := nil;
      end;
    else
      raise;
  end;

  if (forceInternalIndex < 0) and (ptg.exoInit <> nil) then
  begin
    //conwritefln('executing trigger init: [%s]', [gTriggers[find_id].exoInit.toString()]);
    try
      tgscope.me := ptg;
      ptg.exoInit.value(tgscope);
      tgscope.me := nil;
    except
      tgscope.me := nil;
      conwritefln('*** trigger exoactivate error: %s', [ptg.exoInit.toString()]);
      exit;
    end;
  end;

  // Загружаем звук, если это триггер "Звук"
  if (ptg.TriggerType = TRIGGER_SOUND) and (ptg.tgcSoundName <> '') then
  begin
    // Еще нет такого звука
    if not g_Sound_Exists(ptg.tgcSoundName) then
    begin
      fn := g_ExtractWadName(ptg.tgcSoundName);
      if (fn = '') then
      begin // Звук в файле с картой
        mapw := g_ExtractWadName(gMapInfo.Map);
        fn := mapw+':'+g_ExtractFilePathName(ptg.tgcSoundName);
      end
      else // Звук в отдельном файле
      begin
        fn := GameDir + '/wads/' + ptg.tgcSoundName;
      end;

      //e_LogWritefln('loading trigger sound ''%s''', [fn]);
      if not g_Sound_CreateWADEx(ptg.tgcSoundName, fn) then
      begin
        g_FatalError(Format(_lc[I_GAME_ERROR_TR_SOUND], [fn, ptg.tgcSoundName]));
      end;
    end;

    // Создаем объект звука
    with ptg^ do
    begin
      Sound := TPlayableSound.Create();
      if not Sound.SetByName(ptg.tgcSoundName) then
      begin
        Sound.Free();
        Sound := nil;
      end;
    end;
  end;

  // Загружаем музыку, если это триггер "Музыка"
  if (ptg.TriggerType = TRIGGER_MUSIC) and (ptg.tgcMusicName <> '') then
  begin
    // Еще нет такой музыки
    if not g_Sound_Exists(ptg.tgcMusicName) then
    begin
      fn := g_ExtractWadName(ptg.tgcMusicName);

      if fn = '' then
      begin // Музыка в файле с картой
        mapw := g_ExtractWadName(gMapInfo.Map);
        fn := mapw+':'+g_ExtractFilePathName(ptg.tgcMusicName);
      end
      else // Музыка в файле с картой
      begin
        fn := GameDir+'/wads/'+ptg.tgcMusicName;
      end;

      if not g_Sound_CreateWADEx(ptg.tgcMusicName, fn, True) then
      begin
        g_FatalError(Format(_lc[I_GAME_ERROR_TR_SOUND], [fn, ptg.tgcMusicName]));
      end;
    end;
  end;

  // Загружаем данные триггера "Турель"
  if (ptg.TriggerType = TRIGGER_SHOT) then
  begin
    with ptg^ do
    begin
      ShotPanelTime := 0;
      ShotSightTime := 0;
      ShotSightTimeout := 0;
      ShotSightTarget := 0;
      ShotSightTargetN := 0;
      ShotAmmoCount := ptg.tgcAmmo;
      ShotReloadTime := 0;
    end;
  end;

  Result := find_id;
end;


// sorry; grid doesn't support recursive queries, so we have to do this
type
  TSimpleMonsterList = specialize TSimpleList<TMonster>;

var
  tgMonsList: TSimpleMonsterList = nil;

procedure g_Triggers_Update();
var
  a, b, i: Integer;
  Affected: array of Integer;

  function monsNear (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    {
    gTriggers[a].ActivateUID := mon.UID;
    ActivateTrigger(gTriggers[a], ACTIVATE_MONSTERCOLLIDE);
    }
    tgMonsList.append(mon);
  end;

var
  mon: TMonster;
  pan: TPanel;
begin
  if (tgMonsList = nil) then tgMonsList := TSimpleMonsterList.Create();

  if gTriggers = nil then
    Exit;
  SetLength(Affected, 0);

  for a := 0 to High(gTriggers) do
    with gTriggers[a] do
    // Есть триггер:
      if TriggerType <> TRIGGER_NONE then
      begin
        // Уменьшаем время до закрытия двери (открытия ловушки)
        if DoorTime > 0 then DoorTime := DoorTime - 1;
        // Уменьшаем время ожидания после нажатия
        if PressTime > 0 then PressTime := PressTime - 1;
        // Проверяем игроков и монстров, которых ранее запомнили:
        if (TriggerType = TRIGGER_DAMAGE) or (TriggerType = TRIGGER_HEALTH) then
        begin
          for b := 0 to High(Activators) do
          begin
            // Уменьшаем время до повторного воздействия:
            if Activators[b].TimeOut > 0 then
            begin
              Dec(Activators[b].TimeOut);
            end
            else
            begin
              continue;
            end;
            // Считаем, что объект покинул зону действия триггера
            if (tgcInterval = 0) and (Activators[b].TimeOut < 65530) then Activators[b].TimeOut := 0;
          end;
        end;

        // Обрабатываем спавнеры
        if Enabled and AutoSpawn then
        begin
          if SpawnCooldown = 0 then
          begin
            // Если пришло время, спавним монстра
            if (TriggerType = TRIGGER_SPAWNMONSTER) and (tgcDelay > 0)  then
            begin
              ActivateUID := 0;
              ActivateTrigger(gTriggers[a], ACTIVATE_CUSTOM);
            end;
            // Если пришло время, спавним предмет
            if (TriggerType = TRIGGER_SPAWNITEM) and (tgcDelay > 0) then
            begin
              ActivateUID := 0;
              ActivateTrigger(gTriggers[a], ACTIVATE_CUSTOM);
            end;
          end
          else
          begin
            // Уменьшаем время ожидания
            Dec(SpawnCooldown);
          end;
        end;

        // Обрабатываем события триггера "Турель"
        if TriggerType = TRIGGER_SHOT then
        begin
          if ShotPanelTime > 0 then
          begin
            Dec(ShotPanelTime);
            if ShotPanelTime = 0 then g_Map_SwitchTextureGUID({ShotPanelType,} trigPanelGUID);
          end;
          if ShotSightTime > 0 then
          begin
            Dec(ShotSightTime);
            if ShotSightTime = 0 then ShotSightTarget := ShotSightTargetN;
          end;
          if ShotSightTimeout > 0 then
          begin
            Dec(ShotSightTimeout);
            if ShotSightTimeout = 0 then ShotSightTarget := 0;
          end;
          if ShotReloadTime > 0 then
          begin
            Dec(ShotReloadTime);
            if ShotReloadTime = 0 then ShotAmmoCount := tgcAmmo;
          end;
        end;

        // Триггер "Звук" уже отыграл, если нужно еще - перезапускаем
        if Enabled and (TriggerType = TRIGGER_SOUND) and (Sound <> nil) then
        begin
          if (SoundPlayCount > 0) and (not Sound.IsPlaying()) then
          begin
            if tgcPlayCount > 0 then SoundPlayCount -= 1; // Если 0 - играем звук бесконечно
            if tgcLocal then
            begin
              Sound.PlayVolumeAt(X+(Width div 2), Y+(Height div 2), tgcVolume/255.0);
            end
            else
            begin
              Sound.PlayPanVolume((tgcPan-127.0)/128.0, tgcVolume/255.0);
            end;
            if Sound.IsPlaying() and g_Game_IsNet and g_Game_IsServer then MH_SEND_TriggerSound(gTriggers[a]);
          end;
        end;

        // Триггер "Ловушка" - пора открывать
        if (TriggerType = TRIGGER_TRAP) and (DoorTime = 0) and (g_Map_PanelByGUID(trigPanelGUID) <> nil) then
        begin
          tr_OpenDoor(trigPanelGUID, tgcSilent, tgcD2d);
          DoorTime := -1;
        end;

        // Триггер "Дверь 5 сек" - пора закрывать
        if (TriggerType = TRIGGER_DOOR5) and (DoorTime = 0) and (g_Map_PanelByGUID(trigPanelGUID) <> nil) then
        begin
          pan := g_Map_PanelByGUID(trigPanelGUID);
          if (pan <> nil) and pan.isGWall then
          begin
            // Уже закрыта
            if {gWalls[trigPanelID].Enabled} pan.Enabled then
            begin
              DoorTime := -1;
            end
            else
            begin
              // Пока открыта - закрываем
              if tr_CloseDoor(trigPanelGUID, tgcSilent, tgcD2d) then DoorTime := -1;
            end;
          end;
        end;

      // Триггер - расширитель или переключатель, и прошла задержка, и нажали нужное число раз:
        if (TriggerType in [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF]) and
           (PressTime = 0) and (PressCount >= tgcPressCount) then
        begin
          // Сбрасываем задержку активации:
          PressTime := -1;
          // Сбрасываем счетчик нажатий:
          if tgcPressCount > 0 then PressCount -= tgcPressCount else PressCount := 0;

          // Определяем изменяемые им триггеры:
          for b := 0 to High(gTriggers) do
          begin
            if g_Collide(tgcTX, tgcTY, tgcTWidth, tgcTHeight, gTriggers[b].X, gTriggers[b].Y,
               gTriggers[b].Width, gTriggers[b].Height) and
               ((b <> a) or (tgcWait > 0)) then
            begin // Can be self-activated, if there is Data.Wait
              if (not tgcExtRandom) or gTriggers[b].Enabled then
              begin
                SetLength(Affected, Length(Affected) + 1);
                Affected[High(Affected)] := b;
              end;
            end;
          end;

          //HACK!
          // if we have panelid, assume that it will switch the moving platform
          pan := g_Map_PanelByGUID(trigPanelGUID);
          if (pan <> nil) then
          begin
            case TriggerType of
              TRIGGER_PRESS: pan.movingActive := true; // what to do here?
              TRIGGER_ON: pan.movingActive := true;
              TRIGGER_OFF: pan.movingActive := false;
              TRIGGER_ONOFF: pan.movingActive := not pan.movingActive;
            end;
            if not tgcSilent and (Length(tgcSound) > 0) then
            begin
              g_Sound_PlayExAt(tgcSound, X, Y);
              if g_Game_IsServer and g_Game_IsNet then MH_SEND_Sound(X, Y, tgcSound);
            end;
          end;

          // Выбираем один из триггеров для расширителя, если включен рандом:
          if (TriggerType = TRIGGER_PRESS) and tgcExtRandom then
          begin
            if (Length(Affected) > 0) then
            begin
              b := Affected[Random(Length(Affected))];
              gTriggers[b].ActivateUID := gTriggers[a].ActivateUID;
              ActivateTrigger(gTriggers[b], 0);
            end;
          end
          else // В противном случае работаем как обычно:
          begin
            for i := 0 to High(Affected) do
            begin
              b := Affected[i];
              case TriggerType of
                TRIGGER_PRESS:
                  begin
                    gTriggers[b].ActivateUID := gTriggers[a].ActivateUID;
                    ActivateTrigger(gTriggers[b], 0);
                  end;
                TRIGGER_ON:
                  begin
                    gTriggers[b].Enabled := True;
                  end;
                TRIGGER_OFF:
                  begin
                    gTriggers[b].Enabled := False;
                    gTriggers[b].TimeOut := 0;
                    if gTriggers[b].AutoSpawn then
                    begin
                      gTriggers[b].AutoSpawn := False;
                      gTriggers[b].SpawnCooldown := 0;
                    end;
                  end;
                TRIGGER_ONOFF:
                  begin
                    gTriggers[b].Enabled := not gTriggers[b].Enabled;
                    if not gTriggers[b].Enabled then
                    begin
                      gTriggers[b].TimeOut := 0;
                      if gTriggers[b].AutoSpawn then
                      begin
                        gTriggers[b].AutoSpawn := False;
                        gTriggers[b].SpawnCooldown := 0;
                      end;
                    end;
                  end;
              end;
            end;
          end;
          SetLength(Affected, 0);
        end;

      // Уменьшаем время до возможности повторной активации:
        if TimeOut > 0 then
        begin
          TimeOut := TimeOut - 1;
          Continue; // Чтобы не потерять 1 единицу задержки
        end;

      // Ниже идут типы активации, если триггер отключён - идём дальше
        if not Enabled then
          Continue;

      // "Игрок близко":
        if ByteBool(ActivateType and ACTIVATE_PLAYERCOLLIDE) and
           (TimeOut = 0) then
          if gPlayers <> nil then
            for b := 0 to High(gPlayers) do
              if gPlayers[b] <> nil then
                with gPlayers[b] do
                // Жив, есть нужные ключи и он рядом:
                  if alive and ((gTriggers[a].Keys and GetKeys) = gTriggers[a].Keys) and
                     Collide(X, Y, Width, Height) then
                  begin
                    gTriggers[a].ActivateUID := UID;

                    if (gTriggers[a].TriggerType in [TRIGGER_SOUND, TRIGGER_MUSIC]) and
                       PlayerCollide then
                      { Don't activate sound/music again if player is here }
                    else
                      ActivateTrigger(gTriggers[a], ACTIVATE_PLAYERCOLLIDE);
                  end;

        { TODO 5 : активация монстрами триггеров с ключами }

        if ByteBool(ActivateType and ACTIVATE_MONSTERCOLLIDE) and
           ByteBool(ActivateType and ACTIVATE_NOMONSTER) and
           (TimeOut = 0) and (Keys = 0) then
        begin
        // Если "Монстр близко" и "Монстров нет",
        // запускаем триггер на старте карты и снимаем оба флага
          ActivateType := ActivateType and not (ACTIVATE_MONSTERCOLLIDE or ACTIVATE_NOMONSTER);
          gTriggers[a].ActivateUID := 0;
          ActivateTrigger(gTriggers[a], 0);
        end else
        begin
          // "Монстр близко"
          if ByteBool(ActivateType and ACTIVATE_MONSTERCOLLIDE) and
             (TimeOut = 0) and (Keys = 0) then // Если не нужны ключи
          begin
            //g_Mons_ForEach(monsNear);
            //Alive?!
            tgMonsList.reset();
            g_Mons_ForEachAt(gTriggers[a].X, gTriggers[a].Y, gTriggers[a].Width, gTriggers[a].Height, monsNear);
            for mon in tgMonsList do
            begin
              gTriggers[a].ActivateUID := mon.UID;
              ActivateTrigger(gTriggers[a], ACTIVATE_MONSTERCOLLIDE);
            end;
            tgMonsList.reset(); // just in case
          end;

          // "Монстров нет"
          if ByteBool(ActivateType and ACTIVATE_NOMONSTER) and
             (TimeOut = 0) and (Keys = 0) then
            if not g_Mons_IsAnyAliveAt(X, Y, Width, Height) then
            begin
              gTriggers[a].ActivateUID := 0;
              ActivateTrigger(gTriggers[a], ACTIVATE_NOMONSTER);
            end;
        end;

        PlayerCollide := g_CollidePlayer(X, Y, Width, Height);
      end;
end;

procedure g_Triggers_Press(ID: DWORD; ActivateType: Byte; ActivateUID: Word = 0);
begin
  if (ID >= Length(gTriggers)) then exit;
  gTriggers[ID].ActivateUID := ActivateUID;
  ActivateTrigger(gTriggers[ID], ActivateType);
end;

function g_Triggers_PressR(X, Y: Integer; Width, Height: Word; UID: Word;
                           ActivateType: Byte; IgnoreList: DWArray = nil): DWArray;
var
  a: Integer;
  k: Byte;
  p: TPlayer;
begin
  Result := nil;

  if gTriggers = nil then Exit;

  case g_GetUIDType(UID) of
    UID_GAME: k := 255;
    UID_PLAYER:
    begin
      p := g_Player_Get(UID);
      if p <> nil then
        k := p.GetKeys
      else
        k := 0;
    end;
    else k := 0;
  end;

  for a := 0 to High(gTriggers) do
    if (gTriggers[a].TriggerType <> TRIGGER_NONE) and
       (gTriggers[a].TimeOut = 0) and
       (not InDWArray(a, IgnoreList)) and
       ((gTriggers[a].Keys and k) = gTriggers[a].Keys) and
       ByteBool(gTriggers[a].ActivateType and ActivateType) then
      if g_Collide(X, Y, Width, Height,
         gTriggers[a].X, gTriggers[a].Y,
         gTriggers[a].Width, gTriggers[a].Height) then
      begin
        gTriggers[a].ActivateUID := UID;
        if ActivateTrigger(gTriggers[a], ActivateType) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)] := a;
        end;
      end;
end;

procedure g_Triggers_PressL(X1, Y1, X2, Y2: Integer; UID: DWORD; ActivateType: Byte);
var
  a: Integer;
  k: Byte;
  p: TPlayer;
begin
  if gTriggers = nil then Exit;

  case g_GetUIDType(UID) of
    UID_GAME: k := 255;
    UID_PLAYER:
    begin
      p := g_Player_Get(UID);
      if p <> nil then
        k := p.GetKeys
      else
        k := 0;
    end;
    else k := 0;
  end;

  for a := 0 to High(gTriggers) do
    if (gTriggers[a].TriggerType <> TRIGGER_NONE) and
       (gTriggers[a].TimeOut = 0) and
       ((gTriggers[a].Keys and k) = gTriggers[a].Keys) and
       ByteBool(gTriggers[a].ActivateType and ActivateType) then
      if g_CollideLine(x1, y1, x2, y2, gTriggers[a].X, gTriggers[a].Y,
         gTriggers[a].Width, gTriggers[a].Height) then
      begin
        gTriggers[a].ActivateUID := UID;
        ActivateTrigger(gTriggers[a], ActivateType);
      end;
end;

procedure g_Triggers_PressC(CX, CY: Integer; Radius: Word; UID: Word; ActivateType: Byte; IgnoreTrigger: Integer = -1);
var
  a: Integer;
  k: Byte;
  rsq: Word;
  p: TPlayer;
begin
  if gTriggers = nil then
    Exit;

  case g_GetUIDType(UID) of
    UID_GAME: k := 255;
    UID_PLAYER:
    begin
     p := g_Player_Get(UID);
     if p <> nil then
      k := p.GetKeys
     else
      k := 0;
    end;
    else k := 0;
  end;

  rsq := Radius * Radius;

  for a := 0 to High(gTriggers) do
    if (gTriggers[a].ID <> DWORD(IgnoreTrigger)) and
       (gTriggers[a].TriggerType <> TRIGGER_NONE) and
       (gTriggers[a].TimeOut = 0) and
       ((gTriggers[a].Keys and k) = gTriggers[a].Keys) and
       ByteBool(gTriggers[a].ActivateType and ActivateType) then
      with gTriggers[a] do
        if g_Collide(CX-Radius, CY-Radius, 2*Radius, 2*Radius,
                     X, Y, Width, Height) then
          if ((Sqr(CX-X)+Sqr(CY-Y)) < rsq) or // Центр круга близок к верхнему левому углу
             ((Sqr(CX-X-Width)+Sqr(CY-Y)) < rsq) or // Центр круга близок к верхнему правому углу
             ((Sqr(CX-X-Width)+Sqr(CY-Y-Height)) < rsq) or // Центр круга близок к нижнему правому углу
             ((Sqr(CX-X)+Sqr(CY-Y-Height)) < rsq) or // Центр круга близок к нижнему левому углу
             ( (CX > (X-Radius)) and (CX < (X+Width+Radius)) and
               (CY > Y) and (CY < (Y+Height)) ) or // Центр круга недалеко от вертикальных границ прямоугольника
             ( (CY > (Y-Radius)) and (CY < (Y+Height+Radius)) and
               (CX > X) and (CX < (X+Width)) ) then // Центр круга недалеко от горизонтальных границ прямоугольника
          begin
            ActivateUID := UID;
            ActivateTrigger(gTriggers[a], ActivateType);
          end;
end;

procedure g_Triggers_OpenAll();
var
  a: Integer;
  b: Boolean;
begin
  if gTriggers = nil then Exit;

  b := False;
  for a := 0 to High(gTriggers) do
  begin
    with gTriggers[a] do
    begin
      if (TriggerType = TRIGGER_OPENDOOR) or
         (TriggerType = TRIGGER_DOOR5) or
         (TriggerType = TRIGGER_DOOR) then
      begin
        tr_OpenDoor(trigPanelGUID, True, tgcD2d);
        if TriggerType = TRIGGER_DOOR5 then DoorTime := 180;
        b := True;
      end;
    end;
  end;

  if b then g_Sound_PlayEx('SOUND_GAME_DOOROPEN');
end;

procedure g_Triggers_DecreaseSpawner(ID: DWORD);
begin
  if (gTriggers <> nil) then
    if gTriggers[ID].SpawnedCount > 0 then
      Dec(gTriggers[ID].SpawnedCount);
end;


procedure g_Triggers_Free ();
var
  a: Integer;
begin
  for a := 0 to High(gTriggers) do
  begin
    if (gTriggers[a].TriggerType = TRIGGER_SOUND) then
    begin
      if g_Sound_Exists(gTriggers[a].tgcSoundName) then
      begin
        g_Sound_Delete(gTriggers[a].tgcSoundName);
      end;
      gTriggers[a].Sound.Free();
    end;
    if (gTriggers[a].Activators <> nil) then
    begin
      SetLength(gTriggers[a].Activators, 0);
    end;
    gTriggers[a].trigDataRec.Free();

    gTriggers[a].exoThink.Free();
    gTriggers[a].exoCheck.Free();
    gTriggers[a].exoAction.Free();
    gTriggers[a].userVars.Free();
  end;

  gTriggers := nil;
  gSecretsCount := 0;
  SetLength(gMonstersSpawned, 0);
end;


procedure g_Triggers_SaveState (st: TStream);
var
  count, actCount, i, j: Integer;
  sg: Single;
  b: Boolean;
  kv: THashStrVariant.PEntry;
  t: LongInt;
begin
  // Считаем количество существующих триггеров
  count := Length(gTriggers);

  // Количество триггеров
  utils.writeInt(st, LongInt(count));
  if (count = 0) then exit;

  for i := 0 to High(gTriggers) do
  begin
    // Сигнатура триггера
    utils.writeSign(st, 'TRGX');
    utils.writeInt(st, Byte(0));
    // Тип триггера
    utils.writeInt(st, Byte(gTriggers[i].TriggerType));
    if (gTriggers[i].TriggerType = TRIGGER_NONE) then continue; // empty one
    // Специальные данные триггера: потом из карты опять вытащим; сохраним только индекс
    utils.writeInt(st, LongInt(gTriggers[i].mapIndex));
    // Координаты левого верхнего угла
    utils.writeInt(st, LongInt(gTriggers[i].X));
    utils.writeInt(st, LongInt(gTriggers[i].Y));
    // Размеры
    utils.writeInt(st, Word(gTriggers[i].Width));
    utils.writeInt(st, Word(gTriggers[i].Height));
    // Включен ли триггер
    utils.writeBool(st, gTriggers[i].Enabled);
    // Тип активации триггера
    utils.writeInt(st, Byte(gTriggers[i].ActivateType));
    // Ключи, необходимые для активации
    utils.writeInt(st, Byte(gTriggers[i].Keys));
    // ID панели, текстура которой изменится
    utils.writeInt(st, LongInt(gTriggers[i].TexturePanelGUID));
    // Тип этой панели
    //Mem.WriteWord(gTriggers[i].TexturePanelType);
    // Внутренний номер другой панели (по счастливой случайности он будет совпадать с тем, что создано при загрузке карты)
    utils.writeInt(st, LongInt(gTriggers[i].trigPanelGUID));
    // Время до возможности активации
    utils.writeInt(st, Word(gTriggers[i].TimeOut));
    // UID того, кто активировал этот триггер
    utils.writeInt(st, Word(gTriggers[i].ActivateUID));
    // Список UID-ов объектов, которые находились под воздействием
    actCount := Length(gTriggers[i].Activators);
    utils.writeInt(st, LongInt(actCount));
    for j := 0 to actCount-1 do
    begin
      // UID объекта
      utils.writeInt(st, Word(gTriggers[i].Activators[j].UID));
      // Время ожидания
      utils.writeInt(st, Word(gTriggers[i].Activators[j].TimeOut));
    end;
    // Стоит ли игрок в области триггера
    utils.writeBool(st, gTriggers[i].PlayerCollide);
    // Время до закрытия двери
    utils.writeInt(st, LongInt(gTriggers[i].DoorTime));
    // Задержка активации
    utils.writeInt(st, LongInt(gTriggers[i].PressTime));
    // Счетчик нажатий
    utils.writeInt(st, LongInt(gTriggers[i].PressCount));
    // Спавнер активен
    utils.writeBool(st, gTriggers[i].AutoSpawn);
    // Задержка спавнера
    utils.writeInt(st, LongInt(gTriggers[i].SpawnCooldown));
    // Счетчик создания объектов
    utils.writeInt(st, LongInt(gTriggers[i].SpawnedCount));
    // Сколько раз проигран звук
    utils.writeInt(st, LongInt(gTriggers[i].SoundPlayCount));
    // Проигрывается ли звук?
    if (gTriggers[i].Sound <> nil) then b := gTriggers[i].Sound.IsPlaying() else b := false;
    utils.writeBool(st, b);
    if b then
    begin
      // Позиция проигрывания звука
      utils.writeInt(st, LongWord(gTriggers[i].Sound.GetPosition()));
      // Громкость звука
      sg := gTriggers[i].Sound.GetVolume();
      sg := sg/(gSoundLevel/255.0);
      //Mem.WriteSingle(sg);
      st.WriteBuffer(sg, sizeof(sg)); // sorry
      // Стерео смещение звука
      sg := gTriggers[i].Sound.GetPan();
      //Mem.WriteSingle(sg);
      st.WriteBuffer(sg, sizeof(sg)); // sorry
    end;
    // uservars
    if (gTriggers[i].userVars = nil) then
    begin
      utils.writeInt(st, LongInt(0));
    end
    else
    begin
      utils.writeInt(st, LongInt(gTriggers[i].userVars.count)); //FIXME: check for overflow
      for kv in gTriggers[i].userVars.byKeyValue do
      begin
        //writeln('<', kv.key, '>:<', VarToStr(kv.value), '>');
        utils.writeStr(st, kv.key);
        t := LongInt(varType(kv.value));
        utils.writeInt(st, LongInt(t));
        case t of
          varString: utils.writeStr(st, AnsiString(kv.value));
          varBoolean: utils.writeBool(st, Boolean(kv.value));
          varShortInt: utils.writeInt(st, LongInt(kv.value));
          varSmallint: utils.writeInt(st, LongInt(kv.value));
          varInteger: utils.writeInt(st, LongInt(kv.value));
          //varInt64: Mem.WriteInt(Integer(kv.value));
          varByte: utils.writeInt(st, LongInt(kv.value));
          varWord: utils.writeInt(st, LongInt(kv.value));
          varLongWord: utils.writeInt(st, LongInt(kv.value));
          //varQWord:
          else raise Exception.CreateFmt('cannot save uservar ''%s''', [kv.key]);
        end;
      end;
    end;
  end;
end;


procedure g_Triggers_LoadState (st: TStream);
var
  count, actCount, i, j, a: Integer;
  dw: DWORD;
  vol, pan: Single;
  b: Boolean;
  Trig: TTrigger;
  mapIndex: Integer;
  uvcount: Integer;
  vt: LongInt;
  vv: Variant;
  uvname: AnsiString = '';
  ustr: AnsiString = '';
  uint: LongInt;
  ubool: Boolean;
begin
  assert(st <> nil);

  g_Triggers_Free();

  // Количество триггеров
  count := utils.readLongInt(st);
  if (count = 0) then exit;
  if (count < 0) or (count > 1024*1024) then raise XStreamError.Create('invalid trigger count');

  for a := 0 to count-1 do
  begin
    // Сигнатура триггера
    if not utils.checkSign(st, 'TRGX') then raise XStreamError.Create('invalid trigger signature');
    if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid trigger version');
    // Тип триггера
    Trig.TriggerType := utils.readByte(st);
    if (Trig.TriggerType = TRIGGER_NONE) then continue; // empty one
    // Специальные данные триггера: индекс в gCurrentMap.field['triggers']
    mapIndex := utils.readLongInt(st);
    i := g_Triggers_CreateWithMapIndex(Trig, a, mapIndex);
    // Координаты левого верхнего угла
    gTriggers[i].X := utils.readLongInt(st);
    gTriggers[i].Y := utils.readLongInt(st);
    // Размеры
    gTriggers[i].Width := utils.readWord(st);
    gTriggers[i].Height := utils.readWord(st);
    // Включен ли триггер
    gTriggers[i].Enabled := utils.readBool(st);
    // Тип активации триггера
    gTriggers[i].ActivateType := utils.readByte(st);
    // Ключи, необходимые для активации
    gTriggers[i].Keys := utils.readByte(st);
    // ID панели, текстура которой изменится
    gTriggers[i].TexturePanelGUID := utils.readLongInt(st);
    // Тип этой панели
    //Mem.ReadWord(gTriggers[i].TexturePanelType);
    // Внутренний номер другой панели (по счастливой случайности он будет совпадать с тем, что создано при загрузке карты)
    gTriggers[i].trigPanelGUID := utils.readLongInt(st);
    // Время до возможности активации
    gTriggers[i].TimeOut := utils.readWord(st);
    // UID того, кто активировал этот триггер
    gTriggers[i].ActivateUID := utils.readWord(st);
    // Список UID-ов объектов, которые находились под воздействием
    actCount := utils.readLongInt(st);
    if (actCount < 0) or (actCount > 1024*1024) then raise XStreamError.Create('invalid activated object count');
    if (actCount > 0) then
    begin
      SetLength(gTriggers[i].Activators, actCount);
      for j := 0 to actCount-1 do
      begin
        // UID объекта
        gTriggers[i].Activators[j].UID := utils.readWord(st);
        // Время ожидания
        gTriggers[i].Activators[j].TimeOut := utils.readWord(st);
      end;
    end;
    // Стоит ли игрок в области триггера
    gTriggers[i].PlayerCollide := utils.readBool(st);
    // Время до закрытия двери
    gTriggers[i].DoorTime := utils.readLongInt(st);
    // Задержка активации
    gTriggers[i].PressTime := utils.readLongInt(st);
    // Счетчик нажатий
    gTriggers[i].PressCount := utils.readLongInt(st);
    // Спавнер активен
    gTriggers[i].AutoSpawn := utils.readBool(st);
    // Задержка спавнера
    gTriggers[i].SpawnCooldown := utils.readLongInt(st);
    // Счетчик создания объектов
    gTriggers[i].SpawnedCount := utils.readLongInt(st);
    // Сколько раз проигран звук
    gTriggers[i].SoundPlayCount := utils.readLongInt(st);
    // Проигрывается ли звук?
    b := utils.readBool(st);
    if b then
    begin
      // Позиция проигрывания звука
      dw := utils.readLongWord(st);
      // Громкость звука
      //Mem.ReadSingle(vol);
      st.ReadBuffer(vol, sizeof(vol)); // sorry
      // Стерео смещение звука
      //Mem.ReadSingle(pan);
      st.ReadBuffer(pan, sizeof(pan)); // sorry
      // Запускаем звук, если есть
      if (gTriggers[i].Sound <> nil) then
      begin
        gTriggers[i].Sound.PlayPanVolume(pan, vol);
        gTriggers[i].Sound.Pause(True);
        gTriggers[i].Sound.SetPosition(dw);
      end
    end;
    // uservars
    gTriggers[i].userVars.Free();
    gTriggers[i].userVars := nil;
    uvcount := utils.readLongInt(st);
    if (uvcount < 0) or (uvcount > 1024*1024) then raise XStreamError.Create('invalid number of user vars in trigger');
    if (uvcount > 0) then
    begin
      gTriggers[i].userVars := THashStrVariant.Create();
      vv := Unassigned;
      while (uvcount > 0) do
      begin
        Dec(uvcount);
        uvname := utils.readStr(st);
        vt := utils.readLongInt(st);
        case vt of
          varString: begin ustr := utils.readStr(st); vv := ustr; end;
          varBoolean: begin ubool := utils.readBool(st); vv := ubool; end;
          varShortInt: begin uint := utils.readLongInt(st); vv := ShortInt(uint); end;
          varSmallint: begin uint := utils.readLongInt(st); vv := SmallInt(uint); end;
          varInteger: begin uint := utils.readLongInt(st); vv := LongInt(uint); end;
          varByte: begin uint := utils.readLongInt(st); vv := Byte(uint); end;
          varWord: begin uint := utils.readLongInt(st); vv := Word(uint); end;
          varLongWord: begin uint := utils.readLongInt(st); vv := LongWord(uint); end;
          else raise Exception.CreateFmt('cannot load uservar ''%s''', [uvname]);
        end;
        gTriggers[i].userVars.put(uvname, vv);
      end;
    end;
  end;
end;


end.
