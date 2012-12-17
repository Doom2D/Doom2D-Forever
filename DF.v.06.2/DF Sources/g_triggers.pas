unit g_triggers;

interface

uses
  MAPSTRUCT, e_graphics, Windows, MAPDEF, g_basic, g_sound;

type
  TTriggerSaveRec = packed record
   X, Y: Integer;
   Width, Height: Word;
   Enabled: Boolean;
   TexturePanel: Integer;
   TexturePanelType: Word;
   TriggerType: Word;
   ActivateType: Cardinal;
   Keys: Word;
   TimeOut: Word;
   ActivateUID: Word;
   PlayerCollide: Boolean;
   DoorTime: Integer;
   PressTime: Integer;
   PressCount: Integer;
   SoundPlayCount: Integer;
   Data: TTriggerData;
  end;

  TTrigger = record
   X, Y:             Integer;
   Width, Height:    Word;
   Enabled:          Boolean;
   TexturePanel:     Integer;  
   TexturePanelType: Word;
   TriggerType:      Word;
   ActivateType:     Cardinal;
   Keys:             Word;
   TimeOut:          Word;
   ActivateUID:      Word;

   PlayerCollide: Boolean;
   DoorTime: Integer;
   PressTime: Integer;
   PressCount: Integer;
   SoundPlayCount: Integer;
   Sound: TSound;

   flag: Boolean;

   Data: TTriggerData;
  end;

function g_Triggers_Create(Trigger: TTrigger): DWORD;
procedure g_Triggers_Update();
procedure g_Triggers_Press(ID: DWORD);
function g_Triggers_PressR(X, Y: Integer; Width, Height: Word; UID: Word;
                           ActivateType: Cardinal; IgnoreList: DWArray = nil): DWArray;
procedure g_Triggers_PressL(X1, Y1, X2, Y2: Integer; UID: DWORD; ActivateType: Cardinal);
procedure g_Triggers_OpenAll();
procedure g_Triggers_Free();
function g_Triggers_Save(var p: Pointer): Integer;
procedure g_Triggers_Load(p: Pointer; len: Integer);

var
  gTriggers: array of TTrigger;
  gSecretsCount: Integer=0;

implementation

uses
  g_player, g_map, Math, g_gfx, g_game, g_textures, g_console,
  g_monsters, g_items, g_phys, g_weapons, WADEDITOR, g_main, SysUtils,
  e_log;

function FindTrigger(): DWORD;
var
  i: Integer;
begin
 if gTriggers <> nil then
 for i := 0 to High(gTriggers) do
  if gTriggers[i].TriggerType = TRIGGER_NONE then
  begin
   Result := i;
   Exit;
  end;

 if gTriggers = nil then
 begin
  SetLength(gTriggers, 8);
  Result := 0;
 end
  else
 begin
  Result := High(gTriggers) + 1;
  SetLength(gTriggers, Length(gTriggers) + 8);
 end;
end;

function CloseDoor(PanelID: Integer; NoSound: Boolean; d2d: Boolean): Boolean;
var
  a, b, c: Integer;
begin
 Result := False;

 if PanelID = -1 then Exit;

 if not d2d then
 begin
  with gRenderWalls[PanelID] do
  begin
   if g_CollidePlayer(X, Y, Width, Height) or
      g_CollideMonster(X, Y, Width, Height) then Exit;

   if not Enabled then
   begin
    if not NoSound then g_Sound_PlayExAt('SOUND_GAME_DOORCLOSE', 255, X, Y);
    g_Map_EnableWall(PanelID);
    Result := True;
   end;
  end;
 end
  else
 begin
  if gDoorMap = nil then Exit;

  c := -1;
  for a := 0 to High(gDoorMap) do
  begin
   for b := 0 to High(gDoorMap[a]) do
    if gDoorMap[a, b] = DWORD(PanelID) then
    begin
     c := a;
     Break;
    end;

   if c <> -1 then Break;
  end;
  if c = -1 then Exit;

  for b := 0 to High(gDoorMap[c]) do
   with gRenderWalls[gDoorMap[c, b]] do
   begin
    if g_CollidePlayer(X, Y, Width, Height) or
       g_CollideMonster(X, Y, Width, Height) then Exit;
   end;

  if not NoSound then
   for b := 0 to High(gDoorMap[c]) do
    if not gRenderWalls[gDoorMap[c, b]].Enabled then
    begin
     with gRenderWalls[PanelID] do
      g_Sound_PlayExAt('SOUND_GAME_DOORCLOSE', 255, X, Y);
     Break;
    end;

  for b := 0 to High(gDoorMap[c]) do
   if not gRenderWalls[gDoorMap[c, b]].Enabled then
   begin
    g_Map_EnableWall(gDoorMap[c, b]);
    Result := True;
   end;
 end;
end;

procedure CloseTrap(PanelID: Integer; NoSound: Boolean; d2d: Boolean);
var
  a, b, c: Integer;
begin
 if PanelID = -1 then Exit;

 if not d2d then
 begin
  with gRenderWalls[PanelID] do
   if not NoSound then g_Sound_PlayExAt('SOUND_GAME_SWITCH1', 255, X, Y);

  with gRenderWalls[PanelID] do
  begin
   if gPlayers <> nil then
    for a := 0 to High(gPlayers) do
     if (gPlayers[a] <> nil) and gPlayers[a].Live and
        gPlayers[a].Collide(X, Y, Width, Height) then
      gPlayers[a].Damage(1000, 0, 0, 0, HIT_TRAP);

   if gMonsters <> nil then
    for a := 0 to High(gMonsters) do
     if (gMonsters[a] <> nil) and gMonsters[a].Live and
        g_Obj_Collide(X, Y, Width, Height, @gMonsters[a].Obj) then
      gMonsters[a].Damage(1000, 0, 0, 0, HIT_TRAP);

   if not Enabled then g_Map_EnableWall(PanelID);
  end;
 end
  else
 begin
  if gDoorMap = nil then Exit;

  c := -1;
  for a := 0 to High(gDoorMap) do
  begin
   for b := 0 to High(gDoorMap[a]) do
    if gDoorMap[a, b] = DWORD(PanelID) then
    begin
     c := a;
     Break;
    end;

   if c <> -1 then Break;
  end;
  if c = -1 then Exit;

  if not NoSound then
   for b := 0 to High(gDoorMap[c]) do
    if not gRenderWalls[gDoorMap[c, b]].Enabled then
    begin
     with gRenderWalls[PanelID] do
      g_Sound_PlayExAt('SOUND_GAME_SWITCH1', 255, X, Y);
     Break;
    end;

  for b := 0 to High(gDoorMap[c]) do
   with gRenderWalls[gDoorMap[c, b]] do
   begin
    if gPlayers <> nil then
     for a := 0 to High(gPlayers) do
      if (gPlayers[a] <> nil) and gPlayers[a].Live and
         gPlayers[a].Collide(X, Y, Width, Height) then
       gPlayers[a].Damage(1000, 0, 0, 0, HIT_TRAP);

    if gMonsters <> nil then
     for a := 0 to High(gMonsters) do
      if (gMonsters[a] <> nil) and gMonsters[a].Live and
         g_Obj_Collide(X, Y, Width, Height, @gMonsters[a].Obj) then
       gMonsters[a].Damage(1000, 0, 0, 0, HIT_TRAP);

    if not Enabled then g_Map_EnableWall(gDoorMap[c, b]);
   end;
 end;
end;

procedure OpenDoor(PanelID: Integer; NoSound: Boolean; d2d: Boolean);
var
  a, b, c: Integer;
begin
 if PanelID = -1 then Exit;

 if not d2d then
 begin
  with gRenderWalls[PanelID] do
   if Enabled then
   begin
    if not NoSound then g_Sound_PlayExAt('SOUND_GAME_DOOROPEN', 255, X, Y);
    g_Map_DisableWall(PanelID);
   end;
 end
  else
 begin
  if gDoorMap = nil then Exit;

  c := -1;
  for a := 0 to High(gDoorMap) do
  begin
   for b := 0 to High(gDoorMap[a]) do
    if gDoorMap[a, b] = DWORD(PanelID) then
    begin
     c := a;
     Break;
    end;

   if c <> -1 then Break;
  end;
  if c = -1 then Exit;

  if not NoSound then
   for b := 0 to High(gDoorMap[c]) do
    if gRenderWalls[gDoorMap[c, b]].Enabled then
    begin
     with gRenderWalls[PanelID] do
      g_Sound_PlayExAt('SOUND_GAME_DOOROPEN', 255, X, Y);
     Break;
    end;

  for b := 0 to High(gDoorMap[c]) do
   if gRenderWalls[gDoorMap[c, b]].Enabled then g_Map_DisableWall(gDoorMap[c, b]);
 end;
end;

function SetLift(PanelID: Integer; d: Integer; NoSound: Boolean; d2d: Boolean): Boolean;
var
  a, b, c, t: Integer;
begin
 Result := False;

 if PanelID = -1 then Exit;

 case d of
  0: t := 0;
  1: t := 1;
  else t := IfThen(gLifts[PanelID].LiftType = 1, 0, 1);
 end;

 if not d2d then
 begin
  with gLifts[PanelID] do
   if LiftType <> t then
   begin
    g_Map_SetLift(PanelID, t);

    if not NoSound then g_Sound_PlayEx('SOUND_GAME_SWITCH0', 127, 255);
    Result := True;
   end;
 end
  else
 begin
  if gLiftMap = nil then Exit;

  c := -1;
  for a := 0 to High(gLiftMap) do
  begin
   for b := 0 to High(gLiftMap[a]) do
    if gLiftMap[a, b] = DWORD(PanelID) then
    begin
     c := a;
     Break;
    end;

   if c <> -1 then Break;
  end;
  if c = -1 then Exit;

  if not NoSound then
   for b := 0 to High(gLiftMap[c]) do
    if gLifts[gLiftMap[c, b]].LiftType <> t then
    begin
     with gLifts[PanelID] do
      g_Sound_PlayExAt('SOUND_GAME_DOOROPEN', 255, Rect.X, Rect.Y);
     Break;
    end;

  for b := 0 to High(gLiftMap[c]) do
   with gLifts[gLiftMap[c, b]] do
   if LiftType <> t then
   begin
    g_Map_SetLift(gLiftMap[c, b], t);

    Result := True;
   end;
 end;
end;

function ActivateTrigger(var Trigger: TTrigger): Boolean;
var
  animonce: Boolean;
  p: TPlayer;
  m: TMonster;
  i: Integer;
begin
 Result := False;

 if not Trigger.Enabled then Exit;
 if Trigger.TimeOut <> 0 then Exit;

 animonce := False;

 with Trigger do
 begin
  case TriggerType of
   TRIGGER_EXIT:
   begin
    g_Sound_PlayEx('SOUND_GAME_SWITCH0', 127, 255);
    g_Game_ExitLevel(Data.MapName);
    Result := True;
    TimeOut := 18;
    Exit;
   end;

   TRIGGER_TELEPORT:
   begin
    case g_GetUIDType(ActivateUID) of
     UID_PLAYER:
     begin
      p := g_Player_Get(ActivateUID);
      if p = nil then Exit;

      if Data.d2d_teleport then
        begin
          if p.TeleportTo(Data.TargetPoint.X-(p.Obj.Rect.Width div 2),
                          Data.TargetPoint.Y-p.Obj.Rect.Height,
                          Data.silent_teleport,
                          Data.TlpDir) then
            Result := True;
        end
      else
        if p.TeleportTo(Data.TargetPoint.X,
                        Data.TargetPoint.Y,
                        Data.silent_teleport,
                        Data.TlpDir) then
          Result := True;
     end;

     UID_MONSTER:
     begin
      m := g_Monsters_Get(ActivateUID);
      if m = nil then Exit;

      if Data.d2d_teleport then
        begin
          if m.TeleportTo(Data.TargetPoint.X-(m.Obj.Rect.Width div 2),
                          Data.TargetPoint.Y-m.Obj.Rect.Height,
                          Data.silent_teleport,
                          Data.TlpDir) then
            Result := True;
        end
      else
        if m.TeleportTo(Data.TargetPoint.X,
                        Data.TargetPoint.Y,
                        Data.silent_teleport,
                        Data.TlpDir) then
          Result := True;
     end;
    end;

    TimeOut := 0;
   end;

   TRIGGER_OPENDOOR:
   begin
    Result := gRenderWalls[Data.PanelID].Enabled;
    OpenDoor(Data.PanelID, Data.NoSound, Data.d2d_doors);
    TimeOut := 0;
   end;

   TRIGGER_CLOSEDOOR:
   begin
    Result := CloseDoor(Data.PanelID, Data.NoSound, Data.d2d_doors);
    if Result then TimeOut := 0;
   end;

   TRIGGER_DOOR, TRIGGER_DOOR5:
   begin
    if Data.PanelID <> -1 then
    begin
     if gRenderWalls[Data.PanelID].Enabled then
     begin
      OpenDoor(Data.PanelID, Data.NoSound, Data.d2d_doors);
      if TriggerType = TRIGGER_DOOR5 then DoorTime := 180;
      Result := True;
     end else Result := CloseDoor(Data.PanelID, Data.NoSound, Data.d2d_doors);

     if Result then TimeOut := 18;
    end;
   end;

   TRIGGER_CLOSETRAP, TRIGGER_TRAP:
   begin
    CloseTrap(Data.PanelID, Data.NoSound, Data.d2d_doors);
    if TriggerType = TRIGGER_TRAP then DoorTime := 40;
    Result := True;
    TimeOut := DoorTime+36;
   end;

   TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF:
   begin
    PressCount := PressCount+1;
    if PressTime = -1 then PressTime := Data.Wait;
    TimeOut := 18;
    Result := True;
   end;

   TRIGGER_SECRET:
   if g_GetUIDType(ActivateUID) = UID_PLAYER then
   begin
    g_Player_Get(ActivateUID).GetSecret();
    Enabled := False;
    g_Mark(X, Y, Width, Height, MARK_FREE);
    Result := True;
   end;

   TRIGGER_LIFTUP: Result := SetLift(Data.PanelID, 0, Data.NoSound, Data.d2d_doors);
   TRIGGER_LIFTDOWN: Result := SetLift(Data.PanelID, 1, Data.NoSound, Data.d2d_doors);

   TRIGGER_LIFT:
   begin
    Result := SetLift(Data.PanelID, 3, Data.NoSound, Data.d2d_doors);
    if Result then TimeOut := 18;
   end;

   TRIGGER_TEXTURE:
   begin
    if ByteBool(Data.ActivateOnce) then
    begin
     TriggerType := TRIGGER_NONE;
     g_Mark(X, Y, Width, Height, MARK_FREE);
    end else TimeOut := 6;
    animonce := Data.AnimOnce;

    Result := True;
   end;

   TRIGGER_SOUND:
   begin
    if Sound <> nil then
    begin
     if Data.SoundSwitch and Sound.IsPlaying() then
     begin
      Sound.Stop();
      SoundPlayCount := 0;
      Result := True;
     end
      else
     if not ((Data.PlayCount=0) and Sound.IsPlaying()) then
     begin
      if Data.PlayCount > 0 then SoundPlayCount := Data.PlayCount
       else SoundPlayCount := 1;
      Result := True;
     end;
    end;
   end;

   TRIGGER_SPAWNMONSTER:
     if (Data.MonType in [MONSTER_DEMON..MONSTER_MAN]) then
       begin
         i := g_Monsters_Create(Data.MonType,
                Data.MonPos.X, Data.MonPos.Y,
                TDirection(Data.MonDir), True);
         if (Data.MonHealth > 0) then
           gMonsters[i].SetHealth(Data.MonHealth);
         TimeOut := 18;
         Result := True;
       end;

   TRIGGER_SPAWNITEM:
     if (Data.ItemType in [ITEM_MEDKIT_SMALL..ITEM_KEY_BLUE]) then
       begin
         g_Items_Create(Data.ItemPos.X, Data.ItemPos.Y,
           Data.ItemType, Data.ItemFalls, False, True);
         TimeOut := 18;
         Result := True;
       end;

   TRIGGER_MUSIC:
     begin
       g_Game_PlayMusic(Trigger.Data.MusicName);
       TimeOut := 36;
       Result := True;
     end;
  end;
 end;
 
 if Result and (Trigger.TexturePanel <> -1) then
  g_Map_SwitchTexture(Trigger.TexturePanelType, Trigger.TexturePanel, IfThen(animonce, 2, 1));
end;

function g_Triggers_Create(Trigger: TTrigger): DWORD;
var
  find_id, id: DWORD;
  fn, mapw: string;
begin
 Result := DWORD(-1);

 if (Trigger.TriggerType = TRIGGER_EXIT) and not
    LongBool(gGameSettings.Options and GAME_OPTION_ALLOWEXIT) then Exit;

 if (Trigger.TriggerType = TRIGGER_SPAWNMONSTER) and
    (not LongBool(gGameSettings.Options and GAME_OPTION_MONSTERDM)) and
    (gGameSettings.GameType <> GT_SINGLE) then Exit;

 if Trigger.TriggerType = TRIGGER_SECRET then gSecretsCount := gSecretsCount+1;

 find_id := FindTrigger();
 gTriggers[find_id] := Trigger;

 with gTriggers[find_id] do
 begin
  TimeOut := 0;
  DoorTime := -1;
  PressTime := -1;
  PressCount := 0;
  SoundPlayCount := 0;
  PlayerCollide := False;
  ActivateUID := 0;
  Sound := nil;
 end;

 if Trigger.TriggerType = TRIGGER_SOUND then
 begin
  if not g_Sound_Exists(Trigger.Data.SoundName) then
  begin
   g_ProcessResourceStr(Trigger.Data.SoundName, @fn, nil, nil);
   if fn = '' then
   begin
    g_ProcessResourceStr(gMapInfo.Map, @mapw, nil, nil);
    fn := mapw+Trigger.Data.SoundName;
   end else fn := GameDir+'\wads\'+Trigger.Data.SoundName;

   if not g_Sound_CreateWADEx(Trigger.Data.SoundName, fn) then
     g_Console_Add(Format('! Error loading sound %s for trigger', [fn]))
  end;

  if g_Sound_Get(id, Trigger.Data.SoundName) then
   with gTriggers[find_id] do
   begin
    Sound := TSound.Create();
    Sound.SetID(id);
   end;
 end;

 if Trigger.TriggerType = TRIGGER_MUSIC then
   begin
     if not g_Music_Exists(Trigger.Data.MusicName) then
       begin
         g_ProcessResourceStr(Trigger.Data.MusicName, @fn, nil, nil);
         if fn = '' then
           begin
             g_ProcessResourceStr(gMapInfo.Map, @mapw, nil, nil);
             fn := mapw + Trigger.Data.MusicName;
           end
         else
           fn := GameDir+'\wads\'+Trigger.Data.MusicName;

         if not g_Music_CreateWADEx(Trigger.Data.MusicName, fn) then
           g_Console_Add(Format('! Error loading music %s for trigger', [fn]));
       end;
   end;

 Result := find_id;
end;

procedure g_Triggers_Update();
var
  a, b: Integer;
begin
 if gTriggers = nil then Exit;

 for a := 0 to High(gTriggers) do
  with gTriggers[a] do
   if (TriggerType <> TRIGGER_NONE) and Enabled then
   begin
    if DoorTime > 0 then DoorTime := DoorTime-1;
    if PressTime > 0 then PressTime := PressTime-1;

    if (TriggerType = TRIGGER_SOUND) and (Sound <> nil) then
     if (SoundPlayCount > 0) and (not Sound.IsPlaying()) then
     begin
      if Data.PlayCount <> 0 then SoundPlayCount := SoundPlayCount-1;
      if Data.Local then Sound.Play(X+(Width div 2), Y+(Height div 2), Data.Volume)
       else Sound.Play(Data.Pan, Data.Volume);
     end;

    if (TriggerType = TRIGGER_TRAP) and (DoorTime = 0) and (Data.PanelID <> -1) then
    begin
     OpenDoor(Data.PanelID, Data.NoSound, Data.d2d_doors);
     DoorTime := -1;
    end;

    if (TriggerType = TRIGGER_DOOR5) and (DoorTime = 0) and (Data.PanelID <> -1) then
    begin
     if gRenderWalls[Data.PanelID].Enabled then DoorTime := -1
      else if CloseDoor(Data.PanelID, Data.NoSound, Data.d2d_doors) then DoorTime := -1;
    end;

    if (TriggerType in [TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF]) and
       (PressTime = 0) and (PressCount >= Data.Count) then
    begin
     for b := 0 to High(gTriggers) do
      if g_Collide(Data.tX, Data.tY, Data.tWidth, Data.tHeight, gTriggers[b].X, gTriggers[b].Y,
                   gTriggers[b].Width, gTriggers[b].Height) and
         ((b <> a) or (Data.Wait > 0)) then
      begin { Can be self-activated, if there is Data.Wait }                    
       case TriggerType of
        TRIGGER_PRESS:
         begin
          gTriggers[b].ActivateUID := gTriggers[a].ActivateUID;
          ActivateTrigger(gTriggers[b]);
          //if (not gTriggers[b].Enabled) or ActivateTrigger(gTriggers[b]) then
          if a <> b then PressTime := -1;
         end;
        TRIGGER_ON:
         begin
          gTriggers[b].Enabled := True;
          PressTime := -1;
         end;
        TRIGGER_OFF:
         begin
          gTriggers[b].Enabled := False;
          PressTime := -1;
         end;
        TRIGGER_ONOFF:
        begin
         gTriggers[b].Enabled := not gTriggers[b].Enabled;
         PressTime := -1;
        end;
       end;
      end;

     PressCount := 0;
     
     if PressTime = -1 then Continue;
     { TODO 5 : возможен бесконечный цикл }
    end;

    if TimeOut > 0 then
    begin
     TimeOut := TimeOut-1;
     Continue;
    end;

    if LongBool(ActivateType and ACTIVATE_PLAYERCOLLIDE) and
       (TimeOut = 0) then
      if gPlayers <> nil then
        for b := 0 to High(gPlayers) do
          if gPlayers[b] <> nil then
            with gPlayers[b] do
              if ((gTriggers[a].Keys and GetKeys) = gTriggers[a].Keys) and
                 Live and Collide(X, Y, Width, Height) then
                begin
                  gTriggers[a].ActivateUID := UID;

                  if (gTriggers[a].TriggerType in [TRIGGER_SOUND, TRIGGER_MUSIC]) and
                     PlayerCollide then
                    { Don't activate sound/music again if player is here }
                  else
                    ActivateTrigger(gTriggers[a]);
                end;

    { TODO 5 : активация монстрами триггеров с ключами }

    if LongBool(ActivateType and ACTIVATE_MONSTERCOLLIDE) and
       (TimeOut = 0) and (Keys = 0) then
      if gMonsters <> nil then
        for b := 0 to High(gMonsters) do
          if (gMonsters[b] <> nil) and (gMonsters[b].Live) then
            with gMonsters[b] do
              if Collide(X, Y, Width, Height) then
                begin
                  flag := True; { What is this? }
                  gTriggers[a].ActivateUID := UID;
                  ActivateTrigger(gTriggers[a]);
                end;

    if LongBool(ActivateType and ACTIVATE_NOMONSTER) and
       (TimeOut = 0) and (Keys = 0) then
      if not g_CollideMonster(X, Y, Width, Height) then
        begin
          gTriggers[a].ActivateUID := 0;
          ActivateTrigger(gTriggers[a]);
        end;

    PlayerCollide := g_CollidePlayer(X, Y, Width, Height);
    if LongBool(ActivateType and ACTIVATE_NOPLAYER) and
       (not PlayerCollide) and (TimeOut = 0) and (Keys = 0) then
      begin
        gTriggers[a].ActivateUID := 0;
        ActivateTrigger(gTriggers[a]);
      end;

    if LongBool(ActivateType and ACTIVATE_ITEMCOLLIDE) and
       (TimeOut = 0) and (Keys = 0) then
      if gItems <> nil then
        for b := 0 to High(gItems) do
          if (gItems[b].Live) then
            if g_Obj_Collide(X, Y, Width, Height, @gItems[b].Obj) then
              begin
                gTriggers[a].ActivateUID := 0;
                ActivateTrigger(gTriggers[a]);
              end;

    if LongBool(ActivateType and ACTIVATE_NOITEM) and
       (TimeOut = 0) and (Keys = 0) then
      if not g_CollideItem(X, Y, Width, Height) then
        begin
          gTriggers[a].ActivateUID := 0;
          ActivateTrigger(gTriggers[a]);
        end;
   end;
end;

procedure g_Triggers_Press(ID: DWORD);
begin
  gTriggers[ID].ActivateUID := 0;
  ActivateTrigger(gTriggers[ID]);
end;

function g_Triggers_PressR(X, Y: Integer; Width, Height: Word; UID: Word;
                           ActivateType: Cardinal; IgnoreList: DWArray = nil): DWArray;
var
  a: Integer;
  k: Word;
begin
 if gTriggers = nil then Exit;

 case g_GetUIDType(UID) of
  UID_GAME: k := 255;
  UID_PLAYER: k := g_Player_Get(UID).GetKeys;
  else k := 0;
 end;

 Result := nil;

 for a := 0 to High(gTriggers) do
  if (gTriggers[a].TriggerType <> TRIGGER_NONE) and
     (gTriggers[a].TimeOut = 0) and
     (not InDWArray(a, IgnoreList)) and
     ((gTriggers[a].Keys and k) = gTriggers[a].Keys) and
     LongBool(gTriggers[a].ActivateType and ActivateType) then
   if g_Collide(X, Y, Width, Height, gTriggers[a].X, gTriggers[a].Y, gTriggers[a].Width,
                gTriggers[a].Height) then
   begin
    gTriggers[a].ActivateUID := UID;
    if ActivateTrigger(gTriggers[a]) then
    begin
     SetLength(Result, Length(Result)+1);
     Result[High(Result)] := a;
    end;
   end;
end;

procedure g_Triggers_PressL(X1, Y1, X2, Y2: Integer; UID: DWORD; ActivateType: Cardinal);
var
  a: Integer;
  k: Word;
begin
 if gTriggers = nil then Exit;

 case g_GetUIDType(UID) of
  UID_GAME: k := 255;
  UID_PLAYER: k := g_Player_Get(UID).GetKeys;
  else k := 0;
 end;

 for a := 0 to High(gTriggers) do
  if (gTriggers[a].TriggerType <> TRIGGER_NONE) and
     (gTriggers[a].TimeOut = 0) and
     ((gTriggers[a].Keys and k) = gTriggers[a].Keys) and
     LongBool(gTriggers[a].ActivateType and ActivateType) then
   if g_CollideLine(x1, y1, x2, y2, gTriggers[a].X, gTriggers[a].Y, gTriggers[a].Width,
                    gTriggers[a].Height) then
   begin
    gTriggers[a].ActivateUID := UID;
    ActivateTrigger(gTriggers[a]);
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
  with gTriggers[a] do
   if (TriggerType = TRIGGER_OPENDOOR) or
      (TriggerType = TRIGGER_DOOR5) or
      (TriggerType = TRIGGER_DOOR) then
   begin
    OpenDoor(Data.PanelID, True, Data.d2d_doors);
    if TriggerType = TRIGGER_DOOR5 then DoorTime := 180;
    b := True;
   end;

 if b then g_Sound_PlayEx('SOUND_GAME_DOOROPEN', 127, 255);
end;

procedure g_Triggers_Free();
var
  a: Integer;
begin
 if gTriggers <> nil then
  for a := 0 to High(gTriggers) do
   if gTriggers[a].TriggerType = TRIGGER_SOUND then
   begin
    if g_Sound_Exists(gTriggers[a].Data.SoundName) then
     g_Sound_Delete(gTriggers[a].Data.SoundName);

    if gTriggers[a].Sound <> nil then gTriggers[a].Sound.Destroy();
   end;

 gTriggers := nil;
 gSecretsCount := 0;
end;

function g_Triggers_Save(var p: Pointer): Integer;
var
  a, b, i: Integer;
  trigger: TTriggerSaveRec;
begin
 Result := 0;
 b := 0;
 if gTriggers <> nil then
  for i := 0 to High(gTriggers) do
   {if gTriggers[i].TriggerType <> TRIGGER_NONE then} b := b+1;

 if b = 0 then Exit;

 p := GetMemory(SizeOf(TTriggerSaveRec)*b);
 a := 0;

 for i := 0 to High(gTriggers) do
  {if gTriggers[i].TriggerType <> TRIGGER_NONE then}
  begin
   trigger.X := gTriggers[i].X;
   trigger.Y := gTriggers[i].Y;
   trigger.Width := gTriggers[i].Width;
   trigger.Height := gTriggers[i].Height;
   trigger.Enabled := gTriggers[i].Enabled;
   trigger.TexturePanel := gTriggers[i].TexturePanel;
   trigger.TexturePanelType := gTriggers[i].TexturePanelType;
   trigger.TriggerType := gTriggers[i].TriggerType;
   trigger.ActivateType := gTriggers[i].ActivateType;
   trigger.Keys := gTriggers[i].Keys;
   trigger.TimeOut := gTriggers[i].TimeOut;
   trigger.ActivateUID := gTriggers[i].ActivateUID;
   trigger.PlayerCollide := gTriggers[i].PlayerCollide;
   trigger.DoorTime := gTriggers[i].DoorTime;
   trigger.PressTime := gTriggers[i].PressTime;
   trigger.PressCount := gTriggers[i].PressCount;
   trigger.SoundPlayCount := gTriggers[i].SoundPlayCount;
   trigger.Data := gTriggers[i].Data;

   CopyMemory(Pointer(Integer(p)+a*SizeOf(TTriggerSaveRec)), @trigger, SizeOf(TTriggerSaveRec));
   a := a+1;
  end;

 Result := SizeOf(TTriggerSaveRec)*b;
end;

procedure g_Triggers_Load(p: Pointer; len: Integer);
var
  a, b, c: Integer;
  trigger: TTriggerSaveRec;
  trig: TTrigger;
begin
 g_Triggers_Free();

 c := len div SizeOf(TTriggerSaveRec);
 if c = 0 then Exit;

 for a := 0 to c-1 do
 begin
  CopyMemory(@trigger, Pointer(Integer(p)+a*SizeOf(TTriggerSaveRec)), SizeOf(TTriggerSaveRec));
  trig.TriggerType := trigger.TriggerType;
  trig.Data := trigger.Data;
  b := g_Triggers_Create(trig);

  gTriggers[b].X := trigger.X;
  gTriggers[b].Y := trigger.Y;
  gTriggers[b].Width := trigger.Width;
  gTriggers[b].Height := trigger.Height;
  gTriggers[b].Enabled := trigger.Enabled;
  gTriggers[b].TexturePanel := trigger.TexturePanel;
  gTriggers[b].TexturePanelType := trigger.TexturePanelType;
  gTriggers[b].ActivateType := trigger.ActivateType;
  gTriggers[b].Keys := trigger.Keys;
  gTriggers[b].TimeOut := trigger.TimeOut;
  gTriggers[b].ActivateUID := trigger.ActivateUID;
  gTriggers[b].PlayerCollide := trigger.PlayerCollide;
  gTriggers[b].DoorTime := trigger.DoorTime;
  gTriggers[b].PressTime := trigger.PressTime;
  gTriggers[b].PressCount := trigger.PressCount;
  gTriggers[b].SoundPlayCount := trigger.SoundPlayCount;
 end;
end;

end.
