unit g_netmsg;

interface

uses g_net, g_triggers, Classes, SysUtils, md5asm;

const
  NET_MSG_INFO   = 100;

  NET_MSG_CHAT   = 101;
  NET_MSG_SND    = 102;
  NET_MSG_GFX    = 103;
  NET_MSG_GEVENT = 104;
  NET_MSG_SCORE  = 105;
  NET_MSG_COOP   = 106;
  NET_MSG_FLAG   = 107;
  NET_MSG_REQFST = 108;
  NET_MSG_GSET   = 109;

  NET_MSG_PLR    = 111;
  NET_MSG_PLRPOS = 112;
  NET_MSG_PLRSTA = 113;
  NET_MSG_PLRDEL = 114;
  NET_MSG_PLRDMG = 115;
  NET_MSG_PLRDIE = 116;
  NET_MSG_PLRFIRE= 117;
  NET_MSG_PLRSET = 119;
  NET_MSG_CHEAT  = 120;

  NET_MSG_ISPAWN = 121;
  NET_MSG_IDEL   = 122;

  NET_MSG_MSPAWN = 131;
  NET_MSG_MPOS   = 132;
  NET_MSG_MSTATE = 133;
  NET_MSG_MSHOT  = 134;
  NET_MSG_MDEL   = 135;

  NET_MSG_PSTATE = 141;
  NET_MSG_PTEX   = 142;

  NET_MSG_TSOUND = 151;
  NET_MSG_TMUSIC = 152;

  NET_MSG_SHDEL  = 161;
  NET_MSG_SHADD  = 162;
  NET_MSG_SHPOS  = 163;

  NET_MSG_RCON_AUTH  = 191;
  NET_MSG_RCON_CMD   = 192;
  NET_MSG_VOTE_EVENT = 195;

  NET_MSG_MAP_REQUEST = 201;
  NET_MSG_MAP_RESPONSE = 202;
  NET_MSG_RES_REQUEST = 203;
  NET_MSG_RES_RESPONSE = 204;

  NET_GFX_SPARK   = 1;
  NET_GFX_SPAWN   = 2;
  NET_GFX_TELE    = 3;
  NET_GFX_BFG     = 4;
  NET_GFX_FIRE    = 5;
  NET_GFX_RESPAWN = 6;

  NET_EV_MAPSTART     = 1;
  NET_EV_MAPEND       = 2;
  NET_EV_LMS_LOSE     = 3;
  NET_EV_LMS_WIN      = 4;
  NET_EV_TLMS_WIN     = 5;
  NET_EV_LMS_START    = 6;
  NET_EV_LMS_DRAW     = 7;
  NET_EV_LMS_SURVIVOR = 8;
  NET_EV_SCORE_ADD    = 9;
  NET_EV_SCORE_SUB    = 10;
  NET_EV_BIGTEXT      = 11;

  NET_VE_STARTED      = 1;
  NET_VE_PASSED       = 2;
  NET_VE_FAILED       = 3;
  NET_VE_VOTE         = 4;
  NET_VE_REVOKE       = 5;
  NET_VE_INPROGRESS   = 6;

  NET_FLAG_GET    = 1;
  NET_FLAG_DROP   = 2;
  NET_FLAG_CAP    = 3;
  NET_FLAG_RETURN = 4;

  NET_CHEAT_SUICIDE  = 1;
  NET_CHEAT_SPECTATE = 2;

// HOST MESSAGES

procedure MH_RECV_Info(C: pTNetClient; P: Pointer);
procedure MH_RECV_Chat(C: pTNetClient; P: Pointer);
procedure MH_RECV_FullStateRequest(C: pTNetClient; P: Pointer);
function  MH_RECV_PlayerPos(C: pTNetClient; P: Pointer): Word;
procedure MH_RECV_PlayerSettings(C: pTNetClient; P: Pointer);
procedure MH_RECV_CheatRequest(C: pTNetClient; P: Pointer);
procedure MH_RECV_RCONPassword(C: pTNetClient; P: Pointer);
procedure MH_RECV_RCONCommand(C: pTNetClient; P: Pointer);
procedure MH_RECV_MapRequest(C: pTNetClient; P: Pointer);
procedure MH_RECV_ResRequest(C: pTNetClient; P: Pointer);
procedure MH_RECV_Vote(C: pTNetClient; P: Pointer);

// GAME
procedure MH_SEND_Everything(CreatePlayers: Boolean = False; ID: Integer = NET_EVERYONE);
procedure MH_SEND_Info(ID: Byte);
procedure MH_SEND_Chat(Txt: string; ID: Integer = NET_EVERYONE);
procedure MH_SEND_Effect(X, Y: Integer; Ang: SmallInt; Kind: Byte; ID: Integer = NET_EVERYONE);
procedure MH_SEND_Sound(X, Y: Integer; Name: string; ID: Integer = NET_EVERYONE);
procedure MH_SEND_CreateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
procedure MH_SEND_UpdateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
procedure MH_SEND_DeleteShot(Proj: LongInt; X, Y: LongInt; Loud: Boolean = True; ID: Integer = NET_EVERYONE);
procedure MH_SEND_GameStats(ID: Integer = NET_EVERYONE);
procedure MH_SEND_CoopStats(ID: Integer = NET_EVERYONE);
procedure MH_SEND_GameEvent(EvType: Byte; EvParm: string = 'N'; ID: Integer = NET_EVERYONE);
procedure MH_SEND_FlagEvent(Evtype: Byte; Flag: Byte; PID: Word; Quiet: Boolean = False; ID: Integer = NET_EVERYONE);
procedure MH_SEND_GameSettings(ID: Integer = NET_EVERYONE);
// PLAYER
procedure MH_SEND_PlayerCreate(PID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerPos(Reliable: Boolean; PID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerStats(PID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerDelete(PID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerDamage(PID: Word; Kind: Byte; Attacker, Value: Word; VX, VY: Integer; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerFire(PID: Word; Weapon: Byte; X, Y, AX, AY: Integer; ShotID: Integer = -1; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerDeath(PID: Word; KillType, DeathType: Byte; Attacker: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PlayerSettings(PID: Word; Mdl: string = ''; ID: Integer = NET_EVERYONE);
// ITEM
procedure MH_SEND_ItemSpawn(Quiet: Boolean; IID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_ItemDestroy(Quiet: Boolean; IID: Word; ID: Integer = NET_EVERYONE);
// PANEL
procedure MH_SEND_PanelTexture(PType: Word; PID: LongWord; AnimLoop: Byte; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PanelState(PType: Word; PID: LongWord; ID: Integer = NET_EVERYONE);
// MONSTER
procedure MH_SEND_MonsterSpawn(UID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_MonsterPos(UID: Word; ID: Integer = NET_EVERYONE);
procedure MH_SEND_MonsterState(UID: Word; ForcedAnim: Byte = 255; ID: Integer = NET_EVERYONE);
procedure MH_SEND_MonsterShot(UID: Word; X, Y, VX, VY: Integer; ID: Integer = NET_EVERYONE);
procedure MH_SEND_MonsterDelete(UID: Word; ID: Integer = NET_EVERYONE);
// TRIGGER
procedure MH_SEND_TriggerSound(var T: TTrigger; ID: Integer = NET_EVERYONE);
procedure MH_SEND_TriggerMusic(ID: Integer = NET_EVERYONE);
// MISC
procedure MH_SEND_VoteEvent(EvType: Byte;
                            StrArg1: string = 'a'; StrArg2: string = 'b';
                            IntArg1: SmallInt = 0; IntArg2: SmallInt = 0;
                            ID: Integer = NET_EVERYONE);

// CLIENT MESSAGES //

// GAME
procedure MC_RECV_Chat(P: Pointer);
procedure MC_RECV_Effect(P: Pointer);
procedure MC_RECV_Sound(P: Pointer);
procedure MC_RECV_GameStats(P: Pointer);
procedure MC_RECV_CoopStats(P: Pointer);
procedure MC_RECV_GameEvent(P: Pointer);
procedure MC_RECV_FlagEvent(P: Pointer);
procedure MC_RECV_GameSettings(P: Pointer);
// PLAYER
function  MC_RECV_PlayerCreate(P: Pointer): Word;
function  MC_RECV_PlayerPos(P: Pointer): Word;
function  MC_RECV_PlayerStats(P: Pointer): Word;
function  MC_RECV_PlayerDelete(P: Pointer): Word;
function  MC_RECV_PlayerDamage(P: Pointer): Word;
function  MC_RECV_PlayerDeath(P: Pointer): Word;
function  MC_RECV_PlayerFire(P: Pointer): Word;
procedure MC_RECV_PlayerSettings(P: Pointer);
// ITEM
procedure MC_RECV_ItemSpawn(P: Pointer);
procedure MC_RECV_ItemDestroy(P: Pointer);
// PANEL
procedure MC_RECV_PanelTexture(P: Pointer);
procedure MC_RECV_PanelState(P: Pointer);
// MONSTER
procedure MC_RECV_MonsterSpawn(P: Pointer);
procedure MC_RECV_MonsterPos(P: Pointer);
procedure MC_RECV_MonsterState(P: Pointer);
procedure MC_RECV_MonsterShot(P: Pointer);
procedure MC_RECV_MonsterDelete(P: Pointer);
// SHOT
procedure MC_RECV_CreateShot(P: Pointer);
procedure MC_RECV_UpdateShot(P: Pointer);
procedure MC_RECV_DeleteShot(P: Pointer);
// TRIGGER
procedure MC_RECV_TriggerSound(P: Pointer);
procedure MC_RECV_TriggerMusic(P: Pointer);
// VOTE
procedure MC_RECV_VoteEvent(P: Pointer);
// SERVICE
procedure MC_SEND_Info(Password: string = 'ASS');
procedure MC_SEND_Chat(Txt: string);
procedure MC_SEND_PlayerPos();
procedure MC_SEND_FullStateRequest();
procedure MC_SEND_PlayerSettings();
procedure MC_SEND_CheatRequest(Kind: Byte);
procedure MC_SEND_RCONPassword(Password: string = 'ASS');
procedure MC_SEND_RCONCommand(Cmd: string);
procedure MC_SEND_Vote(Start: Boolean = False; Command: string = 'a');
// DOWNLOAD
procedure MC_SEND_MapRequest();
procedure MC_SEND_ResRequest(const resName: AnsiString);

type
  TExternalResourceInfo = record
    Name: string[255];
    md5: TMD5Digest;
  end;

  TResDataMsg = record
    MsgId: Byte;
    FileSize: Integer;
    FileData: AByte;
  end;

  TMapDataMsg = record
    MsgId: Byte;
    FileSize: Integer;
    FileData: AByte;
    ExternalResources: array of TExternalResourceInfo;
  end;

function MapDataFromMsgStream(msgStream: TMemoryStream):TMapDataMsg;
function ResDataFromMsgStream(msgStream: TMemoryStream):TResDataMsg;

implementation

uses
  Windows, Math, ENet, e_input, e_fixedbuffer, e_graphics, e_log,
  g_textures, g_gfx, g_sound, g_console, g_basic, g_options, g_main,
  g_game, g_player, g_map, g_panel, g_items, g_weapons, g_phys, g_gui,
  g_language, g_monsters, g_netmaster,
  WADEDITOR, MAPDEF;

const
  NET_KEY_LEFT  = 1;
  NET_KEY_RIGHT = 2;
  NET_KEY_UP    = 4;
  NET_KEY_DOWN  = 8;
  NET_KEY_JUMP  = 16;
  NET_KEY_FIRE  = 32;
  NET_KEY_OPEN  = 64;
  NET_KEY_NW    = 256;
  NET_KEY_PW    = 512;
  NET_KEY_CHAT  = 2048;

var
  kBytePrev: Word = 0;
  kDirPrev: TDirection = D_LEFT;

// HOST MESSAGES //


// GAME

procedure MH_RECV_Chat(C: pTNetClient; P: Pointer);
var
  Txt: string;
  PID: Word;
  Pl: TPlayer;
begin
  PID := C^.Player;
  Pl := g_Player_Get(PID);

  Txt := e_Raw_Read_String(P);
  if Pl = nil then
    MH_SEND_Chat(Txt)
  else
    MH_SEND_Chat(Pl.Name + ': ' + Txt);
end;

procedure MH_RECV_Info(C: pTNetClient; P: Pointer);
var
  Ver, PName, Model, Pw: string;
  R, G, B, T: Byte;
  PID: Word;
  Color: TRGB;
  I: Integer;
begin
  Ver := e_Raw_Read_String(P);
  Pw := e_Raw_Read_String(P);
  PName := e_Raw_Read_String(P);
  Model := e_Raw_Read_String(P);
  R := e_Raw_Read_Byte(P);
  G := e_Raw_Read_Byte(P);
  B := e_Raw_Read_Byte(P);
  T := e_Raw_Read_Byte(P);

  if Ver <> GAME_VERSION then
  begin
    g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
      _lc[I_NET_DISC_VERSION]);
    enet_peer_disconnect(C^.Peer, NET_DISC_VERSION);
    Exit;
  end;

  if NetPassword <> '' then
    if AnsiLowerCase(NetPassword) <> AnsiLowerCase(Pw) then
    begin
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
        _lc[I_NET_DISC_PASSWORD]);
      enet_peer_disconnect(C^.Peer, NET_DISC_PASSWORD);
      Exit;
    end;

  if not (gGameSettings.GameMode in [GM_TDM, GM_CTF]) then T := TEAM_NONE;

  Color.R := R;
  Color.B := B;
  Color.G := G;

  PID := g_Player_Create(Model, Color, T, False, 0);
  with g_Player_Get(PID) do
  begin
    Name := PName;
    Reset(True);
    if gGameSettings.GameMode = GM_COOP then
      Team := TEAM_COOP;
  end;

  C^.Player := PID;

  g_Console_Add(Format(_lc[I_PLAYER_JOIN], [PName]), True);
  e_WriteLog('NET: Client ' + PName + ' [' + IntToStr(C^.ID) +
             '] connected. Assigned player #' + IntToStr(PID) + '.', MSG_NOTIFY);

  MH_SEND_Info(C^.ID);

  with g_Player_Get(PID) do
  begin
    Name := PName;
    FClientID := C^.ID;
    // round in progress, don't spawn
    if (gGameSettings.MaxLives > 0) and (not gLMSRespawn) then
    begin
      Lives := 0;
      FNoRespawn := True;
      Spectate;
      FWantsInGame := True; // TODO: look into this later
    end
    else
      Respawn(gGameSettings.GameType = GT_SINGLE);
  end;

  for I := Low(NetClients) to High(NetClients) do
  begin
    if NetClients[I].ID = C^.ID then Continue;
    MH_SEND_PlayerCreate(PID, NetClients[I].ID);
    MH_SEND_PlayerPos(True, PID, NetClients[I].ID);
    MH_SEND_PlayerStats(PID, NetClients[I].ID);
  end;

  if gState in [STATE_INTERCUSTOM, STATE_FOLD] then
    MH_SEND_GameEvent(NET_EV_MAPEND, 'N', C^.ID);

  if NetUseMaster then g_Net_Slist_Update;
end;

procedure MH_RECV_FullStateRequest(C: pTNetClient; P: Pointer);
begin
  if gGameOn then
    MH_SEND_Everything((C^.State = NET_STATE_AUTH), C^.ID)
  else
    C^.RequestedFullUpdate := True;
end;

// PLAYER

function  MH_RECV_PlayerPos(C: pTNetClient; P: Pointer): Word;
var
  PID: Word;
  kByte: Word;
  Pl: TPlayer;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := C^.Player;
  Pl := g_Player_Get(PID);
  if Pl = nil then
    Exit;

  with Pl do
  begin
    kByte := e_Raw_Read_Word(P);
    SetDirection(TDirection(e_Raw_Read_Byte(P)));
    ReleaseKeys;

    if kByte = NET_KEY_CHAT then
    begin
      PressKey(KEY_CHAT, 10000);
      Exit;
    end;

    if LongBool(kByte and NET_KEY_LEFT) then PressKey(KEY_LEFT, 10000);
    if LongBool(kByte and NET_KEY_RIGHT) then PressKey(KEY_RIGHT, 10000);
    if LongBool(kByte and NET_KEY_UP) then PressKey(KEY_UP, 10000);
    if LongBool(kByte and NET_KEY_DOWN) then PressKey(KEY_DOWN, 10000);
    if LongBool(kByte and NET_KEY_JUMP) then PressKey(KEY_JUMP, 10000);
    if LongBool(kByte and NET_KEY_FIRE) then PressKey(KEY_FIRE, 10000);
    if LongBool(kByte and NET_KEY_OPEN) then PressKey(KEY_OPEN, 10000);
    if LongBool(kByte and NET_KEY_NW) then PressKey(KEY_NEXTWEAPON, 10000);
    if LongBool(kByte and NET_KEY_PW) then PressKey(KEY_PREVWEAPON, 10000);
  end;

  // MH_SEND_PlayerPos(False, PID, C^.ID);
end;

procedure MH_RECV_CheatRequest(C: pTNetClient; P: Pointer);
var
  CheatKind: Byte;
  Pl: TPlayer;
begin
  Pl := g_Player_Get(C^.Player);
  if Pl = nil then Exit;

  CheatKind := e_Raw_Read_Byte(P);

  case CheatKind of
    NET_CHEAT_SUICIDE:
      Pl.Damage(SUICIDE_DAMAGE, Pl.UID, 0, 0, HIT_SELF);
    NET_CHEAT_SPECTATE:
    begin
      if Pl.FSpectator then
        Pl.Respawn(False)
      else
        Pl.Spectate;
    end;
  end;
end;

procedure MH_RECV_PlayerSettings(C: pTNetClient; P: Pointer);
var
  TmpName: string;
  TmpModel: string;
  TmpColor: TRGB;
  TmpTeam: Byte;
  Pl: TPlayer;
begin
  TmpName := e_Raw_Read_String(P);
  TmpModel := e_Raw_Read_String(P);
  TmpColor.R := e_Raw_Read_Byte(P);
  TmpColor.G := e_Raw_Read_Byte(P);
  TmpColor.B := e_Raw_Read_Byte(P);
  TmpTeam := e_Raw_Read_Byte(P);

  Pl := g_Player_Get(C^.Player);
  if Pl = nil then Exit;

  if (gGameSettings.GameMode in [GM_TDM, GM_CTF]) and (Pl.Team <> TmpTeam) then
    Pl.ChangeTeam
  else
    Pl.SetColor(TmpColor);

  if Pl.Name <> TmpName then
  begin
    g_Console_Add(Format(_lc[I_PLAYER_NAME], [Pl.Name, TmpName]), True);
    Pl.Name := TmpName;
  end;

  if TmpModel <> Pl.Model.Name then
    Pl.SetModel(TmpModel);

  MH_SEND_PlayerSettings(Pl.UID, TmpModel);
end;

// RCON

procedure MH_RECV_RCONPassword(C: pTNetClient; P: Pointer);
var
  Pwd: string;
begin
  Pwd := e_Raw_Read_String(P);
  if not NetAllowRCON then Exit;
  if Pwd = NetRCONPassword then
  begin
    C^.RCONAuth := True;
    MH_SEND_Chat(_lc[I_NET_RCON_PWD_VALID], C^.ID);
  end
  else
    MH_SEND_Chat(_lc[I_NET_RCON_PWD_INVALID], C^.ID);
end;
procedure MH_RECV_RCONCommand(C: pTNetClient; P: Pointer);
var
  Cmd: string;
begin
  Cmd := e_Raw_Read_String(P);
  if not NetAllowRCON then Exit;
  if not C^.RCONAuth then
  begin
    MH_SEND_Chat(_lc[I_NET_RCON_NOAUTH], C^.ID);
    Exit;
  end;
  g_Console_Process(Cmd);
end;

// MISC

procedure MH_RECV_Vote(C: pTNetClient; P: Pointer);
var
  Start: Boolean;
  Name, Command: string;
  Need: Integer;
  Pl: TPlayer;
begin
  Start := e_Raw_Read_Byte(P) <> 0;
  Command := e_Raw_Read_String(P);

  Pl := g_Player_Get(C^.Player);
  if Pl = nil then Exit;
  Name := Pl.Name;
    
  if Start then
  begin
    if not g_Console_CommandBlacklisted(Command) then
      g_Game_StartVote(Command, Name);
  end
  else if gVoteInProgress then
  begin
    if not NetDedicated then
      Need := Floor((NetClientCount+1)/2.0) + 1
    else
      Need := Floor(NetClientCount/2.0) + 1;
    if C^.Voted then
    begin
      Dec(gVoteCount);
      C^.Voted := False;
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_REVOKED], [Name, gVoteCount, Need]), True);
      MH_SEND_VoteEvent(NET_VE_REVOKE, Name, 'a', gVoteCount, Need);
    end
    else
    begin
      Inc(gVoteCount);
      C^.Voted := True;
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_VOTE], [Name, gVoteCount, Need]), True);
      MH_SEND_VoteEvent(NET_VE_VOTE, Name, 'a', gVoteCount, Need);
      g_Game_CheckVote;
    end;
  end;
end;

// GAME (SEND)

procedure MH_SEND_Everything(CreatePlayers: Boolean = False; ID: Integer = NET_EVERYONE);
var
  I: Integer;
begin
  if gPlayers <> nil then
    for I := Low(gPlayers) to High(gPlayers) do
      if gPlayers[I] <> nil then
      begin
        if CreatePlayers then MH_SEND_PlayerCreate(gPlayers[I].UID, ID);
        MH_SEND_PlayerPos(True, gPlayers[I].UID, ID);
        MH_SEND_PlayerStats(gPlayers[I].UID, ID);

        if (gPlayers[I].Flag <> FLAG_NONE) and (gGameSettings.GameMode = GM_CTF) then
          MH_SEND_FlagEvent(FLAG_STATE_CAPTURED, gPlayers[I].Flag, gPlayers[I].UID, True, ID);
      end;

  if gItems <> nil then
  begin
    for I := High(gItems) downto Low(gItems) do
      if gItems[I].Live then
        MH_SEND_ItemSpawn(True, I, ID);
  end;

  if gMonsters <> nil then
    for I := 0 to High(gMonsters) do
      if gMonsters[I] <> nil then
        MH_SEND_MonsterSpawn(gMonsters[I].UID, ID);

  if gWalls <> nil then
    for I := Low(gWalls) to High(gWalls) do
      if gWalls[I] <> nil then
        with gWalls[I] do
        begin
          if Door then
            MH_SEND_PanelState(PanelType, I, ID);

          if GetTextureCount > 1 then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);
        end;

  if gLifts <> nil then
    for I := Low(gLifts) to High(gLifts) do
      if gLifts[I] <> nil then
        with gLifts[I] do
          MH_SEND_PanelState(PanelType, I, ID);

  if gRenderForegrounds <> nil then
    for I := Low(gRenderForegrounds) to High(gRenderForegrounds) do
      if gRenderForegrounds[I] <> nil then
        with gRenderForegrounds[I] do
          if (GetTextureCount > 1) then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);
  if gRenderBackgrounds <> nil then
    for I := Low(gRenderBackgrounds) to High(gRenderBackgrounds) do
      if gRenderBackgrounds[I] <> nil then
        with gRenderBackgrounds[I] do
          if GetTextureCount > 1 then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);
  if gWater <> nil then
    for I := Low(gWater) to High(gWater) do
      if gWater[I] <> nil then
        with gWater[I] do
          if GetTextureCount > 1 then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);
  if gAcid1 <> nil then
    for I := Low(gAcid1) to High(gAcid1) do
      if gAcid1[I] <> nil then
        with gAcid1[I] do
          if GetTextureCount > 1 then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);
  if gAcid2 <> nil then
    for I := Low(gAcid2) to High(gAcid2) do
      if gAcid2[I] <> nil then
        with gAcid2[I] do
          if GetTextureCount > 1 then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);
  if gSteps <> nil then
    for I := Low(gSteps) to High(gSteps) do
      if gSteps[I] <> nil then
        with gSteps[I] do
          if GetTextureCount > 1 then
            MH_SEND_PanelTexture(PanelType, I, LastAnimLoop, ID);

  if gTriggers <> nil then
    for I := Low(gTriggers) to High(gTriggers) do
      if gTriggers[I].TriggerType = TRIGGER_SOUND then
        MH_SEND_TriggerSound(gTriggers[I], ID);

  if Shots <> nil then
    for I := Low(Shots) to High(Shots) do
      if Shots[i].ShotType in [6, 7, 8] then
        MH_SEND_CreateShot(i, ID);

  MH_SEND_TriggerMusic(ID);

  MH_SEND_GameStats(ID);
  MH_SEND_CoopStats(ID);

  if gGameSettings.GameMode = GM_CTF then
  begin
    if gFlags[FLAG_RED].State <> FLAG_STATE_CAPTURED then
      MH_SEND_FlagEvent(gFlags[FLAG_RED].State, FLAG_RED, 0, True, ID);
    if gFlags[FLAG_BLUE].State <> FLAG_STATE_CAPTURED then
      MH_SEND_FlagEvent(gFlags[FLAG_BLUE].State, FLAG_BLUE, 0, True, ID);
  end;

  if CreatePlayers and (ID >= 0) then NetClients[ID].State := NET_STATE_GAME;

  if gLMSRespawn then
    MH_SEND_Chat(IntToStr((gLMSRespawnTime - gTime) div 1000) +
                 _lc[I_PLAYER_SPECT5], ID);
end;

procedure MH_SEND_Info(ID: Byte);
var
  Map: string;
begin
  g_ProcessResourceStr(gMapInfo.Map, nil, nil, @Map);

  e_Buffer_Clear(@NetOut);

  e_Buffer_Write(@NetOut, Byte(NET_MSG_INFO));
  e_Buffer_Write(@NetOut, ID);
  e_Buffer_Write(@NetOut, NetClients[ID].Player);
  e_Buffer_Write(@NetOut, ExtractFileName(gGameSettings.WAD));
  e_Buffer_Write(@NetOut, Map);
  e_Buffer_Write(@NetOut, gWADHash);
  e_Buffer_Write(@NetOut, gGameSettings.GameMode);
  e_Buffer_Write(@NetOut, gGameSettings.GoalLimit);
  e_Buffer_Write(@NetOut, gGameSettings.TimeLimit);
  e_Buffer_Write(@NetOut, gGameSettings.MaxLives);
  e_Buffer_Write(@NetOut, gGameSettings.Options);
  e_Buffer_Write(@NetOut, gTime);

  g_Net_Host_Send(ID, True, NET_CHAN_SERVICE);
end;

procedure MH_SEND_Chat(Txt: string; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_CHAT));
  e_Buffer_Write(@NetOut, Txt);

  g_Net_Host_Send(ID, True, NET_CHAN_CHAT);

  if ID <> NET_EVERYONE then
    Exit;

  if NetNotChat then
  begin
    NetNotChat := False;
    Exit;
  end;

  g_Console_Add(Txt, True);
  e_WriteLog('[Chat] ' + Txt, MSG_NOTIFY);
  g_Sound_PlayEx('SOUND_GAME_RADIO');
end;

procedure MH_SEND_Effect(X, Y: Integer; Ang: SmallInt; Kind: Byte; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_GFX));
  e_Buffer_Write(@NetOut, Kind);
  e_Buffer_Write(@NetOut, X);
  e_Buffer_Write(@NetOut, Y);
  e_Buffer_Write(@NetOut, Ang);

  g_Net_Host_Send(ID, False, NET_CHAN_GAME);
end;

procedure MH_SEND_Sound(X, Y: Integer; Name: string; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_SND));
  e_Buffer_Write(@NetOut, Name);
  e_Buffer_Write(@NetOut, X);
  e_Buffer_Write(@NetOut, Y);

  g_Net_Host_Send(ID, False, NET_CHAN_GAME);
end;

procedure MH_SEND_CreateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
begin
  if (Shots = nil) or (Proj < 0) or (Proj > High(Shots)) then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_SHADD));
  e_Buffer_Write(@NetOut, Proj);
  e_Buffer_Write(@NetOut, Shots[Proj].ShotType);
  e_Buffer_Write(@NetOut, Shots[Proj].Target);
  e_Buffer_Write(@NetOut, Shots[Proj].SpawnerUID);
  e_Buffer_Write(@NetOut, Shots[Proj].Timeout);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.X);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.Y);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.Vel.X);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.Vel.Y);

  g_Net_Host_Send(ID, True, NET_CHAN_SHOTS);
end;

procedure MH_SEND_UpdateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
begin
  if (Shots = nil) or (Proj < 0) or (Proj > High(Shots)) then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_SHPOS));
  e_Buffer_Write(@NetOut, Proj);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.X);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.Y);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.Vel.X);
  e_Buffer_Write(@NetOut, Shots[Proj].Obj.Vel.Y);

  g_Net_Host_Send(ID, False, NET_CHAN_SHOTS);
end;

procedure MH_Send_DeleteShot(Proj: LongInt; X, Y: LongInt; Loud: Boolean = True; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_SHDEL));
  e_Buffer_Write(@NetOut, Proj);
  e_Buffer_Write(@NetOut, Byte(Loud));
  e_Buffer_Write(@NetOut, X);
  e_Buffer_Write(@NetOut, Y);

  g_Net_Host_Send(ID, True, NET_CHAN_SHOTS);
end;

procedure MH_SEND_GameStats(ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_SCORE));
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
    e_Buffer_Write(@NetOut, gTeamStat[TEAM_RED].Goals);
    e_Buffer_Write(@NetOut, gTeamStat[TEAM_BLUE].Goals);
  end
  else
    if gGameSettings.GameMode = GM_COOP then
    begin
      e_Buffer_Write(@NetOut, gCoopMonstersKilled);
      e_Buffer_Write(@NetOut, gCoopSecretsFound);
    end;

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_CoopStats(ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_COOP));
  e_Buffer_Write(@NetOut, gTotalMonsters);
  e_Buffer_Write(@NetOut, gSecretsCount);
  e_Buffer_Write(@NetOut, gCoopTotalMonstersKilled);
  e_Buffer_Write(@NetOut, gCoopTotalSecretsFound);
  e_Buffer_Write(@NetOut, gCoopTotalMonsters);
  e_Buffer_Write(@NetOut, gCoopTotalSecrets);
end;

procedure MH_SEND_GameEvent(EvType: Byte; EvParm: string = 'N'; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_GEVENT));
  e_Buffer_Write(@NetOut, EvType);
  e_Buffer_Write(@NetOut, EvParm);
  e_Buffer_Write(@NetOut, Byte(gLastMap));
  e_Buffer_Write(@NetOut, gTime);
  if (EvType = NET_EV_MAPSTART) and (Pos(':\', EvParm) > 0) then
  begin
    e_Buffer_Write(@NetOut, Byte(1));
    e_Buffer_Write(@NetOut, gWADHash);
  end else
    e_Buffer_Write(@NetOut, Byte(0));

  g_Net_Host_Send(ID, True, NET_CHAN_SERVICE);
end;

procedure MH_SEND_FlagEvent(Evtype: Byte; Flag: Byte; PID: Word; Quiet: Boolean = False; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_FLAG));
  e_Buffer_Write(@NetOut, EvType);
  e_Buffer_Write(@NetOut, Flag);
  e_Buffer_Write(@NetOut, Byte(Quiet));
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, gFlags[Flag].State);
  e_Buffer_Write(@NetOut, gFlags[Flag].Obj.X);
  e_Buffer_Write(@NetOut, gFlags[Flag].Obj.Y);
  e_Buffer_Write(@NetOut, gFlags[Flag].Obj.Vel.X);
  e_Buffer_Write(@NetOut, gFlags[Flag].Obj.Vel.Y);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_GameSettings(ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_GSET));
  e_Buffer_Write(@NetOut, gGameSettings.GameMode);
  e_Buffer_Write(@NetOut, gGameSettings.GoalLimit);
  e_Buffer_Write(@NetOut, gGameSettings.TimeLimit);
  e_Buffer_Write(@NetOut, gGameSettings.MaxLives);
  e_Buffer_Write(@NetOut, gGameSettings.Options);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

// PLAYER (SEND)

procedure MH_SEND_PlayerCreate(PID: Word; ID: Integer = NET_EVERYONE);
var
  P: TPlayer;
begin
  P := g_Player_Get(PID);
  if P = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLR));
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, P.Name);

  e_Buffer_Write(@NetOut, P.FActualModelName);
  e_Buffer_Write(@NetOut, P.Model.Color.R);
  e_Buffer_Write(@NetOut, P.Model.Color.G);
  e_Buffer_Write(@NetOut, P.Model.Color.B);
  e_Buffer_Write(@NetOut, P.Team);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT)
end;

procedure MH_SEND_PlayerPos(Reliable: Boolean; PID: Word; ID: Integer = NET_EVERYONE);
var
  kByte: Word;
  Pl: TPlayer;
begin
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;
  if Pl.FDummy then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRPOS));
  e_Buffer_Write(@NetOut, PID);

  kByte := 0;

  with Pl do
  begin
    e_Buffer_Write(@NetOut, FPing);
    e_Buffer_Write(@NetOut, FLoss);
    if IsKeyPressed(KEY_CHAT) then
      kByte := NET_KEY_CHAT
    else
    begin
      if IsKeyPressed(KEY_LEFT) then kByte := kByte or NET_KEY_LEFT;
      if IsKeyPressed(KEY_RIGHT) then kByte := kByte or NET_KEY_RIGHT;
      if IsKeyPressed(KEY_UP) then kByte := kByte or NET_KEY_UP;
      if IsKeyPressed(KEY_DOWN) then kByte := kByte or NET_KEY_DOWN;
      if IsKeyPressed(KEY_JUMP) then kByte := kByte or NET_KEY_JUMP;
    end;

    e_Buffer_Write(@NetOut, kByte);
    if Direction = D_LEFT then e_Buffer_Write(@NetOut, Byte(0)) else e_Buffer_Write(@NetOut, Byte(1));
    e_Buffer_Write(@NetOut, GameX);
    e_Buffer_Write(@NetOut, GameY);
    e_Buffer_Write(@NetOut, GameVelX);
    e_Buffer_Write(@NetOut, GameVelY);
    e_Buffer_Write(@NetOut, GameAccelX);
    e_Buffer_Write(@NetOut, GameAccelY);
  end;

  g_Net_Host_Send(ID, Reliable, NET_CHAN_PLAYERPOS);
end;

procedure MH_SEND_PlayerStats(PID: Word; ID: Integer = NET_EVERYONE);
var
  P: TPlayer;
  I: Integer;
begin
  P := g_Player_Get(PID);
  if P = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRSTA));
  e_Buffer_Write(@NetOut, PID);

  with P do
  begin
    e_Buffer_Write(@NetOut, Byte(Live));
    e_Buffer_Write(@NetOut, Byte(GodMode));
    e_Buffer_Write(@NetOut, Health);
    e_Buffer_Write(@NetOut, Armor);
    e_Buffer_Write(@NetOut, Air);
    e_Buffer_Write(@NetOut, JetFuel);
    e_Buffer_Write(@NetOut, Lives);

    for I := WEAPON_KASTET to WEAPON_SUPERPULEMET do
      e_Buffer_Write(@NetOut, Byte(FWeapon[I]));

    for I := A_BULLETS to A_CELLS do
      e_Buffer_Write(@NetOut, FAmmo[I]);

    for I := A_BULLETS to A_CELLS do
      e_Buffer_Write(@NetOut, FMaxAmmo[I]);

    for I := MR_SUIT to MR_MAX do
      e_Buffer_Write(@NetOut, LongWord(FMegaRulez[I]));

    e_Buffer_Write(@NetOut, Byte(R_ITEM_BACKPACK in FRulez));
    e_Buffer_Write(@NetOut, Byte(R_KEY_RED in FRulez));
    e_Buffer_Write(@NetOut, Byte(R_KEY_GREEN in FRulez));
    e_Buffer_Write(@NetOut, Byte(R_KEY_BLUE in FRulez));
    e_Buffer_Write(@NetOut, Byte(R_BERSERK in FRulez));

    e_Buffer_Write(@NetOut, Frags);
    e_Buffer_Write(@NetOut, Death);

    e_Buffer_Write(@NetOut, CurrWeap);

    e_Buffer_Write(@NetOut, Byte(FSpectator));
    e_Buffer_Write(@NetOut, Byte(FGhost));
    e_Buffer_Write(@NetOut, Byte(FPhysics));
    e_Buffer_Write(@NetOut, Byte(FNoRespawn));
    e_Buffer_Write(@NetOut, Byte(FJetpack));
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_PLAYER);
end;

procedure MH_SEND_PlayerDamage(PID: Word; Kind: Byte; Attacker, Value: Word; VX, VY: Integer; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRDMG));
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, Kind);
  e_Buffer_Write(@NetOut, Attacker);
  e_Buffer_Write(@NetOut, Value);
  e_Buffer_Write(@NetOut, VX);
  e_Buffer_Write(@NetOut, VY);

  g_Net_Host_Send(ID, False, NET_CHAN_PLAYER);
end;

procedure MH_SEND_PlayerDeath(PID: Word; KillType, DeathType: Byte; Attacker: Word; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRDIE));
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, KillType);
  e_Buffer_Write(@NetOut, DeathType);
  e_Buffer_Write(@NetOut, Attacker);

  g_Net_Host_Send(ID, True, NET_CHAN_PLAYER);
end;

procedure MH_SEND_PlayerFire(PID: Word; Weapon: Byte; X, Y, AX, AY: Integer; ShotID: Integer = -1; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRFIRE));
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, Weapon);
  e_Buffer_Write(@NetOut, X);
  e_Buffer_Write(@NetOut, Y);
  e_Buffer_Write(@NetOut, AX);
  e_Buffer_Write(@NetOut, AY);
  e_Buffer_Write(@NetOut, ShotID);

  g_Net_Host_Send(ID, True, NET_CHAN_SHOTS);
end;

procedure MH_SEND_PlayerDelete(PID: Word; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRDEL));
  e_Buffer_Write(@NetOut, PID);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_PlayerSettings(PID: Word; Mdl: string = ''; ID: Integer = NET_EVERYONE);
var
  Pl: TPlayer;
begin
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRSET));
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, Pl.Name);
  if Mdl = '' then
    e_Buffer_Write(@NetOut, Pl.Model.Name)
  else
    e_Buffer_Write(@NetOut, Mdl);
  e_Buffer_Write(@NetOut, Pl.Model.Color.R);
  e_Buffer_Write(@NetOut, Pl.Model.Color.G);
  e_Buffer_Write(@NetOut, Pl.Model.Color.B);
  e_Buffer_Write(@NetOut, Pl.Team);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

// ITEM (SEND)

procedure MH_SEND_ItemSpawn(Quiet: Boolean; IID: Word; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_ISPAWN));
  e_Buffer_Write(@NetOut, IID);
  e_Buffer_Write(@NetOut, Byte(Quiet));
  e_Buffer_Write(@NetOut, gItems[IID].ItemType);
  e_Buffer_Write(@NetOut, Byte(gItems[IID].Fall));
  e_Buffer_Write(@NetOut, Byte(gItems[IID].Respawnable));
  e_Buffer_Write(@NetOut, gItems[IID].Obj.X);
  e_Buffer_Write(@NetOut, gItems[IID].Obj.Y);
  e_Buffer_Write(@NetOut, gItems[IID].Obj.Vel.X);
  e_Buffer_Write(@NetOut, gItems[IID].Obj.Vel.Y);

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_ItemDestroy(Quiet: Boolean; IID: Word; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_IDEL));
  e_Buffer_Write(@NetOut, IID);
  e_Buffer_Write(@NetOut, Byte(Quiet));

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

// PANEL

procedure MH_SEND_PanelTexture(PType: Word; PID: LongWord; AnimLoop: Byte; ID: Integer = NET_EVERYONE);
var
  TP: TPanel;
begin
  case PType of
    PANEL_WALL, PANEL_OPENDOOR, PANEL_CLOSEDOOR:
      TP := gWalls[PID];
    PANEL_FORE:
      TP := gRenderForegrounds[PID];
    PANEL_BACK:
      TP := gRenderBackgrounds[PID];
    PANEL_WATER:
      TP := gWater[PID];
    PANEL_ACID1:
      TP := gAcid1[PID];
    PANEL_ACID2:
      TP := gAcid2[PID];
    PANEL_STEP:
      TP := gSteps[PID];
    else
      Exit;
  end;

  with TP do
  begin
    e_Buffer_Write(@NetOut, Byte(NET_MSG_PTEX));
    e_Buffer_Write(@NetOut, PType);
    e_Buffer_Write(@NetOut, PID);
    e_Buffer_Write(@NetOut, FCurTexture);
    e_Buffer_Write(@NetOut, FCurFrame);
    e_Buffer_Write(@NetOut, FCurFrameCount);
    e_Buffer_Write(@NetOut, AnimLoop);
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_PanelState(PType: Word; PID: LongWord; ID: Integer = NET_EVERYONE);
var
  TP: TPanel;
begin
  case PType of
    PANEL_WALL, PANEL_OPENDOOR, PANEL_CLOSEDOOR:
      TP := gWalls[PID];
    PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT:
      TP := gLifts[PID];
    else
      Exit;
  end;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_PSTATE));
  e_Buffer_Write(@NetOut, PType);
  e_Buffer_Write(@NetOut, PID);
  e_Buffer_Write(@NetOut, Byte(TP.Enabled));
  e_Buffer_Write(@NetOut, TP.LiftType);

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

// TRIGGER

procedure MH_SEND_TriggerSound(var T: TTrigger; ID: Integer = NET_EVERYONE);
begin
  if gTriggers = nil then Exit;
  if T.Sound = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_TSOUND));
  e_Buffer_Write(@NetOut, T.Sound.Name);
  e_Buffer_Write(@NetOut, Byte(T.Sound.IsPlaying));
  e_Buffer_Write(@NetOut, LongWord(T.Sound.GetPosition));
  e_Buffer_Write(@NetOut, T.SoundPlayCount);

  g_Net_Host_Send(ID, True);
end;

procedure MH_SEND_TriggerMusic(ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_TMUSIC));
  e_Buffer_Write(@NetOut, gMusic.Name);
  e_Buffer_Write(@NetOut, Byte(gMusic.IsPlaying));
  e_Buffer_Write(@NetOut, LongWord(gMusic.GetPosition));
  e_Buffer_Write(@NetOut, Byte(gMusic.SpecPause or gMusic.IsPaused));

  g_Net_Host_Send(ID, True);
end;

// MONSTER

procedure MH_SEND_MonsterSpawn(UID: Word; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_Get(UID);
  if M = nil then
    Exit;

  with M do
  begin
    e_Buffer_Write(@NetOut, Byte(NET_MSG_MSPAWN));
    e_Buffer_Write(@NetOut, UID);
    e_Buffer_Write(@NetOut, MonsterType);
    e_Buffer_Write(@NetOut, MonsterState);
    e_Buffer_Write(@NetOut, MonsterAnim);
    e_Buffer_Write(@NetOut, MonsterTargetUID);
    e_Buffer_Write(@NetOut, MonsterTargetTime);
    e_Buffer_Write(@NetOut, MonsterBehaviour);
    e_Buffer_Write(@NetOut, MonsterSleep);
    e_Buffer_Write(@NetOut, MonsterHealth);
    e_Buffer_Write(@NetOut, MonsterAmmo);
    e_Buffer_Write(@NetOut, GameX);
    e_Buffer_Write(@NetOut, GameY);
    e_Buffer_Write(@NetOut, GameVelX);
    e_Buffer_Write(@NetOut, GameVelY);
    e_Buffer_Write(@NetOut, Byte(GameDirection));
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_MonsterPos(UID: Word; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_Get(UID);
  if M = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_MPOS));
  e_Buffer_Write(@NetOut, UID);

  with M do
  begin
    e_Buffer_Write(@NetOut, GameX);
    e_Buffer_Write(@NetOut, GameY);
    e_Buffer_Write(@NetOut, GameVelX);
    e_Buffer_Write(@NetOut, GameVelY);
    e_Buffer_Write(@NetOut, Byte(GameDirection));
  end;

  g_Net_Host_Send(ID, False, NET_CHAN_MONSTERPOS);
end;

procedure MH_SEND_MonsterState(UID: Word; ForcedAnim: Byte = 255; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_Get(UID);
  if M = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_MSTATE));
  e_Buffer_Write(@NetOut, UID);

  with M do
  begin
    e_Buffer_Write(@NetOut, MonsterState);
    e_Buffer_Write(@NetOut, ForcedAnim);
    e_Buffer_Write(@NetOut, MonsterTargetUID);
    e_Buffer_Write(@NetOut, MonsterTargetTime);
    e_Buffer_Write(@NetOut, MonsterSleep);
    e_Buffer_Write(@NetOut, MonsterHealth);
    e_Buffer_Write(@NetOut, MonsterAmmo);
    e_Buffer_Write(@NetOut, MonsterPain);
    e_Buffer_Write(@NetOut, Byte(AnimIsReverse));
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_MONSTER);
end;

procedure MH_SEND_MonsterShot(UID: Word; X, Y, VX, VY: Integer; ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_MSHOT));
  e_Buffer_Write(@NetOut, UID);
  e_Buffer_Write(@NetOut, X);
  e_Buffer_Write(@NetOut, Y);
  e_Buffer_Write(@NetOut, VX);
  e_Buffer_Write(@NetOut, VY);

  g_Net_Host_Send(ID, True, NET_CHAN_MONSTER);
end;

procedure MH_SEND_MonsterDelete(UID: Word; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_Get(UID);
  if M = nil then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_MDEL));
  e_Buffer_Write(@NetOut, UID);

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

// MISC

procedure MH_SEND_VoteEvent(EvType: Byte;
                            StrArg1: string = 'a'; StrArg2: string = 'b';
                            IntArg1: SmallInt = 0; IntArg2: SmallInt = 0;
                            ID: Integer = NET_EVERYONE);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_VOTE_EVENT));
  e_Buffer_Write(@NetOut, EvType);
  e_Buffer_Write(@NetOut, IntArg1);
  e_Buffer_Write(@NetOut, IntArg2);
  e_Buffer_Write(@NetOut, StrArg1);
  e_Buffer_Write(@NetOut, StrArg2);
  
  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

// CLIENT MESSAGES //

// GAME

procedure MC_RECV_Chat(P: Pointer);
var
  Txt: string;
begin
  Txt := e_Raw_Read_String(P);
  g_Console_Add(Txt, True);
  e_WriteLog('[Chat] ' + Txt, MSG_NOTIFY);
  g_Sound_PlayEx('SOUND_GAME_RADIO');
end;

procedure MC_RECV_Effect(P: Pointer);
var
  Kind: Byte;
  X, Y: Integer;
  Ang: SmallInt;
  Anim: TAnimation;
  ID: LongWord;
begin
  if not gGameOn then Exit;
  Kind := e_Raw_Read_Byte(P);
  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  Ang := e_Raw_Read_SmallInt(P);

  case Kind of
    NET_GFX_SPARK:
      g_GFX_Spark(X, Y, 2 + Random(2), Ang, 0, 0);

    NET_GFX_SPAWN:
      if g_Frames_Get(ID, 'FRAMES_TELEPORT') then
      begin
        Anim := TAnimation.Create(ID, False, 3);
        g_GFX_OnceAnim(X, Y, Anim);
        Anim.Free();
      end;
    NET_GFX_TELE:
    begin
      if g_Frames_Get(ID, 'FRAMES_TELEPORT') then
      begin
        Anim := TAnimation.Create(ID, False, 3);
        g_GFX_OnceAnim(X, Y, Anim);
        Anim.Free();
      end;
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_GAME_TELEPORT', X, Y);
    end;
    NET_GFX_BFG:
    begin
      if g_Frames_Get(ID, 'FRAMES_BFGHIT') then
      begin
        Anim := TAnimation.Create(ID, False, 4);
        g_GFX_OnceAnim(X-32, Y-32, Anim);
        Anim.Free();
      end;
    end;
    NET_GFX_FIRE:
    begin
      if g_Frames_Get(ID, 'FRAMES_FIRE') then
      begin
        Anim := TAnimation.Create(ID, False, 4);
        g_GFX_OnceAnim(X, Y, Anim);
        Anim.Free();
      end;
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_FIRE', X, Y);
    end;
    NET_GFX_RESPAWN:
    begin
      if g_Frames_Get(ID, 'FRAMES_ITEM_RESPAWN') then
      begin
        Anim := TAnimation.Create(ID, False, 4);
        g_GFX_OnceAnim(X, Y, Anim);
        Anim.Free();
      end;
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', X, Y);
    end;
  end;
end;

procedure MC_RECV_Sound(P: Pointer);
var
  Name: string;
  X, Y: Integer;
begin
  Name := e_Raw_Read_String(P);
  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  g_Sound_PlayExAt(Name, X, Y);
end;

procedure MC_RECV_CreateShot(P: Pointer);
var
  I, X, Y, XV, YV: Integer;
  Timeout: LongWord;
  Target, Spawner: Word;
  ShType: Byte;
begin
  I := e_Raw_Read_LongInt(P);
  ShType := e_Raw_Read_Byte(P);
  Target := e_Raw_Read_Word(P);
  Spawner := e_Raw_Read_Word(P);
  Timeout := e_Raw_Read_LongWord(P);
  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  XV := e_Raw_Read_LongInt(P);
  YV := e_Raw_Read_LongInt(P);

  I := g_Weapon_CreateShot(I, ShType, Spawner, Target, X, Y, XV, YV);
  if (Shots <> nil) and (I <= High(Shots)) then
  begin
    Shots[I].Timeout := Timeout;
    //Shots[I].Target := Target; // TODO: find a use for Target later
  end;
end;

procedure MC_RECV_UpdateShot(P: Pointer);
var
  I, TX, TY, TXV, TYV: Integer;
begin
  I := e_Raw_Read_LongInt(P);
  TX := e_Raw_Read_LongInt(P);
  TY := e_Raw_Read_LongInt(P);
  TXV := e_Raw_Read_LongInt(P);
  TYV := e_Raw_Read_LongInt(P);

  if (Shots <> nil) and (I <= High(Shots)) then
    with (Shots[i]) do
    begin
      Obj.X := TX;
      Obj.Y := TY;
      Obj.Vel.X := TXV;
      Obj.Vel.Y := TYV;
    end;
end;

procedure MC_RECV_DeleteShot(P: Pointer);
var
  I, X, Y: Integer;
  L: Boolean;
begin
  if not gGameOn then Exit;
  I := e_Raw_Read_LongInt(P);
  L := (e_Raw_Read_Byte(P) <> 0);
  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);

  g_Weapon_DestroyShot(I, X, Y, L);
end;

procedure MC_RECV_GameStats(P: Pointer);
begin
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
    gTeamStat[TEAM_RED].Goals := e_Raw_Read_SmallInt(P);
    gTeamStat[TEAM_BLUE].Goals := e_Raw_Read_SmallInt(P);
  end
  else
    if gGameSettings.GameMode = GM_COOP then
    begin
      gCoopMonstersKilled := e_Raw_Read_Word(P);
      gCoopSecretsFound := e_Raw_Read_Word(P);
    end;
end;

procedure MC_RECV_CoopStats(P: Pointer);
begin
  gTotalMonsters := e_Raw_Read_LongInt(P);
  gSecretsCount := e_Raw_Read_LongInt(P);
  gCoopTotalMonstersKilled := e_Raw_Read_Word(P);
  gCoopTotalSecretsFound := e_Raw_Read_Word(P);
  gCoopTotalMonsters := e_Raw_Read_Word(P);
  gCoopTotalSecrets := e_Raw_Read_Word(P);
end;

procedure MC_RECV_GameEvent(P: Pointer);
var
  EvType: Byte;
  EvParm, NewWAD, ResName: string;
  EvTime: LongWord;
  BHash: Boolean;
  EvHash: TMD5Digest;
begin
  EvType := e_Raw_Read_Byte(P);
  EvParm := e_Raw_Read_String(P);
  gLastMap := e_Raw_Read_Byte(P) <> 0;
  if gLastMap and (gGameSettings.GameMode = GM_COOP) then gStatsOff := True;
  gStatsPressed := True;
  EvTime := e_Raw_Read_LongWord(P);
  BHash := e_Raw_Read_Byte(P) <> 0;
  if BHash then
    EvHash := e_Raw_Read_MD5(P);

  gTime := EvTime;

  case EvType of
    NET_EV_MAPSTART:
    begin
      gGameOn := False;
      g_Game_ClearLoading();
      g_Game_StopAllSounds(True);

      if Pos(':\', EvParm) = 0 then
      begin
        if not g_Game_StartMap(EvParm, True) then
        begin
          g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD],
                              [ExtractFileName(gGameSettings.WAD) + ':\' + EvParm]));
          Exit;
        end;
      end else
      begin
        g_ProcessResourceStr(MapsDir + EvParm, @NewWAD, nil, @ResName);
        g_Game_LoadWAD(NewWAD, EvHash);

        if not g_Game_StartMap(ResName, True) then
        begin
          g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD],
                              [ExtractFileName(gGameSettings.WAD) + ':\' + ResName]));
          Exit;
        end;
      end;

      MC_SEND_FullStateRequest;
    end;

    NET_EV_MAPEND:
    begin
      gMissionFailed := EvParm = 'MF';
      gExit := EXIT_ENDLEVELCUSTOM;
    end;

    NET_EV_LMS_LOSE:
      g_Game_Message(_lc[I_MESSAGE_LMS_LOSE], 144);
    NET_EV_LMS_WIN:
      g_Game_Message(Format(_lc[I_MESSAGE_LMS_WIN], [AnsiUpperCase(EvParm)]), 144);
    NET_EV_LMS_START:
    begin
      g_Player_RemoveAllCorpses;
      g_Game_Message(_lc[I_MESSAGE_LMS_START], 144);
    end;
    NET_EV_LMS_DRAW:
      g_Game_Message(_lc[I_GAME_WIN_DRAW], 144);
    NET_EV_LMS_SURVIVOR:
      g_Console_Add('*** ' + _lc[I_MESSAGE_LMS_SURVIVOR] + ' ***', True);
    NET_EV_TLMS_WIN:
    begin
      if EvParm = 'r' then
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 144)
      else
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 144);
    end;
    NET_EV_SCORE_ADD:
    begin
      if Pos('r', EvParm) > 0 then
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108)
      else
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
    end;
    NET_EV_SCORE_SUB:
    begin
      if Pos('r', EvParm) > 0 then
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108)
      else
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
    end;
    NET_EV_BIGTEXT:
      g_Game_Message(AnsiUpperCase(EvParm), 144);
  end;
end;

procedure MC_RECV_FlagEvent(P: Pointer);
var
  PID: Word;
  Pl: TPlayer;
  EvType: Byte;
  Fl: Byte;
  Quiet: Boolean;
  s: string;
begin
  EvType := e_Raw_Read_Byte(P);
  Fl := e_Raw_Read_Byte(P);

  if Fl = FLAG_NONE then Exit;

  Quiet := (e_Raw_Read_Byte(P) <> 0);
  PID := e_Raw_Read_Word(P);

  gFlags[Fl].State := e_Raw_Read_Byte(P);
  gFlags[Fl].Obj.X := e_Raw_Read_LongInt(P);
  gFlags[Fl].Obj.Y := e_Raw_Read_LongInt(P);
  gFlags[Fl].Obj.Vel.X := e_Raw_Read_LongInt(P);
  gFlags[Fl].Obj.Vel.Y := e_Raw_Read_LongInt(P);

  Pl := g_Player_Get(PID);
  if (Pl = nil) and (EvType <> FLAG_STATE_NORMAL) and (EvType <> FLAG_STATE_DROPPED) then Exit;

  case EvType of
    FLAG_STATE_NORMAL:
    begin
      if Quiet or (Pl = nil) then Exit;

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);
    end;

    FLAG_STATE_CAPTURED:
    begin
      if (Pl <> nil) then Pl.SetFlag(Fl);

      if Quiet then Exit;

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      g_Console_Add(Format(_lc[I_PLAYER_FLAG_GET], [Pl.Name, s]), True);
      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_GET], [AnsiUpperCase(s)]), 144);
    end;

    FLAG_STATE_DROPPED:
    begin
      if (Pl <> nil) then Pl.SetFlag(FLAG_NONE);

      if Quiet or (Pl = nil) then Exit;

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      g_Console_Add(Format(_lc[I_PLAYER_FLAG_DROP], [Pl.Name, s]), True);
      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_DROP], [AnsiUpperCase(s)]), 144);
    end;

    FLAG_STATE_SCORED:
    begin
      g_Map_ResetFlag(FLAG_RED);
      g_Map_ResetFlag(FLAG_BLUE);
      if Quiet or (Pl = nil) then Exit;
      Pl.SetFlag(FLAG_NONE);

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_BLUE]
      else
        s := _lc[I_PLAYER_FLAG_RED];

      g_Console_Add(Format(_lc[I_PLAYER_FLAG_CAPTURE], [Pl.Name, s]), True);
      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_CAPTURE], [AnsiUpperCase(s)]), 144);
    end;

    FLAG_STATE_RETURNED:
    begin
      g_Map_ResetFlag(Fl);
      if Quiet or (Pl = nil) then Exit;

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);
    end;
  end;
end;

procedure MC_RECV_GameSettings(P: Pointer);
begin
  gGameSettings.GameMode := e_Raw_Read_Byte(P);
  gGameSettings.GoalLimit := e_Raw_Read_Word(P);
  gGameSettings.TimeLimit := e_Raw_Read_Word(P);
  gGameSettings.MaxLives := e_Raw_Read_Byte(P);
  gGameSettings.Options := e_Raw_Read_LongWord(P);
end;

// PLAYER

function MC_RECV_PlayerCreate(P: Pointer): Word;
var
  PID, DID: Word;
  PName, Model: string;
  Color: TRGB;
  T: Byte;
  Pl: TPlayer;
begin
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);

  PName := e_Raw_Read_String(P);
  Model := e_Raw_Read_String(P);
  Color.R := e_Raw_Read_Byte(P);
  Color.G := e_Raw_Read_Byte(P);
  Color.B := e_Raw_Read_Byte(P);
  T := e_Raw_Read_Byte(P);

  Result := 0;
  if PID <> NetPlrUID then
  begin
    if (Pl <> nil) then Exit;
    DID := g_Player_Create(Model, Color, T, False, 0);
    with g_Player_Get(DID) do
    begin
      UID := PID;
      Name := PName;
      Reset(True);
    end;
  end
  else
  begin
    gPlayer1.UID := PID;
    gPlayer1.Model.SetColor(Color.R, Color.G, Color.B);
    gPlayer1.Team := T;
  end;

  g_Console_Add(Format(_lc[I_PLAYER_JOIN], [PName]), True);
  e_WriteLog('NET: Player ' + PName + ' [' + IntToStr(PID) + '] added.', MSG_NOTIFY);
  Result := PID;
end;

function MC_RECV_PlayerPos(P: Pointer): Word;
var
  PID: Word;
  kByte: Word;
  Pl: TPlayer;
  Dir: Byte;
  TmpX, TmpY: Integer;
begin
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  Result := 0;
  if Pl = nil then Exit;

  Result := PID;

  with Pl do
  begin
    FPing := e_Raw_Read_Word(P);
    FLoss := e_Raw_Read_Byte(P);
    kByte := e_Raw_Read_Word(P);
    Dir := e_Raw_Read_Byte(P);
    SetDirection(TDirection(Dir));

    TmpX := e_Raw_Read_LongInt(P);
    TmpY := e_Raw_Read_LongInt(P);

    ReleaseKeys;

    if (kByte = NET_KEY_CHAT) then
      PressKey(KEY_CHAT, 10000)
    else
    begin
      if LongBool(kByte and NET_KEY_LEFT) then PressKey(KEY_LEFT, 10000);
      if LongBool(kByte and NET_KEY_RIGHT) then PressKey(KEY_RIGHT, 10000);
      if LongBool(kByte and NET_KEY_UP) then PressKey(KEY_UP, 10000);
      if LongBool(kByte and NET_KEY_DOWN) then PressKey(KEY_DOWN, 10000);
      if LongBool(kByte and NET_KEY_JUMP) then PressKey(KEY_JUMP, 10000);
    end;

    GameVelX := e_Raw_Read_LongInt(P);
    GameVelY := e_Raw_Read_LongInt(P);
    GameAccelX := e_Raw_Read_LongInt(P);
    GameAccelY := e_Raw_Read_LongInt(P);
    SetLerp(TmpX, TmpY);
    if NetForcePlayerUpdate then Update();
  end;
end;

function MC_RECV_PlayerStats(P: Pointer): Word;
var
  PID: Word;
  Pl: TPlayer;
  I: Integer;
  OldJet: Boolean;
begin
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  Result := 0;
  if Pl = nil then
    Exit;

  with Pl do
  begin
    Live := (e_Raw_Read_Byte(P) <> 0);
    GodMode := (e_Raw_Read_Byte(P) <> 0);
    Health := e_Raw_Read_LongInt(P);
    Armor := e_Raw_Read_LongInt(P);
    Air := e_Raw_Read_LongInt(P);
    JetFuel := e_Raw_Read_LongInt(P);
    Lives := e_Raw_Read_Byte(P);

    for I := WEAPON_KASTET to WEAPON_SUPERPULEMET do
      FWeapon[I] := (e_Raw_Read_Byte(P) <> 0);

    for I := A_BULLETS to A_CELLS do
      FAmmo[I] := e_Raw_Read_Word(P);

    for I := A_BULLETS to A_CELLS do
      FMaxAmmo[I] := e_Raw_Read_Word(P);

    for I := MR_SUIT to MR_MAX do
      FMegaRulez[I] := e_Raw_Read_LongWord(P);

    FRulez := [];
    if (e_Raw_Read_Byte(P) <> 0) then
      FRulez := FRulez + [R_ITEM_BACKPACK];
    if (e_Raw_Read_Byte(P) <> 0) then
      FRulez := FRulez + [R_KEY_RED];
    if (e_Raw_Read_Byte(P) <> 0) then
      FRulez := FRulez + [R_KEY_GREEN];
    if (e_Raw_Read_Byte(P) <> 0) then
      FRulez := FRulez + [R_KEY_BLUE];
    if (e_Raw_Read_Byte(P) <> 0) then
      FRulez := FRulez + [R_BERSERK];

    Frags := e_Raw_Read_LongInt(P);
    Death := e_Raw_Read_LongInt(P);

    SetWeapon(e_Raw_Read_Byte(P));

    FSpectator := e_Raw_Read_Byte(P) <> 0;
    FGhost := e_Raw_Read_Byte(P) <> 0;
    FPhysics := e_Raw_Read_Byte(P) <> 0;
    FNoRespawn := e_Raw_Read_Byte(P) <> 0;
    OldJet := FJetpack;
    FJetpack := e_Raw_Read_Byte(P) <> 0;
    if OldJet and not FJetpack then
      JetpackOff
    else if not OldJet and FJetpack then
      JetpackOn;
  end;

  Result := PID;
end;

function MC_RECV_PlayerDamage(P: Pointer): Word;
var
  PID: Word;
  Pl: TPlayer;
  Kind: Byte;
  Attacker, Value: Word;
  VX, VY: Integer;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  Kind := e_Raw_Read_Byte(P);
  Attacker := e_Raw_Read_Word(P);
  Value := e_Raw_Read_Word(P);
  VX := e_Raw_Read_Word(P);
  VY := e_Raw_Read_Word(P);

  with Pl do
    Damage(Value, Attacker, VX, VY, Kind);

  Result := PID;
end;

function MC_RECV_PlayerDeath(P: Pointer): Word;
var
  PID: Word;
  Pl: TPlayer;
  KillType, DeathType: Byte;
  Attacker: Word;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  KillType := e_Raw_Read_Byte(P);
  DeathType := e_Raw_Read_Byte(P);
  Attacker := e_Raw_Read_Word(P);

  with Pl do
  begin
    Kill(KillType, Attacker, DeathType);
    SoftReset;
  end;
end;

function MC_RECV_PlayerDelete(P: Pointer): Word;
var
  PID: Word;
  Pl: TPlayer;
begin
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  Result := 0;
  if Pl = nil then Exit;

  g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [Pl.Name]), True);
  e_WriteLog('NET: Player ' + Pl.Name + ' [' + IntToStr(PID) + '] removed.', MSG_NOTIFY);

  g_Player_Remove(PID);

  Result := PID;
end;

function MC_RECV_PlayerFire(P: Pointer): Word;
var
  PID: Word;
  Weap: Byte;
  Pl: TPlayer;
  X, Y, AX, AY: Integer;
  SHID: Integer;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  Weap := e_Raw_Read_Byte(P);
  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  AX := e_Raw_Read_LongInt(P);
  AY := e_Raw_Read_LongInt(P);
  SHID := e_Raw_Read_LongInt(P);

  with Pl do
    if Live then NetFire(Weap, X, Y, AX, AY, SHID);
end;

procedure MC_RECV_PlayerSettings(P: Pointer);
var
  TmpName: string;
  TmpModel: string;
  TmpColor: TRGB;
  TmpTeam: Byte;
  Pl: TPlayer;
  PID: Word;
begin
  PID := e_Raw_Read_Word(P);
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  TmpName := e_Raw_Read_String(P);
  TmpModel := e_Raw_Read_String(P);
  TmpColor.R := e_Raw_Read_Byte(P);
  TmpColor.G := e_Raw_Read_Byte(P);
  TmpColor.B := e_Raw_Read_Byte(P);
  TmpTeam := e_Raw_Read_Byte(P);

  if (gGameSettings.GameMode in [GM_TDM, GM_CTF]) and (Pl.Team <> TmpTeam) then
    Pl.ChangeTeam
  else
    Pl.SetColor(TmpColor);

  if Pl.Name <> TmpName then
  begin
    g_Console_Add(Format(_lc[I_PLAYER_NAME], [Pl.Name, TmpName]), True);
    Pl.Name := TmpName;
  end;

  if TmpModel <> Pl.Model.Name then
    Pl.SetModel(TmpModel);
end;

// ITEM

procedure MC_RECV_ItemSpawn(P: Pointer);
var
  ID: Word;
  AID: DWord;
  X, Y, VX, VY: Integer;
  T: Byte;
  Quiet, Fall{, Resp}: Boolean;
  Anim: TAnimation;
begin
  if not gGameOn then Exit;
  ID := e_Raw_Read_Word(P);
  Quiet := e_Raw_Read_Byte(P) <> 0;
  T := e_Raw_Read_Byte(P);
  Fall := e_Raw_Read_Byte(P) <> 0;
  {Resp :=} e_Raw_Read_Byte(P);
  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  VX := e_Raw_Read_LongInt(P);
  VY := e_Raw_Read_LongInt(P);

  g_Items_Create(X, Y, T, Fall, False, False, ID);
  gItems[ID].Obj.Vel.X := VX;
  gItems[ID].Obj.Vel.Y := VY;

  if not Quiet then
  begin
    g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', X, Y);
    if g_Frames_Get(AID, 'FRAMES_ITEM_RESPAWN') then
    begin
      Anim := TAnimation.Create(AID, False, 4);
      g_GFX_OnceAnim(X+(gItems[ID].Obj.Rect.Width div 2)-16, Y+(gItems[ID].Obj.Rect.Height div 2)-16, Anim);
      Anim.Free();
    end;
  end;
end;

procedure MC_RECV_ItemDestroy(P: Pointer);
var
  ID: Word;
  Quiet: Boolean;
begin
  if not gGameOn then Exit;
  ID := e_Raw_Read_Word(P);
  Quiet := e_Raw_Read_Byte(P) <> 0;
  if gItems = nil then Exit;
  if (ID > High(gItems)) then Exit;

  if not Quiet then
  begin
    if gItems[ID].ItemType in [ITEM_SPHERE_BLUE, ITEM_SPHERE_WHITE, ITEM_INVUL, ITEM_INVIS, ITEM_MEDKIT_BLACK] then
      g_Sound_PlayExAt('SOUND_ITEM_GETRULEZ',
        gItems[ID].Obj.X, gItems[ID].Obj.Y)
      else
        if gItems[ID].ItemType in [ITEM_MEDKIT_SMALL, ITEM_MEDKIT_LARGE, ITEM_BOTTLE,
                                   ITEM_HELMET, ITEM_ARMOR_GREEN,
                                   ITEM_ARMOR_BLUE, ITEM_KEY_RED, ITEM_KEY_GREEN, ITEM_KEY_BLUE] then
          g_Sound_PlayExAt('SOUND_ITEM_GETMED',
            gItems[ID].Obj.X, gItems[ID].Obj.Y)
          else
            g_Sound_PlayExAt('SOUND_ITEM_GETITEM',
              gItems[ID].Obj.X, gItems[ID].Obj.Y);
  end;

  g_Items_Remove(ID);
end;

// PANEL

procedure MC_RECV_PanelTexture(P: Pointer);
var
  TP: TPanel;
  PType: Word;
  ID: LongWord;
  Tex, Fr: Integer;
  Loop, Cnt: Byte;
begin
  if not gGameOn then Exit;
  PType := e_Raw_Read_Word(P);
  ID := e_Raw_Read_LongWord(P);
  Tex := e_Raw_Read_LongInt(P);
  Fr := e_Raw_Read_LongInt(P);
  Cnt := e_Raw_Read_Byte(P);
  Loop := e_Raw_Read_Byte(P);

  TP := nil;

  case PType of
    PANEL_WALL, PANEL_OPENDOOR, PANEL_CLOSEDOOR:
     if gWalls <> nil then
      TP := gWalls[ID];
    PANEL_FORE:
     if gRenderForegrounds <> nil then
      TP := gRenderForegrounds[ID];
    PANEL_BACK:
     if gRenderBackgrounds <> nil then
      TP := gRenderBackgrounds[ID];
    PANEL_WATER:
     if gWater <> nil then
      TP := gWater[ID];
    PANEL_ACID1:
     if gAcid1 <> nil then
      TP := gAcid1[ID];
    PANEL_ACID2:
     if gAcid2 <> nil then
      TP := gAcid2[ID];
    PANEL_STEP:
     if gSteps <> nil then
      TP := gSteps[ID];
    else
      Exit;
  end;

  if TP <> nil then
  begin
    TP.SetTexture(Tex, Loop);
    TP.SetFrame(Fr, Cnt);
  end;
end;

procedure MC_RECV_PanelState(P: Pointer);
var
  ID: LongWord;
  E: Boolean;
  Lift: Byte;
  PType: Word;
begin
  if not gGameOn then Exit;
  PType := e_Raw_Read_Word(P);
  ID := e_Raw_Read_LongWord(P);
  E := (e_Raw_Read_Byte(P) <> 0);
  Lift := e_Raw_Read_Byte(P);

  case PType of
    PANEL_WALL, PANEL_OPENDOOR, PANEL_CLOSEDOOR:
      if E then
        g_Map_EnableWall(ID)
      else
        g_Map_DisableWall(ID);

    PANEL_LIFTUP, PANEL_LIFTDOWN, PANEL_LIFTLEFT, PANEL_LIFTRIGHT:
      g_Map_SetLift(ID, Lift);
  end;
end;

// TRIGGERS

procedure MC_RECV_TriggerSound(P: Pointer);
var
  SName: string;
  SPlaying: Boolean;
  SPos: LongWord;
  SCount: LongInt;
  I: Integer;
begin
  if not gGameOn then Exit;
  if gTriggers = nil then Exit;

  SName := e_Raw_Read_String(P);
  SPlaying := e_Raw_Read_Byte(P) <> 0;
  SPos := e_Raw_Read_LongWord(P);
  SCount := e_Raw_Read_LongInt(P);

  for I := Low(gTriggers) to High(gTriggers) do
    if gTriggers[I].TriggerType = TRIGGER_SOUND then
      if gTriggers[I].Sound.Name = SName then
        with gTriggers[I] do
        begin
          if SPlaying then
          begin
            if Data.Local then
              Sound.PlayVolumeAt(X+(Width div 2), Y+(Height div 2), Data.Volume/255.0)
            else
              Sound.PlayPanVolume((Data.Pan-127.0)/128.0, Data.Volume/255.0);
            Sound.SetPosition(SPos);
          end
          else
            if Sound.IsPlaying then Sound.Stop;

          SoundPlayCount := SCount;
        end;
end;

procedure MC_RECV_TriggerMusic(P: Pointer);
var
  MName: string;
  MPlaying: Boolean;
  MPos: LongWord;
  MPaused: Boolean;
begin
  if not gGameOn then Exit;

  MName := e_Raw_Read_String(P);
  MPlaying := e_Raw_Read_Byte(P) <> 0;
  MPos := e_Raw_Read_LongWord(P);
  MPaused := e_Raw_Read_Byte(P) <> 0;

  if MPlaying then
  begin
    gMusic.SetByName(MName);
    gMusic.Play(True);
    gMusic.SetPosition(MPos);
    gMusic.SpecPause := MPaused;
  end
  else
    if gMusic.IsPlaying then gMusic.Stop;
end;

// MONSTERS

procedure MC_RECV_MonsterSpawn(P: Pointer);
var
  ID: Word;
  MType, MState, MDir, MAnim, MBehav: Byte;
  X, Y, VX, VY, MTargTime, MHealth, MAmmo, MSleep: Integer;
  MTarg: Word;
  M: TMonster;
begin
  ID := e_Raw_Read_Word(P);
  M := g_Monsters_Get(ID);
  if M <> nil then
    Exit;

  MType := e_Raw_Read_Byte(P);
  MState := e_Raw_Read_Byte(P);
  MAnim := e_Raw_Read_Byte(P);
  MTarg := e_Raw_Read_Word(P);
  MTargTime := e_Raw_Read_LongInt(P);
  MBehav := e_Raw_Read_Byte(P);
  MSleep := e_Raw_Read_LongInt(P);
  MHealth := e_Raw_Read_LongInt(P);
  MAmmo := e_Raw_Read_LongInt(P);

  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  VX := e_Raw_Read_LongInt(P);
  VY := e_Raw_Read_LongInt(P);
  MDir := e_Raw_Read_Byte(P);

  g_Monsters_Create(MType, X, Y, TDirection(MDir), False, ID);
  M := g_Monsters_Get(ID);
  if M = nil then
    Exit;

  with M do
  begin
    GameX := X;
    GameY := Y;
    GameVelX := VX;
    GameVelY := VY;

    MonsterAnim := MAnim;
    MonsterTargetUID := MTarg;
    MonsterTargetTime := MTargTime;
    MonsterBehaviour := MBehav;
    MonsterSleep := MSleep;
    MonsterAmmo := MAmmo;
    SetHealth(MHealth);

    SetState(MState);
  end;
end;

procedure MC_RECV_MonsterPos(P: Pointer);
var
  M: TMonster;
  ID: Word;
begin
  ID := e_Raw_Read_Word(P);
  M := g_Monsters_Get(ID);
  if M = nil then
    Exit;

  with M do
  begin
    GameX := e_Raw_Read_LongInt(P);
    GameY := e_Raw_Read_LongInt(P);
    GameVelX := e_Raw_Read_LongInt(P);
    GameVelY := e_Raw_Read_LongInt(P);
    GameDirection := TDirection(e_Raw_Read_Byte(P));
  end;
end;

procedure MC_RECV_MonsterState(P: Pointer);
var
  ID: Integer;
  MState, MFAnm: Byte;
  M: TMonster;
  AnimRevert: Boolean;
begin
  ID := e_Raw_Read_Word(P);
  M := g_Monsters_Get(ID);
  if M = nil then Exit;

  MState := e_Raw_Read_Byte(P);
  MFAnm := e_Raw_Read_Byte(P);

  with M do
  begin
    MonsterTargetUID := e_Raw_Read_Word(P);
    MonsterTargetTime := e_Raw_Read_LongInt(P);
    MonsterSleep := e_Raw_Read_LongInt(P);
    MonsterHealth := e_Raw_Read_LongInt(P);
    MonsterAmmo := e_Raw_Read_LongInt(P);
    MonsterPain := e_Raw_Read_LongInt(P);
    AnimRevert := e_Raw_Read_Byte(P) <> 0;
    RevertAnim(AnimRevert);

    if MonsterState <> MState then
    begin
      if (MState = MONSTATE_GO) and (MonsterState = MONSTATE_SLEEP) then
        WakeUpSound;
      if (MState = MONSTATE_DIE) then
        DieSound;
      if (MState = MONSTATE_PAIN) then
        MakeBloodSimple(Min(200, MonsterPain));
      if (MState = MONSTATE_ATTACK) then
        kick(nil);
      if (MState = MONSTATE_DEAD) then
        SetDeadAnim;

      SetState(MState, MFAnm);
    end;
  end;
end;

procedure MC_RECV_MonsterShot(P: Pointer);
var
  ID: Integer;
  M: TMonster;
  X, Y, VX, VY: Integer;
begin
  ID := e_Raw_Read_Word(P);

  M := g_Monsters_Get(ID);
  if M = nil then Exit;

  X := e_Raw_Read_LongInt(P);
  Y := e_Raw_Read_LongInt(P);
  VX := e_Raw_Read_LongInt(P);
  VY := e_Raw_Read_LongInt(P);

  M.ClientAttack(X, Y, VX, VY);
end;

procedure MC_RECV_MonsterDelete(P: Pointer);
var
  ID: Integer;
  M: TMonster;
begin
  ID := e_Raw_Read_Word(P);
  M := g_Monsters_Get(ID);
  if M = nil then Exit;

  gMonsters[ID].SetState(5);
  gMonsters[ID].MonsterRemoved := True;
end;

procedure MC_RECV_VoteEvent(P: Pointer);
var
  EvID: Byte;
  Str1, Str2: string;
  Int1, Int2: SmallInt;
begin
  EvID := e_Raw_Read_Byte(P);
  Int1 := e_Raw_Read_SmallInt(P);
  Int2 := e_Raw_Read_SmallInt(P);
  Str1 := e_Raw_Read_String(P);
  Str2 := e_Raw_Read_String(P);

  case EvID of
    NET_VE_STARTED:
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_STARTED], [Str1, Str2, Int1]), True);
    NET_VE_PASSED:
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_PASSED], [Str1]), True);
    NET_VE_FAILED:
      g_Console_Add(_lc[I_MESSAGE_VOTE_FAILED], True);
    NET_VE_VOTE:
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_VOTE], [Str1, Int1, Int2]), True);
    NET_VE_INPROGRESS:
      g_Console_Add(Format(_lc[I_MESSAGE_VOTE_INPROGRESS], [Str1]), True);
  end;
end;

// CLIENT SEND

procedure MC_SEND_Info(Password: string = 'ASS');
begin
  e_Buffer_Clear(@NetOut);

  e_Buffer_Write(@NetOut, Byte(NET_MSG_INFO));
  e_Buffer_Write(@NetOut, GAME_VERSION);
  e_Buffer_Write(@NetOut, Password);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Name);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Model);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Color.R);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Color.G);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Color.B);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Team);

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;

procedure MC_SEND_Chat(Txt: string);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_CHAT));
  e_Buffer_Write(@NetOut, Txt);

  g_Net_Client_Send(True, NET_CHAN_CHAT);
end;

procedure MC_SEND_PlayerPos();
var
  kByte: Word;
begin
  if not gGameOn then Exit;
  if gPlayers = nil then Exit;

  kByte := 0;

  if (not gConsoleShow) and (not gChatShow) and (g_ActiveWindow = nil) then
   with gGameControls.P1Control do
   begin
    if (e_KeyBuffer[KeyLeft] = $080) and (e_KeyBuffer[KeyRight] <> $080) then
      P1MoveButton := 1
    else
      if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] = $080) then
        P1MoveButton := 2
      else
        if (e_KeyBuffer[KeyLeft] <> $080) and (e_KeyBuffer[KeyRight] <> $080) then
          P1MoveButton := 0;

    if (P1MoveButton = 2) and (e_KeyBuffer[KeyLeft] = $080) then
      gPlayer1.SetDirection(D_LEFT)
    else
     if (P1MoveButton = 1) and (e_KeyBuffer[KeyRight] = $080) then
        gPlayer1.SetDirection(D_RIGHT)
      else
      if P1MoveButton <> 0 then
          gPlayer1.SetDirection(TDirection(P1MoveButton-1));

    gPlayer1.ReleaseKeys;
    if P1MoveButton = 1 then
    begin
      kByte := kByte or NET_KEY_LEFT;
      if NetPredictSelf then gPlayer1.PressKey(KEY_LEFT, 10000);
    end;
    if P1MoveButton = 2 then
    begin
      kByte := kByte or NET_KEY_RIGHT;
      if NetPredictSelf then gPlayer1.PressKey(KEY_RIGHT, 10000);
    end;
    if e_KeyBuffer[KeyUp] = $080 then
    begin
      kByte := kByte or NET_KEY_UP;
      gPlayer1.PressKey(KEY_UP, 10000);
    end;
    if e_KeyBuffer[KeyDown] = $080 then
    begin
      kByte := kByte or NET_KEY_DOWN;
      gPlayer1.PressKey(KEY_DOWN, 10000);
    end;
    if e_KeyBuffer[KeyJump] = $080 then
    begin
      kByte := kByte or NET_KEY_JUMP;
      // gPlayer1.PressKey(KEY_JUMP, 10000); // TODO: Make a prediction option
    end;
    if e_KeyBuffer[KeyFire] = $080 then kByte := kByte or NET_KEY_FIRE;
    if e_KeyBuffer[KeyOpen] = $080 then kByte := kByte or NET_KEY_OPEN;
    if e_KeyBuffer[KeyNextWeapon] = $080 then kByte := kByte or NET_KEY_NW;
    if e_KeyBuffer[KeyPrevWeapon] = $080 then kByte := kByte or NET_KEY_PW;
  end
  else
   kByte := NET_KEY_CHAT;

  if (kBytePrev = kByte) and (kDirPrev = gPlayer1.Direction) then Exit;

  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRPOS));
  e_Buffer_Write(@NetOut, kByte);
  e_Buffer_Write(@NetOut, Byte(gPlayer1.Direction));
  g_Net_Client_Send(True, NET_CHAN_PLAYERPOS);

  kBytePrev := kByte;
  kDirPrev := gPlayer1.Direction
end;

procedure MC_SEND_Vote(Start: Boolean = False; Command: string = 'a');
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_VOTE_EVENT));
  e_Buffer_Write(@NetOut, Byte(Start));
  e_Buffer_Write(@NetOut, Command);
  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;

procedure MC_SEND_PlayerSettings();
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_PLRSET));
  e_Buffer_Write(@NetOut, gPlayer1Settings.Name);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Model);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Color.R);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Color.G);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Color.B);
  e_Buffer_Write(@NetOut, gPlayer1Settings.Team);

  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;

procedure MC_SEND_FullStateRequest();
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_REQFST));

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;

procedure MC_SEND_CheatRequest(Kind: Byte);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_CHEAT));
  e_Buffer_Write(@NetOut, Kind);

  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;
procedure MC_SEND_RCONPassword(Password: string = 'ASS');
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_RCON_AUTH));
  e_Buffer_Write(@NetOut, Password);

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;
procedure MC_SEND_RCONCommand(Cmd: string);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_RCON_CMD));
  e_Buffer_Write(@NetOut, Cmd);

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;

// i have no idea why all this stuff is in here

function ReadFile(const FileName: TFileName): AByte;
var
  FileStream : TFileStream;
begin
  Result := nil;
  FileStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size>0 then
    begin
      SetLength(Result, FileStream.Size);
      FileStream.Read(Result[0], FileStream.Size);
    end;
  finally
    FileStream.Free;
  end;
end;

function CreateMapDataMsg(const FileName: TFileName; ResList: TStringList): TMapDataMsg;
var
  i: Integer;
begin
  Result.MsgId := NET_MSG_MAP_RESPONSE;
  Result.FileData := ReadFile(FileName);
  Result.FileSize := Length(Result.FileData);

  SetLength(Result.ExternalResources, ResList.Count);
  for i:=0 to ResList.Count-1 do
  begin
    Result.ExternalResources[i].Name := ResList.Strings[i];
    Result.ExternalResources[i].md5 := MD5File(GameDir+'\wads\'+ResList.Strings[i]);
  end;
end;

procedure ResDataMsgToBytes(var bytes: AByte; const ResData: TResDataMsg);
var
  ResultStream: TMemoryStream;
begin
  ResultStream := TMemoryStream.Create;

  ResultStream.WriteBuffer(ResData.MsgId, SizeOf(ResData.MsgId)); //msgId
  ResultStream.WriteBuffer(ResData.FileSize, SizeOf(ResData.FileSize));  //file size
  ResultStream.WriteBuffer(ResData.FileData[0], ResData.FileSize);       //file data

  SetLength(bytes, ResultStream.Size);
  ResultStream.Seek(0, soFromBeginning);
  ResultStream.ReadBuffer(bytes[0], ResultStream.Size);

  ResultStream.Free;
end;

function ResDataFromMsgStream(msgStream: TMemoryStream):TResDataMsg;
begin
  msgStream.ReadBuffer(Result.MsgId, SizeOf(Result.MsgId));
  msgStream.ReadBuffer(Result.FileSize, SizeOf(Result.FileSize));
  SetLength(Result.FileData, Result.FileSize);
  msgStream.ReadBuffer(Result.FileData[0], Result.FileSize);
end;

procedure MapDataMsgToBytes(var bytes: AByte; const MapDataMsg: TMapDataMsg);
var
  ResultStream: TMemoryStream;
  resCount: Integer;
begin
  resCount := Length(MapDataMsg.ExternalResources);

  ResultStream := TMemoryStream.Create;

  ResultStream.WriteBuffer(MapDataMsg.MsgId, SizeOf(MapDataMsg.MsgId)); //msgId
  ResultStream.WriteBuffer(MapDataMsg.FileSize, SizeOf(MapDataMsg.FileSize));  //file size
  ResultStream.WriteBuffer(MapDataMsg.FileData[0], MapDataMsg.FileSize);       //file data

  ResultStream.WriteBuffer(resCount, SizeOf(resCount));   //res count
  ResultStream.WriteBuffer(MapDataMsg.ExternalResources[0], resCount*SizeOf(TExternalResourceInfo)); //res data

  SetLength(bytes, ResultStream.Size);
  ResultStream.Seek(0, soFromBeginning);
  ResultStream.ReadBuffer(bytes[0], ResultStream.Size);

  ResultStream.Free;
end;

function MapDataFromMsgStream(msgStream: TMemoryStream):TMapDataMsg;
var
  resCount: Integer;
begin
  msgStream.ReadBuffer(Result.MsgId, SizeOf(Result.MsgId));
  msgStream.ReadBuffer(Result.FileSize, SizeOf(Result.FileSize));   //file size

  SetLength(Result.FileData, Result.FileSize);
  msgStream.ReadBuffer(Result.FileData[0], Result.FileSize);  //file data

  msgStream.ReadBuffer(resCount, SizeOf(resCount));  //res count
  SetLength(Result.ExternalResources, resCount);

  msgStream.ReadBuffer(Result.ExternalResources[0], resCount * SizeOf(TExternalResourceInfo)); //res data
end;

// http://stackoverflow.com/questions/960772/how-can-i-sanitize-a-string-for-use-as-a-filename
function IsValidFilePath(const FileName: String): Boolean;
var
  S: String;
  I: Integer;
begin
  Result := False;
  S := FileName;
  repeat
    I := LastDelimiter('\/', S);
    MoveFile(nil, PChar(S));
    if (GetLastError = ERROR_ALREADY_EXISTS) or
       (
         (GetFileAttributes(PChar(Copy(S, I + 1, MaxInt))) = $FFFFFFFF)
         and
         (GetLastError=ERROR_INVALID_NAME)
       ) then
      Exit;
    if I>0 then
      S := Copy(S,1,I-1);
  until I = 0;
  Result := True;
end;

procedure MC_SEND_MapRequest();
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_MAP_REQUEST));
  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;

procedure MC_SEND_ResRequest(const resName: AnsiString);
begin
  e_Buffer_Write(@NetOut, Byte(NET_MSG_RES_REQUEST));
  e_Buffer_Write(@NetOut, resName);
  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;

procedure MH_RECV_MapRequest(C: pTNetClient; P: Pointer);
var
  payload: AByte;
  peer: pENetPeer;
  mapDataMsg: TMapDataMsg;
begin
  e_WriteLog('NET: Received map request from ' +
             DecodeIPV4(C.Peer.address.host), MSG_NOTIFY);

  mapDataMsg := CreateMapDataMsg(gGameSettings.WAD, gExternalResources);
  peer := NetClients[C.ID].Peer;

  MapDataMsgToBytes(payload, mapDataMsg);
  g_Net_SendData(payload, peer, True, NET_CHAN_DOWNLOAD);

  payload := nil;
  mapDataMsg.FileData := nil;
  mapDataMsg.ExternalResources := nil;
end;

procedure MH_RECV_ResRequest(C: pTNetClient; P: Pointer);
var
  payload: AByte;
  peer: pENetPeer;
  FileName: String;
  resDataMsg: TResDataMsg;
begin
  FileName := ExtractFileName(e_Raw_Read_String(P));
  e_WriteLog('NET: Received res request: ' + FileName +
             ' from ' + DecodeIPV4(C.Peer.address.host), MSG_NOTIFY);

  if not IsValidFilePath(FileName) then
  begin
    e_WriteLog('Invalid filename: ' + FileName, MSG_WARNING);
    exit;
  end;

  peer := NetClients[C.ID].Peer;

  if gExternalResources.IndexOf(FileName) > -1 then
  begin
    resDataMsg.MsgId := NET_MSG_RES_RESPONSE;
    resDataMsg.FileData := ReadFile(GameDir+'\wads\'+FileName);
    resDataMsg.FileSize := Length(resDataMsg.FileData);

    ResDataMsgToBytes(payload, resDataMsg);
    g_Net_SendData(payload, peer, True, NET_CHAN_DOWNLOAD);
  end;
end;

end.
