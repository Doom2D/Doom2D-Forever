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
unit g_netmsg;

interface

uses e_msg, g_net, g_triggers, Classes, SysUtils, md5;

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
  NET_MSG_FLAGPOS= 110;

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
  NET_MSG_IPOS   = 123;

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
  NET_MSG_TIME_SYNC  = 194;
  NET_MSG_VOTE_EVENT = 195;

  {
  NET_MSG_MAP_REQUEST = 201;
  NET_MSG_MAP_RESPONSE = 202;
  NET_MSG_RES_REQUEST = 203;
  NET_MSG_RES_RESPONSE = 204;
  }

  NET_CHAT_SYSTEM = 0;
  NET_CHAT_PLAYER = 1;
  NET_CHAT_TEAM   = 2;

  NET_RCON_NOAUTH = 0;
  NET_RCON_PWGOOD = 1;
  NET_RCON_PWBAD  = 2;

  NET_GFX_SPARK   = 1;
  NET_GFX_TELE    = 2;
  NET_GFX_RESPAWN = 3;
  NET_GFX_FIRE    = 4;
  NET_GFX_EXPLODE = 5;
  NET_GFX_BFGEXPL = 6;
  NET_GFX_BFGHIT  = 7;
  NET_GFX_SHELL1  = 8;
  NET_GFX_SHELL2  = 9;
  NET_GFX_SHELL3  = 10;

  NET_EV_MAPSTART     = 1;
  NET_EV_MAPEND       = 2;
  NET_EV_CHANGE_TEAM  = 3;
  NET_EV_PLAYER_KICK  = 4;
  NET_EV_PLAYER_BAN   = 5;
  NET_EV_LMS_WARMUP   = 6;
  NET_EV_LMS_SURVIVOR = 7;
  NET_EV_RCON         = 8;
  NET_EV_BIGTEXT      = 9;
  NET_EV_SCORE        = 10;
  NET_EV_SCORE_MSG    = 11;
  NET_EV_LMS_START    = 12;
  NET_EV_LMS_WIN      = 13;
  NET_EV_TLMS_WIN     = 14;
  NET_EV_LMS_LOSE     = 15;
  NET_EV_LMS_DRAW     = 16;
  NET_EV_KILLCOMBO    = 17;
  NET_EV_PLAYER_TOUCH = 18;
  NET_EV_SECRET       = 19;
  NET_EV_INTER_READY  = 20;
  NET_EV_LMS_NOSPAWN  = 21;

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
  NET_CHEAT_READY    = 3;
  NET_CHEAT_DROPFLAG = 4;

  NET_MAX_DIFFTIME = 5000 div 36;

// HOST MESSAGES

procedure MH_MalformedPacket(C: pTNetClient);
procedure MH_ProcessFirstSpawn (C: pTNetClient);

procedure MH_RECV_Info(C: pTNetClient; var M: TMsg);
procedure MH_RECV_Chat(C: pTNetClient; var M: TMsg);
procedure MH_RECV_FullStateRequest(C: pTNetClient; var M: TMsg);
function  MH_RECV_PlayerPos(C: pTNetClient; var M: TMsg): Word;
procedure MH_RECV_PlayerSettings(C: pTNetClient; var M: TMsg);
procedure MH_RECV_CheatRequest(C: pTNetClient; var M: TMsg);
procedure MH_RECV_RCONPassword(C: pTNetClient; var M: TMsg);
procedure MH_RECV_RCONCommand(C: pTNetClient; var M: TMsg);
//procedure MH_RECV_MapRequest(C: pTNetClient; var M: TMsg);
//procedure MH_RECV_ResRequest(C: pTNetClient; var M: TMsg);
procedure MH_RECV_Vote(C: pTNetClient; var M: TMsg);

// GAME
procedure MH_SEND_Everything(CreatePlayers: Boolean {= False}; ID: Integer {= NET_EVERYONE});
procedure MH_SEND_Info(ID: Byte);
procedure MH_SEND_Chat(Txt: string; Mode: Byte; ID: Integer = NET_EVERYONE);
procedure MH_SEND_Effect(X, Y: Integer; Ang: SmallInt; Kind: Byte; ID: Integer = NET_EVERYONE);
procedure MH_SEND_Sound(X, Y: Integer; Name: string; Pos: Boolean = True; ID: Integer = NET_EVERYONE);
procedure MH_SEND_CreateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
procedure MH_SEND_UpdateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
procedure MH_SEND_DeleteShot(Proj: LongInt; X, Y: LongInt; Loud: Boolean = True; ID: Integer = NET_EVERYONE);
procedure MH_SEND_GameStats(ID: Integer = NET_EVERYONE);
procedure MH_SEND_CoopStats(ID: Integer = NET_EVERYONE);
procedure MH_SEND_GameEvent(EvType: Byte; EvNum: Integer = 0; EvStr: string = 'N'; ID: Integer = NET_EVERYONE);
procedure MH_SEND_FlagEvent(EvType: Byte; Flag: Byte; PID: Word; Quiet: Boolean = False; ID: Integer = NET_EVERYONE);
procedure MH_SEND_FlagPos(Flag: Byte; ID: Integer = NET_EVERYONE);
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
procedure MH_SEND_ItemPos(IID: Word; ID: Integer = NET_EVERYONE);
// PANEL
procedure MH_SEND_PanelTexture(PGUID: Integer; AnimLoop: Byte; ID: Integer = NET_EVERYONE);
procedure MH_SEND_PanelState(PGUID: Integer; ID: Integer = NET_EVERYONE);
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
procedure MH_SEND_TimeSync(Time: LongWord; ID: Integer = NET_EVERYONE);
procedure MH_SEND_VoteEvent(EvType: Byte;
                            StrArg1: string = 'a'; StrArg2: string = 'b';
                            IntArg1: SmallInt = 0; IntArg2: SmallInt = 0;
                            ID: Integer = NET_EVERYONE);

// CLIENT MESSAGES //

// GAME
procedure MC_RECV_Chat(var M: TMsg);
procedure MC_RECV_Effect(var M: TMsg);
procedure MC_RECV_Sound(var M: TMsg);
procedure MC_RECV_GameStats(var M: TMsg);
procedure MC_RECV_CoopStats(var M: TMsg);
procedure MC_RECV_GameEvent(var M: TMsg);
procedure MC_RECV_FlagEvent(var M: TMsg);
procedure MC_RECV_FlagPos(var M: TMsg);
procedure MC_RECV_GameSettings(var M: TMsg);
// PLAYER
function  MC_RECV_PlayerCreate(var M: TMsg): Word;
function  MC_RECV_PlayerPos(var M: TMsg): Word;
function  MC_RECV_PlayerStats(var M: TMsg): Word;
function  MC_RECV_PlayerDelete(var M: TMsg): Word;
function  MC_RECV_PlayerDamage(var M: TMsg): Word;
function  MC_RECV_PlayerDeath(var M: TMsg): Word;
function  MC_RECV_PlayerFire(var M: TMsg): Word;
procedure MC_RECV_PlayerSettings(var M: TMsg);
// ITEM
procedure MC_RECV_ItemSpawn(var M: TMsg);
procedure MC_RECV_ItemDestroy(var M: TMsg);
procedure MC_RECV_ItemPos(var M: TMsg);
// PANEL
procedure MC_RECV_PanelTexture(var M: TMsg);
procedure MC_RECV_PanelState(var M: TMsg);
// MONSTER
procedure MC_RECV_MonsterSpawn(var M: TMsg);
procedure MC_RECV_MonsterPos(var M: TMsg);
procedure MC_RECV_MonsterState(var M: TMsg);
procedure MC_RECV_MonsterShot(var M: TMsg);
procedure MC_RECV_MonsterDelete(var M: TMsg);
// SHOT
procedure MC_RECV_CreateShot(var M: TMsg);
procedure MC_RECV_UpdateShot(var M: TMsg);
procedure MC_RECV_DeleteShot(var M: TMsg);
// TRIGGER
procedure MC_RECV_TriggerSound(var M: TMsg);
procedure MC_RECV_TriggerMusic(var M: TMsg);
// MISC
procedure MC_RECV_TimeSync(var M: TMsg);
procedure MC_RECV_VoteEvent(var M: TMsg);
// SERVICE
procedure MC_SEND_Info(Password: string);
procedure MC_SEND_Chat(Txt: string; Mode: Byte);
procedure MC_SEND_PlayerPos();
procedure MC_SEND_FullStateRequest();
procedure MC_SEND_PlayerSettings();
procedure MC_SEND_CheatRequest(Kind: Byte);
procedure MC_SEND_RCONPassword(Password: string);
procedure MC_SEND_RCONCommand(Cmd: string);
procedure MC_SEND_Vote(Start: Boolean = False; Command: string = 'a');
// DOWNLOAD
//procedure MC_SEND_MapRequest();
//procedure MC_SEND_ResRequest(const resName: AnsiString);


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

function IsValidFileName(const S: String): Boolean;
function IsValidFilePath(const S: String): Boolean;


implementation

uses
  Math, ENet, e_input, e_log, g_base, g_basic, r_animations, r_gfx,
  g_textures, g_gfx, g_sound, g_console, g_options,
  g_game, g_player, g_map, g_panel, g_items, g_weapons, g_phys, g_gui,
  g_language, g_monsters, g_netmaster, utils, wadreader, MAPDEF;

const
  NET_KEY_LEFT     = 1 shl 0;
  NET_KEY_RIGHT    = 1 shl 1;
  NET_KEY_UP       = 1 shl 2;
  NET_KEY_DOWN     = 1 shl 3;
  NET_KEY_JUMP     = 1 shl 4;
  NET_KEY_FIRE     = 1 shl 5;
  NET_KEY_OPEN     = 1 shl 6;
  NET_KEY_CHAT     = 1 shl 7;
  NET_KEY_FORCEDIR = 1 shl 8;

//var
  //kBytePrev: Word = 0;
  //kDirPrev: TDirection = D_LEFT;
  //HostGameTime: Word = 0;


function IsValidFileName(const S: String): Boolean;
const
  Forbidden: set of Char = ['<', '>', '|', '"', ':', '*', '?'];
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    Result := Result and (not(S[I] in Forbidden));
end;

function IsValidFilePath(const S: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not IsValidFileName(S) then exit;
  if FileExists(S) then exit;
  I := LastDelimiter('\/', S);
  if (I > 0) then
    if (not DirectoryExists(Copy(S, 1, I-1))) then
      exit;
  Result := True;
end;


// HOST MESSAGES //


// GAME

procedure MH_MalformedPacket(C: pTNetClient);
begin
  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
    _lc[I_NET_DISC_PROTOCOL]);
  g_Net_Host_Kick(C^.ID, NET_DISC_PROTOCOL);
end;

procedure MH_RECV_Chat(C: pTNetClient; var M: TMsg);
var
  Txt: string;
  Mode: Byte;
  PID: Word;
  Pl: TPlayer;
  Err: Boolean;
begin
  PID := C^.Player;
  Pl := g_Player_Get(PID);

  Err := False;
  try
    Txt := M.ReadString();
    Mode := M.ReadByte();
  except
    Err := True;
  end;

  if Err then begin MH_MalformedPacket(C); Exit; end;

  if (Mode = NET_CHAT_SYSTEM) then
    Mode := NET_CHAT_PLAYER; // prevent sending system messages from clients
  if (Mode = NET_CHAT_TEAM) and (not gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
    Mode := NET_CHAT_PLAYER; // revert to player chat in non-team game

  if Pl = nil then
    MH_SEND_Chat(Txt, Mode)
  else
    MH_SEND_Chat(Pl.Name + ': ' + Txt, Mode, IfThen(Mode = NET_CHAT_TEAM, Pl.Team, NET_EVERYONE));
end;

procedure MH_RECV_Info(C: pTNetClient; var M: TMsg);
var
  Ver, PName, Model, Pw: string;
  R, G, B, T: Byte;
  WeapSwitch: Byte;
  TmpPrefArray: Array [WP_FIRST .. WP_LAST + 1] of Byte;
  SwitchEmpty: Byte;
  SkipF: Byte;
  PID: Word;
  Color: TRGB;
  I: Integer;
  Err: Boolean;
begin
  Err := False;
  try
    Ver := M.ReadString();
    Pw := M.ReadString();
    PName := M.ReadString();
    Model := M.ReadString();
    R := M.ReadByte();
    G := M.ReadByte();
    B := M.ReadByte();
    T := M.ReadByte();
    WeapSwitch := M.ReadByte();
    for I := WP_FIRST to WP_LAST + 1 do
      TmpPrefArray[I] := M.ReadByte();
    SwitchEmpty := M.ReadByte();
    SkipF := M.ReadByte();
  except
    Err := True;
  end;

  if Err then begin MH_MalformedPacket(C); Exit; end;

  if Ver <> GAME_VERSION then
  begin
    g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
      _lc[I_NET_DISC_VERSION]);
    g_Net_Host_Kick(C^.ID, NET_DISC_VERSION);
    Exit;
  end;

  if g_Net_IsHostBanned(C^.Peer^.address.host) then
  begin
    if g_Net_IsHostBanned(C^.Peer^.address.host, True) then
    begin
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
        _lc[I_NET_DISC_BAN]);
      g_Net_Host_Kick(C^.ID, NET_DISC_BAN);
    end
    else
    begin
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
        _lc[I_NET_DISC_BAN]);
      g_Net_Host_Kick(C^.ID, NET_DISC_TEMPBAN);
    end;
    Exit;
  end;

  if NetPassword <> '' then
    if AnsiLowerCase(NetPassword) <> AnsiLowerCase(Pw) then
    begin
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
        _lc[I_NET_DISC_PASSWORD]);
      g_Net_Host_Kick(C^.ID, NET_DISC_PASSWORD);
      Exit;
    end;

  if (C^.Player <> 0) then
  begin
    // already received info
    g_Net_Penalize(C, 'client info spam');
    Exit;
  end;

  Color.R := R;
  Color.B := B;
  Color.G := G;

  PID := g_Player_Create(Model, Color, T, False);
  with g_Player_Get(PID) do
  begin
    Name := PName;
    WeapSwitchMode := WeapSwitch;
    SetWeaponPrefs(TmpPrefArray);
    SwitchToEmpty := SwitchEmpty;
    SkipFist := SkipF;
    if (g_Force_Model_Get() <> 0) then
      SetModel(g_Forced_Model_GetName());
    Reset(True);
  end;

  C^.Player := PID;
  C^.WaitForFirstSpawn := false;
  C^.AuthTime := 0;

  g_Console_Add(Format(_lc[I_PLAYER_JOIN], [PName]), True);
  e_WriteLog('NET: Client ' + PName + ' [' + IntToStr(C^.ID) +
             '] connected. Assigned player #' + IntToStr(PID) + '.', TMsgType.Notify);

  MH_SEND_Info(C^.ID);

  with g_Player_Get(PID) do
  begin
    Name := PName;
    FClientID := C^.ID;
    // round in progress, don't spawn
    e_LogWritefln('*** client #%u (cid #%u) authenticated...', [C.ID, C.Player]);
    //e_LogWritefln('spawning player with pid #%u...', [PID]);
    //Respawn(gGameSettings.GameType = GT_SINGLE);
    //k8: no, do not spawn a player yet, wait for "request full state" packet
    Lives := 0;
    Spectate;
    FNoRespawn := True;
    // `FWantsInGame` seems to mean "spawn the player on the next occasion".
    // that is, if we'll set it to `true`, the player can be spawned after
    // warmup time ran out, for example, regardless of the real player state.
    // also, this seems to work only for the initial connection. further
    // map changes could initiate resource downloading, but the player will
    // be spawned immediately.
    // the proper solution will require another player state, "ephemeral".
    // the player should start any map in "ephemeral" state, and turned into
    // real mobj only when they sent a special "i am ready" packet. this packet
    // must be sent after receiving the full state, so the player will get a full
    // map view before going into game.
    FWantsInGame := false;
    C^.WaitForFirstSpawn := true;
  end;

  //if not C^.WaitForFirstSpawn then
  begin
    for I := Low(NetClients) to High(NetClients) do
    begin
      if NetClients[I].ID = C^.ID then Continue;
      MH_SEND_PlayerCreate(PID, NetClients[I].ID);
      MH_SEND_PlayerPos(True, PID, NetClients[I].ID);
      MH_SEND_PlayerStats(PID, NetClients[I].ID);
    end;
  end;

  if gState in [STATE_INTERCUSTOM, STATE_FOLD] then
    MH_SEND_GameEvent(NET_EV_MAPEND, 0, 'N', C^.ID);

  if NetUseMaster then
  begin
    //g_Net_Slist_Update;
    g_Net_Slist_Pulse();
  end;
end;


procedure MH_ProcessFirstSpawn (C: pTNetClient);
var
  plr: TPlayer;
begin
  if not C.WaitForFirstSpawn then exit;
  plr := g_Player_Get(C^.Player);
  if not assigned(plr) then exit;
  g_Net_Slist_ServerPlayerComes();
  e_LogWritefln('*** client #%u (cid #%u) first spawn', [C.ID, C.Player]);
  C.WaitForFirstSpawn := false;
  plr.FNoRespawn := false;
  plr.FWantsInGame := true; // TODO: look into this later

  if (gGameSettings.MaxLives > 0) and (gLMSRespawn = LMS_RESPAWN_NONE) then
  begin
    plr.Spectate;
    MH_SEND_GameEvent(NET_EV_LMS_NOSPAWN, 0, 'N', C.ID);
  end
  else
  begin
    plr.Respawn(False);
    if gLMSRespawn > LMS_RESPAWN_NONE then
      MH_SEND_GameEvent(NET_EV_LMS_WARMUP, gLMSRespawnTime - gTime, 'N', C.ID);
  end;
end;


procedure MH_RECV_FullStateRequest(C: pTNetClient; var M: TMsg);
begin
  //e_LogWritefln('*** client #%u (cid #%u) full state request', [C.ID, C.Player]);

  if C^.FullUpdateSent then
  begin
    // FullStateRequest spam?
    g_Net_Penalize(C, 'duplicate full state request');
    exit;
  end;

  if gGameOn then
  begin
    MH_SEND_Everything((C^.State = NET_STATE_AUTH), C^.ID)
  end
  else
  begin
    C^.RequestedFullUpdate := True;
  end;
end;

// PLAYER

function  MH_RECV_PlayerPos(C: pTNetClient; var M: TMsg): Word;
var
  Dir, i: Byte;
  WeaponAct: Byte;
  WeaponSelect: Word;
  PID: Word;
  kByte: Word;
  Pl: TPlayer;
  GT: LongWord;
  Err: Boolean;
begin
  Result := 0;
  Err := False;
  if not gGameOn then Exit;

  try
    GT := M.ReadLongWord();
  except
    Err := True;
  end;

  if Err then begin MH_MalformedPacket(C); Exit; end;

  PID := C^.Player;
  Pl := g_Player_Get(PID);
  if Pl = nil then
    Exit;

  if (GT > gTime + NET_MAX_DIFFTIME) or (GT < Pl.NetTime) then Exit;

  with Pl do
  begin
    NetTime := GT;
    try
      kByte := M.ReadWord();
      Dir := M.ReadByte();
      WeaponAct := M.ReadByte();
      WeaponSelect := M.ReadWord();
    except
      Err := True;
    end;

    if Err then begin MH_MalformedPacket(C); Exit; end;

    //e_WriteLog(Format('R:ws=%d', [WeaponSelect]), MSG_WARNING);
    if Direction <> TDirection(Dir) then
      JustTeleported := False;

    SetDirection(TDirection(Dir));
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

    for i := 0 to 7 do
    begin
      if (WeaponAct and Byte(1 shl i)) <> 0 then
      begin
        //e_WriteLog(Format(' R:wn=%d', [i]), MSG_WARNING);
        ProcessWeaponAction(i);
      end;
    end;

    for i := 0 to 15 do
    begin
      if (WeaponSelect and Word(1 shl i)) <> 0 then
      begin
        //e_WriteLog(Format(' R:wn=%d', [i]), MSG_WARNING);
        QueueWeaponSwitch(i);
      end;
    end;
  end;

  // MH_SEND_PlayerPos(False, PID, C^.ID);
end;

procedure MH_RECV_CheatRequest(C: pTNetClient; var M: TMsg);
var
  CheatKind: Byte;
  Pl: TPlayer;
  Err: Boolean;
begin
  Err := False;
  Pl := g_Player_Get(C^.Player);
  if Pl = nil then Exit;

  try
    CheatKind := M.ReadByte();
  except
    Err := True;
  end;

  if Err then begin MH_MalformedPacket(C); Exit; end;

  case CheatKind of
    NET_CHEAT_SUICIDE:
      Pl.Damage(SUICIDE_DAMAGE, Pl.UID, 0, 0, HIT_SELF);
    NET_CHEAT_SPECTATE:
    begin
      if Pl.FSpectator then
      begin
        if (gGameSettings.MaxLives = 0) or (gLMSRespawn > LMS_RESPAWN_NONE) then
          Pl.Respawn(False)
        else
          MH_SEND_GameEvent(NET_EV_LMS_NOSPAWN, Pl.UID);
      end
      else
        Pl.Spectate;
    end;
    NET_CHEAT_READY:
    begin
      if gState <> STATE_INTERCUSTOM then Exit;
      Pl.FReady := not Pl.FReady;
      if Pl.FReady then
      begin
        MH_SEND_GameEvent(NET_EV_INTER_READY, Pl.UID, 'Y');
        Inc(gInterReadyCount);
      end
      else
      begin
        MH_SEND_GameEvent(NET_EV_INTER_READY, Pl.UID, 'N');
        Dec(gInterReadyCount);
      end;
    end;
    NET_CHEAT_DROPFLAG:
      Pl.TryDropFlag();
  end;
end;

procedure MH_RECV_PlayerSettings(C: pTNetClient; var M: TMsg);
var
  TmpName: string;
  TmpModel: string;
  TmpColor: TRGB;
  TmpTeam: Byte;
  TmpWeapSwitch: Byte;
  TmpPrefArray: Array [WP_FIRST .. WP_LAST + 1] of Byte;
  TmpSwEmpty: Byte;
  TmpSkipF: Byte;
  I: Integer;
  Pl: TPlayer;
  Err: Boolean;
begin
  Err := False;
  try
    TmpName := M.ReadString();
    TmpModel := M.ReadString();
    TmpColor.R := M.ReadByte();
    TmpColor.G := M.ReadByte();
    TmpColor.B := M.ReadByte();
    TmpTeam := M.ReadByte();
    TmpWeapSwitch := M.ReadByte();
    for I := WP_FIRST to WP_LAST + 1 do
      TmpPrefArray[I] := M.ReadByte();
    TmpSwEmpty := M.ReadByte();
    TmpSkipF := M.ReadByte();
  except
    Err := True;
  end;

  if Err then begin MH_MalformedPacket(C); Exit; end;

  Pl := g_Player_Get(C^.Player);
  if Pl = nil then Exit;

  if (gGameSettings.GameMode in [GM_TDM, GM_CTF]) and (Pl.Team <> TmpTeam) then
    Pl.SwitchTeam
  else
    Pl.SetColor(TmpColor);

  if Pl.Name <> TmpName then
  begin
    g_Console_Add(Format(_lc[I_PLAYER_NAME], [Pl.Name, TmpName]), True);
    Pl.Name := TmpName;
  end;

  if (g_Force_Model_Get() <> 0) then
    TmpModel := g_Forced_Model_GetName();
  if TmpModel <> Pl.Model.Name then
    Pl.SetModel(TmpModel);

  if (TmpWeapSwitch <> Pl.WeapSwitchMode) then
    Pl.WeapSwitchMode := TmpWeapSwitch;

  Pl.SetWeaponPrefs(TmpPrefArray);
  if (TmpSwEmpty <> Pl.SwitchToEmpty) then
    Pl.SwitchToEmpty := TmpSwEmpty;

  if (TmpSkipF <> Pl.SkipFist) then
    Pl.SkipFist := TmpSkipF;

  MH_SEND_PlayerSettings(Pl.UID, TmpModel);
end;

// RCON

procedure MH_RECV_RCONPassword(C: pTNetClient; var M: TMsg);
var
  Pwd: string;
  Err: Boolean;
begin
  Err := False;
  try
    Pwd := M.ReadString();
  except
    Err := True;
  end;
  if Err then begin MH_MalformedPacket(C); Exit; end;
  if not NetAllowRCON then Exit;
  if Pwd = NetRCONPassword then
  begin
    C^.RCONAuth := True;
    MH_SEND_GameEvent(NET_EV_RCON, NET_RCON_PWGOOD, 'N', C^.ID);
  end
  else
    MH_SEND_GameEvent(NET_EV_RCON, NET_RCON_PWBAD, 'N', C^.ID);
end;

procedure MH_RECV_RCONCommand(C: pTNetClient; var M: TMsg);
var
  Cmd: string;
  Err: Boolean;
begin
  Err := False;
  try
    Cmd := M.ReadString();
  except
    Err := True;
  end;
  if Err then begin MH_MalformedPacket(C); Exit; end;
  if not NetAllowRCON then Exit;
  if not C^.RCONAuth then
  begin
    MH_SEND_GameEvent(NET_EV_RCON, NET_RCON_NOAUTH, 'N', C^.ID);
    Exit;
  end;
  g_Console_Process(Cmd);
end;

// MISC

procedure MH_RECV_Vote(C: pTNetClient; var M: TMsg);
var
  Start: Boolean;
  Name, Command: string;
  Need: Integer;
  Pl: TPlayer;
  Err: Boolean;
begin
  Err := False;
  try
    Start := M.ReadByte() <> 0;
    Command := M.ReadString();
  except
    Err := True;
  end;

  if Err then begin MH_MalformedPacket(C); Exit; end;

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
    if (gPlayer1 <> nil) or (gPlayer2 <> nil) then
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

procedure MH_SEND_Everything(CreatePlayers: Boolean {= False}; ID: Integer {= NET_EVERYONE});

  function sendItemRespawn (it: PItem): Boolean;
  begin
    result := false; // don't stop
    MH_SEND_ItemSpawn(True, it.myid, ID);
  end;

  function sendMonSpawn (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    MH_SEND_MonsterSpawn(mon.UID, ID);
  end;

  function sendPanelState (pan: TPanel): Boolean;
  begin
    result := false; // don't stop
    MH_SEND_PanelState(pan.guid, ID); // anyway, to sync mplats
    if (pan.CanChangeTexture) then MH_SEND_PanelTexture(pan.guid, pan.LastAnimLoop, ID);
  end;

var
  I: Integer;
begin
  if (ID < 0) or (ID >= Length(NetClients)) then
    exit; // bogus client, this shouldn't happen

  NetClients[ID].FullUpdateSent := True;

  e_LogWritefln('*** client #%u (cid #%u) will get everything', [ID, NetClients[ID].Player]);

  MH_ProcessFirstSpawn(@NetClients[ID]);

  if gPlayers <> nil then
  begin
    for I := Low(gPlayers) to High(gPlayers) do
    begin
      if gPlayers[I] <> nil then
      begin
        if CreatePlayers then MH_SEND_PlayerCreate(gPlayers[I].UID, ID);
        MH_SEND_PlayerPos(True, gPlayers[I].UID, ID);
        MH_SEND_PlayerStats(gPlayers[I].UID, ID);

        if (gPlayers[I].Flag <> FLAG_NONE) and (gGameSettings.GameMode = GM_CTF) then
        begin
          MH_SEND_FlagEvent(FLAG_STATE_CAPTURED, gPlayers[I].Flag, gPlayers[I].UID, True, ID);
        end;
      end;
    end;
  end;

  g_Items_ForEachAlive(sendItemRespawn, true); // backwards
  g_Mons_ForEach(sendMonSpawn);
  g_Map_ForEachPanel(sendPanelState);

  if gTriggers <> nil then
  begin
    for I := Low(gTriggers) to High(gTriggers) do
    begin
      if gTriggers[I].TriggerType = TRIGGER_SOUND then
      begin
        MH_SEND_TriggerSound(gTriggers[I], ID);
      end;
    end;
  end;

  if Shots <> nil then
  begin
    for I := Low(Shots) to High(Shots) do
    begin
      if Shots[i].ShotType in [6, 7, 8] then
      begin
        MH_SEND_CreateShot(i, ID);
      end;
    end;
  end;

  MH_SEND_TriggerMusic(ID);

  MH_SEND_GameStats(ID);
  MH_SEND_CoopStats(ID);

  if gGameSettings.GameMode = GM_CTF then
  begin
    if gFlags[FLAG_RED].State <> FLAG_STATE_CAPTURED then MH_SEND_FlagEvent(gFlags[FLAG_RED].State, FLAG_RED, 0, True, ID);
    if gFlags[FLAG_BLUE].State <> FLAG_STATE_CAPTURED then MH_SEND_FlagEvent(gFlags[FLAG_BLUE].State, FLAG_BLUE, 0, True, ID);
  end;

  if CreatePlayers and (ID >= 0) then NetClients[ID].State := NET_STATE_GAME;

  g_Net_Flush();
end;

procedure MH_SEND_Info(ID: Byte);
begin
  NetOut.Clear();

  NetOut.Write(Byte(NET_MSG_INFO));
  NetOut.Write(ID);
  NetOut.Write(NetClients[ID].Player);
  NetOut.Write(ExtractFileName(gGameSettings.WAD));
  NetOut.Write(g_ExtractFileName(gMapInfo.Map));
  NetOut.Write(gWADHash);
  NetOut.Write(gGameSettings.GameMode);
  NetOut.Write(gGameSettings.ScoreLimit);
  NetOut.Write(gGameSettings.TimeLimit);
  NetOut.Write(gGameSettings.MaxLives);
  NetOut.Write(gGameSettings.Options);
  NetOut.Write(gTime);

  g_Net_Host_Send(ID, True, NET_CHAN_SERVICE);
end;

procedure MH_SEND_Chat(Txt: string; Mode: Byte; ID: Integer = NET_EVERYONE);
var
  Name: string;
  i: Integer;
  Team: Byte;
begin
  if (Mode = NET_CHAT_TEAM) and (not gGameSettings.GameMode in [GM_TDM, GM_CTF]) then
    Mode := NET_CHAT_PLAYER;

  Team := 0;
  if (Mode = NET_CHAT_TEAM) then
  begin
    for i := Low(gPlayers) to High(gPlayers) do
      if (gPlayers[i] <> nil) and (gPlayers[i].FClientID >= 0) and
         (gPlayers[i].Team = ID) then
      begin
        NetOut.Write(Byte(NET_MSG_CHAT));
        NetOut.Write(Txt);
        NetOut.Write(Mode);
        g_Net_Host_Send(gPlayers[i].FClientID, True, NET_CHAN_CHAT);
      end;
    Team := ID;
    ID := NET_EVERYONE;
  end
  else
  begin
    NetOut.Write(Byte(NET_MSG_CHAT));
    NetOut.Write(Txt);
    NetOut.Write(Mode);
    g_Net_Host_Send(ID, True, NET_CHAN_CHAT);
  end;

  if Mode = NET_CHAT_SYSTEM then
    Exit;

  if ID = NET_EVERYONE then
  begin
    if Mode = NET_CHAT_PLAYER then
    begin
      g_Console_Add(Txt, True);
      e_WriteLog('[Chat] ' + b_Text_Unformat(Txt), TMsgType.Notify);
      g_Game_ChatSound(b_Text_Unformat(Txt));
    end
    else
    if Mode = NET_CHAT_TEAM then
      if gPlayer1 <> nil then
      begin
        if (gPlayer1.Team = TEAM_RED) and (Team = TEAM_RED) then
        begin
          g_Console_Add(#18'[Team] '#2 + Txt, True);
          e_WriteLog('[Team Chat] ' + b_Text_Unformat(Txt), TMsgType.Notify);
          g_Game_ChatSound(b_Text_Unformat(Txt));
        end
        else if (gPlayer1.Team = TEAM_BLUE) and (Team = TEAM_BLUE) then
        begin
          g_Console_Add(#20'[Team] '#2 + Txt, True);
          e_WriteLog('[Team Chat] ' + b_Text_Unformat(Txt), TMsgType.Notify);
          g_Game_ChatSound(b_Text_Unformat(Txt));
        end;
      end;
  end
  else
  begin
    Name := g_Net_ClientName_ByID(ID);
    g_Console_Add('-> ' + Name + ': ' + Txt, True);
    e_WriteLog('[Tell ' + Name + '] ' + b_Text_Unformat(Txt), TMsgType.Notify);
    g_Game_ChatSound(b_Text_Unformat(Txt), False);
  end;
end;

procedure MH_SEND_Effect(X, Y: Integer; Ang: SmallInt; Kind: Byte; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_GFX));
  NetOut.Write(Kind);
  NetOut.Write(X);
  NetOut.Write(Y);
  NetOut.Write(Ang);

  g_Net_Host_Send(ID, False, NET_CHAN_GAME);
end;

procedure MH_SEND_Sound(X, Y: Integer; Name: string; Pos: Boolean = True; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_SND));
  NetOut.Write(Name);
  if Pos then
  begin
    NetOut.Write(Byte(1));
    NetOut.Write(X);
    NetOut.Write(Y);
  end
  else
    NetOut.Write(Byte(0));

  g_Net_Host_Send(ID, False, NET_CHAN_GAME);
end;

procedure MH_SEND_CreateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
begin
  if (Shots = nil) or (Proj < 0) or (Proj > High(Shots)) then Exit;

  NetOut.Write(Byte(NET_MSG_SHADD));
  NetOut.Write(Proj);
  NetOut.Write(Shots[Proj].ShotType);
  NetOut.Write(Shots[Proj].Target);
  NetOut.Write(Shots[Proj].SpawnerUID);
  NetOut.Write(Shots[Proj].Timeout);
  NetOut.Write(Shots[Proj].Obj.X);
  NetOut.Write(Shots[Proj].Obj.Y);
  NetOut.Write(Shots[Proj].Obj.Vel.X);
  NetOut.Write(Shots[Proj].Obj.Vel.Y);

  g_Net_Host_Send(ID, True, NET_CHAN_SHOTS);
end;

procedure MH_SEND_UpdateShot(Proj: LongInt; ID: Integer = NET_EVERYONE);
begin
  if (Shots = nil) or (Proj < 0) or (Proj > High(Shots)) then Exit;

  NetOut.Write(Byte(NET_MSG_SHPOS));
  NetOut.Write(Proj);
  NetOut.Write(Shots[Proj].Obj.X);
  NetOut.Write(Shots[Proj].Obj.Y);
  NetOut.Write(Shots[Proj].Obj.Vel.X);
  NetOut.Write(Shots[Proj].Obj.Vel.Y);

  g_Net_Host_Send(ID, False, NET_CHAN_SHOTS);
end;

procedure MH_Send_DeleteShot(Proj: LongInt; X, Y: LongInt; Loud: Boolean = True; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_SHDEL));
  NetOut.Write(Proj);
  NetOut.Write(Byte(Loud));
  NetOut.Write(X);
  NetOut.Write(Y);

  g_Net_Host_Send(ID, True, NET_CHAN_SHOTS);
end;

procedure MH_SEND_GameStats(ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_SCORE));
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
    NetOut.Write(gTeamStat[TEAM_RED].Score);
    NetOut.Write(gTeamStat[TEAM_BLUE].Score);
  end
  else
    if gGameSettings.GameMode = GM_COOP then
    begin
      NetOut.Write(gCoopMonstersKilled);
      NetOut.Write(gCoopSecretsFound);
    end;

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_CoopStats(ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_COOP));
  NetOut.Write(gTotalMonsters);
  NetOut.Write(gSecretsCount);
  NetOut.Write(gCoopTotalMonstersKilled);
  NetOut.Write(gCoopTotalSecretsFound);
  NetOut.Write(gCoopTotalMonsters);
  NetOut.Write(gCoopTotalSecrets);
end;

procedure MH_SEND_GameEvent(EvType: Byte; EvNum: Integer = 0; EvStr: string = 'N'; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_GEVENT));
  NetOut.Write(EvType);
  NetOut.Write(EvNum);
  NetOut.Write(EvStr);
  NetOut.Write(Byte(gLastMap));
  NetOut.Write(gTime);
  if (EvType = NET_EV_MAPSTART) and isWadPath(EvStr) then
  begin
    NetOut.Write(Byte(1));
    NetOut.Write(gWADHash);
  end else
    NetOut.Write(Byte(0));

  g_Net_Host_Send(ID, True, NET_CHAN_SERVICE);
end;

procedure MH_SEND_FlagEvent(EvType: Byte; Flag: Byte; PID: Word; Quiet: Boolean = False; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_FLAG));
  NetOut.Write(EvType);
  NetOut.Write(Flag);
  NetOut.Write(Byte(Quiet));
  NetOut.Write(PID);
  NetOut.Write(gFlags[Flag].State);
  NetOut.Write(gFlags[Flag].CaptureTime);
  NetOut.Write(gFlags[Flag].Obj.X);
  NetOut.Write(gFlags[Flag].Obj.Y);
  NetOut.Write(gFlags[Flag].Obj.Vel.X);
  NetOut.Write(gFlags[Flag].Obj.Vel.Y);
  NetOut.Write(Byte(gFlags[Flag].Direction));

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_FlagPos(Flag: Byte; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_FLAGPOS));
  NetOut.Write(Flag);
  NetOut.Write(gFlags[Flag].Obj.X);
  NetOut.Write(gFlags[Flag].Obj.Y);
  NetOut.Write(gFlags[Flag].Obj.Vel.X);
  NetOut.Write(gFlags[Flag].Obj.Vel.Y);

  g_Net_Host_Send(ID, False, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_GameSettings(ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_GSET));
  NetOut.Write(gGameSettings.GameMode);
  NetOut.Write(gGameSettings.ScoreLimit);
  NetOut.Write(gGameSettings.TimeLimit);
  NetOut.Write(gGameSettings.MaxLives);
  NetOut.Write(gGameSettings.Options);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

// PLAYER (SEND)

procedure MH_SEND_PlayerCreate(PID: Word; ID: Integer = NET_EVERYONE);
var
  P: TPlayer;
begin
  P := g_Player_Get(PID);
  if P = nil then Exit;

  NetOut.Write(Byte(NET_MSG_PLR));
  NetOut.Write(PID);
  NetOut.Write(P.Name);

  NetOut.Write(P.FActualModelName);
  NetOut.Write(P.FColor.R);
  NetOut.Write(P.FColor.G);
  NetOut.Write(P.FColor.B);
  NetOut.Write(P.Team);

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

  NetOut.Write(Byte(NET_MSG_PLRPOS));
  NetOut.Write(gTime);
  NetOut.Write(PID);

  kByte := 0;

  with Pl do
  begin
    NetOut.Write(FPing);
    NetOut.Write(FLoss);
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

    if JustTeleported then kByte := kByte or NET_KEY_FORCEDIR;

    NetOut.Write(kByte);
    if Direction = TDirection.D_LEFT then NetOut.Write(Byte(0)) else NetOut.Write(Byte(1));
    NetOut.Write(GameX);
    NetOut.Write(GameY);
    NetOut.Write(GameVelX);
    NetOut.Write(GameVelY);
    NetOut.Write(GameAccelX);
    NetOut.Write(GameAccelY);
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

  NetOut.Write(Byte(NET_MSG_PLRSTA));
  NetOut.Write(PID);

  with P do
  begin
    NetOut.Write(Byte(alive));
    NetOut.Write(Byte(GodMode));
    NetOut.Write(Health);
    NetOut.Write(Armor);
    NetOut.Write(Air);
    NetOut.Write(JetFuel);
    NetOut.Write(Lives);
    NetOut.Write(Team);

    for I := WP_FIRST to WP_LAST do
      NetOut.Write(Byte(FWeapon[I]));

    for I := A_BULLETS to A_HIGH do
      NetOut.Write(FAmmo[I]);

    for I := A_BULLETS to A_HIGH do
      NetOut.Write(FMaxAmmo[I]);

    for I := MR_SUIT to MR_MAX do
      NetOut.Write(LongWord(FMegaRulez[I]));

    NetOut.Write(Byte(R_ITEM_BACKPACK in FRulez));
    NetOut.Write(Byte(R_KEY_RED in FRulez));
    NetOut.Write(Byte(R_KEY_GREEN in FRulez));
    NetOut.Write(Byte(R_KEY_BLUE in FRulez));
    NetOut.Write(Byte(R_BERSERK in FRulez));

    NetOut.Write(Frags);
    NetOut.Write(Death);

    NetOut.Write(CurrWeap);

    NetOut.Write(Byte(FSpectator));
    NetOut.Write(Byte(FGhost));
    NetOut.Write(Byte(FPhysics));
    NetOut.Write(Byte(FNoRespawn));
    NetOut.Write(Byte(FJetpack));
    NetOut.Write(FFireTime);
    NetOut.Write(Byte(FFlaming));
    NetOut.Write(FSpawnInvul);
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_PLAYER);
end;

procedure MH_SEND_PlayerDamage(PID: Word; Kind: Byte; Attacker, Value: Word; VX, VY: Integer; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_PLRDMG));
  NetOut.Write(PID);
  NetOut.Write(Kind);
  NetOut.Write(Attacker);
  NetOut.Write(Value);
  NetOut.Write(VX);
  NetOut.Write(VY);

  g_Net_Host_Send(ID, False, NET_CHAN_PLAYER);
end;

procedure MH_SEND_PlayerDeath(PID: Word; KillType, DeathType: Byte; Attacker: Word; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_PLRDIE));
  NetOut.Write(PID);
  NetOut.Write(KillType);
  NetOut.Write(DeathType);
  NetOut.Write(Attacker);

  g_Net_Host_Send(ID, True, NET_CHAN_PLAYER);
end;

procedure MH_SEND_PlayerFire(PID: Word; Weapon: Byte; X, Y, AX, AY: Integer; ShotID: Integer = -1; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_PLRFIRE));
  NetOut.Write(PID);
  NetOut.Write(Weapon);
  NetOut.Write(X);
  NetOut.Write(Y);
  NetOut.Write(AX);
  NetOut.Write(AY);
  NetOut.Write(ShotID);

  g_Net_Host_Send(ID, True, NET_CHAN_SHOTS);
end;

procedure MH_SEND_PlayerDelete(PID: Word; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_PLRDEL));
  NetOut.Write(PID);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

procedure MH_SEND_PlayerSettings(PID: Word; Mdl: string = ''; ID: Integer = NET_EVERYONE);
var
  Pl: TPlayer;
begin
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  NetOut.Write(Byte(NET_MSG_PLRSET));
  NetOut.Write(PID);
  NetOut.Write(Pl.Name);
  if Mdl = '' then
    NetOut.Write(Pl.Model.Name)
  else
    NetOut.Write(Mdl);
  NetOut.Write(Pl.FColor.R);
  NetOut.Write(Pl.FColor.G);
  NetOut.Write(Pl.FColor.B);
  NetOut.Write(Pl.Team);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

// ITEM (SEND)

procedure MH_SEND_ItemSpawn(Quiet: Boolean; IID: Word; ID: Integer = NET_EVERYONE);
var
  it: PItem;
  tt: Byte;
begin
  it := g_Items_ByIdx(IID);

  NetOut.Write(Byte(NET_MSG_ISPAWN));
  NetOut.Write(IID);
  NetOut.Write(Byte(Quiet));
  tt := it.ItemType;
  if it.dropped then tt := tt or $80;
  NetOut.Write(tt);
  NetOut.Write(Byte(it.Fall));
  NetOut.Write(Byte(it.Respawnable));
  NetOut.Write(it.Obj.X);
  NetOut.Write(it.Obj.Y);
  NetOut.Write(it.Obj.Vel.X);
  NetOut.Write(it.Obj.Vel.Y);

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_ItemDestroy(Quiet: Boolean; IID: Word; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_IDEL));
  NetOut.Write(IID);
  NetOut.Write(Byte(Quiet));

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_ItemPos(IID: Word; ID: Integer = NET_EVERYONE);
var
  it: PItem;
begin
  it := g_Items_ByIdx(IID);

  NetOut.Write(Byte(NET_MSG_IPOS));
  NetOut.Write(IID);
  NetOut.Write(it.Obj.X);
  NetOut.Write(it.Obj.Y);
  NetOut.Write(it.Obj.Vel.X);
  NetOut.Write(it.Obj.Vel.Y);

  g_Net_Host_Send(ID, False, NET_CHAN_LARGEDATA);
end;

// PANEL

procedure MH_SEND_PanelTexture(PGUID: Integer; AnimLoop: Byte; ID: Integer = NET_EVERYONE);
var
  TP: TPanel;
begin
  TP := g_Map_PanelByGUID(PGUID);
  if (TP = nil) then exit;

  with TP do
  begin
    NetOut.Write(Byte(NET_MSG_PTEX));
    NetOut.Write(LongWord(PGUID));
    NetOut.Write(FCurTexture);
    NetOut.Write(FCurFrame);
    NetOut.Write(FCurFrameCount);
    NetOut.Write(AnimLoop);
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_PanelState(PGUID: Integer; ID: Integer = NET_EVERYONE);
var
  TP: TPanel;
  mpflags: Byte = 0;
begin
  TP := g_Map_PanelByGUID(PGUID);
  if (TP = nil) then exit;

  NetOut.Write(Byte(NET_MSG_PSTATE));
  NetOut.Write(LongWord(PGUID));
  NetOut.Write(Byte(TP.Enabled));
  NetOut.Write(TP.LiftType);
  NetOut.Write(TP.X);
  NetOut.Write(TP.Y);
  NetOut.Write(Word(TP.Width));
  NetOut.Write(Word(TP.Height));
  // mplats
  NetOut.Write(LongInt(TP.movingSpeedX));
  NetOut.Write(LongInt(TP.movingSpeedY));
  NetOut.Write(LongInt(TP.movingStartX));
  NetOut.Write(LongInt(TP.movingStartY));
  NetOut.Write(LongInt(TP.movingEndX));
  NetOut.Write(LongInt(TP.movingEndY));
  NetOut.Write(LongInt(TP.sizeSpeedX));
  NetOut.Write(LongInt(TP.sizeSpeedY));
  NetOut.Write(LongInt(TP.sizeEndX));
  NetOut.Write(LongInt(TP.sizeEndY));
  if TP.movingActive then mpflags := mpflags or 1;
  if TP.moveOnce then mpflags := mpflags or 2;
  NetOut.Write(Byte(mpflags));

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

// TRIGGER

procedure MH_SEND_TriggerSound(var T: TTrigger; ID: Integer = NET_EVERYONE);
begin
  if gTriggers = nil then Exit;
  if T.Sound = nil then Exit;

  NetOut.Write(Byte(NET_MSG_TSOUND));
  NetOut.Write(T.ClientID);
  NetOut.Write(Byte(T.Sound.IsPlaying));
  NetOut.Write(LongWord(T.Sound.GetPosition));
  NetOut.Write(T.SoundPlayCount);

  g_Net_Host_Send(ID, True);
end;

procedure MH_SEND_TriggerMusic(ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_TMUSIC));
  NetOut.Write(gMusic.Name);
  NetOut.Write(Byte(gMusic.IsPlaying));
  NetOut.Write(LongWord(gMusic.GetPosition));
  NetOut.Write(Byte(gMusic.SpecPause or gMusic.IsPaused));

  g_Net_Host_Send(ID, True);
end;

// MONSTER

procedure MH_SEND_MonsterSpawn(UID: Word; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_ByUID(UID);
  if M = nil then
    Exit;

  with M do
  begin
    NetOut.Write(Byte(NET_MSG_MSPAWN));
    NetOut.Write(UID);
    NetOut.Write(MonsterType);
    NetOut.Write(MonsterState);
    NetOut.Write(MonsterAnim);
    NetOut.Write(MonsterTargetUID);
    NetOut.Write(MonsterTargetTime);
    NetOut.Write(MonsterBehaviour);
    NetOut.Write(MonsterSleep);
    NetOut.Write(MonsterHealth);
    NetOut.Write(MonsterAmmo);
    NetOut.Write(GameX);
    NetOut.Write(GameY);
    NetOut.Write(GameVelX);
    NetOut.Write(GameVelY);
    NetOut.Write(Byte(GameDirection));
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

procedure MH_SEND_MonsterPos(UID: Word; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_ByUID(UID);
  if M = nil then Exit;

  NetOut.Write(Byte(NET_MSG_MPOS));
  NetOut.Write(UID);

  with M do
  begin
    NetOut.Write(GameX);
    NetOut.Write(GameY);
    NetOut.Write(GameVelX);
    NetOut.Write(GameVelY);
    NetOut.Write(Byte(GameDirection));
  end;

  g_Net_Host_Send(ID, False, NET_CHAN_MONSTERPOS);
end;

procedure MH_SEND_MonsterState(UID: Word; ForcedAnim: Byte = 255; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_ByUID(UID);
  if M = nil then Exit;

  NetOut.Write(Byte(NET_MSG_MSTATE));
  NetOut.Write(UID);

  with M do
  begin
    NetOut.Write(MonsterState);
    NetOut.Write(ForcedAnim);
    NetOut.Write(MonsterTargetUID);
    NetOut.Write(MonsterTargetTime);
    NetOut.Write(MonsterSleep);
    NetOut.Write(MonsterHealth);
    NetOut.Write(MonsterAmmo);
    NetOut.Write(MonsterPain);
    NetOut.Write(Byte(AnimIsReverse));
    NetOut.Write(FFireTime);
  end;

  g_Net_Host_Send(ID, True, NET_CHAN_MONSTER);
end;

procedure MH_SEND_MonsterShot(UID: Word; X, Y, VX, VY: Integer; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_MSHOT));
  NetOut.Write(UID);
  NetOut.Write(X);
  NetOut.Write(Y);
  NetOut.Write(VX);
  NetOut.Write(VY);

  g_Net_Host_Send(ID, True, NET_CHAN_MONSTER);
end;

procedure MH_SEND_MonsterDelete(UID: Word; ID: Integer = NET_EVERYONE);
var
  M: TMonster;
begin
  M := g_Monsters_ByUID(UID);
  if M = nil then Exit;

  NetOut.Write(Byte(NET_MSG_MDEL));
  NetOut.Write(UID);

  g_Net_Host_Send(ID, True, NET_CHAN_LARGEDATA);
end;

// MISC

procedure MH_SEND_TimeSync(Time: LongWord; ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_TIME_SYNC));
  NetOut.Write(Time);

  g_Net_Host_Send(ID, False, NET_CHAN_SERVICE);
end;

procedure MH_SEND_VoteEvent(EvType: Byte;
                            StrArg1: string = 'a'; StrArg2: string = 'b';
                            IntArg1: SmallInt = 0; IntArg2: SmallInt = 0;
                            ID: Integer = NET_EVERYONE);
begin
  NetOut.Write(Byte(NET_MSG_VOTE_EVENT));
  NetOut.Write(EvType);
  NetOut.Write(IntArg1);
  NetOut.Write(IntArg2);
  NetOut.Write(StrArg1);
  NetOut.Write(StrArg2);

  g_Net_Host_Send(ID, True, NET_CHAN_IMPORTANT);
end;

// CLIENT MESSAGES //

// GAME

procedure MC_RECV_Chat(var M: TMsg);
var
  Txt: string;
  Mode: Byte;
begin
  Txt := M.ReadString();
  Mode := M.ReadByte();

  if Mode <> NET_CHAT_SYSTEM then
  begin
    if NetDeafLevel = 0 then
    begin
      if Mode = NET_CHAT_PLAYER then
      begin
        g_Console_Add(Txt, True);
        e_WriteLog('[Chat] ' + b_Text_Unformat(Txt), TMsgType.Notify);
        g_Game_ChatSound(b_Text_Unformat(Txt));
      end else
      if (Mode = NET_CHAT_TEAM) and (gPlayer1 <> nil) then
      begin
        if gPlayer1.Team = TEAM_RED then
          g_Console_Add(b_Text_Format('\r[Team] ') + Txt, True);
        if gPlayer1.Team = TEAM_BLUE then
          g_Console_Add(b_Text_Format('\b[Team] ') + Txt, True);
        e_WriteLog('[Team Chat] ' + b_Text_Unformat(Txt), TMsgType.Notify);
        g_Game_ChatSound(b_Text_Unformat(Txt));
      end;
    end;
  end else if (NetDeafLevel < 2) then
    g_Console_Add(Txt, True);
end;

procedure MC_RECV_Effect(var M: TMsg);
var
  Kind: Byte;
  X, Y: Integer;
  Ang: SmallInt;
begin
  if not gGameOn then Exit;
  Kind := M.ReadByte();
  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  Ang := M.ReadSmallInt();

  case Kind of
    NET_GFX_SPARK:
      g_GFX_Spark(X, Y, 2 + Random(2), Ang, 0, 0);

    NET_GFX_TELE:
    begin
      r_GFX_OnceAnim(R_GFX_TELEPORT_FAST, X, Y);
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_GAME_TELEPORT', X, Y);
    end;

    NET_GFX_EXPLODE:
    begin
      r_GFX_OnceAnim(R_GFX_EXPLODE_ROCKET, X - 64, Y - 64);
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEROCKET', X, Y);
    end;

    NET_GFX_BFGEXPL:
    begin
      r_GFX_OnceAnim(R_GFX_EXPLODE_BFG, X - 64, Y - 64);
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_WEAPON_EXPLODEBFG', X, Y);
    end;

    NET_GFX_BFGHIT:
    begin
      r_GFX_OnceAnim(R_GFX_BFG_HIT, X - 32, Y - 32);
    end;

    NET_GFX_FIRE:
    begin
      r_GFX_OnceAnim(R_GFX_FIRE, X, Y);
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_FIRE', X, Y);
    end;

    NET_GFX_RESPAWN:
    begin
      r_GFX_OnceAnim(R_GFX_ITEM_RESPAWN, X, Y);
      if Ang = 1 then
        g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', X, Y);
    end;

    NET_GFX_SHELL1:
      g_Player_CreateShell(X, Y, 0, -2, SHELL_BULLET);

    NET_GFX_SHELL2:
      g_Player_CreateShell(X, Y, 0, -2, SHELL_SHELL);

    NET_GFX_SHELL3:
    begin
      g_Player_CreateShell(X, Y, 0, -2, SHELL_SHELL);
      g_Player_CreateShell(X, Y, 0, -2, SHELL_SHELL);
    end;
  end;
end;

procedure MC_RECV_Sound(var M: TMsg);
var
  Name: string;
  X, Y: Integer;
  Pos: Boolean;
begin
  Name := M.ReadString();
  Pos := M.ReadByte() <> 0;
  if Pos then
  begin
    X := M.ReadLongInt();
    Y := M.ReadLongInt();
    g_Sound_PlayExAt(Name, X, Y);
  end
  else
    g_Sound_PlayEx(Name);
end;

procedure MC_RECV_CreateShot(var M: TMsg);
var
  I, X, Y, XV, YV: Integer;
  Timeout: LongWord;
  Target, Spawner: Word;
  ShType: Byte;
begin
  I := M.ReadLongInt();
  ShType := M.ReadByte();
  Target := M.ReadWord();
  Spawner := M.ReadWord();
  Timeout := M.ReadLongWord();
  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  XV := M.ReadLongInt();
  YV := M.ReadLongInt();

  I := g_Weapon_CreateShot(I, ShType, Spawner, Target, X, Y, XV, YV);
  if (Shots <> nil) and (I <= High(Shots)) then
  begin
    Shots[I].Timeout := Timeout;
    //Shots[I].Target := Target; // TODO: find a use for Target later
  end;
end;

procedure MC_RECV_UpdateShot(var M: TMsg);
var
  I, TX, TY, TXV, TYV: Integer;
begin
  I := M.ReadLongInt();
  TX := M.ReadLongInt();
  TY := M.ReadLongInt();
  TXV := M.ReadLongInt();
  TYV := M.ReadLongInt();

  if (Shots <> nil) and (I <= High(Shots)) then
    with (Shots[i]) do
    begin
      Obj.X := TX;
      Obj.Y := TY;
      Obj.Vel.X := TXV;
      Obj.Vel.Y := TYV;
    end;
end;

procedure MC_RECV_DeleteShot(var M: TMsg);
var
  I, X, Y: Integer;
  L: Boolean;
begin
  if not gGameOn then Exit;
  I := M.ReadLongInt();
  L := (M.ReadByte() <> 0);
  X := M.ReadLongInt();
  Y := M.ReadLongInt();

  g_Weapon_DestroyShot(I, X, Y, L);
end;

procedure MC_RECV_GameStats(var M: TMsg);
begin
  if gGameSettings.GameMode in [GM_TDM, GM_CTF] then
  begin
    gTeamStat[TEAM_RED].Score := M.ReadSmallInt();
    gTeamStat[TEAM_BLUE].Score := M.ReadSmallInt();
  end
  else
    if gGameSettings.GameMode = GM_COOP then
    begin
      gCoopMonstersKilled := M.ReadWord();
      gCoopSecretsFound := M.ReadWord();
    end;
end;

procedure MC_RECV_CoopStats(var M: TMsg);
begin
  gTotalMonsters := M.ReadLongInt();
  gSecretsCount := M.ReadLongInt();
  gCoopTotalMonstersKilled := M.ReadWord();
  gCoopTotalSecretsFound := M.ReadWord();
  gCoopTotalMonsters := M.ReadWord();
  gCoopTotalSecrets := M.ReadWord();
end;

procedure MC_RECV_GameEvent(var M: TMsg);
var
  EvType: Byte;
  EvNum: Integer;
  EvStr: string;
  EvTime: LongWord;
  BHash: Boolean;
  EvHash: TMD5Digest;
  pl: TPlayer;
  i1, i2: TStrings_Locale;
  pln: String;
  cnt: Byte;
  goodCmd: Boolean = true;
begin
  FillChar(EvHash, Sizeof(EvHash), 0);
  EvType := M.ReadByte();
  EvNum := M.ReadLongInt();
  EvStr := M.ReadString();
  gLastMap := M.ReadByte() <> 0;
  if gLastMap and (gGameSettings.GameMode = GM_COOP) then gStatsOff := True;
  gStatsPressed := True;
  EvTime := M.ReadLongWord();
  BHash := M.ReadByte() <> 0;
  if BHash then
    EvHash := M.ReadMD5();

  gTime := EvTime;

  if (g_Res_received_map_start <> 0) then
  begin
    if (g_Res_received_map_start < 0) then exit;
    goodCmd := false;
    case EvType of
      NET_EV_MAPSTART: goodCmd := true;
      NET_EV_MAPEND: goodCmd := true;
      NET_EV_PLAYER_KICK: goodCmd := true;
      NET_EV_PLAYER_BAN: goodCmd := true;
      NET_EV_LMS_WARMUP: goodCmd := true;
    end;
    if not goodCmd then exit;
  end;

  case EvType of
    NET_EV_MAPSTART:
    begin
      if (g_Res_received_map_start <> 0) then
      begin
        g_Res_received_map_start := -1;
      end
      else
      begin
        gGameOn := False;
        g_Game_ClearLoading();
        g_Game_StopAllSounds(True);

        gSwitchGameMode := Byte(EvNum);
        gGameSettings.GameMode := gSwitchGameMode;

        gWADHash := EvHash;
        if not g_Game_StartMap(false{asMegawad}, EvStr, True) then
        begin
          if not isWadPath(EvStr) then
            g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [gGameSettings.WAD + ':\' + EvStr]))
          else
            g_FatalError(Format(_lc[I_GAME_ERROR_MAP_LOAD], [EvStr]));
          Exit;
        end;

        MC_SEND_FullStateRequest;
      end;
    end;

    NET_EV_MAPEND:
    begin
      gLMSRespawn := LMS_RESPAWN_NONE;
      gLMSRespawnTime := 0;
      if (g_Res_received_map_start <> 0) then
      begin
        g_Res_received_map_start := -1;
      end
      else
      begin
        gMissionFailed := EvNum <> 0;
        gExit := EXIT_ENDLEVELCUSTOM;
      end;
    end;

    NET_EV_RCON:
    begin
      case EvNum of
        NET_RCON_NOAUTH:
          g_Console_Add(_lc[I_NET_RCON_NOAUTH], True);
        NET_RCON_PWGOOD:
          g_Console_Add(_lc[I_NET_RCON_PWD_VALID], True);
        NET_RCON_PWBAD:
          g_Console_Add(_lc[I_NET_RCON_PWD_INVALID], True);
      end;
    end;

    NET_EV_CHANGE_TEAM:
    begin
      if NetDeafLevel < 2 then
      begin
        if EvNum = TEAM_RED then
          g_Console_Add(Format(_lc[I_PLAYER_CHTEAM_RED], [EvStr]), True);
        if EvNum = TEAM_BLUE then
          g_Console_Add(Format(_lc[I_PLAYER_CHTEAM_BLUE], [EvStr]), True);
      end;
    end;

    NET_EV_PLAYER_KICK:
      begin
        g_Console_Add(Format(_lc[I_PLAYER_KICK], [EvStr]), True);
        if (g_Res_received_map_start <> 0) then g_Res_received_map_start := -1;
      end;

    NET_EV_PLAYER_BAN:
      begin
        g_Console_Add(Format(_lc[I_PLAYER_BAN], [EvStr]), True);
        if (g_Res_received_map_start <> 0) then g_Res_received_map_start := -1;
      end;

    NET_EV_LMS_WARMUP:
    begin
      if EvNum > 0 then
      begin
        gLMSRespawn := LMS_RESPAWN_WARMUP;
        gLMSRespawnTime := gTime + EvNum;
        g_Console_Add(Format(_lc[I_MSG_WARMUP_START], [EvNum div 1000]), True);
      end
      else if gPlayer1 = nil then
      begin
        g_Console_Add(_lc[I_PLAYER_SPECT4], True);
      end;
    end;

    NET_EV_LMS_SURVIVOR:
      g_Console_Add('*** ' + _lc[I_MESSAGE_LMS_SURVIVOR] + ' ***', True);

    NET_EV_BIGTEXT:
      if NetDeafLevel < 2 then g_Game_Message(AnsiUpperCase(EvStr), Word(EvNum));

    NET_EV_SCORE:
    begin
      pl := g_Player_Get(EvNum and $FFFF);
      if pl = nil then
        pln := '?'
      else
        pln := pl.Name;
      cnt := (EvNum shr 16) and $FF;
      if Pos('w', EvStr) = 0 then
      begin
        // Default score
        if Pos('t', EvStr) = 0 then
        begin
          // Player +/- score
          if Pos('-', EvStr) = 0 then
          begin
            if Pos('e', EvStr) = 0 then
              i1 := I_PLAYER_SCORE_ADD_OWN
            else
              i1 := I_PLAYER_SCORE_ADD_ENEMY;
          end else
          begin
            if Pos('e', EvStr) = 0 then
              i1 := I_PLAYER_SCORE_SUB_OWN
            else
              i1 := I_PLAYER_SCORE_SUB_ENEMY;
          end;
          // Which team
          if Pos('r', EvStr) > 0 then
            i2 := I_PLAYER_SCORE_TO_RED
          else
            i2 := I_PLAYER_SCORE_TO_BLUE;
          g_Console_Add(Format(_lc[i1], [pln, cnt, _lc[i2]]), True);
        end else
        begin
          // Team +/- score
          if Pos('-', EvStr) = 0 then
            i1 := I_PLAYER_SCORE_ADD_TEAM
          else
            i1 := I_PLAYER_SCORE_SUB_TEAM;
          // Which team
          if Pos('r', EvStr) > 0 then
            i2 := I_PLAYER_SCORE_RED
          else
            i2 := I_PLAYER_SCORE_BLUE;
          g_Console_Add(Format(_lc[i1], [_lc[i2], cnt]), True);
        end;
      end else
      begin
        // Game Win
        if Pos('e', EvStr) = 0 then
          i1 := I_PLAYER_SCORE_WIN_OWN
        else
          i1 := I_PLAYER_SCORE_WIN_ENEMY;
        // Which team
        if Pos('r', EvStr) > 0 then
          i2 := I_PLAYER_SCORE_TO_RED
        else
          i2 := I_PLAYER_SCORE_TO_BLUE;
        g_Console_Add(Format(_lc[i1], [pln, _lc[i2]]), True);
      end;
    end;

    NET_EV_SCORE_MSG:
    begin
      if EvNum = TEAM_RED then
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108);
      if EvNum = TEAM_BLUE then
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_ADD], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
      if EvNum = -TEAM_RED then
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 108);
      if EvNum = -TEAM_BLUE then
        g_Game_Message(Format(_lc[I_MESSAGE_SCORE_SUB], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 108);
    end;

    NET_EV_LMS_START:
    begin
      g_Player_RemoveAllCorpses;
      gLMSRespawn := LMS_RESPAWN_NONE;
      g_Game_Message(_lc[I_MESSAGE_LMS_START], 144);
    end;

    NET_EV_LMS_WIN:
      g_Game_Message(Format(_lc[I_MESSAGE_LMS_WIN], [AnsiUpperCase(EvStr)]), 144);

    NET_EV_TLMS_WIN:
    begin
      if EvNum = TEAM_RED then
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_RED])]), 144);
      if EvNum = TEAM_BLUE then
        g_Game_Message(Format(_lc[I_MESSAGE_TLMS_WIN], [AnsiUpperCase(_lc[I_GAME_TEAM_BLUE])]), 144);
    end;

    NET_EV_LMS_LOSE:
      g_Game_Message(_lc[I_MESSAGE_LMS_LOSE], 144);

    NET_EV_LMS_DRAW:
      g_Game_Message(_lc[I_GAME_WIN_DRAW], 144);

    NET_EV_LMS_NOSPAWN:
      g_Console_Add(_lc[I_PLAYER_SPECT4], True);

    NET_EV_KILLCOMBO:
      g_Game_Announce_KillCombo(EvNum);

    NET_EV_PLAYER_TOUCH:
    begin
      pl := g_Player_Get(EvNum);
      if pl <> nil then
        pl.Touch();
    end;

    NET_EV_SECRET:
    begin
      pl := g_Player_Get(EvNum);
      if pl <> nil then
      begin
        g_Console_Add(Format(_lc[I_PLAYER_SECRET], [pl.Name]), True);
        g_Sound_PlayEx('SOUND_GAME_SECRET');
      end;
    end;

    NET_EV_INTER_READY:
    begin
      pl := g_Player_Get(EvNum);
      if pl <> nil then pl.FReady := (EvStr = 'Y');
    end;
  end;
end;

procedure MC_RECV_FlagPos(var M: TMsg);
var
  Fl: Byte;
begin
  Fl := M.ReadByte();
  if Fl = FLAG_NONE then Exit;
  gFlags[Fl].Obj.X := M.ReadLongInt();
  gFlags[Fl].Obj.Y := M.ReadLongInt();
  gFlags[Fl].Obj.Vel.X := M.ReadLongInt();
  gFlags[Fl].Obj.Vel.Y := M.ReadLongInt();
end;

procedure MC_RECV_FlagEvent(var M: TMsg);
var
  PID: Word;
  Pl: TPlayer;
  EvType: Byte;
  Fl, a: Byte;
  Quiet: Boolean;
  s, ts: string;
begin
  EvType := M.ReadByte();
  Fl := M.ReadByte();

  if Fl = FLAG_NONE then Exit;

  Quiet := (M.ReadByte() <> 0);
  PID := M.ReadWord();

  gFlags[Fl].State := M.ReadByte();
  gFlags[Fl].CaptureTime := M.ReadLongWord();
  gFlags[Fl].Obj.X := M.ReadLongInt();
  gFlags[Fl].Obj.Y := M.ReadLongInt();
  gFlags[Fl].Obj.Vel.X := M.ReadLongInt();
  gFlags[Fl].Obj.Vel.Y := M.ReadLongInt();
  gFlags[Fl].Direction := TDirection(M.ReadByte());

  Pl := g_Player_Get(PID);
  if (Pl = nil) and
     (EvType <> FLAG_STATE_NORMAL) and
     (EvType <> FLAG_STATE_DROPPED) and
     (EvType <> FLAG_STATE_RETURNED) then
    Exit;

  case EvType of
    FLAG_STATE_NORMAL:
    begin
      if Quiet or (Pl = nil) then Exit;

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);

      if ((Pl = gPlayer1) or (Pl = gPlayer2)
      or ((gPlayer1 <> nil) and (gPlayer1.Team = Pl.Team))
      or ((gPlayer2 <> nil) and (gPlayer2.Team = Pl.Team))) then
        a := 0
      else
        a := 1;

      if not sound_ret_flag[a].IsPlaying() then
        sound_ret_flag[a].Play();
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

      if ((Pl = gPlayer1) or (Pl = gPlayer2)
      or ((gPlayer1 <> nil) and (gPlayer1.Team = Pl.Team))
      or ((gPlayer2 <> nil) and (gPlayer2.Team = Pl.Team))) then
        a := 0
      else
        a := 1;

      if not sound_get_flag[a].IsPlaying() then
        sound_get_flag[a].Play();
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

      if ((Pl = gPlayer1) or (Pl = gPlayer2)
      or ((gPlayer1 <> nil) and (gPlayer1.Team = Pl.Team))
      or ((gPlayer2 <> nil) and (gPlayer2.Team = Pl.Team))) then
        a := 0
      else
        a := 1;

      if not sound_lost_flag[a].IsPlaying() then
        sound_lost_flag[a].Play();
    end;

    FLAG_STATE_SCORED:
    begin
      g_Map_ResetFlag(FLAG_RED);
      g_Map_ResetFlag(FLAG_BLUE);
      if Quiet or (Pl = nil) then Exit;
      Pl.SetFlag(FLAG_NONE);

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      ts := Format('%.4d', [gFlags[Fl].CaptureTime]);
      Insert('.', ts, Length(ts) + 1 - 3);
      g_Console_Add(Format(_lc[I_PLAYER_FLAG_CAPTURE], [Pl.Name, s, ts]), True);
      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_CAPTURE], [AnsiUpperCase(s)]), 144);

      if ((Pl = gPlayer1) or (Pl = gPlayer2)
      or ((gPlayer1 <> nil) and (gPlayer1.Team = Pl.Team))
      or ((gPlayer2 <> nil) and (gPlayer2.Team = Pl.Team))) then
        a := 0
      else
        a := 1;

      if not sound_cap_flag[a].IsPlaying() then
        sound_cap_flag[a].Play();
    end;

    FLAG_STATE_RETURNED:
    begin
      g_Map_ResetFlag(Fl);
      if Quiet then Exit;

      if Fl = FLAG_RED then
        s := _lc[I_PLAYER_FLAG_RED]
      else
        s := _lc[I_PLAYER_FLAG_BLUE];

      g_Game_Message(Format(_lc[I_MESSAGE_FLAG_RETURN], [AnsiUpperCase(s)]), 144);

      if ((Pl = gPlayer1) or (Pl = gPlayer2)
      or ((gPlayer1 <> nil) and (gPlayer1.Team = Pl.Team))
      or ((gPlayer2 <> nil) and (gPlayer2.Team = Pl.Team))) then
        a := 0
      else
        a := 1;

      if not sound_ret_flag[a].IsPlaying() then
        sound_ret_flag[a].Play();
    end;
  end;
end;

procedure MC_RECV_GameSettings(var M: TMsg);
begin
  gGameSettings.GameMode := M.ReadByte();
  gGameSettings.ScoreLimit := M.ReadWord();
  gGameSettings.TimeLimit := M.ReadWord();
  gGameSettings.MaxLives := M.ReadByte();
  gGameSettings.Options := M.ReadLongWord();
end;

// PLAYER

function MC_RECV_PlayerCreate(var M: TMsg): Word;
var
  PID, DID: Word;
  PName, Model: string;
  Color: TRGB;
  T: Byte;
  Pl: TPlayer;
begin
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);

  PName := M.ReadString();
  Model := M.ReadString();
  Color.R := M.ReadByte();
  Color.G := M.ReadByte();
  Color.B := M.ReadByte();
  T := M.ReadByte();

  Result := 0;
  if (PID <> NetPlrUID1) and (PID <> NetPlrUID2) then
  begin
    if (Pl <> nil) then Exit;
    if (g_Force_Model_Get() <> 0) then
      Model := g_Forced_Model_GetName();
    DID := g_Player_Create(Model, Color, T, False);
    with g_Player_Get(DID) do
    begin
      UID := PID;
      Name := PName;
      Reset(True);
    end;
  end
  else
  begin
    if (PID = NetPlrUID1) and (gPlayer1 <> nil) then begin
      gPlayer1.UID := PID;
      gPlayer1.Model.SetColor(Color.R, Color.G, Color.B);
      gPlayer1.ChangeTeam(T);
    end;
    if (PID = NetPlrUID2) and (gPlayer2 <> nil) then begin
      gPlayer2.UID := PID;
      gPlayer2.Model.SetColor(Color.R, Color.G, Color.B);
      gPlayer2.ChangeTeam(T);
    end;
  end;

  if NetDeafLevel < 3 then 
    g_Console_Add(Format(_lc[I_PLAYER_JOIN], [PName]), True);
  e_WriteLog('NET: Player ' + PName + ' [' + IntToStr(PID) + '] added.', TMsgType.Notify);
  Result := PID;
end;

function MC_RECV_PlayerPos(var M: TMsg): Word;
var
  GT: LongWord;
  PID: Word;
  kByte: Word;
  Pl: TPlayer;
  Dir: Byte;
  TmpX, TmpY: Integer;
begin
  Result := 0;

  GT := M.ReadLongWord();
  if GT < gTime - NET_MAX_DIFFTIME then
  begin
    gTime := GT;
    Exit;
  end;
  gTime := GT;

  PID := M.ReadWord();
  Pl := g_Player_Get(PID);

  if Pl = nil then Exit;

  Result := PID;

  with Pl do
  begin
    FPing := M.ReadWord();
    FLoss := M.ReadByte();
    kByte := M.ReadWord();
    Dir := M.ReadByte();

    TmpX := M.ReadLongInt();
    TmpY := M.ReadLongInt();

    ReleaseKeys;

    if LongBool(kByte and NET_KEY_CHAT) then
      PressKey(KEY_CHAT, 10000)
    else
    begin
      if LongBool(kByte and NET_KEY_LEFT) then PressKey(KEY_LEFT, 10000);
      if LongBool(kByte and NET_KEY_RIGHT) then PressKey(KEY_RIGHT, 10000);
      if LongBool(kByte and NET_KEY_UP) then PressKey(KEY_UP, 10000);
      if LongBool(kByte and NET_KEY_DOWN) then PressKey(KEY_DOWN, 10000);
      if LongBool(kByte and NET_KEY_JUMP) then PressKey(KEY_JUMP, 10000);
    end;

    JustTeleported := LongBool(kByte and NET_KEY_FORCEDIR);

    if ((Pl <> gPlayer1) and (Pl <> gPlayer2)) or JustTeleported then
      SetDirection(TDirection(Dir));

    GameVelX := M.ReadLongInt();
    GameVelY := M.ReadLongInt();
    GameAccelX := M.ReadLongInt();
    GameAccelY := M.ReadLongInt();
    SetLerp(TmpX, TmpY);
    if NetForcePlayerUpdate then Update();
  end;
end;

function MC_RECV_PlayerStats(var M: TMsg): Word;
var
  PID: Word;
  Pl: TPlayer;
  I, OldFire: Integer;
  OldJet, Flam: Boolean;
  NewTeam: Byte;
begin
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);
  Result := 0;
  if Pl = nil then
    Exit;

  with Pl do
  begin
    alive := (M.ReadByte() <> 0);
    GodMode := (M.ReadByte() <> 0);
    Health := M.ReadLongInt();
    Armor := M.ReadLongInt();
    Air := M.ReadLongInt();
    JetFuel := M.ReadLongInt();
    Lives := M.ReadByte();
    NewTeam := M.ReadByte();

    for I := WP_FIRST to WP_LAST do
      FWeapon[I] := (M.ReadByte() <> 0);

    for I := A_BULLETS to A_HIGH do
      FAmmo[I] := M.ReadWord();

    for I := A_BULLETS to A_HIGH do
      FMaxAmmo[I] := M.ReadWord();

    for I := MR_SUIT to MR_MAX do
      FMegaRulez[I] := M.ReadLongWord();

    FRulez := [];
    if (M.ReadByte() <> 0) then
      FRulez := FRulez + [R_ITEM_BACKPACK];
    if (M.ReadByte() <> 0) then
      FRulez := FRulez + [R_KEY_RED];
    if (M.ReadByte() <> 0) then
      FRulez := FRulez + [R_KEY_GREEN];
    if (M.ReadByte() <> 0) then
      FRulez := FRulez + [R_KEY_BLUE];
    if (M.ReadByte() <> 0) then
      FRulez := FRulez + [R_BERSERK];

    Frags := M.ReadLongInt();
    Death := M.ReadLongInt();

    SetWeapon(M.ReadByte());

    FSpectator := M.ReadByte() <> 0;
    if FSpectator then
    begin
      if UID = NetPlrUID1 then
      begin
        gSpectLatchPID1 := UID;
        gPlayer1 := nil;
      end;
      if UID = NetPlrUID2 then
      begin
        gSpectLatchPID2 := UID;
        gPlayer2 := nil;
      end;
    end
    else
    begin
      if (gPlayer1 = nil) and (gSpectLatchPID1 > 0) and (UID = gSpectLatchPID1) then
      begin
        gPlayer1 := Pl;
        gSpectLatchPID1 := 0;
      end;
      if (gPlayer2 = nil) and (gSpectLatchPID2 > 0) and (UID = gSpectLatchPID2) then
      begin
        gPlayer2 := Pl;
        gSpectLatchPID2 := 0;
      end;
    end;
    FGhost := M.ReadByte() <> 0;
    FPhysics := M.ReadByte() <> 0;
    FNoRespawn := M.ReadByte() <> 0;
    OldJet := FJetpack;
    FJetpack := M.ReadByte() <> 0;
    OldFire := FFireTime;
    FFireTime := M.ReadLongInt();
    if (OldFire <= 0) and (FFireTime > 0) then
      g_Sound_PlayExAt('SOUND_IGNITE', Obj.X, Obj.Y);
    Flam := M.ReadByte() <> 0;
    FSpawnInvul := M.ReadLongInt();
    if OldJet and not FJetpack then
      JetpackOff
    else if not OldJet and FJetpack then
      JetpackOn;
    if FFlaming and not Flam then
      FlamerOff;
    if Team <> NewTeam then
      Pl.ChangeTeam(NewTeam);
  end;

  Result := PID;
end;

function MC_RECV_PlayerDamage(var M: TMsg): Word;
var
  PID: Word;
  Pl: TPlayer;
  Kind: Byte;
  Attacker, Value: Word;
  VX, VY: Integer;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  Kind := M.ReadByte();
  Attacker := M.ReadWord();
  Value := M.ReadWord();
  VX := M.ReadWord();
  VY := M.ReadWord();

  with Pl do
    Damage(Value, Attacker, VX, VY, Kind);

  Result := PID;
end;

function MC_RECV_PlayerDeath(var M: TMsg): Word;
var
  PID: Word;
  Pl: TPlayer;
  KillType, DeathType: Byte;
  Attacker: Word;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  KillType := M.ReadByte();
  DeathType := M.ReadByte();
  Attacker := M.ReadWord();

  with Pl do
  begin
    Kill(KillType, Attacker, DeathType);
    SoftReset;
  end;
end;

function MC_RECV_PlayerDelete(var M: TMsg): Word;
var
  PID: Word;
  Pl: TPlayer;
begin
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);
  Result := 0;
  if Pl = nil then Exit;

  if NetDeafLevel < 3 then 
    g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [Pl.Name]), True);
  e_WriteLog('NET: Player ' + Pl.Name + ' [' + IntToStr(PID) + '] removed.', TMsgType.Notify);

  g_Player_Remove(PID);

  Result := PID;
end;

function MC_RECV_PlayerFire(var M: TMsg): Word;
var
  PID: Word;
  Weap: Byte;
  Pl: TPlayer;
  X, Y, AX, AY: Integer;
  SHID: Integer;
begin
  Result := 0;
  if not gGameOn then Exit;
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  Weap := M.ReadByte();
  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  AX := M.ReadLongInt();
  AY := M.ReadLongInt();
  SHID := M.ReadLongInt();

  with Pl do
    if alive then NetFire(Weap, X, Y, AX, AY, SHID);
end;

procedure MC_RECV_PlayerSettings(var M: TMsg);
var
  TmpName: string;
  TmpModel: string;
  CheckModel: string;
  TmpColor: TRGB;
  TmpTeam: Byte;
  Pl: TPlayer;
  PID: Word;
begin
  PID := M.ReadWord();
  Pl := g_Player_Get(PID);
  if Pl = nil then Exit;

  TmpName := M.ReadString();
  TmpModel := M.ReadString();
  TmpColor.R := M.ReadByte();
  TmpColor.G := M.ReadByte();
  TmpColor.B := M.ReadByte();
  TmpTeam := M.ReadByte();

  if (gGameSettings.GameMode in [GM_TDM, GM_CTF]) and (Pl.Team <> TmpTeam) then
  begin
    Pl.ChangeTeam(TmpTeam);
    if gPlayer1 = Pl then
      gPlayer1Settings.Team := TmpTeam;
    if gPlayer2 = Pl then
      gPlayer2Settings.Team := TmpTeam;
  end else
    Pl.SetColor(TmpColor);

  if Pl.Name <> TmpName then
  begin
    if NetDeafLevel < 3 then 
      g_Console_Add(Format(_lc[I_PLAYER_NAME], [Pl.Name, TmpName]), True);
    Pl.Name := TmpName;
  end;

  if (g_Force_Model_Get() <> 0) then
    TmpModel := g_Forced_Model_GetName();
  if TmpModel <> Pl.Model.Name then
    Pl.SetModel(TmpModel);
end;

// ITEM

procedure MC_RECV_ItemSpawn(var M: TMsg);
var
  ID: Word;
  X, Y, VX, VY: Integer;
  T: Byte;
  Quiet, Fall{, Resp}: Boolean;
  it: PItem;
begin
  if not gGameOn then Exit;
  ID := M.ReadWord();
  Quiet := M.ReadByte() <> 0;
  T := M.ReadByte();
  Fall := M.ReadByte() <> 0;
  {Resp :=} M.ReadByte();
  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  VX := M.ReadLongInt();
  VY := M.ReadLongInt();

  g_Items_Create(X, Y, T and $7F, Fall, False, False, ID);
  if ((T and $80) <> 0) then g_Items_SetDrop(ID);

  it := g_Items_ByIdx(ID);
  it.Obj.Vel.X := VX;
  it.Obj.Vel.Y := VY;

  if not Quiet then
  begin
    g_Sound_PlayExAt('SOUND_ITEM_RESPAWNITEM', X, Y);
    r_GFX_OnceAnim(R_GFX_ITEM_RESPAWN, X+(it.Obj.Rect.Width div 2)-16, Y+(it.Obj.Rect.Height div 2)-16);
  end;
end;

procedure MC_RECV_ItemDestroy(var M: TMsg);
var
  ID: Word;
  Quiet: Boolean;
begin
  if not gGameOn then Exit;
  ID := M.ReadWord();
  Quiet := M.ReadByte() <> 0;

  if not g_Items_ValidId(ID) then exit;

  if not Quiet then g_Items_EmitPickupSound(ID);

  g_Items_Remove(ID);
end;

procedure MC_RECV_ItemPos(var M: TMsg);
var
  ID: Word;
  X, Y, VX, VY: Integer;
  it: PItem;
begin
  if not gGameOn then Exit;

  ID := M.ReadWord();
  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  VX := M.ReadLongInt();
  VY := M.ReadLongInt();

  if g_Items_ValidId(ID) then
  begin
    it := g_Items_ByIdx(ID);
    it.Obj.X := X;
    it.Obj.Y := Y;
    it.Obj.Vel.X := VX;
    it.Obj.Vel.Y := VY;
    it.positionChanged();
  end;
end;

// PANEL

procedure MC_RECV_PanelTexture(var M: TMsg);
var
  TP: TPanel;
  PGUID: Integer;
  Tex, Fr: Integer;
  Loop, Cnt: Byte;
begin
  if not gGameOn then Exit;

  PGUID := Integer(M.ReadLongWord());
  Tex := M.ReadLongInt();
  Fr := M.ReadLongInt();
  Cnt := M.ReadByte();
  Loop := M.ReadByte();

  TP := g_Map_PanelByGUID(PGUID);
  if (TP <> nil) then
  begin
    // switch texture
    TP.SetTexture(Tex, Loop);
    TP.SetFrame(Fr, Cnt);
  end;
end;

procedure MC_RECV_PanelState(var M: TMsg);
var
  PGUID: Integer;
  E: Boolean;
  Lift: Byte;
  X, Y, W, H: Integer;
  TP: TPanel;
  speedX, speedY, startX, startY, endX, endY: Integer;
  sizeSpX, sizeSpY, sizeEX, sizeEY: Integer;
  mpflags: Byte;
begin
  if not gGameOn then Exit;

  PGUID := Integer(M.ReadLongWord());
  E := (M.ReadByte() <> 0);
  Lift := M.ReadByte();
  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  W := M.ReadWord();
  H := M.ReadWord();
  // mplats
  speedX := M.ReadLongInt();
  speedY := M.ReadLongInt();
  startX := M.ReadLongInt();
  startY := M.ReadLongInt();
  endX := M.ReadLongInt();
  endY := M.ReadLongInt();
  sizeSpX := M.ReadLongInt();
  sizeSpY := M.ReadLongInt();
  sizeEX := M.ReadLongInt();
  sizeEY := M.ReadLongInt();
  mpflags := M.ReadByte(); // bit0: TP.movingActive; bit1: TP.moveOnce

  TP := g_Map_PanelByGUID(PGUID);
  if (TP = nil) then exit;

  // update lifts state
  if TP.isGLift then g_Map_SetLiftGUID(PGUID, Lift);

  // update enabled/disabled state for all panels
  if E then g_Map_EnableWallGUID(PGUID) else g_Map_DisableWallGUID(PGUID);

  // update panel position, as it can be moved (mplat)
  TP.X := X;
  TP.Y := Y;
  TP.Width := W;
  TP.Height := H;
  // update mplat state
  TP.movingSpeedX := speedX;
  TP.movingSpeedY := speedY;
  TP.movingStartX := startX;
  TP.movingStartY := startY;
  TP.movingEndX := endX;
  TP.movingEndY := endY;
  TP.sizeSpeedX := sizeSpX;
  TP.sizeSpeedY := sizeSpY;
  TP.sizeEndX := sizeEX;
  TP.sizeEndY := sizeEY;
  TP.movingActive := ((mpflags and 1) <> 0);
  TP.moveOnce := ((mpflags and 2) <> 0);
  // notify panel of it's position/size change, so it can fix other internal structures
  TP.positionChanged();
end;

// TRIGGERS

procedure MC_RECV_TriggerSound(var M: TMsg);
var
  SPlaying: Boolean;
  SPos, SID: LongWord;
  SCount: LongInt;
  I: Integer;
begin
  if not gGameOn then Exit;
  if gTriggers = nil then Exit;

  SID := M.ReadLongWord();
  SPlaying := M.ReadByte() <> 0;
  SPos := M.ReadLongWord();
  SCount := M.ReadLongInt();

  for I := Low(gTriggers) to High(gTriggers) do
    if gTriggers[I].TriggerType = TRIGGER_SOUND then
      if gTriggers[I].ClientID = SID then
        with gTriggers[I] do
        begin
          if Sound <> nil then
          begin
            if SPlaying then
            begin
              if tgcLocal then
                Sound.PlayVolumeAt(X+(Width div 2), Y+(Height div 2), tgcVolume/255.0)
              else
                Sound.PlayPanVolume((tgcPan-127.0)/128.0, tgcVolume/255.0);
              Sound.SetPosition(SPos);
            end
            else
              if Sound.IsPlaying then Sound.Stop;
          end;

          SoundPlayCount := SCount;
        end;
end;

procedure MC_RECV_TriggerMusic(var M: TMsg);
var
  MName: string;
  MPlaying: Boolean;
  MPos: LongWord;
  MPaused: Boolean;
begin
  if not gGameOn then Exit;

  MName := M.ReadString();
  MPlaying := M.ReadByte() <> 0;
  MPos := M.ReadLongWord();
  MPaused := M.ReadByte() <> 0;
  MPos := MPos+1; //k8: stfu, fpc!

  if MPlaying then
  begin
    gMusic.SetByName(MName);
    gMusic.Play(True);
    // gMusic.SetPosition(MPos);
    gMusic.SpecPause := MPaused;
  end
  else
    if gMusic.IsPlaying then gMusic.Stop;
end;

// MONSTERS

procedure MC_RECV_MonsterSpawn(var M: TMsg);
var
  ID: Word;
  MType, MState, MDir, MAnim, MBehav: Byte;
  X, Y, VX, VY, MTargTime, MHealth, MAmmo, MSleep: Integer;
  MTarg: Word;
  Mon: TMonster;
begin
  ID := M.ReadWord();
  Mon := g_Monsters_ByUID(ID);
  if Mon <> nil then
    Exit;

  MType := M.ReadByte();
  MState := M.ReadByte();
  MAnim := M.ReadByte();
  MTarg := M.ReadWord();
  MTargTime := M.ReadLongInt();
  MBehav := M.ReadByte();
  MSleep := M.ReadLongInt();
  MHealth := M.ReadLongInt();
  MAmmo := M.ReadLongInt();

  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  VX := M.ReadLongInt();
  VY := M.ReadLongInt();
  MDir := M.ReadByte();

  g_Monsters_Create(MType, X, Y, TDirection(MDir), False, ID);
  Mon := g_Monsters_ByUID(ID);
  if Mon = nil then
    Exit;

  with Mon do
  begin

    MonsterAnim := MAnim;
    MonsterTargetUID := MTarg;
    MonsterTargetTime := MTargTime;
    MonsterBehaviour := MBehav;
    MonsterSleep := MSleep;
    MonsterAmmo := MAmmo;
    SetHealth(MHealth);

    SetState(MState);

    setPosition(X, Y); // this will call positionChanged();
    GameVelX := VX;
    GameVelY := VY;
  end;
end;

procedure MC_RECV_MonsterPos(var M: TMsg);
var
  Mon: TMonster;
  ID: Word;
  X, Y: Integer;
begin
  ID := M.ReadWord();
  Mon := g_Monsters_ByUID(ID);
  if Mon = nil then
    Exit;

  with Mon do
  begin
    X := M.ReadLongInt();
    Y := M.ReadLongInt();
    Mon.setPosition(X, Y); // this will call `positionChanged()`
    GameVelX := M.ReadLongInt();
    GameVelY := M.ReadLongInt();
    GameDirection := TDirection(M.ReadByte());
  end;
end;

procedure MC_RECV_MonsterState(var M: TMsg);
var
  ID, OldFire: Integer;
  MState, MFAnm: Byte;
  Mon: TMonster;
  AnimRevert: Boolean;
begin
  ID := M.ReadWord();
  Mon := g_Monsters_ByUID(ID);
  if Mon = nil then Exit;

  MState := M.ReadByte();
  MFAnm := M.ReadByte();

  with Mon do
  begin
    MonsterTargetUID := M.ReadWord();
    MonsterTargetTime := M.ReadLongInt();
    MonsterSleep := M.ReadLongInt();
    MonsterHealth := M.ReadLongInt();
    MonsterAmmo := M.ReadLongInt();
    MonsterPain := M.ReadLongInt();
    AnimRevert := M.ReadByte() <> 0;
    OldFire := FFireTime;
    FFireTime := M.ReadLongInt();
    if (OldFire <= 0) and (FFireTime > 0) then
      g_Sound_PlayExAt('SOUND_IGNITE', Obj.X, Obj.Y);
    RevertAnim(AnimRevert);

    if MonsterState <> MState then
    begin
      if (MState = MONSTATE_GO) and (MonsterState = MONSTATE_SLEEP) then WakeUpSound();
      if (MState = MONSTATE_DIE) then DieSound();
      if (MState = MONSTATE_PAIN) then MakeBloodSimple(Min(200, MonsterPain));
      if (MState = MONSTATE_ATTACK) then kick(nil);
      if (MState = MONSTATE_DEAD) then SetDeadAnim();

      SetState(MState, MFAnm);
    end;
  end;
end;

procedure MC_RECV_MonsterShot(var M: TMsg);
var
  ID: Integer;
  Mon: TMonster;
  X, Y, VX, VY: Integer;
begin
  ID := M.ReadWord();

  Mon := g_Monsters_ByUID(ID);
  if Mon = nil then Exit;

  X := M.ReadLongInt();
  Y := M.ReadLongInt();
  VX := M.ReadLongInt();
  VY := M.ReadLongInt();

  Mon.ClientAttack(X, Y, VX, VY);
end;

procedure MC_RECV_MonsterDelete(var M: TMsg);
var
  ID: Integer;
  Mon: TMonster;
begin
  ID := M.ReadWord();
  Mon := g_Monsters_ByUID(ID);
  if Mon = nil then Exit;
  Mon.SetState(5);
  Mon.MonsterRemoved := True;
end;

procedure MC_RECV_TimeSync(var M: TMsg);
var
  Time: LongWord;
begin
  Time := M.ReadLongWord();

  if gState = STATE_INTERCUSTOM then
    gServInterTime := Min(Time, 255);
end;

procedure MC_RECV_VoteEvent(var M: TMsg);
var
  EvID: Byte;
  Str1, Str2: string;
  Int1, Int2: SmallInt;
begin
  EvID := M.ReadByte();
  Int1 := M.ReadSmallInt();
  Int2 := M.ReadSmallInt();
  Str1 := M.ReadString();
  Str2 := M.ReadString();

  if NetDeafLevel < 2 then 
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

procedure MC_SEND_Info(Password: string);
var i: Integer;
begin
  NetOut.Clear();

  NetOut.Write(Byte(NET_MSG_INFO));
  NetOut.Write(GAME_VERSION);
  NetOut.Write(Password);
  NetOut.Write(gPlayer1Settings.Name);
  NetOut.Write(gPlayer1Settings.Model);
  NetOut.Write(gPlayer1Settings.Color.R);
  NetOut.Write(gPlayer1Settings.Color.G);
  NetOut.Write(gPlayer1Settings.Color.B);
  NetOut.Write(gPlayer1Settings.Team);
  NetOut.Write(gPlayer1Settings.WeaponSwitch);
  for i := WP_FIRST to WP_LAST + 1 do
    NetOut.Write(gPlayer1Settings.WeaponPreferences[i]);
  NetOut.Write(gPlayer1Settings.SwitchToEmpty);
  NetOut.Write(gPlayer1Settings.SkipFist);

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;

procedure MC_SEND_Chat(Txt: string; Mode: Byte);
begin
  NetOut.Write(Byte(NET_MSG_CHAT));
  NetOut.Write(Txt);
  NetOut.Write(Mode);

  g_Net_Client_Send(True, NET_CHAN_CHAT);
end;

procedure MC_SEND_PlayerPos();
var
  kByte: Word;
  Predict: Boolean;
  strafeDir: Byte;
  WeaponAct: Byte = 0;
  WeaponSelect: Word = 0;
  i: Integer;
begin
  if not gGameOn then Exit;
  if gPlayers = nil then Exit;
  if gPlayer1 = nil then Exit;

  kByte := 0;
  Predict := NetPredictSelf; // and (not NetGotKeys);

  if (not gConsoleShow) and (not gChatShow) and (g_ActiveWindow = nil) then
  begin
    strafeDir := P1MoveButton shr 4;
    P1MoveButton := P1MoveButton and $0F;

    if gPlayerAction[0, ACTION_MOVELEFT] and (not gPlayerAction[0, ACTION_MOVERIGHT]) then
      P1MoveButton := 1
    else if (not gPlayerAction[0, ACTION_MOVELEFT]) and gPlayerAction[0, ACTION_MOVERIGHT] then
      P1MoveButton := 2
    else if (not gPlayerAction[0, ACTION_MOVELEFT]) and (not gPlayerAction[0, ACTION_MOVERIGHT]) then
      P1MoveButton := 0;

    // strafing
    if gPlayerAction[0, ACTION_STRAFE] then
    begin
      // new strafe mechanics
      if (strafeDir = 0) then
        strafeDir := P1MoveButton; // start strafing
      // now set direction according to strafe (reversed)
      if (strafeDir = 2) then
        gPlayer1.SetDirection(TDirection.D_LEFT)
      else if (strafeDir = 1) then
        gPlayer1.SetDirection(TDirection.D_RIGHT)
    end
    else
    begin
      strafeDir := 0; // not strafing anymore
      if (P1MoveButton = 2) and gPlayerAction[0, ACTION_MOVELEFT] then
        gPlayer1.SetDirection(TDirection.D_LEFT)
      else if (P1MoveButton = 1) and gPlayerAction[0, ACTION_MOVERIGHT] then
        gPlayer1.SetDirection(TDirection.D_RIGHT)
      else if P1MoveButton <> 0 then
        gPlayer1.SetDirection(TDirection(P1MoveButton-1));
    end;

    gPlayer1.ReleaseKeys;
    if P1MoveButton = 1 then
    begin
      kByte := kByte or NET_KEY_LEFT;
      if Predict then gPlayer1.PressKey(KEY_LEFT, 10000);
    end;
    if P1MoveButton = 2 then
    begin
      kByte := kByte or NET_KEY_RIGHT;
      if Predict then gPlayer1.PressKey(KEY_RIGHT, 10000);
    end;
    if gPlayerAction[0, ACTION_LOOKUP] then
    begin
      kByte := kByte or NET_KEY_UP;
      gPlayer1.PressKey(KEY_UP, 10000);
    end;
    if gPlayerAction[0, ACTION_LOOKDOWN] then
    begin
      kByte := kByte or NET_KEY_DOWN;
      gPlayer1.PressKey(KEY_DOWN, 10000);
    end;
    if gPlayerAction[0, ACTION_JUMP] then
    begin
      kByte := kByte or NET_KEY_JUMP;
      // gPlayer1.PressKey(KEY_JUMP, 10000); // TODO: Make a prediction option
    end;
    if gPlayerAction[0, ACTION_ATTACK] then kByte := kByte or NET_KEY_FIRE;
    if gPlayerAction[0, ACTION_ACTIVATE] then kByte := kByte or NET_KEY_OPEN;

    for i := WP_FACT to WP_LACT do
    begin
      if gWeaponAction[0, i] then
      begin
        WeaponAct := WeaponAct or Byte(1 shl i);
        gWeaponAction[0, i] := False
      end
    end;

    for i := WP_FIRST to WP_LAST do
    begin
      if gSelectWeapon[0, i] then
      begin
        WeaponSelect := WeaponSelect or Word(1 shl i);
        gSelectWeapon[0, i] := False
      end
    end;

    // fix movebutton state
    P1MoveButton := P1MoveButton or (strafeDir shl 4);
  end
  else
    kByte := NET_KEY_CHAT;

  NetOut.Write(Byte(NET_MSG_PLRPOS));
  NetOut.Write(gTime);
  NetOut.Write(kByte);
  NetOut.Write(Byte(gPlayer1.Direction));
  NetOut.Write(WeaponAct);
  NetOut.Write(WeaponSelect);
  //e_WriteLog(Format('S:ws=%d', [WeaponSelect]), MSG_WARNING);
  g_Net_Client_Send(True, NET_CHAN_PLAYERPOS);

  //kBytePrev := kByte;
  //kDirPrev := gPlayer1.Direction;
end;

procedure MC_SEND_Vote(Start: Boolean = False; Command: string = 'a');
begin
  NetOut.Write(Byte(NET_MSG_VOTE_EVENT));
  NetOut.Write(Byte(Start));
  NetOut.Write(Command);
  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;

procedure MC_SEND_PlayerSettings();
var i: Integer;
begin
  NetOut.Write(Byte(NET_MSG_PLRSET));
  NetOut.Write(gPlayer1Settings.Name);
  NetOut.Write(gPlayer1Settings.Model);
  NetOut.Write(gPlayer1Settings.Color.R);
  NetOut.Write(gPlayer1Settings.Color.G);
  NetOut.Write(gPlayer1Settings.Color.B);
  NetOut.Write(gPlayer1Settings.Team);
  NetOut.Write(gPlayer1Settings.WeaponSwitch);
  for i := WP_FIRST to WP_LAST + 1 do
    NetOut.Write(gPlayer1Settings.WeaponPreferences[i]);
  NetOut.Write(gPlayer1Settings.SwitchToEmpty);
  NetOut.Write(gPlayer1Settings.SkipFist);

  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;

procedure MC_SEND_FullStateRequest();
begin
  NetOut.Write(Byte(NET_MSG_REQFST));

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;

procedure MC_SEND_CheatRequest(Kind: Byte);
begin
  NetOut.Write(Byte(NET_MSG_CHEAT));
  NetOut.Write(Kind);

  g_Net_Client_Send(True, NET_CHAN_IMPORTANT);
end;
procedure MC_SEND_RCONPassword(Password: string);
begin
  NetOut.Write(Byte(NET_MSG_RCON_AUTH));
  NetOut.Write(Password);

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;
procedure MC_SEND_RCONCommand(Cmd: string);
begin
  NetOut.Write(Byte(NET_MSG_RCON_CMD));
  NetOut.Write(Cmd);

  g_Net_Client_Send(True, NET_CHAN_SERVICE);
end;


end.
