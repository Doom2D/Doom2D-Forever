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
unit g_nethandler;

interface

uses e_msg, g_net, g_netmsg, ENet;

type
  TNetClientMsgHandler = function (M: TMsg): Boolean;
  TNetHostMsgHandler = function (S: pTNetClient; M: TMsg): Boolean;

procedure g_Net_Client_HandlePacket(P: pENetPacket; Handler: TNetClientMsgHandler);
procedure g_Net_Host_HandlePacket(S: pTNetClient; P: pENetPacket; Handler: TNetHostMsgHandler);

function g_Net_ClientMsgHandler(NetMsg: TMsg): Boolean;
function g_Net_ClientLightMsgHandler(NetMsg: TMsg): Boolean;
function g_Net_HostMsgHandler(S: pTNetClient; NetMsg: TMsg): Boolean;

implementation

uses sysutils, g_console;

procedure g_Net_Client_HandlePacket(P: pENetPacket; Handler: TNetClientMsgHandler);
var
  MNext: Integer;
  MSize: LongWord;
  MHandled: Boolean;
  NetMsg: TMsg;
begin
  if not NetMsg.Init(P^.data, P^.dataLength, True) then
    Exit;

  MNext := 0;

  while NetMsg.BytesLeft() > 0 do
  begin
    MSize := NetMsg.ReadLongWord();
    MNext := NetMsg.ReadCount + MSize;
    MHandled := Handler(NetMsg); // TODO: maybe do something with this bool
    NetMsg.Seek(MNext);
  end;

  enet_packet_destroy(P);
end;

procedure g_Net_Host_HandlePacket(S: pTNetClient; P: pENetPacket; Handler: TNetHostMsgHandler);
var
  MNext: Integer;
  MSize: LongWord;
  MHandled: Boolean;
  NetMsg: TMsg;
begin
  if not NetMsg.Init(P^.data, P^.dataLength, True) then
    Exit;

  MNext := 0;

  while NetMsg.BytesLeft() > 0 do
  begin
    MSize := NetMsg.ReadLongWord();
    MNext := NetMsg.ReadCount + MSize;
    MHandled := Handler(S, NetMsg); // TODO: maybe do something with this bool
    NetMsg.Seek(MNext);
  end;

  enet_packet_destroy(P);
end;


function g_Net_ClientMsgHandler(NetMsg: TMsg): Boolean;
var
  MID: Byte;
begin
  Result := True;
  MID := NetMsg.ReadByte();

  case MID of
    NET_MSG_CHAT:   MC_RECV_Chat(NetMsg);
    NET_MSG_GFX:    MC_RECV_Effect(NetMsg);
    NET_MSG_SND:    MC_RECV_Sound(NetMsg);
    NET_MSG_SCORE:  MC_RECV_GameStats(NetMsg);
    NET_MSG_COOP:   MC_RECV_CoopStats(NetMsg);
    NET_MSG_GEVENT: MC_RECV_GameEvent(NetMsg);
    NET_MSG_FLAG:   MC_RECV_FlagEvent(NetMsg);
    NET_MSG_GSET:   MC_RECV_GameSettings(NetMsg);

    NET_MSG_PLR:    MC_RECV_PlayerCreate(NetMsg);
    NET_MSG_PLRPOS: MC_RECV_PlayerPos(NetMsg);
    NET_MSG_PLRSTA: MC_RECV_PlayerStats(NetMsg);
    NET_MSG_PLRDEL: MC_RECV_PlayerDelete(NetMsg);
    NET_MSG_PLRDMG: MC_RECV_PlayerDamage(NetMsg);
    NET_MSG_PLRDIE: MC_RECV_PlayerDeath(NetMsg);
    NET_MSG_PLRFIRE:MC_RECV_PlayerFire(NetMsg);
    NET_MSG_PLRSET: MC_RECV_PlayerSettings(NetMsg);

    NET_MSG_MSPAWN: MC_RECV_MonsterSpawn(NetMsg);
    NET_MSG_MPOS:   MC_RECV_MonsterPos(NetMsg);
    NET_MSG_MSTATE: MC_RECV_MonsterState(NetMsg);
    NET_MSG_MSHOT:  MC_RECV_MonsterShot(NetMsg);
    NET_MSG_MDEL:   MC_RECV_MonsterDelete(NetMsg);

    NET_MSG_SHADD:  MC_RECV_CreateShot(NetMsg);
    NET_MSG_SHPOS:  MC_RECV_UpdateShot(NetMsg);
    NET_MSG_SHDEL:  MC_RECV_DeleteShot(NetMsg);

    NET_MSG_ISPAWN: MC_RECV_ItemSpawn(NetMsg);
    NET_MSG_IDEL:   MC_RECV_ItemDestroy(NetMsg);

    NET_MSG_PSTATE: MC_RECV_PanelState(NetMsg);
    NET_MSG_PTEX:   MC_RECV_PanelTexture(NetMsg);

    NET_MSG_TSOUND: MC_RECV_TriggerSound(NetMsg);
    NET_MSG_TMUSIC: MC_RECV_TriggerMusic(NetMsg);

    NET_MSG_TIME_SYNC:  MC_RECV_TimeSync(NetMsg);
    NET_MSG_VOTE_EVENT: MC_RECV_VoteEvent(NetMsg);

    else
    begin
      Result := False;
      g_Console_Add('unknown message ID: ' + IntToStr(MID));
    end;
  end;
end;

function g_Net_ClientLightMsgHandler(NetMsg: TMsg): Boolean;
var
  MID: Byte;
begin
  Result := True;
  MID := NetMsg.ReadByte();

  case MID of
    NET_MSG_GEVENT: MC_RECV_GameEvent(NetMsg);
    NET_MSG_GSET:   MC_RECV_GameSettings(NetMsg);

    NET_MSG_PLR:    if NetState <> NET_STATE_AUTH then MC_RECV_PlayerCreate(NetMsg);
    NET_MSG_PLRDEL: if NetState <> NET_STATE_AUTH then MC_RECV_PlayerDelete(NetMsg);

    else Result := False;
  end;
end;

function g_Net_HostMsgHandler(S: pTNetClient; NetMsg: TMsg): Boolean;
var
  MID: Byte;
begin
  Result := True;
  MID := NetMsg.ReadByte();

  case MID of
    NET_MSG_INFO: MH_RECV_Info(S, NetMsg);
    NET_MSG_CHAT: MH_RECV_Chat(S, NetMsg);
    NET_MSG_REQFST: MH_RECV_FullStateRequest(S, NetMsg);

    NET_MSG_PLRPOS: MH_RECV_PlayerPos(S, NetMsg);
    NET_MSG_PLRSET: MH_RECV_PlayerSettings(S, NetMsg);
    NET_MSG_CHEAT:  MH_RECV_CheatRequest(S, NetMsg);

    NET_MSG_RCON_AUTH: MH_RECV_RCONPassword(S, NetMsg);
    NET_MSG_RCON_CMD:  MH_RECV_RCONCommand(S, NetMsg);

    NET_MSG_MAP_REQUEST: MH_RECV_MapRequest(S, NetMsg);
    NET_MSG_RES_REQUEST: MH_RECV_ResRequest(S, NetMsg);

    NET_MSG_VOTE_EVENT: MH_RECV_Vote(S, NetMsg);
  end;
end;

end.
