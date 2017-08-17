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
unit g_nethandler;

interface

uses g_net, g_netmsg, ENet;

procedure g_Net_ClientMsgHandler(P: pENetPacket);
procedure g_Net_ClientLightMsgHandler(P: pENetPacket);
procedure g_Net_HostMsgHandler(S: pTNetClient; P: pENetPacket);

implementation

uses e_fixedbuffer;

procedure g_Net_ClientMsgHandler(P: pENetPacket);
var
  MID: Byte;
  B: Pointer;
begin
  e_Raw_Seek(0);

  B := P^.data;
  if B = nil then Exit;

  MID := e_Raw_Read_Byte(B);

  case MID of
    NET_MSG_CHAT:   MC_RECV_Chat(B);
    NET_MSG_GFX:    MC_RECV_Effect(B);
    NET_MSG_SND:    MC_RECV_Sound(B);
    NET_MSG_SCORE:  MC_RECV_GameStats(B);
    NET_MSG_COOP:   MC_RECV_CoopStats(B);
    NET_MSG_GEVENT: MC_RECV_GameEvent(B);
    NET_MSG_FLAG:   MC_RECV_FlagEvent(B);
    NET_MSG_GSET:   MC_RECV_GameSettings(B);

    NET_MSG_PLR:    MC_RECV_PlayerCreate(B);
    NET_MSG_PLRPOS: MC_RECV_PlayerPos(B);
    NET_MSG_PLRSTA: MC_RECV_PlayerStats(B);
    NET_MSG_PLRDEL: MC_RECV_PlayerDelete(B);
    NET_MSG_PLRDMG: MC_RECV_PlayerDamage(B);
    NET_MSG_PLRDIE: MC_RECV_PlayerDeath(B);
    NET_MSG_PLRFIRE:MC_RECV_PlayerFire(B);
    NET_MSG_PLRSET: MC_RECV_PlayerSettings(B);

    NET_MSG_MSPAWN: MC_RECV_MonsterSpawn(B);
    NET_MSG_MPOS:   MC_RECV_MonsterPos(B);
    NET_MSG_MSTATE: MC_RECV_MonsterState(B);
    NET_MSG_MSHOT:  MC_RECV_MonsterShot(B);
    NET_MSG_MDEL:   MC_RECV_MonsterDelete(B);

    NET_MSG_SHADD:  MC_RECV_CreateShot(B);
    NET_MSG_SHPOS:  MC_RECV_UpdateShot(B);
    NET_MSG_SHDEL:  MC_RECV_DeleteShot(B);

    NET_MSG_ISPAWN: MC_RECV_ItemSpawn(B);
    NET_MSG_IDEL:   MC_RECV_ItemDestroy(B);

    NET_MSG_PSTATE: MC_RECV_PanelState(B);
    NET_MSG_PTEX:   MC_RECV_PanelTexture(B);

    NET_MSG_TSOUND: MC_RECV_TriggerSound(B);
    NET_MSG_TMUSIC: MC_RECV_TriggerMusic(B);

    NET_MSG_TIME_SYNC:  MC_RECV_TimeSync(B);
    NET_MSG_VOTE_EVENT: MC_RECV_VoteEvent(B);
  end;

  enet_packet_destroy(P);
end;

procedure g_Net_ClientLightMsgHandler(P: pENetPacket);
var
  MID: Byte;
  B: Pointer;
begin
  e_Raw_Seek(0);

  B := P^.data;
  if B = nil then Exit;

  MID := e_Raw_Read_Byte(B);

  case MID of
    NET_MSG_GEVENT: MC_RECV_GameEvent(B);
    NET_MSG_GSET:   MC_RECV_GameSettings(B);

    NET_MSG_PLR:    if NetState <> NET_STATE_AUTH then MC_RECV_PlayerCreate(B);
    NET_MSG_PLRDEL: if NetState <> NET_STATE_AUTH then MC_RECV_PlayerDelete(B);
  end;

  enet_packet_destroy(P);
end;

procedure g_Net_HostMsgHandler(S: pTNetClient; P: pENetPacket);
var
  MID: Byte;
  B: Pointer;
begin
  e_Raw_Seek(0);

  B := P^.data;
  if B = nil then Exit;

  MID := e_Raw_Read_Byte(B);

  case MID of
    NET_MSG_INFO: MH_RECV_Info(S, B);
    NET_MSG_CHAT: MH_RECV_Chat(S, B);
    NET_MSG_REQFST: MH_RECV_FullStateRequest(S, B);

    NET_MSG_PLRPOS: MH_RECV_PlayerPos(S, B);
    NET_MSG_PLRSET: MH_RECV_PlayerSettings(S, B);
    NET_MSG_CHEAT:  MH_RECV_CheatRequest(S, B);

    NET_MSG_RCON_AUTH: MH_RECV_RCONPassword(S, B);
    NET_MSG_RCON_CMD:  MH_RECV_RCONCommand(S, B);

    NET_MSG_MAP_REQUEST: MH_RECV_MapRequest(S, B);
    NET_MSG_RES_REQUEST: MH_RECV_ResRequest(S, B);

    NET_MSG_VOTE_EVENT: MH_RECV_Vote(S, B);
  end;

  enet_packet_destroy(P);
end;

end.
