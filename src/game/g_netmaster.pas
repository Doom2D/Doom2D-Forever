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
unit g_netmaster;

interface

uses
  ENet, SysUtils, e_msg;

const
  NET_MCHANS = 2;

  NET_MCHAN_MAIN = 0;
  NET_MCHAN_UPD  = 1;

  NET_MMSG_UPD = 200;
  NET_MMSG_DEL = 201;
  NET_MMSG_GET = 202;

type
  TNetServer = record
    Number: Byte;
    Protocol: Byte;
    Name: AnsiString;
    IP: AnsiString;
    Port: Word;
    Map: AnsiString;
    Players, MaxPlayers, LocalPl, Bots: Byte;
    Ping: Int64;
    GameMode: Byte;
    Password: Boolean;
    PingAddr: ENetAddress;
  end;
  pTNetServer = ^TNetServer;
  TNetServerRow = record
    Indices: Array of Integer;
    Current: Integer;
  end;

  TNetServerList = array of TNetServer;
  pTNetServerList = ^TNetServerList;
  TNetServerTable = array of TNetServerRow;

type
  TMasterHost = record
  public
    hostName: AnsiString;

  public
    peer: pENetPeer;
    enetAddr: ENetAddress;
    // inside the game, calling `connect()` is disasterous, as it is blocking.
    // so we'll use this variable to indicate if "connected" event is received.
    NetHostConnected: Boolean;
    NetHostConReqTime: Int64; // to timeout `connect`; -1 means "waiting for shutdown"
    NetUpdatePending: Boolean; // should we send an update after connection completes?
    lastConnectTime: Int64;
    updateSent: Boolean;
    lastUpdateTime: Int64;
    // server list request working flags
    srvAnswered: Integer;
    srvAnswer: array of TNetServer;
    slMOTD: AnsiString;
    slUrgent: AnsiString;
    slReadUrgent: Boolean;
    // temporary mark
    justAdded: Boolean;

  private
    netmsg: TMsg;

  public
    constructor Create (var ea: ENetAddress);

    procedure clear ();

    function setAddress (var ea: ENetAddress; hostStr: AnsiString): Boolean;

    function isValid (): Boolean;
    function isAlive (): Boolean; // not disconnected
    function isConnecting (): Boolean; // is connection in progress?
    function isConnected (): Boolean;

    // call as often as you want, the object will do the rest
    // but try to call this at least once in 100 msecs
    procedure pulse ();

    procedure disconnect (forced: Boolean);
    function connect (): Boolean;

    procedure update ();
    procedure remove ();

    class procedure writeInfo (var msg: TMsg); static;

    procedure connectedEvent ();
    procedure disconnectedEvent ();
    procedure receivedEvent (pkt: pENetPacket); // `pkt` is never `nil`
  end;


var
  slCurrent:       TNetServerList = nil;
  slTable:         TNetServerTable = nil;
  slWaitStr:       AnsiString = '';
  slReturnPressed: Boolean = True;

  slMOTD: AnsiString = '';
  slUrgent: AnsiString = '';


procedure g_Net_Slist_Set (IP: AnsiString; Port: Word; list: AnsiString='');
function g_Net_Slist_Fetch (var SL: TNetServerList): Boolean;

// make this server private
procedure g_Net_Slist_Private ();
// make this server public
procedure g_Net_Slist_Public ();

// called while the server is running
procedure g_Net_Slist_ServerUpdate ();
// called when the server is started
procedure g_Net_Slist_ServerStarted ();
// called when the server is stopped
procedure g_Net_Slist_ServerClosed ();

// called when new netword player comes
procedure g_Net_Slist_ServerPlayerComes ();
// called when new netword player comes
procedure g_Net_Slist_ServerPlayerLeaves ();
// started new map
procedure g_Net_Slist_ServerMapStarted ();
// this server renamed (or password mode changed, or other params changed)
procedure g_Net_Slist_ServerRenamed ();

// non-zero timeout ignores current status (used to fetch server list)
procedure g_Net_Slist_Pulse (timeout: Integer=0);

procedure g_Net_Slist_ShutdownAll ();

procedure g_Serverlist_GenerateTable (SL: TNetServerList; var ST: TNetServerTable);
procedure g_Serverlist_Draw (var SL: TNetServerList; var ST: TNetServerTable);
procedure g_Serverlist_Control (var SL: TNetServerList; var ST: TNetServerTable);

function GetTimerMS (): Int64;


implementation

uses
  e_input, e_graphics, e_log, g_window, g_net, g_console,
  g_map, g_game, g_sound, g_gui, g_menu, g_options, g_language, g_basic,
  wadreader, g_system, utils, hashtable;


// ////////////////////////////////////////////////////////////////////////// //
var
  NetMHost: pENetHost = nil;
  NetMEvent: ENetEvent;
  mlist: array of TMasterHost = nil;

  slSelection: Byte = 0;
  slFetched: Boolean = False;
  slDirPressed: Boolean = False;
  slReadUrgent: Boolean = False;

  reportsEnabled: Boolean = true;


//==========================================================================
//
//  GetTimerMS
//
//==========================================================================
function GetTimerMS (): Int64;
begin
  Result := sys_GetTicks() {div 1000};
end;


//==========================================================================
//
//  findByPeer
//
//==========================================================================
function findByPeer (peer: pENetPeer): Integer;
var
  f: Integer;
begin
  for f := 0 to High(mlist) do if (mlist[f].peer = peer) then begin result := f; exit; end;
  result := -1;
end;


//==========================================================================
//
//  ShutdownAll
//
//==========================================================================
procedure g_Net_Slist_ShutdownAll ();
var
  f, sres, idx: Integer;
  stt, ct: Int64;
  activeCount: Integer = 0;
begin
  if (NetMHost = nil) then exit;
  for f := 0 to High(mlist) do
  begin
    if (mlist[f].isAlive()) then
    begin
      Inc(activeCount);
      if (mlist[f].isConnected() and mlist[f].updateSent) then
      begin
        writeln('unregistering from [', mlist[f].hostName, ']');
        mlist[f].remove();
      end;
      //mlist[f].disconnect(false);
      enet_peer_disconnect_later(mlist[f].peer, 0);
    end;
  end;
  if (activeCount = 0) then exit;
  stt := GetTimerMS();
  while (activeCount > 0) do
  begin
    ct := GetTimerMS();
    if (ct < stt) or (ct-stt >= 1500) then break;

    sres := enet_host_service(NetMHost, @NetMEvent, 100);
    // if (sres < 0) then break;
    if (sres <= 0) then continue;

    idx := findByPeer(NetMEvent.peer);
    if (idx < 0) then
    begin
      if (NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE) then enet_packet_destroy(NetMEvent.packet);
      continue;
    end;

    if (NetMEvent.kind = ENET_EVENT_TYPE_CONNECT) then
    begin
      mlist[idx].connectedEvent();
      //mlist[idx].disconnect(false);
      enet_peer_disconnect(mlist[f].peer, 0);
    end
    else if (NetMEvent.kind = ENET_EVENT_TYPE_DISCONNECT) then
    begin
      mlist[idx].disconnectedEvent();
      Dec(activeCount);
    end
    else if (NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE) then
    begin
      mlist[idx].receivedEvent(NetMEvent.packet);
      enet_packet_destroy(NetMEvent.packet);
    end;
  end;
  enet_host_destroy(NetMHost);
  NetMHost := nil;
end;


//==========================================================================
//
//  DisconnectAll
//
//==========================================================================
procedure DisconnectAll (forced: Boolean=false);
var
  f: Integer;
begin
  for f := 0 to High(mlist) do
  begin
    if (mlist[f].isAlive()) then mlist[f].disconnect(forced);
  end;
end;


//==========================================================================
//
//  ConnectAll
//
//==========================================================================
procedure ConnectAll (sendUpdate: Boolean);
var
  f: Integer;
begin
  for f := 0 to High(mlist) do
  begin
    // force reconnect
    mlist[f].lastConnectTime := 0;
    //if (not mlist[f].isAlive()) then continue;
    // force updating
    if (sendUpdate) then
    begin
      mlist[f].NetUpdatePending := true;
      mlist[f].lastUpdateTime := 0;
    end;
  end;
end;


//==========================================================================
//
//  UpdateAll
//
//==========================================================================
procedure UpdateAll (force: Boolean);
var
  f: Integer;
begin
  for f := 0 to High(mlist) do
  begin
    if (not mlist[f].isAlive()) then continue;
    mlist[f].NetUpdatePending := true;
    if (force) then mlist[f].lastUpdateTime := 0;
  end;
end;


//**************************************************************************
//
// public api
//
//**************************************************************************

//==========================================================================
//
//  g_Net_Slist_Private
//
//  make this server private
//
//==========================================================================
procedure g_Net_Slist_Private ();
begin
  DisconnectAll();
  reportsEnabled := false;
end;


//==========================================================================
//
//  g_Net_Slist_Public
//
//  make this server public
//
//==========================================================================
procedure g_Net_Slist_Public ();
begin
  if (not reportsEnabled) then
  begin
    reportsEnabled := true;
    ConnectAll(true);
  end;
end;


//==========================================================================
//
//  g_Net_Slist_ServerUpdate
//
//  called while the server is running
//
//==========================================================================
procedure g_Net_Slist_ServerUpdate ();
begin
  UpdateAll(false);
end;


// called when the server is started
procedure g_Net_Slist_ServerStarted ();
begin
  reportsEnabled := NetUseMaster;
  if reportsEnabled and g_Game_IsServer() and g_Game_IsNet() then
  begin
    writeln('*** server started; reporting to master...');
    ConnectAll(true);
  end;
end;


//==========================================================================
//
//  g_Net_Slist_ServerClosed
//
//  called when the server is stopped
//
//==========================================================================
procedure g_Net_Slist_ServerClosed ();
var
  f: Integer;
begin
  if reportsEnabled then
  begin
    reportsEnabled := false;
    for f := 0 to High(mlist) do
    begin
      if (mlist[f].isConnected()) then mlist[f].remove();
    end;
  end;
  DisconnectAll();
end;


//==========================================================================
//
//  g_Net_Slist_ServerPlayerComes
//
//  called when new netword player comes
//
//==========================================================================
procedure g_Net_Slist_ServerPlayerComes ();
begin
  UpdateAll(true);
end;


//==========================================================================
//
//  g_Net_Slist_ServerPlayerLeaves
//
//  called when new netword player comes
//
//==========================================================================
procedure g_Net_Slist_ServerPlayerLeaves ();
begin
  UpdateAll(true);
end;


//==========================================================================
//
//  g_Net_Slist_ServerMapStarted
//
//  started new map
//
//==========================================================================
procedure g_Net_Slist_ServerMapStarted ();
begin
  UpdateAll(true);
end;


//==========================================================================
//
//  g_Net_Slist_ServerRenamed
//
//  this server renamed (or password mode changed, or other params changed)
//
//==========================================================================
procedure g_Net_Slist_ServerRenamed ();
begin
  UpdateAll(true);
end;


//**************************************************************************
//
// TMasterHost
//
//**************************************************************************

//==========================================================================
//
//  TMasterHost.Create
//
//==========================================================================
constructor TMasterHost.Create (var ea: ENetAddress);
begin
  peer := nil;
  NetHostConnected := false;
  NetHostConReqTime := 0;
  NetUpdatePending := false;
  lastConnectTime := 0;
  updateSent := false;
  lastUpdateTime := 0;
  hostName := '';
  ZeroMemory(@enetAddr, sizeof(enetAddr));
  SetLength(srvAnswer, 0);
  srvAnswered := 0;
  slMOTD := '';
  slUrgent := '';
  slReadUrgent := true;
  netmsg.Alloc(NET_BUFSIZE);
  setAddress(ea, '');
end;


//==========================================================================
//
//  TMasterHost.clear
//
//==========================================================================
procedure TMasterHost.clear ();
begin
  updateSent := false; // do not send 'remove'
  disconnect(true);
  hostName := '';
  netmsg.Free();
  SetLength(srvAnswer, 0);
  srvAnswered := 0;
  slMOTD := '';
  slUrgent := '';
  slReadUrgent := true;
  ZeroMemory(@enetAddr, sizeof(enetAddr));
end;


//==========================================================================
//
//  TMasterHost.setAddress
//
//==========================================================================
function TMasterHost.setAddress (var ea: ENetAddress; hostStr: AnsiString): Boolean;
begin
  result := false;
  SetLength(srvAnswer, 0);
  srvAnswered := 0;
  slMOTD := '';
  slUrgent := '';
  slReadUrgent := true;
  updateSent := false; // do not send 'remove'
  disconnect(true);
  hostName := '';

  if (not g_Net_IsNetworkAvailable()) then exit;

  enetAddr := ea;
  if (enetAddr.host = 0) or (enetAddr.port = 0) then exit;

  if (length(hostStr) > 0) then hostName := hostStr else hostName := IntToStr(enetAddr.host)+':'+IntToStr(ea.port);

  result := isValid();
end;


//==========================================================================
//
//  TMasterHost.isValid
//
//==========================================================================
function TMasterHost.isValid (): Boolean;
begin
  result := (enetAddr.host <> 0) and (enetAddr.port <> 0);
end;


//==========================================================================
//
//  TMasterHost.isAlive
//
//  not disconnected
//
//==========================================================================
function TMasterHost.isAlive (): Boolean;
begin
  result := (NetMHost <> nil) and (peer <> nil);
end;


//==========================================================================
//
//  TMasterHost.isConnecting
//
//  is connection in progress?
//
//==========================================================================
function TMasterHost.isConnecting (): Boolean;
begin
  result := isAlive() and (not NetHostConnected) and (NetHostConReqTime <> -1);
end;


//==========================================================================
//
//  TMasterHost.isConnected
//
//==========================================================================
function TMasterHost.isConnected (): Boolean;
begin
  result := isAlive() and (NetHostConnected) and (NetHostConReqTime <> -1);
end;


//==========================================================================
//
//  TMasterHost.connectedEvent
//
//==========================================================================
procedure TMasterHost.connectedEvent ();
begin
  if not isAlive() then exit;
  if NetHostConnected then exit;
  NetHostConnected := true;
  e_LogWritefln('connected to master at [%s]', [hostName], TMsgType.Notify);
  //g_Console_Add(Format(_lc[I_NET_MSG]+_lc[I_NET_SLIST_CONN], [mlist[f].hostName]));
end;


//==========================================================================
//
//  TMasterHost.disconnectedEvent
//
//==========================================================================
procedure TMasterHost.disconnectedEvent ();
begin
  if not isAlive() then exit;
  e_LogWritefln('disconnected from master at [%s]', [hostName], TMsgType.Notify);
  disconnect(true);
  //if (spamConsole) then g_Console_Add(Format(_lc[I_NET_MSG]+_lc[I_NET_SLIST_DISC], [hostName]));
end;


//==========================================================================
//
//  TMasterHost.receivedEvent
//
//  `pkt` is never `nil`
//
//==========================================================================
procedure TMasterHost.receivedEvent (pkt: pENetPacket);
var
  msg: TMsg;
  MID: Byte;
  Cnt: Byte;
  f: Integer;
  s: AnsiString;
begin
  e_LogWritefln('received packed from master at [%s]', [hostName], TMsgType.Notify);
  if not msg.Init(pkt^.data, pkt^.dataLength, True) then exit;
  // packet type
  MID := msg.ReadByte();
  if (MID <> NET_MMSG_GET) then exit;
  e_LogWritefln('received list packet from master at [%s]', [hostName], TMsgType.Notify);
  SetLength(srvAnswer, 0);
  if (srvAnswered > 0) then Inc(srvAnswered);
  slMOTD := '';
  //slUrgent := '';
  slReadUrgent := true;
  // number of items
  Cnt := msg.ReadByte();
  //g_Console_Add(_lc[I_NET_MSG]+Format(_lc[I_NET_SLIST_RETRIEVED], [Cnt, hostName]), True);
  e_LogWritefln('got %u server(s) from master at [%s]', [Cnt, hostName], TMsgType.Notify);
  if (Cnt > 0) then
  begin
    SetLength(srvAnswer, Cnt);
    for f := 0 to Cnt-1 do
    begin
      srvAnswer[f].Number := f;
      srvAnswer[f].IP := msg.ReadString();
      srvAnswer[f].Port := msg.ReadWord();
      srvAnswer[f].Name := msg.ReadString();
      srvAnswer[f].Map := msg.ReadString();
      srvAnswer[f].GameMode := msg.ReadByte();
      srvAnswer[f].Players := msg.ReadByte();
      srvAnswer[f].MaxPlayers := msg.ReadByte();
      srvAnswer[f].Protocol := msg.ReadByte();
      srvAnswer[f].Password := msg.ReadByte() = 1;
      enet_address_set_host(Addr(srvAnswer[f].PingAddr), PChar(Addr(srvAnswer[f].IP[1])));
      srvAnswer[f].Ping := -1;
      srvAnswer[f].PingAddr.port := NET_PING_PORT;
    end;
  end;

  if (msg.ReadCount < msg.CurSize) then
  begin
    // new master, supports version reports
    s := msg.ReadString();
    if (s <> {MyVer}GAME_VERSION) then
    begin
      { TODO }
      g_Console_Add('!!! UpdVer = `'+s+'`');
    end;
    // even newer master, supports extra info
    if (msg.ReadCount < msg.CurSize) then
    begin
      slMOTD := b_Text_Format(msg.ReadString());
      if (slMOTD <> '') then e_LogWritefln('got MOTD from master at [%s]: %s', [hostName, slMOTD], TMsgType.Notify);
      s := b_Text_Format(msg.ReadString());
      // check if the message has updated and the user has to read it again
      if (slUrgent <> s) then slReadUrgent := false;
      slUrgent := s;
      if (s <> '') then e_LogWritefln('got urgent from master at [%s]: %s', [hostName, s], TMsgType.Notify);
    end;
  end;
end;


//==========================================================================
//
//  TMasterHost.pulse
//
//  this performs various scheduled tasks, if necessary
//
//==========================================================================
procedure TMasterHost.pulse ();
var
  ct: Int64;
  mrate: Cardinal;
begin
  if not isAlive() then exit;
  if (NetHostConReqTime = -1) then exit; // waiting for shutdown (disconnect in progress)
  ct := GetTimerMS();
  // process pending connection timeout
  if (not NetHostConnected) then
  begin
    if (ct < NetHostConReqTime) or (ct-NetHostConReqTime >= 3000) then
    begin
      e_LogWritefln('failed to connect to master at [%s]', [hostName], TMsgType.Notify);
      // do not spam with error messages, it looks like the master is down
      //g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_SLIST_ERROR], True);
      enet_peer_disconnect(peer, 0);
      // main pulse will take care of the rest
      NetHostConReqTime := -1;
    end;
    exit;
  end;
  // send update, if necessary
  if (NetUpdatePending) then
  begin
    mrate := NetMasterRate;
         if (mrate < 10000) then mrate := 10000
    else if (mrate > 1000*60*10) then mrate := 1000*60*10;
    if (lastUpdateTime = 0) or (ct < lastUpdateTime) or (ct-lastUpdateTime >= mrate) then
    begin
      lastUpdateTime := ct;
      update();
    end;
  end;
end;


//==========================================================================
//
//  TMasterHost.disconnect
//
//==========================================================================
procedure TMasterHost.disconnect (forced: Boolean);
begin
  if not isAlive() then exit;

  if (forced) then
  begin
    enet_peer_reset(peer);
    peer := nil;
    NetHostConReqTime := 0;
  end
  else
  begin
    enet_peer_disconnect_later(peer, 0);
    // main pulse will take care of the rest
    NetHostConReqTime := -1;
  end;

  NetHostConnected := false;
  NetUpdatePending := false;
  //updateSent := false;
  lastUpdateTime := 0;
  //lastConnectTime := 0;
  //if (spamConsole) then g_Console_Add(Format(_lc[I_NET_MSG]+_lc[I_NET_SLIST_DISC], [hostName]));
end;


//==========================================================================
//
//  TMasterHost.connect
//
//==========================================================================
function TMasterHost.connect (): Boolean;
begin
  result := false;
  if not isValid() then exit;
  if (NetHostConReqTime = -1) then
  begin
    disconnect(true);
  end
  else
  begin
    if isAlive() then begin result := true; exit; end;
  end;

  lastConnectTime := GetTimerMS();
  SetLength(srvAnswer, 0);
  srvAnswered := 0;
  NetHostConnected := false;
  NetHostConReqTime := 0;
  NetUpdatePending := false;
  updateSent := false;
  lastUpdateTime := 0;

  peer := enet_host_connect(NetMHost, @enetAddr, NET_MCHANS, 0);
  if (peer = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR]+_lc[I_NET_ERR_CLIENT], true);
    exit;
  end;

  NetHostConReqTime := lastConnectTime;
  e_LogWritefln('connecting to master at [%s]', [hostName], TMsgType.Notify);
end;


//==========================================================================
//
//  TMasterHost.writeInfo
//
//==========================================================================
class procedure TMasterHost.writeInfo (var msg: TMsg);
var
  wad, map: AnsiString;
begin
  wad := g_ExtractWadNameNoPath(gMapInfo.Map);
  map := g_ExtractFileName(gMapInfo.Map);

  msg.Write(NetServerName);

  msg.Write(wad+':/'+map);
  msg.Write(gGameSettings.GameMode);

  msg.Write(Byte(NetClientCount));

  msg.Write(NetMaxClients);

  msg.Write(Byte(NET_PROTOCOL_VER));
  msg.Write(Byte(NetPassword <> ''));
end;


//==========================================================================
//
//  TMasterHost.update
//
//==========================================================================
procedure TMasterHost.update ();
var
  pkt: pENetPacket;
begin
  if not isAlive() then exit;
  if not isConnected() then
  begin
    NetUpdatePending := isConnecting();
    exit;
  end;

  netmsg.Clear();

  if reportsEnabled and g_Game_IsServer() and g_Game_IsNet() and NetUseMaster then
  begin
    try
      netmsg.Write(Byte(NET_MMSG_UPD));
      netmsg.Write(NetAddr.port);
      //writeln(formatstrf('%08x', [NetAddr.host]), ' : ', NetAddr.host);

      writeInfo(netmsg);

      pkt := enet_packet_create(netmsg.Data, netmsg.CurSize, ENET_PACKET_FLAG_RELIABLE);
      if assigned(pkt) then
      begin
        if (enet_peer_send(peer, NET_MCHAN_UPD, pkt) = 0) then
        begin
          e_LogWritefln('sent update to master at [%s]', [hostName], TMsgType.Notify);
          NetUpdatePending := false;
          updateSent := true;
        end;
      end;
    finally
      netmsg.Clear();
    end;
  end
  else
  begin
    NetUpdatePending := false;
  end;
end;


//==========================================================================
//
//  TMasterHost.remove
//
//==========================================================================
procedure TMasterHost.remove ();
var
  pkt: pENetPacket;
begin
  NetUpdatePending := false;
  lastUpdateTime := 0;
  updateSent := false;
  if not isAlive() then exit;
  if not isConnected() then exit;

  netmsg.Clear();
  try
    netmsg.Write(Byte(NET_MMSG_DEL));
    netmsg.Write(NetAddr.port);

    pkt := enet_packet_create(netmsg.Data, netmsg.CurSize, ENET_PACKET_FLAG_RELIABLE);
    if assigned(pkt) then
    begin
      enet_peer_send(peer, NET_MCHAN_MAIN, pkt);
    end;
  finally
    netmsg.Clear();
  end;
end;


//**************************************************************************
//
// other functions
//
//**************************************************************************
type
  THashStrDWord = specialize THashBase<AnsiString, LongWord, THashKeyStrAnsiCI>;

var
  knownHosts: THashStrDWord = nil;


//==========================================================================
//
//  parseAddressPort
//
//==========================================================================
function parseAddressPort (var ea: ENetAddress; hostandport: AnsiString): Boolean;
var
  cp, port: Integer;
  hostName: AnsiString;
  ip: LongWord;
begin
  result := false;
  if (not g_Net_IsNetworkAvailable()) then exit;

  hostandport := Trim(hostandport);
  if (length(hostandport) = 0) then exit;

  hostName := hostandport;
  port := 25665;

  cp := Pos(':', hostandport);
  if (cp > 0) then
  begin
    hostName := Trim(Copy(hostandport, 1, cp-1));
    Delete(hostandport, 1, cp);
    hostandport := Trim(hostandport);
    if (length(hostandport) > 0) then
    begin
      try
        port := StrToInt(hostandport);
      except
        port := -1;
      end;
    end;
  end;

  if (length(hostName) = 0) then exit;
  if (port < 1) or (port > 65535) then exit;

  if not assigned(knownHosts) then knownHosts := THashStrDWord.Create();

  if knownHosts.get(hostName, ip) then
  begin
    ea.host := ip;
  end
  else
  begin
    if (enet_address_set_host(@ea, PChar(Addr(hostName[1]))) <> 0) then
    begin
      knownHosts.put(hostName, 0);
      exit;
    end;
    knownHosts.put(hostName, ea.host);
  end;
  ea.Port := port;
  result := true;
end;


//==========================================================================
//
//  addMasterRecord
//
//==========================================================================
procedure addMasterRecord (var ea: ENetAddress; sa: AnsiString);
var
  f: Integer;
  freeIdx: Integer;
begin
  freeIdx := -1;
  for f := 0 to High(mlist) do
  begin
    if (mlist[f].enetAddr.host = ea.host) and (mlist[f].enetAddr.port = ea.port) then
    begin
      mlist[f].justAdded := true;
      exit;
    end;
    if (freeIdx < 0) and (not mlist[f].isValid()) then freeIdx := f;
  end;
  if (freeIdx < 0) then
  begin
    freeIdx := length(mlist);
    SetLength(mlist, freeIdx+1);
    mlist[freeIdx].Create(ea);
  end;
  mlist[freeIdx].justAdded := true;
  mlist[freeIdx].setAddress(ea, sa);
  e_LogWritefln('added masterserver with address [%s]', [sa], TMsgType.Notify);
end;


//==========================================================================
//
//  g_Net_Slist_Set
//
//==========================================================================
procedure g_Net_Slist_Set (IP: AnsiString; Port: Word; list: AnsiString='');
var
  f, dest: Integer;
  sa: AnsiString;
  ea: ENetAddress;
  pp: Integer;
begin
  if (not g_Net_IsNetworkAvailable()) then exit;

  for f := 0 to High(mlist) do mlist[f].justAdded := false;

  IP := Trim(IP);
  if (length(IP) > 0) and (Port > 0) then
  begin
    sa := IP+':'+IntToStr(Port);
    if parseAddressPort(ea, sa) then addMasterRecord(ea, sa);
  end;

  list := Trim(list);
  //writeln('list=[', list, ']');
  while (length(list) > 0) do
  begin
    pp := Pos(',', list);
    if (pp < 1) then pp := length(list)+1;
    sa := Trim(Copy(list, 1, pp-1));
    Delete(list, 1, pp);
    //writeln('  sa=[', sa, ']');
    if (length(sa) > 0) and parseAddressPort(ea, sa) then addMasterRecord(ea, sa);
  end;

  // remove unknown master servers
  dest := 0;
  for f := 0 to High(mlist) do
  begin
    if (not mlist[f].justAdded) then mlist[f].clear();
    if (mlist[f].isValid()) then
    begin
      if (dest <> f) then mlist[dest] := mlist[f];
      Inc(dest);
    end;
  end;
  if (dest <> length(mlist)) then SetLength(mlist, dest);
end;


//**************************************************************************
//
// main pulse
//
//**************************************************************************

//==========================================================================
//
//  g_Net_Slist_Pulse
//
//  non-zero timeout ignores current status (used to fetch server list)
//
//==========================================================================
procedure g_Net_Slist_Pulse (timeout: Integer=0);
var
  f: Integer;
  sres: Integer;
  idx: Integer;
  ct: Int64;
begin
  if (not g_Net_IsNetworkAvailable()) then exit;

  if (length(mlist) = 0) then
  begin
    if (NetMHost <> nil) then
    begin
      enet_host_destroy(NetMHost);
      NetMHost := nil;
      exit;
    end;
  end;

  if (NetMHost = nil) then
  begin
    NetMHost := enet_host_create(nil, 64, NET_MCHANS, 1024*1024, 1024*1024);
    if (NetMHost = nil) then
    begin
      e_LogWriteln(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CLIENT] + ' (host_create)', TMsgType.Notify);
      for f := 0 to High(mlist) do mlist[f].clear();
      SetLength(mlist, 0);
      Exit;
    end;
  end;

  ct := GetTimerMS();
  for f := 0 to High(mlist) do
  begin
    if (not mlist[f].isValid()) then continue;
    if (not mlist[f].isAlive()) then
    begin
      if (timeout > 0) or (reportsEnabled and g_Game_IsServer() and g_Game_IsNet() and NetUseMaster) then
      begin
        if (mlist[f].lastConnectTime = 0) or (ct < mlist[f].lastConnectTime) or (ct-mlist[f].lastConnectTime >= 1000*60*5) then
        begin
          e_LogWritefln('reconnecting to master [%s]', [mlist[f].hostName], TMsgType.Notify);
          mlist[f].connect();
        end;
      end;
    end
    else
    begin
      if (timeout = 0) and (not reportsEnabled or not g_Game_IsServer() or not g_Game_IsNet() or not NetUseMaster) then
      begin
        if (mlist[f].isConnected()) and (mlist[f].updateSent) then
        begin
          e_LogWritefln('removing from master [%s]', [mlist[f].hostName], TMsgType.Notify);
          mlist[f].remove();
        end;
        //e_LogWritefln('disconnecting from master [%s]', [mlist[f].hostName], TMsgType.Notify);
        mlist[f].disconnect(false);
      end;
    end;
    mlist[f].pulse();
  end;

  sres := enet_host_service(NetMHost, @NetMEvent, timeout);
  while (sres > 0) do
  begin
    if (sres < 0) then
    begin
      e_LogWriteln(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CLIENT] + ' (host_service)', TMsgType.Notify);
      for f := 0 to High(mlist) do mlist[f].clear();
      SetLength(mlist, 0);
      enet_host_destroy(NetMHost);
      NetMHost := nil;
      exit;
    end;

    idx := findByPeer(NetMEvent.peer);
    if (idx < 0) then
    begin
      e_LogWriteln('network event from unknown master host. ignored.', TMsgType.Warning);
      if (NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE) then enet_packet_destroy(NetMEvent.packet);
    end
    else
    begin
      if (NetMEvent.kind = ENET_EVENT_TYPE_CONNECT) then
      begin
        mlist[idx].connectedEvent();
      end
      else if (NetMEvent.kind = ENET_EVENT_TYPE_DISCONNECT) then
      begin
        mlist[idx].disconnectedEvent();
      end
      else if (NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE) then
      begin
        mlist[idx].receivedEvent(NetMEvent.packet);
        enet_packet_destroy(NetMEvent.packet);
      end;
    end;
    sres := enet_host_service(NetMHost, @NetMEvent, 0);
  end;
end;


//**************************************************************************
//
// gui and server list
//
//**************************************************************************

//==========================================================================
//
//  PingServer
//
//==========================================================================
procedure PingServer (var S: TNetServer; Sock: ENetSocket);
var
  Buf: ENetBuffer;
  Ping: array [0..9] of Byte;
  ClTime: Int64;
begin
  ClTime := GetTimerMS();

  Buf.data := Addr(Ping[0]);
  Buf.dataLength := 2+8;

  Ping[0] := Ord('D');
  Ping[1] := Ord('F');
  Int64(Addr(Ping[2])^) := ClTime;

  enet_socket_send(Sock, Addr(S.PingAddr), @Buf, 1);
end;


//==========================================================================
//
//  PingBcast
//
//==========================================================================
procedure PingBcast (Sock: ENetSocket);
var
  S: TNetServer;
begin
  S.IP := '255.255.255.255';
  S.Port := NET_PING_PORT;
  enet_address_set_host(Addr(S.PingAddr), PChar(Addr(S.IP[1])));
  S.Ping := -1;
  S.PingAddr.port := S.Port;
  PingServer(S, Sock);
end;


//==========================================================================
//
//  g_Net_Slist_Fetch
//
//==========================================================================
function g_Net_Slist_Fetch (var SL: TNetServerList): Boolean;
var
  Cnt: Byte;
  pkt: pENetPacket;
  I, RX: Integer;
  T: Int64;
  Sock: ENetSocket;
  Buf: ENetBuffer;
  InMsg: TMsg;
  SvAddr: ENetAddress;
  FromSL: Boolean;
  MyVer: AnsiString;

  procedure ProcessLocal ();
  begin
    I := Length(SL);
    SetLength(SL, I + 1);
    with SL[I] do
    begin
      IP := DecodeIPV4(SvAddr.host);
      Port := InMsg.ReadWord();
      Ping := InMsg.ReadInt64();
      Ping := GetTimerMS() - Ping;
      Name := InMsg.ReadString();
      Map := InMsg.ReadString();
      GameMode := InMsg.ReadByte();
      Players := InMsg.ReadByte();
      MaxPlayers := InMsg.ReadByte();
      Protocol := InMsg.ReadByte();
      Password := InMsg.ReadByte() = 1;
      LocalPl := InMsg.ReadByte();
      Bots := InMsg.ReadWord();
    end;
  end;

  procedure CheckLocalServers ();
  begin
    SetLength(SL, 0);

    Sock := enet_socket_create(ENET_SOCKET_TYPE_DATAGRAM);
    if Sock = ENET_SOCKET_NULL then Exit;
    enet_socket_set_option(Sock, ENET_SOCKOPT_NONBLOCK, 1);
    enet_socket_set_option(Sock, ENET_SOCKOPT_BROADCAST, 1);
    PingBcast(Sock);

    T := GetTimerMS();

    InMsg.Alloc(NET_BUFSIZE);
    Buf.data := InMsg.Data;
    Buf.dataLength := InMsg.MaxSize;
    while GetTimerMS() - T <= 500 do
    begin
      InMsg.Clear();

      RX := enet_socket_receive(Sock, @SvAddr, @Buf, 1);
      if RX <= 0 then continue;
      InMsg.CurSize := RX;

      InMsg.BeginReading();

      if InMsg.ReadChar() <> 'D' then continue;
      if InMsg.ReadChar() <> 'F' then continue;

      ProcessLocal();
    end;

    InMsg.Free();
    enet_socket_destroy(Sock);

    if Length(SL) = 0 then SL := nil;
  end;

var
  f, c, n, pos: Integer;
  aliveCount: Integer;
  hasUnanswered: Boolean;
  stt, ct: Int64;
begin
  result := false;
  SL := nil;

  if (not g_Net_IsNetworkAvailable()) then
  begin
    SetLength(SL, 0);
    exit;
  end;

  g_Net_Slist_Pulse(); // this will create mhost

  DisconnectAll(true); // forced disconnect

  NetOut.Clear();
  NetOut.Write(Byte(NET_MMSG_GET));

  // TODO: what should we identify the build with?
  MyVer := GAME_VERSION;
  NetOut.Write(MyVer);

  try
    e_WriteLog('Fetching serverlist...', TMsgType.Notify);
    g_Console_Add(_lc[I_NET_MSG]+_lc[I_NET_SLIST_FETCH]);

    // wait until all servers connected and answered
    stt := GetTimerMS();
    while true do
    begin
      aliveCount := 0;
      hasUnanswered := false;
      for f := 0 to High(mlist) do
      begin
        {
        e_LogWritefln('  master #%d: [%s] valid=%d; alive=%d; connected=%d; connecting=%d',
          [f, mlist[f].hostName, Integer(mlist[f].isValid()), Integer(mlist[f].isAlive()),
          Integer(mlist[f].isConnected()), Integer(mlist[f].isConnecting())], TMsgType.Notify);
        }
        if (not mlist[f].isValid()) then continue;
        if (not mlist[f].isAlive()) then
        begin
          mlist[f].connect();
          if (mlist[f].isAlive()) then
          begin
            //g_Console_Add(Format(_lc[I_NET_MSG]+_lc[I_NET_SLIST_WCONN], [mlist[f].hostName]));
            hasUnanswered := true;
            stt := GetTimerMS();
          end;
        end
        else if (mlist[f].isConnected()) then
        begin
          //g_Console_Add(Format(_lc[I_NET_MSG]+_lc[I_NET_SLIST_CONN], [mlist[f].hostName]));
          if (mlist[f].srvAnswered = 0) then
          begin
            pkt := enet_packet_create(NetOut.Data, NetOut.CurSize, Cardinal(ENET_PACKET_FLAG_RELIABLE));
            if assigned(pkt) then
            begin
              if (enet_peer_send(mlist[f].peer, NET_MCHAN_MAIN, pkt) = 0) then
              begin
                hasUnanswered := true;
                mlist[f].srvAnswered := 1;
                stt := GetTimerMS();
              end;
            end;
          end
          else if (mlist[f].srvAnswered = 1) then
          begin
            hasUnanswered := true;
          end
          else if (mlist[f].srvAnswered > 1) then
          begin
            Inc(aliveCount);
          end;
        end
        else if (mlist[f].isConnecting()) then
        begin
          hasUnanswered := true;
        end;
      end;
      if (not hasUnanswered) then break;
      // check for timeout
      ct := GetTimerMS();
      if (ct < stt) or (ct-stt > 4000) then break;
      g_Net_Slist_Pulse(300);
    end;

    if (aliveCount = 0) then
    begin
      DisconnectAll();
      CheckLocalServers();
      exit;
    end;

    slMOTD := '';
    {
    slUrgent := '';
    slReadUrgent := true;
    }

    SetLength(SL, 0);
    for f := 0 to High(mlist) do
    begin
      if (mlist[f].srvAnswered < 2) then continue;
      for n := 0 to High(mlist[f].srvAnswer) do
      begin
        pos := -1;
        for c := 0 to High(SL) do
        begin
          if (SL[c].IP = mlist[f].srvAnswer[n].IP) and (SL[c].Port = mlist[f].srvAnswer[n].Port) then
          begin
            pos := c;
            break;
          end;
        end;
        if (pos < 0) then
        begin
          pos := length(SL);
          SetLength(SL, pos+1);
          SL[pos] := mlist[f].srvAnswer[n];
          SL[pos].Number := pos;
        end;
      end;
      if (not mlist[f].slReadUrgent) and (mlist[f].slUrgent <> '') then
      begin
        if (mlist[f].slUrgent <> slUrgent) then
        begin
          slUrgent := mlist[f].slUrgent;
          slReadUrgent := false;
        end;
      end;
      if (slMOTD <> '') and (mlist[f].slMOTD <> '') then
      begin
        slMOTD := mlist[f].slMOTD;
      end;
    end;

    DisconnectAll();

    if (length(SL) = 0) then
    begin
      CheckLocalServers();
      exit;
    end;

    Sock := enet_socket_create(ENET_SOCKET_TYPE_DATAGRAM);
    if Sock = ENET_SOCKET_NULL then Exit;
    enet_socket_set_option(Sock, ENET_SOCKOPT_NONBLOCK, 1);

    for I := Low(SL) to High(SL) do PingServer(SL[I], Sock);

    enet_socket_set_option(Sock, ENET_SOCKOPT_BROADCAST, 1);
    PingBcast(Sock);

    T := GetTimerMS();

    InMsg.Alloc(NET_BUFSIZE);
    Buf.data := InMsg.Data;
    Buf.dataLength := InMsg.MaxSize;
    Cnt := 0;
    while GetTimerMS() - T <= 500 do
    begin
      InMsg.Clear();

      RX := enet_socket_receive(Sock, @SvAddr, @Buf, 1);
      if RX <= 0 then continue;
      InMsg.CurSize := RX;

      InMsg.BeginReading();

      if InMsg.ReadChar() <> 'D' then continue;
      if InMsg.ReadChar() <> 'F' then continue;

      FromSL := False;
      for I := Low(SL) to High(SL) do
        if (SL[I].PingAddr.host = SvAddr.host) and
           (SL[I].PingAddr.port = SvAddr.port) then
        begin
          with SL[I] do
          begin
            Port := InMsg.ReadWord();
            Ping := InMsg.ReadInt64();
            Ping := GetTimerMS() - Ping;
            Name := InMsg.ReadString();
            Map := InMsg.ReadString();
            GameMode := InMsg.ReadByte();
            Players := InMsg.ReadByte();
            MaxPlayers := InMsg.ReadByte();
            Protocol := InMsg.ReadByte();
            Password := InMsg.ReadByte() = 1;
            LocalPl := InMsg.ReadByte();
            Bots := InMsg.ReadWord();
          end;
          FromSL := True;
          Inc(Cnt);
          break;
        end;
      if not FromSL then
        ProcessLocal();
    end;

    InMsg.Free();
    enet_socket_destroy(Sock);
  finally
    NetOut.Clear();
  end;
end;


//==========================================================================
//
//  GetServerFromTable
//
//==========================================================================
function GetServerFromTable (Index: Integer; SL: TNetServerList; ST: TNetServerTable): TNetServer;
begin
  Result.Number := 0;
  Result.Protocol := 0;
  Result.Name := '';
  Result.IP := '';
  Result.Port := 0;
  Result.Map := '';
  Result.Players := 0;
  Result.MaxPlayers := 0;
  Result.LocalPl := 0;
  Result.Bots := 0;
  Result.Ping := 0;
  Result.GameMode := 0;
  Result.Password := false;
  FillChar(Result.PingAddr, SizeOf(ENetAddress), 0);
  if ST = nil then
    Exit;
  if (Index < 0) or (Index >= Length(ST)) then
    Exit;
  Result := SL[ST[Index].Indices[ST[Index].Current]];
end;


//==========================================================================
//
//  g_Serverlist_Draw
//
//==========================================================================
procedure g_Serverlist_Draw (var SL: TNetServerList; var ST: TNetServerTable);
var
  Srv: TNetServer;
  sy, i, y, mw, mx, l, motdh: Integer;
  cw: Byte = 0;
  ch: Byte = 0;
  ww: Word = 0;
  hh: Word = 0;
  ip: AnsiString;
begin
  ip := '';
  sy := 0;

  e_CharFont_GetSize(gMenuFont, _lc[I_NET_SLIST], ww, hh);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2) - (ww div 2), 16, _lc[I_NET_SLIST]);

  e_TextureFontGetSize(gStdFont, cw, ch);

  ip := _lc[I_NET_SLIST_HELP];
  mw := (Length(ip) * cw) div 2;

  motdh := gScreenHeight - 49 - ch * b_Text_LineCount(slMOTD);

  e_DrawFillQuad(16, 64, gScreenWidth-16, motdh, 64, 64, 64, 110);
  e_DrawQuad(16, 64, gScreenWidth-16, motdh, 255, 127, 0);

  e_TextureFontPrintEx(gScreenWidth div 2 - mw, gScreenHeight-24, ip, gStdFont, 225, 225, 225, 1);

  // MOTD
  if slMOTD <> '' then
  begin
    e_DrawFillQuad(16, motdh, gScreenWidth-16, gScreenHeight-44, 64, 64, 64, 110);
    e_DrawQuad(16, motdh, gScreenWidth-16, gScreenHeight-44, 255, 127, 0);
    e_TextureFontPrintFmt(20, motdh + 3, slMOTD, gStdFont, False, True);
  end;

  // Urgent message
  if not slReadUrgent and (slUrgent <> '') then
  begin
    e_DrawFillQuad(17, 65, gScreenWidth-17, motdh-1, 64, 64, 64, 128);
    e_DrawFillQuad(gScreenWidth div 2 - 256, gScreenHeight div 2 - 60,
      gScreenWidth div 2 + 256, gScreenHeight div 2 + 60, 64, 64, 64, 128);
    e_DrawQuad(gScreenWidth div 2 - 256, gScreenHeight div 2 - 60,
      gScreenWidth div 2 + 256, gScreenHeight div 2 + 60, 255, 127, 0);
    e_DrawLine(1, gScreenWidth div 2 - 256, gScreenHeight div 2 - 40,
      gScreenWidth div 2 + 256, gScreenHeight div 2 - 40, 255, 127, 0);
    l := Length(_lc[I_NET_SLIST_URGENT]) div 2;
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 - 58,
      _lc[I_NET_SLIST_URGENT], gStdFont);
    l := Length(slUrgent) div 2;
    e_TextureFontPrintFmt(gScreenWidth div 2 - 253, gScreenHeight div 2 - 38,
      slUrgent, gStdFont, False, True);
    l := Length(_lc[I_NET_SLIST_URGENT_CONT]) div 2;
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 + 41,
      _lc[I_NET_SLIST_URGENT_CONT], gStdFont);
    e_DrawLine(1, gScreenWidth div 2 - 256, gScreenHeight div 2 + 40,
      gScreenWidth div 2 + 256, gScreenHeight div 2 + 40, 255, 127, 0);
    Exit;
  end;

  if SL = nil then
  begin
    l := Length(slWaitStr) div 2;
    e_DrawFillQuad(17, 65, gScreenWidth-17, motdh-1, 64, 64, 64, 128);
    e_DrawQuad(gScreenWidth div 2 - 192, gScreenHeight div 2 - 10,
      gScreenWidth div 2 + 192, gScreenHeight div 2 + 11, 255, 127, 0);
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 - ch div 2,
      slWaitStr, gStdFont);
    Exit;
  end;

  y := 90;
  if (slSelection < Length(ST)) then
  begin
    I := slSelection;
    sy := y + 42 * I - 4;
    Srv := GetServerFromTable(I, SL, ST);
    ip := _lc[I_NET_ADDRESS] + ' ' + Srv.IP + ':' + IntToStr(Srv.Port);
    if Srv.Password then
      ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ' + _lc[I_MENU_YES]
    else
      ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ' + _lc[I_MENU_NO];
  end else
    if Length(ST) > 0 then
      slSelection := 0;

  mw := (gScreenWidth - 188);
  mx := 16 + mw;

  e_DrawFillQuad(16 + 1, sy, gScreenWidth - 16 - 1, sy + 40, 64, 64, 64, 0);
  e_DrawLine(1, 16 + 1, sy, gScreenWidth - 16 - 1, sy, 205, 205, 205);
  e_DrawLine(1, 16 + 1, sy + 41, gScreenWidth - 16 - 1, sy + 41, 255, 255, 255);

  e_DrawLine(1, 16, 85, gScreenWidth - 16, 85, 255, 127, 0);
  e_DrawLine(1, 16, motdh-20, gScreenWidth-16, motdh-20, 255, 127, 0);

  e_DrawLine(1, mx - 70, 64, mx - 70, motdh, 255, 127, 0);
  e_DrawLine(1, mx, 64, mx, motdh-20, 255, 127, 0);
  e_DrawLine(1, mx + 52, 64, mx + 52, motdh-20, 255, 127, 0);
  e_DrawLine(1, mx + 104, 64, mx + 104, motdh-20, 255, 127, 0);

  e_TextureFontPrintEx(18, 68, 'NAME/MAP', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx - 68, 68, 'PING', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx + 2, 68, 'MODE', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx + 54, 68, 'PLRS', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx + 106, 68, 'VER', gStdFont, 255, 127, 0, 1);

  y := 90;
  for I := 0 to High(ST) do
  begin
    Srv := GetServerFromTable(I, SL, ST);
    // Name and map
    e_TextureFontPrintEx(18, y, Srv.Name, gStdFont, 255, 255, 255, 1);
    e_TextureFontPrintEx(18, y + 16, Srv.Map, gStdFont, 210, 210, 210, 1);

    // Ping and similar count
    if (Srv.Ping < 0) or (Srv.Ping > 999) then
      e_TextureFontPrintEx(mx - 68, y, _lc[I_NET_SLIST_NO_ACCESS], gStdFont, 255, 0, 0, 1)
    else
      if Srv.Ping = 0 then
        e_TextureFontPrintEx(mx - 68, y, '<1' + _lc[I_NET_SLIST_PING_MS], gStdFont, 255, 255, 255, 1)
      else
        e_TextureFontPrintEx(mx - 68, y, IntToStr(Srv.Ping) + _lc[I_NET_SLIST_PING_MS], gStdFont, 255, 255, 255, 1);

    if Length(ST[I].Indices) > 1 then
      e_TextureFontPrintEx(mx - 68, y + 16, '< ' + IntToStr(Length(ST[I].Indices)) + ' >', gStdFont, 210, 210, 210, 1);

    // Game mode
    e_TextureFontPrintEx(mx + 2, y, g_Game_ModeToText(Srv.GameMode), gStdFont, 255, 255, 255, 1);

    // Players
    e_TextureFontPrintEx(mx + 54, y, IntToStr(Srv.Players) + '/' + IntToStr(Srv.MaxPlayers), gStdFont, 255, 255, 255, 1);
    e_TextureFontPrintEx(mx + 54, y + 16, IntToStr(Srv.LocalPl) + '+' + IntToStr(Srv.Bots), gStdFont, 210, 210, 210, 1);

    // Version
    e_TextureFontPrintEx(mx + 106, y, IntToStr(Srv.Protocol), gStdFont, 255, 255, 255, 1);

    y := y + 42;
  end;

  e_TextureFontPrintEx(20, motdh-20+3, ip, gStdFont, 205, 205, 205, 1);
  ip := IntToStr(Length(ST)) + _lc[I_NET_SLIST_SERVERS];
  e_TextureFontPrintEx(gScreenWidth - 48 - (Length(ip) + 1)*cw,
    motdh-20+3, ip, gStdFont, 205, 205, 205, 1);
end;


//==========================================================================
//
//  g_Serverlist_GenerateTable
//
//==========================================================================
procedure g_Serverlist_GenerateTable (SL: TNetServerList; var ST: TNetServerTable);
var
  i, j: Integer;

  function FindServerInTable(Name: AnsiString): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if ST = nil then
      Exit;
    for i := Low(ST) to High(ST) do
    begin
      if Length(ST[i].Indices) = 0 then
        continue;
      if SL[ST[i].Indices[0]].Name = Name then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
  function ComparePing(i1, i2: Integer): Boolean;
  var
    p1, p2: Int64;
  begin
    p1 := SL[i1].Ping;
    p2 := SL[i2].Ping;
    if (p1 < 0) then p1 := 999;
    if (p2 < 0) then p2 := 999;
    Result := p1 > p2;
  end;
  procedure SortIndices(var ind: Array of Integer);
  var
    I, J: Integer;
    T: Integer;
  begin
    for I := High(ind) downto Low(ind) do
      for J := Low(ind) to High(ind) - 1 do
        if ComparePing(ind[j], ind[j+1]) then
        begin
          T := ind[j];
          ind[j] := ind[j+1];
          ind[j+1] := T;
        end;
  end;
  procedure SortRows();
  var
    I, J: Integer;
    T: TNetServerRow;
  begin
    for I := High(ST) downto Low(ST) do
      for J := Low(ST) to High(ST) - 1 do
        if ComparePing(ST[j].Indices[0], ST[j+1].Indices[0]) then
        begin
          T := ST[j];
          ST[j] := ST[j+1];
          ST[j+1] := T;
        end;
  end;
begin
  ST := nil;
  if SL = nil then
    Exit;
  for i := Low(SL) to High(SL) do
  begin
    j := FindServerInTable(SL[i].Name);
    if j = -1 then
    begin
      j := Length(ST);
      SetLength(ST, j + 1);
      ST[j].Current := 0;
      SetLength(ST[j].Indices, 1);
      ST[j].Indices[0] := i;
    end
    else
    begin
      SetLength(ST[j].Indices, Length(ST[j].Indices) + 1);
      ST[j].Indices[High(ST[j].Indices)] := i;
    end;
  end;

  for i := Low(ST) to High(ST) do
    SortIndices(ST[i].Indices);

  SortRows();
end;


//==========================================================================
//
//  g_Serverlist_Control
//
//==========================================================================
procedure g_Serverlist_Control (var SL: TNetServerList; var ST: TNetServerTable);
var
  qm: Boolean;
  Srv: TNetServer;
begin
  g_Net_Slist_Pulse();

  if gConsoleShow or gChatShow then
    Exit;

  qm := sys_HandleInput(); // this updates kbd

  if qm or e_KeyPressed(IK_ESCAPE) or e_KeyPressed(VK_ESCAPE) or
     e_KeyPressed(JOY0_JUMP) or e_KeyPressed(JOY1_JUMP) or
     e_KeyPressed(JOY2_JUMP) or e_KeyPressed(JOY3_JUMP) then
  begin
    SL := nil;
    ST := nil;
    gState := STATE_MENU;
    g_GUI_ShowWindow('MainMenu');
    g_GUI_ShowWindow('NetGameMenu');
    g_GUI_ShowWindow('NetClientMenu');
    g_Sound_PlayEx(WINDOW_CLOSESOUND);
    Exit;
  end;

  // if there's a message on the screen,
  if not slReadUrgent and (slUrgent <> '') then
  begin
    if e_KeyPressed(IK_RETURN) or e_KeyPressed(IK_KPRETURN) or e_KeyPressed(VK_FIRE) or e_KeyPressed(VK_OPEN) or
       e_KeyPressed(JOY0_ATTACK) or e_KeyPressed(JOY1_ATTACK) or e_KeyPressed(JOY2_ATTACK) or e_KeyPressed(JOY3_ATTACK) then
      slReadUrgent := True;
    Exit;
  end;

  if e_KeyPressed(IK_SPACE) or e_KeyPressed(VK_JUMP) or
     e_KeyPressed(JOY0_ACTIVATE) or e_KeyPressed(JOY1_ACTIVATE) or e_KeyPressed(JOY2_ACTIVATE) or e_KeyPressed(JOY3_ACTIVATE) then
  begin
    if not slFetched then
    begin
      slWaitStr := _lc[I_NET_SLIST_WAIT];

      g_Game_Draw;
      sys_Repaint;

      if g_Net_Slist_Fetch(SL) then
      begin
        if SL = nil then
          slWaitStr := _lc[I_NET_SLIST_NOSERVERS];
      end
      else
        if SL = nil then
          slWaitStr := _lc[I_NET_SLIST_ERROR];
      slFetched := True;
      slSelection := 0;
      g_Serverlist_GenerateTable(SL, ST);
    end;
  end
  else
    slFetched := False;

  if SL = nil then Exit;

  if e_KeyPressed(IK_RETURN) or e_KeyPressed(IK_KPRETURN) or e_KeyPressed(VK_FIRE) or e_KeyPressed(VK_OPEN) or
     e_KeyPressed(JOY0_ATTACK) or e_KeyPressed(JOY1_ATTACK) or e_KeyPressed(JOY2_ATTACK) or e_KeyPressed(JOY3_ATTACK) then
  begin
    if not slReturnPressed then
    begin
      Srv := GetServerFromTable(slSelection, SL, ST);
      if Srv.Password then
      begin
        PromptIP := Srv.IP;
        PromptPort := Srv.Port;
        gState := STATE_MENU;
        g_GUI_ShowWindow('ClientPasswordMenu');
        SL := nil;
        ST := nil;
        slReturnPressed := True;
        Exit;
      end
      else
        g_Game_StartClient(Srv.IP, Srv.Port, '');
      SL := nil;
      ST := nil;
      slReturnPressed := True;
      Exit;
    end;
  end
  else
    slReturnPressed := False;

  if e_KeyPressed(IK_DOWN) or e_KeyPressed(IK_KPDOWN) or e_KeyPressed(VK_DOWN) or
     e_KeyPressed(JOY0_DOWN) or e_KeyPressed(JOY1_DOWN) or e_KeyPressed(JOY2_DOWN) or e_KeyPressed(JOY3_DOWN) then
  begin
    if not slDirPressed then
    begin
      Inc(slSelection);
      if slSelection > High(ST) then slSelection := 0;
      slDirPressed := True;
    end;
  end;

  if e_KeyPressed(IK_UP) or e_KeyPressed(IK_KPUP) or e_KeyPressed(VK_UP) or
     e_KeyPressed(JOY0_UP) or e_KeyPressed(JOY1_UP) or e_KeyPressed(JOY2_UP) or e_KeyPressed(JOY3_UP) then
  begin
    if not slDirPressed then
    begin
      if slSelection = 0 then slSelection := Length(ST);
      Dec(slSelection);

      slDirPressed := True;
    end;
  end;

  if e_KeyPressed(IK_RIGHT) or e_KeyPressed(IK_KPRIGHT) or e_KeyPressed(VK_RIGHT) or
     e_KeyPressed(JOY0_RIGHT) or e_KeyPressed(JOY1_RIGHT) or e_KeyPressed(JOY2_RIGHT) or e_KeyPressed(JOY3_RIGHT) then
  begin
    if not slDirPressed then
    begin
      Inc(ST[slSelection].Current);
      if ST[slSelection].Current > High(ST[slSelection].Indices) then ST[slSelection].Current := 0;
      slDirPressed := True;
    end;
  end;

  if e_KeyPressed(IK_LEFT) or e_KeyPressed(IK_KPLEFT) or e_KeyPressed(VK_LEFT) or
     e_KeyPressed(JOY0_LEFT) or e_KeyPressed(JOY1_LEFT) or e_KeyPressed(JOY2_LEFT) or e_KeyPressed(JOY3_LEFT) then
  begin
    if not slDirPressed then
    begin
      if ST[slSelection].Current = 0 then ST[slSelection].Current := Length(ST[slSelection].Indices);
      Dec(ST[slSelection].Current);

      slDirPressed := True;
    end;
  end;

  if (not e_KeyPressed(IK_DOWN)) and
     (not e_KeyPressed(IK_UP)) and
     (not e_KeyPressed(IK_RIGHT)) and
     (not e_KeyPressed(IK_LEFT)) and
     (not e_KeyPressed(IK_KPDOWN)) and
     (not e_KeyPressed(IK_KPUP)) and
     (not e_KeyPressed(IK_KPRIGHT)) and
     (not e_KeyPressed(IK_KPLEFT)) and
     (not e_KeyPressed(VK_DOWN)) and
     (not e_KeyPressed(VK_UP)) and
     (not e_KeyPressed(VK_RIGHT)) and
     (not e_KeyPressed(VK_LEFT)) and
     (not e_KeyPressed(JOY0_UP)) and (not e_KeyPressed(JOY1_UP)) and (not e_KeyPressed(JOY2_UP)) and (not e_KeyPressed(JOY3_UP)) and
     (not e_KeyPressed(JOY0_DOWN)) and (not e_KeyPressed(JOY1_DOWN)) and (not e_KeyPressed(JOY2_DOWN)) and (not e_KeyPressed(JOY3_DOWN)) and
     (not e_KeyPressed(JOY0_LEFT)) and (not e_KeyPressed(JOY1_LEFT)) and (not e_KeyPressed(JOY2_LEFT)) and (not e_KeyPressed(JOY3_LEFT)) and
     (not e_KeyPressed(JOY0_RIGHT)) and (not e_KeyPressed(JOY1_RIGHT)) and (not e_KeyPressed(JOY2_RIGHT)) and (not e_KeyPressed(JOY3_RIGHT))
 then
    slDirPressed := False;
end;


end.
