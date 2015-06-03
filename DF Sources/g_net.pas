unit g_net;

interface

uses
  e_log, e_fixedbuffer, ENet, ENet_Types, Classes;

const
  NET_PROTOCOL_VER = 144;

  NET_MAXCLIENTS = 24;
  NET_CHANS = 11;

  NET_CHAN_SERVICE = 0;
  NET_CHAN_IMPORTANT = 1;
  NET_CHAN_GAME = 2;
  NET_CHAN_PLAYER = 3;
  NET_CHAN_PLAYERPOS = 4;
  NET_CHAN_MONSTER = 5;
  NET_CHAN_MONSTERPOS = 6;
  NET_CHAN_LARGEDATA = 7;
  NET_CHAN_CHAT = 8;
  NET_CHAN_DOWNLOAD = 9;
  NET_CHAN_SHOTS = 10;

  NET_NONE = 0;
  NET_SERVER = 1;
  NET_CLIENT = 2;

  NET_BUFSIZE = 65536;

  NET_EVERYONE = -1;

  NET_DISC_NONE: enet_uint32 = 0;
  NET_DISC_PROTOCOL: enet_uint32 = 1;
  NET_DISC_VERSION: enet_uint32 = 2;
  NET_DISC_FULL: enet_uint32 = 3;
  NET_DISC_KICK: enet_uint32 = 4;
  NET_DISC_DOWN: enet_uint32 = 5;
  NET_DISC_PASSWORD: enet_uint32 = 6;
  NET_DISC_BAN: enet_uint32 = 7;

  NET_STATE_NONE = 0;
  NET_STATE_AUTH = 1;
  NET_STATE_GAME = 2;

type
  TNetClient = record
    ID:      Byte;
    Used:    Boolean;
    State:   Byte;
    Peer:    pENetPeer;
    Player:  Word;
    RequestedFullUpdate: Boolean;
    RCONAuth: Boolean;
    Voted:    Boolean;
  end;
  pTNetClient = ^TNetClient;

  AByte = array of Byte;

var
  NetInitDone:     Boolean = False;
  NetMode:         Byte = NET_NONE;
  
  NetDedicated:    Boolean = False;

  NetServerName:   string = 'Unnamed Server';
  NetPassword:     string = '';
  NetPort:         Word = 25666;

  NetAllowRCON:    Boolean = False;
  NetRCONPassword: string = 'ASS';

  NetTimeToUpdate:   Cardinal = 0;
  NetTimeToReliable: Cardinal = 0;
  NetTimeToMaster:   Cardinal = 0;

  NetHost:       pENetHost = nil;
  NetPeer:       pENetPeer = nil;
  NetEvent:      ENetEvent;
  NetAddr:       ENetAddress;

  NetUseMaster: Boolean = True;
  NetSlistAddr: ENetAddress;
  NetSlistIP:   string = 'mpms.doom2d.org';
  NetSlistPort: Word = 25665;
  
  NetLastIP:      string = '127.0.0.1';
  NetLastPort:    Word   = 25666;

  NetIn, NetOut: TBuffer;

  NetClients:     array of TNetClient;
  NetClientCount: Byte = 0;
  NetMaxClients:  Byte = 255;

  NetState:      Integer = NET_STATE_NONE;

  NetMyID:       Integer = -1;
  NetPlrUID:     Integer = -1;

  NetInterpLevel: Integer = 1;
  NetUpdateRate:  Cardinal = 0;  // as soon as possible
  NetRelupdRate:  Cardinal = 18; // around two times a second
  NetMasterRate:  Cardinal = 60000;

  NetForcePlayerUpdate: Boolean = False;
  NetPredictSelf:       Boolean = True;

  NetGotEverything: Boolean = False;

function  g_Net_Init(): Boolean;
procedure g_Net_Cleanup();
procedure g_Net_Free();

function  g_Net_Host(Port: enet_uint16; MaxClients: Cardinal = 16): Boolean;
procedure g_Net_Host_Die();
procedure g_Net_Host_Send(ID: Integer; Reliable: Boolean; Chan: Byte = NET_CHAN_GAME);
function  g_Net_Host_Update(): enet_size_t;

function  g_Net_Connect(IP: string; Port: enet_uint16): Boolean;
procedure g_Net_Disconnect(Forced: Boolean = False);
procedure g_Net_Client_Send(Reliable: Boolean; Chan: Byte = NET_CHAN_GAME);
function  g_Net_Client_Update(): enet_size_t;
function  g_Net_Client_UpdateWhileLoading(): enet_size_t;

function  g_Net_Client_ByName(Name: string): pTNetClient;
function  g_Net_Client_ByPlayer(PID: Word): pTNetClient;

procedure g_Net_SendData(Data:AByte; peer: pENetPeer; Reliable: Boolean; Chan: Byte = NET_CHAN_DOWNLOAD);
function  g_Net_Wait_Event(msgId: Word): TMemoryStream;

function  IpToStr(IP: LongWord): string;

implementation

uses
  SysUtils,
  e_input, g_nethandler, g_netmsg, g_netmaster, g_player, g_window, g_console,
  g_game, g_language;


{ /// SERVICE FUNCTIONS /// }


function g_Net_FindSlot(): Integer;
var
  I: Integer;
  F: Boolean;
  N: Integer;
begin
  N := -1;
  F := False;
  for I := Low(NetClients) to High(NetClients) do
  begin
    if not NetClients[I].Used then
    begin
      F := True;
      N := I;
      Break;
    end;
  end;

  if not F then
  begin
    if (Length(NetClients) >= NetMaxClients) then
      N := -1
    else
    begin
      SetLength(NetClients, Length(NetClients) + 1);
      N := High(NetClients);
    end;
  end;

  if N >= 0 then
  begin
    NetClients[N].Used := True;
    NetClients[N].ID := N;
    NetClients[N].RequestedFullUpdate := False;
    NetClients[N].RCONAuth := False;
    NetClients[N].Voted := False;
    NetClients[N].Player := 0;
  end;

  Result := N;
end;

function g_Net_Init(): Boolean;
begin
  e_Buffer_Clear(@NetIn);
  e_Buffer_Clear(@NetOut);
  SetLength(NetClients, 0);
  NetPeer := nil;
  NetHost := nil;
  NetMyID := -1;
  NetPlrUID := -1;
  NetAddr.port := 25666;
  
  Result := (enet_initialize() = 0);
end;

procedure g_Net_Cleanup();
begin
  e_Buffer_Clear(@NetIn);
  e_Buffer_Clear(@NetOut);

  SetLength(NetClients, 0);
  NetClientCount := 0;

  NetPeer := nil;
  NetHost := nil;
  NetMPeer := nil;
  NetMHost := nil;
  NetMyID := -1;
  NetPlrUID := -1;
  NetState := NET_STATE_NONE;

  NetTimeToMaster := 0;
  NetTimeToUpdate := 0;
  NetTimeToReliable := 0;

  NetMode := NET_NONE;
end;

procedure g_Net_Free();
begin
  g_Net_Cleanup();

  enet_deinitialize();
  NetInitDone := False;
end;


{ /// SERVER FUNCTIONS /// }


function g_Net_Host(Port: enet_uint16; MaxClients: Cardinal = 16): Boolean;
begin
  if NetMode <> NET_NONE then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_INGAME]);
    Result := False;
    Exit;
  end;

  Result := True;

  g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_MSG_HOST], [Port]));
  if not NetInitDone then
  begin
    if (not g_Net_Init()) then
    begin
      g_Console_Add(_lc[I_NET_MSG_FERROR] + _lc[I_NET_ERR_ENET]);
      Result := False;
      Exit;
    end
    else
      NetInitDone := True;
  end;

  NetAddr.host := ENET_HOST_ANY;
  NetAddr.port := Port;

  NetHost := enet_host_create(@NetAddr, NET_MAXCLIENTS, NET_CHANS, 0, 0);

  if (NetHost = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + Format(_lc[I_NET_ERR_HOST], [Port]));
    Result := False;
    g_Net_Cleanup;
    Exit;
  end;

  NetMode := NET_SERVER;
  e_Buffer_Clear(@NetOut);
end;

procedure g_Net_Host_Die();
var
  I: Integer;
begin
  if NetMode <> NET_SERVER then Exit;

  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_DISCALL]);
  for I := 0 to High(NetClients) do
    if NetClients[I].Used then
      enet_peer_disconnect(NetClients[I].Peer, NET_DISC_DOWN);

  while enet_host_service(NetHost, @NetEvent, 1000) > 0 do
    if NetEvent.kind = ENET_EVENT_TYPE_RECEIVE then
      enet_packet_destroy(NetEvent.packet);

  for I := 0 to High(NetClients) do
    if NetClients[I].Used then
    begin
      FreeMemory(NetClients[I].Peer^.data);
      NetClients[I].Peer^.data := nil;
      enet_peer_reset(NetClients[I].Peer);
      NetClients[I].Peer := nil;
      NetClients[I].Used := False;
    end;

  if (NetMPeer <> nil) and (NetMHost <> nil) then g_Net_Slist_Disconnect;

  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_DIE]);
  enet_host_destroy(NetHost);

  NetMode := NET_NONE;

  g_Net_Cleanup;
  e_WriteLog('NET: Server stopped', MSG_NOTIFY);
end;


procedure g_Net_Host_Send(ID: Integer; Reliable: Boolean; Chan: Byte = NET_CHAN_GAME);
var
  P: pENetPacket;
  F: enet_uint32;
begin
  if (Reliable) then
    F := LongWord(ENET_PACKET_FLAG_RELIABLE)
  else
    F := 0;

  if (ID >= 0) then
  begin
    if ID > High(NetClients) then Exit;
    if NetClients[ID].Peer = nil then Exit;

    P := enet_packet_create(Addr(NetOut.Data), NetOut.Len, F);
    if not Assigned(P) then Exit;

    enet_peer_send(NetClients[ID].Peer, Chan, P);
  end
  else
  begin
    P := enet_packet_create(Addr(NetOut.Data), NetOut.Len, F);
    if not Assigned(P) then Exit;

    enet_host_widecast(NetHost, Chan, P);
  end;
  
  enet_host_flush(NetHost);
  e_Buffer_Clear(@NetOut);
end;

function g_Net_Host_Update(): enet_size_t;
var
  IP: string;
  Port: Word;
  ID: Integer;
  TC: pTNetClient;
  TP: TPlayer;
begin
  IP := '';
  Result := 0;

  if NetUseMaster then g_Net_Slist_Check;

  while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
  begin
    case (NetEvent.kind) of
      ENET_EVENT_TYPE_CONNECT:
      begin
        IP := IpToStr(NetEvent.Peer^.address.host);
        Port := NetEvent.Peer^.address.port;
        g_Console_Add(_lc[I_NET_MSG] +
          Format(_lc[I_NET_MSG_HOST_CONN], [IP, Port]));

        if (NetEvent.data <> NET_PROTOCOL_VER) then
        begin
          g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
            _lc[I_NET_DISC_PROTOCOL]);
          NetEvent.peer^.data := GetMemory(SizeOf(Byte));
          Byte(NetEvent.peer^.data^) := 255;
          enet_peer_disconnect(NetEvent.peer, NET_DISC_PROTOCOL);
          enet_host_flush(NetHost);
          Exit;
        end;

        ID := g_Net_FindSlot();

        if ID < 0 then
        begin
          g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
            _lc[I_NET_DISC_FULL]);
          NetEvent.Peer^.data := GetMemory(SizeOf(Byte));
          Byte(NetEvent.peer^.data^) := 255;
          enet_peer_disconnect(NetEvent.peer, NET_DISC_FULL);
          enet_host_flush(NetHost);
          Exit;
        end;

        NetClients[ID].Peer := NetEvent.peer;
        NetClients[ID].Peer^.data := GetMemory(SizeOf(Byte));
        Byte(NetClients[ID].Peer^.data^) := ID;
        NetClients[ID].State := NET_STATE_AUTH;
        NetClients[ID].RCONAuth := False;

        enet_peer_timeout(NetEvent.peer, ENET_PEER_TIMEOUT_LIMIT * 2, ENET_PEER_TIMEOUT_MINIMUM * 2, ENET_PEER_TIMEOUT_MAXIMUM * 2);

        Inc(NetClientCount);
        g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_MSG_HOST_ADD], [ID]));
      end;

      ENET_EVENT_TYPE_RECEIVE:
      begin
        ID := Byte(NetEvent.peer^.data^);
        if ID > High(NetClients) then Exit;
        TC := @NetClients[ID];

        g_Net_HostMsgHandler(TC, NetEvent.packet);
      end;

      ENET_EVENT_TYPE_DISCONNECT:
      begin
        ID := Byte(NetEvent.peer^.data^);
        if ID > High(NetClients) then Exit;     
        TC := @NetClients[ID];
        if TC = nil then Exit;

        if not (TC^.Used) then Exit;

        TP := g_Player_Get(TC^.Player);

        if TP <> nil then
        begin
          TP.Lives := 0;
          TP.Kill(K_SIMPLEKILL, 0, 0);
          g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [TP.Name]), True);
          e_WriteLog('NET: Client ' + TP.Name + ' [' + IntToStr(ID) + '] disconnected.', MSG_NOTIFY);
          g_Player_Remove(TP.UID);
        end;

        TC^.Used := False;
        TC^.State := NET_STATE_NONE;
        TC^.Peer := nil;
        TC^.Player := 0;
        TC^.RequestedFullUpdate := False;

        FreeMemory(NetEvent.peer^.data);
        NetEvent.peer^.data := nil;
        g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_MSG_HOST_DISC], [ID]));
        Dec(NetClientCount);

        if NetUseMaster then g_Net_Slist_Update;
      end;
    end;
  end;
end;


{ /// CLIENT FUNCTIONS /// }


procedure g_Net_Disconnect(Forced: Boolean = False);
begin
  if NetMode <> NET_CLIENT then Exit;
  if (NetHost = nil) or (NetPeer = nil) then Exit;

  if not Forced then
  begin
    enet_peer_disconnect(NetPeer, NET_DISC_NONE);

    while (enet_host_service(NetHost, @NetEvent, 1500) > 0) do
    begin
      if (NetEvent.kind = ENET_EVENT_TYPE_DISCONNECT) then
      begin
        NetPeer := nil;
        break;
      end;

      if (NetEvent.kind = ENET_EVENT_TYPE_RECEIVE) then
        enet_packet_destroy(NetEvent.packet);
    end;

    if NetPeer <> nil then
    begin
      enet_peer_reset(NetPeer);
      NetPeer := nil;
    end;
  end
  else
  begin
    e_WriteLog('NET: Kicked from server: ' + IntToStr(NetEvent.data), MSG_NOTIFY);
    if (NetEvent.data <= 7) then
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_KICK] +
        _lc[TStrings_Locale(Cardinal(I_NET_DISC_NONE) + NetEvent.data)], True);
  end;

  if NetHost <> nil then
  begin
    enet_host_destroy(NetHost);
    NetHost := nil;
  end;
  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_CLIENT_DISC]);

  g_Net_Cleanup;
  e_WriteLog('NET: Disconnected', MSG_NOTIFY);
end;

procedure g_Net_Client_Send(Reliable: Boolean; Chan: Byte = NET_CHAN_GAME);
var
  P: pENetPacket;
  F: enet_uint32;
begin
  if (Reliable) then
    F := LongWord(ENET_PACKET_FLAG_RELIABLE)
  else
    F := 0;

  P := enet_packet_create(Addr(NetOut.Data), NetOut.Len, F);
  if not Assigned(P) then Exit;

  enet_peer_send(NetPeer, Chan, P);

  enet_host_flush(NetHost);
  e_Buffer_Clear(@NetOut);
end;

function g_Net_Client_Update(): enet_size_t;
begin
  Result := 0;
  while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
  begin
    case NetEvent.kind of
      ENET_EVENT_TYPE_RECEIVE:
        g_Net_ClientMsgHandler(NetEvent.packet);

      ENET_EVENT_TYPE_DISCONNECT:
      begin
        g_Net_Disconnect(True);
        Result := 1;
        Exit;
      end;
    end;
  end
end;

function g_Net_Client_UpdateWhileLoading(): enet_size_t;
begin
  Result := 0;
  while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
  begin
    case NetEvent.kind of
      ENET_EVENT_TYPE_RECEIVE:
        g_Net_ClientLightMsgHandler(NetEvent.packet);

      ENET_EVENT_TYPE_DISCONNECT:
      begin
        g_Net_Disconnect(True);
        Result := 1;
        Exit;
      end;
    end;
  end
end;

function g_Net_Connect(IP: string; Port: enet_uint16): Boolean;
var
  OuterLoop: Boolean;
begin
  if NetMode <> NET_NONE then
  begin
    g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_ERR_INGAME], True);
    Result := False;
    Exit;
  end;

  Result := True;

  g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_MSG_CLIENT_CONN],
    [IP, Port]));
  if not NetInitDone then
  begin
    if (not g_Net_Init()) then
    begin
      g_Console_Add(_lc[I_NET_MSG_FERROR] + _lc[I_NET_ERR_ENET], True);
      Result := False;
      Exit;
    end
    else
      NetInitDone := True;
  end;

  NetHost := enet_host_create(nil, 1, NET_CHANS, 0, 0);

  if (NetHost = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CLIENT], True);
    g_Net_Cleanup;
    Result := False;
    Exit;
  end;
  
  enet_address_set_host(@NetAddr, PChar(Addr(IP[1])));
  NetAddr.port := Port;

  NetPeer := enet_host_connect(NetHost, @NetAddr, NET_CHANS, NET_PROTOCOL_VER);

  if (NetPeer = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CLIENT], True);
    enet_host_destroy(NetHost);
    g_Net_Cleanup;
    Result := False;
    Exit;
  end;

  OuterLoop := True;
  while OuterLoop do
  begin
    while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
    begin
      if (NetEvent.kind = ENET_EVENT_TYPE_CONNECT) then
      begin
        g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_CLIENT_DONE]);
        NetMode := NET_CLIENT;
        e_Buffer_Clear(@NetOut);
        enet_peer_timeout(NetPeer, ENET_PEER_TIMEOUT_LIMIT * 2, ENET_PEER_TIMEOUT_MINIMUM * 2, ENET_PEER_TIMEOUT_MAXIMUM * 2);
        NetLastIP := IP;
        NetLastPort := Port;
        Exit;
      end;
    end;
    PreventWindowFromLockUp;
    e_PollKeyboard();
    if (e_KeyBuffer[1] = $080) or (e_KeyBuffer[57] = $080) then
      OuterLoop := False;
  end;

  g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_TIMEOUT], True);
  if NetPeer <> nil then enet_peer_reset(NetPeer);
  if NetHost <> nil then
  begin
    enet_host_destroy(NetHost);
    NetHost := nil;
  end;
  g_Net_Cleanup;
  Result := False;
end;

function IpToStr(IP: LongWord): string;
var
  Ptr: Pointer;
begin
  Result := '';
  Ptr := Addr(IP);

  e_Raw_Seek(0);
  Result := Result + IntToStr(e_Raw_Read_Byte(Ptr)) + '.';
  Result := Result + IntToStr(e_Raw_Read_Byte(Ptr)) + '.';
  Result := Result + IntToStr(e_Raw_Read_Byte(Ptr)) + '.';
  Result := Result + IntToStr(e_Raw_Read_Byte(Ptr));
  e_Raw_Seek(0);
end;

function g_Net_Client_ByName(Name: string): pTNetClient;
var
  a: Integer;
  pl: TPlayer;
begin
  Result := nil;
  for a := Low(NetClients) to High(NetClients) do
    if (NetClients[a].Used) and (NetClients[a].State = NET_STATE_GAME) then
    begin
      pl := g_Player_Get(NetClients[a].Player);
      if pl = nil then continue;
      if Copy(pl.Name, 1, Length(Name)) <> LowerCase(Name) then continue;
      if NetClients[a].Peer <> nil then
      begin
        Result := @NetClients[a];
        Exit;
      end;
    end;
end;

function g_Net_Client_ByPlayer(PID: Word): pTNetClient;
var
  a: Integer;
begin
  Result := nil;
  for a := Low(NetClients) to High(NetClients) do
    if (NetClients[a].Used) and (NetClients[a].State = NET_STATE_GAME) then
      if NetClients[a].Player = PID then
      begin
        Result := @NetClients[a];
        Exit;
      end;
end;

procedure g_Net_SendData(Data:AByte; peer: pENetPeer; Reliable: Boolean; Chan: Byte = NET_CHAN_DOWNLOAD);
var
  P: pENetPacket;
  F: enet_uint32;
  dataLength: Cardinal;
begin
  dataLength := Length(Data);

  if (Reliable) then
    F := LongWord(ENET_PACKET_FLAG_RELIABLE)
  else
    F := 0;

  if (peer <> nil) then
  begin
    P := enet_packet_create(@Data[0], dataLength, F);
    if not Assigned(P) then Exit;
    enet_peer_send(peer, Chan, P);
  end
  else
  begin
    P := enet_packet_create(@Data[0], dataLength, F);
    if not Assigned(P) then Exit;
    enet_host_widecast(NetHost, Chan, P);
  end;

  enet_host_flush(NetHost);
end;

function g_Net_Wait_Event(msgId: Word): TMemoryStream;
var
  downloadEvent: ENetEvent;
  OuterLoop: Boolean;
  MID: Byte;
  Ptr: Pointer;
  msgStream: TMemoryStream;
begin
  msgStream := nil;
  OuterLoop := True;
  while OuterLoop do
  begin
    while (enet_host_service(NetHost, @downloadEvent, 0) > 0) do
    begin
      if (downloadEvent.kind = ENET_EVENT_TYPE_RECEIVE) then
      begin
        Ptr := downloadEvent.packet^.data;

        MID := Byte(Ptr^);

        if (MID = msgId) then
        begin
          msgStream := TMemoryStream.Create;
          msgStream.SetSize(downloadEvent.packet^.dataLength);
          msgStream.WriteBuffer(Ptr^, downloadEvent.packet^.dataLength);
          msgStream.Seek(0, soFromBeginning);

          OuterLoop := False;
          enet_packet_destroy(downloadEvent.packet);
          break;
        end
        else begin
          enet_packet_destroy(downloadEvent.packet);
        end;
      end
      else
        if (downloadEvent.kind = ENET_EVENT_TYPE_DISCONNECT) then
        begin
          if (downloadEvent.data <= 7) then
            g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' ' +
            _lc[TStrings_Locale(Cardinal(I_NET_DISC_NONE) + downloadEvent.data)], True);
          OuterLoop := False;
          Break;
        end;
    end;

    PreventWindowFromLockUp;

    e_PollKeyboard();
    if (e_KeyBuffer[1] = $080) or (e_KeyBuffer[57] = $080) then
      break;
  end;
  Result := msgStream;
end;

end.
