unit g_netmaster;

interface

uses ENet;

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
    Name: string;
    IP: string;
    Port: Word;
    Map: string;
    Players, MaxPlayers: Byte;
    GameMode: Byte;
    Password: Boolean;
  end;
  pTNetServer = ^TNetServer;

  TNetServerList = array of TNetServer;
  pTNetServerList = ^TNetServerList;

var
  NetMHost:       pENetHost = nil;
  NetMPeer:       pENetPeer = nil;

  slCurrent:       TNetServerList = nil;
  slWaitStr:       string = '';
  slReturnPressed: Boolean = True;

procedure g_Net_Slist_Set(IP: string; Port: Word);
function  g_Net_Slist_Fetch(var SL: TNetServerList): Boolean;
procedure g_Net_Slist_Update;
procedure g_Net_Slist_Remove;
function  g_Net_Slist_Connect: Boolean;
procedure g_Net_Slist_Check;
procedure g_Net_Slist_Disconnect;

procedure g_Serverlist_Draw(var SL: TNetServerList);
procedure g_Serverlist_Control(var SL: TNetServerList);

implementation

uses
  SysUtils, e_fixedbuffer, e_input, e_graphics, e_log, g_window, g_net, g_console,
  g_map, g_game, g_sound, g_textures, g_gui, g_menu, g_options, g_language, WADEDITOR;

var
  NetMEvent:      ENetEvent;
  slSelection:    Byte = 0;
  slFetched:      Boolean = False;
  slDirPressed:   Boolean = False;

function g_Net_Slist_Fetch(var SL: TNetServerList): Boolean;
var
  Cnt: Byte;
  P: pENetPacket;
  MID: Byte;
  I: Integer;
begin
  Result := False;
  SL := nil;

  if (NetMHost <> nil) or (NetMPeer <> nil) then
    Exit;

  if not g_Net_Slist_Connect then
    Exit;

  e_WriteLog('Fetching serverlist...', MSG_NOTIFY);
  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_SLIST_FETCH]);

  e_Buffer_Clear(@NetOut);
  e_Buffer_Write(@NetOut, Byte(NET_MMSG_GET));

  P := enet_packet_create(Addr(NetOut.Data), NetOut.Len, Cardinal(ENET_PACKET_FLAG_RELIABLE));
  enet_peer_send(NetMPeer, NET_MCHAN_MAIN, P);
  enet_host_flush(NetMHost);

  while enet_host_service(NetMHost, @NetMEvent, 5000) > 0 do
  begin
    if NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE then
    begin
      e_Raw_Seek(0);
      MID := e_Raw_Read_Byte(NetMEvent.packet^.data);

      if MID <> NET_MMSG_GET then continue;

      Cnt := e_Raw_Read_Byte(NetMEvent.packet^.data);
      e_WriteLog('Retrieved ' + IntToStr(Cnt) + ' server(s).', MSG_NOTIFY);
      g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_SLIST_RETRIEVED], [Cnt]), True);

      if Cnt > 0 then
      begin
        SetLength(SL, Cnt);

        for I := 0 to Cnt - 1 do
        begin
          SL[I].Number := I;
          SL[I].IP := e_Raw_Read_String(NetMEvent.packet^.data);
          SL[I].Port := e_Raw_Read_Word(NetMEvent.packet^.data);
          SL[I].Name := e_Raw_Read_String(NetMEvent.packet^.data);
          SL[I].Map := e_Raw_Read_String(NetMEvent.packet^.data);
          SL[I].GameMode := e_Raw_Read_Byte(NetMEvent.packet^.data);
          SL[I].Players := e_Raw_Read_Byte(NetMEvent.packet^.data);
          SL[I].MaxPlayers := e_Raw_Read_Byte(NetMEvent.packet^.data);
          SL[I].Protocol := e_Raw_Read_Byte(NetMEvent.packet^.data);
          SL[I].Password := e_Raw_Read_Byte(NetMEvent.packet^.data) = 1;
        end;
      end;

      Result := True;
      break;
    end;
  end;

  g_Net_Slist_Disconnect;
  e_Buffer_Clear(@NetOut);
end;

procedure g_Net_Slist_Update;
var
  Wad, Map: string;
  P: pENetPacket;
begin
  if (NetMHost = nil) or (NetMPeer = nil) then Exit;
  g_ProcessResourceStr(gMapInfo.Map, @Wad, nil, @Map);
  Wad := ExtractFileName(Wad);

  e_Buffer_Clear(@NetOut);
  e_Buffer_Write(@NetOut, Byte(NET_MMSG_UPD));

  e_Buffer_Write(@NetOut, NetAddr.port);
  e_Buffer_Write(@NetOut, NetServerName);

  e_Buffer_Write(@NetOut, Wad + ':\' + Map);
  e_Buffer_Write(@NetOut, gGameSettings.GameMode);

  if gPlayer1 = nil then
    e_Buffer_Write(@NetOut, Byte(NetClientCount))
  else
    e_Buffer_Write(@NetOut, Byte(1 + NetClientCount));

  e_Buffer_Write(@NetOut, NetMaxClients);

  e_Buffer_Write(@NetOut, Byte(NET_PROTOCOL_VER));
  e_Buffer_Write(@NetOut, Byte(NetPassword <> ''));

  P := enet_packet_create(Addr(NetOut.Data), NetOut.Len, Cardinal(ENET_PACKET_FLAG_RELIABLE));
  enet_peer_send(NetMPeer, NET_MCHAN_UPD, P);

  enet_host_flush(NetMHost);
  e_Buffer_Clear(@NetOut);
end;

procedure g_Net_Slist_Remove;
var
  P: pENetPacket;
begin
  if (NetMHost = nil) or (NetMPeer = nil) then Exit;
  e_Buffer_Clear(@NetOut);
  e_Buffer_Write(@NetOut, Byte(NET_MMSG_DEL));
  e_Buffer_Write(@NetOut, NetAddr.port);

  P := enet_packet_create(Addr(NetOut.Data), NetOut.Len, Cardinal(ENET_PACKET_FLAG_RELIABLE));
  enet_peer_send(NetMPeer, NET_MCHAN_MAIN, P);

  enet_host_flush(NetMHost);
  e_Buffer_Clear(@NetOut);
end;

function g_Net_Slist_Connect: Boolean;
begin
  Result := False;

  NetMHost := enet_host_create(nil, 1, NET_MCHANS, 0, 0);
  if (NetMHost = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CLIENT], True);
    Exit;
  end;

  NetMPeer := enet_host_connect(NetMHost, @NetSlistAddr, NET_MCHANS, 0);
  if (NetMPeer = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CLIENT], True);
    enet_host_destroy(NetMHost);
    NetMHost := nil;
    Exit;
  end;

  if (enet_host_service(NetMHost, @NetMEvent, 3000) > 0) then
    if NetMEvent.kind = ENET_EVENT_TYPE_CONNECT then
    begin
      Result := True;
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_SLIST_CONN]);
      Exit;
    end
    else
      if NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE then
        enet_packet_destroy(NetMEvent.packet);

  g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_SLIST_ERROR], True);

  NetMHost := nil;
  NetMPeer := nil;
end;

procedure g_Net_Slist_Disconnect;
begin
  if (NetMHost = nil) and (NetMPeer = nil) then Exit;

  if NetMode = NET_SERVER then g_Net_Slist_Remove;

  enet_peer_disconnect(NetMPeer, 0);
  enet_host_flush(NetMHost);

  enet_peer_reset(NetMPeer);
  enet_host_destroy(NetMHost);
  
  NetMPeer := nil;
  NetMHost := nil;

  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_SLIST_DISC]);
end;

procedure g_Net_Slist_Check;
begin
  if (NetMHost = nil) or (NetMPeer = nil) then Exit;
  
  while (enet_host_service(NetMHost, @NetMEvent, 0) > 0) do
  begin
    if NetMEvent.kind = ENET_EVENT_TYPE_DISCONNECT then
    begin
      g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_SLIST_LOST], True);
      if NetMPeer <> nil then enet_peer_reset(NetMPeer);
      if NetMHost <> nil then enet_host_destroy(NetMHost);
      NetMPeer := nil;
      NetMHost := nil;
      Break;
    end
    else
      if NetMEvent.kind = ENET_EVENT_TYPE_RECEIVE then
        enet_packet_destroy(NetMEvent.packet);
  end;
end;

procedure g_Net_Slist_Set(IP: string; Port: Word);
begin
  if NetInitDone then
  begin
    enet_address_set_host(@NetSlistAddr, PChar(Addr(IP[1])));
    NetSlistAddr.Port := Port;
    e_WriteLog('Masterserver address set to ' + IP + ':' + IntToStr(Port), MSG_NOTIFY);
  end;
end;

procedure g_Serverlist_Draw(var SL: TNetServerList);
const
  GMSTR: array [0..5] of string = ('FUCK', 'DM', 'TDM', 'CTF', 'COOP', 'MOD');
var
  sy, i, y, mw, mx, l: Integer;
  cw, ch: Byte;
  ww, hh: Word;
  ip: string;
begin
  ip := '';
  sy := 0;

  e_CharFont_GetSize(gMenuFont, _lc[I_NET_SLIST], ww, hh);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2) - (ww div 2), 16, _lc[I_NET_SLIST]);

  e_TextureFontGetSize(gStdFont, cw, ch);

  ip := _lc[I_NET_SLIST_HELP];
  mw := (Length(ip) * cw) div 2;

  e_DrawFillQuad(16, 64, gScreenWidth-16, gScreenHeight-44, 64, 64, 64, 110);
  e_DrawQuad(16, 64, gScreenWidth-16, gScreenHeight-44, 255, 127, 0);

  e_TextureFontPrintEx(gScreenWidth div 2 - mw, gScreenHeight-24, ip, gStdFont, 225, 225, 225, 1);

  if SL = nil then
  begin
    l := Length(slWaitStr) div 2;
    e_DrawFillQuad(16, 64, gScreenWidth-16, gScreenHeight-44, 64, 64, 64, 128);
    e_DrawQuad(gScreenWidth div 2 - 192, gScreenHeight div 2 - 10,
      gScreenWidth div 2 + 192, gScreenHeight div 2 + 11, 255, 127, 0);
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 - ch div 2,
      slWaitStr, gStdFont);
    Exit;
  end;

  y := 90;
  if (slSelection < Length(SL)) then
  begin
    I := slSelection;
    sy := y + 42 * I - 4;
    ip := _lc[I_NET_ADDRESS] + ' ' + SL[I].IP + ':' + IntToStr(SL[I].Port);
    if SL[I].Password then
      ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ' + _lc[I_MENU_YES]
    else
      ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ' + _lc[I_MENU_NO];
  end else
    if Length(SL) > 0 then
      slSelection := 0;

  mw := (gScreenWidth - 188);
  mx := 16 + mw;

  e_DrawFillQuad(16 + 1, sy, gScreenWidth - 16 - 1, sy + 40, 64, 64, 64, 0);
  e_DrawLine(1, 16 + 1, sy, gScreenWidth - 16 - 1, sy, 205, 205, 205);
  e_DrawLine(1, 16 + 1, sy + 41, gScreenWidth - 16 - 1, sy + 41, 255, 255, 255);

  e_DrawLine(1, 16, 85, gScreenWidth - 16, 85, 255, 127, 0);
  e_DrawLine(1, 16, gScreenHeight-64, gScreenWidth-16, gScreenHeight-64, 255, 127, 0);

  e_DrawLine(1, mx, 64, mx, gScreenHeight-44, 255, 127, 0);
  e_DrawLine(1, mx + 52, 64, mx + 52, gScreenHeight-64, 255, 127, 0);
  e_DrawLine(1, mx + 104, 64, mx + 104, gScreenHeight-64, 255, 127, 0);

  e_TextureFontPrintEx(18, 68, 'NAME/MAP', gStdFont, 255, 127, 0, 1);

  y := 90;
  for I := 0 to High(SL) do
  begin
    e_TextureFontPrintEx(18, y, SL[I].Name, gStdFont, 255, 255, 255, 1);
    e_TextureFontPrintEx(18, y + 16, SL[I].Map, gStdFont, 210, 210, 210, 1);

    y := y + 42;
  end;

  e_TextureFontPrintEx(mx + 2, 68, 'MODE', gStdFont, 255, 127, 0, 1);
  y := 90;
  for I := 0 to High(SL) do
  begin
    e_TextureFontPrintEx(mx + 2, y, GMSTR[SL[I].GameMode], gStdFont, 255, 255, 255, 1);

    y := y + 42;
  end;

  e_TextureFontPrintEx(mx + 54, 68, 'PLRS', gStdFont, 255, 127, 0, 1);
  y := 90;
  for I := 0 to High(SL) do
  begin
    e_TextureFontPrintEx(mx + 54, y, IntToStr(SL[I].Players) + '/' + IntToStr(SL[I].MaxPlayers), gStdFont, 255, 255, 255, 1);

    y := y + 42;
  end;

  e_TextureFontPrintEx(mx + 106, 68, 'VER', gStdFont, 255, 127, 0, 1);
  y := 90;
  for I := 0 to High(SL) do
  begin
    e_TextureFontPrintEx(mx + 106, y, IntToStr(SL[I].Protocol), gStdFont, 255, 255, 255, 1);

    y := y + 42;
  end;

  e_TextureFontPrintEx(20, gScreenHeight-61, ip, gStdFont, 205, 205, 205, 1);
  ip := IntToStr(Length(SL)) + _lc[I_NET_SLIST_SERVERS];
  e_TextureFontPrintEx(gScreenWidth - 16 - (Length(ip) + 1)*cw,
    gScreenHeight-61, ip, gStdFont, 205, 205, 205, 1);
end;

procedure g_Serverlist_Control(var SL: TNetServerList);
begin
  if gConsoleShow or gChatShow then Exit;
  e_PollKeyboard;

  if (e_KeyBuffer[1] = $080) then
  begin
    SL := nil;
    gState := STATE_MENU;
    g_GUI_ShowWindow('MainMenu');
    g_GUI_ShowWindow('NetGameMenu');
    g_GUI_ShowWindow('NetClientMenu');
    g_Sound_PlayEx(WINDOW_CLOSESOUND);
    Exit;
  end;

  if (e_KeyBuffer[57] = $080) then
  begin
    if not slFetched then
    begin
      slWaitStr := _lc[I_NET_SLIST_WAIT];

      g_Game_Draw;
      ReDrawWindow;

      if g_Net_Slist_Fetch(SL) then
      begin
        if SL = nil then
          slWaitStr := _lc[I_NET_SLIST_NOSERVERS];
      end
      else
        slWaitStr := _lc[I_NET_SLIST_ERROR];
      slFetched := True;
      slSelection := 0;
    end;
  end
  else
    slFetched := False;

  if SL = nil then Exit;

  if (e_KeyBuffer[$1C] = $080) then
  begin
    if not slReturnPressed then
    begin
      if SL[slSelection].Password then
      begin
        PromptIP := SL[slSelection].IP;
        PromptPort := SL[slSelection].Port;
        gState := STATE_MENU;
        g_GUI_ShowWindow('ClientPasswordMenu');
        SL := nil;
        slReturnPressed := True;
        Exit;
      end
      else
        g_Game_StartClient(SL[slSelection].IP, SL[slSelection].Port, '');
      SL := nil;
      slReturnPressed := True;
      Exit;
    end;
  end
  else
    slReturnPressed := False;

  if (e_KeyBuffer[$D0] = $080) then
  begin
    if not slDirPressed then
    begin
      Inc(slSelection);
      if slSelection > High(SL) then slSelection := 0;
      slDirPressed := True;
    end;
  end;
  if (e_KeyBuffer[$C8] = $080) then
  begin
    if not slDirPressed then
    begin
      if slSelection = 0 then slSelection := Length(SL);
      Dec(slSelection);

      slDirPressed := True;
    end;
  end;

  if (e_KeyBuffer[$D0] <> $080) and (e_KeyBuffer[$C8] <> $080) then
    slDirPressed := False;
end;

end.
