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
unit g_net;

interface

uses
  e_log, e_msg, utils, ENet, Classes, md5, MAPDEF{$IFDEF USE_MINIUPNPC}, miniupnpc;{$ELSE};{$ENDIF}

const
  NET_PROTOCOL_VER = 188;

  NET_MAXCLIENTS = 24;
  NET_CHANS = 12;

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
  NET_CHAN_DOWNLOAD_EX = 11;

  NET_NONE = 0;
  NET_SERVER = 1;
  NET_CLIENT = 2;

  NET_BUFSIZE = $FFFF;
  NET_PING_PORT = $DF2D;

  NET_EVERYONE = -1;

  NET_UNRELIABLE = 0;
  NET_RELIABLE = 1;

  NET_DISC_NONE: enet_uint32 = 0;
  NET_DISC_PROTOCOL: enet_uint32 = 1;
  NET_DISC_VERSION: enet_uint32 = 2;
  NET_DISC_FULL: enet_uint32 = 3;
  NET_DISC_KICK: enet_uint32 = 4;
  NET_DISC_DOWN: enet_uint32 = 5;
  NET_DISC_PASSWORD: enet_uint32 = 6;
  NET_DISC_TEMPBAN: enet_uint32 = 7;
  NET_DISC_BAN: enet_uint32 = 8;
  NET_DISC_MAX: enet_uint32 = 8;
  NET_DISC_FILE_TIMEOUT: enet_uint32 = 13;

  NET_STATE_NONE = 0;
  NET_STATE_AUTH = 1;
  NET_STATE_GAME = 2;

  NET_CONNECT_TIMEOUT = 1000 * 10;

  BANLIST_FILENAME = 'banlist.txt';
  NETDUMP_FILENAME = 'netdump';

type
  TNetMapResourceInfo = record
    wadName: AnsiString; // wad file name, without a path
    size: Integer; // wad file size (-1: size and hash are not known)
    hash: TMD5Digest; // wad hash
  end;

  TNetMapResourceInfoArray = array of TNetMapResourceInfo;

  TNetFileTransfer = record
    diskName: string;
    hash: TMD5Digest;
    stream: TStream;
    size: Integer; // file size in bytes
    chunkSize: Integer;
    lastSentChunk: Integer;
    lastAckChunk: Integer;
    lastAckTime: Int64; // msecs; if not "in progress", we're waiting for the first ack
    inProgress: Boolean;
    diskBuffer: PChar; // of `chunkSize` bytes
    resumed: Boolean;
  end;

  TNetClient = record
    ID:       Byte;
    Used:     Boolean;
    State:    Byte;
    Peer:     pENetPeer;
    Player:   Word;
    RequestedFullUpdate: Boolean;
    WaitForFirstSpawn: Boolean; // set to `true` in server, used to spawn a player on first full state request
    RCONAuth: Boolean;
    Voted:    Boolean;
    Crimes:   Integer;
    AuthTime: LongWord;
    MsgTime:  LongWord;
    Transfer: TNetFileTransfer; // only one transfer may be active
    NetOut:   array [0..1] of TMsg;
  end;
  TBanRecord = record
    IP: LongWord;
    Perm: Boolean;
  end;
  pTNetClient = ^TNetClient;

  AByte = array of Byte;

var
  NetInitDone:     Boolean = False;
  NetMode:         Byte = NET_NONE;
  NetDump:         Boolean = False;

  NetServerName:   string = 'Unnamed Server';
  NetPassword:     string = '';
  NetPort:         Word = 25666;

  NetAllowRCON:    Boolean = False;
  NetRCONPassword: string = '';

  NetTimeToUpdate:   Cardinal = 0;
  NetTimeToReliable: Cardinal = 0;
  NetTimeToMaster:   Cardinal = 0;

  NetHost:       pENetHost = nil;
  NetPeer:       pENetPeer = nil;
  NetEvent:      ENetEvent;
  NetAddr:       ENetAddress;

  NetPongAddr:   ENetAddress;
  NetPongSock:   ENetSocket = ENET_SOCKET_NULL;

  NetUseMaster: Boolean = True;
  NetMasterList: string = 'mpms.doom2d.org:25665, deadsoftware.ru:25665';

  NetClientIP:   string = '127.0.0.1';
  NetClientPort: Word   = 25666;

  NetIn, NetOut: TMsg;
  NetBuf:        array [0..1] of TMsg;

  NetClients:     array of TNetClient;
  NetClientCount: Byte = 0;
  NetMaxClients:  Byte = 255;
  NetBannedHosts: array of TBanRecord;

  NetAutoBanLimit: Integer = 5;
  NetAutoBanPerm:  Boolean = True;
  NetAutoBanWarn:  Boolean = False;

  NetAuthTimeout:   Integer = 15 * 1000;
  NetPacketTimeout: Integer = 30 * 1000;

  NetState:      Integer = NET_STATE_NONE;

  NetMyID:       Integer = -1;
  NetPlrUID1:    Integer = -1;
  NetPlrUID2:    Integer = -1;

  NetInterpLevel: Integer = 1;
  NetUpdateRate:  Cardinal = 0;  // as soon as possible
  NetRelupdRate:  Cardinal = 18; // around two times a second
  NetMasterRate:  Cardinal = 60000;

  NetForcePlayerUpdate: Boolean = False;
  NetPredictSelf:       Boolean = True;
  NetForwardPorts:      Boolean = False;

  NetGotEverything: Boolean = False;
  NetGotKeys:       Boolean = False;

  NetDeafLevel: Integer = 0;

{$IFDEF USE_MINIUPNPC}
  NetPortForwarded: Word = 0;
  NetPongForwarded: Boolean = False;
  NetIGDControl: AnsiString;
  NetIGDService: TURLStr;
{$ENDIF}

  NetPortThread: TThreadID = NilThreadId;

  NetDumpFile: TStream;

  g_Res_received_map_start: Integer = 0; // set if we received "map change" event


function  g_Net_Init(): Boolean;
procedure g_Net_Cleanup();
procedure g_Net_Free();
procedure g_Net_Flush();

function  g_Net_Host(IPAddr: LongWord; Port: enet_uint16; MaxClients: Cardinal = 16): Boolean;
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
function  g_Net_ClientName_ByID(ID: Integer): string;

procedure g_Net_SendData(Data: AByte; peer: pENetPeer; Reliable: Boolean; Chan: Byte = NET_CHAN_DOWNLOAD);
//function  g_Net_Wait_Event(msgId: Word): TMemoryStream;
//function g_Net_Wait_FileInfo (var tf: TNetFileTransfer; asMap: Boolean; out resList: TStringList): Integer;

function  IpToStr(IP: LongWord): string;
function  StrToIp(IPstr: string; var IP: LongWord): Boolean;

function  g_Net_IsHostBanned(IP: LongWord; Perm: Boolean = False): Boolean;
procedure g_Net_BanHost(IP: LongWord; Perm: Boolean = True); overload;
procedure g_Net_BanHost(IP: string; Perm: Boolean = True); overload;
function  g_Net_UnbanHost(IP: string): Boolean; overload;
function  g_Net_UnbanHost(IP: LongWord): Boolean; overload;
procedure g_Net_UnbanNonPermHosts();
procedure g_Net_SaveBanList();

procedure g_Net_Penalize(C: pTNetClient; Reason: string);

procedure g_Net_DumpStart();
procedure g_Net_DumpSendBuffer();
procedure g_Net_DumpRecvBuffer(Buf: penet_uint8; Len: LongWord);
procedure g_Net_DumpEnd();

function g_Net_ForwardPorts(ForwardPongPort: Boolean = True): Boolean;
procedure g_Net_UnforwardPorts();

function g_Net_UserRequestExit: Boolean;

function g_Net_Wait_MapInfo (var tf: TNetFileTransfer; var resList: TNetMapResourceInfoArray): Integer;
function g_Net_RequestResFileInfo (resIndex: LongInt; out tf: TNetFileTransfer): Integer;
function g_Net_AbortResTransfer (var tf: TNetFileTransfer): Boolean;
function g_Net_ReceiveResourceFile (resIndex: LongInt; var tf: TNetFileTransfer; strm: TStream): Integer;

function g_Net_IsNetworkAvailable (): Boolean;
procedure g_Net_InitLowLevel ();
procedure g_Net_DeinitLowLevel ();

procedure NetServerCVars(P: SSArray);


implementation

// *enet_host_service()*
// fuck! https://www.mail-archive.com/enet-discuss@cubik.org/msg00852.html
// tl;dr: on shitdows, we can get -1 sometimes, and it is *NOT* a failure.
//        thank you, enet. let's ignore failures altogether then.

uses
  SysUtils,
  e_input, e_res,
  g_nethandler, g_netmsg, g_netmaster, g_player, g_window, g_console,
  g_main, g_game, g_language, g_weapons, ctypes, g_system, g_map;

const
  FILE_CHUNK_SIZE = 8192;

var
  enet_init_success: Boolean = false;
  g_Net_DownloadTimeout: Single;
  trans_omsg: TMsg;


function g_Net_IsNetworkAvailable (): Boolean;
begin
  result := enet_init_success;
end;

procedure g_Net_InitLowLevel ();
  var v: ENetVersion;
begin
  v := enet_linked_version();
  e_LogWritefln('ENet Version: %s.%s.%s', [ENET_VERSION_GET_MAJOR(v), ENET_VERSION_GET_MINOR(v), ENET_VERSION_GET_PATCH(v)]);
  if enet_init_success then raise Exception.Create('wuta?!');
  enet_init_success := (enet_initialize() = 0);
end;

procedure g_Net_DeinitLowLevel ();
begin
  if enet_init_success then
  begin
    enet_deinitialize();
    enet_init_success := false;
  end;
end;


//**************************************************************************
//
// SERVICE FUNCTIONS
//
//**************************************************************************

procedure clearNetClientTransfers (var nc: TNetClient);
begin
  nc.Transfer.stream.Free;
  nc.Transfer.diskName := ''; // just in case
  if (nc.Transfer.diskBuffer <> nil) then FreeMem(nc.Transfer.diskBuffer);
  nc.Transfer.stream := nil;
  nc.Transfer.diskBuffer := nil;
end;


procedure clearNetClient (var nc: TNetClient);
begin
  clearNetClientTransfers(nc);
end;


procedure clearNetClients (clearArray: Boolean);
var
  f: Integer;
begin
  for f := Low(NetClients) to High(NetClients) do clearNetClient(NetClients[f]);
  if (clearArray) then SetLength(NetClients, 0);
end;


function g_Net_UserRequestExit (): Boolean;
begin
  Result := {e_KeyPressed(IK_SPACE) or}
            e_KeyPressed(IK_ESCAPE) or
            e_KeyPressed(VK_ESCAPE) or
            e_KeyPressed(JOY0_JUMP) or
            e_KeyPressed(JOY1_JUMP) or
            e_KeyPressed(JOY2_JUMP) or
            e_KeyPressed(JOY3_JUMP)
end;

//**************************************************************************
//
// file transfer declaraions and host packet processor
//
//**************************************************************************

const
  // server packet type
  NTF_SERVER_DONE = 10; // done with this file
  NTF_SERVER_FILE_INFO = 11; // sent after client request
  NTF_SERVER_CHUNK = 12; // next chunk; chunk number follows
  NTF_SERVER_ABORT = 13; // server abort
  NTF_SERVER_MAP_INFO = 14;

  // client packet type
  NTF_CLIENT_MAP_REQUEST = 100; // map file request; also, returns list of additional wads to download
  NTF_CLIENT_FILE_REQUEST = 101; // resource file request (by index)
  NTF_CLIENT_ABORT = 102; // do not send requested file, or abort current transfer
  NTF_CLIENT_START = 103; // start transfer; client may resume download by sending non-zero starting chunk
  NTF_CLIENT_ACK = 104; // chunk ack; chunk number follows


// disconnect client due to some file transfer error
procedure killClientByFT (var nc: TNetClient);
begin
  e_LogWritefln('disconnected client #%d due to file transfer error', [nc.ID], TMsgType.Warning);
  enet_peer_disconnect(nc.Peer, NET_DISC_FILE_TIMEOUT);
  clearNetClientTransfers(nc);
  g_Net_Slist_ServerPlayerLeaves();
end;


// send file transfer message from server to client
function ftransSendServerMsg (var nc: TNetClient; var m: TMsg): Boolean;
var
  pkt: PENetPacket;
begin
  result := false;
  if (m.CurSize < 1) then exit;
  pkt := enet_packet_create(m.Data, m.CurSize, ENET_PACKET_FLAG_RELIABLE);
  if not Assigned(pkt) then begin killClientByFT(nc); exit; end;
  if (enet_peer_send(nc.Peer, NET_CHAN_DOWNLOAD_EX, pkt) <> 0) then begin killClientByFT(nc); exit; end;
  result := true;
end;


// send file transfer message from client to server
function ftransSendClientMsg (var m: TMsg): Boolean;
var
  pkt: PENetPacket;
begin
  result := false;
  if (m.CurSize < 1) then exit;
  pkt := enet_packet_create(m.Data, m.CurSize, ENET_PACKET_FLAG_RELIABLE);
  if not Assigned(pkt) then exit;
  if (enet_peer_send(NetPeer, NET_CHAN_DOWNLOAD_EX, pkt) <> 0) then exit;
  result := true;
end;


// file chunk sender
procedure ProcessChunkSend (var nc: TNetClient);
var
  tf: ^TNetFileTransfer;
  ct: Int64;
  chunks: Integer;
  rd: Integer;
begin
  tf := @nc.Transfer;
  if (tf.stream = nil) then exit;
  ct := GetTimerMS();
  // arbitrary timeout number
  if (ct-tf.lastAckTime >= 5000) then
  begin
    killClientByFT(nc);
    exit;
  end;
  // check if we need to send something
  if (not tf.inProgress) then exit; // waiting for the initial ack
  // ok, we're sending chunks
  if (tf.lastAckChunk <> tf.lastSentChunk) then exit;
  Inc(tf.lastSentChunk);
  // do it one chunk at a time; client ack will advance our chunk counter
  chunks := (tf.size+tf.chunkSize-1) div tf.chunkSize;

  if (tf.lastSentChunk > chunks) then
  begin
    killClientByFT(nc);
    exit;
  end;

  trans_omsg.Clear();
  if (tf.lastSentChunk = chunks) then
  begin
    // we're done with this file
    e_LogWritefln('download: client #%d, DONE sending chunks #%d/#%d', [nc.ID, tf.lastSentChunk, chunks]);
    trans_omsg.Write(Byte(NTF_SERVER_DONE));
    clearNetClientTransfers(nc);
  end
  else
  begin
    // packet type
    trans_omsg.Write(Byte(NTF_SERVER_CHUNK));
    trans_omsg.Write(LongInt(tf.lastSentChunk));
    // read chunk
    rd := tf.size-(tf.lastSentChunk*tf.chunkSize);
    if (rd > tf.chunkSize) then rd := tf.chunkSize;
    trans_omsg.Write(LongInt(rd));
    //e_LogWritefln('download: client #%d, sending chunk #%d/#%d (%d bytes)', [nc.ID, tf.lastSentChunk, chunks, rd]);
    //FIXME: check for errors here
    try
      tf.stream.Seek(tf.lastSentChunk*tf.chunkSize, soFromBeginning);
      tf.stream.ReadBuffer(tf.diskBuffer^, rd);
      trans_omsg.WriteData(tf.diskBuffer, rd);
    except // sorry
      killClientByFT(nc);
      exit;
    end;
  end;
  // send packet
  ftransSendServerMsg(nc, trans_omsg);
end;


// server file transfer packet processor
// received packet is in `NetEvent`
procedure ProcessDownloadExPacket ();
var
  f: Integer;
  nc: ^TNetClient;
  nid: Integer = -1;
  msg: TMsg;
  cmd: Byte;
  tf: ^TNetFileTransfer;
  fname: string;
  chunk: Integer;
  ridx: Integer;
  dfn: AnsiString;
  md5: TMD5Digest;
  //st: TStream;
  size: LongInt;
  fi: TDiskFileInfo;
begin
  // find client index by peer
  for f := Low(NetClients) to High(NetClients) do
  begin
    if (not NetClients[f].Used) then continue;
    if (NetClients[f].Peer = NetEvent.peer) then
    begin
      nid := f;
      break;
    end;
  end;
  //e_LogWritefln('RECEIVE: dlpacket; client=%d (datalen=%u)', [nid, NetEvent.packet^.dataLength]);

  if (nid < 0) then exit; // wtf?!
  nc := @NetClients[nid];

  if (NetEvent.packet^.dataLength = 0) then
  begin
    killClientByFT(nc^);
    exit;
  end;

  // don't time out clients during a file transfer
  if (NetAuthTimeout > 0) then
    nc^.AuthTime := gTime + NetAuthTimeout;
  if (NetPacketTimeout > 0) then
    nc^.MsgTime := gTime + NetPacketTimeout;

  tf := @NetClients[nid].Transfer;
  tf.lastAckTime := GetTimerMS();

  cmd := Byte(NetEvent.packet^.data^);
  //e_LogWritefln('RECEIVE:   nid=%d; cmd=%u', [nid, cmd]);
  case cmd of
    NTF_CLIENT_FILE_REQUEST: // file request
      begin
        if (tf.stream <> nil) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        if (NetEvent.packet^.dataLength < 2) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        // new transfer request; build packet
        if not msg.Init(NetEvent.packet^.data+1, NetEvent.packet^.dataLength-1, True) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        // get resource index
        ridx := msg.ReadLongInt();
        if (ridx < -1) or (ridx >= length(gExternalResources)) then
        begin
          e_LogWritefln('Invalid resource index %d', [ridx], TMsgType.Warning);
          killClientByFT(nc^);
          exit;
        end;
        if (ridx < 0) then fname := gGameSettings.WAD else fname := gExternalResources[ridx].diskName;
        if (length(fname) = 0) then
        begin
          e_WriteLog('Invalid filename: '+fname, TMsgType.Warning);
          killClientByFT(nc^);
          exit;
        end;
        tf.diskName := findDiskWad(fname);
        if (length(tf.diskName) = 0) then
        begin
          e_LogWritefln('NETWORK: file "%s" not found!', [fname], TMsgType.Fatal);
          killClientByFT(nc^);
          exit;
        end;
        // calculate hash
        //tf.hash := MD5File(tf.diskName);
        if (ridx < 0) then tf.hash := gWADHash else tf.hash := gExternalResources[ridx].hash;
        // create file stream
        tf.diskName := findDiskWad(fname);
        try
          tf.stream := openDiskFileRO(tf.diskName);
        except
          tf.stream := nil;
        end;
        if (tf.stream = nil) then
        begin
          e_WriteLog(Format('NETWORK: file "%s" not found!', [fname]), TMsgType.Fatal);
          killClientByFT(nc^);
          exit;
        end;
        e_LogWritefln('client #%d requested resource #%d (file is `%s` : `%s`)', [nc.ID, ridx, fname, tf.diskName]);
        tf.size := tf.stream.size;
        tf.chunkSize := FILE_CHUNK_SIZE; // arbitrary
        tf.lastSentChunk := -1;
        tf.lastAckChunk := -1;
        tf.lastAckTime := GetTimerMS();
        tf.inProgress := False; // waiting for the first ACK or for the cancel
        GetMem(tf.diskBuffer, tf.chunkSize);
        // sent file info message
        trans_omsg.Clear();
        trans_omsg.Write(Byte(NTF_SERVER_FILE_INFO));
        trans_omsg.Write(tf.hash);
        trans_omsg.Write(tf.size);
        trans_omsg.Write(tf.chunkSize);
        trans_omsg.Write(ExtractFileName(fname));
        if not ftransSendServerMsg(nc^, trans_omsg) then exit;
      end;
    NTF_CLIENT_ABORT: // do not send requested file, or abort current transfer
      begin
        e_LogWritefln('client #%d aborted file transfer', [nc.ID]);
        clearNetClientTransfers(nc^);
      end;
    NTF_CLIENT_START: // start transfer; client may resume download by sending non-zero starting chunk
      begin
        if not Assigned(tf.stream) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        if (tf.lastSentChunk <> -1) or (tf.lastAckChunk <> -1) or (tf.inProgress) then
        begin
          // double ack, get lost
          killClientByFT(nc^);
          exit;
        end;
        if (NetEvent.packet^.dataLength < 2) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        // build packet
        if not msg.Init(NetEvent.packet^.data+1, NetEvent.packet^.dataLength-1, True) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        chunk := msg.ReadLongInt();
        if (chunk < 0) or (chunk > (tf.size+tf.chunkSize-1) div tf.chunkSize) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        e_LogWritefln('client #%d started file transfer from chunk %d', [nc.ID, chunk]);
        // start sending chunks
        tf.inProgress := True;
        tf.lastSentChunk := chunk-1;
        tf.lastAckChunk := chunk-1;
        ProcessChunkSend(nc^);
      end;
    NTF_CLIENT_ACK: // chunk ack; chunk number follows
      begin
        if not Assigned(tf.stream) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        if (tf.lastSentChunk < 0) or (not tf.inProgress) then
        begin
          // double ack, get lost
          killClientByFT(nc^);
          exit;
        end;
        if (NetEvent.packet^.dataLength < 2) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        // build packet
        if not msg.Init(NetEvent.packet^.data+1, NetEvent.packet^.dataLength-1, True) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        chunk := msg.ReadLongInt();
        if (chunk < 0) or (chunk > (tf.size+tf.chunkSize-1) div tf.chunkSize) then
        begin
          killClientByFT(nc^);
          exit;
        end;
        // do it this way, so client may seek, or request retransfers for some reason
        tf.lastAckChunk := chunk;
        tf.lastSentChunk := chunk;
        //e_LogWritefln('client #%d acked file transfer chunk %d', [nc.ID, chunk]);
        ProcessChunkSend(nc^);
      end;
    NTF_CLIENT_MAP_REQUEST:
      begin
        e_LogWritefln('client #%d requested map info', [nc.ID]);
        trans_omsg.Clear();
        dfn := findDiskWad(gGameSettings.WAD);
        if (dfn = '') then dfn := '!wad_not_found!.wad'; //FIXME
        //md5 := MD5File(dfn);
        md5 := gWADHash;
        if (not GetDiskFileInfo(dfn, fi)) then
        begin
          e_LogWritefln('client #%d requested map info, but i cannot get file info', [nc.ID]);
          killClientByFT(nc^);
          exit;
        end;
        size := fi.size;
        {
        st := openDiskFileRO(dfn);
        if not assigned(st) then exit; //wtf?!
        size := st.size;
        st.Free;
        }
        // packet type
        trans_omsg.Write(Byte(NTF_SERVER_MAP_INFO));
        // map wad name
        trans_omsg.Write(ExtractFileName(gGameSettings.WAD));
        // map wad md5
        trans_omsg.Write(md5);
        // map wad size
        trans_omsg.Write(size);
        // number of external resources for map
        trans_omsg.Write(LongInt(length(gExternalResources)));
        // external resource names
        for f := 0 to High(gExternalResources) do
        begin
          // old style packet
          //trans_omsg.Write(ExtractFileName(gExternalResources[f])); // GameDir+'/wads/'+ResList.Strings[i]
          // new style packet
          trans_omsg.Write('!');
          trans_omsg.Write(LongInt(gExternalResources[f].size));
          trans_omsg.Write(gExternalResources[f].hash);
          trans_omsg.Write(ExtractFileName(gExternalResources[f].diskName));
        end;
        // send packet
        if not ftransSendServerMsg(nc^, trans_omsg) then exit;
      end;
    else
      begin
        killClientByFT(nc^);
        exit;
      end;
  end;
end;


//**************************************************************************
//
// file transfer crap (both client and server)
//
//**************************************************************************

function getNewTimeoutEnd (): Int64;
begin
  result := GetTimerMS();
  if (g_Net_DownloadTimeout <= 0) then
  begin
    result := result+1000*60*3; // 3 minutes
  end
  else
  begin
    result := result+trunc(g_Net_DownloadTimeout*1000);
  end;
end;


// send map request to server, and wait for "map info" server reply
//
// returns `false` on error or user abort
// fills:
//    diskName: map wad file name (without a path)
//    hash: map wad hash
//    size: map wad size
//    chunkSize: set too
//    resList: list of resource wads
// returns:
//  <0 on error
//   0 on success
//   1 on user abort
//   2 on server abort
// for maps, first `tf.diskName` name will be map wad name, and `tf.hash`/`tf.size` will contain map info
function g_Net_Wait_MapInfo (var tf: TNetFileTransfer; var resList: TNetMapResourceInfoArray): Integer;
var
  ev: ENetEvent;
  rMsgId: Byte;
  Ptr: Pointer;
  msg: TMsg;
  freePacket: Boolean = false;
  ct, ett: Int64;
  status: cint;
  s: AnsiString;
  rc, f: LongInt;
  ri: ^TNetMapResourceInfo;
begin
  SetLength(resList, 0);

  // send request
  trans_omsg.Clear();
  trans_omsg.Write(Byte(NTF_CLIENT_MAP_REQUEST));
  if not ftransSendClientMsg(trans_omsg) then begin result := -1; exit; end;

  FillChar(ev, SizeOf(ev), 0);
  Result := -1;
  try
    ett := getNewTimeoutEnd();
    repeat
      status := enet_host_service(NetHost, @ev, 300);
      {
      if (status < 0) then
      begin
        g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' network error', True);
        Result := -1;
        exit;
      end;
      }
      if (status <= 0) then
      begin
        // check for timeout
        ct := GetTimerMS();
        if (ct >= ett) then
        begin
          g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' timeout reached', True);
          Result := -1;
          exit;
        end;
      end
      else
      begin
        // some event
        case ev.kind of
          ENET_EVENT_TYPE_RECEIVE:
            begin
              freePacket := true;
              if (ev.channelID <> NET_CHAN_DOWNLOAD_EX) then
              begin
                //e_LogWritefln('g_Net_Wait_MapInfo: skip message from non-transfer channel', []);
                freePacket := false;
                g_Net_Client_HandlePacket(ev.packet, g_Net_ClientLightMsgHandler);
                if (g_Res_received_map_start < 0) then begin result := -666; exit; end;
              end
              else
              begin
                ett := getNewTimeoutEnd();
                if (ev.packet.dataLength < 1) then
                begin
                  e_LogWritefln('g_Net_Wait_MapInfo: invalid server packet (no data)', []);
                  Result := -1;
                  exit;
                end;
                Ptr := ev.packet^.data;
                rMsgId := Byte(Ptr^);
                e_LogWritefln('g_Net_Wait_MapInfo: got message %u from server (dataLength=%u)', [rMsgId, ev.packet^.dataLength]);
                if (rMsgId = NTF_SERVER_FILE_INFO) then
                begin
                  e_LogWritefln('g_Net_Wait_MapInfo: waiting for map info reply, but got file info reply', []);
                  Result := -1;
                  exit;
                end
                else if (rMsgId = NTF_SERVER_ABORT) then
                begin
                  e_LogWritefln('g_Net_Wait_MapInfo: server aborted transfer', []);
                  Result := 2;
                  exit;
                end
                else if (rMsgId = NTF_SERVER_MAP_INFO) then
                begin
                  e_LogWritefln('g_Net_Wait_MapInfo: creating map info packet...', []);
                  if not msg.Init(ev.packet^.data+1, ev.packet^.dataLength-1, True) then exit;
                  e_LogWritefln('g_Net_Wait_MapInfo: parsing map info packet (rd=%d; max=%d)...', [msg.ReadCount, msg.MaxSize]);
                  SetLength(resList, 0); // just in case
                  // map wad name
                  tf.diskName := msg.ReadString();
                  e_LogWritefln('g_Net_Wait_MapInfo: map wad is `%s`', [tf.diskName]);
                  // map wad md5
                  tf.hash := msg.ReadMD5();
                  // map wad size
                  tf.size := msg.ReadLongInt();
                  e_LogWritefln('g_Net_Wait_MapInfo: map wad size is %d', [tf.size]);
                  // number of external resources for map
                  rc := msg.ReadLongInt();
                  if (rc < 0) or (rc > 1024) then
                  begin
                    e_LogWritefln('g_Net_Wait_Event: invalid number of map external resources (%d)', [rc]);
                    Result := -1;
                    exit;
                  end;
                  e_LogWritefln('g_Net_Wait_MapInfo: map external resource count is %d', [rc]);
                  SetLength(resList, rc);
                  // external resource names
                  for f := 0 to rc-1 do
                  begin
                    ri := @resList[f];
                    s := msg.ReadString();
                    if (length(s) = 0) then begin result := -1; exit; end;
                    if (s = '!') then
                    begin
                      // extended packet
                      ri.size := msg.ReadLongInt();
                      ri.hash := msg.ReadMD5();
                      ri.wadName := ExtractFileName(msg.ReadString());
                      if (length(ri.wadName) = 0) or (ri.size < 0) then begin result := -1; exit; end;
                    end
                    else
                    begin
                      // old-style packet, only name
                      ri.wadName := ExtractFileName(s);
                      if (length(ri.wadName) = 0) then begin result := -1; exit; end;
                      ri.size := -1; // unknown
                    end;
                  end;
                  e_LogWritefln('g_Net_Wait_MapInfo: got map info', []);
                  Result := 0; // success
                  exit;
                end
                else
                begin
                  e_LogWritefln('g_Net_Wait_Event: invalid server packet type', []);
                  Result := -1;
                  exit;
                end;
              end;
            end;
          ENET_EVENT_TYPE_DISCONNECT:
            begin
              if (ev.data <= NET_DISC_MAX) then
                g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' ' + _lc[TStrings_Locale(Cardinal(I_NET_DISC_NONE) + ev.data)], True);
              Result := -1;
              exit;
            end;
          else
            begin
              g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' unknown ENet event ' + IntToStr(Ord(ev.kind)), True);
              result := -1;
              exit;
            end;
        end;
        if (freePacket) then begin freePacket := false; enet_packet_destroy(ev.packet); end;
      end;
      ProcessLoading();
      if g_Net_UserRequestExit() then
      begin
        g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' user abort', True);
        Result := 1;
        exit;
      end;
    until false;
  finally
    if (freePacket) then enet_packet_destroy(ev.packet);
  end;
end;


// send file request to server, and wait for server reply
//
// returns `false` on error or user abort
// fills:
//    diskName (actually, base name)
//    hash
//    size
//    chunkSize
// returns:
//  <0 on error
//   0 on success
//   1 on user abort
//   2 on server abort
// for maps, first `tf.diskName` name will be map wad name, and `tf.hash`/`tf.size` will contain map info
function g_Net_RequestResFileInfo (resIndex: LongInt; out tf: TNetFileTransfer): Integer;
var
  ev: ENetEvent;
  rMsgId: Byte;
  Ptr: Pointer;
  msg: TMsg;
  freePacket: Boolean = false;
  ct, ett: Int64;
  status: cint;
begin
  // send request
  trans_omsg.Clear();
  trans_omsg.Write(Byte(NTF_CLIENT_FILE_REQUEST));
  trans_omsg.Write(resIndex);
  if not ftransSendClientMsg(trans_omsg) then begin result := -1; exit; end;

  FillChar(ev, SizeOf(ev), 0);
  Result := -1;
  try
    ett := getNewTimeoutEnd();
    repeat
      status := enet_host_service(NetHost, @ev, 300);
      {
      if (status < 0) then
      begin
        g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' network error', True);
        Result := -1;
        exit;
      end;
      }
      if (status <= 0) then
      begin
        // check for timeout
        ct := GetTimerMS();
        if (ct >= ett) then
        begin
          g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' timeout reached', True);
          Result := -1;
          exit;
        end;
      end
      else
      begin
        // some event
        case ev.kind of
          ENET_EVENT_TYPE_RECEIVE:
            begin
              freePacket := true;
              if (ev.channelID <> NET_CHAN_DOWNLOAD_EX) then
              begin
                //e_LogWriteln('g_Net_Wait_Event: skip message from non-transfer channel');
                freePacket := false;
                g_Net_Client_HandlePacket(ev.packet, g_Net_ClientLightMsgHandler);
                if (g_Res_received_map_start < 0) then begin result := -666; exit; end;
              end
              else
              begin
                ett := getNewTimeoutEnd();
                if (ev.packet.dataLength < 1) then
                begin
                  e_LogWriteln('g_Net_Wait_Event: invalid server packet (no data)');
                  Result := -1;
                  exit;
                end;
                Ptr := ev.packet^.data;
                rMsgId := Byte(Ptr^);
                e_LogWritefln('received transfer packet with id %d (%u bytes)', [rMsgId, ev.packet^.dataLength]);
                if (rMsgId = NTF_SERVER_FILE_INFO) then
                begin
                  if not msg.Init(ev.packet^.data+1, ev.packet^.dataLength-1, True) then exit;
                  tf.hash := msg.ReadMD5();
                  tf.size := msg.ReadLongInt();
                  tf.chunkSize := msg.ReadLongInt();
                  tf.diskName := ExtractFileName(msg.readString());
                  if (tf.size < 0) or (tf.chunkSize <> FILE_CHUNK_SIZE) or (length(tf.diskName) = 0) then
                  begin
                    e_LogWritefln('g_Net_RequestResFileInfo: invalid file info packet', []);
                    Result := -1;
                    exit;
                  end;
                  e_LogWritefln('got file info for resource #%d: size=%d; name=%s', [resIndex, tf.size, tf.diskName]);
                  Result := 0; // success
                  exit;
                end
                else if (rMsgId = NTF_SERVER_ABORT) then
                begin
                  e_LogWriteln('g_Net_RequestResFileInfo: server aborted transfer');
                  Result := 2;
                  exit;
                end
                else if (rMsgId = NTF_SERVER_MAP_INFO) then
                begin
                  e_LogWriteln('g_Net_RequestResFileInfo: waiting for map info reply, but got file info reply');
                  Result := -1;
                  exit;
                end
                else
                begin
                  e_LogWriteln('g_Net_RequestResFileInfo: invalid server packet type');
                  Result := -1;
                  exit;
                end;
              end;
            end;
          ENET_EVENT_TYPE_DISCONNECT:
            begin
              if (ev.data <= NET_DISC_MAX) then
                g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' ' + _lc[TStrings_Locale(Cardinal(I_NET_DISC_NONE) + ev.data)], True);
              Result := -1;
              exit;
            end;
          else
            begin
              g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' unknown ENet event ' + IntToStr(Ord(ev.kind)), True);
              result := -1;
              exit;
            end;
        end;
        if (freePacket) then begin freePacket := false; enet_packet_destroy(ev.packet); end;
      end;
      ProcessLoading();
      if g_Net_UserRequestExit() then
      begin
        g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' user abort', True);
        Result := 1;
        exit;
      end;
    until false;
  finally
    if (freePacket) then enet_packet_destroy(ev.packet);
  end;
end;


// call this to cancel file transfer requested by `g_Net_RequestResFileInfo()`
function g_Net_AbortResTransfer (var tf: TNetFileTransfer): Boolean;
begin
  result := false;
  e_LogWritefln('aborting file transfer...', []);
  // send request
  trans_omsg.Clear();
  trans_omsg.Write(Byte(NTF_CLIENT_ABORT));
  result := ftransSendClientMsg(trans_omsg);
  if result then enet_host_flush(NetHost);
end;


// call this to start file transfer requested by `g_Net_RequestResFileInfo()`
//
// returns `false` on error or user abort
// fills:
//    hash
//    size
//    chunkSize
// returns:
//  <0 on error
//   0 on success
//   1 on user abort
//   2 on server abort
// for maps, first `tf.diskName` name will be map wad name, and `tf.hash`/`tf.size` will contain map info
function g_Net_ReceiveResourceFile (resIndex: LongInt; var tf: TNetFileTransfer; strm: TStream): Integer;
var
  ev: ENetEvent;
  rMsgId: Byte;
  Ptr: Pointer;
  msg: TMsg;
  freePacket: Boolean = false;
  ct, ett: Int64;
  status: cint;
  nextChunk: Integer = 0;
  chunkTotal: Integer;
  chunk: Integer;
  csize: Integer;
  buf: PChar = nil;
  resumed: Boolean;
  //stx: Int64;
begin
  tf.resumed := false;
  e_LogWritefln('file `%s`, size=%d (%d)', [tf.diskName, Integer(strm.size), tf.size], TMsgType.Notify);
  // check if we should resume downloading
  resumed := (strm.size > tf.chunkSize) and (strm.size < tf.size);
  // send request
  trans_omsg.Clear();
  trans_omsg.Write(Byte(NTF_CLIENT_START));
  if resumed then chunk := strm.size div tf.chunkSize else chunk := 0;
  trans_omsg.Write(LongInt(chunk));
  if not ftransSendClientMsg(trans_omsg) then begin result := -1; exit; end;

  strm.Seek(chunk*tf.chunkSize, soFromBeginning);
  chunkTotal := (tf.size+tf.chunkSize-1) div tf.chunkSize;
  e_LogWritefln('receiving file `%s` (%d chunks)', [tf.diskName, chunkTotal], TMsgType.Notify);
  g_Game_SetLoadingText('downloading "'+ExtractFileName(tf.diskName)+'"', chunkTotal, False);
  tf.resumed := resumed;

  if (chunk > 0) then g_Game_StepLoading(chunk);
  nextChunk := chunk;

  // wait for reply data
  FillChar(ev, SizeOf(ev), 0);
  Result := -1;
  GetMem(buf, tf.chunkSize);
  try
    ett := getNewTimeoutEnd();
    repeat
      //stx := -GetTimerMS();
      status := enet_host_service(NetHost, @ev, 300);
      {
      if (status < 0) then
      begin
        g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' network error', True);
        Result := -1;
        exit;
      end;
      }
      if (status <= 0) then
      begin
        // check for timeout
        ct := GetTimerMS();
        if (ct >= ett) then
        begin
          g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' timeout reached', True);
          Result := -1;
          exit;
        end;
      end
      else
      begin
        // some event
        case ev.kind of
          ENET_EVENT_TYPE_RECEIVE:
            begin
              freePacket := true;
              if (ev.channelID <> NET_CHAN_DOWNLOAD_EX) then
              begin
                //e_LogWritefln('g_Net_Wait_Event: skip message from non-transfer channel', []);
                freePacket := false;
                g_Net_Client_HandlePacket(ev.packet, g_Net_ClientLightMsgHandler);
                if (g_Res_received_map_start < 0) then begin result := -666; exit; end;
              end
              else
              begin
                //stx := stx+GetTimerMS();
                //e_LogWritefln('g_Net_ReceiveResourceFile: stx=%d', [Integer(stx)]);
                //stx := -GetTimerMS();
                ett := getNewTimeoutEnd();
                if (ev.packet.dataLength < 1) then
                begin
                  e_LogWritefln('g_Net_ReceiveResourceFile: invalid server packet (no data)', []);
                  Result := -1;
                  exit;
                end;
                Ptr := ev.packet^.data;
                rMsgId := Byte(Ptr^);
                if (rMsgId = NTF_SERVER_DONE) then
                begin
                  e_LogWritefln('file transfer complete.', []);
                  result := 0;
                  exit;
                end
                else if (rMsgId = NTF_SERVER_CHUNK) then
                begin
                  if not msg.Init(ev.packet^.data+1, ev.packet^.dataLength-1, True) then exit;
                  chunk := msg.ReadLongInt();
                  csize := msg.ReadLongInt();
                  if (chunk <> nextChunk) then
                  begin
                    e_LogWritefln('received chunk %d, but expected chunk %d', [chunk, nextChunk]);
                    Result := -1;
                    exit;
                  end;
                  if (csize < 0) or (csize > tf.chunkSize) then
                  begin
                    e_LogWritefln('received chunk with size %d, but expected chunk size is %d', [csize, tf.chunkSize]);
                    Result := -1;
                    exit;
                  end;
                  //e_LogWritefln('got chunk #%d of #%d (csize=%d)', [chunk, (tf.size+tf.chunkSize-1) div tf.chunkSize, csize]);
                  msg.ReadData(buf, csize);
                  strm.WriteBuffer(buf^, csize);
                  nextChunk := chunk+1;
                  g_Game_StepLoading();
                  // send ack
                  trans_omsg.Clear();
                  trans_omsg.Write(Byte(NTF_CLIENT_ACK));
                  trans_omsg.Write(LongInt(chunk));
                  if not ftransSendClientMsg(trans_omsg) then begin result := -1; exit; end;
                end
                else if (rMsgId = NTF_SERVER_ABORT) then
                begin
                  e_LogWritefln('g_Net_ReceiveResourceFile: server aborted transfer', []);
                  Result := 2;
                  exit;
                end
                else
                begin
                  e_LogWritefln('g_Net_ReceiveResourceFile: invalid server packet type', []);
                  Result := -1;
                  exit;
                end;
                //stx := stx+GetTimerMS();
                //e_LogWritefln('g_Net_ReceiveResourceFile: process stx=%d', [Integer(stx)]);
              end;
            end;
          ENET_EVENT_TYPE_DISCONNECT:
            begin
              if (ev.data <= NET_DISC_MAX) then
                g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' ' + _lc[TStrings_Locale(Cardinal(I_NET_DISC_NONE) + ev.data)], True);
              Result := -1;
              exit;
            end;
          else
            begin
              g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' unknown ENet event ' + IntToStr(Ord(ev.kind)), True);
              result := -1;
              exit;
            end;
        end;
        if (freePacket) then begin freePacket := false; enet_packet_destroy(ev.packet); end;
      end;
      ProcessLoading();
      if g_Net_UserRequestExit() then
      begin
        g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_CONN] + ' user abort', True);
        Result := 1;
        exit;
      end;
    until false;
  finally
    FreeMem(buf);
    if (freePacket) then enet_packet_destroy(ev.packet);
  end;
end;


//**************************************************************************
//
// common functions
//
//**************************************************************************

function g_Net_FindSlot(): Integer;
var
  I: Integer;
  F: Boolean;
  N, C: Integer;
begin
  N := -1;
  F := False;
  C := 0;
  for I := Low(NetClients) to High(NetClients) do
  begin
    if NetClients[I].Used then
      Inc(C)
    else
      if not F then
      begin
        F := True;
        N := I;
      end;
  end;
  if C >= NetMaxClients then
  begin
    Result := -1;
    Exit;
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
    NetClients[N].WaitForFirstSpawn := False;
    NetClients[N].RCONAuth := False;
    NetClients[N].Voted := False;
    NetClients[N].Player := 0;
    clearNetClientTransfers(NetClients[N]); // just in case
  end;

  Result := N;
end;


function g_Net_Init(): Boolean;
var
  F: TextFile;
  IPstr: string;
  IP: LongWord;
  path: AnsiString;
begin
  NetIn.Clear();
  NetOut.Clear();
  NetBuf[NET_UNRELIABLE].Clear();
  NetBuf[NET_RELIABLE].Clear();
  //SetLength(NetClients, 0);
  clearNetClients(true); // clear array
  NetPeer := nil;
  NetHost := nil;
  NetMyID := -1;
  NetPlrUID1 := -1;
  NetPlrUID2 := -1;
  NetAddr.port := 25666;
  SetLength(NetBannedHosts, 0);
  path := BANLIST_FILENAME;
  if e_FindResource(DataDirs, path) = true then
  begin
    Assign(F, path);
    Reset(F);
    while not EOF(F) do
    begin
      Readln(F, IPstr);
      if StrToIp(IPstr, IP) then
        g_Net_BanHost(IP);
    end;
    CloseFile(F);
    g_Net_SaveBanList();
  end;

  //Result := (enet_initialize() = 0);
  Result := enet_init_success;
end;

procedure g_Net_Flush();
var
  T: Integer;
  P: pENetPacket;
  F, Chan: enet_uint32;
  I: Integer;
begin
  F := 0;
  Chan := NET_CHAN_GAME;

  if NetMode = NET_SERVER then
    for T := NET_UNRELIABLE to NET_RELIABLE do
    begin
      if NetBuf[T].CurSize > 0 then
      begin
        P := enet_packet_create(NetBuf[T].Data, NetBuf[T].CurSize, F);
        if not Assigned(P) then continue;
        enet_host_broadcast(NetHost, Chan, P);
        NetBuf[T].Clear();
      end;

      for I := Low(NetClients) to High(NetClients) do
      begin
        if not NetClients[I].Used then continue;
        if NetClients[I].NetOut[T].CurSize <= 0 then continue;
        P := enet_packet_create(NetClients[I].NetOut[T].Data, NetClients[I].NetOut[T].CurSize, F);
        if not Assigned(P) then continue;
        enet_peer_send(NetClients[I].Peer, Chan, P);
        NetClients[I].NetOut[T].Clear();
      end;

      // next and last iteration is always RELIABLE
      F := LongWord(ENET_PACKET_FLAG_RELIABLE);
      Chan := NET_CHAN_IMPORTANT;
    end
  else if NetMode = NET_CLIENT then
    for T := NET_UNRELIABLE to NET_RELIABLE do
    begin
      if NetBuf[T].CurSize > 0 then
      begin
        P := enet_packet_create(NetBuf[T].Data, NetBuf[T].CurSize, F);
        if not Assigned(P) then continue;
        enet_peer_send(NetPeer, Chan, P);
        NetBuf[T].Clear();
      end;
      // next and last iteration is always RELIABLE
      F := LongWord(ENET_PACKET_FLAG_RELIABLE);
      Chan := NET_CHAN_IMPORTANT;
    end;
end;

procedure g_Net_Cleanup();
begin
  NetIn.Clear();
  NetOut.Clear();
  NetBuf[NET_UNRELIABLE].Clear();
  NetBuf[NET_RELIABLE].Clear();

  //SetLength(NetClients, 0);
  clearNetClients(true); // clear array
  NetClientCount := 0;

  NetPeer := nil;
  NetHost := nil;
  g_Net_Slist_ServerClosed();
  NetMyID := -1;
  NetPlrUID1 := -1;
  NetPlrUID2 := -1;
  NetState := NET_STATE_NONE;

  NetPongSock := ENET_SOCKET_NULL;

  NetTimeToMaster := 0;
  NetTimeToUpdate := 0;
  NetTimeToReliable := 0;

  NetMode := NET_NONE;

  if NetPortThread <> NilThreadId then
    WaitForThreadTerminate(NetPortThread, 66666);

  NetPortThread := NilThreadId;
  g_Net_UnforwardPorts();

  if NetDump then
    g_Net_DumpEnd();
end;

procedure g_Net_Free();
begin
  g_Net_Cleanup();

  //enet_deinitialize();
  NetInitDone := False;
end;


//**************************************************************************
//
// SERVER FUNCTIONS
//
//**************************************************************************

function ForwardThread(Param: Pointer): PtrInt;
begin
  Result := 0;
  if not g_Net_ForwardPorts() then Result := -1;
end;

function g_Net_Host(IPAddr: LongWord; Port: enet_uint16; MaxClients: Cardinal = 16): Boolean;
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

  NetAddr.host := IPAddr;
  NetAddr.port := Port;

  NetHost := enet_host_create(@NetAddr, NET_MAXCLIENTS, NET_CHANS, 0, 0);

  if (NetHost = nil) then
  begin
    g_Console_Add(_lc[I_NET_MSG_ERROR] + Format(_lc[I_NET_ERR_HOST], [Port]));
    Result := False;
    g_Net_Cleanup;
    Exit;
  end;

  if NetForwardPorts then NetPortThread := BeginThread(ForwardThread);

  NetPongSock := enet_socket_create(ENET_SOCKET_TYPE_DATAGRAM);
  if NetPongSock <> ENET_SOCKET_NULL then
  begin
    NetPongAddr.host := IPAddr;
    NetPongAddr.port := NET_PING_PORT;
    if enet_socket_bind(NetPongSock, @NetPongAddr) < 0 then
    begin
      enet_socket_destroy(NetPongSock);
      NetPongSock := ENET_SOCKET_NULL;
    end
    else
      enet_socket_set_option(NetPongSock, ENET_SOCKOPT_NONBLOCK, 1);
  end;

  NetMode := NET_SERVER;
  NetOut.Clear();
  NetBuf[NET_UNRELIABLE].Clear();
  NetBuf[NET_RELIABLE].Clear();

  if NetDump then
    g_Net_DumpStart();
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
      NetClients[I].Player := 0;
      NetClients[I].Crimes := 0;
      NetClients[I].AuthTime := 0;
      NetClients[I].MsgTime := 0;
      NetClients[I].NetOut[NET_UNRELIABLE].Free();
      NetClients[I].NetOut[NET_RELIABLE].Free();
    end;

  clearNetClients(false); // don't clear array
  g_Net_Slist_ServerClosed();
  if NetPongSock <> ENET_SOCKET_NULL then
    enet_socket_destroy(NetPongSock);

  g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_DIE]);
  enet_host_destroy(NetHost);

  NetMode := NET_NONE;

  g_Net_Cleanup;
  e_WriteLog('NET: Server stopped', TMsgType.Notify);
end;


procedure g_Net_Host_Send(ID: Integer; Reliable: Boolean; Chan: Byte = NET_CHAN_GAME);
var
  T: Integer;
begin
  if (Reliable) then
    T := NET_RELIABLE
  else
    T := NET_UNRELIABLE;

  if (ID >= 0) then
  begin
    if ID > High(NetClients) then Exit;
    if NetClients[ID].Peer = nil then Exit;
    // write size first
    NetClients[ID].NetOut[T].Write(Integer(NetOut.CurSize));
    NetClients[ID].NetOut[T].Write(NetOut);
  end
  else
  begin
    // write size first
    NetBuf[T].Write(Integer(NetOut.CurSize));
    NetBuf[T].Write(NetOut);
  end;

  if NetDump then g_Net_DumpSendBuffer();
  NetOut.Clear();
end;

procedure g_Net_Host_CheckPings();
var
  ClAddr: ENetAddress;
  Buf: ENetBuffer;
  Len: Integer;
  ClTime: Int64;
  Ping: array [0..9] of Byte;
  NPl: Byte;
begin
  if (NetPongSock = ENET_SOCKET_NULL) or (NetHost = nil) then Exit;

  Buf.data := Addr(Ping[0]);
  Buf.dataLength := 2+8;

  Ping[0] := 0;

  Len := enet_socket_receive(NetPongSock, @ClAddr, @Buf, 1);
  if Len < 0 then Exit;

  if (Ping[0] = Ord('D')) and (Ping[1] = Ord('F')) then
  begin
    ClTime := Int64(Addr(Ping[2])^);

    NetOut.Clear();
    NetOut.Write(Byte(Ord('D')));
    NetOut.Write(Byte(Ord('F')));
    NetOut.Write(NetHost.address.port);
    NetOut.Write(ClTime);
    TMasterHost.writeInfo(NetOut);
    NPl := 0;
    if gPlayer1 <> nil then Inc(NPl);
    if gPlayer2 <> nil then Inc(NPl);
    NetOut.Write(NPl);
    NetOut.Write(gNumBots);

    Buf.data := NetOut.Data;
    Buf.dataLength := NetOut.CurSize;
    enet_socket_send(NetPongSock, @ClAddr, @Buf, 1);

    NetOut.Clear();
  end;
end;

procedure g_Net_Host_CheckTimeouts();
var
  ID: Integer;
begin
  for ID := Low(NetClients) to High(NetClients) do
  begin
    with NetClients[ID] do
    begin
      if (Peer = nil) or (State = NET_STATE_NONE) then continue;
      if (State = NET_STATE_AUTH) and (AuthTime > 0) and (AuthTime <= gTime) then
      begin
        g_Net_Penalize(@NetClients[ID], 'auth taking too long');
        AuthTime := gTime + 500; // do it twice a second to give them a chance
      end
      else if (State = NET_STATE_GAME) and (MsgTime > 0) and (MsgTime <= gTime) then
      begin
        g_Net_Penalize(@NetClients[ID], 'message timeout');
        AuthTime := gTime + 500; // do it twice a second to give them a chance
      end;
    end;
  end;
end;

procedure g_Net_Host_Disconnect_Client(ID: Integer; Force: Boolean = False);
var
  TP: TPlayer;
  TC: pTNetClient;
begin
  TC := @NetClients[ID];
  if (TC = nil) then Exit;
  clearNetClient(NetClients[ID]);
  if not (TC^.Used) then Exit;

  TP := g_Player_Get(TC^.Player);

  if TP <> nil then
  begin
    TP.Lives := 0;
    TP.Kill(K_SIMPLEKILL, 0, HIT_DISCON);
    g_Console_Add(Format(_lc[I_PLAYER_LEAVE], [TP.Name]), True);
    e_WriteLog('NET: Client ' + TP.Name + ' [' + IntToStr(TC^.ID) + '] disconnected.', TMsgType.Notify);
    g_Player_Remove(TP.UID);
  end;

  if (TC^.Peer^.data <> nil) then
  begin
    FreeMemory(TC^.Peer^.data);
    TC^.Peer^.data := nil;
  end;

  if (Force) then
    enet_peer_reset(TC^.Peer);

  TC^.Used := False;
  TC^.State := NET_STATE_NONE;
  TC^.Peer := nil;
  TC^.Player := 0;
  TC^.Crimes := 0;
  TC^.AuthTime := 0;
  TC^.MsgTime := 0;
  TC^.RequestedFullUpdate := False;
  TC^.WaitForFirstSpawn := False;
  TC^.NetOut[NET_UNRELIABLE].Free();
  TC^.NetOut[NET_RELIABLE].Free();

  g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_MSG_HOST_DISC], [ID]));
  Dec(NetClientCount);

  if NetUseMaster then g_Net_Slist_ServerPlayerLeaves();
end;


function g_Net_Host_Update(): enet_size_t;
var
  IP: string;
  Port: Word;
  ID: Integer;
  TC: pTNetClient;
begin
  IP := '';
  Result := 0;

  if NetUseMaster then g_Net_Slist_Pulse();
  g_Net_Host_CheckPings();
  g_Net_Host_CheckTimeouts();

  while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
  begin
    case (NetEvent.kind) of
      ENET_EVENT_TYPE_CONNECT:
      begin
        IP := IpToStr(NetEvent.Peer^.address.host);
        Port := NetEvent.Peer^.address.port;
        g_Console_Add(_lc[I_NET_MSG] +
          Format(_lc[I_NET_MSG_HOST_CONN], [IP, Port]));
        e_WriteLog('NET: Connection request from ' + IP + '.', TMsgType.Notify);

        if (NetEvent.data <> NET_PROTOCOL_VER) then
        begin
          g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
            _lc[I_NET_DISC_PROTOCOL]);
          e_WriteLog('NET: Connection request from ' + IP + ' rejected: version mismatch',
            TMsgType.Notify);
          NetEvent.peer^.data := GetMemory(SizeOf(Byte));
          Byte(NetEvent.peer^.data^) := 255;
          enet_peer_disconnect(NetEvent.peer, NET_DISC_PROTOCOL);
          enet_host_flush(NetHost);
          Exit;
        end;

        if g_Net_IsHostBanned(NetEvent.Peer^.address.host) then
        begin
          g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
            _lc[I_NET_DISC_BAN]);
          e_WriteLog('NET: Connection request from ' + IP + ' rejected: banned',
            TMsgType.Notify);
          NetEvent.peer^.data := GetMemory(SizeOf(Byte));
          Byte(NetEvent.peer^.data^) := 255;
          enet_peer_disconnect(NetEvent.Peer, NET_DISC_BAN);
          enet_host_flush(NetHost);
          Exit;
        end;

        ID := g_Net_FindSlot();

        if ID < 0 then
        begin
          g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_HOST_REJECT] +
            _lc[I_NET_DISC_FULL]);
          e_WriteLog('NET: Connection request from ' + IP + ' rejected: server full',
            TMsgType.Notify);
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
        NetClients[ID].Player := 0;
        NetClients[ID].Crimes := 0;
        NetClients[ID].RCONAuth := False;
        NetClients[ID].NetOut[NET_UNRELIABLE].Alloc(NET_BUFSIZE*2);
        NetClients[ID].NetOut[NET_RELIABLE].Alloc(NET_BUFSIZE*2);
        if (NetAuthTimeout > 0) then
          NetClients[ID].AuthTime := gTime + NetAuthTimeout
        else
          NetClients[ID].AuthTime := 0;
        if (NetPacketTimeout > 0) then
          NetClients[ID].MsgTime := gTime + NetPacketTimeout
        else
          NetClients[ID].MsgTime := 0;
        clearNetClientTransfers(NetClients[ID]); // just in case

        enet_peer_timeout(NetEvent.peer, ENET_PEER_TIMEOUT_LIMIT * 2, ENET_PEER_TIMEOUT_MINIMUM * 2, ENET_PEER_TIMEOUT_MAXIMUM * 2);

        Inc(NetClientCount);
        g_Console_Add(_lc[I_NET_MSG] + Format(_lc[I_NET_MSG_HOST_ADD], [ID]));
      end;

      ENET_EVENT_TYPE_RECEIVE:
      begin
        //e_LogWritefln('RECEIVE: chan=%u', [NetEvent.channelID]);
        if (NetEvent.channelID = NET_CHAN_DOWNLOAD_EX) then
        begin
          ProcessDownloadExPacket();
        end
        else
        begin
          ID := Byte(NetEvent.peer^.data^);
          if ID > High(NetClients) then Exit;
          TC := @NetClients[ID];

          if (NetPacketTimeout > 0) then
            TC^.MsgTime := gTime + NetPacketTimeout;

          if NetDump then g_Net_DumpRecvBuffer(NetEvent.packet^.data, NetEvent.packet^.dataLength);
          g_Net_Host_HandlePacket(TC, NetEvent.packet, g_Net_HostMsgHandler);
        end;
      end;

      ENET_EVENT_TYPE_DISCONNECT:
      begin
        ID := Byte(NetEvent.peer^.data^);
        if ID > High(NetClients) then Exit;
        g_Net_Host_Disconnect_Client(ID);
      end;
    end;
  end;
end;


//**************************************************************************
//
// CLIENT FUNCTIONS
//
//**************************************************************************

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
    e_WriteLog('NET: Kicked from server: ' + IntToStr(NetEvent.data), TMsgType.Notify);
    if (NetEvent.data <= NET_DISC_MAX) then
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
  e_WriteLog('NET: Disconnected', TMsgType.Notify);
end;

procedure g_Net_Client_Send(Reliable: Boolean; Chan: Byte = NET_CHAN_GAME);
var
  T: Integer;
begin
  if (Reliable) then
    T := NET_RELIABLE
  else
    T := NET_UNRELIABLE;

  // write size first
  NetBuf[T].Write(Integer(NetOut.CurSize));
  NetBuf[T].Write(NetOut);

  if NetDump then g_Net_DumpSendBuffer();
  NetOut.Clear();
  g_Net_Flush(); // FIXME: for now, send immediately
end;

function g_Net_Client_Update(): enet_size_t;
begin
  Result := 0;
  while (NetHost <> nil) and (enet_host_service(NetHost, @NetEvent, 0) > 0) do
  begin
    case NetEvent.kind of
      ENET_EVENT_TYPE_RECEIVE:
      begin
        if (NetEvent.channelID = NET_CHAN_DOWNLOAD_EX) then continue; // ignore all download packets, they're processed by separate code
        if NetDump then g_Net_DumpRecvBuffer(NetEvent.packet^.data, NetEvent.packet^.dataLength);
        g_Net_Client_HandlePacket(NetEvent.packet, g_Net_ClientMsgHandler);
      end;

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
      begin
        if (NetEvent.channelID = NET_CHAN_DOWNLOAD_EX) then continue; // ignore all download packets, they're processed by separate code
        if NetDump then g_Net_DumpRecvBuffer(NetEvent.packet^.data, NetEvent.packet^.dataLength);
        g_Net_Client_HandlePacket(NetEvent.packet, g_Net_ClientLightMsgHandler);
      end;

      ENET_EVENT_TYPE_DISCONNECT:
      begin
        g_Net_Disconnect(True);
        Result := 1;
        Exit;
      end;
    end;
  end;
  g_Net_Flush();
end;

function g_Net_Connect(IP: string; Port: enet_uint16): Boolean;
var
  OuterLoop: Boolean;
  TimeoutTime, T: Int64;
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

  // предупредить что ждем слишком долго через N секунд
  TimeoutTime := sys_GetTicks() + NET_CONNECT_TIMEOUT;

  OuterLoop := True;
  while OuterLoop do
  begin
    while (enet_host_service(NetHost, @NetEvent, 0) > 0) do
    begin
      if (NetEvent.kind = ENET_EVENT_TYPE_CONNECT) then
      begin
        g_Console_Add(_lc[I_NET_MSG] + _lc[I_NET_MSG_CLIENT_DONE]);
        NetMode := NET_CLIENT;
        NetOut.Clear();
        enet_peer_timeout(NetPeer, ENET_PEER_TIMEOUT_LIMIT * 2, ENET_PEER_TIMEOUT_MINIMUM * 2, ENET_PEER_TIMEOUT_MAXIMUM * 2);
        NetClientIP := IP;
        NetClientPort := Port;
        if NetDump then
          g_Net_DumpStart();
        Exit;
      end;
    end;

    T := sys_GetTicks();
    if T > TimeoutTime then
    begin
      TimeoutTime := T + NET_CONNECT_TIMEOUT * 100; // одного предупреждения хватит
      g_Console_Add(_lc[I_NET_MSG_TIMEOUT_WARN], True);
      g_Console_Add(Format(_lc[I_NET_MSG_PORTS], [Integer(Port), Integer(NET_PING_PORT)]), True);
    end;

    ProcessLoading(true);

    if e_KeyPressed(IK_SPACE) or e_KeyPressed(IK_ESCAPE) or e_KeyPressed(VK_ESCAPE) or
       e_KeyPressed(JOY0_JUMP) or e_KeyPressed(JOY1_JUMP) or e_KeyPressed(JOY2_JUMP) or e_KeyPressed(JOY3_JUMP) then
      OuterLoop := False;
  end;

  g_Console_Add(_lc[I_NET_MSG_ERROR] + _lc[I_NET_ERR_TIMEOUT], True);
  g_Console_Add(Format(_lc[I_NET_MSG_PORTS], [Integer(Port), Integer(NET_PING_PORT)]), True);
  if NetPeer <> nil then enet_peer_reset(NetPeer);
  if NetHost <> nil then
  begin
    enet_host_destroy(NetHost);
    NetHost := nil;
  end;
  g_Net_Cleanup();
  Result := False;
end;

function IpToStr(IP: LongWord): string;
var
  Ptr: Pointer;
begin
  Ptr := Addr(IP);
  Result :=          IntToStr(PByte(Ptr + 0)^) + '.';
  Result := Result + IntToStr(PByte(Ptr + 1)^) + '.';
  Result := Result + IntToStr(PByte(Ptr + 2)^) + '.';
  Result := Result + IntToStr(PByte(Ptr + 3)^);
end;

function StrToIp(IPstr: string; var IP: LongWord): Boolean;
var
  EAddr: ENetAddress;
begin
  Result := enet_address_set_host(@EAddr, PChar(@IPstr[1])) = 0;
  IP := EAddr.host;
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
      if Copy(LowerCase(pl.Name), 1, Length(Name)) <> LowerCase(Name) then continue;
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

function g_Net_ClientName_ByID(ID: Integer): string;
var
  a: Integer;
  pl: TPlayer;
begin
  Result := '';
  if ID = NET_EVERYONE then
    Exit;
  for a := Low(NetClients) to High(NetClients) do
    if (NetClients[a].ID = ID) and (NetClients[a].Used) and (NetClients[a].State = NET_STATE_GAME) then
    begin
      pl := g_Player_Get(NetClients[a].Player);
      if pl = nil then Exit;
      Result := pl.Name;
    end;
end;

procedure g_Net_SendData(Data: AByte; peer: pENetPeer; Reliable: Boolean; Chan: Byte = NET_CHAN_DOWNLOAD);
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
    enet_host_broadcast(NetHost, Chan, P);
  end;

  enet_host_flush(NetHost);
end;

function g_Net_IsHostBanned(IP: LongWord; Perm: Boolean = False): Boolean;
var
  I: Integer;
begin
  Result := False;
  if NetBannedHosts = nil then
    Exit;
  for I := 0 to High(NetBannedHosts) do
    if (NetBannedHosts[I].IP = IP) and ((not Perm) or (NetBannedHosts[I].Perm)) then
    begin
      Result := True;
      break;
    end;
end;

procedure g_Net_BanHost(IP: LongWord; Perm: Boolean = True); overload;
var
  I, P: Integer;
begin
  if IP = 0 then
    Exit;
  if g_Net_IsHostBanned(IP, Perm) then
    Exit;

  P := -1;
  for I := Low(NetBannedHosts) to High(NetBannedHosts) do
    if NetBannedHosts[I].IP = 0 then
    begin
      P := I;
      break;
    end;

  if P < 0 then
  begin
    SetLength(NetBannedHosts, Length(NetBannedHosts) + 1);
    P := High(NetBannedHosts);
  end;

  NetBannedHosts[P].IP := IP;
  NetBannedHosts[P].Perm := Perm;
end;

procedure g_Net_BanHost(IP: string; Perm: Boolean = True); overload;
var
  a: LongWord;
  b: Boolean;
begin
  b := StrToIp(IP, a);
  if b then
    g_Net_BanHost(a, Perm);
end;

procedure g_Net_UnbanNonPermHosts();
var
  I: Integer;
begin
  if NetBannedHosts = nil then
    Exit;
  for I := Low(NetBannedHosts) to High(NetBannedHosts) do
    if (NetBannedHosts[I].IP > 0) and not NetBannedHosts[I].Perm then
    begin
      NetBannedHosts[I].IP := 0;
      NetBannedHosts[I].Perm := True;
    end;
end;

function g_Net_UnbanHost(IP: string): Boolean; overload;
var
  a: LongWord;
begin
  Result := StrToIp(IP, a);
  if Result then
    Result := g_Net_UnbanHost(a);
end;

function g_Net_UnbanHost(IP: LongWord): Boolean; overload;
var
  I: Integer;
begin
  Result := False;
  if IP = 0 then
    Exit;
  if NetBannedHosts = nil then
    Exit;
  for I := 0 to High(NetBannedHosts) do
    if NetBannedHosts[I].IP = IP then
    begin
      NetBannedHosts[I].IP := 0;
      NetBannedHosts[I].Perm := True;
      Result := True;
      // no break here to clear all bans of this host, perm and non-perm
    end;
end;

procedure g_Net_SaveBanList();
var
  F: TextFile;
  I: Integer;
  path: AnsiString;
begin
  path := e_GetWriteableDir(DataDirs);
  if path <> '' then
  begin
    path := e_CatPath(path, BANLIST_FILENAME);
    Assign(F, path);
    Rewrite(F);
    if NetBannedHosts <> nil then
      for I := 0 to High(NetBannedHosts) do
        if NetBannedHosts[I].Perm and (NetBannedHosts[I].IP > 0) then
          Writeln(F, IpToStr(NetBannedHosts[I].IP));
    CloseFile(F)
  end
end;

procedure g_Net_Penalize(C: pTNetClient; Reason: string);
var
  s: string;
begin
  e_LogWritefln('NET: client #%u (cid #%u) triggered a penalty (%d/%d): %s',
    [C^.ID, C^.Player, C^.Crimes + 1, NetAutoBanLimit, Reason]);

  if (NetAutoBanLimit <= 0) then Exit;

  if (C^.Crimes >= NetAutoBanLimit) then
  begin
    // we have tried asking nicely before, now it is time to die
    e_LogWritefln('NET: client #%u (cid #%u) force kicked',
      [C^.ID, C^.Player]);
    g_Net_Host_Disconnect_Client(C^.ID, True);
    Exit;
  end;

  Inc(C^.Crimes);

  if (NetAutoBanWarn) then
    MH_SEND_Chat('You have been warned by the server: ' + Reason, NET_CHAT_SYSTEM, C^.ID);

  if (C^.Crimes >= NetAutoBanLimit) then
  begin
    s := '#' + IntToStr(C^.ID); // can't be arsed
    g_Net_BanHost(C^.Peer^.address.host, NetAutoBanPerm);
    enet_peer_disconnect(C^.Peer, NET_DISC_BAN);
    g_Console_Add(Format(_lc[I_PLAYER_BAN], [s]));
    MH_SEND_GameEvent(NET_EV_PLAYER_BAN, 0, s);
    g_Net_Slist_ServerPlayerLeaves();
  end;
end;

procedure g_Net_DumpStart();
begin
  if NetMode = NET_SERVER then
    NetDumpFile := e_CreateResource(LogDirs, NETDUMP_FILENAME + '_server')
  else
    NetDumpFile := e_CreateResource(LogDirs, NETDUMP_FILENAME + '_client');
end;

procedure g_Net_DumpSendBuffer();
begin
  writeInt(NetDumpFile, gTime);
  writeInt(NetDumpFile, LongWord(NetOut.CurSize));
  writeInt(NetDumpFile, Byte(1));
  NetDumpFile.WriteBuffer(NetOut.Data^, NetOut.CurSize);
end;

procedure g_Net_DumpRecvBuffer(Buf: penet_uint8; Len: LongWord);
begin
  if (Buf = nil) or (Len = 0) then Exit;
  writeInt(NetDumpFile, gTime);
  writeInt(NetDumpFile, Len);
  writeInt(NetDumpFile, Byte(0));
  NetDumpFile.WriteBuffer(Buf^, Len);
end;

procedure g_Net_DumpEnd();
begin
  NetDumpFile.Free();
  NetDumpFile := nil;
end;

function g_Net_ForwardPorts(ForwardPongPort: Boolean = True): Boolean;
{$IFDEF USE_MINIUPNPC}
var
  DevList: PUPNPDev;
  Urls: TUPNPUrls;
  Data: TIGDDatas;
  LanAddr: array [0..255] of Char;
  StrPort: AnsiString;
  Err, I: Integer;
begin
  Result := False;

  if NetHost = nil then
    exit;

  if NetPortForwarded = NetHost.address.port then
  begin
    Result := True;
    exit;
  end;

  NetPongForwarded := False;
  NetPortForwarded := 0;

  DevList := upnpDiscover(1000, nil, nil, 0, 0, 2, Addr(Err));
  if DevList = nil then
  begin
    conwritefln('port forwarding failed: upnpDiscover() failed: %d', [Err]);
    exit;
  end;

  I := UPNP_GetValidIGD(DevList, @Urls, @Data, Addr(LanAddr[0]), 256);

  if I = 0 then
  begin
    conwriteln('port forwarding failed: could not find an IGD device on this LAN');
    FreeUPNPDevList(DevList);
    FreeUPNPUrls(@Urls);
    exit;
  end;

  StrPort := IntToStr(NetHost.address.port);
  I := UPNP_AddPortMapping(
    Urls.controlURL, Addr(data.first.servicetype[1]),
    PChar(StrPort), PChar(StrPort), Addr(LanAddr[0]), PChar('D2DF'),
    PChar('UDP'), nil, PChar('0')
  );

  if I <> 0 then
  begin
    conwritefln('forwarding port %d failed: error %d', [NetHost.address.port, I]);
    FreeUPNPDevList(DevList);
    FreeUPNPUrls(@Urls);
    exit;
  end;

  if ForwardPongPort then
  begin
    StrPort := IntToStr(NET_PING_PORT);
    I := UPNP_AddPortMapping(
      Urls.controlURL, Addr(data.first.servicetype[1]),
      PChar(StrPort), PChar(StrPort), Addr(LanAddr[0]), PChar('D2DF'),
      PChar('UDP'), nil, PChar('0')
    );

    if I <> 0 then
    begin
      conwritefln('forwarding port %d failed: error %d', [NET_PING_PORT, I]);
      NetPongForwarded := False;
    end
    else
    begin
      conwritefln('forwarded port %d successfully', [NET_PING_PORT]);
      NetPongForwarded := True;
    end;
  end;

  conwritefln('forwarded port %d successfully', [NetHost.address.port]);
  NetIGDControl := AnsiString(Urls.controlURL);
  NetIGDService := data.first.servicetype;
  NetPortForwarded := NetHost.address.port;

  FreeUPNPDevList(DevList);
  FreeUPNPUrls(@Urls);
  Result := True;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

procedure g_Net_UnforwardPorts();
{$IFDEF USE_MINIUPNPC}
var
  I: Integer;
  StrPort: AnsiString;
begin
  if NetPortForwarded = 0 then Exit;

  conwriteln('unforwarding ports...');

  StrPort := IntToStr(NetPortForwarded);
  I := UPNP_DeletePortMapping(
    PChar(NetIGDControl), Addr(NetIGDService[1]),
    PChar(StrPort), PChar('UDP'), nil
  );
  conwritefln('  port %d: %d', [NetPortForwarded, I]);

  if NetPongForwarded then
  begin
    NetPongForwarded := False;
    StrPort := IntToStr(NET_PING_PORT);
    I := UPNP_DeletePortMapping(
      PChar(NetIGDControl), Addr(NetIGDService[1]),
      PChar(StrPort), PChar('UDP'), nil
    );
    conwritefln('  port %d: %d', [NET_PING_PORT, I]);
  end;

  NetPortForwarded := 0;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure NetServerCVars(P: SSArray);
var
  cmd, s: string;
  a, b: Integer;
begin
  cmd := LowerCase(P[0]);
  case cmd of
    'sv_name':
      begin
        if (Length(P) > 1) and (Length(P[1]) > 0) then
        begin
          NetServerName := P[1];
          if Length(NetServerName) > 64 then
            SetLength(NetServerName, 64);
          g_Net_Slist_ServerRenamed();
        end;
        g_Console_Add(cmd + ' "' + NetServerName + '"');
      end;
    'sv_passwd':
      begin
        if (Length(P) > 1) and (Length(P[1]) > 0) then
        begin
          NetPassword := P[1];
          if Length(NetPassword) > 24 then
            SetLength(NetPassword, 24);
          g_Net_Slist_ServerRenamed();
        end;
        g_Console_Add(cmd + ' "' + AnsiLowerCase(NetPassword) + '"');
      end;
    'sv_maxplrs':
      begin
        if (Length(P) > 1) then
        begin
          NetMaxClients := nclamp(StrToIntDef(P[1], NetMaxClients), 1, NET_MAXCLIENTS);
          if g_Game_IsServer and g_Game_IsNet then
          begin
            b := 0;
            for a := 0 to High(NetClients) do
            begin
              if NetClients[a].Used then
              begin
                Inc(b);
                if b > NetMaxClients then
                begin
                  s := g_Player_Get(NetClients[a].Player).Name;
                  enet_peer_disconnect(NetClients[a].Peer, NET_DISC_FULL);
                  g_Console_Add(Format(_lc[I_PLAYER_KICK], [s]));
                  MH_SEND_GameEvent(NET_EV_PLAYER_KICK, 0, s);
                end;
              end;
            end;
            g_Net_Slist_ServerRenamed();
          end;
        end;
        g_Console_Add(cmd + ' ' + IntToStr(NetMaxClients));
      end;
    'sv_public':
      begin
        if (Length(P) > 1) then
        begin
          NetUseMaster := StrToIntDef(P[1], Byte(NetUseMaster)) <> 0;
          if NetUseMaster then g_Net_Slist_Public() else g_Net_Slist_Private();
        end;
        g_Console_Add(cmd + ' ' + IntToStr(Byte(NetUseMaster)));
      end;
    'sv_port':
      begin
        if (Length(P) > 1) then
        begin
          if not g_Game_IsNet then
            NetPort := nclamp(StrToIntDef(P[1], NetPort), 0, $FFFF)
          else
            g_Console_Add(_lc[I_MSG_NOT_NETGAME]);
        end;
        g_Console_Add(cmd + ' ' + IntToStr(Ord(NetUseMaster)));
      end;
  end;
end;

initialization
  conRegVar('cl_downloadtimeout', @g_Net_DownloadTimeout, 0.0, 1000000.0, '', 'timeout in seconds, 0 to disable it');
  conRegVar('cl_predictself', @NetPredictSelf, '', 'predict local player');
  conRegVar('cl_forceplayerupdate', @NetForcePlayerUpdate, '', 'update net players on NET_MSG_PLRPOS');
  conRegVar('cl_interp', @NetInterpLevel, '', 'net player interpolation steps');
  conRegVar('cl_last_ip', @NetClientIP, '', 'address of the last you have connected to');
  conRegVar('cl_last_port', @NetClientPort, '', 'port of the last server you have connected to');
  conRegVar('cl_deafen', @NetDeafLevel, '', 'filter server messages (0-3)');

  conRegVar('sv_forwardports', @NetForwardPorts, '', 'forward server port using miniupnpc (requires server restart)');
  conRegVar('sv_rcon', @NetAllowRCON, '', 'enable remote console');
  conRegVar('sv_rcon_password', @NetRCONPassword, '', 'remote console password');
  conRegVar('sv_update_interval', @NetUpdateRate, '', 'unreliable update interval');
  conRegVar('sv_reliable_interval', @NetRelupdRate, '', 'reliable update interval');
  conRegVar('sv_master_interval', @NetMasterRate, '', 'master server update interval');

  conRegVar('sv_autoban_threshold', @NetAutoBanLimit, '', 'max crimes before autoban (0 = no autoban)');
  conRegVar('sv_autoban_permanent', @NetAutoBanPerm, '', 'whether autobans are permanent');
  conRegVar('sv_autoban_warn', @NetAutoBanWarn, '', 'send warnings to the client when he triggers penalties');

  conRegVar('sv_auth_timeout', @NetAuthTimeout, '', 'number of msec in which connecting clients must complete auth (0 = unlimited)');
  conRegVar('sv_packet_timeout', @NetPacketTimeout, '', 'number of msec the client must idle to be kicked (0 = unlimited)');

  conRegVar('net_master_list', @NetMasterList, '', 'list of master servers');

  SetLength(NetClients, 0);
  g_Net_DownloadTimeout := 60;
  NetIn.Alloc(NET_BUFSIZE);
  NetOut.Alloc(NET_BUFSIZE);
  NetBuf[NET_UNRELIABLE].Alloc(NET_BUFSIZE*2);
  NetBuf[NET_RELIABLE].Alloc(NET_BUFSIZE*2);
  trans_omsg.Alloc(NET_BUFSIZE);
finalization
  NetIn.Free();
  NetOut.Free();
  NetBuf[NET_UNRELIABLE].Free();
  NetBuf[NET_RELIABLE].Free();
end.
