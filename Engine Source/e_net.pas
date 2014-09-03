unit e_net;

interface

function e_Net_Init(): Boolean;
procedure e_Net_Free();
procedure e_Net_Clear();
procedure e_Net_ClearAPL();
function e_Net_GetExternalIP(): PChar;
function e_Net_GetHost(): PChar;
function e_Net_GetLocalIP(): PChar;
function e_Net_HostToIP(Host: PChar): PChar;
function e_Net_InitSocket(Port: WORD): Integer;
function e_Net_Write(Buf: Pointer; Count: Integer): Boolean;
function e_Net_Send(IP: PChar; Port: WORD; APL: Boolean): Integer;
function e_Net_Recv(Buf: Pointer; Count: Integer; var IP: PChar; var Port: Integer;
                   var RecvBytes: Integer): Integer;
procedure e_Net_Update();

implementation

uses
 Windows, WinSock;

const
 MaxBufLen = 65507;
 APLreglen = 128;

type
 TByteArray = array [0..1024] of Byte;
 PByteArray = ^TByteArray;

 PAPLpacket = ^TAPLpacket;
 TAPLpacket = record
  trys: Byte;
  UID:  WORD;
  Time: DWORD;
  Size: WORD;
  Port: WORD;
  IP:   string;
  Data: PByteArray;
 end;

 TAPLreg = record
  UID:  WORD;
  Port: WORD;
  IP:   string;
 end;

var
 NetReady:  Boolean = False;
 NetBuf:    PByteArray;
 NetBufLen: Integer;
 NetSocket: Integer = -1;

 NetAPLtime: DWORD = 3000;
 NetAPLs:    array of PAPLpacket;
 NetAPLUID:  WORD = 5;
 Nettrys:    Byte = 4;

 APLreg:  array [0..APLreglen-1] of TAPLreg;
 regseek: Integer = 0;

var
 NettmpBuf: PByteArray;

function StrPas(str: PChar): string;
begin
 Result := str;
end;

procedure e_Net_Free();
begin
 if NetReady then
 begin
  if NetSocket > 0 then CloseSocket(NetSocket);

  NetSocket := -1;
  FreeMem(NetBuf);
  FreeMem(NettmpBuf);
  NetBuf := nil;
  NettmpBuf := nil;
  NetAPLs := nil;
  WSACleanup;
  NetReady := False;
 end;
end;

procedure e_Net_Clear();
begin
 NetBufLen := 1;
end;

procedure e_Net_ClearAPL();
var
 i: Integer;
begin
 for i := 0 to Length(NetAPLs)-1 do
 begin
  FreeMem(NetAPLs[i]^.Data);
  Dispose(NetAPLs[i]);
  NetAPLs[i] := nil;
 end;

 NetAPLs := nil;
end;

function e_Net_Init(): Boolean;
var
 d: WSAData;
 i: Integer;
begin
 Result := False;

 e_Net_Free();
 NetReady := False;
 if WSAStartup($0101, d) <> 0 then Exit;

 NetReady := True;
 e_Net_Clear();

 for i := 0 to Length(NetAPLs) - 1 do
  if NetAPLs[i] <> nil then
  begin
   FreeMem(NetAPLs[i]^.Data);
   Dispose(NetAPLs[i]);
  end;

 NetSocket := -1;
 NetAPLs := nil;
 NetAPLUID := 1;

 if NetBuf = nil then GetMem(NetBuf, MaxBufLen);
 if NettmpBuf = nil then GetMem(NettmpBuf, MaxBufLen);

 Result := True;
end;

function e_Net_InitSocket(Port: Word): Integer;
var
 sock, flag, i: integer;
 address: sockaddr_in;
begin
 Result := 0;
 i := 1;
 flag := 1;

 if NetSocket > 0 then CloseSocket(NetSocket);

 sock := socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
 if sock = -1 then Exit;

 if ioctlsocket(sock, FIONBIO, flag) = -1 then Exit;

 setsockopt(sock, SOL_SOCKET, SO_BROADCAST, PChar(@i), SizeOf(i));

 address.sin_addr.S_addr := INADDR_ANY;
 address.sin_port := htons(Port);
 address.sin_family := AF_INET;

 if bind(sock, address, sizeof(address)) = -1 then
 begin
  CloseSocket(sock);
  Exit;
 end;

 NetSocket := sock;
 Result := sock;
end;

function e_Net_GetLocalIP(): PChar;
var
 Error: DWORD;
 HostEntry: PHostEnt;
 Address: In_Addr;
 Buffer: array [0..63] of Char;
begin
 GetHostName(Buffer, SizeOf(Buffer));
 HostEntry := gethostbyname(Buffer);
 Error := GetLastError;

 if Error = 0 then
 begin
  Address := PInAddr(HostEntry^.h_addr_list^)^;
  Result  := inet_ntoa(Address);
 end else Result := '';
end;

function e_Net_GetExternalIP(): PChar;
type
 TaPInAddr = array[0..10] of PInAddr;
 PaPInAddr = ^TaPInAddr;

var
 phe: PHostEnt;
 p: PaPInAddr;
 Buffer: array [0..63] of Char;
begin
 Result := nil;

 GetHostName(Buffer, SizeOf(Buffer));
 phe := GetHostByName(buffer);
 if phe = nil then Exit;

 p := PaPInAddr(phe^.h_addr_list);
 if p^[1] <> nil then Result := inet_ntoa(p^[1]^);
end;

function e_Net_HostToIP(Host: PChar): PChar;
type
 TaPInAddr = array [0..10] of PInAddr;
 PaPInAddr = ^TaPInAddr;

var
  phe: PHostEnt;
  p: PaPInAddr;
begin
 Result := nil;

 phe := gethostbyname(Host);
 if phe = nil then Exit;

 p := PaPInAddr(phe^.h_addr_list);
 if p^[0] <> nil then Result := inet_ntoa(p^[0]^);
end;

function e_Net_GetHost(): PChar;
var
  phe: PHostEnt;
  Buffer: array[0..63] of Char;
begin
 Result := '';

 GetHostName(Buffer, SizeOf(Buffer));
 phe := GetHostByName(buffer);
 if phe = nil then Exit;

 Result := phe^.h_name;
end;

function e_Net_Write(Buf: pointer; Count: Integer): Boolean;
var
 i: integer;
begin
 Result := False;

 if (not NetReady) or (NetSocket <= 0) then Exit;
 if Count <= 0 then Exit;

 if NetBufLen + Count < MaxBufLen then
 begin
  for i := NetBufLen to NetBufLen+Count-1 do
   NetBuf[i] := PByteArray(buf)[i-NetBufLen];

  NetBufLen := NetBufLen+Count;
  Result := True;
 end;
end;

function e_Net_Recv(Buf: Pointer; Count: Integer; var IP: PChar; var Port: Integer;
                    var RecvBytes: Integer): Integer;
var
 from: sockaddr_in;
 i: Integer;
 UID: DWORD;
 s: string;
begin
 Result := 0;

 if (not NetReady) or (NetSocket <= 0) then Exit;
 if (Count <= 0) or (Count > MaxBufLen) then Exit;
 i := SizeOf(from);
 Result := recvfrom(NetSocket, NettmpBuf[0], Count, 0, from, i);
 if Result <= 0 then
 begin
  Result := -1;
  RecvBytes := Result;
  Exit;
 end;

 dec(Result);
 if Result > 0 then
 begin
  IP := inet_ntoa(from.sin_addr);
  Port := ntohs(from.sin_port);

  case NettmpBuf[0] of
   0: Move(NettmpBuf[1], PByteArray(buf)[0], Result);
   1: begin
       Move(NettmpBuf[1], UID, 2);
       s := StrPas(IP);
       for i := 0 to Length(NetAPLs)-1 do
        if (NetAPLs[i].UID  = UID) and
           (NetAPLs[i].Port = Port) and
           (NetAPLs[i].IP   = s) then
        begin
         NetAPLs[i].trys := 255;
         Break;
        end;

       Result := e_Net_Recv(Buf, Count, IP, Port, RecvBytes);
      end;
   2: begin
       Result := Result-2;

       Move(NettmpBuf[1], UID, 2);

       NettmpBuf[0] := 1;
       i := SendTo(NetSocket, NettmpBuf[0], 3, 0, from, SizeOf(from));

       for i := 0 to APLreglen-1 do
        if (APLreg[i].UID = UID) and
           (APLreg[i].Port = Port) and
           (APLreg[i].IP = StrPas(IP)) then
        begin
         Result := e_Net_Recv(Buf, Count, IP, Port, RecvBytes);
         Exit;
        end;

       Move(NettmpBuf[3], PByteArray(Buf)[0], Result);

       APLreg[regseek].UID  := UID;
       APLreg[regseek].Port := Port;
       APLreg[regseek].IP   := StrPas(IP);

       regseek := regseek+1;
       if regseek >= APLreglen then regseek := 0;
      end;
  end;
 end;

 RecvBytes := Result;
end;

function e_Net_Send(IP: PChar; Port: WORD; APL: Boolean): Integer;
var
 address: sockaddr_in;
 APLpacket: PAPLpacket;
begin
 Result := 0;

 if (not NetReady) or (NetSocket <= 0) then Exit;

 address.sin_family := AF_INET;
 address.sin_port := htons(Port);
 if IP <> nil then address.sin_addr.S_addr := inet_addr(IP)
  else address.sin_addr.S_addr := INADDR_BROADCAST;
 FillChar(address.sin_zero, SizeOf(address.sin_zero), 0);

 if (not APL) or (IP = nil) then
 begin
  NetBuf[0] := 0;
  Result := SendTo(NetSocket, NetBuf[0], NetBufLen, 0, address, SizeOf(address));
 end
  else
 begin
  NetAPLUID := NetAPLUID+1;
  if NetAPLUID < 5 then NetAPLUID := 5;

  New(APLpacket);
  APLpacket^.trys := 1;
  APLpacket^.UID  := NetAPLUID;
  APLpacket^.Time := GetTickCount;
  APLpacket^.Size := NetBufLen;
  APLpacket^.Port := Port;
  APLpacket^.IP   := StrPas(IP);
  GetMem(APLpacket^.Data, NetBufLen+2);

  APLpacket^.Data[0] := 2;
  Move(APLpacket^.UID, APLpacket^.Data[1], 2);
  Move(NetBuf[1], APLpacket^.Data[3], NetBufLen-1);

  SetLength(NetAPLs, Length(NetAPLs)+1);
  NetAPLs[Length(NetAPLs)-1] := APLpacket;
  Result := SendTo(NetSocket, APLpacket^.Data[0], NetBufLen+2, 0, address, SizeOf(address));
 end;
end;

procedure e_Net_Update();
var
 i, j: Integer;
 t: DWORD;
 address: sockaddr_in;
begin
 if not NetReady then Exit;

 t := GetTickCount;
 for i := 0 to Length(NetAPLs) - 1 do
 begin
  if t-NetAPLs[i]^.Time > NetAPLtime then
  with NetAPLs[i]^ do
  begin
   address.sin_family := AF_INET;
   address.sin_port := htons(Port);
   address.sin_addr.S_addr := inet_addr(PChar(IP));
   FillChar(address.sin_zero, SizeOf(address.sin_zero), 0);
   trys := trys+1;
   Time := t;
   SendTo(NetSocket, Data^, Size, 0, address, SizeOf(address));
  end;

  if NetAPLs[i]^.trys >= Nettrys then
  begin
   FreeMem(NetAPLs[i]^.Data);
   Dispose(NetAPLs[i]);
   NetAPLs[i] := nil;
  end
 end;

 i := 0;
 while i < Length(NetAPLs) do
  if NetAPLs[i] = nil then
  begin
   for j := i to Length(NetAPLs)-2 do
    NetAPLs[j] := NetAPLs[j+1];
   SetLength(NetAPLs, Length(NetAPLs)-1);
  end else i := i+1;
end;

end.

