unit ENet_Win32;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: win32.h
  16.08.2014
}

interface

uses
  IdWinSock2, ENet_Types; //ENet_Types used only for size_t

const
  libraryENet = 'enet.dll';

  ENET_SOCKET_NULL = INVALID_SOCKET;

type
  ENetSocket = TSocket;

  ENetSocketSet = TFDSet;
  pENetSocketSet = ^ENetSocketSet;

  pENetBuffer = ^ENetBuffer;
  ENetBuffer = record
    dataLength : enet_size_t;
    data       : Pointer;
  end;

//inline macros
function ENET_HOST_TO_NET_16( value: u_short ): u_short; // inline;
function ENET_HOST_TO_NET_32( value: u_long ): u_long; // inline;

function ENET_NET_TO_HOST_16( value: u_short ): u_short; // inline;
function ENET_NET_TO_HOST_32( value: u_long ): u_long; // inline;

procedure ENET_SOCKETSET_EMPTY( sockset: ENetSocketSet ); // inline;
procedure ENET_SOCKETSET_ADD( sockset: ENetSocketSet; socket: ENetSocket ); // inline;
procedure ENET_SOCKETSET_REMOVE( sockset: ENetSocketSet; socket: ENetSocket ); // inline;
function  ENET_SOCKETSET_CHECK( sockset: ENetSocketSet; socket: ENetSocket ): Boolean; // inline;

implementation

function ENET_HOST_TO_NET_16;
   begin Result := htons(value);
     end;
function ENET_HOST_TO_NET_32;
   begin Result := htonl(value);
     end;

function ENET_NET_TO_HOST_16;
   begin Result := ntohs(value);
     end;
function ENET_NET_TO_HOST_32;
   begin Result := ntohl(value);
     end;

procedure ENET_SOCKETSET_EMPTY;
    begin FD_ZERO( sockset );
      end;
procedure ENET_SOCKETSET_ADD;
    begin FD_SET( socket, sockset );
      end;
procedure ENET_SOCKETSET_REMOVE;
    begin FD_CLR( socket, sockset );
      end;

function ENET_SOCKETSET_CHECK;
   begin Result := FD_ISSET( socket, sockset );
     end;

end.
