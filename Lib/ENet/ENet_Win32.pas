unit ENet_Win32;

{
  ENet - Reliable UDP networking library

  Delphi 7 DLL header: ENet_Win32.pas
  Copyright (c) 2014-2015 Dmitry D. Chernov aka Black Doomer

  Original file: win32.h
  Copyright (c) 2002-2014 Lee Salzman

  Version 1 for 1.3.12: 16.08.2014
  Version 2 for 1.3.12: 10.02.2015

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
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
function ENET_HOST_TO_NET_16( const value: u_short ): u_short; // inline;
function ENET_HOST_TO_NET_32( const value: u_long ): u_long; // inline;

function ENET_NET_TO_HOST_16( const value: u_short ): u_short; // inline;
function ENET_NET_TO_HOST_32( const value: u_long ): u_long; // inline;

procedure ENET_SOCKETSET_EMPTY( var sockset: ENetSocketSet ); // inline;
procedure ENET_SOCKETSET_ADD( var sockset: ENetSocketSet; socket: ENetSocket ); // inline;
procedure ENET_SOCKETSET_REMOVE( var sockset: ENetSocketSet; socket: ENetSocket ); // inline;
function  ENET_SOCKETSET_CHECK( var sockset: ENetSocketSet; socket: ENetSocket ): Boolean; // inline;

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
