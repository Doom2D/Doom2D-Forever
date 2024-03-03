{$MODE OBJFPC}
{$LONGSTRINGS ON}
{$MACRO ON}

{$PACKRECORDS C}
{$PACKENUM 4}

unit MiniUPnPc;

{
  MiniUPnP - UPnP IGD client lightweight library
  Copyright (c) 2005-2024 Thomas Bernard
  All rights reserved.

  Shared (dynamic) library binding unit for Free Pascal
  Version 1 for 2.3.5: 2024-03-04
  Copyright (c) 2024 Dmitry D. Chernov aka BlackDoomer
  Based on Version 0 (2018-02-06) by Dmitry V. Merkulov aka fgsfds aka PrimuS.

  http://miniupnp.free.fr
  https://miniupnp.tuxfamily.org
  https://github.com/miniupnp/miniupnp

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

interface

uses
  ctypes;

////////////////////////////////////////////////////////////////////////////////////////////////////
// miniupnpc_declspec.h
////////////////////////////////////////////////////////////////////////////////////////////////////

{$IFNDEF MINIUPNP_LIBSPEC}
  {$IFDEF WINDOWS}
    {$IFDEF MINIUPNP_STATICLIB}
      {$LINKLIB libminiupnpc.a}
      {$LINKLIB libiphlpapi.a}
      {$DEFINE MINIUPNP_LIBSPEC := cdecl; external}
    {$ELSE}
      {$DEFINE MINIUPNP_LIBSPEC := cdecl; external 'miniupnpc.dll'}
    {$ENDIF}
  {$ELSE}
    {$LINKLIB libminiupnpc}
    {$DEFINE MINIUPNP_LIBSPEC := cdecl; external 'libminiupnpc'}
  {$ENDIF}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////////////////////////
// miniupnpctypes.h
////////////////////////////////////////////////////////////////////////////////////////////////////

type
  UNSIGNED_INTEGER = {$IF DECLARED(cuint64)}cuint64{$ELSE}cuint{$ENDIF};

////////////////////////////////////////////////////////////////////////////////////////////////////
// portlistingparse.h
////////////////////////////////////////////////////////////////////////////////////////////////////

(* sample of PortMappingEntry :
  <p:PortMappingEntry>
    <p:NewRemoteHost>202.233.2.1</p:NewRemoteHost>
    <p:NewExternalPort>2345</p:NewExternalPort>
    <p:NewProtocol>TCP</p:NewProtocol>
    <p:NewInternalPort>2345</p:NewInternalPort>
    <p:NewInternalClient>192.168.1.137</p:NewInternalClient>
    <p:NewEnabled>1</p:NewEnabled>
    <p:NewDescription>dooom</p:NewDescription>
    <p:NewLeaseTime>345</p:NewLeaseTime>
  </p:PortMappingEntry>
 *)

type
  portMappingElt = (PortMappingEltNone, PortMappingEntry, NewRemoteHost, NewExternalPort,
    NewProtocol, NewInternalPort, NewInternalClient, NewEnabled, NewDescription, NewLeaseTime);

  pTPortMapping = ^TPortMapping;
  TPortMapping = record
    l_next: pTPortMapping;  // list next element
    leaseTime: UNSIGNED_INTEGER;
    externalPort: cushort;
    internalPort: cushort;
    remoteHost: array[0..63] of AnsiChar;
    internalClient: array[0..63] of AnsiChar;
    description: array[0..63] of AnsiChar;
    protocol: array[0..3] of AnsiChar;
    enabled: cuchar;
  end;

  pTPortMappingParserData = ^TPortMappingParserData;
  TPortMappingParserData = record
    l_head: pTPortMapping;  // list head
    curelt: portMappingElt;
  end;

procedure ParsePortListing(const buffer: PAnsiChar; bufsize: cint;
  pdata: pTPortMappingParserData); MINIUPNP_LIBSPEC;

procedure FreePortListing(pdata: pTPortMappingParserData); MINIUPNP_LIBSPEC;

////////////////////////////////////////////////////////////////////////////////////////////////////
// igd_desc_parse.h
////////////////////////////////////////////////////////////////////////////////////////////////////

const
  MINIUPNPC_URL_MAXSIZE = 128;

type
  // Structure to store the result of the parsing of UPnP descriptions of Internet Gateway Devices
  pTIGDdatas_service = ^TIGDdatas_service;
  TIGDdatas_service = record
    controlurl: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    eventsuburl: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    scpdurl: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    servicetype: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    //devicetype: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
  end;

  pIGDdatas = ^TIGDdatas;
  TIGDdatas = record
    cureltname: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    urlbase: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    presentationurl: array[0..MINIUPNPC_URL_MAXSIZE-1] of AnsiChar;
    level: cint;
    //state: cint;
    (* "urn:schemas-upnp-org:service:WANCommonInterfaceConfig:1" *)
    CIF: TIGDdatas_service;
    (* "urn:schemas-upnp-org:service:WANIPConnection:1"
     * "urn:schemas-upnp-org:service:WANPPPConnection:1" *)
    first: TIGDdatas_service;
    (* if both WANIPConnection and WANPPPConnection are present *)
    second: TIGDdatas_service;
    (* "urn:schemas-upnp-org:service:WANIPv6FirewallControl:1" *)
    IPv6FC: TIGDdatas_service;
    (* tmp *)
    tmp: TIGDdatas_service;
  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// upnpdev.h
////////////////////////////////////////////////////////////////////////////////////////////////////

  pUPNPDev = ^TUPNPDev;
  TUPNPDev = record
    pNext: pUPNPDev;
    descURL: PAnsiChar;
    st: PAnsiChar;
    scope_id: cuint;
    buffer: array[0..1] of cchar;  // C99 flexible array member - Fallback to a hack
  end;

procedure freeUPNPDevlist(devlist: pUPNPDev); MINIUPNP_LIBSPEC;

////////////////////////////////////////////////////////////////////////////////////////////////////
// upnpcommands.h
////////////////////////////////////////////////////////////////////////////////////////////////////

const
  (* MiniUPnPc return codes : *)
  UPNPCOMMAND_SUCCESS = 0;
  UPNPCOMMAND_UNKNOWN_ERROR = -1;
  UPNPCOMMAND_INVALID_ARGS = -2;
  UPNPCOMMAND_HTTP_ERROR = -3;
  UPNPCOMMAND_INVALID_RESPONSE = -4;
  UPNPCOMMAND_MEM_ALLOC_ERROR = -5;

function UPNP_GetTotalBytesSent(const controlURL: PAnsiChar;
  const servicetype: PAnsiChar): UNSIGNED_INTEGER; MINIUPNP_LIBSPEC;

function UPNP_GetTotalBytesReceived(const controlURL: PAnsiChar;
  const servicetype: PAnsiChar): UNSIGNED_INTEGER; MINIUPNP_LIBSPEC;

function UPNP_GetTotalPacketsSent(const controlURL: PAnsiChar;
  const servicetype: PAnsiChar): UNSIGNED_INTEGER; MINIUPNP_LIBSPEC;

function UPNP_GetTotalPacketsReceived(const controlURL: PAnsiChar;
  const servicetype: PAnsiChar): UNSIGNED_INTEGER; MINIUPNP_LIBSPEC;

(* UPNP_GetStatusInfo()
 * status and lastconnerror are 64 byte buffers
 * Return values :
 * UPNPCOMMAND_SUCCESS, UPNPCOMMAND_INVALID_ARGS, UPNPCOMMAND_UNKNOWN_ERROR
 * or a UPnP Error code
 *)
function UPNP_GetStatusInfo(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  status: PAnsiChar; uptime: pcuint; lastconnerror: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetConnectionTypeInfo()
 * argument connectionType is a 64 character buffer
 * Return Values :
 * UPNPCOMMAND_SUCCESS, UPNPCOMMAND_INVALID_ARGS, UPNPCOMMAND_UNKNOWN_ERROR
 * or a UPnP Error code
 *)
function UPNP_GetConnectionTypeInfo(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  connectionType: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetExternalIPAddress() call the corresponding UPNP method.
 * if the third arg is not null the value is copied to it.
 * at least 16 bytes must be available
 *
 * Return values :
 * 0 : SUCCESS
 * NON ZERO : ERROR Either an UPnP error code or an unknown error.
 *
 * possible UPnP Errors :
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 501 Action Failed - See UPnP Device Architecture section on Control.
 *)
function UPNP_GetExternalIPAddress(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  extIpAdd: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetLinkLayerMaxBitRates()
 * call WANCommonInterfaceConfig:1#GetCommonLinkProperties
 *
 * return values :
 * UPNPCOMMAND_SUCCESS, UPNPCOMMAND_INVALID_ARGS, UPNPCOMMAND_UNKNOWN_ERROR
 * or a UPnP Error Code.
 *)
function UPNP_GetLinkLayerMaxBitRates(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  bitrateDown: pcuint; bitrateUp: pcuint): cint; MINIUPNP_LIBSPEC;

(* UPNP_AddPortMapping()
 * if desc is NULL, it will be defaulted to "libminiupnpc"
 * remoteHost is usually NULL because IGD don't support it.
 *
 * Return values :
 * 0 : SUCCESS
 * NON ZERO : ERROR. Either an UPnP error code or an unknown error.
 *
 * List of possible UPnP errors for AddPortMapping :
 * errorCode errorDescription (short) - Description (long)
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 501 Action Failed - See UPnP Device Architecture section on Control.
 * 606 Action not authorized - The action requested REQUIRES authorization and
 *                             the sender was not authorized.
 * 715 WildCardNotPermittedInSrcIP - The source IP address cannot be
 *                                   wild-carded
 * 716 WildCardNotPermittedInExtPort - The external port cannot be wild-carded
 * 718 ConflictInMappingEntry - The port mapping entry specified conflicts
 *                     with a mapping assigned previously to another client
 * 724 SamePortValuesRequired - Internal and External port values
 *                              must be the same
 * 725 OnlyPermanentLeasesSupported - The NAT implementation only supports
 *                  permanent lease times on port mappings
 * 726 RemoteHostOnlySupportsWildcard - RemoteHost must be a wildcard
 *                             and cannot be a specific IP address or DNS name
 * 727 ExternalPortOnlySupportsWildcard - ExternalPort must be a wildcard and
 *                                        cannot be a specific port value
 * 728 NoPortMapsAvailable - There are not enough free ports available to
 *                           complete port mapping.
 * 729 ConflictWithOtherMechanisms - Attempted port mapping is not allowed
 *                                   due to conflict with other mechanisms.
 * 732 WildCardNotPermittedInIntPort - The internal port cannot be wild-carded
 *)
function UPNP_AddPortMapping(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const extPort: PAnsiChar; const inPort: PAnsiChar; const inClient: PAnsiChar;
  const desc: PAnsiChar; const proto: PAnsiChar; const remoteHost: PAnsiChar;
  const leaseDuration: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_AddAnyPortMapping()
 * if desc is NULL, it will be defaulted to "libminiupnpc"
 * remoteHost is usually NULL because IGD don't support it.
 *
 * Return values :
 * 0 : SUCCESS
 * NON ZERO : ERROR. Either an UPnP error code or an unknown error.
 *
 * List of possible UPnP errors for AddPortMapping :
 * errorCode errorDescription (short) - Description (long)
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 501 Action Failed - See UPnP Device Architecture section on Control.
 * 606 Action not authorized - The action requested REQUIRES authorization and
 *                             the sender was not authorized.
 * 715 WildCardNotPermittedInSrcIP - The source IP address cannot be
 *                                   wild-carded
 * 716 WildCardNotPermittedInExtPort - The external port cannot be wild-carded
 * 728 NoPortMapsAvailable - There are not enough free ports available to
 *                           complete port mapping.
 * 729 ConflictWithOtherMechanisms - Attempted port mapping is not allowed
 *                                   due to conflict with other mechanisms.
 * 732 WildCardNotPermittedInIntPort - The internal port cannot be wild-carded
 *)
function UPNP_AddAnyPortMapping(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const extPort: PAnsiChar; const inPort: PAnsiChar; const inClient: PAnsiChar;
  const desc: PAnsiChar; const proto: PAnsiChar; const remoteHost: PAnsiChar;
  const leaseDuration: PAnsiChar; reservedPort: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_DeletePortMapping()
 * Use same argument values as what was used for AddPortMapping().
 * remoteHost is usually NULL because IGD don't support it.
 * Return Values :
 * 0 : SUCCESS
 * NON ZERO : error. Either an UPnP error code or an undefined error.
 *
 * List of possible UPnP errors for DeletePortMapping :
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 606 Action not authorized - The action requested REQUIRES authorization
 *                             and the sender was not authorized.
 * 714 NoSuchEntryInArray - The specified value does not exist in the array
 *)
function UPNP_DeletePortMapping(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const extPort: PAnsiChar; const proto: PAnsiChar;
  const remoteHost: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_DeletePortRangeMapping()
 * Use same argument values as what was used for AddPortMapping().
 * remoteHost is usually NULL because IGD don't support it.
 * Return Values :
 * 0 : SUCCESS
 * NON ZERO : error. Either an UPnP error code or an undefined error.
 *
 * List of possible UPnP errors for DeletePortMapping :
 * 606 Action not authorized - The action requested REQUIRES authorization
 *                             and the sender was not authorized.
 * 730 PortMappingNotFound - This error message is returned if no port
 *                           mapping is found in the specified range.
 * 733 InconsistentParameters - NewStartPort and NewEndPort values are not consistent.
 *)
function UPNP_DeletePortMappingRange(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const extPortStart: PAnsiChar; const extPortEnd: PAnsiChar; const proto: PAnsiChar;
  const manage: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetPortMappingNumberOfEntries()
 * not supported by all routers
 *)
function UPNP_GetPortMappingNumberOfEntries(const controlURL: PAnsiChar;
  const servicetype: PAnsiChar; numEntries: pcuint): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetSpecificPortMappingEntry()
 *    retrieves an existing port mapping
 * params :
 *  in   extPort
 *  in   proto
 *  in   remoteHost
 *  out  intClient (16 bytes)
 *  out  intPort (6 bytes)
 *  out  desc (80 bytes)
 *  out  enabled (4 bytes)
 *  out  leaseDuration (16 bytes)
 *
 * return value :
 * UPNPCOMMAND_SUCCESS, UPNPCOMMAND_INVALID_ARGS, UPNPCOMMAND_UNKNOWN_ERROR
 * or a UPnP Error Code.
 *
 * List of possible UPnP errors for _GetSpecificPortMappingEntry :
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 501 Action Failed - See UPnP Device Architecture section on Control.
 * 606 Action not authorized - The action requested REQUIRES authorization
 *                             and the sender was not authorized.
 * 714 NoSuchEntryInArray - The specified value does not exist in the array.
 *)
function UPNP_GetSpecificPortMappingEntry(const controlURL: PAnsiChar;
  const servicetype: PAnsiChar; const extPort: PAnsiChar; const proto: PAnsiChar;
  const remoteHost: PAnsiChar; intClient: PAnsiChar; intPort: PAnsiChar; desc: PAnsiChar;
  enabled: PAnsiChar; leaseDuration: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetGenericPortMappingEntry()
 * params :
 *  in   index
 *  out  extPort (6 bytes)
 *  out  intClient (16 bytes)
 *  out  intPort (6 bytes)
 *  out  protocol (4 bytes)
 *  out  desc (80 bytes)
 *  out  enabled (4 bytes)
 *  out  rHost (64 bytes)
 *  out  duration (16 bytes)
 *
 * return value :
 * UPNPCOMMAND_SUCCESS, UPNPCOMMAND_INVALID_ARGS, UPNPCOMMAND_UNKNOWN_ERROR
 * or a UPnP Error Code.
 *
 * Possible UPNP Error codes :
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 606 Action not authorized - The action requested REQUIRES authorization
 *                             and the sender was not authorized.
 * 713 SpecifiedArrayIndexInvalid - The specified array index is out of bounds
 *)
function UPNP_GetGenericPortMappingEntry(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const index: PAnsiChar; extPort: PAnsiChar; intClient: PAnsiChar; intPort: PAnsiChar;
  protocol: PAnsiChar; desc: PAnsiChar; enabled: PAnsiChar; rHost: PAnsiChar;
  duration: PAnsiChar): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetListOfPortMappings()      Available in IGD v2
 *
 *
 * Possible UPNP Error codes :
 * 606 Action not Authorized
 * 730 PortMappingNotFound - no port mapping is found in the specified range.
 * 733 InconsistantParameters - NewStartPort and NewEndPort values are not
 *                              consistent.
 *)
function UPNP_GetListOfPortMappings(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const startPort: PAnsiChar; const endPort: PAnsiChar; const protocol: PAnsiChar;
  const numberOfPorts: PAnsiChar; data: pTPortMappingParserData): cint; MINIUPNP_LIBSPEC;

(* IGD:2, functions for service WANIPv6FirewallControl:1 *)

function UPNP_GetFirewallStatus(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  firewallEnabled: pcint; inboundPinholeAllowed: pcint): cint; MINIUPNP_LIBSPEC;

function UPNP_GetOutboundPinholeTimeout(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const remoteHost: PAnsiChar; const remotePort: PAnsiChar; const intClient: PAnsiChar;
  const intPort: PAnsiChar; const proto: PAnsiChar; opTimeout: pcint): cint; MINIUPNP_LIBSPEC;

function UPNP_AddPinhole(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const remoteHost: PAnsiChar; const remotePort: PAnsiChar; const intClient: PAnsiChar;
  const intPort: PAnsiChar; const proto: PAnsiChar; const leaseTime: PAnsiChar;
  uniqueID: PAnsiChar): cint; MINIUPNP_LIBSPEC;

function UPNP_UpdatePinhole(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const uniqueID: PAnsiChar; const leaseTime: PAnsiChar): cint; MINIUPNP_LIBSPEC;

function UPNP_DeletePinhole(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const uniqueID: PAnsiChar): cint; MINIUPNP_LIBSPEC;

function UPNP_CheckPinholeWorking(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const uniqueID: PAnsiChar; isWorking: pcint): cint; MINIUPNP_LIBSPEC;

function UPNP_GetPinholePackets(const controlURL: PAnsiChar; const servicetype: PAnsiChar;
  const uniqueID: PAnsiChar; packets: pcint): cint; MINIUPNP_LIBSPEC;

////////////////////////////////////////////////////////////////////////////////////////////////////
// upnperrors.h
////////////////////////////////////////////////////////////////////////////////////////////////////

(* strupnperror()
 * Return a string description of the UPnP error code
 * or NULL for undefinded errors
 *)
function strupnperror(err: cint): PAnsiChar; MINIUPNP_LIBSPEC;

////////////////////////////////////////////////////////////////////////////////////////////////////
// miniupnpc.h
////////////////////////////////////////////////////////////////////////////////////////////////////

const
  { error codes : }
  UPNPDISCOVER_SUCCESS = 0;
  UPNPDISCOVER_UNKNOWN_ERROR = -1;
  UPNPDISCOVER_SOCKET_ERROR = -101;
  UPNPDISCOVER_MEMORY_ERROR = -102;

  { versions : }
  MINIUPNPC_VERSION = '2.3.5'; //'2.2.6';
  MINIUPNPC_API_VERSION = 17;

  { Source port:
    Using "1" as an alias for 1900 for backwards compatibility
    (presuming one would have used that for the "sameport" parameter) }
  UPNP_LOCAL_PORT_ANY = 0;
  UPNP_LOCAL_PORT_SAME = 1;

type
  // 'struct UPNParg' is used only by simpleUPnPcommand(), which is not exported, and thus omitted.

  (* structure used to get fast access to urls
   * controlURL: controlURL of the WANIPConnection
   * ipcondescURL: url of the description of the WANIPConnection
   * controlURL_CIF: controlURL of the WANCommonInterfaceConfig
   * controlURL_6FC: controlURL of the WANIPv6FirewallControl
   *)
  pUPNPUrls = ^TUPNPUrls;
  TUPNPUrls = record
    controlURL: PAnsiChar;
    ipcondescURL: PAnsiChar;
    controlURL_CIF: PAnsiChar;
    controlURL_6FC: PAnsiChar;
    rootdescURL: PAnsiChar;
  end;

(* upnpDiscover()
 * discover UPnP devices on the network.
 * The discovered devices are returned as a chained list.
 * It is up to the caller to free the list with freeUPNPDevlist().
 * delay (in millisecond) is the maximum time for waiting any device
 * response.
 * If available, device list will be obtained from MiniSSDPd.
 * Default path for minissdpd socket will be used if minissdpdsock argument
 * is NULL.
 * If multicastif is not NULL, it will be used instead of the default
 * multicast interface for sending SSDP discover packets.
 * If localport is set to UPNP_LOCAL_PORT_SAME(1) SSDP packets will be sent
 * from the source port 1900 (same as destination port), if set to
 * UPNP_LOCAL_PORT_ANY(0) system assign a source port, any other value will
 * be attempted as the source port.
 * "searchalltypes" parameter is useful when searching several types,
 * if 0, the discovery will stop with the first type returning results.
 * TTL should default to 2.
 *)
function upnpDiscover(delay: cint; const multicastif: PAnsiChar; const minissdpdsock: PAnsiChar;
  localport: cint; ipV6: cint; ttl: cuchar; error: pcint): pUPNPDev; MINIUPNP_LIBSPEC;
function upnpDiscoverAll(delay: cint; const multicastif: PAnsiChar; const minissdpdsock: PAnsiChar;
  localport: cint; ipv6: cint; ttl: cuchar; error: pcint): pUPNPDev; MINIUPNP_LIBSPEC;
function upnpDiscoverDevice(const device: PAnsiChar; delay: cint; const multicastif: PAnsiChar;
  const minissdpdsock: PAnsiChar; localport: cint; ipv6: cint; ttl: cuchar;
  error: pcint): pUPNPDev; MINIUPNP_LIBSPEC;
function upnpDiscoverDevices(const deviceTypes: PPAnsiChar; delay: cint;
  const multicastif: PAnsiChar; const minissdpdsock: PAnsiChar; localport: cint; ipv6: cint;
  ttl: cuchar; error: pcint; searchalltypes: cint): pUPNPDev; MINIUPNP_LIBSPEC;

(* parserootdesc() :
 * parse root XML description of a UPnP device and fill the IGDdatas
 * structure.
 *)
procedure parserootdesc(const buffer: PAnsiChar; bufsize: cint; data: pIGDdatas); MINIUPNP_LIBSPEC;

(* UPNP_GetValidIGD() :
 * return values :
 *     0 = NO IGD found
 *     1 = A valid connected IGD has been found
 *     2 = A valid IGD has been found but it reported as
 *         not connected
 *     3 = an UPnP device has been found but was not recognized as an IGD
 *
 * In any non zero return case, the urls and data structures
 * passed as parameters are set. Donc forget to call FreeUPNPUrls(urls) to
 * free allocated memory.
 *)
function UPNP_GetValidIGD(devlist: pUPNPDev; urls: pUPNPUrls; data: pIGDdatas; lanaddr: PAnsiChar;
  lanaddrlen: cint): cint; MINIUPNP_LIBSPEC;

(* UPNP_GetIGDFromUrl()
 * Used when skipping the discovery process.
 * When succeding, urls, data, and lanaddr arguments are set.
 * return value :
 *   0 - Not ok
 *   1 - OK
 *)
function UPNP_GetIGDFromUrl(const rootdescurl: PAnsiChar; urls: pUPNPUrls; data: pIGDdatas;
  lanaddr: PAnsiChar; lanaddrlen: cint): cint; MINIUPNP_LIBSPEC;

procedure GetUPNPUrls(urls: pUPNPUrls; data: pIGDdatas; const descURL: PAnsiChar;
  scope_id: cuint); MINIUPNP_LIBSPEC;

procedure FreeUPNPUrls(urls: pUPNPUrls); MINIUPNP_LIBSPEC;

(* return 0 or 1 *)
function UPNPIGD_IsConnected(urls: pUPNPUrls; data: pIGDdatas): cint; MINIUPNP_LIBSPEC;

////////////////////////////////////////////////////////////////////////////////////////////////////
// miniwget.h
////////////////////////////////////////////////////////////////////////////////////////////////////

function miniwget(const url: PAnsiChar; size: pcint; scope_id: cuint;
  status_code: pcint): Pointer; MINIUPNP_LIBSPEC;

function miniwget_getaddr(const url: PAnsiChar; size: pcint; addr: PAnsiChar; addrlen: cint;
  scope_id: cuint; status_code: pcint): Pointer; MINIUPNP_LIBSPEC;

////////////////////////////////////////////////////////////////////////////////////////////////////
implementation

end.
