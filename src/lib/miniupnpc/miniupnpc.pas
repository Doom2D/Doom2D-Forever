{.$DEFINE LIBMINIUPNPC_WINDOZE_STATIC}

{$MODE OBJFPC}
{$PACKRECORDS C}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$LONGSTRINGS ON}
{$MACRO ON}

{$Z4} // Force four-byte enums

unit miniupnpc;

interface

{$IFDEF MSWINDOWS}
  {$IFDEF LIBMINIUPNPC_WINDOZE_STATIC}
    {$LINKLIB libminiupnpc.a}
    {$LINKLIB libiphlpapi.a}
    {$DEFINE MINIUPNPC_IMPL := cdecl; external}
  {$ELSE}
    {$DEFINE MINIUPNPC_IMPL := cdecl; external 'miniupnpc.dll'}
  {$ENDIF}
{$ELSE}
  {$DEFINE MINIUPNPC_IMPL := cdecl; external 'miniupnpc'}
{$ENDIF}

const MINIUPNPC_URL_MAXSIZE=128;
Type 
  PUPNPDev = ^TUPNPDev;
  TUPNPDev = record
    pNext:PUPNPDev;
    descURL:pchar;
    st:pchar;
	scope_id:word;
    buffer:array[0..1] of byte;
	end;
	
  PUPNPUrls = ^TUPNPUrls;
  TUPNPUrls = record
    controlURL:pchar;
    ipcondescURL:pchar;
    controlURL_CIF:pchar;
	controlURL_6FC:pchar;
	rootdescURL:pchar;
	end;
	TUrlStr = array [1..MINIUPNPC_URL_MAXSIZE] of char;
   TIGDdatas_service  = record
		controlurl : TUrlStr;
		eventsuburl: TUrlStr;
		scpdurl: TUrlStr;
		servicetype: TUrlStr;
		//char devicetype[MINIUPNPC_URL_MAXSIZE];
	end;
  
  PIGDdatas = ^TIGDdatas;
  TIGDdatas = record
		cureltname: TUrlStr;
		urlbase: TUrlStr;
		presentationurl : TUrlStr;
		level:integer;
		//int state;
		//"urn:schemas-upnp-org:service:WANCommonInterfaceConfig:1" 
		CIF : TIGDdatas_service;
		// "urn:schemas-upnp-org:service:WANIPConnection:1"
		// "urn:schemas-upnp-org:service:WANPPPConnection:1"
		first: TIGDdatas_service;
		//if both WANIPConnection and WANPPPConnection are present
		second: TIGDdatas_service;
		//"urn:schemas-upnp-org:service:WANIPv6FirewallControl:1"
		IPv6FC : TIGDdatas_service;
		// tmp
		tmp: TIGDdatas_service;
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
 * If sameport is not null, SSDP packets will be sent from the source port
 * 1900 (same as destination port) otherwise system assign a source port. *)
function upnpDiscover(
      delay:integer;
      multicastif:pchar;
      minissdpdsock:pchar;
      sameport:integer;
	  IPV6:integer;
	  error:pinteger):PUPNPDev; MINIUPNPC_IMPL;


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
function UPNP_GetValidIGD(
				devlist:PUPNPDev; 
				urls:PUPNPUrls; 
				data:PIGDdatas; 
				lanaddr:pchar; 
				lanaddrlen:integer):integer; MINIUPNPC_IMPL;


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
 * 501 Action Failed - See UPnP Device Architecture section on Control. *)
function UPNP_GetExternalIPAddress(
		controlURL:pchar; 
		servicetype:pchar; 
		extIpAdd:pchar):integer; MINIUPNPC_IMPL;


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
 *                                        cannot be a specific port value *)

function UPNP_AddPortMapping(
					controlURL:pchar; 
					servicetype:pchar; 
					extPort:pchar; 
					inPort:pchar; 
					inClient:pchar; 
					desc:pchar; 
					proto:pchar; 
					remoteHost:pchar;
					leaseDuration:pchar):integer; MINIUPNPC_IMPL;

(* UPNP_DeletePortMapping()
 * Use same argument values as what was used for AddPortMapping().
 * remoteHost is usually NULL because IGD don't support it.
 * Return Values :
 * 0 : SUCCESS
 * NON ZERO : error. Either an UPnP error code or an undefined error.
 *
 * List of possible UPnP errors for DeletePortMapping :
 * 402 Invalid Args - See UPnP Device Architecture section on Control.
 * 714 NoSuchEntryInArray - The specified value does not exist in the array *)

function UPNP_DeletePortMapping(
								controlURL:pchar; 
								servicetype:pchar; 
								extPort:pchar; 
								proto:pchar; 
								remoteHost:pchar):integer; MINIUPNPC_IMPL;



function UPNP_GetGenericPortMappingEntry(
               const controlURL :pchar;
               const servicetype:pchar;
							 index:pchar;
							 extPort:pchar;
							 intClient:pchar;
							 intPort:pchar;
							 protocol:pchar;
							 desc:pchar;
							 enabled:pchar;
							 rHost:pchar;
							 duration:pchar):integer; MINIUPNPC_IMPL;

procedure FreeUPNPUrls(urls: PUPNPUrls); MINIUPNPC_IMPL;
procedure freeUPNPDevlist(devl: PUPNPDev); MINIUPNPC_IMPL;

implementation

end.
