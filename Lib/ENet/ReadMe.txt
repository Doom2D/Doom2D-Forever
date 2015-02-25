ENet - Reliable UDP networking library
DLL include files (headers) for Delphi 7
Version 1 for 1.3.12: 16.08.2014
Version 2 for 1.3.12: 10.02.2015

Copyright (c) Dmitry D. Chernov aka Black Doomer (blackdoomer@yandex.ru)
DISTRIBUTED UNDER TERMS AND CONDITIONS OF ORIGINAL LICENSE OF USED ENET VERSION
2014-2015

================================================================================
LIST OF TRANSLATED FILES AND THEIR ANCESTORS:

ENet.pas            <=> enet.h
ENet_Types.pas      <=> types.h
ENet_Protocol.pas   <=> protocol.h
ENet_Callbacks.pas  <=> callbacks.h
ENet_List.pas       <=> list.h
ENet_Time.pas       <=> time.h
ENet_Win32.pas      <=> win32.h

================================================================================
LIST OF FILES THAT WERE NOT TRANSLATED:

unix.h     - because Delphi 7 is Windows-only
utility.h  - because ENET_MIN and ENET_MAX functions have analogues in Math unit

================================================================================
LIST OF RENAMED IDENTIFIERS DUE TO NAMES CONFLICTS:

enet_host_broadcast()      >>>  enet_host_widecast()
enet_peer_ping_interval()  >>>  enet_peer_ping_frequency()
ENetEvent.type             >>>  ENetEvent.kind

================================================================================
NOTES ON TRANSLATING THESE HEADERS FOR NEWER VERSIONS OF DELPHI OR FREEPASCAL:

1. All inline functions have commented "inline" directive in declarations.
   Delphi 7 doesn't support inlines but newer versions of Delphi or FreePascal
   do. So, it's recommended to uncomment these directives when translating to
   these versions.

2. Headers were written using pure Delphi 7 syntax. It's really easy to
   translate them for newer or even older Delphi versions, so for FreePascal.

3. There 2 types were added in ENet_Types.pas that weren't present in
   original types.h: enet_size_t and enet_int. They are simply aliases for
   corresponding C types size_t and int and were added just to simplify
   translating these headers to other Pascal-derived languages.

================================================================================
THANKS TO:

Doom2D.org Community (www.doom2d.org)
 - because ENet headers were translated for Delphi 7 to use ENet in their
   project, Doom 2D: Forever.

Do-wan Kim
 - author of ENet 1.1 full translation to Delphi 7 (included), which were very
   helpful for me when creating these headers.

Dmitry V. Merkulov aka PrimuS aka Prostovitalik aka fgsfds aka Smokepuff
 - that amazing guy from Doom2D.org Community for whom I created these headers,
   who compiled enet.dll (included) and who created long-awaited multiplayer in
   Doom 2D: Forever, based on ENet.

// 10.02.2015 (d.m.y) UTC+10 //
