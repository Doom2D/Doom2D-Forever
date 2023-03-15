{$MACRO ON}
{.$DEFINE LIBENET_WINDOZE_STATIC}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF LIBENET_WINDOZE_STATIC}
    {$LINKLIB libenet.a}
    {$LINKLIB libwinmm.a}
    {$LINKLIB libws2_32.a}
    {$LINKLIB libkernel32.a}
    {$LINKLIB libm.a}
    {$LINKLIB libmingwex.a}
    {$LINKLIB libmingw32.a}
    {$LINKLIB libmsvcrt.a}
    {$LINKLIB libgcc.a}
    {$DEFINE libraryENet := cdecl; external}
  {$ENDIF}
{$ELSE}
  {$IFDEF DARWIN}
    {$LINKLIB libenet}
    {$DEFINE libraryENet := cdecl; external}
  {$ENDIF}
{$ENDIF}


{$INCLUDE 'ENet-Pascal/enet.pp'}
