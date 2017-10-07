{.$DEFINE LIBJIT_WINDOZE_STATIC}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{.$MODE OBJFPC}
{$PACKRECORDS C}
{$MACRO ON}

{$Z4} // Force four-byte enums


{$IFDEF MSWINDOWS}
  {$IFDEF LIBJIT_WINDOZE_STATIC}
    {$LINKLIB libjit.a}
    {$LINKLIB libkernel32.a}
    {$LINKLIB libm.a}
    {$LINKLIB libmingwex.a}
    {$LINKLIB libmingw32.a}
    {$LINKLIB libmsvcrt.a}
    {$LINKLIB libgcc.a}
    {$DEFINE libraryLibJITDecl := cdecl}
    {$DEFINE libraryLibJITImp := cdecl; external}
    {$DEFINE libraryLibJITVar := cvar; external}
  {$ELSE}
    {$DEFINE libraryLibJITDecl := cdecl}
    {$DEFINE libraryLibJITImp := cdecl; external 'libjit-0.dll'}
    {.$DEFINE libraryLibJITVar := cvar; external}
    {$DEFINE libraryLibJITVar := external 'libjit-0.dll'}
    // external LIBNAME name 'var_name' would've been more correct here
    // because just external is case insensitive, but fuck it
  {$ENDIF}
{$ELSE}
  {$DEFINE libraryLibJITDecl := cdecl}
  {$DEFINE libraryLibJITImp := cdecl; external 'jit'}
  {$DEFINE libraryLibJITVar := cvar; external 'jit'}
{$ENDIF}
