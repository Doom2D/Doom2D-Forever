{
  Translation of the OpenAL headers for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
  Copyright (C) 2019 by fgsfds
}

unit AL;

{$mode objfpc}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$IFNDEF AL_WINDOZE_STATIC}
    {$DEFINE AL_DYNAMIC}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(AL_DYNAMIC)}
const
{$IF DEFINED(WINDOWS)}
  openallib = 'openal32.dll';
{$ELSEIF DEFINED(UNIX)}
  openallib = 'libopenal.so';
{$ELSE}
  {$MESSAGE ERROR 'AL_DYNAMIC not supported'}
{$IFEND}
{$ELSEIF DEFINED(Darwin)}
{$LINKFRAMEWORK OpenAL}
{$ELSE}
  {$LINKLIB libopenal.a}
{$ENDIF}

{$include al.inc}
{$include alc.inc}
{$include alext.inc}
{$include efx.inc}

implementation

end.
