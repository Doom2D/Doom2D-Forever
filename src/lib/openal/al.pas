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

{$IF DEFINED(WINDOWS)}
  {$IFDEF OPENAL_WINDOZE_STATIC}
    {$LINKLIB libopenal.a}
  {$ELSE}
    const openallib = 'openal32.dll';
    {$DEFINE AL_DYNAMIC}
  {$ENDIF}
{$ELSEIF DEFINED(UNIX)}
  const openallib = 'libopenal.so';
  {$DEFINE AL_DYNAMIC}
{$ELSE}
  {$ERROR OpenAL not supported on this platform. Fix it!}
{$ENDIF}

{$include al.inc}
{$include alc.inc}
{$include alext.inc}
{$include efx.inc}

implementation

end.
