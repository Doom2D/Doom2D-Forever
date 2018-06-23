{$IFDEF USE_NANOGL}
  {$DEFINE USE_GLES1}
{$ELSE}
  {$IFDEF USE_NOGL}
    {$DEFINE USE_GLES1}
  {$ENDIF}
{$ENDIF}

{$IFDEF USE_GLES1}
  {$IFNDEF USE_NANOGL}
    {$DEFINE USE_NOGL}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(USE_NANOGL) and DEFINED(USE_NOGL)}
  {$WARNING you can use only one OpenGL ES wrapper}
{$ENDIF}

{$IFDEF USE_NANOGL}
  nanoGL in '../lib/nanogl/nanoGL.pas',
{$ELSE}
  {$IFDEF USE_NOGL}
    noGL in '../nogl/noGL.pas',
  {$ELSE}
    GL, GLExt,
  {$ENDIF}
{$ENDIF}
