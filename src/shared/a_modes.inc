// compiler options, common for all game modules
{$MODE OBJFPC}

{$MODESWITCH ADVANCEDRECORDS+}
{$MODESWITCH ALLOWINLINE+}
{$MODESWITCH ANSISTRINGS+} // Allow use of ansistrings.
{$MODESWITCH AUTODEREF+} // Automatic (silent) dereferencing of typed pointers.
{$MODESWITCH CLASS+}
{$MODESWITCH CLASSICPROCVARS+} // Use classical procedural variables.
{$MODESWITCH DEFAULTPARAMETERS+} // Allow use of default parameter values.
{$MODESWITCH DUPLICATELOCALS-} // Allow local variables in class methods to have the same names as properties of the class.
{$MODESWITCH EXCEPTIONS+}
{$MODESWITCH HINTDIRECTIVE+} // Support the hint directives (deprecated, platform etc.)
{$MODESWITCH INITFINAL+} // Allow use of Initialization and Finalization
{.$MODESWITCH ISOUNARYMINUS-} // Unary minus as required by ISO pascal.
{$MODESWITCH MACPROCVARS-} // Use mac-style procedural variables.
{$MODESWITCH NESTEDCOMMENTS-}
{$MODESWITCH NESTEDPROCVARS+}
{$MODESWITCH OBJPAS+}
{$MODESWITCH OUT+} // Allow use of the out parameter type.
{$MODESWITCH PCHARTOSTRING+}
{$MODESWITCH POINTERTOPROCVAR+} // Allow silent conversion of pointers to procedural variables.
{$MODESWITCH PROPERTIES+}
{$MODESWITCH REPEATFORWARD+} // Implementation and Forward declaration must match completely.
{$MODESWITCH RESULT+}
{$MODESWITCH TYPEHELPERS+} // Allow the use of type helpers.
{$MODESWITCH UNICODESTRINGS-}

{$IFDEF DARWIN}
  {$MODESWITCH OBJECTIVEC1}
{$ENDIF}

{$ASSERTIONS ON}
{$BITPACKING OFF}
{$BOOLEVAL OFF}
{$COPERATORS ON}
{$EXTENDEDSYNTAX ON}
{$IFDEF CPU386}
  {$FPUTYPE SSE}
{$ENDIF}
{$GOTO ON}
{$IEEEERRORS OFF}
{$INLINE ON}
{$LONGSTRINGS ON}
{$MACRO OFF}
{$OBJECTCHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$POINTERMATH ON}
{$RANGECHECKS OFF}
{$SAFEFPUEXCEPTIONS OFF}
{$SCOPEDENUMS ON} // this may be changed later
{$SMARTLINK ON}
{$TYPEDADDRESS ON}
{$TYPEINFO ON}
{$VARSTRINGCHECKS OFF}

{$S-} // disable stack checking
{$MMX-} // get lost, mmx

{$IF DEFINED(D2F_DEBUG)}
  {$STACKFRAMES ON}
  {$HINTS OFF}
{$ELSE}
  {$STACKFRAMES OFF}
  {$HINTS OFF}
  {$DEFINE D2F_MORE_OPTIM}
{$ENDIF}
{$WARNINGS ON}
{.$NOTES ON}

{$IF DEFINED(D2F_DEBUG_OPTIM) or DEFINED(D2F_MORE_OPTIM)}
  {$OPTIMIZATION DEADVALUES}
  {$OPTIMIZATION CONSTPROP}
  {$OPTIMIZATION DEADSTORE}
{$ENDIF}

{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFNDEF WINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF WINDOWS}
{$ENDIF}

(* --------- Build options check --------- *)

{$IFDEF HEADLESS}
  {$DEFINE USE_SYSSTUB}
  {$IF DEFINED(USE_SDL) OR DEFINED(USE_SDL2)}
    {$WARNING System drivers not supported in headless mode. Force stub driver...}
    {$UNDEF USE_SDL}
    {$UNDEF USE_SDL2}
  {$ENDIF}
  {$DEFINE USE_GLSTUB}
  {$IF DEFINED(USE_GLES1) OR DEFINED(USE_OPENGL)}
    {$WARNING Render drivers not needed in headless mode. Force stub driver...}
    {$UNDEF USE_GLES1}
    {$UNDEF USE_OPENGL}
  {$ENDIF}
  {$IFDEF ENABLE_HOLMES}
    {$WARNING Holmes in headless mode has no sense. Disabled.}
    {$UNDEF ENABLE_HOLMES}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(USE_SYSSTUB)}
  {$IF DEFINED(USE_SDL) OR DEFINED(USE_SDL2)}
    {$ERROR Only one system driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_SDL)}
  {$IF DEFINED(USE_SYSSTUB) OR DEFINED(USE_SDL2)}
    {$ERROR Only one system driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_SDL2)}
  {$IF DEFINED(USE_SYSSTUB) OR DEFINED(USE_SDL)}
    {$ERROR Only one system driver must be selected!}
  {$ENDIF}
{$ELSE}
  {$DEFINE USE_SDL2}
  {.$ERROR System driver not selected. Use -dUSE_SYSSTUB or -dUSE_SDL or -dUSE_SDL2}
{$ENDIF}

{$IF DEFINED(USE_SOUNDSTUB)}
  {$IF DEFINED(USE_SDLMIXER) OR DEFINED(USE_FMOD) OR DEFINED(USE_OPENAL)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_SDLMIXER)}
  {$IF DEFINED(USE_SOUNDSTUB) OR DEFINED(USE_FMOD) OR DEFINED(USE_OPENAL)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_FMOD)}
  {$IF DEFINED(USE_SOUNDSTUB) OR DEFINED(USE_SDLMIXER) OR DEFINED(USE_OPENAL)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_OPENAL)}
  {$IF DEFINED(USE_SOUNDSTUB) OR DEFINED(USE_SDLMIXER) OR DEFINED(USE_FMOD)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSE}
  {$IFDEF IN_TOOLS}
    {$DEFINE USE_SOUNDSTUB}
  {$ELSE}
    {$ERROR Sound driver not selected. Use -dUSE_SOUNDSTUB or -dUSE_SDLMIXER or -dUSE_FMOD or -dUSE_OPENAL}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(USE_GLSTUB)}
  {$IF DEFINED(USE_GLES1) OR DEFINED(USE_OPENGL)}
    {$ERROR Only one render driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_GLES1)}
  {$IF DEFINED(USE_GLSTUB) OR DEFINED(USE_OPENGL)}
    {$ERROR Only one render driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_OPENGL)}
  {$IF DEFINED(USE_GLSTUB) OR DEFINED(USE_GLES1)}
    {$ERROR Only one render driver must be selected!}
  {$ENDIF}
{$ELSE}
  {$DEFINE USE_OPENGL}
  {.$ERROR Render driver not selected. Use -dUSE_GLSTUB or -dUSE_GLES1 or -dUSE_OPENGL}
{$ENDIF}

{$IFDEF ENABLE_HOLMES}
  {$IFNDEF USE_SDL2}
    {$ERROR Holmes supported only with SDL2}
  {$ENDIF}
  {$IFNDEF USE_OPENGL}
    {$ERROR Holmes supported only with desktop OpenGL}
  {$ENDIF}
{$ENDIF}
