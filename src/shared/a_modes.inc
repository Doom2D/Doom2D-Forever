(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

// compiler options, common for all game modules
{$MODE OBJFPC}

{$MODESWITCH ADVANCEDRECORDS+}
{$MODESWITCH ALLOWINLINE+}
{$MODESWITCH ANSISTRINGS+}  // Allow use of ansistrings.
{$MODESWITCH AUTODEREF+}  // Automatic (silent) dereferencing of typed pointers.
{$MODESWITCH CLASS+}
{$MODESWITCH CLASSICPROCVARS-}
{$MODESWITCH DEFAULTPARAMETERS+}  // Allow use of default parameter values.
{$MODESWITCH DUPLICATELOCALS-}  // Allow local variables in class methods to have the same names as properties of the class.
{$MODESWITCH EXCEPTIONS+}
{$MODESWITCH HINTDIRECTIVE+}  // Support the hint directives (deprecated, platform etc.)
{$MODESWITCH INITFINAL+}  // Allow use of Initialization and Finalization
{.$MODESWITCH ISOUNARYMINUS-}  // Unary minus as required by ISO pascal.
{$MODESWITCH MACPROCVARS-}  // Use mac-style procedural variables.
{$MODESWITCH NESTEDCOMMENTS-}
{$MODESWITCH NESTEDPROCVARS+}
{$MODESWITCH OBJPAS+}
{$MODESWITCH OUT+}  // Allow use of the out parameter type.
{$MODESWITCH PCHARTOSTRING+}
{$MODESWITCH POINTERTOPROCVAR+}  // Allow silent conversion of pointers to procedural variables.
{$MODESWITCH PROPERTIES+}
{$MODESWITCH REPEATFORWARD+}  // Implementation and Forward declaration must match completely.
{$MODESWITCH RESULT+}
{$MODESWITCH TYPEHELPERS+}  // Allow the use of type helpers.
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

// properly described here: https://gitlab.com/freepascal.org/fpc/source/-/issues/14757
{$IEEEERRORS OFF}

{$INLINE ON}
{$LONGSTRINGS ON}
{$MACRO OFF}  // useless unusable piece of junk that doesn't even support parameter passing
{$OBJECTCHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$POINTERMATH ON}
{$RANGECHECKS OFF}
{$SAFEFPUEXCEPTIONS OFF}
{$SCOPEDENUMS ON}  // this may be changed later
{$SMARTLINK ON}
{$TYPEDADDRESS ON}  // make @ operator return typed pointers. use Addr() to obtain an untyped one.
{$TYPEINFO ON}
{$VARSTRINGCHECKS OFF}

{$S-}  // disable stack checking
{$MMX-}  // get lost, mmx

{$IF DEFINED(D2F_DEBUG)}
  {$STACKFRAMES ON}
  {$HINTS OFF}
{$ELSE}
  {$STACKFRAMES ON}
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
    {$WARNING System drivers are not supported in headless mode. Force stub driver...}
    {$UNDEF USE_SDL}
    {$UNDEF USE_SDL2}
  {$ENDIF}
  {$DEFINE USE_GLSTUB}
  {$IF DEFINED(USE_GLES1) OR DEFINED(USE_OPENGL)}
    {$WARNING Rendering drivers are not needed in headless mode. Force stub driver...}
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

{$IF DEFINED(DISABLE_SOUND)}
  {$IF DEFINED(ENABLE_SOUND)}
    {$ERROR Can't be either defined ENABLE_SOUND and DISABLE_SOUND at same time}
  {$ENDIF}
  // Force disable drivers
  {$UNDEF USE_SOUNDSTUB}
  {$UNDEF USE_SDLMIXER}
  {$UNDEF USE_FMOD}
  {$UNDEF USE_OPENAL}
  // Force disable loaders
  {$UNDEF USE_VORBIS}
  {$UNDEF USE_FLUIDSYNTH}
  {$UNDEF USE_MODPLUG}
  {$UNDEF USE_XMP}
  {$UNDEF USE_MPG123}
  {$UNDEF USE_OPUS}
  {$UNDEF USE_GME}
{$ELSEIF DEFINED(USE_SOUNDSTUB)}
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
{$IF NOT DEFINED(DISABLE_SOUND) AND NOT DEFINED(ENABLE_SOUND)}
  {$DEFINE ENABLE_SOUND}
{$ENDIF}

{$IF DEFINED(USE_GLSTUB)}
  {$IF DEFINED(USE_GLES1) OR DEFINED(USE_OPENGL)}
    {$ERROR Only one rendering driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_GLES1)}
  {$IF DEFINED(USE_GLSTUB) OR DEFINED(USE_OPENGL)}
    {$ERROR Only one rendering driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_OPENGL)}
  {$IF DEFINED(USE_GLSTUB) OR DEFINED(USE_GLES1)}
    {$ERROR Only one rendering driver must be selected!}
  {$ENDIF}
{$ELSE}
  {$DEFINE USE_OPENGL}
  {.$ERROR Rendering driver not selected. Use -dUSE_GLSTUB or -dUSE_GLES1 or -dUSE_OPENGL}
{$ENDIF}

{$IFDEF ENABLE_HOLMES}
  {$IFNDEF USE_SDL2}
    {$ERROR Holmes is supported only with SDL2}
  {$ENDIF}
  {$IFNDEF USE_OPENGL}
    {$ERROR Holmes is supported only with desktop OpenGL}
  {$ENDIF}
{$ENDIF}
