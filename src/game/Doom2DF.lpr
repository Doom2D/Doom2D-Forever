(* Doom2D Forever - A remake of the 1996 game Doom2D by Prikol Software with online multiplayer.
 * Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
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

{$INCLUDE ../shared/a_modes.inc}
{$IFNDEF ANDROID} program {$ELSE} library {$ENDIF} Doom2DF;

{$MODESWITCH CLASSICPROCVARS-}  // TODO: Make this default.

// hope this has no negative impact in case of 'library' above
// https://stackoverflow.com/questions/11716350/effects-of-switching-between-subsystemconsole-to-subsystemwindows-in-a-dll
// https://docwiki.embarcadero.com/RADStudio/en/Application_type_(Delphi) - against, but anyway
{$IFNDEF HEADLESS}
  {$APPTYPE GUI}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

// TODO: Use {$UNITPATH ../*;../lib/*} instead of a lot of explicit relative paths in 'uses' block?

uses
{$IFDEF ANDROID}
  ctypes,
{$ENDIF}
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  mempool in '../shared/mempool.pas',
  conbuf in '../shared/conbuf.pas',
  geom in '../shared/geom.pas',
  math,

{$INCLUDE ../nogl/noGLuses.inc}

{$IFDEF USE_MINIUPNPC}
  miniupnpc in '../lib/miniupnpc/miniupnpc.pas',
{$ENDIF}

{$IFDEF USE_SDL}
  SDL in '../lib/sdl/sdl.pas',
  {$IFDEF USE_SDLMIXER}
    SDL_mixer in '../lib/sdl/sdl_mixer.pas',
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_SDL2}
  SDL2 in '../lib/sdl2/sdl2.pas',
  {$IFDEF USE_SDLMIXER}
    SDL2_mixer in '../lib/sdl2/SDL2_mixer.pas',
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_SYSSTUB}
  {$IFDEF USE_SDLMIXER}
    SDL2 in '../lib/sdl2/sdl2.pas',
    SDL2_mixer in '../lib/sdl2/SDL2_mixer.pas',
  {$ENDIF}
{$ENDIF}

{$IFDEF USE_OPENAL}
  AL in '../lib/openal/al.pas',
  e_soundfile in '../engine/e_soundfile.pas',
  {$IF DEFINED(USE_SDL) OR DEFINED(USE_SDL2)}
    e_soundfile_wav in '../engine/e_soundfile_wav.pas',
  {$ENDIF}
  {$IFDEF USE_VORBIS}
    vorbis in '../lib/vorbis/vorbis.pas',
    e_soundfile_vorbis in '../engine/e_soundfile_vorbis.pas',
  {$ENDIF}
  {$IFDEF USE_FLUIDSYNTH}
    fluidsynth in '../lib/fluidsynth/fluidsynth.pas',
    e_soundfile_fluid in '../engine/e_soundfile_fluid.pas',
  {$ENDIF}
  {$IFDEF USE_MODPLUG}
    modplug in '../lib/modplug/modplug.pas',
    e_soundfile_modplug in '../engine/e_soundfile_modplug.pas',
  {$ENDIF}
  {$IFDEF USE_XMP}
    xmp in '../lib/xmp/xmp.pas',
    e_soundfile_xmp in '../engine/e_soundfile_xmp.pas',
  {$ENDIF}
  {$IFDEF USE_GME}
    gme in '../lib/gme/gme.pas',
    e_soundfile_gme in '../engine/e_soundfile_gme.pas',
  {$ENDIF}
  {$IFDEF USE_MPG123}
    mpg123 in '../lib/mpg123/mpg123.pas',
    e_soundfile_mp3 in '../engine/e_soundfile_mp3.pas',
  {$ENDIF}
  {$IFDEF USE_OPUS}
    opus in '../lib/opus/opus.pas',
    e_soundfile_opus in '../engine/e_soundfile_opus.pas',
  {$ENDIF}
  {$IF DEFINED(USE_VORBIS) OR DEFINED(USE_OPUS)}
    ogg in '../lib/vorbis/ogg.pas', // this should come last for proper linking order
  {$ENDIF}
{$ENDIF}

  ENet in '../lib/enet/enet.pp',
  e_graphics in '../engine/e_graphics.pas',
  e_input in '../engine/e_input.pas',
  e_log in '../engine/e_log.pas',
{$IFDEF ENABLE_SOUND}
  e_sound in '../engine/e_sound.pas',
{$ENDIF}
  e_texture in '../engine/e_texture.pas',
  e_msg in '../engine/e_msg.pas',
  e_res in '../engine/e_res.pas',
  utils in '../shared/utils.pas',
  xstreams in '../shared/xstreams.pas',
  sfs in '../sfs/sfs.pas',
  sfsPlainFS in '../sfs/sfsPlainFS.pas',
  sfsZipFS in '../sfs/sfsZipFS.pas',
  wadreader in '../shared/wadreader.pas',
  MAPDEF in '../shared/MAPDEF.pas',
  CONFIG in '../shared/CONFIG.pas',
  g_basic in 'g_basic.pas',
  g_console in 'g_console.pas',
  g_net in 'g_net.pas',
  g_netmsg in 'g_netmsg.pas',
  g_nethandler in 'g_nethandler.pas',
  g_netmaster in 'g_netmaster.pas',
  g_res_downloader in 'g_res_downloader.pas',
  g_grid in 'g_grid.pas',
  g_game in 'g_game.pas',
  g_gfx in 'g_gfx.pas',
  g_gui in 'g_gui.pas',
  g_items in 'g_items.pas',
  g_main in 'g_main.pas',
  g_map in 'g_map.pas',
  g_menu in 'g_menu.pas',
  g_monsters in 'g_monsters.pas',
  g_options in 'g_options.pas',
  g_phys in 'g_phys.pas',
  g_player in 'g_player.pas',
  g_playermodel in 'g_playermodel.pas',
  g_saveload in 'g_saveload.pas',
{$IFDEF ENABLE_SOUND}
  g_sound in 'g_sound.pas',
{$ENDIF}
  g_textures in 'g_textures.pas',
  g_triggers in 'g_triggers.pas',
  g_weapons in 'g_weapons.pas',
  g_window in 'g_window.pas',
  g_system in 'g_system.pas',
  g_touch in 'g_touch.pas',

{$IFDEF USE_FMOD}
  fmod in '../lib/FMOD/fmod.pas',
  fmoderrors in '../lib/FMOD/fmoderrors.pas',
  fmodpresets in '../lib/FMOD/fmodpresets.pas',
  fmodtypes in '../lib/FMOD/fmodtypes.pas',
{$ENDIF}
  xprofiler in '../shared/xprofiler.pas',
  binheap in '../shared/binheap.pas',
  hashtable in '../shared/hashtable.pas',
  fhashdb in '../shared/fhashdb.pas',
  idpool in '../shared/idpool.pas',
  xparser in '../shared/xparser.pas',
  xdynrec in '../shared/xdynrec.pas',
  exoma in '../shared/exoma.pas',
  envvars in '../shared/envvars.pas',
  g_panel in 'g_panel.pas',
  g_language in 'g_language.pas',

{$IFDEF ENABLE_HOLMES}
  g_holmes in 'g_holmes.pas',

  sdlcarcass in '../flexui/sdlcarcass.pas',
  //sdlstandalone in '../flexui/sdlstandalone.pas',

  fui_wadread in '../flexui/fui_wadread.pas',
  fui_common in '../flexui/fui_common.pas',
  fui_gfx_gl in '../flexui/fui_gfx_gl.pas',
  fui_events in '../flexui/fui_events.pas',
  fui_style in '../flexui/fui_style.pas',
  fui_flexlay in '../flexui/fui_flexlay.pas',
  fui_ctls in '../flexui/fui_ctls.pas',
{$ENDIF}
  {$I ../shared/vampimg.inc}
  SysUtils;

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

// FIXME: As of 3.2.2, FormatBuf() allocates on heap, unfortunately, which could be corrupt.
// https://gitlab.com/freepascal.org/fpc/source/-/issues/41475
procedure FormatStaticString(out aString: ShortString; constref aFormat: String {type of constants};
  const aList: array of const); inline;
begin
  SetLength(aString,
    FormatBuf(aString[1], High(aString), PChar(aFormat)^, Length(aFormat), aList)
  );
end;

// NB: Variables here are treated by FPC just like the unit ones, so it initializes them. See this:
// - https://www.freepascal.org/docs-html/3.2.2/ref/refse21.html - 4.1: Variables / Definition
// - https://wiki.freepascal.org/Global_variables
// - https://forum.lazarus.freepascal.org/index.php?topic=60809.0 - Global Variable in program

var
  k: Integer = 1;
  StopOnException: Boolean;
  ErrorText: ShortString;
  ErrorType: TClass;
  ErrorCode: LongInt;  // NB: this shadows ErrorCode typed constant in the System unit

// RTL sources in FPC 3.2.2 related to the exception handling, in decreasing order of importance:
// > units:       System, SysUtils, ObjPas
// - system.pp:   source/rtl/inc/system.inc
// - system.pp:   source/rtl/inc/systemh.inc
// - objpas.inc:  source/rtl/inc/except.inc
// - systemh.inc: source/rtl/inc/objpash.inc
// - systemh.inc: source/rtl/inc/excepth.inc
// - sysutils.pp: source/rtl/objpas/sysutils/sysutils.inc
// - sysutils.pp: source/rtl/objpas/sysutils/sysutilh.inc
// - system.inc:  source/rtl/inc/objpas.inc

{$IFDEF ANDROID}
function SDL_main(argc: CInt; argv: PPChar): CInt; cdecl;
{$ENDIF}
const
  FormatException = '%s (%s:%d #%x, at $%p)';
  FormatFatalError = 'FATAL ERROR (%s.%s $%p, at $%p)';
  FormatMalfunction = 'invalid fault $%p at $%p - PROBABLY YOUR SOFTWARE OR HARDWARE IS BROKEN';
begin
{$IF DECLARED(UseHeapTrace)}
  heaptrc.HaltOnError := False;  // continue execution even if there is a heap error
{$ENDIF}

{$IFDEF ANDROID}
  System.argc := argc;
  System.argv := argv;
{$ENDIF}

  // Disable x87 and SSE floating point exceptions (https://wiki.freepascal.org/SetExceptionMask).
  // NB: TFPUException instead of TFPUExceptionMask here seems to include exactly the same values.
  SetExceptionMask([Low(TFPUExceptionMask)..High(TFPUExceptionMask)]);

  while k <= ParamCount() do
  begin
    case ParamStr(k) of
    '--gdb': StopOnException := True;
    '--log': conbufDumpToStdOut := True;
    '--safe-log': e_SetSafeSlowLog(True);
    '--log-file':
      if k < ParamCount() then
      begin
        k += 1;
        LogFileName := ParamStr(k);
      end;
    end;
    k += 1;
  end;

  try
  ////////////////////////////////////////////////////////////////////////////////////////////////
  try
    Main();
    e_WriteLog('Shutdown with no errors.', TMsgType.Notify);
  except on E: TObject do
  begin
    if E is Exception then
    begin
      // gather some additional info from all standard descendants known
      if E is EVariantError then ErrorCode := EVariantError(E).ErrCode
      else if E is EInOutError then ErrorCode := EInOutError(E).ErrorCode
      else if E is EOSError then ErrorCode := EOSError(E).ErrorCode;

      ErrorType := Exception;
      with Exception(E) do FormatStaticString(ErrorText, FormatException,
        [Message, ClassName(), HelpContext, ErrorCode, ExceptAddr()]);
    end
    else
    begin
      ErrorType := TObject;
      // TODO: Switch to using TObject.QualifiedClassName() here? (available since FPC 3.1.1)
      FormatStaticString(ErrorText, FormatFatalError,
        [E.UnitName(), E.ClassName(), Addr(E), ExceptAddr()]);
    end;

    e_WriteStackTrace(ErrorText);

    // Also write string representation if available (check if method is overridden).
    // NOTE: This expression compares only the code pointers here actually, which is what we need.
    // See https://www.freepascal.org/docs-html/ref/refse17.html - 3.6: Types / Procedural types.
    if @E.ToString <> @ErrorType.ToString then
      e_WriteLog(E.ToString(), TMsgType.Fatal);  // AnsiString requires heap, so this must go last.

    if StopOnException then
    begin
    {$IF DECLARED(UseHeapTrace)}
      heaptrc.UseHeapTrace := False;  // prevent clogging of debug session with Heaptrc windows
    {$ENDIF}
      Raise;
    end;

    // Unhandled exceptions cause their own exit codes, so we set our own only at the very end.
    ExitCode := 1;  // EXIT_FAILURE
  end
  else
    // I doubt this being theoretically possible, as there can be no descendants not from TObject,
    // but RTL also checks for that case explicitly - see sysutils.inc:CatchUnhandledException().
    // Wondering what the result of SysUtils.ExceptObject() would be like in such a situation.
    FormatStaticString(ErrorText, FormatMalfunction, [Pointer(ExceptObject()), ExceptAddr()]);
    e_WriteStackTrace(ErrorText);
    Raise;
  end;
  ////////////////////////////////////////////////////////////////////////////////////////////////
  finally
    // NB: This shall be an exception-free place to ensure proper flushing of the log file buffers.
    e_DeinitLog();
  end;

{$IF DECLARED(UseHeapTrace)}
  // Append Heaptrc report to the completed log after the program finish. Note that Heaptrc allows
  // to set the output file by specifying the "LOG=" environment variable, but we don't support
  // this because the Heaptrc API doesn't provide a way to check this directly.
  heaptrc.SetHeapTraceOutput(LogFileName);
{$ENDIF}

{$IFDEF ANDROID}
  Result := ExitCode;
end; // SDL_main

exports SDL_main;
{$ENDIF}

end.
