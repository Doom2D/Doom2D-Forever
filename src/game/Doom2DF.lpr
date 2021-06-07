(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
{$IFDEF ANDROID}library{$ELSE}program{$ENDIF} Doom2DF;

{$IFNDEF HEADLESS}
  {$IFDEF WINDOWS}
    {$APPTYPE GUI}
  {$ENDIF}
{$ENDIF}
{$HINTS OFF}

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
    ogg in '../lib/vorbis/ogg.pas', // this has to come last because link order
  {$ENDIF}
{$ENDIF}

  ENet in '../lib/enet/enet.pp',
  e_graphics in '../engine/e_graphics.pas',
  e_input in '../engine/e_input.pas',
  e_log in '../engine/e_log.pas',
  e_sound in '../engine/e_sound.pas',
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
  g_sound in 'g_sound.pas',
  g_textures in 'g_textures.pas',
  g_triggers in 'g_triggers.pas',
  g_weapons in 'g_weapons.pas',
  g_window in 'g_window.pas',
{$IFDEF USE_SYSSTUB}
  g_system in 'stub/g_system.pas',
  g_touch in 'stub/g_touch.pas',
{$ENDIF}
{$IFDEF USE_SDL}
  g_system in 'sdl/g_system.pas',
  g_touch in 'sdl/g_touch.pas',
{$ENDIF}
{$IFDEF USE_SDL2}
  g_system in 'sdl2/g_system.pas',
  g_touch in 'sdl2/g_touch.pas',
{$ENDIF}

  r_console in 'opengl/r_console.pas',
  r_game in 'opengl/r_game.pas',
  r_gfx in 'opengl/r_gfx.pas',
  r_items in 'opengl/r_items.pas',
  r_map in 'opengl/r_map.pas',
  r_monsters in 'opengl/r_monsters.pas',
  r_panel in 'opengl/r_panel.pas',

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

{$IFDEF ANDROID}
function SDL_main(argc: CInt; argv: PPChar): CInt; cdecl;
{$ENDIF ANDROID}

var
  f: Integer;
  noct: Boolean = false;
{$IFDEF ANDROID}
  storage: String;
{$ENDIF}
  //tfo: Text;
begin
{$IFDEF ANDROID}
  System.argc := argc;
  System.argv := argv;
{$ENDIF}

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]); //k8: fuck off, that's why

  f := 1;
  while f <= ParamCount do
  begin
    case ParamStr(f) of
    '--gdb': noct := true;
    '--log': conbufDumpToStdOut := true;
    '--safe-log': e_SetSafeSlowLog(true);
    '--log-file':
      if f + 1 <= ParamCount then
      begin
        Inc(f);
        LogFileName := ParamStr(f)
      end;
    end;
    Inc(f)
  end;

  if noct then
  begin
    Main()
  end
  else
  begin
    try
      Main();
      e_WriteLog('Shutdown with no errors.', TMsgType.Notify);
    except
      on e: Exception do
        begin
          e_WriteStackTrace(e.message);
          //e_WriteLog(Format(_lc[I_SYSTEM_ERROR_MSG], [E.Message]), MSG_FATALERROR);
          (*
          AssignFile(tfo, GameDir+'/trace.log');
          {$I-}
          Append(tfo);
          if (IOResult <> 0) then Rewrite(tfo);
          if (IOResult = 0) then begin writeln(tfo, '====================='); DumpExceptionBackTrace(tfo); CloseFile(tfo); end;
          *)
        end
      else
        begin
          //e_WriteLog(Format(_lc[I_SYSTEM_ERROR_UNKNOWN], [NativeUInt(ExceptAddr())]), MSG_FATALERROR);
          e_WriteStackTrace('FATAL ERROR');
        end;
    end;
  end;
  e_DeinitLog();

{$IFDEF ANDROID}
  result := 0;
end; // SDL_main
exports SDL_main;
{$ENDIF ANDROID}
end.
