(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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

{$IF DEFINED(USE_SDLMIXER)}
  {$IF DEFINED(USE_FMOD) OR DEFINED(USE_OPENAL)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_FMOD)}
  {$IF DEFINED(USE_SDLMIXER) OR DEFINED(USE_OPENAL)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSEIF DEFINED(USE_OPENAL)}
  {$IF DEFINED(USE_SDLMIXER) OR DEFINED(USE_FMOD)}
    {$ERROR Only one sound driver must be selected!}
  {$ENDIF}
{$ELSE}
  {$ERROR Sound driver not selected. Use -DUSE_SDLMIXER or -DUSE_FMOD or -DUSE_OPENAL}
{$ENDIF}

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
  SDL2 in '../lib/sdl2/sdl2.pas',
{$IFDEF USE_SDLMIXER}
  SDL2_mixer in '../lib/sdl2/SDL2_mixer.pas',
{$ENDIF}
{$IFDEF USE_OPENAL}
  AL in '../lib/openal/al.pas',
  e_soundfile in '../engine/e_soundfile.pas',
  e_soundfile_wav in '../engine/e_soundfile_wav.pas',
  {$IFDEF USE_MODPLUG}
    modplug in '../lib/modplug/modplug.pas',
    e_soundfile_modplug in '../engine/e_soundfile_modplug.pas',
  {$ENDIF}
  {$IFDEF USE_XMP}
    xmp in '../lib/xmp/xmp.pas',
    e_soundfile_xmp in '../engine/e_soundfile_xmp.pas',
  {$ENDIF}
  {$IFDEF USE_MPG123}
    mpg123 in '../lib/mpg123/mpg123.pas',
    e_soundfile_mp3 in '../engine/e_soundfile_mp3.pas',
  {$ENDIF}
{$ENDIF}
  ENet in '../lib/enet/enet.pp',
  e_graphics in '../engine/e_graphics.pas',
  e_input in '../engine/e_input.pas',
  e_log in '../engine/e_log.pas',
  e_sound in '../engine/e_sound.pas',
  e_texture in '../engine/e_texture.pas',
  e_msg in '../engine/e_msg.pas',
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
  SysUtils,
{$IFDEF USE_FMOD}
  fmod in '../lib/FMOD/fmod.pas',
  fmoderrors in '../lib/FMOD/fmoderrors.pas',
  fmodpresets in '../lib/FMOD/fmodpresets.pas',
  fmodtypes in '../lib/FMOD/fmodtypes.pas',
{$ENDIF}
  xprofiler in '../shared/xprofiler.pas',
  binheap in '../shared/binheap.pas',
  hashtable in '../shared/hashtable.pas',
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

  ImagingTypes,
  Imaging,
  ImagingUtility;

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
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]); //k8: fuck off, that's why

{$IFDEF ANDROID}
{$I-}
  e_SetSafeSlowLog(true);
  if SDL_AndroidGetExternalStorageState() <> 0 then
  begin
    storage := SDL_AndroidGetExternalStoragePath();
    Chdir(storage);
    e_WriteLog('Use external storage: ' + storage, TMsgType.Notify)
  end
  else
  begin
    storage := SDL_AndroidGetInternalStoragePath();
    Chdir(storage);
    e_WriteLog('Use internal storage: ' + storage, TMsgType.Notify)
  end;
  if IOresult <> 0 then
  begin
    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, PChar('Invalid path'), PChar('Can''t chdir to ' + storage), nil);
    result := 1;
    exit
  end;
  SetEnvVar('TIMIDITY_CFG', 'timidity.cfg');
{$ENDIF ANDROID}

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

  if LogFileName = '' then
  begin
{$IFDEF HEADLESS}
    LogFileName := 'Doom2DF_H.log';
{$ELSE}
    LogFileName := 'Doom2DF.log';
{$ENDIF}
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
