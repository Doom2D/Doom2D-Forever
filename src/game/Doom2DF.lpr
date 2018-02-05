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
program Doom2DF;
{$IFNDEF HEADLESS}
  {$IFDEF WINDOWS}
    {$APPTYPE GUI}
  {$ENDIF}
{$ENDIF}
{$HINTS OFF}

{$IFDEF USE_SDLMIXER}
 {$IFDEF USE_FMOD}
  {$ERROR define only one of USE_SDLMIXER or USE_FMOD}
 {$ENDIF}
{$ELSE}
 {$UNDEF USE_SDLMIXER}
 {$DEFINE USE_FMOD}
{$ENDIF}

uses
  mempool in '../shared/mempool.pas',
  conbuf in '../shared/conbuf.pas',
  geom in '../shared/geom.pas',
  math,
  GL,
  GLExt,
  miniupnpc in '../lib/miniupnpc/miniupnpc.pas',
  SDL2 in '../lib/sdl2/sdl2.pas',
{$IFDEF USE_SDLMIXER}
  SDL2_mixer in '../lib/sdl2/SDL2_mixer.pas',
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
  g_holmes in 'g_holmes.pas',
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

  sdlcarcass in '../flexui/sdlcarcass.pas',
  //sdlstandalone in '../flexui/sdlstandalone.pas',

  fui_wadread in '../flexui/fui_wadread.pas',
  fui_common in '../flexui/fui_common.pas',
  fui_gfx_gl in '../flexui/fui_gfx_gl.pas',
  fui_events in '../flexui/fui_events.pas',
  fui_style in '../flexui/fui_style.pas',
  fui_flexlay in '../flexui/fui_flexlay.pas',
  fui_ctls in '../flexui/fui_ctls.pas',

  ImagingTypes,
  Imaging,
  ImagingUtility;

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

var
  f: Integer;
  noct: Boolean = false;
  //tfo: Text;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]); //k8: fuck off, that's why
  for f := 1 to ParamCount do
  begin
         if ParamStr(f) = '--gdb' then noct := true
    else if ParamStr(f) = '--log' then conbufDumpToStdOut := true
    else if ParamStr(f) = '--safe-log' then e_SetSafeSlowLog(true);
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
end.
