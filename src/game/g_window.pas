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

{$INCLUDE ../shared/a_modes.inc}
unit g_window;

interface

function PerformExecution (): Integer;
procedure ResetTimer ();
procedure ProcessLoading (forceUpdate: Boolean = False);

var
  gwin_has_stencil: Boolean;
  gwin_k8_enable_light_experiments: Boolean;
  g_dbg_aimline_on: Boolean;
  g_dbg_input: Boolean;

implementation

uses
{$IFDEF WINDOWS}Windows,{$ENDIF}
{$IFDEF ENABLE_HOLMES}
  g_holmes, sdlcarcass, fui_ctls,
{$ENDIF}
{$INCLUDE ../nogl/noGLuses.inc}
{$IFDEF ENABLE_SOUND}
  g_sound, e_sound,
{$ENDIF}
  SysUtils, Classes, MAPDEF, Math, utils,
  e_graphics, e_log, e_texture, g_main,
  g_console, e_input, g_options, g_game,
  g_basic, g_textures, g_menu, ENet, g_net,
  g_map, g_gfx, g_monsters, xprofiler,
  g_touch, g_gui, g_system, g_netmaster;


const
  ProgressUpdateMSecs = 35; //1;//100;

var
  Time, Time_Delta, Time_Old: Int64;
  Frame: Int64;
  flag: Boolean;
  wNeedTimeReset: Boolean;
  wMinimized: Boolean;
  prevSoundTimeMs: UInt64;

procedure UpdateSound ();
var
  ms: UInt64;
begin
  ms := GetTickCount64();
{$IFDEF ENABLE_SOUND}
  e_SoundUpdate();
{$ELSE}
  if gMusicPlay and not gMusicPause then
    gMusicPos := gMusicPos + (ms - prevSoundTimeMs);
{$ENDIF}
  prevSoundTimeMs := ms;
end;

procedure ResetTimer ();
begin
  wNeedTimeReset := True;
end;

{$IFNDEF HEADLESS}
var
  prevLoadingUpdateTime: UInt64;
{$ENDIF}

procedure ProcessLoading (forceUpdate: Boolean);
{$IFNDEF HEADLESS}
var
  stt: UInt64;
{$ENDIF}
begin
  if sys_HandleEvents() then
    Exit;

{$IFNDEF HEADLESS}
  if not wMinimized then
  begin
    if not forceUpdate then
    begin
      stt := getTimeMilli();
      forceUpdate := (stt < prevLoadingUpdateTime) or (stt-prevLoadingUpdateTime >= ProgressUpdateMSecs);
    end;

    if forceUpdate then
    begin
      e_SetRendertarget(True);
      e_SetViewPort(0, 0, gScreenWidth, gScreenHeight);

      DrawMenuBackground('INTER');
      e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
      DrawLoadingStat();
      g_Console_Draw(True);

      e_SetRendertarget(False);
      e_SetViewPort(0, 0, gWinSizeX, gWinSizeY);
      e_BlitFramebuffer(gWinSizeX, gWinSizeY);

      sys_Repaint;
      prevLoadingUpdateTime := getTimeMilli();
    end;
  end;
{$ENDIF}

  UpdateSound();

  // TODO: At the moment, I left here only host network processing, because the client code must
  // handle network events on its own. Otherwise separate network cases that use different calls to
  // enet_host_service() WILL lose their packets (for example, resource downloading). So they have
  // to handle everything by themselves. But in general, this MUST be removed completely, since
  // updating the window should never affect the network. Use single enet_host_service(), period.
  if NetMode = NET_SERVER
    then g_Net_Host_Update();
end;


function ProcessMessage (): Boolean;
var
  i, t: Integer;
begin
  // BEWARE: Short-circuit evaluation matters here!
  Result := (gExit = EXIT_QUIT) or sys_HandleEvents();
  if Result then
  begin
    g_Game_Free();
    g_Game_Quit();
    Exit;
  end;

  Time := sys_GetTicks();
  Time_Delta := Time-Time_Old;

  flag := False;

  if wNeedTimeReset then
  begin
    Frame := 0;
    Time_Delta := UPS_INTERVAL;
    wNeedTimeReset := False;
  end;

  g_Map_ProfilersBegin();
  g_Mons_ProfilersBegin();

  t := Time_Delta div UPS_INTERVAL;
  if t > 0 then
  begin
    flag := True;
    for i := 1 to t do
      Update();
  end;

  g_Map_ProfilersEnd();
  g_Mons_ProfilersEnd();

  // Время предыдущего обновления
  if flag then
    Time_Old := Time - (Time_Delta mod UPS_INTERVAL);

  // don't wait if VSync is on, GL already probably waits enough
  if gLerpActors then
    flag := (Time - Frame >= gFrameTime) or gVSync;

  if flag then
  begin
    if not wMinimized then
    begin
      if gPause or not gLerpActors or (gState = STATE_FOLD)
        then gLerpFactor := 1.0
        else gLerpFactor := nmin(1.0, (Time - Time_Old) / UPS_INTERVAL);
      Draw();
      sys_Repaint();
    end;
    Frame := Time;
  end
  else
    sys_YieldTimeSlice();  // force thread scheduler switch to avoid false-positive 100% CPU load

  UpdateSound();
end;

function GLExtensionList (): SSArray;
var
  s: PChar;
  i, j, num: GLint;
begin
  Result := nil;
  s := glGetString(GL_EXTENSIONS);
  if s <> nil then
  begin
    num := 0;
    i := 0;
    j := 0;
    while (s[i] <> #0) and (s[i] = ' ') do Inc(i);
    while (s[i] <> #0) do
    begin
      while (s[i] <> #0) and (s[i] <> ' ') do Inc(i);
      SetLength(result, num+1);
      result[num] := Copy(s, j+1, i-j);
      while (s[i] <> #0) and (s[i] = ' ') do Inc(i);
      j := i;
      Inc(num);
    end;
  end;
end;

function GLExtensionSupported (ext: AnsiString): Boolean;
var
  exts: SSArray;
  e: AnsiString;
begin
  Result := False;
  exts := GLExtensionList();
  for e in exts do
  begin
    //writeln('<', e, '> : [', ext, '] = ', strEquCI1251(e, ext));
    if strEquCI1251(e, ext) then Exit(True);
  end;
end;

procedure PrintGLSupportedExtensions ();
begin
  e_LogWritefln('GL Vendor: %s', [glGetString(GL_VENDOR)]);
  e_LogWritefln('GL Renderer: %s', [glGetString(GL_RENDERER)]);
  e_LogWritefln('GL Version: %s', [glGetString(GL_VERSION)]);
  e_LogWritefln('GL Shaders: %s', [glGetString(GL_SHADING_LANGUAGE_VERSION)]);
  e_LogWritefln('GL Extensions: %s', [glGetString(GL_EXTENSIONS)]);
end;

function PerformExecution (): Integer;
var
  idx: Integer = 1;
  arg: AnsiString;
  mdfo: TStream;
{$IFDEF ENABLE_HOLMES}
  itmp: Integer;
  valres: Word;
{$ENDIF}
begin
{$IFDEF HEADLESS}
  e_NoGraphics := True;
{$ENDIF}

  while idx <= ParamCount() do
  begin
    arg := ParamStr(idx);

    if arg = '--jah' then g_profile_history_size := 100;
    if arg = '--no-particles' then gpart_dbg_enabled := False;
    if arg = '--no-los' then gmon_dbg_los_enabled := False;

    if arg = '--profile-render' then g_profile_frame_draw := True;
    if arg = '--profile-coldet' then g_profile_collision := True;
    if arg = '--profile-los' then g_profile_los := True;

    if arg = '--no-part-phys' then gpart_dbg_phys_enabled := False;
    if arg = '--no-part-physics' then gpart_dbg_phys_enabled := False;
    if arg = '--no-particles-phys' then gpart_dbg_phys_enabled := False;
    if arg = '--no-particles-physics' then gpart_dbg_phys_enabled := False;
    if arg = '--no-particle-phys' then gpart_dbg_phys_enabled := False;
    if arg = '--no-particle-physics' then gpart_dbg_phys_enabled := False;

    if arg = '--debug-input' then g_dbg_input := True;

  {.$IF DEFINED(D2F_DEBUG)}
    if arg = '--aimline' then g_dbg_aimline_on := True;
  {.$ENDIF}

  {$IFDEF ENABLE_HOLMES}
    if arg = '--holmes' then
    begin
      g_holmes_enabled := True;
      g_Game_SetDebugMode();
    end;

    if (arg = '--holmes-ui-scale') or (arg = '-holmes-ui-scale') then
    begin
      if idx < ParamCount() then
      begin
        idx += 1;
        if not conParseFloat(fuiRenderScale, ParamStr(idx)) then
          fuiRenderScale := 1.0;
      end;
    end;

    if (arg = '--holmes-font') or (arg = '-holmes-font') then
    begin
      if idx < ParamCount() then
      begin
        idx += 1;
        itmp := 0;
        Val(ParamStr(idx), itmp, valres);
      {$IFNDEF HEADLESS}
        if (valres = 0) and not g_holmes_nonfunctional then
        begin
          case itmp of
            8: uiContext.font := 'win8';
            14: uiContext.font := 'win14';
            16: uiContext.font := 'win16';
          end;
        end;
      {$ELSE}
        // fuck off, fpc!
        itmp := itmp;
        valres := valres;
      {$ENDIF}
      end;
    end;
  {$ENDIF}

    if (arg = '--game-scale') or (arg = '-game-scale') then
    begin
      if idx < ParamCount() then
      begin
        idx += 1;
        if not conParseFloat(g_dbg_scale, ParamStr(idx)) then
          g_dbg_scale := 1.0;
      end;
    end;

    if (arg = '--write-mapdef') or (arg = '-write-mapdef') then
    begin
      mdfo := createDiskFile('mapdef.txt');
      mdfo.WriteBuffer(defaultMapDef[1], Length(defaultMapDef));
      mdfo.Destroy();
      Halt(0);
    end;

    if (arg = '--pixel-scale') or (arg = '-pixel-scale') then
    begin
      if idx < ParamCount() then
      begin
        idx += 1;
        if not conParseFloat(r_pixel_scale, ParamStr(idx)) then
          r_pixel_scale := 1.0;
      end;
    end;

    idx += 1;
  end;

{$IFNDEF USE_SYSSTUB}
  PrintGLSupportedExtensions();
  glLegacyNPOT := not (GLExtensionSupported('GL_ARB_texture_non_power_of_two') or
    GLExtensionSupported('GL_OES_texture_npot'));
{$ELSE}
  glLegacyNPOT := False;
  glRenderToFBO := False;
{$ENDIF}
  if glNPOTOverride and glLegacyNPOT then
  begin
    glLegacyNPOT := True;
    e_logWriteln('NPOT texture emulation: FORCED');
  end
  else
  begin
    if glLegacyNPOT
      then e_logWriteln('NPOT texture emulation: enabled')
      else e_logWriteln('NPOT texture emulation: disabled');
  end;

  Init();
  Time_Old := sys_GetTicks();

  g_Net_InitLowLevel();

  // Командная строка
  if ParamCount > 0 then g_Game_Process_Params();

{$IFNDEF HEADLESS}
  // Запрос языка
  if not gGameOn and gAskLanguage then g_Menu_AskLanguage();
{$ENDIF}

  e_WriteLog('Entering the main loop', TMsgType.Notify);

  // main loop
  repeat until ProcessMessage();

  g_Net_Slist_ShutdownAll();

  Release();

  g_Net_DeinitLowLevel();
  Result := 0;
end;


initialization
  prevSoundTimeMs := GetTickCount64();
  conRegVar('d_input', @g_dbg_input, '', '')
end.
