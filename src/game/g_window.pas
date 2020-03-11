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
unit g_window;

interface

uses
  utils;

function SDLMain (): Integer;
procedure ResetTimer ();
procedure ProcessLoading (forceUpdate: Boolean=false);

var
  gwin_dump_extensions: Boolean = false;
  gwin_has_stencil: Boolean = false;
  gwin_k8_enable_light_experiments: Boolean = false;
  g_dbg_aimline_on: Boolean = false;
  g_dbg_input: Boolean = False;

implementation

uses
{$IFDEF WINDOWS}Windows,{$ENDIF}
{$IFDEF ENABLE_HOLMES}
  g_holmes, sdlcarcass, fui_ctls,
{$ENDIF}
{$INCLUDE ../nogl/noGLuses.inc}
  SysUtils, Classes, MAPDEF, Math,
  e_graphics, e_log, e_texture, g_main,
  g_console, e_input, g_options, g_game,
  g_basic, g_textures, e_sound, g_sound, g_menu, ENet, g_net,
  g_map, g_gfx, g_monsters, xprofiler,
  g_touch, g_gui, g_system, g_netmaster;


const
  ProgressUpdateMSecs = 35; //1;//100;

var
  Time, Time_Delta, Time_Old: Int64;
  Frame: Int64;
  flag: Boolean;
  wNeedTimeReset: Boolean = false;
  wMinimized: Boolean = false;
  wLoadingQuit: Boolean = false;

procedure ResetTimer ();
begin
  wNeedTimeReset := true;
end;

{$IFNDEF HEADLESS}
var
  prevLoadingUpdateTime: UInt64 = 0;
{$ENDIF}

procedure ProcessLoading (forceUpdate: Boolean=false);
{$IFNDEF HEADLESS}
var
  stt: UInt64;
{$ENDIF}
begin
  if sys_HandleInput() = True then
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

  e_SoundUpdate();

  if NetMode = NET_SERVER then
  begin
    g_Net_Host_Update();
  end
  else
  begin
    if (NetMode = NET_CLIENT) and (NetState <> NET_STATE_AUTH) then g_Net_Client_UpdateWhileLoading();
  end;
end;


function ProcessMessage (): Boolean;
var
  i, t: Integer;
begin
  result := sys_HandleInput();

  Time := sys_GetTicks();
  Time_Delta := Time-Time_Old;

  flag := false;

  if wNeedTimeReset then
  begin
    Frame := 0;
    Time_Delta := 28;
    wNeedTimeReset := false;
  end;

  g_Map_ProfilersBegin();
  g_Mons_ProfilersBegin();

  t := Time_Delta div 28;
  if (t > 0) then
  begin
    flag := true;
    for i := 1 to t do
    begin
           if (NetMode = NET_SERVER) then g_Net_Host_Update()
      else if (NetMode = NET_CLIENT) then g_Net_Client_Update();
      Update();
    end;
  end;

  if NetMode = NET_SERVER then g_Net_Flush();

  g_Map_ProfilersEnd();
  g_Mons_ProfilersEnd();

  if wLoadingQuit then
  begin
    g_Game_Free();
    g_Game_Quit();
  end;

  if (gExit = EXIT_QUIT) then
  begin
    result := true;
    exit;
  end;

  // Время предыдущего обновления
  if flag then
    Time_Old := Time - (Time_Delta mod 28);

  // don't wait if VSync is on, GL already probably waits enough
  if gLerpActors then
    flag := (Time - Frame >= gFrameTime) or gVSync;

  if flag then
  begin
    if (not wMinimized) then
    begin
      if gPause or (not gLerpActors) or (gState = STATE_FOLD) then
        gLerpFactor := 1.0
      else
        gLerpFactor := nmin(1.0, (Time - Time_Old) / 28.0);
      Draw;
      sys_Repaint
    end;
    Frame := Time
  end
  else
    sys_Delay(1);

  e_SoundUpdate();
end;

function GLExtensionList (): SSArray;
var
  s: PChar;
  i, j, num: GLint;
begin
  result := nil;
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
  result := false;
  exts := GLExtensionList();
  for e in exts do
  begin
    //writeln('<', e, '> : [', ext, '] = ', strEquCI1251(e, ext));
    if (strEquCI1251(e, ext)) then begin result := true; exit; end;
  end;
end;

procedure PrintGLSupportedExtensions;
begin
  e_LogWritefln('GL Vendor: %s', [glGetString(GL_VENDOR)]);
  e_LogWritefln('GL Renderer: %s', [glGetString(GL_RENDERER)]);
  e_LogWritefln('GL Version: %s', [glGetString(GL_VERSION)]);
  e_LogWritefln('GL Shaders: %s', [glGetString(GL_SHADING_LANGUAGE_VERSION)]);
  e_LogWritefln('GL Extensions: %s', [glGetString(GL_EXTENSIONS)]);
end;

function SDLMain (): Integer;
var
  idx: Integer;
  arg: AnsiString;
  mdfo: TStream;
  {$IFDEF ENABLE_HOLMES}
  itmp: Integer;
  valres: Word;
  {$ENDIF}
begin
{$IFDEF HEADLESS}
  e_NoGraphics := true;
{$ENDIF}

  idx := 1;
  while (idx <= ParamCount) do
  begin
    arg := ParamStr(idx);
    Inc(idx);
    if arg = '--opengl-dump-exts' then gwin_dump_extensions := true;
    //if arg = '--twinkletwinkle' then gwin_k8_enable_light_experiments := true;
    if arg = '--jah' then g_profile_history_size := 100;
    if arg = '--no-particles' then gpart_dbg_enabled := false;
    if arg = '--no-los' then gmon_dbg_los_enabled := false;

    if arg = '--profile-render' then g_profile_frame_draw := true;
    if arg = '--profile-coldet' then g_profile_collision := true;
    if arg = '--profile-los' then g_profile_los := true;

    if arg = '--no-part-phys' then gpart_dbg_phys_enabled := false;
    if arg = '--no-part-physics' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particles-phys' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particles-physics' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particle-phys' then gpart_dbg_phys_enabled := false;
    if arg = '--no-particle-physics' then gpart_dbg_phys_enabled := false;

    if arg = '--debug-input' then g_dbg_input := True;

    {.$IF DEFINED(D2F_DEBUG)}
    if arg = '--aimline' then g_dbg_aimline_on := true;
    {.$ENDIF}

{$IFDEF ENABLE_HOLMES}
    if arg = '--holmes' then begin g_holmes_enabled := true; g_Game_SetDebugMode(); end;

    if (arg = '--holmes-ui-scale') or (arg = '-holmes-ui-scale') then
    begin
      if (idx <= ParamCount) then
      begin
        if not conParseFloat(fuiRenderScale, ParamStr(idx)) then fuiRenderScale := 1.0;
        Inc(idx);
      end;
    end;

    if (arg = '--holmes-font') or (arg = '-holmes-font') then
    begin
      if (idx <= ParamCount) then
      begin
        itmp := 0;
        val(ParamStr(idx), itmp, valres);
        {$IFNDEF HEADLESS}
        if (valres = 0) and (not g_holmes_imfunctional) then
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
        Inc(idx);
      end;
    end;
{$ENDIF}

    if (arg = '--game-scale') or (arg = '-game-scale') then
    begin
      if (idx <= ParamCount) then
      begin
        if not conParseFloat(g_dbg_scale, ParamStr(idx)) then g_dbg_scale := 1.0;
        Inc(idx);
      end;
    end;

    if (arg = '--write-mapdef') or (arg = '-write-mapdef') then
    begin
      mdfo := createDiskFile('mapdef.txt');
      mdfo.WriteBuffer(defaultMapDef[1], Length(defaultMapDef));
      mdfo.Free();
      Halt(0);
    end;

    if (arg = '--pixel-scale') or (arg = '-pixel-scale') then
    begin
      if (idx <= ParamCount) then
      begin
        if not conParseFloat(r_pixel_scale, ParamStr(idx)) then r_pixel_scale := 1.0;
        Inc(idx);
      end;
    end;
  end;

{$IFNDEF USE_SYSSTUB}
  PrintGLSupportedExtensions;
  glLegacyNPOT := not (GLExtensionSupported('GL_ARB_texture_non_power_of_two') or GLExtensionSupported('GL_OES_texture_npot'));
{$ELSE}
  glLegacyNPOT := False;
  glRenderToFBO := False;
{$ENDIF}
  if glNPOTOverride and glLegacyNPOT then
  begin
    glLegacyNPOT := true;
    e_logWriteln('NPOT texture emulation: FORCED');
  end
  else
  begin
    if (glLegacyNPOT) then e_logWriteln('NPOT texture emulation: enabled')
    else e_logWriteln('NPOT texture emulation: disabled');
  end;
  gwin_dump_extensions := false;

  Init;
  Time_Old := sys_GetTicks();

  g_Net_InitLowLevel();

  // Командная строка
  if (ParamCount > 0) then g_Game_Process_Params();

{$IFNDEF HEADLESS}
  // Запрос языка
  if (not gGameOn) and gAskLanguage then g_Menu_AskLanguage();
{$ENDIF}

  e_WriteLog('Entering the main loop', TMsgType.Notify);

  // main loop
  while not ProcessMessage() do begin end;

  g_Net_Slist_ShutdownAll();

  Release();

  g_Net_DeinitLowLevel();
  result := 0;
end;


initialization
  conRegVar('d_input', @g_dbg_input, '', '')
end.
