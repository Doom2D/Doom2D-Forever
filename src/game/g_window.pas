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
procedure ProcessLoading (forceUpdate: Boolean = False);

var
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
  SysUtils, Classes, MAPDEF, Math, r_graphics,
  r_window, e_log, e_res, envvars, r_game,
  g_console, r_console, e_input, g_options, g_game,
  g_basic, g_textures, e_sound, g_sound, g_menu, ENet, g_net,
  g_map, g_gfx, g_monsters, xprofiler,
  g_touch, g_gui, g_system, g_netmaster;

var
  Time, Time_Delta, Time_Old: Int64;
  Frame: Int64;
  flag: Boolean;
  wNeedTimeReset: Boolean = false;
  wLoadingQuit: Boolean = false;

  {$IFDEF USE_SDLMIXER}
    UseNativeMusic: Boolean;
  {$ENDIF}

procedure Update ();
begin
  // remember old mobj positions, prepare for update
  g_Game_PreUpdate();
  // server: receive client commands for new frame
  // client: receive game state changes from server
       if (NetMode = NET_SERVER) then g_Net_Host_Update()
  else if (NetMode = NET_CLIENT) then g_Net_Client_Update();
  // think
  g_Game_Update();
  // server: send any accumulated outgoing data to clients
  if NetMode = NET_SERVER then g_Net_Flush();
end;


procedure Draw ();
begin
  r_Game_Draw();
end;


procedure Init();
  {$IFDEF USE_SDLMIXER}
    var timiditycfg: AnsiString;
    var oldcwd, newcwd: RawByteString;
  {$ENDIF}
  var NoSound: Boolean;
begin
  Randomize;

{$IFDEF HEADLESS}
 {$IFDEF USE_SDLMIXER}
  NoSound := False; // hope env has set SDL_AUDIODRIVER to dummy
 {$ELSE}
  NoSound := True; // FMOD backend will sort it out
 {$ENDIF}
{$ELSE}
  NoSound := False;
{$ENDIF}

  g_Touch_Init;

(*
  if (e_JoysticksAvailable > 0) then
    e_WriteLog('Input: Joysticks available.', TMsgType.Notify)
  else
    e_WriteLog('Input: No Joysticks.', TMsgType.Notify);
*)

  if gNoSound = false then
  begin
    e_WriteLog('Initializing sound system', TMsgType.Notify);
    {$IFDEF USE_SDLMIXER}
      newcwd := '';
      if UseNativeMusic then
        SetEnvVar('SDL_NATIVE_MUSIC', '1');
      timiditycfg := GetEnvironmentVariable('TIMIDITY_CFG');
      if timiditycfg = '' then
      begin
        timiditycfg := 'timidity.cfg';
        if e_FindResource(ConfigDirs, timiditycfg) OR e_FindResource(DataDirs, timiditycfg) then
        begin
          timiditycfg := ExpandFileName(timiditycfg);
          newcwd := ExtractFileDir(timiditycfg);
          SetEnvVar('TIMIDITY_CFG', timiditycfg);
        end
        else
          timiditycfg := '';
      end;
      e_LogWritefln('TIMIDITY_CFG = "%s"', [timiditycfg]);
      e_LogWritefln('SDL_NATIVE_MUSIC = "%s"', [GetEnvironmentVariable('SDL_NATIVE_MUSIC')]);
    {$ENDIF}
    e_InitSoundSystem(NoSound);
    {$IFDEF USE_SDLMIXER}
      if e_TimidityDecoder and (newcwd <> '') then
      begin
        (* HACK: Set CWD to load GUS patches relatively to cfg file. *)
        (*       CWD not restored after sound init because timidity  *)
        (*       store relative pathes internally and load patches   *)
        (*       later. I hope game never relies on CWD.             *)
        oldcwd := '';
        GetDir(0, oldcwd);
        ChDir(newcwd);
        e_logwritefln('WARNING: USED TIMIDITY CONFIG HACK, CWD SWITCHED "%s" -> "%s"', [oldcwd, newcwd]);
      end;
    {$ENDIF}
  end;

  e_WriteLog('Init game', TMsgType.Notify);
  g_Game_Init();

//  FillChar(charbuff, sizeof(charbuff), ' ');
end;

procedure Release();
begin
  e_WriteLog('Releasing engine', TMsgType.Notify);
  e_ReleaseEngine();

  e_WriteLog('Releasing input', TMsgType.Notify);
  e_ReleaseInput();

  if not gNoSound then
  begin
    e_WriteLog('Releasing sound', TMsgType.Notify);
    e_ReleaseSoundSystem();
  end;
end;

procedure ResetTimer ();
begin
  wNeedTimeReset := true;
end;

procedure ProcessLoading (forceUpdate: Boolean=false);
begin
  if sys_HandleInput() = True then
    Exit;

{$IFNDEF HEADLESS}
  r_Window_DrawLoading(forceUpdate);
{$ENDIF}

  e_SoundUpdate();

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
      Update();
  end;

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
    if gPause or (not gLerpActors) or (gState = STATE_FOLD) then
      gLerpFactor := 1.0
    else
      gLerpFactor := nmin(1.0, (Time - Time_Old) / 28.0);
    Draw;
    sys_Repaint;
    Frame := Time
  end
  else
    sys_Delay(1);

  e_SoundUpdate();
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

  idx := 1;
  while (idx <= ParamCount) do
  begin
    arg := ParamStr(idx);
    Inc(idx);
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

  r_Window_Initialize;

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
{$IFDEF USE_SDLMIXER}
  conRegVar('sdl_native_music', @UseNativeMusic, 'use native midi music output when possible', 'use native midi');
  {$IFDEF DARWIN}
    UseNativeMusic := true; (* OSX have a good midi support, so why not? *)
  {$ELSE}
    UseNativeMusic := false;
  {$ENDIF}
{$ENDIF}
  conRegVar('d_input', @g_dbg_input, '', '')
end.
