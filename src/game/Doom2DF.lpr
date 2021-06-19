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
  ctypes, jni,
{$ENDIF}
{$IFDEF UNIX}
  cthreads, BaseUnix,
{$ENDIF}
{$IFDEF DARWIN}
  MacOSAll, CocoaAll,
{$ENDIF}
  mempool in '../shared/mempool.pas',
  conbuf in '../shared/conbuf.pas',
  geom in '../shared/geom.pas',
  math,

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
  e_input in '../engine/e_input.pas',
  e_log in '../engine/e_log.pas',
  e_sound in '../engine/e_sound.pas',
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
  g_base in 'g_base.pas',
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
  r_graphics in 'opengl/r_graphics.pas',
  r_items in 'opengl/r_items.pas',
  r_map in 'opengl/r_map.pas',
  r_monsters in 'opengl/r_monsters.pas',
  r_netmaster in 'opengl/r_netmaster.pas',
  r_panel in 'opengl/r_panel.pas',
  r_player in 'opengl/r_player.pas',
  r_playermodel in 'opengl/r_playermodel.pas',
  r_texture in 'opengl/r_texture.pas',
  r_weapons in 'opengl/r_weapons.pas',
  r_window in 'opengl/r_window.pas',
  r_render in 'opengl/r_render.pas',

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

  SysUtils, Classes;

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

  const
    autoexecScript = 'autoexec.cfg';

  var
    noct: Boolean = False;
    binPath: AnsiString = '';
    forceBinDir: Boolean = False;
    {$IFDEF USE_SDLMIXER}
      UseNativeMusic: Boolean;
    {$ENDIF}

    wLoadingQuit: Boolean = false;
    Time, Time_Delta, Time_Old: Int64;
    Frame: Int64;
    flag: Boolean = false;

    NoSound: Boolean;

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
    r_Game_Draw;
    sys_Repaint;
    Frame := Time
  end
  else
    sys_Delay(1);

  e_SoundUpdate();
end;

procedure DebugOptions;
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
end;

function GetBinaryPath (): AnsiString;
  {$IFDEF LINUX}
    var sl: AnsiString;
  {$ENDIF}
begin
  result := ExtractFilePath(ParamStr(0));
  {$IFDEF LINUX}
  // it may be a symlink; do some guesswork here
  sl := fpReadLink(ExtractFileName(ParamStr(0)));
  if (sl = ParamStr(0)) then
  begin
    // use current directory, as we don't have anything better
    //result := '.';
    GetDir(0, result);
  end;
  {$ENDIF}
  result := fixSlashes(result);
  if (length(result) > 0) and (result[length(result)] <> '/') then
    result := result + '/';
end;

procedure PrintDirs (msg: AnsiString; dirs: SSArray);
  var dir: AnsiString;
begin
  e_LogWriteln(msg + ':');
  for dir in dirs do
    e_LogWriteln('  ' + dir);
end;

{$IFDEF DARWIN}
  function NSStringToAnsiString (s: NSString): AnsiString;
    var i: Integer;
  begin
    result := '';
    for i := 0 to s.length - 1 do
      result := result + AnsiChar(s.characterAtIndex(i));
  end;

  function GetBundlePath (): AnsiString;
    var pathRef: CFURLRef; pathCFStr: CFStringRef; pathStr: ShortString;
  begin
    pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
    pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
    CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
    CFRelease(pathRef);
    CFRelease(pathCFStr);
    Result := pathStr;
  end;
{$ENDIF}

procedure InitPath;
  var i: Integer; rwdir, rodir: AnsiString; rwdirs, rodirs: SSArray;

  procedure AddDir (var dirs: SSArray; append: AnsiString);
  begin
    SetLength(dirs, Length(dirs) + 1);
    dirs[High(dirs)] := ExpandFileName(append)
  end;

  function IsSep (ch: Char): Boolean;
  begin
    {$IFDEF WINDOWS}
    result := (ch = '/') or (ch = '\');
    {$ELSE}
    result := (ch = '/');
    {$ENDIF}
  end;

  function OptimizePath (dir: AnsiString): AnsiString;
    var i, len: Integer; s: AnsiString;
  begin
    i := 1; len := Length(dir); s := '';
    while i <= len do
    begin
      if IsSep(dir[i]) then
      begin
        s := s + DirectorySeparator;
        Inc(i);
        while (i <= len) and IsSep(dir[i]) do Inc(i);
        if (i <= len) and (dir[i] = '.') then
        begin
          if (i = len) or IsSep(dir[i + 1]) then
          begin
            Inc(i)
          end
          else if (i + 1 <= len) and (dir[i + 1] = '.') then
          begin
            if (i + 1 = len) or IsSep(dir[i + 2]) then
            begin
              s := e_UpperDir(s);
              Inc(i, 2)
            end
          end
        end
      end
      else
      begin
        s := s + dir[i];
        Inc(i)
      end
    end;
    result := s
  end;

  procedure OptimizeDirs (var dirs: SSArray);
    var i, j, k: Integer;
  begin
    for i := 0 to High(dirs) do
      dirs[i] := OptimizePath(dirs[i]);
    // deduplicate
    i := High(dirs);
    while i >= 0 do
    begin
      j := 0;
      while j < i do
      begin
        if dirs[j] = dirs[i] then
        begin
          for k := j + 1 to High(dirs) do
            dirs[k - 1] := dirs[k];
          Dec(i);
          SetLength(dirs, High(dirs))
        end
        else
        begin
          Inc(j)
        end
      end;
      Dec(i)
    end
  end;

  procedure AddDef (var dirs: SSArray; base: SSArray; append: AnsiString);
    var s: AnsiString;
  begin
    if Length(dirs) = 0 then
      for s in base do
        AddDir(dirs, e_CatPath(s, append));
    OptimizeDirs(dirs)
  end;

  function GetDefaultRODirs (): SSArray;
    {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(ANDROID)}
      var home: AnsiString;
    {$ENDIF}
    {$IFDEF WINDOWS}
      var appdata: AnsiString;
    {$ENDIF}
    {$IFDEF DARWIN}
      var bundle, s: AnsiString; dirArr: NSArray; i: Integer;
    {$ENDIF}
  begin
    result := nil;
    {$IFDEF DARWIN}
      bundle := GetBundlePath();
      if ExtractFileExt(bundle) <> '.app' then
        AddDir(result, binpath);
    {$ELSE}
      AddDir(result, binPath);
    {$ENDIF}
    if forceBinDir = false then
    begin
      {$IFDEF USE_SDL2}
        AddDir(result, SDL_GetBasePath());
        AddDir(result, SDL_GetPrefPath('', 'doom2df'));
      {$ENDIF}
      {$IFDEF WINDOWS}
        appdata := GetEnvironmentVariable('APPDATA') + '\doom2df';
        if appdata <> '' then
          AddDir(result, appdata);
      {$ENDIF}
      {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(ANDROID)}
        AddDir(result, '/usr/share/doom2df');
        AddDir(result, '/usr/local/share/doom2df');
        home := GetEnvironmentVariable('HOME');
        if home <> '' then
          AddDir(result, e_CatPath(home, '.doom2df'));
      {$ENDIF}
      {$IFDEF DARWIN}
        bundle := GetBundlePath();
        if bundle <> '' then
          AddDir(result, e_CatPath(bundle, 'Contents/Resources'));
        dirArr := NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, true);
        for i := 0 to dirArr.count - 1 do
        begin
          s := NSStringToAnsiString(dirArr.objectAtIndex(i));
          AddDir(result, e_CatPath(s, 'Doom 2D Forever'))
        end;
      {$ENDIF}
      {$IF DEFINED(ANDROID) AND DEFINED(USE_SDL2)}
        AddDir(result, SDL_AndroidGetInternalStoragePath());
        if SDL_AndroidGetExternalStorageState() <> 0 then
          AddDir(result, SDL_AndroidGetExternalStoragePath());
      {$ENDIF}
    end
  end;

  function GetDefaultRWDirs (): SSArray;
    {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(ANDROID)}
      var home: AnsiString;
    {$ENDIF}
    {$IFDEF WINDOWS}
      var appdata: AnsiString;
    {$ENDIF}
    {$IFDEF DARWIN}
      var bundle, s: AnsiString; dirArr: NSArray; i: Integer;
    {$ENDIF}
  begin
    result := nil;
    {$IFDEF DARWIN}
      bundle := GetBundlePath();
      if ExtractFileExt(bundle) <> '.app' then
        AddDir(result, binPath);
    {$ELSE}
      AddDir(result, binPath);
    {$ENDIF}
    if forceBinDir = false then
    begin
      {$IFDEF USE_SDL2}
        AddDir(result, SDL_GetPrefPath('', 'doom2df'));
      {$ENDIF}
      {$IFDEF WINDOWS}
        appdata := GetEnvironmentVariable('APPDATA') + '\doom2df';
        if appdata <> '' then
          AddDir(result, appdata);
      {$ENDIF}
      {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(ANDROID)}
        home := GetEnvironmentVariable('HOME');
        if home <> '' then
          AddDir(result, e_CatPath(home, '.doom2df'));
      {$ENDIF}
      {$IFDEF DARWIN}
        dirArr := NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, true);
        for i := 0 to dirArr.count - 1 do
        begin
          s := NSStringToAnsiString(dirArr.objectAtIndex(i));
          AddDir(result, e_CatPath(s, 'Doom 2D Forever'))
        end;
      {$ENDIF}
      {$IF DEFINED(ANDROID) AND DEFINED(USE_SDL2)}
        if SDL_AndroidGetExternalStorageState() <> 0 then
          AddDir(result, SDL_AndroidGetExternalStoragePath());
      {$ENDIF}
    end
  end;

begin
  forceBinDir := false;
  binPath := GetBinaryPath();

  i := 1;
  while i < ParamCount do
  begin
    case ParamStr(i) of
    '--like-windoze': forceBinDir := true;
    '--rw-dir':
      begin
        Inc(i);
        rwdir := ParamStr(i);
        (* RW *)
        AddDir(LogDirs, e_CatPath(rwdir, ''));
        AddDir(SaveDirs, e_CatPath(rwdir, 'data'));
        AddDir(CacheDirs, e_CatPath(rwdir, 'data/cache'));
        AddDir(ConfigDirs, e_CatPath(rwdir, ''));
        AddDir(MapDownloadDirs, e_CatPath(rwdir, 'maps/downloads'));
        AddDir(WadDownloadDirs, e_CatPath(rwdir, 'wads/downloads'));
        AddDir(ScreenshotDirs, e_CatPath(rwdir, 'screenshots'));
        AddDir(StatsDirs, e_CatPath(rwdir, 'stats'));
        (* RO *)
        AddDir(DataDirs, e_CatPath(rwdir, 'data'));
        AddDir(ModelDirs, e_CatPath(rwdir, 'data/models'));
        AddDir(MegawadDirs, e_CatPath(rwdir, 'maps/megawads'));
        AddDir(MapDirs, e_CatPath(rwdir, 'maps'));
        AddDir(WadDirs, e_CatPath(rwdir, 'wads'));
      end;
    '--ro-dir':
      begin
        Inc(i);
        rodir := ParamStr(i);
        (* RO *)
        AddDir(DataDirs, e_CatPath(rodir, 'data'));
        AddDir(ModelDirs, e_CatPath(rodir, 'data/models'));
        AddDir(MegawadDirs, e_CatPath(rodir, 'maps/megawads'));
        AddDir(MapDirs, e_CatPath(rodir, 'maps'));
        AddDir(WadDirs, e_CatPath(rodir, 'wads'));
      end;
    '--game-wad':
      begin
        Inc(i);
        GameWADName := ParamStr(i);
      end;
    '--config':
      begin
        Inc(i);
        gConfigScript := ParamStr(i);
      end;
    end;
    Inc(i)
  end;

  // prefer bin dir if it writable and contains game.wad
  if forceBinDir = false then
  begin
    if findDiskWad(binPath + 'data' + '/' + GameWADName) <> '' then
      if e_CanCreateFilesAt(binPath) then
        forceBinDir := true
  end;

  (* RO *)
  rodirs := GetDefaultRODirs();
  AddDef(DataDirs, rodirs, 'data');
  AddDef(ModelDirs, rodirs, 'data/models');
  AddDef(MegawadDirs, rodirs, 'maps/megawads');
  AddDef(MapDirs, rodirs, 'maps');
  AddDef(WadDirs, rodirs, 'wads');

  (* RW *)
  rwdirs := GetDefaultRWDirs();
  AddDef(LogDirs, rwdirs, '');
  AddDef(SaveDirs, rwdirs, 'data');
  AddDef(CacheDirs, rwdirs, 'data/cache');
  AddDef(ConfigDirs, rwdirs, '');
  AddDef(MapDownloadDirs, rwdirs, 'maps/downloads');
  AddDef(WadDownloadDirs, rwdirs, 'wads/downloads');
  AddDef(ScreenshotDirs, rwdirs, 'screenshots');
  AddDef(StatsDirs, rwdirs, 'stats');

  for i := 0 to High(MapDirs) do
    AddDir(AllMapDirs, MapDirs[i]);
  for i := 0 to High(MegawadDirs) do
    AddDir(AllMapDirs, MegawadDirs[i]);
  OptimizeDirs(AllMapDirs);

  // HACK: ensure the screenshots folder also has a stats subfolder in it
  rwdir := e_GetWriteableDir(ScreenshotDirs, false);
  if rwdir <> '' then CreateDir(rwdir + '/stats');
end;

  procedure EntryParams;
    var i: Integer;
  begin
    i := 1;
    while i <= ParamCount do
    begin
      case ParamStr(i) of
      '--gdb': noct := true;
      '--log', '--con-stdout': conbufDumpToStdOut := true;
      '--safe-log': e_SetSafeSlowLog(true);
      '--log-file':
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          LogFileName := ParamStr(i)
        end;
        '--no-fbo': glRenderToFBO := false;
      end;
      Inc(i)
    end
  end;

  procedure InitLog;
    var rwdir: AnsiString;
  begin
    if LogFileName = '' then
    begin
      rwdir := e_GetWriteableDir(LogDirs, false);
      if rwdir <> '' then
      begin
        {$IFDEF HEADLESS}
          LogFileName := e_CatPath(rwdir, 'Doom2DF_H.log');
        {$ELSE}
          LogFileName := e_CatPath(rwdir, 'Doom2DF.log');
        {$ENDIF}
      end
    end;
    if LogFileName <> '' then
      e_InitLog(LogFileName, TWriteMode.WM_NEWFILE);
    e_InitWritelnDriver
  end;

  procedure InitPrep;
  begin
    e_WriteLog('Doom 2D: Forever version ' + GAME_VERSION + ' proto ' + IntToStr(NET_PROTOCOL_VER), TMsgType.Notify);
    e_WriteLog('Build arch: ' + g_GetBuildArch(), TMsgType.Notify);
    e_WriteLog('Build date: ' + GAME_BUILDDATE + ' ' + GAME_BUILDTIME, TMsgType.Notify);
    e_WriteLog('Build hash: ' + g_GetBuildHash(), TMsgType.Notify);
    e_WriteLog('Build by: ' + g_GetBuilderName(), TMsgType.Notify);

    e_LogWritefln('Force bin dir: %s', [forceBinDir], TMsgType.Notify);
    e_LogWritefln('BINARY PATH: [%s]', [binPath], TMsgType.Notify);

    PrintDirs('DataDirs', DataDirs);
    PrintDirs('ModelDirs', ModelDirs);
    PrintDirs('MegawadDirs', MegawadDirs);
    PrintDirs('MapDirs', MapDirs);
    PrintDirs('WadDirs', WadDirs);

    PrintDirs('LogDirs', LogDirs);
    PrintDirs('SaveDirs', SaveDirs);
    PrintDirs('CacheDirs', CacheDirs);
    PrintDirs('ConfigDirs', ConfigDirs);
    PrintDirs('ScreenshotDirs', ScreenshotDirs);
    PrintDirs('StatsDirs', StatsDirs);
    PrintDirs('MapDownloadDirs', MapDownloadDirs);
    PrintDirs('WadDownloadDirs', WadDownloadDirs);

    {$IFDEF HEADLESS}
      {$IFDEF USE_SDLMIXER}
        NoSound := False; // hope env has set SDL_AUDIODRIVER to dummy
      {$ELSE}
        NoSound := True; // FMOD backend will sort it out
      {$ENDIF}
    {$ELSE}
      NoSound := False;
    {$ENDIF}

    GameWAD := e_FindWad(DataDirs, GameWADName);
    if GameWad = '' then
    begin
      e_WriteLog('WAD ' + GameWADName + ' not found in data directories.', TMsgType.Fatal);
      {$IF DEFINED(USE_SDL2) AND NOT DEFINED(HEADLESS)}
        if forceBinDir = false then
          SDL_ShowSimpleMessageBox(
            SDL_MESSAGEBOX_ERROR,
            'Doom 2D Forever',
            PChar('WAD ' + GameWADName + ' not found in data directories.'),
            nil
          );
      {$ENDIF}
      e_DeinitLog;
      Halt(1);
    end
  end;

{$IFDEF ENABLE_HOLMES}
  procedure InitHolmes;
    var flexloaded: Boolean;
  begin
    flexloaded := true;
    if not fuiAddWad('flexui.wad') then
    begin
      if not fuiAddWad('./data/flexui.wad') then fuiAddWad('./flexui.wad');
    end;
    try
      fuiGfxLoadFont('win8', 'flexui/fonts/win8.fuifont');
      fuiGfxLoadFont('win14', 'flexui/fonts/win14.fuifont');
      fuiGfxLoadFont('win16', 'flexui/fonts/win16.fuifont');
      fuiGfxLoadFont('dos8', 'flexui/fonts/dos8.fuifont');
      fuiGfxLoadFont('msx6', 'flexui/fonts/msx6.fuifont');
    except on e: Exception do
      begin
        writeln('ERROR loading FlexUI fonts');
        flexloaded := false;
        //raise;
      end;
    else
      begin
        flexloaded := false;
        //raise;
      end;
    end;
    if flexloaded then
    begin
      try
        e_LogWriteln('FlexUI: loading stylesheet...');
        uiLoadStyles('flexui/widgets.wgs');
      except on e: TParserException do
        begin
          writeln('ERROR at (', e.tokLine, ',', e.tokCol, '): ', e.message);
          //raise;
          flexloaded := false;
        end;
      else
        begin
          //raise;
          flexloaded := false;
        end;
      end;
    end;
    g_holmes_imfunctional := not flexloaded;
    if not g_holmes_imfunctional then
    begin
      uiInitialize();
      uiContext.font := 'win14';
    end;
    if assigned(oglInitCB) then oglInitCB;
  end;

  procedure FreeHolmes;
  begin
    if assigned(oglDeinitCB) then
      oglDeinitCB
  end;
{$ENDIF}

  procedure InitSound;
    {$IFDEF USE_SDLMIXER}
      var timiditycfg: AnsiString;
      var oldcwd, newcwd: RawByteString;
    {$ENDIF}
  begin
    if NoSound = false then
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
  end;

  procedure ScreenResize (w, h: Integer);
  begin
    r_Render_Resize(w, h);
    {$IFDEF ENABLE_HOLMES}
      fuiScrWdt := w;
      fuiScrHgt := h;
    {$ENDIF}
    g_Game_SetupScreenSize;
    {$IFNDEF ANDROID}
      (* This will fix menu reset on keyboard showing *)
      g_Menu_Reset;
    {$ENDIF}
    //g_Game_ClearLoading;
    {$IFDEF ENABLE_HOLMES}
      if assigned(oglInitCB) then oglInitCB;
    {$ENDIF}
  end;

  procedure Startup;
  begin
    Randomize;
    InitPath;
    InitLog;
    InitPrep;
    e_Input_Initialize;
    InitSound;
    sys_Init;
    sys_CharPress := @CharPress; (* install hook *)
    sys_ScreenResize := @ScreenResize; (* install hook *)
    g_Options_SetDefault;
    g_Options_SetDefaultVideo;
    g_Console_Initialize;
    // TODO move load configs here
    g_Language_Set(gLanguage);
    r_Render_Initialize;
    g_Touch_Init;
    DebugOptions;
    g_Net_InitLowLevel;
    // TODO init serverlist
    {$IFDEF ENABLE_HOLMES}
      InitHolmes;
    {$ENDIF}
    g_Game_Init;
    {$IFNDEF HEADLESS}
      g_Menu_Init;
      g_GUI_Init;
    {$ENDIF}
    g_Game_Process_Params;
    // TODO reload GAME textures
    g_Console_Init; // welcome message
    {$IFNDEF HEADLESS}
      if (not gGameOn) and gAskLanguage then
        g_Menu_AskLanguage;
    {$ENDIF}
    Time_Old := sys_GetTicks();
    while not ProcessMessage() do begin end;
    g_Console_WriteGameConfig;
    {$IFNDEF HEADLESS}
      g_GUI_Destroy;
      g_Menu_Free;
    {$ENDIF}
    {$IFDEF ENABLE_HOLMES}
      FreeHolmes;
    {$ENDIF}
    g_Net_Slist_ShutdownAll;
    g_Net_DeinitLowLevel;
    (* g_Touch_Finalize; *)
    r_Render_Finalize;
    sys_Final;
    g_Console_Finalize;
    e_ReleaseSoundSystem;
    e_Input_Finalize;
    e_WriteLog('Shutdown with no errors.', TMsgType.Notify)
  end;

  procedure InitCVars;
  begin
    {$IFDEF USE_SDLMIXER}
      conRegVar('sdl_native_music', @UseNativeMusic, 'use native midi music output when possible', 'use native midi');
      {$IFDEF DARWIN}
        UseNativeMusic := true; (* OSX have a good midi support, so why not? *)
      {$ELSE}
        UseNativeMusic := false;
      {$ENDIF}
    {$ENDIF}
  end;

  procedure EntryPoint;
  begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]); //k8: fuck off, that's why
    InitCVars;
    EntryParams;
    e_Log_Initialize;
    {$IFDEF HEADLESS}
      conbufDumpToStdOut := true;
    {$ENDIF}
    if noct then
      Startup
    else
    try
      Startup
    except on e: Exception do
      e_WriteStackTrace(e.message)
    else
      e_WriteStackTrace('FATAL ERROR')
    end;
    e_Log_Finalize
  end;

{$IFDEF ANDROID}
  function SDL_main (argc: CInt; argv: PPChar): CInt; cdecl;
  begin
    {$IFDEF ANDROID}
      System.argc := argc;
      System.argv := argv;
    {$ENDIF}
    EntryPoint;
    result := 0
  end;

  function JNI_OnLoad (vm: PJavaVM; reserved: pointer): JInt; cdecl;
  begin
    result:= JNI_VERSION_1_6;
  end;

  procedure JNI_OnUnload(vm: PJavaVM; reserved: pointer); cdecl;
  begin
  end;

  // DONT REMOVE JNI FUNCTIONS. SPECIAL HANDLING BY FPC.
  exports SDL_main name 'SDL_main';
  exports JNI_OnLoad name 'JNI_OnLoad';
  exports JNI_OnUnload name 'JNI_Unload';
{$ELSE}
begin
  EntryPoint
{$ENDIF}

end.
