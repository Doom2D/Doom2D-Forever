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
unit g_main;

interface

  uses Utils;

procedure Main ();
procedure Init ();
procedure Release ();
procedure Update ();
procedure Draw ();
procedure KeyPress (K: Word);
procedure CharPress (C: AnsiChar);

var
  {--- Read-only dirs ---}
  GameWAD: string;
  DataDirs: SSArray;
  ModelDirs: SSArray;
  MegawadDirs: SSArray;
  MapDirs: SSArray;
  WadDirs: SSArray;
  AllMapDirs: SSArray; // Maps + Megawads

  {--- Read-Write dirs ---}
  LogFileName: string;
  LogDirs: SSArray;
  SaveDirs: SSArray;
  CacheDirs: SSArray;
  ConfigDirs: SSArray;
  ScreenshotDirs: SSArray;
  StatsDirs: SSArray;
  MapDownloadDirs: SSArray;
  WadDownloadDirs: SSArray;

  GameWADName: string = 'GAME';

implementation

uses
{$INCLUDE ../nogl/noGLuses.inc}
{$IFDEF ENABLE_HOLMES}
  g_holmes, sdlcarcass, fui_ctls, fui_wadread, fui_style, fui_gfx_gl,
{$ENDIF}
{$IFDEF LINUX}
  BaseUnix,
{$ENDIF}
{$IFDEF DARWIN}
  MacOSAll, CocoaAll,
{$ENDIF}
{$IFDEF USE_SDL2}
  SDL2,
{$ENDIF}
  wadreader, e_log, g_window,
  e_graphics, e_input, g_game, g_console, g_gui,
  e_sound, g_options, g_sound, g_player, g_basic,
  g_weapons, SysUtils, g_triggers, MAPDEF, g_map, e_res,
  g_menu, g_language, g_net, g_touch, g_system, g_res_downloader,
  conbuf, envvars, r_game,
  xparser;


var
  charbuff: packed array [0..15] of AnsiChar;
  binPath: AnsiString = '';
  forceBinDir: Boolean;
  {$IFDEF USE_SDLMIXER}
    UseNativeMusic: Boolean;
  {$ENDIF}

function GetBinaryPath (): AnsiString;
{$IFDEF LINUX}
var
  //cd: AnsiString;
  sl: AnsiString;
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
  if (length(result) > 0) and (result[length(result)] <> '/') then result := result+'/';
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
  
  // HACK: ensure the screenshots folder also has a stats subfolder in it
  rwdir := e_GetWriteableDir(ScreenshotDirs, false);
  if rwdir <> '' then CreateDir(rwdir + '/stats');
end;

procedure InitPrep;
  var i: Integer;
begin
  {$IFDEF HEADLESS}
    conbufDumpToStdOut := true;
  {$ENDIF}
  for i := 1 to ParamCount do
  begin
    case ParamStr(i) of
      '--con-stdout': conbufDumpToStdOut := true;
      '--no-fbo': glRenderToFBO := false;
    end
  end;

  if LogFileName <> '' then
    e_InitLog(LogFileName, TWriteMode.WM_NEWFILE);
  e_InitWritelnDriver();
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
  end;
end;

procedure Main();
{$IFDEF ENABLE_HOLMES}
  var flexloaded: Boolean;
{$ENDIF}
begin
  InitPath;
  InitPrep;
  e_InitInput;
  sys_Init;

  g_Options_SetDefault;
  g_Options_SetDefaultVideo;
  g_Console_SysInit;
  if sys_SetDisplayMode(gRC_Width, gRC_Height, gBPP, gRC_FullScreen, gRC_Maximized) = False then
    raise Exception.Create('Failed to set videomode on startup.');

  e_WriteLog(gLanguage, TMsgType.Notify);
  g_Language_Set(gLanguage);

{$IF not DEFINED(HEADLESS) and DEFINED(ENABLE_HOLMES)}
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
  if (flexloaded) then
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

  if (not g_holmes_imfunctional) then
  begin
    uiInitialize();
    uiContext.font := 'win14';
  end;

  if assigned(oglInitCB) then oglInitCB;
{$ENDIF}

  //g_Res_CreateDatabases(true); // it will be done before connecting to the server for the first time

  e_WriteLog('Entering SDLMain', TMsgType.Notify);

  {$WARNINGS OFF}
    SDLMain();
  {$WARNINGS ON}

  {$IFDEF ENABLE_HOLMES}
    if assigned(oglDeinitCB) then oglDeinitCB;
  {$ENDIF}

  g_Console_WriteGameConfig;
  sys_Final;
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

  FillChar(charbuff, sizeof(charbuff), ' ');
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


function Translit (const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    case Result[i] of
      'É': Result[i] := 'Q';
      'Ö': Result[i] := 'W';
      'Ó': Result[i] := 'E';
      'Ê': Result[i] := 'R';
      'Å': Result[i] := 'T';
      'Í': Result[i] := 'Y';
      'Ã': Result[i] := 'U';
      'Ø': Result[i] := 'I';
      'Ù': Result[i] := 'O';
      'Ç': Result[i] := 'P';
      'Õ': Result[i] := '['; //Chr(219);
      'Ú': Result[i] := ']'; //Chr(221);
      'Ô': Result[i] := 'A';
      'Û': Result[i] := 'S';
      'Â': Result[i] := 'D';
      'À': Result[i] := 'F';
      'Ï': Result[i] := 'G';
      'Ð': Result[i] := 'H';
      'Î': Result[i] := 'J';
      'Ë': Result[i] := 'K';
      'Ä': Result[i] := 'L';
      'Æ': Result[i] := ';'; //Chr(186);
      'Ý': Result[i] := #39; //Chr(222);
      'ß': Result[i] := 'Z';
      '×': Result[i] := 'X';
      'Ñ': Result[i] := 'C';
      'Ì': Result[i] := 'V';
      'È': Result[i] := 'B';
      'Ò': Result[i] := 'N';
      'Ü': Result[i] := 'M';
      'Á': Result[i] := ','; //Chr(188);
      'Þ': Result[i] := '.'; //Chr(190);
    end;
  end;
end;


function CheckCheat (ct: TStrings_Locale; eofs: Integer=0): Boolean;
var
  ls1, ls2: string;
begin
  ls1 :=          CheatEng[ct];
  ls2 := Translit(CheatRus[ct]);
  if length(ls1) = 0 then ls1 := '~';
  if length(ls2) = 0 then ls2 := '~';
  result :=
    (Copy(charbuff, 17-Length(ls1)-eofs, Length(ls1)) = ls1) or
    (Translit(Copy(charbuff, 17-Length(ls1)-eofs, Length(ls1))) = ls1) or
    (Copy(charbuff, 17-Length(ls2)-eofs, Length(ls2)) = ls2) or
    (Translit(Copy(charbuff, 17-Length(ls2)-eofs, Length(ls2))) = ls2);
  {
  if ct = I_GAME_CHEAT_JETPACK then
  begin
    e_WriteLog('ls1: ['+ls1+']', MSG_NOTIFY);
    e_WriteLog('ls2: ['+ls2+']', MSG_NOTIFY);
    e_WriteLog('bf0: ['+Copy(charbuff, 17-Length(ls1)-eofs, Length(ls1))+']', MSG_NOTIFY);
    e_WriteLog('bf1: ['+Translit(Copy(charbuff, 17-Length(ls1)-eofs, Length(ls1)))+']', MSG_NOTIFY);
    e_WriteLog('bf2: ['+Copy(charbuff, 17-Length(ls2)-eofs, Length(ls2))+']', MSG_NOTIFY);
    e_WriteLog('bf3: ['+Translit(Copy(charbuff, 17-Length(ls2)-eofs, Length(ls2)))+']', MSG_NOTIFY);
  end;
  }
end;


procedure Cheat ();
const
  CHEAT_DAMAGE = 500;
label
  Cheated;
var
  s, s2: string;
  c: ShortString;
  a: Integer;
begin
  {
  if (not gGameOn) or (not gCheats) or ((gGameSettings.GameType <> GT_SINGLE) and
    (gGameSettings.GameMode <> GM_COOP) and (not gDebugMode))
    or g_Game_IsNet then Exit;
  }
  if not gGameOn then exit;
  if not conIsCheatsEnabled then exit;

  s := 'SOUND_GAME_RADIO';

  //
  if CheckCheat(I_GAME_CHEAT_GODMODE) then
  begin
    if gPlayer1 <> nil then gPlayer1.GodMode := not gPlayer1.GodMode;
    if gPlayer2 <> nil then gPlayer2.GodMode := not gPlayer2.GodMode;
    goto Cheated;
  end;
  // RAMBO
  if CheckCheat(I_GAME_CHEAT_WEAPONS) then
  begin
    if gPlayer1 <> nil then gPlayer1.AllRulez(False);
    if gPlayer2 <> nil then gPlayer2.AllRulez(False);
    goto Cheated;
  end;
  // TANK
  if CheckCheat(I_GAME_CHEAT_HEALTH) then
  begin
    if gPlayer1 <> nil then gPlayer1.AllRulez(True);
    if gPlayer2 <> nil then gPlayer2.AllRulez(True);
    goto Cheated;
  end;
  // IDDQD
  if CheckCheat(I_GAME_CHEAT_DEATH) then
  begin
    if gPlayer1 <> nil then gPlayer1.Damage(CHEAT_DAMAGE, 0, 0, 0, HIT_TRAP);
    if gPlayer2 <> nil then gPlayer2.Damage(CHEAT_DAMAGE, 0, 0, 0, HIT_TRAP);
    s := 'SOUND_MONSTER_HAHA';
    goto Cheated;
  end;
  //
  if CheckCheat(I_GAME_CHEAT_DOORS) then
  begin
    g_Triggers_OpenAll();
    goto Cheated;
  end;
  // GOODBYE
  if CheckCheat(I_GAME_CHEAT_NEXTMAP) then
  begin
    if gTriggers <> nil then
      for a := 0 to High(gTriggers) do
        if gTriggers[a].TriggerType = TRIGGER_EXIT then
        begin
          gExitByTrigger := True;
          //g_Game_ExitLevel(gTriggers[a].Data.MapName);
          g_Game_ExitLevel(gTriggers[a].tgcMap);
          Break;
        end;
    goto Cheated;
  end;
  //
  s2 := Copy(charbuff, 15, 2);
  if CheckCheat(I_GAME_CHEAT_CHANGEMAP, 2) and (s2[1] >= '0') and (s2[1] <= '9') and (s2[2] >= '0') and (s2[2] <= '9') then
  begin
    if g_Map_Exist(gGameSettings.WAD + ':\MAP' + s2) then
    begin
      c := 'MAP' + s2;
      g_Game_ExitLevel(c);
    end;
    goto Cheated;
  end;
  //
  if CheckCheat(I_GAME_CHEAT_FLY) then
  begin
    gFly := not gFly;
    goto Cheated;
  end;
  // BULLFROG
  if CheckCheat(I_GAME_CHEAT_JUMPS) then
  begin
    VEL_JUMP := 30-VEL_JUMP;
    goto Cheated;
  end;
  // FORMULA1
  if CheckCheat(I_GAME_CHEAT_SPEED) then
  begin
    MAX_RUNVEL := 32-MAX_RUNVEL;
    goto Cheated;
  end;
  // CONDOM
  if CheckCheat(I_GAME_CHEAT_SUIT) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_SUIT);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_SUIT);
    goto Cheated;
  end;
  //
  if CheckCheat(I_GAME_CHEAT_AIR) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_OXYGEN);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_OXYGEN);
    goto Cheated;
  end;
  // PURELOVE
  if CheckCheat(I_GAME_CHEAT_BERSERK) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_MEDKIT_BLACK);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_MEDKIT_BLACK);
    goto Cheated;
  end;
  //
  if CheckCheat(I_GAME_CHEAT_JETPACK) then
  begin
    if gPlayer1 <> nil then gPlayer1.GiveItem(ITEM_JETPACK);
    if gPlayer2 <> nil then gPlayer2.GiveItem(ITEM_JETPACK);
    goto Cheated;
  end;
  // CASPER
  if CheckCheat(I_GAME_CHEAT_NOCLIP) then
  begin
    if gPlayer1 <> nil then gPlayer1.SwitchNoClip;
    if gPlayer2 <> nil then gPlayer2.SwitchNoClip;
    goto Cheated;
  end;
  //
  if CheckCheat(I_GAME_CHEAT_NOTARGET) then
  begin
    if gPlayer1 <> nil then gPlayer1.NoTarget := not gPlayer1.NoTarget;
    if gPlayer2 <> nil then gPlayer2.NoTarget := not gPlayer2.NoTarget;
    goto Cheated;
  end;
  // INFERNO
  if CheckCheat(I_GAME_CHEAT_NORELOAD) then
  begin
    if gPlayer1 <> nil then gPlayer1.NoReload := not gPlayer1.NoReload;
    if gPlayer2 <> nil then gPlayer2.NoReload := not gPlayer2.NoReload;
    goto Cheated;
  end;
  if CheckCheat(I_GAME_CHEAT_AIMLINE) then
  begin
    gAimLine := not gAimLine;
    goto Cheated;
  end;
  if CheckCheat(I_GAME_CHEAT_AUTOMAP) then
  begin
    gShowMap := not gShowMap;
    goto Cheated;
  end;
  Exit;

Cheated:
  g_Sound_PlayEx(s);
end;


procedure KeyPress (K: Word);
{$IFNDEF HEADLESS}
var
  Msg: g_gui.TMessage;
{$ENDIF}
begin
{$IFNDEF HEADLESS}
  case K of
    VK_ESCAPE: // <Esc>:
      begin
        if (g_ActiveWindow <> nil) then
        begin
          Msg.Msg := WM_KEYDOWN;
          Msg.WParam := VK_ESCAPE;
          g_ActiveWindow.OnMessage(Msg);
          if (not g_Game_IsNet) and (g_ActiveWindow = nil) then g_Game_Pause(false); //Fn loves to do this
        end
        else if (gState <> STATE_FOLD) then
        begin
          if gGameOn or (gState = STATE_INTERSINGLE) or (gState = STATE_INTERCUSTOM) then
          begin
            g_Game_InGameMenu(True);
          end
          else if (gExit = 0) and (gState <> STATE_SLIST) then
          begin
            if (gState <> STATE_MENU) then
            begin
              if (NetMode <> NET_NONE) then
              begin
                g_Game_StopAllSounds(True);
                g_Game_Free;
                gState := STATE_MENU;
                Exit;
              end;
            end;
            g_GUI_ShowWindow('MainMenu');
            g_Sound_PlayEx('MENU_OPEN');
          end;
        end;
      end;

    IK_F2, IK_F3, IK_F4, IK_F5, IK_F6, IK_F7, IK_F10:
      begin // <F2> .. <F6> ï¿½ <F12>
        if gGameOn and (not gConsoleShow) and (not gChatShow) then
        begin
          while (g_ActiveWindow <> nil) do g_GUI_HideWindow(False);
          if (not g_Game_IsNet) then g_Game_Pause(True);
          case K of
            IK_F2: g_Menu_Show_SaveMenu();
            IK_F3: g_Menu_Show_LoadMenu();
            IK_F4: g_Menu_Show_GameSetGame();
            IK_F5: g_Menu_Show_OptionsVideo();
            IK_F6: g_Menu_Show_OptionsSound();
            IK_F7: g_Menu_Show_EndGameMenu();
            IK_F10: g_Menu_Show_QuitGameMenu();
          end;
        end;
      end;

    else
      begin
        gJustChatted := False;
        if gConsoleShow or gChatShow then
        begin
          g_Console_Control(K);
        end
        else if (g_ActiveWindow <> nil) then
        begin
          Msg.Msg := WM_KEYDOWN;
          Msg.WParam := K;
          g_ActiveWindow.OnMessage(Msg);
        end
        else if (gState = STATE_MENU) then
        begin
          g_GUI_ShowWindow('MainMenu');
          g_Sound_PlayEx('MENU_OPEN');
        end;
      end;
  end;
{$ENDIF}
end;


procedure CharPress (C: AnsiChar);
var
  Msg: g_gui.TMessage;
  a: Integer;
begin
  if gConsoleShow or gChatShow then
  begin
    g_Console_Char(C)
  end
  else if (g_ActiveWindow <> nil) then
  begin
    Msg.Msg := WM_CHAR;
    Msg.WParam := Ord(C);
    g_ActiveWindow.OnMessage(Msg);
  end
  else
  begin
    for a := 0 to 14 do charbuff[a] := charbuff[a+1];
    charbuff[15] := upcase1251(C);
    Cheat();
  end;
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
end.
