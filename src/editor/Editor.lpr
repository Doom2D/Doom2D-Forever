program Editor;

{$INCLUDE ../shared/a_modes.inc}

uses
  {$IFDEF DARWIN}
    MacOSAll, CocoaAll,
  {$ENDIF}
  Forms, Interfaces, Dialogs,
  GL, GLExt, SysUtils,
  e_graphics in '../engine/e_graphics.pas',
  e_log in '../engine/e_log.pas',
  e_textures in '../engine/e_textures.pas',
  MAPSTRUCT in '../shared/MAPSTRUCT.pas',
  MAPREADER in '../shared/MAPREADER.pas',
  MAPWRITER in '../shared/MAPWRITER.pas',
  MAPDEF in '../shared/MAPDEF.pas',
  WADEDITOR in '../shared/WADEDITOR.pas',
  WADSTRUCT in '../shared/WADSTRUCT.pas',
  CONFIG in '../shared/CONFIG.pas',
  f_about in 'f_about.pas' {AboutForm},
  f_options in 'f_options.pas' {OptionsForm},
  f_main in 'f_main.pas' {MainForm},
  g_map in 'g_map.pas',
  f_mapoptions in 'f_mapoptions.pas' {MapOptionsForm},
  f_activationtype in 'f_activationtype.pas' {ActivationTypeForm},
  f_addresource in 'f_addresource.pas' {AddResourceForm},
  f_keys in 'f_keys.pas' {KeysForm},
  f_mapcheck in 'f_mapcheck.pas' {MapCheckForm},
  f_mapoptimization in 'f_mapoptimization.pas' {MapOptimizationForm},
  g_basic in 'g_basic.pas',
  g_textures in 'g_textures.pas',
  f_addresource_texture in 'f_addresource_texture.pas' {AddTextureForm},
  f_savemap in 'f_savemap.pas' {SaveMapForm},
  f_selectmap in 'f_selectmap.pas' {SelectMapForm},
  f_addresource_sky in 'f_addresource_sky.pas' {AddSkyForm},
  f_addresource_sound in 'f_addresource_sound.pas' {AddSoundForm},
  spectrum in 'spectrum.pas',
  f_saveminimap in 'f_saveminimap.pas' {SaveMiniMapForm},
  f_packmap in 'f_packmap.pas' {PackMapForm},
  f_choosetype in 'f_choosetype.pas' {ChooseTypeForm},
{$IFNDEF NOSOUND}
  fmod,
  fmoderrors,
  fmodpresets,
  fmodtypes,
{$ENDIF}
  ImagingTypes,
  Imaging,
  ImagingUtility,
  g_options in 'g_options.pas',
  g_language in 'g_language.pas';

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

  type
    THandlerObject = class (TObject)
      procedure ExceptionHandler (Sender: TObject; e: Exception);
    end;

  var
    LogFileName: AnsiString = '';
    ParamFileIndex: Integer = 1;

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

  procedure THandlerObject.ExceptionHandler (Sender: TObject; e: Exception);
  begin
    e_WriteStackTrace(e.message);
    MessageDlg('Unhandled exception: ' + e.message + ' (see Editor.log for more information)', mtError, [mbOK], 0);
  end;

  procedure CheckParamOptions;
    var i: Integer; p: AnsiString;
  begin
    i := 1;
    while (i <= ParamCount) and (Length(ParamStr(i)) > 0) and (ParamStr(i)[1] = '-') do
    begin
      p := ParamStr(i);
      if p = '--log-file' then
      begin
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          LogFileName := ParamStr(i);
        end;
      end
      else if p = '--config' then
      begin
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          CfgFileName := ParamStr(i);
        end;
      end
      else if p = '--game-wad' then
      begin
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          GameWad := ParamStr(i);
        end;
      end
      else if p = '--editor-wad' then
      begin
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          EditorWad := ParamStr(i);
        end;
      end
      else if p = '--wads-dir' then
      begin
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          WadsDir := ParamStr(i);
        end;
      end
      else if p = '--lang-dir' then
      begin
        if i + 1 <= ParamCount then
        begin
          Inc(i);
          LangDir := ParamStr(i);
        end;
      end;
      Inc(i);
    end;
    ParamFileIndex := i;
  end;

  procedure CheckParamFiles;
    var i: Integer;
  begin
    i := ParamFileIndex;
    if i <= ParamCount then
      StartMap := ParamStr(i);
  end;

  procedure InitPathes;
    {$IFDEF DARWIN}
      var BundlePath, DFPath, DocPath: AnsiString; ns: NSString;
      var ApplicationSupportDirs, DocumentDirs: NSArray;
      var count: Integer;
    {$ELSE}
      var EditorDir: AnsiString;
    {$ENDIF}
  begin
    {$IFDEF DARWIN}
      BundlePath := GetBundlePath();
      ApplicationSupportDirs := NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, true);
      count := ApplicationSupportDirs.count;
      ns := ApplicationSupportDirs.objectAtIndex(count - 1);
      DFPath := NSStringToAnsiString(ns) + DirectorySeparator + 'Doom 2D Forever';
      DocumentDirs := NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, true);
      count := DocumentDirs.count;
      ns := DocumentDirs.objectAtIndex(count - 1);
      DocPath := NSStringToAnsiString(ns) + DirectorySeparator + 'Doom 2D Forever';
      GameExeFile := 'Doom 2D Forever.app';
      CfgFileName := DFPath + DirectorySeparator + 'Editor.cfg';
      LogFileName := DFPath + DirectorySeparator + 'Editor.log';
      MapsDir := DocPath + DirectorySeparator + 'Maps';
      WadsDir := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' + DirectorySeparator + 'wads';
      LangDIr := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' + DirectorySeparator + 'data' + DirectorySeparator + 'lang';
      GameWad := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' + DirectorySeparator + 'data' + DirectorySeparator + 'game.wad';
      EditorWad := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' + DirectorySeparator + 'data' + DirectorySeparator + 'editor.wad';
    {$ELSE}
      EditorDir := ExtractFilePath(Application.ExeName);
      {$IFDEF WINDOWS}
        GameExeFile := 'Doom2DF.exe';
      {$ELSE}
        GameExeFile := 'Doom2DF';
      {$ENDIF}
      CfgFileName := EditorDir + DirectorySeparator + 'Editor.cfg';
      LogFileName := EditorDir + DirectorySeparator + 'Editor.log';
      MapsDir := EditorDir + DirectorySeparator + 'maps';
      WadsDir := EditorDir + DirectorySeparator + 'wads';
      LangDir := EditorDir + DirectorySeparator + 'data' + DirectorySeparator + 'lang';
      GameWad := EditorDir + DirectorySeparator + 'data' + DirectorySeparator + 'game.wad';
      EditorWad := EditorDir + DirectorySeparator + 'data' + DirectorySeparator + 'editor.wad';
    {$ENDIF}
    ForceDirectories(MapsDir);
    ForceDirectories(WadsDir);
  end;

  procedure InitLogs;
  begin
    e_InitLog(LogFileName, WM_NEWFILE);

    {$IF DECLARED(UseHeapTrace)}
      (* http://wiki.freepascal.org/heaptrc *)
      GlobalSkipIfNoLeaks := True;
      //SetHeapTraceOutput('EditorLeaks.log');
      //HaltOnError := False;
    {$ENDIF}

    e_WriteLog('Used file pathes:', MSG_NOTIFY);
    e_WriteLog('  GameExeFile = ' + GameExeFile, MSG_NOTIFY);
    e_WriteLog('  CfgFileName = ' + CfgFileName, MSG_NOTIFY);
    e_WriteLog('  LogFileName = ' + LogFileName, MSG_NOTIFY);
    e_WriteLog('  MapsDir     = ' + MapsDir, MSG_NOTIFY);
    e_WriteLog('  WadsDir     = ' + WadsDir, MSG_NOTIFY);
    e_WriteLog('  LangDir     = ' + LangDir, MSG_NOTIFY);
    e_WriteLog('  GameWad     = ' + GameWad, MSG_NOTIFY);
    e_WriteLog('  EditorWad   = ' + EditorWad, MSG_NOTIFY);
  end;

begin
  Application.ExceptionDialog := aedOkMessageBox;
  Application.AddOnExceptionHandler(THandlerObject.ExceptionHandler, True);
  Application.Initialize();
  {$IFDEF DARWIN}
    // Disable icons in menu on OSX by default
    Application.ShowMenuGlyphs := sbgNever;
  {$ENDIF}

  InitPathes;
  CheckParamOptions;
  InitLogs;

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TMapOptionsForm, MapOptionsForm);
  Application.CreateForm(TActivationTypeForm, ActivationTypeForm);
  Application.CreateForm(TAddResourceForm, AddResourceForm);
  Application.CreateForm(TKeysForm, KeysForm);
  Application.CreateForm(TMapCheckForm, MapCheckForm);
  Application.CreateForm(TMapOptimizationForm, MapOptimizationForm);
  Application.CreateForm(TAddTextureForm, AddTextureForm);
  Application.CreateForm(TSaveMapForm, SaveMapForm);
  Application.CreateForm(TSelectMapForm, SelectMapForm);
  Application.CreateForm(TAddSkyForm, AddSkyForm);
  Application.CreateForm(TAddSoundForm, AddSoundForm);
  Application.CreateForm(TSaveMiniMapForm, SaveMiniMapForm);
  Application.CreateForm(TPackMapForm, PackMapForm);
  Application.CreateForm(TChooseTypeForm, ChooseTypeForm);

  g_Language_Set(gLanguage);

  CheckParamFiles;

  Application.Run();
end.
