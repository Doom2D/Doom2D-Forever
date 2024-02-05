program Editor;

{$INCLUDE ../shared/a_modes.inc}

uses
  SysUtils, Interfaces,
  Forms, Dialogs,
  GL, GLExt,
{$IFDEF DARWIN}
  MacOSAll, CocoaAll,
{$ENDIF}

  Imaging, ImagingTypes, ImagingUtility,
{$IFNDEF NOSOUND}
  fmod, fmodtypes, fmoderrors, fmodpresets,
{$ENDIF}

  WADEDITOR_dfwad in '../shared/WADEDITOR_dfwad.pas',
  WADEDITOR_dfzip in '../shared/WADEDITOR_dfzip.pas',
  e_log in '../engine/e_log.pas',

  g_language, g_options,
  f_main;

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

type
  THandlerObject = class
    procedure ExceptionHandler (Sender: TObject; e: Exception);
  end;

var
  LogFileName: AnsiString = '';
  ParamFileIndex: Integer = 1;

procedure THandlerObject.ExceptionHandler (Sender: TObject; e: Exception);
begin
  e_WriteStackTrace(e.message);
  MessageDlg('Unhandled exception: ' + e.message + ' (see Editor.log for more information)',
    mtError, [mbOK], 0);
end;

procedure InitLogs ();
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

procedure CheckParamOptions ();
var
  i: Integer;
  p: AnsiString;
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

procedure CheckParamFiles ();
var
  i: Integer;
begin
  i := ParamFileIndex;
  if i <= ParamCount then
    StartMap := ParamStr(i);
end;

{$IFDEF DARWIN}
function NSStringToAnsiString (s: NSString): AnsiString;
var
  i: Integer;
begin
  result := '';
  for i := 0 to s.length-1 do
    result := result + AnsiChar(s.characterAtIndex(i));
end;

function GetBundlePath (): AnsiString;
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: ShortString;
begin
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  Result := pathStr;
end;
{$ENDIF}

procedure InitPathes ();
var
{$IFNDEF DARWIN}
  EditorDir: AnsiString;
{$ELSE}
  BundlePath, DFPath, DocPath: AnsiString; ns: NSString;
  ApplicationSupportDirs, DocumentDirs: NSArray;
  count: Integer;
{$ENDIF}
begin
{$IFNDEF DARWIN}
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
{$ELSE}
  BundlePath := GetBundlePath();
  ApplicationSupportDirs := NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory,
    NSUserDomainMask, true);
  count := ApplicationSupportDirs.count;
  ns := ApplicationSupportDirs.objectAtIndex(count - 1);
  DFPath := NSStringToAnsiString(ns) + DirectorySeparator + 'Doom2D Forever';
  DocumentDirs := NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, true);
  count := DocumentDirs.count;
  ns := DocumentDirs.objectAtIndex(count - 1);
  DocPath := NSStringToAnsiString(ns) + DirectorySeparator + 'Doom2D Forever';
  GameExeFile := 'Doom2D Forever.app';
  CfgFileName := DFPath + DirectorySeparator + 'Editor.cfg';
  LogFileName := DFPath + DirectorySeparator + 'Editor.log';
  MapsDir := DocPath + DirectorySeparator + 'Maps';
  WadsDir := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' +
    DirectorySeparator + 'wads';
  LangDIr := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' +
    DirectorySeparator + 'data' + DirectorySeparator + 'lang';
  GameWad := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' +
    DirectorySeparator + 'data' + DirectorySeparator + 'game.wad';
  EditorWad := BundlePath + DirectorySeparator + 'Contents' + DirectorySeparator + 'Resources' +
    DirectorySeparator + 'data' + DirectorySeparator + 'editor.wad';
{$ENDIF}
  ForceDirectories(MapsDir);
  ForceDirectories(WadsDir);
end;

begin
{$IFDEF DARWIN}
  // Disable icons in menu on OSX by default
  Application.ShowMenuGlyphs := sbgNever;
{$ENDIF}

  Application.ExceptionDialog := aedOkMessageBox;
  Application.AddOnExceptionHandler(THandlerObject.ExceptionHandler, True);
  Application.Initialize();  // LCL requires this to come strictly before any call to .CreateForm()

  InitPathes;
  CheckParamOptions;
  InitLogs;

  Application.CreateForm(TMainForm, MainForm);
  g_Language_Set(gLanguage);

  CheckParamFiles;
  Application.Run();
end.
