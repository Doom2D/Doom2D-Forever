program Editor;

{$INCLUDE ../shared/a_modes.inc}

uses
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
  xstreams in '../shared/xstreams.pas',
  dfzip in '../shared/dfzip.pas',
  sfs in '../sfs/sfs.pas',
  sfsPlainFS in '../sfs/sfsPlainFS.pas',
  sfsZipFS in '../sfs/sfsZipFS.pas',

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
  f_maptest in 'f_maptest.pas' {MapTestForm},
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
  g_language in 'g_language.pas',
  f_selectlang in 'f_selectlang.pas' {SelectLanguageForm};

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

  procedure InitLogs;
  begin
    if LogFileName = '' then
      LogFileName := 'Editor.log';

    if LogFileName <> '' then
      e_InitLog(LogFileName, WM_NEWFILE);

    {$IF DECLARED(UseHeapTrace)}
      (* http://wiki.freepascal.org/heaptrc *)
      GlobalSkipIfNoLeaks := True;
      //SetHeapTraceOutput('EditorLeaks.log');
      //HaltOnError := False;
    {$ENDIF}
  end;

begin
  Application.ExceptionDialog := aedOkMessageBox;
  Application.AddOnExceptionHandler(THandlerObject.ExceptionHandler, True);
  Application.Initialize();

  EditorDir := ExtractFilePath(Application.ExeName);
  CfgFileName := EditorDir + DirectorySeparator + 'Editor.cfg';
  GameWad := EditorDir + DirectorySeparator + 'data' + DirectorySeparator + 'game.wad';
  EditorWad := EditorDir + DirectorySeparator + 'data' + DirectorySeparator + 'editor.wad';
  WadsDir := EditorDir + DirectorySeparator + 'wads';

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
  Application.CreateForm(TMapTestForm, MapTestForm);
  Application.CreateForm(TChooseTypeForm, ChooseTypeForm);
  Application.CreateForm(TSelectLanguageForm, SelectLanguageForm);

  CheckParamFiles;

  Application.Run();
end.
