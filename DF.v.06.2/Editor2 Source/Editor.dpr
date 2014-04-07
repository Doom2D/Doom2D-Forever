program Editor;

uses
  Forms,
  dglOpenGL in '..\Lib\OpenGL\dglOpenGL.pas',
  e_graphics in '..\Engine Source\e_graphics.pas',
  e_log in '..\Engine Source\e_log.pas',
  e_textures in '..\Engine Source\e_textures.pas',
  MAPSTRUCT in '..\Shared Source\MAPSTRUCT.pas',
  MAPREADER in '..\Shared Source\MAPREADER.pas',
  MAPWRITER in '..\Shared Source\MAPWRITER.pas',
  MAPDEF in '..\Shared Source\MAPDEF.pas',
  WADEDITOR in '..\Shared Source\WADEDITOR.pas',
  WADSTRUCT in '..\Shared Source\WADSTRUCT.pas',
  CONFIG in '..\Shared Source\CONFIG.pas',
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
  fmod in '..\Lib\FMOD\fmod.pas',
  fmoderrors in '..\Lib\FMOD\fmoderrors.pas',
  fmodpresets in '..\Lib\FMOD\fmodpresets.pas',
  fmodtypes in '..\Lib\FMOD\fmodtypes.pas',
  g_language in 'g_language.pas';

{$R *.res}

begin
  Application.Initialize;
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
  if ParamStr(1) <> '' then OpenMap(ParamStr(1), '');

  Application.Run;
end.
