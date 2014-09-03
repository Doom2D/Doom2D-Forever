program Editor;

uses
  Forms,
  dglOpenGL in '..\Lib\OpenGL\dglOpenGL.pas',
  e_graphics in '..\Engine Source\e_graphics.pas',
  e_log in '..\Engine Source\e_log.pas',
  e_textures in '..\Engine Source\e_textures.pas',
  f_about in 'f_about.pas' {AboutForm},
  f_activationtype in 'f_activationtype.pas' {ActivationTypeForm},
  f_addtexture in 'f_addtexture.pas' {AddTextureForm},
  f_keys in 'f_keys.pas' {KeysForm},
  f_main in 'f_main.pas' {MainForm},
  f_mapcheck in 'f_mapcheck.pas' {MapCheckForm},
  f_mapoptimization in 'f_mapoptimization.pas' {MapOptimizationForm},
  f_mapoptions in 'f_mapoptions.pas' {MapOptionsForm},
  f_options in 'f_options.pas' {OptionsForm},
  f_savemap in 'f_savemap.pas' {SaveMapForm},
  f_selectmap in 'f_selectmap.pas' {SelectMapForm},
  g_areas in 'g_areas.pas',
  g_basic in 'g_basic.pas',
  g_monsters in 'g_monsters.pas',
  g_textures in 'g_textures.pas',
  g_triggers in 'g_triggers.pas',
  g_wad in '..\DF Sources\g_wad.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Doom2D: Forever map editor';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAddTextureForm, AddTextureForm);
  Application.CreateForm(TMapOptionsForm, MapOptionsForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TMapCheckForm, MapCheckForm);
  Application.CreateForm(TMapOptimizationForm, MapOptimizationForm);
  Application.CreateForm(TSelectMapForm, SelectMapForm);
  Application.CreateForm(TSaveMapForm, SaveMapForm);
  Application.CreateForm(TActivationTypeForm, ActivationTypeForm);
  Application.CreateForm(TKeysForm, KeysForm);
  Application.Run;
end.
