program AnimView;

uses
  Forms,
  UMain in 'UMain.pas' {MainForm},
  dglOpenGL in '..\Lib\OpenGL\dglOpenGL.pas',
  GL in '..\Lib\OpenGL\GL.pas',
  GLext in '..\Lib\OpenGL\glExt.pas',
  e_graphics in '..\Engine Source\e_graphics.pas',
  e_textures in '..\Engine Source\e_textures.pas',
  e_log in '..\Engine Source\e_log.pas',
  UAnimSel in 'UAnimSel.pas' {AnimSelForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAnimSelForm, AnimSelForm);
  Application.Run;
end.
