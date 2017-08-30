unit f_options;

{$INCLUDE ../shared/a_modes.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Registry;

type
  TOptionsForm = class (TForm)
    bOK: TButton;
    bCancel: TButton;
    ColorDialog: TColorDialog;
    GroupBox1: TGroupBox;
  // Общие настройки:
    cbShowDots: TCheckBox;
    cbShowTexture: TCheckBox;
    cbShowSize: TCheckBox;
  // Шаги сетки:
    LabelGrid: TLabel;
    eDotStepOne: TEdit;
    UpDown1: TUpDown;
    eDotStepTwo: TEdit;
    UpDown2: TUpDown;
  // Цвет сетки:
    LabelGridCol: TLabel;
    sDotColor: TShape;
    bGrid: TButton;
  // Цвет фона:
    LabelBack: TLabel;
    sBackColor: TShape;
    bBack: TButton;
  // Цвет превью:
    LabelPreview: TLabel;
    sPreviewColor: TShape;
    bPreview: TButton;
  // Масштаб миникарты:
    LabelMinimap: TLabel;
    cbScale: TComboBox;
  // Количество недавно открытых:
    LabelRecent: TLabel;
    eRecent: TEdit;
    UpDown3: TUpDown;
    LabelLanguage: TLabel;
    rbRussian: TRadioButton;
    rbEnglish: TRadioButton;
    LabelGridSize: TLabel;
    cbDotSize: TComboBox;

    procedure bGridClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bBackClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

procedure RegisterFileType(ext: String; FileName: String);

implementation

uses
  f_main, StdConvs, CONFIG, g_language;

{$R *.lfm}

procedure RegisterFileType(ext: String; FileName: String);
var
  reg: TRegistry;

begin
  reg := TRegistry.Create();

  with reg do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.'+ext,True);
    WriteString('',ext+'file');
    CloseKey();
    CreateKey(ext+'file');
    OpenKey(ext+'file\DefaultIcon',True);
    WriteString('',FileName+',0');
    CloseKey();
    OpenKey(ext+'file\shell\open\command',True);
    WriteString('',FileName+' "%1"');
    CloseKey();
    Free();
  end;
end;

procedure TOptionsForm.bGridClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    sDotColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
  sDotColor.Brush.Color := DotColor;
  cbShowDots.Checked := DotEnable;
  cbShowTexture.Checked := DrawTexturePanel;
  cbShowSize.Checked := DrawPanelSize;
  eDotStepOne.Text := IntToStr(DotStepOne);
  eDotStepTwo.Text := IntToStr(DotStepTwo);
  sBackColor.Brush.Color := BackColor;
  sPreviewColor.Brush.Color := PreviewColor;
  if Scale = 2 then
    cbScale.ItemIndex := 1
  else
    cbScale.ItemIndex := 0;
  if DotSize = 2 then
    cbDotSize.ItemIndex := 1
  else
    cbDotSize.ItemIndex := 0;
  eRecent.Text := IntToStr(RecentCount);

// Язык:
  if gLanguage = LANGUAGE_RUSSIAN then
    begin
      rbRussian.Checked := True;
      rbEnglish.Checked := False;
    end
  else
    begin
      rbRussian.Checked := False;
      rbEnglish.Checked := True;
    end;
end;

procedure TOptionsForm.bOKClick(Sender: TObject);
var
  config: TConfig;
  re: Integer;
  d1: Boolean;
  str: String;

begin
  re := StrToIntDef(eRecent.Text, 5);
  if re < 2 then
    re := 2;
  if re > 10 then
    re := 10;

  if rbRussian.Checked then
    str := LANGUAGE_RUSSIAN
  else
    str := LANGUAGE_ENGLISH;

// Нужно сменить язык:
  if gLanguage <> str then
  begin
    gLanguage := str;
    //e_WriteLog('Read language file', MSG_NOTIFY);
    //g_Language_Load(EditorDir+'\data\'+gLanguage+LANGUAGE_FILE_NAME);
    g_Language_Set(gLanguage);
  end;
  
  DotColor := sDotColor.Brush.Color;
  DotEnable := cbShowDots.Checked;

  if DotStep = DotStepOne then
    d1 := True
  else
    d1 := False;
  DotStepOne := StrToIntDef(eDotStepOne.Text, 16);
  DotStepTwo := StrToIntDef(eDotStepTwo.Text, 8);
  if d1 then
    DotStep := DotStepOne
  else
    DotStep := DotStepTwo;

  DrawTexturePanel := cbShowTexture.Checked;
  DrawPanelSize := cbShowSize.Checked;
  BackColor := sBackColor.Brush.Color;
  PreviewColor := sPreviewColor.Brush.Color;

  if cbScale.ItemIndex = 1 then
    Scale := 2
  else
    Scale := 1;

  if cbDotSize.ItemIndex = 1 then
    DotSize := 2
  else
    DotSize := 1;

  config := TConfig.CreateFile(EditorDir+'Editor.cfg');

  config.WriteInt('Editor', 'DotColor', DotColor);
  config.WriteBool('Editor', 'DotEnable', DotEnable);
  config.WriteInt('Editor', 'DotStepOne', DotStepOne);
  config.WriteInt('Editor', 'DotStepTwo', DotStepTwo);
  config.WriteInt('Editor', 'DotStep', DotStep);
  config.WriteInt('Editor', 'DotSize', cbDotSize.ItemIndex);
  config.WriteBool('Editor', 'DrawTexturePanel', DrawTexturePanel);
  config.WriteBool('Editor', 'DrawPanelSize', DrawPanelSize);
  config.WriteInt('Editor', 'BackColor', BackColor);
  config.WriteInt('Editor', 'PreviewColor', PreviewColor);
  config.WriteInt('Editor', 'Scale', cbScale.ItemIndex);
  config.WriteInt('Editor', 'RecentCount', re);
  config.WriteStr('Editor', 'Language', gLanguage);

  if RecentCount <> re then
  begin
    RecentCount := re;
    MainForm.RefreshRecentMenu();
  end;

  config.SaveFile(EditorDir+'Editor.cfg');
  config.Free();
  Close();
end;

procedure TOptionsForm.bCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TOptionsForm.bBackClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    sBackColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.bPreviewClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    sPreviewColor.Brush.Color := ColorDialog.Color;
end;

end.
