unit f_options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Registry;

type
  TOptionsForm = class(TForm)
    ColorDialog: TColorDialog;
    GroupBox1: TGroupBox;
    sDotColor: TShape;
    bOK: TButton;
    bCancel: TButton;
    cbShowDots: TCheckBox;
    Label1: TLabel;
    UpDown1: TUpDown;
    eDotStep: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    sBackColor: TShape;
    Button2: TButton;
    Label4: TLabel;
    sPreviewColor: TShape;
    Button3: TButton;
    Label5: TLabel;
    cbScale: TComboBox;
    cbShowTexture: TCheckBox;
    cbShowSize: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

procedure RegisterFileType(ext: string; FileName: string);

implementation

uses f_main, StdConvs, CONFIG;

{$R *.dfm}

procedure RegisterFileType(ext: string; FileName: string);
var
  reg: TRegistry;
begin
 reg := TRegistry.Create;

 with reg do
 begin
  RootKey := HKEY_CLASSES_ROOT;
  OpenKey('.'+ext,True);
  WriteString('',ext+'file');
  CloseKey;
  CreateKey(ext+'file');
  OpenKey(ext+'file\DefaultIcon',True);
  WriteString('',FileName+',0');
  CloseKey;
  OpenKey(ext+'file\shell\open\command',True);
  WriteString('',FileName+' "%1"');
  CloseKey;
  Free;
 end;
end;

procedure TOptionsForm.Button1Click(Sender: TObject);
begin
 if ColorDialog.Execute then sDotColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
 sDotColor.Brush.Color := DotColor;
 cbShowDots.Checked := DotEnable;
 cbShowTexture.Checked := DrawTexturePanel;
 cbShowSize.Checked := DrawPanelSize;
 eDotStep.Text := IntToStr(DotStep);
 sBackColor.Brush.Color := BackColor;
 sPreviewColor.Brush.Color := PreviewColor;
 if Scale = 1 then cbScale.ItemIndex := 0 else cbScale.ItemIndex := 1;
end;

procedure TOptionsForm.bOKClick(Sender: TObject);
var
  config: TConfig;
begin
 config := TConfig.CreateFile(EditorDir+'\editor.cfg');
 config.WriteInt('Editor', 'DotColor', sDotColor.Brush.Color);
 config.WriteBool('Editor', 'DotEnable', cbShowDots.Checked);
 config.WriteInt('Editor', 'DotStep', StrToInt(eDotStep.Text));
 config.WriteBool('Editor', 'DrawTexturePanel', cbShowTexture.Checked);
 config.WriteBool('Editor', 'DrawPanelSize', cbShowSize.Checked);
 config.WriteInt('Editor', 'BackColor', sBackColor.Brush.Color);
 config.WriteInt('Editor', 'PreviewColor', sPreviewColor.Brush.Color);
 config.WriteInt('Editor', 'Scale', cbScale.ItemIndex);

 DotColor := sDotColor.Brush.Color;
 DotEnable := cbShowDots.Checked;
 DrawTexturePanel := cbShowTexture.Checked;
 DrawPanelSize := cbShowSize.Checked;
 DotStep := StrToInt(eDotStep.Text);
 BackColor := sBackColor.Brush.Color;
 PreviewColor := sPreviewColor.Brush.Color;
 if cbScale.ItemIndex = 0 then Scale := 1 else Scale := 2;

 config.SaveFile(EditorDir+'\editor.cfg');
 config.Destroy;
 Close;
end;

procedure TOptionsForm.bCancelClick(Sender: TObject);
begin
 Close;
end;

procedure TOptionsForm.Button2Click(Sender: TObject);
begin
 if ColorDialog.Execute then sBackColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.Button3Click(Sender: TObject);
begin
 if ColorDialog.Execute then sPreviewColor.Brush.Color := ColorDialog.Color;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  config: TConfig;
begin
 config := TConfig.CreateFile(EditorDir+'\editor.cfg');

 DotColor := config.ReadInt('Editor', 'DotColor', $FFFFFF);
 DotEnable := config.ReadBool('Editor', 'DotEnable', True);
 DotStep := config.ReadInt('Editor', 'DotStep', 16);
 DrawTexturePanel := config.ReadBool('Editor', 'DrawTexturePanel', True);
 DrawPanelSize := config.ReadBool('Editor', 'DrawPanelSize', True);
 BackColor := config.ReadInt('Editor', 'BackColor', $7F6040);
 PreviewColor := config.ReadInt('Editor', 'PreviewColor', $00FF00);
 if config.ReadInt('Editor', 'Scale', 0) = 0 then Scale := 1 else Scale := 2;

 config.Destroy;
end;

end.
