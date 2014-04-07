unit UAnimSel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

Type
  TAnimTexs = Array of Record
                         T: Array of DWORD;
                         Name: String;
                       End;

  TAnimSelForm = class(TForm)
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    eFrameWidth: TEdit;
    eFrameHeight: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    bOK: TButton;
    bCancel: TButton;
    Label3: TLabel;
    ePath: TEdit;
    Button1: TButton;
    Label4: TLabel;
    Label5: TLabel;
    eHitWidth: TEdit;
    eHitHeight: TEdit;
    UpDown3: TUpDown;
    UpDown4: TUpDown;

    procedure bOKClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    
  private
    { Private declarations }
  public
    wFrameWidth, wFrameHeight: Word;
    wHitWidth, wHitHeight: Word;
    aTexIDs: TAnimTexs;
    sPicName: String;
  end;

var
  AnimSelForm: TAnimSelForm;

implementation

uses
  e_graphics, UMain, Math;

{$R *.dfm}

procedure TAnimSelForm.bOKClick(Sender: TObject);
var
  SR: TSearchRec;
  i, len, res: Integer;
  frC, wTexWidth, wTexHeight: Word;
  id: DWORD;
  path: String;

begin
  sPicName := '';

  if ePath.Text = '' then
  begin
    ModalResult := mrCancel;
    Close();
    Exit;
  end;

  wFrameWidth := StrToIntDef(eFrameWidth.Text, 16);
  wFrameHeight := StrToIntDef(eFrameHeight.Text, 16);
  wHitWidth := StrToIntDef(eHitWidth.Text, 16);
  wHitHeight := StrToIntDef(eHitHeight.Text, 16);

  for i := 0 to Length(aTexIDs)-1 do
    SetLength(aTexIDs[i].T, 0);
  SetLength(aTexIDs, 0);

  path := ExtractFilePath(ePath.Text);
  sPicName := ExtractFileName(ePath.Text);
  if Pos('*', sPicName) > 0 then
    Delete(sPicName, Pos('*', sPicName), 1);

  res :=  FindFirst(ePath.Text, faAnyFile, SR);

  if res = 0 then
  begin
    repeat
    // Не каталог:
      if (SR.Attr and faDirectory) = 0 then
      begin
        if not e_CreateTexture(path+SR.Name, id) then
        begin
          MessageBox(MainForm.Handle, PChar('Ошибка загрузки "'+path+SR.Name+'"!'), 'Ошибка', MB_ICONERROR+MB_OK);
          ModalResult := mrCancel;
          Close();
          Exit;
        end;

        e_GetTextureSize(id, @wTexWidth, @wTexHeight);
        e_DeleteTexture(id);

        if (wFrameWidth <= wTexWidth) and (wFrameHeight = wTexHeight) then
          begin
            frC := wTexWidth div wFrameWidth;

            len := Length(aTexIDs);
            SetLength(aTexIDs, len+1);
            SetLength(aTexIDs[len].T, frC);

            aTexIDs[len].Name := SR.Name;

            for i := 0 to frC-1 do
              if not e_CreateTextureEx(path+SR.Name,
                                       aTexIDs[len].T[i],
                                       i*wFrameWidth, 0,
                                       wFrameWidth, wFrameHeight) then
              begin
                SetLength(aTexIDs[len].T, i);
                Break;
              end;
          end
        else
           MessageBox(MainForm.Handle, PChar('Рамер текстуры "'+SR.Name+'" не тот!'), 'Предупреждение', MB_ICONINFORMATION+MB_OK);
      end;

      res := FindNext(SR);
    until res <> 0;
  end;

  FindClose(sr);
end;

procedure TAnimSelForm.Button1Click(Sender: TObject);
var
  s: String;
  id: DWORD;
  texW, texH: Word;

begin
  if OpenDialog.Execute() then
  begin
    s := ExtractFileName(OpenDialog.FileName);
    s := Copy(s, 1, Pos('_', s)-1) + '*';

    ePath.Text := ExtractFilePath(OpenDialog.FileName) + s;
    OpenDialog.InitialDir := ePath.Text;

    if not e_CreateTexture(OpenDialog.FileName, id) then
    begin
      MessageBox(MainForm.Handle, PChar('Ошибка загрузки "'+OpenDialog.FileName+'"!'), 'Ошибка', MB_ICONERROR+MB_OK);
      Exit;
    end;

    e_GetTextureSize(id, @texW, @texH);
    e_DeleteTexture(id);

    eFrameWidth.Text := IntToStr(min(texW, texH));
    eFrameHeight.Text := IntToStr(texH);
  end;
end;

end.
