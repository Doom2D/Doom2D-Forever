unit f_addresource;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, e_Log, e_textures, WADEDITOR;

type
  TOKFunction = function: Boolean;

  TAddResourceForm = class(TForm)
   bOK: TButton;
   bCancel: TButton;
   cbWADList: TComboBox;
   lbResourcesList: TListBox;
   Label1: TLabel;
   cbSectionsList: TComboBox;
   Label2: TLabel;
   procedure FormActivate(Sender: TObject);
   procedure bOKClick(Sender: TObject);
   procedure cbWADListChange(Sender: TObject);
   procedure cbSectionsListChange(Sender: TObject);
   procedure lbResourcesListClick(Sender: TObject);
  protected
   FResourceName: string;
   FFullResourceName: string;
   FResourceSelected: Boolean;
   FOKFunction: TOKFunction;
  public
   property ResourceName: string read FResourceName;
   property FullResourceName: string read FFullResourceName;
   property OKFunction:TOKFunction read FOKFunction write FOKFunction;
  end;

const
  WAD_SPECIAL_MAP = '<ÊÀÐÒÀ>';
  WAD_SPECIAL_TEXTURES = '<ÑÏÅÖÒÅÊÑÒÓÐÛ>';

var
  AddResourceForm: TAddResourceForm;

implementation

uses f_main, WADSTRUCT;

{$R *.dfm}

procedure TAddResourceForm.FormActivate(Sender: TObject);
var
  SR: TSearchRec;
begin
 cbWADList.Clear;
 cbSectionsList.Clear;
 lbResourcesList.Clear;

 FResourceName := '';
 FFullResourceName := '';
 FResourceSelected := False;

 ChDir(EditorDir);
 if FindFirst(EditorDir+'wads\*.wad', faAnyFile, SR) = 0 then
 repeat
  cbWADList.Items.Add(SR.Name);
 until FindNext(SR) <> 0;
 FindClose(SR);

 if cbWADList.Items.IndexOf('Standart.wad') > 0 then
 begin
  cbWADList.Items.Delete(cbWADList.Items.IndexOf('Standart.wad'));
  cbWADList.Items.Insert(0, 'Standart.wad');
 end;

 if OpenedMap <> '' then cbWADList.Items.Add(WAD_SPECIAL_MAP);
end;

procedure TAddResourceForm.bOKClick(Sender: TObject);
begin
 if FResourceName = '' then
 begin
  MessageDlg('Íå âûáðàí ðåñóðñ', mtError, [mbOK], 0);
  Exit;
 end;

 if @FOKFunction <> nil then (if FOKFunction then Close) else Close;
end;

procedure TAddResourceForm.cbWADListChange(Sender: TObject);
var
  WAD: TWADEditor_1;
  SectionList: SArray;
  i: Integer;
  FileName, fn, sn, rn: string;
begin
 WAD := TWADEditor_1.Create;

 if cbWADList.Text <> WAD_SPECIAL_MAP then FileName := EditorDir+'wads\'+cbWADList.Text
  else
 begin
  g_ProcessResourceStr(OpenedMap, fn, sn, rn);
  //FileName := EditorDir+'maps\'+ExtractFileName(fn);
  FileName := fn;
 end;

 WAD.ReadFile(FileName);
 SectionList := WAD.GetSectionList();
 WAD.Destroy;

 cbSectionsList.Clear;
 lbResourcesList.Clear;

 if SectionList <> nil then
  for i := 0 to High(SectionList) do
   if SectionList[i] <> '' then cbSectionsList.Items.Add(SectionList[i])
    else cbSectionsList.Items.Add('..');
end;

procedure TAddResourceForm.cbSectionsListChange(Sender: TObject);
var
  ResourceList: SArray;
  WAD: TWADEditor_1;
  i: DWORD;
  FileName, SectionName, fn, sn, rn: string;
begin
 WAD := TWADEditor_1.Create;

 if cbWADList.Text <> WAD_SPECIAL_MAP then FileName := EditorDir+'wads\'+cbWADList.Text
  else
 begin
  g_ProcessResourceStr(OpenedMap, fn, sn, rn);
  //FileName := EditorDir+'maps\'+ExtractFileName(fn);
  FileName := fn;
 end;

 WAD.ReadFile(FileName);

 if cbSectionsList.Text <> '..' then SectionName := cbSectionsList.Text
  else SectionName := '';
 ResourceList := WAD.GetResourcesList(SectionName);

 WAD.Destroy;

 lbResourcesList.Clear;

 if ResourceList <> nil then
  for i := 0 to High(ResourceList) do
   lbResourcesList.Items.Add(ResourceList[i]);
end;

procedure TAddResourceForm.lbResourcesListClick(Sender: TObject);
var
  FileName, SectionName, fn: string;
begin
 FResourceSelected := (lbResourcesList.SelCount > 0) or
                      (lbResourcesList.ItemIndex > -1);

 if not FResourceSelected then
 begin
  FResourceName := '';
  FFullResourceName := '';
  Exit;
 end;

 if cbSectionsList.Text = '..' then SectionName := ''
  else SectionName := cbSectionsList.Text;

 if cbWADList.Text[1] <> '<' then FileName := cbWADList.Text else FileName := '';

 FResourceName := FileName+':'+SectionName+'\'+lbResourcesList.Items[lbResourcesList.ItemIndex];

 if FileName <> '' then FFullResourceName := EditorDir+'wads\'+FResourceName
  else
 begin
  g_ProcessResourceStr(OpenedMap, @fn, nil, nil);
  //FFullResourceName := EditorDir+'maps\'+ExtractFileName(fn)+FResourceName;
  FFullResourceName := fn+FResourceName;
 end;
end;

end.
