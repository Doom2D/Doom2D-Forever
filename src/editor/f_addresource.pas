unit f_addresource;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, e_Log, e_textures, WADEDITOR;

type
  TOKFunction = function: Boolean;

  TAddResourceForm = class (TForm)
    LabelWADs: TLabel;
    cbWADList: TComboBox;
    LabelSections: TLabel;
    cbSectionsList: TComboBox;
    lbResourcesList: TListBox;
    bOK: TButton;
    bCancel: TButton;

    procedure FormActivate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbWADListChange(Sender: TObject);
    procedure cbSectionsListChange(Sender: TObject);
    procedure lbResourcesListClick(Sender: TObject);

  protected
    FResourceName: String;
    FFullResourceName: String;
    FResourceSelected: Boolean;
    FOKFunction: TOKFunction;

  public
    property ResourceName: String read FResourceName;
    property FullResourceName: String read FFullResourceName;
    property OKFunction: TOKFunction read FOKFunction write FOKFunction;
  end;

var
  AddResourceForm: TAddResourceForm;

implementation

uses
  f_main, WADSTRUCT, g_language;

{$R *.lfm}

const
  STANDART_WAD = 'Standart.wad';

procedure TAddResourceForm.FormActivate(Sender: TObject);
var
  SR: TSearchRec;
  
begin
  cbWADList.Clear();
  cbSectionsList.Clear();
  lbResourcesList.Clear();

  FResourceName := '';
  FFullResourceName := '';
  FResourceSelected := False;

  ChDir(EditorDir);
  if FindFirst(EditorDir+'wads/*.wad', faAnyFile, SR) = 0 then
  repeat
    cbWADList.Items.Add(SR.Name);
  until FindNext(SR) <> 0;
  FindClose(SR);

// "Standart.wad" в начало списка:
  if cbWADList.Items.IndexOf(STANDART_WAD) > 0 then
  begin
    cbWADList.Items.Delete(cbWADList.Items.IndexOf(STANDART_WAD));
    cbWADList.Items.Insert(0, STANDART_WAD);
  end;

// WAD карты:
  if OpenedMap <> '' then
    cbWADList.Items.Add(_lc[I_WAD_SPECIAL_MAP]);
end;

procedure TAddResourceForm.bOKClick(Sender: TObject);
begin
  if FResourceName = '' then
  begin
    MessageBox(0, PChar(_lc[I_MSG_CHOOSE_RES]),
               PChar(_lc[I_MSG_ERROR]), MB_OK + MB_ICONERROR);
    Exit;
  end;

  if @FOKFunction <> nil then
    begin
      if FOKFunction() then
        Close();
    end
  else
    Close();
end;

procedure TAddResourceForm.cbWADListChange(Sender: TObject);
var
  WAD: TWADEditor_1;
  SectionList: SArray;
  i: Integer;
  FileName, fn, sn, rn: String;

begin
  WAD := TWADEditor_1.Create();

// Внешний WAD:
  if cbWADList.Text <> _lc[I_WAD_SPECIAL_MAP] then
     FileName := EditorDir+'wads/'+cbWADList.Text
  else // WAD карты:
    begin
      g_ProcessResourceStr(OpenedMap, fn, sn, rn);
      FileName := fn;
    end;

// Читаем секции:
  WAD.ReadFile(FileName);
  SectionList := WAD.GetSectionList();
  WAD.Free();

  cbSectionsList.Clear();
  lbResourcesList.Clear();

  if SectionList <> nil then
    for i := 0 to High(SectionList) do
      if SectionList[i] <> '' then
        cbSectionsList.Items.Add(SectionList[i])
      else
        cbSectionsList.Items.Add('..');
end;

procedure TAddResourceForm.cbSectionsListChange(Sender: TObject);
var
  ResourceList: SArray;
  WAD: TWADEditor_1;
  i: DWORD;
  FileName, SectionName, fn, sn, rn: String;

begin
  WAD := TWADEditor_1.Create();

// Внешний WAD:
  if cbWADList.Text <> _lc[I_WAD_SPECIAL_MAP] then
    FileName := EditorDir+'wads/'+cbWADList.Text
  else // WAD карты:
    begin
      g_ProcessResourceStr(OpenedMap, fn, sn, rn);
      FileName := fn;
    end;

// Читаем WAD:
  WAD.ReadFile(FileName);

  if cbSectionsList.Text <> '..' then
    SectionName := cbSectionsList.Text
  else
    SectionName := '';

// Читаем ресурсы выбранной секции:
  ResourceList := WAD.GetResourcesList(SectionName);

  WAD.Free();

  lbResourcesList.Clear();

  if ResourceList <> nil then
    for i := 0 to High(ResourceList) do
      lbResourcesList.Items.Add(ResourceList[i]);
end;

procedure TAddResourceForm.lbResourcesListClick(Sender: TObject);
var
  FileName, SectionName, fn: String;

begin
  FResourceSelected := (lbResourcesList.SelCount > 0) or
                       (lbResourcesList.ItemIndex > -1);

  if not FResourceSelected then
  begin
    FResourceName := '';
    FFullResourceName := '';
    Exit;
  end;

  if cbSectionsList.Text = '..' then
    SectionName := ''
  else
    SectionName := cbSectionsList.Text;

  if cbWADList.Text[1] <> '<' then
    FileName := cbWADList.Text
  else
    FileName := '';

  FResourceName := FileName+':'+SectionName+'\'+lbResourcesList.Items[lbResourcesList.ItemIndex];

  if FileName <> '' then
    FFullResourceName := EditorDir+'wads/'+FResourceName
  else
    begin
      g_ProcessResourceStr(OpenedMap, @fn, nil, nil);
      FFullResourceName := fn+FResourceName;
    end;
end;

end.
