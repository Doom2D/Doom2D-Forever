unit f_packmap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TPackMapForm = class(TForm)
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    bPack: TButton;
    Label5: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    eWAD: TEdit;
    bSelectWAD: TButton;
    eResource: TEdit;
    eTSection: TEdit;
    eSSection: TEdit;
    Label4: TLabel;
    eMSection: TEdit;
    Label6: TLabel;
    cbTextrures: TCheckBox;
    cbSky: TCheckBox;
    cbMusic: TCheckBox;
    cbAdd: TCheckBox;
    cbNonStandart: TCheckBox;
    procedure bSelectWADClick(Sender: TObject);
    procedure bPackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PackMapForm: TPackMapForm;

implementation

uses
  WADEDITOR, g_map, MAPREADER, MAPWRITER, MAPSTRUCT, f_main, math;

{$R *.dfm}

procedure TPackMapForm.bSelectWADClick(Sender: TObject);
begin
 if SaveDialog.Execute then eWAD.Text := SaveDialog.FileName;
end;

function ProcessResource(wad_to: TWADEditor_1; section_to, filename, section, resource: string): Boolean;
var
  wad2: TWADEditor_1;
  data: Pointer;
  reslen: Integer;
  //s: string;
begin
 Result := False;
 
 if filename = '' then g_ProcessResourceStr(OpenedMap, @filename, nil, nil)
  else filename := EditorDir+'wads\'+filename;

 wad2 := TWADEditor_1.Create();
 if not wad2.ReadFile(filename) then
 begin
  MessageDlg(Format('Ошибка при открытии вада %s', [ExtractFileName(filename)]), mtError, [mbOK], 0);
  wad2.Destroy();
  Exit;
 end;

 if not wad2.GetResource(section, resource, data, reslen) then
 begin
  MessageDlg(Format('Ошибка при чтении ресурса %s:%s\%s', [filename, section, resource]), mtError, [mbOK], 0);
  wad2.Destroy();
  Exit;
 end;

 wad2.Destroy();

 {if wad_to.HaveResource(section_to, resource) then
 begin
  for a := 2 to 256 do
  begin
   s := IntToStr(a);
   if not wad_to.HaveResource(section_to, resource+s) then Break;
  end;
  resource := resource+s;
 end;}

 if not wad_to.HaveResource(section_to, resource) then
 begin
  if not wad_to.HaveSection(section_to) then wad_to.AddSection(section_to);
  wad_to.AddResource(data, reslen, resource, section_to);
 end;

 FreeMem(data);

 Result := True;
end;

procedure TPackMapForm.bPackClick(Sender: TObject);
var
  wad: TWADEditor_1;
  mr: TMapReader_2;
  mw: TMapWriter_2;
  data: Pointer;
  len: LongWord;
  textures: TTexturesRec1Array;
  header: TMapHeaderRec_1;
  a: Integer;
  res, tsection, ssection, msection, filename, section, resource: string;
begin
 if eWAD.Text = '' then Exit;
 if eResource.Text = '' then Exit;

 tsection := eTSection.Text;
 ssection := eSSection.Text;
 msection := eMSection.Text;

 data := SaveMap('');
 if data = nil then Exit;

 wad := TWADEditor_1.Create();
 if cbAdd.Checked then
  if wad.ReadFile(eWAD.Text) then wad.CreateImage();

 mr := TMapReader_2.Create();
 mr.LoadMap(data);
 FreeMem(data);

 textures := mr.GetTextures();
 if cbTextrures.Checked and (textures <> nil) then
  for a := 0 to High(textures) do
  begin
   res := textures[a].Resource;
   if IsSpecialTexture(res) then Continue;

   g_ProcessResourceStr(res, @filename, @section, @resource);

   if cbNonStandart.Checked then
    if (AnsiLowerCase(filename) = 'standart.wad') or
       (AnsiLowerCase(filename) = 'shrshade.wad') then Continue;

   if not ProcessResource(wad, tsection, filename, section, resource) then
   begin
    mr.Destroy();
    wad.Destroy();
    Exit;
   end;

   res := Format(':%s\%s', [tsection, resource]);
   ZeroMemory(@textures[a].Resource[0], 64);
   CopyMemory(@textures[a].Resource[0], @res[1], Min(Length(res), 64));
  end;

 header := mr.GetMapHeader();
 if cbSky.Checked then
 begin
  res := header.SkyName;
  g_ProcessResourceStr(res, @filename, @section, @resource);

  if (not cbNonStandart.Checked) or
     ((AnsiLowerCase(filename) <> 'standart.wad') and
      (AnsiLowerCase(filename) <> 'shrshade.wad')) then
  begin
   if not ProcessResource(wad, ssection, filename, section, resource) then
   begin
    mr.Destroy();
    wad.Destroy();
    Exit;
   end;

   res := Format(':%s\%s', [ssection, resource]);
   ZeroMemory(@header.SkyName[0], 64);
   CopyMemory(@header.SkyName[0], @res[1], Min(Length(res), 64));
  end;
 end;

 if cbMusic.Checked then
 begin
  res := header.MusicName;
  g_ProcessResourceStr(res, @filename, @section, @resource);

  if (not cbNonStandart.Checked) or
     ((AnsiLowerCase(filename) <> 'standart.wad') and
      (AnsiLowerCase(filename) <> 'shrshade.wad')) then
  begin
   if not ProcessResource(wad, msection, filename, section, resource) then
   begin
    mr.Destroy();
    wad.Destroy();
    Exit;
   end;

   res := Format(':%s\%s', [msection, resource]);
   ZeroMemory(@header.MusicName[0], 64);
   CopyMemory(@header.MusicName[0], @res[1], Min(Length(res), 64));
  end;
 end;

 mw := TMapWriter_2.Create();
 mw.AddHeader(header);
 mw.AddTextures(textures);
 mw.AddPanels(mr.GetPanels());
 mw.AddItems(mr.GetItems());
 mw.AddAreas(mr.GetAreas());
 mw.AddMonsters(mr.GetMonsters());
 mw.AddTriggers(mr.GetTriggers());

 len := mw.SaveMap(data);
 WAD.AddResource(data, len, eResource.Text, '');
 WAD.SaveTo(eWAD.Text);

 mw.Destroy();

 MessageDlg(Format('Карта %s вместе с ресурсами сохранена в %s',
                   [eResource.Text, ExtractFileName(eWAD.Text)]),
            mtInformation, [mbOK], 0);

 Close;
end;

procedure TPackMapForm.FormCreate(Sender: TObject);
begin
 SaveDialog.InitialDir := EditorDir;
end;

end.
