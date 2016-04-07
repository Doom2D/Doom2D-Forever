unit WADEDITOR;

{
-----------------------------------
WADEDITOR.PAS ВЕРСИЯ ОТ 26.08.08

Поддержка вадов версии 1
-----------------------------------
}

interface

uses
  sfs, xstreams;


type
  SArray = array of ShortString;

  TWADEditor_1 = class(TObject)
  private
    fFileName: string; // empty: not opened
    fIter: TSFSFileList;

    function getIsOpen (): Boolean;

   public
    constructor Create();
    destructor Destroy(); override;

    procedure FreeWAD();

    function ReadFile (FileName: string): Boolean;
    function ReadMemory (Data: Pointer; Len: LongWord): Boolean;
    function GetResource (Section, Resource: string; var pData: Pointer; var Len: Integer): Boolean;
    function GetResourcesList (Section: string): SArray;

    property isOpen: Boolean read getIsOpen;
  end;

{
const
  DFWAD_NOERROR                = 0;
  DFWAD_ERROR_WADNOTFOUND      = -1;
  DFWAD_ERROR_CANTOPENWAD      = -2;
  DFWAD_ERROR_RESOURCENOTFOUND = -3;
  DFWAD_ERROR_FILENOTWAD       = -4;
  DFWAD_ERROR_WADNOTLOADED     = -5;
  DFWAD_ERROR_READRESOURCE     = -6;
  DFWAD_ERROR_READWAD          = -7;
  DFWAD_ERROR_WRONGVERSION     = -8;
}


procedure g_ProcessResourceStr (ResourceStr: String; var FileName, SectionName, ResourceName: String); overload;
procedure g_ProcessResourceStr (ResourceStr: String; FileName, SectionName, ResourceName: PString); overload;


implementation

uses
  SysUtils, Classes, BinEditor, e_log;


procedure g_ProcessResourceStr (ResourceStr: String; var FileName, SectionName, ResourceName: String);
var
  a, i: Integer;

begin
  //e_WriteLog(Format('g_ProcessResourceStr0: [%s]', [ResourceStr]), MSG_NOTIFY);
  for i := Length(ResourceStr) downto 1 do
    if ResourceStr[i] = ':' then
      Break;

  FileName := Copy(ResourceStr, 1, i-1);

  for a := i+1 to Length(ResourceStr) do
    if (ResourceStr[a] = '\') or (ResourceStr[a] = '/') then Break;

  ResourceName := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
  SectionName := Copy(ResourceStr, i+1, Length(ResourceStr)-Length(ResourceName)-Length(FileName)-2);
end;


procedure g_ProcessResourceStr (ResourceStr: AnsiString; FileName, SectionName, ResourceName: PAnsiString);
var
  a, i, l1, l2: Integer;

begin
  //e_WriteLog(Format('g_ProcessResourceStr1: [%s]', [ResourceStr]), MSG_NOTIFY);
  for i := Length(ResourceStr) downto 1 do
    if ResourceStr[i] = ':' then
      Break;

  if FileName <> nil then
    begin
      FileName^ := Copy(ResourceStr, 1, i-1);
      l1 := Length(FileName^);
    end
  else
    l1 := 0;

  for a := i+1 to Length(ResourceStr) do
    if (ResourceStr[a] = '\') or (ResourceStr[a] = '/') then Break;

  if ResourceName <> nil then
    begin
      ResourceName^ := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
      l2 := Length(ResourceName^);
    end
  else
    l2 := 0;

  if SectionName <> nil then
    SectionName^ := Copy(ResourceStr, i+1, Length(ResourceStr)-l2-l1-2);
end;


{ TWADEditor_1 }
constructor TWADEditor_1.Create();
begin
  fFileName := '';
end;


destructor TWADEditor_1.Destroy();
begin
  FreeWAD();
  inherited;
end;


function TWADEditor_1.getIsOpen (): Boolean;
begin
  result := (fFileName <> '');
end;


procedure TWADEditor_1.FreeWAD();
begin
  if fIter <> nil then FreeAndNil(fIter);
  if fFileName <> '' then e_WriteLog(Format('TWADEditor_1.ReadFile: [%s] closed', [fFileName]), MSG_NOTIFY);
  fFileName := '';
end;


function TWADEditor_1.GetResource (Section, Resource: string; var pData: Pointer; var Len: Integer): Boolean;
var
  f: Integer;
  fi: TSFSFileInfo;
  fs: TStream;
  fn: string;
begin
  Result := False;
  if not isOpen or (fIter = nil) then Exit;
  if (length(Section) <> 0) and (Section[length(Section)] <> '/') then Section := Section+'/';
  for f := 0 to fIter.Count-1 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    //e_WriteLog(Format('DFWAD: searching for [%s : %s] in [%s]; current is [%s : %s] (%d, %d)', [Section, Resource, fFileName, fi.path, fi.name, SFSStrComp(fi.path, Section), SFSStrComp(fi.name, Resource)]), MSG_NOTIFY);
    if (SFSStrComp(fi.path, Section) = 0) and (SFSStrComp(fi.name, Resource) = 0) then
    begin
      // i found her!
      fn := fFileName+'::'+fi.path+fi.name;
      fs := SFSFileOpen(fn);
      if fs = nil then
      begin
        e_WriteLog(Format('DFWAD: can''t open file [%s]', [fn]), MSG_NOTIFY);
        break;
      end;
      Len := Integer(fs.size);
      GetMem(pData, Len);
      fs.ReadBuffer(pData^, Len);
      fs.Free;
      result := true;
      e_WriteLog(Format('DFWAD: file [%s%s] FOUND in [%s]; size is %d bytes', [Section, Resource, fFileName, Len]), MSG_NOTIFY);
      exit;
    end;
  end;
  e_WriteLog(Format('DFWAD: file [%s%s] not found in [%s]', [Section, Resource, fFileName]), MSG_WARNING);
end;


function TWADEditor_1.GetResourcesList (Section: string): SArray;
var
  f: Integer;
  fi: TSFSFileInfo;
begin
  Result := nil;
  if not isOpen or (fIter = nil) then Exit;
  if (length(Section) <> 0) and (Section[length(Section)] <> '/') then Section := Section+'/';
  for f := 0 to fIter.Count-1 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    if SFSStrComp(fi.path, Section) = 0 then
    begin
      SetLength(result, Length(result)+1);
      result[high(result)] := fi.name;
    end;
  end;
end;


function TWADEditor_1.ReadFile (FileName: string): Boolean;
begin
  Result := False;
  e_WriteLog(Format('TWADEditor_1.ReadFile: [%s]', [FileName]), MSG_NOTIFY);
  FreeWAD();
  if not FileExists(FileName) then Exit;
  fIter := SFSFileList(FileName);
  if fIter = nil then Exit;
  fFileName := FileName;
  e_WriteLog(Format('TWADEditor_1.ReadFile: [%s] opened', [fFileName]), MSG_NOTIFY);
  Result := True;
end;


var
  uniqueCounter: Integer = 0;

function TWADEditor_1.ReadMemory (Data: Pointer; Len: LongWord): Boolean;
var
  Signature: array[0..4] of Char;
  a: Integer;
  fn: string;
  st: TStream = nil;
begin
  Result := False;
  FreeWAD();
  if (Data = nil) or (Len = 0) then Exit;

  fn := Format(' -- memwad %d -- ', [uniqueCounter]);
  Inc(uniqueCounter);
  e_WriteLog(Format('TWADEditor_1.ReadMemory: [%s]', [fn]), MSG_NOTIFY);

  try
    st := TSFSMemoryStreamRO.Create(Data, Len);
    if not SFSAddSubDataFile(fn, st) then
    begin
      st.Free;
      Exit;
    end;
  except
    st.Free;
    Exit;
  end;

  fIter := SFSFileList(fn);
  if fIter = nil then Exit;

  fFileName := fn;
  e_WriteLog(Format('TWADEditor_1.ReadMemory: [%s] opened', [fFileName]), MSG_NOTIFY);

  Result := True;
end;


begin
  sfsDiskDirs := '<exedir>/data'; //FIXME
end.
