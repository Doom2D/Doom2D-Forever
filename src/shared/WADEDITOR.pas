unit WADEDITOR;

{$DEFINE SFS_DWFAD_DEBUG}

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

// return fixed string or empty string
function findDiskWad (fname: string): string;


implementation

uses
  SysUtils, Classes, BinEditor, e_log, g_options;


function findDiskWad (fname: string): string;
var
  path, rfn: string;
begin
  result := '';
  path := ExtractFilePath(fname);
  rfn := ExtractFileName(fname);
  if not sfsFindFileCI(path, rfn) then
  begin
    //e_WriteLog(Format('TWADEditor_1.ReadFile: error looking for [%s] [%s]', [path, ExtractFileName(fname)]), MSG_NOTIFY);
    if SFSStrEqu(ExtractFileExt(fname), '.wad') then
    begin
      rfn := ChangeFileExt(ExtractFileName(fname), '.pk3');
      //e_WriteLog(Format('  looking for [%s] [%s]', [path, rfn]), MSG_NOTIFY);
      if not sfsFindFileCI(path, rfn) then
      begin
        //e_WriteLog(Format('  looking for [%s] [%s]', [path, rfn]), MSG_NOTIFY);
        rfn := ChangeFileExt(ExtractFileName(fname), '.zip');
        if not sfsFindFileCI(path, rfn) then exit;
      end;
    end
    else
    begin
      exit;
    end;
    //e_WriteLog(Format('TWADEditor_1.ReadFile: FOUND [%s]', [rfn]), MSG_NOTIFY);
  end
  else
  begin
    //if rfn <> ExtractFileName(FileName) then e_WriteLog(Format('TWADEditor_1.ReadFile: FOUND [%s]', [rfn]), MSG_NOTIFY);
  end;
  result := path+rfn;
end;


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
  //if fFileName <> '' then e_WriteLog(Format('TWADEditor_1.ReadFile: [%s] closed', [fFileName]), MSG_NOTIFY);
  fFileName := '';
end;


function removeExt (s: string): string;
var
  i: Integer;
begin
  i := length(s)+1;
  while (i > 1) and (s[i-1] <> '.') and (s[i-1] <> '/') do Dec(i);
  if (i > 1) and (s[i-1] = '.') then
  begin
    //writeln('[', s, '] -> [', Copy(s, 1, i-2), ']');
    s := Copy(s, 1, i-2);
  end;
  result := s;
end;

function TWADEditor_1.GetResource (Section, Resource: string; var pData: Pointer; var Len: Integer): Boolean;
var
  f: Integer;
  fi: TSFSFileInfo;
  fs: TStream;
  fpp: Pointer;
  //fn: string;
begin
  Result := False;
  if not isOpen or (fIter = nil) then Exit;
  if length(Resource) = 0 then Exit; // just in case
  if (length(Section) <> 0) and (Section[length(Section)] <> '/') then Section := Section+'/';
  // backwards, due to possible similar names and such
  for f := fIter.Count-1 downto 0 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    //e_WriteLog(Format('DFWAD: searching for [%s : %s] in [%s]; current is [%s : %s]', [Section, Resource, fFileName, fi.path, fi.name]), MSG_NOTIFY);
    if {SFSStrEqu}SFSDFPathEqu(fi.path, Section) and SFSStrEqu(removeExt(fi.name), Resource) then
    begin
      // i found her!
      //fn := fFileName+'::'+fi.path+fi.name;
      //fs := SFSFileOpen(fn);
      try
        fs := fIter.volume.OpenFileByIndex(f);
      except
        fs := nil;
      end;
      if fs = nil then
      begin
        e_WriteLog(Format('DFWAD: can''t open file [%s%s] in [%s]', [Section, Resource, fFileName]), MSG_WARNING);
        break;
      end;
      Len := Integer(fs.size);
      GetMem(pData, Len);
      fpp := pData;
      try
        fs.ReadBuffer(pData^, Len);
        fpp := nil;
      finally
        if fpp <> nil then
        begin
          FreeMem(fpp);
          pData := nil;
          Len := 0;
        end;
        fs.Free;
      end;
      result := true;
      {$IFDEF SFS_DWFAD_DEBUG}
      if gSFSDebug then
        e_WriteLog(Format('DFWAD: file [%s%s] FOUND in [%s]; size is %d bytes', [Section, Resource, fFileName, Len]), MSG_NOTIFY);
      {$ENDIF}
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
    if length(fi.name) = 0 then continue;
    if {SFSStrEqu}SFSDFPathEqu(fi.path, Section) then
    begin
      SetLength(result, Length(result)+1);
      result[high(result)] := removeExt(fi.name);
    end;
  end;
end;


function TWADEditor_1.ReadFile (FileName: string): Boolean;
var
  rfn, path: string;
  //f: Integer;
  //fi: TSFSFileInfo;
begin
  Result := False;
  //e_WriteLog(Format('TWADEditor_1.ReadFile: [%s]', [FileName]), MSG_NOTIFY);
  FreeWAD();
  rfn := findDiskWad(FileName);
  if length(rfn) = 0 then
  begin
    e_WriteLog(Format('TWADEditor_1.ReadFile: error looking for [%s]', [FileName]), MSG_NOTIFY);
    exit;
  end;
  {$IFDEF SFS_DWFAD_DEBUG}
  if gSFSDebug then e_WriteLog(Format('TWADEditor_1.ReadFile: FOUND [%s]', [rfn]), MSG_NOTIFY);
  {$ENDIF}
  // cache this wad
  try
    if gSFSFastMode then
    begin
      if not SFSAddDataFile(rfn, true) then exit;
    end
    else
    begin
      if not SFSAddDataFileTemp(rfn, true) then exit;
    end;
  except
    exit;
  end;
  fIter := SFSFileList(rfn);
  if fIter = nil then Exit;
  fFileName := rfn;
  {$IFDEF SFS_DWFAD_DEBUG}
  if gSFSDebug then e_WriteLog(Format('TWADEditor_1.ReadFile: [%s] opened', [fFileName]), MSG_NOTIFY);
  {$ENDIF}
  Result := True;
end;


var
  uniqueCounter: Integer = 0;

function TWADEditor_1.ReadMemory (Data: Pointer; Len: LongWord): Boolean;
var
  fn: string;
  st: TStream = nil;
  //f: Integer;
  //fi: TSFSFileInfo;
begin
  Result := False;
  FreeWAD();
  if (Data = nil) or (Len = 0) then
  begin
    e_WriteLog('TWADEditor_1.ReadMemory: EMPTY SUBWAD!', MSG_WARNING);
    Exit;
  end;

  fn := Format(' -- memwad %d -- ', [uniqueCounter]);
  Inc(uniqueCounter);
  {$IFDEF SFS_DWFAD_DEBUG}
    e_WriteLog(Format('TWADEditor_1.ReadMemory: [%s]', [fn]), MSG_NOTIFY);
  {$ENDIF}

  try
    st := TSFSMemoryStreamRO.Create(Data, Len);
    if not SFSAddSubDataFile(fn, st, true) then
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
  {$IFDEF SFS_DWFAD_DEBUG}
    e_WriteLog(Format('TWADEditor_1.ReadMemory: [%s] opened', [fFileName]), MSG_NOTIFY);
  {$ENDIF}

  {
  for f := 0 to fIter.Count-1 do
  begin
    fi := fIter.Files[f];
    if fi = nil then continue;
    st := fIter.volume.OpenFileByIndex(f);
    if st = nil then
    begin
      e_WriteLog(Format('[%s]: [%s : %s] CAN''T OPEN', [fFileName, fi.path, fi.name]), MSG_NOTIFY);
    end
    else
    begin
      e_WriteLog(Format('[%s]: [%s : %s] %u', [fFileName, fi.path, fi.name, st.size]), MSG_NOTIFY);
      st.Free;
    end;
  end;
  //fIter.volume.OpenFileByIndex(0);
  }

  Result := True;
end;


begin
  sfsDiskDirs := '<exedir>/data'; //FIXME
end.
