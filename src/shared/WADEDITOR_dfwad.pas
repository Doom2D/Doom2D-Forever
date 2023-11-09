{$INCLUDE ../shared/a_modes.inc}

unit WADEDITOR_dfwad;

interface

  uses Classes, WADEDITOR;

  type
    TData = class
      ref: Integer;          // number of links
      pos: Int64;            // position in source (if pos < 0 -> not in source file)
      csize: Int64;          // compressed size
      usize: Int64;          // decompressed size (usize < 0 -> unknown)
      stream: TMemoryStream; // copy of compressed data
    end;

    TResource = record
      name: AnsiString;
      data: TData;
    end;

    TSection = record
      name: AnsiString;
      list: array of TResource;
    end;

    PResource = ^TResource;
    PSection = ^TSection;

    TDFWEditor = class sealed(WADEDITOR.TWADEditor)
      private
        FSection: array of TSection;
        FData: array of TData;
        FStream: TStream;
        FLastError: Integer;
        FVersion: Byte;

        function FindSectionIDRAW(name: AnsiString; caseSensitive: Boolean): Integer;
        function FindSectionRAW(name: AnsiString; caseSensitive: Boolean): PSection;
        function InsertSectionRAW(name: AnsiString): PSection;

        function FindSectionID(name: AnsiString): Integer;
        function FindSection(name: AnsiString): PSection;
        function InsertSection(name: AnsiString): PSection;

        function FindDataID(pos: Int64): Integer;
        function FindData(pos: Int64): TData;
        function InsertData(ref, pos, csize, usize: Int64; stream: TMemoryStream): TData;

        function InsertFileInfoS(p: PSection; const name: AnsiString; pos, csize, usize: Int64; stream: TMemoryStream): PResource;
        function InsertFileInfo(const section, name: AnsiString; pos, csize, usize: Int64; stream: TMemoryStream): PResource;
        function Preload(data: TData): Boolean;
        function GetSourceStream(p: PResource): TStream;

        procedure Clear();
        procedure Collect();
        procedure ReadFromStream(s: TStream);
        procedure SaveToStream(s: TStream);

      public
        constructor Create();
        destructor Destroy(); override;
        procedure FreeWAD(); override;
        function  ReadFile2(FileName: string): Boolean; override;
        function  ReadMemory(Data: Pointer; Len: LongWord): Boolean; override;
        procedure CreateImage(); override;
        function AddResource(Data: Pointer; Len: LongWord; Name, Section: String): Boolean; override; overload;
        function AddResource(FileName, Name, Section: String): Boolean; override; overload;
        function AddAlias(Res, Alias: String): Boolean; override;
        procedure AddSection(Name: String); override;
        procedure RemoveResource(Section, Resource: String); override;
        procedure SaveTo(FileName: String); override;
        function HaveResource(Section, Resource: String): Boolean; override;
        function HaveSection(Section: string): Boolean; override;
        function GetResource(Section, Resource: String; var pData: Pointer; var Len: Integer): Boolean; override;
        function GetSectionList(): SArray; override;
        function GetResourcesList(Section: String): SArray; override;

        function GetLastError: Integer; override;
        function GetLastErrorStr: String; override;
        function GetResourcesCount: Word; override;
        function GetVersion: Byte; override;
    end;

implementation

  uses SysUtils, StrUtils, DateUtils, Math, utils, zstream, crc, e_log;

  function PrepString(const s: AnsiString; caseSensitive, extSensitive: Boolean): AnsiString; inline;
    var i: Integer;
  begin
    Result := s;
    if caseSensitive = False then
    begin
      Result := UpperCase(Result);
    end;
    if extSensitive = False then
    begin
      i := Pos('.', Result); // fix dotfiles
      if i > 1 then
        SetLength(Result, i - 1);
    end;
  end;

  function FindResourceIDRAW(p: PSection; name: AnsiString; caseSensitive, extSensitive: Boolean): Integer;
    var i: Integer; pname: AnsiString;
  begin
    if p <> nil then
    begin
      pname := PrepString(name, caseSensitive, extSensitive);
      for i := 0 to High(p.list) do
      begin
        if PrepString(p.list[i].name, caseSensitive, extSensitive) = pname then
        begin
          Result := i;
          exit;
        end;
      end;
    end;
    Result := -1;
  end;

  function FindResourceID(p: PSection; name: AnsiString): Integer;
    var i: Integer;
  begin
    i := FindResourceIDRAW(p, name, True, True); // CaSeNaMe.Ext
    if i < 0 then
    begin
      i := FindResourceIDRAW(p, name, False, True); // CASENAME.EXT
      if i < 0 then
      begin
        i := FindResourceIDRAW(p, name, True, False); // CaSeNaMe
        if i < 0 then
        begin
          i := FindResourceIDRAW(p, name, False, False); // CASENAME
        end;
      end;
    end;
    Result := i;
  end;

  function FindResource(p: PSection; name: AnsiString): PResource;
    var i: Integer;
  begin
    i := FindResourceID(p, name);
    if i >= 0 then Result := @p.list[i] else Result := nil;
  end;



  function TDFWEditor.FindSectionIDRAW(name: AnsiString; caseSensitive: Boolean): Integer;
    var i: Integer; pname: AnsiString;
  begin
    if FSection <> nil then
    begin
      pname := PrepString(name, caseSensitive, True);
      for i := 0 to High(FSection) do
      begin
        if PrepString(FSection[i].name, caseSensitive, True) = pname then
        begin
          Result := i;
          exit;
        end;
      end;
    end;
    Result := -1;
  end;

  function TDFWEditor.FindSectionRAW(name: AnsiString; caseSensitive: Boolean): PSection;
    var i: Integer;
  begin
    i := FindSectionIDRAW(name, caseSensitive);
    if i >= 0 then Result := @FSection[i] else Result := nil;
  end;

  function TDFWEditor.InsertSectionRAW(name: AnsiString): PSection;
    var i: Integer;
  begin
    if FSection = nil then i := 0 else i := Length(FSection);
    SetLength(FSection, i + 1);
    FSection[i] := Default(TSection);
    FSection[i].name := name;
    Result := @FSection[i];
  end;



  function TDFWEditor.FindSectionID(name: AnsiString): Integer;
    var fixName: AnsiString;
  begin
    fixName := StringReplace(name, '\', '/', [rfReplaceAll], TStringReplaceAlgorithm.sraManySmall);
    Result := FindSectionIDRAW(fixName, True); // CaSeNaMe
    if Result < 0 then
      Result := FindSectionIDRAW(fixName, False); // CASENAME
  end;

  function TDFWEditor.FindSection(name: AnsiString): PSection;
    var fixName: AnsiString;
  begin
    fixName := StringReplace(name, '\', '/', [rfReplaceAll], TStringReplaceAlgorithm.sraManySmall);
    Result := FindSectionRAW(fixName, True); // CaSeNaMe
    if Result = nil then
      Result := FindSectionRAW(fixName, False); // CASENAME
  end;

  function TDFWEditor.InsertSection(name: AnsiString): PSection;
  begin
    Result := FindSection(name);
    if Result = nil then
      Result := InsertSectionRAW(name);
  end;



  function TDFWEditor.FindDataID(pos: Int64): Integer;
    var i: Integer;
  begin
    if (pos >= 0) and (FData <> nil) then
    begin
      for i := 0 to High(FData) do
      begin
        if FData[i].pos = pos then
        begin
          Result := i;
          exit;
        end;
      end;
    end;
    Result := -1;
  end;

  function TDFWEditor.FindData(pos: Int64): TData;
    var i: Integer;
  begin
    i := FindDataID(pos);
    if i >= 0 then Result := FData[i] else Result := nil;
  end;

  function TDFWEditor.InsertData(ref, pos, csize, usize: Int64; stream: TMemoryStream): TData;
    var i: Integer; data: TData;
  begin
    data := TData.Create();
    data.ref := ref;
    data.pos := pos;
    data.csize := csize;
    data.usize := usize;
    data.stream := stream;

    if FData = nil then i := 0 else i := Length(FData);
    SetLength(FData, i + 1);
    FData[i] := data;
    Result := data;
  end;



  function TDFWEditor.InsertFileInfoS(p: PSection; const name: AnsiString; pos, csize, usize: Int64; stream: TMemoryStream): PResource;
    var i: Integer; data: TData;
  begin
    Result := nil;
    if p = nil then
      exit;

    data := FindData(pos);
    if data = nil then
      data := InsertData(0, pos, csize, usize, stream);

    if p.list = nil then i := 0 else i := Length(p.list);
    SetLength(p.list, i + 1);
    Inc(data.ref);
    p.list[i] := Default(TResource);
    p.list[i].name := name;
    p.list[i].data := data;
    Result := @p.list[i];
  end;

  function TDFWEditor.InsertFileInfo(const section, name: AnsiString; pos, csize, usize: Int64; stream: TMemoryStream): PResource;
    var p: PSection;
  begin
    p := FindSectionRAW(section, True);
    if p = nil then
      p := InsertSectionRAW(section);

    Result := InsertFileInfoS(p, name, pos, csize, usize, stream);
  end;



  function TDFWEditor.AddAlias(Res, Alias: String): Boolean;
  begin
    // New hard-links are not supported
    // However, they never created by editor
    Result := False;
  end;

  function TDFWEditor.AddResource(Data: Pointer; Len: LongWord; Name, Section: String): Boolean;
    const level: TCompressionLevel = TCompressionLevel.clMax;
    var s: TMemoryStream; cs: TCompressionStream; p: PResource;
  begin
    Name := win2utf(Name);
    Section := win2utf(Section);
    Result := False;
    if Name <> '' then
    begin
      s := TMemoryStream.Create();
      try
        cs := TCompressionStream.Create(level, s, False);
        try
          cs.WriteBuffer(PByte(Data)[0], Len);
          cs.Flush();
        finally
          cs.Free();
        end;
        p := InsertFileInfo(Section, Name, -1, s.Size, Len, s);
        Result := p <> nil;
      except
        s.Free();
        raise;
      end;
    end;
  end;

  function TDFWEditor.AddResource(FileName, Name, Section: String): Boolean;
    var s: TFileStream; ptr: PByte;
  begin
    Result := False;
    FLastError := DFWAD_ERROR_READWAD;
    try
      s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        GetMem(ptr, s.Size);
        try
          s.ReadBuffer(ptr[0], s.Size);
          Result := AddResource(ptr, s.Size, Name, Section);
          if Result = True then FLastError := DFWAD_NOERROR;
        finally
          FreeMem(ptr);
        end;
      finally
        s.Free();
      end;
    except
      on e: EFOpenError do
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_INFO then
          e_WriteLog('DFWAD: AddResource: failed to open file ' + FileName, MSG_NOTIFY);
        FLastError := DFWAD_ERROR_CANTOPENWAD;
      end;
    end;
  end;

  constructor TDFWEditor.Create();
  begin
    FSection := nil;
    FData := nil;
    FStream := nil;
    FLastError := DFWAD_NOERROR;
    FVersion := 1;
    FreeWAD();
  end;

  destructor TDFWEditor.Destroy();
  begin
    Clear();
    inherited;
  end;

  procedure TDFWEditor.Clear();
    var i: Integer;
  begin
    if FSection <> nil then
    begin
      for i := 0 to High(FSection) do
        if FSection[i].list <> nil then
          SetLength(FSection[i].list, 0);
      SetLength(FSection, 0);
    end;
    if FData <> nil then
    begin
      for i := 0 to High(FData) do
      begin
        if FData[i] <> nil then
        begin
          if FData[i].stream <> nil then
            FreeAndNil(FData[i].stream);
          FreeAndNil(FData[i]);
        end;
      end;
      SetLength(FData, 0);
    end;
    if FStream <> nil then
      FreeAndNil(FStream);
  end;

  procedure TDFWEditor.FreeWAD();
  begin
    Clear();
    FLastError := DFWAD_NOERROR;
    FVersion := 1;
  end;

  function TDFWEditor.Preload(data: TData): Boolean;
    var s: TMemoryStream;
  begin
    Result := False;
    if data <> nil then
    begin
      Result := data.stream <> nil;
      if (data.stream = nil) and (FStream <> nil) then
      begin
        s := TMemoryStream.Create();
        try
          if data.csize > 0 then
          begin
            FStream.Seek(data.pos, TSeekOrigin.soBeginning);
            s.CopyFrom(FStream, data.csize);
          end;
          Assert(s.Size = data.csize); // wtf, random size if copied zero bytes!
          data.stream := s;
          Result := True;
        except
          s.Free();
        end;
      end;
    end;
  end;

  procedure TDFWEditor.CreateImage();
    var i, j: Integer;
  begin
    if FStream = nil then
    begin
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        e_WriteLog('DFWAD: CreateImage: File not assigned', MSG_NOTIFY);
      FLastError := DFWAD_ERROR_WADNOTLOADED;
    end
    else if FStream is TMemoryStream then
    begin
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        e_WriteLog('DFWAD: CreateImage: Memory stream', MSG_NOTIFY);
      FLastError := DFWAD_NOERROR;
    end
    else
    begin
      if FSection <> nil then
      begin
        for i := 0 to High(FData) do
        begin
          if Preload(FData[i]) = False then
          begin
            if gWADEditorLogLevel >= DFWAD_LOG_WARN then
              e_WriteLog('DFWAD: CreateImage: failed to preload resource data #' + IntToStr(i), MSG_WARNING);
            FLastError := DFWAD_ERROR_CANTOPENWAD;
            exit;
          end;
          FData[i].pos := -1;
        end;
      end;
      FreeAndNil(FStream);
      FLastError := DFWAD_NOERROR;
    end;
  end;

  procedure TDFWEditor.AddSection(Name: String);
  begin
    Name := win2utf(Name);
    if InsertSection(Name) = nil then
      raise Exception.Create('DFWAD: AddSection[' + Name + ']: failed to insert');
  end;

  function TDFWEditor.HaveResource(Section, Resource: String): Boolean;
  begin
    Section := win2utf(Section);
    Resource := win2utf(Resource);
    Result := FindResource(FindSection(Section), Resource) <> nil;
  end;

  function TDFWEditor.HaveSection(Section: String): Boolean;
  begin
    Section := win2utf(Section);
    Result := FindSection(Section) <> nil;
  end;

  function TDFWEditor.GetSourceStream(p: PResource): TStream;
    var src: TStream;
  begin
    src := nil;
    if p.data.stream <> nil then
    begin
      src := p.data.stream;
      src.Seek(0, TSeekOrigin.soBeginning);
    end
    else if (p.data.pos >= 0) and (FStream <> nil) then
    begin
      src := FStream;
      src.Seek(p.data.pos, TSeekOrigin.soBeginning);
    end;
    Result := src;
  end;

  function TDFWEditor.GetResource(Section, Resource: String; var pData: Pointer; var Len: Integer): Boolean;
    const BLOCK_STEP = 4096;
    var p: PResource; src: TStream; tmp: TDecompressionStream; ptr: PByte; size, r: Int64;
  begin
    Section := win2utf(Section);
    Resource := win2utf(Resource);
    FLastError := DFWAD_ERROR_CANTOPENWAD;
    Result := False;
    pData := nil;
    Len := 0;
    p := FindResource(FindSection(Section), Resource);
    if p <> nil then
    begin
      src := GetSourceStream(p);
      if src <> nil then
      begin
        try
          tmp := TDecompressionStream.Create(src, False);
          try
            if p.data.usize < 0 then
            begin
              size := 0;
              GetMem(ptr, BLOCK_STEP);
              try
                repeat
                  r := tmp.Read(ptr[size], BLOCK_STEP);
                  size := size + r;
                  if r <> 0 then
                    ReallocMem(ptr, size + BLOCK_STEP);
                until r = 0;
                ReallocMem(ptr, size);
                p.data.usize := size; // cache size
                pData := ptr;
                Len := size;
                Result := True;
                FLastError := DFWAD_NOERROR;
              except
                FreeMem(ptr);
                raise;
              end;
            end
            else
            begin
              GetMem(ptr, p.data.usize);
              try
                tmp.ReadBuffer(ptr[0], p.data.usize);
                pData := ptr;
                Len := p.data.usize;
                Result := True;
                FLastError := DFWAD_NOERROR;
              except
                FreeMem(ptr);
                raise;
              end;
            end;
          finally
            tmp.Free();
          end;
        except
          on e: EStreamError do
          begin
            if gWADEditorLogLevel >= DFWAD_LOG_INFO then
              e_WriteLog('DFWAD: Failed to decompress DEFLATEd data, reason: ' + e.Message, MSG_WARNING);
            raise e;
          end;
        end;
      end
      else
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_WARN then
          e_WriteLog('DFWAD: No available source for file data', MSG_WARNING);
        FLastError := DFWAD_ERROR_WADNOTLOADED;
      end;
    end
    else
    begin
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        e_WriteLog('DFWAD: Resource not found', MSG_NOTIFY);
      FLastError := DFWAD_ERROR_RESOURCENOTFOUND;
    end;
  end;

  function TDFWEditor.GetResourcesList(Section: String): SArray;
    var p: PSection; i: Integer;
  begin
    Section := win2utf(Section);
    Result := nil;
    p := FindSection(Section);
    if (p <> nil) and (p.list <> nil) then
    begin
      SetLength(Result, Length(p.list));
      for i := 0 to High(p.list) do
      begin
        Result[i] := utf2win(p.list[i].name);
      end;
    end;
  end;

  function TDFWEditor.GetSectionList(): SArray;
    var i: Integer;
  begin
    Result := nil;
    if FSection <> nil then
    begin
      SetLength(Result, Length(FSection));
      for i := 0 to High(FSection) do
      begin
        Result[i] := utf2win(FSection[i].name);
      end;
    end;
  end;

  procedure TDFWEditor.ReadFromStream(s: TStream);
    var sig: packed array [0..4] of Char;
    var ver: UInt8; nrec: UInt16; offset, csize: UInt32;
    var name1251: packed array [0..16] of Char;
    var section, name: AnsiString;
    var i: Integer;
    var sec: PSection;
    var res: PResource;
  begin
    s.ReadBuffer(sig[0], 5);
    if sig = 'DFWAD' then
    begin
      ver := s.ReadByte();
      if ver = 1 then
      begin
        nrec := LEtoN(s.ReadWord());
        section := '';
        sec := nil;
        for i := 0 to nrec - 1 do
        begin
          s.ReadBuffer(name1251[0], 16);
          name1251[16] := #0;
          name := win2utf(PChar(@name1251[0]));
          offset := LEtoN(s.ReadDWord());
          csize := LEtoN(s.ReadDWord());
          if csize = 0 then
          begin
            section := name;
            sec := InsertSectionRAW(section);
            if sec = nil then
              raise Exception.Create('Failed to register section [' + section + ']');
          end
          else
          begin
            if sec = nil then
              sec := InsertSectionRAW('');
            if sec = nil then
              raise Exception.Create('Failed to create root section');
            res := InsertFileInfoS(sec, name, offset, csize, -1, nil);
            if res = nil then
              raise Exception.Create('Failed to register resource [' + section + '][' + name + ']');
            if res.data.csize <> csize then
              raise Exception.Create('Invalid compressed size for [' + section + '][' + name + '] (corrupted archive?)');
          end;
        end;
      end
      else
      begin
        FLastError := DFWAD_ERROR_WRONGVERSION;
        raise Exception.Create('Unsupported DFWAD version ' + IntToStr(ver) + ' (expected 1)');
      end;
    end
    else
    begin
      FLastError := DFWAD_ERROR_FILENOTWAD;
      raise Exception.Create('Not DFWAD file');
    end;
  end;

  function TDFWEditor.ReadFile2(FileName: String): Boolean;
    var s: TFileStream;
  begin
    FreeWAD();
    Result := False;
    try
      s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        ReadFromStream(s);
        FStream := s;
        FLastError := DFWAD_NOERROR;
        Result := True;
      except
        s.Free();
        raise;
      end;
    except
      on e: EFOpenError do
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_INFO then
          e_WriteLog('DFWAD: Failed to open file ' + FileName + ', reason: ' + e.Message, MSG_WARNING);
        if FileExists(FileName) then
          FLastError := DFWAD_ERROR_CANTOPENWAD
        else
          FLastError := DFWAD_ERROR_WADNOTFOUND;
      end;
      on e: Exception do
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_INFO then
          e_WriteLog('DFWAD: Failed to read DFWAD from file ' + FileName + ', reason: ' + e.Message, MSG_WARNING);
        Clear();
      end;
    end;
  end;

  function TDFWEditor.ReadMemory(Data: Pointer; Len: LongWord): Boolean;
    var s: TMemoryStream;
  begin
    FreeWAD();
    Result := False;
    try
      s := TMemoryStream.Create;
      try
        s.SetSize(Len);
        s.WriteBuffer(PByte(Data)[0], Len);
        s.Seek(0, soBeginning);
        ReadFromStream(s);
        FStream := s;
        FLastError := DFWAD_NOERROR;
        Result := True;
      except
        s.Free();
        raise;
      end;
    except
      on e: Exception do
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_INFO then
          e_WriteLog('DFWAD: Failed to read DFWAD from memory, reason: ' + e.Message, MSG_WARNING);
        Clear();
      end;
    end;
  end;

  procedure TDFWEditor.Collect();
    var i, n: Integer;
  begin
    if FData <> nil then
    begin
      n := 0;
      for i := 0 to High(FData) do
      begin
        if FData[i] <> nil then
        begin
          if FData[i].ref > 0 then
          begin
            FData[n] := FData[i];
            Inc(n);
          end
          else
          begin
            if FData[i].stream <> nil then
              FreeAndNil(FData[i].stream);
            FreeAndNil(FData[i]);
          end;
        end;
      end;
      SetLength(FData, n);
    end;
  end;

  procedure TDFWEditor.RemoveResource(Section, Resource: String);
    var p: PSection; i: Integer; data: TData;
  begin
    Section := win2utf(Section);
    Resource := win2utf(Resource);
    p := FindSection(Section);
    i := FindResourceID(p, Resource);
    if i >= 0 then
    begin
      data := p.list[i].data;
      for i := i + 1 to High(p.list) do
      begin
        p.list[i - 1] := p.list[i];
      end;
      SetLength(p.list, High(p.list));
      Dec(data.ref);
      if data.ref <= 0 then
        Collect();
    end;
  end;

  procedure TDFWEditor.SaveToStream(s: TStream);
    type TName16 = packed array [0..16] of Char;
    var count: UInt16;
    var name1251: TName16;
    var i, j: Integer;
    var p: PResource;
    var data: TData;

    function GetOffset(data: TData): UInt32;
      var i: Integer;
    begin
      Assert(data <> nil);
      Result := 6 + 2 + count * 24;
      for i := 0 to High(FData) do
      begin
        if FData[i] = data then
          exit;
        if FData[i] <> nil then
          Result := Result + FData[i].csize;
      end;
      raise Exception.Create('Failed to calculate offset (BUG!)');
    end;

  begin
    count := GetResourcesCount();
    s.WriteBuffer('DFWAD', 5);
    s.WriteByte(1);
    WriteInt(s, UInt16(count));
    if FSection <> nil then
    begin
      for i := 0 to High(FSection) do
      begin
        if (i <> 0) or (FSection[i].name <> '') then
        begin
          name1251 := Default(TName16);
          name1251 := utf2win(FSection[i].name);
          s.WriteBuffer(name1251[0], 16);
          WriteInt(s, UInt32(0));
          WriteInt(s, UInt32(0));
        end;
        if FSection[i].list <> nil then
        begin
          for j := 0 to High(FSection[i].list) do
          begin
            p := @FSection[i].list[j];
            name1251 := Default(TName16);
            name1251 := utf2win(p.name);
            s.WriteBuffer(name1251[0], 16);
            WriteInt(s, UInt32(GetOffset(p.data)));
            WriteInt(s, UInt32(p.data.csize));
          end;
        end;
      end;
      if FData <> nil then
      begin
        for i := 0 to High(FData) do
        begin
          data := FData[i];
          if data <> nil then
          begin
            Assert(s.Position = GetOffset(data));
            if data.stream <> nil then
            begin
              Assert(data.stream.Size = data.csize);
              data.stream.SaveToStream(s);
            end
            else if (data.pos >= 0) and (FStream <> nil) then
            begin
              FStream.Seek(data.pos, TSeekOrigin.soBeginning);
              s.CopyFrom(FStream, data.csize);
            end
            else
            begin
              raise Exception.Create('No data source available (somethig very wrong)');
            end;
          end;
        end;
      end;
    end;
  end;

  procedure TDFWEditor.SaveTo(FileName: String);
    var s: TFileStream;
  begin
    try
      s := TFileStream.Create(FileName, fmCreate);
      try
        SaveToStream(s);
      finally
        s.Free();
      end;
    except
      on e: Exception do
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_INFO then
          e_WriteLog('DFWAD: Failed to create file ' + FileName + ', reason: ' + e.Message, MSG_WARNING);
        raise e;
      end;
    end;
  end;

  function TDFWEditor.GetLastError: Integer;
  begin
    Result := FLastError;
  end;

  function TDFWEditor.GetLastErrorStr: String;
  begin
    case FLastError of
      DFWAD_NOERROR: Result := '';
      DFWAD_ERROR_WADNOTFOUND: Result := 'DFWAD file not found';
      DFWAD_ERROR_CANTOPENWAD: Result := 'Can''t open DFWAD file';
      DFWAD_ERROR_RESOURCENOTFOUND: Result := 'Resource not found';
      DFWAD_ERROR_FILENOTWAD: Result := 'File is not DFWAD';
      DFWAD_ERROR_WADNOTLOADED: Result := 'DFWAD file is not loaded';
      DFWAD_ERROR_READRESOURCE: Result := 'Read resource error';
      DFWAD_ERROR_READWAD: Result := 'Read DFWAD error';
      otherwise Result := IntToStr(FLastError);
    end;
  end;

  function TDFWEditor.GetResourcesCount: Word;
    var i: Integer;
  begin
    Result := 0;
    if FSection <> nil then
    begin
      Result := Result + Length(FSection);
      for i := 0 to High(FSection) do
        if FSection[i].list <> nil then
          Result := Result + Length(FSection[i].list);
      if FSection[0].name = '' then
        Dec(Result); // First root section not counted
    end;
  end;

  function TDFWEditor.GetVersion: Byte;
  begin
    Result := FVersion;
  end;

begin
  gWADEditorFactory.RegisterEditor('DFWAD', TDFWEditor);
end.
