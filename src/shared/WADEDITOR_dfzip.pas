{$INCLUDE ../shared/a_modes.inc}

unit WADEDITOR_dfzip;

// Implementation restrictions:
// - File must start with LFH or EOCD signature
// - EOCD must be located strictly at the end of file
// - Multi-disk ZIP files are not supported
// - Expect UTF-8 or CP1251 encoded names
// - ZIP64 not supported
// - Encryption not supported
// - Zero-length file names not supported
// - CDR holds most actual data about file, LFH mostly ignored
// - Attributes, comments and extra data are ignored and not saved
// - Store and Deflate compression supported

interface

  uses Classes, WADEDITOR;

  type
    TResource = record
      name: AnsiString;
      pos: UInt32;
      csize: UInt32;
      usize: UInt32;
      comp: UInt32;
      chksum: UInt32;
      stream: TMemoryStream;
    end;

    TSection = record
      name: AnsiString;
      list: array of TResource;
    end;

    PResource = ^TResource;
    PSection = ^TSection;

    TZIPEditor = class sealed(WADEDITOR.TWADEditor)
      private
        FSection: array of TSection;
        FStream: TStream;
        FLastError: Integer;
        FVersion: Byte;

        function FindSectionIDRAW(name: AnsiString; caseSensitive: Boolean): Integer;
        function FindSectionRAW(name: AnsiString; caseSensitive: Boolean): PSection;
        function InsertSectionRAW(name: AnsiString): PSection;

        function FindSectionID(name: AnsiString): Integer;
        function FindSection(name: AnsiString): PSection;
        function InsertSection(name: AnsiString): PSection;

        function InsertFileInfo(const section, name: AnsiString; pos, csize, usize, comp, crc: UInt32): PResource;
        function Preload(p: PResource): Boolean;
        function GetSourceStream(p: PResource): TStream;

        procedure ReadLFH(s: TStream; fname: AnsiString; xcsize, xusize, xcomp, xcrc: UInt32);
        procedure ReadCDR(s: TStream; cdrid: Integer);
        function FindEOCD(s: TStream): Boolean;
        procedure ReadEOCD(s: TStream);

        procedure WriteLFH(s: TStream; comp, crc, csize, usize: UInt32; const afname: AnsiString);
        procedure WriteCDR(s: TStream; comp, crc, csize, usize, eattr, offset: UInt32; const afname: AnsiString; cdrid: Integer);
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

  uses SysUtils, StrUtils, Math, utils, zstream, crc, e_log;

  const
    ZIP_SIGN_CDR  = 'PK'#1#2;
    ZIP_SIGN_LFH  = 'PK'#3#4;
    ZIP_SIGN_EOCD = 'PK'#5#6;

  const
    ZIP_COMP_STORE     = 0;
    ZIP_COMP_SHRUNK    = 1;
    ZIP_COMP_REDUCE1   = 2;
    ZIP_COMP_REDUCE2   = 3;
    ZIP_COMP_REDUCE3   = 4;
    ZIP_COMP_REDUCE4   = 5;
    ZIP_COMP_IMPLODE   = 6;
    ZIP_COMP_TOKENIZED = 7;
    ZIP_COMP_DEFLATE   = 8;
    ZIP_COMP_DEFLATE64 = 9;
    ZIP_COMP_TERSE1    = 10;
    ZIP_COMP_BZIP2     = 12;
    ZIP_COMP_LZMA      = 14;
    ZIP_COMP_CMPSC     = 16;
    ZIP_COMP_TERSE2    = 18;
    ZIP_COMP_LZ77      = 19;
    ZIP_COMP_ZSTD1     = 20;
    ZIP_COMP_ZSTD2     = 93;
    ZIP_COMP_MP3       = 94;
    ZIP_COMP_XZ        = 95;
    ZIP_COMP_JPEG      = 96;
    ZIP_COMP_WAVPACK   = 97;
    ZIP_COMP_PPMD      = 98;
    ZIP_COMP_AE        = 99;

  const
    ZIP_SYSTEM     = 0;  // DOS / FAT
    ZIP_MAXVERSION = 63; // Max supported version

  const
    ZIP_ENCRYPTION_MASK = (1 << 0) or (1 << 6) or (1 << 13);
    ZIP_UTF8_MASK = (1 << 11);

  function IsASCII(const s: AnsiString): Boolean;
    var i: Integer;
  begin
    for i := 1 to Length(s) do
    begin
      if s[i] >= #$80 then
      begin
        Result := False;
        exit;
      end;
    end;
    Result := True;
  end;

  procedure ToSectionFile(fname: AnsiString; out section, name: AnsiString); inline;
    var i: SizeInt;
  begin
    i := LastDelimiter('/', fname);
    section := Copy(fname, 1, i - 1);
    name := Copy(fname, i + 1)
  end;

  function GetFileName(const Section, Name: AnsiString): AnsiString; inline;
  begin
    if Section = '' then
      Result := Name
    else
      Result := Section + '/' + Name;
  end;

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
    if i >= 0 then
      Result := @p.list[i]
    else
      Result := nil;
  end;



  function TZIPEditor.FindSectionIDRAW(name: AnsiString; caseSensitive: Boolean): Integer;
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

  function TZIPEditor.FindSectionRAW(name: AnsiString; caseSensitive: Boolean): PSection;
    var i: Integer;
  begin
    i := FindSectionIDRAW(name, caseSensitive);
    if i >= 0 then
      Result := @FSection[i]
    else
      Result := nil;
  end;

  function TZIPEditor.InsertSectionRAW(name: AnsiString): PSection;
    var i: Integer;
  begin
    if FSection = nil then i := 0 else i := Length(FSection);
    SetLength(FSection, i + 1);
    FSection[i] := Default(TSection);
    FSection[i].name := name;
    Result := @FSection[i];
  end;



  function TZIPEditor.FindSectionID(name: AnsiString): Integer;
    var fixName: AnsiString;
  begin
    fixName := StringReplace(name, '\', '/', [rfReplaceAll], TStringReplaceAlgorithm.sraManySmall);
    Result := FindSectionIDRAW(fixName, True); // CaSeNaMe
    if Result < 0 then
      Result := FindSectionIDRAW(fixName, False); // CASENAME
  end;

  function TZIPEditor.FindSection(name: AnsiString): PSection;
    var fixName: AnsiString;
  begin
    fixName := StringReplace(name, '\', '/', [rfReplaceAll], TStringReplaceAlgorithm.sraManySmall);
    Result := FindSectionRAW(fixName, True); // CaSeNaMe
    if Result = nil then
      Result := FindSectionRAW(fixName, False); // CASENAME
  end;

  function TZIPEditor.InsertSection(name: AnsiString): PSection;
  begin
    Result := FindSection(name);
    if Result = nil then
      Result := InsertSectionRAW(name);
  end;



  function TZIPEditor.InsertFileInfo(const section, name: AnsiString; pos, csize, usize, comp, crc: UInt32): PResource;
    var p: PSection; i: Integer;
  begin
    p := FindSectionRAW(section, True);
    if p = nil then
      p := InsertSectionRAW(section);
    if p.list = nil then i := 0 else i := Length(p.list);
    SetLength(p.list, i + 1);
    p.list[i] := Default(TResource);
    p.list[i].name := name;
    p.list[i].pos := pos;
    p.list[i].csize := csize;
    p.list[i].usize := usize;
    p.list[i].comp := comp;
    p.list[i].chksum := crc;
    p.list[i].stream := nil;
    Result := @p.list[i];
  end;



  function TZIPEditor.AddAlias(Res, Alias: String): Boolean;
  begin
    // Hard-links not supported in ZIP
    // However, they never created by editor
    Result := False;
  end;

  function TZIPEditor.AddResource(Data: Pointer; Len: LongWord; Name, Section: String): Boolean;
    const compress: Boolean = True;
    const level: TCompressionLevel = TCompressionLevel.clMax;
    var s: TMemoryStream; cs: TCompressionStream; p: PResource;
    var comp, crc: UInt32;
  begin
    Result := False;
    if Name <> '' then
    begin
      s := TMemoryStream.Create();
      try
        if compress and (Len > 0) then
        begin
          cs := TCompressionStream.Create(level, s, True);
          try
            cs.WriteBuffer(PByte(Data)[0], Len);
            cs.Flush();
            comp := ZIP_COMP_DEFLATE;
          finally
            cs.Free();
          end;
        end;
        if (Len = 0) or (compress = False) or (s.Size >= Len) then
        begin
          s.Seek(0, TSeekOrigin.soBeginning);
          s.SetSize(Len);
          s.WriteBuffer(PByte(Data)[0], Len);
          comp := ZIP_COMP_STORE;
          Assert(s.Size = Len);
        end;
        crc := crc32(0, nil, 0);
        crc := crc32(crc, data, len);
        p := InsertFileInfo(Section, Name, $ffffffff, s.Size, Len, comp, crc);
        p.stream := s;
        Result := True;
      except
        s.Free();
        raise;
      end;
    end;
  end;

  function TZIPEditor.AddResource(FileName, Name, Section: String): Boolean;
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
          e_WriteLog('DFZIP: AddResource: failed to open file ' + FileName, MSG_NOTIFY);
        FLastError := DFWAD_ERROR_CANTOPENWAD;
      end;
    end;
  end;

  constructor TZIPEditor.Create();
  begin
    FSection := nil;
    FStream := nil;
    FLastError := DFWAD_NOERROR;
    FVersion := 10;
    FreeWAD();
  end;

  destructor TZIPEditor.Destroy();
  begin
    FreeWAD();
    inherited;
  end;

  procedure TZIPEditor.FreeWAD();
    var i, j: Integer;
  begin
    if FSection <> nil then
    begin
      for i := 0 to High(FSection) do
      begin
        if FSection[i].list <> nil then
        begin
          for j := 0 to High(FSection[i].list) do
          begin
            if FSection[i].list[j].stream <> nil then
            begin
              FreeAndNil(FSection[i].list[j].stream);
            end;
          end;
          SetLength(FSection[i].list, 0);
        end;
      end;
      SetLength(FSection, 0);
    end;
    if FStream <> nil then
    begin
      FreeAndNil(FStream);
    end;
    FLastError := DFWAD_NOERROR;
    FVersion := 10;
  end;

  function TZIPEditor.Preload(p: PResource): Boolean;
    var s: TMemoryStream;
  begin
    Result := False;
    if p <> nil then
    begin
      Result := p.stream <> nil;
      if (p.stream = nil) and (FStream <> nil) then
      begin
        s := TMemoryStream.Create();
        try
          if p.csize > 0 then
          begin
            FStream.Seek(p.pos, TSeekOrigin.soBeginning);
            s.CopyFrom(FStream, p.csize);
          end;
          Assert(s.Size = p.csize); // wtf, random size if copied zero bytes!
          p.stream := s;
          Result := True;
        except
          s.Free();
        end;
      end;
    end;
  end;

  procedure TZIPEditor.CreateImage();
    var i, j: Integer;
  begin
    if FStream = nil then
    begin
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        e_WriteLog('DFZIP: CreateImage: File not assigned', MSG_NOTIFY);
      FLastError := DFWAD_ERROR_WADNOTLOADED;
    end
    else if FStream is TMemoryStream then
    begin
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        e_WriteLog('DFZIP: CreateImage: Memory stream', MSG_NOTIFY);
      FLastError := DFWAD_NOERROR;
    end
    else
    begin
      if FSection <> nil then
      begin
        for i := 0 to High(FSection) do
        begin
          if FSection[i].list <> nil then
          begin
            for j := 0 to High(FSection[i].list) do
            begin
              if Preload(@FSection[i].list[j]) = False then
              begin
                if gWADEditorLogLevel >= DFWAD_LOG_WARN then
                  e_WriteLog('DFZIP: CreateImage: failed to preload resource [' + FSection[i].name + '][' + FSection[i].list[j].name + ']', MSG_WARNING);
                FLastError := DFWAD_ERROR_CANTOPENWAD;
                exit;
              end;
            end;
          end;
        end;
      end;
      FreeAndNil(FStream);
      FLastError := DFWAD_NOERROR;
    end;
  end;

  procedure TZIPEditor.AddSection(Name: String);
  begin
    if InsertSection(Name) = nil then
      raise Exception.Create('DFZIP: AddSection[' + Name + ']: failed to insert');
  end;

  function TZIPEditor.HaveResource(Section, Resource: String): Boolean;
  begin
    Result := FindResource(FindSection(Section), Resource) <> nil;
  end;

  function TZIPEditor.HaveSection(Section: String): Boolean;
  begin
    Result := FindSection(Section) <> nil;
  end;

  function TZIPEditor.GetSourceStream(p: PResource): TStream;
    var src: TStream;
  begin
    src := nil;
    if p.stream <> nil then
    begin
      src := p.stream;
      src.Seek(0, TSeekOrigin.soBeginning);
    end
    else if FStream <> nil then
    begin
      src := FStream;
      src.Seek(p.pos, TSeekOrigin.soBeginning);
    end;
    Result := src;
  end;

  function TZIPEditor.GetResource(Section, Resource: String; var pData: Pointer; var Len: Integer): Boolean;
    var p: PResource; ptr: PByte; src: TStream; tmp: TDecompressionStream; crc: UInt32;
  begin
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
        case p.comp of
          ZIP_COMP_STORE:
            begin
              Assert(p.csize = p.usize);
              GetMem(ptr, p.usize);
              try
                try
                  src.ReadBuffer(ptr[0], p.usize);
                  Result := True;
                except
                  FreeMem(ptr);
                  raise;
                end;
              except on e: EReadError do
                if gWADEditorLogLevel >= DFWAD_LOG_WARN then
                  e_WriteLog('DFZIP: Failed to read STOREd data, reason: ' + e.Message, MSG_WARNING);
              end;
            end;
          ZIP_COMP_DEFLATE:
            try
              tmp := TDecompressionStream.Create(src, True);
              try
                GetMem(ptr, p.usize);
                try
                  tmp.ReadBuffer(ptr[0], p.usize);
                  Result := True;
                except
                  FreeMem(ptr);
                  raise;
                end;
              finally
                tmp.Free();
              end;
            except
              on e: EStreamError do
              begin
                if gWADEditorLogLevel >= DFWAD_LOG_INFO then
                  e_WriteLog('DFZIP: Failed to decompress DEFLATEd data, reason: ' + e.Message, MSG_WARNING);
                raise e;
              end;
            end;
          otherwise
            if gWADEditorLogLevel >= DFWAD_LOG_INFO then
              e_WriteLog('DFZIP: Unsupported compression method: ' + IntToStr(p.comp), MSG_WARNING);
        end;
      end
      else
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_WARN then
          e_WriteLog('DFZIP: No available source for file data', MSG_WARNING);
        FLastError := DFWAD_ERROR_WADNOTLOADED;
      end;
      if Result = True then
      begin
        crc := crc32(0, nil, 0);
        crc := crc32(crc, ptr, p.usize);
        Result := crc = p.chksum;
        if Result = True then
        begin
          pData := ptr;
          Len := p.usize;
          FLastError := DFWAD_NOERROR;
        end
        else
        begin
          if gWADEditorLogLevel >= DFWAD_LOG_INFO then
            e_WriteLog('DFZIP: File integrity check failed: expected CRC32 $' + IntToHex(p.chksum, 8) + ', calculated CRC32 $' + IntToHex(crc, 8), MSG_WARNING);
          FreeMem(ptr);
        end;
      end;
    end
    else
    begin
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        e_WriteLog('DFZIP: Resource not found', MSG_NOTIFY);
      FLastError := DFWAD_ERROR_RESOURCENOTFOUND;
    end;
  end;

  function TZIPEditor.GetResourcesList(Section: String): SArray;
    var p: PSection; i: Integer;
  begin
    Result := nil;
    p := FindSection(Section);
    if (p <> nil) and (p.list <> nil) then
    begin
      SetLength(Result, Length(p.list));
      for i := 0 to High(p.list) do
      begin
        Result[i] := p.list[i].name;
      end;
    end;
  end;

  function TZIPEditor.GetSectionList(): SArray;
    var i: Integer;
  begin
    Result := nil;
    if FSection <> nil then
    begin
      SetLength(Result, Length(FSection));
      for i := 0 to High(FSection) do
      begin
        Result[i] := FSection[i].name;
      end;
    end;
  end;

  procedure TZIPEditor.ReadLFH(s: TStream; fname: AnsiString; xcsize, xusize, xcomp, xcrc: UInt32);
    var sig: packed array [0..3] of Char;
    var va, vb, flags, comp: UInt16;
    var mtime, crc, csize, usize: UInt32;
    var fnlen, extlen: UInt16;
    var mypos, datapos: UInt64;
    var section, name: AnsiString;
    var p: Pointer;
  begin
    mypos := s.Position;
    if mypos + 30 <= s.Size then
    begin
      s.ReadBuffer(sig[0], 4);
      if sig = ZIP_SIGN_LFH then
      begin
        va := s.ReadByte(); // Min Version
        vb := s.ReadByte(); // Min System
        flags := LEtoN(s.ReadWord());
        comp := LEtoN(s.ReadWord());
        mtime := LEtoN(s.ReadDWord());
        crc := LEtoN(s.ReadDWord());
        csize := LEtoN(s.ReadDWord());
        usize := LEtoN(s.ReadDWord());
        fnlen := LEtoN(s.ReadWord());
        extlen := LEtoN(s.ReadWord());
        datapos := s.Position + fnlen + extlen;
        if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        begin
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Min Version       : ' + IntToStr(va), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Min System        : ' + IntToStr(vb), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Flags             : $' + IntToHex(flags, 4), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Compression       : ' + IntToStr(comp), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Modification Time : $' + IntToHex(mtime, 8), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': CRC-32            : $' + IntToHex(crc, 8), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Compressed size   : ' + IntToStr(csize), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Decompressed size : ' + IntToStr(usize), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Name Length       : ' + IntToStr(fnlen), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Extension Length  : ' + IntToStr(extlen), MSG_NOTIFY);
          e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': <DATA OFFSET>     : $' + IntToHex(datapos, 8), MSG_NOTIFY);
        end;
        if (va >= 10) and (va <= ZIP_MAXVERSION) then
        begin
          if datapos + xcsize <= s.Size then
          begin
            ToSectionFile(fname, section, name);
            if name = '' then
            begin
              p := FindSectionRAW(section, True);
              if p = nil then
                p := InsertSectionRAW(section)
            end
            else
            begin
              p := InsertFileInfo(section, name, datapos, xcsize, xusize, xcomp, xcrc);
            end;
            if p = nil then
              raise Exception.Create('Failed to register resource [' + fname + ']');
          end
          else
            raise Exception.Create('Invalid LFH size (corrupted file?)');
        end
        else
        begin
          FLastError := DFWAD_ERROR_WRONGVERSION;
          raise Exception.Create('Unsupported CDR version ' + IntToStr(va) + ', not in range [10..' + IntToStr(ZIP_MAXVERSION) + ']');
        end;
      end
      else
        raise Exception.Create('Invalid LFH signature $' +IntToHex(Ord(sig[0]), 2) + ' $' +IntToHex(Ord(sig[1]), 2) + ' $' +IntToHex(Ord(sig[2]), 2) + ' $' +IntToHex(Ord(sig[3]), 2) + ' (corrupted file?)');
    end
    else
      raise Exception.Create('Invalid LFH size (corrupted file?)');
  end;

  procedure TZIPEditor.ReadCDR(s: TStream; cdrid: Integer);
    var sig: packed array [0..3] of Char;
    var vva, vvb, va, vb, flags, comp: UInt16;
    var mtime, crc, csize, usize: UInt32;
    var fnlen, extlen, comlen, disk, iattr: UInt16;
    var eattr, offset: UInt32;
    var mypos, next: UInt64;
    var name: PChar;
    var aname: AnsiString;
  begin
    mypos := s.Position;
    s.ReadBuffer(sig[0], 4);
    if sig = ZIP_SIGN_CDR then
    begin
      // Valid Central Directory Signature
      vva := s.ReadByte(); // Writer Version
      vvb := s.ReadByte(); // Writer System
      va := s.ReadByte();  // Min Version
      vb := s.ReadByte();  // Min System
      flags := LEtoN(s.ReadWord());
      comp := LEtoN(s.ReadWord());
      mtime := LEtoN(s.ReadDWord());
      crc := LEtoN(s.ReadDWord());
      csize := LEtoN(s.ReadDWord());
      usize := LEtoN(s.ReadDWord());
      fnlen := LEtoN(s.ReadWord());
      extlen := LEtoN(s.ReadWord());
      comlen := LEtoN(s.ReadWord());
      disk := LEtoN(s.ReadWord());
      iattr := LEtoN(s.ReadWord());
      eattr := LEtoN(s.ReadDWord());
      offset := LEtoN(s.ReadDWord());
      next := s.Position + fnlen + extlen + comlen;
      FVersion := va;
      if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
      begin
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Writer Version    : ' + IntToStr(vva), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Writer System     : ' + IntToStr(vvb), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Min Version       : ' + IntToStr(va), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Min System        : ' + IntToStr(vb), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Flags             : $' + IntToHex(flags, 4), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Compression       : ' + IntToStr(comp), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Modification Time : $' + IntToHex(mtime, 8), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': CRC-32            : $' + IntToHex(crc, 8), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Compressed size   : ' + IntToStr(csize), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Decompressed size : ' + IntToStr(usize), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Name Length       : ' + IntToStr(fnlen), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Extension Length  : ' + IntToStr(extlen), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Comment Length    : ' + IntToStr(comlen), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Disk              : ' + IntToStr(disk), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Internal Attrib   : $' + IntToHex(iattr, 4), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': External Attrib   : $' + IntToHex(eattr, 8), MSG_NOTIFY);
        e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': LFH Offset        : $' + IntToHex(offset, 8), MSG_NOTIFY);
      end;
      if (vva = $10) and (vvb = $0A) and (va = $10) and (vb = $00) and (flags = (1 << 10)) and (mtime = 0) and (iattr = 0) and (eattr = 0) then
      begin
        // HACK: Editor and wadcvt for long time sets incorrent flag for UTF-8
        if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
          e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': WADCVT BUG        : YES', MSG_NOTIFY);
        flags := ZIP_UTF8_MASK;
      end;
      if (va >= 10) and (va <= ZIP_MAXVERSION) then
      begin
        if (flags and ZIP_ENCRYPTION_MASK) = 0 then
        begin
          if (csize <> $ffffffff) and (usize <> $ffffffff) and (disk <> $ffff) and (offset <> $ffffffff) then
          begin
            if disk = 0 then
            begin
              if (next <= s.Size) and (fnlen > 0) then
              begin
                case comp of
                  ZIP_COMP_STORE:
                    if csize <> usize then
                      raise Exception.Create('Compressed size ' + IntToStr(csize) + ' != Descompressed size ' + IntToStr(usize) + 'for STORE method (corrupted file?)');
                  ZIP_COMP_SHRUNK,
                  ZIP_COMP_REDUCE1,
                  ZIP_COMP_REDUCE2,
                  ZIP_COMP_REDUCE3,
                  ZIP_COMP_REDUCE4,
                  ZIP_COMP_IMPLODE,
                  ZIP_COMP_DEFLATE,
                  ZIP_COMP_DEFLATE64,
                  ZIP_COMP_TERSE1,
                  ZIP_COMP_BZIP2,
                  ZIP_COMP_LZMA,
                  ZIP_COMP_CMPSC,
                  ZIP_COMP_TERSE2,
                  ZIP_COMP_LZ77,
                  ZIP_COMP_ZSTD1,
                  ZIP_COMP_ZSTD2,
                  ZIP_COMP_MP3,
                  ZIP_COMP_XZ,
                  ZIP_COMP_JPEG,
                  ZIP_COMP_WAVPACK,
                  ZIP_COMP_PPMD:
                    ; // ok
                  ZIP_COMP_AE:
                    raise Exception.Create('Encrypted archives not supported');
                  otherwise
                    raise Exception.Create('Unknown compression method ' + IntToStr(comp));
                end;
                GetMem(name, UInt32(fnlen) + 1);
                try
                  s.ReadBuffer(name[0], fnlen);
                  name[fnlen] := #0;
                  aname := name;
                  if (flags and ZIP_UTF8_MASK = 0) and (IsASCII(name) = False) then
                  begin
                    // TODO: Detect UTF-8
                    //       Older or invalid packers does not set bit 11
                    //       Some newer packers may write UTF-8 names into extended data
                    if gWADEditorLogLevel >= DFWAD_LOG_WARN then
                      e_WriteLog('ZIP: Non UTF-8 encoded name, threat it as CP1251', MSG_WARNING);
                    aname := win2utf(aname);
                  end;
                  if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
                    e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Name              : "' + aname + '"', MSG_NOTIFY);
                  s.Seek(offset, TSeekOrigin.soBeginning);
                  ReadLFH(s, aname, csize, usize, comp, crc);
                finally
                  s.Seek(next, TSeekOrigin.soBeginning);
                  FreeMem(name);
                end;
              end
              else
                raise Exception.Create('Empty files names not supported');
            end
            else
              raise Exception.Create('Splitted archives not supported');
          end
          else
          begin
            FLastError := DFWAD_ERROR_WRONGVERSION;
            raise Exception.Create('ZIP64 not supported');
          end;
        end
        else
        begin
          FLastError := DFWAD_ERROR_READWAD;
          raise Exception.Create('Encrypted archives not supported');
        end;
      end
      else
      begin
        FLastError := DFWAD_ERROR_WRONGVERSION;
        raise Exception.Create('Unsupported CDR version ' + IntToStr(va) + ', not in range [10..' + IntToStr(ZIP_MAXVERSION) + ']');
      end;
    end
    else
      raise Exception.Create('Invalid CDR signature $' + IntToHex(Ord(sig[0]), 2) + ' $' +IntToHex(Ord(sig[1]), 2) + ' $' +IntToHex(Ord(sig[2]), 2) + ' $' +IntToHex(Ord(sig[3]), 2) + ' (corrupted file?)');
  end;

  function TZIPEditor.FindEOCD(s: TStream): Boolean;
    const maxedir = 20; // end of central directory entry
    const maxecdir = maxedir + 65536; // + comment
    var sig: packed array [0..3] of Char; off, lim: Int64;
  begin
    Result := False;
    if s.Size >= maxedir then
    begin
      if s.Size < maxecdir then lim := s.Size else lim := maxecdir;
      lim := lim - maxedir;
      off := maxedir;
      while (off <= lim) and (Result = False) do
      begin
        s.Seek(s.Size - off, TSeekOrigin.soBeginning);
        s.ReadBuffer(sig[0], 4);
        Result := sig = ZIP_SIGN_EOCD;
        Inc(off);
      end;
    end;
  end;

  procedure TZIPEditor.ReadEOCD(s: TStream);
    var sig: packed array [0..3] of Char;
    var idisk, ndisk, nrec, total, comlen: UInt16;
    var csize, cpos, i: UInt32;
    var mypos: UInt64;
  begin
    FLastError := DFWAD_ERROR_FILENOTWAD;
    FVersion := 0;
    s.ReadBuffer(sig[0], 4);
    if (sig = ZIP_SIGN_LFH) or (sig = ZIP_SIGN_EOCD) then
    begin
      if FindEOCD(s) then
      begin
        // End of Central Directory found
        FLastError := DFWAD_ERROR_READWAD;
        mypos := s.Position - 4;
        idisk := LEtoN(s.ReadWord());
        ndisk := LEtoN(s.ReadWord());
        nrec := LEtoN(s.ReadWord());
        total := LEtoN(s.ReadWord());
        csize := LEtoN(s.ReadDWord());
        cpos := LEtoN(s.ReadDWord());
        comlen := LEtoN(s.ReadWord());
        if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
        begin
          e_WriteLog('==============================================', MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Disk ID           : ' + IntToStr(idisk), MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Disk ID with CD   : ' + IntToStr(ndisk), MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Available CDR''s   : ' + IntToStr(nrec), MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Total CDR''s       : ' + IntToStr(total), MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': CD Length         : ' + IntToStr(csize), MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': CD Offset         : $' + IntToHex(cpos, 8), MSG_NOTIFY);
          e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Comment Length    : ' + IntToStr(comlen), MSG_NOTIFY);
        end;
        if (idisk <> $ffff) and (ndisk <> $ffff) and (nrec <> $ffff) and (total <> $ffff) and (csize <> $ffffffff) and (cpos <> $ffffffff) then
        begin
          if s.Position + comlen = s.Size then
          begin
            if (idisk = 0) and (ndisk = 0) and (nrec = total) then
            begin
              if (nrec * 46 <= csize) and (UInt64(cpos) + csize <= s.Size) then
              begin
                if total > 0 then
                begin
                  i := 0;
                  s.Seek(cpos, TSeekOrigin.soBeginning);
                  while i < nrec do
                  begin
                    if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
                      e_WriteLog('==============================================', MSG_NOTIFY);
                    ReadCDR(s, i);
                    Inc(i);
                  end;
                  if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
                    e_WriteLog('==============================================', MSG_NOTIFY);
                end;
              end
              else
                raise Exception.Create('Central Directory too big (corrupted file?)');
            end
            else
              raise Exception.Create('Splitted archives not supported');
          end
          else
            raise Exception.Create('EOCD too big (corrupted file?)');
        end
        else
          raise Exception.Create('ZIP64 not supported');
      end
      else
        raise Exception.Create('EOCD not found (corrupted file?)');
    end
    else
      raise Exception.Create('Not DFZIP formated file');
  end;

  function TZIPEditor.ReadFile2(FileName: String): Boolean;
    var s: TFileStream;
  begin
    FreeWAD();
    Result := False;
    try
      try
        s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        try
          ReadEOCD(s);
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
            e_WriteLog('ZIP: Failed to read ZIP from file ' + FileName + ', reason: ' + e.Message, MSG_WARNING);
          FreeWAD();
        end;
      end;
    except
      on e: EFOpenError do
      begin
        if gWADEditorLogLevel >= DFWAD_LOG_INFO then
          e_WriteLog('DFZIP: Failed to open file ' + FileName + ', reason: ' + e.Message, MSG_WARNING);
        if FileExists(FileName) then
          FLastError := DFWAD_ERROR_CANTOPENWAD
        else
          FLastError := DFWAD_ERROR_WADNOTFOUND;
      end;
    end;
  end;

  function TZIPEditor.ReadMemory(Data: Pointer; Len: LongWord): Boolean;
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
        ReadEOCD(s);
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
          e_WriteLog('DFZIP: Failed to read ZIP from memory, reason: ' + e.Message, MSG_WARNING);
        FreeWAD();
      end;
    end;
  end;

  procedure TZIPEditor.RemoveResource(Section, Resource: String);
    var p: PSection; i: Integer;
  begin
    p := FindSection(Section);
    i := FindResourceID(p, Resource);
    if i >= 0 then
    begin
      if p.list[i].stream <> nil then
        FreeAndNil(p.list[i].stream);
      for i := i + 1 to High(p.list) do
      begin
        p.list[i - 1] := p.list[i];
      end;
      SetLength(p.list, High(p.list));
    end;
  end;

  function GetZIPVersion(const afname: AnsiString; flags, comp: UInt16): UInt8;
    var version: UInt8;
  begin
    version := 10; // Base version
    case comp of
      ZIP_COMP_STORE:     version := 10;
      ZIP_COMP_SHRUNK:    version := 10;
      ZIP_COMP_REDUCE1:   version := 10;
      ZIP_COMP_REDUCE2:   version := 10;
      ZIP_COMP_REDUCE3:   version := 10;
      ZIP_COMP_REDUCE4:   version := 10;
      ZIP_COMP_IMPLODE:   version := 10;
      ZIP_COMP_TOKENIZED: version := 20;
      ZIP_COMP_DEFLATE:   version := 20;
      ZIP_COMP_DEFLATE64: version := 21;
      ZIP_COMP_TERSE1:    version := 25; // PKWARE DCL Implode
      ZIP_COMP_BZIP2:     version := 46;
      ZIP_COMP_LZMA:      version := 63;
      ZIP_COMP_CMPSC:     version := 63;
      ZIP_COMP_TERSE2:    version := 63;
      ZIP_COMP_LZ77:      version := 63;
      ZIP_COMP_ZSTD1:     version := 63;
      ZIP_COMP_ZSTD2:     version := 63;
      ZIP_COMP_MP3:       version := 63;
      ZIP_COMP_XZ:        version := 63;
      ZIP_COMP_JPEG:      version := 63;
      ZIP_COMP_WAVPACK:   version := 63;
      ZIP_COMP_PPMD:      version := 63;
      ZIP_COMP_AE:        version := 63;
    end;
    if afname[Length(afname)] = '/' then
      version := Max(20, version); // Folder
    if flags and ZIP_UTF8_MASK <> 0 then
      version := Max(63, version); // UTF-8 name
    Result := version;
  end;

  procedure TZIPEditor.WriteLFH(s: TStream; comp, crc, csize, usize: UInt32; const afname: AnsiString);
    var fname: PChar; version: UInt8; fnlen, flags: UInt16; mypos: UInt64;
  begin
    mypos := s.Position;
    fname := PChar(afname);
    fnlen := Length(fname);
    flags := 0;
    if IsASCII(afname) = False then
      flags := flags or ZIP_UTF8_MASK;
    version := GetZIPVersion(afname, flags, comp);
    if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
    begin
      e_WriteLog('==============================================', MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Min Version       : ' + IntToStr(version), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Min System        : ' + IntToStr(ZIP_SYSTEM), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Flags             : $' + IntToHex(flags, 4), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Compression       : ' + IntToStr(comp), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Modification Time : $' + IntToHex(0, 8), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': CRC-32            : $' + IntToHex(crc, 8), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Compressed size   : ' + IntToStr(csize), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Decompressed size : ' + IntToStr(usize), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Name Length       : ' + IntToStr(fnlen), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Extension Length  : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('LFH   @' + IntToHex(mypos, 8) + ': Name              : "' + fname + '"', MSG_NOTIFY);
    end;
    s.WriteBuffer(ZIP_SIGN_LFH, 4); // LFH Signature
    s.WriteByte(version);           // Min version
    s.WriteByte(ZIP_SYSTEM);        // System
    WriteInt(s, UInt16(flags));     // Flags
    WriteInt(s, UInt16(comp));      // Compression method
    WriteInt(s, UInt32(0));         // Modification time/date
    WriteInt(s, UInt32(crc));       // CRC-32
    WriteInt(s, UInt32(csize));     // Compressed size
    WriteInt(s, UInt32(usize));     // Decompressed size
    WriteInt(s, UInt16(fnlen));     // Name field length
    WriteInt(s, UInt16(0));         // Extra field length
    s.WriteBuffer(fname[0], fnlen); // File Name
  end;

  procedure TZIPEditor.WriteCDR(s: TStream; comp, crc, csize, usize, eattr, offset: UInt32; const afname: AnsiString; cdrid: Integer);
    var fname: PChar; version: UInt8; fnlen, flags: UInt16; mypos: UInt64;
  begin
    mypos := s.Position;
    fname := PChar(afname);
    fnlen := Length(fname);
    flags := 0;
    if IsASCII(afname) = False then
      flags := flags or ZIP_UTF8_MASK;
    version := GetZIPVersion(afname, flags, comp);
    if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
    begin
      e_WriteLog('==============================================', MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Writer Version    : ' + IntToStr(ZIP_MAXVERSION), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Writer System     : ' + IntToStr(ZIP_SYSTEM), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Min Version       : ' + IntToStr(version), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Min System        : ' + IntToStr(ZIP_SYSTEM), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Flags             : $' + IntToHex(flags, 4), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Compression       : ' + IntToStr(comp), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Modification Time : $' + IntToHex(0, 8), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': CRC-32            : $' + IntToHex(crc, 8), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Compressed size   : ' + IntToStr(csize), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Decompressed size : ' + IntToStr(usize), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Name Length       : ' + IntToStr(fnlen), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Extension Length  : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Comment Length    : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Disk              : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Internal Attrib   : $' + IntToHex(0, 4), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': External Attrib   : $' + IntToHex(eattr, 8), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': LFH Offset        : $' + IntToHex(offset, 8), MSG_NOTIFY);
      e_WriteLog('CDR#' + IntToStr(cdrid) + ' @' + IntToHex(mypos, 8) + ': Name              : "' + fname + '"', MSG_NOTIFY);
    end;
    s.WriteBuffer(ZIP_SIGN_CDR, 4); // CDR Signature
    s.WriteByte(ZIP_MAXVERSION);    // Used version
    s.WriteByte(ZIP_SYSTEM);        // Used system
    s.WriteByte(version);           // Min version
    s.WriteByte(ZIP_SYSTEM);        // Min system
    WriteInt(s, UInt16(flags));     // Flags
    WriteInt(s, UInt16(comp));      // Compression method
    WriteInt(s, UInt32(0));         // Modification time/date
    WriteInt(s, UInt32(crc));       // CRC-32
    WriteInt(s, UInt32(csize));     // Compressed size
    WriteInt(s, UInt32(usize));     // Decompressed size
    WriteInt(s, UInt16(fnlen));     // Name field length
    WriteInt(s, UInt16(0));         // Extra field length
    WriteInt(s, UInt16(0));         // Comment field length
    WriteInt(s, UInt16(0));         // Disk
    WriteInt(s, UInt16(0));         // Internal attributes
    WriteInt(s, UInt32(eattr));     // External attributes
    WriteInt(s, UInt32(offset));    // LFH offset
    s.WriteBuffer(fname[0], fnlen); // File Name
  end;

  procedure TZIPEditor.SaveToStream(s: TStream);
    var i, j: Integer;
    var start, offset, loffset, size, zcrc, count: UInt32;
    var p: PResource;
    var afname: AnsiString;
    var mypos: UInt64;
  begin
    // Write LFH headers and data
    start := s.Position;
    zcrc := crc32(0, nil, 0);
    if FSection <> nil then
    begin
      for i := 0 to High(FSection) do
      begin
        if FSection[i].list <> nil then
        begin
          for j := 0 to High(FSection[i].list) do
          begin
            p := @FSection[i].list[j];
            afname := GetFileName(FSection[i].name, p.name);
            WriteLFH(s, p.comp, p.chksum, p.csize, p.usize, afname);
            if p.stream <> nil then
            begin
              Assert(p.stream.Size = p.csize);
              p.stream.SaveToStream(s);
            end
            else if FStream <> nil then
            begin
              FStream.Seek(p.pos, TSeekOrigin.soBeginning);
              s.CopyFrom(FStream, p.csize);
            end
            else
            begin
              raise Exception.Create('No data source available (somethig very wrong)');
            end;
          end;
        end
        else
        begin
          afname := GetFileName(FSection[i].name, '');
          WriteLFH(s, ZIP_COMP_STORE, zcrc, 0, 0, afname);
        end;
      end;
    end;
    // Write CDR headers
    count := 0;
    loffset := 0;
    offset := s.Position - start;
    if FSection <> nil then
    begin
      for i := 0 to High(FSection) do
      begin
        if FSection[i].list <> nil then
        begin
          for j := 0 to High(FSection[i].list) do
          begin
            p := @FSection[i].list[j];
            afname := GetFileName(FSection[i].name, p.name);
            WriteCDR(s, p.comp, p.chksum, p.csize, p.usize, $00, loffset, afname, i);
            loffset := loffset + 30 + Length(afname) + p.csize;
            Inc(count);
          end;
        end
        else
        begin
          afname := GetFileName(FSection[i].name, '');
          WriteCDR(s, ZIP_COMP_STORE, zcrc, 0, 0, $10, loffset, afname, i);
          loffset := loffset + 30 + Length(afname) + 0;
          Inc(count);
        end;
      end;
    end;
    Assert(loffset = offset);
    Assert(count < $ffff);
    size := s.Position - start - offset;
    // Write EOCD header
    mypos := s.Position;
    if gWADEditorLogLevel >= DFWAD_LOG_DEBUG then
    begin
      e_WriteLog('==============================================', MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Disk ID           : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Disk ID with CD   : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Available CDR''s   : ' + IntToStr(count), MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Total CDR''s       : ' + IntToStr(count), MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': CD Length         : ' + IntToStr(size), MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': CD Offset         : $' + IntToHex(offset, 8), MSG_NOTIFY);
      e_WriteLog('EOCD  @' + IntToHex(mypos, 8) + ': Comment Length    : ' + IntToStr(0), MSG_NOTIFY);
      e_WriteLog('==============================================', MSG_NOTIFY);
    end;
    s.WriteBuffer(ZIP_SIGN_EOCD, 4); // EOCD Signature
    WriteInt(s, UInt16(0));          // Disk
    WriteInt(s, UInt16(0));          // Num of Disks
    WriteInt(s, UInt16(count));      // Num of CDRs
    WriteInt(s, UInt16(count));      // Total CDR entries
    WriteInt(s, UInt32(size));       // Central Directory size
    WriteInt(s, UInt32(offset));     // Central Directory offset
    WriteInt(s, UInt16(0));          // Comment field length
  end;

  procedure TZIPEditor.SaveTo(FileName: String);
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
          e_WriteLog('ZIP: Failed to create file ' + FileName + ', reason: ' + e.Message, MSG_WARNING);
        raise e;
      end;
    end;
  end;

  function TZIPEditor.GetLastError: Integer;
  begin
    Result := FLastError;
  end;

  function TZIPEditor.GetLastErrorStr: String;
  begin
    case FLastError of
      DFWAD_NOERROR: Result := '';
      DFWAD_ERROR_WADNOTFOUND: Result := 'DFZIP file not found';
      DFWAD_ERROR_CANTOPENWAD: Result := 'Can''t open DFZIP file';
      DFWAD_ERROR_RESOURCENOTFOUND: Result := 'Resource not found';
      DFWAD_ERROR_FILENOTWAD: Result := 'File is not DFZIP';
      DFWAD_ERROR_WADNOTLOADED: Result := 'DFZIP file is not loaded';
      DFWAD_ERROR_READRESOURCE: Result := 'Read resource error';
      DFWAD_ERROR_READWAD: Result := 'Read DFZIP error';
      otherwise Result := IntToStr(FLastError);
    end;
  end;

  function TZIPEditor.GetResourcesCount: Word;
    var i: Integer;
  begin
    Result := 0;
    if FSection <> nil then
    begin
      Result := Result + Length(FSection);
      for i := 0 to High(FSection) do
        if FSection[i].list <> nil then
          Result := Result + Length(FSection[i].list);
    end;
  end;

  function TZIPEditor.GetVersion: Byte;
  begin
    Result := FVersion;
  end;

begin
  gWADEditorFactory.RegisterEditor('DFZIP', TZIPEditor);
end.
