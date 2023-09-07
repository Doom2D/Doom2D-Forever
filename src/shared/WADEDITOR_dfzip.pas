{$INCLUDE ../shared/a_modes.inc}

unit WADEDITOR_dfzip;

// Implementation restrictions:
// - File must start with LFH or EOCD signature
// - EOCD must be located strictly at the end of file
// - Multi-disk ZIP files are not supported
// - UTF-8 not supported yet, expected WIN1251 encoding
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

        function ReadLFH(s: TStream; fname: AnsiString; xcsize, xusize, xcomp, xcrc: UInt32): Boolean;
        function ReadCDR(s: TStream): Boolean;
        function FindEOCD(s: TStream): Boolean;
        function ReadEOCD(s: TStream): Boolean;

        procedure WriteLFH(s: TStream; comp, crc, csize, usize: UInt32; const afname: AnsiString);
        procedure WriteCDR(s: TStream; comp, crc, csize, usize, attr, offset: UInt32; const afname: AnsiString);
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

  uses SysUtils, StrUtils, zstream, crc, e_log;

  const
    ZIP_SIGN_CDR  = 'PK'#1#2;
    ZIP_SIGN_LFH  = 'PK'#3#4;
    ZIP_SIGN_EOCD = 'PK'#5#6;

  const
    ZIP_COMP_STORE   = 0;
    ZIP_COMP_DEFLATE = 8;

  const
    ZIP_SYSTEM     = 0;  // DOS / FAT
    ZIP_VERSION    = 20; // Min version
    ZIP_MAXVERSION = 63; // Max supported version

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
      s := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
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
    except on e: EFOpenError do
      FLastError := DFWAD_ERROR_CANTOPENWAD;
    end;
  end;

  constructor TZIPEditor.Create();
  begin
    FSection := nil;
    FStream := nil;
    FLastError := DFWAD_NOERROR;
    FVersion := ZIP_VERSION;
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
    FVersion := ZIP_VERSION;
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
          raise;
        end;
      end;
    end;
  end;

  procedure TZIPEditor.CreateImage();
    var i, j: Integer;
  begin
    if FStream = nil then
    begin
      FLastError := DFWAD_ERROR_WADNOTLOADED;
    end
    else if FStream is TMemoryStream then
    begin
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
      raise Exception.Create('ZIP: AddSection: failed to add section');
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
            if p.csize = p.usize then
            begin
              GetMem(ptr, p.usize);
              try
                src.ReadBuffer(ptr[0], p.usize);
                Result := True;
              except
                FreeMem(ptr);
              end;
            end;
          ZIP_COMP_DEFLATE:
            begin
              tmp := TDecompressionStream.Create(src, True);
              try
                GetMem(ptr, p.usize);
                try
                  tmp.ReadBuffer(ptr[0], p.usize);
                  Result := True;
                except
                  FreeMem(ptr);
                end;
              finally
                tmp.Free();
              end;
            end;
        end;
      end
      else
      begin
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
          FreeMem(ptr);
        end;
      end;
    end
    else
    begin
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

  function TZIPEditor.ReadLFH(s: TStream; fname: AnsiString; xcsize, xusize, xcomp, xcrc: UInt32): Boolean;
    var sig: packed array [0..3] of Char;
    var v, flags, comp: UInt16;
    var mtime, crc, csize, usize: UInt32;
    var fnlen, extlen: UInt16;
    var datapos: UInt64;
    var section, name: AnsiString;
  begin
    Result := False;
    if s.Position + 30 <= s.Size then
    begin
      s.ReadBuffer(sig[0], 4);
      if sig = ZIP_SIGN_LFH then
      begin
        v := LEtoN(s.ReadWord());
        flags := LEtoN(s.ReadWord());
        comp := LEtoN(s.ReadWord());
        mtime := LEtoN(s.ReadDWord());
        crc := LEtoN(s.ReadDWord());
        csize := LEtoN(s.ReadDWord());
        usize := LEtoN(s.ReadDWord());
        fnlen := LEtoN(s.ReadWord());
        extlen := LEtoN(s.ReadWord());
        datapos := s.Position + fnlen + extlen;
        if datapos + xcsize <= s.Size then
        begin
          // Valid Record Size
          ToSectionFile(fname, section, name);
          if name = '' then
            Result := InsertSection(section) <> nil
          else
            Result := InsertFileInfo(section, name, datapos, xcsize, xusize, xcomp, xcrc) <> nil;
        end;
      end;
    end;
  end;

  function TZIPEditor.ReadCDR(s: TStream): Boolean;
    var sig: packed array [0..3] of Char;
    var v, va, vb, flags, comp: UInt16;
    var mtime, crc, csize, usize: UInt32;
    var fnlen, extlen, comlen, disk, iattr: UInt16;
    var eattr, offset: UInt32;
    var next: UInt64;
    var name: PChar;
  begin
    Result := False;
    s.ReadBuffer(sig[0], 4);
    if sig = ZIP_SIGN_CDR then
    begin
      // Valid Central Directory Signature
      v := LEtoN(s.ReadWord());
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
      comlen := LEtoN(s.ReadWord());
      disk := LEtoN(s.ReadWord());
      iattr := LEtoN(s.ReadWord());
      eattr := LEtoN(s.ReadDWord());
      offset := LEtoN(s.ReadDWord());
      next := s.Position + fnlen + extlen + comlen;
      FVersion := va;
      if va <= ZIP_MAXVERSION then
      begin
        if (flags and ((1 << 0) or (1 << 6) or (1 << 13))) = 0 then
        begin
          // TODO: check bit 11 (UTF8 name and comment)
          if (csize <> $ffffffff) and (usize <> $ffffffff) and (disk <> $ffff) and (offset <> $ffffffff) then
          begin
            // Old Style ZIP
            if disk = 0 then
            begin
              // Single Volume ZIP
              if (next <= s.Size) and (fnlen > 0) then
              begin
                // Valid Central Directory Entry
                GetMem(name, UInt32(fnlen) + 1);
                try
                  s.ReadBuffer(name[0], fnlen);
                  name[fnlen] := #0;
                  s.Seek(offset, TSeekOrigin.soBeginning);
                  Result := ReadLFH(s, name, csize, usize, comp, crc);
                finally
                  s.Seek(next, TSeekOrigin.soBeginning);
                  FreeMem(name);
                end;
              end;
            end;
          end
          else
          begin
            // ZIP64
            FLastError := DFWAD_ERROR_WRONGVERSION;
          end;
        end
        else
        begin
          // Encrypted file
          FLastError := DFWAD_ERROR_READWAD;
        end;
      end
      else
      begin
        // Unsupported version
        FLastError := DFWAD_ERROR_WRONGVERSION;
      end;
    end;
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

  function TZIPEditor.ReadEOCD(s: TStream): Boolean;
    var sig: packed array [0..3] of Char;
    var idisk, ndisk, nrec, total, comlen: UInt16;
    var csize, cpos, i: UInt32;
  begin
    Result := False;
    FLastError := DFWAD_ERROR_FILENOTWAD;
    FVersion := 0;
    s.ReadBuffer(sig[0], 4);
    if (sig = ZIP_SIGN_LFH) or (sig = ZIP_SIGN_EOCD) then
    begin
      if FindEOCD(s) then
      begin
        // End of Central Directory found
        FLastError := DFWAD_ERROR_READWAD;
        idisk := LEtoN(s.ReadWord());
        ndisk := LEtoN(s.ReadWord());
        nrec := LEtoN(s.ReadWord());
        total := LEtoN(s.ReadWord());
        csize := LEtoN(s.ReadDWord());
        cpos := LEtoN(s.ReadDWord());
        comlen := LEtoN(s.ReadWord());
        if (idisk <> $ffff) and (ndisk <> $ffff) and (nrec <> $ffff) and (total <> $ffff) and (csize <> $ffffffff) and (cpos <> $ffffffff) then
        begin
          // Old Style ZIP
          if s.Position + comlen = s.Size then
          begin
            // Valid End of Central Directory size (located exactly at the end of file)
            if (idisk = 0) and (ndisk = 0) and (nrec = total) then
            begin
              // Single volume ZIP
              if (UInt64(cpos) + csize <= s.Size) then
              begin
                // Valid Cental Directry Record position and size
                Result := True;
                if total > 0 then
                begin
                  // At least one Central Directry present
                  i := 0;
                  s.Seek(cpos, TSeekOrigin.soBeginning);
                  while (i < nrec) and (Result = True) do
                  begin
                    Result := ReadCDR(s);
                    Inc(i);
                  end;
                  // if Result = False then
                  //   writeln('Invalid Central Directory #', i - 1);
                end;
              end;
            end;
          end;
        end
        else
        begin
          // ZIP64
          FLastError := DFWAD_ERROR_WRONGVERSION;
        end;
      end;
    end;
  end;

  function TZIPEditor.ReadFile2(FileName: String): Boolean;
    var s: TFileStream;
  begin
    FreeWAD();
    Result := False;
    try
      s := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
      try
        Result := ReadEOCD(s);
        if Result = True then
        begin
          FStream := s;
          FLastError := DFWAD_NOERROR;
        end
        else
        begin
          FStream := nil;
          s.Free();
        end;
      except
        s.Free();
      end;
    except on e: EFOpenError do
      if FileExists(FileName) then
        FLastError := DFWAD_ERROR_CANTOPENWAD
      else
        FLastError := DFWAD_ERROR_WADNOTFOUND;
    end;
  end;

  function TZIPEditor.ReadMemory(Data: Pointer; Len: LongWord): Boolean;
    var s: TMemoryStream;
  begin
    FreeWAD();
    Result := False;
    s := TMemoryStream.Create;
    try
      s.SetSize(Len);
      s.WriteBuffer(PByte(Data)[0], Len);
      s.Seek(0, soBeginning);
      Result := ReadEOCD(s);
      if Result = True then
      begin
        FStream := s;
        FLastError := DFWAD_NOERROR;
      end
      else
      begin
        FStream := nil;
        s.Free();
      end;
    except
      s.Free();
      raise;
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

  procedure TZIPEditor.WriteLFH(s: TStream; comp, crc, csize, usize: UInt32; const afname: AnsiString);
    var fname: PChar; flen: UInt16;
  begin
    fname := PChar(afname);
    flen := Length(fname);
    s.WriteBuffer(ZIP_SIGN_LFH, 4); // LFH Signature
    s.WriteByte(ZIP_VERSION);       // Min version
    s.WriteByte(ZIP_SYSTEM);        // System
    s.WriteWord(NtoLE(0));          // Flags
    s.WriteWord(NtoLE(comp));       // Compression method
    s.WriteDWord(NtoLE(0));         // Modification time/date
    s.WriteDWord(NtoLE(crc));       // CRC-32
    s.WriteDWord(NtoLE(csize));     // Compressed size
    s.WriteDWord(NtoLE(usize));     // Decompressed size
    s.WriteWord(NtoLE(flen));       // Name field length
    s.WriteWord(NtoLE(0));          // Extra field length
    s.WriteBuffer(fname[0], flen);  // File Name
  end;

  procedure TZIPEditor.WriteCDR(s: TStream; comp, crc, csize, usize, attr, offset: UInt32; const afname: AnsiString);
    var fname: PChar; flen: UInt16;
  begin
    fname := PChar(afname);
    flen := Length(fname);
    s.WriteBuffer(ZIP_SIGN_CDR, 4); // CDR Signature
    s.WriteByte(ZIP_MAXVERSION);    // Used version
    s.WriteByte(ZIP_SYSTEM);        // Used system
    s.WriteByte(ZIP_VERSION);       // Min version
    s.WriteByte(ZIP_SYSTEM);        // Min system
    s.WriteWord(NtoLE(0));          // Flags
    s.WriteWord(NtoLE(comp));       // Compression method
    s.WriteDWord(NtoLE(0));         // Modification time/date
    s.WriteDWord(NtoLE(crc));       // CRC-32
    s.WriteDWord(NtoLE(csize));     // Compressed size
    s.WriteDWord(NtoLE(usize));     // Decompressed size
    s.WriteWord(NtoLE(flen));       // Name field length
    s.WriteWord(NtoLE(0));          // Extra field length
    s.WriteWord(NtoLE(0));          // Comment field length
    s.WriteWord(NtoLE(0));          // Disk
    s.WriteWord(NtoLE(0));          // Internal attributes
    s.WriteDWord(NtoLE(attr));      // External attributes
    s.WriteDWord(NtoLE(offset));    // LFH offset
    s.WriteBuffer(fname[0], flen);  // File Name
  end;

  procedure TZIPEditor.SaveToStream(s: TStream);
    var i, j: Integer;
    var start, offset, loffset, size, zcrc, count: UInt32;
    var p: PResource;
    var afname: AnsiString;
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
              raise Exception.Create('ZIP: SaveToStream: No data source available');
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
    loffset := start;
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
            WriteCDR(s, p.comp, p.chksum, p.csize, p.usize, 0, loffset - start, afname);
            loffset := loffset + 30 + Length(afname) + p.csize;
            Inc(count);
          end;
        end
        else
        begin
          afname := GetFileName(FSection[i].name, '');
          WriteCDR(s, ZIP_COMP_STORE, zcrc, 0, 0, $10, loffset - start, afname);
          loffset := loffset + 30 + Length(afname) + 0;
          Inc(count);
        end;
      end;
    end;
    Assert(loffset = offset);
    Assert(count < $ffff);
    size := s.Position - start - offset;
    // Write EOCD header
    s.WriteBuffer(ZIP_SIGN_EOCD, 4); // EOCD Signature
    s.WriteWord(NtoLE(0));           // Disk
    s.WriteWord(NtoLE(0));           // Num of Disks
    s.WriteWord(NtoLE(count));       // Num of CDRs
    s.WriteWord(NtoLE(count));       // Total CDR entries
    s.WriteDWord(NtoLE(size));       // Central Directory size
    s.WriteDWord(NtoLE(offset));     // Central Directory offset
    s.WriteWord(NtoLE(0));           // Comment field length
  end;

  procedure TZIPEditor.SaveTo(FileName: String);
    var s: TFileStream;
  begin
    s := TFileStream.Create(FileName, fmCreate);
    try
      SaveToStream(s);
    finally
      s.Free();
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
      otherwise Result := '';
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
