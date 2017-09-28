// coded by Ketmar // Invisible Vector
{.$DEFINE WRITE_RAW_SECTIONS}
{$INCLUDE a_modes.inc}
{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}
program expdump;

uses
  SysUtils, Classes,
  pe32U in 'pe32U.pas';


const
  secInfoFlags: array [0..9] of record flg: LongWord; name: string[8]; end = (
    (flg:$00000020; name:'code'),
    (flg:$00000040; name:'data'),
    (flg:$00000080; name:'bss'),
    (flg:$02000000; name:'disc'),
    (flg:$04000000; name:'no-cache'),
    (flg:$08000000; name:'no-page'),
    (flg:$10000000; name:'shr'),
    (flg:$20000000; name:'exe'),
    (flg:$40000000; name:'rd'),
    (flg:$80000000; name:'wr'));

type
  TExportNameRecord = record
    name: AnsiString;  // empty: by ordinal
    ordinal: Word;
  end;


var
  inFile: AnsiString = '';

  showSexInfo: Boolean = true;
  showExports: Boolean = true;

  pe: Pointer = nil;
  peSize: Integer = 0;
  isDll: Boolean = false;

  expDLLName: AnsiString = '';
  expOrdBase: LongWord = 0;
  expNames: array of TExportNameRecord = nil;
  expOrds: array of LongWord = nil; // rva's
  expFwds: array of AnsiString = nil;   // <> '': no fwd
  //expHasFwd: Boolean = false;


// ////////////////////////////////////////////////////////////////////////// //
function itoa (i: Integer): AnsiString; inline; begin str(i, result); end;


function i2hex (i: LongWord; len: Integer): AnsiString;
const
  hexD: packed array [0..15] of AnsiChar = '0123456789ABCDEF';
var
  o: packed array [0..22] of AnsiChar;
  p: Integer;
begin
  p := High(o);
  repeat
    o[p] := hexD[i and $0F]; Dec(p);
    i := (i shr 4) and $0FFFFFFF;
    if (len > 0) then Dec(len);
  until (i = 0);
  i := High(o)-p;
  Inc(p);
  if (len < 0) then len := 0;
  SetLength(result, len+Integer(i));
  i := 1;
  while (len > 0) do begin result[i] := '0'; Inc(i); Dec(len); end;
  while (p <= High(o)) do begin result[i] := o[p]; Inc(i); Inc(p); end;
end;


procedure fatal (const msg: AnsiString);
begin
  writeln('***fatal: ', msg);
  Halt(1);
end;


function loadPE (const fileName: AnsiString): Boolean;
var
  st: TStream = nil;
begin
  result := false;
  try
    st := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
    peSize := st.size;
    if (peSize < 1024) then begin st.Free(); exit; end;
    ReallocMem(pe, peSize);
    st.ReadBuffer(pe^, peSize);
  except // sorry
    st.Free();
    exit;
  end;
  st.Free();
  result := true;
end;


procedure checkPE ();
var
  h: PPEHeader;
  si: PPESectionInfo;
  f, c: Integer;
  s: ShortString;
  {pkl,} comma: Boolean;
begin
  //pkl := false;
  h := getPEHeaderPtr(pe);
  isDll := ((h.flags and IMAGE_FILE_DLL) <> 0);
  //if (h.flags and IMAGE_FILE_DLL) <> 0 then Error('DLL: not yet');
  //!!if h.baseOfCode <> $1000 then Error('invalid base_of_code');
  c := h.numberOfSections;
  if (c = 0) then fatal('no sections');
  if (c > 127) then fatal('invalid number of sections');
  si := getPESectionsPtr(pe);
  if isDll then writeln('this PE is DLL');
  if showSexInfo then
  begin
    writeln(
      'image: base=$', i2hex(h.imageBase, 8),
      '; size=$', i2hex(h.imageSize, 8),
      '; entry=$', i2hex(h.entryRVA, 8));
    writeln('name     rva      ofs      size     vsize    flags');
  end;
  while (c > 0) do
  begin
    s[0] := #8;
    Move(si.name[0], s[1], 8);
    for f := 1 to 8 do if not (s[f] in [#32..#126]) then s[f] := ' ';
    if showSexInfo then
    begin
      write(s, ' ',
        '', i2hex(si.rva, 8),
        ' ', i2hex(si.physOffset, 8),
        ' ', i2hex(si.physSize, 8),
        ' ', i2hex(si.virtualSize, 8),
        ' ', i2hex(si.flags, 8));
      comma := false;
      for f := 0 to High(secInfoFlags) do
      begin
        if (si.flags and secInfoFlags[f].flg) <> 0 then
        begin
          if comma then write(', ') else write('; ');
          comma := true;
          write(secInfoFlags[f].name);
        end;
      end;
      writeln;
    end;
    //if s = '.pklstb ' then pkl := true;
    {
    if not doNotPack then
    begin
      if (si.rva <> 0) and (si.physSize <> 0) and (si.physOffset <> 0) and
         (((si.flags and IMAGE_SCN_MEM_DISCARDABLE) = 0) or
          ((si.flags and IMAGE_SCN_MEM_EXECUTE) <> 0)) and
         ((si.flags and IMAGE_SCN_MEM_WRITE) <> 0) then
      begin
        if (si.flags and IMAGE_SCN_MEM_SHARED) <> 0 then
          Error('writeable shared sections: not yet', true);
      end;
    end;
    }
    Dec(c); Inc(si);
  end;
  //if pkl then Error('probably PKLITEd file', true);
end;


procedure checkImports ();
var
  h: PPEHeader;
  ir: PPEImportRec;
  th: LongWord;
  p: PLongWord;
begin
  h := getPEHeaderPtr(pe);
  if (h.importTableRVA = 0) or (h.totalImportDataSize = 0) then exit;
  ir := rva2ptr(pe, h.importTableRVA);
  if (ir = nil) then fatal('invalid import_directory_entry');
  while (ir.nameRVA <> 0) do
  begin
    //!!if ir.nameRVA < h.baseOfCode then Error('invalid import directory');
    th := ir.origFirstThunkRVA;
    if (th = 0) then th := ir.firstThunkRVA;
    p := rva2ptr(pe, th);
    if (th <> 0) and (p <> nil) then
    begin
      while (p^ <> 0) do
      begin
        //if (p^ and $7FFFFFFF) < $1000 then Error('invalid import directory');
        if (p^ < $1000) then fatal('invalid import directory');
        Inc(p);
      end;
    end;
    Inc(ir);
  end;
end;


function getStrz (rva: LongWord): AnsiString;
var
  len: Integer;
  p, pc: PAnsiChar;
begin
  pc := rva2ptr(pe, rva);
  p := pc;
  len := 0;
  while (p^ <> #0) do begin Inc(len); Inc(p); end;
  SetString(result, pc, len);
end;


procedure extractExports ();
var
  h: PPEHeader;
  p: PtrUInt;
  f, nof, non, frva, nrva, norva: LongWord;
begin
  expOrdBase := expOrdBase; // shut up, fpc!
  h := getPEHeaderPtr(pe);
  if (h.exportTableRVA = 0) or (h.totalExportDataSize = 0) then exit;
  p := PtrUInt(rva2ptr(pe, h.exportTableRVA))+3*4;
  expDLLName := getStrz(PLongWord(p)^);
  Inc(p, 4);
  if (expDLLName = '') then fatal('invalid DLL name');
  expOrdBase := PLongWord(p)^; Inc(p, 4);
  nof := PLongWord(p)^; Inc(p, 4);
  non := PLongWord(p)^; Inc(p, 4);
  frva := PLongWord(p)^; Inc(p, 4);  // функции (nof)
  nrva := PLongWord(p)^; Inc(p, 4);  // имена (non)
  norva := PLongWord(p)^;{Inc(p, 4);}// оридналы имён (non)
  if (nof = 0) then fatal('invalid export section (0)');
  // rva для экспортов
  SetLength(expOrds, nof);
  SetLength(expFwds, nof);
  p := PtrUInt(rva2ptr(pe, frva));
  for f := 0 to nof-1 do
  begin
    expOrds[f] := PLongWord(p)^; Inc(p, 4);
    // проверим на форварды
    if (expOrds[f] <> 0) and
       (expOrds[f] >= h.exportTableRVA) and
       (expOrds[f] < h.exportTableRVA+h.totalExportDataSize) then
    begin
      expFwds[f] := getStrz(expOrds[f]);
      //expHasFwd := true;
    end
    else
    begin
      expFwds[f] := '';
    end;
  end;
  // экспорты по именам
  SetLength(expNames, non);
  if (non <> 0) then
  begin
    p := PtrUInt(rva2ptr(pe, nrva));
    for f := 0 to non-1 do begin expNames[f].name := getStrz(PLongWord(p)^); Inc(p, 4); end;
    p := PtrUInt(rva2ptr(pe, norva));
    for f := 0 to non-1 do begin expNames[f].ordinal := PWord(p)^; Inc(p, 2); end;
  end;
  // отладочный дамп
  if not showExports then exit;
  writeln(nof, ' ordinals, ', non, ' names');
  {
  writeln('ordinals:');
  for f := 0 to nof-1 do
  begin
    if expFwds[f] <> '' then writeln('  ', f+expOrdBase, ': "', expFwds[f], '"') else writeln('  ', f+expOrdBase);
  end;
  }
  writeln('index ordinal name');
  for f := 0 to non-1 do writeln(f:5, ' ', expNames[f].ordinal:7, ' ', expNames[f].name);
  writeln;
end;


// ////////////////////////////////////////////////////////////////////////// //
begin
  if (ParamCount <> 1) then
  begin
    writeln('FILENAME!');
    Halt(1);
  end;
  inFile := ParamStr(1);
  //inFile := '../libjit-0.dll';

  writeln('loading PE...');
  if not loadPE(inFile) then fatal('can''t load PE file');
  writeln('checking PE...');
  if not isValidPE(pe) then fatal('invalid PE file');
  checkPE();
  checkImports();
  extractExports();
end.
