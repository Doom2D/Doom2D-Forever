// pe32U.pas v0.0.1
// PE32 headers (and other shit)
// collected from the various sources by Ketmar // Invisible Vector
// public domain
{$INCLUDE a_modes.inc}
unit pe32U;

interface


const
  IMAGE_DOS_SIGNATURE = $5A4D;     // MZ
  IMAGE_DOS_MAGIC     = $5A4D;     // MZ
  IMAGE_NT_SIGNATURE  = $00004550; // PE00

  IMAGE_SIZEOF_STD_OPTIONAL_HEADER = 28;
  IMAGE_SIZEOF_NT_OPTIONAL_HEADER  = 224;
  IMAGE_NT_OPTIONAL_HDR_MAGIC      = $010B;

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

  IMAGE_FILE_EXECUTABLE_IMAGE = $0002;  // file is executable  (i.e. no unresolved externel references)
  IMAGE_FILE_32BIT_MACHINE    = $0100;  // 32 bit word machine
  IMAGE_FILE_DLL              = $2000;  // file is a DLL

  IMAGE_FILE_MACHINE_I386 = $14C;

  IMAGE_SUBSYSTEM_WINDOWS_GUI = 2;  // Windows GUI subsystem
  IMAGE_SUBSYSTEM_WINDOWS_CUI = 3;  // Windows character subsystem (console mode)

  IMAGE_DIRECTORY_ENTRY_EXPORT       = 0;   // export Directory
  IMAGE_DIRECTORY_ENTRY_IMPORT       = 1;   // import Directory
  IMAGE_DIRECTORY_ENTRY_RESOURCE     = 2;   // resource Directory
  IMAGE_DIRECTORY_ENTRY_EXCEPTION    = 3;   // exception Directory
  IMAGE_DIRECTORY_ENTRY_SECURITY     = 4;   // security Directory
  IMAGE_DIRECTORY_ENTRY_BASERELOC    = 5;   // base Relocation Table
  IMAGE_DIRECTORY_ENTRY_DEBUG        = 6;   // debug Directory
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT    = 7;   // description String
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR    = 8;   // machine Value (MIPS GP)
  IMAGE_DIRECTORY_ENTRY_TLS          = 9;   // TLS Directory
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG  = 10;  // load Configuration Directory
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11;  // Bound Import Directory in headers
  IMAGE_DIRECTORY_ENTRY_IAT          = 12;  // Import Address Table

  IMAGE_SIZEOF_SECTION_HEADER = 40;
  IMAGE_SIZEOF_SHORT_NAME     = 8;   // section name

  // section characteristics
  IMAGE_SCN_CNT_CODE               = $00000020;  // contains code
  IMAGE_SCN_CNT_INITIALIZED_DATA   = $00000040;  // contains initialized data
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = $00000080;  // contains uninitialized data

  IMAGE_SCN_MEM_DISCARDABLE        = $02000000;  // can be discarded
  IMAGE_SCN_MEM_NOT_CACHED         = $04000000;  // is not cachable
  IMAGE_SCN_MEM_NOT_PAGED          = $08000000;  // is not pageable
  IMAGE_SCN_MEM_SHARED             = $10000000;  // is shareable
  IMAGE_SCN_MEM_EXECUTE            = $20000000;  // is executable
  IMAGE_SCN_MEM_READ               = $40000000;  // is readable
  IMAGE_SCN_MEM_WRITE    = LongWord($80000000);  // is writeable

  PE_DIR_EXPORT        = 0;
  PE_DIR_IMPORT        = 1;
  PE_DIR_RESOURCE      = 2;
  PE_DIR_EXCEPTION     = 3;
  PE_DIR_SECURITY      = 4;
  PE_DIR_FIXUP         = 5;
  PE_DIR_DEBUG         = 6;
  PE_DIR_DESCRIPTION   = 7;
  PE_DIR_MACHINE       = 8;
  PE_DIR_TLS           = 9;
  PE_DIR_LOADCONFIG    = 10;
  PE_DIR_BOUNDIMPORT   = 11;
  PE_DIR_IMPORTADDR    = 12;
  PE_DIR_DELAYIMPORT   = 13;
  PE_DIR_COMPLUSHEADER = 14;

  RT_CURSOR            = 1;
  RT_BITMAP            = 2;
  RT_ICON              = 3;
  RT_MENU              = 4;
  RT_DIALOG            = 5;
  RT_STRING_TABLE      = 6;
  RT_FONT_DIR          = 7;
  RT_FONT              = 8;
  RT_ACCELERATORS      = 9;
  RT_RCDATA            = 10; // ???
  RT_MESSAGE_TABLE     = 11;
  RT_GROUP_CURSOR      = 12;
  RT_GROUP_ICON        = 14;
  RT_VERSION_INFO      = 16;
  RT_XP_MANIFEST       = 24;


type
  // DOS .EXE header
  TImageDOSHeader = packed record
    e_magic: Word;     // magic number
    e_cblp: Word;      // bytes on last page of file
    e_cp: Word;        // pages in file
    e_crlc: Word;      // relocations
    e_cparhdr: Word;   // size of header in paragraphs
    e_minalloc: Word;  // minimum extra paragraphs needed
    e_maxalloc: Word;  // maximum extra paragraphs needed
    e_ss: Word;        // initial (relative) SS value
    e_sp: Word;        // initial SP value
    e_csum: Word;      // checksum
    e_ip: Word;        // initial IP value
    e_cs: Word;        // initial (relative) CS value
    e_lfarlc: Word;    // file address of relocation table
    e_ovno: Word;      // overlay number
    e_res: packed array [0..3] of Word; // reserved words
    e_oemid: Word;     // OEM identifier (for e_oeminfo)
    e_oeminfo: Word;   // OEM information; e_oemid specific
    e_res2: packed array [0..9] of Word; // reserved words
    _lfanew: LongWord; // +$3C; file address of new exe header
  end;

  PPEHeader = ^TPEHeader;
  TPEHeader = packed record
    signature: LongWord;            //+$00; 'PE'#0#0
    machine: Word;                  //+$04; 0x14C-0x14F - x86-compatible
    numberOfSections: Word;         //+$06; max - 96 (why? %-)
    timeDateStamp: LongWord;        //+$08; shit. PE is full of shit...
    pointerToSymbolTable: LongWord; //+$0C; COFF symbol table. another shit.
    numberOfSymbols: LongWord;      //+$10; and shit again.
    ntHdrSize: Word;                //+$14; for COFF(obj) - 0, for pe - not 0. ;-)
    flags: Word;                    //+$16
      //  ok, let me tell you 'bout this stuff... %-)
      //  0: =1 - relocations is stripped. for COFF %-) PE ignores this
      //  1: =0 - image cannot be executed (really?)
      //2,3: =1 - COFF line numbers and local symbols was stripped
      //  4: =1 - aggresively trim working set (does nothing, afaik)
      //  5: =1 - application can handle >2Gb addresses (newer saw one)
      //  6: =0 - reserved, should be zero (the best flag of all! %-)
      //  7: =1 - bytes swaped (x86-like) (Windows ignores this. at least XP)
      //  8: =1 - 32-bit machine
      //  9: =1 - debug info stripped (COFF again?)
      // 10: =1 - copy to swap if executed from removable media (shit)
      // 11: =0 - reserved
      // 12: =1 - system file (and what it means?)
      // 13: =1 - DLL, else -- application (which, in fact, can be loaded like
      //          any normal DLL. if it has the export section, we even can import
      //          something from EXE. but there will be no ititialization or
      //          shutdown for EXEs (alas...%-( )
      //          note: Win2K (may be NT/XP too?) doesn't apply fixups. asshole.
      //          chorus behind the scene: I LOVE MICRO$OFT!!!
      // 14: =1 - "File should be run only on a UP machine". UP means "uniprocessor"
      // 15: =1 - big endian machine (i.e. motorolas, etc.)
      //          let's drop a sight at bit 7. WHY IT IS HERE?!
      //          chorus: I *REALLY* LOVE MICRO$OFT!!!
    // and "optional" header which is not optional at all %-)
    magic: Word;           //+$18; $010B - PE32
    linkerVersion: Word;   //+$1A; shit
    sizeOfCode: LongWord;  //+$1C; 0
    sizeOfData: LongWord;  //+$20; 0
    sizeOfBSS: LongWord;   //+$24; 0
    entryRVA: LongWord;    //+$28; rva. optional for DLLs (yep? ;-)
    baseOfCode: LongWord;  //+$2C; rva again
    baseOfData: LongWord;  //+$30; and one more rva, unused in PE
    // NT additional fields
    imageBase: LongWord;       //+$34; preferred loading address (not an RVA)
    sectionAlignment: LongWord;//+$38; default value is 0x1000 (page size)
    fileAlignment: LongWord;   //+$3C; power of 2(512-64K), if sectionAlignment is less then page size then this must match SA
    osMajor: Word;          //+$40; how's interesting...
    osMinor: Word;          //+$42; ...
    userMajor: Word;        //+$44; another meaningful fields...
    userMinor: Word;        //+$46; ...
    subsysMajor: Word;      //+$48; they driving me crazy... ;-)
    subsysMinor: Word;      //+$4A; ...
    w32VerValue: LongWord;  //+$4C; noting...
    imageSize: LongWord;    //+$50; size, in bytes, of image, including all headers; must be a multiple of Section Alignment
    headerSize: LongWord;   //+$54; combined size of MS-DOS stub, PE header, and section headers rounded up to a multiple of fileAlignment
    fileChecksum: LongWord; //+$58; significant for all drivers, any DLL loaded at boot time, and any DLL that ends up in the server. see IMAGHELP.DLL for algorithm
    subsystem: Word;        //+$5C; 2 - GUI, 3 - CONSOLE, other is shit
    dllFlags: Word;         //+$5E; bits 0..3 are reserved, bit 15 - terminal server aware, bit 13 - WDM driver, bit 11 - do not bind image
    stackReserveSize: LongWord; //+$60; max stack
    stackCommitSize: LongWord;  //+$64; stack growing up by stackCommitSize bytes
    heapReserveSize: LongWord;  //+$68; the same for heap
    heapCommitSize: LongWord;   //+$6C; ...
    loaderFlags: LongWord;      //+$70; nothing. obsolete field
    numberOfRvaAndSizes: LongWord; //+$74; # of valid objects in the following directory

    //+$78; IMAGE_DATA_DIRECTORY - 16 entries
    case Integer of
    0: (
      exportTableRVA: LongWord;          //+$78
      totalExportDataSize: LongWord;     //+$7C
      importTableRVA: LongWord;          //+$80
      totalImportDataSize: LongWord;     //+$84
      resourceTableRVA: LongWord;        //+$88
      totalResourceDataSize: LongWord;   //+$8C
      exceptionTableRVA: LongWord;       //+$90 not used on x86
      totalExceptionDataSize: LongWord;  //+$94
      securityTableRVA: LongWord;        //+$98 this will NOT be load into memory
      totalSecurityDataSize: LongWord;   //+$9C
      relocationTableRVA: LongWord;      //+$A0
      totalRelocationDataSize: LongWord; //+$A4
      debugTableRVA: LongWord;           //+$A8
      totalDebugDataSize: LongWord;      //+$AC
      descriptionRVA: LongWord;          //+$B0
      totalDescriptionDataSize: LongWord;//+$B4
      machineSpecificRVA: LongWord;      //+$B8
      totalMachineSpecificDataSize: LongWord;//+$BC
      tlsTableRVA: LongWord;  //+$C0 statical data must NOT be used in DLLs that can be LoadLibrary'ed
      totalTLSDataSize: LongWord;        //+$C4
      loadConfigurationTableRVA: LongWord;//+$C8
      totalLoadConfigurationDataSize: LongWord;//+$CC
      boundImportTableRVA: LongWord;     //+$D0
      totalBoundImportDataSize: LongWord;//+$D4
      iatTableRVA: LongWord;             //+$D8
      totalIATDataSize: LongWord;        //+$DC
      delayImportTableRVA: LongWord;     //+$E0
      totalDelayImportDataSize: LongWord;//+$E4
      comPlusRuntimeHeaderRVA: LongWord; //+$E8
      comPlusRuntimeHeaderDataSize: LongWord;//+$EC
      resObjData: packed array [0..1] of LongWord;//+$F0
    );
    1: (dirEntries: packed array [0..15] of record rva, size: LongWord; end);
    // if rva = 0 - field is unused
  end;

  PPESectionInfo = ^TPESectionInfo;
  TPESectionInfo = packed record
    name: packed array [0..7] of Char;
    virtualSize: LongWord;       //+$08; if this value is greater than PhysicalSize, the section is zero-padded
    rva: LongWord;               //+$0C;
    physSize: LongWord;          //+$10; on disk
    physOffset: LongWord;        //+$14; in file, can be 0 if section contains only uninitialized data
    fixupPointer: LongWord;      //+$18; file pointer, COFF only
    lineNumberPointer: LongWord; //+$1C; COFF only
    fixupCount: Word;            //+$20; COFF only
    lineCount: Word;             //+$22; COFF only
    flags: LongWord;             //+$24
      //  bits 0-5 are reserved
      //  6: =1 - contains executable code
      //  7: =1 - contains initialized data
      //  8: =1 - contains uninitialized data
      //  9-24: reserved
      // 25: =1 - can be discarded
      // 26: =1 - cannot be cached
      // 27: =1 - not pageable
      // 28: =1 - can be shared in memory (can or should???)
      // 29: =1 - can be executed
      // 30: =1 - can be read
      // 31: =1 - can be written to
  end;

  PPEImportRec = ^TPEImportRec;
  TPEImportRec = packed record
    origFirstThunkRVA: LongWord;    //+$00, can be 0, then take firstThunkRVA
    timeDateStamp: LongWord;        //+$04
    forwarderChainRVA: LongWord;    //+$08
    nameRVA: LongWord;              //+$0C
    firstThunkRVA: LongWord;        //+$10
  end;

  PPEImportByName = ^TPEImportByName;
  TPEImportByName = packed record
    hint: Word;
    name: packed array [0..0] of Char;
  end;

  PPEExportDir = ^TPEExportDir;
  TPEExportDir = packed record
    characteristics: LongWord;
    timeDateStamp: LongWord;
    majorVersion: Word;
    minorVersion: Word;
    nameRVA: LongWord;
    base: LongWord;
    functionCount: LongWord;
    nameCount: LongWord;
    functionsRVA: LongWord;
    namesRVA: LongWord;
    nameOrdinalsRVA: LongWord;
  end;

  PPETLSCallback = procedure (dllHandle: Pointer; reason: LongWord; reserved: Pointer); stdcall;

  PPETLSDir = ^TPETLSDir;
  TPETLSDir = packed record
    rawDataStart: LongWord;        // ~rva, add baseOfCode to obtain rva
    rawDataEnd: LongWord;          // ~rva, add baseOfCode to obtain rva
    indexRVA: LongWord;            // rva (PDWORD, as Jedi says). wtf???
    callbacksRVA: LongWord;
      // rva; array of PPETLSCallback (ends with zero dword)
    sizeOfZeroFill: LongWord;      // ???
    characteristics: LongWord;     // zero (afais)
  end;

  PPEResourceDirTable = ^TPEResourceDirTable;
  TPEResourceDirTable = packed record
    characteristics: LongWord;     // unused?
    timeDateStamp: LongWord;
    majorVersion: Word;
    minorVersion: Word;
    numberOfNamedEntries: Word;
    numberOfIdEntries: Word;
  end;

  // this immediately follows TPEResourceDirTable, named first
  PPEResourceDirEntry = ^TPEResourceDirEntry;
  TPEResourceDirEntry = packed record
    id: LongWord;
      // bit 31:
      //  =0: number
      //  =1: name, ofs from the beginning of the resource raw data,
      //            unicode string, first word is len, no trailing zero
      // for ROOT dir (level 0): RT_xxx
      // level 1: resource id or resource name
      // level 2: language-id (must be a number)
    subdirOfs: LongWord;
      // offset from the beginning of the resource raw data
      // bit 31:
      //  =0: to TPEResourceDataEntry
      //  =1: to the next dir (TPEResourceDirTable)
  end;

  PPEResourceDataEntry = ^TPEResourceDataEntry;
  TPEResourceDataEntry = packed record
    dataOfs: LongWord;
    size: LongWord;
    codePage: LongWord;
    reserved: LongWord;
  end;

  PPEResourceTableDirEntry = ^TPEResourceTableDirEntry;
  TPEResourceTableDirEntry = packed record
    table: TPEResourceDirTable;
    directory: TPEResourceDirEntry;
  end;

  PPEIconDirEntry = ^TPEIconDirEntry;
  TPEIconDirEntry = packed record
    width: Byte;
    height: Byte;
    colorCount: Byte;
    reserved: Byte;
    planes: Word;
    bitCount: Word;
    bytesInRes: LongWord;
    id: Word;
  end;

  PPEIconDir = ^TPEIconDir;
  TPEIconDir = packed record
    reserved: Word;
    resType: Word;
    count: Word;
    //entries: packed array [0..31] of TPEIconDirEntry;
  end;

// peImg: buffer with PE image (loaded as-is)
// functions doesn't validate PE

// can return nil (out of image data)
// but this can be BSS!
function rva2ptr (peImg: Pointer; rva: LongWord): Pointer;

function getPEHeaderPtr (peImg: Pointer): PPEHeader;
function getPESectionsPtr (peImg: Pointer): PPESectionInfo;

// simple checks only!
function isValidPE (peImg: Pointer): Boolean;


type
  PResData = ^TResData;
  TResData = record
    rtype: LongWord;
    data: PChar; // читаем данные отсюда
    size: LongWord;
    rvaPatch: PLongWord; // ставится не здесь
  end;

// ищем первый ресурс с типом rt
function peFindFirstRT (pe: Pointer; rt: LongWord; out rdo: TResData; cnt: Integer=-1): Boolean;


implementation


function getPEHeaderPtr (peImg: Pointer): PPEHeader;
begin
  result := PPEHeader(PtrUInt(PtrUInt(peImg)+PInteger(PtrUInt(peImg)+$3C)^));
end;


function getPESectionsPtr (peImg: Pointer): PPESectionInfo;
var
  h: PPEHeader;
begin
  h := getPEHeaderPtr(peImg);
  result := PPESectionInfo(PtrUInt(PtrUInt(h)+h.ntHdrSize+$18));
end;


function rva2ptr (peImg: Pointer; rva: LongWord): Pointer;
var
  h: PPEHeader;
  si: PPESectionInfo;
  c: Integer;
begin
  h := getPEHeaderPtr(peImg);
  c := h.numberOfSections;
  si := Pointer(PtrUInt(PtrUInt(h)+h.ntHdrSize+$18));
  while (c > 0) do
  begin
    if (si.rva <> 0) and (si.physSize <> 0) and
       (rva >= si.rva) and (rva < si.rva+si.physSize) then
    begin
      result := Pointer(PtrUInt(PtrUInt(peImg)+si.physOffset+(rva-si.rva)));
      exit;
    end;
    Dec(c);
    Inc(si);
  end;
  if (rva < h.headerSize) then result := Pointer(PtrUInt(PtrUInt(peImg)+rva)) else result := nil;
end;


function isValidPE (peImg: Pointer): Boolean;
var
  lfanew: LongWord;
  h: PPEHeader;
begin
  result := false;
  if (peImg = nil) then exit;
  try
    //if IsBadReadPtr(peImg, $40) then exit;
    if (PWord(peImg)^ <> IMAGE_DOS_MAGIC) then exit;
    lfanew := PLongWord(PtrUInt(peImg)+$3C)^;
    if (lfanew = 0) then exit;
    h := Pointer(PtrUInt(PtrUInt(peImg)+lfanew));
    //if IsBadReadPtr(h, SizeOf(TPEHeader)) then exit;
    if (h.signature <> IMAGE_NT_SIGNATURE) or
       (h.magic <> IMAGE_NT_OPTIONAL_HDR_MAGIC) or
       (h.machine < $14C) or (h.machine > $14F) then exit;
  except // sorry
    exit;
  end;
  result := true;
end;


function peFindFirstRT (pe: Pointer; rt: LongWord; out rdo: TResData; cnt: Integer=-1): Boolean;
var
  h: PPEHeader;
  rptr: PtrUInt;
  rtbl: PPEResourceDirTable;
  re, re1: PPEResourceDirEntry;
  rd: PPEResourceDataEntry;
  f: Integer;
  nc: Integer;
begin
  result := false;
  rdo.data := nil;
  rdo.size := 0;
  rdo.rvaPatch := nil;
  h := getPEHeaderPtr(pe);
  if (h.resourceTableRVA = 0) or (h.totalResourceDataSize = 0) then exit;
  rptr := PtrUInt(rva2ptr(pe, h.resourceTableRVA));
  if (rptr = 0) then exit;
  rtbl := PPEResourceDirTable(rptr);
  nc := Integer(rtbl.numberOfNamedEntries);
  f := Integer(rtbl.numberOfIdEntries);
  Inc(f, nc);
  if (f = 0) then exit;
  re := Pointer(PtrUInt(rptr+sizeof(TPEResourceDirTable)));
  while (f > 0) do
  begin
    if (re.id = rt) then
    begin
      if cnt > 0 then
      begin
        Dec(cnt);
      end
      else
      begin
        // ищем собственно ресурс
        re1 := re;
        while ((re1.subdirOfs and $80000000) <> 0) do
        begin
          rtbl := PPEResourceDirTable(PtrUInt(rptr+(re1.subdirOfs and $7FFFFFFF)));
          //nc := Integer(rtbl.numberOfNamedEntries)+Integer(rtbl.numberOfIdEntries);
          //if nc = 0 then Error('resource shit!');
          re1 := PPEResourceDirEntry(PtrUInt(PtrUInt(rtbl)+sizeof(TPEResourceDirTable)));
        end;
        rd := PPEResourceDataEntry(PtrUInt(rptr+re1.subdirOfs));
        rdo.size := rd.size;
        // а здесь -- RVA; заебись некроблядь суперпоследовательна
        rdo.data := rva2ptr(pe, rd.dataOfs);
        if (rdo.data = nil) then rdo.size := 0 else begin rdo.rtype := rt; result := true; exit; end;
      end;
    end;
    Inc(re);
    Dec(f);
  end;
end;


end.
