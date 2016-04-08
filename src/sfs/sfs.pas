// streaming file system (virtual)
{$MODE DELPHI}
{.$R-}
unit sfs;

interface

uses
  SysUtils, Classes, Contnrs;


type
  ESFSError = class(Exception);

  TSFSChar = AnsiChar;
  TSFSString = AnsiString;

  TSFSVolume = class;

  TSFSFileInfo = class
  public
    fOwner: TSFSVolume; // так, на всякий случай
    fPath: TSFSString;  // разделители каталогов -- "/"; корень никак не обозначен, если не пустое, обязано завершается "/"
    fName: TSFSString;  // только имя
    fSize: Int64;       // unpacked
    fOfs: Int64;        // in VFS (many of 'em need this %-)

    constructor Create (pOwner: TSFSVolume);
    destructor Destroy (); override;

    property path: TSFSString read fPath;
    property name: TSFSString read fName;
    property size: Int64 read fSize;
  end;

  // виртуальная файловая система. ТОЛЬКО ДЛЯ ЧТЕНИЯ!
  // том НЕ ДОЛЖЕН убиваться никак иначе, чем при помощи фабрики!
  TSFSVolume = class
  protected
    fRC: Integer; // refcounter for other objects
    fFileName: TSFSString;// обычно имя оригинального файла
    fFileStream: TStream; // обычно поток для чтения оригинального файла
    fFiles: TObjectList;  // TSFSFileInfo или наследники

    // пришибить все структуры.
    // не должна падать, если её вызывают несколько раз.
    procedure Clear (); virtual;

    // вызывается из DoDirectoryRead() для заполнения списка файлов.
    // считается, что все магики уже проверены и файл точно наш.
    // fFileName, fFileStream уже установлены, fFiles создан,
    // в нём, скорее всего, никого нет.
    // позиция потока -- та, что оставила фабрика.
    // при ошибках кидать исключение, тогда том будет прибит фабрикой.
    // разделители путей должны быть только "/", корневой "/" должен
    // быть опущен, пути (если не пустые) должны завершаться "/"!
    // fName должно содержать только имя, fPath -- только путь.
    // в принципе, об этом позаботится DoDirectoryRead(), но зачем
    // давать ему лишнюю работу?
    procedure ReadDirectory (); virtual; abstract;

    // найти файл, вернуть его индекс в fFiles.
    // эта процедура может менять fFiles!
    // fPath -- в правильной форме, с "/", корневой "/" убит, финальный добавлен.
    // если файл не найден, вернуть -1.
    function FindFile (const fPath, fName: TSFSString): Integer; virtual;

    // возвращает количество файлов в fFiles
    function GetFileCount (): Integer; virtual;

    // возвращает файл с индексом index.
    // может возвращать NIL.
    // никаких падений на неправильные индексы!
    function GetFiles (index: Integer): TSFSFileInfo; virtual;

  public
    // pSt не обязательно запоминать, если он не нужен.
    constructor Create (const pFileName: TSFSString; pSt: TStream); virtual;
    // fFileStream уничтожать нельзя, если он равен параметру pSt конструктора.
    destructor Destroy (); override;

    // вызывает ReadDirectory().
    // эта процедура сама разберётся с дубликатами имён: подобавляет в
    // конец имён-дубликатов подчёркивание и десятичный номер.
    // также она нормализует вид имён.
    procedure DoDirectoryRead ();

    // при ошибках кидаться исключениями.
    function OpenFileByIndex (const index: Integer): TStream; virtual; abstract;

    // если не смогло откупорить файло (или ещё где ошиблось), зашвырнёт исключение.
    function OpenFileEx (const fName: TSFSString): TStream; virtual;

    property FileCount: Integer read GetFileCount; // может вернуть ноль
    // может возвращать NIL.
    // никаких падений на неправильные индексы!
    property Files [index: Integer]: TSFSFileInfo read GetFiles;
  end;

  // фабрика томов. все SFS при старте добавляют свои фабрики.
  // благодаря этому можно создавать разные всякие SFS стандартным
  // вызовом стандартной процедуры.
  // фабрика НЕ ДОЛЖНА убиваться никак иначе, чем при помощи вызова
  // SFSUnregisterVolumeFactory()! это гарантирует, что движок
  // перед расстрелом отдаст ей все её тома.
  TSFSVolumeFactory = class
  public
    // если добавляем файл данных файл с именем типа "zip:....", то
    // SFS извлечёт это "zip" и передаст в сию функцию.
    // ежели функция вернёт правду, то SFS вызовет Produce для данного
    // файла. если ни одна фабрика префикс не признает, то файл не откроют.
    // используется для скипания автодетекта.
    // SFS НЕ СЧИТАЕТ ПРЕФИКСОМ СТРОКУ КОРОЧЕ ТРЁХ СИМВОЛОВ!
    function IsMyVolumePrefix (const prefix: TSFSString): Boolean; virtual; abstract;
    // проверяет, может ли фабрика сделать том для данного файла.
    // st -- открытый для чтения файловй поток. указатель чтения стоит в начале.
    // этот поток нельзя закрывать!
    // prefix: то, что было передано в IsMyVolumePrefix() или ''.
    // исключение считается ошибкой, возврат NIL считается ошибкой.
    function Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume; virtual; abstract;
    // когда том больше не нужен, он будет отдан фабрике на переработку.
    // далее движок не будет юзать сей том.
    procedure Recycle (vol: TSFSVolume); virtual; abstract;
  end;

  // "итератор", возвращаемый SFSFileList()
  TSFSFileList = class
  protected
    fVolume: TSFSVolume;

    function GetCount (): Integer;
    function GetFiles (index: Integer): TSFSFileInfo;

  public
    constructor Create (const pVolume: TSFSVolume);
    destructor Destroy (); override;

    property Volume: TSFSVolume read fVolume;
    property Count: Integer read GetCount;
    // при неправильном индексе молча вернёт NIL.
    // при правильном тоже может вернуть NIL!
    // очень не советую менять содержимое полученного класса.
    // конечно, я мог бы возвращать новую структуру или нечто похожее,
    // но блин, если ты идиот и не умеешь даже комменты читать, то
    // какого ты вообще в программинг полез?
    property Files [index: Integer]: TSFSFileInfo read GetFiles; default;
  end;


procedure SFSRegisterVolumeFactory (factory: TSFSVolumeFactory);
// эта функция автоматически прибьёт factory.
procedure SFSUnregisterVolumeFactory (factory: TSFSVolumeFactory);

// добавить сборник в постоянный список.
// если сборник с таким именем уже открыт, то не открывает его повторно.
// никогда не кидает исключений.
// top: добавить в начало списка поиска.
// вернёт ложь при ошибке.
// способно открывать сборники в сборниках при помощи крутых имён a-la:
// "zip:pack0::pack:pack1::wad2:pack2".
// в дальнейшем следует обращаться к сборнику как "pack2::xxx".
// или можно написать:
// "zip:pack0::pack:pack1::wad2:pack2|datafile".
// и обращаться как "datafile::xxx".
// "||" преобразуются в простой "|" и разделителем не считаются.
// принимается во внимание только последняя труба.
function SFSAddDataFile (const dataFileName: TSFSString; top: Boolean=false): Boolean;

// добавить в постоянный список сборник из потока ds.
// если возвращает истину, то SFS становится влядельцем потока ds и сама
// угробит сей поток по необходимости.
// virtualName становится именем сборника для операции открытия файла типа
// "packfile:file.ext".
// если какой-нибудь сборник с именем virtualName уже открыт, вернёт false.
// никогда не кидает исключений.
// top: добавить в начало списка поиска.
// вернёт ложь при ошибке.
// открывает сборник из потока. dataFileName -- ВИРТУАЛЬНОЕ имя.
// т.е. на самом деле такого файла может и не быть на диске.
function SFSAddSubDataFile (const virtualName: TSFSString; ds: TStream; top: Boolean=false): Boolean;

// швыряется исключениями.
// если fName не имеет указания на файл данных (это то, что отделено от
// остального имени двоеточием), то ищем сначала по всем зарегистрированным
// файлам данных, потом в текущем каталоге, потом в каталоге, откуда стартовали.
// если ничего не нашли, кидаем исключение.
function SFSFileOpenEx (const fName: TSFSString): TStream;

// при ошибке -- NIL, и никаких исключений.
function SFSFileOpen (const fName: TSFSString): TStream;

// возвращает NIL при ошибке.
// после использования, натурально, итератор надо грохнуть %-)
function SFSFileList (const dataFileName: TSFSString): TSFSFileList;

function SFSReplacePathDelims (const s: TSFSString; newDelim: TSFSChar): TSFSString;
// игнорирует регистр символов
function SFSStrEqu (const s0, s1: TSFSString): Boolean;

// разобрать толстое имя файла, вернуть виртуальное имя последнего списка
// или пустую стороку, если списков не было.
function SFSGetLastVirtualName (const fn: TSFSString): string;

// преобразовать число в строку, красиво разбавляя запятыми
function Int64ToStrComma (i: Int64): string;

// Wildcard matching
// this code is meant to allow wildcard pattern matches. tt is VERY useful
// for matching filename wildcard patterns. tt allows unix grep-like pattern
// comparisons, for instance:
//
//       ?       Matches any single characer
//       +       Matches any single characer or nothing
//       *       Matches any number of contiguous characters
//       [abc]   Matches a or b or c at that position
//       [!abc]  Matches anything but a or b or c at that position
//       [a-e]   Matches a through e at that position
//
//       'ma?ch.*'       -Would match match.exe, mavch.dat, march.on, etc
//       'this [e-n]s a [!zy]est' -Would match 'this is a test', but would
//                                 not match 'this as a yest'
//
function WildMatch (pattern, text: TSFSString): Boolean;
function WildListMatch (wildList, text: TSFSString; delimChar: AnsiChar=':'): Integer;
function HasWildcards (const pattern: TSFSString): Boolean;


var
  // правда: разрешено искать файло не только в файлах данных, но и на диске.
  sfsDiskEnabled: Boolean = true;
  // правда: если файл не префиксован, то сначала ищем файло на диске,
  // потом в файлах данных.
  sfsDiskFirst: Boolean = true;
  // правда: даже для префиксованых файлов сначала просмотрим диск
  // (если установлен флажок sfsDiskFirst и sfsDiskEnabled).
  sfsForceDiskForPrefixed: Boolean = false;
  // список дисковых каталогов для поиска файла. если пуст -- ищем только в
  // текущем. каталоги разделяются трубой ("|").
  // <currentdir> заменяется на текущий каталог (с завершающим "/"),
  // <exedir> заменяется на каталог, где сидит .EXE (с завершающим "/").
  sfsDiskDirs: TSFSString = '<currentdir>|<exedir>';


implementation

uses
  xstreams;


function Int64ToStrComma (i: Int64): string;
var
  f: Integer;
begin
  Str(i, result);
  f := Length(result)+1;
  while f > 4 do
  begin
    Dec(f, 3); Insert(',', result, f);
  end;
end;


const
  // character defines
  WILD_CHAR_ESCAPE         = '\';
  WILD_CHAR_SINGLE         = '?';
  WILD_CHAR_SINGLE_OR_NONE = '+';
  WILD_CHAR_MULTI          = '*';
  WILD_CHAR_RANGE_OPEN     = '[';
  WILD_CHAR_RANGE          = '-';
  WILD_CHAR_RANGE_CLOSE    = ']';
  WILD_CHAR_RANGE_NOT      = '!';


function HasWildcards (const pattern: TSFSString): Boolean;
begin
  result :=
    (Pos(WILD_CHAR_ESCAPE, pattern) <> 0) or
    (Pos(WILD_CHAR_SINGLE, pattern) <> 0) or
    (Pos(WILD_CHAR_SINGLE_OR_NONE, pattern) <> 0) or
    (Pos(WILD_CHAR_MULTI, pattern) <> 0) or
    (Pos(WILD_CHAR_RANGE_OPEN, pattern) <> 0);
end;

function MatchMask (const pattern: TSFSString; p, pend: Integer; const text: TSFSString; t, tend: Integer): Boolean;
var
  rangeStart, rangeEnd: AnsiChar;
  rangeNot, rangeMatched: Boolean;
  ch: AnsiChar;
begin
  // sanity checks
  if (pend < 0) or (pend > Length(pattern)) then pend := Length(pattern);
  if (tend < 0) or (tend > Length(text)) then tend := Length(text);
  if t < 1 then t := 1;
  if p < 1 then p := 1;
  while p <= pend do
  begin
    if t > tend then
    begin
      // no more text. check if there's no more chars in pattern (except "*" & "+")
      while (p <= pend) and
            ((pattern[p] = WILD_CHAR_MULTI) or
             (pattern[p] = WILD_CHAR_SINGLE_OR_NONE)) do Inc(p);
      result := (p > pend);
      exit;
    end;
    case pattern[p] of
      WILD_CHAR_SINGLE: ;
      WILD_CHAR_ESCAPE:
        begin
          Inc(p);
          if p > pend then result := false else result := (pattern[p] = text[t]);
          if not result then exit;
        end;
      WILD_CHAR_RANGE_OPEN:
        begin
          result := false;
          Inc(p); if p > pend then exit; // sanity check
          rangeNot := (pattern[p] = WILD_CHAR_RANGE_NOT);
          if rangeNot then begin Inc(p); if p > pend then exit; {sanity check} end;
          if pattern[p] = WILD_CHAR_RANGE_CLOSE then exit; // sanity check
          ch := text[t]; // speed reasons
          rangeMatched := false;
          repeat
            if p > pend then exit; // sanity check
            rangeStart := pattern[p];
            if rangeStart = WILD_CHAR_RANGE_CLOSE then break;
            Inc(p); if p > pend then exit; // sanity check
            if pattern[p] = WILD_CHAR_RANGE then
            begin
              Inc(p); if p > pend then exit; // sanity check
              rangeEnd := pattern[p]; Inc(p);
              if rangeStart < rangeEnd then
              begin
                rangeMatched := (ch >= rangeStart) and (ch <= rangeEnd);
              end
              else rangeMatched := (ch >= rangeEnd) and (ch <= rangeStart);
            end
            else rangeMatched := (ch = rangeStart);
          until rangeMatched;
          if rangeNot = rangeMatched then exit;

          // skip the rest or the range
          while (p <= pend) and (pattern[p] <> WILD_CHAR_RANGE_CLOSE) do Inc(p);
          if p > pend then exit; // sanity check
        end;
      WILD_CHAR_SINGLE_OR_NONE:
        begin
          Inc(p);
          result := MatchMask(pattern, p, pend, text, t, tend);
          if not result then result := MatchMask(pattern, p, pend, text, t+1, tend);
          exit;
        end;
      WILD_CHAR_MULTI:
        begin
          while (p <= pend) and (pattern[p] = WILD_CHAR_MULTI) do Inc(p);
          result := (p > pend); if result then exit;
          while not result and (t <= tend) do
          begin
            result := MatchMask(pattern, p, pend, text, t, tend);
            Inc(t);
          end;
          exit;
        end;
      else result := (pattern[p] = text[t]); if not result then exit;
    end;
    Inc(p); Inc(t);
  end;
  result := (t > tend);
end;


function WildMatch (pattern, text: TSFSString): Boolean;
begin
  if pattern <> '' then pattern := AnsiLowerCase(pattern);
  if text <> '' then text := AnsiLowerCase(text);
  result := MatchMask(pattern, 1, -1, text, 1, -1);
end;

function WildListMatch (wildList, text: TSFSString; delimChar: AnsiChar=':'): Integer;
var
  s, e: Integer;
begin
  if wildList <> '' then wildList := AnsiLowerCase(wildList);
  if text <> '' then text := AnsiLowerCase(text);
  result := 0;
  s := 1;
  while s <= Length(wildList) do
  begin
    e := s; while e <= Length(wildList) do
    begin
      if wildList[e] = WILD_CHAR_RANGE_OPEN then
      begin
        while (e <= Length(wildList)) and (wildList[e] <> WILD_CHAR_RANGE_CLOSE) do Inc(e);
      end;
      if wildList[e] = delimChar then break;
      Inc(e);
    end;
    if s < e then
    begin
      if MatchMask(wildList, s, e-1, text, 1, -1) then exit;
    end;
    Inc(result);
    s := e+1;
  end;
  result := -1;
end;


type
  TVolumeInfo = class
    fFactory: TSFSVolumeFactory;
    fVolume: TSFSVolume;
    fPackName: TSFSString; // для одного и того же файла будет только один том!
    fStream: TStream; // файловый поток для сборника
    fPermanent: Boolean; // истина -- не будет угроблена, если не останется ни одного открытого тома
    // истина -- этот том был создан из потока и не имеет дискового файла, потому фабрике будет передано не имя сборника, а пустая строка
    fNoDiskFile: Boolean;
    fOpenedFilesCount: Integer;

    destructor Destroy (); override;
  end;

  TOwnedPartialStream = class (TSFSPartialStream)
  protected
    fOwner: TVolumeInfo;

  public
    constructor Create (pOwner: TVolumeInfo; pSrc: TStream; pPos, pSize: Int64; pKillSrc: Boolean);
    destructor Destroy (); override;
  end;


var
  factories: TObjectList; // TSFSVolumeFactory
  volumes: TObjectList;   // TVolumeInfo


// разбить имя файла на части: префикс файловой системы, имя файла данных,
// собственно имя файла
// имя выглядит как:
// (("sfspfx:")?"datafile::")*"filename"
procedure SplitFName (const fn: string; out dataFile, fileName: string);
var
  f: Integer;
begin
  f := Length(fn)-1;
  while f >= 1 do
  begin
    if (fn[f] = ':') and (fn[f+1] = ':') then break;
    Dec(f);
  end;
  if f < 1 then begin dataFile := ''; fileName := fn; end
  else
  begin
    dataFile := Copy(fn, 1, f-1);
    fileName := Copy(fn, f+2, maxInt-10000);
  end;
end;

// сайдэффект: вырезает виртуальное имя из dataFile.
function ExtractVirtName (var dataFile: string): string;
var
  f: Integer;
begin
  f := Length(dataFile); result := dataFile;
  while f > 1 do
  begin
    if dataFile[f] = ':' then break;
    if dataFile[f] = '|' then
    begin
      if dataFile[f-1] = '|' then begin Dec(f); Delete(dataFile, f, 1); end
      else
      begin
        result := Copy(dataFile, f+1, Length(dataFile));
        Delete(dataFile, f, Length(dataFile));
        break;
      end;
    end;
    Dec(f);
  end;
end;

// разбить имя сборника на части: префикс файловой системы, имя файла данных,
// виртуальное имя. если виртуального имени не дано, оно будет равно dataFile.
// имя выглядит как:
// [sfspfx:]datafile[|virtname]
// если перед двоеточием меньше трёх букв, то это считается не префиксом,
// а именем диска.
procedure SplitDataName (const fn: string; out pfx, dataFile, virtName: string);
var
  f: Integer;
begin
  f := Pos(':', fn);
  if f <= 3 then begin pfx := ''; dataFile := fn; end
  else
  begin
    pfx := Copy(fn, 1, f-1);
    dataFile := Copy(fn, f+1, maxInt-10000);
  end;
  virtName := ExtractVirtName(dataFile);
end;

// найти производителя для этого файла (если файл уже открыт).
// onlyPerm: только "постоянные" производители.
function FindVolumeInfo (const dataFileName: TSFSString; onlyPerm: Boolean=false): Integer;
var
  f: Integer;
  vi: TVolumeInfo;
begin
  f := 0;
  while f < volumes.Count do
  begin
    if volumes[f] <> nil then
    begin
      vi := TVolumeInfo(volumes[f]);
      if not onlyPerm or vi.fPermanent then
      begin
        if SFSStrEqu(vi.fPackName, dataFileName) then
        begin
          result := f;
          exit;
        end;
      end;
    end;
    Inc(f);
  end;
  result := -1;
end;

// найти инфу для этого тома.
// хорошее имя, правда? %-)
function FindVolumeInfoByVolumeInstance (vol: TSFSVolume): Integer;
begin
  result := volumes.Count-1;
  while result >= 0 do
  begin
    if volumes[result] <> nil then
    begin
      if TVolumeInfo(volumes[result]).fVolume = vol then exit;
    end;
    Dec(result);
  end;
end;

function le2upper (ch: Char): Char;
begin
  if ch < #128 then
  begin
    if (ch >= 'a') and (ch <= 'z') then Dec(ch, 32);
  end
  else
  begin
    if (ch >= #224) and (ch <= #255) then
    begin
      Dec(ch, 32);
    end
    else
    begin
      case ch of
        #184, #186, #191: Dec(ch, 16);
        #162, #179: Dec(ch);
      end;
    end;
  end;
  result := ch;
end;

function SFSStrEqu (const s0, s1: TSFSString): Boolean;
var
  i: Integer;
begin
  //result := (AnsiCompareText(s0, s1) == 0);
  result := false;
  if length(s0) <> length(s1) then exit;
  for i := 1 to length(s0) do
  begin
    if le2upper(s0[i]) <> le2upper(s1[i]) then exit;
  end;
  result := true;
end;

function SFSReplacePathDelims (const s: TSFSString; newDelim: TSFSChar): TSFSString;
var
  f: Integer;
begin
  result := s;
  for f := 1 to Length(result) do
  begin
    if (result[f] = '/') or (result[f] = '\') then
    begin
      // avoid unnecessary string changes
      if result[f] <> newDelim then result[f] := newDelim;
    end;
  end;
end;

function SFSGetLastVirtualName (const fn: TSFSString): string;
var
  rest, tmp: string;
  f: Integer;
begin
  rest := fn;
  repeat
    f := Pos('::', rest); if f = 0 then f := Length(rest)+1;
    tmp := Copy(rest, 1, f-1); Delete(rest, 1, f+1);
    result := ExtractVirtName(tmp);
  until rest = '';
end;


{ TVolumeInfo }
destructor TVolumeInfo.Destroy ();
var
  f, me: Integer;
  used: Boolean; // флажок заюзаности потока кем-то ещё
begin
  if fFactory <> nil then fFactory.Recycle(fVolume);
  if fVolume <> nil then used := (fVolume.fRC <> 0) else used := false;
  fVolume := nil;
  fFactory := nil;
  fPackName := '';

  // типа мусоросборник: если наш поток более никем не юзается, то угробить его нафиг
  if not used then
  begin
    me := volumes.IndexOf(self);
    f := volumes.Count-1;
    while not used and (f >= 0) do
    begin
      if (f <> me) and (volumes[f] <> nil) then
      begin
        used := (TVolumeInfo(volumes[f]).fStream = fStream);
        if not used then
        begin
          used := (TVolumeInfo(volumes[f]).fVolume.fFileStream = fStream);
        end;
        if used then break;
      end;
      Dec(f);
    end;
  end;
  if not used then FreeAndNil(fStream); // если больше никем не юзано, пришибём
  inherited Destroy();
end;


{ TOwnedPartialStream }
constructor TOwnedPartialStream.Create (pOwner: TVolumeInfo; pSrc: TStream;
  pPos, pSize: Int64; pKillSrc: Boolean);
begin
  inherited Create(pSrc, pPos, pSize, pKillSrc);
  fOwner := pOwner;
  if pOwner <> nil then Inc(pOwner.fOpenedFilesCount);
end;

destructor TOwnedPartialStream.Destroy ();
var
  f: Integer;
begin
  inherited Destroy();
  if fOwner <> nil then
  begin
    Dec(fOwner.fOpenedFilesCount);
    if not fOwner.fPermanent and (fOwner.fOpenedFilesCount < 1) then
    begin
      f := volumes.IndexOf(fOwner);
      if f <> -1 then volumes[f] := nil; // this will destroy the volume
    end;
  end;
end;


{ TSFSFileInfo }
constructor TSFSFileInfo.Create (pOwner: TSFSVolume);
begin
  inherited Create();
  fOwner := pOwner;
  fPath := '';
  fName := '';
  fSize := 0;
  fOfs := 0;
  if pOwner <> nil then pOwner.fFiles.Add(self);
end;

destructor TSFSFileInfo.Destroy ();
begin
  if fOwner <> nil then fOwner.fFiles.Extract(self);
  inherited Destroy();
end;


{ TSFSVolume }
constructor TSFSVolume.Create (const pFileName: TSFSString; pSt: TStream);
begin
  inherited Create();
  fRC := 0;
  fFileStream := pSt;
  fFileName := pFileName;
  fFiles := TObjectList.Create(true);
end;

procedure TSFSVolume.DoDirectoryRead ();
var
  fl: TStringList; //!!!FIXME! change to list of wide TSFSStrings or so!
  f, c, n: Integer;
  sfi: TSFSFileInfo;
  tmp, fn, ext: TSFSString;
begin
  fl := nil;
  fFileName := ExpandFileName(SFSReplacePathDelims(fFileName, '/'));
  try
    ReadDirectory();
    fFiles.Pack();

    // check for duplicate file names
    fl := TStringList.Create(); fl.Sorted := true;
    for f := 0 to fFiles.Count-1 do
    begin
      sfi := TSFSFileInfo(fFiles[f]);

      // normalize name & path
      sfi.fPath := SFSReplacePathDelims(sfi.fPath, '/');
      if (sfi.fPath <> '') and (sfi.fPath[1] = '/') then Delete(sfi.fPath, 1, 1);
      if (sfi.fPath <> '') and (sfi.fPath[Length(sfi.fPath)] <> '/') then sfi.fPath := sfi.fPath+'/';
      tmp := SFSReplacePathDelims(sfi.fName, '/');
      c := Length(tmp); while (c > 0) and (tmp[c] <> '/') do Dec(c);
      if c > 0 then
      begin
        // split path and name
        Delete(sfi.fName, 1, c); // cut name
        tmp := Copy(tmp, 1, c);  // get path
        if tmp = '/' then tmp := ''; // just delimiter; ignore it
        sfi.fPath := sfi.fPath+tmp;
      end;

      // check for duplicates
      if fl.Find(sfi.fPath+sfi.fName, c) then
      begin
        n := 0; tmp := sfi.fName;
        c := Length(tmp); while (c > 0) and (tmp[c] <> '.') do Dec(c);
        if c < 1 then c := Length(tmp)+1;
        fn := Copy(tmp, 1, c-1); ext := Copy(tmp, c, Length(tmp));
        repeat
          tmp := fn+'_'+IntToStr(n)+ext;
          if not fl.Find(sfi.fPath+tmp, c) then break;
          Inc(n);
        until false;
        sfi.fName := tmp;
      end;
      fl.Add(sfi.fName);
    end;
    fl.Free();
  except
    fl.Free();
    raise;
  end;
end;

destructor TSFSVolume.Destroy ();
begin
  Clear();
  FreeAndNil(fFiles);
  inherited Destroy();
end;

procedure TSFSVolume.Clear ();
begin
  fRC := 0; //FIXME
  fFiles.Clear();
end;

function TSFSVolume.FindFile (const fPath, fName: TSFSString): Integer;
begin
  if fFiles = nil then result := -1
  else
  begin
    result := fFiles.Count;
    while result > 0 do
    begin
      Dec(result);
      if fFiles[result] <> nil then
      begin
        if SFSStrEqu(fPath, TSFSFileInfo(fFiles[result]).fPath) and
           SFSStrEqu(fName, TSFSFileInfo(fFiles[result]).fName) then exit;
      end;
    end;
    result := -1;
  end;
end;

function TSFSVolume.GetFileCount (): Integer;
begin
  if fFiles = nil then result := 0 else result := fFiles.Count;
end;

function TSFSVolume.GetFiles (index: Integer): TSFSFileInfo;
begin
  if fFiles = nil then result := nil
  else
  begin
    if (index < 0) or (index >= fFiles.Count) then result := nil
    else result := TSFSFileInfo(fFiles[index]);
  end;
end;

function TSFSVolume.OpenFileEx (const fName: TSFSString): TStream;
var
  fp, fn: TSFSString;
  f, ls: Integer;
begin
  fp := fName;
  // normalize name, find split position
  if (fp <> '') and ((fp[1] = '/') or (fp[1] = '\')) then Delete(fp, 1, 1);
  ls := 0;
  for f := 1 to Length(fp) do
  begin
    if fp[f] = '\' then fp[f] := '/';
    if fp[f] = '/' then ls := f;
  end;
  fn := Copy(fp, ls+1, Length(fp));
  fp := Copy(fp, 1, ls);
  f := FindFile(fp, fn);
  if f = -1 then raise ESFSError.Create('file not found: "'+fName+'"');
  result := OpenFileByIndex(f);
  if result = nil then raise ESFSError.Create('file not found: "'+fName+'"');
end;


{ TSFSFileList }
constructor TSFSFileList.Create (const pVolume: TSFSVolume);
var
  f: Integer;
begin
  inherited Create();
  ASSERT(pVolume <> nil);
  f := FindVolumeInfoByVolumeInstance(pVolume);
  ASSERT(f <> -1);
  fVolume := pVolume;
  Inc(TVolumeInfo(volumes[f]).fOpenedFilesCount); // не позволим убить запись!
end;

destructor TSFSFileList.Destroy ();
var
  f: Integer;
begin
  f := FindVolumeInfoByVolumeInstance(fVolume);
  ASSERT(f <> -1);
  if fVolume <> nil then Dec(fVolume.fRC);
  Dec(TVolumeInfo(volumes[f]).fOpenedFilesCount);
  // убьём запись, если она временная, и в ней нет больше ничего открытого
  if not TVolumeInfo(volumes[f]).fPermanent and
     (TVolumeInfo(volumes[f]).fOpenedFilesCount < 1) then volumes[f] := nil;
  inherited Destroy();
end;

function TSFSFileList.GetCount (): Integer;
begin
  result := fVolume.fFiles.Count;
end;

function TSFSFileList.GetFiles (index: Integer): TSFSFileInfo;
begin
  if (index < 0) or (index >= fVolume.fFiles.Count) then result := nil
  else result := TSFSFileInfo(fVolume.fFiles[index]);
end;


procedure SFSRegisterVolumeFactory (factory: TSFSVolumeFactory);
var
  f: Integer;
begin
  if factory = nil then exit;
  if factories.IndexOf(factory) <> -1 then
    raise ESFSError.Create('duplicate factories are not allowed');
  f := factories.IndexOf(nil);
  if f = -1 then factories.Add(factory) else factories[f] := factory;
end;

procedure SFSUnregisterVolumeFactory (factory: TSFSVolumeFactory);
var
  f: Integer;
  c: Integer;
begin
  if factory = nil then exit;
  f := factories.IndexOf(factory);
  if f = -1 then raise ESFSError.Create('can''t unregister nonexisting factory');
  c := 0; while c < volumes.Count do
  begin
    if (volumes[c] <> nil) and (TVolumeInfo(volumes[c]).fFactory = factory) then volumes[c] := nil;
    Inc(c);
  end;
  factories[f] := nil;
end;


function SFSAddDataFileEx (dataFileName: TSFSString; ds: TStream; top, permanent: Integer): Integer;
// dataFileName может иметь префикс типа "zip:" (см. выше: IsMyPrefix).
// может выкинуть исключение!
// top:
//   <0: добавить в начало списка поиска.
//   =0: не менять.
//   >0: добавить в конец списка поиска.
// permanent:
//   <0: создать "временный" том.
//   =0: не менять флажок постоянства.
//   >0: создать "постоянный" том.
// если ds <> nil, то создаёт сборник из потока. если сборник с именем
// dataFileName уже зарегистрирован, то падает нафиг.
// возвращает индекс в volumes.
// умеет делать рекурсию.
var
  fac: TSFSVolumeFactory;
  vol: TSFSVolume;
  vi: TVolumeInfo;
  f: Integer;
  st, st1: TStream;
  pfx: TSFSString;
  fn, vfn, tmp: TSFSString;
begin
  f := Pos('::', dataFileName);
  if f <> 0 then
  begin
    // рекурсивное открытие.
    // разобьём dataFileName на имя сборника и остаток.
    // pfx будет именем сборника, dataFileName -- остатком.
    pfx := Copy(dataFileName, 1, f-1); Delete(dataFileName, 1, f+1);
    // сначала откроем первый список...
    result := SFSAddDataFileEx(pfx, ds, 0, 0);
    // ...теперь продолжим с остатком.
    // узнаем, какое файло открывать.
    // выковыряем первый "::" префикс (это будет имя файла).
    f := Pos('::', dataFileName); if f = 0 then f := Length(dataFileName)+1;
    fn := Copy(dataFileName, 1, f-1); Delete(dataFileName, 1, f-1);
    // dataFileName хранит остаток.
    // извлечём имя файла:
    SplitDataName(fn, pfx, tmp, vfn);
    // откроем этот файл
    vi := TVolumeInfo(volumes[result]); st := nil;
    try
      st := vi.fVolume.OpenFileEx(tmp);
      st1 := TOwnedPartialStream.Create(vi, st, 0, st.Size, true);
    except
      FreeAndNil(st);
      // удалим неиспользуемый временный том.
      if not vi.fPermanent and (vi.fOpenedFilesCount < 1) then volumes[result] := nil;
      raise;
    end;
    // ура. открыли файл. кидаем в воздух чепчики, продолжаем развлечение.
    fn := fn+dataFileName;
    try
      st1.Position := 0;
      result := SFSAddDataFileEx(fn, st1, top, permanent);
    except
      st1.Free(); // а вот не заладилось. закрыли открытое файло, вылетели.
      raise;
    end;
    exit;
  end;

  // обыкновенное нерекурсивное открытие.
  SplitDataName(dataFileName, pfx, fn, vfn);

  f := FindVolumeInfo(vfn);
  if f <> -1 then
  begin
    if ds <> nil then raise ESFSError.Create('subdata name conflict');
    if permanent <> 0 then TVolumeInfo(volumes[f]).fPermanent := (permanent > 0);
    if top = 0 then result := f
    else if top < 0 then result := 0
    else result := volumes.Count-1;
    if result <> f then volumes.Move(f, result);
    exit;
  end;

  if ds <> nil then st := ds
  else st := TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  st.Position := 0;

  volumes.Pack();

  fac := nil; vol := nil;
  try
    for f := 0 to factories.Count-1 do
    begin
      fac := TSFSVolumeFactory(factories[f]);
      if fac = nil then continue;
      if (pfx <> '') and not fac.IsMyVolumePrefix(pfx) then continue;
      st.Position := 0;
      try
        if ds <> nil then vol := fac.Produce(pfx, '', st)
        else vol := fac.Produce(pfx, fn, st);
      except
        vol := nil;
      end;
      if vol <> nil then break;
    end;
    if vol = nil then raise ESFSError.Create('no factory for "'+dataFileName+'"');
  except
    if st <> ds then st.Free();
    raise;
  end;

  vi := TVolumeInfo.Create();
  try
    if top < 0 then
    begin
      result := 0;
      volumes.Insert(0, vi);
    end
    else result := volumes.Add(vi);
  except
    vol.Free();
    if st <> ds then st.Free();
    vi.Free();
    raise;
  end;

  vi.fFactory := fac;
  vi.fVolume := vol;
  vi.fPackName := vfn;
  vi.fStream := st;
  vi.fPermanent := (permanent > 0);
  vi.fNoDiskFile := (ds <> nil);
  vi.fOpenedFilesCount := 0;
end;

function SFSAddSubDataFile (const virtualName: TSFSString; ds: TStream;
  top: Boolean = false): Boolean;
var
  tv: Integer;
begin
  ASSERT(ds <> nil);
  try
    if top then tv := -1 else tv := 1;
    SFSAddDataFileEx(virtualName, ds, tv, 0);
    result := true;
  except
    result := false;
  end;
end;

function SFSAddDataFile (const dataFileName: TSFSString; top: Boolean = false): Boolean;
var
  tv: Integer;
begin
  try
    if top then tv := -1 else tv := 1;
    SFSAddDataFileEx(dataFileName, nil, tv, 1);
    result := true;
  except
    result := false;
  end;
end;


function SFSExpandDirName (const s: TSFSString): TSFSString;
var
  f, e: Integer;
  es: TSFSString;
begin
  f := 1; result := s;
  while f < Length(result) do
  begin
    while (f < Length(result)) and (result[f] <> '<') do Inc(f);
    if f >= Length(result) then exit;
    e := f; while (e < Length(result)) and (result[e] <> '>') do Inc(e);
    es := Copy(result, f, e+1-f);

    if es = '<currentdir>' then es := GetCurrentDir
    else if es = '<exedir>' then es := ExtractFilePath(ParamStr(0))
    else es := '';

    if es <> '' then
    begin
      if (es[Length(es)] <> '/') and (es[Length(es)] <> '\') then es := es+'/';
      Delete(result, f, e+1-f);
      Insert(es, result, f);
      Inc(f, Length(es));
    end
    else f := e+1;
  end;
end;

function SFSFileOpenEx (const fName: TSFSString): TStream;
var
  dataFileName, fn: TSFSString;
  f: Integer;
  vi: TVolumeInfo;
  diskChecked: Boolean;
  ps: TStream;

  function CheckDisk (): TStream;
  // проверим, есть ли фало fn где-то на дисках.
  var
    dfn, dirs, cdir: TSFSString;
    f: Integer;
  begin
    result := nil;
    if diskChecked or not sfsDiskEnabled then exit;
    diskChecked := true;
    dfn := SFSReplacePathDelims(fn, '/');
    dirs := sfsDiskDirs; if dirs = '' then dirs := '<currentdir>';
    while dirs <> '' do
    begin
      f := 1; while (f <= Length(dirs)) and (dirs[f] <> '|') do Inc(f);
      cdir := Copy(dirs, 1, f-1); Delete(dirs, 1, f);
      if cdir = '' then continue;
      cdir := SFSReplacePathDelims(SFSExpandDirName(cdir), '/');
      if cdir[Length(cdir)] <> '/' then cdir := cdir+'/';
      try
        result := TFileStream.Create(cdir+dfn, fmOpenRead or fmShareDenyWrite);
        exit;
      except
      end;
    end;
  end;

begin
  SplitFName(fName, dataFileName, fn);
  if fn = '' then raise ESFSError.Create('invalid file name: "'+fName+'"');

  diskChecked := false;

  if dataFileName <> '' then
  begin
    // префиксованый файл
    if sfsForceDiskForPrefixed then
    begin
      result := CheckDisk();
      if result <> nil then exit;
    end;

    f := SFSAddDataFileEx(dataFileName, nil, 0, 0);
    vi := TVolumeInfo(volumes[f]);

    try
      result := vi.fVolume.OpenFileEx(fn);
      ps := TOwnedPartialStream.Create(vi, result, 0, result.Size, true);
    except
      result.Free();
      if not vi.fPermanent and (vi.fOpenedFilesCount < 1) then volumes[f] := nil;
      result := CheckDisk(); // облом с datafile, проверим диск
      if result = nil then raise ESFSError.Create('file not found: "'+fName+'"');
      exit;
    end;
    //Inc(vi.fOpenedFilesCount);
    result := ps;
    exit;
  end;

  // непрефиксованый файл
  if sfsDiskFirst then
  begin
    result := CheckDisk();
    if result <> nil then exit;
  end;
  // ищем по всем перманентным префиксам
  f := 0;
  while f < volumes.Count do
  begin
    vi := TVolumeInfo(volumes[f]);
    if (vi <> nil) and vi.fPermanent then
    begin
      if vi.fVolume <> nil then
      begin
        result := vi.fVolume.OpenFileEx(fn);
        if result <> nil then
        begin
          try
            ps := TOwnedPartialStream.Create(vi, result, 0, result.Size, true);
            result := ps;
            //Inc(vi.fOpenedFilesCount);
          except
            FreeAndNil(result);
          end;
        end;
        if result <> nil then exit;
      end;
    end;
    Inc(f);
  end;
  result := CheckDisk();
  if result = nil then raise ESFSError.Create('file not found: "'+fName+'"');
end;

function SFSFileOpen (const fName: TSFSString): TStream;
begin
  try
    result := SFSFileOpenEx(fName);
  except
    result := nil;
  end;
end;

function SFSFileList (const dataFileName: TSFSString): TSFSFileList;
var
  f: Integer;
  vi: TVolumeInfo;
begin
  result := nil;
  if dataFileName = '' then exit;

  try
    f := SFSAddDataFileEx(dataFileName, nil, 0, 0);
  except
    exit;
  end;
  vi := TVolumeInfo(volumes[f]);

  try
    result := TSFSFileList.Create(vi.fVolume);
    Inc(vi.fVolume.fRC);
  except
    if not vi.fPermanent and (vi.fOpenedFilesCount < 1) then volumes[f] := nil;
  end;
end;


initialization
  factories := TObjectList.Create(true);
  volumes := TObjectList.Create(true);
finalization
  //volumes.Free(); // it fails for some reason... Runtime 217 (^C hit). wtf?!
  //factories.Free(); // not need to be done actually...
end.
