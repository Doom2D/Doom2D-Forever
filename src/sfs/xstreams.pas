// special stream classes
{$MODE DELPHI}
{.$R-}
unit xstreams;

interface

uses
  SysUtils, Classes, SDL2;


type
  // поток-обёртка для SDL_RWops
  TSFSSDLStream = class(TStream)
  protected
    fRW: PSDL_RWops;      // SDL-ная прокладка
    fFreeSource: Boolean; // убивать исходник при помирании?

  public
    constructor Create (aSrc: PSDL_RWops; aFreeSource: Boolean=true);
    destructor Destroy (); override;

    function Read (var buffer; count: LongInt): LongInt; override;
    function Write (const buffer; count: LongInt): LongInt; override;
    function Seek (const offset: Int64; origin: TSeekOrigin): Int64; override;
  end;

  // read-only поток для извлечения из исходного только кусочка
  TSFSPartialStream = class(TStream)
  protected
    fSource: TStream;     // исходный поток
    fKillSource: Boolean; // убивать исходник при помирании?
    fLastReadPos: Int64;  // последний Read() остановился здесь (относ. fStartPos)
    fCurrentPos: Int64;   // последний Seek() остановился здесь (относ. fStartPos)
    fStartPos: Int64;     // начало кусочка
    fSize: Int64;         // длина кусочка
    fPreBuf: packed array of Byte; // этот буфер будет перед файлом

    procedure CheckPos ();

  public
    // aSrc: поток-исходник.
    // aPos: начальная позиция в потоке. -1 -- с текущей.
    //       если aPos < текущей позиции, то исходный поток должен
    //       нормально поддерживать Seek()!
    // aSize: количество байтиков, которое можно прочесть из потока.
    //        если меньше нуля -- то до конца.
    // aKillSrc: убивать ли исходный поток, когда сами умираем?
    // также может пришпандорить к началу файла буфер. bufSz будет добавлено к
    // длине файла.
    constructor Create (aSrc: TStream; aPos, aSize: Int64; aKillSrc: Boolean; preBuf: Pointer=nil; bufSz: Integer=0);
    destructor Destroy (); override;

    // нормализует count и читает.
    function Read (var buffer; count: LongInt): LongInt; override;
    // Write() просто громко падает.
    function Write (const buffer; count: LongInt): LongInt; override;
    // Seek() реализовано, чтобы могла работать пропертя Size.
    // вообще-то можно перекрыть метод GetSize(), но вдруг какой
    // больной на голову кодер будет получать размер при помощи
    // Seek()'а?
    function Seek (const offset: Int64; origin: TSeekOrigin): Int64; override;
  end;

  TSFSGuardStream = class(TStream)
  protected
    fSource: TStream;        // исходный поток
    fGuardedStream: TStream; // поток, который завалим при помирании
    fKillSource: Boolean;    // убивать исходник при помирании?
    fKillGuarded: Boolean;   // убивать охраняемый при помирании?
    fGuardedFirst: Boolean;  // при смерти первым пришибаем охраняемого?

  public
    // aSrc: поток-исходник (на который замапены операции чтения/записи).
    // aKillSrc: убивать ли исходный поток, когда сами умираем?
    // aKillGuarded: убивать ли охраняемый поток, когда сами умираем?
    // aGuardedFirst: true: при смерти первым пришибаем охраняемого.
    constructor Create (aSrc, aGuarded: TStream; aKillSrc, aKillGuarded: Boolean; aGuardedFirst: Boolean=true);
    destructor Destroy (); override;

    // нижеследующее замаплено на fSource
    function Read (var buffer; count: LongInt): LongInt; override;
    function Write (const buffer; count: LongInt): LongInt; override;
    function Seek (const offset: Int64; origin: TSeekOrigin): Int64; override;
  end;

  TSFSMemoryStreamRO = class(TCustomMemoryStream)
  public
    constructor Create (pMem: Pointer; pSize: Integer);

    function Write (const buffer; count: LongInt): LongInt; override;
  end;


implementation

uses
  sfs; // for ESFSError

{ TSFSSDLStream }
constructor TSFSSDLStream.Create (aSrc: PSDL_RWops; aFreeSource: Boolean=true);
begin
  inherited Create();
  //ASSERT(aSrc <> nil);
  fRW := aSrc;
  fFreeSource := aFreeSource;
end;

destructor TSFSSDLStream.Destroy ();
begin
  if fFreeSource and (fRW <> nil) then SDL_FreeRW(fRW);
  inherited Destroy();
end;

function TSFSSDLStream.Read (var buffer; count: LongInt): LongInt;
begin
  if (fRW = nil) or (count <= 0) then begin result := 0; exit; end;
  result := SDL_RWread(fRW, @buffer, 1, count);
end;

function TSFSSDLStream.Write (const buffer; count: LongInt): LongInt;
begin
  if (fRW = nil) or (count <= 0) then begin result := 0; exit; end;
  result := SDL_RWwrite(fRW, @buffer, 1, count);
end;

function TSFSSDLStream.Seek (const offset: Int64; origin: TSeekOrigin): Int64;
var
  ss: Integer;
begin
  if fRW = nil then begin result := 0; exit; end;
  case origin of
    soBeginning: ss := RW_SEEK_SET;
    soCurrent: ss := RW_SEEK_CUR;
    soEnd: ss := RW_SEEK_END;
    else raise ESFSError.Create('invalid Seek() call');
    // других не бывает. а у кого бывает, тому я не доктор.
  end;
  result := SDL_RWseek(fRW, offset, ss);
  if result = -1 then raise ESFSError.Create('Seek() error');
end;


{ TSFSPartialStream }
constructor TSFSPartialStream.Create (aSrc: TStream; aPos, aSize: Int64; aKillSrc: Boolean; preBuf: Pointer=nil; bufSz: Integer=0);
begin
  inherited Create();
  ASSERT(aSrc <> nil);
  if aPos < 0 then aPos := aSrc.Position;
  if aSize < 0 then aSize := 0;
  fSource := aSrc;
  fKillSource := aKillSrc;
  fLastReadPos := 0;
  fCurrentPos := 0;
  fStartPos := aPos;
  fSize := aSize;
  if bufSz > 0 then
  begin
    SetLength(fPreBuf, bufSz);
    Move(preBuf^, fPreBuf[0], bufSz);
    Inc(fSize, bufSz);
  end
  else
  begin
    fPreBuf := nil;
  end;
end;

destructor TSFSPartialStream.Destroy ();
begin
  if fKillSource then FreeAndNil(fSource);
  inherited Destroy();
end;

procedure TSFSPartialStream.CheckPos ();
begin
  {
  if fSource.Position <> fStartPos+fCurrentPos-Length(fPreBuf) then
  begin
    fSource.Position := fStartPos+fCurrentPos-Length(fPreBuf);
  end;
  }
  if fCurrentPos >= length(fPreBuf) then
  begin
    //writeln('seeking at ', fCurrentPos, ' (real: ', fStartPos+fCurrentPos-Length(fPreBuf), ')');
    fSource.Position := fStartPos+fCurrentPos-Length(fPreBuf);
  end;
  fLastReadPos := fCurrentPos;
end;

function TSFSPartialStream.Write (const buffer; count: LongInt): LongInt;
begin
  result := 0;
  raise ESFSError.Create('can''t write to read-only stream');
  // а не ходи, нехороший, в наш садик гулять!
end;

function TSFSPartialStream.Read (var buffer; count: LongInt): LongInt;
var
  left: Int64;
  pc: Pointer;
  rd: LongInt;
begin
  if count < 0 then raise ESFSError.Create('invalid Read() call'); // сказочный долбоёб...
  if count = 0 then begin result := 0; exit; end;
  pc := @buffer;
  result := 0;
  if (Length(fPreBuf) > 0) and (fCurrentPos < Length(fPreBuf)) then
  begin
    fLastReadPos := fCurrentPos;
    left := Length(fPreBuf)-fCurrentPos;
    if left > count then left := count;
    if left > 0 then
    begin
      Move(fPreBuf[fCurrentPos], pc^, left);
      Inc(PChar(pc), left);
      Inc(fCurrentPos, left);
      fLastReadPos := fCurrentPos;
      Dec(count, left);
      result := left;
      if count = 0 then exit;
    end;
  end;
  CheckPos();
  left := fSize-fCurrentPos;
  if left < count then count := left; // и так случается...
  if count > 0 then
  begin
    rd := fSource.Read(pc^, count);
    Inc(result, rd);
    Inc(fCurrentPos, rd);
    fLastReadPos := fCurrentPos;
  end
  else
  begin
    result := 0;
  end;
end;

function TSFSPartialStream.Seek (const offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    soBeginning: result := offset;
    soCurrent: result := offset+fCurrentPos;
    soEnd: result := fSize+offset;
    else raise ESFSError.Create('invalid Seek() call');
    // других не бывает. а у кого бывает, тому я не доктор.
  end;
  if result < 0 then result := 0
  else if result > fSize then result := fSize;
  fCurrentPos := result;
end;


{ TSFSGuardStream }
constructor TSFSGuardStream.Create (aSrc, aGuarded: TStream; aKillSrc, aKillGuarded: Boolean; aGuardedFirst: Boolean=true);
begin
  inherited Create();
  fSource := aSrc; fGuardedStream := aGuarded;
  fKillSource := aKillSrc; fKillGuarded := aKillGuarded;
  fGuardedFirst := aGuardedFirst;
end;

destructor TSFSGuardStream.Destroy ();
begin
  if fKillGuarded and fGuardedFirst then FreeAndNil(fGuardedStream);
  if fKillSource then FreeAndNil(fSource);
  if fKillGuarded and not fGuardedFirst then FreeAndNil(fGuardedStream);
  inherited Destroy();
end;

function TSFSGuardStream.Read (var buffer; count: LongInt): LongInt;
begin
  result := fSource.Read(buffer, count);
end;

function TSFSGuardStream.Write (const buffer; count: LongInt): LongInt;
begin
  result := fSource.Write(buffer, count);
end;

function TSFSGuardStream.Seek (const offset: Int64; origin: TSeekOrigin): Int64;
begin
  result := fSource.Seek(offset, origin);
end;


{ TSFSMemoryStreamRO }
constructor TSFSMemoryStreamRO.Create (pMem: Pointer; pSize: Integer);
begin
  inherited Create();
  SetPointer(pMem, pSize);
  Position := 0;
end;

function TSFSMemoryStreamRO.Write (const buffer; count: LongInt): LongInt;
begin
  result := 0;
  raise ESFSError.Create('can''t write to read-only stream');
  // совсем сбрендил...
end;


end.
