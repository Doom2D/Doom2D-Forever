// special stream classes
{$MODE DELPHI}
{$R+}
unit xstreams;

interface

uses
  SysUtils, Classes,
  zbase{z_stream};


type
  XStreamError = class(Exception);

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

  // this stream can kill both `proxied` and `guarded` streams on closing
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
  private
    fFreeMem: Boolean;
    fMem: Pointer;

  public
    constructor Create (pMem: Pointer; pSize: Integer; aFreeMem: Boolean=false);
    destructor Destroy (); override;

    function Write (const buffer; count: LongInt): LongInt; override;
  end;

  TUnZStream = class(TStream)
  protected
    fSrcSt: TStream;
    fZlibSt: z_stream;
    fBuffer: PByte;
    fPos: Int64;
    fSkipHeader: Boolean;
    fSize: Int64; // can be -1
    fSrcStPos: Int64;
    fSkipToPos: Int64; // >0: skip to this position
    fKillSrc: Boolean;

    procedure reset ();
    function readBuf (var buffer; count: LongInt): LongInt;
    procedure fixPos ();
    procedure determineSize ();

  public
    // `aSize` can be -1 if stream size is unknown
    constructor create (asrc: TStream; aSize: Int64; aKillSrc: Boolean; aSkipHeader: boolean=false);
    destructor destroy (); override;
    function read (var buffer; count: LongInt): LongInt; override;
    function write (const buffer; count: LongInt): LongInt; override;
    function seek (const offset: Int64; origin: TSeekOrigin): Int64; override;
  end;


implementation

uses
  zinflate;


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
  raise XStreamError.Create('can''t write to read-only stream');
  // а не ходи, нехороший, в наш садик гулять!
end;

function TSFSPartialStream.Read (var buffer; count: LongInt): LongInt;
var
  left: Int64;
  pc: Pointer;
  rd: LongInt;
begin
  if count < 0 then raise XStreamError.Create('invalid Read() call'); // сказочный долбоёб...
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
    else raise XStreamError.Create('invalid Seek() call');
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
constructor TSFSMemoryStreamRO.Create (pMem: Pointer; pSize: Integer; aFreeMem: Boolean=false);
begin
  fFreeMem := aFreeMem;
  fMem := pMem;
  inherited Create();
  SetPointer(pMem, pSize);
  Position := 0;
end;

destructor TSFSMemoryStreamRO.Destroy ();
begin
  if fFreeMem and (fMem <> nil) then FreeMem(fMem);
end;

function TSFSMemoryStreamRO.Write (const buffer; count: LongInt): LongInt;
begin
  result := 0;
  raise XStreamError.Create('can''t write to read-only stream');
  // совсем сбрендил...
end;


// ////////////////////////////////////////////////////////////////////////// //
{ TUnZStream }
const ZBufSize = 32768; // size of the buffer used for temporarily storing data from the child stream


constructor TUnZStream.create (asrc: TStream; aSize: Int64; aKillSrc: Boolean; aSkipHeader: boolean=false);
var
  err: Integer;
begin
  fKillSrc := aKillSrc;
  fPos := 0;
  fSkipToPos := -1;
  fSrcSt := asrc;
  fSize := aSize;
  GetMem(fBuffer, ZBufSize);
  fSkipHeader := aSkipHeader;
  if fSkipHeader then err := inflateInit2(fZlibSt, -MAX_WBITS) else err := inflateInit(fZlibSt);
  if err <> Z_OK then raise XStreamError.Create(zerror(err));
  fSrcStPos := fSrcSt.position;
end;


destructor TUnZStream.destroy ();
begin
  inflateEnd(fZlibSt);
  FreeMem(fBuffer);
  if fKillSrc then fSrcSt.Free();
  inherited Destroy();
end;


function TUnZStream.readBuf (var buffer; count: LongInt): LongInt;
var
  err: Integer;
  sz: LongInt;
begin
  result := 0;
  if count > 0 then
  begin
    fZlibSt.next_out := @buffer;
    fZlibSt.avail_out := count;
    sz := fZlibSt.avail_out;
    while fZlibSt.avail_out > 0 do
    begin
      if fZlibSt.avail_in = 0 then
      begin
        // refill the buffer
        fZlibSt.next_in := fBuffer;
        fZlibSt.avail_in := fSrcSt.read(Fbuffer^, ZBufSize);
      end;
      err := inflate(fZlibSt, Z_NO_FLUSH);
      if (err <> Z_OK) and (err <> Z_STREAM_END) then raise XStreamError.Create(zerror(err));
      Inc(result, sz-fZlibSt.avail_out);
      Inc(fPos, sz-fZlibSt.avail_out);
      sz := fZlibSt.avail_out;
      if err = Z_STREAM_END then begin fSize := fPos; break; end;
    end;
  end;
end;


procedure TUnZStream.fixPos ();
var
  buf: array [0..4095] of Byte;
  rd, rr: LongInt;
begin
  if fSkipToPos < 0 then exit;
  //writeln('fixing pos: fPos=', fPos, '; fSkipToPos=', fSkipToPos);
  if fSkipToPos < fPos then reset();
  while fPos < fSkipToPos do
  begin
    if fSkipToPos-fPos > 4096 then rd := 4096 else rd := LongInt(fSkipToPos-fPos);
    //writeln('  reading ', rd, ' bytes...');
    rr := readBuf(buf, rd);
    //writeln('  got ', rr, ' bytes; fPos=', fPos, '; fSkipToPos=', fSkipToPos);
    if rd <> rr then raise XStreamError.Create('seek error');
  end;
  //writeln('  pos: fPos=', fPos, '; fSkipToPos=', fSkipToPos);
  fSkipToPos := -1;
end;


procedure TUnZStream.determineSize ();
var
  buf: array [0..4095] of Byte;
  rd: LongInt;
  opos: Int64;
begin
  if fSize >= 0 then exit;
  opos := fPos;
  try
    //writeln('determining unzstream size...');
    while true do
    begin
      rd := readBuf(buf, 4096);
      if rd <> 4096 then break;
    end;
    fSize := fPos;
    //writeln('  unzstream size is ', fSize);
  finally
    fSkipToPos := opos;
  end;
end;


function TUnZStream.read (var buffer; count: LongInt): LongInt;
begin
  if fSkipToPos >= 0 then fixPos();
  result := readBuf(buffer, count);
end;


function TUnZStream.write (const buffer; count: LongInt): LongInt;
begin
  result := 0;
  raise XStreamError.Create('can''t write to read-only stream');
end;


procedure TUnZStream.reset ();
var
  err: Integer;
begin
  fSrcSt.position := fSrcStPos;
  fPos := 0;
  inflateEnd(fZlibSt);
  if fSkipHeader then err := inflateInit2(fZlibSt, -MAX_WBITS) else err := inflateInit(fZlibSt);
  if err <> Z_OK then raise XStreamError.Create(zerror(err));
end;


function TUnZStream.Seek (const offset: Int64; origin: TSeekOrigin): Int64;
var
  cpos: Int64;
begin
  cpos := fPos;
  if fSkipToPos >= 0 then cpos := fSkipToPos;
  case origin of
    soBeginning: result := offset;
    soCurrent: result := offset+cpos;
    soEnd: begin determineSize(); result := fSize+offset; end;
    else raise XStreamError.Create('invalid Seek() call');
    // других не бывает. а у кого бывает, тому я не доктор.
  end;
  if result < 0 then result := 0;
  fSkipToPos := result;
  //writeln('seek: ofs=', offset, '; origin=', origin, '; result=', result);
end;


end.
