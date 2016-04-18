// special stream classes
{$MODE OBJFPC}
{$R+}
unit xstreams_sdl;

interface

uses
  SysUtils, Classes, xstreams;


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


implementation


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
    else raise XStreamError.Create('invalid Seek() call');
    // других не бывает. а у кого бывает, тому я не доктор.
  end;
  result := SDL_RWseek(fRW, offset, ss);
  if result = -1 then raise XStreamError.Create('Seek() error');
end;


end.
