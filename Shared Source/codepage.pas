unit codepage;

interface

implementation

uses windows;

function ConOutFunc(var Text: TTextRec): Integer;
var
  a: LongWord;
  _pos: Integer;
begin
 Result := 0;

 _pos := Text.BufPos;

 if _pos <= 0 then Exit;

 Text.BufPos := 0;
 CharToOemBuff(Text.BufPtr, Text.BufPtr, _pos);
 if not WriteFile(Text.Handle, Text.BufPtr^, _pos, a, nil) then
  Result := GetLastError;
end;

initialization

Rewrite(Output);
TTextRec(Output).InOutFunc := @ConOutFunc;
TTextRec(Output).FlushFunc := @ConOutFunc;

end.
