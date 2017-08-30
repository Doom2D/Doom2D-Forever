{$INCLUDE a_modes.inc}
{$M+}

uses
  SysUtils, Classes,
  xparser in 'xparser.pas',
  xdynrec in 'xdynrec.pas',
  utils in 'utils.pas';


// ////////////////////////////////////////////////////////////////////////// //
var
  pr: TTextParser;
  dfmapdef: TDynMapDef;
  fo: TextFile;
  st: TStream;
  ch: AnsiChar;
  wdt: Integer;
  s: AnsiString;
begin
  writeln('parsing "mapdef.txt"...');
  pr := TFileTextParser.Create('mapdef.txt');
  try
    dfmapdef := TDynMapDef.Create(pr);
  except on e: Exception do
    begin
      writeln('ERROR at (', pr.line, ',', pr.col, '): ', e.message);
      Halt(1);
    end;
  end;

  writeln('writing "mapdef.inc"...');
  AssignFile(fo, 'mapdef.inc');
  Rewrite(fo);
  write(fo, '// *** WARNING! ***'#10);
  write(fo, '//   regenerate this part directly from "mapdef.txt" with ''zmapgen'', NEVER manually change anything here!'#10#10#10);
  write(fo, dfmapdef.pasdef);

  st := openDiskFileRO('mapdef.txt');
  write(fo, #10#10'const defaultMapDef: AnsiString = ''''+'#10'  ');
  wdt := 2;
  while true do
  begin
    if (st.Read(ch, 1) <> 1) then break;
    s := formatstrf('#%d', [Byte(ch)]);
    if (wdt+Length(s) > 78) then begin wdt := 2; write(fo, '+'#10'  '); end;
    write(fo, s);
    Inc(wdt, Length(s));
  end;
  write(fo, #10';');

  CloseFile(fo);
end.
