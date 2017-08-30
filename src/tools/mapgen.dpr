{$INCLUDE ../shared/a_modes.inc}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  xstreams in '../shared/xstreams.pas',
  xparser in '../shared/xparser.pas',
  xdynrec in '../shared/xdynrec.pas',
  utils in '../shared/utils.pas',
  conbuf in '../shared/conbuf.pas',
  e_log in '../engine/e_log.pas';


// ////////////////////////////////////////////////////////////////////////// //
var
  pr: TTextParser;
  dfmapdef: TDynMapDef;
  fo: TextFile;
  st: TStream = nil;
  ch: AnsiChar;
  wdt: Integer;
  s: AnsiString;
begin
  //writeln(getFilenamePath(ParamStr(0)), '|');

  e_InitWritelnDriver();
  conbufDumpToStdOut := true;
  conbufConPrefix := false;

  writeln('parsing "mapdef.txt"...');
  try
    st := openDiskFileRO('mapdef.txt');
    writeln('found: local mapdef');
  except // sorry
    st := nil;
  end;
  try
    writeln(filenameConcat(getFilenamePath(ParamStr(0)), '../mapdef/mapdef.txt'), '|');
    st := openDiskFileRO(filenameConcat(getFilenamePath(ParamStr(0)), '../mapdef/mapdef.txt'));
    writeln('found: system mapdef');
  except // sorry
    writeln('FATAL: mapdef not found!');
  end;

  pr := TFileTextParser.Create(st, false); // don't own
  try
    dfmapdef := TDynMapDef.Create(pr);
  except on e: Exception do
    begin
      writeln('ERROR at (', pr.line, ',', pr.col, '): ', e.message);
      Halt(1);
    end;
  end;
  pr.Free();

  writeln('writing "mapdef.inc"...');
  AssignFile(fo, 'mapdef.inc');
  Rewrite(fo);
  write(fo, '// *** WARNING! ***'#10);
  write(fo, '//   regenerate this part directly from "mapdef.txt" with ''mapgen'', NEVER manually change anything here!'#10#10#10);
  write(fo, dfmapdef.pasdef);

  //st := openDiskFileRO('mapdef.txt');
  st.position := 0;
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
