{$INCLUDE ../shared/a_modes.inc}
{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils, Classes,
  xstreams in '../shared/xstreams.pas',
  xparser in '../shared/xparser.pas',
  xdynrec in '../shared/xdynrec.pas',
  xprofiler in '../shared/xprofiler.pas',
  utils in '../shared/utils.pas',
  hashtable in '../shared/hashtable.pas',
  conbuf in '../shared/conbuf.pas',
  e_log in '../engine/e_log.pas';


// ////////////////////////////////////////////////////////////////////////// //
var
  pr: TTextParser;
  dfmapdef: TDynMapDef;
  fo, fohlp, foimpl: TextFile;
  st: TStream = nil;
  ch: AnsiChar;
  wdt: Integer;
  s: AnsiString;
  tidx, nidx, fidx: Integer;
  needComma: Boolean;
  trec: TDynRecord;
  fld: TDynField;
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

  AssignFile(fohlp, 'mapdef_help.inc');
  Rewrite(fohlp);

  AssignFile(foimpl, 'mapdef_impl.inc');
  Rewrite(foimpl);

  write(fo, '// *** WARNING! ***'#10);
  write(fo, '//   regenerate this part directly from "mapdef.txt" with ''mapgen'', NEVER manually change anything here!'#10#10#10);
  write(fo, dfmapdef.pasdefconst);

  write(fohlp, '// *** WARNING! ***'#10);
  write(fohlp, '//   regenerate this part directly from "mapdef.txt" with ''mapgen'', NEVER manually change anything here!'#10#10);

  // generate trigger helpers
{
function TDynRecordHelper.trigTargetPoint (): TDFPoint; inline; begin result := getPointField('target'); end;
function TDynRecordHelper.trigD2DTeleport (): Boolean; inline; begin result := (getFieldWithType('d2d', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigSilentTeleport (): Boolean; inline; begin result := (getFieldWithType('silent', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigTlpDir (): Byte; inline; begin result := Byte(getFieldWithType('direction', TDynField.TType.TUByte).ival); end;
}

  write(foimpl, #10#10'// ////////////////////////////////////////////////////////////////////////// //'#10);
  write(foimpl, '// trigger helpers'#10);
  for tidx := 0 to dfmapdef.trigTypeCount-1 do
  begin
    // header comment
    write(foimpl, #10'// ');
    needComma := false;
    trec := dfmapdef.trigType[tidx];
    for nidx := 0 to trec.forTrigCount-1 do
    begin
      if needComma then write(foimpl, ', ') else needComma := true;
      write(foimpl, trec.forTrigAt[nidx]);
    end;
    write(foimpl, #10);
    // fields
    for fidx := 0 to trec.count-1 do
    begin
      fld := trec.fieldAt[fidx];
      if fld.internal then continue;
      if (fld.binOfs < 0) then continue;
      // HACK!
      if (fld.name = 'panelid') or (fld.name = 'monsterid') then
      begin
        writeln('skipping ', fld.pasname, ' <', fld.name, '>');
        continue;
      end;
      if (fld.baseType <> TDynField.TType.TPoint) and (fld.baseType <> TDynField.TType.TSize) then
      begin
        write(foimpl, 'function TDynRecordHelper.trig', fld.pasname, ' (): ');
        write(fohlp, 'function trig', fld.pasname, ' (): ');
      end;
      case fld.baseType of
        TDynField.TType.TBool:
          begin
            write(fohlp, 'Boolean; inline;'#10);
            write(foimpl, 'Boolean; inline; begin result := (getFieldWithType(''', fld.name, ''', TDynField.TType.TBool).ival ');
            if fld.negbool then write(foimpl, '=') else write(foimpl, '<>');
            write(foimpl, ' 0); end;'#10);
          end;
        TDynField.TType.TChar:
          begin
            write(fohlp, 'AnsiString; inline;'#10);
            write(foimpl, 'AnsiString; inline; begin result := utf2win(getFieldWithType(''', fld.name, ''', TDynField.TType.TChar).sval); end;'#10);
          end;
        TDynField.TType.TByte:
          begin
            write(fohlp, 'SmallInt; inline;'#10);
            write(foimpl, 'SmallInt; inline; begin result := ShortInt(getFieldWithType(''', fld.name, ''', TDynField.TType.TByte).ival); end;'#10);
          end;
        TDynField.TType.TUByte:
          begin
            write(fohlp, 'Byte; inline;'#10);
            write(foimpl, 'Byte; inline; begin result := Byte(getFieldWithType(''', fld.name, ''', TDynField.TType.TUByte).ival); end;'#10);
          end;
        TDynField.TType.TShort:
          begin
            write(fohlp, 'ShortInt; inline;'#10);
            write(foimpl, 'ShortInt; inline; begin result := SmallInt(getFieldWithType(''', fld.name, ''', TDynField.TType.TShort).ival); end;'#10);
          end;
        TDynField.TType.TUShort:
          begin
            write(fohlp, 'Word; inline;'#10);
            write(foimpl, 'Word; inline; begin result := Word(getFieldWithType(''', fld.name, ''', TDynField.TType.TUShort).ival); end;'#10);
          end;
        TDynField.TType.TInt:
          begin
            write(fohlp, 'LongInt; inline;'#10);
            write(foimpl, 'LongInt; inline; begin result := LongInt(getFieldWithType(''', fld.name, ''', TDynField.TType.TInt).ival); end;'#10);
          end;
        TDynField.TType.TUInt:
          begin
            write(fohlp, 'LongWord; inline;'#10);
            write(foimpl, 'LongWord; inline; begin result := LongWord(getFieldWithType(''', fld.name, ''', TDynField.TType.TUInt).ival); end;'#10);
          end;
        TDynField.TType.TString:
          begin
            write(fohlp, 'AnsiString; inline;'#10);
            write(foimpl, 'AnsiString; inline; begin result := utf2win(getFieldWithType(''', fld.name, ''', TDynField.TType.TChar).sval); end;'#10);
          end;
        TDynField.TType.TPoint:
          begin
            if fld.hasTPrefix or fld.separatePasFields then
            begin
              write(fohlp, 'function trig'); if fld.hasTPrefix then write(fohlp, 'T'); write(fohlp, 'X (): LongInt; inline;'#10);
              write(fohlp, 'function trig'); if fld.hasTPrefix then write(fohlp, 'T'); write(fohlp, 'Y (): LongInt; inline;'#10);
              // [T]X
              write(foimpl, 'function TDynRecordHelper.trig');
              if fld.hasTPrefix then write(foimpl, 'T');
              write(foimpl, 'X (): LongInt; inline; begin result := LongInt(getFieldWithType(''', fld.name, ''', TDynField.TType.TPoint).ival); end;'#10);
              // [T]Y
              write(foimpl, 'function TDynRecordHelper.trig');
              if fld.hasTPrefix then write(foimpl, 'T');
              write(foimpl, 'Y (): LongInt; inline; begin result := LongInt(getFieldWithType(''', fld.name, ''', TDynField.TType.TPoint).ival2); end;'#10);
            end
            else
            begin
              write(fohlp, 'function trig', fld.pasname, ' (): TDFPoint; inline;'#10);
              write(foimpl, 'function TDynRecordHelper.trig', fld.pasname, ' (): TDFPoint; inline; begin result := getPointField(''', fld.name, '''); end;'#10);
            end;
          end;
        TDynField.TType.TSize:
          begin
            if fld.hasTPrefix or fld.separatePasFields then
            begin
              write(fohlp, 'function trig'); if fld.hasTPrefix then write(fohlp, 'T'); write(fohlp, 'Width (): Word; inline;'#10);
              write(fohlp, 'function trig'); if fld.hasTPrefix then write(fohlp, 'T'); write(fohlp, 'Height (): Word; inline;'#10);
              // [T]X
              write(foimpl, 'function TDynRecordHelper.trig');
              if fld.hasTPrefix then write(foimpl, 'T');
              write(foimpl, 'Width (): Word; inline; begin result := Word(getFieldWithType(''', fld.name, ''', TDynField.TType.TSize).ival); end;'#10);
              // [T]Y
              write(foimpl, 'function TDynRecordHelper.trig');
              if fld.hasTPrefix then write(foimpl, 'T');
              write(foimpl, 'Height (): Word; inline; begin result := Word(getFieldWithType(''', fld.name, ''', TDynField.TType.TSize).ival2); end;'#10);
            end
            else
            begin
              raise Exception.Create('no non-separate sizes in triggers, pelase');
            end;
          end;
        TDynField.TType.TList:
          raise Exception.Create('no lists in triggers, pelase');
        TDynField.TType.TTrigData:
          raise Exception.Create('no triggers in triggers, pelase');
      end;
    end;
  end;


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
  CloseFile(fohlp);
  CloseFile(foimpl);
end.
