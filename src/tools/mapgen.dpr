{$INCLUDE ../shared/a_modes.inc}
{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils, Classes,
  {$IFDEF USE_SDL}
    SDL in '../lib/sdl/sdl.pas',
  {$ENDIF}
  {$IFDEF USE_SDL2}
    SDL2 in '../lib/sdl2/sdl2.pas',
  {$ENDIF}
  mempool in '../shared/mempool.pas',
  xstreams in '../shared/xstreams.pas',
  xparser in '../shared/xparser.pas',
  xdynrec in '../shared/xdynrec.pas',
  xprofiler in '../shared/xprofiler.pas',
  utils in '../shared/utils.pas',
  hashtable in '../shared/hashtable.pas',
  conbuf in '../shared/conbuf.pas',
  e_log in '../engine/e_log.pas';


// ////////////////////////////////////////////////////////////////////////// //
type
  THashStrFld = specialize THashBase<AnsiString, TDynField, THashKeyStr>;


// ////////////////////////////////////////////////////////////////////////// //
var
  dfmapdef: TDynMapDef;


// ////////////////////////////////////////////////////////////////////////// //
procedure genTrigCacheVars (const fname: AnsiString);
var
  fo: TextFile;
  tidx, fidx, nidx: Integer;
  trec: TDynRecord;
  fld: TDynField;
  palias: AnsiString;
  fldknown: THashStrFld = nil; // key: palias; value: prev field
begin
  AssignFile(fo, fname);
  {$I+}Rewrite(fo);{$I-}

  fldknown := THashStrFld.Create();

  write(fo, '// trigger cache'#10);
  for tidx := 0 to dfmapdef.trigTypeCount-1 do
  begin
    // header comment
    write(fo, #10);
    trec := dfmapdef.trigTypeAt[tidx];
    for nidx := 0 to trec.forTrigCount-1 do
    begin
      write(fo, '//', trec.forTrigAt[nidx], #10);
    end;
    // fields
    for fidx := 0 to trec.count-1 do
    begin
      fld := trec.fieldAt[fidx];
      if fld.internal then continue;
      // HACK!
      if (fld.name = 'panelid') or (fld.name = 'monsterid') then
      begin
        //writeln('skipping <', fld.name, '>');
        continue;
      end;
      palias := fld.palias(true);
      // don't write duplicate fields
      if fldknown.has(toLowerCase1251(palias)) then continue;
      fldknown.put(toLowerCase1251(palias), fld);
      // write field definition
      case fld.baseType of
        TDynField.TType.TBool: write(fo, 'tgc', palias, ': Boolean;'#10);
        TDynField.TType.TChar: write(fo, 'tgc', palias, ': AnsiString;'#10);
        TDynField.TType.TByte: write(fo, 'tgc', palias, ': SmallInt;'#10);
        TDynField.TType.TUByte: write(fo, 'tgc', palias, ': Byte;'#10);
        TDynField.TType.TShort: write(fo, 'tgc', palias, ': ShortInt;'#10);
        TDynField.TType.TUShort: write(fo, 'tgc', palias, ': Word;'#10);
        TDynField.TType.TInt: write(fo, 'tgc', palias, ': LongInt;'#10);
        TDynField.TType.TUInt: write(fo, 'tgc', palias, ': LongWord;'#10);
        TDynField.TType.TString: write(fo, 'tgc', palias, ': AnsiString;'#10);
        TDynField.TType.TPoint:
          begin
            if fld.hasTPrefix then
            begin
              write(fo, 'tgcTX: LongInt;'#10);
              write(fo, 'tgcTY: LongInt;'#10);
            end
            else if fld.separatePasFields then
            begin
              write(fo, 'tgcX: LongInt;'#10);
              write(fo, 'tgcY: LongInt;'#10);
            end
            else
            begin
              write(fo, 'tgc', palias, ': TDFPoint;'#10);
            end;
          end;
        TDynField.TType.TSize:
          begin
            if fld.hasTPrefix then
            begin
              write(fo, 'tgcTWidth: LongInt;'#10);
              write(fo, 'tgcTHeight: LongInt;'#10);
            end
            else if fld.separatePasFields then
            begin
              write(fo, 'tgcWidth: LongInt;'#10);
              write(fo, 'tgcHeight: LongInt;'#10);
            end
            else
            begin
              write(fo, 'tgc', palias, ': TDFSize;'#10);
            end;
          end;
        TDynField.TType.TList:
          raise Exception.Create('no lists in triggers, pelase');
        TDynField.TType.TTrigData:
          raise Exception.Create('no triggers in triggers, pelase');
      end;
    end;
  end;

  CloseFile(fo);
  fldknown.Free();
end;


// ////////////////////////////////////////////////////////////////////////// //
procedure genTrigLoadCache (const fname: AnsiString);
var
  fo: TextFile;
  tidx, fidx, nidx: Integer;
  trec: TDynRecord;
  fld: TDynField;
  palias: AnsiString;
  needComma: Boolean;
begin
  AssignFile(fo, fname);
  {$I+}Rewrite(fo);{$I-}

  write(fo, '// trigger cache loader'#10);
  write(fo, '// set `TriggerType` in `tgt` before calling this'#10);
  write(fo, 'procedure trigUpdateCacheData (var tgt: TTrigger; tdata: TDynRecord);'#10);
  write(fo, 'begin'#10);
  write(fo, '  case tgt.TriggerType of'#10);
  for tidx := 0 to dfmapdef.trigTypeCount-1 do
  begin
    // case switch
    needComma := false;
    write(fo, '    ');
    trec := dfmapdef.trigTypeAt[tidx];
    for nidx := 0 to trec.forTrigCount-1 do
    begin
      if needComma then write(fo, ','#10'    ') else needComma := true;
      write(fo, trec.forTrigAt[nidx]);
    end;
    write(fo, ':'#10);
    write(fo, '      begin'#10);
    // fields
    for fidx := 0 to trec.count-1 do
    begin
      fld := trec.fieldAt[fidx];
      if fld.internal then continue;
      // HACK!
      if (fld.name = 'panelid') or (fld.name = 'monsterid') then
      begin
        //writeln('skipping <', fld.name, '>');
        continue;
      end;
      palias := fld.palias(true);
      // write field definition
      case fld.baseType of
        TDynField.TType.TBool,
        TDynField.TType.TChar,
        TDynField.TType.TByte,
        TDynField.TType.TUByte,
        TDynField.TType.TShort,
        TDynField.TType.TUShort,
        TDynField.TType.TInt,
        TDynField.TType.TUInt,
        TDynField.TType.TString:
          write(fo, '        tgt.tgc', palias, ' := tdata.trig', palias, ';'#10);
        TDynField.TType.TPoint:
          begin
            if fld.hasTPrefix then
            begin
              write(fo, '        tgt.tgcTX := tdata.trigTX;'#10);
              write(fo, '        tgt.tgcTY := tdata.trigTY;'#10);
            end
            else if fld.separatePasFields then
            begin
              write(fo, '        tgt.tgcX := tdata.trigX;'#10);
              write(fo, '        tgt.tgcY := tdata.trigY;'#10);
            end
            else
            begin
              write(fo, '        tgt.tgc', palias, ' := tdata.trig', palias, ';'#10);
            end;
          end;
        TDynField.TType.TSize:
          begin
            if fld.hasTPrefix then
            begin
              write(fo, '        tgt.tgcTWidth := tdata.trigTWidth;'#10);
              write(fo, '        tgt.tgcTHeight := tdata.trigTHeight;'#10);
            end
            else if fld.separatePasFields then
            begin
              write(fo, '        tgt.tgcWidth := tdata.trigWidth;'#10);
              write(fo, '        tgt.tgcHeight := tdata.trigHeight;'#10);
            end
            else
            begin
              write(fo, '        tgt.tgc', palias, ' := tdata.trig', palias, ';'#10);
            end;
          end;
        TDynField.TType.TList:
          raise Exception.Create('no lists in triggers, pelase');
        TDynField.TType.TTrigData:
          raise Exception.Create('no triggers in triggers, pelase');
      end;
    end;
    write(fo, '      end;'#10);
  end;
  write(fo, '  end;'#10);
  write(fo, 'end;'#10);

  CloseFile(fo);
end;


// ////////////////////////////////////////////////////////////////////////// //
var
  pr: TTextParser;
  fo, fohlp, foimpl: TextFile;
  st: TStream = nil;
  ch: AnsiChar;
  wdt: Integer;
  s: AnsiString;
  tidx, nidx, fidx: Integer;
  needComma: Boolean;
  trec: TDynRecord;
  fld: TDynField;
  palias: AnsiString;
  fldknown: THashStrFld = nil; // key: palias; value: prev field
  knownfld: TDynField;
begin
  fldknown := THashStrFld.Create();
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

  writeln('parsing "mapdef.txt"...');
  pr := TFileTextParser.Create(st, false); // don't own
  try
    dfmapdef := TDynMapDef.Create(pr);
  except
    on e: TDynParseException do
      begin
        writeln('ERROR at (', e.tokLine, ',', e.tokCol, '): ', e.message);
        Halt(1);
      end;
    on e: Exception do
      begin
        writeln('ERROR: ', e.message);
        Halt(1);
      end;
  end;
  pr.Free();

  writeln('writing "mapdef.inc"...');
  AssignFile(fo, 'mapdef.inc');
  {$I+}Rewrite(fo);{$I-}

  AssignFile(fohlp, 'mapdef_help.inc');
  {$I+}Rewrite(fohlp);{$I-}

  AssignFile(foimpl, 'mapdef_impl.inc');
  {$I+}Rewrite(foimpl);{$I-}

  write(fo, '// *** WARNING! ***'#10);
  write(fo, '//   regenerate this part directly from "mapdef.txt" with ''mapgen'', NEVER manually change anything here!'#10#10#10);
  write(fo, dfmapdef.pasdefconst);

  write(fohlp, '// *** WARNING! ***'#10);
  write(fohlp, '//   regenerate this part directly from "mapdef.txt" with ''mapgen'', NEVER manually change anything here!'#10#10);

  // generate trigger helpers
  write(foimpl, #10#10'// ////////////////////////////////////////////////////////////////////////// //'#10);
  write(foimpl, '// trigger helpers'#10);
  for tidx := 0 to dfmapdef.trigTypeCount-1 do
  begin
    // header comment
    write(foimpl, #10'// ');
    write(fohlp, #10'// ');
    needComma := false;
    trec := dfmapdef.trigTypeAt[tidx];
    for nidx := 0 to trec.forTrigCount-1 do
    begin
      if needComma then write(fohlp, ', ');
      if needComma then write(foimpl, ', ') else needComma := true;
      write(fohlp, trec.forTrigAt[nidx]);
      write(foimpl, trec.forTrigAt[nidx]);
    end;
    write(foimpl, #10);
    write(fohlp, #10);
    // fields
    for fidx := 0 to trec.count-1 do
    begin
      fld := trec.fieldAt[fidx];
      if fld.internal then continue;
      //if (fld.binOfs < 0) then continue;
      // HACK!
      if (fld.name = 'panelid') or (fld.name = 'monsterid') then
      begin
        writeln('skipping <', fld.name, '>');
        continue;
      end;
      palias := fld.palias(true);
      // check for known aliases
      //writeln('<', palias, '> : <', toLowerCase1251(palias), '>');
      knownfld := nil;
      if fldknown.get(toLowerCase1251(palias), knownfld) then
      begin
        if (fld.name <> knownfld.name) then raise Exception.Create(formatstrf('field ''%s'' of record ''%s'' conflicts with other field ''%s''', [fld.name, trec.typeName, knownfld.name]));
        if (fld.baseType <> knownfld.baseType) then raise Exception.Create(formatstrf('field ''%s'' of record ''%s'' conflicts with other field ''%s'' by type', [fld.name, trec.typeName, knownfld.name]));
        writeln('skipped duplicate field ''', fld.name, '''');
        continue;
      end;
      fldknown.put(toLowerCase1251(palias), fld);
      // write it
      if (fld.baseType <> TDynField.TType.TPoint) and (fld.baseType <> TDynField.TType.TSize) then
      begin
        write(foimpl, 'function TDynRecordHelper.trig', palias, ' (): ');
        write(fohlp, 'function trig', palias, ' (): ');
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
            write(foimpl, 'AnsiString; inline; begin result := utf2win(getFieldWithType(''', fld.name, ''', TDynField.TType.TString).sval); end;'#10);
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
              write(fohlp, 'function trig', palias, ' (): TDFPoint; inline;'#10);
              write(foimpl, 'function TDynRecordHelper.trig', palias, ' (): TDFPoint; inline; begin result := getPointField(''', fld.name, '''); end;'#10);
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
              //raise Exception.Create('no non-separate sizes in triggers, pelase');
              write(fohlp, 'function trig', palias, ' (): TDFSize; inline;'#10);
              write(foimpl, 'function TDynRecordHelper.trig', palias, ' (): TDFSize; inline; begin result := getSizeField(''', fld.name, '''); end;'#10);
            end;
          end;
        TDynField.TType.TList:
          raise Exception.Create('no lists in triggers, pelase');
        TDynField.TType.TTrigData:
          raise Exception.Create('no triggers in triggers, pelase');
      end;
    end;
  end;

  genTrigCacheVars('mapdef_tgc_def.inc');
  genTrigLoadCache('mapdef_tgc_impl.inc');

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
