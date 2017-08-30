{$INCLUDE ../shared/a_modes.inc}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  sfs in '../sfs/sfs.pas',
  sfsPlainFS in '../sfs/sfsPlainFS.pas',
  sfsZipFS in '../sfs/sfsZipFS.pas',
  xstreams in '../shared/xstreams.pas',
  xparser in '../shared/xparser.pas',
  xdynrec in '../shared/xdynrec.pas',
  xprofiler in '../shared/xprofiler.pas',
  utils in '../shared/utils.pas',
  conbuf in '../shared/conbuf.pas',
  e_log in '../engine/e_log.pas',
  wadreader in '../shared/wadreader.pas',
  MAPDEF in '../shared/MAPDEF.pas';


// ////////////////////////////////////////////////////////////////////////// //
var
  pr: TTextParser;
  dfmapdef: TDynMapDef;
  wr: TTextWriter;
  map: TDynRecord;
  st: TStream;
  stt: UInt64;
  inname: AnsiString = '';
  outname: AnsiString = '';
  totext: Integer = -1; // <0: guess; force outname extension
  sign: packed array[0..3] of AnsiChar;
  wad: TWADFile = nil;
  waddata: Pointer;
  waddlen: Integer;
begin
  if (ParamCount = 0) then
  begin
    writeln('usage: mapcvt inname outname');
    Halt(1);
  end;

  inname := ParamStr(1);
  //writeln('inname: [', inname, ']');
  if (ParamCount = 1) then
  begin
    outname := forceFilenameExt(ParamStr(1), '');
    if isWadPath(outname) then
    begin
      outname := SFSReplacePathDelims(g_ExtractFilePathName(outname), '/');
      if (Length(outname) = 0) then begin writeln('FATAL: can''t guess output name!'); Halt(1); end;
    end;
  end
  else
  begin
    outname := ParamStr(2);
         if StrEquCI1251(getFilenameExt(outname), '.txt') then totext := 1
    else if StrEquCI1251(getFilenameExt(outname), '.map') then totext := 0
    else if StrEquCI1251(getFilenameExt(outname), '.dfmap') then totext := 0
    else if (Length(getFilenameExt(outname)) = 0) then totext := -1
    else begin writeln('FATAL: can''t guess output format!'); Halt(1); end;
  end;
  //writeln('outname: [', outname, ']; totext=', totext);

  e_InitWritelnDriver();
  conbufDumpToStdOut := true;
  conbufConPrefix := false;

  writeln('parsing "mapdef.txt"...');
  //pr := TFileTextParser.Create('mapdef.txt');
  pr := TStrTextParser.Create(defaultMapDef);
  try
    dfmapdef := TDynMapDef.Create(pr);
  except on e: Exception do
    begin
      writeln('ERROR at (', pr.line, ',', pr.col, '): ', e.message);
      Halt(1);
    end;
  end;

  writeln('parsing "', inname, '"...');

  if isWadPath(inname) then
  begin
    wad := TWADFile.Create();
    wad.ReadFile(g_ExtractWadName(inname));
    wad.GetMapResource(g_ExtractFilePathName(inname), waddata, waddlen);
    st := TSFSMemoryChunkStream.Create(waddata, waddlen, true);
    wad.Free();
  end
  else
  begin
    st := openDiskFileRO(inname);
  end;

  st.ReadBuffer(sign, 4);
  st.position := 0;
  if (sign[0] = 'M') and (sign[1] = 'A') and (sign[2] = 'P') and (sign[3] = #1) then
  begin
    // binary map
    if (totext < 0) then begin outname := forceFilenameExt(outname, '.txt'); totext := 1; end;
    stt := curTimeMicro();
    map := dfmapdef.parseBinMap(st);
    stt := curTimeMicro()-stt;
    writeln('binary map parsed in ', stt div 1000, '.', stt mod 1000, ' milliseconds');
    st.Free();
  end
  else
  begin
    // text map
    if (totext < 0) then begin outname := forceFilenameExt(outname, '.map'); totext := 0; end;
    pr := TFileTextParser.Create(st);
    try
      stt := curTimeMicro();
      map := dfmapdef.parseMap(pr);
      stt := curTimeMicro()-stt;
      writeln('text map parsed in ', stt div 1000, '.', stt mod 1000, ' milliseconds');
    except on e: Exception do
      begin
        writeln('ERROR at (', pr.line, ',', pr.col, '): ', e.message);
        Halt(1);
      end;
    end;
    pr.Free();
  end;

  assert(totext >= 0);

  writeln('writing "', outname, '"...');
  st := createDiskFile(outname);
  if (totext = 0) then
  begin
    // write binary map
    stt := curTimeMicro();
    map.writeBinTo(st);
    stt := curTimeMicro()-stt;
    writeln('binary map written in ', stt div 1000, '.', stt mod 1000, ' milliseconds');
  end
  else
  begin
    // write text map
    wr := TFileTextWriter.Create(st);
    stt := curTimeMicro();
    map.writeTo(wr);
    stt := curTimeMicro()-stt;
    writeln('text map written in ', stt div 1000, '.', stt mod 1000, ' milliseconds');
    wr.Free();
  end;
end.
