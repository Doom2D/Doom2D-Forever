{$IFDEF WIN32}
  {$APPTYPE CONSOLE}
{$ENDIF}
{$MODE DELPHI}
program __wadcvt__;

uses
  SysUtils,
  Classes,
  SDL2 in '../lib/sdl2/sdl2.pas',
  sfs,
  sfsPlainFS,
  sfsZipFS,
  sfsMemFS,
  zipper;


type
  TProg = class(TObject)
    lastname: string;
    lastlen: Integer;

    procedure putStr (const s: string; newline: Boolean=false);

    procedure onProgress (sender: TObject; const percent: double);
    procedure onFileStart (sender: TObject; const fileName: string);
    procedure onFileEnd (sender: TObject; const ratio: double);
  end;


procedure TProg.putStr (const s: string; newline: Boolean=false);
begin
  write(#13, s);
  while lastlen > length(s) do
  begin
    write(' ');
    Dec(lastlen);
  end;
  if newline then
  begin
    writeln;
    lastlen := 0;
  end
  else
  begin
    lastlen := length(s);
  end;
end;

procedure TProg.onProgress (sender: TObject; const percent: double);
var
  prc: Integer;
begin
  prc := trunc(percent*100.0);
  putStr(Format('compressing %-33s  %3d%%', [lastname, prc]));
end;

procedure TProg.onFileStart (sender: TObject; const fileName: string);
begin
  lastname := fileName;
  putStr(Format('compressing %-33s  %3d%%', [lastname, 0]));
end;

procedure TProg.onFileEnd (sender: TObject; const ratio: double);
begin
  putStr(Format('compressed  %-33s  %f', [lastname, ratio]), true);
end;


// returns new file name
function detectExt (fpath, fname: string; fs: TStream): string;
var
  buf: PChar;
  buflen: Integer;
  f: Integer;
  st: string[24];
begin
  result := fname;
  if length(ExtractFileExt(fname)) <> 0 then exit;
  if fs.size < 16 then exit;
  buflen := Integer(fs.size);
  GetMem(buf, buflen);
  try
    fs.ReadBuffer(buf^, buflen);
    // xm
    Move(buf^, (PChar(@st[1]))^, 16);
    st[0] := #16;
    if (st = 'Extended Module:') then
    begin
      result := result+'.xm';
      exit;
    end;
    if (buf[0] = 'D') and (buf[1] = 'F') and (buf[2] = 'W') and
       (buf[3] = 'A') and (buf[4] = 'D') and (buf[5] = #$1) then
    begin
      result := result+'.wad';
      exit;
    end;
    if (buf[0] = 'M') and (buf[1] = 'A') and (buf[2] = 'P') and (buf[3] = #$1) then
    begin
      result := result+'.dfmap';
      exit;
    end;
    if (buf[0] = 'M') and (buf[1] = 'T') and (buf[2] = 'h') and (buf[3] = 'd') then
    begin
      result := result+'.mid';
      exit;
    end;
    if (buf[0] = 'R') and (buf[1] = 'I') and (buf[2] = 'F') and (buf[3] = 'F') and
       (buf[8] = 'W') and (buf[9] = 'A') and (buf[10] = 'V') and (buf[11] = 'E') then
    begin
      result := result+'.wav';
      exit;
    end;
    // mp3 (stupid hack)
    for f := 0 to 128-6 do
    begin
      if (buf[f+0] = #$4) and (buf[f+1] = 'L') and
         (buf[f+2] = 'A') and (buf[f+3] = 'M') and
         (buf[f+4] = 'E') and (buf[f+5] = '3') then
      begin
        result := result+'.mp3';
        exit;
      end;
    end;
    // more mp3 hacks
    if (buf[0] = 'I') and (buf[1] = 'D') and (buf[2] = '3') and (buf[3] <= #4) then
    begin
      result := result+'.mp3';
      exit;
    end;
    if buflen > 128 then
    begin
      if (buf[buflen-128] = 'T') and (buf[buflen-127] = 'A') and (buf[buflen-126] = 'G') then
      begin
        result := result+'.mp3';
        exit;
      end;
    end;
    // targa (stupid hack; this "signature" is not required by specs)
    if buflen >= 18 then
    begin
      Move((buf+buflen-18)^, (PChar(@st[1]))^, 16);
      st[0] := #16;
      if st = 'TRUEVISION-XFILE' then
      begin
        result := result+'.tga';
        exit;
      end;
    end;
  finally
    FreeMem(buf);
  end;
end;


var
  fs: TStream;
  fl: TSFSFileList;
  f: Integer;
  infname: string;
  outfname: string;
  zip: TZipper;
  dvfn: string;
  ZEntries: TZipFileEntries;
  newname: string;
  prg: TProg;
begin
  if ParamCount() < 1 then
  begin
    WriteLn('usage: wadcvt file.wad');
    Halt(1);
  end;

  infname := ParamStr(1);
  if not SFSStrEqu(ExtractFileExt(infname), '.wad') and not SFSStrEqu(ExtractFileExt(infname), '.dfwad') then
  begin
    writeln('wtf?!');
    Halt(1);
  end;

  if ParamCount() > 1 then
  begin
    outfname := ParamStr(2);
  end
  else
  begin
    outfname := ChangeFileExt(infname, '.pk3');
  end;

  if not SFSAddDataFile(infname) then begin WriteLn('shit!'); Halt(1); end;
  dvfn := SFSGetLastVirtualName(infname);

  {
  tot := 0;
  fl := SFSFileList(ParamStr(1));
  if fl <> nil then
  begin
    for f := 0 to fl.Count-1 do
    begin
      WriteLn(f:4, ': ', fl[f].fSize:10, ' "', fl[f].fPath, fl[f].fName, '"');
      Inc(tot, fl[f].fSize);
    end;
    WriteLn('===================================================');
    WriteLn(fl.Count, ' files; ', Int64ToStrComma(tot), ' bytes.');
    fl.Free();
  end;
  }

  zip := TZipper.Create;
  zip.Filename := outfname;

  fl := SFSFileList(dvfn);
  if fl <> nil then
  begin
    ZEntries := TZipFileEntries.Create(TZipFileEntry);
    for f := 0 to fl.Count-1 do
    begin
      if length(fl[f].fName) = 0 then continue;
      fs := SFSFileOpen(dvfn+'::'+fl[f].fPath+fl[f].fName);
      newname := detectExt(fl[f].fPath, fl[f].fName, fs);
      fs.Free;
      fs := SFSFileOpen(dvfn+'::'+fl[f].fPath+fl[f].fName);
      writeln('[', f+1, '/', fl.Count, ']: ', fl[f].fPath+newname, '  ', fs.size);
      ZEntries.AddFileEntry(fs, fl[f].fPath+newname);
    end;
    try
      if ZEntries.Count > 0 then
      begin
        writeln('creating ''', outfname, '''');
        prg := TProg.Create();
        zip.OnProgress := prg.onProgress;
        zip.OnStartFile := prg.onFileStart;
        zip.OnEndFile := prg.onFileEnd;
        zip.ZipFiles(ZEntries);
        prg.Free;
      end;
    except
      on E: EZipError do E.CreateFmt('Zipfile could not be created%sReason: %s', [LineEnding, E.Message])
    end;
  end
  else
  begin
    writeln('SFSFileList(): faled!');
  end;
end.
