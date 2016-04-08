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


var
  fs: TStream;
  fl: TSFSFileList;
  f: Integer;
  infname: string;
  outfname: string;
  zip: TZipper;
  dvfn: string;
  ZEntries: TZipFileEntries;
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
      writeln('[', f+1, '/', fl.Count, ']: ', fl[f].fPath+fl[f].fName, '  ', fs.size);
      ZEntries.AddFileEntry(fs, fl[f].fPath+fl[f].fName);
    end;
    try
      if ZEntries.Count > 0 then
      begin
        writeln('creating ''', outfname, '''');
        zip.ZipFiles(ZEntries);
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
