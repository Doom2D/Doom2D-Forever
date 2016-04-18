// Streaming R/O Virtual File System v0.2.0
// Copyright (C) XL A.S. Ketmar.  All rights reserved
// See the file aplicense.txt for conditions of use.
//
// simple grouping files w/o packing:
//   wad, doom       : DooM .WAD (IWAD, PWAD)
//   pak, quake      : Quake I/II .PAK (PACK)
//   grp, duke3d     : Duke3D .GRP (KenSilverman)
//   spe, spec, abuse: Abuse .SPE (SPEC1.0)
//   wad2            : Quake .WAD (WAD2)
//   allegro         : DOS Allegro (slh.ALL.; ALL.)
//   dune2 pak       : alas, no signature %-(
//   M.A.X. res      : RES0
//   sin             : SiN .SIN (SPAK)
//
{.$DEFINE SFS_PLAIN_FS_ALTERNATIVE_SPEC}
  // define this and the first byte of each file in .SPE will contain
  // file type.
  // undefine this and file type will be directory name.
{.$DEFINE SFS_PLAIN_FS_DEBUG_ALLEGRO}
{.$DEFINE SFS_PLAINFS_FULL}
{$MODE DELPHI}
{.$R-}
unit sfsPlainFS;

interface

uses
  SysUtils, Classes, Contnrs, sfs;


type
  TSFSPlainVolumeType =
    (sfspvNone,
     sfspvPAK,
     sfspvSIN
     {$IFDEF SFS_PLAINFS_FULL}
     ,sfspvWAD,
     sfspvGRP,
     sfspvSPE,
     sfspvWAD2,
     sfspvALL,
     sfspvDune2,
     sfspvMAX
     {$ENDIF}
     );

  TSFSPlainVolume = class (TSFSVolume)
  protected
    fType: TSFSPlainVolumeType;

    procedure PAKReadDirectory ();
    procedure SINReadDirectory ();
    {$IFDEF SFS_PLAINFS_FULL}
    procedure WADReadDirectory ();
    procedure GRPReadDirectory ();
    procedure SPEReadDirectory ();
    procedure WAD2ReadDirectory ();
    procedure ALLReadDirectory ();
    procedure Dune2ReadDirectory ();
    procedure MAXReadDirectory ();
    {$ENDIF}

    procedure ReadDirectory (); override;

  public
    function OpenFileByIndex (const index: Integer): TStream; override;
  end;

  TSFSPlainVolumeFactory = class (TSFSVolumeFactory)
  public
    function IsMyVolumePrefix (const prefix: TSFSString): Boolean; override;
    function Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume; override;
    procedure Recycle (vol: TSFSVolume); override;
  end;



implementation

uses
  xstreams, utils;


type
  TSFSExtFileInfo = class (TSFSFileInfo)
  public
    fVBuf: packed array of Byte;
    fLink: TSFSString;
  end;

{$IFDEF SFS_PLAINFS_FULL}
  TAllegroProperty = class
    name: TSFSString;
    ofs: Int64;
    size: Integer;
  end;
{$ENDIF}


function ReadMD (st: TStream): Integer;
// read dword in big-endian format. portable.
var
  buf: packed array [0..3] of Byte;
begin
  st.ReadBuffer(buf[0], 4);
  result := (buf[0] shl 24) or (buf[1] shl 16) or (buf[2] shl 8) or buf[3];
end;

{$IFDEF SFS_PLAINFS_FULL}
function WADCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
  fcnt, dofs: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(fcnt, 4); st.ReadBuffer(dofs, 4);
  st.Seek(-12, soCurrent);
  if (sign <> 'IWAD') and (sign <> 'PWAD') then exit;
  if (dofs < 0) or (dofs > st.Size) or (fcnt < 0) or
     (dofs+fcnt*16 > st.Size) then exit;
  result := true;
end;
{$ENDIF}

function PAKCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
  dsize, dofs: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(dofs, 4); st.ReadBuffer(dsize, 4);
  st.Seek(-12, soCurrent);
  if sign <> 'PACK' then exit;
  if (dsize < 0) or (dofs < 0) or (dofs > st.Size) or (dofs+dsize > st.Size) or
     (dsize mod 64 <> 0) then exit;
  result := true;
end;

function SINCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
  dsize, dofs: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(dofs, 4); st.ReadBuffer(dsize, 4);
  st.Seek(-12, soCurrent);
  if sign <> 'SPAK' then exit;
  if (dsize < 0) or (dofs < 0) or (dofs > st.Size) or (dofs+dsize > st.Size) or
     (dsize mod 64 <> 0) then exit;
  result := true;
end;

{$IFDEF SFS_PLAINFS_FULL}
function GRPCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..11] of Char;
  fcnt: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 12);
  st.ReadBuffer(fcnt, 4);
  st.Seek(-16, soCurrent);
  if sign <> 'KenSilverman' then exit;
  if (fcnt < 0) or (fcnt*16 > st.Size-16) then exit;
  result := true;
end;

function SPECheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..6] of Char;
  b: Byte;
  fcnt: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 7); st.ReadBuffer(b, 1);
  st.ReadBuffer(fcnt, 4);
  st.Seek(-12, soCurrent);
  if (sign <> 'SPEC1.0') or (b <> 0) or (fcnt < 0) then exit;
  result := true;
end;

function WAD2CheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
  fcnt, dofs: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(fcnt, 4); st.ReadBuffer(dofs, 4);
  st.Seek(-12, soCurrent);
  if sign <> 'WAD2' then exit;
  if (dofs < 0) or (dofs > st.Size) or (fcnt < 0) or
     (dofs+fcnt*32 > st.Size) then exit;
  result := true;
end;

function ALLCheckMagic (st: TStream): Boolean;
var
  sign0, sign1: packed array [0..3] of Char;
begin
  result := false;
  st.ReadBuffer(sign0[0], 4);
  st.ReadBuffer(sign1[0], 4);
  st.Seek(-8, soCurrent);
  if sign0 = 'slh.' then
  begin
    if sign1 <> 'ALL.' then exit;
  end else if sign0 <> 'ALL.' then exit;
  result := true;
end;

function Dune2CheckMagic (st: TStream): Boolean;
var
  cpos, np, f: Integer;
begin
  cpos := st.Position;
  st.ReadBuffer(np, 4);
  st.Position := np-4;
  st.ReadBuffer(f, 4);
  st.Position := cpos;
  result := (f = 0);
end;

function MAXCheckMagic (st: TStream): Boolean;
var
  sign: packed array [0..3] of Char;
  fcnt, dofs: Integer;
begin
  result := false;
  st.ReadBuffer(sign[0], 4);
  st.ReadBuffer(dofs, 4); st.ReadBuffer(fcnt, 4);
  st.Seek(-12, soCurrent);
  if sign <> 'RES0' then exit;
  if (dofs < 0) or (dofs > st.Size) or (fcnt < 0) or
     (dofs+fcnt > st.Size) then exit;
  result := true;
end;
{$ENDIF}


{ TSFSPlainVolume }
{$IFDEF SFS_PLAINFS_FULL}
procedure TSFSPlainVolume.WADReadDirectory ();
var
  fcnt: LongWord;
  dofs: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..9] of Char;
begin
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(fcnt, 4);
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.Position := dofs;
  while fcnt <> 0 do
  begin
    fi := TSFSFileInfo.Create(self);
    fFileStream.ReadBuffer(fi.fOfs, 4);
    fFileStream.ReadBuffer(fi.fSize, 4);
    FillChar(name[0], Length(name), 0);
    fFileStream.ReadBuffer(name[0], 8);
    fi.fName := PChar(@name[0]);
    Dec(fcnt);
  end;
end;
{$ENDIF}

procedure TSFSPlainVolume.PAKReadDirectory ();
var
  dsize, dofs: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..56] of Char;
begin
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.ReadBuffer(dsize, 4);
  fFileStream.Position := dofs;
  while dsize >= 64 do
  begin
    fi := TSFSFileInfo.Create(self);
    FillChar(name[0], Length(name), 0);
    fFileStream.ReadBuffer(name[0], 56);
    fi.fName := PChar(@name[0]);
    fFileStream.ReadBuffer(fi.fOfs, 4);
    fFileStream.ReadBuffer(fi.fSize, 4);
    Dec(dsize, 64);
  end;
end;

procedure TSFSPlainVolume.SINReadDirectory ();
var
  dsize, dofs: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..120] of Char;
begin
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.ReadBuffer(dsize, 4);
  fFileStream.Position := dofs;
  while dsize >= 128 do
  begin
    fi := TSFSFileInfo.Create(self);
    FillChar(name[0], Length(name), 0);
    fFileStream.ReadBuffer(name[0], 120);
    fi.fName := PChar(@name[0]);
    fFileStream.ReadBuffer(fi.fOfs, 4);
    fFileStream.ReadBuffer(fi.fSize, 4);
    Dec(dsize, 128);
  end;
end;

{$IFDEF SFS_PLAINFS_FULL}
procedure TSFSPlainVolume.GRPReadDirectory ();
var
  fcnt: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..12] of Char;
  ofs: Int64;
begin
  fFileStream.Seek(12, soCurrent); // skip signature
  fFileStream.ReadBuffer(fcnt, 4);
  ofs := fFileStream.Position+fcnt*16;
  while fcnt <> 0 do
  begin
    fi := TSFSFileInfo.Create(self);
    fi.fOfs := ofs;
    FillChar(name[0], Length(name), 0);
    fFileStream.ReadBuffer(name[0], 12);
    fi.fName := PChar(@name[0]);
    fFileStream.ReadBuffer(fi.fSize, 4);
    Inc(ofs, fi.fSize);
    Dec(fcnt);
  end;
end;

procedure TSFSPlainVolume.SPEReadDirectory ();
var
  fcnt: Word;
  fi: TSFSExtFileInfo;
  {$IFNDEF SFS_PLAIN_FS_ALTERNATIVE_SPEC}
  pp: TSFSString;
  {$ENDIF}
  name: ShortString;
  f, c: Integer;
  b: Byte;
  wasUnfixedLink: Boolean;
begin
  fFileStream.Seek(8, soCurrent); // skip signature
  fFileStream.ReadBuffer(fcnt, 2);
  while fcnt <> 0 do
  begin
    fi := TSFSExtFileInfo.Create(self);
   {$IFDEF SFS_PLAIN_FS_ALTERNATIVE_SPEC}
    SetLength(fi.fVBuf, 1); fFileStream.ReadBuffer(fi.fVBuf[0], 1);
   {$ELSE}
     SetLength(fi.fVBuf, 0);
    fFileStream.ReadBuffer(b, 1);
    pp := IntToHex(b, 2)+'/';
   {$ENDIF}
    fFileStream.ReadBuffer(name[0], 1);
    if name[0] <> #0 then fFileStream.ReadBuffer(name[1], Length(name));
    f := 1; while (f <= ord(name[0])) and (name[f] <> #0) do Inc(f); name[0] := chr(f-1);
    fi.fName := SFSReplacePathDelims(name, '/');
    if fi.fName = '' then fi.fName := 'untitled_file';
    if fi.fName[1] = '/' then Delete(fi.fName, 1, 1);
   {$IFNDEF SFS_PLAIN_FS_ALTERNATIVE_SPEC}
    fi.fName := pp+fi.fName;
   {$ENDIF}
    fFileStream.ReadBuffer(b, 1);
    if (b and $01) <> 0 then
    begin
      // link
      fFileStream.ReadBuffer(name[0], 1);
      if name[0] <> #0 then fFileStream.ReadBuffer(name[1], Length(name));
      f := 1; while (f <= ord(name[0])) and (name[f] <> #0) do Inc(f); name[0] := chr(f-1);
      if name[0] = #0 then name := #0;
      fi.fLink := name;
    end
    else
    begin
      fi.fLink := '';
      fFileStream.ReadBuffer(fi.fSize, 4);
     {$IFDEF SFS_PLAIN_FS_ALTERNATIVE_SPEC}
      Inc(fi.fSize); // plus type byte
     {$ENDIF}
      fFileStream.ReadBuffer(fi.fOfs, 4);
    end;
    Dec(fcnt);
  end;

  // now fixup links
  // nobody uses this shit, but it was documented by JC. %-)
  // i even allow links to links! %-)
  wasUnfixedLink := true;
  while wasUnfixedLink do
  begin
    f := 0; wasUnfixedLink := false;
    while f < fFiles.Count do
    begin
      fi := TSFSExtFileInfo(fFiles[f]); Inc(f);
      if (fi = nil) or (fi.fLink = '') then continue;
      c := 0;
      while c < fFiles.Count do
      begin
        if c <> f then
        begin
          // link can't be linked to itself
          if StrEquCI1251(TSFSExtFileInfo(fFiles[c]).fName, fi.fLink) then break;
        end;
        Inc(c);
      end;
      if c < fFiles.Count then
      begin
        if TSFSExtFileInfo(fFiles[c]).fLink <> '' then wasUnfixedLink := true
        else
        begin
          TSFSExtFileInfo(fFiles[c]).fOfs := fi.fOfs;
          TSFSExtFileInfo(fFiles[c]).fSize := fi.fSize;
          TSFSExtFileInfo(fFiles[c]).fLink := '';
        end;
      end
      else begin Dec(f); fFiles.Delete(f); end; // invalid link
    end;
  end;
end;

procedure TSFSPlainVolume.WAD2ReadDirectory ();
var
  fcnt, dofs: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..16] of Char;
  f, c: Integer;
begin
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(fcnt, 4);
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.Position := dofs;
  while fcnt <> 0 do
  begin
    fi := TSFSFileInfo.Create(self);
    fFileStream.ReadBuffer(fi.fOfs, 4);
    fFileStream.ReadBuffer(fi.fSize, 4);
    fFileStream.ReadBuffer(f, 4);
    fFileStream.ReadBuffer(c, 4);
    FillChar(name[0], Length(name), 0);
    fFileStream.ReadBuffer(name[0], 16);
    fi.fName := PChar(@name[0]);
    Dec(fcnt);
  end;
end;

procedure TSFSPlainVolume.ALLReadDirectory ();
var
  fcnt: Integer;
  fi: TSFSFileInfo;
  sign: packed array [0..3] of Char;
  nameList: TStringList;
  propList: TObjectList;
  name: ShortString;
  f, c: Integer;
  prp: TAllegroProperty;
begin
  nameList := TStringList.Create(); propList := nil;
  try
    propList := TObjectList.Create(true);
    fFileStream.ReadBuffer(sign[0], 4);
    if sign[0] = 's' then fFileStream.ReadBuffer(sign[0], 4);
    // signature skipped
    fcnt := ReadMD(fFileStream);
    while fcnt > 0 do
    begin
      // collect properties
      nameList.Clear(); propList.Clear();
      repeat
        fFileStream.ReadBuffer(sign[0], 4);
        if sign <> 'prop' then break;
        fFileStream.ReadBuffer(sign[0], 4);
        f := ReadMD(fFileStream); // size
        if f < 0 then
        begin
         {$IFDEF SFS_PLAIN_FS_DEBUG_ALLEGRO}
          WriteLn(ErrOutput, 'ALLEGRO: invalid property length at $', IntToHex(fFileStream.Position-8, 8));
         {$ENDIF}
          raise ESFSError.Create('invalid ALLEGRO file');
        end;
        if sign = 'NAME' then
        begin
          if f > 255 then c := 255 else c := f;
          FillChar(name, SizeOf(name), 0);
          fFileStream.ReadBuffer(name[1], c); name[0] := chr(c);
          Dec(f, c);
          c := 1; while (c <= ord(name[0])) and (name[c] <> #0) do Inc(c); name[0] := chr(c-1);
          nameList.Add(name);
        end
        else
        begin
          prp := TAllegroProperty.Create();
          Move(sign[0], name[1], 4); name[0] := #4;
          c := 1; while (c <= ord(name[0])) and (name[c] <> #0) do Inc(c); name[0] := chr(c-1);
          prp.name := sign;
          prp.ofs := fFileStream.Position;
          prp.size := f;
          propList.Add(prp);
        end;
        fFileStream.Seek(f, soCurrent);
      until false;
      if nameList.Count = 0 then nameList.Add('untitled_file');

      Move(sign[0], name[1], 4); name[5] := #0;
      f := 1; while (f <= 4) and (name[f] <> #0) do Inc(f);
      while (f > 0) and (name[f] <= ' ') do Dec(f);
      name[0] := chr(f);

      // read size
      f := ReadMD(fFileStream);
      c := ReadMD(fFileStream);
      if f <> c then
      begin
       {$IFDEF SFS_PLAIN_FS_DEBUG_ALLEGRO}
        WriteLn(ErrOutput, 'ALLEGRO: probably a packed data at $', IntToHex(fFileStream.Position-8, 8));
       {$ENDIF}
        raise ESFSError.Create('invalid ALLEGRO file');
      end;

      // add files
      while nameList.Count > 0 do
      begin
        fi := TSFSFileInfo.Create(self);
        fi.fName := nameList[0];
        fi.fPath := name;
        fi.fSize := c;
        fi.fOfs := fFileStream.Position;
        // add properties
        for f := 0 to propList.Count-1 do
        begin
          prp := TAllegroProperty(propList[f]);
          fi := TSFSFileInfo.Create(self);
          fi.fName := prp.name;
          fi.fPath := name+'.props/'+nameList[0];
          fi.fSize := prp.size;
          fi.fOfs := prp.ofs;
        end;
        nameList.Delete(0);
      end;
      fFileStream.Seek(c, soCurrent);
      Dec(fcnt);
    end;
   {$IFDEF SFS_PLAIN_FS_DEBUG_ALLEGRO}
    WriteLn(ErrOutput, 'ALLEGRO: ok');
   {$ENDIF}
  finally
    propList.Free();
    nameList.Free();
  end;
end;

procedure TSFSPlainVolume.Dune2ReadDirectory ();
var
  ofs: LongWord;
  fi: TSFSFileInfo;
  name: string[255];
  ch: Char;
begin
  repeat
    fFileStream.ReadBuffer(ofs, 4);
    if ofs = 0 then break;
    name[0] := #0;
    fFileStream.ReadBuffer(ch, 1);
    while ch <> #0 do
    begin
      if name[0] <> #255 then
      begin
        Inc(name[0]); name[ord(name[0])] := ch;
      end;
      fFileStream.ReadBuffer(ch, 1);
    end;
    if fFiles.Count > 0 then
    begin
      fi := TSFSFileInfo(fFiles[fFiles.Count-1]);
      fi.fSize := ofs-fi.fOfs;
    end;
    fi := TSFSFileInfo.Create(self);
    fi.fOfs := ofs;
    fi.fSize := 0;
    fi.fName := name;
  until false;
  if fFiles.Count > 0 then
  begin
    fi := TSFSFileInfo(fFiles[fFiles.Count-1]);
    fi.fSize := fFileStream.Size-fi.fOfs;
  end;
end;

procedure TSFSPlainVolume.MAXReadDirectory ();
var
  fcnt: LongInt;
  dofs: LongWord;
  fi: TSFSFileInfo;
  name: packed array [0..9] of Char;
begin
  fFileStream.Seek(4, soCurrent); // skip signature
  fFileStream.ReadBuffer(dofs, 4);
  fFileStream.ReadBuffer(fcnt, 4);
  fFileStream.Position := dofs;
  while fcnt >= 16 do
  begin
    fi := TSFSFileInfo.Create(self);
    FillChar(name[0], Length(name), 0);
    fFileStream.ReadBuffer(name[0], 8);
    fFileStream.ReadBuffer(fi.fOfs, 4);
    fFileStream.ReadBuffer(fi.fSize, 4);
    fi.fName := PChar(@name[0]);
    Dec(fcnt, 16);
  end;
end;
{$ENDIF}

procedure TSFSPlainVolume.ReadDirectory ();
begin
  case fType of
    sfspvPAK: PAKReadDirectory();
    sfspvSIN: SINReadDirectory();
    {$IFDEF SFS_PLAINFS_FULL}
    sfspvWAD: WADReadDirectory();
    sfspvGRP: GRPReadDirectory();
    sfspvSPE: SPEReadDirectory();
    sfspvWAD2: WAD2ReadDirectory();
    sfspvALL: ALLReadDirectory();
    sfspvDune2: Dune2ReadDirectory();
    sfspvMAX: MAXReadDirectory();
    {$ENDIF}
    else raise ESFSError.Create('invalid plain SFS');
  end;
end;

function TSFSPlainVolume.OpenFileByIndex (const index: Integer): TStream;
var
  fs: TStream;
  kill: Boolean;
begin
  result := nil; fs := nil;
  if fFiles = nil then exit;
  if (index < 0) or (index >= fFiles.Count) or (fFiles[index] = nil) then exit;
  if not (fFiles[index] is TSFSExtFileInfo) or
     (Length(TSFSExtFileInfo(fFiles[index]).fVBuf) < 1) then
  begin
    kill := false;
    try
      try
        fs := TFileStream.Create(fFileName, fmOpenRead or {fmShareDenyWrite}fmShareDenyNone);
        kill := true;
      except
        fs := fFileStream;
      end;
      result := TSFSPartialStream.Create(fs,
        TSFSFileInfo(fFiles[index]).fOfs,
        TSFSFileInfo(fFiles[index]).fSize, kill);
    except
      if kill then FreeAndNil(fs);
      result := nil;
    end;
  end
  else
  begin
    kill := false;
    try
      try
        fs := TFileStream.Create(fFileName, fmOpenRead or {fmShareDenyWrite}fmShareDenyNone);
        kill := true;
      except
        fs := fFileStream;
      end;
      result := TSFSPartialStream.Create(fs,
        TSFSExtFileInfo(fFiles[index]).fOfs,
        TSFSExtFileInfo(fFiles[index]).fSize-Length(TSFSExtFileInfo(fFiles[index]).fVBuf),
        kill,
        @(TSFSExtFileInfo(fFiles[index]).fVBuf[0]),
        Length(TSFSExtFileInfo(fFiles[index]).fVBuf));
    except
      if kill then FreeAndNil(fs);
      result := nil;
    end;
  end;
end;


{ TSFSPlainVolumeFactory }
function TSFSPlainVolumeFactory.IsMyVolumePrefix (const prefix: TSFSString): Boolean;
begin
  result :=
    StrEquCI1251(prefix, 'pak') or
    StrEquCI1251(prefix, 'sin') or
    StrEquCI1251(prefix, 'quake')
    {$IFDEF SFS_PLAINFS_FULL}
    or
    StrEquCI1251(prefix, 'wad') or // sorry
    StrEquCI1251(prefix, 'wad2') or
    StrEquCI1251(prefix, 'grp') or
    StrEquCI1251(prefix, 'spe') or
    StrEquCI1251(prefix, 'spec') or
    StrEquCI1251(prefix, 'doom') or
    StrEquCI1251(prefix, 'duke3d') or
    StrEquCI1251(prefix, 'abuse') or
    StrEquCI1251(prefix, 'allegro') or
    StrEquCI1251(prefix, 'dune2') or
    StrEquCI1251(prefix, 'max')
    {$ENDIF}
    ;
end;

procedure TSFSPlainVolumeFactory.Recycle (vol: TSFSVolume);
begin
  vol.Free();
end;

function TSFSPlainVolumeFactory.Produce (const prefix, fileName: TSFSString; st: TStream): TSFSVolume;
var
  vt: TSFSPlainVolumeType;
begin
  vt := sfspvNone;
  if PAKCheckMagic(st) then vt := sfspvPAK
  else if SINCheckMagic(st) then vt := sfspvSIN
  {$IFDEF SFS_PLAINFS_FULL}
  else if WADCheckMagic(st) then vt := sfspvWAD
  else if GRPCheckMagic(st) then vt := sfspvGRP
  else if SPECheckMagic(st) then vt := sfspvSPE
  else if WAD2CheckMagic(st) then vt := sfspvWAD2
  //else if ALLCheckMagic(st) then vt := sfspvALL
  else if MAXCheckMagic(st) then vt := sfspvMAX
  //else if Dune2CheckMagic(st) then vt := sfspvDune2 // this must be the last!
  {$ENDIF}
  ;

  if vt <> sfspvNone then
  begin
    result := TSFSPlainVolume.Create(fileName, st);
    TSFSPlainVolume(result).fType := vt;
    try
      result.DoDirectoryRead();
    except
      FreeAndNil(result);
      raise;
    end;
  end
  else result := nil;
end;


var
  pakf: TSFSPlainVolumeFactory;
initialization
  pakf := TSFSPlainVolumeFactory.Create();
  SFSRegisterVolumeFactory(pakf);
//finalization
//  SFSUnregisterVolumeFactory(pakf);
end.
