unit g_resources;

interface

  (**
    g_ReadResource
      Read whole file from wad

    g_ReadSubResource
      Read whole file from folded wad

    g_DeleteResource
      Delete file from wad, res = 0 when ok

    g_AddResource
      Add/overwrite file to wad, res = 0 when ok
  **)

  procedure g_ReadResource (wad, section, name: String; out data: PByte; out len: Integer);
  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);
  procedure g_DeleteResource (wad, section, name: String; out res: Integer);
  procedure g_AddResource (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);

implementation

  uses sfs, xstreams, dfzip, utils, Classes, SysUtils, WADEDITOR;

  procedure g_AddResourceToDFWAD (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
    var f: TWADEditor_1;
  begin
    res := 1; (* error *)
    f := TWADEditor_1.Create();
    if not f.ReadFile(wad) then
    begin
      (* do nothing *)
    end;
    f.CreateImage;
    f.RemoveResource(section, name);
    f.AddResource(data, len, name, section);
    f.SaveTo(wad);
    f.Free;
    res := 0
  end;

  procedure g_AddResourceToZip (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
    var
      i, n, len0: Integer;
      data0: PByte;
      list: TSFSFileList;
      tmp, entry: String;
      ts: TFileStream;
      dir: array of TFileInfo;

    procedure Add (name: String; data: PByte; len: Integer);
      var ds: TSFSMemoryChunkStream;
    begin
      SetLength(dir, n + 1);
      ds := TSFSMemoryChunkStream.Create(data, len, false);
      dir[n] := dfzip.ZipOne(ts, name, ds);
      ds.Free;
      INC(n);
    end;

  begin
    res := 1;
    wad := ExpandFileName(wad);
    list := SFSFileList(wad);
    tmp := wad + '.tmp' + IntToStr(Random(100000));
    ts := TFileStream.Create(tmp, fmCreate);
    n := 0;
    SetLength(dir, 0);
    if list <> nil then
    begin
      for i := 0 to list.Count - 1 do
      begin
        if (list.Files[i].path <> section) or (list.Files[i].name <> section) then
        begin
          g_ReadResource(wad, list.Files[i].path, list.Files[i].name, data0, len0);
          if list.Files[i].path = '' then
            entry := list.Files[i].name
          else
            entry := list.Files[i].path + '/' + list.Files[i].name;
          Add(entry, data0, len0);
          FreeMem(data0)
        end
      end;
      list.Destroy
    end;

    if section = '' then
      entry := name
    else
      entry := section + '/' + name;

    Add(entry, data, len);
    dfzip.writeCentralDir(ts, dir);
    ts.Free;

    if FileExists(wad) then
      ASSERT(RenameFile(wad, wad + '.bak'));
    ASSERT(RenameFile(tmp, wad));
    res := 0
  end;

  procedure g_AddResource (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
    var ext: String;
  begin
    res := 2; (* unknown type *)
    ext := LowerCase(SysUtils.ExtractFileExt(wad));
    if ext = '.wad' then
      g_AddResourceToDFWAD(wad, section, name, data, len, res)
    else if (ext = '.pk3') or (ext = '.zip') or (ext = '.dfzip') then
      g_AddResourceToZip(wad, section, name, data, len, res)
  end;

  procedure g_DeleteResourceFromDFWAD (wad, section, name: String; out res: Integer);
    var f: TWADEditor_1;
  begin
    res := 1; (* error *)
    f := TWADEditor_1.Create;
    if not f.ReadFile(wad) then
    begin
      f.Free;
      Exit
    end;
    f.CreateImage;
    f.RemoveResource(section, name);
    f.SaveTo(wad);
    f.Free;
    res := 0 (* ok *)
  end;

  procedure g_DeleteResourceFromZip (wad, section, name: String; out res: Integer);
  begin
    res := 1 (* not implemented *)
  end;

  procedure g_DeleteResource (wad, section, name: String; out res: Integer);
    var ext: String;
  begin
    res := 2; (* unknown type *)
    ext := LowerCase(SysUtils.ExtractFileExt(wad));
    if ext = '.wad' then
      g_DeleteResourceFromDFWAD(wad, section, name, res)
    else if (ext = '.pk3') or (ext = '.zip') or (ext = '.dfzip') then
      g_DeleteResourceFromZip(wad, section, name, res)
  end;

  procedure g_ReadResource (wad, section, name: String; out data: PByte; out len: Integer);
    var
      stream: TStream;
      str: String;
      i: Integer;
  begin
    section := utf2win(section);
    name := utf2win(name);
    data := nil;
    len := 0;
    if SFSAddDataFileTemp(wad, TRUE) then
    begin
      str := SFSGetLastVirtualName(section + '\' + name);
      stream := SFSFileOpen(wad + '::' + str);
      if stream <> nil then
      begin
        len := stream.Size;
        GetMem(data, len);
        //stream.ReadBuffer(data, len); (* leads to segfault *)
        for i := 0 to len - 1 do
          data[i] := stream.ReadByte();
        stream.Destroy
      end
    end;
    SFSGCCollect
  end;

  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);
    var
      stream0, stream1: TStream;
      str0, str1: String;
      i: Integer;
  begin
    data := nil;
    len := 0;
    if (wad = '') OR (section0 = '') OR (name0 = '') OR (section1 = '') OR (name1 = '') then Exit;
    section0 := utf2win(section0);
    name0 := utf2win(name0);
    section1 := utf2win(section1);
    name1 := utf2win(name1);
    if SFSAddDataFileTemp(wad, TRUE) then
    begin
      str0 := SFSGetLastVirtualName(section0 + '\' + name0);
      stream0 := SFSFileOpen(wad + '::' + str0);
      if stream0 <> nil then
      begin
        if SFSAddSubDataFile(wad + '\' + str0, stream0, TRUE) then
        begin
          str1 := SFSGetLastVirtualName(section1 + '\' + name1);
          stream1 := SFSFileOpen(wad + '\' + str0 + '::' + str1);
          if stream1 <> nil then
          begin
            len := stream1.Size;
            GetMem(data, len);
            //stream1.ReadBuffer(data, len); (* leads to segfault *)
            for i := 0 to len - 1 do
              data[i] := stream1.ReadByte();
            stream1.Destroy
          end
        end
      end
      else
      begin
        stream0.Destroy
      end
    end;
    SFSGCCollect
  end;

end.
