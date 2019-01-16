unit g_resources;

interface

  (**
    g_GetResourceSection
      Parse path in form 'path/to/file.wad:some/section/resouce' to
      wad = 'path/to/file.wa', section = 'some/section', name = 'resource'

    g_DeleteFile
      Delete file if it exists. Make backup if enabled.

    g_ReadResource
      Read whole file from wad
      (data <> nil) and (len > 0) when ok
      use FreeMem(data) when done

    g_ReadSubResource
      Read whole file from folded wad
      (data <> nil) and (len > 0) when ok
      use FreeMem(data) when done

    g_DeleteResource
      Delete file from wad
      res = 0 when ok

    g_AddResource
      Add/overwrite file to wad
      res = 0 when ok

    g_ExistsResource
      Check that resource exists
      res = 0 when ok
  **)

  (* Editor options *)
  var
    Compress: Boolean;
    Backup: Boolean;

  procedure g_GetResourceSection (path: String; out wad, section, name: String);
  procedure g_DeleteFile(wad: String; backupPostfix: String = '.bak');

  procedure g_ReadResource (wad, section, name: String; out data: PByte; out len: Integer);
  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);
  procedure g_DeleteResource (wad, section, name: String; out res: Integer);
  procedure g_AddResource (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
  procedure g_ExistsResource (wad, section, name: String; out res: Integer);

implementation

  uses sfs, xstreams, dfzip, utils, Classes, SysUtils, WADEDITOR, e_log;

  function NoTrailing (path: String): String;
    var i: Integer;
  begin
    i := Length(path);
    while (i > 0) and ((path[i] = '/') or (path[i] = '\')) do dec(i);
    result := Copy(path, 1, i)
  end;

  function g_CleanPath (path: String; sys: Boolean = False): String;
    var i, len: Integer;
  begin
    i := 1;
    result := '';
    len := Length(path);
    (* drop separators at the end *)
    while (len > 1) and ((path[i] = '/') or (path[i] = '\')) do dec(len);
    while i <= len do
    begin
      while (i <= len) and (path[i] <> '/') and (path[i] <> '\') do
      begin
        result := result + path[i];
        inc(i)
      end;
      if i <= len then
        if sys then
          result := result + DirectorySeparator
        else
          result := result + '/';
      inc(i);
      while (i <= len) and ((path[i] = '/') or (path[i] = '\')) do inc(i)
    end;
  end;

  procedure g_GetResourceSection (path: String; out wad, section, name: String);
    var i, j, len: Integer;
  begin
    len := Length(path);
    i := len;
    while (i > 0) and (path[i] <> '/') and (path[i] <> '\') do dec(i);
    name := Copy(path, i + 1, len);
    j := i;
    while (i > 0) and (path[i] <> ':') do dec(i);
    section := Copy(path, i + 1, j - i - 1);
    wad := Copy(path, 1, i - 1);
  end;

  procedure g_DeleteFile (wad: String; backupPostfix: String = '.bak');
    var newwad: String;
  begin
    if Backup then
    begin
      if FileExists(wad) then
      begin
        newwad := wad + backupPostfix;
        if FileExists(newwad) then
          ASSERT(DeleteFile(newwad));
        ASSERT(RenameFile(wad, newwad))
      end
    end
    else
    begin
      if FileExists(wad) then
        ASSERT(DeleteFile(wad))
    end
  end;

  procedure g_AddResourceToDFWAD (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
    var f: TWADEditor_1;
  begin
    res := 1; (* error *)
    wad := utf2win(wad);
    section := utf2win(NoTrailing(section));
    name := utf2win(name);
    ASSERT(name <> '');
    f := TWADEditor_1.Create();
    if not f.ReadFile(wad) then
    begin
      (* do nothing *)
    end;
    f.CreateImage;
    f.RemoveResource(section, name);
    f.AddResource(data, len, name, section);
    g_DeleteFile(wad);
    f.SaveTo(wad);
    f.Free;
    res := 0
  end;

  procedure g_AddResourceToZip (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
    var
      i, n, len0: Integer;
      data0: PByte;
      list: TSFSFileList;
      tmp, path: String;
      ts: TFileStream;
      dir: array of TFileInfo;

    procedure Add (name: String; data: PByte; len: Integer);
      var ds: TSFSMemoryChunkStream;
    begin
      SetLength(dir, n + 1);
      ds := TSFSMemoryChunkStream.Create(data, len, False);
      dir[n] := dfzip.ZipOne(ts, name, ds, Compress);
      ds.Free;
      INC(n);
    end;

  begin
    res := 1;
    wad := ExpandFileName(wad);
    section := utf2win(NoTrailing(section));
    name := utf2win(name);
    ASSERT(name <> '');
    list := SFSFileList(wad);
    tmp := wad + '.tmp' + IntToStr(Random(100000));
    ts := TFileStream.Create(tmp, fmCreate);
    n := 0;
    SetLength(dir, 0);
    if list <> nil then
    begin
      for i := 0 to list.Count - 1 do
      begin
        path := NoTrailing(list.Files[i].path);
        if (path <> section) or (list.Files[i].name <> name) then
        begin
          g_ReadResource(wad, path, list.Files[i].name, data0, len0);
          ASSERT(data0 <> nil);
          if path = '' then
            path := list.Files[i].name
          else
            path := path + '/' + list.Files[i].name;
          Add(path, data0, len0);
          FreeMem(data0)
        end
      end;
      list.Destroy
    end;

    if section = '' then
      path := name
    else
      path := section + '/' + name;
    Add(path, data, len);

    dfzip.writeCentralDir(ts, dir);
    ts.Free;

    g_DeleteFile(wad);
    ASSERT(RenameFile(tmp, wad));
    res := 0
  end;

  procedure g_AddResource (wad, section, name: String; const data: PByte; len: Integer; out res: Integer);
    var ext: String;
  begin
    ASSERT(name <> '');
    res := 2; (* unknown type *)
    ext := LowerCase(SysUtils.ExtractFileExt(wad));
    e_WriteLog('g_AddResource "' + wad + '" "' + section + '" "' + name + '"', MSG_NOTIFY);
    if ext = '.wad' then
      g_AddResourceToDFWAD(wad, section, name, data, len, res)
    else
      g_AddResourceToZip(wad, section, name, data, len, res)
  end;

  procedure g_DeleteResourceFromDFWAD (wad, section, name: String; out res: Integer);
    var f: TWADEditor_1;
  begin
    ASSERT(name <> '');
    res := 1; (* error *)
    section := utf2win(NoTrailing(section));
    name := utf2win(name);
    f := TWADEditor_1.Create;
    if not f.ReadFile(wad) then
    begin
      f.Free;
      Exit
    end;
    f.CreateImage;
    f.RemoveResource(section, name);
    g_DeleteFile(wad);
    f.SaveTo(wad);
    f.Free;
    res := 0 (* ok *)
  end;

  procedure g_DeleteResourceFromZip (wad, section, name: String; out res: Integer);
    var
      data0: PByte;
      i, n, len0: Integer;
      list: TSFSFileList;
      tmp, path: String;
      ts: TFileStream;
      dir: array of TFileInfo;

    procedure Add (name: String; data: PByte; len: Integer);
      var ds: TSFSMemoryChunkStream;
    begin
      SetLength(dir, n + 1);
      ds := TSFSMemoryChunkStream.Create(data, len, False);
      dir[n] := dfzip.ZipOne(ts, name, ds, Compress);
      ds.Free;
      INC(n);
    end;

  begin
    res := 1;
    wad := ExpandFileName(wad);
    section := utf2win(NoTrailing(section));
    name := utf2win(name);
    ASSERT(name <> '');
    list := SFSFileList(wad);
    tmp := wad + '.tmp' + IntToStr(Random(100000));
    ts := TFileStream.Create(tmp, fmCreate);
    n := 0;
    SetLength(dir, 0);
    if list <> nil then
    begin
      for i := 0 to list.Count - 1 do
      begin
        path := NoTrailing(list.Files[i].path);
        if (path <> section) or (list.Files[i].name <> name) then
        begin
          g_ReadResource(wad, path, list.Files[i].name, data0, len0);
          ASSERT(data0 <> nil);
          if path = '' then
            path := list.Files[i].name
          else
            path := path + '/' + list.Files[i].name;
          Add(path, data0, len0);
          FreeMem(data0)
        end
      end;
      list.Destroy
    end;

    dfzip.writeCentralDir(ts, dir);
    ts.Free;

    g_DeleteFile(wad);
    ASSERT(RenameFile(tmp, wad));
    res := 0
  end;

  procedure g_DeleteResource (wad, section, name: String; out res: Integer);
    var ext: String;
  begin
    ASSERT(name <> '');
    res := 2; (* unknown type *)
    ext := LowerCase(SysUtils.ExtractFileExt(wad));
    if ext = '.wad' then
      g_DeleteResourceFromDFWAD(wad, section, name, res)
    else
      g_DeleteResourceFromZip(wad, section, name, res)
  end;

  procedure g_ExistsResource (wad, section, name: String; out res: Integer);
    var str: String; stream: TStream;
  begin
    res := 1;
    section := utf2win(NoTrailing(section));
    name := utf2win(name);
    ASSERT(name <> '');
    if SFSAddDataFileTemp(wad, TRUE) then
    begin
      str := SFSGetLastVirtualName(section + '\' + name);
      stream := SFSFileOpen(wad + '::' + str);
      if stream <> nil then
      begin
        res := 0;
        stream.Destroy
      end
    end;
    SFSGCCollect
  end;

  procedure g_ReadResource (wad, section, name: String; out data: PByte; out len: Integer);
    var stream: TStream; str: String; i: Integer;
  begin
    e_WriteLog('g_ReadResource: "' + wad + '" "' + section + '" "' + name + '"', MSG_NOTIFY);
    section := utf2win(NoTrailing(section));
    name := utf2win(name);
    data := nil;
    len := 0;
    //ASSERT(name <> '');
    if name = '' then Exit; (* SKY can be void *)
    if SFSAddDataFileTemp(wad, TRUE) then
    begin
      str := SFSGetLastVirtualName(section + '/' + name);
      stream := SFSFileOpen(wad + '::' + str);
      if stream <> nil then
      begin
        len := stream.Size;
        GetMem(data, len);
        ASSERT(data <> nil);
        //stream.ReadBuffer(data, len); (* leads to segfault *)
        for i := 0 to len - 1 do
          data[i] := stream.ReadByte();
        stream.Destroy
      end
    end;
    SFSGCCollect
  end;

  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);
    var stream0, stream1: TStream; str0, str1: String; i: Integer;
  begin
    data := nil;
    len := 0;
    section0 := utf2win(NoTrailing(section0));
    name0 := utf2win(name0);
    section1 := utf2win(NoTrailing(section1));
    name1 := utf2win(name1);
    //ASSERT(name0 <> '');
    //ASSERT(name1 <> '');
    if (wad = '') OR (name0 = '') OR (name1 = '') then Exit; (* ??? *)
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
            ASSERT(data <> nil);
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
