unit g_resources;

interface

  procedure g_ReadResource (wad, section, name: String; out data: PByte; out len: Integer);
  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);

implementation

  uses sfs, utils, Classes;

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
    sfsGCDisable;
    if SFSAddDataFileTemp(wad) then
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
    sfsGCEnable
  end;

  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);
    var
      stream0, stream1: TStream;
      str0, str1: String;
      i: Integer;
  begin
    section0 := utf2win(section0);
    name0 := utf2win(name0);
    section1 := utf2win(section1);
    name1 := utf2win(name1);
    data := nil;
    len := 0;
    sfsGCDisable;
    if SFSAddDataFile(wad) then
    begin
      str0 := SFSGetLastVirtualName(section0 + '\' + name0);
      stream0 := SFSFileOpen(wad + '::' + str0);
      if stream0 <> nil then
      begin
        if SFSAddSubDataFile(wad + '\' + str0, stream0) then
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
        end;
        //stream0.Destroy (* leads to memory corruption *)
      end
    end;
    sfsGCEnable;
  end;

end.
