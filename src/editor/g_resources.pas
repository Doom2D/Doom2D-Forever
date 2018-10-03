unit g_resources;

interface

  procedure g_ReadResource (wad, section, name: String; out data: PByte; out len: Integer);
  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);

implementation

  uses sfs, xstreams, utils, Classes;

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
  end;

  procedure g_ReadSubResource (wad, section0, name0, section1, name1: String; out data: PByte; out len: Integer);
    var
      stream0, stream1: TStream;
      str0, str1: String;
      xdata: Pointer;
      i, xlen: Integer;
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
          stream1 := SFSFileOpenEx(wad + '\' + str0 + '::' + str1);
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
    end
  end;

end.
