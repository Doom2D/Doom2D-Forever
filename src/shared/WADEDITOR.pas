{$INCLUDE ../shared/a_modes.inc}

unit WADEDITOR;

// TWADEditor errors:
// - Create = DFWAD_NOERROR
// - FreeWAD = DFWAD_NOERROR
// - ReadFile -> DFWAD_ERROR_WADNOTFOUND, DFWAD_ERROR_CANTOPENWAD, DFWAD_ERROR_FILENOTWAD, DFWAD_ERROR_WRONGVERSION, DFWAD_ERROR_READWAD, DFWAD_NOERROR
// - ReadMemory -> DFWAD_ERROR_FILENOTWAD, DFWAD_ERROR_WRONGVERSION, DFWAD_NOERROR
// - CreateImage -> DFWAD_ERROR_WADNOTLOADED, DFWAD_OPENED_MEMORY, DFWAD_ERROR_CANTOPENWAD, DFWAD_NOERROR
// - AddResource (pointer)
// - AddResource (filename) -> DFWAD_ERROR_CANTOPENWAD, DFWAD_ERROR_READWAD, DFWAD_NOERROR
// - AddAlias
// - AddSection
// - RemoveResource
// - SaveTo
// - HaveResource
// - HaveSection
// - GetResource -> DFWAD_ERROR_WADNOTLOADED, DFWAD_ERROR_RESOURCENOTFOUND, DFWAD_ERROR_CANTOPENWAD, DFWAD_NOERROR
// - GetSectionList
// - GetResourcesList

interface

  uses Classes;

  const
    DFWAD_NOERROR                = 0;
    DFWAD_ERROR_WADNOTFOUND      = -1;
    DFWAD_ERROR_CANTOPENWAD      = -2;
    DFWAD_ERROR_RESOURCENOTFOUND = -3;
    DFWAD_ERROR_FILENOTWAD       = -4;
    DFWAD_ERROR_WADNOTLOADED     = -5;
    DFWAD_ERROR_READRESOURCE     = -6;
    DFWAD_ERROR_READWAD          = -7;
    DFWAD_ERROR_WRONGVERSION     = -8;

  type
    SArray = array of ShortString;

    TWADEditor = class abstract(TObject)
      public
        function ReadFile(FileName: string): Boolean;

        function ReadFile2(FileName: string): Boolean; virtual; abstract;
        function ReadMemory(Data: Pointer; Len: LongWord): Boolean; virtual; abstract;
        procedure FreeWAD(); virtual; abstract;
        procedure CreateImage(); virtual; abstract;
        function AddResource(Data: Pointer; Len: LongWord; Name: string; Section: string): Boolean; virtual; abstract; overload;
        function AddResource(FileName, Name, Section: string): Boolean; overload; virtual; abstract;
        function AddAlias(Res, Alias: string): Boolean; virtual; abstract;
        procedure AddSection(Name: string); virtual; abstract;
        procedure RemoveResource(Section, Resource: string); virtual; abstract;
        procedure SaveTo(FileName: string); virtual; abstract;
        function HaveResource(Section, Resource: string): Boolean; virtual; abstract;
        function HaveSection(Section: string): Boolean; virtual; abstract;
        function GetResource(Section, Resource: string; var pData: Pointer; var Len: Integer): Boolean; virtual; abstract;
        function GetSectionList(): SArray; virtual; abstract;
        function GetResourcesList(Section: string): SArray; virtual; abstract;

        function GetLastError: Integer; virtual; abstract;
        function GetLastErrorStr: String; virtual; abstract;
        function GetResourcesCount: Word; virtual; abstract;
        function GetVersion: Byte; virtual; abstract;
    end;

    TWADEditorClass = class of TWADEditor;

    TWADEditorMapping = class sealed(TObject)
      private
        FName: String;
        FWADEditorClass: TWADEditorClass;
      public
        constructor CreateEx(const name: String; const eclass: TWADEditorClass);
        property Name: String read FName;
        property WADEditorClass: TWADEditorClass read FWADEditorClass;
    end;

    TWADEditorFactory = class sealed(TObject)
      private
        FMappings: TStringList;
        FDefault: TWADEditorClass;
      public
        constructor Create;
        destructor Destroy; override;
        procedure RegisterEditor(const name: String; const eclass: TWADEditorClass);
        procedure SetDefaultEditor(const name: String);
        function CreateEditor(const name: String): TWADEditor;
        function CreateDefaultEditor(): TWADEditor;
        function OpenFile(FileName: String): TWADEditor;
        function OpenMemory(Data: Pointer; Len: Integer): TWADEditor;
        procedure GetRegistredEditors(var list: TStringList);
    end;

    // TWADEditor_1 deprecated
    TWADEditor_1 = class sealed(TObject)
      private
        FBase: TWADEditor;
      public
        constructor Create();
        destructor Destroy(); override;
        procedure FreeWAD();
        function ReadFile(FileName: string): Boolean;
        function ReadMemory(Data: Pointer; Len: LongWord): Boolean;
        procedure CreateImage();
        function AddResource(Data: Pointer; Len: LongWord; Name: string; Section: string): Boolean; overload;
        function AddResource(FileName, Name, Section: string): Boolean; overload;
        function AddAlias(Res, Alias: string): Boolean;
        procedure AddSection(Name: string);
        procedure RemoveResource(Section, Resource: string);
        procedure SaveTo(FileName: string);
        function HaveResource(Section, Resource: string): Boolean;
        function HaveSection(Section: string): Boolean;
        function GetResource(Section, Resource: string; var pData: Pointer; var Len: Integer): Boolean;
        function GetSectionList(): SArray;
        function GetResourcesList(Section: string): SArray;

        function GetLastError: Integer;
        function GetLastErrorStr: String;
        function GetResourcesCount: Word;
        function GetVersion: Byte;
    end;

  procedure g_ProcessResourceStr(ResourceStr: String; var FileName, SectionName, ResourceName: String); overload;
  procedure g_ProcessResourceStr(ResourceStr: String; FileName, SectionName, ResourceName: PString); overload;

  function gWADEditorFactory: TWADEditorFactory;

implementation

  uses SysUtils, utils;

  var
    uWADEditorFactory: TWADEditorFactory;

  procedure g_ProcessResourceStr(ResourceStr: String; var FileName, SectionName, ResourceName: String);
    var a, i: Integer;
  begin
    for i := Length(ResourceStr) downto 1 do
      if ResourceStr[i] = ':' then
        Break;

    FileName := Copy(ResourceStr, 1, i-1);

    for a := i+1 to Length(ResourceStr) do
      if (ResourceStr[a] = '\') or (ResourceStr[a] = '/') then Break;

    ResourceName := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
    SectionName := Copy(ResourceStr, i+1, Length(ResourceStr)-Length(ResourceName)-Length(FileName)-2);
  end;

  procedure g_ProcessResourceStr(ResourceStr: AnsiString; FileName, SectionName, ResourceName: PAnsiString);
    var a, i, l1, l2: Integer;
  begin
    for i := Length(ResourceStr) downto 1 do
      if ResourceStr[i] = ':' then
        Break;

    if FileName <> nil then
      begin
        FileName^ := Copy(ResourceStr, 1, i-1);
        l1 := Length(FileName^);
      end
    else
      l1 := 0;

    for a := i+1 to Length(ResourceStr) do
      if (ResourceStr[a] = '\') or (ResourceStr[a] = '/') then Break;

    if ResourceName <> nil then
      begin
        ResourceName^ := Copy(ResourceStr, a+1, Length(ResourceStr)-Abs(a));
        l2 := Length(ResourceName^);
      end
    else
      l2 := 0;

    if SectionName <> nil then
      SectionName^ := Copy(ResourceStr, i+1, Length(ResourceStr)-l2-l1-2);
  end;

{ TWADEditor }

  function TWADEditor.ReadFile(FileName: String): Boolean;
    var fname: String;
  begin
    fname := findFileCIStr(FileName);
    Result := ReadFile2(fname);
  end;

{ TWADEditorMapping }

  constructor TWADEditorMapping.CreateEx(const name: String; const eclass: TWADEditorClass);
  begin
    Create;
    FName := name;
    FWADEditorClass := eclass;
  end;

{ TWADEditorFactory }

  constructor TWADEditorFactory.Create;
  begin
    FMappings := TStringList.Create();
    FDefault := nil;
  end;

  destructor TWADEditorFactory.Destroy;
    var i: Integer;
  begin
    for i := 0 to FMappings.Count - 1 do
      FMappings.Objects[i].Free();
    FMappings.Free();
    FDefault := nil;
  end;

  procedure TWADEditorFactory.RegisterEditor(const name: String; const eclass: TWADEditorClass);
  begin
    if FMappings.IndexOf(UpperCase(name)) <> -1 then
      raise Exception.Create('Registering a duplicate WAD Editor name <' + name + '>');
    if FDefault = nil then
      FDefault := eclass;
    FMappings.AddObject(UpperCase(name), TWADEditorMapping.CreateEx(name, eclass));
  end;

  procedure TWADEditorFactory.SetDefaultEditor(const name: String);
    var i: Integer;
  begin
    i := FMappings.IndexOf(UpperCase(name));
    if i = -1 then
      raise Exception.Create('No WAD Editor was registred by the name <' + name + '>');
    FDefault := TWADEditorMapping(FMappings.Objects[i]).WADEditorClass;
  end;

  function TWADEditorFactory.CreateEditor(const name: String): TWADEditor;
    var i: Integer;
  begin
    if name = '' then
    begin
      Result := CreateDefaultEditor();
    end
    else
    begin
      i := FMappings.IndexOf(UpperCase(name));
      if i = -1 then
        raise Exception.Create('No WAD Editor was registred by the name <' + name + '>');
      Result := TWADEditorMapping(FMappings.Objects[i]).WADEditorClass.Create();
    end;
  end;

  function TWADEditorFactory.CreateDefaultEditor(): TWADEditor;
  begin
    if FDefault = nil then
      raise Exception.Create('No default WAD Editor was registred');
    Result := FDefault.Create();
  end;

  function TWADEditorFactory.OpenFile(FileName: String): TWADEditor;
    var i: Integer; tmp: TWADEditor; fname: String;
  begin
    Result := nil;
    if FMappings <> nil then
    begin
      fname := findFileCIStr(FileName);
      for i := 0 to FMappings.Count - 1 do
      begin
        tmp := gWADEditorFactory.CreateEditor(FMappings[i]);
        if tmp.ReadFile2(fname) then
        begin
          Result := tmp;
          break;
        end;
        FreeAndNil(tmp);
      end;
    end;
  end;

  function TWADEditorFactory.OpenMemory(Data: Pointer; Len: Integer): TWADEditor;
    var i: Integer; tmp: TWADEditor;
  begin
    Result := nil;
    if FMappings <> nil then
    begin
      for i := 0 to FMappings.Count - 1 do
      begin
        tmp := gWADEditorFactory.CreateEditor(FMappings[i]);
        if tmp.ReadMemory(Data, Len) then
        begin
          Result := tmp;
          break;
        end;
        FreeAndNil(tmp);
      end;
    end;
  end;

  procedure TWADEditorFactory.GetRegistredEditors(var list: TStringList);
    var i: Integer;
  begin
    list.Clear();
    for i := 0 to FMappings.Count - 1 do
      list.Add(TWADEditorMapping(FMappings.Objects[i]).Name);
  end;

  function gWADEditorFactory: TWADEditorFactory;
  begin
    if not Assigned(uWADEditorFactory) then
      uWADEditorFactory := TWADEditorFactory.Create();
    Result := uWADEditorFactory;
  end;

{ TWADEditor_1 }

  constructor TWADEditor_1.Create();
  begin
    FBase := gWADEditorFactory.CreateDefaultEditor();
  end;

  destructor TWADEditor_1.Destroy();
  begin
    if FBase <> nil then
      FBase.Free();
    inherited;
  end;

  procedure TWADEditor_1.CreateImage();
  begin
    FBase.CreateImage();
  end;

  procedure TWADEditor_1.FreeWAD();
  begin
    FBase.FreeWAD();
  end;

  function TWADEditor_1.ReadFile(FileName: String): Boolean;
    var tmp: TWADEditor;
  begin
    Result := FBase.ReadFile(FileName);
    if Result = False then
    begin
      tmp := gWADEditorFactory.OpenFile(FileName);
      if tmp <> nil then
      begin
        FreeAndNil(FBase);
        FBase := tmp;
        Result := True;
      end;
    end;
  end;

  function TWADEditor_1.ReadMemory(Data: Pointer; Len: LongWord): Boolean;
    var tmp: TWADEditor;
  begin
    Result := FBase.ReadMemory(Data, Len);
    if Result = False then
    begin
      tmp := gWADEditorFactory.OpenMemory(Data, Len);
      if tmp <> nil then
      begin
        FreeAndNil(FBase);
        FBase := tmp;
        Result := True;
      end;
    end;
  end;

  procedure TWADEditor_1.SaveTo(FileName: string);
    var fname: AnsiString;
  begin
    fname := findFileCIStr(FileName);
    FBase.SaveTo(fname);
  end;

  function TWADEditor_1.AddAlias(Res, Alias: string): Boolean;
  begin
    Result := FBase.AddAlias(Res, Alias);
  end;

  function TWADEditor_1.AddResource(Data: Pointer; Len: LongWord; Name: string; Section: string): Boolean;
  begin
    Result := FBase.AddResource(Data, Len, Name, Section);
  end;

  function TWADEditor_1.AddResource(FileName, Name, Section: string): Boolean;
    var fname: AnsiString;
  begin
    fname := findFileCIStr(FileName);
    Result := FBase.AddResource(fname, Name, Section);
  end;

  procedure TWADEditor_1.AddSection(Name: string);
  begin
    FBase.AddSection(Name);
  end;

  function TWADEditor_1.GetSectionList(): SArray;
  begin
    Result := FBase.GetSectionList();
  end;

  function TWADEditor_1.HaveSection(Section: string): Boolean;
  begin
    Result := FBase.HaveSection(Section);
  end;

  function TWADEditor_1.GetResourcesList(Section: string): SArray;
  begin
    Result := FBase.GetResourcesList(Section);
  end;

  function TWADEditor_1.HaveResource(Section, Resource: string): Boolean;
  begin
   Result := FBase.HaveResource(Section, Resource);
  end;

  function TWADEditor_1.GetResource(Section, Resource: string; var pData: Pointer; var Len: Integer): Boolean;
  begin
    Result := FBase.GetResource(Section, Resource, pData, Len);
  end;

  procedure TWADEditor_1.RemoveResource(Section, Resource: string);
  begin
    FBase.RemoveResource(Section, Resource);
  end;

  function TWADEditor_1.GetLastError: Integer;
  begin
    Result := FBase.GetLastError();
  end;

  function TWADEditor_1.GetLastErrorStr: String;
  begin
    Result := FBase.GetLastErrorStr();
  end;

  function TWADEditor_1.GetResourcesCount: Word;
  begin
    Result := FBase.GetResourcesCount();
  end;

  function TWADEditor_1.GetVersion: Byte;
  begin
    Result := FBase.GetVersion;
  end;

finalization
  FreeAndNil(uWADEditorFactory);
end.
