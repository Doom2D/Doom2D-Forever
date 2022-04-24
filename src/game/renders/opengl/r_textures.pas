(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../../../shared/a_modes.inc}
unit r_textures;

interface

  uses
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    r_atlas,
    utils    // SSArray
  ;

  type
    TGLAtlas = class;

    TGLAtlasNode = class (TAtlasNode)
      private
        mBase: TGLAtlas;

      public
        constructor Create (base: TGLAtlas);
        destructor Destroy; override;

        function GetID (): GLuint;

        property base: TGLAtlas read mBase;
        property id: GLuint read GetID;
    end;

    TGLAtlas = class (TAtlas)
      private
        mID: GLuint;

      public
        constructor Create (ww, hh: Integer; id: GLuint);
        destructor Destroy; override;

        function CreateNode (): TGLAtlasNode; override;
        function Alloc (ww, hh: Integer): TGLAtlasNode; overload;

        property id: GLuint read mID write mID default 0;
    end;

    TGLTexture = class
      private
        mWidth: Integer;
        mHeight: Integer;
        mCols: Integer;
        mTile: array of TGLAtlasNode;

      public
        destructor Destroy; override;

        function GetTile (col, line: Integer): TGLAtlasNode;

        function GetLines (): Integer; inline;

        property width: Integer read mWidth;
        property height: Integer read mHeight;
        property cols: Integer read mCols;
        property lines: Integer read GetLines;
    end;

    TGLMultiTexture = class
      private
        mTexture: array of TGLTexture;
        mBackanim: Boolean;

      public
        destructor Destroy; override;

        function GetWidth (): Integer; inline;
        function GetHeight (): Integer; inline;
        function GetCount (): Integer; inline;
        function GetTexture (i: Integer): TGLTexture; {inline;}

        property width: Integer read GetWidth;
        property height: Integer read GetHeight;
        property count: Integer read GetCount;
        property backAnim: Boolean read mBackanim; (* this property must be located at TAnimState? *)
    end;

  procedure r_Textures_Initialize;
  procedure r_Textures_Finalize;

  function r_Textures_LoadFromFile (const filename: AnsiString; log: Boolean = True): TGLTexture;
  function r_Textures_LoadMultiFromFile (const filename: AnsiString; log: Boolean = True): TGLMultiTexture;
  function r_Textures_LoadMultiFromFileAndInfo (const filename: AnsiString; w, h, count: Integer; backanim: Boolean; log: Boolean = True): TGLMultiTexture;

implementation

  uses
    SysUtils, Classes,
    e_log, e_res, WADReader, Config,
    Imaging, ImagingTypes, ImagingUtility
  ;

  var
    maxTileSize: Integer;
    atl: array of TGLAtlas;
//    tex: array of TGLTexture;

  (* --------- TGLAtlasNode --------- *)

  constructor TGLAtlasNode.Create (base: TGLAtlas);
  begin
    ASSERT(base <> nil);
    inherited Create();
    self.mBase := base;
  end;

  destructor TGLAtlasNode.Destroy;
  begin
    inherited;
  end;

  function TGLAtlasNode.GetID (): GLuint;
  begin
    result := self.base.id
  end;

  procedure r_Textures_UpdateNode (n: TGLAtlasNode; data: Pointer; x, y, w, h: Integer);
  begin
    ASSERT(n <> nil);
    // ASSERT(n.leaf);
    ASSERT(n.base <> nil);
    ASSERT(data <> nil);
    ASSERT(x >= 0);
    ASSERT(y >= 0);
    ASSERT(n.l + x + w - 1 <= n.r);
    ASSERT(n.t + y + h - 1 <= n.b);
    ASSERT(n.id > 0);
    glBindTexture(GL_TEXTURE_2D, n.id);
    glTexSubImage2D(GL_TEXTURE_2D, 0, n.l + x, n.t + y, w, h, GL_RGBA, GL_UNSIGNED_BYTE, data);
    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  (* --------- TGLAtlas --------- *)

  constructor TGLAtlas.Create (ww, hh: Integer; id: GLuint);
  begin
    ASSERT(ww > 0);
    ASSERT(hh > 0);
    inherited Create(ww, hh);
    self.mID := id;
  end;

  destructor TGLAtlas.Destroy;
  begin
    inherited;
  end;

  function TGLAtlas.CreateNode (): TGLAtlasNode;
  begin
    result := TGLAtlasNode.Create(self);
  end;

  function TGLAtlas.Alloc (ww, hh: Integer): TGLAtlasNode;
  begin
    result := TGLAtlasNode(inherited Alloc(ww, hh));
  end;

  function r_Textures_AllocHWTexture (w, h: Integer): GLuint;
    var id: GLuint;
  begin
    glGenTextures(1, @id);
    if id <> 0 then
    begin
      glBindTexture(GL_TEXTURE_2D, id);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      glBindTexture(GL_TEXTURE_2D, 0);
    end;
    result := id
  end;

  function r_Textures_AllocAtlas (): TGLAtlas;
    var i: Integer; id: GLuint;
  begin
    result := nil;
    id := r_Textures_AllocHWTexture(maxTileSize, maxTileSize);
    if id <> 0 then
    begin
      i := Length(atl);
      SetLength(atl, i + 1);
      atl[i] := TGLAtlas.Create(maxTileSize, maxTileSize, id);
      result := atl[i];
    end;
  end;

  function r_Textures_AllocNode (w, h: Integer): TGLAtlasNode;
    var i: Integer; n: TGLAtlasNode; a: TGLAtlas;
  begin
    n := nil;
    if atl <> nil then
    begin
      i := High(atl);
      while (i >= 0) and (n = nil) do
      begin
        n := atl[i].Alloc(w, h);
        Dec(i);
      end;
    end;
    if n = nil then
    begin
      a := r_Textures_AllocAtlas();
      if a <> nil then
        n := a.Alloc(w, h);
    end;
    result := n
  end;

  (* --------- TGLTexture --------- *)

  destructor TGLTexture.Destroy;
    var i: Integer;
  begin
    if self.mTile <> nil then
    begin
      for i := 0 to High(self.mTile) do
      begin
        if self.mTile[i] <> nil then
        begin
          self.mTile[i].Dealloc;
          self.mTile[i] := nil;
        end;
      end;
      self.mTile := nil;
    end;
    inherited;
  end;

  function TGLTexture.GetLines (): Integer;
  begin
    ASSERT(self.mTile <> nil);
    result := Length(self.mTile) div self.mCols
  end;

  function TGLTexture.GetTile (col, line: Integer): TGLAtlasNode;
    var i: Integer;
  begin
    ASSERT(col >= 0);
    ASSERT(col <= mCols);
    ASSERT(self.mTile <> nil);
    i := line * mCols + col;
    ASSERT(i >= 0);
    ASSERT(i < Length(mTile));
    result := mTile[i];
    ASSERT(result <> nil)
  end;

  function r_Textures_Alloc (w, h: Integer): TGLTexture;
    var x, y, mw, mh, cols, lines: Integer; t: TGLTexture;
  begin
    ASSERT(w > 0);
    ASSERT(h > 0);
    cols := (w + maxTileSize - 1) div maxTileSize;
    lines := (h + maxTileSize - 1) div maxTileSize;
    t := TGLTexture.Create;
    t.mWidth := w;
    t.mHeight := h;
    t.mCols := cols;
    // t.mLines := lines;
    SetLength(t.mTile, cols * lines);
    for y := 0 to lines - 1 do
    begin
      mh := Min(maxTileSize, h - y * maxTileSize);
      ASSERT(mh > 0);
      for x := 0 to cols - 1 do
      begin
        mw := Min(maxTileSize, w - x * maxTileSize);
        ASSERT(mw > 0);
        t.mTile[y * cols + x] := r_Textures_AllocNode(mw, mh);
      end
    end;
    result := t;
  end;

  (* --------- TGLMultiTexture --------- *)

  destructor TGLMultiTexture.Destroy;
    var i: Integer;
  begin
    for i := 0 to self.count - 1 do
      self.mTexture[i].Free;
    self.mTexture := nil;
    inherited;
  end;

  function TGLMultiTexture.GetWidth (): Integer;
  begin
    result := self.mTexture[0].width
  end;

  function TGLMultiTexture.GetHeight (): Integer;
  begin
    result := self.mTexture[0].height
  end;

  function TGLMultiTexture.GetCount (): Integer;
  begin
    result := Length(self.mTexture)
  end;

  function TGLMultiTexture.GetTexture (i: Integer): TGLTexture;
  begin
    ASSERT(i >= 0);
    ASSERT(i < self.count);
    result := self.mTexture[i];
    ASSERT(result <> nil);
  end;

  (* --------- Init / Fin --------- *)

  function r_Textures_GetMaxHardwareSize (): Integer;
    var size: GLint = 0;
  begin
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @size);
    if size < 64 then size := 64;
    //if size > 512 then size := 512;
    //size := 64; // !!!
    result := size;
  end;

  procedure r_Textures_Initialize;
  begin
    maxTileSize := r_Textures_GetMaxHardwareSize();
  end;

  procedure r_Textures_Finalize;
    var i: Integer;
  begin
    if atl <> nil then
    begin
      for i := 0 to High(atl) do
      begin
        glDeleteTextures(1, @atl[i].id);
        atl[i].id := 0;
        atl[i].Free;
      end;
      atl := nil;
    end;
  end;

  function r_Textures_LoadFromImage (var img: TImageData): TGLTexture;
    var t: TGLTexture; n: TGLAtlasNode; c: TDynImageDataArray; cw, ch, i, j: LongInt;
  begin
    // e_logwritefln('r_Textures_CreateFromImage: w=%s h=%s', [img.width, img.height]);
    result := nil;
    if SplitImage(img, c, maxTileSize, maxTileSize, cw, ch, False) then
    begin
      t := r_Textures_Alloc(img.width, img.height);
      if t <> nil then
      begin
        ASSERT(cw = t.cols);
        ASSERT(ch = t.lines);
        for j := 0 to ch - 1 do
        begin
          for i := 0 to cw - 1 do
          begin
            n := t.GetTile(i, j);
            if n <> nil then
              r_Textures_UpdateNode(n, c[j * cw + i].bits, 0, 0, n.width, n.height)
          end
        end;
        result := t
      end;
      FreeImagesInArray(c);
    end;
  end;

  function r_Textures_LoadFromMemory (data: Pointer; size: LongInt): TGLTexture;
    var img: TImageData;
  begin
    result := nil;
    if (data <> nil) and (size > 0) then
    begin
      InitImage(img);
      try
        if LoadImageFromMemory(data, size, img) then
          if ConvertImage(img, TImageFormat.ifA8R8G8B8) then
            if SwapChannels(img, ChannelRed, ChannelBlue) then // wth
              result := r_Textures_LoadFromImage(img)
      except
      end;
      FreeImage(img);
    end;
  end;

  function r_Textures_LoadFromFile (const filename: AnsiString; log: Boolean = True): TGLTexture;
    var wad: TWADFile; wadName, resName: AnsiString; data: Pointer; size: Integer;
  begin
    result := nil;
    wadName := g_ExtractWadName(filename);
    wad := TWADFile.Create();
    if wad.ReadFile(wadName) then
    begin
      resName := g_ExtractFilePathName(filename);
      if wad.GetResource(resName, data, size, log) then
      begin
        result := r_Textures_LoadFromMemory(data, size);
        FreeMem(data);
      end;
      wad.Free
    end
  end;

  function r_Textures_LoadMultiFromImageAndInfo (var img: TImageData; w, h, c: Integer; b: Boolean): TGLMultiTexture;
    var t: TImageData; a: array of TGLTexture; i: Integer; m: TGLMultiTexture;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    ASSERT(c >= 1);
    result := nil;
    SetLength(a, c);
    for i := 0 to c - 1 do
    begin
      InitImage(t);
      if NewImage(w, h, img.Format, t) then
        if CopyRect(img, w * i, 0, w, h, t, 0, 0) then
          a[i] := r_Textures_LoadFromImage(t);
      ASSERT(a[i] <> nil);
      FreeImage(t);
    end;
    m := TGLMultiTexture.Create();
    m.mTexture := a;
    m.mBackanim := b;
    ASSERT(m.mTexture <> nil);
    result := m;
  end;

  function r_Textures_LoadMultiFromDataAndInfo (data: Pointer; size: LongInt; w, h, c: Integer; b: Boolean): TGLMultiTexture;
    var img: TImageData;
  begin
    ASSERT(w > 0);
    ASSERT(h > 0);
    ASSERT(c >= 1);
    result := nil;
    if (data <> nil) and (size > 0) then
    begin
      InitImage(img);
      try
        if LoadImageFromMemory(data, size, img) then
          if ConvertImage(img, TImageFormat.ifA8R8G8B8) then
            if SwapChannels(img, ChannelRed, ChannelBlue) then // wtf
              result := r_Textures_LoadMultiFromImageAndInfo(img, w, h, c, b)
      except
      end;
      FreeImage(img);
    end;
  end;

  function r_Textures_LoadMultiFromWad (wad: TWADFile): TGLMultiTexture;
    var data: Pointer; size: LongInt; TexRes: AnsiString; w, h, c: Integer; b: Boolean; cfg: TConfig; img: TImageData;
  begin
    ASSERT(wad <> nil);
    result := nil;
    if wad.GetResource('TEXT/ANIM', data, size) then
    begin
      cfg := TConfig.CreateMem(data, size);
      FreeMem(data);
      if cfg <> nil then
      begin
        TexRes := cfg.ReadStr('', 'resource', '');
        w := cfg.ReadInt('', 'framewidth', 0);
        h := cfg.ReadInt('', 'frameheight', 0);
        c := cfg.ReadInt('', 'framecount', 0);
        b := cfg.ReadBool('', 'backanim', false);
        if (TexRes <> '') and (w > 0) and (h > 0) and (c > 0) then
        begin
          if wad.GetResource('TEXTURES/' + TexRes, data, size) then
          begin
            InitImage(img);
            try
              if LoadImageFromMemory(data, size, img) then
                if ConvertImage(img, TImageFormat.ifA8R8G8B8) then
                  if SwapChannels(img, ChannelRed, ChannelBlue) then // wtf
                    result := r_Textures_LoadMultiFromImageAndInfo(img, w, h, c, b)
            finally
              FreeMem(data);
            end;
            FreeImage(img);
          end
        end;
        cfg.Free;
      end
    end;
  end;

  function r_Textures_LoadMultiFromMemory (data: Pointer; size: LongInt): TGLMultiTexture;
    var wad: TWADFile; t: TGLTexture; m: TGLMultiTexture;
  begin
    result := nil;
    if (data <> nil) and (size > 0) then
    begin
      t := r_Textures_LoadFromMemory(data, size);
      if t <> nil then
      begin
        m := TGLMultiTexture.Create();
        SetLength(m.mTexture, 1);
        m.mTexture[0] := t;
        m.mBackanim := false;
        result := m;
      end
      else if IsWadData(data, size) then
      begin
        wad := TWADFile.Create();
        if wad.ReadMemory(data, size) then
        begin
          result := r_Textures_LoadMultiFromWad(wad);
          wad.Free;
        end
      end
    end
  end;

  function r_Textures_LoadMultiFromFile (const filename: AnsiString; log: Boolean = True): TGLMultiTexture;
    var wad: TWADFile; wadName, resName: AnsiString; data: Pointer; size: Integer; t: TGLTexture;
  begin
    result := nil;
    wadName := g_ExtractWadName(filename);
    wad := TWADFile.Create();
    if wad.ReadFile(wadName) then
    begin
      resName := g_ExtractFilePathName(filename);
      if wad.GetResource(resName, data, size, log) then
      begin
        result := r_Textures_LoadMultiFromMemory(data, size);
        FreeMem(data);
      end;
      wad.Free
    end
  end;

  function r_Textures_LoadMultiFromFileAndInfo (const filename: AnsiString; w, h, count: Integer; backanim: Boolean; log: Boolean = True): TGLMultiTexture;
    var wad: TWADFile; wadName, resName: AnsiString; data: Pointer; size: Integer;
  begin
    ASSERT(w > 0);
    ASSERT(h > 0);
    ASSERT(count >= 1);
    result := nil;
    wadName := g_ExtractWadName(filename);
    wad := TWADFile.Create();
    if wad.ReadFile(wadName) then
    begin
      resName := g_ExtractFilePathName(filename);
      if wad.GetResource(resName, data, size, log) then
      begin
        result := r_Textures_LoadMultiFromDataAndInfo(data, size, w, h, count, backanim);
        FreeMem(data);
      end;
      wad.Free
    end
  end;

end.
