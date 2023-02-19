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
    {$I ../../../nogl/noGLuses.inc}
    g_base, g_animations,  // TRectHW, TAnimInfo
    utils,
    r_atlas, r_fonts
  ;

  type
    TGLHints = (txNoRepeat);
    TGLHintsSet = set of TGLHints;

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
        mHints: TGLHintsSet;

      public
        destructor Destroy; override;

        function GetTile (col, line: Integer): TGLAtlasNode;

        function GetLines (): Integer; inline;

        property width: Integer read mWidth;
        property height: Integer read mHeight;
        property cols: Integer read mCols;
        property lines: Integer read GetLines;
        property hints: TGLHintsSet read mHints;
    end;

    TGLMultiTexture = class
      private
        mTexture: array of TGLTexture;

      public
        destructor Destroy; override;

        function GetWidth (): Integer; inline;
        function GetHeight (): Integer; inline;
        function GetCount (): Integer; inline;
        function GetTexture (i: Integer): TGLTexture; {inline;}

        property width: Integer read GetWidth;
        property height: Integer read GetHeight;
        property count: Integer read GetCount;
    end;

    TGLTextureArray = array of TGLTexture;

    TRectArray = array of TRectWH;

    TGLFont = class sealed (TFont)
      private
        info: TFontInfo;
        ch: TGLTextureArray;

      public
        destructor Destroy; override;
        function GetChar (c: AnsiChar): TGLTexture;
        function GetWidth (c: AnsiChar): Integer;
        function GetMaxWidth (): Integer;
        function GetMaxHeight (): Integer;
        function GetSpace (): Integer;
    end;

    TAnimTextInfo = record
      name: AnsiString;
      w, h: Integer;
      anim: TAnimInfo;
    end;

    TConvProc = function (x: Integer): Integer;

  procedure r_Textures_Initialize;
  procedure r_Textures_Finalize;

  function r_Textures_LoadFromFile (const filename: AnsiString; hints: TGLHintsSet; log: Boolean = True): TGLTexture;
  function r_Textures_LoadMultiFromFile (const filename: AnsiString; hints: TGLHintsSet; log: Boolean = True): TGLMultiTexture;
  function r_Textures_LoadMultiFromFileAndInfo (const filename: AnsiString; w, h, count: Integer; hints: TGLHintsSet; log: Boolean = True): TGLMultiTexture;
  function r_Textures_LoadMultiTextFromFile (const filename: AnsiString; var txt: TAnimTextInfo; hints: TGLHintsSet; log: Boolean = True): TGLMultiTexture;

  function r_Textures_LoadStreamFromFile (const filename: AnsiString; w, h, count, cw: Integer; st: TGLTextureArray; rs: TRectArray; hints: TGLHintsSet; log: Boolean = True): Boolean;

  function r_Textures_LoadFontFromFile (const filename: AnsiString; constref f: TFontInfo; font2enc: TConvProc; log: Boolean = true): TGLFont;

  procedure r_Textures_GL_Bind (id: GLuint);

implementation

  uses
    SysUtils, Classes,
    r_common,
    e_log, e_res, WADReader, Config,
    g_console, // cvar declaration
    Imaging, ImagingTypes, ImagingUtility
  ;

  var
    r_GL_MaxTexSize: WORD;
    r_GL_RepeatOpt: Boolean;
    maxTileSize: Integer;
    atl, ratl: array of TGLAtlas;
    currentTexture2D: GLuint;

  procedure r_Textures_GL_Bind (id: GLuint);
  begin
    if id <> currentTexture2D then
    begin
      glBindTexture(GL_TEXTURE_2D, id);
      currentTexture2D := id;
    end
  end;

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
    r_Textures_GL_Bind(n.id);
    glTexSubImage2D(GL_TEXTURE_2D, 0, n.l + x, n.t + y, w, h, GL_RGBA, GL_UNSIGNED_BYTE, data);
    r_Textures_GL_Bind(0);
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
      r_Textures_GL_Bind(id);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      r_Textures_GL_Bind(0);
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

  function r_Textures_AllocRepeatAtlas (w, h: Integer): TGLAtlas;
    var i: Integer; id: GLuint;
  begin
    result := nil;
    id := r_Textures_AllocHWTexture(w, h);
    if id <> 0 then
    begin
      i := Length(ratl);
      SetLength(ratl, i + 1);
      ratl[i] := TGLAtlas.Create(w, h, id);
      result := ratl[i];
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

  function r_Textures_AllocRepeatNode (w, h: Integer): TGLAtlasNode;
    var i: Integer; n: TGLAtlasNode; a: TGLAtlas;
  begin
    n := nil; a := nil;
    if ratl <> nil then
    begin
      i := High(ratl);
      while (i >= 0) and (ratl[i] <> nil) do DEC(i);
      if i >= 0 then a := ratl[i];
    end;
    if a = nil then a := r_Textures_AllocRepeatAtlas(w, h);
    if a <> nil then
    begin
      n := a.Alloc(w, h);
      if n = nil then
      begin
        i := High(ratl); while (i >= 0) and (ratl[i] <> a) do DEC(i);
        if i >= 0 then ratl[i] := nil;
        r_Common_FreeAndNil(a);
      end;
    end;
    result := n
  end;

  (* --------- TGLTexture --------- *)

  destructor TGLTexture.Destroy;
    var i: Integer; a: TGLAtlas;
  begin
    if self.mTile <> nil then
    begin
      if TGLHints.txNoRepeat in self.hints then (* non repeatable texture -> delete tiles only *)
      begin
        for i := 0 to High(self.mTile) do
        begin
          if self.mTile[i] <> nil then
          begin
            self.mTile[i].Dealloc;
            self.mTile[i] := nil
          end
        end
      end
      else (* repeatable texture -> delete whole atlas *)
      begin
        a := self.mTile[0].base;
        i := High(ratl); while (i >= 0) and (ratl[i] <> a) do DEC(i);
        if i >= 0 then ratl[i] := nil;
        r_Common_FreeAndNil(a);
      end;
      SetLength(self.mTile, 0);
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

  function r_Textures_Alloc (w, h: Integer; hints: TGLHintsSet): TGLTexture;
    var x, y, mw, mh, cols, lines: Integer; t: TGLTexture;
  begin
    ASSERT(w > 0);
    ASSERT(h > 0);
    if TGLHints.txNoRepeat in hints then
    begin
      cols := (w + maxTileSize - 1) div maxTileSize;
      lines := (h + maxTileSize - 1) div maxTileSize;
      t := TGLTexture.Create;
      t.mWidth := w;
      t.mHeight := h;
      t.mCols := cols;
      // t.mLines := lines;
      t.mHints := hints;
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
    end
    else
    begin
      t := TGLTexture.Create;
      t.mWidth := w;
      t.mHeight := h;
      t.mCols := 1;
      // t.mLines := 1
      t.mHints := hints;
      SetLength(t.mTile, 1);
      t.mTile[0] := r_Textures_AllocRepeatNode(w, h);
    end;
    result := t;
  end;

  (* --------- TGLMultiTexture --------- *)

  destructor TGLMultiTexture.Destroy;
    var i: Integer;
  begin
    for i := 0 to self.count - 1 do
      r_Common_FreeAndNil(self.mTexture[i]);
    SetLength(self.mTexture, 0);
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

  function IsPOT (v: LongWord): Boolean;
  begin
    result := (v <> 0) and ((v and (v - 1)) = 0)
  end;

  function NextPOT (v: LongWord): LongWord;
  begin
    DEC(v);
    v := v or (v >> 1);
    v := v or (v >> 2);
    v := v or (v >> 4);
    v := v or (v >> 8);
    v := v or (v >> 16);
    INC(v);
    result := v;
  end;

  function r_Textures_GetMaxHardwareSize (): Integer;
    var size: GLint = 0;
  begin
    if r_GL_MaxTexSize <= 0 then
    begin
      // auto, max possible reccomended by driver
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @size);
      size := size div 2; (* hack: on some devices max size may produce invalid texture *)
      if size < 64 then size := 64; (* at least 64x64 are guarantied by specification *)
    end
    else
    begin
      // selected by user
      if IsPOT(r_GL_MaxTexSize) then
        size := r_GL_MaxTexSize
      else
        size := NextPOT(r_GL_MaxTexSize);
    end;
    result := size;
  end;

  procedure r_Textures_Initialize;
  begin
    currentTexture2D := 0;
    maxTileSize := r_Textures_GetMaxHardwareSize();
    e_LogWritefln('Texture Tile Size: %s', [maxTileSize]);
  end;

  procedure r_Textures_Finalize;
    var i: Integer;
  begin
    if atl <> nil then
    begin
      for i := 0 to High(atl) do
      begin
        if atl[i] <> nil then
        begin
          glDeleteTextures(1, @atl[i].id);
          atl[i].id := 0;
          r_Common_FreeAndNil(atl[i]);
        end;
      end;
    end;
    SetLength(atl, 0);

    if ratl <> nil then
    begin
      for i := 0 to High(ratl) do
      begin
        if ratl[i] <> nil then
        begin
          glDeleteTextures(1, @ratl[i].id);
          ratl[i].id := 0;
          r_Common_FreeAndNil(ratl[i]);
        end;
      end;
    end;
    SetLength(ratl, 0);
  end;

  function r_Textures_FixImageData (var img: TImageData): Boolean;
  begin
    result := false;
    if ConvertImage(img, TImageFormat.ifA8R8G8B8) then
      if SwapChannels(img, ChannelRed, ChannelBlue) then // wtf
        result := true;
  end;

  function r_Textures_ValidRepeatTexture (w, h: Integer; hints: TGLHintsSet): Boolean;
  begin
    result := r_GL_RepeatOpt and
              not (TGLHints.txNoRepeat in hints) and
              (w <= maxTileSize) and
              (h <= maxTileSize) and
              IsPOT(w) and
              IsPOT(h)
  end;

  function r_Textures_LoadFromImage (var img: TImageData; hints: TGLHintsSet): TGLTexture; // !!!
    var t: TGLTexture; n: TGLAtlasNode; c: TDynImageDataArray; cw, ch, i, j: LongInt;
  begin
    result := nil;
    if r_Textures_ValidRepeatTexture(img.width, img.height, hints) then
    begin
      t := r_Textures_Alloc(img.width, img.height, hints - [TGLHints.txNoRepeat]);
      if t <> nil then
      begin
        n := t.GetTile(0, 0);
        ASSERT(n <> nil);
        r_Textures_UpdateNode(n, img.bits, 0, 0, n.width, n.height);
        result := t
      end
    end
    else if SplitImage(img, c, maxTileSize, maxTileSize, cw, ch, False) then
    begin
      t := r_Textures_Alloc(img.width, img.height, hints + [TGLHints.txNoRepeat]);
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

  function r_Textures_LoadFromMemory (data: Pointer; size: LongInt; hints: TGLHintsSet): TGLTexture;
    var img: TImageData;
  begin
    result := nil;
    if (data <> nil) and (size > 0) then
    begin
      InitImage(img);
      try
        if LoadImageFromMemory(data, size, img) then
          if r_Textures_FixImageData(img) then
            result := r_Textures_LoadFromImage(img, hints)
      except
      end;
      FreeImage(img);
    end;
  end;

  function r_Textures_LoadFromFile (const filename: AnsiString; hints: TGLHintsSet; log: Boolean = True): TGLTexture;
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
        result := r_Textures_LoadFromMemory(data, size, hints);
        FreeMem(data);
      end;
      wad.Free
    end
  end;

  function r_Textures_LoadMultiFromImageAndInfo (var img: TImageData; w, h, c: Integer; hints: TGLHintsSet): TGLMultiTexture;
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
          a[i] := r_Textures_LoadFromImage(t, hints);
      ASSERT(a[i] <> nil);
      FreeImage(t);
    end;
    m := TGLMultiTexture.Create();
    m.mTexture := a;
    ASSERT(m.mTexture <> nil);
    result := m;
  end;

  function r_Textures_LoadMultiFromDataAndInfo (data: Pointer; size: LongInt; w, h, c: Integer; hints: TGLHintsSet): TGLMultiTexture;
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
          if r_Textures_FixImageData(img) then
            result := r_Textures_LoadMultiFromImageAndInfo(img, w, h, c, hints)
      except
      end;
      FreeImage(img);
    end;
  end;

  function r_Textures_LoadTextFromMemory (data: Pointer; size: LongInt; var text: TAnimTextInfo): Boolean;
    var cfg: TConfig;
  begin
    result := false;
    if data <> nil then
    begin
      cfg := TConfig.CreateMem(data, size);
      if cfg <> nil then
      begin
        text.name := cfg.ReadStr('', 'resource', '');
        text.w := cfg.ReadInt('', 'framewidth', 0);
        text.h := cfg.ReadInt('', 'frameheight', 0);
        text.anim.loop := true;
        text.anim.delay := cfg.ReadInt('', 'waitcount', 0);
        text.anim.frames := cfg.ReadInt('', 'framecount', 0);
        text.anim.back := cfg.ReadBool('', 'backanim', false);
        if text.w <= 0 then e_LogWritefln('Warning: bad animation width %s for %s', [text.w, text.name]);
        if text.h <= 0 then e_LogWritefln('Warning: bad animation height %s for %s', [text.h, text.name]);
        if text.anim.delay <= 0 then e_LogWritefln('Warning: bad animation delay %s for %s', [text.anim.delay, text.name]);
        if text.anim.frames <= 0 then e_LogWritefln('Warning: bad animation frame count %s for %s', [text.anim.frames, text.name]);
        text.w := MAX(0, text.w);
        text.h := MAX(0, text.h);
        text.anim.delay := MAX(1, text.anim.delay);
        text.anim.frames := MAX(1, text.anim.frames);
        cfg.Free;
        result := (text.name <> '') and (text.w > 0) and (text.h > 0) and (text.anim.delay > 0) and (text.anim.frames > 0);
      end;
    end;
  end;

  function r_Textures_LoadMultiFromWad (wad: TWADFile; var txt: TAnimTextInfo; hints: TGLHintsSet): TGLMultiTexture;
    var data: Pointer; size: LongInt; img: TImageData;
  begin
    ASSERT(wad <> nil);
    result := nil;
    if wad.GetResource('TEXT/ANIM', data, size) then
    begin
      if r_Textures_LoadTextFromMemory(data, size, txt) then
      begin
        FreeMem(data);
        if wad.GetResource('TEXTURES/' + txt.name, data, size) then
        begin
          InitImage(img);
          try
            if LoadImageFromMemory(data, size, img) then
              if r_Textures_FixImageData(img) then
                result := r_Textures_LoadMultiFromImageAndInfo(img, txt.w, txt.h, txt.anim.frames, hints);
          finally
            FreeMem(data);
          end;
          FreeImage(img);
        end;
      end
      else
        FreeMem(data);
    end;
  end;

  function r_Textures_LoadMultiFromMemory (data: Pointer; size: LongInt; var txt: TAnimTextInfo; hints: TGLHintsSet): TGLMultiTexture;
    var wad: TWADFile; t: TGLTexture; m: TGLMultiTexture;
  begin
    result := nil;
    if (data <> nil) and (size > 0) then
    begin
      t := r_Textures_LoadFromMemory(data, size, hints);
      if t <> nil then
      begin
        m := TGLMultiTexture.Create();
        SetLength(m.mTexture, 1);
        m.mTexture[0] := t;
        txt.name := '';
        txt.w := m.width;
        txt.h := m.height;
        txt.anim.loop := true;
        txt.anim.delay := 1;
        txt.anim.frames := 1;
        txt.anim.back := false;
        result := m;
      end
      else if IsWadData(data, size) then
      begin
        wad := TWADFile.Create();
        if wad.ReadMemory(data, size) then
        begin
          result := r_Textures_LoadMultiFromWad(wad, txt, hints);
          wad.Free;
        end
      end
    end
  end;

  function r_Textures_LoadMultiTextFromFile (const filename: AnsiString; var txt: TAnimTextInfo; hints: TGLHintsSet; log: Boolean = True): TGLMultiTexture;
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
        result := r_Textures_LoadMultiFromMemory(data, size, txt, hints);
        FreeMem(data);
      end;
      wad.Free
    end
  end;

  function r_Textures_LoadMultiFromFile (const filename: AnsiString; hints: TGLHintsSet; log: Boolean = True): TGLMultiTexture;
    var txt: TAnimTextInfo;
  begin
    result := r_Textures_LoadMultiTextFromFile(filename, txt, hints, log);
  end;

  function r_Textures_LoadMultiFromFileAndInfo (const filename: AnsiString; w, h, count: Integer; hints: TGLHintsSet; log: Boolean = True): TGLMultiTexture;
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
        result := r_Textures_LoadMultiFromDataAndInfo(data, size, w, h, count, hints);
        FreeMem(data);
      end;
      wad.Free
    end
  end;

  function r_Textures_GetRect (var img: TImageData): TRectWH;
    var i, j, w, h: Integer; done: Boolean;

    function IsVoid (i, j: Integer): Boolean; inline;
    begin
      result := GetPixel32(img, i, j).Channels[3] = 0
    end;

  begin
    w := img.Width;
    h := img.Height;

    (* trace x from right to left *)
    done := false; i := 0;
    while not done and (i < w) do
    begin
      j := 0;
      while (j < h) and IsVoid(i, j) do inc(j);
      done := (j < h) and (IsVoid(i, j) = false);
      result.x := i;
      inc(i);
    end;

    (* trace y from up to down *)
    done := false; j := 0;
    while not done and (j < h) do
    begin
      i := 0;
      while (i < w) and IsVoid(i, j) do inc(i);
      done := (i < w) and (IsVoid(i, j) = false);
      result.y := j;
      inc(j);
    end;

    (* trace x from right to left *)
    done := false; i := w - 1;
    while not done and (i >= 0) do
    begin
      j := 0;
      while (j < h) and IsVoid(i, j) do inc(j);
      done := (j < h) and (IsVoid(i, j) = false);
      result.width := i - result.x + 1;
      dec(i);
    end;

    (* trace y from down to up *)
    done := false; j := h - 1;
    while not done and (j >= 0) do
    begin
      i := 0;
      while (i < w) and IsVoid(i, j) do inc(i);
      done := (i < w) and (IsVoid(i, j) = false);
      result.height := j - result.y + 1;
      dec(j);
    end;
  end;

  function r_Textures_LoadStreamFromImage (var img: TImageData; w, h, c, cw: Integer; st: TGLTextureArray; rs: TRectArray; hints: TGLHintsSet): Boolean;
    var i, x, y: Integer; t: TImageData;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    ASSERT(c >= 1);
    ASSERT(cw >= 1);
    ASSERT((st <> nil) and (Length(st) >= c));
    ASSERT((rs = nil) or (Length(rs) >= c));
    result := true;
    for i := 0 to c - 1 do
    begin
      x := i mod cw;
      y := i div cw;
      InitImage(t);
      st[i] := nil;
      if NewImage(w, h, img.Format, t) then
      begin
        if CopyRect(img, x * w, y * h, w, h, t, 0, 0) then
        begin
          if rs <> nil then
            rs[i] := r_Textures_GetRect(t);
          st[i] := r_Textures_LoadFromImage(t, hints);
        end;
      end;
      ASSERT(st[i] <> nil);
      FreeImage(t);
    end;
  end;

  function r_Textures_LoadStreamFromMemory (data: Pointer; size: LongInt; w, h, c, cw: Integer; st: TGLTextureArray; rs: TRectArray; hints: TGLHintsSet): Boolean;
    var img: TImageData;
  begin
    ASSERT(w >= 0);
    ASSERT(h >= 0);
    ASSERT(c >= 1);
    ASSERT(cw >= 1);
    ASSERT((st <> nil) and (Length(st) >= c));
    ASSERT((rs = nil) or (Length(rs) >= c));
    result := false;
    if (data <> nil) and (size > 0) then
    begin
      InitImage(img);
      try
        if LoadImageFromMemory(data, size, img) then
        begin
          if r_Textures_FixImageData(img) then
          begin
            result := r_Textures_LoadStreamFromImage(img, w, h, c, cw, st, rs, hints)
          end;
        end;
      except
      end;
      FreeImage(img);
    end;
  end;

  function r_Textures_LoadStreamFromFile (const filename: AnsiString; w, h, count, cw: Integer; st: TGLTextureArray; rs: TRectArray; hints: TGLHintsSet; log: Boolean = True): Boolean;
    var wad: TWADFile; wadName, resName: AnsiString; data: Pointer; size: Integer;
  begin
    ASSERT(w > 0);
    ASSERT(h > 0);
    ASSERT(count >= 1);
    ASSERT(cw >= 1);
    ASSERT((st <> nil) and (Length(st) >= count));
    ASSERT((rs = nil) or (Length(rs) >= count));
    result := false;
    wadName := g_ExtractWadName(filename);
    wad := TWADFile.Create();
    if wad.ReadFile(wadName) then
    begin
      resName := g_ExtractFilePathName(filename);
      if wad.GetResource(resName, data, size, log) then
      begin
        result := r_Textures_LoadStreamFromMemory(data, size, w, h, count, cw, st, rs, hints);
        FreeMem(data);
      end;
      wad.Free
    end;
  end;

  (* --------- TGLFont --------- *)

  function r_Textures_LoadFontFromFile (const filename: AnsiString; constref f: TFontInfo; font2enc: TConvProc; log: Boolean = true): TGLFont;
    var i, ch: Integer; st, stch: TGLTextureArray; font: TGLFont;
  begin
    result := nil;
    SetLength(st, 256);
    if r_Textures_LoadStreamFromFile(filename, f.w, f.h, 256, 16, st, nil, [TGLHints.txNoRepeat], log) then
    begin
      font := TGLFont.Create();
      font.info := f;
      font.ch := st;
      if Assigned(font2enc) then
      begin
        SetLength(stch, 256);
        for i := 0 to 255 do
        begin
          ch := font2enc(i);
          ASSERT((ch >= 0) and (ch <= 255));
          stch[ch] := st[i];
        end;
        font.ch := stch;
        SetLength(st, 0);
      end;
      result := font;
    end;
  end;

  destructor TGLFont.Destroy;
    var i: Integer;
  begin
    if self.ch <> nil then
      for i := 0 to High(self.ch) do
        self.ch[i].Free;
    self.ch := nil;
  end;

  function TGLFont.GetChar (c: AnsiChar): TGLTexture;
  begin
    result := self.ch[ORD(c)];
  end;

  function TGLFont.GetWidth (c: AnsiChar): Integer;
  begin
    result := self.info.ch[c].w;
    if result = 0 then
      result := self.info.w;
    if self.info.kern < 0 then
      result := result + self.info.kern;
  end;

  function TGLFont.GetMaxWidth (): Integer;
  begin
    result := self.info.w;
    if self.info.kern < 0 then
      result := result + self.info.kern;
  end;

  function TGLFont.GetMaxHeight (): Integer;
  begin
    result := self.info.h;
  end;

  function TGLFont.GetSpace (): Integer;
  begin
    result := self.info.kern;
  end;

initialization
  conRegVar('r_gl_maxtexsize', @r_GL_MaxTexSize, '', '');
  conRegVar('r_gl_repeat', @r_GL_RepeatOpt, '', '');
  r_GL_MaxTexSize := 0; // default is automatic value
  r_GL_RepeatOpt := true;
end.
