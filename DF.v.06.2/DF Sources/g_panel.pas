Unit g_panel;

Interface

Uses
  MAPSTRUCT, BinEditor, g_textures;

Type
  TAddTextureArray = Array of
    record
      Texture: Cardinal;
      Anim: Boolean;
    end;

  TPanel = Class (TObject)
  Private
    FTextureWidth:    Word;
    FTextureHeight:   Word;
    FAlpha:           Byte;
    FBlending:        Boolean;
    FCurTexture:      Integer; // Номер текущей текстуры
    FTextureIDs:      Array of
                        record
                          case Anim: Boolean of
                            False: (Tex: Cardinal);
                            True:  (AnTex: TAnimation);
                        end;

  Public
    X, Y:             Integer;
    Width, Height:    Word;
    PanelType:        Word;
    SaveIt:           Boolean; // Сохранять при SaveState?
    Enabled:          Boolean;
    Door:             Boolean;
    LiftType:         Byte;

    constructor Create(PanelRec: TPanelRec_1;
                       AddTextures: TAddTextureArray;
                       CurTex: Integer;
                       var Textures: TLevelTextureArray);
    destructor  Destroy(); override;

    procedure   Draw();
    procedure   Update();
    procedure   NextTexture(AnimLoop: Byte = 0);
    function    GetTextureID(): Cardinal;

    procedure   SaveState(var Mem: TBinMemoryWriter);
    procedure   LoadState(var Mem: TBinMemoryReader);
  end;

  TPanelArray = Array of TPanel;

Implementation

Uses
  windows, g_basic, g_map, MAPDEF, g_game, e_graphics,
  g_console, g_language;

Const
  PANEL_SIGNATURE = $4C4E4150; // 'PANL'

{ T P a n e l : }

constructor TPanel.Create(PanelRec: TPanelRec_1;
                          AddTextures: TAddTextureArray;
                          CurTex: Integer;
                          var Textures: TLevelTextureArray);
var
  i: Integer;

begin
  X := PanelRec.X;
  Y := PanelRec.Y;
  Width := PanelRec.Width;
  Height := PanelRec.Height;
  FAlpha := 0;
  FBlending := False;

// Тип панели:
  PanelType := PanelRec.PanelType;
  Enabled := True;
  Door := False;
  LiftType := 0;
  SaveIt := False;

  case PanelType of
    PANEL_OPENDOOR:
      begin
        Enabled := False;
        Door := True;
        SaveIt := True;
      end;
    PANEL_CLOSEDOOR:
      begin
        Door := True;
        SaveIt := True;
      end;
    PANEL_LIFTUP:
      SaveIt := True;
    PANEL_LIFTDOWN:
      begin
        LiftType := 1;
        SaveIt := True;
      end;
  end;

// Невидимая:
  if ByteBool(PanelRec.Flags and PANEL_FLAG_HIDE) then
  begin
    SetLength(FTextureIDs, 0);
    FCurTexture := -1;
    Exit;
  end;

// Если это жидкость без текстуры - спецтекстуру:
  if WordBool(PanelType and (PANEL_WATER or PANEL_ACID1 or PANEL_ACID2)) and
     (not ByteBool(PanelRec.Flags and PANEL_FLAG_WATERTEXTURES)) then
  begin
    SetLength(FTextureIDs, 1);
    FTextureIDs[0].Anim := False;

    case PanelRec.PanelType of
      PANEL_WATER:
        FTextureIDs[0].Tex := TEXTURE_SPECIAL_WATER;
      PANEL_ACID1:
        FTextureIDs[0].Tex := TEXTURE_SPECIAL_ACID1;
      PANEL_ACID2:
        FTextureIDs[0].Tex := TEXTURE_SPECIAL_ACID2;
    end;

    FCurTexture := 0;
    Exit;
  end;

  SetLength(FTextureIDs, Length(AddTextures));
  
  if CurTex < 0 then
    FCurTexture := -1
  else
    if CurTex >= Length(FTextureIDs) then
      FCurTexture := Length(FTextureIDs) - 1
    else
      FCurTexture := CurTex;

  for i := 0 to Length(FTextureIDs)-1 do
  begin
    FTextureIDs[i].Anim := AddTextures[i].Anim;
    if FTextureIDs[i].Anim then
      begin // Анимированная текстура
        FTextureIDs[i].AnTex :=
           TAnimation.Create(Textures[AddTextures[i].Texture].FramesID,
                             True, Textures[AddTextures[i].Texture].Speed);
        FTextureIDs[i].AnTex.Blending := ByteBool(PanelRec.Flags and PANEL_FLAG_BLENDING);
        FTextureIDs[i].AnTex.Alpha := PanelRec.Alpha;
        SaveIt := True;
      end
    else
      begin // Обычная текстура
        FTextureIDs[i].Tex := Textures[AddTextures[i].Texture].TextureID;
      end;
  end;

// Текстур несколько - нужно сохранять текущую:
  if Length(FTextureIDs) > 1 then
    SaveIt := True;

// Если не спецтекстура, то задаем размеры:
  if not g_Map_IsSpecialTexture(Textures[PanelRec.TextureNum].TextureName) then
  begin
    FTextureWidth := Textures[PanelRec.TextureNum].Width;
    FTextureHeight := Textures[PanelRec.TextureNum].Height;
    FAlpha := PanelRec.Alpha;
    FBlending := ByteBool(PanelRec.Flags and PANEL_FLAG_BLENDING);
  end;
end;

destructor TPanel.Destroy();
var
  i: Integer;

begin
  for i := 0 to High(FTextureIDs) do
    if FTextureIDs[i].Anim then
      FTextureIDs[i].AnTex.Free();
  SetLength(FTextureIDs, 0);

  Inherited;
end;

procedure TPanel.Draw();
var
  xx, yy: Integer;

begin
  if Enabled and (FCurTexture >= 0) and
     (Width > 0) and (Height > 0) and (FAlpha < 255) and
     g_Collide(X, Y, Width, Height,
               sX, sY, sWidth, sHeight) then
  begin
    if FTextureIDs[FCurTexture].Anim then
      begin // Анимированная текстура
        if FTextureIDs[FCurTexture].AnTex = nil then
          Exit;
  
        for xx := 0 to (Width div FTextureWidth)-1 do
          for yy := 0 to (Height div FTextureHeight)-1 do
            FTextureIDs[FCurTexture].AnTex.Draw(
              X + xx*FTextureWidth,
              Y + yy*FTextureHeight, M_NONE);
      end
    else
      begin // Обычная текстура
        case FTextureIDs[FCurTexture].Tex of
          TEXTURE_SPECIAL_WATER:
            e_DrawFillQuad(X, Y, X+Width-1, Y+Height-1,
                           0, 0, 255, 0, B_FILTER);
          TEXTURE_SPECIAL_ACID1:
            e_DrawFillQuad(X, Y, X+Width-1, Y+Height-1,
                           0, 128, 0, 0, B_FILTER);
          TEXTURE_SPECIAL_ACID2:
            e_DrawFillQuad(X, Y, X+Width-1, Y+Height-1,
                           128, 0, 0, 0, B_FILTER);
          TEXTURE_NONE:
            begin
              xx := X + (Width div 2);
              yy := Y + (Height div 2);
              e_DrawFillQuad(X, Y, xx, yy,
                             255, 0, 255, 0);
              e_DrawFillQuad(xx, Y, X+Width-1, yy,
                             255, 255, 0, 0);
              e_DrawFillQuad(X, yy, xx, Y+Height-1,
                             255, 255, 0, 0);
              e_DrawFillQuad(xx, yy, X+Width-1, Y+Height-1,
                             255, 0, 255, 0);
            end;
            
          else
            e_DrawFill(FTextureIDs[FCurTexture].Tex, X, Y,
                       Width div FTextureWidth,
                       Height div FTextureHeight,
                       FAlpha, True, FBlending);
        end;
      end;
  end;
end;

procedure TPanel.Update();
begin
  if Enabled and (FCurTexture >= 0) and
     (FTextureIDs[FCurTexture].Anim) and
     (FTextureIDs[FCurTexture].AnTex <> nil) and
     (Width > 0) and (Height > 0) and (FAlpha < 255) then
    FTextureIDs[FCurTexture].AnTex.Update();
end;

procedure TPanel.NextTexture(AnimLoop: Byte = 0);
begin
  Assert(FCurTexture >= -1, 'FCurTexture < -1');

// Нет текстур:
  if Length(FTextureIDs) = 0 then
    FCurTexture := -1
  else
  // Только одна текстура:
    if Length(FTextureIDs) = 1 then
      begin
        if FCurTexture = 0 then
          FCurTexture := -1
        else
          FCurTexture := 0;
      end
    else
    // Больше одной текстуры:
      begin
      // Следующая:
        Inc(FCurTexture);
      // Следующей нет - возврат к началу:
        if FCurTexture >= Length(FTextureIDs) then
          FCurTexture := 0;
      end;

// Переключились на видимую аним. текстуру:
  if (FCurTexture >= 0) and FTextureIDs[FCurTexture].Anim then
  begin
    if (FTextureIDs[FCurTexture].AnTex = nil) then
    begin
      g_FatalError(_lc[I_GAME_ERROR_SWITCH_TEXTURE]);
      Exit;
    end;

    if AnimLoop = 1 then
      FTextureIDs[FCurTexture].AnTex.Loop := True
    else
      if AnimLoop = 2 then
        FTextureIDs[FCurTexture].AnTex.Loop := False;
        
    FTextureIDs[FCurTexture].AnTex.Reset();
  end;
end;

function TPanel.GetTextureID(): DWORD;
begin
  Result := TEXTURE_NONE;

  if (FCurTexture >= 0) then
  begin
    if FTextureIDs[FCurTexture].Anim then
      Result := FTextureIDs[FCurTexture].AnTex.FramesID
    else
      Result := FTextureIDs[FCurTexture].Tex;
  end;
end;

procedure TPanel.SaveState(Var Mem: TBinMemoryWriter);
var
  sig: DWORD;
  anim: Boolean;

begin
  if (not SaveIt) or (Mem = nil) then
    Exit;

// Сигнатура панели:
  sig := PANEL_SIGNATURE; // 'PANL'
  Mem.WriteDWORD(sig);
// Открыта/закрыта, если дверь:
  Mem.WriteBoolean(Enabled);
// Направление лифта, если лифт:
  Mem.WriteByte(LiftType);
// Номер текущей текстуры:
  Mem.WriteInt(FCurTexture);
// Анимированная ли текущая текстура:
  if (FCurTexture >= 0) and (FTextureIDs[FCurTexture].Anim) then
    begin
      Assert(FTextureIDs[FCurTexture].AnTex <> nil,
             'TPanel.SaveState: No animation object');
      anim := True;
    end
  else
    anim := False;
  Mem.WriteBoolean(anim);
// Если да - сохраняем анимацию:
  if anim then
    FTextureIDs[FCurTexture].AnTex.SaveState(Mem);
end;

procedure TPanel.LoadState(var Mem: TBinMemoryReader);
var
  sig: DWORD;
  anim: Boolean;

begin
  if (not SaveIt) or (Mem = nil) then
    Exit;

// Сигнатура панели:
  Mem.ReadDWORD(sig);
  if sig <> PANEL_SIGNATURE then // 'PANL'
  begin
    raise EBinSizeError.Create('TPanel.LoadState: Wrong Panel Signature');
  end;
// Открыта/закрыта, если дверь:
  Mem.ReadBoolean(Enabled);
// Направление лифта, если лифт:
  Mem.ReadByte(LiftType);
// Номер текущей текстуры:
  Mem.ReadInt(FCurTexture);
// Анимированная ли текущая текстура:
  Mem.ReadBoolean(anim);
// Если да - загружаем анимацию:
  if anim then
  begin
    Assert((FCurTexture >= 0) and
           (FTextureIDs[FCurTexture].Anim) and
           (FTextureIDs[FCurTexture].AnTex <> nil),
           'TPanel.LoadState: No animation object');
    FTextureIDs[FCurTexture].AnTex.LoadState(Mem);
  end;
end;


End.
