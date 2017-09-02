(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
{$M+}
unit g_panel;

interface

uses
  MAPDEF, BinEditor, g_textures, xdynrec;

type
  TAddTextureArray = Array of
    record
      Texture: Cardinal;
      Anim: Boolean;
    end;

  TPanel = Class (TObject)
  private
    const
  private
    mGUID: Integer; // will be assigned in "g_map.pas"
    FTextureWidth:    Word;
    FTextureHeight:   Word;
    FAlpha:           Byte;
    FBlending:        Boolean;
    FTextureIDs:      Array of
                        record
                          case Anim: Boolean of
                            False: (Tex: Cardinal);
                            True:  (AnTex: TAnimation);
                        end;

    mMovingSpeed: TDFPoint;
    mMovingStart: TDFPoint;
    mMovingEnd: TDFPoint;
    mMovingActive: Boolean;

  private
    function getx1 (): Integer; inline;
    function gety1 (): Integer; inline;
    function getvisvalid (): Boolean; inline;

  public
    FCurTexture:      Integer; // Номер текущей текстуры
    FCurFrame:        Integer;
    FCurFrameCount:   Byte;
    FX, FY:           Integer;
    FWidth, FHeight:  Word;
    FPanelType:       Word;
    FSaveIt:          Boolean; // Сохранять при SaveState?
    FEnabled:         Boolean;
    FDoor:            Boolean;
    FMoved:           Boolean;
    FLiftType:        Byte;
    FLastAnimLoop:    Byte;
    // sorry, there fields are public to allow setting 'em in g_map; this should be fixed later
    // for now, PLEASE, don't modify 'em, or all hell will break loose
    arrIdx:           Integer; // index in one of internal arrays; sorry
    tag:              Integer; // used in coldets and such; sorry; see g_map.GridTagXXX
    proxyId:          Integer; // proxy id in map grid (DO NOT USE!)
    mapId:            AnsiString; // taken directly from map file; dunno why it is here

    constructor Create(PanelRec: TDynRecord;
                       AddTextures: TAddTextureArray;
                       CurTex: Integer;
                       var Textures: TLevelTextureArray; aguid: Integer);
    destructor  Destroy(); override;

    procedure   Draw();
    procedure   DrawShadowVolume(lightX: Integer; lightY: Integer; radius: Integer);
    procedure   Update();
    procedure   SetFrame(Frame: Integer; Count: Byte);
    procedure   NextTexture(AnimLoop: Byte = 0);
    procedure   SetTexture(ID: Integer; AnimLoop: Byte = 0);
    function    GetTextureID(): Cardinal;
    function    GetTextureCount(): Integer;

    procedure   SaveState(var Mem: TBinMemoryWriter);
    procedure   LoadState(var Mem: TBinMemoryReader);

    procedure positionChanged (); inline;

    function isGBack (): Boolean; inline; // gRenderBackgrounds
    function isGStep (): Boolean; inline; // gSteps
    function isGWall (): Boolean; inline; // gWalls
    function isGAcid1 (): Boolean; inline; // gAcid1
    function isGAcid2 (): Boolean; inline; // gAcid2
    function isGWater (): Boolean; inline; // gWater
    function isGFore (): Boolean; inline; // gRenderForegrounds
    function isGLift (): Boolean; inline; // gLifts
    function isGBlockMon (): Boolean; inline; // gBlockMon


  public
    property visvalid: Boolean read getvisvalid; // panel is "visvalid" when it's width and height are positive

  published
    property guid: Integer read mGUID; // will be assigned in "g_map.pas"
    property x0: Integer read FX;
    property y0: Integer read FY;
    property x1: Integer read getx1; // inclusive!
    property y1: Integer read gety1; // inclusive!
    property x: Integer read FX write FX;
    property y: Integer read FY write FY;
    property width: Word read FWidth write FWidth;
    property height: Word read FHeight write FHeight;
    property panelType: Word read FPanelType write FPanelType;
    property saveIt: Boolean read FSaveIt write FSaveIt; // Сохранять при SaveState?
    property enabled: Boolean read FEnabled write FEnabled; // Сохранять при SaveState?
    property door: Boolean read FDoor write FDoor; // Сохранять при SaveState?
    property moved: Boolean read FMoved write FMoved; // Сохранять при SaveState?
    property liftType: Byte read FLiftType write FLiftType; // Сохранять при SaveState?
    property lastAnimLoop: Byte read FLastAnimLoop write FLastAnimLoop; // Сохранять при SaveState?

    property movingActive: Boolean read mMovingActive write mMovingActive;

  public
    property movingSpeed: TDFPoint read mMovingSpeed write mMovingSpeed;
    property movingStart: TDFPoint read mMovingStart write mMovingStart;
    property movingEnd: TDFPoint read mMovingEnd write mMovingEnd;
  end;

  TPanelArray = Array of TPanel;

implementation

uses
  SysUtils, g_basic, g_map, g_game, g_gfx, e_graphics,
  g_console, g_language, g_monsters, g_player, e_log, GL;

const
  PANEL_SIGNATURE = $4C4E4150; // 'PANL'

{ T P a n e l : }

constructor TPanel.Create(PanelRec: TDynRecord;
                          AddTextures: TAddTextureArray;
                          CurTex: Integer;
                          var Textures: TLevelTextureArray; aguid: Integer);
var
  i: Integer;
begin
  X := PanelRec.X;
  Y := PanelRec.Y;
  Width := PanelRec.Width;
  Height := PanelRec.Height;
  FAlpha := 0;
  FBlending := False;
  FCurFrame := 0;
  FCurFrameCount := 0;
  LastAnimLoop := 0;
  Moved := False;

  mapId := PanelRec.id;
  mGUID := aguid;

  mMovingSpeed := PanelRec.moveSpeed;
  mMovingStart := PanelRec.moveStart;
  mMovingEnd := PanelRec.moveEnd;
  mMovingActive := PanelRec['move_active'].varvalue;

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
    PANEL_LIFTLEFT:
      begin
        LiftType := 2;
        SaveIt := True;
      end;
    PANEL_LIFTRIGHT:
      begin
        LiftType := 3;
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
// Панели, не использующие текстуры:
  if ByteBool(PanelType and
    (PANEL_LIFTUP or
     PANEL_LIFTDOWN or
     PANEL_LIFTLEFT or
     PANEL_LIFTRIGHT or
     PANEL_BLOCKMON)) then
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
        FTextureIDs[0].Tex := LongWord(TEXTURE_SPECIAL_WATER);
      PANEL_ACID1:
        FTextureIDs[0].Tex := LongWord(TEXTURE_SPECIAL_ACID1);
      PANEL_ACID2:
        FTextureIDs[0].Tex := LongWord(TEXTURE_SPECIAL_ACID2);
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
  if PanelRec.TextureNum > High(Textures) then
  begin
    e_WriteLog(Format('WTF?! PanelRec.TextureNum is out of limits! (%d : %d)', [PanelRec.TextureNum, High(Textures)]), MSG_FATALERROR);
    FTextureWidth := 2;
    FTextureHeight := 2;
    FAlpha := 0;
    FBlending := ByteBool(0);
  end
  else if not g_Map_IsSpecialTexture(Textures[PanelRec.TextureNum].TextureName) then
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

function TPanel.getx1 (): Integer; inline; begin result := X+Width-1; end;
function TPanel.gety1 (): Integer; inline; begin result := Y+Height-1; end;
function TPanel.getvisvalid (): Boolean; inline; begin result := (Width > 0) and (Height > 0); end;

function TPanel.isGBack (): Boolean; inline; begin result := ((tag and GridTagBack) <> 0); end;
function TPanel.isGStep (): Boolean; inline; begin result := ((tag and GridTagStep) <> 0); end;
function TPanel.isGWall (): Boolean; inline; begin result := ((tag and (GridTagWall or GridTagDoor)) <> 0); end;
function TPanel.isGAcid1 (): Boolean; inline; begin result := ((tag and GridTagAcid1) <> 0); end;
function TPanel.isGAcid2 (): Boolean; inline; begin result := ((tag and GridTagAcid2) <> 0); end;
function TPanel.isGWater (): Boolean; inline; begin result := ((tag and GridTagWater) <> 0); end;
function TPanel.isGFore (): Boolean; inline; begin result := ((tag and GridTagFore) <> 0); end;
function TPanel.isGLift (): Boolean; inline; begin result := ((tag and GridTagLift) <> 0); end;
function TPanel.isGBlockMon (): Boolean; inline; begin result := ((tag and GridTagBlockMon) <> 0); end;

procedure TPanel.Draw();
var
  xx, yy: Integer;
  NoTextureID: DWORD;
  NW, NH: Word;
begin
  if {Enabled and} (FCurTexture >= 0) and
     (Width > 0) and (Height > 0) and (FAlpha < 255) and
     ((g_dbg_scale <> 1.0) or g_Collide(X, Y, Width, Height, sX, sY, sWidth, sHeight)) then
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
          LongWord(TEXTURE_SPECIAL_WATER):
            e_DrawFillQuad(X, Y, X+Width-1, Y+Height-1,
                           0, 0, 255, 0, B_FILTER);
          LongWord(TEXTURE_SPECIAL_ACID1):
            e_DrawFillQuad(X, Y, X+Width-1, Y+Height-1,
                           0, 128, 0, 0, B_FILTER);
          LongWord(TEXTURE_SPECIAL_ACID2):
            e_DrawFillQuad(X, Y, X+Width-1, Y+Height-1,
                           128, 0, 0, 0, B_FILTER);
          LongWord(TEXTURE_NONE):
            if g_Texture_Get('NOTEXTURE', NoTextureID) then
            begin
              e_GetTextureSize(NoTextureID, @NW, @NH);
              e_DrawFill(NoTextureID, X, Y, Width div NW, Height div NH,
                         0, False, False);
            end else
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

procedure TPanel.DrawShadowVolume(lightX: Integer; lightY: Integer; radius: Integer);
  procedure extrude (x: Integer; y: Integer);
  begin
    glVertex2i(x+(x-lightX)*500, y+(y-lightY)*500);
    //e_WriteLog(Format('  : (%d,%d)', [x+(x-lightX)*300, y+(y-lightY)*300]), MSG_WARNING);
  end;

  procedure drawLine (x0: Integer; y0: Integer; x1: Integer; y1: Integer);
  begin
    // does this side facing the light?
    if ((x1-x0)*(lightY-y0)-(lightX-x0)*(y1-y0) >= 0) then exit;
    //e_WriteLog(Format('lightpan: (%d,%d)-(%d,%d)', [x0, y0, x1, y1]), MSG_WARNING);
    // this edge is facing the light, extrude and draw it
    glVertex2i(x0, y0);
    glVertex2i(x1, y1);
    extrude(x1, y1);
    extrude(x0, y0);
  end;

begin
  if radius < 4 then exit;
  if Enabled and (FCurTexture >= 0) and (Width > 0) and (Height > 0) and (FAlpha < 255) and g_Collide(X, Y, Width, Height, sX, sY, sWidth, sHeight) then
  begin
    if not FTextureIDs[FCurTexture].Anim then
    begin
      case FTextureIDs[FCurTexture].Tex of
        LongWord(TEXTURE_SPECIAL_WATER): exit;
        LongWord(TEXTURE_SPECIAL_ACID1): exit;
        LongWord(TEXTURE_SPECIAL_ACID2): exit;
        LongWord(TEXTURE_NONE): exit;
      end;
    end;
    if (X+Width < lightX-radius) then exit;
    if (Y+Height < lightY-radius) then exit;
    if (X > lightX+radius) then exit;
    if (Y > lightY+radius) then exit;
    //e_DrawFill(FTextureIDs[FCurTexture].Tex, X, Y, Width div FTextureWidth, Height div FTextureHeight, FAlpha, True, FBlending);

    glBegin(GL_QUADS);
      drawLine(x,       y,        x+width, y); // top
      drawLine(x+width, y,        x+width, y+height); // right
      drawLine(x+width, y+height, x,       y+height); // bottom
      drawLine(x,       y+height, x,       y); // left
    glEnd();
  end;
end;


procedure TPanel.positionChanged (); inline;
begin
  if (proxyId >= 0) then mapGrid.moveBody(proxyId, X, Y);
end;


var
  monMoveList: array of TMonster = nil;
  monMoveListUsed: Integer = 0;

procedure TPanel.Update();
var
  nx, ny: Integer;
  f: Integer;

  function doPush (px, py, pw, ph: Integer; out dx, dy: Integer): Boolean;
  begin
    result := g_Collide(px, py, pw, ph, nx, ny, Width, Height);
    if result then
    begin
      // need to push
           if (mMovingSpeed.X < 0) then dx := nx-(px+pw)
      else if (mMovingSpeed.X > 0) then dx := (nx+Width)-px
      else dx := 0;
           if (mMovingSpeed.Y < 0) then dy := ny-(py+ph)
      else if (mMovingSpeed.Y > 0) then dy := (ny+Height)-py
      else dy := 0;
    end
    else
    begin
      dx := 0;
      dy := 0;
    end;
  end;

  function monMove (mon: TMonster): Boolean;
  begin
    result := false; // don't stop
    mon.GameX := mon.GameX+mMovingSpeed.X;
    mon.GameY := mon.GameY+mMovingSpeed.Y;
    if (monMoveListUsed >= Length(monMoveList)) then SetLength(monMoveList, monMoveListUsed+64);
    monMoveList[monMoveListUsed] := mon;
    Inc(monMoveListUsed);
  end;

  function monPush (mon: TMonster): Boolean;
  var
    px, py, pw, ph, dx, dy: Integer;
  begin
    result := false; // don't stop
    mon.getMapBox(px, py, pw, ph);
    if doPush(px, py, pw, ph, dx, dy) then
    begin
      mon.GameX := mon.GameX+dx;
      mon.GameY := mon.GameY+dy;
      if (monMoveListUsed >= Length(monMoveList)) then SetLength(monMoveList, monMoveListUsed+64);
      monMoveList[monMoveListUsed] := mon;
      Inc(monMoveListUsed);
    end;
  end;

  procedure plrMove (plr: TPlayer);
  var
    px, py, pw, ph, dx, dy: Integer;
  begin
    if (plr = nil) then exit;
    plr.getMapBox(px, py, pw, ph);
    if (py+ph <> Y) then
    begin
      // push player
      if doPush(px, py, pw, ph, dx, dy) then
      begin
        plr.GameX := plr.GameX+dx;
        plr.GameY := plr.GameY+dy;
        plr.positionChanged();
      end;
      exit;
    end;
    if (px+pw <= X) then exit;
    if (px >= X+Width) then exit;
    plr.GameX := plr.GameX+mMovingSpeed.X;
    plr.GameY := plr.GameY+mMovingSpeed.Y;
    plr.positionChanged();
  end;

begin
  if Enabled and (FCurTexture >= 0) and
    (FTextureIDs[FCurTexture].Anim) and
    (FTextureIDs[FCurTexture].AnTex <> nil) and
    (Width > 0) and (Height > 0) and (FAlpha < 255) then
  begin
    FTextureIDs[FCurTexture].AnTex.Update();
    FCurFrame := FTextureIDs[FCurTexture].AnTex.CurrentFrame;
    FCurFrameCount := FTextureIDs[FCurTexture].AnTex.CurrentCounter;
  end;

  if mMovingActive and (not mMovingSpeed.isZero) then
  begin
    monMoveListUsed := 0;
    nx := X+mMovingSpeed.X;
    ny := Y+mMovingSpeed.Y;
    // move monsters on lifts
    g_Mons_ForEachAt(X, Y-1, Width, 1, monMove);
    // push monsters
    g_Mons_ForEachAt(nx, ny, Width, Height, monPush);
    // move and push players
    for f := 0 to High(gPlayers) do plrMove(gPlayers[f]);
    // reverse moving direction, if necessary
         if (mMovingSpeed.X < 0) and (nx <= mMovingStart.X) then mMovingSpeed.X := -mMovingSpeed.X
    else if (mMovingSpeed.X > 0) and (nx >= mMovingEnd.X) then mMovingSpeed.X := -mMovingSpeed.X;
         if (mMovingSpeed.Y < 0) and (ny <= mMovingStart.Y) then mMovingSpeed.Y := -mMovingSpeed.Y
    else if (mMovingSpeed.Y > 0) and (ny >= mMovingEnd.Y) then mMovingSpeed.Y := -mMovingSpeed.Y;
    // awake particles
    g_Mark(X, Y, Width, Height, MARK_WALL, false);
    X := nx;
    Y := ny;
    g_Mark(nx, ny, Width, Height, MARK_WALL);
    // fix grid
    positionChanged();
    // notify moved monsters about their movement
    for f := 0 to monMoveListUsed-1 do monMoveList[f].positionChanged();
  end;
end;

procedure TPanel.SetFrame(Frame: Integer; Count: Byte);

  function ClampInt(X, A, B: Integer): Integer;
  begin
    Result := X;
    if X < A then Result := A else if X > B then Result := B;
  end;

begin
  if Enabled and (FCurTexture >= 0) and
    (FTextureIDs[FCurTexture].Anim) and
    (FTextureIDs[FCurTexture].AnTex <> nil) and
    (Width > 0) and (Height > 0) and (FAlpha < 255) then
  begin
    FCurFrame := ClampInt(Frame, 0, FTextureIDs[FCurTexture].AnTex.TotalFrames);
    FCurFrameCount := Count;
    FTextureIDs[FCurTexture].AnTex.CurrentFrame := FCurFrame;
    FTextureIDs[FCurTexture].AnTex.CurrentCounter := FCurFrameCount;
  end;
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

  LastAnimLoop := AnimLoop;
end;

procedure TPanel.SetTexture(ID: Integer; AnimLoop: Byte = 0);
begin
// Нет текстур:
  if Length(FTextureIDs) = 0 then
    FCurTexture := -1
  else
  // Только одна текстура:
    if Length(FTextureIDs) = 1 then
      begin
        if (ID = 0) or (ID = -1) then
          FCurTexture := ID;
      end
    else
    // Больше одной текстуры:
      begin
        if (ID >= -1) and (ID <= High(FTextureIDs)) then
          FCurTexture := ID;
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

  LastAnimLoop := AnimLoop;
end;

function TPanel.GetTextureID(): DWORD;
begin
  Result := LongWord(TEXTURE_NONE);

  if (FCurTexture >= 0) then
  begin
    if FTextureIDs[FCurTexture].Anim then
      Result := FTextureIDs[FCurTexture].AnTex.FramesID
    else
      Result := FTextureIDs[FCurTexture].Tex;
  end;
end;

function TPanel.GetTextureCount(): Integer;
begin
  Result := Length(FTextureIDs);
  if Enabled and (FCurTexture >= 0) then
     if (FTextureIDs[FCurTexture].Anim) and
        (FTextureIDs[FCurTexture].AnTex <> nil) and
        (Width > 0) and (Height > 0) and (FAlpha < 255) then
       Result := Result + 100;
end;

procedure TPanel.SaveState(Var Mem: TBinMemoryWriter);
var
  sig: DWORD;
  anim: Boolean;
begin
  if (Mem = nil) then exit;
  //if not SaveIt then exit;

// Сигнатура панели:
  sig := PANEL_SIGNATURE; // 'PANL'
  Mem.WriteDWORD(sig);
// Открыта/закрыта, если дверь:
  Mem.WriteBoolean(FEnabled);
// Направление лифта, если лифт:
  Mem.WriteByte(FLiftType);
// Номер текущей текстуры:
  Mem.WriteInt(FCurTexture);
// Коорды
  Mem.WriteInt(FX);
  Mem.WriteInt(FY);
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
  // moving platform state
  Mem.WriteInt(mMovingSpeed.X);
  Mem.WriteInt(mMovingSpeed.Y);
  Mem.WriteInt(mMovingStart.X);
  Mem.WriteInt(mMovingStart.Y);
  Mem.WriteInt(mMovingEnd.X);
  Mem.WriteInt(mMovingEnd.Y);
  Mem.WriteBoolean(mMovingActive);
end;

procedure TPanel.LoadState(var Mem: TBinMemoryReader);
var
  sig: DWORD;
  anim: Boolean;
  //ox, oy: Integer;
begin
  if (Mem = nil) then exit;
  //if not SaveIt then exit;

// Сигнатура панели:
  Mem.ReadDWORD(sig);
  if sig <> PANEL_SIGNATURE then // 'PANL'
  begin
    raise EBinSizeError.Create('TPanel.LoadState: Wrong Panel Signature');
  end;
// Открыта/закрыта, если дверь:
  Mem.ReadBoolean(FEnabled);
// Направление лифта, если лифт:
  Mem.ReadByte(FLiftType);
// Номер текущей текстуры:
  Mem.ReadInt(FCurTexture);
// Коорды
  //ox := FX;
  //oy := FY;
  Mem.ReadInt(FX);
  Mem.ReadInt(FY);
  //e_LogWritefln('panel %s(%s): old=(%s,%s); new=(%s,%s); delta=(%s,%s)', [arrIdx, proxyId, ox, oy, FX, FY, FX-ox, FY-oy]);
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
  // moving platform state
  Mem.ReadInt(mMovingSpeed.X);
  Mem.ReadInt(mMovingSpeed.Y);
  Mem.ReadInt(mMovingStart.X);
  Mem.ReadInt(mMovingStart.Y);
  Mem.ReadInt(mMovingEnd.X);
  Mem.ReadInt(mMovingEnd.Y);
  Mem.ReadBoolean(mMovingActive);

  positionChanged();
  mapGrid.proxyEnabled[proxyId] := FEnabled;
end;

end.
