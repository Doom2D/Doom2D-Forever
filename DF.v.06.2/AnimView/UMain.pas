unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TMainForm = class(TForm)
    Render: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RenderResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
      
  private
    { Private declarations }
  public
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure Draw();
  end;

var
  MainForm: TMainForm;

implementation

uses
  dglOpenGL, e_graphics, e_textures, e_log, UAnimSel;

{$R *.dfm}

var
  HomeDir: String;
  hDC: THandle;
  hRC: THandle;

  TexLoaded: Boolean;
  FrameWidth, FrameHeight: Word;
  CurrentTex: Word;
  CurrentFrame: Word;
  Zoom: Byte;
  AlphaEnabled: Boolean;
  Mirror: Boolean;
  OffsetX, OffsetY: Integer;
  ViewMode: Boolean;
  FontID: DWORD;
  HitWidth, HitHeight: Word;
  PicName: String;
  TexID: Array of Record
                    T: Array of DWORD;
                    X, Y: Integer;
                    Name: String;
                  End;


procedure ReCalcOffsets();
begin
  OffsetX := (MainForm.Render.Width - FrameWidth*Zoom) div 2;
  OffsetY := (MainForm.Render.Height - FrameHeight*Zoom) div 2;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  PixelFormat: GLuint;
  pfd: TPIXELFORMATDESCRIPTOR;
  
begin
  HomeDir := ExtractFilePath(Application.ExeName);

  e_InitLog(HomeDir+'AnimView.log', WM_NEWFILE);

  e_WriteLog('Init OpenGL', MSG_NOTIFY);

  InitOpenGL();
  hDC := GetDC(Render.Handle);

  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do
  begin
    nSize := SizeOf(pfd);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cDepthBits := 32;
    iLayerType := PFD_MAIN_PLANE;
  end;
  PixelFormat := ChoosePixelFormat (hDC, @pfd);
  SetPixelFormat (hDC, PixelFormat, @pfd);

  hRC := wglCreateContext(hDC);
  ActivateRenderingContext(hDC, hRC);

  e_InitGL(False);

  FontID := e_SimpleFontCreate('Arial Cyr', 12, FW_NORMAL, hDC);

  FrameWidth := 1;
  FrameHeight := 1;
  HitWidth := 1;
  HitHeight := 1;
  Zoom := 1;
  ViewMode := False;
  CurrentTex := 0;
  CurrentFrame := 0;
  AlphaEnabled := True;
  Mirror := False;

  SetLength(TexID, 0);
  TexLoaded := False;

  Application.OnIdle := OnIdle;
end;

procedure DeleteTextures();
var
  i, j: Integer;

begin
  if TexLoaded then
  begin
    for i := 0 to Length(TexID)-1 do
    begin
      for j := 0 to Length(TexID[i].T)-1 do
        e_DeleteTexture(TexID[i].T[j]);

      SetLength(TexID[i].T, 0);
    end;

    SetLength(TexID, 0);
    TexLoaded := False;
  end;
end;

procedure CreateLoadTextures(frW, frH, hitW, hitH: Word;
                             var aTexIDs: TAnimTexs; aName: String);
var
  i, j: Integer;
  
begin
  TexLoaded := False;
  SetLength(TexID, Length(aTexIDs));

  for i := 0 to Length(aTexIDs)-1 do
  begin
    SetLength(TexID[i].T, Length(aTexIDs[i].T));

    for j := 0 to Length(aTexIDs[i].T)-1 do
      TexID[i].T[j] := aTexIDs[i].T[j];

    TexID[i].X := 0;
    TexID[i].Y := 0;
    TexID[i].Name := aTexIDs[i].Name;
  end;

  FrameWidth := frW;
  FrameHeight := frH;
  HitWidth := hitW;
  HitHeight := hitH;
  PicName := aName;

  CurrentTex := 0;
  CurrentFrame := 0;
  ViewMode := False;

  ReCalcOffsets();

  if Length(TexID) > 0 then
  begin
    MainForm.Caption := PicName;

    TexLoaded := True;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  wglDeleteContext(hRC);
end;

procedure e_DrawLineAlpha(X1, Y1, X2, Y2: Integer; Red, Green, Blue, Alpha: Byte);
begin
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor4ub(Red, Green, Blue, 255-Alpha);
  glLineWidth(1);

  glBegin(GL_LINES);
    glVertex2i(X1, Y1);
    glVertex2i(X2, Y2);
  glEnd();

  glDisable(GL_BLEND);
  glColor4ub(e_Colors.R, e_Colors.G, e_Colors.B, 255);
end;

procedure TMainForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Draw();
end;

procedure TMainForm.Draw();
var
  ps: TPaintStruct;
  mir: TMirrorType;
  k, hitX, hitY, dx, dy, w, h: Integer;
  str: String;

begin
  BeginPaint(Render.Handle, ps);

  glClearColor(0.5, 0.5, 0.5, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);

  if TexLoaded then
  begin
    if Mirror then
      mir := M_HORIZONTAL
    else
      mir := M_NONE;

    hitX := (FrameWidth - HitWidth) div 2;
    hitY := (FrameHeight - HitHeight) div 2;
    dx := hitX - TexID[CurrentTex].X;
    dy := hitY - TexID[CurrentTex].Y;

    if not ViewMode then
      begin
        e_DrawQuad(OffsetX - 1,
                   OffsetY - 1,
                   OffsetX + FrameWidth*Zoom,
                   OffsetY + FrameHeight*Zoom,
                   255, 0, 0);

        e_DrawSizeMirror(TexID[CurrentTex].T[CurrentFrame],
                         OffsetX, OffsetY,
                         0, AlphaEnabled, False,
                         FrameWidth*Zoom, FrameHeight*Zoom,
                         mir);

        w := TexID[CurrentTex].X + HitWidth;
        h := TexID[CurrentTex].Y + HitHeight;

        for k := 0 to Zoom-1 do
        begin
          e_DrawLineAlpha(OffsetX + (TexID[CurrentTex].X+1)*Zoom,
                          OffsetY + TexID[CurrentTex].Y*Zoom + k,
                          OffsetX + (w-1)*Zoom,
                          OffsetY + TexID[CurrentTex].Y*Zoom + k,
                          255, 0, 255, 128);
          e_DrawLineAlpha(OffsetX + TexID[CurrentTex].X*Zoom,
                          OffsetY + (h-1)*Zoom + k,
                          OffsetX + (w-1+1)*Zoom,
                          OffsetY + (h-1)*Zoom + k,
                          255, 0, 255, 128);
          e_DrawLineAlpha(OffsetX + TexID[CurrentTex].X*Zoom + k + 1,
                          OffsetY + TexID[CurrentTex].Y*Zoom,
                          OffsetX + TexID[CurrentTex].X*Zoom + k + 1,
                          OffsetY + (h-1)*Zoom,
                          255, 0, 255, 128);
          e_DrawLineAlpha(OffsetX + (w-1)*Zoom + k + 1,
                          OffsetY + TexID[CurrentTex].Y*Zoom,
                          OffsetX + (w-1)*Zoom + k + 1,
                          OffsetY + (h-1)*Zoom,
                          255, 0, 255, 128);
        end;
      end
    else
      begin
        if Mirror then
        begin
          dx := -FrameWidth - dx + hitX + HitWidth + hitX;
        end;

        e_DrawSizeMirror(TexID[CurrentTex].T[CurrentFrame],
                         OffsetX + dx*Zoom, OffsetY + dy*Zoom,
                         0, AlphaEnabled, False,
                         FrameWidth*Zoom, FrameHeight*Zoom,
                         mir);

        w := hitX + HitWidth;
        h := hitY + HitHeight;

        for k := 0 to Zoom-1 do
        begin
          e_DrawLineAlpha(OffsetX + (hitX+1)*Zoom,
                          OffsetY + hitY*Zoom + k,
                          OffsetX + (w-1)*Zoom,
                          OffsetY + hitY*Zoom + k,
                          255, 0, 255, 128);
          e_DrawLineAlpha(OffsetX + hitX*Zoom,
                          OffsetY + (h-1)*Zoom + k,
                          OffsetX + (w-1+1)*Zoom,
                          OffsetY + (h-1)*Zoom + k,
                          255, 0, 255, 128);
          e_DrawLineAlpha(OffsetX + hitX*Zoom + k + 1,
                          OffsetY + hitY*Zoom,
                          OffsetX + hitX*Zoom + k + 1,
                          OffsetY + (h-1)*Zoom,
                          255, 0, 255, 128);
          e_DrawLineAlpha(OffsetX + (w-1)*Zoom + k + 1,
                          OffsetY + hitY*Zoom,
                          OffsetX + (w-1)*Zoom + k + 1,
                          OffsetY + (h-1)*Zoom,
                          255, 0, 255, 128);
        end;
      end;

    str := Format('%d/%d;   Frame: %d/%d;   %s',
                  [CurrentTex+1, Length(TexID),
                   CurrentFrame+1, Length(TexID[CurrentTex].T),
                   TexID[CurrentTex].Name]);
    e_SimpleFontPrint(5, 15, PChar(str), FontID, 255, 255, 255);

    str := Format('X: %3d;   Y: %3d', [TexID[CurrentTex].X, TexID[CurrentTex].Y]);
    e_SimpleFontPrint(5, 30, PChar(str), FontID, 255, 255, 255);

    str := Format('Width: %3d;   Height: %3d', [HitWidth, HitHeight]);
    e_SimpleFontPrint(5, 45, PChar(str), FontID, 255, 255, 255);

    str := Format('HitX: %3d;   HitY: %3d', [hitX, hitY]);
    e_SimpleFontPrint(5, 60, PChar(str), FontID, 255, 255, 255);

    str := Format('DeltaX: %3d;   DeltaY: %3d', [dx, dy]);
    e_SimpleFontPrint(5, 75, PChar(str), FontID, 255, 255, 255);
  end;

  SwapBuffers(hDC);
  EndPaint(Render.Handle, ps);
end;

procedure TMainForm.RenderResize(Sender: TObject);
begin
  if MainForm.Visible then
    MainForm.Resize();
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  e_SetViewPort(0, 0, Render.Width, Render.Height);

  ReCalcOffsets();
end;

procedure SaveCoords();
var
  SL: TStringList;
  i, hitX, hitY, dx, dy: Integer;

begin
  SL := TStringList.Create();

  hitX := (FrameWidth - HitWidth) div 2;
  hitY := (FrameHeight - HitHeight) div 2;

  SL.Add(Format('X:%3d;  Y:%3d;  Width:%3d;  Height:%3d',
                [hitX, hitY, HitWidth, HitHeight]));

  for i := 0 to Length(TexID)-1 do
  begin
    dx := hitX - TexID[i].X;
    dy := hitY - TexID[i].Y;

    SL.Add(Format('%-20s: dX:%3d;  dY:%3d', [TexID[i].Name, dx, dy]));
  end;

  SL.SaveToFile('T_' + PicName + '.txt');
  SL.Free();
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  n: Integer;

begin
// Загрузка:
  if Key = Ord('L') then
    begin
      with AnimSelForm do
        if (ShowModal() = mrOK) then
          CreateLoadTextures(wFrameWidth, wFrameHeight,
                             wHitWidth, wHitHeight,
                             aTexIDs, sPicName);
    end

// Вверх:
  else if Key = VK_UP then
    begin
      if TexLoaded then
      begin
        if ssShift in Shift then
          Dec(TexID[CurrentTex].Y, 5)
        else
          Dec(TexID[CurrentTex].Y);

        if TexID[CurrentTex].Y < (1-HitHeight) then
          TexID[CurrentTex].Y := 1-HitHeight;
      end;
    end

// Вниз:
  else if Key = VK_DOWN then
    begin
      if TexLoaded then
      begin
        if ssShift in Shift then
          Inc(TexID[CurrentTex].Y, 5)
        else
          Inc(TexID[CurrentTex].Y);

        if (TexID[CurrentTex].Y) > (FrameHeight-1) then
          TexID[CurrentTex].Y := FrameHeight-1;
      end;
    end

// Влево:
  else if Key = VK_LEFT then
    begin
      if TexLoaded then
      begin
        if ssShift in Shift then
          Dec(TexID[CurrentTex].X, 5)
        else
          Dec(TexID[CurrentTex].X);

        if TexID[CurrentTex].X < (1-HitWidth) then
          TexID[CurrentTex].X := 1-HitWidth;
      end;
    end

// Вправо:
  else if Key = VK_RIGHT then
    begin
      if TexLoaded then
      begin
        if ssShift in Shift then
          Inc(TexID[CurrentTex].X, 5)
        else
          Inc(TexID[CurrentTex].X);

        if (TexID[CurrentTex].X) > (FrameWidth-1) then
          TexID[CurrentTex].X := FrameWidth-1;
      end;
    end

// Предыдущая анимация:
  else if Key = 188 then
    begin
      if TexLoaded then
      begin
        if CurrentTex > 0 then
          Dec(CurrentTex)
        else
          CurrentTex := Length(TexID)-1;
        CurrentFrame := 0;
      end;
    end

// Следующая анимация:
  else if Key = 190 then
    begin
      if TexLoaded then
      begin
        if CurrentTex < (Length(TexID)-1) then
          Inc(CurrentTex)
        else
          CurrentTex := 0;
        CurrentFrame := 0;
      end;
    end

// Произвольная анимация:
  else if (Key >= Ord('1')) and (Key <= Ord('9')) then
    begin
      if TexLoaded then
      begin
        n := Key - Ord('1');
        if (n >= 0) and (n < Length(TexID)) then
        begin
          CurrentTex := n;
          CurrentFrame := 0;
        end;
      end;
    end

// Предыдущий кадр:
  else if Key = Ord('B') then
    begin
      if TexLoaded then
      begin
        if CurrentFrame > 0 then
          Dec(CurrentFrame)
        else
          CurrentFrame := Length(TexID[CurrentTex].T)-1;
      end;
    end

// Следующий кадр:
  else if Key = VK_SPACE then
    begin
      if TexLoaded then
      begin
        if CurrentFrame < (Length(TexID[CurrentTex].T)-1) then
          Inc(CurrentFrame)
        else
          CurrentFrame := 0;
      end;
    end

// Прозрачность:
  else if Key = Ord('T') then
    begin
      AlphaEnabled := not AlphaEnabled;
    end

// Отражение:
  else if Key = Ord('M') then
    begin
      Mirror := not Mirror;
    end

// Масштаб:
  else if Key = Ord('Z') then
    begin
      if TexLoaded then
      begin
        case Zoom of
          1:   Zoom := 2;
          2:   Zoom := 4;
          else Zoom := 1;
        end;

        ReCalcOffsets();
      end;
    end

// Переключение режима вида:
  else if Key = VK_RETURN then
    begin
      ViewMode := not ViewMode;
    end

// Подгон под центр:
  else if Key = Ord('C') then
    begin
      if TexLoaded then
      begin
        TexID[CurrentTex].X := (FrameWidth - HitWidth) div 2;
        TexID[CurrentTex].Y := (FrameHeight - HitHeight) div 2;
      end;
    end

// Сохранение:
  else if Key = Ord('S') then
    begin
      if TexLoaded then
      begin
        SaveCoords();
      end;
    end;
end;

End.
