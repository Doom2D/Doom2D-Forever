unit spectrum;

interface

uses
  Windows, Classes, Controls, Messages, Graphics,
  fmod, fmodtypes;

const
  N_SPECTRUM_VALUES = 512;

type
  TSpectrumStyle = (ssSmooth, ssBlock);

  TMiniSpectrum = class (TGraphicControl)
  private
    FGradient: TBitmap;
    FBuffer: TBitmap;
    FScale: Single;
    FStyle: TSpectrumStyle;
    FValues: array [0..N_SPECTRUM_VALUES-1] of Single;
    FGradientCount: Word;
    FChannel: FMOD_CHANNEL;
    FRawData: System.PSingle;

    procedure SetStyle(const Value: TSpectrumStyle);

  protected
    procedure Paint(); override;
    procedure Resize(); override;
    procedure SetEnabled(Value: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw();
    procedure SetChannel(ch: FMOD_CHANNEL);

  published
    property Align;
    property Scale: Single read FScale write FScale;
    property Style: TSpectrumStyle read FStyle write SetStyle;
    property OnClick;
  end;

implementation

uses
  g_language;

{ TMiniSpectrum }

constructor TMiniSpectrum.Create(AOwner: TComponent);
var
  X, Y: Integer;
  R, G, B: Integer;
  C: TColor;

begin
  inherited;
  
  Color := clBlack;
  Parent := AOwner as TWinControl;
  Width := Parent.Width;
  Height := Parent.Height;
  FScale := 64.0;
  FStyle := ssSmooth;
  Enabled := False;
  FChannel := nil;
  GetMem(FRawData, N_SPECTRUM_VALUES * SizeOf(Single));

// Create draw buffer:
  FBuffer := TBitmap.Create();
  FBuffer.PixelFormat := pf32bit;
  FBuffer.Width := Width;
  FBuffer.Height := Height;

// Create gradient bitmap:
  FGradientCount := 40;
  FGradient := TBitmap.Create();
  FGradient.PixelFormat := pf32bit;
  FGradient.Width := Width div FGradientCount;
  FGradient.Height := Height;

  R := 255;
  G := 0;
  B := 0;

  for Y := 0 to Height-1 do
  begin
    if Y > (Height div 2)-1 then
      Dec(R, 16)
    else
      Inc(G, 16);
    if R < 0 then
      R := 0;
    if G > 255 then
      G := 255;
    C := TColor(RGB(R, G, B));
    for X := 0 to Width-2 do
      FGradient.Canvas.Pixels[X, Y] := C;
    FGradient.Canvas.Pixels[Width-1, Y] := TColor(0);
  end;
end;

destructor TMiniSpectrum.Destroy;
begin
  FreeMem(FRawData);
  FGradient.Free;
  FBuffer.Free;
  
  inherited;
end;

type
  PSingleArray = ^TSingleArray;
  TSingleArray = array [0..0] of Single;

procedure TMiniSpectrum.Draw();
var
  Data: PSingleArray;
  PeakData: Single;
  X, Y, a, nGC: Integer;
  ARect: TRect;
  res: FMOD_RESULT; 

begin
{$R-}
  FBuffer.Canvas.Brush.Color := Color;
  FBuffer.Canvas.FillRect(BoundsRect);

  if Enabled then
    begin
      if FChannel <> nil then
        begin
          res := FMOD_Channel_GetSpectrum(FChannel, FRawData,
                   N_SPECTRUM_VALUES, 0, FMOD_DSP_FFT_WINDOW_MAX);
          if res <> FMOD_OK then
            begin
              ZeroMemory(@FValues, SizeOf(FValues));
            end
          else
            begin
              Data := PSingleArray(FRawData);
              for X := 0 to High(FValues) do
              begin
                FValues[X] := Data^[X] * FScale;
                if FValues[X] > 1.0 then
                  FValues[X] := 1.0;
              end;
            end;
        end
      else
        begin
          ZeroMemory(@FValues, SizeOf(FValues));
        end;

      case FStyle of
        ssSmooth,
        ssBlock:
          begin
            PeakData := 0.0;
            nGC := N_SPECTRUM_VALUES div FGradientCount;

            for X := 0 to FGradientCount do
            begin
              for a := X*nGC to (X+1)*nGC-1 do
                if PeakData < FValues[a] then
                  PeakData := FValues[a];

              if PeakData > 0.0 then
              begin
                Y := Height - Trunc(PeakData*Height);
                PeakData := 0;
                FBuffer.Canvas.CopyRect(Rect(X*FGradient.Width+1, Y, (X+1)*FGradient.Width, Height),
                                 FGradient.Canvas, Rect(0, Y, FGradient.Width, FGradient.Height));
              end;
            end;
          end;
      end;
    end
  else // if Enabled ...
    begin
      FBuffer.Canvas.Font.Color := clWhite;
      ARect := BoundsRect;
      DrawText(FBuffer.Canvas.Handle, PChar(_lc[I_LAB_SPECTRUM]), -1, ARect,
               DT_WORDBREAK or DT_NOPREFIX or DT_VCENTER or DT_CENTER);
    end;

  Canvas.Draw(0, 0, FBuffer);
{$R+}
end;

procedure TMiniSpectrum.SetChannel(ch: FMOD_CHANNEL);
begin
  FChannel := ch;
end;

procedure TMiniSpectrum.Paint;
begin
  Draw();
end;

procedure TMiniSpectrum.Resize;
begin
  inherited;

  if Assigned(FBuffer) then
  begin
    FBuffer.Width := Width;
    FBuffer.Height := Height;
  end;
end;

procedure TMiniSpectrum.SetEnabled(Value: Boolean);
begin
  inherited;
  
  //FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit, Value);
end;

procedure TMiniSpectrum.SetStyle(const Value: TSpectrumStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    ZeroMemory(@FValues, SizeOf(FValues));
  end;
end;

end.
