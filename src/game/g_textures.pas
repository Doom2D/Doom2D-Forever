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
{$INCLUDE ../shared/a_modes.inc}
unit g_textures;

interface

uses
  SysUtils, Classes,
  {$IFDEF USE_MEMPOOL}mempool,{$ENDIF}
  g_base, r_graphics, MAPDEF, ImagingTypes, Imaging, ImagingUtility;

type
  TLevelTexture = record
    textureName: AnsiString;
    width, height: Word;
    case anim: Boolean of
      false: (textureID: LongWord);
      true: (framesID: LongWord; framesCount: Byte; speed: Byte);
  end;

  TLevelTextureArray = array of TLevelTexture;

  TAnimation = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    mId: LongWord;
    mAlpha: Byte;
    mBlending: Boolean;
    mCounter: Byte; // Счетчик ожидания между кадрами
    mSpeed: Byte; // Время ожидания между кадрами
    mCurrentFrame: Integer; // Текущий кадр (начиная с 0)
    mLoop: Boolean; // Переходить на первый кадр после последнего?
    mEnabled: Boolean; // Работа разрешена?
    mPlayed: Boolean; // Проиграна вся хотя бы раз?
    mHeight: Word;
    mWidth: Word;
    mMinLength: Byte; // Ожидание после проигрывания
    mRevert: Boolean; // Смена кадров обратная?

  public
    constructor Create (aframesID: LongWord; aloop: Boolean; aspeed: Byte);
    destructor  Destroy (); override;

    procedure reset ();
    procedure update ();
    procedure enable ();
    procedure disable ();
    procedure revert (r: Boolean);

    procedure saveState (st: TStream);
    procedure loadState (st: TStream);

    function totalFrames (): Integer; inline;

  public
    property played: Boolean read mPlayed;
    property enabled: Boolean read mEnabled;
    property isReverse: Boolean read mRevert;
    property loop: Boolean read mLoop write mLoop;
    property speed: Byte read mSpeed write mSpeed;
    property minLength: Byte read mMinLength write mMinLength;
    property currentFrame: Integer read mCurrentFrame write mCurrentFrame;
    property currentCounter: Byte read mCounter write mCounter;
    property counter: Byte read mCounter;
    property blending: Boolean read mBlending write mBlending;
    property alpha: Byte read mAlpha write mAlpha;
    property framesId: LongWord read mId;
    property width: Word read mWidth;
    property height: Word read mHeight;

    property id: LongWord read mId;
  end;

implementation

uses
  g_game, e_log, g_basic, g_console, wadreader, r_animations,
  g_language, utils, xstreams;

constructor TAnimation.Create (aframesID: LongWord; aloop: Boolean; aspeed: Byte);
begin
  if (aframesID >= Length(framesArray)) then
  begin
    //raise Exception.Create('trying to create inexisting frame: something is very wrong here');
    e_LogWritefln('trying to create inexisting frame %u of %u: something is very wrong here', [aframesID, LongWord(Length(framesArray))], TMsgType.Warning);
    aframesID := 0;
    if (Length(framesArray) = 0) then raise Exception.Create('trying to create inexisting frame: something is very wrong here');
  end;
  mId := aframesID;
  mMinLength := 0;
  mLoop := aloop;
  mSpeed := aspeed;
  mEnabled := true;
  mCurrentFrame := 0;
  mPlayed := false;
  mAlpha := 0;
  mWidth := framesArray[mId].FrameWidth;
  mHeight := framesArray[mId].FrameHeight;
end;


destructor TAnimation.Destroy ();
begin
  inherited;
end;


procedure TAnimation.update ();
begin
  if (not mEnabled) then exit;

  mCounter += 1;

  if (mCounter >= mSpeed) then
  begin
    // Ожидание между кадрами закончилось
    // Обратный порядок кадров?
    if mRevert then
    begin
      // Дошли до конца анимации. Возможно, ждем еще
      if (mCurrentFrame = 0) then
      begin
        if (Length(framesArray[mId].TexturesID)*mSpeed+mCounter < mMinLength) then exit;
      end;

      mCurrentFrame -= 1;
      mPlayed := (mCurrentFrame < 0);

      // Повторять ли анимацию по кругу?
      if mPlayed then
      begin
        if mLoop then mCurrentFrame := High(framesArray[mId].TexturesID) else mCurrentFrame += 1;
      end;

      mCounter := 0;
    end
    else
    begin
      // Прямой порядок кадров
      // Дошли до конца анимации. Возможно, ждем еще
      if (mCurrentFrame = High(framesArray[mId].TexturesID)) then
      begin
        if (Length(framesArray[mId].TexturesID)*mSpeed+mCounter < mMinLength) then exit;
      end;

      mCurrentFrame += 1;
      mPlayed := (mCurrentFrame > High(framesArray[mId].TexturesID));

      // Повторять ли анимацию по кругу?
      if mPlayed then
      begin
        if mLoop then mCurrentFrame := 0 else mCurrentFrame -= 1;
      end;

      mCounter := 0;
    end;
  end;
end;


procedure TAnimation.reset ();
begin
  if mRevert then mCurrentFrame := High(framesArray[mId].TexturesID) else mCurrentFrame := 0;
  mCounter := 0;
  mPlayed := false;
end;


procedure TAnimation.disable (); begin mEnabled := false; end;
procedure TAnimation.enable (); begin mEnabled := true; end;


function TAnimation.totalFrames (): Integer; inline; begin result := Length(framesArray[mId].TexturesID); end;


procedure TAnimation.revert (r: Boolean);
begin
  mRevert := r;
  reset();
end;


procedure TAnimation.saveState (st: TStream);
begin
  if (st = nil) then exit;

  utils.writeSign(st, 'ANIM');
  utils.writeInt(st, Byte(0)); // version
  // Счетчик ожидания между кадрами
  utils.writeInt(st, Byte(mCounter));
  // Текущий кадр
  utils.writeInt(st, LongInt(mCurrentFrame));
  // Проиграна ли анимация целиком
  utils.writeBool(st, mPlayed);
  // Alpha-канал всей текстуры
  utils.writeInt(st, Byte(mAlpha));
  // Размытие текстуры
  utils.writeInt(st, Byte(mBlending));
  // Время ожидания между кадрами
  utils.writeInt(st, Byte(mSpeed));
  // Зациклена ли анимация
  utils.writeBool(st, mLoop);
  // Включена ли
  utils.writeBool(st, mEnabled);
  // Ожидание после проигрывания
  utils.writeInt(st, Byte(mMinLength));
  // Обратный ли порядок кадров
  utils.writeBool(st, mRevert);
end;


procedure TAnimation.loadState (st: TStream);
begin
  if (st = nil) then exit;

  if not utils.checkSign(st, 'ANIM') then raise XStreamError.Create('animation chunk expected');
  if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid animation chunk version');
  // Счетчик ожидания между кадрами
  mCounter := utils.readByte(st);
  // Текущий кадр
  mCurrentFrame := utils.readLongInt(st);
  // Проиграна ли анимация целиком
  mPlayed := utils.readBool(st);
  // Alpha-канал всей текстуры
  mAlpha := utils.readByte(st);
  // Размытие текстуры
  mBlending := utils.readBool(st);
  // Время ожидания между кадрами
  mSpeed := utils.readByte(st);
  // Зациклена ли анимация
  mLoop := utils.readBool(st);
  // Включена ли
  mEnabled := utils.readBool(st);
  // Ожидание после проигрывания
  mMinLength := utils.readByte(st);
  // Обратный ли порядок кадров
  mRevert := utils.readBool(st);
end;

end.
