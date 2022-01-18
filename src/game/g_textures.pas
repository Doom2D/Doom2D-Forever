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
  g_base, MAPDEF;

type
  TLevelTexture = record
    TextureName: AnsiString; // as stored in wad
    FullName: AnsiString; // full path to texture // !!! merge it with TextureName
    framesCount, speed: Byte;
  end;

  TLevelTextureArray = array of TLevelTexture;

  TAnimationState = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
  private
    mCounter: Byte; // Счетчик ожидания между кадрами
    mSpeed: Byte; // Время ожидания между кадрами
    mCurrentFrame: Integer; // Текущий кадр (начиная с 0)
    mLoop: Boolean; // Переходить на первый кадр после последнего?
    mEnabled: Boolean; // Работа разрешена?
    mPlayed: Boolean; // Проиграна вся хотя бы раз?
    mMinLength: Byte; // Ожидание после проигрывания
    mRevert: Boolean; // Смена кадров обратная?

    mLength: Integer;

  public
    constructor Create (aloop: Boolean; aspeed: Byte; len: Integer);
    destructor  Destroy (); override;

    procedure reset ();
    procedure update ();
    procedure enable ();
    procedure disable ();
    procedure revert (r: Boolean);

    procedure saveState (st: TStream; mAlpha: Byte; mBlending: Boolean);
    procedure loadState (st: TStream; out mAlpha: Byte; out mBlending: Boolean);

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
    property length: Integer read mLength;
  end;

implementation

uses
  g_game, e_log, g_basic, g_console, wadreader,
  g_language, utils, xstreams;

constructor TAnimationState.Create (aloop: Boolean; aspeed: Byte; len: Integer);
begin
  assert(len >= 0);
  mLength := len;

  mMinLength := 0;
  mLoop := aloop;
  mSpeed := aspeed;
  mEnabled := true;
  mCurrentFrame := 0;
  mPlayed := false;
end;

destructor TAnimationState.Destroy;
begin
  inherited;
end;

procedure TAnimationState.update;
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
        if (mLength * mSpeed + mCounter < mMinLength) then exit;
      end;

      mCurrentFrame -= 1;
      mPlayed := (mCurrentFrame < 0);

      // Повторять ли анимацию по кругу?
      if mPlayed then
      begin
        if mLoop then
          mCurrentFrame := mLength - 1
        else
          mCurrentFrame += 1
      end;

      mCounter := 0;
    end
    else
    begin
      // Прямой порядок кадров
      // Дошли до конца анимации. Возможно, ждем еще
      if (mCurrentFrame = mLength - 1) then
      begin
        if (mLength * mSpeed + mCounter < mMinLength) then exit;
      end;

      mCurrentFrame += 1;
      mPlayed := (mCurrentFrame > mLength - 1);

      // Повторять ли анимацию по кругу?
      if mPlayed then
      begin
        if mLoop then mCurrentFrame := 0 else mCurrentFrame -= 1;
      end;

      mCounter := 0;
    end;
  end;
end;

procedure TAnimationState.reset;
begin
  if mRevert then
    mCurrentFrame := mLength - 1
  else
    mCurrentFrame := 0;
  mCounter := 0;
  mPlayed := false
end;

procedure TAnimationState.disable;
begin
  mEnabled := false
end;

procedure TAnimationState.enable;
begin
  mEnabled := true
end;

procedure TAnimationState.revert (r: Boolean);
begin
  mRevert := r;
  reset
end;

function TAnimationState.totalFrames (): Integer; inline;
begin
  result := mLength
end;

procedure TAnimationState.saveState (st: TStream; mAlpha: Byte; mBlending: Boolean);
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


procedure TAnimationState.loadState (st: TStream; out mAlpha: Byte; out mBlending: Boolean);
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
