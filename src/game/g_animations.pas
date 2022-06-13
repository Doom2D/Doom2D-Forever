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
unit g_animations;

interface

  uses Classes;

  type
    TAnimState = record
      private
        mCounter: Byte;         // delay counter (normally [0..mSpeed])
        mSpeed: Byte;           // delay between frames
        mCurrentFrame: Integer; // current frame (normally [0..mLength - 1])
        mLoop: Boolean;         // looped animation
        mEnabled: Boolean;      // allow update state
        mPlayed: Boolean;       // anmation played at least once
        mMinLength: Byte;       // delay at animation end
        mRevert: Boolean;       // reverse play
        mLength: Integer;       // total frames (normally mLength > 0)

      public
        constructor Create (aloop: Boolean; aspeed: Byte; len: Integer);
        procedure Invalidate;

        procedure Reset;
        procedure Update;
        procedure Enable;
        procedure Disable;
        procedure Revert (r: Boolean);

        procedure SaveState (st: TStream; mAlpha: Byte; mBlending: Boolean);
        procedure LoadState (st: TStream; out mAlpha: Byte; out mBlending: Boolean);

        function TotalFrames (): Integer; inline;
        function IsInvalid (): Boolean; inline;
        function IsValid (): Boolean; inline;

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

  type
    TAnimInfo = record
      loop: Boolean; (* loop animation normalization *)
      delay: Byte;   (* delay between frames [1..255] *)
      frames: Word;  (* number of frames in animation stream [1..65535] *)
      back: Boolean; (* back animation normalization *)
    end;

  function g_Anim_GetTotalFrames (const a: TAnimInfo): LongWord;
  function g_Anim_GetTotalTime (const a: TAnimInfo): LongWord;
  function g_Anim_GetCountByTime (const a: TAnimInfo; time: LongWord): LongInt;
  procedure g_Anim_GetFrameByTime (const a: TAnimInfo; time: LongWord; out count, frame: LongInt);
  procedure g_Anim_GetState (const anim: TAnimInfo; time: LongWord; out state: TAnimState);

implementation

  uses Math, utils, xstreams;

  constructor TAnimState.Create (aloop: Boolean; aspeed: Byte; len: Integer);
  begin
    ASSERT(len >= 0);

    self := Default(TAnimState);
    self.mLength := len;
    self.mMinLength := 0;
    self.mLoop := aloop;
    self.mSpeed := aspeed;
    self.mEnabled := true;
    self.mCurrentFrame := 0;
    self.mPlayed := false;
  end;

  procedure TAnimState.Invalidate;
  begin
    self := Default(TAnimState);
  end;

  procedure TAnimState.Update;
  begin
    ASSERT(self.IsValid());
    if self.mEnabled then
    begin
      INC(self.mCounter);
      if self.mCounter >= self.mSpeed then
      begin
        if self.mRevert then
        begin
          if (self.mCurrentFrame <> 0) or (mLength * mSpeed + mCounter >= mMinLength) then
          begin
            DEC(self.mCurrentFrame);
            self.mPlayed := self.mCurrentFrame < 0;
            if self.mPlayed then
            begin
              if self.mLoop then self.mCurrentFrame := self.mLength - 1 else INC(self.mCurrentFrame);
            end;
            self.mCounter := 0;
          end;
        end
        else
        begin
          if (self.mCurrentFrame <> self.mLength - 1) or (mLength * mSpeed + mCounter >= mMinLength) then
          begin
            INC(self.mCurrentFrame);
            self.mPlayed := self.mCurrentFrame > self.mLength - 1;
            if self.mPlayed then
            begin
              if self.mLoop then self.mCurrentFrame := 0 else DEC(self.mCurrentFrame);
            end;
            self.mCounter := 0;
          end;
        end;
      end;
    end;
  end;

  procedure TAnimState.Reset;
  begin
    ASSERT(self.IsValid());
    if self.mRevert then self.mCurrentFrame := self.mLength - 1 else self.mCurrentFrame := 0;
    self.mCounter := 0;
    self.mPlayed := false;
  end;

  procedure TAnimState.Disable;
  begin
    ASSERT(self.IsValid());
    self.mEnabled := false;
  end;

  procedure TAnimState.Enable;
  begin
    ASSERT(self.IsValid());
    self.mEnabled := true;
  end;

  procedure TAnimState.revert (r: Boolean);
  begin
    ASSERT(self.IsValid());
    self.mRevert := r;
    self.Reset;
  end;

  function TAnimState.TotalFrames (): Integer;
  begin
    ASSERT(self.IsValid());
    result := self.mLength;
  end;

  function TAnimState.IsInvalid (): Boolean;
  begin
    result := self.mLength <= 0
  end;

  function TAnimState.IsValid (): Boolean;
  begin
    result := self.mLength > 0;
  end;

  procedure TAnimState.SaveState (st: TStream; mAlpha: Byte; mBlending: Boolean);
  begin
    if st <> nil then
    begin
      utils.writeSign(st, 'ANIM');
      utils.writeInt(st, Byte(0)); // version
      utils.writeInt(st, Byte(mCounter));
      utils.writeInt(st, LongInt(mCurrentFrame));
      utils.writeBool(st, mPlayed);
      utils.writeInt(st, Byte(mAlpha));
      utils.writeInt(st, Byte(mBlending));
      utils.writeInt(st, Byte(mSpeed));
      utils.writeBool(st, mLoop);
      utils.writeBool(st, mEnabled);
      utils.writeInt(st, Byte(mMinLength));
      utils.writeBool(st, mRevert);
    end;
  end;

  procedure TAnimState.LoadState (st: TStream; out mAlpha: Byte; out mBlending: Boolean);
  begin
    if st <> nil then
    begin
      if utils.checkSign(st, 'ANIM') = false then
        raise XStreamError.Create('animation chunk expected');
      if utils.readByte(st) <> 0 then
        raise XStreamError.Create('invalid animation chunk version');
      mCounter := utils.readByte(st);
      mCurrentFrame := utils.readLongInt(st);
      mPlayed := utils.readBool(st);
      mAlpha := utils.readByte(st);
      mBlending := utils.readBool(st);
      mSpeed := utils.readByte(st);
      mLoop := utils.readBool(st);
      mEnabled := utils.readBool(st);
      mMinLength := utils.readByte(st);
      mRevert := utils.readBool(st);
    end;
  end;

  function g_Anim_GetTotalFrames (const a: TAnimInfo): LongWord;
  begin
    ASSERT(a.frames > 0);
    ASSERT(a.delay > 0);
    if a.back then result := MAX(1, a.frames * 2 - 2) else result := a.frames;
  end;

  function g_Anim_GetTotalTime (const a: TAnimInfo): LongWord;
  begin
    ASSERT(a.frames > 0);
    ASSERT(a.delay > 0);
    result := g_Anim_GetTotalFrames(a) * a.delay;
  end;

  function g_Anim_GetCountByTime (const a: TAnimInfo; time: LongWord): LongInt;
    var n, f, t: LongWord;
  begin
    ASSERT(a.frames > 0);
    ASSERT(a.delay > 0);
    n := g_Anim_GetTotalFrames(a);
    t := g_Anim_GetTotalTime(a);
    f := n * time div t;
    if a.loop then result := f div n
    else if f >= n then result := 1
    else result := 0;
  end;

  procedure g_Anim_GetFrameByTime (const a: TAnimInfo; time: LongWord; out count, frame: LongInt);
    var n, f, t: LongWord;
  begin
    ASSERT(a.frames > 0);
    ASSERT(a.delay > 0);
    (* 1. Get total number frames for one animation cycle *)
    n := g_Anim_GetTotalFrames(a);
    (* 2. Get time for one animation cycle *)
    t := g_Anim_GetTotalTime(a);
    (* 3. Get frame for specified time *)
    f := n * time div t;
    (* 4. Get how many times is played *)
    if a.loop then count := f div n
    else if f >= n then count := 1
    else count := 0;
    (* 5. Normalize loop animation *)
    if a.loop then f := f mod n else f := MIN(f, n - 1);
    (* 6. Normalize back animation *)
    if a.back and (f >= a.frames) then f := n - f;
    frame := f;
  end;

  procedure g_Anim_GetState (const anim: TAnimInfo; time: LongWord; out state: TAnimState);
    var count, frame: LongInt; a: TAnimInfo;
  begin
    ASSERT(anim.frames > 0);
    ASSERT(anim.delay > 0);
    a := anim;
    if a.back then
    begin
      a.frames := MAX(1, a.frames * 2 - 2);
      a.back := false;
    end;
    g_Anim_GetFrameByTime(a, time, count, frame);
    state := TAnimState.Create(a.loop, a.delay, a.frames);
    state.mCounter := time MOD a.delay;
    state.mCurrentFrame := frame;
    state.mPlayed := count >= 1;
  end;

end.
