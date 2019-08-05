procedure getBytesAt (var dest; const buf; ofs, len: Integer);
begin
  Move((PChar(@buf)+ofs)^, dest, len);
end;

procedure getWordAt (var dest; const buf; ofs: Integer);
type PWord = ^Word; PByte = ^Byte;
var
  p: PByte;
  d: PWord;
begin
  p := PByte(@buf); Inc(p, ofs);
  d := PWord(@dest);
  d^ := p^;
  Inc(p);
  d^ := (d^) or ((p^) shl 8);
end;

procedure getIntAt (var dest; const buf; ofs: Integer);
type PInt = ^LongWord; PByte = ^Byte;
var
  p: PByte;
  d: PInt;
begin
  p := PByte(@buf); Inc(p, ofs);
  d := PInt(@dest);
  d^ := p^;
  Inc(p);
  d^ := (d^) or ((p^) shl 8);
  Inc(p);
  d^ := (d^) or ((p^) shl 16);
  Inc(p);
  d^ := (d^) or ((p^) shl 24);
end;

procedure putBytesAt (var buf; ofs: Integer; const src; len: Integer);
begin
  Move(src, (PChar(@buf)+ofs)^, len);
end;

procedure putWordAt (var buf; ofs: Integer; const src);
type PWord = ^Word; PByte = ^Byte;
var
  p: PByte;
  d: PWord;
begin
  p := PByte(PChar(@buf)+ofs);
  d := PWord(@src);
  p^ := (d^) and $ff;
  Inc(p);
  p^ := ((d^) shr 8) and $ff;
end;

procedure putIntAt (var buf; ofs: Integer; const src);
type PInt = ^LongWord; PByte = ^Byte;
var
  p: PByte;
  d: PInt;
begin
  p := PByte(PChar(@buf)+ofs);
  d := PInt(@src);
  p^ := (d^) and $ff;
  Inc(p);
  p^ := ((d^) shr 8) and $ff;
  Inc(p);
  p^ := ((d^) shr 16) and $ff;
  Inc(p);
  p^ := ((d^) shr 24) and $ff;
end;

procedure mb_Read_TriggerData (var tr: TTriggerData; ttype: Integer; const buf; bufsize: Integer);
  procedure xreadExit ();
  begin
    getBytesAt(tr.MapName, buf, 0, 16);
  end;

  procedure xreadTeleport ();
  begin
    getIntAt(tr.TargetPoint.x, buf, 0);
    getIntAt(tr.TargetPoint.y, buf, 4);
    getBytesAt(tr.d2d_teleport, buf, 8, 1);
    getBytesAt(tr.silent_teleport, buf, 9, 1);
    getBytesAt(tr.TlpDir, buf, 10, 1);
  end;

  procedure xreadOpendoor ();
  begin
    getIntAt(tr.PanelID, buf, 0);
    getBytesAt(tr.NoSound, buf, 4, 1);
    getBytesAt(tr.d2d_doors, buf, 5, 1);
  end;

  procedure xreadPress ();
  begin
    getIntAt(tr.tX, buf, 0);
    getIntAt(tr.tY, buf, 4);
    getWordAt(tr.tWidth, buf, 8);
    getWordAt(tr.tHeight, buf, 10);
    getWordAt(tr.Wait, buf, 12);
    getWordAt(tr.Count, buf, 14);
    getIntAt(tr.MonsterID, buf, 16);
    getBytesAt(tr.ExtRandom, buf, 20, 1);
  end;

  procedure xreadSecret ();
  begin
  end;

  procedure xreadTexture ();
  begin
    getBytesAt(tr.ActivateOnce, buf, 0, 1);
    getBytesAt(tr.AnimOnce, buf, 1, 1);
  end;

  procedure xreadSound ();
  begin
    getBytesAt(tr.SoundName, buf, 0, 64);
    getBytesAt(tr.Volume, buf, 64, 1);
    getBytesAt(tr.Pan, buf, 65, 1);
    getBytesAt(tr.Local, buf, 66, 1);
    getBytesAt(tr.PlayCount, buf, 67, 1);
    getBytesAt(tr.SoundSwitch, buf, 68, 1);
  end;

  procedure xreadSpawnmonster ();
  begin
    getIntAt(tr.MonPos.x, buf, 0);
    getIntAt(tr.MonPos.y, buf, 4);
    getBytesAt(tr.MonType, buf, 8, 1);
    getIntAt(tr.MonHealth, buf, 12);
    getBytesAt(tr.MonDir, buf, 16, 1);
    getBytesAt(tr.MonActive, buf, 17, 1);
    getIntAt(tr.MonCount, buf, 20);
    getBytesAt(tr.MonEffect, buf, 24, 1);
    getWordAt(tr.MonMax, buf, 26);
    getWordAt(tr.MonDelay, buf, 28);
    getBytesAt(tr.MonBehav, buf, 30, 1);
  end;

  procedure xreadSpawnitem ();
  begin
    getIntAt(tr.ItemPos.x, buf, 0);
    getIntAt(tr.ItemPos.y, buf, 4);
    getBytesAt(tr.ItemType, buf, 8, 1);
    getBytesAt(tr.ItemFalls, buf, 9, 1);
    getBytesAt(tr.ItemOnlyDM, buf, 10, 1);
    getIntAt(tr.ItemCount, buf, 12);
    getBytesAt(tr.ItemEffect, buf, 16, 1);
    getWordAt(tr.ItemMax, buf, 18);
    getWordAt(tr.ItemDelay, buf, 20);
  end;

  procedure xreadMusic ();
  begin
    getBytesAt(tr.MusicName, buf, 0, 64);
    getBytesAt(tr.MusicAction, buf, 64, 1);
  end;

  procedure xreadPush ();
  begin
    getWordAt(tr.PushAngle, buf, 0);
    getBytesAt(tr.PushForce, buf, 2, 1);
    getBytesAt(tr.ResetVel, buf, 3, 1);
  end;

  procedure xreadScore ();
  begin
    getBytesAt(tr.ScoreAction, buf, 0, 1);
    getBytesAt(tr.ScoreCount, buf, 1, 1);
    getBytesAt(tr.ScoreTeam, buf, 2, 1);
    getBytesAt(tr.ScoreCon, buf, 3, 1);
    getBytesAt(tr.ScoreMsg, buf, 4, 1);
  end;

  procedure xreadMessage ();
  begin
    getBytesAt(tr.MessageKind, buf, 0, 1);
    getBytesAt(tr.MessageSendTo, buf, 1, 1);
    getBytesAt(tr.MessageText, buf, 2, 100);
    getWordAt(tr.MessageTime, buf, 102);
  end;

  procedure xreadDamage ();
  begin
    getWordAt(tr.DamageValue, buf, 0);
    getWordAt(tr.DamageInterval, buf, 2);
    getBytesAt(tr.DamageKind, buf, 4, 1);
  end;

  procedure xreadHealth ();
  begin
    getWordAt(tr.HealValue, buf, 0);
    getWordAt(tr.HealInterval, buf, 2);
    getBytesAt(tr.HealMax, buf, 4, 1);
    getBytesAt(tr.HealSilent, buf, 5, 1);
  end;

  procedure xreadShot ();
  begin
    getIntAt(tr.ShotPos.x, buf, 0);
    getIntAt(tr.ShotPos.y, buf, 4);
    getBytesAt(tr.ShotType, buf, 8, 1);
    getBytesAt(tr.ShotTarget, buf, 9, 1);
    getBytesAt(tr.ShotSound, buf, 10, 1);
    getBytesAt(tr.ShotAllMap, buf, 11, 1);
    getIntAt(tr.ShotPanelID, buf, 12);
    getWordAt(tr.ShotIntSight, buf, 16);
    getWordAt(tr.ShotAngle, buf, 18);
    getWordAt(tr.ShotWait, buf, 20);
    getWordAt(tr.ShotAccuracy, buf, 22);
    getWordAt(tr.ShotAmmo, buf, 24);
    getWordAt(tr.ShotIntReload, buf, 26);
  end;

  procedure xreadEffect ();
  begin
    getBytesAt(tr.FXCount, buf, 0, 1);
    getBytesAt(tr.FXType, buf, 1, 1);
    getBytesAt(tr.FXSubType, buf, 2, 1);
    getBytesAt(tr.FXColorR, buf, 3, 1);
    getBytesAt(tr.FXColorG, buf, 4, 1);
    getBytesAt(tr.FXColorB, buf, 5, 1);
    getBytesAt(tr.FXPos, buf, 6, 1);
    getWordAt(tr.FXWait, buf, 8);
    getBytesAt(tr.FXVelX, buf, 10, 1);
    getBytesAt(tr.FXVelY, buf, 11, 1);
    getBytesAt(tr.FXSpreadL, buf, 12, 1);
    getBytesAt(tr.FXSpreadR, buf, 13, 1);
    getBytesAt(tr.FXSpreadU, buf, 14, 1);
    getBytesAt(tr.FXSpreadD, buf, 15, 1);
  end;

  procedure xreadScript ();
  begin
    getBytesAt(tr.SCRProc, buf, 0, 64);
    getIntAt(tr.SCRArg, buf, 64);
  end;

begin
  if (bufsize < 104) then raise Exception.Create('invalid buffer size in mb_Read_TriggerData');
  if (ttype = TRIGGER_EXIT) then begin xreadExit(); exit; end;
  if (ttype = TRIGGER_TELEPORT) then begin xreadTeleport(); exit; end;
  if (ttype = TRIGGER_OPENDOOR) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_CLOSEDOOR) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_DOOR) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_DOOR5) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_CLOSETRAP) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_TRAP) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_LIFTUP) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_LIFTDOWN) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_LIFT) then begin xreadOpendoor(); exit; end;
  if (ttype = TRIGGER_PRESS) then begin xreadPress(); exit; end;
  if (ttype = TRIGGER_ON) then begin xreadPress(); exit; end;
  if (ttype = TRIGGER_OFF) then begin xreadPress(); exit; end;
  if (ttype = TRIGGER_ONOFF) then begin xreadPress(); exit; end;
  if (ttype = TRIGGER_SECRET) then begin xreadSecret(); exit; end;
  if (ttype = TRIGGER_TEXTURE) then begin xreadTexture(); exit; end;
  if (ttype = TRIGGER_SOUND) then begin xreadSound(); exit; end;
  if (ttype = TRIGGER_SPAWNMONSTER) then begin xreadSpawnmonster(); exit; end;
  if (ttype = TRIGGER_SPAWNITEM) then begin xreadSpawnitem(); exit; end;
  if (ttype = TRIGGER_MUSIC) then begin xreadMusic(); exit; end;
  if (ttype = TRIGGER_PUSH) then begin xreadPush(); exit; end;
  if (ttype = TRIGGER_SCORE) then begin xreadScore(); exit; end;
  if (ttype = TRIGGER_MESSAGE) then begin xreadMessage(); exit; end;
  if (ttype = TRIGGER_DAMAGE) then begin xreadDamage(); exit; end;
  if (ttype = TRIGGER_HEALTH) then begin xreadHealth(); exit; end;
  if (ttype = TRIGGER_SHOT) then begin xreadShot(); exit; end;
  if (ttype = TRIGGER_EFFECT) then begin xreadEffect(); exit; end;
  if (ttype = TRIGGER_SCRIPT) then begin xreadScript(); exit; end;
  raise Exception.Create('invalid trigger type in mb_Read_TriggerData');
end;


procedure mb_Read_TMapHeaderRec_1 (var tr: TMapHeaderRec_1; const buf; bufsize: Integer);
  procedure xreadTmapheaderrec_1 ();
  begin
    getBytesAt(tr.MapName, buf, 0, 32);
    getBytesAt(tr.MapAuthor, buf, 32, 32);
    getBytesAt(tr.MapDescription, buf, 64, 256);
    getBytesAt(tr.MusicName, buf, 320, 64);
    getBytesAt(tr.SkyName, buf, 384, 64);
    getWordAt(tr.Width, buf, 448);
    getWordAt(tr.Height, buf, 450);
  end;

begin
  if (bufsize < 452) then raise Exception.Create('invalid buffer size in readTMapHeaderRec_1');
  xreadTmapheaderrec_1();
end;

procedure mb_Read_TTextureRec_1 (var tr: TTextureRec_1; const buf; bufsize: Integer);
  procedure xreadTtexturerec_1 ();
  begin
    getBytesAt(tr.Resource, buf, 0, 64);
    getBytesAt(tr.Anim, buf, 64, 1);
  end;

begin
  if (bufsize < 65) then raise Exception.Create('invalid buffer size in readTTextureRec_1');
  xreadTtexturerec_1();
end;

procedure mb_Read_TPanelRec_1 (var tr: TPanelRec_1; const buf; bufsize: Integer);
  procedure xreadTpanelrec_1 ();
  begin
    getIntAt(tr.X, buf, 0);
    getIntAt(tr.Y, buf, 4);
    getWordAt(tr.Width, buf, 8);
    getWordAt(tr.Height, buf, 10);
    getWordAt(tr.TextureNum, buf, 12);
    getWordAt(tr.PanelType, buf, 14);
    getBytesAt(tr.Alpha, buf, 16, 1);
    getBytesAt(tr.Flags, buf, 17, 1);
  end;

begin
  if (bufsize < 18) then raise Exception.Create('invalid buffer size in readTPanelRec_1');
  xreadTpanelrec_1();
end;

procedure mb_Read_TItemRec_1 (var tr: TItemRec_1; const buf; bufsize: Integer);
  procedure xreadTitemrec_1 ();
  begin
    getIntAt(tr.X, buf, 0);
    getIntAt(tr.Y, buf, 4);
    getBytesAt(tr.ItemType, buf, 8, 1);
    getBytesAt(tr.Options, buf, 9, 1);
  end;

begin
  if (bufsize < 10) then raise Exception.Create('invalid buffer size in readTItemRec_1');
  xreadTitemrec_1();
end;

procedure mb_Read_TMonsterRec_1 (var tr: TMonsterRec_1; const buf; bufsize: Integer);
  procedure xreadTmonsterrec_1 ();
  begin
    getIntAt(tr.X, buf, 0);
    getIntAt(tr.Y, buf, 4);
    getBytesAt(tr.MonsterType, buf, 8, 1);
    getBytesAt(tr.Direction, buf, 9, 1);
  end;

begin
  if (bufsize < 10) then raise Exception.Create('invalid buffer size in readTMonsterRec_1');
  xreadTmonsterrec_1();
end;

procedure mb_Read_TAreaRec_1 (var tr: TAreaRec_1; const buf; bufsize: Integer);
  procedure xreadTarearec_1 ();
  begin
    getIntAt(tr.X, buf, 0);
    getIntAt(tr.Y, buf, 4);
    getBytesAt(tr.AreaType, buf, 8, 1);
    getBytesAt(tr.Direction, buf, 9, 1);
  end;

begin
  if (bufsize < 10) then raise Exception.Create('invalid buffer size in readTAreaRec_1');
  xreadTarearec_1();
end;

procedure mb_Read_TTriggerRec_1 (var tr: TTriggerRec_1; const buf; bufsize: Integer);
  procedure xreadTtriggerrec_1 ();
  begin
    getIntAt(tr.X, buf, 0);
    getIntAt(tr.Y, buf, 4);
    getWordAt(tr.Width, buf, 8);
    getWordAt(tr.Height, buf, 10);
    getBytesAt(tr.Enabled, buf, 12, 1);
    getIntAt(tr.TexturePanel, buf, 13);
    getBytesAt(tr.TriggerType, buf, 17, 1);
    getBytesAt(tr.ActivateType, buf, 18, 1);
    getBytesAt(tr.Keys, buf, 19, 1);
    getBytesAt(tr.DATA, buf, 20, 128);
  end;

begin
  if (bufsize < 148) then raise Exception.Create('invalid buffer size in readTTriggerRec_1');
  xreadTtriggerrec_1();
end;

procedure mb_Write_TriggerData (var buf; bufsize: Integer; ttype: Integer; var tr: TTriggerData);
  procedure xwriteExit ();
  begin
    putBytesAt(buf, 0, tr.MapName, 16);
  end;

  procedure xwriteTeleport ();
  begin
    putIntAt(buf, 0, tr.TargetPoint.x);
    putIntAt(buf, 4, tr.TargetPoint.y);
    putBytesAt(buf, 8, tr.d2d_teleport, 1);
    putBytesAt(buf, 9, tr.silent_teleport, 1);
    putBytesAt(buf, 10, tr.TlpDir, 1);
  end;

  procedure xwriteOpendoor ();
  begin
    putIntAt(buf, 0, tr.PanelID);
    putBytesAt(buf, 4, tr.NoSound, 1);
    putBytesAt(buf, 5, tr.d2d_doors, 1);
  end;

  procedure xwritePress ();
  begin
    putIntAt(buf, 0, tr.tX);
    putIntAt(buf, 4, tr.tY);
    putWordAt(buf, 8, tr.tWidth);
    putWordAt(buf, 10, tr.tHeight);
    putWordAt(buf, 12, tr.Wait);
    putWordAt(buf, 14, tr.Count);
    putIntAt(buf, 16, tr.MonsterID);
    putBytesAt(buf, 20, tr.ExtRandom, 1);
  end;

  procedure xwriteSecret ();
  begin
  end;

  procedure xwriteTexture ();
  begin
    putBytesAt(buf, 0, tr.ActivateOnce, 1);
    putBytesAt(buf, 1, tr.AnimOnce, 1);
  end;

  procedure xwriteSound ();
  begin
    putBytesAt(buf, 0, tr.SoundName, 64);
    putBytesAt(buf, 64, tr.Volume, 1);
    putBytesAt(buf, 65, tr.Pan, 1);
    putBytesAt(buf, 66, tr.Local, 1);
    putBytesAt(buf, 67, tr.PlayCount, 1);
    putBytesAt(buf, 68, tr.SoundSwitch, 1);
  end;

  procedure xwriteSpawnmonster ();
  begin
    putIntAt(buf, 0, tr.MonPos.x);
    putIntAt(buf, 4, tr.MonPos.y);
    putBytesAt(buf, 8, tr.MonType, 1);
    putIntAt(buf, 12, tr.MonHealth);
    putBytesAt(buf, 16, tr.MonDir, 1);
    putBytesAt(buf, 17, tr.MonActive, 1);
    putIntAt(buf, 20, tr.MonCount);
    putBytesAt(buf, 24, tr.MonEffect, 1);
    putWordAt(buf, 26, tr.MonMax);
    putWordAt(buf, 28, tr.MonDelay);
    putBytesAt(buf, 30, tr.MonBehav, 1);
  end;

  procedure xwriteSpawnitem ();
  begin
    putIntAt(buf, 0, tr.ItemPos.x);
    putIntAt(buf, 4, tr.ItemPos.y);
    putBytesAt(buf, 8, tr.ItemType, 1);
    putBytesAt(buf, 9, tr.ItemFalls, 1);
    putBytesAt(buf, 10, tr.ItemOnlyDM, 1);
    putIntAt(buf, 12, tr.ItemCount);
    putBytesAt(buf, 16, tr.ItemEffect, 1);
    putWordAt(buf, 18, tr.ItemMax);
    putWordAt(buf, 20, tr.ItemDelay);
  end;

  procedure xwriteMusic ();
  begin
    putBytesAt(buf, 0, tr.MusicName, 64);
    putBytesAt(buf, 64, tr.MusicAction, 1);
  end;

  procedure xwritePush ();
  begin
    putWordAt(buf, 0, tr.PushAngle);
    putBytesAt(buf, 2, tr.PushForce, 1);
    putBytesAt(buf, 3, tr.ResetVel, 1);
  end;

  procedure xwriteScore ();
  begin
    putBytesAt(buf, 0, tr.ScoreAction, 1);
    putBytesAt(buf, 1, tr.ScoreCount, 1);
    putBytesAt(buf, 2, tr.ScoreTeam, 1);
    putBytesAt(buf, 3, tr.ScoreCon, 1);
    putBytesAt(buf, 4, tr.ScoreMsg, 1);
  end;

  procedure xwriteMessage ();
  begin
    putBytesAt(buf, 0, tr.MessageKind, 1);
    putBytesAt(buf, 1, tr.MessageSendTo, 1);
    putBytesAt(buf, 2, tr.MessageText, 100);
    putWordAt(buf, 102, tr.MessageTime);
  end;

  procedure xwriteDamage ();
  begin
    putWordAt(buf, 0, tr.DamageValue);
    putWordAt(buf, 2, tr.DamageInterval);
    putBytesAt(buf, 4, tr.DamageKind, 1);
  end;

  procedure xwriteHealth ();
  begin
    putWordAt(buf, 0, tr.HealValue);
    putWordAt(buf, 2, tr.HealInterval);
    putBytesAt(buf, 4, tr.HealMax, 1);
    putBytesAt(buf, 5, tr.HealSilent, 1);
  end;

  procedure xwriteShot ();
  begin
    putIntAt(buf, 0, tr.ShotPos.x);
    putIntAt(buf, 4, tr.ShotPos.y);
    putBytesAt(buf, 8, tr.ShotType, 1);
    putBytesAt(buf, 9, tr.ShotTarget, 1);
    putBytesAt(buf, 10, tr.ShotSound, 1);
    putBytesAt(buf, 11, tr.ShotAllMap, 1);
    putIntAt(buf, 12, tr.ShotPanelID);
    putWordAt(buf, 16, tr.ShotIntSight);
    putWordAt(buf, 18, tr.ShotAngle);
    putWordAt(buf, 20, tr.ShotWait);
    putWordAt(buf, 22, tr.ShotAccuracy);
    putWordAt(buf, 24, tr.ShotAmmo);
    putWordAt(buf, 26, tr.ShotIntReload);
  end;

  procedure xwriteEffect ();
  begin
    putBytesAt(buf, 0, tr.FXCount, 1);
    putBytesAt(buf, 1, tr.FXType, 1);
    putBytesAt(buf, 2, tr.FXSubType, 1);
    putBytesAt(buf, 3, tr.FXColorR, 1);
    putBytesAt(buf, 4, tr.FXColorG, 1);
    putBytesAt(buf, 5, tr.FXColorB, 1);
    putBytesAt(buf, 6, tr.FXPos, 1);
    putWordAt(buf, 8, tr.FXWait);
    putBytesAt(buf, 10, tr.FXVelX, 1);
    putBytesAt(buf, 11, tr.FXVelY, 1);
    putBytesAt(buf, 12, tr.FXSpreadL, 1);
    putBytesAt(buf, 13, tr.FXSpreadR, 1);
    putBytesAt(buf, 14, tr.FXSpreadU, 1);
    putBytesAt(buf, 15, tr.FXSpreadD, 1);
  end;

begin
  if (bufsize < 104) then raise Exception.Create('invalid buffer size in mb_Write_TriggerData');
  if (ttype = TRIGGER_EXIT) then begin xwriteExit(); exit; end;
  if (ttype = TRIGGER_TELEPORT) then begin xwriteTeleport(); exit; end;
  if (ttype = TRIGGER_OPENDOOR) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_CLOSEDOOR) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_DOOR) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_DOOR5) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_CLOSETRAP) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_TRAP) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_LIFTUP) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_LIFTDOWN) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_LIFT) then begin xwriteOpendoor(); exit; end;
  if (ttype = TRIGGER_PRESS) then begin xwritePress(); exit; end;
  if (ttype = TRIGGER_ON) then begin xwritePress(); exit; end;
  if (ttype = TRIGGER_OFF) then begin xwritePress(); exit; end;
  if (ttype = TRIGGER_ONOFF) then begin xwritePress(); exit; end;
  if (ttype = TRIGGER_SECRET) then begin xwriteSecret(); exit; end;
  if (ttype = TRIGGER_TEXTURE) then begin xwriteTexture(); exit; end;
  if (ttype = TRIGGER_SOUND) then begin xwriteSound(); exit; end;
  if (ttype = TRIGGER_SPAWNMONSTER) then begin xwriteSpawnmonster(); exit; end;
  if (ttype = TRIGGER_SPAWNITEM) then begin xwriteSpawnitem(); exit; end;
  if (ttype = TRIGGER_MUSIC) then begin xwriteMusic(); exit; end;
  if (ttype = TRIGGER_PUSH) then begin xwritePush(); exit; end;
  if (ttype = TRIGGER_SCORE) then begin xwriteScore(); exit; end;
  if (ttype = TRIGGER_MESSAGE) then begin xwriteMessage(); exit; end;
  if (ttype = TRIGGER_DAMAGE) then begin xwriteDamage(); exit; end;
  if (ttype = TRIGGER_HEALTH) then begin xwriteHealth(); exit; end;
  if (ttype = TRIGGER_SHOT) then begin xwriteShot(); exit; end;
  if (ttype = TRIGGER_EFFECT) then begin xwriteEffect(); exit; end;
  raise Exception.Create('invalid trigger type in mb_Write_TriggerData');
end;


procedure mb_Write_TMapHeaderRec_1 (var buf; bufsize: Integer; var tr: TMapHeaderRec_1);
  procedure xwriteTmapheaderrec_1 ();
  begin
    putBytesAt(buf, 0, tr.MapName, 32);
    putBytesAt(buf, 32, tr.MapAuthor, 32);
    putBytesAt(buf, 64, tr.MapDescription, 256);
    putBytesAt(buf, 320, tr.MusicName, 64);
    putBytesAt(buf, 384, tr.SkyName, 64);
    putWordAt(buf, 448, tr.Width);
    putWordAt(buf, 450, tr.Height);
  end;

begin
  if (bufsize < 452) then raise Exception.Create('invalid buffer size in writeTMapHeaderRec_1');
  xwriteTmapheaderrec_1();
end;

procedure mb_Write_TTextureRec_1 (var buf; bufsize: Integer; var tr: TTextureRec_1);
  procedure xwriteTtexturerec_1 ();
  begin
    putBytesAt(buf, 0, tr.Resource, 64);
    putBytesAt(buf, 64, tr.Anim, 1);
  end;

begin
  if (bufsize < 65) then raise Exception.Create('invalid buffer size in writeTTextureRec_1');
  xwriteTtexturerec_1();
end;

procedure mb_Write_TPanelRec_1 (var buf; bufsize: Integer; var tr: TPanelRec_1);
  procedure xwriteTpanelrec_1 ();
  begin
    putIntAt(buf, 0, tr.X);
    putIntAt(buf, 4, tr.Y);
    putWordAt(buf, 8, tr.Width);
    putWordAt(buf, 10, tr.Height);
    putWordAt(buf, 12, tr.TextureNum);
    putWordAt(buf, 14, tr.PanelType);
    putBytesAt(buf, 16, tr.Alpha, 1);
    putBytesAt(buf, 17, tr.Flags, 1);
  end;

begin
  if (bufsize < 18) then raise Exception.Create('invalid buffer size in writeTPanelRec_1');
  xwriteTpanelrec_1();
end;

procedure mb_Write_TItemRec_1 (var buf; bufsize: Integer; var tr: TItemRec_1);
  procedure xwriteTitemrec_1 ();
  begin
    putIntAt(buf, 0, tr.X);
    putIntAt(buf, 4, tr.Y);
    putBytesAt(buf, 8, tr.ItemType, 1);
    putBytesAt(buf, 9, tr.Options, 1);
  end;

begin
  if (bufsize < 10) then raise Exception.Create('invalid buffer size in writeTItemRec_1');
  xwriteTitemrec_1();
end;

procedure mb_Write_TMonsterRec_1 (var buf; bufsize: Integer; var tr: TMonsterRec_1);
  procedure xwriteTmonsterrec_1 ();
  begin
    putIntAt(buf, 0, tr.X);
    putIntAt(buf, 4, tr.Y);
    putBytesAt(buf, 8, tr.MonsterType, 1);
    putBytesAt(buf, 9, tr.Direction, 1);
  end;

begin
  if (bufsize < 10) then raise Exception.Create('invalid buffer size in writeTMonsterRec_1');
  xwriteTmonsterrec_1();
end;

procedure mb_Write_TAreaRec_1 (var buf; bufsize: Integer; var tr: TAreaRec_1);
  procedure xwriteTarearec_1 ();
  begin
    putIntAt(buf, 0, tr.X);
    putIntAt(buf, 4, tr.Y);
    putBytesAt(buf, 8, tr.AreaType, 1);
    putBytesAt(buf, 9, tr.Direction, 1);
  end;

begin
  if (bufsize < 10) then raise Exception.Create('invalid buffer size in writeTAreaRec_1');
  xwriteTarearec_1();
end;

procedure mb_Write_TTriggerRec_1 (var buf; bufsize: Integer; var tr: TTriggerRec_1);
  procedure xwriteTtriggerrec_1 ();
  begin
    putIntAt(buf, 0, tr.X);
    putIntAt(buf, 4, tr.Y);
    putWordAt(buf, 8, tr.Width);
    putWordAt(buf, 10, tr.Height);
    putBytesAt(buf, 12, tr.Enabled, 1);
    putIntAt(buf, 13, tr.TexturePanel);
    putBytesAt(buf, 17, tr.TriggerType, 1);
    putBytesAt(buf, 18, tr.ActivateType, 1);
    putBytesAt(buf, 19, tr.Keys, 1);
    putBytesAt(buf, 20, tr.DATA, 128);
  end;

begin
  if (bufsize < 148) then raise Exception.Create('invalid buffer size in writeTTriggerRec_1');
  xwriteTtriggerrec_1();
end;

