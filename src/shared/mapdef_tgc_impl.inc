// trigger cache loader
// set `TriggerType` in `tgt` before calling this
procedure trigUpdateCacheData (var tgt: TTrigger; tdata: TDynRecord);
begin
  case tgt.TriggerType of
    TRIGGER_EXIT:
      begin
        tgt.tgcMap := tdata.trigMap;
      end;
    TRIGGER_TELEPORT:
      begin
        tgt.tgcTarget := tdata.trigTarget;
        tgt.tgcD2d := tdata.trigD2d;
        tgt.tgcSilent := tdata.trigSilent;
        tgt.tgcDirection := tdata.trigDirection;
      end;
    TRIGGER_OPENDOOR,
    TRIGGER_CLOSEDOOR,
    TRIGGER_DOOR,
    TRIGGER_DOOR5,
    TRIGGER_CLOSETRAP,
    TRIGGER_TRAP,
    TRIGGER_LIFTUP,
    TRIGGER_LIFTDOWN,
    TRIGGER_LIFT:
      begin
        tgt.tgcSilent := tdata.trigSilent;
        tgt.tgcD2d := tdata.trigD2d;
      end;
    TRIGGER_PRESS,
    TRIGGER_ON,
    TRIGGER_OFF,
    TRIGGER_ONOFF:
      begin
        tgt.tgcTX := tdata.trigTX;
        tgt.tgcTY := tdata.trigTY;
        tgt.tgcTWidth := tdata.trigTWidth;
        tgt.tgcTHeight := tdata.trigTHeight;
        tgt.tgcWait := tdata.trigWait;
        tgt.tgcPressCount := tdata.trigPressCount;
        tgt.tgcExtRandom := tdata.trigExtRandom;
        tgt.tgcSilent := tdata.trigSilent;
        tgt.tgcSound := tdata.trigSound;
      end;
    TRIGGER_SECRET:
      begin
      end;
    TRIGGER_TEXTURE:
      begin
        tgt.tgcActivateOnce := tdata.trigActivateOnce;
        tgt.tgcAnimateOnce := tdata.trigAnimateOnce;
      end;
    TRIGGER_SOUND:
      begin
        tgt.tgcSoundName := tdata.trigSoundName;
        tgt.tgcVolume := tdata.trigVolume;
        tgt.tgcPan := tdata.trigPan;
        tgt.tgcLocal := tdata.trigLocal;
        tgt.tgcPlayCount := tdata.trigPlayCount;
        tgt.tgcSoundSwitch := tdata.trigSoundSwitch;
      end;
    TRIGGER_SPAWNMONSTER:
      begin
        tgt.tgcTX := tdata.trigTX;
        tgt.tgcTY := tdata.trigTY;
        tgt.tgcSpawnMonsType := tdata.trigSpawnMonsType;
        tgt.tgcHealth := tdata.trigHealth;
        tgt.tgcDirection := tdata.trigDirection;
        tgt.tgcActive := tdata.trigActive;
        tgt.tgcMonsCount := tdata.trigMonsCount;
        tgt.tgcEffect := tdata.trigEffect;
        tgt.tgcMax := tdata.trigMax;
        tgt.tgcDelay := tdata.trigDelay;
        tgt.tgcBehaviour := tdata.trigBehaviour;
      end;
    TRIGGER_SPAWNITEM:
      begin
        tgt.tgcTX := tdata.trigTX;
        tgt.tgcTY := tdata.trigTY;
        tgt.tgcSpawnItemType := tdata.trigSpawnItemType;
        tgt.tgcGravity := tdata.trigGravity;
        tgt.tgcDmonly := tdata.trigDmonly;
        tgt.tgcItemCount := tdata.trigItemCount;
        tgt.tgcEffect := tdata.trigEffect;
        tgt.tgcMax := tdata.trigMax;
        tgt.tgcDelay := tdata.trigDelay;
      end;
    TRIGGER_MUSIC:
      begin
        tgt.tgcMusicName := tdata.trigMusicName;
        tgt.tgcMusicAction := tdata.trigMusicAction;
      end;
    TRIGGER_PUSH:
      begin
        tgt.tgcAngle := tdata.trigAngle;
        tgt.tgcForce := tdata.trigForce;
        tgt.tgcResetVelocity := tdata.trigResetVelocity;
      end;
    TRIGGER_SCORE:
      begin
        tgt.tgcScoreAction := tdata.trigScoreAction;
        tgt.tgcScoreCount := tdata.trigScoreCount;
        tgt.tgcScoreTeam := tdata.trigScoreTeam;
        tgt.tgcScoreCon := tdata.trigScoreCon;
        tgt.tgcScoreMsg := tdata.trigScoreMsg;
      end;
    TRIGGER_MESSAGE:
      begin
        tgt.tgcKind := tdata.trigKind;
        tgt.tgcMsgDest := tdata.trigMsgDest;
        tgt.tgcText := tdata.trigText;
        tgt.tgcMsgTime := tdata.trigMsgTime;
      end;
    TRIGGER_DAMAGE:
      begin
        tgt.tgcAmount := tdata.trigAmount;
        tgt.tgcInterval := tdata.trigInterval;
      end;
    TRIGGER_HEALTH:
      begin
        tgt.tgcAmount := tdata.trigAmount;
        tgt.tgcInterval := tdata.trigInterval;
        tgt.tgcHealMax := tdata.trigHealMax;
        tgt.tgcSilent := tdata.trigSilent;
      end;
    TRIGGER_SHOT:
      begin
        tgt.tgcTX := tdata.trigTX;
        tgt.tgcTY := tdata.trigTY;
        tgt.tgcShotType := tdata.trigShotType;
        tgt.tgcShotTarget := tdata.trigShotTarget;
        tgt.tgcQuiet := tdata.trigQuiet;
        tgt.tgcAim := tdata.trigAim;
        tgt.tgcSight := tdata.trigSight;
        tgt.tgcAngle := tdata.trigAngle;
        tgt.tgcWait := tdata.trigWait;
        tgt.tgcAccuracy := tdata.trigAccuracy;
        tgt.tgcAmmo := tdata.trigAmmo;
        tgt.tgcReload := tdata.trigReload;
      end;
    TRIGGER_EFFECT:
      begin
        tgt.tgcFXCount := tdata.trigFXCount;
        tgt.tgcFXType := tdata.trigFXType;
        tgt.tgcFXSubType := tdata.trigFXSubType;
        tgt.tgcFXRed := tdata.trigFXRed;
        tgt.tgcFXGreen := tdata.trigFXGreen;
        tgt.tgcFXBlue := tdata.trigFXBlue;
        tgt.tgcFXPos := tdata.trigFXPos;
        tgt.tgcWait := tdata.trigWait;
        tgt.tgcVelX := tdata.trigVelX;
        tgt.tgcVelY := tdata.trigVelY;
        tgt.tgcSpreadL := tdata.trigSpreadL;
        tgt.tgcSpreadR := tdata.trigSpreadR;
        tgt.tgcSpreadU := tdata.trigSpreadU;
        tgt.tgcSpreadD := tdata.trigSpreadD;
      end;
  end;
end;
