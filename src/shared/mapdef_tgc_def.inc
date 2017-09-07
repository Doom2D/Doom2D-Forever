// trigger cache

//TRIGGER_EXIT
tgcMap: AnsiString;

//TRIGGER_TELEPORT
tgcTarget: TDFPoint;
tgcD2d: Boolean;
tgcSilent: Boolean;
tgcDirection: Byte;

//TRIGGER_OPENDOOR
//TRIGGER_CLOSEDOOR
//TRIGGER_DOOR
//TRIGGER_DOOR5
//TRIGGER_CLOSETRAP
//TRIGGER_TRAP
//TRIGGER_LIFTUP
//TRIGGER_LIFTDOWN
//TRIGGER_LIFT

//TRIGGER_PRESS
//TRIGGER_ON
//TRIGGER_OFF
//TRIGGER_ONOFF
tgcTX: LongInt;
tgcTY: LongInt;
tgcTWidth: LongInt;
tgcTHeight: LongInt;
tgcWait: Word;
tgcPressCount: Word;
tgcExtRandom: Boolean;
tgcSound: AnsiString;

//TRIGGER_SECRET

//TRIGGER_TEXTURE
tgcActivateOnce: Boolean;
tgcAnimateOnce: Boolean;

//TRIGGER_SOUND
tgcSoundName: AnsiString;
tgcVolume: Byte;
tgcPan: Byte;
tgcLocal: Boolean;
tgcPlayCount: Byte;
tgcSoundSwitch: Boolean;

//TRIGGER_SPAWNMONSTER
tgcSpawnMonsType: Byte;
tgcHealth: LongInt;
tgcActive: Boolean;
tgcMonsCount: LongInt;
tgcEffect: Byte;
tgcMax: Word;
tgcDelay: Word;
tgcBehaviour: Byte;

//TRIGGER_SPAWNITEM
tgcSpawnItemType: Byte;
tgcGravity: Boolean;
tgcDmonly: Boolean;
tgcItemCount: LongInt;

//TRIGGER_MUSIC
tgcMusicName: AnsiString;
tgcMusicAction: Byte;

//TRIGGER_PUSH
tgcAngle: Word;
tgcForce: Byte;
tgcResetVelocity: Boolean;

//TRIGGER_SCORE
tgcScoreAction: Byte;
tgcScoreCount: Byte;
tgcScoreTeam: Byte;
tgcScoreCon: Boolean;
tgcScoreMsg: Boolean;

//TRIGGER_MESSAGE
tgcKind: Byte;
tgcMsgDest: Byte;
tgcText: AnsiString;
tgcMsgTime: Word;

//TRIGGER_DAMAGE
tgcAmount: Word;
tgcInterval: Word;

//TRIGGER_HEALTH
tgcHealMax: Boolean;

//TRIGGER_SHOT
tgcShotType: Byte;
tgcShotTarget: Byte;
tgcQuiet: Boolean;
tgcAim: SmallInt;
tgcSight: Word;
tgcAccuracy: Word;
tgcAmmo: Word;
tgcReload: Word;

//TRIGGER_EFFECT
tgcFXCount: Byte;
tgcFXType: Byte;
tgcFXSubType: Byte;
tgcFXRed: Byte;
tgcFXGreen: Byte;
tgcFXBlue: Byte;
tgcFXPos: Byte;
tgcVelX: SmallInt;
tgcVelY: SmallInt;
tgcSpreadL: Byte;
tgcSpreadR: Byte;
tgcSpreadU: Byte;
tgcSpreadD: Byte;
