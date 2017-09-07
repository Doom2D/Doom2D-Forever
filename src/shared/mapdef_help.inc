// *** WARNING! ***
//   regenerate this part directly from "mapdef.txt" with 'mapgen', NEVER manually change anything here!


// TRIGGER_EXIT
function trigMap (): AnsiString; inline;

// TRIGGER_TELEPORT
function trigTarget (): TDFPoint; inline;
function trigD2d (): Boolean; inline;
function trigSilent (): Boolean; inline;
function trigDirection (): Byte; inline;

// TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT

// TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF
function trigTX (): LongInt; inline;
function trigTY (): LongInt; inline;
function trigTWidth (): Word; inline;
function trigTHeight (): Word; inline;
function trigWait (): Word; inline;
function trigPressCount (): Word; inline;
function trigExtRandom (): Boolean; inline;
function trigSound (): AnsiString; inline;

// TRIGGER_SECRET

// TRIGGER_TEXTURE
function trigActivateOnce (): Boolean; inline;
function trigAnimateOnce (): Boolean; inline;

// TRIGGER_SOUND
function trigSoundName (): AnsiString; inline;
function trigVolume (): Byte; inline;
function trigPan (): Byte; inline;
function trigLocal (): Boolean; inline;
function trigPlayCount (): Byte; inline;
function trigSoundSwitch (): Boolean; inline;

// TRIGGER_SPAWNMONSTER
function trigSpawnMonsType (): Byte; inline;
function trigHealth (): LongInt; inline;
function trigActive (): Boolean; inline;
function trigMonsCount (): LongInt; inline;
function trigEffect (): Byte; inline;
function trigMax (): Word; inline;
function trigDelay (): Word; inline;
function trigBehaviour (): Byte; inline;

// TRIGGER_SPAWNITEM
function trigSpawnItemType (): Byte; inline;
function trigGravity (): Boolean; inline;
function trigDmonly (): Boolean; inline;
function trigItemCount (): LongInt; inline;

// TRIGGER_MUSIC
function trigMusicName (): AnsiString; inline;
function trigMusicAction (): Byte; inline;

// TRIGGER_PUSH
function trigAngle (): Word; inline;
function trigForce (): Byte; inline;
function trigResetVelocity (): Boolean; inline;

// TRIGGER_SCORE
function trigScoreAction (): Byte; inline;
function trigScoreCount (): Byte; inline;
function trigScoreTeam (): Byte; inline;
function trigScoreCon (): Boolean; inline;
function trigScoreMsg (): Boolean; inline;

// TRIGGER_MESSAGE
function trigKind (): Byte; inline;
function trigMsgDest (): Byte; inline;
function trigText (): AnsiString; inline;
function trigMsgTime (): Word; inline;

// TRIGGER_DAMAGE
function trigAmount (): Word; inline;
function trigInterval (): Word; inline;

// TRIGGER_HEALTH
function trigHealMax (): Boolean; inline;

// TRIGGER_SHOT
function trigShotType (): Byte; inline;
function trigShotTarget (): Byte; inline;
function trigQuiet (): Boolean; inline;
function trigAim (): SmallInt; inline;
function trigSight (): Word; inline;
function trigAccuracy (): Word; inline;
function trigAmmo (): Word; inline;
function trigReload (): Word; inline;

// TRIGGER_EFFECT
function trigFXCount (): Byte; inline;
function trigFXType (): Byte; inline;
function trigFXSubType (): Byte; inline;
function trigFXRed (): Byte; inline;
function trigFXGreen (): Byte; inline;
function trigFXBlue (): Byte; inline;
function trigFXPos (): Byte; inline;
function trigVelX (): SmallInt; inline;
function trigVelY (): SmallInt; inline;
function trigSpreadL (): Byte; inline;
function trigSpreadR (): Byte; inline;
function trigSpreadU (): Byte; inline;
function trigSpreadD (): Byte; inline;
