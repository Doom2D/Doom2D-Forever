{ !!! auto-generated from "mapdef.txt" with 'mapgen' }


// ////////////////////////////////////////////////////////////////////////// //
// trigger helpers

// TRIGGER_EXIT
function TDynRecordHelper.trigMap (): AnsiString; inline; begin result := utf2win(getFieldWithType('map', TDynField.TType.TChar).sval); end;

// TRIGGER_TELEPORT
function TDynRecordHelper.trigTarget (): TDFPoint; inline; begin result := getPointField('target'); end;
function TDynRecordHelper.trigD2d (): Boolean; inline; begin result := (getFieldWithType('d2d', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigSilent (): Boolean; inline; begin result := (getFieldWithType('silent', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigDirection (): Byte; inline; begin result := Byte(getFieldWithType('direction', TDynField.TType.TUByte).ival); end;

// TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT

// TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF
function TDynRecordHelper.trigTX (): LongInt; inline; begin result := LongInt(getFieldWithType('position', TDynField.TType.TPoint).ival); end;
function TDynRecordHelper.trigTY (): LongInt; inline; begin result := LongInt(getFieldWithType('position', TDynField.TType.TPoint).ival2); end;
function TDynRecordHelper.trigTWidth (): Word; inline; begin result := Word(getFieldWithType('size', TDynField.TType.TSize).ival); end;
function TDynRecordHelper.trigTHeight (): Word; inline; begin result := Word(getFieldWithType('size', TDynField.TType.TSize).ival2); end;
function TDynRecordHelper.trigWait (): Word; inline; begin result := Word(getFieldWithType('wait', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigPressCount (): Word; inline; begin result := Word(getFieldWithType('count', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigExtRandom (): Boolean; inline; begin result := (getFieldWithType('ext_random', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigSound (): AnsiString; inline; begin result := utf2win(getFieldWithType('sound', TDynField.TType.TString).sval); end;

// TRIGGER_SECRET

// TRIGGER_TEXTURE
function TDynRecordHelper.trigActivateOnce (): Boolean; inline; begin result := (getFieldWithType('activate_once', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigAnimateOnce (): Boolean; inline; begin result := (getFieldWithType('animate_once', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SOUND
function TDynRecordHelper.trigSoundName (): AnsiString; inline; begin result := utf2win(getFieldWithType('sound_name', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.trigVolume (): Byte; inline; begin result := Byte(getFieldWithType('volume', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigPan (): Byte; inline; begin result := Byte(getFieldWithType('pan', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigLocal (): Boolean; inline; begin result := (getFieldWithType('local', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigPlayCount (): Byte; inline; begin result := Byte(getFieldWithType('play_count', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigSoundSwitch (): Boolean; inline; begin result := (getFieldWithType('sound_switch', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SPAWNMONSTER
function TDynRecordHelper.trigSpawnMonsType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigHealth (): LongInt; inline; begin result := LongInt(getFieldWithType('health', TDynField.TType.TInt).ival); end;
function TDynRecordHelper.trigActive (): Boolean; inline; begin result := (getFieldWithType('active', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigMonsCount (): LongInt; inline; begin result := LongInt(getFieldWithType('count', TDynField.TType.TInt).ival); end;
function TDynRecordHelper.trigEffect (): Byte; inline; begin result := Byte(getFieldWithType('effect', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMax (): Word; inline; begin result := Word(getFieldWithType('max', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigDelay (): Word; inline; begin result := Word(getFieldWithType('delay', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigBehaviour (): Byte; inline; begin result := Byte(getFieldWithType('behaviour', TDynField.TType.TUByte).ival); end;

// TRIGGER_SPAWNITEM
function TDynRecordHelper.trigSpawnItemType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigGravity (): Boolean; inline; begin result := (getFieldWithType('gravity', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigDmonly (): Boolean; inline; begin result := (getFieldWithType('dmonly', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigItemCount (): LongInt; inline; begin result := LongInt(getFieldWithType('count', TDynField.TType.TInt).ival); end;

// TRIGGER_MUSIC
function TDynRecordHelper.trigMusicName (): AnsiString; inline; begin result := utf2win(getFieldWithType('name', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.trigMusicAction (): Byte; inline; begin result := Byte(getFieldWithType('action', TDynField.TType.TUByte).ival); end;

// TRIGGER_PUSH
function TDynRecordHelper.trigAngle (): Word; inline; begin result := Word(getFieldWithType('angle', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigForce (): Byte; inline; begin result := Byte(getFieldWithType('force', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigResetVelocity (): Boolean; inline; begin result := (getFieldWithType('reset_velocity', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SCORE
function TDynRecordHelper.trigScoreAction (): Byte; inline; begin result := Byte(getFieldWithType('action', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigScoreCount (): Byte; inline; begin result := Byte(getFieldWithType('count', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigScoreTeam (): Byte; inline; begin result := Byte(getFieldWithType('team', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigScoreCon (): Boolean; inline; begin result := (getFieldWithType('console', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigScoreMsg (): Boolean; inline; begin result := (getFieldWithType('message', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_MESSAGE
function TDynRecordHelper.trigKind (): Byte; inline; begin result := Byte(getFieldWithType('kind', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMsgDest (): Byte; inline; begin result := Byte(getFieldWithType('dest', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigText (): AnsiString; inline; begin result := utf2win(getFieldWithType('text', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.trigMsgTime (): Word; inline; begin result := Word(getFieldWithType('time', TDynField.TType.TUShort).ival); end;

// TRIGGER_DAMAGE
function TDynRecordHelper.trigAmount (): Word; inline; begin result := Word(getFieldWithType('amount', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigInterval (): Word; inline; begin result := Word(getFieldWithType('interval', TDynField.TType.TUShort).ival); end;

// TRIGGER_HEALTH
function TDynRecordHelper.trigHealMax (): Boolean; inline; begin result := (getFieldWithType('max', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SHOT
function TDynRecordHelper.trigShotType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigShotTarget (): Byte; inline; begin result := Byte(getFieldWithType('target', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigShotSound (): Boolean; inline; begin result := (getFieldWithType('sound', TDynField.TType.TBool).ival = 0); end;
function TDynRecordHelper.trigAim (): SmallInt; inline; begin result := ShortInt(getFieldWithType('aim', TDynField.TType.TByte).ival); end;
function TDynRecordHelper.trigSight (): Word; inline; begin result := Word(getFieldWithType('sight', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigAccuracy (): Word; inline; begin result := Word(getFieldWithType('accuracy', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigAmmo (): Word; inline; begin result := Word(getFieldWithType('ammo', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigReload (): Word; inline; begin result := Word(getFieldWithType('reload', TDynField.TType.TUShort).ival); end;

// TRIGGER_EFFECT
function TDynRecordHelper.trigFXCount (): Byte; inline; begin result := Byte(getFieldWithType('count', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXSubType (): Byte; inline; begin result := Byte(getFieldWithType('subtype', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXRed (): Byte; inline; begin result := Byte(getFieldWithType('red', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXGreen (): Byte; inline; begin result := Byte(getFieldWithType('green', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXBlue (): Byte; inline; begin result := Byte(getFieldWithType('blue', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXPos (): Byte; inline; begin result := Byte(getFieldWithType('pos', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigVelX (): SmallInt; inline; begin result := ShortInt(getFieldWithType('vel_x', TDynField.TType.TByte).ival); end;
function TDynRecordHelper.trigVelY (): SmallInt; inline; begin result := ShortInt(getFieldWithType('vel_y', TDynField.TType.TByte).ival); end;
function TDynRecordHelper.trigSpreadL (): Byte; inline; begin result := Byte(getFieldWithType('spread_l', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigSpreadR (): Byte; inline; begin result := Byte(getFieldWithType('spread_r', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigSpreadU (): Byte; inline; begin result := Byte(getFieldWithType('spread_u', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigSpreadD (): Byte; inline; begin result := Byte(getFieldWithType('spread_d', TDynField.TType.TUByte).ival); end;
