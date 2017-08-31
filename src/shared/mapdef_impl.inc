

// ////////////////////////////////////////////////////////////////////////// //
// trigger helpers

// TRIGGER_EXIT
function TDynRecordHelper.trigMapName (): AnsiString; inline; begin result := utf2win(getFieldWithType('map', TDynField.TType.TChar).sval); end;

// TRIGGER_TELEPORT
function TDynRecordHelper.trigTargetPoint (): TDFPoint; inline; begin result := getPointField('target'); end;
function TDynRecordHelper.trigd2d_teleport (): Boolean; inline; begin result := (getFieldWithType('d2d', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigsilent_teleport (): Boolean; inline; begin result := (getFieldWithType('silent', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigTlpDir (): Byte; inline; begin result := Byte(getFieldWithType('direction', TDynField.TType.TUByte).ival); end;

// TRIGGER_OPENDOOR, TRIGGER_CLOSEDOOR, TRIGGER_DOOR, TRIGGER_DOOR5, TRIGGER_CLOSETRAP, TRIGGER_TRAP, TRIGGER_LIFTUP, TRIGGER_LIFTDOWN, TRIGGER_LIFT
function TDynRecordHelper.trigNoSound (): Boolean; inline; begin result := (getFieldWithType('silent', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigd2d_doors (): Boolean; inline; begin result := (getFieldWithType('d2d', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_PRESS, TRIGGER_ON, TRIGGER_OFF, TRIGGER_ONOFF
function TDynRecordHelper.trigTX (): LongInt; inline; begin result := LongInt(getFieldWithType('position', TDynField.TType.TPoint).ival); end;
function TDynRecordHelper.trigTY (): LongInt; inline; begin result := LongInt(getFieldWithType('position', TDynField.TType.TPoint).ival2); end;
function TDynRecordHelper.trigTWidth (): Word; inline; begin result := Word(getFieldWithType('size', TDynField.TType.TSize).ival); end;
function TDynRecordHelper.trigTHeight (): Word; inline; begin result := Word(getFieldWithType('size', TDynField.TType.TSize).ival2); end;
function TDynRecordHelper.trigWait (): Word; inline; begin result := Word(getFieldWithType('wait', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigCount (): Word; inline; begin result := Word(getFieldWithType('count', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigExtRandom (): Boolean; inline; begin result := (getFieldWithType('extrandom', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SECRET

// TRIGGER_TEXTURE
function TDynRecordHelper.trigActivateOnce (): Boolean; inline; begin result := (getFieldWithType('activateonce', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigAnimOnce (): Boolean; inline; begin result := (getFieldWithType('animateonce', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SOUND
function TDynRecordHelper.trigSoundName (): AnsiString; inline; begin result := utf2win(getFieldWithType('soundname', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.trigVolume (): Byte; inline; begin result := Byte(getFieldWithType('volume', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigPan (): Byte; inline; begin result := Byte(getFieldWithType('pan', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigLocal (): Boolean; inline; begin result := (getFieldWithType('local', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigPlayCount (): Byte; inline; begin result := Byte(getFieldWithType('playcount', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigSoundSwitch (): Boolean; inline; begin result := (getFieldWithType('soundswitch', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SPAWNMONSTER
function TDynRecordHelper.trigMonPos (): TDFPoint; inline; begin result := getPointField('position'); end;
function TDynRecordHelper.trigMonType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMonHealth (): LongInt; inline; begin result := LongInt(getFieldWithType('health', TDynField.TType.TInt).ival); end;
function TDynRecordHelper.trigMonDir (): Byte; inline; begin result := Byte(getFieldWithType('direction', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMonActive (): Boolean; inline; begin result := (getFieldWithType('active', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigMonCount (): LongInt; inline; begin result := LongInt(getFieldWithType('count', TDynField.TType.TInt).ival); end;
function TDynRecordHelper.trigMonEffect (): Byte; inline; begin result := Byte(getFieldWithType('effect', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMonMax (): Word; inline; begin result := Word(getFieldWithType('max', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigMonDelay (): Word; inline; begin result := Word(getFieldWithType('delay', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigMonBehav (): Byte; inline; begin result := Byte(getFieldWithType('behaviour', TDynField.TType.TUByte).ival); end;

// TRIGGER_SPAWNITEM
function TDynRecordHelper.trigItemPos (): TDFPoint; inline; begin result := getPointField('position'); end;
function TDynRecordHelper.trigItemType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigItemFalls (): Boolean; inline; begin result := (getFieldWithType('gravity', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigItemOnlyDM (): Boolean; inline; begin result := (getFieldWithType('dmonly', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigItemCount (): LongInt; inline; begin result := LongInt(getFieldWithType('count', TDynField.TType.TInt).ival); end;
function TDynRecordHelper.trigItemEffect (): Byte; inline; begin result := Byte(getFieldWithType('effect', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigItemMax (): Word; inline; begin result := Word(getFieldWithType('max', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigItemDelay (): Word; inline; begin result := Word(getFieldWithType('delay', TDynField.TType.TUShort).ival); end;

// TRIGGER_MUSIC
function TDynRecordHelper.trigMusicName (): AnsiString; inline; begin result := utf2win(getFieldWithType('name', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.trigMusicAction (): Byte; inline; begin result := Byte(getFieldWithType('action', TDynField.TType.TUByte).ival); end;

// TRIGGER_PUSH
function TDynRecordHelper.trigPushAngle (): Word; inline; begin result := Word(getFieldWithType('angle', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigPushForce (): Byte; inline; begin result := Byte(getFieldWithType('force', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigResetVel (): Boolean; inline; begin result := (getFieldWithType('resetvelocity', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SCORE
function TDynRecordHelper.trigScoreAction (): Byte; inline; begin result := Byte(getFieldWithType('action', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigScoreCount (): Byte; inline; begin result := Byte(getFieldWithType('count', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigScoreTeam (): Byte; inline; begin result := Byte(getFieldWithType('team', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigScoreCon (): Boolean; inline; begin result := (getFieldWithType('console', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigScoreMsg (): Boolean; inline; begin result := (getFieldWithType('message', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_MESSAGE
function TDynRecordHelper.trigMessageKind (): Byte; inline; begin result := Byte(getFieldWithType('kind', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMessageSendTo (): Byte; inline; begin result := Byte(getFieldWithType('sendto', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigMessageText (): AnsiString; inline; begin result := utf2win(getFieldWithType('text', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.trigMessageTime (): Word; inline; begin result := Word(getFieldWithType('time', TDynField.TType.TUShort).ival); end;

// TRIGGER_DAMAGE
function TDynRecordHelper.trigDamageValue (): Word; inline; begin result := Word(getFieldWithType('amount', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigDamageInterval (): Word; inline; begin result := Word(getFieldWithType('interval', TDynField.TType.TUShort).ival); end;

// TRIGGER_HEALTH
function TDynRecordHelper.trigHealValue (): Word; inline; begin result := Word(getFieldWithType('amount', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigHealInterval (): Word; inline; begin result := Word(getFieldWithType('interval', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigHealMax (): Boolean; inline; begin result := (getFieldWithType('max', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.trigHealSilent (): Boolean; inline; begin result := (getFieldWithType('silent', TDynField.TType.TBool).ival <> 0); end;

// TRIGGER_SHOT
function TDynRecordHelper.trigShotPos (): TDFPoint; inline; begin result := getPointField('position'); end;
function TDynRecordHelper.trigShotType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigShotTarget (): Byte; inline; begin result := Byte(getFieldWithType('target', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigShotSound (): Boolean; inline; begin result := (getFieldWithType('silent', TDynField.TType.TBool).ival = 0); end;
function TDynRecordHelper.trigShotAim (): SmallInt; inline; begin result := ShortInt(getFieldWithType('aim', TDynField.TType.TByte).ival); end;
function TDynRecordHelper.trigShotIntSight (): Word; inline; begin result := Word(getFieldWithType('sight', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigShotAngle (): Word; inline; begin result := Word(getFieldWithType('angle', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigShotWait (): Word; inline; begin result := Word(getFieldWithType('wait', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigShotAccuracy (): Word; inline; begin result := Word(getFieldWithType('accuracy', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigShotAmmo (): Word; inline; begin result := Word(getFieldWithType('ammo', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigShotIntReload (): Word; inline; begin result := Word(getFieldWithType('reload', TDynField.TType.TUShort).ival); end;

// TRIGGER_EFFECT
function TDynRecordHelper.trigFXCount (): Byte; inline; begin result := Byte(getFieldWithType('count', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXSubType (): Byte; inline; begin result := Byte(getFieldWithType('subtype', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXColorR (): Byte; inline; begin result := Byte(getFieldWithType('colorr', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXColorG (): Byte; inline; begin result := Byte(getFieldWithType('colorg', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXColorB (): Byte; inline; begin result := Byte(getFieldWithType('colorb', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXPos (): Byte; inline; begin result := Byte(getFieldWithType('position', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXWait (): Word; inline; begin result := Word(getFieldWithType('wait', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.trigFXVelX (): SmallInt; inline; begin result := ShortInt(getFieldWithType('velx', TDynField.TType.TByte).ival); end;
function TDynRecordHelper.trigFXVelY (): SmallInt; inline; begin result := ShortInt(getFieldWithType('vely', TDynField.TType.TByte).ival); end;
function TDynRecordHelper.trigFXSpreadL (): Byte; inline; begin result := Byte(getFieldWithType('spreadl', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXSpreadR (): Byte; inline; begin result := Byte(getFieldWithType('spreadr', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXSpreadU (): Byte; inline; begin result := Byte(getFieldWithType('spreadu', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.trigFXSpreadD (): Byte; inline; begin result := Byte(getFieldWithType('spreadd', TDynField.TType.TUByte).ival); end;
