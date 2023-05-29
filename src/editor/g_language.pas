Unit g_language;

{$INCLUDE ../shared/a_modes.inc}

Interface

  uses g_Basic, MAPDEF, Classes;

  resourcestring
    MsgNotAccessible = 'N/A';

    MsgArrayBoolFalse = 'No';
    MsgArrayBoolTrue = 'Yes';

    MsgArrayDirLeft = 'Left';
    MsgArrayDirRight = 'Right';
    MsgArrayDirSame = 'Don''t change';
    MsgArrayDirReversed = 'Reversed';

    MsgArrayDirbtnLeft = 'Left';
    MsgArrayDirbtnRight = 'Right';
    MsgArrayDirbtnUp = 'Up';
    MsgArrayDirbtnDown = 'Down';

    MsgArrayPanelWall = 'Wall';
    MsgArrayPanelBack = 'Background';
    MsgArrayPanelFront = 'Foreground';
    MsgArrayPanelDoorOpen = 'Open Door';
    MsgArrayPanelDoorClose = 'Closed Door';
    MsgArrayPanelStair = 'Step';
    MsgArrayPanelWater = 'Water';
    MsgArrayPanelAcid1 = 'Acid 1';
    MsgArrayPanelAcid2 = 'Acid 2';
    MsgArrayPanelLiftUp = 'Stream Up';
    MsgArrayPanelLiftDown = 'Stream Down';
    MsgArrayPanelLiftLeft = 'Stream Left';
    MsgArrayPanelLiftRight = 'Stream Right';
    MsgArrayPanelBlockmon = 'Monster Boundary';

    MsgArrayFxNone = 'None';
    MsgArrayFxTeleport = 'Teleport';
    MsgArrayFxRespawn = 'Respawn';
    MsgArrayFxFire = 'Arch-Vile Fire';

    MsgArrayItemMedkit = 'Stimpack';
    MsgArrayItemLargeMedkit = 'Medikit';
    MsgArrayItemBlackMedkit = 'Berserk Pack';
    MsgArrayItemGreenArmor = 'Green Armor';
    MsgArrayItemBlueArmor = 'Blue Armor';
    MsgArrayItemBlueSphere = 'Soulsphere';
    MsgArrayItemMegasphere = 'Megasphere';
    MsgArrayItemHazSuit = 'Envirosuit';
    MsgArrayItemOxygen = 'Scuba';
    MsgArrayItemInvulnerability = 'Invulnerability';
    MsgArrayItemChainsaw = 'Chainsaw';
    MsgArrayItemShotgun = 'Shotgun';
    MsgArrayItemDbShotgun = 'Super Shotgun';
    MsgArrayItemChaingun = 'Chaingun';
    MsgArrayItemRocketLauncher = 'Rocket Launcher';
    MsgArrayItemPlasmaRifle = 'Plasma Rifle';
    MsgArrayItemBfg = 'BFG9000';
    MsgArrayItemSuperMinigun = 'Super Chaingun';
    MsgArrayItemFlamethrower = 'Flamethrower';
    MsgArrayItemClip = 'Clip';
    MsgArrayItemAmmoBox = 'Box of Bullets';
    MsgArrayItem4Shells = '4 Shells';
    MsgArrayItem25Shells = 'Box of Shells';
    MsgArrayItem1Rocket = 'Rocket';
    MsgArrayItemRocketBox = 'Box of Rockets';
    MsgArrayItemCell = 'Energy Cell';
    MsgArrayItemLargeCell = 'Energy Cell Pack';
    MsgArrayItemFuelcan = 'Fuel Canister';
    MsgArrayItemBackpack = 'Backpack';
    MsgArrayItemKeyRed = 'Red Key';
    MsgArrayItemKeyGreen = 'Green Key';
    MsgArrayItemKeyBlue = 'Blue Key';
    MsgArrayItemBottle = 'Health Globe';
    MsgArrayItemHelmet = 'Armor Shard';
    MsgArrayItemJetpack = 'Jetpack';
    MsgArrayItemInvis = 'Invisibility';

    MsgArrayShotPistol = 'Pistol shot';
    MsgArrayShotBullet = 'Chaingun shot';
    MsgArrayShotShotgun = 'Shotgun shot';
    MsgArrayShotSsg = 'Super Shotgun shot';
    MsgArrayShotImp = 'Imp fireball';
    MsgArrayShotPlasma = 'Blue plasma';
    MsgArrayShotSpider = 'Arachnotron plasma';
    MsgArrayShotCaco = 'Cacodemon fireball';
    MsgArrayShotBaron = 'Hell Baron projectile';
    MsgArrayShotMancub = 'Mancubus gunshot';
    MsgArrayShotRev = 'Revenant projectile';
    MsgArrayShotRocket = 'Rocket';
    MsgArrayShotBfg = 'BFG ball';
    MsgArrayShotExpl = 'Explosion';
    MsgArrayShotBfgexpl = 'BFG explosion';
    MsgArrayShotFlame = 'Flame';

    MsgArrayMonDemon = 'Pinky';
    MsgArrayMonImp = 'Imp';
    MsgArrayMonZombie = 'Zombie';
    MsgArrayMonSergeant = 'Sergeant';
    MsgArrayMonCyber = 'Cyberdemon';
    MsgArrayMonCgun = 'Commando';
    MsgArrayMonHellBaron = 'Hell Baron';
    MsgArrayMonHellKnight = 'Hell Knight';
    MsgArrayMonCacodemon = 'Cacodemon';
    MsgArrayMonLostSoul = 'Lost Soul';
    MsgArrayMonPainElemental = 'Pain Elemental';
    MsgArrayMonMastermind = 'Spider Mastermind';
    MsgArrayMonArachnatron = 'Arachnotron';
    MsgArrayMonMancubus = 'Mancubus';
    MsgArrayMonRevenant = 'Revenant';
    MsgArrayMonArchvile = 'Arch-Vile';
    MsgArrayMonFish = 'Piranha';
    MsgArrayMonBarrel = 'Barrel';
    MsgArrayMonRobot = 'Robot';
    MsgArrayMonPrikolist = 'Prikolist';

    MsgArrayAreaPlayerOne = 'Player 1';
    MsgArrayAreaPlayerTwo = 'Player 2';
    MsgArrayAreaDm = 'DM Spawn Point';
    MsgArrayAreaFlagRed = 'Red Flag';
    MsgArrayAreaFlagBlue = 'Blue Flag';
    MsgArrayAreaFlagDom = 'Domination Flag';
    MsgArrayAreaTeamRed = 'Red Team';
    MsgArrayAreaTeamBlue = 'Blue Team';

    MsgArrayTrExit = 'Exit';
    MsgArrayTrTeleport = 'Teleport';
    MsgArrayTrDoorOpen = 'Open Door';
    MsgArrayTrDoorClose = 'Close Door';
    MsgArrayTrDoorSwitch = 'Door';
    MsgArrayTrDoor5Sec = 'Door (5 sec)';
    MsgArrayTrTrapClose = 'Close Trap';
    MsgArrayTrTrap = 'Trap';
    MsgArrayTrExtend = 'Extender';
    MsgArrayTrSecret = 'Secret';
    MsgArrayTrLiftUp = 'Turn stream up/left';
    MsgArrayTrLiftDown = 'Turn stream down/right';
    MsgArrayTrLiftSwitch = 'Revert stream';
    MsgArrayTrTexture = 'Change Texture';
    MsgArrayTrOn = 'Enable Trigger';
    MsgArrayTrOff = 'Disable Trigger';
    MsgArrayTrSwitch = 'Trigger Toggle';
    MsgArrayTrSound = 'Play Sound';
    MsgArrayTrSpawnMonster = 'Spawn Monster';
    MsgArrayTrSpawnItem = 'Spawn Item';
    MsgArrayTrMusic = 'Play Music';
    MsgArrayTrPush = 'Push';
    MsgArrayTrScore = 'Team Score';
    MsgArrayTrMessage = 'Message';
    MsgArrayTrDamage = 'Damage';
    MsgArrayTrHealth = 'Healer';
    MsgArrayTrShot = 'Turret';
    MsgArrayTrEffect = 'Effect';

    MsgPropId = 'ID';
    MsgPropX = 'X';
    MsgPropY = 'Y';
    MsgPropWidth = 'Width';
    MsgPropHeight = 'Height';
    MsgPropPanelType = 'Panel Type';
    MsgPropPanelTex = 'Texture';
    MsgPropPanelAlpha = 'Transparency';
    MsgPropPanelBlend = 'Blending';
    MsgPropDmOnly = 'DM Only';
    MsgPropItemFalls = 'Falls';
    MsgPropDirection = 'Direction';

    MsgPropTrType = 'Trigger Type';
    MsgPropTrEnabled = 'Enabled';
    MsgPropTrTexturePanel = 'Textured Panel';
    MsgPropTrActivation = 'Activation Type';
    MsgPropTrKeys = 'Keys';
    MsgPropTrD2d = 'D2D-like';
    MsgPropTrSilent = 'Silent';
    MsgPropTrCount = 'Count';
    MsgPropTrInterval = 'Interval (in ticks)';
    MsgPropTrHealth = 'Health';
    MsgPropTrNextMap = 'Next Map';
    MsgPropTrTeleportTo = 'Teleport to';
    MsgPropTrTeleportSilent = 'Silent';
    MsgPropTrTeleportDir = 'New Direction';
    MsgPropTrDoorPanel = 'Door Panel';
    MsgPropTrTrapPanel = 'Trap Panel';
    MsgPropTrExArea = 'Action Area';
    MsgPropTrExDelay = 'Delay';
    MsgPropTrExCount = 'Count';
    MsgPropTrExMonster = 'Monster ID';
    MsgPropTrExRandom = 'Random';
    MsgPropTrLiftPanel = 'Stream Panel';
    MsgPropTrTextureOnce = 'Once';
    MsgPropTrTextureAnimOnce = 'Animate Once';
    MsgPropTrSoundName = 'Sound                              (snd)';
    MsgPropTrSoundVolume = 'Volume';
    MsgPropTrSoundPan = 'Pan';
    MsgPropTrSoundCount = 'Loops';
    MsgPropTrSoundLocal = 'Local';
    MsgPropTrSoundSwitch = 'Switch';
    MsgPropTrFxType = 'Effect';
    MsgPropTrSpawnTo = 'Spawn at';
    MsgPropTrSpawnMax = 'Maximum';
    MsgPropTrSpawnDelay = 'Autospawn (in ticks)';
    MsgPropTrMonsterType = 'Monster';
    MsgPropTrMonsterActive = 'Active';
    MsgPropTrMonsterBehaviour = 'Behaviour';
    MsgPropTrMonsterBehaviour0 = 'Normal';
    MsgPropTrMonsterBehaviour1 = 'Killer';
    MsgPropTrMonsterBehaviour2 = 'Maniac';
    MsgPropTrMonsterBehaviour3 = 'Insane';
    MsgPropTrMonsterBehaviour4 = 'Cannibal';
    MsgPropTrMonsterBehaviour5 = 'Good';
    MsgPropTrItemType = 'Item';
    MsgPropTrMusicName = 'Music';
    MsgPropTrMusicAct = 'Action                             (m)';
    MsgPropTrMusicOn = 'Play';
    MsgPropTrMusicOff = 'Pause';
    MsgPropTrPushAngle = 'Angle';
    MsgPropTrPushForce = 'Force';
    MsgPropTrPushReset = 'Reset velocity';
    MsgPropTrScoreAct = 'Action                             (s)';
    MsgPropTrScoreAct0 = 'Add points';
    MsgPropTrScoreAct1 = 'Subtract points';
    MsgPropTrScoreAct2 = 'Team Wins';
    MsgPropTrScoreAct3 = 'Team Loses';
    MsgPropTrScoreTeam = 'Team';
    MsgPropTrScoreTeam0 = 'My';
    MsgPropTrScoreTeam1 = 'Enemy';
    MsgPropTrScoreTeam2 = 'Red';
    MsgPropTrScoreTeam3 = 'Blue';
    MsgPropTrScoreCon = 'Console message';
    MsgPropTrScoreMsg = 'Game message';
    MsgPropTrDamageValue = 'Damage';
    MsgPropTrDamageKind = 'Damage type';
    MsgPropTrDamageKind0 = 'HIT_SOME';
    MsgPropTrDamageKind3 = 'HIT_TRAP';
    MsgPropTrDamageKind4 = 'HIT_FALL';
    MsgPropTrDamageKind5 = 'HIT_WATER';
    MsgPropTrDamageKind6 = 'HIT_ACID';
    MsgPropTrDamageKind7 = 'HIT_ELECTRO';
    MsgPropTrDamageKind8 = 'HIT_FLAME';
    MsgPropTrHealthMax = 'To maximum';
    MsgPropTrShotType = 'Projectile';
    MsgPropTrShotSound = 'Shot sound';
    MsgPropTrShotAngle = 'Angle';
    MsgPropTrShotAcc = 'Spread';
    MsgPropTrShotTo = 'Auto targeting';
    MsgPropTrShotTo0 = 'None';
    MsgPropTrShotTo1 = 'Monsters';
    MsgPropTrShotTo2 = 'Players';
    MsgPropTrShotTo3 = 'Red team';
    MsgPropTrShotTo4 = 'Blue team';
    MsgPropTrShotTo5 = 'Monsters, players';
    MsgPropTrShotTo6 = 'Players, monsters';
    MsgPropTrShotAim = 'Auto-aiming mode';
    MsgPropTrShotAim0 = 'Trigger area';
    MsgPropTrShotAim1 = 'Entire map';
    MsgPropTrShotAim2 = 'Trace trigger area';
    MsgPropTrShotAim3 = 'Trace entire map';
    MsgPropTrShotAmmo = 'Ammo limit';
    MsgPropTrShotReload = 'Reload interval (in ticks)';
    MsgPropTrShotSight = 'Sight interval (in ticks)';
    MsgPropTrShotPanel = 'Indicator panel';
    MsgPropTrMessageKind = 'Message kind';
    MsgPropTrMessageKind0 = 'Console message';
    MsgPropTrMessageKind1 = 'Event message';
    MsgPropTrMessageTo = 'Send to';
    MsgPropTrMessageTo0 = 'Me';
    MsgPropTrMessageTo1 = 'My team';
    MsgPropTrMessageTo2 = 'Enemy team';
    MsgPropTrMessageTo3 = 'Red team';
    MsgPropTrMessageTo4 = 'Blue team';
    MsgPropTrMessageTo5 = 'Everyone';
    MsgPropTrMessageText = 'Message text';
    MsgPropTrMessageTime = 'Time (in ticks)';
    MsgPropTrEffectType = 'Effect type';
    MsgPropTrEffectSubtype = 'Effect subtype';
    MsgPropTrEffectColor = 'Effect color';
    MsgPropTrEffectCenter = 'Trigger center';
    MsgPropTrEffectVelx = 'Horizontal speed';
    MsgPropTrEffectVely = 'Vertical speed';
    MsgPropTrEffectSpl = 'Speed variance left';
    MsgPropTrEffectSpr = 'Speed variance right';
    MsgPropTrEffectSpu = 'Speed variance up';
    MsgPropTrEffectSpd = 'Speed variance down';
    MsgPropTrEffectParticle = 'Particle generator';
    MsgPropTrEffectAnimation = 'Animation';
    MsgPropTrEffectSliquid = 'Water splash';
    MsgPropTrEffectLliquid = 'Light colored splash';
    MsgPropTrEffectDliquid = 'Dark colored splash';
    MsgPropTrEffectBlood = 'Blood';
    MsgPropTrEffectSpark = 'Sparks';
    MsgPropTrEffectBubble = 'Bubbles';

    MsgMsgError = 'Error';
    MsgMsgWrongTexwidth = 'Panel Width must be a multiple of Texture Width (%d)';
    MsgMsgWrongTexheight = 'Panel Height must be a multiple of Texture Height (%d)';
    MsgMsgWrongAlpha = 'Transparency must be in [0..255] interval';
    MsgMsgWrongSize = 'Width and Height must be greater than 0';
    MsgMsgWrongXy = 'X or Y coordinate hasn''t been set';
    MsgMsgTextureAlready = 'Texture "%s" already exists';
    MsgMsgResName64 = 'Resource "%s" name must not be longer than 64 chars';
    MsgMsgChooseItem = 'Select Item';
    MsgMsgChooseMonster = 'Select Monster';
    MsgMsgChooseArea = 'Select Area';
    MsgMsgChooseTexture = 'Select Texture';
    MsgMsgChooseRes = 'Resource hasn''t been selected';
    MsgMsgExit = 'Exit';
    MsgMsgExitPromt = 'Leaving so soon?';
    MsgMsgDelTexture = 'Delete the texture';
    MsgMsgDelTexturePromt = 'Delete the texture "%s" ?';
    MsgMsgDelTextureCant = 'Can''t delete texture in use. Replace it on all panels with this texture.';
    MsgMsgDelRecent = 'File does not longer exist';
    MsgMsgDelRecentPromt = 'Remove entry from recent list?';
    MsgMsgClearMap = 'New map';
    MsgMsgClearMapPromt = 'Clear the entire map?';
    MsgMsgDeleteMap = 'Delete the map';
    MsgMsgDeleteMapPromt = 'Delete the map "%s" from "%s" ?';
    MsgMsgMapDeleted = 'Map is deleted';
    MsgMsgMapDeletedPromt = 'Map "%s" is deleted';
    MsgMsgReopenMapPromt = 'Reopen this map?';
    MsgMsgExecError = 'Game start error';
    MsgMsgSoundError = 'Can''t play sound';
    MsgMsgWadError = 'Can''t open WAD: %s';
    MsgMsgResError = 'Can''t read resource: %s:%s\%s';
    MsgMsgPacked = 'Map "%s" with resources saved to "%s"';
    MsgMsgMapExists = 'Map "%s" already exists. Overwrite?';
    MsgMsgSaveMap = 'Save the map';

    MsgHintTeleport = 'Choose destination of Teleport';
    MsgHintSpawn = 'Choose Spawn point';
    MsgHintPanelDoor = 'Choose Door';
    MsgHintPanelTexture = 'Choose textured Panel';
    MsgHintPanelShot = 'Choose textured shot indicator Panel';
    MsgHintPanelLift = 'Choose Stream Panel';
    MsgHintMonster = 'Choose Monster';
    MsgHintExtArea = 'Specify action Area';
    MsgHintWidth = 'Width: %d';
    MsgHintHeight = 'Height: %d';

    MsgMenuAppleAbout = 'About Editor';
    MsgMenuApplePref = 'Preferences...';

    MsgMenuFile = 'File';
    MsgMenuFileNew = 'New';
    MsgMenuFileOpen = 'Open...';
    MsgMenuFileRecentMac = 'Open Recent';
    MsgMenuFileRecentClearMac = 'Clear Recent';
    MsgMenuFileSave = 'Save';
    MsgMenuFileSaveAs = 'Save As...';
    MsgMenuFileOpenWad = 'Select Map...';
    MsgMenuFileReopen = 'Revert to Saved';
    MsgMenuFileSaveMini = 'Save Minimap...';
    MsgMenuFileDelete = 'Remove from WAD...';
    MsgMenuFilePackMap = 'Pack to WAD...';
    MsgMenuFileRecentWin = 'Recent Files';
    MsgMenuFileExitWin = 'Exit';

    MsgMenuEdit = 'Edit';
    MsgMenuEditUndo = 'Undo';
    MsgMenuEditCopy = 'Copy';
    MsgMenuEditCut = 'Cut';
    MsgMenuEditPaste = 'Paste';
    MsgMenuEditSelectAll = 'Select All';
    MsgMenuEditSnapGrid = 'Snap to Grid';
    MsgMenuEditStepGrid = 'Switch Grid Granularity';
    MsgMenuEditToFore = 'Bring to Front';
    MsgMenuEditToBack = 'Send to Back';
    MsgMenuEditMapProps = 'Map Properties...';
    MsgMenuEditPrefWin = 'Preferences...';

    MsgMenuView = 'View';
    MsgMenuViewLayers = 'Layers';
    MsgMenuViewMinimap = 'Show Minimap';
    MsgMenuViewBounds = 'Show Map Bounds';
    MsgMenuViewPreview = 'Preview Mode';

    MsgMenuServ = 'Service';
    MsgMenuServTest = 'Analyse Map...';
    MsgMenuServOpt = 'Optimize Map...';
    MsgMenuServLaunch = 'Run Test';

    MsgMenuWindow = 'Window';
    MsgMenuWindowMinimize = 'Minimize';
    MsgMenuWindowZoom = 'Zoom';

    MsgMenuHelp = 'Help';
    MsgMenuHelpAboutWin = 'About Editor';

    MsgMenuLayerBack = '1. Background';
    MsgMenuLayerWall = '2. Walls';
    MsgMenuLayerFore = '3. Foreground';
    MsgMenuLayerStair = '4. Steps';
    MsgMenuLayerWater = '5. Liquids';
    MsgMenuLayerItem = '6. Items';
    MsgMenuLayerMonster = '7. Monsters';
    MsgMenuLayerArea = '8. Areas';
    MsgMenuLayerTrigger = '9. Triggers';

    MsgMenuTbNew = 'New Map';
    MsgMenuTbOpen = 'Open Map';
    MsgMenuTbSave = 'Save Map';
    MsgMenuTbOpenWad = 'Open another Map from same WAD';
    MsgMenuTbMinimap = 'Show Mini-map';
    MsgMenuTbLayers = 'Show/Hide Objects';
    MsgMenuTbGrid = 'Grid On/Off';
    MsgMenuTbGridStep = 'Switch Grid step';
    MsgMenuTbLaunch = 'Run Test';

    MsgMenuLaunchSets = 'Settings...';

    MsgCapFxType = 'Select Effect type';
    MsgCapMonsterType = 'Select Monster type';
    MsgCapItemType = 'Select Item type';
    MsgCapAbout = 'About Map Editor';
    MsgCapAct = 'Activation type';
    MsgCapAddSky = 'Select Sky Texture';
    MsgCapAddSound = 'Select Sound/Music';
    MsgCapAddTexture = 'Select Texture';
    MsgCapKeys = 'Keys';
    MsgCapTest = 'Analyse Map';
    MsgCapOpt = 'Optimize Map';
    MsgCapSets = 'Map Properties';
    MsgCapLaunch = 'In-game test Settings';
    MsgCapEs = 'Preferences';
    MsgCapPack = 'Pack Map';
    MsgCapSave = 'Save Map';
    MsgCapMini = 'Save Minimap';
    MsgCapSelect = 'Select Map';
    MsgCapOpen = 'Open Map';
    MsgCapRemove = 'Remove Map';
    MsgCapTexture = 'Texture: %dx%d';
    MsgCapAnimation = 'Animation: %dx%d, %d frames';
    MsgCapStatSelected = 'Selected objects: %d';

    MsgCtrlPropKey = 'Property';
    MsgCtrlPropValue = 'Value';

    MsgCtrlPanels = 'Panels';
    MsgCtrlItems = 'Items';
    MsgCtrlMonsters = 'Monsters';
    MsgCtrlAreas = 'Areas';
    MsgCtrlTriggers = 'Triggers';

    MsgCtrlListTexture = 'Texture List';
    MsgCtrlListItem = 'Item List';
    MsgCtrlListMonster = 'Monster List';
    MsgCtrlListArea = 'Area List';
    MsgCtrlListTrigger = 'Trigger List';
    MsgCtrlListActive = 'Activation Type';
    MsgCtrlListKeys = 'Keys Needed';

    MsgCtrlPreview = 'Texture Preview';
    MsgCtrlItemDm = 'DM Only';
    MsgCtrlItemFalls = 'Falls';
    MsgCtrlLeft = 'Left';
    MsgCtrlRight = 'Right';

    MsgCtrlActPlayerClose = 'Player Collides';
    MsgCtrlActMonsterClose = 'Monster Collides';
    MsgCtrlActPlayerPress = 'Player Pressed';
    MsgCtrlActMonsterPress = 'Monster Pressed';
    MsgCtrlActShot = 'Shot';
    MsgCtrlActNoMonster = 'No Monsters';

    MsgCtrlKeysRed = 'Red Key';
    MsgCtrlKeysGreen = 'Green Key';
    MsgCtrlKeysBlue = 'Blue Key';
    MsgCtrlKeysTeamRed = 'Red Team';
    MsgCtrlKeysTeamBlue = 'Blue Team';

    MsgCtrlOptGb = 'Optimization';
    MsgCtrlOptTexture = 'Texture Optimization';
    MsgCtrlOptPanel = 'Panel Optimization';
    MsgCtrlOptWall = 'Walls';
    MsgCtrlOptFore = 'Foreground';
    MsgCtrlOptBack = 'Background';
    MsgCtrlOptStair = 'Steps';
    MsgCtrlOptWater = 'Water';
    MsgCtrlOptAcid1 = 'Acid 1';
    MsgCtrlOptAcid2 = 'Acid 2';
    MsgCtrlOptLift = 'Streams';
    MsgCtrlOptBlockmon = 'Monster Boundary';
    MsgCtrlOptDescTexture = 'Deletes unused textures from texture list.';
    MsgCtrlOptDescPanel = 'Merges nearby panels of same type. Increases game performance.';
    MsgCtrlSetsStats = 'Statistics:';
    MsgCtrlSetsSizes = 'Map Size:';

    MsgCtrlEsTesting = 'Testing';
    MsgCtrlLaunchDm = 'Deathmatch';
    MsgCtrlLaunchTdm = 'Team Deathmatch';
    MsgCtrlLaunchCtf = 'Capture the Flag';
    MsgCtrlLaunchCoop = 'Cooperative';
    MsgCtrlLaunch2p = 'Two Players';
    MsgCtrlLaunchFf = 'Friendly Fire';
    MsgCtrlLaunchExit = 'Enable Exit';
    MsgCtrlLaunchWeapons = 'Weapons Stay';
    MsgCtrlLaunchMonsters = 'Enable Monsters';
    MsgCtrlLaunchClose = 'Close the game after exiting the map';
    MsgCtrlLaunchOpen = 'Select Doom 2D: Forever executable';

    MsgCtrlEsGeneral = 'General';
    MsgCtrlEsGrid = 'Show Grid';
    MsgCtrlEsTexture = 'Show Panel Texture';
    MsgCtrlEsPanelSize = 'Show Panel Size';
    MsgCtrlEsCheckerboard = 'Use Checkerboard';

    MsgCtrlPackTextures = 'Textures';
    MsgCtrlPackSky = 'Sky';
    MsgCtrlPackMusic = 'Music';
    MsgCtrlPackAdd = 'Don''t overwrite WAD';
    MsgCtrlPackNonStd = 'Non-standard Resources only';

    MsgLabTexWidth = 'Texture Width:';
    MsgLabTexHeight = 'Texture Height:';

    MsgLabSpectrum = 'Show the Spectrum';

    MsgLabAboutVer = 'Version 2.1.5';
    MsgLabAboutAuthor = 'Author: rs.falcon';
    MsgLabAboutAuthor2 = 'Additions: Pss';
    MsgLabAboutMail = 'rmw.falcon@mail.ru';
    MsgLabAboutMail2 = 'pssxx@mail.ru';
    MsgLabAboutSite = 'Doom 2D: Forever Web-site';
    MsgLabAboutHttp = 'www.doom2d.org';

    MsgLabAddWads = 'WAD Files:';
    MsgLabAddSections = 'WAD Sections:';

    MsgLabSetsName = 'Map Name:';
    MsgLabSetsDesc = 'Map Description';
    MsgLabSetsAuthor = 'Author:';
    MsgLabSetsBack = 'Background/Sky:';
    MsgLabSetsMusic = 'Music:';
    MsgLabSetsTextures = 'Textures:';
    MsgLabSetsPanels = 'Panels:';
    MsgLabSetsItems = 'Items:';
    MsgLabSetsMonsters = 'Monsters:';
    MsgLabSetsAreas = 'Areas:';
    MsgLabSetsTriggers = 'Triggers:';
    MsgLabSetsSizes = 'Current:';
    MsgLabSetsWidth = 'Width:';
    MsgLabSetsHeight = 'Height:';
    MsgLabSetsMapmove = 'Direction to shift map objects:';
    MsgLabSetsSnapping = 'Preserve snapping';

    MsgLabLaunchTime = 'Time Limit:';
    MsgLabLaunchSecs = 'seconds';
    MsgLabLaunchScore = 'Score Limit:';
    MsgLabLaunchPathWin = 'Path to Doom2DF.exe:';
    MsgLabLaunchPathMac = 'Path to Doom 2D Forever.app:';
    MsgLabLaunchPathUnix = 'Path to Doom2DF:';
    MsgLabLaunchArgs = 'Launch Arguments:';

    MsgLabEsGrid = 'Grid Step:';
    MsgLabEsGridColor = 'Grid Color:';
    MsgLabEsGridSize = 'Grid Dot Size:';
    MsgLabEsBack = 'Background Color:';
    MsgLabEsPreview = 'Texture Preview Background Color:';
    MsgLabEsMinimap = 'Mini-map Scale:';
    MsgLabEsRecent = 'Recent Maps List Contains:';
    MsgLabEsLanguage = 'Language:';
    MsgLabEsLanguageAuto = 'System Default';

    MsgCtrlEsFiles = 'Files';
    MsgLabEsCompress = 'Compress archive when save';
    MsgLabEsBackup = 'Make backup before save';

    MsgLabPackSaveTo = 'Save to:';
    MsgLabPackMapName = 'Map Resource Name:';
    MsgLabPackTextures = 'Texture Section:';
    MsgLabPackSky = 'Sky Section:';
    MsgLabPackMusic = 'Music Section:';

    MsgLabMiniScale = 'Scale:';

    MsgBtnApplyProps = 'Apply Properties';
    MsgBtnOk = 'OK';
    MsgBtnCancel = 'Cancel';
    MsgBtnAdd = 'Add';
    MsgBtnClose = 'Close';
    MsgBtnAddClose = 'Add and Close';
    MsgBtnTestAgain = 'Check';
    MsgBtnStart = 'Run';
    MsgBtnPack = 'Pack';
    MsgBtnSave = 'Save';
    MsgBtnNoSound = 'No sound';
    MsgBtnTextureAdd = 'Add texture to the list';
    MsgBtnTextureDelete = 'Delete texture from the list';
    MsgBtnTextureEmpty = 'Deselect texture';

    MsgLoadWad = 'Reading WAD';
    MsgLoadMap = 'Loading Map';
    MsgLoadTextures = 'Reading Textures';
    MsgLoadPanels = 'Reading Panels';
    MsgLoadItems = 'Reading Items';
    MsgLoadMonsters = 'Reading Monsters';
    MsgLoadAreas = 'Reading Areas';
    MsgLoadTriggers = 'Reading Triggers';

    MsgTestAreaWall = 'Player collides with the wall and will be stuck.';
    MsgTestAreaWallStr = 'Area #%d collides with Map (%d:%d)';
    MsgTestSpawns1 = 'There are several spawn points for First Player on the map';
    MsgTestSpawns2 = 'There are several spawn points for Second Player on the map';
    MsgTestSpawns = 'There are several spawn points on the map. Random one will be used.';
    MsgTestNoDm = 'There are no DM spawn points on the map';
    MsgTestNoDmEx = 'There are no DM spawn points on the map. Only "Single Player" mode available.';
    MsgTestMonsterWall = 'Monster collides with the wall and will be stuck.';
    MsgTestMonsterWallStr = 'Monster #%d collides with a map (%d:%d)';

    MsgOptNoTextures = 'Texture list is empty';
    MsgOptDeletedTextures = 'Deleted Textures:';
    MsgOptTotalTextures = 'Textures Total:';
    MsgOptTexDeleted = 'Textures Deleted:';
    MsgOptPanelsOpt = 'Panels Optimized:';
    MsgOptWalls = 'Walls Optimization...';
    MsgOptFores = 'Foreground Optimization...';
    MsgOptBacks = 'Background Optimization...';
    MsgOptStairs = 'Steps Optimization...';
    MsgOptWater = 'Water Optimization...';
    MsgOptAcid1 = 'Acid 1 Optimization...';
    MsgOptAcid2 = 'Acid 2 Optimization...';
    MsgOptLifts = 'Streams Optimization...';
    MsgOptBlockmon = 'Monster Boundaries Optimization...';
    MsgOptTotalPanels = 'Panels Total:';
    MsgOptPanelsAfter = 'Panels after Optimization:';

    MsgWadSpecialMap = '<MAP WAD-FILE>';
    MsgWadSpecialTexs = '<EXTRA TEXTURES>';

    MsgFileFilterAll = 'Doom 2D: Forever Maps (*.dfz, *.dfzip, *.zip, *.wad)|*.dfz;*.dfzip;*.zip;*.wad|Doom 2D: Forever 0.30 Maps (*.ini)|*.ini|All Files (*.*)|*.*';
    MsgFileFilterWad = 'Doom 2D: Forever Maps (*.dfz)|*.dfz|Doom 2D: Forever Maps (*.dfzip)|*.dfzip|Doom 2D: Forever Maps (*.zip)|*.zip|Doom 2D: Forever Maps (*.wad)|*.wad|All Files (*.*)|*.*';

    MsgEditorTitle = 'Doom 2D: Forever Map Editor';

Var
  BoolNames: Array [False..True] of String;
  DirNames: Array [D_LEFT..D_RIGHT] of String;
  DirNamesAdv: Array [0..3] of String;
  DirButtonNames: Array [1..4] of String;
  PANELNAMES: Array[0..13] of String;
  EffectNames: Array [EFFECT_NONE..EFFECT_FIRE] of String;
  ItemNames: Array [ITEM_MEDKIT_SMALL..ITEM_MAX] of String;
  ShotNames: Array [TRIGGER_SHOT_PISTOL..TRIGGER_SHOT_MAX] of String;
  MonsterNames: Array [MONSTER_DEMON..MONSTER_MAN] of String;
  AreaNames: Array [AREA_PLAYERPOINT1..AREA_BLUETEAMPOINT] of String;
  TriggerNames: Array [TRIGGER_EXIT..TRIGGER_MAX] of String;

function g_Language_GetList (): TStringList;
procedure g_Language_Set(lang: String);

Implementation

Uses
  g_options, IniFiles, gettext, LazFileUtils,
  SysUtils, e_log, f_main, f_about, f_activationtype,
  f_addresource_sky, f_addresource_sound,
  f_addresource_texture, f_choosetype, f_keys, f_mapcheck,
  f_mapoptions, f_mapoptimization, f_options,
  f_packmap, f_savemap, f_saveminimap, f_selectmap, Forms, utils;

  const
    InSourceLanguage = 'en_US';

procedure SetupArrays();
var
  i: Integer;

begin
// Да/Нет:
  BoolNames[False] := MsgArrayBoolFalse;
  BoolNames[True] := MsgArrayBoolTrue;

// Направления:
  DirNames[D_LEFT] := MsgArrayDirLeft;
  DirNames[D_RIGHT] := MsgArrayDirRight;

// Смены направления:
  DirNamesAdv[0] := MsgArrayDirSame;
  DirNamesAdv[1] := MsgArrayDirLeft;
  DirNamesAdv[2] := MsgArrayDirRight;
  DirNamesAdv[3] := MsgArrayDirReversed;

// Направление (на кнопках):
  DirButtonNames[1] := MsgArrayDirbtnLeft;
  DirButtonNames[2] := MsgArrayDirbtnRight;
  DirButtonNames[3] := MsgArrayDirbtnUp;
  DirButtonNames[4] := MsgArrayDirbtnDown;

// Названия панелей:
  PANELNAMES[0] := MsgArrayPanelWall;
  PANELNAMES[1] := MsgArrayPanelBack;
  PANELNAMES[2] := MsgArrayPanelFront;
  PANELNAMES[3] := MsgArrayPanelDoorOpen;
  PANELNAMES[4] := MsgArrayPanelDoorClose;
  PANELNAMES[5] := MsgArrayPanelStair;
  PANELNAMES[6] := MsgArrayPanelWater;
  PANELNAMES[7] := MsgArrayPanelAcid1;
  PANELNAMES[8] := MsgArrayPanelAcid2;
  PANELNAMES[9] := MsgArrayPanelLiftUp;
  PANELNAMES[10] := MsgArrayPanelLiftDown;
  PANELNAMES[11] := MsgArrayPanelLiftLeft;
  PANELNAMES[12] := MsgArrayPanelLiftRight;
  PANELNAMES[13] := MsgArrayPanelBlockmon;

// Названия эффектов:
  EffectNames[EFFECT_NONE] := MsgArrayFxNone;
  EffectNames[EFFECT_TELEPORT] := MsgArrayFxTeleport;
  EffectNames[EFFECT_RESPAWN] := MsgArrayFxRespawn;
  EffectNames[EFFECT_FIRE] := MsgArrayFxFire;

// Названия предметов:
  ItemNames[ITEM_MEDKIT_SMALL] := MsgArrayItemMedkit;
  ItemNames[ITEM_MEDKIT_LARGE] := MsgArrayItemLargeMedkit;
  ItemNames[ITEM_MEDKIT_BLACK] := MsgArrayItemBlackMedkit;
  ItemNames[ITEM_ARMOR_GREEN] := MsgArrayItemGreenArmor;
  ItemNames[ITEM_ARMOR_BLUE] := MsgArrayItemBlueArmor;
  ItemNames[ITEM_SPHERE_BLUE] := MsgArrayItemBlueSphere;
  ItemNames[ITEM_SPHERE_WHITE] := MsgArrayItemMegasphere;
  ItemNames[ITEM_SUIT] := MsgArrayItemHazSuit;
  ItemNames[ITEM_OXYGEN] := MsgArrayItemOxygen;
  ItemNames[ITEM_INVUL] := MsgArrayItemInvulnerability;
  ItemNames[ITEM_WEAPON_SAW] := MsgArrayItemChainsaw;
  ItemNames[ITEM_WEAPON_SHOTGUN1] := MsgArrayItemShotgun;
  ItemNames[ITEM_WEAPON_SHOTGUN2] := MsgArrayItemDbShotgun;
  ItemNames[ITEM_WEAPON_CHAINGUN] := MsgArrayItemChaingun;
  ItemNames[ITEM_WEAPON_ROCKETLAUNCHER] := MsgArrayItemRocketLauncher;
  ItemNames[ITEM_WEAPON_PLASMA] := MsgArrayItemPlasmaRifle;
  ItemNames[ITEM_WEAPON_BFG] := MsgArrayItemBfg;
  ItemNames[ITEM_WEAPON_SUPERPULEMET] := MsgArrayItemSuperMinigun;
  ItemNames[ITEM_WEAPON_FLAMETHROWER] := MsgArrayItemFlamethrower;
  ItemNames[ITEM_AMMO_BULLETS] := MsgArrayItemClip;
  ItemNames[ITEM_AMMO_BULLETS_BOX] := MsgArrayItemAmmoBox;
  ItemNames[ITEM_AMMO_SHELLS] := MsgArrayItem4Shells;
  ItemNames[ITEM_AMMO_SHELLS_BOX] := MsgArrayItem25Shells;
  ItemNames[ITEM_AMMO_ROCKET] := MsgArrayItem1Rocket;
  ItemNames[ITEM_AMMO_ROCKET_BOX] := MsgArrayItemRocketBox;
  ItemNames[ITEM_AMMO_CELL] := MsgArrayItemCell;
  ItemNames[ITEM_AMMO_CELL_BIG] := MsgArrayItemLargeCell;
  ItemNames[ITEM_AMMO_FUELCAN] := MsgArrayItemFuelcan;
  ItemNames[ITEM_AMMO_BACKPACK] := MsgArrayItemBackpack;
  ItemNames[ITEM_KEY_RED] := MsgArrayItemKeyRed;
  ItemNames[ITEM_KEY_GREEN] := MsgArrayItemKeyGreen;
  ItemNames[ITEM_KEY_BLUE] := MsgArrayItemKeyBlue;
  ItemNames[ITEM_WEAPON_KASTET] := '?';
  ItemNames[ITEM_WEAPON_PISTOL] := '??';
  ItemNames[ITEM_BOTTLE] := MsgArrayItemBottle;
  ItemNames[ITEM_HELMET] := MsgArrayItemHelmet;
  ItemNames[ITEM_JETPACK] := MsgArrayItemJetpack;
  ItemNames[ITEM_INVIS] := MsgArrayItemInvis;

// Названия снарядов:
  ShotNames[TRIGGER_SHOT_PISTOL] := MsgArrayShotPistol;
  ShotNames[TRIGGER_SHOT_BULLET] := MsgArrayShotBullet;
  ShotNames[TRIGGER_SHOT_SHOTGUN] := MsgArrayShotShotgun;
  ShotNames[TRIGGER_SHOT_SSG] := MsgArrayShotSsg;
  ShotNames[TRIGGER_SHOT_IMP] := MsgArrayShotImp;
  ShotNames[TRIGGER_SHOT_PLASMA] := MsgArrayShotPlasma;
  ShotNames[TRIGGER_SHOT_SPIDER] := MsgArrayShotSpider;
  ShotNames[TRIGGER_SHOT_CACO] := MsgArrayShotCaco;
  ShotNames[TRIGGER_SHOT_BARON] := MsgArrayShotBaron;
  ShotNames[TRIGGER_SHOT_MANCUB] := MsgArrayShotMancub;
  ShotNames[TRIGGER_SHOT_REV] := MsgArrayShotRev;
  ShotNames[TRIGGER_SHOT_ROCKET] := MsgArrayShotRocket;
  ShotNames[TRIGGER_SHOT_BFG] := MsgArrayShotBfg;
  ShotNames[TRIGGER_SHOT_EXPL] := MsgArrayShotExpl;
  ShotNames[TRIGGER_SHOT_BFGEXPL] := MsgArrayShotBfgexpl;
  ShotNames[TRIGGER_SHOT_FLAME] := MsgArrayShotFlame;

// Названия монстров:
  MonsterNames[MONSTER_DEMON] := MsgArrayMonDemon;
  MonsterNames[MONSTER_IMP] := MsgArrayMonImp;
  MonsterNames[MONSTER_ZOMBY] := MsgArrayMonZombie;
  MonsterNames[MONSTER_SERG] := MsgArrayMonSergeant;
  MonsterNames[MONSTER_CYBER] := MsgArrayMonCyber;
  MonsterNames[MONSTER_CGUN] := MsgArrayMonCgun;
  MonsterNames[MONSTER_BARON] := MsgArrayMonHellBaron;
  MonsterNames[MONSTER_KNIGHT] := MsgArrayMonHellKnight;
  MonsterNames[MONSTER_CACO] := MsgArrayMonCacodemon;
  MonsterNames[MONSTER_SOUL] := MsgArrayMonLostSoul;
  MonsterNames[MONSTER_PAIN] := MsgArrayMonPainElemental;
  MonsterNames[MONSTER_SPIDER] := MsgArrayMonMastermind;
  MonsterNames[MONSTER_BSP] := MsgArrayMonArachnatron;
  MonsterNames[MONSTER_MANCUB] := MsgArrayMonMancubus;
  MonsterNames[MONSTER_SKEL] := MsgArrayMonRevenant;
  MonsterNames[MONSTER_VILE] := MsgArrayMonArchvile;
  MonsterNames[MONSTER_FISH] := MsgArrayMonFish;
  MonsterNames[MONSTER_BARREL] := MsgArrayMonBarrel;
  MonsterNames[MONSTER_ROBO] := MsgArrayMonRobot;
  MonsterNames[MONSTER_MAN] := MsgArrayMonPrikolist;

// Названия областей:
  AreaNames[AREA_PLAYERPOINT1] := MsgArrayAreaPlayerOne;
  AreaNames[AREA_PLAYERPOINT2] := MsgArrayAreaPlayerTwo;
  AreaNames[AREA_DMPOINT] := MsgArrayAreaDm;
  AreaNames[AREA_REDFLAG] := MsgArrayAreaFlagRed;
  AreaNames[AREA_BLUEFLAG] := MsgArrayAreaFlagBlue;
  AreaNames[AREA_DOMFLAG] := MsgArrayAreaFlagDom;
  AreaNames[AREA_REDTEAMPOINT] := MsgArrayAreaTeamRed;
  AreaNames[AREA_BLUETEAMPOINT] := MsgArrayAreaTeamBlue;

// Названия триггеров:
  TriggerNames[TRIGGER_EXIT] := MsgArrayTrExit;
  TriggerNames[TRIGGER_TELEPORT] := MsgArrayTrTeleport;
  TriggerNames[TRIGGER_OPENDOOR] := MsgArrayTrDoorOpen;
  TriggerNames[TRIGGER_CLOSEDOOR] := MsgArrayTrDoorClose;
  TriggerNames[TRIGGER_DOOR] := MsgArrayTrDoorSwitch;
  TriggerNames[TRIGGER_DOOR5] := MsgArrayTrDoor5Sec;
  TriggerNames[TRIGGER_CLOSETRAP] := MsgArrayTrTrapClose;
  TriggerNames[TRIGGER_TRAP] := MsgArrayTrTrap;
  TriggerNames[TRIGGER_PRESS] := MsgArrayTrExtend;
  TriggerNames[TRIGGER_SECRET] := MsgArrayTrSecret;
  TriggerNames[TRIGGER_LIFTUP] := MsgArrayTrLiftUp;
  TriggerNames[TRIGGER_LIFTDOWN] := MsgArrayTrLiftDown;
  TriggerNames[TRIGGER_LIFT] := MsgArrayTrLiftSwitch;
  TriggerNames[TRIGGER_TEXTURE] := MsgArrayTrTexture;
  TriggerNames[TRIGGER_ON] := MsgArrayTrOn;
  TriggerNames[TRIGGER_OFF] := MsgArrayTrOff;
  TriggerNames[TRIGGER_ONOFF] := MsgArrayTrSwitch;
  TriggerNames[TRIGGER_SOUND] := MsgArrayTrSound;
  TriggerNames[TRIGGER_SPAWNMONSTER] := MsgArrayTrSpawnMonster;
  TriggerNames[TRIGGER_SPAWNITEM] := MsgArrayTrSpawnItem;
  TriggerNames[TRIGGER_MUSIC] := MsgArrayTrMusic;
  TriggerNames[TRIGGER_PUSH] := MsgArrayTrPush;
  TriggerNames[TRIGGER_SCORE] := MsgArrayTrScore;
  TriggerNames[TRIGGER_MESSAGE] := MsgArrayTrMessage;
  TriggerNames[TRIGGER_DAMAGE] := MsgArrayTrDamage;
  TriggerNames[TRIGGER_HEALTH] := MsgArrayTrHealth;
  TriggerNames[TRIGGER_SHOT] := MsgArrayTrShot;
  TriggerNames[TRIGGER_EFFECT] := MsgArrayTrEffect;

// Установка значений в панели выбора объектов:
  with MainForm do
  begin
    lbPanelType.Items.Clear();
    for i := 0 to High(PANELNAMES) do
      lbPanelType.Items.Add(PANELNAMES[i]);
    lbPanelType.ItemIndex := 0;

    lbItemList.Clear();
    for i := ITEM_MEDKIT_SMALL to ITEM_KEY_BLUE do
      lbItemList.Items.Add(ItemNames[i]);
    lbItemList.Items.Add(ItemNames[ITEM_BOTTLE]);
    lbItemList.Items.Add(ItemNames[ITEM_HELMET]);
    lbItemList.Items.Add(ItemNames[ITEM_JETPACK]);
    lbItemList.Items.Add(ItemNames[ITEM_INVIS]);
    lbItemList.Items.Add(ItemNames[ITEM_WEAPON_FLAMETHROWER]);
    lbItemList.Items.Add(ItemNames[ITEM_AMMO_FUELCAN]);

    lbMonsterList.Clear();
    for i := MONSTER_DEMON to MONSTER_MAN do
      lbMonsterList.Items.Add(MonsterNames[i]);

    lbAreasList.Clear();
    for i := AREA_PLAYERPOINT1 to AREA_BLUETEAMPOINT do
      lbAreasList.Items.Add(AreaNames[i]);

    lbTriggersList.Clear();
    for i := Low(TriggerNames) to High(TriggerNames) do
      lbTriggersList.Items.Add(TriggerNames[i]);

    clbActivationType.Clear();
    clbActivationType.Items.Add(MsgCtrlActPlayerClose);
    clbActivationType.Items.Add(MsgCtrlActMonsterClose);
    clbActivationType.Items.Add(MsgCtrlActPlayerPress);
    clbActivationType.Items.Add(MsgCtrlActMonsterPress);
    clbActivationType.Items.Add(MsgCtrlActShot);
    clbActivationType.Items.Add(MsgCtrlActNoMonster);

    clbKeys.Clear();
    clbKeys.Items.Add(MsgCtrlKeysRed);
    clbKeys.Items.Add(MsgCtrlKeysGreen);
    clbKeys.Items.Add(MsgCtrlKeysBlue);
    clbKeys.Items.Add(MsgCtrlKeysTeamRed);
    clbKeys.Items.Add(MsgCtrlKeysTeamBlue);
  end;
end;

procedure SetupCaptions();
  var i: Integer; s: AnsiString;
begin
// Главная форма:
  with MainForm do
  begin
  // Заголовок:
    s := g_GetBuildHash(false);
    if s = 'custom build' then
      s := s + ' by ' + g_GetBuilderName() + ' ' + EDITOR_BUILDDATE + ' ' + EDITOR_BUILDTIME;
    FormCaption := MsgEditorTitle + ' (' + s + ')';
    i := Pos('-', Caption);
    if i > 0 then
      begin
        Caption := FormCaption + ' ' + Copy(Caption, i, Length(Caption)-i+1);
      end
    else
      Caption := FormCaption;

  // Apple menu:
    miAppleAbout.Caption := MsgMenuAppleAbout;
    miApplePref.Caption := MsgMenuApplePref;
  // File menu:
    miMenuFile.Caption := MsgMenuFile;
    miNewMap.Caption := MsgMenuFileNew;
    miOpenMap.Caption := MsgMenuFileOpen;
    miMacRecentSubMenu.Caption := MsgMenuFileRecentMac;
    miMacRecentClear.Caption := MsgMenuFileRecentClearMac;
    miSaveMap.Caption := MsgMenuFileSave;
    miSaveMapAs.Caption := MsgMenuFileSaveAs;
    miOpenWadMap.Caption := MsgMenuFileOpenWad;
    miReopenMap.Caption := MsgMenuFileReopen;
    miSaveMiniMap.Caption := MsgMenuFileSaveMini;
    miDeleteMap.Caption := MsgMenuFileDelete;
    miPackMap.Caption := MsgMenuFilePackMap;
    miWinRecent.Caption := MsgMenuFileRecentWin;
    miExit.Caption := MsgMenuFileExitWin;
  // Edit menu:
    miMenuEdit.Caption := MsgMenuEdit;
    miUndo.Caption := MsgMenuEditUndo;
    miCopy.Caption := MsgMenuEditCopy;
    miCut.Caption := MsgMenuEditCut;
    miPaste.Caption := MsgMenuEditPaste;
    miSelectAll.Caption := MsgMenuEditSelectAll;
    miSnapToGrid.Caption := MsgMenuEditSnapGrid;
    miSwitchGrid.Caption := MsgMenuEditStepGrid;
    miToFore.Caption := MsgMenuEditToFore;
    miToBack.Caption := MsgMenuEditToBack;
    miMapOptions.Caption := MsgMenuEditMapProps;
    miOptions.Caption := MsgMenuEditPrefWin;
  // View menu:
    miMenuView.Caption := MsgMenuView;
    miLayers.Caption := MsgMenuViewLayers;
    miLayer1.Caption := MsgMenuLayerBack;
    miLayer2.Caption := MsgMenuLayerWall;
    miLayer3.Caption := MsgMenuLayerFore;
    miLayer4.Caption := MsgMenuLayerStair;
    miLayer5.Caption := MsgMenuLayerWater;
    miLayer6.Caption := MsgMenuLayerItem;
    miLayer7.Caption := MsgMenuLayerMonster;
    miLayer8.Caption := MsgMenuLayerArea;
    miLayer9.Caption := MsgMenuLayerTrigger;
    miMiniMap.Caption := MsgMenuViewMinimap;
    miShowEdges.Caption := MsgMenuViewBounds;
    miMapPreview.Caption := MsgMenuViewPreview;
  // Service menu:
    miMenuService.Caption := MsgMenuServ;
    miCheckMap.Caption := MsgMenuServTest;
    miOptimmization.Caption := MsgMenuServOpt;
    miTestMap.Caption := MsgMenuServLaunch;
  // Window menu:
    miMenuWindow.Caption := MsgMenuWindow;
    miMacMinimize.Caption := MsgMenuWindowMinimize;
    miMacZoom.Caption := MsgMenuWindowZoom;
  // Help menu:
    miMenuHelp.Caption := MsgMenuHelp;
    miAbout.Caption := MsgMenuHelpAboutWin;

  // Toolbar:
    tbNewMap.Hint := MsgMenuTbNew;
    tbOpenMap.Hint := MsgMenuTbOpen;
    tbSaveMap.Hint := MsgMenuTbSave;
    tbOpenWadMap.Hint := MsgMenuTbOpenWad;
    tbShowMap.Hint := MsgMenuTbMinimap;
    tbShow.Hint := MsgMenuTbLayers;
    miLayerP1.Caption := MsgMenuLayerBack;
    miLayerP2.Caption := MsgMenuLayerWall;
    miLayerP3.Caption := MsgMenuLayerFore;
    miLayerP4.Caption := MsgMenuLayerStair;
    miLayerP5.Caption := MsgMenuLayerWater;
    miLayerP6.Caption := MsgMenuLayerItem;
    miLayerP7.Caption := MsgMenuLayerMonster;
    miLayerP8.Caption := MsgMenuLayerArea;
    miLayerP9.Caption := MsgMenuLayerTrigger;
    tbGridOn.Hint := MsgMenuTbGrid;
    tbGrid.Hint := MsgMenuTbGridStep;
    tbTestMap.Hint := MsgMenuTbLaunch;

  // Object property editor:
    bApplyProperty.Caption := MsgBtnApplyProps;
    vleObjectProperty.TitleCaptions[0] := MsgCtrlPropKey;
    vleObjectProperty.TitleCaptions[1] := MsgCtrlPropValue;

  // Panels Tab:
    tsPanels.Caption := MsgCtrlPanels;
    lbPanelType.Hint := MsgPropPanelType;
    lbTextureList.Hint := MsgCtrlListTexture;
    LabelTxW.Caption := MsgLabTexWidth;
    LabelTxH.Caption := MsgLabTexHeight;
    cbPreview.Caption := MsgCtrlPreview;
    bbAddTexture.Hint := MsgBtnTextureAdd;
    bbRemoveTexture.Hint := MsgBtnTextureDelete;
    bClearTexture.Hint := MsgBtnTextureEmpty;

  // Items Tab:
    tsItems.Caption := MsgCtrlItems;
    lbItemList.Hint := MsgCtrlListItem;
    cbOnlyDM.Caption := MsgCtrlItemDm;
    cbFall.Caption := MsgCtrlItemFalls;

  // Monters Tab:
    tsMonsters.Caption := MsgCtrlMonsters;
    lbMonsterList.Hint := MsgCtrlListMonster;
    rbMonsterLeft.Caption := MsgCtrlLeft;
    rbMonsterRight.Caption := MsgCtrlRight;

  // Areas Tab:
    tsAreas.Caption := MsgCtrlAreas;
    lbAreasList.Hint := MsgCtrlListArea;
    rbAreaLeft.Caption := MsgCtrlLeft;
    rbAreaRight.Caption := MsgCtrlRight;

  // Triggers Tab:
    tsTriggers.Caption := MsgCtrlTriggers;
    lbTriggersList.Hint := MsgCtrlListTrigger;
    clbActivationType.Hint := MsgCtrlListActive;
    clbKeys.Hint := MsgCtrlListKeys;
  end;

// Форма "О программе":
  with AboutForm do
  begin
    Caption := MsgCapAbout;
    LabelTitle.Caption := MsgEditorTitle;
    LabelVer.Caption := MsgLabAboutVer;
    LabelAuthor.Caption := MsgLabAboutAuthor;
    LabelAuthor2.Caption := MsgLabAboutAuthor2;
    LabelMail.Caption := MsgLabAboutMail;
    LabelMail2.Caption := MsgLabAboutMail2;
    LabelSite.Caption := MsgLabAboutSite;
    LabelHttp.Caption := MsgLabAboutHttp;
  end;

// Форма "Тип активации":
  with ActivationTypeForm do
  begin
    Caption := MsgCapAct;
    cbPlayerCollide.Caption := MsgCtrlActPlayerClose;
    cbMonsterCollide.Caption := MsgCtrlActMonsterClose;
    cbPlayerPress.Caption := MsgCtrlActPlayerPress;
    cbMonsterPress.Caption := MsgCtrlActMonsterPress;
    cbShot.Caption := MsgCtrlActShot;
    cbNoMonster.Caption := MsgCtrlActNoMonster;
    bOK.Caption := MsgBtnOk;
  end;

// Форма "Выбор текстуры для неба":
  with AddSkyForm do
  begin
    Caption := MsgCapAddSky;
    LabelWADs.Caption := MsgLabAddWads;
    LabelSections.Caption := MsgLabAddSections;
    bOK.Caption := MsgBtnOk;
    bCancel.Caption := MsgBtnCancel;
  end;

// Форма "Выбор звука или музыки":
  with AddSoundForm do
  begin
    Caption := MsgCapAddSound;
    LabelWADs.Caption := MsgLabAddWads;
    LabelSections.Caption := MsgLabAddSections;
    bOK.Caption := MsgBtnOk;
    bCancel.Caption := MsgBtnCancel;
    bEmpty.Caption := MsgBtnNoSound;
  end;

// Форма "Выбор текстуры":
  with AddTextureForm do
  begin
    Caption := MsgCapAddTexture;
    LabelWADs.Caption := MsgLabAddWads;
    LabelSections.Caption := MsgLabAddSections;
    bAddTexture.Caption := MsgBtnAdd;
    bClose.Caption := MsgBtnClose;
    bAddClose.Caption := MsgBtnAddClose;
  end;

// Форма "Выбор типа монстра" / "Выбор типа предмета":
  ChooseTypeForm.bOK.Caption := MsgBtnOk;

// Форма "Ключи":
  with KeysForm do
  begin
    Caption := MsgCapKeys;
    cbRedKey.Caption := MsgCtrlKeysRed;
    cbGreenKey.Caption := MsgCtrlKeysGreen;
    cbBlueKey.Caption := MsgCtrlKeysBlue;
    cbRedTeam.Caption := MsgCtrlKeysTeamRed;
    cbBlueTeam.Caption := MsgCtrlKeysTeamBlue;
    bOK.Caption := MsgBtnOk;
  end;

// Форма "Проверка карты":
  with MapCheckForm do
  begin
    Caption := MsgCapTest;
    bCheckMap.Caption := MsgBtnTestAgain;
    bClose.Caption := MsgBtnClose;
  end;

// Форма "Оптимизация карты":
  with MapOptimizationForm do
  begin
    Caption := MsgCapOpt;
  // Выбор оптимизации:
    GroupBoxOpt.Caption := MsgCtrlOptGb;
    rbTexturesOptimization.Caption := MsgCtrlOptTexture;
    rbPanelsOptimization.Caption := MsgCtrlOptPanel;
  // Оптимизация текстур:
    bBeginTextureOptimization.Caption := MsgBtnStart;
  // Оптимизация панелей:
    cbOptimizeWalls.Caption := MsgCtrlOptWall;
    cbOptimizeForeGround.Caption := MsgCtrlOptFore;
    cbOptimizeBackGround.Caption := MsgCtrlOptBack;
    cbOptimizeSteps.Caption := MsgCtrlOptStair;
    cbOptimizeWater.Caption := MsgCtrlOptWater;
    cbOptimizeAcid1.Caption := MsgCtrlOptAcid1;
    cbOptimizeAcid2.Caption := MsgCtrlOptAcid2;
    cbOptimizeLift.Caption := MsgCtrlOptLift;
    cbOptimizeBlockMon.Caption := MsgCtrlOptBlockmon;
    bBeginPanelsOptimization.Caption := MsgBtnStart;
  end;

// Форма "Параметры карты":
  with MapOptionsForm do
  begin
    Caption := MsgCapSets;
  // Основные параметры:
    LabelName.Caption := MsgLabSetsName;
    LabelDesc.Caption := MsgLabSetsDesc;
    LabelAuthor.Caption := MsgLabSetsAuthor;
    LabelBack.Caption := MsgLabSetsBack;
    LabelMusic.Caption := MsgLabSetsMusic;
  // Статистика:
    GBStats.Caption := MsgCtrlSetsStats;
    LabelTexs.Caption := MsgLabSetsTextures;
    LabelPanels.Caption := MsgLabSetsPanels;
    LabelItems.Caption := MsgLabSetsItems;
    LabelMonsters.Caption := MsgLabSetsMonsters;
    LabelAreas.Caption := MsgLabSetsAreas;
    LabelTriggers.Caption := MsgLabSetsTriggers;
  // Размеры:
    GBSizes.Caption := MsgCtrlSetsSizes;
    LabelWidth.Caption := MsgLabSetsWidth;
    LabelHeight.Caption := MsgLabSetsHeight;
    LabelCurSize.Caption := MsgLabSetsSizes;
    LabelMapMove.Caption := MsgLabSetsMapmove;
    cbSnapping.Caption := MsgLabSetsSnapping;
  // Кнопки:
    bOK.Caption := MsgBtnOk;
    bCancel.Caption := MsgBtnCancel;
  end;

// Form preferences:
  with OptionsForm do
  begin
    Caption := MsgCapEs;
    bOK.Caption := MsgBtnOk;
    bCancel.Caption := MsgBtnCancel;
  // TabGeneral:
    TabGeneral.Caption := MsgCtrlEsGeneral;
    cbShowDots.Caption := MsgCtrlEsGrid;
    cbShowTexture.Caption := MsgCtrlEsTexture;
    cbShowSize.Caption := MsgCtrlEsPanelSize;
    cbCheckerboard.Caption := MsgCtrlEsCheckerboard;
    LabelGrid.Caption := MsgLabEsGrid;
    LabelGridCol.Caption := MsgLabEsGridColor;
    LabelGridSize.Caption := MsgLabEsGridSize;
    LabelBack.Caption := MsgLabEsBack;
    LabelPreview.Caption := MsgLabEsPreview;
    LabelMinimap.Caption := MsgLabEsMinimap;
    LabelLanguage.Caption := MsgLabEsLanguage;
  // TabFiles:
    TabFiles.Caption := MsgCtrlEsFiles;
    cbCompress.Caption := MsgLabEsCompress;
    cbBackup.Caption := MsgLabEsBackup;
    LabelRecent.Caption := MsgLabEsRecent;
  // TabTesting:
    TabTesting.Caption := MsgCtrlEsTesting;
    rbDM.Caption := MsgCtrlLaunchDm;
    rbTDM.Caption := MsgCtrlLaunchTdm;
    rbCTF.Caption := MsgCtrlLaunchCtf;
    rbCOOP.Caption := MsgCtrlLaunchCoop;
    cbTwoPlayers.Caption := MsgCtrlLaunch2p;
    cbTeamDamage.Caption := MsgCtrlLaunchFf;
    cbAllowExit.Caption := MsgCtrlLaunchExit;
    cbWeaponStay.Caption := MsgCtrlLaunchWeapons;
    cbMonstersDM.Caption := MsgCtrlLaunchMonsters;
    cbMapOnce.Caption := MsgCtrlLaunchClose;
    LabelTime.Caption := MsgLabLaunchTime;
    LabelSecs.Caption := MsgLabLaunchSecs;
    LabelScore.Caption := MsgLabLaunchScore;
    {$IF DEFINED(DARWIN)}
      LabelPath.Caption := MsgLabLaunchPathMac;
    {$ELSEIF DEFINED(WINDOWS)}
      LabelPath.Caption := MsgLabLaunchPathWin;
    {$ELSE}
      LabelPath.Caption := MsgLabLaunchPathUnix;
    {$ENDIF}
    FindD2dDialog.Title := MsgCtrlLaunchOpen;
    LabelArgs.Caption := MsgLabLaunchArgs;
  end;

// Форма "Упаковать карту":
  with PackMapForm do
  begin
    Caption := MsgCapPack;
    bPack.Caption := MsgBtnPack;
    LabelSaveTo.Caption := MsgLabPackSaveTo;
    LabelMapName.Caption := MsgLabPackMapName;
  // Что упаковывать:
    cbTextrures.Caption := MsgCtrlPackTextures;
    LabelTextures.Caption := MsgLabPackTextures;
    cbSky.Caption := MsgCtrlPackSky;
    LabelSky.Caption := MsgLabPackSky;
    cbMusic.Caption := MsgCtrlPackMusic;
    LabelMusic.Caption := MsgLabPackMusic;
    cbAdd.Caption := MsgCtrlPackAdd;
    cbNonStandart.Caption := MsgCtrlPackNonStd;
  end;

// Форма "Сохранить карту":
  with SaveMapForm do
  begin
    Caption := MsgCapSave;
    bOK.Caption := MsgBtnOk;
    bCancel.Caption := MsgBtnCancel;
  end;

// Форма "Сохранить мини-карту":
  with SaveMiniMapForm do
  begin
    Caption := MsgCapMini;
    LabelScale.Caption := MsgLabMiniScale;
    bSave.Caption := MsgBtnSave;
    bClose.Caption := MsgBtnClose;
  end;

// Форма "Выбор карты":
  with SelectMapForm do
  begin
    Caption := MsgCapSelect;
    bOK.Caption := MsgBtnOk;
    bCancel.Caption := MsgBtnCancel;
  end;

// Заголовок приложения:
  Application.Title := MsgEditorTitle;
end;

type
  TResArg = record
    ini: TIniFile;
    ignored: TStringList;
  end;
  PResArg = ^TResArg;

function gResourceItarator (name, value: AnsiString; hash: LongInt; arg: Pointer): AnsiString;
  var res: PResArg; orig: AnsiString;
begin
  res := PResArg(arg);
  orig := res.ini.ReadString('resourcestring', name + '$', '');
  if (orig = '') or (orig = value) then
  begin
    if res.ini.ValueExists('resourcestring', name) then
    begin
      result := res.ini.ReadString('resourcestring', name, '');
    end
    else
    begin
      result := value;
      if res.ignored.IndexOf(Copy(name, 1, Pos('.', name) - 1)) < 0 then
        e_WriteLog('  Seems that key ' + name + ' not translated', MSG_NOTIFY);
    end;
  end
  else
  begin
    e_WriteLog('  Original resource string for ' + name + ' do not match, translation are outdated?', MSG_WARNING);
    e_WriteLog('    [' + value + '] -> [' + orig + ']', MSG_WARNING);
    result := value;
  end;
end;

procedure gSetLanguageFormStream (const lang: AnsiString; stream: TStream; out ok: Boolean);
  var res: TResArg;
begin
  ok := False;
  try
    res.ini := TIniFile.Create(stream, [ifoStripComments, ifoStripQuotes, ifoEscapeLineFeeds]);
  except
    res.ini := nil;
  end;
  if res.ini <> nil then
  begin
    try
      ok := res.ini.SectionExists('resourcestring');
      if ok then
      begin
        res.ignored := TStringList.Create;
        res.ignored.CaseSensitive := False;
        res.ini.ReadSection('ignore', res.ignored);
        res.ignored.Sort;
        SetResourceStrings(gResourceItarator, @res);
        res.ignored.Free();
      end;
    finally
      res.ini.Free();
    end;
  end;
  if not ok then e_WriteLog('Translation file for ' + lang + ' are invalid ', MSG_FATALERROR);
end;

procedure gSetLanguageFromFile (const lang: AnsiString; out ok: Boolean);
  const langfilename = 'editor';
  var stream: TFileStream; name: AnsiString;
begin
  name := LangDir + DirectorySeparator + langfilename + '.' + lang + '.lng';
  try
    stream := TFileStream.Create(name, fmOpenRead);
    try
      gSetLanguageFormStream(lang, stream, ok);
    finally
      stream.Free();
    end;
  except on E: EFOpenError do
    ok := False;
  end;
end;

procedure gSetLanguage (const lang: AnsiString; out ok: Boolean);
begin
  gSetLanguageFromFile(lang, ok);
end;

function g_Language_GetList (): TStringList;
  const langfilename = 'editor';
  var list: TStringList; info: TSearchRec;
begin
  list := TStringList.Create;
  list.Duplicates := dupIgnore;
  list.Add(InSourceLanguage);
  if FindFirst(LangDir + DirectorySeparator + langfilename + '.*.lng', faAnyFile, info) = 0 then
  begin
    repeat
      list.Add(Copy(ExtractFileNameWithoutExt(info.Name), Length(langfilename) + 2));
    until FindNext(info) <> 0;
    FindClose(info);
  end;
  list.Sort;
  result := list;
end;

procedure g_Language_Set(lang: String);
  var syslang, fallbacklang: String; ok: Boolean;
begin
  ResetResourceTables;

  if lang = '' then
  begin
    GetLanguageIDs(syslang, fallbacklang); // TODO: remove dependency on gettext
    e_WriteLog('g_Language_Set: try strings "' + syslang + '" (system)', MSG_NOTIFY);
    gSetLanguage(syslang, ok);
    if not ok then
    begin
      e_WriteLog('g_Language_Set: try strings "' + fallbacklang + '" (fallback)', MSG_NOTIFY);
      gSetLanguage(syslang, ok);
    end;
  end
  else
  begin
    e_WriteLog('g_Language_Set: try strings "' + lang + '" (user specified)', MSG_NOTIFY);
    gSetLanguage(lang, ok);
  end;
  if not ok then e_WriteLog('g_Language_Set: use default strings "' + InSourceLanguage + '" (in-source)', MSG_NOTIFY);

  SetupArrays();
  SetupCaptions();
  RemoveSelectFromObjects();
end;

End.
