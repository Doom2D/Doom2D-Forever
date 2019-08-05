unit MAPDEF;

{$INCLUDE ../shared/a_modes.inc}

{
-----------------------------------
MAPDEF.PAS ВЕРСИЯ ОТ 22.03.09

Поддержка карт версии 1
-----------------------------------
}

interface

uses
  MAPSTRUCT;

const
  PANEL_NONE      = 0;
  PANEL_WALL      = 1;
  PANEL_BACK      = 2;
  PANEL_FORE      = 4;
  PANEL_WATER     = 8;
  PANEL_ACID1     = 16;
  PANEL_ACID2     = 32;
  PANEL_STEP      = 64;
  PANEL_LIFTUP    = 128;
  PANEL_LIFTDOWN  = 256;
  PANEL_OPENDOOR  = 512;
  PANEL_CLOSEDOOR = 1024;
  PANEL_BLOCKMON  = 2048;
  PANEL_LIFTLEFT  = 4096;
  PANEL_LIFTRIGHT = 8192;

  PANEL_FLAG_BLENDING      = 1;
  PANEL_FLAG_HIDE          = 2;
  PANEL_FLAG_WATERTEXTURES = 4;

  EFFECT_NONE     = 0;
  EFFECT_TELEPORT = 1;
  EFFECT_RESPAWN  = 2;
  EFFECT_FIRE     = 3;

  ITEM_NONE                  = 0;
  ITEM_MEDKIT_SMALL          = 1;
  ITEM_MEDKIT_LARGE          = 2;
  ITEM_MEDKIT_BLACK          = 3;
  ITEM_ARMOR_GREEN           = 4;
  ITEM_ARMOR_BLUE            = 5;
  ITEM_SPHERE_BLUE           = 6;
  ITEM_SPHERE_WHITE          = 7;
  ITEM_SUIT                  = 8;
  ITEM_OXYGEN                = 9;
  ITEM_INVUL                 = 10;
  ITEM_WEAPON_SAW            = 11;
  ITEM_WEAPON_SHOTGUN1       = 12;
  ITEM_WEAPON_SHOTGUN2       = 13;
  ITEM_WEAPON_CHAINGUN       = 14;
  ITEM_WEAPON_ROCKETLAUNCHER = 15;
  ITEM_WEAPON_PLASMA         = 16;
  ITEM_WEAPON_BFG            = 17;
  ITEM_WEAPON_SUPERPULEMET   = 18;
  ITEM_AMMO_BULLETS          = 19;
  ITEM_AMMO_BULLETS_BOX      = 20;
  ITEM_AMMO_SHELLS           = 21;
  ITEM_AMMO_SHELLS_BOX       = 22;
  ITEM_AMMO_ROCKET           = 23;
  ITEM_AMMO_ROCKET_BOX       = 24;
  ITEM_AMMO_CELL             = 25;
  ITEM_AMMO_CELL_BIG         = 26;
  ITEM_AMMO_BACKPACK         = 27;
  ITEM_KEY_RED               = 28;
  ITEM_KEY_GREEN             = 29;
  ITEM_KEY_BLUE              = 30;
  ITEM_WEAPON_KASTET         = 31;
  ITEM_WEAPON_PISTOL         = 32;
  ITEM_BOTTLE                = 33;
  ITEM_HELMET                = 34;
  ITEM_JETPACK               = 35;
  ITEM_INVIS                 = 36;
  ITEM_WEAPON_FLAMETHROWER   = 37;
  ITEM_AMMO_FUELCAN          = 38;

  ITEM_MAX                   = 38; // store the last item's id in here
                                   // use this in for loops

  ITEM_OPTION_ONLYDM = 1;
  ITEM_OPTION_FALL   = 2;

  AREA_NONE          = 0;
  AREA_PLAYERPOINT1  = 1;
  AREA_PLAYERPOINT2  = 2;
  AREA_DMPOINT       = 3;
  AREA_REDFLAG       = 4;
  AREA_BLUEFLAG      = 5;
  AREA_DOMFLAG       = 6;
  AREA_REDTEAMPOINT  = 7;
  AREA_BLUETEAMPOINT = 8;

  MONSTER_NONE   = 0;
  MONSTER_DEMON  = 1;
  MONSTER_IMP    = 2;
  MONSTER_ZOMBY  = 3;
  MONSTER_SERG   = 4;
  MONSTER_CYBER  = 5;
  MONSTER_CGUN   = 6;
  MONSTER_BARON  = 7;
  MONSTER_KNIGHT = 8;
  MONSTER_CACO   = 9;
  MONSTER_SOUL   = 10;
  MONSTER_PAIN   = 11;
  MONSTER_SPIDER = 12;
  MONSTER_BSP    = 13;
  MONSTER_MANCUB = 14;
  MONSTER_SKEL   = 15;
  MONSTER_VILE   = 16;
  MONSTER_FISH   = 17;
  MONSTER_BARREL = 18;
  MONSTER_ROBO   = 19;
  MONSTER_MAN    = 20;

  TRIGGER_NONE            = 0;
  TRIGGER_EXIT            = 1;
  TRIGGER_TELEPORT        = 2;
  TRIGGER_OPENDOOR        = 3;
  TRIGGER_CLOSEDOOR       = 4;
  TRIGGER_DOOR            = 5;
  TRIGGER_DOOR5           = 6;
  TRIGGER_CLOSETRAP       = 7;
  TRIGGER_TRAP            = 8;
  TRIGGER_PRESS           = 9;
  TRIGGER_SECRET          = 10;
  TRIGGER_LIFTUP          = 11;
  TRIGGER_LIFTDOWN        = 12;
  TRIGGER_LIFT            = 13;
  TRIGGER_TEXTURE         = 14;
  TRIGGER_ON              = 15;
  TRIGGER_OFF             = 16;
  TRIGGER_ONOFF           = 17;
  TRIGGER_SOUND           = 18;
  TRIGGER_SPAWNMONSTER    = 19;
  TRIGGER_SPAWNITEM       = 20;
  TRIGGER_MUSIC           = 21;
  TRIGGER_PUSH            = 22;
  TRIGGER_SCORE           = 23;
  TRIGGER_MESSAGE         = 24;
  TRIGGER_DAMAGE          = 25;
  TRIGGER_HEALTH          = 26;
  TRIGGER_SHOT            = 27;
  TRIGGER_EFFECT          = 28;
  TRIGGER_MAX             = 28;

  TRIGGER_SHOT_PISTOL  = 0;
  TRIGGER_SHOT_BULLET  = 1;
  TRIGGER_SHOT_SHOTGUN = 2;
  TRIGGER_SHOT_SSG     = 3;
  TRIGGER_SHOT_IMP     = 4;
  TRIGGER_SHOT_PLASMA  = 5;
  TRIGGER_SHOT_SPIDER  = 6;
  TRIGGER_SHOT_CACO    = 7;
  TRIGGER_SHOT_BARON   = 8;
  TRIGGER_SHOT_MANCUB  = 9;
  TRIGGER_SHOT_REV     = 10;
  TRIGGER_SHOT_ROCKET  = 11;
  TRIGGER_SHOT_BFG     = 12;
  TRIGGER_SHOT_EXPL    = 13;
  TRIGGER_SHOT_BFGEXPL = 14;
  TRIGGER_SHOT_FLAME   = 15;
  TRIGGER_SHOT_MAX     = 15;

  TRIGGER_SHOT_TARGET_NONE   = 0;
  TRIGGER_SHOT_TARGET_MON    = 1;
  TRIGGER_SHOT_TARGET_PLR    = 2;
  TRIGGER_SHOT_TARGET_RED    = 3;
  TRIGGER_SHOT_TARGET_BLUE   = 4;
  TRIGGER_SHOT_TARGET_MONPLR = 5;
  TRIGGER_SHOT_TARGET_PLRMON = 6;

  TRIGGER_SHOT_AIM_DEFAULT   = 0;
  TRIGGER_SHOT_AIM_ALLMAP    = 1;
  TRIGGER_SHOT_AIM_TRACE     = 2;
  TRIGGER_SHOT_AIM_TRACEALL  = 3;

  TRIGGER_EFFECT_PARTICLE  = 0;
  TRIGGER_EFFECT_ANIMATION = 1;

  TRIGGER_EFFECT_SLIQUID = 0;
  TRIGGER_EFFECT_LLIQUID = 1;
  TRIGGER_EFFECT_DLIQUID = 2;
  TRIGGER_EFFECT_BLOOD   = 3;
  TRIGGER_EFFECT_SPARK   = 4;
  TRIGGER_EFFECT_BUBBLE  = 5;
  TRIGGER_EFFECT_MAX     = 5;

  TRIGGER_EFFECT_POS_CENTER = 0;
  TRIGGER_EFFECT_POS_AREA   = 1;

  ACTIVATE_PLAYERCOLLIDE  = 1;
  ACTIVATE_MONSTERCOLLIDE = 2;
  ACTIVATE_PLAYERPRESS    = 4;
  ACTIVATE_MONSTERPRESS   = 8;
  ACTIVATE_SHOT           = 16;
  ACTIVATE_NOMONSTER      = 32;
  ACTIVATE_CUSTOM         = 255;

  KEY_RED      = 1;
  KEY_GREEN    = 2;
  KEY_BLUE     = 4;
  KEY_REDTEAM  = 8;
  KEY_BLUETEAM = 16;

  TEXTURE_NAME_WATER = '_water_0';
  TEXTURE_NAME_ACID1 = '_water_1';
  TEXTURE_NAME_ACID2 = '_water_2';

  TEXTURE_SPECIAL_WATER = DWORD(-1);
  TEXTURE_SPECIAL_ACID1 = DWORD(-2);
  TEXTURE_SPECIAL_ACID2 = DWORD(-3);
  TEXTURE_NONE = DWORD(-4);

Type
  TPoint = packed record
    X, Y: LongInt;
  end;

  TTriggerData = record
    case Byte of
      0: (Default: Byte128);
      TRIGGER_EXIT:         (MapName: Char16);
      TRIGGER_TELEPORT:     (TargetPoint: TPoint;
                             d2d_teleport: Boolean;
                             silent_teleport: Boolean;
                             TlpDir: Byte);
      TRIGGER_OPENDOOR,
      TRIGGER_CLOSEDOOR,
      TRIGGER_DOOR,
      TRIGGER_DOOR5,
      TRIGGER_CLOSETRAP,
      TRIGGER_TRAP,
      TRIGGER_LIFTUP,
      TRIGGER_LIFTDOWN,
      TRIGGER_LIFT:         (PanelID: Integer;
                             NoSound: Boolean;
                             d2d_doors: Boolean);
      TRIGGER_PRESS,
      TRIGGER_ON,
      TRIGGER_OFF,
      TRIGGER_ONOFF:        (tX, tY: Integer;
                             tWidth, tHeight: Word;
                             Wait: Word;
                             Count: Word;
                             MonsterID: Integer;
                             ExtRandom: Boolean);
      TRIGGER_SECRET:       ();
      TRIGGER_TEXTURE:      (ActivateOnce: Boolean;
                             AnimOnce: Boolean);
      TRIGGER_SOUND:        (SoundName: Char64;
                             Volume: Byte;
                             Pan: Byte;
                             Local: Boolean;
                             PlayCount: Byte;
                             SoundSwitch: Boolean);
      TRIGGER_SPAWNMONSTER: (MonPos: TPoint;
                             MonType: Byte;
                             MonHealth: Integer;
                             MonDir: Byte;
                             MonActive: Boolean;
                             MonCount: Integer;
                             MonEffect: Byte;
                             MonMax: Word;
                             MonDelay: Word;
                             MonBehav: Byte);
      TRIGGER_SPAWNITEM:    (ItemPos: TPoint;
                             ItemType: Byte;
                             ItemFalls: Boolean;
                             ItemOnlyDM: Boolean;
                             ItemCount: Integer;
                             ItemEffect: Byte;
                             ItemMax: Word;
                             ItemDelay: Word);
      TRIGGER_MUSIC:        (MusicName: Char64;
                             MusicAction: Byte);
      TRIGGER_PUSH:         (PushAngle: Word;
                             PushForce: Byte;
                             ResetVel: Boolean);
      TRIGGER_SCORE:        (ScoreAction: Byte;
                             ScoreCount: Byte;
                             ScoreTeam: Byte;
                             ScoreCon,
                             ScoreMsg: Boolean);
      TRIGGER_MESSAGE:      (MessageKind: Byte;
                             MessageSendTo: Byte;
                             MessageText: Char100;
                             MessageTime: Word);
      TRIGGER_DAMAGE:       (DamageValue: Word;
                             DamageInterval: Word;
                             DamageKind: Byte);
      TRIGGER_HEALTH:       (HealValue: Word;
                             HealInterval: Word;
                             HealMax: Boolean;
                             HealSilent: Boolean);
      TRIGGER_SHOT:         (ShotPos: TPoint;
                             ShotType: Byte;
                             ShotTarget: Byte;
                             ShotSound: Boolean;
                             ShotAim: Byte;
                             ShotPanelID: Integer;
                             ShotIntSight: Word;
                             ShotAngle: Word;
                             ShotWait: Word;
                             ShotAccuracy: Word;
                             ShotAmmo: Word;
                             ShotIntReload: Word);
      TRIGGER_EFFECT:       (FXCount: Byte;
                             FXType: Byte;
                             FXSubType: Byte;
                             FXColorR: Byte;
                             FXColorG: Byte;
                             FXColorB: Byte;
                             FXPos: Byte;
                             FXWait: Word;
                             FXVelX: ShortInt;
                             FXVelY: ShortInt;
                             FXSpreadL: Byte;
                             FXSpreadR: Byte;
                             FXSpreadU: Byte;
                             FXSpreadD: Byte);
  end;

implementation

end.
