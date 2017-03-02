Unit g_language;

{$MODE Delphi}

Interface

Uses
  g_Basic, MAPDEF;

Type
  TStrings_Locale = (
    I_NOT_ACCESSIBLE,

    I_ARRAY_BOOL_FALSE,
    I_ARRAY_BOOL_TRUE,

    I_ARRAY_DIR_LEFT,
    I_ARRAY_DIR_RIGHT,
    I_ARRAY_DIR_SAME,
    I_ARRAY_DIR_REVERSED,

    I_ARRAY_DIRBTN_LEFT,
    I_ARRAY_DIRBTN_RIGHT,
    I_ARRAY_DIRBTN_UP,
    I_ARRAY_DIRBTN_DOWN,

    I_ARRAY_PANEL_WALL,
    I_ARRAY_PANEL_BACK,
    I_ARRAY_PANEL_FRONT,
    I_ARRAY_PANEL_DOOR_OPEN,
    I_ARRAY_PANEL_DOOR_CLOSE,
    I_ARRAY_PANEL_STAIR,
    I_ARRAY_PANEL_WATER,
    I_ARRAY_PANEL_ACID_1,
    I_ARRAY_PANEL_ACID_2,
    I_ARRAY_PANEL_LIFT_UP,
    I_ARRAY_PANEL_LIFT_DOWN,
    I_ARRAY_PANEL_LIFT_LEFT,
    I_ARRAY_PANEL_LIFT_RIGHT,
    I_ARRAY_PANEL_BLOCKMON,

    I_ARRAY_FX_NONE,
    I_ARRAY_FX_TELEPORT,
    I_ARRAY_FX_RESPAWN,
    I_ARRAY_FX_FIRE,

    I_ARRAY_ITEM_MEDKIT,
    I_ARRAY_ITEM_LARGE_MEDKIT,
    I_ARRAY_ITEM_BLACK_MEDKIT,
    I_ARRAY_ITEM_GREEN_ARMOR,
    I_ARRAY_ITEM_BLUE_ARMOR,
    I_ARRAY_ITEM_BLUE_SPHERE,
    I_ARRAY_ITEM_MEGASPHERE,
    I_ARRAY_ITEM_HAZ_SUIT,
    I_ARRAY_ITEM_OXYGEN,
    I_ARRAY_ITEM_INVULNERABILITY,
    I_ARRAY_ITEM_CHAINSAW,
    I_ARRAY_ITEM_SHOTGUN,
    I_ARRAY_ITEM_DB_SHOTGUN,
    I_ARRAY_ITEM_CHAINGUN,
    I_ARRAY_ITEM_ROCKET_LAUNCHER,
    I_ARRAY_ITEM_PLASMA_RIFLE,
    I_ARRAY_ITEM_BFG,
    I_ARRAY_ITEM_SUPER_MINIGUN,
    I_ARRAY_ITEM_CLIP,
    I_ARRAY_ITEM_AMMO_BOX,
    I_ARRAY_ITEM_4_SHELLS,
    I_ARRAY_ITEM_25_SHELLS,
    I_ARRAY_ITEM_1_ROCKET,
    I_ARRAY_ITEM_ROCKET_BOX,
    I_ARRAY_ITEM_CELL,
    I_ARRAY_ITEM_LARGE_CELL,
    I_ARRAY_ITEM_BACKPACK,
    I_ARRAY_ITEM_KEY_RED,
    I_ARRAY_ITEM_KEY_GREEN,
    I_ARRAY_ITEM_KEY_BLUE,
    I_ARRAY_ITEM_BOTTLE,
    I_ARRAY_ITEM_HELMET,
    I_ARRAY_ITEM_JETPACK,
    I_ARRAY_ITEM_INVIS,

    I_ARRAY_SHOT_PISTOL,
    I_ARRAY_SHOT_BULLET,
    I_ARRAY_SHOT_SHOTGUN,
    I_ARRAY_SHOT_SSG,
    I_ARRAY_SHOT_IMP,
    I_ARRAY_SHOT_PLASMA,
    I_ARRAY_SHOT_SPIDER,
    I_ARRAY_SHOT_CACO,
    I_ARRAY_SHOT_BARON,
    I_ARRAY_SHOT_MANCUB,
    I_ARRAY_SHOT_REV,
    I_ARRAY_SHOT_ROCKET,
    I_ARRAY_SHOT_BFG,
    I_ARRAY_SHOT_EXPL,
    I_ARRAY_SHOT_BFGEXPL,

    I_ARRAY_MON_DEMON,
    I_ARRAY_MON_IMP,
    I_ARRAY_MON_ZOMBIE,
    I_ARRAY_MON_SERGEANT,
    I_ARRAY_MON_CYBER,
    I_ARRAY_MON_CGUN,
    I_ARRAY_MON_HELL_BARON,
    I_ARRAY_MON_HELL_KNIGHT,
    I_ARRAY_MON_CACODEMON,
    I_ARRAY_MON_LOST_SOUL,
    I_ARRAY_MON_PAIN_ELEMENTAL,
    I_ARRAY_MON_MASTERMIND,
    I_ARRAY_MON_ARACHNATRON,
    I_ARRAY_MON_MANCUBUS,
    I_ARRAY_MON_REVENANT,
    I_ARRAY_MON_ARCHVILE,
    I_ARRAY_MON_FISH,
    I_ARRAY_MON_BARREL,
    I_ARRAY_MON_ROBOT,
    I_ARRAY_MON_PRIKOLIST,

    I_ARRAY_AREA_PLAYER_ONE,
    I_ARRAY_AREA_PLAYER_TWO,
    I_ARRAY_AREA_DM,
    I_ARRAY_AREA_FLAG_RED,
    I_ARRAY_AREA_FLAG_BLUE,
    I_ARRAY_AREA_FLAG_DOM,
    I_ARRAY_AREA_TEAM_RED,
    I_ARRAY_AREA_TEAM_BLUE,

    I_ARRAY_TR_EXIT,
    I_ARRAY_TR_TELEPORT,
    I_ARRAY_TR_DOOR_OPEN,
    I_ARRAY_TR_DOOR_CLOSE,
    I_ARRAY_TR_DOOR_SWITCH,
    I_ARRAY_TR_DOOR_5_SEC,
    I_ARRAY_TR_TRAP_CLOSE,
    I_ARRAY_TR_TRAP,
    I_ARRAY_TR_EXTEND,
    I_ARRAY_TR_SECRET,
    I_ARRAY_TR_LIFT_UP,
    I_ARRAY_TR_LIFT_DOWN,
    I_ARRAY_TR_LIFT_SWITCH,
    I_ARRAY_TR_TEXTURE,
    I_ARRAY_TR_ON,
    I_ARRAY_TR_OFF,
    I_ARRAY_TR_SWITCH,
    I_ARRAY_TR_SOUND,
    I_ARRAY_TR_SPAWN_MONSTER,
    I_ARRAY_TR_SPAWN_ITEM,
    I_ARRAY_TR_MUSIC,
    I_ARRAY_TR_PUSH,
    I_ARRAY_TR_SCORE,
    I_ARRAY_TR_MESSAGE,
    I_ARRAY_TR_DAMAGE,
    I_ARRAY_TR_HEALTH,
    I_ARRAY_TR_SHOT,
    I_ARRAY_TR_EFFECT,

    I_PROP_ID,
    I_PROP_X,
    I_PROP_Y,
    I_PROP_WIDTH,
    I_PROP_HEIGHT,
    I_PROP_PANEL_TYPE,
    I_PROP_PANEL_TEX,
    I_PROP_PANEL_ALPHA,
    I_PROP_PANEL_BLEND,
    I_PROP_DM_ONLY,
    I_PROP_ITEM_FALLS,
    I_PROP_DIRECTION,

    I_PROP_TR_TYPE,
    I_PROP_TR_ENABLED,
    I_PROP_TR_TEXTURE_PANEL,
    I_PROP_TR_ACTIVATION,
    I_PROP_TR_KEYS,
    I_PROP_TR_D2D,
    I_PROP_TR_SILENT,
    I_PROP_TR_COUNT,
    I_PROP_TR_INTERVAL,
    I_PROP_TR_HEALTH,
    I_PROP_TR_NEXT_MAP,
    I_PROP_TR_TELEPORT_TO,
    I_PROP_TR_TELEPORT_SILENT,
    I_PROP_TR_TELEPORT_DIR,
    I_PROP_TR_DOOR_PANEL,
    I_PROP_TR_TRAP_PANEL,
    I_PROP_TR_EX_AREA,
    I_PROP_TR_EX_DELAY,
    I_PROP_TR_EX_COUNT,
    I_PROP_TR_EX_MONSTER,
    I_PROP_TR_EX_RANDOM,
    I_PROP_TR_LIFT_PANEL,
    I_PROP_TR_TEXTURE_ONCE,
    I_PROP_TR_TEXTURE_ANIM_ONCE,
    I_PROP_TR_SOUND_NAME,
    I_PROP_TR_SOUND_VOLUME,
    I_PROP_TR_SOUND_PAN,
    I_PROP_TR_SOUND_COUNT,
    I_PROP_TR_SOUND_LOCAL,
    I_PROP_TR_SOUND_SWITCH,
    I_PROP_TR_FX_TYPE,
    I_PROP_TR_SPAWN_TO,
    I_PROP_TR_SPAWN_MAX,
    I_PROP_TR_SPAWN_DELAY,
    I_PROP_TR_MONSTER_TYPE,
    I_PROP_TR_MONSTER_ACTIVE,
    I_PROP_TR_MONSTER_BEHAVIOUR,
    I_PROP_TR_MONSTER_BEHAVIOUR_0,
    I_PROP_TR_MONSTER_BEHAVIOUR_1,
    I_PROP_TR_MONSTER_BEHAVIOUR_2,
    I_PROP_TR_MONSTER_BEHAVIOUR_3,
    I_PROP_TR_MONSTER_BEHAVIOUR_4,
    I_PROP_TR_MONSTER_BEHAVIOUR_5,
    I_PROP_TR_ITEM_TYPE,
    I_PROP_TR_MUSIC_NAME,
    I_PROP_TR_MUSIC_ACT,
    I_PROP_TR_MUSIC_ON,
    I_PROP_TR_MUSIC_OFF,
    I_PROP_TR_PUSH_ANGLE,
    I_PROP_TR_PUSH_FORCE,
    I_PROP_TR_PUSH_RESET,
    I_PROP_TR_SCORE_ACT,
    I_PROP_TR_SCORE_ACT_0,
    I_PROP_TR_SCORE_ACT_1,
    I_PROP_TR_SCORE_ACT_2,
    I_PROP_TR_SCORE_ACT_3,
    I_PROP_TR_SCORE_TEAM,
    I_PROP_TR_SCORE_TEAM_0,
    I_PROP_TR_SCORE_TEAM_1,
    I_PROP_TR_SCORE_TEAM_2,
    I_PROP_TR_SCORE_TEAM_3,
    I_PROP_TR_SCORE_CON,
    I_PROP_TR_SCORE_MSG,
    I_PROP_TR_DAMAGE_VALUE,
    I_PROP_TR_HEALTH_MAX,
    I_PROP_TR_SHOT_TYPE,
    I_PROP_TR_SHOT_SOUND,
    I_PROP_TR_SHOT_ANGLE,
    I_PROP_TR_SHOT_ACC,
    I_PROP_TR_SHOT_TO,
    I_PROP_TR_SHOT_TO_0,
    I_PROP_TR_SHOT_TO_1,
    I_PROP_TR_SHOT_TO_2,
    I_PROP_TR_SHOT_TO_3,
    I_PROP_TR_SHOT_TO_4,
    I_PROP_TR_SHOT_TO_5,
    I_PROP_TR_SHOT_TO_6,
    I_PROP_TR_SHOT_AIM,
    I_PROP_TR_SHOT_AIM_0,
    I_PROP_TR_SHOT_AIM_1,
    I_PROP_TR_SHOT_AIM_2,
    I_PROP_TR_SHOT_AIM_3,
    I_PROP_TR_SHOT_AMMO,
    I_PROP_TR_SHOT_RELOAD,
    I_PROP_TR_SHOT_SIGHT,
    I_PROP_TR_SHOT_PANEL,
    I_PROP_TR_MESSAGE_KIND,
    I_PROP_TR_MESSAGE_KIND_0,
    I_PROP_TR_MESSAGE_KIND_1,
    I_PROP_TR_MESSAGE_TO,
    I_PROP_TR_MESSAGE_TO_0,
    I_PROP_TR_MESSAGE_TO_1,
    I_PROP_TR_MESSAGE_TO_2,
    I_PROP_TR_MESSAGE_TO_3,
    I_PROP_TR_MESSAGE_TO_4,
    I_PROP_TR_MESSAGE_TO_5,
    I_PROP_TR_MESSAGE_TEXT,
    I_PROP_TR_MESSAGE_TIME,
    I_PROP_TR_EFFECT_TYPE,
    I_PROP_TR_EFFECT_SUBTYPE,
    I_PROP_TR_EFFECT_COLOR,
    I_PROP_TR_EFFECT_CENTER,
    I_PROP_TR_EFFECT_VELX,
    I_PROP_TR_EFFECT_VELY,
    I_PROP_TR_EFFECT_SPL,
    I_PROP_TR_EFFECT_SPR,
    I_PROP_TR_EFFECT_SPU,
    I_PROP_TR_EFFECT_SPD,
    I_PROP_TR_EFFECT_PARTICLE,
    I_PROP_TR_EFFECT_ANIMATION,
    I_PROP_TR_EFFECT_SLIQUID,
    I_PROP_TR_EFFECT_LLIQUID,
    I_PROP_TR_EFFECT_DLIQUID,
    I_PROP_TR_EFFECT_BLOOD,
    I_PROP_TR_EFFECT_SPARK,
    I_PROP_TR_EFFECT_BUBBLE,

    I_MSG_ERROR,
    I_MSG_WRONG_TEXWIDTH,
    I_MSG_WRONG_TEXHEIGHT,
    I_MSG_WRONG_ALPHA,
    I_MSG_WRONG_SIZE,
    I_MSG_WRONG_XY,
    I_MSG_TEXTURE_ALREADY,
    I_MSG_RES_NAME_64,
    I_MSG_CHOOSE_ITEM,
    I_MSG_CHOOSE_MONSTER,
    I_MSG_CHOOSE_AREA,
    I_MSG_CHOOSE_TEXTURE,
    I_MSG_CHOOSE_RES,
    I_MSG_EXIT,
    I_MSG_EXIT_PROMT,
    I_MSG_DEL_TEXTURE,
    I_MSG_DEL_TEXTURE_PROMT,
    I_MSG_DEL_TEXTURE_CANT,
    I_MSG_CLEAR_MAP,
    I_MSG_CLEAR_MAP_PROMT,
    I_MSG_DELETE_MAP,
    I_MSG_DELETE_MAP_PROMT,
    I_MSG_MAP_DELETED,
    I_MSG_MAP_DELETED_PROMT,
    I_MSG_EXEC_ERROR,
    I_MSG_SOUND_ERROR,
    I_MSG_WAD_ERROR,
    I_MSG_RES_ERROR,
    I_MSG_PACKED,
    I_MSG_MAP_EXISTS,
    I_MSG_SAVE_MAP,

    I_HINT_TELEPORT,
    I_HINT_SPAWN,
    I_HINT_PANEL_DOOR,
    I_HINT_PANEL_TEXTURE,
    I_HINT_PANEL_SHOT,
    I_HINT_PANEL_LIFT,
    I_HINT_MONSTER,
    I_HINT_EXT_AREA,
    I_HINT_WIDTH,
    I_HINT_HEIGHT,

    I_MENU_FILE,
    I_MENU_FILE_NEW,
    I_MENU_FILE_OPEN,
    I_MENU_FILE_SAVE,
    I_MENU_FILE_SAVE_AS,
    I_MENU_FILE_OPEN_WAD,
    I_MENU_FILE_SAVE_MINI,
    I_MENU_FILE_DELETE,
    I_MENU_FILE_PACK_MAP,
    I_MENU_FILE_EXIT,

    I_MENU_EDIT,
    I_MENU_EDIT_UNDO,
    I_MENU_EDIT_COPY,
    I_MENU_EDIT_CUT,
    I_MENU_EDIT_PASTE,
    I_MENU_EDIT_SELECT_ALL,
    I_MENU_EDIT_TO_FORE,
    I_MENU_EDIT_TO_BACK,

    I_MENU_TOOLS,
    I_MENU_TOOLS_SNAP_GRID,
    I_MENU_TOOLS_MINIMAP,
    I_MENU_TOOLS_STEP_GRID,
    I_MENU_TOOLS_SHOW_EDGES,
    I_MENU_TOOLS_LAYERS,

    I_MENU_SERV,
    I_MENU_SERV_TEST,
    I_MENU_SERV_OPT,
    I_MENU_SERV_PREVIEW,
    I_MENU_SERV_LAUNCH,

    I_MENU_SETS,
    I_MENU_SETS_MAP,
    I_MENU_SETS_EDITOR,
    I_MENU_SETS_LAUNCH,

    I_MENU_HELP,
    I_MENU_HELP_ABOUT,

    I_MENU_LAYER_BACK,
    I_MENU_LAYER_WALL,
    I_MENU_LAYER_FORE,
    I_MENU_LAYER_STAIR,
    I_MENU_LAYER_WATER,
    I_MENU_LAYER_ITEM,
    I_MENU_LAYER_MONSTER,
    I_MENU_LAYER_AREA,
    I_MENU_LAYER_TRIGGER,

    I_MENU_TB_NEW,
    I_MENU_TB_OPEN,
    I_MENU_TB_SAVE,
    I_MENU_TB_OPEN_WAD,
    I_MENU_TB_MINIMAP,
    I_MENU_TB_LAYERS,
    I_MENU_TB_GRID,
    I_MENU_TB_GRID_STEP,
    I_MENU_TB_LAUNCH,

    I_MENU_LAUNCH_SETS,

    I_CAP_FX_TYPE,
    I_CAP_MONSTER_TYPE,
    I_CAP_ITEM_TYPE,
    I_CAP_ABOUT,
    I_CAP_ACT,
    I_CAP_ADD_SKY,
    I_CAP_ADD_SOUND,
    I_CAP_ADD_TEXTURE,
    I_CAP_KEYS,
    I_CAP_TEST,
    I_CAP_OPT,
    I_CAP_SETS,
    I_CAP_LAUNCH,
    I_CAP_ES,
    I_CAP_PACK,
    I_CAP_SAVE,
    I_CAP_MINI,
    I_CAP_SELECT,
    
    I_CTRL_PROP_KEY,
    I_CTRL_PROP_VALUE,
    I_CTRL_PANELS,
    I_CTRL_ITEMS,
    I_CTRL_MONSTERS,
    I_CTRL_AREAS,
    I_CTRL_TRIGGERS,

    I_CTRL_LIST_TEXTURE,
    I_CTRL_LIST_ITEM,
    I_CTRL_LIST_MONSTER,
    I_CTRL_LIST_AREA,
    I_CTRL_LIST_TRIGGER,
    I_CTRL_LIST_ACTIVE,
    I_CTRL_LIST_KEYS,

    I_CTRL_PREVIEW,
    I_CTRL_ITEM_DM,
    I_CTRL_ITEM_FALLS,
    I_CTRL_LEFT,
    I_CTRL_RIGHT,

    I_CTRL_ACT_PLAYER_CLOSE,
    I_CTRL_ACT_MONSTER_CLOSE,
    I_CTRL_ACT_PLAYER_PRESS,
    I_CTRL_ACT_MONSTER_PRESS,
    I_CTRL_ACT_SHOT,
    I_CTRL_ACT_NO_MONSTER,

    I_CTRL_KEYS_RED,
    I_CTRL_KEYS_GREEN,
    I_CTRL_KEYS_BLUE,
    I_CTRL_KEYS_TEAM_RED,
    I_CTRL_KEYS_TEAM_BLUE,
    
    I_CTRL_OPT_GB,
    I_CTRL_OPT_TEXTURE,
    I_CTRL_OPT_PANEL,
    I_CTRL_OPT_WALL,
    I_CTRL_OPT_FORE,
    I_CTRL_OPT_BACK,
    I_CTRL_OPT_STAIR,
    I_CTRL_OPT_WATER,
    I_CTRL_OPT_ACID1,
    I_CTRL_OPT_ACID2,
    I_CTRL_OPT_LIFT,
    I_CTRL_OPT_BLOCKMON,
    I_CTRL_OPT_DESC_TEXTURE,
    I_CTRL_OPT_DESC_PANEL,

    I_CTRL_SETS_STATS,
    I_CTRL_SETS_SIZES,

    I_CTRL_LAUNCH_DM,
    I_CTRL_LAUNCH_TDM,
    I_CTRL_LAUNCH_CTF,
    I_CTRL_LAUNCH_COOP,
    I_CTRL_LAUNCH_2P,
    I_CTRL_LAUNCH_FF,
    I_CTRL_LAUNCH_EXIT,
    I_CTRL_LAUNCH_WEAPONS,
    I_CTRL_LAUNCH_MONSTERS,
    I_CTRL_LAUNCH_CLOSE,
    I_CTRL_LAUNCH_OPEN,

    I_CTRL_ES_GRID,
    I_CTRL_ES_TEXTURE,
    I_CTRL_ES_PANEL_SIZE,

    I_CTRL_PACK_TEXTURES,
    I_CTRL_PACK_SKY,
    I_CTRL_PACK_MUSIC,
    I_CTRL_PACK_ADD,
    I_CTRL_PACK_NON_STD,

    I_LAB_TEX_WIDTH,
    I_LAB_TEX_HEIGHT,

    I_LAB_SPECTRUM,

    I_LAB_ABOUT_VER,
    I_LAB_ABOUT_AUTHOR,
    I_LAB_ABOUT_AUTHOR_2,
    I_LAB_ABOUT_MAIL,
    I_LAB_ABOUT_MAIL_2,
    I_LAB_ABOUT_SITE,
    I_LAB_ABOUT_HTTP,

    I_LAB_ADD_WADS,
    I_LAB_ADD_SECTIONS,

    I_LAB_SETS_NAME,
    I_LAB_SETS_DESC,
    I_LAB_SETS_AUTHOR,
    I_LAB_SETS_BACK,
    I_LAB_SETS_MUSIC,
    I_LAB_SETS_TEXTURES,
    I_LAB_SETS_PANELS,
    I_LAB_SETS_ITEMS,
    I_LAB_SETS_MONSTERS,
    I_LAB_SETS_AREAS,
    I_LAB_SETS_TRIGGERS,
    I_LAB_SETS_SIZES,
    I_LAB_SETS_WIDTH,
    I_LAB_SETS_HEIGHT,
    I_LAB_SETS_MAPMOVE,
    I_LAB_SETS_SNAPPING,

    I_LAB_LAUNCH_TIME,
    I_LAB_LAUNCH_SECS,
    I_LAB_LAUNCH_SCORE,
    I_LAB_LAUNCH_PATH,

    I_LAB_ES_GRID,
    I_LAB_ES_GRID_COLOR,
    I_LAB_ES_BACK,
    I_LAB_ES_PREVIEW,
    I_LAB_ES_MINIMAP,
    I_LAB_ES_RECENT,
    I_LAB_ES_LANGUAGE,

    I_LAB_PACK_SAVE_TO,
    I_LAB_PACK_MAP_NAME,
    I_LAB_PACK_TEXTURES,
    I_LAB_PACK_SKY,
    I_LAB_PACK_MUSIC,

    I_LAB_MINI_SCALE,
    
    I_BTN_APPLY_PROPS,
    I_BTN_OK,
    I_BTN_CANCEL,
    I_BTN_ADD,
    I_BTN_CLOSE,
    I_BTN_ADD_CLOSE,
    I_BTN_TEST_AGAIN,
    I_BTN_START,
    I_BTN_PACK,
    I_BTN_SAVE,
    I_BTN_NO_SOUND,
    I_BTN_TEXTURE_ADD,
    I_BTN_TEXTURE_DELETE,
    I_BTN_TEXTURE_EMPTY,

    I_LOAD_WAD,
    I_LOAD_MAP,
    I_LOAD_TEXTURES,
    I_LOAD_PANELS,
    I_LOAD_ITEMS,
    I_LOAD_MONSTERS,
    I_LOAD_AREAS,
    I_LOAD_TRIGGERS,

    I_TEST_AREA_WALL,
    I_TEST_AREA_WALL_STR,
    I_TEST_SPAWNS_1,
    I_TEST_SPAWNS_2,
    I_TEST_SPAWNS,
    I_TEST_NO_DM,
    I_TEST_NO_DM_EX,
    I_TEST_MONSTER_WALL,
    I_TEST_MONSTER_WALL_STR,

    I_OPT_NO_TEXTURES,
    I_OPT_DELETED_TEXTURES,
    I_OPT_TOTAL_TEXTURES,
    I_OPT_TEX_DELETED,
    I_OPT_PANELS_OPT,
    I_OPT_WALLS,
    I_OPT_FORES,
    I_OPT_BACKS,
    I_OPT_STAIRS,
    I_OPT_WATER,
    I_OPT_ACID1,
    I_OPT_ACID2,
    I_OPT_LIFTS,
    I_OPT_BLOCKMON,
    I_OPT_TOTAL_PANELS,
    I_OPT_PANELS_AFTER,

    I_WAD_SPECIAL_MAP,
    I_WAD_SPECIAL_TEXS,

    I_FILE_FILTER_ALL,
    I_FILE_FILTER_WAD,

    I_EDITOR_TITLE,
    
    I_LAST );

Const
  LANGUAGE_RUSSIAN = 'Russian';
  LANGUAGE_ENGLISH = 'English';
  LANGUAGE_RUSSIAN_N = 3;
  LANGUAGE_ENGLISH_N = 2;

Var
  _lc: Array [TStrings_Locale] of String;  // for the GUI (in UTF-8)
  _glc: Array [TStrings_Locale] of String; // for GL text (in CP1251)

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


procedure g_Language_Load(fileName: String);
procedure g_Language_Set(lang: String);
procedure g_Language_Dump(fileName: String);

Implementation

Uses
  SysUtils, e_log, f_main, f_about, f_activationtype,
  f_addresource_sky, f_addresource_sound,
  f_addresource_texture, f_choosetype, f_keys, f_mapcheck,
  f_mapoptions, f_maptest, f_mapoptimization, f_options,
  f_packmap, f_savemap, f_saveminimap, f_selectmap, Forms, utils;

Const
  g_lang_default: Array [TStrings_Locale] of Array [1..3] of String = (
    ('NOT ACCESSIBLE',                 'N/A',
                                       'Н/Д'),

    ('ARRAY BOOL FALSE',               'No',
                                       'Нет'),
    ('ARRAY BOOL TRUE',                'Yes',
                                       'Да'),

    ('ARRAY DIR LEFT',                 'Left',
                                       'Влево'),
    ('ARRAY DIR RIGHT',                'Right',
                                       'Вправо'),
    ('ARRAY DIR SAME',                 'Don''t change',
                                       'Не менять'),
    ('ARRAY DIR REVERSED',             'Reversed',
                                       'Обратное'),

    ('ARRAY DIRBTN LEFT',              'Left',
                                       'Влево'),
    ('ARRAY DIRBTN RIGHT',             'Right',
                                       'Вправо'),
    ('ARRAY DIRBTN UP',                'Up',
                                       'Вверх'),
    ('ARRAY DIRBTN DOWN',              'Down',
                                       'Вниз'),

    ('ARRAY PANEL WALL',               'Wall',
                                       'Стена'),
    ('ARRAY PANEL BACK',               'Background',
                                       'Фон'),
    ('ARRAY PANEL FRONT',              'Foreground',
                                       'Передний план'),
    ('ARRAY PANEL DOOR OPEN',          'Open Door',
                                       'Открытая дверь'),
    ('ARRAY PANEL DOOR CLOSE',         'Closed Door',
                                       'Закрытая дверь'),
    ('ARRAY PANEL STAIR',              'Step',
                                       'Ступень'),
    ('ARRAY PANEL WATER',              'Water',
                                       'Вода'),
    ('ARRAY PANEL ACID 1',             'Acid 1',
                                       'Кислота 1'),
    ('ARRAY PANEL ACID 2',             'Acid 2',
                                       'Кислота 2'),
    ('ARRAY PANEL LIFT UP',            'Stream Up',
                                       'Лифт вверх'),
    ('ARRAY PANEL LIFT DOWN',          'Stream Down',
                                       'Лифт вниз'),
    ('ARRAY PANEL LIFT LEFT',          'Stream Left',
                                       'Поток влево'),
    ('ARRAY PANEL LIFT RIGHT',         'Stream Right',
                                       'Поток вправо'),
    ('ARRAY PANEL BLOCKMON',           'Monster Boundary',
                                       'Блокиратор монстров'),

    ('ARRAY FX NONE',                  'None',
                                       'Нет'),
    ('ARRAY FX TELEPORT',              'Teleport',
                                       'Телепортация'),
    ('ARRAY FX RESPAWN',               'Respawn',
                                       'Респаун'),
    ('ARRAY FX FIRE',                  'Arch-Vile Fire',
                                       'Огонь колдуна'),

    ('ARRAY ITEM MEDKIT',              'Stimpack',
                                       'Аптечка'),
    ('ARRAY ITEM LARGE MEDKIT',        'Medikit',
                                       'Большая аптечка'),
    ('ARRAY ITEM BLACK MEDKIT',        'Berserk Pack',
                                       'Черная аптечка'),
    ('ARRAY ITEM GREEN ARMOR',         'Green Armor',
                                       'Зелёная броня'),
    ('ARRAY ITEM BLUE ARMOR',          'Blue Armor',
                                       'Синяя броня'),
    ('ARRAY ITEM BLUE SPHERE',         'Soulsphere',
                                       'Шарик 100%'),
    ('ARRAY ITEM MEGASPHERE',          'Megasphere',
                                       'Мегасфера'),
    ('ARRAY ITEM HAZ SUIT',            'Envirosuit',
                                       'Защитный костюм'),
    ('ARRAY ITEM OXYGEN',              'Scuba',
                                       'Акваланг'),
    ('ARRAY ITEM INVULNERABILITY',     'Invulnerability',
                                       'Неуязвимость'),
    ('ARRAY ITEM CHAINSAW',            'Chainsaw',
                                       'Бензопила'),
    ('ARRAY ITEM SHOTGUN',             'Shotgun',
                                       'Ружьё'),
    ('ARRAY ITEM DB SHOTGUN',          'Super Shotgun',
                                       'Двустволка'),
    ('ARRAY ITEM CHAINGUN',            'Chaingun',
                                       'Пулемёт'),
    ('ARRAY ITEM ROCKET LAUNCHER',     'Rocket Launcher',
                                       'Ракетница'),
    ('ARRAY ITEM PLASMA RIFLE',        'Plasma Rifle',
                                       'Плазмаган'),
    ('ARRAY ITEM BFG',                 'BFG9000',
                                       'BFG9000'),
    ('ARRAY ITEM SUPER MINIGUN',       'Super Chaingun',
                                       'Суперпулемёт'),
    ('ARRAY ITEM CLIP',                'Clip',
                                       'Патроны'),
    ('ARRAY ITEM AMMO BOX',            'Box of Bullets',
                                       'Ящик патронов'),
    ('ARRAY ITEM 4 SHELLS',            '4 Shells',
                                       '4 гильзы'),
    ('ARRAY ITEM 25 SHELLS',           'Box of Shells',
                                       '25 гильз'),
    ('ARRAY ITEM 1 ROCKET',            'Rocket',
                                       '1 ракета'),
    ('ARRAY ITEM ROCKET BOX',          'Box of Rockets',
                                       '5 ракет'),
    ('ARRAY ITEM CELL',                'Energy Cell',
                                       'Батарейка'),
    ('ARRAY ITEM LARGE CELL',          'Energy Cell Pack',
                                       'Батарея'),
    ('ARRAY ITEM BACKPACK',            'Backpack',
                                       'Рюкзак'),
    ('ARRAY ITEM KEY RED',             'Red Key',
                                       'Красный ключ'),
    ('ARRAY ITEM KEY GREEN',           'Green Key',
                                       'Зеленый ключ'),
    ('ARRAY ITEM KEY BLUE',            'Blue Key',
                                       'Синий ключ'),
    ('ARRAY ITEM BOTTLE',              'Health Globe',
                                       'Живая вода'),
    ('ARRAY ITEM HELMET',              'Armor Shard',
                                       'Бронешлем'),
    ('ARRAY ITEM JETPACK',             'Jetpack',
                                       'Реактивный ранец'),
    ('ARRAY ITEM INVIS',               'Invisibility',
                                       'Невидимость'),

    ('ARRAY SHOT PISTOL',              'Pistol shot',
                                       'Выстрел пистолета'),
    ('ARRAY SHOT BULLET',              'Chaingun shot',
                                       'Выстрел пулемета'),
    ('ARRAY SHOT SHOTGUN',             'Shotgun shot',
                                       'Выстрел ружья'),
    ('ARRAY SHOT SSG',                 'Super Shotgun shot',
                                       'Выстрел двустволки'),
    ('ARRAY SHOT IMP',                 'Imp fireball',
                                       'Шар беса'),
    ('ARRAY SHOT PLASMA',              'Blue plasma',
                                       'Синяя плазма'),
    ('ARRAY SHOT SPIDER',              'Arachnotron plasma',
                                       'Плазма арахнотрона'),
    ('ARRAY SHOT CACO',                'Cacodemon fireball',
                                       'Шар какодемона'),
    ('ARRAY SHOT BARON',               'Hell Baron projectile',
                                       'Выстрел барона ада'),
    ('ARRAY SHOT MANCUB',              'Mancubus gunshot',
                                       'Снаряд манкубуса'),
    ('ARRAY SHOT REV',                 'Revenant projectile',
                                       'Снаряд скелета'),
    ('ARRAY SHOT ROCKET',              'Rocket',
                                       'Ракета'),
    ('ARRAY SHOT BFG',                 'BFG ball',
                                       'Шар BFG'),
    ('ARRAY SHOT EXPL',                'Explosion',
                                       'Взрыв'),
    ('ARRAY SHOT BFGEXPL',             'BFG explosion',
                                       'Взрыв BFG'),

    ('ARRAY MON DEMON',                'Pinky',
                                       'Демон'),
    ('ARRAY MON IMP',                  'Imp',
                                       'Бес'),
    ('ARRAY MON ZOMBIE',               'Zombie',
                                       'Зомби'),
    ('ARRAY MON SERGEANT',             'Sergeant',
                                       'Сержант'),
    ('ARRAY MON CYBER',                'Cyberdemon',
                                       'Кибердемон'),
    ('ARRAY MON CGUN',                 'Commando',
                                       'Пулемётчик'),
    ('ARRAY MON HELL BARON',           'Hell Baron',
                                       'Барон ада'),
    ('ARRAY MON HELL KNIGHT',          'Hell Knight',
                                       'Рыцарь ада'),
    ('ARRAY MON CACODEMON',            'Cacodemon',
                                       'Какодемон'),
    ('ARRAY MON LOST SOUL',            'Lost Soul',
                                       'Огненный череп'),
    ('ARRAY MON PAIN ELEMENTAL',       'Pain Elemental',
                                       'Авиабаза'),
    ('ARRAY MON MASTERMIND',           'Spider Mastermind',
                                       'Большой паук'),
    ('ARRAY MON ARACHNATRON',          'Arachnotron',
                                       'Арахнотрон'),
    ('ARRAY MON MANCUBUS',             'Mancubus',
                                       'Манкубус'),
    ('ARRAY MON REVENANT',             'Revenant',
                                       'Скелет'),
    ('ARRAY MON ARCHVILE',             'Arch-Vile',
                                       'Колдун'),
    ('ARRAY MON FISH',                 'Piranha',
                                       'Рыба'),
    ('ARRAY MON BARREL',               'Barrel',
                                       'Бочка'),
    ('ARRAY MON ROBOT',                'Robot',
                                       'Робот'),
    ('ARRAY MON PRIKOLIST',            'Prikolist',
                                       'Приколист'),

    ('ARRAY AREA PLAYER ONE',          'Player 1',
                                       'Первый игрок'),
    ('ARRAY AREA PLAYER TWO',          'Player 2',
                                       'Второй игрок'),
    ('ARRAY AREA DM',                  'DM Spawn Point',
                                       'Точка DM'),
    ('ARRAY AREA FLAG RED',            'Red Flag',
                                       'Красный флаг'),
    ('ARRAY AREA FLAG BLUE',           'Blue Flag',
                                       'Синий флаг'),
    ('ARRAY AREA FLAG DOM',            'Domination Flag',
                                       'DOM флаг'),
    ('ARRAY AREA TEAM RED',            'Red Team',
                                       'Красная команда'),
    ('ARRAY AREA TEAM BLUE',           'Blue Team',
                                       'Синяя команда'),

    ('ARRAY TR EXIT',                  'Exit',
                                       'Выход'),
    ('ARRAY TR TELEPORT',              'Teleport',
                                       'Телепортация'),
    ('ARRAY TR DOOR OPEN',             'Open Door',
                                       'Открыть дверь'),
    ('ARRAY TR DOOR CLOSE',            'Close Door',
                                       'Закрыть дверь'),
    ('ARRAY TR DOOR SWITCH',           'Door',
                                       'Дверь'),
    ('ARRAY TR DOOR 5 SEC',            'Door (5 sec)',
                                       'Дверь (5 секунд)'),
    ('ARRAY TR TRAP CLOSE',            'Close Trap',
                                       'Закрыть ловушку'),
    ('ARRAY TR TRAP',                  'Trap',
                                       'Ловушка'),
    ('ARRAY TR EXTEND',                'Extender',
                                       'Расширитель'),
    ('ARRAY TR SECRET',                'Secret',
                                       'Секрет'),
    ('ARRAY TR LIFT UP',               'Turn stream up/left',
                                       'Лифт вверх/поток влево'),
    ('ARRAY TR LIFT DOWN',             'Turn stream down/right',
                                       'Лифт вниз/поток вправо'),
    ('ARRAY TR LIFT SWITCH',           'Revert stream',
                                       'Лифт'),
    ('ARRAY TR TEXTURE',               'Change Texture',
                                       'Смена текстуры'),
    ('ARRAY TR ON',                    'Enable Trigger',
                                       'Включить триггер'),
    ('ARRAY TR OFF',                   'Disable Trigger',
                                       'Выключить триггер'),
    ('ARRAY TR SWITCH',                'Trigger Toggle',
                                       'Переключить триггер'),
    ('ARRAY TR SOUND',                 'Play Sound',
                                       'Звук'),
    ('ARRAY TR SPAWN MONSTER',         'Spawn Monster',
                                       'Создать монстра'),
    ('ARRAY TR SPAWN ITEM',            'Spawn Item',
                                       'Создать предмет'),
    ('ARRAY TR MUSIC',                 'Play Music',
                                       'Музыка'),
    ('ARRAY TR PUSH',                  'Push',
                                       'Ускорение'),
    ('ARRAY TR SCORE',                 'Team Score',
                                       'Начисление очков'),
    ('ARRAY TR MESSAGE',               'Message',
                                       'Сообщение'),
    ('ARRAY TR DAMAGE',                'Damage',
                                       'Урон'),
    ('ARRAY TR HEALTH',                'Healer',
                                       'Оздоровитель'),
    ('ARRAY TR SHOT',                  'Turret',
                                       'Турель'),
    ('ARRAY TR EFFECT',                'Effect',
                                       'Эффект'),

    ('PROP ID',                        'ID',
                                       'ID'),
    ('PROP X',                         'X',
                                       'X'),
    ('PROP Y',                         'Y',
                                       'Y'),
    ('PROP WIDTH',                     'Width',
                                       'Ширина'),
    ('PROP HEIGHT',                    'Height',
                                       'Высота'),
    ('PROP PANEL TYPE',                'Panel Type',
                                       'Тип панели'),
    ('PROP PANEL TEX',                 'Texture',
                                       'Текстура'),
    ('PROP PANEL ALPHA',               'Transparency',
                                       'Прозрачность'),
    ('PROP PANEL BLEND',               'Blending',
                                       'Смешивание'),
    ('PROP DM ONLY',                   'DM Only',
                                       'Только в DM'),
    ('PROP ITEM FALLS',                'Falls',
                                       'Падает'),
    ('PROP DIRECTION',                 'Direction',
                                       'Направление'),

    ('PROP TR TYPE',                   'Trigger Type',
                                       'Тип триггера'),
    ('PROP TR ENABLED',                'Enabled',
                                       'Включен'),
    ('PROP TR TEXTURE PANEL',          'Textured Panel',
                                       'Панель с текстурой'),
    ('PROP TR ACTIVATION',             'Activation Type',
                                       'Тип активации'),
    ('PROP TR KEYS',                   'Keys',
                                       'Ключи'),
    ('PROP TR D2D',                    'D2D-like',
                                       'Как в D2D'),
    ('PROP TR SILENT',                 'Silent',
                                       'Без звука'),
    ('PROP TR COUNT',                  'Count',
                                       'Количество'),
    ('PROP TR INTERVAL',               'Interval (in ticks)',
                                       'Интервал (в тиках)'),
    ('PROP TR HEALTH',                 'Health',
                                       'Здоровье'),
    ('PROP TR NEXT MAP',               'Next Map',
                                       'След. карта'),
    ('PROP TR TELEPORT TO',            'Teleport to',
                                       'Точка телепорта'),
    ('PROP TR TELEPORT SILENT',        'Silent',
                                       'Тихий телепорт'),
    ('PROP TR TELEPORT DIR',           'New Direction',
                                       'Направление после'),
    ('PROP TR DOOR PANEL',             'Door Panel',
                                       'Панель двери'),
    ('PROP TR TRAP PANEL',             'Trap Panel',
                                       'Панель ловушки'),
    ('PROP TR EX AREA',                'Action Area',
                                       'Область воздействия'),
    ('PROP TR EX DELAY',               'Delay',
                                       'Задержка'),
    ('PROP TR EX COUNT',               'Count',
                                       'Счётчик'),
    ('PROP TR EX MONSTER',             'Monster ID',
                                       'ID монстра'),
    ('PROP TR EX RANDOM',              'Random',
                                       'Случайный'),
    ('PROP TR LIFT PANEL',             'Stream Panel',
                                       'Панель лифта'),
    ('PROP TR TEXTURE ONCE',           'Once',
                                       'Один раз'),
    ('PROP TR TEXTURE ANIM ONCE',      'Animate Once',
                                       'Аним. один раз'),
    ('PROP TR SOUND NAME',             'Sound                              (snd)',
                                       'Звук                               (snd)'),
    ('PROP TR SOUND VOLUME',           'Volume',
                                       'Громкость'),
    ('PROP TR SOUND PAN',              'Pan',
                                       'Стерео'),
    ('PROP TR SOUND COUNT',            'Loops',
                                       'Играть раз'),
    ('PROP TR SOUND LOCAL',            'Local',
                                       'Локальный'),
    ('PROP TR SOUND SWITCH',           'Switch',
                                       'Переключение'),
    ('PROP TR FX TYPE',                'Effect',
                                       'Эффект'),
    ('PROP TR SPAWN TO',               'Spawn at',
                                       'Точка появления'),
    ('PROP TR SPAWN MAX',              'Maximum',
                                       'Максимум'),
    ('PROP TR SPAWN DELAY',            'Autospawn (in ticks)',
                                       'Автоспаун (в тиках)'),
    ('PROP TR MONSTER TYPE',           'Monster',
                                       'Тип монстра'),
    ('PROP TR MONSTER ACTIVE',         'Active',
                                       'Активен'),
    ('PROP TR MONSTER BEHAVIOUR',      'Behaviour',
                                       'Поведение'),
    ('PROP TR MONSTER BEHAVIOUR 0',    'Normal',
                                       'Обычное'),
    ('PROP TR MONSTER BEHAVIOUR 1',    'Killer',
                                       'Убийца'),
    ('PROP TR MONSTER BEHAVIOUR 2',    'Maniac',
                                       'Маньяк'),
    ('PROP TR MONSTER BEHAVIOUR 3',    'Insane',
                                       'Поехавший'),
    ('PROP TR MONSTER BEHAVIOUR 4',    'Cannibal',
                                       'Каннибал'),
    ('PROP TR MONSTER BEHAVIOUR 5',    'Good',
                                       'Добрый'),
    ('PROP TR ITEM TYPE',              'Item',
                                       'Тип предмета'),
    ('PROP TR MUSIC NAME',             'Music',
                                       'Музыка'),
    ('PROP TR MUSIC ACT',              'Action                             (m)',
                                       'Действие                           (m)'),
    ('PROP TR MUSIC ON',               'Play',
                                       'Включить'),
    ('PROP TR MUSIC OFF',              'Pause',
                                       'Выключить'),
    ('PROP TR PUSH ANGLE',             'Angle',
                                       'Угол'),
    ('PROP TR PUSH FORCE',             'Force',
                                       'Сила'),
    ('PROP TR PUSH RESET',             'Reset velocity',
                                       'Сбрасывать скорость'),
    ('PROP TR SCORE ACT',              'Action                             (s)',
                                       'Действие                           (s)'),
    ('PROP TR SCORE ACT 0',            'Add points',
                                       'Прибавить очки'),
    ('PROP TR SCORE ACT 1',            'Subtract points',
                                       'Отнять очки'),
    ('PROP TR SCORE ACT 2',            'Team Wins',
                                       'Выигрыш'),
    ('PROP TR SCORE ACT 3',            'Team Loses',
                                       'Проигрыш'),
    ('PROP TR SCORE TEAM',             'Team',
                                       'Команда'),
    ('PROP TR SCORE TEAM 0',           'My',
                                       'Своя'),
    ('PROP TR SCORE TEAM 1',           'Enemy',
                                       'Соперника'),
    ('PROP TR SCORE TEAM 2',           'Red',
                                       'Красная'),
    ('PROP TR SCORE TEAM 3',           'Blue',
                                       'Синяя'),
    ('PROP TR SCORE CON',              'Console message',
                                       'Сообщение в консоль'),
    ('PROP TR SCORE MSG',              'Game message',
                                       'Сообщение в игру'),
    ('PROP TR DAMAGE VALUE',           'Damage',
                                       'Ущерб'),
    ('PROP TR HEALTH MAX',             'To maximum',
                                       'До максимума'),
    ('PROP TR SHOT TYPE',              'Projectile',
                                       'Снаряд'),
    ('PROP TR SHOT SOUND',             'Shot sound',
                                       'Звук выстрела'),
    ('PROP TR SHOT ANGLE',             'Angle',
                                       'Угол'),
    ('PROP TR SHOT ACC',               'Spread',
                                       'Разброс'),
    ('PROP TR SHOT TO',                'Auto targeting',
                                       'Автонаведение'),
    ('PROP TR SHOT TO 0',              'None',
                                       'Нет'),
    ('PROP TR SHOT TO 1',              'Monsters',
                                       'Монстры'),
    ('PROP TR SHOT TO 2',              'Players',
                                       'Игроки'),
    ('PROP TR SHOT TO 3',              'Red team',
                                       'Красная команда'),
    ('PROP TR SHOT TO 4',              'Blue team',
                                       'Синяя команда'),
    ('PROP TR SHOT TO 5',              'Monsters, players',
                                       'Монстры, игроки'),
    ('PROP TR SHOT TO 6',              'Players, monsters',
                                       'Игроки, монстры'),
    ('PROP TR SHOT AIM',               'Auto-aiming mode',
                                       'Режим автонаведения'),
    ('PROP TR SHOT AIM 0',             'Trigger area',
                                       'Область триггера'),
    ('PROP TR SHOT AIM 1',             'Entire map',
                                       'По всей карте'),
    ('PROP TR SHOT AIM 2',             'Trace trigger area',
                                       'Область триггера с трассировкой'),
    ('PROP TR SHOT AIM 3',             'Trace entire map',
                                       'Вся карта с трассировкой'),
    ('PROP TR SHOT AMMO',              'Ammo limit',
                                       'Кол-во патронов'),
    ('PROP TR SHOT RELOAD',            'Reload interval (in ticks)',
                                       'Перезарядка (в тиках)'),
    ('PROP TR SHOT SIGHT',             'Sight interval (in ticks)',
                                       'Реакция (в тиках)'),
    ('PROP TR SHOT PANEL',             'Indicator panel',
                                       'Панель индикации'),
    ('PROP TR MESSAGE KIND',           'Message kind',
                                       'Тип сообщения'),
    ('PROP TR MESSAGE KIND 0',         'Console message',
                                       'Текст в консоли'),
    ('PROP TR MESSAGE KIND 1',         'Event message',
                                       'Крупный текст по центру'),
    ('PROP TR MESSAGE TO',             'Send to',
                                       'Получатель'),
    ('PROP TR MESSAGE TO 0',           'Me',
                                       'Я'),
    ('PROP TR MESSAGE TO 1',           'My team',
                                       'Моя команда'),
    ('PROP TR MESSAGE TO 2',           'Enemy team',
                                       'Вражеская команда'),
    ('PROP TR MESSAGE TO 3',           'Red team',
                                       'Красная команда'),
    ('PROP TR MESSAGE TO 4',           'Blue team',
                                       'Синяя команда'),
    ('PROP TR MESSAGE TO 5',           'Everyone',
                                       'Все игроки'),
    ('PROP TR MESSAGE TEXT',           'Message text',
                                       'Текст сообщения'),
    ('PROP TR MESSAGE TIME',           'Time (in ticks)',
                                       'Время показа (в тиках)'),
    ('PROP TR EFFECT TYPE',            'Effect type',
                                       'Тип эффекта'),
    ('PROP TR EFFECT SUBTYPE',         'Effect subtype',
                                       'Подтип эффекта'),
    ('PROP TR EFFECT COLOR',           'Effect color',
                                       'Цвет эффекта'),
    ('PROP TR EFFECT CENTER',          'Trigger center',
                                       'По центру'),
    ('PROP TR EFFECT VELX',            'Horizontal speed',
                                       'Скорость по горизонтали'),
    ('PROP TR EFFECT VELY',            'Vertical speed',
                                       'Скорость по вертикали'),
    ('PROP TR EFFECT SPL',             'Speed variance left',
                                       'Отклонение влево'),
    ('PROP TR EFFECT SPR',             'Speed variance right',
                                       'Отклонение вправо'),
    ('PROP TR EFFECT SPU',             'Speed variance up',
                                       'Отклонение вверх'),
    ('PROP TR EFFECT SPD',             'Speed variance down',
                                       'Отклонение вниз'),
    ('PROP TR EFFECT PARTICLE',        'Particle generator',
                                       'Генератор частиц'),
    ('PROP TR EFFECT ANIMATION',       'Animation',
                                       'Анимация'),
    ('PROP TR EFFECT SLIQUID',         'Water splash',
                                       'Брызги воды'),
    ('PROP TR EFFECT LLIQUID',         'Light colored splash',
                                       'Цветные брызги (светлее)'),
    ('PROP TR EFFECT DLIQUID',         'Dark colored splash',
                                       'Цветные брызги (темнее)'),
    ('PROP TR EFFECT BLOOD',           'Blood',
                                       'Кровь'),
    ('PROP TR EFFECT SPARK',           'Sparks',
                                       'Искры'),
    ('PROP TR EFFECT BUBBLE',          'Bubbles',
                                       'Пузырьки'),

    ('MSG ERROR',                      'Error',
                                       'Ошибка'),
    ('MSG WRONG TEXWIDTH',             'Panel Width must be a multiple of Texture Width (%d)',
                                       'Ширина панели должна быть кратна ширине текстуры (%d)'),
    ('MSG WRONG TEXHEIGHT',            'Panel Height must be a multiple of Texture Height (%d)',
                                       'Высота панели должна быть кратна высоте текстуры (%d)'),
    ('MSG WRONG ALPHA',                'Transparency must be in [0..255] interval',
                                       'Прозрачность должна быть в интервале [0..255]'),
    ('MSG WRONG SIZE',                 'Width and Height must be greater than 0',
                                       'Ширина и высота должны быть больше 0'),
    ('MSG WRONG XY',                   'X or Y coordinate hasn''t been set',
                                       'Не задана X или Y координата'),
    ('MSG TEXTURE ALREADY',            'Texture "%s" already exists',
                                       'Текстура "%s" уже добавлена'),
    ('MSG RES NAME 64',                'Resource "%s" name must not be longer than 64 chars',
                                       'Имя ресурса "%s" должно быть не длиннее 64 символов'),
    ('MSG CHOOSE ITEM',                'Select Item',
                                       'Выберите предмет'),
    ('MSG CHOOSE MONSTER',             'Select Monster',
                                       'Выберите монстра'),
    ('MSG CHOOSE AREA',                'Select Area',
                                       'Выберите область'),
    ('MSG CHOOSE TEXTURE',             'Select Texture',
                                       'Выберите текстуру'),
    ('MSG CHOOSE RES',                 'Resource hasn''t been selected',
                                       'Не выбран ресурс'),
    ('MSG EXIT',                       'Exit',
                                       'Выход'),
    ('MSG EXIT PROMT',                 'Leaving so soon?',
                                       'Уже уходите?'),
    ('MSG DEL TEXTURE',                'Delete the texture',
                                       'Удалить текстуру'),
    ('MSG DEL TEXTURE PROMT',          'Delete the texture "%s" ?',
                                       'Удалить текстуру "%s" ?'),
    ('MSG DEL TEXTURE CANT',           'Can''t delete texture in use. Replace it on all panels with this texture.',
                                       'Нельзя удалить используемую текстуру. Замените её на каждой панели с ней.'),
    ('MSG CLEAR MAP',                  'New map',
                                       'Новая карта'),
    ('MSG CLEAR MAP PROMT',            'Clear the entire map?',
                                       'Очистить всю карту?'),
    ('MSG DELETE MAP',                 'Delete the map',
                                       'Удалить карту'),
    ('MSG DELETE MAP PROMT',           'Delete the map "%s" from "%s" ?',
                                       'Удалить карту "%s" из "%s" ?'),
    ('MSG MAP DELETED',                'Map is deleted',
                                       'Карта удалена'),
    ('MSG MAP DELETED PROMT',          'Map "%s" is deleted',
                                       'Карта "%s" удалена'),
    ('MSG EXEC ERROR',                 'Game execution error',
                                       'Ошибка запуска игры'),
    ('MSG SOUND ERROR',                'Can''t play sound',
                                       'Не удалось проиграть звук'),
    ('MSG WAD ERROR',                  'Can''t open WAD: %s',
                                       'Ошибка при открытии WAD: %s'),
    ('MSG RES ERROR',                  'Can''t read resource: %s:%s\%s',
                                       'Ошибка при чтении ресурса: %s:%s\%s'),
    ('MSG PACKED',                     'Map "%s" with resources saved to "%s"',
                                       'Карта "%s" вместе с ресурсами сохранена в "%s"'),
    ('MSG MAP EXISTS',                 'Map "%s" already exists. Overwrite?',
                                       'Карта "%s" уже существует. Заменить?'),
    ('MSG SAVE MAP',                   'Save the map',
                                       'Сохранить карту'),

    ('HINT TELEPORT',                  'Choose destination of Teleport',
                                       'Выберите точку телепорта'),
    ('HINT SPAWN',                     'Choose Spawn point',
                                       'Выберите точку появления'),
    ('HINT PANEL DOOR',                'Choose Door',
                                       'Выберите панель двери'),
    ('HINT PANEL TEXTURE',             'Choose textured Panel',
                                       'Выберите панель с текстурой'),
    ('HINT PANEL SHOT',                'Choose textured shot indicator Panel',
                                       'Выберите панель индикации выстрела с текстурой'),
    ('HINT PANEL LIFT',                'Choose Stream Panel',
                                       'Выберите панель лифта'),
    ('HINT MONSTER',                   'Choose Monster',
                                       'Выберите монстра'),
    ('HINT EXT AREA',                  'Specify action Area',
                                       'Укажите область воздействия'),
    ('HINT WIDTH',                     'Width: %d',
                                       'Ширина: %d'),
    ('HINT HEIGHT',                    'Height: %d',
                                       'Высота: %d'),

    ('MENU FILE',                      'File',
                                       'Файл'),
    ('MENU FILE NEW',                  'New Map',
                                       'Новая карта'),
    ('MENU FILE OPEN',                 'Open Map',
                                       'Открыть карту'),
    ('MENU FILE SAVE',                 'Save Map',
                                       'Сохранить карту'),
    ('MENU FILE SAVE AS',              'Save Map As...',
                                       'Сохранить карту как...'),
    ('MENU FILE OPEN WAD',             'Select Map',
                                       'Выбрать карту'),
    ('MENU FILE SAVE MINI',            'Save Mini-map',
                                       'Сохранить мини-карту'),
    ('MENU FILE DELETE',               'Delete Map from WAD...',
                                       'Удалить карту из WAD...'),
    ('MENU FILE PACK MAP',             'Pack Map',
                                       'Упаковать карту'),
    ('MENU FILE EXIT',                 'Exit',
                                       'Выход'),

    ('MENU EDIT',                      'Edit',
                                       'Правка'),
    ('MENU EDIT UNDO',                 'Undo',
                                       'Отменить'),
    ('MENU EDIT COPY',                 'Copy',
                                       'Копировать'),
    ('MENU EDIT CUT',                  'Cut',
                                       'Вырезать'),
    ('MENU EDIT PASTE',                'Paste',
                                       'Вставить'),
    ('MENU EDIT SELECT ALL',           'Select All',
                                       'Выделить всё'),
    ('MENU EDIT TO FORE',              'Bring to Front',
                                       'Передвинуть вперед'),
    ('MENU EDIT TO BACK',              'Send to Back',
                                       'Передвинуть назад'),

    ('MENU TOOLS',                     'Tools',
                                       'Инструменты'),
    ('MENU TOOLS SNAP GRID',           'Snap to Grid',
                                       'Привязка к сетке'),
    ('MENU TOOLS MINIMAP',             'Show Mini-map',
                                       'Мини-карта'),
    ('MENU TOOLS STEP GRID',           'Switch Grid step',
                                       'Сменить шаг сетки'),
    ('MENU TOOLS SHOW EDGES',          'Switch Map edges',
                                       'Границы карты'),
    ('MENU TOOLS LAYERS',              'Layers',
                                       'Слои'),

    ('MENU SERV',                      'Service',
                                       'Сервис'),
    ('MENU SERV TEST',                 'Check the Map',
                                       'Проверка карты'),
    ('MENU SERV OPT',                  'Optimize the Map',
                                       'Оптимизация карты'),
    ('MENU SERV PREVIEW',              'Map Preview',
                                       'Предварительный просмотр'),
    ('MENU SERV LAUNCH',               'In-game test',
                                       'Тест карты в игре'),

    ('MENU SETS',                      'Settings',
                                       'Настройка'),
    ('MENU SETS MAP',                  'Map parameters',
                                       'Параметры карты'),
    ('MENU SETS EDITOR',               'Editor settings',
                                       'Настройки редактора'),
    ('MENU SETS LAUNCH',               'In-game test settings',
                                       'Настройки теста в игре'),

    ('MENU HELP',                      'Help',
                                       'Справка'),
    ('MENU HELP ABOUT',                'About',
                                       'О программе'),

    ('MENU LAYER BACK',                '1. Background',
                                       '1. Фон'),
    ('MENU LAYER WALL',                '2. Walls',
                                       '2. Стены'),
    ('MENU LAYER FORE',                '3. Foreground',
                                       '3. Передний план'),
    ('MENU LAYER STAIR',               '4. Steps',
                                       '4. Ступени'),
    ('MENU LAYER WATER',               '5. Liquids',
                                       '5. Жидкости'),
    ('MENU LAYER ITEM',                '6. Items',
                                       '6. Предметы'),
    ('MENU LAYER MONSTER',             '7. Monsters',
                                       '7. Монстры'),
    ('MENU LAYER AREA',                '8. Areas',
                                       '8. Области'),
    ('MENU LAYER TRIGGER',             '9. Triggers',
                                       '9. Триггеры'),

    ('MENU TB NEW',                    'New Map',
                                       'Новая карта'),
    ('MENU TB OPEN',                   'Open Map',
                                       'Открыть карту'),
    ('MENU TB SAVE',                   'Save Map',
                                       'Сохранить карту'),
    ('MENU TB OPEN WAD',               'Open another Map from same WAD',
                                       'Открыть другую карту из этого же WAD''а'),
    ('MENU TB MINIMAP',                'Show Mini-map',
                                       'Показать мини-карту'),
    ('MENU TB LAYERS',                 'Show/Hide Objects',
                                       'Отрисовка панелей/объектов'),
    ('MENU TB GRID',                   'Grid On/Off',
                                       'Включить/Отключить отображение сетки'),
    ('MENU TB GRID STEP',              'Switch Grid step',
                                       'Изменить шаг сетки'),
    ('MENU TB LAUNCH',                 'In-game test',
                                       'Тест карты в игре'),

    ('MENU LAUNCH SETS',               'Settings...',
                                       'Параметры...'),

    ('CAP FX TYPE',                    'Select Effect type',
                                       'Выберите тип эффекта'),
    ('CAP MONSTER TYPE',               'Select Monster type',
                                       'Выберите тип монстра'),
    ('CAP ITEM TYPE',                  'Select Item type',
                                       'Выберите тип предмета'),
    ('CAP ABOUT',                      'About Map Editor',
                                       'О программе'),
    ('CAP ACT',                        'Activation type',
                                       'Тип активации'),
    ('CAP ADD SKY',                    'Select Sky Texture',
                                       'Выберите текстуру неба'),
    ('CAP ADD SOUND',                  'Select Sound/Music',
                                       'Выберите звук/музыку'),
    ('CAP ADD TEXTURE',                'Select Texture',
                                       'Выберите текстуру'),
    ('CAP KEYS',                       'Keys',
                                       'Ключи'),
    ('CAP TEST',                       'Map Checking',
                                       'Проверка карты'),
    ('CAP OPT',                        'Map Optimization',
                                       'Оптимизация карты'),
    ('CAP SETS',                       'Map Parameters',
                                       'Параметры карты'),
    ('CAP LAUNCH',                     'In-game test Settings',
                                       'Настройки теста в игре'),
    ('CAP ES',                         'Editor Settings',
                                       'Настройки редактора'),
    ('CAP PACK',                       'Pack Map',
                                       'Упаковать карту'),
    ('CAP SAVE',                       'Save Map',
                                       'Сохранить карту'),
    ('CAP MINI',                       'Save Mini-map',
                                       'Сохранить мини-карту'),
    ('CAP SELECT',                     'Select Map',
                                       'Выберите карту'),

    ('CTRL PROP KEY',                  'Property',
                                       'Свойство'),
    ('CTRL PROP VALUE',                'Value',
                                       'Значение'),

    ('CTRL PANELS',                    'Panels',
                                       'Панели'),
    ('CTRL ITEMS',                     'Items',
                                       'Предметы'),
    ('CTRL MONSTERS',                  'Monsters',
                                       'Монстры'),
    ('CTRL AREAS',                     'Areas',
                                       'Области'),
    ('CTRL TRIGGERS',                  'Triggers',
                                       'Триггеры'),

    ('CTRL LIST TEXTURE',              'Texture List',
                                       'Список текстур'),
    ('CTRL LIST ITEM',                 'Item List',
                                       'Список предметов'),
    ('CTRL LIST MONSTER',              'Monster List',
                                       'Список монстров'),
    ('CTRL LIST AREA',                 'Area List',
                                       'Список областей'),
    ('CTRL LIST TRIGGER',              'Trigger List',
                                       'Список триггеров'),
    ('CTRL LIST ACTIVE',               'Activation Type',
                                       'Тип активации триггера'),
    ('CTRL LIST KEYS',                 'Keys Needed',
                                       'Ключи для активации'),

    ('CTRL PREVIEW',                   'Texture Preview',
                                       'Предварительный просмотр'),
    ('CTRL ITEM DM',                   'DM Only',
                                       'Только в DM'),
    ('CTRL ITEM FALLS',                'Falls',
                                       'Падает'),
    ('CTRL LEFT',                      'Left',
                                       'Влево'),
    ('CTRL RIGHT',                     'Right',
                                       'Вправо'),

    ('CTRL ACT PLAYER CLOSE',          'Player Collides',
                                       'Игрок близко'),
    ('CTRL ACT MONSTER CLOSE',         'Monster Collides',
                                       'Монстр близко'),
    ('CTRL ACT PLAYER PRESS',          'Player Pressed',
                                       'Игрок нажал'),
    ('CTRL ACT MONSTER PRESS',         'Monster Pressed',
                                       'Монстр нажал'),
    ('CTRL ACT SHOT',                  'Shot',
                                       'Выстрел'),
    ('CTRL ACT NO MONSTER',            'No Monsters',
                                       'Монстров нет'),

    ('CTRL KEYS RED',                  'Red Key',
                                       'Красный ключ'),
    ('CTRL KEYS GREEN',                'Green Key',
                                       'Зеленый ключ'),
    ('CTRL KEYS BLUE',                 'Blue Key',
                                       'Синий ключ'),
    ('CTRL KEYS TEAM RED',             'Red Team',
                                       'Красная команда'),
    ('CTRL KEYS TEAM BLUE',            'Blue Team',
                                       'Синяя команда'),

    ('CTRL OPT GB',                    'Optimization',
                                       'Оптимизация'),
    ('CTRL OPT TEXTURE',               'Texture Optimization',
                                       'Оптимизация текстур'),
    ('CTRL OPT PANEL',                 'Panel Optimization',
                                       'Оптимизация панелей'),
    ('CTRL OPT WALL',                  'Walls',
                                       'Стены'),
    ('CTRL OPT FORE',                  'Foreground',
                                       'Передний план'),
    ('CTRL OPT BACK',                  'Background',
                                       'Фон'),
    ('CTRL OPT STAIR',                 'Steps',
                                       'Ступени'),
    ('CTRL OPT WATER',                 'Water',
                                       'Вода'),
    ('CTRL OPT ACID1',                 'Acid 1',
                                       'Кислота 1'),
    ('CTRL OPT ACID2',                 'Acid 2',
                                       'Кислота 2'),
    ('CTRL OPT LIFT',                  'Streams',
                                       'Лифты'),
    ('CTRL OPT BLOCKMON',              'Monster Boundary',
                                       'Блокиратор монстров'),
    ('CTRL OPT DESC TEXTURE',          'Deletes unused textures from texture list.',
                                       'Удаляет неиспользуемые текстуры из списка.'),
    ('CTRL OPT DESC PANEL',            'Merges nearby panels of same type. Increases game performance.',
                                       'Объединяет одинаковые панели, расположенные вплотную друг к другу, увеличивая тем самым производительность игры.'),
    ('CTRL SETS STATS',                'Statistics:',
                                       'Статистика:'),
    ('CTRL SETS SIZES',                'Map Size:',
                                       'Размеры:'),

    ('CTRL LAUNCH DM',                 'Deathmatch',
                                       'Deathmatch'),
    ('CTRL LAUNCH TDM',                'Team Deathmatch',
                                       'Team Deathmatch'),
    ('CTRL LAUNCH CTF',                'Capture the Flag',
                                       'Capture the Flag'),
    ('CTRL LAUNCH COOP',               'Cooperative',
                                       'Cooperative'),
    ('CTRL LAUNCH 2P',                 'Two Players',
                                       'Два игрока'),
    ('CTRL LAUNCH FF',                 'Friendly Fire',
                                       'Урон своих'),
    ('CTRL LAUNCH EXIT',               'Enable Exit',
                                       'Включить выход'),
    ('CTRL LAUNCH WEAPONS',            'Weapons Stay',
                                       'Оружие остаётся'),
    ('CTRL LAUNCH MONSTERS',           'Enable Monsters',
                                       'Монстры в DM'),
    ('CTRL LAUNCH CLOSE',              'Close the game after exiting the map',
                                       'Закрыть игру после выхода из карты'),
    ('CTRL LAUNCH OPEN',               'Select Doom 2D: Forever .exe',
                                       'Выберите файл игры Doom 2D: Forever'),

    ('CTRL ES GRID',                   'Show Grid',
                                       'Показывать сетку'),
    ('CTRL ES TEXTURE',                'Show Panel Texture',
                                       'Показывать текстуру панели'),
    ('CTRL ES PANEL SIZE',             'Show Panel Size',
                                       'Показывать размеры панели '),

    ('CTRL PACK TEXTURES',             'Textures',
                                       'Текстуры'),
    ('CTRL PACK SKY',                  'Sky',
                                       'Небо'),
    ('CTRL PACK MUSIC',                'Music',
                                       'Музыку'),
    ('CTRL PACK ADD',                  'Don''t overwrite WAD',
                                       'Не перезаписывать WAD'),
    ('CTRL PACK NON STD',              'Non-standard Resources only',
                                       'Только нестандартные ресурсы'),

    ('LAB TEX WIDTH',                  'Texture Width:',
                                       'Ширина текстуры:'),
    ('LAB TEX HEIGHT',                 'Texture Height:',
                                       'Высота текстуры:'),

    ('LAB SPECTRUM',                   'Show the Spectrum',
                                       'Показать спектр'),

    ('LAB ABOUT VER',                  'Version 2.1.5',
                                       'Версия 2.1.5'),
    ('LAB ABOUT AUTHOR',               'Author: rs.falcon',
                                       'Автор: rs.falcon'),
    ('LAB ABOUT AUTHOR 2',             'Additions: Pss',
                                       'Дорабатывал: Pss'),
    ('LAB ABOUT MAIL',                 'rmw.falcon@mail.ru',
                                       'rmw.falcon@mail.ru'),
    ('LAB ABOUT MAIL 2',               'pssxx@mail.ru',
                                       'pssxx@mail.ru'),
    ('LAB ABOUT SITE',                 'Doom 2D: Forever Web-site',
                                       'Сайт Doom 2D: Forever'),
    ('LAB ABOUT HTTP',                 'www.doom2d.org',
                                       'www.doom2d.org'),

    ('LAB ADD WADS',                   'WAD Files:',
                                       'Список WAD-файлов:'),
    ('LAB ADD SECTIONS',               'WAD Sections:',
                                       'Список секций WAD-файла:'),

    ('LAB SETS NAME',                  'Map Name:',
                                       'Название карты:'),
    ('LAB SETS DESC',                  'Map Description',
                                       'Описание карты:'),
    ('LAB SETS AUTHOR',                'Author:',
                                       'Автор:'),
    ('LAB SETS BACK',                  'Background/Sky:',
                                       'Фон/Небо:'),
    ('LAB SETS MUSIC',                 'Music:',
                                       'Музыка:'),
    ('LAB SETS TEXTURES',              'Textures:',
                                       'Текстур:'),
    ('LAB SETS PANELS',                'Panels:',
                                       'Панелей:'),
    ('LAB SETS ITEMS',                 'Items:',
                                       'Предметов:'),
    ('LAB SETS MONSTERS',              'Monsters:',
                                       'Монстров:'),
    ('LAB SETS AREAS',                 'Areas:',
                                       'Областей:'),
    ('LAB SETS TRIGGERS',              'Triggers:',
                                       'Триггеров:'),
    ('LAB SETS SIZES',                 'Current:',
                                       'Текущий:'),
    ('LAB SETS WIDTH',                 'Width:',
                                       'Ширина:'),
    ('LAB SETS HEIGHT',                'Height:',
                                       'Высота:'),
    ('LAB SETS MAPMOVE',               'Direction of shifting map objects:',
                                       'Направление смещения объектов карты:'),
    ('LAB SETS SNAPPING',              'Preserve snapping',
                                       'Сохранять привязку'),

    ('LAB LAUNCH TIME',                'Time Limit:',
                                       'Лимит времени:'),
    ('LAB LAUNCH SECS',                'seconds',
                                       'секунд'),
    ('LAB LAUNCH SCORE',               'Score Limit:',
                                       'Лимит очков:'),
    ('LAB LAUNCH PATH',                'Path to Doom2DF.exe:',
                                       'Путь к Doom2DF.exe:'),

    ('LAB ES GRID',                    'Grid Step:',
                                       'Шаг сетки:'),
    ('LAB ES GRID COLOR',              'Grid Color:',
                                       'Цвет сетки:'),
    ('LAB ES BACK',                    'Background Color:',
                                       'Цвет фона:'),
    ('LAB ES PREVIEW',                 'Texture Preview Background Color:',
                                       'Цвет фона поля предпросмотра текстуры:'),
    ('LAB ES MINIMAP',                 'Mini-map Scale:',
                                       'Масштаб мини-карты:'),
    ('LAB ES RECENT',                  'Recent Maps List Contains:',
                                       'Запоминать последних открытых карт:'),
    ('LAB ES LANGUAGE',                'Language:',
                                       'Язык:'),

    ('LAB PACK SAVE TO',               'Save to:',
                                       'Сохранить в:'),
    ('LAB PACK MAP NAME',              'Map Resource Name:',
                                       'Название ресурса карты:'),
    ('LAB PACK TEXTURES',              'Texture Section:',
                                       'Секция для текстур:'),
    ('LAB PACK SKY',                   'Sky Section:',
                                       'Секция для неба:'),
    ('LAB PACK MUSIC',                 'Music Section:',
                                       'Секция для музыки:'),

    ('LAB MINI SCALE',                 'Scale:',
                                       'Масштаб:'),

    ('BTN APPLY PROPS',                'Apply Properties',
                                       'Применить свойства'),
    ('BTN OK',                         'OK',
                                       'OK'),
    ('BTN CANCEL',                     'Cancel',
                                       'Отмена'),
    ('BTN ADD',                        'Add',
                                       'Добавить'),
    ('BTN CLOSE',                      'Close',
                                       'Закрыть'),
    ('BTN ADD CLOSE',                  'Add and Close',
                                       'Добавить и закрыть'),
    ('BTN TEST AGAIN',                 'Check',
                                       'Проверить'),
    ('BTN START',                      'Run',
                                       'Начать'),
    ('BTN PACK',                       'Pack',
                                       'Упаковать'),
    ('BTN SAVE',                       'Save',
                                       'Сохранить'),
    ('BTN NO SOUND',                   'No sound',
                                       'Нет звука'),
    ('BTN TEXTURE ADD',                'Add texture to the list',
                                       'Добавить текстуру в список'),
    ('BTN TEXTURE DELETE',             'Delete texture from the list',
                                       'Удалить текстуру из списка'),
    ('BTN TEXTURE EMPTY',              'Deselect texture',
                                       'Снять выбор с текстуры'),

    ('LOAD WAD',                       'Reading WAD',
                                       'Чтение WAD'),
    ('LOAD MAP',                       'Loading Map',
                                       'Чтение карты'),
    ('LOAD TEXTURES',                  'Reading Textures',
                                       'Чтение текстур'),
    ('LOAD PANELS',                    'Reading Panels',
                                       'Чтение панелей'),
    ('LOAD ITEMS',                     'Reading Items',
                                       'Чтение предметов'),
    ('LOAD MONSTERS',                  'Reading Monsters',
                                       'Чтение монстров'),
    ('LOAD AREAS',                     'Reading Areas',
                                       'Чтение областей'),
    ('LOAD TRIGGERS',                  'Reading Triggers',
                                       'Чтение триггеров'),

    ('TEST AREA WALL',                 'Player collides with the wall and will be stuck.',
                                       'Если в этой области появится игрок, то он застрянет в стене и не сможет двигаться.'),
    ('TEST AREA WALL STR',             'Area #%d collides with Map (%d:%d)',
                                       'Область #%d пересекается с картой (%d:%d)'),
    ('TEST SPAWNS 1',                  'There are several spawn points for First Player on the map',
                                       'Несколько точек появления первого игрока'),
    ('TEST SPAWNS 2',                  'There are several spawn points for Second Player on the map',
                                       'Несколько точек появления второго игрока'),
    ('TEST SPAWNS',                    'There are several spawn points on the map. Random one will be used.',
                                       'Несколько точек появления игрока, будет использована случайная.'),
    ('TEST NO DM',                     'There are no DM spawn points on the map',
                                       'На карте нет точек DM'),
    ('TEST NO DM EX',                  'There are no DM spawn points on the map. Only "Single Player" mode available.',
                                       'На карте нет точек DM, поиграть получится только в режиме "Single Player".'),
    ('TEST MONSTER WALL',              'Monster collides with the wall and will be stuck.',
                                       'Монстр пересекается с картой. Он застрянет в стене и не сможет двигаться'),
    ('TEST MONSTER WALL STR',          'Monster #%d collides with a map (%d:%d)',
                                       'Монстр #%d пересекается с картой (%d:%d)'),

    ('OPT NO TEXTURES',                'Texture list is empty',
                                       'Список текстур пуст'),
    ('OPT DELETED TEXTURES',           'Deleted Textures:',
                                       'Удалённые текстуры:'),
    ('OPT TOTAL TEXTURES',             'Textures Total:',
                                       'Всего текстур:'),
    ('OPT TEX DELETED',                'Textures Deleted:',
                                       'Удалено текстур:'),
    ('OPT PANELS OPT',                 'Panels Optimized:',
                                       'Оптимизировано панелей:'),
    ('OPT WALLS',                      'Walls Optimization...',
                                       'Оптимизация стен...'),
    ('OPT FORES',                      'Foreground Optimization...',
                                       'Оптимизация переднего плана...'),
    ('OPT BACKS',                      'Background Optimization...',
                                       'Оптимизация фона...'),
    ('OPT STAIRS',                     'Steps Optimization...',
                                       'Оптимизация ступеней...'),
    ('OPT WATER',                      'Water Optimization...',
                                       'Оптимизация воды...'),
    ('OPT ACID1',                      'Acid 1 Optimization...',
                                       'Оптимизация кислоты 1...'),
    ('OPT ACID2',                      'Acid 2 Optimization...',
                                       'Оптимизация кислоты 2...'),
    ('OPT LIFTS',                      'Streams Optimization...',
                                       'Оптимизация лифтов...'),
    ('OPT BLOCKMON',                   'Monster Boundaries Optimization...',
                                       'Оптимизация блокираторов монстров....'), 
    ('OPT TOTAL PANELS',               'Panels Total:',
                                       'Всего панелей:'),
    ('OPT PANELS AFTER',               'Panels after Optimization:',
                                       'Панелей после оптимизации:'),

    ('WAD SPECIAL MAP',                '<MAP WAD-FILE>',
                                       '<WAD-ФАЙЛ КАРТЫ>'),
    ('WAD SPECIAL TEXS',               '<EXTRA TEXTURES>',
                                       '<СПЕЦТЕКСТУРЫ>'),

    ('FILE FILTER ALL',                'Doom 2D: Forever Maps (*.wad)|*.wad|Old Doom 2D: Forever 0.30 Maps (*.ini)|*.ini|All Files (*.*)|*.*',
                                       'Карты Doom 2D: Forever (*.wad)|*.wad|Старые карты Doom 2D: Forever 0.30 (*.ini)|*.ini|Все файлы (*.*)|*.*'),
    ('FILE FILTER WAD',                'Doom 2D: Forever Maps (*.wad)|*.wad|All Files (*.*)|*.*',
                                       'Карты Doom 2D: Forever (*.wad)|*.wad|Все файлы (*.*)|*.*'),

    ('EDITOR TITLE',                   'Doom 2D: Forever Map Editor',
                                       'Редактор карт Doom 2D: Forever'),

                                       
    ('', '', '') );


procedure SetupArrays();
var
  i: Integer;

begin
// Да/Нет:
  BoolNames[False] := _lc[I_ARRAY_BOOL_FALSE];
  BoolNames[True] := _lc[I_ARRAY_BOOL_TRUE];

// Направления:
  DirNames[D_LEFT] := _lc[I_ARRAY_DIR_LEFT];
  DirNames[D_RIGHT] := _lc[I_ARRAY_DIR_RIGHT];

// Смены направления:
  DirNamesAdv[0] := _lc[I_ARRAY_DIR_SAME];
  DirNamesAdv[1] := _lc[I_ARRAY_DIR_LEFT];
  DirNamesAdv[2] := _lc[I_ARRAY_DIR_RIGHT];
  DirNamesAdv[3] := _lc[I_ARRAY_DIR_REVERSED];

// Направление (на кнопках):
  DirButtonNames[1] := _lc[I_ARRAY_DIRBTN_LEFT];
  DirButtonNames[2] := _lc[I_ARRAY_DIRBTN_RIGHT];
  DirButtonNames[3] := _lc[I_ARRAY_DIRBTN_UP];
  DirButtonNames[4] := _lc[I_ARRAY_DIRBTN_DOWN];

// Названия панелей:
  PANELNAMES[0] := _lc[I_ARRAY_PANEL_WALL];
  PANELNAMES[1] := _lc[I_ARRAY_PANEL_BACK];
  PANELNAMES[2] := _lc[I_ARRAY_PANEL_FRONT];
  PANELNAMES[3] := _lc[I_ARRAY_PANEL_DOOR_OPEN];
  PANELNAMES[4] := _lc[I_ARRAY_PANEL_DOOR_CLOSE];
  PANELNAMES[5] := _lc[I_ARRAY_PANEL_STAIR];
  PANELNAMES[6] := _lc[I_ARRAY_PANEL_WATER];
  PANELNAMES[7] := _lc[I_ARRAY_PANEL_ACID_1];
  PANELNAMES[8] := _lc[I_ARRAY_PANEL_ACID_2];
  PANELNAMES[9] := _lc[I_ARRAY_PANEL_LIFT_UP];
  PANELNAMES[10] := _lc[I_ARRAY_PANEL_LIFT_DOWN];
  PANELNAMES[11] := _lc[I_ARRAY_PANEL_LIFT_LEFT];
  PANELNAMES[12] := _lc[I_ARRAY_PANEL_LIFT_RIGHT];
  PANELNAMES[13] := _lc[I_ARRAY_PANEL_BLOCKMON];

// Названия эффектов:
  EffectNames[EFFECT_NONE] := _lc[I_ARRAY_FX_NONE];
  EffectNames[EFFECT_TELEPORT] := _lc[I_ARRAY_FX_TELEPORT];
  EffectNames[EFFECT_RESPAWN] := _lc[I_ARRAY_FX_RESPAWN];
  EffectNames[EFFECT_FIRE] := _lc[I_ARRAY_FX_FIRE];

// Названия предметов:
  ItemNames[ITEM_MEDKIT_SMALL] := _lc[I_ARRAY_ITEM_MEDKIT];
  ItemNames[ITEM_MEDKIT_LARGE] := _lc[I_ARRAY_ITEM_LARGE_MEDKIT];
  ItemNames[ITEM_MEDKIT_BLACK] := _lc[I_ARRAY_ITEM_BLACK_MEDKIT];
  ItemNames[ITEM_ARMOR_GREEN] := _lc[I_ARRAY_ITEM_GREEN_ARMOR];
  ItemNames[ITEM_ARMOR_BLUE] := _lc[I_ARRAY_ITEM_BLUE_ARMOR];
  ItemNames[ITEM_SPHERE_BLUE] := _lc[I_ARRAY_ITEM_BLUE_SPHERE];
  ItemNames[ITEM_SPHERE_WHITE] := _lc[I_ARRAY_ITEM_MEGASPHERE];
  ItemNames[ITEM_SUIT] := _lc[I_ARRAY_ITEM_HAZ_SUIT];
  ItemNames[ITEM_OXYGEN] := _lc[I_ARRAY_ITEM_OXYGEN];
  ItemNames[ITEM_INVUL] := _lc[I_ARRAY_ITEM_INVULNERABILITY];
  ItemNames[ITEM_WEAPON_SAW] := _lc[I_ARRAY_ITEM_CHAINSAW];
  ItemNames[ITEM_WEAPON_SHOTGUN1] := _lc[I_ARRAY_ITEM_SHOTGUN];
  ItemNames[ITEM_WEAPON_SHOTGUN2] := _lc[I_ARRAY_ITEM_DB_SHOTGUN];
  ItemNames[ITEM_WEAPON_CHAINGUN] := _lc[I_ARRAY_ITEM_CHAINGUN];
  ItemNames[ITEM_WEAPON_ROCKETLAUNCHER] := _lc[I_ARRAY_ITEM_ROCKET_LAUNCHER];
  ItemNames[ITEM_WEAPON_PLASMA] := _lc[I_ARRAY_ITEM_PLASMA_RIFLE];
  ItemNames[ITEM_WEAPON_BFG] := _lc[I_ARRAY_ITEM_BFG];
  ItemNames[ITEM_WEAPON_SUPERPULEMET] := _lc[I_ARRAY_ITEM_SUPER_MINIGUN];
  ItemNames[ITEM_AMMO_BULLETS] := _lc[I_ARRAY_ITEM_CLIP];
  ItemNames[ITEM_AMMO_BULLETS_BOX] := _lc[I_ARRAY_ITEM_AMMO_BOX];
  ItemNames[ITEM_AMMO_SHELLS] := _lc[I_ARRAY_ITEM_4_SHELLS];
  ItemNames[ITEM_AMMO_SHELLS_BOX] := _lc[I_ARRAY_ITEM_25_SHELLS];
  ItemNames[ITEM_AMMO_ROCKET] := _lc[I_ARRAY_ITEM_1_ROCKET];
  ItemNames[ITEM_AMMO_ROCKET_BOX] := _lc[I_ARRAY_ITEM_ROCKET_BOX];
  ItemNames[ITEM_AMMO_CELL] := _lc[I_ARRAY_ITEM_CELL];
  ItemNames[ITEM_AMMO_CELL_BIG] := _lc[I_ARRAY_ITEM_LARGE_CELL];
  ItemNames[ITEM_AMMO_BACKPACK] := _lc[I_ARRAY_ITEM_BACKPACK];
  ItemNames[ITEM_KEY_RED] := _lc[I_ARRAY_ITEM_KEY_RED];
  ItemNames[ITEM_KEY_GREEN] := _lc[I_ARRAY_ITEM_KEY_GREEN];
  ItemNames[ITEM_KEY_BLUE] := _lc[I_ARRAY_ITEM_KEY_BLUE];
  ItemNames[ITEM_WEAPON_KASTET] := '?';
  ItemNames[ITEM_WEAPON_PISTOL] := '??';
  ItemNames[ITEM_BOTTLE] := _lc[I_ARRAY_ITEM_BOTTLE];
  ItemNames[ITEM_HELMET] := _lc[I_ARRAY_ITEM_HELMET];
  ItemNames[ITEM_JETPACK] := _lc[I_ARRAY_ITEM_JETPACK];
  ItemNames[ITEM_INVIS] := _lc[I_ARRAY_ITEM_INVIS];

// Названия снарядов:
  ShotNames[TRIGGER_SHOT_PISTOL] := _lc[I_ARRAY_SHOT_PISTOL];
  ShotNames[TRIGGER_SHOT_BULLET] := _lc[I_ARRAY_SHOT_BULLET];
  ShotNames[TRIGGER_SHOT_SHOTGUN] := _lc[I_ARRAY_SHOT_SHOTGUN];
  ShotNames[TRIGGER_SHOT_SSG] := _lc[I_ARRAY_SHOT_SSG];
  ShotNames[TRIGGER_SHOT_IMP] := _lc[I_ARRAY_SHOT_IMP];
  ShotNames[TRIGGER_SHOT_PLASMA] := _lc[I_ARRAY_SHOT_PLASMA];
  ShotNames[TRIGGER_SHOT_SPIDER] := _lc[I_ARRAY_SHOT_SPIDER];
  ShotNames[TRIGGER_SHOT_CACO] := _lc[I_ARRAY_SHOT_CACO];
  ShotNames[TRIGGER_SHOT_BARON] := _lc[I_ARRAY_SHOT_BARON];
  ShotNames[TRIGGER_SHOT_MANCUB] := _lc[I_ARRAY_SHOT_MANCUB];
  ShotNames[TRIGGER_SHOT_REV] := _lc[I_ARRAY_SHOT_REV];
  ShotNames[TRIGGER_SHOT_ROCKET] := _lc[I_ARRAY_SHOT_ROCKET];
  ShotNames[TRIGGER_SHOT_BFG] := _lc[I_ARRAY_SHOT_BFG];
  ShotNames[TRIGGER_SHOT_EXPL] := _lc[I_ARRAY_SHOT_EXPL];
  ShotNames[TRIGGER_SHOT_BFGEXPL] := _lc[I_ARRAY_SHOT_BFGEXPL];

// Названия монстров:
  MonsterNames[MONSTER_DEMON] := _lc[I_ARRAY_MON_DEMON];
  MonsterNames[MONSTER_IMP] := _lc[I_ARRAY_MON_IMP];
  MonsterNames[MONSTER_ZOMBY] := _lc[I_ARRAY_MON_ZOMBIE];
  MonsterNames[MONSTER_SERG] := _lc[I_ARRAY_MON_SERGEANT];
  MonsterNames[MONSTER_CYBER] := _lc[I_ARRAY_MON_CYBER];
  MonsterNames[MONSTER_CGUN] := _lc[I_ARRAY_MON_CGUN];
  MonsterNames[MONSTER_BARON] := _lc[I_ARRAY_MON_HELL_BARON];
  MonsterNames[MONSTER_KNIGHT] := _lc[I_ARRAY_MON_HELL_KNIGHT];
  MonsterNames[MONSTER_CACO] := _lc[I_ARRAY_MON_CACODEMON];
  MonsterNames[MONSTER_SOUL] := _lc[I_ARRAY_MON_LOST_SOUL];
  MonsterNames[MONSTER_PAIN] := _lc[I_ARRAY_MON_PAIN_ELEMENTAL];
  MonsterNames[MONSTER_SPIDER] := _lc[I_ARRAY_MON_MASTERMIND];
  MonsterNames[MONSTER_BSP] := _lc[I_ARRAY_MON_ARACHNATRON];
  MonsterNames[MONSTER_MANCUB] := _lc[I_ARRAY_MON_MANCUBUS];
  MonsterNames[MONSTER_SKEL] := _lc[I_ARRAY_MON_REVENANT];
  MonsterNames[MONSTER_VILE] := _lc[I_ARRAY_MON_ARCHVILE];
  MonsterNames[MONSTER_FISH] := _lc[I_ARRAY_MON_FISH];
  MonsterNames[MONSTER_BARREL] := _lc[I_ARRAY_MON_BARREL];
  MonsterNames[MONSTER_ROBO] := _lc[I_ARRAY_MON_ROBOT];
  MonsterNames[MONSTER_MAN] := _lc[I_ARRAY_MON_PRIKOLIST];

// Названия областей:
  AreaNames[AREA_PLAYERPOINT1] := _lc[I_ARRAY_AREA_PLAYER_ONE];
  AreaNames[AREA_PLAYERPOINT2] := _lc[I_ARRAY_AREA_PLAYER_TWO];
  AreaNames[AREA_DMPOINT] := _lc[I_ARRAY_AREA_DM];
  AreaNames[AREA_REDFLAG] := _lc[I_ARRAY_AREA_FLAG_RED];
  AreaNames[AREA_BLUEFLAG] := _lc[I_ARRAY_AREA_FLAG_BLUE];
  AreaNames[AREA_DOMFLAG] := _lc[I_ARRAY_AREA_FLAG_DOM];
  AreaNames[AREA_REDTEAMPOINT] := _lc[I_ARRAY_AREA_TEAM_RED];
  AreaNames[AREA_BLUETEAMPOINT] := _lc[I_ARRAY_AREA_TEAM_BLUE];

// Названия триггеров:
  TriggerNames[TRIGGER_EXIT] := _lc[I_ARRAY_TR_EXIT];
  TriggerNames[TRIGGER_TELEPORT] := _lc[I_ARRAY_TR_TELEPORT];
  TriggerNames[TRIGGER_OPENDOOR] := _lc[I_ARRAY_TR_DOOR_OPEN];
  TriggerNames[TRIGGER_CLOSEDOOR] := _lc[I_ARRAY_TR_DOOR_CLOSE];
  TriggerNames[TRIGGER_DOOR] := _lc[I_ARRAY_TR_DOOR_SWITCH];
  TriggerNames[TRIGGER_DOOR5] := _lc[I_ARRAY_TR_DOOR_5_SEC];
  TriggerNames[TRIGGER_CLOSETRAP] := _lc[I_ARRAY_TR_TRAP_CLOSE];
  TriggerNames[TRIGGER_TRAP] := _lc[I_ARRAY_TR_TRAP];
  TriggerNames[TRIGGER_PRESS] := _lc[I_ARRAY_TR_EXTEND];
  TriggerNames[TRIGGER_SECRET] := _lc[I_ARRAY_TR_SECRET];
  TriggerNames[TRIGGER_LIFTUP] := _lc[I_ARRAY_TR_LIFT_UP];
  TriggerNames[TRIGGER_LIFTDOWN] := _lc[I_ARRAY_TR_LIFT_DOWN];
  TriggerNames[TRIGGER_LIFT] := _lc[I_ARRAY_TR_LIFT_SWITCH];
  TriggerNames[TRIGGER_TEXTURE] := _lc[I_ARRAY_TR_TEXTURE];
  TriggerNames[TRIGGER_ON] := _lc[I_ARRAY_TR_ON];
  TriggerNames[TRIGGER_OFF] := _lc[I_ARRAY_TR_OFF];
  TriggerNames[TRIGGER_ONOFF] := _lc[I_ARRAY_TR_SWITCH];
  TriggerNames[TRIGGER_SOUND] := _lc[I_ARRAY_TR_SOUND];
  TriggerNames[TRIGGER_SPAWNMONSTER] := _lc[I_ARRAY_TR_SPAWN_MONSTER];
  TriggerNames[TRIGGER_SPAWNITEM] := _lc[I_ARRAY_TR_SPAWN_ITEM];
  TriggerNames[TRIGGER_MUSIC] := _lc[I_ARRAY_TR_MUSIC];
  TriggerNames[TRIGGER_PUSH] := _lc[I_ARRAY_TR_PUSH];
  TriggerNames[TRIGGER_SCORE] := _lc[I_ARRAY_TR_SCORE];
  TriggerNames[TRIGGER_MESSAGE] := _lc[I_ARRAY_TR_MESSAGE];
  TriggerNames[TRIGGER_DAMAGE] := _lc[I_ARRAY_TR_DAMAGE];
  TriggerNames[TRIGGER_HEALTH] := _lc[I_ARRAY_TR_HEALTH];
  TriggerNames[TRIGGER_SHOT] := _lc[I_ARRAY_TR_SHOT];
  TriggerNames[TRIGGER_EFFECT] := _lc[I_ARRAY_TR_EFFECT];

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
    clbActivationType.Items.Add(_lc[I_CTRL_ACT_PLAYER_CLOSE]);
    clbActivationType.Items.Add(_lc[I_CTRL_ACT_MONSTER_CLOSE]);
    clbActivationType.Items.Add(_lc[I_CTRL_ACT_PLAYER_PRESS]);
    clbActivationType.Items.Add(_lc[I_CTRL_ACT_MONSTER_PRESS]);
    clbActivationType.Items.Add(_lc[I_CTRL_ACT_SHOT]);
    clbActivationType.Items.Add(_lc[I_CTRL_ACT_NO_MONSTER]);

    clbKeys.Clear();
    clbKeys.Items.Add(_lc[I_CTRL_KEYS_RED]);
    clbKeys.Items.Add(_lc[I_CTRL_KEYS_GREEN]);
    clbKeys.Items.Add(_lc[I_CTRL_KEYS_BLUE]);
    clbKeys.Items.Add(_lc[I_CTRL_KEYS_TEAM_RED]);
    clbKeys.Items.Add(_lc[I_CTRL_KEYS_TEAM_BLUE]);
  end;
end;

procedure SetupCaptions();
var
  i: Integer;

begin
// Главная форма:
  with MainForm do
  begin
  // Заголовок:
    FormCaption := _lc[I_EDITOR_TITLE];
    i := Pos('-', Caption);
    if i > 0 then
      begin
        Caption := FormCaption + ' ' + Copy(Caption, i, Length(Caption)-i+1);
      end
    else
      Caption := FormCaption;
  // Главное меню:
  // "Файл":
    miMenuFile.Caption := _lc[I_MENU_FILE];
    miNewMap.Caption := _lc[I_MENU_FILE_NEW];
    miOpenMap.Caption := _lc[I_MENU_FILE_OPEN];
    miSaveMap.Caption := _lc[I_MENU_FILE_SAVE];
    miSaveMapAs.Caption := _lc[I_MENU_FILE_SAVE_AS];
    miOpenWadMap.Caption := _lc[I_MENU_FILE_OPEN_WAD];
    miSaveMiniMap.Caption := _lc[I_MENU_FILE_SAVE_MINI];
    miDeleteMap.Caption := _lc[I_MENU_FILE_DELETE];
    miPackMap.Caption := _lc[I_MENU_FILE_PACK_MAP];
    miExit.Caption := _lc[I_MENU_FILE_EXIT];
  // "Правка":
    miMenuEdit.Caption := _lc[I_MENU_EDIT];
    miUndo.Caption := _lc[I_MENU_EDIT_UNDO];
    miCopy.Caption := _lc[I_MENU_EDIT_COPY];
    miCut.Caption := _lc[I_MENU_EDIT_CUT];
    miPaste.Caption := _lc[I_MENU_EDIT_PASTE];
    miSelectAll.Caption := _lc[I_MENU_EDIT_SELECT_ALL];
    miToFore.Caption := _lc[I_MENU_EDIT_TO_FORE];
    miToBack.Caption := _lc[I_MENU_EDIT_TO_BACK];
  // "Инструменты":
    miMenuTools.Caption := _lc[I_MENU_TOOLS];
    miSnapToGrid.Caption := _lc[I_MENU_TOOLS_SNAP_GRID];
    miMiniMap.Caption := _lc[I_MENU_TOOLS_MINIMAP];
    miSwitchGrid.Caption := _lc[I_MENU_TOOLS_STEP_GRID];
    miShowEdges.Caption := _lc[I_MENU_TOOLS_SHOW_EDGES];
    miLayers.Caption := _lc[I_MENU_TOOLS_LAYERS];
    miLayer1.Caption := _lc[I_MENU_LAYER_BACK];
    miLayer2.Caption := _lc[I_MENU_LAYER_WALL];
    miLayer3.Caption := _lc[I_MENU_LAYER_FORE];
    miLayer4.Caption := _lc[I_MENU_LAYER_STAIR];
    miLayer5.Caption := _lc[I_MENU_LAYER_WATER];
    miLayer6.Caption := _lc[I_MENU_LAYER_ITEM];
    miLayer7.Caption := _lc[I_MENU_LAYER_MONSTER];
    miLayer8.Caption := _lc[I_MENU_LAYER_AREA];
    miLayer9.Caption := _lc[I_MENU_LAYER_TRIGGER];
  // "Сервис":
    miMenuService.Caption := _lc[I_MENU_SERV];
    miCheckMap.Caption := _lc[I_MENU_SERV_TEST];
    miOptimmization.Caption := _lc[I_MENU_SERV_OPT];
    miMapPreview.Caption := _lc[I_MENU_SERV_PREVIEW];
    miTestMap.Caption := _lc[I_MENU_SERV_LAUNCH];
  // "Настройка":
    miMenuSettings.Caption := _lc[I_MENU_SETS];
    miMapOptions.Caption := _lc[I_MENU_SETS_MAP];
    miOptions.Caption := _lc[I_MENU_SETS_EDITOR];
    miMapTestSettings.Caption := _lc[I_MENU_SETS_LAUNCH];
  // "Справка":
    miMenuHelp.Caption := _lc[I_MENU_HELP];
    miAbout.Caption := _lc[I_MENU_HELP_ABOUT];

  // Панель инструментов:
    tbNewMap.Hint := _lc[I_MENU_TB_NEW];
    tbOpenMap.Hint := _lc[I_MENU_TB_OPEN];
    tbSaveMap.Hint := _lc[I_MENU_TB_SAVE];
    tbOpenWadMap.Hint := _lc[I_MENU_TB_OPEN_WAD];
    tbShowMap.Hint := _lc[I_MENU_TB_MINIMAP];
    tbShow.Hint := _lc[I_MENU_TB_LAYERS];
    tbGridOn.Hint := _lc[I_MENU_TB_GRID];
    tbGrid.Hint := _lc[I_MENU_TB_GRID_STEP];
    tbTestMap.Hint := _lc[I_MENU_TB_LAUNCH];
  // Всплывающее меню для кнопки слоев:
    miLayerP1.Caption := _lc[I_MENU_LAYER_BACK];
    miLayerP2.Caption := _lc[I_MENU_LAYER_WALL];
    miLayerP3.Caption := _lc[I_MENU_LAYER_FORE];
    miLayerP4.Caption := _lc[I_MENU_LAYER_STAIR];
    miLayerP5.Caption := _lc[I_MENU_LAYER_WATER];
    miLayerP6.Caption := _lc[I_MENU_LAYER_ITEM];
    miLayerP7.Caption := _lc[I_MENU_LAYER_MONSTER];
    miLayerP8.Caption := _lc[I_MENU_LAYER_AREA];
    miLayerP9.Caption := _lc[I_MENU_LAYER_TRIGGER];
  // Всплывающее меню для кнопки теста карты:
    miMapTestPMSet.Caption := _lc[I_MENU_LAUNCH_SETS];

  // Кнопка применения свойств:
    bApplyProperty.Caption := _lc[I_BTN_APPLY_PROPS];
  // Редактор свойств объектов:
    vleObjectProperty.TitleCaptions[0] := _lc[I_CTRL_PROP_KEY];
    vleObjectProperty.TitleCaptions[1] := _lc[I_CTRL_PROP_VALUE];

  // Вкладка "Панели":
    tsPanels.Caption := _lc[I_CTRL_PANELS];
    lbPanelType.Hint := _lc[I_PROP_PANEL_TYPE];
    lbTextureList.Hint := _lc[I_CTRL_LIST_TEXTURE];
  // Панель настройки текстур:
    LabelTxW.Caption := _lc[I_LAB_TEX_WIDTH];
    LabelTxH.Caption := _lc[I_LAB_TEX_HEIGHT];
    cbPreview.Caption := _lc[I_CTRL_PREVIEW];
    bbAddTexture.Hint := _lc[I_BTN_TEXTURE_ADD];
    bbRemoveTexture.Hint := _lc[I_BTN_TEXTURE_DELETE];
    bClearTexture.Hint := _lc[I_BTN_TEXTURE_EMPTY];

  // Вкладка "Предметы":
    tsItems.Caption := _lc[I_CTRL_ITEMS];
    lbItemList.Hint := _lc[I_CTRL_LIST_ITEM];
    cbOnlyDM.Caption := _lc[I_CTRL_ITEM_DM];
    cbFall.Caption := _lc[I_CTRL_ITEM_FALLS];

  // Вкладка "Монстры":
    tsMonsters.Caption := _lc[I_CTRL_MONSTERS];
    lbMonsterList.Hint := _lc[I_CTRL_LIST_MONSTER];
    rbMonsterLeft.Caption := _lc[I_CTRL_LEFT];
    rbMonsterRight.Caption := _lc[I_CTRL_RIGHT];

  // Вкладка "Области":
    tsAreas.Caption := _lc[I_CTRL_AREAS];
    lbAreasList.Hint := _lc[I_CTRL_LIST_AREA];
    rbAreaLeft.Caption := _lc[I_CTRL_LEFT];
    rbAreaRight.Caption := _lc[I_CTRL_RIGHT];

  // Вкладка "Триггеры":
    tsTriggers.Caption := _lc[I_CTRL_TRIGGERS];
    lbTriggersList.Hint := _lc[I_CTRL_LIST_TRIGGER];
    clbActivationType.Hint := _lc[I_CTRL_LIST_ACTIVE];
    clbKeys.Hint := _lc[I_CTRL_LIST_KEYS];
  end;

// Форма "О программе":
  with AboutForm do
  begin
    Caption := _lc[I_CAP_ABOUT];
    LabelTitle.Caption := _lc[I_EDITOR_TITLE];
    LabelVer.Caption := _lc[I_LAB_ABOUT_VER];
    LabelAuthor.Caption := _lc[I_LAB_ABOUT_AUTHOR];
    LabelAuthor2.Caption := _lc[I_LAB_ABOUT_AUTHOR_2];
    LabelMail.Caption := _lc[I_LAB_ABOUT_MAIL];
    LabelMail2.Caption := _lc[I_LAB_ABOUT_MAIL_2];
    LabelSite.Caption := _lc[I_LAB_ABOUT_SITE];
    LabelHttp.Caption := _lc[I_LAB_ABOUT_HTTP];
  end;

// Форма "Тип активации":
  with ActivationTypeForm do
  begin
    Caption := _lc[I_CAP_ACT];
    cbPlayerCollide.Caption := _lc[I_CTRL_ACT_PLAYER_CLOSE];
    cbMonsterCollide.Caption := _lc[I_CTRL_ACT_MONSTER_CLOSE];
    cbPlayerPress.Caption := _lc[I_CTRL_ACT_PLAYER_PRESS];
    cbMonsterPress.Caption := _lc[I_CTRL_ACT_MONSTER_PRESS];
    cbShot.Caption := _lc[I_CTRL_ACT_SHOT];
    cbNoMonster.Caption := _lc[I_CTRL_ACT_NO_MONSTER];
    bOK.Caption := _lc[I_BTN_OK];
  end;

// Форма "Выбор текстуры для неба":
  with AddSkyForm do
  begin
    Caption := _lc[I_CAP_ADD_SKY];
    LabelWADs.Caption := _lc[I_LAB_ADD_WADS];
    LabelSections.Caption := _lc[I_LAB_ADD_SECTIONS];
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
  end;

// Форма "Выбор звука или музыки":
  with AddSoundForm do
  begin
    Caption := _lc[I_CAP_ADD_SOUND];
    LabelWADs.Caption := _lc[I_LAB_ADD_WADS];
    LabelSections.Caption := _lc[I_LAB_ADD_SECTIONS];
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
    bEmpty.Caption := _lc[I_BTN_NO_SOUND];
  end;

// Форма "Выбор текстуры":
  with AddTextureForm do
  begin
    Caption := _lc[I_CAP_ADD_TEXTURE];
    LabelWADs.Caption := _lc[I_LAB_ADD_WADS];
    LabelSections.Caption := _lc[I_LAB_ADD_SECTIONS];
    bAddTexture.Caption := _lc[I_BTN_ADD];
    bClose.Caption := _lc[I_BTN_CLOSE];
    bAddClose.Caption := _lc[I_BTN_ADD_CLOSE];
  end;

// Форма "Выбор типа монстра" / "Выбор типа предмета":
  ChooseTypeForm.bOK.Caption := _lc[I_BTN_OK];

// Форма "Ключи":
  with KeysForm do
  begin
    Caption := _lc[I_CAP_KEYS];
    cbRedKey.Caption := _lc[I_CTRL_KEYS_RED];
    cbGreenKey.Caption := _lc[I_CTRL_KEYS_GREEN];
    cbBlueKey.Caption := _lc[I_CTRL_KEYS_BLUE];
    cbRedTeam.Caption := _lc[I_CTRL_KEYS_TEAM_RED];
    cbBlueTeam.Caption := _lc[I_CTRL_KEYS_TEAM_BLUE];
    bOK.Caption := _lc[I_BTN_OK];
  end;

// Форма "Проверка карты":
  with MapCheckForm do
  begin
    Caption := _lc[I_CAP_TEST];
    bCheckMap.Caption := _lc[I_BTN_TEST_AGAIN];
    bClose.Caption := _lc[I_BTN_CLOSE];
  end;

// Форма "Оптимизация карты":
  with MapOptimizationForm do
  begin
    Caption := _lc[I_CAP_OPT];
  // Выбор оптимизации:
    GroupBoxOpt.Caption := _lc[I_CTRL_OPT_GB];
    rbTexturesOptimization.Caption := _lc[I_CTRL_OPT_TEXTURE];
    rbPanelsOptimization.Caption := _lc[I_CTRL_OPT_PANEL];
  // Оптимизация текстур:
    bBeginTextureOptimization.Caption := _lc[I_BTN_START];
  // Оптимизация панелей:
    cbOptimizeWalls.Caption := _lc[I_CTRL_OPT_WALL];
    cbOptimizeForeGround.Caption := _lc[I_CTRL_OPT_FORE];
    cbOptimizeBackGround.Caption := _lc[I_CTRL_OPT_BACK];
    cbOptimizeSteps.Caption := _lc[I_CTRL_OPT_STAIR];
    cbOptimizeWater.Caption := _lc[I_CTRL_OPT_WATER];
    cbOptimizeAcid1.Caption := _lc[I_CTRL_OPT_ACID1];
    cbOptimizeAcid2.Caption := _lc[I_CTRL_OPT_ACID2];
    cbOptimizeLift.Caption := _lc[I_CTRL_OPT_LIFT];
    cbOptimizeBlockMon.Caption := _lc[I_CTRL_OPT_BLOCKMON];
    bBeginPanelsOptimization.Caption := _lc[I_BTN_START];
  end;

// Форма "Параметры карты":
  with MapOptionsForm do
  begin
    Caption := _lc[I_CAP_SETS];
  // Основные параметры:
    LabelName.Caption := _lc[I_LAB_SETS_NAME];
    LabelDesc.Caption := _lc[I_LAB_SETS_DESC];
    LabelAuthor.Caption := _lc[I_LAB_SETS_AUTHOR];
    LabelBack.Caption := _lc[I_LAB_SETS_BACK];
    LabelMusic.Caption := _lc[I_LAB_SETS_MUSIC];
  // Статистика:
    GBStats.Caption := _lc[I_CTRL_SETS_STATS];
    LabelTexs.Caption := _lc[I_LAB_SETS_TEXTURES];
    LabelPanels.Caption := _lc[I_LAB_SETS_PANELS];
    LabelItems.Caption := _lc[I_LAB_SETS_ITEMS];
    LabelMonsters.Caption := _lc[I_LAB_SETS_MONSTERS];
    LabelAreas.Caption := _lc[I_LAB_SETS_AREAS];
    LabelTriggers.Caption := _lc[I_LAB_SETS_TRIGGERS];
  // Размеры:
    GBSizes.Caption := _lc[I_CTRL_SETS_SIZES];
    LabelWidth.Caption := _lc[I_LAB_SETS_WIDTH];
    LabelHeight.Caption := _lc[I_LAB_SETS_HEIGHT];
    LabelCurSize.Caption := _lc[I_LAB_SETS_SIZES];
    LabelMapMove.Caption := _lc[I_LAB_SETS_MAPMOVE];
    cbSnapping.Caption := _lc[I_LAB_SETS_SNAPPING];
  // Кнопки:
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
  end;

// Форма "Настройки теста карты":
  with MapTestForm do
  begin
    Caption := _lc[I_CAP_LAUNCH];
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
  // Режим игры:
    rbDM.Caption := _lc[I_CTRL_LAUNCH_DM];
    rbTDM.Caption := _lc[I_CTRL_LAUNCH_TDM];
    rbCTF.Caption := _lc[I_CTRL_LAUNCH_CTF];
    rbCOOP.Caption := _lc[I_CTRL_LAUNCH_COOP];
  // Опции:
    cbTwoPlayers.Caption := _lc[I_CTRL_LAUNCH_2P];
    cbTeamDamage.Caption := _lc[I_CTRL_LAUNCH_FF];
    cbAllowExit.Caption := _lc[I_CTRL_LAUNCH_EXIT];
    cbWeaponStay.Caption := _lc[I_CTRL_LAUNCH_WEAPONS];
    cbMonstersDM.Caption := _lc[I_CTRL_LAUNCH_MONSTERS];
    cbMapOnce.Caption := _lc[I_CTRL_LAUNCH_CLOSE];
    LabelTime.Caption := _lc[I_LAB_LAUNCH_TIME];
    LabelSecs.Caption := _lc[I_LAB_LAUNCH_SECS];
    LabelScore.Caption := _lc[I_LAB_LAUNCH_SCORE];
  // Путь:
    LabelPath.Caption := _lc[I_LAB_LAUNCH_PATH];
    FindD2dDialog.Title := _lc[I_CTRL_LAUNCH_OPEN];
  end;

// Форма "Настройки редактора":
  with OptionsForm do
  begin
    Caption := _lc[I_CAP_ES];
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
  // Настройки:
    cbShowDots.Caption := _lc[I_CTRL_ES_GRID];
    cbShowTexture.Caption := _lc[I_CTRL_ES_TEXTURE];
    cbShowSize.Caption := _lc[I_CTRL_ES_PANEL_SIZE];
    LabelGrid.Caption := _lc[I_LAB_ES_GRID];
    LabelGridCol.Caption := _lc[I_LAB_ES_GRID_COLOR];
    LabelBack.Caption := _lc[I_LAB_ES_BACK];
    LabelPreview.Caption := _lc[I_LAB_ES_PREVIEW];
    LabelMinimap.Caption := _lc[I_LAB_ES_MINIMAP];
    LabelRecent.Caption := _lc[I_LAB_ES_RECENT];
    LabelLanguage.Caption := _lc[I_LAB_ES_LANGUAGE];
  end;

// Форма "Упаковать карту":
  with PackMapForm do
  begin
    Caption := _lc[I_CAP_PACK];
    bPack.Caption := _lc[I_BTN_PACK];
    LabelSaveTo.Caption := _lc[I_LAB_PACK_SAVE_TO];
    LabelMapName.Caption := _lc[I_LAB_PACK_MAP_NAME];
  // Что упаковывать:
    cbTextrures.Caption := _lc[I_CTRL_PACK_TEXTURES];
    LabelTextures.Caption := _lc[I_LAB_PACK_TEXTURES];
    cbSky.Caption := _lc[I_CTRL_PACK_SKY];
    LabelSky.Caption := _lc[I_LAB_PACK_SKY];
    cbMusic.Caption := _lc[I_CTRL_PACK_MUSIC];
    LabelMusic.Caption := _lc[I_LAB_PACK_MUSIC];
    cbAdd.Caption := _lc[I_CTRL_PACK_ADD];
    cbNonStandart.Caption := _lc[I_CTRL_PACK_NON_STD];
  end;

// Форма "Сохранить карту":
  with SaveMapForm do
  begin
    Caption := _lc[I_CAP_SAVE];
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
  end;

// Форма "Сохранить мини-карту":
  with SaveMiniMapForm do
  begin
    Caption := _lc[I_CAP_MINI];
    LabelScale.Caption := _lc[I_LAB_MINI_SCALE];
    bSave.Caption := _lc[I_BTN_SAVE];
    bClose.Caption := _lc[I_BTN_CLOSE];
  end;

// Форма "Выбор карты":
  with SelectMapForm do
  begin
    Caption := _lc[I_CAP_SELECT];
    bOK.Caption := _lc[I_BTN_OK];
    bCancel.Caption := _lc[I_BTN_CANCEL];
  end;

// Заголовок приложения:
  Application.Title := _lc[I_EDITOR_TITLE];
end;

procedure g_Language_Load(fileName: String);
var
  F: TextFile;
  key, value: String;
  i: TStrings_Locale;
  k: Integer;
  ok: Boolean;

begin
// Значения по-умолчанию:
  for i := Low(TStrings_Locale) to High(TStrings_Locale) do
    _lc[i] := g_lang_default[i][LANGUAGE_ENGLISH_N];

  if FileExists(fileName) then
    begin
      AssignFile(F, fileName);
      ReSet(F);
      k := 0;

    // Читаем файл:
      while not EoF(F) do
      begin
      // Читаем строку:
        ReadLn(F, key);
        key := Trim(key);

      // Строка - ключ перевода:
        if (key <> '') and
           (key[1] = '[') and
           (Pos(']', key) > 2) then
        begin
          key := UpperCase(Copy(key, 2, Pos(']', key)-2));

        // Пропускаем пустые строки до строки - перевода:
          value := '';
          while (not EoF(F)) and (value = '') do
          begin
            ReadLn(F, value);
            value := Trim(value);
          end;

        // Есть строка - перевод:
          if value <> '' then
          begin
          // Ищем индекс ключа перевода:
            ok := False;
            i := TStrings_Locale(k);

          // От текущего к первому:
            while i > Low(TStrings_Locale) do
            begin
              if g_lang_default[i][1] = key then
              begin
                _lc[i] := value;
                ok := True;
                Break;
              end;

              Dec(i);
            end;

          // Первый:
            if not ok then
            begin
              i := Low(TStrings_Locale);

              if (g_lang_default[i][1] = key) then
              begin
                _lc[i] := value;
                ok := True;
              end;
            end;

          // От следующего за текущим до последнего:
            if not ok then
            begin
              i := TStrings_Locale(k);

              while i < High(TStrings_Locale) do
              begin
                Inc(i);

                if g_lang_default[i][1] = key then
                begin
                  _lc[i] := value;
                  Break;
                end;
              end;
            end;
          end;

          Inc(k);
        end;
      end;

      CloseFile(F);
    end
  else
    e_WriteLog('Language file "'+fileName+'" not found!', MSG_WARNING);

  SetupArrays();
  SetupCaptions();
  RemoveSelectFromObjects();
end;

procedure g_Language_Set(lang: String);
var
  i: TStrings_Locale;
  n: Byte;

begin
  if lang = LANGUAGE_ENGLISH then
    n := LANGUAGE_ENGLISH_N
  else
    n := LANGUAGE_RUSSIAN_N;

  for i := Low(TStrings_Locale) to High(TStrings_Locale) do
  begin
    _lc[i] := g_lang_default[i][n];
    // have to cache a CP1251 version for GL text
    _glc[i] := utf8to1251(g_lang_default[i][n]);
  end;

  SetupArrays();
  SetupCaptions();
  RemoveSelectFromObjects();
end;

procedure g_Language_Dump(fileName: String);
var
  F: TextFile;
  i: TStrings_Locale;

begin
  AssignFile(F, fileName);
  ReWrite(F);

  for i := Low(TStrings_Locale) to High(TStrings_Locale) do
    WriteLn(F, _lc[i]);

  CloseFile(F);
end;

End.
