Unit g_language;

Interface

Uses
  MAPDEF;

Type
  TStrings_Locale = (
    I_CONSOLE_DUMPED,  
    I_CONSOLE_ERROR_WRITE,
    I_CONSOLE_SCREENSHOT,             
    I_CONSOLE_UNKNOWN,                
    I_CONSOLE_WELCOME,                

    I_GAME_ERROR_GET_SPAWN,              
    I_GAME_ERROR_CTF,
    I_GAME_ERROR_MAP_WAD,
    I_GAME_ERROR_MAP_RES,
    I_GAME_ERROR_MAP_LOAD,
    I_GAME_ERROR_MAP_SELECT,          
    I_GAME_ERROR_PLAYER_CREATE,
    I_GAME_ERROR_TEXTURE_ANIM,
    I_GAME_ERROR_TEXTURE_SIMPLE,
    I_GAME_ERROR_MODEL,
    I_GAME_ERROR_SKY,
    I_GAME_ERROR_MUSIC,
    I_GAME_ERROR_SAVE,
    I_GAME_ERROR_LOAD,
    I_GAME_ERROR_SOUND,
    I_GAME_ERROR_FRAMES,
    I_GAME_ERROR_TR_SOUND,
    I_GAME_ERROR_SWITCH_TEXTURE,

    I_GAME_PLAYER_NAME,               
    I_GAME_GAME_TIME,                 
    I_GAME_FRAGS,                     
    I_GAME_DEATHS,                    
    I_GAME_DM,                        
    I_GAME_CTF,                       
    I_GAME_TDM,                       
    I_GAME_FRAG_LIMIT,                
    I_GAME_SCORE_LIMIT,               
    I_GAME_TIME_LIMIT,                
    I_GAME_TEAM_SCORE_RED,
    I_GAME_TEAM_SCORE_BLUE,
    I_GAME_TEAM_RED,                  
    I_GAME_TEAM_BLUE,                 
    I_GAME_WIN_RED,                   
    I_GAME_WIN_BLUE,                  
    I_GAME_WIN_DRAW,                  

    I_MENU_START_GAME,                
    I_MENU_MAIN_MENU,                 
    I_MENU_NEW_GAME,                  
    I_MENU_MULTIPLAYER,               
    I_MENU_OPTIONS,                   
    I_MENU_AUTHORS,                   
    I_MENU_EXIT,                      
    I_MENU_1_PLAYER,                  
    I_MENU_2_PLAYERS,                 
    I_MENU_CUSTOM_GAME,               
    I_MENU_EPISODE,                   
    I_MENU_SELECT_MAP,                
    I_MENU_VIDEO_OPTIONS,             
    I_MENU_SOUND_OPTIONS,             
    I_MENU_SAVED_OPTIONS,             
    I_MENU_DEFAULT_OPTIONS,           
    I_MENU_GAME_OPTIONS,              
    I_MENU_CONTROLS_OPTIONS,          
    I_MENU_PLAYER_OPTIONS,
    I_MENU_LANGUAGE_OPTIONS,
    I_MENU_LOAD_GAME,                 
    I_MENU_SAVE_GAME,                 
    I_MENU_END_GAME,                  
    I_MENU_RESTART,                   
    I_MENU_SET_GAME,                  

    I_MENU_STATISTICS,                
    I_MENU_MAP,                       
    I_MENU_GAME_TYPE,                 
    I_MENU_GAME_TYPE_DM,              
    I_MENU_GAME_TYPE_CTF,             
    I_MENU_GAME_TYPE_TDM,             
    I_MENU_GAME_TYPE_COOP,            
    I_MENU_TIME_LIMIT,                
    I_MENU_GOAL_LIMIT,                
    I_MENU_TEAM_DAMAGE,               
    I_MENU_ENABLE_EXITS,              
    I_MENU_WEAPONS_STAY,              
    I_MENU_ENABLE_MONSTERS,           
    I_MENU_BOTS_VS,                   
    I_MENU_BOTS_VS_PLAYERS,           
    I_MENU_BOTS_VS_MONSTERS,          
    I_MENU_BOTS_VS_ALL,               

    I_MENU_MAP_WAD,                   
    I_MENU_MAP_RESOURCE,              
    I_MENU_MAP_NAME,                  
    I_MENU_MAP_AUTHOR,                
    I_MENU_MAP_DESCRIPTION,           
    I_MENU_MAP_SIZE,                  
    I_MENU_PLAYERS,                   
    I_MENU_PLAYERS_ONE,               
    I_MENU_PLAYERS_TWO,               

    I_MENU_INTER1,                    
    I_MENU_INTER2,                    
    I_MENU_INTER3,                    
    I_MENU_INTER4,                    
    I_MENU_INTER5,                    
    I_MENU_INTER6,
    I_MENU_LOADING,                   
    I_MENU_PLAYER_1,                  
    I_MENU_PLAYER_2,

    I_MENU_CONTROL_GLOBAL,            
    I_MENU_CONTROL_SCREENSHOT,        
    I_MENU_CONTROL_STAT,              
    I_MENU_CONTROL_LEFT,              
    I_MENU_CONTROL_RIGHT,             
    I_MENU_CONTROL_UP,                
    I_MENU_CONTROL_DOWN,              
    I_MENU_CONTROL_JUMP,              
    I_MENU_CONTROL_FIRE,              
    I_MENU_CONTROL_USE,               
    I_MENU_CONTROL_NEXT_WEAPON,
    I_MENU_CONTROL_PREV_WEAPON,

    I_MENU_COUNT_NONE,                
    I_MENU_COUNT_SMALL,               
    I_MENU_COUNT_NORMAL,              
    I_MENU_COUNT_BIG,                 
    I_MENU_COUNT_VERYBIG,             

    I_MENU_GAME_BLOOD_COUNT,          
    I_MENU_GAME_MAX_GIBS,             
    I_MENU_GAME_MAX_CORPSES,          
    I_MENU_GAME_GIBS_COUNT,
    I_MENU_GAME_BLOOD_TYPE,           
    I_MENU_GAME_BLOOD_TYPE_SIMPLE,    
    I_MENU_GAME_BLOOD_TYPE_ADV,       
    I_MENU_GAME_CORPSE_TYPE,          
    I_MENU_GAME_CORPSE_TYPE_SIMPLE,   
    I_MENU_GAME_CORPSE_TYPE_ADV,      
    I_MENU_GAME_GIBS_TYPE,            
    I_MENU_GAME_GIBS_TYPE_SIMPLE,     
    I_MENU_GAME_GIBS_TYPE_ADV,        
    I_MENU_GAME_PARTICLES_COUNT,
    I_MENU_GAME_SCREEN_FLASH,     
    I_MENU_GAME_BACKGROUND,           
    I_MENU_GAME_MESSAGES,             
    I_MENU_GAME_REVERT_PLAYERS,       

    I_MENU_VIDEO_RESOLUTION,          
    I_MENU_VIDEO_BPP,                 
    I_MENU_VIDEO_VSYNC,               
    I_MENU_VIDEO_FILTER_SKY,          
    I_MENU_VIDEO_NEED_RESTART,

    I_MENU_RESOLUTION_SELECT,
    I_MENU_RESOLUTION_CURRENT,
    I_MENU_RESOLUTION_LIST,
    I_MENU_RESOLUTION_FULLSCREEN,
    I_MENU_RESOLUTION_APPLY,

    I_MENU_SOUND_MUSIC_LEVEL,         
    I_MENU_SOUND_SOUND_LEVEL,         
    I_MENU_SOUND_MAX_SIM_SOUNDS,      
    I_MENU_SOUND_INACTIVE_SOUNDS,     
    I_MENU_SOUND_INACTIVE_SOUNDS_ON,  
    I_MENU_SOUND_INACTIVE_SOUNDS_OFF,

    I_MENU_PLAYER_NAME,               
    I_MENU_PLAYER_TEAM,               
    I_MENU_PLAYER_TEAM_RED,           
    I_MENU_PLAYER_TEAM_BLUE,          
    I_MENU_PLAYER_MODEL,              
    I_MENU_PLAYER_RED,                
    I_MENU_PLAYER_GREEN,              
    I_MENU_PLAYER_BLUE,
               
    I_MENU_MODEL_INFO,                
    I_MENU_MODEL_ANIMATION,           
    I_MENU_MODEL_CHANGE_WEAPON,       
    I_MENU_MODEL_ROTATE,              
    I_MENU_MODEL_NAME,                
    I_MENU_MODEL_AUTHOR,              
    I_MENU_MODEL_COMMENT,             
    I_MENU_MODEL_OPTIONS,             
    I_MENU_MODEL_WEAPON,

    I_MENU_LANGUAGE_RUSSIAN,
    I_MENU_LANGUAGE_ENGLISH,

    I_MENU_PAUSE,                     
    I_MENU_YES,                       
    I_MENU_NO,                        
    I_MENU_OK,                        
    I_MENU_FINISH,                    

    I_MENU_END_GAME_PROMT,            
    I_MENU_RESTART_GAME_PROMT,        
    I_MENU_EXIT_PROMT,                
    I_MENU_SET_DEFAULT_PROMT,         
    I_MENU_LOAD_SAVED_PROMT,          

    I_PLAYER_KILL,                    
    I_PLAYER_KILL_EXTRAHARD_1,
    I_PLAYER_KILL_EXTRAHARD_2,
    I_PLAYER_KILL_ACID,
    I_PLAYER_KILL_TRAP,
    I_PLAYER_KILL_FALL,
    I_PLAYER_KILL_SELF,
    I_PLAYER_KILL_WATER,

    I_PLAYER_FLAG_GET,                
    I_PLAYER_FLAG_RETURN,             
    I_PLAYER_FLAG_CAPTURE,            
    I_PLAYER_FLAG_DROP,               
    I_PLAYER_FLAG_RED,                
    I_PLAYER_FLAG_BLUE,               

    I_MESSAGE_FLAG_GET,               
    I_MESSAGE_FLAG_RETURN,            
    I_MESSAGE_FLAG_CAPTURE,           
    I_MESSAGE_FLAG_DROP,              

    I_KEY_UP,                         
    I_KEY_DOWN,                       
    I_KEY_LEFT,                       
    I_KEY_RIGHT,

    I_MONSTER_DEMON,
    I_MONSTER_IMP,
    I_MONSTER_ZOMBIE,
    I_MONSTER_SERGEANT,
    I_MONSTER_CYBER,
    I_MONSTER_CGUN,
    I_MONSTER_BARON,
    I_MONSTER_KNIGHT,
    I_MONSTER_CACODEMON,
    I_MONSTER_SOUL,
    I_MONSTER_PAIN,
    I_MONSTER_MASTERMIND,
    I_MONSTER_SPIDER,
    I_MONSTER_MANCUBUS,
    I_MONSTER_REVENANT,
    I_MONSTER_ARCHVILE,
    I_MONSTER_FISH,
    I_MONSTER_BARREL,
    I_MONSTER_ROBOT,
    I_MONSTER_PRIKOLIST,

    I_LOAD_MUSIC,                     
    I_LOAD_MODELS,                    
    I_LOAD_MENUS,                     
    I_LOAD_CONSOLE,                   
    I_LOAD_ITEMS_DATA,                
    I_LOAD_WEAPONS_DATA,              
    I_LOAD_GAME_DATA,                 
    I_LOAD_COLLIDE_MAP,               
    I_LOAD_DOOR_MAP,                  
    I_LOAD_LIFT_MAP,                  
    I_LOAD_WATER_MAP,                 
    I_LOAD_WAD_FILE,                  
    I_LOAD_MAP,                       
    I_LOAD_TEXTURES,                  
    I_LOAD_TRIGGERS,                  
    I_LOAD_PANELS,                    
    I_LOAD_TRIGGERS_TABLE,            
    I_LOAD_LINK_TRIGGERS,             
    I_LOAD_CREATE_TRIGGERS,           
    I_LOAD_ITEMS,                     
    I_LOAD_CREATE_ITEMS,              
    I_LOAD_AREAS,                     
    I_LOAD_CREATE_AREAS,              
    I_LOAD_MONSTERS,                  
    I_LOAD_CREATE_MONSTERS,           
    I_LOAD_MAP_HEADER,                
    I_LOAD_SKY,                       
    I_LOAD_MONSTER_TEXTURES,          
    I_LOAD_MONSTER_SOUNDS,            
    I_LOAD_SAVE_FILE,                 
    I_LOAD_MAP_STATE,                 
    I_LOAD_ITEMS_STATE,               
    I_LOAD_TRIGGERS_STATE,            
    I_LOAD_WEAPONS_STATE,             
    I_LOAD_MONSTERS_STATE,            

    I_CREDITS_CAP_1,
    I_CREDITS_CAP_2,
    I_CREDITS_A_1,
    I_CREDITS_A_1_1,
    I_CREDITS_A_2,
    I_CREDITS_A_2_1,
    I_CREDITS_A_2_2,
    I_CREDITS_A_2_3,
    I_CREDITS_A_2_4,
    I_CREDITS_A_3,
    I_CREDITS_A_3_1,
    I_CREDITS_A_3_2,
    I_CREDITS_A_4,
    I_CREDITS_A_4_1,
    I_CREDITS_A_4_2,
    I_CREDITS_A_4_3,
    I_CREDITS_A_4_4,
    I_CREDITS_CAP_3,                      
    I_CREDITS_CLO_1,
    I_CREDITS_CLO_2,
    I_CREDITS_CLO_3,
    I_CREDITS_CLO_4,
    I_CREDITS_CLO_5,
    I_CREDITS_CLO_6,

    I_MSG_SHOW_FPS_ON,
    I_MSG_SHOW_FPS_OFF,
    I_MSG_FRIENDLY_FIRE_ON,
    I_MSG_FRIENDLY_FIRE_OFF,
    I_MSG_TIME_ON,
    I_MSG_TIME_OFF,
    I_MSG_SCORE_ON,
    I_MSG_SCORE_OFF,
    I_MSG_STATS_ON,
    I_MSG_STATS_OFF,
    I_MSG_KILL_MSGS_ON,
    I_MSG_KILL_MSGS_OFF,
    I_MSG_NO_MAP,
    I_MSG_NO_MONSTER,
    I_MSG_SCORE_LIMIT,
    I_MSG_TIME_LIMIT,

    I_TEXTURE_ENDPIC,

    I_VERSION,                    

    I_FATAL_ERROR,
    I_SIMPLE_ERROR,
    I_SYSTEM_ERROR_UNKNOWN,
    I_SYSTEM_ERROR_MSG,
    
    I_LAST );

Const
  LANGUAGE_RUSSIAN = 'Russian';
  LANGUAGE_ENGLISH = 'English';
  LANGUAGE_RUSSIAN_N = 3;
  LANGUAGE_ENGLISH_N = 2;

Var
  _lc: Array [TStrings_Locale] of String;
  KilledByMonster: Array [MONSTER_DEMON..MONSTER_MAN] of String;

  
procedure g_Language_Load(fileName: String);
procedure g_Language_Set(lang: String);
procedure g_Language_Dump(fileName: String);


Implementation

Uses
  SysUtils, g_gui, g_basic, e_log;

Const
  g_lang_default: Array [TStrings_Locale] of Array [1..3] of String = (
    ('CONSOLE DUMPED',                 'Saved to "%s"',
                                       'Сохранено в "%s"'),
    ('CONSOLE ERROR WRITE',            'Error writing file "%s"',
                                       'Ошибка при записи в файл "%s"'),
    ('CONSOLE SCREENSHOT',             'Screenshot saved to "%s"',
                                       'Скриншот сохранен в "%s"'),
    ('CONSOLE UNKNOWN',                'Unknown command "%s"',
                                       'Неизвестная команда "%s"'),
    ('CONSOLE WELCOME',                'Welcome to Doom 2D: Forever %s',
                                       'Добро пожаловать в Doom 2D: Forever %s'),

    ('GAME ERROR GET SPAWN',           'Can''t find a spawn point!',
                                       'Не удалось получить точку респауна!'),
    ('GAME ERROR CTF',                 'There is no flags on this map!',
                                       'На карте нет флагов!'),
    ('GAME ERROR MAP WAD',             'Can''t read map WAD "%s"',
                                       'Не удалось загрузить WAD карты: "%s"'),
    ('GAME ERROR MAP RES',             'Can''t load map resource "%s"',
                                       'Не удалось загрузить ресурс карты из WAD: "%s"'),
    ('GAME ERROR MAP LOAD',            'Can''t load map "%s"',
                                       'Не удалось загрузить карту "%s"'),
    ('GAME ERROR MAP SELECT',          'Map Reading Error!',
                                       'Карта не читается!'),
    ('GAME ERROR PLAYER CREATE',       'Can''t create player #%d',
                                       'Не удалось создать Игрока #%d'),
    ('GAME ERROR TEXTURE ANIM',        'Can''t create Animation Texture "%s"',
                                       'Не получилось создать анимированную текстуру "%s"'),
    ('GAME ERROR TEXTURE SIMPLE',      'Can''t create ordinary Texture "%s"',
                                       'Не получилось создать обычную текстуру "%s"'),
    ('GAME ERROR MODEL',               'Model "%s" not found',
                                       'Модель "%s" не найдена'),
    ('GAME ERROR SKY',                 'Can''t load sky "%s"',
                                       'Не удалось загрузить небо "%s"'),
    ('GAME ERROR MUSIC',               'Can''t load music "%s"',
                                       'Не удалось загрузить музыку "%s"'),
    ('GAME ERROR SAVE',                'Saving State Error!',
                                       'Ошибка во время сохранения!'),
    ('GAME ERROR LOAD',                'Loading State Error!',
                                       'Ошибка во время загрузки!'),
    ('GAME ERROR SOUND',               'Can''t load sound "%s"',
                                       'Не удалось загрузить звук "%s"'),
    ('GAME ERROR FRAMES',              'Can''t load animation''s frame list "%s"',
                                       'Не удалось загрузить список кадров анимации: "%s"'),
    ('GAME ERROR TR SOUND',            'Can''t load sound "%s:%s" for trigger',
                                       'Не удалось загрузить звук "%s:%s" для триггера'),
    ('GAME ERROR SWITCH TEXTURE',      'Error texture switching: no Animation',
                                       'Ошибка при переключении текстуры: нет анимации'),

    ('GAME PLAYER NAME',               'Player name',
                                       'Игрок'),
    ('GAME GAME TIME',                 'Game time:',
                                       'Время игры:'),
    ('GAME FRAGS',                     'Frags',
                                       'Фрагов'),
    ('GAME DEATHS',                    'Deaths',
                                       'Смертей'),
    ('GAME DM',                        'DeathMatch',
                                       'МЯСОПОВАЛ'),
    ('GAME CTF',                       'CTF',
                                       'ЗАХВАТ ФЛАГА'),
    ('GAME TDM',                       'TDM',
                                       'КОМАНДНЫЙ МЯСОПОВАЛ'),
    ('GAME FRAG LIMIT',                'Frag Limit: %d',
                                       'ОГРАНИЧЕНИЕ ФРАГОВ: %d'),
    ('GAME SCORE LIMIT',               'Score Limit: %d',
                                       'ОГРАНИЧЕНИЕ ОЧКОВ: %d'),
    ('GAME TIME LIMIT',                'Time Limit: %.2d:%.2d',
                                       'ОГРАНИЧЕНИЕ ПО ВРЕМЕНИ: %.2d:%.2d'),
    ('GAME TEAM SCORE RED',            'Red Team (%d)',
                                       'КРАСНАЯ КОМАНДА (%d)'),
    ('GAME TEAM SCORE BLUE',           'Blue Team (%d)',
                                       'СИНЯЯ КОМАНДА (%d)'),
    ('GAME TEAM RED',                  'Red',
                                       'КРАСНАЯ'),
    ('GAME TEAM BLUE',                 'Blue',
                                       'СИНЯЯ'),
    ('GAME WIN RED',                   'Red Team Wins!',
                                       'ПО РЕЗУЛЬТАТАМ ИГРЫ ПОБЕДИЛА КРАСНАЯ КОМАНДА'),
    ('GAME WIN BLUE',                  'Blue Team Wins!',
                                       'ПО РЕЗУЛЬТАТАМ ИГРЫ ПОБЕДИЛА СИНЯЯ КОМАНДА'),
    ('GAME WIN DRAW',                  'Stalemate!',
                                       'ПО РЕЗУЛЬТАТАМ ИГРЫ ПОБЕДИТЕЛЯ НЕ ВЫЯВЛЕНО'),

    ('MENU START GAME',                'Start Game',
                                       'НАЧАТЬ ИГРУ'),
    ('MENU MAIN MENU',                 'Menu',
                                       'МЕНЮ'),
    ('MENU NEW GAME',                  'New Game',
                                       'НОВАЯ ИГРА'),
    ('MENU MULTIPLAYER',               'Multiplayer',
                                       'МУЛЬТИПЛЕЕР'),
    ('MENU OPTIONS',                   'Options',
                                       'НАСТРОЙКИ'),
    ('MENU AUTHORS',                   'Credits',
                                       'АВТОРЫ'),
    ('MENU EXIT',                      'Exit',
                                       'ВЫХОД'),
    ('MENU 1 PLAYER',                  'Single player',
                                       'ОДИН ИГРОК'),
    ('MENU 2 PLAYERS',                 'Two players',
                                       'ДВА ИГРОКА'),
    ('MENU CUSTOM GAME',               'Custom game',
                                       'СВОЯ ИГРА'),
    ('MENU EPISODE',                   'Episode select',
                                       'ВЫБОР ЭПИЗОДА'),
    ('MENU SELECT MAP',                'Map',
                                       'КАРТА'),
    ('MENU VIDEO OPTIONS',             'Video',
                                       'ВИДЕО'),
    ('MENU SOUND OPTIONS',             'Sound',
                                       'ЗВУК'),
    ('MENU SAVED OPTIONS',             'Saved options',
                                       'СОХРАНЕННЫЕ'),
    ('MENU DEFAULT OPTIONS',           'Default options',
                                       'СТАНДАРТНЫЕ'),
    ('MENU GAME OPTIONS',              'Gameplay',
                                       'ИГРА'),
    ('MENU CONTROLS OPTIONS',          'Controls',
                                       'УПРАВЛЕНИЕ'),
    ('MENU PLAYER OPTIONS',            'Players',
                                       'ИГРОКИ'),
    ('MENU LANGUAGE OPTIONS',          'Language',
                                       'ЯЗЫК'),
    ('MENU LOAD GAME',                 'Load game',
                                       'СТАРАЯ ИГРА'),
    ('MENU SAVE GAME',                 'Save game',
                                       'СОХРАНИТЬ ИГРУ'),
    ('MENU END GAME',                  'End game',
                                       'ЗАКОНЧИТЬ ИГРУ'),
    ('MENU RESTART',                   'Restart game',
                                       'НАЧАТЬ ЗАНОВО'),
    ('MENU SET GAME',                  'Setup game',
                                       'РАЗНОЕ'),

    ('MENU STATISTICS',                'Statistics',
                                       'СТАТИСТИКА ИГРЫ'),
    ('MENU MAP',                       'Map:',
                                       'Карта:'),
    ('MENU GAME TYPE',                 'Game type:',
                                       'Тип игры:'),
    ('MENU GAME TYPE DM',              'DM',
                                       'DM'),
    ('MENU GAME TYPE CTF',             'CTF',
                                       'CTF'),
    ('MENU GAME TYPE TDM',             'TDM',
                                       'TDM'),
    ('MENU GAME TYPE COOP',            'SINGLE / COOP',
                                       'SINGLE / COOP'),
    ('MENU TIME LIMIT',                'Time Limit:',
                                       'Огр. по времени:'),
    ('MENU GOAL LIMIT',                'Score Limit:',
                                       'Огр. по очкам:'),
    ('MENU TEAM DAMAGE',               'Friendly Fire:',
                                       'Урон своим:'),
    ('MENU ENABLE EXITS',              'Enable Exit:',
                                       'Включить выход:'),
    ('MENU WEAPONS STAY',              'Weapons stay:',
                                       'Оружие остается:'),
    ('MENU ENABLE MONSTERS',           'Enable monsters:',
                                       'Монстры:'),
    ('MENU BOTS VS',                   'Bots fight with:',
                                       'Боты против:'),
    ('MENU BOTS VS PLAYERS',           'Players',
                                       'Игроков'),
    ('MENU BOTS VS MONSTERS',          'Monsters',
                                       'Монстров'),
    ('MENU BOTS VS ALL',               'Everybody',
                                       'Всех'),

    ('MENU MAP WAD',                   'Select WAD:',
                                       'Выбор WAD''а:'),
    ('MENU MAP RESOURCE',              'Select Map:',
                                       'Выбор карты:'),
    ('MENU MAP NAME',                  'Map Name:',
                                       'Название:'),
    ('MENU MAP AUTHOR',                'Author:',
                                       'Автор:'),
    ('MENU MAP DESCRIPTION',           'Description:',
                                       'Описание:'),
    ('MENU MAP SIZE',                  'Size:',
                                       'Размер:'),
    ('MENU PLAYERS',                   'Players:',
                                       'Число игроков:'),
    ('MENU PLAYERS ONE',               'One',
                                       'Один'),
    ('MENU PLAYERS TWO',               'Two',
                                       'Два'),

    ('MENU INTER1',                    'Game over',
                                       'ИГРА ЗАКОНЧЕНА'),
    ('MENU INTER2',                    'Level Complete',
                                       'УРОВЕНЬ ПРОЙДЕН'),
    ('MENU INTER3',                    'Time:',
                                       'ВРЕМЯ:'),
    ('MENU INTER4',                    'Kills:',
                                       'УБИЛ:'),
    ('MENU INTER5',                    'Kills-per-minute:',
                                       'УБИЙСТВ В МИНУТУ:'),
    ('MENU INTER6',                    'Secrets found:',
                                       'НАШЕЛ СЕКРЕТОВ:'),
    ('MENU LOADING',                   'Loading...',
                                       'Загрузка...'),
    ('MENU PLAYER 1',                  'Player 1',
                                       'Первый игрок'),
    ('MENU PLAYER 2',                  'Player 2',
                                       'Второй игрок'),

    ('MENU CONTROL GLOBAL',            'Global Controls',
                                       'ОБЩЕЕ УПРАВЛЕНИЕ'),
    ('MENU CONTROL SCREENSHOT',        'Screenshot:',
                                       'Скриншот:'),
    ('MENU CONTROL STAT',              'Statistics:',
                                       'Статистика:'),
    ('MENU CONTROL LEFT',              'Left:',
                                       'Влево:'),
    ('MENU CONTROL RIGHT',             'Right:',
                                       'Вправо:'),
    ('MENU CONTROL UP',                'Up:',
                                       'Вверх:'),
    ('MENU CONTROL DOWN',              'Down:',
                                       'Вниз:'),
    ('MENU CONTROL JUMP',              'Jump:',
                                       'Прыжок:'),
    ('MENU CONTROL FIRE',              'Fire:',
                                       'Огонь:'),
    ('MENU CONTROL USE',               'Use:',
                                       'Использовать:'),
    ('MENU CONTROL NEXT WEAPON',       'Next weapon:',
                                       'След. оружие:'),
    ('MENU CONTROL PREV WEAPON',       'Prev weapon:',
                                       'Пред. оружие:'),

    ('MENU COUNT NONE',                'None',
                                       'Нет'),
    ('MENU COUNT SMALL',               'Little',
                                       'Мало'),
    ('MENU COUNT NORMAL',              'Normal',
                                       'Средне'),
    ('MENU COUNT BIG',                 'Lots',
                                       'Много'),
    ('MENU COUNT VERYBIG',             'Massacre',
                                       'Очень много'),

    ('MENU GAME BLOOD COUNT',          'Blood:',
                                       'Количество крови:'),
    ('MENU GAME MAX GIBS',             'Max. gibs:',
                                       'Макс. кусков:'),
    ('MENU GAME MAX CORPSES',          'Max. corpses:',
                                       'Макс. трупов:'),
    ('MENU GAME GIBS COUNT',           'Gibs count:',
                                       'Кусков за раз:'),
    ('MENU GAME BLOOD TYPE',           'Blood type:',
                                       'Тип крови:'),
    ('MENU GAME BLOOD TYPE SIMPLE',    'Simple',
                                       'Простая'),
    ('MENU GAME BLOOD TYPE ADV',       'Dripping',
                                       'Продвинутая'),
    ('MENU GAME CORPSE TYPE',          'Corpse type:',
                                       'Тип трупов:'),
    ('MENU GAME CORPSE TYPE SIMPLE',   'Simple',
                                       'Простые'),
    ('MENU GAME CORPSE TYPE ADV',      'Interactive',
                                       'Продвинутые'),
    ('MENU GAME GIBS TYPE',            'Gibs type:',
                                       'Тип кусков:'),
    ('MENU GAME GIBS TYPE SIMPLE',     'Simple',
                                       'Простые'),
    ('MENU GAME GIBS TYPE ADV',        'Interactive',
                                       'Продвинутые'),
    ('MENU GAME PARTICLES COUNT',      'Max. Particles:',
                                       'Количество частиц:'),
    ('MENU GAME SCREEN FLASH',         'Screen flash:',
                                       'Вспышки экрана:'),
    ('MENU GAME BACKGROUND',           'Draw Background:',
                                       'Рисовать фон:'),
    ('MENU GAME MESSAGES',             'Show messages:',
                                       'Выводить сообщения:'),
    ('MENU GAME REVERT PLAYERS',       'Revert players:',
                                       'Перевернуть экраны:'),

    ('MENU VIDEO RESOLUTION',          'Set Video Mode',
                                       'Установка видеорежима'),
    ('MENU VIDEO BPP',                 'Bits-per-pixel:',
                                       'Глубина цвета:'),
    ('MENU VIDEO VSYNC',               'VSync',
                                       'Верт. синхронизация:'),
    ('MENU VIDEO FILTER SKY',          'Anisotropic Sky',
                                       'Фильтрация неба:'),
    ('MENU VIDEO NEED RESTART',        'Video settings will be changed after game restart.',
                                       'Данные настройки видео вступят в силу после перезапуска игры.'),

    ('MENU RESOLUTION SELECT',         'SET VIDEO MODE',
                                       'УСТАНОВКА ВИДЕОРЕЖИМА'),
    ('MENU RESOLUTION CURRENT',        'Current:',
                                       'Текущий:'),
    ('MENU RESOLUTION LIST',           'New:',
                                       'Новый:'),
    ('MENU RESOLUTION FULLSCREEN',     'Fullscreen:',
                                       'Полный экран:'),
    ('MENU RESOLUTION APPLY',          'Apply',
                                       'Применить'),

    ('MENU SOUND MUSIC LEVEL',         'Music Volume:',
                                       'Громкость музыки:'),
    ('MENU SOUND SOUND LEVEL',         'Sound Volume:',
                                       'Громкость звука:'),
    ('MENU SOUND MAX SIM SOUNDS',      'One sound count:',
                                       'Кол-во одного звука:'),
    ('MENU SOUND INACTIVE SOUNDS',     'Window inactive:',
                                       'В неактивном:'),
    ('MENU SOUND INACTIVE SOUNDS ON',  'Sounds play',
                                       'Звуки есть'),
    ('MENU SOUND INACTIVE SOUNDS OFF', 'Sounds mute',
                                       'Звуков нет'),

    ('MENU PLAYER NAME',               'Name:',
                                       'Имя:'),
    ('MENU PLAYER TEAM',               'Team:',
                                       'Команда:'),
    ('MENU PLAYER TEAM RED',           'Red',
                                       'Красная'),
    ('MENU PLAYER TEAM BLUE',          'Blue',
                                       'Синяя'),
    ('MENU PLAYER MODEL',              'Model:',
                                       'Модель:'),
    ('MENU PLAYER RED',                'Red:',
                                       'Красный:'),
    ('MENU PLAYER GREEN',              'Green:',
                                       'Зеленый:'),
    ('MENU PLAYER BLUE',               'Blue:',
                                       'Синий:'),

    ('MENU MODEL INFO',                'Model info',
                                       'Информация о модели'),
    ('MENU MODEL ANIMATION',           'Change anim',
                                       'Сменить анимацию'),
    ('MENU MODEL CHANGE WEAPON',       'Change weapon',
                                       'Сменить оружие'),
    ('MENU MODEL ROTATE',              'Reflect model',
                                       'Повернуть модель'),
    ('MENU MODEL NAME',                'Model name:',
                                       'Имя:'),
    ('MENU MODEL AUTHOR',              'Author:',
                                       'Автор:'),
    ('MENU MODEL COMMENT',             'Description:',
                                       'Комментарий:'),
    ('MENU MODEL OPTIONS',             'Model Options:',
                                       'Опции модели:'),
    ('MENU MODEL WEAPON',              'Weapon:',
                                       'Оружие:'),

    ('MENU LANGUAGE RUSSIAN',          'Русский',
                                       'Русский'),
    ('MENU LANGUAGE ENGLISH',          'English',
                                       'English'),

    ('MENU PAUSE',                     'Pause',
                                       'ПАУЗА'),
    ('MENU YES',                       'Yes',
                                       'Да'),
    ('MENU NO',                        'No',
                                       'Нет'),
    ('MENU OK',                        'OK',
                                       'OK'),
    ('MENU FINISH',                    'Done',
                                       'Готово'),

    ('MENU END GAME PROMT',            'End game?',
                                       'Вы действительно хотите закончить игру?'),
    ('MENU RESTART GAME PROMT',        'Restart level?',
                                       'Вы действительно хотите начать уровень заново?'),
    ('MENU EXIT PROMT',                'Chickening out already?',
                                       'Вы действительно хотите выйти из Doom 2D: Forever?'),
    ('MENU SET DEFAULT PROMT',         'Load default settings?',
                                       'Изменить все настройки на стандартные?'),
    ('MENU LOAD SAVED PROMT',          'Load saved settings?',
                                       'Вернуть все настройки на сохраненные?'),

    ('PLAYER KILL',                    '*** %s was killed by %s',
                                       '*** %s был убит %s'),
    ('PLAYER KILL EXTRAHARD 1',        '*** %s was fragged by %s',
                                       '*** %s был разорван на куски %s'),
    ('PLAYER KILL EXTRAHARD 2',        '*** %s was murdered by %s',
                                       '*** %s был зверски убит %s'),
    ('PLAYER KILL ACID',               '*** %s dissolved in acid',
                                       '*** %s утонул в кислоте'),
    ('PLAYER KILL TRAP',               '*** %s got caught in a trap',
                                       '*** %s напоролся на ловушку'),
    ('PLAYER KILL FALL',               '*** %s fell too far',
                                       '*** %s улетел'),
    ('PLAYER KILL SELF',               '*** %s killed himself',
                                       '*** %s убил себя'),
    ('PLAYER KILL WATER',              '*** %s drowned',
                                       '*** %s утонул'),

    ('PLAYER FLAG GET',                '*** %s stole the %s flag!',
                                       '*** %s схватил %s флаг!'),
    ('PLAYER FLAG RETURN',             '*** %s returned the %s flag!',
                                       '*** %s вернул %s флаг!'),
    ('PLAYER FLAG CAPTURE',            '*** %s captured the %s flag! Team scored.',
                                       '*** %s принес %s флаг!'),
    ('PLAYER FLAG DROP',               '*** %s dropped the %s flag!',
                                       '*** %s потерял %s флаг!'),
    ('PLAYER FLAG RED',                'red',
                                       'красный'),
    ('PLAYER FLAG BLUE',               'blue',
                                       'синий'),

    ('MESSAGE FLAG GET',               '%s flag stolen',
                                       '%s ФЛАГ УКРАДЕН'),
    ('MESSAGE FLAG RETURN',            '%s flag returned',
                                       '%s ФЛАГ ВОЗВРАЩЕН'),
    ('MESSAGE FLAG CAPTURE',           '%s flag captured',
                                       '%s ФЛАГ ЗАХВАЧЕН'),
    ('MESSAGE FLAG DROP',              '%s flag dropped',
                                       '%s ФЛАГ ПОТЕРЯН'),

    ('KEY UP',                         'Up',
                                       'Вверх'),
    ('KEY DOWN',                       'Down',
                                       'Вниз'),
    ('KEY LEFT',                       'Left',
                                       'Влево'),
    ('KEY RIGHT',                      'Right',
                                       'Вправо'),

    ('MONSTER DEMON',                  'Pinky',
                                       'Демоном'),
    ('MONSTER IMP',                    'Imp',
                                       'Бесом'),
    ('MONSTER ZOMBIE',                 'Zombie',
                                       'Зомби'),
    ('MONSTER SERGEANT',               'Shotgun Guy',
                                       'Сержантом'),
    ('MONSTER CYBER',                  'Cyberdemon',
                                       'Кибердемоном'),
    ('MONSTER CGUN',                   'Commando',
                                       'Пулеметчиком'),
    ('MONSTER BARON',                  'Hell Baron',
                                       'Бароном ада'),
    ('MONSTER KNIGHT',                 'Hell Knight',
                                       'Рыцарем ада'),
    ('MONSTER CACODEMON',              'Cacodemon',
                                       'Какодемоном'),
    ('MONSTER SOUL',                   'Lost Soul',
                                       'Огненным черепом'),
    ('MONSTER PAIN',                   'Pain Elemental',
                                       'Авиабазой'),
    ('MONSTER MASTERMIND',             'Spider Mastermind',
                                       'Большим пауком'),
    ('MONSTER SPIDER',                 'Arachnotron',
                                       'Арахнотроном'),
    ('MONSTER MANCUBUS',               'Mancubus',
                                       'Манкубусом'),
    ('MONSTER REVENANT',               'Revenant',
                                       'Скелетом'),
    ('MONSTER ARCHVILE',               'Arch-Vile',
                                       'Колдуном'),
    ('MONSTER FISH',                   'Piranha',
                                       'Рыбой'),
    ('MONSTER BARREL',                 'Barrel explosion',
                                       'взрывом Бочки'),
    ('MONSTER ROBOT',                  'Robot',
                                       'Роботом'),
    ('MONSTER PRIKOLIST',              'Prikolist',
                                       'Приколистом'),

    ('LOAD MUSIC',                     'Music',
                                       'Музыка'),
    ('LOAD MODELS',                    'Models',
                                       'Модели'),
    ('LOAD MENUS',                     'Menus',
                                       'Меню'),
    ('LOAD CONSOLE',                   'Console',
                                       'Консоль'),
    ('LOAD ITEMS DATA',                'Items Data',
                                       'Данные предметов'),
    ('LOAD WEAPONS DATA',              'Weapons Data',
                                       'Данные оружия'),
    ('LOAD GAME DATA',                 'Game Data',
                                       'Данные игры'),
    ('LOAD COLLIDE MAP',               'Collide Map',
                                       'Карта столкновений'),
    ('LOAD DOOR MAP',                  'Door Map',
                                       'Карта дверей'),
    ('LOAD LIFT MAP',                  'Lift Map',
                                       'Карта лифтов'),
    ('LOAD WATER MAP',                 'Water Map',
                                       'Карта воды'),
    ('LOAD WAD FILE',                  'WAD File',
                                       'WAD-файл'),
    ('LOAD MAP',                       'Map',
                                       'Карта'),
    ('LOAD TEXTURES',                  'Textures',
                                       'Текстуры'),
    ('LOAD TRIGGERS',                  'Triggers',
                                       'Триггеры'),
    ('LOAD PANELS',                    'Panels',
                                       'Панели'),
    ('LOAD TRIGGERS TABLE',            'Triggers Table',
                                       'Таблица триггеров'),
    ('LOAD LINK TRIGGERS',             'Link Triggers',
                                       'Привязка триггеров'),
    ('LOAD CREATE TRIGGERS',           'Adding of Triggers',
                                       'Добавление триггеров'),
    ('LOAD ITEMS',                     'Items',
                                       'Предметы'),
    ('LOAD CREATE ITEMS',              'Adding of Items',
                                       'Добавление предметов'),
    ('LOAD AREAS',                     'Areas',
                                       'Области'),
    ('LOAD CREATE AREAS',              'Adding of Areas',
                                       'Добавление областей'),
    ('LOAD MONSTERS',                  'Monsters',
                                       'Монстры'),
    ('LOAD CREATE MONSTERS',           'Adding of Monsters',
                                       'Добавление монстров'),
    ('LOAD MAP HEADER',                'Map Description',
                                       'Описание карты'),
    ('LOAD SKY',                       'Background',
                                       'Фон'),
    ('LOAD MONSTER TEXTURES',          'Monsters'' Textures',
                                       'Текстуры монстров'),
    ('LOAD MONSTER SOUNDS',            'Monsters'' Sounds',
                                       'Звуки монстров'),
    ('LOAD SAVE FILE',                 'Save File',
                                       'Файл сохранения'),
    ('LOAD MAP STATE',                 'Map State',
                                       'Настройка карты'),
    ('LOAD ITEMS STATE',               'Items State',
                                       'Расположение предметов'),
    ('LOAD TRIGGERS STATE',            'Triggers State',
                                       'Установка триггеров'),
    ('LOAD WEAPONS STATE',             'Weapons State',
                                       'Расположение оружия'),
    ('LOAD MONSTERS STATE',            'Monsters State',
                                       'Расположение монстров'),

    ('CREDITS CAP 1',                  'Doom 2D: Forever',
                                       'Doom 2D: Forever'),
    ('CREDITS CAP 1',                  'version %s',
                                       'Версия %s'),
    ('CREDITS A 1',                    'Project Author:',
                                       'Автор проекта:'),
    ('CREDITS A 1 1',                  'rs.falcon',
                                       'rs.falcon'),
    ('CREDITS A 2',                    'Programmers:',
                                       'Программисты:'),
    ('CREDITS A 2 1',                  'PSS',
                                       'PSS'),
    ('CREDITS A 2 2',                  'rs.falcon',
                                       'rs.falcon'),
    ('CREDITS A 2 3',                  'PrimuS',
                                       'PrimuS'),
    ('CREDITS A 2 4',                  'OutCast',
                                       'OutCast'),
    ('CREDITS A 3',                    'Artists:',
                                       'Художники:'),
    ('CREDITS A 3 1',                  'Jabberwock',
                                       'Jabberwock'),
    ('CREDITS A 3 2',                  'FireHawK',
                                       'FireHawK'),
    ('CREDITS A 4',                    'Assistants:',
                                       'Ассистенты:'),
    ('CREDITS A 4 1',                  'Jabberwock',
                                       'Jabberwock'),
    ('CREDITS A 4 2',                  'Black Doomer',
                                       'Чёрный Думер'),
    ('CREDITS A 4 3',                  'DEAD',
                                       'DEAD'),
    ('CREDITS A 4 4',                  'kevingpo',
                                       'kevingpo'),
    ('CREDITS CAP 3',                  'Special respect to:',
                                       'Мы их не забудем:'),
    ('CREDITS CLO 1',                  'Prikol Software, who made Doom 2D.',
                                       'Команду Prikol Software, создавшую Doom 2D.'),
    ('CREDITS CLO 2',                  'ID Software, for the great games,',
                                       'Компанию ID Software за те хорошие игры,'),
    ('CREDITS CLO 3',                  'which they used to make.',
                                       'что она умела делать.'),
    ('CREDITS CLO 4',                  'And everyone who helped us on this project.',
                                       'А также всех, кто помогал нашему проекту.'),
    ('CREDITS CLO 5',                  'www.doom2d.org',
                                       'www.doom2d.org'),
    ('CREDITS CLO 6',                  '2003-2014',
                                       '2003-2014'),

    ('MSG SHOW FPS ON',                'FPS are Shown',
                                       'FPS показываются'),
    ('MSG SHOW FPS OFF',               'FPS are Hidden',
                                       'FPS не показываются'),
    ('MSG FRIENDLY FIRE ON',           'Friendly Fire Enabled',
                                       'Урон своим включен'),
    ('MSG FRIENDLY FIRE OFF',          'Friendly Fire Disabled',
                                       'Урона своим нет'),
    ('MSG TIME ON',                    'Time is Shown',
                                       'Время показывается'),
    ('MSG TIME OFF',                   'Time is Hidden',
                                       'Время не показывается'),
    ('MSG SCORE ON',                   'Score is Shown',
                                       'Очки показываются'),
    ('MSG SCORE OFF',                  'Score is Hidden',
                                       'Очки не показываются'),
    ('MSG STATS ON',                   'Statistics is Shown',
                                       'Статистика показывается'),
    ('MSG STATS OFF',                  'Statistics is Hidden',
                                       'Статистика не показывается'),
    ('MSG KILL MSGS ON',               'Death Messages are Shown',
                                       'Сообщения о смерти есть'),
    ('MSG KILL MSGS OFF',              'Death Messages are Hidden',
                                       'Сообщений о смерти нет'),
    ('MSG NO MAP',                     'Map "%s" doesn''t exist!',
                                       'Карта "%s" не найдена!'),
    ('MSG NO MONSTER',                 '"%s" is wrong monster type!',
                                       '"%s" - нет такого монстра!'),
    ('MSG SCORE LIMIT',                'Score Limit is %d',
                                       'Ограничение очков - %d'),
    ('MSG TIME LIMIT',                 'Time Limit is %.2d:%.2d',
                                       'Ограничение времени - %.2d:%.2d'),

    ('TEXTURE ENDPIC',                 'ENDGAME_EN',
                                       'ENDGAME_RU'),

    ('VERSION',                        'Doom 2D: Forever v %s',
                                       'Doom 2D: Forever v %s'),

    ('FATAL ERROR',                    'Fatal error: %s',
                                       'Критическая ошибка: %s'),
    ('SIMPLE ERROR',                   'Error: %s',
                                       'Ошибка: %s'),
    ('SYSTEM ERROR UNKNOWN',           'CRASH! Unknown Error. Address: $%.8x',
                                       'ИГРА УПАЛА! Неизвестная ошибка. Адрес: $%.8x'),
    ('SYSTEM ERROR MSG',               'CRASH! Error: %s',
                                       'ИГРА УПАЛА! Ошибка: %s'),

    ('', '', '') );


procedure SetupArrays();
begin
// Названия клавиш перемещения курсора:
  KEYTABLE[200] := _lc[I_KEY_UP] + ' ' + Chr(30);
  KEYTABLE[203] := _lc[I_KEY_LEFT] + ' ' + Chr(17);
  KEYTABLE[205] := _lc[I_KEY_RIGHT] + ' ' + Chr(16);
  KEYTABLE[208] := _lc[I_KEY_DOWN] + ' ' + Chr(31);

// Имена монстров в творительном падеже:
  KilledByMonster[MONSTER_DEMON] := _lc[I_MONSTER_DEMON];
  KilledByMonster[MONSTER_IMP] := _lc[I_MONSTER_IMP];
  KilledByMonster[MONSTER_ZOMBY] := _lc[I_MONSTER_ZOMBIE];
  KilledByMonster[MONSTER_SERG] := _lc[I_MONSTER_SERGEANT];
  KilledByMonster[MONSTER_CYBER] := _lc[I_MONSTER_CYBER];
  KilledByMonster[MONSTER_CGUN] := _lc[I_MONSTER_CGUN];
  KilledByMonster[MONSTER_BARON] := _lc[I_MONSTER_BARON];
  KilledByMonster[MONSTER_KNIGHT] := _lc[I_MONSTER_KNIGHT];
  KilledByMonster[MONSTER_CACO] := _lc[I_MONSTER_CACODEMON];
  KilledByMonster[MONSTER_SOUL] := _lc[I_MONSTER_SOUL];
  KilledByMonster[MONSTER_PAIN] := _lc[I_MONSTER_PAIN];
  KilledByMonster[MONSTER_SPIDER] := _lc[I_MONSTER_MASTERMIND];
  KilledByMonster[MONSTER_BSP] := _lc[I_MONSTER_SPIDER];
  KilledByMonster[MONSTER_MANCUB] := _lc[I_MONSTER_MANCUBUS];
  KilledByMonster[MONSTER_SKEL] := _lc[I_MONSTER_REVENANT];
  KilledByMonster[MONSTER_VILE] := _lc[I_MONSTER_ARCHVILE];
  KilledByMonster[MONSTER_FISH] := _lc[I_MONSTER_FISH];
  KilledByMonster[MONSTER_BARREL] := _lc[I_MONSTER_BARREL];
  KilledByMonster[MONSTER_ROBO] := _lc[I_MONSTER_ROBOT];
  KilledByMonster[MONSTER_MAN] := _lc[I_MONSTER_PRIKOLIST];
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

          // От слудующего за текущим до последнего:
            if not ok then
            begin
              i := TStrings_Locale(k);

              while i < High(TStrings_Locale) do
              begin
                Inc(i);

                if g_lang_default[i][1] = key then
                begin
                  _lc[i] := value;
                  ok := True;
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
end;

procedure g_Language_Set(lang: String);
var
  i: TStrings_Locale;
  n: Byte;

begin
  if lang = LANGUAGE_RUSSIAN then
    n := LANGUAGE_RUSSIAN_N
  else
    n := LANGUAGE_ENGLISH_N;

  for i := Low(TStrings_Locale) to High(TStrings_Locale) do
    _lc[i] := g_lang_default[i][n];

  SetupArrays();
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
