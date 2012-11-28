/**************************************************************************\
*                                                                          *
*  Форматы карт Doom'а 2D                                       Версия 2   *
*                                                                          *
*  Prikol Software                                           10.VII.1996   *
*                                                                          *
*  Разрешается свободно распространять этот файл, при условии сохранения   *
*  ссылки на Prikol Software, версии и даты выпуска файла.                 *
*                                                                          *
*  Этот include-файл расчитан на WATCOM C 10.0 и DOS4GW                    *
*  Вы можете переделать его на любой другой язык/компилятор, но сохраните  *
*  этот файл (если собираетесь его распространять), а в своем укажите,     *
*  что он изменен (переделан).                                             *
*                                                                          *
\**************************************************************************/

#pragma pack(1)

/* Старый формат - версия 1.04 alpha (и раньше)

  названия текстур (old_wall_t)
    кончается пустой строкой (old_wall_t.n[0]==0)

  фон           - массив 100x100 байт - номера текстур

  тип стенок    - массив 100x100 байт:
    0 - пусто
    1 - стена
    2 - закрытая дверь
    3 - открытая дверь
    4 - ступенька

  передний план - массив 100x100 байт - номера текстур

  вещи, монстры и др. (old_thing_t)
    кончается нулевым типом (old_thing_t.t==0)

  переключатели (old_switch_t)
    кончается нулевым типом (old_switch_t.t==0)

*/

typedef struct{
  char n[8];			// название текстуры
  char t;			// тип: 0-сплошная 1-"решётка"
}old_wall_t;

typedef struct{
  short x,y;			// координаты
  short t;			// тип
  unsigned short f;		// флаги
}old_thing_t;

typedef struct{
  unsigned char x,y;		// координаты/8
  unsigned char t;		// тип
  unsigned char tm;		// должно быть 0
  unsigned char a,b;		// обычно - координаты/8 двери
  unsigned short c;		// не используется (вроде бы)
}old_switch_t;

/* Новый формат - начиная с версии 1.05 alpha

  заголовок карты (map_header_t)

  блоки (map_block_t)
    кончается блоком MB_END (map_block_t.t==MB_END)

*/

typedef struct{
  char id[8];			// "подпись" - "Doom2D\x1A"
  short ver;			// версия карты
}map_header_t;

typedef struct{
  short t;			// тип блока
  short st;			// подтип (метод упаковки, например)
				// если не используется, то должен быть 0
				// (для будущей совместимости)
  int sz;			// размер (сколько байт после этой структуры)
}map_block_t;

enum{
  MB_COMMENT=-1,MB_END=0,
  MB_WALLNAMES,MB_BACK,MB_WTYPE,MB_FRONT,MB_THING,MB_SWITCH,
  MB_MUSIC,MB_SKY,
  MB_SWITCH2,
  MB__UNKNOWN
};

/* Версия 0  (Doom2D версии 1.05 alpha)

  MB_COMMENT - комментарий

  MB_WALLNAMES - названия текстур (см. старую версию)
    количество - по размеру блока

  MB_BACK,MB_WTYPE,MB_FRONT - фон,тип,передний план (см. старую версию)
    подтип 0 - без упаковки (как в старой версии)

  MB_THING - вещи,монстры и др. (см. старую версию)
    количество - по размеру блока

  MB_SWITCH - переключатели (см. старую версию)
    количество - по размеру блока

*/

/* Версия 1  (Doom2D версии 1.06 alpha)

  MB_WALLNAMES
    добавлены псевдо-текстуры _WATER_*
      где * это 0=вода,1=кислота,2=кровь

  MB_WTYPE
    добавлен новый тип 5 - вода

  MB_MUSIC - новый блок - название музыки (8 байт)

  MB_SKY - новый блок - тип неба (2 байта - short)
    1 = облака
    2 = город
    3 = ад

*/

#define SW_PL_PRESS		1
#define SW_MN_PRESS		2
#define SW_PL_NEAR		4
#define SW_MN_NEAR		8
#define SW_KEY_R		16
#define SW_KEY_G		32
#define SW_KEY_B		64

typedef struct{
  unsigned char x,y;		// координаты/8
  unsigned char t;		// тип
  unsigned char tm;		// должно быть 0
  unsigned char a,b;		// обычно - координаты/8 двери
  unsigned short c;		// не используется (вроде бы)
  unsigned char f;		// флаги
}switch2_t;

/* Версия 2  (Doom2D версии 1.17 alpha)

  блок MB_SWITCH заменен на MB_SWITCH2 (см. switch2_t)

*/

#define LAST_MAP_VER 2	// Самая последняя версия карты

#pragma pack()

/*  КОНЕЦ  */
