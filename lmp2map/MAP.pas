unit MAP;

interface

{
  Форматы карт Doom'а 2D                       Версия 2
  Оригинал - Prikol Software                   10.VII.1996
  Конвертация - rmw.falcon                     24.IX.2006
}

type
 map_header_t = packed record
  id: array [0..7] of Char;
  ver: smallint;
 end;

 map_block_t = packed record
  t: SmallInt;
  st: SmallInt;
  sz: Integer;
 end;

const
  MB_COMMENT   = -1;
  MB_END       = 0;
  MB_WALLNAMES = 1;
  MB_BACK      = 2;
  MB_WTYPE     = 3;
  MB_FRONT     = 4;
  MB_THING     = 5;
  MB_SWITCH    = 6;
  MB_MUSIC     = 7;
  MB_SKY       = 8;
  MB_SWITCH2   = 9;
  MB__UNKNOWN  = 10;

  LAST_MAP_VER = 2;
  SIGNATURE = 'Doom2D'#26;

implementation

end.
