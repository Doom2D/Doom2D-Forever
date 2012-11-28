unit inter_ru;

interface

const
  INTER_HELP1 = 'использовать: %s <LST файл> <WAD файл> [-i]';
  INTER_HELP2 = 'например: %s standart.lst Standart.wad';
  INTER_HELP3 = '[i] - показать доп. информацию';
  INTER_LSTERROR = '^^1! Ошибка чтения LST файла';
  INTER_RESERROR = '^^1! Файл ресурса %s не найден';
  INTER_DFWADERROR = '^^1! Ошибка DFWAD %s:';
  INTER_RESINFO = '^^1! FileName=%s ResourceName=%s';
  INTER_ALIASERROR = '^^1! Ошибка псевдонима: оригинальный ресурс %s не найден';
  INTER_FILENAME = 'Файл:   ^2%s';
  INTER_RESNAME  = 'Ресурс: ^2%s';
  INTER_ALIAS = 'Псевдоним для: ^2%s';
  INTER_INFO1 = 'Всего ресурсов в списке: ^2%d';
  INTER_INFO2 = 'Записано ресурсов в WAD: ^2%d';
  INTER_INFO3 = 'Записано секций в WAD:   ^2%d';
  INTER_INFO4 = 'Имя файла:     ^2%s';
  INTER_INFO5 = 'Версия WAD:    ^2%d';
  INTER_INFO6 = 'Записей в WAD: ^2%d';
  INTER_INFO7 = 'Ошибок:	%s';
  INTER_WADCREATED = 'DFWAD создан';

implementation

end.
