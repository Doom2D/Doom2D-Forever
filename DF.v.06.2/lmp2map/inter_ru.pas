unit inter_ru;

interface

const
  INTER_HELP1 = 'использовать: %s <LMP файл> <MAP файл> [-inboc]';
  INTER_HELP2 = 'например: %s MAP01.lmp MAP01.map -i';
  INTER_HELP3 = '[i] - показать доп. информацию';
  INTER_HELP4 = '[n] - только информация, не конвертировать';
  INTER_HELP5 = '[b] - пакетный режим, не ждать нажатия ENTER';
  INTER_HELP6 = '[o] - делать оптимизацию панелей';
  INTER_HELP7 = '[c] - коррекция монстров\областей';
  INTER_ERROR1 = '^^1!Ошибка: LMP файл ^^3%s ^^1не найден';
  INTER_ERROR2 = '^^1!Ошибка: файл списка текстур ^^3%s ^^1не найден';
  INTER_ERROR3 = '^^1!Ошибка: сигнатура ^^3%s ^^1не найдена';
  INTER_ERROR4 = '^^1!Ошибка: неизвестная версия';
  INTER_ERROR5 = '^^1!Ошибка: текстура ^^3%s ^^1не найдена в ^^3%s';
  INTER_MAPID      = 'ID:     ^2%s';
  INTER_MAPVERSION = 'ВЕРСИЯ: ^2%d';
  INTER_BLOCK = '[БЛОК] тип:^2%d^7[^2%-12s^7] st:^2%d ^7размер:^2%d';
  INTER_COMMENT = ' ^2КОММЕНТАРИЙ: %s';
  INTER_UNKNOWNBLOCK = 'неизвестный тип блока: ^2%d';
  INTER_INFO = '^^2--- ИНФОРМАЦИЯ ---';
  INTER_MUSIC = 'МУЗЫКА: ^2%s';
  INTER_SKY   = 'НЕБО:   ^2%d';
  INTER_WALL = 'СТЕНА: ^7n:^2%-8s ^7t:^2%d';
  INTER_THING = 'ВЕЩЬ: ^7x:^2%-3d ^7y:^2%-3d ^7t:^2%-3d ^7f:^2%-3d';
  INTER_SWITCH = 'ВЫКЛЮЧАТЕЛЬ: x:^2%-3d ^7y:^2%-3d ^7t:^2%-3d ^7tm:^2%-3d '+
                 '^7a:^2%-3d ^7b:^2%-3d ^7c:^2%-3d ^7d:^2%-3d ^7f:^2%-3d';
  INTER_CONVERT = 'конвертирование карты...';
  INTER_OPTIMIZATION = 'оптимизация карты...';
  INTER_CORRECTION = 'коррекция...';
  INTER_SAVEDTO = 'сохранено в ^2%s';

implementation

end.
