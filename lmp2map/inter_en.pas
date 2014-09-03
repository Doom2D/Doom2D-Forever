unit inter_en;

interface

const
  INTER_HELP1 = 'Usage: %s <LMP file> <MAP file> [-in]';
  INTER_HELP2 = 'e.g. %s MAP01.lmp MAP01.map -i';
  INTER_HELP3 = '[i] - show additional info';
  INTER_HELP4 = '[n] - information only, do not convert';
  INTER_HELP5 = '[b] - batch mode';
  INTER_HELP6 = '[o] - perform map optimization';
  INTER_HELP7 = '[c] - monsters\areas correction';
  INTER_ERROR1 = '^^1!Error: LMP file ^^3%s ^^1not found';
  INTER_ERROR2 = '^^1!Error: texture list ^^3%s ^^1not found';
  INTER_ERROR3 = '^^1!Error: label ^^3%s ^^1not found';
  INTER_ERROR4 = '^^1!Error: unknown version';
  INTER_ERROR5 = '^^1!Error: texture ^^3%s ^^1not found in ^^3%s';
  INTER_MAPID      = 'ID:      ^2%s';
  INTER_MAPVERSION = 'VERSION: ^2%d';
  INTER_BLOCK = '[BLOCK] type:^2%d^7[^2%-12s^7] st:^2%d ^7size:^2%d';
  INTER_COMMENT = ' ^2COMMENT: %s';
  INTER_UNKNOWNBLOCK = 'Unknown block type: ^2%d';
  INTER_INFO = '^^2--- INFO ---';
  INTER_MUSIC = 'MUSIC: ^2%s';
  INTER_SKY   = 'SKY:   ^2%d';
  INTER_WALL = 'WALL: ^7n:^2%-8s ^7t:^2%d';
  INTER_THING = 'THING: ^7x:^2%-3d ^7y:^2%-3d ^7t:^2%-3d ^7f:^2%-3d';
  INTER_SWITCH = 'SWITCH: x:^2%-3d ^7y:^2%-3d ^7t:^2%-3d ^7tm:^2%-3d '+
                 '^7a:^2%-3d ^7b:^2%-3d ^7c:^2%-3d ^7d:^2%-3d ^7f:^2%-3d';
  INTER_CONVERT = 'converting...';
  INTER_OPTIMIZATION = 'optimization...';
  INTER_CORRECTION = 'correction...';
  INTER_SAVEDTO = 'saved as ^2%s';

implementation

end.
