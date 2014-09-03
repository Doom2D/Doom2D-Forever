/*************************************************************************\
*                                                                         *
*  Типы предметов в картах Doom'а 2D версии 1.22 alpha                    *
*                                                                         *
*  Prikol Software                                           28.VII.1996  *
*                                                                         *
*  Разрешается свободно распространять этот файл, при условии сохранения  *
*  ссылки на Prikol Software и даты выпуска файла.                        *
*                                                                         *
\*************************************************************************/

// флаги
#define THF_DIR 1			// направление: 1=вправо 0=влево
#define THF_DM 16			// появляется только в DeathMatch'е

enum{TH_NONE=0,
  TH_PLR1,TH_PLR2,TH_DMSTART,
  TH_CLIP=100,TH_SHEL,TH_ROCKET,TH_CELL,TH_AMMO,TH_SBOX,TH_RBOX,TH_CELP,
  TH_STIM,TH_MEDI,TH_BPACK,
  TH_CSAW,TH_SGUN,TH_SGUN2,TH_MGUN,TH_LAUN,TH_PLAS,TH_BFG,
  TH_ARM1,TH_ARM2,TH_MEGA,TH_INVL,TH_AQUA,
  TH_RKEY,TH_GKEY,TH_BKEY,
  TH_SUIT,TH_SUPER,
  TH_RTORCH,TH_GTORCH,TH_BTORCH,
  TH_GOR1,TH_FCAN,
  TH_GUN2,
  TH__LASTI,
  TH_DEMON=200,TH_IMP,TH_ZOMBY,TH_SERG,TH_CYBER,TH_CGUN,
  TH_BARON,TH_KNIGHT,TH_CACO,TH_SOUL,TH_PAIN,TH_SPIDER,TH_BSP,
  TH_MANCUB,TH_SKEL,TH_VILE,TH_FISH,TH_BARREL,TH_ROBO,TH_MAN,
  TH__LASTM
};
