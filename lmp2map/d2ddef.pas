unit d2ddef;

interface

type
  wall_t = packed record
   n: array[0..7] of Char;
   t: Byte;
  end;

  sw_t = packed record
   x, y: Byte;
   t, tm: Byte;
   a, b, c, d: Byte;
   f: Byte;
  end;

  thing_t = packed record
   x: SmallInt;
   y: SmallInt;
   t: SmallInt;
   f: Word;
  end;

const
  FLDW = 100;
  FLDH = 100;
  MAXTH = 500;
  MAXSW = 100;

  W_CLEAR      = 0;
  W_WALL       = 1;
  W_CLOSEDDOOR = 2;
  W_OPENEDDOOR = 3;
  W_STEP       = 4;
  W_WATER      = 5;
  W_ACID1      = 6;
  W_ACID2      = 7;
  W_BLOCKMON   = 8;
  W_LIFTUP     = 9;
  W_LIFTDOWN   = 10;

  TH_NONE    = 0;
  TH_PLR1    = 1;
  TH_PLR2    = 2;
  TH_DMSTART = 3;
  TH_CLIP   = 100;
  TH_SHEL   = 101;
  TH_ROCKET = 102;
  TH_CELL   = 103;
  TH_AMMO   = 104;
  TH_SBOX   = 105;
  TH_RBOX   = 106;
  TH_CELP   = 107;
  TH_STIM   = 108;
  TH_MEDI   = 109;
  TH_BPACK  = 110;
  TH_CSAW   = 111;
  TH_SGUN   = 112;
  TH_SGUN2  = 113;
  TH_MGUN   = 114;
  TH_LAUN   = 115;
  TH_PLAS   = 116;
  TH_BFG    = 117;
  TH_ARM1   = 118;
  TH_ARM2   = 119;
  TH_MEGA   = 120;
  TH_INVL   = 121;
  TH_AQUA   = 122;
  TH_RKEY   = 123;
  TH_GKEY   = 124;
  TH_BKEY   = 125;
  TH_SUIT   = 126;
  TH_SUPER  = 127;
  TH_RTORCH = 128;
  TH_GTORCH = 129;
  TH_BTORCH = 130;
  TH_GOR1   = 131;
  TH_FCAN   = 132;
  TH_GUN2   = 133;
  TH__LASTI = 134;
  TH_DEMON  = 200;
  TH_IMP    = 201;
  TH_ZOMBY  = 202;
  TH_SERG   = 203;
  TH_CYBER  = 204;
  TH_CGUN   = 205;
  TH_BARON  = 206;
  TH_KNIGHT = 207;
  TH_CACO   = 208;
  TH_SOUL   = 209;
  TH_PAIN   = 210;
  TH_SPIDER = 211;
  TH_BSP    = 212;
  TH_MANCUB = 213;
  TH_SKEL   = 214;
  TH_VILE   = 215;
  TH_FISH   = 216;
  TH_BARREL = 217;
  TH_ROBO   = 218;
  TH_MAN    = 219;
  TH__LASTM = 220;

  THF_DIR = 1;
  THF_DM  = 16;

  SW_NONE     = 0;
  SW_EXIT     = 1;
  SW_EXITS    = 2;
  SW_OPENDOOR = 3;
  SW_SHUTDOOR = 4;
  SW_SHUTTRAP = 5;
  SW_DOOR     = 6;
  SW_DOOR5    = 7;
  SW_PRESS    = 8;
  SW_TELE     = 9;
  SW_SECRET   = 10;
  SW_LIFTUP   = 11;
  SW_LIFTDOWN = 12;
  SW_TRAP     = 13;
  SW_LIFT     = 14;

  SW_PL_PRESS = 1;
  SW_MN_PRESS = 2;
  SW_PL_NEAR  = 4;
  SW_MN_NEAR  = 8;
  SW_KEY_R = 16;
  SW_KEY_G = 32;
  SW_KEY_B = 64;

implementation

end.
