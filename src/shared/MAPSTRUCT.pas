unit MAPSTRUCT;

{
-----------------------------------
MAPSTRUCT.PAS ВЕРСИЯ ОТ 13.11.07

Поддержка карт версии 1
-----------------------------------
}

{
 Карта представляет собою WAD, в котором ресурсы в корне - собственно сами карты
 (MAP01, MAP02 и т.д.).

 Блоки заканчиваются нулевым блоком (BlockType=BLOCK_NONE)

 Структура карты (MAP01, MAP02...):
 --------------------------------------
 SIGNATURE    | Byte[3]         | 'MAP'
 VERSION      | Byte            | $01
 BLOCK1       | TBlock          |
 BLOCK1DATA   | RAW             |
 ...          | ......          |
 BLOCKN       | TBlock          |
 BLOCKNDATA   | RAW             |
 --------------------------------------

 Структура блока:
 --------------------------------------
 BLOCKTYPE    | Byte     | (BLOCK_TEXTURES, BLOCK_PANELS,...)
 RESERVED     | LongWord | $00000000
 BLOCKSIZE    | LongWord | Сколько этот блок в размере (байт после record'а)
 --------------------------------------
}

interface

const
  MAP_SIGNATURE = 'MAP';
  BLOCK_NONE      = 0;
  BLOCK_TEXTURES  = 1;
  BLOCK_PANELS    = 2;
  BLOCK_ITEMS     = 3;
  BLOCK_AREAS     = 4;
  BLOCK_MONSTERS  = 5;
  BLOCK_TRIGGERS  = 6;
  BLOCK_HEADER    = 7;

type
  Char16     = packed array[0..15] of Char;
  Char32     = packed array[0..31] of Char;
  Char64     = packed array[0..63] of Char;
  Char100    = packed array[0..99] of Char;
  Char256    = packed array[0..255] of Char;
  Byte128    = packed array[0..127] of Byte;

  TMapHeaderRec_1 = packed record
   MapName:        Char32;
   MapAuthor:      Char32;
   MapDescription: Char256;
   MusicName:      Char64;
   SkyName:        Char64;
   Width:          Word;
   Height:         Word;
  end;

  TTextureRec_1 = packed record
   Resource: Char64;
   Anim:     Byte;
  end;

  TPanelRec_1 = packed record
   X, Y:       Integer;
   Width,
   Height:     Word;
   TextureNum: Word;
   PanelType:  Word;
   Alpha:      Byte;
   Flags:      Byte;
  end;

  TItemRec_1 = packed record
   X, Y:     Integer;
   ItemType: Byte;
   Options:  Byte;
  end;

  TMonsterRec_1 = packed record
   X, Y:        Integer;
   MonsterType: Byte;
   Direction:   Byte;
  end;

  TAreaRec_1 = packed record
   X, Y:      Integer;
   AreaType:  Byte;
   Direction: Byte;
  end;

  TTriggerRec_1 = packed record
   X, Y:         Integer;
   Width,
   Height:       Word;
   Enabled:      Byte;
   TexturePanel: Integer;
   TriggerType:  Byte;
   ActivateType: Byte;
   Keys:         Byte;
   DATA:         Byte128;
  end;

  TBlock = packed record
   BlockType: Byte;
   Reserved:  LongWord;
   BlockSize: LongWord;
  end;

  TTexturesRec1Array = array of TTextureRec_1;
  TPanelsRec1Array = array of TPanelRec_1;
  TItemsRec1Array = array of TItemRec_1;
  TMonsterRec1Array = array of TMonsterRec_1;
  TAreasRec1Array = array of TAreaRec_1;
  TTriggersRec1Array = array of TTriggerRec_1;

implementation

end.
