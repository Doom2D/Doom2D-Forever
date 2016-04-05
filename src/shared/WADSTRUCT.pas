unit WADSTRUCT;

{
-----------------------------------
WADSTRUCT.PAS ВЕРСИЯ ОТ 24.09.06

Поддержка вадов версии 1
-----------------------------------

Структура DFWAD-файла версии 1:
 ------------------------------------------
 SIGNATURE  | Byte[5]             | 'DFWAD'
 VERSION    | Byte                | $01
 HEADER     | TWADHeaderRec_1     |
 RESRECORD1 | TResourceTableRec_1 |
 ...        | ................... |
 RESRECORDN | TResourceTableRec_1 |
 DATA       | RAW                 |
 ------------------------------------------
}

interface

type
  Char16 = packed array[0..15] of Char;
    
  TWADHeaderRec_1 = packed record
   RecordsCount: Word;
  end;

  TResourceTableRec_1 = packed record
   ResourceName: Char16;
   Address:      LongWord;
   Length:       LongWord;
  end;

const
  DFWAD_SIGNATURE = 'DFWAD';
  DFWAD_VERSION   = $01;

implementation

end.
