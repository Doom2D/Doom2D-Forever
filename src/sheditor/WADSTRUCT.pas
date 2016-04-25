(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$MODE DELPHI}
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
