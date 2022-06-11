(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../../../shared/a_modes.inc}
unit r_fonts;

interface

  type
    TFontInfo = record
      w, h: Integer;
      kern: Integer;
      ch: array [AnsiChar] of record
        w: Byte;
      end;
    end;

    TFont = class abstract
    end;

  function r_Font_LoadInfoFromFile (const filename: AnsiString; var f: TFontInfo): Boolean;

implementation

  uses
    Math, SysUtils,
    WADREADER, CONFIG, utils,
    e_log
  ;

  function r_Font_LoadInfoFromMemory (data: Pointer; size: LongInt; var f: TFontInfo): Boolean;
    var cfg: TConfig; c: AnsiChar;
  begin
    result := false;
    if data <> nil then
    begin
      cfg := TConfig.CreateMem(data, size);
      if cfg <> nil then
      begin
        f.w := MIN(MAX(cfg.ReadInt('FontMap', 'CharWidth', 0), 0), 255);
        f.h := MIN(MAX(cfg.ReadInt('FontMap', 'CharHeight', 0), 0), 255);
        f.kern := MIN(MAX(cfg.ReadInt('FontMap', 'Kerning', 0), -128), 127);
        for c := #0 to #255 do
        begin
          f.ch[c].w := MIN(MAX(cfg.ReadInt(IntToStr(ORD(c)), 'Width', 0), 0), 255);
        end;
        result := (f.w > 0) and (f.h > 0);
        cfg.Free;
      end;
    end;
  end;

  function r_Font_LoadInfoFromFile (const filename: AnsiString; var f: TFontInfo): Boolean;
    var wad: TWADFile; wadName, resName: AnsiString; data: Pointer; size: Integer;
  begin
    result := false;
    wadName := g_ExtractWadName(filename);
    wad := TWADFile.Create();
    if wad.ReadFile(wadName) then
    begin
      resName := g_ExtractFilePathName(filename);
      if wad.GetResource(resName, data, size, false) then
      begin
        result := r_Font_LoadInfoFromMemory(data, size, f);
        FreeMem(data);
      end;
      wad.Free;
    end;
  end;  

end.
