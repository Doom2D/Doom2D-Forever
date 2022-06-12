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
unit r_common;

interface

  uses r_textures;

  var
    stdfont: TGLFont;
    smallfont: TGLFont;
    menufont: TGLFont;

  procedure r_Common_Load;
  procedure r_Common_Free;

implementation

  uses e_log, r_fonts, g_options;

  function r_Common_LoadFont (const name: AnsiString): TGLFont;
    var info: TFontInfo; skiphack: Integer;
  begin
    result := nil;
    if name = 'STD' then skiphack := 144 else skiphack := 0;
    if r_Font_LoadInfoFromFile(GameWad + ':FONTS/' + name + 'TXT', info) then
      result := r_Textures_LoadFontFromFile(GameWad + ':FONTS/' + name + 'FONT', info, skiphack, true);
    if result = nil then
      e_logwritefln('failed to load font %s', [name]);
  end;

  procedure r_Common_Load;
  begin
    stdfont := r_Common_LoadFont('STD');
    smallfont := r_Common_LoadFont('SMALL');
    menufont := r_Common_LoadFont('MENU');
  end;

  procedure r_Common_Free;
  begin
    menufont.Free;
    smallfont.Free;
    stdfont.Free;
  end;

end.
