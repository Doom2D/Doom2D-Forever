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

  type
    THereTexture = record
      name: AnsiString;
      id: TGLTexture;
    end;

  var
    stdfont: TGLFont;
    smallfont: TGLFont;
    menufont: TGLFont;

  function  r_Common_LoadThis (const name: AnsiString; var here: THereTexture): Boolean;
  procedure r_Common_FreeThis (var here: THereTexture);

  procedure r_Common_CalcAspect (ow, oh, nw, nh: LongInt; horizontal: Boolean; out ww, hh: LongInt);

  procedure r_Common_Load;
  procedure r_Common_Free;

implementation

  uses e_log, r_fonts, g_options;

  procedure r_Common_FreeThis (var here: THereTexture);
  begin
    here.name := '';
    if here.id <> nil then
      here.id.Free;
    here.id := nil;
  end;

  function r_Common_LoadThis (const name: AnsiString; var here: THereTexture): Boolean;
  begin
    if name <> here.name then
      r_Common_FreeThis(here);
    if (name <> '') and (here.name <> name) then
      here.id := r_Textures_LoadFromFile(name);

    result := here.id <> nil;

    if result then
      here.name := name;
  end;

  procedure r_Common_CalcAspect (ow, oh, nw, nh: LongInt; horizontal: Boolean; out ww, hh: LongInt);
  begin
    if horizontal then
    begin
      ww := nw;
      hh := nw * oh div ow;
    end
    else
    begin
      ww := nh * ow div oh;
      hh := nh;
    end;
  end;

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
