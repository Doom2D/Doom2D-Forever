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
{$INCLUDE ../../shared/a_modes.inc}
unit r_items;

interface

  procedure r_Items_Draw;
  procedure r_Items_DrawDrop;

implementation

  uses
    SysUtils, Classes, Math,
    r_graphics, r_animations,
    MAPDEF,
    g_base, g_basic, g_game,
    g_items
  ;

procedure itemsDrawInternal (dropflag: Boolean);
var
  i, fX, fY: Integer;
  it: PItem;
begin
  if (ggItems = nil) then exit;

  for i := 0 to High(ggItems) do
  begin
    it := @ggItems[i];
    if (not it.used) or (it.ItemType = ITEM_NONE) then continue; // just in case
    if not it.alive then continue;
    if (it.dropped <> dropflag) then continue;

    with it^ do
    begin
      if g_Collide(Obj.X, Obj.Y, Obj.Rect.Width, Obj.Rect.Height, sX, sY, sWidth, sHeight) then
      begin
        Obj.lerp(gLerpFactor, fX, fY);
        if (Animation = nil) then
        begin
          e_Draw(gItemsTexturesID[ItemType], fX, fY, 0, true, false);
        end
        else
        begin
          r_Animation_Draw(Animation, fX, fY, TMirrorType.None);
        end;

        if g_debug_Frames then
        begin
          e_DrawQuad(Obj.X+Obj.Rect.X,
                     Obj.Y+Obj.Rect.Y,
                     Obj.X+Obj.Rect.X+Obj.Rect.Width-1,
                     Obj.Y+Obj.Rect.Y+Obj.Rect.Height-1,
                     0, 255, 0);
        end;
      end;
    end;
  end;
end;

procedure r_Items_Draw;
begin
  itemsDrawInternal(false);
end;

procedure r_Items_DrawDrop;
begin
  itemsDrawInternal(true);
end;

end.
