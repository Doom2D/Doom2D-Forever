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
unit r_map;

interface

  uses MAPDEF; // TDFColor

  procedure r_Map_DrawBack (dx, dy: Integer);
  procedure r_Map_DrawPanels (PanelType: Word; hasAmbient: Boolean; constref ambColor: TDFColor); // unaccelerated
  procedure r_Map_CollectDrawPanels (x0, y0, wdt, hgt: Integer);
  procedure r_Map_DrawPanelShadowVolumes (lightX: Integer; lightY: Integer; radius: Integer);
  procedure r_Map_DrawFlags;

implementation

  uses
    {$INCLUDE ../nogl/noGLuses.inc}
    SysUtils, Classes, Math,
    r_graphics,
    g_base, g_basic, g_game, g_options,
    g_panel, g_map,
    r_panel
  ;

procedure dplClear ();
begin
  if (gDrawPanelList = nil) then gDrawPanelList := TBinHeapPanelDraw.Create() else gDrawPanelList.clear();
end;

// old algo
procedure r_Map_DrawPanels (PanelType: Word; hasAmbient: Boolean; constref ambColor: TDFColor);

  procedure DrawPanels (constref panels: TPanelArray; drawDoors: Boolean=False);
  var
    idx: Integer;
  begin
    if (panels <> nil) then
    begin
      // alas, no visible set
      for idx := 0 to High(panels) do
      begin
        if not (drawDoors xor panels[idx].Door) then
          r_Panel_Draw(panels[idx], hasAmbient, ambColor);
      end;
    end;
  end;

begin
  case PanelType of
    PANEL_WALL:       DrawPanels(gWalls);
    PANEL_CLOSEDOOR:  DrawPanels(gWalls, True);
    PANEL_BACK:       DrawPanels(gRenderBackgrounds);
    PANEL_FORE:       DrawPanels(gRenderForegrounds);
    PANEL_WATER:      DrawPanels(gWater);
    PANEL_ACID1:      DrawPanels(gAcid1);
    PANEL_ACID2:      DrawPanels(gAcid2);
    PANEL_STEP:       DrawPanels(gSteps);
  end;
end;

// new algo
procedure r_Map_CollectDrawPanels (x0, y0, wdt, hgt: Integer);
var
  mwit: PPanel;
  it: TPanelGrid.Iter;
begin
  dplClear();
  it := mapGrid.forEachInAABB(x0, y0, wdt, hgt, GridDrawableMask);
  for mwit in it do if (((mwit^.tag and GridTagDoor) <> 0) = mwit^.Door) then gDrawPanelList.insert(mwit^);
  it.release();
  // list will be rendered in `g_game.DrawPlayer()`
end;

procedure r_Map_DrawPanelShadowVolumes (lightX: Integer; lightY: Integer; radius: Integer);
var
  mwit: PPanel;
  it: TPanelGrid.Iter;
begin
  it := mapGrid.forEachInAABB(lightX-radius, lightY-radius, radius*2, radius*2, (GridTagWall or GridTagDoor));
  for mwit in it do r_Panel_DrawShadowVolume(mwit^, lightX, lightY, radius);
  it.release();
end;

procedure r_Map_DrawBack(dx, dy: Integer);
begin
  if gDrawBackGround and (BackID <> DWORD(-1)) then
    e_DrawSize(BackID, dx, dy, 0, False, False, gBackSize.X, gBackSize.Y)
  else
    e_Clear(GL_COLOR_BUFFER_BIT, 0, 0, 0);
end;

procedure r_Map_DrawFlags();
var
  i, dx: Integer;
  tx, ty: Integer;
  Mirror: TMirrorType;
begin
  if gGameSettings.GameMode <> GM_CTF then
    Exit;

  for i := FLAG_RED to FLAG_BLUE do
    with gFlags[i] do
      if State <> FLAG_STATE_CAPTURED then
      begin
        if State = FLAG_STATE_NONE then
          continue;

        Obj.lerp(gLerpFactor, tx, ty);

        if Direction = TDirection.D_LEFT then
          begin
            Mirror := TMirrorType.Horizontal;
            dx := -1;
          end
        else
          begin
            Mirror := TMirrorType.None;
            dx := 1;
          end;

        Animation.Draw(tx + dx, ty + 1, Mirror);

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

end.
