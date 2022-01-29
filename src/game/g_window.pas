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
{$INCLUDE ../shared/a_modes.inc}
unit g_window;

interface

  procedure ProcessLoading (forceUpdate: Boolean=false);

implementation

  uses
    {$IFDEF ENABLE_RENDER}
      r_render,
    {$ENDIF}
    {$IFNDEF HEADLESS}
      g_system,
    {$ENDIF}
    e_sound, g_net
  ;

  procedure ProcessLoading (forceUpdate: Boolean = False);
    var update: Boolean;
  begin
    {$IFDEF HEADLESS}
      update := True;
    {$ELSE}
      update := sys_HandleInput() = False;
    {$ENDIF}
    if update then
    begin
      {$IFDEF ENABLE_RENDER}
        r_Render_DrawLoading(forceUpdate);
      {$ENDIF}
      e_SoundUpdate();
      // TODO: At the moment, I left here only host network processing, because the client code must
      // handle network events on its own. Otherwise separate network cases that use different calls to
      // enet_host_service() WILL lose their packets (for example, resource downloading). So they have
      // to handle everything by themselves. But in general, this MUST be removed completely, since
      // updating the window should never affect the network. Use single enet_host_service(), period.
      if NetMode = NET_SERVER then g_Net_Host_Update();
    end
  end;

end.
