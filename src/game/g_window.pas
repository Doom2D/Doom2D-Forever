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

  procedure g_Game_ClearLoading;
  procedure g_Game_SetLoadingText (const text: String; maxval: Integer; rewrite: Boolean);
  procedure g_Game_StepLoading (value: Integer = -1);

  procedure ProcessLoading (forceUpdate: Boolean=false);

implementation

  uses
    {$IFDEF ENABLE_RENDER}
      r_render,
    {$ENDIF}
    {$IFDEF ENABLE_SYSTEM}
      g_system,
    {$ENDIF}
    {$IFDEF ENABLE_MENU}
      g_gui,
    {$ENDIF}
    e_sound, g_net
  ;

  procedure ProcessLoading (forceUpdate: Boolean = False);
  begin
    {$IFDEF ENABLE_MENU}
      g_ActiveWindow := nil;
    {$ENDIF}
    {$IFDEF ENABLE_RENDER}
      r_Render_DrawLoading(forceUpdate);
    {$ENDIF}
  end;

  procedure g_Game_ClearLoading;
  begin
    {$IFDEF ENABLE_MENU}
      g_ActiveWindow := nil;
    {$ENDIF}
    {$IFDEF ENABLE_RENDER}
      r_Render_ClearLoading;
    {$ENDIF}
  end;

  procedure g_Game_SetLoadingText (const text: String; maxval: Integer; rewrite: Boolean);
  begin
    {$IFDEF ENABLE_MENU}
      g_ActiveWindow := nil;
    {$ENDIF}
    {$IFDEF ENABLE_RENDER}
      if maxval < 0 then maxval := 0;
      r_Render_SetLoading(text, maxval);
    {$ENDIF}
  end;

  procedure g_Game_StepLoading (value: Integer = -1);
  begin
    {$IFDEF ENABLE_MENU}
      g_ActiveWindow := nil;
    {$ENDIF}
    {$IFDEF ENABLE_RENDER}
      if value < 0 then value := 1;
      r_Render_StepLoading(value);
    {$ENDIF}
  end;

end.
