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
unit r_loadscreen;

interface

  procedure r_LoadScreen_Initialize;
  procedure r_LoadScreen_Finalize;

  procedure r_LoadScreen_Load;
  procedure r_LoadScreen_Free;

  procedure r_LoadScreen_Clear;
  procedure r_LoadScreen_Set (const text: AnsiString; maxval: Integer);
  procedure r_LoadScreen_Step (incval: Integer);
  procedure r_LoadScreen_Draw (force: Boolean);

implementation

  uses
    {$IFDEF USE_GLES1}
      GLES11,
    {$ELSE}
      GL, GLEXT,
    {$ENDIF}
    {$IFDEF ENABLE_SYSTEM}
      g_system,
    {$ENDIF}
    SysUtils, Classes, Math,
    e_log, utils, g_language, g_options, g_console, g_game,
    r_draw, r_textures, r_fonts, r_common, r_console
  ;

  var
    FPSTime: LongWord = 0;
    r_loadscreen_fps: WORD = 0;
    BarL, BarM, BarR, BarF: TGLTexture;
    LoadingScreen: array of record
      msg: AnsiString;
      val, maxval: Integer;
    end = nil;

  procedure r_LoadScreen_Initialize;
  begin
  end;

  procedure r_LoadScreen_Finalize;
  begin
  end;

  procedure r_LoadScreen_Load;
  begin
    BarL := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/LLEFT');
    BarM := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/LMIDDLE');
    BarR := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/LRIGHT');
    BarF := r_Textures_LoadFromFile(GameWAD + ':TEXTURES/LMARKER');
  end;

  procedure r_LoadScreen_Free;
  begin
    r_Common_FreeAndNil(BarL);
    r_Common_FreeAndNil(BarM);
    r_Common_FreeAndNil(BarR);
    r_Common_FreeAndNil(BarF);
  end;

  procedure r_LoadScreen_DrawLoadingBar (x0, x1, y, val, maxval: Integer);
    var l, t, r, b, minw, reqw, midw, fillw, curw, x: Integer;
  begin
    if (BarL <> nil) and (BarM <> nil) and (BarR <> nil) and (BarF <> nil) and (maxval > 0) then
    begin
      minw := BarL.width + BarM.width + BarR.width;
      reqw := x1 - x0;
      if reqw >= minw then
      begin
        midw := (reqw - BarL.width - BarR.width) div BarM.width * BarM.width;
        x := x0 - (reqw - (BarL.width + midw + BarR.width)) div 2;
        fillw := midw + 2;
        curw := MIN(val * fillw div maxval, fillw);
        r_Draw_TextureRepeat(BarL, x, y, BarL.width, BarL.height, false, 255, 255, 255, 255, false);
        r_Draw_TextureRepeat(BarM, x + BarL.width, y, midw, BarM.height, false, 255, 255, 255, 255, false);
        r_Draw_TextureRepeat(BarR, x + BarL.width + midw, y, BarR.width, BarR.height, false, 255, 255, 255, 255, false);
        if curw > 0 then
        begin
          r_Draw_GetRect(l, t, r, b);
          r_Draw_SetRect(x + BarL.width - 1, y, x + BarL.width - 1 + curw - 1, y + BarF.height - 1);
          r_Draw_TextureRepeat(BarF, x + BarL.width - 1, y, curw, BarF.height, false, 255, 255, 255, 255, false);
          r_Draw_SetRect(l, t, r, b);
        end;
      end;
    end;
  end;

  procedure r_LoadScreen_Clear;
  begin
    LoadingScreen := nil;
  end;

  procedure r_LoadScreen_Set (const text: String; maxval: Integer);
    var i: Integer;
  begin
    if LoadingScreen = nil then i := 0 else i := Length(LoadingScreen);
    SetLength(LoadingScreen, i + 1);
    LoadingScreen[i].msg := text;
    LoadingScreen[i].val := 0;
    LoadingScreen[i].maxval := MAX(0, maxval);
  end;

  procedure r_LoadScreen_Step (incval: Integer);
    var i: Integer;
  begin
    if LoadingScreen <> nil then
    begin
      i := HIGH(LoadingScreen);
      INC(LoadingScreen[i].val, MAX(0, incval));
    end;
  end;

  procedure r_LoadScreen_Draw (force: Boolean);
    const LOADING_INTERLINE = 20;
    var xx, yy, hh, i, n: Integer; s: AnsiString; time, delay: LongWord;
  begin
    time := GetTickCount64();
    if r_loadscreen_fps <= 0 then delay := 1000 div GAME_TICK else delay := 1000 div r_loadscreen_fps;
    if force or (time - FPSTime >= delay) then
    begin
      FPSTime := time;

      xx := gScreenWidth div 3;
      yy := gScreenHeight div 3;
      hh := gScreenHeight - yy - 96;
      r_Draw_Setup(gScreenWidth, gScreenHeight);
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      r_Common_DrawBackground(GameWad + ':TEXTURES/INTER');
      r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
      if menufont <> nil then
      begin
        r_Common_DrawText(_lc[I_MENU_LOADING], gScreenWidth div 2, yy - 32, 255, 255, 255, 255, menufont, TBasePoint.BP_DOWN);
      end;
      if (smallfont <> nil) and (LoadingScreen <> nil) then
      begin
        n := hh div LOADING_INTERLINE - 1;
        for i := MAX(0, Length(LoadingScreen) - n) to HIGH(LoadingScreen) do
        begin
          if LoadingScreen[i].maxval > 1 then
            s := LoadingScreen[i].msg + '  ' + IntToStr(LoadingScreen[i].val) + '/' + IntToStr(LoadingScreen[i].maxval)
          else
            s := LoadingScreen[i].msg;
          r_Common_DrawText(s, xx, yy, 255, 0, 0, 255, smallfont, TBasePoint.BP_LEFTUP);
          INC(yy, LOADING_INTERLINE);
        end;
      end;
      if (BarF <> nil) and (LoadingScreen <> nil) then
      begin
        i := HIGH(LoadingScreen);
        if LoadingScreen[i].maxval > 1 then
          r_LoadScreen_DrawLoadingBar(64, gScreenWidth - 64, gScreenHeight - 64, LoadingScreen[i].val, LoadingScreen[i].maxval);
      end;
      if stdfont <> nil then
      begin
        r_Console_Draw(true);
      end;
      sys_Repaint;
    end;
  end;

initialization
  conRegVar('r_loadscreen_fps', @r_loadscreen_fps, '', '');
  r_loadscreen_fps := 0; // auto
end.
