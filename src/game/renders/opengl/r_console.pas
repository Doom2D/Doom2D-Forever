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
unit r_console;

interface

  procedure r_Console_Initialize;
  procedure r_Console_Finalize;

  procedure r_Console_Load;
  procedure r_Console_Free;

  procedure r_Console_Update;
  procedure r_Console_Draw (MessagesOnly: Boolean);

implementation

  uses
    Math, SysUtils, utils, conbuf,
    g_game, g_options, g_console, g_language,
    r_draw, r_textures, r_fonts, r_common
  ;

  var
    Background: TGLTexture;

    ConsoleTrans: Single;
    ChatTop: BOOLEAN;

    Cons_Y: SmallInt;
    ConsoleHeight: Single;
    ConsoleStep: Single;
    Cons_Shown: Boolean; // draw console

  procedure r_Console_Initialize;
  begin
    Cons_Y := Round(-Floor(gScreenHeight * ConsoleHeight));
  end;

  procedure r_Console_Finalize;
  begin
  end;

  procedure r_Console_Load;
  begin
    r_Common_SetLoading(_lc[I_LOAD_CONSOLE], 1);
    Background := r_Textures_LoadFromFile(GameWad + ':TEXTURES/CONSOLE');
  end;

  procedure r_Console_Free;
  begin
    r_Common_FreeAndNil(Background);
  end;

  procedure r_Console_Update;
    var step, miny: Integer;
  begin
    step := MAX(1, Round(Floor(gScreenHeight * ConsoleHeight) * ConsoleStep));
    miny := Round(-Floor(gScreenHeight * ConsoleHeight));
    if gConsoleShow then
      Cons_Y := Cons_Y + step
    else
      Cons_Y := Cons_Y - step;
    Cons_Y := MIN(MAX(Cons_Y, miny), 0);
    Cons_Shown := Cons_Y > miny;
  end;

  procedure r_Console_DrawLog;
    var cw, ch, ty, skip: Integer; sp, ep: LongWord;

    procedure PutLine (sp, ep: LongWord);
      var p: LongWord; w, chw: Integer;
    begin
      p := sp; w := 0; chw := 0;
      while (p <> ep) and (w + chw <= gScreenWidth - 8) do
      begin
        chw := stdfont.GetWidth(cbufAt(p));
        INC(w, chw);
        cbufNext(p);
      end;
      if p <> ep then
        PutLine(p, ep);
      if skip = 0 then
      begin
        ep := p; p := sp; w := 2;
        while p <> ep do
        begin
          r_Draw_Text(cbufAt(p), w, ty, 255, 255, 255, 255, stdfont);
          chw := stdfont.GetWidth(cbufAt(p));
          INC(w, chw);
          cbufNext(p);
        end;
        DEC(ty, ch);
      end
      else
      begin
        DEC(skip);
      end;
    end;

  begin
    cw := stdfont.GetMaxWidth();
    ch := stdfont.GetMaxHeight();
    ty := Floor(gScreenHeight * ConsoleHeight) - 4 - 2 * ch - Abs(Cons_Y);
    skip := conSkipLines;
    cbufLastLine(sp, ep);
    repeat
      PutLine(sp, ep);
    until (ty + ch <= 0) or (cbufLineUp(sp, ep) = false);
  end;

  procedure r_Console_Draw (MessagesOnly: Boolean);
    var cw, ch, y, i: Integer; s: AnsiString;
  begin
    cw := stdfont.GetMaxWidth();
    ch := stdfont.GetMaxHeight();

    if ChatTop and gChatShow then y := ch else y := 0;

    for i := 0 to High(MsgArray) do
      if MsgArray[i].time > 0 then
        r_Draw_Text(MsgArray[i].msg, 0, y + ch * i, 255, 255, 255, 255, stdfont);

    if MessagesOnly = false then
    begin
      if gChatShow then
      begin
        if ChatTop then y := 0 else y := gScreenHeight - ch - 1;
        if gChatTeam then s := 'say team> ' else s := 'say> ';
        r_Draw_Text(s + Line, 0, y, 255, 255, 255, 255, stdfont);
        r_Draw_Text('_', (CPos + 4) * cw, y, 255, 255, 255, 255, stdfont);
      end;

      if Cons_Shown then
      begin
        if gDebugMode then
        begin
          // TODO draw debug string
        end;

        if Background <> nil then
          r_Draw_Texture(Background, 0, Cons_Y, gScreenWidth, Floor(gScreenHeight * ConsoleHeight), false, 255, 255, 255, Round((1.0 - ConsoleTrans) * 255), false);

        r_Console_DrawLog;

        r_Draw_Text('> ' + Line, 0, Cons_Y + Floor(gScreenHeight * ConsoleHeight) - ch - 4, 255, 255, 255, 255, stdfont);
        r_Draw_Text('_', (CPos + 1) * cw, Cons_Y + Floor(gScreenHeight * ConsoleHeight) - 21, 255, 255, 255, 255, stdfont);
      end;
    end;
  end;

initialization
  conRegVar('chat_at_top', @ChatTop, 'draw chat at top border', 'draw chat at top border');
  conRegVar('console_height', @ConsoleHeight, 0.0, 1.0, 'set console size', 'set console size');
  conRegVar('console_step', @ConsoleStep, 0.0, 1.0, 'set console animation speed', 'set console animation speed');
  conRegVar('console_trans', @ConsoleTrans, 0.0, 1.0, 'set console transparency', 'set console transparency');
{$IFDEF ANDROID}
  ChatTop := True;
  ConsoleHeight := 0.35;
{$ELSE}
  ChatTop := False;
  ConsoleHeight := 0.5;
{$ENDIF}
  ConsoleStep := 0.07;
  ConsoleTrans := 0.1;
end.
