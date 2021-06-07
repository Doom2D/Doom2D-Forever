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
unit r_console;

interface

  procedure r_Console_Init;
  procedure r_Console_Update;
  procedure r_Console_Draw (MessagesOnly: Boolean = False);

implementation

  uses
    SysUtils, Classes, Math,
    e_log, e_graphics,
    conbuf,
    g_main, g_console, g_game, g_menu, g_textures
  ;

(* ====== Console ====== *)

const
  DEBUG_STRING = 'DEBUG MODE';

var
  ID: DWORD;
  ConsoleTrans: Single;
  ChatTop: BOOLEAN;

  Cons_Y: SmallInt;
  ConsoleHeight: Single;
  ConsoleStep: Single;
  Cons_Shown: Boolean; // draw console

procedure drawConsoleText ();
var
  CWidth, CHeight: Byte;
  ty: Integer;
  sp, ep: LongWord;
  skip: Integer;

  procedure putLine (sp, ep: LongWord);
  var
    p: LongWord;
    wdt, cw: Integer;
  begin
    p := sp;
    wdt := 0;
    while p <> ep do
    begin
      cw := e_TextureFontCharWidth(cbufAt(p), gStdFont);
      if wdt+cw > gScreenWidth-8 then break;
      //e_TextureFontPrintChar(X, Y: Integer; Ch: Char; FontID: DWORD; Shadow: Boolean = False);
      Inc(wdt, cw);
      cbufNext(p);
    end;
    if p <> ep then putLine(p, ep); // do rest of the line first
    // now print our part
    if skip = 0 then
    begin
      ep := p;
      p := sp;
      wdt := 2;
      while p <> ep do
      begin
        cw := e_TextureFontCharWidth(cbufAt(p), gStdFont);
        e_TextureFontPrintCharEx(wdt, ty, cbufAt(p), gStdFont);
        Inc(wdt, cw);
        cbufNext(p);
      end;
      Dec(ty, CHeight);
    end
    else
    begin
      Dec(skip);
    end;
  end;

begin
  e_TextureFontGetSize(gStdFont, CWidth, CHeight);
  ty := Floor(gScreenHeight * ConsoleHeight) - 4 - 2 * CHeight - Abs(Cons_Y);
  skip := conSkipLines;
  cbufLastLine(sp, ep);
  repeat
    putLine(sp, ep);
    if ty+CHeight <= 0 then break;
  until not cbufLineUp(sp, ep);
end;

procedure r_Console_Draw (MessagesOnly: Boolean = False);
var
  CWidth, CHeight: Byte;
  mfW, mfH: Word;
  a, b, offset_y: Integer;
begin
  e_TextureFontGetSize(gStdFont, CWidth, CHeight);

  if ChatTop and gChatShow then
    offset_y := CHeight
  else
    offset_y := 0;

  for a := 0 to High(MsgArray) do
    if MsgArray[a].Time > 0 then
      e_TextureFontPrintFmt(0, offset_y + CHeight * a, MsgArray[a].Msg, gStdFont, True);

  if MessagesOnly then Exit;

  if gChatShow then
  begin
    if ChatTop then
      offset_y := 0
    else
      offset_y := gScreenHeight - CHeight - 1;
    if gChatTeam then
    begin
      e_TextureFontPrintEx(0, offset_y, 'say team> ' + Line, gStdFont, 255, 255, 255, 1, True);
      e_TextureFontPrintEx((CPos + 9) * CWidth, offset_y, '_', gStdFont, 255, 255, 255, 1, True);
    end
    else
    begin
      e_TextureFontPrintEx(0, offset_y, 'say> ' + Line, gStdFont, 255, 255, 255, 1, True);
      e_TextureFontPrintEx((CPos + 4) * CWidth, offset_y, '_', gStdFont, 255, 255, 255, 1, True);
    end
  end;

  if not Cons_Shown then
    Exit;

  if gDebugMode then
  begin
    e_CharFont_GetSize(gMenuFont, DEBUG_STRING, mfW, mfH);
    a := (gScreenWidth - 2*mfW) div 2;
    b := Cons_Y + (Floor(gScreenHeight * ConsoleHeight) - 2 * mfH) div 2;
    e_CharFont_PrintEx(gMenuFont, a div 2, b div 2, DEBUG_STRING,
                       _RGB(128, 0, 0), 2.0);
  end;

  e_DrawSize(ID, 0, Cons_Y, Round(ConsoleTrans * 255), False, False, gScreenWidth, Floor(gScreenHeight * ConsoleHeight));
  e_TextureFontPrint(0, Cons_Y + Floor(gScreenHeight * ConsoleHeight) - CHeight - 4, '> ' + Line, gStdFont);

  drawConsoleText();
  (*
  if ConsoleHistory <> nil then
  begin
    b := 0;
    if CHeight > 0 then
      if Length(ConsoleHistory) > (Floor(gScreenHeight * ConsoleHeight) div CHeight) - 1 then
        b := Length(ConsoleHistory) - (Floor(gScreenHeight * ConsoleHeight) div CHeight) + 1;

    b := Max(b-Offset, 0);
    d := Max(High(ConsoleHistory)-Offset, 0);

    c := 2;
    for a := d downto b do
    begin
      e_TextureFontPrintFmt(0, Floor(gScreenHeight * ConsoleHeight) - 4 - c * CHeight - Abs(Cons_Y), ConsoleHistory[a], gStdFont, True);
      c := c + 1;
    end;
  end;
  *)

  e_TextureFontPrint((CPos + 1) * CWidth, Cons_Y + Floor(gScreenHeight * ConsoleHeight) - 21, '_', gStdFont);
end;

  procedure r_Console_Update;
    var step, miny: Integer;
  begin
    step := Max(1, Round(Floor(gScreenHeight * ConsoleHeight) * ConsoleStep));
    miny := Round(-Floor(gScreenHeight * ConsoleHeight));
    if gConsoleShow then
      Cons_Y := Cons_Y + step
    else
      Cons_Y := Cons_Y - step;
    Cons_Y := Min(Max(Cons_Y, miny), 0);
    Cons_Shown := Cons_Y > miny;
  end;

  procedure r_Console_Init;
    var miny: Integer;
  begin
    g_Texture_CreateWAD(ID, GameWAD + ':TEXTURES\CONSOLE');
    miny := Round(-Floor(gScreenHeight * ConsoleHeight));
    Cons_Y := miny;
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
