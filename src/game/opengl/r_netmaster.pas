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
unit r_netmaster;

interface

  uses g_netmaster;

  procedure r_Serverlist_Draw (var SL: TNetServerList; var ST: TNetServerTable);

implementation

  uses
    SysUtils, Classes, Math,
    e_graphics,
    g_basic, g_language, g_game, g_menu
  ;

procedure r_Serverlist_Draw (var SL: TNetServerList; var ST: TNetServerTable);
var
  Srv: TNetServer;
  sy, i, y, mw, mx, l, motdh: Integer;
  cw: Byte = 0;
  ch: Byte = 0;
  ww: Word = 0;
  hh: Word = 0;
  ip: AnsiString;
begin
  ip := '';
  sy := 0;

  e_CharFont_GetSize(gMenuFont, _lc[I_NET_SLIST], ww, hh);
  e_CharFont_Print(gMenuFont, (gScreenWidth div 2) - (ww div 2), 16, _lc[I_NET_SLIST]);

  e_TextureFontGetSize(gStdFont, cw, ch);

  ip := _lc[I_NET_SLIST_HELP];
  mw := (Length(ip) * cw) div 2;

  motdh := gScreenHeight - 49 - ch * b_Text_LineCount(slMOTD);

  e_DrawFillQuad(16, 64, gScreenWidth-16, motdh, 64, 64, 64, 110);
  e_DrawQuad(16, 64, gScreenWidth-16, motdh, 255, 127, 0);

  e_TextureFontPrintEx(gScreenWidth div 2 - mw, gScreenHeight-24, ip, gStdFont, 225, 225, 225, 1);

  // MOTD
  if slMOTD <> '' then
  begin
    e_DrawFillQuad(16, motdh, gScreenWidth-16, gScreenHeight-44, 64, 64, 64, 110);
    e_DrawQuad(16, motdh, gScreenWidth-16, gScreenHeight-44, 255, 127, 0);
    e_TextureFontPrintFmt(20, motdh + 3, slMOTD, gStdFont, False, True);
  end;

  // Urgent message
  if not slReadUrgent and (slUrgent <> '') then
  begin
    e_DrawFillQuad(17, 65, gScreenWidth-17, motdh-1, 64, 64, 64, 128);
    e_DrawFillQuad(gScreenWidth div 2 - 256, gScreenHeight div 2 - 60,
      gScreenWidth div 2 + 256, gScreenHeight div 2 + 60, 64, 64, 64, 128);
    e_DrawQuad(gScreenWidth div 2 - 256, gScreenHeight div 2 - 60,
      gScreenWidth div 2 + 256, gScreenHeight div 2 + 60, 255, 127, 0);
    e_DrawLine(1, gScreenWidth div 2 - 256, gScreenHeight div 2 - 40,
      gScreenWidth div 2 + 256, gScreenHeight div 2 - 40, 255, 127, 0);
    l := Length(_lc[I_NET_SLIST_URGENT]) div 2;
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 - 58,
      _lc[I_NET_SLIST_URGENT], gStdFont);
    l := Length(slUrgent) div 2;
    e_TextureFontPrintFmt(gScreenWidth div 2 - 253, gScreenHeight div 2 - 38,
      slUrgent, gStdFont, False, True);
    l := Length(_lc[I_NET_SLIST_URGENT_CONT]) div 2;
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 + 41,
      _lc[I_NET_SLIST_URGENT_CONT], gStdFont);
    e_DrawLine(1, gScreenWidth div 2 - 256, gScreenHeight div 2 + 40,
      gScreenWidth div 2 + 256, gScreenHeight div 2 + 40, 255, 127, 0);
    Exit;
  end;

  if SL = nil then
  begin
    l := Length(slWaitStr) div 2;
    e_DrawFillQuad(17, 65, gScreenWidth-17, motdh-1, 64, 64, 64, 128);
    e_DrawQuad(gScreenWidth div 2 - 192, gScreenHeight div 2 - 10,
      gScreenWidth div 2 + 192, gScreenHeight div 2 + 11, 255, 127, 0);
    e_TextureFontPrint(gScreenWidth div 2 - cw * l, gScreenHeight div 2 - ch div 2,
      slWaitStr, gStdFont);
    Exit;
  end;

  y := 90;
  if (slSelection < Length(ST)) then
  begin
    I := slSelection;
    sy := y + 42 * I - 4;
    Srv := GetServerFromTable(I, SL, ST);
    ip := _lc[I_NET_ADDRESS] + ' ' + Srv.IP + ':' + IntToStr(Srv.Port);
    if Srv.Password then
      ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ' + _lc[I_MENU_YES]
    else
      ip := ip + '  ' + _lc[I_NET_SERVER_PASSWORD] + ' ' + _lc[I_MENU_NO];
  end else
    if Length(ST) > 0 then
      slSelection := 0;

  mw := (gScreenWidth - 188);
  mx := 16 + mw;

  e_DrawFillQuad(16 + 1, sy, gScreenWidth - 16 - 1, sy + 40, 64, 64, 64, 0);
  e_DrawLine(1, 16 + 1, sy, gScreenWidth - 16 - 1, sy, 205, 205, 205);
  e_DrawLine(1, 16 + 1, sy + 41, gScreenWidth - 16 - 1, sy + 41, 255, 255, 255);

  e_DrawLine(1, 16, 85, gScreenWidth - 16, 85, 255, 127, 0);
  e_DrawLine(1, 16, motdh-20, gScreenWidth-16, motdh-20, 255, 127, 0);

  e_DrawLine(1, mx - 70, 64, mx - 70, motdh, 255, 127, 0);
  e_DrawLine(1, mx, 64, mx, motdh-20, 255, 127, 0);
  e_DrawLine(1, mx + 52, 64, mx + 52, motdh-20, 255, 127, 0);
  e_DrawLine(1, mx + 104, 64, mx + 104, motdh-20, 255, 127, 0);

  e_TextureFontPrintEx(18, 68, 'NAME/MAP', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx - 68, 68, 'PING', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx + 2, 68, 'MODE', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx + 54, 68, 'PLRS', gStdFont, 255, 127, 0, 1);
  e_TextureFontPrintEx(mx + 106, 68, 'VER', gStdFont, 255, 127, 0, 1);

  y := 90;
  for I := 0 to High(ST) do
  begin
    Srv := GetServerFromTable(I, SL, ST);
    // Name and map
    e_TextureFontPrintEx(18, y, Srv.Name, gStdFont, 255, 255, 255, 1);
    e_TextureFontPrintEx(18, y + 16, Srv.Map, gStdFont, 210, 210, 210, 1);

    // Ping and similar count
    if (Srv.Ping < 0) or (Srv.Ping > 999) then
      e_TextureFontPrintEx(mx - 68, y, _lc[I_NET_SLIST_NO_ACCESS], gStdFont, 255, 0, 0, 1)
    else
      if Srv.Ping = 0 then
        e_TextureFontPrintEx(mx - 68, y, '<1' + _lc[I_NET_SLIST_PING_MS], gStdFont, 255, 255, 255, 1)
      else
        e_TextureFontPrintEx(mx - 68, y, IntToStr(Srv.Ping) + _lc[I_NET_SLIST_PING_MS], gStdFont, 255, 255, 255, 1);

    if Length(ST[I].Indices) > 1 then
      e_TextureFontPrintEx(mx - 68, y + 16, '< ' + IntToStr(Length(ST[I].Indices)) + ' >', gStdFont, 210, 210, 210, 1);

    // Game mode
    e_TextureFontPrintEx(mx + 2, y, g_Game_ModeToText(Srv.GameMode), gStdFont, 255, 255, 255, 1);

    // Players
    e_TextureFontPrintEx(mx + 54, y, IntToStr(Srv.Players) + '/' + IntToStr(Srv.MaxPlayers), gStdFont, 255, 255, 255, 1);
    e_TextureFontPrintEx(mx + 54, y + 16, IntToStr(Srv.LocalPl) + '+' + IntToStr(Srv.Bots), gStdFont, 210, 210, 210, 1);

    // Version
    e_TextureFontPrintEx(mx + 106, y, IntToStr(Srv.Protocol), gStdFont, 255, 255, 255, 1);

    y := y + 42;
  end;

  e_TextureFontPrintEx(20, motdh-20+3, ip, gStdFont, 205, 205, 205, 1);
  ip := IntToStr(Length(ST)) + _lc[I_NET_SLIST_SERVERS];
  e_TextureFontPrintEx(gScreenWidth - 48 - (Length(ip) + 1)*cw,
    motdh-20+3, ip, gStdFont, 205, 205, 205, 1);
end;

end.
