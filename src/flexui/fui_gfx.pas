(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE ../shared/a_modes.inc}
{$DEFINE FUI_TEXT_ICONS}
unit fui_gfx;

interface

  uses fui_common;

  type
    TGxContext = class abstract
      public
        type
          TMarkIcon = (Checkbox, Radiobox);
          TWinIcon = (Close);

      public (* abstract interface *)
        procedure line (x1, y1, x2, y2: Integer); virtual; abstract;
        procedure hline (x, y, len: Integer); virtual; abstract;
        procedure vline (x, y, len: Integer); virtual; abstract;
        procedure rect (x, y, w, h: Integer); virtual; abstract;
        procedure fillRect (x, y, w, h: Integer); virtual; abstract;
        procedure darkenRect (x, y, w, h: Integer; a: Integer);  virtual; abstract;

        function charWidth (const ch: AnsiChar): Integer; virtual; abstract;
        function charHeight (const ch: AnsiChar): Integer; virtual; abstract;
        function textWidth (const s: AnsiString): Integer; virtual; abstract;
        function textHeight (const s: AnsiString): Integer; virtual; abstract;
        function drawChar (x, y: Integer; const ch: AnsiChar): Integer; virtual; abstract; // returns char width
        function drawText (x, y: Integer; const s: AnsiString): Integer; virtual; abstract; // returns text width

        procedure resetClip (); virtual; abstract;
        function combineClip (constref aclip: TGxRect): TGxRect; virtual; abstract; // returns previous clip

      protected (* abstract interface *)
        function  getColor (): TGxRGBA; virtual; abstract;
        procedure setColor (const clr: TGxRGBA); virtual; abstract;

        function getFont (): AnsiString; virtual; abstract;
        procedure setFont (const aname: AnsiString); virtual; abstract;

        function  getClipRect (): TGxRect; virtual; abstract;
        procedure setClipRect (const aclip: TGxRect); virtual; abstract;

        procedure onActivate (); virtual; abstract;
        procedure onDeactivate (); virtual; abstract;

        procedure setScale (a: Single); virtual; abstract;

      public (* portable interface *)
        function iconMarkWidth (ic: TMarkIcon): Integer;
        function iconMarkHeight (ic: TMarkIcon): Integer;
        procedure drawIconMark (ic: TMarkIcon; x, y: Integer; marked: Boolean);

        function iconWinWidth (ic: TWinIcon): Integer;
        function iconWinHeight (ic: TWinIcon): Integer;
        procedure drawIconWin (ic: TWinIcon; x, y: Integer; pressed: Boolean);

        procedure drawVSBar (x, y, wdt, hgt: Integer; cur, min, max: Integer; constref clrfull, clrempty: TGxRGBA);
        procedure drawHSBar (x, y, wdt, hgt: Integer; cur, min, max: Integer; constref clrfull, clrempty: TGxRGBA);

        class function sbarFilled (wh: Integer; cur, min, max: Integer): Integer;
        class function sbarPos (cxy: Integer; xy, wh: Integer; min, max: Integer): Integer;

      private
        mActive: Boolean;

      public
        property active: Boolean read mActive;
        property color: TGxRGBA read getColor write setColor;
        property font: AnsiString read getFont write setFont;
        property clip: TGxRect read getClipRect write setClipRect; // clipping is unaffected by offset
    end;

  // set active context; `ctx` can be `nil`
  function gxCreateContext (): TGxContext;
  procedure gxSetContext (ctx: TGxContext; ascale: Single=1.0);
  procedure gxGfxLoadFont (const fontname: AnsiString; const fontFile: AnsiString; proportional: Boolean=false);

  var (* installed by implementation *)
    gxPreSetContextCallback: procedure = nil;
    gxCreateContextCallback: function (): TGxContext = nil;
    gxFuiGfxLoadFontCallback: procedure (const fontname: AnsiString; const fontFile: AnsiString; proportional: Boolean) = nil;

implementation

  uses SysUtils, utils;

  var
    curCtx: TGxContext = nil;

  function gxCreateContext (): TGxContext;
  begin
    result := nil;
    if Assigned(gxCreateContextCallback) then
      result := gxCreateContextCallback();
  end;

  procedure gxSetContext (ctx: TGxContext; ascale: Single=1.0);
  begin
    if Assigned(gxPreSetContextCallback) then
      gxPreSetContextCallback;
    if curCtx <> nil then
    begin
      curCtx.onDeactivate();
      curCtx.mActive := false;
    end;
    curCtx := ctx;
    if ctx <> nil then
    begin
      ctx.mActive := true;
      ctx.onActivate();
      ctx.setScale(ascale);
    end;
  end;

  procedure gxGfxLoadFont (const fontname: AnsiString; const fontFile: AnsiString; proportional: Boolean=false);
  begin
    if Assigned(gxFuiGfxLoadFontCallback) then
      gxFuiGfxLoadFontCallback(fontname, fontFile, proportional)
    else
      raise Exception.Create('FlexUI: hook not installed: font named '''+fontname+''' can not be loaded')
  end;

  function TGxContext.iconMarkWidth (ic: TMarkIcon): Integer;
  begin
    {$IFDEF FUI_TEXT_ICONS}
    case ic of
      TMarkIcon.Checkbox: result := textWidth('[x]');
      TMarkIcon.Radiobox: result := textWidth('(*)');
      else result := textWidth('[x]');
    end;
    {$ELSE}
    result := 11;
    {$ENDIF}
  end;

  function TGxContext.iconMarkHeight (ic: TMarkIcon): Integer;
  begin
    {$IFDEF FUI_TEXT_ICONS}
    case ic of
      TMarkIcon.Checkbox: result := textHeight('[x]');
      TMarkIcon.Radiobox: result := textHeight('(*)');
      else result := textHeight('[x]');
    end;
    {$ELSE}
    result := 8;
    {$ENDIF}
  end;

  procedure TGxContext.drawIconMark (ic: TMarkIcon; x, y: Integer; marked: Boolean);
  var
    {$IFDEF FUI_TEXT_ICONS}
    xstr: AnsiString;
    {$ELSE}
    f: Integer;
    {$ENDIF}
  begin
    if (not self.active) or (self.clip.w < 1) or (self.clip.h < 1) or (self.color.a = 0) then exit;
    {$IFDEF FUI_TEXT_ICONS}
    case ic of
      TMarkIcon.Checkbox: xstr := '[x]';
      TMarkIcon.Radiobox: xstr := '(*)';
      else exit;
    end;
    if (marked) then
    begin
      drawText(x, y, xstr);
    end
    else
    begin
      drawChar(x, y, xstr[1]);
      drawChar(x+textWidth(xstr)-charWidth(xstr[3]), y, xstr[3]);
    end;
    {$ELSE}
    if (ic = TMarkIcon.Checkbox) then
    begin
      vline(x, y, 7);
      vline(x+10, y, 7);
      hline(x+1, y, 1);
      hline(x+1, y+6, 1);
      hline(x+9, y, 1);
      hline(x+9, y+6, 1);
    end
    else
    begin
      vline(x, y+1, 5);
      vline(x+10, y+1, 5);
      hline(x+1, y, 1);
      hline(x+1, y+6, 1);
      hline(x+9, y, 1);
      hline(x+9, y+6, 1);
    end;
    if (not marked) then exit;
    case ic of
      TMarkIcon.Checkbox:
        begin
          for f := 0 to 4 do
          begin
            vline(x+3+f, y+1+f, 1);
            vline(x+7-f, y+1+f, 1);
          end;
        end;
      TMarkIcon.Radiobox:
        begin
          hline(x+4, y+1, 3);
          hline(x+3, y+2, 5);
          hline(x+3, y+3, 5);
          hline(x+3, y+4, 5);
          hline(x+4, y+5, 3);
        end;
    end;
    {$ENDIF}
  end;

  function TGxContext.iconWinWidth (ic: TWinIcon): Integer;
  begin
    {$IFDEF FUI_TEXT_ICONS}
    case ic of
      TWinIcon.Close: result := nmax(textWidth('[x]'), textWidth('[#]'));
      else result := nmax(textWidth('[x]'), textWidth('[#]'));
    end;
    {$ELSE}
    result := 9;
    {$ENDIF}
  end;

  function TGxContext.iconWinHeight (ic: TWinIcon): Integer;
  begin
    {$IFDEF FUI_TEXT_ICONS}
    case ic of
      TWinIcon.Close: result := nmax(textHeight('[x]'), textHeight('[#]'));
      else result := nmax(textHeight('[x]'), textHeight('[#]'));
    end;
    {$ELSE}
    result := 8;
    {$ENDIF}
  end;

  procedure TGxContext.drawIconWin (ic: TWinIcon; x, y: Integer; pressed: Boolean);
  var
    {$IFDEF FUI_TEXT_ICONS}
    xstr: AnsiString;
    wdt: Integer;
    {$ELSE}
    f: Integer;
    {$ENDIF}
  begin
    if (not self.active) or (self.clip.w < 1) or (self.clip.h < 1) or (self.color.a = 0) then exit;
    {$IFDEF FUI_TEXT_ICONS}
    case ic of
      TWinIcon.Close: if (pressed) then xstr := '[#]' else xstr := '[x]';
      else exit;
    end;
    wdt := nmax(textWidth('[x]'), textWidth('[#]'));
    drawChar(x, y, xstr[1]);
    drawChar(x+wdt-charWidth(xstr[3]), y, xstr[3]);
    drawChar(x+((wdt-charWidth(xstr[2])) div 2), y, xstr[2]);
    {$ELSE}
    if pressed then rect(x, y, 9, 8);
    for f := 1 to 5 do
    begin
      vline(x+1+f, y+f, 1);
      vline(x+1+6-f, y+f, 1);
    end;
    {$ENDIF}
  end;

  // vertical scroll bar
  procedure TGxContext.drawVSBar (x, y, wdt, hgt: Integer; cur, min, max: Integer; constref clrfull, clrempty: TGxRGBA);
  var
    filled: Integer;
  begin
    if (wdt < 1) or (hgt < 1) then exit;
    filled := sbarFilled(hgt, cur, min, max);
    color := clrfull;
    fillRect(x, y, wdt, filled);
    color := clrempty;
    fillRect(x, y+filled, wdt, hgt-filled);
  end;

  // horizontal scrollbar
  procedure TGxContext.drawHSBar (x, y, wdt, hgt: Integer; cur, min, max: Integer; constref clrfull, clrempty: TGxRGBA);
  var
    filled: Integer;
  begin
    if (wdt < 1) or (hgt < 1) then exit;
    filled := sbarFilled(wdt, cur, min, max);
    color := clrfull;
    fillRect(x, y, filled, hgt);
    color := clrempty;
    fillRect(x+filled, y, wdt-filled, hgt);
  end;

  class function TGxContext.sbarFilled (wh: Integer; cur, min, max: Integer): Integer;
  begin
         if (wh < 1) then result := 0
    else if (min > max) then result := 0
    else if (min = max) then result := wh
    else
    begin
      if (cur < min) then cur := min else if (cur > max) then cur := max;
      result := wh*(cur-min) div (max-min);
    end;
  end;

  class function TGxContext.sbarPos (cxy: Integer; xy, wh: Integer; min, max: Integer): Integer;
  begin
    if (wh < 1) then begin result := 0; exit; end;
    if (min > max) then begin result := 0; exit; end;
    if (min = max) then begin result := max; exit; end;
    if (cxy < xy) then begin result := min; exit; end;
    if (cxy >= xy+wh) then begin result := max; exit; end;
    result := min+((max-min)*(cxy-xy) div wh);
    assert((result >= min) and (result <= max));
  end;

end.
