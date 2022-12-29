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

      public
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

        function iconMarkWidth (ic: TMarkIcon): Integer; virtual; abstract;
        function iconMarkHeight (ic: TMarkIcon): Integer; virtual; abstract;
        procedure drawIconMark (ic: TMarkIcon; x, y: Integer; marked: Boolean); virtual; abstract;

        function iconWinWidth (ic: TWinIcon): Integer; virtual; abstract;
        function iconWinHeight (ic: TWinIcon): Integer; virtual; abstract;
        procedure drawIconWin (ic: TWinIcon; x, y: Integer; pressed: Boolean); virtual; abstract;

        procedure resetClip (); virtual; abstract;

//        function setOffset (constref aofs: TGxOfs): TGxOfs; virtual; abstract; // returns previous offset
//        function setClip (constref aclip: TGxRect): TGxRect; virtual; abstract; // returns previous clip

        function combineClip (constref aclip: TGxRect): TGxRect; virtual; abstract; // returns previous clip

        // vertical scrollbar
        procedure drawVSBar (x, y, wdt, hgt: Integer; cur, min, max: Integer; constref clrfull, clrempty: TGxRGBA); virtual; abstract;
        // horizontal scrollbar
        procedure drawHSBar (x, y, wdt, hgt: Integer; cur, min, max: Integer; constref clrfull, clrempty: TGxRGBA); virtual; abstract;

        class function sbarFilled (wh: Integer; cur, min, max: Integer): Integer;
        class function sbarPos (cxy: Integer; xy, wh: Integer; min, max: Integer): Integer;

      protected
        function  getColor (): TGxRGBA; virtual; abstract;
        procedure setColor (const clr: TGxRGBA); virtual; abstract;

        function getFont (): AnsiString; virtual; abstract;
        procedure setFont (const aname: AnsiString); virtual; abstract;

        function  getClipRect (): TGxRect; virtual; abstract;
        procedure setClipRect (const aclip: TGxRect); virtual; abstract;

        procedure onActivate (); virtual; abstract;
        procedure onDeactivate (); virtual; abstract;

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

  uses SysUtils;

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
    end;
  end;

  procedure gxGfxLoadFont (const fontname: AnsiString; const fontFile: AnsiString; proportional: Boolean=false);
  begin
    if Assigned(gxFuiGfxLoadFontCallback) then
      gxFuiGfxLoadFontCallback(fontname, fontFile, proportional)
    else
      raise Exception.Create('FlexUI: hook not installed: font named '''+fontname+''' can not be loaded')
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
