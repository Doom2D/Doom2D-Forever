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
unit r_common;

interface

  uses r_textures, r_fonts, g_player, g_phys;

  type
    TBasePoint = (
      BP_LEFTUP,   BP_UP,     BP_RIGHTUP,
      BP_LEFT,     BP_CENTER, BP_RIGHT,
      BP_LEFTDOWN, BP_DOWN,   BP_RIGHTDOWN
    );

    THereTexture = record
      name: AnsiString;
      id: TGLTexture;
    end;

  var
    stdfont: TGLFont;
    smallfont: TGLFont;
    menufont: TGLFont;

  var
    r_Common_ProcessLoadingCallback: TProcedure;

  function  r_Common_LoadThis (const name: AnsiString; var here: THereTexture): Boolean;
  procedure r_Common_FreeThis (var here: THereTexture);

  procedure r_Common_CalcAspect (ow, oh, nw, nh: LongInt; horizontal: Boolean; out ww, hh: LongInt);

  procedure r_Common_GetBasePoint (x, y, w, h: Integer; p: TBasePoint; out xx, yy: Integer);
  procedure r_Common_DrawText (const text: AnsiString; x, y: Integer; r, g, b, a: Byte; f: TGLFont; p: TBasePoint);
  procedure r_Common_DrawTexture (img: TGLTexture; x, y, w, h: Integer; p: TBasePoint);
  procedure r_Common_GetFormatTextSize (const text: AnsiString; f: TGLFont; out w, h: Integer);
  procedure r_Common_DrawFormatText (const text: AnsiString; x, y: Integer; a: Byte; f: TGLFont; p: TBasePoint);

  function r_Common_TimeToStr (t: LongWord): AnsiString;

  procedure r_Common_GetObjectPos (const obj: TObj; out x, y: Integer);
  procedure r_Common_GetPlayerPos (const p: TPlayer; out x, y: Integer);
  procedure r_Common_GetCameraPos (const p: TPlayer; center: Boolean; out x, y: Integer);
  function  r_Common_GetPosByUID (uid: WORD; out obj: TObj; out x, y: Integer): Boolean;

  procedure r_Common_DrawBackgroundImage (img: TGLTexture);
  procedure r_Common_DrawBackground (const name: AnsiString);

  procedure r_Common_ClearLoading;
  procedure r_Common_SetLoading (const text: String; maxval: Integer);
  procedure r_Common_StepLoading (incval: Integer);
  procedure r_Common_DrawLoading (force: Boolean);

  function r_Common_LoadTextureFromFile (const filename: AnsiString; log: Boolean = True): TGLTexture;
  function r_Common_LoadTextureMultiFromFile (const filename: AnsiString; log: Boolean = True): TGLMultiTexture;
  function r_Common_LoadTextureMultiFromFileAndInfo (const filename: AnsiString; w, h, count: Integer; log: Boolean = True): TGLMultiTexture;
  function r_Common_LoadTextureMultiTextFromFile (const filename: AnsiString; var txt: TAnimTextInfo; log: Boolean = True): TGLMultiTexture;
  function r_Common_LoadTextureStreamFromFile (const filename: AnsiString; w, h, count, cw: Integer; st: TGLTextureArray; rs: TRectArray; log: Boolean = True): Boolean;
  function r_Common_LoadTextureFontFromFile (const filename: AnsiString; constref f: TFontInfo; skipch: Integer; log: Boolean = true): TGLFont;

  procedure r_Common_Load;
  procedure r_Common_Free;

implementation

  uses
    Math, SysUtils,
    e_log, utils,
    g_base, g_basic, g_options, g_game, g_map,
    {$IFDEF ENABLE_CORPSES}
      g_corpses,
    {$ENDIF}
    r_draw, r_loadscreen
  ;

  var
    BackgroundTexture: THereTexture;

  procedure r_Common_GetObjectPos (const obj: TObj; out x, y: Integer);
    var fx, fy: Integer;
  begin
    obj.Lerp(gLerpFactor, fx, fy);
    x := fx;
    y := fy + obj.slopeUpLeft;
  end;

  procedure r_Common_GetPlayerPos (const p: TPlayer; out x, y: Integer);
    var fx, fy, fSlope: Integer;
  begin
    ASSERT(p <> nil);
    p.obj.Lerp(gLerpFactor, fx, fy);
    fSlope := nlerp(p.SlopeOld, p.obj.slopeUpLeft, gLerpFactor);
    x := fx;
    y := fy + fSlope;
  end;

{$IFDEF ENABLE_CORPSES}
  function r_Common_GetPlayerCorpse (const p: TPlayer): TCorpse;
  begin
    result := nil;
    if (p <> nil) and (p.Alive = false) and (p.Spectator = false) and (p.Corpse >= 0) then
      if (gCorpses <> nil) and (gCorpses[p.Corpse] <> nil) and (gCorpses[p.Corpse].PlayerUID = p.UID) then
        result := gCorpses[p.Corpse];
  end;
{$ENDIF}

  procedure r_Common_GetCameraPos (const p: TPlayer; center: Boolean; out x, y: Integer);
    {$IFDEF ENABLE_CORPSES}
      var corpse: TCorpse;
    {$ENDIF}
  begin
{$IFDEF ENABLE_CORPSES}
    corpse := r_Common_GetPlayerCorpse(p);
    if corpse <> nil then
    begin
      r_Common_GetObjectPos(corpse.obj, x, y);
      if center then
      begin
        x := x + corpse.obj.rect.width div 2;
        y := y + corpse.obj.rect.height div 2;
      end;
    end
    else
{$ENDIF}
    if p <> nil then
    begin
      r_Common_GetPlayerPos(p, x, y);
      y := y - nlerp(p.IncCamOld, p.IncCam, gLerpFactor);
      if center then
      begin
        x := x + p.obj.rect.width div 2;
        y := y + p.obj.rect.height div 2;
      end;
    end
    else
    begin
      x := 0;
      y := 0;
      if center then
      begin
        x := x + gMapInfo.Width div 2;
        y := y + gMapInfo.Height div 2;
      end;
    end;
  end;

  function r_Common_GetPosByUID (uid: WORD; out obj: TObj; out x, y: Integer): Boolean;
    var p: TPlayer; found: Boolean;
  begin
    found := false;
    if g_GetUIDType(uid) = UID_PLAYER then
    begin
      p := g_Player_Get(uid);
      found := p <> nil;
      if found then
      begin
        r_Common_GetPlayerPos(p, x, y);
        obj := p.obj;
      end;
    end
    else if GetPos(uid, @obj) then
    begin
      found := true;
      r_Common_GetObjectPos(obj, x, y);
    end;
    result := found;
  end;

  procedure r_Common_GetBasePoint (x, y, w, h: Integer; p: TBasePoint; out xx, yy: Integer);
  begin
    case p of
      TBasePoint.BP_LEFTUP,  TBasePoint.BP_LEFT,   TBasePoint.BP_LEFTDOWN:  xx := x;
      TBasePoint.BP_UP,      TBasePoint.BP_CENTER, TBasePoint.BP_DOWN:      xx := x - w div 2;
      TBasePoint.BP_RIGHTUP, TBasePoint.BP_RIGHT,  TBasePoint.BP_RIGHTDOWN: xx := x - w;
    end;
    case p of
      TBasePoint.BP_LEFTUP,   TBasePoint.BP_UP,     TBasePoint.BP_RIGHTUP:   yy := y;
      TBasePoint.BP_LEFT,     TBasePoint.BP_CENTER, TBasePoint.BP_RIGHT:     yy := y - h div 2;
      TBasePoint.BP_LEFTDOWN, TBasePoint.BP_DOWN,   TBasePoint.BP_RIGHTDOWN: yy := y - h;
    end;
  end;

  procedure r_Common_DrawText (const text: AnsiString; x, y: Integer; r, g, b, a: Byte; f: TGLFont; p: TBasePoint);
    var xx, yy, w, h: Integer;
  begin
    xx := x; yy := y;
    if p <> TBasePoint.BP_LEFTUP then
    begin
      r_Draw_GetTextSize(text, f, w, h);
      r_Common_GetBasePoint(x, y, w, h, p, xx, yy);
    end;
    r_Draw_Text(text, xx, yy, r, g, b, a, f);
  end;

  procedure r_Common_DrawTexture (img: TGLTexture; x, y, w, h: Integer; p: TBasePoint);
  begin
    r_Common_GetBasePoint(x, y, w, h, p, x, y);
    r_Draw_TextureRepeat(img, x, y, w, h, false, 255, 255, 255, 255, false);
  end;

  procedure r_Common_GetFormatTextSize (const text: AnsiString; f: TGLFont; out w, h: Integer);
    var i, cw, ch, cln, curw, curh, maxw, maxh: Integer;
  begin
    curw := 0; curh := 0; maxw := 0; maxh := 0;
    r_Draw_GetTextSize('W', f, cw, cln);
    for i := 1 to Length(text) do
    begin
      case text[i] of
        #10:
        begin
          maxw := MAX(maxw, curw);
          curh := curh + cln;
          curw := 0;
        end;
        #1, #2, #3, #4, #18, #19, #20, #21:
        begin
          // skip color modifiers
        end;
        otherwise
        begin
          r_Draw_GetTextSize(text[i], f, cw, ch);
          maxh := MAX(maxh, curh + ch);
          curw := curw + cw;
        end;
      end;
    end;
    w := MAX(maxw, curw);
    h := MAX(maxh, curh);
  end;

  procedure r_Common_DrawFormatText (const text: AnsiString; x, y: Integer; a: Byte; f: TGLFont; p: TBasePoint);
    const
      colors: array [boolean, 0..5] of TRGB = (
        ((R:$00; G:$00; B:$00), (R:$FF; G:$00; B:$00), (R:$00; G:$FF; B:$00), (R:$FF; G:$FF; B:$00), (R:$00; G:$00; B:$FF), (R:$FF; G:$FF; B:$FF)),
        ((R:$00; G:$00; B:$00), (R:$7F; G:$00; B:$00), (R:$00; G:$7F; B:$00), (R:$FF; G:$7F; B:$00), (R:$00; G:$00; B:$7F), (R:$7F; G:$7F; B:$7F))
      );
    var
      i, xx, yy, cx, cy, w, h, cw, ch, cln, color: Integer; dark: Boolean;
  begin
    xx := x; yy := y;
    if p <> TBasePoint.BP_LEFTUP then
    begin
      r_Common_GetFormatTextSize(text, f, w, h);
      r_Common_GetBasePoint(x, y, w, h, p, xx, yy);
    end;
    cx := xx; cy := yy; color := 5; dark := false;
    r_Draw_GetTextSize('W', f, cw, cln);
    for i := 1 to Length(text) do
    begin
      case text[i] of
        #10:
        begin
          cx := xx;
          INC(cy, cln);
        end;
        #1: color := 0;
        #2: color := 5;
        #3: dark := true;
        #4: dark := false;
        #18: color := 1;
        #19: color := 2;
        #20: color := 4;
        #21: color := 3;
        otherwise
        begin
          r_Draw_GetTextSize(text[i], f, cw, ch);
          r_Draw_Text(text[i], cx, cy, colors[dark, color].R, colors[dark, color].G, colors[dark, color].B, a, f);
          INC(cx, cw);
        end;
      end;
    end;
  end;

  function r_Common_TimeToStr (t: LongWord): AnsiString;
    var h, m, s: Integer;
  begin
    h := t div 1000 div 3600;
    m := t div 1000 div 60 mod 60;
    s := t div 1000 mod 60;
    result := Format('%d:%.2d:%.2d', [h, m, s]);
  end;

  (* ---------  --------- *)

  procedure r_Common_FreeThis (var here: THereTexture);
  begin
    here.name := '';
    if here.id <> nil then
      here.id.Free;
    here.id := nil;
  end;

  function r_Common_LoadThis (const name: AnsiString; var here: THereTexture): Boolean;
  begin
    if name <> here.name then
      r_Common_FreeThis(here);
    if (name <> '') and (here.name <> name) then
      here.id := r_Textures_LoadFromFile(name);

    result := here.id <> nil;

    if result then
      here.name := name;
  end;

  procedure r_Common_CalcAspect (ow, oh, nw, nh: LongInt; horizontal: Boolean; out ww, hh: LongInt);
  begin
    if horizontal then
    begin
      ww := nw;
      hh := nw * oh div ow;
    end
    else
    begin
      ww := nh * ow div oh;
      hh := nh;
    end;
  end;

  procedure r_Common_DrawBackgroundImage (img: TGLTexture);
    var fw, w, h: LongInt;
  begin
    if img <> nil then
    begin
      img := BackgroundTexture.id;
      if img.width = img.height then fw := img.width * 4 div 3 else fw := img.width; // fix aspect 4:3
      r_Common_CalcAspect(fw, img.height, gScreenWidth, gScreenHeight, false, w, h);
      r_Draw_Texture(img, gScreenWidth div 2 - w div 2, 0, w, h, false, 255, 255, 255, 255, false);
    end
  end;

  procedure r_Common_DrawBackground (const name: AnsiString);
  begin
    if r_Common_LoadThis(name, BackgroundTexture) then
      r_Common_DrawBackgroundImage(BackgroundTexture.id)
  end;

  function r_Common_LoadFont (const name: AnsiString): TGLFont;
    var info: TFontInfo; skiphack: Integer;
  begin
    result := nil;
    if name = 'STD' then skiphack := 144 else skiphack := 0;
    if r_Font_LoadInfoFromFile(GameWad + ':FONTS/' + name + 'TXT', info) then
      result := r_Common_LoadTextureFontFromFile(GameWad + ':FONTS/' + name + 'FONT', info, skiphack, true);
    if result = nil then
      e_logwritefln('failed to load font %s', [name]);
  end;

  procedure r_Common_Load;
  begin
    r_Common_SetLoading('Fonts', 3);
    menufont := r_Common_LoadFont('MENU');
    smallfont := r_Common_LoadFont('SMALL');
    stdfont := r_Common_LoadFont('STD');
    BackgroundTexture := DEFAULT(THereTexture);
  end;

  procedure r_Common_Free;
  begin
    r_Common_FreeThis(BackgroundTexture);
    menufont.Free;
    smallfont.Free;
    stdfont.Free;
  end;

  (* --------- Loading screen helpers --------- *)

  procedure r_Common_ProcessLoading;
  begin
    if @r_Common_ProcessLoadingCallback <> nil then
      r_Common_ProcessLoadingCallback;
  end;

  procedure r_Common_DrawLoading (force: Boolean);
  begin
    r_LoadScreen_Draw(force);
    r_Common_ProcessLoading;
  end;

  procedure r_Common_ClearLoading;
  begin
    r_LoadScreen_Clear;
    r_Common_DrawLoading(true);
  end;

  procedure r_Common_SetLoading (const text: String; maxval: Integer);
  begin
    r_LoadScreen_Set(text, maxval);
    r_Common_DrawLoading(true);
  end;

  procedure r_Common_StepLoading (incval: Integer);
  begin
    r_LoadScreen_Step(incval);
    r_Common_DrawLoading(false);
  end;

  function r_Common_LoadTextureFromFile (const filename: AnsiString; log: Boolean = True): TGLTexture;
  begin
    result := r_Textures_LoadFromFile(filename, log);
    r_Common_StepLoading(1);
  end;

  function r_Common_LoadTextureMultiFromFile (const filename: AnsiString; log: Boolean = True): TGLMultiTexture;
  begin
    result := r_Textures_LoadMultiFromFile(filename, log);
    r_Common_StepLoading(1);
  end;

  function r_Common_LoadTextureMultiFromFileAndInfo (const filename: AnsiString; w, h, count: Integer; log: Boolean = True): TGLMultiTexture;
  begin
    result := r_Textures_LoadMultiFromFileAndInfo(filename, w, h, count, log);
    r_Common_StepLoading(1);
  end;

  function r_Common_LoadTextureMultiTextFromFile (const filename: AnsiString; var txt: TAnimTextInfo; log: Boolean = True): TGLMultiTexture;
  begin
    result := r_Textures_LoadMultiTextFromFile(filename, txt, log);
    r_Common_StepLoading(1);
  end;

  function r_Common_LoadTextureStreamFromFile (const filename: AnsiString; w, h, count, cw: Integer; st: TGLTextureArray; rs: TRectArray; log: Boolean = True): Boolean;
  begin
    r_Textures_LoadStreamFromFile(filename, w, h, count, cw, st, rs, log);
    r_Common_StepLoading(1);
  end;

  function r_Common_LoadTextureFontFromFile (const filename: AnsiString; constref f: TFontInfo; skipch: Integer; log: Boolean = true): TGLFont;
  begin
    result := r_Textures_LoadFontFromFile (filename, f, skipch, log);
    r_Common_StepLoading(1);
  end;

end.
