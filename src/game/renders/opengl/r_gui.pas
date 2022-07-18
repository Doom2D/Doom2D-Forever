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
unit r_gui;

interface

  uses g_gui;

  procedure r_GUI_Load;
  procedure r_GUI_Free;

  procedure r_GUI_GetSize (ctrl: TGUIControl; out w, h: Integer);
  procedure r_GUI_GetLogoSize (out w, h: Integer);
  procedure r_GUI_GetMaxFontSize (BigFont: Boolean; out w, h: Integer);
  procedure r_GUI_GetStringSize (BigFont: Boolean; str: String; out w, h: Integer);

  procedure r_GUI_Draw_Window (win: TGUIWindow);

implementation

  uses
    Classes, Math, SysUtils,
    MAPDEF, utils,
    g_basic, g_base, e_input, g_options,
    r_draw, r_textures, r_common, r_map,
    g_game, g_menu
  ;

  const
    EDIT_CURSORLEN = 10;

  var
    Box: Array [0..8] of TGLTexture;
    MarkerID: array [Boolean] of TGLTexture;
    ScrollLeft, ScrollRight, ScrollMiddle, ScrollMarker: TGLTexture;
    EditLeft, EditRight, EditMiddle: TGLTexture;
    BScrollUp, BScrollDown: array [Boolean] of TGLTexture;
    BScrollMiddle: TGLTexture;

    Font: array [boolean] of TGLFont; (* Small[FALSE] / Big[TRUE] *)
    LogoTex: TGLTexture;
    nopic: TGLTexture;

    Background: THereTexture;
    ImageControl: THereTexture;

  procedure r_GUI_Load;
    var i: Integer;
  begin
    Font[FALSE] := smallfont;
    Font[TRUE] := menufont;

    MarkerID[FALSE] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/MARKER1');
    MarkerID[TRUE] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/MARKER2');

    for i := 0 to 8 do
      Box[i] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/BOX' + IntToStr(i + 1));

    ScrollLeft := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SLEFT');
    ScrollRight := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SRIGHT');
    ScrollMiddle := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SMIDDLE');
    ScrollMarker := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SMARKER');

    EditLeft := r_Textures_LoadFromFile(GameWad + ':TEXTURES/ELEFT');
    EditRight := r_Textures_LoadFromFile(GameWad + ':TEXTURES/ERIGHT');
    EditMiddle := r_Textures_LoadFromFile(GameWad + ':TEXTURES/EMIDDLE');

    BScrollUp[true] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SCROLLUPA');
    BScrollUp[false] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SCROLLUPU');
    BScrollDown[true] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SCROLLDOWNA');
    BScrollDown[false] := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SCROLLDOWNU');
    BScrollMiddle := r_Textures_LoadFromFile(GameWad + ':TEXTURES/SCROLLMIDDLE');

    LogoTex := r_Textures_LoadFromFile(GameWad + ':TEXTURES/MAINLOGO');
    nopic := r_Textures_LoadFromFile(GameWad + ':TEXTURES/NOPIC');
  end;

  procedure r_GUI_Free;
    var i: Integer;
  begin
    Font[FALSE] := nil;
    Font[TRUE] := nil;

    MarkerID[FALSE].Free;
    MarkerID[TRUE].Free;

    for i := 0 to 8 do
      Box[i].Free;

    ScrollLeft.Free;
    ScrollRight.Free;
    ScrollMiddle.Free;
    ScrollMarker.Free;

    EditLeft.Free;
    EditRight.Free;
    EditMiddle.Free;

    BScrollUp[true].Free;
    BScrollUp[false].Free;
    BScrollDown[true].Free;
    BScrollDown[false].Free;
    BScrollMiddle.Free;

    LogoTex.Free;
    nopic.Free;

    r_Common_FreeThis(Background);
    r_Common_FreeThis(ImageControl);
  end;

  procedure r_GUI_GetMaxFontSize (BigFont: Boolean; out w, h: Integer);
    var f: TGLFont;
  begin
    f := Font[BigFont];
    w := f.GetMaxWidth();
    h := f.GetMaxHeight();
  end;

  procedure r_GUI_GetStringSize (BigFont: Boolean; str: String; out w, h: Integer);
  begin
    r_Draw_GetTextSize(str, Font[BigFont], w, h);
  end;

  procedure r_GUI_GetLogoSize (out w, h: Integer);
  begin
    w := 0; h := 0;
    if LogoTex <> nil then
    begin
      w := LogoTex.width;
      h := LogoTex.height;
    end;
  end;

  procedure r_GUI_GetSize_TextButton (ctrl: TGUITextButton; out w, h: Integer);
  begin
    r_Draw_GetTextSize(ctrl.Caption, Font[ctrl.BigFont], w, h);
  end;

  procedure r_GUI_GetSize_Label (ctrl: TGUILabel; out w, h: Integer);
    var f: TGLFont;
  begin
    f := Font[ctrl.BigFont];
    r_Draw_GetTextSize(ctrl.Text, f, w, h);
    if ctrl.FixedLength <> 0 then
      w := f.GetMaxWidth() * ctrl.FixedLength;
  end;

  procedure r_GUI_GetSize_Switch (ctrl: TGUISwitch; out w, h: Integer);
    var i: Integer;
  begin
    w := 0; h := 0;
    if ctrl.Items <> nil then
      for i := 0 to High(ctrl.Items) do
        r_Draw_GetTextSize(ctrl.Items[i], Font[ctrl.BigFont], w, h);
  end;

  procedure r_GUI_GetSize_KeyRead (ctrl: TGUIKeyRead; out w, h: Integer);
    var i, ww, hh: Integer; f: TGLFont;
  begin
    w := 0; h := 0; // ??? h always 0
    f := Font[ctrl.BigFont];
    for i := 0 to 255 do
    begin
      r_Draw_GetTextSize(e_KeyNames[i], f, ww, hh);
      w := MAX(w, ww);
    end;
    r_Draw_GetTextSize(KEYREAD_QUERY, f, ww, hh);
    w := MAX(w, ww);
    r_Draw_GetTextSize(KEYREAD_CLEAR, f, ww, hh);
    w := MAX(w, ww);
  end;

  procedure r_GUI_GetSize (ctrl: TGUIControl; out w, h: Integer);
  begin
    w := 0;
    h := 0;
    if ctrl is TGUITextButton then
      r_GUI_GetSize_TextButton(ctrl as TGUITextButton, w, h)
    else if ctrl is TGUILabel then
      r_GUI_GetSize_Label(ctrl as TGUILabel, w, h)
    else if ctrl is TGUIScroll then
      w := 16 + ((ctrl as TGUIScroll).Max + 1) * 8 // ??? but h = 0
    else if ctrl is TGUISwitch then
      r_GUI_GetSize_Switch(ctrl as TGUISwitch, w, h)
    else if ctrl is TGUIEdit then
      w := 16 + (ctrl as TGUIEdit).Width * 16 // ??? but h = 0
    else if ctrl is TGUIKeyRead then
      r_GUI_GetSize_KeyRead(ctrl as TGUIKeyRead, w, h)
    else if ctrl is TGUIKeyRead2 then
      w := (ctrl as TGUIKeyRead2).MaxKeyNameWdt * 2 + 8 + 8 + 16 // ??? but h = 0
    else if ctrl is TGUIListBox then
    begin
      w := 8 + ((ctrl as TGUIListBox).Width + 1) * 16; // recheck w & h
      h := 8 + (ctrl as TGUIListBox).Height * 16;
    end
    else if ctrl is TGUIMemo then
    begin
      w := 8 + ((ctrl as TGUIMemo).Width + 1) * 16;
      h := 8 + (ctrl as TGUIMemo).Height * 16;
    end
    else
    begin
      w := ctrl.GetWidth();
      h := ctrl.GetHeight();
    end;
  end;

  procedure r_GUI_Draw_Control (ctrl: TGUIControl); forward;

  procedure r_GUI_Draw_TextButton (ctrl: TGUITextButton);
  begin
    r_Draw_Text(ctrl.Caption, ctrl.x, ctrl.y, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B, 255, Font[ctrl.BigFont]);
  end;

  procedure r_GUI_Draw_Label (ctrl: TGUILabel);
    var w, h: Integer; f: TGLFont;
  begin
    f := Font[ctrl.BigFont];
    if ctrl.RightAlign then
    begin
      r_Draw_GetTextSize(ctrl.Text, f, w, h);
      r_Draw_Text(ctrl.Text, ctrl.X + ctrl.CMaxWidth - w, ctrl.Y, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B, 255, f);
    end
    else
      r_Draw_Text(ctrl.Text, ctrl.X, ctrl.Y, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B, 255, f);
  end;

  procedure r_GUI_Draw_Scroll (ctrl: TGUIScroll);
  begin
    r_Draw_Texture(ScrollLeft, ctrl.X, ctrl.Y, ScrollLeft.width, ScrollLeft.height, false, 255, 255, 255, 255, false);
    r_Draw_TextureRepeat(ScrollMiddle, ctrl.X + 8 + 0 * 8, ctrl.Y, 8 + ctrl.Max * 8, ScrollMiddle.height, false, 255, 255, 255, 255, false);
    r_Draw_Texture(ScrollRight, ctrl.X + 8 + (ctrl.Max + 1) * 8, ctrl.Y, ScrollRight.width, ScrollRight.height, false, 255, 255, 255, 255, false);
    r_Draw_Texture(ScrollMarker, ctrl.X + 8 + ctrl.Value * 8, ctrl.Y, ScrollMarker.width, ScrollMarker.height, false, 255, 255, 255, 255, false);
  end;

  procedure r_GUI_Draw_Switch (ctrl: TGUISwitch);
  begin
    r_Draw_Text(ctrl.Items[ctrl.ItemIndex], ctrl.X, ctrl.Y, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B, 255, Font[ctrl.BigFont]);
  end;

  procedure r_GUI_Draw_Edit (ctrl: TGUIEdit);
    var w, h: Integer; r, g, b: Byte; f: TGLFont;
  begin
    r_Draw_Texture(EditLeft, ctrl.X, ctrl.Y, EditLeft.width, EditLeft.height, false, 255, 255, 255, 255, false);
    r_Draw_TextureRepeat(EditMiddle, ctrl.X + 8, ctrl.Y, 8 + (ctrl.Width - 1) * 16, EditMiddle.height, false, 255, 255, 255, 255, false);
    r_Draw_Texture(EditRight, ctrl.X + 8 + ctrl.Width * 16, ctrl.Y, EditRight.width, EditRight.height, false, 255, 255, 255, 255, false);
    r := ctrl.Color.R;
    g := ctrl.Color.G;
    b := ctrl.Color.B;
    if ctrl.Invalid and (ctrl.Window.ActiveControl <> ctrl) then
    begin
      r := 128;
      g := 128;
      b := 128;
    end;
    f := Font[ctrl.BigFont];
    r_Draw_Text(ctrl.Text, ctrl.X + 8, ctrl.Y, r, g, b, 255, f);
    if ctrl.Window.ActiveControl = ctrl then
    begin
      r_Draw_GetTextSize(Copy(ctrl.Text, 1, ctrl.CaretPos), f, w, h);
      r_Draw_FillRect(ctrl.X + 8 + w, ctrl.Y + h - 4, ctrl.X + 8 + w + EDIT_CURSORLEN, ctrl.Y + h - 2, 200, 0, 0, 255);
    end;
  end;

  procedure r_GUI_Draw_KeyRead (ctrl: TGUIKeyRead);
    var k: AnsiString;
  begin
    if ctrl.IsQuery then
      k := KEYREAD_QUERY
    else if ctrl.Key <> 0 then
      k := e_KeyNames[ctrl.Key]
    else
      k := KEYREAD_CLEAR;
    r_Draw_Text(k, ctrl.X, ctrl.Y, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B, 255, Font[ctrl.BigFont]);
  end;

  procedure r_GUI_Draw_KeyRead2 (ctrl: TGUIKeyRead2);

    procedure drawText (idx: Integer);
      var x, y: Integer; r, g, b: Byte; kk: DWORD; str: AnsiString;
    begin
      if idx = 0 then kk := ctrl.Key0 else kk := ctrl.Key1;
      y := ctrl.Y;
      if idx = 0 then x := ctrl.X + 8 else x := ctrl.X + 8 + ctrl.MaxKeyNameWdt + 16;
      r := 255; g := 0; b := 0;
      if ctrl.KeyIdx = idx then
      begin
        r := 255; g := 255; b := 255;
      end;
      if ctrl.IsQuery and (ctrl.KeyIdx = idx) then
        str := KEYREAD_QUERY
      else if kk <> 0 then
        str := e_KeyNames[kk]
      else
        str := KEYREAD_CLEAR;
      r_Draw_Text(str, x, y, r, g, b, 255, Font[ctrl.BigFont]);
    end;

  begin
    drawText(0);
    drawText(1);
  end;

  procedure DrawBox (x, y, w, h: Integer);
  begin
    r_Draw_Texture(Box[0], x, y, 4, 4, false, 255, 255, 255, 255, false);
    r_Draw_TextureRepeat(Box[1], x + 4, y, w * 16, 4, false, 255, 255, 255, 255, false);
    r_Draw_Texture(Box[2], x + 4 + w * 16, y, 4, 4, false, 255, 255, 255, 255, false);

    r_Draw_TextureRepeat(Box[3], x, y + 4, 4, h * 16, false, 255, 255, 255, 255, false);
    r_Draw_TextureRepeat(Box[4], x + 4, y + 4, w * 16, h * 16, false, 255, 255, 255, 255, false);
    r_Draw_TextureRepeat(Box[5], x + 4 + w * 16, y + 4, 4, h * 16, false, 255, 255, 255, 255, false);

    r_Draw_Texture(Box[6], x, y + 4 + h * 16, 4, 4, false, 255, 255, 255, 255, false);
    r_Draw_TextureRepeat(Box[7], x + 4, y + 4 + h * 16, w * 16, 4, false, 255, 255, 255, 255, false);
    r_Draw_Texture(Box[8], x + 4 + w * 16, y + 4 + h * 16, 4, 4, false, 255, 255, 255, 255, false);
  end;

  procedure r_GUI_Draw_ModelView (ctrl: TGUIModelView);
  begin
    DrawBox(ctrl.X, ctrl.Y, 4, 4);
    if ctrl.Model <> nil then
      r_Map_DrawPlayerModel(ctrl.Model, ctrl.X + 4, ctrl.Y + 4, 255);
  end;

  procedure r_GUI_Draw_MapPreview (ctrl: TGUIMapPreview);
    var a: Integer; r, g, b: Byte;
  begin
    DrawBox(ctrl.X, ctrl.Y, MAPPREVIEW_WIDTH, MAPPREVIEW_HEIGHT);
    if (ctrl.MapSize.X <= 0) or (ctrl.MapSize.Y <= 0) then
      Exit;
    r_Draw_FillRect(ctrl.X + 4, ctrl.Y + 4, ctrl.X + 4 + Trunc(ctrl.MapSize.X / ctrl.Scale), ctrl.Y + 4 + Trunc(ctrl.MapSize.Y / ctrl.Scale), 32, 32, 32, 255);
    if ctrl.MapData <> nil then
      for a := 0 to High(ctrl.MapData) do
        with ctrl.MapData[a] do
        begin
          if X1 > MAPPREVIEW_WIDTH * 16 then Continue;
          if Y1 > MAPPREVIEW_HEIGHT * 16 then Continue;
          if X2 < 0 then Continue;
          if Y2 < 0 then Continue;
          if X2 > MAPPREVIEW_WIDTH * 16 then X2 := MAPPREVIEW_WIDTH * 16;
          if Y2 > MAPPREVIEW_HEIGHT * 16 then Y2 := MAPPREVIEW_HEIGHT * 16;
          if X1 < 0 then X1 := 0;
          if Y1 < 0 then Y1 := 0;
          case PanelType of
            PANEL_WALL:
            begin
              r := 255; g := 255; b := 255;
            end;
            PANEL_CLOSEDOOR:
            begin
              r := 255; g := 255; b := 0;
            end;
            PANEL_WATER:
            begin
              r := 0; g := 0; b := 192;
            end;
            PANEL_ACID1:
            begin
              r := 0; g := 176; b := 0;
            end;
            PANEL_ACID2:
            begin
              r := 176; g := 0; b := 0;
            end;
          else
            r := 128; g := 128; b := 128;
          end;
          if ((X2 - X1) > 0) and ((Y2 - Y1) > 0) then
            r_Draw_FillRect(ctrl.X + 4 + X1, ctrl.Y + 4 + Y1, ctrl.X + 4 + X2, ctrl.Y + 4 + Y2, r, g, b, 255);
      end;
  end;

  procedure r_GUI_Draw_Image (ctrl: TGUIImage);
    var pic: TGLTexture;
  begin
    pic := nopic;
    if ctrl.ImageRes <> '' then
      if r_Common_LoadThis(ctrl.ImageRes, ImageControl) then
        pic := ImageControl.id;
    if pic <> nil then
      r_Draw_Texture(pic, ctrl.x, ctrl.y, pic.width, pic.height, false, 255, 255, 255, 255, false);
  end;

  procedure DrawScroll(x, y, h: Integer; Up, Down: Boolean);
    var t: TGLTexture;
  begin
    if h >= 3 then
    begin
      t := BScrollUp[Up];
      r_Draw_Texture(t, x, y, t.width, t.height, false, 255, 255, 255, 255, false);
      t := BScrollDown[Down];
      r_Draw_Texture(t, x, y + (h - 1) * 16, t.width, t.height, false, 255, 255, 255, 255, false);
      t := BScrollMiddle;
      r_Draw_TextureRepeat(t, x, y + 16, t.width, (h - 2) * 16, false, 255, 255, 255, 255, false);
    end;
  end;

  procedure r_GUI_Draw_ListBox (ctrl: TGUIListBox); // + TGUIFileListBox
    var a, w2, h2: Integer; s: string; col: TRGB; f: TGLFont;
  begin
    if ctrl.DrawBack then
      DrawBox(ctrl.X, ctrl.Y, ctrl.Width + 1, ctrl.Height);
    if ctrl.DrawScrollBar then
      DrawScroll(ctrl.X + 4 + ctrl.Width * 16, ctrl.Y + 4, ctrl.Height, (ctrl.StartLine > 0) and (ctrl.Items <> nil), (ctrl.StartLine + ctrl.Height - 1 < High(ctrl.Items)) and (ctrl.Items <> nil));
    if ctrl.Items <> nil then
    begin
      f := Font[ctrl.BigFont];
      for a := ctrl.StartLine to Min(High(ctrl.Items), ctrl.StartLine + ctrl.Height - 1) do
      begin
        s := ctrl.Items[a];
        r_Draw_GetTextSize(s, f, w2, h2);
        while (Length(s) > 0) and (w2 > ctrl.Width * 16) do
        begin
          SetLength(s, Length(s) - 1);
          r_Draw_GetTextSize(s, f, w2, h2);
        end;
        if a = ctrl.ItemIndex then col := ctrl.ActiveColor else col := ctrl.UnActiveColor;
        r_Draw_Text(s, ctrl.X + 4, ctrl.Y + 4 + (a - ctrl.StartLine) * 16, col.r, col.g, col.b, 255, f);
      end;
    end;
  end;

  procedure r_GUI_Draw_Memo (ctrl: TGUIMemo);
    var i: Integer;
  begin
    if ctrl.DrawBack then
      DrawBox(ctrl.X, ctrl.Y, ctrl.Width + 1, ctrl.Height);
    if ctrl.DrawScrollBar then
      DrawScroll(ctrl.X + 4 + ctrl.Width * 16, ctrl.Y + 4, ctrl.Height, (ctrl.StartLine > 0) and (ctrl.Lines <> nil), (ctrl.StartLine + ctrl.Height - 1 < High(ctrl.Lines)) and (ctrl.Lines <> nil));
    if ctrl.Lines <> nil then
      for i := ctrl.StartLine to Min(High(ctrl.Lines), ctrl.StartLine + ctrl.Height - 1) do
        r_Draw_Text(ctrl.Lines[i], ctrl.X + 4, ctrl.Y + 4 + (i - ctrl.StartLine) * 16, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B, 255, Font[ctrl.BigFont]);
  end;

  procedure r_GUI_Draw_MainMenu (ctrl: TGUIMainMenu);
    var i, w, h: Integer; ID: TGLTexture;
  begin
    if ctrl.Header <> nil then
    begin
      r_GUI_Draw_Label(ctrl.Header)
    end
    else if LogoTex <> nil then
    begin
      r_GUI_GetLogoSize(w, h);
      r_Draw_Texture(LogoTex, ((gScreenWidth div 2) - (w div 2)), ctrl.Buttons[0].Y - ctrl.Buttons[0].GetHeight - h, w, h, false, 255, 255, 255, 255, false);
    end;
    if ctrl.Buttons <> nil then
    begin
      for i := 0 to High(ctrl.Buttons) do
        if ctrl.Buttons[i] <> nil then
          r_GUI_Draw_TextButton(ctrl.Buttons[i]);
      if ctrl.Index <> -1 then
      begin
        ID := MarkerID[ctrl.Counter DIV MAINMENU_MARKERDELAY MOD 2 <> 0];
        r_Draw_Texture(ID, ctrl.Buttons[ctrl.Index].X - 48, ctrl.Buttons[ctrl.Index].Y, ID.width, ID.height, false, 255, 255, 255, 255, false);
      end
    end;
  end;

  procedure r_GUI_Draw_Menu (ctrl: TGUIMenu);
    var a, locx, locy: Integer; f: TGLFont;
  begin
    if ctrl.Header <> nil then
      r_GUI_Draw_Label(ctrl.Header);
    if ctrl.Items <> nil then
    begin
      for a := 0 to High(ctrl.Items) do
      begin
        if ctrl.Items[a].Text <> nil then
          r_GUI_Draw_Control(ctrl.Items[a].Text);
        if ctrl.Items[a].Control <> nil then
          r_GUI_Draw_Control(ctrl.Items[a].Control);
      end;
    end;
    if (ctrl.Index <> -1) and (ctrl.Counter > MENU_MARKERDELAY div 2) then
    begin
      locx := 0;
      locy := 0;
      if ctrl.Items[ctrl.Index].Text <> nil then
      begin
        locx := ctrl.Items[ctrl.Index].Text.X;
        locy := ctrl.Items[ctrl.Index].Text.Y;
        //HACK!
        if ctrl.Items[ctrl.Index].Text.RightAlign then
        begin
          locx := locx + ctrl.Items[ctrl.Index].Text.CMaxWidth - ctrl.Items[ctrl.Index].Text.GetWidth;
        end;
      end
      else if ctrl.Items[ctrl.Index].Control <> nil then
      begin
        locx := ctrl.Items[ctrl.Index].Control.X;
        locy := ctrl.Items[ctrl.Index].Control.Y;
      end;
      f := Font[ctrl.BigFont];
      locx := locx - f.GetMaxWidth();
      r_Draw_Text(#16, locx, locy, 255, 0, 0, 255, f);
    end;
  end;

  procedure r_GUI_Draw_Control (ctrl: TGUIControl);
  begin
    if ctrl is TGUITextButton then
      r_GUI_Draw_TextButton(TGUITextButton(ctrl))
    else if ctrl is TGUILabel then
      r_GUI_Draw_Label(TGUILabel(ctrl))
    else if ctrl is TGUIScroll then
      r_GUI_Draw_Scroll(TGUIScroll(ctrl))
    else if ctrl is TGUISwitch then
      r_GUI_Draw_Switch(TGUISwitch(ctrl))
    else if ctrl is TGUIEdit then
      r_GUI_Draw_Edit(TGUIEdit(ctrl))
    else if ctrl is TGUIKeyRead then
      r_GUI_Draw_KeyRead(TGUIKeyRead(ctrl))
    else if ctrl is TGUIKeyRead2 then
      r_GUI_Draw_KeyRead2(TGUIKeyRead2(ctrl))
    else if ctrl is TGUIModelView then
      r_GUI_Draw_ModelView(TGUIModelView(ctrl))
    else if ctrl is TGUIMapPreview then
      r_GUI_Draw_MapPreview(TGUIMapPreview(ctrl))
    else if ctrl is TGUIImage then
      r_GUI_Draw_Image(TGUIImage(ctrl))
    else if ctrl is TGUIListBox then
      r_GUI_Draw_ListBox(TGUIListBox(ctrl)) // + TGUIFileListBox
    else if ctrl is TGUIMemo then
      r_GUI_Draw_Memo(TGUIMemo(ctrl))
    else if ctrl is TGUIMainMenu then
      r_GUI_Draw_MainMenu(TGUIMainMenu(ctrl))
    else if ctrl is TGUIMenu then
      r_GUI_Draw_Menu(TGUIMenu(ctrl))
    else
      Assert(False)
  end;

  procedure r_GUI_Draw_Window (win: TGUIWindow);
    var i, tw, th: Integer;
  begin
    // Here goes code duplication from g_game.pas:DrawMenuBackground()
    if win.BackTexture <> '' then
      if r_Common_LoadThis(win.BackTexture, Background) then
      begin
        r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 255);
        tw := Background.id.width;
        th := Background.id.height;
        if tw = th then
          tw := round(tw * 1.333 * (gScreenHeight / th))
        else
          tw := trunc(tw * (gScreenHeight / th));
        r_Draw_Texture(Background.id, (gScreenWidth - tw) div 2, 0, tw, gScreenHeight, false, 255, 255, 255, 255, false);
      end
      else
        r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 127, 127, 127, 255);

    // small hack here
    if win.Name = 'AuthorsMenu' then
      r_Draw_FillRect(0, 0, gScreenWidth - 1, gScreenHeight - 1, 0, 0, 0, 105);
    for i := 0 to High(win.Childs) do
      if win.Childs[i] <> nil then
        r_GUI_Draw_Control(win.Childs[i]);
  end;

end.
