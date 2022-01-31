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
unit r_gui;

interface

  uses g_gui;

  procedure r_GUI_Load;
  procedure r_GUI_Free;

  procedure r_GUI_GetSize (ctrl: TGUIControl; out w, h: Integer);
  procedure r_GUI_GetLogoSize (out w, h: WORD);
  procedure r_GUI_Draw_Window (win: TGUIWindow);

implementation

  uses
    Classes, Math,
    MAPDEF, utils,
    g_basic, g_base, e_input, g_options,
    r_graphics, r_textures, r_playermodel, r_game,
    g_game, g_menu
  ;

  const
    BOX1 = 'BOX1';
    BOX2 = 'BOX2';
    BOX3 = 'BOX3';
    BOX4 = 'BOX4';
    BOX5 = 'BOX5';
    BOX6 = 'BOX6';
    BOX7 = 'BOX7';
    BOX8 = 'BOX8';
    BOX9 = 'BOX9';

  var
    Box: Array [0..8] of DWORD;
    MarkerID: array [Boolean] of DWORD;
    ScrollLeft, ScrollRight, ScrollMiddle, ScrollMarker: DWORD;
    EditLeft, EditRight, EditMiddle: DWORD;

    Font: array [boolean] of TFont; (* Small[FALSE] / Big[TRUE] *)
    LogoTex: DWORD;

  procedure r_GUI_GetLogoSize (out w, h: WORD);
  begin
    w := 0;
    h := 0;
    if LogoTex <> 0 then
      e_GetTextureSize(LogoTex, @w, @h);
  end;

  procedure r_GUI_Load;
  begin
    g_Texture_CreateWADEx('MAINMENU_LOGO', GameWAD + ':TEXTURES\MAINLOGO');
    g_Texture_CreateWADEx('MAINMENU_MARKER1', GameWAD + ':TEXTURES\MARKER1');
    g_Texture_CreateWADEx('MAINMENU_MARKER2', GameWAD + ':TEXTURES\MARKER2');
    g_Texture_CreateWADEx('SCROLL_LEFT', GameWAD + ':TEXTURES\SLEFT');
    g_Texture_CreateWADEx('SCROLL_RIGHT', GameWAD + ':TEXTURES\SRIGHT');
    g_Texture_CreateWADEx('SCROLL_MIDDLE', GameWAD + ':TEXTURES\SMIDDLE');
    g_Texture_CreateWADEx('SCROLL_MARKER', GameWAD + ':TEXTURES\SMARKER');
    g_Texture_CreateWADEx('EDIT_LEFT', GameWAD + ':TEXTURES\ELEFT');
    g_Texture_CreateWADEx('EDIT_RIGHT', GameWAD + ':TEXTURES\ERIGHT');
    g_Texture_CreateWADEx('EDIT_MIDDLE', GameWAD + ':TEXTURES\EMIDDLE');
    g_Texture_CreateWADEx('BOX1', GameWAD +  ':TEXTURES\BOX1');
    g_Texture_CreateWADEx('BOX2', GameWAD + ':TEXTURES\BOX2');
    g_Texture_CreateWADEx('BOX3', GameWAD + ':TEXTURES\BOX3');
    g_Texture_CreateWADEx('BOX4', GameWAD + ':TEXTURES\BOX4');
    g_Texture_CreateWADEx('BOX5', GameWAD + ':TEXTURES\BOX5');
    g_Texture_CreateWADEx('BOX6', GameWAD + ':TEXTURES\BOX6');
    g_Texture_CreateWADEx('BOX7', GameWAD + ':TEXTURES\BOX7');
    g_Texture_CreateWADEx('BOX8', GameWAD + ':TEXTURES\BOX8');
    g_Texture_CreateWADEx('BOX9', GameWAD + ':TEXTURES\BOX9');
    g_Texture_CreateWADEx('BSCROLL_UP_A', GameWAD + ':TEXTURES\SCROLLUPA');
    g_Texture_CreateWADEx('BSCROLL_UP_U', GameWAD + ':TEXTURES\SCROLLUPU');
    g_Texture_CreateWADEx('BSCROLL_DOWN_A', GameWAD + ':TEXTURES\SCROLLDOWNA');
    g_Texture_CreateWADEx('BSCROLL_DOWN_U', GameWAD + ':TEXTURES\SCROLLDOWNU');
    g_Texture_CreateWADEx('BSCROLL_MIDDLE', GameWAD + ':TEXTURES\SCROLLMIDDLE');
    g_Texture_CreateWADEx('NOPIC', GameWAD + ':TEXTURES\NOPIC');

    g_Texture_Get(MAINMENU_MARKER1, MarkerID[FALSE]);
    g_Texture_Get(MAINMENU_MARKER2, MarkerID[TRUE]);

    g_Texture_Get(BOX1, Box[0]);
    g_Texture_Get(BOX2, Box[1]);
    g_Texture_Get(BOX3, Box[2]);
    g_Texture_Get(BOX4, Box[3]);
    g_Texture_Get(BOX5, Box[4]);
    g_Texture_Get(BOX6, Box[5]);
    g_Texture_Get(BOX7, Box[6]);
    g_Texture_Get(BOX8, Box[7]);
    g_Texture_Get(BOX9, Box[8]);

    g_Texture_Get(SCROLL_LEFT, ScrollLeft);
    g_Texture_Get(SCROLL_RIGHT, ScrollRight);
    g_Texture_Get(SCROLL_MIDDLE, ScrollMiddle);
    g_Texture_Get(SCROLL_MARKER, ScrollMarker);

    g_Texture_Get(EDIT_LEFT, EditLeft);
    g_Texture_Get(EDIT_RIGHT, EditRight);
    g_Texture_Get(EDIT_MIDDLE, EditMiddle);

    Font[FALSE] := TFont.Create(gMenuSmallFont, TFontType.Character);
    Font[TRUE] := TFont.Create(gMenuFont, TFontType.Character);

    g_Texture_Get('MAINMENU_LOGO', LogoTex)
  end;

  procedure r_GUI_Free;
  begin
    g_Texture_Delete('MAINMENU_LOGO');
    g_Texture_Delete('MAINMENU_MARKER1');
    g_Texture_Delete('MAINMENU_MARKER2');
    g_Texture_Delete('SCROLL_LEFT');
    g_Texture_Delete('SCROLL_RIGHT');
    g_Texture_Delete('SCROLL_MIDDLE');
    g_Texture_Delete('SCROLL_MARKER');
    g_Texture_Delete('EDIT_LEFT');
    g_Texture_Delete('EDIT_RIGHT');
    g_Texture_Delete('EDIT_MIDDLE');
    g_Texture_Delete('BOX1');
    g_Texture_Delete('BOX2');
    g_Texture_Delete('BOX3');
    g_Texture_Delete('BOX4');
    g_Texture_Delete('BOX5');
    g_Texture_Delete('BOX6');
    g_Texture_Delete('BOX7');
    g_Texture_Delete('BOX8');
    g_Texture_Delete('BOX9');
    g_Texture_Delete('BSCROLL_UP_A');
    g_Texture_Delete('BSCROLL_UP_U');
    g_Texture_Delete('BSCROLL_DOWN_A');
    g_Texture_Delete('BSCROLL_DOWN_U');
    g_Texture_Delete('BSCROLL_MIDDLE');
    g_Texture_Delete('NOPIC');
  end;

  procedure r_GUI_GetSize_TextButton (ctrl: TGUITextButton; out w, h: Integer);
    var ww, hh: WORD;
  begin
    ctrl.Font.GetTextSize(ctrl.Caption, ww, hh);
    w := ww;
    h := hh;
  end;

  procedure r_GUI_GetSize_Label (ctrl: TGUILabel; out w, h: Integer);
    var ww, hh: WORD;
  begin
    ctrl.Font.GetTextSize(ctrl.Text, ww, hh);
    h := hh;
    if ctrl.FixedLength = 0 then
      w := ww
    else
      w := e_CharFont_GetMaxWidth(ctrl.Font.ID) * ctrl.FixedLength
  end;

  procedure r_GUI_GetSize_Switch (ctrl: TGUISwitch; out w, h: Integer);
    var i: Integer; ww, hh: WORD;
  begin
    w := 0;
    h := 0;
    if ctrl.Items <> nil then
    begin
      for i := 0 to High(ctrl.Items) do
      begin
        ctrl.Font.GetTextSize(ctrl.Items[i], ww, hh);
        if ww > w then
          w := ww;
      end;
    end;
  end;

  procedure r_GUI_GetSize_KeyRead (ctrl: TGUIKeyRead; out w, h: Integer);
    var i: Integer; ww, hh: WORD;
  begin
    w := 0;
    h := 0; // ??? always 0
    for i := 0 to 255 do
    begin
      ctrl.Font.GetTextSize(e_KeyNames[i], ww, hh);
      w := MAX(w, ww);
    end;
    ctrl.Font.GetTextSize(KEYREAD_QUERY, ww, hh);
    if ww > w then w := ww;
    ctrl.Font.GetTextSize(KEYREAD_CLEAR, ww, hh);
    if ww > w then w := ww;
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
    ctrl.Font.Draw(ctrl.X, ctrl.Y, ctrl.Caption, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B)
  end;

  procedure r_GUI_Draw_Label (ctrl: TGUILabel);
    var w, h: Word;
  begin
    if ctrl.RightAlign then
    begin
      ctrl.Font.GetTextSize(ctrl.Text, w, h);
      ctrl.Font.Draw(ctrl.X + ctrl.CMaxWidth - w, ctrl.Y, ctrl.Text, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B);
    end
    else
      ctrl.Font.Draw(ctrl.X, ctrl.Y, ctrl.Text, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B);
  end;

  procedure r_GUI_Draw_Scroll (ctrl: TGUIScroll);
    var a: Integer;
  begin
    e_Draw(ScrollLeft, ctrl.X, ctrl.Y, 0, True, False);
    e_Draw(ScrollRight, ctrl.X + 8 + (ctrl.Max + 1) * 8, ctrl.Y, 0, True, False);
    for a := 0 to ctrl.Max do
      e_Draw(ScrollMiddle, ctrl.X + 8 + a * 8, ctrl.Y, 0, True, False);
    e_Draw(ScrollMarker, ctrl.X + 8 + ctrl.Value * 8, ctrl.Y, 0, True, False);
  end;

  procedure r_GUI_Draw_Switch (ctrl: TGUISwitch);
  begin
    ctrl.Font.Draw(ctrl.X, ctrl.Y, ctrl.Items[ctrl.ItemIndex], ctrl.Color.R, ctrl.Color.G, ctrl.Color.B);
  end;

  procedure r_GUI_Draw_Edit (ctrl: TGUIEdit);
    var c, w, h: Word; r, g, b: Byte;
  begin
    e_Draw(EditLeft, ctrl.X, ctrl.Y, 0, True, False);
    e_Draw(EditRight, ctrl.X + 8 + ctrl.Width * 16, ctrl.Y, 0, True, False);
    for c := 0 to ctrl.Width - 1 do
      e_Draw(EditMiddle, ctrl.X + 8 + c * 16, ctrl.Y, 0, True, False);
    r := ctrl.Color.R;
    g := ctrl.Color.G;
    b := ctrl.Color.B;
    if ctrl.Invalid and (ctrl.Window.ActiveControl <> ctrl) then
    begin
      r := 128;
      g := 128;
      b := 128;
    end;
    ctrl.Font.Draw(ctrl.X + 8, ctrl.Y, ctrl.Text, r, g, b);
    if ctrl.Window.ActiveControl = ctrl then
    begin
      ctrl.Font.GetTextSize(Copy(ctrl.Text, 1, ctrl.CaretPos), w, h);
      h := e_CharFont_GetMaxHeight(ctrl.Font.ID);
      e_DrawLine(2, ctrl.X + 8 + w, ctrl.Y + h - 3, ctrl.X + 8 + w + EDIT_CURSORLEN, ctrl.Y + h - 3, EDIT_CURSORCOLOR.R, EDIT_CURSORCOLOR.G, EDIT_CURSORCOLOR.B);
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
    ctrl.Font.Draw(ctrl.X, ctrl.Y, k, ctrl.Color.R, ctrl.Color.G, ctrl.Color.B);
  end;

  procedure r_GUI_Draw_KeyRead2 (ctrl: TGUIKeyRead2);

    procedure drawText (idx: Integer);
      var x, y: Integer; r, g, b: Byte; kk: DWORD; str: AnsiString;
    begin
      if idx = 0 then kk := ctrl.Key0 else kk := ctrl.Key1;
      y := ctrl.Y;
      if idx = 0 then x := ctrl.X + 8 else x := ctrl.X + 8 + ctrl.MaxKeyNameWdt + 16;
      r := 255;
      g := 0;
      b := 0;
      if ctrl.KeyIdx = idx then
      begin
        r := 255; g := 255; b := 255;
      end;
      if ctrl.IsQuery and (ctrl.KeyIdx = idx) then
      begin
        ctrl.Font.Draw(x, y, KEYREAD_QUERY, r, g, b)
      end
      else
      begin
        if kk <> 0 then
          str := e_KeyNames[kk]
        else
          str := KEYREAD_CLEAR;
        ctrl.Font.Draw(x, y, str, r, g, b);
      end
    end;

  begin
    drawText(0);
    drawText(1);
  end;

  procedure DrawBox(X, Y: Integer; Width, Height: Word);
  begin
    e_Draw(Box[0], X, Y, 0, False, False);
    e_DrawFill(Box[1], X + 4, Y, Width * 4, 1, 0, False, False);
    e_Draw(Box[2], X + 4 + Width * 16, Y, 0, False, False);
    e_DrawFill(Box[3], X, Y + 4, 1, Height * 4, 0, False, False);
    e_DrawFill(Box[4], X + 4, Y + 4, Width, Height, 0, False, False);
    e_DrawFill(Box[5], X + 4 + Width * 16, Y + 4, 1, Height * 4, 0, False, False);
    e_Draw(Box[6], X, Y + 4 + Height * 16, 0, False, False);
    e_DrawFill(Box[7], X + 4, Y + 4 + Height * 16, Width * 4, 1, 0, False, False);
    e_Draw(Box[8], X + 4 + Width * 16, Y + 4 + Height * 16, 0, False, False);
  end;

  procedure r_GUI_Draw_ModelView (ctrl: TGUIModelView);
  begin
    DrawBox(ctrl.X, ctrl.Y, 4, 4);
    if ctrl.Model <> nil then
      r_PlayerModel_Draw(ctrl.Model, ctrl.X + 4, ctrl.Y + 4);
  end;

  procedure r_GUI_Draw_MapPreview (ctrl: TGUIMapPreview);
    var a: Integer; r, g, b: Byte;
  begin
    DrawBox(ctrl.X, ctrl.Y, MAPPREVIEW_WIDTH, MAPPREVIEW_HEIGHT);
    if (ctrl.MapSize.X <= 0) or (ctrl.MapSize.Y <= 0) then
      Exit;
    e_DrawFillQuad(ctrl.X + 4, ctrl.Y + 4, ctrl.X + 4 + Trunc(ctrl.MapSize.X / ctrl.Scale) - 1, ctrl.Y + 4 + Trunc(ctrl.MapSize.Y / ctrl.Scale) - 1, 32, 32, 32, 0);
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
            e_DrawFillQuad(ctrl.X + 4 + X1, ctrl.Y + 4 + Y1, ctrl.X + 4 + X2 - 1, ctrl.Y + 4 + Y2 - 1, r, g, b, 0);
      end;    
  end;

  procedure r_GUI_Draw_Image (ctrl: TGUIImage);
    var ID: DWORD;
  begin
    if ctrl.ImageRes = '' then
    begin
      if g_Texture_Get(ctrl.DefaultRes, ID) then
        e_Draw(ID, ctrl.X, ctrl.Y, 0, True, False);
    end
    else
    begin
      if g_Texture_Get(ctrl.ImageRes, ID) then
        e_Draw(ID, ctrl.X, ctrl.Y, 0, True, False);
    end;
  end;

  procedure DrawScroll(X, Y: Integer; Height: Word; Up, Down: Boolean);
    var ID: DWORD;
  begin
    if Height < 3 then
      Exit;
    if Up then
      g_Texture_Get(BSCROLL_UPA, ID)
    else
      g_Texture_Get(BSCROLL_UPU, ID);
    e_Draw(ID, X, Y, 0, False, False);
    if Down then
      g_Texture_Get(BSCROLL_DOWNA, ID)
    else
      g_Texture_Get(BSCROLL_DOWNU, ID);
    e_Draw(ID, X, Y + (Height - 1) * 16, 0, False, False);
    g_Texture_Get(BSCROLL_MIDDLE, ID);
    e_DrawFill(ID, X, Y + 16, 1, Height - 2, 0, False, False);
  end;

  procedure r_GUI_Draw_ListBox (ctrl: TGUIListBox); // + TGUIFileListBox
    var w2, h2: Word; a: Integer; s: string;
  begin
    if ctrl.DrawBack then
      DrawBox(ctrl.X, ctrl.Y, ctrl.Width + 1, ctrl.Height);
    if ctrl.DrawScrollBar then
      DrawScroll(ctrl.X + 4 + ctrl.Width * 16, ctrl.Y + 4, ctrl.Height, (ctrl.StartLine > 0) and (ctrl.Items <> nil), (ctrl.StartLine + ctrl.Height - 1 < High(ctrl.Items)) and (ctrl.Items <> nil));
    if ctrl.Items <> nil then
    begin
      for a := ctrl.StartLine to Min(High(ctrl.Items), ctrl.StartLine + ctrl.Height - 1) do
      begin
        s := ctrl.Items[a];
        ctrl.Font.GetTextSize(s, w2, h2);
        while (Length(s) > 0) and (w2 > ctrl.Width * 16) do
        begin
          SetLength(s, Length(s) - 1);
          ctrl.Font.GetTextSize(s, w2, h2);
        end;
        if a = ctrl.ItemIndex then
          ctrl.Font.Draw(ctrl.X + 4, ctrl.Y + 4 + (a - ctrl.StartLine) * 16, s, ctrl.ActiveColor.R, ctrl.ActiveColor.G, ctrl.ActiveColor.B)
        else
          ctrl.Font.Draw(ctrl.X + 4, ctrl.Y + 4 + (a - ctrl.StartLine) * 16, s, ctrl.UnActiveColor.R, ctrl.UnActiveColor.G, ctrl.UnActiveColor.B);
      end;
    end;
  end;

  procedure r_GUI_Draw_Memo (ctrl: TGUIMemo);
    var a: Integer;
  begin
    if ctrl.DrawBack then
      DrawBox(ctrl.X, ctrl.Y, ctrl.Width + 1, ctrl.Height);
    if ctrl.DrawScrollBar then
      DrawScroll(ctrl.X + 4 + ctrl.Width * 16, ctrl.Y + 4, ctrl.Height, (ctrl.StartLine > 0) and (ctrl.Lines <> nil), (ctrl.StartLine + ctrl.Height - 1 < High(ctrl.Lines)) and (ctrl.Lines <> nil));
    if ctrl.Lines <> nil then
      for a := ctrl.StartLine to Min(High(ctrl.Lines), ctrl.StartLine + ctrl.Height - 1) do
        ctrl.Font.Draw(ctrl.X + 4, ctrl.Y + 4 + (a - ctrl.StartLine) * 16, ctrl.Lines[a], ctrl.Color.R, ctrl.Color.G, ctrl.Color.B);
  end;

  procedure r_GUI_Draw_MainMenu (ctrl: TGUIMainMenu);
    var a: Integer; w, h: Word; ID: DWORD;
  begin
    if ctrl.Header <> nil then
    begin
      r_GUI_Draw_Label(ctrl.Header)
    end
    else if LogoTex <> 0 then
    begin
      e_GetTextureSize(LogoTex, @w, @h);
      e_Draw(LogoTex, ((gScreenWidth div 2) - (w div 2)), ctrl.Buttons[0].Y - ctrl.Buttons[0].GetHeight - h, 0, True, False);
    end;
    if ctrl.Buttons <> nil then
    begin
      for a := 0 to High(ctrl.Buttons) do
        if ctrl.Buttons[a] <> nil then
          r_GUI_Draw_TextButton(ctrl.Buttons[a]);
      if ctrl.Index <> -1 then
      begin
        ID := MarkerID[ctrl.Counter DIV MAINMENU_MARKERDELAY MOD 2 <> 0];
        e_Draw(ID, ctrl.Buttons[ctrl.Index].X - 48, ctrl.Buttons[ctrl.Index].Y, 0, True, False);
      end
    end;    
  end;

  procedure r_GUI_Draw_Menu (ctrl: TGUIMenu);
    var a, locx, locy: Integer;
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
      locx := locx - e_CharFont_GetMaxWidth(ctrl.FontID);
      e_CharFont_PrintEx(ctrl.FontID, locx, locy, #16, _RGB(255, 0, 0));
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
    var i: Integer; ID: DWORD; tw, th: Word;
  begin
    // Here goes code duplication from g_game.pas:DrawMenuBackground()
    if win.BackTexture <> '' then
      if g_Texture_Get(win.BackTexture, ID) then
      begin
        e_Clear(0, 0, 0);
        e_GetTextureSize(ID, @tw, @th);
        if tw = th then
          tw := round(tw * 1.333 * (gScreenHeight / th))
        else
          tw := trunc(tw * (gScreenHeight / th));
        e_DrawSize(ID, (gScreenWidth - tw) div 2, 0, 0, False, False, tw, gScreenHeight);
      end
      else
        e_Clear(0.5, 0.5, 0.5);

    // small hack here
    if win.Name = 'AuthorsMenu' then
      e_DarkenQuadWH(0, 0, gScreenWidth, gScreenHeight, 150);
    for i := 0 to High(win.Childs) do
      if win.Childs[i] <> nil then
        r_GUI_Draw_Control(win.Childs[i]);
  end;

end.
