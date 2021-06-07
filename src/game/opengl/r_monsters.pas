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
unit r_monsters;

interface

  procedure r_Monsters_Draw;
  procedure r_Monsters_DrawHealth;

implementation

  uses
    SysUtils, Classes, Math,
    e_graphics,
    MAPDEF,
    g_basic, g_game, g_phys,
    g_monsters
  ;

  procedure r_Monsters_Draw (constref monster: TMonster);
    var m: TMirrorType; dx, dy, c, fX, fY: Integer; o: TObj;
  begin
    with monster do
    begin
      //e_CharFont_Print(gMenuSmallFont, Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y, 'TYPE: ' + IntToStr(MonsterType));
      //e_CharFont_Print(gMenuSmallFont, Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y + 16, 'STATE: ' + IntToStr(MonsterState));

      Obj.lerp(gLerpFactor, fX, fY);

      // Если колдун стреляет, то рисуем огонь:
      if MonsterType = MONSTER_VILE then
        if MonsterState = MONSTATE_SHOOT then
          if GetPos(MonsterTargetUID, @o) then
            VileFireAnim.Draw(o.X + o.Rect.X + (o.Rect.Width div 2) - 32, o.Y + o.Rect.Y + o.Rect.Height - 128, TMirrorType.None);

      // Не в области рисования не ресуем:
      //FIXME!
      if (g_dbg_scale = 1.0) then
      begin
        if not g_Collide(Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y, Obj.Rect.Width, Obj.Rect.Height, sX - 128, sY - 128, sWidth + 256, sHeight + 256) then
          Exit;
      end;

      // Эти монстры, умирая, не оставляют трупов:
      if MonsterState = MONSTATE_DEAD then
        case MonsterType of
          MONSTER_BARREL, MONSTER_SOUL, MONSTER_PAIN: Exit;
        end;

      // Есть что рисовать при текущем поведении:
      if DirAnim[MonsterAnim, GameDirection] <> nil then
      begin
        // Если нет левой анимации или она совпадает с правой => отражаем правую:
        if (GameDirection = TDirection.D_LEFT) and ((not MONSTER_ANIMTABLE[MonsterType].LeftAnim) or (DirAnim[MonsterAnim, TDirection.D_LEFT].FramesID = DirAnim[MonsterAnim, TDirection.D_RIGHT].FramesID)) and (MonsterType <> MONSTER_BARREL) then
          m := TMirrorType.Horizontal
        else
          m := TMirrorType.None;

        // Левая анимация => меняем смещение относительно центра:
        if (GameDirection = TDirection.D_LEFT) and (MonsterType <> MONSTER_BARREL) then
        begin
          dx := MONSTER_ANIMTABLE[MonsterType].AnimDeltaLeft[MonsterAnim].X;
          dy := MONSTER_ANIMTABLE[MonsterType].AnimDeltaLeft[MonsterAnim].Y;

          if m = TMirrorType.Horizontal then
          begin
            // Нет отдельной левой анимации
            // Расстояние от края текстуры до края визуального положения объекта на текстуре:
            c := (MONSTERTABLE[MonsterType].Rect.X - dx) + MONSTERTABLE[MonsterType].Rect.Width;
            // Расстояние от края хит бокса до края визуального положения объекта на текстуре:
            dx := DirAnim[MonsterAnim, GameDirection].Width - c - MONSTERTABLE[MonsterType].Rect.X;
            // Т.к. двигать текстуру нужно будет в противоположном направлении:
            dx := -dx;
            // Это значит: dX := -frameWidth - animDeltaX + hitX + hitWidth + hitX
          end
        end
        else // Правая анимация
        begin
          dx := MONSTER_ANIMTABLE[MonsterType].AnimDeltaRight[MonsterAnim].X;
          dy := MONSTER_ANIMTABLE[MonsterType].AnimDeltaRight[MonsterAnim].Y;
        end;

        // Рисуем:
        DirAnim[MonsterAnim, GameDirection].Draw(fX + dx, fY + dy, m);
      end;

      if g_debug_Frames then
      begin
        e_DrawQuad(Obj.X + Obj.Rect.X, Obj.Y + Obj.Rect.Y, Obj.X + Obj.Rect.X + Obj.Rect.Width - 1, Obj.Y + Obj.Rect.Y + Obj.Rect.Height - 1, 0, 255, 0);
      end
    end
  end;

  procedure r_Monsters_Draw;
    var a: Integer;
  begin
    if gMonsters <> nil then
      for a := 0 to High(gMonsters) do
        if (gMonsters[a] <> nil) then r_Monsters_Draw(gMonsters[a]);
  end;

  procedure r_Monsters_DrawHealth;
    var a: Integer; fW, fH: Byte;
  begin
    if gMonsters = nil then Exit;
    e_TextureFontGetSize(gStdFont, fW, fH);
    for a := 0 to High(gMonsters) do
    begin
      if gMonsters[a] <> nil then
      begin
        e_TextureFontPrint(gMonsters[a].Obj.X + gMonsters[a].Obj.Rect.X,
        gMonsters[a].Obj.Y + gMonsters[a].Obj.Rect.Y + gMonsters[a].Obj.Rect.Height - fH,
        IntToStr(gMonsters[a].MonsterHealth), gStdFont);
      end
    end
  end;

end.
