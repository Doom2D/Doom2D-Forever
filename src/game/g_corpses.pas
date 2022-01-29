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
unit g_corpses;

interface

  uses Classes, g_phys, g_player, g_playermodel, g_base;

  const
    CORPSE_STATE_REMOVEME = 0;
    CORPSE_STATE_NORMAL   = 1;
    CORPSE_STATE_MESS     = 2;

    PLAYER_CORPSERECT: TRectWH = (X:15; Y:48; Width:34; Height:16);

    DefaultCorpsesMax = 20;

  type
    TCorpse = class{$IFDEF USE_MEMPOOL}(TPoolObject){$ENDIF}
    private
      FMess:          Boolean;
      FState:         Byte;
      FDamage:        Byte;
      FObj:           TObj;
      FPlayerUID:     Word;
      FModel:   TPlayerModel;

    public
      constructor Create(X, Y: Integer; ModelName: String; aMess: Boolean);
      destructor  Destroy(); override;
      procedure   Damage(Value: Word; SpawnerUID: Word; vx, vy: Integer);
      procedure   Update();
      procedure   SaveState (st: TStream);
      procedure   LoadState (st: TStream);

      procedure getMapBox (out x, y, w, h: Integer); inline;
      procedure moveBy (dx, dy: Integer); inline;

      procedure positionChanged ();  inline; //WARNING! call this after entity position was changed, or coldet will not work right!

      function ObjPtr (): PObj; inline;

      property Obj: TObj read FObj; // copies object
      property State: Byte read FState;
      property Mess: Boolean read FMess;
      property Model: TPlayerModel read FModel;
      property PlayerUID: Word read FPlayerUID;
    end;

  var
    gCorpses: Array of TCorpse;

  procedure g_Corpses_SetMax (Count: Word);
  function g_Corpses_GetMax (): Word;

  function g_Corpses_Create (Player: TPlayer): Integer;
  procedure g_Corpses_RemoveAll;
  procedure g_Corpses_Update;

  {$IFNDEF HEADLESS}
    function g_Corpses_GetCameraObj (Player: TPlayer): TObj;
  {$ENDIF}

implementation

  uses
    {$IFDEF ENABLE_GFX}
      g_gfx,
    {$ENDIF}
    {$IFDEF ENABLE_GIBS}
      g_gibs,
    {$ENDIF}
    Math,
    utils, g_saveload, xstreams,
    g_game, g_textures, g_map
  ;

  var
    MaxCorpses: Word = DefaultCorpsesMax;

  constructor TCorpse.Create (X, Y: Integer; ModelName: String; aMess: Boolean);
  begin
    g_Obj_Init(@FObj);
    FObj.X := X;
    FObj.Y := Y;
    FObj.Rect := PLAYER_CORPSERECT;
    FMess := aMess;
    FModel := g_PlayerModel_Get(ModelName);
    if FMess then
    begin
      FState := CORPSE_STATE_MESS;
      FModel.ChangeAnimation(A_DIE2);
    end
    else
    begin
      FState := CORPSE_STATE_NORMAL;
      FModel.ChangeAnimation(A_DIE1);
    end;
  end;

  destructor TCorpse.Destroy;
  begin
    FModel.Free;
    inherited;
  end;

  function TCorpse.ObjPtr (): PObj; inline;
  begin
    Result := @FObj;
  end;

  procedure TCorpse.positionChanged; inline;
  begin
  end;

  procedure TCorpse.moveBy (dx, dy: Integer); inline;
  begin
    if (dx <> 0) or (dy <> 0) then
    begin
      FObj.X += dx;
      FObj.Y += dy;
      positionChanged;
    end;
  end;

  procedure TCorpse.getMapBox (out x, y, w, h: Integer); inline;
  begin
    x := FObj.X+PLAYER_CORPSERECT.X;
    y := FObj.Y+PLAYER_CORPSERECT.Y;
    w := PLAYER_CORPSERECT.Width;
    h := PLAYER_CORPSERECT.Height;
  end;

  procedure TCorpse.Damage (Value: Word; SpawnerUID: Word; vx, vy: Integer);
    {$IFDEF ENABLE_GFX}
      var Blood: TModelBlood;
    {$ENDIF}
  begin
    if FState = CORPSE_STATE_REMOVEME then
      Exit;
    FDamage := FDamage + Value;

{$IFDEF ENABLE_GIBS}
    if FDamage > 150 then
    begin
      if FModel <> nil then
      begin
        FState := CORPSE_STATE_REMOVEME;
        g_Gibs_Create(
          FObj.X + FObj.Rect.X + (FObj.Rect.Width div 2),
          FObj.Y + FObj.Rect.Y + (FObj.Rect.Height div 2),
          FModel.id,
          FModel.Color
        );
        // Звук мяса от трупа:
        FModel.PlaySound(MODELSOUND_DIE, 5, FObj.X, FObj.Y);
        // Зловещий смех:
        if (gBodyKillEvent <> -1) and gDelayedEvents[gBodyKillEvent].Pending then
          gDelayedEvents[gBodyKillEvent].Pending := False;
        gBodyKillEvent := g_Game_DelayEvent(DE_BODYKILL, 1050, SpawnerUID);
        FModel.Free;
        FModel := nil;
      end
    end
    else
{$ENDIF}
    begin
      FObj.Vel.X := FObj.Vel.X + vx;
      FObj.Vel.Y := FObj.Vel.Y + vy;
      {$IFDEF ENABLE_GFX}
        Blood := FModel.GetBlood();
        g_GFX_Blood(FObj.X+PLAYER_CORPSERECT.X+(PLAYER_CORPSERECT.Width div 2),
                    FObj.Y+PLAYER_CORPSERECT.Y+(PLAYER_CORPSERECT.Height div 2),
                    Value, vx, vy, 16, (PLAYER_CORPSERECT.Height*2) div 3,
                    Blood.R, Blood.G, Blood.B, Blood.Kind);
      {$ENDIF}
    end;
  end;

  procedure TCorpse.Update;
    var st: Word;
  begin
    if FState = CORPSE_STATE_REMOVEME then
      Exit;

    FObj.oldX := FObj.X;
    FObj.oldY := FObj.Y;
    if gTime mod (GAME_TICK*2) <> 0 then
    begin
      g_Obj_Move(@FObj, True, True, True);
      positionChanged(); // this updates spatial accelerators
      Exit;
    end;

    // Сопротивление воздуха для трупа:
    FObj.Vel.X := z_dec(FObj.Vel.X, 1);

    st := g_Obj_Move(@FObj, True, True, True);
    positionChanged; // this updates spatial accelerators

    if WordBool(st and MOVE_FALLOUT) then
      FState := CORPSE_STATE_REMOVEME
    else if FModel <> nil then
      FModel.Update;
  end;

  procedure TCorpse.SaveState (st: TStream);
    var anim: Boolean;
  begin
    assert(st <> nil);

    // Сигнатура трупа
    utils.writeSign(st, 'CORP');
    utils.writeInt(st, Byte(0));
    // Состояние
    utils.writeInt(st, Byte(FState));
    // Накопленный урон
    utils.writeInt(st, Byte(FDamage));
    // Цвет
    utils.writeInt(st, Byte(FModel.Color.R));
    utils.writeInt(st, Byte(FModel.Color.G));
    utils.writeInt(st, Byte(FModel.Color.B));
    // Объект трупа
    Obj_SaveState(st, @FObj);
    utils.writeInt(st, Word(FPlayerUID));
    // animation
    anim := (FModel <> nil);
    utils.writeBool(st, anim);
    if anim then FModel.AnimState.SaveState(st, 0, False);
    // animation for mask (same as animation, compat with older saves)
    anim := (FModel <> nil);
    utils.writeBool(st, anim);
    if anim then FModel.AnimState.SaveState(st, 0, False);
  end;

  procedure TCorpse.LoadState (st: TStream);
    var anim, blending: Boolean; r, g, b, alpha: Byte; stub: TAnimationState;
  begin
    assert(st <> nil);

    // Сигнатура трупа
    if not utils.checkSign(st, 'CORP') then raise XStreamError.Create('invalid corpse signature');
    if (utils.readByte(st) <> 0) then raise XStreamError.Create('invalid corpse version');
    // Состояние
    FState := utils.readByte(st);
    // Накопленный урон
    FDamage := utils.readByte(st);
    // Цвет
    r := utils.readByte(st);
    g := utils.readByte(st);
    b := utils.readByte(st);
    FModel.SetColor(r, g, b);
    // Объект трупа
    Obj_LoadState(@FObj, st);
    FPlayerUID := utils.readWord(st);
    // animation
    stub := TAnimationState.Create(False, 0, 0);
    anim := utils.readBool(st);
    if anim then
    begin
      stub.LoadState(st, alpha, blending);
      FModel.AnimState.CurrentFrame := Min(stub.CurrentFrame, FModel.AnimState.Length);
    end
    else
    begin
      FModel.Free;
      FModel := nil
    end;
    // animation for mask (same as animation, compat with older saves)
    anim := utils.readBool(st);
    if anim then stub.LoadState(st, alpha, blending);
    stub.Free;
  end;

  procedure g_Corpses_SetMax (Count: Word);
  begin
    MaxCorpses := Count;
    SetLength(gCorpses, Count);
  end;

  function g_Corpses_GetMax (): Word;
  begin
    Result := MaxCorpses;
  end;

  function  g_Corpses_Create (Player: TPlayer): Integer;
    var i: Integer; find_id: DWORD; ok: Boolean;
  begin
    Result := -1;
    if Player.alive then
      Exit;
    // Разрываем связь с прежним трупом:
    i := Player.FCorpse;
    if (i >= 0) and (i < Length(gCorpses)) then
    begin
      if (gCorpses[i] <> nil) and (gCorpses[i].FPlayerUID = Player.UID) then
        gCorpses[i].FPlayerUID := 0;
    end;

    if Player.Obj.Y >= gMapInfo.Height+128 then
      Exit;

{$IFDEF ENABLE_GIBS}
    if (Player.Health < -50) and (gGibsCount > 0) then
    begin
      g_Gibs_Create(Player.Obj.X + PLAYER_RECT_CX, Player.Obj.Y + PLAYER_RECT_CY, Player.Model.id, Player.Model.Color);
    end
    else
{$ENDIF}
    begin
      if (gCorpses = nil) or (Length(gCorpses) = 0) then
        Exit;
      ok := False;
      for find_id := 0 to High(gCorpses) do
        if gCorpses[find_id] = nil then
        begin
          ok := True;
          Break;
        end;
      if not ok then
        find_id := Random(Length(gCorpses));
      gCorpses[find_id] := TCorpse.Create(Player.Obj.X, Player.Obj.Y, Player.Model.GetName(), Player.Health < -20);
      gCorpses[find_id].FModel.Color := Player.Model.Color;
      gCorpses[find_id].FObj.Vel := Player.Obj.Vel;
      gCorpses[find_id].FObj.Accel := Player.Obj.Accel;
      gCorpses[find_id].FPlayerUID := Player.UID;
      Result := find_id;
    end
  end;

  procedure g_Corpses_Update;
    var i: Integer;
  begin
    if gCorpses <> nil then
    begin
      for i := 0 to High(gCorpses) do
      begin
        if gCorpses[i] <> nil then
        begin
          if gCorpses[i].State = CORPSE_STATE_REMOVEME then
          begin
            gCorpses[i].Free();
            gCorpses[i] := nil;
          end
          else
            gCorpses[i].Update();
        end;
      end;
    end;
  end;

  procedure g_Corpses_RemoveAll;
    var i: Integer;
  begin
    if gCorpses <> nil then
      for i := 0 to High(gCorpses) do
        gCorpses[i].Free();
    gCorpses := nil;
    SetLength(gCorpses, MaxCorpses);
  end;

{$IFNDEF HEADLESS}
  function g_Corpses_GetCameraObj (Player: TPlayer): TObj;
  begin
    {$IFDEF ENABLE_CORPSES}
      if (not Player.Alive) and (not Player.Spectator) and
         (Player.Corpse >= 0) and (Player.Corpse < Length(gCorpses)) and
         (gCorpses[Player.Corpse] <> nil) and (gCorpses[Player.Corpse].PlayerUID = Player.UID) then
      begin
        gCorpses[Player.Corpse].FObj.slopeUpLeft := Player.Obj.slopeUpLeft;
        Result := gCorpses[Player.Corpse].Obj;
      end
      else
      begin
        Result := Player.Obj;
      end;
    {$ELSE}
      Result := Player.Obj;
    {$ENDIF}
  end;
{$ENDIF}

end.
