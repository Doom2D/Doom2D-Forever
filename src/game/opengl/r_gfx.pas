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
unit r_gfx;

interface

{
  const
    R_GFX_FLAME_WIDTH = 32;
    R_GFX_FLAME_HEIGHT = 32;
    R_GFX_SMOKE_WIDTH = 32;
    R_GFX_SMOKE_HEIGHT = 32;
}

  procedure r_GFX_Load;
  procedure r_GFX_Free;
  procedure r_GFX_Draw;
  procedure r_GFX_Update;

  procedure r_GFX_OnceAnim (AnimType, X, Y: Integer);

implementation

  uses
    {$INCLUDE ../nogl/noGLuses.inc}
    SysUtils, Classes, Math,
    utils,
    g_base, r_graphics, g_options, r_animations,
    g_game, g_textures,
    g_gfx
  ;

  type
    TOnceAnim = record
      AnimType:   Byte;
      x, y:       Integer;
      oldX, oldY: Integer;
      Animation:  TAnimationState;
    end;

  var
    OnceAnims: array of TOnceAnim = nil;
    gfxFrames: array [0..R_GFX_LAST] of DWORD;

  procedure r_GFX_Load;
  begin
    g_Frames_CreateWAD(nil, 'FRAMES_TELEPORT', GameWAD+':TEXTURES\TELEPORT', 64, 64, 10, False);
    g_Frames_Get(gfxFrames[R_GFX_TELEPORT], 'FRAMES_TELEPORT');
    g_Frames_Get(gfxFrames[R_GFX_FLAME], 'FRAMES_FLAME');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_ROCKET], 'FRAMES_EXPLODE_ROCKET');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_BFG], 'FRAMES_EXPLODE_BFG');
    g_Frames_Get(gfxFrames[R_GFX_BFG_HIT], 'FRAMES_BFGHIT');
    g_Frames_Get(gfxFrames[R_GFX_FIRE], 'FRAMES_FIRE');
    g_Frames_Get(gfxFrames[R_GFX_ITEM_RESPAWN], 'FRAMES_ITEM_RESPAWN');
    g_Frames_Get(gfxFrames[R_GFX_SMOKE], 'FRAMES_SMOKE');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_SKELFIRE], 'FRAMES_EXPLODE_SKELFIRE');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_PLASMA], 'FRAMES_EXPLODE_PLASMA');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_BSPFIRE], 'FRAMES_EXPLODE_BSPFIRE');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_IMPFIRE], 'FRAMES_EXPLODE_IMPFIRE');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_CACOFIRE], 'FRAMES_EXPLODE_CACOFIRE');
    g_Frames_Get(gfxFrames[R_GFX_EXPLODE_BARONFIRE], 'FRAMES_EXPLODE_BARONFIRE');
  end;

  procedure r_GFX_Free;
    var a: Integer;
  begin
    g_Frames_DeleteByName('FRAMES_TELEPORT');
    if OnceAnims <> nil then
    begin
      for a := 0 to High(OnceAnims) do
        OnceAnims[a].Animation.Free();
      OnceAnims := nil;
    end;
  end;

  function FindOnceAnim (): DWORD;
    var i: Integer;
  begin
    if OnceAnims <> nil then
      for i := 0 to High(OnceAnims) do
        if OnceAnims[i].Animation = nil then
        begin
          Result := i;
          Exit;
        end;
    if OnceAnims = nil then
    begin
      SetLength(OnceAnims, 16);
      Result := 0;
    end
    else
    begin
      Result := High(OnceAnims) + 1;
      SetLength(OnceAnims, Length(OnceAnims) + 16);
    end;
  end;

  procedure r_GFX_OnceAnim (AnimType, x, y: Integer);
    var find_id: DWORD; a: TAnimationState;
  begin
    if not gpart_dbg_enabled then exit;
    find_id := FindOnceAnim();
    case AnimType of
      R_GFX_NONE: a := nil;
      R_GFX_TELEPORT: a := TAnimationState.Create(false, 6, 10); // !!! speed can be 3
      R_GFX_TELEPORT_FAST:
      begin
        AnimType := R_GFX_TELEPORT;
        a := TAnimationState.Create(false, 3, 10);
      end;
      R_GFX_FLAME: a := TAnimationState.Create(false, 3, 11);
      R_GFX_FLAME_RAND:
      begin
        AnimType := R_GFX_FLAME;
        a := TAnimationState.Create(false, 2 + Random(2), 10);
      end;
      R_GFX_EXPLODE_ROCKET: a := TAnimationState.Create(false, 6, 6);
      R_GFX_EXPLODE_BFG: a := TAnimationState.Create(false, 6, 6);
      R_GFX_BFG_HIT: a := TAnimationState.Create(false, 4, 4);
      R_GFX_FIRE: a := TAnimationState.Create(false, 4, 8); // !!! speed can be random
      R_GFX_ITEM_RESPAWN: a := TAnimationState.Create(false, 4, 5);
      R_GFX_SMOKE: a := TAnimationState.Create(false, 3, 10);
      R_GFX_SMOKE_TRANS:
      begin
        AnimType := R_GFX_SMOKE;
        a := TAnimationState.Create(false, 3, 10);
        a.alpha := 150;
      end;
      R_GFX_EXPLODE_SKELFIRE: a := TAnimationState.Create(false, 8, 3);
      R_GFX_EXPLODE_PLASMA: a := TAnimationState.Create(false, 3, 4);
      R_GFX_EXPLODE_BSPFIRE: a := TAnimationState.Create(false, 3, 5);
      R_GFX_EXPLODE_IMPFIRE: a := TAnimationState.Create(false, 6, 3);
      R_GFX_EXPLODE_CACOFIRE: a := TAnimationState.Create(false, 6, 3);
      R_GFX_EXPLODE_BARONFIRE: a := TAnimationState.Create(false, 6, 3);
    else
      a := nil;
      assert(false)
    end;
    OnceAnims[find_id].AnimType := AnimType;
    OnceAnims[find_id].Animation := a;
//    OnceAnims[find_id].Animation.Blending := Anim.Blending;
//    OnceAnims[find_id].Animation.alpha := Anim.alpha;
    OnceAnims[find_id].x := x;
    OnceAnims[find_id].y := y;
  end;

  procedure r_GFX_Update;
    var a: Integer;
  begin
    if OnceAnims <> nil then
    begin
      for a := 0 to High(OnceAnims) do
      begin
        if OnceAnims[a].Animation <> nil then
        begin
          OnceAnims[a].oldx := OnceAnims[a].x;
          OnceAnims[a].oldy := OnceAnims[a].y;
          case OnceAnims[a].AnimType of
            R_GFX_FLAME, R_GFX_SMOKE: (*ONCEANIM_SMOKE:*)
            begin
              if Random(3) = 0 then
                OnceAnims[a].x := OnceAnims[a].x-1+Random(3);
              if Random(2) = 0 then
                OnceAnims[a].y := OnceAnims[a].y-Random(2);
            end;
          end;
          if OnceAnims[a].Animation.Played then
          begin
            OnceAnims[a].Animation.Free();
            OnceAnims[a].Animation := nil;
          end
          else
            OnceAnims[a].Animation.Update();
        end;
      end;
    end;
  end;

procedure r_GFX_Draw;
  var
    a, len, fx, fy: Integer;
begin
  if not gpart_dbg_enabled then exit;

  if (Particles <> nil) then
  begin
    glDisable(GL_TEXTURE_2D);
         if (g_dbg_scale < 0.6) then glPointSize(1)
    else if (g_dbg_scale > 1.3) then glPointSize(g_dbg_scale+1)
    else glPointSize(2);
    glDisable(GL_POINT_SMOOTH);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glBegin(GL_POINTS);

    len := High(Particles);
    for a := 0 to len do
    begin
      with Particles[a] do
      begin
        if not alive then continue;
        if (x >= sX) and (y >= sY) and (x <= sX+sWidth) and (sY <= sY+sHeight) then
        begin
          fx := nlerp(oldx, x, gLerpFactor);
          fy := nlerp(oldy, y, gLerpFactor);
          glColor4ub(red, green, blue, alpha);
          glVertex2f(fx+0.37, fy+0.37);
        end;
      end;
    end;

    glEnd();

    glDisable(GL_BLEND);
  end;

  if (OnceAnims <> nil) then
  begin
    len := High(OnceAnims);
    for a := 0 to len do
    begin
      if (OnceAnims[a].Animation <> nil) then
      begin
        with OnceAnims[a] do
        begin
          fx := nlerp(oldx, x, gLerpFactor);
          fy := nlerp(oldy, y, gLerpFactor);
          r_AnimationState_Draw(gfxFrames[AnimType], Animation, x, y, TMirrorType.None);
        end;
      end;
    end;
  end;
end;

end.
