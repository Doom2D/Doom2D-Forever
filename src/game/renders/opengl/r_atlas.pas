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
unit r_atlas;

interface

  type
    TAtlasNode = class
      private
        left, right, up: TAtlasNode;
        mL, mT, mR, mB: Integer;
        leaf: Boolean;

      public
        constructor Create;
        destructor Destroy; override;

        procedure Dealloc;

        function GetWidth (): Integer; inline;
        function GetHeight (): Integer; inline;

        property x: Integer read mL;
        property y: Integer read mT;
        property width: Integer read GetWidth;
        property height: Integer read GetHeight;

        property l: Integer read mL;
        property t: Integer read mT;
        property r: Integer read mR;
        property b: Integer read mB;
    end;

    TAtlas = class
      public
        constructor Create (w, h: Integer);
        destructor Destroy; override; (* also destroed attached nodes *)

        (* never free TAtlasNode directly, use Dealloc method. *)
        function CreateNode (): TAtlasNode; virtual; (* allocate empty node (user defined type) *)
        function Alloc (w, h: Integer): TAtlasNode;  (* allocate node and attach it *)

        function GetWidth (): Integer; inline;
        function GetHeight (): Integer; inline;

      private
        root: TAtlasNode;

        function NewNode (p: TAtlasNode; w, h: Integer): TAtlasNode;

      public
        property w: Integer read GetWidth;
        property h: Integer read GetHeight;
    end;


implementation

  procedure FreeNodeRecursive (n: TAtlasNode);
  begin
    if n <> nil then
    begin
      FreeNodeRecursive(n.left);
      FreeNodeRecursive(n.right);
      n.Free();
    end;
  end;

  function IsLeafTree (n: TAtlasNode): Boolean;
  begin
    result := (n <> nil) and (n.leaf or IsLeafTree(n.left) or IsLeafTree(n.right))
  end;

  (* --------- TNode --------- *)

  constructor TAtlasNode.Create;
  begin
    inherited;
  end;

  destructor TAtlasNode.Destroy;
    var p: TAtlasNode;
  begin
    p := self.up;
    if p <> nil then
    begin
      if p.left = self then
        p.left := nil
      else if p.right = self then
        p.right := nil
    end;
    self.up := nil;
    if self.left <> nil then
      self.left.Free;
    if self.right <> nil then
      self.right.Free;
    inherited;
  end;

  procedure TAtlasNode.Dealloc;
    var p: TAtlasNode;
  begin
    ASSERT(self.leaf = true);
    ASSERT(self.right = nil);
    ASSERT(self.left = nil);
    self.leaf := false;
    p := self.up;
    while p <> nil do
    begin
      ASSERT(p.leaf = false);
      ASSERT(p.left <> nil);
      ASSERT(p.right <> nil);
      if IsLeafTree(p) = false then
      begin
        FreeNodeRecursive(p.left); p.left := nil;
        FreeNodeRecursive(p.right); p.right := nil;
        p := p.up
      end
      else
      begin
        p := nil
      end
    end
  end;

  function TAtlasNode.GetWidth (): Integer;
  begin
    result := self.r - self.l + 1;
  end;

  function TAtlasNode.GetHeight (): Integer;
  begin
    result := self.b - self.t + 1;
  end;

  (* --------- TAtlas --------- *)

  constructor TAtlas.Create (w, h: Integer);
  begin
    inherited Create();
    self.root := self.CreateNode();
    ASSERT(self.root <> nil);
    self.root.mR := w - 1;
    self.root.mB := h - 1;
  end;

  destructor TAtlas.Destroy;
  begin
    FreeNodeRecursive(self.root.left);
    FreeNodeRecursive(self.root.right);
    inherited;
  end;

  function TAtlas.GetWidth (): Integer;
  begin
    result := self.root.r {- self.root.l} + 1;
  end;

  function TAtlas.GetHeight (): Integer;
  begin
    result := self.root.b {- self.root.t} + 1;
  end;

  function TAtlas.CreateNode (): TAtlasNode;
  begin
    result := TAtlasNode.Create()
  end;

  function TAtlas.NewNode (p: TAtlasNode; w, h: Integer): TAtlasNode;
    var n: TAtlasNode;
  begin
    ASSERT(p <> nil);
    ASSERT(w > 0);
    ASSERT(h > 0);
    if p.left <> nil then
    begin
      ASSERT(p.right <> nil);
      n := NewNode(p.left, w, h);
      if n = nil then
        n := NewNode(p.right, w, h);
      result := n;
    end
    else if p.leaf or (p.width < w) or (p.height < h) then
    begin
      result := nil;
    end
    else if (p.width = w) and (p.height = h) then
    begin
      p.leaf := true;
      result := p;
    end
    else
    begin
      p.left := self.CreateNode();
      p.right := self.CreateNode();
      if (p.left = nil) or (p.right = nil) then
      begin
        (* failed to allocate nodes *)
        if p.left <> nil then
          p.left.Free();
        if p.right <> nil then
          p.right.Free();
        p.left := nil;
        p.right := nil;
        result := nil;
      end
      else
      begin
        p.left.up := p;
        p.right.up := p;
        if p.width - w > p.height - h then
        begin
          p.left.mL := p.l;
          p.left.mT := p.t;
          p.left.mR := p.l + w - 1;
          p.left.mB := p.b;
          p.right.mL := p.l + w;
          p.right.mT := p.t;
          p.right.mR := p.r;
          p.right.mB := p.b;
        end
        else
        begin
          p.left.mL := p.l;
          p.left.mt := p.t;
          p.left.mR := p.r;
          p.left.mB := p.t + h - 1;
          p.right.mL := p.l;
          p.right.mT := p.t + h;
          p.right.mR := p.r;
          p.right.mB := p.b;
        end;
        result := NewNode(p.left, w, h);
      end
    end
  end;

  function TAtlas.Alloc (w, h: Integer): TAtlasNode;
    var n: TAtlasNode;
  begin
    ASSERT(w > 0);
    ASSERT(h > 0);
    n := nil;
    if (w <= self.w) and (h <= self.h) then
    begin
      n := NewNode(self.root, w, h);
      if n <> nil then
        ASSERT(n.leaf);
    end;
    result := n
  end;

end.
