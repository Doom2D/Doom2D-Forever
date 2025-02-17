(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

implementation

  uses SDL2;

  const
    GL_FLOAT = $1406;
    GL_VERTEX_ARRAY = $8074;
    GL_NORMAL_ARRAY = $8075;
    GL_COLOR_ARRAY = $8076;
    GL_TEXTURE_COORD_ARRAY = $8078;
    GL_TRIANGLES = $0004;
    GL_TEXTURE0 = $84C0;
    GL_INVALID_ENUM = $0500;

  const
    ValPerVertex = 2;
    ValPerColor = 4;
    ValPerCoord = 2;

  type
    TArrayFloat = array of GLfloat;

    TCmds = record
      mode: GLenum;
      v, c, t: TArrayFloat;
    end;

  var
    cmds: TCmds;

  var
    extList: string = '';

  var
    es_glVertexPointer: procedure (size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl;
    es_glColorPointer: procedure (size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl;
    es_glTexCoordPointer: procedure (size: GLint; _type: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl;
    es_glEnableClientState: procedure (arr: GLenum); cdecl;
    es_glDisableClientState: procedure (arr: GLenum); cdecl;
    es_glDrawArrays: procedure (mode: GLenum; first: GLint; count: GLsizei); cdecl;
//    es_glActiveTexture: procedure(texture: GLenum); cdecl;
//    es_glClientActiveTexture: procedure(texture: GLenum); cdecl;
    es_glColor4f: procedure(red, green, blue, alpha: GLfloat); cdecl;

    es_glEnable: procedure (cap: GLenum); cdecl;
    es_glDisable: procedure (cap: GLenum); cdecl;
    es_glIsEnabled: function (cap: GLenum): GLBoolean; cdecl;
    es_glIsTexture: function (texture: GLuint): GLboolean; cdecl;
    es_glGetString: function (name: GLenum): PChar; cdecl;
    es_glGetError: function (): GLenum; cdecl;
    es_glClearColor: procedure (red, green, blue, alpha: GLclampf); cdecl;
    es_glClear: procedure (mask: GLbitfield); cdecl;
    es_glAlphaFunc: procedure (func: GLenum; ref: GLclampf); cdecl;
    es_glBlendFunc: procedure (sfactor, dfactor: GLenum); cdecl;
    es_glPointSize: procedure (size: GLfloat); cdecl;
    es_glLineWidth: procedure (width: GLfloat); cdecl;
    es_glGetIntegerv: procedure (pname: GLenum; params: PGLint); cdecl;
    es_glFlush: procedure; cdecl;
    es_glFinish: procedure; cdecl;

    es_glLoadIdentity: procedure; cdecl;
    es_glMatrixMode: procedure (mode: GLenum); cdecl;
    es_glLoadMatrixf: procedure (m: PGLfloat); cdecl;
    es_glPushMatrix: procedure; cdecl;
    es_glPopMatrix: procedure; cdecl;
    es_glTranslatef: procedure (x, y, z: GLfloat); cdecl;
    es_glRotatef: procedure (angle, x, y, z: GLfloat); cdecl;
    es_glScalef: procedure (x, y, z: GLfloat); cdecl;
    es_glOrthof: procedure (left, right, bottom, top, zNear, zFar: GLfloat); cdecl;

    es_glReadPixels: procedure (x, y: GLint; width, height: GLsizei; format,_type: GLenum; pixels: PGLvoid); cdecl;

    es_glViewport: procedure (x, y: GLint; width, height: GLsizei); cdecl;
    es_glScissor: procedure (x, y: GLint; width, height: GLsizei); cdecl;
    es_glStencilMask: procedure (mask: GLuint); cdecl;
    es_glStencilFunc: procedure (func: GLenum; ref: GLint; mask: GLuint); cdecl;
    es_glStencilOp: procedure (fail, zfail, zpass: GLenum); cdecl;
    es_glColorMask: procedure (red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); cdecl;

    es_glBindTexture: procedure (target: GLenum; texture: GLuint); cdecl;
    es_glGenTextures: procedure (n: GLsizei; textures: PGLuint); cdecl;
    es_glTexEnvi: procedure (target: GLenum; pname: GLenum; param: GLint); cdecl;
    es_glTexParameterf: procedure (target: GLenum; pname: GLenum; param: GLfloat); cdecl;
    es_glTexParameteri: procedure (target: GLenum; pname: GLenum; param: GLint); cdecl;
    es_glTexImage2D: procedure (target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, _type:GLenum; pixels: PGLvoid); cdecl;
    es_glTexSubImage2D: procedure (target: GLenum; level: GLint; xoffset, yoffset: GLint; width, height: GLsizei; format, _type: GLenum; pixels: PGLvoid); cdecl;
    es_glDeleteTextures: procedure (n: GLsizei; textures: PGLuint); cdecl;

    es_glGenFramebuffers: procedure (n: GLsizei; framebuffers: PGLuint); cdecl;
    es_glBindFramebuffer: procedure (target: GLenum; framebuffer: GLuint); cdecl;
    es_glFramebufferTexture2D: procedure (target, attachment, textarget: GLenum; texture: GLuint; level: GLint); cdecl;
    es_glFramebufferRenderbuffer: procedure (target, attachment, rbotarget: GLenum; rbo: GLuint); cdecl;
    es_glCheckFramebufferStatus: function (framebuffer: GLuint): GLenum; cdecl;
    es_glDeleteFramebuffers: procedure (n: GLsizei; const framebuffers: PGLuint); cdecl;

    es_glGenRenderbuffers: procedure (n: GLsizei; framebuffers: PGLuint); cdecl;
    es_glBindRenderbuffer: procedure (target: GLenum; renderbuffer: GLuint); cdecl;
    es_glRenderbufferStorage: procedure (target, internalformat: GLenum; w, h: GLsizei); cdecl;
    es_glDeleteRenderbuffers: procedure (n: GLsizei; const renderbuffers: PGLuint); cdecl;

  procedure glEnable(cap: GLenum);
  begin
    es_glEnable(cap);
  end;

  procedure glDisable(cap: GLenum);
  begin
    es_glDisable(cap);
  end;

  function glIsEnabled(cap: GLenum): GLboolean;
  begin
    result := es_glIsEnabled(cap);
  end;

  function glIsTexture(texture: GLuint): GLboolean;
  begin
    Result := es_glIsTexture(texture);
  end;

  function glGetString(name: GLenum): PChar;
  begin
    result := es_glGetString(name);
  end;

  function glGetError: GLenum;
  begin
    result := es_glGetError
  end;

  procedure glClearColor(red, green, blue, alpha: GLclampf);
  begin
    es_glClearColor(red, green, blue, alpha);
  end;

  procedure glClear(mask: GLbitfield);
  begin
    es_glClear(mask);
  end;

  procedure glAlphaFunc(func: GLenum; ref: GLclampf);
  begin
    es_glAlphaFunc(func, ref);
  end;

  procedure glBlendFunc(sfactor, dfactor: GLenum);
  begin
    es_glBlendFunc(sfactor, dfactor);
  end;

  procedure glPointSize(size: GLfloat);
  begin
    es_glPointSize(size);
  end;

  procedure glLineWidth(width: GLfloat);
  begin
    es_glLineWidth(width);
  end;

  procedure glGetIntegerv(pname: GLenum; params: PGLint);
  begin
    es_glGetIntegerv(pname, params);
  end;

  procedure glFlush;
  begin
    es_glFlush;
  end;

  procedure glFinish;
  begin
    es_glFinish;
  end;





  procedure glBegin(mode: GLenum);
  begin
    assert(cmds.mode = GL_INVALID_ENUM);
    assert((mode = GL_POINTS) or (mode = GL_LINES) or (mode = GL_QUADS));
    cmds.mode := mode;
    SetLength(cmds.v, 0);
    SetLength(cmds.c, 0);
    SetLength(cmds.t, 0);
  end;

  procedure glEnd;
  begin
    assert(cmds.mode <> GL_INVALID_ENUM);
    assert(Length(cmds.v) mod ValPerVertex = 0);
    assert(Length(cmds.c) mod ValPerColor = 0);
    assert(Length(cmds.t) mod ValPerCoord = 0);
    if Length(cmds.c) <> 0 then
      assert(Length(cmds.v) div ValPerVertex = Length(cmds.c) div ValPerColor);
    if Length(cmds.t) <> 0 then
      assert(Length(cmds.v) div ValPerVertex = Length(cmds.t) div ValPerCoord);

    es_glVertexPointer(ValPerVertex, GL_FLOAT, 0, @cmds.v[0]);
    es_glColorPointer(ValPerColor, GL_FLOAT, 0, @cmds.c[0]);
    es_glTexCoordPointer(ValPerCoord, GL_FLOAT, 0, @cmds.t[0]);
    es_glEnableClientState(GL_VERTEX_ARRAY);
    es_glEnableClientState(GL_COLOR_ARRAY);
    es_glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    es_glDisableClientState(GL_NORMAL_ARRAY);

    if Length(cmds.c) = 0 then
      es_glDisableClientState(GL_COLOR_ARRAY);
    if Length(cmds.t) = 0 then
      es_glDisableClientState(GL_TEXTURE_COORD_ARRAY);

    if cmds.mode = GL_QUADS then
      es_glDrawArrays(GL_TRIANGLES, 0, Length(cmds.v) div ValPerVertex)
    else
      es_glDrawArrays(cmds.mode, 0, Length(cmds.v) div ValPerVertex);

    SetLength(cmds.v, 0);
    SetLength(cmds.c, 0);
    SetLength(cmds.t, 0);
    cmds.mode := GL_INVALID_ENUM;
  end;

(* ---------- begin internals ---------- *)

  procedure AddFloatRaw (var x: TArrayFloat; f: GLfloat);
    var i: Integer;
  begin
    i := Length(x);
    SetLength(x, i + 1);
    x[i] := f;
  end;

  procedure AddFloatX (var x: TArrayFloat; v: array of GLfloat); inline;
    var i, j, vpx: Integer;
  begin
    i := Length(x);
    vpx := Length(v);
    if (cmds.mode = GL_QUADS) and (i div vpx mod 4 = 3) then
    begin
      for j := 0 to vpx - 1 do
        AddFloatRaw(x, x[i - 3*vpx + j]);
      for j := 0 to vpx - 1 do
        AddFloatRaw(x, x[i - 1*vpx + j]);
    end;
    for j := 0 to vpx - 1 do
      AddFloatRaw(x, v[j]);
  end;

(* ---------- end internals ---------- *)

  procedure glVertex2f(x, y: GLfloat);
  begin
    AddFloatX(cmds.v, [x, y]);
  end;

  procedure glVertex2i(x, y: GLint);
  begin
    AddFloatX(cmds.v, [x, y]);
  end;

  procedure glColor4f(red, green, blue, alpha: GLfloat);
  begin
    if cmds.mode = GL_INVALID_ENUM then
      es_glColor4f(red, green, blue, alpha)
    else
      AddFloatX(cmds.c, [red, green, blue, alpha])
  end;

  procedure glColor4ub(red, green, blue, alpha: GLubyte);
  begin
    glColor4f(red / 255, green / 255, blue / 255, alpha / 255)
  end;

  procedure glColor3ub(red, green, blue: GLubyte);
  begin
    glColor4f(red / 255, green / 255, blue / 255, 255)
  end;

  procedure glTexCoord2f(s, t: GLfloat);
  begin
    AddFloatX(cmds.t, [s, t]);
  end;

  procedure glTexCoord2i(s, t: GLint);
  begin
    AddFloatX(cmds.t, [s, t]);
  end;







  procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer);
  begin
    es_glReadPixels(x, y, width, height, format, atype, pixels);
  end;






  procedure glLoadIdentity;
  begin
    es_glLoadIdentity;
  end;

  procedure glMatrixMode(mode: GLenum);
  begin
    es_glMatrixMode(mode);
  end;

  procedure glLoadMatrixd(const m: PGLdouble);
    var
      i: Integer;
      n: array [0..15] of GLfloat;
  begin
    for i := 0 to 15 do
      n[i] := m[i];
    es_glLoadMatrixf(@n[0]);
  end;

  procedure glPushMatrix;
  begin
    es_glPushMatrix;
  end;

  procedure glPopMatrix;
  begin
    es_glPopMatrix;
  end;

  procedure glTranslatef(x, y, z: GLfloat);
  begin
    es_glTranslatef(x, y, z);
  end;

  procedure glRotatef(angle, x, y, z: GLfloat);
  begin
    es_glRotatef(angle, x, y, z);
  end;

  procedure glScalef(x, y, z: GLfloat);
  begin
    es_glScalef(x, y, z);
  end;

  procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble);
  begin
    es_glOrthof(left, right, bottom, top, zNear, zFar);
  end;






  procedure glViewport(x, y: GLint; width, height: GLsizei);
  begin
    es_glViewport(x, y, width, height);
  end;

  procedure glScissor(x, y: GLint; width, height: GLsizei);
  begin
    es_glScissor(x, y, width, height);
  end;

  procedure glStencilMask(mask: GLuint);
  begin
    es_glStencilMask(mask);
  end;

  procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint);
  begin
    es_glStencilFunc(func, ref, mask);
  end;

  procedure glStencilOp(fail, zfail, zpass: GLenum);
  begin
    es_glStencilOp(fail, zfail, zpass);
  end;

  procedure glColorMask(red, green, blue, alpha: GLboolean);
  begin
    es_glColorMask(red, green, blue, alpha);
  end;






  procedure glBindTexture(target: GLenum; texture: GLuint);
  begin
    es_glBindTexture(target, texture);
  end;

  procedure glGenTextures(n: GLsizei; textures: PGLuint);
  begin
    es_glGenTextures(n, textures);
  end;

  procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint);
  begin
    es_glTexEnvi(target, pname, param);
  end;

  procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
  begin
    es_glTexParameterf(target, pname, param);
  end;

  procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);
  begin
    es_glTexParameteri(target, pname, param);
  end;

  procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer);
  begin
    es_glTexImage2D(target, level, internalformat, width, height, border, format, atype, pixels);
  end;

  procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer);
  begin
    es_glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, atype, pixels);
  end;

  procedure glDeleteTextures(n: GLsizei; const textures: PGLuint);
  begin
    es_glDeleteTextures(n, textures);
  end;






  procedure glGenFramebuffers(n: GLsizei; framebuffers: PGLuint);
  begin
    es_glGenFramebuffers(n, framebuffers);
  end;

  procedure glBindFramebuffer(target: GLenum; framebuffer: GLuint);
  begin
    es_glBindFramebuffer(target, framebuffer);
  end;

  procedure glFramebufferTexture2D(target, attachment, textarget: GLenum; texture: GLuint; level: GLint);
  begin
    es_glFramebufferTexture2D(target, attachment, textarget, texture, level);
  end;

  procedure glFramebufferRenderbuffer(target, attachment, rbotarget: GLenum; rbo: GLuint);
  begin
    es_glFramebufferRenderbuffer(target, attachment, rbotarget, rbo);
  end;

  function glCheckFramebufferStatus(framebuffer: GLuint): GLenum;
  begin
    result := es_glCheckFramebufferStatus(framebuffer);
  end;

  procedure glDeleteFramebuffers(n: GLsizei; const framebuffers: PGLuint);
  begin
    es_glDeleteFramebuffers(n, framebuffers);
  end;

  procedure glGenRenderbuffers(n: GLsizei; renderbuffers: PGLuint);
  begin
    es_glGenRenderbuffers(n, renderbuffers);
  end;

  procedure glBindRenderbuffer(target: GLenum; renderbuffer: GLuint);
  begin
    es_glBindRenderbuffer(target, renderbuffer);
  end;

  procedure glRenderbufferStorage(target, internalformat: GLenum; w, h: GLsizei);
  begin
    es_glRenderbufferStorage(target, internalformat, w, h);
  end;

  procedure glDeleteRenderbuffers(n: GLsizei; const renderbuffers: PGLuint);
  begin
    es_glDeleteRenderbuffers(n, renderbuffers);
  end;






  procedure nogl_Init;
  begin
    es_glVertexPointer := SDL_GL_GetProcAddress('glVertexPointer');
    es_glColorPointer := SDL_GL_GetProcAddress('glColorPointer');
    es_glTexCoordPointer := SDL_GL_GetProcAddress('glTexCoordPointer');
    es_glEnableClientState := SDL_GL_GetProcAddress('glEnableClientState');
    es_glDisableClientState := SDL_GL_GetProcAddress('glDisableClientState');
    es_glDrawArrays := SDL_GL_GetProcAddress('glDrawArrays');
//    es_glActiveTexture := SDL_GL_GetProcAddress('glActiveTexture');
//    es_glClientActiveTexture := SDL_GL_GetProcAddress('glClientActiveTexture');
    es_glColor4f := SDL_GL_GetProcAddress('glColor4f');

    es_glEnable := SDL_GL_GetProcAddress('glEnable');
    es_glDisable := SDL_GL_GetProcAddress('glDisable');
    es_glIsEnabled := SDL_GL_GetProcAddress('glIsEnabled');
    es_glIsTexture := SDL_GL_GetProcAddress('glIsTexture');
    es_glGetString := SDL_GL_GetProcAddress('glGetString');
    es_glGetError := SDL_GL_GetProcAddress('glGetError');
    es_glClearColor := SDL_GL_GetProcAddress('glClearColor');
    es_glClear := SDL_GL_GetProcAddress('glClear');
    es_glAlphaFunc := SDL_GL_GetProcAddress('glAlphaFunc');
    es_glBlendFunc := SDL_GL_GetProcAddress('glBlendFunc');
    es_glPointSize := SDL_GL_GetProcAddress('glPointSize');
    es_glLineWidth := SDL_GL_GetProcAddress('glLineWidth');
    es_glGetIntegerv := SDL_GL_GetProcAddress('glGetIntegerv');
    es_glFlush := SDL_GL_GetProcAddress('glFlush');
    es_glFinish := SDL_GL_GetProcAddress('glFinish');

    es_glLoadIdentity := SDL_GL_GetProcAddress('glLoadIdentity');
    es_glMatrixMode := SDL_GL_GetProcAddress('glMatrixMode');
    es_glLoadMatrixf := SDL_GL_GetProcAddress('glLoadMatrixf');
    es_glPushMatrix := SDL_GL_GetProcAddress('glPushMatrix');
    es_glPopMatrix := SDL_GL_GetProcAddress('glPopMatrix');
    es_glTranslatef := SDL_GL_GetProcAddress('glTranslatef');
    es_glRotatef := SDL_GL_GetProcAddress('glRotatef');
    es_glScalef := SDL_GL_GetProcAddress('glScalef');
    es_glOrthof := SDL_GL_GetProcAddress('glOrthof');

    es_glReadPixels := SDL_GL_GetProcAddress('glReadPixels');

    es_glViewport := SDL_GL_GetProcAddress('glViewport');
    es_glScissor := SDL_GL_GetProcAddress('glScissor');
    es_glStencilMask := SDL_GL_GetProcAddress('glStencilMask');
    es_glStencilFunc := SDL_GL_GetProcAddress('glStencilFunc');
    es_glStencilOp := SDL_GL_GetProcAddress('glStencilOp');
    es_glColorMask := SDL_GL_GetProcAddress('glColorMask');

    es_glBindTexture := SDL_GL_GetProcAddress('glBindTexture');
    es_glGenTextures := SDL_GL_GetProcAddress('glGenTextures');
    es_glTexEnvi := SDL_GL_GetProcAddress('glTexEnvi');
    es_glTexParameterf := SDL_GL_GetProcAddress('glTexParameterf');
    es_glTexParameteri := SDL_GL_GetProcAddress('glTexParameteri');
    es_glTexImage2D := SDL_GL_GetProcAddress('glTexImage2D');
    es_glTexSubImage2D := SDL_GL_GetProcAddress('glTexSubImage2D');
    es_glDeleteTextures := SDL_GL_GetProcAddress('glDeleteTextures');

    es_glGenFramebuffers := SDL_GL_GetProcAddress('glGenFramebuffersOES');
    es_glBindFramebuffer := SDL_GL_GetProcAddress('glBindFramebufferOES');
    es_glFramebufferTexture2D := SDL_GL_GetProcAddress('glFramebufferTexture2DOES');;
    es_glFramebufferRenderbuffer := SDL_GL_GetProcAddress('glFramebufferRenderbufferOES');
    es_glCheckFramebufferStatus := SDL_GL_GetProcAddress('glCheckFramebufferStatusOES');
    es_glDeleteFramebuffers := SDL_GL_GetProcAddress('glDeleteFramebuffersOES');

    es_glGenRenderbuffers := SDL_GL_GetProcAddress('glGenRenderbuffersOES');
    es_glBindRenderbuffer := SDL_GL_GetProcAddress('glBindRenderbufferOES');
    es_glRenderbufferStorage := SDL_GL_GetProcAddress('glRenderbufferStorageOES');
    es_glDeleteRenderbuffers := SDL_GL_GetProcAddress('glDeleteRenderbuffersOES');

    cmds.mode := GL_INVALID_ENUM
  end;

  procedure nogl_Quit;
  begin
    es_glVertexPointer := nil;
    es_glColorPointer := nil;
    es_glTexCoordPointer := nil;
    es_glEnableClientState := nil;
    es_glDisableClientState := nil;
    es_glDrawArrays := nil;
//    es_glActiveTexture := nil;
//    es_glClientActiveTexture := nil;
    es_glColor4f := nil;

    es_glEnable := nil;
    es_glDisable := nil;
    es_glIsEnabled := nil;
    es_glIsTexture := nil;
    es_glGetString := nil;
    es_glClearColor := nil;
    es_glClear := nil;
    es_glAlphaFunc := nil;
    es_glBlendFunc := nil;
    es_glPointSize := nil;
    es_glLineWidth := nil;
    es_glGetIntegerv := nil;
    es_glFlush := nil;
    es_glFinish := nil;

    es_glLoadIdentity := nil;
    es_glMatrixMode := nil;
    es_glLoadMatrixf := nil;
    es_glPushMatrix := nil;
    es_glPopMatrix := nil;
    es_glTranslatef := nil;
    es_glRotatef := nil;
    es_glScalef := nil;
    es_glOrthof := nil;

    es_glReadPixels := nil;

    es_glViewport := nil;
    es_glScissor := nil;
    es_glStencilMask := nil;
    es_glStencilFunc := nil;
    es_glStencilOp := nil;
    es_glColorMask := nil;

    es_glBindTexture := nil;
    es_glGenTextures := nil;
    es_glTexEnvi := nil;
    es_glTexParameterf := nil;
    es_glTexParameteri := nil;
    es_glTexImage2D := nil;
    es_glTexSubImage2D := nil;
    es_glDeleteTextures := nil;

    es_glGenFramebuffers := nil;
    es_glBindFramebuffer := nil;
    es_glFramebufferTexture2D := nil;
    es_glFramebufferRenderbuffer := nil;
    es_glCheckFramebufferStatus := nil;
    es_glDeleteFramebuffers := nil;

    es_glGenRenderbuffers := nil;
    es_glBindRenderbuffer := nil;
    es_glRenderbufferStorage := nil;
    es_glDeleteRenderbuffers := nil;
  end;

  function nogl_ExtensionSupported(ext: string): Boolean;
  begin
    if (extList = '') and Assigned(es_glGetString) then
      extList := glGetString(GL_EXTENSIONS);
    result := pos(ext, extList) <> 0;
  end;

initialization
