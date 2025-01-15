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

  procedure glEnable(cap: GLenum);
  begin
  end;

  procedure glDisable(cap: GLenum);
  begin
  end;

  function glIsEnabled(cap: GLenum): GLboolean;
  begin
    result := 0
  end;

  function glGetString(name: GLenum): PChar;
  begin
    result := ''
  end;

  function glGetError: GLenum;
  begin
    result := GL_NO_ERROR
  end;

  procedure glClearColor(red, green, blue, alpha: GLclampf);
  begin
  end;

  procedure glClear(mask: GLbitfield);
  begin
  end;

  procedure glAlphaFunc(func: GLenum; ref: GLclampf);
  begin
  end;

  procedure glBlendFunc(sfactor, dfactor: GLenum);
  begin
  end;

  procedure glPointSize(size: GLfloat);
  begin
  end;

  procedure glLineWidth(width: GLfloat);
  begin
  end;

  procedure glGetIntegerv(pname: GLenum; params: PGLint);
  begin
  end;

  procedure glFlush;
  begin
  end;

  procedure glFinish;
  begin
  end;

  procedure glBegin(mode: GLenum);
  begin
  end;

  procedure glEnd;
  begin
  end;

  procedure glVertex2f(x, y: GLfloat);
  begin
  end;

  procedure glVertex2i(x, y: GLint);
  begin
  end;

  procedure glColor4f(red, green, blue, alpha: GLfloat);
  begin
  end;

  procedure glColor4ub(red, green, blue, alpha: GLubyte);
  begin
  end;

  procedure glColor3ub(red, green, blue: GLubyte);
  begin
  end;

  procedure glTexCoord2f(s, t: GLfloat);
  begin
  end;

  procedure glTexCoord2i(s, t: GLint);
  begin
  end;

  procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer);
  begin
  end;

  procedure glLoadIdentity;
  begin
  end;

  procedure glMatrixMode(mode: GLenum);
  begin
  end;

  procedure glLoadMatrixd(const m: PGLdouble);
  begin
  end;

  procedure glPushMatrix;
  begin
  end;

  procedure glPopMatrix;
  begin
  end;

  procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble);
  begin
  end;

  procedure glTranslatef(x, y, z: GLfloat);
  begin
  end;

  procedure glRotatef(angle, x, y, z: GLfloat);
  begin
  end;

  procedure glScalef(x, y, z: GLfloat);
  begin
  end;

  procedure glViewport(x, y: GLint; width, height: GLsizei);
  begin
  end;

  procedure glScissor(x, y: GLint; width, height: GLsizei);
  begin
  end;

  procedure glStencilMask(mask: GLuint);
  begin
  end;

  procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint);
  begin
  end;

  procedure glStencilOp(fail, zfail, zpass: GLenum);
  begin
  end;

  procedure glColorMask(red, green, blue, alpha: GLboolean);
  begin
  end;

  procedure glBindTexture(target: GLenum; texture: GLuint);
  begin
  end;

  procedure glGenTextures(n: GLsizei; textures: PGLuint);
  begin
  end;

  procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint);
  begin
  end;

  procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
  begin
  end;

  procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);
  begin
  end;

  procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer);
  begin
  end;

  procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer);
  begin
  end;

  procedure glDeleteTextures(n: GLsizei; const textures: PGLuint);
  begin
  end;

  procedure glGenFramebuffers(n: GLsizei; framebuffers: PGLuint);
  begin
  end;

  procedure glBindFramebuffer(target: GLenum; framebuffer: GLuint);
  begin
  end;

  procedure glFramebufferTexture2D(target, attachment, textarget: GLenum; texture: GLuint; level: GLint);
  begin
  end;

  procedure glFramebufferRenderbuffer(target, attachment, rbotarget: GLenum; rbo: GLuint);
  begin
  end;

  function glCheckFramebufferStatus(framebuffer: GLuint): GLenum;
  begin
    result := GL_FRAMEBUFFER_UNSUPPORTED
  end;

  procedure glDeleteFramebuffers(n: GLsizei; const framebuffers: PGLuint);
  begin
  end;

  procedure glGenRenderbuffers(n: GLsizei; renderbuffers: PGLuint);
  begin
  end;

  procedure glBindRenderbuffer(target: GLenum; renderbuffer: GLuint);
  begin
  end;

  procedure glRenderbufferStorage(target, internalformat: GLenum; w, h: GLsizei);
  begin
  end;

  procedure glDeleteRenderbuffers(n: GLsizei; const renderbuffers: PGLuint);
  begin
  end;

  procedure nogl_Init;
  begin
  end;

  procedure nogl_Quit;
  begin
  end;

  function nogl_ExtensionSupported(ext: string): Boolean;
  begin
    result := false;
  end;

initialization
