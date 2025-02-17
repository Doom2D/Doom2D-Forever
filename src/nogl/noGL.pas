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

{$INCLUDE ../shared/a_modes.inc}
unit noGL;

interface

  uses ctypes;

  type
    GLenum     = cuint;   PGLenum     = ^GLenum;
    GLboolean  = cuchar;  PGLboolean  = ^GLboolean;
    GLbitfield = cuint;   PGLbitfield = ^GLbitfield;
    GLbyte     = cschar;  PGLbyte     = ^GLbyte;
    GLshort    = cshort;  PGLshort    = ^GLshort;
    GLint      = cint;    PGLint      = ^GLint;
    GLsizei    = cint;    PGLsizei    = ^GLsizei;
    GLubyte    = cuchar;  PGLubyte    = ^GLubyte;
    GLushort   = cushort; PGLushort   = ^GLushort;
    GLuint     = cuint;   PGLuint     = ^GLuint;
    GLfloat    = cfloat;  PGLfloat    = ^GLfloat;
    GLclampf   = cfloat;  PGLclampf   = ^GLclampf;
    GLdouble   = cdouble; PGLdouble   = ^GLdouble;
    GLclampd   = cdouble; PGLclampd   = ^GLclampd;
  { GLvoid     = void; }  PGLvoid     = Pointer;     PPGLvoid = ^PGLvoid;
    GLfixed    = cint;    PGLfixed    = ^Integer;
    GLclampx   = cint;    PGLclampx   = ^Integer;

    TGLenum     = GLenum;
    TGLboolean  = GLboolean;
    TGLbitfield = GLbitfield;
    TGLbyte     = GLbyte;
    TGLshort    = GLshort;
    TGLint      = GLint;
    TGLsizei    = GLsizei;
    TGLubyte    = GLubyte;
    TGLushort   = GLushort;
    TGLuint     = GLuint;
    TGLfloat    = GLfloat;
    TGLclampf   = GLclampf;
    TGLdouble   = GLdouble;
    TGLclampd   = GLclampd;
    TGLfixed    = GLfixed;
    TGLclampx   = GLclampx;

  const
    GL_NO_ERROR = 0;
    GL_NEAREST = $2600;
    GL_DEPTH_TEST = $0B71;
    GL_SCISSOR_TEST = $0C11;
    GL_MODELVIEW = $1700;
    GL_PROJECTION = $1701;
    GL_BLEND = $0BE2;
    GL_SRC_ALPHA = $0302;
    GL_ONE_MINUS_SRC_ALPHA = $0303;
    GL_ONE = $1;
    GL_TEXTURE_2D = $0DE1;
    GL_QUADS = $0007;
    GL_ZERO = $0;
    GL_POINTS = $0000;
    GL_LINES = $0001;
    GL_DST_COLOR = $0306;
    GL_SRC_COLOR = $0300;
    GL_ONE_MINUS_DST_COLOR = $0307;
    GL_GREATER = $0204;
    GL_COLOR_BUFFER_BIT = $00004000;
    GL_RGB = $1907;
    GL_UNSIGNED_BYTE = $1401;
    GL_ALPHA_TEST = $0BC0;
    GL_VIEWPORT = $0BA2;
    GL_TEXTURE_WRAP_S = $2802;
    GL_TEXTURE_WRAP_T = $2803;
    GL_REPEAT = $2901;
    GL_TEXTURE_MIN_FILTER = $2801;
    GL_TEXTURE_MAG_FILTER = $2800;
    GL_RGBA = $1908;
    GL_POINT_SMOOTH = $0B10;
    GL_STENCIL_TEST = $0B90;
    GL_SCISSOR_BOX = $0C10;
    GL_FALSE = $0;
    GL_TRUE = $1;
    GL_ALWAYS = $0207;
    GL_STENCIL_BUFFER_BIT = $00000400;
    GL_EQUAL = $0202;
    GL_KEEP = $1E00;
    GL_INCR = $1E02;
    GL_LINEAR = $2601;
    GL_TEXTURE_ENV = $2300;
    GL_TEXTURE_ENV_MODE = $2200;
    GL_MODULATE = $2100;

    GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
    GL_FRAMEBUFFER_DEFAULT = $8218;
    GL_FRAMEBUFFER_UNDEFINED = $8219;
    GL_DEPTH_STENCIL_ATTACHMENT = $821A;
    GL_MAX_RENDERBUFFER_SIZE = $84E8;
    GL_DEPTH_STENCIL = $84F9;
    GL_DEPTH_COMPONENT16 = $81A5;
    GL_DEPTH24_STENCIL8 = $88F0;
    GL_FRAMEBUFFER_BINDING = $8CA6;
    GL_DRAW_FRAMEBUFFER_BINDING = GL_FRAMEBUFFER_BINDING;
    GL_RENDERBUFFER_BINDING = $8CA7;
    GL_READ_FRAMEBUFFER = $8CA8;
    GL_DRAW_FRAMEBUFFER = $8CA9;
    GL_READ_FRAMEBUFFER_BINDING = $8CAA;
    GL_RENDERBUFFER_SAMPLES = $8CAB;
    GL_FRAMEBUFFER_COMPLETE = $8CD5;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
    GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
    GL_MAX_COLOR_ATTACHMENTS = $8CDF;
    GL_COLOR_ATTACHMENT0 = $8CE0;
    GL_DEPTH_ATTACHMENT = $8D00;
    GL_STENCIL_ATTACHMENT = $8D20;
    GL_FRAMEBUFFER = $8D40;
    GL_RENDERBUFFER = $8D41;
    GL_STENCIL_INDEX1 = $8D46;
    GL_STENCIL_INDEX4 = $8D47;
    GL_STENCIL_INDEX8 = $8D48;
    GL_MAX_SAMPLES = $8D57;

    GL_VENDOR = $1F00;
    GL_RENDERER = $1F01;
    GL_VERSION = $1F02;
    GL_EXTENSIONS = $1F03;
    GL_SHADING_LANGUAGE_VERSION = $8B8C;

//  const
//    GL_CULL_FACE = $0B44;
//    GL_FLOAT = $1406;
//    GL_VERTEX_ARRAY = $8074;
//    GL_NORMAL_ARRAY = $8075;
//    GL_COLOR_ARRAY = $8076;
//    GL_TEXTURE_COORD_ARRAY = $8078;
//    GL_NOTEQUAL = $0205;
//    GL_LINE_SMOOTH = $0B20;
//    GL_POLYGON_SMOOTH = $0B41;
//    GL_LIGHTING = $0B50;
//    GL_DITHER = $0BD0;
//    GL_MODELVIEW_MATRIX = $0BA6;
//    GL_MATRIX_MODE = $0BA0;
//    GL_TEXTURE_BINDING_2D = $8069;
//    GL_TEXTURE = $1702;
//    GL_COLOR = $1800;
//    GL_ALL_ATTRIB_BITS = $000FFFFF;

  procedure glEnable(cap: GLenum); inline;
  procedure glDisable(cap: GLenum); inline;
  function  glIsEnabled(cap: GLenum): GLboolean; inline;
  function glIsTexture(texture: GLuint): GLboolean; inline;
  function  glGetString(name: GLenum): PChar; inline;
  function  glGetError: GLenum; inline;
  procedure glClearColor(red, green, blue, alpha: GLclampf); inline;
  procedure glClear(mask: GLbitfield); inline;
  procedure glAlphaFunc(func: GLenum; ref: GLclampf); inline;
  procedure glBlendFunc(sfactor, dfactor: GLenum); inline;
  procedure glPointSize(size: GLfloat); inline;
  procedure glLineWidth(width: GLfloat); inline;
  procedure glGetIntegerv(pname: GLenum; params: PGLint); inline;
  procedure glFlush; inline;
  procedure glFinish; inline;

  procedure glBegin(mode: GLenum); inline;
  procedure glEnd; inline;
  procedure glTexCoord2f(s, t: GLfloat); inline;
  procedure glVertex2i(x, y: GLint); inline;
  procedure glColor4ub(red, green, blue, alpha: GLubyte); inline;
  procedure glColor3ub(red, green, blue: GLubyte); inline;
  procedure glVertex2f(x, y: GLfloat); inline;
  procedure glTexCoord2i(s, t: GLint); inline;
  procedure glColor4f(red, green, blue, alpha: GLfloat); inline;

  procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); inline;

  procedure glLoadIdentity; inline;
  procedure glScissor(x, y: GLint; width, height: GLsizei); inline;
  procedure glViewport(x, y: GLint; width, height: GLsizei); inline;
  procedure glMatrixMode(mode: GLenum); inline;
  procedure glLoadMatrixd(const m: PGLdouble); inline;
  procedure glPushMatrix; inline;
  procedure glTranslatef(x, y, z: GLfloat); inline;
  procedure glRotatef(angle, x, y, z: GLfloat); inline;
  procedure glScalef(x, y, z: GLfloat); inline;
  procedure glPopMatrix; inline;
  procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble); inline;

  procedure glStencilMask(mask: GLuint); inline;
  procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint); inline;
  procedure glStencilOp(fail, zfail, zpass: GLenum); inline;
  procedure glColorMask(red, green, blue, alpha: GLboolean); inline;
  
  procedure glBindTexture(target: GLenum; texture: GLuint); inline;
  procedure glGenTextures(n: GLsizei; textures: PGLuint); inline;
  procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint); inline;
  procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat); inline;
  procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint); inline;
  procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); inline;
  procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); inline;
  procedure glDeleteTextures(n: GLsizei; const textures: PGLuint); inline;

  procedure glGenFramebuffers(n: GLsizei; framebuffers: PGLuint); inline;
  procedure glBindFramebuffer(target: GLenum; framebuffer: GLuint); inline;
  procedure glFramebufferTexture2D(target, attachment, textarget: GLenum; texture: GLuint; level: GLint); inline;
  procedure glFramebufferRenderbuffer(target, attachment, rbotarget: GLenum; rbo: GLuint); inline;
  function glCheckFramebufferStatus(framebuffer: GLuint): GLenum; inline;
  procedure glDeleteFramebuffers(n: GLsizei; const framebuffers: PGLuint); inline;

  procedure glGenRenderbuffers(n: GLsizei; renderbuffers: PGLuint); inline;
  procedure glBindRenderbuffer(target: GLenum; renderbuffer: GLuint); inline;
  procedure glRenderbufferStorage(target, internalformat: GLenum; w, h: GLsizei); inline;
  procedure glDeleteRenderbuffers(n: GLsizei; const renderbuffers: PGLuint); inline;

//  procedure glVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); inline;
//  procedure glEnableClientState(aarray: GLenum); inline;
//  procedure glDisableClientState(aarray: GLenum); inline;
//  procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); inline;
//  procedure glGetFloatv(pname: GLenum; params: PGLfloat); inline;
//  procedure glPushAttrib(mask: GLbitfield); inline;
//  procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble); inline;
//  procedure glPopAttrib; inline;
//  procedure glColorPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); inline;

  procedure nogl_Init;
  procedure nogl_Quit;
  function nogl_ExtensionSupported(ext: string): Boolean;

{$IFDEF USE_GLES1}
  {$I noGLES1.inc}
{$ENDIF}
{$IFDEF USE_GLSTUB}
  {$I noGLSTUB.inc}
{$ENDIF}

end.
