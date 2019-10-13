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

  procedure glEnable(cap: GLenum);
  procedure glDisable(cap: GLenum);
  function  glIsEnabled(cap: GLenum): GLboolean;
  function  glGetString(name: GLenum): PChar;
  procedure glClearColor(red, green, blue, alpha: GLclampf);
  procedure glClear(mask: GLbitfield);
  procedure glAlphaFunc(func: GLenum; ref: GLclampf);
  procedure glBlendFunc(sfactor, dfactor: GLenum);
  procedure glPointSize(size: GLfloat);
  procedure glLineWidth(width: GLfloat);
  procedure glGetIntegerv(pname: GLenum; params: PGLint);
  procedure glFlush;
  procedure glFinish;

  procedure glBegin(mode: GLenum);
  procedure glEnd;
  procedure glTexCoord2f(s, t: GLfloat);
  procedure glVertex2i(x, y: GLint);
  procedure glColor4ub(red, green, blue, alpha: GLubyte);
  procedure glColor3ub(red, green, blue: GLubyte);
  procedure glVertex2f(x, y: GLfloat);
  procedure glTexCoord2i(s, t: GLint);
  procedure glColor4f(red, green, blue, alpha: GLfloat);

  procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer);

  procedure glLoadIdentity;
  procedure glScissor(x, y: GLint; width, height: GLsizei);
  procedure glViewport(x, y: GLint; width, height: GLsizei);
  procedure glMatrixMode(mode: GLenum);
  procedure glLoadMatrixd(const m: PGLdouble);
  procedure glPushMatrix;
  procedure glTranslatef(x, y, z: GLfloat);
  procedure glRotatef(angle, x, y, z: GLfloat);
  procedure glScalef(x, y, z: GLfloat);
  procedure glPopMatrix;

  procedure glStencilMask(mask: GLuint);
  procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint);
  procedure glStencilOp(fail, zfail, zpass: GLenum);
  procedure glColorMask(red, green, blue, alpha: GLboolean);
  
  procedure glBindTexture(target: GLenum; texture: GLuint);
  procedure glGenTextures(n: GLsizei; textures: PGLuint);
  procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint);
  procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
  procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);
  procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer);
  procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer);
  procedure glDeleteTextures(n: GLsizei; const textures: PGLuint);

//  procedure glVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer);
//  procedure glEnableClientState(aarray: GLenum);
//  procedure glDisableClientState(aarray: GLenum);
//  procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei);
//  procedure glGetFloatv(pname: GLenum; params: PGLfloat);
//  procedure glPushAttrib(mask: GLbitfield);
//  procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble);
//  procedure glPopAttrib;
//  procedure glColorPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer);    

  procedure nogl_Init;
  procedure nogl_Quit;

{$IFDEF USE_GLES1}
  {$I noGLES1.inc}
{$ENDIF}
{$IFDEF USE_GLSTUB}
  {$I noGLSTUB.inc}
{$ENDIF}

end.
