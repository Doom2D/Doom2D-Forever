(*
 * Copyright (C) 2007-2009 Olli Hinkka
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *)

(* Sources: https://github.com/FWGS/nanogl *)
unit nanoGL;

interface

uses ctypes;

const
{$IF DEFINED(WINDOWS)}
  nanoGL_LibName = 'nanoGL.dll';
{$ELSEIF DEFINED(DARWIN)}
  nanoGL_LibName = 'libnanoGL.dylib';
{$ELSEIF DEFINED(UNIX)}
  nanoGL_LibName = 'libnanoGL.so';
{$ENDIF}

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;
                              PPGLvoid    = ^PGLvoid;
  GLfixed    = Integer;       PGLfixed    = ^Integer;
  GLclampx   = Integer;       PGLclampx   = ^Integer;

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

(* Boolean values *)
  GL_FALSE = $0;
  GL_TRUE = $1;

(* Data types *)
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_2_BYTES = $1407;
  GL_3_BYTES = $1408;
  GL_4_BYTES = $1409;
  GL_DOUBLE = $140A;

(* StringName *)
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;

(* TextureEnvMode *)
  GL_MODULATE = $2100;
  GL_DECAL = $2101;
(*      GL_BLEND *)
  GL_ADD = $0104;
(*      GL_REPLACE *)

(* Primitives *)
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
  GL_QUADS = $0007;
  GL_QUAD_STRIP = $0008;
  GL_POLYGON = $0009;

(* EnableCap *)
//  GL_FOG = $0B60;
//  GL_LIGHTING = $0B50;
//  GL_TEXTURE_2D = $0DE1;
//  GL_CULL_FACE = $0B44;
  GL_ALPHA_TEST = $0BC0;
//  GL_BLEND = $0BE2;
//  GL_COLOR_LOGIC_OP = $0BF2;
//  GL_DITHER = $0BD0;
//  GL_STENCIL_TEST = $0B90;
//  GL_DEPTH_TEST = $0B71;
(*      GL_LIGHT0 *)
(*      GL_LIGHT1 *)
(*      GL_LIGHT2 *)
(*      GL_LIGHT3 *)
(*      GL_LIGHT4 *)
(*      GL_LIGHT5 *)
(*      GL_LIGHT6 *)
(*      GL_LIGHT7 *)
  GL_POINT_SMOOTH = $0B10;
  GL_LINE_SMOOTH = $0B20;
  GL_SCISSOR_TEST = $0C11;
//  GL_COLOR_MATERIAL = $0B57;
//  GL_NORMALIZE = $0BA1;
  GL_RESCALE_NORMAL = $803A;
//  GL_POLYGON_OFFSET_FILL = $8037;
  GL_VERTEX_ARRAY = $8074;
  GL_NORMAL_ARRAY = $8075;
  GL_COLOR_ARRAY = $8076;
  GL_TEXTURE_COORD_ARRAY = $8078;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;

(* Texture mapping *)
  GL_TEXTURE_ENV = $2300;
  GL_TEXTURE_ENV_MODE = $2200;
  GL_TEXTURE_1D = $0DE0;
  GL_TEXTURE_2D = $0DE1;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_ENV_COLOR = $2201;
  GL_TEXTURE_GEN_S = $0C60;
  GL_TEXTURE_GEN_T = $0C61;
  GL_TEXTURE_GEN_MODE = $2500;
  GL_TEXTURE_BORDER_COLOR = $1004;
  GL_TEXTURE_WIDTH = $1000;
  GL_TEXTURE_HEIGHT = $1001;
  GL_TEXTURE_BORDER = $1005;
  GL_TEXTURE_COMPONENTS = $1003;
  GL_TEXTURE_RED_SIZE = $805C;
  GL_TEXTURE_GREEN_SIZE = $805D;
  GL_TEXTURE_BLUE_SIZE = $805E;
  GL_TEXTURE_ALPHA_SIZE = $805F;
  GL_TEXTURE_LUMINANCE_SIZE = $8060;
  GL_TEXTURE_INTENSITY_SIZE = $8061;
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  GL_OBJECT_LINEAR = $2401;
  GL_OBJECT_PLANE = $2501;
  GL_EYE_LINEAR = $2400;
  GL_EYE_PLANE = $2502;
  GL_SPHERE_MAP = $2402;
//  GL_DECAL = $2101;
//  GL_MODULATE = $2100;
  GL_NEAREST = $2600;
  GL_REPEAT = $2901;
  GL_CLAMP = $2900;
  GL_S = $2000;
  GL_T = $2001;
  GL_R = $2002;
  GL_Q = $2003;
  GL_TEXTURE_GEN_R = $0C62;
  GL_TEXTURE_GEN_Q = $0C63;
  GL_CLAMP_TO_EDGE = $812F;

(* Matrix Mode *)
  GL_MATRIX_MODE = $0BA0;
  GL_MODELVIEW = $1700;
  GL_PROJECTION = $1701;
  GL_TEXTURE = $1702;

(* Buffers, Pixel Drawing/Reading *)
  GL_NONE = $0;
  GL_LEFT = $0406;
  GL_RIGHT = $0407;
(*GL_FRONT					0x0404 *)
(*GL_BACK					0x0405 *)
(*GL_FRONT_AND_BACK				0x0408 *)
  GL_FRONT_LEFT = $0400;
  GL_FRONT_RIGHT = $0401;
  GL_BACK_LEFT = $0402;
  GL_BACK_RIGHT = $0403;
  GL_AUX0 = $0409;
  GL_AUX1 = $040A;
  GL_AUX2 = $040B;
  GL_AUX3 = $040C;
  GL_COLOR_INDEX = $1900;
  GL_RED = $1903;
  GL_GREEN = $1904;
  GL_BLUE = $1905;
  GL_ALPHA = $1906;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;
  GL_ALPHA_BITS = $0D55;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_INDEX_BITS = $0D51;
//  GL_SUBPIXEL_BITS = $0D50;
  GL_AUX_BUFFERS = $0C00;
  GL_READ_BUFFER = $0C02;
  GL_DRAW_BUFFER = $0C01;
  GL_DOUBLEBUFFER = $0C32;
  GL_STEREO = $0C33;
  GL_BITMAP = $1A00;
  GL_COLOR = $1800;
  GL_DEPTH = $1801;
  GL_STENCIL = $1802;
  GL_DITHER = $0BD0;
  GL_RGB = $1907;
  GL_RGBA = $1908;

(* Fog *)
  GL_FOG = $0B60;
  GL_FOG_MODE = $0B65;
  GL_FOG_DENSITY = $0B62;
  GL_FOG_COLOR = $0B66;
  GL_FOG_INDEX = $0B61;
  GL_FOG_START = $0B63;
  GL_FOG_END = $0B64;
  GL_LINEAR = $2601;
  GL_EXP = $0800;
  GL_EXP2 = $0801;

(* Polygons *)
  GL_POINT = $1B00;
  GL_LINE = $1B01;
  GL_FILL = $1B02;
  GL_CW = $0900;
  GL_CCW = $0901;
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_POLYGON_MODE = $0B40;
  GL_POLYGON_SMOOTH = $0B41;
  GL_POLYGON_STIPPLE = $0B42;
  GL_EDGE_FLAG = $0B43;
  GL_CULL_FACE = $0B44;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_POLYGON_OFFSET_UNITS = $2A00;
  GL_POLYGON_OFFSET_POINT = $2A01;
  GL_POLYGON_OFFSET_LINE = $2A02;
  GL_POLYGON_OFFSET_FILL = $8037;

(* Lighting *)
  GL_LIGHTING = $0B50;
  GL_LIGHT0 = $4000;
  GL_LIGHT1 = $4001;
  GL_LIGHT2 = $4002;
  GL_LIGHT3 = $4003;
  GL_LIGHT4 = $4004;
  GL_LIGHT5 = $4005;
  GL_LIGHT6 = $4006;
  GL_LIGHT7 = $4007;
  GL_SPOT_EXPONENT = $1205;
  GL_SPOT_CUTOFF = $1206;
  GL_CONSTANT_ATTENUATION = $1207;
  GL_LINEAR_ATTENUATION = $1208;
  GL_QUADRATIC_ATTENUATION = $1209;
  GL_AMBIENT = $1200;
  GL_DIFFUSE = $1201;
  GL_SPECULAR = $1202;
  GL_SHININESS = $1601;
  GL_EMISSION = $1600;
  GL_POSITION = $1203;
  GL_SPOT_DIRECTION = $1204;
  GL_AMBIENT_AND_DIFFUSE = $1602;
  GL_COLOR_INDEXES = $1603;
  GL_LIGHT_MODEL_TWO_SIDE = $0B52;
  GL_LIGHT_MODEL_LOCAL_VIEWER = $0B51;
  GL_LIGHT_MODEL_AMBIENT = $0B53;
  GL_FRONT_AND_BACK = $0408;
  GL_SHADE_MODEL = $0B54;
  GL_FLAT = $1D00;
  GL_SMOOTH = $1D01;
  GL_COLOR_MATERIAL = $0B57;
  GL_COLOR_MATERIAL_FACE = $0B55;
  GL_COLOR_MATERIAL_PARAMETER = $0B56;
  GL_NORMALIZE = $0BA1;

(* Blending *)
  GL_BLEND = $0BE2;
  GL_BLEND_SRC = $0BE1;
  GL_BLEND_DST = $0BE0;
  GL_ZERO = $0;
  GL_ONE = $1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;

(* ClipPlaneName *)
  GL_CLIP_PLANE0 = $3000;
  GL_CLIP_PLANE1 = $3001;
  GL_CLIP_PLANE2 = $3002;
  GL_CLIP_PLANE3 = $3003;
  GL_CLIP_PLANE4 = $3004;
  GL_CLIP_PLANE5 = $3005;

(* OpenGL 1.1 *)
  GL_PROXY_TEXTURE_1D = $8063;
  GL_PROXY_TEXTURE_2D = $8064;
  GL_TEXTURE_PRIORITY = $8066;
  GL_TEXTURE_RESIDENT = $8067;
  GL_TEXTURE_BINDING_1D = $8068;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_TEXTURE_INTERNAL_FORMAT = $1003;
  GL_ALPHA4 = $803B;
  GL_ALPHA8 = $803C;
  GL_ALPHA12 = $803D;
  GL_ALPHA16 = $803E;
  GL_LUMINANCE4 = $803F;
  GL_LUMINANCE8 = $8040;
  GL_LUMINANCE12 = $8041;
  GL_LUMINANCE16 = $8042;
  GL_LUMINANCE4_ALPHA4 = $8043;
  GL_LUMINANCE6_ALPHA2 = $8044;
  GL_LUMINANCE8_ALPHA8 = $8045;
  GL_LUMINANCE12_ALPHA4 = $8046;
  GL_LUMINANCE12_ALPHA12 = $8047;
  GL_LUMINANCE16_ALPHA16 = $8048;
  GL_INTENSITY = $8049;
  GL_INTENSITY4 = $804A;
  GL_INTENSITY8 = $804B;
  GL_INTENSITY12 = $804C;
  GL_INTENSITY16 = $804D;
  GL_R3_G3_B2 = $2A10;
  GL_RGB4 = $804F;
  GL_RGB5 = $8050;
  GL_RGB8 = $8051;
  GL_RGB10 = $8052;
  GL_RGB12 = $8053;
  GL_RGB16 = $8054;
  GL_RGBA2 = $8055;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGBA8 = $8058;
  GL_RGB10_A2 = $8059;
  GL_RGBA12 = $805A;
  GL_RGBA16 = $805B;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;

  GL_CLIENT_PIXEL_STORE_BIT = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT = $00000002;
  GL_ALL_CLIENT_ATTRIB_BITS = $FFFFFFFF;
  GL_CLIENT_ALL_ATTRIB_BITS = $FFFFFFFF;

(* Stencil *)
  GL_STENCIL_TEST = $0B90;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_STENCIL_BITS = $0D57;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_INDEX = $1901;
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;

(* Hints *)
  GL_FOG_HINT = $0C54;
  GL_LINE_SMOOTH_HINT = $0C52;
  GL_PERSPECTIVE_CORRECTION_HINT = $0C50;
  GL_POINT_SMOOTH_HINT = $0C51;
  GL_POLYGON_SMOOTH_HINT = $0C53;
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;

(* Gets *)
  GL_ATTRIB_STACK_DEPTH = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH = $0BB1;
//  GL_COLOR_CLEAR_VALUE = $0C22;
//  GL_COLOR_WRITEMASK = $0C23;
  GL_CURRENT_INDEX = $0B01;
//  GL_CURRENT_COLOR = $0B00;
//  GL_CURRENT_NORMAL = $0B02;
  GL_CURRENT_RASTER_COLOR = $0B04;
  GL_CURRENT_RASTER_DISTANCE = $0B09;
  GL_CURRENT_RASTER_INDEX = $0B05;
  GL_CURRENT_RASTER_POSITION = $0B07;
  GL_CURRENT_RASTER_TEXTURE_COORDS = $0B06;
  GL_CURRENT_RASTER_POSITION_VALID = $0B08;
//  GL_CURRENT_TEXTURE_COORDS = $0B03;
  GL_INDEX_CLEAR_VALUE = $0C20;
  GL_INDEX_MODE = $0C30;
  GL_INDEX_WRITEMASK = $0C21;
  GL_MODELVIEW_MATRIX = $0BA6;
  GL_MODELVIEW_STACK_DEPTH = $0BA3;
  GL_NAME_STACK_DEPTH = $0D70;
  GL_PROJECTION_MATRIX = $0BA7;
//  GL_PROJECTION_STACK_DEPTH = $0BA4;
  GL_RENDER_MODE = $0C40;
  GL_RGBA_MODE = $0C31;
  GL_TEXTURE_MATRIX = $0BA8;
//  GL_TEXTURE_STACK_DEPTH = $0BA5;
  GL_VIEWPORT = $0BA2;

(* glPush/PopAttrib bits *)
  GL_CURRENT_BIT = $00000001;
  GL_POINT_BIT = $00000002;
  GL_LINE_BIT = $00000004;
  GL_POLYGON_BIT = $00000008;
  GL_POLYGON_STIPPLE_BIT = $00000010;
  GL_PIXEL_MODE_BIT = $00000020;
  GL_LIGHTING_BIT = $00000040;
  GL_FOG_BIT = $00000080;
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_ACCUM_BUFFER_BIT = $00000200;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_VIEWPORT_BIT = $00000800;
  GL_TRANSFORM_BIT = $00001000;
  GL_ENABLE_BIT = $00002000;
  GL_COLOR_BUFFER_BIT = $00004000;
  GL_HINT_BIT = $00008000;
  GL_EVAL_BIT = $00010000;
  GL_LIST_BIT = $00020000;
  GL_TEXTURE_BIT = $00040000;
  GL_SCISSOR_BIT = $00080000;
  GL_ALL_ATTRIB_BITS = $000FFFFF;

(* Depth buffer *)
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;
  GL_DEPTH_TEST = $0B71;
  GL_DEPTH_BITS = $0D56;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_COMPONENT = $1902;

(* TextureUnit *)
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE = $84E1;

(* GetPName *)
  GL_CURRENT_COLOR = $0B00;
  GL_CURRENT_NORMAL = $0B02;
  GL_CURRENT_TEXTURE_COORDS = $0B03;
  GL_POINT_SIZE = $0B11;
  GL_POINT_SIZE_MIN = $8126;
  GL_POINT_SIZE_MAX = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_POINT_DISTANCE_ATTENUATION = $8129;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_LINE_WIDTH = $0B21;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
//  GL_CULL_FACE_MODE = $0B45;
//  GL_FRONT_FACE = $0B46;
//  GL_SHADE_MODEL = $0B54;
//  GL_DEPTH_RANGE = $0B70;
//  GL_DEPTH_WRITEMASK = $0B72;
//  GL_DEPTH_CLEAR_VALUE = $0B73;
//  GL_DEPTH_FUNC = $0B74;
//  GL_STENCIL_CLEAR_VALUE = $0B91;
//  GL_STENCIL_FUNC = $0B92;
//  GL_STENCIL_VALUE_MASK = $0B93;
//  GL_STENCIL_FAIL = $0B94;
//  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
//  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
//  GL_STENCIL_REF = $0B97;
//  GL_STENCIL_WRITEMASK = $0B98;
//  GL_MATRIX_MODE = $0BA0;
//  GL_VIEWPORT = $0BA2;
//  GL_MODELVIEW_STACK_DEPTH = $0BA3;
  GL_PROJECTION_STACK_DEPTH = $0BA4;
  GL_TEXTURE_STACK_DEPTH = $0BA5;
//  GL_MODELVIEW_MATRIX = $0BA6;
//  GL_PROJECTION_MATRIX = $0BA7;
//  GL_TEXTURE_MATRIX = $0BA8;
  GL_ALPHA_TEST_FUNC = $0BC1;
  GL_ALPHA_TEST_REF = $0BC2;
//  GL_BLEND_DST = $0BE0;
//  GL_BLEND_SRC = $0BE1;
  GL_LOGIC_OP_MODE = $0BF0;
  GL_SCISSOR_BOX = $0C10;
//  GL_SCISSOR_TEST = $0C11;
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAX_LIGHTS = $0D31;
  GL_MAX_CLIP_PLANES = $0D32;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_MODELVIEW_STACK_DEPTH = $0D36;
  GL_MAX_PROJECTION_STACK_DEPTH = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH = $0D39;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_MAX_TEXTURE_UNITS = $84E2;
  GL_SUBPIXEL_BITS = $0D50;
//  GL_RED_BITS = $0D52;
//  GL_GREEN_BITS = $0D53;
//  GL_BLUE_BITS = $0D54;
//  GL_ALPHA_BITS = $0D55;
//  GL_DEPTH_BITS = $0D56;
//  GL_STENCIL_BITS = $0D57;
//  GL_POLYGON_OFFSET_UNITS = $2A00;
//  GL_POLYGON_OFFSET_FILL = $8037;
//  GL_POLYGON_OFFSET_FACTOR = $8038;
//  GL_TEXTURE_BINDING_2D = $8069;
  GL_VERTEX_ARRAY_SIZE = $807A;
  GL_VERTEX_ARRAY_TYPE = $807B;
  GL_VERTEX_ARRAY_STRIDE = $807C;
  GL_NORMAL_ARRAY_TYPE = $807E;
  GL_NORMAL_ARRAY_STRIDE = $807F;
  GL_COLOR_ARRAY_SIZE = $8081;
  GL_COLOR_ARRAY_TYPE = $8082;
  GL_COLOR_ARRAY_STRIDE = $8083;
  GL_TEXTURE_COORD_ARRAY_SIZE = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE = $808A;
  GL_VERTEX_ARRAY_POINTER = $808E;
  GL_NORMAL_ARRAY_POINTER = $808F;
  GL_COLOR_ARRAY_POINTER = $8090;
  GL_TEXTURE_COORD_ARRAY_POINTER = $8092;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;

(* ErrorCode *)
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_STACK_OVERFLOW = $0503;
  GL_STACK_UNDERFLOW = $0504;
  GL_OUT_OF_MEMORY = $0505;

(* Macros *)
procedure glVertex2i(x, y: GLint);
procedure glTexCoord2d(s, t: GLdouble);
procedure glVertex3d(x, y, z: GLdouble);

(* This Port Specific *)
procedure glLoadMatrixd(const m: PGLdouble);
procedure glTexCoord2i(s, t: GLint);

procedure glBegin(mode: GLenum); cdecl; external nanoGL_LibName;
procedure glEnd; cdecl; external nanoGL_LibName;
procedure glEnable(cap: GLenum); cdecl; external nanoGL_LibName;
procedure glDisable(cap: GLenum); cdecl; external nanoGL_LibName;
procedure glVertex2f(x, y: GLfloat); cdecl; external nanoGL_LibName;
procedure glColor3f(red, green, blue: GLfloat); cdecl; external nanoGL_LibName;
procedure glTexCoord2f(s, t: GLfloat); cdecl; external nanoGL_LibName;
procedure glViewport(x, y: GLint; width, height: GLsizei); cdecl; external nanoGL_LibName;
procedure glLoadIdentity; cdecl; external nanoGL_LibName;
procedure glColor4f(red, green, blue, alpha: GLfloat); cdecl; external nanoGL_LibName;
procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble); cdecl; external nanoGL_LibName;
procedure glMatrixMode(mode: GLenum); cdecl; external nanoGL_LibName;
//procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); cdecl; external nanoGL_LibName;
procedure glDrawBuffer(mode: GLenum); cdecl; external nanoGL_LibName;
procedure glTranslatef(x, y, z: GLfloat); cdecl; external nanoGL_LibName;
procedure glRotatef(angle, x, y, z: GLfloat); cdecl; external nanoGL_LibName;
procedure glScalef(x, y, z: GLfloat); cdecl; external nanoGL_LibName;
procedure glDepthRange(zNear, zFar: GLclampd); cdecl; external nanoGL_LibName;
procedure glDepthFunc(func: GLenum); cdecl; external nanoGL_LibName;
procedure glFinish; cdecl; external nanoGL_LibName;
procedure glGetFloatv(pname: GLenum; params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glGetIntegerv(pname: GLenum; params: PGLint); cdecl; external nanoGL_LibName;
procedure glCullFace(mode: GLenum); cdecl; external nanoGL_LibName;
procedure glFrustum(left, right, bottom, top, zNear, zFar: GLdouble); cdecl; external nanoGL_LibName;
procedure glClear(mask: GLbitfield); cdecl; external nanoGL_LibName;
procedure glVertex3f(x, y, z: GLfloat); cdecl; external nanoGL_LibName;
procedure glColor4fv(const v: PGLfloat); cdecl; external nanoGL_LibName;
procedure glHint(target, mode: GLenum); cdecl; external nanoGL_LibName;
procedure glBlendFunc(sfactor, dfactor: GLenum); cdecl; external nanoGL_LibName;
procedure glPopMatrix; cdecl; external nanoGL_LibName;
procedure glShadeModel(mode: GLenum); cdecl; external nanoGL_LibName;
procedure glPushMatrix; cdecl; external nanoGL_LibName;
procedure glTexEnvf(target: GLenum; pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glVertex3fv(const v: PGLfloat); cdecl; external nanoGL_LibName;
procedure glDepthMask(flag: GLboolean); cdecl; external nanoGL_LibName;
procedure glBindTexture(target: GLenum; texture: GLuint); cdecl; external nanoGL_LibName;
function  glGetString(name: GLenum): PChar; cdecl; external nanoGL_LibName; // originally returns PGLubyte
procedure glAlphaFunc(func: GLenum; ref: GLclampf); cdecl; external nanoGL_LibName;
procedure glFlush; cdecl; external nanoGL_LibName;
procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); cdecl; external nanoGL_LibName;
procedure glReadBuffer(mode: GLenum); cdecl; external nanoGL_LibName;
procedure glLoadMatrixf(const m: PGLfloat); cdecl; external nanoGL_LibName;
procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); cdecl; external nanoGL_LibName;
procedure glClearColor(red, green, blue, alpha: GLclampf); cdecl; external nanoGL_LibName;
function  glGetError(): GLenum; cdecl; external nanoGL_LibName;
procedure glActiveTexture(texture: GLenum); cdecl; external nanoGL_LibName;
procedure glClientActiveTexture(texture: GLenum); cdecl; external nanoGL_LibName;
procedure glActiveTextureARB(texture: GLenum); cdecl; external nanoGL_LibName;
procedure glClientActiveTextureARB(texture: GLenum); cdecl; external nanoGL_LibName;
procedure glColor3ubv(const v: PGLubyte); cdecl; external nanoGL_LibName;
procedure glPolygonMode(face, mode: GLenum); cdecl; external nanoGL_LibName;
procedure glArrayElement(i: GLint); cdecl; external nanoGL_LibName;
procedure glLineWidth(width: GLfloat); cdecl; external nanoGL_LibName;
procedure glCallList(list: GLuint); cdecl; external nanoGL_LibName;
procedure glTexCoord2fv(const v: PGLfloat); cdecl; external nanoGL_LibName;
procedure glColorMask(red, green, blue, alpha: GLboolean); cdecl; external nanoGL_LibName;
procedure glStencilFunc(func: GLenum; ref: GLint; mask: GLuint); cdecl; external nanoGL_LibName;
procedure glStencilOp(fail, zfail, zpass: GLenum); cdecl; external nanoGL_LibName;
procedure glColor4ubv(const v: PGLubyte); cdecl; external nanoGL_LibName;
procedure glDrawElements(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); cdecl; external nanoGL_LibName;
procedure glEnableClientState(aarray: GLenum); cdecl; external nanoGL_LibName;
procedure glDisableClientState(aarray: GLenum); cdecl; external nanoGL_LibName;
procedure glVertexPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); cdecl; external nanoGL_LibName;
procedure glTexCoordPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); cdecl; external nanoGL_LibName;
procedure glColorPointer(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); cdecl; external nanoGL_LibName;
procedure glPolygonOffset(factor, units: GLfloat); cdecl; external nanoGL_LibName;
procedure glClearDepth(depth: GLclampd); cdecl; external nanoGL_LibName;
procedure glDeleteTextures(n: GLsizei; const textures: PGLuint); cdecl; external nanoGL_LibName;
procedure glTexParameterfv(target: GLenum; pname: GLenum; const params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glStencilMask(mask: GLuint); cdecl; external nanoGL_LibName;
procedure glClearStencil(s: GLint); cdecl; external nanoGL_LibName;
procedure glScissor(x, y: GLint; width, height: GLsizei); cdecl; external nanoGL_LibName;
procedure glClipPlane(plane: GLenum; const equation: PGLdouble); cdecl; external nanoGL_LibName;
procedure glColor3fv(const v: PGLfloat); cdecl; external nanoGL_LibName;
procedure glPointSize(size: GLfloat); cdecl; external nanoGL_LibName;
//procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); cdecl; external nanoGL_LibName;
procedure glMultMatrixf(const m: PGLfloat); cdecl; external nanoGL_LibName;
procedure glPixelStorei(pname: GLenum; param: GLint); cdecl; external nanoGL_LibName;
procedure glFogi(pname: GLenum; param: GLint); cdecl; external nanoGL_LibName;
procedure glFogf(pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glFogfv(pname: GLenum; const params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glGetTexParameteriv(target, pname: GLenum; params: PGLint); cdecl; external nanoGL_LibName;
procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint); cdecl; external nanoGL_LibName;
procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glTexParameterx(target: GLenum; pname: GLenum; param: GLfixed); cdecl; external nanoGL_LibName;
procedure glGenTextures(n: GLsizei; textures: PGLuint); cdecl; external nanoGL_LibName;
procedure glFrontFace(mode: GLenum); cdecl; external nanoGL_LibName;
procedure glLightf(light, pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glLightfv(light, pname: GLenum; const params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glLightModelf(pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glLightModelfv(pname: GLenum; const params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glMaterialf(face, pname: GLenum; param: GLfloat); cdecl; external nanoGL_LibName;
procedure glMaterialfv(face, pname: GLenum; const params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glColorMaterial(face, mode: GLenum); cdecl; external nanoGL_LibName;
procedure glColor3ub(red, green, blue: GLubyte); cdecl; external nanoGL_LibName;
procedure glNormal3fv(const v: PGLfloat); cdecl; external nanoGL_LibName;
procedure glCopyTexImage2D(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); cdecl; external nanoGL_LibName;
procedure glTexImage1D(target: GLenum; level: GLInt; internalformat: GLEnum; width: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); cdecl; external nanoGL_LibName;
procedure glTexImage3D(target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; _type: GLenum; const pixels: PGLvoid); cdecl; external nanoGL_LibName;
procedure glTexSubImage1D(target: GLenum; level, xoffset: GLint; width: GLsizei; format, atype: GLenum; const pixels: Pointer); cdecl; external nanoGL_LibName;
procedure glTexSubImage3D(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; _type: GLenum; const pixels: PGLvoid); cdecl; external nanoGL_LibName;
function  glIsTexture(texture: GLuint): GLboolean; cdecl; external nanoGL_LibName;
procedure glTexGeni(coord: GLenum; pname: GLenum; param: GLint); cdecl; external nanoGL_LibName;
procedure glTexGenfv(coord: GLenum; pname: GLenum; const params: PGLfloat); cdecl; external nanoGL_LibName;
procedure glColor4ub(red, green, blue, alpha: GLubyte); cdecl; external nanoGL_LibName;
procedure glCopyTexSubImage2D(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); cdecl; external nanoGL_LibName;
procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint); cdecl; external nanoGL_LibName;
procedure glBindFramebuffer(target: GLenum; framebuffer: GLuint); cdecl; external nanoGL_LibName;
procedure glDeleteFramebuffers(n: GLsizei; const framebuffers: PGLuint); cdecl; external nanoGL_LibName;
procedure glGenFramebuffers(n: GLsizei; framebuffers: PGLuint); cdecl; external nanoGL_LibName;
function  glCheckFramebufferStatus(target: GLenum): GLenum; cdecl; external nanoGL_LibName;
function  glIsRenderbuffer(renderbuffer: GLuint): GLboolean; cdecl; external nanoGL_LibName;
procedure glBindRenderbuffer(target: GLenum; renderbuffer: GLuint); cdecl; external nanoGL_LibName;
procedure glDeleteRenderbuffers(n: GLsizei; const renderbuffers: PGLuint); cdecl; external nanoGL_LibName;
procedure glGenRenderbuffers(n: GLsizei; renderbuffers: PGLuint); cdecl; external nanoGL_LibName;
procedure glRenderbufferStorage(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); cdecl; external nanoGL_LibName;
procedure glFramebufferTexture2D(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); cdecl; external nanoGL_LibName;
procedure glFramebufferRenderbuffer(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); cdecl; external nanoGL_LibName;
procedure glNormalPointer(atype: GLenum; stride: GLsizei; const pointer: Pointer); cdecl; external nanoGL_LibName;
procedure glMultiTexCoord3f(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); cdecl; external nanoGL_LibName;
procedure glMultiTexCoord3fARB(target: GLenum; s: GLfloat; t: GLfloat; r: GLfloat); cdecl; external nanoGL_LibName;
procedure glMultiTexCoord2f(target: GLenum; s: GLfloat; t: GLfloat); cdecl; external nanoGL_LibName;
procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); cdecl; external nanoGL_LibName;
procedure glBindBufferARB(target: GLuint; index: GLuint); cdecl; external nanoGL_LibName;
procedure glGenBuffersARB(count: Gluint; indexes: PGLuint); cdecl; external nanoGL_LibName;
procedure glDeleteBuffersARB(count: GLuint; indexes: PGLuint); cdecl; external nanoGL_LibName;
procedure glBufferDataARB(target: GLuint; size: GLuint; buffer: PGLvoid; _type: GLuint); cdecl; external nanoGL_LibName;
procedure glBufferSubDataARB(target: GLuint; offset: GLsizei; size: GLsizei; buffer: PGLvoid); cdecl; external nanoGL_LibName;

function nanoGL_Init(): CInt; cdecl; external nanoGL_LibName;
procedure nanoGL_Destroy; cdecl; external nanoGL_LibName;
procedure nanoGL_Flush; cdecl; external nanoGL_LibName;
function nanoGL_GetProcAddress(name: PAnsiChar): Pointer; cdecl; external nanoGL_LibName;
procedure nanoGL_Reset; cdecl; external nanoGL_LibName;

implementation

procedure glVertex2i(x, y: GLint);
begin
  glVertex3f(x, y, 0);
end;

procedure glTexCoord2d(s, t: GLdouble);
begin
  glTexCoord2f(s, t);
end;

procedure glVertex3d(x, y, z: GLdouble);
begin
  glVertex3f(x, y, z);
end;

(**** ****)

procedure glLoadMatrixd(const m: PGLdouble);
  var
    i: Integer;
    n: array [0..15] of GLfloat;
begin
  for i := 0 to 15 do
    n[i] := m[i];
  glLoadMatrixf(@n[0]);
end;

procedure glTexCoord2i(s, t: GLint);
begin
  glTexCoord2f(1 / s, 1 / t);
end;

end.
