//*************************************************************************
//
// Copyright 1998, 1999, NVIDIA Corporation.
// All rights Reserved.
//
// THE INFORMATION CONTAINED HEREIN IS PROPRIETARY AND CONFIDENTIAL TO
// NVIDIA, CORPORATION.  USE, REPRODUCTION OR DISCLOSURE TO ANY THIRD PARTY
// IS SUBJECT TO WRITTEN PRE-APPROVAL BY NVIDIA, CORPORATION.
//
//*************************************************************************
unit glExt;

interface

uses OpenGL;

const
  // Extensions
  GL_ARB_multitexture                   = 1;
  GL_EXT_abgr                           = 1;
{$IFNDEF GL_EXT_bgra}
  GL_EXT_bgra                           = 1;
{$ENDIF}
  GL_EXT_blend_color                    = 1;
  GL_EXT_blend_minmax                   = 1;
  GL_EXT_blend_subtract                 = 1;
  GL_EXT_clip_volume_hint               = 1;
  GL_EXT_compiled_vertex_array          = 1;
  GL_EXT_fog_coord                      = 1;
  GL_EXT_light_max_exponent             = 1;
  GL_EXT_packed_pixels                  = 1;
  {$IFNDEF GL_EXT_paletted_texture}
    GL_EXT_paletted_texture             = 1;
  {$ENDIF}
  GL_EXT_point_parameters               = 1;
  GL_EXT_rescale_normal                 = 1;
  GL_EXT_secondary_color                = 1;
  GL_EXT_separate_specular_color        = 1;
  GL_EXT_shared_texture_palette         = 1;
  GL_EXT_stencil_wrap                   = 1;
  GL_ARB_texture_cube_map               = 1;
  GL_EXT_texture_env_add                = 1;
  GL_EXT_texture_env_combine            = 1;
  GL_EXT_texture_filter_anisotropic     = 1;
  GL_EXT_texture_lod_bias               = 1;
  {$IFNDEF GL_EXT_vertex_array}
    GL_EXT_vertex_array                 = 1;
  {$ENDIF}
  GL_EXT_vertex_weighting               = 1;
  GL_NV_blend_square                    = 1;
  GL_NV_fog_distance                    = 1;
  GL_NV_register_combiners              = 1;
  GL_NV_texgen_emboss                   = 1;
  GL_ARG_texgen_reflection              = 1;
  GL_NV_texture_env_combine4            = 1;
  GL_NV_vertex_array_range              = 1;
  GL_SGIS_multitexture                  = 1;
  GL_SGIS_texture_lod                   = 1;
  WGL_EXT_swap_control                  = 1;

  // EXT_abgr
  GL_ABGR_EXT                           = $8000;

  // EXT_blend_color
  GL_CONSTANT_COLOR_EXT                 = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT       = $8002;
  GL_CONSTANT_ALPHA_EXT                 = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT       = $8004;
  GL_BLEND_COLOR_EXT                    = $8005;

  // EXT_blend_minmax
  GL_FUNC_ADD_EXT                       = $8006;
  GL_MIN_EXT                            = $8007;
  GL_MAX_EXT                            = $8008;
  GL_BLEND_EQUATION_EXT                 = $8009;

  // EXT_blend_subtract
  GL_FUNC_SUBTRACT_EXT                  = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT          = $800B;

  // EXT_packed_pixels
  GL_UNSIGNED_BYTE_3_3_2_EXT            = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT         = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT         = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT           = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT        = $8036;

  // OpenGL 1.2
  {$IFNDEF GL_VERSION_1_2}
    GL_RESCALE_NORMAL                   = $803A;
    GL_MAX_ELEMENTS_VERTICES            = $80E8;
    GL_MAX_ELEMENTS_INDICES             = $80E9;
    GL_CLAMP_TO_EDGE                    = $812F;
    GL_TEXTURE_MIN_LOD                  = $813A;
    GL_TEXTURE_MAX_LOD                  = $813B;
    GL_TEXTURE_BASE_LEVEL               = $813C;
    GL_TEXTURE_MAX_LEVEL                = $813D;
    GL_SINGLE_COLOR                     = $81F9;
    GL_SEPARATE_SPECULAR_COLOR          = $81FA;
    GL_LIGHT_MODEL_COLOR_CONTROL        = $81F8;
  {$ENDIF}

  // ARB_imaging
  {IFNDEF ARB_imaging}
    GL_CONSTANT_COLOR                   = $8001;
    GL_ONE_MINUS_CONSTANT_COLOR         = $8002;
    GL_CONSTANT_ALPHA                   = $8003;
    GL_ONE_MINUS_CONSTANT_ALPHA         = $8004;
    GL_BLEND_COLOR                      = $8005;
    GL_FUNC_ADD                         = $8006;
    GL_MIN                              = $8007;
    GL_MAX                              = $8008;
    GL_BLEND_EQUATION                   = $8009;
    GL_FUNC_SUBTRACT                    = $800A;
    GL_FUNC_REVERSE_SUBTRACT            = $800B;
  {ENDIF}

  // EXT_vertex_array
  {$IFNDEF GL_VERTEX_ARRAY_EXT}
    GL_VERTEX_ARRAY_EXT                 = $8074;
    GL_NORMAL_ARRAY_EXT                 = $8075;
    GL_COLOR_ARRAY_EXT                  = $8076;
    GL_INDEX_ARRAY_EXT                  = $8077;
    GL_TEXTURE_COORD_ARRAY_EXT          = $8078;
    GL_EDGE_FLAG_ARRAY_EXT              = $8079;
    GL_VERTEX_ARRAY_SIZE_EXT            = $807A;
    GL_VERTEX_ARRAY_TYPE_EXT            = $807B;
    GL_VERTEX_ARRAY_STRIDE_EXT          = $807C;
    GL_VERTEX_ARRAY_COUNT_EXT           = $807D;
    GL_NORMAL_ARRAY_TYPE_EXT            = $807E;
    GL_NORMAL_ARRAY_STRIDE_EXT          = $807F;
    GL_NORMAL_ARRAY_COUNT_EXT           = $8080;
    GL_COLOR_ARRAY_SIZE_EXT             = $8081;
    GL_COLOR_ARRAY_TYPE_EXT             = $8082;
    GL_COLOR_ARRAY_STRIDE_EXT           = $8083;
    GL_COLOR_ARRAY_COUNT_EXT            = $8084;
    GL_INDEX_ARRAY_TYPE_EXT             = $8085;
    GL_INDEX_ARRAY_STRIDE_EXT           = $8086;
    GL_INDEX_ARRAY_COUNT_EXT            = $8087;
    GL_TEXTURE_COORD_ARRAY_SIZE_EXT     = $8088;
    GL_TEXTURE_COORD_ARRAY_TYPE_EXT     = $8089;
    GL_TEXTURE_COORD_ARRAY_STRIDE_EXT   = $808A;
    GL_TEXTURE_COORD_ARRAY_COUNT_EXT    = $808B;
    GL_EDGE_FLAG_ARRAY_STRIDE_EXT       = $808C;
    GL_EDGE_FLAG_ARRAY_COUNT_EXT        = $808D;
    GL_VERTEX_ARRAY_POINTER_EXT         = $808E;
    GL_NORMAL_ARRAY_POINTER_EXT         = $808F;
    GL_COLOR_ARRAY_POINTER_EXT          = $8090;
    GL_INDEX_ARRAY_POINTER_EXT          = $8091;
    GL_TEXTURE_COORD_ARRAY_POINTER_EXT  = $8092;
    GL_EDGE_FLAG_ARRAY_POINTER_EXT      = $8093;
  {$ENDIF}

  // EXT_color_table
  {$IFNDEF GL_COLOR_TABLE_FORMAT_EXT}
    GL_TABLE_TOO_LARGE_EXT              = $8031;
    GL_COLOR_TABLE_FORMAT_EXT           = $80D8;
    GL_COLOR_TABLE_WIDTH_EXT            = $80D9;
    GL_COLOR_TABLE_RED_SIZE_EXT         = $80DA;
    GL_COLOR_TABLE_GREEN_SIZE_EXT       = $80DB;
    GL_COLOR_TABLE_BLUE_SIZE_EXT        = $80DC;
    GL_COLOR_TABLE_ALPHA_SIZE_EXT       = $80DD;
    GL_COLOR_TABLE_LUMINANCE_SIZE_EXT   = $80DE;
    GL_COLOR_TABLE_INTENSITY_SIZE_EXT   = $80DF;
  {$ENDIF}

  // EXT_bgra
  {$IFNDEF GL_BGR_EXT}
    GL_BGR_EXT                          = $80E0;
    GL_BGRA_EXT                         = $80E1;
  {$ENDIF}

  // SGIS_texture_lod
  GL_TEXTURE_MIN_LOD_SGIS               = $813A;
  GL_TEXTURE_MAX_LOD_SGIS               = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS            = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS             = $813D;

  // EXT_paletted_texture
  {$IFNDEF GL_COLOR_INDEX1_EXT}
    GL_COLOR_INDEX1_EXT                 = $80E2;
    GL_COLOR_INDEX2_EXT                 = $80E3;
    GL_COLOR_INDEX4_EXT                 = $80E4;
    GL_COLOR_INDEX8_EXT                 = $80E5;
    GL_COLOR_INDEX12_EXT                = $80E6;
    GL_COLOR_INDEX16_EXT                = $80E7;
  {$ENDIF}

  // EXT_clip_volume_hint
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT      = $80F0;

  // EXT_point_parameters
  GL_POINT_SIZE_MIN_EXT                 = $8126;
  GL_POINT_SIZE_MAX_EXT                 = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT      = $8128;
  GL_DISTANCE_ATTENUATION_EXT           = $8129;

  // EXT_compiled_vertex_array
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT       = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT       = $81A9;

  // EXT_shared_texture_palette
  GL_SHARED_TEXTURE_PALETTE_EXT         = $81FB;

  // SGIS_multitexture
  GL_SELECTED_TEXTURE_SGIS              = $835C;
  GL_MAX_TEXTURES_SGIS                  = $835D;
  GL_TEXTURE0_SGIS                      = $835E;
  GL_TEXTURE1_SGIS                      = $835F;
  GL_TEXTURE2_SGIS                      = $8360;
  GL_TEXTURE3_SGIS                      = $8361;

  // ARB_multitexture
  GL_ACTIVE_TEXTURE_ARB                 = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB          = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB              = $84E2;
  GL_TEXTURE0_ARB                       = $84C0;
  GL_TEXTURE1_ARB                       = $84C1;
  GL_TEXTURE2_ARB                       = $84C2;
  GL_TEXTURE3_ARB                       = $84C3;
  GL_TEXTURE4_ARB                       = $84C4;
  GL_TEXTURE5_ARB                       = $84C5;
  GL_TEXTURE6_ARB                       = $84C6;
  GL_TEXTURE7_ARB                       = $84C7;
  GL_TEXTURE8_ARB                       = $84C8;
  GL_TEXTURE9_ARB                       = $84C9;
  GL_TEXTURE10_ARB                      = $84CA;
  GL_TEXTURE11_ARB                      = $84CB;
  GL_TEXTURE12_ARB                      = $84CC;
  GL_TEXTURE13_ARB                      = $84CD;
  GL_TEXTURE14_ARB                      = $84CE;
  GL_TEXTURE15_ARB                      = $84CF;
  GL_TEXTURE16_ARB                      = $84D0;
  GL_TEXTURE17_ARB                      = $84D1;
  GL_TEXTURE18_ARB                      = $84D2;
  GL_TEXTURE19_ARB                      = $84D3;
  GL_TEXTURE20_ARB                      = $84D4;
  GL_TEXTURE21_ARB                      = $84D5;
  GL_TEXTURE22_ARB                      = $84D6;
  GL_TEXTURE23_ARB                      = $84D7;
  GL_TEXTURE24_ARB                      = $84D8;
  GL_TEXTURE25_ARB                      = $84D9;
  GL_TEXTURE26_ARB                      = $84DA;
  GL_TEXTURE27_ARB                      = $84DB;
  GL_TEXTURE28_ARB                      = $84DC;
  GL_TEXTURE29_ARB                      = $84DD;
  GL_TEXTURE30_ARB                      = $84DE;
  GL_TEXTURE31_ARB                      = $84DF;

  // EXT_fog_coord
  GL_FOG_COORDINATE_SOURCE_EXT          = $8450;
  GL_FOG_COORDINATE_EXT                 = $8451;
  GL_FRAGMENT_DEPTH_EXT                 = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT         = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT      = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT    = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT   = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT           = $8457;

  // EXT_secondary_color
  GL_COLOR_SUM_EXT                      = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT        = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT     = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT     = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT   = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT  = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT          = $845E;

  // EXT_separate_specular_color
  GL_SINGLE_COLOR_EXT                   = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT        = $81FA;
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT      = $81F8;

  // EXT_rescale_normal
  GL_RESCALE_NORMAL_EXT                 = $803A;

  // EXT_stencil_wrap
  GL_INCR_WRAP_EXT                      = $8507;
  GL_DECR_WRAP_EXT                      = $8508;

  // EXT_vertex_weighting
  GL_MODELVIEW0_MATRIX_EXT              = GL_MODELVIEW_MATRIX;
  GL_MODELVIEW1_MATRIX_EXT              = $8506;
  GL_MODELVIEW0_STACK_DEPTH_EXT         = GL_MODELVIEW_STACK_DEPTH;
  GL_MODELVIEW1_STACK_DEPTH_EXT         = $8502;
  GL_VERTEX_WEIGHTING_EXT               = $8509;
  GL_MODELVIEW0_EXT                     = GL_MODELVIEW;
  GL_MODELVIEW1_EXT                     = $850A;
  GL_CURRENT_VERTEX_WEIGHT_EXT          = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT            = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT       = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT       = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT     = $850F;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT    = $8510;

  // NV_texgen_reflection
  GL_NORMAL_MAP_NV                      = $8511;
  GL_REFLECTION_MAP_NV                  = $8512;

  // EXT_texture_cube_map
  GL_NORMAL_MAP_ARB                     = $8511;
  GL_REFLECTION_MAP_ARB                 = $8512;
  GL_TEXTURE_CUBE_MAP_ARB               = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB       = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB    = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB    = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB    = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB    = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB    = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB    = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB         = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB      = $851C;

  // NV_vertex_array_range
  GL_VERTEX_ARRAY_RANGE_NV              = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV       = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV        = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV  = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV      = $8521;

  // NV_register_combiners
  GL_REGISTER_COMBINERS_NV              = $8522;
  GL_COMBINER0_NV                       = $8550;
  GL_COMBINER1_NV                       = $8551;
  GL_COMBINER2_NV                       = $8552;
  GL_COMBINER3_NV                       = $8553;
  GL_COMBINER4_NV                       = $8554;
  GL_COMBINER5_NV                       = $8555;
  GL_COMBINER6_NV                       = $8556;
  GL_COMBINER7_NV                       = $8557;
  GL_VARIABLE_A_NV                      = $8523;
  GL_VARIABLE_B_NV                      = $8524;
  GL_VARIABLE_C_NV                      = $8525;
  GL_VARIABLE_D_NV                      = $8526;
  GL_VARIABLE_E_NV                      = $8527;
  GL_VARIABLE_F_NV                      = $8528;
  GL_VARIABLE_G_NV                      = $8529;
  // GL_ZERO
  GL_CONSTANT_COLOR0_NV                 = $852A;
  GL_CONSTANT_COLOR1_NV                 = $852B;
  // GL_FOG
  GL_PRIMARY_COLOR_NV                   = $852C;
  GL_SECONDARY_COLOR_NV                 = $852D;
  GL_SPARE0_NV                          = $852E;
  GL_SPARE1_NV                          = $852F;
  // GL_TEXTURE0_ARB
  // GL_TEXTURE1_ARB
  GL_UNSIGNED_IDENTITY_NV               = $8536;
  GL_UNSIGNED_INVERT_NV                 = $8537;
  GL_EXPAND_NORMAL_NV                   = $8538;
  GL_EXPAND_NEGATE_NV                   = $8539;
  GL_HALF_BIAS_NORMAL_NV                = $853A;
  GL_HALF_BIAS_NEGATE_NV                = $853B;
  GL_SIGNED_IDENTITY_NV                 = $853C;
  GL_SIGNED_NEGATE_NV                   = $853D;
  GL_E_TIMES_F_NV                       = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV     = $8532;
  // GL_NONE
  GL_SCALE_BY_TWO_NV                    = $853E;
  GL_SCALE_BY_FOUR_NV                   = $853F;
  GL_SCALE_BY_ONE_HALF_NV               = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV       = $8541;
  GL_DISCARD_NV                         = $8530;
  GL_COMBINER_INPUT_NV                  = $8542;
  GL_COMBINER_MAPPING_NV                = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV        = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV         = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV         = $8546;
  GL_COMBINER_MUX_SUM_NV                = $8547;
  GL_COMBINER_SCALE_NV                  = $8548;
  GL_COMBINER_BIAS_NV                   = $8549;
  GL_COMBINER_AB_OUTPUT_NV              = $854a;
  GL_COMBINER_CD_OUTPUT_NV              = $854b;
  GL_COMBINER_SUM_OUTPUT_NV             = $854c;
  GL_MAX_GENERAL_COMBINERS_NV           = $854d;
  GL_NUM_GENERAL_COMBINERS_NV           = $854e;
  GL_COLOR_SUM_CLAMP_NV                 = $854f;

  // NV_fog_distance
  GL_FOG_DISTANCE_MODE_NV               = $855a;
  GL_EYE_RADIAL_NV                      = $855b;
  // GL_EYE_PLANE
  GL_EYE_PLANE_ABSOLUTE_NV              = $855c;

  // NV_texgen_emboss
  GL_EMBOSS_LIGHT_NV                    = $855d;
  GL_EMBOSS_CONSTANT_NV                 = $855e;
  GL_EMBOSS_MAP_NV                      = $855f;

  // EXT_light_max_exponent
  GL_MAX_SHININESS_EXT                  = $8504;
  GL_MAX_SPOT_EXPONENT_EXT              = $8505;

  // EXT_texture_env_combine
  GL_COMBINE_EXT                        = $8570;
  GL_COMBINE_RGB_EXT                    = $8571;
  GL_COMBINE_ALPHA_EXT                  = $8572;
  GL_RGB_SCALE_EXT                      = $8573;
  GL_ADD_SIGNED_EXT                     = $8574;
  GL_INTERPOLATE_EXT                    = $8575;
  GL_CONSTANT_EXT                       = $8576;
  GL_PRIMARY_COLOR_EXT                  = $8577;
  GL_PREVIOUS_EXT                       = $8578;
  GL_SOURCE0_RGB_EXT                    = $8580;
  GL_SOURCE1_RGB_EXT                    = $8581;
  GL_SOURCE2_RGB_EXT                    = $8582;
  GL_SOURCE0_ALPHA_EXT                  = $8588;
  GL_SOURCE1_ALPHA_EXT                  = $8589;
  GL_SOURCE2_ALPHA_EXT                  = $858A;
  GL_OPERAND0_RGB_EXT                   = $8590;
  GL_OPERAND1_RGB_EXT                   = $8591;
  GL_OPERAND2_RGB_EXT                   = $8592;
  GL_OPERAND0_ALPHA_EXT                 = $8598;
  GL_OPERAND1_ALPHA_EXT                 = $8599;
  GL_OPERAND2_ALPHA_EXT                 = $859A;

  // NV_texture_env_combine4
  GL_COMBINE4_NV                        = $8503;
  GL_SOURCE3_RGB_NV                     = $8583;
  GL_SOURCE3_ALPHA_NV                   = $858B;
  GL_OPERAND3_RGB_NV                    = $8593;
  GL_OPERAND3_ALPHA_NV                  = $859B;

  // EXT_texture_filter_anisotropic
  GL_TEXTURE_MAX_ANISOTROPY_EXT         = $84fe;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT     = $84ff;

  // EXT_texture_lod_bias
  GL_MAX_TEXTURE_LOD_BIAS_EXT           = $84fd;
  GL_TEXTURE_FILTER_CONTROL_EXT         = $8500;
  GL_TEXTURE_LOD_BIAS_EXT               = $8501;

type

  PGLBoolean = ^GLBoolean;
  // EXT_vertex_array
  PFNGLARRAYELEMENTEXTPROC      = procedure(i : GLint); stdcall;
  PFNGLCOLORPOINTEREXTPROC      = procedure(size : GLint; ctype : GLenum; stride, count : GLsizei; ptr : Pointer); stdcall;
  PFNGLDRAWARRAYSEXTPROC        = procedure(mode : GLenum; first : GLint; count : GLsizei); stdcall;
  PFNGLEDGEFLAGPOINTEREXTPROC   = procedure(stride : GLsizei; count : GLsizei; pointer : PGLBoolean); stdcall;
  PFNGLGETPOINTERVEXTPROC       = procedure(pname : GLenum; params : Pointer); stdcall;
  PFNGLINDEXPOINTEREXTPROC      = procedure(types : GLenum; stride, count : GLsizei; ptr : Pointer); stdcall;
  PFNGLNORMALPOINTEREXTPROC     = procedure(types : GLenum; stride, count : GLsizei; ptr : Pointer); stdcall;
  PFNGLTEXCOORDPOINTEREXTPROC   = procedure(size : GLint; types : GLenum; stride, count : GLsizei; ptr : Pointer); stdcall;
  PFNGLVERTEXPOINTEREXTPROC     = procedure(size : GLint; types : GLenum; stride, count : GLsizei; ptr : Pointer); stdcall;

  // EXT_color_subtable
  PFNGLCOLORSUBTABLEEXTPROC     = procedure(target : GLenum; start, count : GLsizei; format, types : GLenum; table : Pointer); stdcall;

  // EXT_color_table
  PFNGLCOLORTABLEEXTPROC        = procedure(target, internalformat : GLenum; width : GLsizei; format, types : GLenum; table : Pointer); stdcall;
  PFNGLCOPYCOLORTABLEEXTPROC    = procedure(target, internalformat : GLenum; x, y : GLint; width : GLsizei); stdcall;
  PFNGLGETCOLORTABLEEXTPROC     = procedure(target, format, types  : GLenum; table : Pointer); stdcall;
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname : GLenum; params : PGLfloat); stdcall;
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname : GLenum; params : PGLint); stdcall;

  // EXT_compiled_vertex_array
  PFNGLLOCKARRAYSEXTPROC        = procedure(first : GLint; count : GLsizei); stdcall;
  PFNGLUNLOCKARRAYSEXTPROC      = procedure(); stdcall;

  // WIN_swap_hint
  PFNGLADDSWAPHINTRECTWINPROC   = procedure(x, y : GLint; width, height : GLsizei); stdcall;

  // EXT_point_parameter
  PFNGLPOINTPARAMETERFEXTPROC   = procedure(pname : GLenum; param : GLfloat); stdcall;
  PFNGLPOINTPARAMETERFVEXTPROC  = procedure(pname : GLenum; params : PGLfloat); stdcall;

  // ARB_multitexture
  PFNGLMULTITEXCOORD1DARBPROC   = procedure(target : GLenum; s : GLdouble); stdcall;
  PFNGLMULTITEXCOORD1DVARBPROC  = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD1FARBPROC   = procedure(target : GLenum; s : GLfloat); stdcall;
  PFNGLMULTITEXCOORD1FVARBPROC  = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD1IARBPROC   = procedure(target : GLenum; s : GLint); stdcall;
  PFNGLMULTITEXCOORD1IVARBPROC  = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD1SARBPROC   = procedure(target : GLenum; s : GLshort); stdcall;
  PFNGLMULTITEXCOORD1SVARBPROC  = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORD2DARBPROC   = procedure(target : GLenum; s, t : GLdouble); stdcall;
  PFNGLMULTITEXCOORD2DVARBPROC  = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD2FARBPROC   = procedure(target : GLenum; s, t : GLfloat); stdcall;
  PFNGLMULTITEXCOORD2FVARBPROC  = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD2IARBPROC   = procedure(target : GLenum; s, t : GLint); stdcall;
  PFNGLMULTITEXCOORD2IVARBPROC  = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD2SARBPROC   = procedure(target : GLenum; s, t : GLshort); stdcall;
  PFNGLMULTITEXCOORD2SVARBPROC  = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORD3DARBPROC   = procedure(target : GLenum; s, t, r : GLdouble); stdcall;
  PFNGLMULTITEXCOORD3DVARBPROC  = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD3FARBPROC   = procedure(target : GLenum; s, t, r : GLfloat); stdcall;
  PFNGLMULTITEXCOORD3FVARBPROC  = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD3IARBPROC   = procedure(target : GLenum; s, t, r : GLint); stdcall;
  PFNGLMULTITEXCOORD3IVARBPROC  = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD3SARBPROC   = procedure(target : GLenum; s, t, r : GLshort); stdcall;
  PFNGLMULTITEXCOORD3SVARBPROC  = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORD4DARBPROC   = procedure(target : GLenum; s, t, r, q : GLdouble); stdcall;
  PFNGLMULTITEXCOORD4DVARBPROC  = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD4FARBPROC   = procedure(target : GLenum; s, t, r, q : GLfloat); stdcall;
  PFNGLMULTITEXCOORD4FVARBPROC  = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD4IARBPROC   = procedure(target : GLenum; s, t, r, q : GLint); stdcall;
  PFNGLMULTITEXCOORD4IVARBPROC  = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD4SARBPROC   = procedure(target : GLenum; s, t, r, q : GLshort); stdcall;
  PFNGLMULTITEXCOORD4SVARBPROC  = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLACTIVETEXTUREARBPROC     = procedure(target : GLenum); stdcall;
  PFNGLCLIENTACTIVETEXTUREARBPROC = procedure(target : GLenum); stdcall;

  // SGIS_multitexture
  PFNGLMULTITEXCOORD1DSGISPROC  = procedure(target : GLenum; s : GLdouble); stdcall;
  PFNGLMULTITEXCOORD1DVSGISPROC = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD1FSGISPROC  = procedure(target : GLenum; s : GLfloat); stdcall;
  PFNGLMULTITEXCOORD1FVSGISPROC = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD1ISGISPROC  = procedure(target : GLenum; s : GLint); stdcall;
  PFNGLMULTITEXCOORD1IVSGISPROC = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD1SSGISPROC  = procedure(target : GLenum; s : GLshort); stdcall;
  PFNGLMULTITEXCOORD1SVSGISPROC = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORD2DSGISPROC  = procedure(target : GLenum; s, t :  GLdouble); stdcall;
  PFNGLMULTITEXCOORD2DVSGISPROC = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD2FSGISPROC  = procedure(target : GLenum; s, t : GLfloat); stdcall;
  PFNGLMULTITEXCOORD2FVSGISPROC = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD2ISGISPROC  = procedure(target : GLenum; s, t : GLint); stdcall;
  PFNGLMULTITEXCOORD2IVSGISPROC = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD2SSGISPROC  = procedure(target : GLenum; s, t : GLshort); stdcall;
  PFNGLMULTITEXCOORD2SVSGISPROC = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORD3DSGISPROC  = procedure(target : GLenum; s, t, r : GLdouble); stdcall;
  PFNGLMULTITEXCOORD3DVSGISPROC = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD3FSGISPROC  = procedure(target : GLenum; s, t, r : GLfloat); stdcall;
  PFNGLMULTITEXCOORD3FVSGISPROC = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD3ISGISPROC  = procedure(target : GLenum; s, t, r : GLint); stdcall;
  PFNGLMULTITEXCOORD3IVSGISPROC = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD3SSGISPROC  = procedure(target : GLenum; s, t, r : GLshort); stdcall;
  PFNGLMULTITEXCOORD3SVSGISPROC = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORD4DSGISPROC  = procedure(target : GLenum; s, t, r, q : GLdouble); stdcall;
  PFNGLMULTITEXCOORD4DVSGISPROC = procedure(target : GLenum; v : PGLdouble); stdcall;
  PFNGLMULTITEXCOORD4FSGISPROC  = procedure(target : GLenum; s, t, r, q : GLfloat); stdcall;
  PFNGLMULTITEXCOORD4FVSGISPROC = procedure(target : GLenum; v : PGLfloat); stdcall;
  PFNGLMULTITEXCOORD4ISGISPROC  = procedure(target : GLenum; s, t, r, q : GLint); stdcall;
  PFNGLMULTITEXCOORD4IVSGISPROC = procedure(target : GLenum; v : PGLint); stdcall;
  PFNGLMULTITEXCOORD4SSGISPROC  = procedure(target : GLenum; s, t, r, q : GLshort); stdcall;
  PFNGLMULTITEXCOORD4SVSGISPROC = procedure(target : GLenum; v : PGLshort); stdcall;

  PFNGLMULTITEXCOORDPOINTERSGISPROC = procedure(target : GLenum; size : GLint; types : GLenum; stride : GLsizei; ptr : Pointer); stdcall;
  PFNGLSELECTTEXTURESGISPROC    = procedure(target : GLenum); stdcall;
  PFNGLSELECTTEXTURECOORDSETSGISPROC = procedure(target : GLenum); stdcall;

  // EXT_vertex_weighting
  PFNGLVERTEXWEIGHTFEXTPROC     = procedure(weight : GLfloat); stdcall;
  PFNGLVERTEXWEIGHTFVEXTPROC    = procedure(weight : PGLfloat); stdcall;
  PFNGLVERTEXWEIGHTPOINTEREXTPROC = procedure(size : GLsizei; types : GLenum; stride : GLsizei; ptr : Pointer); stdcall;

  // EXT_blend_color
  PFNGLBLENDCOLOREXTPROC        = procedure(red, green, blue, alpha : GLclampf); stdcall;

  // EXT_blend_minmax
  PFNGLBLENDEQUATIONEXTPROC     = procedure(mode : GLenum); stdcall;

  // EXT_fog_coord
  PFNGLFOGCOORDDEXTPROC         = procedure(fog : GLdouble); stdcall;
  PFNGLFOGCOORDDVEXTPROC        = procedure(fog : PGLdouble); stdcall;
  PFNGLFOGCOORDFEXTPROC         = procedure(fog : GLfloat); stdcall;
  PFNGLFOGCOORDFVEXTPROC        = procedure(fog : PGLfloat); stdcall;
  PFNGLFOGCOORDPOINTEREXTPROC   = procedure(size : GLsizei; types : GLenum; stride : GLsizei; ptr : Pointer); stdcall;

  // EXT_secondary_color
  PFNGLSECONDARYCOLOR3BEXTPROC  = procedure(red, green, blue : GLbyte); stdcall;
  PFNGLSECONDARYCOLOR3BVEXTPROC = procedure(v : PGLbyte); stdcall;
  PFNGLSECONDARYCOLOR3DEXTPROC  = procedure(red, green, blue : GLdouble); stdcall;
  PFNGLSECONDARYCOLOR3DVEXTPROC = procedure(v : PGLdouble); stdcall;
  PFNGLSECONDARYCOLOR3FEXTPROC  = procedure(red, green, blue : GLfloat); stdcall;
  PFNGLSECONDARYCOLOR3FVEXTPROC = procedure(v : PGLfloat); stdcall;
  PFNGLSECONDARYCOLOR3IEXTPROC  = procedure(red, green, blue : GLint); stdcall;
  PFNGLSECONDARYCOLOR3IVEXTPROC = procedure(v : PGLint); stdcall;
  PFNGLSECONDARYCOLOR3SEXTPROC  = procedure(red, green, blue : GLshort); stdcall;
  PFNGLSECONDARYCOLOR3SVEXTPROC = procedure(v : PGLshort); stdcall;
  PFNGLSECONDARYCOLOR3UBEXTPROC = procedure(red, green, blue : GLubyte); stdcall;
  PFNGLSECONDARYCOLOR3UBVEXTPROC = procedure(v : PGLubyte); stdcall;
  PFNGLSECONDARYCOLOR3UIEXTPROC = procedure(red, green, blue : GLuint); stdcall;
  PFNGLSECONDARYCOLOR3UIVEXTPROC = procedure(v : PGLuint); stdcall;
  PFNGLSECONDARYCOLOR3USEXTPROC = procedure(red, green, blue : GLushort); stdcall;
  PFNGLSECONDARYCOLOR3USVEXTPROC = procedure(v : PGLushort); stdcall;
  PFNGLSECONDARYCOLORPOINTEREXTPROC = procedure(size : GLint; types : GLenum; stride : GLsizei; ptr : Pointer); stdcall;

  // NV_vertex_array_range
  PFNGLFLUSHVERTEXARRAYRANGENVPROC = procedure(); stdcall;
  PFNGLVERTEXARRAYRANGENVPROC   = procedure(size : GLsizei; ptr : Pointer); stdcall;
  PFNWGLALLOCATEMEMORYNVPROC    = function(size : GLsizei; readfreq, writefreq, priority : GLfloat) : Pointer; stdcall;
  PFNWGLFREEMEMORYNVPROC        = procedure(ptr : Pointer); stdcall;

  // NV_register_combiners
  PFNGLCOMBINERPARAMETERFVNVPROC = procedure(pname : GLenum; params : PGLfloat); stdcall;
  PFNGLCOMBINERPARAMETERFNVPROC = procedure(pname : GLenum; param : GLfloat); stdcall;
  PFNGLCOMBINERPARAMETERIVNVPROC = procedure(pname : GLenum; params : PGLint); stdcall;
  PFNGLCOMBINERPARAMETERINVPROC = procedure(pname : GLenum; param : GLint); stdcall;
  PFNGLCOMBINERINPUTNVPROC = procedure(stage, portion, variable, input, mapping, componentUsage : GLenum); stdcall;
  PFNGLCOMBINEROUTPUTNVPROC = procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias : GLenum; abDotProduct, cdDotProduct, muxSum : GLboolean); stdcall;
  PFNGLFINALCOMBINERINPUTNVPROC = procedure(variable, input, mapping, componentUsage : GLenum); stdcall;
  PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC = procedure(stage, portion, variable, pname : GLenum; params : PGLfloat); stdcall;
  PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC = procedure(stage, portion, variable, pname : GLenum; params : PGLint); stdcall;
  PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC = procedure(stage, portion, pname : GLenum; params : PGLfloat); stdcall;
  PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC = procedure(stage, portion, pname : GLenum; params : PGLint); stdcall;
  PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC = procedure(variable, pname : GLenum; params : PGLfloat); stdcall;
  PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC = procedure(variable, pname : GLenum; params : PGLint); stdcall;

  // WGL_EXT_swap_control
  PFNWGLSWAPINTERVALEXTPROC     = function(interval : Integer) : Integer; stdcall;
  PFNWGLGETSWAPINTERVALEXTPROC  = function() : Integer; stdcall;

  function isSupported(extension : String) : Boolean;
  
implementation

function StrPos(const Str1, Str2: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX
        JE      @@2
        OR      EDX,EDX
        JE      @@2
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     AL,AL
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        DEC     ECX
        JE      @@2
        MOV     ESI,ECX
        MOV     EDI,EBX
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        SUB     ECX,ESI
        JBE     @@2
        MOV     EDI,EBX
        LEA     EBX,[ESI-1]
@@1:    MOV     ESI,EDX
        LODSB
        REPNE   SCASB
        JNE     @@2
        MOV     EAX,ECX
        PUSH    EDI
        MOV     ECX,EBX
        REPE    CMPSB
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;

function isSupported(extension : String) : Boolean;
var
  extensions : PChar;
  where : PChar;
  start, terminator : PChar;

  hasSpaces : Integer;
begin
  hasSpaces := Pos(PChar(extension), ' ');
  if (hasSpaces <> 0) or (extension = '') then begin
    Result := False;
    Exit;
  end;

  extensions := glGetString(GL_EXTENSIONS);
  start := extensions;
  while (True) do begin
    where := StrPos(start, PChar(extension)); 
    if (where = nil) then begin
      Result := False;
      Exit;
    end;

    terminator := where + Length(extension);
    if (where = start) or ((where-1)^ = ' ') then
      if (terminator^ = ' ') or (terminator^ = #0) then begin 
        Result := True;
        Exit;
      end;

    start := terminator;
  end;
  Result := False;  
end;


end.
