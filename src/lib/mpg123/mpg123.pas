//
// mpg123.h header binding for the Free Pascal Compiler aka FPC
//
// Binaries and demos available at http://www.djmaster.com/
//

(*
    libmpg123: MPEG Audio Decoder library

    copyright 1995-2015 by the mpg123 project
    free software under the terms of the LGPL 2.1
    see COPYING and AUTHORS files in distribution or http://mpg123.org
*)


(** \file mpg123.h The header file for the libmpg123 MPEG Audio decoder *)

unit mpg123;

{$MODE OBJFPC}{$H+}

interface

uses
  ctypes;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFNDEF LIBMPG123_WINDOZE_STATIC}
    {$DEFINE MPG123_DYNAMIC}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(MPG123_DYNAMIC)}
const
{$IF DEFINED(WINDOWS)}
  LIB_MPG123 = 'libmpg123-0.dll';
{$ELSEIF DEFINED(UNIX)}
  LIB_MPG123 = 'libmpg123.so';
{$ELSE}
  {$MESSAGE ERROR 'MPG123_DYNAMIC not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB libmpg123.a}
{$ENDIF}

type
  ppcchar = ^pcchar;
  ppclong = ^pclong;
  pcsize_t = ^csize_t;
  ppcint = ^pcint;
(* off_t = cint; *)
  off_t = coff_t;
  ppoff_t = ^poff_t;
  poff_t = ^coff_t;
  ppcuchar = ^pcuchar;

const
(** A macro to check at compile time which set of API functions to expect.
 * This should be incremented at least each time a new symbol is added
 * to the header.
 *)
  MPG123_LIB_VERSION = '1.25.11';
  MPG123_API_VERSION = 44;
  MPG123_LIB_PATCHLEVEL = 7;

//TODO /** Defines needed for MS Visual Studio(tm) DLL builds.
//TODO  * Every public function must be prefixed with MPG123_EXPORT. When building
//TODO  * the DLL ensure to define BUILD_MPG123_DLL. This makes the function accessible
//TODO  * for clients and includes it in the import library which is created together
//TODO  * with the DLL. When consuming the DLL ensure to define LINK_MPG123_DLL which
//TODO  * imports the functions from the DLL.
//TODO  */
//TODO #ifdef BUILD_MPG123_DLL
//TODO /* The dll exports. */
//TODO #define MPG123_EXPORT __declspec(dllexport)
//TODO #else
//TODO #ifdef LINK_MPG123_DLL
//TODO /* The exe imports. */
//TODO #define MPG123_EXPORT __declspec(dllimport)
//TODO #else
//TODO /* Nothing on normal/UNIX builds */
//TODO #define MPG123_EXPORT
//TODO #endif
//TODO #endif
//TODO
//TODO /* This is for Visual Studio, so this header works as distributed in the binary downloads */
//TODO #if defined(_MSC_VER) && !defined(MPG123_DEF_SSIZE_T)
//TODO #define MPG123_DEF_SSIZE_T
//TODO #include <stddef.h>
//TODO typedef ptrdiff_t ssize_t;
//TODO #endif
//TODO 
//TODO #ifndef MPG123_NO_CONFIGURE /* Enable use of this file without configure. */
//TODO #include <stdlib.h>
//TODO #include <sys/types.h>
//TODO
//TODO /* Simplified large file handling.
//TODO  I used to have a check here that prevents building for a library with conflicting large file setup
//TODO  (application that uses 32 bit offsets with library that uses 64 bits).
//TODO  While that was perfectly fine in an environment where there is one incarnation of the library,
//TODO  it hurt GNU/Linux and Solaris systems with multilib where the distribution fails to provide the
//TODO  correct header matching the 32 bit library (where large files need explicit support) or
//TODO  the 64 bit library (where there is no distinction).
//TODO
//TODO  New approach: When the app defines _FILE_OFFSET_BITS, it wants non-default large file support,
//TODO  and thus functions with added suffix (mpg123_open_64).
//TODO  Any mismatch will be caught at link time because of the _FILE_OFFSET_BITS setting used when
//TODO  building libmpg123. Plus, there's dual mode large file support in mpg123 since 1.12 now.
//TODO  Link failure is not the expected outcome of any half-sane usage anymore.
//TODO
//TODO  More complication: What about client code defining _LARGEFILE64_SOURCE? It might want direct access to the _64 functions, along with the ones without suffix. Well, that's possible now via defining MPG123_NO_LARGENAME and MPG123_LARGESUFFIX, respectively, for disabling or enforcing the suffix names.
//TODO */
//TODO
//TODO /*
//TODO  Now, the renaming of large file aware functions.
//TODO  By default, it appends underscore _FILE_OFFSET_BITS (so, mpg123_seek_64 for mpg123_seek), if _FILE_OFFSET_BITS is defined. You can force a different suffix via MPG123_LARGESUFFIX (that must include the underscore), or you can just disable the whole mess by defining MPG123_NO_LARGENAME.
//TODO */
//TODO #if (!defined MPG123_NO_LARGENAME) && ((defined _FILE_OFFSET_BITS) || (defined MPG123_LARGESUFFIX))
//TODO
//TODO /* Need some trickery to concatenate the value(s) of the given macro(s). */
//TODO #define MPG123_MACROCAT_REALLY(a, b) a ## b
//TODO #define MPG123_MACROCAT(a, b) MPG123_MACROCAT_REALLY(a, b)
//TODO #ifndef MPG123_LARGESUFFIX
//TODO #define MPG123_LARGESUFFIX MPG123_MACROCAT(_, _FILE_OFFSET_BITS)
//TODO #endif
//TODO #define MPG123_LARGENAME(func) MPG123_MACROCAT(func, MPG123_LARGESUFFIX)
//TODO
//TODO #define mpg123_open         MPG123_LARGENAME(mpg123_open)
//TODO #define mpg123_open_fd      MPG123_LARGENAME(mpg123_open_fd)
//TODO #define mpg123_open_handle  MPG123_LARGENAME(mpg123_open_handle)
//TODO #define mpg123_framebyframe_decode MPG123_LARGENAME(mpg123_framebyframe_decode)
//TODO #define mpg123_decode_frame MPG123_LARGENAME(mpg123_decode_frame)
//TODO #define mpg123_tell         MPG123_LARGENAME(mpg123_tell)
//TODO #define mpg123_tellframe    MPG123_LARGENAME(mpg123_tellframe)
//TODO #define mpg123_tell_stream  MPG123_LARGENAME(mpg123_tell_stream)
//TODO #define mpg123_seek         MPG123_LARGENAME(mpg123_seek)
//TODO #define mpg123_feedseek     MPG123_LARGENAME(mpg123_feedseek)
//TODO #define mpg123_seek_frame   MPG123_LARGENAME(mpg123_seek_frame)
//TODO #define mpg123_timeframe    MPG123_LARGENAME(mpg123_timeframe)
//TODO #define mpg123_index        MPG123_LARGENAME(mpg123_index)
//TODO #define mpg123_set_index    MPG123_LARGENAME(mpg123_set_index)
//TODO #define mpg123_position     MPG123_LARGENAME(mpg123_position)
//TODO #define mpg123_length       MPG123_LARGENAME(mpg123_length)
//TODO #define mpg123_framelength  MPG123_LARGENAME(mpg123_framelength)
//TODO #define mpg123_set_filesize MPG123_LARGENAME(mpg123_set_filesize)
//TODO #define mpg123_replace_reader MPG123_LARGENAME(mpg123_replace_reader)
//TODO #define mpg123_replace_reader_handle MPG123_LARGENAME(mpg123_replace_reader_handle)
//TODO #define mpg123_framepos MPG123_LARGENAME(mpg123_framepos)
//TODO
//TODO #endif /* largefile hackery */
//TODO
//TODO #endif /* MPG123_NO_CONFIGURE */

(** \defgroup mpg123_init mpg123 library and handle setup
 *
 * Functions to initialise and shutdown the mpg123 library and handles.
 * The parameters of handles have workable defaults, you only have to tune them when you want to tune something;-)
 * Tip: Use a RVA setting...
 *
 * @{
 *)

(** Opaque structure for the libmpg123 decoder handle. *)
type
  pmpg123_handle_struct = ^mpg123_handle_struct;
  mpg123_handle_struct = record
  end;

(** Opaque structure for the libmpg123 decoder handle.
 *  Most functions take a pointer to a mpg123_handle as first argument and operate on its data in an object-oriented manner.
 *)
type
  pmpg123_handle = ^mpg123_handle;
  mpg123_handle = pmpg123_handle_struct;

(** Function to initialise the mpg123 library.
 *    This function is not thread-safe. Call it exactly once per process, before any other (possibly threaded) work with the library.
 *
 *    \return MPG123_OK if successful, otherwise an error number.
 *)
function mpg123_init(): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Function to close down the mpg123 library.
 *    This function is not thread-safe. Call it exactly once per process, before any other (possibly threaded) work with the library. *)
procedure mpg123_exit(); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Create a handle with optional choice of decoder (named by a string, see mpg123_decoders() or mpg123_supported_decoders()).
 *  and optional retrieval of an error code to feed to mpg123_plain_strerror().
 *  Optional means: Any of or both the parameters may be NULL.
 *
 *  \param decoder optional choice of decoder variant (NULL for default)
 *  \param error optional address to store error codes
 *  \return Non-NULL pointer to fresh handle when successful.
*)
function mpg123_new(const decoder: pchar; error: pcint): pmpg123_handle; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Delete handle, mh is either a valid mpg123 handle or NULL.
 *  \param mh handle
 *)
procedure mpg123_delete(mh: pmpg123_handle); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Enumeration of the parameters types that it is possible to set/get. *)
type
  mpg123_parms = clong;
const
  MPG123_VERBOSE = 0;       (**< set verbosity value for enabling messages to stderr, >= 0 makes sense (integer) *)
  MPG123_FLAGS = 1;         (**< set all flags, p.ex val = MPG123_GAPLESS|MPG123_MONO_MIX (integer) *)
  MPG123_ADD_FLAGS = 2;     (**< add some flags (integer) *)
  MPG123_FORCE_RATE = 3;    (**< when value > 0, force output rate to that value (integer) *)
  MPG123_DOWN_SAMPLE = 4;   (**< 0=native rate, 1=half rate, 2=quarter rate (integer) *)
  MPG123_RVA = 5;           (**< one of the RVA choices above (integer) *)
  MPG123_DOWNSPEED = 6;     (**< play a frame N times (integer) *)
  MPG123_UPSPEED = 7;       (**< play every Nth frame (integer) *)
  MPG123_START_FRAME = 8;   (**< start with this frame (skip frames before that, integer) *)
  MPG123_DECODE_FRAMES = 9; (**< decode only this number of frames (integer) *)
  MPG123_ICY_INTERVAL = 10; (**< stream contains ICY metadata with this interval (integer) *)
  MPG123_OUTSCALE = 11;     (**< the scale for output samples (amplitude - integer or float according to mpg123 output format, normally integer) *)
  MPG123_TIMEOUT = 12;      (**< timeout for reading from a stream (not supported on win32, integer) *)
  MPG123_REMOVE_FLAGS = 13; (**< remove some flags (inverse of MPG123_ADD_FLAGS, integer) *)
  MPG123_RESYNC_LIMIT = 14; (**< Try resync on frame parsing for that many bytes or until end of stream (<0 ... integer). This can enlarge the limit for skipping junk on beginning, too (but not reduce it). *)
  MPG123_INDEX_SIZE = 15;   (**< Set the frame index size (if supported). Values <0 mean that the index is allowed to grow dynamically in these steps (in positive direction, of course) -- Use this when you really want a full index with every individual frame. *)
  MPG123_PREFRAMES = 16;    (**< Decode/ignore that many frames in advance for layer 3. This is needed to fill bit reservoir after seeking, for example (but also at least one frame in advance is needed to have all "normal" data for layer 3). Give a positive integer value, please.*)
  MPG123_FEEDPOOL = 17;     (**< For feeder mode, keep that many buffers in a pool to avoid frequent malloc/free. The pool is allocated on mpg123_open_feed(). If you change this parameter afterwards, you can trigger growth and shrinkage during decoding. The default value could change any time. If you care about this, then set it. (integer) *)
  MPG123_FEEDBUFFER = 18;   (**< Minimal size of one internal feeder buffer, again, the default value is subject to change. (integer) *)

(** Flag bits for MPG123_FLAGS, use the usual binary or to combine. *)
type
  mpg123_param_flags = clong;
const
  MPG123_FORCE_MONO = $7;             (**<     0111 Force some mono mode: This is a test bitmask for seeing if any mono forcing is active. *)
  MPG123_MONO_LEFT = $1;              (**<     0001 Force playback of left channel only. *)
  MPG123_MONO_RIGHT = $2;             (**<     0010 Force playback of right channel only. *)
  MPG123_MONO_MIX = $4;               (**<     0100 Force playback of mixed mono. *)
  MPG123_FORCE_STEREO = $8;           (**<     1000 Force stereo output. *)
  MPG123_FORCE_8BIT = $10;            (**< 00010000 Force 8bit formats. *)
  MPG123_QUIET = $20;                 (**< 00100000 Suppress any printouts (overrules verbose). *)
  MPG123_GAPLESS = $40;               (**< 01000000 Enable gapless decoding (default on if libmpg123 has support). *)
  MPG123_NO_RESYNC = $80;             (**< 10000000 Disable resync stream after error. *)
  MPG123_SEEKBUFFER = $100;           (**< 000100000000 Enable small buffer on non-seekable streams to allow some peek-ahead (for better MPEG sync). *)
  MPG123_FUZZY = $200;                (**< 001000000000 Enable fuzzy seeks (guessing byte offsets or using approximate seek points from Xing TOC) *)
  MPG123_FORCE_FLOAT = $400;          (**< 010000000000 Force floating point output (32 or 64 bits depends on mpg123 internal precision). *)
  MPG123_PLAIN_ID3TEXT = $800;        (**< 100000000000 Do not translate ID3 text data to UTF-8. ID3 strings will contain the raw text data, with the first byte containing the ID3 encoding code. *)
  MPG123_IGNORE_STREAMLENGTH = $1000; (**< 1000000000000 Ignore any stream length information contained in the stream, which can be contained in a 'TLEN' frame of an ID3v2 tag or a Xing tag *)
  MPG123_SKIP_ID3V2 = $2000;          (**< 10 0000 0000 0000 Do not parse ID3v2 tags, just skip them. *)
  MPG123_IGNORE_INFOFRAME = $4000;    (**< 100 0000 0000 0000 Do not parse the LAME/Xing info frame, treat it as normal MPEG data. *)
  MPG123_AUTO_RESAMPLE = $8000;       (**< 1000 0000 0000 0000 Allow automatic internal resampling of any kind (default on if supported). Especially when going lowlevel with replacing output buffer, you might want to unset this flag. Setting MPG123_DOWNSAMPLE or MPG123_FORCE_RATE will override this. *)
  MPG123_PICTURE = $10000;            (**< 17th bit: Enable storage of pictures from tags (ID3v2 APIC). *)
  MPG123_NO_PEEK_END = $20000;        (**< 18th bit: Do not seek to the end of
                                       *  the stream in order to probe
                                       *  the stream length and search for the id3v1 field. This also means
                                       *  the file size is unknown unless set using mpg123_set_filesize() and
                                       *  the stream is assumed as non-seekable unless overridden.
                                       *)
  MPG123_FORCE_SEEKABLE = $40000;     (**< 19th bit: Force the stream to be seekable. *)


(** choices for MPG123_RVA *)
type
  mpg123_param_rva = clong;
const
  MPG123_RVA_OFF = 0;                (**< RVA disabled (default). *)
  MPG123_RVA_MIX = 1;                (**< Use mix/track/radio gain. *)
  MPG123_RVA_ALBUM = 2;              (**< Use album/audiophile gain *)
  MPG123_RVA_MAX = MPG123_RVA_ALBUM; (**< The maximum RVA code, may increase in future. *)

(** Set a specific parameter, for a specific mpg123_handle, using a parameter
 *  type key chosen from the mpg123_parms enumeration, to the specified value.
 *  \param mh handle
 *  \param type parameter choice
 *  \param value integer value
 *  \param fvalue floating point value
 * \return MPG123_OK on success
 *)
function mpg123_param(mh: pmpg123_handle; _type: mpg123_parms; value: clong; fvalue: cdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get a specific parameter, for a specific mpg123_handle.
 *  See the mpg123_parms enumeration for a list of available parameters.
 *  \param mh handle
 *  \param type parameter choice
 *  \param value integer value return address
 *  \param fvalue floating point value return address
 *  \return MPG123_OK on success
 *)
function mpg123_getparam(mh: pmpg123_handle; _type: mpg123_parms; val: pclong; fval: pcdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Feature set available for query with mpg123_feature. *)
type
  mpg123_feature_set = clong;
const
  MPG123_FEATURE_ABI_UTF8OPEN = 0;       (**< mpg123 expects path names to be given in UTF-8 encoding instead of plain native. *)
  MPG123_FEATURE_OUTPUT_8BIT = 1;        (**< 8bit output *)
  MPG123_FEATURE_OUTPUT_16BIT = 2;       (**< 16bit output *)
  MPG123_FEATURE_OUTPUT_32BIT = 3;       (**< 32bit output *)
  MPG123_FEATURE_INDEX = 4;              (**< support for building a frame index for accurate seeking *)
  MPG123_FEATURE_PARSE_ID3V2 = 5;        (**< id3v2 parsing *)
  MPG123_FEATURE_DECODE_LAYER1 = 6;      (**< mpeg layer-1 decoder enabled *)
  MPG123_FEATURE_DECODE_LAYER2 = 7;      (**< mpeg layer-2 decoder enabled *)
  MPG123_FEATURE_DECODE_LAYER3 = 8;      (**< mpeg layer-3 decoder enabled *)
  MPG123_FEATURE_DECODE_ACCURATE = 9;    (**< accurate decoder rounding *)
  MPG123_FEATURE_DECODE_DOWNSAMPLE = 10; (**< downsample (sample omit) *)
  MPG123_FEATURE_DECODE_NTOM = 11;       (**< flexible rate decoding *)
  MPG123_FEATURE_PARSE_ICY = 12;         (**< ICY support *)
  MPG123_FEATURE_TIMEOUT_READ = 13;      (**< Reader with timeout (network). *)
  MPG123_FEATURE_EQUALIZER = 14;         (**< tunable equalizer *)

(** Query libmpg123 features.
 *  \param key feature selection
 *  \return 1 for success, 0 for unimplemented functions
 *)
function mpg123_feature(const key: mpg123_feature_set): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(* @} *)

(** \defgroup mpg123_error mpg123 error handling
 *
 * Functions to get text version of the error numbers and an enumeration
 * of the error codes returned by libmpg123.
 *
 * Most functions operating on a mpg123_handle simply return MPG123_OK (0)
 * on success and MPG123_ERR (-1) on failure, setting the internal error
 * variable of the handle to the specific error code. If there was not a valid
 * (non-NULL) handle provided to a function operating on one, MPG123_BAD_HANDLE
 * may be returned if this can not be confused with a valid positive return
 * value.
 * Meaning: A function expected to return positive integers on success will
 * always indicate error or a special condition by returning a negative one.
 *
 * Decoding/seek functions may also return message codes MPG123_DONE,
 * MPG123_NEW_FORMAT and MPG123_NEED_MORE (all negative, see below on how to
 * react). Note that calls to those can be nested, so generally watch out
 * for these codes after initial handle setup.
 * Especially any function that needs information about the current stream
 * to work will try to at least parse the beginning if that did not happen
 * yet.
 *
 * On a function that is supposed to return MPG123_OK on success and
 * MPG123_ERR on failure, make sure you check for != MPG123_OK, not
 * == MPG123_ERR, as the error code could get more specific in future,
 * or there is just a special message from a decoding routine as indicated
 * above.
 *
 * @{
 *)

(** Enumeration of the message and error codes and returned by libmpg123 functions. *)
type
  mpg123_errors = clong;
const
  MPG123_DONE = -12;             (**< Message: Track ended. Stop decoding. *)
  MPG123_NEW_FORMAT = -11;       (**< Message: Output format will be different on next call. Note that some libmpg123 versions between 1.4.3 and 1.8.0 insist on you calling mpg123_getformat() after getting this message code. Newer verisons behave like advertised: You have the chance to call mpg123_getformat(), but you can also just continue decoding and get your data. *)
  MPG123_NEED_MORE = -10;        (**< Message: For feed reader: "Feed me more!" (call mpg123_feed() or mpg123_decode() with some new input data). *)
  MPG123_ERR = -1;               (**< Generic Error *)
  MPG123_OK = 0;                 (**< Success *)
  MPG123_BAD_OUTFORMAT = 1;      (**< Unable to set up output format! *)
  MPG123_BAD_CHANNEL = 2;        (**< Invalid channel number specified. *)
  MPG123_BAD_RATE = 3;           (**< Invalid sample rate specified. *)
  MPG123_ERR_16TO8TABLE = 4;     (**< Unable to allocate memory for 16 to 8 converter table! *)
  MPG123_BAD_PARAM = 5;          (**< Bad parameter id! *)
  MPG123_BAD_BUFFER = 6;         (**< Bad buffer given -- invalid pointer or too small size. *)
  MPG123_OUT_OF_MEM = 7;         (**< Out of memory -- some malloc() failed. *)
  MPG123_NOT_INITIALIZED = 8;    (**< You didn't initialize the library! *)
  MPG123_BAD_DECODER = 9;        (**< Invalid decoder choice. *)
  MPG123_BAD_HANDLE = 10;        (**< Invalid mpg123 handle. *)
  MPG123_NO_BUFFERS = 11;        (**< Unable to initialize frame buffers (out of memory?). *)
  MPG123_BAD_RVA = 12;           (**< Invalid RVA mode. *)
  MPG123_NO_GAPLESS = 13;        (**< This build doesn't support gapless decoding. *)
  MPG123_NO_SPACE = 14;          (**< Not enough buffer space. *)
  MPG123_BAD_TYPES = 15;         (**< Incompatible numeric data types. *)
  MPG123_BAD_BAND = 16;          (**< Bad equalizer band. *)
  MPG123_ERR_NULL = 17;          (**< Null pointer given where valid storage address needed. *)
  MPG123_ERR_READER = 18;        (**< Error reading the stream. *)
  MPG123_NO_SEEK_FROM_END = 19;  (**< Cannot seek from end (end is not known). *)
  MPG123_BAD_WHENCE = 20;        (**< Invalid 'whence' for seek function.*)
  MPG123_NO_TIMEOUT = 21;        (**< Build does not support stream timeouts. *)
  MPG123_BAD_FILE = 22;          (**< File access error. *)
  MPG123_NO_SEEK = 23;           (**< Seek not supported by stream. *)
  MPG123_NO_READER = 24;         (**< No stream opened. *)
  MPG123_BAD_PARS = 25;          (**< Bad parameter handle. *)
  MPG123_BAD_INDEX_PAR = 26;     (**< Bad parameters to mpg123_index() and mpg123_set_index() *)
  MPG123_OUT_OF_SYNC = 27;       (**< Lost track in bytestream and did not try to resync. *)
  MPG123_RESYNC_FAIL = 28;       (**< Resync failed to find valid MPEG data. *)
  MPG123_NO_8BIT = 29;           (**< No 8bit encoding possible. *)
  MPG123_BAD_ALIGN = 30;         (**< Stack aligmnent error *)
  MPG123_NULL_BUFFER = 31;       (**< NULL input buffer with non-zero size... *)
  MPG123_NO_RELSEEK = 32;        (**< Relative seek not possible (screwed up file offset) *)
  MPG123_NULL_POINTER = 33;      (**< You gave a null pointer somewhere where you shouldn't have. *)
  MPG123_BAD_KEY = 34;           (**< Bad key value given. *)
  MPG123_NO_INDEX = 35;          (**< No frame index in this build. *)
  MPG123_INDEX_FAIL = 36;        (**< Something with frame index went wrong. *)
  MPG123_BAD_DECODER_SETUP = 37; (**< Something prevents a proper decoder setup *)
  MPG123_MISSING_FEATURE = 38;   (**< This feature has not been built into libmpg123. *)
  MPG123_BAD_VALUE = 39;         (**< A bad value has been given, somewhere. *)
  MPG123_LSEEK_FAILED = 40;      (**< Low-level seek failed. *)
  MPG123_BAD_CUSTOM_IO = 41;     (**< Custom I/O not prepared. *)
  MPG123_LFS_OVERFLOW = 42;      (**< Offset value overflow during translation of large file API calls -- your client program cannot handle that large file. *)
  MPG123_INT_OVERFLOW = 43;      (**< Some integer overflow. *)

(** An enum over all sample types possibly known to mpg123.
 *  The values are designed as bit flags to allow bitmasking for encoding families.
 *
 *  Note that (your build of) libmpg123 does not necessarily support all these.
 *  Usually, you can expect the 8bit encodings and signed 16 bit.
 *  Also 32bit float will be usual beginning with mpg123-1.7.0 .
 *  What you should bear in mind is that (SSE, etc) optimized routines may be absent
 *  for some formats. We do have SSE for 16, 32 bit and float, though.
 *  24 bit integer is done via postprocessing of 32 bit output -- just cutting
 *  the last byte, no rounding, even. If you want better, do it yourself.
 *
 *  All formats are in native byte order. If you need different endinaness, you
 *  can simply postprocess the output buffers (libmpg123 wouldn't do anything else).
 *  mpg123_encsize() can be helpful there.
 *)

type
  mpg123_enc_enum = clong;
const
  MPG123_ENC_8           = $00f;  (*      0000 0000 1111 Some 8 bit  integer encoding. *)
  MPG123_ENC_16          = $040;  (*      0000 0100 0000 Some 16 bit integer encoding. *)
  MPG123_ENC_24          = $4000; (* 0100 0000 0000 0000 Some 24 bit integer encoding. *)
  MPG123_ENC_32          = $100;  (*      0001 0000 0000 Some 32 bit integer encoding. *)
  MPG123_ENC_SIGNED      = $080;  (*      0000 1000 0000 Some signed integer encoding. *)
  MPG123_ENC_FLOAT       = $e00;  (*      1110 0000 0000 Some float encoding. *)
  MPG123_ENC_SIGNED_16   = (MPG123_ENC_16 or MPG123_ENC_SIGNED or $10); (*           1101 0000 signed 16 bit *)
  MPG123_ENC_UNSIGNED_16 = (MPG123_ENC_16 or $20);                      (*           0110 0000 unsigned 16 bit *)
  MPG123_ENC_UNSIGNED_8  = $01;                                         (*           0000 0001 unsigned 8 bit *)
  MPG123_ENC_SIGNED_8    = (MPG123_ENC_SIGNED or $02);                  (*           1000 0010 signed 8 bit *)
  MPG123_ENC_ULAW_8      = $04;                                         (*           0000 0100 ulaw 8 bit *)
  MPG123_ENC_ALAW_8      = $08;                                         (*           0000 1000 alaw 8 bit *)
  MPG123_ENC_SIGNED_32   = MPG123_ENC_32 or MPG123_ENC_SIGNED or $1000; (* 0001 0001 1000 0000 signed 32 bit *)
  MPG123_ENC_UNSIGNED_32 = MPG123_ENC_32 or $2000;                      (* 0010 0001 0000 0000 unsigned 32 bit *)
  MPG123_ENC_SIGNED_24   = MPG123_ENC_24 or MPG123_ENC_SIGNED or $1000; (* 0101 0000 1000 0000 signed 24 bit *)
  MPG123_ENC_UNSIGNED_24 = MPG123_ENC_24 or $2000;                      (* 0110 0000 0000 0000 unsigned 24 bit *)
  MPG123_ENC_FLOAT_32    = $200;                                        (*      0010 0000 0000 32bit float *)
  MPG123_ENC_FLOAT_64    = $400;                                        (*      0100 0000 0000 64bit float *)
  MPG123_ENC_ANY = ( MPG123_ENC_SIGNED_16    or MPG123_ENC_UNSIGNED_16 or MPG123_ENC_UNSIGNED_8
                     or MPG123_ENC_SIGNED_8  or MPG123_ENC_ULAW_8      or MPG123_ENC_ALAW_8
                     or MPG123_ENC_SIGNED_32 or MPG123_ENC_UNSIGNED_32
                     or MPG123_ENC_SIGNED_24 or MPG123_ENC_UNSIGNED_24
                     or MPG123_ENC_FLOAT_32  or MPG123_ENC_FLOAT_64 );  (* Any encoding on the list. *)

(** Look up error strings given integer code.
 *  \param errcode integer error code
 *  \return string describing what that error error code means
 *)
function mpg123_plain_strerror(errcode: cint): pchar; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Give string describing what error has occured in the context of handle mh.
 *  When a function operating on an mpg123 handle returns MPG123_ERR, you should check for the actual reason via
 *  char *errmsg = mpg123_strerror(mh)
 *  This function will catch mh == NULL and return the message for MPG123_BAD_HANDLE.
 *  \param mh handle
 *  \return error message
 *)
function mpg123_strerror(mh: pmpg123_handle): pchar; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Return the plain errcode intead of a string.
 *  \param mh handle
 *  \return error code recorded in handle or MPG123_BAD_HANDLE
 *)
function mpg123_errcode(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(*@}*)

(** \defgroup mpg123_decoder mpg123 decoder selection
 *
 * Functions to list and select the available decoders.
 * Perhaps the most prominent feature of mpg123: You have several (optimized) decoders to choose from (on x86 and PPC (MacOS) systems, that is).
 *
 * @{
 *)

(** Get available decoder list.
 *  \return NULL-terminated array of generally available decoder names (plain 8bit ASCII)
 *)
function mpg123_decoders(): ppchar; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get supported decoder list.
 *  \return NULL-terminated array of the decoders supported by the CPU (plain 8bit ASCII)
 *)
function mpg123_supported_decoders(): ppchar; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the active decoder.
 *  \param mh handle
 *  \param decoder_name name of decoder
 *  \return MPG123_OK on success
 *)
function mpg123_decoder(mh: pmpg123_handle; const decoder_name: pchar): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get the currently active decoder name.
 *  The active decoder engine can vary depening on output constraints,
 *  mostly non-resampling, integer output is accelerated via 3DNow & Co. but for
 *  other modes a fallback engine kicks in.
 *  Note that this can return a decoder that is ony active in the hidden and not
 *  available as decoder choice from the outside.
 *  \param mh handle
 *  \return The decoder name or NULL on error.
 *)
function mpg123_current_decoder(mh: pmpg123_handle): pchar; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(*@}*)

(** \defgroup mpg123_output mpg123 output audio format
 *
 * Functions to get and select the format of the decoded audio.
 *
 * Before you dive in, please be warned that you might get confused by this. This seems to happen a lot, therefore I am trying to explain in advance.
 *
 * The mpg123 library decides what output format to use when encountering the first frame in a stream, or actually any frame that is still valid but differs from the frames before in the prompted output format. At such a deciding point, an internal table of allowed encodings, sampling rates and channel setups is consulted. According to this table, an output format is chosen and the decoding engine set up accordingly (including optimized routines for different output formats). This might seem unusual but it just follows from the non-existence of "MPEG audio files" with defined overall properties. There are streams, streams are concatenations of (semi) independent frames. We store streams on disk and call them "MPEG audio files", but that does not change their nature as the decoder is concerned (the LAME/Xing header for gapless decoding makes things interesting again).
 *
 * To get to the point: What you do with mpg123_format() and friends is to fill the internal table of allowed formats before it is used. That includes removing support for some formats or adding your forced sample rate (see MPG123_FORCE_RATE) that will be used with the crude internal resampler. Also keep in mind that the sample encoding is just a question of choice -- the MPEG frames do only indicate their native sampling rate and channel count. If you want to decode to integer or float samples, 8 or 16 bit ... that is your decision. In a "clean" world, libmpg123 would always decode to 32 bit float and let you handle any sample conversion. But there are optimized routines that work faster by directly decoding to the desired encoding / accuracy. We prefer efficiency over conceptual tidyness.
 *
 * People often start out thinking that mpg123_format() should change the actual decoding format on the fly. That is wrong. It only has effect on the next natural change of output format, when libmpg123 will consult its format table again. To make life easier, you might want to call mpg123_format_none() before any thing else and then just allow one desired encoding and a limited set of sample rates / channel choices that you actually intend to deal with. You can force libmpg123 to decode everything to 44100 KHz, stereo, 16 bit integer ... it will duplicate mono channels and even do resampling if needed (unless that feature is disabled in the build, same with some encodings). But I have to stress that the resampling of libmpg123 is very crude and doesn't even contain any kind of "proper" interpolation.
 *
 * In any case, watch out for MPG123_NEW_FORMAT as return message from decoding routines and call mpg123_getformat() to get the currently active output format.
 *
 * @{
 *)

(** They can be combined into one number (3) to indicate mono and stereo... *)
type
  mpg123_channelcount = clong;
const
  MPG123_MONO = 1;   (**< mono *)
  MPG123_STEREO = 2; (**< stereo *)

(** An array of supported standard sample rates
 *  These are possible native sample rates of MPEG audio files.
 *  You can still force mpg123 to resample to a different one, but by default you will only get audio in one of these samplings.
 *  \param list Store a pointer to the sample rates array there.
 *  \param number Store the number of sample rates there. *)
procedure mpg123_rates(const list: ppclong; number: pcsize_t); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** An array of supported audio encodings.
 *  An audio encoding is one of the fully qualified members of mpg123_enc_enum (MPG123_ENC_SIGNED_16, not MPG123_SIGNED).
 *  \param list Store a pointer to the encodings array there.
 *  \param number Store the number of encodings there. *)
procedure mpg123_encodings(const list: ppcint; number: pcsize_t); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Return the size (in bytes) of one mono sample of the named encoding.
 * \param encoding The encoding value to analyze.
 * \return positive size of encoding in bytes, 0 on invalid encoding. *)
function mpg123_encsize(encoding: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Configure a mpg123 handle to accept no output format at all,
 *  use before specifying supported formats with mpg123_format
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_format_none(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Configure mpg123 handle to accept all formats
 *  (also any custom rate you may set) -- this is default.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_format_all(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the audio format support of a mpg123_handle in detail:
 *  \param mh handle
 *  \param rate The sample rate value (in Hertz).
 *  \param channels A combination of MPG123_STEREO and MPG123_MONO.
 *  \param encodings A combination of accepted encodings for rate and channels, p.ex MPG123_ENC_SIGNED16 | MPG123_ENC_ULAW_8 (or 0 for no support). Please note that some encodings may not be supported in the library build and thus will be ignored here.
 *  \return MPG123_OK on success, MPG123_ERR if there was an error. *)
function mpg123_format(mh: pmpg123_handle; rate: clong; channels: cint; encoding: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Check to see if a specific format at a specific rate is supported
 *  by mpg123_handle.
 *  \param mh handle
 *  \param rate sampling rate
 *  \param encoding encoding
 *  \return 0 for no support (that includes invalid parameters), MPG123_STEREO,
 *          MPG123_MONO or MPG123_STEREO|MPG123_MONO. *)
function mpg123_format_support(mh: pmpg123_handle; rate: clong; encoding: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get the current output format written to the addresses given.
 *  If the stream is freshly loaded, this will try to parse enough
 *  of it to give you the format to come. This clears the flag that
 *  would otherwise make the first decoding call return
 *  MPG123_NEW_FORMAT.
 *  \param mh handle
 *  \param rate sampling rate return address
 *  \param channels channel count return address
 *  \param encoding encoding return address
 *  \return MPG123_OK on success
 *)
function mpg123_getformat(mh: pmpg123_handle; rate: pclong; channels: pcint; encoding: pcint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get the current output format written to the addresses given.
 *  This differs from plain mpg123_getformat() in that you can choose
 *  _not_ to clear the flag that would trigger the next decoding call
 *  to return MPG123_NEW_FORMAT in case of a new format arriving.
 *  \param mh handle
 *  \param rate sampling rate return address
 *  \param channels channel count return address
 *  \param encoding encoding return address
 *  \param clear_flag if true, clear internal format flag
 *  \return MPG123_OK on success
 *)
function mpg123_getformat2(mh: pmpg123_handle;	rate: pclong; channels: pcint; encoding: pcint; clear_flag: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(*@}*)

(** \defgroup mpg123_input mpg123 file input and decoding
 *
 * Functions for input bitstream and decoding operations.
 * Decoding/seek functions may also return message codes MPG123_DONE, MPG123_NEW_FORMAT and MPG123_NEED_MORE (please read up on these on how to react!).
 * @{
 *)

(* reading samples / triggering decoding, possible return values: *)
(** Enumeration of the error codes returned by libmpg123 functions. *)

(** Open and prepare to decode the specified file by filesystem path.
 *  This does not open HTTP urls; libmpg123 contains no networking code.
 *  If you want to decode internet streams, use mpg123_open_fd() or mpg123_open_feed().
 *  \param mh handle
 *  \param path filesystem path
 *  \return MPG123_OK on success
 *)
function mpg123_open(mh: pmpg123_handle; const path: pchar): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Use an already opened file descriptor as the bitstream input
 *  mpg123_close() will _not_ close the file descriptor.
 *  \param mh handle
 *  \param fd file descriptor
 *  \return MPG123_OK on success
 *)
function mpg123_open_fd(mh: pmpg123_handle; fd: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Use an opaque handle as bitstream input. This works only with the
 *  replaced I/O from mpg123_replace_reader_handle()!
 *  mpg123_close() will call the cleanup callback for your handle (if you gave one).
 *  \param mh handle
 *  \param iohandle your handle
 *  \return MPG123_OK on success
 *)
function mpg123_open_handle(mh: pmpg123_handle; iohandle: pointer): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Open a new bitstream and prepare for direct feeding
 *  This works together with mpg123_decode(); you are responsible for reading and feeding the input bitstream.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_open_feed(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Closes the source, if libmpg123 opened it.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_close(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Read from stream and decode up to outmemsize bytes.
 *  \param mh handle
 *  \param outmemory address of output buffer to write to
 *  \param outmemsize maximum number of bytes to write
 *  \param done address to store the number of actually decoded bytes to
 *  \return MPG123_OK or error/message code
 *)
function mpg123_read(mh: pmpg123_handle; outmemory: pcuchar; outmemsize: csize_t; done: pcsize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Feed data for a stream that has been opened with mpg123_open_feed().
 *  It's give and take: You provide the bytestream, mpg123 gives you the decoded samples.
 *  \param mh handle
 *  \param in input buffer
 *  \param size number of input bytes
 *  \return MPG123_OK or error/message code.
 *)
function mpg123_feed(mh: pmpg123_handle; const _in: pcuchar; size: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Decode MPEG Audio from inmemory to outmemory.
 *  This is very close to a drop-in replacement for old mpglib.
 *  When you give zero-sized output buffer the input will be parsed until
 *  decoded data is available. This enables you to get MPG123_NEW_FORMAT (and query it)
 *  without taking decoded data.
 *  Think of this function being the union of mpg123_read() and mpg123_feed() (which it actually is, sort of;-).
 *  You can actually always decide if you want those specialized functions in separate steps or one call this one here.
 *  \param mh handle
 *  \param inmemory input buffer
 *  \param inmemsize number of input bytes
 *  \param outmemory output buffer
 *  \param outmemsize maximum number of output bytes
 *  \param done address to store the number of actually decoded bytes to
 *  \return error/message code (watch out especially for MPG123_NEED_MORE)
 *)
function mpg123_decode(mh: pmpg123_handle; const inmemory: pcuchar ; inmemsize: csize_t; outmemory: pcuchar; outmemsize: csize_t; done: pcsize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Decode next MPEG frame to internal buffer
 *  or read a frame and return after setting a new format.
 *  \param mh handle
 *  \param num current frame offset gets stored there
 *  \param audio This pointer is set to the internal buffer to read the decoded audio from.
 *  \param bytes number of output bytes ready in the buffer
 *  \return MPG123_OK or error/message code
 *)
function mpg123_decode_frame(mh: pmpg123_handle; num: poff_t; audio: ppcuchar; bytes: pcsize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Decode current MPEG frame to internal buffer.
 * Warning: This is experimental API that might change in future releases!
 * Please watch mpg123 development closely when using it.
 *  \param mh handle
 *  \param num last frame offset gets stored there
 *  \param audio this pointer is set to the internal buffer to read the decoded audio from.
 *  \param bytes number of output bytes ready in the buffer
 *  \return MPG123_OK or error/message code
 *)
function mpg123_framebyframe_decode(mh: pmpg123_handle; num: poff_t; audio: ppcuchar; bytes: pcsize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Find, read and parse the next mp3 frame
 * Warning: This is experimental API that might change in future releases!
 * Please watch mpg123 development closely when using it.
 *  \param mh handle
 *  \return MPG123_OK or error/message code
 *)
function mpg123_framebyframe_next(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get access to the raw input data for the last parsed frame.
 * This gives you a direct look (and write access) to the frame body data.
 * Together with the raw header, you can reconstruct the whole raw MPEG stream without junk and meta data, or play games by actually modifying the frame body data before decoding this frame (mpg123_framebyframe_decode()).
 * A more sane use would be to use this for CRC checking (see mpg123_info() and MPG123_CRC), the first two bytes of the body make up the CRC16 checksum, if present.
 * You can provide NULL for a parameter pointer when you are not interested in the value.
 *
 *  \param mh handle
 * \param header the 4-byte MPEG header
 * \param bodydata pointer to the frame body stored in the handle (without the header)
 * \param bodybytes size of frame body in bytes (without the header)
 * \return MPG123_OK if there was a yet un-decoded frame to get the
 *    data from, MPG123_BAD_HANDLE or MPG123_ERR otherwise (without further
 *    explanation, the error state of the mpg123_handle is not modified by
 *    this function).
 *)
function mpg123_framedata(mh: pmpg123_handle; header: pculong; bodydata: ppcuchar; bodybytes: pcsize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get the input position (byte offset in stream) of the last parsed frame.
 * This can be used for external seek index building, for example.
 * It just returns the internally stored offset, regardless of validity -- you ensure that a valid frame has been parsed before! *)
function mpg123_framepos(mh: pmpg123_handle): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(*@}*)

(** \defgroup mpg123_seek mpg123 position and seeking
 *
 * Functions querying and manipulating position in the decoded audio bitstream.
 * The position is measured in decoded audio samples, or MPEG frame offset for the specific functions.
 * If gapless code is in effect, the positions are adjusted to compensate the skipped padding/delay - meaning, you should not care about that at all and just use the position defined for the samples you get out of the decoder;-)
 * The general usage is modelled after stdlib's ftell() and fseek().
 * Especially, the whence parameter for the seek functions has the same meaning as the one for fseek() and needs the same constants from stdlib.h:
 * - SEEK_SET: set position to (or near to) specified offset
 * - SEEK_CUR: change position by offset from now
 * - SEEK_END: set position to offset from end
 *
 * Note that sample-accurate seek only works when gapless support has been enabled at compile time; seek is frame-accurate otherwise.
 * Also, really sample-accurate seeking (meaning that you get the identical sample value after seeking compared to plain decoding up to the position) is only guaranteed when you do not mess with the position code by using MPG123_UPSPEED, MPG123_DOWNSPEED or MPG123_START_FRAME. The first two mainly should cause trouble with NtoM resampling, but in any case with these options in effect, you have to keep in mind that the sample offset is not the same as counting the samples you get from decoding since mpg123 counts the skipped samples, too (or the samples played twice only once)!
 * Short: When you care about the sample position, don't mess with those parameters;-)
 * Also, seeking is not guaranteed to work for all streams (underlying stream may not support it).
 * And yet another caveat: If the stream is concatenated out of differing pieces (Frankenstein stream), seeking may suffer, too.
 *
 * @{
 *)

(** Returns the current position in samples.
 *  On the next successful read, you'd get that sample.
 *  \param mh handle
 *  \return sample offset or MPG123_ERR (null handle)
 *)
function mpg123_tell(mh: pmpg123_handle): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Returns the frame number that the next read will give you data from.
 *  \param mh handle
 *  \return frame offset or MPG123_ERR (null handle)
 *)
function mpg123_tellframe(mh: pmpg123_handle): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Returns the current byte offset in the input stream.
 *  \param mh handle
 *  \return byte offset or MPG123_ERR (null handle)
 *)
function mpg123_tell_stream(mh: pmpg123_handle): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Seek to a desired sample offset.
 *  Usage is modelled afer the standard lseek().
 * \param mh handle
 * \param sampleoff offset in PCM samples
 * \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 * \return The resulting offset >= 0 or error/message code
 *)
function mpg123_seek(mh: pmpg123_handle; sampleoff: off_t; whence: cint): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Seek to a desired sample offset in data feeding mode.
 *  This just prepares things to be right only if you ensure that the next chunk of input data will be from input_offset byte position.
 *  \param mh handle
 *  \param sampleoff offset in PCM samples
 *  \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 *  \param input_offset The position it expects to be at the
 *                      next time data is fed to mpg123_decode().
 *  \return The resulting offset >= 0 or error/message code *)
function mpg123_feedseek(mh: pmpg123_handle; sampleoff: off_t; whence: cint; input_offset: poff_t): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Seek to a desired MPEG frame offset.
 *  Usage is modelled afer the standard lseek().
 * \param mh handle
 * \param frameoff offset in MPEG frames
 * \param whence one of SEEK_SET, SEEK_CUR or SEEK_END
 * \return The resulting offset >= 0 or error/message code *)
function mpg123_seek_frame(mh: pmpg123_handle; frameoff: off_t; whence: cint): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Return a MPEG frame offset corresponding to an offset in seconds.
 *  This assumes that the samples per frame do not change in the file/stream, which is a good assumption for any sane file/stream only.
 *  \return frame offset >= 0 or error/message code *)
function mpg123_timeframe(mh: pmpg123_handle; sec: cdouble): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Give access to the frame index table that is managed for seeking.
 *  You are asked not to modify the values... Use mpg123_set_index to set the
 *  seek index
 *  \param mh handle
 *  \param offsets pointer to the index array
 *  \param step one index byte offset advances this many MPEG frames
 *  \param fill number of recorded index offsets; size of the array
 *  \return MPG123_OK on success
 *)
function mpg123_index(mh: pmpg123_handle; offsets: ppoff_t; step: poff_t; fill: pcsize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the frame index table
 *  Setting offsets to NULL and fill > 0 will allocate fill entries. Setting offsets
 *  to NULL and fill to 0 will clear the index and free the allocated memory used by the index.
 *  \param mh handle
 *  \param offsets pointer to the index array
 *  \param step    one index byte offset advances this many MPEG frames
 *  \param fill    number of recorded index offsets; size of the array
 *  \return MPG123_OK on success
 *)
function mpg123_set_index(mh: pmpg123_handle; offsets: poff_t; step: off_t; fill: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** An old crutch to keep old mpg123 binaries happy.
 *  WARNING: This function is there only to avoid runtime linking errors with
 *  standalone mpg123 before version 1.23.0 (if you strangely update the
 *  library but not the end-user program) and actually is broken
 *  for various cases (p.ex. 24 bit output). Do never use. It might eventually
 *  be purged from the library.
 *)
function mpg123_position( mh: pmpg123_handle; frame_offset: off_t; buffered_bytes: off_t; current_frame: poff_t; frames_left: poff_t; current_seconds: pcdouble; seconds_left: pcdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(*@}*)

(** \defgroup mpg123_voleq mpg123 volume and equalizer
 *
 * @{
 *)

(** another channel enumeration, for left/right choice *)
type
  mpg123_channels = clong;
const
  MPG123_LEFT = $1;  (**< The Left Channel. *)
  MPG123_RIGHT = $2; (**< The Right Channel. *)
  MPG123_LR = $3;    (**< Both left and right channel; same as MPG123_LEFT|MPG123_RIGHT *)

(** Set the 32 Band Audio Equalizer settings.
 *  \param mh handle
 *  \param channel Can be MPG123_LEFT, MPG123_RIGHT or MPG123_LEFT|MPG123_RIGHT for both.
 *  \param band The equaliser band to change (from 0 to 31)
 *  \param val The (linear) adjustment factor.
 *  \return MPG123_OK on success
 *)
function mpg123_eq(mh: pmpg123_handle; channel: mpg123_channels; band: cint; val: cdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get the 32 Band Audio Equalizer settings.
 *  \param mh handle
 *  \param channel Can be MPG123_LEFT, MPG123_RIGHT or MPG123_LEFT|MPG123_RIGHT for (arithmetic mean of) both.
 *  \param band The equaliser band to change (from 0 to 31)
 *  \return The (linear) adjustment factor (zero for pad parameters) *)
function mpg123_geteq(mh: pmpg123_handle; channel: mpg123_channels; band: cint): cdouble; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Reset the 32 Band Audio Equalizer settings to flat
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_reset_eq(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the absolute output volume including the RVA setting,
 *  vol<0 just applies (a possibly changed) RVA setting.
 *  \param mh handle
 *  \param vol volume value (linear factor)
 *  \return MPG123_OK on success
 *)
function mpg123_volume(mh: pmpg123_handle; vol: cdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Adjust output volume including the RVA setting by chosen amount
 *  \param mh handle
 *  \param change volume value (linear factor increment)
 *  \return MPG123_OK on success
 *)
function mpg123_volume_change(mh: pmpg123_handle; change: cdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Return current volume setting, the actual value due to RVA, and the RVA
 *  adjustment itself. It's all as double float value to abstract the sample
 *  format. The volume values are linear factors / amplitudes (not percent)
 *  and the RVA value is in decibels.
 *  \param mh handle
 *  \param base return address for base volume (linear factor)
 *  \param really return address for actual volume (linear factor)
 *  \param rva_db return address for RVA value (decibels)
 *  \return MPG123_OK on success
 *)
function mpg123_getvolume(mh: pmpg123_handle; base: pcdouble; really: pcdouble; rva_db: pcdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(* TODO: Set some preamp in addition / to replace internal RVA handling? *)

(*@}*)

(** \defgroup mpg123_status mpg123 status and information
 *
 * @{
 *)

(** Enumeration of the mode types of Variable Bitrate *)
type
  mpg123_vbr_type = clong;
const
  MPG123_CBR = 0; (**< Constant Bitrate Mode (default) *)
  MPG123_VBR = 1; (**< Variable Bitrate Mode *)
  MPG123_ABR = 2; (**< Average Bitrate Mode *)

(** Enumeration of the MPEG Versions *)
type
  mpg123_version = clong;
const
  MPG123_1_0 = 0; (**< MPEG Version 1.0 *)
  MPG123_2_0 = 1; (**< MPEG Version 2.0 *)
  MPG123_2_5 = 2; (**< MPEG Version 2.5 *)


(** Enumeration of the MPEG Audio mode.
 *  Only the mono mode has 1 channel, the others have 2 channels. *)
type
  mpg123_mode = clong;
const
  MPG123_M_STEREO = 0; (**< Standard Stereo. *)
  MPG123_M_JOINT = 1;  (**< Joint Stereo. *)
  MPG123_M_DUAL = 2;   (**< Dual Channel. *)
  MPG123_M_MONO = 3;   (**< Single Channel. *)


(** Enumeration of the MPEG Audio flag bits *)
type
  mpg123_flags_type = clong;
const
    MPG123_CRC = $1;       (**< The bitstream is error protected using 16-bit CRC. *)
    MPG123_COPYRIGHT = $2; (**< The bitstream is copyrighted. *)
    MPG123_PRIVATE = $4;   (**< The private bit has been set. *)
    MPG123_ORIGINAL = $8;  (**< The bitstream is an original, not a copy. *)

(** Data structure for storing information about a frame of MPEG Audio *)
type
  pmpg123_frameinfo = ^mpg123_frameinfo;
  mpg123_frameinfo = record
    version: mpg123_version;  (**< The MPEG version (1.0/2.0/2.5). *)
    layer: cint;              (**< The MPEG Audio Layer (MP1/MP2/MP3). *)
    rate: clong;              (**< The sampling rate in Hz. *)
    mode: mpg123_mode;        (**< The audio mode (Mono, Stereo, Joint-stero, Dual Channel). *)
    mode_ext: cint;           (**< The mode extension bit flag. *)
    framesize: cint;          (**< The size of the frame (in bytes, including header). *)
    flags: mpg123_flags_type; (**< MPEG Audio flag bits. Just now I realize that it should be declared as int, not enum. It's a bitwise combination of the enum values. *)
    emphasis: cint;           (**< The emphasis type. *)
    bitrate: cint;            (**< Bitrate of the frame (kbps). *)
    abr_rate: cint;           (**< The target average bitrate. *)
    vbr: mpg123_vbr_type;     (**< The VBR mode. *)
  end;

(** Get frame information about the MPEG audio bitstream and store it in a mpg123_frameinfo structure.
 *  \param mh handle
 *  \param mi address of existing frameinfo structure to write to
 *  \return MPG123_OK on success
 *)
function mpg123_info(mh: pmpg123_handle; mi: pmpg123_frameinfo): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get the safe output buffer size for all cases
 *  (when you want to replace the internal buffer)
 *  \return safe buffer size
 *)
function mpg123_safe_buffer(): csize_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Make a full parsing scan of each frame in the file. ID3 tags are found. An
 *  accurate length value is stored. Seek index will be filled. A seek back to
 *  current position is performed. At all, this function refuses work when
 *  stream is not seekable.
 *  \param mh handle
 *  \return MPG123_OK on success
 *)
function mpg123_scan(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Return, if possible, the full (expected) length of current track in frames.
 * \param mh handle
 * \return length >= 0 or MPG123_ERR if there is no length guess possible.
 *)
function mpg123_framelength(mh: pmpg123_handle): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Return, if possible, the full (expected) length of current track in samples.
 * \param mh handle
 * \return length >= 0 or MPG123_ERR if there is no length guess possible.
 *)
function mpg123_length(mh: pmpg123_handle): off_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Override the value for file size in bytes.
 * Useful for getting sensible track length values in feed mode or for HTTP streams.
 *  \param mh handle
 *  \param size file size in bytes
 *  \return MPG123_OK on success
 *)
function mpg123_set_filesize(mh: pmpg123_handle; size: off_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get MPEG frame duration in seconds.
 *  \param mh handle
 *  \return frame duration in seconds, <0 on error
 *)
function mpg123_tpf(mh: pmpg123_handle): cdouble; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get MPEG frame duration in samples.
 *  \param mh handle
 *  \return samples per frame for the most recently parsed frame; <0 on errors
 *)
function mpg123_spf(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get and reset the clip count.
 *  \param mh handle
 *  \return count of clipped samples
 *)
function mpg123_clip(mh: pmpg123_handle): clong; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** The key values for state information from mpg123_getstate(). *)
type
  mpg123_state = clong;
const
  MPG123_ACCURATE = 1;      (**< Query if positons are currently accurate (integer value, 0 if false, 1 if true). *)
  MPG123_BUFFERFILL = 2;    (**< Get fill of internal (feed) input buffer as integer byte count returned as long and as double. An error is returned on integer overflow while converting to (signed) long, but the returned floating point value shold still be fine. *)
  MPG123_FRANKENSTEIN = 3;  (**< Stream consists of carelessly stitched together files. Seeking may yield unexpected results (also with MPG123_ACCURATE, it may be confused). *)
  MPG123_FRESH_DECODER = 4; (**< Decoder structure has been updated, possibly indicating changed stream (integer value, 0 if false, 1 if true). Flag is cleared after retrieval. *)

(** Get various current decoder/stream state information.
 *  \param mh handle
 *  \param key the key to identify the information to give.
 *  \param val the address to return (long) integer values to
 *  \param fval the address to return floating point values to
 *  \return MPG123_OK on success
 *)
function mpg123_getstate(mh: pmpg123_handle; key: mpg123_state; val: pclong; fval: pcdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(*@}*)

(** \defgroup mpg123_metadata mpg123 metadata handling
 *
 * Functions to retrieve the metadata from MPEG Audio files and streams.
 * Also includes string handling functions.
 *
 * @{
 *)

(** Data structure for storing strings in a safer way than a standard C-String.
 *  Can also hold a number of null-terminated strings. *)
type
  pmpg123_string = ^mpg123_string;
  mpg123_string = record
    p: pchar;     (**< pointer to the string data *)
    size: csize_t; (**< raw number of bytes allocated *)
    fill: csize_t; (**< number of used bytes (including closing zero byte) *)
  end;

(** Create and allocate memory for a new mpg123_string
 *  \param sb string handle (address of existing structure on your side)
 *)
procedure mpg123_init_string(sb: pmpg123_string); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Free-up mempory for an existing mpg123_string
 *  \param sb string handle
 *)
procedure mpg123_free_string(sb: pmpg123_string); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Change the size of a mpg123_string
 *  \param sb string handle
 *  \param news new size in bytes
 *  \return 0 on error, 1 on success
 *)
function mpg123_resize_string(sb: pmpg123_string; news: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Increase size of a mpg123_string if necessary (it may stay larger).
 *  Note that the functions for adding and setting in current libmpg123
 *  use this instead of mpg123_resize_string().
 *  That way, you can preallocate memory and safely work afterwards with
 *  pieces.
 *  \param sb string handle
 *  \param news new minimum size
 *  \return 0 on error, 1 on success
 *)
function mpg123_grow_string(sb: pmpg123_string; news: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Copy the contents of one mpg123_string string to another.
 *  Yes the order of arguments is reversed compated to memcpy().
 *  \param from string handle
 *  \param to string handle
 *  \return 0 on error, 1 on success
 *)
function mpg123_copy_string(from: pmpg123_string; _to: pmpg123_string ): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Append a C-String to an mpg123_string
 *  \param sb string handle
 *  \param stuff to append
 *  \return 0 on error, 1 on success
 *)
function mpg123_add_string(sb: pmpg123_string; const stuff: pchar): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Append a C-substring to an mpg123 string
 *  \param sb string handle
 *  \param stuff content to copy
 *  \param from offset to copy from
 *  \param count number of characters to copy (a null-byte is always appended)
 *  \return 0 on error, 1 on success
 *)
function mpg123_add_substring(sb: pmpg123_string; const stuff: pchar; from: csize_t; count: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the content of a mpg123_string to a C-string
 *  \param sb string handle
 *  \param stuff content to copy
 *  \return 0 on error, 1 on success
 *)
function mpg123_set_string(sb: pmpg123_string; const stuff: pchar): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the content of a mpg123_string to a C-substring
 *  \param sb string handle
 *  \param stuff the future content
 *  \param from offset to copy from
 *  \param count number of characters to copy (a null-byte is always appended)
 *  \return 0 on error, 1 on success
 *)
function mpg123_set_substring(sb: pmpg123_string; const stuff: pchar; from: csize_t; count: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Count characters in a mpg123 string (non-null bytes or UTF-8 characters).
 *  Even with the fill property, the character count is not obvious as there could be multiple trailing null bytes.
 *  \param sb string handle
 *  \param utf8 a flag to tell if the string is in utf8 encoding
 *  \return character count
*)
function mpg123_strlen(sb: pmpg123_string; utf8: cint): csize_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Remove trailing \\r and \\n, if present.
 *  \param sb string handle
 *  \return 0 on error, 1 on success
 *)
function mpg123_chomp_string(sb: pmpg123_string): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** The mpg123 text encodings. This contains encodings we encounter in ID3 tags or ICY meta info. *)
type
  mpg123_text_encoding = clong;
const
  mpg123_text_unknown = 0;  (**< Unkown encoding... mpg123_id3_encoding can return that on invalid codes. *)
  mpg123_text_utf8 = 1;     (**< UTF-8 *)
  mpg123_text_latin1 = 2;   (**< ISO-8859-1. Note that sometimes latin1 in ID3 is abused for totally different encodings. *)
  mpg123_text_icy = 3;      (**< ICY metadata encoding, usually CP-1252 but we take it as UTF-8 if it qualifies as such. *)
  mpg123_text_cp1252 = 4;   (**< Really CP-1252 without any guessing. *)
  mpg123_text_utf16 = 5;    (**< Some UTF-16 encoding. The last of a set of leading BOMs (byte order mark) rules.
                              *   When there is no BOM, big endian ordering is used. Note that UCS-2 qualifies as UTF-8 when
                              *   you don't mess with the reserved code points. If you want to decode little endian data
                              *   without BOM you need to prepend 0xff 0xfe yourself. *)
  mpg123_text_utf16bom = 6; (**< Just an alias for UTF-16, ID3v2 has this as distinct code. *)
  mpg123_text_utf16be = 7;  (**< Another alias for UTF16 from ID3v2. Note, that, because of the mess that is reality,
                              *   BOMs are used if encountered. There really is not much distinction between the UTF16 types for mpg123
                              *   One exception: Since this is seen in ID3v2 tags, leading null bytes are skipped for all other UTF16
                              *   types (we expect a BOM before real data there), not so for utf16be!*)
  mpg123_text_max = 7;      (**< Placeholder for the maximum encoding value. *)

(** The encoding byte values from ID3v2. *)
type
  mpg123_id3_enc = clong;
const
  mpg123_id3_latin1 = 0;   (**< Note: This sometimes can mean anything in practice... *)
  mpg123_id3_utf16bom = 1; (**< UTF16, UCS-2 ... it's all the same for practical purposes. *)
  mpg123_id3_utf16be = 2;  (**< Big-endian UTF-16, BOM see note for mpg123_text_utf16be. *)
  mpg123_id3_utf8 = 3;     (**< Our lovely overly ASCII-compatible 8 byte encoding for the world. *)
  mpg123_id3_enc_max = 3;  (**< Placeholder to check valid range of encoding byte. *)

(** Convert ID3 encoding byte to mpg123 encoding index.
 *  \param id3_enc_byte the ID3 encoding code
 *  \return the mpg123 encoding index
 *)
function mpg123_enc_from_id3(id3_enc_byte: cuchar): mpg123_text_encoding; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Store text data in string, after converting to UTF-8 from indicated encoding
 *  A prominent error can be that you provided an unknown encoding value, or this build of libmpg123 lacks support for certain encodings (ID3 or ICY stuff missing).
 *  Also, you might want to take a bit of care with preparing the data; for example, strip leading zeroes (I have seen that).
 *  \param sb  target string
 *  \param enc mpg123 text encoding value
 *  \param source source buffer with plain unsigned bytes (you might need to cast from signed char)
 *  \param source_size number of bytes in the source buffer
 *  \return 0 on error, 1 on success (on error, mpg123_free_string is called on sb)
 *)
function mpg123_store_utf8(sb: pmpg123_string; enc: mpg123_text_encoding; const source: pcuchar; source_size: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Sub data structure for ID3v2, for storing various text fields (including comments).
 *  This is for ID3v2 COMM, TXXX and all the other text fields.
 *  Only COMM and TXXX have a description, only COMM and USLT have a language.
 *  You should consult the ID3v2 specification for the use of the various text fields ("frames" in ID3v2 documentation, I use "fields" here to separate from MPEG frames). *)
type
  pmpg123_text = ^mpg123_text;
  mpg123_text = record
    lang: array[0..2] of char;  (**< Three-letter language code (not terminated). *)
    id: array[0..3] of char;    (**< The ID3v2 text field id, like TALB, TPE2, ... (4 characters, no string termination). *)
    description: mpg123_string; (**< Empty for the generic comment... *)
    text: mpg123_string;        (**< ... *)
  end;

(** The picture type values from ID3v2. *)
type
  mpg123_id3_pic_type = char;
const
  mpg123_id3_pic_other = #0;           (**< see ID3v2 docs *)
  mpg123_id3_pic_icon = #1;            (**< see ID3v2 docs *)
  mpg123_id3_pic_other_icon = #2;      (**< see ID3v2 docs *)
  mpg123_id3_pic_front_cover = #3;     (**< see ID3v2 docs *)
  mpg123_id3_pic_back_cover = #4;      (**< see ID3v2 docs *)
  mpg123_id3_pic_leaflet = #5;         (**< see ID3v2 docs *)
  mpg123_id3_pic_media = #6;           (**< see ID3v2 docs *)
  mpg123_id3_pic_lead = #7;            (**< see ID3v2 docs *)
  mpg123_id3_pic_artist = #8;          (**< see ID3v2 docs *)
  mpg123_id3_pic_conductor = #9;       (**< see ID3v2 docs *)
  mpg123_id3_pic_orchestra = #10;      (**< see ID3v2 docs *)
  mpg123_id3_pic_composer = #11;       (**< see ID3v2 docs *)
  mpg123_id3_pic_lyricist = #12;       (**< see ID3v2 docs *)
  mpg123_id3_pic_location = #13;       (**< see ID3v2 docs *)
  mpg123_id3_pic_recording = #14;      (**< see ID3v2 docs *)
  mpg123_id3_pic_performance = #15;    (**< see ID3v2 docs *)
  mpg123_id3_pic_video = #16;          (**< see ID3v2 docs *)
  mpg123_id3_pic_fish = #17;           (**< see ID3v2 docs *)
  mpg123_id3_pic_illustration = #18;   (**< see ID3v2 docs *)
  mpg123_id3_pic_artist_logo = #19;    (**< see ID3v2 docs *)
  mpg123_id3_pic_publisher_logo = #20; (**< see ID3v2 docs *)

(** Sub data structure for ID3v2, for storing picture data including comment.
 *  This is for the ID3v2 APIC field. You should consult the ID3v2 specification
 *  for the use of the APIC field ("frames" in ID3v2 documentation, I use "fields"
 *  here to separate from MPEG frames). *)
type
  pmpg123_picture_type = ^mpg123_picture_type;
  mpg123_picture_type = record
    pic_type: char;             (**< mpg123_id3_pic_type value *)
    description: mpg123_string; (**< description string *)
    mime_type: mpg123_string;   (**< MIME type *)
    size: csize_t;               (**< size in bytes *)
    data: pcuchar;              (**< pointer to the image data *)
  end;

(** Data structure for storing IDV3v2 tags.
 *  This structure is not a direct binary mapping with the file contents.
 *  The ID3v2 text frames are allowed to contain multiple strings.
 *  So check for null bytes until you reach the mpg123_string fill.
 *  All text is encoded in UTF-8. *)
type
  pmpg123_id3v2 = ^mpg123_id3v2;
  ppmpg123_id3v2 = ^pmpg123_id3v2;
  mpg123_id3v2 = record
    version: cuchar;               (**< 3 or 4 for ID3v2.3 or ID3v2.4. *)
    title: pmpg123_string;         (**< Title string (pointer into text_list). *)
    artist: pmpg123_string;        (**< Artist string (pointer into text_list). *)
    album: pmpg123_string;         (**< Album string (pointer into text_list). *)
    year: pmpg123_string;          (**< The year as a string (pointer into text_list). *)
    genre: pmpg123_string;         (**< Genre String (pointer into text_list). The genre string(s) may very well need postprocessing, esp. for ID3v2.3. *)
    comment: pmpg123_string;       (**< Pointer to last encountered comment text with empty description. *)
    (* Encountered ID3v2 fields are appended to these lists.
       There can be multiple occurences, the pointers above always point to the last encountered data. *)
    comment_list: pmpg123_text;    (**< Array of comments. *)
    comments: csize_t;              (**< Number of comments. *)
    text: pmpg123_text;            (**< Array of ID3v2 text fields (including USLT) *)
    texts: csize_t;                 (**< Numer of text fields. *)
    extra: pmpg123_text;           (**< The array of extra (TXXX) fields. *)
    extras: csize_t;                (**< Number of extra text (TXXX) fields. *)
    picture: pmpg123_picture_type; (**< Array of ID3v2 pictures fields (APIC). *)
    pictures: csize_t;              (**< Number of picture (APIC) fields. *)
  end;

(** Data structure for ID3v1 tags (the last 128 bytes of a file).
 *  Don't take anything for granted (like string termination)!
 *  Also note the change ID3v1.1 did: comment[28] = 0; comment[29] = track_number
 *  It is your task to support ID3v1 only or ID3v1.1 ...*)
type
  pmpg123_id3v1 = ^mpg123_id3v1;
  ppmpg123_id3v1 = ^pmpg123_id3v1;
  mpg123_id3v1 = record
    tag: array[0..2] of char;      (**< Always the string "TAG", the classic intro. *)
    title: array[0..29] of char;   (**< Title string. *)
    artist: array[0..29] of char;  (**< Artist string. *)
    album: array[0..29] of char;   (**< Album string. *)
    year: array[0..3] of char;     (**< Year string. *)
    comment: array[0..29] of char; (**< Comment string. *)
    genre: cuchar;                 (**< Genre index. *)
  end;

const
  MPG123_ID3_CONST = $3; (**< 0011 There is some ID3 info. Also matches 0010 or NEW_ID3. *)
  MPG123_NEW_ID3 = $1;   (**< 0001 There is ID3 info that changed since last call to mpg123_id3. *)
  MPG123_ICY_CONST = $c; (**< 1100 There is some ICY info. Also matches 0100 or NEW_ICY.*)
  MPG123_NEW_ICY = $4;   (**< 0100 There is ICY info that changed since last call to mpg123_icy. *)

(** Query if there is (new) meta info, be it ID3 or ICY (or something new in future).
 *  \param mh handle
 *  \return combination of flags, 0 on error (same as "nothing new")
 *)
function mpg123_meta_check(mh: pmpg123_handle): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF}; (* On error (no valid handle) just 0 is returned. *)

(** Clean up meta data storage (ID3v2 and ICY), freeing memory.
 *  \param mh handle
 *)
procedure mpg123_meta_free(mh: pmpg123_handle); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Point v1 and v2 to existing data structures wich may change on any next read/decode function call.
 *  v1 and/or v2 can be set to NULL when there is no corresponding data.
 *  \return MPG123_OK on success
 *)
function mpg123_id3(mh: pmpg123_handle; v1: ppmpg123_id3v1; v2: ppmpg123_id3v2): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Point icy_meta to existing data structure wich may change on any next read/decode function call.
 *  \param mh handle
 *  \param icy_meta return address for ICY meta string (set to NULL if nothing there)
 *  \return MPG123_OK on success
 *)
function mpg123_icy(mh: pmpg123_handle; icy_meta: ppchar): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF}; (* same for ICY meta string *)

(** Decode from windows-1252 (the encoding ICY metainfo used) to UTF-8.
 *  Note that this is very similar to mpg123_store_utf8(&sb, mpg123_text_icy, icy_text, strlen(icy_text+1)) .
 *  \param icy_text The input data in ICY encoding
 *  \return pointer to newly allocated buffer with UTF-8 data (You free() it!) *)
function mpg123_icy2utf8(const icy_text: pchar): pchar; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(* @} *)

(** \defgroup mpg123_advpar mpg123 advanced parameter API
 *
 *  Direct access to a parameter set without full handle around it.
 *    Possible uses:
 *    - Influence behaviour of library _during_ initialization of handle (MPG123_VERBOSE).
 *    - Use one set of parameters for multiple handles.
 *
 *    The functions for handling mpg123_pars (mpg123_par() and mpg123_fmt()
 *  family) directly return a fully qualified mpg123 error code, the ones
 *  operating on full handles normally MPG123_OK or MPG123_ERR, storing the
 *  specific error code itseld inside the handle.
 *
 * @{
 *)

(** Opaque structure for the libmpg123 decoder parameters. *)
type
  pmpg123_pars_struct = ^mpg123_pars_struct;
  mpg123_pars_struct = record
  end;

(** Opaque structure for the libmpg123 decoder parameters. *)
type
  pmpg123_pars = ^mpg123_pars;
  mpg123_pars = pmpg123_pars_struct;

(** Create a handle with preset parameters.
 *  \param mp parameter handle
 *  \param decoder decoder choice
 *  \param error error code return address
 *  \return mpg123 handle
 *)
function mpg123_parnew(mp: pmpg123_pars; const decoder: pchar; error: pcint): pmpg123_handle; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Allocate memory for and return a pointer to a new mpg123_pars
 *  \param error error code return address
 *  \return new parameter handle
 *)
function mpg123_new_pars(error: pcint): pmpg123_pars; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Delete and free up memory used by a mpg123_pars data structure
 *  \param mp parameter handle
 *)
procedure mpg123_delete_pars(mp: pmpg123_pars); cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Configure mpg123 parameters to accept no output format at all,
 * use before specifying supported formats with mpg123_format
 *  \param mp parameter handle
 *  \return MPG123_OK on success
 *)
function mpg123_fmt_none(mp: pmpg123_pars): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Configure mpg123 parameters to accept all formats
 *  (also any custom rate you may set) -- this is default.
 *  \param mp parameter handle
 *  \return MPG123_OK on success
 *)
function mpg123_fmt_all(mp: pmpg123_pars): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set the audio format support of a mpg123_pars in detail:
 * \param mp parameter handle
 * \param rate The sample rate value (in Hertz).
 * \param channels A combination of MPG123_STEREO and MPG123_MONO.
 * \param encodings A combination of accepted encodings for rate and channels,
 *                  p.ex MPG123_ENC_SIGNED16|MPG123_ENC_ULAW_8 (or 0 for no
 *                  support).
 * \return MPG123_OK on success
*)
function mpg123_fmt(mp: pmpg123_pars; rate: clong; channels: cint; encodings: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF}; (* 0 is good, -1 is error *)

(** Check to see if a specific format at a specific rate is supported
 *  by mpg123_pars.
 *  \param mp parameter handle
 *  \param rate sampling rate
 *  \param encoding encoding
 *  \return 0 for no support (that includes invalid parameters), MPG123_STEREO,
 *          MPG123_MONO or MPG123_STEREO|MPG123_MONO. *)
function mpg123_fmt_support(mp: pmpg123_pars; rate: clong; encoding: cint): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Set a specific parameter, for a specific mpg123_pars, using a parameter
 *  type key chosen from the mpg123_parms enumeration, to the specified value.
 *  \param mp parameter handle
 *  \param type parameter choice
 *  \param value integer value
 *  \param fvalue floating point value
 *  \return MPG123_OK on success
 *)
function mpg123_par(mp: pmpg123_pars; _type: mpg123_parms; value: clong; fvalue: cdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Get a specific parameter, for a specific mpg123_pars.
 *  See the mpg123_parms enumeration for a list of available parameters.
 *  \param mp parameter handle
 *  \param type parameter choice
 *  \param value integer value return address
 *  \param fvalue floating point value return address
 *  \return MPG123_OK on success
 *)
function mpg123_getpar(mp: pmpg123_pars; _type: mpg123_parms; val: pclong; fval: pcdouble): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(* @} *)


(** \defgroup mpg123_lowio mpg123 low level I/O
  * You may want to do tricky stuff with I/O that does not work with mpg123's default file access or you want to make it decode into your own pocket...
  *
  * @{ *)

(** Replace default internal buffer with user-supplied buffer.
  * Instead of working on it's own private buffer, mpg123 will directly use the one you provide for storing decoded audio.
  * Note that the required buffer size could be bigger than expected from output
  * encoding if libmpg123 has to convert from primary decoder output (p.ex. 32 bit
  * storage for 24 bit output).
  * \param mh handle
  * \param data pointer to user buffer
  * \param size of buffer in bytes
  * \return MPG123_OK on success
  *)
function mpg123_replace_buffer(mh: pmpg123_handle; data: pcuchar; size: csize_t): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** The max size of one frame's decoded output with current settings.
 *  Use that to determine an appropriate minimum buffer size for decoding one frame.
 *  \param mh handle
 *  \return maximum decoded data size in bytes
 *)
function mpg123_outblock(mh: pmpg123_handle): csize_t; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Replace low-level stream access functions; read and lseek as known in POSIX.
 *  You can use this to make any fancy file opening/closing yourself,
 *  using mpg123_open_fd() to set the file descriptor for your read/lseek
 *  (doesn't need to be a "real" file descriptor...).
 *  Setting a function to NULL means that the default internal read is
 *  used (active from next mpg123_open call on).
 *  Note: As it would be troublesome to mess with this while having a file open,
 *  this implies mpg123_close().
 * \param mh handle
 * \param r_read callback for reading (behaviour like POSIX read)
 * \param r_lseek callback for seeking (like POSIX lseek)
 * \return MPG123_OK on success
 *)
type
  mpg123_readproc = function(fd: pointer; buf: pointer; count: csize_t): csize_t; cdecl;
  mpg123_seekproc = function(fd: pointer; count: off_t; offset: cint): off_t; cdecl;

function mpg123_replace_reader(mh: pmpg123_handle; r_read: mpg123_readproc; r_lseek: mpg123_seekproc): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(** Replace I/O functions with your own ones operating on some kind of
 *  handle instead of integer descriptors.
 *  The handle is a void pointer, so you can pass any data you want...
 *  mpg123_open_handle() is the call you make to use the I/O defined here.
 *  There is no fallback to internal read/seek here.
 *  Note: As it would be troublesome to mess with this while having a file open,
 *  this mpg123_close() is implied here.
 *  \param mh handle
 *  \param r_read callback for reading (behaviour like POSIX read)
 *  \param r_lseek callback for seeking (like POSIX lseek)
 *  \param cleanup A callback to clean up an I/O handle on mpg123_close,
 *         can be NULL for none (you take care of cleaning your handles).
 * \return MPG123_OK on success
 *)
function mpg123_replace_reader_handle(mh: pmpg123_handle; r_read: mpg123_readproc; r_lseek: mpg123_seekproc; cleanup: pointer): cint; cdecl; external {$IFDEF MPG123_DYNAMIC}LIB_MPG123{$ENDIF};

(* @} *)

implementation

end.

