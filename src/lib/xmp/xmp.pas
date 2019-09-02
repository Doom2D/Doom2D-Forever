unit XMP;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$MODE OBJFPC}
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFNDEF LIBXMP_WINDOZE_STATIC}
    {$DEFINE XMP_DYNAMIC}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(XMP_DYNAMIC)}
const
{$IF DEFINED(WINDOWS)}
  {$IF DEFINED(USE_XMP_FULL)}
  xmplib = 'libxmp.dll';
  {$ELSE}
  xmplib = 'libxmp-lite.dll';
  {$ENDIF}
{$ELSEIF DEFINED(UNIX)}
  {$IF DEFINED(USE_XMP_FULL)}
  xmplib = 'libxmp.so';
  {$ELSE}
  xmplib = 'libxmp-lite.so';
  {$ENDIF}
{$ELSE}
  {$MESSAGE ERROR 'XMP_DYNAMIC not supported'}
{$IFEND}
{$ELSE}
  {$IF DEFINED(USE_XMP_FULL)}
  {$LINKLIB libxmp.a}
  {$ELSE}
  {$LINKLIB libxmp-lite.a}
  {$ENDIF}
{$ENDIF}

const
  XMP_VER_STRING = '4.4.1'; 
  XMP_VER_CODE = $040401; 
  XMP_VER_MAJOR = 4; 
  XMP_VER_MINOR = 4; 
  XMP_VER_RELEASE = 1; 

const
  XMP_NAME_SIZE = 64; (* Size of module name and type *)
  XMP_KEY_OFF = $81; (* Note number for key off event *)
  XMP_KEY_CUT = $82; (* Note number for key cut event *)
  XMP_KEY_FADE = $83; (* Note number for fade event *)
  (* mixer parameter macros *)
  (* sample format flags *)
  XMP_FORMAT_8BIT = (1 shl 0); (* Mix to 8-bit instead of 16 *)
  XMP_FORMAT_UNSIGNED = (1 shl 1); (* Mix to unsigned samples *)
  XMP_FORMAT_MONO = (1 shl 2); (* Mix to mono instead of stereo *)
  (* player parameters *)
  XMP_PLAYER_AMP = 0; (* Amplification factor *)
  XMP_PLAYER_MIX = 1; (* Stereo mixing *)
  XMP_PLAYER_INTERP = 2; (* Interpolation type *)
  XMP_PLAYER_DSP = 3; (* DSP effect flags *)
  XMP_PLAYER_FLAGS = 4; (* Player flags *)
  XMP_PLAYER_CFLAGS = 5; (* Player flags for current module *)
  XMP_PLAYER_SMPCTL = 6; (* Sample control flags *)
  XMP_PLAYER_VOLUME = 7; (* Player module volume *)
  XMP_PLAYER_STATE = 8; (* Internal player state (read only) *)
  XMP_PLAYER_SMIX_VOLUME = 9; (* SMIX volume *)
  XMP_PLAYER_DEFPAN = 10; (* Default pan setting *)
  XMP_PLAYER_MODE = 11; (* Player personality *)
  XMP_PLAYER_MIXER_TYPE = 12; (* Current mixer (read only) *)
  XMP_PLAYER_VOICES = 13; (* Maximum number of mixer voices *)
  (* interpolation types *)
  XMP_INTERP_NEAREST = 0; (* Nearest neighbor *)
  XMP_INTERP_LINEAR = 1; (* Linear (default) *)
  XMP_INTERP_SPLINE = 2; (* Cubic spline *)
  (* dsp effect types *)
  XMP_DSP_LOWPASS = (1 shl 0); (* Lowpass filter effect *)
  XMP_DSP_ALL = (XMP_DSP_LOWPASS); 
  (* player state *)
  XMP_STATE_UNLOADED = 0; (* Context created *)
  XMP_STATE_LOADED = 1; (* Module loaded *)
  XMP_STATE_PLAYING = 2; (* Module playing *)
  (* player flags *)
  XMP_FLAGS_VBLANK = (1 shl 0); (* Use vblank timing *)
  XMP_FLAGS_FX9BUG = (1 shl 1); (* Emulate FX9 bug *)
  XMP_FLAGS_FIXLOOP = (1 shl 2); (* Emulate sample loop bug *)
  XMP_FLAGS_A500 = (1 shl 3); (* Use Paula mixer in Amiga modules *)
  (* player modes *)
  XMP_MODE_AUTO = 0; (* Autodetect mode (default) *)
  XMP_MODE_MOD = 1; (* Play as a generic MOD player *)
  XMP_MODE_NOISETRACKER = 2; (* Play using Noisetracker quirks *)
  XMP_MODE_PROTRACKER = 3; (* Play using Protracker quirks *)
  XMP_MODE_S3M = 4; (* Play as a generic S3M player *)
  XMP_MODE_ST3 = 5; (* Play using ST3 bug emulation *)
  XMP_MODE_ST3GUS = 6; (* Play using ST3+GUS quirks *)
  XMP_MODE_XM = 7; (* Play as a generic XM player *)
  XMP_MODE_FT2 = 8; (* Play using FT2 bug emulation *)
  XMP_MODE_IT = 9; (* Play using IT quirks *)
  XMP_MODE_ITSMP = 10; (* Play using IT sample mode quirks *)
  (* mixer types *)
  XMP_MIXER_STANDARD = 0; (* Standard mixer *)
  XMP_MIXER_A500 = 1; (* Amiga 500 *)
  XMP_MIXER_A500F = 2; (* Amiga 500 with led filter *)
  (* sample flags *)
  XMP_SMPCTL_SKIP = (1 shl 0); (* Don't load samples *)
  (* limits *)
  XMP_MAX_KEYS = 121; (* Number of valid keys *)
  XMP_MAX_ENV_POINTS = 32; (* Max number of envelope points *)
  XMP_MAX_MOD_LENGTH = 256; (* Max number of patterns in module *)
  XMP_MAX_CHANNELS = 64; (* Max number of channels in module *)
  XMP_MAX_SRATE = 49170; (* max sampling rate (Hz) *)
  XMP_MIN_SRATE = 4000; (* min sampling rate (Hz) *)
  XMP_MIN_BPM = 20; (* min BPM *)
  (* frame rate = (50 * bpm / 125) Hz *)
  (* frame size = (sampling rate * channels * size) / frame rate *)
  XMP_MAX_FRAMESIZE = (5*XMP_MAX_SRATE*2 div XMP_MIN_BPM); 
  (* error codes *)
  XMP_END = 1; 
  XMP_ERROR_INTERNAL = 2; (* Internal error *)
  XMP_ERROR_FORMAT = 3; (* Unsupported module format *)
  XMP_ERROR_LOAD = 4; (* Error loading file *)
  XMP_ERROR_DEPACK = 5; (* Error depacking file *)
  XMP_ERROR_SYSTEM = 6; (* System error *)
  XMP_ERROR_INVALID = 7; (* Invalid parameter *)
  XMP_ERROR_STATE = 8; (* Invalid player state *)

const
  XMP_CHANNEL_SYNTH = (1 shl 0); (* Channel is synthesized *)
  XMP_CHANNEL_MUTED = (1 shl 1); (* Channel is muted *)
  XMP_CHANNEL_SPLIT = (1 shl 2); (* Split Amiga channel in bits 5-4 *)
  XMP_CHANNEL_SURROUND = (1 shl 4); (* Surround channel *)

const
  XMP_ENVELOPE_ON = (1 shl 0); (* Envelope is enabled *)
  XMP_ENVELOPE_SUS = (1 shl 1); (* Envelope has sustain point *)
  XMP_ENVELOPE_LOOP = (1 shl 2); (* Envelope has loop *)
  XMP_ENVELOPE_FLT = (1 shl 3); (* Envelope is used for filter *)
  XMP_ENVELOPE_SLOOP = (1 shl 4); (* Envelope has sustain loop *)
  XMP_ENVELOPE_CARRY = (1 shl 5); (* Don't reset envelope position *)

const
  XMP_INST_NNA_CUT = $00;
  XMP_INST_NNA_CONT = $01;
  XMP_INST_NNA_OFF = $02;
  XMP_INST_NNA_FADE = $03;
  XMP_INST_DCT_OFF = $00;
  XMP_INST_DCT_NOTE = $01;
  XMP_INST_DCT_SMP = $02;
  XMP_INST_DCT_INST = $03;
  XMP_INST_DCA_CUT = XMP_INST_NNA_CUT;
  XMP_INST_DCA_OFF = XMP_INST_NNA_OFF;
  XMP_INST_DCA_FADE = XMP_INST_NNA_FADE;

const
  XMP_SAMPLE_16BIT = (1 shl 0); (* 16bit sample *)
  XMP_SAMPLE_LOOP = (1 shl 1); (* Sample is looped *)
  XMP_SAMPLE_LOOP_BIDIR = (1 shl 2); (* Bidirectional sample loop *)
  XMP_SAMPLE_LOOP_REVERSE = (1 shl 3); (* Backwards sample loop *)
  XMP_SAMPLE_LOOP_FULL = (1 shl 4); (* Play full sample before looping *)
  XMP_SAMPLE_SLOOP = (1 shl 5); (* Sample has sustain loop *)
  XMP_SAMPLE_SLOOP_BIDIR = (1 shl 6); (* Bidirectional sustain loop *)
  XMP_SAMPLE_SYNTH = (1 shl 15); (* Data contains synth patch *)

type
  xmp_channel = record
    pan: longint; (* Channel pan (0x80 is center) *)
    vol: longint; (* Channel volume *)
    flg: longint; (* Channel flags *)
  end;

  xmp_pattern = record
    rows: longint; (* Number of rows *)
    index: array [0..0] of longint; (* Track index *)
  end;

  xmp_event = record
    note: byte; (* Note number (0 means no note) *)
    ins: byte; (* Patch number *)
    vol: byte; (* Volume (0 to basevol) *)
    fxt: byte; (* Effect type *)
    fxp: byte; (* Effect parameter *)
    f2t: byte; (* Secondary effect type *)
    f2p: byte; (* Secondary effect parameter *)
    _flag: byte; (* Internal (reserved) flags *)
  end;

  pxmp_event = ^xmp_event;

  xmp_track = record
    rows: longint; (* Number of rows *)
    event: array [0..0] of xmp_event; (* Event data *)
  end;

  xmp_envelope = record
    flg: longint; (* Flags *)
    npt: longint; (* Number of envelope points *)
    scl: longint; (* Envelope scaling *)
    sus: longint; (* Sustain start point *)
    sue: longint; (* Sustain end point *)
    lps: longint; (* Loop start point *)
    lpe: longint; (* Loop end point *)
    data: array [0..(XMP_MAX_ENV_POINTS*2)-1] of smallint; 
  end;

  xmp_subinstrument = record
    vol: longint; (* Default volume *)
    gvl: longint; (* Global volume *)
    pan: longint; (* Pan *)
    xpo: longint; (* Transpose *)
    fin: longint; (* Finetune *)
    vwf: longint; (* Vibrato waveform *)
    vde: longint; (* Vibrato depth *)
    vra: longint; (* Vibrato rate *)
    vsw: longint; (* Vibrato sweep *)
    rvv: longint; (* Random volume/pan variation (IT) *)
    sid: longint; (* Sample number *)
    nna: longint; (* New note action *)
    dct: longint; (* Duplicate check type *)
    dca: longint; (* Duplicate check action *)
    ifc: longint; (* Initial filter cutoff *)
    ifr: longint; (* Initial filter resonance *)
  end;

  pxmp_subinstrument = ^xmp_subinstrument;

  xmp_instrument = record
    name: array [0..31] of char; (* Instrument name *)
    vol: longint; (* Instrument volume *)
    nsm: longint; (* Number of samples *)
    rls: longint; (* Release (fadeout) *)
    aei: xmp_envelope; (* Amplitude envelope info *)
    pei: xmp_envelope; (* Pan envelope info *)
    fei: xmp_envelope; (* Frequency envelope info *)
    map : array[0..(XMP_MAX_KEYS)-1] of record
      ins : byte;
      xpo : char;
    end;
    sub: pxmp_subinstrument;
    extra: pointer;
  end;

  xmp_sample = record
    name: array [0..31] of char; (* Sample name *)
    len: longint; (* Sample length *)
    lps: longint; (* Loop start *)
    lpe: longint; (* Loop end *)
    flg: longint; (* Flags *)
    data: pbyte;  (* Sample data *)
  end;

  xmp_sequence = record
    entry_point: longint; 
    duration: longint; 
  end;

  pxmp_sequence = ^xmp_sequence;

  xmp_module = record
    name: array [0..XMP_NAME_SIZE-1] of char; (* Module title *)
    format: array [0..XMP_NAME_SIZE-1] of char; (* Module format *)

    pat: longint; (* Number of patterns *)
    trk: longint; (* Number of tracks *)
    chn: longint; (* Tracks per pattern *)
    ins: longint; (* Number of instruments *)
    smp: longint; (* Number of samples *)
    spd: longint; (* Initial speed *)
    bpm: longint; (* Initial BPM *)
    len: longint; (* Module length in patterns *)
    rst: longint; (* Restart position *)
    gvl: longint; (* Global volume *)

    xxp: pointer; // xmp_pattern**
    xxt: pointer; // xmp_track**
    xxi: pointer; // xmp_instrument*
    xxs: pointer; // xmp_sample*
    xxc: array [0..XMP_MAX_CHANNELS-1] of xmp_channel;
    xxo: array [0..XMP_MAX_MOD_LENGTH-1] of byte;
  end;

  pxmp_module = ^xmp_module;

  xmp_test_info = record
    name: array [0..XMP_NAME_SIZE-1] of char; (* Module title *)
    format: array [0..XMP_NAME_SIZE-1] of char; (* Module format *)
  end;

  xmp_module_info = record
    md5: array [0..15] of byte; (* MD5 message digest *)
    vol_base: longint; (* Volume scale *)
    module: pxmp_module; (* Pointer to module data *)
    comment: pchar; (* Comment text, if any *)
    num_sequences: longint; (* Number of valid sequences *)
    seq_data: pxmp_sequence; (* Pointer to sequence data *)
  end;

  xmp_channel_info = record
    period: longword; (* Sample period (times 4096) *)
    position: longword; (* Sample position *)
    pitchbend: word; (* Linear bend from base note*)
    note: byte; (* Current base note number *)
    instrument: byte; (* Current instrument number *)
    usample: byte; (* Current sample number *)
    volume: byte; (* Current volume *)
    pan: byte; (* Current stereo pan *)
    reserved: byte; (* Reserved *)
    event: xmp_event; (* Current track event *)
  end;

  (* Current frame information *)
  xmp_frame_info = record
    pos: longint; (* Current position *)
    pattern: longint; (* Current pattern *)
    row: longint; (* Current row in pattern *)
    num_rows: longint; (* Number of rows in current pattern *)
    frame: longint; (* Current frame *)
    speed: longint; (* Current replay speed *)
    bpm: longint; (* Current bpm *)
    time: longint; (* Current module time in ms *)
    total_time: longint; (* Estimated replay time in ms*)
    frame_time: longint; (* Frame replay time in us *)
    buffer: plongint; (* Pointer to sound buffer *)
    buffer_size: longint; (* Used buffer size *)
    total_size: longint; (* Total buffer size *)
    volume: longint; (* Current master volume *)
    loop_count: longint; (* Loop counter *)
    virt_channels: longint; (* Number of virtual channels *)
    virt_used: longint; (* Used virtual channels *)
    sequence: longint; (* Current sequence *)
    channel_info: array [0..XMP_MAX_CHANNELS-1] of xmp_channel_info;
  end;

  xmp_context = pointer; 

var
  xmp_version: pchar; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
  xmp_vercode: longword; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

function xmp_create_context(): xmp_context; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_free_context(ctx: xmp_context); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_get_format_list(): ppchar; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

function xmp_test_module(fname: pchar; var info: xmp_test_info): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF}; 
function xmp_load_module(ctx: xmp_context; fname: pchar): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_load_module_from_memory(ctx: xmp_context; buf: pointer; bufsiz: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_load_module_from_file(ctx: xmp_context; fstream: pointer; msize: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_scan_module(ctx: xmp_context); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_get_module_info(ctx: xmp_context; var info: xmp_module_info); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_release_module(ctx: xmp_context); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

function xmp_start_player(ctx: xmp_context; rate, format: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_play_frame(ctx: xmp_context): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF}; 
function xmp_play_buffer(ctx: xmp_context; buf: pointer; bufsiz: longint; loop: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_get_frame_info(ctx: xmp_context; var info: xmp_frame_info); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_end_player(ctx: xmp_context); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

procedure xmp_inject_event(ctx: xmp_context; ch: longint; ev: pxmp_event); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

function xmp_next_position(ctx: xmp_context): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_prev_position(ctx: xmp_context): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_set_position(ctx: xmp_context; pos: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_seek_time(ctx: xmp_context; ms: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_stop_module(ctx: xmp_context); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
procedure xmp_restart_module(ctx: xmp_context); cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

function xmp_channel_mute(ctx: xmp_context; ch, mute: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_channel_vol(ctx: xmp_context; ch, vol: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

function xmp_set_player(ctx: xmp_context; param, value: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_get_player(ctx: xmp_context; param: longint): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};
function xmp_set_instrument_path(ctx: xmp_context; path: pchar): longint; cdecl; external {$IFDEF XMP_DYNAMIC}xmplib{$ENDIF};

(*
function xmp_start_smix; 
procedure xmp_end_smix; 
function xmp_smix_play_instrument; 
function xmp_smix_play_sample; 
function xmp_smix_channel_pan; 
function xmp_smix_load_sample; 
function xmp_smix_release_sample; 
*)

implementation


end.
