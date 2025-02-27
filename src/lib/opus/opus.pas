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

unit opus;

{$MODE OBJFPC}{$H+}
{$PACKRECORDS C}
{$MINENUMSIZE 4}

interface

uses
  CTypes, SysUtils, ogg;

{$IF DEFINED(WINDOWS)}
  {$IFDEF OPUS_WINDOWS_STATIC}
    {$LINKLIB libopusfile.a}
    {$LINKLIB libopus.a}
  {$ELSE}
    {$DEFINE OPUS_DYNAMIC}
    const opuslib = 'libopus.dll';
    const opusfilelib = 'libopusfile.dll';
  {$ENDIF}
{$ELSEIF DEFINED(UNIX)}
  {$DEFINE OPUS_DYNAMIC}
  {$LINKLIB libopus}
  {$LINKLIB libopusfile}
  const opuslib = 'libopus.so';
  const opusfilelib = 'libopusfile.so';
{$ELSE}
  {$ERROR libopus is not supported on this platform. Fix it!}
{$ENDIF}

const
  OP_FALSE = -1;
  OP_HOLE = -3;
  OP_EREAD = -128;
  OP_EFAULT = -129;
  OP_EIMPL = -130;
  OP_EINVAL = -131;
  OP_ENOTVORBIS = -132;
  OP_EBADHEADER = -133;
  OP_EVERSION = -134;
  OP_ENOTAUDIO = -135;
  OP_EBADPACKET = -136;
  OP_EBADLINK = -137;
  OP_ENOSEEK = -138;
  OP_EBADTIMESTAMP = -139;

const
  OPUS_OK = 0;
  OPUS_BAD_ARG = -1;
  OPUS_BUFFER_TOO_SMALL = -2;
  OPUS_INTERNAL_ERROR = -3;
  OPUS_INVALID_PACKET = -4;
  OPUS_UNIMPLEMENTED = -5;
  OPUS_INVALID_STATE = -6;
  OPUS_ALLOC_FAIL = -7;

  OPUS_APPLICATION_VOIP = 2048;
  OPUS_APPLICATION_AUDIO = 2049;
  OPUS_APPLICATION_RESTRICTED_LOWDELAY = 2051;

  OPUS_SIGNAL_VOICE = 3001; // Signal being encoded is voice
  OPUS_SIGNAL_MUSIC = 3002; // Signal being encoded is music

  OPUS_BANDWIDTH_NARROWBAND = 1101; // 4 kHz bandpass @hideinitializer
  OPUS_BANDWIDTH_MEDIUMBAND = 1102; // 6 kHz bandpass @hideinitializer
  OPUS_BANDWIDTH_WIDEBAND = 1103;  // 8 kHz bandpass @hideinitializer
  OPUS_BANDWIDTH_SUPERWIDEBAND = 1104; // 12 kHz bandpass @hideinitializer
  OPUS_BANDWIDTH_FULLBAND = 1105; // 20 kHz bandpass @hideinitializer

  OPUS_FRAMESIZE_ARG = 5000; // Select frame size from the argument (default)
  OPUS_FRAMESIZE_2_5_MS = 5001; // Use 2.5 ms frames
  OPUS_FRAMESIZE_5_MS = 5002; // Use 5 ms frames
  OPUS_FRAMESIZE_10_MS = 5003; // Use 10 ms frames
  OPUS_FRAMESIZE_20_MS = 5004; // Use 20 ms frames
  OPUS_FRAMESIZE_40_MS = 5005; // Use 40 ms frames
  OPUS_FRAMESIZE_60_MS = 5006; // Use 60 ms frames

  OPUS_CHANNEL_COUNT_MAX = 255;

const
  OPUS_SET_APPLICATION_REQUEST = 4000;
  OPUS_GET_APPLICATION_REQUEST = 4001;
  OPUS_SET_BITRATE_REQUEST = 4002;
  OPUS_GET_BITRATE_REQUEST = 4003;
  OPUS_SET_MAX_BANDWIDTH_REQUEST = 4004;
  OPUS_GET_MAX_BANDWIDTH_REQUEST = 4005;
  OPUS_SET_VBR_REQUEST = 4006;
  OPUS_GET_VBR_REQUEST = 4007;
  OPUS_SET_BANDWIDTH_REQUEST = 4008;
  OPUS_GET_BANDWIDTH_REQUEST = 4009;
  OPUS_SET_COMPLEXITY_REQUEST = 4010;
  OPUS_GET_COMPLEXITY_REQUEST = 4011;
  OPUS_SET_INBAND_FEC_REQUEST = 4012;
  OPUS_GET_INBAND_FEC_REQUEST = 4013;
  OPUS_SET_PACKET_LOSS_PERC_REQUEST = 4014;
  OPUS_GET_PACKET_LOSS_PERC_REQUEST = 4015;
  OPUS_SET_DTX_REQUEST = 4016;
  OPUS_GET_DTX_REQUEST = 4017;
  OPUS_SET_VBR_CONSTRAINT_REQUEST = 4020;
  OPUS_GET_VBR_CONSTRAINT_REQUEST = 4021;
  OPUS_SET_FORCE_CHANNELS_REQUEST = 4022;
  OPUS_GET_FORCE_CHANNELS_REQUEST = 4023;
  OPUS_SET_SIGNAL_REQUEST = 4024;
  OPUS_GET_SIGNAL_REQUEST = 4025;
  OPUS_GET_LOOKAHEAD_REQUEST = 4027;
  OPUS_RESET_STATE_REQUEST = 4028;
  OPUS_GET_SAMPLE_RATE_REQUEST = 4029;
  OPUS_GET_FINAL_RANGE_REQUEST = 4031;
  OPUS_GET_PITCH_REQUEST = 4033;
  OPUS_SET_GAIN_REQUEST = 4034;
  OPUS_GET_GAIN_REQUEST = 4045;
  OPUS_SET_LSB_DEPTH_REQUEST = 4036;
  OPUS_GET_LSB_DEPTH_REQUEST = 4037;
  OPUS_GET_LAST_PACKET_DURATION_REQUEST = 4039;
  OPUS_SET_EXPERT_FRAME_DURATION_REQUEST = 4040;
  OPUS_GET_EXPERT_FRAME_DURATION_REQUEST = 4041;
  OPUS_SET_PREDICTION_DISABLED_REQUEST = 4042;
  OPUS_GET_PREDICTION_DISABLED_REQUEST = 4043;
  OPUS_MULTISTREAM_GET_ENCODER_STATE_REQUEST = 5120;
  OPUS_MULTISTREAM_GET_DECODER_STATE_REQUEST = 5122;

type
  OpusHead = record
    version: cint;
    channel_count: cint;
    pre_skip: cuint;
    input_sample_rate: cuint32;
    output_gain: cint;
    mapping_family: cint;
    stream_count: cint;
    coupled_count: cint;
    mapping: array [0..OPUS_CHANNEL_COUNT_MAX-1] of byte;
  end;
  POpusHead = ^OpusHead;

type
  OggOpusFile = record end;
  POggOpusFile = ^OggOpusFile;

type
  op_read_func = function (stream: Pointer; buffer: pointer; nbytes: cint): cint; cdecl;
  op_seek_func = function (stream: Pointer; offset: Int64; whence: cint): cint; cdecl;
  op_tell_func = function (stream: Pointer): Int64; cdecl;
  op_close_func = function (stream: Pointer): cint; cdecl;

  OpusFileCallbacks = record
    read: op_read_func;
    seek: op_seek_func;
    tell: op_tell_func;
    close: op_close_func;
  end;

function opus_get_version_string(): PAnsiChar; cdecl; external {$IFDEF OPUS_DYNAMIC}opuslib{$ENDIF};
function opus_strerror(error: cint): PAnsiChar; cdecl; external {$IFDEF OPUS_DYNAMIC}opuslib{$ENDIF};

function op_open_file(path: pchar; err: pcint): POggOpusFile; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_open_memory(data: pointer; size: csize_t; err: pcint): POggOpusFile; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};

function op_test_file(path: pchar; err: pcint): POggOpusFile; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_test_memory(data: pointer; size: csize_t; err: pcint): POggOpusFile; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};

procedure op_free(opf: POggOpusFile); cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};

function op_seekable(opf: POggOpusFile): cint; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_channel_count(opf: POggOpusFile): cuint32; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_head(opf: POggOpusFile; li: cint): POpusHead; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_pcm_tell(opf: POggOpusFile): cint64; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_pcm_total(opf: POggOpusFile; li: cint): cint64; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};

function op_read(opf: POggOpusFile; pcm: pcint16; bufsiz: cint; li: cint): cint; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_read_stereo(opf: POggOpusFile; pcm: pcint16; bufsiz: cint): cint; cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};
function op_pcm_seek(opf: POggOpusFile; pos: cint64): cint;  cdecl; external {$IFDEF OPUS_DYNAMIC}opusfilelib{$ENDIF};

implementation

end.
