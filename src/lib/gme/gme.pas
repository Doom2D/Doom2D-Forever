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

unit GME;

{$IFDEF FPC}
{$PACKRECORDS C}
{$MODE OBJFPC}
{$ENDIF}

interface

uses ctypes;

{$IF DEFINED(WINDOWS)}
  {$IFDEF LIBGME_WINDOZE_STATIC}
    {$ERROR libgme won't static-link on Windows until we switch to FPC 3.2.0}
    // {$LINKLIB libgme.a}
  {$ELSE}
    {$DEFINE GME_DYNAMIC}
    const gmelib = 'libgme.dll';
  {$ENDIF}
{$ELSEIF DEFINED(UNIX)}
  {$DEFINE GME_DYNAMIC}
  {$LINKLIB libgme}
  const gmelib = 'libgme.so';
{$ELSE}
  {$ERROR libgme not supported on this platform. Fix it!}
{$ENDIF}

type
  // first parameter of most gme_ functions is a pointer to the Music_Emu
  pgme_music_emu = pointer;
  ppgme_music_emu = ^pgme_music_emu;

  // track information
  gme_info_t = record
    // times in milliseconds, -1 if unknown
    length:       longint; // total length, if file specifies it
    intro_length: longint; // length of song up to looping section
    loop_length:  longint; // length of looping section
    play_length:  longint; // length if available, otherwise intro_length+loop_length*2 if available,
                           // otherwise a default of 150000 (2.5 minutes).
    i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15: longint; // reserved (jesus christ)
    // various metadata (empty string if not available)
    system:    pchar;
    game:      pchar;
    song:      pchar;
    author:    pchar;
    copyright: pchar;
    comment:   pchar;
    dump:      pchar;
    // reserved (holy fuck)
    s7,s8,s9,s10,s11,s12,s13,s14,s15: pchar; 
  end;
  pgme_info_t = ^gme_info_t;
  ppgme_info_t = ^pgme_info_t;

  // frequency equalizer parameters
  gme_equalizer_t = record
    treble: double;
    bass: double;
    d2,d3,d4,d5,d6,d7,d8,d9: double; // reserved (please stop)
  end;
  pgme_equalizer_t = ^gme_equalizer_t;

  // music file type identifier; can also hold NULL
  gme_type_t = pointer;
  pgme_type_t = ^gme_type_t;

  // all errors are just const char* msg, NULL === success
  gme_err_t = pchar;

const
  gme_info_only = -1;

var
  // emulator type constants for each supported file type
  gme_ay_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_gbs_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_gym_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_hes_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_kss_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_nsf_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_nsfe_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_sap_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_spc_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_vgm_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};
  gme_vgz_type: gme_type_t; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

  // error returned if GME encounters an invalid file type
  gme_wrong_file_type: pchar; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

{ basic API }

// create emulator and load game music file/data into it. Sets *out to new emulator.
function gme_open_file(const path: pchar; eout: ppgme_music_emu; sample_rate: longint): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// same as gme_open_file(), but uses file data already in memory; makes copy of data;
// the resulting Music_Emu object will be set to single channel mode
function gme_open_data(const data: pointer; size: clong; eout: ppgme_music_emu; sample_rate: longint): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// number of tracks available
function gme_track_count(const emu: pgme_music_emu): longint; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// start a track, where 0 is the first track
function gme_start_track(emu: pgme_music_emu; track: longint): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// generate 'count' 16-bit signed samples into 'out'; output is in stereo
function gme_play(emu: pgme_music_emu; count: longint; buf: pword): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// finish using emulator and free memory
procedure gme_delete(emu: pgme_music_emu); cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

{ track positioning }

// Set time to start fading track out. Once fade ends track_ended() returns true.
// Fade time can be changed while track is playing.
procedure gme_set_fade(emu: pgme_music_emu; start_msec: longint); cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// true if a track has reached its end
function gme_track_ended(const emu: pgme_music_emu): longint; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// number of milliseconds (1000 = one second) played since beginning of track 
function gme_tell(const emu: pgme_music_emu): longint; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// number of samples generated since beginning of track
function gme_tell_samples(const emu: pgme_music_emu): longint; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// seek to new time in track; seeking backwards or far forward can take a while
function gme_seek(emu: pgme_music_emu; msec: longint): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// equivalent to restarting track then skipping n samples
function gme_seek_samples(emu: pgme_music_emu; n: longint): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// If do_autoload_limit is nonzero, then automatically load track length
// metadata (if present) and terminate playback once the track length has been
// reached. Otherwise playback will continue for an arbitrary period of time
// until a prolonged period of silence is detected.
// By default, playback limits are loaded and applied.
procedure gme_set_autoload_playback_limit(emu: pgme_music_emu; do_autoload_limit: longint); cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

{ informational }

// most recent warning string, or NULL if none; clears current warning after returning
function gme_warning(emu: pgme_music_emu): pchar; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// gets information for a particular track (length, name, author, etc.); must be freed after use
function gme_track_info(const emu: pgme_music_emu; iout: ppgme_info_t; track: longint): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// frees track information
procedure gme_free_info(info: pgme_info_t); cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

{ advanced playback }

// Adjust stereo echo depth, where 0.0 = off and 1.0 = maximum.
// Has no effect for GYM, SPC, and Sega Genesis VGM music
procedure gme_set_stereo_depth(emu: pgme_music_emu; depth: double); cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// enables/disables most accurate sound emulation options
procedure gme_enable_accuracy(emu: pgme_music_emu; enable: longint); cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

{ music type ident }

// Type of this emulator
function gme_type(const emu: pgme_music_emu): gme_type_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// Pointer to array of all music types, with NULL entry at end. Allows a player linked
// to this library to support new music types without having to be updated.
function gme_type_list(): pgme_type_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// Determine likely game music type based on first four bytes of file. Returns
// string containing proper file suffix (i.e. "NSF", "SPC", etc.) or "" if
// file header is not recognized.
function gme_identify_header(const header: pointer): pchar; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// Get corresponding music type for file path or extension passed in.
function gme_identify_extension(const path_or_extension: pchar): gme_type_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// Create new emulator and set sample rate. Returns NULL if out of memory.
// If you only need track information, pass gme_info_only for sample_rate.
function gme_new_emu(stype: gme_type_t; sample_rate: longint): pgme_music_emu; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// Load music file into emulator
function gme_load_file(emu: pgme_music_emu; const path: pchar): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};

// Load music file from memory into emulator. Makes a copy of data passed.
function gme_load_data(emu: pgme_music_emu; const data: pointer; len: clong): gme_err_t; cdecl; external {$IFDEF GME_DYNAMIC}gmelib{$ENDIF};


implementation


end.
