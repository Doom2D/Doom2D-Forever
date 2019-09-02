unit fluidsynth;

{$MODE OBJFPC}{$H+}

interface

uses
  ctypes;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$IF DEFINED(WINDOWS)}
  {$IFDEF FLUIDSYNTH_WINDOZE_STATIC}
    {$LINKLIB libfluidsynth.a}
  {$ELSE}
    {$DEFINE FS_DYNAMIC}
    const fluidlib = 'libfluidsynth.dll';
  {$ENDIF}
{$ELSEIF DEFINED(UNIX)}
  {$DEFINE FS_DYNAMIC}
  const fluidlib = 'libfluidsynth.so';
{$ELSE}
  {$ERROR fluidsynth not supported on this platform. Fix it!}
{$ENDIF}

const
  FLUID_OK = 0;
  FLUID_FAILED = -1;

type
  pfluid_settings_t = pointer;
  pfluid_synth_t = pointer;
  pfluid_player_t = pointer;
  pfluid_sfont_t = pointer;
  pfluid_sfloader_t = pointer;

function fluid_version_str(): pchar; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
procedure fluid_version(major, minor, patch: pcint); cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};

function new_fluid_settings(): pfluid_settings_t; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
procedure delete_fluid_settings(s: pfluid_settings_t); cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};

function fluid_settings_setstr(s: pfluid_settings_t; key, val: pchar): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_settings_getstr(s: pfluid_settings_t; key: pchar; var val: pchar): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_settings_setnum(s: pfluid_settings_t; key: pchar; val: cdouble): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_settings_getnum(s: pfluid_settings_t; key: pchar; var val: cdouble): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_settings_setint(s: pfluid_settings_t; key: pchar; val: cint): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_settings_getint(s: pfluid_settings_t; key: pchar; var val: cint): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};

function new_fluid_synth(settings: pfluid_settings_t): pfluid_synth_t; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function delete_fluid_synth(synth: pfluid_synth_t): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_synth_get_settings(synth: pfluid_synth_t): pfluid_settings_t; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_synth_sfload(synth: pfluid_synth_t; fname: pchar; reset: cint): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};

function fluid_synth_write_s16(synth: pfluid_synth_t; len: cint; lout: pointer; loff, linc: cint; rout: pointer; roff, rinc: cint): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};

function new_fluid_player(synth: pfluid_synth_t): pfluid_player_t; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function delete_fluid_player(player: pfluid_player_t): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_player_add(player: pfluid_player_t; fname: pchar): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_player_add_mem(player: pfluid_player_t; buf: pointer; len: csize_t): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_player_play(player: pfluid_player_t): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_player_stop(player: pfluid_player_t): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_player_set_loop(player: pfluid_player_t; loop: cint): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};
function fluid_player_get_status(player: pfluid_player_t): cint; cdecl; external {$IFDEF FS_DYNAMIC}fluidlib{$ENDIF};

implementation

end.
