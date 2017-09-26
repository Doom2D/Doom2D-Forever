(*
 * jit.h - General definitions for JIT back-ends.
 *
 * Copyright (C) 2004  Southern Storm Software, Pty Ltd.
 * Copyright (C) 2016  Ketmar Dark
 *
 * The libjit library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * The libjit library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with the libjit library.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)
unit libjit_types;

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$MODE OBJFPC}
{$PACKRECORDS C}
{$MACRO ON}

{$Z4} // Force four-byte enums

interface

uses
  libjit;

//LIBJIT_LIBNAME = 'libjit.dll';
{$IF not DEFINED(MSWINDOWS)}
(*const*) var jit_opcodes: packed array [0..JIT_OP_NUM_OPCODES-1] of jit_opcode_info_t; cvar; external LIBJIT_LIBNAME;

(*
 * Pre-defined type descriptors.
 *)
(*const*) var jit_type_void: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sbyte: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_ubyte: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_short: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_ushort: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_int: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_uint: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_nint: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_nuint: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_long: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_ulong: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_float32: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_float64: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_nfloat: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_void_ptr: jit_type_t; cvar; external LIBJIT_LIBNAME;

(*
 * Type descriptors for the system "char", "int", "long", etc types.
 * These are defined to one of the above values.
 *)
(*const*) var jit_type_sys_bool: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_char: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_schar: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_uchar: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_short: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_ushort: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_int: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_uint: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_long: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_ulong: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_longlong: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_ulonglong: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_float: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_double: jit_type_t; cvar; external LIBJIT_LIBNAME;
(*const*) var jit_type_sys_long_double: jit_type_t; cvar; external LIBJIT_LIBNAME;

{$ELSE}
function jit_type_void (): jit_type_t;
function jit_type_sbyte (): jit_type_t;
function jit_type_ubyte (): jit_type_t;
function jit_type_short (): jit_type_t;
function jit_type_ushort (): jit_type_t;
function jit_type_int (): jit_type_t;
function jit_type_uint (): jit_type_t;
function jit_type_nint (): jit_type_t;
function jit_type_nuint (): jit_type_t;
function jit_type_long (): jit_type_t;
function jit_type_ulong (): jit_type_t;
function jit_type_float32 (): jit_type_t;
function jit_type_float64 (): jit_type_t;
function jit_type_nfloat (): jit_type_t;
function jit_type_void_ptr (): jit_type_t;

function jit_type_sys_bool (): jit_type_t;
function jit_type_sys_char (): jit_type_t;
function jit_type_sys_schar (): jit_type_t;
function jit_type_sys_uchar (): jit_type_t;
function jit_type_sys_short (): jit_type_t;
function jit_type_sys_ushort (): jit_type_t;
function jit_type_sys_int (): jit_type_t;
function jit_type_sys_uint (): jit_type_t;
function jit_type_sys_long (): jit_type_t;
function jit_type_sys_ulong (): jit_type_t;
function jit_type_sys_longlong (): jit_type_t;
function jit_type_sys_ulonglong (): jit_type_t;
function jit_type_sys_float (): jit_type_t;
function jit_type_sys_double (): jit_type_t;
function jit_type_sys_long_double (): jit_type_t;
{$ENDIF}


implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows, SysUtils;

var
  libjitH: HModule = 0;


function libjitImport (const name: AnsiString): Pointer;
begin
  if (libjitH = 0) then
  begin
    libjitH := LoadLibrary(LIBJIT_LIBNAME);
    if (libjitH = 0) then raise Exception.Create('cannot load '+LIBJIT_LIBNAME);
  end;
  result := GetProcAddress(libjitH, PAnsiChar(name));
  if (result = nil) then raise Exception.Create('cannot load '''+name+''' from '+LIBJIT_LIBNAME);
end;


var
  imp_jit_type_void: Pointer = nil;
  imp_jit_type_sbyte: Pointer = nil;
  imp_jit_type_ubyte: Pointer = nil;
  imp_jit_type_short: Pointer = nil;
  imp_jit_type_ushort: Pointer = nil;
  imp_jit_type_int: Pointer = nil;
  imp_jit_type_uint: Pointer = nil;
  imp_jit_type_nint: Pointer = nil;
  imp_jit_type_nuint: Pointer = nil;
  imp_jit_type_long: Pointer = nil;
  imp_jit_type_ulong: Pointer = nil;
  imp_jit_type_float32: Pointer = nil;
  imp_jit_type_float64: Pointer = nil;
  imp_jit_type_nfloat: Pointer = nil;
  imp_jit_type_void_ptr: Pointer = nil;

  imp_jit_type_sys_bool: Pointer = nil;
  imp_jit_type_sys_char: Pointer = nil;
  imp_jit_type_sys_schar: Pointer = nil;
  imp_jit_type_sys_uchar: Pointer = nil;
  imp_jit_type_sys_short: Pointer = nil;
  imp_jit_type_sys_ushort: Pointer = nil;
  imp_jit_type_sys_int: Pointer = nil;
  imp_jit_type_sys_uint: Pointer = nil;
  imp_jit_type_sys_long: Pointer = nil;
  imp_jit_type_sys_ulong: Pointer = nil;
  imp_jit_type_sys_longlong: Pointer = nil;
  imp_jit_type_sys_ulonglong: Pointer = nil;
  imp_jit_type_sys_float: Pointer = nil;
  imp_jit_type_sys_double: Pointer = nil;
  imp_jit_type_sys_long_double: Pointer = nil;


function jit_type_void (): jit_type_t; begin if (imp_jit_type_void = nil) then imp_jit_type_void := jit_type_t(libjitImport('jit_type_void')); result := imp_jit_type_void; end;
function jit_type_sbyte (): jit_type_t; begin if (imp_jit_type_sbyte = nil) then imp_jit_type_sbyte := jit_type_t(libjitImport('jit_type_sbyte')); result := imp_jit_type_sbyte; end;
function jit_type_ubyte (): jit_type_t; begin if (imp_jit_type_ubyte = nil) then imp_jit_type_ubyte := jit_type_t(libjitImport('jit_type_ubyte')); result := imp_jit_type_ubyte; end;
function jit_type_short (): jit_type_t; begin if (imp_jit_type_short = nil) then imp_jit_type_short := jit_type_t(libjitImport('jit_type_short')); result := imp_jit_type_short; end;
function jit_type_ushort (): jit_type_t; begin if (imp_jit_type_ushort = nil) then imp_jit_type_ushort := jit_type_t(libjitImport('jit_type_ushort')); result := imp_jit_type_ushort; end;
function jit_type_int (): jit_type_t; begin if (imp_jit_type_int = nil) then imp_jit_type_int := jit_type_t(libjitImport('jit_type_int')); result := imp_jit_type_int; end;
function jit_type_uint (): jit_type_t; begin if (imp_jit_type_uint = nil) then imp_jit_type_uint := jit_type_t(libjitImport('jit_type_uint')); result := imp_jit_type_uint; end;
function jit_type_nint (): jit_type_t; begin if (imp_jit_type_nint = nil) then imp_jit_type_nint := jit_type_t(libjitImport('jit_type_nint')); result := imp_jit_type_nint; end;
function jit_type_nuint (): jit_type_t; begin if (imp_jit_type_nuint = nil) then imp_jit_type_nuint := jit_type_t(libjitImport('jit_type_nuint')); result := imp_jit_type_nuint; end;
function jit_type_long (): jit_type_t; begin if (imp_jit_type_long = nil) then imp_jit_type_long := jit_type_t(libjitImport('jit_type_long')); result := imp_jit_type_long; end;
function jit_type_ulong (): jit_type_t; begin if (imp_jit_type_ulong = nil) then imp_jit_type_ulong := jit_type_t(libjitImport('jit_type_ulong')); result := imp_jit_type_ulong; end;
function jit_type_float32 (): jit_type_t; begin if (imp_jit_type_float32 = nil) then imp_jit_type_float32 := jit_type_t(libjitImport('jit_type_float32')); result := imp_jit_type_float32; end;
function jit_type_float64 (): jit_type_t; begin if (imp_jit_type_float64 = nil) then imp_jit_type_float64 := jit_type_t(libjitImport('jit_type_float64')); result := imp_jit_type_float64; end;
function jit_type_nfloat (): jit_type_t; begin if (imp_jit_type_nfloat = nil) then imp_jit_type_nfloat := jit_type_t(libjitImport('jit_type_nfloat')); result := imp_jit_type_nfloat; end;
function jit_type_void_ptr (): jit_type_t; begin if (imp_jit_type_void_ptr = nil) then imp_jit_type_void_ptr := jit_type_t(libjitImport('jit_type_void_ptr')); result := imp_jit_type_void_ptr; end;

function jit_type_sys_bool (): jit_type_t; begin if (imp_jit_type_sys_bool = nil) then imp_jit_type_sys_bool := jit_type_t(libjitImport('jit_type_sys_bool')); result := imp_jit_type_sys_bool; end;
function jit_type_sys_char (): jit_type_t; begin if (imp_jit_type_sys_char = nil) then imp_jit_type_sys_char := jit_type_t(libjitImport('jit_type_sys_char')); result := imp_jit_type_sys_char; end;
function jit_type_sys_schar (): jit_type_t; begin if (imp_jit_type_sys_schar = nil) then imp_jit_type_sys_schar := jit_type_t(libjitImport('jit_type_sys_schar')); result := imp_jit_type_sys_schar; end;
function jit_type_sys_uchar (): jit_type_t; begin if (imp_jit_type_sys_uchar = nil) then imp_jit_type_sys_uchar := jit_type_t(libjitImport('jit_type_sys_uchar')); result := imp_jit_type_sys_uchar; end;
function jit_type_sys_short (): jit_type_t; begin if (imp_jit_type_sys_short = nil) then imp_jit_type_sys_short := jit_type_t(libjitImport('jit_type_sys_short')); result := imp_jit_type_sys_short; end;
function jit_type_sys_ushort (): jit_type_t; begin if (imp_jit_type_sys_ushort = nil) then imp_jit_type_sys_ushort := jit_type_t(libjitImport('jit_type_sys_ushort')); result := imp_jit_type_sys_ushort; end;
function jit_type_sys_int (): jit_type_t; begin if (imp_jit_type_sys_int = nil) then imp_jit_type_sys_int := jit_type_t(libjitImport('jit_type_sys_int')); result := imp_jit_type_sys_int; end;
function jit_type_sys_uint (): jit_type_t; begin if (imp_jit_type_sys_uint = nil) then imp_jit_type_sys_uint := jit_type_t(libjitImport('jit_type_sys_uint')); result := imp_jit_type_sys_uint; end;
function jit_type_sys_long (): jit_type_t; begin if (imp_jit_type_sys_long = nil) then imp_jit_type_sys_long := jit_type_t(libjitImport('jit_type_sys_long')); result := imp_jit_type_sys_long; end;
function jit_type_sys_ulong (): jit_type_t; begin if (imp_jit_type_sys_ulong = nil) then imp_jit_type_sys_ulong := jit_type_t(libjitImport('jit_type_sys_ulong')); result := imp_jit_type_sys_ulong; end;
function jit_type_sys_longlong (): jit_type_t; begin if (imp_jit_type_sys_longlong = nil) then imp_jit_type_sys_longlong := jit_type_t(libjitImport('jit_type_sys_longlong')); result := imp_jit_type_sys_longlong; end;
function jit_type_sys_ulonglong (): jit_type_t; begin if (imp_jit_type_sys_ulonglong = nil) then imp_jit_type_sys_ulonglong := jit_type_t(libjitImport('jit_type_sys_ulonglong')); result := imp_jit_type_sys_ulonglong; end;
function jit_type_sys_float (): jit_type_t; begin if (imp_jit_type_sys_float = nil) then imp_jit_type_sys_float := jit_type_t(libjitImport('jit_type_sys_float')); result := imp_jit_type_sys_float; end;
function jit_type_sys_double (): jit_type_t; begin if (imp_jit_type_sys_double = nil) then imp_jit_type_sys_double := jit_type_t(libjitImport('jit_type_sys_double')); result := imp_jit_type_sys_double; end;
function jit_type_sys_long_double (): jit_type_t; begin if (imp_jit_type_sys_long_double = nil) then imp_jit_type_sys_long_double := jit_type_t(libjitImport('jit_type_sys_long_double')); result := imp_jit_type_sys_long_double; end;
{$ENDIF}


end.
