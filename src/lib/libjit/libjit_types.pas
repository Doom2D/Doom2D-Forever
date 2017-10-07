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
{$MODE OBJFPC}
{$INCLUDE libjit_opts.inc}
unit libjit_types;

interface

uses
  libjit;

(*const*) var jit_opcodes: packed array [0..JIT_OP_NUM_OPCODES-1] of jit_opcode_info_t; libraryLibJITVar;

(*
 * Pre-defined type descriptors.
 *)
(*const*) var jit_type_void: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sbyte: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_ubyte: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_short: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_ushort: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_int: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_uint: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_nint: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_nuint: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_long: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_ulong: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_float32: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_float64: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_nfloat: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_void_ptr: jit_type_t; libraryLibJITVar;

(*
 * Type descriptors for the system "char", "int", "long", etc types.
 * These are defined to one of the above values.
 *)
(*const*) var jit_type_sys_bool: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_char: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_schar: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_uchar: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_short: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_ushort: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_int: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_uint: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_long: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_ulong: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_longlong: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_ulonglong: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_float: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_double: jit_type_t; libraryLibJITVar;
(*const*) var jit_type_sys_long_double: jit_type_t; libraryLibJITVar;


implementation


end.
