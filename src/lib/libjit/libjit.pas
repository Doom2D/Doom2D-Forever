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
unit libjit;

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$MODE DELPHI}
{$PACKRECORDS C}
{$MACRO ON}

{$Z4} // Force four-byte enums

interface

const
  {$IFDEF MSWINDOWS}
    //{$LINKLIB libjit.dll.a}
    LIBJIT_LIBNAME = 'libjit-0.dll';
    {$DEFINE libraryLibJITDecl := cdecl}
    {$DEFINE libraryLibJITImp := cdecl; external LIBJIT_LIBNAME}
  {$ELSE}
    LIBJIT_LIBNAME = 'jit';
    {$DEFINE libraryLibJITDecl := cdecl}
    {$DEFINE libraryLibJITImp := cdecl; external LIBJIT_LIBNAME}
  {$ENDIF}


type
  jit_sbyte = ShortInt; pjit_sbyte = ^jit_sbyte;
  jit_ubyte = Byte; pjit_ubyte = ^jit_ubyte;
  jit_short = SmallInt; pjit_short = ^jit_short;
  jit_ushort = Word; pjit_ushort = ^jit_ushort;
  jit_int = LongInt; pjit_int = ^jit_int;
  jit_uint = LongWord; pjit_uint = ^jit_uint;
  // "native" types (should prolly be 8 bytes on 64-bit arch)
  //jit_nint = PtrInt; pjit_nint = ^jit_nint; // "native"
  //jit_nuint = PtrUInt; pjit_nuint = ^jit_nuint; // "native"
  jit_long = Int64; pjit_long = ^jit_long;
  jit_ulong = UInt64; pjit_ulong = ^jit_ulong;
  jit_float32 = Single; pjit_float32 = ^jit_float32;
  jit_float64 = Double; pjit_float64 = ^jit_float64;
  jit_nfloat = Extended; pjit_nfloat = ^jit_nfloat; //k8: seems to be the same for x86; i think that FPC's `Extended` is C's `long double`
  jit_ptr = Pointer; pjit_ptr = ^jit_ptr;
  {$IF DEFINED(CPU32)}
    {$DEFINE JIT_NATIVE_INT32}
    jit_nint = LongInt; pjit_nint = ^jit_nint; // "native"
    jit_nuint = LongWord; pjit_nuint = ^jit_nuint; // "native"
  {$ELSEIF DEFINED(CPU64)}
    {$UNDEF JIT_NATIVE_INT32}
    jit_nint = Int64; pjit_nint = ^jit_nint; // "native"
    jit_nuint = UInt64; pjit_nuint = ^jit_nuint; // "native"
  {$ELSE}
    {$ERROR unknown CPU bitness}
  {$ENDIF}

const
  jit_min_int   = jit_int(jit_int(1) shl (sizeof(jit_int)*8-1));
  jit_max_int   = jit_int(jit_int(not jit_min_int));
  jit_max_uint  = jit_uint(not (jit_uint(0)));
  jit_min_long  = jit_long(jit_long(1) shl (sizeof(jit_long)*8-1));
  jit_max_long  = jit_long(not jit_min_long);
  jit_max_ulong = jit_ulong(not (jit_ulong(0)));

(*
 * Opaque structure that represents a context.
 *)
type jit_context_t = Pointer;

(*
 * Opaque structure that represents a function.
 *)
type jit_function_t = Pointer;

(*
 * Opaque structure that represents a block.
 *)
type jit_block_t = Pointer;

(*
 * Opaque structure that represents an instruction.
 *)
type jit_insn_t = Pointer;

(*
 * Opaque structure that represents a value.
 *)
type jit_value_t = Pointer;
type pjit_value_t = ^jit_value_t;

(*
 * Opaque structure that represents a type descriptor.
 *)
type jit_type_t = Pointer;
type pjit_type_t = ^jit_type_t;

(*
 * Opaque type that represents an exception stack trace.
 *)
type jit_stack_trace_t = Pointer;

(*
 * Block label identifier.
 *)
type jit_label_t = jit_nuint;
type pjit_label_t = ^jit_label_t;

(*
 * Value that represents an undefined label.
 *)
const jit_label_undefined = jit_label_t(not (jit_uint(0)));

(*
 * Value that represents an undefined offset.
 *)
const JIT_NO_OFFSET = jit_uint(not (jit_uint(0)));

(*
 * Function that is used to free user-supplied metadata.
 *)
type jit_meta_free_func = procedure (data: Pointer); libraryLibJITDecl;

(*
 * Function that is used to compile a function on demand.
 * Returns zero if the compilation process failed for some reason.
 *)
type jit_on_demand_func = function (func: jit_function_t): Integer; libraryLibJITDecl; //FIXME: should this be `jit_int` instead for x64?

(*
 * Function that is used to control on demand compilation.
 * Typically, it should take care of the context locking and unlocking,
 * calling function's on demand compiler, and final compilation.
 *)
type jit_on_demand_driver_func = function (func: jit_function_t): Pointer; libraryLibJITDecl;


function jit_context_create (): jit_context_t; libraryLibJITDecl;
procedure jit_context_destroy (context: jit_context_t); libraryLibJITDecl;

procedure jit_context_build_start (context: jit_context_t); libraryLibJITDecl;
procedure jit_context_build_end (context: jit_context_t); libraryLibJITDecl;

procedure jit_context_set_on_demand_driver (context: jit_context_t; driver: jit_on_demand_driver_func); libraryLibJITDecl;

//moved down;procedure jit_context_set_memory_manager (context: jit_context_t; manager: jit_memory_manager_t); libraryLibJITDecl;

function jit_context_set_meta (context: jit_context_t; type_: Integer; data: Pointer; free_data: jit_meta_free_func): Integer; libraryLibJITDecl;
function jit_context_set_meta_numeric (context: jit_context_t; type_: Integer; data: jit_nuint): Integer; libraryLibJITDecl;
function jit_context_get_meta (context: jit_context_t; type_: Integer): Pointer; libraryLibJITDecl;
function jit_context_get_meta_numeric (context: jit_context_t; type_: Integer): jit_nuint; libraryLibJITDecl;
procedure jit_context_free_meta (context: jit_context_t; type_: Integer); libraryLibJITDecl;

(*
 * Standard meta values for builtin configurable options.
 *)
const JIT_OPTION_CACHE_LIMIT = 10000;
const JIT_OPTION_CACHE_PAGE_SIZE = 10001;
const JIT_OPTION_PRE_COMPILE = 10002;
const JIT_OPTION_DONT_FOLD = 10003;
const JIT_OPTION_POSITION_INDEPENDENT = 10004;
const JIT_OPTION_CACHE_MAX_PAGE_FACTOR = 10005;


(*
 * Prototype for closure functions.
 *)
type jit_closure_func = procedure (signature: jit_type_t; result_: Pointer; args: PPointer; user_data: Pointer); libraryLibJITDecl;

(*
 * Opaque type for accessing vararg parameters on closures.
 *)
type jit_closure_va_list_t = Pointer;

(*
 * External function declarations.
 *)
procedure jit_apply (signature: jit_type_t; func: Pointer; args: PPointer; num_fixed_args: LongWord; return_value: Pointer); libraryLibJITDecl;
procedure jit_apply_raw (signature: jit_type_t; func: Pointer; args: Pointer; return_value: Pointer); libraryLibJITDecl;
function jit_raw_supported (signature: jit_type_t): Integer; libraryLibJITDecl;

function jit_closure_create (context: jit_context_t; signature: jit_type_t; func: jit_closure_func; user_data: Pointer): Pointer; libraryLibJITDecl;

function jit_closure_va_get_nint (va: jit_closure_va_list_t): jit_nint; libraryLibJITDecl;
function jit_closure_va_get_nuint (va: jit_closure_va_list_t): jit_nuint; libraryLibJITDecl;
function jit_closure_va_get_long (va: jit_closure_va_list_t): jit_long; libraryLibJITDecl;
function jit_closure_va_get_ulong (va: jit_closure_va_list_t): jit_ulong; libraryLibJITDecl;
function jit_closure_va_get_float32 (va: jit_closure_va_list_t): jit_float32; libraryLibJITDecl;
function jit_closure_va_get_float64 (va: jit_closure_va_list_t): jit_float64; libraryLibJITDecl;
function jit_closure_va_get_nfloat (va: jit_closure_va_list_t): jit_nfloat; libraryLibJITDecl;
function jit_closure_va_get_ptr (va: jit_closure_va_list_t): Pointer; libraryLibJITDecl;
procedure jit_closure_va_get_struct (va: jit_closure_va_list_t; buf: Pointer; type_: jit_type_t); libraryLibJITDecl;

function jit_block_get_function (block: jit_block_t): jit_function_t; libraryLibJITDecl;
function jit_block_get_context (block: jit_block_t): jit_context_t; libraryLibJITDecl;
function jit_block_get_label (block: jit_block_t): jit_label_t; libraryLibJITDecl;
function jit_block_get_next_label (block: jit_block_t; label_: jit_label_t): jit_label_t; libraryLibJITDecl;
function jit_block_next (func: jit_function_t; previous: jit_block_t): jit_block_t; libraryLibJITDecl;
function jit_block_previous (func: jit_function_t; previous: jit_block_t): jit_block_t; libraryLibJITDecl;
function jit_block_from_label (func: jit_function_t; label_: jit_label_t): jit_block_t; libraryLibJITDecl;
function jit_block_set_meta (block: jit_block_t; type_: Integer; data: Pointer; free_data: jit_meta_free_func): Integer; libraryLibJITDecl;
function jit_block_get_meta (block: jit_block_t; type_: Integer): Pointer; libraryLibJITDecl;
procedure jit_block_free_meta (block: jit_block_t; type_: Integer); libraryLibJITDecl;
function jit_block_is_reachable (block: jit_block_t): Integer; libraryLibJITDecl;
function jit_block_ends_in_dead (block: jit_block_t): Integer; libraryLibJITDecl;
function jit_block_current_is_dead (func: jit_function_t): Integer; libraryLibJITDecl;

type jit_debugger_t = Pointer;
type jit_debugger_thread_id_t = jit_nint;
type jit_debugger_breakpoint_id_t = jit_nint;

type jit_debugger_event_t = record
  type_: Integer;
  thread: jit_debugger_thread_id_t;
  function_: jit_function_t;
  data1: jit_nint;
  data2: jit_nint;
  id: jit_debugger_breakpoint_id_t;
  trace: jit_stack_trace_t;
end;
type pjit_debugger_event_t = ^jit_debugger_event_t;

const JIT_DEBUGGER_TYPE_QUIT = 0;
const JIT_DEBUGGER_TYPE_HARD_BREAKPOINT = 1;
const JIT_DEBUGGER_TYPE_SOFT_BREAKPOINT = 2;
const JIT_DEBUGGER_TYPE_USER_BREAKPOINT = 3;
const JIT_DEBUGGER_TYPE_ATTACH_THREAD = 4;
const JIT_DEBUGGER_TYPE_DETACH_THREAD = 5;

type jit_debugger_breakpoint_info = record
  flags: Integer;
  thread: jit_debugger_thread_id_t;
  function_: jit_function_t;
  data1: jit_nint;
  data2: jit_nint;
end;
type jit_debugger_breakpoint_info_t = ^jit_debugger_breakpoint_info;

const JIT_DEBUGGER_FLAG_THREAD = (1 shl 0);
const JIT_DEBUGGER_FLAG_FUNCTION = (1 shl 1);
const JIT_DEBUGGER_FLAG_DATA1 = (1 shl 2);
const JIT_DEBUGGER_FLAG_DATA2 = (1 shl 3);

const JIT_DEBUGGER_DATA1_FIRST = 10000;
const JIT_DEBUGGER_DATA1_LINE = 10000;
const JIT_DEBUGGER_DATA1_ENTER = 10001;
const JIT_DEBUGGER_DATA1_LEAVE = 10002;
const JIT_DEBUGGER_DATA1_THROW = 10003;

type jit_debugger_hook_func = procedure (func: jit_function_t; data1: jit_nint; data2: jit_nint); libraryLibJITDecl;

function jit_debugging_possible (): Integer; libraryLibJITDecl;

function jit_debugger_create (context: jit_context_t): jit_debugger_t; libraryLibJITDecl;
procedure jit_debugger_destroy (dbg: jit_debugger_t); libraryLibJITDecl;

function jit_debugger_get_context (dbg: jit_debugger_t): jit_context_t; libraryLibJITDecl;
function jit_debugger_from_context (context: jit_context_t): jit_debugger_t; libraryLibJITDecl;

function jit_debugger_get_self (dbg: jit_debugger_t): jit_debugger_thread_id_t; libraryLibJITDecl;
function jit_debugger_get_thread (dbg: jit_debugger_t; const native_thread: Pointer): jit_debugger_thread_id_t; libraryLibJITDecl;
function jit_debugger_get_native_thread (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t; native_thread: Pointer): Integer; libraryLibJITDecl;
procedure jit_debugger_set_breakable (dbg: jit_debugger_t; const native_thread: Pointer; flag: Integer); libraryLibJITDecl;

procedure jit_debugger_attach_self (dbg: jit_debugger_t; stop_immediately: Integer); libraryLibJITDecl;
procedure jit_debugger_detach_self (dbg: jit_debugger_t); libraryLibJITDecl;

function jit_debugger_wait_event (dbg: jit_debugger_t; event: pjit_debugger_event_t; timeout: jit_int): Integer; libraryLibJITDecl;

function jit_debugger_add_breakpoint (dbg: jit_debugger_t; info: jit_debugger_breakpoint_info_t): jit_debugger_breakpoint_id_t; libraryLibJITDecl;
procedure jit_debugger_remove_breakpoint (dbg: jit_debugger_t; id: jit_debugger_breakpoint_id_t); libraryLibJITDecl;
procedure jit_debugger_remove_all_breakpoints (dbg: jit_debugger_t); libraryLibJITDecl;

function jit_debugger_is_alive (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t): Integer; libraryLibJITDecl;
function jit_debugger_is_running (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t): Integer; libraryLibJITDecl;
procedure jit_debugger_run (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t); libraryLibJITDecl;
procedure jit_debugger_step (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t); libraryLibJITDecl;
procedure jit_debugger_next (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t); libraryLibJITDecl;
procedure jit_debugger_finish (dbg: jit_debugger_t; thread: jit_debugger_thread_id_t); libraryLibJITDecl;

procedure jit_debugger_break (dbg: jit_debugger_t); libraryLibJITDecl;

procedure jit_debugger_quit (dbg: jit_debugger_t); libraryLibJITDecl;

function jit_debugger_set_hook (context: jit_context_t; hook: jit_debugger_hook_func): jit_debugger_hook_func; libraryLibJITDecl;


(*
 * Opaque types that represent a loaded ELF binary in read or write mode.
 *)
type jit_readelf_t = Pointer;
type jit_writeelf_t = Pointer;
type pjit_readelf_t = ^jit_readelf_t;

(*
 * Flags for "jit_readelf_open".
 *)
const JIT_READELF_FLAG_FORCE = (1 shl 0); (* Force file to load *)
const JIT_READELF_FLAG_DEBUG = (1 shl 1); (* Print debugging information *)

(*
 * Result codes from "jit_readelf_open".
 *)
const JIT_READELF_OK = 0; (* File was opened successfully *)
const JIT_READELF_CANNOT_OPEN = 1; (* Could not open the file *)
const JIT_READELF_NOT_ELF = 2; (* Not an ELF-format binary *)
const JIT_READELF_WRONG_ARCH = 3; (* Wrong architecture for local system *)
const JIT_READELF_BAD_FORMAT = 4; (* ELF file, but badly formatted *)
const JIT_READELF_MEMORY = 5; (* Insufficient memory to load the file *)

(*
 * External function declarations.
 *)
function jit_readelf_open (readelf: pjit_readelf_t; const filename: PAnsiChar; flags: Integer): Integer; libraryLibJITDecl;
procedure jit_readelf_close (readelf: jit_readelf_t); libraryLibJITDecl;
function jit_readelf_get_name (readelf: jit_readelf_t): PAnsiChar; libraryLibJITDecl; // const
function jit_readelf_get_symbol (readelf: jit_readelf_t; const name: PAnsiChar): Pointer; libraryLibJITDecl;
function jit_readelf_get_section (readelf: jit_readelf_t; const name: PAnsiChar; size: pjit_nuint): Pointer; libraryLibJITDecl;
function jit_readelf_get_section_by_type (readelf: jit_readelf_t; type_: jit_int; size: pjit_nuint): Pointer; libraryLibJITDecl;
function jit_readelf_map_vaddr (readelf: jit_readelf_t; vaddr: jit_nuint): Pointer; libraryLibJITDecl;
function jit_readelf_num_needed (readelf: jit_readelf_t): LongWord; libraryLibJITDecl;
function jit_readelf_get_needed (readelf: jit_readelf_t; index: LongWord): PAnsiChar; libraryLibJITDecl; // const
procedure jit_readelf_add_to_context (readelf: jit_readelf_t; context: jit_context_t); libraryLibJITDecl;
function jit_readelf_resolve_all (context: jit_context_t; print_failures: Integer): Integer; libraryLibJITDecl;
function jit_readelf_register_symbol (context: jit_context_t; const name: PAnsiChar; value: Pointer; after: Integer): Integer; libraryLibJITDecl;

function jit_writeelf_create (const library_name: PAnsiChar): jit_writeelf_t; libraryLibJITDecl;
procedure jit_writeelf_destroy (writeelf: jit_writeelf_t); libraryLibJITDecl;
function jit_writeelf_write (writeelf: jit_writeelf_t; const filename: PAnsiChar): Integer; libraryLibJITDecl;
function jit_writeelf_add_function (writeelf: jit_writeelf_t; func: jit_function_t; const name: PAnsiChar): Integer; libraryLibJITDecl;
function jit_writeelf_add_needed (writeelf: jit_writeelf_t; const library_name: PAnsiChar): Integer; libraryLibJITDecl;
function jit_writeelf_write_section (writeelf: jit_writeelf_t; const name: PAnsiChar; type_: jit_int; const buf: Pointer; len: LongWord; discardable: Integer): Integer; libraryLibJITDecl;

(*
 * Builtin exception type codes, and result values for intrinsic functions.
 *)
const JIT_RESULT_OK = (1);
const JIT_RESULT_OVERFLOW = (0);
const JIT_RESULT_ARITHMETIC = (-1);
const JIT_RESULT_DIVISION_BY_ZERO = (-2);
const JIT_RESULT_COMPILE_ERROR = (-3);
const JIT_RESULT_OUT_OF_MEMORY = (-4);
const JIT_RESULT_NULL_REFERENCE = (-5);
const JIT_RESULT_NULL_FUNCTION = (-6);
const JIT_RESULT_CALLED_NESTED = (-7);
const JIT_RESULT_OUT_OF_BOUNDS = (-8);
const JIT_RESULT_UNDEFINED_LABEL = (-9);
const JIT_RESULT_MEMORY_FULL = (-10000);

(*
 * Exception handling function for builtin exceptions.
 *)
type jit_exception_func = function (exception_type: Integer): Pointer; libraryLibJITDecl;

(*
 * External function declarations.
 *)
function jit_exception_get_last (): Pointer; libraryLibJITDecl;
function jit_exception_get_last_and_clear (): Pointer; libraryLibJITDecl;
procedure jit_exception_set_last (object_: Pointer); libraryLibJITDecl;
procedure jit_exception_clear_last (); libraryLibJITDecl;
procedure jit_exception_throw (object_: Pointer); libraryLibJITDecl;
procedure jit_exception_builtin (exception_type: Integer); libraryLibJITDecl;
function jit_exception_set_handler (handler: jit_exception_func): jit_exception_func; libraryLibJITDecl;
function jit_exception_get_handler (): jit_exception_func; libraryLibJITDecl;
function jit_exception_get_stack_trace (): jit_stack_trace_t; libraryLibJITDecl;
function jit_stack_trace_get_size (trace: jit_stack_trace_t): LongWord; libraryLibJITDecl;
function jit_stack_trace_get_function (context: jit_context_t; trace: jit_stack_trace_t; posn: LongWord): jit_function_t; libraryLibJITDecl;
function jit_stack_trace_get_pc (trace: jit_stack_trace_t; posn: LongWord): Pointer; libraryLibJITDecl;
function jit_stack_trace_get_offset (context: jit_context_t; trace: jit_stack_trace_t; posn: LongWord): LongWord; libraryLibJITDecl;
procedure jit_stack_trace_free (trace: jit_stack_trace_t); libraryLibJITDecl;


(* Optimization levels *)
const JIT_OPTLEVEL_NONE = 0;
const JIT_OPTLEVEL_NORMAL = 1;

function jit_function_create (context: jit_context_t; signature: jit_type_t): jit_function_t; libraryLibJITDecl;
function jit_function_create_nested (context: jit_context_t; signature: jit_type_t; parent: jit_function_t): jit_function_t; libraryLibJITDecl;
procedure jit_function_abandon (func: jit_function_t); libraryLibJITDecl;
function jit_function_get_context (func: jit_function_t): jit_context_t; libraryLibJITDecl;
function jit_function_get_signature (func: jit_function_t): jit_type_t; libraryLibJITDecl;
function jit_function_set_meta (func: jit_function_t; type_: Integer; data: Pointer; free_data: jit_meta_free_func; build_only: Integer): Integer; libraryLibJITDecl;
function jit_function_get_meta (func: jit_function_t; type_: Integer): Pointer; libraryLibJITDecl;
procedure jit_function_free_meta (func: jit_function_t; type_: Integer); libraryLibJITDecl;
function jit_function_next (context: jit_context_t; prev: jit_function_t): jit_function_t; libraryLibJITDecl;
function jit_function_previous (context: jit_context_t; prev: jit_function_t): jit_function_t; libraryLibJITDecl;
function jit_function_get_entry (func: jit_function_t): jit_block_t; libraryLibJITDecl;
function jit_function_get_current (func: jit_function_t): jit_block_t; libraryLibJITDecl;
function jit_function_get_nested_parent (func: jit_function_t): jit_function_t; libraryLibJITDecl;
function jit_function_compile (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_function_is_compiled (func: jit_function_t): Integer; libraryLibJITDecl;
procedure jit_function_set_recompilable (func: jit_function_t); libraryLibJITDecl;
procedure jit_function_clear_recompilable (func: jit_function_t); libraryLibJITDecl;
function jit_function_is_recompilable (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_function_compile_entry (func: jit_function_t; entry_point: PPointer): Integer; libraryLibJITDecl;
procedure jit_function_setup_entry (func: jit_function_t; entry_point: Pointer); libraryLibJITDecl;
function jit_function_to_closure (func: jit_function_t): Pointer; libraryLibJITDecl;
function jit_function_from_closure (context: jit_context_t; closure: Pointer): jit_function_t; libraryLibJITDecl;
function jit_function_from_pc (context: jit_context_t; pc: Pointer; handler: PPointer): jit_function_t; libraryLibJITDecl;
function jit_function_to_vtable_pointer (func: jit_function_t): Pointer; libraryLibJITDecl;
function jit_function_from_vtable_pointer (context: jit_context_t; vtable_pointer: Pointer): jit_function_t; libraryLibJITDecl;
procedure jit_function_set_on_demand_compiler (func: jit_function_t; on_demand: jit_on_demand_func); libraryLibJITDecl;
function jit_function_get_on_demand_compiler (func: jit_function_t): jit_on_demand_func; libraryLibJITDecl;
function jit_function_apply (func: jit_function_t; args: PPointer; return_area: Pointer): Integer; libraryLibJITDecl;
function jit_function_apply_vararg (func: jit_function_t; signature: jit_type_t; args: PPointer; return_area: Pointer): Integer; libraryLibJITDecl;
procedure jit_function_set_optimization_level (func: jit_function_t; level: LongWord); libraryLibJITDecl;
function jit_function_get_optimization_level (func: jit_function_t): LongWord; libraryLibJITDecl;
function jit_function_get_max_optimization_level (): LongWord; libraryLibJITDecl;
function jit_function_reserve_label (func: jit_function_t): jit_label_t; libraryLibJITDecl;
function jit_function_labels_equal (func: jit_function_t; label_: jit_label_t; label2: jit_label_t): Integer; libraryLibJITDecl;


procedure jit_init (); libraryLibJITDecl;

function jit_uses_interpreter (): Integer; libraryLibJITDecl;

function jit_supports_threads (): Integer; libraryLibJITDecl;

function jit_supports_virtual_memory (): Integer; libraryLibJITDecl;

function jit_supports_closures (): Integer; libraryLibJITDecl;

function jit_get_closure_size (): LongWord; libraryLibJITDecl;
function jit_get_closure_alignment (): LongWord; libraryLibJITDecl;
function jit_get_trampoline_size (): LongWord; libraryLibJITDecl;
function jit_get_trampoline_alignment (): LongWord; libraryLibJITDecl;


(*
 * Descriptor for an intrinsic function.
 *)
type jit_intrinsic_descr_t = record
  return_type: jit_type_t;
  ptr_result_type: jit_type_t;
  arg1_type: jit_type_t;
  arg2_type: jit_type_t;
end;
type pjit_intrinsic_descr_t = ^jit_intrinsic_descr_t;

(*
 * Structure for iterating over the instructions in a block.
 * This should be treated as opaque.
 *)
type jit_insn_iter_t = record
  block: jit_block_t;
  posn: Integer;
end;
type pjit_insn_iter_t = ^jit_insn_iter_t;

(*
 * Flags for "jit_insn_call" and friends.
 *)
const JIT_CALL_NOTHROW = (1 shl 0);
const JIT_CALL_NORETURN = (1 shl 1);
const JIT_CALL_TAIL = (1 shl 2);

function jit_insn_get_opcode (insn: jit_insn_t): Integer; libraryLibJITDecl;
function jit_insn_get_dest (insn: jit_insn_t): jit_value_t; libraryLibJITDecl;
function jit_insn_get_value1 (insn: jit_insn_t): jit_value_t; libraryLibJITDecl;
function jit_insn_get_value2 (insn: jit_insn_t): jit_value_t; libraryLibJITDecl;
function jit_insn_get_label (insn: jit_insn_t): jit_label_t; libraryLibJITDecl;
function jit_insn_get_function (insn: jit_insn_t): jit_function_t; libraryLibJITDecl;
function jit_insn_get_native (insn: jit_insn_t): Pointer; libraryLibJITDecl;
function jit_insn_get_name (insn: jit_insn_t): PAnsiChar; libraryLibJITDecl; // const
function jit_insn_get_signature (insn: jit_insn_t): jit_type_t; libraryLibJITDecl;
function jit_insn_dest_is_value (insn: jit_insn_t): Integer; libraryLibJITDecl;

function jit_insn_label (func: jit_function_t; label_: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_label_tight (func: jit_function_t; label_: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_new_block (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_insn_load (func: jit_function_t; value: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_dup (func: jit_function_t; value: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_store (func: jit_function_t; dest: jit_value_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_load_relative (func: jit_function_t; value: jit_value_t; offset: jit_nint; type_: jit_type_t): jit_value_t; libraryLibJITDecl;
function jit_insn_store_relative (func: jit_function_t; dest: jit_value_t; offset: jit_nint; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_add_relative (func: jit_function_t; value: jit_value_t; offset: jit_nint): jit_value_t; libraryLibJITDecl;
function jit_insn_load_elem (func: jit_function_t; base_addr: jit_value_t; index: jit_value_t; elem_type: jit_type_t): jit_value_t; libraryLibJITDecl;
function jit_insn_load_elem_address (func: jit_function_t; base_addr: jit_value_t; index: jit_value_t; elem_type: jit_type_t): jit_value_t; libraryLibJITDecl;
function jit_insn_store_elem (func: jit_function_t; base_addr: jit_value_t; index: jit_value_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_check_null (func: jit_function_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_nop (func: jit_function_t): Integer; libraryLibJITDecl;

function jit_insn_add (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_add_ovf (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sub (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sub_ovf (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_mul (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_mul_ovf (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_div (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_rem (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_rem_ieee (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_neg (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_and (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_or (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_xor (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_not (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_shl (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_shr (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_ushr (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sshr (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_eq (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_ne (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_lt (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_le (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_gt (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_ge (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_cmpl (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_cmpg (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_to_bool (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_to_not_bool (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_acos (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_asin (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_atan (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_atan2 (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_ceil (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_cos (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_cosh (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_exp (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_floor (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_log (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_log10 (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_pow (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_rint (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_round (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sin (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sinh (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sqrt (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_tan (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_tanh (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_trunc (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_is_nan (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_is_finite (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_is_inf (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_abs (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_min (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_max (func: jit_function_t; value1: jit_value_t; value2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_sign (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_branch (func: jit_function_t; label_: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_branch_if (func: jit_function_t; value: jit_value_t; label_: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_branch_if_not (func: jit_function_t; value: jit_value_t; label_: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_jump_table (func: jit_function_t; value: jit_value_t; labels: pjit_label_t; num_labels: LongWord): Integer; libraryLibJITDecl;
function jit_insn_address_of (func: jit_function_t; value1: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_address_of_label (func: jit_function_t; label_: pjit_label_t): jit_value_t; libraryLibJITDecl;
function jit_insn_convert (func: jit_function_t; value: jit_value_t; type_: jit_type_t; overflow_check: Integer): jit_value_t; libraryLibJITDecl;

function jit_insn_call (func: jit_function_t; const name: PAnsiChar; jit_func: jit_function_t; signature: jit_type_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jit_insn_call_indirect (func: jit_function_t; value: jit_value_t; signature: jit_type_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jit_insn_call_indirect_vtable (func: jit_function_t; value: jit_value_t; signature: jit_type_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jit_insn_call_native (func: jit_function_t; const name: PAnsiChar; native_func: Pointer; signature: jit_type_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jit_insn_call_intrinsic (func: jit_function_t; const name: PAnsiChar; intrinsic_func: Pointer; const descriptor: pjit_intrinsic_descr_t; arg1: jit_value_t; arg2: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_incoming_reg (func: jit_function_t; value: jit_value_t; reg: Integer): Integer; libraryLibJITDecl;
function jit_insn_incoming_frame_posn (func: jit_function_t; value: jit_value_t; frame_offset: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_outgoing_reg (func: jit_function_t; value: jit_value_t; reg: Integer): Integer; libraryLibJITDecl;
function jit_insn_outgoing_frame_posn (func: jit_function_t; value: jit_value_t; frame_offset: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_return_reg (func: jit_function_t; value: jit_value_t; reg: Integer): Integer; libraryLibJITDecl;
function jit_insn_setup_for_nested (func: jit_function_t; nested_level: Integer; reg: Integer): Integer; libraryLibJITDecl;
function jit_insn_flush_struct (func: jit_function_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_import (func: jit_function_t; value: jit_value_t): jit_value_t; libraryLibJITDecl;
function jit_insn_push (func: jit_function_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_push_ptr (func: jit_function_t; value: jit_value_t; type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_insn_set_param (func: jit_function_t; value: jit_value_t; offset: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_set_param_ptr (func: jit_function_t; value: jit_value_t; type_: jit_type_t; offset: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_push_return_area_ptr (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_insn_pop_stack (func: jit_function_t; num_items: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_defer_pop_stack (func: jit_function_t; num_items: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_flush_defer_pop (func: jit_function_t; num_items: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_return (func: jit_function_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_return_ptr (func: jit_function_t; value: jit_value_t; type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_insn_default_return (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_insn_throw (func: jit_function_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_get_call_stack (func: jit_function_t): jit_value_t; libraryLibJITDecl;

function jit_insn_thrown_exception (func: jit_function_t): jit_value_t; libraryLibJITDecl;
function jit_insn_uses_catcher (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_insn_start_catcher (func: jit_function_t): jit_value_t; libraryLibJITDecl;
function jit_insn_branch_if_pc_not_in_range (func: jit_function_t; start_label: jit_label_t; end_label: jit_label_t; label_: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_rethrow_unhandled (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_insn_start_finally (func: jit_function_t; finally_label: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_return_from_finally (func: jit_function_t): Integer; libraryLibJITDecl;
function jit_insn_call_finally (func: jit_function_t; finally_label: pjit_label_t): Integer; libraryLibJITDecl;
function jit_insn_start_filter (func: jit_function_t; label_: pjit_label_t; type_: jit_type_t): jit_value_t; libraryLibJITDecl;
function jit_insn_return_from_filter (func: jit_function_t; value: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_call_filter (func: jit_function_t; label_: pjit_label_t; value: jit_value_t; type_: jit_type_t): jit_value_t; libraryLibJITDecl;

function jit_insn_memcpy (func: jit_function_t; dest: jit_value_t; src: jit_value_t; size: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_memmove (func: jit_function_t; dest: jit_value_t; src: jit_value_t; size: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_memset (func: jit_function_t; dest: jit_value_t; value: jit_value_t; size: jit_value_t): Integer; libraryLibJITDecl;
function jit_insn_alloca (func: jit_function_t; size: jit_value_t): jit_value_t; libraryLibJITDecl;

function jit_insn_move_blocks_to_end (func: jit_function_t; from_label: jit_label_t; to_label: jit_label_t): Integer; libraryLibJITDecl;
function jit_insn_move_blocks_to_start (func: jit_function_t; from_label: jit_label_t; to_label: jit_label_t): Integer; libraryLibJITDecl;

function jit_insn_mark_offset (func: jit_function_t; offset: jit_int): Integer; libraryLibJITDecl;
function jit_insn_mark_breakpoint (func: jit_function_t; data1: jit_nint; data2: jit_nint): Integer; libraryLibJITDecl;
function jit_insn_mark_breakpoint_variable (func: jit_function_t; data1: jit_value_t; data2: jit_value_t): Integer; libraryLibJITDecl;

procedure jit_insn_iter_init (iter: pjit_insn_iter_t; block: jit_block_t); libraryLibJITDecl;
procedure jit_insn_iter_init_last (iter: pjit_insn_iter_t; block: jit_block_t); libraryLibJITDecl;
function jit_insn_iter_next (iter: pjit_insn_iter_t): jit_insn_t; libraryLibJITDecl;
function jit_insn_iter_previous (iter: pjit_insn_iter_t): jit_insn_t; libraryLibJITDecl;


(*
 * Perform operations on signed 32-bit integers.
 *)
function jit_int_add (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_sub (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_mul (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_div (result_: pjit_int; value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_rem (result_: pjit_int; value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_add_ovf (result_: pjit_int; value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_sub_ovf (result_: pjit_int; value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_mul_ovf (result_: pjit_int; value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_neg (value1: jit_int): jit_int; libraryLibJITDecl;
function jit_int_and (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_or (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_xor (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_not (value1: jit_int): jit_int; libraryLibJITDecl;
function jit_int_shl (value1: jit_int; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_int_shr (value1: jit_int; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_int_eq (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_ne (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_lt (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_le (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_gt (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_ge (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_cmp (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_abs (value1: jit_int): jit_int; libraryLibJITDecl;
function jit_int_min (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_max (value1: jit_int; value2: jit_int): jit_int; libraryLibJITDecl;
function jit_int_sign (value1: jit_int): jit_int; libraryLibJITDecl;

(*
 * Perform operations on unsigned 32-bit integers.
 *)
function jit_uint_add (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_sub (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_mul (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_div (result_: pjit_uint; value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_rem (result_: pjit_uint; value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_add_ovf (result_: pjit_uint; value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_sub_ovf (result_: pjit_uint; value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_mul_ovf (result_: pjit_uint; value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_neg (value1: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_and (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_or (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_xor (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_not (value1: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_shl (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_shr (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_eq (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_ne (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_lt (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_le (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_gt (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_ge (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_cmp (value1: jit_uint; value2: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_min (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_max (value1: jit_uint; value2: jit_uint): jit_uint; libraryLibJITDecl;

(*
 * Perform operations on signed 64-bit integers.
 *)
function jit_long_add (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_sub (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_mul (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_div (result_: pjit_long; value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_rem (result_: pjit_long; value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_add_ovf (result_: pjit_long; value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_sub_ovf (result_: pjit_long; value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_mul_ovf (result_: pjit_long; value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_neg (value1: jit_long): jit_long; libraryLibJITDecl;
function jit_long_and (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_or (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_xor (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_not (value1: jit_long): jit_long; libraryLibJITDecl;
function jit_long_shl (value1: jit_long; value2: jit_uint): jit_long; libraryLibJITDecl;
function jit_long_shr (value1: jit_long; value2: jit_uint): jit_long; libraryLibJITDecl;
function jit_long_eq (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_ne (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_lt (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_le (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_gt (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_ge (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_cmp (value1: jit_long; value2: jit_long): jit_int; libraryLibJITDecl;
function jit_long_abs (value1: jit_long): jit_long; libraryLibJITDecl;
function jit_long_min (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_max (value1: jit_long; value2: jit_long): jit_long; libraryLibJITDecl;
function jit_long_sign (value1: jit_long): jit_int; libraryLibJITDecl;

(*
 * Perform operations on unsigned 64-bit integers.
 *)
function jit_ulong_add (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_sub (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_mul (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_div (result_: pjit_ulong; value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_rem (result_: pjit_ulong; value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_add_ovf (result_: pjit_ulong; value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_sub_ovf (result_: pjit_ulong; value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_mul_ovf (result_: pjit_ulong; value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_neg (value1: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_and (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_or (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_xor (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_not (value1: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_shl (value1: jit_ulong; value2: jit_uint): jit_ulong; libraryLibJITDecl;
function jit_ulong_shr (value1: jit_ulong; value2: jit_uint): jit_ulong; libraryLibJITDecl;
function jit_ulong_eq (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_ne (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_lt (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_le (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_gt (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_ge (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_cmp (value1: jit_ulong; value2: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_min (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;
function jit_ulong_max (value1: jit_ulong; value2: jit_ulong): jit_ulong; libraryLibJITDecl;

(*
 * Perform operations on 32-bit floating-point values.
 *)
function jit_float32_add (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_sub (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_mul (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_div (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_rem (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_ieee_rem (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_neg (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_eq (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_ne (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_lt (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_le (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_gt (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_ge (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_cmpl (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_cmpg (value1: jit_float32; value2: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_acos (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_asin (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_atan (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_atan2 (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_ceil (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_cos (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_cosh (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_exp (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_floor (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_log (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_log10 (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_pow (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_rint (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_round (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_sin (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_sinh (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_sqrt (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_tan (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_tanh (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_trunc (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_is_finite (value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_is_nan (value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_is_inf (value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_abs (value1: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_min (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_max (value1: jit_float32; value2: jit_float32): jit_float32; libraryLibJITDecl;
function jit_float32_sign (value1: jit_float32): jit_int; libraryLibJITDecl;

(*
 * Perform operations on 64-bit floating-point values.
 *)
function jit_float64_add (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_sub (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_mul (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_div (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_rem (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_ieee_rem (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_neg (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_eq (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_ne (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_lt (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_le (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_gt (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_ge (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_cmpl (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_cmpg (value1: jit_float64; value2: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_acos (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_asin (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_atan (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_atan2 (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_ceil (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_cos (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_cosh (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_exp (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_floor (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_log (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_log10 (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_pow (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_rint (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_round (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_sin (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_sinh (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_sqrt (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_tan (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_tanh (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_trunc (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_is_finite (value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_is_nan (value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_is_inf (value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_abs (value1: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_min (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_max (value1: jit_float64; value2: jit_float64): jit_float64; libraryLibJITDecl;
function jit_float64_sign (value1: jit_float64): jit_int; libraryLibJITDecl;

(*
 * Perform operations on native floating-point values.
 *)
function jit_nfloat_add (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_sub (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_mul (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_div (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_rem (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_ieee_rem (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_neg (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_eq (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_ne (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_lt (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_le (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_gt (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_ge (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_cmpl (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_cmpg (value1: jit_nfloat; value2: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_acos (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_asin (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_atan (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_atan2 (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_ceil (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_cos (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_cosh (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_exp (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_floor (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_log (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_log10 (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_pow (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_rint (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_round (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_sin (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_sinh (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_sqrt (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_tan (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_tanh (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_trunc (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_is_finite (value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_is_nan (value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_is_inf (value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_abs (value1: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_min (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_max (value1: jit_nfloat; value2: jit_nfloat): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_sign (value1: jit_nfloat): jit_int; libraryLibJITDecl;

(*
 * Convert between integer types.
 *)
function jit_int_to_sbyte (value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_ubyte (value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_short (value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_ushort (value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_int (value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_uint (value: jit_int): jit_uint; libraryLibJITDecl;
function jit_int_to_long (value: jit_int): jit_long; libraryLibJITDecl;
function jit_int_to_ulong (value: jit_int): jit_ulong; libraryLibJITDecl;
function jit_uint_to_int (value: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_to_uint (value: jit_uint): jit_uint; libraryLibJITDecl;
function jit_uint_to_long (value: jit_uint): jit_long; libraryLibJITDecl;
function jit_uint_to_ulong (value: jit_uint): jit_ulong; libraryLibJITDecl;
function jit_long_to_int (value: jit_long): jit_int; libraryLibJITDecl;
function jit_long_to_uint (value: jit_long): jit_uint; libraryLibJITDecl;
function jit_long_to_long (value: jit_long): jit_long; libraryLibJITDecl;
function jit_long_to_ulong (value: jit_long): jit_ulong; libraryLibJITDecl;
function jit_ulong_to_int (value: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_to_uint (value: jit_ulong): jit_uint; libraryLibJITDecl;
function jit_ulong_to_long (value: jit_ulong): jit_long; libraryLibJITDecl;
function jit_ulong_to_ulong (value: jit_ulong): jit_ulong; libraryLibJITDecl;

(*
 * Convert between integer types with overflow detection.
 *)
function jit_int_to_sbyte_ovf (result_: pjit_int; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_ubyte_ovf (result_: pjit_int; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_short_ovf (result_: pjit_int; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_ushort_ovf (result_: pjit_int; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_int_ovf (result_: pjit_int; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_uint_ovf (result_: pjit_uint; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_long_ovf (result_: pjit_long; value: jit_int): jit_int; libraryLibJITDecl;
function jit_int_to_ulong_ovf (result_: pjit_ulong; value: jit_int): jit_int; libraryLibJITDecl;
function jit_uint_to_int_ovf (result_: pjit_int; value: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_to_uint_ovf (result_: pjit_uint; value: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_to_long_ovf (result_: pjit_long; value: jit_uint): jit_int; libraryLibJITDecl;
function jit_uint_to_ulong_ovf (result_: pjit_ulong; value: jit_uint): jit_int; libraryLibJITDecl;
function jit_long_to_int_ovf (result_: pjit_int; value: jit_long): jit_int; libraryLibJITDecl;
function jit_long_to_uint_ovf (result_: pjit_uint; value: jit_long): jit_int; libraryLibJITDecl;
function jit_long_to_long_ovf (result_: pjit_long; value: jit_long): jit_int; libraryLibJITDecl;
function jit_long_to_ulong_ovf (result_: pjit_ulong; value: jit_long): jit_int; libraryLibJITDecl;
function jit_ulong_to_int_ovf (result_: pjit_int; value: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_to_uint_ovf (result_: pjit_uint; value: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_to_long_ovf (result_: pjit_long; value: jit_ulong): jit_int; libraryLibJITDecl;
function jit_ulong_to_ulong_ovf (result_: pjit_ulong; value: jit_ulong): jit_int; libraryLibJITDecl;

(*
 * Convert a 32-bit floating-point value into various integer types.
 *)
function jit_float32_to_int (value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_to_uint (value: jit_float32): jit_uint; libraryLibJITDecl;
function jit_float32_to_long (value: jit_float32): jit_long; libraryLibJITDecl;
function jit_float32_to_ulong (value: jit_float32): jit_ulong; libraryLibJITDecl;

(*
 * Convert a 32-bit floating-point value into various integer types,
 * with overflow detection.
 *)
function jit_float32_to_int_ovf (result_: pjit_int; value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_to_uint_ovf (result_: pjit_uint; value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_to_long_ovf (result_: pjit_long; value: jit_float32): jit_int; libraryLibJITDecl;
function jit_float32_to_ulong_ovf (result_: pjit_ulong; value: jit_float32): jit_int; libraryLibJITDecl;

(*
 * Convert a 64-bit floating-point value into various integer types.
 *)
function jit_float64_to_int (value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_to_uint (value: jit_float64): jit_uint; libraryLibJITDecl;
function jit_float64_to_long (value: jit_float64): jit_long; libraryLibJITDecl;
function jit_float64_to_ulong (value: jit_float64): jit_ulong; libraryLibJITDecl;

(*
 * Convert a 64-bit floating-point value into various integer types,
 * with overflow detection.
 *)
function jit_float64_to_int_ovf (result_: pjit_int; value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_to_uint_ovf (result_: pjit_uint; value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_to_long_ovf (result_: pjit_long; value: jit_float64): jit_int; libraryLibJITDecl;
function jit_float64_to_ulong_ovf (result_: pjit_ulong; value: jit_float64): jit_int; libraryLibJITDecl;

(*
 * Convert a native floating-point value into various integer types.
 *)
function jit_nfloat_to_int (value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_to_uint (value: jit_nfloat): jit_uint; libraryLibJITDecl;
function jit_nfloat_to_long (value: jit_nfloat): jit_long; libraryLibJITDecl;
function jit_nfloat_to_ulong (value: jit_nfloat): jit_ulong; libraryLibJITDecl;

(*
 * Convert a native floating-point value into various integer types,
 * with overflow detection.
 *)
function jit_nfloat_to_int_ovf (result_: pjit_int; value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_to_uint_ovf (result_: pjit_uint; value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_to_long_ovf (result_: pjit_long; value: jit_nfloat): jit_int; libraryLibJITDecl;
function jit_nfloat_to_ulong_ovf (result_: pjit_ulong; value: jit_nfloat): jit_int; libraryLibJITDecl;

(*
 * Convert integer types into floating-point values.
 *)
function jit_int_to_float32 (value: jit_int): jit_float32; libraryLibJITDecl;
function jit_int_to_float64 (value: jit_int): jit_float64; libraryLibJITDecl;
function jit_int_to_nfloat (value: jit_int): jit_nfloat; libraryLibJITDecl;
function jit_uint_to_float32 (value: jit_uint): jit_float32; libraryLibJITDecl;
function jit_uint_to_float64 (value: jit_uint): jit_float64; libraryLibJITDecl;
function jit_uint_to_nfloat (value: jit_uint): jit_nfloat; libraryLibJITDecl;
function jit_long_to_float32 (value: jit_long): jit_float32; libraryLibJITDecl;
function jit_long_to_float64 (value: jit_long): jit_float64; libraryLibJITDecl;
function jit_long_to_nfloat (value: jit_long): jit_nfloat; libraryLibJITDecl;
function jit_ulong_to_float32 (value: jit_ulong): jit_float32; libraryLibJITDecl;
function jit_ulong_to_float64 (value: jit_ulong): jit_float64; libraryLibJITDecl;
function jit_ulong_to_nfloat (value: jit_ulong): jit_nfloat; libraryLibJITDecl;

(*
 * Convert between floating-point types.
 *)
function jit_float32_to_float64 (value: jit_float32): jit_float64; libraryLibJITDecl;
function jit_float32_to_nfloat (value: jit_float32): jit_nfloat; libraryLibJITDecl;
function jit_float64_to_float32 (value: jit_float64): jit_float32; libraryLibJITDecl;
function jit_float64_to_nfloat (value: jit_float64): jit_nfloat; libraryLibJITDecl;
function jit_nfloat_to_float32 (value: jit_nfloat): jit_float32; libraryLibJITDecl;
function jit_nfloat_to_float64 (value: jit_nfloat): jit_float64; libraryLibJITDecl;


type jit_meta_t = Pointer;
type pjit_meta_t = ^jit_meta_t;

function jit_meta_set (list: pjit_meta_t; type_: Integer; data: Pointer; free_data: jit_meta_free_func; pool_owner: jit_function_t): Integer; libraryLibJITDecl;
function jit_meta_get (list: jit_meta_t; type_: Integer): Pointer; libraryLibJITDecl;
procedure jit_meta_free (list: pjit_meta_t; type_: Integer); libraryLibJITDecl;
procedure jit_meta_destroy (list: pjit_meta_t); libraryLibJITDecl;


(*
 * Opaque types that describe object model elements.
 *)
//struct jit_objmodel {}
type jitom_class_t = Pointer;
type pjitom_class_t = ^jitom_class_t;
type jitom_field_t = Pointer;
type pjitom_field_t = ^jitom_field_t;
type jitom_method_t = Pointer;
type pjitom_method_t = ^jitom_method_t;

(*
 * Internal structure of an object model handler.
 *)
type jit_objmodel_t = ^jit_objmodel;
     jit_objmodel = record
  (*
   * Size of this structure, for versioning.
   *)
  size: LongWord;

  (*
   * Reserved fields that can be used by the handler to store its state.
   *)
  reserved0: Pointer;
  reserved1: Pointer;
  reserved2: Pointer;
  reserved3: Pointer;

  (*
   * Operations on object models.
   *)
  destroy_model: procedure (model: jit_objmodel_t); libraryLibJITDecl;
  get_class_by_name: function (model: jit_objmodel_t; const name: PAnsiChar): jitom_class_t; libraryLibJITDecl;

  (*
   * Operations on object model classes.
   *)
  class_get_name: function (model: jit_objmodel_t; klass: jitom_class_t): PAnsiChar; libraryLibJITDecl;
  class_get_modifiers: function (model: jit_objmodel_t; klass: jitom_class_t): Integer; libraryLibJITDecl;
  class_get_type: function (model: jit_objmodel_t; klass: jitom_class_t): jit_type_t; libraryLibJITDecl;
  class_get_value_type: function (model: jit_objmodel_t; klass: jitom_class_t): jit_type_t; libraryLibJITDecl;
  class_get_primary_super: function (model: jit_objmodel_t; klass: jitom_class_t): jitom_class_t; libraryLibJITDecl;
  class_get_all_supers: function (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_class_t; libraryLibJITDecl;
  class_get_interfaces: function (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_class_t; libraryLibJITDecl;
  class_get_fields: function (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_field_t; libraryLibJITDecl;
  class_get_methods: function (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_method_t; libraryLibJITDecl;
  class_new: function (model: jit_objmodel_t; klass: jitom_class_t; ctor: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
  class_new_value: function (model: jit_objmodel_t; klass: jitom_class_t; ctor: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
  class_delete: function (model: jit_objmodel_t; klass: jitom_class_t; obj_value: jit_value_t): Integer; libraryLibJITDecl;
  class_add_ref: function (model: jit_objmodel_t; klass: jitom_class_t; obj_value: jit_value_t): Integer; libraryLibJITDecl;

  (*
   * Operations on object model fields.
   *)
  field_get_name: function (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t): PAnsiChar; libraryLibJITDecl;
  field_get_type: function (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t): jit_type_t; libraryLibJITDecl;
  field_get_modifiers: function (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t): Integer; libraryLibJITDecl;
  field_load: function (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t; func: jit_function_t; obj_value: jit_value_t): jit_value_t; libraryLibJITDecl;
  field_load_address: function (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t; func: jit_function_t; obj_value: jit_value_t): jit_value_t; libraryLibJITDecl;
  field_store: function (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t; func: jit_function_t; obj_value: jit_value_t; value: jit_value_t): Integer; libraryLibJITDecl;

  (*
   * Operations on object model methods.
   *)
  method_get_name: function (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t): PAnsiChar; libraryLibJITDecl;
  method_get_type: function (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t): jit_type_t; libraryLibJITDecl;
  method_get_modifiers: function (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t): Integer; libraryLibJITDecl;
  method_invoke: function (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
  method_invoke_virtual: function (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
end;


(*
 * Modifier flags that describe an item's properties.
 *)
const JITOM_MODIFIER_ACCESS_MASK = $0007;
const JITOM_MODIFIER_PUBLIC = $0000;
const JITOM_MODIFIER_PRIVATE = $0001;
const JITOM_MODIFIER_PROTECTED = $0002;
const JITOM_MODIFIER_PACKAGE = $0003;
const JITOM_MODIFIER_PACKAGE_OR_PROTECTED = $0004;
const JITOM_MODIFIER_PACKAGE_AND_PROTECTED = $0005;
const JITOM_MODIFIER_OTHER1 = $0006;
const JITOM_MODIFIER_OTHER2 = $0007;
const JITOM_MODIFIER_STATIC = $0008;
const JITOM_MODIFIER_VIRTUAL = $0010;
const JITOM_MODIFIER_NEW_SLOT = $0020;
const JITOM_MODIFIER_ABSTRACT = $0040;
const JITOM_MODIFIER_LITERAL = $0080;
const JITOM_MODIFIER_CTOR = $0100;
const JITOM_MODIFIER_STATIC_CTOR = $0200;
const JITOM_MODIFIER_DTOR = $0400;
const JITOM_MODIFIER_INTERFACE = $0800;
const JITOM_MODIFIER_VALUE = $1000;
const JITOM_MODIFIER_FINAL = $2000;
const JITOM_MODIFIER_DELETE = $4000;
const JITOM_MODIFIER_REFERENCE_COUNTED = $8000;

(*
 * Type tags that are used to mark instances of object model classes.
 *)
const JITOM_TYPETAG_CLASS = 11000; (* Object reference *)
const JITOM_TYPETAG_VALUE = 11001; (* Inline stack value *)

(*
 * Operations on object models.
 *)
procedure jitom_destroy_model (model: jit_objmodel_t); libraryLibJITDecl;
function jitom_get_class_by_name (model: jit_objmodel_t; const name: PAnsiChar): jitom_class_t; libraryLibJITDecl;

(*
 * Operations on object model classes.
 *)
function jitom_class_get_name (model: jit_objmodel_t; klass: jitom_class_t): PAnsiChar; libraryLibJITDecl;
function jitom_class_get_modifiers (model: jit_objmodel_t; klass: jitom_class_t): Integer; libraryLibJITDecl;
function jitom_class_get_type (model: jit_objmodel_t; klass: jitom_class_t): jit_type_t; libraryLibJITDecl;
function jitom_class_get_value_type (model: jit_objmodel_t; klass: jitom_class_t): jit_type_t; libraryLibJITDecl;
function jitom_class_get_primary_super (model: jit_objmodel_t; klass: jitom_class_t): jitom_class_t; libraryLibJITDecl;
function jitom_class_get_all_supers (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_class_t; libraryLibJITDecl;
function jitom_class_get_interfaces (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_class_t; libraryLibJITDecl;
function jitom_class_get_fields (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_field_t; libraryLibJITDecl;
function jitom_class_get_methods (model: jit_objmodel_t; klass: jitom_class_t; num: PLongWord): pjitom_method_t; libraryLibJITDecl;
function jitom_class_new (model: jit_objmodel_t; klass: jitom_class_t; ctor: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jitom_class_new_value (model: jit_objmodel_t; klass: jitom_class_t; ctor: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jitom_class_delete (model: jit_objmodel_t; klass: jitom_class_t; obj_value: jit_value_t): Integer; libraryLibJITDecl;
function jitom_class_add_ref (model: jit_objmodel_t; klass: jitom_class_t; obj_value: jit_value_t): Integer; libraryLibJITDecl;

(*
 * Operations on object model fields.
 *)
function jitom_field_get_name (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t): PAnsiChar; libraryLibJITDecl; // const
function jitom_field_get_type (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t): jit_type_t; libraryLibJITDecl;
function jitom_field_get_modifiers (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t): Integer; libraryLibJITDecl;
function jitom_field_load (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t; func: jit_function_t; obj_value: jit_value_t): jit_value_t; libraryLibJITDecl;
function jitom_field_load_address (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t; func: jit_function_t; obj_value: jit_value_t): jit_value_t; libraryLibJITDecl;
function jitom_field_store (model: jit_objmodel_t; klass: jitom_class_t; field: jitom_field_t; func: jit_function_t; obj_value: jit_value_t; value: jit_value_t): Integer; libraryLibJITDecl;

(*
 * Operations on object model methods.
 *)
function jitom_method_get_name (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t): PAnsiChar; libraryLibJITDecl; // const
function jitom_method_get_type (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t): jit_type_t; libraryLibJITDecl;
function jitom_method_get_modifiers (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t): Integer; libraryLibJITDecl;
function jitom_method_invoke (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;
function jitom_method_invoke_virtual (model: jit_objmodel_t; klass: jitom_class_t; method: jitom_method_t; func: jit_function_t; args: pjit_value_t; num_args: LongWord; flags: Integer): jit_value_t; libraryLibJITDecl;

(*
 * Manipulate types that represent objects and inline values.
 *)
function jitom_type_tag_as_class (type_: jit_type_t; model: jit_objmodel_t; klass: jitom_class_t; incref: Integer): jit_type_t; libraryLibJITDecl;
function jitom_type_tag_as_value (type_: jit_type_t; model: jit_objmodel_t; klass: jitom_class_t; incref: Integer): jit_type_t; libraryLibJITDecl;
function jitom_type_is_class (type_: jit_type_t): Integer; libraryLibJITDecl;
function jitom_type_is_value (type_: jit_type_t): Integer; libraryLibJITDecl;
function jitom_type_get_model (type_: jit_type_t): jit_objmodel_t; libraryLibJITDecl;
function jitom_type_get_class (type_: jit_type_t): jitom_class_t; libraryLibJITDecl;


const JIT_OP_NOP = $0000;
const JIT_OP_TRUNC_SBYTE = $0001;
const JIT_OP_TRUNC_UBYTE = $0002;
const JIT_OP_TRUNC_SHORT = $0003;
const JIT_OP_TRUNC_USHORT = $0004;
const JIT_OP_TRUNC_INT = $0005;
const JIT_OP_TRUNC_UINT = $0006;
const JIT_OP_CHECK_SBYTE = $0007;
const JIT_OP_CHECK_UBYTE = $0008;
const JIT_OP_CHECK_SHORT = $0009;
const JIT_OP_CHECK_USHORT = $000A;
const JIT_OP_CHECK_INT = $000B;
const JIT_OP_CHECK_UINT = $000C;
const JIT_OP_LOW_WORD = $000D;
const JIT_OP_EXPAND_INT = $000E;
const JIT_OP_EXPAND_UINT = $000F;
const JIT_OP_CHECK_LOW_WORD = $0010;
const JIT_OP_CHECK_SIGNED_LOW_WORD = $0011;
const JIT_OP_CHECK_LONG = $0012;
const JIT_OP_CHECK_ULONG = $0013;
const JIT_OP_FLOAT32_TO_INT = $0014;
const JIT_OP_FLOAT32_TO_UINT = $0015;
const JIT_OP_FLOAT32_TO_LONG = $0016;
const JIT_OP_FLOAT32_TO_ULONG = $0017;
const JIT_OP_CHECK_FLOAT32_TO_INT = $0018;
const JIT_OP_CHECK_FLOAT32_TO_UINT = $0019;
const JIT_OP_CHECK_FLOAT32_TO_LONG = $001A;
const JIT_OP_CHECK_FLOAT32_TO_ULONG = $001B;
const JIT_OP_INT_TO_FLOAT32 = $001C;
const JIT_OP_UINT_TO_FLOAT32 = $001D;
const JIT_OP_LONG_TO_FLOAT32 = $001E;
const JIT_OP_ULONG_TO_FLOAT32 = $001F;
const JIT_OP_FLOAT32_TO_FLOAT64 = $0020;
const JIT_OP_FLOAT64_TO_INT = $0021;
const JIT_OP_FLOAT64_TO_UINT = $0022;
const JIT_OP_FLOAT64_TO_LONG = $0023;
const JIT_OP_FLOAT64_TO_ULONG = $0024;
const JIT_OP_CHECK_FLOAT64_TO_INT = $0025;
const JIT_OP_CHECK_FLOAT64_TO_UINT = $0026;
const JIT_OP_CHECK_FLOAT64_TO_LONG = $0027;
const JIT_OP_CHECK_FLOAT64_TO_ULONG = $0028;
const JIT_OP_INT_TO_FLOAT64 = $0029;
const JIT_OP_UINT_TO_FLOAT64 = $002A;
const JIT_OP_LONG_TO_FLOAT64 = $002B;
const JIT_OP_ULONG_TO_FLOAT64 = $002C;
const JIT_OP_FLOAT64_TO_FLOAT32 = $002D;
const JIT_OP_NFLOAT_TO_INT = $002E;
const JIT_OP_NFLOAT_TO_UINT = $002F;
const JIT_OP_NFLOAT_TO_LONG = $0030;
const JIT_OP_NFLOAT_TO_ULONG = $0031;
const JIT_OP_CHECK_NFLOAT_TO_INT = $0032;
const JIT_OP_CHECK_NFLOAT_TO_UINT = $0033;
const JIT_OP_CHECK_NFLOAT_TO_LONG = $0034;
const JIT_OP_CHECK_NFLOAT_TO_ULONG = $0035;
const JIT_OP_INT_TO_NFLOAT = $0036;
const JIT_OP_UINT_TO_NFLOAT = $0037;
const JIT_OP_LONG_TO_NFLOAT = $0038;
const JIT_OP_ULONG_TO_NFLOAT = $0039;
const JIT_OP_NFLOAT_TO_FLOAT32 = $003A;
const JIT_OP_NFLOAT_TO_FLOAT64 = $003B;
const JIT_OP_FLOAT32_TO_NFLOAT = $003C;
const JIT_OP_FLOAT64_TO_NFLOAT = $003D;
const JIT_OP_IADD = $003E;
const JIT_OP_IADD_OVF = $003F;
const JIT_OP_IADD_OVF_UN = $0040;
const JIT_OP_ISUB = $0041;
const JIT_OP_ISUB_OVF = $0042;
const JIT_OP_ISUB_OVF_UN = $0043;
const JIT_OP_IMUL = $0044;
const JIT_OP_IMUL_OVF = $0045;
const JIT_OP_IMUL_OVF_UN = $0046;
const JIT_OP_IDIV = $0047;
const JIT_OP_IDIV_UN = $0048;
const JIT_OP_IREM = $0049;
const JIT_OP_IREM_UN = $004A;
const JIT_OP_INEG = $004B;
const JIT_OP_LADD = $004C;
const JIT_OP_LADD_OVF = $004D;
const JIT_OP_LADD_OVF_UN = $004E;
const JIT_OP_LSUB = $004F;
const JIT_OP_LSUB_OVF = $0050;
const JIT_OP_LSUB_OVF_UN = $0051;
const JIT_OP_LMUL = $0052;
const JIT_OP_LMUL_OVF = $0053;
const JIT_OP_LMUL_OVF_UN = $0054;
const JIT_OP_LDIV = $0055;
const JIT_OP_LDIV_UN = $0056;
const JIT_OP_LREM = $0057;
const JIT_OP_LREM_UN = $0058;
const JIT_OP_LNEG = $0059;
const JIT_OP_FADD = $005A;
const JIT_OP_FSUB = $005B;
const JIT_OP_FMUL = $005C;
const JIT_OP_FDIV = $005D;
const JIT_OP_FREM = $005E;
const JIT_OP_FREM_IEEE = $005F;
const JIT_OP_FNEG = $0060;
const JIT_OP_DADD = $0061;
const JIT_OP_DSUB = $0062;
const JIT_OP_DMUL = $0063;
const JIT_OP_DDIV = $0064;
const JIT_OP_DREM = $0065;
const JIT_OP_DREM_IEEE = $0066;
const JIT_OP_DNEG = $0067;
const JIT_OP_NFADD = $0068;
const JIT_OP_NFSUB = $0069;
const JIT_OP_NFMUL = $006A;
const JIT_OP_NFDIV = $006B;
const JIT_OP_NFREM = $006C;
const JIT_OP_NFREM_IEEE = $006D;
const JIT_OP_NFNEG = $006E;
const JIT_OP_IAND = $006F;
const JIT_OP_IOR = $0070;
const JIT_OP_IXOR = $0071;
const JIT_OP_INOT = $0072;
const JIT_OP_ISHL = $0073;
const JIT_OP_ISHR = $0074;
const JIT_OP_ISHR_UN = $0075;
const JIT_OP_LAND = $0076;
const JIT_OP_LOR = $0077;
const JIT_OP_LXOR = $0078;
const JIT_OP_LNOT = $0079;
const JIT_OP_LSHL = $007A;
const JIT_OP_LSHR = $007B;
const JIT_OP_LSHR_UN = $007C;
const JIT_OP_BR = $007D;
const JIT_OP_BR_IFALSE = $007E;
const JIT_OP_BR_ITRUE = $007F;
const JIT_OP_BR_IEQ = $0080;
const JIT_OP_BR_INE = $0081;
const JIT_OP_BR_ILT = $0082;
const JIT_OP_BR_ILT_UN = $0083;
const JIT_OP_BR_ILE = $0084;
const JIT_OP_BR_ILE_UN = $0085;
const JIT_OP_BR_IGT = $0086;
const JIT_OP_BR_IGT_UN = $0087;
const JIT_OP_BR_IGE = $0088;
const JIT_OP_BR_IGE_UN = $0089;
const JIT_OP_BR_LFALSE = $008A;
const JIT_OP_BR_LTRUE = $008B;
const JIT_OP_BR_LEQ = $008C;
const JIT_OP_BR_LNE = $008D;
const JIT_OP_BR_LLT = $008E;
const JIT_OP_BR_LLT_UN = $008F;
const JIT_OP_BR_LLE = $0090;
const JIT_OP_BR_LLE_UN = $0091;
const JIT_OP_BR_LGT = $0092;
const JIT_OP_BR_LGT_UN = $0093;
const JIT_OP_BR_LGE = $0094;
const JIT_OP_BR_LGE_UN = $0095;
const JIT_OP_BR_FEQ = $0096;
const JIT_OP_BR_FNE = $0097;
const JIT_OP_BR_FLT = $0098;
const JIT_OP_BR_FLE = $0099;
const JIT_OP_BR_FGT = $009A;
const JIT_OP_BR_FGE = $009B;
const JIT_OP_BR_FLT_INV = $009C;
const JIT_OP_BR_FLE_INV = $009D;
const JIT_OP_BR_FGT_INV = $009E;
const JIT_OP_BR_FGE_INV = $009F;
const JIT_OP_BR_DEQ = $00A0;
const JIT_OP_BR_DNE = $00A1;
const JIT_OP_BR_DLT = $00A2;
const JIT_OP_BR_DLE = $00A3;
const JIT_OP_BR_DGT = $00A4;
const JIT_OP_BR_DGE = $00A5;
const JIT_OP_BR_DLT_INV = $00A6;
const JIT_OP_BR_DLE_INV = $00A7;
const JIT_OP_BR_DGT_INV = $00A8;
const JIT_OP_BR_DGE_INV = $00A9;
const JIT_OP_BR_NFEQ = $00AA;
const JIT_OP_BR_NFNE = $00AB;
const JIT_OP_BR_NFLT = $00AC;
const JIT_OP_BR_NFLE = $00AD;
const JIT_OP_BR_NFGT = $00AE;
const JIT_OP_BR_NFGE = $00AF;
const JIT_OP_BR_NFLT_INV = $00B0;
const JIT_OP_BR_NFLE_INV = $00B1;
const JIT_OP_BR_NFGT_INV = $00B2;
const JIT_OP_BR_NFGE_INV = $00B3;
const JIT_OP_ICMP = $00B4;
const JIT_OP_ICMP_UN = $00B5;
const JIT_OP_LCMP = $00B6;
const JIT_OP_LCMP_UN = $00B7;
const JIT_OP_FCMPL = $00B8;
const JIT_OP_FCMPG = $00B9;
const JIT_OP_DCMPL = $00BA;
const JIT_OP_DCMPG = $00BB;
const JIT_OP_NFCMPL = $00BC;
const JIT_OP_NFCMPG = $00BD;
const JIT_OP_IEQ = $00BE;
const JIT_OP_INE = $00BF;
const JIT_OP_ILT = $00C0;
const JIT_OP_ILT_UN = $00C1;
const JIT_OP_ILE = $00C2;
const JIT_OP_ILE_UN = $00C3;
const JIT_OP_IGT = $00C4;
const JIT_OP_IGT_UN = $00C5;
const JIT_OP_IGE = $00C6;
const JIT_OP_IGE_UN = $00C7;
const JIT_OP_LEQ = $00C8;
const JIT_OP_LNE = $00C9;
const JIT_OP_LLT = $00CA;
const JIT_OP_LLT_UN = $00CB;
const JIT_OP_LLE = $00CC;
const JIT_OP_LLE_UN = $00CD;
const JIT_OP_LGT = $00CE;
const JIT_OP_LGT_UN = $00CF;
const JIT_OP_LGE = $00D0;
const JIT_OP_LGE_UN = $00D1;
const JIT_OP_FEQ = $00D2;
const JIT_OP_FNE = $00D3;
const JIT_OP_FLT = $00D4;
const JIT_OP_FLE = $00D5;
const JIT_OP_FGT = $00D6;
const JIT_OP_FGE = $00D7;
const JIT_OP_FLT_INV = $00D8;
const JIT_OP_FLE_INV = $00D9;
const JIT_OP_FGT_INV = $00DA;
const JIT_OP_FGE_INV = $00DB;
const JIT_OP_DEQ = $00DC;
const JIT_OP_DNE = $00DD;
const JIT_OP_DLT = $00DE;
const JIT_OP_DLE = $00DF;
const JIT_OP_DGT = $00E0;
const JIT_OP_DGE = $00E1;
const JIT_OP_DLT_INV = $00E2;
const JIT_OP_DLE_INV = $00E3;
const JIT_OP_DGT_INV = $00E4;
const JIT_OP_DGE_INV = $00E5;
const JIT_OP_NFEQ = $00E6;
const JIT_OP_NFNE = $00E7;
const JIT_OP_NFLT = $00E8;
const JIT_OP_NFLE = $00E9;
const JIT_OP_NFGT = $00EA;
const JIT_OP_NFGE = $00EB;
const JIT_OP_NFLT_INV = $00EC;
const JIT_OP_NFLE_INV = $00ED;
const JIT_OP_NFGT_INV = $00EE;
const JIT_OP_NFGE_INV = $00EF;
const JIT_OP_IS_FNAN = $00F0;
const JIT_OP_IS_FINF = $00F1;
const JIT_OP_IS_FFINITE = $00F2;
const JIT_OP_IS_DNAN = $00F3;
const JIT_OP_IS_DINF = $00F4;
const JIT_OP_IS_DFINITE = $00F5;
const JIT_OP_IS_NFNAN = $00F6;
const JIT_OP_IS_NFINF = $00F7;
const JIT_OP_IS_NFFINITE = $00F8;
const JIT_OP_FACOS = $00F9;
const JIT_OP_FASIN = $00FA;
const JIT_OP_FATAN = $00FB;
const JIT_OP_FATAN2 = $00FC;
const JIT_OP_FCEIL = $00FD;
const JIT_OP_FCOS = $00FE;
const JIT_OP_FCOSH = $00FF;
const JIT_OP_FEXP = $0100;
const JIT_OP_FFLOOR = $0101;
const JIT_OP_FLOG = $0102;
const JIT_OP_FLOG10 = $0103;
const JIT_OP_FPOW = $0104;
const JIT_OP_FRINT = $0105;
const JIT_OP_FROUND = $0106;
const JIT_OP_FSIN = $0107;
const JIT_OP_FSINH = $0108;
const JIT_OP_FSQRT = $0109;
const JIT_OP_FTAN = $010A;
const JIT_OP_FTANH = $010B;
const JIT_OP_FTRUNC = $010C;
const JIT_OP_DACOS = $010D;
const JIT_OP_DASIN = $010E;
const JIT_OP_DATAN = $010F;
const JIT_OP_DATAN2 = $0110;
const JIT_OP_DCEIL = $0111;
const JIT_OP_DCOS = $0112;
const JIT_OP_DCOSH = $0113;
const JIT_OP_DEXP = $0114;
const JIT_OP_DFLOOR = $0115;
const JIT_OP_DLOG = $0116;
const JIT_OP_DLOG10 = $0117;
const JIT_OP_DPOW = $0118;
const JIT_OP_DRINT = $0119;
const JIT_OP_DROUND = $011A;
const JIT_OP_DSIN = $011B;
const JIT_OP_DSINH = $011C;
const JIT_OP_DSQRT = $011D;
const JIT_OP_DTAN = $011E;
const JIT_OP_DTANH = $011F;
const JIT_OP_DTRUNC = $0120;
const JIT_OP_NFACOS = $0121;
const JIT_OP_NFASIN = $0122;
const JIT_OP_NFATAN = $0123;
const JIT_OP_NFATAN2 = $0124;
const JIT_OP_NFCEIL = $0125;
const JIT_OP_NFCOS = $0126;
const JIT_OP_NFCOSH = $0127;
const JIT_OP_NFEXP = $0128;
const JIT_OP_NFFLOOR = $0129;
const JIT_OP_NFLOG = $012A;
const JIT_OP_NFLOG10 = $012B;
const JIT_OP_NFPOW = $012C;
const JIT_OP_NFRINT = $012D;
const JIT_OP_NFROUND = $012E;
const JIT_OP_NFSIN = $012F;
const JIT_OP_NFSINH = $0130;
const JIT_OP_NFSQRT = $0131;
const JIT_OP_NFTAN = $0132;
const JIT_OP_NFTANH = $0133;
const JIT_OP_NFTRUNC = $0134;
const JIT_OP_IABS = $0135;
const JIT_OP_LABS = $0136;
const JIT_OP_FABS = $0137;
const JIT_OP_DABS = $0138;
const JIT_OP_NFABS = $0139;
const JIT_OP_IMIN = $013A;
const JIT_OP_IMIN_UN = $013B;
const JIT_OP_LMIN = $013C;
const JIT_OP_LMIN_UN = $013D;
const JIT_OP_FMIN = $013E;
const JIT_OP_DMIN = $013F;
const JIT_OP_NFMIN = $0140;
const JIT_OP_IMAX = $0141;
const JIT_OP_IMAX_UN = $0142;
const JIT_OP_LMAX = $0143;
const JIT_OP_LMAX_UN = $0144;
const JIT_OP_FMAX = $0145;
const JIT_OP_DMAX = $0146;
const JIT_OP_NFMAX = $0147;
const JIT_OP_ISIGN = $0148;
const JIT_OP_LSIGN = $0149;
const JIT_OP_FSIGN = $014A;
const JIT_OP_DSIGN = $014B;
const JIT_OP_NFSIGN = $014C;
const JIT_OP_CHECK_NULL = $014D;
const JIT_OP_CALL = $014E;
const JIT_OP_CALL_TAIL = $014F;
const JIT_OP_CALL_INDIRECT = $0150;
const JIT_OP_CALL_INDIRECT_TAIL = $0151;
const JIT_OP_CALL_VTABLE_PTR = $0152;
const JIT_OP_CALL_VTABLE_PTR_TAIL = $0153;
const JIT_OP_CALL_EXTERNAL = $0154;
const JIT_OP_CALL_EXTERNAL_TAIL = $0155;
const JIT_OP_RETURN = $0156;
const JIT_OP_RETURN_INT = $0157;
const JIT_OP_RETURN_LONG = $0158;
const JIT_OP_RETURN_FLOAT32 = $0159;
const JIT_OP_RETURN_FLOAT64 = $015A;
const JIT_OP_RETURN_NFLOAT = $015B;
const JIT_OP_RETURN_SMALL_STRUCT = $015C;
const JIT_OP_SETUP_FOR_NESTED = $015D;
const JIT_OP_SETUP_FOR_SIBLING = $015E;
const JIT_OP_IMPORT = $015F;
const JIT_OP_THROW = $0160;
const JIT_OP_RETHROW = $0161;
const JIT_OP_LOAD_PC = $0162;
const JIT_OP_LOAD_EXCEPTION_PC = $0163;
const JIT_OP_ENTER_FINALLY = $0164;
const JIT_OP_LEAVE_FINALLY = $0165;
const JIT_OP_CALL_FINALLY = $0166;
const JIT_OP_ENTER_FILTER = $0167;
const JIT_OP_LEAVE_FILTER = $0168;
const JIT_OP_CALL_FILTER = $0169;
const JIT_OP_CALL_FILTER_RETURN = $016A;
const JIT_OP_ADDRESS_OF_LABEL = $016B;
const JIT_OP_COPY_LOAD_SBYTE = $016C;
const JIT_OP_COPY_LOAD_UBYTE = $016D;
const JIT_OP_COPY_LOAD_SHORT = $016E;
const JIT_OP_COPY_LOAD_USHORT = $016F;
const JIT_OP_COPY_INT = $0170;
const JIT_OP_COPY_LONG = $0171;
const JIT_OP_COPY_FLOAT32 = $0172;
const JIT_OP_COPY_FLOAT64 = $0173;
const JIT_OP_COPY_NFLOAT = $0174;
const JIT_OP_COPY_STRUCT = $0175;
const JIT_OP_COPY_STORE_BYTE = $0176;
const JIT_OP_COPY_STORE_SHORT = $0177;
const JIT_OP_ADDRESS_OF = $0178;
const JIT_OP_INCOMING_REG = $0179;
const JIT_OP_INCOMING_FRAME_POSN = $017A;
const JIT_OP_OUTGOING_REG = $017B;
const JIT_OP_OUTGOING_FRAME_POSN = $017C;
const JIT_OP_RETURN_REG = $017D;
const JIT_OP_PUSH_INT = $017E;
const JIT_OP_PUSH_LONG = $017F;
const JIT_OP_PUSH_FLOAT32 = $0180;
const JIT_OP_PUSH_FLOAT64 = $0181;
const JIT_OP_PUSH_NFLOAT = $0182;
const JIT_OP_PUSH_STRUCT = $0183;
const JIT_OP_POP_STACK = $0184;
const JIT_OP_FLUSH_SMALL_STRUCT = $0185;
const JIT_OP_SET_PARAM_INT = $0186;
const JIT_OP_SET_PARAM_LONG = $0187;
const JIT_OP_SET_PARAM_FLOAT32 = $0188;
const JIT_OP_SET_PARAM_FLOAT64 = $0189;
const JIT_OP_SET_PARAM_NFLOAT = $018A;
const JIT_OP_SET_PARAM_STRUCT = $018B;
const JIT_OP_PUSH_RETURN_AREA_PTR = $018C;
const JIT_OP_LOAD_RELATIVE_SBYTE = $018D;
const JIT_OP_LOAD_RELATIVE_UBYTE = $018E;
const JIT_OP_LOAD_RELATIVE_SHORT = $018F;
const JIT_OP_LOAD_RELATIVE_USHORT = $0190;
const JIT_OP_LOAD_RELATIVE_INT = $0191;
const JIT_OP_LOAD_RELATIVE_LONG = $0192;
const JIT_OP_LOAD_RELATIVE_FLOAT32 = $0193;
const JIT_OP_LOAD_RELATIVE_FLOAT64 = $0194;
const JIT_OP_LOAD_RELATIVE_NFLOAT = $0195;
const JIT_OP_LOAD_RELATIVE_STRUCT = $0196;
const JIT_OP_STORE_RELATIVE_BYTE = $0197;
const JIT_OP_STORE_RELATIVE_SHORT = $0198;
const JIT_OP_STORE_RELATIVE_INT = $0199;
const JIT_OP_STORE_RELATIVE_LONG = $019A;
const JIT_OP_STORE_RELATIVE_FLOAT32 = $019B;
const JIT_OP_STORE_RELATIVE_FLOAT64 = $019C;
const JIT_OP_STORE_RELATIVE_NFLOAT = $019D;
const JIT_OP_STORE_RELATIVE_STRUCT = $019E;
const JIT_OP_ADD_RELATIVE = $019F;
const JIT_OP_LOAD_ELEMENT_SBYTE = $01A0;
const JIT_OP_LOAD_ELEMENT_UBYTE = $01A1;
const JIT_OP_LOAD_ELEMENT_SHORT = $01A2;
const JIT_OP_LOAD_ELEMENT_USHORT = $01A3;
const JIT_OP_LOAD_ELEMENT_INT = $01A4;
const JIT_OP_LOAD_ELEMENT_LONG = $01A5;
const JIT_OP_LOAD_ELEMENT_FLOAT32 = $01A6;
const JIT_OP_LOAD_ELEMENT_FLOAT64 = $01A7;
const JIT_OP_LOAD_ELEMENT_NFLOAT = $01A8;
const JIT_OP_STORE_ELEMENT_BYTE = $01A9;
const JIT_OP_STORE_ELEMENT_SHORT = $01AA;
const JIT_OP_STORE_ELEMENT_INT = $01AB;
const JIT_OP_STORE_ELEMENT_LONG = $01AC;
const JIT_OP_STORE_ELEMENT_FLOAT32 = $01AD;
const JIT_OP_STORE_ELEMENT_FLOAT64 = $01AE;
const JIT_OP_STORE_ELEMENT_NFLOAT = $01AF;
const JIT_OP_MEMCPY = $01B0;
const JIT_OP_MEMMOVE = $01B1;
const JIT_OP_MEMSET = $01B2;
const JIT_OP_ALLOCA = $01B3;
const JIT_OP_MARK_OFFSET = $01B4;
const JIT_OP_MARK_BREAKPOINT = $01B5;
const JIT_OP_JUMP_TABLE = $01B6;
const JIT_OP_NUM_OPCODES = $01B7;

(*
 * Opcode information.
 *)
type jit_opcode_info_t = record
  name: PAnsiChar; // const
  flags: Integer;
end;
const JIT_OPCODE_DEST_MASK = $0000000F;
const JIT_OPCODE_DEST_EMPTY = $00000000;
const JIT_OPCODE_DEST_INT = $00000001;
const JIT_OPCODE_DEST_LONG = $00000002;
const JIT_OPCODE_DEST_FLOAT32 = $00000003;
const JIT_OPCODE_DEST_FLOAT64 = $00000004;
const JIT_OPCODE_DEST_NFLOAT = $00000005;
const JIT_OPCODE_DEST_ANY = $00000006;
const JIT_OPCODE_SRC1_MASK = $000000F0;
const JIT_OPCODE_SRC1_EMPTY = $00000000;
const JIT_OPCODE_SRC1_INT = $00000010;
const JIT_OPCODE_SRC1_LONG = $00000020;
const JIT_OPCODE_SRC1_FLOAT32 = $00000030;
const JIT_OPCODE_SRC1_FLOAT64 = $00000040;
const JIT_OPCODE_SRC1_NFLOAT = $00000050;
const JIT_OPCODE_SRC1_ANY = $00000060;
const JIT_OPCODE_SRC2_MASK = $00000F00;
const JIT_OPCODE_SRC2_EMPTY = $00000000;
const JIT_OPCODE_SRC2_INT = $00000100;
const JIT_OPCODE_SRC2_LONG = $00000200;
const JIT_OPCODE_SRC2_FLOAT32 = $00000300;
const JIT_OPCODE_SRC2_FLOAT64 = $00000400;
const JIT_OPCODE_SRC2_NFLOAT = $00000500;
const JIT_OPCODE_SRC2_ANY = $00000600;
const JIT_OPCODE_IS_BRANCH = $00001000;
const JIT_OPCODE_IS_CALL = $00002000;
const JIT_OPCODE_IS_CALL_EXTERNAL = $00004000;
const JIT_OPCODE_IS_REG = $00008000;
const JIT_OPCODE_IS_ADDROF_LABEL = $00010000;
const JIT_OPCODE_IS_JUMP_TABLE = $00020000;
const JIT_OPCODE_OPER_MASK = $01F00000;
const JIT_OPCODE_OPER_NONE = $00000000;
const JIT_OPCODE_OPER_ADD = $00100000;
const JIT_OPCODE_OPER_SUB = $00200000;
const JIT_OPCODE_OPER_MUL = $00300000;
const JIT_OPCODE_OPER_DIV = $00400000;
const JIT_OPCODE_OPER_REM = $00500000;
const JIT_OPCODE_OPER_NEG = $00600000;
const JIT_OPCODE_OPER_AND = $00700000;
const JIT_OPCODE_OPER_OR = $00800000;
const JIT_OPCODE_OPER_XOR = $00900000;
const JIT_OPCODE_OPER_NOT = $00A00000;
const JIT_OPCODE_OPER_EQ = $00B00000;
const JIT_OPCODE_OPER_NE = $00C00000;
const JIT_OPCODE_OPER_LT = $00D00000;
const JIT_OPCODE_OPER_LE = $00E00000;
const JIT_OPCODE_OPER_GT = $00F00000;
const JIT_OPCODE_OPER_GE = $01000000;
const JIT_OPCODE_OPER_SHL = $01100000;
const JIT_OPCODE_OPER_SHR = $01200000;
const JIT_OPCODE_OPER_SHR_UN = $01300000;
const JIT_OPCODE_OPER_COPY = $01400000;
const JIT_OPCODE_OPER_ADDRESS_OF = $01500000;
{$IF DEFINED(JIT_NATIVE_INT32)}
  const JIT_OPCODE_DEST_PTR = JIT_OPCODE_DEST_INT;
  const JIT_OPCODE_SRC1_PTR = JIT_OPCODE_SRC1_INT;
  const JIT_OPCODE_SRC2_PTR = JIT_OPCODE_SRC2_INT;
{$ELSE}
  const JIT_OPCODE_DEST_PTR = JIT_OPCODE_DEST_LONG;
  const JIT_OPCODE_SRC1_PTR = JIT_OPCODE_SRC1_LONG;
  const JIT_OPCODE_SRC2_PTR = JIT_OPCODE_SRC2_LONG;
{$ENDIF}
//k8: not yet
//(*const*) var jit_opcodes: packed array [0..JIT_OP_NUM_OPCODES-1] of jit_opcode_info_t; external name 'jit_opcodes';


(*
 * Some obsolete opcodes that have been removed because they are duplicates
 * of other opcodes.
 *)
const JIT_OP_FEQ_INV = JIT_OP_FEQ;
const JIT_OP_FNE_INV = JIT_OP_FNE;
const JIT_OP_DEQ_INV = JIT_OP_DEQ;
const JIT_OP_DNE_INV = JIT_OP_DNE;
const JIT_OP_NFEQ_INV = JIT_OP_NFEQ;
const JIT_OP_NFNE_INV = JIT_OP_NFNE;
const JIT_OP_BR_FEQ_INV = JIT_OP_BR_FEQ;
const JIT_OP_BR_FNE_INV = JIT_OP_BR_FNE;
const JIT_OP_BR_DEQ_INV = JIT_OP_BR_DEQ;
const JIT_OP_BR_DNE_INV = JIT_OP_BR_DNE;
const JIT_OP_BR_NFEQ_INV = JIT_OP_BR_NFEQ;
const JIT_OP_BR_NFNE_INV = JIT_OP_BR_NFNE;


{
(*
 * Pre-defined type descriptors.
 *)
(*const*) var jit_type_void_: jit_type_t; external name 'jit_type_void';
(*const*) var jit_type_sbyte: jit_type_t; external;
(*const*) var jit_type_ubyte: jit_type_t; external;
(*const*) var jit_type_short: jit_type_t; external;
(*const*) var jit_type_ushort: jit_type_t; external;
(*const*) var jit_type_int: jit_type_t; external;
(*const*) var jit_type_uint: jit_type_t; external;
(*const*) var jit_type_nint: jit_type_t; external;
(*const*) var jit_type_nuint: jit_type_t; external;
(*const*) var jit_type_long: jit_type_t; external;
(*const*) var jit_type_ulong: jit_type_t; external;
(*const*) var jit_type_float32: jit_type_t; external;
(*const*) var jit_type_float64: jit_type_t; external;
(*const*) var jit_type_nfloat: jit_type_t; external;
(*const*) var jit_type_void_ptr: jit_type_t; external;

(*
 * Type descriptors for the system "char", "int", "long", etc types.
 * These are defined to one of the above values.
 *)
(*const*) var jit_type_sys_bool: jit_type_t; external;
(*const*) var jit_type_sys_char: jit_type_t; external;
(*const*) var jit_type_sys_schar: jit_type_t; external;
(*const*) var jit_type_sys_uchar: jit_type_t; external;
(*const*) var jit_type_sys_short: jit_type_t; external;
(*const*) var jit_type_sys_ushort: jit_type_t; external;
(*const*) var jit_type_sys_int: jit_type_t; external;
(*const*) var jit_type_sys_uint: jit_type_t; external;
(*const*) var jit_type_sys_long: jit_type_t; external;
(*const*) var jit_type_sys_ulong: jit_type_t; external;
(*const*) var jit_type_sys_longlong: jit_type_t; external;
(*const*) var jit_type_sys_ulonglong: jit_type_t; external;
(*const*) var jit_type_sys_float: jit_type_t; external;
(*const*) var jit_type_sys_double: jit_type_t; external;
(*const*) var jit_type_sys_long_double: jit_type_t; external;
}


(*
 * Type kinds that may be returned by "jit_type_get_kind".
 *)
const TJIT_TYPE_INVALID = -1;
const TJIT_TYPE_VOID = 0;
const TJIT_TYPE_SBYTE = 1;
const TJIT_TYPE_UBYTE = 2;
const TJIT_TYPE_SHORT = 3;
const TJIT_TYPE_USHORT = 4;
const TJIT_TYPE_INT = 5;
const TJIT_TYPE_UINT = 6;
const TJIT_TYPE_NINT = 7;
const TJIT_TYPE_NUINT = 8;
const TJIT_TYPE_LONG = 9;
const TJIT_TYPE_ULONG = 10;
const TJIT_TYPE_FLOAT32 = 11;
const TJIT_TYPE_FLOAT64 = 12;
const TJIT_TYPE_NFLOAT = 13;
const TJIT_TYPE_MAX_PRIMITIVE = TJIT_TYPE_NFLOAT;
const TJIT_TYPE_STRUCT = 14;
const TJIT_TYPE_UNION = 15;
const TJIT_TYPE_SIGNATURE = 16;
const TJIT_TYPE_PTR = 17;
const TJIT_TYPE_FIRST_TAGGED = 32;

(*
 * Special tag types.
 *)
const JIT_TYPETAG_NAME = 10000;
const JIT_TYPETAG_STRUCT_NAME = 10001;
const JIT_TYPETAG_UNION_NAME = 10002;
const JIT_TYPETAG_ENUM_NAME = 10003;
const JIT_TYPETAG_CONST = 10004;
const JIT_TYPETAG_VOLATILE = 10005;
const JIT_TYPETAG_REFERENCE = 10006;
const JIT_TYPETAG_OUTPUT = 10007;
const JIT_TYPETAG_RESTRICT = 10008;
const JIT_TYPETAG_SYS_BOOL = 10009;
const JIT_TYPETAG_SYS_CHAR = 10010;
const JIT_TYPETAG_SYS_SCHAR = 10011;
const JIT_TYPETAG_SYS_UCHAR = 10012;
const JIT_TYPETAG_SYS_SHORT = 10013;
const JIT_TYPETAG_SYS_USHORT = 10014;
const JIT_TYPETAG_SYS_INT = 10015;
const JIT_TYPETAG_SYS_UINT = 10016;
const JIT_TYPETAG_SYS_LONG = 10017;
const JIT_TYPETAG_SYS_ULONG = 10018;
const JIT_TYPETAG_SYS_LONGLONG = 10019;
const JIT_TYPETAG_SYS_ULONGLONG = 10020;
const JIT_TYPETAG_SYS_FLOAT = 10021;
const JIT_TYPETAG_SYS_DOUBLE = 10022;
const JIT_TYPETAG_SYS_LONGDOUBLE = 10023;

(*
 * ABI types for function signatures.
 *)
type jit_abi_t = (
  jit_abi_cdecl,    (* Native C calling conventions *)
  jit_abi_vararg,   (* Native C with optional variable arguments *)
  jit_abi_stdcall,  (* Win32 STDCALL (same as cdecl if not Win32) *)
  jit_abi_fastcall  (* Win32 FASTCALL (same as cdecl if not Win32) *)
);

(*
 * External function declarations.
 *)
function jit_type_copy (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
procedure jit_type_free (type_: jit_type_t); libraryLibJITDecl;
function jit_type_create_struct (fields: pjit_type_t; num_fields: LongWord; incref: Integer): jit_type_t; libraryLibJITDecl;
function jit_type_create_union (fields: pjit_type_t; num_fields: LongWord; incref: Integer): jit_type_t; libraryLibJITDecl;
function jit_type_create_signature (abi: jit_abi_t; return_type: jit_type_t; params: pjit_type_t; num_params: LongWord; incref: Integer): jit_type_t; libraryLibJITDecl;
function jit_type_create_pointer (type_: jit_type_t; incref: Integer): jit_type_t; libraryLibJITDecl;
function jit_type_create_tagged (type_: jit_type_t; kind: Integer; data: Pointer; free_func: jit_meta_free_func; incref: Integer): jit_type_t; libraryLibJITDecl;
function jit_type_set_names (type_: jit_type_t; names: PPAnsiChar; num_names: LongWord): Integer; libraryLibJITDecl;
procedure jit_type_set_size_and_alignment (type_: jit_type_t; size: jit_nint; alignment: jit_nint); libraryLibJITDecl;
procedure jit_type_set_offset (type_: jit_type_t; field_index: LongWord; offset: jit_nuint); libraryLibJITDecl;
function jit_type_get_kind (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_get_size (type_: jit_type_t): jit_nuint; libraryLibJITDecl;
function jit_type_get_alignment (type_: jit_type_t): jit_nuint; libraryLibJITDecl;
function jit_type_num_fields (type_: jit_type_t): LongWord; libraryLibJITDecl;
function jit_type_get_field (type_: jit_type_t; field_index: LongWord): jit_type_t; libraryLibJITDecl;
function jit_type_get_offset (type_: jit_type_t; field_index: LongWord): jit_nuint; libraryLibJITDecl;
function jit_type_get_name (type_: jit_type_t; index: LongWord): PAnsiChar; libraryLibJITDecl; // const
const JIT_INVALID_NAME = LongWord(not ((LongWord(0))));
function jit_type_find_name (type_: jit_type_t; const name: PAnsiChar): LongWord; libraryLibJITDecl;
function jit_type_num_params (type_: jit_type_t): LongWord; libraryLibJITDecl;
function jit_type_get_return (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
function jit_type_get_param (type_: jit_type_t; param_index: LongWord): jit_type_t; libraryLibJITDecl;
function jit_type_get_abi (type_: jit_type_t): jit_abi_t; libraryLibJITDecl;
function jit_type_get_ref (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
function jit_type_get_tagged_type (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
procedure jit_type_set_tagged_type (type_: jit_type_t; underlying: jit_type_t; incref: Integer); libraryLibJITDecl;
function jit_type_get_tagged_kind (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_get_tagged_data (type_: jit_type_t): Pointer; libraryLibJITDecl;
procedure jit_type_set_tagged_data (type_: jit_type_t; data: Pointer; free_func: jit_meta_free_func); libraryLibJITDecl;
function jit_type_is_primitive (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_is_struct (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_is_union (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_is_signature (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_is_pointer (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_is_tagged (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_best_alignment (): jit_nuint; libraryLibJITDecl;
function jit_type_normalize (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
function jit_type_remove_tags (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
function jit_type_promote_int (type_: jit_type_t): jit_type_t; libraryLibJITDecl;
function jit_type_return_via_pointer (type_: jit_type_t): Integer; libraryLibJITDecl;
function jit_type_has_tag (type_: jit_type_t; kind: Integer): Integer; libraryLibJITDecl;


type jit_unwind_context_t = record
  frame: Pointer;
  cache: Pointer;
  context: jit_context_t;
(*k8: it isn't here in x86/x86_65/arm/generic
#ifdef _JIT_ARCH_UNWIND_DATA
  _JIT_ARCH_UNWIND_DATA
#endif
*)
end;
type pjit_unwind_context_t = ^jit_unwind_context_t;

function jit_unwind_init (unwind: pjit_unwind_context_t; context: jit_context_t): Integer; libraryLibJITDecl;
procedure jit_unwind_free (unwind: pjit_unwind_context_t); libraryLibJITDecl;

function jit_unwind_next (unwind: pjit_unwind_context_t): Integer; libraryLibJITDecl;
function jit_unwind_next_pc (unwind: pjit_unwind_context_t): Integer; libraryLibJITDecl;
function jit_unwind_get_pc (unwind: pjit_unwind_context_t): Pointer; libraryLibJITDecl;

function jit_unwind_jump (unwind: pjit_unwind_context_t; pc: Pointer): Integer; libraryLibJITDecl;

function jit_unwind_get_function (unwind: pjit_unwind_context_t): jit_function_t; libraryLibJITDecl;
function jit_unwind_get_offset (unwind: pjit_unwind_context_t): LongWord; libraryLibJITDecl;


(*
 * Memory allocation routines.
 *)
function jit_malloc (size: LongWord): Pointer; libraryLibJITDecl;
function jit_calloc (num: LongWord; size: LongWord): Pointer; libraryLibJITDecl;
function jit_realloc (ptr: Pointer; size: LongWord): Pointer; libraryLibJITDecl;
procedure jit_free (ptr: Pointer); libraryLibJITDecl;

(*
#define jit_new(type)   ((type * )jit_malloc(sizeof(type_)))
#define jit_cnew(type_)    ((type_ * )jit_calloc(1; sizeof(type_)))
*)

(*
 * Memory set/copy/compare routines.
 *)
function jit_memset (dest: Pointer; ch: Integer; len: LongWord): Pointer; libraryLibJITDecl;
function jit_memcpy (dest: Pointer; const src: Pointer; len: LongWord): Pointer; libraryLibJITDecl;
function jit_memmove (dest: Pointer; const src: Pointer; len: LongWord): Pointer; libraryLibJITDecl;
function jit_memcmp (const s1: Pointer; const s2: Pointer; len: LongWord): Integer; libraryLibJITDecl;
function jit_memchr (const str: Pointer; ch: Integer; len: LongWord): Pointer; libraryLibJITDecl;

(*
 * String routines.
 *)
function jit_strlen (const str: PAnsiChar): LongWord; libraryLibJITDecl;
function jit_strcpy (dest: PAnsiChar; const src: PAnsiChar): PAnsiChar; libraryLibJITDecl;
function jit_strcat (dest: PAnsiChar; const src: PAnsiChar): PAnsiChar; libraryLibJITDecl;
function jit_strncpy (dest: PAnsiChar; const src: PAnsiChar; len: LongWord): PAnsiChar; libraryLibJITDecl;
function jit_strdup (const str: PAnsiChar): PAnsiChar; libraryLibJITDecl;
function jit_strndup (const str: PAnsiChar; len: LongWord): PAnsiChar; libraryLibJITDecl;
function jit_strcmp (const str1: PAnsiChar; const str2: PAnsiChar): Integer; libraryLibJITDecl;
function jit_strncmp (const str1: PAnsiChar; const str2: PAnsiChar; len: LongWord): Integer; libraryLibJITDecl;
function jit_stricmp (const str1: PAnsiChar; const str2: PAnsiChar): Integer; libraryLibJITDecl;
function jit_strnicmp (const str1: PAnsiChar; const str2: PAnsiChar; len: LongWord): Integer; libraryLibJITDecl;
function jit_strchr (const str: PAnsiChar; ch: Integer): PAnsiChar; libraryLibJITDecl;
function jit_strrchr (const str: PAnsiChar; ch: Integer): PAnsiChar; libraryLibJITDecl;
{$IFNDEF MSWINDOWS}
function jit_sprintf (str: PAnsiChar; const format: PAnsiChar): Integer; libraryLibJITDecl; varargs;
function jit_snprintf (str: PAnsiChar; len: LongWord; const format: PAnsiChar): Integer; libraryLibJITDecl; varargs;
{$ENDIF}


(*
 * Full struction that can hold a constant of any type.
 *)
type jit_constant_t = record
  type_: jit_type_t;
  case Integer of
    TJIT_TYPE_PTR: (ptr_value: Pointer);
    TJIT_TYPE_INT: (int_value: jit_int);
    TJIT_TYPE_UINT: (uint_value: jit_uint);
    TJIT_TYPE_NINT: (nint_value: jit_nint);
    TJIT_TYPE_NUINT: (nuint_value: jit_nuint);
    TJIT_TYPE_LONG: (long_value: jit_long);
    TJIT_TYPE_ULONG: (ulong_value: jit_ulong);
    TJIT_TYPE_FLOAT32: (float32_value: jit_float32);
    TJIT_TYPE_FLOAT64: (float64_value: jit_float64);
    TJIT_TYPE_NFLOAT: (nfloat_value: jit_nfloat);
    (*un;*) //k8
end;
type pjit_constant_t = ^jit_constant_t;


(*
 * External function declarations.
 *)
function jit_value_create (func: jit_function_t; type_: jit_type_t): jit_value_t; libraryLibJITDecl;
function jit_value_create_nint_constant (func: jit_function_t; type_: jit_type_t; const_value: jit_nint): jit_value_t; libraryLibJITDecl;
function jit_value_create_long_constant (func: jit_function_t; type_: jit_type_t; const_value: jit_long): jit_value_t; libraryLibJITDecl;
function jit_value_create_float32_constant (func: jit_function_t; type_: jit_type_t; const_value: jit_float32): jit_value_t; libraryLibJITDecl;
function jit_value_create_float64_constant (func: jit_function_t; type_: jit_type_t; const_value: jit_float64): jit_value_t; libraryLibJITDecl;
function jit_value_create_nfloat_constant (func: jit_function_t; type_: jit_type_t; const_value: jit_nfloat): jit_value_t; libraryLibJITDecl;
function jit_value_create_constant (func: jit_function_t; const const_value: pjit_constant_t): jit_value_t; libraryLibJITDecl;
function jit_value_get_param (func: jit_function_t; param: LongWord): jit_value_t; libraryLibJITDecl;
function jit_value_get_struct_pointer (func: jit_function_t): jit_value_t; libraryLibJITDecl;
function jit_value_is_temporary (value: jit_value_t): Integer; libraryLibJITDecl;
function jit_value_is_local (value: jit_value_t): Integer; libraryLibJITDecl;
function jit_value_is_constant (value: jit_value_t): Integer; libraryLibJITDecl;
function jit_value_is_parameter (value: jit_value_t): Integer; libraryLibJITDecl;
procedure jit_value_ref (func: jit_function_t; value: jit_value_t); libraryLibJITDecl;
procedure jit_value_set_volatile (value: jit_value_t); libraryLibJITDecl;
function jit_value_is_volatile (value: jit_value_t): Integer; libraryLibJITDecl;
procedure jit_value_set_addressable (value: jit_value_t); libraryLibJITDecl;
function jit_value_is_addressable (value: jit_value_t): Integer; libraryLibJITDecl;
function jit_value_get_type (value: jit_value_t): jit_type_t; libraryLibJITDecl;
function jit_value_get_function (value: jit_value_t): jit_function_t; libraryLibJITDecl;
function jit_value_get_block (value: jit_value_t): jit_block_t; libraryLibJITDecl;
function jit_value_get_context (value: jit_value_t): jit_context_t; libraryLibJITDecl;
function jit_value_get_constant (value: jit_value_t): jit_constant_t; libraryLibJITDecl;
function jit_value_get_nint_constant (value: jit_value_t): jit_nint; libraryLibJITDecl;
function jit_value_get_long_constant (value: jit_value_t): jit_long; libraryLibJITDecl;
function jit_value_get_float32_constant (value: jit_value_t): jit_float32; libraryLibJITDecl;
function jit_value_get_float64_constant (value: jit_value_t): jit_float64; libraryLibJITDecl;
function jit_value_get_nfloat_constant (value: jit_value_t): jit_nfloat; libraryLibJITDecl;
function jit_value_is_true (value: jit_value_t): Integer; libraryLibJITDecl;
function jit_constant_convert (result_: pjit_constant_t; const value: pjit_constant_t; type_: jit_type_t; overflow_check: Integer): Integer; libraryLibJITDecl;


type jit_prot_t = (
  JIT_PROT_NONE,
  JIT_PROT_READ,
  JIT_PROT_READ_WRITE,
  JIT_PROT_EXEC_READ,
  JIT_PROT_EXEC_READ_WRITE
);


procedure jit_vmem_init (); libraryLibJITDecl;

function jit_vmem_page_size (): jit_uint; libraryLibJITDecl;
function jit_vmem_round_up (value: jit_nuint): jit_nuint; libraryLibJITDecl;
function jit_vmem_round_down (value: jit_nuint): jit_nuint; libraryLibJITDecl;

function jit_vmem_reserve (size: jit_uint): Pointer; libraryLibJITDecl;
function jit_vmem_reserve_committed (size: jit_uint; prot: jit_prot_t): Pointer; libraryLibJITDecl;
function jit_vmem_release (addr: Pointer; size: jit_uint): Integer; libraryLibJITDecl;

function jit_vmem_commit (addr: Pointer; size: jit_uint; prot: jit_prot_t): Integer; libraryLibJITDecl;
function jit_vmem_decommit (addr: Pointer; size: jit_uint): Integer; libraryLibJITDecl;

function jit_vmem_protect (addr: Pointer; size: jit_uint; prot: jit_prot_t): Integer; libraryLibJITDecl;


(*
 * Result values for "_jit_cache_start_function" and "_jit_cache_end_function".
 *)
const JIT_MEMORY_OK = 0; (* Function is OK *)
const JIT_MEMORY_RESTART = 1; (* Restart is required *)
const JIT_MEMORY_TOO_BIG = 2; (* Function is too big for the cache *)
const JIT_MEMORY_ERROR = 3; (* Other error *)


(* TODO: the proper place for this is jit-def.h and it's going to depend on the platform. *)
type jit_size_t = LongWord;

type jit_memory_context_t = Pointer;
type jit_function_info_t = Pointer;

type jit_memory_manager = record
  create: function (context: jit_context_t): jit_memory_context_t; libraryLibJITDecl;
  destroy: procedure (memctx: jit_memory_context_t); libraryLibJITDecl;

  find_function_info: function (memctx: jit_memory_context_t; pc: Pointer): jit_function_info_t; libraryLibJITDecl;
  get_function: function (memctx: jit_memory_context_t; info: jit_function_info_t): jit_function_t; libraryLibJITDecl;
  get_function_start: function (memctx: jit_memory_context_t; info: jit_function_info_t): Pointer; libraryLibJITDecl;
  get_function_end: function (memctx: jit_memory_context_t; info: jit_function_info_t): Pointer; libraryLibJITDecl;

  alloc_function: function (memctx: jit_memory_context_t): jit_function_t; libraryLibJITDecl;
  free_function: procedure (memctx: jit_memory_context_t; func: jit_function_t); libraryLibJITDecl;

  start_function: function (memctx: jit_memory_context_t; func: jit_function_t): Integer; libraryLibJITDecl;
  end_function: function (memctx: jit_memory_context_t; result_: Integer): Integer; libraryLibJITDecl;
  extend_limit: function (memctx: jit_memory_context_t; count: Integer): Integer; libraryLibJITDecl;

  get_limit: function (memctx: jit_memory_context_t): Pointer; libraryLibJITDecl;
  get_break: function (memctx: jit_memory_context_t): Pointer; libraryLibJITDecl;
  set_break: procedure (memctx: jit_memory_context_t; brk: Pointer); libraryLibJITDecl;

  alloc_trampoline: function (memctx: jit_memory_context_t): Pointer; libraryLibJITDecl;
  free_trampoline: procedure (memctx: jit_memory_context_t; ptr: Pointer); libraryLibJITDecl;

  alloc_closure: function (memctx: jit_memory_context_t): Pointer; libraryLibJITDecl;
  free_closure: procedure (memctx: jit_memory_context_t; ptr: Pointer); libraryLibJITDecl;

  alloc_data: function (memctx: jit_memory_context_t; size: jit_size_t; align_: jit_size_t): Pointer; libraryLibJITDecl;
end;
type jit_memory_manager_t = (*const(jit_memory_manager)* *)^jit_memory_manager; //k8: const?! was const in C header

function jit_default_memory_manager (): jit_memory_manager_t; libraryLibJITDecl;
procedure jit_context_set_memory_manager (context: jit_context_t; manager: jit_memory_manager_t); libraryLibJITDecl;


(*
import core.stdc.stdio : FILE;

procedure jit_dump_type (FILE* stream; type_: jit_type_t); libraryLibJITDecl;
procedure jit_dump_value (FILE* stream; func: value: jit_function_t;: jit_value_t; const prefix: PAnsiChar); libraryLibJITDecl;
procedure jit_dump_insn (FILE* stream; func: jit_function_t; insn: jit_insn_t); libraryLibJITDecl;
procedure jit_dump_function (FILE* stream; func: jit_function_t; const name: PAnsiChar); libraryLibJITDecl;
*)


(*
 * Get the frame address for a frame which is "n" levels up the stack.
 * A level value of zero indicates the current frame.
 *)
//k8 void* _jit_get_frame_address (void* start, uint n) nothrow @nogc;
(*k8: not complete
#if defined(__GNUC__)
# define jit_get_frame_address(n) \
  (_jit_get_frame_address(jit_get_current_frame(), (n)))
#else
# define jit_get_frame_address(n) (_jit_get_frame_address(0, (n)))
#endif
*)

(*
 * Get the frame address for the current frame.  May be more efficient
 * than using "jit_get_frame_address(0)".
 *
 * Note: some gcc vestions have broken __builtin_frame_address() so use
 * _JIT_ARCH_GET_CURRENT_FRAME() if available.
 *)
(*k8:???
#if defined(__GNUC__)
# define JIT_FAST_GET_CURRENT_FRAME 1
# if defined(_JIT_ARCH_GET_CURRENT_FRAME)
#  define jit_get_current_frame()     \
  ({            \
    void* address;        \
    _JIT_ARCH_GET_CURRENT_FRAME(address); \
    address;        \
  })
# else
#  define jit_get_current_frame() (__builtin_frame_address(0))
# endif
#else
# define JIT_FAST_GET_CURRENT_FRAME 0
# define jit_get_current_frame()  (jit_get_frame_address(0))
#endif

( *
  * Get the next frame up the stack from a specified frame.
  * Returns NULL if it isn't possible to retrieve the next frame.
  * )
Pointer _jit_get_next_frame_address(Pointer frame);
#if defined(__GNUC__) && defined(_JIT_ARCH_GET_NEXT_FRAME)
# define jit_get_next_frame_address(frame)      \
  ({              \
    void* address;          \
    _JIT_ARCH_GET_NEXT_FRAME(address, (frame)); \
    address;          \
  })
#else
# define jit_get_next_frame_address(frame)  \
  (_jit_get_next_frame_address(frame))
#endif

( *
  * Get the return address for a specific frame.
  * )
Pointer _jit_get_return_address(Pointer frame; Pointer frame0; Pointer return0);
#if defined(__GNUC__)
# if defined(_JIT_ARCH_GET_RETURN_ADDRESS)
#  define jit_get_return_address(frame)       \
  ({              \
    void* address;          \
    _JIT_ARCH_GET_RETURN_ADDRESS(address, (frame)); \
    address;          \
  })
# else
#  define jit_get_return_address(frame)     \
  (_jit_get_return_address      \
    ((frame);       \
     __builtin_frame_address(0);    \
     __builtin_return_address(0)))
# endif
#else
# define jit_get_return_address(frame)  \
  (_jit_get_return_address((frame); 0; 0))
#endif

( *
  * Get the return address for the current frame.  May be more efficient
  * than using "jit_get_return_address(0)".
  * )
#if defined(__GNUC__)
# if defined(_JIT_ARCH_GET_CURRENT_RETURN)
#  define jit_get_current_return()      \
  ({            \
    void* address;        \
    _JIT_ARCH_GET_CURRENT_RETURN(address);  \
    address;        \
  })
# else
#  define jit_get_current_return()  (__builtin_return_address(0))
# endif
#else
# define jit_get_current_return() \
  (jit_get_return_address(jit_get_current_frame()))
#endif
*)

(*
 * Declare a stack crawl mark variable.  The address of this variable
 * can be passed to "jit_frame_contains_crawl_mark" to determine
 * if a frame contains the mark.
 *)
//k8:??? struct jit_crawl_mark_t { void* volatile mark; }
//k8:??? #define jit_declare_crawl_mark(name)  jit_crawl_mark_t name = {0}

(*
 * Determine if the stack frame just above "frame" contains a
 * particular crawl mark.
 *)
//k8:??? int jit_frame_contains_crawl_mark(void* frame, jit_crawl_mark_t* mark);


implementation

function jit_context_create; libraryLibJITImp;
procedure jit_context_destroy; libraryLibJITImp;
procedure jit_context_build_start; libraryLibJITImp;
procedure jit_context_build_end; libraryLibJITImp;
procedure jit_context_set_on_demand_driver; libraryLibJITImp;
function jit_context_set_meta; libraryLibJITImp;
function jit_context_set_meta_numeric; libraryLibJITImp;
function jit_context_get_meta; libraryLibJITImp;
function jit_context_get_meta_numeric; libraryLibJITImp;
procedure jit_context_free_meta; libraryLibJITImp;
procedure jit_apply; libraryLibJITImp;
procedure jit_apply_raw; libraryLibJITImp;
function jit_raw_supported; libraryLibJITImp;
function jit_closure_create; libraryLibJITImp;
function jit_closure_va_get_nint; libraryLibJITImp;
function jit_closure_va_get_nuint; libraryLibJITImp;
function jit_closure_va_get_long; libraryLibJITImp;
function jit_closure_va_get_ulong; libraryLibJITImp;
function jit_closure_va_get_float32; libraryLibJITImp;
function jit_closure_va_get_float64; libraryLibJITImp;
function jit_closure_va_get_nfloat; libraryLibJITImp;
function jit_closure_va_get_ptr; libraryLibJITImp;
procedure jit_closure_va_get_struct; libraryLibJITImp;
function jit_block_get_function; libraryLibJITImp;
function jit_block_get_context; libraryLibJITImp;
function jit_block_get_label; libraryLibJITImp;
function jit_block_get_next_label; libraryLibJITImp;
function jit_block_next; libraryLibJITImp;
function jit_block_previous; libraryLibJITImp;
function jit_block_from_label; libraryLibJITImp;
function jit_block_set_meta; libraryLibJITImp;
function jit_block_get_meta; libraryLibJITImp;
procedure jit_block_free_meta; libraryLibJITImp;
function jit_block_is_reachable; libraryLibJITImp;
function jit_block_ends_in_dead; libraryLibJITImp;
function jit_block_current_is_dead; libraryLibJITImp;
function jit_debugging_possible; libraryLibJITImp;
function jit_debugger_create; libraryLibJITImp;
procedure jit_debugger_destroy; libraryLibJITImp;
function jit_debugger_get_context; libraryLibJITImp;
function jit_debugger_from_context; libraryLibJITImp;
function jit_debugger_get_self; libraryLibJITImp;
function jit_debugger_get_thread; libraryLibJITImp;
function jit_debugger_get_native_thread; libraryLibJITImp;
procedure jit_debugger_set_breakable; libraryLibJITImp;
procedure jit_debugger_attach_self; libraryLibJITImp;
procedure jit_debugger_detach_self; libraryLibJITImp;
function jit_debugger_wait_event; libraryLibJITImp;
function jit_debugger_add_breakpoint; libraryLibJITImp;
procedure jit_debugger_remove_breakpoint; libraryLibJITImp;
procedure jit_debugger_remove_all_breakpoints; libraryLibJITImp;
function jit_debugger_is_alive; libraryLibJITImp;
function jit_debugger_is_running; libraryLibJITImp;
procedure jit_debugger_run; libraryLibJITImp;
procedure jit_debugger_step; libraryLibJITImp;
procedure jit_debugger_next; libraryLibJITImp;
procedure jit_debugger_finish; libraryLibJITImp;
procedure jit_debugger_break; libraryLibJITImp;
procedure jit_debugger_quit; libraryLibJITImp;
function jit_debugger_set_hook; libraryLibJITImp;
function jit_readelf_open; libraryLibJITImp;
procedure jit_readelf_close; libraryLibJITImp;
function jit_readelf_get_name; libraryLibJITImp;
function jit_readelf_get_symbol; libraryLibJITImp;
function jit_readelf_get_section; libraryLibJITImp;
function jit_readelf_get_section_by_type; libraryLibJITImp;
function jit_readelf_map_vaddr; libraryLibJITImp;
function jit_readelf_num_needed; libraryLibJITImp;
function jit_readelf_get_needed; libraryLibJITImp;
procedure jit_readelf_add_to_context; libraryLibJITImp;
function jit_readelf_resolve_all; libraryLibJITImp;
function jit_readelf_register_symbol; libraryLibJITImp;
function jit_writeelf_create; libraryLibJITImp;
procedure jit_writeelf_destroy; libraryLibJITImp;
function jit_writeelf_write; libraryLibJITImp;
function jit_writeelf_add_function; libraryLibJITImp;
function jit_writeelf_add_needed; libraryLibJITImp;
function jit_writeelf_write_section; libraryLibJITImp;
function jit_exception_get_last; libraryLibJITImp;
function jit_exception_get_last_and_clear; libraryLibJITImp;
procedure jit_exception_set_last; libraryLibJITImp;
procedure jit_exception_clear_last; libraryLibJITImp;
procedure jit_exception_throw; libraryLibJITImp;
procedure jit_exception_builtin; libraryLibJITImp;
function jit_exception_set_handler; libraryLibJITImp;
function jit_exception_get_handler; libraryLibJITImp;
function jit_exception_get_stack_trace; libraryLibJITImp;
function jit_stack_trace_get_size; libraryLibJITImp;
function jit_stack_trace_get_function; libraryLibJITImp;
function jit_stack_trace_get_pc; libraryLibJITImp;
function jit_stack_trace_get_offset; libraryLibJITImp;
procedure jit_stack_trace_free; libraryLibJITImp;
function jit_function_create; libraryLibJITImp;
function jit_function_create_nested; libraryLibJITImp;
procedure jit_function_abandon; libraryLibJITImp;
function jit_function_get_context; libraryLibJITImp;
function jit_function_get_signature; libraryLibJITImp;
function jit_function_set_meta; libraryLibJITImp;
function jit_function_get_meta; libraryLibJITImp;
procedure jit_function_free_meta; libraryLibJITImp;
function jit_function_next; libraryLibJITImp;
function jit_function_previous; libraryLibJITImp;
function jit_function_get_entry; libraryLibJITImp;
function jit_function_get_current; libraryLibJITImp;
function jit_function_get_nested_parent; libraryLibJITImp;
function jit_function_compile; libraryLibJITImp;
function jit_function_is_compiled; libraryLibJITImp;
procedure jit_function_set_recompilable; libraryLibJITImp;
procedure jit_function_clear_recompilable; libraryLibJITImp;
function jit_function_is_recompilable; libraryLibJITImp;
function jit_function_compile_entry; libraryLibJITImp;
procedure jit_function_setup_entry; libraryLibJITImp;
function jit_function_to_closure; libraryLibJITImp;
function jit_function_from_closure; libraryLibJITImp;
function jit_function_from_pc; libraryLibJITImp;
function jit_function_to_vtable_pointer; libraryLibJITImp;
function jit_function_from_vtable_pointer; libraryLibJITImp;
procedure jit_function_set_on_demand_compiler; libraryLibJITImp;
function jit_function_get_on_demand_compiler; libraryLibJITImp;
function jit_function_apply; libraryLibJITImp;
function jit_function_apply_vararg; libraryLibJITImp;
procedure jit_function_set_optimization_level; libraryLibJITImp;
function jit_function_get_optimization_level; libraryLibJITImp;
function jit_function_get_max_optimization_level; libraryLibJITImp;
function jit_function_reserve_label; libraryLibJITImp;
function jit_function_labels_equal; libraryLibJITImp;
procedure jit_init; libraryLibJITImp;
function jit_uses_interpreter; libraryLibJITImp;
function jit_supports_threads; libraryLibJITImp;
function jit_supports_virtual_memory; libraryLibJITImp;
function jit_supports_closures; libraryLibJITImp;
function jit_get_closure_size; libraryLibJITImp;
function jit_get_closure_alignment; libraryLibJITImp;
function jit_get_trampoline_size; libraryLibJITImp;
function jit_get_trampoline_alignment; libraryLibJITImp;
function jit_insn_get_opcode; libraryLibJITImp;
function jit_insn_get_dest; libraryLibJITImp;
function jit_insn_get_value1; libraryLibJITImp;
function jit_insn_get_value2; libraryLibJITImp;
function jit_insn_get_label; libraryLibJITImp;
function jit_insn_get_function; libraryLibJITImp;
function jit_insn_get_native; libraryLibJITImp;
function jit_insn_get_name; libraryLibJITImp;
function jit_insn_get_signature; libraryLibJITImp;
function jit_insn_dest_is_value; libraryLibJITImp;
function jit_insn_label; libraryLibJITImp;
function jit_insn_label_tight; libraryLibJITImp;
function jit_insn_new_block; libraryLibJITImp;
function jit_insn_load; libraryLibJITImp;
function jit_insn_dup; libraryLibJITImp;
function jit_insn_store; libraryLibJITImp;
function jit_insn_load_relative; libraryLibJITImp;
function jit_insn_store_relative; libraryLibJITImp;
function jit_insn_add_relative; libraryLibJITImp;
function jit_insn_load_elem; libraryLibJITImp;
function jit_insn_load_elem_address; libraryLibJITImp;
function jit_insn_store_elem; libraryLibJITImp;
function jit_insn_check_null; libraryLibJITImp;
function jit_insn_nop; libraryLibJITImp;
function jit_insn_add; libraryLibJITImp;
function jit_insn_add_ovf; libraryLibJITImp;
function jit_insn_sub; libraryLibJITImp;
function jit_insn_sub_ovf; libraryLibJITImp;
function jit_insn_mul; libraryLibJITImp;
function jit_insn_mul_ovf; libraryLibJITImp;
function jit_insn_div; libraryLibJITImp;
function jit_insn_rem; libraryLibJITImp;
function jit_insn_rem_ieee; libraryLibJITImp;
function jit_insn_neg; libraryLibJITImp;
function jit_insn_and; libraryLibJITImp;
function jit_insn_or; libraryLibJITImp;
function jit_insn_xor; libraryLibJITImp;
function jit_insn_not; libraryLibJITImp;
function jit_insn_shl; libraryLibJITImp;
function jit_insn_shr; libraryLibJITImp;
function jit_insn_ushr; libraryLibJITImp;
function jit_insn_sshr; libraryLibJITImp;
function jit_insn_eq; libraryLibJITImp;
function jit_insn_ne; libraryLibJITImp;
function jit_insn_lt; libraryLibJITImp;
function jit_insn_le; libraryLibJITImp;
function jit_insn_gt; libraryLibJITImp;
function jit_insn_ge; libraryLibJITImp;
function jit_insn_cmpl; libraryLibJITImp;
function jit_insn_cmpg; libraryLibJITImp;
function jit_insn_to_bool; libraryLibJITImp;
function jit_insn_to_not_bool; libraryLibJITImp;
function jit_insn_acos; libraryLibJITImp;
function jit_insn_asin; libraryLibJITImp;
function jit_insn_atan; libraryLibJITImp;
function jit_insn_atan2; libraryLibJITImp;
function jit_insn_ceil; libraryLibJITImp;
function jit_insn_cos; libraryLibJITImp;
function jit_insn_cosh; libraryLibJITImp;
function jit_insn_exp; libraryLibJITImp;
function jit_insn_floor; libraryLibJITImp;
function jit_insn_log; libraryLibJITImp;
function jit_insn_log10; libraryLibJITImp;
function jit_insn_pow; libraryLibJITImp;
function jit_insn_rint; libraryLibJITImp;
function jit_insn_round; libraryLibJITImp;
function jit_insn_sin; libraryLibJITImp;
function jit_insn_sinh; libraryLibJITImp;
function jit_insn_sqrt; libraryLibJITImp;
function jit_insn_tan; libraryLibJITImp;
function jit_insn_tanh; libraryLibJITImp;
function jit_insn_trunc; libraryLibJITImp;
function jit_insn_is_nan; libraryLibJITImp;
function jit_insn_is_finite; libraryLibJITImp;
function jit_insn_is_inf; libraryLibJITImp;
function jit_insn_abs; libraryLibJITImp;
function jit_insn_min; libraryLibJITImp;
function jit_insn_max; libraryLibJITImp;
function jit_insn_sign; libraryLibJITImp;
function jit_insn_branch; libraryLibJITImp;
function jit_insn_branch_if; libraryLibJITImp;
function jit_insn_branch_if_not; libraryLibJITImp;
function jit_insn_jump_table; libraryLibJITImp;
function jit_insn_address_of; libraryLibJITImp;
function jit_insn_address_of_label; libraryLibJITImp;
function jit_insn_convert; libraryLibJITImp;
function jit_insn_call; libraryLibJITImp;
function jit_insn_call_indirect; libraryLibJITImp;
function jit_insn_call_indirect_vtable; libraryLibJITImp;
function jit_insn_call_native; libraryLibJITImp;
function jit_insn_call_intrinsic; libraryLibJITImp;
function jit_insn_incoming_reg; libraryLibJITImp;
function jit_insn_incoming_frame_posn; libraryLibJITImp;
function jit_insn_outgoing_reg; libraryLibJITImp;
function jit_insn_outgoing_frame_posn; libraryLibJITImp;
function jit_insn_return_reg; libraryLibJITImp;
function jit_insn_setup_for_nested; libraryLibJITImp;
function jit_insn_flush_struct; libraryLibJITImp;
function jit_insn_import; libraryLibJITImp;
function jit_insn_push; libraryLibJITImp;
function jit_insn_push_ptr; libraryLibJITImp;
function jit_insn_set_param; libraryLibJITImp;
function jit_insn_set_param_ptr; libraryLibJITImp;
function jit_insn_push_return_area_ptr; libraryLibJITImp;
function jit_insn_pop_stack; libraryLibJITImp;
function jit_insn_defer_pop_stack; libraryLibJITImp;
function jit_insn_flush_defer_pop; libraryLibJITImp;
function jit_insn_return; libraryLibJITImp;
function jit_insn_return_ptr; libraryLibJITImp;
function jit_insn_default_return; libraryLibJITImp;
function jit_insn_throw; libraryLibJITImp;
function jit_insn_get_call_stack; libraryLibJITImp;
function jit_insn_thrown_exception; libraryLibJITImp;
function jit_insn_uses_catcher; libraryLibJITImp;
function jit_insn_start_catcher; libraryLibJITImp;
function jit_insn_branch_if_pc_not_in_range; libraryLibJITImp;
function jit_insn_rethrow_unhandled; libraryLibJITImp;
function jit_insn_start_finally; libraryLibJITImp;
function jit_insn_return_from_finally; libraryLibJITImp;
function jit_insn_call_finally; libraryLibJITImp;
function jit_insn_start_filter; libraryLibJITImp;
function jit_insn_return_from_filter; libraryLibJITImp;
function jit_insn_call_filter; libraryLibJITImp;
function jit_insn_memcpy; libraryLibJITImp;
function jit_insn_memmove; libraryLibJITImp;
function jit_insn_memset; libraryLibJITImp;
function jit_insn_alloca; libraryLibJITImp;
function jit_insn_move_blocks_to_end; libraryLibJITImp;
function jit_insn_move_blocks_to_start; libraryLibJITImp;
function jit_insn_mark_offset; libraryLibJITImp;
function jit_insn_mark_breakpoint; libraryLibJITImp;
function jit_insn_mark_breakpoint_variable; libraryLibJITImp;
procedure jit_insn_iter_init; libraryLibJITImp;
procedure jit_insn_iter_init_last; libraryLibJITImp;
function jit_insn_iter_next; libraryLibJITImp;
function jit_insn_iter_previous; libraryLibJITImp;
function jit_int_add; libraryLibJITImp;
function jit_int_sub; libraryLibJITImp;
function jit_int_mul; libraryLibJITImp;
function jit_int_div; libraryLibJITImp;
function jit_int_rem; libraryLibJITImp;
function jit_int_add_ovf; libraryLibJITImp;
function jit_int_sub_ovf; libraryLibJITImp;
function jit_int_mul_ovf; libraryLibJITImp;
function jit_int_neg; libraryLibJITImp;
function jit_int_and; libraryLibJITImp;
function jit_int_or; libraryLibJITImp;
function jit_int_xor; libraryLibJITImp;
function jit_int_not; libraryLibJITImp;
function jit_int_shl; libraryLibJITImp;
function jit_int_shr; libraryLibJITImp;
function jit_int_eq; libraryLibJITImp;
function jit_int_ne; libraryLibJITImp;
function jit_int_lt; libraryLibJITImp;
function jit_int_le; libraryLibJITImp;
function jit_int_gt; libraryLibJITImp;
function jit_int_ge; libraryLibJITImp;
function jit_int_cmp; libraryLibJITImp;
function jit_int_abs; libraryLibJITImp;
function jit_int_min; libraryLibJITImp;
function jit_int_max; libraryLibJITImp;
function jit_int_sign; libraryLibJITImp;
function jit_uint_add; libraryLibJITImp;
function jit_uint_sub; libraryLibJITImp;
function jit_uint_mul; libraryLibJITImp;
function jit_uint_div; libraryLibJITImp;
function jit_uint_rem; libraryLibJITImp;
function jit_uint_add_ovf; libraryLibJITImp;
function jit_uint_sub_ovf; libraryLibJITImp;
function jit_uint_mul_ovf; libraryLibJITImp;
function jit_uint_neg; libraryLibJITImp;
function jit_uint_and; libraryLibJITImp;
function jit_uint_or; libraryLibJITImp;
function jit_uint_xor; libraryLibJITImp;
function jit_uint_not; libraryLibJITImp;
function jit_uint_shl; libraryLibJITImp;
function jit_uint_shr; libraryLibJITImp;
function jit_uint_eq; libraryLibJITImp;
function jit_uint_ne; libraryLibJITImp;
function jit_uint_lt; libraryLibJITImp;
function jit_uint_le; libraryLibJITImp;
function jit_uint_gt; libraryLibJITImp;
function jit_uint_ge; libraryLibJITImp;
function jit_uint_cmp; libraryLibJITImp;
function jit_uint_min; libraryLibJITImp;
function jit_uint_max; libraryLibJITImp;
function jit_long_add; libraryLibJITImp;
function jit_long_sub; libraryLibJITImp;
function jit_long_mul; libraryLibJITImp;
function jit_long_div; libraryLibJITImp;
function jit_long_rem; libraryLibJITImp;
function jit_long_add_ovf; libraryLibJITImp;
function jit_long_sub_ovf; libraryLibJITImp;
function jit_long_mul_ovf; libraryLibJITImp;
function jit_long_neg; libraryLibJITImp;
function jit_long_and; libraryLibJITImp;
function jit_long_or; libraryLibJITImp;
function jit_long_xor; libraryLibJITImp;
function jit_long_not; libraryLibJITImp;
function jit_long_shl; libraryLibJITImp;
function jit_long_shr; libraryLibJITImp;
function jit_long_eq; libraryLibJITImp;
function jit_long_ne; libraryLibJITImp;
function jit_long_lt; libraryLibJITImp;
function jit_long_le; libraryLibJITImp;
function jit_long_gt; libraryLibJITImp;
function jit_long_ge; libraryLibJITImp;
function jit_long_cmp; libraryLibJITImp;
function jit_long_abs; libraryLibJITImp;
function jit_long_min; libraryLibJITImp;
function jit_long_max; libraryLibJITImp;
function jit_long_sign; libraryLibJITImp;
function jit_ulong_add; libraryLibJITImp;
function jit_ulong_sub; libraryLibJITImp;
function jit_ulong_mul; libraryLibJITImp;
function jit_ulong_div; libraryLibJITImp;
function jit_ulong_rem; libraryLibJITImp;
function jit_ulong_add_ovf; libraryLibJITImp;
function jit_ulong_sub_ovf; libraryLibJITImp;
function jit_ulong_mul_ovf; libraryLibJITImp;
function jit_ulong_neg; libraryLibJITImp;
function jit_ulong_and; libraryLibJITImp;
function jit_ulong_or; libraryLibJITImp;
function jit_ulong_xor; libraryLibJITImp;
function jit_ulong_not; libraryLibJITImp;
function jit_ulong_shl; libraryLibJITImp;
function jit_ulong_shr; libraryLibJITImp;
function jit_ulong_eq; libraryLibJITImp;
function jit_ulong_ne; libraryLibJITImp;
function jit_ulong_lt; libraryLibJITImp;
function jit_ulong_le; libraryLibJITImp;
function jit_ulong_gt; libraryLibJITImp;
function jit_ulong_ge; libraryLibJITImp;
function jit_ulong_cmp; libraryLibJITImp;
function jit_ulong_min; libraryLibJITImp;
function jit_ulong_max; libraryLibJITImp;
function jit_float32_add; libraryLibJITImp;
function jit_float32_sub; libraryLibJITImp;
function jit_float32_mul; libraryLibJITImp;
function jit_float32_div; libraryLibJITImp;
function jit_float32_rem; libraryLibJITImp;
function jit_float32_ieee_rem; libraryLibJITImp;
function jit_float32_neg; libraryLibJITImp;
function jit_float32_eq; libraryLibJITImp;
function jit_float32_ne; libraryLibJITImp;
function jit_float32_lt; libraryLibJITImp;
function jit_float32_le; libraryLibJITImp;
function jit_float32_gt; libraryLibJITImp;
function jit_float32_ge; libraryLibJITImp;
function jit_float32_cmpl; libraryLibJITImp;
function jit_float32_cmpg; libraryLibJITImp;
function jit_float32_acos; libraryLibJITImp;
function jit_float32_asin; libraryLibJITImp;
function jit_float32_atan; libraryLibJITImp;
function jit_float32_atan2; libraryLibJITImp;
function jit_float32_ceil; libraryLibJITImp;
function jit_float32_cos; libraryLibJITImp;
function jit_float32_cosh; libraryLibJITImp;
function jit_float32_exp; libraryLibJITImp;
function jit_float32_floor; libraryLibJITImp;
function jit_float32_log; libraryLibJITImp;
function jit_float32_log10; libraryLibJITImp;
function jit_float32_pow; libraryLibJITImp;
function jit_float32_rint; libraryLibJITImp;
function jit_float32_round; libraryLibJITImp;
function jit_float32_sin; libraryLibJITImp;
function jit_float32_sinh; libraryLibJITImp;
function jit_float32_sqrt; libraryLibJITImp;
function jit_float32_tan; libraryLibJITImp;
function jit_float32_tanh; libraryLibJITImp;
function jit_float32_trunc; libraryLibJITImp;
function jit_float32_is_finite; libraryLibJITImp;
function jit_float32_is_nan; libraryLibJITImp;
function jit_float32_is_inf; libraryLibJITImp;
function jit_float32_abs; libraryLibJITImp;
function jit_float32_min; libraryLibJITImp;
function jit_float32_max; libraryLibJITImp;
function jit_float32_sign; libraryLibJITImp;
function jit_float64_add; libraryLibJITImp;
function jit_float64_sub; libraryLibJITImp;
function jit_float64_mul; libraryLibJITImp;
function jit_float64_div; libraryLibJITImp;
function jit_float64_rem; libraryLibJITImp;
function jit_float64_ieee_rem; libraryLibJITImp;
function jit_float64_neg; libraryLibJITImp;
function jit_float64_eq; libraryLibJITImp;
function jit_float64_ne; libraryLibJITImp;
function jit_float64_lt; libraryLibJITImp;
function jit_float64_le; libraryLibJITImp;
function jit_float64_gt; libraryLibJITImp;
function jit_float64_ge; libraryLibJITImp;
function jit_float64_cmpl; libraryLibJITImp;
function jit_float64_cmpg; libraryLibJITImp;
function jit_float64_acos; libraryLibJITImp;
function jit_float64_asin; libraryLibJITImp;
function jit_float64_atan; libraryLibJITImp;
function jit_float64_atan2; libraryLibJITImp;
function jit_float64_ceil; libraryLibJITImp;
function jit_float64_cos; libraryLibJITImp;
function jit_float64_cosh; libraryLibJITImp;
function jit_float64_exp; libraryLibJITImp;
function jit_float64_floor; libraryLibJITImp;
function jit_float64_log; libraryLibJITImp;
function jit_float64_log10; libraryLibJITImp;
function jit_float64_pow; libraryLibJITImp;
function jit_float64_rint; libraryLibJITImp;
function jit_float64_round; libraryLibJITImp;
function jit_float64_sin; libraryLibJITImp;
function jit_float64_sinh; libraryLibJITImp;
function jit_float64_sqrt; libraryLibJITImp;
function jit_float64_tan; libraryLibJITImp;
function jit_float64_tanh; libraryLibJITImp;
function jit_float64_trunc; libraryLibJITImp;
function jit_float64_is_finite; libraryLibJITImp;
function jit_float64_is_nan; libraryLibJITImp;
function jit_float64_is_inf; libraryLibJITImp;
function jit_float64_abs; libraryLibJITImp;
function jit_float64_min; libraryLibJITImp;
function jit_float64_max; libraryLibJITImp;
function jit_float64_sign; libraryLibJITImp;
function jit_nfloat_add; libraryLibJITImp;
function jit_nfloat_sub; libraryLibJITImp;
function jit_nfloat_mul; libraryLibJITImp;
function jit_nfloat_div; libraryLibJITImp;
function jit_nfloat_rem; libraryLibJITImp;
function jit_nfloat_ieee_rem; libraryLibJITImp;
function jit_nfloat_neg; libraryLibJITImp;
function jit_nfloat_eq; libraryLibJITImp;
function jit_nfloat_ne; libraryLibJITImp;
function jit_nfloat_lt; libraryLibJITImp;
function jit_nfloat_le; libraryLibJITImp;
function jit_nfloat_gt; libraryLibJITImp;
function jit_nfloat_ge; libraryLibJITImp;
function jit_nfloat_cmpl; libraryLibJITImp;
function jit_nfloat_cmpg; libraryLibJITImp;
function jit_nfloat_acos; libraryLibJITImp;
function jit_nfloat_asin; libraryLibJITImp;
function jit_nfloat_atan; libraryLibJITImp;
function jit_nfloat_atan2; libraryLibJITImp;
function jit_nfloat_ceil; libraryLibJITImp;
function jit_nfloat_cos; libraryLibJITImp;
function jit_nfloat_cosh; libraryLibJITImp;
function jit_nfloat_exp; libraryLibJITImp;
function jit_nfloat_floor; libraryLibJITImp;
function jit_nfloat_log; libraryLibJITImp;
function jit_nfloat_log10; libraryLibJITImp;
function jit_nfloat_pow; libraryLibJITImp;
function jit_nfloat_rint; libraryLibJITImp;
function jit_nfloat_round; libraryLibJITImp;
function jit_nfloat_sin; libraryLibJITImp;
function jit_nfloat_sinh; libraryLibJITImp;
function jit_nfloat_sqrt; libraryLibJITImp;
function jit_nfloat_tan; libraryLibJITImp;
function jit_nfloat_tanh; libraryLibJITImp;
function jit_nfloat_trunc; libraryLibJITImp;
function jit_nfloat_is_finite; libraryLibJITImp;
function jit_nfloat_is_nan; libraryLibJITImp;
function jit_nfloat_is_inf; libraryLibJITImp;
function jit_nfloat_abs; libraryLibJITImp;
function jit_nfloat_min; libraryLibJITImp;
function jit_nfloat_max; libraryLibJITImp;
function jit_nfloat_sign; libraryLibJITImp;
function jit_int_to_sbyte; libraryLibJITImp;
function jit_int_to_ubyte; libraryLibJITImp;
function jit_int_to_short; libraryLibJITImp;
function jit_int_to_ushort; libraryLibJITImp;
function jit_int_to_int; libraryLibJITImp;
function jit_int_to_uint; libraryLibJITImp;
function jit_int_to_long; libraryLibJITImp;
function jit_int_to_ulong; libraryLibJITImp;
function jit_uint_to_int; libraryLibJITImp;
function jit_uint_to_uint; libraryLibJITImp;
function jit_uint_to_long; libraryLibJITImp;
function jit_uint_to_ulong; libraryLibJITImp;
function jit_long_to_int; libraryLibJITImp;
function jit_long_to_uint; libraryLibJITImp;
function jit_long_to_long; libraryLibJITImp;
function jit_long_to_ulong; libraryLibJITImp;
function jit_ulong_to_int; libraryLibJITImp;
function jit_ulong_to_uint; libraryLibJITImp;
function jit_ulong_to_long; libraryLibJITImp;
function jit_ulong_to_ulong; libraryLibJITImp;
function jit_int_to_sbyte_ovf; libraryLibJITImp;
function jit_int_to_ubyte_ovf; libraryLibJITImp;
function jit_int_to_short_ovf; libraryLibJITImp;
function jit_int_to_ushort_ovf; libraryLibJITImp;
function jit_int_to_int_ovf; libraryLibJITImp;
function jit_int_to_uint_ovf; libraryLibJITImp;
function jit_int_to_long_ovf; libraryLibJITImp;
function jit_int_to_ulong_ovf; libraryLibJITImp;
function jit_uint_to_int_ovf; libraryLibJITImp;
function jit_uint_to_uint_ovf; libraryLibJITImp;
function jit_uint_to_long_ovf; libraryLibJITImp;
function jit_uint_to_ulong_ovf; libraryLibJITImp;
function jit_long_to_int_ovf; libraryLibJITImp;
function jit_long_to_uint_ovf; libraryLibJITImp;
function jit_long_to_long_ovf; libraryLibJITImp;
function jit_long_to_ulong_ovf; libraryLibJITImp;
function jit_ulong_to_int_ovf; libraryLibJITImp;
function jit_ulong_to_uint_ovf; libraryLibJITImp;
function jit_ulong_to_long_ovf; libraryLibJITImp;
function jit_ulong_to_ulong_ovf; libraryLibJITImp;
function jit_float32_to_int; libraryLibJITImp;
function jit_float32_to_uint; libraryLibJITImp;
function jit_float32_to_long; libraryLibJITImp;
function jit_float32_to_ulong; libraryLibJITImp;
function jit_float32_to_int_ovf; libraryLibJITImp;
function jit_float32_to_uint_ovf; libraryLibJITImp;
function jit_float32_to_long_ovf; libraryLibJITImp;
function jit_float32_to_ulong_ovf; libraryLibJITImp;
function jit_float64_to_int; libraryLibJITImp;
function jit_float64_to_uint; libraryLibJITImp;
function jit_float64_to_long; libraryLibJITImp;
function jit_float64_to_ulong; libraryLibJITImp;
function jit_float64_to_int_ovf; libraryLibJITImp;
function jit_float64_to_uint_ovf; libraryLibJITImp;
function jit_float64_to_long_ovf; libraryLibJITImp;
function jit_float64_to_ulong_ovf; libraryLibJITImp;
function jit_nfloat_to_int; libraryLibJITImp;
function jit_nfloat_to_uint; libraryLibJITImp;
function jit_nfloat_to_long; libraryLibJITImp;
function jit_nfloat_to_ulong; libraryLibJITImp;
function jit_nfloat_to_int_ovf; libraryLibJITImp;
function jit_nfloat_to_uint_ovf; libraryLibJITImp;
function jit_nfloat_to_long_ovf; libraryLibJITImp;
function jit_nfloat_to_ulong_ovf; libraryLibJITImp;
function jit_int_to_float32; libraryLibJITImp;
function jit_int_to_float64; libraryLibJITImp;
function jit_int_to_nfloat; libraryLibJITImp;
function jit_uint_to_float32; libraryLibJITImp;
function jit_uint_to_float64; libraryLibJITImp;
function jit_uint_to_nfloat; libraryLibJITImp;
function jit_long_to_float32; libraryLibJITImp;
function jit_long_to_float64; libraryLibJITImp;
function jit_long_to_nfloat; libraryLibJITImp;
function jit_ulong_to_float32; libraryLibJITImp;
function jit_ulong_to_float64; libraryLibJITImp;
function jit_ulong_to_nfloat; libraryLibJITImp;
function jit_float32_to_float64; libraryLibJITImp;
function jit_float32_to_nfloat; libraryLibJITImp;
function jit_float64_to_float32; libraryLibJITImp;
function jit_float64_to_nfloat; libraryLibJITImp;
function jit_nfloat_to_float32; libraryLibJITImp;
function jit_nfloat_to_float64; libraryLibJITImp;
function jit_meta_set; libraryLibJITImp;
function jit_meta_get; libraryLibJITImp;
procedure jit_meta_free; libraryLibJITImp;
procedure jit_meta_destroy; libraryLibJITImp;
procedure jitom_destroy_model; libraryLibJITImp;
function jitom_get_class_by_name; libraryLibJITImp;
function jitom_class_get_name; libraryLibJITImp;
function jitom_class_get_modifiers; libraryLibJITImp;
function jitom_class_get_type; libraryLibJITImp;
function jitom_class_get_value_type; libraryLibJITImp;
function jitom_class_get_primary_super; libraryLibJITImp;
function jitom_class_get_all_supers; libraryLibJITImp;
function jitom_class_get_interfaces; libraryLibJITImp;
function jitom_class_get_fields; libraryLibJITImp;
function jitom_class_get_methods; libraryLibJITImp;
function jitom_class_new; libraryLibJITImp;
function jitom_class_new_value; libraryLibJITImp;
function jitom_class_delete; libraryLibJITImp;
function jitom_class_add_ref; libraryLibJITImp;
function jitom_field_get_name; libraryLibJITImp;
function jitom_field_get_type; libraryLibJITImp;
function jitom_field_get_modifiers; libraryLibJITImp;
function jitom_field_load; libraryLibJITImp;
function jitom_field_load_address; libraryLibJITImp;
function jitom_field_store; libraryLibJITImp;
function jitom_method_get_name; libraryLibJITImp;
function jitom_method_get_type; libraryLibJITImp;
function jitom_method_get_modifiers; libraryLibJITImp;
function jitom_method_invoke; libraryLibJITImp;
function jitom_method_invoke_virtual; libraryLibJITImp;
function jitom_type_tag_as_class; libraryLibJITImp;
function jitom_type_tag_as_value; libraryLibJITImp;
function jitom_type_is_class; libraryLibJITImp;
function jitom_type_is_value; libraryLibJITImp;
function jitom_type_get_model; libraryLibJITImp;
function jitom_type_get_class; libraryLibJITImp;
function jit_type_copy; libraryLibJITImp;
procedure jit_type_free; libraryLibJITImp;
function jit_type_create_struct; libraryLibJITImp;
function jit_type_create_union; libraryLibJITImp;
function jit_type_create_signature; libraryLibJITImp;
function jit_type_create_pointer; libraryLibJITImp;
function jit_type_create_tagged; libraryLibJITImp;
function jit_type_set_names; libraryLibJITImp;
procedure jit_type_set_size_and_alignment; libraryLibJITImp;
procedure jit_type_set_offset; libraryLibJITImp;
function jit_type_get_kind; libraryLibJITImp;
function jit_type_get_size; libraryLibJITImp;
function jit_type_get_alignment; libraryLibJITImp;
function jit_type_num_fields; libraryLibJITImp;
function jit_type_get_field; libraryLibJITImp;
function jit_type_get_offset; libraryLibJITImp;
function jit_type_get_name; libraryLibJITImp;
function jit_type_find_name; libraryLibJITImp;
function jit_type_num_params; libraryLibJITImp;
function jit_type_get_return; libraryLibJITImp;
function jit_type_get_param; libraryLibJITImp;
function jit_type_get_abi; libraryLibJITImp;
function jit_type_get_ref; libraryLibJITImp;
function jit_type_get_tagged_type; libraryLibJITImp;
procedure jit_type_set_tagged_type; libraryLibJITImp;
function jit_type_get_tagged_kind; libraryLibJITImp;
function jit_type_get_tagged_data; libraryLibJITImp;
procedure jit_type_set_tagged_data; libraryLibJITImp;
function jit_type_is_primitive; libraryLibJITImp;
function jit_type_is_struct; libraryLibJITImp;
function jit_type_is_union; libraryLibJITImp;
function jit_type_is_signature; libraryLibJITImp;
function jit_type_is_pointer; libraryLibJITImp;
function jit_type_is_tagged; libraryLibJITImp;
function jit_type_best_alignment; libraryLibJITImp;
function jit_type_normalize; libraryLibJITImp;
function jit_type_remove_tags; libraryLibJITImp;
function jit_type_promote_int; libraryLibJITImp;
function jit_type_return_via_pointer; libraryLibJITImp;
function jit_type_has_tag; libraryLibJITImp;
function jit_unwind_init; libraryLibJITImp;
procedure jit_unwind_free; libraryLibJITImp;
function jit_unwind_next; libraryLibJITImp;
function jit_unwind_next_pc; libraryLibJITImp;
function jit_unwind_get_pc; libraryLibJITImp;
function jit_unwind_jump; libraryLibJITImp;
function jit_unwind_get_function; libraryLibJITImp;
function jit_unwind_get_offset; libraryLibJITImp;
function jit_malloc; libraryLibJITImp;
function jit_calloc; libraryLibJITImp;
function jit_realloc; libraryLibJITImp;
procedure jit_free; libraryLibJITImp;
function jit_memset; libraryLibJITImp;
function jit_memcpy; libraryLibJITImp;
function jit_memmove; libraryLibJITImp;
function jit_memcmp; libraryLibJITImp;
function jit_memchr; libraryLibJITImp;
function jit_strlen; libraryLibJITImp;
function jit_strcpy; libraryLibJITImp;
function jit_strcat; libraryLibJITImp;
function jit_strncpy; libraryLibJITImp;
function jit_strdup; libraryLibJITImp;
function jit_strndup; libraryLibJITImp;
function jit_strcmp; libraryLibJITImp;
function jit_strncmp; libraryLibJITImp;
function jit_stricmp; libraryLibJITImp;
function jit_strnicmp; libraryLibJITImp;
function jit_strchr; libraryLibJITImp;
function jit_strrchr; libraryLibJITImp;
{$IFNDEF MSWINDOWS}
function jit_sprintf; libraryLibJITImp;
function jit_snprintf; libraryLibJITImp;
{$ENDIF}
function jit_value_create; libraryLibJITImp;
function jit_value_create_nint_constant; libraryLibJITImp;
function jit_value_create_long_constant; libraryLibJITImp;
function jit_value_create_float32_constant; libraryLibJITImp;
function jit_value_create_float64_constant; libraryLibJITImp;
function jit_value_create_nfloat_constant; libraryLibJITImp;
function jit_value_create_constant; libraryLibJITImp;
function jit_value_get_param; libraryLibJITImp;
function jit_value_get_struct_pointer; libraryLibJITImp;
function jit_value_is_temporary; libraryLibJITImp;
function jit_value_is_local; libraryLibJITImp;
function jit_value_is_constant; libraryLibJITImp;
function jit_value_is_parameter; libraryLibJITImp;
procedure jit_value_ref; libraryLibJITImp;
procedure jit_value_set_volatile; libraryLibJITImp;
function jit_value_is_volatile; libraryLibJITImp;
procedure jit_value_set_addressable; libraryLibJITImp;
function jit_value_is_addressable; libraryLibJITImp;
function jit_value_get_type; libraryLibJITImp;
function jit_value_get_function; libraryLibJITImp;
function jit_value_get_block; libraryLibJITImp;
function jit_value_get_context; libraryLibJITImp;
function jit_value_get_constant; libraryLibJITImp;
function jit_value_get_nint_constant; libraryLibJITImp;
function jit_value_get_long_constant; libraryLibJITImp;
function jit_value_get_float32_constant; libraryLibJITImp;
function jit_value_get_float64_constant; libraryLibJITImp;
function jit_value_get_nfloat_constant; libraryLibJITImp;
function jit_value_is_true; libraryLibJITImp;
function jit_constant_convert; libraryLibJITImp;
procedure jit_vmem_init; libraryLibJITImp;
function jit_vmem_page_size; libraryLibJITImp;
function jit_vmem_round_up; libraryLibJITImp;
function jit_vmem_round_down; libraryLibJITImp;
function jit_vmem_reserve; libraryLibJITImp;
function jit_vmem_reserve_committed; libraryLibJITImp;
function jit_vmem_release; libraryLibJITImp;
function jit_vmem_commit; libraryLibJITImp;
function jit_vmem_decommit; libraryLibJITImp;
function jit_vmem_protect; libraryLibJITImp;
function jit_default_memory_manager; libraryLibJITImp;
procedure jit_context_set_memory_manager; libraryLibJITImp;
procedure jit_dump_type; libraryLibJITImp;
procedure jit_dump_value; libraryLibJITImp;
procedure jit_dump_insn; libraryLibJITImp;
procedure jit_dump_function; libraryLibJITImp;


end.
