(*
Tutorial 1 - mul_add

Builds and compiles the following function:

int mul_add(int x, int y, int z)
{
  return x * y + z;
}
*)
{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$PACKRECORDS C}
{$ENDIF}
program t1;

uses
  SysUtils, Classes,
  libjit in 'libjit.pas',
  libjit_types in 'libjit_types.pas';


var
  context: jit_context_t;
  params: array [0..2] of jit_type_t;
  signature: jit_type_t;
  function_: jit_function_t;
  x, y, z: jit_value_t;
  temp1, temp2: jit_value_t;
  arg1, arg2, arg3: jit_int;
  args: array [0..2] of Pointer;
  res: jit_int;
begin
  if (jit_type_int = nil) then raise Exception.Create('fuuuuu (0)');
  //if (PPointer(jit_type_int)^ = nil) then raise Exception.Create('fuuuuu (1)');

  (* Create a context to hold the JIT's primary state *)
  writeln('creating context...');
  context := jit_context_create();
  if (context = nil) then raise Exception.Create('cannot create jit context');

  (* Lock the context while we build and compile the function *)
  writeln('starting builder...');
  jit_context_build_start(context);

  (* Build the function signature *)
  writeln('creating signature...');
  params[0] := jit_type_int;
  params[1] := jit_type_int;
  params[2] := jit_type_int;
  signature := jit_type_create_signature(jit_abi_cdecl, jit_type_int, @params[0], 3, 1);

  (* Create the function object *)
  writeln('creating function object...');
  function_ := jit_function_create(context, signature);
  writeln('freeing signature...');
  jit_type_free(signature);

  (* Construct the function body *)
  writeln('creating function body...');
  x := jit_value_get_param(function_, 0);
  y := jit_value_get_param(function_, 1);
  z := jit_value_get_param(function_, 2);
  temp1 := jit_insn_mul(function_, x, y);
  temp2 := jit_insn_add(function_, temp1, z);
  jit_insn_return(function_, temp2);

  (* Compile the function *)
  writeln('compiling function...');
  jit_function_compile(function_);

  (* Unlock the context *)
  writeln('finalizing builder...');
  jit_context_build_end(context);

  (* Execute the function and print the result *)
  writeln('executing function...');
  arg1 := 3;
  arg2 := 5;
  arg3 := 2;
  args[0] := @arg1;
  args[1] := @arg2;
  args[2] := @arg3;
  jit_function_apply(function_, @args[0], @res);
  writeln('mul_add(3, 5, 2) = ', Integer(res));

  (* Clean up *)
  jit_context_destroy(context);
end.
