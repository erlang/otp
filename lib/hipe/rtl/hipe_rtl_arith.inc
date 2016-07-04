%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
%% File    : hipe_rtl_arith.inc
%% Created : Feb 2004
%% Purpose : Implements arithmetic which is parameterized by the size
%%           of the word of the target architecture (given as defines).
%%----------------------------------------------------------------------


%% Returns a tuple
%%  {Res, Sign, Zero, Overflow, Carry}
%%  Res will be a number in the range 
%%   MAX_UNSIGNED_INT >= Res >= 0
%% The other four values are flags that are either true or false
%% 
eval_alu(Op, Arg1, Arg2) 
  when Arg1 =< ?MAX_UNSIGNED_INT,
       Arg1 >= ?MIN_SIGNED_INT,
       Arg2 =< ?MAX_UNSIGNED_INT,
       Arg2 >= ?MIN_SIGNED_INT ->

  Sign1 = sign_bit(Arg1),
  Sign2 = sign_bit(Arg2),

  case Op of
    'sub' ->
      Res = (Arg1 - Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (Sign1 andalso (not Sign2) andalso (not N)) 
	or
          ((not Sign1) andalso Sign2 andalso N),
      C = ((not Sign1) andalso Sign2) 
	or 
	  (N andalso ((not Sign1) orelse Sign2)),
     {Res, N, Z, V, C};
    'add' ->
      Res = (Arg1 + Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (Sign1 andalso Sign2 andalso (not N)) 
	or
          ((not Sign1) andalso (not Sign2) andalso N),
      C = (Sign1 andalso Sign2)
	or 
	  ((not N) andalso (Sign1 orelse Sign2)),
     {Res, N, Z, V, C};
    'mul' ->
      FullRes = Arg1 * Arg2,
      Res = FullRes band ?WORDMASK,
      ResHi = FullRes bsr ?BITS,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (N andalso (ResHi =/= -1)) orelse ((not N) andalso (ResHi =/= 0)),
      C = V,
     {Res, N, Z, V, C};
    'sra' ->
      Res = (Arg1 bsr Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),    
      V = 0,
      C = 0,
     {Res, N, Z, V, C};
    'srl' ->
      Res = (Arg1 bsr Arg2) band shiftmask(Arg2),
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0,
     {Res, N, Z, V, C};
    'sll' ->
      Res = (Arg1 bsl Arg2) band ?WORDMASK, 
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0,
     {Res, N, Z, V, C};
    'or' ->
      Res = (Arg1 bor Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0,
     {Res, N, Z, V, C};
    'and' ->
      Res = (Arg1 band Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0,
     {Res, N, Z, V, C};
    'xor' ->
      Res = (Arg1 bxor Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0,
     {Res, N, Z, V, C};
    Op ->
      ?EXIT({"unknown alu op", Op})
  end;
eval_alu(Op, Arg1, Arg2) ->
  ?EXIT({argument_overflow, Op, Arg1, Arg2}).

%% Björn & Bjarni:
%% We need to be able to do evaluations based only on the bits, since
%% there are cases where we can evaluate a subset of the bits, but can
%% not do a full eval-alub call (eg. a + 0 gives no carry)
%%
-spec eval_cond_bits(hipe_rtl:alub_cond(), boolean(),
		     boolean(), boolean(), boolean()) -> boolean().

eval_cond_bits(Cond, N, Z, V, C) ->
  case Cond of
    'eq' ->
      Z;
    'ne' -> 
      not Z;
    'gt' ->
      not (Z orelse (N xor V));
    'gtu' -> 
      not (C orelse Z);
    'ge' -> 
      not (N xor V);
    'geu'-> 
      not C;
    'lt' ->
      N xor V;
    'ltu'->
      C;
    'le' ->
      Z orelse (N xor V);
    'leu'->
      C orelse Z;
    'overflow' ->
      V;
    'not_overflow' ->
      not V
  end.

eval_alub(Op, Cond, Arg1, Arg2) ->
  {Res, N, Z, V, C} = eval_alu(Op, Arg1, Arg2),
  {Res, eval_cond_bits(Cond, N, Z, V, C)}.

eval_cond(Cond, Arg1, Arg2) ->
  {_, Bool} = eval_alub('sub', Cond, Arg1, Arg2),
  Bool.

sign_bit(Val) ->
  ((Val bsr ?SIGN_BIT) band 1) =:= 1.

shiftmask(Arg) ->
  Setbits = ?BITS - Arg,
  (1 bsl Setbits) - 1.

zero(Val) ->
  Val =:= 0.
