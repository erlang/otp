%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
%%   MAX_SIGNED_INT >= Res >= MIN_SIGNED_INT
%% The other four values are flags that are either true or false
%% 
eval_alu(Op, Arg1, Arg2) 
  when Arg1 =< ?MAX_SIGNED_INT, 
       Arg1 >= ?MIN_SIGNED_INT,
       Arg2 =< ?MAX_SIGNED_INT, 
       Arg2 >= ?MIN_SIGNED_INT ->

  Sign1 = sign_bit(Arg1),
  Sign2 = sign_bit(Arg2),

  case Op of
    'sub' ->
      Res = (Arg1 - Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (Sign1 and (not Sign2) and (not N)) 
	or
          ((not Sign1) and Sign2 and N),
      C = ((not Sign1) and Sign2) 
	or 
	  (N and ((not Sign1) or Sign2));
    'add' ->
      Res = (Arg1 + Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (Sign1 and Sign2 and (not N)) 
	or
          ((not Sign1) and (not Sign2) and N),
      C = (Sign1 and Sign2)
	or 
	  ((not N) and (Sign1 or Sign2));
    'mul' ->
      FullRes = Arg1 * Arg2,
      Res = FullRes band ?WORDMASK,
      ResHi = FullRes bsr ?BITS,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (N and (ResHi =/= -1)) or ((not N) and (ResHi =/= 0)),
      C = V;
    'sra' ->
      Res = (Arg1 bsr Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),    
      V = 0,
      C = 0;
    'srl' ->
      Res = (Arg1 bsr Arg2) band shiftmask(Arg2),
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'sll' ->
      Res = (Arg1 bsl Arg2) band ?WORDMASK, 
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'or' ->
      Res = (Arg1 bor Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'and' ->
      Res = (Arg1 band Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'xor' ->
      Res = (Arg1 bxor Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    Op ->
      Res = N = Z = V = C = 0,
      ?EXIT({"unknown alu op", Op})
  end,
  {two_comp_to_erl(Res), N, Z, V, C};
eval_alu(Op, Arg1, Arg2) ->
  ?EXIT({argument_overflow,Op,Arg1,Arg2}).

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
      not (Z or (N xor V));
    'gtu' -> 
      not (C or Z);
    'ge' -> 
      not (N xor V);
    'geu'-> 
      not C;
    'lt' ->
      N xor V;
    'ltu'->
      C;
    'le' ->
      Z or (N xor V);
    'leu'->
      C or Z;
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

two_comp_to_erl(V) ->
  if V > ?MAX_SIGNED_INT ->
      - ((?MAX_UNSIGNED_INT + 1) - V);
     true -> V
  end.

shiftmask(Arg) ->
  Setbits = ?BITS - Arg,
  (1 bsl Setbits) - 1.

zero(Val) ->
  Val =:= 0.

