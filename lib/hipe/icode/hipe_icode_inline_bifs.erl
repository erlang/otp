%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File    : hipe_icode_inline_bifs.erl
%% Author  : Per Gustafsson <pergu@it.uu.se>
%% Purpose : Inlines BIFs which can be expressed easily in ICode.
%%           This allows for optimizations in later ICode passes
%%           and makes the code faster.
%%                   
%% Created : 14 May 2007 by Per Gustafsson <pergu@it.uu.se>
%%--------------------------------------------------------------------

%% Currently inlined BIFs:
%% and, or, xor, not, <, >, >=, =<, ==, /=, =/=, =:=
%% is_atom, is_boolean, is_binary, is_float, is_function,
%% is_integer, is_list, is_pid, is_port, is_reference, is_tuple

-module(hipe_icode_inline_bifs).

-export([cfg/1]).

-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").

%%--------------------------------------------------------------------

-spec cfg(#cfg{}) -> #cfg{}.

cfg(Cfg) ->
  Linear =  hipe_icode_cfg:cfg_to_linear(Cfg),
  #icode{code = StraightCode} = Linear,
  FinalCode = lists:flatten([inline_bif(I) || I <- StraightCode]),
  Cfg1 = hipe_icode_cfg:linear_to_cfg(Linear#icode{code = FinalCode}),
  hipe_icode_cfg:remove_unreachable_code(Cfg1).

inline_bif(I = #icode_call{}) ->
  try_conditional(I);
inline_bif(I) ->
  I.

try_conditional(I = #icode_call{dstlist = [Dst], 'fun' = {erlang, Name, 2},
				args = [Arg1, Arg2],
				continuation = Cont}) ->
  case is_conditional(Name) of
    true ->
      inline_conditional(Dst, Name, Arg1, Arg2, Cont);
    false ->
      try_bool(I)
  end;
try_conditional(I) ->
  try_bool(I).

is_conditional(Name) ->
  case Name of
    '=:=' -> true;
    '=/=' -> true;
    '=='  -> true;
    '/='  -> true;
    '>'   -> true;
    '<'   -> true;
    '>='  -> true;
    '=<'  -> true;
    _     -> false
  end.

try_bool(I = #icode_call{dstlist = [Dst], 'fun' = Name,
			 args = [Arg1, Arg2],
			 continuation = Cont, fail_label = Fail}) ->
  case is_binary_bool(Name) of
    {true, Results, ResLbls} ->
      inline_binary_bool(Dst, Results, ResLbls, Arg1, Arg2, Cont, Fail, I); 
    false ->
      try_type_tests(I)
  end;
try_bool(I = #icode_call{dstlist = [Dst], 'fun' = {erlang, 'not', 1},
			 args = [Arg1],
			 continuation = Cont,
			 fail_label = Fail}) ->
  inline_unary_bool(Dst, {false, true}, Arg1, Cont, Fail, I);
try_bool(I) -> try_type_tests(I).

is_binary_bool({erlang, Name, 2}) -> 
  ResTLbl = hipe_icode:mk_new_label(),
  ResFLbl = hipe_icode:mk_new_label(),
  ResTL = hipe_icode:label_name(ResTLbl),
  ResFL = hipe_icode:label_name(ResFLbl),
  case Name of
    'and' -> {true, {ResTL, ResFL, ResFL}, {ResTLbl, ResFLbl}};
    'or' -> {true, {ResTL, ResTL, ResFL}, {ResTLbl, ResFLbl}};
    'xor' -> {true, {ResFL, ResTL, ResFL}, {ResTLbl, ResFLbl}};
    _ -> false
  end;
is_binary_bool(_) -> false.

try_type_tests(I = #icode_call{dstlist=[Dst], 'fun' = {erlang, Name, 1},
			       args = Args, continuation = Cont}) ->
  case is_type_test(Name) of
    {true, Type} ->
      inline_type_test(Dst, Type, Args, Cont);
    false ->
      I
  end;
try_type_tests(I) -> I.

is_type_test(Name) ->
  case Name of
    is_integer -> {true, integer};
    is_float -> {true, float};
    is_tuple -> {true, tuple};
    is_binary -> {true, binary};	       
    is_list -> {true, list};
    is_pid -> {true, pid};
    is_atom -> {true, atom};
    is_boolean -> {true, boolean};
    is_function -> {true, function};	       
    is_reference -> {true, reference};
    is_port -> {true, port};
    _ -> false
  end.
 
inline_type_test(BifRes, Type, Src, Cont) ->
  {NewCont, NewEnd} = get_cont_lbl(Cont),
  TLbl  = hipe_icode:mk_new_label(),
  FLbl = hipe_icode:mk_new_label(),
  TL  = hipe_icode:label_name(TLbl),
  FL = hipe_icode:label_name(FLbl),
  [hipe_icode:mk_type(Src, Type, TL, FL),
   TLbl,
   hipe_icode:mk_move(BifRes, hipe_icode:mk_const(true)),
   hipe_icode:mk_goto(NewCont),
   FLbl,
   hipe_icode:mk_move(BifRes, hipe_icode:mk_const(false)),
   hipe_icode:mk_goto(NewCont)|
   NewEnd].  

inline_conditional(BifRes, Op, Src1, Src2, Cont) ->
  {NewCont, NewEnd} = get_cont_lbl(Cont),
  TLbl  = hipe_icode:mk_new_label(),
  FLbl = hipe_icode:mk_new_label(),
  TL  = hipe_icode:label_name(TLbl),
  FL = hipe_icode:label_name(FLbl),
  [hipe_icode:mk_if(Op, [Src1, Src2], TL, FL),
   TLbl,
   hipe_icode:mk_move(BifRes, hipe_icode:mk_const(true)),
   hipe_icode:mk_goto(NewCont),
   FLbl,
   hipe_icode:mk_move(BifRes, hipe_icode:mk_const(false)),
   hipe_icode:mk_goto(NewCont)|
   NewEnd].

%%
%% The TTL TFL FFL labelnames points to either ResTLbl or ResFLbl
%% Depending on what boolean expression we are inlining
%%

inline_binary_bool(Dst, {TTL, TFL, FFL}, {ResTLbl, ResFLbl},
		   Arg1, Arg2, Cont, Fail, I) ->
  {NewCont, NewEnd} = get_cont_lbl(Cont),
  {NewFail, FailCode} = get_fail_lbl(Fail, I), 
  EndCode = FailCode++NewEnd,
  TLbl = hipe_icode:mk_new_label(),
  FLbl = hipe_icode:mk_new_label(),
  NotTLbl = hipe_icode:mk_new_label(),
  NotTTLbl = hipe_icode:mk_new_label(),
  NotTFLbl = hipe_icode:mk_new_label(),
  TL = hipe_icode:label_name(TLbl),
  FL = hipe_icode:label_name(FLbl),
  NotTL = hipe_icode:label_name(NotTLbl),
  NotTTL = hipe_icode:label_name(NotTTLbl),
  NotTFL = hipe_icode:label_name(NotTFLbl),
  [hipe_icode:mk_type([Arg1], {atom, true}, TL, NotTL, 0.5),
   NotTLbl,
   hipe_icode:mk_type([Arg1], {atom, false}, FL, NewFail, 0.99),
   TLbl,
   hipe_icode:mk_type([Arg2], {atom, true}, TTL, NotTTL, 0.5),
   NotTTLbl,
   hipe_icode:mk_type([Arg2], {atom, false}, TFL, NewFail, 0.99),
   FLbl,
   hipe_icode:mk_type([Arg2], {atom, true}, TFL, NotTFL, 0.5),
   NotTFLbl,
   hipe_icode:mk_type([Arg2], {atom, false}, FFL, NewFail, 0.99),
   ResTLbl,
   hipe_icode:mk_move(Dst, hipe_icode:mk_const(true)),
   hipe_icode:mk_goto(NewCont),
   ResFLbl,
   hipe_icode:mk_move(Dst, hipe_icode:mk_const(false)),
   hipe_icode:mk_goto(NewCont)|
   EndCode].
   
inline_unary_bool(Dst, {T,F}, Arg1, Cont, Fail, I) ->
  TLbl  = hipe_icode:mk_new_label(),
  NotTLbl  = hipe_icode:mk_new_label(),
  FLbl = hipe_icode:mk_new_label(),
  TL  = hipe_icode:label_name(TLbl),
  NotTL = hipe_icode:label_name(NotTLbl),
  FL = hipe_icode:label_name(FLbl),
  {NewCont, NewEnd} = get_cont_lbl(Cont),
  {NewFail, FailCode} = get_fail_lbl(Fail, I),
  EndCode = FailCode ++ NewEnd,
  Arg1L = [Arg1],
  [hipe_icode:mk_type(Arg1L, {atom, true}, TL, NotTL, 0.5),
   NotTLbl,
   hipe_icode:mk_type(Arg1L, {atom, false}, FL, NewFail, 0.99),
   TLbl,
   hipe_icode:mk_move(Dst, hipe_icode:mk_const(T)),
   hipe_icode:mk_goto(NewCont),
   FLbl,
   hipe_icode:mk_move(Dst, hipe_icode:mk_const(F)),
   hipe_icode:mk_goto(NewCont)|
   EndCode].

get_cont_lbl([]) ->
  NL = hipe_icode:mk_new_label(),
  {hipe_icode:label_name(NL), [NL]};
get_cont_lbl(Cont) ->
  {Cont, []}.

get_fail_lbl([], I) ->
  NL = hipe_icode:mk_new_label(),
  {hipe_icode:label_name(NL), [NL, I]};
get_fail_lbl(Fail, _) ->
  {Fail, []}.
