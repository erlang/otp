%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_rtl_exceptions.erl
%%  Module   :	hipe_rtl_exceptions
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@it.uu.se): 
%%               Created.
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_exceptions).

-export([gen_fail/3, gen_begin_handler/3]).

-include("../main/hipe.hrl").
-include("hipe_literals.hrl").

%% --------------------------------------------------------------------
%% Handle the Icode instruction
%% FAIL
%%
gen_fail(Class, Args, L) ->
  case Args of
    [Reason] ->
      case Class of
	exit -> 
	  gen_exit(Reason, L);
	throw ->
	  gen_throw(Reason, L);
	error ->
	  gen_error(Reason, L)
      end;
    [Arg1,Arg2] ->
      case Class of
 	error ->
	  Reason = Arg1, ArgList = Arg2,
	  gen_error(Reason, ArgList, L);
	rethrow ->
	  Exception = Arg1, Reason = Arg2,
	  gen_rethrow(Exception, Reason, L)
      end
  end.

%% --------------------------------------------------------------------
%% Exception handler glue; interfaces between the runtime system's
%% exception state and the Icode view of exception handling.

gen_begin_handler(I, VarMap, ConstTab) ->
  Ds = hipe_icode:begin_handler_dstlist(I),
  {Vars, VarMap1} = hipe_rtl_varmap:ivs2rvs(Ds, VarMap),
  [FTagVar,FValueVar,FTraceVar] = Vars,
  {[hipe_rtl:mk_comment('begin_handler'),
    hipe_rtl_arch:pcb_load(FValueVar, ?P_FVALUE),
    hipe_rtl_arch:pcb_load(FTraceVar, ?P_FTRACE),
    %% synthesized from P->freason by hipe_handle_exception()
    hipe_rtl_arch:pcb_load(FTagVar, ?P_ARG0)
   ], 
   VarMap1, ConstTab}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      
%% Exceptions

gen_exit(Reason, L) ->
  gen_fail_call({erlang,exit,1}, [Reason], L).

gen_throw(Reason, L) ->
  gen_fail_call({erlang,throw,1}, [Reason], L).

gen_error(Reason, L) ->
  gen_fail_call({erlang,error,1}, [Reason], L).

gen_error(Reason, ArgList, L) ->
  gen_fail_call({erlang,error,2}, [Reason,ArgList], L).

gen_rethrow(Exception, Reason, L) ->
  gen_fail_call(rethrow, [Exception,Reason], L).

%% Generic fail. We can't use 'enter' with a fail label (there can be no
%% stack descriptor info for an enter), so for a non-nil fail label we
%% generate a call followed by a dummy return.
%%
%% Update: The runtime system now interprets the return address of
%% the BIF call in order to list the invoking MFA in the stack trace.
%% Generating tailcalls here defeats that purpose, so we no longer do that.

%%gen_fail_call(Fun, Args, []) ->
%%  [hipe_rtl:mk_enter(Fun, Args, remote)];
gen_fail_call(Fun, Args, L) ->
  ContLbl = hipe_rtl:mk_new_label(),
  Cont = hipe_rtl:label_name(ContLbl),
  Zero = hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0)),
  [hipe_rtl:mk_call([], Fun, Args, Cont, L, remote),
   ContLbl,
   hipe_rtl:mk_return([Zero])].
