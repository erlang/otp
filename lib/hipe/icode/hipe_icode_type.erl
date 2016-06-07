%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%%--------------------------------------------------------------------
%%% File    : hipe_icode_type.erl
%%% Author  : Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%% Description : Propagate type information.
%%%
%%% Created : 25 Feb 2003 by Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%%--------------------------------------------------------------------

-module(hipe_icode_type).

-export([cfg/4, unannotate_cfg/1, specialize/1]).

%%=====================================================================
%% Icode Coordinator Callbacks
%%=====================================================================

-export([replace_nones/1,
	 update__info/2, new__info/1, return__info/1,
	 return_none/0, return_none_args/2, return_any_args/2]).

%%=====================================================================

-include("../main/hipe.hrl").
-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").
-include("hipe_icode_type.hrl").
-include("../flow/cfg.hrl").

-type args_fun()  :: fun((mfa(), cfg()) -> [erl_types:erl_type()]).
-type call_fun()  :: fun((mfa(), [_]) -> erl_types:erl_type()).
-type final_fun() :: fun((mfa(), [_]) -> 'ok').
-type data()	  :: {mfa(), args_fun(), call_fun(), final_fun()}.

%-define(DO_HIPE_ICODE_TYPE_TEST, false).

-ifdef(DO_HIPE_ICODE_TYPE_TEST).
-export([test/0]).
-endif.

-define(MFA_debug, fun(_, _, _) -> ok end).

%-define(debug, fun(X, Y) -> io:format("~s ~p~n", [X, Y]) end).
-define(debug, fun(_, _) -> ok end).

%-define(flow_debug, fun(X, Y) -> io:format("flow: ~s ~p~n", [X, Y]) end).
-define(flow_debug, fun(_, _) -> ok end).

%-define(widening_debug, fun(X, Y) -> io:format("wid: ~s ~p~n", [X, Y]) end).
-define(widening_debug, fun(_, _) -> ok end).

%-define(call_debug, fun(X, Y) -> io:format("call: ~s ~p~n", [X, Y]) end).
-define(call_debug, fun(_, _) -> ok end).

%-define(ineq_debug, fun(X, Y) -> io:format("ineq: ~s ~p~n", [X, Y]) end).
-define(ineq_debug, fun(_, _) -> ok end).

%-define(server_debug, fun(X, Y) -> io:format("~p server: ~s ~p~n", [self(), X, Y]) end).
-define(server_debug, fun(_, _) -> ok end).

-import(erl_types, [number_min/1, number_max/1,
		    t_any/0, t_atom/1, t_atom/0, t_atom_vals/1,
		    t_binary/0, t_bitstr/0, t_bitstr_base/1, t_bitstr_unit/1, 
		    t_boolean/0, t_cons/0,
		    t_float/0, t_from_term/1, t_from_range/2,
		    t_fun/0, t_fun/1, t_fun/2, t_fun_args/1, t_fun_arity/1, 
		    t_inf/2, t_inf_lists/2, t_integer/0,
		    t_integer/1, t_is_atom/1, t_is_any/1,
		    t_is_binary/1, t_is_bitstr/1, t_is_bitwidth/1,
		    t_is_boolean/1, t_is_fixnum/1, t_is_cons/1, t_is_map/1,
		    t_is_maybe_improper_list/1, t_is_equal/2, t_is_float/1,
		    t_is_fun/1, t_is_integer/1, t_is_non_neg_integer/1, 
		    t_is_number/1, t_is_matchstate/1,
		    t_is_none/1, t_is_port/1, t_is_pid/1,
		    t_is_reference/1, t_is_subtype/2, t_is_tuple/1,
		    t_limit/2, t_matchstate_present/1, t_matchstate/0, 
		    t_matchstate_slots/1, t_maybe_improper_list/0, t_map/0,
		    t_nil/0, t_none/0, t_number/0, t_number/1, t_number_vals/1,
		    t_pid/0, t_port/0, t_reference/0, t_subtract/2, t_sup/2,
		    t_to_tlist/1, t_tuple/0, t_tuple/1, t_tuple_sizes/1]).

-record(state, {info_map  = gb_trees:empty() :: gb_trees:tree(),
		cfg                          :: cfg(),
		liveness  = gb_trees:empty() :: gb_trees:tree(),
		arg_types                    :: [erl_types:erl_type()],
		ret_type  = [t_none()]       :: [erl_types:erl_type()],
		lookupfun                    :: call_fun(),
		resultaction                 :: final_fun()}).
-type state() :: #state{}.

%%-----------------------------------------------------------------------
%% The main exported function
%%-----------------------------------------------------------------------

-spec cfg(cfg(), mfa(), comp_options(), #comp_servers{}) -> cfg().

cfg(Cfg, MFA, Options, Servers) ->
  case proplists:get_bool(concurrent_comp, Options) of
    true ->
      concurrent_cfg(Cfg, MFA, Servers#comp_servers.type);
    false ->
      ordinary_cfg(Cfg, MFA)
  end.

concurrent_cfg(Cfg, MFA, CompServer) ->
  CompServer ! {ready, {MFA, self()}},
  {ArgsFun, CallFun, FinalFun} = do_analysis(Cfg, MFA),
  Ans = do_rewrite(Cfg, MFA, ArgsFun, CallFun, FinalFun),
  CompServer ! {done_rewrite, MFA},
  Ans.

do_analysis(Cfg, MFA) ->
  receive
    {analyse, {ArgsFun,CallFun,FinalFun}} ->
      analyse(Cfg, {MFA,ArgsFun,CallFun,FinalFun}),
      do_analysis(Cfg, MFA);
    {done, {_NewArgsFun,_NewCallFun,_NewFinalFun} = Done} ->
      Done
  end.

do_rewrite(Cfg, MFA, ArgsFun, CallFun, FinalFun) ->
  common_rewrite(Cfg, {MFA,ArgsFun,CallFun,FinalFun}).
 
ordinary_cfg(Cfg, MFA) ->
  Data = make_data(Cfg,MFA),
  common_rewrite(Cfg, Data).
  
common_rewrite(Cfg, Data) ->
  State = safe_analyse(Cfg, Data),
  NewState = simplify_controlflow(State),  
  NewCfg = state__cfg(annotate_cfg(NewState)),
  hipe_icode_cfg:remove_unreachable_code(specialize(NewCfg)).

make_data(Cfg, {_M,_F,A}=MFA) ->
  NoArgs = 
    case hipe_icode_cfg:is_closure(Cfg) of
      true -> hipe_icode_cfg:closure_arity(Cfg);
      false -> A
    end,
  Args = lists:duplicate(NoArgs, t_any()), 
  ArgsFun = fun(_,_) -> Args end,
  CallFun = fun(_,_) -> t_any() end,
  FinalFun = fun(_,_) -> ok end,
  {MFA,ArgsFun,CallFun,FinalFun}.

%%debug_make_data(Cfg, {_M,_F,A}=MFA) ->
%%  NoArgs = 
%%    case hipe_icode_cfg:is_closure(Cfg) of
%%      true -> hipe_icode_cfg:closure_arity(Cfg);
%%      false -> A
%%    end,
%%  Args = lists:duplicate(NoArgs, t_any()), 
%%  ArgsFun = fun(MFA,_Cfg) -> io:format("Start:~p~n",[MFA]),Args end,
%%  CallFun = fun(MFA,Types) -> io:format("Call With:~p~nTo:~p~n",[Types,MFA]), t_any() end,
%%  FinalFun = fun(MFA,Type) -> io:format("ResType:~p~nFor:~p~n",[Type,MFA]),ok end,
%%  {MFA,ArgsFun,CallFun,FinalFun}.


%%-------------------------------------------------------------------
%% Global type analysis on the whole function. Demands that the code
%% is in SSA form. When we encounter a phi node, the types of the
%% arguments are joined. At the end of a block the information out is
%% joined with the current information in for all _valid_ successors,
%% that is, of all successors that actually can be reached. If the
%% join produces new information in for the successor, this
%% information is added to the worklist.
%%-------------------------------------------------------------------

-spec analyse(cfg(), data()) -> 'ok'.

analyse(Cfg, Data) ->
  try
    #state{} = safe_analyse(Cfg, Data),
    ok
  catch throw:no_input -> ok % No need to do anything since we have no input
  end.

-spec safe_analyse(cfg(), data()) -> state().

safe_analyse(Cfg, {MFA,_,_,_}=Data) ->
  State = new_state(Cfg, Data),
  NewState = analyse_blocks(State,MFA),
  (state__resultaction(NewState))(MFA,state__ret_type(NewState)),
  NewState.

analyse_blocks(State, MFA) ->
  Work = init_work(State),
  analyse_blocks(Work, State, MFA).

analyse_blocks(Work, State, MFA) ->
  case get_work(Work) of
    fixpoint ->
      State;
    {Label, NewWork} ->
      Info = state__info_in(State, Label),
      {NewState, NewLabels}  =
	try analyse_block(Label, Info, State)
	catch throw:none_type ->
	    %% io:format("received none type at label: ~p~n", [Label]),
	    {State,[]}
	end,
      NewWork2 = add_work(NewWork, NewLabels),
      analyse_blocks(NewWork2, NewState, MFA)
  end.
  
analyse_block(Label, InfoIn, State) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:butlast(BB),
  Last = hipe_bb:last(BB),
  InfoOut = analyse_insns(Code, InfoIn, state__lookupfun(State)),
  NewState = state__info_out_update(State, Label, InfoOut),
  case Last of
    #icode_if{} ->
      UpdateInfo = do_if(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #icode_type{} ->
      UpdateInfo = do_type(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #icode_switch_tuple_arity{} ->
      UpdateInfo = do_switch_tuple_arity(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #icode_switch_val{} ->
      UpdateInfo = do_switch_val(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #icode_enter{} ->
      NewState1 = do_enter(Last, InfoOut, NewState, state__lookupfun(NewState)),
      do_updates(NewState1,[]);
    #icode_call{} ->
      {NewState1,UpdateInfo} = do_last_call(Last, InfoOut, NewState, Label),
      do_updates(NewState1, UpdateInfo);
    #icode_return{} ->
      NewState1 = do_return(Last, InfoOut, NewState),
      do_updates(NewState1,[]);
    _ ->
      UpdateInfo = [{X, InfoOut} || X <- state__succ(NewState, Label)],
      do_updates(NewState, UpdateInfo)
  end.

analyse_insns([I|Insns], Info, LookupFun) ->
  NewInfo = analyse_insn(I, Info, LookupFun),
  analyse_insns(Insns, NewInfo, LookupFun);
analyse_insns([], Info, _) ->
  Info.

analyse_insn(I, Info, LookupFun) ->
  case I of
    #icode_move{} ->
      do_move(I, Info);
    #icode_call{} ->
      NewInfo = do_call(I, Info, LookupFun),
      %% io:format("Analysing Call: ~w~n~w~n", [I, NewInfo]),
      update_call_arguments(I, NewInfo);
    #icode_phi{} ->
      Type = t_limit(join_list(hipe_icode:args(I), Info), ?TYPE_DEPTH),
      enter_defines(I, Type, Info);
    #icode_begin_handler{} ->
      enter_defines(I, t_any(), Info);
    _ ->
      %% Just an assert
      case defines(I) of
	[] -> Info;
	_ -> exit({"Instruction with destination not analysed", I})
      end
  end.

do_move(I, Info) ->
  %% Can't use uses/1 since we must keep constants.
  [Src] = hipe_icode:args(I),
  enter_defines(I, lookup(Src, Info), Info).

do_basic_call(I, Info, LookupFun) -> 
    case hipe_icode:call_type(I) of
      primop ->
	Fun = hipe_icode:call_fun(I),
	ArgTypes = lookup_list(hipe_icode:args(I), Info),
	primop_type(Fun, ArgTypes);      
      remote ->
	{M, F, A} = hipe_icode:call_fun(I),
	ArgTypes = lookup_list(hipe_icode:args(I), Info),
	None = t_none(),
	case erl_bif_types:type(M, F, A, ArgTypes) of
	  None ->
	    NewArgTypes = add_funs_to_arg_types(ArgTypes),
	    erl_bif_types:type(M, F, A, NewArgTypes);
	  Other ->
	    Other
	end;
      local ->
	MFA = hipe_icode:call_fun(I),
	ArgTypes = lookup_list(hipe_icode:args(I), Info),
	%% io:format("Call:~p~nTypes: ~p~n",[I,ArgTypes]),
	LookupFun(MFA,ArgTypes)
    end.

do_call(I, Info, LookupFun) ->
  RetType = do_basic_call(I, Info, LookupFun),
  IsNone = t_is_none(RetType),
  %% io:format("RetType ~p~nIsNone ~p~n~p~n",[RetType,IsNone,I]),
  if IsNone -> throw(none_type);
     true -> enter_defines(I, RetType, Info)
  end.

do_safe_call(I, Info, LookupFun) ->
  RetType = do_basic_call(I, Info, LookupFun),
  enter_defines(I, RetType, Info).

do_last_call(Last, InfoOut, State, Label) ->
  try 
    NewInfoOut = do_call(Last, InfoOut, state__lookupfun(State)),
    NewState = state__info_out_update(State, Label, NewInfoOut),
    ContInfo = update_call_arguments(Last, NewInfoOut),
    Cont = hipe_icode:call_continuation(Last),
    Fail = hipe_icode:call_fail_label(Last),
    ?call_debug("Continfo, NewInfoOut", {ContInfo, NewInfoOut}),
    UpdateInfo =
    case Fail of
      [] ->
	[{Cont, ContInfo}];
      _ ->
	case call_always_fails(Last, InfoOut) of
	  true ->
	    [{Fail, NewInfoOut}];
	  false ->
	    Fun = hipe_icode:call_fun(Last),
	    case hipe_icode_primops:fails(Fun) of
	      true ->
		[{Cont, ContInfo}, {Fail, NewInfoOut}];
	      false ->
		[{Cont, ContInfo}]
	    end
	end
    end,
    {NewState,UpdateInfo}
  catch throw:none_type ->
      State2 = state__info_out_update(State, Label, InfoOut),
      case hipe_icode:call_fail_label(Last) of
	[] -> throw(none_type);
	FailLbl ->
	  {State2,[{FailLbl, InfoOut}]}
      end
  end.

call_always_fails(#icode_call{} = I, Info) ->
  case hipe_icode:call_fun(I) of
    %% These can actually be calls too.
    {erlang, halt, 0} -> false;
    {erlang, halt, 1} -> false;
    {erlang, halt, 2} -> false;
    {erlang, exit, 1} -> false;
    {erlang, error, 1} -> false;
    {erlang, error, 2} -> false;
    {erlang, throw, 1} -> false;
    {erlang, hibernate, 3} -> false;
    Fun ->
      case hipe_icode:call_type(I) of
	primop ->
	  Args = safe_lookup_list(hipe_icode:call_args(I), Info),
	  ReturnType = primop_type(Fun, Args),
	  t_is_none(ReturnType);
	_ -> false
      end
  end.

do_enter(I, Info, State, LookupFun) ->
  %% io:format("Enter:~p~n",[I]),
  ArgTypes = lookup_list(hipe_icode:args(I), Info),
  RetTypes = 
    case hipe_icode:enter_type(I) of
      local ->
	MFA = hipe_icode:enter_fun(I),
	LookupFun(MFA,ArgTypes);
      remote ->
	{M, F, A} = hipe_icode:enter_fun(I),
	None = t_none(),
	case erl_bif_types:type(M, F, A, ArgTypes) of
	  None -> 
	    NewArgTypes = add_funs_to_arg_types(ArgTypes),
	    erl_bif_types:type(M, F, A, NewArgTypes);
	  Other ->
	    Other
	end;
      primop ->
	Fun = hipe_icode:enter_fun(I),
	primop_type(Fun, ArgTypes)             
    end,
  state__ret_type_update(State, RetTypes).

do_return(I, Info, State) ->
  RetTypes = lookup_list(hipe_icode:args(I), Info),
  state__ret_type_update(State, RetTypes).

do_if(I, Info) ->
  %% XXX: Could probably do better than this.
  TrueLab = hipe_icode:if_true_label(I),
  FalseLab = hipe_icode:if_false_label(I),
  case hipe_icode:if_args(I) of
    [Arg1, Arg2] = Args ->
      [Type1, Type2] = lookup_list(Args, Info),
      case t_is_none(Type1) orelse t_is_none(Type2) of
	true ->
	  [{TrueLab, Info}, {FalseLab, Info}];
	false ->
	  Inf = t_inf(Type1, Type2),
	  case hipe_icode:if_op(I) of
	    '=:='->
	      case t_is_none(Inf) of
		true ->
		  [{FalseLab, Info}];
		false ->
		  [{TrueLab, enter(Arg1, Inf, enter(Arg2, Inf, Info))}, 
		   {FalseLab, Info}]
	      end;
	    '=/=' ->
	      case t_is_none(Inf) of
		true -> 
		  [{TrueLab, Info}];
		false ->
		  [{FalseLab, enter(Arg1, Inf, enter(Arg2, Inf, Info))}, 
		   {TrueLab, Info}]
	      end;
	    '==' ->
	      [{TrueLab, Info}, {FalseLab, Info}];
	    '/=' ->
	      [{TrueLab, Info}, {FalseLab, Info}];
	    Op ->
	      integer_range_inequality_propagation(Op, Arg1, Arg2,
						   TrueLab, FalseLab, Info)
	      %%_ ->
	      %%  [{TrueLab, Info}, {FalseLab, Info}]
	  end
      end;
    _ ->
      %% Only care for binary if:s
      [{TrueLab, Info}, {FalseLab, Info}]
  end.

integer_range_inequality_propagation(Op, A1, A2, TrueLab, FalseLab, Info) ->
  Arg1 = lookup(A1, Info), 
  Arg2 = lookup(A2, Info), 
  ?ineq_debug("args", [Arg1,Arg2]),
  IntArg1 = t_inf(Arg1, t_integer()),
  IntArg2 = t_inf(Arg2, t_integer()),
  NonIntArg1 = t_subtract(Arg1, t_integer()),
  NonIntArg2 = t_subtract(Arg2, t_integer()),
  ?ineq_debug("nonintargs", [NonIntArg1,NonIntArg2]),
  case t_is_none(IntArg1) orelse t_is_none(IntArg2) of
    true ->
      ?ineq_debug("one is none", [IntArg1,IntArg2]),
      [{TrueLab, Info}, {FalseLab, Info}];
    false ->
      {TrueArg1, TrueArg2, FalseArg1, FalseArg2} =
	case Op of
	  '>=' ->
	    {FA1, FA2, TA1, TA2} = int_range_lt_propagator(IntArg1, IntArg2),
	    {TA1, TA2, FA1, FA2};
	  '>' ->
	    {TA2, TA1, FA2, FA1} = int_range_lt_propagator(IntArg2, IntArg1),
	    {TA1, TA2, FA1, FA2};
	  '<' ->
	    int_range_lt_propagator(IntArg1, IntArg2);
	  '=<' ->
	    {FA2, FA1, TA2, TA1} = int_range_lt_propagator(IntArg2, IntArg1),
	    {TA1, TA2, FA1, FA2}
	end,
      ?ineq_debug("int res", [TrueArg1, TrueArg2, FalseArg1, FalseArg2]),
      False = {FalseLab, enter(A1, t_sup(FalseArg1, NonIntArg1),
			       enter(A2, t_sup(FalseArg2, NonIntArg2), Info))},
      True = {TrueLab, enter(A1, t_sup(TrueArg1, NonIntArg1),
			     enter(A2, t_sup(TrueArg2, NonIntArg2), Info))},
      [True, False]
  end.

int_range_lt_propagator(IntArg1, IntArg2) ->
  Min1 = number_min(IntArg1),
  Max1 = number_max(IntArg1),
  Min2 = number_min(IntArg2),
  Max2 = number_max(IntArg2),
  %% is this the same as erl_types:t_subtract?? no ... ??
  TrueMax1 = erl_types:min(Max1, erl_bif_types:infinity_add(Max2, -1)),
  TrueMin2 = erl_types:max(erl_bif_types:infinity_add(Min1, 1), Min2),
  FalseMin1 = erl_types:max(Min1, Min2),
  FalseMax2 = erl_types:min(Max1, Max2),
  {t_from_range(Min1, TrueMax1),
   t_from_range(TrueMin2, Max2),
   t_from_range(FalseMin1, Max1),
   t_from_range(Min2, FalseMax2)}.

do_type(I, Info) ->
  case hipe_icode:args(I) of
    [Var] -> do_type(I, Info, Var);
    [Var1,Var2] -> do_type2(I, Info, Var1, Var2)
  end.

do_type2(I, Info, FunVar, ArityVar) -> % function2(Fun,Arity)
  %% Just for sanity.
  function2 = hipe_icode:type_test(I),
  FunType = lookup(FunVar, Info),
  ArityType = lookup(ArityVar, Info),
  TrueLab = hipe_icode:type_true_label(I),
  FalseLab = hipe_icode:type_false_label(I),
  SuccType1 = t_inf(t_fun(), FunType),
  case combine_test(test_type(function, FunType), 
		    test_type(integer, ArityType)) of
    true ->
      case t_number_vals(ArityType) of
	[Arity] ->
	  case t_fun_arity(SuccType1) of
	    unknown ->
	      SuccType = t_inf(t_fun(Arity,t_any()),FunType),
	      [{TrueLab, enter(FunVar, SuccType, Info)},
	       {FalseLab, Info}];
	    Arity when is_integer(Arity) ->
	      FalseType = t_subtract(FunType, t_fun(Arity, t_any())),
	      [{TrueLab,  enter(FunVar, SuccType1, Info)},
	       {FalseLab, enter(FunVar, FalseType, Info)}]
	  end;
	_ ->
	  case t_fun_arity(SuccType1) of
	    unknown ->
	      [{TrueLab, enter(FunVar,SuccType1,Info)},
	       {FalseLab, Info}];
	    Arity when is_integer(Arity) ->
	      T = t_from_term(Arity),
	      NewInfo = enter(ArityVar, T, Info),
	      [{TrueLab, enter(FunVar, SuccType1, NewInfo)},
	       {FalseLab, enter(ArityVar, t_subtract(T, ArityType), Info)}]
	  end
      end;
    false ->
      [{FalseLab, Info}];
    maybe ->
      GenTrueArity = t_inf(t_integer(), ArityType),
      GenTrueFun = t_inf(t_fun(), FunType),
      case {t_number_vals(GenTrueArity), t_fun_arity(GenTrueFun)} of
	{unknown, unknown} ->
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[GenTrueFun, GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{unknown, Arity} when is_integer(Arity) ->
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[GenTrueFun, t_integer(Arity)], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{[Val], unknown} when is_integer(Val) ->
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[t_inf(GenTrueFun, t_fun(Val, t_any())),
				 GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{Vals, unknown} when is_list(Vals) ->
	  %% The function type gets widened when we have more than one arity.
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[GenTrueFun, GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{Vals, Arity} when is_list(Vals), is_integer(Arity) ->
	  case lists:member(Arity, Vals) of
	    false ->
	      [{FalseLab, Info}];
	    true ->
	      TrueInfo = enter_list([FunVar, ArityVar], 
				    [GenTrueFun, t_integer(Arity)], Info),
	      [{TrueLab, TrueInfo}, {FalseLab, Info}]
	  end
      end
  end.

combine_test(true, true) -> true;
combine_test(false, _)   -> false;
combine_test(_, false)   -> false;
combine_test(_, _)       -> maybe.

do_type(I, Info, Var) ->
  TrueLab = hipe_icode:type_true_label(I),
  FalseLab = hipe_icode:type_false_label(I),
  None = t_none(),
  case lookup(Var, Info) of
    None ->
      [{TrueLab, Info}, {FalseLab, Info}];
    VarInfo ->
      case hipe_icode:type_test(I) of
	cons ->
	  test_cons_or_nil(t_cons(), Var, VarInfo, TrueLab, FalseLab, Info);
	nil ->
	  test_cons_or_nil(t_nil(), Var, VarInfo, TrueLab, FalseLab, Info);
	{atom, A} = Test ->
	  test_number_or_atom(fun(X) -> t_atom(X) end, 
			      A, Var, VarInfo, Test, TrueLab, FalseLab, Info);
	{integer, N} = Test ->
	  test_number_or_atom(fun(X) -> t_number(X) end, 
			      N, Var, VarInfo, Test, TrueLab, FalseLab, Info);
	{record, Atom, Size} ->
	  test_record(Atom, Size, Var, VarInfo, TrueLab, FalseLab, Info);
	Other ->
	  case t_is_any(VarInfo) of
	    true ->
	      TrueType = t_inf(true_branch_info(Other), VarInfo),
	      TrueInfo = enter(Var, TrueType, Info),
	      [{TrueLab, TrueInfo}, {FalseLab, Info}];
	    false ->
	      case test_type(Other, VarInfo) of
		true ->
		  [{TrueLab, Info}];
		false ->
		  [{FalseLab, Info}];
		maybe ->
		  TrueType = t_inf(true_branch_info(Other), VarInfo),
		  TrueInfo = enter(Var, TrueType, Info),
		  FalseType = t_subtract(VarInfo, TrueType),
		  FalseInfo = enter(Var, FalseType, Info),
		  [{TrueLab, TrueInfo}, {FalseLab, FalseInfo}]
	      end
	  end
      end
  end.

do_switch_tuple_arity(I, Info) ->
  Var = hipe_icode:switch_tuple_arity_term(I),
  VarType = lookup(Var, Info),
  Cases = hipe_icode:switch_tuple_arity_cases(I),
  FailLabel = hipe_icode:switch_tuple_arity_fail_label(I),
  case legal_switch_tuple_arity_cases(Cases, VarType) of
    [] ->
      [{FailLabel, Info}];
    LegalCases ->      
      {Fail, UpdateInfo} =
	switch_tuple_arity_update_info(LegalCases, Var, VarType, 
				       FailLabel, VarType, Info, []),
      case switch_tuple_arity_can_fail(LegalCases, VarType) of
	true -> [Fail|UpdateInfo];
	false -> UpdateInfo
      end
  end.

legal_switch_tuple_arity_cases(Cases, Type) ->
  case t_is_tuple(Type) of
    false -> 
      Inf = t_inf(t_tuple(), Type),
      case t_is_tuple(Inf) of
	true -> legal_switch_tuple_arity_cases_1(Cases, Inf);
	false -> []
      end;
    true ->
      legal_switch_tuple_arity_cases_1(Cases, Type)
  end.

legal_switch_tuple_arity_cases_1(Cases, Type) ->
  case t_tuple_sizes(Type) of
    unknown ->
      Cases;
    TupleSizes ->
      [Case || {Size, _Label} = Case <- Cases, 
	       lists:member(hipe_icode:const_value(Size), TupleSizes)]
  end.

switch_tuple_arity_can_fail(LegalCases, ArgType) ->
  case t_is_tuple(ArgType) of
    false -> true;
    true ->
      case t_tuple_sizes(ArgType) of
	unknown -> true;
	Sizes1 ->
	  Sizes2 = [hipe_icode:const_value(X) || {X, _} <- LegalCases],
	  Set1 = sets:from_list(Sizes1),
	  Set2 = sets:from_list(Sizes2),
	  not sets:is_subset(Set1, Set2)
      end
  end.

switch_tuple_arity_update_info([{Arity, Label}|Left], Var, TupleType, 
			       FailLabel, FailType, Info, Acc) ->
  Inf = t_inf(TupleType, t_tuple(hipe_icode:const_value(Arity))),
  NewInfo = enter(Var, Inf, Info),
  NewFailType = t_subtract(FailType, Inf),
  switch_tuple_arity_update_info(Left, Var, TupleType, FailLabel, NewFailType,
				 Info, [{Label, NewInfo}|Acc]);
switch_tuple_arity_update_info([], Var, _TupleType, 
			       FailLabel, FailType, Info, Acc) ->
  {{FailLabel, enter(Var, FailType, Info)}, Acc}.

do_switch_val(I, Info) ->
  Var = hipe_icode:switch_val_term(I),
  VarType = lookup(Var, Info),
  Cases = hipe_icode:switch_val_cases(I),
  FailLabel = hipe_icode:switch_val_fail_label(I),
  case legal_switch_val_cases(Cases, VarType) of
    [] ->
      [{FailLabel, Info}];
    LegalCases ->
      switch_val_update_info(LegalCases, Var, VarType, 
			     FailLabel, VarType, Info, [])
  end.

legal_switch_val_cases(Cases, Type) ->
  legal_switch_val_cases(Cases, Type, []).

legal_switch_val_cases([{Val, _Label} = VL|Left], Type, Acc) ->
  ConstType = t_from_term(hipe_icode:const_value(Val)),
  case t_is_subtype(ConstType, Type) of
    true ->
      legal_switch_val_cases(Left, Type, [VL|Acc]);
    false ->
      legal_switch_val_cases(Left, Type, Acc)
  end;
legal_switch_val_cases([], _Type, Acc) ->
  lists:reverse(Acc).

switch_val_update_info([{Const, Label}|Left], Arg, ArgType, 
		       FailLabel, FailType, Info, Acc) ->
  TrueType = t_from_term(hipe_icode:const_value(Const)),
  NewInfo = enter(Arg, TrueType, Info),
  NewFailType = t_subtract(FailType, TrueType),
  switch_val_update_info(Left, Arg, ArgType, FailLabel, NewFailType,
			 Info, [{Label, NewInfo}|Acc]);
switch_val_update_info([], Arg, _ArgType, FailLabel, FailType,Info, Acc) ->
  [{FailLabel, enter(Arg, FailType, Info)}|Acc].

test_cons_or_nil(Type, Var, VarInfo, TrueLab, FalseLab, Info) ->
  case t_is_any(VarInfo) of
    true -> 
      [{TrueLab, enter(Var, Type, Info)},
       {FalseLab, Info}];
    false ->      
      TrueType = t_inf(VarInfo, Type),
      FalseType = t_subtract(VarInfo, TrueType),
      case t_is_none(FalseType) of
	true ->
	  [{TrueLab, Info}];
	false ->
	  case t_is_none(TrueType) of
	    true ->
	      [{FalseLab, Info}];
	    false ->
	      [{TrueLab, enter(Var, TrueType, Info)},
	       {FalseLab, enter(Var, FalseType, Info)}]
	  end
      end
  end.

test_number_or_atom(Fun, X, Var, VarInfo, TypeTest,
		    TrueLab, FalseLab, Info) ->
  case t_is_any(VarInfo) of
    true ->
      [{TrueLab, enter(Var, Fun(X), Info)},
       {FalseLab, Info}];
    false ->
      case test_type(TypeTest, VarInfo) of
	false ->
	  [{FalseLab, Info}];
	true ->
	  [{TrueLab, Info}];
	maybe ->
	  FalseType = t_subtract(VarInfo, Fun(X)),
	  [{TrueLab, enter(Var, Fun(X), Info)},
	   {FalseLab, enter(Var, FalseType, Info)}]
      end
  end.

test_record(Atom, Size, Var, VarInfo, TrueLab, FalseLab, Info) ->
  AnyList = lists:duplicate(Size - 1, t_any()),
  RecordType = t_tuple([t_atom(Atom)|AnyList]),
  Inf = t_inf(RecordType, VarInfo),
  case t_is_none(Inf) of
    true ->
      [{FalseLab, Info}];
    false ->
      Sub = t_subtract(VarInfo, Inf),
      case t_is_none(Sub) of
	true ->
	  [{TrueLab, enter(Var, Inf, Info)}];
	false ->
	  [{TrueLab, enter(Var, Inf, Info)},
	   {FalseLab, enter(Var, Sub, Info)}]
      end
  end.

test_type(Test, Type) ->
  %% io:format("Test is: ~w\n", [Test]),
  %% io:format("Type is: ~s\n", [format_type(Type)]),
  Ans = 
    case t_is_any(Type) of
      true -> maybe;
      false ->
	TrueTest = true_branch_info(Test),
	Inf = t_inf(TrueTest, Type),
	%% io:format("TrueTest is: ~s\n", [format_type(TrueTest)]),
	%% io:format("Inf is: ~s\n", [format_type(Inf)]),
	case t_is_equal(Type, Inf) of
	  true ->
	    not t_is_none(Type);
	  false ->
	    case t_is_equal(TrueTest, Inf) of
	      true ->
		case test_type0(Test, Type) of
		  false ->
		    maybe;
		  true ->
		    true;
		  maybe ->
		    maybe
		end;
	      false ->
		case test_type0(Test, Inf) of
		  true ->
		    maybe;
		  false ->
		    false;
		  maybe ->
		    maybe
		end
	    end
	end
    end,
  %% io:format("Result is: ~s\n\n", [Ans]),
  Ans.

test_type0(integer, T) ->
  t_is_integer(T);
test_type0({integer, N}, T) ->
  case t_is_integer(T) of
    true -> 
      case t_number_vals(T) of
	unknown -> maybe;
	[N] -> true;
	List when is_list(List) -> 
	  case lists:member(N, List) of
	    true -> maybe;
	    false -> false
	  end
      end;
    false -> false
  end;
test_type0(float, T) ->
  t_is_float(T);  
test_type0(number, T) ->
  t_is_number(T);
test_type0(atom, T) ->
  t_is_atom(T);
test_type0({atom, A}, T) ->
  case t_is_atom(T) of
    true -> 
      case t_atom_vals(T) of
	unknown -> maybe;
	[A] -> true;
	List when is_list(List) -> 
	  case lists:member(A, List) of
	    true -> maybe;
	    false -> false
	  end
      end;
    false -> false
  end;
test_type0(tuple, T) ->
  t_is_tuple(T);
test_type0({tuple, N}, T) ->
  case t_is_tuple(T) of
    true -> 
      case t_tuple_sizes(T) of
	unknown -> maybe;
	[X] when is_integer(X) -> X =:= N;
	List when is_list(List) ->
	  case lists:member(N, List) of
	    true -> maybe;
	    false -> false
	  end
      end;
    false -> false
  end;
test_type0(pid, T) ->
  t_is_pid(T);
test_type0(port, T) ->
  t_is_port(T);
test_type0(binary, T) ->
  t_is_binary(T);
test_type0(bitstr, T) ->
  t_is_bitstr(T);
test_type0(reference, T) ->
  t_is_reference(T);
test_type0(function, T) ->
  t_is_fun(T);
test_type0(boolean, T) ->
  t_is_boolean(T);
test_type0(list, T) ->
  t_is_maybe_improper_list(T);
%% test_type0(cons, T) ->
%%   t_is_cons(T);
%% test_type0(nil, T) ->
%%   t_is_nil(T).
test_type0(map, T) ->
  t_is_map(T).

true_branch_info(integer) ->
  t_integer();
true_branch_info({integer, N}) ->
  t_integer(N);
true_branch_info(float) ->
  t_float();  
true_branch_info(number) ->
  t_number();
true_branch_info(atom) ->
  t_atom();
true_branch_info({atom, A}) ->
  t_atom(A);
true_branch_info(list) ->
  t_maybe_improper_list();
true_branch_info(tuple) ->
  t_tuple();
true_branch_info({tuple, N}) ->
  t_tuple(N);
true_branch_info(pid) ->
  t_pid();
true_branch_info(port) ->
  t_port();
true_branch_info(binary) ->
  t_binary();
true_branch_info(bitstr) ->
  t_bitstr();
true_branch_info(reference) ->
  t_reference();
true_branch_info(function) ->
  t_fun();
%% true_branch_info(cons) ->
%%   t_cons();
%% true_branch_info(nil) ->
%%   t_nil();
true_branch_info(boolean) ->
  t_boolean();
true_branch_info(map) ->
  t_map();
true_branch_info(T) ->
  exit({?MODULE, unknown_typetest, T}).


%% _________________________________________________________________
%%
%% Remove the redundant type tests. If a test is removed, the trace
%% that isn't taken is explicitly removed from the CFG to simplify
%% the handling of Phi nodes. If a Phi node is left and at least one
%% branch into it has disappeared, the SSA propagation pass cannot
%% handle it.
%%
%% If the CFG has changed at the end of this pass, the analysis is
%% done again since we might be able to find more information because
%% of the simplification of the CFG.
%%

simplify_controlflow(State) ->
  Cfg = state__cfg(State),
  simplify_controlflow(hipe_icode_cfg:reverse_postorder(Cfg), State).

simplify_controlflow([Label|Left], State) ->
  Info = state__info_out(State, Label),
  NewState = 
    case state__bb(State, Label) of
      not_found -> State;
      BB ->
	I = hipe_bb:last(BB),
	case I of
	  #icode_if{} ->
	    rewrite_if(State,I,BB,Info,Label);
	  #icode_type{} ->
	    rewrite_type(State,I,BB,Info,Label);
	  #icode_switch_tuple_arity{} ->
	    rewrite_switch_tuple_arity(State,I,BB,Info,Label);    
	  #icode_switch_val{} ->
	    rewrite_switch_val(State,I,BB,Info,Label);
	  #icode_call{} ->
	    rewrite_call(State,I,BB,Info,Label);
	  _ ->
	    State
	end
    end,
  simplify_controlflow(Left, NewState);
simplify_controlflow([], State) ->
  State.

rewrite_if(State, I, BB, Info, Label) ->
  case do_if(I, Info) of
    [{Lab, _}] ->
      mk_goto(State, BB, Label, Lab);
    [_,_] ->
      State
  end.

rewrite_type(State, I, BB, Info, Label) ->
  FalseLab = hipe_icode:type_false_label(I),
  case hipe_icode:type_true_label(I) of
    FalseLab ->
      %% true label = false label, this can occur!
      mk_goto(State, BB, Label, FalseLab);
    TrueLab ->
      case do_type(I, Info) of
	[{TrueLab, _}] -> 
	  mk_goto(State, BB, Label, TrueLab);
	[{FalseLab, _}] -> 
	  mk_goto(State, BB, Label, FalseLab);
	[_,_] -> %% Maybe
	  State
      end
  end.

rewrite_switch_tuple_arity(State, I, BB, Info, Label) ->
  Cases = hipe_icode:switch_tuple_arity_cases(I),
  Var = hipe_icode:switch_tuple_arity_term(I),
  Type = safe_lookup(Var, Info),	  
  case legal_switch_tuple_arity_cases(Cases, Type) of
    [] ->
      Fail = hipe_icode:switch_tuple_arity_fail_label(I),
      mk_goto(State, BB, Label, Fail);
    Cases -> 
      %% Nothing changed.
      case switch_tuple_arity_can_fail(Cases, Type) of
	true -> State;
	false ->
	  NewCases = butlast(Cases),
	  {_Arity, NewFail} = lists:last(Cases),
	  TmpI = 
	    hipe_icode:switch_tuple_arity_fail_label_update(I, NewFail),
	  NewI =
	    hipe_icode:switch_tuple_arity_cases_update(TmpI, NewCases),
	  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
	  state__bb_add(State, Label, NewBB)
      end;
    LegalCases ->
      NewI =
	case switch_tuple_arity_can_fail(LegalCases, Type) of
	  true -> 
	    hipe_icode:switch_tuple_arity_cases_update(I, LegalCases);
	  false ->
	    NewCases = butlast(LegalCases),
	    {_Arity, NewFail} = lists:last(LegalCases),
	    TmpI = 
	      hipe_icode:switch_tuple_arity_cases_update(I, NewCases),
	    hipe_icode:switch_tuple_arity_fail_label_update(TmpI, NewFail)
	end,
      NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
      state__bb_add(State, Label, NewBB)
  end.

rewrite_switch_val(State, I, BB, Info, Label) ->
  Cases = hipe_icode:switch_val_cases(I),
  Var = hipe_icode:switch_val_term(I),
  VarType = safe_lookup(Var, Info),
  case legal_switch_val_cases(Cases, VarType) of
    [] ->
      Fail = hipe_icode:switch_val_fail_label(I),
      mk_goto(State, BB, Label, Fail);
    Cases ->
      State;
    %% TODO: Find out whether switch_val can fail 
    %% just as switch_tuple_arity
    LegalCases ->
      NewI = hipe_icode:switch_val_cases_update(I, LegalCases),
      NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
      state__bb_add(State, Label, NewBB)
  end.

rewrite_call(State,I,BB,Info,Label) ->
  case call_always_fails(I, Info) of
    false ->
      Fun = hipe_icode:call_fun(I),
      case hipe_icode_primops:fails(Fun) of
	false ->
	  case hipe_icode:call_fail_label(I) of
	    [] -> State;
	    _ -> unset_fail(State, BB, Label, I)
	  end;
	true -> State
      end;
    true ->
      case hipe_icode:call_in_guard(I) of
	false -> State;
	true ->
	  FailLabel = hipe_icode:call_fail_label(I),
	  mk_goto(State, BB, Label, FailLabel)
      end
  end.

mk_goto(State, BB, Label, Succ) ->
  NewI = hipe_icode:mk_goto(Succ),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
  state__bb_add(State, Label, NewBB).

unset_fail(State, BB, Label, I) ->
  %%io:format("Setting a guard that cannot fail\n", []),
  NewI = hipe_icode:call_set_fail_label(I, []),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
  state__bb_add(State, Label, NewBB).

%% _________________________________________________________________
%%
%% Make transformations (specialisations) based on the type knowledge. 
%%
%% Annotate the variables with the local information. Since we have
%% the code in SSA form and the type information can only depend on
%% assignments or branches (type tests), we can use the information
%% out of the block to annotate all variables in it.
%%

-spec specialize(cfg()) -> cfg().

specialize(Cfg) ->
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  transform_bbs(Labels, Cfg).

transform_bbs([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = make_transformations(Code),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  transform_bbs(Left, NewCfg);
transform_bbs([], Cfg) ->
  Cfg.

make_transformations(Is) ->
  lists:flatten([transform_insn(I) || I <- Is]).

transform_insn(I) ->
  case I of
    #icode_call{} ->
      handle_call_and_enter(I);
    #icode_enter{} ->
      handle_call_and_enter(I);
    #icode_if{} ->
      CurrentIfOp = hipe_icode:if_op(I),
      UsesFixnums = all_fixnums([get_type(A) || A <- hipe_icode:args(I)]),
      AnyImmediate = any_immediate([get_type(A) || A <- hipe_icode:args(I)]),
      ExactComp = is_exact_comp(CurrentIfOp),
      if UsesFixnums ->
	  hipe_icode:if_op_update(I, fixnum_ifop(CurrentIfOp));
	 AnyImmediate andalso ExactComp ->
	  hipe_icode:if_op_update(I, fixnum_ifop(CurrentIfOp));
	 true ->
	  I
      end;
    _ ->
      I
  end.

handle_call_and_enter(I) ->
  case call_or_enter_fun(I) of
    #element{} ->
      transform_insn(update_call_or_enter(I, {erlang, element, 2}));
    {erlang, element, 2} ->
      NewI1 = transform_element2(I),
      case is_record(I, icode_call) andalso hipe_icode:call_in_guard(I) of
	true ->
	  case hipe_icode:call_fun(NewI1) of
	    #unsafe_element{} -> NewI1;
	    _ -> I
	  end;
	false -> 
	  NewI1
      end;
    {erlang, hd, 1} -> transform_hd_or_tl(I, unsafe_hd);
    {erlang, tl, 1} -> transform_hd_or_tl(I, unsafe_tl);
    {hipe_bs_primop, BsOP} ->
      NewBsOp =
	bit_opts(BsOP, get_type_list(hipe_icode:args(I))),
      update_call_or_enter(I, {hipe_bs_primop, NewBsOp});
    conv_to_float -> 
      [Src] = hipe_icode:args(I),
      case t_is_float(get_type(Src)) of
	true -> 
	  update_call_or_enter(I, unsafe_untag_float);
	false -> 
	  I
      end;
    FunName ->
      case is_arith_function(FunName) of
	true ->
	  case strength_reduce(I, FunName) of
	    NewIs when is_list(NewIs) ->
	      [pos_transform_arith(NewI) || NewI <- NewIs];
	    NewI ->
	      pos_transform_arith(NewI)
	  end;
	false ->
	  I
      end
  end.

pos_transform_arith(I) ->
  case hipe_icode:is_enter(I) orelse hipe_icode:is_call(I) of
    true ->
      FunName = call_or_enter_fun(I),
      transform_arith(I, FunName);
    false ->
      I
  end.

is_arith_function(Name) ->
  case Name of
    'band' -> true;
    'bor'  -> true;
    'bxor' -> true;
    'bnot' -> true;
    'bsl'  -> true;
    'bsr'  -> true;
    '+'    -> true;
    '-'    -> true;
    '*'    -> true;
    'div'  -> true;
    'rem'  -> true;
    _      -> false
  end.

%%---------------------------------------------------------------------
%% Perform a limited form of strength reduction for multiplication and
%% division of an integer with constants which are multiples of 2.
%%---------------------------------------------------------------------

strength_reduce(I, Op) ->
  case Op of
    '*' ->
      [Arg1, Arg2] = mult_args_const_second(I),
      ArgT1 = get_type(Arg1),
      case t_is_integer(ArgT1) of
	true ->
          case hipe_icode:is_const(Arg2) of
            true ->
              case hipe_icode:const_value(Arg2) of
		  0 -> case call_dstlist(I) of
			 [] -> remove_useless_arithmetic_instruction(I);
			 [Dst] -> create_strength_reduce_move(I, Dst, Arg2)
		       end;
		  1 -> case call_dstlist(I) of
			 [] -> remove_useless_arithmetic_instruction(I);
			 [Dst] -> create_strength_reduce_move(I, Dst, Arg1)
		       end;
		  2 -> strength_reduce_imult(I, Arg1, 1);
		  4 -> strength_reduce_imult(I, Arg1, 2);
		  8 -> strength_reduce_imult(I, Arg1, 3);
		 16 -> strength_reduce_imult(I, Arg1, 4);
		 32 -> strength_reduce_imult(I, Arg1, 5);
		 64 -> strength_reduce_imult(I, Arg1, 6);
                128 -> strength_reduce_imult(I, Arg1, 7);
                256 -> strength_reduce_imult(I, Arg1, 8);
                ___ -> I
	      end;
            false -> I
          end;
        false -> I
      end;
    'div' ->
      [Arg1, Arg2] = hipe_icode:args(I),
      ArgT1 = get_type(Arg1),
      case t_is_non_neg_integer(ArgT1) of
        true -> %% the optimization is NOT valid for negative integers
          case hipe_icode:is_const(Arg2) of
            true ->
              case hipe_icode:const_value(Arg2) of
		  0 -> io:fwrite("Integer division by 0 detected!\n"), I;
		  1 -> case call_dstlist(I) of
			 [] -> remove_useless_arithmetic_instruction(I);
			 [Dst] -> create_strength_reduce_move(I, Dst, Arg1)
		       end;
		  2 -> strength_reduce_div(I, Arg1, 1);
		  4 -> strength_reduce_div(I, Arg1, 2);
		  8 -> strength_reduce_div(I, Arg1, 3);
		 16 -> strength_reduce_div(I, Arg1, 4);
		 32 -> strength_reduce_div(I, Arg1, 5);
		 64 -> strength_reduce_div(I, Arg1, 6);
                128 -> strength_reduce_div(I, Arg1, 7);
                256 -> strength_reduce_div(I, Arg1, 8);
                ___ -> I
	      end;
            false -> I
          end;
        false -> I
      end;
    'rem' ->
      [Arg1, Arg2] = hipe_icode:args(I),
      ArgT1 = get_type(Arg1),
      case t_is_non_neg_integer(ArgT1) of
	true -> %% the optimization is NOT valid for negative integers
	  case hipe_icode:is_const(Arg2) of
	    true ->
	      case hipe_icode:const_value(Arg2) of
		  0 -> io:fwrite("Remainder with 0 detected!\n"), I;
		  1 -> case call_dstlist(I) of
			 [] -> remove_useless_arithmetic_instruction(I);
			 [Dst] -> create_strength_reduce_move(
				    I, Dst, hipe_icode:mk_const(0))
		       end;
		  2 -> strength_reduce_rem(I, Arg1,   1);
		  4 -> strength_reduce_rem(I, Arg1,   3);
		  8 -> strength_reduce_rem(I, Arg1,   7);
		 16 -> strength_reduce_rem(I, Arg1,  15);
		 32 -> strength_reduce_rem(I, Arg1,  31);
		 64 -> strength_reduce_rem(I, Arg1,  63);
		128 -> strength_reduce_rem(I, Arg1, 127);
		256 -> strength_reduce_rem(I, Arg1, 255);
                ___ -> I
	      end;
            false -> I
          end;
        false -> I
      end;
    _ -> I
  end.

remove_useless_arithmetic_instruction(_) ->
  [].

create_strength_reduce_move(I, Dst, Val) ->
  case hipe_icode:call_continuation(I) of
    [] ->
      hipe_icode:mk_move(Dst, Val);
    Lbl ->
      [hipe_icode:mk_move(Dst, Val),
       hipe_icode:mk_goto(Lbl)]
  end.
      
%% Puts the args of a multiplication in a form where the constant
%% (if present) is always the second argument.
mult_args_const_second(I) ->
  [Arg1, Arg2] = Args = hipe_icode:args(I),
  case hipe_icode:is_const(Arg1) of
    true  -> [Arg2, Arg1];
    false -> Args
  end.

%% In all three functions below:
%%   - Arg1 is a variable of integer type
%%   - N is a small positive integer that will be used in a bit shift operation
strength_reduce_imult(I, Arg1, N) ->
  case t_number_vals(get_type(Arg1)) of
    [X] when is_integer(X) ->
      %% io:format("Multiplication with constant arguments:\n  ~w\n", [I]),
      case call_dstlist(I) of
	[] -> remove_useless_arithmetic_instruction(I);
	[D] -> create_strength_reduce_move(I, D, hipe_icode:mk_const(X bsl N))
      end;
    _ ->
      update_call_or_enter(I, 'bsl', [Arg1, hipe_icode:mk_const(N)])
  end.

strength_reduce_div(I, Arg1, N) ->
  case t_number_vals(get_type(Arg1)) of
    [X] when is_integer(X) ->
      %% io:format("Division with constant arguments:\n  ~w\n", [I]),
      case call_dstlist(I) of
	[] -> remove_useless_arithmetic_instruction(I);
	[D] -> create_strength_reduce_move(I, D, hipe_icode:mk_const(X bsr N))
      end;
    _ ->
      update_call_or_enter(I, 'bsr', [Arg1, hipe_icode:mk_const(N)])
  end.

strength_reduce_rem(I, Arg1, N) ->
  case t_number_vals(get_type(Arg1)) of
    [X] when is_integer(X) ->
      %% io:format("Remainder with constant arguments:\n  ~w\n", [I]),
      case call_dstlist(I) of
	[] -> remove_useless_arithmetic_instruction(I);
	[D] -> create_strength_reduce_move(I, D, hipe_icode:mk_const(X band N))
      end;
    _ ->
      update_call_or_enter(I, 'band', [Arg1, hipe_icode:mk_const(N)])
  end.

%%---------------------------------------------------------------------

call_or_enter_fun(I) ->
  case hipe_icode:is_call(I) of
    true -> hipe_icode:call_fun(I);
    false -> hipe_icode:enter_fun(I)
  end.

update_call_or_enter(I, NewFun) ->
  case hipe_icode:is_call(I) of
    true ->
      case hipe_icode_primops:fails(NewFun) of
	false ->
	  NewI = hipe_icode:call_fun_update(I, NewFun),
	  hipe_icode:call_set_fail_label(NewI, []);
	true ->
	  hipe_icode:call_fun_update(I, NewFun)
      end;
    false -> hipe_icode:enter_fun_update(I, NewFun)
  end.

update_call_or_enter(I, NewFun, NewArgs) ->
  case hipe_icode:is_call(I) of
    true -> 
      I1 = hipe_icode:call_args_update(I, NewArgs),
      hipe_icode:call_fun_update(I1, NewFun);
    false -> 
      I1 = hipe_icode:enter_args_update(I, NewArgs),
      hipe_icode:enter_fun_update(I1, NewFun)
  end.

transform_element2(I) ->
  [Index, Tuple] = hipe_icode:args(I),
  IndexType = get_type(Index),
  TupleType = get_type(Tuple),
  ?debug("Tuple", TupleType),
  NewIndex =
    case test_type(integer, IndexType) of
      true ->
	case t_number_vals(IndexType) of
	  unknown -> unknown;
	  [_|_] = Vals -> {number, Vals}
	end;
      _ -> unknown
    end,
  MinSize =
    case test_type(tuple, TupleType) of
      true ->
	?debug("is tuple", TupleType),
	case t_tuple_sizes(TupleType) of
	  unknown -> unknown;
	  Sizes -> {tuple, lists:min(Sizes)}
	end;
      _ -> unknown
    end,
  case {NewIndex, MinSize} of
    {{number, [_|_] = Ns}, {tuple, A}} when is_integer(A) ->
      case lists:all(fun(X) -> 0 < X andalso X =< A end, Ns) of
	true ->
	  case Ns of
	    [Idx] ->
	      [_, Tuple] = hipe_icode:args(I),
	      update_call_or_enter(I, #unsafe_element{index = Idx}, [Tuple]);
	    [_|_] ->
	      NewFun = {element, [MinSize, valid]},
	      update_call_or_enter(I, NewFun)
	  end;
	false ->
	  case lists:all(fun(X) -> hipe_tagscheme:is_fixnum(X) end, Ns) of
	    true ->
	      NewFun = {element, [MinSize, fixnums]},
	      update_call_or_enter(I, NewFun);
	    false ->
	      NewFun = {element, [MinSize, unknown]},
	      update_call_or_enter(I, NewFun)
	  end
      end;
    _ when (NewIndex =:= unknown) orelse (MinSize =:= unknown) ->
      case t_is_fixnum(IndexType) of
	true ->
	  NewFun = {element, [MinSize, fixnums]},
	  update_call_or_enter(I, NewFun);
	false ->
	  NewFun = {element, [MinSize, NewIndex]},	  
	  update_call_or_enter(I, NewFun)
      end
  end.

transform_hd_or_tl(I, Primop) ->
  [Arg] = hipe_icode:args(I),
  case t_is_cons(get_type(Arg)) of
    true -> update_call_or_enter(I, Primop);
    false -> I
  end.

transform_arith(I, Op) ->
  ArgTypes = get_type_list(hipe_icode:args(I)),
  %% io:format("Op = ~w, Args = ~w\n", [Op, ArgTypes]),
  DstTypes = 
    case hipe_icode:is_call(I) of
      true -> get_type_list(call_dstlist(I));
      false -> [erl_bif_types:type(erlang, Op, length(ArgTypes), ArgTypes)]
    end,
  case valid_unsafe_args(ArgTypes, Op) of
    true -> 
      case all_is_fixnum(DstTypes) of
	true ->
	  update_call_or_enter(I, arithop_to_extra_unsafe(Op));
	false ->
	  update_call_or_enter(I, arithop_to_unsafe(Op))
      end;
    false -> 
      I
  end.

all_is_fixnum(Types) ->
  lists:all(fun erl_types:t_is_fixnum/1, Types).

valid_unsafe_args(Args, Op) ->
  if Op =:= 'bnot' ->
      [Arg] = Args,
      t_is_fixnum(Arg);
     true ->
      [LeftArg, RightArg] = Args,
      case Op of
	'bsl' -> t_is_fixnum(LeftArg) and t_is_bitwidth(RightArg);
	'bsr' -> t_is_fixnum(LeftArg) and t_is_bitwidth(RightArg);
	_     -> t_is_fixnum(LeftArg) and t_is_fixnum(RightArg)
      end
  end.

arithop_to_extra_unsafe(Op) ->
  case Op of
    '+'    -> extra_unsafe_add;
    '-'    -> extra_unsafe_sub;
    '*'    -> '*';	%% XXX: Revise?
    'div'  -> 'div';	%% XXX: Revise?
    'rem'  -> 'rem';    %% XXX: Revise?
    'band' -> unsafe_band;
    'bor'  -> unsafe_bor;
    'bxor' -> unsafe_bxor;
    'bnot' -> unsafe_bnot;
    'bsl'  -> unsafe_bsl;
    'bsr'  -> unsafe_bsr
  end.

arithop_to_unsafe(Op) ->
  case Op of
    '+' -> unsafe_add;
    '-' -> unsafe_sub;
    _   -> Op
  end.

fixnum_ifop(Op) ->
  case Op of
    '=:=' -> 'fixnum_eq';
    '=/=' -> 'fixnum_neq';
    '==' -> 'fixnum_eq';
    '/=' -> 'fixnum_neq';
    '>'   -> 'fixnum_gt';
    '<'   -> 'fixnum_lt';
    '>='  -> 'fixnum_ge';
    '=<'  -> 'fixnum_le';
    Op    -> Op
  end.

bit_opts({Name, Size, Flags} = I, [MSType]) when Name =:= bs_get_integer;
						 Name =:= bs_get_float;
						 Name =:= bs_get_binary ->
  Bits = t_matchstate_present(MSType),
  case t_is_bitstr(Bits) of
    true ->
      Base = t_bitstr_base(Bits),
      if Base >= Size ->
	  {Name, Size, Flags bor 16};
	 true -> I
      end;
    false -> I
  end;
bit_opts({bs_get_binary_all, Size, Flags} = I, [MSType]) ->
  Bits = t_matchstate_present(MSType),
  case t_is_bitstr(Bits) of
    true ->
      Base = t_bitstr_base(Bits),
      Unit = t_bitstr_unit(Bits),
      if (Base rem Size) =:= 0 andalso (Unit rem Size) =:= 0 ->
	  {bs_get_binary_all, Size, Flags bor 16};
	 true -> I
      end;
    false -> I
  end;
bit_opts({bs_test_unit, Size} = I, [MSType]) ->
  Bits = t_matchstate_present(MSType),
  case t_is_bitstr(Bits) of
    true ->
      Base = t_bitstr_base(Bits),
      Unit = t_bitstr_unit(Bits),
      if (Base rem Size) =:= 0 andalso (Unit rem Size) =:= 0 ->
	  {bs_test_unit, 1};
	 true -> I
      end;    
    false -> I
  end;
bit_opts({bs_put_integer, Size, Flags, ConstInfo} = I, [Src|_]) -> 
  case t_is_fixnum(Src) of
    true -> 
      {unsafe_bs_put_integer, Size, Flags, ConstInfo};
    false -> I
  end;
bit_opts({bs_start_match, Max} = I, [Src]) -> 
  case t_is_bitstr(Src) of
    true -> {{bs_start_match, bitstr}, Max};
    false ->
      MSorNone = t_inf(t_matchstate(), Src),
      case t_is_matchstate(MSorNone) of
	true ->
	  Slots = t_matchstate_slots(MSorNone),
	  case t_is_any(Slots) orelse (length(t_to_tlist(Slots)) =< Max) of
	    true -> I;
	    false -> {{bs_start_match, ok_matchstate}, Max}
	  end;
	false -> I
      end
  end;
bit_opts(I, _) -> I.

is_exact_comp(Op) ->
  case Op of
    '=:=' -> true;
    '=/=' -> true;
    _Op   -> false
  end.

all_fixnums([Type|Types]) ->
  t_is_fixnum(Type) andalso all_fixnums(Types);
all_fixnums([]) ->
  true.

any_immediate([Type|Types]) ->
  t_is_fixnum(Type) orelse t_is_atom(Type) orelse any_immediate(Types);
any_immediate([]) -> false.

get_standard_primop(unsafe_bsl) -> 'bsl';
get_standard_primop(unsafe_bsr) -> 'bsr';
get_standard_primop(unsafe_add) -> '+';
get_standard_primop(extra_unsafe_add) -> '+';
get_standard_primop(unsafe_bnot) -> 'bnot';
get_standard_primop(unsafe_bxor) -> 'bxor';
get_standard_primop(unsafe_band) -> 'band';
get_standard_primop(unsafe_bor) -> 'bor';
get_standard_primop(unsafe_sub) -> '-';
get_standard_primop(extra_unsafe_sub) -> '-';
get_standard_primop(Op) -> Op.

primop_type(Op, Args) ->
  case Op of
    #mkfun{mfa = MFA} ->
      t_inf(t_fun(), find_signature_mfa(MFA));
    _ ->
      None = t_none(),
      Primop = get_standard_primop(Op),
      RetType = hipe_icode_primops:type(Primop, Args),
      case RetType of
	None ->
	  hipe_icode_primops:type(Primop, add_funs_to_arg_types(Args));
	Other ->
	  Other
      end
  end.

%%------------------------------------------------------------------
%% Various help functions.
%%------------------------------------------------------------------

add_arg_types(Args, Types) ->
  add_arg_types(Args, Types, gb_trees:empty()).

add_arg_types([Arg|Args], [Type|Types], Acc) ->
  Type1 =
    case t_is_none(Type) of
      true -> t_any();
      false -> Type
    end,
  add_arg_types(Args,Types, enter(Arg, Type1, Acc));
add_arg_types(_, [], Acc) ->
  Acc.

get_type_list(ArgList) ->
  [get_type(Arg) || Arg <- ArgList].

get_type(Arg) ->
  case hipe_icode:is_annotated_variable(Arg) of
    true ->
      None = t_none(),
      case hipe_icode:variable_annotation(Arg) of
	{type_anno, None, _} -> t_any();
	{type_anno, Type, _} -> Type
      end;
    false ->
      case hipe_icode:is_const(Arg) of
	true -> const_type(Arg);
	false -> t_any()
      end
  end.

%% Lookup treats anything that is neither in the map or a constant as
%% t_none(). Use this during type propagation!

lookup(Var, Tree) ->
  case gb_trees:lookup(Var, Tree) of
    none ->
      case hipe_icode:is_const(Var) of
	true -> const_type(Var);
	false -> t_none()
      end;
    {value, Type} ->
       Type
  end.

lookup_list(List, Info) ->
  lookup_list0(List, Info, []).

lookup_list0([H|T], Info, Acc) ->
  lookup_list0(T, Info, [lookup(H, Info)|Acc]);
lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

%% safe_lookup treats anything that is neither in the map nor a
%% constant as t_any(). Use this during transformations.

safe_lookup(Var, Tree) ->
  case gb_trees:lookup(Var, Tree) of
    none ->
      case hipe_icode:is_const(Var) of
	true -> const_type(Var);
	false ->
	  %% io:format("Expression has undefined type\n",[]),
	  t_any()
      end;
    {value, Type} ->
      Type
  end.

safe_lookup_list(List, Info) ->
  safe_lookup_list0(List, Info, []).

safe_lookup_list0([H|T], Info, Acc) ->
  safe_lookup_list0(T, Info, [safe_lookup(H, Info)|Acc]);
safe_lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

enter_list([Var|VarLeft], [Type|TypeLeft], Info) ->
  NewInfo = enter(Var, Type, Info),
  enter_list(VarLeft, TypeLeft, NewInfo);
enter_list([], [], Info) ->
  Info.

enter([Key], Value, Tree) ->
  enter(Key, Value, Tree);
enter(Key, Value, Tree) ->
  case is_var_or_reg(Key) of 
    true ->
      case t_is_none(Value) of
	true ->
	  gb_trees:delete_any(Key, Tree);
	false ->
	  gb_trees:enter(Key, Value, Tree)
      end;
    false ->
      Tree
  end.

join_list(List, Info) ->
  join_list(List, Info, t_none()).
 
join_list([H|T], Info, Acc) ->
  Type = t_sup(lookup(H, Info), Acc),
  join_list(T, Info, Type);
join_list([], _, Acc) ->
  Acc.

join_info_in([], _OldInfo, _NewInfo) ->
  %% No variables are live in. The information must be at a fixpoint.
  fixpoint;
join_info_in(Vars, OldInfo, NewInfo) ->
  NewInfo2 = join_info_in(Vars, Vars, OldInfo, NewInfo, gb_trees:empty()),
  case info_is_equal(NewInfo2, OldInfo) of
    true -> fixpoint;
    false -> NewInfo2
  end.

%% NOTE: Variables can be bound to other variables. Joining these is
%% only possible if the binding is the same from both traces and this
%% variable is still live.

join_info_in([Var|Left], LiveIn, Info1, Info2, Acc) ->
  Type1 = gb_trees:lookup(Var, Info1),
  Type2 = gb_trees:lookup(Var, Info2),
  case {Type1, Type2} of
    {none, none} ->
      join_info_in(Left, LiveIn, Info1, Info2, Acc);
    {none, {value, Val}} ->
      NewTree = gb_trees:insert(Var, Val, Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree);
    {{value, Val}, none} ->
      NewTree = gb_trees:insert(Var, Val, Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree);
    {{value, Val1}, {value, Val2}} ->
       NewTree = gb_trees:insert(Var, t_sup(Val1, Val2), Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree)
  end;
join_info_in([], _LiveIn, _Info1, _Info2, Acc) ->
  Acc.

info_is_equal(Info1, Info2) ->
  compare(gb_trees:to_list(Info1), gb_trees:to_list(Info2)).

compare([{Var, Type1}|Left1], [{Var, Type2}|Left2]) ->
  t_is_equal(Type1, Type2) andalso compare(Left1, Left2);
compare([], []) ->
  true;
compare(_, _) ->
  false.

const_type(Const) ->
  t_from_term(hipe_icode:const_value(Const)).

do_updates(State, List) ->
  do_updates(State, List, []).

do_updates(State, [{Label, Info}|Tail], Worklist) ->
  case state__info_in_update(State, Label, Info) of
    fixpoint ->
      %% io:format("Info in for ~w is: fixpoint\n", [Label]),
      do_updates(State, Tail, Worklist);
    NewState ->
      %% io:format("Info in for ~w is:\n", [Label]),
      %% [io:format("~w: ~p\n", [X, format_type(Y)])
      %%  || {X, Y} <- gb_trees:to_list(state__info_in(NewState, Label))],
      do_updates(NewState, Tail, [Label|Worklist])
  end;
do_updates(State, [], Worklist) ->
  {State, Worklist}.

enter_defines(I, Type, Info) ->
  case defines(I) of
    [] -> Info;
    [Def] ->
      enter(Def, Type, Info);
    Defs ->
      Pairs = case t_is_any(Type) of
		true ->
		  [{Def, t_any()} || Def <- Defs];
		false ->
		  case t_is_none(Type) of
		    true ->
		      [{Def, t_none()} || Def <- Defs];
		    false ->
		      lists:zip(Defs, t_to_tlist(Type))
		  end
	      end,
      lists:foldl(fun({X, T}, Inf) -> enter(X, T, Inf) end, Info, Pairs)
  end.

defines(I) ->
  keep_vars_and_regs(hipe_icode:defines(I)).

call_dstlist(I) ->
  hipe_icode:call_dstlist(I).

uses(I) ->
  keep_vars_and_regs(hipe_icode:uses(I)).

keep_vars_and_regs(Vars) ->
  [V || V <- Vars, is_var_or_reg(V)].

butlast([_]) ->
  [];
butlast([H|T]) ->
  [H|butlast(T)].

-spec any_is_none([erl_types:erl_type()]) -> boolean().

any_is_none(Types) ->
  lists:any(fun (T) -> t_is_none(T) end, Types).

is_var_or_reg(X) ->
  hipe_icode:is_var(X) orelse hipe_icode:is_reg(X).

%% _________________________________________________________________
%%
%% Handling the state
%%

new_state(Cfg, {MFA, GetCallFun, GetResFun, FinalAction}) ->
  Start = hipe_icode_cfg:start_label(Cfg),
  Params = hipe_icode_cfg:params(Cfg),
  ParamTypes = GetCallFun(MFA, Cfg),
  case any_is_none(ParamTypes) of
    true ->
      FinalAction(MFA, [t_none()]),
      throw(no_input);
    false ->
      Info = add_arg_types(Params, ParamTypes),
      InfoMap = gb_trees:insert({Start, in}, Info, gb_trees:empty()),
      Liveness = hipe_icode_ssa:ssa_liveness__analyze(Cfg),
      #state{info_map = InfoMap, cfg = Cfg, liveness = Liveness, 
	     arg_types = ParamTypes, lookupfun = GetResFun, 
	     resultaction = FinalAction}
  end.

state__cfg(#state{cfg = Cfg}) ->
  Cfg.

state__succ(#state{cfg = Cfg}, Label) ->
  hipe_icode_cfg:succ(Cfg, Label).

state__bb(#state{cfg = Cfg}, Label) ->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_add(S = #state{cfg = Cfg}, Label, BB) ->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__params_update(S = #state{cfg = Cfg}, NewParams) ->
  NewCfg = hipe_icode_cfg:params_update(Cfg, NewParams),
  S#state{cfg = NewCfg}.

state__ret_type(#state{ret_type = RT}) -> RT.

state__lookupfun(#state{lookupfun = LF}) -> LF.

state__resultaction(#state{resultaction = RA}) -> RA.

state__info_in(S, Label) ->
  state__info(S, {Label, in}).

state__info_out(S, Label) ->
  state__info(S, {Label, out}).

state__info(#state{info_map = IM}, Label) ->
  case gb_trees:lookup(Label, IM) of
    {value, Info} -> Info;
    none -> gb_trees:empty()
  end.

state__ret_type_update(#state{ret_type = RT} = State, NewType) when 
  is_list(NewType) ->
  TotType = lists:zipwith(fun erl_types:t_sup/2, RT, NewType),
  State#state{ret_type = TotType};
state__ret_type_update(#state{ret_type = RT} = State, NewType) ->
  state__ret_type_update(State, [NewType || _ <- RT]). 

state__info_in_update(S=#state{info_map=IM, liveness=Liveness}, Label, Info) ->
  LiveIn = hipe_icode_ssa:ssa_liveness__livein(Liveness, Label),
  LabelIn = {Label, in},
  case gb_trees:lookup(LabelIn, IM) of
    none -> 
      OldInfo = gb_trees:empty(),
      case join_info_in(LiveIn, OldInfo, Info) of
	fixpoint -> 
	  %% If the BB has not been handled we ignore the fixpoint.
	  S#state{info_map = gb_trees:enter(LabelIn, OldInfo, IM)};
	NewInfo ->
	  S#state{info_map = gb_trees:enter(LabelIn, NewInfo, IM)}
      end;
    {value, OldInfo} ->
      case join_info_in(LiveIn, OldInfo, Info) of
	fixpoint -> 
	  fixpoint;
	NewInfo ->
	  S#state{info_map = gb_trees:enter(LabelIn, NewInfo, IM)}
      end
  end.

state__info_out_update(#state{info_map = IM} = State, Label, Info) ->
  State#state{info_map = gb_trees:enter({Label, out}, Info, IM)}.

%% _________________________________________________________________
%%
%% The worklist.
%%

init_work(State) ->
  %% Labels = hipe_icode_cfg:reverse_postorder(state__cfg(State)),
  Labels = [hipe_icode_cfg:start_label(state__cfg(State))],
  {Labels, [], gb_sets:from_list(Labels)}.

get_work({[Label|Left], List, Set}) ->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set}, [Label|Left]) ->
  case gb_sets:is_member(Label, Set) of
    true ->
      add_work(Work, Left);
    false ->
      %% io:format("Adding work: ~w\n", [Label]),
      add_work({List1, [Label|List2], gb_sets:insert(Label, Set)}, Left)
  end;
add_work(Work, []) ->
  Work.

%% _________________________________________________________________
%%
%% Annotator
%%

annotate_cfg(State) ->
  Cfg = state__cfg(State),
  NewState = annotate_params(hipe_icode_cfg:params(Cfg), State,
			     hipe_icode_cfg:start_label(Cfg)),
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  annotate_bbs(Labels, NewState).
 
annotate_params(Params, State, Start) ->
  Info = state__info_in(State, Start),
  AnnoFun = fun hipe_icode:annotate_variable/2,
  NewParams =
    lists:zipwith(AnnoFun, Params, [make_annotation(P,Info) || P <- Params]),
  state__params_update(State,NewParams).

annotate_bbs([Label|Left], State) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Info = state__info_in(State, Label),
  NewCode = annotate_instr_list(Code, Info, state__lookupfun(State), []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState = state__bb_add(State, Label, NewBB),
  annotate_bbs(Left, NewState);
annotate_bbs([], State) ->
  State.

annotate_instr_list([I], Info, LookupFun, Acc) ->
  NewInfo =
    case I of
      #icode_call{} ->
	do_safe_call(I, Info, LookupFun);
      _ ->
	analyse_insn(I, Info, LookupFun)
    end,
  NewI = annotate_instr(I, NewInfo, Info),
  lists:reverse([NewI|Acc]);
annotate_instr_list([I|Left], Info, LookupFun, Acc) ->
  NewInfo = 
    case I of
      #icode_call{} ->
	do_safe_call(I, Info, LookupFun);
      _ ->
	analyse_insn(I, Info, LookupFun)
    end,
  NewI = annotate_instr(I, NewInfo, Info),
  annotate_instr_list(Left, NewInfo, LookupFun, [NewI|Acc]).

annotate_instr(I, DefInfo, UseInfo) ->
  Def = defines(I),
  Use = uses(I),
  Fun = fun hipe_icode:annotate_variable/2,
  DefSubst = [{X, Fun(X, make_annotation(X, DefInfo))} || X <- Def],
  UseSubst = [{X, Fun(X, make_annotation(X, UseInfo))} || X <- Use],
  case DefSubst ++ UseSubst of
    [] ->
      I;
    Subst ->
      hipe_icode:subst(Subst, I)
  end.

make_annotation(X, Info) ->
  {type_anno, safe_lookup(X, Info), fun erl_types:t_to_string/1}. 

-spec unannotate_cfg(cfg()) -> cfg().

unannotate_cfg(Cfg) ->
  NewCfg = unannotate_params(Cfg),
  Labels = hipe_icode_cfg:labels(NewCfg),
  unannotate_bbs(Labels, NewCfg).

unannotate_params(Cfg) ->
  Params = hipe_icode_cfg:params(Cfg),
  NewParams = [hipe_icode:unannotate_variable(X)
	       || X <- Params, hipe_icode:is_variable(X)],
  hipe_icode_cfg:params_update(Cfg, NewParams).
  
unannotate_bbs([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = unannotate_instr_list(Code, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  unannotate_bbs(Left, NewCfg);
unannotate_bbs([], Cfg) ->
  Cfg.

unannotate_instr_list([I|Left], Acc) ->
  NewI = unannotate_instr(I),
  unannotate_instr_list(Left, [NewI|Acc]);
unannotate_instr_list([], Acc) ->
  lists:reverse(Acc).

unannotate_instr(I) ->
  DefUses = hipe_icode:defines(I) ++ hipe_icode:uses(I),
  Subst = [{X, hipe_icode:unannotate_variable(X)} || X <- DefUses,
						     hipe_icode:is_variable(X)],
  if Subst =:= [] -> I;
     true -> hipe_icode:subst(Subst, I)
  end.

%% _________________________________________________________________
%%
%% Find the types of the arguments to a call
%%

update_call_arguments(I, Info) ->
  Args = hipe_icode:call_args(I),
  ArgTypes = lookup_list(Args, Info),
  Signature = find_signature(hipe_icode:call_fun(I), length(Args)),
  case t_fun_args(Signature) of
    unknown ->
      Info;
    PltArgTypes ->
      NewArgTypes = t_inf_lists(ArgTypes, PltArgTypes),
      enter_list(Args, NewArgTypes, Info)
  end.

%% _________________________________________________________________
%%
%% PLT info
%%

find_signature(MFA = {_, _, _}, _) -> find_signature_mfa(MFA);
find_signature(Primop, Arity) -> find_signature_primop(Primop, Arity).

find_signature_mfa(MFA) ->
  case get_mfa_arg_types(MFA) of
    any ->
      t_fun(get_mfa_type(MFA));
    BifArgs ->
      t_fun(BifArgs, get_mfa_type(MFA))
  end.

find_signature_primop(Primop, Arity) ->
  case get_primop_arg_types(Primop) of
    any ->
      t_fun(Arity, get_primop_type(Primop));
    ArgTypes ->
      t_fun(ArgTypes, get_primop_type(Primop))
  end.

get_primop_arg_types(Primop) ->
  case hipe_icode_primops:arg_types(Primop) of
    unknown -> any;
    ArgTypes -> add_tuple_to_args(ArgTypes)
  end.

get_mfa_arg_types({M, F, A}) ->
  case erl_bif_types:arg_types(M, F, A) of
    unknown ->
      any;
    BifArgs ->
      add_tuple_to_args(BifArgs)
  end.

get_mfa_type({M, F, A}) ->
  erl_bif_types:type(M, F, A).

get_primop_type(Primop) ->
  hipe_icode_primops:type(get_standard_primop(Primop)). 
      
add_tuple_to_args(Types) ->  
  [add_tuple_to_type(T) || T <- Types].

add_tuple_to_type(T) ->
  None = t_none(),
  case t_inf(t_fun(), T) of
    None -> T;
    _Other -> t_sup(T, t_tuple([t_atom(),t_atom()]))
  end.

add_funs_to_arg_types(Types) ->  
  [add_fun_to_arg_type(T) || T <- Types].

add_fun_to_arg_type(T) ->
  None = t_none(),
  case t_inf(t_tuple([t_atom(),t_atom()]), T) of
    None -> T;
    _Other -> t_sup(T, t_fun())
  end.

%%=====================================================================
%% Icode Coordinator Callbacks
%%=====================================================================

-spec replace_nones([erl_types:erl_type()] | erl_types:erl_type()) ->
        [erl_types:erl_type()].

replace_nones(Types) when is_list(Types) ->
  [replace_none(T) || T <- Types];
replace_nones(Type) ->
  [replace_none(Type)].

-spec replace_none(erl_types:erl_type()) -> erl_types:erl_type().

replace_none(Type) ->
  case erl_types:t_is_none(Type) of
    true ->
      erl_types:t_any();
    false ->
      Type
  end.

-spec update__info([erl_types:erl_type()], [erl_types:erl_type()]) ->
        {boolean(), [erl_types:erl_type()]}.

update__info(NewTypes, OldTypes) ->
  SupFun =
    fun(T1, T2) -> erl_types:t_limit(erl_types:t_sup(T1,T2), ?TYPE_DEPTH) end,
  EqFun = fun erl_types:t_is_equal/2,
  ResTypes = lists:zipwith(SupFun, NewTypes, OldTypes),
  Change = lists:zipwith(EqFun, ResTypes, OldTypes),
  {lists:all(fun(X) -> X end, Change), ResTypes}.
 
-spec new__info([erl_types:erl_type()]) -> [erl_types:erl_type()].

new__info(NewTypes) ->
  [erl_types:t_limit(T, ?TYPE_DEPTH) || T <- NewTypes].

-spec return__info(erl_types:erl_type()) -> erl_types:erl_type().

return__info(Types) ->
  Types.

-spec return_none() -> [erl_types:erl_type(),...].

return_none() ->
  [erl_types:t_none()].

-spec return_none_args(cfg(), mfa()) -> [erl_types:erl_type()].

return_none_args(Cfg, {_M,_F,A}) ->
  NoArgs = 
    case hipe_icode_cfg:is_closure(Cfg) of
      true -> hipe_icode_cfg:closure_arity(Cfg) - 1;
      false -> A
    end,
  lists:duplicate(NoArgs, erl_types:t_none()).

-spec return_any_args(cfg(), mfa()) -> [erl_types:erl_type()].

return_any_args(Cfg, {_M,_F,A}) ->
  NoArgs = 
    case hipe_icode_cfg:is_closure(Cfg) of
      true -> hipe_icode_cfg:closure_arity(Cfg);
      false -> A
    end,
  lists:duplicate(NoArgs, erl_types:t_any()).

%%=====================================================================
%% Testing function below
%%=====================================================================

-ifdef(DO_HIPE_ICODE_TYPE_TEST).

test() ->
  Range1 = t_from_range(1, pos_inf),
  Range2 = t_from_range(0, 5),
  Var1 = hipe_icode:mk_var(1),
  Var2 = hipe_icode:mk_var(2),

  Info = enter(Var1, Range1, enter(Var2, Range2, gb_trees:empty())),
  io:format("A1 ~p~n", [Info]),
  A = integer_range_inequality_propagation('<', Var1, Var2, 1, 2, Info),
  B = integer_range_inequality_propagation('>=', Var1, Var2, 1, 2, Info),
  C = integer_range_inequality_propagation('=<', Var1, Var2, 1, 2, Info),
  D = integer_range_inequality_propagation('>', Var1, Var2, 1, 2, Info),

  io:format("< ~p~n", [A]),
  io:format(">= ~p~n", [B]),
  io:format("<= ~p~n", [C]),
  io:format("> ~p~n", [D]).

-endif.
