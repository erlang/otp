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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_dataflow.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created : 19 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

-module(dialyzer_dataflow).

-export([get_fun_types/5, get_warnings/5, format_args/3]).

%% Data structure interfaces.
-export([state__add_warning/2, state__cleanup/1,
	 state__duplicate/1, dispose_state/1,
         state__get_callgraph/1, state__get_races/1,
         state__get_records/1, state__put_callgraph/2,
         state__put_races/2, state__records_only/1,
         state__find_function/2]).

-export_type([state/0]).

-include("dialyzer.hrl").

-import(erl_types,
        [t_inf/2, t_inf/3, t_inf_lists/2, t_inf_lists/3,
         t_inf_lists/3, t_is_equal/2, t_is_subtype/2, t_subtract/2,
         t_sup/1, t_sup/2]).

-import(erl_types,
	[any_none/1, t_any/0, t_atom/0, t_atom/1, t_atom_vals/1, t_atom_vals/2,
	 t_binary/0, t_boolean/0,
	 t_bitstr/0, t_bitstr/2, t_bitstr_concat/1, t_bitstr_match/2,
	 t_cons/0, t_cons/2, t_cons_hd/2, t_cons_tl/2,
         t_contains_opaque/2,
	 t_find_opaque_mismatch/3, t_float/0, t_from_range/2, t_from_term/1,
	 t_fun/0, t_fun/2, t_fun_args/1, t_fun_args/2, t_fun_range/1,
	 t_fun_range/2, t_integer/0, t_integers/1,
	 t_is_any/1, t_is_atom/1, t_is_atom/2, t_is_any_atom/3,
         t_is_boolean/2,
	 t_is_integer/2, t_is_list/1,
	 t_is_nil/2, t_is_none/1, t_is_none_or_unit/1,
	 t_is_number/2, t_is_reference/2, t_is_pid/2, t_is_port/2,
         t_is_unit/1,
	 t_limit/2, t_list/0, t_list_elements/2,
	 t_maybe_improper_list/0, t_module/0,
	 t_none/0, t_non_neg_integer/0, t_number/0, t_number_vals/2,
	 t_pid/0, t_port/0, t_product/1, t_reference/0,
         t_to_string/2, t_to_tlist/1,
	 t_tuple/0, t_tuple/1, t_tuple_args/1, t_tuple_args/2,
         t_tuple_subtypes/2,
	 t_unit/0, t_unopaque/2,
	 t_map/0, t_map/1, t_is_singleton/2
     ]).

%%-define(DEBUG, true).
%%-define(DEBUG_PP, true).
%%-define(DEBUG_TIME, true).

-ifdef(DEBUG).
-import(erl_types, [t_to_string/1]).
-define(debug(S_, L_), io:format(S_, L_)).
-else.
-define(debug(S_, L_), ok).
-endif.

%%--------------------------------------------------------------------

-type type()      :: erl_types:erl_type().
-type types()     :: erl_types:type_table().

-type curr_fun()  :: 'undefined' | 'top' | mfa_or_funlbl().

-define(no_arg, no_arg).

-define(TYPE_LIMIT, 3).

-define(BITS, 128).

%% Types with comment 'race' are due to dialyzer_races.erl.
-record(state, {callgraph            :: dialyzer_callgraph:callgraph()
                                      | 'undefined', % race
                codeserver           :: dialyzer_codeserver:codeserver()
                                      | 'undefined', % race
		envs                 :: env_tab()
                                      | 'undefined', % race
		fun_tab		     :: fun_tab()
                                      | 'undefined', % race
                fun_homes            :: dict:dict(label(), mfa())
                                      | 'undefined', % race
                reachable_funs       :: sets:set(label())
                                      | 'undefined', % race
		plt		     :: dialyzer_plt:plt()
                                      | 'undefined', % race
		opaques              :: [type()]
                                      | 'undefined', % race
		races = dialyzer_races:new() :: dialyzer_races:races(),
		records = dict:new() :: types(),
		tree_map	     :: dict:dict(label(), cerl:cerl())
                                      | 'undefined', % race
		warning_mode = false :: boolean(),
		warnings = []        :: [raw_warning()],
		work                 :: {[_], [_], sets:set()}
                                      | 'undefined', % race
		module               :: module(),
                curr_fun             :: curr_fun()
               }).

-record(map, {map = maps:new()    :: type_tab(),
              subst = maps:new()  :: subst_tab(),
              modified = []       :: [Key :: term()],
              modified_stack = [] :: [{[Key :: term()],reference()}],
              ref = undefined     :: reference() | undefined}).

-type env_tab()   :: dict:dict(label(), #map{}).
-type fun_entry() :: {Args :: [type()], RetType :: type()}.
-type fun_tab()   :: dict:dict('top' | label(),
                               {'not_handled', fun_entry()} | fun_entry()).
-type key()       :: label() | cerl:cerl().
-type type_tab()  :: #{key() => type()}.
-type subst_tab() :: #{key() => cerl:cerl()}.

%% Exported Types

-opaque state() :: #state{}.

%%--------------------------------------------------------------------

-type fun_types() :: orddict:orddict(label(), type()).

-spec get_warnings(cerl:c_module(), dialyzer_plt:plt(),
                   dialyzer_callgraph:callgraph(),
                   dialyzer_codeserver:codeserver(),
                   types()) ->
	{[raw_warning()], fun_types()}.

get_warnings(Tree, Plt, Callgraph, Codeserver, Records) ->
  State1 = analyze_module(Tree, Plt, Callgraph, Codeserver, Records, true),
  State2 = state__renew_warnings(state__get_warnings(State1), State1),
  State3 = state__get_race_warnings(State2),
  {State3#state.warnings, state__all_fun_types(State3)}.

-spec get_fun_types(cerl:c_module(), dialyzer_plt:plt(),
                    dialyzer_callgraph:callgraph(),
                    dialyzer_codeserver:codeserver(),
                    types()) -> fun_types().

get_fun_types(Tree, Plt, Callgraph, Codeserver, Records) ->
  State = analyze_module(Tree, Plt, Callgraph, Codeserver, Records, false),
  state__all_fun_types(State).

%%% ===========================================================================
%%%
%%%  The analysis.
%%%
%%% ===========================================================================

analyze_module(Tree, Plt, Callgraph, Codeserver, Records, GetWarnings) ->
  debug_pp(Tree, false),
  Module = cerl:atom_val(cerl:module_name(Tree)),
  TopFun = cerl:ann_c_fun([{label, top}], [], Tree),
  State = state__new(Callgraph, Codeserver, TopFun, Plt, Module, Records),
  State1 = state__race_analysis(not GetWarnings, State),
  State2 = analyze_loop(State1),
  case GetWarnings of
    true ->
      State3 = state__set_warning_mode(State2),
      State4 = analyze_loop(State3),
      dialyzer_races:race(State4);
    false ->
      State2
  end.

analyze_loop(State) ->
  case state__get_work(State) of
    none -> state__set_curr_fun(undefined, State);
    {Fun, NewState0} ->
      NewState1 = state__set_curr_fun(get_label(Fun), NewState0),
      {ArgTypes, IsCalled} = state__get_args_and_status(Fun, NewState1),
      case not IsCalled of
	true ->
	  ?debug("Not handling (not called) ~w: ~ts\n",
		 [NewState1#state.curr_fun,
		  t_to_string(t_product(ArgTypes))]),
	  analyze_loop(NewState1);
	false ->
	  case state__fun_env(Fun, NewState1) of
	    none ->
	      ?debug("Not handling (no env) ~w: ~ts\n",
		     [NewState1#state.curr_fun,
		      t_to_string(t_product(ArgTypes))]),
	      analyze_loop(NewState1);
	    Map ->
	      ?debug("Handling fun ~p: ~ts\n",
		     [NewState1#state.curr_fun,
		      t_to_string(state__fun_type(Fun, NewState1))]),
	      Vars = cerl:fun_vars(Fun),
	      Map1 = enter_type_lists(Vars, ArgTypes, Map),
	      Body = cerl:fun_body(Fun),
              FunLabel = get_label(Fun),
	      IsRaceAnalysisEnabled = is_race_analysis_enabled(State),
              NewState3 =
                case IsRaceAnalysisEnabled of
                  true ->
                    NewState2 = state__renew_curr_fun(
                      state__lookup_name(FunLabel, NewState1), FunLabel,
                      NewState1),
                    state__renew_race_list([], 0, NewState2);
		  false -> NewState1
                end,
	      {NewState4, _Map2, BodyType} =
		traverse(Body, Map1, NewState3),
	      ?debug("Done analyzing: ~w:~ts\n",
		     [NewState1#state.curr_fun,
		      t_to_string(t_fun(ArgTypes, BodyType))]),
              NewState5 =
                case IsRaceAnalysisEnabled of
                  true -> renew_race_code(NewState4);
                  false -> NewState4
                end,
              NewState6 =
                state__update_fun_entry(Fun, ArgTypes, BodyType, NewState5),
              ?debug("done adding stuff for ~tw\n",
                     [state__lookup_name(get_label(Fun), State)]),
              analyze_loop(NewState6)
	  end
      end
  end.

traverse(Tree, Map, State) ->
  ?debug("Handling ~p\n", [cerl:type(Tree)]),
  %% debug_pp_map(Map),
  case cerl:type(Tree) of
    alias ->
      %% This only happens when checking for illegal record patterns
      %% so the handling is a bit rudimentary.
      traverse(cerl:alias_pat(Tree), Map, State);
    apply ->
      handle_apply(Tree, Map, State);
    binary ->
      Segs = cerl:binary_segments(Tree),
      {State1, Map1, SegTypes} = traverse_list(Segs, Map, State),
      {State1, Map1, t_bitstr_concat(SegTypes)};
    bitstr ->
      handle_bitstr(Tree, Map, State);
    call ->
      handle_call(Tree, Map, State);
    'case' ->
      handle_case(Tree, Map, State);
    'catch' ->
      {State1, _Map1, _} = traverse(cerl:catch_body(Tree), Map, State),
      {State1, Map, t_any()};
    cons ->
      handle_cons(Tree, Map, State);
    'fun' ->
      Type = state__fun_type(Tree, State),
      case state__warning_mode(State) of
        true -> {State, Map, Type};
        false ->
          FunLbl = get_label(Tree),
          State2 = state__add_work(FunLbl, State),
          State3 = state__update_fun_env(Tree, Map, State2),
          State4 = state__add_reachable(FunLbl, State3),
          {State4, Map, Type}
      end;
    'let' ->
      handle_let(Tree, Map, State);
    letrec ->
      Defs = cerl:letrec_defs(Tree),
      Body = cerl:letrec_body(Tree),
      %% By not including the variables in scope we can assure that we
      %% will get the current function type when using the variables.
      FoldFun = fun({Var, Fun}, {AccState, AccMap}) ->
		    {NewAccState, NewAccMap0, FunType} =
		      traverse(Fun, AccMap, AccState),
		    NewAccMap = enter_type(Var, FunType, NewAccMap0),
		    {NewAccState, NewAccMap}
		end,
      {State1, Map1} = lists:foldl(FoldFun, {State, Map}, Defs),
      traverse(Body, Map1, State1);
    literal ->
      Type = literal_type(Tree),
      {State, Map, Type};
    module ->
      handle_module(Tree, Map, State);
    primop ->
      case cerl:atom_val(cerl:primop_name(Tree)) of
        match_fail ->
          {State, Map, t_none()};
        raise ->
          {State, Map, t_none()};
        bs_init_writable ->
          {State, Map, t_from_term(<<>>)};
        build_stacktrace ->
          {State, Map, erl_bif_types:type(erlang, build_stacktrace, 0)};
        dialyzer_unknown ->
          {State, Map, t_any()};
        recv_peek_message ->
          {State, Map, t_product([t_boolean(), t_any()])};
        recv_wait_timeout ->
          [Arg] = cerl:primop_args(Tree),
          {State1, Map1, TimeoutType} = traverse(Arg, Map, State),
          Opaques = State1#state.opaques,
          case t_is_atom(TimeoutType, Opaques) andalso
            t_atom_vals(TimeoutType, Opaques) =:= ['infinity'] of
            true ->
              {State1, Map1, t_boolean()};
            false ->
              {State1, Map1, t_boolean()}
          end;
        remove_message ->
          {State, Map, t_any()};
        timeout ->
          {State, Map, t_any()};
        Other -> erlang:error({'Unsupported primop', Other})
      end;
    'receive' ->
      handle_receive(Tree, Map, State);
    seq ->
      Arg = cerl:seq_arg(Tree),
      Body = cerl:seq_body(Tree),
      {State1, Map1, ArgType} = SMA = traverse(Arg, Map, State),
      case t_is_none_or_unit(ArgType) of
	true ->
	  SMA;
	false ->
	  State2 =
	    case
              t_is_any(ArgType)
              orelse t_is_simple(ArgType, State)
              orelse is_call_to_send(Arg)
	      orelse is_lc_simple_list(Arg, ArgType, State)
            of
	      true -> % do not warn in these cases
		State1;
	      false ->
		state__add_warning(State1, ?WARN_UNMATCHED_RETURN, Arg,
				   {unmatched_return,
				    [format_type(ArgType, State1)]})
	    end,
	  traverse(Body, Map1, State2)
      end;
    'try' ->
      handle_try(Tree, Map, State);
    tuple ->
      handle_tuple(Tree, Map, State);
    map ->
      handle_map(Tree, Map, State);
    values ->
      Elements = cerl:values_es(Tree),
      {State1, Map1, EsType} = traverse_list(Elements, Map, State),
      Type = t_product(EsType),
      {State1, Map1, Type};
    var ->
      ?debug("Looking up unknown variable: ~p\n", [Tree]),
      case state__lookup_type_for_letrec(Tree, State) of
	error ->
	  LType = lookup_type(Tree, Map),
          {State, Map, LType};
	{ok, Type} -> {State, Map, Type}
      end;
    Other ->
      erlang:error({'Unsupported type', Other})
  end.

traverse_list(Trees, Map, State) ->
  traverse_list(Trees, Map, State, []).

traverse_list([Tree|Tail], Map, State, Acc) ->
  {State1, Map1, Type} = traverse(Tree, Map, State),
  traverse_list(Tail, Map1, State1, [Type|Acc]);
traverse_list([], Map, State, Acc) ->
  {State, Map, lists:reverse(Acc)}.

%%________________________________________
%%
%% Special instructions
%%

handle_apply(Tree, Map, State) ->
  Args = cerl:apply_args(Tree),
  Op = cerl:apply_op(Tree),
  {State0, Map1, ArgTypes} = traverse_list(Args, Map, State),
  {State1, Map2, OpType} = traverse(Op, Map1, State0),
  case any_none(ArgTypes) of
    true ->
      {State1, Map2, t_none()};
    false ->
      FunList =
	case state__lookup_call_site(Tree, State) of
	  error -> [external]; %% so that we go directly in the fallback
	  {ok, List} -> List
	end,
      FunInfoList = [{local, state__fun_info(Fun, State)} || Fun <- FunList],
      case
        handle_apply_or_call(FunInfoList, Args, ArgTypes, Map2, Tree, State1)
      of
	{had_external, State2} ->
	  %% Fallback: use whatever info we collected from traversing the op
	  %% instead of the result that has been generalized to t_any().
	  Arity = length(Args),
	  OpType1 = t_inf(OpType, t_fun(Arity, t_any())),
	  case t_is_none(OpType1) of
	    true ->
	      Msg = {fun_app_no_fun,
		     [format_cerl(Op), format_type(OpType, State2), Arity]},
	      State3 = state__add_warning(State2, ?WARN_FAILING_CALL,
					  Tree, Msg),
	      {State3, Map2, t_none()};
	    false ->
	      NewArgs = t_inf_lists(ArgTypes,
                                    t_fun_args(OpType1, 'universe')),
	      case any_none(NewArgs) of
		true ->
                  EnumNewArgs = lists:zip(lists:seq(1, length(NewArgs)),
                                          NewArgs),
                  ArgNs = [Arg ||
                            {Arg, Type} <- EnumNewArgs, t_is_none(Type)],
		  Msg = {fun_app_args,
			 [ArgNs,
			  format_args(Args, ArgTypes, State),
			  format_type(OpType, State)]},
		  State3 = state__add_warning(State2, ?WARN_FAILING_CALL,
					      Tree, Msg),
		  {State3, enter_type(Op, OpType1, Map2), t_none()};
		false ->
		  Map3 = enter_type_lists(Args, NewArgs, Map2),
		  Range0 = t_fun_range(OpType1, 'universe'),
		  Range =
		    case t_is_unit(Range0) of
		      true  -> t_none();
		      false -> Range0
		    end,
		  {State2, enter_type(Op, OpType1, Map3), Range}
	      end
	  end;
	Normal -> Normal
      end
  end.

handle_apply_or_call(FunInfoList, Args, ArgTypes, Map, Tree, State) ->
  None = t_none(),
  %% Call-site analysis may be inaccurate and consider more funs than those that
  %% are actually possible. If all of them are incorrect, then warnings can be
  %% emitted. If at least one fun is ok, however, then no warning is emitted,
  %% just in case the bad ones are not really possible. The last argument is
  %% used for this, with the following encoding:
  %%   Initial value: {none, []}
  %%   First fun checked: {one, <List of warns>}
  %%   More funs checked: {many, <List of warns>}
  %% A '{one, []}' can only become '{many, []}'.
  %% If at any point an fun does not add warnings, then the list is also
  %% replaced with an empty list.
  handle_apply_or_call(FunInfoList, Args, ArgTypes, Map, Tree, State,
		       [None || _ <- ArgTypes], None, false, {none, []}).

handle_apply_or_call([{local, external}|Left], Args, ArgTypes, Map, Tree, State,
		     _AccArgTypes, _AccRet, _HadExternal, Warns) ->
  {HowMany, _} = Warns,
  NewHowMany =
    case HowMany of
      none -> one;
      _ -> many
    end,
  NewWarns = {NewHowMany, []},      
  handle_apply_or_call(Left, Args, ArgTypes, Map, Tree, State,
		       ArgTypes, t_any(), true, NewWarns);
handle_apply_or_call([{TypeOfApply, {Fun, Sig, Contr, LocalRet}}|Left],
		     Args, ArgTypes, Map, Tree,
                     #state{opaques = Opaques} = State,
                     AccArgTypes, AccRet, HadExternal, Warns) ->
  Any = t_any(),
  AnyArgs = [Any || _ <- Args],
  GenSig = {AnyArgs, fun(_) -> t_any() end},
  {CArgs, CRange} =
    case Contr of
      {value, #contract{args = As} = C} ->
	{As, fun(FunArgs) ->
		 dialyzer_contracts:get_contract_return(C, FunArgs)
	     end};
      none -> GenSig
    end,
  {BifArgs, BifRange} =
    case TypeOfApply of
      remote ->
	{M, F, A} = Fun,
	case erl_bif_types:is_known(M, F, A) of
	  true ->
	    BArgs = erl_bif_types:arg_types(M, F, A),
	    BRange =
	      fun(FunArgs) ->
		  erl_bif_types:type(M, F, A, FunArgs, Opaques)
	      end,
	    {BArgs, BRange};
          false ->
            GenSig
	end;
      local -> GenSig
    end,
  {SigArgs, SigRange} =
    case Sig of
      {value, {SR, SA}} -> {SA, SR};
      none -> {AnyArgs, t_any()}
    end,

  ?debug("--------------------------------------------------------\n", []),
  ?debug("Fun: ~tp\n", [state__lookup_name(Fun, State)]),
  ?debug("Module ~p\n", [State#state.module]),
  ?debug("CArgs ~ts\n", [erl_types:t_to_string(t_product(CArgs))]),
  ?debug("ArgTypes ~ts\n", [erl_types:t_to_string(t_product(ArgTypes))]),
  ?debug("BifArgs ~tp\n", [erl_types:t_to_string(t_product(BifArgs))]),

  NewArgsSig = t_inf_lists(SigArgs, ArgTypes, Opaques),
  ?debug("SigArgs ~ts\n", [erl_types:t_to_string(t_product(SigArgs))]),
  ?debug("NewArgsSig: ~ts\n", [erl_types:t_to_string(t_product(NewArgsSig))]),
  NewArgsContract = t_inf_lists(CArgs, ArgTypes, Opaques),
  ?debug("NewArgsContract: ~ts\n",
	 [erl_types:t_to_string(t_product(NewArgsContract))]),
  NewArgsBif = t_inf_lists(BifArgs, ArgTypes, Opaques),
  ?debug("NewArgsBif: ~ts\n", [erl_types:t_to_string(t_product(NewArgsBif))]),
  NewArgTypes0 = t_inf_lists(NewArgsSig, NewArgsContract),
  NewArgTypes = t_inf_lists(NewArgTypes0, NewArgsBif, Opaques),
  ?debug("NewArgTypes ~ts\n", [erl_types:t_to_string(t_product(NewArgTypes))]),
  ?debug("\n", []),

  BifRet = BifRange(NewArgTypes),
  ContrRet = CRange(NewArgTypes),
  RetWithoutContr = t_inf(SigRange, BifRet),
  RetWithoutLocal = t_inf(ContrRet, RetWithoutContr),

  ?debug("RetWithoutContr: ~ts\n",[erl_types:t_to_string(RetWithoutContr)]),
  ?debug("RetWithoutLocal: ~ts\n", [erl_types:t_to_string(RetWithoutLocal)]),
  ?debug("BifRet: ~ts\n", [erl_types:t_to_string(BifRange(NewArgTypes))]),
  ?debug("SigRange: ~ts\n", [erl_types:t_to_string(SigRange)]),
  ?debug("ContrRet: ~ts\n", [erl_types:t_to_string(ContrRet)]),
  ?debug("LocalRet: ~ts\n", [erl_types:t_to_string(LocalRet)]),

  State1 =
    case is_race_analysis_enabled(State) of
      true ->
        Ann = cerl:get_ann(Tree),
        File = get_file(Ann, State),
        Line = abs(get_line(Ann)),
        dialyzer_races:store_race_call(Fun, ArgTypes, Args,
                                       {File, Line}, State);
      false -> State
    end,
  FailedConj = any_none([RetWithoutLocal|NewArgTypes]),
  IsFailBif = t_is_none(BifRange(BifArgs)),
  IsFailSig = t_is_none(SigRange),
  ?debug("FailedConj: ~p~n", [FailedConj]),
  ?debug("IsFailBif: ~p~n", [IsFailBif]),
  ?debug("IsFailSig: ~p~n", [IsFailSig]),
  State2 =
    case FailedConj andalso not (IsFailBif orelse IsFailSig) of
      true ->
	case t_is_none(RetWithoutLocal) andalso
	  not t_is_none(RetWithoutContr) andalso
	  not any_none(NewArgTypes) of
	  true ->
	    {value, C1} = Contr,
	    Contract = dialyzer_contracts:contract_to_string(C1),
	    {M1, F1, A1} = state__lookup_name(Fun, State),
	    ArgStrings = format_args(Args, ArgTypes, State),
	    CRet = erl_types:t_to_string(RetWithoutContr),
	    %% This Msg will be post_processed by dialyzer_succ_typings
	    Msg =
	      {contract_range, [Contract, M1, F1, A1, ArgStrings, CRet]},
	    state__add_warning(State1, ?WARN_CONTRACT_RANGE, Tree, Msg);
	  false ->
	    FailedSig = any_none(NewArgsSig),
	    FailedContract =
	      any_none([CRange(NewArgsContract)|NewArgsContract]),
	    FailedBif = any_none([BifRange(NewArgsBif)|NewArgsBif]),
	    InfSig = t_inf(t_fun(SigArgs, SigRange),
                           t_fun(BifArgs, BifRange(BifArgs))),
	    FailReason =
	      apply_fail_reason(FailedSig, FailedBif, FailedContract),
	    Msg = get_apply_fail_msg(Fun, Args, ArgTypes, NewArgTypes, InfSig,
				     Contr, CArgs, State1, FailReason, Opaques),
	    WarnType = case Msg of
			 {call, _} -> ?WARN_FAILING_CALL;
			 {apply, _} -> ?WARN_FAILING_CALL;
			 {call_with_opaque, _} -> ?WARN_OPAQUE;
			 {call_without_opaque, _} -> ?WARN_OPAQUE;
			 {opaque_type_test, _} -> ?WARN_OPAQUE
		       end,
            Frc = {erlang, is_record, 3} =:= state__lookup_name(Fun, State),
	    state__add_warning(State1, WarnType, Tree, Msg, Frc)
	end;
      false -> State1
    end,
  State3 =
    case TypeOfApply of
      local ->
        case state__is_escaping(Fun, State2) of
          true -> State2;
          false ->
            ForwardArgs = [t_limit(X, ?TYPE_LIMIT) || X <- ArgTypes],
            forward_args(Fun, ForwardArgs, State2)
        end;
      remote ->
        add_bif_warnings(Fun, NewArgTypes, Tree, State2)
    end,
  NewAccArgTypes =
    case FailedConj of
      true -> AccArgTypes;
      false -> [t_sup(X, Y) || {X, Y} <- lists:zip(NewArgTypes, AccArgTypes)]
    end,
  TotalRet =
    case t_is_none(LocalRet) andalso t_is_unit(RetWithoutLocal) of
      true -> RetWithoutLocal;
      false -> t_inf(RetWithoutLocal, LocalRet)
    end,
  NewAccRet = t_sup(AccRet, TotalRet),
  ?debug("NewAccRet: ~ts\n", [t_to_string(NewAccRet)]),
  {NewWarnings, State4} = state__remove_added_warnings(State, State3),
  {HowMany, OldWarnings} = Warns,
  NewWarns =
    case HowMany of
      none -> {one, NewWarnings};
      _ ->
        case OldWarnings =:= [] of
          true -> {many, []};
          false ->
            case NewWarnings =:= [] of
              true -> {many, []};
              false -> {many, NewWarnings ++ OldWarnings}
            end
        end
    end,
  handle_apply_or_call(Left, Args, ArgTypes, Map, Tree,
		       State4, NewAccArgTypes, NewAccRet, HadExternal, NewWarns);
handle_apply_or_call([], Args, _ArgTypes, Map, _Tree, State,
		     AccArgTypes, AccRet, HadExternal, {_, Warnings}) ->
  State1 = state__add_warnings(Warnings, State),
  case HadExternal of
    false ->
      NewMap = enter_type_lists(Args, AccArgTypes, Map),
      {State1, NewMap, AccRet};
    true ->
      {had_external, State1}
  end.

apply_fail_reason(FailedSig, FailedBif, FailedContract) ->
  if
    (FailedSig orelse FailedBif) andalso (not FailedContract) -> only_sig;
    FailedContract andalso (not (FailedSig orelse FailedBif)) -> only_contract;
    true                                                      -> both
  end.

get_apply_fail_msg(Fun, Args, ArgTypes, NewArgTypes,
		   Sig, Contract, ContrArgs, State, FailReason, Opaques) ->
  ArgStrings = format_args(Args, ArgTypes, State),
  ContractInfo =
    case Contract of
      {value, #contract{} = C} ->
	{dialyzer_contracts:is_overloaded(C),
	 dialyzer_contracts:contract_to_string(C)};
      none -> {false, none}
    end,
  EnumArgTypes = lists:zip(lists:seq(1, length(NewArgTypes)), NewArgTypes),
  ArgNs = [Arg || {Arg, Type} <- EnumArgTypes, t_is_none(Type)],
  case state__lookup_name(Fun, State) of
    {M, F, A} ->
      case is_opaque_type_test_problem(Fun, Args, NewArgTypes, State) of
	{yes, Arg, ArgType} ->
	  {opaque_type_test, [atom_to_list(F), ArgStrings,
                              format_arg(Arg), format_type(ArgType, State)]};
	no ->
	  SigArgs = t_fun_args(Sig),
          BadOpaque =
            opaque_problems([SigArgs, ContrArgs], ArgTypes, Opaques, ArgNs),
          %% In fact *both* 'call_with_opaque' and
          %% 'call_without_opaque' are possible.
          case lists:keyfind(decl, 1, BadOpaque) of
            {decl, BadArgs} ->
              %% a structured term is used where an opaque is expected
              ExpectedTriples =
                case FailReason of
                  only_sig -> expected_arg_triples(BadArgs, SigArgs, State);
                  _ -> expected_arg_triples(BadArgs, ContrArgs, State)
                end,
              {call_without_opaque, [M, F, ArgStrings, ExpectedTriples]};
            false ->
              case lists:keyfind(use, 1, BadOpaque) of
                {use, BadArgs} ->
                  %% an opaque term is used where a structured term is expected
                  ExpectedArgs =
                    case FailReason of
                      only_sig -> SigArgs;
                      _ -> ContrArgs
                    end,
                  {call_with_opaque, [M, F, ArgStrings, BadArgs, ExpectedArgs]};
                false ->
                  case
                    erl_bif_types:opaque_args(M, F, A, ArgTypes, Opaques)
                  of
                    [] ->  %% there is a structured term clash in some argument
                      {call, [M, F, ArgStrings,
                              ArgNs, FailReason,
                              format_sig_args(Sig, State),
                              format_type(t_fun_range(Sig), State),
                              ContractInfo]};
                    Ns ->
                      {call_with_opaque, [M, F, ArgStrings, Ns, ContrArgs]}
                  end
	      end
	  end
      end;
    Label when is_integer(Label) ->
      {apply, [ArgStrings,
	       ArgNs, FailReason,
	       format_sig_args(Sig, State),
	       format_type(t_fun_range(Sig), State),
	       ContractInfo]}
  end.

%% -> [{ElementI, [ArgN]}] where [ArgN] is a non-empty list of
%% arguments containing unknown opaque types and Element is 1 or 2.
opaque_problems(ContractOrSigList, ArgTypes, Opaques, ArgNs) ->
  ArgElementList = find_unknown(ContractOrSigList, ArgTypes, Opaques, ArgNs),
  F = fun(1) -> decl; (2) -> use end,
  [{F(ElementI), lists:usort([ArgN || {ArgN, EI} <- ArgElementList,
                                      EI =:= ElementI])} ||
    ElementI <- lists:usort([EI || {_, EI} <- ArgElementList])].

%% -> [{ArgN, ElementI}] where ElementI = 1 means there is an unknown
%% opaque type in argument ArgN of the the contract/signature,
%% and ElementI = 2 means that there is an unknown opaque type in
%% argument ArgN of the the (current) argument types.
find_unknown(ContractOrSigList, ArgTypes, Opaques, NoneArgNs) ->
  ArgNs = lists:seq(1, length(ArgTypes)),
  [{ArgN, ElementI} ||
    ContractOrSig <- ContractOrSigList,
    {E1, E2, ArgN} <- lists:zip3(ContractOrSig, ArgTypes, ArgNs),
    lists:member(ArgN, NoneArgNs),
    ElementI <- erl_types:t_find_unknown_opaque(E1, E2, Opaques)].

is_opaque_type_test_problem(Fun, Args, ArgTypes, State) ->
  case Fun of
    {erlang, FN, 1} when FN =:= is_atom;      FN =:= is_boolean;
			 FN =:= is_binary;    FN =:= is_bitstring;
			 FN =:= is_float;     FN =:= is_function;
			 FN =:= is_integer;   FN =:= is_list;
			 FN =:= is_number;    FN =:= is_pid; FN =:= is_port;
			 FN =:= is_reference; FN =:= is_tuple;
			 FN =:= is_map ->
      type_test_opaque_arg(Args, ArgTypes, State#state.opaques);
    {erlang, FN, 2} when FN =:= is_function ->
      type_test_opaque_arg(Args, ArgTypes, State#state.opaques);
    _ -> no
  end.

type_test_opaque_arg([], [], _Opaques) ->
  no;
type_test_opaque_arg([Arg|Args], [ArgType|ArgTypes], Opaques) ->
  case erl_types:t_has_opaque_subtype(ArgType, Opaques) of
    true -> {yes, Arg, ArgType};
    false -> type_test_opaque_arg(Args, ArgTypes, Opaques)
  end.

expected_arg_triples(ArgNs, ArgTypes, State) ->
  [begin
     Arg = lists:nth(N, ArgTypes),
     {N, Arg, format_type(Arg, State)}
   end || N <- ArgNs].

add_bif_warnings({erlang, Op, 2}, [T1, T2] = Ts, Tree, State)
  when Op =:= '=:='; Op =:= '==' ->
  Opaques = State#state.opaques,
  Inf = t_inf(T1, T2, Opaques),
  case
    t_is_none(Inf) andalso (not any_none(Ts))
    andalso (not is_int_float_eq_comp(T1, Op, T2, Opaques))
  of
    true ->
      %% Give priority to opaque warning (as usual).
      case erl_types:t_find_unknown_opaque(T1, T2, Opaques) of
        [] ->
          Args = comp_format_args([], T1, Op, T2, State),
          state__add_warning(State, ?WARN_MATCHING, Tree, {exact_eq, Args});
        Ns ->
          Args = comp_format_args(Ns, T1, Op, T2, State),
	  state__add_warning(State, ?WARN_OPAQUE, Tree, {opaque_eq, Args})
      end;
    false ->
      State
  end;
add_bif_warnings({erlang, Op, 2}, [T1, T2] = Ts, Tree, State)
  when Op =:= '=/='; Op =:= '/=' ->
  Opaques = State#state.opaques,
  case
    (not any_none(Ts))
    andalso (not is_int_float_eq_comp(T1, Op, T2, Opaques))
  of
    true ->
      case erl_types:t_find_unknown_opaque(T1, T2, Opaques) of
        [] -> State;
        Ns ->
          Args = comp_format_args(Ns, T1, Op, T2, State),
	  state__add_warning(State, ?WARN_OPAQUE, Tree, {opaque_neq, Args})
      end;
    false ->
      State
  end;
add_bif_warnings(_, _, _, State) ->
  State.

is_int_float_eq_comp(T1, Op, T2, Opaques) ->
  (Op =:= '==' orelse Op =:= '/=') andalso
    ((erl_types:t_is_float(T1, Opaques)
      andalso t_is_integer(T2, Opaques)) orelse
     (t_is_integer(T1, Opaques)
      andalso erl_types:t_is_float(T2, Opaques))).

comp_format_args([1|_], T1, Op, T2, State) ->
  [format_type(T2, State), Op, format_type(T1, State)];
comp_format_args(_, T1, Op, T2, State) ->
  [format_type(T1, State), Op, format_type(T2, State)].

%%----------------------------------------

handle_bitstr(Tree, Map, State) ->
  %% Construction of binaries.
  Size = cerl:bitstr_size(Tree),
  Val = cerl:bitstr_val(Tree),
  BitstrType = cerl:concrete(cerl:bitstr_type(Tree)),
  {State1, Map1, SizeType0} = traverse(Size, Map, State),
  {State2, Map2, ValType0} = traverse(Val, Map1, State1),
  case cerl:bitstr_bitsize(Tree) of
    BitSz when BitSz =:= all orelse BitSz =:= utf ->
      ValType =
	case BitSz of
	  all ->
	    true = (BitstrType =:= binary),
	    t_inf(ValType0, t_bitstr());
	  utf ->
	    true = lists:member(BitstrType, [utf8, utf16, utf32]),
	    t_inf(ValType0, t_integer())
	end,
      Map3 = enter_type(Val, ValType, Map2),
      case t_is_none(ValType) of
	true ->
	  Msg = {bin_construction, ["value",
				    format_cerl(Val), format_cerl(Tree),
				    format_type(ValType0, State2)]},
	  State3 = state__add_warning(State2, ?WARN_BIN_CONSTRUCTION, Val, Msg),
	  {State3, Map3, t_none()};
	false ->
	  {State2, Map3, t_bitstr()}
      end;
    BitSz when is_integer(BitSz) orelse BitSz =:= any ->
      SizeType = t_inf(SizeType0, t_non_neg_integer()),
      ValType =
	case BitstrType of
	  binary -> t_inf(ValType0, t_bitstr());
	  float -> t_inf(ValType0, t_number());
	  integer -> t_inf(ValType0, t_integer())
	end,
      case any_none([SizeType, ValType]) of
	true ->
	  {Msg, Offending} =
	    case t_is_none(SizeType) of
	      true ->
		{{bin_construction,
		  ["size", format_cerl(Size), format_cerl(Tree),
		   format_type(SizeType0, State2)]},
		 Size};
	      false ->
		{{bin_construction,
		  ["value", format_cerl(Val), format_cerl(Tree),
		   format_type(ValType0, State2)]},
		 Val}
	    end,
	  State3 = state__add_warning(State2, ?WARN_BIN_CONSTRUCTION,
				      Offending, Msg),
	  {State3, Map2, t_none()};
	false ->
	  UnitVal = cerl:concrete(cerl:bitstr_unit(Tree)),
          Opaques = State2#state.opaques,
          NumberVals = t_number_vals(SizeType, Opaques),
          {State3, Type} =
            case t_contains_opaque(SizeType, Opaques) of
              true ->
                Msg = {opaque_size, [format_type(SizeType, State2),
                                     format_cerl(Size)]},
                {state__add_warning(State2, ?WARN_OPAQUE, Size, Msg),
                 t_none()};
              false ->
                case NumberVals of
                  [OneSize] -> {State2, t_bitstr(0, OneSize * UnitVal)};
                  unknown -> {State2, t_bitstr()};
                  _ ->
                    MinSize = erl_types:number_min(SizeType, Opaques),
                    {State2, t_bitstr(UnitVal, UnitVal * MinSize)}
                end
            end,
	  Map3 = enter_type_lists([Val, Size, Tree],
				  [ValType, SizeType, Type], Map2),
	  {State3, Map3, Type}
      end
  end.

%%----------------------------------------

handle_call(Tree, Map, State) ->
  M = cerl:call_module(Tree),
  F = cerl:call_name(Tree),
  Args = cerl:call_args(Tree),
  MFAList = [M, F|Args],
  {State1, Map1, [MType0, FType0|As]} = traverse_list(MFAList, Map, State),
  Opaques = State#state.opaques,
  MType = t_inf(t_module(), MType0, Opaques),
  FType = t_inf(t_atom(), FType0, Opaques),
  Map2 = enter_type_lists([M, F], [MType, FType], Map1),
  MOpaque = t_is_none(MType) andalso (not t_is_none(MType0)),
  FOpaque = t_is_none(FType) andalso (not t_is_none(FType0)),
  case any_none([MType, FType|As]) of
    true ->
      State2 =
        if
          MOpaque -> % This is a problem we just detected; not a known one
            MS = format_cerl(M),
            case t_is_none(t_inf(t_module(), MType0)) of
              true ->
                Msg = {app_call, [MS, format_cerl(F),
                                  format_args(Args, As, State1),
                                  MS, format_type(t_module(), State1),
                                  format_type(MType0, State1)]},
                state__add_warning(State1, ?WARN_FAILING_CALL, Tree, Msg);
              false ->
                Msg = {opaque_call, [MS, format_cerl(F),
                                     format_args(Args, As, State1),
                                     MS, format_type(MType0, State1)]},
                state__add_warning(State1, ?WARN_FAILING_CALL, Tree, Msg)
            end;
          FOpaque ->
            FS = format_cerl(F),
            case t_is_none(t_inf(t_atom(), FType0)) of
              true ->
                Msg = {app_call, [format_cerl(M), FS,
                                  format_args(Args, As, State1),
                                  FS, format_type(t_atom(), State1),
                                  format_type(FType0, State1)]},
                state__add_warning(State1, ?WARN_FAILING_CALL, Tree, Msg);
              false ->
                Msg = {opaque_call, [format_cerl(M), FS,
                                     format_args(Args, As, State1),
                                     FS, format_type(FType0, State1)]},
                state__add_warning(State1, ?WARN_FAILING_CALL, Tree, Msg)
            end;
          true -> State1
	end,
      {State2, Map2, t_none()};
    false ->
      case t_is_atom(MType) of
	true ->
	  %% XXX: Consider doing this for all combinations of MF
	  case {t_atom_vals(MType), t_atom_vals(FType)} of
	    {[MAtom], [FAtom]} ->
	      FunInfo = [{remote, state__fun_info({MAtom, FAtom, length(Args)},
						  State1)}],
	      handle_apply_or_call(FunInfo, Args, As, Map2, Tree, State1);
	    {_MAtoms, _FAtoms} ->
	      {State1, Map2, t_any()}
	  end;
	false ->
	  {State1, Map2, t_any()}
      end
  end.

%%----------------------------------------

handle_case(Tree, Map, State) ->
  Arg = cerl:case_arg(Tree),
  Clauses = cerl:case_clauses(Tree),
  {State1, Map1, ArgType} = SMA = traverse(Arg, Map, State),
  case t_is_none_or_unit(ArgType) of
    true -> SMA;
    false ->
      State2 =
        case is_race_analysis_enabled(State) of
          true ->
	    {RaceList, RaceListSize} = get_race_list_and_size(State1),
            state__renew_race_list([beg_case|RaceList],
                                   RaceListSize + 1, State1);
          false -> State1
        end,
      Map2 = join_maps_begin(Map1),
      {MapList, State3, Type, Warns} =
	handle_clauses(Clauses, Arg, ArgType, ArgType, State2,
		       [], Map2, [], [], []),
      %% Non-Erlang BEAM languages, such as Elixir, expand language constructs
      %% into case statements. In that case, we do not want to warn on
      %% individual clauses not matching unless none of them can.
      SupressForced = is_compiler_generated(cerl:get_ann(Tree))
	andalso not (t_is_none(Type)),
      State4 = lists:foldl(fun({T,R,M,F}, S) ->
			       state__add_warning(
				 S,T,R,M,F andalso (not SupressForced))
			   end, State3, Warns),
      Map3 = join_maps_end(MapList, Map2),
      debug_pp_map(Map3),
      {State4, Map3, Type}
  end.

%%----------------------------------------

handle_cons(Tree, Map, State) ->
  Hd = cerl:cons_hd(Tree),
  Tl = cerl:cons_tl(Tree),
  {State1, Map1, HdType} = traverse(Hd, Map, State),
  {State2, Map2, TlType} = traverse(Tl, Map1, State1),
  State3 =
    case t_is_none(t_inf(TlType, t_list(), State2#state.opaques)) of
      true ->
	Msg = {improper_list_constr, [format_type(TlType, State2)]},
	state__add_warning(State2, ?WARN_NON_PROPER_LIST, Tree, Msg);
      false ->
	State2
    end,
  Type = t_cons(HdType, TlType),
  {State3, Map2, Type}.

%%----------------------------------------

handle_let(Tree, Map, State) ->
  IsRaceAnalysisEnabled = is_race_analysis_enabled(State),
  Arg = cerl:let_arg(Tree),
  Vars = cerl:let_vars(Tree),
  {Map0, State0} =
    case cerl:is_c_var(Arg) of
      true ->
	[Var] = Vars,
	{enter_subst(Var, Arg, Map),
         case IsRaceAnalysisEnabled of
           true ->
	    {RaceList, RaceListSize} = get_race_list_and_size(State),
             state__renew_race_list(
               [dialyzer_races:let_tag_new(Var, Arg)|RaceList],
               RaceListSize + 1, State);
           false -> State
         end};
      false -> {Map, State}
    end,
  Body = cerl:let_body(Tree),
  {State1, Map1, ArgTypes} = SMA = traverse(Arg, Map0, State0),
  State2 =
    case IsRaceAnalysisEnabled andalso cerl:is_c_call(Arg) of
      true ->
        Mod = cerl:call_module(Arg),
        Name = cerl:call_name(Arg),
        case cerl:is_literal(Mod) andalso
             cerl:concrete(Mod) =:= ets andalso
             cerl:is_literal(Name) andalso
             cerl:concrete(Name) =:= new of
          true -> renew_race_public_tables(Vars, State1);
          false -> State1
        end;
      false -> State1
    end,
  case t_is_none_or_unit(ArgTypes) of
    true -> SMA;
    false ->
      Map2 = enter_type_lists(Vars, t_to_tlist(ArgTypes), Map1),
      traverse(Body, Map2, State2)
  end.

%%----------------------------------------

handle_module(Tree, Map, State) ->
  %% By not including the variables in scope we can assure that we
  %% will get the current function type when using the variables.
  Defs = cerl:module_defs(Tree),
  PartFun = fun({_Var, Fun}) ->
		state__is_escaping(get_label(Fun), State)
	    end,
  {Defs1, Defs2} = lists:partition(PartFun, Defs),
  Letrec = cerl:c_letrec(Defs1, cerl:c_int(42)),
  {State1, Map1, _FunTypes} = traverse(Letrec, Map, State),
  %% Also add environments for the other top-level functions.
  VarTypes = [{Var, state__fun_type(Fun, State1)} || {Var, Fun} <- Defs],
  EnvMap = enter_type_list(VarTypes, Map),
  FoldFun = fun({_Var, Fun}, AccState) ->
		state__update_fun_env(Fun, EnvMap, AccState)
	    end,
  State2 = lists:foldl(FoldFun, State1, Defs2),
  {State2, Map1, t_any()}.

%%----------------------------------------

handle_receive(Tree, Map, State) ->
  Clauses = cerl:receive_clauses(Tree),
  Timeout = cerl:receive_timeout(Tree),
  State1 =
    case is_race_analysis_enabled(State) of
      true ->
	{RaceList, RaceListSize} = get_race_list_and_size(State),
        state__renew_race_list([beg_case|RaceList],
                               RaceListSize + 1, State);
      false -> State
    end,
  {MapList, State2, ReceiveType, Warns} =
    handle_clauses(Clauses, ?no_arg, t_any(), t_any(), State1, [], Map,
		   [], [], []),
  State3 = lists:foldl(fun({T,R,M,F}, S) -> state__add_warning(S,T,R,M,F) end,
		       State2, Warns),
  Map1 = join_maps(MapList, Map),
  {State4, Map2, TimeoutType} = traverse(Timeout, Map1, State3),
  Opaques = State4#state.opaques,
  case (t_is_atom(TimeoutType, Opaques) andalso
	(t_atom_vals(TimeoutType, Opaques) =:= ['infinity'])) of
    true ->
      {State4, Map2, ReceiveType};
    false ->
      Action = cerl:receive_action(Tree),
      {State5, Map3, ActionType} = traverse(Action, Map, State4),
      Map4 = join_maps([Map3, Map1], Map),
      Type = t_sup(ReceiveType, ActionType),
      {State5, Map4, Type}
  end.

%%----------------------------------------

handle_try(Tree, Map, State) ->
  Arg = cerl:try_arg(Tree),
  EVars = cerl:try_evars(Tree),
  Vars = cerl:try_vars(Tree),
  Body = cerl:try_body(Tree),
  Handler = cerl:try_handler(Tree),
  {State1, Map1, ArgType} = traverse(Arg, Map, State),
  Map2 = mark_as_fresh(Vars, Map1),
  {SuccState, SuccMap, SuccType} =
    case bind_pat_vars(Vars, t_to_tlist(ArgType), [], Map2, State1) of
      {error, _, _, _, _} ->
	{State1, map__new(), t_none()};
      {SuccMap1, VarTypes} ->
	%% Try to bind the argument. Will only succeed if
	%% it is a simple structured term.
	SuccMap2 =
	  case bind_pat_vars_reverse([Arg], [t_product(VarTypes)], [],
				     SuccMap1, State1) of
	    {error, _, _, _, _} -> SuccMap1;
	    {SM, _} -> SM
	  end,
	traverse(Body, SuccMap2, State1)
    end,
  ExcMap1 = mark_as_fresh(EVars, Map),
  {State2, ExcMap2, HandlerType} = traverse(Handler, ExcMap1, SuccState),
  TryType = t_sup(SuccType, HandlerType),
  {State2, join_maps([ExcMap2, SuccMap], Map1), TryType}.

%%----------------------------------------

handle_map(Tree,Map,State) ->
  Pairs = cerl:map_es(Tree),
  Arg = cerl:map_arg(Tree),
  {State1, Map1, ArgType} = traverse(Arg, Map, State),
  ArgType1 = t_inf(t_map(), ArgType),
  case t_is_none_or_unit(ArgType1) of
    true ->
      {State1, Map1, ArgType1};
    false ->
      {State2, Map2, TypePairs, ExactKeys} =
	traverse_map_pairs(Pairs, Map1, State1, t_none(), [], []),
      InsertPair = fun({KV,assoc,_},Acc) -> erl_types:t_map_put(KV,Acc);
		      ({KV,exact,KVTree},Acc) ->
		       case t_is_none(T=erl_types:t_map_update(KV,Acc)) of
			 true -> throw({none, Acc, KV, KVTree});
			 false -> T
		       end
		   end,
      try lists:foldl(InsertPair, ArgType1, TypePairs)
      of ResT ->
	  BindT = t_map([{K, t_any()} || K <- ExactKeys]),
	  case bind_pat_vars_reverse([Arg], [BindT], [], Map2, State2) of
	    {error, _, _, _, _} -> {State2, Map2, ResT};
	    {Map3, _} ->           {State2, Map3, ResT}
	  end
      catch {none, MapType, {K,_}, KVTree} ->
	  Msg2 = {map_update, [format_type(MapType, State2),
			       format_type(K, State2)]},
	  {state__add_warning(State2, ?WARN_MAP_CONSTRUCTION, KVTree, Msg2),
	   Map2, t_none()}
      end
  end.

traverse_map_pairs([], Map, State, _ShadowKeys, PairAcc, KeyAcc) ->
  {State, Map, lists:reverse(PairAcc), KeyAcc};
traverse_map_pairs([Pair|Pairs], Map, State, ShadowKeys, PairAcc, KeyAcc) ->
  Key = cerl:map_pair_key(Pair),
  Val = cerl:map_pair_val(Pair),
  Op = cerl:map_pair_op(Pair),
  {State1, Map1, [K,V]} = traverse_list([Key,Val],Map,State),
  KeyAcc1 =
    case cerl:is_literal(Op) andalso cerl:concrete(Op) =:= exact andalso
      t_is_singleton(K, State#state.opaques) andalso
      t_is_none(t_inf(ShadowKeys, K)) of
      true -> [K|KeyAcc];
      false -> KeyAcc
  end,
  traverse_map_pairs(Pairs, Map1, State1, t_sup(K, ShadowKeys),
		     [{{K,V},cerl:concrete(Op),Pair}|PairAcc], KeyAcc1).

%%----------------------------------------

handle_tuple(Tree, Map, State) ->
  Elements = cerl:tuple_es(Tree),
  {State1, Map1, EsType} = traverse_list(Elements, Map, State),
  TupleType = t_tuple(EsType),
  case t_is_none(TupleType) of
    true ->
      {State1, Map1, t_none()};
    false ->
      %% Let's find out if this is a record
      case Elements of
	[Tag|Left] ->
	  case cerl:is_c_atom(Tag) andalso is_literal_record(Tree) of
	    true ->
	      TagVal = cerl:atom_val(Tag),
              case state__lookup_record(TagVal, length(Left), State1) of
                error -> {State1, Map1, TupleType};
                {ok, RecType, FieldNames} ->
                  InfTupleType = t_inf(RecType, TupleType),
                  case t_is_none(InfTupleType) of
                    true ->
                      RecC = format_type(TupleType, State1),
                      FieldDiffs = format_field_diffs(TupleType, State1),
                      Msg = {record_constr, [RecC, FieldDiffs]},
                      State2 = state__add_warning(State1, ?WARN_MATCHING,
                                                  Tree, Msg),
                      {State2, Map1, t_none()};
                    false ->
                      case bind_pat_vars(Elements, t_tuple_args(RecType),
                                         [], Map1, State1) of
                        {error, bind, ErrorPat, ErrorType, _} ->
                          Msg = {record_constr,
                                 [TagVal, format_patterns(ErrorPat),
                                  format_type(ErrorType, State1)]},
                          State2 = state__add_warning(State1, ?WARN_MATCHING,
                                                      Tree, Msg),
                          {State2, Map1, t_none()};
                        {error, opaque, ErrorPat, ErrorType, OpaqueType} ->
                          OpaqueStr = format_type(OpaqueType, State1),
                          Name = field_name(Elements, ErrorPat, FieldNames),
                          Msg = {opaque_match,
                                 ["record field" ++ Name ++
                                  " declared to be of type " ++
                                    format_type(ErrorType, State1),
                                  OpaqueStr, OpaqueStr]},
                          State2 = state__add_warning(State1, ?WARN_OPAQUE,
                                                      Tree, Msg),
                          {State2, Map1, t_none()};
                        {error, record, ErrorPat, ErrorType, _} ->
                          Msg = {record_match,
                                 [format_patterns(ErrorPat),
                                  format_type(ErrorType, State1)]},
                          State2 = state__add_warning(State1, ?WARN_MATCHING,
                                                      Tree, Msg),
                          {State2, Map1, t_none()};
                        {Map2, ETypes} ->
                          {State1, Map2, t_tuple(ETypes)}
                      end
                  end
	      end;
	    false ->
	      {State1, Map1, t_tuple(EsType)}
	  end;
	[] ->
	  {State1, Map1, t_tuple([])}
      end
  end.

field_name(Elements, ErrorPat, FieldNames) ->
  try
    [Pat] = ErrorPat,
    Take = lists:takewhile(fun(X) -> X =/= Pat end, Elements),
    " " ++ format_atom(lists:nth(length(Take), FieldNames))
  catch
    _:_ -> ""
  end.

%%----------------------------------------
%% Clauses
%%
handle_clauses([C|Left], Arg, ArgType, OrigArgType, State, CaseTypes, MapIn,
	       Acc, ClauseAcc, WarnAcc0) ->
  IsRaceAnalysisEnabled = is_race_analysis_enabled(State),
  State1 =
    case IsRaceAnalysisEnabled of
      true ->
	{RaceList, RaceListSize} = get_race_list_and_size(State),
        state__renew_race_list(
          [dialyzer_races:beg_clause_new(Arg, cerl:clause_pats(C),
                                         cerl:clause_guard(C))|
           RaceList], RaceListSize + 1,
          State);
      false -> State
    end,
  {State2, ClauseMap, BodyType, NewArgType, WarnAcc} =
    do_clause(C, Arg, ArgType, OrigArgType, MapIn, State1, WarnAcc0),
  {NewClauseAcc, State3} =
    case IsRaceAnalysisEnabled of
      true ->
	{RaceList1, RaceListSize1} = get_race_list_and_size(State2),
        EndClause = dialyzer_races:end_clause_new(Arg, cerl:clause_pats(C),
                                                  cerl:clause_guard(C)),
        {[EndClause|ClauseAcc],
         state__renew_race_list([EndClause|RaceList1],
                                RaceListSize1 + 1, State2)};
      false -> {ClauseAcc, State2}
    end,
  {NewCaseTypes, NewAcc} =
    case t_is_none(BodyType) of
      true -> {CaseTypes, Acc};
      false -> {[BodyType|CaseTypes], [ClauseMap|Acc]}
    end,
  handle_clauses(Left, Arg, NewArgType, OrigArgType, State3,
		 NewCaseTypes, MapIn, NewAcc, NewClauseAcc, WarnAcc);
handle_clauses([], _Arg, _ArgType, _OrigArgType, State, CaseTypes, _MapIn, Acc,
	       ClauseAcc, WarnAcc) ->
  State1 =
    case is_race_analysis_enabled(State) of
      true ->
	{RaceList, RaceListSize} = get_race_list_and_size(State),
        state__renew_race_list(
          [dialyzer_races:end_case_new(ClauseAcc)|RaceList],
	  RaceListSize + 1, State);
      false -> State
    end,
  {lists:reverse(Acc), State1, t_sup(CaseTypes), WarnAcc}.

do_clause(C, Arg, ArgType0, OrigArgType, Map, State, Warns) ->
  Pats = cerl:clause_pats(C),
  Guard = cerl:clause_guard(C),
  Body = cerl:clause_body(C),
  State1 =
    case is_race_analysis_enabled(State) of
      true ->
        state__renew_fun_args(Pats, State);
      false -> State
    end,
  Map0 = mark_as_fresh(Pats, Map),
  Map1 = if Arg =:= ?no_arg -> Map0;
	    true -> bind_subst(Arg, Pats, Map0)
	 end,
  BindRes =
    case t_is_none(ArgType0) of
      true ->
	{error, bind, Pats, ArgType0, ArgType0};
      false ->
	ArgTypes =
	  case t_is_any(ArgType0) of
	    true -> [ArgType0 || _ <- Pats];
	    false -> t_to_tlist(ArgType0)
	  end,
	bind_pat_vars(Pats, ArgTypes, [], Map1, State1)
    end,
  case BindRes of
    {error, ErrorType, NewPats, Type, OpaqueTerm} ->
      ?debug("Failed binding pattern: ~ts\nto ~ts\n",
	     [cerl_prettypr:format(C), format_type(ArgType0, State1)]),
      case state__warning_mode(State1) of
        false ->
	  {State1, Map, t_none(), ArgType0, Warns};
	true ->
	  {Msg, Force} =
	    case t_is_none(ArgType0) of
	      true ->
		%% See if this is covered by an earlier clause or if it
		%% simply cannot match
		OrigArgTypes =
		  case t_is_any(OrigArgType) of
		    true -> Any = t_any(), [Any || _ <- Pats];
		    false -> t_to_tlist(OrigArgType)
		  end,
                PatString = format_patterns(Pats),
                ArgTypeString = format_type(OrigArgType, State1),
                BindResOrig =
                  bind_pat_vars(Pats, OrigArgTypes, [], Map1, State1),
		Tag =
		  case BindResOrig of
		    {error,   bind, _, _, _} -> pattern_match;
		    {error, record, _, _, _} -> record_match;
		    {error, opaque, _, _, _} -> opaque_match;
		    {_, _} -> pattern_match_cov
		  end,
                PatTypes = case BindResOrig of
                             {error, opaque, _, _, OpaqueType} ->
                               [PatString, ArgTypeString,
                                format_type(OpaqueType, State1)];
                             _ -> [PatString, ArgTypeString]
                           end,
                {{Tag, PatTypes}, false};
	      false ->
		%% Try to find out if this is a default clause in a list
		%% comprehension and suppress this. A real Hack(tm)
		Force0 =
		  case is_compiler_generated(cerl:get_ann(C)) of
		    true ->
		      case Pats of
			[Pat] ->
			  case cerl:is_c_cons(Pat) of
			    true ->
			      not (cerl:is_c_var(cerl:cons_hd(Pat)) andalso
				   cerl:is_c_var(cerl:cons_tl(Pat)) andalso
				   cerl:is_literal(Guard) andalso
				   (cerl:concrete(Guard) =:= true));
			    false ->
			      true
			  end;
			[Pat0, Pat1] -> % binary comprehension
			  case cerl:is_c_cons(Pat0) of
			    true ->
			      not (cerl:is_c_var(cerl:cons_hd(Pat0)) andalso
				   cerl:is_c_var(cerl:cons_tl(Pat0)) andalso
                                   cerl:is_c_var(Pat1) andalso
				   cerl:is_literal(Guard) andalso
				   (cerl:concrete(Guard) =:= true));
			    false ->
			      true
			  end;
			_ -> true
		      end;
		    false ->
		      true
		  end,
                PatString =
                  case ErrorType of
                    bind   -> format_patterns(Pats);
                    record -> format_patterns(NewPats);
                    opaque -> format_patterns(NewPats)
                  end,
		PatTypes = case ErrorType of
			     bind -> [PatString, format_type(ArgType0, State1)];
			     record -> [PatString, format_type(Type, State1)];
			     opaque -> [PatString, format_type(Type, State1),
					format_type(OpaqueTerm, State1)]
			   end,
		FailedTag = case ErrorType of
			      bind  -> pattern_match;
			      record -> record_match;
			      opaque -> opaque_match
			    end,
		{{FailedTag, PatTypes}, Force0}
	    end,
	  WarnType = case Msg of
		       {opaque_match, _} -> ?WARN_OPAQUE;
		       {pattern_match, _} -> ?WARN_MATCHING;
		       {record_match, _} -> ?WARN_MATCHING;
		       {pattern_match_cov, _} -> ?WARN_MATCHING
		     end,
	  {State1, Map, t_none(), ArgType0, [{WarnType, C, Msg, Force}|Warns]}
      end;
    {Map2, PatTypes} ->
      Map3 =
	case Arg =:= ?no_arg of
	  true -> Map2;
	  false ->
	    %% Try to bind the argument. Will only succeed if
	    %% it is a simple structured term.
	    case bind_pat_vars_reverse([Arg], [t_product(PatTypes)],
				       [], Map2, State1) of
	      {error, _, _, _, _} -> Map2;
	      {NewMap, _} -> NewMap
	    end
	end,
      NewArgType =
	case Arg =:= ?no_arg of
	  true -> ArgType0;
	  false ->
	    GenType = dialyzer_typesig:get_safe_underapprox(Pats, Guard),
	    t_subtract(t_product(t_to_tlist(ArgType0)), GenType)
	end,
      case bind_guard(Guard, Map3, State1) of
	{error, Reason} ->
	  ?debug("Failed guard: ~ts\n",
		 [cerl_prettypr:format(C, [{hook, cerl_typean:pp_hook()}])]),
	  PatString = format_patterns(Pats),
	  DefaultMsg =
	    case Pats =:= [] of
	      true -> {guard_fail, []};
	      false ->
		{guard_fail_pat, [PatString, format_type(ArgType0, State1)]}
	    end,
	  Warn =
	    case Reason of
	      none -> {?WARN_MATCHING, C, DefaultMsg, false};
	      {FailGuard, Msg} ->
		case is_compiler_generated(cerl:get_ann(FailGuard)) of
		  false ->
		    WarnType = case Msg of
				 {guard_fail, _} -> ?WARN_MATCHING;
				 {neg_guard_fail, _} -> ?WARN_MATCHING;
				 {opaque_guard, _} -> ?WARN_OPAQUE
			       end,
		    {WarnType, FailGuard, Msg, false};
		  true ->
		    {?WARN_MATCHING, C, Msg, false}
		end
	    end,
	  {State1, Map, t_none(), NewArgType, [Warn|Warns]};
        Map4 ->
          {RetState, RetMap, BodyType} = traverse(Body, Map4, State1),
          {RetState, RetMap, BodyType, NewArgType, Warns}
      end
  end.

bind_subst(Arg, Pats, Map) ->
  case cerl:type(Arg) of
    values ->
      bind_subst_list(cerl:values_es(Arg), Pats, Map);
    var ->
      [Pat] = Pats,
      enter_subst(Arg, Pat, Map);
    _ ->
      Map
  end.

bind_subst_list([Arg|ArgLeft], [Pat|PatLeft], Map) ->
  NewMap =
    case {cerl:type(Arg), cerl:type(Pat)} of
      {var, var} ->         enter_subst(Arg, Pat, Map);
      {var, alias} ->       enter_subst(Arg, cerl:alias_pat(Pat), Map);
      {literal, literal} -> Map;
      {T, T} ->             bind_subst_list(lists:flatten(cerl:subtrees(Arg)),
					    lists:flatten(cerl:subtrees(Pat)),
					    Map);
      _ ->                  Map
    end,
  bind_subst_list(ArgLeft, PatLeft, NewMap);
bind_subst_list([], [], Map) ->
  Map.

%%----------------------------------------
%% Patterns
%%

bind_pat_vars(Pats, Types, Acc, Map, State) ->
  try
    bind_pat_vars(Pats, Types, Acc, Map, State, false)
  catch
    throw:Error -> 
      %% Error = {error, bind | opaque | record, ErrorPats, ErrorType}
      Error 
  end.

bind_pat_vars_reverse(Pats, Types, Acc, Map, State) ->
  try
    bind_pat_vars(Pats, Types, Acc, Map, State, true)
  catch
    throw:Error -> 
      %% Error = {error, bind | opaque | record, ErrorPats, ErrorType}
      Error
  end.

bind_pat_vars([Pat|PatLeft], [Type|TypeLeft], Acc, Map, State, Rev) ->
  ?debug("Binding pat: ~tw to ~ts\n", [cerl:type(Pat), format_type(Type, State)]
),
  Opaques = State#state.opaques,
  {NewMap, TypeOut} =
    case cerl:type(Pat) of
      alias ->
	%% Map patterns are more allowing than the type of their literal. We
	%% must unfold AliasPat if it is a literal.
	AliasPat = dialyzer_utils:refold_pattern(cerl:alias_pat(Pat)),
	Var = cerl:alias_var(Pat),
	Map1 = enter_subst(Var, AliasPat, Map),
	{Map2, [PatType]} = bind_pat_vars([AliasPat], [Type], [],
					  Map1, State, Rev),
	{enter_type(Var, PatType, Map2), PatType};
      binary ->
	%% Cannot bind the binary if we are in reverse match since
	%% binary patterns and binary construction are not symmetric.
	case Rev of
	  true -> {Map, t_bitstr()};
	  false ->
	    BinType = t_inf(t_bitstr(), Type, Opaques),
	    case t_is_none(BinType) of
	      true ->
                case t_find_opaque_mismatch(t_bitstr(), Type, Opaques) of
                  {ok, T1, T2}  ->
                    bind_error([Pat], T1, T2, opaque);
                  error ->
                    bind_error([Pat], Type, t_none(), bind)
                end;
	      false ->
		Segs = cerl:binary_segments(Pat),
		{Map1, SegTypes} = bind_bin_segs(Segs, BinType, Map, State),
		{Map1, t_bitstr_concat(SegTypes)}
	    end
	end;
      cons ->
	Cons = t_inf(Type, t_cons(), Opaques),
	case t_is_none(Cons) of
	  true ->
	    bind_opaque_pats(t_cons(), Type, Pat, State);
	  false ->
	    {Map1, [HdType, TlType]} =
	      bind_pat_vars([cerl:cons_hd(Pat), cerl:cons_tl(Pat)],
			    [t_cons_hd(Cons, Opaques),
                             t_cons_tl(Cons, Opaques)],
			    [], Map, State, Rev),
	    {Map1, t_cons(HdType, TlType)}
	end;
      literal ->
	Pat0 = dialyzer_utils:refold_pattern(Pat),
	case cerl:is_literal(Pat0) of
	  true ->
	    Literal = literal_type(Pat),
	    case t_is_none(t_inf(Literal, Type, Opaques)) of
	      true ->
		bind_opaque_pats(Literal, Type, Pat, State);
	      false -> {Map, Literal}
	    end;
	  false ->
	    %% Retry with the unfolded pattern
	    {Map1, [PatType]}
	      = bind_pat_vars([Pat0], [Type], [], Map, State, Rev),
	    {Map1, PatType}
	end;
      map ->
	MapT = t_inf(Type, t_map(), Opaques),
	case t_is_none(MapT) of
	  true ->
	    bind_opaque_pats(t_map(), Type, Pat, State);
	  false ->
	    case Rev of
	      %% TODO: Reverse matching (propagating a matched subset back to a value)
	      true -> {Map, MapT};
	      false ->
		FoldFun =
		  fun(Pair, {MapAcc, ListAcc}) ->
		      %% Only exact (:=) can appear in patterns
		      exact = cerl:concrete(cerl:map_pair_op(Pair)),
		      Key = cerl:map_pair_key(Pair),
		      KeyType =
			case cerl:type(Key) of
			  var ->
			    case state__lookup_type_for_letrec(Key, State) of
			      error -> lookup_type(Key, MapAcc);
			      {ok, RecType} -> RecType
			    end;
			  literal ->
			    literal_type(Key)
			end,
		      Bind = erl_types:t_map_get(KeyType, MapT),
		      {MapAcc1, [ValType]} =
			bind_pat_vars([cerl:map_pair_val(Pair)],
				      [Bind], [], MapAcc, State, Rev),
		      case t_is_singleton(KeyType, Opaques) of
			true  -> {MapAcc1, [{KeyType, ValType}|ListAcc]};
			false -> {MapAcc1, ListAcc}
		      end
		  end,
		{Map1, Pairs} = lists:foldl(FoldFun, {Map, []}, cerl:map_es(Pat)),
		{Map1, t_inf(MapT, t_map(Pairs))}
	    end
	end;
      tuple ->
	Es = cerl:tuple_es(Pat),
	{TypedRecord, Prototype} =
	  case Es of
	    [] -> {false, t_tuple([])};
	    [Tag|Left] ->
	      case cerl:is_c_atom(Tag) andalso is_literal_record(Pat) of
		true ->
		  TagAtom = cerl:atom_val(Tag),
		  case state__lookup_record(TagAtom, length(Left), State) of
		    error -> {false, t_tuple(length(Es))};
		    {ok, Record, _FieldNames} ->
		      [_Head|AnyTail] = [t_any() || _ <- Es],
		      UntypedRecord = t_tuple([t_atom(TagAtom)|AnyTail]),
		      {not t_is_equal(Record, UntypedRecord), Record}
		  end;
		false -> {false, t_tuple(length(Es))}
	      end
	  end,
	Tuple = t_inf(Prototype, Type, Opaques),
	case t_is_none(Tuple) of
	  true ->
	    bind_opaque_pats(Prototype, Type, Pat, State);
	  false ->
	    SubTuples = t_tuple_subtypes(Tuple, Opaques),
	    %% Need to call the top function to get the try-catch wrapper
            MapJ = join_maps_begin(Map),
	    Results =
	      case Rev of
		true ->
		  [bind_pat_vars_reverse(Es, t_tuple_args(SubTuple, Opaques),
                                         [], MapJ, State)
		   || SubTuple <- SubTuples];
		false ->
		  [bind_pat_vars(Es, t_tuple_args(SubTuple, Opaques), [],
                                 MapJ, State)
		   || SubTuple <- SubTuples]
	      end,
	    case lists:keyfind(opaque, 2, Results) of
	      {error, opaque, _PatList, _Type, Opaque} ->
		bind_error([Pat], Tuple, Opaque, opaque);
	      false ->
		case [M || {M, _} <- Results, M =/= error] of
		  [] ->
		    case TypedRecord of
		      true -> bind_error([Pat], Tuple, Prototype, record);
		      false -> bind_error([Pat], Tuple, t_none(), bind)
		    end;
		  Maps ->
		    Map1 = join_maps_end(Maps, MapJ),
		    TupleType = t_sup([t_tuple(EsTypes)
				       || {M, EsTypes} <- Results, M =/= error]),
		    {Map1, TupleType}
		end
	    end
	end;
      values ->
	Es = cerl:values_es(Pat),
	{Map1, EsTypes} =
	  bind_pat_vars(Es, t_to_tlist(Type), [], Map, State, Rev),
	{Map1, t_product(EsTypes)};
      var ->
	VarType1 =
	  case state__lookup_type_for_letrec(Pat, State) of
	    error -> lookup_type(Pat, Map);
	    {ok, RecType} -> RecType
	  end,
	%% Must do inf when binding args to pats. Vars in pats are fresh.
	VarType2 = t_inf(VarType1, Type, Opaques),
	case t_is_none(VarType2) of
	  true ->
	    case t_find_opaque_mismatch(VarType1, Type, Opaques) of
	      {ok, T1, T2}  ->
		bind_error([Pat], T1, T2, opaque);
	      error ->
		bind_error([Pat], Type, t_none(), bind)
	    end;
	  false ->
	    Map1 = enter_type(Pat, VarType2, Map),
	    {Map1, VarType2}
	end;
      _Other ->
	%% Catch all is needed when binding args to pats
	?debug("Failed match for ~p\n", [_Other]),
	bind_error([Pat], Type, t_none(), bind)
    end,
  bind_pat_vars(PatLeft, TypeLeft, [TypeOut|Acc], NewMap, State, Rev);
bind_pat_vars([], [], Acc, Map, _State, _Rev) ->
  {Map, lists:reverse(Acc)}.

bind_bin_segs(BinSegs, BinType, Map, State) ->
  bind_bin_segs(BinSegs, BinType, [], Map, State).

bind_bin_segs([Seg|Segs], BinType, Acc, Map, State) ->
  Val = cerl:bitstr_val(Seg),
  SegType = cerl:concrete(cerl:bitstr_type(Seg)),
  UnitVal = cerl:concrete(cerl:bitstr_unit(Seg)),
  case cerl:bitstr_bitsize(Seg) of
    all ->
      binary = SegType, [] = Segs, %% just an assert
      T = t_inf(t_bitstr(UnitVal, 0), BinType),
      {Map1, [Type]} = bind_pat_vars([Val], [T], [], Map, State, false),
      Type1 = remove_local_opaque_types(Type, State#state.opaques),
      bind_bin_segs(Segs, t_bitstr(0, 0), [Type1|Acc], Map1, State);
    utf -> % XXX: possibly can be strengthened
      true = lists:member(SegType, [utf8, utf16, utf32]),
      {Map1, [_]} = bind_pat_vars([Val], [t_integer()], [], Map, State, false),
      Type = t_binary(),
      bind_bin_segs(Segs, BinType, [Type|Acc], Map1, State);
    BitSz when is_integer(BitSz) orelse BitSz =:= any ->
      Size = cerl:bitstr_size(Seg),
      {Map1, [SizeType]} =
	bind_pat_vars([Size], [t_non_neg_integer()], [], Map, State, false),
      Opaques = State#state.opaques,
      NumberVals = t_number_vals(SizeType, Opaques),
      case t_contains_opaque(SizeType, Opaques) of
        true -> bind_error([Seg], SizeType, t_none(), opaque);
        false -> ok
      end,
      Type =
	case NumberVals of
	  [OneSize] -> t_bitstr(0, UnitVal * OneSize);
	  _ -> % 'unknown' too
	    MinSize = erl_types:number_min(SizeType, Opaques),
	    t_bitstr(UnitVal, UnitVal * MinSize)
	end,
      ValConstr =
	case SegType of
	  binary -> Type; %% The same constraints as for the whole bitstr
	  float -> t_float();
	  integer ->
	    case NumberVals of
	      unknown -> t_integer();
	      List ->
		SizeVal = lists:max(List),
		Flags = cerl:concrete(cerl:bitstr_flags(Seg)),
		N = SizeVal * UnitVal,
                case N >= ?BITS  of
                  true ->
                    case lists:member(signed, Flags) of
                      true -> t_from_range(neg_inf, pos_inf);
                      false -> t_from_range(0, pos_inf)
                    end;
                  false ->
                    case lists:member(signed, Flags) of
                      true -> t_from_range(-(1 bsl (N - 1)), 1 bsl (N - 1) - 1);
                      false -> t_from_range(0, 1 bsl N - 1)
                    end
                end
	    end
	end,
      {Map2, [_]} = bind_pat_vars([Val], [ValConstr], [], Map1, State, false),
      NewBinType = t_bitstr_match(Type, BinType),
      case t_is_none(NewBinType) of
	true -> bind_error([Seg], BinType, t_none(), bind);
	false -> bind_bin_segs(Segs, NewBinType, [Type|Acc], Map2, State)
      end
  end;
bind_bin_segs([], _BinType, Acc, Map, _State) ->
  {Map, lists:reverse(Acc)}.

bind_error(Pats, Type, OpaqueType, Error0) ->
  Error = case {Error0, Pats} of
            {bind, [Pat]} ->
              case is_literal_record(Pat) of
                true -> record;
                false -> Error0
              end;
            _ -> Error0
          end,
  throw({error, Error, Pats, Type, OpaqueType}).

-spec bind_opaque_pats(type(), type(), cerl:c_literal(), state()) ->
                          no_return().

bind_opaque_pats(GenType, Type, Pat, State) ->
  case t_find_opaque_mismatch(GenType, Type, State#state.opaques) of
    {ok, T1, T2}  ->
	bind_error([Pat], T1, T2, opaque);
    error ->
      bind_error([Pat], Type, t_none(), bind)
  end.

%%----------------------------------------
%% Guards
%%

bind_guard(Guard, Map, State) ->
  try bind_guard(Guard, Map, maps:new(), pos, State) of
    {Map1, _Type} -> Map1
  catch
    throw:{fail, Warning} -> {error, Warning};
    throw:{fatal_fail, Warning} -> {error, Warning}
  end.

bind_guard(Guard, Map, Env, Eval, State) ->
  ?debug("Handling ~tw guard: ~ts\n",
	 [Eval, cerl_prettypr:format(Guard, [{noann, true}])]),
  case cerl:type(Guard) of
    binary ->
      {Map, t_binary()};
    'case' ->
      Arg = cerl:case_arg(Guard),
      Clauses = cerl:case_clauses(Guard),
      bind_guard_case_clauses(Arg, Clauses, Map, Env, Eval, State);
    cons ->
      Hd = cerl:cons_hd(Guard),
      Tl = cerl:cons_tl(Guard),
      {Map1, HdType} = bind_guard(Hd, Map, Env, dont_know, State),
      {Map2, TlType} = bind_guard(Tl, Map1, Env, dont_know, State),
      {Map2, t_cons(HdType, TlType)};
    literal ->
      {Map, literal_type(Guard)};
    'try' ->
      Arg = cerl:try_arg(Guard),
      [Var] = cerl:try_vars(Guard),
      EVars = cerl:try_evars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      Map1 = join_maps_begin(Map),
      Map2 = mark_as_fresh(EVars, Map1),
      %% Visit handler first so we know if it should be ignored
      {{HandlerMap, HandlerType}, HandlerE} =
	try {bind_guard(cerl:try_handler(Guard), Map2, Env, Eval, State), none}
	catch throw:HE ->
	    {{Map2, t_none()}, HE}
	end,
      BodyEnv = maps:put(get_label(Var), Arg, Env),
      Wanted = case Eval of pos -> t_atom(true); neg -> t_atom(false);
		 dont_know -> t_any() end,
      case t_is_none(t_inf(HandlerType, Wanted)) of
	%% Handler won't save us; pretend it does not exist
	true -> bind_guard(cerl:try_body(Guard), Map, BodyEnv, Eval, State);
	false ->
	  {{BodyMap, BodyType}, BodyE} =
	    try {bind_guard(cerl:try_body(Guard), Map1, BodyEnv,
			    Eval, State), none}
	    catch throw:BE ->
		{{Map1, t_none()}, BE}
	    end,
	  Map3 = join_maps_end([BodyMap, HandlerMap], Map1),
	  case t_is_none(Sup = t_sup(BodyType, HandlerType)) of
	    true ->
	      %% Pick a reason. N.B. We assume that the handler is always
	      %% compiler-generated if the body is; that way, we won't need to
	      %% check.
	      Fatality = case {BodyE, HandlerE} of
			   {{fatal_fail, _}, _} -> fatal_fail;
			   {_, {fatal_fail, _}} -> fatal_fail;
			   _ -> fail
			 end,
	      throw({Fatality,
		     case {BodyE, HandlerE} of
		       {{_, Rsn}, _} when Rsn =/= none -> Rsn;
		       {_, {_,Rsn}} -> Rsn;
		       _ -> none
		     end});
	    false -> {Map3, Sup}
	  end
      end;
    tuple ->
      Es0 = cerl:tuple_es(Guard),
      {Map1, Es} = bind_guard_list(Es0, Map, Env, dont_know, State),
      {Map1, t_tuple(Es)};
    map ->
      case Eval of
	dont_know -> handle_guard_map(Guard, Map, Env, State);
	_PosOrNeg -> {Map, t_none()}  %% Map exprs do not produce bools
      end;
    'let' ->
      Arg = cerl:let_arg(Guard),
      [Var] = cerl:let_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = maps:put(get_label(Var), Arg, Env),
      bind_guard(cerl:let_body(Guard), Map, NewEnv, Eval, State);
    values ->
      Es = cerl:values_es(Guard),
      List = [bind_guard(V, Map, Env, dont_know, State) || V <- Es],
      Type = t_product([T || {_, T} <- List]),
      {Map, Type};
    var ->
      ?debug("Looking for var(~w)...", [cerl_trees:get_label(Guard)]),
      case maps:find(get_label(Guard), Env) of
	error ->
	  ?debug("Did not find it\n", []),
	  Type = lookup_type(Guard, Map),
	  Constr =
	    case Eval of
	      pos -> t_atom(true);
	      neg -> t_atom(false);
	      dont_know -> Type
	    end,
	  Inf = t_inf(Constr, Type),
	  {enter_type(Guard, Inf, Map), Inf};
	{ok, Tree} ->
	  ?debug("Found it\n", []),
	  {Map1, Type} = bind_guard(Tree, Map, Env, Eval, State),
	  {enter_type(Guard, Type, Map1), Type}
      end;
    call ->
      handle_guard_call(Guard, Map, Env, Eval, State)
  end.

handle_guard_call(Guard, Map, Env, Eval, State) ->
  MFA = {cerl:atom_val(cerl:call_module(Guard)),
	 cerl:atom_val(cerl:call_name(Guard)),
	 cerl:call_arity(Guard)},
  case MFA of
    {erlang, F, 1} when F =:= is_atom; F =:= is_boolean;
			F =:= is_binary; F =:= is_bitstring;
			F =:= is_float; F =:= is_function;
			F =:= is_integer; F =:= is_list; F =:= is_map;
			F =:= is_number; F =:= is_pid; F =:= is_port;
			F =:= is_reference; F =:= is_tuple ->
      handle_guard_type_test(Guard, F, Map, Env, Eval, State);
    {erlang, is_function, 2} ->
      handle_guard_is_function(Guard, Map, Env, Eval, State);
    MFA when (MFA =:= {erlang, internal_is_record, 3}) or
	     (MFA =:= {erlang, is_record, 3}) ->
      handle_guard_is_record(Guard, Map, Env, Eval, State);
    {erlang, '=:=', 2} ->
      handle_guard_eqeq(Guard, Map, Env, Eval, State);
    {erlang, '==', 2} ->
      handle_guard_eq(Guard, Map, Env, Eval, State);
    {erlang, 'and', 2} ->
      handle_guard_and(Guard, Map, Env, Eval, State);
    {erlang, 'or', 2} ->
      handle_guard_or(Guard, Map, Env, Eval, State);
    {erlang, 'not', 1} ->
      handle_guard_not(Guard, Map, Env, Eval, State);
    {erlang, Comp, 2} when Comp =:= '<'; Comp =:= '=<';
                           Comp =:= '>'; Comp =:= '>=' ->
      handle_guard_comp(Guard, Comp, Map, Env, Eval, State);
    _ ->
      handle_guard_gen_fun(MFA, Guard, Map, Env, Eval, State)
  end.

handle_guard_gen_fun({M, F, A}, Guard, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  {Map1, As} = bind_guard_list(Args, Map, Env, dont_know, State),
  Opaques = State#state.opaques,
  BifRet = erl_bif_types:type(M, F, A, As, Opaques),
  case t_is_none(BifRet) of
    true ->
      %% Is this an error-bif?
      case t_is_none(erl_bif_types:type(M, F, A)) of
	true -> signal_guard_fail(Eval, Guard, As, State);
	false -> signal_guard_fatal_fail(Eval, Guard, As, State)
      end;
    false ->
      BifArgs = bif_args(M, F, A),
      Map2 = enter_type_lists(Args, t_inf_lists(BifArgs, As, Opaques), Map1),
      Ret =
	case Eval of
	  pos -> t_inf(t_atom(true), BifRet);
	  neg -> t_inf(t_atom(false), BifRet);
	  dont_know -> BifRet
	end,
      case t_is_none(Ret) of
	true ->
	  case Eval =:= pos of
	    true -> signal_guard_fail(Eval, Guard, As, State);
	    false -> throw({fail, none})
	  end;
	false -> {Map2, Ret}
      end
  end.

handle_guard_type_test(Guard, F, Map, Env, Eval, State) ->
  [Arg] = cerl:call_args(Guard),
  {Map1, ArgType} = bind_guard(Arg, Map, Env, dont_know, State),
  case bind_type_test(Eval, F, ArgType, State) of
    error ->
      ?debug("Type test: ~w failed\n", [F]),
      signal_guard_fail(Eval, Guard, [ArgType], State);
    {ok, NewArgType, Ret} ->
      ?debug("Type test: ~w succeeded, NewType: ~ts, Ret: ~ts\n",
	     [F, t_to_string(NewArgType), t_to_string(Ret)]),
      {enter_type(Arg, NewArgType, Map1), Ret}
  end.

bind_type_test(Eval, TypeTest, ArgType, State) ->
  Type = case TypeTest of
	   is_atom -> t_atom();
	   is_boolean -> t_boolean();
	   is_binary -> t_binary();
	   is_bitstring -> t_bitstr();
	   is_float -> t_float();
	   is_function -> t_fun();
	   is_integer -> t_integer();
	   is_list -> t_maybe_improper_list();
	   is_map -> t_map();
	   is_number -> t_number();
	   is_pid -> t_pid();
	   is_port -> t_port();
	   is_reference -> t_reference();
	   is_tuple -> t_tuple()
	 end,
  case Eval of
    pos ->
      Inf = t_inf(Type, ArgType, State#state.opaques),
      case t_is_none(Inf) of
	true -> error;
	false -> {ok, Inf, t_atom(true)}
      end;
    neg ->
      Sub = t_subtract(ArgType, Type),
      case t_is_none(Sub) of
        true -> error;
        false -> {ok, Sub, t_atom(false)}
      end;
    dont_know ->
      {ok, ArgType, t_boolean()}
  end.

handle_guard_comp(Guard, Comp, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  [Arg1, Arg2] = Args,
  {Map1, ArgTypes} = bind_guard_list(Args, Map, Env, dont_know, State),
  Opaques = State#state.opaques,
  [Type1, Type2] = ArgTypes,
  IsInt1 = t_is_integer(Type1, Opaques),
  IsInt2 = t_is_integer(Type2, Opaques),
  case {type(Arg1), type(Arg2)} of
    {{literal, Lit1}, {literal, Lit2}} ->
      case erlang:Comp(cerl:concrete(Lit1), cerl:concrete(Lit2)) of
	true  when Eval =:= pos ->       {Map, t_atom(true)};
	true  when Eval =:= dont_know -> {Map, t_atom(true)};
	true  when Eval =:= neg ->       {Map, t_atom(true)};
	false when Eval =:= pos ->
	  signal_guard_fail(Eval, Guard, ArgTypes, State);
	false when Eval =:= dont_know -> {Map, t_atom(false)};
	false when Eval =:= neg ->       {Map, t_atom(false)}
      end;
    {{literal, Lit1}, var} when IsInt1 andalso IsInt2 andalso (Eval =:= pos) ->
      case bind_comp_literal_var(Lit1, Arg2, Type2, Comp, Map1, Opaques) of
	error -> signal_guard_fail(Eval, Guard, ArgTypes, State);
	{ok, NewMap} -> {NewMap, t_atom(true)}
      end;
    {var, {literal, Lit2}} when IsInt1 andalso IsInt2 andalso (Eval =:= pos) ->
      case bind_comp_literal_var(Lit2, Arg1, Type1, invert_comp(Comp),
                                 Map1, Opaques) of
	error -> signal_guard_fail(Eval, Guard, ArgTypes, State);
	{ok, NewMap} -> {NewMap, t_atom(true)}
      end;
    {_, _} ->
      handle_guard_gen_fun({erlang, Comp, 2}, Guard, Map, Env, Eval, State)
  end.

invert_comp('=<') -> '>=';
invert_comp('<')  -> '>';
invert_comp('>=') -> '=<';
invert_comp('>')  -> '<'.

bind_comp_literal_var(Lit, Var, VarType, CompOp, Map, Opaques) ->
  LitVal = cerl:concrete(Lit),
  NewVarType =
    case t_number_vals(VarType, Opaques) of
      unknown ->
	Range =
	  case CompOp of
	    '=<' -> t_from_range(LitVal, pos_inf);
	    '<'  -> t_from_range(LitVal + 1, pos_inf);
	    '>=' -> t_from_range(neg_inf, LitVal);
	    '>'  -> t_from_range(neg_inf, LitVal - 1)
	  end,
	t_inf(Range, VarType, Opaques);
      NumberVals ->
	NewNumberVals = [X || X <- NumberVals, erlang:CompOp(LitVal, X)],
	t_integers(NewNumberVals)
    end,
  case t_is_none(NewVarType) of
    true -> error;
    false -> {ok, enter_type(Var, NewVarType, Map)}
  end.

handle_guard_is_function(Guard, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  {Map1, ArgTypes0} = bind_guard_list(Args, Map, Env, dont_know, State),
  [FunType0, ArityType0] = ArgTypes0,
  Opaques = State#state.opaques,
  ArityType = t_inf(ArityType0, t_integer(), Opaques),
  case t_is_none(ArityType) of
    true -> signal_guard_fail(Eval, Guard, ArgTypes0, State);
    false ->
      FunTypeConstr =
	case t_number_vals(ArityType, State#state.opaques) of
	  unknown -> t_fun();
	  Vals ->
	    t_sup([t_fun(lists:duplicate(X, t_any()), t_any()) || X <- Vals])
	end,
      FunType = t_inf(FunType0, FunTypeConstr, Opaques),
      case t_is_none(FunType) of
	true ->
	  case Eval of
	    pos -> signal_guard_fail(Eval, Guard, ArgTypes0, State);
	    neg -> {Map1, t_atom(false)};
	    dont_know -> {Map1, t_atom(false)}
	  end;
	false ->
	  case Eval of
	    pos -> {enter_type_lists(Args, [FunType, ArityType], Map1),
		    t_atom(true)};
	    neg -> {Map1, t_atom(false)};
	    dont_know -> {Map1, t_boolean()}
	  end
      end
  end.

handle_guard_is_record(Guard, Map, Env, Eval, State) ->
  Args = cerl:call_args(Guard),
  [Rec, Tag0, Arity0] = Args,
  Tag = cerl:atom_val(Tag0),
  Arity = cerl:int_val(Arity0),
  {Map1, RecType} = bind_guard(Rec, Map, Env, dont_know, State),
  ArityMin1 = Arity - 1,
  Opaques = State#state.opaques,
  Tuple = t_tuple([t_atom(Tag)|lists:duplicate(ArityMin1, t_any())]),
  case t_is_none(t_inf(Tuple, RecType, Opaques)) of
    true ->
      case erl_types:t_has_opaque_subtype(RecType, Opaques) of
        true ->
          signal_guard_fail(Eval, Guard,
                            [RecType, t_from_term(Tag),
                             t_from_term(Arity)],
                            State);
        false ->
          case Eval of
            pos -> signal_guard_fail(Eval, Guard,
                                     [RecType, t_from_term(Tag),
                                      t_from_term(Arity)],
                                     State);
            neg -> {Map1, t_atom(false)};
            dont_know -> {Map1, t_atom(false)}
          end
      end;
    false ->
      TupleType =
        case state__lookup_record(Tag, ArityMin1, State) of
          error -> Tuple;
          {ok, Prototype, _FieldNames} -> Prototype
        end,
      Type = t_inf(TupleType, RecType, State#state.opaques),
      case t_is_none(Type) of
        true ->
          %% No special handling of opaque errors.
          FArgs = "record " ++ format_type(RecType, State),
          Msg = {record_matching, [FArgs, Tag]},
          throw({fail, {Guard, Msg}});
        false ->
          case Eval of
            pos -> {enter_type(Rec, Type, Map1), t_atom(true)};
            neg -> {Map1, t_atom(false)};
            dont_know -> {Map1, t_boolean()}
          end
      end
  end.

handle_guard_eq(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  case {type(Arg1), type(Arg2)} of
    {{literal, Lit1}, {literal, Lit2}} ->
      case cerl:concrete(Lit1) =:= cerl:concrete(Lit2) of
	true ->
	  if
	    Eval =:= pos -> {Map, t_atom(true)};
	    Eval =:= neg ->
	      ArgTypes = [t_from_term(cerl:concrete(Lit1)),
			  t_from_term(cerl:concrete(Lit2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State);
	    Eval =:= dont_know -> {Map, t_atom(true)}
	  end;
	false ->
	  if
	    Eval =:= neg -> {Map, t_atom(false)};
	    Eval =:= dont_know -> {Map, t_atom(false)};
	    Eval =:= pos ->
	      ArgTypes = [t_from_term(cerl:concrete(Lit1)),
			  t_from_term(cerl:concrete(Lit2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State)
	  end
      end;
    {{literal, Lit1}, _} when Eval =:= pos ->
      case cerl:concrete(Lit1) of
	Atom when is_atom(Atom) ->
	  bind_eqeq_guard_lit_other(Guard, Lit1, Arg2, Map, Env, State);
	[] ->
	  bind_eqeq_guard_lit_other(Guard, Lit1, Arg2, Map, Env, State);
	_ ->
	  bind_eq_guard(Guard, Lit1, Arg2, Map, Env, Eval, State)
      end;
    {_, {literal, Lit2}} when Eval =:= pos ->
      case cerl:concrete(Lit2) of
	Atom when is_atom(Atom) ->
	  bind_eqeq_guard_lit_other(Guard, Lit2, Arg1, Map, Env, State);
	[] ->
	  bind_eqeq_guard_lit_other(Guard, Lit2, Arg1, Map, Env, State);
	_ ->
	  bind_eq_guard(Guard, Arg1, Lit2, Map, Env, Eval, State)
      end;
    {_, _} ->
      bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
  end.

bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State) ->
  {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
  {Map2, Type2} = bind_guard(Arg2, Map1, Env, dont_know, State),
  Opaques = State#state.opaques,
  case
    t_is_nil(Type1, Opaques) orelse t_is_nil(Type2, Opaques)
    orelse t_is_atom(Type1, Opaques) orelse t_is_atom(Type2, Opaques)
  of
    true -> bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State);
    false ->
      %% XXX. Is this test OK?
      OpArgs = erl_types:t_find_unknown_opaque(Type1, Type2, Opaques),
      case OpArgs =:= [] of
        true ->
          case Eval of
            pos -> {Map2, t_atom(true)};
            neg -> {Map2, t_atom(false)};
            dont_know -> {Map2, t_boolean()}
          end;
        false ->
          signal_guard_fail(Eval, Guard, [Type1, Type2], State)
      end
  end.

handle_guard_eqeq(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  case {type(Arg1), type(Arg2)} of
    {{literal, Lit1}, {literal, Lit2}} ->

      case cerl:concrete(Lit1) =:= cerl:concrete(Lit2) of
	true ->
	  if Eval =:= neg ->
	      ArgTypes = [t_from_term(cerl:concrete(Lit1)),
			  t_from_term(cerl:concrete(Lit2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State);
	     Eval =:= pos -> {Map, t_atom(true)};
	     Eval =:= dont_know -> {Map, t_atom(true)}
	  end;
	false ->
	  if Eval =:= neg -> {Map, t_atom(false)};
	     Eval =:= dont_know -> {Map, t_atom(false)};
	     Eval =:= pos ->
	      ArgTypes = [t_from_term(cerl:concrete(Lit1)),
			  t_from_term(cerl:concrete(Lit2))],
	      signal_guard_fail(Eval, Guard, ArgTypes, State)
	  end
      end;
    {{literal, Lit1}, _} when Eval =:= pos ->
      bind_eqeq_guard_lit_other(Guard, Lit1, Arg2, Map, Env, State);
    {_, {literal, Lit2}} when Eval =:= pos ->
      bind_eqeq_guard_lit_other(Guard, Lit2, Arg1, Map, Env, State);
    {_, _} ->
      bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
  end.

bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State) ->
  {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
  {Map2, Type2} = bind_guard(Arg2, Map1, Env, dont_know, State),
  ?debug("Types are:~ts =:= ~ts\n", [t_to_string(Type1),
                                     t_to_string(Type2)]),
  Opaques = State#state.opaques,
  Inf = t_inf(Type1, Type2, Opaques),
  case t_is_none(Inf) of
    true ->
      OpArgs = erl_types:t_find_unknown_opaque(Type1, Type2, Opaques),
      case OpArgs =:= [] of
        true ->
          case Eval of
            neg -> {Map2, t_atom(false)};
            dont_know -> {Map2, t_atom(false)};
            pos -> signal_guard_fail(Eval, Guard, [Type1, Type2], State)
          end;
        false ->
          signal_guard_fail(Eval, Guard, [Type1, Type2], State)
      end;
    false ->
      case Eval of
        pos ->
          case {cerl:type(Arg1), cerl:type(Arg2)} of
            {var, var} ->
              Map3 = enter_subst(Arg1, Arg2, Map2),
              Map4 = enter_type(Arg2, Inf, Map3),
              {Map4, t_atom(true)};
            {var, _} ->
              Map3 = enter_type(Arg1, Inf, Map2),
              {Map3, t_atom(true)};
            {_, var} ->
              Map3 = enter_type(Arg2, Inf, Map2),
              {Map3, t_atom(true)};
            {_, _} ->
              {Map2, t_atom(true)}
          end;
        neg ->
          {Map2, t_atom(false)};
        dont_know ->
          {Map2, t_boolean()}
      end
  end.

bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State) ->
  Eval = dont_know,
  Opaques = State#state.opaques,
  case cerl:concrete(Arg1) of
    true ->
      {_, Type} = MT = bind_guard(Arg2, Map, Env, pos, State),
      case t_is_any_atom(true, Type, Opaques) of
	true -> MT;
	false ->
	  {_, Type0} = bind_guard(Arg2, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0, t_atom(true)], State)
      end;
    false ->
      {Map1, Type} = bind_guard(Arg2, Map, Env, neg, State),
      case t_is_any_atom(false, Type, Opaques) of
	true -> {Map1, t_atom(true)};
	false ->
	  {_, Type0} = bind_guard(Arg2, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0, t_atom(false)], State)
      end;
    Term ->
      LitType = t_from_term(Term),
      {Map1, Type} = bind_guard(Arg2, Map, Env, Eval, State),
      case t_is_subtype(LitType, Type) of
	false -> signal_guard_fail(Eval, Guard, [Type, LitType], State);
	true ->
	  case cerl:is_c_var(Arg2) of
	    true -> {enter_type(Arg2, LitType, Map1), t_atom(true)};
	    false -> {Map1, t_atom(true)}
	  end
      end
  end.

handle_guard_and(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  Opaques = State#state.opaques,
  case Eval of
    pos ->
      {Map1, Type1} = bind_guard(Arg1, Map, Env, Eval, State),
      case t_is_any_atom(true, Type1, Opaques) of
	false -> signal_guard_fail(Eval, Guard, [Type1, t_any()], State);
	true ->
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env, Eval, State),
	  case t_is_any_atom(true, Type2, Opaques) of
	    false -> signal_guard_fail(Eval, Guard, [Type1, Type2], State);
	    true -> {Map2, t_atom(true)}
	  end
      end;
    neg ->
      MapJ = join_maps_begin(Map),
      {Map1, Type1} =
	try bind_guard(Arg1, MapJ, Env, neg, State)
	catch throw:{fail, _} -> bind_guard(Arg2, MapJ, Env, pos, State)
	end,
      {Map2, Type2} =
	try bind_guard(Arg2, MapJ, Env, neg, State)
	catch throw:{fail, _} -> bind_guard(Arg1, MapJ, Env, pos, State)
	end,
      case
        t_is_any_atom(false, Type1, Opaques)
        orelse t_is_any_atom(false, Type2, Opaques)
      of
	true -> {join_maps_end([Map1, Map2], MapJ), t_atom(false)};
	false -> signal_guard_fail(Eval, Guard, [Type1, Type2], State)
      end;
    dont_know ->
      MapJ = join_maps_begin(Map),
      {Map1, Type1} = bind_guard(Arg1, MapJ, Env, dont_know, State),
      {Map2, Type2} = bind_guard(Arg2, MapJ, Env, dont_know, State),
      Bool1 = t_inf(Type1, t_boolean()),
      Bool2 = t_inf(Type2, t_boolean()),
      case t_is_none(Bool1) orelse t_is_none(Bool2) of
	true -> throw({fatal_fail, none});
	false ->
	  NewMap = join_maps_end([Map1, Map2], MapJ),
	  NewType =
	    case {t_atom_vals(Bool1, Opaques), t_atom_vals(Bool2, Opaques)} of
	      {['true'] , ['true'] } -> t_atom(true);
	      {['false'], _        } -> t_atom(false);
	      {_        , ['false']} -> t_atom(false);
              {unknown  , _        } ->
                signal_guard_fail(Eval, Guard, [Type1, Type2], State);
              {_        , unknown  } ->
                signal_guard_fail(Eval, Guard, [Type1, Type2], State);
	      {_        , _        } -> t_boolean()

	    end,
	  {NewMap, NewType}
      end
  end.

handle_guard_or(Guard, Map, Env, Eval, State) ->
  [Arg1, Arg2] = cerl:call_args(Guard),
  Opaques = State#state.opaques,
  case Eval of
    pos ->
      MapJ = join_maps_begin(Map),
      {Map1, Bool1} =
	try bind_guard(Arg1, MapJ, Env, pos, State)
	catch
	  throw:{fail,_} -> bind_guard(Arg1, MapJ, Env, dont_know, State)
	end,
      {Map2, Bool2} =
	try bind_guard(Arg2, MapJ, Env, pos, State)
	catch
	  throw:{fail,_} -> bind_guard(Arg2, MapJ, Env, dont_know, State)
	end,
      case
        ((t_is_any_atom(true, Bool1, Opaques)
          andalso t_is_boolean(Bool2, Opaques))
         orelse
           (t_is_any_atom(true, Bool2, Opaques)
            andalso t_is_boolean(Bool1, Opaques)))
      of
	true -> {join_maps_end([Map1, Map2], MapJ), t_atom(true)};
	false -> signal_guard_fail(Eval, Guard, [Bool1, Bool2], State)
      end;
    neg ->
      {Map1, Type1} = bind_guard(Arg1, Map, Env, neg, State),
      case t_is_any_atom(false, Type1, Opaques) of
	false -> signal_guard_fail(Eval, Guard, [Type1, t_any()], State);
	true ->
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env, neg, State),
	  case t_is_any_atom(false, Type2, Opaques) of
	    false -> signal_guard_fail(Eval, Guard, [Type1, Type2], State);
	    true -> {Map2, t_atom(false)}
	  end
      end;
    dont_know ->
      MapJ = join_maps_begin(Map),
      {Map1, Type1} = bind_guard(Arg1, MapJ, Env, dont_know, State),
      {Map2, Type2} = bind_guard(Arg2, MapJ, Env, dont_know, State),
      Bool1 = t_inf(Type1, t_boolean()),
      Bool2 = t_inf(Type2, t_boolean()),
      case t_is_none(Bool1) orelse t_is_none(Bool2) of
	true -> throw({fatal_fail, none});
	false ->
	  NewMap = join_maps_end([Map1, Map2], MapJ),
	  NewType =
	    case {t_atom_vals(Bool1, Opaques), t_atom_vals(Bool2, Opaques)} of
	      {['false'], ['false']} -> t_atom(false);
	      {['true'] , _        } -> t_atom(true);
	      {_        , ['true'] } -> t_atom(true);
              {unknown  , _        } ->
                signal_guard_fail(Eval, Guard, [Type1, Type2], State);
              {_        , unknown  } ->
                signal_guard_fail(Eval, Guard, [Type1, Type2], State);
	      {_        , _        } -> t_boolean()
	    end,
	  {NewMap, NewType}
      end
  end.

handle_guard_not(Guard, Map, Env, Eval, State) ->
  [Arg] = cerl:call_args(Guard),
  Opaques = State#state.opaques,
  case Eval of
    neg ->
      {Map1, Type} = bind_guard(Arg, Map, Env, pos, State),
      case t_is_any_atom(true, Type, Opaques) of
	true -> {Map1, t_atom(false)};
	false ->
	  {_, Type0} = bind_guard(Arg, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0], State)
      end;
    pos ->
      {Map1, Type} = bind_guard(Arg, Map, Env, neg, State),
      case t_is_any_atom(false, Type, Opaques) of
	true -> {Map1, t_atom(true)};
	false ->
	  {_, Type0} = bind_guard(Arg, Map, Env, Eval, State),
	  signal_guard_fail(Eval, Guard, [Type0], State)
      end;
    dont_know ->
      {Map1, Type} = bind_guard(Arg, Map, Env, dont_know, State),
      Bool = t_inf(Type, t_boolean()),
      case t_is_none(Bool) of
	true -> throw({fatal_fail, none});
	false ->
	  case t_atom_vals(Bool, Opaques) of
	    ['true'] -> {Map1, t_atom(false)};
	    ['false'] -> {Map1, t_atom(true)};
	    [_, _] -> {Map1, Bool};
            unknown -> signal_guard_fail(Eval, Guard, [Type], State)
	  end
      end
  end.

bind_guard_list(Guards, Map, Env, Eval, State) ->
  bind_guard_list(Guards, Map, Env, Eval, State, []).

bind_guard_list([G|Gs], Map, Env, Eval, State, Acc) ->
  {Map1, T} = bind_guard(G, Map, Env, Eval, State),
  bind_guard_list(Gs, Map1, Env, Eval, State, [T|Acc]);
bind_guard_list([], Map, _Env, _Eval, _State, Acc) ->
  {Map, lists:reverse(Acc)}.

handle_guard_map(Guard, Map, Env, State) ->
  Pairs = cerl:map_es(Guard),
  Arg = cerl:map_arg(Guard),
  {Map1, ArgType0} = bind_guard(Arg, Map, Env, dont_know, State),
  ArgType1 = t_inf(t_map(), ArgType0),
  case t_is_none_or_unit(ArgType1) of
    true -> {Map1, t_none()};
    false ->
      {Map2, TypePairs} = bind_guard_map_pairs(Pairs, Map1, Env, State, []),
      {Map2, lists:foldl(fun({KV,assoc},Acc) -> erl_types:t_map_put(KV,Acc);
			    ({KV,exact},Acc) -> erl_types:t_map_update(KV,Acc)
			 end, ArgType1, TypePairs)}
  end.

bind_guard_map_pairs([], Map, _Env, _State, PairAcc) ->
  {Map, lists:reverse(PairAcc)};
bind_guard_map_pairs([Pair|Pairs], Map, Env, State, PairAcc) ->
  Key = cerl:map_pair_key(Pair),
  Val = cerl:map_pair_val(Pair),
  Op = cerl:map_pair_op(Pair),
  {Map1, [K,V]} = bind_guard_list([Key,Val],Map,Env,dont_know,State),
  bind_guard_map_pairs(Pairs, Map1, Env, State,
		       [{{K,V},cerl:concrete(Op)}|PairAcc]).

-type eval() :: 'pos' | 'neg' | 'dont_know'.

-spec signal_guard_fail(eval(), cerl:c_call(), [type()],
			state()) -> no_return().

signal_guard_fail(Eval, Guard, ArgTypes, State) ->
  signal_guard_failure(Eval, Guard, ArgTypes, fail, State).

-spec signal_guard_fatal_fail(eval(), cerl:c_call(), [erl_types:erl_type()],
			      state()) -> no_return().

signal_guard_fatal_fail(Eval, Guard, ArgTypes, State) ->
  signal_guard_failure(Eval, Guard, ArgTypes, fatal_fail, State).

signal_guard_failure(Eval, Guard, ArgTypes, Tag, State) ->
  Args = cerl:call_args(Guard),
  F = cerl:atom_val(cerl:call_name(Guard)),
  {M, F, A} = MFA = {cerl:atom_val(cerl:call_module(Guard)), F, length(Args)},
  Opaques = State#state.opaques,
  {Kind, XInfo} =
    case erl_bif_types:opaque_args(M, F, A, ArgTypes, Opaques) of
      [] ->
        {case Eval of
           neg -> neg_guard_fail;
           pos -> guard_fail;
           dont_know -> guard_fail
         end,
         []};
      Ns -> {opaque_guard, [Ns]}
    end,
  FArgs =
    case is_infix_op(MFA) of
      true ->
	[ArgType1, ArgType2] = ArgTypes,
	[Arg1, Arg2] = Args,
	[format_args_1([Arg1], [ArgType1], State),
         atom_to_list(F),
         format_args_1([Arg2], [ArgType2], State)] ++ XInfo;
      false ->
        [F, format_args(Args, ArgTypes, State)]
    end,
  Msg = {Kind, FArgs},
  throw({Tag, {Guard, Msg}}).

is_infix_op({erlang, '=:=', 2}) -> true;
is_infix_op({erlang, '==', 2}) -> true;
is_infix_op({erlang, '=/=', 2}) -> true;
is_infix_op({erlang, '=/', 2}) -> true;
is_infix_op({erlang, '<', 2}) -> true;
is_infix_op({erlang, '=<', 2}) -> true;
is_infix_op({erlang, '>', 2}) -> true;
is_infix_op({erlang, '>=', 2}) -> true;
is_infix_op({M, F, A}) when is_atom(M), is_atom(F),
			    is_integer(A), 0 =< A, A =< 255 -> false.

bif_args(M, F, A) ->
  case erl_bif_types:arg_types(M, F, A) of
    unknown -> lists:duplicate(A, t_any());
    List -> List
  end.

bind_guard_case_clauses(Arg, Clauses, Map0, Env, Eval, State) ->
  Clauses1 = filter_fail_clauses(Clauses),
  Map = join_maps_begin(Map0),
  {GenMap, GenArgType} = bind_guard(Arg, Map, Env, dont_know, State),
  bind_guard_case_clauses(GenArgType, GenMap, Arg, Clauses1, Map, Env, Eval,
			  t_none(), [], [], State).

filter_fail_clauses([Clause|Left]) ->
  case (cerl:clause_pats(Clause) =:= []) of
    true ->
      Body = cerl:clause_body(Clause),
      case cerl:is_literal(Body) andalso (cerl:concrete(Body) =:= fail) orelse
	cerl:is_c_primop(Body) andalso
	(cerl:atom_val(cerl:primop_name(Body)) =:= match_fail) of
	true -> filter_fail_clauses(Left);
	false -> [Clause|filter_fail_clauses(Left)]
      end;
    false ->
      [Clause|filter_fail_clauses(Left)]
  end;
filter_fail_clauses([]) ->
  [].

bind_guard_case_clauses(GenArgType, GenMap, ArgExpr, [Clause|Left],
			Map, Env, Eval, AccType, AccMaps, Throws, State) ->
  Pats = cerl:clause_pats(Clause),
  {NewMap0, ArgType} =
    case Pats of
      [Pat] ->
	case cerl:is_literal(Pat) of
	  true ->
	    try
	      case cerl:concrete(Pat) of
		true -> bind_guard(ArgExpr, Map, Env, pos, State);
		false -> bind_guard(ArgExpr, Map, Env, neg, State);
		_ -> {GenMap, GenArgType}
	      end
	    catch
	      throw:{fail, _} -> {none, GenArgType}
	    end;
	  false ->
	    {GenMap, GenArgType}
	end;
      _ -> {GenMap, GenArgType}
    end,
  NewMap1 =
    case Pats =:= [] of
      true -> NewMap0;
      false ->
	case t_is_none(ArgType) of
	  true -> none;
	  false ->
	    ArgTypes = case t_is_any(ArgType) of
			 true -> Any = t_any(), [Any || _ <- Pats];
			 false -> t_to_tlist(ArgType)
		       end,
	    case bind_pat_vars(Pats, ArgTypes, [], NewMap0, State) of
	      {error, _, _, _, _} -> none;
	      {PatMap, _PatTypes} -> PatMap
	    end
	end
    end,
  Guard = cerl:clause_guard(Clause),
  GenPatType = dialyzer_typesig:get_safe_underapprox(Pats, Guard),
  NewGenArgType = t_subtract(GenArgType, GenPatType),
  case (NewMap1 =:= none) orelse t_is_none(GenArgType) of
    true ->
      bind_guard_case_clauses(NewGenArgType, GenMap, ArgExpr, Left, Map, Env,
			      Eval, AccType, AccMaps, Throws, State);
    false ->
      {NewAccType, NewAccMaps, NewThrows} =
	try
	  {NewMap2, GuardType} = bind_guard(Guard, NewMap1, Env, pos, State),
	  case t_is_none(t_inf(t_atom(true), GuardType)) of
	    true -> throw({fail, none});
	    false -> ok
	  end,
	  {NewMap3, CType} = bind_guard(cerl:clause_body(Clause), NewMap2,
					Env, Eval, State),
          Opaques = State#state.opaques,
	  case Eval of
	    pos ->
	      case t_is_any_atom(true, CType, Opaques) of
		true -> ok;
		false -> throw({fail, none})
	      end;
	    neg ->
	      case t_is_any_atom(false, CType, Opaques) of
		true -> ok;
		false -> throw({fail, none})
	      end;
	    dont_know ->
	      ok
	  end,
	  {t_sup(AccType, CType), [NewMap3|AccMaps], Throws}
	catch
	  throw:{fail, Reason} ->
            Throws1 = case Reason of
                        none -> Throws;
                        _ -> Throws ++ [Reason]
                      end,
            {AccType, AccMaps, Throws1}
	end,
      bind_guard_case_clauses(NewGenArgType, GenMap, ArgExpr, Left, Map, Env,
			      Eval, NewAccType, NewAccMaps, NewThrows, State)
  end;
bind_guard_case_clauses(_GenArgType, _GenMap, _ArgExpr, [], Map, _Env, _Eval,
			AccType, AccMaps, Throws, _State) ->
  case t_is_none(AccType) of
    true ->
      case Throws of
        [Throw|_] -> throw({fail, Throw});
        [] -> throw({fail, none})
      end;
    false -> {join_maps_end(AccMaps, Map), AccType}
  end.

%%% ===========================================================================
%%%
%%%  Maps and types.
%%%
%%% ===========================================================================

map__new() ->
  #map{}.

%% join_maps_begin pushes 'modified' to the stack; join_maps pops
%% 'modified' from the stack.

join_maps_begin(#map{modified = M, modified_stack = S, ref = Ref} = Map) ->
  Map#map{ref = make_ref(), modified = [], modified_stack = [{M,Ref} | S]}.

join_maps_end(Maps, MapOut) ->
  #map{ref = Ref, modified_stack = [{M1,R1} | S]} = MapOut,
  true = lists:all(fun(M) -> M#map.ref =:= Ref end, Maps), % sanity
  Keys0 = lists:usort(lists:append([M#map.modified || M <- Maps])),
  #map{map = Map, subst = Subst} = MapOut,
  Keys = [Key ||
           Key <- Keys0,
           maps:is_key(Key, Map) orelse maps:is_key(Key, Subst)],
  Out = case Maps of
          [] -> join_maps(Maps, MapOut);
          _ -> join_maps(Keys, Maps, MapOut)
        end,
  debug_join_check(Maps, MapOut, Out),
  Out#map{ref = R1,
          modified = Out#map.modified ++ M1, % duplicates possible
          modified_stack = S}.

join_maps(Maps, MapOut) ->
  #map{map = Map, subst = Subst} = MapOut,
  Keys = ordsets:from_list(maps:keys(Map) ++ maps:keys(Subst)),
  join_maps(Keys, Maps, MapOut).

join_maps(Keys, Maps, MapOut) ->
  KTs = join_maps_collect(Keys, Maps, MapOut),
  lists:foldl(fun({K, T}, M) -> enter_type(K, T, M) end, MapOut, KTs).

join_maps_collect([Key|Left], Maps, MapOut) ->
  Type = join_maps_one_key(Maps, Key, t_none()),
  case t_is_equal(lookup_type(Key, MapOut), Type) of
    true ->  join_maps_collect(Left, Maps, MapOut);
    false -> [{Key, Type} | join_maps_collect(Left, Maps, MapOut)]
  end;
join_maps_collect([], _Maps, _MapOut) ->
  [].

join_maps_one_key([Map|Left], Key, AccType) ->
  case t_is_any(AccType) of
    true ->
      %% We can stop here
      AccType;
    false ->
      join_maps_one_key(Left, Key, t_sup(lookup_type(Key, Map), AccType))
  end;
join_maps_one_key([], _Key, AccType) ->
  AccType.

-ifdef(DEBUG).
debug_join_check(Maps, MapOut, Out) ->
  #map{map = Map, subst = Subst} = Out,
  #map{map = Map2, subst = Subst2} = join_maps(Maps, MapOut),
  F = fun(D) -> lists:keysort(1, maps:to_list(D)) end,
  [throw({bug, join_maps}) ||
    F(Map) =/= F(Map2) orelse F(Subst) =/= F(Subst2)].
-else.
debug_join_check(_Maps, _MapOut, _Out) -> ok.
-endif.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_list([{Key, Val}|Left], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_list(Left, Map1);
enter_type_list([], Map) ->
  Map.

enter_type(Key, Val, MS) ->
  case cerl:is_literal(Key) of
    true -> MS;
    false ->
      case cerl:is_c_values(Key) of
	true ->
          Keys = cerl:values_es(Key),
	  case t_is_any(Val) orelse t_is_none(Val) of
	    true ->
	      enter_type_lists(Keys, [Val || _ <- Keys], MS);
	    false ->
	      enter_type_lists(Keys, t_to_tlist(Val), MS)
	  end;
	false ->
          #map{map = Map, subst = Subst} = MS,
	  KeyLabel = get_label(Key),
	  case maps:find(KeyLabel, Subst) of
	    {ok, NewKey} ->
	      ?debug("Binding ~p to ~p\n", [KeyLabel, NewKey]),
	      enter_type(NewKey, Val, MS);
	    error ->
	      ?debug("Entering ~p :: ~ts\n", [KeyLabel, t_to_string(Val)]),
	      case maps:find(KeyLabel, Map) of
		{ok, Value} ->
                  case erl_types:t_is_equal(Val, Value) of
                    true -> MS;
                    false -> store_map(KeyLabel, Val, MS)
                  end;
		error -> store_map(KeyLabel, Val, MS)
	      end
	  end
      end
  end.

store_map(Key, Val, #map{map = Map, ref = undefined} = MapRec) ->
  MapRec#map{map = maps:put(Key, Val, Map)};
store_map(Key, Val, #map{map = Map, modified = Mod} = MapRec) ->
  MapRec#map{map = maps:put(Key, Val, Map), modified = [Key | Mod]}.

enter_subst(Key, Val0, #map{subst = Subst} = MS) ->
  KeyLabel = get_label(Key),
  Val = dialyzer_utils:refold_pattern(Val0),
  case cerl:is_literal(Val) of
    true ->
      store_map(KeyLabel, literal_type(Val), MS);
    false ->
      case cerl:is_c_var(Val) of
	false -> MS;
	true ->
	  ValLabel = get_label(Val),
	  case maps:find(ValLabel, Subst) of
	    {ok, NewVal} ->
	      enter_subst(Key, NewVal, MS);
	    error ->
	      if KeyLabel =:= ValLabel -> MS;
		 true ->
		  ?debug("Subst: storing ~p = ~p\n", [KeyLabel, ValLabel]),
                  store_subst(KeyLabel, ValLabel, MS)
	      end
	  end
      end
  end.

store_subst(Key, Val, #map{subst = S, ref = undefined} = Map) ->
  Map#map{subst = maps:put(Key, Val, S)};
store_subst(Key, Val, #map{subst = S, modified = Mod} = Map) ->
  Map#map{subst = maps:put(Key, Val, S), modified = [Key | Mod]}.

lookup_type(Key, #map{map = Map, subst = Subst}) ->
  lookup(Key, Map, Subst, t_none()).

lookup(Key, Map, Subst, AnyNone) ->
  case cerl:is_literal(Key) of
    true -> literal_type(Key);
    false ->
      Label = get_label(Key),
      case maps:find(Label, Subst) of
	{ok, NewKey} -> lookup(NewKey, Map, Subst, AnyNone);
	error ->
	  case maps:find(Label, Map) of
	    {ok, Val} -> Val;
	    error -> AnyNone
	  end
      end
  end.

lookup_fun_sig(Fun, Callgraph, Plt) ->
  MFAorLabel =
    case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
      error -> Fun;
      {ok, MFA} -> MFA
    end,
  dialyzer_plt:lookup(Plt, MFAorLabel).

literal_type(Lit) ->
  t_from_term(cerl:concrete(Lit)).

mark_as_fresh([Tree|Left], Map) ->
  SubTrees1 = lists:append(cerl:subtrees(Tree)),
  {SubTrees2, Map1} =
    case cerl:type(Tree) of
      bitstr ->
	%% The Size field is not fresh.
	{SubTrees1 -- [cerl:bitstr_size(Tree)], Map};
      map_pair ->
	%% The keys are not fresh
	{SubTrees1 -- [cerl:map_pair_key(Tree)], Map};
      var ->
	{SubTrees1, enter_type(Tree, t_any(), Map)};
      _ ->
	{SubTrees1, Map}
    end,
  mark_as_fresh(SubTrees2 ++ Left, Map1);
mark_as_fresh([], Map) ->
  Map.

-ifdef(DEBUG).
debug_pp_map(#map{map = Map}=MapRec) ->
  Keys = maps:keys(Map),
  io:format("Map:\n", []),
  lists:foreach(fun (Key) ->
		    io:format("\t~w :: ~ts\n",
			      [Key, t_to_string(lookup_type(Key, MapRec))])
		end, Keys),
  ok.
-else.
debug_pp_map(_Map) -> ok.
-endif.

%%% ===========================================================================
%%%
%%%  Utilities
%%%
%%% ===========================================================================

get_label(L) when is_integer(L) ->
  L;
get_label(T) ->
  cerl_trees:get_label(T).

t_is_simple(ArgType, State) ->
  Opaques = State#state.opaques,
  t_is_atom(ArgType, Opaques) orelse t_is_number(ArgType, Opaques)
    orelse t_is_port(ArgType, Opaques)
    orelse t_is_pid(ArgType, Opaques) orelse t_is_reference(ArgType, Opaques)
    orelse t_is_nil(ArgType, Opaques).

remove_local_opaque_types(Type, Opaques) ->
  t_unopaque(Type, Opaques).

%% t_is_structured(ArgType) ->
%%   case t_is_nil(ArgType) of
%%     true -> false;
%%     false ->
%%       SType = t_inf(t_sup([t_list(), t_tuple(), t_binary()]), ArgType),
%%       t_is_equal(ArgType, SType)
%%   end.

is_call_to_send(Tree) ->
  case cerl:is_c_call(Tree) of
    false -> false;
    true ->
      Mod = cerl:call_module(Tree),
      Name = cerl:call_name(Tree),
      Arity = cerl:call_arity(Tree),
      cerl:is_c_atom(Mod)
	andalso cerl:is_c_atom(Name)
        andalso is_send(cerl:atom_val(Name))
	andalso (cerl:atom_val(Mod) =:= erlang)
	andalso (Arity =:= 2)
  end.

is_send('!') -> true;
is_send(send) -> true;
is_send(_) -> false.

is_lc_simple_list(Tree, TreeType, State) ->
  Opaques = State#state.opaques,
  Ann = cerl:get_ann(Tree),
  lists:member(list_comprehension, Ann)
    andalso t_is_list(TreeType)
    andalso t_is_simple(t_list_elements(TreeType, Opaques), State).

%%% ===========================================================================
%%%
%%%  The State.
%%%
%%% ===========================================================================

state__new(Callgraph, Codeserver, Tree, Plt, Module, Records) ->
  Opaques = erl_types:t_opaque_from_records(Records),
  {TreeMap, FunHomes} = build_tree_map(Tree, Callgraph),
  Funs = dict:fetch_keys(TreeMap),
  FunTab = init_fun_tab(Funs, dict:new(), TreeMap, Callgraph, Plt),
  ExportedFunctions =
    [Fun ||
      Fun <- Funs--[top],
      dialyzer_callgraph:is_escaping(Fun, Callgraph),
      dialyzer_callgraph:lookup_name(Fun, Callgraph) =/= error
    ],
  Work = init_work(ExportedFunctions),
  Env = lists:foldl(fun(Fun, Env) -> dict:store(Fun, map__new(), Env) end,
		    dict:new(), Funs),
  #state{callgraph = Callgraph, codeserver = Codeserver,
         envs = Env, fun_tab = FunTab, fun_homes = FunHomes, opaques = Opaques,
	 plt = Plt, races = dialyzer_races:new(), records = Records,
	 warning_mode = false, warnings = [], work = Work, tree_map = TreeMap,
	 module = Module, reachable_funs = sets:new()}.

state__warning_mode(#state{warning_mode = WM}) ->
  WM.

state__set_warning_mode(#state{tree_map = TreeMap, fun_tab = FunTab,
                               races = Races, callgraph = Callgraph,
                               reachable_funs = ReachableFuns} = State) ->
  ?debug("==========\nStarting warning pass\n==========\n", []),
  Funs = dict:fetch_keys(TreeMap),
  Work =
    [Fun ||
      Fun <- Funs--[top],
      dialyzer_callgraph:lookup_name(Fun, Callgraph) =/= error orelse
        sets:is_element(Fun, ReachableFuns)],
  State#state{work = init_work(Work),
	      fun_tab = FunTab, warning_mode = true,
              races = dialyzer_races:put_race_analysis(true, Races)}.

state__race_analysis(Analysis, #state{races = Races} = State) ->
  State#state{races = dialyzer_races:put_race_analysis(Analysis, Races)}.

state__renew_curr_fun(CurrFun, CurrFunLabel,
                      #state{races = Races} = State) ->
  State#state{races = dialyzer_races:put_curr_fun(CurrFun, CurrFunLabel,
                                                  Races)}.

state__renew_fun_args(Args, #state{races = Races} = State) ->
  case state__warning_mode(State) of
    true -> State;
    false ->
      State#state{races = dialyzer_races:put_fun_args(Args, Races)}
  end.

state__renew_race_list(RaceList, RaceListSize,
                       #state{races = Races} = State) ->
  State#state{races = dialyzer_races:put_race_list(RaceList, RaceListSize,
                                                   Races)}.

state__renew_warnings(Warnings, State) ->
  State#state{warnings = Warnings}.

-spec state__add_warning(raw_warning(), state()) -> state().

state__add_warning(Warn, #state{warnings = Warnings} = State) ->
  State#state{warnings = [Warn|Warnings]}.

state__add_warning(State, Tag, Tree, Msg) ->
  state__add_warning(State, Tag, Tree, Msg, false).

state__add_warning(#state{warning_mode = false} = State, _, _, _, _) ->
  State;
state__add_warning(#state{warnings = Warnings, warning_mode = true} = State,
		   Tag, Tree, Msg, Force) ->
  Ann = cerl:get_ann(Tree),
  case Force of
    true ->
      WarningInfo = {get_file(Ann, State),
                     abs(get_line(Ann)),
                     State#state.curr_fun},
      Warn = {Tag, WarningInfo, Msg},
      ?debug("MSG ~ts\n", [dialyzer:format_warning(Warn)]),
      State#state{warnings = [Warn|Warnings]};
    false ->
      case is_compiler_generated(Ann) of
        true -> State;
        false ->
          WarningInfo = {get_file(Ann, State),
                         get_line(Ann),
                         State#state.curr_fun},
          Warn = {Tag, WarningInfo, Msg},
          case Tag of
            ?WARN_CONTRACT_RANGE -> ok;
            _ -> ?debug("MSG ~ts\n", [dialyzer:format_warning(Warn)])
          end,
          State#state{warnings = [Warn|Warnings]}
      end
  end.

state__remove_added_warnings(OldState, NewState) ->
  #state{warnings = OldWarnings} = OldState,
  #state{warnings = NewWarnings} = NewState,
  case NewWarnings =:= OldWarnings of
    true -> {[], NewState};
    false -> {NewWarnings -- OldWarnings, NewState#state{warnings = OldWarnings}}
  end.

state__add_warnings(Warns, #state{warnings = Warnings} = State) ->
  State#state{warnings = Warns ++ Warnings}.

-spec state__set_curr_fun(curr_fun(), state()) -> state().

state__set_curr_fun(undefined, State) ->
  State#state{curr_fun = undefined};
state__set_curr_fun(FunLbl, State) ->
  State#state{curr_fun = find_function(FunLbl, State)}.

-spec state__find_function(mfa_or_funlbl(), state()) -> mfa_or_funlbl().

state__find_function(FunLbl, State) ->
  find_function(FunLbl, State).

state__get_race_warnings(#state{races = Races} = State) ->
  {Races1, State1} = dialyzer_races:get_race_warnings(Races, State),
  State1#state{races = Races1}.

state__get_warnings(#state{tree_map = TreeMap, fun_tab = FunTab,
			   callgraph = Callgraph, plt = Plt,
                           reachable_funs = ReachableFuns} = State) ->
  FoldFun =
    fun({top, _}, AccState) -> AccState;
       ({FunLbl, Fun}, AccState) ->
        AccState1 = state__set_curr_fun(FunLbl, AccState),
	{NotCalled, Ret} =
	  case dict:fetch(get_label(Fun), FunTab) of
	    {not_handled, {_Args0, Ret0}} -> {true, Ret0};
	    {_Args0, Ret0} -> {false, Ret0}
	  end,
	case NotCalled of
	  true ->
            case dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
              error -> AccState1;
              {ok, {_M, F, A}} ->
                Msg = {unused_fun, [F, A]},
                state__add_warning(AccState1, ?WARN_NOT_CALLED, Fun, Msg)
            end;
	  false ->
	    {Name, Contract} =
	      case dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
		error -> {[], none};
		{ok, {_M, F, A} = MFA} ->
		  {[F, A], dialyzer_plt:lookup_contract(Plt, MFA)}
	      end,
	    case t_is_none(Ret) of
	      true ->
		%% Check if the function has a contract that allows this.
		Warn =
		  case Contract of
		    none -> not parent_allows_this(FunLbl, AccState1);
		    {value, C} ->
		      GenRet = dialyzer_contracts:get_contract_return(C),
		      not t_is_unit(GenRet)
		  end,
                %% Do not output warnings for unreachable funs.
		case
                  Warn andalso
                  (dialyzer_callgraph:lookup_name(FunLbl, Callgraph) =/= error
                   orelse sets:is_element(FunLbl, ReachableFuns))
                of
		  true ->
		    case classify_returns(Fun) of
		      no_match ->
			Msg = {no_return, [no_match|Name]},
			state__add_warning(AccState1, ?WARN_RETURN_NO_RETURN,
					   Fun, Msg);
		      only_explicit ->
			Msg = {no_return, [only_explicit|Name]},
			state__add_warning(AccState1, ?WARN_RETURN_ONLY_EXIT,
					   Fun, Msg);
		      only_normal ->
			Msg = {no_return, [only_normal|Name]},
			state__add_warning(AccState1, ?WARN_RETURN_NO_RETURN,
					   Fun, Msg);
		      both ->
			Msg = {no_return, [both|Name]},
			state__add_warning(AccState1, ?WARN_RETURN_NO_RETURN,
					   Fun, Msg)
		    end;
		  false ->
		    AccState
		end;
	      false ->
		AccState
	    end
	end
    end,
  #state{warnings = Warn} = lists:foldl(FoldFun, State, dict:to_list(TreeMap)),
  Warn.

state__is_escaping(Fun, #state{callgraph = Callgraph}) ->
  dialyzer_callgraph:is_escaping(Fun, Callgraph).

state__lookup_type_for_letrec(Var, #state{callgraph = Callgraph} = State) ->
  Label = get_label(Var),
  case dialyzer_callgraph:lookup_letrec(Label, Callgraph) of
    error -> error;
    {ok, FunLabel} ->
      {ok, state__fun_type(FunLabel, State)}
  end.

state__lookup_name({_, _, _} = MFA, #state{}) ->
  MFA;
state__lookup_name(top, #state{}) ->
  top;
state__lookup_name(Fun, #state{callgraph = Callgraph}) ->
  case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
    {ok, MFA} -> MFA;
    error -> Fun
  end.

state__lookup_record(Tag, Arity, #state{records = Records}) ->
  case erl_types:lookup_record(Tag, Arity, Records) of
    {ok, Fields} ->
      RecType =
        t_tuple([t_atom(Tag)|
                 [FieldType || {_FieldName, _Abstr, FieldType} <- Fields]]),
      FieldNames = [FieldName || {FieldName, _Abstr, _FieldType} <- Fields],
      {ok, RecType, FieldNames};
    error ->
      error
  end.

state__get_args_and_status(Tree, #state{fun_tab = FunTab}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    {ok, {not_handled, {ArgTypes, _}}} -> {ArgTypes, false};
    {ok, {ArgTypes, _}} -> {ArgTypes, true}
  end.

state__add_reachable(FunLbl, #state{reachable_funs = ReachableFuns}=State) ->
  NewReachableFuns = sets:add_element(FunLbl, ReachableFuns),
  State#state{reachable_funs = NewReachableFuns}.

build_tree_map(Tree, Callgraph) ->
  Fun =
    fun(T, {Dict, Homes, FunLbls} = Acc) ->
	case cerl:is_c_fun(T) of
	  true ->
            FunLbl = get_label(T),
	    Dict1 = dict:store(FunLbl, T, Dict),
            case catch dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
              {ok, MFA} ->
                F2 =
                  fun(Lbl, Dict0) ->
                      dict:store(Lbl, MFA, Dict0)
                  end,
                Homes1 = lists:foldl(F2, Homes, [FunLbl|FunLbls]),
                {Dict1, Homes1, []};
              _ ->
                {Dict1, Homes, [FunLbl|FunLbls]}
            end;
	  false ->
            Acc
	end
    end,
  Dict0 = dict:new(),
  {Dict, Homes, _} = cerl_trees:fold(Fun, {Dict0, Dict0, []}, Tree),
  {Dict, Homes}.

init_fun_tab([top|Left], Dict, TreeMap, Callgraph, Plt) ->
  NewDict = dict:store(top, {[], t_none()}, Dict),
  init_fun_tab(Left, NewDict, TreeMap, Callgraph, Plt);
init_fun_tab([Fun|Left], Dict, TreeMap, Callgraph, Plt) ->
  Arity = cerl:fun_arity(dict:fetch(Fun, TreeMap)),
  FunEntry =
    case dialyzer_callgraph:is_escaping(Fun, Callgraph) of
      true ->
        Args = lists:duplicate(Arity, t_any()),
	case lookup_fun_sig(Fun, Callgraph, Plt) of
	  none -> {Args, t_unit()};
	  {value, {RetType, _}} ->
	    case t_is_none(RetType) of
	      true -> {Args, t_none()};
	      false -> {Args, t_unit()}
	    end
	end;
      false -> {not_handled, {lists:duplicate(Arity, t_none()), t_unit()}}
    end,
  NewDict = dict:store(Fun, FunEntry, Dict),
  init_fun_tab(Left, NewDict, TreeMap, Callgraph, Plt);
init_fun_tab([], Dict, _TreeMap, _Callgraph, _Plt) ->
  ?debug("DICT:~p\n",[dict:to_list(Dict)]),
  Dict.

state__update_fun_env(Tree, Map, #state{envs = Envs} = State) ->
  NewEnvs = dict:store(get_label(Tree), Map, Envs),
  State#state{envs = NewEnvs}.

state__fun_env(Tree, #state{envs = Envs}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, Envs) of
    error -> none;
    {ok, Map} -> Map
  end.

state__clean_not_called(#state{fun_tab = FunTab} = State) ->
  NewFunTab =
    dict:map(fun(top, Entry) -> Entry;
		(_Fun, {not_handled, {Args, _}}) -> {Args, t_none()};
		(_Fun, Entry) -> Entry
	     end, FunTab),
  State#state{fun_tab = NewFunTab}.

state__all_fun_types(State) ->
  #state{fun_tab = FunTab} = state__clean_not_called(State),
  Tab1 = dict:erase(top, FunTab),
  List = [{Fun, t_fun(Args, Ret)} ||
           {Fun, {Args, Ret}} <- dict:to_list(Tab1)],
  orddict:from_list(List).

state__fun_type(Fun, #state{fun_tab = FunTab}) ->
  Label =
    if is_integer(Fun) -> Fun;
       true -> get_label(Fun)
    end,
  Entry = dict:find(Label, FunTab),
  ?debug("FunType ~p:~p\n",[Label, Entry]),
  case Entry of
    {ok, {not_handled, {A, R}}} ->
      t_fun(A, R);
    {ok, {A, R}} ->
      t_fun(A, R)
  end.

state__update_fun_entry(Tree, ArgTypes, Out0,
			#state{fun_tab=FunTab, callgraph=CG, plt=Plt} = State)->
  Fun = get_label(Tree),
  Out1 =
    if Fun =:= top -> Out0;
       true ->
	case lookup_fun_sig(Fun, CG, Plt) of
	  {value, {SigRet, _}} -> t_inf(SigRet, Out0);
	  none -> Out0
	end
    end,
  Out = t_limit(Out1, ?TYPE_LIMIT),
  {ok, {OldArgTypes, OldOut}} = dict:find(Fun, FunTab),
  SameArgs = lists:all(fun({A, B}) -> erl_types:t_is_equal(A, B)
                       end, lists:zip(OldArgTypes, ArgTypes)),
  SameOut = t_is_equal(OldOut, Out),
  if
    SameArgs, SameOut ->
      ?debug("Fixpoint for ~tw: ~ts\n",
	     [state__lookup_name(Fun, State),
	      t_to_string(t_fun(ArgTypes, Out))]),
      State;
    true ->
      %% Can only happen in self-recursive functions.
      NewEntry = {OldArgTypes, Out},
      ?debug("New Entry for ~tw: ~ts\n",
	     [state__lookup_name(Fun, State),
	      t_to_string(t_fun(OldArgTypes, Out))]),
      NewFunTab = dict:store(Fun, NewEntry, FunTab),
      State1 = State#state{fun_tab = NewFunTab},
      state__add_work_from_fun(Tree, State1)
  end.

state__add_work_from_fun(_Tree, #state{warning_mode = true} = State) ->
  State;
state__add_work_from_fun(Tree, #state{callgraph = Callgraph,
				      tree_map = TreeMap} = State) ->
  case get_label(Tree) of
    top -> State;
    Label when is_integer(Label) ->
      case dialyzer_callgraph:in_neighbours(Label, Callgraph) of
	none -> State;
	MFAList ->
	  LabelList = [dialyzer_callgraph:lookup_label(MFA, Callgraph)
		       || MFA <- MFAList],
	  %% Must filter the result for results in this module.
	  FilteredList = [L || {ok, L} <- LabelList, dict:is_key(L, TreeMap)],
	  ?debug("~tw: Will try to add:~tw\n",
		 [state__lookup_name(Label, State), MFAList]),
	  lists:foldl(fun(L, AccState) ->
			  state__add_work(L, AccState)
		      end, State, FilteredList)
      end
  end.

state__add_work(external, State) ->
  State;
state__add_work(top, State) ->
  State;
state__add_work(Fun, #state{work = Work} = State) ->
  NewWork = add_work(Fun, Work),
  State#state{work = NewWork}.

state__get_work(#state{work = Work, tree_map = TreeMap} = State) ->
  case get_work(Work) of
    none -> none;
    {Fun, NewWork} ->
      {dict:fetch(Fun, TreeMap), State#state{work = NewWork}}
  end.

state__lookup_call_site(Tree, #state{callgraph = Callgraph}) ->
  Label = get_label(Tree),
  dialyzer_callgraph:lookup_call_site(Label, Callgraph).

state__fun_info(external, #state{}) ->
  external;
state__fun_info({_, _, _} = MFA, #state{plt = PLT}) ->
  {MFA,
   dialyzer_plt:lookup(PLT, MFA),
   dialyzer_plt:lookup_contract(PLT, MFA),
   t_any()};
state__fun_info(Fun, #state{callgraph = CG, fun_tab = FunTab, plt = PLT}) ->
  {Sig, Contract} =
    case dialyzer_callgraph:lookup_name(Fun, CG) of
      error ->
	{dialyzer_plt:lookup(PLT, Fun), none};
      {ok, MFA} ->
	{dialyzer_plt:lookup(PLT, MFA), dialyzer_plt:lookup_contract(PLT, MFA)}
    end,
  LocalRet =
    case dict:fetch(Fun, FunTab) of
      {not_handled, {_Args, Ret}} -> Ret;
      {_Args, Ret} -> Ret
    end,
  ?debug("LocalRet: ~ts\n", [t_to_string(LocalRet)]),
  {Fun, Sig, Contract, LocalRet}.

forward_args(Fun, ArgTypes, #state{work = Work, fun_tab = FunTab} = State) ->
  {NewArgTypes, OldOut, Fixpoint} =
    case dict:find(Fun, FunTab) of
      {ok, {not_handled, {_OldArgTypesAreNone, OldOut0}}} ->
	{ArgTypes, OldOut0, false};
      {ok, {OldArgTypes0, OldOut0}} ->
        NewArgTypes0 = [t_sup(X, Y) ||
                         {X, Y} <- lists:zip(ArgTypes, OldArgTypes0)],
	{NewArgTypes0, OldOut0,
         t_is_equal(t_product(NewArgTypes0), t_product(OldArgTypes0))}
    end,
  case Fixpoint of
    true -> State;
    false ->
      NewWork = add_work(Fun, Work),
      ?debug("~tw: forwarding args ~ts\n",
	     [state__lookup_name(Fun, State),
	      t_to_string(t_product(NewArgTypes))]),
      NewFunTab = dict:store(Fun, {NewArgTypes, OldOut}, FunTab),
      State#state{work = NewWork, fun_tab = NewFunTab}
  end.

-spec state__cleanup(state()) -> state().

state__cleanup(#state{callgraph = Callgraph,
                      races = Races,
                      records = Records}) ->
  #state{callgraph = dialyzer_callgraph:cleanup(Callgraph),
         races = dialyzer_races:cleanup(Races),
         records = Records}.

-spec state__duplicate(state()) -> state().

state__duplicate(#state{callgraph = Callgraph} = State) ->
  State#state{callgraph = dialyzer_callgraph:duplicate(Callgraph)}.

-spec dispose_state(state()) -> ok.

dispose_state(#state{callgraph = Callgraph}) ->
  dialyzer_callgraph:dispose_race_server(Callgraph).

-spec state__get_callgraph(state()) -> dialyzer_callgraph:callgraph().

state__get_callgraph(#state{callgraph = Callgraph}) ->
  Callgraph.

-spec state__get_races(state()) -> dialyzer_races:races().

state__get_races(#state{races = Races}) ->
  Races.

-spec state__get_records(state()) -> types().

state__get_records(#state{records = Records}) ->
  Records.

-spec state__put_callgraph(dialyzer_callgraph:callgraph(), state()) ->
  state().

state__put_callgraph(Callgraph, State) ->
  State#state{callgraph = Callgraph}.

-spec state__put_races(dialyzer_races:races(), state()) -> state().

state__put_races(Races, State) ->
  State#state{races = Races}.

-spec state__records_only(state()) -> state().

state__records_only(#state{records = Records}) ->
  #state{records = Records}.

-spec state__translate_file(file:filename(), state()) -> file:filename().

state__translate_file(FakeFile, State) ->
  #state{codeserver = CodeServer, module = Module} = State,
  dialyzer_codeserver:translate_fake_file(CodeServer, Module, FakeFile).

%%% ===========================================================================
%%%
%%%  Races
%%%
%%% ===========================================================================

is_race_analysis_enabled(#state{races = Races, callgraph = Callgraph}) ->
  RaceDetection = dialyzer_callgraph:get_race_detection(Callgraph),
  RaceAnalysis = dialyzer_races:get_race_analysis(Races),
  RaceDetection andalso RaceAnalysis.

get_race_list_and_size(#state{races = Races}) ->
  dialyzer_races:get_race_list_and_size(Races).

renew_race_code(#state{races = Races, callgraph = Callgraph,
		       warning_mode = WarningMode} = State) ->
  case WarningMode of
    true -> State;
    false ->
      NewCallgraph = dialyzer_callgraph:renew_race_code(Races, Callgraph),
      State#state{callgraph = NewCallgraph}
  end.

renew_race_public_tables([Var], #state{races = Races, callgraph = Callgraph,
				      warning_mode = WarningMode} = State) ->
  case WarningMode of
    true -> State;
    false ->
      Table = dialyzer_races:get_new_table(Races),
      case Table of
	no_t -> State;
	_Other ->
	  VarLabel = get_label(Var),
	  NewCallgraph =
	    dialyzer_callgraph:renew_race_public_tables(VarLabel, Callgraph),
	  State#state{callgraph = NewCallgraph}
      end
  end.

%%% ===========================================================================
%%%
%%%  Worklist
%%%
%%% ===========================================================================

init_work(List) ->
  {List, [], sets:from_list(List)}.

get_work({[], [], _Set}) ->
  none;
get_work({[H|T], Rev, Set}) ->
  {H, {T, Rev, sets:del_element(H, Set)}};
get_work({[], Rev, Set}) ->
  get_work({lists:reverse(Rev), [], Set}).

add_work(New, {List, Rev, Set} = Work) ->
  case sets:is_element(New, Set) of
    true -> Work;
    false -> {List, [New|Rev], sets:add_element(New, Set)}
  end.

%%% ===========================================================================
%%%
%%%  Utilities.
%%%
%%% ===========================================================================

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([], _State) -> [];
get_file([{file, FakeFile}|_], State) ->
  state__translate_file(FakeFile, State);
get_file([_|Tail], State) ->
  get_file(Tail, State).

is_compiler_generated(Ann) ->
  lists:member(compiler_generated, Ann) orelse (get_line(Ann) < 1).

is_literal_record(Tree) ->
  Ann = cerl:get_ann(Tree),
  lists:member(record, Ann).

-spec format_args([cerl:cerl()], [type()], state()) ->
  nonempty_string().

format_args([], [], _State) ->
  "()";
format_args(ArgList0, TypeList, State) ->
  ArgList = fold_literals(ArgList0),
  "(" ++ format_args_1(ArgList, TypeList, State) ++ ")".

format_args_1([Arg], [Type], State) ->
  format_arg_1(Arg, Type, State);
format_args_1([Arg|Args], [Type|Types], State) ->
  format_arg_1(Arg, Type, State) ++ "," ++ format_args_1(Args, Types, State).

format_arg_1(Arg, Type, State) ->
  case cerl:is_literal(Arg) of
    true -> format_cerl(Arg);
    false -> format_arg(Arg) ++ format_type(Type, State)
  end.

format_arg(Arg) ->
  Default = "",
  case cerl:is_c_var(Arg) of
    true ->
      case cerl:var_name(Arg) of
	Atom when is_atom(Atom) ->
	  case atom_to_list(Atom) of
	    "@"++_ -> Default;
	    "cor"++_ -> Default;
	    "rec"++_ -> Default;
	    Name -> Name ++ "::"
	  end;
	_What -> Default
      end;
    false ->
      Default
  end.

-spec format_type(type(), state()) -> string().

format_type(Type, #state{records = R}) ->
  t_to_string(Type, R).

-spec format_field_diffs(type(), state()) -> string().

format_field_diffs(RecConstruction, #state{records = R}) ->
  erl_types:record_field_diffs_to_string(RecConstruction, R).

-spec format_sig_args(type(), state()) -> string().

format_sig_args(Type, #state{opaques = Opaques} = State) ->
  SigArgs = t_fun_args(Type, Opaques),
  case SigArgs of
    [] -> "()";
    [SArg|SArgs] ->
      lists:flatten("(" ++ format_type(SArg, State)
		        ++ ["," ++ format_type(T, State) || T <- SArgs] ++ ")")
    end.

format_cerl(Tree) ->
  cerl_prettypr:format(cerl:set_ann(Tree, []),
		       [{hook, dialyzer_utils:pp_hook()},
			{noann, true},
			{paper, 100000}, %% These guys strip
			{ribbon, 100000} %% newlines.
		       ]).

format_patterns(Pats0) ->
  Pats = fold_literals(Pats0),
  NewPats = map_pats(cerl:c_values(Pats)),
  String = format_cerl(NewPats),
  case Pats of
    [PosVar] ->
      case cerl:is_c_var(PosVar) andalso (cerl:var_name(PosVar) =/= '') of
	true -> "variable "++String;
	false -> "pattern "++String
      end;
    _ ->
      "pattern "++String
  end.

map_pats(Pats) ->
  Fun = fun(Tree) ->
	    case cerl:is_c_var(Tree) of
	      true ->
		case cerl:var_name(Tree) of
		  Atom when is_atom(Atom) ->
		    case atom_to_list(Atom) of
		      "@"++_ -> cerl:c_var('');
		      "cor"++_ -> cerl:c_var('');
		      "rec"++_ -> cerl:c_var('');
		      _ -> cerl:set_ann(Tree, [])
		    end;
		  _What -> cerl:c_var('')
		end;
	      false ->
		cerl:set_ann(Tree, [])
	    end
	end,
  cerl_trees:map(Fun, Pats).

fold_literals(TreeList) ->
  [cerl:fold_literal(Tree) || Tree <- TreeList].

format_atom(A) ->
  format_cerl(cerl:c_atom(A)).

type(Tree) ->
  Folded = cerl:fold_literal(Tree),
  case cerl:type(Folded) of
    literal -> {literal, Folded};
    Type -> Type
  end.

is_literal(Tree) ->
  Folded = cerl:fold_literal(Tree),
  case cerl:is_literal(Folded) of
    true -> {yes, Folded};
    false -> no
  end.

parent_allows_this(FunLbl, #state{callgraph = Callgraph, plt = Plt} =State) ->
  case state__is_escaping(FunLbl, State) of
    false -> false; % if it isn't escaping it can't be a return value
    true ->
      case state__lookup_name(FunLbl, State) of
	{_M, _F, _A} -> false; % if it has a name it is not a fun
	_ ->
	  case dialyzer_callgraph:in_neighbours(FunLbl, Callgraph) of
	    [Parent] ->
	      case state__lookup_name(Parent, State) of
		{_M, _F, _A} = PMFA ->
		  case dialyzer_plt:lookup_contract(Plt, PMFA) of
		    none -> false;
		    {value, C} ->
		      GenRet = dialyzer_contracts:get_contract_return(C),
		      case erl_types:t_is_fun(GenRet) of
			false -> false; % element of structure? far-fetched...
			true -> t_is_unit(t_fun_range(GenRet))
		      end
		  end;
		_ -> false % parent should have a name to have a contract
	      end;
	    _ -> false % called in other funs? far-fetched...
	  end
      end
  end.

find_function({_, _, _} = MFA, _State) ->
  MFA;
find_function(top, _State) ->
  top;
find_function(FunLbl, #state{fun_homes = Homes}) ->
  dict:fetch(FunLbl, Homes).

classify_returns(Tree) ->
  case find_terminals(cerl:fun_body(Tree)) of
    {false, false} -> no_match;
    {true, false} -> only_explicit;
    {false, true} -> only_normal;
    {true, true} -> both
  end.

find_terminals(Tree) ->
  case cerl:type(Tree) of
    apply -> {false, true};
    binary -> {false, true};
    bitstr -> {false, true};
    call ->
      M0 = cerl:call_module(Tree),
      F0 = cerl:call_name(Tree),
      A = length(cerl:call_args(Tree)),
      case {is_literal(M0), is_literal(F0)} of
        {{yes, LitM}, {yes, LitF}} ->
	  M = cerl:concrete(LitM),
	  F = cerl:concrete(LitF),
	  case (erl_bif_types:is_known(M, F, A)
		andalso t_is_none(erl_bif_types:type(M, F, A))) of
	    true -> {true, false};
	    false -> {false, true}
	  end;
        _ ->
	  %% We cannot make assumptions. Say that both are true.
	  {true, true}
      end;
    'case' ->
      case cerl:case_clauses(Tree) of
        [] ->
          case lists:member(receive_timeout, cerl:get_ann(Tree)) of
            true ->
              %% Handle a never ending receive without any
              %% clauses specially. (Not sure why.)
              {false, true};
            false ->
              {false, false}
          end;
        [_|_] ->
          find_terminals_list(cerl:case_clauses(Tree))
      end;
    'catch' -> find_terminals(cerl:catch_body(Tree));
    clause -> find_terminals(cerl:clause_body(Tree));
    cons -> {false, true};
    'fun' -> {false, true};
    'let' -> find_terminals(cerl:let_body(Tree));
    letrec -> find_terminals(cerl:letrec_body(Tree));
    literal -> {false, true};
    map -> {false, true};
    primop -> {false, false}; %% match_fail, etc. are not explicit exits.
    'receive' ->
      Timeout = cerl:receive_timeout(Tree),
      Clauses = cerl:receive_clauses(Tree),
      case (cerl:is_literal(Timeout) andalso
	    (cerl:concrete(Timeout) =:= infinity)) of
	true ->
	  if Clauses =:= [] -> {false, true}; %% A never ending receive.
	     true -> find_terminals_list(Clauses)
	  end;
	false -> find_terminals_list([cerl:receive_action(Tree)|Clauses])
      end;
    seq -> find_terminals(cerl:seq_body(Tree));
    'try' ->
      find_terminals_list([cerl:try_handler(Tree), cerl:try_body(Tree)]);
    tuple -> {false, true};
    values -> {false, true};
    var -> {false, true}
  end.

find_terminals_list(List) ->
  find_terminals_list(List, false, false).

find_terminals_list([Tree|Left], Explicit1, Normal1) ->
  {Explicit2, Normal2} = find_terminals(Tree),
  case {Explicit1 or Explicit2, Normal1 or Normal2} of
    {true, true} = Ans -> Ans;
    {NewExplicit, NewNormal} ->
      find_terminals_list(Left, NewExplicit, NewNormal)
  end;
find_terminals_list([], Explicit, Normal) ->
  {Explicit, Normal}.

%%----------------------------------------------------------------------------

-ifdef(DEBUG_PP).
debug_pp(Tree, true) ->
  io:put_chars(cerl_prettypr:format(Tree, [{hook, cerl_typean:pp_hook()}])),
  io:nl(),
  ok;
debug_pp(Tree, false) ->
  io:put_chars(cerl_prettypr:format(strip_annotations(Tree))),
  io:nl(),
  ok.

strip_annotations(Tree) ->
  Fun = fun(T) ->
	    case cerl:type(T) of
	      var ->
		cerl:set_ann(T, [{label, cerl_trees:get_label(T)}]);
	      'fun' ->
		cerl:set_ann(T, [{label, cerl_trees:get_label(T)}]);
	      _ ->
		cerl:set_ann(T, [])
	    end
	end,
  cerl_trees:map(Fun, Tree).

-else.

debug_pp(_Tree, _UseHook) ->
  ok.
-endif.
