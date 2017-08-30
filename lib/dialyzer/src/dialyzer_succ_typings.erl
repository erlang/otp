%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2015. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_succ_typings.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 11 Sep 2006 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_succ_typings).

-export([analyze_callgraph/3, 
	 analyze_callgraph/6,
	 get_warnings/7
	]).

-export([
	 find_succ_types_for_scc/2,
	 refine_one_module/2,
	 find_required_by/2,
	 find_depends_on/2,
	 collect_warnings/2,
	 lookup_names/2
	]).

-export_type([typesig_init_data/0, dataflow_init_data/0, warnings_init_data/0]).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

-define(TYPE_LIMIT, 4).

%%--------------------------------------------------------------------

-include("dialyzer.hrl").

%%--------------------------------------------------------------------
%% State record -- local to this module

-type parent() :: 'none' | pid().
-type typesig_init_data() :: term().
-type dataflow_init_data() :: term().
-type warnings_init_data() :: term().

-type fixpoint_init_data() :: typesig_init_data() | dataflow_init_data().

-type scc()             :: [mfa_or_funlbl()] | [module()].

-record(st, {callgraph      :: dialyzer_callgraph:callgraph(),
	     codeserver     :: dialyzer_codeserver:codeserver(),
	     parent = none  :: parent(),
	     timing_server  :: dialyzer_timing:timing_server(),
             solvers        :: [solver()],
	     plt            :: dialyzer_plt:plt()}).

%%--------------------------------------------------------------------

-spec analyze_callgraph(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
			dialyzer_codeserver:codeserver()) ->
	 dialyzer_plt:plt().

analyze_callgraph(Callgraph, Plt, Codeserver) ->
  analyze_callgraph(Callgraph, Plt, Codeserver, none, [], none).

-spec analyze_callgraph(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
			dialyzer_codeserver:codeserver(),
			dialyzer_timing:timing_server(),
                        [solver()], parent()) ->
         dialyzer_plt:plt().

analyze_callgraph(Callgraph, Plt, Codeserver, TimingServer, Solvers, Parent) ->
  NewState =
    init_state_and_get_success_typings(Callgraph, Plt, Codeserver,
				       TimingServer, Solvers, Parent),
  dialyzer_plt:restore_full_plt(NewState#st.plt, Plt).

%%--------------------------------------------------------------------

init_state_and_get_success_typings(Callgraph, Plt, Codeserver,
				   TimingServer, Solvers, Parent) ->
  {SCCs, Callgraph1} =
    ?timing(TimingServer, "order", dialyzer_callgraph:finalize(Callgraph)),
  State = #st{callgraph = Callgraph1, plt = dialyzer_plt:get_mini_plt(Plt),
	      codeserver = Codeserver, parent = Parent,
	      timing_server = TimingServer, solvers = Solvers},
  get_refined_success_typings(SCCs, State).

get_refined_success_typings(SCCs, #st{callgraph = Callgraph,
				      timing_server = TimingServer} = State) ->
  case find_succ_typings(SCCs, State) of
    {fixpoint, State1} -> State1;
    {not_fixpoint, NotFixpoint1, State1} ->
      {ModulePostorder, ModCallgraph} =
	?timing(
	   TimingServer, "order", _C1,
	   dialyzer_callgraph:module_postorder_from_funs(NotFixpoint1,
							 Callgraph)),
      ModState = State1#st{callgraph = ModCallgraph},
      case refine_succ_typings(ModulePostorder, ModState) of
	{fixpoint, State2} ->
	  State2;
	{not_fixpoint, NotFixpoint2, State2} ->
	  %% Need to reset the callgraph.
	  {NewSCCs, Callgraph2} =
	    ?timing(TimingServer, "order", _C2,
		    dialyzer_callgraph:reset_from_funs(NotFixpoint2,
						       ModCallgraph)),
	  NewState = State2#st{callgraph = Callgraph2},
	  get_refined_success_typings(NewSCCs, NewState)
      end
  end.

-spec get_warnings(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
		   doc_plt(), dialyzer_codeserver:codeserver(),
		   dialyzer_timing:timing_server(), [solver()], pid()) ->
	 {[raw_warning()], dialyzer_plt:plt(), doc_plt()}.

get_warnings(Callgraph, Plt, DocPlt, Codeserver,
	     TimingServer, Solvers, Parent) ->
  InitState =
    init_state_and_get_success_typings(Callgraph, Plt, Codeserver,
				       TimingServer, Solvers, Parent),
  Mods = dialyzer_callgraph:modules(InitState#st.callgraph),
  MiniPlt = InitState#st.plt,
  FindOpaques = lookup_and_find_opaques_fun(Codeserver),
  CWarns =
    dialyzer_contracts:get_invalid_contract_warnings(Mods, Codeserver,
                                                     MiniPlt, FindOpaques),
  MiniDocPlt = dialyzer_plt:get_mini_plt(DocPlt),
  ModWarns =
    ?timing(TimingServer, "warning",
	    get_warnings_from_modules(Mods, InitState, MiniDocPlt)),
  {postprocess_warnings(CWarns ++ ModWarns, Codeserver),
   dialyzer_plt:restore_full_plt(MiniPlt, Plt),
   dialyzer_plt:restore_full_plt(MiniDocPlt, DocPlt)}.

get_warnings_from_modules(Mods, State, DocPlt) ->
  #st{callgraph = Callgraph, codeserver = Codeserver,
      plt = Plt, timing_server = TimingServer} = State,
  Init = {Codeserver, Callgraph, Plt, DocPlt},
  dialyzer_coordinator:parallel_job(warnings, Mods, Init, TimingServer).

-spec collect_warnings(module(), warnings_init_data()) -> [raw_warning()].

collect_warnings(M, {Codeserver, Callgraph, Plt, DocPlt}) ->
  ModCode = dialyzer_codeserver:lookup_mod_code(M, Codeserver),
  Records = dialyzer_codeserver:lookup_mod_records(M, Codeserver),
  Contracts = dialyzer_codeserver:lookup_mod_contracts(M, Codeserver),
  AllFuns = collect_fun_info([ModCode]),
  %% Check if there are contracts for functions that do not exist
  Warnings1 =
    dialyzer_contracts:contracts_without_fun(Contracts, AllFuns, Callgraph),
  {Warnings2, FunTypes} =
    dialyzer_dataflow:get_warnings(ModCode, Plt, Callgraph, Codeserver,
				   Records),
  Attrs = cerl:module_attrs(ModCode),
  Warnings3 =
    dialyzer_behaviours:check_callbacks(M, Attrs, Records, Plt, Codeserver),
  DocPlt = insert_into_doc_plt(FunTypes, Callgraph, DocPlt),
  lists:flatten([Warnings1, Warnings2, Warnings3]).

postprocess_warnings(RawWarnings, Codeserver) ->
  Pred =
    fun({?WARN_CONTRACT_RANGE, _, _}) -> true;
       (_) -> false
    end,
  {CRWarns, NonCRWarns} = lists:partition(Pred, RawWarnings),
  postprocess_dataflow_warns(CRWarns, Codeserver, NonCRWarns, []).

postprocess_dataflow_warns([], _Callgraph, WAcc, Acc) ->
  lists:reverse(Acc, WAcc);
postprocess_dataflow_warns([{?WARN_CONTRACT_RANGE, WarningInfo, Msg}|Rest],
			   Codeserver, WAcc, Acc) ->
  {CallF, CallL, _CallMFA} = WarningInfo,
  {contract_range, [Contract, M, F, A, ArgStrings, CRet]} = Msg,
  case dialyzer_codeserver:lookup_mfa_contract({M,F,A}, Codeserver) of
    {ok, {{ContrF, ContrL}, _C, _X}} ->
      case CallF =:= ContrF of
	true ->
	  NewMsg = {contract_range, [Contract, M, F, ArgStrings, CallL, CRet]},
          WarningInfo2 = {ContrF, ContrL, {M, F, A}},
	  W = {?WARN_CONTRACT_RANGE, WarningInfo2, NewMsg},
	  Filter =
	    fun({?WARN_CONTRACT_TYPES, WI, _}) when WI =:= WarningInfo2 -> false;
	       (_) -> true
	    end,
	  FilterWAcc = lists:filter(Filter, WAcc),
	  postprocess_dataflow_warns(Rest, Codeserver, FilterWAcc, [W|Acc]);
	false ->
	  postprocess_dataflow_warns(Rest, Codeserver, WAcc, Acc)
      end;
    error ->
      %% The contract is not in a module that is currently under analysis.
      %% We display the warning in the file/line of the call.
      NewMsg = {contract_range, [Contract, M, F, ArgStrings, CallL, CRet]},
      W = {?WARN_CONTRACT_RANGE, WarningInfo, NewMsg},
      postprocess_dataflow_warns(Rest, Codeserver, WAcc, [W|Acc])
  end.
  
refine_succ_typings(Modules, #st{codeserver = Codeserver,
                                 callgraph = Callgraph,
                                 plt = Plt,
				 timing_server = Timing,
                                 solvers = Solvers} = State) ->
  ?debug("Module postorder: ~p\n", [Modules]),
  Init = {Codeserver, Callgraph, Plt, Solvers},
  NotFixpoint =
    ?timing(Timing, "refine",
	    dialyzer_coordinator:parallel_job(dataflow, Modules, Init, Timing)),
  ?debug("==================== Dataflow done ====================\n\n", []),
  case NotFixpoint =:= [] of
    true -> {fixpoint, State};
    false -> {not_fixpoint, NotFixpoint, State}
  end.

-spec find_depends_on(scc() | module(), fixpoint_init_data()) -> [scc()].

find_depends_on(SCC, {_Codeserver, Callgraph, _Plt, _Solvers}) ->
  dialyzer_callgraph:get_depends_on(SCC, Callgraph).

-spec find_required_by(scc() | module(), fixpoint_init_data()) -> [scc()].

find_required_by(SCC, {_Codeserver, Callgraph, _Plt, _Solvers}) ->
  dialyzer_callgraph:get_required_by(SCC, Callgraph).

-spec lookup_names([label()], fixpoint_init_data()) -> [mfa_or_funlbl()].

lookup_names(Labels, {_Codeserver, Callgraph, _Plt, _Solvers}) ->
  [lookup_name(F, Callgraph) || F <- Labels].

-spec refine_one_module(module(), dataflow_init_data()) -> [label()]. % ordset

refine_one_module(M, {CodeServer, Callgraph, Plt, _Solvers}) ->
  ModCode = dialyzer_codeserver:lookup_mod_code(M, CodeServer),
  AllFuns = collect_fun_info([ModCode]),
  Records = dialyzer_codeserver:lookup_mod_records(M, CodeServer),
  FunTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),
  NewFunTypes =
    dialyzer_dataflow:get_fun_types(ModCode, Plt, Callgraph, CodeServer, Records),
  Contracts1 = dialyzer_codeserver:lookup_mod_contracts(M, CodeServer),
  Contracts = orddict:from_list(dict:to_list(Contracts1)),
  FindOpaques = find_opaques_fun(Records),
  DecoratedFunTypes =
    decorate_succ_typings(Contracts, Callgraph, NewFunTypes, FindOpaques),
  %% ?debug("NewFunTypes       ~p\n   ~n", [dict:to_list(NewFunTypes)]),
  %% ?debug("refine DecoratedFunTypes ~p\n   ~n", [dict:to_list(DecoratedFunTypes)]),
  debug_pp_functions("Refine", NewFunTypes, DecoratedFunTypes, Callgraph),

  case reached_fixpoint(FunTypes, DecoratedFunTypes) of
    true -> [];
    {false, NotFixpoint} ->
      ?debug("Not fixpoint\n", []),
      Plt = insert_into_plt(dict:from_list(NotFixpoint), Callgraph, Plt),
      [FunLbl || {FunLbl,_Type} <- NotFixpoint]
  end.

reached_fixpoint(OldTypes, NewTypes) ->
  reached_fixpoint(OldTypes, NewTypes, false).

reached_fixpoint_strict(OldTypes, NewTypes) ->
  case reached_fixpoint(OldTypes, NewTypes, true) of
    true -> true;
    {false, _} -> false
  end.

reached_fixpoint(OldTypes0, NewTypes0, Strict) ->
  MapFun = fun(_Key, Type) ->
	       case is_failed_or_not_called_fun(Type) of
		 true -> failed_fun;
		 false -> erl_types:t_limit(Type, ?TYPE_LIMIT)
	       end
	   end,
  OldTypes = dict:map(MapFun, OldTypes0),
  NewTypes = dict:map(MapFun, NewTypes0),
  compare_types(OldTypes, NewTypes, Strict).

is_failed_or_not_called_fun(Type) ->
  erl_types:any_none([erl_types:t_fun_range(Type)|erl_types:t_fun_args(Type)]).

compare_types(Dict1, Dict2, Strict) ->  
  List1 = lists:keysort(1, dict:to_list(Dict1)),
  List2 = lists:keysort(1, dict:to_list(Dict2)),
  compare_types_1(List1, List2, Strict, []).

compare_types_1([{X, _Type1}|Left1], [{X, failed_fun}|Left2], 
		Strict, NotFixpoint) ->
  compare_types_1(Left1, Left2, Strict, NotFixpoint);
compare_types_1([{X, failed_fun}|Left1], [{X, _Type2}|Left2], 
		Strict, NotFixpoint) ->
  compare_types_1(Left1, Left2, Strict, NotFixpoint);
compare_types_1([{X, Type1}|Left1], [{X, Type2}|Left2], Strict, NotFixpoint) ->
  Res = case Strict of
	  true -> erl_types:t_is_equal(Type1, Type2);
	  false -> erl_types:t_is_subtype(Type1, Type2)
	end,
  case Res of
    true -> compare_types_1(Left1, Left2, Strict, NotFixpoint);
    false -> 
      ?debug("Failed fixpoint for ~w: ~s =/= ~s\n",
	     [X, erl_types:t_to_string(Type1), erl_types:t_to_string(Type2)]),
      compare_types_1(Left1, Left2, Strict, [{X, Type2}|NotFixpoint])
  end;
compare_types_1([_|Left1], List2, Strict, NotFixpoint) ->
  %% If the function was not called.
  compare_types_1(Left1, List2, Strict, NotFixpoint);
compare_types_1([], [], _Strict, NotFixpoint) ->
  case NotFixpoint =:= [] of
    true -> true;
    false -> {false, NotFixpoint}
  end.

find_succ_typings(SCCs, #st{codeserver = Codeserver, callgraph = Callgraph,
			    plt = Plt, timing_server = Timing,
                            solvers = Solvers} = State) ->
  Init = {Codeserver, Callgraph, Plt, Solvers},
  NotFixpoint =
    ?timing(Timing, "typesig",
	    dialyzer_coordinator:parallel_job(typesig, SCCs, Init, Timing)),
  ?debug("==================== Typesig done ====================\n\n", []),
  case NotFixpoint =:= [] of
    true -> {fixpoint, State};
    false -> {not_fixpoint, NotFixpoint, State}
  end.

-spec find_succ_types_for_scc(scc(), typesig_init_data()) -> [mfa_or_funlbl()].

find_succ_types_for_scc(SCC, {Codeserver, Callgraph, Plt, Solvers}) ->
  SCC_Info = [{MFA, 
	       dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
	       dialyzer_codeserver:lookup_mod_records(M, Codeserver)}
	      || {M, _, _} = MFA <- SCC],
  Contracts1 = [{MFA, dialyzer_codeserver:lookup_mfa_contract(MFA, Codeserver)}
		|| {_, _, _} = MFA <- SCC],
  Contracts2 = [{MFA, Contract} || {MFA, {ok, Contract}} <- Contracts1],
  Contracts3 = orddict:from_list(Contracts2),
  Label = dialyzer_codeserver:get_next_core_label(Codeserver),
  AllFuns = collect_fun_info([Fun || {_MFA, {_Var, Fun}, _Rec} <- SCC_Info]),
  PropTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),
  %% Assume that the PLT contains the current propagated types
  FunTypes = dialyzer_typesig:analyze_scc(SCC_Info, Label, Callgraph,
                                          Plt, PropTypes, Solvers),
  AllFunSet = sets:from_list([X || {X, _} <- AllFuns]),
  FilteredFunTypes =
    dict:filter(fun(X, _) -> sets:is_element(X, AllFunSet) end, FunTypes),
  FindOpaques = lookup_and_find_opaques_fun(Codeserver),
  DecoratedFunTypes =
    decorate_succ_typings(Contracts3, Callgraph, FilteredFunTypes, FindOpaques),
  %% Check contracts
  PltContracts =
    dialyzer_contracts:check_contracts(Contracts3, Callgraph,
                                       DecoratedFunTypes, FindOpaques),
  %% ?debug("FilteredFunTypes ~p\n   ~n", [dict:to_list(FilteredFunTypes)]),
  %% ?debug("SCC DecoratedFunTypes ~p\n   ~n", [dict:to_list(DecoratedFunTypes)]),
  debug_pp_functions("SCC", FilteredFunTypes, DecoratedFunTypes, Callgraph),
  ContractFixpoint =
    lists:all(fun({MFA, _C}) ->
		  %% Check the non-deleted PLT
		  case dialyzer_plt:lookup_contract(Plt, MFA) of
		    none -> false;
		    {value, _} -> true
		  end
	      end, PltContracts),
  Plt = insert_into_plt(DecoratedFunTypes, Callgraph, Plt),
  Plt = dialyzer_plt:insert_contract_list(Plt, PltContracts),
  case (ContractFixpoint andalso 
	reached_fixpoint_strict(PropTypes, DecoratedFunTypes)) of
    true -> [];
    false ->
      ?debug("Not fixpoint for: ~w\n", [AllFuns]),
      [Fun || {Fun, _Arity} <- AllFuns]
  end.

decorate_succ_typings(Contracts, Callgraph, FunTypes, FindOpaques) ->
  F = fun(Label, Type) ->
          case dialyzer_callgraph:lookup_name(Label, Callgraph) of
            {ok, MFA} ->
              case orddict:find(MFA, Contracts) of
                {ok, {_FileLine, Contract, _Xtra}} ->
                  Args = dialyzer_contracts:get_contract_args(Contract),
                  Ret = dialyzer_contracts:get_contract_return(Contract),
                  C = erl_types:t_fun(Args, Ret),
                  {M, _, _} = MFA,
                  Opaques = FindOpaques(M),
                  erl_types:t_decorate_with_opaque(Type, C, Opaques);
                error -> Type
              end;
            error -> Type
          end
      end,
  dict:map(F, FunTypes).

lookup_and_find_opaques_fun(Codeserver) ->
  fun(Module) ->
      Records = dialyzer_codeserver:lookup_mod_records(Module, Codeserver),
      (find_opaques_fun(Records))(Module)
  end.

find_opaques_fun(Records) ->
  fun(_Module) -> erl_types:t_opaque_from_records(Records) end.

get_fun_types_from_plt(FunList, Callgraph, Plt) ->
  get_fun_types_from_plt(FunList, Callgraph, Plt, dict:new()).

get_fun_types_from_plt([{FunLabel, Arity}|Left], Callgraph, Plt, Map) ->
  Type = lookup_fun_type(FunLabel, Arity, Callgraph, Plt),
  get_fun_types_from_plt(Left, Callgraph, Plt, dict:store(FunLabel, Type, Map));
get_fun_types_from_plt([], _Callgraph, _Plt, Map) ->
  Map.

collect_fun_info(Trees) ->
  collect_fun_info(Trees, []).

collect_fun_info([Tree|Trees], List) ->
  Fun = fun(SubTree, Acc) ->
	    case cerl:is_c_fun(SubTree) of
	      true ->
		[{cerl_trees:get_label(SubTree), cerl:fun_arity(SubTree)}|Acc];
	      false -> Acc
	    end
	end,
  collect_fun_info(Trees, cerl_trees:fold(Fun, List, Tree));
collect_fun_info([], List) ->
  List.

lookup_fun_type(Label, Arity, Callgraph, Plt) ->
  ID = lookup_name(Label, Callgraph),
  case dialyzer_plt:lookup(Plt, ID) of
    none -> erl_types:t_fun(Arity, erl_types:t_any());
    {value, {RetT, ArgT}} -> erl_types:t_fun(ArgT, RetT)
  end.

insert_into_doc_plt(_FunTypes, _Callgraph, undefined) ->
  undefined;
insert_into_doc_plt(FunTypes, Callgraph, DocPlt) ->
  SuccTypes = format_succ_types(FunTypes, Callgraph),
  dialyzer_plt:insert_list(DocPlt, SuccTypes).

insert_into_plt(SuccTypes0, Callgraph, Plt) ->
  SuccTypes = format_succ_types(SuccTypes0, Callgraph),
  debug_pp_succ_typings(SuccTypes),
  dialyzer_plt:insert_list(Plt, SuccTypes).

format_succ_types(SuccTypes, Callgraph) ->
  format_succ_types(dict:to_list(SuccTypes), Callgraph, []).

format_succ_types([{Label, Type0}|Left], Callgraph, Acc) ->
  Type = erl_types:t_limit(Type0, ?TYPE_LIMIT+1),
  Id = lookup_name(Label, Callgraph),
  NewTuple = {Id, {erl_types:t_fun_range(Type), erl_types:t_fun_args(Type)}},
  format_succ_types(Left, Callgraph, [NewTuple|Acc]);
format_succ_types([], _Callgraph, Acc) ->
  Acc.

-ifdef(DEBUG).
debug_pp_succ_typings(SuccTypes) ->
  ?debug("Succ typings:\n", []),
  [?debug("  ~w :: ~s\n", 
	  [MFA, erl_types:t_to_string(erl_types:t_fun(ArgT, RetT))])
   || {MFA, {RetT, ArgT}} <- SuccTypes],
  ?debug("Contracts:\n", []),
  [?debug("  ~w :: ~s\n", 
	  [MFA, erl_types:t_to_string(erl_types:t_fun(ArgT, RetFun(ArgT)))])
   || {MFA, {contract, RetFun, ArgT}} <- SuccTypes],
  ?debug("\n", []),
  ok.

debug_pp_functions(Header, FunTypes, DecoratedFunTypes, Callgraph) ->
  ?debug("FunTypes (~s)\n", [Header]),
  FTypes = lists:keysort(1, dict:to_list(FunTypes)),
  DTypes = lists:keysort(1, dict:to_list(DecoratedFunTypes)),
  Fun = fun({{Label, Type},{Label, DecoratedType}}) ->
            Name = lookup_name(Label, Callgraph),
            ?debug("~w (~w): ~s\n",
                   [Name, Label, erl_types:t_to_string(Type)]),
            case erl_types:t_is_equal(Type, DecoratedType) of
              true -> ok;
              false ->
                ?debug("  With opaque types: ~s\n",
                       [erl_types:t_to_string(DecoratedType)])
            end
        end,
  lists:foreach(Fun, lists:zip(FTypes, DTypes)),
  ?debug("\n", []).
-else.
debug_pp_succ_typings(_) ->
  ok.

debug_pp_functions(_, _, _, _) ->
  ok.
-endif.

lookup_name(F, CG) ->
  case dialyzer_callgraph:lookup_name(F, CG) of
    error -> F;
    {ok, Name} -> Name
  end.
