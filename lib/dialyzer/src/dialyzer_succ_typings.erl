%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%% Original author: Tobias Lindahl <tobiasl@it.uu.se>
%%
%% Purpose: Orchestrate calculation of success typings.
%%

-module(dialyzer_succ_typings).
-moduledoc false.

%% Main entry points.
-export([analyze_callgraph/5,
	 get_warnings/6
	]).

%% Entry points for dialyzer_worker.
-export([find_succ_types_for_scc/2,
	 refine_one_module/2,
         add_to_result/3,
	 find_depends_on/2,
	 collect_warnings/2
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

-type typesig_init_data() :: term().
-type dataflow_init_data() :: term().
-type warnings_init_data() :: term().

-type fixpoint_init_data() :: typesig_init_data() | dataflow_init_data().

-type scc()             :: [mfa_or_funlbl()] | [module()].

-record(st, {callgraph      :: dialyzer_callgraph:callgraph(),
	     codeserver     :: dialyzer_codeserver:codeserver(),
	     timing_server  :: dialyzer_timing:timing_server(),
             solvers        :: [solver()],
	     plt            :: dialyzer_plt:plt()}).

%% --------------------------------------------------------------------
%% The main entry points.

-spec analyze_callgraph(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
			dialyzer_codeserver:codeserver(),
			dialyzer_timing:timing_server(),
                        [solver()]) ->
         dialyzer_plt:plt().

analyze_callgraph(Callgraph, Plt, Codeserver, TimingServer, Solvers) ->
  _ = get_success_typings(Callgraph, Plt, Codeserver, TimingServer, Solvers),
  Plt.

-spec get_warnings(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
		   doc_plt(), dialyzer_codeserver:codeserver(),
		   dialyzer_timing:timing_server(), [solver()]) ->
	 {[raw_warning()], dialyzer_plt:plt(), doc_plt()}.

get_warnings(Callgraph, Plt, DocPlt, Codeserver,
	     TimingServer, Solvers) ->
  InitState = get_success_typings(Callgraph, Plt, Codeserver,
                                  TimingServer, Solvers),
  Mods = dialyzer_callgraph:modules(InitState#st.callgraph),
  CWarns =
    dialyzer_contracts:get_invalid_contract_warnings(Mods, Codeserver, Plt),
  ModWarns =
    ?timing(TimingServer, "warning",
	    get_warnings_from_modules(Mods, InitState, DocPlt)),
  {postprocess_warnings(CWarns ++ ModWarns, Codeserver),
   Plt,
   DocPlt}.

%% --------------------------------------------------------------------
%% Callback functions called from dialyzer_worker.

-spec find_succ_types_for_scc(scc(), typesig_init_data()) -> [mfa_or_funlbl()].

find_succ_types_for_scc(SCC0, {Codeserver, Callgraph, Plt, Solvers}) ->
  SCC = [MFA || {_, _, _} = MFA <- SCC0],
  Label = dialyzer_codeserver:get_next_core_label(Codeserver),
  F = fun(MFA) ->
          {_Var, Fun} = dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
          collect_fun_info(Fun)
      end,
  AllFuns = lists:flatmap(F, SCC),
  PropTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),

  %% Assume that the PLT contains the current propagated types
  FunTypes = dialyzer_typesig:analyze_scc(SCC, Label, Callgraph,
                                          Codeserver, Plt, PropTypes,
                                          Solvers),

  %% FunTypes may now have picked up funs outside of the SCC. Get rid of them.
  AllFunKeys = [X || {X, _} <- AllFuns],
  Set = sofs:set(AllFunKeys, [id]),
  BinRel = sofs:from_external(FunTypes, [{id,type}]), %Already sorted.
  FilteredFunTypes = sofs:to_external(sofs:restriction(BinRel, Set)),

  FunMFAContracts = get_contracts(FilteredFunTypes, Callgraph, Codeserver),

  %% Check contracts
  Contracts = orddict:from_list([{MFA, Contract} ||
                                  {_, {MFA, Contract}} <- FunMFAContracts]),
  PltContracts =
    dialyzer_contracts:check_contracts(Contracts, Callgraph,
                                       FilteredFunTypes),

  NewPltContracts = [MC ||
                      {MFA, _C}=MC <- PltContracts,
                      %% Check the non-deleted PLT
                      not dialyzer_plt:is_contract(Plt, MFA)],

  _ = insert_into_plt(FilteredFunTypes, Callgraph, Plt),
  _ = dialyzer_plt:insert_contract_list(Plt, NewPltContracts),

  %% Check whether we have reached a fixpoint.
  case NewPltContracts =:= [] andalso
    reached_fixpoint_strict(PropTypes, FilteredFunTypes) of
    true -> [];
    false ->
      ?debug("Not fixpoint for: ~tw\n", [AllFuns]),
      AllFunKeys
  end.

-spec refine_one_module(module(), dataflow_init_data()) -> [mfa_or_funlbl()].

refine_one_module(M, {CodeServer, Callgraph, Plt, _Solvers}) ->
  ModCode = dialyzer_codeserver:lookup_mod_code(M, CodeServer),
  AllFuns = collect_fun_info(ModCode),
  FunTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),
  Records = dialyzer_codeserver:lookup_mod_records(M, CodeServer),
  NewFunTypes =
    dialyzer_dataflow:get_fun_types(ModCode, Plt, Callgraph, CodeServer, Records),

  case updated_types(FunTypes, NewFunTypes) of
    [] -> [];
    [_|_]=NotFixpoint ->
      ?debug("Not fixpoint\n", []),
      _ = insert_into_plt(NotFixpoint, Callgraph, Plt),
      [FunLbl || {FunLbl,_Type} <- NotFixpoint]
  end.

-spec collect_warnings(module(), warnings_init_data()) -> [raw_warning()].

collect_warnings(M, {Codeserver, Callgraph, Plt, DocPlt}) ->
  ModCode = dialyzer_codeserver:lookup_mod_code(M, Codeserver),
  Contracts = dialyzer_codeserver:lookup_mod_contracts(M, Codeserver),
  AllFuns = collect_fun_info(ModCode),
  %% Check if there are contracts for functions that do not exist
  Warnings1 =
    dialyzer_contracts:contracts_without_fun(Contracts, AllFuns, Callgraph),
  Attrs = cerl:module_attrs(ModCode),
  Records = dialyzer_codeserver:lookup_mod_records(M, Codeserver),
  {Warnings2, FunTypes} =
    dialyzer_dataflow:get_warnings(ModCode, Plt, Callgraph, Codeserver,
				   Records),
  Warnings3 =
    dialyzer_behaviours:check_callbacks(M, Attrs, Records, Plt, Codeserver),
  DocPlt = insert_into_doc_plt(FunTypes, Callgraph, DocPlt),
  lists:flatten([Warnings1, Warnings2, Warnings3]).

-spec find_depends_on(scc() | module(), fixpoint_init_data()) -> [scc()].

find_depends_on(SCC, {_Codeserver, Callgraph, _Plt, _Solvers}) ->
  dialyzer_callgraph:get_depends_on(SCC, Callgraph).

-spec add_to_result([label()], [mfa()], fixpoint_init_data()) -> [mfa()].

add_to_result(Labels, Result, {_Codeserver, Callgraph, _Plt, _Solver}) ->
  [lookup_name(Label, Callgraph) || Label <- Labels] ++ Result.

%% --------------------------------------------------------------------
%% Local functions.
%% --------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Calculate success typings.

get_success_typings(Callgraph, Plt, Codeserver, TimingServer, Solvers) ->
  %% Condense the call graph to its strongly connected components (SCCs).
  {LabelledSCCs, Callgraph1} =
    ?timing(TimingServer, "order", dialyzer_callgraph:finalize(Callgraph)),
  State = #st{callgraph = Callgraph1, plt = Plt,
              codeserver = Codeserver,
              timing_server = TimingServer, solvers = Solvers},
  get_refined_success_typings(LabelledSCCs, State).

get_refined_success_typings(LabelledSCCs, #st{callgraph = Callgraph,
				      timing_server = TimingServer} = State) ->
  %% Find the success types for the SCCs.
  case find_succ_typings(LabelledSCCs, State) of
    [] ->
      %% No new type information was discovered. We are done.
      State;
    NotFixpoint1 ->
      %% New type information was discovered. Refine the type
      %% information in each module using a dataflow analysis.
      {ModulePostorder, ModCallgraph} =
	?timing(TimingServer, "order", _C1,
                dialyzer_callgraph:module_postorder_from_funs(NotFixpoint1,
                                                              Callgraph)),

      ModState = State#st{callgraph = ModCallgraph},
      case refine_succ_typings(ModulePostorder, ModState) of
        [] ->
          %% No new type information was found. We are done.
          ModState;
	NotFixpoint2 ->
	  %% Need to reset the callgraph before repeating.
	  {NewLabelledSCCs, Callgraph2} =
	    ?timing(TimingServer, "order", _C2,
		    dialyzer_callgraph:reset_from_funs(NotFixpoint2,
						       ModCallgraph)),
	  NewState = ModState#st{callgraph = Callgraph2},
	  get_refined_success_typings(NewLabelledSCCs, NewState)
      end
  end.

find_succ_typings(LabelledSCCs, State) ->
  {Init, Timing} = init_pass_data(State),
  Updated =
    ?timing(Timing, "typesig",
	    dialyzer_coordinator:parallel_job(typesig, LabelledSCCs, Init, Timing)),
  ?debug("==================== Typesig done ====================\n\n", []),
  Updated.

refine_succ_typings(Modules, State) ->
  {Init, Timing} = init_pass_data(State),
  Updated =
    ?timing(Timing, "refine",
            dialyzer_coordinator:parallel_job(dataflow, Modules, Init, Timing)),
  ?debug("==================== Dataflow done ====================\n\n", []),
  Updated.

init_pass_data(#st{codeserver = Codeserver,
                   callgraph = Callgraph,
                   plt = Plt,
                   timing_server = Timing,
                   solvers = Solvers}) ->
  Init = {Codeserver, Callgraph, Plt, Solvers},
  {Init, Timing}.

%%--------------------------------------------------------------------
%% Produce warnings.

get_warnings_from_modules(Mods, State, DocPlt) ->
  #st{callgraph = Callgraph, codeserver = Codeserver,
      plt = Plt, timing_server = TimingServer} = State,
  Init = {Codeserver, Callgraph, Plt, DocPlt},
  dialyzer_coordinator:parallel_job(warnings, Mods, Init, TimingServer).

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

%%--------------------------------------------------------------------
%% Helpers.

reached_fixpoint_strict([{Key,Type1}|Types1], [{Key,Type2}|Types2]) ->
  case is_failed_or_not_called_fun(Type2) of
    true ->
      reached_fixpoint_strict(Types1, Types2);
    false ->
      LimitedType1 = erl_types:t_limit(Type1, ?TYPE_LIMIT),
      LimitedType2 = erl_types:t_limit(Type2, ?TYPE_LIMIT),
      erl_types:t_is_equal(LimitedType1, LimitedType2) andalso
        reached_fixpoint_strict(Types1, Types2)
  end;
reached_fixpoint_strict([{Key1,_}|Types1], [{Key2,_}|_]=Types2)
  when Key1 < Key2 ->
  %% The function was never called.
  reached_fixpoint_strict(Types1, Types2);
reached_fixpoint_strict([], []) ->
  true.

updated_types(OldTypes, NewTypes) ->
  updated_types_1(OldTypes, NewTypes, []).

updated_types_1([{Key,Type1}|Types1], [{Key,Type2}|Types2], Acc) ->
  case is_failed_or_not_called_fun(Type2) of
    true ->
      updated_types_1(Types1, Types2, Acc);
    false ->
      LimitedType1 = erl_types:t_limit(Type1, ?TYPE_LIMIT),
      LimitedType2 = erl_types:t_limit(Type2, ?TYPE_LIMIT),
      case erl_types:t_is_subtype(LimitedType1, LimitedType2) of
        true ->
          updated_types_1(Types1, Types2, Acc);
        false ->
          ?debug("Failed fixpoint for ~w: ~ts =/= ~ts\n",
                 [Key, erl_types:t_to_string(Type1), erl_types:t_to_string(Type2)]),
          updated_types_1(Types1, Types2, [{Key, Type2}|Acc])
      end
  end;
updated_types_1([], [], Acc) ->
  Acc.

is_failed_or_not_called_fun(Type) ->
  erl_types:any_none([erl_types:t_fun_range(Type)|erl_types:t_fun_args(Type)]).

get_contracts(FunTypes, Callgraph, Codeserver) ->
  F = fun({Label, _Type}=LabelType, Acc) ->
          case dialyzer_callgraph:lookup_name(Label, Callgraph) of
            {ok, MFA} ->
              case dialyzer_codeserver:lookup_mfa_contract(MFA, Codeserver) of
                {ok, {_FileLocation, Contract, _Xtra}} ->
                  [{LabelType, {MFA, Contract}}|Acc];
                error -> [{LabelType, no}|Acc]
              end;
            error -> [{LabelType, no}|Acc]
          end
      end,
  lists:foldl(F, [], FunTypes).

get_fun_types_from_plt(FunList, Callgraph, Plt) ->
  get_fun_types_from_plt(FunList, Callgraph, Plt, []).

get_fun_types_from_plt([{FunLabel, Arity}|Left], Callgraph, Plt, Map) ->
  Type = lookup_fun_type(FunLabel, Arity, Callgraph, Plt),
  get_fun_types_from_plt(Left, Callgraph, Plt, [{FunLabel, Type}|Map]);
get_fun_types_from_plt([], _Callgraph, _Plt, Map) ->
  orddict:from_list(Map).

collect_fun_info(Tree) ->
  Fun = fun(SubTree, Acc) ->
	    case cerl:is_c_fun(SubTree) of
	      true ->
		[{cerl_trees:get_label(SubTree), cerl:fun_arity(SubTree)}|Acc];
	      false -> Acc
	    end
	end,
  cerl_trees:fold(Fun, [], Tree).

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
  format_succ_types(SuccTypes, Callgraph, []).

format_succ_types([{Label, Type0}|Left], Callgraph, Acc) ->
  Type = erl_types:t_limit(Type0, ?TYPE_LIMIT+1),
  Id = lookup_name(Label, Callgraph),
  NewTuple = {Id, {erl_types:t_fun_range(Type), erl_types:t_fun_args(Type)}},
  format_succ_types(Left, Callgraph, [NewTuple|Acc]);
format_succ_types([], _Callgraph, Acc) ->
  Acc.

lookup_name(F, CG) ->
  case dialyzer_callgraph:lookup_name(F, CG) of
    error -> F;
    {ok, Name} -> Name
  end.

%%--------------------------------------------------------------------
%% Debugging.

-ifdef(DEBUG).
debug_pp_succ_typings(SuccTypes) ->
  ?debug("Succ typings:\n", []),
  [?debug("  ~tw :: ~ts\n",
	  [MFA, erl_types:t_to_string(erl_types:t_fun(ArgT, RetT))])
   || {MFA, {RetT, ArgT}} <- SuccTypes],
  ?debug("Contracts:\n", []),
  [?debug("  ~tw :: ~ts\n",
	  [MFA, erl_types:t_to_string(erl_types:t_fun(ArgT, RetFun(ArgT)))])
   || {MFA, {contract, RetFun, ArgT}} <- SuccTypes],
  ?debug("\n", []),
  ok.
-else.
debug_pp_succ_typings(_) ->
  ok.
-endif.
