%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_succ_typings.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 11 Sep 2006 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_succ_typings).

-export([analyze_callgraph/3, 
	 analyze_callgraph/4,
	 get_warnings/6,
	 find_succ_types_for_scc/1,
	 refine_one_module/1,
	 collect_scc_data/2,
	 collect_refine_scc_data/2,
	 find_required_by/2,
	 find_depends_on/2
	]).

%% These are only intended as debug functions.
-export([doit/1,
	 get_top_level_signatures/3]).

%%-define(DEBUG, true).
%%-define(DEBUG_PP, true).

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

-record(st, {callgraph      :: dialyzer_callgraph:callgraph(),
	     codeserver     :: dialyzer_codeserver:codeserver(),
	     no_warn_unused :: set(),
	     parent = none  :: parent(),
	     plt            :: dialyzer_plt:plt()}).

%%--------------------------------------------------------------------

-spec analyze_callgraph(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
			dialyzer_codeserver:codeserver()) ->
	 dialyzer_plt:plt().

analyze_callgraph(Callgraph, Plt, Codeserver) ->
  analyze_callgraph(Callgraph, Plt, Codeserver, none).

-spec analyze_callgraph(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
			dialyzer_codeserver:codeserver(), parent()) ->
         dialyzer_plt:plt().

analyze_callgraph(Callgraph, Plt, Codeserver, Parent) ->
  NewState =
    init_state_and_get_success_typings(Callgraph, Plt, Codeserver, Parent),
  dialyzer_plt:restore_full_plt(NewState#st.plt, Plt).

%%--------------------------------------------------------------------

init_state_and_get_success_typings(Callgraph, Plt, Codeserver, Parent) ->
  {SCCs, Callgraph1} = dialyzer_callgraph:finalize(Callgraph),
  State = #st{callgraph = Callgraph1, plt = dialyzer_plt:get_mini_plt(Plt),
	      codeserver = Codeserver, parent = Parent},
  get_refined_success_typings(SCCs, State).

get_refined_success_typings(SCCs, State) ->
  case find_succ_typings(SCCs, State) of
    {fixpoint, State1} -> State1;
    {not_fixpoint, NotFixpoint1, State1} ->
      Callgraph = State1#st.callgraph,
      NotFixpoint2 = [lookup_name(F, Callgraph) || F <- NotFixpoint1],
      {ModulePostorder, ModCallgraph} =
	dialyzer_callgraph:module_postorder_from_funs(NotFixpoint2, Callgraph),
      ModState = State1#st{callgraph = ModCallgraph},
      case refine_succ_typings(ModulePostorder, ModState) of
	{fixpoint, State2} ->
	  State2;
	{not_fixpoint, NotFixpoint3, State2} ->
	  Callgraph1 = State2#st.callgraph,
	  %% Need to reset the callgraph.
	  NotFixpoint4 = [lookup_name(F, Callgraph1) || F <- NotFixpoint3],
	  {NewSCCs, Callgraph2} =
	    dialyzer_callgraph:reset_from_funs(NotFixpoint4, Callgraph1),
	  NewState = State2#st{callgraph = Callgraph2},
	  get_refined_success_typings(NewSCCs, NewState)
      end
  end.

-type doc_plt() :: 'undefined' | dialyzer_plt:plt().
-spec get_warnings(dialyzer_callgraph:callgraph(), dialyzer_plt:plt(),
		   doc_plt(), dialyzer_codeserver:codeserver(), set(),
		   pid()) ->
	 {[dial_warning()], dialyzer_plt:plt(), doc_plt()}.

get_warnings(Callgraph, Plt, DocPlt, Codeserver, NoWarnUnused, Parent) ->
  InitState =
    init_state_and_get_success_typings(Callgraph, Plt, Codeserver, Parent),
  NewState =
    InitState#st{no_warn_unused = NoWarnUnused,
		 plt = dialyzer_plt:restore_full_plt(InitState#st.plt, Plt)},
  Mods = dialyzer_callgraph:modules(NewState#st.callgraph),
  CWarns = dialyzer_contracts:get_invalid_contract_warnings(Mods, Codeserver,
							    NewState#st.plt),
  get_warnings_from_modules(Mods, NewState, DocPlt, CWarns).

get_warnings_from_modules([M|Ms], State, DocPlt, Acc) when is_atom(M) ->
  send_log(State#st.parent, io_lib:format("Getting warnings for ~w\n", [M])),
  #st{callgraph = Callgraph, codeserver = Codeserver,
      no_warn_unused = NoWarnUnused, plt = Plt} = State,
  ModCode = dialyzer_codeserver:lookup_mod_code(M, Codeserver),
  Records = dialyzer_codeserver:lookup_mod_records(M, Codeserver),
  Contracts = dialyzer_codeserver:lookup_mod_contracts(M, Codeserver),
  AllFuns = collect_fun_info([ModCode]),
  %% Check if there are contracts for functions that do not exist
  Warnings1 = 
    dialyzer_contracts:contracts_without_fun(Contracts, AllFuns, Callgraph),
  {RawWarnings2, FunTypes} =
    dialyzer_dataflow:get_warnings(ModCode, Plt, Callgraph,
				   Records, NoWarnUnused),
  {NewAcc, Warnings2} = postprocess_dataflow_warns(RawWarnings2, State, Acc),
  Attrs = cerl:module_attrs(ModCode),
  Warnings3 = dialyzer_behaviours:check_callbacks(M, Attrs, Plt, Codeserver),
  NewDocPlt = insert_into_doc_plt(FunTypes, Callgraph, DocPlt),
  get_warnings_from_modules(Ms, State, NewDocPlt,
			    [Warnings1, Warnings2, Warnings3|NewAcc]);
get_warnings_from_modules([], #st{plt = Plt}, DocPlt, Acc) ->
  {lists:flatten(Acc), Plt, DocPlt}.

postprocess_dataflow_warns(RawWarnings, State, WarnAcc) ->
  postprocess_dataflow_warns(RawWarnings, State, WarnAcc, []).

postprocess_dataflow_warns([], _State, WAcc, Acc) ->
  {WAcc, lists:reverse(Acc)};
postprocess_dataflow_warns([{?WARN_CONTRACT_RANGE, {CallF, CallL}, Msg}|Rest],
			   #st{codeserver = Codeserver} = State, WAcc, Acc) ->
  {contract_range, [Contract, M, F, A, ArgStrings, CRet]} = Msg,
  case dialyzer_codeserver:lookup_mfa_contract({M,F,A}, Codeserver) of
    {ok, {{ContrF, _ContrL} = FileLine, _C}} ->
      case CallF =:= ContrF of
	true ->
	  NewMsg = {contract_range, [Contract, M, F, ArgStrings, CallL, CRet]},
	  W = {?WARN_CONTRACT_RANGE, FileLine, NewMsg},
	  Filter =
	    fun({?WARN_CONTRACT_TYPES, FL, _}) when FL =:= FileLine -> false;
	       (_) -> true
	    end,
	  FilterWAcc = lists:filter(Filter, WAcc),
	  postprocess_dataflow_warns(Rest, State, FilterWAcc, [W|Acc]);
	false ->
	  postprocess_dataflow_warns(Rest, State, WAcc, Acc)
      end;
    error ->
      %% The contract is not in a module that is currently under analysis.
      %% We display the warning in the file/line of the call.
      NewMsg = {contract_range, [Contract, M, F, ArgStrings, CallL, CRet]},
      W = {?WARN_CONTRACT_RANGE, {CallF, CallL}, NewMsg},
      postprocess_dataflow_warns(Rest, State, WAcc, [W|Acc])
  end;
postprocess_dataflow_warns([W|Rest], State, Wacc, Acc) ->
  postprocess_dataflow_warns(Rest, State, Wacc, [W|Acc]).
  
refine_succ_typings(ModulePostorder, #st{codeserver = Codeserver,
					 callgraph = Callgraph,
					 plt = Plt} = State) ->
  ?debug("Module postorder: ~p\n", [ModulePostorder]),
  Servers = {Codeserver, Callgraph, Plt},
  Coordinator = dialyzer_coordinator:start(dataflow, Servers),
  refine_succ_typings(ModulePostorder, State, Coordinator).

refine_succ_typings([M|Rest], State, Coordinator) ->
  Msg = io_lib:format("Dataflow of module: ~w\n", [M]),
  send_log(State#st.parent, Msg),
  ?debug("~s\n", [Msg]),
  dialyzer_coordinator:scc_spawn(M, Coordinator),
  refine_succ_typings(Rest, State, Coordinator);
refine_succ_typings([], State, Coordinator) ->
  dialyzer_coordinator:all_spawned(Coordinator),
  NotFixpoint = dialyzer_coordinator:receive_not_fixpoint(),
  ?debug("==================== Dataflow done ====================\n\n", []),
  case NotFixpoint =:= [] of
    true -> {fixpoint, State};
    false -> {not_fixpoint, NotFixpoint, State}
  end.

-type servers()         :: term().
-type scc_data()        :: term().
-type scc_refine_data() :: term().
-type scc()             :: [mfa_or_funlbl()] | [module()].

-spec find_depends_on(scc(), servers()) -> [scc()].

find_depends_on(SCC, {_Codeserver, Callgraph, _Plt}) ->
  dialyzer_callgraph:get_depends_on(SCC, Callgraph).

-spec find_required_by(scc(), servers()) -> [scc()].

find_required_by(SCC, {_Codeserver, Callgraph, _Plt}) ->
  dialyzer_callgraph:get_required_by(SCC, Callgraph).

-spec collect_refine_scc_data(module(), servers()) -> scc_refine_data().

collect_refine_scc_data(M, {CodeServer, Callgraph, PLT}) ->
  ModCode = dialyzer_codeserver:lookup_mod_code(M, CodeServer),
  AllFuns = collect_fun_info([ModCode]),
  Records = dialyzer_codeserver:lookup_mod_records(M, CodeServer),
  FunTypes = get_fun_types_from_plt(AllFuns, Callgraph, PLT),
  {ModCode, PLT, Callgraph, Records, FunTypes}.

-spec refine_one_module(scc_refine_data()) ->
        {dialyzer_callgraph:callgraph(), [label()]}. % ordset

refine_one_module({ModCode, PLT, Callgraph, Records, FunTypes}) ->
  NewFunTypes =
    dialyzer_dataflow:get_fun_types(ModCode, PLT, Callgraph, Records),
  case reached_fixpoint(FunTypes, NewFunTypes) of
    true ->
      ordsets:new();
    {false, NotFixpoint} ->
      ?debug("Not fixpoint\n", []),
      insert_into_plt(dict:from_list(NotFixpoint), Callgraph, PLT),
      ordsets:from_list([FunLbl || {FunLbl,_Type} <- NotFixpoint])
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
			    plt = Plt} = State) ->
  Servers = {Codeserver, dialyzer_callgraph:mini_callgraph(Callgraph), Plt},
  Coordinator = dialyzer_coordinator:start(typesig, Servers),
  find_succ_typings(SCCs, State, Coordinator).

find_succ_typings([SCC|Rest], #st{parent = Parent} = State, Coordinator) ->
  Msg = io_lib:format("Typesig analysis for SCC: ~w\n", [format_scc(SCC)]),
  ?debug("~s", [Msg]),
  send_log(Parent, Msg),
  dialyzer_coordinator:scc_spawn(SCC, Coordinator),
  find_succ_typings(Rest, State, Coordinator);
find_succ_typings([], State, Coordinator) ->
  dialyzer_coordinator:all_spawned(Coordinator),
  NotFixpoint = dialyzer_coordinator:receive_not_fixpoint(),
  ?debug("==================== Typesig done ====================\n\n", []),
  case NotFixpoint =:= [] of
    true -> {fixpoint, State};
    false -> {not_fixpoint, NotFixpoint, State}
  end.

-spec collect_scc_data(scc(), servers()) -> scc_data().

collect_scc_data(SCC, {Codeserver, Callgraph, Plt}) ->
  SCC_Info = [{MFA, 
	       dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
	       dialyzer_codeserver:lookup_mod_records(M, Codeserver)}
	      || {M, _, _} = MFA <- SCC],
  Contracts1 = [{MFA, dialyzer_codeserver:lookup_mfa_contract(MFA, Codeserver)}
		|| {_, _, _} = MFA <- SCC],
  Contracts2 = [{MFA, Contract} || {MFA, {ok, Contract}} <- Contracts1],
  Contracts3 = orddict:from_list(Contracts2),
  NextLabel = dialyzer_codeserver:get_next_core_label(Codeserver),
  AllFuns = collect_fun_info([Fun || {_MFA, {_Var, Fun}, _Rec} <- SCC_Info]),
  PropTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),
  {SCC_Info, Contracts3, NextLabel, AllFuns, PropTypes, Callgraph, Plt}.

-spec find_succ_types_for_scc(scc_data()) -> [mfa_or_funlbl()].

find_succ_types_for_scc({SCC_Info, Contracts, NextLabel, AllFuns,
			 PropTypes, Callgraph, Plt}) ->
  %% Assume that the PLT contains the current propagated types
  FunTypes = dialyzer_typesig:analyze_scc(SCC_Info, NextLabel, 
					  Callgraph, Plt, PropTypes),
  AllFunSet = sets:from_list([X || {X, _} <- AllFuns]),
  FilteredFunTypes = dict:filter(fun(X, _) ->
				      sets:is_element(X, AllFunSet) 
				  end, FunTypes),
  %% Check contracts
  PltContracts = dialyzer_contracts:check_contracts(Contracts, Callgraph, 
						    FilteredFunTypes),
  ContractFixpoint =
    lists:all(fun({MFA, _C}) ->
		  %% Check the non-deleted PLT
		  case dialyzer_plt:lookup_contract(Plt, MFA) of
		    none -> false;
		    {value, _} -> true
		  end
	      end, PltContracts),
  insert_into_plt(FilteredFunTypes, Callgraph, Plt),
  dialyzer_plt:insert_contract_list(Plt, PltContracts),
  case (ContractFixpoint andalso 
	reached_fixpoint_strict(PropTypes, FilteredFunTypes)) of
    true -> [];
    false ->
      ?debug("Not fixpoint for: ~w\n", [AllFuns]),
      ordsets:from_list([Fun || {Fun, _Arity} <- AllFuns])
  end.

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
-else.
debug_pp_succ_typings(_) ->
  ok.
-endif.

lookup_name(F, CG) ->
  case dialyzer_callgraph:lookup_name(F, CG) of
    error -> F;
    {ok, Name} -> Name
  end.

send_log(none, _Msg) ->
  ok;
send_log(Parent, Msg) ->
  Parent ! {self(), log, lists:flatten(Msg)},
  ok.

format_scc(SCC) ->
  [MFA || {_M, _F, _A} = MFA <- SCC].

%% ============================================================================
%%
%%  Debug interface.
%%
%% ============================================================================

-spec doit(atom() | file:filename()) -> 'ok'.

doit(Module) ->
  {ok, AbstrCode} = dialyzer_utils:get_abstract_code_from_src(Module),
  {ok, Code} = dialyzer_utils:get_core_from_abstract_code(AbstrCode),
  {ok, Records} = dialyzer_utils:get_record_and_type_info(AbstrCode),
  %% contract typing info in dictionary format
  {ok, Contracts, _Callbacks} =
    dialyzer_utils:get_spec_info(cerl:concrete(cerl:module_name(Code)),
                                 AbstrCode, Records),
  Sigs0 = get_top_level_signatures(Code, Records, Contracts),
  M = if is_atom(Module) ->  
	  list_to_atom(filename:basename(atom_to_list(Module)));
	 is_list(Module) -> 
	  list_to_atom(filename:basename(Module))
      end,
  Sigs1 = [{{M, F, A}, Type} || {{F, A}, Type} <- Sigs0],
  Sigs = ordsets:from_list(Sigs1),
  io:format("==================== Final result ====================\n\n", []),
  pp_signatures(Sigs, Records),
  ok.

-spec get_top_level_signatures(cerl:c_module(), dict(), dict()) ->
	 [{{atom(), arity()}, erl_types:erl_type()}].

get_top_level_signatures(Code, Records, Contracts) ->
  Tree = cerl:from_records(Code),
  {LabeledTree, NextLabel} = cerl_trees:label(Tree),
  Plt = get_def_plt(),
  ModuleName = cerl:atom_val(cerl:module_name(LabeledTree)),
  Plt1 = dialyzer_plt:delete_module(Plt, ModuleName),
  Plt2 = analyze_module(LabeledTree, NextLabel, Plt1, Records, Contracts),
  M = cerl:concrete(cerl:module_name(Tree)),
  Functions = [{M, cerl:fname_id(V), cerl:fname_arity(V)} 
	       || {V, _F} <- cerl:module_defs(LabeledTree)],
  %% First contracts check
  AllContracts = dict:fetch_keys(Contracts),
  ErrorContracts = AllContracts -- Functions,  
  lists:foreach(fun(C) -> 
		    io:format("Contract for non-existing function: ~w\n",[C])
		end, ErrorContracts),
  Types = [{MFA, dialyzer_plt:lookup(Plt2, MFA)} || MFA <- Functions],
  Sigs = [{{F, A}, erl_types:t_fun(ArgT, RetT)} 
	  || {{_M, F, A}, {value, {RetT, ArgT}}} <- Types],
  ordsets:from_list(Sigs).

get_def_plt() ->
  try 
    dialyzer_plt:from_file(dialyzer_plt:get_default_plt())
  catch
    error:no_such_file -> dialyzer_plt:new();
    throw:{dialyzer_error, _} -> dialyzer_plt:new()
  end.

pp_signatures([{{_, module_info, 0}, _}|Left], Records) -> 
  pp_signatures(Left, Records);
pp_signatures([{{_, module_info, 1}, _}|Left], Records) -> 
  pp_signatures(Left, Records);
pp_signatures([{{M, F, _A}, Type}|Left], Records) ->
  TypeString =
    case cerl:is_literal(Type) of
%% Commented out so that dialyzer does not complain
%%      false -> 
%%        "fun(" ++ String = erl_types:t_to_string(Type, Records),
%%        string:substr(String, 1, length(String)-1);
      true ->
	io_lib:format("~w", [cerl:concrete(Type)])
    end,
  io:format("~w:~w~s\n", [M, F, TypeString]),
  pp_signatures(Left, Records);
pp_signatures([], _Records) ->
  ok.

-ifdef(DEBUG_PP).
debug_pp(Tree, _Map) -> 
  Tree1 = strip_annotations(Tree),
  io:put_chars(cerl_prettypr:format(Tree1)),
  io:nl().  

strip_annotations(Tree) ->
  cerl_trees:map(fun(T) ->
		     case cerl:is_literal(T) orelse cerl:is_c_values(T) of
		       true -> cerl:set_ann(T, []);
		       false ->
			 Label = cerl_trees:get_label(T),
			 cerl:set_ann(T, [{'label', Label}])
		     end
		 end, Tree).
-else.
debug_pp(_Tree, _Map) ->
  ok.
-endif. % DEBUG_PP

%%
%% Analysis of a single module
%%
analyze_module(LabeledTree, NextLbl, Plt, Records, Contracts) ->
  debug_pp(LabeledTree, dict:new()),
  CallGraph1 = dialyzer_callgraph:new(),
  CallGraph2 = dialyzer_callgraph:scan_core_tree(LabeledTree, CallGraph1),
  {CallGraph3, _Ext} = dialyzer_callgraph:remove_external(CallGraph2),
  CallGraph4 = dialyzer_callgraph:finalize(CallGraph3),
  CodeServer1 = dialyzer_codeserver:new(),
  Mod = cerl:concrete(cerl:module_name(LabeledTree)),
  CodeServer2 = dialyzer_codeserver:insert(Mod, LabeledTree, CodeServer1),
  CodeServer3 = dialyzer_codeserver:set_next_core_label(NextLbl, CodeServer2),
  CodeServer4 = dialyzer_codeserver:store_records(Mod, Records, CodeServer3),
  CodeServer5 = dialyzer_codeserver:store_contracts(Mod, Contracts, CodeServer4),
  Res = analyze_callgraph(CallGraph4, Plt, CodeServer5),
  dialyzer_callgraph:delete(CallGraph4),
  dialyzer_codeserver:delete(CodeServer5),
  Res.
