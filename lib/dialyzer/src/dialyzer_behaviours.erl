%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%% File        : dialyzer_behaviours.erl
%%% Authors     : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Tools for analyzing proper behaviour usage.
%%%
%%% Created     : 28 Oct 2009 by Stavros Aronis <aronisstav@gmail.com>
%%%-------------------------------------------------------------------
%%% NOTE: This module is currently experimental -- do NOT rely on it!
%%%-------------------------------------------------------------------

-module(dialyzer_behaviours).

-export([check_callbacks/4, get_behaviour_apis/1,
	 translate_behaviour_api_call/5, translatable_behaviours/1,
	 translate_callgraph/3]).

-export_type([behaviour/0, behaviour_api_dict/0]).

%%--------------------------------------------------------------------

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-type behaviour() :: atom().

-record(state, {plt        :: dialyzer_plt:plt(),
		codeserver :: dialyzer_codeserver:codeserver(),
		filename   :: file:filename(),
		behlines   :: [{behaviour(), non_neg_integer()}]}).

%%--------------------------------------------------------------------

-spec check_callbacks(module(), [{cerl:cerl(), cerl:cerl()}],
		      dialyzer_plt:plt(),
		      dialyzer_codeserver:codeserver()) -> [dial_warning()].

check_callbacks(Module, Attrs, Plt, Codeserver) ->
  {Behaviours, BehLines} = get_behaviours(Attrs),
  case Behaviours of
    [] -> [];
    _ ->
      MFA = {Module,module_info,0},
      {_Var,Code} = dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
      File = get_file(cerl:get_ann(Code)),
      State = #state{plt = Plt, filename = File, behlines = BehLines,
		     codeserver = Codeserver},
      Warnings = get_warnings(Module, Behaviours, State),
      [add_tag_file_line(Module, W, State) || W <- Warnings]
  end.

%%--------------------------------------------------------------------

get_behaviours(Attrs) ->
  BehaviourListsAndLine = [{cerl:concrete(L2), hd(cerl:get_ann(L2))} ||
		  {L1, L2} <- Attrs, cerl:is_literal(L1),
		  cerl:is_literal(L2), cerl:concrete(L1) =:= 'behaviour'],
  Behaviours = lists:append([Behs || {Behs,_} <- BehaviourListsAndLine]),
  BehLines = [{B,L} || {L1,L} <- BehaviourListsAndLine, B <- L1],
  {Behaviours, BehLines}.

get_warnings(Module, Behaviours, State) ->
  get_warnings(Module, Behaviours, State, []).

get_warnings(_, [], _, Acc) ->
  Acc;
get_warnings(Module, [Behaviour|Rest], State, Acc) ->
  NewAcc = check_behaviour(Module, Behaviour, State, Acc),
  get_warnings(Module, Rest, State, NewAcc).

check_behaviour(Module, Behaviour, #state{plt = Plt} = State, Acc) ->
  case dialyzer_plt:lookup_callbacks(Plt, Behaviour) of
    [] -> [{callback_info_missing, [Behaviour]}|Acc];
    Callbacks -> check_all_callbacks(Module, Behaviour, Callbacks, State, Acc)
  end.

check_all_callbacks(_Module, _Behaviour, [], _State, Acc) ->
  Acc;
check_all_callbacks(Module, Behaviour, [Cb|Rest],
		    #state{plt = Plt, codeserver = Codeserver} = State, Acc) ->
  {{Behaviour, Function, Arity},
   {{_BehFile, _BehLine}, Callback}} = Cb,
  CbMFA = {Module, Function, Arity},
  CbReturnType = dialyzer_contracts:get_contract_return(Callback),
  CbArgTypes = dialyzer_contracts:get_contract_args(Callback),
  Records =
    case dict:find(Module, dialyzer_codeserver:get_records(Codeserver)) of
      {ok, V} -> V;
      error -> dict:new()
    end,
  Acc0 = Acc,
  Acc1 = 
    case dialyzer_plt:lookup(Plt, CbMFA) of
      'none' -> [{callback_missing, [Behaviour, Function, Arity]}|Acc0];
      {'value', RetArgTypes} ->
	Acc00 = Acc0,
	{ReturnType, ArgTypes} = RetArgTypes,
	Acc01 =
	  case erl_types:t_is_subtype(ReturnType, CbReturnType) of
	    true -> Acc00;
	    false ->
	      case erl_types:t_is_none(
		     erl_types:t_inf(ReturnType, CbReturnType)) of
		false -> Acc00;
		true ->
		  [{callback_type_mismatch,
		    [Behaviour, Function, Arity,
		     erl_types:t_to_string(ReturnType, Records),
		     erl_types:t_to_string(CbReturnType, Records)]}|Acc00]
	      end
	  end,
	Acc02 =
	  case erl_types:any_none(
		 erl_types:t_inf_lists(ArgTypes, CbArgTypes)) of
	    false -> Acc01;
	    true ->
	      find_mismatching_args(type, ArgTypes, CbArgTypes, Behaviour,
				    Function, Arity, Records, 1, Acc01)
	  end,
	Acc02
    end,
  Acc2 =
    case dialyzer_codeserver:lookup_mfa_contract(CbMFA, Codeserver) of
      'error' -> Acc1;
      {ok, {{File, Line}, Contract}} ->
	Acc10 = Acc1,
	SpecReturnType = dialyzer_contracts:get_contract_return(Contract),
	SpecArgTypes = dialyzer_contracts:get_contract_args(Contract),
	Acc11 =
	  case erl_types:t_is_subtype(SpecReturnType, CbReturnType) of
	    true -> Acc10;
	    false -> [{callback_spec_type_mismatch,
		       [File, Line, Behaviour, Function, Arity,
			erl_types:t_to_string(SpecReturnType, Records),
			erl_types:t_to_string(CbReturnType, Records)]}|Acc10]
	  end,
	Acc12 =
	  case erl_types:any_none(
		 erl_types:t_inf_lists(SpecArgTypes, CbArgTypes)) of
	    false -> Acc11;
	    true ->
	      find_mismatching_args({spec, File, Line}, SpecArgTypes,
				    CbArgTypes, Behaviour, Function,
				    Arity, Records, 1, Acc11)
	  end,
	Acc12
    end,
  NewAcc = Acc2,
  check_all_callbacks(Module, Behaviour, Rest, State, NewAcc).

find_mismatching_args(_, [], [], _Beh, _Function, _Arity, _Records, _N, Acc) ->
  Acc;
find_mismatching_args(Kind, [Type|Rest], [CbType|CbRest], Behaviour,
		      Function, Arity, Records, N, Acc) ->
  case erl_types:t_is_none(erl_types:t_inf(Type, CbType)) of
    false ->
      find_mismatching_args(Kind, Rest, CbRest, Behaviour, Function,
			    Arity, Records, N+1, Acc);
    true ->
      Info =
	[Behaviour, Function, Arity, N,
	 erl_types:t_to_string(Type, Records),
	 erl_types:t_to_string(CbType, Records)],
      NewAcc =
	[case Kind of
	   type -> {callback_arg_type_mismatch, Info};
	   {spec, File, Line} ->
	     {callback_spec_arg_type_mismatch, [File, Line | Info]}
	 end | Acc],
      find_mismatching_args(Kind, Rest, CbRest, Behaviour, Function,
			    Arity, Records, N+1, NewAcc)
  end.

add_tag_file_line(_Module, {Tag, [B|_R]} = Warn, State)
  when Tag =:= callback_missing;
       Tag =:= callback_info_missing ->
  {B, Line} = lists:keyfind(B, 1, State#state.behlines),
  Category =
    case Tag of
      callback_missing -> ?WARN_BEHAVIOUR;
      callback_info_missing -> ?WARN_UNDEFINED_CALLBACK
    end,
  {Category, {State#state.filename, Line}, Warn};
add_tag_file_line(_Module, {Tag, [File, Line|R]}, _State)
  when Tag =:= callback_spec_type_mismatch;
       Tag =:= callback_spec_arg_type_mismatch ->
  {?WARN_BEHAVIOUR, {File, Line}, {Tag, R}};
add_tag_file_line(Module, {_Tag, [_B, Fun, Arity|_R]} = Warn, State) ->
  {_A, FunCode} =
    dialyzer_codeserver:lookup_mfa_code({Module, Fun, Arity},
					State#state.codeserver),
  Anns = cerl:get_ann(FunCode),
  FileLine = {get_file(Anns), get_line(Anns)},
  {?WARN_BEHAVIOUR, FileLine, Warn}.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([{file, File}|_]) -> File;
get_file([_|Tail]) -> get_file(Tail).

%%-----------------------------------------------------------------------------

-spec translatable_behaviours(cerl:c_module()) -> behaviour_api_dict().

translatable_behaviours(Tree) ->
  Attrs = cerl:module_attrs(Tree),
  {Behaviours, _BehLines} = get_behaviours(Attrs),
  [{B, Calls} || B <- Behaviours, (Calls = behaviour_api_calls(B)) =/= []].

-spec get_behaviour_apis([behaviour()]) -> [mfa()].

get_behaviour_apis(Behaviours) ->
  get_behaviour_apis(Behaviours, []).

-spec translate_behaviour_api_call(dialyzer_races:mfa_or_funlbl(),
				   [erl_types:erl_type()],
				   [dialyzer_races:core_vars()],
				   module(),
				   behaviour_api_dict()) ->
				      {dialyzer_races:mfa_or_funlbl(),
				       [erl_types:erl_type()],
				       [dialyzer_races:core_vars()]}
					| 'plain_call'.

translate_behaviour_api_call(_Fun, _ArgTypes, _Args, _Module, []) ->
  plain_call;
translate_behaviour_api_call({Module, Fun, Arity}, ArgTypes, Args,
			     CallbackModule, BehApiInfo) ->
  case lists:keyfind(Module, 1, BehApiInfo) of
    false -> plain_call;
    {Module, Calls} ->
      case lists:keyfind({Fun, Arity}, 1, Calls) of
	false -> plain_call;
	{{Fun, Arity}, {CFun, CArity, COrder}} ->
	  {{CallbackModule, CFun, CArity},
	   [nth_or_0(N, ArgTypes, erl_types:t_any()) || N <-COrder],
	   [nth_or_0(N, Args, bypassed) || N <-COrder]}
      end
  end;
translate_behaviour_api_call(_Fun, _ArgTypes, _Args, _Module, _BehApiInfo) ->
  plain_call.

-spec translate_callgraph(behaviour_api_dict(), atom(),
			  dialyzer_callgraph:callgraph()) ->
			     dialyzer_callgraph:callgraph().

translate_callgraph([{Behaviour,_}|Behaviours], Module, Callgraph) ->
  UsedCalls = [Call || {_From, {M, _F, _A}} = Call <-
			 dialyzer_callgraph:get_behaviour_api_calls(Callgraph),
		       M =:= Behaviour],
  Calls = [{{Behaviour, API, Arity}, Callback} ||
	    {{API, Arity}, Callback} <- behaviour_api_calls(Behaviour)],
  DirectCalls = [{From, {Module, Fun, Arity}} ||
		  {From, To} <- UsedCalls,{API, {Fun, Arity, _Ord}} <- Calls,
		  To =:= API],
  NewCallgraph = dialyzer_callgraph:add_edges(DirectCalls, Callgraph),
  translate_callgraph(Behaviours, Module, NewCallgraph);
translate_callgraph([], _Module, Callgraph) ->
  Callgraph.

get_behaviour_apis([], Acc) ->
  Acc;
get_behaviour_apis([Behaviour | Rest], Acc) ->
  MFAs = [{Behaviour, Fun, Arity} ||
	   {{Fun, Arity}, _} <- behaviour_api_calls(Behaviour)],
  get_behaviour_apis(Rest, MFAs ++ Acc).

%------------------------------------------------------------------------------

nth_or_0(0, _List, Zero) ->
  Zero;
nth_or_0(N, List, _Zero) ->
  lists:nth(N, List).

%------------------------------------------------------------------------------

-type behaviour_api_dict()::[{behaviour(), behaviour_api_info()}].
-type behaviour_api_info()::[{original_fun(), replacement_fun()}].
-type original_fun()::{atom(), arity()}.
-type replacement_fun()::{atom(), arity(), arg_list()}.
-type arg_list()::[byte()].

-spec behaviour_api_calls(behaviour()) -> behaviour_api_info().

behaviour_api_calls(gen_server) ->
  [{{start_link, 3}, {init, 1, [2]}},
   {{start_link, 4}, {init, 1, [3]}},
   {{start, 3}, {init, 1, [2]}},
   {{start, 4}, {init, 1, [3]}},
   {{call, 2}, {handle_call, 3, [2, 0, 0]}},
   {{call, 3}, {handle_call, 3, [2, 0, 0]}},
   {{multi_call, 2}, {handle_call, 3, [2, 0, 0]}},
   {{multi_call, 3}, {handle_call, 3, [3, 0, 0]}},
   {{multi_call, 4}, {handle_call, 3, [3, 0, 0]}},
   {{cast, 2}, {handle_cast, 2, [2, 0]}},
   {{abcast, 2}, {handle_cast, 2, [2, 0]}},
   {{abcast, 3}, {handle_cast, 2, [3, 0]}}];
behaviour_api_calls(_Other) ->
  [].
