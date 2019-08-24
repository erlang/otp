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
%%% File        : dialyzer_behaviours.erl
%%% Authors     : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Tools for analyzing proper behaviour usage.
%%%
%%% Created     : 28 Oct 2009 by Stavros Aronis <aronisstav@gmail.com>
%%%-------------------------------------------------------------------

-module(dialyzer_behaviours).

-export([check_callbacks/5]).

-export_type([behaviour/0]).

%%--------------------------------------------------------------------

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-type behaviour() :: atom().

-type rectab() :: erl_types:type_table().

-record(state, {plt        :: dialyzer_plt:plt(),
		codeserver :: dialyzer_codeserver:codeserver(),
		filename   :: file:filename(),
		behlines   :: [{behaviour(), non_neg_integer()}],
		records    :: rectab()}).

%%--------------------------------------------------------------------

-spec check_callbacks(module(), [{cerl:cerl(), cerl:cerl()}], rectab(),
		      dialyzer_plt:plt(),
		      dialyzer_codeserver:codeserver()) -> [raw_warning()].

check_callbacks(Module, Attrs, Records, Plt, Codeserver) ->
  {Behaviours, BehLines} = get_behaviours(Attrs),
  case Behaviours of
    [] -> [];
    _ ->
      MFA = {Module,module_info,0},
      {_Var,Code} = dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
      File = get_file(Codeserver, Module, cerl:get_ann(Code)),
      State = #state{plt = Plt, filename = File, behlines = BehLines,
                     codeserver = Codeserver, records = Records},
      Warnings = get_warnings(Module, Behaviours, State),
      [add_tag_warning_info(Module, W, State) || W <- Warnings]
  end.

%%--------------------------------------------------------------------

get_behaviours(Attrs) ->
  BehaviourListsAndLine =
    [{cerl:concrete(L2), hd(cerl:get_ann(L2))} ||
      {L1, L2} <- Attrs, cerl:is_literal(L1),
      cerl:is_literal(L2), cerl:concrete(L1) =:= 'behaviour' orelse
	cerl:concrete(L1) =:= 'behavior'],
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
    none -> [{callback_info_missing, [Behaviour]}|Acc];
    {value, Callbacks} ->
      check_all_callbacks(Module, Behaviour, Callbacks, State, Acc)
  end.

check_all_callbacks(_Module, _Behaviour, [], _State, Acc) ->
  Acc;
check_all_callbacks(Module, Behaviour, [Cb|Rest],
		    #state{plt = Plt, codeserver = Codeserver,
			   records = Records} = State, Acc) ->
  {{Behaviour, Function, Arity},
   {{_BehFile, _BehLine}, Callback, Xtra}} = Cb,
  CbMFA = {Module, Function, Arity},
  CbReturnType = dialyzer_contracts:get_contract_return(Callback),
  CbArgTypes = dialyzer_contracts:get_contract_args(Callback),
  Acc0 = Acc,
  Acc1 = 
    case dialyzer_plt:lookup(Plt, CbMFA) of
      'none' ->
        case lists:member(optional_callback, Xtra) of
          true -> Acc0;
          false -> [{callback_missing, [Behaviour, Function, Arity]}|Acc0]
        end;
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
	case erl_types:any_none(erl_types:t_inf_lists(ArgTypes, CbArgTypes)) of
	  false -> Acc01;
	  true ->
	    find_mismatching_args(type, ArgTypes, CbArgTypes, Behaviour,
				  Function, Arity, Records, 1, Acc01)
	end
    end,
  Acc2 =
    case dialyzer_codeserver:lookup_mfa_contract(CbMFA, Codeserver) of
      'error' -> Acc1;
      {ok, {{File, Line}, Contract, _Xtra}} ->
	Acc10 = Acc1,
	SpecReturnType0 = dialyzer_contracts:get_contract_return(Contract),
	SpecArgTypes0 = dialyzer_contracts:get_contract_args(Contract),
	SpecReturnType = erl_types:subst_all_vars_to_any(SpecReturnType0),
	SpecArgTypes =
	  [erl_types:subst_all_vars_to_any(ArgT0) || ArgT0 <- SpecArgTypes0],
	Acc11 =
	  case erl_types:t_is_subtype(SpecReturnType, CbReturnType) of
	    true -> Acc10;
	    false ->
	      ExtraType = erl_types:t_subtract(SpecReturnType, CbReturnType),
	      [{callback_spec_type_mismatch,
		[File, Line, Behaviour, Function, Arity,
		 erl_types:t_to_string(ExtraType, Records),
		 erl_types:t_to_string(CbReturnType, Records)]}|Acc10]
	  end,
	case erl_types:any_none(
	       erl_types:t_inf_lists(SpecArgTypes, CbArgTypes)) of
	  false -> Acc11;
	  true ->
	    find_mismatching_args({spec, File, Line}, SpecArgTypes,
				  CbArgTypes, Behaviour, Function,
				  Arity, Records, 1, Acc11)
	end
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

add_tag_warning_info(Module, {Tag, [B|_R]} = Warn, State)
  when Tag =:= callback_missing;
       Tag =:= callback_info_missing ->
  {B, Line} = lists:keyfind(B, 1, State#state.behlines),
  Category =
    case Tag of
      callback_missing -> ?WARN_BEHAVIOUR;
      callback_info_missing -> ?WARN_UNDEFINED_CALLBACK
    end,
  {Category, {State#state.filename, Line, Module}, Warn};
add_tag_warning_info(Module, {Tag, [File, Line|R]}, _State)
  when Tag =:= callback_spec_type_mismatch;
       Tag =:= callback_spec_arg_type_mismatch ->
  {?WARN_BEHAVIOUR, {File, Line, Module}, {Tag, R}};
add_tag_warning_info(Module, {_Tag, [_B, Fun, Arity|_R]} = Warn, State) ->
  {_A, FunCode} =
    dialyzer_codeserver:lookup_mfa_code({Module, Fun, Arity},
					State#state.codeserver),
  Anns = cerl:get_ann(FunCode),
  File = get_file(State#state.codeserver, Module, Anns),
  WarningInfo = {File, get_line(Anns), {Module, Fun, Arity}},
  {?WARN_BEHAVIOUR, WarningInfo, Warn}.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file(Codeserver, Module, [{file, FakeFile}|_]) ->
  dialyzer_codeserver:translate_fake_file(Codeserver, Module, FakeFile);
get_file(Codeserver, Module, [_|Tail]) ->
  get_file(Codeserver, Module, Tail).
