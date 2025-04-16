%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2011-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File        : dialyzer_behaviours.erl
%%% Authors     : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Tools for analyzing proper behaviour usage.
%%%
%%% Created     : 28 Oct 2009 by Stavros Aronis <aronisstav@gmail.com>
%%%-------------------------------------------------------------------

-module(dialyzer_behaviours).
-moduledoc false.

-export([check_callbacks/5, get_behaviours/1]).

-export_type([behaviour/0]).

%%--------------------------------------------------------------------

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-type behaviour() :: atom().

-type rectab() :: erl_types:type_table().

-record(state, {plt        :: dialyzer_plt:plt(),
		codeserver :: dialyzer_codeserver:codeserver(),
		filename   :: file:filename(),
		behlines   :: [{behaviour(), file_location()}],
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

-spec get_behaviours([{cerl:cerl(), cerl:cerl()}]) -> {[behaviour()], [{behaviour(), term()}]}.

get_behaviours(Attrs) ->
  BehaviourListsAndLocation =
    [{cerl:concrete(L2), hd(cerl:get_ann(L2))} ||
      {L1, L2} <- Attrs, cerl:is_literal(L1),
      cerl:is_literal(L2), cerl:concrete(L1) =:= 'behaviour' orelse
	cerl:concrete(L1) =:= 'behavior'],
  Behaviours = lists:append([Behs || {Behs,_} <- BehaviourListsAndLocation]),
  BehLocations = [{B,L} || {L1,L} <- BehaviourListsAndLocation, B <- L1],
  {Behaviours, BehLocations}.

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
		    #state{plt = Plt, codeserver = Codeserver} = State,
                    Acc0) ->
  {{Behaviour, Function, Arity},
   {{_BehFile, _BehLocation}, Callback, Xtra}} = Cb,
  CbMFA = {Module, Function, Arity},
  Acc1 = case dialyzer_plt:lookup(Plt, CbMFA) of
           none ->
             case lists:member(optional_callback, Xtra) of
               true -> Acc0;
               false -> [{callback_missing, [Behaviour, Function, Arity]}|Acc0]
             end;
           {value, RetArgTypes} ->
             case dialyzer_codeserver:is_exported(CbMFA, Codeserver) of
               true ->
                 check_callback(RetArgTypes, CbMFA, Behaviour, Callback, State, Acc0);
               false ->
                 case lists:member(optional_callback, Xtra) of
                   true -> Acc0;
                   false -> [{callback_not_exported, [Behaviour, Function, Arity]}|Acc0]
                 end
             end
         end,
  check_all_callbacks(Module, Behaviour, Rest, State, Acc1).

check_callback(RetArgTypes, CbMFA, Behaviour, Callback,
               #state{plt = _Plt, codeserver = Codeserver,
                      records = Records}, Acc0) ->
  {_Module, Function, Arity} = CbMFA,
  CbReturnType = dialyzer_contracts:get_contract_return(Callback),
  CbArgTypes = dialyzer_contracts:get_contract_args(Callback),
  {ReturnType, ArgTypes} = RetArgTypes,
  Acc1 =
      % Allow none() as the return type to be backwards compatible
      % with logic that allows crashes in callbacks
      case (not erl_types:t_is_none(ReturnType)) andalso erl_types:t_is_none(erl_types:t_inf(ReturnType, CbReturnType)) of
        false ->
          Acc0;
        true ->
          [{callback_type_mismatch,
            [Behaviour, Function, Arity,
             erl_types:t_to_string(ReturnType, Records),
             erl_types:t_to_string(CbReturnType, Records)]}|Acc0]
      end,
  Acc2 = case erl_types:any_none(erl_types:t_inf_lists(ArgTypes, CbArgTypes)) of
           false -> Acc1;
           true ->
             find_mismatching_args(type, ArgTypes, CbArgTypes, Behaviour,
                                   Function, Arity, Records, 1, Acc1)
         end,
  case dialyzer_codeserver:lookup_mfa_contract(CbMFA, Codeserver) of
    error ->
      Acc2;
    {ok, {{File, Location}, Contract, _Xtra}} ->
      SpecReturnType0 = dialyzer_contracts:get_contract_return(Contract),
      SpecArgTypes0 = dialyzer_contracts:get_contract_args(Contract),
      SpecReturnType = erl_types:subst_all_vars_to_any(SpecReturnType0),
      SpecArgTypes =
        [erl_types:subst_all_vars_to_any(ArgT0) || ArgT0 <- SpecArgTypes0],
      Acc3 =
        % Allow none() as the return type to be backwards compatible
        % with logic that allows crashes in callbacks
        case (not erl_types:t_is_none(SpecReturnType)) andalso erl_types:t_is_none(erl_types:t_inf(SpecReturnType, CbReturnType)) of
          false ->
            Acc2;
          true ->
            ExtraType = erl_types:t_subtract(SpecReturnType, CbReturnType),
            [{callback_spec_type_mismatch,
              [File, Location, Behaviour, Function, Arity,
               erl_types:t_to_string(ExtraType, Records),
               erl_types:t_to_string(CbReturnType, Records)]}|Acc2]
        end,
      case erl_types:any_none(erl_types:t_inf_lists(SpecArgTypes, CbArgTypes)) of
        false -> Acc3;
        true ->
          find_mismatching_args({spec, File, Location}, SpecArgTypes,
                                CbArgTypes, Behaviour, Function,
                                Arity, Records, 1, Acc3)
      end
  end.

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
	   {spec, File, Location} ->
	     {callback_spec_arg_type_mismatch, [File, Location | Info]}
	 end | Acc],
      find_mismatching_args(Kind, Rest, CbRest, Behaviour, Function,
			    Arity, Records, N+1, NewAcc)
  end.

add_tag_warning_info(Module, {Tag, [B|_R]} = Warn, State)
  when Tag =:= callback_missing;
       Tag =:= callback_info_missing ->
  {B, Location} = lists:keyfind(B, 1, State#state.behlines),
  Category =
    case Tag of
      callback_missing -> ?WARN_BEHAVIOUR;
      callback_info_missing -> ?WARN_UNDEFINED_CALLBACK
    end,
  {Category, {State#state.filename, Location, Module}, Warn};
add_tag_warning_info(Module, {Tag, [File, Location|R]}, _State)
  when Tag =:= callback_spec_type_mismatch;
       Tag =:= callback_spec_arg_type_mismatch ->
  {?WARN_BEHAVIOUR, {File, Location, Module}, {Tag, R}};
add_tag_warning_info(Module, {_Tag, [_B, Fun, Arity|_R]} = Warn, State) ->
  {_A, FunCode} =
    dialyzer_codeserver:lookup_mfa_code({Module, Fun, Arity},
					State#state.codeserver),
  Anns = cerl:get_ann(FunCode),
  File = get_file(State#state.codeserver, Module, Anns),
  WarningInfo = {File, get_location(FunCode), {Module, Fun, Arity}},
  {?WARN_BEHAVIOUR, WarningInfo, Warn}.

get_location(Tree) ->
  dialyzer_utils:get_location(Tree, -1).

get_file(Codeserver, Module, [{file, FakeFile}|_]) ->
  dialyzer_codeserver:translate_fake_file(Codeserver, Module, FakeFile);
get_file(Codeserver, Module, [_|Tail]) ->
  get_file(Codeserver, Module, Tail).
