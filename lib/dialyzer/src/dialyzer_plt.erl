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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_plt.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : Interface to display information in the persistent
%%%               lookup tables stored in memory, and other commonality
%%%               between the various kinds of persisted PLT files.
%%%
%%% Created : 23 Jul 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_plt).
-moduledoc false.

-export([contains_mfa/2,
	 all_modules/1,
	 delete_list/2,
	 delete_module/2,
         get_module_types/2,
         get_exported_types/1,
	 insert_list/2,
	 insert_contract_list/2,
	 insert_callbacks/2,
	 insert_types/2,
         insert_exported_types/2,
	 lookup/2,
         is_contract/2,
	 lookup_contract/2,
	 lookup_callbacks/2,
	 lookup_module/2,
         merge_plts/1,
	 new/0,
         delete/1,
         get_all_types/1,
         get_all_contracts/1,
         get_all_callbacks/1,
         plt_kind/1
	]).

-export_type([plt/0]).

-include_lib("stdlib/include/ms_transform.hrl").

%%----------------------------------------------------------------------

%% The following are used for searching the PLT when using the GUI
%% (e.g. in show or search PLT contents). The user might be searching
%% with a partial specification, in which case the missing items
%% default to '_'
-type arity_patt() :: '_' | arity().
-type mfa_patt()   :: {module(), atom(), arity_patt()}.

%%----------------------------------------------------------------------

-include("dialyzer.hrl").

-type plt() :: #plt{}.

%%----------------------------------------------------------------------

-spec new() -> plt().

new() ->
  [ETSInfo, ETSContracts] =
    [ets:new(Name, [public]) ||
      Name <- [plt_info, plt_contracts]],
  [ETSTypes, ETSCallbacks, ETSExpTypes] =
    [ets:new(Name, [compressed, public]) ||
      Name <- [plt_types, plt_callbacks, plt_exported_types]],
  #plt{info = ETSInfo,
       types = ETSTypes,
       contracts = ETSContracts,
       callbacks = ETSCallbacks,
       exported_types = ETSExpTypes}.

-spec delete_module(plt(), atom()) -> plt().

delete_module(#plt{info = Info, types = Types,
		   contracts = Contracts,
		   callbacks = Callbacks,
                   exported_types = ExpTypes}, Mod) ->
  #plt{info = table_delete_module(Info, Mod),
       types = table_delete_module2(Types, Mod),
       contracts = table_delete_module(Contracts, Mod),
       callbacks = table_delete_module2(Callbacks, Mod),
       exported_types = table_delete_module1(ExpTypes, Mod)}.

-spec delete_list(plt(), [mfa() | integer()]) -> plt().

delete_list(#plt{info = Info,
                 contracts = Contracts}=Plt, List) ->
  Plt#plt{info = ets_table_delete_list(Info, List),
          contracts = ets_table_delete_list(Contracts, List)}.

-spec insert_contract_list(plt(), dialyzer_contracts:plt_contracts()) -> plt().

insert_contract_list(#plt{contracts = Contracts} = PLT, List) ->
  true = ets:insert(Contracts, List),
  PLT.

-spec insert_callbacks(plt(), dialyzer_codeserver:codeserver()) -> plt().

insert_callbacks(#plt{callbacks = Callbacks} = Plt, Codeserver) ->
  CallbacksList = dialyzer_codeserver:get_callbacks(Codeserver),
  CallbacksByModule =
    [{M, [Cb || {{M1,_,_},_} = Cb <- CallbacksList, M1 =:= M]} ||
      M <- lists:usort([M || {{M,_,_},_} <- CallbacksList])],
  true = ets:insert(Callbacks, CallbacksByModule),
  Plt.

-spec is_contract(plt(), mfa()) -> boolean().

is_contract(#plt{contracts = ETSContracts},
            {M, F, _} = MFA) when is_atom(M), is_atom(F) ->
  ets:member(ETSContracts, MFA).

-spec lookup_contract(plt(), mfa_patt()) -> 'none' | {'value', #contract{}}.

lookup_contract(#plt{contracts = ETSContracts},
		{M, F, _} = MFA) when is_atom(M), is_atom(F) ->
  ets_table_lookup(ETSContracts, MFA).

-spec lookup_callbacks(plt(), module()) ->
	 'none' | {'value', [{mfa(), dialyzer_contracts:file_contract()}]}.

lookup_callbacks(#plt{callbacks = ETSCallbacks}, Mod) when is_atom(Mod) ->
  ets_table_lookup(ETSCallbacks, Mod).

-type ret_args_types() :: {erl_types:erl_type(), [erl_types:erl_type()]}.

-spec insert_list(plt(), [{mfa() | integer(), ret_args_types()}]) -> plt().

insert_list(#plt{info = Info} = PLT, List) ->
  true = ets:insert(Info, List),
  PLT.

-spec lookup(plt(), integer() | mfa_patt()) ->
        'none' | {'value', ret_args_types()}.

lookup(Plt, {M, F, _} = MFA) when is_atom(M), is_atom(F) ->
  lookup_1(Plt, MFA);
lookup(Plt, Label) when is_integer(Label) ->
  lookup_1(Plt, Label).

lookup_1(#plt{info = Info}, MFAorLabel) ->
  ets_table_lookup(Info, MFAorLabel).

-spec insert_types(plt(), ets:tid()) -> plt().

insert_types(PLT, Records) ->
  ok = dialyzer_utils:ets_move(Records, PLT#plt.types),
  PLT.

-spec insert_exported_types(plt(), ets:tid()) -> plt().

insert_exported_types(PLT, ExpTypes) ->
  ok = dialyzer_utils:ets_move(ExpTypes, PLT#plt.exported_types),
  PLT.

-spec get_module_types(plt(), atom()) ->
                          'none' | {'value', erl_types:type_table()}.

get_module_types(#plt{types = Types}, M) when is_atom(M) ->
  ets_table_lookup(Types, M).

-spec get_exported_types(plt()) -> sets:set().

get_exported_types(#plt{exported_types = ETSExpTypes}) ->
  sets:from_list([E || {E} <- table_to_list(ETSExpTypes)]).

-type mfa_types() :: {mfa(), erl_types:erl_type(), [erl_types:erl_type()]}.

-spec lookup_module(plt(), atom()) -> 'none' | {'value', [mfa_types()]}.

lookup_module(#plt{info = Info}, M) when is_atom(M) ->
  table_lookup_module(Info, M).

-spec all_modules(plt()) -> sets:set().

all_modules(#plt{info = Info, contracts = Cs}) ->
  sets:union(table_all_modules(Info), table_all_modules(Cs)).

-spec contains_mfa(plt(), mfa()) -> boolean().

contains_mfa(#plt{info = Info, contracts = Contracts}, MFA) ->
  ets:member(Info, MFA) orelse ets:member(Contracts, MFA).

-spec merge_plts([plt()]) -> plt().

%% One of the PLTs of the list is augmented with the contents of the
%% other PLTs, and returned. The other PLTs are deleted.

merge_plts(List) ->
  {InfoList, TypesList, ExpTypesList, ContractsList, CallbacksList} =
    group_fields(List),
  #plt{info = table_merge(InfoList),
       types = table_merge(TypesList),
       exported_types = sets_merge(ExpTypesList),
       contracts = table_merge(ContractsList),
       callbacks = table_merge(CallbacksList)
      }.


group_fields(List) ->
  InfoList = [Info || #plt{info = Info} <- List],
  TypesList = [Types || #plt{types = Types} <- List],
  ExpTypesList = [ExpTypes || #plt{exported_types = ExpTypes} <- List],
  ContractsList = [Contracts || #plt{contracts = Contracts} <- List],
  CallbacksList = [Callbacks || #plt{callbacks = Callbacks} <- List],
  {InfoList, TypesList, ExpTypesList, ContractsList, CallbacksList}.


-spec delete(plt()) -> 'ok'.

delete(#plt{info = ETSInfo,
            types = ETSTypes,
            contracts = ETSContracts,
            callbacks = ETSCallbacks,
            exported_types = ETSExpTypes}) ->
  true = ets:delete(ETSContracts),
  true = ets:delete(ETSTypes),
  true = ets:delete(ETSInfo),
  true = ets:delete(ETSCallbacks),
  true = ets:delete(ETSExpTypes),
  ok.

%%---------------------------------------------------------------------------
%% Ets table

table_to_list(Plt) ->
  ets:tab2list(Plt).

table_delete_module(Tab, Mod) ->
  MS = ets:fun2ms(fun({{M, _F, _A}, _Val}) -> M =:= Mod;
                     ({_, _}) -> false
                  end),
  _NumDeleted = ets:select_delete(Tab, MS),
  Tab.

table_delete_module1(Tab, Mod) ->
  MS = ets:fun2ms(fun({{M, _F, _A}}) -> M =:= Mod end),
  _NumDeleted = ets:select_delete(Tab, MS),
  Tab.

table_delete_module2(Tab, Mod) ->
  true = ets:delete(Tab, Mod),
  Tab.

ets_table_delete_list(Tab, [H|T]) ->
  ets:delete(Tab, H),
  ets_table_delete_list(Tab, T);
ets_table_delete_list(Tab, []) ->
  Tab.

ets_table_lookup(Plt, Obj) ->
  try ets:lookup_element(Plt, Obj, 2) of
      Val -> {value, Val}
  catch
    _:_ -> none
  end.

table_lookup_module(Tab, Mod) ->
  MS = ets:fun2ms(fun({{M, F, A}, V}) when M =:= Mod ->
                      {{M, F, A}, V} end),
  List = [begin
            {V1, V2} = V,
            {MFA, V1, V2}
          end || {MFA, V} <- ets:select(Tab, MS)],
  case List =:= [] of
    true -> none;
    false -> {value, List}
  end.

table_all_modules(Tab) ->
  Ks = ets:match(Tab, {'$1', '_'}, 100),
  all_mods(Ks, sets:new()).

all_mods('$end_of_table', S) ->
  S;
all_mods({ListsOfKeys, Cont}, S) ->
  S1 = lists:foldl(fun([{M, _F, _A}], S0) -> sets:add_element(M, S0)
                   end, S, ListsOfKeys),
  all_mods(ets:match(Cont), S1).

table_merge([H|T]) ->
  table_merge(T, H).

table_merge([], Acc) ->
  Acc;
table_merge([Plt|Plts], Acc) ->
  NewAcc = merge_tables(Plt, Acc),
  table_merge(Plts, NewAcc).

sets_merge([H|T]) ->
  sets_merge(T, H).

sets_merge([], Acc) ->
  Acc;
sets_merge([Plt|Plts], Acc) ->
  NewAcc = merge_tables(Plt, Acc),
  sets_merge(Plts, NewAcc).


merge_tables(T1, T2) ->
  tab_merge(ets:first(T1), T1, T2).

tab_merge('$end_of_table', T1, T2) ->
  case ets:first(T1) of % no safe_fixtable()...
    '$end_of_table' ->
      true = ets:delete(T1),
      T2;
    Key ->
      tab_merge(Key, T1, T2)
  end;
tab_merge(K1, T1, T2) ->
  NextK1 = ets:next(T1, K1),
  Vs = ets:take(T1, K1),
  true = ets:insert(T2, Vs),
  tab_merge(NextK1, T1, T2).


%% Returns all contracts stored in the PLT
-spec get_all_contracts(plt()) -> #{mfa() => #contract{}}.
get_all_contracts(#plt{contracts = ETSContracts}) ->
  maps:from_list(ets:tab2list(ETSContracts)).

%% Returns all callbacks stored in the PLT
-spec get_all_callbacks(plt()) -> #{mfa() => #contract{}}.
get_all_callbacks(#plt{callbacks = ETSCallbacks}) ->
  #{K => V ||
    {_M, Cbs} <- ets:tab2list(ETSCallbacks),
    {K, V} <- Cbs}.

%% Returns all types stored in the PLT
-spec get_all_types(plt()) -> #{module() => erl_types:type_table()}.
get_all_types(#plt{types = ETSTypes}) ->
  Types = ets:tab2list(ETSTypes),
  maps:from_list(Types).

-spec plt_kind(file:filename()) -> 'iplt' | 'cplt' | 'bad_file' | 'no_file'.
plt_kind(FileName) ->
  case filelib:is_regular(FileName) of
    true ->
      case dialyzer_iplt:is_iplt(FileName) of
        true -> iplt;
        false ->
          case dialyzer_cplt:is_cplt(FileName) of
            true -> cplt;
            false -> bad_file
          end
      end;
    false -> no_file
  end.
