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
%%% File    : dialyzer_codeserver.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created :  4 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_codeserver).

-export([delete/1,
	 finalize_contracts/3,
         finalize_exported_types/2,
	 finalize_records/2,
	 get_contracts/1,
	 get_callbacks/1,
         get_exported_types/1,
	 get_exports/1,
	 get_records/1,
	 get_next_core_label/1,
	 get_temp_contracts/1,
         get_temp_exported_types/1,
	 get_temp_records/1,
	 insert/3,
	 insert_exports/2,
         insert_temp_exported_types/2,
         insert_fun_meta_info/2,
	 is_exported/2,
	 lookup_mod_code/2,
	 lookup_mfa_code/2,
	 lookup_mod_records/2,
	 lookup_mod_contracts/2,
	 lookup_mfa_contract/2,
         lookup_meta_info/2,
	 new/0,
	 set_next_core_label/2,
	 set_temp_records/2,
	 store_temp_records/3,
	 store_temp_contracts/4]).

-export_type([codeserver/0, fun_meta_info/0]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-type dict_ets() :: ets:tid().
-type  set_ets() :: ets:tid().

-type types()         :: erl_types:type_table().
-type mod_records()   :: dict:dict(module(), types()).

-type contracts()     :: dict:dict(mfa(),dialyzer_contracts:file_contract()).
-type mod_contracts() :: dict:dict(module(), contracts()).

%% A property-list of data compiled from -compile and -dialyzer attributes.
-type meta_info()     :: [{{'nowarn_function' | dial_warn_tag()},
                           'mod' | 'func'}].
-type fun_meta_info() :: [{mfa(), meta_info()}
                          | {module(), [dial_warn_tag()]}].

-record(codeserver, {next_core_label = 0 :: label(),
		     code		 :: dict_ets(),
                     exported_types      :: set_ets() | 'undefined', % set(mfa())
		     records             :: dict_ets() | 'undefined',
		     contracts           :: dict_ets() | 'undefined',
		     callbacks           :: dict_ets() | 'undefined',
                     fun_meta_info       :: dict_ets(), % {mfa(), meta_info()}
		     exports             :: 'clean' | set_ets(), % set(mfa())
                     temp_exported_types :: 'clean' | set_ets(), % set(mfa())
		     temp_records        :: 'clean' | dict_ets(),
		     temp_contracts      :: 'clean' | dict_ets(),
		     temp_callbacks      :: 'clean' | dict_ets()
		    }).

-opaque codeserver() :: #codeserver{}.

%%--------------------------------------------------------------------

ets_dict_find(Key, Table) ->
  try ets:lookup_element(Table, Key, 2) of
      Val -> {ok, Val}
  catch
    _:_ -> error
  end.

ets_dict_store(Key, Element, Table) ->
  true = ets:insert(Table, {Key, Element}),
  Table.

ets_dict_store_dict(Dict, Table) ->
  true = ets:insert(Table, dict:to_list(Dict)).

ets_dict_to_dict(Table) ->
  Fold = fun({Key,Value}, Dict) -> dict:store(Key, Value, Dict) end,
  ets:foldl(Fold, dict:new(), Table).

ets_set_is_element(Key, Table) ->
  ets:lookup(Table, Key) =/= [].

ets_set_insert_set(Set, Table) ->
  ets_set_insert_list(sets:to_list(Set), Table).

ets_set_insert_list(List, Table) ->
  true = ets:insert(Table, [{E} || E <- List]).

ets_set_to_set(Table) ->
  Fold = fun({E}, Set) -> sets:add_element(E, Set) end,
  ets:foldl(Fold, sets:new(), Table).

ets_read_concurrent_table(Name) ->
  ets:new(Name, [{read_concurrency, true}]).

%%--------------------------------------------------------------------

-spec new() -> codeserver().

new() ->
  CodeOptions = [compressed, public, {read_concurrency, true}],
  Code = ets:new(dialyzer_codeserver_code, CodeOptions),
  TempOptions = [public, {write_concurrency, true}],
  [Exports, FunMetaInfo, TempExportedTypes, TempRecords, TempContracts,
   TempCallbacks] =
    [ets:new(Name, TempOptions) ||
      Name <-
	[dialyzer_codeserver_exports, dialyzer_codeserver_fun_meta_info,
         dialyzer_codeserver_temp_exported_types,
	 dialyzer_codeserver_temp_records, dialyzer_codeserver_temp_contracts,
	 dialyzer_codeserver_temp_callbacks]],
  #codeserver{code                = Code,
	      exports             = Exports,
              fun_meta_info       = FunMetaInfo,
	      temp_exported_types = TempExportedTypes,
	      temp_records        = TempRecords,
	      temp_contracts      = TempContracts,
	      temp_callbacks      = TempCallbacks}.

-spec delete(codeserver()) -> 'ok'.

delete(#codeserver{code = Code, exported_types = ExportedTypes,
		   records = Records, contracts = Contracts,
		   callbacks = Callbacks}) ->
  lists:foreach(fun ets:delete/1,
		[Code, ExportedTypes, Records, Contracts, Callbacks]).

-spec insert(atom(), cerl:c_module(), codeserver()) -> codeserver().

insert(Mod, ModCode, CS) ->
  Name = cerl:module_name(ModCode),
  Exports = cerl:module_exports(ModCode),
  Attrs = cerl:module_attrs(ModCode),
  Defs = cerl:module_defs(ModCode),
  As = cerl:get_ann(ModCode),
  Funs =
    [{{Mod, cerl:fname_id(Var), cerl:fname_arity(Var)},
      Val} || Val = {Var, _Fun} <- Defs],
  Keys = [Key || {Key, _Value} <- Funs],
  ModEntry = {Mod, {Name, Exports, Attrs, Keys, As}},
  true = ets:insert(CS#codeserver.code, [ModEntry|Funs]),
  CS.

-spec get_temp_exported_types(codeserver()) -> sets:set(mfa()).

get_temp_exported_types(#codeserver{temp_exported_types = TempExpTypes}) ->
  ets_set_to_set(TempExpTypes).

-spec insert_temp_exported_types(sets:set(mfa()), codeserver()) -> codeserver().

insert_temp_exported_types(Set, CS) ->
  TempExportedTypes = CS#codeserver.temp_exported_types,
  true = ets_set_insert_set(Set, TempExportedTypes),
  CS.

-spec insert_exports([mfa()], codeserver()) -> codeserver().

insert_exports(List, #codeserver{exports = Exports} = CS) ->
  true = ets_set_insert_list(List, Exports),
  CS.

-spec insert_fun_meta_info(fun_meta_info(), codeserver()) -> codeserver().

insert_fun_meta_info(List, #codeserver{fun_meta_info = FunMetaInfo} = CS) ->
  true = ets:insert(FunMetaInfo, List),
  CS.

-spec is_exported(mfa(), codeserver()) -> boolean().

is_exported(MFA, #codeserver{exports = Exports}) ->
  ets_set_is_element(MFA, Exports).

-spec get_exported_types(codeserver()) -> sets:set(mfa()).

get_exported_types(#codeserver{exported_types = ExpTypes}) ->
  ets_set_to_set(ExpTypes).

-spec get_exports(codeserver()) -> sets:set(mfa()).

get_exports(#codeserver{exports = Exports}) ->
  ets_set_to_set(Exports).

-spec finalize_exported_types(sets:set(mfa()), codeserver()) -> codeserver().

finalize_exported_types(Set, CS) ->
  ExportedTypes = ets_read_concurrent_table(dialyzer_codeserver_exported_types),
  true = ets_set_insert_set(Set, ExportedTypes),
  TempExpTypes = CS#codeserver.temp_exported_types,
  true = ets:delete(TempExpTypes),
  CS#codeserver{exported_types = ExportedTypes, temp_exported_types = clean}.

-spec lookup_mod_code(atom(), codeserver()) -> cerl:c_module().

lookup_mod_code(Mod, CS) when is_atom(Mod) ->
  table__lookup(CS#codeserver.code, Mod).

-spec lookup_mfa_code(mfa(), codeserver()) -> {cerl:c_var(), cerl:c_fun()}.

lookup_mfa_code({_M, _F, _A} = MFA, CS) ->
  table__lookup(CS#codeserver.code, MFA).

-spec get_next_core_label(codeserver()) -> label().

get_next_core_label(#codeserver{next_core_label = NCL}) ->
  NCL.

-spec set_next_core_label(label(), codeserver()) -> codeserver().

set_next_core_label(NCL, CS) ->
  CS#codeserver{next_core_label = NCL}.

-spec lookup_mod_records(atom(), codeserver()) -> types().

lookup_mod_records(Mod, #codeserver{records = RecDict}) when is_atom(Mod) ->
  case ets_dict_find(Mod, RecDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec get_records(codeserver()) -> mod_records().

get_records(#codeserver{records = RecDict}) ->
  ets_dict_to_dict(RecDict).

-spec store_temp_records(module(), types(), codeserver()) -> codeserver().

store_temp_records(Mod, Dict, #codeserver{temp_records = TempRecDict} = CS)
  when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#codeserver{temp_records = ets_dict_store(Mod, Dict, TempRecDict)}
  end.

-spec get_temp_records(codeserver()) -> mod_records().

get_temp_records(#codeserver{temp_records = TempRecDict}) ->
  ets_dict_to_dict(TempRecDict).

-spec set_temp_records(mod_records(), codeserver()) -> codeserver().

set_temp_records(Dict, CS) ->
  true = ets:delete(CS#codeserver.temp_records),
  TempRecords = ets:new(dialyzer_codeserver_temp_records,[]),
  true = ets_dict_store_dict(Dict, TempRecords),
  CS#codeserver{temp_records = TempRecords}.

-spec finalize_records(mod_records(), codeserver()) -> codeserver().

finalize_records(Dict, CS) ->
  true = ets:delete(CS#codeserver.temp_records),
  Records = ets_read_concurrent_table(dialyzer_codeserver_records),
  true = ets_dict_store_dict(Dict, Records),
  CS#codeserver{records = Records, temp_records = clean}.

-spec lookup_mod_contracts(atom(), codeserver()) -> contracts().

lookup_mod_contracts(Mod, #codeserver{contracts = ContDict})
  when is_atom(Mod) ->
  case ets_dict_find(Mod, ContDict) of
    error -> dict:new();
    {ok, Keys} ->
      dict:from_list([get_file_contract(Key, ContDict)|| Key <- Keys])
  end.

get_file_contract(Key, ContDict) ->
  {Key, ets:lookup_element(ContDict, Key, 2)}.

-spec lookup_mfa_contract(mfa(), codeserver()) ->
         'error' | {'ok', dialyzer_contracts:file_contract()}.

lookup_mfa_contract(MFA, #codeserver{contracts = ContDict}) ->
  ets_dict_find(MFA, ContDict).

-spec lookup_meta_info(module() | mfa(), codeserver()) -> meta_info().

lookup_meta_info(MorMFA, #codeserver{fun_meta_info = FunMetaInfo}) ->
  case ets_dict_find(MorMFA, FunMetaInfo) of
    error -> [];
    {ok, PropList} -> PropList
  end.

-spec get_contracts(codeserver()) -> mod_contracts().

get_contracts(#codeserver{contracts = ContDict}) ->
  ets_dict_to_dict(ContDict).

-spec get_callbacks(codeserver()) -> list().

get_callbacks(#codeserver{callbacks = CallbDict}) ->
  ets:tab2list(CallbDict).

-spec store_temp_contracts(module(), contracts(), contracts(), codeserver()) ->
	 codeserver().

store_temp_contracts(Mod, SpecDict, CallbackDict,
		     #codeserver{temp_contracts = Cn,
				 temp_callbacks = Cb} = CS)
  when is_atom(Mod) ->
  CS1 =
    case dict:size(SpecDict) =:= 0 of
      true -> CS;
      false ->
	CS#codeserver{temp_contracts = ets_dict_store(Mod, SpecDict, Cn)}
    end,
  case dict:size(CallbackDict) =:= 0 of
    true -> CS1;
    false ->
      CS1#codeserver{temp_callbacks = ets_dict_store(Mod, CallbackDict, Cb)}
  end.

-spec get_temp_contracts(codeserver()) -> {mod_contracts(), mod_contracts()}.

get_temp_contracts(#codeserver{temp_contracts = TempContDict,
			       temp_callbacks = TempCallDict}) ->
  {ets_dict_to_dict(TempContDict), ets_dict_to_dict(TempCallDict)}.

-spec finalize_contracts(mod_contracts(), mod_contracts(), codeserver()) ->
                           codeserver().

finalize_contracts(SpecDict, CallbackDict, CS)  ->
  Contracts = ets_read_concurrent_table(dialyzer_codeserver_contracts),
  Callbacks = ets_read_concurrent_table(dialyzer_codeserver_callbacks),
  Contracts = dict:fold(fun decompose_spec_dict/3, Contracts, SpecDict),
  Callbacks = dict:fold(fun decompose_cb_dict/3, Callbacks, CallbackDict),
  CS#codeserver{contracts = Contracts, callbacks = Callbacks,
		temp_contracts = clean, temp_callbacks = clean}.

decompose_spec_dict(Mod, Dict, Table) ->
  Keys = dict:fetch_keys(Dict),
  true = ets:insert(Table, dict:to_list(Dict)),
  true = ets:insert(Table, {Mod, Keys}),
  Table.

decompose_cb_dict(_Mod, Dict, Table) ->
  true = ets:insert(Table, dict:to_list(Dict)),
  Table.

table__lookup(TablePid, M) when is_atom(M) ->
  {Name, Exports, Attrs, Keys, As} = ets:lookup_element(TablePid, M, 2),
  Defs = [table__lookup(TablePid, Key) || Key <- Keys],
  cerl:ann_c_module(As, Name, Exports, Attrs, Defs);
table__lookup(TablePid, MFA) ->
  ets:lookup_element(TablePid, MFA, 2).
