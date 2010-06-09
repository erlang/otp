%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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
%%% File    : dialyzer_codeserver.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description :
%%%
%%% Created :  4 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_codeserver).

-export([delete/1,
	 finalize_contracts/2,
         finalize_exported_types/2,
	 finalize_records/2,
	 get_contracts/1,
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
	 is_exported/2,
	 lookup_mod_code/2,
	 lookup_mfa_code/2,
	 lookup_mod_records/2,
	 lookup_mod_contracts/2,
	 lookup_mfa_contract/2,
	 new/0,
	 set_next_core_label/2,
	 set_temp_records/2,
	 store_records/3,
	 store_temp_records/3,
	 store_contracts/3,
	 store_temp_contracts/3]).

-export_type([codeserver/0]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-record(codeserver, {table_pid		              :: pid(),
                     exported_types      = sets:new() :: set(), % set(mfa())
                     temp_exported_types = sets:new() :: set(), % set(mfa())
		     exports             = sets:new() :: set(), % set(mfa())
		     next_core_label     = 0          :: label(),
		     records             = dict:new() :: dict(),
		     temp_records        = dict:new() :: dict(),
		     contracts           = dict:new() :: dict(),
		     temp_contracts      = dict:new() :: dict()}).

-opaque codeserver() :: #codeserver{}.

%%--------------------------------------------------------------------

-spec new() -> codeserver().

new() ->
  #codeserver{table_pid = table__new()}.

-spec delete(codeserver()) -> 'ok'.

delete(#codeserver{table_pid = TablePid}) ->
  table__delete(TablePid).

-spec insert(atom(), cerl:c_module(), codeserver()) -> codeserver().

insert(Mod, ModCode, CS) ->
  NewTablePid = table__insert(CS#codeserver.table_pid, Mod, ModCode),
  CS#codeserver{table_pid = NewTablePid}.

-spec insert_temp_exported_types(set(), codeserver()) -> codeserver().

insert_temp_exported_types(Set, CS) ->
  CS#codeserver{temp_exported_types = Set}.

-spec insert_exports([mfa()], codeserver()) -> codeserver().

insert_exports(List, #codeserver{exports = Exports} = CS) ->
  Set = sets:from_list(List),
  NewExports = sets:union(Exports, Set),
  CS#codeserver{exports = NewExports}.

-spec is_exported(mfa(), codeserver()) -> boolean().

is_exported(MFA, #codeserver{exports = Exports}) ->
  sets:is_element(MFA, Exports).

-spec get_exported_types(codeserver()) -> set(). % set(mfa())

get_exported_types(#codeserver{exported_types = ExpTypes}) ->
  ExpTypes.

-spec get_temp_exported_types(codeserver()) -> set().

get_temp_exported_types(#codeserver{temp_exported_types = TempExpTypes}) ->
  TempExpTypes.

-spec get_exports(codeserver()) -> set().  % set(mfa())

get_exports(#codeserver{exports = Exports}) ->
  Exports.

-spec finalize_exported_types(set(), codeserver()) -> codeserver().

finalize_exported_types(Set, CS) ->
  CS#codeserver{exported_types = Set, temp_exported_types = sets:new()}.

-spec lookup_mod_code(atom(), codeserver()) -> cerl:c_module().

lookup_mod_code(Mod, CS) when is_atom(Mod) ->
  table__lookup(CS#codeserver.table_pid, Mod).

-spec lookup_mfa_code(mfa(), codeserver()) -> {cerl:c_var(), cerl:c_fun()}.

lookup_mfa_code({_M, _F, _A} = MFA, CS) ->
  table__lookup(CS#codeserver.table_pid, MFA).

-spec get_next_core_label(codeserver()) -> label().

get_next_core_label(#codeserver{next_core_label = NCL}) ->
  NCL.

-spec set_next_core_label(label(), codeserver()) -> codeserver().

set_next_core_label(NCL, CS) ->
  CS#codeserver{next_core_label = NCL}.

-spec store_records(atom(), dict(), codeserver()) -> codeserver().

store_records(Mod, Dict, #codeserver{records = RecDict} = CS)
  when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#codeserver{records = dict:store(Mod, Dict, RecDict)}
  end.

-spec lookup_mod_records(atom(), codeserver()) -> dict().

lookup_mod_records(Mod, #codeserver{records = RecDict})
  when is_atom(Mod) ->
  case dict:find(Mod, RecDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec get_records(codeserver()) -> dict().

get_records(#codeserver{records = RecDict}) ->
  RecDict.

-spec store_temp_records(atom(), dict(), codeserver()) -> codeserver().

store_temp_records(Mod, Dict, #codeserver{temp_records = TempRecDict} = CS)
  when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#codeserver{temp_records = dict:store(Mod, Dict, TempRecDict)}
  end.

-spec get_temp_records(codeserver()) -> dict().

get_temp_records(#codeserver{temp_records = TempRecDict}) ->
  TempRecDict.

-spec set_temp_records(dict(), codeserver()) -> codeserver().

set_temp_records(Dict, CS) ->
  CS#codeserver{temp_records = Dict}.

-spec finalize_records(dict(), codeserver()) -> codeserver().

finalize_records(Dict, CS) ->
  CS#codeserver{records = Dict, temp_records = dict:new()}.

-spec store_contracts(atom(), dict(), codeserver()) -> codeserver().

store_contracts(Mod, Dict, #codeserver{contracts = C} = CS) when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#codeserver{contracts = dict:store(Mod, Dict, C)}
  end.

-spec lookup_mod_contracts(atom(), codeserver()) -> dict().

lookup_mod_contracts(Mod, #codeserver{contracts = ContDict})
  when is_atom(Mod) ->
  case dict:find(Mod, ContDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec lookup_mfa_contract(mfa(), codeserver()) ->
         'error' | {'ok', dialyzer_contracts:file_contract()}.

lookup_mfa_contract({M,_F,_A} = MFA, #codeserver{contracts = ContDict}) ->
  case dict:find(M, ContDict) of
    error -> error;
    {ok, Dict} -> dict:find(MFA, Dict)
  end.

-spec get_contracts(codeserver()) -> dict().

get_contracts(#codeserver{contracts = ContDict}) ->
  ContDict.

-spec store_temp_contracts(atom(), dict(), codeserver()) -> codeserver().

store_temp_contracts(Mod, Dict, #codeserver{temp_contracts = C} = CS)
  when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#codeserver{temp_contracts = dict:store(Mod, Dict, C)}
  end.

-spec get_temp_contracts(codeserver()) -> dict().

get_temp_contracts(#codeserver{temp_contracts = TempContDict}) ->
  TempContDict.

-spec finalize_contracts(dict(), codeserver()) -> codeserver().

finalize_contracts(Dict, CS)  ->
  CS#codeserver{contracts = Dict, temp_contracts = dict:new()}.

table__new() ->
  spawn_link(fun() -> table__loop(none, dict:new()) end).

table__delete(TablePid) ->
  TablePid ! stop,
  ok.

table__lookup(TablePid, Key) ->
  TablePid ! {self(), lookup, Key},
  receive
    {TablePid, Key, Ans} -> Ans
  end.

table__insert(TablePid, Key, Val) ->
  TablePid ! {insert, [{Key, term_to_binary(Val, [compressed])}]},
  TablePid.

table__loop(Cached, Map) ->
  receive
    stop -> ok;
    {Pid, lookup, {M, F, A} = MFA} ->
      {NewCached, Ans} =
	case Cached of
	  {M, Tree} ->
	    [Val] = [VarFun || {Var, _Fun} = VarFun <- cerl:module_defs(Tree),
			       cerl:fname_id(Var) =:= F,
			       cerl:fname_arity(Var) =:= A],
	    {Cached, Val};
	  _ ->
	    Tree = fetch_and_expand(M, Map),
	    [Val] = [VarFun || {Var, _Fun} = VarFun <- cerl:module_defs(Tree),
			       cerl:fname_id(Var) =:= F,
			       cerl:fname_arity(Var) =:= A],
	    {{M, Tree}, Val}
	end,
      Pid ! {self(), MFA, Ans},
      table__loop(NewCached, Map);
    {Pid, lookup, Mod} when is_atom(Mod) ->
      Ans = case Cached of
	      {Mod, Tree} -> Tree;
	      _ -> fetch_and_expand(Mod, Map)
	    end,
      Pid ! {self(), Mod, Ans},
      table__loop({Mod, Ans}, Map);
    {insert, List} ->
      NewMap = lists:foldl(fun({Key, Val}, AccMap) ->
			       dict:store(Key, Val, AccMap)
			   end, Map, List),
      table__loop(Cached, NewMap)
  end.

fetch_and_expand(Mod, Map) ->
  try
    Bin = dict:fetch(Mod, Map),
    binary_to_term(Bin)
  catch
    _:_ ->
      S = atom_to_list(Mod),
      Msg = "found no module named '" ++ S ++ "' in the analyzed files",
      exit({error, Msg})
  end.
