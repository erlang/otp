%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_snmp_hook.erl,v 1.1 2008/12/17 09:53:39 mikpe Exp $
%%
-module(mnesia_snmp_hook).

%% Hooks (called from mnesia)
-export([check_ustruct/1, create_table/3, delete_table/2,
	 key_to_oid/3, update/1, start/2,
	 get_row/2, get_next_index/2, get_mnesia_key/2]).

%% sys callback functions
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4
	]).

%% Internal exports
-export([b_init/2]).

check_ustruct([]) ->
    true;  %% default value, not SNMP'ified
check_ustruct([{key, Types}]) ->
    is_snmp_type(to_list(Types));
check_ustruct(_) -> false.

to_list(Tuple) when tuple(Tuple) -> tuple_to_list(Tuple);
to_list(X) -> [X].

is_snmp_type([integer | T]) -> is_snmp_type(T);
is_snmp_type([string | T]) -> is_snmp_type(T);
is_snmp_type([fix_string | T]) -> is_snmp_type(T);
is_snmp_type([]) -> true;
is_snmp_type(_) -> false.

create_table([], MnesiaTab, _Storage) ->
    mnesia:abort({badarg, MnesiaTab, {snmp, empty_snmpstruct}});

create_table([{key, Us}], MnesiaTab, Storage) ->
    Tree = b_new(MnesiaTab, Us),
    mnesia_lib:db_fixtable(Storage, MnesiaTab, true),
    First = mnesia_lib:db_first(Storage, MnesiaTab),
    build_table(First, MnesiaTab, Tree, Us, Storage),
    mnesia_lib:db_fixtable(Storage, MnesiaTab, false),
    Tree.

build_table(MnesiaKey, MnesiaTab, Tree, Us, Storage)
  when MnesiaKey /= '$end_of_table' ->
%%    SnmpKey = key_to_oid(MnesiaTab, MnesiaKey, Us),
%%    update(write, Tree, MnesiaKey, SnmpKey),
    update(write, Tree, MnesiaKey, MnesiaKey),
    Next = mnesia_lib:db_next_key(Storage, MnesiaTab, MnesiaKey),
    build_table(Next, MnesiaTab, Tree, Us, Storage);
build_table('$end_of_table', _MnesiaTab, _Tree, _Us, _Storage) ->
    ok.

delete_table(_MnesiaTab, Tree) ->
    exit(Tree, shutdown),
    ok.

%%-----------------------------------------------------------------
%% update({Op, MnesiaTab, MnesiaKey, SnmpKey})
%%-----------------------------------------------------------------

update({clear_table, MnesiaTab}) ->
    Tree = mnesia_lib:val({MnesiaTab, {index, snmp}}),
    b_clear(Tree);

update({Op, MnesiaTab, MnesiaKey, SnmpKey}) ->
    Tree = mnesia_lib:val({MnesiaTab, {index, snmp}}),
    update(Op, Tree, MnesiaKey, SnmpKey).

update(Op, Tree, MnesiaKey, _) ->
    case Op of
	write ->
	    b_insert(Tree, MnesiaKey, MnesiaKey);
	update_counter ->
	    ignore;
	delete ->
	    b_delete(Tree, MnesiaKey);
	delete_object ->
	    b_delete(Tree, MnesiaKey)
    end,
    ok.

%%-----------------------------------------------------------------
%% Func: key_to_oid(Tab, Key, Ustruct)
%% Args: Key ::= key()
%%         key() ::= int() | string() | {int() | string()}
%%       Type ::= {fix_string | term()}
%% Make an OBJECT IDENTIFIER out of it.
%% Variable length objects are prepended by their length.
%% Ex. Key = {"pelle", 42} AND Type = {string, integer} =>
%%        OID [5, $p, $e, $l, $l, $e, 42]
%%     Key = {"pelle", 42} AND Type = {fix_string, integer} =>
%%        OID [$p, $e, $l, $l, $e, 42]
%%-----------------------------------------------------------------
key_to_oid(Tab, Key, [{key, Types}]) ->
    MnesiaOid = {Tab, Key},
    if
	tuple(Key), tuple(Types) ->
	    case {size(Key), size(Types)} of
		{Size, Size} ->
		    keys_to_oid(MnesiaOid, Size, Key, [], Types);
		_ ->
		    exit({bad_snmp_key, MnesiaOid})
	    end;
	true ->
	    key_to_oid_i(MnesiaOid, Key, Types)
    end.

key_to_oid_i(_MnesiaOid, Key, integer) when integer(Key) -> [Key];
key_to_oid_i(_MnesiaOid, Key, fix_string) when list(Key) -> Key;
key_to_oid_i(_MnesiaOid, Key, string) when list(Key) -> [length(Key) | Key];
key_to_oid_i(MnesiaOid, Key, Type) ->
    exit({bad_snmp_key, [MnesiaOid, Key, Type]}).

keys_to_oid(_MnesiaOid, 0, _Key, Oid, _Types) -> Oid;
keys_to_oid(MnesiaOid, N, Key, Oid, Types) ->
    Type = element(N, Types),
    KeyPart = element(N, Key),
    Oid2 = key_to_oid_i(MnesiaOid, KeyPart, Type) ++ Oid,
    keys_to_oid(MnesiaOid, N-1, Key, Oid2, Types).

%%-----------------------------------------------------------------
%% Func: get_row/2
%% Args: Name is the name of the table (atom)
%%       RowIndex is an Oid
%% Returns: {ok, Row} | undefined
%%          Note that the Row returned might contain columns that
%%          are not visible via SNMP. e.g. the first column may be
%%          ifIndex, and the last MFA ({ifIndex, col1, col2, MFA}).
%%          where ifIndex is used only as index (not as a real col),
%%          and MFA as extra info, used by the application.
%%-----------------------------------------------------------------
get_row(Name, RowIndex) ->
    Tree = mnesia_lib:val({Name, {index, snmp}}),
    case b_lookup(Tree, RowIndex) of
	{ok, {_RowIndex, Key}} ->
	    [Row] = mnesia:dirty_read({Name, Key}),
	    {ok, Row};
	_ ->
	    undefined
    end.

%%-----------------------------------------------------------------
%% Func: get_next_index/2
%% Args: Name is the name of the table (atom)
%%       RowIndex is an Oid
%% Returns: {ok, NextIndex} | endOfTable
%%-----------------------------------------------------------------
get_next_index(Name, RowIndex) ->
    Tree = mnesia_lib:val({Name, {index, snmp}}),
    case b_lookup_next(Tree, RowIndex) of
	{ok, {NextIndex, _Key}} ->
	    {ok, NextIndex};
	_ ->
	    endOfTable
    end.

%%-----------------------------------------------------------------
%% Func: get_mnesia_key/2
%% Purpose: Get the mnesia key corresponding to the RowIndex.
%% Args: Name is the name of the table (atom)
%%       RowIndex is an Oid
%% Returns: {ok, Key} | undefiend
%%-----------------------------------------------------------------
get_mnesia_key(Name, RowIndex) ->
    Tree = mnesia_lib:val({Name, {index, snmp}}),
    case b_lookup(Tree, RowIndex) of
	{ok, {_RowIndex, Key}} ->
	    {ok, Key};
	_ ->
	    undefined
    end.

%%-----------------------------------------------------------------
%% Encapsulate a bplus_tree in a process.
%%-----------------------------------------------------------------

b_new(MnesiaTab, Us) ->
    case supervisor:start_child(mnesia_snmp_sup, [MnesiaTab, Us]) of
	{ok, Tree} ->
	    Tree;
	{error, Reason} ->
	    exit({badsnmp, MnesiaTab, Reason})
    end.

start(MnesiaTab, Us) ->
    Name = {mnesia_snmp, MnesiaTab},
    mnesia_monitor:start_proc(Name, ?MODULE, b_init, [self(), Us]).

b_insert(Tree, Key, Val) -> Tree ! {insert, Key, Val}.
b_delete(Tree, Key) -> Tree ! {delete, Key}.
b_lookup(Tree, Key) ->
    Tree ! {lookup, self(), Key},
    receive
	{bplus_res, Res} ->
	    Res
    end.
b_lookup_next(Tree, Key) ->
    Tree ! {lookup_next, self(), Key},
    receive
	{bplus_res, Res} ->
	    Res
    end.

b_clear(Tree) ->
    Tree ! clear,
    ok.

b_init(Parent, Us) ->
    %% Do not trap exit
    Tree = snmp_index:new(Us),
    proc_lib:init_ack(Parent, {ok, self()}),
    b_loop(Parent, Tree, Us).

b_loop(Parent, Tree, Us) ->
    receive
	{insert, Key, Val} ->
	    NTree = snmp_index:insert(Tree, Key, Val),
	    b_loop(Parent, NTree, Us);
	{delete, Key} ->
	    NTree = snmp_index:delete(Tree, Key),
	    b_loop(Parent, NTree, Us);
	{lookup, From, Key} ->
	    Res = snmp_index:get(Tree, Key),
	    From ! {bplus_res, Res},
	    b_loop(Parent, Tree, Us);
	{lookup_next, From, Key} ->
	    Res = snmp_index:get_next(Tree, Key),
	    From ! {bplus_res, Res},
	    b_loop(Parent, Tree, Us);
	clear ->
	    catch snmp_index:delete(Tree), %% Catch because delete/1 is not
	    NewTree = snmp_index:new(Us),  %% available in old snmp (before R5)
	    b_loop(Parent, NewTree, Us);

	{'EXIT', Parent, Reason} ->
	    exit(Reason);

	{system, From, Msg} ->
	    mnesia_lib:dbg_out("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], {Tree, Us})

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

system_continue(Parent, _Debug, {Tree, Us}) ->
    b_loop(Parent, Tree, Us).

system_terminate(Reason, _Parent, _Debug, _Tree) ->
    exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.
