%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

%%
-module(mnesia_snmp_hook).

%% Hooks (called from mnesia)
-export([check_ustruct/1, create_table/3, delete_table/2,
	 key_to_oid/2, key_to_oid/3, oid_to_key/2, 
	 update/1, 
	 get_row/2, get_next_index/2, get_mnesia_key/2]).

-export([key_to_oid_i/2, oid_to_key_1/2]). %% Test

-include("mnesia.hrl").


check_ustruct([]) ->
    true;  %% default value, not SNMP'ified
check_ustruct([{key, Types}]) -> 
    is_snmp_type(to_list(Types));
check_ustruct(_) -> false.
    
to_list(Tuple) when is_tuple(Tuple) -> tuple_to_list(Tuple);
to_list(X) -> [X].

is_snmp_type([integer    | T]) -> is_snmp_type(T);
is_snmp_type([string     | T]) -> is_snmp_type(T);
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
    %%update(write, Tree, MnesiaKey, MnesiaKey),
    SnmpKey = key_to_oid_i(MnesiaKey, Us),
    b_insert(Tree, SnmpKey, MnesiaKey),
    Next = mnesia_lib:db_next_key(Storage, MnesiaTab, MnesiaKey), 
    build_table(Next, MnesiaTab, Tree, Us, Storage);
build_table('$end_of_table', _MnesiaTab, _Tree, _Us, _Storage) ->
    ok.

delete_table(_MnesiaTab, Tree) ->
    b_delete_tree(Tree),
    ok.

%%-----------------------------------------------------------------
%% update({Op, MnesiaTab, MnesiaKey, SnmpKey})
%%-----------------------------------------------------------------
   
update({clear_table, MnesiaTab}) ->
    Tree = mnesia_lib:val({MnesiaTab, {index, snmp}}),
    b_clear(Tree),
    ok;
    
update({Op, MnesiaTab, MnesiaKey, SnmpKey}) ->
    Tree = mnesia_lib:val({MnesiaTab, {index, snmp}}),
    update(Op, Tree, MnesiaKey, SnmpKey).

update(Op, Tree, MnesiaKey, SnmpKey) ->
    case Op of
	write ->
	    b_insert(Tree, SnmpKey, MnesiaKey);
	update_counter ->
	    ignore;
	delete ->
	    b_delete(Tree, SnmpKey);
	delete_object ->
	    b_delete(Tree, SnmpKey)
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

key_to_oid(Tab,Key) ->
    Types = mnesia_lib:val({Tab,snmp}),
    key_to_oid(Tab, Key, Types).
	     
key_to_oid(Tab, Key, [{key, Types}]) ->
    try key_to_oid_i(Key,Types) 
    catch _:_ ->
	    mnesia:abort({bad_snmp_key, {Tab,Key}, Types})
    end.
	
key_to_oid_i(Key, integer) when is_integer(Key) -> [Key];
key_to_oid_i(Key, fix_string) when is_list(Key) -> Key;
key_to_oid_i(Key, string) when is_list(Key) -> [length(Key) | Key];
key_to_oid_i(Key, Types) -> keys_to_oid(size(Key), Key, [], Types).

keys_to_oid(0, _Key, Oid, _Types) -> Oid;
keys_to_oid(N, Key, Oid, Types) ->
    Oid2 = lists:append(key_to_oid_i(element(N, Key), element(N, Types)), Oid),
    keys_to_oid(N-1, Key, Oid2, Types).

%%--------------------------------------------------
%% The reverse of the above, i.e. snmp oid to mnesia key.
%% This can be lookup up in tree but that might be on a remote node.
%% It's probably faster to look it up, but use when it migth be remote 
oid_to_key(Oid, Tab) ->
    [{key, Types}] = mnesia_lib:val({Tab,snmp}),
    oid_to_key_1(Types, Oid). 

oid_to_key_1(integer, [Key])  -> Key;
oid_to_key_1(fix_string, Key) -> Key;
oid_to_key_1(string, [_|Key]) -> Key;
oid_to_key_1(Tuple, Oid) ->
    try 
	List = oid_to_key_2(1, size(Tuple), Tuple, Oid),
	list_to_tuple(List)
    catch 
	_:_ -> unknown
    end.

oid_to_key_2(N, Sz, Tuple, Oid0) when N =< Sz ->
    case element(N, Tuple) of
	integer -> 
	    [Key|Oid] = Oid0,
	    [Key|oid_to_key_2(N+1, Sz, Tuple, Oid)];
	fix_string when N =:= Sz ->
	    [Oid0];
	fix_string ->
	    throw(fix_string);
	string ->
	    [Len|Oid1] = Oid0,
	    {Str,Oid} = lists:split(Len, Oid1),
	    [Str|oid_to_key_2(N+1, Sz, Tuple, Oid)]
    end;
oid_to_key_2(N, Sz, _, []) when N =:= (Sz+1) ->
    [].

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
%% Returns: {NextIndex,MnesiaKey}  | {endOfTable, undefined}
%%-----------------------------------------------------------------
get_next_index(Name, RowIndex) ->
    Tree = mnesia_lib:val({Name, {index, snmp}}),
    case b_lookup_next(Tree, RowIndex) of
	{ok, R} ->
	    R;
	_ ->
	    {endOfTable,undefined}
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
%% Internal implementation, ordered_set ets.

b_new(_Tab, _Us) ->
    mnesia_monitor:unsafe_mktab(?MODULE, [public, ordered_set]).

b_delete_tree(Tree) ->
    ets:delete(Tree).  %% Close via mnesia_monitor ?

b_clear(Tree) ->
    ets:delete_all_objects(Tree).

b_insert(Tree, SnmpKey, MnesiaKey) ->
    ets:insert(Tree, {SnmpKey, MnesiaKey}).

b_delete(Tree, SnmpKey) ->
    ets:delete(Tree, SnmpKey).

b_lookup(Tree, RowIndex) ->
    case ets:lookup(Tree, RowIndex) of
	[X] ->
	    {ok, X};
	_ ->
	    undefined
    end.

b_lookup_next(Tree,RowIndex) ->
    case ets:next(Tree, RowIndex) of
	'$end_of_table' ->
	    undefined;
	Key ->
	    b_lookup(Tree, Key)
    end.
