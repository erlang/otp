%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(snmp_index).

-export([new/1, new/2, 
	 insert/3, 
	 delete/1, delete/2, 
	 get/2, get_next/2,
	 get_last/1, 
	 key_to_oid/2]).


-define(VMODULE,"IDX").
-include("snmp_verbosity.hrl").

-record(tab, {id, keys}).

-define(badarg(F, A), exit({badarg, {?MODULE, F, A}})).
-define(bad_new(A),   ?badarg(new, A)).
-define(bad_get(A),   ?badarg(get, A)).


%%%-----------------------------------------------------------------
%%% This module implements an SNMP index structure as an ADT.
%%% It is supposed to be used as a separate structure which implements
%%% the SNMP ordering of the keys in the SNMP table.  The advantage
%%% with this is that the get-next operation is automatically
%%% taken care of.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Args: KeyTypes = key() | {key(), ...}
%%       key() = integer | string | fix_string
%% Returns: handle()
%%-----------------------------------------------------------------

new(KeyTypes) ->
    ?vlog("new -> entry with"
	  "~n   KeyTypes: ~p", [KeyTypes]),
    do_new(KeyTypes, ?MODULE, [public, ordered_set]).

new(KeyTypes, Name) when is_atom(Name) ->
    ?vlog("new -> entry with"
	  "~n   KeyTypes: ~p"
	  "~n   Name:     ~p", [KeyTypes, Name]),
    do_new(KeyTypes, Name, [public, ordered_set, named_table]);
new(KeyTypes, Name) ->
    ?vinfo("new -> bad data"
	   "~n   KeyTypes: ~p"
	   "~n   Name:     ~p", [KeyTypes, Name]),
    ?bad_new([KeyTypes, Name]).

do_new(KeyTypes, EtsName, EtsOpts) ->
    ?vdebug("do_new -> entry with"
	    "~n   KeyTypes: ~p"
	    "~n   EtsName:  ~p"
	    "~n   EtsOpts:  ~p", [KeyTypes, EtsName, EtsOpts]),
    case is_snmp_type(to_list(KeyTypes)) of
	true ->
	    Tab = #tab{id = ets:new(EtsName, EtsOpts), keys = KeyTypes},
	    ?vtrace("do_new -> "
		    "~n   Tab:  ~p", [Tab]),
	    Tab;
	false ->
	    ?bad_new([KeyTypes, EtsName])
    end.


get(#tab{id = OrdSet}, KeyOid) ->
    ?vlog("get -> entry with"
	  "~n   OrdSet: ~p"
	  "~n   KeyOid: ~p", [OrdSet, KeyOid]),
    case ets:lookup(OrdSet, KeyOid) of
	[X] ->
	    {ok, X};
	_ ->
	    undefined
    end.

      

get_next(#tab{id = OrdSet} = Tab, KeyOid) ->
    ?vlog("get_next -> entry with"
	  "~n   Tab:    ~p"
	  "~n   KeyOid: ~p", [Tab, KeyOid]),
    case ets:next(OrdSet, KeyOid) of
	'$end_of_table' ->
	    undefined;
	Key ->
	    get(Tab, Key)
    end.

get_last(#tab{id = OrdSet} = Tab) ->
    ?vlog("get_last -> entry with"
	  "~n   Tab: ~p", [Tab]),
    case ets:last(OrdSet) of
	'$end_of_table' ->
	    undefined;
	Key ->
	    get(Tab, Key)
    end.

insert(#tab{id = OrdSet, keys = KeyTypes} = Tab, Key, Val) ->
    ets:insert(OrdSet, {key_to_oid_i(Key, KeyTypes), Val}),
    Tab.

delete(#tab{id = OrdSet, keys = KeyTypes} = Tab, Key) ->
    ets:delete(OrdSet, key_to_oid_i(Key, KeyTypes)),
    Tab.

delete(#tab{id = OrdSet}) ->
    ets:delete(OrdSet).

key_to_oid(#tab{keys = KeyTypes}, Key) ->
    key_to_oid_i(Key, KeyTypes).

to_list(Tuple) when is_tuple(Tuple) -> tuple_to_list(Tuple);
to_list(X) -> [X].

is_snmp_type([integer | T]) -> is_snmp_type(T);
is_snmp_type([string | T]) -> is_snmp_type(T);
is_snmp_type([fix_string | T]) -> is_snmp_type(T);
is_snmp_type([]) -> true;
is_snmp_type(_) -> false.


%%-----------------------------------------------------------------
%% Args: Key = key()
%%       key() = int() | string() | {int() | string(), ...}
%%       Type = {fix_string | term()}
%% Make an OBJECT IDENTIFIER out of it.
%% Variable length objects are prepended by their length.
%% Ex. Key = {"pelle", 42} AND Type = {string, integer} =>
%%        OID [5, $p, $e, $l, $l, $e, 42]
%%     Key = {"pelle", 42} AND Type = {fix_string, integer} =>
%%        OID [$p, $e, $l, $l, $e, 42]
%%-----------------------------------------------------------------
key_to_oid_i(Key, _Type) when is_integer(Key) -> [Key];
key_to_oid_i(Key, fix_string) -> Key;
key_to_oid_i(Key, _Type) when is_list(Key) -> [length(Key) | Key];
key_to_oid_i(Key, Types) -> keys_to_oid(size(Key), Key, [], Types).

keys_to_oid(0, _Key, Oid, _Types) -> Oid;
keys_to_oid(N, Key, Oid, Types) ->
    Oid2 = lists:append(key_to_oid_i(element(N, Key), element(N, Types)), Oid),
    keys_to_oid(N-1, Key, Oid2, Types).

