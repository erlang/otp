%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
-moduledoc """
Abstract Data Type for SNMP Indexing

The module `snmp_index` implements an Abstract Data Type (ADT) for an SNMP index
structure for SNMP tables. It is implemented as an ets table of the ordered_set
data-type, which means that all operations are O(log n). In the table, the key
is an ASN.1 OBJECT IDENTIFIER.

This index is used to separate the implementation of the SNMP ordering from the
actual implementation of the table. The SNMP ordering, that is implementation of
GET NEXT, is implemented in this module.

For example, suppose there is an SNMP table, which is best implemented in Erlang
as one process per SNMP table row. Suppose further that the INDEX in the SNMP
table is an OCTET STRING. The index structure would be created as follows:

```text
snmp_index:new(string)
```

For each new process we create, we insert an item in an `snmp_index` structure:

```erlang
new_process(Name, SnmpIndex) ->
  Pid = start_process(),
  NewSnmpIndex =
    snmp_index:insert(SnmpIndex, Name, Pid),
  <...>
```

With this structure, we can now map an OBJECT IDENTIFIER in e.g. a GET NEXT
request, to the correct process:

```erlang
get_next_pid(Oid, SnmpIndex) ->
  {ok, {_, Pid}} = snmp_index:get_next(SnmpIndex, Oid),
  Pid.
```

## Warnings

> #### Warning {: .warning }
>
> [](){: #1 } All API functions that update the index return a `NewIndex` term.
> This is for backward compatibility with a previous implementation that used a
> B+ tree written purely in Erlang for the index. The `NewIndex` return value
> can now be ignored. The return value is now the unchanged table identifier for
> the ets table.
>
> The implementation using ets tables introduces a semantic incompatibility with
> older implementations. In those older implementations, using pure Erlang
> terms, the index was garbage collected like any other Erlang term and did not
> have to be deleted when discarded. An ets table is deleted only when the
> process creating it explicitly deletes it or when the creating process
> terminates.
>
> A new interface [`delete/1`](`delete/1`) is now added to handle the case when
> a process wants to discard an index table (i.e. to build a completely new).
> Any application using transient snmp indexes has to be modified to handle
> this.
>
> As an snmp adaption usually keeps the index for the whole of the systems
> lifetime, this is rarely a problem.

""".

-export([new/1, new/2, 
	 insert/3, 
	 delete/1, delete/2, 
	 get/2, get_next/2,
	 get_last/1, 
	 key_to_oid/2]).

-export_type([
              index/0,
              key_types/0,
              key_spec/0,
              type_spec/0,
              key/0
             ]).

-define(VMODULE,"IDX").
-include("snmp_verbosity.hrl").

-record(tab, {id, keys}).

-define(badarg(F, A), exit({badarg, {?MODULE, F, A}})).
-define(bad_new(A),   ?badarg(new, A)).
-define(bad_get(A),   ?badarg(get, A)).

-doc "This type denotes an snmp index structure.".
-opaque index()     :: #tab{}.
-doc """
This type is used when creating the index structure, and the `t:key/0` type is
used when inserting and deleting items from the structure.

If the INDEX column is of type INTEGER, or derived from INTEGER, the
corresponding type should be `integer`. If it is a variable length type (e.g.
OBJECT IDENTIFIER, OCTET STRING), the corresponding type should be `string`.
Finally, if the type is of variable length, but with a fixed size restriction
(e.g. IpAddress), the corresponding type should be `fix_string`.

There is no way to propely describe this type in the erlang type language, which
is why `t:tuple/0` was used above. The proper definition looks like:

`key_types = type_spec() | {type_spec(), type_spec(), ...}`
""".
-type   key_types() :: type_spec() | tuple().
-doc """
This type correlates to the `t:key_types/0` type. If the `t:key_types/0` is a
single atom, the corresponding `t:key/0` is a single type as well, but if the
`t:key_types/0` is a tuple, `t:key/0` must be a tuple of the same size.

In the example above, valid `keys` could be `{"hi", "mom"}` and
`{"no", "thanks"}`, whereas `"hi"`, `{"hi", 42}` and `{"hello", "there"}` would
be invalid.

There is no way to propely describe this type in the erlang type language, which
is why `t:tuple/0` was used above. The proper definition looks like:

`key() = key_spec() | {key_spec(), key_spec(), ...}`
""".
-type   key()       :: key_spec()  | tuple().
-type   key_spec()  :: string() | integer().
-type   type_spec() :: fix_string | string | integer.


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
%% Returns: index()
%%-----------------------------------------------------------------

-doc "Create an new anonymous snmp index structure.".
-spec new(KeyTypes) -> Index when
      KeyTypes :: key_types(),
      Index    :: index().

new(KeyTypes) ->
    ?vlog("new -> entry with"
	  "~n   KeyTypes: ~p", [KeyTypes]),
    do_new(KeyTypes, ?MODULE, [public, ordered_set]).


-doc "Creates a new named snmp index structure.".
-doc(#{since => <<"OTP 27.0">>}).
-spec new(KeyTypes, Name) -> Index when
      KeyTypes :: key_types(),
      Name     :: atom(),
      Index    :: index().

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


-doc """
Gets the item with key `KeyOid`. Could be used from within an SNMP
instrumentation function.
""".
-spec get(Index, KeyOid) -> {ok, {KeyOid, Value}} | undefined when
      Index  :: index(),
      KeyOid :: snmp:oid(),
      Value  :: term().

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

      

-doc "Gets the last item in the index structure.".
-spec get_last(Index) -> {ok, {KeyOid, Value}} | undefined when
      Index  :: index(),
      KeyOid :: snmp:oid(),
      Value  :: term().
      
get_last(#tab{id = OrdSet} = Index) ->
    ?vlog("get_last -> entry with"
	  "~n   OrdSet: ~p", [OrdSet]),
    case ets:last(OrdSet) of
	'$end_of_table' ->
	    undefined;
	Key ->
	    get(Index, Key)
    end.


-doc """
Gets the next item in the SNMP lexicographic ordering, after `KeyOid` in the
index structure. `KeyOid` does not have to refer to an existing item in the
index.
""".
-spec get_next(Index, KeyOid) -> {ok, {NextKeyOid, Value}} | undefined when
      Index      :: index(),
      KeyOid     :: snmp:oid(),
      NextKeyOid :: snmp:oid(),
      Value      :: term().

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


-doc """
Inserts a new key value tuple into the index structure. If an item with the same
key already exists, the new `Value` overwrites the old value.
""".
-spec insert(Index, Key, Value) -> NewIndex when
      Index    :: index(),
      Key      :: key(),
      Value    :: term(),
      NewIndex :: index().

insert(#tab{id = OrdSet, keys = KeyTypes} = Tab, Key, Val) ->
    ets:insert(OrdSet, {key_to_oid_i(Key, KeyTypes), Val}),
    Tab.


-doc """
Deletes a complete index structure (i.e. the ets table holding the index). The
index can no longer be referenced after this call. See the
[warning note](`m:snmp_index#1`) above.
""".
-spec delete(Index) -> true when
      Index :: index().

delete(#tab{id = OrdSet}) ->
    ets:delete(OrdSet).


-doc "Deletes a key and its value from the index structure. Returns a new structure.".
-spec delete(Index, Key) -> NewIndex when
      Index    :: index(),
      Key      :: key(),
      NewIndex :: index().

delete(#tab{id = OrdSet, keys = KeyTypes} = Tab, Key) ->
    ets:delete(OrdSet, key_to_oid_i(Key, KeyTypes)),
    Tab.


-doc "Converts `Key` to an OBJECT IDENTIFIER.".
-spec key_to_oid(Index, Key) -> KeyOid when
      Index  :: index(),
      Key    :: key(),
      KeyOid :: snmp:oid().

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
key_to_oid_i(Key, Types) when is_tuple(Key) -> keys_to_oid(tuple_size(Key), Key, [], Types).

keys_to_oid(0, _Key, Oid, _Types) -> Oid;
keys_to_oid(N, Key, Oid, Types) ->
    Oid2 = lists:append(key_to_oid_i(element(N, Key), element(N, Types)), Oid),
    keys_to_oid(N-1, Key, Oid2, Types).

