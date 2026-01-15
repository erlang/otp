%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Implements hashing functionality for fragmented tables
%%%----------------------------------------------------------------------

%header_doc_include
-module(mnesia_frag_hash).
-moduledoc """
Defines mnesia_frag_hash callback behavior

This module defines a callback behavior for user-defined hash functions of
fragmented tables.

Which module that is selected to implement the `mnesia_frag_hash` behavior for a
particular fragmented table is specified together with the other
`frag_properties`. The `hash_module` defines the module name. The `hash_state`
defines the initial hash state.

This module implements dynamic hashing, which is a kind of hashing that grows
nicely when new fragments are added. It is well suited for scalable hash tables.

## See Also

`m:mnesia`
""".
-compile([{nowarn_deprecated_function, [{erlang,phash,2}]}]).

%% Fragmented Table Hashing callback functions
-export([
	 init_state/2,
	 add_frag/1,
	 del_frag/1,
	 key_to_frag_number/2,
	 match_spec_to_frag_numbers/2
	]).

%header_doc_include
%%-behaviour(mnesia_frag_hash).

%impl_doc_include
-record(hash_state,
	{n_fragments,
	 next_n_to_split,
	 n_doubles,
	 function}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-doc """
Starts when a fragmented table is created with the function
`mnesia:create_table/2` or when a normal (unfragmented) table is converted to be
a fragmented table with `mnesia:change_table_frag/2`.

Notice that the function `add_frag/2` is started one time for each of the other
fragments (except number 1) as a part of the table creation procedure.

`State` is the initial value of the `hash_state` `frag_property`. `NewState` is
stored as `hash_state` among the other `frag_properties`.
""".
-spec init_state(Tab, State) -> NewState when
      Tab :: atom(),
      State :: term(),
      NewState :: term().
init_state(_Tab, State) when State == undefined ->
    #hash_state{n_fragments     = 1,
		next_n_to_split = 1,
		n_doubles       = 0,
		function        = phash2}.

convert_old_state({hash_state, N, P, L}) ->
    #hash_state{n_fragments     = N,
		next_n_to_split = P,
		n_doubles       = L,
		function        = phash}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
To scale well, it is a good idea to ensure that the records are evenly
distributed over all fragments, including the new one.

`NewState` is stored as `hash_state` among the other `frag_properties`.

As a part of the `add_frag` procedure, Mnesia iterates over all fragments
corresponding to the `IterFrags` numbers and starts
[`key_to_frag_number(NewState,RecordKey)`](`key_to_frag_number/2`) for each
record. If the new fragment differs from the old fragment, the record is moved
to the new fragment.

As the `add_frag` procedure is a part of a schema transaction, Mnesia acquires
write locks on the affected tables. That is, both the fragments corresponding to
`IterFrags` and those corresponding to `AdditionalLockFrags`.
""".
-spec add_frag(State :: term()) -> {NewState, IterFrags, AdditionalLockFrags} when
      NewState :: term(),
      IterFrags :: [integer()],
      AdditionalLockFrags :: [integer()].
add_frag(#hash_state{next_n_to_split = SplitN, n_doubles = L, n_fragments = N} = State) ->
    P = SplitN + 1,
    NewN = N + 1,
    State2 = case power2(L) + 1 of
		 P2 when P2 == P ->
		     State#hash_state{n_fragments      = NewN,
				      n_doubles        = L + 1,
				      next_n_to_split = 1};
		 _ ->
		     State#hash_state{n_fragments     = NewN,
				      next_n_to_split = P}
	     end,
    {State2, [SplitN], [NewN]};
add_frag(OldState) ->
    State = convert_old_state(OldState),
    add_frag(State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc """
`NewState` is stored as `hash_state` among the other `frag_properties`.

As a part of the `del_frag` procedure, Mnesia iterates over all fragments
corresponding to the `IterFrags` numbers and starts
[`key_to_frag_number(NewState,RecordKey)`](`key_to_frag_number/2`) for each
record. If the new fragment differs from the old fragment, the record is moved
to the new fragment.

Notice that all records in the last fragment must be moved to another fragment,
as the entire fragment is deleted.

As the `del_frag` procedure is a part of a schema transaction, Mnesia acquires
write locks on the affected tables. That is, both the fragments corresponding to
`IterFrags` and those corresponding to `AdditionalLockFrags`.
""".
-spec del_frag(State :: term()) -> {NewState, IterFrags, AdditionalLockFrags} when
      NewState :: term(),
      IterFrags :: [integer()],
      AdditionalLockFrags :: [integer()].
del_frag(#hash_state{next_n_to_split = SplitN, n_doubles = L, n_fragments = N} = State) ->
    P = SplitN - 1,
    if
	P < 1 ->
	    L2 = L - 1,
	    MergeN = power2(L2),
	    State2 = State#hash_state{n_fragments     = N - 1,
				      next_n_to_split = MergeN,
				      n_doubles       = L2},
	    {State2, [N], [MergeN]};
	true ->
	    MergeN = P,
	    State2 = State#hash_state{n_fragments     = N - 1,
				      next_n_to_split = MergeN},
	    {State2, [N], [MergeN]}
	end;
del_frag(OldState) ->
    State = convert_old_state(OldState),
    del_frag(State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-doc """
Starts whenever Mnesia needs to determine which fragment a certain record
belongs to. It is typically started at `read`, `write`, and `delete`.
""".
-spec key_to_frag_number(State, Key) -> Fragnum when
      State :: term(),
      Key :: term(),
      Fragnum :: integer().
key_to_frag_number(#hash_state{function = phash, n_fragments = N, n_doubles = L}, Key) ->
    A = erlang:phash(Key, power2(L + 1)),
    if
	A > N ->
	    A - power2(L);
	true ->
	    A
    end;
key_to_frag_number(#hash_state{function = phash2, n_fragments = N, n_doubles = L}, Key) ->
    A = erlang:phash2(Key, power2(L + 1)) + 1,
    if
	A > N ->
	    A - power2(L);
	true ->
	    A
    end;
key_to_frag_number(OldState, Key) ->
    State = convert_old_state(OldState),
    key_to_frag_number(State, Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-doc """
This function is called whenever Mnesia needs to determine which fragments that
need to be searched for a `MatchSpec`. It is typically called by `select` and
`match_object`.
""".
-spec match_spec_to_frag_numbers(State, MatchSpec) -> Fragnums when
      State :: term(),
      MatchSpec :: ets:match_spec(),
      Fragnums :: [integer()].
match_spec_to_frag_numbers(#hash_state{n_fragments = N} = State, MatchSpec) ->
    case MatchSpec of
	[{HeadPat, _, _}] when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
	    KeyPat = element(2, HeadPat),
	    case has_var(KeyPat) of
		false ->
		    [key_to_frag_number(State, KeyPat)];
		true ->
		    lists:seq(1, N)
	    end;
	_ -> 
	    lists:seq(1, N)
    end;
match_spec_to_frag_numbers(OldState, MatchSpec) ->
    State = convert_old_state(OldState),
    match_spec_to_frag_numbers(State, MatchSpec).

power2(Y) ->
    1 bsl Y. % trunc(math:pow(2, Y)).

%impl_doc_include

has_var(Pat) ->
    mnesia:has_var(Pat).
