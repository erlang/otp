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
%%     $Id: mnesia_frag_hash.erl,v 1.1 2008/12/17 09:53:38 mikpe Exp $
%%%----------------------------------------------------------------------
%%% Purpose : Implements hashing functionality for fragmented tables
%%%----------------------------------------------------------------------

%header_doc_include
-module(mnesia_frag_hash).
-behaviour(mnesia_frag_hash).

%% Fragmented Table Hashing callback functions
-export([
	 init_state/2,
	 add_frag/1,
	 del_frag/1,
	 key_to_frag_number/2,
	 match_spec_to_frag_numbers/2
	]).

%header_doc_include

%impl_doc_include
-record(hash_state, {n_fragments, next_n_to_split, n_doubles}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(_Tab, State) when State == undefined ->
    #hash_state{n_fragments = 1,
		next_n_to_split = 1,
		n_doubles = 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_frag(State) when record(State, hash_state) ->
    SplitN = State#hash_state.next_n_to_split,
    P = SplitN + 1,
    L = State#hash_state.n_doubles,
    NewN = State#hash_state.n_fragments + 1,
    State2 = case trunc(math:pow(2, L)) + 1 of
		 P2 when P2 == P ->
		     State#hash_state{n_fragments = NewN,
				      n_doubles = L + 1,
				      next_n_to_split = 1};
		 _ ->
		     State#hash_state{n_fragments = NewN,
				      next_n_to_split = P}
	     end,
    {State2, [SplitN], [NewN]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

del_frag(State) when record(State, hash_state) ->
    P = State#hash_state.next_n_to_split - 1,
    L = State#hash_state.n_doubles,
    N = State#hash_state.n_fragments,
    if
	P < 1 ->
	    L2 = L - 1,
	    MergeN = trunc(math:pow(2, L2)),
	    State2 = State#hash_state{n_fragments = N - 1,
				      next_n_to_split = MergeN,
				      n_doubles = L2},
	    {State2, [N], [MergeN]};
	true ->
	    MergeN = P,
	    State2 = State#hash_state{n_fragments = N - 1,
				      next_n_to_split = MergeN},
	    {State2, [N], [MergeN]}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

key_to_frag_number(State, Key) when record(State, hash_state) ->
    L = State#hash_state.n_doubles,
    A = erlang:phash(Key, trunc(math:pow(2, L))),
    P = State#hash_state.next_n_to_split,
    if
	A < P ->
	    erlang:phash(Key, trunc(math:pow(2, L + 1)));
	true ->
	    A
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_spec_to_frag_numbers(State, MatchSpec) when record(State, hash_state) ->
    case MatchSpec of
	[{HeadPat, _, _}] when tuple(HeadPat), size(HeadPat) > 2 ->
	    KeyPat = element(2, HeadPat),
	    case has_var(KeyPat) of
		false ->
		    [key_to_frag_number(State, KeyPat)];
		true ->
		    lists:seq(1, State#hash_state.n_fragments)
	    end;
	_ ->
	    lists:seq(1, State#hash_state.n_fragments)
    end.

%impl_doc_include

has_var(Pat) ->
    mnesia:has_var(Pat).
