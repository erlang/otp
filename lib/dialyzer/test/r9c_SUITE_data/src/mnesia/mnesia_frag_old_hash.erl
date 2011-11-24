%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_frag_old_hash.erl,v 1.1 2008/12/17 09:53:38 mikpe Exp $
%%
%%%----------------------------------------------------------------------
%%% Purpose : Implements hashing functionality for fragmented tables
%%%----------------------------------------------------------------------

-module(mnesia_frag_old_hash).
-behaviour(mnesia_frag_hash).

%% Hashing callback functions
-export([
	 init_state/2,
	 add_frag/1,
	 del_frag/1,
	 key_to_frag_number/2,
	 match_spec_to_frag_numbers/2
	]).

-record(old_hash_state,
	{n_fragments,
	 next_n_to_split,
	 n_doubles}).

%% Old style. Kept for backwards compatibility.
-record(frag_hash,
	{foreign_key,
	 n_fragments,
	 next_n_to_split,
	 n_doubles}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_state(_Tab, InitialState) when InitialState == undefined ->
    #old_hash_state{n_fragments     = 1,
		    next_n_to_split = 1,
		    n_doubles       = 0};
init_state(_Tab, FH) when record(FH, frag_hash) ->
    %% Old style. Kept for backwards compatibility.
    #old_hash_state{n_fragments     = FH#frag_hash.n_fragments,
		    next_n_to_split = FH#frag_hash.next_n_to_split,
		    n_doubles       = FH#frag_hash.n_doubles}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_frag(State) when record(State, old_hash_state) ->
    SplitN = State#old_hash_state.next_n_to_split,
    P = SplitN + 1,
    L = State#old_hash_state.n_doubles,
    NewN = State#old_hash_state.n_fragments + 1,
    State2 = case trunc(math:pow(2, L)) + 1 of
		 P2 when P2 == P ->
		     State#old_hash_state{n_fragments = NewN,
					  next_n_to_split = 1,
					  n_doubles = L + 1};
		 _ ->
		     State#old_hash_state{n_fragments = NewN,
					  next_n_to_split = P}
	     end,
    {State2, [SplitN], [NewN]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

del_frag(State) when record(State, old_hash_state) ->
    P = State#old_hash_state.next_n_to_split - 1,
    L = State#old_hash_state.n_doubles,
    N = State#old_hash_state.n_fragments,
    if
	P < 1 ->
	    L2 = L - 1,
	    MergeN = trunc(math:pow(2, L2)),
	    State2 = State#old_hash_state{n_fragments = N - 1,
					  next_n_to_split = MergeN,
					  n_doubles = L2},
	    {State2, [N], [MergeN]};
	true ->
	    MergeN = P,
	    State2 = State#old_hash_state{n_fragments = N - 1,
					  next_n_to_split = MergeN},
	    {State2, [N], [MergeN]}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

key_to_frag_number(State, Key) when record(State, old_hash_state) ->
    L = State#old_hash_state.n_doubles,
    A = erlang:hash(Key, trunc(math:pow(2, L))),
    P = State#old_hash_state.next_n_to_split,
    if
	A < P ->
	    erlang:hash(Key, trunc(math:pow(2, L + 1)));
	true ->
	    A
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_spec_to_frag_numbers(State, MatchSpec) when record(State, old_hash_state) ->
    case MatchSpec of
	[{HeadPat, _, _}] when tuple(HeadPat), size(HeadPat) > 2 ->
	    KeyPat = element(2, HeadPat),
	    case has_var(KeyPat) of
		false ->
		    [key_to_frag_number(State, KeyPat)];
		true ->
		    lists:seq(1, State#old_hash_state.n_fragments)
	    end;
	_ ->
	    lists:seq(1, State#old_hash_state.n_fragments)
    end.

has_var(Pat) ->
    mnesia:has_var(Pat).
