-module(mnesia_frag_hash_test).

-export([test/0]).

-define(NUM_FRAGS, 20).
-define(NUM_KEYS, 10000).

-record(hash_state,
	{n_fragments,
	 next_n_to_split,
	 n_doubles,
	 function}).

% OLD mnesia_frag_hash:key_to_frag_number/2.
old_key_to_frag_number(#hash_state{function = phash, next_n_to_split = SplitN, n_doubles = L}, Key) ->
    P = SplitN,
    A = erlang:phash(Key, power2(L)),
    if
	A < P ->
	    erlang:phash(Key, power2(L + 1));
	true ->
	    A
    end;
old_key_to_frag_number(#hash_state{function = phash2, next_n_to_split = SplitN, n_doubles = L}, Key) ->
    P = SplitN,
    A = erlang:phash2(Key, power2(L)) + 1,
    if
	A < P ->
	    erlang:phash2(Key, power2(L + 1)) + 1;
	true ->
	    A
    end;
old_key_to_frag_number(OldState, Key) ->
    State = convert_old_state(OldState),
    old_key_to_frag_number(State, Key).


% NEW mnesia_frag_hash:key_to_frag_number/2.
new_key_to_frag_number(#hash_state{function = phash, n_fragments = N, n_doubles = L}, Key) ->
    A = erlang:phash(Key, power2(L + 1)),
    if
	A > N ->
	    A - power2(L);
	true ->
	    A
    end;
new_key_to_frag_number(#hash_state{function = phash2, n_fragments = N, n_doubles = L}, Key) ->
    A = erlang:phash2(Key, power2(L + 1)) + 1,
    if
	A > N ->
	    A - power2(L);
	true ->
	    A
    end;
new_key_to_frag_number(OldState, Key) ->
    State = convert_old_state(OldState),
    new_key_to_frag_number(State, Key).


% Helpers for key_to_frag_number functions.

power2(Y) ->
    1 bsl Y. % trunc(math:pow(2, Y)).

convert_old_state({hash_state, N, P, L}) ->
    #hash_state{n_fragments     = N,
		next_n_to_split = P,
		n_doubles       = L,
		function        = phash}.


test() ->
    test2(mnesia_frag_hash:init_state(undefined, undefined)), % phash2
    test2({hash_state, 1, 1, 0}). % phash

test2(I) ->
    test_keys(I),
    lists:foldl(
        fun(_, S) -> test_frag(S) end,
        I, lists:seq(1, ?NUM_FRAGS)),
    ok.

test_frag(State) ->
    {State2,_,_} = mnesia_frag_hash:add_frag(State),
    test_keys(State2),
    State2.

test_keys(State) ->
    [test_key(State, Key) || Key <- lists:seq(1, ?NUM_KEYS)].

test_key(State, Key) ->
    Old = old_key_to_frag_number(State, Key),
    New = new_key_to_frag_number(State, Key),
    Old = New.
