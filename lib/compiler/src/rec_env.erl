%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 1999-2004 Richard Carlsson
%% @doc Abstract environments, supporting self-referential bindings and
%% automatic new-key generation.

%% The current implementation is based on Erlang standard library
%% dictionaries.

%%% -define(DEBUG, true).

-module(rec_env).

-export([bind/3, bind_list/3, bind_recursive/4, delete/2, empty/0,
	 get/2, is_defined/2, is_empty/1, keys/1, lookup/2, new_key/1,
	 new_key/2, new_keys/2, new_keys/3, size/1, to_list/1]).

-export_type([environment/0]).

-ifdef(DEBUG).
-export([test/1, test_custom/1, test_custom/2]).
-endif.

-ifdef(DEBUG).
%% Code for testing:
%%@hidden
test(N) ->
    test_0(integer, N).

%%@hidden
test_custom(N) ->
    F = fun (X) -> list_to_atom("X"++integer_to_list(X)) end,
    test_custom(F, N).

%%@hidden
test_custom(F, N) ->
    test_0({custom, F}, N).

test_0(Type, N) ->
    put(new_key_calls, 0),
    put(new_key_retries, 0),
    put(new_key_max, 0),
    Env = test_1(Type, N, empty()),
    io:fwrite("\ncalls: ~w.\n", [get(new_key_calls)]),
    io:fwrite("\nretries: ~w.\n", [get(new_key_retries)]),
    io:fwrite("\nmax: ~w.\n", [get(new_key_max)]),
    dict:to_list(element(1,Env)).

test_1(integer = Type, N, Env) when is_integer(N), N > 0 ->
    Key = new_key(Env),
    test_1(Type, N - 1, bind(Key, value, Env));
test_1({custom, F} = Type, N, Env) when is_integer(N), N > 0 ->
    Key = new_key(F, Env),
    test_1(Type, N - 1, bind(Key, value, Env));
test_1(_,0, Env) ->
    Env.
-endif.


%% Representation:
%%
%%	environment() = [Mapping]
%%
%%      Mapping = {map, Dict} | {rec, Dict, Dict}
%%	Dict = dict:dictionary()
%%
%% An empty environment is a list containing a single `{map, Dict}'
%% element - empty lists are not valid environments. To find a key in an
%% environment, it is searched for in each mapping in the list, in
%% order, until it the key is found in some mapping, or the end of the
%% list is reached. In a 'rec' mapping, we keep the original dictionary
%% together with a version where entries may have been deleted - this
%% makes it possible to garbage collect the entire 'rec' mapping when
%% all its entries are unused (for example, by being shadowed by later
%% definitions).


%% =====================================================================
%% @type environment(). An abstract environment.

-type mapping()     :: {'map', dict()} | {'rec', dict(), dict()}.
-type environment() :: [mapping(),...].

%% =====================================================================
%% @spec empty() -> environment()
%%
%% @doc Returns an empty environment.

-spec empty() -> environment().

empty() ->
    [{map, dict:new()}].


%% =====================================================================
%% @spec is_empty(Env::environment()) -> boolean()
%%
%% @doc Returns <code>true</code> if the environment is empty, otherwise
%% <code>false</code>.

-spec is_empty(environment()) -> boolean().

is_empty([{map, Dict} | Es]) ->
    N = dict:size(Dict),
    if N =/= 0 -> false;
       Es =:= [] -> true;
       true -> is_empty(Es)
    end;
is_empty([{rec, Dict, _} | Es]) ->
    N = dict:size(Dict),
    if N =/= 0 -> false;
       Es =:= [] -> true;
       true -> is_empty(Es)
    end.


%% =====================================================================
%% @spec size(Env::environment()) -> integer()
%%
%% @doc Returns the number of entries in an environment.

%% (The name 'size' cannot be used in local calls, since there exists a
%% built-in function with the same name.)

-spec size(environment()) -> non_neg_integer().

size(Env) ->
    env_size(Env).

env_size([{map, Dict}]) ->
    dict:size(Dict);
env_size([{map, Dict} | Env]) ->
    dict:size(Dict) + env_size(Env);
env_size([{rec, Dict, _Dict0} | Env]) ->
    dict:size(Dict) + env_size(Env).


%% =====================================================================
%% @spec is_defined(Key, Env) -> boolean()
%%
%%	Key = term()
%%	Env = environment()
%%
%% @doc Returns <code>true</code> if <code>Key</code> is bound in the
%% environment, otherwise <code>false</code>.

-spec is_defined(term(), environment()) -> boolean().

is_defined(Key, [{map, Dict} | Env]) ->
    case dict:is_key(Key, Dict) of
	true ->
	    true;
	false when Env =:= [] ->
	    false;
	false ->
	    is_defined(Key, Env)
    end;
is_defined(Key, [{rec, Dict, _Dict0} | Env]) ->
    dict:is_key(Key, Dict) orelse is_defined(Key, Env).


%% =====================================================================
%% @spec keys(Env::environment()) -> [term()]
%%
%% @doc Returns the ordered list of all keys in the environment.

-spec keys(environment()) -> [term()].

keys(Env) ->
    lists:sort(keys(Env, [])).

keys([{map, Dict}], S) ->
    dict:fetch_keys(Dict) ++ S;
keys([{map, Dict} | Env], S) ->
    keys(Env, dict:fetch_keys(Dict) ++ S);
keys([{rec, Dict, _Dict0} | Env], S) ->
    keys(Env, dict:fetch_keys(Dict) ++ S).


%% =====================================================================
%% @spec to_list(Env) -> [{Key, Value}]
%%
%%	Env = environment()
%%	Key = term()
%%	Value = term()
%%
%% @doc Returns an ordered list of <code>{Key, Value}</code> pairs for
%% all keys in <code>Env</code>. <code>Value</code> is the same as that
%% returned by {@link get/2}.

-spec to_list(environment()) -> [{term(), term()}].

to_list(Env) ->
    lists:sort(to_list(Env, [])).

to_list([{map, Dict}], S) ->
    dict:to_list(Dict) ++ S;
to_list([{map, Dict} | Env], S) ->
    to_list(Env, dict:to_list(Dict) ++ S);
to_list([{rec, Dict, _Dict0} | Env], S) ->
    to_list(Env, dict:to_list(Dict) ++ S).


%% =====================================================================
%% @spec bind(Key, Value, Env) -> environment()
%%
%%	Key = term()
%%	Value = term()
%%	Env = environment()
%%
%% @doc Make a nonrecursive entry. This binds <code>Key</code> to
%% <code>Value</code>. If the key already existed in the environment,
%% the old entry is replaced.

%% Note that deletion is done to free old bindings so they can be
%% garbage collected.

-spec bind(term(), term(), environment()) -> environment().

bind(Key, Value, [{map, Dict}]) ->
    [{map, dict:store(Key, Value, Dict)}];
bind(Key, Value, [{map, Dict} | Env]) ->
    [{map, dict:store(Key, Value, Dict)} | delete_any(Key, Env)];
bind(Key, Value, Env) ->
    [{map, dict:store(Key, Value, dict:new())} | delete_any(Key, Env)].


%% =====================================================================
%% @spec bind_list(Keys, Values, Env) -> environment()
%%
%%	Keys = [term()]
%%	Values = [term()]
%%	Env = environment()
%%
%% @doc Make N nonrecursive entries. This binds each key in
%% <code>Keys</code> to the corresponding value in
%% <code>Values</code>. If some key already existed in the environment,
%% the previous entry is replaced. If <code>Keys</code> does not have
%% the same length as <code>Values</code>, an exception is generated.

-spec bind_list([term()], [term()], environment()) -> environment().

bind_list(Ks, Vs, [{map, Dict}]) ->
    [{map, store_list(Ks, Vs, Dict)}];
bind_list(Ks, Vs, [{map, Dict} | Env]) ->
    [{map, store_list(Ks, Vs, Dict)} | delete_list(Ks, Env)];
bind_list(Ks, Vs, Env) ->
    [{map, store_list(Ks, Vs, dict:new())} | delete_list(Ks, Env)].

store_list([K | Ks], [V | Vs], Dict) ->
    store_list(Ks, Vs, dict:store(K, V, Dict));
store_list([], _, Dict) ->
    Dict.

delete_list([K | Ks], Env) ->
    delete_list(Ks, delete_any(K, Env));
delete_list([], Env) ->
    Env.

%% By not calling `delete' unless we have to, we avoid unnecessary
%% rewriting of the data.

delete_any(Key, Env) ->
    case is_defined(Key, Env) of
	true ->
	    delete(Key, Env);
	false ->
	    Env
    end.

%% =====================================================================
%% @spec delete(Key, Env) -> environment()
%%
%%	Key = term()
%%	Env = environment()
%%
%% @doc Delete an entry. This removes <code>Key</code> from the
%% environment.

-spec delete(term(), environment()) -> environment().

delete(Key, [{map, Dict} = E | Env]) ->
    case dict:is_key(Key, Dict) of
	true ->
	    [{map, dict:erase(Key, Dict)} | Env];
	false ->
	    delete_1(Key, Env, E)
    end;
delete(Key, [{rec, Dict, Dict0} = E | Env]) ->
    case dict:is_key(Key, Dict) of
	true ->
	    %% The Dict0 component must be preserved as it is until all
	    %% keys in Dict have been deleted.
	    Dict1 = dict:erase(Key, Dict),
	    case dict:size(Dict1) of
		0 ->
		    Env;    % the whole {rec,...} is now garbage
		_ ->
		    [{rec, Dict1, Dict0} | Env]
	    end;
	false ->
	    [E | delete(Key, Env)]
    end.

%% This is just like above, except we pass on the preceding 'map'
%% mapping in the list to enable merging when removing 'rec' mappings.

delete_1(Key, [{rec, Dict, Dict0} = E | Env], E1) ->
    case dict:is_key(Key, Dict) of
	true ->
	    Dict1 = dict:erase(Key, Dict),
	    case dict:size(Dict1) of
		0 ->
		    concat(E1, Env);
		_ ->
		    [E1, {rec, Dict1, Dict0} | Env]
	    end;
	false ->
	    [E1, E | delete(Key, Env)]
    end.

concat({map, D1}, [{map, D2} | Env]) ->
    [dict:merge(fun (_K, V1, _V2) -> V1 end, D1, D2) | Env];
concat(E1, Env) ->
    [E1 | Env].


%% =====================================================================
%% @spec bind_recursive(Keys, Values, Fun, Env) -> NewEnv
%%
%%	Keys = [term()]
%%	Values = [term()]
%%	Fun = (Value, Env) -> term()
%%	Env = environment()
%%	NewEnv = environment()
%%
%% @doc Make N recursive entries. This binds each key in
%% <code>Keys</code> to the value of <code>Fun(Value, NewEnv)</code> for
%% the corresponding <code>Value</code>. If <code>Keys</code> does not
%% have the same length as <code>Values</code>, an exception is
%% generated. If some key already existed in the environment, the old
%% entry is replaced.
%%
%% <p>Note: the function <code>Fun</code> is evaluated each time one of
%% the stored keys is looked up, but only then.</p>
%%
%% <p>Examples:
%%<pre>
%%    NewEnv = bind_recursive([foo, bar], [1, 2],
%%	                      fun (V, E) -> V end,
%%	                      Env)</pre>
%%
%% This does nothing interesting; <code>get(foo, NewEnv)</code> yields
%% <code>1</code> and <code>get(bar, NewEnv)</code> yields
%% <code>2</code>, but there is more overhead than if the {@link
%% bind_list/3} function had been used.
%%
%% <pre>
%%    NewEnv = bind_recursive([foo, bar], [1, 2],
%%                            fun (V, E) -> {V, E} end,
%%                            Env)</pre>
%%
%% Here, however, <code>get(foo, NewEnv)</code> will yield <code>{1,
%% NewEnv}</code> and <code>get(bar, NewEnv)</code> will yield <code>{2,
%% NewEnv}</code>, i.e., the environment <code>NewEnv</code> contains
%% recursive bindings.</p>

-spec bind_recursive([term()], [term()],
		     fun((term(), environment()) -> term()),
		     environment()) -> environment().

bind_recursive([], [], _, Env) ->
    Env;
bind_recursive(Ks, Vs, F, Env) ->
    F1 = fun (V) ->
		 fun (Dict) -> F(V, [{rec, Dict, Dict} | Env]) end
	 end,
    Dict = bind_recursive_1(Ks, Vs, F1, dict:new()),
    [{rec, Dict, Dict} | Env].

bind_recursive_1([K | Ks], [V | Vs], F, Dict) ->
    bind_recursive_1(Ks, Vs, F, dict:store(K, F(V), Dict));
bind_recursive_1([], [], _, Dict) ->
    Dict.


%% =====================================================================
%% @spec lookup(Key, Env) -> error | {ok, Value}
%%
%%	Key = term()
%%	Env = environment()
%%	Value = term()
%%
%% @doc Returns <code>{ok, Value}</code> if <code>Key</code> is bound to
%% <code>Value</code> in <code>Env</code>, and <code>error</code>
%% otherwise.

-spec lookup(term(), environment()) -> 'error' | {'ok', term()}.

lookup(Key, [{map, Dict} | Env]) ->
    case dict:find(Key, Dict) of
	{ok, _}=Value ->
	    Value;
	error when Env =:= [] ->
	    error;
	error ->
	    lookup(Key, Env)
    end;
lookup(Key, [{rec, Dict, Dict0} | Env]) ->
    case dict:find(Key, Dict) of
	{ok, F} ->
	    {ok, F(Dict0)};
	error ->
	    lookup(Key, Env)
    end.


%% =====================================================================
%% @spec get(Key, Env) -> Value
%%
%%	Key = term()
%%	Env = environment()
%%	Value = term()
%%
%% @doc Returns the value that <code>Key</code> is bound to in
%% <code>Env</code>. Throws <code>{undefined, Key}</code> if the key
%% does not exist in <code>Env</code>.

-spec get(term(), environment()) -> term().

get(Key, Env) ->
    case lookup(Key, Env) of
	{ok, Value} -> Value;
	error -> throw({undefined, Key})
    end.


%% =====================================================================
%% The key-generating algorithm could possibly be further improved. The
%% important thing to keep in mind is, that when we need a new key, we
%% are generally in mid-traversal of a syntax tree, and existing names
%% in the tree may be closely grouped and evenly distributed or even
%% forming a compact range (often having been generated by a "gensym",
%% or by this very algorithm itself). This means that if we generate an
%% identifier whose value is too close to those already seen (i.e.,
%% which are in the environment), it is very probable that we will
%% shadow a not-yet-seen identifier further down in the tree, the result
%% being that we induce another later renaming, and end up renaming most
%% of the identifiers, completely contrary to our intention. We need to
%% generate new identifiers in a way that avoids such systematic
%% collisions.
%%
%% One way of getting a new key to try when the previous attempt failed
%% is of course to e.g. add one to the last tried value. However, in
%% general it's a bad idea to try adjacent identifiers: the percentage
%% of retries will typically increase a lot, so you may lose big on the
%% extra lookups while gaining only a little from the quicker
%% computation.
%%
%% We want an initial range that is large enough for most typical cases.
%% If we start with, say, a range of 10, we might quickly use up most of
%% the values in the range 1-10 (or 1-100) for new top-level variables -
%% but as we start traversing the syntax tree, it is quite likely that
%% exactly those variables will be encountered again (this depends on
%% how the names in the tree were created), and will then need to be
%% renamed. If we instead begin with a larger range, it is less likely
%% that any top-level names that we introduce will shadow names that we
%% will find in the tree. Of course we cannot know how large is large
%% enough: for any initial range, there is some syntax tree that uses
%% all the values in that range, and thus any top-level names introduced
%% will shadow names in the tree. The point is to avoid this happening
%% all the time - a range of about 1000 seems enough for most programs.
%%
%% The following values have been shown to work well:

-define(MINIMUM_RANGE, 1000).
-define(START_RANGE_FACTOR, 50).
-define(MAX_RETRIES, 2).      % retries before enlarging range
-define(ENLARGE_FACTOR, 10).  % range enlargment factor

-ifdef(DEBUG).
%% If you want to use these process dictionary counters, make sure to
%% initialise them to zero before you call any of the key-generating
%% functions.
%%
%%	new_key_calls		total number of calls
%%	new_key_retries		failed key generation attempts
%%	new_key_max		maximum generated integer value
%%
-define(measure_calls(),
	put(new_key_calls, 1 + get(new_key_calls))).
-define(measure_max_key(N),
	case N > get(new_key_max) of
	    true ->
		put(new_key_max, N);
	    false ->
		ok
	end).
-define(measure_retries(N),
	put(new_key_retries, get(new_key_retries) + N)).
-else.
-define(measure_calls(), ok).
-define(measure_max_key(N), ok).
-define(measure_retries(N), ok).
-endif.


%% =====================================================================
%% @spec new_key(Env::environment()) -> integer()
%%
%% @doc Returns an integer which is not already used as key in the
%% environment. New integers are generated using an algorithm which
%% tries to keep the values randomly distributed within a reasonably
%% small range relative to the number of entries in the environment.
%%
%% <p>This function uses the Erlang standard library module
%% <code>random</code> to generate new keys.</p>
%%
%% <p>Note that only the new key is returned; the environment itself is
%% not updated by this function.</p>

-spec new_key(environment()) -> integer().

new_key(Env) ->
    new_key(fun (X) -> X end, Env).


%% =====================================================================
%% @spec new_key(Function, Env) -> term()
%%
%%	Function = (integer()) -> term()
%%	Env = environment()
%%
%% @doc Returns a term which is not already used as key in the
%% environment. The term is generated by applying <code>Function</code>
%% to an integer generated as in {@link new_key/1}.
%%
%% <p>Note that only the generated term is returned; the environment
%% itself is not updated by this function.</p>

-spec new_key(fun((integer()) -> term()), environment()) -> term().

new_key(F, Env) ->
    ?measure_calls(),
    R = start_range(Env),
    %% io:fwrite("Start range: ~w.\n", [R]),
    new_key(R, F, Env).

new_key(R, F, Env) ->
    new_key(generate(R, R), R, 0, F, Env).

new_key(N, R, T, F, Env) when T < ?MAX_RETRIES ->
    A = F(N),
    case is_defined(A, Env) of
	true ->
	    %% io:fwrite("CLASH: ~w.\n", [A]),
	    new_key(generate(N, R), R, T + 1, F, Env);
	false ->
	    ?measure_max_key(N),
	    ?measure_retries(T),
	    %% io:fwrite("New: ~w.\n", [N]),
	    A
    end;
new_key(N, R, _T, F, Env) ->
    %% Too many retries - enlarge the range and start over.
    ?measure_retries((_T + 1)),
    R1 = trunc(R * ?ENLARGE_FACTOR),
    %% io:fwrite("**NEW RANGE**: ~w.\n", [R1]),
    new_key(generate(N, R1), R1, 0, F, Env).

start_range(Env) ->
    erlang:max(env_size(Env) * ?START_RANGE_FACTOR, ?MINIMUM_RANGE).

%% The previous key might or might not be used to compute the next key
%% to be tried. It is currently not used.
%%
%% In order to avoid causing cascading renamings, it is important that
%% this function does not generate values in order, but
%% (pseudo-)randomly distributed over the range.

generate(_N, Range) ->
    random:uniform(Range).    % works well


%% =====================================================================
%% @spec new_keys(N, Env) -> [integer()]
%%
%%	N = integer()
%%	Env = environment()
%%
%% @doc Returns a list of <code>N</code> distinct integers that are not
%% already used as keys in the environment. See {@link new_key/1} for
%% details.

-spec new_keys(integer(), environment()) -> [integer()].

new_keys(N, Env) when is_integer(N) ->
    new_keys(N, fun (X) -> X end, Env).

    
%% =====================================================================
%% @spec new_keys(N, Function, Env) -> [term()]
%%
%%	    N = integer()
%%	    Function = (integer()) -> term()
%%	    Env = environment()
%%
%% @doc Returns a list of <code>N</code> distinct terms that are not
%% already used as keys in the environment. See {@link new_key/3} for
%% details.

-spec new_keys(integer(), fun((integer()) -> term()), environment()) -> [term()].

new_keys(N, F, Env) when is_integer(N) ->
    R = start_range(Env),
    new_keys(N, [], R, F, Env).

new_keys(N, Ks, R, F, Env) when N > 0 ->
    Key = new_key(R, F, Env),
    Env1 = bind(Key, true, Env),    % dummy binding
    new_keys(N - 1, [Key | Ks], R, F, Env1);
new_keys(0, Ks, _, _, _) ->
    Ks.
