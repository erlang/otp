%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2000-2003 Richard Carlsson
%% Copyright Ericsson AB 2000-2025. All Rights Reserved.
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
-module(proplists).
-moduledoc(#{ authors => [~"Richard Carlsson <carlsson.richard@gmail.com>"] }).
-moduledoc """
Support functions for property lists.

Property lists are ordinary lists containing entries in the form of either
tuples, whose first elements are keys used for lookup and insertion, or atoms,
which work as shorthand for tuples `{Atom, true}`. (Other terms are allowed in
the lists, but are ignored by this module.) If there is more than one entry in a
list for a certain key, the first occurrence normally overrides any later
(irrespective of the arity of the tuples).

Property lists are useful for representing inherited properties, such as options
passed to a function where a user can specify options overriding the default
settings, object properties, annotations, and so on.

Two keys are considered equal if they match (`=:=`). That is, numbers are
compared literally rather than by value, so that, for example, `1` and `1.0` are
different keys.
""".

-export([property/1, property/2, unfold/1, compact/1, lookup/2,
	 lookup_all/2, is_defined/2, get_value/2, get_value/3,
	 get_all_values/2, append_values/2, get_bool/2, get_keys/1,
	 delete/2, substitute_aliases/2, substitute_negations/2,
	 expand/2, normalize/2, split/2, from_map/1, to_map/1,
	 to_map/2]).

%% ---------------------------------------------------------------------

-export_type([property/0, proplist/0]).

-doc "A property item within a list".
-type property()  :: atom() | tuple().
-doc "A list of `t:property/0`, also knows as a proplist.".
-type proplist()  :: [property()].

%% ---------------------------------------------------------------------

-doc """
Creates a normal form (minimal) representation of a property. If `PropertyIn` is
`{Key, true}`, where `Key` is an atom, `Key` is returned, otherwise the whole
term `PropertyIn` is returned.

See also `property/2`.
""".
-spec property(PropertyIn) -> PropertyOut when
      PropertyIn :: property(),
      PropertyOut :: property().

property({Key, true}) when is_atom(Key) ->
    Key;
property(Property) ->
    Property.


-doc """
Creates a normal form (minimal) representation of a simple key/value property.
Returns `Key` if `Value` is `true` and `Key` is an atom, otherwise a tuple
`{Key, Value}` is returned.

See also `property/1`.
""".
-spec property(Key, Value) -> Property when
      Key :: term(),
      Value :: term(),
      Property :: atom() | {term(), term()}.

property(Key, true) when is_atom(Key) ->
    Key;
property(Key, Value) ->
    {Key, Value}.


%% ---------------------------------------------------------------------

-doc """
Unfolds all occurrences of atoms in `ListIn` to tuples `{Atom, true}`.

See also `compact/1`.
""".
-spec unfold(ListIn) -> ListOut when
      ListIn :: [term()],
      ListOut :: [term()].

unfold([P | Ps]) ->
    if is_atom(P) ->
	    [{P, true} | unfold(Ps)];
       true ->
	    [P | unfold(Ps)]
    end;
unfold([]) ->
    [].

-doc """
Minimizes the representation of all entries in the list. This is equivalent to
`[property(P) || P <- ListIn]`.

See also `property/1`, `unfold/1`.
""".
-spec compact(ListIn) -> ListOut when
      ListIn :: [property()],
      ListOut :: [property()].

compact(ListIn) ->
    [property(P) || P <- ListIn].


%% ---------------------------------------------------------------------

-doc """
Returns the first entry associated with `Key` in `List`, if one exists,
otherwise returns `none`. For an atom `A` in the list, the tuple `{A, true}` is
the entry associated with `A`.

See also `get_bool/2`, `get_value/2`, `lookup_all/2`.
""".
-spec lookup(Key, List) -> 'none' | tuple() when
      Key :: term(),
      List :: [term()].

lookup(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    {Key, true};
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    %% Note that <code>Key</code> does not have to be an atom in this case.
	    P;
       true ->
	    lookup(Key, Ps)
    end;
lookup(_Key, []) ->
    none.

-doc """
Returns the list of all entries associated with `Key` in `List`. If no such
entry exists, the result is the empty list.

See also `lookup/2`.
""".
-spec lookup_all(Key, List) -> [tuple()] when
      Key :: term(),
      List :: [term()].

lookup_all(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    [{Key, true} | lookup_all(Key, Ps)];
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    [P | lookup_all(Key, Ps)];
       true ->
	    lookup_all(Key, Ps)
    end;
lookup_all(_Key, []) ->
    [].


%% ---------------------------------------------------------------------

-doc """
Returns `true` if `List` contains at least one entry associated with `Key`,
otherwise `false`.
""".
-spec is_defined(Key, List) -> boolean() when
      Key :: term(),
      List :: [term()].

is_defined(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    true;
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    true;
       true ->
	    is_defined(Key, Ps)
    end;
is_defined(_Key, []) ->
    false.


%% ---------------------------------------------------------------------

-doc(#{ equiv => get_value(Key, List, undefined) }).
-spec get_value(Key, List) -> term() when
      Key :: term(),
      List :: [term()].

get_value(Key, List) ->
    get_value(Key, List, undefined).

-doc """
Returns the value of a simple key/value property in `List`. If
[`lookup(Key, List)`](`lookup/2`) would yield `{Key, Value}`, this function
returns the corresponding `Value`, otherwise `Default`.

See also `get_all_values/2`, `get_bool/2`, `get_value/2`, `lookup/2`.
""".
-spec get_value(Key, List, Default) -> term() when
      Key :: term(),
      List :: [term()],
      Default :: term().

get_value(Key, [P | Ps], Default) ->
    if is_atom(P), P =:= Key ->
	    true;
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} ->
		    Value;
		_ ->
		    %% Don't continue the search!
		    Default
	    end;
       true ->
	    get_value(Key, Ps, Default)
    end;
get_value(_Key, [], Default) ->
    Default.

-doc """
Similar to `get_value/2`, but returns the list of values for _all_ entries
`{Key, Value}` in `List`. If no such entry exists, the result is the empty list.
""".
-spec get_all_values(Key, List) -> [term()] when
      Key :: term(),
      List :: [term()].

get_all_values(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    [true | get_all_values(Key, Ps)];
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} ->
		    [Value | get_all_values(Key, Ps)];
		_ ->
		    get_all_values(Key, Ps)
	    end;
       true ->
	    get_all_values(Key, Ps)
    end;
get_all_values(_Key, []) ->
    [].

-doc """
Similar to `get_all_values/2`, but each value is wrapped in a list unless it is
already itself a list. The resulting list of lists is concatenated. This is
often useful for "incremental" options.

_Example:_

```erlang
append_values(a, [{a, [1,2]}, {b, 0}, {a, 3}, {c, -1}, {a, [4]}])
```

returns:

```erlang
[1,2,3,4]
```
""".
-spec append_values(Key, ListIn) -> ListOut when
      Key :: term(),
      ListIn :: [term()],
      ListOut :: [term()].

append_values(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    [true | append_values(Key, Ps)];
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, Value} when is_list(Value) ->
		    Value ++ append_values(Key, Ps);
		{_, Value} ->
		    [Value | append_values(Key, Ps)];
		_ ->
		    append_values(Key, Ps)
	    end;
       true ->
	    append_values(Key, Ps)
    end;
append_values(_Key, []) ->
    [].


%% ---------------------------------------------------------------------

-doc """
Returns the value of a boolean key/value option. If
[`lookup(Key, List)`](`lookup/2`) would yield `{Key, true}`, this function
returns `true`, otherwise `false`.

See also `get_value/2`, `lookup/2`.
""".
-spec get_bool(Key, List) -> boolean() when
      Key :: term(),
      List :: [term()].

get_bool(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    true;
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, true} ->
		    true;
		_ ->
		    %% Don't continue the search!
		    false
	    end;
       true ->
	    get_bool(Key, Ps)
    end;
get_bool(_Key, []) ->
    false.


%% ---------------------------------------------------------------------

-doc "Returns an unordered list of the keys used in `List`, not containing duplicates.".
-spec get_keys(List) -> [term()] when
      List :: [term()].

get_keys(Ps) ->
    sets:to_list(get_keys(Ps, sets:new())).

get_keys([P | Ps], Keys) ->
    if is_atom(P) ->
	    get_keys(Ps, sets:add_element(P, Keys));
       tuple_size(P) >= 1 ->
	    get_keys(Ps, sets:add_element(element(1, P), Keys));
       true ->
	    get_keys(Ps, Keys)
    end;
get_keys([], Keys) ->
    Keys.


%% ---------------------------------------------------------------------

-doc "Deletes all entries associated with `Key` from `List`.".
-spec delete(Key, List) -> List when
      Key :: term(),
      List :: [term()].

delete(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    delete(Key, Ps);
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    delete(Key, Ps);
       true ->
	    [P | delete(Key, Ps)]
    end;
delete(_, []) ->
    [].


%% ---------------------------------------------------------------------

-doc """
Substitutes keys of properties. For each entry in `ListIn`, if it is associated
with some key `K1` such that `{K1, K2}` occurs in `Aliases`, the key of the
entry is changed to `K2`. If the same `K1` occurs more than once in `Aliases`,
only the first occurrence is used.

For example,
[`substitute_aliases([{color, colour}], L)`](`substitute_aliases/2`) replaces
all tuples `{color, ...}` in `L` with `{colour, ...}`, and all atoms `color`
with `colour`.

See also `normalize/2`, `substitute_negations/2`.
""".
-spec substitute_aliases(Aliases, ListIn) -> ListOut when
      Aliases :: [{Key, Key}],
      Key :: term(),
      ListIn :: [term()],
      ListOut :: [term()].

substitute_aliases(As, Props) ->
    [substitute_aliases_1(As, P) || P <- Props].

substitute_aliases_1([{Key, Key1} | As], P) ->
    if is_atom(P), P =:= Key ->
	    property(Key1, true);
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    property(setelement(1, P, Key1));
       true ->
	    substitute_aliases_1(As, P)
    end;
substitute_aliases_1([], P) ->
    P.


%% ---------------------------------------------------------------------

-doc """
Substitutes keys of boolean-valued properties and simultaneously negates their
values.

For each entry in `ListIn`, if it is associated with some key `K1` such
that `{K1, K2}` occurs in `Negations`: if the entry was `{K1, true}`, it is
replaced with `{K2, false}`, otherwise with `K2`, thus changing the name of the
option and simultaneously negating the value specified by
[`get_bool(Key, ListIn)`](`get_bool/2`). If the same `K1` occurs more than once
in `Negations`, only the first occurrence is used.

For example,
[`substitute_negations([{no_foo, foo}], L)`](`substitute_negations/2`) replaces
any atom `no_foo` or tuple `{no_foo, true}` in `L` with `{foo, false}`, and any
other tuple `{no_foo, ...}` with `foo`.

See also `get_bool/2`, `normalize/2`, `substitute_aliases/2`.
""".
-spec substitute_negations(Negations, ListIn) -> ListOut when
      Negations :: [{Key1, Key2}],
      Key1 :: term(),
      Key2 :: term(),
      ListIn :: [term()],
      ListOut :: [term()].

substitute_negations(As, Props) ->
    [substitute_negations_1(As, P) || P <- Props].

substitute_negations_1([{Key, Key1} | As], P) ->
    if is_atom(P), P =:= Key ->
	    property(Key1, false);
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    case P of
		{_, true} ->
		    property(Key1, false);
		{_, false} ->
		    property(Key1, true);
		_ ->
		    %% The property is supposed to be a boolean, so any
		    %% other tuple is interpreted as `false', as done in
		    %% `get_bool'.
		    property(Key1, true)
	    end;		    
       true ->
	    substitute_negations_1(As, P)
    end;
substitute_negations_1([], P) ->
    P.


%% ---------------------------------------------------------------------

-doc """
Expands particular properties to corresponding sets of properties (or other
terms).

For each pair `{Property, Expansion}` in `Expansions`: if `E` is the
first entry in `ListIn` with the same key as `Property`, and `E` and `Property`
have equivalent normal forms, then `E` is replaced with the terms in
`Expansion`, and any following entries with the same key are deleted from
`ListIn`.

For example, the following expressions all return `[fie, bar, baz, fum]`:

```erlang
expand([{foo, [bar, baz]}], [fie, foo, fum])
expand([{{foo, true}, [bar, baz]}], [fie, foo, fum])
expand([{{foo, false}, [bar, baz]}], [fie, {foo, false}, fum])
```

However, no expansion is done in the following call because `{foo, false}`
shadows `foo`:

```erlang
expand([{{foo, true}, [bar, baz]}], [{foo, false}, fie, foo, fum])
```

Notice that if the original property term is to be preserved in the result when
expanded, it must be included in the expansion list. The inserted terms are not
expanded recursively. If `Expansions` contains more than one property with the
same key, only the first occurrence is used.

See also `normalize/2`.
""".
-spec expand(Expansions, ListIn) -> ListOut when
      Expansions :: [{Property :: property(), Expansion :: [term()]}],
      ListIn :: [term()],
      ListOut :: [term()].

expand(Es, Ps) when is_list(Ps) ->
    Es1 = [{property(P), V} || {P, V} <- Es],
    flatten(expand_0(key_uniq(Es1), Ps)).

%% Here, all key properties are normalized and there are no multiple
%% entries in the list of expansions for any specific key property. We
%% insert the expansions one at a time - this is quadratic, but gives
%% the desired behaviour in a simple way.

expand_0([{P, L} | Es], Ps) ->
    expand_0(Es, expand_1(P, L, Ps));
expand_0([], Ps) ->
    Ps.

expand_1(P, L, Ps) ->
    %% First, we must find out what key to look for.
    %% P has a minimal representation here.
    if is_atom(P) ->
	    expand_2(P, P, L, Ps);
       tuple_size(P) >= 1 ->
	    expand_2(element(1, P), P, L, Ps);
       true ->
	    Ps    % refuse to expand non-property
    end.

expand_2(Key, P1, L, [P | Ps]) ->
    if is_atom(P), P =:= Key ->
	    expand_3(Key, P1, P, L, Ps);
       tuple_size(P) >= 1, element(1, P) =:= Key ->
	    expand_3(Key, P1, property(P), L, Ps);
       true ->
	    %% This case handles non-property entries, and thus
	    %% any already inserted expansions (lists), by simply
	    %% ignoring them.
	    [P | expand_2(Key, P1, L, Ps)]
    end;
expand_2(_, _, _, []) ->
    [].

expand_3(Key, P1, P, L, Ps) ->
    %% Here, we have found the first entry with a matching key. Both P
    %% and P1 have minimal representations here. The inserted list will
    %% be flattened afterwards. If the expansion is done, we drop the
    %% found entry and alao delete any later entries with the same key.
    if P1 =:= P ->
	    [L | delete(Key, Ps)];
       true ->
	    %% The existing entry does not match - keep it.
	    [P | Ps]
    end.

key_uniq([{K, V} | Ps]) ->
    [{K, V} | key_uniq_1(K, Ps)];
key_uniq([]) ->
    [].

key_uniq_1(K, [{K1, V} | Ps]) ->
    if K =:= K1 ->
	    key_uniq_1(K, Ps);
       true ->
	    [{K1, V} | key_uniq_1(K1, Ps)]
    end;
key_uniq_1(_, []) ->
    [].

%% This does top-level flattening only.

flatten([E | Es]) when is_list(E) ->
    E ++ flatten(Es);
flatten([E | Es]) ->
    [E | flatten(Es)];
flatten([]) ->
    [].


%% ---------------------------------------------------------------------

-doc """
Passes `ListIn` through a sequence of substitution/expansion stages. For an
`aliases` operation, function `substitute_aliases/2` is applied using the
specified list of aliases:

- For a `negations` operation,
  [`substitute_negations/2`](`substitute_negations/2`) is applied using the
  specified negation list.
- For an `expand` operation, function `expand/2` is applied using the specified
  list of expansions.

The final result is automatically compacted (compare `compact/1`).

Typically you want to substitute negations first, then aliases, then perform one
or more expansions (sometimes you want to pre-expand particular entries before
doing the main expansion). You might want to substitute negations and/or aliases
repeatedly, to allow such forms in the right-hand side of aliases and expansion
lists.

See also `substitute_negations/2`.
""".
-spec normalize(ListIn, Stages) -> ListOut when
      ListIn :: [term()],
      Stages :: [Operation],
      Operation :: {'aliases', Aliases}
                 | {'negations', Negations}
                 | {'expand', Expansions},
      Aliases :: [{Key, Key}],
      Negations :: [{Key, Key}],
      Expansions :: [{Property :: property(), Expansion :: [term()]}],
      ListOut :: [term()].

normalize(L, Stages) ->
    compact(apply_stages(L, Stages)).

apply_stages(L, [{aliases, As} | Xs]) ->
    apply_stages(substitute_aliases(As, L), Xs);
apply_stages(L, [{expand, Es} | Xs]) ->
    apply_stages(expand(Es, L), Xs);
apply_stages(L, [{negations, Ns} | Xs]) ->
    apply_stages(substitute_negations(Ns, L), Xs);
apply_stages(L, []) ->
    L.

%% ---------------------------------------------------------------------

-doc """
Partitions `List` into a list of sublists and a remainder.

`Lists` contains one sublist for each key in `Keys`, in the corresponding order.
The relative order of the elements in each sublist is preserved from the original `List`.
`Rest` contains the elements in `List` that are not associated with any of the
specified keys, also with their original relative order preserved.

_Example:_

```erlang
split([{c, 2}, {e, 1}, a, {c, 3, 4}, d, {b, 5}, b], [a, b, c])
```

returns:

```erlang
{[[a], [{b, 5}, b],[{c, 2}, {c, 3, 4}]], [{e, 1}, d]}
```
""".
-spec split(List, Keys) -> {Lists, Rest} when
      List :: [term()],
      Keys :: [term()],
      Lists :: [[term()]],
      Rest :: [term()].

split(List, Keys) ->
    {Store, Rest} = split(List, #{K => [] || K <- Keys}, []),
    {[lists:reverse(map_get(K, Store)) || K <- Keys],
     lists:reverse(Rest)}.

split([P | Ps], Store, Rest) ->
    if is_atom(P) ->
	    case is_map_key(P, Store) of
		true ->
		    split(Ps, maps_prepend(P, P, Store), Rest);
		false ->
		    split(Ps, Store, [P | Rest])
	    end;
       tuple_size(P) >= 1 ->
	    %% Note that Key does not have to be an atom in this case.
	    Key = element(1, P),
	    case is_map_key(Key, Store) of
		true ->
		    split(Ps, maps_prepend(Key, P, Store), Rest);
		false ->
		    split(Ps, Store, [P | Rest])
	    end;
       true ->
	    split(Ps, Store, [P | Rest])
    end;
split([], Store, Rest) ->
    {Store, Rest}.

maps_prepend(Key, Val, Dict) ->
    Dict#{Key := [Val | map_get(Key, Dict)]}.

%% ---------------------------------------------------------------------

-doc """
Converts the property list `List` to a map.

Shorthand atom values in `List` will be expanded to an association of the form
`Atom => true`. Tuples of the form `{Key, Value}` in `List` will be converted to
an association of the form `Key => Value`. Anything else will be silently
ignored.

If the same key appears in `List` multiple times, the value of the one appearing
nearest to the head of `List` will be in the result map, that is the value that
would be returned by a call to [`get_value(Key, List)`](`get_value/2`).

_Example:_

```erlang
to_map([a, {b, 1}, {c, 2}, {c, 3}])
```

returns:

```erlang
#{a => true, b => 1, c => 2}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec to_map(List) -> Map when
      List :: [Shorthand | {Key, Value} | term()],
      Map :: #{Shorthand => 'true', Key => Value},
      Shorthand :: atom(),
      Key :: term(),
      Value :: term().

to_map(List) ->
    lists:foldr(
        fun
            ({K, V}, M) ->
                M#{K => V};
            %% if tuples with arity /= 2 appear before atoms or
            %% tuples with arity == 2, get_value/2,3 returns early
            (T, M) when 1 =< tuple_size(T) ->
                maps:remove(element(1, T), M);
            (K, M) when is_atom(K) ->
                M#{K => true};
            (_, M) ->
                M
        end,
        #{},
        List
    ).

-doc """
Converts the property list `List` to a map after applying the normalizations
given in `Stages`.

See also `normalize/2`, `to_map/1`.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec to_map(List, Stages) -> Map when
      List :: [term()],
      Stages :: [Operation],
      Operation :: {'aliases', Aliases}
                 | {'negations', Negations}
                 | {'expand', Expansions},
      Aliases :: [{Key, Key}],
      Negations :: [{Key, Key}],
      Expansions :: [{Property :: property(), Expansion :: [term()]}],
      Map :: #{term() => term()}.

to_map(List, Stages) ->
    to_map(apply_stages(List, Stages)).

-doc "Converts the map `Map` to a property list.".
-doc(#{since => <<"OTP 24.0">>}).
-spec from_map(Map) -> List when
    Map :: #{Key => Value},
    List :: [{Key, Value}],
    Key :: term(),
    Value :: term().

from_map(Map) ->
    maps:to_list(Map).
