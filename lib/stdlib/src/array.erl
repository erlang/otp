%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2006-2016 Richard Carlsson
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
-module(array).
-moduledoc """
Functional, extendible arrays.

Arrays can have fixed size, or can grow automatically as needed. A default value
is used for entries that have not been explicitly set.

Arrays uses _zero_-based indexing. This is a deliberate design choice and
differs from other Erlang data structures, for example, tuples.

Unless specified by the user when the array is created, the default value is the
atom `undefined`. There is no difference between an unset entry and an entry
that has been explicitly set to the same value as the default one (compare
`reset/2`). If you need to differentiate between unset and set entries, ensure
that the default value cannot be confused with the values of set entries.

The array never shrinks automatically. If an index `I` has been used to set an
entry successfully, all indices in the range `[0,I]` stay accessible unless the
array size is explicitly changed by calling `resize/2`.

_Examples:_

Create a fixed-size array with entries 0-9 set to `undefined`:

```
A0 = array:new(10).
10 = array:size(A0).
```

Create an extendible array and set entry 17 to `true`, causing the array to grow
automatically:

```
A1 = array:set(17, true, array:new()).
18 = array:size(A1).
```

Read back a stored value:

```
true = array:get(17, A1).
```

Accessing an unset entry returns default value:

```
undefined = array:get(3, A1)
```

Accessing an entry beyond the last set entry also returns the default value, if
the array does not have fixed size:

```
undefined = array:get(18, A1).
```

"Sparse" functions ignore default-valued entries:

```
A2 = array:set(4, false, A1).
[{4, false}, {17, true}] = array:sparse_to_orddict(A2).
```

An extendible array can be made fixed-size later:

```
A3 = array:fix(A2).
```

A fixed-size array does not grow automatically and does not allow accesses
beyond the last set entry:

```
{'EXIT',{badarg,_}} = (catch array:set(18, true, A3)).
{'EXIT',{badarg,_}} = (catch array:get(18, A3)).
```
""".

-export([new/0, new/1, new/2, is_array/1, set/3, get/2, size/1,
	 sparse_size/1, default/1, reset/2, to_list/1, sparse_to_list/1,
	 from_list/1, from_list/2, to_orddict/1, sparse_to_orddict/1,
         from/2, from/3,
	 from_orddict/1, from_orddict/2, map/2, sparse_map/2, foldl/3,
	 foldr/3, sparse_foldl/3, sparse_foldr/3, fix/1, relax/1, is_fix/1,
	 resize/1, resize/2]).

-export_type([array/0, array/1]).

-moduledoc(#{ authors => [~"Richard Carlsson <carlsson.richard@gmail.com>",
                          ~"Dan Gudmundsson <dgud@erix.ericsson.se>"] }).

%% Developers:
%%
%% The key to speed is to minimize the number of tests, on
%% large input. Always make the most probable path as short as possible.
%% In particular, keep in mind that for large trees, the probability of
%% a leaf node is small relative to that of an internal node.
%%
%% If you try to tweak the set_1 and get_1 loops: Measure, look at the
%% generated Beam code, and measure again! The argument order matters!


%% Representation:
%%
%% A tree is either a leaf, with LEAFSIZE elements (the "base"), an
%% internal node with LEAFSIZE elements, or an unexpanded tree,
%% represented by a single integer: the number of elements that may be
%% stored in the tree when it is expanded. 
%%
%% Note that to update an entry in a tree of height h = log[b] n, the
%% total number of written words is (b+1)+(h-1)*(b+2), since tuples use
%% a header word on the heap. 4 is the optimal base for minimizing the
%% number of words written, but causes higher trees, which takes time.
%% Current measurements gives a size of 16 as the best.
%%
%% Old comment: The best compromise between speed and memory usage seems to lie
%% around 8-10. Measurements indicate that the optimum base for speed is
%% 24 - above that, it gets slower again due to the high memory usage.
%% Base 10 is a good choice, giving 2/3 of the possible speedup from
%% base 4, but only using 1/3 more memory. (Base 24 uses 65% more memory
%% per write than base 10, but the speedup is only 21%.)

-define(DEFAULT, undefined).
-define(SHIFT, 4).                  % log2(LEAFSIZE), for bitwise div operation
-define(LEAFSIZE, (1 bsl ?SHIFT)).  % the "base" (assumed to be > 1)
-define(MASK, (?LEAFSIZE-1)).       % LEAFSIZE - 1, for bitwise rem operation
-define(MASK(X), ((1 bsl (X))-1)).
-define(SIZE(M), (1 bsl (M))).
-define(NODESIZE, ?LEAFSIZE).       % must not be LEAFSIZE-1; keep same as leaf
-define(NEW_NODE(S), erlang:make_tuple((?NODESIZE),(S))).  % when E = S
-define(NEW_LEAF(D), erlang:make_tuple(?LEAFSIZE,(D))).

-define(NEW_CACHE(D), ?NEW_LEAF(D)).

-define(reduce(X), ((X) - ?SHIFT)).
-define(extend(X), ((X) + ?SHIFT)).

%%--------------------------------------------------------------------------

-type leaf_tuple(T) :: {T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T}.

-type element_tuple(T) ::
        leaf_tuple(T)
      | {element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), non_neg_integer()}.

-type elements(T) :: non_neg_integer()
                   | element_tuple(T)
                   | nil(). % kill reference, for GC

-type cache() :: leaf_tuple(_).

-record(array, {size :: non_neg_integer(),	%% number of defined entries
		fix  :: boolean(),	        %% not automatically growing
		default,	%% the default value (usually 'undefined')
                cache :: cache(),               %% cached leaf tuple
                cache_index :: non_neg_integer(),
                elements :: elements(_),         %% the tuple tree
                bits :: integer() %% in bits
	       }).

-type array() :: array(term()).

-doc """
A functional, extendible array. The representation is not documented and is
subject to change without notice. Notice that arrays cannot be directly compared
for equality.
""".
-opaque array(Type) ::
          #array{default :: Type, elements :: elements(Type)}.

%%
%% Types
%%

-type array_indx() :: non_neg_integer().

-type array_opt()  :: {'fixed', boolean()} | 'fixed'
                    | {'default', Type :: term()}
                    | {'size', N :: non_neg_integer()}
                    | (N :: non_neg_integer()).
-type array_opts() :: array_opt() | [array_opt()].

-type indx_pair(Type)  :: {Index :: array_indx(), Type}.
-type indx_pairs(Type) :: [indx_pair(Type)].

%%--------------------------------------------------------------------------

-doc """
Creates a new, extendible array with initial size zero.

See also `new/1`, `new/2`.
""".
-spec new() -> array().

new() ->
    new([]).

-doc """
Creates a new array according to the specified options. By default, the array is
extendible and has initial size zero. Array indices start at `0`.

`Options` is a single term or a list of terms, selected from the following:

- **`N::integer() >= 0` or `{size, N::integer() >= 0}`** - Specifies the initial
  array size; this also implies `{fixed, true}`. If `N` is not a non-negative
  integer, the call fails with reason `badarg`.

- **`fixed` or `{fixed, true}`** - Creates a fixed-size array. See also `fix/1`.

- **`{fixed, false}`** - Creates an extendible (non-fixed-size) array.

- **`{default, Value}`** - Sets the default value for the array to `Value`.

Options are processed in the order they occur in the list, that is, later
options have higher precedence.

The default value is used as the value of uninitialized entries, and cannot be
changed once the array has been created.

_Examples:_

```
array:new(100)
```

creates a fixed-size array of size 100.

```
array:new({default,0})
```

creates an empty, extendible array whose default value is `0`.

```
array:new([{size,10},{fixed,false},{default,-1}])
```

creates an extendible array with initial size 10 whose default value is `-1`.

See also `fix/1`, `from_list/2`, `get/2`, `new/0`, `new/2`, `set/3`.
""".
-spec new(Options :: array_opts()) -> array().

new(Options) ->
    new_0(Options, 0, false).

-doc """
Creates a new array according to the specified size and options.

If `Size` is not a non-negative integer, the call fails with reason `badarg`.
By default, the array has fixed size. Notice that any size specifications in
`Options` override parameter `Size`.

If `Options` is a list, this is equivalent to
[`new([{size, Size} | Options])`](`new/1`), otherwise it is equivalent to
[`new([{size, Size} | [Options]])`](`new/1`). However, using this function
directly is more efficient.

_Example:_

```
array:new(100, {default,0})
```

creates a fixed-size array of size 100, whose default value is `0`.

See also `new/1`.
""".
-spec new(Size :: non_neg_integer(), Options :: array_opts()) -> array().

new(Size, Options) when is_integer(Size), Size >= 0 ->
    new_0(Options, Size, true);
new(_, _) ->
    erlang:error(badarg).

new_0(Options, Size, Fixed) when is_list(Options) ->
    new_1(Options, Size, Fixed, ?DEFAULT);
new_0(Options, Size, Fixed) ->
    new_1([Options], Size, Fixed, ?DEFAULT).

new_1([fixed | Options], Size, _, Default) ->
    new_1(Options, Size, true, Default);
new_1([{fixed, Fixed} | Options], Size, _, Default)
  when is_boolean(Fixed) ->
    new_1(Options, Size, Fixed, Default);
new_1([{default, Default} | Options], Size, Fixed, _) ->
    new_1(Options, Size, Fixed, Default);
new_1([{size, Size} | Options], _, _, Default)
  when is_integer(Size), Size >= 0 ->
    new_1(Options, Size, true, Default);
new_1([Size | Options], _, _, Default)
  when is_integer(Size), Size >= 0 ->
    new_1(Options, Size, true, Default);
new_1([], Size, Fixed, Default) ->
    new(Size, Fixed, Default);
new_1(_Options, _Size, _Fixed, _Default) ->
    erlang:error(badarg).

new(Size, Fixed, Default) ->
    E = find_max(Size - 1, ?SHIFT),
    C = ?NEW_CACHE(Default),
    #array{size = Size, fix = Fixed, cache = C, cache_index = 0,
           default = Default, elements = E, bits = ?reduce(E)}.

-spec find_max(integer(), non_neg_integer()) -> non_neg_integer().

find_max(I, M) when I >= ?SIZE(M) ->
    find_max(I, ?extend(M));
find_max(_I, M) ->
    M.


-doc """
Returns `true` if `X` is an array, otherwise `false`.

Notice that the check is only shallow, as there is no guarantee that `X` is a
well-formed array representation even if this function returns `true`.
""".
-spec is_array(X :: term()) -> boolean().

is_array(#array{size = Size})
  when is_integer(Size) ->
    true;
is_array(_) ->
    false.


-doc """
Gets the number of entries in the array. Entries are numbered from `0` to
`size(Array)-1`. Hence, this is also the index of the first entry that is
guaranteed to not have been previously set.

See also `set/3`, `sparse_size/1`.
""".
-spec size(Array :: array()) -> non_neg_integer().

size(#array{size = N}) -> N;
size(_) -> erlang:error(badarg).


-doc """
Gets the value used for uninitialized entries.

See also `new/2`.
""".
-spec default(Array :: array(Type)) -> Value :: Type.

default(#array{default = D}) -> D;
default(_) -> erlang:error(badarg).


-doc """
Fixes the array size. This prevents it from growing automatically upon
insertion.

See also `set/3` and `relax/1`.
""".
-spec fix(Array :: array(Type)) -> array(Type).

fix(#array{}=A) ->
    A#array{fix = true}.

%% similar to set_1()
set_leaf(_I, 0, _E, C) ->
    C;
set_leaf(I, S, E, C) when is_integer(E) ->
    set_leaf_1(I, S, C);
set_leaf(I, S, E, C) when S > 0 ->
    IDiv = (I bsr S) band ?MASK,
    I1 = IDiv+1,
    setelement(I1, E, set_leaf(I, ?reduce(S), element(I1, E), C)).

set_leaf_1(I, S, C) when S > 0 ->
    IDiv = (I bsr S) band ?MASK,
    setelement(IDiv+1, ?NEW_NODE(S), set_leaf_1(I, ?reduce(S), C));
set_leaf_1(_I, _S, C) ->
    C.

-doc """
Checks if the array has fixed size. Returns `true` if the array is fixed,
otherwise `false`.

See also `fix/1`.
""".
-spec is_fix(Array :: array()) -> boolean().

is_fix(#array{fix = true}) -> true;
is_fix(#array{}) -> false.


-doc """
Makes the array resizable. (Reverses the effects of `fix/1`.)

See also `fix/1`.
""".
-spec relax(Array :: array(Type)) -> array(Type).

relax(#array{size = N}=A) when is_integer(N), N >= 0 ->
    A#array{fix = false}.


%% similar to get_1
get_leaf(_I, _, E, D) when is_integer(E) ->
    ?NEW_CACHE(D);
get_leaf(_I, 0, E, _D) ->
    E;
get_leaf(I, S, E, D) ->
    IDiv = (I bsr S) band ?MASK,
    get_leaf(I, ?reduce(S), element(IDiv + 1, E), D).

-doc """
Change the array size.

If `Size` is not a non-negative integer, the call fails with reason `badarg`. If
the specified array has fixed size, also the resulting array has fixed size.
""".
-spec resize(Size :: non_neg_integer(), Array :: array(Type)) ->
                    array(Type).

resize(Size, #array{size = N, fix = Fix, cache = C, cache_index = CI, elements = E, default = D, bits = S}=A)
  when is_integer(Size), Size >= 0, is_integer(N), N >= 0,
       is_boolean(Fix), is_integer(CI), is_integer(S) ->
    if Size > N ->
            {E1, S1} = grow(Size-1, E, S),
	    A#array{size = Size, elements = E1, bits = S1};
       Size < N ->
            E1 = set_leaf(CI, S, E, C),
            {E2, S1} = shrink(Size-1, S, E1, D),

            CI1 = 0,
            C1 = get_leaf(CI1, S1, E2, D),
	    A#array{size = Size, elements = E2, cache = C1, cache_index = CI1, bits = S1};
       true ->
	    A
    end;
resize(_Size, _) ->
    erlang:error(badarg).

%% like grow(), but only used when explicitly resizing down
shrink(I, _S, _E, D) when I < 0 ->
    {?NEW_LEAF(D), 0};
shrink(I, S, E, D) ->
    shrink_1(I, S, E, D).

%% I is the largest index, 0 or more (empty arrays handled above)
shrink_1(I, S, E, D) when I < ?SIZE(S) ->
    shrink_1(I band ?MASK(S), ?reduce(S), element(1, E), D);
shrink_1(I, S, E, D) when S > 0 ->
    IDiv = I bsr S,
    IRem = I band ?MASK(S),
    E1 = prune(E, IDiv, ?NODESIZE, S),
    I1 = IDiv + 1,
    case element(I1, E1) of
        E2 when is_integer(E2) ->
            {E1, S};
        E2 ->
            {E3,_} = shrink_1(IRem, ?reduce(S), E2, D),
            {setelement(I1, E1, E3), S}
    end;
shrink_1(I, _S, E, D) ->
    {prune(E, I, ?LEAFSIZE, D), 0}.

%% the M limiter is needed for the extra data at the end of nodes
prune(E, N, M, D) ->  %% M Can be removed later
    list_to_tuple(prune(0, N, M, D, tuple_to_list(E))).

prune(I, N, M, D, [E|Es]) when I =< N ->
    [E | prune(I+1, N, M, D, Es)];
prune(I, N, M, D, [_|Es]) when I < M ->
    [D | prune(I+1, N, M, D, Es)];
prune(_I, _N, _M, _D, Es) ->
    Es.


-doc """
Changes the array size to that reported by `sparse_size/1`. If the specified
array has fixed size, also the resulting array has fixed size.

See also `resize/2`, `sparse_size/1`.
""".
-spec resize(Array :: array(Type)) -> array(Type).

resize(Array) ->
    resize(sparse_size(Array), Array).

-doc """
Sets entry `I` of the array to `Value`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`.

If the array does not have fixed size, and `I` is greater than `size(Array)-1`,
the array grows to size `I+1`.

See also `get/2`, `reset/2`.
""".
-spec set(I :: array_indx(), Value :: Type, Array :: array(Type)) -> array(Type).

set(I, Value, #array{size = N, fix = Fix, cache = C, cache_index = CI,
                     default = D, elements = E, bits = S}=A)
  when is_integer(I), I >= 0, is_integer(N), is_integer(CI), is_integer(S) ->
    if I < N ->
            if I >= CI, I < CI + ?LEAFSIZE ->
                    A#array{cache = setelement(1 + I - CI, C, Value)};
               true ->
                    R = I band ?MASK,
                    CI1 = I - R,
                    E1 = set_leaf(CI, S, E, C),
                    C1 = get_leaf(CI1, S, E1, D),
                    C2 = setelement(1 + I - CI1, C1, Value),
                    A#array{elements = E1, cache = C2, cache_index = CI1}
            end;
       Fix ->
	    erlang:error(badarg);
       true ->
            M = ?SIZE(?extend(S)),
            if I < M ->
                    R = I band ?MASK,
                    CI1 = I - R,
                    if CI1 =/= CI ->
                            E1 = set_leaf(CI, S, E, C),
                            C1 = get_leaf(CI1, S, E1, D),
                            C2 = setelement(1 + R, C1, Value),
                            A#array{size = I+1, elements = E1,
                                    cache = C2, cache_index = CI1};
                       true ->
                            C1 = setelement(1 + R, C, Value),
                            A#array{size = I+1, cache = C1, cache_index = CI1}
                    end;
               true ->
                    R = I band ?MASK,
                    CI1 = I - R,
                    {E1,S1} = grow(I, E, S),
                    E2 = set_leaf(CI, S1, E1, C),
                    C1 = get_leaf(CI1, S1, E2, D),
                    C2 = setelement(1 + R, C1, Value),
                    A#array{size = I+1, elements = E2,
                            cache = C2, cache_index = CI1, bits = S1}
            end
    end;
set(_I, _V, _A) ->
    erlang:error(badarg).

%% Enlarging the array upwards to accommodate an index `I'

grow(I, E, M) when is_integer(I), is_integer(E) ->
    S = find_max(I, M),
    {S, ?reduce(S)};
grow(I, E, 0) ->
    grow_1(I, E, 0);
grow(I, E, M) ->
    grow_1(I, E, M).

grow_1(I, E, M0) ->
    M = ?extend(M0),
    case I >= ?SIZE(M) of
        true ->
            grow_1(I, setelement(1, ?NEW_NODE(M), E), M);
        false ->
            {E, M0}
    end.

-doc """
Gets the value of entry `I`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`.

If the array does not have fixed size, the default value for any index `I`
greater than `size(Array)-1` is returned.

See also `set/3`.
""".
-spec get(I :: array_indx(), Array :: array(Type)) -> Value :: Type.

get(I, #array{size = N, fix = Fix, cache = C, cache_index = CI, elements = E, default = D, bits = S})
  when is_integer(I), I >= 0, is_integer(N), is_integer(CI), is_integer(S) ->
    if I < N ->
            if I >= CI, I < CI + ?LEAFSIZE ->
                    element(1 + I - CI, C);
               true ->
                    get_1(I, S, E, D)
            end;
       Fix ->
	    erlang:error(badarg);
       true ->
	    D
    end;
get(_I, _A) ->
    erlang:error(badarg).

get_1(_I, _S, E, D) when is_integer(E) ->
    D;
get_1(I, 0, E, _D) ->
    element((I band ?MASK)+1, E);
get_1(I, S, E, D) ->
    IDiv = (I bsr S) band ?MASK,
    get_1(I, ?reduce(S), element(IDiv + 1, E), D).

%% TODO: a reset_range function

-doc """
Resets entry `I` to the default value for the array. If the value of entry `I`
is the default value, the array is returned unchanged.

Reset never changes the array size. Shrinking can be done explicitly by calling
`resize/2`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`; compare
`set/3`

See also `new/2`, `set/3`.
""".
-spec reset(I :: array_indx(), Array :: array(Type)) -> array(Type).

reset(I, #array{size = N, fix = Fix, cache = C, cache_index = CI, default = D, elements = E, bits = S}=A)
    when is_integer(I), I >= 0, is_integer(N), is_boolean(Fix), is_integer(CI) ->
    if I < N ->
            if I >= CI, I < CI + ?LEAFSIZE ->
                    A#array{cache = setelement(1 + I - CI, C, D)};
               true ->
                    try A#array{elements = reset_1(I, S, E, D)}
                    catch throw:default -> A
                    end
	    end;
       Fix ->
	    erlang:error(badarg);
       true ->
	    A
    end;
reset(_I, _A) ->
    erlang:error(badarg).

reset_1(_I, _, E, _D) when is_integer(E) ->
    throw(default);
reset_1(I, 0, E, D) ->
    Indx = (I band ?MASK)+1,
    case element(Indx, E) of
	D -> throw(default);
	_ -> setelement(Indx, E, D)
    end;
reset_1(I, S, E, D) ->
    IDiv = (I bsr S) band ?MASK,
    I1 = IDiv + 1,
    setelement(I1, E, reset_1(I, ?reduce(S), element(I1, E), D)).

-doc """
Converts the array to a list.

See also `from_list/2`, `sparse_to_list/1`.
""".
-spec to_list(Array :: array(Type)) -> list(Value :: Type).

to_list(#array{size = 0}) ->
    [];
to_list(#array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_integer(N), is_integer(CI), is_integer(M) ->
    E1 = set_leaf(CI, M, E, C),
    to_list_1(E1, M, D, N - 1);
to_list(_) ->
    erlang:error(badarg).

%% this part handles the rightmost subtrees

to_list_1(E, _S, D, I) when is_integer(E) ->
    push(I+1, D, []);
to_list_1(E, 0, _D, I) ->
    push_tuple(I+1, E, []);
to_list_1(E, S, D, I) ->
    N = I bsr S,
    IRem = I band ?MASK(S),
    to_list_3(N, S, D, to_list_1(element(N+1, E), ?reduce(S), D, IRem), E).

%% this part handles full trees only

to_list_2(E, _S, D, L) when is_integer(E) ->
    push(?SIZE(E), D, L);
to_list_2(E, 0, _D, L) ->
    push_tuple(?LEAFSIZE, E, L);
to_list_2(E, S, D, L) ->
    to_list_3(?NODESIZE, S, D, L, E).

to_list_3(0, _S,  _D, L, _E) ->
    L;
to_list_3(N, S, D, L, E) ->
    to_list_3(N-1, S, D, to_list_2(element(N, E), ?reduce(S), D, L), E).

push(0, _E, L) ->
    L;
push(N, E, L) ->
    push(N - 1, E, [E | L]).

push_tuple(0, _T, L) ->
    L;
push_tuple(N, T, L) ->
    push_tuple(N - 1, T, [element(N, T) | L]).

-doc """
Converts the array to a list, skipping default-valued entries.

See also `to_list/1`.
""".
-spec sparse_to_list(Array :: array(Type)) -> list(Value :: Type).

sparse_to_list(#array{size = 0}) ->
    [];
sparse_to_list(#array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_integer(N), is_integer(CI), is_integer(M) ->
    E1 = set_leaf(CI, M, E, C),
    sparse_to_list_1(E1, M, D, N - 1);
sparse_to_list(_) ->
    erlang:error(badarg).

%% see to_list/1 for details

sparse_to_list_1(E, _S, _D, _I) when is_integer(E) ->
    [];
sparse_to_list_1(E, 0, D, I) ->
    sparse_push_tuple(I+1, D, E, []);
sparse_to_list_1(E, S, D, I) ->
    N = I bsr S,
    IRem = I band ?MASK(S),
    sparse_to_list_3(N, S, D,
		     sparse_to_list_1(element(N+1, E), ?reduce(S), D, IRem),
		     E).

sparse_to_list_2(E, _S, _D, L) when is_integer(E) ->
    L;
sparse_to_list_2(E, 0, D, L) ->
    sparse_push_tuple(?LEAFSIZE, D, E, L);
sparse_to_list_2(E, S, D, L) ->
    sparse_to_list_3(?NODESIZE, S, D, L, E).

sparse_to_list_3(0, _S, _D, L, _E) ->
    L;
sparse_to_list_3(N, S, D, L, E) ->
    sparse_to_list_3(N-1, S, D, sparse_to_list_2(element(N, E), ?reduce(S), D, L), E).

sparse_push_tuple(0, _D, _T, L) ->
    L;
sparse_push_tuple(N, D, T, L) ->
    case element(N, T) of
	D -> sparse_push_tuple(N - 1, D, T, L);
	E -> sparse_push_tuple(N - 1, D, T, [E | L])
    end.

%% @equiv from_list(List, undefined)

-doc "Equivalent to [`from_list(List, undefined)`](`from_list/2`).".
-spec from_list(List :: list(Value :: Type)) -> array(Type).

from_list(List) ->
    from_list(List, undefined).

-doc """
Converts a list to an extendible array. `Default` is used as the value for
uninitialized entries of the array.

If `List` is not a proper list, the call fails with reason `badarg`.

See also `new/2`, `to_list/1`.
""".
-spec from_list(List :: list(Value :: Type), Default :: term()) -> array(Type).

from_list([], Default) ->
    new({default,Default});
from_list(List, Default) when is_list(List) ->
    {E, N, M0} = from_list_1(?LEAFSIZE, List, Default, 0, [], []),
    CI = 0,
    M = ?reduce(M0),
    C = get_leaf(CI, M, E, Default),
    #array{size = N, fix = false, cache = C, cache_index = CI,
           default = Default, elements = E, bits = M};
from_list(_, _) ->
    erlang:error(badarg).

%% Note: A cleaner but slower algorithm is to first take the length of
%% the list and compute the max size of the final tree, and then
%% decompose the list. The below algorithm is almost twice as fast,
%% however.

%% Building the leaf nodes (padding the last one as necessary) and
%% counting the total number of elements.
from_list_1(0, Xs, D, N, As, Es) ->
    E = list_to_tuple(lists:reverse(As)),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?SHIFT};
		_ ->
		    from_list_2_0(N, [E | Es], ?SHIFT)
	    end;
	[_|_] ->
	    from_list_1(?LEAFSIZE, Xs, D, N, [], [E | Es]);
	_ ->
	    erlang:error(badarg)
    end;
from_list_1(I, Xs, D, N, As, Es) ->
    case Xs of
	[X | Xs1] ->
	    from_list_1(I-1, Xs1, D, N+1, [X | As], Es);
	_ ->
	    from_list_1(I-1, Xs, D, N, [D | As], Es)
    end.

%% Building the internal nodes (note that the input is reversed).
from_list_2_0(N, Es, S) ->
    from_list_2(?NODESIZE, pad(((N-1) bsr S) + 1, ?NODESIZE, S, Es),
		S, N, [], []).

from_list_2(0, Xs, S, N, As, Es) ->
    E = list_to_tuple(As),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?extend(S)};
		_ ->
		    from_list_2_0(N, lists:reverse([E | Es]),
				  ?extend(S))
	    end;
	_ ->
	    from_list_2(?NODESIZE, Xs, S, N, [], [E | Es])
    end;
from_list_2(I, [X | Xs], S, N, As, Es) ->
    from_list_2(I-1, Xs, S, N, [X | As], Es).


%% left-padding a list Es with elements P to the nearest multiple of K
%% elements from N (adding 0 to K-1 elements).
pad(N, K, P, Es) ->
    push((K - (N rem K)) rem K, P, Es).


-doc "Equivalent to [`from(Fun, State, undefined)`](`from/3`).".
-doc(#{since => <<"OTP 29.0">>}).
-spec from(Function, State :: term()) -> array(Type) when
      Function :: fun((State0 :: term()) -> {Type, State1 :: term()} | done).

from(Fun, State)  ->
    from(Fun, State, undefined).


-doc """
Creates an extendible array with values obtained with `Function(State)`.

The 'Function(State)' shall return `{Value, NewState}` or `done`, and is invoked
until `done` is returned, otherwise the call fails with reason `badarg`.

`Default` is used as the value for uninitialized entries of the array.

## Examples

```erlang
1> Floats = << <<N:32/float-native>> || N <- lists:seq(0, 2047)>>.
2> BinToVal = fun(I) ->
     case Floats of
         <<_:I/binary, N:32/float-native, _/binary>> ->
             {N, I+4};
         _ ->
             done
     end
   end.
3> A = array:from(BinToVal, 0).
4> array:get(10, A).
10.0
5> array:size(A).
2048
6> ValToBin = fun(_K, V, Acc) -> <<Acc/binary, V:32/float-native>> end.
7> Floats == array:foldl(ValToBin, <<>>, A).
true
```

See also `new/2`, `from_list/1`, `foldl/3`.
""".

-doc(#{since => <<"OTP 29.0">>}).
-spec from(Function, State :: term(), Default :: term()) -> array(Type) when
      Function :: fun((State0 :: term()) -> {Type, State1 :: term()} | done).

from(Fun, S0, Default) when is_function(Fun, 1) ->
    VS = Fun(S0),
    {E, N, M0} = from_fun_1(?LEAFSIZE, Default, Fun, VS, 0, [], []),
    CI = 0,
    M = ?reduce(M0),
    C = get_leaf(CI, M, E, Default),
    #array{size = N, fix = false, cache = C, cache_index = CI,
           default = Default, elements = E, bits = M};
from(_, _, _) ->
    error(badarg).


from_fun_1(0, D, Fun, VS, N, As, Es) ->
    E = list_to_tuple(lists:reverse(As)),
    case VS of
	done ->
	    case Es of
		[] ->
		    {E, N, ?SHIFT};
		_ ->
		    from_list_2_0(N, [E | Es], ?SHIFT)
	    end;
	_ ->
	    from_fun_1(?LEAFSIZE, D, Fun, VS, N, [], [E | Es])
    end;
from_fun_1(I, D, Fun, done, N, As, Es) ->
    from_fun_1(I-1, D, Fun, done, N, [D | As], Es);
from_fun_1(I, D, Fun, {X, S}, N, As, Es) ->
    from_fun_1(I-1, D, Fun, Fun(S), N+1, [X | As], Es);
from_fun_1(_I, _D, _Fun, _VS, _N, _As, _Es) ->
    erlang:error(badarg).


-doc """
Converts the array to an ordered list of pairs `{Index, Value}`.

See also `from_orddict/2`, `sparse_to_orddict/1`.
""".
-spec to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).

to_orddict(#array{size = 0}) ->
    [];
to_orddict(#array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_integer(N), is_integer(CI), is_integer(M) ->
    E1 = set_leaf(CI, M, E, C),
    I = N - 1,
    to_orddict_1(E1, I, D, I, M);
to_orddict(_) ->
    erlang:error(badarg).

%% see to_list/1 for comparison

to_orddict_1(E, R, D, I, _S) when is_integer(E) ->
    push_pairs(I+1, R, D, []);
to_orddict_1(E, R, _D, I, 0) ->
    push_tuple_pairs(I+1, R, E, []);
to_orddict_1(E, R, D, I, S) ->
    N = I bsr S,
    I1 = I band ?MASK(S),
    to_orddict_3(N, R - I1 - 1, D,
 		 to_orddict_1(element(N+1, E), R, D, I1, ?reduce(S)),
 		 E, S).

to_orddict_2(E, R, D, L, _S) when is_integer(E) ->
    push_pairs(?SIZE(E), R, D, L);
to_orddict_2(E, R, _D, L, 0) ->
    push_tuple_pairs(?LEAFSIZE, R, E, L);
to_orddict_2(E, R, D, L, S) ->
    to_orddict_3(?NODESIZE, R, D, L, E, S).

to_orddict_3(0, _R, _D, L, _E, _S) -> %% when is_integer(R) ->
    L;
to_orddict_3(N, R, D, L, E, S) ->
    to_orddict_3(N-1, R - ?SIZE(S), D,
 		 to_orddict_2(element(N, E), R, D, L, ?reduce(S)),
 		 E, S).

-spec push_pairs(non_neg_integer(), array_indx(), term(), indx_pairs(Type)) ->
	  indx_pairs(Type).

push_pairs(0, _I, _E, L) ->
    L;
push_pairs(N, I, E, L) ->
    push_pairs(N-1, I-1, E, [{I, E} | L]).

-spec push_tuple_pairs(non_neg_integer(), array_indx(), term(), indx_pairs(Type)) ->
	  indx_pairs(Type).

push_tuple_pairs(0, _I, _T, L) ->
    L;
push_tuple_pairs(N, I, T, L) ->
    push_tuple_pairs(N-1, I-1, T, [{I, element(N, T)} | L]).


-doc """
Converts the array to an ordered list of pairs `{Index, Value}`, skipping
default-valued entries.

See also `to_orddict/1`.
""".
-spec sparse_to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).

sparse_to_orddict(#array{size = 0}) ->
    [];
sparse_to_orddict(#array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_integer(N), is_integer(CI), is_integer(M) ->
    E1 = set_leaf(CI, M, E, C),
    I = N - 1,
    sparse_to_orddict_1(E1, I, D, I, M);
sparse_to_orddict(_) ->
    erlang:error(badarg).

%% see to_orddict/1 for details
sparse_to_orddict_1(E, _R, _D, _I,_S) when is_integer(E) ->
    [];
sparse_to_orddict_1(E, R, D, I, 0) ->
    sparse_push_tuple_pairs(I+1, R, D, E, []);
sparse_to_orddict_1(E, R, D, I, S) ->
    N = I bsr S,
    I1 = I band ?MASK(S),
    sparse_to_orddict_3(N, R - I1 - 1, D,
                        sparse_to_orddict_1(element(N+1, E), R, D, I1, ?reduce(S)),
 		 E, S).

sparse_to_orddict_2(E, _R, _D, L, _S) when is_integer(E) ->
    L;
sparse_to_orddict_2(E, R, D, L, 0) ->
    sparse_push_tuple_pairs(?LEAFSIZE, R, D, E, L);
sparse_to_orddict_2(E, R, D, L, S) ->
    sparse_to_orddict_3(?NODESIZE, R, D, L, E, S).

sparse_to_orddict_3(0, _R, _D, L, _E, _S) -> % when is_integer(R) ->
    L;
sparse_to_orddict_3(N, R, D, L, E, S) ->
    sparse_to_orddict_3(N-1, R - ?SIZE(S), D,
                        sparse_to_orddict_2(element(N, E), R, D, L,?reduce(S)),
                        E, S).

-spec sparse_push_tuple_pairs(non_neg_integer(), array_indx(),
			      _, _, indx_pairs(Type)) -> indx_pairs(Type).

sparse_push_tuple_pairs(0, _I, _D, _T, L) ->
    L;
sparse_push_tuple_pairs(N, I, D, T, L) ->
    case element(N, T) of
	D -> sparse_push_tuple_pairs(N-1, I-1, D, T, L);
	E -> sparse_push_tuple_pairs(N-1, I-1, D, T, [{I, E} | L])
    end.

-doc "Equivalent to [`from_orddict(Orddict, undefined)`](`from_orddict/2`).".
-spec from_orddict(Orddict :: indx_pairs(Value :: Type)) -> array(Type).

from_orddict(Orddict) ->
    from_orddict(Orddict, undefined).

-doc """
Converts an ordered list of pairs `{Index, Value}` to a corresponding extendible
array. `Default` is used as the value for uninitialized entries of the array.

If `Orddict` is not a proper, ordered list of pairs whose first elements are
non-negative integers, the call fails with reason `badarg`.

See also `new/2`, `to_orddict/1`.
""".
-spec from_orddict(Orddict :: indx_pairs(Value :: Type), Default :: Type) ->
                          array(Type).

from_orddict([], Default) ->
    new({default,Default});
from_orddict(List, Default) when is_list(List) ->
    {E, N, M0} = from_orddict_0(List, 0, ?LEAFSIZE, Default, []),
    CI = 0,
    M = ?reduce(M0),
    C = get_leaf(CI, M, E, Default),
    #array{size = N, fix = false, cache = C, cache_index = CI,
           default = Default, elements = E, bits = M};
from_orddict(_, _) ->
    erlang:error(badarg).

%% 2 pass implementation, first pass builds the needed leaf nodes
%% and adds hole sizes.
%% (inserts default elements for missing list entries in the leafs
%%  and pads the last tuple if necessary).
%% Second pass builds the tree from the leafs and the holes.
%%
%% Doesn't build/expand unnecessary leaf nodes which costs memory
%% and time for sparse arrays.

from_orddict_0([], N, _Bits, _D, Es) ->
    %% Finished, build the resulting tree
    case Es of
	[E] ->
	    {E, N, ?SHIFT};
	_ ->
	    collect_leafs(N, Es, ?SHIFT)
    end;

from_orddict_0(Xs=[{Ix1, _}|_], Ix, S0, D, Es0)
  when is_integer(Ix1), Ix1 > S0  ->
    %% We have a hole larger than a leaf
    Hole = Ix1-Ix,
    Step = Hole - (Hole band ?MASK),
    Next = Ix+Step,
    from_orddict_0(Xs, Next, Next+?LEAFSIZE, D, [Step|Es0]);
from_orddict_0(Xs0=[{_, _}|_], Ix0, S, D, Es) ->
    %% Fill a leaf
    {Xs,E,Ix} = from_orddict_1(Ix0, S, Xs0, Ix0, D, []),
    from_orddict_0(Xs, Ix, Ix+?LEAFSIZE, D, [E|Es]);
from_orddict_0(Xs, _, _, _,_) ->
    erlang:error({badarg, Xs}).

from_orddict_1(Ix, Ix, Xs, N, _D, As) ->
    %% Leaf is full
    E = list_to_tuple(lists:reverse(As)),
    {Xs, E, N};
from_orddict_1(Ix, S, Xs, N0, D, As) ->
    case Xs of
	[{Ix, Val} | Xs1] ->
	    N = Ix+1,
	    from_orddict_1(N, S, Xs1, N, D, [Val | As]);
	[{Ix1, _} | _] when is_integer(Ix1), Ix1 > Ix ->
	    N = Ix+1,
	    from_orddict_1(N, S, Xs, N, D, [D | As]);
	[_ | _] ->
	    erlang:error({badarg, Xs});
	_ ->
	    from_orddict_1(Ix+1, S, Xs, N0, D, [D | As])
    end.

%% Es is reversed i.e. starting from the largest leafs
collect_leafs(N, Es, S) ->
    I = ((N-1) bsr S) + 1,
    Pad = ((?NODESIZE - (I rem ?NODESIZE)) rem ?NODESIZE) * ?SIZE(S),
    case Pad of
	0 ->
	    collect_leafs(?NODESIZE, Es, S, N, [], []);
	_ ->  %% Pad the end
	    collect_leafs(?NODESIZE, [Pad|Es], S, N, [], [])
    end.

collect_leafs(0, Xs, S, N, As, Es) ->
    E = list_to_tuple(As),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?extend(S)};
		_ ->
		    collect_leafs(N, lists:reverse([E | Es]),
			       ?extend(S))
	    end;
	_ ->
	    collect_leafs(?NODESIZE, Xs, S, N, [], [E | Es])
    end;
collect_leafs(I, [X | Xs], S, N, As0, Es0)
  when is_integer(X) ->
    %% A hole, pad accordingly.
    Step0 = (X bsr S),
    if
	Step0 < I ->
	    As = push(Step0, S, As0),
	    collect_leafs(I-Step0, Xs, S, N, As, Es0);
	I =:= ?NODESIZE ->
	    Step = Step0 rem ?NODESIZE,
	    As = push(Step, S, As0),
	    collect_leafs(I-Step, Xs, S, N, As, [X|Es0]);
	I =:= Step0 ->
	    As = push(I, S, As0),
	    collect_leafs(0, Xs, S, N, As, Es0);
	true ->
	    As = push(I, S, As0),
	    Step = Step0 - I,
	    collect_leafs(0, [Step bsl S|Xs], S, N, As, Es0)
    end;
collect_leafs(I, [X | Xs], S, N, As, Es) ->
    collect_leafs(I-1, Xs, S, N, [X | As], Es);
collect_leafs(?NODESIZE, [], S, N, [], Es) ->
    collect_leafs(N, lists:reverse(Es), ?extend(S)).

%%    Function = (Index::integer(), Value::term()) -> term()

-doc """
Maps the specified function onto each array element. The elements are visited in
order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldl/3`, `foldr/3`, `sparse_map/2`.
""".
-spec map(Function, Array :: array(Type1)) -> array(Type2) when
      Function :: fun((Index :: array_indx(), Type1) -> Type2).

map(Function, Array=#array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_function(Function, 2), is_integer(N), is_integer(CI), is_integer(M) ->
    if N > 0 ->
            E1 = set_leaf(CI, M, E, C),
	    A = Array#array{elements = []}, % kill reference, for GC
            E2 = map_1(N-1, E1, M, 0, Function, D),
            CI1 = 0,
            C1 = get_leaf(CI1, M, E2, D),
	    A#array{elements = E2, cache = C1, cache_index = CI1};
       true ->
	    Array
    end;
map(_, _) ->
    erlang:error(badarg).

%% It might be simpler to traverse the array right-to-left, as done e.g.
%% in the to_orddict/1 function, but it is better to guarantee
%% left-to-right application over the elements - that is more likely to
%% be a generally useful property.

map_1(N, E, S, Ix, F, D) when is_integer(E) ->
    map_1(N, unfold(E, D), S, Ix, F, D);
map_1(N, E, 0, Ix, F, D) ->
    list_to_tuple(lists:reverse(map_3(1, E, Ix, F, D, N+1, [])));
map_1(N, E, S, Ix, F, D) ->
    List = map_2(1, E, S, Ix, F, D, [],
                 (N bsr S) + 1, N band ?MASK(S)),
    list_to_tuple(lists:reverse([S | List])).

map_2(I, E, S, Ix, F, D, L, I, R) ->
    map_2_1(I+1, E, [map_1(R, element(I, E), ?reduce(S), Ix, F, D) | L]);
map_2(I, E, S, Ix, F, D, L, N, R) ->
    Sz = ?SIZE(S),
    map_2(I+1, E, S, Ix + Sz, F, D,
	  [map_1(Sz-1, element(I, E), ?reduce(S), Ix, F, D) | L],
	  N, R).

map_2_1(I, E, L) when I =< ?NODESIZE ->
    map_2_1(I+1, E, [element(I, E) | L]);
map_2_1(_I, _E, L) ->
    L.

-spec map_3(pos_integer(), _, array_indx(),
	    fun((array_indx(),_) -> _), _, non_neg_integer(), [X]) -> [X].

map_3(I, E, Ix, F, D, N, L) when I =< N ->
    map_3(I+1, E, Ix+1, F, D, N, [F(Ix, element(I, E)) | L]);
map_3(I, E, Ix, F, D, N, L) when I =< ?LEAFSIZE ->
    map_3(I+1, E, Ix+1, F, D, N, [D | L]);
map_3(_I, _E, _Ix, _F, _D, _N, L) ->
    L.


unfold(S, _D) when ?SIZE(S) > ?LEAFSIZE ->
    ?NEW_NODE(?reduce(S));
unfold(_S, D) ->
    ?NEW_LEAF(D).

-doc """
Maps the specified function onto each array element, skipping default-valued
entries. The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `map/2`.
""".
-spec sparse_map(Function, Array :: array(Type1)) -> array(Type2) when
      Function :: fun((Index :: array_indx(), Type1) -> Type2).

sparse_map(Function, Array=#array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_function(Function, 2), is_integer(N), is_integer(CI), is_integer(M) ->
    if N > 0 ->
            E1 = set_leaf(CI, M, E, C),
	    A = Array#array{elements = []}, % kill reference, for GC
            E2 = sparse_map_1(N-1, E1, M, 0, Function, D),
            CI1 = 0,
            C1 = get_leaf(CI1, M, E2, D),
	    A#array{elements = E2, cache = C1, cache_index = CI1};
       true ->
	    Array
    end;
sparse_map(_, _) ->
    erlang:error(badarg).

%% see map/2 for details
%% TODO: we can probably optimize away the use of div/rem here

sparse_map_1(_N, E, _S, _Ix, _F, _D) when is_integer(E) ->
    E;
sparse_map_1(_N, E, 0, Ix, F, D) ->
    list_to_tuple(lists:reverse(sparse_map_3(1, E, Ix, F, D, [])));
sparse_map_1(N, E, S, Ix, F, D) ->
    List = sparse_map_2(1, E, S, Ix, F, D, [], (N bsr S) + 1, N band ?MASK(S)),
    list_to_tuple(lists:reverse([S | List])).

sparse_map_2(I, E, S, Ix, F, D, L, N, R) when N =:= I ->
    sparse_map_2_1(I+1, E,
		   [sparse_map_1(R, element(I, E), ?reduce(S), Ix, F, D) | L]);
sparse_map_2(I, E, S, Ix, F, D, L, N, R) ->
    Sz = ?SIZE(S),
    sparse_map_2(I+1, E, S, Ix + Sz, F, D,
                 [sparse_map_1(Sz-1, element(I, E), ?reduce(S), Ix, F, D) | L],
                 N, R).

sparse_map_2_1(I, E, L) when I =< ?NODESIZE ->
    sparse_map_2_1(I+1, E, [element(I, E) | L]);
sparse_map_2_1(_I, _E, L) ->
    L.

-spec sparse_map_3(pos_integer(), _, array_indx(),
		   fun((array_indx(),_) -> _), _, [X]) -> [X].

sparse_map_3(I, T, Ix, F, D, L) when I =< ?LEAFSIZE ->
    case element(I, T) of
	D -> sparse_map_3(I+1, T, Ix+1, F, D, [D | L]);
	E -> sparse_map_3(I+1, T, Ix+1, F, D, [F(Ix, E) | L])
    end;
sparse_map_3(_I, _E, _Ix, _F, _D, L) ->
    L.

-doc """
Folds the array elements using the specified function and initial accumulator
value. The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldr/3`, `map/2`, `sparse_foldl/3`.
""".
-spec foldl(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

foldl(Function, A, #array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_function(Function, 3), is_integer(N), is_integer(CI), is_integer(M) ->
    if N > 0 ->
            E1 = set_leaf(CI, M, E, C),
	    foldl_1(N-1, E1, M, A, 0, Function, D);
       true ->
	    A
    end;
foldl(_, _, _) ->
    erlang:error(badarg).

foldl_1(N, E, S, A, Ix, F, D) when is_integer(E) ->
    foldl_1(N, unfold(E, D), S, A, Ix, F, D);
foldl_1(N, E, 0, A, Ix, F, _D) ->
    foldl_3(1, E, A, Ix, F, N+1);
foldl_1(N, E, S, A, Ix, F, D) ->
    foldl_2(1, E, S, A, Ix, F, D, (N bsr S) + 1, N band ?MASK(S)).

foldl_2(I, E, S, A, Ix, F, D, I, R) ->
    foldl_1(R, element(I, E), ?reduce(S), A, Ix, F, D);
foldl_2(I, E, S, A, Ix, F, D, N, R) ->
    Sz = ?SIZE(S),
    Acc = foldl_1(Sz-1, element(I, E), ?reduce(S), A, Ix, F, D),
    foldl_2(I+1, E, S, Acc, Ix + Sz, F, D, N, R).

-spec foldl_3(pos_integer(), _, A, array_indx(),
	      fun((array_indx(), _, A) -> B), integer()) -> B.

foldl_3(I, E, A, Ix, F, N) when I =< N ->
    foldl_3(I+1, E, F(Ix, element(I, E), A), Ix+1, F, N);
foldl_3(_I, _E, A, _Ix, _F, _N) ->
    A.


-doc """
Folds the array elements using the specified function and initial accumulator
value, skipping default-valued entries. The elements are visited in order from
the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldl/3`, `sparse_foldr/3`.
""".
-spec sparse_foldl(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

sparse_foldl(Function, A, #array{size = N, cache = C, cache_index = CI,
                                 elements = E, default = D, bits = M})
  when is_function(Function, 3), is_integer(N), is_integer(CI), is_integer(M) ->
    if N > 0 ->
            E1 = set_leaf(CI, M, E, C),
	    sparse_foldl_1(N-1, E1, M, A, 0, Function, D);
       true ->
	    A
    end;
sparse_foldl(_, _, _) ->
    erlang:error(badarg).

%% see foldl/3 for details
%% TODO: this can be optimized

sparse_foldl_1(_N, E, _S, A, _Ix, _F, _D) when is_integer(E) ->
    A;
sparse_foldl_1(N, E, 0, A, Ix, F, D) ->
    sparse_foldl_3(1, E, A, Ix, F, D, N+1);
sparse_foldl_1(N, E, S, A, Ix, F, D) ->
    sparse_foldl_2(1, E, S, A, Ix, F, D, (N bsr S) + 1, N band ?MASK(S)).

sparse_foldl_2(I, E, S, A, Ix, F, D, I, R) ->
    sparse_foldl_1(R, element(I, E), ?reduce(S), A, Ix, F, D);
sparse_foldl_2(I, E, S, A, Ix, F, D, N, R) ->
    Sz = ?SIZE(S),
    sparse_foldl_2(I+1, E, S,
                   sparse_foldl_1(Sz-1, element(I, E), ?reduce(S), A, Ix, F, D),
                   Ix + Sz, F, D, N, R).

sparse_foldl_3(I, T, A, Ix, F, D, N) when I =< N ->
    case element(I, T) of
	D -> sparse_foldl_3(I+1, T, A, Ix+1, F, D, N);
	E -> sparse_foldl_3(I+1, T, F(Ix, E, A), Ix+1, F, D, N)
    end;
sparse_foldl_3(_I, _T, A, _Ix, _F, _D, _N) ->
    A.


-doc """
Folds the array elements right-to-left using the specified function and initial
accumulator value. The elements are visited in order from the highest index to
the lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldl/3`, `map/2`.
""".
-spec foldr(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

foldr(Function, A, #array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_function(Function, 3), is_integer(N), is_integer(CI), is_integer(M) ->
    if N > 0 ->
	    I = N - 1,
            E1 = set_leaf(CI, M, E, C),
	    foldr_1(I, E1, M, I, A, Function, D);
       true ->
	    A
    end;
foldr(_, _, _) ->
    erlang:error(badarg).

%% this is based on to_orddict/1
foldr_1(I, E, S, Ix, A, F, D) when is_integer(E) ->
    foldr_1(I, unfold(E, D), S, Ix, A, F, D);
foldr_1(I, E, 0, Ix, A, F, _D) ->
    I1 = I+1,
    foldr_3(I1, E, Ix-I1, A, F);
foldr_1(I, E, S, Ix, A, F, D) ->
    foldr_2((I bsr S) + 1, E, S, Ix, A, F, D, I band ?MASK(S), ?SIZE(S)-1).

foldr_2(0, _E, _S, _Ix, A, _F, _D, _R, _R0) ->
    A;
foldr_2(I, E, S, Ix, A, F, D, R, R0) ->
    foldr_2(I-1, E, S, Ix - R - 1,
	    foldr_1(R, element(I, E), ?reduce(S), Ix, A, F, D),
	    F, D, R0, R0).

-spec foldr_3(array_indx(), term(), integer(), A,
	      fun((array_indx(), _, A) -> B)) -> B.

foldr_3(0, _E, _Ix, A, _F) ->
    A;
foldr_3(I, E, Ix, A, F) ->
    foldr_3(I-1, E, Ix, F(Ix+I, element(I, E), A), F).


-doc """
Folds the array elements right-to-left using the specified function and initial
accumulator value, skipping default-valued entries. The elements are visited in
order from the highest index to the lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldr/3`, `sparse_foldl/3`.
""".
-spec sparse_foldr(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

sparse_foldr(Function, A, #array{size = N, cache = C, cache_index = CI, elements = E, default = D, bits = M})
  when is_function(Function, 3), is_integer(N), is_integer(CI), is_integer(M) ->
    if N > 0 ->
	    I = N - 1,
            E1 = set_leaf(CI, M, E, C),
	    sparse_foldr_1(I, E1, M, I, A, Function, D);
       true ->
	    A
    end;
sparse_foldr(_, _, _) ->
    erlang:error(badarg).

%% see foldr/3 for details
%% TODO: this can be optimized

sparse_foldr_1(_I, E, _S, _Ix, A, _F, _D) when is_integer(E) ->
    A;
sparse_foldr_1(I, E, 0, Ix, A, F, D) ->
    I1 = I+1,
    sparse_foldr_3(I1, E, Ix-I1, A, F, D);
sparse_foldr_1(I, E, S, Ix, A, F, D) ->
    sparse_foldr_2((I bsr S) + 1, E, S, Ix, A, F, D, I band ?MASK(S), ?SIZE(S)-1).

sparse_foldr_2(0, _E, _S, _Ix, A, _F, _D, _R, _R0) ->
    A;
sparse_foldr_2(I, E, S, Ix, A, F, D, R, R0) ->
    sparse_foldr_2(I-1, E, S, Ix - R - 1,
                   sparse_foldr_1(R, element(I, E), ?reduce(S), Ix, A, F, D),
                   F, D, R0, R0).

-spec sparse_foldr_3(array_indx(), _, array_indx(), A,
		     fun((array_indx(), _, A) -> B), _) -> B.

sparse_foldr_3(0, _T, _Ix, A, _F, _D) ->
    A;
sparse_foldr_3(I, T, Ix, A, F, D) ->
    case element(I, T) of
	D -> sparse_foldr_3(I-1, T, Ix, A, F, D);
	E -> sparse_foldr_3(I-1, T, Ix, F(Ix+I, E, A), F, D)
    end.


-doc """
Gets the number of entries in the array up until the last non-default-valued
entry. That is, returns `I+1` if `I` is the last non-default-valued entry in the
array, or zero if no such entry exists.

See also `resize/1`, `size/1`.
""".
-spec sparse_size(Array :: array()) -> non_neg_integer().

sparse_size(A) ->
    F = fun (I, _V, _A) -> throw({value, I}) end,
    try sparse_foldr(F, [], A) of
	[] -> 0
    catch
	{value, I} when is_integer(I) ->
	    I + 1
    end.
