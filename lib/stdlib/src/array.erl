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

## Examples

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

Accessing an unset entry returns the default value:

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
         concat/2, concat/1, from/2, from/3,
	 from_orddict/1, from_orddict/2, map/2, sparse_map/2, foldl/3,
	 foldl/5, foldr/3, foldr/5, sparse_foldl/3, sparse_foldl/5,
	 sparse_foldr/3, sparse_foldr/5, mapfoldl/3, mapfoldl/5,
	 mapfoldr/3, mapfoldr/5, sparse_mapfoldl/3, sparse_mapfoldl/5,
	 sparse_mapfoldr/3, sparse_mapfoldr/5,
         fix/1, relax/1, is_fix/1,
	 resize/1, resize/2, shift/2, slice/3, prepend/2, append/2]).

-export([upgrade/1]).

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
%% represented by EMPTY.
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
-define(SIZE(S), (1 bsl (S))).
-define(NODESIZE, ?LEAFSIZE).       % must not be LEAFSIZE-1; keep same as leaf
-define(NEW_NODE(S),   %% Hardcoded to get a literal
        {?EMPTY,?EMPTY,?EMPTY,?EMPTY, ?EMPTY,?EMPTY,?EMPTY,?EMPTY,
         ?EMPTY,?EMPTY,?EMPTY,?EMPTY, ?EMPTY,?EMPTY,?EMPTY,?EMPTY}).
%% -define(NEW_NODE(S), erlang:make_tuple(?NODESIZE,(?EMPTY))).     %% S not actually used
-define(NEW_LEAF(D), erlang:make_tuple(?LEAFSIZE,(D))).
-define(EMPTY, []).  % placeholder for empty subtree (keep as immediate)
-define(NEW_CACHE(D), ?NEW_LEAF(D)).

-define(reduce(X), ((X) - ?SHIFT)).
-define(extend(X), ((X) + ?SHIFT)).

%%--------------------------------------------------------------------------

-type leaf_tuple(T) :: {T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T}.

-type element_tuple(T) ::
        leaf_tuple(T)
      | ?EMPTY
      | {element_tuple(T), element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T), element_tuple(T)}.

-type elements(T) :: ?EMPTY | element_tuple(T).

-type cache() :: leaf_tuple(dynamic()).

-record(array, {size :: non_neg_integer(),	  %% number of defined entries
                zero :: non_neg_integer(),        %% offset of zero point
		fix  :: boolean(),	          %% not automatically growing
		default :: dynamic(),   %% the default value (usually 'undefined')
                cache :: cache(),                 %% cached leaf tuple
                cache_index :: non_neg_integer(),
                elements  :: elements(dynamic()), %% the tuple tree
                bits :: integer()                 %% in bits
	       }).

-type array() :: array(dynamic()).

-doc """
A functional, extendible array.

The representation is not documented and is subject to change without
notice. Notice that arrays cannot be directly compared for equality.
""".
-opaque array(Type) ::
          #array{default :: Type, elements :: elements(Type)}.

%%
%% Types
%%

-type array_indx() :: non_neg_integer().

-type array_opt()  :: {'fixed', boolean()} | 'fixed'
                    | {'default', Type :: dynamic()}
                    | {'size', N :: non_neg_integer()}
                    | (N :: non_neg_integer()).
-type array_opts() :: array_opt() | [array_opt()].

-type indx_pair(Type)  :: {Index :: array_indx(), Type}.
-type indx_pairs(Type) :: [indx_pair(Type)].

%%--------------------------------------------------------------------------

-doc """
Creates a new, extendible array with initial size zero.
""".
-spec new() -> array().

new() ->
    new([]).

-doc """
Creates a new array according to the specified options.

By default, the array is extendible and has initial size zero. Array
indices start at `0`.

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

## Examples

```erlang
1> array:new(100)
```

creates a fixed-size array of size 100.

```
1> array:new({default,0})
```

creates an empty, extendible array whose default value is `0`.

```
1> array:new([{size,10},{fixed,false},{default,-1}])
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

## Examples

```erlang
1> array:new(100, {default,0})
```

Creates a fixed-size array of size 100, whose default value is `0`.
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
    S = find_bits(Size - 1, ?SHIFT),
    C = ?NEW_CACHE(Default),
    #array{size = Size, zero = 0, fix = Fixed, cache = C, cache_index = 0,
           default = Default, elements = ?EMPTY, bits = S}.

-spec find_bits(integer(), non_neg_integer()) -> non_neg_integer().

find_bits(I, S) when I < ?SIZE(?extend(S)) ->
    S;
find_bits(I, S) ->
    find_bits(I, ?extend(S)).

-doc """
Returns `true` if `X` is an array, otherwise `false`.

Notice that the check is only shallow, as there is no guarantee that `X` is a
well-formed array representation even if this function returns `true`.

## Examples

```erlang
1> array:is_array(array:new(4, [])).
true
```

""".
-spec is_array(X :: term()) -> boolean().

is_array(#array{size = Size})
  when is_integer(Size) ->
    true;
is_array(_) ->
    false.


-doc """
Gets the number of entries in the array.

Entries are numbered from `0` to `size(Array)-1`. Hence, this is also
the index of the first entry that is guaranteed to not have been
previously set.

## Examples

```erlang
1> array:size(array:new(4, [])).
4
2> array:size(array:set(5, value, array:new())).
6
```
""".
-spec size(Array :: array()) -> non_neg_integer().

size(#array{size = N}) -> N;
size(_) -> erlang:error(badarg).


-doc """
Gets the value used for uninitialized entries.

## Examples

```erlang
1> array:default(array:new()).
undefined
2> array:get(52, array:new()).
undefined
3> array:default(array:new([{default, 0}])).
0
4> array:get(52, array:new([{default, 0}])).
0
```

See also `new/2`.
""".
-spec default(Array :: array(Type)) -> Value :: Type.

default(#array{default = D}) -> D;
default(_) -> erlang:error(badarg).


-doc """
Fixes the array size to prevent it from growing automatically upon
insertion.

Note that operations which explicitly increase the array size, such as
`append/2`, may still be used on a fixed size array.

## Examples

```erlang
1> array:get(1, array:from_list([a,b,c])).
b
2> array:get(10, array:from_list([a,b,c])).
undefined
3> array:get(10, array:fix(array:from_list([a,b,c]))).
** exception error: bad argument
     in function  array:get/2
```

See also `relax/1`, `set/3`.
""".
-spec fix(Array :: array(Type)) -> array(Type).

fix(#array{}=A) ->
    A#array{fix = true}.

-doc """
Checks if the array has fixed size.

Returns `true` if the array is fixed, otherwise `false`.

## Examples

```erlang
1> array:is_fix(array:new()).
false
2> array:is_fix(array:new({fixed, true})).
true
```

See also `fix/1`.
""".
-spec is_fix(Array :: array()) -> boolean().

is_fix(#array{fix = true}) -> true;
is_fix(#array{}) -> false.


-doc """
Makes the array extendible, reversing the effects of `fix/1`.

## Examples

```erlang
1> array:get(10, array:new({fixed, true})).
** exception error: bad argument
     in function  array:get/2
2> array:get(10, array:relax(array:new())).
undefined
```

See also `fix/1`.
""".
-spec relax(Array :: array(Type)) -> array(Type).

relax(#array{size = N}=A) when is_integer(N), N >= 0 ->
    A#array{fix = false}.


-doc """
Change the array size.

If `Size` is not a non-negative integer, the call fails with reason `badarg`. If
the specified array has fixed size, also the resulting array has fixed size.

Note: As of OTP 29, resizing ensures that entries outside the new range are
pruned so that garbage collection can recover the memory.

## Examples

```erlang
1> array:get(10, array:new({fixed, true})).
** exception error: bad argument
     in function  array:get/2
2> array:get(10, array:resize(20, array:new({fixed, true}))).
undefined
```

See also `shift/2`.
""".
-spec resize(Size :: non_neg_integer(), Array :: array(Type)) ->
                    array(Type).

resize(Size, #array{size = N, zero = Z, cache = C, cache_index = CI, elements = E, default = D, bits = S}=A)
  when is_integer(Size), Size >= 0, is_integer(N), N >= 0,
       is_integer(CI), is_integer(S) ->
    if Size > N ->
            {E1, S1} = grow(Z + Size-1, E, S),
	    A#array{size = Size, elements = E1, bits = S1};
       Size < N ->
            E1 = set_leaf(CI, S, E, C),
            {E2, S1} = shrink(Z + Size-1, S, E1, D),
            CI1 = 0,
            C1 = get_leaf(CI1, S1, E2, D),
	    A#array{size = Size, elements = E2, cache = C1, cache_index = CI1, bits = S1};
       true ->
	    A
    end;
resize(_Size, _) ->
    erlang:error(badarg).

%% like grow(), but only used when explicitly resizing down
shrink(I, _S, _E, _D) when I < 0 ->
    S = find_bits(I, ?SHIFT),
    {?EMPTY, S};
shrink(I, S, E, D) ->
    shrink_1(I, S, E, D).

%% I is the largest index, 0 or more (empty arrays handled above).
%% This first discards any unnecessary tuples from the top
shrink_1(I, _S, ?EMPTY, _D) ->
    S = find_bits(I, ?SHIFT),
    {?EMPTY, S};
shrink_1(I, 0, E, D) ->
    {prune(E, I, D), 0};
shrink_1(I, S, E, D) when I < ?SIZE(S) ->
    shrink_1(I, ?reduce(S), element(1, E), D);
shrink_1(I, S, E, D) ->
    shrink_2(I, S, E, D).

%% Here we have at least one top tuple that should be kept
%% and we must not discard any intermediate levels
shrink_2(_I, S, ?EMPTY, _D) ->
    {?EMPTY, S};
shrink_2(I, 0, E, D) ->
    {prune(E, I, D), 0};
shrink_2(I, S, E, D) ->
    IDiv = I bsr S,
    IRem = I band ?MASK(S),
    E1 = prune(E, IDiv, ?EMPTY),
    I1 = IDiv + 1,
    {E2,_} = shrink_2(IRem, ?reduce(S), element(I1, E1), D),
    {setelement(I1, E1, E2), S}.

prune(E, N, D) when is_tuple(E) ->
    if N < tuple_size(E) - 1 ->
            list_to_tuple(prune(0, N, D, tuple_to_list(E)));
       true ->
            E
    end.

prune(I, N, D, [E|Es]) when I =< N ->
    [E | prune(I+1, N, D, Es)];
prune(I, N, D, [_|Es]) ->
    [D | prune(I+1, N, D, Es)];
prune(_I, _N, _D, []) ->
    [].


-doc """
Changes the array size to that reported by `sparse_size/1`.

If the specified array has fixed size, the resulting array also has
fixed size.

## Examples

```erlang
1> A = array:set(1, x, array:new(4, [])).
2> array:size(A).
4
3> array:size(array:resize(A)).
2
```

See also `resize/2`, `sparse_size/1`.
""".
-spec resize(Array :: array(Type)) -> array(Type).

resize(Array) ->
    %% eqwalizer:ignore ambiguous_union
    resize(sparse_size(Array), Array).


-doc """
Sets entry `I` of the array to `Value`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`.

If the array does not have fixed size, and `I` is greater than `size(Array)-1`,
the array grows to size `I+1`.

## Examples

```erlang
1> A = array:new(4, [{fixed,true}]).
2> array:set(1, x, A).
3> array:set(5, x, A).
** exception error: bad argument
     in function  array:set/3
```

See also `get/2`, `reset/2`.
""".
-spec set(I :: array_indx(), Value :: Type, Array :: array(Type)) -> array(Type).

set(I0, Value, #array{size = N, zero = Z, fix = Fix, cache = C, cache_index = CI,
                     default = D, elements = E, bits = S}=A)
  when is_integer(I0), I0 >= 0, is_integer(N), is_integer(CI), is_integer(S), is_integer(Z) ->
    I = I0 + Z,
    if I0 < N ->
            if I >= CI, I < CI + ?LEAFSIZE ->
                    A#array{cache = setelement(1 + I - CI, C, Value)};
               true ->
                    R = I band ?MASK,
                    CI1 = I - R,
                    E1 = set_leaf(CI, S, E, C),
                    C1 = get_leaf(CI1, S, E1, D),
                    C2 = setelement(1 + R, C1, Value),
                    A#array{elements = E1, cache = C2, cache_index = CI1}
            end;
       Fix ->
	    erlang:error(badarg);
       true ->
            N1 = I0 + 1,
            if I < ?SIZE(?extend(S)) ->
                    R = I band ?MASK,
                    CI1 = I - R,
                    if CI1 =/= CI ->
                            E1 = set_leaf(CI, S, E, C),
                            C1 = get_leaf(CI1, S, E1, D),
                            C2 = setelement(1 + R, C1, Value),
                            A#array{size = N1, elements = E1,
                                    cache = C2, cache_index = CI1};
                       true ->
                            C1 = setelement(1 + R, C, Value),
                            A#array{size = N1, cache = C1, cache_index = CI1}
                    end;
               true ->
                    R = I band ?MASK,
                    CI1 = I - R,
                    {E1,S1} = grow(I, E, S),
                    E2 = set_leaf(CI, S1, E1, C),
                    C1 = get_leaf(CI1, S1, E2, D),
                    C2 = setelement(1 + R, C1, Value),
                    A#array{size = N1, elements = E2,
                            cache = C2, cache_index = CI1, bits = S1}
            end
    end;
set(_I, _V, _A) ->
    erlang:error(badarg).

%% Enlarging the array upwards to accommodate an index `I'

grow(I, ?EMPTY, S) when is_integer(I) ->
    S1 = find_bits(I, S),
    {?EMPTY, S1};
grow(I, E, 0) ->
    grow_1(I, E, 0);
grow(I, E, S) ->
    grow_1(I, E, S).

grow_1(I, E, S) ->
    S1 = ?extend(S),
    if I < ?SIZE(S1) ->
            {E, S};
        true ->
            grow_1(I, setelement(1, ?NEW_NODE(S), E), S1)
    end.

%% similar to get_1
get_leaf(_I, _, ?EMPTY, D) ->
    ?NEW_CACHE(D);
get_leaf(_I, 0, E, _D) ->
    E;
get_leaf(I, S, E, D) ->
    IDiv = (I bsr S) band ?MASK,
    get_leaf(I, ?reduce(S), element(IDiv + 1, E), D).

%% similar to set_1()
set_leaf(_I, 0, _E, C) ->
    C;
set_leaf(I, S, ?EMPTY, C) ->
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
Shift the array a number of steps to the left, or to the right if the
number is negative.

Shifting left drops elements from the left side, reducing the array
size, and shifting right adds space on the left, increasing the array
size.

The fixed option does not affect the result of shift.

Note: For efficiency, this does not prune the representation, which means
that a subsequent shift or similar operation can bring back the values that
were shifted out. Use `resize/2` or `resize/1` if you want to ensure that
values outside the range get pruned.

## Examples

```erlang
1> A = array:new(10, [{fixed, true}]).
2> array:size(A).
10
3> array:size(array:shift(-5, A)).
15
4> array:size(array:shift(5, A)).
5
```
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec shift(Steps :: integer(), Array :: array(Type)) -> array(Type).
shift(0, A=#array{}) ->
    A;
shift(Steps, #array{size = N, zero = Z}=A)
  when is_integer(Steps), is_integer(N), Steps =< N, is_integer(Z) ->
    Z1 = Z + Steps,
    N1 = N - Steps,
    if Z1 >= 0 ->
            A#array{size = N1, zero = Z1};
       true ->
            #array{cache_index = CI, elements = E, bits = S} = A,
            {E1, S1, Z2} = grow_left(Z1, E, S),
            CI1 = CI + (Z2-Z1),
            A#array{size = N1, zero = Z2, cache_index = CI1, elements = E1, bits = S1}
    end;
shift(_Steps, _A) ->
    erlang:error(badarg).

%% Enlarging the array to the left until the zero point is no longer negative.

grow_left(Z, ?EMPTY, S) ->
    grow_left_2(Z, S);
grow_left(Z, E, S) ->
    grow_left_1(Z, E, S).

grow_left_1(Z, E, S) when Z >= 0 ->
    {E, S, Z};
grow_left_1(Z, E, S) ->
    S1 = ?extend(S),
    I = ?NODESIZE div 2,
    grow_left_1(Z + I*?SIZE(S1), setelement(I+1, ?NEW_NODE(S1), E), S1).

grow_left_2(Z, S) when Z >= 0 ->
    {?EMPTY, S, Z};
grow_left_2(Z, S) ->
    S1 = ?extend(S),
    I = ?NODESIZE div 2,
    grow_left_2(Z + I*?SIZE(S1), S1).


-doc """
Extract a slice of the array.

This drops elements before `I` as with `shift/2`, and takes the following
`Length` elements starting from `I`.

If `N` is less than or equal to zero, the resulting array is empty. To extract
a slice from `Start` to `End` inclusive, use `slice(Start, End-Start+1,
Array)`.

Note: For efficiency, this does not prune the representation, which means
that a subsequent shift or similar operation can bring back the values that
were shifted out. Use `resize/2` or `resize/1` if you want to ensure that
values outside the range get pruned.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:to_list(array:slice(2,3,A)).
[2,3,4]
```
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec slice(I :: array_indx(), Length :: non_neg_integer(), Array :: array(Type)) -> array(Type).
slice(I, Length, #array{size = N}=A)
  when is_integer(I), I >= 0, is_integer(N), N >= 0, I + Length =< N ->
    %% eqwalizer:ignore ambiguous_union
    A1 = shift(I, A),
    A1#array{size = Length};
slice(_I, _N, _A) ->
    erlang:error(badarg).


-doc """
Append a single value to the right side of the array.

The operation is always allowed even if the array is fixed.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(array:size(A), array:append(last, A)).
last
```

See also `prepend/2`, `concat/2`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec append(Value :: any(), Array :: array(Type)) -> array(Type).
append(Value, #array{size = N, zero = Z, cache = C, cache_index = CI,
                     default = D, elements = E, bits = S}=A)
  when is_integer(N), is_integer(CI), is_integer(S), is_integer(Z) ->
    I = N + Z,
    N1 = N + 1,
    %% for speed, this is an inlined copy of the growing case from set/3
    %% since append always allows growing
    if
        I < CI + ?LEAFSIZE ->
            A#array{size = N1, cache = setelement(1 + I - CI, C, Value)};
        I < ?SIZE(?extend(S)) ->
            R = I band ?MASK,
            CI1 = I - R,
            E1 = set_leaf(CI, S, E, C),
            C1 = get_leaf(CI1, S, E1, D),
            C2 = setelement(1 + R, C1, Value),
            A#array{size = N1, elements = E1,
                    cache = C2, cache_index = CI1};
       true ->
            R = I band ?MASK,
            CI1 = I - R,
            {E1,S1} = grow(I, E, S),
            E2 = set_leaf(CI, S1, E1, C),
            C1 = get_leaf(CI1, S1, E2, D),
            C2 = setelement(1 + R, C1, Value),
            A#array{size = N1, elements = E2,
                    cache = C2, cache_index = CI1, bits = S1}
    end;
append(_V, _A) ->
    erlang:error(badarg).


-doc """
Prepend a single value to the left side of the array.

The operation is always allowed even if the array is fixed.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(0, array:prepend(first, A)).
first
```

See also `append/2`, `concat/2`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec prepend(Value :: Type, Array :: array(Type)) -> array(Type).
prepend(Value, #array{}=A) ->
    %% eqwalizer:ignore ambiguous_union
    set(0, Value, shift(-1, A)).


-doc """
Gets the value of entry `I`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`.

If the array does not have fixed size, the default value for any index `I`
greater than `size(Array)-1` is returned.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(4,A).
4
3> array:get(10, A).
undefined
```

See also `set/3`.
""".
-spec get(I :: array_indx(), Array :: array(Type)) -> Value :: Type.

get(I0, #array{size = N, zero = Z, fix = Fix, cache = C, cache_index = CI,
               elements = E, default = D, bits = S})
  when is_integer(I0), I0 >= 0, is_integer(N), is_integer(CI), is_integer(S), is_integer(Z) ->
    if I0 < N ->
            I = I0 + Z,
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

get_1(_I, _S, ?EMPTY, D) ->
    D;
get_1(I, 0, E, _D) ->
    element((I band ?MASK)+1, E);
get_1(I, S, E, D) ->
    IDiv = (I bsr S) band ?MASK,
    get_1(I, ?reduce(S), element(IDiv + 1, E), D).

%% TODO: a reset_range function

-doc """
Resets entry `I` to the default value for the array.

If the value of entry `I` is the default value, the array is returned
unchanged.

Reset never changes the array size. Shrinking can be done explicitly by calling
`resize/2`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`; compare
`set/3`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(5, array:reset(5, A)).
undefined
```

See also `new/2`, `set/3`.
""".
-spec reset(I :: array_indx(), Array :: array(Type)) -> array(Type).

reset(I0, #array{size = N, zero = Z, fix = Fix, cache = C, cache_index = CI,
                 default = D, elements = E, bits = S}=A)
  when is_integer(I0), I0 >= 0, is_integer(N), is_integer(CI), is_integer(S), is_integer(Z) ->
    if I0 < N ->
            I = I0 + Z,
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

reset_1(_I, _, ?EMPTY, _D) ->
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
Concatenates two arrays.

Adds the elements of `B` onto `A`.

## Examples

```erlang
1> A = array:set(1, a, array:new([{default, xa}, {size,3}, {fixed, true}])).
2> B = array:set(2, b, array:new([{default, xb}, {size,4}, {fixed, false}])).
3> AB = array:concat(A,B).
4> array:to_list(AB).
[xa,a,xa,xb,xb,b,xb]
```

See also `concat/1`, `append/2`, `prepend/2`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec concat(A :: array(Type), B :: array(Type)) -> AB :: array(Type).

concat(#array{size = LeftN, fix = Fix, default = DefA}=Left,
       #array{size = RightN, default = DefB}=Right) ->
    if RightN > LeftN, DefA =:= DefB ->
            %% eqwalizer:ignore ambiguous_union
            foldr(fun (_I, V, Acc) -> prepend(V, Acc) end, Right#array{fix = Fix}, Left);
       true ->
            %% eqwalizer:ignore ambiguous_union
            foldl(fun (_I, V, Acc) -> append(V, Acc) end, Left, Right)
    end;
concat(_, _) ->
    erlang:error(badarg).

-doc """
Concatenates a nonempty list of arrays.

## Examples

```erlang
1> A = array:from_list([a]).
2> B = array:from_list([b]).
3> array:to_list(array:concat([A,B])).
[a,b]
```

See also `concat/2`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec concat(Arrays :: [array(Type)]) -> array(Type).

concat([A0|As]) ->
    %% eqwalizer:ignore ambiguous_union
    lists:foldl(fun (A, Acc) -> concat(Acc, A) end, A0, As);
concat(_) ->
    erlang:error(badarg).


-doc """
Converts the array to a list.

## Examples

```erlang
1> A = array:set(2, x, array:new()).
2> array:to_list(A).
[undefined,undefined,x]
```

See also `from_list/2`, `sparse_to_list/1` and `to_orddict/1`.
""".
-spec to_list(Array :: array(Type)) -> list(Value :: Type).

to_list(Array) ->
    %% eqwalizer:ignore ambiguous_union
    foldr(fun (_I, V, A) -> [V|A] end, [], Array).

-doc """
Converts the array to a list, skipping default-valued entries.

## Examples

```erlang
1> A = array:set(2, x, array:new()).
2> array:to_list(A).
[undefined,undefined,x]
3> array:sparse_to_list(A).
[x]
```

See also `to_list/1`  and `to_orddict/1`.
""".
-spec sparse_to_list(Array :: array(Type)) -> list(Value :: Type).

sparse_to_list(Array) ->
    %% eqwalizer:ignore ambiguous_union
    sparse_foldr(fun (_I, V, A) -> [V|A] end, [], Array).


-doc "Equivalent to [`from_list(List, undefined)`](`from_list/2`).".
-spec from_list(List :: list(Value :: Type)) -> array(Type).

from_list(List) ->
    from_list(List, undefined).

-doc """
Converts a list to an extendible array.

`Default` is used as the value for uninitialized entries of the array.

If `List` is not a proper list, the call fails with reason `badarg`.

Note: Use `fix/1` on the resulting array if you want to prevent accesses
outside the size range.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,2), default).
2> array:to_list(array:reset(1, A)).
[0,default,2]
```

See also `new/2`, `to_list/1`.
""".
-spec from_list(List :: list(Value :: Type), Default :: term()) -> array(Type).

from_list([], Default) ->
    new({default,Default});
from_list(List, Default) when is_list(List) ->
    {E, N, S0} = from_list_1(?LEAFSIZE, List, Default, 0, [], []),
    CI = 0,
    S = ?reduce(S0),
    C = get_leaf(CI, S, E, Default),
    #array{size = N, zero = 0, fix = false, cache = C, cache_index = CI,
           default = Default, elements = E, bits = S};
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
    from_list_2(?NODESIZE, pad(((N-1) bsr S) + 1, ?NODESIZE, ?EMPTY, Es),
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
    push_n((K - (N rem K)) rem K, P, Es).

push_n(0, _E, L) ->
    L;
push_n(N, E, L) ->
    push_n(N - 1, E, [E | L]).


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

Note: Use `fix/1` on the resulting array if you want to prevent accesses
outside the size range.

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

from(Fun, St0, Default) when is_function(Fun, 1) ->
    VS = Fun(St0),
    {E, N, S0} = from_fun_1(?LEAFSIZE, Default, Fun, VS, 0, [], []),
    CI = 0,
    S = ?reduce(S0),
    C = get_leaf(CI, S, E, Default),
    #array{size = N, zero = 0, fix = false, cache = C, cache_index = CI,
           default = Default, elements = E, bits = S};
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

## Examples

```erlang
1> A = array:from_list(lists:seq(0,2), default).
2> array:to_orddict(array:reset(1, A)).
[{0,0},{1,default},{2,2}]
```

See also `from_orddict/2`, `sparse_to_orddict/1` and `to_list/1`.
""".
-spec to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).

to_orddict(Array) ->
    %% eqwalizer:ignore ambiguous_union
    foldr(fun (I, V, A) -> [{I,V}|A] end, [], Array).


-doc """
Converts the array to an ordered list of pairs `{Index, Value}`, skipping
default-valued entries.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,2), default).
2> array:to_orddict(array:reset(1, A)).
[{0,0},{1,default},{2,2}]
3> array:sparse_to_orddict(array:reset(1, A)).
[{0,0},{2,2}]
```

See also `to_orddict/1`.
""".
-spec sparse_to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).

sparse_to_orddict(Array) ->
    %% eqwalizer:ignore ambiguous_union
    sparse_foldr(fun (I, V, A) -> [{I,V}|A] end, [], Array).


-doc "Equivalent to [`from_orddict(Orddict, undefined)`](`from_orddict/2`).".
-spec from_orddict(Orddict :: indx_pairs(Value :: Type)) -> array(Type).

from_orddict(Orddict) ->
    from_orddict(Orddict, undefined).

-doc """
Converts an ordered list of pairs `{Index, Value}` to a corresponding extendible
array.

`Default` is used as the value for uninitialized entries of the array.

If `Orddict` is not a proper, ordered list of pairs whose first elements are
non-negative integers, the call fails with reason `badarg`.

Note: Use `fix/1` on the resulting array if you want to prevent accesses
outside the size range.

## Examples

```erlang
1> A = array:from_orddict([{K,V} || K <:- lists:seq(2,4) && V <- [v1,v2,v3]], vx).
2> array:to_orddict(A).
[{0,vx},{1,vx},{2,v1},{3,v2},{4,v3}]
```

See also `new/2`, `to_orddict/1`.
""".
-spec from_orddict(Orddict :: indx_pairs(Value :: Type), Default :: dynamic()) ->
                          array(Type).

from_orddict([], Default) ->
    new({default,Default});
from_orddict(List, Default) when is_list(List) ->
    {E, N, S0} = from_orddict_0(List, 0, ?LEAFSIZE, Default, []),
    CI = 0,
    S = ?reduce(S0),
    C = get_leaf(CI, S, E, Default),
    #array{size = N, zero = 0, fix = false, cache = C, cache_index = CI,
           default = Default, elements = E, bits = S};
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
	    As = push_n(Step0, ?EMPTY, As0),
	    collect_leafs(I-Step0, Xs, S, N, As, Es0);
	I =:= ?NODESIZE ->
	    Step = Step0 rem ?NODESIZE,
	    As = push_n(Step, ?EMPTY, As0),
	    collect_leafs(I-Step, Xs, S, N, As, [X|Es0]);
	I =:= Step0 ->
	    As = push_n(I, ?EMPTY, As0),
	    collect_leafs(0, Xs, S, N, As, Es0);
	true ->
	    As = push_n(I, ?EMPTY, As0),
	    Step = Step0 - I,
	    collect_leafs(0, [Step bsl S|Xs], S, N, As, Es0)
    end;
collect_leafs(I, [X | Xs], S, N, As, Es) ->
    collect_leafs(I-1, Xs, S, N, [X | As], Es);
collect_leafs(?NODESIZE, [], S, N, [], Es) ->
    collect_leafs(N, lists:reverse(Es), ?extend(S)).

%%    Function = (Index::integer(), Value::term()) -> term()

-doc """
Maps the specified function onto each array element.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,3)).
2> B = array:map(fun(K, V) -> K*V end, A).
3> array:to_orddict(B).
[{0,0},{1,1},{2,4},{3,9}]
```

See also `mapfoldl/3`, `sparse_map/2`.
""".
-spec map(Function, Array :: array(Type1)) -> array(Type1 | Type2) when
      Function :: fun((Index :: array_indx(), Type1) -> Type2).

map(Function, Array) when is_function(Function, 2) ->
    %% eqwalizer:ignore ambiguous_union
    {Array1, _} = mapfoldl(fun (I, V, _) -> {Function(I, V), []} end, [], Array),
    Array1;
map(_, _) ->
    erlang:error(badarg).


-doc """
Maps the specified function onto each array element, skipping default-valued
entries.

The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `map/2`.
""".
-spec sparse_map(Function, Array :: array(Type1)) -> array(Type1 | Type2) when
      Function :: fun((Index :: array_indx(), Type1) -> Type2).

sparse_map(Function, Array) when is_function(Function, 2) ->
    %% eqwalizer:ignore ambiguous_union
    {Array1, _} = sparse_mapfoldl(fun (I, V, _) -> {Function(I, V), []} end, [], Array),
    Array1;
sparse_map(_, _) ->
    erlang:error(badarg).


-doc """
Folds the array elements using the specified function and initial accumulator
value.

The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,3)).
2> array:foldl(fun(_K, V, Acc) -> V+Acc end, 0, A).
6
```

See also `foldl/5`, `foldr/3`, `sparse_foldl/3`.
""".
-spec foldl(Function, InitialAcc :: A, Array :: array(Type)) -> A when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

foldl(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    %% eqwalizer:ignore ambiguous_union
    foldl(0, N-1, Function, Acc, Array);
foldl(_, _, _) ->
    erlang:error(badarg).

-doc """
Folds the array elements from `Low` to `High` using the specified function and
initial accumulator value.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,100)).
2> array:foldl(50, 59, fun(_K, V, Acc) -> V+Acc end, 0, A).
545
```

See also `foldl/3`, `sparse_foldl/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec foldl(Low, High, Function, InitialAcc :: A, Array) -> A when
      Low :: array_indx(),
      High :: array_indx(),
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A),
      Array :: array(Type).

foldl(Low, High, Function, Acc,
      #array{size = N, zero = Z, cache = C, cache_index = CI, elements = E, default = D, bits = S})
  when is_integer(Low), Low >= 0, is_integer(High), is_integer(N), High < N,
       is_function(Function, 3),
       is_integer(Z), is_integer(CI), is_integer(S) ->
    if Low =< High ->
            E1 = set_leaf(CI, S, E, C),
            foldl_1(Low + Z, High + Z, Low, S, E1, D, Function, Acc);
       true ->
            Acc
    end;
foldl(_, _, _, _, _) ->
    erlang:error(badarg).


foldl_1(Low, High, Ix, S, ?EMPTY, D, F, A) ->
    foldl_4(Low, High, Ix, S, D, F, A);
foldl_1(Low, High, Ix, 0, E, _D, F, A) ->
    foldl_3(Low, High, Ix, E, F, A);
foldl_1(Low, High, Ix, S, E, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            foldl_1(LRem, HRem, Ix, ?reduce(S), element(LDiv+1, E), D, F, A);
       true ->
            A1 = foldl_1(LRem, ?SIZE(S)-1, Ix, ?reduce(S), element(LDiv+1, E), D, F, A),
            foldl_2(LDiv+1, HDiv, Ix + ?SIZE(S) - LRem, S, E, D, F, A1, HRem)
    end.

foldl_2(Low, High, Ix, S, E, D, F, A, HRem) when Low < High ->
    A1 = foldl_1(0, ?SIZE(S)-1, Ix, ?reduce(S), element(Low+1, E), D, F, A),
    foldl_2(Low+1, High, Ix + ?SIZE(S), S, E, D, F, A1, HRem);
foldl_2(Low, _High, Ix, S, E, D, F, A, HRem) ->
    foldl_1(0, HRem, Ix, ?reduce(S), element(Low+1, E), D, F, A).


-spec foldl_3(array_indx(), array_indx(), array_indx(), tuple(),
	      fun((array_indx(), _, A) -> A), A) -> A.

foldl_3(Low, High, Ix, E, F, A) when Low =< High ->
    foldl_3(Low+1, High, Ix+1, E, F, F(Ix, element(Low+1, E), A));
foldl_3(_Low, _High, _Ix, _E, _F, A) ->
    A.

%% unexpanded tree
foldl_4(Low, High, Ix, 0, D, F, A) ->
    foldl_6(Low, High, Ix, D, F, A);
foldl_4(Low, High, Ix, S, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            foldl_4(LRem, HRem, Ix, ?reduce(S), D, F, A);
       true ->
            A1 = foldl_4(LRem, ?SIZE(S)-1, Ix, ?reduce(S), D, F, A),
            foldl_5(LDiv+1, HDiv, Ix + ?SIZE(S) - LRem, S, D, F, A1, HRem)
    end.

foldl_5(Low, High, Ix, S, D, F, A, HRem) when Low < High ->
    A1 = foldl_4(0, ?SIZE(S)-1, Ix, ?reduce(S), D, F, A),
    foldl_5(Low+1, High, Ix + ?SIZE(S), S, D, F, A1, HRem);
foldl_5(_Low, _High, Ix, S, D, F, A, HRem) ->
    foldl_4(0, HRem, Ix, ?reduce(S), D, F, A).

foldl_6(Low, High, Ix, D, F, A) when Low =< High ->
    foldl_6(Low+1, High, Ix+1, D, F, F(Ix, D, A));
foldl_6(_Low, _High, _Ix, _D, _F, A) ->
    A.

-doc """
Folds the array elements using the specified function and initial accumulator
value, skipping default-valued entries.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldl/5`, `foldl/3`.
""".
-spec sparse_foldl(Function, InitialAcc :: A, Array :: array(Type)) -> A when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

sparse_foldl(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    %% eqwalizer:ignore ambiguous_union
    sparse_foldl(0, N-1, Function, Acc, Array);
sparse_foldl(_, _, _) ->
    erlang:error(badarg).


-doc """
Folds the array elements from `Low` to `High` using the specified
function and initial accumulator value, skipping default-valued entries.

The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldl/3`, `foldl/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec sparse_foldl(Low :: array_indx(), High :: array_indx(), Function,
                   InitialAcc :: A, Array :: array(Type)) -> A when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

sparse_foldl(Low, High, Function, InitialAcc, #array{default = D}=Array)
  when is_function(Function, 3) ->
    Skip = fun (_I, V, A) when V =:= D -> A;
               (I, V, A) -> Function(I, V, A)
           end,
    %% eqwalizer:ignore ambiguous_union
    foldl(Low, High, Skip, InitialAcc, Array);
sparse_foldl(_, _, _, _, _) ->
    erlang:error(badarg).


-doc """
Folds the array elements right-to-left using the specified function and initial
accumulator value.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldr/5`, `foldl/3`, `sparse_foldr/3`.
""".
-spec foldr(Function, InitialAcc :: A, Array :: array(Type)) -> A when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

foldr(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    %% eqwalizer:ignore ambiguous_union
    foldr(0, N-1, Function, Acc, Array);
foldr(_, _, _) ->
    erlang:error(badarg).


-doc """
Folds the array elements from `High` to `Low` using the specified function and
initial accumulator value.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldr/3`, `foldl/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec foldr(Low, High, Function, InitialAcc :: A, Array :: array(Type)) -> A when
      Low :: array_indx(),
      High :: array_indx(),
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

foldr(Low, High, Function, Acc, #array{size = N, zero = Z, cache = C,
                                       cache_index = CI, elements = E, default = D, bits = S})
  when is_integer(Low), Low >= 0, is_integer(High), is_function(Function, 3),
       is_integer(N), High < N, is_integer(Z), is_integer(CI), is_integer(S) ->
    if Low =< High ->
            E1 = set_leaf(CI, S, E, C),
            foldr_1(Low + Z, High + Z, High, S, E1, D, Function, Acc);
       true ->
            Acc
    end;
foldr(_, _, _, _, _) ->
    erlang:error(badarg).

foldr_1(Low, High, Ix, S, ?EMPTY, D, F, A) ->
    foldr_4(Low, High, Ix, S, D, F, A);
foldr_1(Low, High, Ix, 0, E, _D, F, A) ->
    foldr_3(Low, High, Ix, E, F, A);
foldr_1(Low, High, Ix, S, E, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            foldr_1(LRem, HRem, Ix, ?reduce(S), element(HDiv+1, E), D, F, A);
       true ->
            A1 = foldr_1(0, HRem, Ix, ?reduce(S), element(HDiv+1, E), D, F, A),
            foldr_2(LDiv, HDiv-1, Ix - HRem - 1, S, E, D, F, A1, LRem)
    end.

foldr_2(Low, High, Ix, S, E, D, F, A, LRem) when Low < High ->
    A1 = foldr_1(0, ?SIZE(S)-1, Ix, ?reduce(S), element(High+1, E), D, F, A),
    foldr_2(Low, High-1, Ix - ?SIZE(S), S, E, D, F, A1, LRem);
foldr_2(_Low, High, Ix, S, E, D, F, A, LRem) ->
    foldr_1(LRem, ?SIZE(S)-1, Ix, ?reduce(S), element(High+1, E), D, F, A).

foldr_3(Low, High, Ix, E, F, A) when Low =< High ->
    foldr_3(Low, High-1, Ix-1, E, F, F(Ix, element(High+1, E), A));
foldr_3(_Low, _High, _Ix, _D, _F, A) ->
    A.

%% unexpanded tree
foldr_4(Low, High, Ix, 0, D, F, A) ->
    foldr_6(Low, High, Ix, D, F, A);
foldr_4(Low, High, Ix, S, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            foldr_4(LRem, HRem, Ix, ?reduce(S), D, F, A);
       true ->
            A1 = foldr_4(0, HRem, Ix, ?reduce(S), D, F, A),
            foldr_5(LDiv, HDiv-1, Ix - HRem - 1, S, D, F, A1, LRem)
    end.

foldr_5(Low, High, Ix, S, D, F, A, LRem) when Low < High ->
    A1 = foldr_4(0, ?SIZE(S)-1, Ix, ?reduce(S), D, F, A),
    foldr_5(Low, High-1, Ix - ?SIZE(S), S, D, F, A1, LRem);
foldr_5(_Low, _High, Ix, S, D, F, A, LRem) ->
    foldr_4(LRem, ?SIZE(S)-1, Ix, ?reduce(S), D, F, A).

foldr_6(Low, High, Ix, D, F, A) when Low =< High ->
    foldr_6(Low, High-1, Ix-1, D, F, F(Ix, D, A));
foldr_6(_Low, _High, _Ix, _D, _F, A) ->
    A.

-doc """
Folds the array elements right-to-left using the specified function and initial
accumulator value, skipping default-valued entries.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldr/5`, `foldr/3`, `sparse_foldl/3`.
""".
-spec sparse_foldr(Function, InitialAcc :: A, Array :: array(Type)) -> A when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

sparse_foldr(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    %% eqwalizer:ignore ambiguous_union
    sparse_foldr(0, N-1, Function, Acc, Array);
sparse_foldr(_, _, _) ->
    erlang:error(badarg).


-doc """
Folds the array elements from `High` to `Low` using the specified
function and initial accumulator value, skipping default-valued entries.

The elements are visited in order from the highest index to the lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldr/3`, `foldr/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec sparse_foldr(Low :: array_indx(), High :: array_indx(), Function,
                   InitialAcc :: A, Array :: array(Type)) -> A when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).

sparse_foldr(Low, High, Function, InitialAcc, #array{default = D}=Array)
  when is_function(Function, 3) ->
    Skip = fun (_I, V, A) when V =:= D -> A;
               (I, V, A) -> Function(I, V, A)
           end,
    %% eqwalizer:ignore ambiguous_union
    foldr(Low, High, Skip, InitialAcc, Array);
sparse_foldr(_, _, _, _, _) ->
    erlang:error(badarg).


-doc """
Gets the number of entries in the array up until the last non-default-valued
entry.

That is, returns `I+1` if `I` is the last non-default-valued entry in
the array, or zero if no such entry exists.

## Examples

```erlang
1> A = array:set(3, 42, array:new(10)).
2> array:size(A).
10
3> array:sparse_size(A).
4
```

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


-doc """
Combined map and fold over the array elements using the specified
function and initial accumulator value.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,3)).
2> {B, Acc} = array:mapfoldl(fun(K, V, Sum) -> {K*V, V+Sum} end, 0, A).
3> Acc.
6
4> array:to_orddict(B).
[{0,0}, {1,1}, {2,4}, {3,9}]
```

See also `mapfoldl/5`, `foldl/3`, `map/2`, `sparse_mapfoldl/3`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec mapfoldl(Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A} when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).

mapfoldl(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    mapfoldl(0, N-1, Function, Acc, Array);
mapfoldl(_, _, _) ->
    erlang:error(badarg).

-doc """
Combined map and fold over the array elements from `Low` to `High` using
the specified function and initial accumulator value.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `mapfoldl/3`, `sparse_mapfoldl/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec mapfoldl(Low, High, Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A} when
      Low :: array_indx(),
      High :: array_indx(),
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).

mapfoldl(Low, High, Function, Acc, #array{size = N, zero = Z, cache = C, cache_index = CI,
                                          elements = E, default = D, bits = S}=Array)
  when is_integer(Low), Low >= 0, is_integer(High), is_function(Function, 3),
       is_integer(N), High < N, is_integer(Z), is_integer(CI), is_integer(S) ->
    if Low =< High ->
            E0 = set_leaf(CI, S, E, C),
            {E1, Acc1} = mapfoldl_1(Low + Z, High + Z, Low, S, E0, D, Function, Acc),
            C1 = get_leaf(CI, S, E1, D),
            {Array#array{elements = E1, cache = C1}, Acc1};
       true ->
            {Array, Acc}
    end;
mapfoldl(_, _, _, _, _) ->
    erlang:error(badarg).

mapfoldl_1(Low, High, Ix, S, ?EMPTY, D, F, A) ->
    mapfoldl_1(Low, High, Ix, S, unfold(S, D), D, F, A);
mapfoldl_1(Low, High, Ix, 0, E, _D, F, A) ->
    mapfoldl_3(Low, High, Ix, tuple_to_list(E), F, A, [], 0);
mapfoldl_1(Low, High, Ix, S, E, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            {E1, A1} = mapfoldl_1(LRem, HRem, Ix, ?reduce(S), element(LDiv+1, E), D, F, A),
            {setelement(LDiv+1, E, E1), A1};
       true ->
            Es = tuple_to_list(E),
            {Es1, A1} = mapfoldl_2(LDiv, HDiv, Ix, S, Es, D, F, A, HRem, [], LRem, 0),
            {list_to_tuple(Es1), A1}
    end.

mapfoldl_2(Low, High, Ix, S, [E|Es], D, F, A, HRem, Es1, LRem, I) when I < Low ->
    mapfoldl_2(Low, High, Ix, S, Es, D, F, A, HRem, [E|Es1], LRem, I + 1);
mapfoldl_2(Low, High, Ix, S, [E|Es], D, F, A, HRem, Es1, LRem, _I) ->
    {E1, A1} = mapfoldl_1(LRem, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    mapfoldl_2_1(Low+1, High, Ix + ?SIZE(S) - LRem, S, Es, D, F, A1, HRem, [E1|Es1]).

mapfoldl_2_1(Low, High, Ix, S, [E|Es], D, F, A, HRem, Es1) when Low < High ->
    {E1, A1} = mapfoldl_1(0, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    mapfoldl_2_1(Low+1, High, Ix + ?SIZE(S), S, Es, D, F, A1, HRem, [E1|Es1]);
mapfoldl_2_1(_Low, _High, Ix, S, [E|Es], D, F, A, HRem, Es1) ->
    {E1, A1} = mapfoldl_1(0, HRem, Ix, ?reduce(S), E, D, F, A),
    {lists:reverse(lists:reverse(Es, [E1|Es1])), A1}.

mapfoldl_3(Low, High, Ix, [E|Es], F, A, Es1, I) when I < Low ->
    mapfoldl_3(Low, High, Ix, Es, F, A, [E|Es1], I + 1);
mapfoldl_3(Low, High, Ix, Es, F, A, Es1, _I) ->
    mapfoldl_3_1(Low, High, Ix, Es, F, A, Es1).

mapfoldl_3_1(Low, High, Ix, [E|Es], F, A, Es1) when Low =< High ->
    {E1, A1} = F(Ix, E, A),
    mapfoldl_3_1(Low+1, High, Ix+1, Es, F, A1, [E1|Es1]);
mapfoldl_3_1(_Low, _High, _Ix, Es, _F, A, Es1) ->
    {list_to_tuple(lists:reverse(lists:reverse(Es, Es1))), A}.

unfold(S, _D) when S > 0 ->
    ?NEW_NODE(S);
unfold(_S, D) ->
    ?NEW_LEAF(D).


-doc """
Like `mapfoldl/3` but skips default-valued entries.

See also `sparse_mapfoldl/5`, `sparse_mapfoldr/3`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec sparse_mapfoldl(Function, InitialAcc :: A, Array) -> {ArrayRes, A} when
      Array :: array(Type1),
      Function :: fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
      ArrayRes :: array(Type1 | Type2).

sparse_mapfoldl(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    %% eqwalizer:ignore ambiguous_union
    sparse_mapfoldl(0, N-1, Function, Acc, Array);
sparse_mapfoldl(_, _, _) ->
    erlang:error(badarg).

-doc """
Like `mapfoldl/5` but skips default-valued entries.

See also `sparse_mapfoldl/3`, `sparse_mapfoldr/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec sparse_mapfoldl(Low, High, Function, InitialAcc :: A, Array) -> {ArrayRes, A} when
      Low :: array_indx(),
      High :: array_indx(),
      Function :: fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
      Array :: array(Type1),
      ArrayRes :: array(Type1 | Type2).

sparse_mapfoldl(Low, High, Function, Acc, #array{size = N, zero = Z, cache = C, cache_index = CI,
                                                 elements = E, default = D, bits = S}=Array)
  when is_integer(Low), Low >= 0, is_integer(High), is_function(Function, 3), is_integer(N),
       High < N, is_integer(Z), is_integer(CI), is_integer(S) ->
    if Low =< High ->
            E0 = set_leaf(CI, S, E, C),
            {E1, Acc1} = sparse_mapfoldl_1(Low + Z, High + Z, Low, S, E0, D, Function, Acc),
            C1 = get_leaf(CI, S, E1, D),
            {Array#array{elements = E1, cache = C1}, Acc1};
       true ->
            {Array, Acc}
    end;
sparse_mapfoldl(_, _, _, _, _) ->
    erlang:error(badarg).

sparse_mapfoldl_1(_Low, _High, _Ix, _S, ?EMPTY, _D, _F, A) ->
    {?EMPTY, A};
sparse_mapfoldl_1(Low, High, Ix, 0, E, D, F, A) ->
    sparse_mapfoldl_3(Low, High, Ix, tuple_to_list(E), D, F, A, [], 0);
sparse_mapfoldl_1(Low, High, Ix, S, E, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            {E1, A1} = sparse_mapfoldl_1(LRem, HRem, Ix, ?reduce(S), element(LDiv+1, E), D, F, A),
            {setelement(LDiv+1, E, E1), A1};
       true ->
            Es = tuple_to_list(E),
            {Es1, A1} = sparse_mapfoldl_2(LDiv, HDiv, Ix, S, Es, D, F, A, HRem, [], LRem, 0),
            {list_to_tuple(Es1), A1}
    end.

sparse_mapfoldl_2(Low, High, Ix, S, [E|Es], D, F, A, HRem, Es1, LRem, I) when I < Low ->
    sparse_mapfoldl_2(Low, High, Ix, S, Es, D, F, A, HRem, [E|Es1], LRem, I + 1);
sparse_mapfoldl_2(Low, High, Ix, S, [E|Es], D, F, A, HRem, Es1, LRem, _I) ->
    {E1, A1} = sparse_mapfoldl_1(LRem, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    sparse_mapfoldl_2_1(Low+1, High, Ix + ?SIZE(S) - LRem, S, Es, D, F, A1, HRem, [E1|Es1]).

sparse_mapfoldl_2_1(Low, High, Ix, S, [E|Es], D, F, A, HRem, Es1) when Low < High ->
    {E1, A1} = sparse_mapfoldl_1(0, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    sparse_mapfoldl_2_1(Low+1, High, Ix + ?SIZE(S), S, Es, D, F, A1, HRem, [E1|Es1]);
sparse_mapfoldl_2_1(_Low, _High, Ix, S, [E|Es], D, F, A, HRem, Es1) ->
    {E1, A1} = sparse_mapfoldl_1(0, HRem, Ix, ?reduce(S), E, D, F, A),
    {lists:reverse(lists:reverse(Es, [E1|Es1])), A1}.

sparse_mapfoldl_3(Low, High, Ix, [E|Es], D, F, A, Es1, I) when I < Low ->
    sparse_mapfoldl_3(Low, High, Ix, Es, D, F, A, [E|Es1], I + 1);
sparse_mapfoldl_3(Low, High, Ix, Es, D, F, A, Es1, _I) ->
    sparse_mapfoldl_3_1(Low, High, Ix, Es, D, F, A, Es1).

sparse_mapfoldl_3_1(Low, High, Ix, [E|Es], D, F, A, Es1) when Low =< High ->
    if E =:= D ->
            sparse_mapfoldl_3_1(Low+1, High, Ix+1, Es, D, F, A, [E|Es1]);
       true ->
            {E1, A1} = F(Ix, E, A),
            sparse_mapfoldl_3_1(Low+1, High, Ix+1, Es, D, F, A1, [E1|Es1])
    end;
sparse_mapfoldl_3_1(_Low, _High, _Ix, Es, _D, _F, A, Es1) ->
    {list_to_tuple(lists:reverse(lists:reverse(Es, Es1))), A}.


-doc """
Combined map and fold over the array elements using the specified
function and initial accumulator value.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `mapfoldr/5`, `foldr/3`, `map/2`, `sparse_mapfoldr/3`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec mapfoldr(Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A} when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).

mapfoldr(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    mapfoldr(0, N-1, Function, Acc, Array);
mapfoldr(_, _, _) ->
    erlang:error(badarg).

-doc """
Combined map and fold over the array elements from `Low` to `High` using
the specified function and initial accumulator value.

The elements are visited in order from the highest index to the lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `mapfoldr/3`, `mapfoldl/5`, `sparse_mapfoldr/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec mapfoldr(Low, High, Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A} when
      Low :: array_indx(),
      High :: array_indx(),
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).

mapfoldr(Low, High, Function, Acc, #array{size = N, zero = Z, cache = C, cache_index = CI,
                                          elements = E, default = D, bits = S}=Array)
  when is_integer(Low), Low >= 0, is_integer(High), is_function(Function, 3), is_integer(N),
       High < N, is_integer(Z), is_integer(CI), is_integer(S) ->
    if Low =< High ->
            E0 = set_leaf(CI, S, E, C),
            {E1, Acc1} = mapfoldr_1(Low + Z, High + Z, High, S, E0, D, Function, Acc),
            C1 = get_leaf(CI, S, E1, D),
            {Array#array{elements = E1, cache = C1}, Acc1};
       true ->
            {Array, Acc}
    end;
mapfoldr(_, _, _, _, _) ->
    erlang:error(badarg).

mapfoldr_1(Low, High, Ix, S, ?EMPTY, D, F, A) ->
    mapfoldr_1(Low, High, Ix, S, unfold(S, D), D, F, A);
mapfoldr_1(Low, High, Ix, 0, E, _D, F, A) ->
    mapfoldr_3(Low, High, Ix, lists:reverse(tuple_to_list(E)), F, A, [], ?LEAFSIZE-1);
mapfoldr_1(Low, High, Ix, S, E, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            {E1, A1} = mapfoldr_1(LRem, HRem, Ix, ?reduce(S), element(HDiv+1, E), D, F, A),
            {setelement(HDiv+1, E, E1), A1};
       true ->
            Es = lists:reverse(tuple_to_list(E)),
            {Es1, A1} = mapfoldr_2(LDiv, HDiv, Ix, S, Es, D, F, A, LRem, [], HRem, ?NODESIZE-1),
            {list_to_tuple(Es1), A1}
    end.

mapfoldr_2(Low, High, Ix, S, [E|Es], D, F, A, LRem, Es1, HRem, I) when High < I ->
    mapfoldr_2(Low, High, Ix, S, Es, D, F, A, LRem, [E|Es1], HRem, I - 1);
mapfoldr_2(Low, High, Ix, S, [E|Es], D, F, A, LRem, Es1, HRem, _I) ->
    {E1, A1} = mapfoldr_1(0, HRem, Ix, ?reduce(S), E, D, F, A),
    mapfoldr_2_1(Low, High-1, Ix - HRem - 1, S, Es, D, F, A1, LRem, [E1|Es1]).

mapfoldr_2_1(Low, High, Ix, S, [E|Es], D, F, A, LRem, Es1) when Low < High ->
    {E1, A1} = mapfoldr_1(0, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    mapfoldr_2_1(Low, High-1, Ix - ?SIZE(S), S, Es, D, F, A1, LRem, [E1|Es1]);
mapfoldr_2_1(_Low, _High, Ix, S, [E|Es], D, F, A, LRem, Es1) ->
    {E1, A1} = mapfoldr_1(LRem, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    {lists:reverse(Es, [E1|Es1]), A1}.

mapfoldr_3(Low, High, Ix, [E|Es], F, A, Es1, I) when High < I ->
    mapfoldr_3(Low, High, Ix, Es, F, A, [E|Es1], I - 1);
mapfoldr_3(Low, High, Ix, Es, F, A, Es1, _I) ->
    mapfoldr_3_1(Low, High, Ix, Es, F, A, Es1).

mapfoldr_3_1(Low, High, Ix, [E|Es], F, A, Es1) when Low =< High ->
    {E1, A1} = F(Ix, E, A),
    mapfoldr_3_1(Low, High-1, Ix-1, Es, F, A1, [E1|Es1]);
mapfoldr_3_1(_Low, _High, _Ix, Es, _F, A, Es1) ->
    {list_to_tuple(lists:reverse(Es, Es1)), A}.


-doc """
Like `mapfoldr/3` but skips default-valued entries.

See also `sparse_mapfoldr/5`, `sparse_mapfoldl/3`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec sparse_mapfoldr(Function, InitialAcc :: A, Array) -> {ArrayRes, A} when
      Array :: array(Type1),
      Function :: fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
      ArrayRes :: array(Type1 | Type2).

sparse_mapfoldr(Function, Acc, #array{size = N}=Array) when is_integer(N) ->
    sparse_mapfoldr(0, N-1, Function, Acc, Array);
sparse_mapfoldr(_, _, _) ->
    erlang:error(badarg).

-doc """
Like `mapfoldr/5` but skips default-valued entries.

See also `sparse_mapfoldr/3`, `sparse_mapfoldl/5`.
""".
-doc #{ since => ~"OTP @OTP-20004@" }.
-spec sparse_mapfoldr(Low, High, Function, InitialAcc :: A, Array) -> {ArrayRes, A} when
      Low :: array_indx(),
      High :: array_indx(),
      Function :: fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
      Array :: array(Type1),
      ArrayRes :: array(Type1 | Type2).

sparse_mapfoldr(Low, High, Function, Acc, #array{size = N, zero = Z, cache = C, cache_index = CI,
                                                 elements = E, default = D, bits = S}=Array)
  when is_integer(Low), Low >= 0, is_integer(High), is_function(Function, 3), is_integer(N),
       High < N, is_integer(Z), is_integer(CI), is_integer(S) ->
    if Low =< High ->
            E0 = set_leaf(CI, S, E, C),
            {E1, Acc1} = sparse_mapfoldr_1(Low + Z, High + Z, High, S, E0, D, Function, Acc),
            C1 = get_leaf(CI, S, E1, D),
            {Array#array{elements = E1, cache = C1}, Acc1};
       true ->
            {Array, Acc}
    end;
sparse_mapfoldr(_, _, _, _, _) ->
    erlang:error(badarg).

sparse_mapfoldr_1(_Low, _High, _Ix, _S, ?EMPTY, _D, _F, A) ->
    {?EMPTY, A};
sparse_mapfoldr_1(Low, High, Ix, 0, E, D, F, A) ->
    sparse_mapfoldr_3(Low, High, Ix, lists:reverse(tuple_to_list(E)), D, F, A, [], ?LEAFSIZE-1);
sparse_mapfoldr_1(Low, High, Ix, S, E, D, F, A) ->
    LDiv = Low bsr S,
    HDiv = High bsr S,
    LRem = Low band ?MASK(S),
    HRem = High band ?MASK(S),
    if LDiv =:= HDiv ->
            {E1, A1} = sparse_mapfoldr_1(LRem, HRem, Ix, ?reduce(S), element(HDiv+1, E), D, F, A),
            {setelement(HDiv+1, E, E1), A1};
       true ->
            Es = lists:reverse(tuple_to_list(E)),
            {Es1, A1} = sparse_mapfoldr_2(LDiv, HDiv, Ix, S, Es, D, F, A, LRem, [], HRem, ?NODESIZE-1),
            {list_to_tuple(Es1), A1}
    end.

sparse_mapfoldr_2(Low, High, Ix, S, [E|Es], D, F, A, LRem, Es1, HRem, I) when High < I ->
    sparse_mapfoldr_2(Low, High, Ix, S, Es, D, F, A, LRem, [E|Es1], HRem, I - 1);
sparse_mapfoldr_2(Low, High, Ix, S, [E|Es], D, F, A, LRem, Es1, HRem, _I) ->
    {E1, A1} = sparse_mapfoldr_1(0, HRem, Ix, ?reduce(S), E, D, F, A),
    sparse_mapfoldr_2_1(Low, High-1, Ix - HRem - 1, S, Es, D, F, A1, LRem, [E1|Es1]).

sparse_mapfoldr_2_1(Low, High, Ix, S, [E|Es], D, F, A, LRem, Es1) when Low < High ->
    {E1, A1} = sparse_mapfoldr_1(0, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    sparse_mapfoldr_2_1(Low, High-1, Ix - ?SIZE(S), S, Es, D, F, A1, LRem, [E1|Es1]);
sparse_mapfoldr_2_1(_Low, _High, Ix, S, [E|Es], D, F, A, LRem, Es1) ->
    {E1, A1} = sparse_mapfoldr_1(LRem, ?SIZE(S)-1, Ix, ?reduce(S), E, D, F, A),
    {lists:reverse(Es, [E1|Es1]), A1}.

sparse_mapfoldr_3(Low, High, Ix, [E|Es], D, F, A, Es1, I) when High < I ->
    sparse_mapfoldr_3(Low, High, Ix, Es, D, F, A, [E|Es1], I - 1);
sparse_mapfoldr_3(Low, High, Ix, Es, D, F, A, Es1, _I) ->
    sparse_mapfoldr_3_1(Low, High, Ix, Es, D, F, A, Es1).

sparse_mapfoldr_3_1(Low, High, Ix, [E|Es], D, F, A, Es1) when Low =< High ->
    if E =:= D ->
            sparse_mapfoldr_3_1(Low, High-1, Ix-1, Es, D, F, A, [E|Es1]);
       true ->
            {E1, A1} = F(Ix, E, A),
            sparse_mapfoldr_3_1(Low, High-1, Ix-1, Es, D, F, A1, [E1|Es1])
    end;
sparse_mapfoldr_3_1(_Low, _High, _Ix, Es, _D, _F, A, Es1) ->
    {list_to_tuple(lists:reverse(Es, Es1)), A}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Be nice if someone did not read the docs and stored old format to disk.
%%
-doc false.
-spec upgrade(OldArray::tuple() | array()) -> array().
upgrade(A = #array{}) ->
    A;
upgrade({array, Size, Max, Def, Es}) ->
    A = old_sparse_foldl_1(Size-1, Es, new({default, Def}), 0, fun set/3, Def),
    case Max == 0 of
        true -> fix(A);
        false -> A
    end;
upgrade(A) ->
    error({badarg, A}).

old_sparse_foldl_1(N, E={_,_,_,_,_, _,_,_,_,_,S}, A, Ix, F, D) ->
    old_sparse_foldl_2(1, E, A, Ix, F, D, N div S + 1, N rem S, S);
old_sparse_foldl_1(_N, E, A, _Ix, _F, _D) when is_integer(E) ->
    A;
old_sparse_foldl_1(N, E, A, Ix, F, D) ->
    old_sparse_foldl_3(1, E, A, Ix, F, D, N+1).

old_sparse_foldl_2(I, E, A, Ix, F, D, I, R, _S) ->
    old_sparse_foldl_1(R, element(I, E), A, Ix, F, D);
old_sparse_foldl_2(I, E, A, Ix, F, D, N, R, S) ->
    old_sparse_foldl_2(I+1, E, old_sparse_foldl_1(S-1, element(I, E), A, Ix, F, D),
                       Ix + S, F, D, N, R, S).

old_sparse_foldl_3(I, T, A, Ix, F, D, N) when I =< N ->
    case element(I, T) of
        D -> old_sparse_foldl_3(I+1, T, A, Ix+1, F, D, N);
        E -> old_sparse_foldl_3(I+1, T, F(Ix, E, A), Ix+1, F, D, N)
    end;
old_sparse_foldl_3(_I, _T, A, _Ix, _F, _D, _N) ->
    A.
