<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Maps

This guide to using maps efficiently starts with a brief section on the choice
between records or maps, followed by three sections giving concrete (but brief)
advice on using maps as an alternative to records, as dictionaries, and as sets.
The remaining sections dig deeper, looking at how maps are implemented, the map
syntax, and finally the functions in the `m:maps` module.

[](){: #terminology }

Terminology used in this chapter:

- A map with at most 32 elements will informally be called a _small map_.
- A map with more than 32 elements will informally be called a _large map_.

## Maps or Records?

If the advice in this chapter is followed, the performance of records compared
to using small maps instead of records is expected to be similar. Therefore, the
choice between records and maps should be based on the desired properties of the
data structure and not performance.

The advantages of records compared to maps are:

- If the name of a record field is misspelled, there will be a compilation
  error. If a map key is misspelled, the compiler will give no warning and
  program will fail in some way when it is run.
- Records will use slightly less memory than maps, and performance is expected
  to be _slightly_ better than maps in most circumstances.

The disadvantage of records compared to maps is that if a new field is added to
a record, all code that uses that record must be recompiled. Because of that, it
is recommended to only use records within a unit of code that can easily be
recompiled all at once, for example within a single application or single
module.

## Using Maps as an Alternative to Records

- Use the map syntax instead of the functions in the `m:maps` module.
- Avoid having more than 32 elements in the map. As soon as there are more than
  32 elements in the map, it will require more memory and keys can no longer be
  shared with other instances of the map.
- When creating a new map, always create it with all keys that will ever be
  used. To maximize sharing of keys (thus minimizing memory use), create a
  single function that constructs the map using the map syntax and always use
  it.
- Always update the map using the `:=` operator (that is, requiring that an
  element with that key already exists). The `:=` operator is slightly more
  efficient, and it helps catching mispellings of keys.
- Whenever possible, match multiple map elements at once.
- Whenever possible, update multiple map elements at once.
- Avoid default values and the `maps:get/3` function. If there are default
  values, sharing of keys between different instances of the map will be less
  effective, and it is not possible to match multiple elements having default
  values in one go.
- To avoid having to deal with a map that may lack some keys, `maps:merge/2` can
  efficiently add multiple default values. For example:

  ```erlang
        DefaultMap = #{shoe_size => 42, editor => emacs},
        MapWithDefaultsApplied = maps:merge(DefaultMap, OtherMap)
  ```

## Using Maps as Dictionaries

Using a map as a dictionary implies the following usage pattern:

- Keys are usually variables not known at compile-time.
- There can be any number of elements in the map.
- Usually, no more than one element is looked up or updated at once.

Given that usage pattern, the difference in performance between using the map
syntax and the maps module is usually small. Therefore, which one to use is
mostly a matter of taste.

Maps are usually the most efficient dictionary data structure, with a few
exceptions:

- If it is necessary to frequently convert a dictionary to a sorted list, or
  from a sorted list to a dictionary, using `m:gb_trees` can be a better choice.
- If all keys are non-negative integers, the `m:array` module can be a better
  choice.

## Using Maps as Sets

Starting in OTP 24, the `m:sets` module has an option to represent sets as maps.
Examples:

```erlang
1> sets:new([{version,2}]).
#{}
2> sets:from_list([x,y,z], [{version,2}]).
#{x => [],y => [],z => []}
```

`sets` backed by maps is generally the most efficient set representation, with a
few possible exceptions:

- `ordsets:intersection/2` can be more efficient than `sets:intersection/2`. If
  the intersection operation is frequently used and operations that operate on a
  single element in a set (such as `is_element/2`) are avoided, `m:ordsets` can
  be a better choice than `m:sets`.
- If the intersection operation is frequently used and operations that operate
  on a single element in a set (such as `is_element/2`) must also be efficient,
  `m:gb_sets` can potentially be a better choice than `m:sets`.
- If the elements of the set are integers in a fairly compact range, the set can
  be represented as an integer where each bit represents an element in the set.
  The union operation is performed by `bor` and the intersection operation by
  `band`.

## How Maps are Implemented

Internally, maps have two distinct representations depending on the number of
elements in the map. The representation changes when a map grows beyond 32
elements, or when it shrinks to 32 elements or less.

- A map with at most 32 elements has a compact representation, making it
  suitable as an alternative to records.
- A map with more than 32 elements is represented as a tree that can be
  efficiently searched and updated regardless of how many elements there are.

### How Small Maps are Implemented

A small map looks like this inside the runtime system:

| `FLATMAP` | _N_ | _Keys_ | _Value1_ | _. . ._ | _ValueN_ |
| --------- | --- | ------ | -------- | ------- | -------- |

_Table: The representation of a small map_

- **`FLATMAP`** - The tag for a small map (called _flat map_ in the source code
  for the runtime system).

- **N** - The number of elements in the map.

- **Keys** - A tuple with keys of the map: `{Key1,...,KeyN}`. The keys are
  sorted.

- **Value1** - The value corresponding to the first key in the key tuple.

- **ValueN** - The value corresponding to the last key in the key tuple.

As an example, let us look at how the map `#{a => foo, z => bar}` is
represented:

| `FLATMAP` | _2_ | _\{a,z\}_ | _foo_ | _bar_ |
| --------- | --- | --------- | ----- | ----- |

_Table: \#\{a => foo, z => bar\}_

Let us update the map: `M#{q => baz}`. The map now looks like this:

| `FLATMAP` | _3_ | _\{a,q,z\}_ | _foo_ | _baz_ | _bar_ |
| --------- | --- | ----------- | ----- | ----- | ----- |

_Table: \#\{a => foo, q => baz, z => bar\}_

Finally, change the value of one element: `M#{z := bird}`. The map now looks
like this:

| `FLATMAP` | _3_ | _\{a,q,z\}_ | _foo_ | _baz_ | _bird_ |
| --------- | --- | ----------- | ----- | ----- | ------ |

_Table: \#\{a => foo, q => baz, z => bird\}_

When the value for an existing key is updated, the key tuple is not updated,
allowing the key tuple to be shared with other instances of the map that have
the same keys. In fact, the key tuple can be shared between all maps with the
same keys with some care. To arrange that, define a function that returns a map.
For example:

```erlang
new() ->
    #{a => default, b => default, c => default}.
```

Defined like this, the key tuple `{a,b,c}` will be a global literal. To ensure
that the key tuple is shared when creating an instance of the map, always call
`new()` and modify the returned map:

```erlang
    (SOME_MODULE:new())#{a := 42}.
```

Using the map syntax with small maps is particularly efficient. As long as the
keys are known at compile-time, the map is updated in one go, making the time to
update a map essentially constant regardless of the number of keys updated. The
same goes for matching. (When the keys are variables, one or more of the keys
could be identical, so the operations need to be performed sequentially from
left to right.)

The memory size for a small map is the size of all keys and values plus 5 words.
See [Advanced](advanced.md#memory) for more information about memory sizes.

### How Large Maps are Implemented

A map with more than 32 elements is implemented as a
[Hash array mapped trie (HAMT)](https://en.wikipedia.org/wiki/Hash_array_mapped_trie).
A large map can be efficiently searched and updated regardless of the number of
elements in the map.

There is less performance to be gained by matching or updating multiple elements
using the map syntax on a large map compared to a small map. The execution time
is roughly proportional to the number of elements matched or updated.

The storage overhead for a large map is higher than for a small map. For a large
map, the extra number of words besides the keys and values is roughly
proportional to the number of elements. For a map with 33 elements the overhead
is at least 53 heap words according to the formula in
[Advanced](advanced.md#memory) (compared to 5 extra words for a small map
regardless of the number of elements).

When a large map is updated, the updated map and the original map will share
common parts of the HAMT, but sharing will never be as effective as the best
possible sharing of the key tuple for small maps.

Therefore, if maps are used instead of records and it is expected that many
instances of the map will be created, it is more efficient from a memory
standpoint to avoid using large maps (for example, by grouping related map
elements into sub maps to reduce the number of elements).

## Using the Map Syntax

Using the map syntax is usually slightly more efficient than using the
corresponding function in the `m:maps` module.

The gain in efficiency for the map syntax is more noticeable for the following
operations that can only be achieved using the map syntax:

- Matching multiple literal keys
- Updating multiple literal keys
- Adding multiple literal keys to a map

For example:

_DO_

```erlang
    Map = Map1#{x := X, y := Y, z := Z}
```

_DO NOT_

```erlang
    Map2 = maps:update(x, X, Map1),
    Map3 = maps:update(y, Y, Map2),
    Map = maps:update(z, Z, Map3)
```

If the map is a small map, the first example runs roughly three times as fast.

Note that for variable keys, the elements are updated sequentially from left to
right. For example, given the following update with variable keys:

```erlang
    Map = Map1#{Key1 := X, Key2 := Y, Key3 := Z}
```

the compiler rewrites it like this to ensure that the updates are applied from
left to right:

```erlang
    Map2 = Map1#{Key1 := X},
    Map3 = Map2#{Key2 := Y},
    Map = Map3#{Key3 := Z}
```

If a key is known to exist in a map, using the `:=` operator is slightly more
efficient than using the `=>` operator for a small map.

## Using the Functions in the maps Module

Here follows some notes about most of the functions in the `maps` module. For
each function, the implementation language (C or Erlang) is stated. The reason
we mention the language is that it gives an hint about how efficient the
function is:

- If a function is implemented in C, it is pretty much impossible to implement
  the same functionality more efficiently in Erlang.
- However, it might be possible to beat the `maps` modules functions implemented
  in Erlang, because they are generally implemented in a way that attempts to
  make the performance reasonable for all possible inputs.

  For example, `maps:map/2` iterates over all elements of the map, calling the
  mapping fun, collects the updated map elements in a list, and finally converts
  the list back to a map using `maps:from_list/1`. If it is known that at most
  one percent of the values in the map will change, it can be more efficient to
  update only the changed values.

> #### Note {: .info }
>
> The implementation details given in this section can change in the future.

### maps:filter/2

`maps:filter/2` is implemented in Erlang. It creates a new map using
`maps:from_list/1`. If it is known that only a minority of the values will be
removed, it can be more efficient to avoid `maps:filter/2` and write a function
that will use [maps:remove/3](`maps:remove/2`) to remove the unwanted values.

### maps:filtermap/2

`maps:filtermap/2` is implemented in Erlang. It creates a new map using
`maps:from_list/1`. See the notes for `maps:map/2` and `maps:filter/2` for hints
on how to implement a more efficient version.

### maps:find/2

`maps:find/2` is implemented in C.

Using the map matching syntax instead of `maps:find/2` will be slightly more
efficient since building an `{ok,Value}` tuple will be avoided.

### maps:get/2

As an optimization, the compiler will rewrite a call to `maps:get/2` to a call
to the guard BIF [map_get/2](`erlang:map_get/2`). A call to a guard BIF is more
efficient than calls to other BIFs, making the performance similar to using the
map matching syntax.

If the map is small and the keys are constants known at compile-time, using the
map matching syntax will be more efficient than multiple calls to `maps:get/2`.

[](){: #maps_get_3 }

### maps:get/3

As an optimization, the compiler will rewrite a call to `maps:get/3` to Erlang
code similar to the following:

```erlang
    Result = case Map of
                 #{Key := Value} -> Value;
                 #{} -> Default
             end
```

This is reasonably efficient, but if a small map is used as an alternative to
using a record it is often better not to rely on default values as it prevents
sharing of keys, which may in the end use more memory than what you save from
not storing default values in the map.

If default values are nevertheless required, instead of calling `maps:get/3`
multiple times, consider putting the default values in a map and merging that
map with the other map:

```erlang
    DefaultMap = #{Key1 => Value2, Key2 => Value2, ..., KeyN => ValueN},
    MapWithDefaultsApplied = maps:merge(DefaultMap, OtherMap)
```

This helps share keys between the default map and the one you applied defaults
to, as long as the default map contains _all_ the keys that will ever be used
and not just the ones with default values. Whether this is faster than calling
`maps:get/3` multiple times depends on the size of the map and the number of
default values.

> #### Change {: .info }
>
> Before OTP 26.0 `maps:get/3` was implemented by calling the function instead
> of rewriting it as an Erlang expression. It is now slightly faster but can no
> longer be traced.

### maps:intersect/2, maps:intersect_with/3

`maps:intersect/2` and `maps:intersect_with/3` are implemented in Erlang. They
both create new maps using `maps:from_list/1`.

> #### Note {: .info }
>
> A map is usually the most efficient way to implement a set, but an exception
> is the intersection operation, where `ordsets:intersection/2` used on
> `m:ordsets` can be more efficient than `maps:intersect/2` on sets implemented
> as maps.

### maps:from_list/1

`maps:from_list/1` is implemented in C.

### maps:from_keys/2

`maps:from_keys/2` is implemented in C.

### maps:is_key/2

As an optimization, the compiler rewrites calls to `maps:is_key/2` to calls to
the guard BIF [is_map_key/2](`erlang:is_map_key/2`). A call to a guard BIF is
more efficient than calls to other BIFs, making the performance similar to using
the map matching syntax.

### maps:iterator/1

`maps:iterator/1` is efficiently implemented in C and Erlang.

### maps:keys/1

`maps:keys/1` is implemented in C. If the resulting list needs to be ordered,
use `lists:sort/1` to sort the result.

### maps:map/2

`maps:map/2` is implemented in Erlang. It creates a new map using
`maps:from_list/1`. If it is known that only a minority of the values will be
updated, it can be more efficient to avoid `maps:map/2` and write a function
that will call `maps:update/3` to update only the values that have changed.

### maps:merge/2

`maps:merge/2` is implemented in C. For [small maps](maps.md#terminology), the
key tuple may be shared with any of the argument maps if that argument map
contains all the keys. Literal key tuples are prefered if possible.

> #### Change {: .info }
>
> The sharing of key tuples by `maps:merge/2` was introduced in OTP 26.0. Older
> versions always contructed a new key tuple on the callers heap.

### maps:merge_with/3

`maps:merge_with/3` is implemented in Erlang. It updates and returns the larger
of the two maps.

### maps:new/0

The compiler rewrites a call to `maps:new/0` to using the syntax `#{}` for
constructing an empty map.

### maps:next/1

`maps:next/1` is efficiently implemented in C and Erlang.

### maps:put/3

`maps:put/3` is implemented in C.

If the key is known to already exist in the map, `maps:update/3` is slightly
more efficient than `maps:put/3`.

If the keys are constants known at compile-time, using the map update syntax
with the `=>` operator is more efficient than multiple calls to `maps:put/3`,
especially for small maps.

### maps:remove/2

`maps:remove/2` is implemented in C.

### maps:size/1

As an optimization, the compiler rewrites calls to `maps:size/1` to calls to the
guard BIF [map_size/1](`erlang:map_size/1`). Calls to guard BIFs are more
efficient than calls to other BIFs.

### maps:take/2

`maps:take/2` is implemented in C.

### maps:to_list/1

`maps:to_list/1` is efficiently implemented in C and Erlang. If the resulting
list needs to be ordered, use `lists:sort/1` to sort the result.

> #### Note {: .info }
>
> Maps are usually more performant than `m:gb_trees`, but if it is necessary to
> frequently convert to and from sorted lists, `gb_trees` can be a better
> choice.

### maps:update/3

`maps:update/3` is implemented in C.

If the keys are constants known at compile-time, using the map update syntax
with the `:=` operator is more efficient than multiple calls to `maps:update/3`,
especially for [small maps](maps.md#terminology).

### maps:values/1

`maps:values/1` is implemented in C.

### maps:with/2

`maps:with/2` is implemented in Erlang. It creates a new map using
`maps:from_list/1`.

### maps:without/2

`maps:without/2` is implemented in Erlang. It returns a modified copy of the
input map.
