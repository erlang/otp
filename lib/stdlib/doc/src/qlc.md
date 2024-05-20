This module provides a query interface to [Mnesia](`m:mnesia`), [ETS](`m:ets`),
[Dets](`m:dets`), and other data structures that provide an iterator style
traversal of objects.

## Overview

This module provides a query interface to _QLC tables_. Typical QLC tables are
Mnesia, ETS, and Dets tables. Support is also provided for user-defined tables,
see section [Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`). [](){:
#query_list_comprehension } A _query_ is expressed using _Query List
Comprehensions_ (QLCs). The answers to a query are determined by data in QLC
tables that fulfill the constraints expressed by the QLCs of the query. QLCs are
similar to ordinary list comprehensions as described in
[Erlang Reference Manual](`e:system:expressions.md#lcs`) and
[Programming Examples](`e:system:list_comprehensions.md`), except that variables
introduced in patterns cannot be used in list expressions. In the absence of
optimizations and options such as `cache` and `unique` (see section
[Common Options](`m:qlc#common_options`), every QLC free of QLC tables evaluates
to the same list of answers as the identical ordinary list comprehension.

While ordinary list comprehensions evaluate to lists, calling [`q/1,2`](`q/1`)
returns a _query handle_{: #query_handle }. To obtain all the answers to a
query, [`eval/1,2`](`eval/1`) is to be called with the query handle as first
argument. Query handles are essentially functional objects (funs) created in the
module calling `q/1,2`. As the funs refer to the module code, be careful not to
keep query handles too long if the module code is to be replaced. Code
replacement is described in section
[Compilation and Code Loading](`e:system:code_loading.md`) in the Erlang
Reference Manual. The list of answers can also be traversed in chunks by use of
a _query cursor_{: #query_cursor }. Query cursors are created by calling
[`cursor/1,2`](`cursor/1`) with a query handle as first argument. Query cursors
are essentially Erlang processes. One answer at a time is sent from the query
cursor process to the process that created the cursor.

## Syntax

Syntactically QLCs have the same parts as ordinary list comprehensions:

```text
[Expression || Qualifier1, Qualifier2, ...]
```

`Expression` (the _template_) is any Erlang expression. Qualifiers are either
_filters_ or _generators_. Filters are Erlang expressions returning
`t:boolean/0`. Generators have the form `Pattern <- ListExpression`, where
`ListExpression` is an expression evaluating to a query handle or a list. Query
handles are returned from [`append/1,2`](`append/1`),
[`keysort/2,3`](`keysort/2`), [`q/1,2`](`q/1`), [`sort/1,2`](`sort/1`),
[`string_to_handle/1,2,3`](`string_to_handle/1`), and `table/2`.

## Evaluation

A query handle is evaluated in the following order:

- Inspection of options and the collection of information about tables. As a
  result, qualifiers are modified during the optimization phase.
- All list expressions are evaluated. If a cursor has been created, evaluation
  takes place in the cursor process. For list expressions that are QLCs, the
  list expressions of the generators of the QLCs are evaluated as well. Be
  careful if list expressions have side effects, as list expressions are
  evaluated in unspecified order.
- The answers are found by evaluating the qualifiers from left to right,
  backtracking when some filter returns `false`, or collecting the template when
  all filters return `true`.

Filters that do not return `t:boolean/0` but fail are handled differently
depending on their syntax: if the filter is a guard, it returns `false`,
otherwise the query evaluation fails. This behavior makes it possible for the
`qlc` module to do some optimizations without affecting the meaning of a query.
For example, when testing some position of a table and one or more constants for
equality, only the objects with equal values are candidates for further
evaluation. The other objects are guaranteed to make the filter return `false`,
but never fail. The (small) set of candidate objects can often be found by
looking up some key values of the table or by traversing the table using a match
specification. It is necessary to place the guard filters immediately after the
table generator, otherwise the candidate objects are not restricted to a small
set. The reason is that objects that could make the query evaluation fail must
not be excluded by looking up a key or running a match specification.

## Join

The `qlc` module supports fast join of two query handles. Fast join is possible
if some position `P1` of one query handler and some position `P2` of another
query handler are tested for equality. Two fast join methods are provided:

- _Lookup join_ traverses all objects of one query handle and finds objects of
  the other handle (a QLC table) such that the values at `P1` and `P2` match or
  compare equal. The `qlc` module does not create any indexes but looks up
  values using the key position and the indexed positions of the QLC table.
- _Merge join_ sorts the objects of each query handle if necessary and filters
  out objects where the values at `P1` and `P2` do not compare equal. If many
  objects with the same value of `P2` exist, a temporary file is used for the
  equivalence classes.

The `qlc` module warns at compile time if a QLC combines query handles in such a
way that more than one join is possible. That is, no query planner is provided
that can select a good order between possible join operations. It is up to the
user to order the joins by introducing query handles.

The join is to be expressed as a guard filter. The filter must be placed
immediately after the two joined generators, possibly after guard filters that
use variables from no other generators but the two joined generators. The `qlc`
module inspects the operands of `=:=/2`, `==/2`, [`is_record/2`](`is_record/2`),
[`element/2`](`element/2`), and logical operators (`and/2`, `or/2`, `andalso/2`,
`orelse/2`, `xor/2`) when determining which joins to consider.

[](){: #common_options }

## Common Options

The following options are accepted by `cursor/2`, `eval/2`, `fold/4`, and
`info/2`:

- `{cache_all, Cache}`, where `Cache` is equal to `ets` or `list` adds a
  `{cache, Cache}` option to every list expression of the query except tables
  and lists. Defaults to `{cache_all, no}`. Option `cache_all` is equivalent to
  `{cache_all, ets}`.
- `{max_list_size, MaxListSize}`{: #max_list_size }, where `MaxListSize` is the
  size in bytes of terms on the external format. If the accumulated size of
  collected objects exceeds `MaxListSize`, the objects are written onto a
  temporary file. This option is used by option `{cache, list}` and by the merge
  join method. Defaults to 512\*1024 bytes.
- `{tmpdir_usage, TmpFileUsage}` determines the action taken when `qlc` is about
  to create temporary files on the directory set by option `tmpdir`. If the
  value is `not_allowed`, an error tuple is returned, otherwise temporary files
  are created as needed. Default is `allowed`, which means that no further
  action is taken. The values `info_msg`, `warning_msg`, and `error_msg` mean
  that the function with the corresponding name in module `m:error_logger` is
  called for printing some information (currently the stacktrace).
- `{tmpdir, TempDirectory}` sets the directory used by merge join for temporary
  files and by option `{cache, list}`. The option also overrides option `tmpdir`
  of `keysort/3` and `sort/2`. Defaults to `""`, which means that the directory
  returned by `file:get_cwd()` is used.
- `{unique_all, true}` adds a `{unique, true}` option to every list expression
  of the query. Defaults to `{unique_all, false}`. Option `unique_all` is
  equivalent to `{unique_all, true}`.

[](){: #getting_started }

## Getting Started

As mentioned earlier, queries are expressed in the list comprehension syntax as
described in section [Expressions](`e:system:expressions.md`) in Erlang
Reference Manual. In the following, some familiarity with list comprehensions is
assumed. The examples in section
[List Comprehensions](`e:system:list_comprehensions.md`) in Programming Examples
can get you started. Notice that list comprehensions do not add any
computational power to the language; anything that can be done with list
comprehensions can also be done without them. But they add syntax for expressing
simple search problems, which is compact and clear once you get used to it.

Many list comprehension expressions can be evaluated by the `qlc` module.
Exceptions are expressions, such that variables introduced in patterns (or
filters) are used in some generator later in the list comprehension. As an
example, consider an implementation of `lists:append(L)`:
`[X ||Y <- L, X <- Y]`. `Y` is introduced in the first generator and used in the
second. The ordinary list comprehension is normally to be preferred when there
is a choice as to which to use. One difference is that [`eval/1,2`](`eval/1`)
collects answers in a list that is finally reversed, while list comprehensions
collect answers on the stack that is finally unwound.

What the `qlc` module primarily adds to list comprehensions is that data can be
read from QLC tables in small chunks. A QLC table is created by calling
[`qlc:table/2`](`table/2`). Usually `qlc:table/2` is not called directly from
the query but through an interface function of some data structure. Erlang/OTP
includes a few examples of such functions:
[`mnesia:table/1,2`](`mnesia:table/1`), [`ets:table/1,2`](`ets:table/1`), and
[`dets:table/1,2`](`dets:table/1`). For a given data structure, many functions
can create QLC tables, but common for these functions is that they return a
query handle created by [`qlc:table/2`](`table/2`). Using the QLC tables
provided by Erlang/OTP is usually probably sufficient, but for the more advanced
user section [Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`)
describes the implementation of a function calling `qlc:table/2`.

Besides `qlc:table/2`, other functions return query handles. They are used more
seldom than tables, but are sometimes useful. [`qlc:append/1,2`](`append/1`)
traverses objects from many tables or lists after each other. If, for example,
you want to traverse all answers to a query `QH` and then finish off by a term
`{finished}`, you can do that by calling `qlc:append(QH, [{finished}])`.
[`append/2`](`append/2`) first returns all objects of `QH`, then `{finished}`.
If a tuple `{finished}` exists among the answers to `QH`, it is returned twice
from [`append/2`](`append/2`).

As another example, consider concatenating the answers to two queries `QH1` and
`QH2` while removing all duplicates. This is accomplished by using option
`unique`:

```erlang
qlc:q([X || X <- qlc:append(QH1, QH2)], {unique, true})
```

The cost is substantial: every returned answer is stored in an ETS table. Before
returning an answer, it is looked up in the ETS table to check if it has already
been returned. Without the `unique` option, all answers to `QH1` would be
returned followed by all answers to `QH2`. The `unique` option keeps the order
between the remaining answers.

If the order of the answers is not important, there is an alternative to the
`unique` option, namely to sort the answers uniquely:

```erlang
qlc:sort(qlc:q([X || X <- qlc:append(QH1, QH2)], {unique, true})).
```

This query also removes duplicates but the answers are sorted. If there are many
answers, temporary files are used. Notice that to get the first unique answer,
all answers must be found and sorted. Both alternatives find duplicates by
comparing answers, that is, if `A1` and `A2` are answers found in that order,
then `A2` is a removed if `A1 == A2`.

To return only a few answers, cursors can be used. The following code returns no
more than five answers using an ETS table for storing the unique answers:

```erlang
C = qlc:cursor(qlc:q([X || X <- qlc:append(QH1, QH2)],{unique,true})),
R = qlc:next_answers(C, 5),
ok = qlc:delete_cursor(C),
R.
```

QLCs are convenient for stating constraints on data from two or more tables. The
following example does a natural join on two query handles on position 2:

```erlang
qlc:q([{X1,X2,X3,Y1} ||
          {X1,X2,X3} <- QH1,
          {Y1,Y2} <- QH2,
          X2 =:= Y2])
```

The `qlc` module evaluates this differently depending on the query handles `QH1`
and `QH2`. If, for example, `X2` is matched against the key of a QLC table, the
lookup join method traverses the objects of `QH2` while looking up key values in
the table. However, if not `X2` or `Y2` is matched against the key or an indexed
position of a QLC table, the merge join method ensures that `QH1` and `QH2` are
both sorted on position 2 and next do the join by traversing the objects one by
one.

Option `join` can be used to force the `qlc` module to use a certain join
method. For the rest of this section it is assumed that the excessively slow
join method called "nested loop" has been chosen:

```erlang
qlc:q([{X1,X2,X3,Y1} ||
          {X1,X2,X3} <- QH1,
          {Y1,Y2} <- QH2,
          X2 =:= Y2],
      {join, nested_loop})
```

In this case the filter is applied to every possible pair of answers to `QH1`
and `QH2`, one at a time. If there are M answers to `QH1` and N answers to
`QH2`, the filter is run M\*N times.

If `QH2` is a call to the function for `m:gb_trees`, as defined in section
[Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`), then
[`gb_table:table/1` ](`m:qlc#gb_table`), the iterator for the gb-tree is
initiated for each answer to `QH1`. The objects of the gb-tree are then returned
one by one. This is probably the most efficient way of traversing the table in
that case, as it takes minimal computational power to get the following object.
But if `QH2` is not a table but a more complicated QLC, it can be more efficient
to use some RAM memory for collecting the answers in a cache, particularly if
there are only a few answers. It must then be assumed that evaluating `QH2` has
no side effects so that the meaning of the query does not change if `QH2` is
evaluated only once. One way of caching the answers is to evaluate `QH2` first
of all and substitute the list of answers for `QH2` in the query. Another way is
to use option `cache`. It is expressed like this:

```erlang
QH2' = qlc:q([X || X <- QH2], {cache, ets})
```

or only

```text
QH2' = qlc:q([X || X <- QH2], cache)
```

The effect of option `cache` is that when generator `QH2'` is run the first
time, every answer is stored in an ETS table. When the next answer of `QH1` is
tried, answers to `QH2'` are copied from the ETS table, which is very fast. As
for option `unique` the cost is a possibly substantial amount of RAM memory.

Option `{cache, list}` offers the possibility to store the answers in a list on
the process heap. This has the potential of being faster than ETS tables, as
there is no need to copy answers from the table. However, it can often result in
slower evaluation because of more garbage collections of the process heap and
increased RAM memory consumption because of larger heaps. Another drawback with
cache lists is that if the list size exceeds a limit, a temporary file is used.
Reading the answers from a file is much slower than copying them from an ETS
table. But if the available RAM memory is scarce, setting the
[limit](`m:qlc#max_list_size`) to some low value is an alternative.

Option `cache_all` can be set to `ets` or `list` when evaluating a query. It
adds a `cache` or `{cache, list}` option to every list expression except QLC
tables and lists on all levels of the query. This can be used for testing if
caching would improve efficiency at all. If the answer is yes, further testing
is needed to pinpoint the generators that are to be cached.

[](){: #implementing_a_qlc_table }

## Implementing a QLC Table

As an example of how to use function `table/2`, the implementation of a QLC
table for the `m:gb_trees` module is given:

[](){: #gb_table }

```erlang
-module(gb_table).

-export([table/1]).

table(T) ->
    TF = fun() -> qlc_next(gb_trees:next(gb_trees:iterator(T))) end,
    InfoFun = fun(num_of_objects) -> gb_trees:size(T);
                 (keypos) -> 1;
                 (is_sorted_key) -> true;
                 (is_unique_objects) -> true;
                 (_) -> undefined
              end,
    LookupFun =
        fun(1, Ks) ->
                lists:flatmap(fun(K) ->
                                      case gb_trees:lookup(K, T) of
                                          {value, V} -> [{K,V}];
                                          none -> []
                                      end
                              end, Ks)
        end,
    FormatFun =
        fun({all, NElements, ElementFun}) ->
                ValsS = io_lib:format("gb_trees:from_orddict(~w)",
                                      [gb_nodes(T, NElements, ElementFun)]),
                io_lib:format("gb_table:table(~s)", [ValsS]);
           ({lookup, 1, KeyValues, _NElements, ElementFun}) ->
                ValsS = io_lib:format("gb_trees:from_orddict(~w)",
                                      [gb_nodes(T, infinity, ElementFun)]),
                io_lib:format("lists:flatmap(fun(K) -> "
                              "case gb_trees:lookup(K, ~s) of "
                              "{value, V} -> [{K,V}];none -> [] end "
                              "end, ~w)",
                              [ValsS, [ElementFun(KV) || KV <- KeyValues]])
        end,
    qlc:table(TF, [{info_fun, InfoFun}, {format_fun, FormatFun},
                   {lookup_fun, LookupFun},{key_equality,'=='}]).

qlc_next({X, V, S}) ->
    [{X,V} | fun() -> qlc_next(gb_trees:next(S)) end];
qlc_next(none) ->
    [].

gb_nodes(T, infinity, ElementFun) ->
    gb_nodes(T, -1, ElementFun);
gb_nodes(T, NElements, ElementFun) ->
    gb_iter(gb_trees:iterator(T), NElements, ElementFun).

gb_iter(_I, 0, _EFun) ->
    '...';
gb_iter(I0, N, EFun) ->
    case gb_trees:next(I0) of
        {X, V, I} ->
            [EFun({X,V}) | gb_iter(I, N-1, EFun)];
        none ->
            []
    end.
```

`TF` is the traversal function. The `qlc` module requires that there is a way of
traversing all objects of the data structure. `gb_trees` has an iterator
function suitable for that purpose. Notice that for each object returned, a new
fun is created. As long as the list is not terminated by `[]`, it is assumed
that the tail of the list is a nullary function and that calling the function
returns further objects (and functions).

The lookup function is optional. It is assumed that the lookup function always
finds values much faster than it would take to traverse the table. The first
argument is the position of the key. As `qlc_next/1` returns the objects as
`{Key, Value}` pairs, the position is 1. Notice that the lookup function is to
return `{Key, Value}` pairs, as the traversal function does.

The format function is also optional. It is called by [`info/1,2`](`info/1`) to
give feedback at runtime of how the query is to be evaluated. Try to give as
good feedback as possible without showing too much details. In the example, at
most seven objects of the table are shown. The format function handles two
cases: `all` means that all objects of the table are traversed;
`{lookup, 1, KeyValues}` means that the lookup function is used for looking up
key values.

Whether the whole table is traversed or only some keys looked up depends on how
the query is expressed. If the query has the form

```text
qlc:q([T || P <- LE, F])
```

and `P` is a tuple, the `qlc` module analyzes `P` and `F` in compile time to
find positions of tuple `P` that are tested for equality to constants. If such a
position at runtime turns out to be the key position, the lookup function can be
used, otherwise all objects of the table must be traversed. The info function
`InfoFun` returns the key position. There can be indexed positions as well, also
returned by the info function. An index is an extra table that makes lookup on
some position fast. Mnesia maintains indexes upon request, and introduces so
called secondary keys. The `qlc` module prefers to look up objects using the key
before secondary keys regardless of the number of constants to look up.

## Key Equality

Erlang/OTP has two operators for testing term equality: `==/2` and `=:=/2`. The
difference is all about the integers that can be represented by floats. For
example, `2 == 2.0` evaluates to `true` while `2 =:= 2.0` evaluates to `false`.
Normally this is a minor issue, but the `qlc` module cannot ignore the
difference, which affects the user's choice of operators in QLCs.

If the `qlc` module at compile time can determine that some constant is free of
integers, it does not matter which one of `==/2` or `=:=/2` is used:

```erlang
1> E1 = ets:new(t, [set]), % uses =:=/2 for key equality
Q1 = qlc:q([K ||
{K} <- ets:table(E1),
K == 2.71 orelse K == a]),
io:format("~s~n", [qlc:info(Q1)]).
ets:match_spec_run(
       lists:flatmap(fun(V) ->
			    ets:lookup(#Ref<0.3098908599.2283929601.256025>,
				       V)
		     end,
		     [a, 2.71]),
       ets:match_spec_compile([{{'$1'}, [], ['$1']}]))
```

In the example, operator `==/2` has been handled exactly as `=:=/2` would have
been handled. However, if it cannot be determined at compile time that some
constant is free of integers, and the table uses `=:=/2` when comparing keys for
equality (see option [key_equality](`m:qlc#key_equality`)), then the `qlc`
module does not try to look up the constant. The reason is that there is in the
general case no upper limit on the number of key values that can compare equal
to such a constant; every combination of integers and floats must be looked up:

```erlang
2> E2 = ets:new(t, [set]),
true = ets:insert(E2, [{{2,2},a},{{2,2.0},b},{{2.0,2},c}]),
F2 = fun(I) ->
qlc:q([V || {K,V} <- ets:table(E2), K == I])
end,
Q2 = F2({2,2}),
io:format("~s~n", [qlc:info(Q2)]).
ets:table(#Ref<0.3098908599.2283929601.256125>,
          [{traverse,
            {select,
             [{{'$1', '$2'}, [{'==', '$1', {const, {2, 2}}}], ['$2']}]}}])
3> lists:sort(qlc:e(Q2)).
[a,b,c]
```

Looking up only `{2,2}` would not return `b` and `c`.

If the table uses `==/2` when comparing keys for equality, the `qlc` module
looks up the constant regardless of which operator is used in the QLC. However,
`==/2` is to be preferred:

```erlang
4> E3 = ets:new(t, [ordered_set]), % uses ==/2 for key equality
true = ets:insert(E3, [{{2,2.0},b}]),
F3 = fun(I) ->
qlc:q([V || {K,V} <- ets:table(E3), K == I])
end,
Q3 = F3({2,2}),
io:format("~s~n", [qlc:info(Q3)]).
ets:match_spec_run(ets:lookup(#Ref<0.3098908599.2283929601.256211>,
                              {2, 2}),
                   ets:match_spec_compile([{{'$1', '$2'}, [], ['$2']}]))
5> qlc:e(Q3).
[b]
```

Lookup join is handled analogously to lookup of constants in a table: if the
join operator is `==/2`, and the table where constants are to be looked up uses
`=:=/2` when testing keys for equality, then the `qlc` module does not consider
lookup join for that table.

## See Also

`m:dets`, `m:erl_eval`, `m:erlang`, `m:error_logger`, `m:ets`, `m:file`,
`m:file_sorter`, `m:mnesia`, `m:shell`,
[Erlang Reference Manual](`e:system:index.html`),
[Programming Examples](`e:system:index.html`)
