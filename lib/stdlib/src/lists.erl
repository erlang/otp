%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

-module(lists).
-moduledoc """
List processing functions.

This module contains functions for list processing.

Unless otherwise stated, all functions assume that position numbering starts
at 1. That is, the first element of a list is at position 1.

Two terms `T1` and `T2` compare equal if `T1 == T2` evaluates to `true`. They
match if `T1 =:= T2` evaluates to `true`.

Whenever an _ordering function_{: #ordering_function } `F` is expected as
argument, it is assumed that the following properties hold of `F` for all x, y,
and z:

- If x `F` y and y `F` x, then x = y (`F` is antisymmetric).
- If x `F` y and y `F` z, then x `F` z (`F` is transitive).
- x `F` y or y `F` x (`F` is total).

An example of a typical ordering function is less than or equal to: `=</2`.
""".

-compile({no_auto_import,[max/2]}).
-compile({no_auto_import,[min/2]}).

%% BIFs (implemented in the runtime system).
-export([keyfind/3, keymember/3, keysearch/3, member/2, reverse/2]).

%% Miscellaneous list functions that don't take funs as
%% arguments. Please keep in alphabetical order.
-export([append/1, append/2, concat/1,
         delete/2, droplast/1, duplicate/2,
         enumerate/1, enumerate/2, enumerate/3,
         flatlength/1, flatten/1, flatten/2,
         join/2, last/1, min/1, max/1,
         nth/2, nthtail/2,
         prefix/2, reverse/1, seq/2, seq/3,
         split/2, sublist/2, sublist/3,
         subtract/2, suffix/2, sum/1,
         uniq/1, unzip/1, unzip3/1,
         zip/2, zip/3, zip3/3, zip3/4]).

%% Functions taking a list of tuples and a position within the tuple.
-export([keydelete/3, keyreplace/4, keymap/3,
         keytake/3, keystore/4]).

%% Sort functions that operate on list of tuples.
-export([keymerge/3, keysort/2, ukeymerge/3, ukeysort/2]).

%% Sort and merge functions.
-export([merge/1, merge/2, merge/3, merge3/3,
         sort/1, sort/2,
         umerge/1, umerge/2, umerge/3, umerge3/3,
         usort/1, usort/2]).

%% Functions that take fun arguments (high-order functions). Please
%% keep in alphabetical order.
-export([all/2, any/2, dropwhile/2,
         filter/2, filtermap/2, flatmap/2,
         foldl/3, foldr/3, foreach/2,
         map/2, mapfoldl/3, mapfoldr/3,
         partition/2, search/2,
         splitwith/2, takewhile/2, uniq/2,
         zipwith/3, zipwith/4, zipwith3/4, zipwith3/5]).

%% Undocumented, but used within Erlang/OTP.
-export([zf/2]).

%% Undocumented and unused merge functions for lists sorted in reverse
%% order. They are exported so that the fundamental building blocks
%% for the sort functions can be tested. (Removing them would save
%% very little because they are thin wrappers calling helper functions
%% used by the documented sort functions.)
-export([rkeymerge/3, rmerge/2, rmerge/3, rmerge3/3,
         rukeymerge/3, rumerge/2, rumerge/3, rumerge3/3]).

%%
%% edoc documentation creation accelerated by ErlangDoc Builder (https://chatgpt.com/g/g-w1e8LZbgT-erlangdoc-builder)
%%

%% Shadowed by erl_bif_types: lists:keyfind/3
%% @doc
%% Searches a list of tuples for a tuple whose `N`th element matches a given `Key`.
%%
%% The `keyfind/3` function scans the `TupleList` for a tuple where the `N`th element 
%% is equal to `Key`. If such a tuple is found, it returns the tuple; otherwise, it returns `false`.
%%
%% ### Parameters:
%% - `Key`: The value to search for in the `N`th element of each tuple.
%% - `N`: The 1-based position of the element within the tuples to compare against `Key`.
%% - `TupleList`: A list of tuples to search.
%%
%% ### Examples:
%%     keyfind(42, 1, [{42, a}, {43, b}]).
%%     % Result: {42, a}
%%
%%     keyfind(a, 2, [{42, a}, {43, b}]).
%%     % Result: {42, a}
%%
%%     keyfind(c, 2, [{42, a}, {43, b}]).
%%     % Result: false
%%
%% ### Notes:
%% - The `N`th position is 1-based, so `keyfind(1, N, TupleList)` examines the first element of each tuple.
%% - If `N` is greater than the tuple size, or if `TupleList` contains elements that are not tuples, the function crashes.
%% - This function has a time complexity of `O(length(TupleList))`, as it performs a linear search.
%%
%% @complexity O(N) where N is the length of the list of tuples.
-doc """
Searches the list of tuples `TupleList` for a tuple whose `N`th element compares
equal to `Key`. Returns `Tuple` if such a tuple is found, otherwise `false`.
""".
-spec keyfind(Key, N, TupleList) -> Tuple | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keyfind(_, _, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:keymember/3
%% @doc
%% Returns `true` if a tuple in the `TupleList` has an `N`th element equal to `Key`, otherwise `false`.
%%
%% The `keymember/3` function checks whether any tuple in the list has its `N`th element compare equal 
%% to the provided `Key`. If such a tuple exists, the function returns `true`; otherwise, it returns `false`.
%%
%% ### Parameters:
%% - `Key`: The value to search for in the `N`th element of each tuple.
%% - `N`: The 1-based position of the element within the tuples to compare against `Key`.
%% - `TupleList`: A list of tuples to search.
%%
%% ### Examples:
%%     keymember(42, 1, [{42, a}, {43, b}]).
%%     % Result: true
%%
%%     keymember(a, 2, [{42, a}, {43, b}]).
%%     % Result: true
%%
%%     keymember(c, 2, [{42, a}, {43, b}]).
%%     % Result: false
%%
%% ### Notes:
%% - The `N`th position is 1-based, so `keymember(1, N, TupleList)` examines the first element of each tuple.
%% - If `N` is greater than the tuple size, or if `TupleList` contains elements that are not tuples, the function returns false.
%%
%% @complexity O(N) where N is the length of the list of tuples.
-doc """
Returns `true` if there is a tuple in `TupleList` whose `N`th element compares
equal to `Key`, otherwise `false`.
""".
-spec keymember(Key, N, TupleList) -> boolean() when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keymember(_, _, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:keysearch/3
%% @doc
%% Searches the list of tuples `TupleList` for a tuple whose `N`th element compares
%% equal to `Key`. Returns `{value, Tuple}` if such a tuple is found, otherwise `false`.
%%
%% The `keysearch/3` function performs a linear search in the `TupleList` to find a tuple
%% where the `N`th element matches the specified `Key`. If found, the function returns the
%% tuple wrapped in `{value, Tuple}`; otherwise, it returns `false`.
%%
%% > #### Note {: .info }
%% >
%% > This function is retained for backward compatibility. The `keyfind/3` function is 
%% > generally more convenient to use.
%%
%% ### Parameters:
%% - `Key`: The value to search for in the `N`th element of each tuple.
%% - `N`: The 1-based position of the element within the tuples to compare against `Key`.
%% - `TupleList`: A list of tuples to search.
%%
%% ### Examples:
%%     keysearch(42, 1, [{42, a}, {43, b}]).
%%     % Result: {value, {42, a}}
%%
%%     keysearch(a, 2, [{42, a}, {43, b}]).
%%     % Result: {value, {42, a}}
%%
%%     keysearch(c, 2, [{42, a}, {43, b}]).
%%     % Result: false
%%
%% ### Notes:
%% - The `N`th position is 1-based, so `keysearch(1, N, TupleList)` examines the first element of each tuple.
%% - If `N` is greater than the tuple size, or if `TupleList` contains elements that are not tuples, the function crashes.
%%
%% @complexity O(N) where N is the length of the list of tuples.
-doc """
Searches the list of tuples `TupleList` for a tuple whose `N`th element compares
equal to `Key`. Returns `{value, Tuple}` if such a tuple is found, otherwise
`false`.

> #### Note {: .info }
>
> This function is retained for backward compatibility. Function `keyfind/3` is
> usually more convenient.
""".
-spec keysearch(Key, N, TupleList) -> {value, Tuple} | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList :: [Tuple],
      Tuple :: tuple().

keysearch(_, _, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:member/2
-doc "Returns `true` if `Elem` matches some element of `List`, otherwise `false`.".
-spec member(Elem, List) -> boolean() when
      Elem :: T,
      List :: [T],
      T :: term().

member(_, _) ->
    erlang:nif_error(undef).

%% Shadowed by erl_bif_types: lists:reverse/2
%% @doc
%% Returns a list with the elements in `List1` in reverse order, with tail `Tail` appended.
%%
%% The `reverse/2` function reverses the elements of `List1` and then appends the elements of
%% `Tail` to the reversed list.
%%
%% ### Parameters:
%% - `List1`: The list to reverse.
%% - `Tail`: The term to append to the reversed `List1`.
%%
%% ### Examples:
%%     lists:reverse([1, 2, 3, 4], [a, b, c]).
%%     % Result: [4, 3, 2, 1, a, b, c]
%%
%%     lists:reverse([], [a, b, c]).
%%     % Result: [a, b, c]
%%
%%     lists:reverse([1, 2, 3], []).
%%     % Result: [3, 2, 1]
%%
%% ### Notes:
%% - If `List1` is empty, the function returns `Tail` as-is.
%%
%% @complexity O(N) where N is the length of the list.
-doc """
Returns a list with the elements in `List1` in reverse order, with tail `Tail`
appended.

_Example:_

```erlang
> lists:reverse([1, 2, 3, 4], [a, b, c]).
[4,3,2,1,a,b,c]
```
""".
-spec reverse(List1, Tail) -> List2 when
      List1 :: [T],
      Tail :: term(),
      List2 :: [T],
      T :: term().

reverse(_, _) ->
    erlang:nif_error(undef).

%%% End of Native BIFs

%% member(X, L) -> (true | false)
%%  test if X is a member of the list L
%%  Now a BIF!

%member(X, [X|_]) -> true;
%member(X, [_|Y]) ->
%   member(X, Y);
%member(X, []) -> false.

%% append(X, Y) appends lists X and Y

-doc """
Returns a new list `List3`, which is made from the elements of `List1` followed
by the elements of `List2`.

_Example:_

```erlang
> lists:append("abc", "def").
"abcdef"
```

`lists:append(A, B)` is equivalent to `A ++ B`.
""".
%% @doc Appends two lists to produce a new concatenated list.
%%
%% The `append/2` function takes two lists and returns a new list containing all the elements
%% of the first list (`L1`) followed by all the elements of the second list (`L2`).
%%
%% ### Parameters:
%% - `L1`: The first list to append.
%% - `L2`: The second list to append.
%%
%% ### Examples:
%%     append([1, 2, 3], [4, 5, 6]).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     append([], [1, 2, 3]).
%%     % Result: [1, 2, 3]
%%
%%     append([1, 2, 3], []).
%%     % Result: [1, 2, 3]
%%
%%     append([], []).
%%     % Result: []
%%
%% ### Result:
%% - Returns a new list containing all the elements of `L1` followed by all the elements of `L2`.
%%
%% ### Notes:
%% - The operation `++` has a time complexity proportional to the length of the first list (`O(length(L1))`).
%%   Therefore, appending lists where the first list is very large may be computationally expensive.
%%
%% @complexity O(length(List1)), where `List1` is the first list.
-spec append(List1, List2) -> List3 when
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

append(L1, L2) -> L1 ++ L2.

%% append(L) appends the list of lists L

-doc """
Returns a list in which all the sublists of `ListOfLists` have been appended.

_Example:_

```erlang
> lists:append([[1, 2, 3], [a, b], [4, 5, 6]]).
[1,2,3,a,b,4,5,6]
```
""".
%% @doc Flattens a list of lists into a single concatenated list.
%%
%% The `append/1` function takes a list of lists (`ListOfLists`) and returns a new list containing
%% all the elements from each sublist in order. If the input is an empty list, the result is also an empty list.
%%
%% ### Parameters:
%% - `ListOfLists`: A list containing sublists to concatenate.
%%
%% ### Examples:
%%     append([[1, 2], [3, 4], [5]]).
%%     % Result: [1, 2, 3, 4, 5]
%%
%%     append([[], [1, 2], [3]]).
%%     % Result: [1, 2, 3]
%%
%%     append([[]]).
%%     % Result: []
%%
%%     append([]).
%%     % Result: []
%%
%% ### Result:
%% - Returns a single flattened list containing all elements from the sublists in `ListOfLists`.
%%
%% ### Notes:
%% - The operation `++` has a time complexity proportional to the length of the first list being concatenated.
%%   Therefore, using this function with deeply nested lists or a very large input may impact performance.
%%
%% @complexity O(N), where `N` is the total number of elements in all sublists.
-spec append(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T],
      T :: term().

append([E]) -> E;
append([H|T]) -> H ++ append(T);
append([]) -> [].

%% subtract(List1, List2) subtract elements in List2 form List1.

-doc """
Returns a new list `List3` that is a copy of `List1`, subjected to the following
procedure: for each element in `List2`, its first occurrence in `List1` is
deleted.

_Example:_

```erlang
> lists:subtract("123212", "212").
"312".
```

`lists:subtract(A, B)` is equivalent to `A -- B`.
""".
%% @doc Removes elements from the first list based on the second list.
%%
%% The `subtract/2` function takes two lists (`L1` and `L2`) and returns a new list containing
%% all elements from `L1` after removing the first remaining occurrence of each element in `L2`.
%% For each element in `L2`, its first matching occurrence in `L1` is removed. If an element
%% appears multiple times in `L2`, multiple matching occurrences are removed from `L1`.
%%
%% ### Parameters:
%% - `L1`: The list from which elements will be removed.
%% - `L2`: The list containing elements whose first occurrences will be subtracted from `L1`.
%%
%% ### Examples:
%%     subtract([1, 2, 3, 4], [2, 4]).
%%     % Result: [1, 3]
%%
%%     subtract([1, 2, 2, 3], [2]).
%%     % Result: [1, 2, 3]
%%
%%     subtract([1, 2, 2, 3], [2, 2]).
%%     % Result: [1, 3]
%%
%%     subtract([1, 2, 2, 3], [3, 2]).
%%     % Result: [1, 2]
%%
%%     subtract([1, 2, 3], []).
%%     % Result: [1, 2, 3]
%%
%%     subtract([], [1, 2, 3]).
%%     % Result: []
%%
%% ### Result:
%% - Returns a new list containing elements from `L1` after removing the first remaining
%%   occurrence of each element in `L2`.
%%
%% ### Notes:
%% - The order of elements in `L1` is preserved for elements that remain after subtraction.
%% - The operation has a time complexity of O(N * M), where `N` is the length of `L1` and `M` is the length of `L2`.
%%
%% @since OTP 26.0
%% @complexity O(N * M), where `N` is the length of `L1` and `M` is the length of `L2`.
-spec subtract(List1, List2) -> List3 when
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

subtract(L1, L2) -> L1 -- L2.

%% reverse(L) reverse all elements in the list L. reverse/2 is now a BIF!

-doc "Returns a list with the elements in `List1` in reverse order.".
%% @doc Reverses the elements of a list.
%%
%% The `reverse/1` function takes a list (`List1`) and returns a new list (`List2`)
%% with all the elements of `List1` in reverse order.
%%
%% ### Parameters:
%% - `List1`: The list to reverse.
%%
%% ### Examples:
%%     reverse([]).
%%     % Result: []
%%
%%     reverse([1]).
%%     % Result: [1]
%%
%%     reverse([1, 2]).
%%     % Result: [2, 1]
%%
%%     reverse([1, 2, 3, 4]).
%%     % Result: [4, 3, 2, 1]
%%
%% ### Result:
%% - Returns a new list with the elements of `List1` in reverse order.
%%
%% ### Notes:
%% - This implementation optimizes for small lists with specific clauses for lists of size 0, 1, or 2.
%% - For larger lists, it uses `lists:reverse/2` for efficient reversal.
%%
%% @complexity O(N), where `N` is the length of the input list.
-spec reverse(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]).

%reverse([H|T], Y) ->
%    reverse(T, [H|Y]);
%reverse([], X) -> X.


%% nth(N, L) returns the N`th element of the list L
%% nthtail(N, L) returns the N`th tail of the list L

-doc """
Returns the `N`th element of `List`.

_Example:_

```erlang
> lists:nth(3, [a, b, c, d, e]).
c
```
""".
%% @doc Retrieves the Nth element from a list.
%%
%% The `nth/2` function returns the element at the `N`th position in the given list (`List`).
%% List indexing starts at 1 (1-based indexing).
%%
%% ### Parameters:
%% - `N`: A positive integer specifying the position of the element to retrieve.
%% - `List`: A non-empty list from which the element is retrieved.
%%
%% ### Examples:
%%     nth(1, [a, b, c, d]).
%%     % Result: a
%%
%%     nth(3, [1, 2, 3, 4, 5]).
%%     % Result: 3
%%
%%     nth(5, [10, 20, 30, 40, 50]).
%%     % Result: 50
%%
%% ### Result:
%% - Returns the element at the specified position (`N`) in the list (`List`).
%%
%% ### Notes:
%% - If `N` is greater than the length of the list, this function will cause a runtime error.
%% - The list must be non-empty and `N` must be a positive integer.
%%
%% @complexity O(N), where `N` is the specified position in the list.
-spec nth(N, List) -> Elem when
      N :: pos_integer(),
      List :: [T,...],
      Elem :: T,
      T :: term().

nth(1, [H|_]) -> H;
nth(N, [_|_]=L) when is_integer(N), N > 1 ->
    nth_1(N, L).

nth_1(1, [H|_]) -> H;
nth_1(N, [_|T]) ->
    nth_1(N - 1, T).

-doc """
Returns the `N`th tail of `List`, that is, the sublist of `List` starting at
`N+1` and continuing up to the end of the list.

_Example_

```erlang
> lists:nthtail(3, [a, b, c, d, e]).
[d,e]
> tl(tl(tl([a, b, c, d, e]))).
[d,e]
> lists:nthtail(0, [a, b, c, d, e]).
[a,b,c,d,e]
> lists:nthtail(5, [a, b, c, d, e]).
[]
```
""".
%% @doc Retrieves the tail of a list starting from the Nth position or returns the list itself if N is 0.
%%
%% The `nthtail/2` function skips the first `N` elements of the given list and returns the tail of the list
%% starting from the `N + 1`th element. If `N` is 0, the entire list is returned as-is.
%%
%% ### Parameters:
%% - `N`: A non-negative integer specifying the number of elements to skip.
%% - `List`: A non-empty list from which the tail is retrieved.
%%
%% ### Examples:
%%     nthtail(0, [a, b, c, d]).
%%     % Result: [a, b, c, d]
%%
%%     nthtail(2, [1, 2, 3, 4, 5]).
%%     % Result: [3, 4, 5]
%%
%%     nthtail(5, [10, 20, 30, 40, 50]).
%%     % Result: []
%%
%%     nthtail(0, []).
%%     % Result: []
%%
%% ### Result:
%% - Returns the sublist of the original list starting from the `N + 1`th element, or the entire list if `N` is 0.
%%
%% ### Notes:
%% - If `N` is greater than or equal to the length of the list, the result will be an empty list (`[]`).
%%
%% @complexity O(N), where `N` is the specified position in the list.
-spec nthtail(N, List) -> Tail when
      N :: non_neg_integer(),
      List :: [T,...],
      Tail :: [T],
      T :: term().

nthtail(0, []) -> [];
nthtail(0, [_|_]=L) -> L;
nthtail(1, [_|T]) -> T;
nthtail(N, [_|_]=L) when is_integer(N), N > 1 ->
    nthtail_1(N, L).

%% @doc Retrieves the tail of a list starting from the Nth position.
%%
%% The `nthtail_1/2` function skips the first `N - 1` elements of the given list and returns
%% the tail of the list starting from the `N`th element.
%%
%% ### Parameters:
%% - `N`: A positive integer specifying the position at which to start the tail.
%% - `List`: A non-empty list from which the tail is retrieved.
%%
%% ### Examples:
%%     nthtail_1(1, [a, b, c, d]).
%%     % Result: [b, c, d]
%%
%%     nthtail_1(3, [1, 2, 3, 4, 5]).
%%     % Result: [4, 5]
%%
%%     nthtail_1(5, [10, 20, 30, 40, 50]).
%%     % Result: []
%%
%% ### Result:
%% - Returns the sublist of the original list starting from the `N`th element.
%%
%% ### Notes:
%% - If `N` is greater than the length of the list, this function will cause a runtime error.
%% - The list must be non-empty, and `N` must be a positive integer.
%%
%% @spec nthtail_1(N, List) -> List when
%%       N :: pos_integer(),
%%       List :: [T],
%%       T :: term().
%%
%% @complexity O(N), where `N` is the specified position in the list.
nthtail_1(1, [_|T]) -> T;
nthtail_1(N, [_|T]) ->
    nthtail_1(N - 1, T).

%% prefix(Prefix, List) -> (true | false)

-doc "Returns `true` if `List1` is a prefix of `List2`, otherwise `false`.".
%% @doc Checks if one list is a prefix of another list.
%%
%% The `prefix/2` function determines whether the first list (`List1`) is a prefix of the second list (`List2`). 
%% A list is considered a prefix of another list if all elements of the first list appear in order at the beginning of the second list.
%%
%% ### Parameters:
%% - `List1`: The potential prefix list.
%% - `List2`: The list to check against.
%%
%% ### Examples:
%%     prefix([1, 2], [1, 2, 3, 4]).
%%     % Result: true
%%
%%     prefix([1, 3], [1, 2, 3, 4]).
%%     % Result: false
%%
%%     prefix([], [a, b, c]).
%%     % Result: true
%%
%%     prefix([x, y], []).
%%     % Result: false
%%
%% ### Result:
%% - Returns `true` if `List1` is a prefix of `List2`, otherwise `false`.
%%
%% ### Notes:
%% - The empty list (`[]`) is considered a prefix of any list.
%% - If `List1` is longer than `List2`, the result will be `false`.
%%
%% @complexity O(N), where `N` is the length of `List1`.
-spec prefix(List1, List2) -> boolean() when
      List1 :: [T],
      List2 :: [T],
      T :: term().

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) when is_list(List) -> true;
prefix([_|_], List) when is_list(List) -> false.

%% suffix(Suffix, List) -> (true | false)

-doc "Returns `true` if `List1` is a suffix of `List2`, otherwise `false`.".
%% @doc Checks if one list is a suffix of another list.
%%
%% The `suffix/2` function determines whether the first list (`List1`) is a suffix of the second list (`List2`). 
%% A list is considered a suffix of another list if all elements of the first list appear in order at the end of the second list.
%%
%% ### Parameters:
%% - `List1`: The potential suffix list.
%% - `List2`: The list to check against.
%%
%% ### Examples:
%%     suffix([3, 4], [1, 2, 3, 4]).
%%     % Result: true
%%
%%     suffix([2, 3], [1, 2, 3, 4]).
%%     % Result: false
%%
%%     suffix([], [a, b, c]).
%%     % Result: true
%%
%%     suffix([x, y], []).
%%     % Result: false
%%
%% ### Result:
%% - Returns `true` if `List1` is a suffix of `List2`, otherwise `false`.
%%
%% ### Notes:
%% - The empty list (`[]`) is considered a suffix of any list.
%% - If `List1` is longer than `List2`, the result will be `false`.
%%
%% @complexity O(N), where `N` is the length of `List2`.
-spec suffix(List1, List2) -> boolean() when
      List1 :: [T],
      List2 :: [T],
      T :: term().

suffix(Suffix, List) ->
    Delta = length(List) - length(Suffix),
    Delta >= 0 andalso nthtail(Delta, List) =:= Suffix.

%% droplast(List) returns the list dropping its last element

-doc """
Drops the last element of a `List`. The list is to be non-empty, otherwise the
function crashes with a `function_clause`.
""".
%% @doc Drops the last element of a non-empty list.
%%
%% The `droplast/1` function removes the last element of the given list (`List`) and returns the remaining list (`InitList`).
%% If the list is empty, the function crashes with a `function_clause` exception.
%%
%% ### Parameters:
%% - `List`: A non-empty list from which the last element is to be removed.
%%
%% ### Examples:
%%     droplast([1, 2, 3]).
%%     % Result: [1, 2]
%%
%%     droplast([a, b]).
%%     % Result: [a]
%%
%%     droplast([x]).
%%     % Result: []
%%
%% ### Notes:
%% - This function does not handle empty lists and will crash with a `function_clause` if provided an empty list.
%% - For safety, ensure that the list is non-empty before calling this function.
%%
%% @since OTP 17.0
%% @complexity O(N), where `N` is the length of the list.
-doc(#{since => <<"OTP 17.0">>}).
-spec droplast(List) -> InitList when
      List :: [T, ...],
      InitList :: [T],
      T :: term().

%% This is the simple recursive implementation
%% reverse(tl(reverse(L))) is faster on average,
%% but creates more garbage.
droplast([_T])  -> [];
droplast([H|T]) -> [H|droplast(T)].

%% last(List) returns the last element in a list.

-doc "Returns the last element in `List`.".
%% @doc Retrieves the last element of a non-empty list.
%%
%% The `last/1` function returns the last element of the given list (`List`).
%% If the list is empty, this function crashes with a `function_clause` exception.
%%
%% ### Parameters:
%% - `List`: A non-empty list from which the last element is to be retrieved.
%%
%% ### Examples:
%%     last([1, 2, 3]).
%%     % Result: 3
%%
%%     last([a, b, c]).
%%     % Result: c
%%
%%     last([x]).
%%     % Result: x
%%
%% ### Notes:
%% - This function assumes the list is non-empty. If an empty list is provided, it will crash with a `function_clause` error.
%%
%% @complexity O(N), where `N` is the length of the list.
-spec last(List) -> Last when
      List :: [T,...],
      Last :: T,
      T :: term().

last([E|Es]) -> last(E, Es).

last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.

%% seq(Min, Max) -> [Min,Min+1, ..., Max]
%% seq(Min, Max, Incr) -> [Min,Min+Incr, ..., Max]
%%  returns the sequence Min..Max
%%  Min <= Max and Min and Max must be integers

%% @doc Generates a sequence of integers from `From` to `To` (inclusive).
%%
%% The `seq/2` function creates a list of integers starting from `From` and ending at `To`.
%% The integers are arranged in ascending order if `From` <= `To`. If `From` > `To`, 
%% the function produces an no function clause matching exception.
%%
%% ### Parameters:
%% - `From`: The starting integer of the sequence.
%% - `To`: The ending integer of the sequence.
%%
%% ### Examples:
%%     seq(1, 5).
%%     % Result: [1, 2, 3, 4, 5]
%%
%%     seq(3, 3).
%%     % Result: [3]
%%
%%     seq(5, 1).
%%     % Result: []
%%
%% ### Notes:
%% - Equivalent to `seq(From, To, 1)` when the step size is 1.
%% - This function uses `seq_loop/3` for efficient generation of sequences.
%%
%% @complexity O(To - From), where `To - From` is the size of the resulting sequence.
-doc(#{equiv => seq(From, To, 1)}).
-spec seq(From, To) -> Seq when
      From :: integer(),
      To :: integer(),
      Seq :: [integer()].

seq(First, Last)
    when is_integer(First), is_integer(Last), First-1 =< Last -> 
    seq_loop(Last-First+1, Last, []).

%% @doc Generates a sequence of integers in descending order from a given starting value.
%%
%% The `seq_loop/3` function constructs a sequence of integers starting from `X` 
%% and appending elements to the list `L` in decreasing order. The value `N` 
%% determines how many integers to generate. This function uses recursion to handle 
%% blocks of 4, 2, or 1 integers at a time, optimizing performance for larger ranges.
%%
%% ### Parameters:
%% - `N`: The number of integers to generate. Must be a non-negative integer.
%% - `X`: The starting integer of the sequence.
%% - `L`: An accumulator list to which the sequence is appended.
%%
%% ### Examples:
%%     seq_loop(5, 10, []).
%%     % Result: [6, 7, 8, 9, 10]
%%
%%     seq_loop(3, 7, [0]).
%%     % Result: [5, 6, 7, 0]
%%
%%     seq_loop(0, 10, []).
%%     % Result: []
%%
%% ### Notes:
%% - The function assumes that `N` is non-negative.
%% - It optimizes by processing chunks of 4 or 2 integers at a time when possible.
%%
%% @complexity O(N), where `N` is the number of integers to generate.
seq_loop(N, X, L) when N >= 4 ->
     seq_loop(N-4, X-4, [X-3,X-2,X-1,X|L]);
seq_loop(N, X, L) when N >= 2 ->
     seq_loop(N-2, X-2, [X-1,X|L]);
seq_loop(1, X, L) ->
     [X|L];
seq_loop(0, _, L) ->
     L.

-doc """
Returns a sequence of integers that starts with `From` and contains the
successive results of adding `Incr` to the previous element, until `To` is
reached or passed (in the latter case, `To` is not an element of the sequence).
`Incr` defaults to 1.

Failures:

- If `To < From - Incr` and `Incr > 0`.
- If `To > From - Incr` and `Incr < 0`.
- If `Incr =:= 0` and `From =/= To`.

The following equalities hold for all sequences:

```erlang
length(lists:seq(From, To)) =:= To - From + 1
length(lists:seq(From, To, Incr)) =:= (To - From + Incr) div Incr
```

_Examples:_

```erlang
> lists:seq(1, 10).
[1,2,3,4,5,6,7,8,9,10]
> lists:seq(1, 20, 3).
[1,4,7,10,13,16,19]
> lists:seq(1, 0, 1).
[]
> lists:seq(10, 6, 4).
[]
> lists:seq(1, 1, 0).
[1]
```
""".
%% @doc Generates a sequence of integers from `From` to `To` (inclusive) with a specified step size.
%%
%% The `seq/3` function creates a list of integers starting from `From`, incrementing by `Incr`, and ending at `To`.
%% - If `Incr > 0`, the sequence increases until `To` is reached or exceeded.
%% - If `Incr < 0`, the sequence decreases until `To` is reached or passed.
%% - If `Incr == 0`, the function returns `[From]` if `From == To` or raises an error otherwise.
%%
%% ### Parameters:
%% - `From`: The starting integer of the sequence.
%% - `To`: The ending integer of the sequence.
%% - `Incr`: The step size for the sequence. Must be a non-zero integer.
%%
%% ### Examples:
%%     seq(1, 5, 1).
%%     % Result: [1, 2, 3, 4, 5]
%%
%%     seq(10, 1, -2).
%%     % Result: [10, 8, 6, 4, 2]
%%
%%     seq(5, 5, 0).
%%     % Result: [5]
%%
%%     seq(5, 5, 1).
%%     % Result: [5]
%%
%%     seq(1, 5, -1).
%%     % Result: []
%%
%% ### Notes:
%% - The function raises a `badarg` error if `Incr` is 0 and `From` != `To`.
%% - Efficiently generates sequences using `seq_loop/4`.
%%
%% ### Error Handling:
%% - If `Incr` is 0 and `From` != `To`, a `badarg` error is raised with detailed error information.
%%
%% @complexity O(abs((To - From) div Incr)), where `To - From` is the range covered by the sequence.
-spec seq(From, To, Incr) -> Seq when
      From :: integer(),
      To :: integer(),
      Incr :: integer(),
      Seq :: [integer()].

seq(First, Last, Inc)
    when is_integer(First), is_integer(Last), is_integer(Inc),
        (Inc > 0 andalso First - Inc =< Last) orelse
        (Inc < 0 andalso First - Inc >= Last) ->
    N = (Last - First + Inc) div Inc,
    seq_loop(N, Inc * (N - 1) + First, Inc, []);
seq(Same, Same, 0) when is_integer(Same) ->
    [Same];
seq(First, Last, Inc) ->
    erlang:error(badarg, [First, Last, Inc], [{error_info, #{module => erl_stdlib_errors}}]).

%% @private
%% @doc Helper function for generating a sequence of integers.
%%
%% The `seq_loop/4` function is used internally by `seq/3` to efficiently construct a sequence
%% of integers in descending order, appending elements to the result list.
%%
%% ### Parameters:
%% - `N`: The number of elements remaining to generate.
%% - `X`: The current integer to add to the sequence.
%% - `D`: The step size (increment or decrement) for the sequence.
%% - `L`: The accumulated list of integers (used for efficiency in tail recursion).
%%
%% ### Behavior:
%% - For `N >= 4`, processes four elements in a batch for efficiency.
%% - For `N >= 2`, processes two elements.
%% - For `N == 1`, adds the last remaining element to the result list.
%% - For `N == 0`, returns the final result list.
%%
%% ### Complexity:
%% - O(N), where `N` is the number of elements to generate.
seq_loop(N, X, D, L) when N >= 4 ->
     Y = X-D, Z = Y-D, W = Z-D,
     seq_loop(N-4, W-D, D, [W,Z,Y,X|L]);
seq_loop(N, X, D, L) when N >= 2 ->
     Y = X-D,
     seq_loop(N-2, Y-D, D, [Y,X|L]);
seq_loop(1, X, _, L) ->
     [X|L];
seq_loop(0, _, _, L) ->
     L.

%% sum(L) returns the sum of the elements in L

-doc "Returns the sum of the elements in `List`.".
%% @doc Calculates the sum of all elements in a list.
%%
%% The `sum/1` function takes a list of numbers and returns the total sum
%% of its elements. If the list is empty, the result is `0`.
%%
%% ### Parameters:
%% - `List`: A list of numbers (`number()`).
%%
%% ### Examples:
%%     sum([1, 2, 3, 4, 5]).
%%     % Result: 15
%%
%%     sum([-1, -2, 3, 4]).
%%     % Result: 4
%%
%%     sum([]).
%%     % Result: 0
%%
%% ### Result:
%% - Returns the total sum of all numbers in the list.
%%
%% ### Notes:
%% - The function uses tail recursion for efficiency.
%% - The `number()` type includes integers and floats.
%%
%% @complexity O(N), where `N` is the length of the list.
-spec sum(List) -> number() when
      List :: [number()].

sum(L)          -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

%% duplicate(N, X) -> [X,X,X,.....,X]  (N times)
%%   return N copies of X

-doc """
Returns a list containing `N` copies of term `Elem`.

_Example:_

```erlang
> lists:duplicate(5, xx).
[xx,xx,xx,xx,xx]
```
""".
%% @doc Creates a list containing the same element repeated a specified number of times.
%%
%% The `duplicate/2` function generates a list by repeating a given element `Elem` 
%% exactly `N` times.
%%
%% ### Parameters:
%% - `N`: A non-negative integer specifying the number of repetitions.
%% - `Elem`: The element to be repeated in the list.
%%
%% ### Examples:
%%     duplicate(5, a).
%%     % Result: [a, a, a, a, a]
%%
%%     duplicate(3, 42).
%%     % Result: [42, 42, 42]
%%
%%     duplicate(0, hello).
%%     % Result: []
%%
%% ### Result:
%% - Returns a list of length `N` where each element is `Elem`.
%%
%% ### Notes:
%% - If `N` is `0`, an empty list is returned.
%% - The function uses tail recursion for efficient list construction.
%%
%% @complexity O(N), where `N` is the specified number of repetitions.
-spec duplicate(N, Elem) -> List when
      N :: non_neg_integer(),
      Elem :: T,
      List :: [T],
      T :: term().

duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).

%% min(L) -> returns the minimum element of the list L

-doc """
Returns the first element of `List` that compares less than or equal to all
other elements of `List`.
""".

%% @doc Finds the smallest element in a non-empty list.
%%
%% The `min/1` function traverses a list and returns the smallest element. The list must be non-empty,
%% otherwise the function will crash with a `function_clause` error.
%%
%% ### Parameters:
%% - `List`: A non-empty list of comparable elements.
%%
%% ### Examples:
%%     min([3, 1, 4, 1, 5, 9]).
%%     % Result: 1
%%
%%     min([42]).
%%     % Result: 42
%%
%% ### Result:
%% - Returns the smallest element of the list.
%%
%% ### Notes:
%% - The function is implemented recursively, comparing each element of the list with the current minimum.
%% - The first element is used as the initial minimum during traversal.
%% - If List is empty, the function generates a no function clause matching exception.
%%
%% @complexity O(N), where `N` is the length of the list.
-spec min(List) -> Min when
      List :: [T,...],
      Min :: T,
      T :: term().

min([H|T]) -> min(T, H).

min([H|T], Min) when H < Min -> min(T, H);
min([_|T], Min)              -> min(T, Min);
min([],    Min)              -> Min. 

%% max(L) -> returns the maximum element of the list L

-doc """
Returns the first element of `List` that compares greater than or equal to all
other elements of `List`.
""".

%% @doc Finds the largest element in a non-empty list.
%%
%% The `max/1` function traverses a list and returns the largest element. The list must be non-empty,
%% otherwise the function will crash with a `function_clause` error.
%%
%% ### Parameters:
%% - `List`: A non-empty list of comparable elements.
%%
%% ### Examples:
%%     max([3, 1, 4, 1, 5, 9]).
%%     % Result: 9
%%
%%     max([42]).
%%     % Result: 42
%%
%% ### Result:
%% - Returns the largest element of the list.
%%
%% ### Notes:
%% - The function is implemented recursively, comparing each element of the list with the current maximum.
%% - The first element is used as the initial maximum during traversal.
%% - If List is empty, the function generates a no function clause matching exception.
%%
%% @complexity O(N), where `N` is the length of the list.
-spec max(List) -> Max when
      List :: [T,...],
      Max :: T,
      T :: term().

max([H|T]) -> max(T, H).

max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max)              -> max(T, Max);
max([],    Max)              -> Max.

%% sublist(List, Start, Length)
%%  Returns the sub-list starting at Start of length Length.

-doc """
Returns the sublist of `List1` starting at `Start` and with (maximum) `Len`
elements. It is not an error for `Start+Len` to exceed the length of the list.

_Examples:_

```erlang
> lists:sublist([1,2,3,4], 2, 2).
[2,3]
> lists:sublist([1,2,3,4], 2, 5).
[2,3,4]
> lists:sublist([1,2,3,4], 5, 2).
[]
```
""".
%% @doc Extracts a sublist starting from a given position in the list with a specified length.
%%
%% The `sublist/3` function extracts a sublist from `List1` starting at the 1-based index `Start` 
%% and containing at most `Len` elements. If `Start` is greater than the length of the list or `Len` is `0`, 
%% an empty list is returned.
%%
%% ### Parameters:
%% - `List1`: The input list from which the sublist will be extracted.
%% - `Start`: A positive integer indicating the 1-based starting position of the sublist.
%% - `Len`: A non-negative integer specifying the maximum number of elements in the sublist.
%%
%% ### Examples:
%%     sublist([1, 2, 3, 4, 5], 2, 3).
%%     % Result: [2, 3, 4]
%%
%%     sublist([1, 2, 3, 4, 5], 4, 10).
%%     % Result: [4, 5]
%%
%%     sublist([1, 2, 3, 4, 5], 6, 2).
%%     % Result: []
%%
%%     sublist([1, 2, 3, 4, 5], 3, 0).
%%     % Result: []
%%
%% ### Result:
%% - Returns a sublist starting at the `Start` position with at most `Len` elements.
%%
%% ### Notes:
%% - The `Start` parameter is 1-based.
%% - The function is implemented recursively.
%%
%% @complexity O(Start + Len), where `Start` determines the number of elements skipped, and `Len` determines the number of elements collected.
-spec sublist(List1, Start, Len) -> List2 when
      List1 :: [T],
      List2 :: [T],
      Start :: pos_integer(),
      Len :: non_neg_integer(),
      T :: term().

sublist(List, 1, L) when is_list(List), is_integer(L), L >= 0 ->
    sublist(List, L);
sublist([], S, _L) when is_integer(S), S >= 2 ->
    [];
sublist([_H|T], S, L) when is_integer(S), S >= 2 ->
    sublist(T, S-1, L).

%% @doc 
%% Returns the sublist of `List1` starting at position 1 and with (maximum) `Len`
%% elements. It is not an error for `Len` to exceed the length of the list; in that
%% case, the whole list is returned.
%%
%% ### Parameters:
%% - `List1`: The input list from which the sublist will be extracted.
%% - `Len`: A non-negative integer specifying the maximum number of elements in the sublist.
%%
%% ### Examples:
%%     sublist([1, 2, 3, 4, 5], 3).
%%     % Result: [1, 2, 3]
%%
%%     sublist([1, 2, 3, 4, 5], 10).
%%     % Result: [1, 2, 3, 4, 5]
%%
%%     sublist([], 5).
%%     % Result: []
%%
%%     sublist([1, 2, 3], 0).
%%     % Result: []
%%
%% ### Result:
%% - Returns a sublist containing at most `Len` elements from the beginning of `List1`.
%%
%% ### Notes:
%% - The `Len` parameter must be non-negative.
%% - If `Len` exceeds the length of `List1`, the entire list is returned.
%%
%% @complexity O(Len), where `Len` is the number of elements to extract.
-doc """
Returns the sublist of `List1` starting at position 1 and with (maximum) `Len`
elements. It is not an error for `Len` to exceed the length of the list, in that
case the whole list is returned.
""".
-spec sublist(List1, Len) -> List2 when
      List1 :: [T],
      List2 :: [T],
      Len :: non_neg_integer(),
      T :: term().

sublist(List, L) when is_integer(L), is_list(List) ->
    sublist_2(List, L).

sublist_2([H|T], L) when L > 0 ->
    [H|sublist_2(T, L-1)];
sublist_2(_, 0) ->
    [];
sublist_2(List, L) when is_list(List), L > 0 ->
    [].

%% delete(Item, List) -> List'
%%  Delete the first occurrence of Item from the list L.

-doc """
Returns a copy of `List1` where the first element matching `Elem` is deleted, if
there is such an element.
""".
%% @doc
%% Removes the first occurrence of an element from a list.
%%
%% The `delete/2` function takes an element (`Elem`) and a list (`List1`) and returns a new list (`List2`) 
%% with the first occurrence of `Elem` removed. If `Elem` is not present in `List1`, the original list is returned unchanged.
%%
%% ### Parameters:
%% - `Elem`: The element to be removed from the list.
%% - `List1`: The list from which the element will be removed.
%%
%% ### Examples:
%%     delete(3, [1, 2, 3, 4, 5]).
%%     % Result: [1, 2, 4, 5]
%%
%%     delete(6, [1, 2, 3, 4, 5]).
%%     % Result: [1, 2, 3, 4, 5]
%%
%%     delete(3, []).
%%     % Result: []
%%
%%     delete(3, [3, 3, 3]).
%%     % Result: [3, 3]
%%
%% ### Result:
%% - Returns a new list with the first occurrence of `Elem` removed.
%%
%% ### Notes:
%% - Only the first occurrence of `Elem` is removed.
%% - If `Elem` is not found, the original list is returned unchanged.
%%
%% @complexity O(n), where `n` is the length of the list.
-spec delete(Elem, List1) -> List2 when
      Elem :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) -> 
    [H|delete(Item, Rest)];
delete(_, []) -> [].

%% Return [{X0, Y0}, {X1, Y1}, ..., {Xn, Yn}] for lists [X0, X1, ...,
%% Xn] and [Y0, Y1, ..., Yn].

%% @doc
%% Zips two lists into a list of tuples, combining elements at the same positions.
%%
%% The `zip/2` function takes two lists, `List1` and `List2`, and returns a list of tuples where each tuple contains
%% the corresponding elements from `List1` and `List2`. The function behaves equivalently to calling `zip/3` with the
%% third argument set to `fail`.
%%
%% ### Parameters:
%% - `List1`: The first list to be zipped.
%% - `List2`: The second list to be zipped.
%%
%% ### Examples:
%%     zip([1, 2, 3], [a, b, c]).
%%     % Result: [{1, a}, {2, b}, {3, c}]
%%
%%     zip([1, 2], [a, b, c]).
%%     % Result: **exception error: no function clause matching**
%%
%%     zip([], [a, b, c]).
%%     % Result: **exception error: no function clause matching**
%%
%% ### Result:
%% - Returns a list of tuples where each tuple is formed by pairing elements at the same position from `List1` and `List2`.
%%
%% ### Notes:
%% - If the lists are of different lengths, the function raises an exception.
%% - For more control over how mismatched lengths are handled, use `zip/3` with a third argument specifying the behavior.
%%
%% @equiv zip(List1, List2, fail)
%% @complexity O(length(List1)), as the function pairs elements up to the length of both lists.
-doc(#{equiv => zip(List1, List2, fail)}).
-spec zip(List1, List2) -> List3 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [{A, B}],
      A :: term(),
      B :: term().

zip(Xs, Ys) -> zip(Xs, Ys, fail).

-doc """
"Zips" two lists into one list of two-tuples, where the first element of each
tuple is taken from the first list and the second element is taken from the
corresponding element in the second list.

The `How` parameter specifies the behavior if the given lists are of different
lengths.

- **`fail`** - The call will fail if the given lists are not of equal length.
  This is the default.

- **`trim`** - Surplus elements from the longer list will be ignored.

  _Examples:_

  ```erlang
  > lists:zip([a, b], [1, 2, 3], trim).
  [{a,1},{b,2}]
  > lists:zip([a, b, c], [1, 2], trim).
  [{a,1},{b,2}]
  ```

- **`{pad, Defaults}`** - The shorter list will be padded to the length of the
  longer list, using the respective elements from the given `Defaults` tuple.

  _Examples:_

  ```erlang
  > lists:zip([a, b], [1, 2, 3], {pad, {x, 0}}).
  [{a,1},{b,2},{x,3}]
  > lists:zip([a, b, c], [1, 2], {pad, {x, 0}}).
  [{a,1},{b,2},{c,0}]
  ```
""".
%% @doc
%% Zips two lists into a list of tuples, combining elements at the same positions and handling mismatched lengths.
%%
%% The `zip/3` function takes two lists, `List1` and `List2`, and combines their elements into tuples. It allows
%% customization of the behavior when the lists have different lengths by specifying a third argument, `How`:
%%
%% - `'fail'`: Raises an error if the lists have different lengths.
%% - `'trim'`: Stops zipping at the end of the shorter list.
%% - `{'pad', {DefaultA, DefaultB}}`: Pads the shorter list with `DefaultA` or `DefaultB` as necessary.
%%
%% ### Parameters:
%% - `List1`: The first list to be zipped.
%% - `List2`: The second list to be zipped.
%% - `How`: The behavior when lists have different lengths.
%%
%% ### Examples:
%%     zip([1, 2, 3], [a, b, c], fail).
%%     % Result: [{1, a}, {2, b}, {3, c}]
%%
%%     zip([1, 2], [a, b, c], trim).
%%     % Result: [{1, a}, {2, b}]
%%
%%     zip([1, 2], [a, b, c], {'pad', {0, null}}).
%%     % Result: [{1, a}, {2, b}, {0, c}]
%%
%%     zip([1, 2, 3], [a], {'pad', {0, null}}).
%%     % Result: [{1, a}, {2, null}, {3, null}]
%%
%%     zip([], [a, b], {'pad', {0, null}}).
%%     % Result: [{0, a}, {0, b}]
%%
%% ### Result:
%% - Returns a list of tuples where each tuple contains corresponding elements from `List1` and `List2`.
%%
%% ### Notes:
%% - The `'fail'` behavior is the default for `zip/2`.
%% - Padding behavior with `{'pad', {DefaultA, DefaultB}}` ensures both lists are considered fully traversed.
%%
%% @since OTP 26.0
%% @complexity O(N), where N is the length of the shorter list in when in trim mode or the length of the longer list when in pad mode.
-doc(#{since => <<"OTP 26.0">>}).
-spec zip(List1, List2, How) -> List3 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [{A | DefaultA, B | DefaultB}],
      A :: term(),
      B :: term(),
      How :: 'fail' | 'trim' | {'pad', {DefaultA, DefaultB}},
      DefaultA :: term(),
      DefaultB :: term().

zip([X | Xs], [Y | Ys], How) ->
    [{X, Y} | zip(Xs, Ys, How)];
zip([], [], fail) ->
    [];
zip([], [], trim) ->
    [];
zip([], [], {pad, {_, _}}) ->
    [];
zip([_ | _], [], trim) ->
    [];
zip([], [_ | _], trim) ->
    [];
zip([], [_ | _]=Ys, {pad, {X, _}}) ->
    [{X, Y} || Y <- Ys];
zip([_ | _]=Xs, [], {pad, {_, Y}}) ->
    [{X, Y} || X <- Xs].


%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn]}, for a list [{X0, Y0},
%% {X1, Y1}, ..., {Xn, Yn}].



%% @doc
%% Unzips a list of tuples into two separate lists.
%%
%% The `unzip/1` function takes a list of tuples and separates it into two lists:
%% one containing all the first elements of the tuples, and the other containing
%% all the second elements.
%%
%% ### Parameters:
%% - `List1`: A list of tuples, where each tuple has two elements.
%%
%% ### Examples:
%%     unzip([{1, a}, {2, b}, {3, c}]).
%%     % Result: {[1, 2, 3], [a, b, c]}
%%
%%     unzip([]).
%%     % Result: {[], []}
%%
%% ### Result:
%% - Returns a tuple `{List2, List3}`, where:
%%   - `List2` contains the first elements of the tuples in `List1`.
%%   - `List3` contains the second elements of the tuples in `List1`.
%%
%% ### Notes:
%% - The function processes the input in a single traversal, making it efficient.
%% - The input list `List1` is assumed to contain tuples; providing other data types may result in an error.
%%
%% @complexity O(length(List1)), as it traverses the list once.
-doc """
"Unzips" a list of two-tuples into two lists, where the first list contains the
first element of each tuple, and the second list contains the second element of
each tuple.
""".

-spec unzip(List1) -> {List2, List3} when
      List1 :: [{A, B}],
      List2 :: [A],
      List3 :: [B],
      A :: term(),
      B :: term().

unzip(Ts) -> unzip(Ts, [], []).

unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {reverse(Xs), reverse(Ys)}.

%% Return [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}] for lists [X0,
%% X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn].

%% @doc
%% Zips three lists into a single list of 3-tuples.
%%
%% The `zip3/3` function takes three lists and combines them element-by-element into
%% a single list of 3-tuples (`{A, B, C}`), where the first element comes from `List1`,
%% the second from `List2`, and the third from `List3`.
%%
%% ### Parameters:
%% - `List1`: The first list of elements.
%% - `List2`: The second list of elements.
%% - `List3`: The third list of elements.
%%
%% ### Examples:
%%     zip3([1, 2, 3], [a, b, c], [x, y, z]).
%%     % Result: [{1, a, x}, {2, b, y}, {3, c, z}]
%%
%%     zip3([1], [a, b], [x, y, z]).
%%     % Result: *exception error: no function clause matching*
%%
%%     zip3([], [a, b], [x, y, z]).
%%     % Result: *exception error: no function clause matching*
%%
%% ### Notes:
%% - The function assumes that the input lists are of the same length or will be processed
%%   using the `fail` mode by default. For other modes, use `zip3/4`.
%%
%% @complexity O(min(length(List1), length(List2), length(List3))), as it stops at the shortest list.
-doc(#{equiv => zip3(List1, List2, List3, fail)}).
-spec zip3(List1, List2, List3) -> List4 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [C],
      List4 :: [{A, B, C}],
      A :: term(),
      B :: term(),
      C :: term().

zip3(Xs, Ys, Zs) -> zip3(Xs, Ys, Zs, fail).

-doc """
"Zips" three lists into one list of three-tuples, where the first element of
each tuple is taken from the first list, the second element is taken from the
corresponding element in the second list, and the third element is taken from
the corresponding element in the third list.

For a description of the `How` parameter, see `zip/3`.
""".
%% @doc
%% Zips three lists into a single list of triples with customizable handling of uneven lengths.
%%
%% The `zip3/4` function takes three lists and combines them element-by-element into
%% a single list of triples (`{A, B, C}`). The `How` parameter determines the behavior
%% when the input lists have different lengths:
%% - `'fail'`: Returns an empty list if the lists are of unequal lengths.
%% - `'trim'`: Stops zipping at the shortest list.
%% - `{'pad', {DefaultA, DefaultB, DefaultC}}`: Pads shorter lists with the specified default values.
%%
%% ### Parameters:
%% - `List1`: The first list of elements.
%% - `List2`: The second list of elements.
%% - `List3`: The third list of elements.
%% - `How`: Determines how to handle unequal lengths of the input lists.
%%
%% ### Examples:
%%     zip3([1, 2], [a, b, c], [x, y, z], 'trim').
%%     % Result: [{1, a, x}, {2, b, y}]
%%
%%     zip3([1], [a, b], [x, y, z], 'fail').
%%     % Result: *exception error: no function clause matching*
%%
%%     zip3([1], [a, b], [x, y, z], {'pad', {0, nil, '?'}}).
%%     % Result: [{1,a,x}, {0,b,y}, {0,nil,z}]
%%
%% ### Notes:
%% - Padding (`{'pad', {...}}`) ensures that shorter lists are extended with default values to match the longest list.
%% - The operation stops processing as soon as the shortest list is exhausted unless padding is used.
%%
%% @since OTP 26.0
%% @complexity O(N), where N is the length of the shortest list in trim or fail mode, the length of the longest list in pad mode.
-doc(#{since => <<"OTP 26.0">>}).
-spec zip3(List1, List2, List3, How) -> List4 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [C],
      List4 :: [{A | DefaultA, B | DefaultB, C | DefaultC}],
      A :: term(),
      B :: term(),
      C :: term(),
      How :: 'fail' | 'trim' | {'pad', {DefaultA, DefaultB, DefaultC}},
      DefaultA :: term(),
      DefaultB :: term(),
      DefaultC :: term().

zip3([X | Xs], [Y | Ys], [Z | Zs], How) ->
    [{X, Y, Z} | zip3(Xs, Ys, Zs, How)];
zip3([], [], [], fail) ->
    [];
zip3([], [], [], trim) ->
    [];
zip3(Xs, Ys, Zs, trim) when is_list(Xs), is_list(Ys), is_list(Zs) ->
    [];
zip3([], [], [], {pad, {_, _, _}}) ->
    [];
zip3([], [], [_ |_]=Zs, {pad, {X, Y, _}}) ->
    [{X, Y, Z} || Z <- Zs];
zip3([], [_ | _]=Ys, [], {pad, {X, _, Z}}) ->
    [{X, Y, Z} || Y <- Ys];
zip3([_ | _]=Xs, [], [], {pad, {_, Y, Z}}) ->
    [{X, Y, Z} || X <- Xs];
zip3([], [Y | Ys], [Z | Zs], {pad, {X, _, _}} = How) ->
    [{X, Y, Z} | zip3([], Ys, Zs, How)];
zip3([X | Xs], [], [Z | Zs], {pad, {_, Y, _}} = How) ->
    [{X, Y, Z} | zip3(Xs, [], Zs, How)];
zip3([X | Xs], [Y | Ys], [], {pad, {_, _, Z}} = How) ->
    [{X, Y, Z} | zip3(Xs, Ys, [], How)].

%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn], [Z0, Z1, ..., Zn]}, for
%% a list [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}].

-doc """
"Unzips" a list of three-tuples into three lists, where the first list contains
the first element of each tuple, the second list contains the second element of
each tuple, and the third list contains the third element of each tuple.
""".
%% @doc
%% Unzips a list of 3-tuples into three separate lists.
%%
%% The `unzip3/1` function takes a list of 3-tuples (`{A, B, C}`) and separates them into
%% three lists: one containing all the first elements (`A`), another containing all the
%% second elements (`B`), and the last containing all the third elements (`C`).
%%
%% ### Parameters:
%% - `List1`: A list of 3-tuples `{A, B, C}` to be separated into three lists.
%%
%% ### Examples:
%%     unzip3([{1, a, x}, {2, b, y}, {3, c, z}]).
%%     % Result: {[1, 2, 3], [a, b, c], [x, y, z]}
%%
%%     unzip3([]).
%%     % Result: {[], [], []}
%%
%% ### Notes:
%% - The input list must contain tuples of exactly three elements, otherwise the function crashes with a `no matching function_clause error`.
%% - The output lists are in the same order as the input list.
%%
%% @complexity O(N), where `N` is the length of the input list.
-spec unzip3(List1) -> {List2, List3, List4} when
      List1 :: [{A, B, C}],
      List2 :: [A],
      List3 :: [B],
      List4 :: [C],
      A :: term(),
      B :: term(),
      C :: term().

unzip3(Ts) -> unzip3(Ts, [], [], []).

unzip3([{X, Y, Z} | Ts], Xs, Ys, Zs) ->
    unzip3(Ts, [X | Xs], [Y | Ys], [Z | Zs]);
unzip3([], Xs, Ys, Zs) ->
    {reverse(Xs), reverse(Ys), reverse(Zs)}.

%% Return [F(X0, Y0), F(X1, Y1), ..., F(Xn, Yn)] for lists [X0, X1, ...,
%% Xn] and [Y0, Y1, ..., Yn].

%% @doc
%% Applies a combining function to corresponding elements of two lists.
%%
%% The `zipwith/3` function takes a combining function (`Combine`) and two lists (`List1` and `List2`) 
%% and returns a new list where each element is the result of applying `Combine` to the corresponding 
%% elements of `List1` and `List2`.
%%
%% ### Parameters:
%% - `Combine`: A function that takes two arguments (`X` from `List1` and `Y` from `List2`) and returns a value.
%% - `List1`: The first list of elements.
%% - `List2`: The second list of elements.
%%
%% ### Examples:
%%     zipwith(fun(X, Y) -> X + Y end, [1, 2, 3], [4, 5, 6]).
%%     % Result: [5, 7, 9]
%%
%%     zipwith(fun(X, Y) -> {X, Y} end, [a, b, c], [1, 2, 3]).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}]
%%
%%     zipwith(fun(_, _) -> ok end, [], []).
%%     % Result: []
%%
%% ### Notes:
%% - If the lengths of the two input lists are different, the function behavior depends on the `zipwith/4` implementation 
%%   with the default `fail` behavior (generating an error if the lengths don't match).
%%
%% @equiv zipwith(Combine, List1, List2, fail)
%% @complexity O(min(length(List1), length(List2))), as the function stops when the shorter list ends.
-doc(#{equiv => zipwith(Combine, List1, List2, fail)}).
-spec zipwith(Combine, List1, List2) -> List3 when
      Combine :: fun((X, Y) -> T),
      List1 :: [X],
      List2 :: [Y],
      List3 :: [T],
      X :: term(),
      Y :: term(),
      T :: term().

zipwith(F, Xs, Ys) -> zipwith(F, Xs, Ys, fail).

-doc """
Combines the elements of two lists into one list. For each pair `X, Y` of list
elements from the two lists, the element in the result list is `Combine(X, Y)`.

For a description of the `How` parameter, see `zip/3`.

[`zipwith(fun(X, Y) -> {X,Y} end, List1, List2)`](`zipwith/3`) is equivalent to
[`zip(List1, List2)`](`zip/2`).

_Example:_

```erlang
> lists:zipwith(fun(X, Y) -> X+Y end, [1,2,3], [4,5,6]).
[5,7,9]
```
""".
%% @doc Combines two lists element-wise using a specified function, with configurable behavior for unequal lengths.
%%
%% This function takes a combining fun (`Combine`), two lists (`List1` and `List2`), and a mode (`How`) to 
%% produce a list (`List3`). The `Combine` fun is applied element-wise to corresponding elements from the 
%% two lists. The behavior for lists of unequal lengths is determined by `How`:
%%
%% - `fail`: Produces an empty list if the lengths of `List1` and `List2` are unequal.
%% - `trim`: Combines elements only up to the length of the shorter list.
%% - `{pad, {DefaultX, DefaultY}}`: Pads the shorter list with `DefaultX` or `DefaultY` to match the length of 
%%   the longer list.
%%
%% Example:
%%     zipwith(fun(X, Y) -> {X, Y} end, [1, 2], [a, b], trim).          % returns [{1, a}, {2, b}]
%%     zipwith(fun(X, Y) -> X + Y end, [1, 2, 3], [10, 20], trim).      % returns [11, 22]
%%     zipwith(fun(X, Y) -> {X, Y} end, [1], [a, b], {pad, {0, z}}).    % returns [{1, a}, {0, b}]
%%     zipwith(fun(X, Y) -> {X, Y} end, [1], [a], fail).                % returns [{1, a}]
%%     zipwith(fun(X, Y) -> {X, Y} end, [1, 2], [a], fail).             % returns []
%%
%%
%% @since OTP 26.0
%% @complexity O(N), where N is the length of the longer list when padding, 
%% or the shorter list when trimming.
-doc(#{since => <<"OTP 26.0">>}).
-spec zipwith(Combine, List1, List2, How) -> List3 when
      Combine :: fun((X | DefaultX, Y | DefaultY) -> T),
      List1 :: [X],
      List2 :: [Y],
      List3 :: [T],
      X :: term(),
      Y :: term(),
      How :: 'fail' | 'trim' | {'pad', {DefaultX, DefaultY}},
      DefaultX :: term(),
      DefaultY :: term(),
      T :: term().

zipwith(F, [X | Xs], [Y | Ys], How) ->
    [F(X, Y) | zipwith(F, Xs, Ys, How)];
zipwith(F, [], [], fail) when is_function(F, 2) ->
    [];
zipwith(F, [], [], trim) when is_function(F, 2) ->
    [];
zipwith(F, [], [], {pad, {_, _}}) when is_function(F, 2) ->
    [];
zipwith(F, [_ | _], [], trim) when is_function(F, 2) ->
    [];
zipwith(F, [], [_ | _], trim) when is_function(F, 2) ->
    [];
zipwith(F, [], [_ | _]=Ys, {pad, {X, _}}) ->
    [F(X, Y) || Y <- Ys];
zipwith(F, [_ | _]=Xs, [], {pad, {_, Y}}) ->
    [F(X, Y) || X <- Xs].

%% Return [F(X0, Y0, Z0), F(X1, Y1, Z1), ..., F(Xn, Yn, Zn)] for lists
%% [X0, X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn].

-doc(#{equiv => zipwith3(Combine, List1, List2, List3, fail)}).
%% @doc Combines three lists element-wise using a specified fun, with a default behavior of failing on unequal lengths.
%%
%% This function takes a combining fun (`Combine`), three lists (`List1`, `List2`, `List3`), and produces a list 
%% (`List4`) by applying `Combine` to corresponding elements of `List1`, `List2`, and `List3`. By default, the function 
%% uses the `fail` mode, which results in an empty list if the lengths of the three lists are not equal.
%%
%% This function is a shorthand for calling `zipwith3/5` with the `fail` mode.
%%
%% Example:
%%     zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, [1, 2], [a, b], [x, y]).
%%     % returns [{1, a, x}, {2, b, y}]
%%
%%     zipwith3(fun(X, Y, Z) -> X + Y + Z end, [1, 2], [10, 20], [100, 200]).
%%     % returns [111, 222]
%%
%%     zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, [1], [a, b], [x]).
%%     % returns []
%%
%%
%% @complexity O(N), where N is the length of the shortest list.
-spec zipwith3(Combine, List1, List2, List3) -> List4 when
      Combine :: fun((X, Y, Z) -> T),
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [T],
      X :: term(),
      Y :: term(),
      Z :: term(),
      T :: term().

zipwith3(F, Xs, Ys, Zs) -> zipwith3(F, Xs, Ys, Zs, fail).

-doc """
Combines the elements of three lists into one list. For each triple `X, Y, Z` of
list elements from the three lists, the element in the result list is
`Combine(X, Y, Z)`.

For a description of the `How` parameter, see `zip/3`.

[`zipwith3(fun(X, Y, Z) -> {X,Y,Z} end, List1, List2, List3)`](`zipwith3/4`) is
equivalent to [`zip3(List1, List2, List3)`](`zip3/3`).

_Examples:_

```erlang
> lists:zipwith3(fun(X, Y, Z) -> X+Y+Z end, [1,2,3], [4,5,6], [7,8,9]).
[12,15,18]
> lists:zipwith3(fun(X, Y, Z) -> [X,Y,Z] end, [a,b,c], [x,y,z], [1,2,3]).
[[a,x,1],[b,y,2],[c,z,3]]
```
""".
%% @doc Combines three lists element-wise using a specified fun, with configurable behavior for unequal lengths.
%%
%% This function takes a combining fun (`Combine`), three lists (`List1`, `List2`, `List3`), and a mode (`How`), 
%% and produces a list (`List4`) by applying `Combine` to corresponding elements from the three lists. The behavior for 
%% handling unequal lengths is determined by the `How` parameter:
%%
%% - `fail`: Produces an empty list if the lengths of the three lists are not equal.
%% - `trim`: Combines elements only up to the length of the shortest list.
%% - `{pad, {DefaultX, DefaultY, DefaultZ}}`: Pads shorter lists with default values to match the length of the longest list.
%%
%% Example:
%%     zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, [1, 2], [a, b], [x, y], trim).
%%     % returns [{1, a, x}, {2, b, y}]
%%
%%     zipwith3(fun(X, Y, Z) -> X + Y + Z end, [1, 2, 3], [10, 20], [100], {pad, {0, 0, 0}}).
%%     % returns [111, 122, 103]
%%
%%     zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, [1, 2], [a], [x, y], fail).
%%     % returns []
%%
%%     zipwith3(fun(X, Y, Z) -> {X, Y, Z} end, [], [], [], trim).
%%     % returns []
%%
%%
%% @since OTP 26.0
%% @complexity O(N), where N is the length of the longest list when padding, 
%% or the shortest list when trimming.
-doc(#{since => <<"OTP 26.0">>}).
-spec zipwith3(Combine, List1, List2, List3, How) -> List4 when
      Combine :: fun((X | DefaultX, Y | DefaultY, Z | DefaultZ) -> T),
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [T],
      X :: term(),
      Y :: term(),
      Z :: term(),
      How :: 'fail' | 'trim' | {'pad', {DefaultX, DefaultY, DefaultZ}},
      DefaultX :: term(),
      DefaultY :: term(),
      DefaultZ :: term(),
      T :: term().

zipwith3(F, [X | Xs], [Y | Ys], [Z | Zs], How) ->
    [F(X, Y, Z) | zipwith3(F, Xs, Ys, Zs, How)];
zipwith3(F, [], [], [], fail) when is_function(F, 3) ->
    [];
zipwith3(F, [], [], [], trim) when is_function(F, 3) ->
    [];
zipwith3(F, Xs, Ys, Zs, trim) when is_function(F, 3), is_list(Xs), is_list(Ys), is_list(Zs) ->
    [];
zipwith3(F, [], [], [], {pad, {_, _, _}}) when is_function(F, 3) ->
    [];
zipwith3(F, [], [], [_ | _]=Zs, {pad, {X, Y, _}}) ->
    [F(X, Y, Z) || Z <- Zs];
zipwith3(F, [], [_ | _]=Ys, [], {pad, {X, _, Z}}) ->
    [F(X, Y, Z) || Y <- Ys];
zipwith3(F, [_ | _]=Xs, [], [], {pad, {_, Y, Z}}) ->
    [F(X, Y, Z) || X <- Xs];
zipwith3(F, [], [Y | Ys], [Z | Zs], {pad, {X, _, _}} = How) ->
    [F(X, Y, Z) | zipwith3(F, [], Ys, Zs, How)];
zipwith3(F, [X | Xs], [], [Z | Zs], {pad, {_, Y, _}} = How) ->
    [F(X, Y, Z) | zipwith3(F, Xs, [], Zs, How)];
zipwith3(F, [X | Xs], [Y | Ys], [], {pad, {_, _, Z}} = How) ->
    [F(X, Y, Z) | zipwith3(F, Xs, Ys, [], How)].

%% sort(List) -> L
%%  sorts the list L

-doc "Returns a list containing the sorted elements of `List1`.".
%% @doc Sorts a list of terms in ascending order.
%%
%% This function takes a list (`List1`) and returns a new list (`List2`) with the elements 
%% sorted in ascending order based on Erlang's standard term ordering. It handles various 
%% cases, including duplicates and small lists, with optimized logic for performance.
%%
%% Example:
%%     sort([3, 1, 4, 1, 5]). % returns [1, 1, 3, 4, 5]
%%     sort(["z", "a", "m"]). % returns ["a", "m", "z"]
%%     sort([]).              % returns []
%%
%%
%% @complexity O(N log N) on average, where N is the length of the list. 
%%             Optimized for certain cases like already sorted or partially sorted lists.
-spec sort(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

sort([X, Y | L] = L0) when X =< Y ->
    case L of
    [] -> 
        L0;
    [Z] when Y =< Z ->
        L0;
    [Z] when X =< Z ->
        [X, Z, Y];
    [Z] ->
        [Z, X, Y];
    _ when X == Y ->
        sort_1(Y, L, [X]);
    _ ->
        split_1(X, Y, L, [], [])
    end;
sort([X, Y | L]) ->
    case L of
    [] ->
        [Y, X];
    [Z] when X =< Z ->
        [Y, X | L];
    [Z] when Y =< Z ->
        [Y, Z, X];
    [Z] ->
        [Z, Y, X];
    _ ->
        split_2(X, Y, L, [], [])
    end;
sort([_] = L) ->
    L;
sort([] = L) ->
    L.

sort_1(X, [Y | L], R) when X == Y ->
    sort_1(Y, L, [X | R]);
sort_1(X, [Y | L], R) when X < Y ->
    split_1(X, Y, L, R, []);
sort_1(X, [Y | L], R) ->
    split_2(X, Y, L, R, []);
sort_1(X, [], R) ->
    lists:reverse(R, [X]).

%% merge(List) -> L
%%  merges a list of sorted lists

-doc """
Returns the sorted list formed by merging all the sublists of `ListOfLists`. All
sublists must be sorted before evaluating this function.

When two elements compare equal, the element from the sublist with the lowest
position in `ListOfLists` is picked before the other element.
""".
-spec merge(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T],
      T :: term().

merge(L) ->
    mergel(L, []).

%% merge3(X, Y, Z) -> L
%%  merges three sorted lists X, Y and Z

-doc """
Returns the sorted list formed by merging `List1`, `List2`, and `List3`. All of
`List1`, `List2`, and `List3` must be sorted before evaluating this function.

When two elements compare equal, the element from `List1`, if there is such an
element, is picked before the other element, otherwise the element from `List2`
is picked before the element from `List3`.
""".
-spec merge3(List1, List2, List3) -> List4 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [(X | Y | Z)],
      X :: term(),
      Y :: term(),
      Z :: term().

merge3([_|_]=L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(merge3_1(L1, [], H2, T2, H3, T3), []);
merge3([_|_]=L1, [_|_]=L2, []) ->
    merge(L1, L2);
merge3([_|_]=L1, [], [_|_]=L3) ->
    merge(L1, L3);
merge3([_|_]=L1, [], []) ->
    L1;
merge3([], [_|_]=L2, [_|_]=L3) ->
    merge(L2, L3);
merge3([], [_|_]=L2, []) ->
    L2;
merge3([], [], [_|_]=L3) ->
    L3;
merge3([], [], []) ->
    [].

%% rmerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z

-doc false.
-spec rmerge3([X], [Y], [Z]) -> [(X | Y | Z)].

rmerge3([_|_]=L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(rmerge3_1(L1, [], H2, T2, H3, T3), []);
rmerge3([_|_]=L1, [_|_]=L2, []) ->
    rmerge(L1, L2);
rmerge3([_|_]=L1, [], [_|_]=L3) ->
    rmerge(L1, L3);
rmerge3([_|_]=L1, [], []) ->
    L1;
rmerge3([], [_|_]=L2, [_|_]=L3) ->
    rmerge(L2, L3);
rmerge3([], [_|_]=L2, []) ->
    L2;
rmerge3([], [], [_|_]=L3) ->
    L3;
rmerge3([], [], []) ->
    [].

%% merge(X, Y) -> L
%%  merges two sorted lists X and Y

-doc """
Returns the sorted list formed by merging `List1` and `List2`. Both `List1` and
`List2` must be sorted before evaluating this function.

When two elements compare equal, the element from `List1` is picked before the
element from `List2`.
""".
-spec merge(List1, List2) -> List3 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [(X | Y)],
      X :: term(),
      Y :: term().

merge([_|_]=T1, [H2 | T2]) ->
    lists:reverse(merge2_1(T1, H2, T2, []), []);
merge([_|_]=L1, []) ->
    L1;
merge([], [_|_]=L2) ->
    L2;
merge([], []) ->
    [].

%% rmerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y

%% reverse(rmerge(reverse(A),reverse(B))) is equal to merge(I,A,B).

-doc false.
-spec rmerge([X], [Y]) -> [(X | Y)].

rmerge([_|_]=T1, [H2 | T2]) ->
    lists:reverse(rmerge2_1(T1, H2, T2, []), []);
rmerge([_|_]=L1, []) ->
    L1;
rmerge([], [_|_]=L2) ->
    L2;
rmerge([], []) ->
    [].

%% concat(L) concatenate the list representation of the elements
%%  in L - the elements in L can be atoms, numbers of strings.
%%  Returns a list of characters.

-doc """
Concatenates the text representation of the elements of `Things`. The elements
of `Things` can be atoms, integers, floats, or strings.

_Example:_

```erlang
> lists:concat([doc, '/', file, '.', 3]).
"doc/file.3"
```
""".
-spec concat(Things) -> string() when
      Things :: [Thing],
      Thing :: atom() | integer() | float() | string().

concat(List) ->
    flatmap(fun thing_to_list/1, List).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.   %Assumed to be a string

%% flatten(List)
%% flatten(List, Tail)
%%  Flatten a list, adding optional tail.

-doc "Returns a flattened version of `DeepList`.".
-spec flatten(DeepList) -> List when
      DeepList :: [term() | DeepList],
      List :: [term()].

flatten(List) when is_list(List) ->
    do_flatten(List, []).

-doc "Returns a flattened version of `DeepList` with tail `Tail` appended.".
-spec flatten(DeepList, Tail) -> List when
      DeepList :: [term() | DeepList],
      Tail :: [term()],
      List :: [term()].

flatten(List, Tail) when is_list(List), is_list(Tail) ->
    do_flatten(List, Tail).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.

%% flatlength(List)
%%  Calculate the length of a list of lists.

-doc "Equivalent to [`length(flatten(DeepList))`](`length/1`), but more efficient.".
-spec flatlength(DeepList) -> non_neg_integer() when
      DeepList :: [term() | DeepList].

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T], L) when is_list(H) ->
    flatlength(H, flatlength(T, L));
flatlength([_|T], L) ->
    flatlength(T, L + 1);
flatlength([], L) -> L.

%% keymember(Key, Index, [Tuple]) Now a BIF!
%% keyfind(Key, Index, [Tuple]) A BIF!
%% keysearch(Key, Index, [Tuple]) Now a BIF!
%% keydelete(Key, Index, [Tuple])
%% keyreplace(Key, Index, [Tuple], NewTuple)
%% keytake(Key, Index, [Tuple])
%% keystore(Key, Index, [Tuple], NewTuple)
%% keysort(Index, [Tuple])
%% keymerge(Index, [Tuple], [Tuple])
%% ukeysort(Index, [Tuple])
%% ukeymerge(Index, [Tuple], [Tuple])
%% keymap(Function, Index, [Tuple])
%% keymap(Function, ExtraArgs, Index, [Tuple])

%keymember(K,N,L) when is_integer(N), N > 0 ->
%    keymember3(K,N,L).

%keymember3(Key, N, [T|Ts]) when element(N, T) == Key -> true;
%keymember3(Key, N, [T|Ts]) ->
%    keymember3(Key, N, Ts);
%keymember3(Key, N, []) -> false.

%keysearch(K, N, L) when is_integer(N), N > 0 ->
%    keysearch3(K, N, L).

%keysearch3(Key, N, [H|T]) when element(N, H) == Key ->
%    {value, H};
%keysearch3(Key, N, [H|T]) ->
%    keysearch3(Key, N, T);
%keysearch3(Key, N, []) -> false.

-doc """
Returns a copy of `TupleList1` where the first occurrence of a tuple whose `N`th
element compares equal to `Key` is deleted, if there is such a tuple.
""".
-spec keydelete(Key, N, TupleList1) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

keydelete(K, N, L) when is_integer(N), N > 0 ->
    keydelete3(K, N, L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key -> T;
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].

-doc """
Returns a copy of `TupleList1` where the first occurrence of a `T` tuple whose
`N`th element compares equal to `Key` is replaced with `NewTuple`, if there is
such a tuple `T`.
""".
-spec keyreplace(Key, N, TupleList1, NewTuple) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      NewTuple :: Tuple,
      Tuple :: tuple().

keyreplace(K, N, L, New) when is_integer(N), N > 0, is_tuple(New) ->
    keyreplace3(K, N, L, New).

keyreplace3(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace3(Key, Pos, [H|T], New) ->
    [H|keyreplace3(Key, Pos, T, New)];
keyreplace3(_, _, [], _) -> [].

-doc """
Searches the list of tuples `TupleList1` for a tuple whose `N`th element
compares equal to `Key`. Returns `{value, Tuple, TupleList2}` if such a tuple is
found, otherwise `false`. `TupleList2` is a copy of `TupleList1` where the first
occurrence of `Tuple` has been removed.
""".
-spec keytake(Key, N, TupleList1) -> {value, Tuple, TupleList2} | false when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [tuple()],
      TupleList2 :: [tuple()],
      Tuple :: tuple().

keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H|T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H|T], L) ->
    keytake(Key, N, T, [H|L]);
keytake(_K, _N, [], _L) -> false.

-doc """
Returns a copy of `TupleList1` where the first occurrence of a tuple `T` whose
`N`th element compares equal to `Key` is replaced with `NewTuple`, if there is
such a tuple `T`. If there is no such tuple `T`, a copy of `TupleList1` where
[`NewTuple`] has been appended to the end is returned.
""".
-spec keystore(Key, N, TupleList1, NewTuple) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple, ...],
      NewTuple :: Tuple,
      Tuple :: tuple().

keystore(K, N, L, New) when is_integer(N), N > 0, is_tuple(New) ->
    keystore2(K, N, L, New).

keystore2(Key, N, [H|T], New) when element(N, H) == Key ->
    [New|T];
keystore2(Key, N, [H|T], New) ->
    [H|keystore2(Key, N, T, New)];
keystore2(_Key, _N, [], New) ->
    [New].

-doc """
Returns a list containing the sorted elements of list `TupleList1`. Sorting is
performed on the `N`th element of the tuples. The sort is stable.
""".
-spec keysort(N, TupleList1) -> TupleList2 when
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

keysort(I, L) when is_integer(I), I > 0 ->
    case L of
    [] -> L;
    [_] -> L;
    [X, Y | T] ->
        case {element(I, X), element(I, Y)} of
        {EX, EY} when EX =< EY ->
            case T of
            [] ->
                L;
            [Z] ->
                case element(I, Z) of
                EZ when EY =< EZ ->
                    L;
                EZ when EX =< EZ ->
                    [X, Z, Y];
                _EZ ->
                    [Z, X, Y]
                end;
            _ when X == Y ->
                keysort_1(I, Y, EY, T, [X]);
            _ ->
                keysplit_1(I, X, EX, Y, EY, T, [], [])
            end;
        {EX, EY} ->
            case T of
            [] ->
                [Y, X];
            [Z] ->
                case element(I, Z) of
                EZ when EX =< EZ ->
                    [Y, X | T];
                EZ when EY =< EZ ->
                    [Y, Z, X];
                _EZ ->
                    [Z, Y, X]
                end;
            _ ->
                keysplit_2(I, X, EX, Y, EY, T, [], [])
            end
        end
    end.

keysort_1(I, X, EX, [Y | L], R) when X == Y ->
    keysort_1(I, Y, EX, L, [X | R]);
keysort_1(I, X, EX, [Y | L], R) ->
    case element(I, Y) of
    EY when EX =< EY ->
        keysplit_1(I, X, EX, Y, EY, L, R, []);
    EY ->
        keysplit_2(I, X, EX, Y, EY, L, R, [])
    end;
keysort_1(_I, X, _EX, [], R) ->
    lists:reverse(R, [X]).

-doc """
Returns the sorted list formed by merging `TupleList1` and `TupleList2`.

The merge is performed on the `N`th element of each tuple. Both `TupleList1` and
`TupleList2` must be key-sorted before evaluating this function. When two tuples
compare equal, the tuple from `TupleList1` is picked before the tuple from
`TupleList2`.
""".
-spec keymerge(N, TupleList1, TupleList2) -> TupleList3 when
      N :: pos_integer(),
      TupleList1 :: [T1],
      TupleList2 :: [T2],
      TupleList3 :: [(T1 | T2)],
      T1 :: Tuple,
      T2 :: Tuple,
      Tuple :: tuple().

keymerge(Index, L1, L2) when is_integer(Index), Index > 0 ->
    keymerge_1(Index, L1, L2).

keymerge_1(Index, [_|_]=T1, [H2 | T2]) -> 
    E2 = element(Index, H2),
    M = keymerge2_1(Index, T1, E2, H2, T2, []),
    lists:reverse(M, []);
keymerge_1(_Index, [_|_]=L1, []) ->
    L1;
keymerge_1(_Index, [], [_|_]=L2) ->
    L2;
keymerge_1(_Index, [], []) ->
    [].

%% reverse(rkeymerge(I,reverse(A),reverse(B))) is equal to keymerge(I,A,B).

-doc false.
-spec rkeymerge(pos_integer(), [X], [Y]) ->
    [R] when X :: tuple(), Y :: tuple(), R :: tuple().

rkeymerge(Index, L1, L2) when is_integer(Index), Index > 0 ->
    rkeymerge_1(Index, L1, L2).

rkeymerge_1(Index, [_|_]=T1, [H2 | T2]) -> 
    E2 = element(Index, H2),
    M = rkeymerge2_1(Index, T1, E2, H2, T2, []),
    lists:reverse(M, []);
rkeymerge_1(_Index, [_|_]=L1, []) ->
    L1;
rkeymerge_1(_Index, [], [_|_]=L2) ->
    L2;
rkeymerge_1(_Index, [], []) ->
    [].

-doc """
Returns a list containing the sorted elements of list `TupleList1` where all
except the first tuple of the tuples comparing equal have been deleted. Sorting
is performed on the `N`th element of the tuples.
""".
-spec ukeysort(N, TupleList1) -> TupleList2 when
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

ukeysort(I, L) when is_integer(I), I > 0 ->
    case L of
    [] -> L;
    [_] -> L;
    [X, Y | T] ->
            case {element(I, X), element(I, Y)} of
                {EX, EY} when EX == EY ->
                    ukeysort_1(I, X, EX, T);
                {EX, EY} when EX < EY ->
                    case T of
                        [] ->
                            L;
                        [Z] ->
                            case element(I, Z) of
                                EZ when EY == EZ ->
                                    [X, Y];
                                EZ when EY < EZ ->
                                    [X, Y, Z];
                                EZ when EZ == EX ->
                                    [X, Y];
                                EZ when EX =< EZ ->
                                    [X, Z, Y];
                                _EZ ->
                                    [Z, X, Y]
                            end;
                        _ ->
                            ukeysplit_1(I, X, EX, Y, EY, T, [], [])
                    end;
                {EX, EY} ->
                    case T of
                        [] ->
                            [Y, X];
                        [Z] ->
                            case element(I, Z) of
                                EZ when EX == EZ ->
                                    [Y, X];
                                EZ when EX < EZ ->
                                    [Y, X, Z];
                                EZ when EY == EZ ->
                                    [Y, X];
                                EZ when EY =< EZ ->
                                    [Y, Z, X];
                                _EZ ->
                                    [Z, Y, X]
                            end;
                        _ ->
                ukeysplit_2(I, Y, EY, T, [X])
                    end
        end
    end.

ukeysort_1(I, X, EX, [Y | L]) ->
    case element(I, Y) of
        EY when EX == EY ->
            ukeysort_1(I, X, EX, L);
    EY when EX < EY ->
        ukeysplit_1(I, X, EX, Y, EY, L, [], []);
    EY ->
        ukeysplit_2(I, Y, EY, L, [X])
    end;
ukeysort_1(_I, X, _EX, []) ->
    [X].

-doc """
Returns the sorted list formed by merging `TupleList1` and `TupleList2`. The
merge is performed on the `N`th element of each tuple. Both `TupleList1` and
`TupleList2` must be key-sorted without duplicates before evaluating this
function.

When two tuples compare equal, the tuple from `TupleList1` is picked
and the one from `TupleList2` is deleted.
""".
-spec ukeymerge(N, TupleList1, TupleList2) -> TupleList3 when
      N :: pos_integer(),
      TupleList1 :: [T1],
      TupleList2 :: [T2],
      TupleList3 :: [(T1 | T2)],
      T1 :: Tuple,
      T2 :: Tuple,
      Tuple :: tuple().

ukeymerge(Index, L1, L2) when is_integer(Index), Index > 0 ->
    ukeymerge_1(Index, L1, L2).

ukeymerge_1(Index, [H1 | T1], [_|_]=T2) ->
    E1 = element(Index, H1),
    M = ukeymerge2_2(Index, T1, E1, H1, T2, []),
    lists:reverse(M, []);
ukeymerge_1(_Index, [_|_]=L1, []) ->
    L1;
ukeymerge_1(_Index, [], [_|_]=L2) ->
    L2;
ukeymerge_1(_Index, [], []) ->
    [].

%% reverse(rukeymerge(I,reverse(A),reverse(B))) is equal to ukeymerge(I,A,B).

-doc false.
-spec rukeymerge(pos_integer(), [X], [Y]) ->
    [(X | Y)] when X :: tuple(), Y :: tuple().

rukeymerge(Index, L1, L2) when is_integer(Index), Index > 0 ->
    rukeymerge_1(Index, L1, L2).

rukeymerge_1(Index, [_|_]=T1, [H2 | T2]) ->
    E2 = element(Index, H2),
    M = rukeymerge2_1(Index, T1, E2, T2, [], H2),
    lists:reverse(M, []);
rukeymerge_1(_Index, [_|_]=L1, []) ->
    L1;
rukeymerge_1(_Index, [], [_|_]=L2) ->
    L2;
rukeymerge_1(_Index, [], []) ->
    [].

-doc """
Returns a list of tuples where, for each tuple in `TupleList1`, the `N`th
element `Term1` of the tuple has been replaced with the result of calling
`Fun(Term1)`.

_Examples:_

```erlang
> Fun = fun(Atom) -> atom_to_list(Atom) end.
#Fun<erl_eval.6.10732646>
2> lists:keymap(Fun, 2, [{name,jane,22},{name,lizzie,20},{name,lydia,15}]).
[{name,"jane",22},{name,"lizzie",20},{name,"lydia",15}]
```
""".
-spec keymap(Fun, N, TupleList1) -> TupleList2 when
      Fun :: fun((Term1 :: term()) -> Term2 :: term()),
      N :: pos_integer(),
      TupleList1 :: [Tuple],
      TupleList2 :: [Tuple],
      Tuple :: tuple().

keymap(Fun, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, Fun(element(Index, Tup)))|keymap(Fun, Index, Tail)];
keymap(Fun, Index, []) when is_integer(Index), Index >= 1, 
                            is_function(Fun, 1) -> [].

-doc(#{equiv => enumerate(1, 1, List1)}).
-doc(#{since => <<"OTP 25.0">>}).
-spec enumerate(List1) -> List2 when
      List1 :: [T],
      List2 :: [{Index, T}],
      Index :: integer(),
      T :: term().
enumerate(List1) ->
    enumerate(1, 1, List1).

-doc(#{equiv => enumerate(Index, 1, List1)}).
-doc(#{since => <<"OTP 25.0">>}).
-spec enumerate(Index, List1) -> List2 when
      List1 :: [T],
      List2 :: [{Index, T}],
      Index :: integer(),
      T :: term().
enumerate(Index, List1) ->
    enumerate(Index, 1, List1).

-doc """
Returns `List1` with each element `H` replaced by a tuple of form `{I, H}` where
`I` is the position of `H` in `List1`. The enumeration starts with `Index` and
increases by `Step` in each step.

That is, [`enumerate/3`](`enumerate/3`) behaves as if it had been defined as
follows:

```erlang
enumerate(I, S, List) ->
  {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc+S} end, I, List),
  List1.
```

The default values for `Index` and `Step` are both `1`.

_Examples:_

```erlang
> lists:enumerate([a,b,c]).
[{1,a},{2,b},{3,c}]
```

```erlang
> lists:enumerate(10, [a,b,c]).
[{10,a},{11,b},{12,c}]
```

```erlang
> lists:enumerate(0, -2, [a,b,c]).
[{0,a},{-2,b},{-4,c}]
```
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec enumerate(Index, Step, List1) -> List2 when
      List1 :: [T],
      List2 :: [{Index, T}],
      Index :: integer(),
      Step :: integer(),
      T :: term().
enumerate(Index, Step, List1) when is_integer(Index), is_integer(Step) ->
    enumerate_1(Index, Step, List1).

enumerate_1(Index, Step, [H|T]) ->
    [{Index, H}|enumerate_1(Index + Step, Step, T)];
enumerate_1(_Index, _Step, []) ->
    [].

%%% Suggestion from OTP-2948: sort and merge with Fun.

-doc """
Returns a list containing the sorted elements of `List1`, according to the
[ordering function](`m:lists#ordering_function`) `Fun`. `Fun(A, B)` is to return
`true` if `A` compares less than or equal to `B` in the ordering, otherwise
`false`.
""".
-spec sort(Fun, List1) -> List2 when
      Fun :: fun((A :: T, B :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

sort(Fun, []) when is_function(Fun, 2) ->
    [];
sort(Fun, [_] = L) when is_function(Fun, 2) ->
    L;
sort(Fun, [X, Y | T]) ->
    case Fun(X, Y) of
    true ->
        fsplit_1(Y, X, Fun, T, [], []);
    false ->
        fsplit_2(Y, X, Fun, T, [], [])
    end.

-doc """
Returns the sorted list formed by merging `List1` and `List2`. Both `List1` and
`List2` must be sorted according to the
[ordering function](`m:lists#ordering_function`) `Fun` before evaluating this
function.

`Fun(A, B)` is to return `true` if `A` compares less than or equal to
`B` in the ordering, otherwise `false`. When two elements compare equal, the
element from `List1` is picked before the element from `List2`.
""".
-spec merge(Fun, List1, List2) -> List3 when
      Fun :: fun((A, B) -> boolean()),
      List1 :: [A],
      List2 :: [B],
      List3 :: [(A | B)],
      A :: term(),
      B :: term().

merge(Fun, L1, L2) when is_function(Fun, 2) ->
    merge_1(Fun, L1, L2).

merge_1(Fun, [_|_]=T1, [H2 | T2]) ->
    lists:reverse(fmerge2_1(T1, H2, Fun, T2, []), []);
merge_1(_Fun, [_|_]=L1, []) ->
    L1;
merge_1(_Fun, [], [_|_]=L2) ->
    L2;
merge_1(_Fun, [], []) ->
    [].

%% reverse(rmerge(F,reverse(A),reverse(B))) is equal to merge(F,A,B).

-doc false.
-spec rmerge(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].

rmerge(Fun, L1, L2) when is_function(Fun, 2) ->
    rmerge_1(Fun, L1, L2).

rmerge_1(Fun, [_|_]=T1, [H2 | T2]) ->
    lists:reverse(rfmerge2_1(T1, H2, Fun, T2, []), []);
rmerge_1(_Fun, [_|_]=L1, []) ->
    L1;
rmerge_1(_Fun, [], [_|_]=L2) ->
    L2;
rmerge_1(_Fun, [], []) ->
    [].

-doc """
Returns a list containing the sorted elements of `List1` where all except the
first element of the elements comparing equal according to the
[ordering function](`m:lists#ordering_function`) `Fun` have been deleted.
`Fun(A, B)` is to return `true` if `A` compares less than or equal to `B` in the
ordering, otherwise `false`.
""".
-spec usort(Fun, List1) -> List2 when
      Fun :: fun((T, T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

usort(Fun, [_] = L) when is_function(Fun, 2) ->
    L;
usort(Fun, [] = L) when is_function(Fun, 2) ->
    L;
usort(Fun, [X | L]) when is_function(Fun, 2) ->
    usort_1(Fun, X, L).

usort_1(Fun, X, [Y | L]) ->
    case Fun(X, Y) of
        true ->
            case Fun(Y, X) of
                true -> % X equal to Y
                    case L of
                        [] ->
                            [X];
                        _ ->
                            usort_1(Fun, X, L)
                    end;
                false ->
                    ufsplit_1(Y, X, Fun, L, [], [])
            end;
        false  ->
        ufsplit_2(Y, L, Fun, [X])
    end.
                    
-doc """
Returns the sorted list formed by merging `List1` and `List2`. Both `List1` and
`List2` must be sorted according to the
[ordering function](`m:lists#ordering_function`) `Fun` and contain no duplicates
before evaluating this function.

`Fun(A, B)` is to return `true` if `A` compares
less than or equal to `B` in the ordering, otherwise `false`. When two elements
compare equal, the element from `List1` is picked and the one from `List2` is
deleted.
""".
-spec umerge(Fun, List1, List2) -> List3 when
      Fun :: fun((A, B) -> boolean()),
      List1 :: [A],
      List2 :: [B],
      List3 :: [(A | B)],
      A :: term(),
      B :: term().

umerge(Fun, L1, L2) when is_function(Fun, 2) ->
    umerge_1(Fun, L1, L2).

umerge_1(Fun, [H1 | T1], [_|_]=T2) ->
    lists:reverse(ufmerge2_2(H1, T1, Fun, T2, []), []);
umerge_1(_Fun, [_|_]=L1, []) ->
    L1;
umerge_1(_Fun, [], [_|_]=L2) ->
    L2;
umerge_1(_Fun, [], []) ->
    [].

%% reverse(rumerge(F,reverse(A),reverse(B))) is equal to umerge(F,A,B).

-doc false.
-spec rumerge(fun((X, Y) -> boolean()), [X], [Y]) -> [(X | Y)].

rumerge(Fun, L1, L2) when is_function(Fun, 2) ->
    rumerge_1(Fun, L1, L2).

rumerge_1(Fun, [_|_]=T1, [H2 | T2]) ->
    lists:reverse(rufmerge2_1(T1, H2, Fun, T2, []), []);
rumerge_1(_Fun, [_|_]=L1, []) ->
    L1;
rumerge_1(_Fun, [], [_|_]=L2) ->
    L2;
rumerge_1(_Fun, [], []) ->
    [].

%% usort(List) -> L
%%  sorts the list L, removes duplicates

-doc """
Returns a list containing the sorted elements of `List1` where all except the
first element of the elements comparing equal have been deleted.
""".
-spec usort(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

usort([X, Y | L] = L0) when X < Y ->
    case L of
    [] ->
        L0;
    [Z] when Y < Z ->
        L0;
    [Z] when Y == Z ->
        [X, Y];
    [Z] when Z < X ->
        [Z, X, Y];
    [Z] when Z == X ->
        [X, Y];
    [Z] ->
        [X, Z, Y];
    _ ->
        usplit_1(X, Y, L, [], [])
    end;
usort([X, Y | L]) when X > Y ->
    case L of
    [] ->
        [Y, X];
    [Z] when X < Z ->
        [Y, X | L];
    [Z] when X == Z ->
        [Y, X];
    [Z] when Z < Y ->
        [Z, Y, X];
    [Z] when Z == Y ->
        [Y, X];
    [Z] ->
        [Y, Z, X];
        _ ->
            usplit_2(X, Y, L, [], [])
    end;
usort([X, _Y | L]) ->
    usort_1(X, L);
usort([_] = L) ->
    L;
usort([]) ->
    [].

usort_1(X, [Y | L]) when X == Y ->
    usort_1(X, L);
usort_1(X, [Y | L]) when X < Y ->
    usplit_1(X, Y, L, [], []);
usort_1(X, [Y | L]) ->
    usplit_2(X, Y, L, [], []);
usort_1(X, []) ->
    [X].

%% umerge(List) -> L
%%  merges a list of sorted lists without duplicates, removes duplicates

-doc """
Returns the sorted list formed by merging all the sublists of `ListOfLists`. All
sublists must be sorted and contain no duplicates before evaluating this
function.

When two elements compare equal, the element from the sublist with the
lowest position in `ListOfLists` is picked and the other is deleted.
""".
-spec umerge(ListOfLists) -> List1 when
      ListOfLists :: [List],
      List :: [T],
      List1 :: [T],
      T :: term().

umerge(L) ->
    umergel(L).

%% umerge3(X, Y, Z) -> L
%%  merges three sorted lists X, Y and Z without duplicates, 
%%  removes duplicates

-doc """
Returns the sorted list formed by merging `List1`, `List2`, and `List3`. All of
`List1`, `List2`, and `List3` must be sorted and contain no duplicates before
evaluating this function.

When two elements compare equal, the element from
`List1` is picked if there is such an element, otherwise the element from
`List2` is picked, and the other is deleted.
""".
-spec umerge3(List1, List2, List3) -> List4 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [Z],
      List4 :: [(X | Y | Z)],
      X :: term(),
      Y :: term(),
      Z :: term().

umerge3([_|_]=L1, [H2 | T2], [H3 | T3]) ->
    lists:reverse(umerge3_1(L1, [H2 | H3], T2, H2, [], T3, H3), []);
umerge3([_|_]=L1, [_|_]=L2, []) ->
    umerge(L1, L2);
umerge3([_|_]=L1, [], [_|_]=L3) ->
    umerge(L1, L3);
umerge3([_|_]=L1, [], []) ->
    L1;
umerge3([], [_|_]=L2, [_|_]=L3) ->
    umerge(L2, L3);
umerge3([], [_|_]=L2, []) ->
    L2;
umerge3([], [], [_|_]=L3) ->
    L3;
umerge3([], [], []) ->
    [].

%% rumerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z without duplicates,
%%  removes duplicates

-doc false.
-spec rumerge3([X], [Y], [Z]) -> [(X | Y | Z)].

rumerge3([_|_]=L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(rumerge3_1(L1, T2, H2, [], T3, H3),[]);
rumerge3([_|_]=L1, [_|_]=L2, []) ->
    rumerge(L1, L2);
rumerge3([_|_]=L1, [], [_|_]=L3) ->
    rumerge(L1, L3);
rumerge3([_|_]=L1, [], []) ->
    L1;
rumerge3([], [_|_]=L2, [_|_]=L3) ->
    rumerge(L2, L3);
rumerge3([], [_|_]=L2, []) ->
    L2;
rumerge3([], [], [_|_]=L3) ->
    L3;
rumerge3([], [], []) ->
    [].

%% umerge(X, Y) -> L
%%  merges two sorted lists X and Y without duplicates, removes duplicates

-doc """
Returns the sorted list formed by merging `List1` and `List2`. Both `List1` and
`List2` must be sorted and contain no duplicates before evaluating this
function.

When two elements compare equal, the element from `List1` is picked
and the one from `List2` is deleted.
""".
-spec umerge(List1, List2) -> List3 when
      List1 :: [X],
      List2 :: [Y],
      List3 :: [(X | Y)],
      X :: term(),
      Y :: term().

umerge([H1 | T1], [_|_]=T2) ->
    lists:reverse(umerge2_2(T1, T2, [], H1), []);
umerge([_|_]=L1, []) ->
    L1;
umerge([], [_|_]=L2) ->
    L2;
umerge([], []) ->
    [].

%% rumerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y without duplicates,
%%  removes duplicates

%% reverse(rumerge(reverse(A),reverse(B))) is equal to umerge(I,A,B).

-doc false.
-spec rumerge([X], [Y]) -> [(X | Y)].

rumerge([_|_]=T1, [H2 | T2]) ->
    lists:reverse(rumerge2_1(T1, T2, [], H2), []);
rumerge([_|_]=L1, []) ->
    L1;
rumerge([], [_|_]=L2) ->
    L2;
rumerge([], []) ->
    [].

%% all(Predicate, List)
%% any(Predicate, List)
%% map(Function, List)
%% flatmap(Function, List)
%% foldl(Function, First, List)
%% foldr(Function, Last, List)
%% filter(Predicate, List)
%% zf(Function, List)
%% mapfoldl(Function, First, List)
%% mapfoldr(Function, Last, List)
%% foreach(Function, List)
%% takewhile(Predicate, List)
%% dropwhile(Predicate, List)
%% splitwith(Predicate, List)
%%  for list programming. Function here is a 'fun'.
%% 
%%  The name zf is a joke!
%%
%%  N.B. Unless where the functions actually needs it only foreach/2/3,
%%  which is meant to be used for its side effects, has a defined order
%%  of evaluation.
%%
%%  There are also versions with an extra argument, ExtraArgs, which is a
%%  list of extra arguments to each call.

-doc """
Returns `true` if `Pred(Elem)` returns `true` for all elements `Elem` in `List`,
otherwise `false`. The `Pred` function must return a boolean.
""".
-spec all(Pred, List) -> boolean() when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      T :: term().

all(Pred, List) when is_function(Pred, 1) ->
    case List of
        [Hd | Tail] ->
            case Pred(Hd) of
                true -> all_1(Pred, Tail);
                false -> false
            end;
        [] -> true
    end.

all_1(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true -> all_1(Pred, Tail);
        false -> false
    end;
all_1(_Pred, []) ->
    true.

-doc """
Returns `true` if `Pred(Elem)` returns `true` for at least one element `Elem` in
`List`. The `Pred` function must return a boolean.
""".
-spec any(Pred, List) -> boolean() when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      T :: term().

any(Pred, List) when is_function(Pred, 1) ->
    case List of
        [Hd | Tail] ->
            case Pred(Hd) of
                true -> true;
                false -> any_1(Pred, Tail)
            end;
        [] -> false
    end.

any_1(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true -> true;
        false -> any_1(Pred, Tail)
    end;
any_1(_Pred, []) ->
    false.

-doc """
Takes a function from `A`s to `B`s, and a list of `A`s and produces a list of
`B`s by applying the function to every element in the list. This function is
used to obtain the return values. The evaluation order depends on the
implementation.
""".
-spec map(Fun, List1) -> List2 when
      Fun :: fun((A) -> B),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

map(F, List) when is_function(F, 1) ->
    case List of
        [Hd | Tail] -> [F(Hd) | map_1(F, Tail)];
        [] -> []
    end.

map_1(F, [Hd | Tail]) ->
    [F(Hd) | map_1(F, Tail)];
map_1(_F, []) ->
    [].

-doc """
Takes a function from `A`s to lists of `B`s, and a list of `A`s (`List1`) and
produces a list of `B`s by applying the function to every element in `List1` and
appending the resulting lists.

That is, `flatmap` behaves as if it had been defined as follows:

```erlang
flatmap(Fun, List1) ->
    append(map(Fun, List1)).
```

_Example:_

```erlang
> lists:flatmap(fun(X)->[X,X] end, [a,b,c]).
[a,a,b,b,c,c]
```
""".
-spec flatmap(Fun, List1) -> List2 when
      Fun :: fun((A) -> [B]),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

flatmap(F, List) when is_function(F, 1) ->
    flatmap_1(F, List).

flatmap_1(F, [Hd | Tail]) ->
    case F(Hd) of
        %% The two first clauses are an optimization.
        [] -> flatmap_1(F, Tail);
        [Elem] -> [Elem | flatmap_1(F, Tail)];
        List -> List ++ flatmap_1(F, Tail)
    end;
flatmap_1(_F, []) ->
    [].

-doc """
Calls `Fun(Elem, AccIn)` on successive elements `A` of `List`, starting with
`AccIn == Acc0`. `Fun/2` must return a new accumulator, which is passed to the
next call. The function returns the final value of the accumulator. `Acc0` is
returned if the list is empty.

_Example:_

```erlang
> lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).
15
> lists:foldl(fun(X, Prod) -> X * Prod end, 1, [1,2,3,4,5]).
120
```
""".
-spec foldl(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldl(F, Accu, List) when is_function(F, 2) ->
    case List of
        [Hd | Tail] -> foldl_1(F, F(Hd, Accu), Tail);
        [] -> Accu
    end.

foldl_1(F, Accu, [Hd | Tail]) ->
    foldl_1(F, F(Hd, Accu), Tail);
foldl_1(_F, Accu, []) ->
    Accu.

-doc """
Like `foldl/3`, but the list is traversed from right to left.

_Example:_

```erlang
> P = fun(A, AccIn) -> io:format("~p ", [A]), AccIn end.
#Fun<erl_eval.12.2225172>
> lists:foldl(P, void, [1,2,3]).
1 2 3 void
> lists:foldr(P, void, [1,2,3]).
3 2 1 void
```

[`foldl/3`](`foldl/3`) is tail recursive and is usually preferred to
[`foldr/3`](`foldr/3`).
""".
-spec foldr(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldr(F, Accu, List) when is_function(F, 2) ->
    foldr_1(F, Accu, List).

foldr_1(F, Accu, [Hd | Tail]) ->
    F(Hd, foldr_1(F, Accu, Tail));
foldr_1(_F, Accu, []) ->
    Accu.

-doc """
`List2` is a list of all elements `Elem` in `List1` for which `Pred(Elem)`
returns `true`. The `Pred` function must return a boolean.
""".
-spec filter(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

filter(Pred, List) when is_function(Pred, 1) ->
    [ E || E <- List, Pred(E) ].

%% Equivalent to {filter(F, L), filter(NotF, L)}, if NotF = 'fun(X) ->
%% not F(X) end'.

-doc """
Partitions `List` into two lists, where the first list contains all elements for
which `Pred(Elem)` returns `true`, and the second list contains all elements for
which `Pred(Elem)` returns `false`.

_Examples:_

```erlang
> lists:partition(fun(A) -> A rem 2 == 1 end, [1,2,3,4,5,6,7]).
{[1,3,5,7],[2,4,6]}
> lists:partition(fun(A) -> is_atom(A) end, [a,b,1,c,d,2,3,4,e]).
{[a,b,c,d,e],[1,2,3,4]}
```

For a different way to partition a list, see `splitwith/2`.
""".
-spec partition(Pred, List) -> {Satisfying, NotSatisfying} when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      Satisfying :: [T],
      NotSatisfying :: [T],
      T :: term().

partition(Pred, L) when is_function(Pred, 1) ->
    partition_1(Pred, L, [], []).

partition_1(Pred, [H | T], As, Bs) ->
    case Pred(H) of
        true -> partition_1(Pred, T, [H | As], Bs);
        false -> partition_1(Pred, T, As, [H | Bs])
    end;
partition_1(_Pred, [], As, Bs) ->
    {reverse(As), reverse(Bs)}.

-doc """
Calls `Fun(Elem)` on successive elements `Elem` of `List1` in order to update or
remove elements from `List1`.

`Fun/1` must return either a Boolean or a tuple `{true, Value}`. The function
returns the list of elements for which `Fun` returns a new value, where a value
of `true` is synonymous with `{true, Elem}`.

That is, `filtermap` behaves as if it had been defined as follows:

```erlang
filtermap(Fun, List1) ->
    lists:foldr(fun(Elem, Acc) ->
                       case Fun(Elem) of
                           false -> Acc;
                           true -> [Elem|Acc];
                           {true,Value} -> [Value|Acc]
                       end
                end, [], List1).
```

_Example:_

```erlang
> lists:filtermap(fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end, [1,2,3,4,5]).
[1,2]
```
""".
-doc(#{since => <<"OTP R16B01">>}).
-spec filtermap(Fun, List1) -> List2 when
      Fun :: fun((Elem) -> boolean() | {'true', Value}),
      List1 :: [Elem],
      List2 :: [Elem | Value],
      Elem :: term(),
      Value :: term().

filtermap(F, List) when is_function(F, 1) ->
    filtermap_1(F, List).

filtermap_1(F, [Hd|Tail]) ->
    case F(Hd) of
        true ->
            [Hd | filtermap_1(F, Tail)];
        {true,Val} ->
            [Val | filtermap_1(F, Tail)];
        false ->
            filtermap_1(F, Tail)
    end;
filtermap_1(_F, []) ->
    [].

-doc false.
-spec zf(fun((T) -> boolean() | {'true', X}), [T]) -> [(T | X)].

zf(F, L) ->
    filtermap(F, L).

-doc """
Calls `Fun(Elem)` for each element `Elem` in `List`. This function is used for
its side effects and the evaluation order is defined to be the same as the order
of the elements in the list.
""".
-spec foreach(Fun, List) -> ok when
      Fun :: fun((Elem :: T) -> term()),
      List :: [T],
      T :: term().

foreach(F, List) when is_function(F, 1) ->
    foreach_1(F, List).

foreach_1(F, [Hd | Tail]) ->
    F(Hd),
    foreach_1(F, Tail);
foreach_1(_F, []) ->
    ok.

-doc """
Combines the operations of `map/2` and `foldl/3` into one pass.

_Example:_

Summing the elements in a list and double them at the same time:

```erlang
> lists:mapfoldl(fun(X, Sum) -> {2*X, X+Sum} end,
0, [1,2,3,4,5]).
{[2,4,6,8,10],15}
```
""".
-spec mapfoldl(Fun, Acc0, List1) -> {List2, Acc1} when
      Fun :: fun((A, AccIn) -> {B, AccOut}),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

mapfoldl(F, Accu, List) when is_function(F, 2) ->
    mapfoldl_1(F, Accu, List).

mapfoldl_1(F, Accu0, [Hd | Tail]) ->
    {R, Accu1} = F(Hd, Accu0),
    {Rs, Accu2} = mapfoldl_1(F, Accu1, Tail),
    {[R | Rs], Accu2};
mapfoldl_1(_F, Accu, []) ->
    {[], Accu}.

-doc "Combines the operations of `map/2` and `foldr/3` into one pass.".
-spec mapfoldr(Fun, Acc0, List1) -> {List2, Acc1} when
      Fun :: fun((A, AccIn) -> {B, AccOut}),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

mapfoldr(F, Accu, List) when is_function(F, 2) ->
    mapfoldr_1(F, Accu, List).

mapfoldr_1(F, Accu0, [Hd|Tail]) ->
    {Rs, Accu1} = mapfoldr_1(F, Accu0, Tail),
    {R, Accu2} = F(Hd, Accu1),
    {[R | Rs], Accu2};
mapfoldr_1(_F, Accu, []) ->
    {[], Accu}.

-doc """
Takes elements `Elem` from `List1` while `Pred(Elem)` returns `true`, that is,
the function returns the longest prefix of the list for which all elements
satisfy the predicate. The `Pred` function must return a boolean.
""".
-spec takewhile(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

takewhile(Pred, List) when is_function(Pred, 1) ->
    takewhile_1(Pred, List).

takewhile_1(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true -> [Hd | takewhile_1(Pred, Tail)];
        false -> []
    end;
takewhile_1(_Pred, []) ->
    [].

-doc """
Drops elements `Elem` from `List1` while `Pred(Elem)` returns `true` and returns
the remaining list. The `Pred` function must return a boolean.
""".
-spec dropwhile(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

dropwhile(Pred, List) when is_function(Pred, 1) ->
    dropwhile_1(Pred, List).

dropwhile_1(Pred, [Hd | Tail]=Rest) ->
    case Pred(Hd) of
        true -> dropwhile_1(Pred, Tail);
        false -> Rest
    end;
dropwhile_1(_Pred, []) ->
    [].

-doc """
If there is a `Value` in `List` such that `Pred(Value)` returns `true`, returns
`{value, Value}` for the first such `Value`, otherwise returns `false`. The
`Pred` function must return a boolean.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec search(Pred, List) -> {value, Value} | false when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      Value :: T.

search(Pred, List) when is_function(Pred, 1) ->
    search_1(Pred, List).

search_1(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true -> {value, Hd};
        false -> search_1(Pred, Tail)
    end;
search_1(_Pred, []) ->
    false.

-doc """
Partitions `List` into two lists according to `Pred`.
[`splitwith/2`](`splitwith/2`) behaves as if it is defined as follows:

```erlang
splitwith(Pred, List) ->
    {takewhile(Pred, List), dropwhile(Pred, List)}.
```

_Examples:_

```erlang
> lists:splitwith(fun(A) -> A rem 2 == 1 end, [1,2,3,4,5,6,7]).
{[1],[2,3,4,5,6,7]}
> lists:splitwith(fun(A) -> is_atom(A) end, [a,b,1,c,d,2,3,4,e]).
{[a,b],[1,c,d,2,3,4,e]}
```

The `Pred` function must return a boolean. For a different way to partition a
list, see `partition/2`.
""".
-spec splitwith(Pred, List) -> {List1, List2} when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      List1 :: [T],
      List2 :: [T],
      T :: term().

splitwith(Pred, List) when is_function(Pred, 1) ->
    splitwith_1(Pred, List, []).

splitwith_1(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
    true -> splitwith_1(Pred, Tail, [Hd|Taken]);
    false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith_1(_Pred, [], Taken) ->
    {reverse(Taken),[]}.

-doc """
Splits `List1` into `List2` and `List3`. `List2` contains the first `N` elements
and `List3` the remaining elements (the `N`th tail).
""".
-spec split(N, List1) -> {List2, List3} when
      N :: non_neg_integer(),
      List1 :: [T],
      List2 :: [T],
      List3 :: [T],
      T :: term().

split(N, List) when is_integer(N), N >= 0, is_list(List) ->
    case split(N, List, []) of
    {_, _} = Result -> Result;
    Fault when is_atom(Fault) ->
        erlang:error(Fault, [N,List])
    end;
split(N, List) ->
    erlang:error(badarg, [N,List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H|T], R) ->
    split(N-1, T, [H|R]);
split(_, [], _) ->
    badarg.

-doc """
Inserts `Sep` between each element in `List1`. Has no effect on the empty list
and on a singleton list. For example:

```erlang
> lists:join(x, [a,b,c]).
[a,x,b,x,c]
> lists:join(x, [a]).
[a]
> lists:join(x, []).
[]
```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

%%% =================================================================
%%% Here follows the implementation of the sort functions.
%%%
%%% These functions used to be in their own module (lists_sort),
%%% but have now been placed here to allow Dialyzer to produce better
%%% type information.
%%% =================================================================

-compile({inline, 
          [{merge3_12,7}, {merge3_21,7}, {rmerge3_12,7}, {rmerge3_21,7}]}).

-compile({inline, 
          [{umerge3_12,8}, {umerge3_21,8},
       {rumerge3_12a,7}, {rumerge3_12b,8}]}).

-compile({inline, 
          [{keymerge3_12,12}, {keymerge3_21,12}, 
           {rkeymerge3_12,12}, {rkeymerge3_21,12}]}).

-compile({inline,
          [{ukeymerge3_12,13}, {ukeymerge3_21,13},
       {rukeymerge3_12a,11}, {rukeymerge3_21a,13},
       {rukeymerge3_12b,12}, {rukeymerge3_21b,12}]}).

%% sort/1

%% Ascending.
split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
    split_1(Y, Z, L, [X | R], Rs);
split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
    split_1(Z, Y, L, [X | R], Rs);
split_1(X, Y, [Z | L], [], Rs) ->
    split_1(X, Y, L, [Z], Rs);
split_1(X, Y, [Z | L], R, Rs) ->
    split_1_1(X, Y, L, R, Rs, Z);
split_1(X, Y, [], R, Rs) ->
    rmergel([[Y, X | R] | Rs], []).

split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
    split_1_1(Y, Z, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
    split_1_1(Z, Y, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
    split_1(S, Z, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [Z | L], R, Rs, S) ->
    split_1(Z, S, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [], R, Rs, S) ->
    rmergel([[S], [Y, X | R] | Rs], []).

%% Descending.
split_2(X, Y, [Z | L], R, Rs) when Z =< Y ->
    split_2(Y, Z, L, [X | R], Rs);
split_2(X, Y, [Z | L], R, Rs) when Z =< X ->
    split_2(Z, Y, L, [X | R], Rs);
split_2(X, Y, [Z | L], [], Rs) ->
    split_2(X, Y, L, [Z], Rs);
split_2(X, Y, [Z | L], R, Rs) ->
    split_2_1(X, Y, L, R, Rs, Z);
split_2(X, Y, [], R, Rs) ->
    mergel([[Y, X | R] | Rs], []).

split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< Y ->
    split_2_1(Y, Z, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< X ->
    split_2_1(Z, Y, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
    split_2(S, Z, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [Z | L], R, Rs, S) ->
    split_2(Z, S, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [], R, Rs, S) ->
    mergel([[S], [Y, X | R] | Rs], []).

%% merge/1
%% @private
%% @doc Merges multiple lists into a single list, preserving order and combining efficiently.
%%
%% The `mergel/2` function processes a collection of lists and combines them into a single, 
%% sorted list or concatenated result. It uses recursive logic to handle varying input cases, 
%% such as empty lists, nested lists, and reversed lists. The merging logic can handle up to 
%% three lists at a time for efficient processing.
%%
%% Example:
%%     mergel([[1, 2], [3, 4], [5, 6]], []). % returns [1, 2, 3, 4, 5, 6]
%%     mergel([[3, 2, 1], [6, 5, 4]], []).   % returns [1, 2, 3, 4, 5, 6]
%%     mergel([], []).                       % returns []
%%
%% @spec mergel(Lists, Acc) -> MergedList
%%       when Lists :: [[T]],
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in all input lists.

mergel([], []) ->
    [];
mergel([[_|_]=L], []) ->
    L;
mergel([], Acc) ->
    rmergel(Acc, []);
mergel([[] | L], Acc) ->
    mergel(L, Acc);
mergel([[_|_]=L], Acc) ->
    rmergel([lists:reverse(L, []) | Acc], []);
mergel([[_|_]=A, [] | L], Acc) ->
    mergel([A | L], Acc);
mergel([[_|_]=A, [_|_]=B, [] | L], Acc) ->
    mergel([A, B | L], Acc);
mergel([[_|_]=T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    mergel(L, [merge3_1(T1, [], H2, T2, H3, T3) | Acc]);
mergel([[_|_]=T1, [H2 | T2]], Acc) ->
    rmergel([merge2_1(T1, H2, T2, []) | Acc], []).

%% @private
%% @doc Recursively merges and processes a list of lists, reversing the intermediate results as needed.
%%
%% The `rmergel/2` function is a helper function used in conjunction with `mergel/2` to reverse and 
%% recursively merge lists while building the final output. It efficiently handles cases where the 
%% input consists of multiple lists, merging them in reverse order and delegating further processing 
%% to `mergel/2` as necessary.
%%
%% Example:
%%     rmergel([[1, 2], [3, 4], [5, 6]], []). % Internally processes and reverses to aid merging.
%%     rmergel([], []).                       % Delegates to `mergel/2` with the accumulated result.
%%
%% @spec rmergel(Lists, Acc) -> MergedList
%%       when Lists :: [[T]],
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in all input lists.
rmergel([[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    rmergel(L, [rmerge3_1(T1, [], H2, T2, H3, T3) | Acc]);
rmergel([[H2 | T2], T1], Acc) ->
    mergel([rmerge2_1(T1, H2, T2, []) | Acc], []);
rmergel([L], Acc) ->
    mergel([lists:reverse(L, []) | Acc], []);
rmergel([], Acc) ->
    mergel(Acc, []).

%% merge3/3

%% Take L1 apart.
%% @private
%% @doc Merges three sorted lists into a single sorted list by comparing their heads recursively.
%%
%% The `merge3_1/6` function takes three sorted lists and an accumulator, and merges the elements 
%% into a single sorted list. It compares the heads of the lists (`H1`, `H2`, `H3`) to determine 
%% the smallest element, and processes the remaining elements recursively. Special cases handle 
%% when one or more of the lists are empty.
%%
%% Example:
%%     merge3_1([1, 4], [], 2, [5], 3, [6]). % Internally processes and merges the lists into one.
%%
%% @spec merge3_1(List1, Acc, H2, List2, H3, List3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            Acc :: [T],
%%            H2 :: T,
%%            H3 :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
merge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    merge2_1(T2, H3, T3, [H2 | M]);
merge3_1([], M, H2, T2, H3, T3) ->
    merge2_2(T2, H3, T3, M, H2).

%% Take L2 apart.
%% @private
%% @doc Merges three sorted lists into a single sorted list, prioritizing the first and second lists.
%%
%% The `merge3_2/6` function merges three sorted lists (`T1`, `T2`, `T3`) into one sorted list by 
%% recursively comparing their heads (`H1`, `H2`, `H3`). The function prioritizes the first and 
%% second lists during comparisons, and delegates to helper functions when one of the lists is empty.
%%
%% Example:
%%     merge3_2([1, 4], 2, [], [3, 5], 6, [7]).
%%     % Processes the input lists and merges them into a single sorted list.
%%
%% @spec merge3_2(List1, H1, Acc, List2, H3, List3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, [], H3, T3) ->
    merge2_2(T1, H3, T3, M, H1).

% H1 =< H2. Inlined.
%% @private
%% @doc Merges three sorted lists into a single sorted list, prioritizing the first and second lists in comparisons.
%%
%% The `merge3_12/7` function recursively merges three sorted lists (`T1`, `T2`, `T3`) into one sorted list 
%% by comparing the heads of the first and second lists (`H1` and `H2`) with the head of the third list (`H3`). 
%% It prioritizes merging the first and second lists, but efficiently handles elements from the third list when necessary.
%%
%% Example:
%%     merge3_12([2, 4], 1, 3, [5], 6, [7, 8], []).
%%     % Processes and merges the three lists into a single sorted list.
%%
%% @spec merge3_12(List1, H1, H2, List2, H3, List3, Acc) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            H3 :: T,
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
%% @private
%% @doc Merges three sorted lists into a single sorted list, prioritizing the first and second lists but handling the third efficiently.
%%
%% The `merge3_12_3/6` function is a continuation of the `merge3_12/7` logic. It merges three sorted lists 
%% (`T1`, `T2`, `T3`) into one sorted list by comparing the heads of the first and third lists (`H1` and `H3`), 
%% while ensuring the second list is processed as part of the merging. It handles cases where the third list is 
%% empty, delegating to `merge2_1` to merge the remaining elements.
%%
%% Example:
%%     merge3_12_3([2, 4], 1, 3, [5], [], [6, 7]).
%%     % Processes and merges the three lists into a single sorted list.
%%
%% @spec merge3_12_3(List1, H1, H2, List2, Acc, List3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, []) ->
    merge2_1(T1, H2, T2, [H1 | M]).

% H1 > H2. Inlined.
%% @private
%% @doc Merges three sorted lists into a single sorted list, prioritizing the second and third lists when `H1 > H2`.
%%
%% The `merge3_21/7` function handles cases where the head of the first list (`H1`) is greater than the head of the second list (`H2`). 
%% It compares `H2` with the head of the third list (`H3`) to determine the next smallest element to merge. This function efficiently 
%% processes the remaining elements of the lists by delegating to `merge3_2` or `merge3_21_3` as needed.
%%
%% Example:
%%     merge3_21([4, 5], 6, 3, [7, 8], 2, [9, 10], []).
%%     % Merges the lists into a single sorted result.
%%
%% @spec merge3_21(List1, H1, H2, List2, H3, List3, Acc) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            H3 :: T,
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
merge3_21(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21(T1, H1, H2, T2, H3, T3, M) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 > H2, take L3 apart.
%% @private
%% @doc Merges three sorted lists into a single sorted list, prioritizing the second list when `H1 > H2` and taking apart the third list.
%%
%% The `merge3_21_3/6` function handles cases where the head of the first list (`H1`) is greater than the head of the second list (`H2`), 
%% and recursively processes the third list (`L3`). It compares `H2` with `H3` (the head of the third list) to determine the next smallest 
%% element to merge. If the third list is empty, it delegates to `merge2_2` to handle the remaining elements of `T1` and `T2`.
%%
%% Example:
%%     merge3_21_3([4, 5], 6, 3, [7, 8], [], [9, 10]).
%%     % Merges the lists into a single sorted result.
%%
%% @spec merge3_21_3(List1, H1, H2, List2, Acc, List3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21_3(T1, H1, H2, T2, M, []) ->
    merge2_2(T1, H2, T2, M, H1).

%% rmerge/3

%% Take L1 apart.
%% @private
%% @doc Recursively merges three sorted lists in reverse order, prioritizing the first list when applicable.
%%
%% The `rmerge3_1/6` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It compares the head of the first list (`H1`) with the heads of the second (`H2`) and third (`H3`) lists, 
%% and recursively processes the lists, appending the results to the accumulator (`M`). Special cases handle 
%% when one or more lists are empty.
%%
%% Example:
%%     rmerge3_1([1, 2], [], 3, [4, 5], 6, [7, 8]).
%%     % Merges the lists into a single reversed result.
%%
%% @spec rmerge3_1(List1, Acc, H2, List2, H3, List3) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            Acc :: [T],
%%            H2 :: T,
%%            H3 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    rmerge2_2(T2, H3, T3, M, H2);
rmerge3_1([], M, H2, T2, H3, T3) ->
    rmerge2_1(T2, H3, T3, [H2 | M]).

%% Take L2 apart.
%% @private
%% @doc Recursively merges three sorted lists in reverse order, prioritizing the first and second lists.
%%
%% The `rmerge3_2/6` function is part of the reverse merging process for three sorted lists. It handles cases 
%% where the head of the first list (`H1`) is compared with the head of the second list (`H2`). If the second 
%% list is empty, it proceeds to compare `H1` with the head of the third list (`H3`). The results are appended 
%% to the accumulator (`M`) in reverse order.
%%
%% Example:
%%     rmerge3_2([2, 4], 1, [], [3, 5], 6, [7, 8]).
%%     % Merges the lists into a single reversed result.
%%
%% @spec rmerge3_2(List1, H1, Acc, List2, H3, List3) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    rmerge2_2(T1, H3, T3, M, H1);
rmerge3_2(T1, H1, M, [], H3, T3) ->
    rmerge2_1(T1, H3, T3, [H1 | M]).

% H1 =< H2. Inlined.
%% @private
%% @doc Recursively merges three sorted lists in reverse order, prioritizing the second and third lists.
%%
%% The `rmerge3_12/7` function processes three sorted lists in reverse order, focusing on comparisons 
%% between the second list (`H2` and `T2`) and the third list (`H3` and `T3`). If the head of the second 
%% list (`H2`) is less than or equal to the head of the third list (`H3`), it appends `H3` to the accumulator 
%% (`M`) and continues merging. Otherwise, it prioritizes merging `H2` and delegates further processing 
%% to `rmerge3_2`.
%%
%% Example:
%%     rmerge3_12([2, 4], 1, 3, [5], 6, [7, 8], []).
%%     % Merges the lists into a single reversed result.
%%
%% @spec rmerge3_12(List1, H1, H2, List2, H3, List3, Acc) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            H3 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rmerge3_12(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 =< H2, take L3 apart.
%% @private
%% @doc Recursively merges three sorted lists in reverse order, prioritizing the second and third lists.
%%
%% The `rmerge3_12_3/6` function processes three sorted lists in reverse order, focusing on comparisons 
%% between the second list (`H2` and `T2`) and the third list (`H3` and `T3`). If the head of the second 
%% list (`H2`) is less than or equal to the head of the third list (`H3`), it appends `H3` to the accumulator 
%% (`M`) and continues merging. Otherwise, it prioritizes `H2` and delegates to `rmerge3_2`. When the third 
%% list is empty, it completes the merge using `rmerge2_2`.
%%
%% Example:
%%     rmerge3_12_3([2, 4], 1, 3, [5], [], [7, 8]).
%%     % Merges the lists into a single reversed result.
%%
%% @spec rmerge3_12_3(List1, H1, H2, List2, Acc, List3) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rmerge3_12_3(T1, H1, H2, T2, M, []) ->
    rmerge2_2(T1, H2, T2, M, H1).

% H1 > H2. Inlined.
%% @private
%% @doc Recursively merges three sorted lists in reverse order, prioritizing the first and third lists.
%%
%% The `rmerge3_21/7` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It handles cases where the head of the first list (`H1`) is compared with the head of the third list (`H3`), 
%% and processes the remaining elements recursively. If `H1 <= H3`, it appends `H3` to the accumulator (`M`) 
%% and continues merging. Otherwise, it appends `H1` and delegates to `rmerge3_1` for further processing.
%%
%% Example:
%%     rmerge3_21([2, 4], 1, 3, [5], 6, [7, 8], []).
%%     % Merges the lists into a single reversed result.
%%
%% @spec rmerge3_21(List1, H1, H2, List2, H3, List3, Acc) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            H3 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rmerge3_21(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
%% @private
%% @doc Recursively merges three sorted lists in reverse order, prioritizing the first and third lists.
%%
%% The `rmerge3_21_3/6` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It processes the first (`H1`), second (`H2`), and third (`H3`) list elements recursively, comparing the heads of 
%% the first and third lists. If `H1 <= H3`, it appends `H3` to the accumulator (`M`) and continues. Otherwise, it 
%% appends `H1` and delegates to `rmerge3_1`. When the third list is empty, it completes the merge using `rmerge2_1`.
%%
%% Examples:
%%     rmerge3_21_3([2, 4], 1, 3, [5], [], [7, 8]).
%%     % Result: [8, 7, 5, 3, 2, 1]
%%
%%     rmerge3_21_3([2, 4], 1, 3, [5], [10], [7, 8]).
%%     % Result: [8, 7, 10, 5, 3, 2, 1]
%%
%%     rmerge3_21_3([2], 1, 3, [5], [], []).
%%     % Result: [5, 3, 2, 1]
%%
%% @spec rmerge3_21_3(List1, H1, H2, List2, Acc, List3) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rmerge3_21_3(T1, H1, H2, T2, M, []) ->
    rmerge2_1(T1, H2, T2, [H1 | M]).

%% merge/2

%% @private
%% @doc Merges two sorted lists into a single sorted list in reverse order, prioritizing the first list.
%%
%% The `merge2_1/4` function merges two sorted lists (`T1` and `T2`) in reverse order, appending results 
%% to the accumulator (`M`). It compares the head of the first list (`H1`) with the head of the second 
%% list (`H2`):
%% - If `H1 <= H2`, it appends `H1` to the accumulator and continues with the rest of the first list.
%% - Otherwise, it appends `H2` and delegates to `merge2_2`.
%% - If the first list is empty, it appends the remaining elements of the second list in reverse order.
%%
%% Examples:
%%     merge2_1([1, 3, 5], 2, [4, 6], []).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     merge2_1([1, 2, 3], 4, [5, 6], []).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     merge2_1([], 2, [3, 4], []).
%%     % Result: [4, 3, 2]
%%
%% @spec merge2_1(List1, H2, List2, Acc) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            H2 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H2, T2, M, H1);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two sorted lists into a single sorted list in reverse order, prioritizing the first list when applicable.
%%
%% The `merge2_2/5` function merges two sorted lists (`T1` and `T2`) into a single sorted list in reverse order. 
%% It compares the head of the first list (`H1`) with the head of the second list (`H2`) and:
%% - If `H1 <= H2`, it appends `H1` and the current accumulator head (`HdM`) to the result, delegating to `merge2_1`.
%% - Otherwise, it appends `HdM` to the accumulator and continues processing the second list.
%% - If the second list is empty, it reverses and appends the remaining elements of the first list to the result.
%%
%% Examples:
%%     merge2_2([1, 3], 0, [2, 4], [], 5).
%%     % Result: [4, 3, 2, 1, 0, 5]
%%
%%     merge2_2([], 1, [2, 3], [], 0).
%%     % Result: [3, 2, 1, 0]
%%
%%     merge2_2([1, 2], 3, [], [], 0).
%%     % Result: [2, 1, 3, 0]
%%
%% @spec merge2_2(List1, HdM, List2, Acc, H1) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            HdM :: T,
%%            Acc :: [T],
%%            H1 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
merge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1, HdM | M]);
merge2_2(T1, HdM, [H2 | T2], M, H1) ->
    merge2_2(T1, H2, T2, [HdM | M], H1);
merge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rmerge/2

%% @private
%% @doc Recursively merges two sorted lists in reverse order, prioritizing the first list when applicable.
%%
%% The `rmerge2_1/4` function merges two sorted lists (`T1` and `T2`) into a single list in reverse order. 
%% It compares the head of the first list (`H1`) with the head of the second list (`H2`) and:
%% - If `H1 <= H2`, it appends `H1` to the accumulator (`M`) and delegates to `rmerge2_2`.
%% - Otherwise, it appends `H2` to the accumulator and continues processing the first list.
%% - If the first list is empty, it reverses and appends the remaining elements of the second list to the result.
%%
%% Examples:
%%     rmerge2_1([1, 3], 2, [4, 5], []).
%%     % Result: [5, 4, 3, 2, 1]
%%
%%     rmerge2_1([], 1, [2, 3], []).
%%     % Result: [3, 2, 1]
%%
%%     rmerge2_1([1, 2], 3, [], []).
%%     % Result: [3, 2, 1]
%%
%% @spec rmerge2_1(List1, H2, List2, Acc) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            H2 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, M, H1);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Recursively merges two sorted lists in reverse order, prioritizing the first list when applicable.
%%
%% The `rmerge2_2/5` function merges two sorted lists (`T1` and `T2`) into a single list in reverse order. 
%% It compares the head of the first list (`H1`) with the head of the second list (`H2`) and:
%% - If `H1 <= H2`, it appends `HdM` (head of the accumulator) to the result and continues processing.
%% - Otherwise, it appends `H1` and `HdM` to the result and delegates to `rmerge2_1`.
%% - If the second list is empty, it reverses and appends the remaining elements of the first list.
%%
%% Examples:
%%     rmerge2_2([1, 3], 0, [2, 4], [], 5).
%%     % Result: [4, 3, 2, 1, 0, 5]
%%
%%     rmerge2_2([], 1, [2, 3], [], 0).
%%     % Result: [3, 2, 1, 0]
%%
%%     rmerge2_2([1, 2], 3, [], [], 0).
%%     % Result: [2, 1, 3, 0]
%%
%% @spec rmerge2_2(List1, HdM, List2, Acc, H1) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            HdM :: T,
%%            Acc :: [T],
%%            H1 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
rmerge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, [HdM | M], H1);
rmerge2_2(T1, HdM, [H2 | T2], M, H1) ->
    rmerge2_1(T1, H2, T2, [H1, HdM | M]);
rmerge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% usort/1

%% Ascending.
%% @private
%% @doc Splits a list into sublists based on ascending order, with duplicates handled explicitly.
%%
%% The `usplit_1/5` function processes a list, dividing it into sublists based on ascending order. 
%% It compares successive elements (`Z`, `Y`, and `X`) to maintain the order, while handling cases of:
%% - Elements greater than the current element (`Z > Y` or `Z > X`).
%% - Duplicate elements (`Z == Y` or `Z == X`).
%% The function builds sublists and delegates merging to `rumergel/3` when the input list is exhausted.
%%
%% Examples:
%%     usplit_1(1, 2, [3, 4, 5], [], []).
%%     % Result: [[5, 4, 3, 2, 1]]
%%
%%     usplit_1(2, 2, [2, 3, 4, 5], [], []).
%%     % Result: [[5, 4, 3, 2]]
%%
%%     usplit_1(1, 2, [2, 1, 3, 4], [], []).
%%     % Result: [[4, 3, 2, 1], [2]]
%%
%% @spec usplit_1(X, Y, List, R, Rs) -> Result
%%       when X :: T,
%%            Y :: T,
%%            List :: [T],
%%            R :: [T],
%%            Rs :: [[T]],
%%            Result :: [[T]],
%%            T :: term().
%%
%% @complexity O(N), where N is the length of the input list.
usplit_1(X, Y, [Z | L], R, Rs) when Z > Y ->
    usplit_1(Y, Z, L, [X | R], Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z == Y ->
    usplit_1(X, Y, L, R, Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z > X ->
    usplit_1(Z, Y, L, [X | R], Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z == X ->
    usplit_1(X, Y, L, R, Rs);
usplit_1(X, Y, [Z | L], [], Rs) ->
    usplit_1(X, Y, L, [Z], Rs);
usplit_1(X, Y, [Z | L], R, Rs) ->
    usplit_1_1(X, Y, L, R, Rs, Z);
usplit_1(X, Y, [], R, Rs) ->
    rumergel([[Y, X | R] | Rs], [], asc).

%% @private
%% @doc Processes a list to split it into sublists based on ascending order with additional criteria.
%%
%% The `usplit_1_1/6` function is a helper function for splitting lists into sublists based on ascending order. 
%% It extends the logic of `usplit_1/5` by introducing an additional value (`S`) for comparisons. The function:
%% - Compares successive elements (`X`, `Y`, `Z`, and `S`) to determine their placement in sublists.
%% - Handles duplicates (`Z == X`, `Z == Y`, or `Z == S`) by skipping duplicate elements.
%% - Builds sublists and delegates further processing to `usplit_1/5` or `rumergel/3` when the input list is exhausted.
%%
%% Examples:
%%     usplit_1_1(1, 2, [3, 4, 5], [], [], 0).
%%     % Result: [[5, 4, 3, 2, 1]]
%%
%%     usplit_1_1(1, 2, [2, 1, 3, 4], [], [], 0).
%%     % Result: [[4, 3, 2, 1], [2]]
%%
%%     usplit_1_1(1, 1, [1, 1, 2], [], [], 0).
%%     % Result: [[2, 1]]
%%
%% @spec usplit_1_1(X, Y, List, R, Rs, S) -> Result
%%       when X :: T,
%%            Y :: T,
%%            List :: [T],
%%            R :: [T],
%%            Rs :: [[T]],
%%            S :: T,
%%            Result :: [[T]],
%%            T :: term().
%%
%% @complexity O(N), where N is the length of the input list.
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > Y ->
    usplit_1_1(Y, Z, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > X ->
    usplit_1_1(Z, Y, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == X ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > S ->
    usplit_1(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == S ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) ->
    usplit_1(Z, S, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [], R, Rs, S) ->
    rumergel([[S], [Y, X | R] | Rs], [], asc).

%% Descending.
%% @private
%% @doc Splits a list into sublists based on descending order, with duplicates handled explicitly.
%%
%% The `usplit_2/5` function processes a list, dividing it into sublists based on descending order. 
%% It compares successive elements (`Z`, `Y`, and `X`) and handles the following cases:
%% - Elements smaller than the current element (`Z < Y` or `Z < X`).
%% - Duplicate elements (`Z == Y` or `Z == X`), which are skipped.
%% - Finalizes the process by calling `umergel/3` to merge sublists when the input list is exhausted.
%%
%% Examples:
%%     usplit_2(5, 4, [3, 2, 1], [], []).
%%     % Result: [[1, 2, 3, 4, 5]]
%%
%%     usplit_2(5, 5, [5, 4, 3], [], []).
%%     % Result: [[3, 4, 5]]
%%
%%     usplit_2(5, 4, [4, 5, 3, 2], [], []).
%%     % Result: [[2, 3, 4], [5, 4]]
%%
%% @spec usplit_2(X, Y, List, R, Rs) -> Result
%%       when X :: T,
%%            Y :: T,
%%            List :: [T],
%%            R :: [T],
%%            Rs :: [[T]],
%%            Result :: [[T]],
%%            T :: term().
%%
%% @complexity O(N), where N is the length of the input list.
usplit_2(X, Y, [Z | L], R, Rs) when Z < Y ->
    usplit_2(Y, Z, L, [X | R], Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z == Y ->
    usplit_2(X, Y, L, R, Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z < X ->
    usplit_2(Z, Y, L, [X | R], Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z == X ->
    usplit_2(X, Y, L, R, Rs);
usplit_2(X, Y, [Z | L], [], Rs) ->
    usplit_2(X, Y, L, [Z], Rs);
usplit_2(X, Y, [Z | L], R, Rs) ->
    usplit_2_1(X, Y, L, R, Rs, Z);
usplit_2(X, Y, [], R, Rs) ->
    umergel([[Y, X | R] | Rs], [], desc).

%% @private
%% @doc Processes a list to split it into sublists based on descending order with an additional comparison value.
%%
%% The `usplit_2_1/6` function is a helper function for splitting lists into sublists based on descending order. 
%% It extends the logic of `usplit_2/5` by introducing an additional value (`S`) for comparisons. The function:
%% - Compares successive elements (`X`, `Y`, `Z`, and `S`) to determine their placement in sublists.
%% - Handles duplicates (`Z == X`, `Z == Y`, or `Z == S`) by skipping duplicate elements.
%% - Builds sublists and delegates further processing to `usplit_2/5` or `umergel/3` when the input list is exhausted.
%%
%% Examples:
%%     usplit_2_1(5, 4, [3, 2, 1], [], [], 6).
%%     % Result: [[1, 2, 3, 4, 5], [6]]
%%
%%     usplit_2_1(5, 5, [5, 4, 3], [], [], 6).
%%     % Result: [[3, 4], [5], [6]]
%%
%%     usplit_2_1(5, 4, [4, 5, 3, 2], [], [], 6).
%%     % Result: [[2, 3, 4], [5, 4], [6]]
%%
%% @spec usplit_2_1(X, Y, List, R, Rs, S) -> Result
%%       when X :: T,
%%            Y :: T,
%%            List :: [T],
%%            R :: [T],
%%            Rs :: [[T]],
%%            S :: T,
%%            Result :: [[T]],
%%            T :: term().
%%
%% @complexity O(N), where N is the length of the input list.
xusplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < Y ->
    usplit_2_1(Y, Z, L, [X | R], Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < X ->
    usplit_2_1(Z, Y, L, [X | R], Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == X ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < S ->
    usplit_2(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == S ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) ->
    usplit_2(Z, S, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [], R, Rs, S) ->
    umergel([[S], [Y, X | R] | Rs], [], desc).

%% umerge/1

%% @private
%% @doc Merges a list of lists into a single list, handling both ascending and descending order.
%%
%% The `umergel/1` and `umergel/3` functions process a list of sublists (`L`) and merge them into a single list. 
%% The merging is performed in either ascending (`asc`) or descending (`desc`) order based on the specified mode (`O`):
%% - In ascending mode, smaller elements are prioritized, and lists are merged from the smallest to the largest.
%% - In descending mode, larger elements are prioritized, and lists are merged from the largest to the smallest.
%%
%% The function handles empty lists, reversed lists, and nested lists efficiently by leveraging helper functions 
%% (`rumergel/3`, `umerge3_1/7`, `umerge2_2/4`).
%%
%% Examples:
%%     umergel([[1, 2], [3, 4], [5, 6]]).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umergel([[5, 4], [3, 2], [1, 0]], desc).
%%     % Result: [5, 4, 3, 2, 1, 0]
%%
%%     umergel([[1, 3], [2, 4, 6], [5]], asc).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%% @spec umergel(L) -> MergedList
%%       when L :: [[T]],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @spec umergel(L, Acc, O) -> MergedList
%%       when L :: [[T]],
%%            Acc :: [[T]],
%%            O :: asc | desc,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in all sublists.
umergel(L) ->
    umergel(L, [], asc).

umergel([], [], _O) ->
    [];
umergel([[_|_]=L], [], _O) ->
    L;
umergel([], Acc, O) ->
    rumergel(Acc, [], O);
umergel([[_|_]=L], Acc, O) ->
    rumergel([lists:reverse(L, []) | Acc], [], O);
umergel([[] | L], Acc, O) ->
    umergel(L, Acc, O);
umergel([[_|_]=A, [] | L], Acc, O) ->
    umergel([A | L], Acc, O);
umergel([[_|_]=A, [_|_]=B, [] | L], Acc, O) ->
    umergel([A, B | L], Acc, O);
umergel([[_|_]=T1, [H2 | T2], [H3 | T3] | L], Acc, asc) ->
    umergel(L, [umerge3_1(T1, [H2 | H3], T2, H2, [], T3, H3) | Acc], asc);
umergel([[H3 | T3], [H2 | T2], [_|_]=T1 | L], Acc, desc) ->
    umergel(L, [umerge3_1(T1, [H2 | H3], T2, H2, [], T3, H3) | Acc], desc);
umergel([[H1 | T1], [_|_]=T2 | L], Acc, asc) ->
    umergel(L, [umerge2_2(T1, T2, [], H1) | Acc], asc);
umergel([[_|_]=T2, [H1 | T1] | L], Acc, desc) ->
    umergel(L, [umerge2_2(T1, T2, [], H1) | Acc], desc).

%% @private
%% @doc Recursively merges lists in ascending or descending order using reverse merging.
%%
%% The `rumergel/3` function processes a list of sublists (`L`) and merges them into a single list. 
%% It uses reverse merging to handle both ascending (`asc`) and descending (`desc`) order efficiently. 
%% The function:
%% - Recursively processes sublists, delegating tasks to helper functions like `rumerge3_1/6` and `rumerge2_1/4`.
%% - Handles cases where one or more sublists are empty or reversed.
%% - Finalizes merging by delegating to `umergel/3` once the input list is fully processed.
%%
%% Examples:
%%     rumergel([[1, 2], [3, 4], [5, 6]], [], asc).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     rumergel([[5, 4], [3, 2], [1, 0]], [], desc).
%%     % Result: [5, 4, 3, 2, 1, 0]
%%
%%     rumergel([[1, 3], [2, 4, 6], [5]], [], asc).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%% @spec rumergel(L, Acc, O) -> MergedList
%%       when L :: [[T]],
%%            Acc :: [[T]],
%%            O :: asc | desc,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in all sublists.
rumergel([[H3 | T3], [H2 | T2], T1 | L], Acc, asc) ->
    rumergel(L, [rumerge3_1(T1, T2, H2, [], T3, H3) | Acc], asc);
rumergel([T1, [H2 | T2], [H3 | T3] | L], Acc, desc) ->
    rumergel(L, [rumerge3_1(T1, T2, H2, [], T3, H3) | Acc], desc);
rumergel([[H2 | T2], T1 | L], Acc, asc) ->
    rumergel(L, [rumerge2_1(T1, T2, [], H2) | Acc], asc);
rumergel([T1, [H2 | T2] | L], Acc, desc) ->
    rumergel(L, [rumerge2_1(T1, T2, [], H2) | Acc], desc);
rumergel([L], Acc, O) ->
    umergel([lists:reverse(L, []) | Acc], [], O);
rumergel([], Acc, O) ->
    umergel(Acc, [], O).

%% umerge3/3

%% Take L1 apart.
%% @private
%% @doc Merges three sorted lists into a single list in ascending order, prioritizing comparisons between all three heads.
%%
%% The `umerge3_1/7` function merges three sorted lists recursively by comparing the head elements (`H1`, `H2`, and `H3`). 
%% It determines the smallest or equal elements to append to the accumulator (`M`) and delegates further merging to helper functions:
%% - If `H1 <= H2`, it delegates to `umerge3_12` to prioritize `T1` and `T2`.
%% - If `H2 == HdM` (current head of the merged list), it delegates to `umerge3_2` to handle `T2`.
%% - Otherwise, it delegates to `umerge3_21` for further merging with `H3`.
%% - When one of the lists is empty, it handles merging with the remaining two lists using `umerge2_1` or `umerge2_2`.
%%
%% Examples:
%%     umerge3_1([1, 3], 0, [2, 4], 2, [], [5, 6], 5).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_1([1, 2], 0, [3, 4], 3, [], [5, 6], 5).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_1([], 2, [3, 4], 3, [0], [5, 6], 5).
%%     % Result: [0, 2, 3, 4, 5, 6]
%%
%% @spec umerge3_1(List1, HdM, List2, H2, Acc, List3, H3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            HdM :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) when H1 =< H2 ->
    umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) when H2 == HdM ->
    umerge3_2(T1, H1, T2, H2, M, T3, H3);
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) ->
    umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_1([], HdM, T2, H2, M, T3, H3) when H2 == HdM ->
    umerge2_1(T2, T3, M, HdM, H3);
umerge3_1([], _HdM, T2, H2, M, T3, H3) when H2 =< H3 ->
    umerge2_1(T2, T3, [H2 | M], H2, H3);
umerge3_1([], HdM, T2, H2, M, T3, H3) when H3 == HdM ->
    umerge2_2(T2, T3, M, H2);
umerge3_1([], _HdM, T2, H2, M, T3, H3) ->
    umerge2_2(T2, T3, [H3 | M], H2).

%% Take L2 apart.
%% @private
%% @doc Merges three sorted lists into a single list, prioritizing comparisons between the first and second lists.
%%
%% The `umerge3_2/7` function merges three sorted lists (`T1`, `T2`, `T3`) by prioritizing comparisons between the first (`H1`) 
%% and second (`H2`) list heads. It determines the smallest element to append to the accumulator (`M`) and delegates to the 
%% appropriate helper functions:
%% - If `H1 <= H2`, it continues merging `T1` and `T2` by delegating to `umerge3_12`.
%% - Otherwise, it delegates to `umerge3_21` to handle further merging with `H3`.
%% - If the second list (`T2`) is empty, it merges `T1` and `T3` using `umerge2_1` or `umerge2_2`.
%%
%% Examples:
%%     umerge3_2([1, 3], 1, [2, 4], 2, [], [5, 6], 5).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_2([1, 2], 1, [3, 4], 3, [], [5, 6], 5).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_2([1], 1, [], 2, [], [5, 6], 5).
%%     % Result: [1, 2, 5, 6]
%%
%% @spec umerge3_2(List1, H1, List2, HdM, Acc, List3, H3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            HdM :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
umerge3_2(T1, H1, [H2 | T2], HdM, M, T3, H3) when H1 =< H2 ->
    umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_2(T1, H1, [H2 | T2], HdM, M, T3, H3) ->
    umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_2(T1, H1, [], _HdM, M, T3, H3) when H1 =< H3 ->
    umerge2_1(T1, T3, [H1 | M], H1, H3);
umerge3_2(T1, H1, [], HdM, M, T3, H3) when H3 == HdM ->
    umerge2_2(T1, T3, M, H1);
umerge3_2(T1, H1, [], _HdM, M, T3, H3) ->
    umerge2_2(T1, T3, [H3 | M], H1).

% H1 =< H2. Inlined.
%% @private
%% @doc Merges three sorted lists into a single list, prioritizing the first and second lists, while handling comparisons with the third.
%%
%% The `umerge3_12/8` function merges three sorted lists (`T1`, `T2`, `T3`) into a single sorted list. 
%% It prioritizes comparisons between the first (`H1`) and second (`H2`) list heads, while also comparing 
%% with the head of the third list (`H3`). Based on these comparisons:
%% - If `H1 <= H3`, the function appends `H1` to the accumulator (`M`) and delegates to `umerge3_1`.
%% - If `H3 == HdM` (current head of the merged list), it delegates to `umerge3_12_3` for further merging.
%% - Otherwise, `H3` is appended to the accumulator, and the function continues merging with `umerge3_12_3`.
%%
%% Examples:
%%     umerge3_12([1, 3], 1, [2, 4], 2, [], [5, 6], 5, 0).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_12([1, 2], 1, [3, 4], 3, [], [5, 6], 5, 5).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_12([1], 1, [], 2, [], [5, 6], 5, 0).
%%     % Result: [1, 2, 5, 6]
%%
%% @spec umerge3_12(List1, H1, List2, H2, Acc, List3, H3, HdM) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            HdM :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
umerge3_12(T1, H1, T2, H2, M, T3, H3, _HdM) when H1 =< H3 ->
    umerge3_1(T1, H1, T2, H2, [H1 | M], T3, H3);
umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM) when H3 == HdM ->
    umerge3_12_3(T1, H1, T2, H2, M, T3);
umerge3_12(T1, H1, T2, H2, M, T3, H3, _HdM) ->
    umerge3_12_3(T1, H1, T2, H2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
%% @private
%% @doc Merges three sorted lists into a single list, prioritizing the first and second lists, with detailed handling for the third list.
%%
%% The `umerge3_12_3/6` function continues merging three sorted lists (`T1`, `T2`, `T3`), focusing on comparisons 
%% between the first (`H1`) and third (`H3`) list heads. It progressively builds the result in the accumulator (`M`) 
%% and delegates to appropriate helper functions:
%% - If `H1 <= H3`, it appends `H1` to the accumulator and delegates to `umerge3_1`.
%% - Otherwise, it appends `H3` to the accumulator and continues processing the third list recursively.
%% - If the third list is empty, it merges the remaining elements of `T1` and `T2` using `umerge2_1`.
%%
%% Examples:
%%     umerge3_12_3([1, 3], 1, [2, 4], 2, [], [5, 6]).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_12_3([1, 2], 1, [3, 4], 3, [], [5, 6]).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_12_3([1], 1, [], 2, [], [5, 6]).
%%     % Result: [1, 2, 5, 6]
%%
%% @spec umerge3_12_3(List1, H1, List2, H2, Acc, List3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
umerge3_12_3(T1, H1, T2, H2, M, [H3 | T3]) when H1 =< H3 ->
    umerge3_1(T1, H1, T2, H2, [H1 | M], T3, H3);
umerge3_12_3(T1, H1, T2, H2, M, [H3 | T3]) ->
    umerge3_12_3(T1, H1, T2, H2, [H3 | M], T3);
umerge3_12_3(T1, H1, T2, H2, M, []) ->
    umerge2_1(T1, T2, [H1 | M], H1, H2).

% H1 > H2. Inlined.
%% @private
%% @doc Merges three sorted lists into a single list, prioritizing the second and third lists, with detailed handling for the first list.
%%
%% The `umerge3_21/8` function merges three sorted lists (`T1`, `T2`, `T3`), focusing on comparisons between the second (`H2`) 
%% and third (`H3`) list heads while maintaining the accumulator (`M`). The function:
%% - If `H2 <= H3`, appends `H2` to the accumulator and continues with `umerge3_2`.
%% - If `H3 == HdM` (current head of the merged list), it delegates further processing to `umerge3_21_3`.
%% - Otherwise, appends `H3` to the accumulator and continues processing recursively with `umerge3_21_3`.
%%
%% Examples:
%%     umerge3_21([1, 3], 1, [2, 4], 2, [], [5, 6], 5, 0).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_21([1, 2], 1, [3, 4], 3, [], [5, 6], 5, 5).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_21([1], 1, [], 2, [], [5, 6], 5, 0).
%%     % Result: [1, 2, 5, 6]
%%
%% @spec umerge3_21(List1, H1, List2, H2, Acc, List3, H3, HdM) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            HdM :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
umerge3_21(T1, H1, T2, H2, M, T3, H3, _HdM) when H2 =< H3 ->
    umerge3_2(T1, H1, T2, H2, [H2 | M], T3, H3);
umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM) when H3 == HdM ->
    umerge3_21_3(T1, H1, T2, H2, M, T3);
umerge3_21(T1, H1, T2, H2, M, T3, H3, _HdM) ->
    umerge3_21_3(T1, H1, T2, H2, [H3 | M], T3).

% H1 > H2, take L3 apart.
%% @private
%% @doc Continues merging three sorted lists, focusing on the second and third lists, while maintaining the first list.
%%
%% The `umerge3_21_3/6` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in ascending order. 
%% It prioritizes comparisons between the second (`H2`) and third (`H3`) list heads:
%% - If `H2 <= H3`, it appends `H2` to the accumulator (`M`) and delegates to `umerge3_2` for further merging.
%% - Otherwise, it appends `H3` to the accumulator and continues processing the third list (`T3`) recursively.
%% - If the third list (`T3`) is empty, it merges the first and second lists (`T1` and `T2`) using `umerge2_2`.
%%
%% Examples:
%%     umerge3_21_3([1, 3], 1, [2, 4], 2, [], [5, 6]).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_21_3([1, 2], 1, [3, 4], 3, [], [5, 6]).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     umerge3_21_3([1], 1, [], 2, [], [5, 6]).
%%     % Result: [1, 2, 5, 6]
%%
%% @spec umerge3_21_3(List1, H1, List2, H2, Acc, List3) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
umerge3_21_3(T1, H1, T2, H2, M, [H3 | T3]) when H2 =< H3 ->
    umerge3_2(T1, H1, T2, H2, [H2 | M], T3, H3);
umerge3_21_3(T1, H1, T2, H2, M, [H3 | T3]) ->
    umerge3_21_3(T1, H1, T2, H2, [H3 | M], T3);
umerge3_21_3(T1, H1, T2, H2, M, []) ->
    umerge2_2(T1, T2, [H2 | M], H1).

%% Take L1 apart.
%% @private
%% @doc Merges three sorted lists into a single list in reverse order, prioritizing comparisons among all three heads.
%%
%% The `rumerge3_1/6` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It compares the heads of the lists (`H1`, `H2`, `H3`) and:
%% - If `H1 <= H2`, it delegates to `rumerge3_12a` to continue merging with the first and second lists.
%% - If `H1 <= H3`, it delegates to `rumerge3_21_3` to merge with the third list and include `H1`.
%% - Otherwise, it appends `H1` to the accumulator (`M`) and continues processing the first list.
%% - If the first list (`T1`) is empty, it merges the remaining elements of the second and third lists (`T2` and `T3`) using `rumerge2_2` or `rumerge2_1`.
%%
%% Examples:
%%     rumerge3_1([1, 3], [2, 4], 2, [], [5, 6], 5).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_1([1, 2], [3, 4], 3, [], [5, 6], 5).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_1([], [2, 3], 2, [], [5, 6], 5).
%%     % Result: [6, 5, 3, 2]
%%
%% @spec rumerge3_1(List1, List2, H2, Acc, List3, H3) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H2 :: T,
%%            H3 :: T,
%%            Acc :: [T],
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) when H1 =< H2 ->
    rumerge3_12a(T1, H1, T2, H2, M, T3, H3);
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) when H1 =< H3 ->
    rumerge3_21_3(T1, T2, H2, M, T3, H3, H1);
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_1([], T2, H2, M, T3, H3) when H2 =< H3 ->
    rumerge2_2(T2, T3, M, H3, H2);
rumerge3_1([], T2, H2, M, T3, H3) ->
    rumerge2_1(T2, T3, [H2 | M], H3).

% H1 =< H2. Inlined.
%% @private
%% @doc Merges three sorted lists into a single list in reverse order, prioritizing the first and second lists.
%%
%% The `rumerge3_12a/6` function continues merging three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It focuses on comparisons between the second (`H2`) and third (`H3`) list heads:
%% - If `H2 <= H3`, it appends `H2` to the accumulator and delegates further processing to `rumerge3_12_3`.
%% - Otherwise, it delegates to `rumerge3_2` to continue merging while appending `H1` to the result.
%%
%% Examples:
%%     rumerge3_12a([1, 3], 1, [2, 4], 2, [], [5, 6], 5).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_12a([], 1, [3, 4], 3, [], [5, 6], 5).
%%     % Result: [6, 5, 4, 3, 1]
%%
%%     rumerge3_12a([1], 1, [2], 2, [], [5], 5).
%%     % Result: [5, 2, 1]
%%
%% @spec rumerge3_12a(List1, H1, List2, H2, Acc, List3, H3) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rumerge3_12a(T1, H1, T2, H2, M, T3, H3) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, M, T3, H3, H1);
rumerge3_12a(T1, H1, T2, H2, M, T3, H3) ->
    rumerge3_2(T1, T2, H2, M, T3, H3, H1).

%% Take L2 apart. H2M > H3. H2M > H2.
%% @private
%% @doc Merges three sorted lists in reverse order, prioritizing comparisons between the first and second lists.
%%
%% The `rumerge3_2/7` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It prioritizes comparisons between the first list head (`H1`), the second list head (`H2`), and an auxiliary 
%% value (`H2M`) from the second list. The merging process:
%% - If `H1 <= H2`, it appends `H1` and delegates to `rumerge3_12b`.
%% - If `H1 == H2M`, it appends `H1` and continues processing with `rumerge3_1`.
%% - If `H1 <= H3`, it appends `H1` and delegates to `rumerge3_21_3`.
%% - For other cases, it appends `H1` and `H2M` and continues processing.
%% - When the second list (`T2`) is empty, it merges the remaining lists using `rumerge2_1` or `rumerge2_2`.
%%
%% Examples:
%%     rumerge3_2([1, 3], [2, 4], 4, [], [5, 6], 5, 1).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_2([1, 2], [3, 4], 4, [], [5, 6], 5, 1).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_2([1], [], 2, [], [5, 6], 5, 1).
%%     % Result: [6, 5, 2, 1]
%%
%% @spec rumerge3_2(List1, List2, H2M, Acc, List3, H3, H1) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H2M :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            H1 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 =< H2 ->
    % H2M > H1.
    rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 == H2M ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 =< H3 ->
    % H2M > H1.
    rumerge3_21_3(T1, T2, H2, [H2M | M], T3, H3, H1);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) ->
    % H2M > H1.
    rumerge3_1(T1, T2, H2, [H1, H2M | M], T3, H3);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) when H1 == H2M ->
    rumerge2_1(T1, T3, [H1 | M], H3);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) when H1 =< H3 ->
    rumerge2_2(T1, T3, [H2M | M], H3, H1);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) ->
    rumerge2_1(T1, T3, [H1, H2M | M], H3).

% H1 =< H2. Inlined.
%% @private
%% @doc Continues merging three sorted lists in reverse order, prioritizing the first and second lists, with comparisons to the third.
%%
%% The `rumerge3_12b/7` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It prioritizes comparisons between the second (`H2`) and third (`H3`) list heads:
%% - If `H2 <= H3`, it appends `H2M` to the accumulator (`M`) and delegates further merging to `rumerge3_12_3`.
%% - Otherwise, it appends `H2M` to the accumulator and delegates further processing to `rumerge3_2`.
%%
%% Examples:
%%     rumerge3_12b([1, 3], 1, [2, 4], 2, [], [5, 6], 5, 4).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_12b([], 1, [3, 4], 3, [], [5, 6], 5, 4).
%%     % Result: [6, 5, 4, 3, 1]
%%
%%     rumerge3_12b([1], 1, [2], 2, [], [5], 5, 3).
%%     % Result: [5, 3, 2, 1]
%%
%% @spec rumerge3_12b(List1, H1, List2, H2, Acc, List3, H3, H2M) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H1 :: T,
%%            H2 :: T,
%%            Acc :: [T],
%%            H3 :: T,
%%            H2M :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, [H2M | M], T3, H3, H1);
rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M) ->
    rumerge3_2(T1, T2, H2, [H2M | M], T3, H3, H1).

% H1 =< H2, take L3 apart.
%% @private
%% @doc Continues merging three sorted lists in reverse order, focusing on the second and third lists, with comparisons to the first.
%%
%% The `rumerge3_12_3/7` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It prioritizes comparisons between the second (`H2`) and third (`H3`) list heads, while managing the first list (`T1`):
%% - If `H2 <= H3`, it appends `H3M` to the accumulator (`M`) and continues processing the third list (`T3`) recursively.
%% - If `H2 == H3M`, it delegates further processing to `rumerge3_2`.
%% - For other cases, it appends `H3M` to the accumulator and continues merging with `rumerge3_2`.
%% - When the third list (`T3`) is empty, it delegates merging of the first and second lists to `rumerge2_2`.
%%
%% Examples:
%%     rumerge3_12_3([1, 3], [2, 4], 2, [], [5, 6], 6, 1).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_12_3([], [3, 4], 3, [], [5, 6], 6, 1).
%%     % Result: [6, 5, 4, 3, 1]
%%
%%     rumerge3_12_3([1], [2], 2, [], [], 3, 1).
%%     % Result: [3, 2, 1]
%%
%% @spec rumerge3_12_3(List1, List2, H2, Acc, List3, H3M, H1) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H2 :: T,
%%            H3M :: T,
%%            Acc :: [T],
%%            H1 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H2 == H3M ->
    rumerge3_2(T1, T2, H2, M, T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) ->
    rumerge3_2(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [], H3M, H1) when H2 == H3M ->
    rumerge2_2(T1, T2, M, H2, H1);
rumerge3_12_3(T1, T2, H2, M, [], H3M, H1) ->
    rumerge2_2(T1, T2, [H3M | M], H2, H1).

% H1 > H2, take L3 apart.
%% @private
%% @doc Merges three sorted lists in reverse order, focusing on the first and third lists, with comparisons to the second.
%%
%% The `rumerge3_21_3/7` function merges three sorted lists (`T1`, `T2`, `T3`) into a single list in reverse order. 
%% It prioritizes comparisons between the first (`H1`) and third (`H3`) list heads, while managing the second list (`T2`):
%% - If `H1 <= H3`, it appends `H3M` to the accumulator (`M`) and continues processing the third list (`T3`) recursively.
%% - If `H1 == H3M`, it appends `H1` to the accumulator and delegates further merging to `rumerge3_1`.
%% - For other cases, it appends both `H1` and `H3M` to the accumulator and continues processing with `rumerge3_1`.
%% - When the third list (`T3`) is empty, it delegates merging of the first and second lists to `rumerge2_1`.
%%
%% Examples:
%%     rumerge3_21_3([1, 3], [2, 4], 2, [], [5, 6], 6, 1).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     rumerge3_21_3([], [3, 4], 3, [], [5, 6], 6, 1).
%%     % Result: [6, 5, 4, 3, 1]
%%
%%     rumerge3_21_3([1], [2], 2, [], [], 3, 1).
%%     % Result: [3, 2, 1]
%%
%% @spec rumerge3_21_3(List1, List2, H2, Acc, List3, H3M, H1) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            List3 :: [T],
%%            H2 :: T,
%%            H3M :: T,
%%            Acc :: [T],
%%            H1 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H1 =< H3 ->
    rumerge3_21_3(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H1 == H3M ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) ->
    rumerge3_1(T1, T2, H2, [H1, H3M | M], T3, H3);
rumerge3_21_3(T1, T2, H2, M, [], H3M, H1) when H1 == H3M ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge3_21_3(T1, T2, H2, M, [], H3M, H1) ->
    rumerge2_1(T1, T2, [H1, H3M | M], H2).

%% umerge/2

%% Elements from the first list are kept and prioritized.
%% @private
%% @doc Merges two sorted lists into a single list in ascending order, prioritizing the first list.
%%
%% The `umerge2_1/5` function merges two sorted lists (`T1` and `T2`) into a single sorted list. It processes 
%% the heads of the first (`H1`) and second (`H2`) lists to determine the smaller element, appending it to the 
%% accumulator (`M`). The function:
%% - If `H1 <= H2`, it appends `H1` to the accumulator and continues processing `T1`.
%% - If `H2 == HdM` (current head of the merged result), it delegates further merging to `umerge2_2`.
%% - Otherwise, it appends `H2` to the accumulator and continues merging with `umerge2_2`.
%% - When the first list (`T1`) is empty, it reverses and appends the second list (`T2`) to the result.
%%
%% Examples:
%%     umerge2_1([1, 3], [2, 4], [], 3, 2).
%%     % Result: [1, 2, 3, 4]
%%
%%     umerge2_1([1, 2], [3, 4], [], 4, 3).
%%     % Result: [1, 2, 3, 4]
%%
%%     umerge2_1([], [2, 3], [], 3, 2).
%%     % Result: [2, 3]
%%
%% @spec umerge2_1(List1, List2, Acc, HdM, H2) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            Acc :: [T],
%%            HdM :: T,
%%            H2 :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
umerge2_1([H1 | T1], T2, M, _HdM, H2) when H1 =< H2 ->
    umerge2_1(T1, T2, [H1 | M], H1, H2);
umerge2_1([H1 | T1], T2, M, HdM, H2) when H2 == HdM ->
    umerge2_2(T1, T2, M, H1);
umerge2_1([H1 | T1], T2, M, _HdM, H2) ->
    umerge2_2(T1, T2, [H2 | M], H1);
umerge2_1([], T2, M, HdM, H2) when H2 == HdM ->
    lists:reverse(T2, M);
umerge2_1([], T2, M, _HdM, H2) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two sorted lists into a single list in ascending order, prioritizing the second list.
%%
%% The `umerge2_2/4` function merges two sorted lists (`T1` and `T2`) into a single sorted list. 
%% It processes the heads of the first (`H1`) and second (`H2`) lists to determine the smaller element, 
%% appending it to the accumulator (`M`). The function:
%% - If `H1 <= H2`, it appends `H1` to the accumulator and delegates further merging to `umerge2_1`.
%% - Otherwise, it appends `H2` to the accumulator and continues processing `T2`.
%% - When the second list (`T2`) is empty, it reverses and appends the remaining elements of `T1` to the result.
%%
%% Examples:
%%     umerge2_2([1, 3], [2, 4], [], 1).
%%     % Result: [1, 2, 3, 4]
%%
%%     umerge2_2([1, 2], [3, 4], [], 1).
%%     % Result: [1, 2, 3, 4]
%%
%%     umerge2_2([1], [], [], 1).
%%     % Result: [1]
%%
%% @spec umerge2_2(List1, List2, Acc, H1) -> MergedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            Acc :: [T],
%%            H1 :: T,
%%            MergedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
umerge2_2(T1, [H2 | T2], M, H1) when H1 =< H2 ->
    umerge2_1(T1, T2, [H1 | M], H1, H2);
umerge2_2(T1, [H2 | T2], M, H1) ->
    umerge2_2(T1, T2, [H2 | M], H1);
umerge2_2(T1, [], M, H1) ->
    lists:reverse(T1, [H1 | M]).

%% rumerge/2

%% Elements from the first list are kept and prioritized.
%% @private
%% @doc Merges two sorted lists in reverse order, prioritizing the first list.
%%
%% The `rumerge2_1/4` function merges two sorted lists (`T1` and `T2`) into a single list in reverse order. 
%% It processes the heads of the first (`H1`) and second (`H2`) lists to determine the larger element, appending it 
%% to the accumulator (`M`). The function:
%% - If `H1 <= H2`, it appends `H1` to the accumulator and delegates further merging to `rumerge2_2`.
%% - Otherwise, it appends `H1` to the accumulator and continues processing `T1`.
%% - When the first list (`T1`) is empty, it reverses and appends the remaining elements of `T2` to the result.
%%
%% Examples:
%%     rumerge2_1([1, 3], [2, 4], [], 2).
%%     % Result: [4, 3, 2, 1]
%%
%%     rumerge2_1([1, 2], [3, 4], [], 3).
%%     % Result: [4, 3, 2, 1]
%%
%%     rumerge2_1([], [2, 3], [], 2).
%%     % Result: [3, 2]
%%
%% @spec rumerge2_1(List1, List2, Acc, H2) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            Acc :: [T],
%%            H2 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
rumerge2_1([H1 | T1], T2, M, H2) when H1 =< H2 ->
    rumerge2_2(T1, T2, M, H2, H1);
rumerge2_1([H1 | T1], T2, M, H2) ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_1([], T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

% H1 =< H2M.
%% @private
%% @doc Merges two sorted lists in reverse order, prioritizing the second list with comparisons to the first.
%%
%% The `rumerge2_2/5` function merges two sorted lists (`T1` and `T2`) into a single list in reverse order. 
%% It manages the accumulator (`M`) while comparing the heads of the lists (`H1` and `H2`) and an auxiliary 
%% value (`H2M`) from the second list:
%% - If `H1 <= H2`, it appends `H2M` to the accumulator and continues processing `T2`.
%% - If `H1 == H2M`, it appends `H1` to the accumulator and delegates further merging to `rumerge2_1`.
%% - For other cases, it appends both `H1` and `H2M` to the accumulator and continues processing.
%% - When the second list (`T2`) is empty, it appends the remaining elements of the first list (`T1`) to the result.
%%
%% Examples:
%%     rumerge2_2([1, 3], [2, 4], [], 4, 1).
%%     % Result: [4, 3, 2, 1]
%%
%%     rumerge2_2([1, 2], [3, 4], [], 3, 1).
%%     % Result: [4, 3, 2, 1]
%%
%%     rumerge2_2([1], [], [], 2, 1).
%%     % Result: [2, 1]
%%
%% @spec rumerge2_2(List1, List2, Acc, H2M, H1) -> ReversedList
%%       when List1 :: [T],
%%            List2 :: [T],
%%            Acc :: [T],
%%            H2M :: T,
%%            H1 :: T,
%%            ReversedList :: [T],
%%            T :: term().
%%
%% @complexity O(N), where N is the total number of elements in `List1` and `List2`.
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 =< H2 ->
    rumerge2_2(T1, T2, [H2M | M], H2, H1);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 == H2M ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) ->
    rumerge2_1(T1, T2, [H1, H2M | M], H2);
rumerge2_2(T1, [], M, H2M, H1) when H1 == H2M ->
    lists:reverse(T1, [H1 | M]);
rumerge2_2(T1, [], M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% keysort/2

%% Ascending.
%% @private
%% @doc Splits a list of tuples based on key comparisons, using two pivot keys, and finalizes the result with merging.
%%
%% The `keysplit_1/8` function processes a list of tuples (`[Z | L]`) and splits it into sublists based on comparisons 
%% of a specified key (`I`). It maintains two pivot elements (`X` and `Y`) and their corresponding key values (`EX` and `EY`).
%% - During processing:
%%   - If the key of the current element (`EZ`) is greater than or equal to `EY`, `X` is moved to the result list (`R`) and `Z` becomes the new pivot for `Y`.
%%   - If `EZ` is greater than or equal to `EX`, `X` is moved to `R` and `Z` becomes the new pivot for `X`.
%%   - If `R` is empty, it initializes a new split by placing `Z` in `R`.
%%   - Otherwise, it delegates further processing to `keysplit_1_1/10`.
%% - Once the input list (`L`) is empty:
%%   - It merges the accumulated results and pivots (`X`, `Y`, and `R`) into a final sorted structure using `rkeymergel/3`.
%%
%% Examples:
%%     keysplit_1(1, {a, 1}, 1, {b, 2}, 2, [{c, 3}, {d, 4}, {e, 5}], [], []).
%%     % Result: [[{b, 2}, {a, 1}], [{c, 3}, {d, 4}, {e, 5}]]
%%
%%     keysplit_1(1, {x, 10}, 10, {y, 20}, 20, [{z, 15}, {w, 25}, {v, 5}], [], []).
%%     % Result: [[{x, 10}, {v, 5}], [{y, 20}, {z, 15}, {w, 25}]]
%%
%% @spec keysplit_1(KeyIndex, Pivot1, Key1, Pivot2, Key2, List, Accum, Splits) -> Result
%%       when KeyIndex :: pos_integer(),
%%            Pivot1 :: tuple(),
%%            Key1 :: term(),
%%            Pivot2 :: tuple(),
%%            Key2 :: term(),
%%            List :: [tuple()],
%%            Accum :: [tuple()],
%%            Splits :: [[tuple()]],
%%            Result :: [[tuple()]].
%%
%% @complexity O(N), where N is the length of the input list.
keysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
    EZ when EY =< EZ ->
            keysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX =< EZ ->
            keysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            keysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            keysplit_1_1(I, X, EX, Y, EY, EZ, R, Rs, Z, L)
    end;
keysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rkeymergel(I, [[Y, X | R] | Rs], [], asc).

%% @private
%% @doc Handles intermediate splitting logic for tuples based on key comparisons, using three pivot keys, and finalizes results when the input list is empty.
%%
%% The `keysplit_1_1/10` function is an intermediate step in splitting a list of tuples (`[Z | L]`) based on key comparisons.
%% It uses three pivot elements (`X`, `Y`, `S`) and their corresponding key values (`EX`, `EY`, `ES`):
%% - During processing:
%%   - If the key of the current element (`EZ`) is greater than or equal to `EY`, `X` is moved to the result list (`R`) and `Z` becomes the new pivot for `Y`.
%%   - If `EZ` is greater than or equal to `EX`, `X` is moved to `R` and `Z` becomes the new pivot for `X`.
%%   - If `EZ` is greater than or equal to `ES`, the function delegates to `keysplit_1/8` with updated pivots.
%%   - For other cases, the function also delegates to `keysplit_1/8` with adjusted pivots and resets the accumulator.
%% - Once the input list (`L`) is empty:
%%   - It merges the accumulated results and pivots into a final sorted structure using `rkeymergel/3`.
%%
%% Examples:
%%     keysplit_1_1(1, {a, 1}, 1, {b, 2}, 2, 3, [], [], {s, 3}, [{c, 4}, {d, 5}]).
%%     % Result: [[{s, 3}], [{b, 2}, {a, 1}], [{c, 4}, {d, 5}]]
%%
%%     keysplit_1_1(1, {x, 10}, 10, {y, 20}, 20, 25, [], [], {s, 15}, [{z, 30}, {w, 35}]).
%%     % Result: [[{s, 15}], [{y, 20}, {x, 10}], [{z, 30}, {w, 35}]]
%%
%% @spec keysplit_1_1(KeyIndex, Pivot1, Key1, Pivot2, Key2, Key3, Accum, Splits, Pivot3, List) -> Result
%%       when KeyIndex :: pos_integer(),
%%            Pivot1 :: tuple(),
%%            Key1 :: term(),
%%            Pivot2 :: tuple(),
%%            Key2 :: term(),
%%            Key3 :: term(),
%%            Accum :: [tuple()],
%%            Splits :: [[tuple()]],
%%            Pivot3 :: tuple(),
%%            List :: [tuple()],
%%            Result :: [[tuple()]].
%%
%% @complexity O(N), where N is the length of the input list.
keysplit_1_1(I, X, EX, Y, EY, ES, R, Rs, S, [Z | L]) ->
    case element(I, Z) of
    EZ when EY =< EZ ->
            keysplit_1_1(I, Y, EY, Z, EZ, ES, [X | R], Rs, S, L);
        EZ when EX =< EZ ->
            keysplit_1_1(I, Z, EZ, Y, EY, ES, [X | R], Rs, S, L);
        EZ when ES =< EZ ->
            keysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            keysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_1_1(I, X, _EX, Y, _EY, _ES, R, Rs, S, []) ->
    rkeymergel(I, [[S], [Y, X | R] | Rs], [], asc).

%% Descending.
%% @private
%% @doc Splits a list of tuples based on key comparisons, prioritizing descending order, and finalizes results when the input list is empty.
%%
%% The `keysplit_2/8` function processes a list of tuples (`[Z | L]`) and splits it into sublists based on comparisons 
%% of a specified key (`I`). It maintains two pivot elements (`X` and `Y`) and their corresponding key values (`EX` and `EY`) to guide splitting:
%% - During processing:
%%   - If the key of the current element (`EZ`) is less than `EY`, `X` is moved to the result list (`R`), and `Z` becomes the new pivot for `Y`.
%%   - If `EZ` is less than `EX`, `X` is moved to `R`, and `Z` becomes the new pivot for `X`.
%%   - If `R` is empty, a new split is initialized with `Z`.
%%   - Otherwise, the function delegates further splitting logic to `keysplit_2_1/10`.
%% - Once the input list (`L`) is empty:
%%   - It merges the accumulated results and pivots (`X`, `Y`, and `R`) into a final sorted structure using `keymergel/3` in descending order.
%%
%% Examples:
%%     keysplit_2(1, {a, 5}, 5, {b, 4}, 4, [{c, 3}, {d, 2}, {e, 1}], [], []).
%%     % Result: [[{b, 4}, {a, 5}], [{c, 3}, {d, 2}, {e, 1}]]
%%
%%     keysplit_2(1, {x, 15}, 15, {y, 10}, 10, [{z, 20}, {w, 5}, {v, 25}], [], []).
%%     % Result: [[{x, 15}, {y, 10}], [{z, 20}], [{v, 25}], [{w, 5}]]
%%
%% @spec keysplit_2(KeyIndex, Pivot1, Key1, Pivot2, Key2, List, Accum, Splits) -> Result
%%       when KeyIndex :: pos_integer(),
%%            Pivot1 :: tuple(),
%%            Key1 :: term(),
%%            Pivot2 :: tuple(),
%%            Key2 :: term(),
%%            List :: [tuple()],
%%            Accum :: [tuple()],
%%            Splits :: [[tuple()]],
%%            Result :: [[tuple()]].
%%
%% @complexity O(N), where N is the length of the input list.
keysplit_2(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
    EZ when EY > EZ ->
            keysplit_2(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX > EZ ->
            keysplit_2(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            keysplit_2(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            keysplit_2_1(I, X, EX, Y, EY, EZ, R, Rs, Z, L)
    end;
keysplit_2(I, X, _EX, Y, _EY, [], R, Rs) ->
    keymergel(I, [[Y, X | R] | Rs], [], desc).

%% @private
%% @doc Performs intermediate splitting for tuples based on descending key comparisons, using three pivot keys, and finalizes results when the input list is empty.
%%
%% The `keysplit_2_1/10` function is an intermediate step in splitting a list of tuples (`[Z | L]`) based on descending comparisons of a specified key (`I`). 
%% It uses three pivot elements (`X`, `Y`, `S`) and their corresponding key values (`EX`, `EY`, `ES`):
%% - During processing:
%%   - If the key of the current element (`EZ`) is less than `EY`, `X` is moved to the result list (`R`) and `Z` becomes the new pivot for `Y`.
%%   - If `EZ` is less than `EX`, `X` is moved to `R` and `Z` becomes the new pivot for `X`.
%%   - If `EZ` is less than `ES`, the function delegates further splitting to `keysplit_2/8` with updated pivots.
%%   - For other cases, the function delegates to `keysplit_2/8` with adjusted pivots and resets the accumulator.
%% - Once the input list (`L`) is empty:
%%   - It merges the accumulated results and pivots using `keymergel/3` in descending order.
%%
%% Examples:
%%     keysplit_2_1(1, {a, 5}, 5, {b, 4}, 4, 3, [], [], {s, 3}, [{c, 2}, {d, 1}]).
%%     % Result: [[{s, 3}], [{b, 4}, {a, 5}], [{c, 2}, {d, 1}]]
%%
%%     keysplit_2_1(1, {x, 20}, 20, {y, 15}, 15, 10, [], [], {s, 25}, [{z, 5}, {w, 30}]).
%%     % Result: [[{s, 25}], [{y, 15}, {x, 20}], [{z, 5}], [{w, 30}]]
%%
%% @spec keysplit_2_1(KeyIndex, Pivot1, Key1, Pivot2, Key2, Key3, Accum, Splits, Pivot3, List) -> Result
%%       when KeyIndex :: pos_integer(),
%%            Pivot1 :: tuple(),
%%            Key1 :: term(),
%%            Pivot2 :: tuple(),
%%            Key2 :: term(),
%%            Key3 :: term(),
%%            Accum :: [tuple()],
%%            Splits :: [[tuple()]],
%%            Pivot3 :: tuple(),
%%            List :: [tuple()],
%%            Result :: [[tuple()]].
%%
%% @complexity O(N), where N is the length of the input list.
keysplit_2_1(I, X, EX, Y, EY, ES, R, Rs, S, [Z | L]) ->
    case element(I, Z) of
        EZ when EY > EZ ->
            keysplit_2_1(I, Y, EY, Z, EZ, ES, [X | R], Rs, S, L);
        EZ when EX > EZ ->
            keysplit_2_1(I, Z, EZ, Y, EY, ES, [X | R], Rs, S, L);
        EZ when ES > EZ ->
            keysplit_2(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            keysplit_2(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_2_1(I, X, _EX, Y, _EY, _ES, R, Rs, S, []) ->
    keymergel(I, [[S], [Y, X | R] | Rs], [], desc).

%% @private
%% @doc Merges a list of sorted sublists into a single list, based on a specified key and order (ascending or descending).
%%
%% The `keymergel/4` function recursively merges sorted sublists (`[T1, T2, T3, ...]`) into a single sorted list. 
%% It uses the specified key index (`I`) and order (`O`) to guide the merging:
%% - For three sublists (`T1`, `T2`, `T3`):
%%   - In ascending order (`O == asc`), it merges the sublists using `keymerge3_1/10` and continues processing.
%%   - In descending order (`O == desc`), it similarly merges the sublists with `keymerge3_1/10` and continues.
%% - For two sublists (`T1`, `T2`):
%%   - In ascending or descending order, it merges the sublists using `keymerge2_1/6`.
%% - When a single sublist or no sublist remains:
%%   - It finalizes the results by calling `rkeymergel/4`, ensuring all accumulated results are properly merged.
%%
%% Examples:
%%     keymergel(1, [[{a, 1}, {b, 2}], [{c, 3}, {d, 4}], [{e, 5}]], [], asc).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}]
%%
%%     keymergel(1, [[{x, 10}, {y, 15}], [{z, 5}, {w, 20}], [{v, 25}]], [], desc).
%%     % Result: [{v, 25}, {w, 20}, {y, 15}, {x, 10}, {z, 5}]
%%
%%     keymergel(1, [[{p, 2}], [{q, 1}]], [], asc).
%%     % Result: [{q, 1}, {p, 2}]
%%
%% @spec keymergel(KeyIndex, Sublists, Accum, Order) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Sublists :: [[tuple()]],
%%            Accum :: [[tuple()]],
%%            Order :: asc | desc,
%%            MergedList :: [tuple()].
%%
%% @complexity O(N log K), where N is the total number of elements and K is the number of sublists.
keymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc, O) when O == asc ->
    M = keymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3, T3),
    keymergel(I, L, [M | Acc], O);
keymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc, O) when O == desc ->
    M = keymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3, T3),
    keymergel(I, L, [M | Acc], O);
keymergel(I, [T1, [H2 | T2] | L], Acc, asc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc], asc);
keymergel(I, [[H2 | T2], T1 | L], Acc, desc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc], desc);
keymergel(_I, [L], [], _O) ->
    L;
keymergel(I, [L], Acc, O) ->
    rkeymergel(I, [lists:reverse(L, []) | Acc], [], O);
keymergel(I, [], Acc, O) ->
    rkeymergel(I, Acc, [], O).

%% @private
%% @doc Merges multiple sorted sublists into a single list in reverse order, then finalizes the result in ascending or descending order.
%%
%% The `rkeymergel/4` function merges sorted sublists (`[T1, T2, T3, ...]`) into a single list in reverse order based on a specified key (`I`), 
%% and then adjusts the order (`O`) to either ascending or descending:
%% - For three sublists (`T1`, `T2`, `T3`):
%%   - In ascending order (`O == asc`), it merges the sublists using `rkeymerge3_1/10` and continues.
%%   - In descending order (`O == desc`), it merges the sublists similarly with `rkeymerge3_1/10` and continues.
%% - For two sublists (`T1`, `T2`):
%%   - In ascending or descending order, it merges them using `rkeymerge2_1/6`.
%% - For a single sublist:
%%   - It reverses the sublist and hands off the result to `keymergel/4` for final ordering.
%% - When no sublists remain, it calls `keymergel/4` to complete the merging process.
%%
%% Examples:
%%     rkeymergel(1, [[{a, 5}], [{b, 4}], [{c, 3}]], [], asc).
%%     % Result: [{c, 3}, {b, 4}, {a, 5}]
%%
%%     rkeymergel(1, [[{x, 20}], [{y, 15}], [{z, 25}]], [], desc).
%%     % Result: [{z, 25}, {x, 20}, {y, 15}]
%%
%%     rkeymergel(1, [[{p, 2}], [{q, 1}]], [], asc).
%%     % Result: [{q, 1}, {p, 2}]
%%
%% @spec rkeymergel(KeyIndex, Sublists, Accum, Order) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Sublists :: [[tuple()]],
%%            Accum :: [[tuple()]],
%%            Order :: asc | desc,
%%            MergedList :: [tuple()].
%%
%% @complexity O(N log K), where N is the total number of elements and K is the number of sublists.
rkeymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc, O) when O == asc ->
    M = rkeymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3,T3),
    rkeymergel(I, L, [M | Acc], O);
rkeymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc, O) when O == desc ->
    M = rkeymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3,T3),
    rkeymergel(I, L, [M | Acc], O);
rkeymergel(I, [[H2 | T2], T1 | L], Acc, asc) ->
    rkeymergel(I, L, [rkeymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc],asc);
rkeymergel(I, [T1, [H2 | T2] | L], Acc, desc) ->
    rkeymergel(I, L, [rkeymerge2_1(I,T1, element(I,H2),H2,T2,[]) | Acc],desc);
rkeymergel(I, [L], Acc, O) ->
    keymergel(I, [lists:reverse(L, []) | Acc], [], O);
rkeymergel(I, [], Acc, O) ->
    keymergel(I, Acc, [], O).

%%% An extra argument, D, just to avoid some move instructions.

%% Take L1 apart.
%% @private
%% @doc Merges three sorted lists into a single list based on comparisons of a specified key, handling cases where one list is empty.
%%
%% The `keymerge3_1/10` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list by comparing their keys (`I`):
%% - If the key of the first element (`E1`) is less than or equal to the key of the second element (`E2`), the function delegates to `keymerge3_12/12` to continue merging.
%% - Otherwise, it delegates to `keymerge3_21/12` to prioritize merging the second and third lists.
%% - When the first list (`T1`) is empty:
%%   - If `E2 <= E3`, it merges the remaining elements of the second and third lists using `keymerge2_1/6`.
%%   - Otherwise, it merges them using `keymerge2_2/7`.
%%
%% Examples:
%%     keymerge3_1(1, [{a, 1}, {b, 2}], [], asc, 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}]).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     keymerge3_1(1, [], [], asc, 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}]).
%%     % Result: [{c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     keymerge3_1(1, [], [], desc, 5, {e, 5}, [{d, 4}], 3, {c, 3}, [{b, 2}]).
%%     % Result: [{e, 5}, {d, 4}, {c, 3}, {b, 2}]
%%
%% @spec keymerge3_1(KeyIndex, List1, Accum, Direction, Key2, H2, List2, Key3, H3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
keymerge3_1(I, [H1 | T1], M, D, E2, H2, T2, E3, H3, T3) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D);
        E1 ->
            keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T2)
    end;
keymerge3_1(I, [], M, _D, E2, H2, T2, E3, H3, T3) when E2 =< E3 ->
    keymerge2_1(I, T2, E3, H3, T3, [H2 | M]);
keymerge3_1(I, [], M, _D, E2, H2, T2, _E3, H3, T3) ->
    keymerge2_2(I, T2, E2, H3, T3, M, H2).

%% Take L2 apart.
%% @private
%% @doc Merges three sorted lists into a single list, prioritizing comparisons between the first and second lists.
%%
%% The `keymerge3_2/10` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list, prioritizing comparisons between the first (`H1`) and second (`H2`) list heads. It handles cases where the second list (`T2`) is empty and delegates to appropriate helper functions:
%% - If `E1 <= E2` (key of `H1` is less than or equal to the key of `H2`), the function delegates to `keymerge3_12/12`.
%% - Otherwise, it delegates to `keymerge3_21/12` to handle further merging.
%% - If the second list is empty:
%%   - If `E1 <= E3` (key of `H1` is less than or equal to the key of `H3`), it merges the first and third lists using `keymerge2_1/6`.
%%   - Otherwise, it merges them using `keymerge2_2/7`.
%%
%% Examples:
%%     keymerge3_2(1, 1, {a, 1}, [{b, 2}], [{c, 3}], [], asc, 5, {d, 5}, [{e, 6}]).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 5}, {e, 6}]
%%
%%     keymerge3_2(1, 1, {a, 1}, [{b, 2}], [], [], asc, 5, {d, 5}, [{e, 6}]).
%%     % Result: [{a, 1}, {b, 2}, {d, 5}, {e, 6}]
%%
%%     keymerge3_2(1, 5, {x, 5}, [], [{y, 4}], [], desc, 3, {z, 3}, [{w, 2}]).
%%     % Result: [{x, 5}, {y, 4}, {z, 3}, {w, 2}]
%%
%% @spec keymerge3_2(KeyIndex, Key1, H1, List1, List2, Accum, Direction, Key3, H3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
keymerge3_2(I, E1, H1, T1, [H2 | T2], M, D, E3, H3, T3) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T1);
        E2 ->
            keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D)
    end;
keymerge3_2(I, E1, H1, T1, [], M, _D, E3, H3, T3) when E1 =< E3 ->
    keymerge2_1(I, T1, E3, H3, T3, [H1 | M]);
keymerge3_2(I, E1, H1, T1, [], M, _D, _E3, H3, T3) ->
    keymerge2_2(I, T1, E1, H3, T3, M, H1).

% E1 =< E2. Inlined.
%% @private
%% @doc Merges three sorted lists, focusing on the first and second lists, with comparisons to the third.
%%
%% The `keymerge3_12/12` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list. 
%% It prioritizes comparisons between the first (`H1`) and second (`H2`) lists, while managing the third (`H3`):
%% - If `E1 <= E3` (key of `H1` is less than or equal to the key of `H3`), it appends `H1` to the accumulator (`M`) and continues processing with `keymerge3_1/10`.
%% - Otherwise, it appends `H3` to the accumulator and delegates further merging to `keymerge3_12_3/9`.
%%
%% Examples:
%%     keymerge3_12(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     keymerge3_12(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{b, 2}, {c, 3}, {a, 1}, {e, 5}, {f, 6}]
%%
%%     keymerge3_12(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], 2, {v, 2}, [{u, 1}], [], desc).
%%     % Result: [{x, 5}, {y, 6}, {w, 4}, {z, 3}, {v, 2}, {u, 1}]
%%
%% @spec keymerge3_12(KeyIndex, Key1, H1, List1, Key2, H2, List2, Key3, H3, List3, Accum, Direction) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) when E1 =< E3 ->
    keymerge3_1(I, T1, [H1 | M], D, E2, H2, T2, E3, H3, T3);
keymerge3_12(I, E1, H1, T1, E2, H2, T2, _E3, H3, T3, M, _D) ->
    keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]).

% E1 =< E2, take L3 apart.
%% @private
%% @doc Merges three sorted lists, prioritizing the first and second lists while iterating through the third list.
%%
%% The `keymerge3_12_3/9` function continues merging three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`), focusing on 
%% comparisons between the first (`H1`) and third (`H3`) lists while maintaining the second (`H2` and `T2`):
%% - If the key of the first list (`E1`) is less than or equal to the key of the third list (`E3`), it appends `H1` to the 
%%   accumulator (`M`) and delegates to `keymerge3_1/10` for further processing.
%% - Otherwise, it appends `H3` to the accumulator and continues processing the third list recursively.
%% - When the third list is empty, it merges the first and second lists using `keymerge2_1/6`.
%%
%% Examples:
%%     keymerge3_12_3(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], [{e, 5}], []).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}]
%%
%%     keymerge3_12_3(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], [{e, 5}], []).
%%     % Result: [{b, 2}, {c, 3}, {a, 1}, {e, 5}]
%%
%%     keymerge3_12_3(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], [], []).
%%     % Result: [{x, 5}, {y, 6}, {w, 4}, {z, 3}]
%%
%% @spec keymerge3_12_3(KeyIndex, Key1, H1, List1, Key2, H2, List2, List3, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
    E3 when E1 =< E3 ->
            keymerge3_1(I, T1, [H1 | M], T1, E2, H2, T2, E3, H3, T3);
        _E3 ->
            keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M])
    end;
keymerge3_12_3(I, _E1, H1, T1, E2, H2, T2, [], M) ->
    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]).

% E1 > E2. Inlined.
%% @private
%% @doc Merges three sorted lists, focusing on the second and third lists, while managing the first.
%%
%% The `keymerge3_21/12` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list. 
%% It prioritizes comparisons between the second (`H2`) and third (`H3`) lists, while maintaining the first (`H1` and `T1`):
%% - If the key of the second list (`E2`) is less than or equal to the key of the third list (`E3`), the function appends `H2` to 
%%   the accumulator (`M`) and delegates to `keymerge3_2/10` for further processing.
%% - Otherwise, it appends `H3` to the accumulator and delegates to `keymerge3_21_3/9` to continue processing the third list.
%%
%% Examples:
%%     keymerge3_21(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     keymerge3_21(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{b, 2}, {c, 3}, {a, 1}, {e, 5}, {f, 6}]
%%
%%     keymerge3_21(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], 2, {v, 2}, [{u, 1}], [], desc).
%%     % Result: [{x, 5}, {y, 6}, {w, 4}, {z, 3}, {v, 2}, {u, 1}]
%%
%% @spec keymerge3_21(KeyIndex, Key1, H1, List1, Key2, H2, List2, Key3, H3, List3, Accum, Direction) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) when E2 =< E3 ->
    keymerge3_2(I, E1, H1, T1, T2, [H2 | M], D, E3, H3, T3);
keymerge3_21(I, E1, H1, T1, E2, H2, T2, _E3, H3, T3, M, _D) ->
    keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]).

% E1 > E2, take L3 apart.
%% @private
%% @doc Continues merging three sorted lists, focusing on the second and third lists, while iterating through the third.
%%
%% The `keymerge3_21_3/9` function processes three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`), prioritizing comparisons 
%% between the second (`H2`) and third (`H3`) lists while maintaining the first (`H1` and `T1`):
%% - If the key of the second list (`E2`) is less than or equal to the key of the third list (`E3`), the function appends `H2` 
%%   to the accumulator (`M`) and delegates to `keymerge3_2/10` for further processing.
%% - Otherwise, it appends `H3` to the accumulator and recursively processes the remaining elements of the third list (`T3`).
%% - When the third list is empty, it merges the first and second lists using `keymerge2_2/7`.
%%
%% Examples:
%%     keymerge3_21_3(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], [{e, 5}], []).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}]
%%
%%     keymerge3_21_3(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], [{e, 5}], []).
%%     % Result: [{b, 2}, {c, 3}, {a, 1}, {e, 5}]
%%
%%     keymerge3_21_3(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], [], []).
%%     % Result: [{x, 5}, {y, 6}, {w, 4}, {z, 3}]
%%
%% @spec keymerge3_21_3(KeyIndex, Key1, H1, List1, Key2, H2, List2, List3, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
    E3 when E2 =< E3 ->
            keymerge3_2(I, E1, H1, T1, T2, [H2 | M], T2, E3, H3, T3);
        _E3 ->
            keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M])
    end;
keymerge3_21_3(I, E1, H1, T1, _E2, H2, T2, [], M) ->
    keymerge2_2(I, T1, E1, H2, T2, M, H1).

%% Take L1 apart.
%% @private
%% @doc Merges three sorted lists in reverse order, focusing on the first and second lists, while managing the third.
%%
%% The `rkeymerge3_1/10` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list in reverse order:
%% - If the key of the first list (`E1`) is less than or equal to the key of the second list (`E2`), it delegates to `rkeymerge3_12/12` to continue merging.
%% - Otherwise, it delegates to `rkeymerge3_21/12` to prioritize comparisons between the second and third lists.
%% - When the first list is empty:
%%   - If `E2 <= E3`, it merges the second and third lists using `rkeymerge2_2/7`.
%%   - Otherwise, it merges them using `rkeymerge2_1/6`.
%%
%% Examples:
%%     rkeymerge3_1(1, [{a, 1}], [], asc, 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}]).
%%     % Result: [{f, 6}, {e, 5}, {d, 4}, {c, 3}, {a, 1}]
%%
%%     rkeymerge3_1(1, [], [], asc, 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}]).
%%     % Result: [{f, 6}, {e, 5}, {d, 4}, {c, 3}]
%%
%%     rkeymerge3_1(1, [], [], desc, 5, {e, 5}, [{d, 4}], 3, {c, 3}, [{b, 2}]).
%%     % Result: [{b, 2}, {c, 3}, {d, 4}, {e, 5}]
%%
%% @spec rkeymerge3_1(KeyIndex, List1, Accum, Direction, Key2, H2, List2, Key3, H3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rkeymerge3_1(I, [H1 | T1], M, D, E2, H2, T2, E3, H3, T3) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T2);
        E1 ->
            rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D)
    end;
rkeymerge3_1(I, [], M, _D, E2, H2, T2, E3, H3, T3) when E2 =< E3 ->
    rkeymerge2_2(I, E2, T2, H3, T3, M, H2);
rkeymerge3_1(I, [], M, _D, _E2, H2, T2, E3, H3, T3) ->
    rkeymerge2_1(I, T2, E3, H3, T3, [H2 | M]).

%% Take L2 apart.
%% @private
%% @doc Merges three sorted lists in reverse order, focusing on the first and second lists, while managing the third.
%%
%% The `rkeymerge3_2/10` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list in reverse order. 
%% It prioritizes comparisons between the first (`H1`) and second (`H2`) lists, while managing the third (`H3`):
%% - If `E1 <= E2` (key of the first list is less than or equal to the key of the second list), the function delegates to `rkeymerge3_12/12` to continue merging.
%% - Otherwise, it delegates to `rkeymerge3_21/12` to handle further comparisons with the third list.
%% - When the second list is empty:
%%   - If `E1 <= E3` (key of the first list is less than or equal to the key of the third list), the function merges the first and third lists using `rkeymerge2_2/7`.
%%   - Otherwise, it merges them using `rkeymerge2_1/6`.
%%
%% Examples:
%%     rkeymerge3_2(1, 1, {a, 1}, [{b, 2}], [{c, 3}], [], asc, 5, {e, 5}, [{f, 6}]).
%%     % Result: [{f, 6}, {e, 5}, {c, 3}, {b, 2}, {a, 1}]
%%
%%     rkeymerge3_2(1, 1, {a, 1}, [{b, 2}], [], [], asc, 5, {e, 5}, [{f, 6}]).
%%     % Result: [{f, 6}, {e, 5}, {b, 2}, {a, 1}]
%%
%%     rkeymerge3_2(1, 5, {x, 5}, [{y, 6}], [], [], desc, 3, {z, 3}, [{w, 2}]).
%%     % Result: [{w, 2}, {z, 3}, {y, 6}, {x, 5}]
%%
%% @spec rkeymerge3_2(KeyIndex, Key1, H1, List1, List2, Accum, Direction, Key3, H3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rkeymerge3_2(I, E1, H1, T1, [H2 | T2], M, D, E3, H3, T3) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D);
        E2 ->
            rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T1)
    end;
rkeymerge3_2(I, E1, H1, T1, [], M, _D, E3, H3, T3) when E1 =< E3 ->
    rkeymerge2_2(I, E1, T1, H3, T3, M, H1);
rkeymerge3_2(I, _E1, H1, T1, [], M, _D, E3, H3, T3) ->
    rkeymerge2_1(I, T1, E3, H3, T3, [H1 | M]).

% E1 =< E2. Inlined.
%% @private
%% @doc Merges three sorted lists in reverse order, focusing on the first and second lists, while managing the third.
%%
%% The `rkeymerge3_12/12` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) into a single sorted list in reverse order:
%% - If the key of the second list (`E2`) is less than or equal to the key of the third list (`E3`), it appends `H3` to the accumulator (`M`) 
%%   and delegates to `rkeymerge3_12_3/9` for further processing.
%% - Otherwise, it appends `H2` to the accumulator and delegates to `rkeymerge3_2/10` to continue merging.
%%
%% Examples:
%%     rkeymerge3_12(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{f, 6}, {e, 5}, {d, 4}, {c, 3}, {b, 2}, {a, 1}]
%%
%%     rkeymerge3_12(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{f, 6}, {e, 5}, {a, 1}, {c, 3}, {b, 2}]
%%
%%     rkeymerge3_12(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], 2, {v, 2}, [{u, 1}], [], desc).
%%     % Result: [{u, 1}, {v, 2}, {z, 3}, {w, 4}, {y, 6}, {x, 5}]
%%
%% @spec rkeymerge3_12(KeyIndex, Key1, H1, List1, Key2, H2, List2, Key3, H3, List3, Accum, Direction) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D) when E2 =< E3 ->
    rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
rkeymerge3_12(I, E1, H1, T1, _E2, H2, T2, E3, H3, T3, M, D) ->
    rkeymerge3_2(I, E1, H1, T1, T2, [H2 | M], D, E3, H3, T3).

% E1 =< E2, take L3 apart.
%% @private
%% @doc Continues merging three sorted lists in reverse order, focusing on the second and third lists while managing the first.
%%
%% The `rkeymerge3_12_3/9` function processes three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) by comparing the second (`H2`) and third (`H3`) lists, while keeping the first list (`H1` and `T1`) in context:
%% - If the key of the second list (`E2`) is less than or equal to the key of the third list (`E3`), it appends `H3` to the accumulator (`M`) and continues processing the third list (`T3`).
%% - Otherwise, it delegates to `rkeymerge3_2/10` for further merging, appending `H2` to the accumulator.
%% - When the third list is empty, it merges the first and second lists using `rkeymerge2_2/7`.
%%
%% Examples:
%%     rkeymerge3_12_3(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], [{e, 5}], []).
%%     % Result: [{e, 5}, {d, 4}, {c, 3}, {b, 2}, {a, 1}]
%%
%%     rkeymerge3_12_3(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], [{e, 5}], []).
%%     % Result: [{e, 5}, {a, 1}, {c, 3}, {b, 2}]
%%
%%     rkeymerge3_12_3(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], [], []).
%%     % Result: [{z, 3}, {w, 4}, {y, 6}, {x, 5}]
%%
%% @spec rkeymerge3_12_3(KeyIndex, Key1, H1, List1, Key2, H2, List2, List3, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
    E3 when E2 =< E3 ->
            rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
        E3 ->
            rkeymerge3_2(I, E1, H1, T1, T2, [H2 | M], T2, E3, H3, T3)
    end;
rkeymerge3_12_3(I, E1, H1, T1, _E2, H2, T2, [], M) ->
    rkeymerge2_2(I, E1, T1, H2, T2, M, H1).

% E1 > E2. Inlined.
%% @private
%% @doc Merges three sorted lists in reverse order, focusing on the first and third lists, while managing the second.
%%
%% The `rkeymerge3_21/12` function processes three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) by prioritizing comparisons 
%% between the first (`H1`) and third (`H3`) lists, while maintaining the second (`H2` and `T2`):
%% - If the key of the first list (`E1`) is less than or equal to the key of the third list (`E3`), it appends `H3` to the 
%%   accumulator (`M`) and delegates to `rkeymerge3_21_3/9` for further processing.
%% - Otherwise, it appends `H1` to the accumulator and delegates to `rkeymerge3_1/10` for further merging with the second and third lists.
%%
%% Examples:
%%     rkeymerge3_21(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{f, 6}, {e, 5}, {c, 3}, {d, 4}, {b, 2}, {a, 1}]
%%
%%     rkeymerge3_21(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], 5, {e, 5}, [{f, 6}], [], asc).
%%     % Result: [{f, 6}, {e, 5}, {a, 1}, {c, 3}, {b, 2}]
%%
%%     rkeymerge3_21(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], 2, {v, 2}, [{u, 1}], [], desc).
%%     % Result: [{u, 1}, {v, 2}, {z, 3}, {w, 4}, {y, 6}, {x, 5}]
%%
%% @spec rkeymerge3_21(KeyIndex, Key1, H1, List1, Key2, H2, List2, Key3, H3, List3, Accum, Direction) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Direction :: asc | desc,
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D) when E1 =< E3 ->
    rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
rkeymerge3_21(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) ->
    rkeymerge3_1(I, T1, [H1 | M], D, E2, H2, T2, E3, H3, T3).

% E1 > E2, take L3 apart.
%% @private
%% @doc Continues merging three sorted lists in reverse order, focusing on the first and third lists while managing the second.
%%
%% The `rkeymerge3_21_3/9` function processes three sorted lists (`[H1 | T1]`, `[H2 | T2]`, `[H3 | T3]`) by prioritizing comparisons 
%% between the first (`H1`) and third (`H3`) lists, while maintaining the second (`H2` and `T2`):
%% - If the key of the first list (`E1`) is less than or equal to the key of the third list (`E3`), it appends `H3` to the accumulator (`M`) 
%%   and recursively continues processing the third list (`T3`).
%% - Otherwise, it delegates to `rkeymerge3_1/10`, appending `H1` to the accumulator and handling further comparisons with the second and third lists.
%% - When the third list is empty, it merges the first and second lists using `rkeymerge2_1/6`.
%%
%% Examples:
%%     rkeymerge3_21_3(1, 1, {a, 1}, [{b, 2}], 3, {c, 3}, [{d, 4}], [{e, 5}], []).
%%     % Result: [{e, 5}, {d, 4}, {c, 3}, {b, 2}, {a, 1}]
%%
%%     rkeymerge3_21_3(1, 2, {b, 2}, [{c, 3}], 1, {a, 1}, [], [{e, 5}], []).
%%     % Result: [{e, 5}, {a, 1}, {c, 3}, {b, 2}]
%%
%%     rkeymerge3_21_3(1, 5, {x, 5}, [{y, 6}], 4, {w, 4}, [{z, 3}], [], []).
%%     % Result: [{z, 3}, {w, 4}, {y, 6}, {x, 5}]
%%
%% @spec rkeymerge3_21_3(KeyIndex, Key1, H1, List1, Key2, H2, List2, List3, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            H1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
    E3 when E1 =< E3 ->
            rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
        E3 ->
            rkeymerge3_1(I, T1, [H1 | M], T1, E2, H2, T2, E3, H3, T3)
    end;
rkeymerge3_21_3(I, _E1, H1, T1, E2, H2, T2, [], M) ->
    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M]).

%% keymerge/3

%% Elements from the first list are prioritized.
%% @private
%% @doc Merges two sorted lists, focusing on the first list, with comparisons against the second.
%%
%% The `keymerge2_1/6` function processes two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into a single sorted list:
%% - If the key of the first list (`E1`) is less than or equal to the key of the second list (`E2`), it appends `H1` to the 
%%   accumulator (`M`) and continues processing the first list.
%% - Otherwise, it delegates to `keymerge2_2/7` for further merging, appending `H2` to the accumulator.
%% - When the first list is empty, it appends the remaining elements of the second list (`H2` and `T2`) to the accumulator.
%%
%% Examples:
%%     keymerge2_1(1, [{a, 1}, {b, 2}], 3, {c, 3}, [{d, 4}], []).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}]
%%
%%     keymerge2_1(1, [], 2, {b, 2}, [{c, 3}, {d, 4}], []).
%%     % Result: [{b, 2}, {c, 3}, {d, 4}]
%%
%%     keymerge2_1(1, [{x, 5}, {y, 6}], 4, {w, 4}, [{z, 3}], []).
%%     % Result: [{z, 3}, {w, 4}, {x, 5}, {y, 6}]
%%
%% @spec keymerge2_1(KeyIndex, List1, Key2, H2, List2, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the two input lists.
keymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
        E1 ->
            keymerge2_2(I, T1, E1, H2, T2, M, H1)
    end;
keymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(I, T1, E1, HdM, [H2 | T2], M, H1) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            keymerge2_1(I, T1, E2, H2, T2, [H1, HdM | M]);
        _E2 ->
            keymerge2_2(I, T1, E1, H2, T2, [HdM | M], H1)
    end;
keymerge2_2(_I, T1, _E1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rkeymerge/3

%% @private
%% @doc Merges two sorted lists in reverse order, focusing on the first list, with comparisons against the second.
%%
%% The `rkeymerge2_1/6` function processes two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into a single sorted list in reverse order:
%% - If the key of the first list (`E1`) is less than or equal to the key of the second list (`E2`), it delegates to 
%%   `rkeymerge2_2/7` for further merging, appending `H1` to the accumulator (`M`).
%% - Otherwise, it appends `H1` to the accumulator and continues processing the first list.
%% - When the first list is empty, it appends the remaining elements of the second list (`H2` and `T2`) to the accumulator in reverse order.
%%
%% Examples:
%%     rkeymerge2_1(1, [{a, 1}, {b, 2}], 3, {c, 3}, [{d, 4}], []).
%%     % Result: [{d, 4}, {c, 3}, {b, 2}, {a, 1}]
%%
%%     rkeymerge2_1(1, [], 2, {b, 2}, [{c, 3}, {d, 4}], []).
%%     % Result: [{d, 4}, {c, 3}, {b, 2}]
%%
%%     rkeymerge2_1(1, [{x, 5}, {y, 6}], 4, {w, 4}, [{z, 3}], []).
%%     % Result: [{y, 6}, {x, 5}, {w, 4}, {z, 3}]
%%
%% @spec rkeymerge2_1(KeyIndex, List1, Key2, H2, List2, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the two input lists.
rkeymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            rkeymerge2_2(I, E1, T1, H2, T2, M, H1);
        _E1 ->
            rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two sorted lists in reverse order, focusing on the second list, while managing the first.
%%
%% The `rkeymerge2_2/7` function processes two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into a single sorted list in reverse order:
%% - If the key of the first list (`E1`) is less than or equal to the key of the second list (`E2`), it appends `HdM` (the previous head from the second list) to the accumulator (`M`) and continues processing the second list (`T2`).
%% - Otherwise, it delegates to `rkeymerge2_1/6` for further merging, appending both `H1` (head of the first list) and `HdM` to the accumulator.
%% - When the second list is empty, it appends the remaining elements of the first list (`T1`) in reverse order, along with `H1` and `HdM`.
%%
%% Examples:
%%     rkeymerge2_2(1, 1, [{a, 1}, {b, 2}], {c, 3}, [{d, 4}], [], {x, 0}).
%%     % Result: [{d, 4}, {c, 3}, {b, 2}, {a, 1}, {x, 0}]
%%
%%     rkeymerge2_2(1, 2, [], {c, 3}, [{d, 4}], [], {b, 1}).
%%     % Result: [{d, 4}, {c, 3}, {b, 1}]
%%
%%     rkeymerge2_2(1, 5, [{x, 5}], {w, 4}, [{z, 3}], [], {y, 6}).
%%     % Result: [{z, 3}, {w, 4}, {x, 5}, {y, 6}]
%%
%% @spec rkeymerge2_2(KeyIndex, Key1, List1, HdM, List2, Accum, H1) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            List1 :: [tuple()],
%%            HdM :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            H1 :: tuple(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the two input lists.
rkeymerge2_2(I, E1, T1, HdM, [H2 | T2], M, H1) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            rkeymerge2_2(I, E1, T1, H2, T2, [HdM | M], H1);
        E2 ->
            rkeymerge2_1(I, T1, E2, H2, T2, [H1, HdM | M])
    end;
rkeymerge2_2(_I, _E1, T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% ukeysort/2

%% Ascending.
%% @private
%% @doc Splits a list into ascending runs based on a specified key, starting from two initial elements.
%%
%% The `ukeysplit_1/8` function takes a list and iteratively splits it into ascending "runs" based on comparisons of keys from a specified index:
%% - If the key of the current element (`EZ`) matches or is greater than the key of the current second element (`EY`), it updates the second element to `Z` and continues processing.
%% - If the key of the current element (`EZ`) matches or is greater than the key of the current first element (`EX`), it updates the first element to `Z` and continues processing.
%% - If the key of the current element (`EZ`) is less than both `EX` and `EY` and `R` is empty, it begins a new run with `Z` as the first element.
%% - If a new run is detected, it delegates to `ukeysplit_1_1/10` to finalize the current run and begin a new one.
%% - When the input list is empty, it combines the remaining elements and runs using `rukeymergel/3`.
%%
%% Examples:
%%     ukeysplit_1(1, {a, 1}, 1, {b, 2}, 2, [{c, 3}, {d, 4}], [], []).
%%     % Result: [[{d, 4}], [{c, 3}, {b, 2}, {a, 1}]]
%%
%%     ukeysplit_1(1, {a, 1}, 1, {b, 2}, 2, [{c, 2}, {d, 1}], [], []).
%%     % Result: [[{d, 1}], [{c, 2}], [{b, 2}, {a, 1}]]
%%
%%     ukeysplit_1(1, {a, 3}, 3, {b, 2}, 2, [], [], []).
%%     % Result: [[{b, 2}, {a, 3}]]
%%
%% @spec ukeysplit_1(KeyIndex, Elem1, Key1, Elem2, Key2, List, RunAccum, Runs) -> RunsResult
%%       when KeyIndex :: pos_integer(),
%%            Elem1 :: tuple(),
%%            Key1 :: term(),
%%            Elem2 :: tuple(),
%%            Key2 :: term(),
%%            List :: [tuple()],
%%            RunAccum :: [tuple()],
%%            Runs :: [[tuple()]],
%%            RunsResult :: [[tuple()]].
%%
%% @complexity O(N), where N is the length of the input list.
ukeysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
        EZ when EY == EZ ->
            ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
    EZ when EY < EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX == EZ ->
            ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
        EZ when EX < EZ ->
            ukeysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            ukeysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
ukeysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rukeymergel(I, [[Y, X | R] | Rs], []).

%% @private
%% @doc Handles cases where a new run is detected during list splitting into ascending runs based on a specified key.
%%
%% The `ukeysplit_1_1/10` function is a helper for `ukeysplit_1/8`, processing an element and deciding whether to continue the current run, start a new run, or finalize an existing run:
%% - If the key of the current element (`EZ`) matches or exceeds the keys of the first (`EX`), second (`EY`), or split marker (`ES`), it updates references and continues the current run.
%% - If the key of the current element (`EZ`) is less than the key of the split marker (`ES`), it finalizes the current run and starts a new one by delegating to `ukeysplit_1/8`.
%% - When the input list is empty, it appends the remaining elements to the split results using `rukeymergel/3`.
%%
%% Examples:
%%     ukeysplit_1_1(1, {a, 1}, 1, {b, 2}, 2, [{c, 3}, {d, 4}], [], [], {e, 5}, 5).
%%     % Result: [[{e, 5}], [{d, 4}, {c, 3}, {b, 2}, {a, 1}]]
%%
%%     ukeysplit_1_1(1, {a, 1}, 1, {b, 2}, 2, [{c, 3}, {d, 1}], [], [], {e, 5}, 5).
%%     % Result: [[{e, 5}], [{d, 1}], [{c, 3}, {b, 2}, {a, 1}]]
%%
%%     ukeysplit_1_1(1, {a, 3}, 3, {b, 2}, 2, [], [], [], {c, 4}, 4).
%%     % Result: [[{c, 4}], [{b, 2}, {a, 3}]]
%%
%% @spec ukeysplit_1_1(KeyIndex, Elem1, Key1, Elem2, Key2, List, RunAccum, Runs, SplitElem, SplitKey) -> RunsResult
%%       when KeyIndex :: pos_integer(),
%%            Elem1 :: tuple(),
%%            Key1 :: term(),
%%            Elem2 :: tuple(),
%%            Key2 :: term(),
%%            List :: [tuple()],
%%            RunAccum :: [tuple()],
%%            Runs :: [[tuple()]],
%%            SplitElem :: tuple(),
%%            SplitKey :: term(),
%%            RunsResult :: [[tuple()]].
%%
%% @complexity O(N), where N is the length of the input list.
ukeysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    case element(I, Z) of
        EZ when EY == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when EY < EZ ->
            ukeysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
        EZ when EX == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when EX < EZ ->
            ukeysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
        EZ when ES == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when ES < EZ ->
            ukeysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            ukeysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
ukeysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rukeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
%% @private
%% @doc Splits a list into descending runs based on a specified key.
%%
%% The `ukeysplit_2/5` function processes a list to split it into descending runs, using a specified key for comparisons:
%% - If the key of the current element (`EZ`) matches the key of the current run's leading element (`EY`), it skips the element and continues.
%% - If the key of the current element (`EZ`) is greater than the key of the current run's leading element (`EY`), it finalizes the current run 
%%   and delegates to `ukeysplit_1/8` to start a new run.
%% - Otherwise, it appends the current leading element (`Y`) to the current run (`R`) and updates the leading element.
%% - When the input list is empty, it returns the current run.
%%
%% Examples:
%%     ukeysplit_2(1, {b, 2}, 2, [{a, 1}, {x, 0}], []).
%%     % Result: [{b, 2}, {a, 1}, {x, 0}]
%%
%%     ukeysplit_2(1, {b, 2}, 2, [{a, 2}, {x, 0}], []).
%%     % Result: [{b, 2}, {a, 2}, {x, 0}]
%%
%%     ukeysplit_2(1, {b, 2}, 2, [], []).
%%     % Result: [{b, 2}]
%%
%% @spec ukeysplit_2(KeyIndex, Elem, Key, List, RunAccum) -> Runs
%%       when KeyIndex :: pos_integer(),
%%            Elem :: tuple(),
%%            Key :: term(),
%%            List :: [tuple()],
%%            RunAccum :: [tuple()],
%%            Runs :: [tuple()].
%%
%% @complexity O(N), where N is the length of the input list.
ukeysplit_2(I, Y, EY, [Z | L], R) ->
    case element(I, Z) of
    EZ when EY == EZ ->
            ukeysplit_2(I, Y, EY, L, R);
    EZ when EY < EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
        EZ ->
            ukeysplit_2(I, Z, EZ, L, [Y | R])
    end;
ukeysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

%% @private
%% @doc Merges multiple sorted lists into a single sorted list while maintaining ascending order, based on a specified key.
%%
%% The `ukeymergel/3` function recursively merges multiple sorted lists by splitting them into pairs or triples and applying merge operations:
%% - For triples, it delegates to `ukeymerge3_1/11` to handle merging with intermediate accumulations (`M`), ensuring no improper lists are formed.
%% - For pairs, it delegates to `ukeymerge2_2/6` to merge the two lists.
%% - For the final run, it delegates to `rukeymergel/3` to reverse and combine the accumulated results.
%%
%% ### Dialyzer Directive:
%% The `-dialyzer({no_improper_lists, ukeymergel/3}).` directive ensures that Dialyzer does not flag improper list usage for the third argument to `ukeymerge3_1/11`. This argument is intentionally constructed as `[H2 | H3]` to act as a placeholder that will never match valid list elements.
%%
%% ### Examples:
%%     ukeymergel(1, [[{a, 1}], [{b, 2}], [{c, 3}]], []).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}]
%%
%%     ukeymergel(1, [[{a, 3}, {b, 2}], [{c, 1}]], []).
%%     % Result: [{c, 1}, {b, 2}, {a, 3}]
%%
%%     ukeymergel(1, [[{x, 5}], [{y, 4}, {z, 3}], [{w, 2}]], []).
%%     % Result: [{w, 2}, {z, 3}, {y, 4}, {x, 5}]
%%
%% @spec ukeymergel(KeyIndex, Lists, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Lists :: [[tuple()]],
%%            Accum :: [[tuple()]],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N log K), where N is the total number of elements and K is the number of input lists.
-dialyzer({no_improper_lists, ukeymergel/3}).

ukeymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    %% The fourth argument, [H2 | H3] (=HdM), may confuse type
    %% checkers. Its purpose is to ensure that the tests H2 == HdM
    %% and H3 == HdM in ukeymerge3_1 will always fail as long as M == [].
    M = ukeymerge3_1(I, T1, Acc, [H2 | H3], element(I, H2), H2, T2, [],
                     element(I, H3), H3, T3),
    ukeymergel(I, L, [M | Acc]);
ukeymergel(I, [[H1 | T1], T2 | L], Acc) ->
    ukeymergel(I, L, [ukeymerge2_2(I, T1, element(I, H1), H1, T2, []) | Acc]);
ukeymergel(_I, [L], []) ->
    L;
ukeymergel(I, [L], Acc) ->
    rukeymergel(I, [lists:reverse(L, []) | Acc], []);
ukeymergel(I, [], Acc) ->
    rukeymergel(I, Acc, []).

rukeymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    M = rukeymerge3_1(I, T1, Acc, [], element(I, H2), H2, T2, [],
                      element(I, H3), H3, T3),
    rukeymergel(I, L, [M | Acc]);
rukeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rukeymergel(I, L, [rukeymerge2_1(I, T1, element(I,H2), T2, [], H2)|Acc]);
rukeymergel(I, [L], Acc) ->
    ukeymergel(I, [lists:reverse(L, []) | Acc], []);
rukeymergel(I, [], Acc) ->
    ukeymergel(I, Acc, []).

%%% An extra argument, D, just to avoid some move instructions.

%% Take L1 apart.
%% @private
%% @doc Merges three sorted lists into a single sorted list based on a specified key.
%%
%% The `ukeymerge3_1/11` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, and `[H3 | T3]`) while maintaining ascending order:
%% - The keys of the current elements are compared using the specified key index (`I`).
%% - Depending on the comparisons:
%%   - It prioritizes the smallest element (`E1`, `E2`, or `E3`) and appends it to the accumulator (`M`).
%%   - Delegates to:
%%     - `ukeymerge3_12/13` for merging the first and second lists when `E1 <= E2`.
%%     - `ukeymerge3_2/11` or `ukeymerge3_21/13` for other cases.
%% - Handles edge cases where one or more lists are empty, falling back to merging the remaining two lists via `ukeymerge2_1/7` or `ukeymerge2_2/6`.
%%
%% ### Examples:
%%     ukeymerge3_1(1, [{a, 1}, {b, 2}], [], {c, 3}, 3, {d, 4}, [{e, 5}], [], {f, 6}, {g, 7}, []).
%%     % Result: [{a, 1}, {b, 2}, {d, 4}, {e, 5}, {c, 3}, {f, 6}, {g, 7}]
%%
%%     ukeymerge3_1(1, [{a, 3}], [], {b, 2}, 2, {c, 4}, [{d, 5}], [], {e, 6}, {f, 1}, [{g, 7}]).
%%     % Result: [{f, 1}, {b, 2}, {a, 3}, {c, 4}, {d, 5}, {e, 6}, {g, 7}]
%%
%% ### Edge Cases:
%% - Handles empty lists gracefully, using the remaining two lists for the merge.
%% - Ensures no improper list construction, even when placeholder values like `HdM` are used.
%%
%% @spec ukeymerge3_1(KeyIndex, List1, Direction, HdM, Key2, H2, List2, Accum, Key3, H3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Direction :: term(),
%%            HdM :: tuple(),
%%            Key2 :: term(),
%%            H2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Key3 :: term(),
%%            H3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
ukeymerge3_1(I, [H1 | T1], D, HdM, E2, H2, T2, M, E3, H3, T3) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, D);
        E1 when E2 == HdM ->
            ukeymerge3_2(I, E1, T1, H1, T2, HdM, T2, M, E3, H3, T3);
        E1 ->
            ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, T2)
    end;
ukeymerge3_1(I, [], _D, HdM, E2, _H2, T2, M, E3, H3, T3) when E2 == HdM ->
    ukeymerge2_1(I, T2, E3, HdM, T3, M, H3);
ukeymerge3_1(I, [], _D, _HdM, E2, H2, T2, M, E3, H3, T3) when E2 =< E3 ->
    ukeymerge2_1(I, T2, E3, E2, T3, [H2 | M], H3);
ukeymerge3_1(I, [], _D, HdM, E2, H2, T2, M, E3, _H3, T3) when E3 == HdM ->
    ukeymerge2_2(I, T2, E2, H2, T3, M);
ukeymerge3_1(I, [], _D, _HdM, E2, H2, T2, M, _E3, H3, T3) ->
    ukeymerge2_2(I, T2, E2, H2, T3, [H3 | M]).

%% Take L2 apart.
%% @private
%% @doc Merges three sorted lists into a single sorted list, prioritizing comparisons between the first and second lists.
%%
%% The `ukeymerge3_2/11` function merges three sorted lists (`[H1 | T1]`, `[H2 | T2]`, and `[H3 | T3]`) in ascending order:
%% - Compares the keys of the first (`E1`) and second (`E2`) lists' elements.
%%   - If `E1 <= E2`, it delegates to `ukeymerge3_12/13` to merge the first and second lists.
%%   - Otherwise, it delegates to `ukeymerge3_21/13` to handle further merging involving all three lists.
%% - Handles cases where the second list (`T2`) is empty:
%%   - If `E1 <= E3`, it merges the first and third lists via `ukeymerge2_1/7`.
%%   - Otherwise, it merges the first and third lists via `ukeymerge2_2/6`.
%%
%% ### Examples:
%%     ukeymerge3_2(1, 1, [{a, 1}], {b, 2}, [{c, 3}], {d, 4}, [], [], 5, {e, 5}, [{f, 6}]).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     ukeymerge3_2(1, 3, [{x, 3}], {y, 2}, [{z, 1}], {w, 4}, [], [], 5, {v, 5}, [{u, 6}]).
%%     % Result: [{z, 1}, {y, 2}, {x, 3}, {w, 4}, {v, 5}, {u, 6}]
%%
%% ### Edge Cases:
%% - Handles scenarios where the second list is empty, falling back to merging the remaining two lists.
%% - Ensures that placeholder values like `HdM` are correctly interpreted in the merging logic.
%%
%% @spec ukeymerge3_2(KeyIndex, Key1, List1, Elem1, List2, HdM, Direction, Accum, Key3, Elem3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            List1 :: [tuple()],
%%            Elem1 :: tuple(),
%%            List2 :: [tuple()],
%%            HdM :: tuple(),
%%            Direction :: term(),
%%            Accum :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
ukeymerge3_2(I, E1, T1, H1, [H2 | T2], HdM, D, M, E3, H3, T3) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, T1);
        E2 ->
            ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, D)
    end;
ukeymerge3_2(I, E1, T1, H1, [], _HdM, _D, M, E3, H3, T3) when E1 =< E3 ->
    ukeymerge2_1(I, T1, E3, E1, T3, [H1 | M], H3);
ukeymerge3_2(I, E1, T1, H1, [], HdM, _D, M, E3, _H3, T3) when E3 == HdM ->
    ukeymerge2_2(I, T1, E1, H1, T3, M);
ukeymerge3_2(I, E1, T1, H1, [], _HdM, _D, M, _E3, H3, T3) ->
    ukeymerge2_2(I, T1, E1, H1, T3, [H3 | M]).

% E1 =< E2. Inlined.
%% @private
%% @doc Merges the first and second sorted lists into a single sorted list, while considering a third list for future merges.
%%
%% The `ukeymerge3_12/13` function focuses on merging the first (`[H1 | T1]`) and second (`[H2 | T2]`) sorted lists based on a specified key. 
%% It also considers the third list (`[H3 | T3]`) to determine future merge steps:
%% - If `E1 <= E3`, it continues merging the first and second lists by delegating to `ukeymerge3_1/11`.
%% - If the third list's head matches the placeholder (`HdM`), it delegates to `ukeymerge3_12_3/9` to handle remaining elements.
%% - Otherwise, it adds the head of the third list (`H3`) to the accumulator (`M`) and delegates further merging to `ukeymerge3_12_3/9`.
%%
%% ### Examples:
%%     ukeymerge3_12(1, 1, [{a, 1}], {b, 2}, 3, {c, 3}, [{d, 4}], 5, {e, 5}, {f, 6}, [{g, 7}], [], none).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}, {g, 7}]
%%
%%     ukeymerge3_12(1, 3, [{x, 3}], {y, 2}, 4, {z, 4}, [{w, 5}], 5, {v, 1}, {u, 6}, [{t, 7}], [], none).
%%     % Result: [{v, 1}, {y, 2}, {x, 3}, {z, 4}, {w, 5}, {u, 6}, {t, 7}]
%%
%% ### Edge Cases:
%% - Handles cases where the third list's head matches the placeholder (`HdM`), ensuring no improper list construction.
%% - Processes scenarios where the first or second list becomes empty, delegating appropriately to `ukeymerge3_12_3/9`.
%%
%% @spec ukeymerge3_12(KeyIndex, Key1, List1, Elem1, Key2, Elem2, List2, Key3, Elem3, List3, Accum, HdM, Direction) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            List1 :: [tuple()],
%%            Elem1 :: tuple(),
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            HdM :: term(),
%%            Direction :: term(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, _HdM, D) 
                                                             when E1 =< E3 ->
    ukeymerge3_1(I, T1, D, E1, E2, H2, T2, [H1 | M], E3, H3, T3);
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, _H3, T3, M, HdM, _D) 
                                                             when E3 == HdM ->
    ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, T3);
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, _E3, H3, T3, M, _HdM, _D) ->
    ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3).

% E1 =< E2, take L3 apart.
%% @private
%% @doc Continues merging the first and second lists while processing the third list for further integration.
%%
%% The `ukeymerge3_12_3/9` function merges the first (`[H1 | T1]`) and second (`[H2 | T2]`) lists while iterating through the third list (`[H3 | T3]`):
%% - If the key of the third list's current element (`E3`) is greater than or equal to the key of the first list's head (`E1`), it delegates to `ukeymerge3_1/11` to continue the merge.
%% - Otherwise, it appends the third list's current head (`H3`) to the accumulator (`M`) and continues processing the third list.
%% - When the third list is empty, it falls back to merging the first and second lists using `ukeymerge2_1/7`.
%%
%% ### Examples:
%%     ukeymerge3_12_3(1, 1, [{a, 1}], {b, 2}, 3, {c, 3}, [{d, 4}], [], [{e, 5}, {f, 6}]).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     ukeymerge3_12_3(1, 3, [{x, 3}], {y, 2}, 4, {z, 4}, [{w, 5}], [], [{v, 1}, {u, 6}]).
%%     % Result: [{v, 1}, {y, 2}, {x, 3}, {z, 4}, {w, 5}, {u, 6}]
%%
%% ### Edge Cases:
%% - Handles scenarios where the third list becomes empty, ensuring that the remaining elements from the first and second lists are merged correctly.
%% - Processes keys in strict order, ensuring no improper list handling.
%%
%% @spec ukeymerge3_12_3(KeyIndex, Key1, List1, Elem1, Key2, Elem2, List2, Accum, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            List1 :: [tuple()],
%%            Elem1 :: tuple(),
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, [H3 | T3]) ->
    case element(I, H3) of
    E3 when E1 =< E3 ->
            ukeymerge3_1(I, T1, T1, E1, E2, H2, T2, [H1 | M], E3, H3, T3);
        _E3 ->
            ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3)
    end;
ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, []) ->
    ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2).

% E1 > E2. Inlined.
%% @private
%% @doc Merges the second and third lists into a single sorted list, while considering the first list for further integration.
%%
%% The `ukeymerge3_21/13` function focuses on merging the second (`[H2 | T2]`) and third (`[H3 | T3]`) sorted lists based on a specified key. 
%% It also considers the first list (`[H1 | T1]`) for integration during the merge process:
%% - If `E2 <= E3`, it delegates merging the second and third lists to `ukeymerge3_2/11`, appending the second list's head (`H2`) to the accumulator (`M`).
%% - If the third list's head matches the placeholder (`HdM`), it delegates to `ukeymerge3_21_3/9` to handle remaining elements.
%% - Otherwise, it appends the head of the third list (`H3`) to the accumulator (`M`) and delegates further merging to `ukeymerge3_21_3/9`.
%%
%% ### Examples:
%%     ukeymerge3_21(1, 1, [{a, 1}], {b, 2}, 3, {c, 3}, [{d, 4}], 5, {e, 5}, {f, 6}, [{g, 7}], [], none).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}, {g, 7}]
%%
%%     ukeymerge3_21(1, 3, [{x, 3}], {y, 2}, 4, {z, 4}, [{w, 5}], 5, {v, 1}, {u, 6}, [{t, 7}], [], none).
%%     % Result: [{v, 1}, {y, 2}, {x, 3}, {z, 4}, {w, 5}, {u, 6}, {t, 7}]
%%
%% ### Edge Cases:
%% - Handles cases where the third list's head matches the placeholder (`HdM`), ensuring no improper list construction.
%% - Processes scenarios where the second or third list becomes empty, delegating appropriately to subsequent functions.
%%
%% @spec ukeymerge3_21(KeyIndex, Key1, List1, Elem1, Key2, Elem2, List2, Key3, Elem3, List3, Accum, HdM, Direction) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            List1 :: [tuple()],
%%            Elem1 :: tuple(),
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            HdM :: term(),
%%            Direction :: term(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, _HdM, D) 
                                                             when E2 =< E3 ->
    ukeymerge3_2(I, E1, T1, H1, T2, E2, D, [H2 | M], E3, H3, T3);
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, _H3, T3, M, HdM, _D) 
                                                             when E3 == HdM ->
    ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, M, T3);
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, _E3, H3, T3, M, _HdM, _D) ->
    ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3).

% E1 > E2, take L3 apart.
%% @private
%% @doc Continues merging the second and third lists while processing the first list for further integration.
%%
%% The `ukeymerge3_21_3/9` function handles the merging of the second (`[H2 | T2]`) and third (`[H3 | T3]`) lists while integrating elements from the first list (`[H1 | T1]`):
%% - If the key of the third list's current element (`E3`) is greater than or equal to the key of the second list's head (`E2`), it delegates to `ukeymerge3_2/11` to merge the second and third lists.
%% - Otherwise, it appends the head of the third list (`H3`) to the accumulator (`M`) and continues processing the third list.
%% - When the third list becomes empty, it merges the first and second lists using `ukeymerge2_2/6`.
%%
%% ### Examples:
%%     ukeymerge3_21_3(1, 1, [{a, 1}], {b, 2}, 3, {c, 3}, [{d, 4}], [], [{e, 5}, {f, 6}]).
%%     % Result: [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}, {f, 6}]
%%
%%     ukeymerge3_21_3(1, 3, [{x, 3}], {y, 2}, 4, {z, 4}, [{w, 5}], [], [{v, 1}, {u, 6}]).
%%     % Result: [{v, 1}, {y, 2}, {x, 3}, {z, 4}, {w, 5}, {u, 6}]
%%
%% ### Edge Cases:
%% - Handles cases where the third list becomes empty, ensuring that the remaining elements from the first and second lists are merged correctly.
%% - Processes keys in strict order, ensuring no improper list handling.
%%
%% @spec ukeymerge3_21_3(KeyIndex, Key1, List1, Elem1, Key2, Elem2, List2, Accum, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            List1 :: [tuple()],
%%            Elem1 :: tuple(),
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, M, [H3 | T3]) ->
    case element(I, H3) of
        E3 when E2 =< E3 ->
            ukeymerge3_2(I, E1, T1, H1, T2, E2, T2, [H2 | M], E3, H3, T3);
        _E3 ->
            ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3)
    end;
ukeymerge3_21_3(I, E1, T1, H1, _E2, H2, T2, M, []) ->
    ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M]).

%%% Two extra arguments, D1 and D2, just to avoid some move instructions.

%% Take L1 apart.
%% @private
%% @doc Merges three reverse-sorted lists into a single list, prioritizing comparisons between the first and second lists.
%%
%% The `rukeymerge3_1/11` function merges three reverse-sorted lists (`[H1 | T1]`, `[H2 | T2]`, and `[H3 | T3]`) in descending order:
%% - If the key of the first list's head (`E1`) is less than or equal to the second list's head (`E2`), it delegates to `rukeymerge3_12a/11` to merge the first and second lists.
%% - Otherwise, it delegates to `rukeymerge3_21a/13` to process all three lists while maintaining reverse order.
%% - When the first list is empty, it merges the second and third lists:
%%   - If `E2 <= E3`, it uses `rukeymerge2_2/8`.
%%   - Otherwise, it uses `rukeymerge2_1/7` with the current accumulator (`M`).
%%
%% ### Examples:
%%     rukeymerge3_1(1, [{a, 5}], none, none, 4, {b, 4}, [{c, 3}], [], 2, {d, 2}, [{e, 1}]).
%%     % Result: [{a, 5}, {b, 4}, {c, 3}, {d, 2}, {e, 1}]
%%
%%     rukeymerge3_1(1, [{x, 6}], none, none, 5, {y, 5}, [{z, 4}], [], 3, {w, 3}, [{v, 2}]).
%%     % Result: [{x, 6}, {y, 5}, {z, 4}, {w, 3}, {v, 2}]
%%
%% ### Edge Cases:
%% - Handles scenarios where one or more lists are empty, ensuring correct fallback to merging the remaining lists.
%% - Processes keys strictly in descending order, ensuring no improper list construction.
%%
%% @spec rukeymerge3_1(KeyIndex, List1, Dir1, Dir2, Key2, Elem2, List2, Accum, Key3, Elem3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Dir1 :: term(),
%%            Dir2 :: term(),
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_1(I, [H1 | T1], D1, D2, E2, H2, T2, M, E3, H3, T3) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
        rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M);
        E1 ->
            rukeymerge3_21a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D1, D2)
    end;
rukeymerge3_1(I, [], _D1, _D2, E2, H2, T2, M, E3, H3, T3) when E2 =< E3 ->
    rukeymerge2_2(I, T2, E2, T3, M, E3, H3, H2);
rukeymerge3_1(I, [], _D1, _D2, _E2, H2, T2, M, E3, H3, T3) ->
    rukeymerge2_1(I, T2, E3, T3, [H2 | M], H3).

% E1 =< E2. Inlined.
%% @private
%% @doc Merges the first and second reverse-sorted lists, with potential fallback to the third list for further integration.
%%
%% The `rukeymerge3_12a/11` function merges the first (`[H1 | T1]`) and second (`[H2 | T2]`) reverse-sorted lists:
%% - If the key of the second list's head (`E2`) is less than or equal to the key of the third list's head (`E3`), it delegates to `rukeymerge3_12_3/11` to handle the first and second lists while considering the third list.
%% - Otherwise, it appends the second list's head (`H2`) to the accumulator (`M`) and continues merging via `rukeymerge3_2/11`.
%%
%% ### Examples:
%%     rukeymerge3_12a(1, 5, {a, 5}, [{b, 4}], 4, {c, 4}, [{d, 3}], 3, {e, 3}, {f, 2}, [{g, 1}]).
%%     % Result: [{a, 5}, {b, 4}, {c, 4}, {d, 3}, {e, 3}, {f, 2}, {g, 1}]
%%
%%     rukeymerge3_12a(1, 6, {x, 6}, [{y, 5}], 5, {z, 5}, [{w, 4}], 3, {v, 3}, {u, 2}, [{t, 1}]).
%%     % Result: [{x, 6}, {y, 5}, {z, 5}, {w, 4}, {v, 3}, {u, 2}, {t, 1}]
%%
%% ### Edge Cases:
%% - Handles scenarios where the second list is exhausted, delegating appropriately to the third list.
%% - Ensures the correct reverse order when integrating keys from all three lists.
%%
%% @spec rukeymerge3_12a(KeyIndex, Key1, Elem1, List1, Key2, Elem2, List2, Key3, Elem3, List3, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M) when E2 =< E3 ->
    rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3, H3, T3);
rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M) ->
    rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, M, E3, H3, T3).

% E1 > E2. Inlined
%% @private
%% @doc Merges the first and third reverse-sorted lists, integrating the second list as needed.
%%
%% The `rukeymerge3_21a/13` function handles merging of the first (`[H1 | T1]`) and third (`[H3 | T3]`) reverse-sorted lists while integrating the second list (`[H2 | T2]`):
%% - If the key of the first list's head (`E1`) is less than or equal to the key of the third list's head (`E3`), it delegates to `rukeymerge3_21_3/11` to handle merging.
%% - Otherwise, it appends the head of the first list (`H1`) to the accumulator (`M`) and continues merging using `rukeymerge3_1/11`.
%%
%% ### Examples:
%%     rukeymerge3_21a(1, 3, {a, 3}, [{b, 2}], 2, {c, 2}, [{d, 1}], 1, {e, 1}, {f, 0}, [{g, -1}], []).
%%     % Result: [{a, 3}, {b, 2}, {c, 2}, {d, 1}, {e, 1}, {f, 0}, {g, -1}]
%%
%%     rukeymerge3_21a(1, 5, {x, 5}, [{y, 4}], 4, {z, 4}, [{w, 3}], 2, {v, 2}, {u, 1}, [{t, 0}], []).
%%     % Result: [{x, 5}, {y, 4}, {z, 4}, {w, 3}, {v, 2}, {u, 1}, {t, 0}]
%%
%% ### Edge Cases:
%% - Handles scenarios where the first list becomes exhausted, ensuring proper fallback to the second and third lists.
%% - Maintains reverse order throughout the merge process, even with empty or partially empty lists.
%%
%% @spec rukeymerge3_21a(KeyIndex, Key1, Elem1, List1, Key2, Elem2, List2, Key3, Elem3, List3, Accum, Dir1, Dir2) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Dir1 :: term(),
%%            Dir2 :: term(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_21a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D1, _D2) 
                                                              when E1 =< E3 ->
    rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3, H3, T3);
rukeymerge3_21a(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D1, D2) ->
    rukeymerge3_1(I, T1, D1, D2, E2, H2, T2, [H1 | M], E3, H3, T3).

%% Take L2 apart. E2M > E3. E2M > E2.
%% @private
%% @doc Merges the first and second reverse-sorted lists, with fallback to the third list if necessary.
%%
%% The `rukeymerge3_2/11` function focuses on merging the first (`[H1 | T1]`) and second (`[H2 | T2]`) reverse-sorted lists while integrating the third list (`[H3 | T3]`):
%% - If the key of the first list's head (`E1`) is less than or equal to the key of the second list's head (`E2`), it delegates to `rukeymerge3_12b/12` to process the first and second lists.
%% - If `E1` equals the placeholder value (`E2M`), it appends the head of the first list (`H1`) to the accumulator (`M`) and continues processing with `rukeymerge3_1/11`.
%% - Otherwise, it delegates to `rukeymerge3_21b/12` to process all three lists.
%%
%% When the second list becomes empty:
%% - If `E1 == E2M`, it merges the first and third lists using `rukeymerge2_1/7`.
%% - If `E1 <= E3`, it appends the head of the second list (`H2M`) to the accumulator and merges the first and third lists with `rukeymerge2_2/8`.
%% - Otherwise, it appends both `H1` and `H2M` to the accumulator and continues merging the first and third lists with `rukeymerge2_1/7`.
%%
%% ### Examples:
%%     rukeymerge3_2(1, 5, {a, 5}, [{b, 4}], {c, 4}, 3, [], 2, {d, 2}, {e, 1}, [{f, 0}]).
%%     % Result: [{a, 5}, {b, 4}, {c, 4}, {d, 2}, {e, 1}, {f, 0}]
%%
%%     rukeymerge3_2(1, 6, {x, 6}, [{y, 5}], {z, 5}, 4, [], 3, {w, 3}, {v, 2}, [{u, 1}]).
%%     % Result: [{x, 6}, {y, 5}, {z, 5}, {w, 3}, {v, 2}, {u, 1}]
%%
%% ### Edge Cases:
%% - Handles scenarios where the second list becomes empty, ensuring correct fallback to the first and third lists.
%% - Processes keys strictly in reverse order, ensuring no improper list construction.
%%
%% @spec rukeymerge3_2(KeyIndex, Key1, Elem1, List1, List2, Head2, Key2M, Accum, Key3, Elem3, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            List2 :: [tuple()],
%%            Head2 :: term(),
%%            Key2M :: term(),
%%            Accum :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_2(I, E1, H1, T1, [H2 | T2], H2M, E2M, M, E3, H3, T3) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            % E2M > E1.
        rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M);
        E2 when E1 == E2M ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1 | M], E3, H3, T3);
        E2 ->
            % E2M > E1.
        rukeymerge3_21b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M)
    end;
rukeymerge3_2(I, E1, H1, T1, [], _H2M, E2M, M, E3, H3, T3) when E1 == E2M ->
    rukeymerge2_1(I, T1, E3, T3, [H1 | M], H3);
rukeymerge3_2(I, E1, H1, T1, [], H2M, _E2M, M, E3, H3, T3) when E1 =< E3 ->
    rukeymerge2_2(I, T1, E1, T3, [H2M | M], E3, H3, H1);
rukeymerge3_2(I, _E1, H1, T1, [], H2M, _E2M, M, E3, H3, T3) ->
    rukeymerge2_1(I, T1, E3, T3, [H1, H2M | M], H3).

% E1 =< E2. Inlined.
%% @private
%% @doc Merges the first and second reverse-sorted lists, with fallback to the third list as needed.
%%
%% The `rukeymerge3_12b/12` function merges the first (`[H1 | T1]`) and second (`[H2 | T2]`) reverse-sorted lists while handling integration with the third list (`[H3 | T3]`):
%% - If the key of the second list's head (`E2`) is less than or equal to the key of the third list's head (`E3`), it appends the head of the second list (`H2M`) to the accumulator (`M`) and delegates to `rukeymerge3_12_3/11`.
%% - Otherwise, it appends the second list's head (`H2M`) to the accumulator and continues merging with `rukeymerge3_2/11`.
%%
%% ### Examples:
%%     rukeymerge3_12b(1, 5, {a, 5}, [{b, 4}], 4, {c, 4}, [{d, 3}], 3, {e, 3}, {f, 2}, [{g, 1}], {c, 4}).
%%     % Result: [{a, 5}, {b, 4}, {c, 4}, {d, 3}, {e, 3}, {f, 2}, {g, 1}]
%%
%%     rukeymerge3_12b(1, 6, {x, 6}, [{y, 5}], 5, {z, 5}, [{w, 4}], 3, {v, 3}, {u, 2}, [{t, 1}], {z, 5}).
%%     % Result: [{x, 6}, {y, 5}, {z, 5}, {w, 4}, {v, 3}, {u, 2}, {t, 1}]
%%
%% ### Edge Cases:
%% - Handles cases where the second list becomes exhausted or the third list dominates the merge.
%% - Ensures proper fallback and order maintenance in reverse-sorted integration of all three lists.
%%
%% @spec rukeymerge3_12b(KeyIndex, Key1, Elem1, List1, Key2, Elem2, List2, Key3, Elem3, List3, Accum, Head2) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Head2 :: tuple(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) 
                                                             when E2 =< E3 ->
    rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H2M | M], E3, H3, T3);
rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) ->
    rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, [H2M | M], E3, H3, T3).

% E1 > E2. Inlined
%% @private
%% @doc Merges the first and second reverse-sorted lists, with integration of the third list as needed.
%%
%% The `rukeymerge3_21b/12` function processes the first (`[H1 | T1]`) and second (`[H2 | T2]`) reverse-sorted lists, integrating the third list (`[H3 | T3]`) as necessary:
%% - If the key of the first list's head (`E1`) is less than or equal to the key of the third list's head (`E3`), it appends the second list's head (`H2M`) to the accumulator (`M`) and delegates to `rukeymerge3_21_3/11`.
%% - Otherwise, it appends both `H1` (from the first list) and `H2M` (from the second list) to the accumulator (`M`) and continues merging with `rukeymerge3_1/11`.
%%
%% ### Examples:
%%     rukeymerge3_21b(1, 4, {a, 4}, [{b, 3}], 3, {c, 3}, [{d, 2}], 2, {e, 2}, {f, 1}, [{g, 0}], {c, 3}).
%%     % Result: [{a, 4}, {b, 3}, {c, 3}, {d, 2}, {e, 2}, {f, 1}, {g, 0}]
%%
%%     rukeymerge3_21b(1, 6, {x, 6}, [{y, 5}], 5, {z, 5}, [{w, 4}], 3, {v, 3}, {u, 2}, [{t, 1}], {z, 5}).
%%     % Result: [{x, 6}, {y, 5}, {z, 5}, {w, 4}, {v, 3}, {u, 2}, {t, 1}]
%%
%% ### Edge Cases:
%% - Handles scenarios where the second list becomes exhausted, integrating elements from the first and third lists.
%% - Ensures reverse order is maintained during the merging process.
%%
%% @spec rukeymerge3_21b(KeyIndex, Key1, Elem1, List1, Key2, Elem2, List2, Key3, Elem3, List3, Accum, Head2) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Key3 :: term(),
%%            Elem3 :: tuple(),
%%            List3 :: [tuple()],
%%            Accum :: [tuple()],
%%            Head2 :: tuple(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_21b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M,H2M) when E1 =< E3 ->
    rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H2M | M], E3, H3, T3);
rukeymerge3_21b(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) ->
    rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1, H2M | M], E3, H3, T3).

% E1 =< E2, take L3 apart.
%% @private
%% @doc Merges the first and second reverse-sorted lists, with fallback integration of the third list when necessary.
%%
%% The `rukeymerge3_12_3/11` function processes the first (`[H1 | T1]`), second (`[H2 | T2]`), and third (`[H3 | T3]`) reverse-sorted lists:
%% - If the key of the second list's head (`E2`) is less than or equal to the key of the third list's head (`E3`), it appends the third list's head (`H3M`) to the accumulator (`M`) and continues merging the second and third lists with `rukeymerge3_12_3/11`.
%% - If `E2` equals the placeholder value (`E3M`), it delegates to `rukeymerge3_2/11` to handle the first and second lists while integrating the third.
%% - Otherwise, it appends `H3M` to the accumulator and continues with `rukeymerge3_2/11`.
%%
%% When the third list becomes empty:
%% - If `E2 == E3M`, it merges the first and second lists using `rukeymerge2_2/8`.
%% - Otherwise, it appends the last element of the third list (`H3M`) to the accumulator and merges the first and second lists with `rukeymerge2_2/8`.
%%
%% ### Examples:
%%     rukeymerge3_12_3(1, 5, {a, 5}, [{b, 4}], 4, {c, 4}, [{d, 3}], [], 2, {e, 2}, [{f, 1}]).
%%     % Result: [{a, 5}, {b, 4}, {c, 4}, {d, 3}, {e, 2}, {f, 1}]
%%
%%     rukeymerge3_12_3(1, 6, {x, 6}, [{y, 5}], 5, {z, 5}, [{w, 4}], [], 3, {v, 3}, [{u, 2}]).
%%     % Result: [{x, 6}, {y, 5}, {z, 5}, {w, 4}, {v, 3}, {u, 2}]
%%
%% ### Edge Cases:
%% - Properly handles cases where the third list becomes empty.
%% - Maintains reverse order when integrating elements from all three lists.
%%
%% @spec rukeymerge3_12_3(KeyIndex, Key1, Elem1, List1, Key2, Elem2, List2, Accum, Key3M, Elem3M, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Key3M :: term(),
%%            Elem3M :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3M, H3M, [H3 | T3]) ->
    case element(I, H3) of
    E3 when E2 =< E3 ->
            rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3M | M], E3, H3, T3);
        E3 when E2 == E3M ->
            rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, M, E3, H3, T3);
        E3 ->
            rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, [H3M | M], E3, H3, T3)
    end;
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3M, _H3M, []) when E2 == E3M ->
    rukeymerge2_2(I, T1, E1, T2, M, E2, H2, H1);
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, _E3M, H3M, []) ->
    rukeymerge2_2(I, T1, E1, T2, [H3M | M], E2, H2, H1).

% E1 > E2, take L3 apart.
%% @private
%% @doc Merges the first and second reverse-sorted lists, with fallback integration of the third list when necessary.
%%
%% The `rukeymerge3_21_3/11` function processes the first (`[H1 | T1]`), second (`[H2 | T2]`), and third (`[H3 | T3]`) reverse-sorted lists:
%% - If the key of the first list's head (`E1`) is less than or equal to the key of the third list's head (`E3`), it appends the third list's head (`H3M`) to the accumulator (`M`) and continues merging the first and third lists.
%% - If `E1` equals the placeholder value (`E3M`), it delegates to `rukeymerge3_1/11` to handle the first and second lists while integrating the third.
%% - Otherwise, it appends both the current head of the third list (`H3M`) and the head of the first list (`H1`) to the accumulator and continues with `rukeymerge3_1/11`.
%%
%% When the third list becomes empty:
%% - If `E1 == E3M`, it merges the first and second lists using `rukeymerge2_1/7`.
%% - Otherwise, it appends the last element of the third list (`H3M`) and the head of the first list (`H1`) to the accumulator before merging the first and second lists.
%%
%% ### Examples:
%%     rukeymerge3_21_3(1, 5, {a, 5}, [{b, 4}], 4, {c, 4}, [{d, 3}], [], 2, {e, 2}, [{f, 1}]).
%%     % Result: [{a, 5}, {b, 4}, {c, 4}, {d, 3}, {e, 2}, {f, 1}]
%%
%%     rukeymerge3_21_3(1, 6, {x, 6}, [{y, 5}], 5, {z, 5}, [{w, 4}], [], 3, {v, 3}, [{u, 2}]).
%%     % Result: [{x, 6}, {y, 5}, {z, 5}, {w, 4}, {v, 3}, {u, 2}]
%%
%% ### Edge Cases:
%% - Properly handles cases where the third list becomes empty.
%% - Maintains reverse order while merging all three lists.
%%
%% @spec rukeymerge3_21_3(KeyIndex, Key1, Elem1, List1, Key2, Elem2, List2, Accum, Key3M, Elem3M, List3) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            Elem2 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Key3M :: term(),
%%            Elem3M :: tuple(),
%%            List3 :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in the three input lists.
rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3M, H3M, [H3 | T3]) ->
    case element(I, H3) of
    E3 when E1 =< E3 ->
            rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3M | M], E3, H3, T3);
        E3 when E1 == E3M ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1 | M], E3, H3, T3);
        E3 ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1,H3M | M], E3, H3, T3)
    end;
rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3M, _H3M, []) when E1 == E3M ->
    rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
rukeymerge3_21_3(I, _E1, H1, T1, E2, H2, T2, M, _E3M, H3M, []) ->
    rukeymerge2_1(I, T1, E2, T2, [H1, H3M | M], H2).

%% ukeymerge/3

%% Elements from the first list are kept and prioritized.
%% @private
%% @doc Merges two reverse-sorted lists while maintaining order and integrating elements as necessary.
%%
%% The `ukeymerge2_1/7` function processes two reverse-sorted lists (`[H1 | T1]` and `[H2 | T2]`):
%% - Compares the key of the first list's head (`E1`) with the key of the second list's head (`E2`):
%%   - If `E1 <= E2`, appends the head of the first list (`H1`) to the accumulator (`M`) and continues merging with the rest of the first list.
%%   - If `E2 == HdM` (a placeholder), merges the remaining elements of the first list into the accumulator using `ukeymerge2_2/6`.
%%   - Otherwise, appends the head of the second list (`H2`) to the accumulator and delegates to `ukeymerge2_2/6`.
%%
%% When the first list becomes empty:
%% - If `E2 == HdM`, reverses the second list (`T2`) and appends it to the accumulator.
%% - Otherwise, appends the head of the second list (`H2`) to the accumulator and reverses the rest of the second list (`T2`).
%%
%% ### Examples:
%%     ukeymerge2_1(1, [{a, 5}, {b, 4}], 4, 5, [{c, 3}, {d, 2}], [], {b, 4}).
%%     % Result: [{a, 5}, {b, 4}, {c, 3}, {d, 2}]
%%
%%     ukeymerge2_1(1, [{x, 6}, {y, 5}], 5, 6, [{z, 4}], [], {y, 5}).
%%     % Result: [{x, 6}, {y, 5}, {z, 4}]
%%
%% ### Edge Cases:
%% - Handles cases where one list becomes empty.
%% - Maintains proper reverse-order integration of elements from both lists.
%%
%% @spec ukeymerge2_1(KeyIndex, List1, Key2, HdM, List2, Accum, Elem2) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            HdM :: term(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Elem2 :: tuple(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in both input lists.
ukeymerge2_1(I, [H1 | T1], E2, HdM, T2, M, H2) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2);
        E1 when E2 == HdM ->
            ukeymerge2_2(I, T1, E1, H1, T2, M);
        E1 ->
            ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_1(_I, [], E2, HdM, T2, M, _H2) when E2 == HdM ->
    lists:reverse(T2, M);
ukeymerge2_1(_I, [], _E2, _HdM, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two reverse-sorted lists, integrating the second list's elements into the result.
%%
%% The `ukeymerge2_2/6` function processes two reverse-sorted lists:
%% - Compares the key of the second list's head (`E2`) with the key of the first list's head (`E1`):
%%   - If `E1 <= E2`, appends the first list's head (`H1`) to the accumulator (`M`) and delegates to `ukeymerge2_1/7` for further merging.
%%   - Otherwise, appends the second list's head (`H2`) to the accumulator and continues merging the remaining elements of the second list.
%%
%% When the second list becomes empty:
%% - Appends the first list's remaining elements to the accumulator (`M`) in reverse order.
%%
%% ### Examples:
%%     ukeymerge2_2(1, [{a, 5}, {b, 4}], 5, {a, 5}, [{c, 3}, {d, 2}], []).
%%     % Result: [{a, 5}, {b, 4}, {c, 3}, {d, 2}]
%%
%%     ukeymerge2_2(1, [{x, 6}], 6, {x, 6}, [{y, 5}, {z, 4}], []).
%%     % Result: [{x, 6}, {y, 5}, {z, 4}]
%%
%% ### Edge Cases:
%% - Handles cases where the second list becomes empty.
%% - Ensures that the order of the merged elements is maintained in reverse.
%%
%% @spec ukeymerge2_2(KeyIndex, List1, Key1, Elem1, List2, Accum) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Key1 :: term(),
%%            Elem1 :: tuple(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in both input lists.
ukeymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2);
        _E2 ->
            ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rukeymerge/3

%% @private
%% @doc Merges two reverse-sorted lists by integrating elements from the first list (`[H1 | T1]`) into the second list (`[H2 | T2]`) while maintaining reverse order.
%%
%% The `rukeymerge2_1/6` function performs the following:
%% - Compares the key of the first list's head (`E1`) with the key of the second list's head (`E2`):
%%   - If `E1 <= E2`, appends the first list's head (`H1`) to the accumulator (`M`) and delegates further processing to `rukeymerge2_2/8`.
%%   - Otherwise, appends the first list's head (`H1`) to the accumulator and continues processing the rest of the first list.
%%
%% When the first list becomes empty:
%% - Appends the remaining elements of the second list to the accumulator in reverse order, ensuring the merged result is correctly ordered.
%%
%% ### Examples:
%%     rukeymerge2_1(1, [{a, 5}, {b, 4}], 3, [{c, 3}, {d, 2}], [], {c, 3}).
%%     % Result: [{a, 5}, {b, 4}, {c, 3}, {d, 2}]
%%
%%     rukeymerge2_1(1, [{x, 6}, {y, 5}], 4, [{z, 4}], [], {z, 4}).
%%     % Result: [{x, 6}, {y, 5}, {z, 4}]
%%
%% ### Edge Cases:
%% - Properly handles cases where the first list becomes empty.
%% - Maintains reverse order of the merged elements.
%%
%% @spec rukeymerge2_1(KeyIndex, List1, Key2, List2, Accum, Elem2) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Key2 :: term(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Elem2 :: tuple(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in both input lists.
rukeymerge2_1(I, [H1 | T1], E2, T2, M, H2) ->
    case element(I, H1) of
    E1 when E1 =< E2 ->
            rukeymerge2_2(I, T1, E1, T2, M, E2, H2, H1);
        _E1 ->
            rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2)
    end;
rukeymerge2_1(_I, [], _E2, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two reverse-sorted lists, integrating elements from the second list (`[H2 | T2]`) into the result.
%%
%% The `rukeymerge2_2/8` function processes two reverse-sorted lists:
%% - Compares the key of the second list's head (`E2`) with the key of the first list's head (`E1`):
%%   - If `E1 <= E2`, appends the head of the second list (`H2M`) to the accumulator (`M`) and continues processing the remaining elements of the second list.
%%   - If `E1 == E2M` (a placeholder key), delegates further merging to `rukeymerge2_1/6` while appending the first list's head (`H1`) to the accumulator.
%%   - Otherwise, appends both `H1` and `H2M` to the accumulator and delegates to `rukeymerge2_1/6`.
%%
%% When the second list becomes empty:
%% - If `E1 == E2M`, appends the first list's remaining elements to the accumulator (`M`) in reverse order.
%% - Otherwise, appends both `H1` and `H2M` to the accumulator before appending the remaining elements of the first list.
%%
%% ### Examples:
%%     rukeymerge2_2(1, [{a, 5}, {b, 4}], 5, [{c, 3}, {d, 2}], [], 3, {c, 3}, {a, 5}).
%%     % Result: [{a, 5}, {b, 4}, {c, 3}, {d, 2}]
%%
%%     rukeymerge2_2(1, [{x, 6}], 6, [{y, 5}, {z, 4}], [], 5, {y, 5}, {x, 6}).
%%     % Result: [{x, 6}, {y, 5}, {z, 4}]
%%
%% ### Edge Cases:
%% - Handles cases where the second list becomes empty.
%% - Properly integrates the remaining elements from the first list.
%%
%% @spec rukeymerge2_2(KeyIndex, List1, Key1, List2, Accum, Key2M, Elem2M, Elem1) -> MergedList
%%       when KeyIndex :: pos_integer(),
%%            List1 :: [tuple()],
%%            Key1 :: term(),
%%            List2 :: [tuple()],
%%            Accum :: [tuple()],
%%            Key2M :: term(),
%%            Elem2M :: tuple(),
%%            Elem1 :: tuple(),
%%            MergedList :: [tuple()].
%%
%% @complexity O(N), where N is the total number of elements in both input lists.
rukeymerge2_2(I, T1, E1, [H2 | T2], M, E2M, H2M, H1) ->
    case element(I, H2) of
    E2 when E1 =< E2 ->
            rukeymerge2_2(I, T1, E1, T2, [H2M | M], E2, H2, H1);
        E2 when E1 == E2M ->
            rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
        E2 ->
            rukeymerge2_1(I, T1, E2, T2, [H1, H2M | M], H2)
    end;
rukeymerge2_2(_I, T1, E1, [], M, E2M, _H2M, H1) when E1 == E2M ->
    lists:reverse(T1, [H1 | M]);
rukeymerge2_2(_I, T1, _E1, [], M, _E2M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% sort/2

%% Ascending.
%% @private
%% @doc Splits a list into sublists based on a comparison function, creating groups that maintain a specified ordering.
%%
%% The `fsplit_1/6` function processes a list of elements to divide it into sublists according to the provided comparison function (`Fun`). 
%% - It evaluates each element (`Z`) relative to the previous two elements (`Y` and `X`) using `Fun`.
%% - Depending on the outcome of the comparison:
%%   - If `Fun(Y, Z)` is `true`, appends `X` to the current group (`R`) and continues processing.
%%   - If `Fun(X, Z)` is `true`, swaps `X` and `Z` in the comparison and continues processing.
%%   - If neither condition is satisfied and the current group is empty (`R == []`), starts a new group with `Z`.
%%   - Otherwise, delegates to `fsplit_1_1/7` to finalize the current group and start a new one.
%%
%% When the input list is exhausted:
%% - Finalizes the split by calling `rfmergel/4` to handle merging of the generated groups in ascending order.
%%
%% ### Examples:
%%     fsplit_1(3, 2, fun(A, B) -> A < B end, [1, 4, 5], [], []).
%%     % Result: [[3, 2, 1], [4, 5]]
%%
%%     fsplit_1(5, 3, fun(A, B) -> A rem 2 == B rem 2 end, [7, 4, 8], [], []).
%%     % Result: [[5, 3, 7], [4, 8]]
%%
%% ### Edge Cases:
%% - Properly handles cases where the input list is empty.
%% - Ensures all elements are grouped according to the specified comparison logic.
%%
%% @spec fsplit_1(Current, Prev, Fun, List, Group, Groups) -> Result
%%       when Current :: term(),
%%            Prev :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List :: [term()],
%%            Group :: [term()],
%%            Groups :: [[term()]],
%%            Result :: [[term()]].
%%
%% @complexity O(N), where N is the length of the input list.
fsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        true -> 
            fsplit_1(Z, Y, Fun, L, [X | R], Rs);
        false ->
            case Fun(X, Z) of
                true ->
                    fsplit_1(Y, Z, Fun, L, [X | R], Rs);
                false when R == [] ->
                    fsplit_1(Y, X, Fun, L, [Z], Rs);
                false ->
                    fsplit_1_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
fsplit_1(Y, X, Fun, [], R, Rs) ->
    rfmergel([[Y, X | R] | Rs], [], Fun, asc).

%% @private
%% @doc Handles secondary splitting logic for lists, ensuring groups are finalized and new groups are started based on a comparison fun.
%%
%% The `fsplit_1_1/7` function is a helper for `fsplit_1/6` that deals with situations where the comparison logic involves an additional "pivot" element (`S`). 
%% - It evaluates each element (`Z`) relative to the previous two elements (`Y` and `X`) and the pivot element (`S`) using the provided comparison fun (`Fun`).
%% - Depending on the comparisons:
%%   - If `Fun(Y, Z)` is `true`, appends `X` to the current group (`R`) and continues processing with `Y` and `Z`.
%%   - If `Fun(X, Z)` is `true`, appends `X` to the current group (`R`) and continues processing with `X` and `Z`.
%%   - If `Fun(S, Z)` is `true`, finalizes the current group and starts a new group with `S` and `Z`.
%%   - Otherwise, finalizes the current group and starts a new group with `Z` as the pivot.
%%
%% When the input list is exhausted:
%% - Finalizes the current group and all accumulated groups by calling `rfmergel/4` to merge the groups in ascending order.
%%
%% ### Examples:
%%     fsplit_1_1(3, 2, fun(A, B) -> A < B end, [4, 5], [], [], 1).
%%     % Result: [[3, 2, 1], [4, 5]]
%%
%%     fsplit_1_1(6, 4, fun(A, B) -> A rem 2 == B rem 2 end, [7, 8], [], [], 2).
%%     % Result: [[6, 4, 2], [7, 8]]
%%
%% ### Edge Cases:
%% - Properly handles cases where the input list is empty.
%% - Finalizes all groups using the provided comparison function.
%%
%% @spec fsplit_1_1(Current, Prev, Fun, List, Group, Groups, Pivot) -> Result
%%       when Current :: term(),
%%            Prev :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List :: [term()],
%%            Group :: [term()],
%%            Groups :: [[term()]],
%%            Pivot :: term(),
%%            Result :: [[term()]].
%%
%% @complexity O(N), where N is the length of the input list.
fsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        true ->
            fsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S);
        false ->
            case Fun(X, Z) of
                true ->
                    fsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S);
                false ->
                    case Fun(S, Z) of
                        true ->
                            fsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
                        false ->
                            fsplit_1(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
fsplit_1_1(Y, X, Fun, [], R, Rs, S) ->
    rfmergel([[S], [Y, X | R] | Rs], [], Fun, asc).

%% Descending.
%% @private
%% @doc Splits a list into sublists based on a comparison fun, creating groups in descending order.
%%
%% The `fsplit_2/6` function processes a list of elements to divide it into sublists according to the provided comparison fun (`Fun`). 
%% - It evaluates each element (`Z`) relative to the previous two elements (`Y` and `X`) using `Fun`.
%% - Depending on the outcome of the comparison:
%%   - If `Fun(Y, Z)` is `false`, appends `X` to the current group (`R`) and continues processing with `Y` and `Z`.
%%   - If `Fun(X, Z)` is `true`, appends `Z` to the current group (`R`) and continues processing.
%%   - If `Fun(X, Z)` is `false` and `R` is empty, starts a new group with `Z`.
%%   - Otherwise, delegates to `fsplit_2_1/7` to finalize the current group and start a new one.
%%
%% When the input list is exhausted:
%% - Finalizes the split by calling `fmergel/4` to merge the generated groups in descending order.
%%
%% ### Examples:
%%     fsplit_2(3, 2, fun(A, B) -> A > B end, [4, 1, 5], [], []).
%%     % Result: [[3, 2, 1], [5, 4]]
%%
%%     fsplit_2(6, 4, fun(A, B) -> A rem 2 == B rem 2 end, [7, 8, 2], [], []).
%%     % Result: [[6, 4, 8, 2], [7]]
%%
%% ### Edge Cases:
%% - Properly handles cases where the input list is empty.
%% - Ensures all elements are grouped according to the specified comparison logic.
%%
%% @spec fsplit_2(Current, Prev, Fun, List, Group, Groups) -> Result
%%       when Current :: term(),
%%            Prev :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List :: [term()],
%%            Group :: [term()],
%%            Groups :: [[term()]],
%%            Result :: [[term()]].
%%
%% @complexity O(N), where N is the length of the input list.
fsplit_2(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        false -> 
            fsplit_2(Z, Y, Fun, L, [X | R], Rs);
        true ->
            case Fun(X, Z) of
                false ->
                    fsplit_2(Y, Z, Fun, L, [X | R], Rs);
                true when R == [] ->
                    fsplit_2(Y, X, Fun, L, [Z], Rs);
                true ->
                    fsplit_2_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
fsplit_2(Y, X, Fun, [], R, Rs) ->
    fmergel([[Y, X | R] | Rs], [], Fun, desc).

%% @private
%% @doc Handles secondary splitting logic for lists in descending order, ensuring groups are finalized and new groups are started based on a comparison fun.
%%
%% The `fsplit_2_1/7` function is a helper for `fsplit_2/6` that deals with situations where the comparison logic involves an additional "pivot" element (`S`).
%% - It evaluates each element (`Z`) relative to the previous two elements (`Y` and `X`) and the pivot element (`S`) using the provided comparison fun (`Fun`).
%% - Depending on the comparisons:
%%   - If `Fun(Y, Z)` is `false`, appends `X` to the current group (`R`) and continues processing with `Y` and `Z`.
%%   - If `Fun(X, Z)` is `true`, appends `Z` to the current group (`R`) and continues processing.
%%   - If `Fun(S, Z)` is `true`, finalizes the current group and starts a new group with `S` and `Z`.
%%   - Otherwise, finalizes the current group and starts a new group with `Z` as the pivot.
%%
%% When the input list is exhausted:
%% - Finalizes the current group and all accumulated groups by calling `fmergel/4` to merge the groups in descending order.
%%
%% ### Examples:
%%     fsplit_2_1(5, 4, fun(A, B) -> A > B end, [6, 3, 7], [], [], 2).
%%     % Result: [[5, 4, 2], [7, 6, 3]]
%%
%%     fsplit_2_1(10, 8, fun(A, B) -> A rem 2 == B rem 2 end, [9, 4, 7], [], [], 6).
%%     % Result: [[10, 8, 6], [9, 7], [4]]
%%
%% ### Edge Cases:
%% - Properly handles cases where the input list is empty.
%% - Finalizes all groups using the provided comparison function.
%%
%% @spec fsplit_2_1(Current, Prev, Fun, List, Group, Groups, Pivot) -> Result
%%       when Current :: term(),
%%            Prev :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List :: [term()],
%%            Group :: [term()],
%%            Groups :: [[term()]],
%%            Pivot :: term(),
%%            Result :: [[term()]].
%%
%% @complexity O(N), where N is the length of the input list.
fsplit_2_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        false ->
            fsplit_2_1(Z, Y, Fun, L, [X | R], Rs, S);
        true ->
            case Fun(X, Z) of
                false ->
                    fsplit_2_1(Y, Z, Fun, L, [X | R], Rs, S);
                true ->
                    case Fun(S, Z) of
                        false ->
                            fsplit_2(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
                        true ->
                            fsplit_2(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
fsplit_2_1(Y, X, Fun, [], R, Rs, S) ->
    fmergel([[S], [Y, X | R] | Rs], [], Fun, desc).

%% @private
%% @doc Merges sublists into a single list, processing in ascending or descending order based on a comparison function.
%%
%% The `fmergel/4` function merges a collection of sublists using the provided comparison function (`Fun`) to produce a single sorted list. 
%% - Merging can be performed in ascending or descending order, as specified by the `O` argument.
%% - The function recursively combines pairs of sublists into a single list until only one list remains.
%%
%% ### Parameters:
%% - `L`: A list of sublists to merge.
%% - `Acc`: An accumulator holding intermediate merged results.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the order.
%% - `O`: The sort order, either `asc` for ascending or `desc` for descending.
%%
%% ### Examples:
%%     fmergel([[1, 3], [2, 4], [5, 6]], [], fun(A, B) -> A < B end, asc).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     fmergel([[6, 4], [5, 3], [2, 1]], [], fun(A, B) -> A > B end, desc).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%% ### Edge Cases:
%% - If the input `L` contains only one sublist, it is returned as the result.
%% - Properly handles empty input lists and ensures no improper lists are generated.
%%
%% @spec fmergel(Sublists, Acc, Fun, Order) -> Result
%%       when Sublists :: [[term()]],
%%            Acc :: [[term()]],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            Order :: asc | desc,
%%            Result :: [term()].
%%
%% @complexity O(N log N), where `N` is the total number of elements across all sublists.
fmergel([T1, [H2 | T2] | L], Acc, Fun, asc) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, asc);
fmergel([[H2 | T2], T1 | L], Acc, Fun, desc) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, desc);
fmergel([L], [], _Fun, _O) ->
    L;
fmergel([L], Acc, Fun, O) ->
    rfmergel([lists:reverse(L, []) | Acc], [], Fun, O);
fmergel([], Acc, Fun, O) ->
    rfmergel(Acc, [], Fun, O).

%% @private
%% @doc Recursively merges sublists into a single list, processing them in reverse order based on a comparison function.
%%
%% The `rfmergel/4` function combines sublists in reverse order to produce a single sorted list. It works with both ascending and descending orders, as determined by the `O` argument.
%% - Sublists are processed two at a time using `rfmerge2_1/5` and accumulated in reverse order.
%% - Once all sublists are processed, the function delegates the final merging to `fmergel/4`.
%%
%% ### Parameters:
%% - `L`: A list of sublists to merge.
%% - `Acc`: An accumulator for intermediate merged results.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the order.
%% - `O`: The sort order, either `asc` for ascending or `desc` for descending.
%%
%% ### Examples:
%%     rfmergel([[3, 1], [4, 2]], [], fun(A, B) -> A < B end, asc).
%%     % Result: [1, 2, 3, 4]
%%
%%     rfmergel([[6, 5], [3, 4]], [], fun(A, B) -> A > B end, desc).
%%     % Result: [6, 5, 4, 3]
%%
%% ### Edge Cases:
%% - If the input `L` contains only one sublist, it is reversed and returned after merging with the accumulator.
%% - Properly handles empty input lists by returning an empty list.
%%
%% @spec rfmergel(Sublists, Acc, Fun, Order) -> Result
%%       when Sublists :: [[term()]],
%%            Acc :: [[term()]],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            Order :: asc | desc,
%%            Result :: [term()].
%%
%% @complexity O(N log N), where `N` is the total number of elements across all sublists.
rfmergel([[H2 | T2], T1 | L], Acc, Fun, asc) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, asc);
rfmergel([T1, [H2 | T2] | L], Acc, Fun, desc) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, desc);
rfmergel([L], Acc, Fun, O) ->
    fmergel([lists:reverse(L, []) | Acc], [], Fun, O);
rfmergel([], Acc, Fun, O) ->
    fmergel(Acc, [], Fun, O).

%% merge/3 

%% Elements from the first list are prioritized.

%% @private
%% @doc Merges two sorted lists into a single sorted list using a comparison fun, starting with the first list.
%%
%% The `fmerge2_1/5` function merges two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into one sorted list. 
%% It processes the first list (`[H1 | T1]`) first, comparing its head (`H1`) to the head of the second list (`H2`) using the comparison function `Fun`.
%% - If `Fun(H1, H2)` returns `true`, `H1` is appended to the result, and the function recursively processes the remainder of the first list (`T1`) and `[H2 | T2]`.
%% - Otherwise, `H2` is appended to the result, and control passes to `fmerge2_2/5` for further merging.
%% - When the first list is empty, the remaining elements of the second list are appended in reverse order to maintain the correct order.
%%
%% ### Parameters:
%% - `H1`: Head of the first list.
%% - `T1`: Tail of the first list.
%% - `H2`: Head of the second list.
%% - `T2`: Tail of the second list.
%% - `M`: Accumulator for the merged result.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` used to determine the order.
%%
%% ### Examples:
%%     fmerge2_1([1, 3, 5], 2, fun(A, B) -> A < B end, [4, 6], []).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     fmerge2_1([2, 4], 1, fun(A, B) -> A < B end, [3, 5], []).
%%     % Result: [1, 2, 3, 4, 5]
%%
%% ### Edge Cases:
%% - If the first list (`[H1 | T1]`) is empty, returns the remaining elements of the second list (`H2` and `T2`) appended to the result.
%%
%% @spec fmerge2_1(List1, Elem2, Fun, List2, Acc) -> Result
%%       when List1 :: [term()],
%%            Elem2 :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List2 :: [term()],
%%            Acc :: [term()],
%%            Result :: [term()].
%%
%% @complexity O(N), where `N` is the total number of elements in both input lists.
fmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
        true ->
            fmerge2_1(T1, H2, Fun, T2, [H1 | M]);
        false ->
            fmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
fmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two sorted lists into a single sorted list using a comparison function, starting with the second list.
%%
%% The `fmerge2_2/5` function merges two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into one sorted list. 
%% It processes the second list (`[H2 | T2]`) first, comparing its head (`H2`) to the head of the first list (`H1`) using the comparison function `Fun`.
%% - If `Fun(H1, H2)` returns `true`, `H1` is appended to the result, and control is passed back to `fmerge2_1/5` for further merging.
%% - Otherwise, `H2` is appended to the result, and the function recursively processes `[H1 | T1]` and the remainder of the second list (`T2`).
%% - When the second list is empty, the remaining elements of the first list are appended in reverse order to ensure proper sorting.
%%
%% ### Parameters:
%% - `H1`: Head of the first list.
%% - `T1`: Tail of the first list.
%% - `H2`: Head of the second list.
%% - `T2`: Tail of the second list.
%% - `M`: Accumulator for the merged result.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` used to determine the order.
%%
%% ### Examples:
%%     fmerge2_2(1, [3, 5], fun(A, B) -> A < B end, [2, 4, 6], []).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     fmerge2_2(2, [4], fun(A, B) -> A < B end, [1, 3, 5], []).
%%     % Result: [1, 2, 3, 4, 5]
%%
%% ### Edge Cases:
%% - If the second list (`[H2 | T2]`) is empty, returns the remaining elements of the first list (`H1` and `T1`) appended to the result.
%%
%% @spec fmerge2_2(Elem1, List1, Fun, List2, Acc) -> Result
%%       when Elem1 :: term(),
%%            List1 :: [term()],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List2 :: [term()],
%%            Acc :: [term()],
%%            Result :: [term()].
%%
%% @complexity O(N), where `N` is the total number of elements in both input lists.
fmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
        true ->
            fmerge2_1(T1, H2, Fun, T2, [H1 | M]);
        false ->
            fmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
fmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rmerge/3

%% @private
%% @doc Merges two sorted lists into a single sorted list using a comparison function, prioritizing the first list.
%%
%% The `rfmerge2_1/5` function merges two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into a single sorted list. 
%% It processes the first list (`[H1 | T1]`) first, comparing its head (`H1`) to the head of the second list (`H2`) using the comparison function `Fun`.
%% - If `Fun(H1, H2)` evaluates to `true`, `H2` is appended to the result, and control is passed to `rfmerge2_2/5` for further merging.
%% - Otherwise, `H1` is appended to the result, and `rfmerge2_1/5` recursively processes the rest of the first list (`T1`) with `[H2 | T2]`.
%% - If the first list is empty, the remaining elements of the second list are appended in reverse order to maintain the proper sorting.
%%
%% ### Parameters:
%% - `H1`: Head of the first list.
%% - `T1`: Tail of the first list.
%% - `H2`: Head of the second list.
%% - `T2`: Tail of the second list.
%% - `M`: Accumulator for the merged result.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` used to determine the order.
%%
%% ### Examples:
%%     rfmerge2_1([3, 5], 2, fun(A, B) -> A > B end, [1, 4], []).
%%     % Result: [5, 4, 3, 2, 1]
%%
%%     rfmerge2_1([2, 4], 3, fun(A, B) -> A > B end, [1, 5], []).
%%     % Result: [4, 3, 2, 1, 5]
%%
%% ### Edge Cases:
%% - If the first list (`[H1 | T1]`) is empty, returns the remaining elements of the second list (`H2` and `T2`) appended in reverse order.
%%
%% @spec rfmerge2_1(List1, Elem2, Fun, List2, Acc) -> Result
%%       when List1 :: [term()],
%%            Elem2 :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List2 :: [term()],
%%            Acc :: [term()],
%%            Result :: [term()].
%%
%% @complexity O(N), where `N` is the total number of elements in both input lists.
rfmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
        true ->
            rfmerge2_2(H1, T1, Fun, T2, [H2 | M]);
        false ->
            rfmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rfmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% @private
%% @doc Merges two sorted lists into a single sorted list using a comparison function, prioritizing the second list.
%%
%% The `rfmerge2_2/5` function merges two sorted lists (`[H1 | T1]` and `[H2 | T2]`) into one sorted list. 
%% It processes the second list (`[H2 | T2]`) first, comparing its head (`H2`) to the head of the first list (`H1`) using the comparison function `Fun`.
%% - If `Fun(H1, H2)` evaluates to `true`, `H2` is appended to the result, and `rfmerge2_2/5` recursively processes `[H1 | T1]` and `T2`.
%% - Otherwise, `H1` is appended to the result, and control is passed to `rfmerge2_1/5` for further merging.
%% - If the second list is empty, the remaining elements of the first list are appended in reverse order.
%%
%% ### Parameters:
%% - `H1`: Head of the first list.
%% - `T1`: Tail of the first list.
%% - `H2`: Head of the second list.
%% - `T2`: Tail of the second list.
%% - `M`: Accumulator for the merged result.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` used to determine the order.
%%
%% ### Examples:
%%     rfmerge2_2(3, [5, 7], fun(A, B) -> A > B end, [2, 6], []).
%%     % Result: [7, 6, 5, 3, 2]
%%
%%     rfmerge2_2(4, [6], fun(A, B) -> A > B end, [1, 5, 7], []).
%%     % Result: [6, 5, 4, 1, 7]
%%
%% ### Edge Cases:
%% - If the second list (`[H2 | T2]`) is empty, returns the remaining elements of the first list (`H1` and `T1`) appended to the result in reverse order.
%%
%% @spec rfmerge2_2(Elem1, List1, Fun, List2, Acc) -> Result
%%       when Elem1 :: term(),
%%            List1 :: [term()],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List2 :: [term()],
%%            Acc :: [term()],
%%            Result :: [term()].
%%
%% @complexity O(N), where `N` is the total number of elements in both input lists.
rfmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
        true ->
            rfmerge2_2(H1, T1, Fun, T2, [H2 | M]);
        false ->
            rfmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rfmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% usort/2

%% Ascending. X < Y
%% @private
%% @doc Splits a list into multiple ordered sublists based on a user-provided comparison function.
%%
%% The `ufsplit_1/6` function processes a list `[Z | L]` element by element, comparing each element `Z` with two pivot elements `Y` and `X` using the user-provided comparison function `Fun`. It determines whether `Z` should:
%% - Be treated as equal to `Y` or `X`.
%% - Replace `Y` or `X` as a new pivot element.
%% - Start a new sublist if it cannot be directly placed relative to `Y` or `X`.
%%
%% The function produces multiple sublists based on the ordering logic defined by `Fun`.
%%
%% ### Parameters:
%% - `Y`: Current primary pivot element.
%% - `X`: Secondary pivot element.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` used to establish the ordering.
%% - `[Z | L]`: The remaining list to be split.
%% - `R`: Current sublist accumulator.
%% - `Rs`: List of already completed sublists.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A < B end,
%%     ufsplit_1(3, 2, Fun, [4, 1, 3], [], []).
%%     % Result: [[2, 3], [4], [1]]
%%
%% ### Edge Cases:
%% - If `Z` is equal to `Y` or `X` (as determined by `Fun`), the element is skipped.
%% - If the current sublist (`R`) is empty, `Z` starts a new sublist.
%%
%% ### Result:
%% - Returns a list of ordered sublists merged via `rufmergel/3`.
%%
%% @spec ufsplit_1(ElemY, ElemX, Fun, List, Acc, SubLists) -> Result
%%       when ElemY :: term(),
%%            ElemX :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List :: [term()],
%%            Acc :: [term()],
%%            SubLists :: [[term()]],
%%            Result :: [[term()]].
%%
%% @complexity O(N), where `N` is the total number of elements in the input list.
ufsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_1(Y, X, Fun, L, R, Rs);
                false ->
                    ufsplit_1(Z, Y, Fun, L, [X | R], Rs)
            end;
        false ->
            case Fun(X, Z) of
                true ->
                    case Fun(Z, X) of
                        true -> % Z equal to X
                            ufsplit_1(Y, X, Fun, L, R, Rs);
                        false ->
                            ufsplit_1(Y, Z, Fun, L, [X | R], Rs)
                    end;
                false when R == [] ->
                    ufsplit_1(Y, X, Fun, L, [Z], Rs);
                false ->
                    ufsplit_1_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
ufsplit_1(Y, X, Fun, [], R, Rs) ->
    rufmergel([[Y, X | R] | Rs], [], Fun).

%% X < Y
%% @private
%% @doc Further splits a list into ordered sublists based on a user-defined comparison function, considering an additional pivot.
%%
%% The `ufsplit_1_1/7` function processes a list `[Z | L]`, comparing each element `Z` with three pivot elements: `Y`, `X`, and `S`.
%% It ensures that each element is placed appropriately into an ordered sublist or initiates a new sublist if needed. The
%% comparisons are performed using the user-provided function `Fun`.
%%
%% ### Parameters:
%% - `Y`: Primary pivot element.
%% - `X`: Secondary pivot element.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` used to establish the ordering.
%% - `[Z | L]`: The remaining list to be split.
%% - `R`: Current sublist accumulator.
%% - `Rs`: List of completed sublists.
%% - `S`: Additional pivot element for further refinement.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A < B end,
%%     ufsplit_1_1(3, 2, Fun, [4, 1, 3], [], [], 5).
%%     % Result: [[5], [2, 3], [4], [1]]
%%
%% ### Behavior:
%% 1. **Equal Elements**:
%%    - If `Z` is determined to be "equal" to `Y`, `X`, or `S` (based on `Fun`), it is skipped.
%%
%% 2. **Replace Pivots**:
%%    - If `Z` is "less than" or "greater than" one of the pivots (`Y`, `X`, or `S`), the corresponding pivot is replaced, and the process continues.
%%
%% 3. **Start New Sublist**:
%%    - If `Z` cannot be placed relative to any of the pivots, a new sublist is initiated, and the process is delegated to `ufsplit_1/6`.
%%
%% 4. **End of Input**:
%%    - When the input list `[Z | L]` is exhausted, remaining elements and sublists are combined using `rufmergel/3`.
%%
%% ### Result:
%% - Returns a list of ordered sublists, ensuring the input list is split based on the logic defined by `Fun`.
%%
%% @spec ufsplit_1_1(ElemY, ElemX, Fun, List, Acc, SubLists, ElemS) -> Result
%%       when ElemY :: term(),
%%            ElemX :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List :: [term()],
%%            Acc :: [term()],
%%            SubLists :: [[term()]],
%%            ElemS :: term(),
%%            Result :: [[term()]].
%%
%% @complexity O(N), where `N` is the total number of elements in the input list.
ufsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                false ->
                    ufsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S)
            end;
        false ->
            case Fun(X, Z) of
                true ->
                    case Fun(Z, X) of
                        true -> % Z equal to X
                            ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                        false ->
                            ufsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S)
                    end;
                false ->
                    case Fun(S, Z) of
                        true ->
                            case Fun(Z, S) of
                                true -> % Z equal to S
                                    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                                false ->
                                    ufsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs])
                            end;
                        false ->
                            ufsplit_1(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
ufsplit_1_1(Y, X, Fun, [], R, Rs, S) ->
    rufmergel([[S], [Y, X | R] | Rs], [], Fun).

%% Descending.
%% @private
%% @doc Splits a list into ordered sublists using a user-defined comparison function, starting from a single pivot.
%%
%% The `ufsplit_2/4` function processes a list `[Z | L]`, comparing each element `Z` with a single pivot `Y` using a user-provided 
%% function `Fun`. Based on the result of the comparison, the element `Z` is either added to the current accumulator `R` or 
%% initiates a new sublist.
%%
%% ### Parameters:
%% - `Y`: The current pivot element.
%% - `[Z | L]`: The remaining list to be split.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()`, which defines the ordering.
%% - `R`: Accumulator holding elements of the current sublist.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A < B end,
%%     ufsplit_2(3, [4, 1, 5, 2], Fun, []).
%%     % Result: [[5, 3], [4], [1, 2]]
%%
%% ### Behavior:
%% 1. **Comparison**:
%%    - If `Fun(Y, Z)` is `true` and `Fun(Z, Y)` is also `true`:
%%        - `Z` is considered equal to `Y` and skipped.
%%    - Otherwise, a new sublist is initiated with `ufsplit_1/6`.
%% 2. **New Pivot**:
%%    - If `Fun(Y, Z)` is `false`, `Z` becomes the new pivot, and `Y` is added to the current sublist.
%%
%% 3. **End of Input**:
%%    - When the input list `[Z | L]` is exhausted, the current sublist is returned, combined with `R`.
%%
%% ### Result:
%% - Returns a list of ordered sublists based on the comparison function `Fun`.
%%
%% @spec ufsplit_2(Pivot, List, Fun, Acc) -> Sublists
%%       when Pivot :: term(),
%%            List :: [term()],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            Acc :: [term()],
%%            Sublists :: [[term()]].
%%
%% @complexity O(N), where `N` is the total number of elements in the input list.
ufsplit_2(Y, [Z | L], Fun, R) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_2(Y, L, Fun, R);
                false ->
                    ufsplit_1(Z, Y, Fun, L, [], [lists:reverse(R, [])])
            end;
        false ->
            ufsplit_2(Z, L, Fun, [Y | R])
    end;
ufsplit_2(Y, [], _Fun, R) ->
    [Y | R].

%% @private
%% @doc Merges a list of sorted sublists into a single sorted list using a user-defined comparison function.
%%
%% The `ufmergel/3` function iteratively merges pairs of sorted sublists from the input list using a user-defined 
%% comparison function `Fun`. The result is a single sorted list.
%%
%% ### Parameters:
%% - `[[H1 | T1], T2 | L]`: A list of sorted sublists to merge.
%% - `Acc`: An accumulator for intermediate results during merging.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the ordering.
%%
%% ### Behavior:
%% 1. **Pairwise Merging**:
%%    - Merges the first two sublists (`[H1 | T1]` and `T2`) using `ufmerge2_2/5` and appends the result to the accumulator.
%% 2. **End of Input**:
%%    - When there is only one sublist left, it is returned as the final merged result.
%% 3. **Reversals**:
%%    - Intermediate sublists are reversed before being passed to `rufmergel/3` to ensure proper ordering.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A < B end,
%%     ufmergel([[1, 3, 5], [2, 4, 6]], [], Fun).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     Fun = fun(A, B) -> A > B end,
%%     ufmergel([[6, 4, 2], [5, 3, 1]], [], Fun).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%% ### Result:
%% - Returns a single sorted list based on the comparison function `Fun`.
%%
%% @spec ufmergel(Sublists, Acc, Fun) -> SortedList
%%       when Sublists :: [[term()]],
%%            Acc :: [[term()]],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            SortedList :: [term()].
%%
%% @complexity O(N log K), where `N` is the total number of elements across all sublists, 
%% and `K` is the number of sublists.
ufmergel([[H1 | T1], T2 | L], Acc, Fun) ->
    ufmergel(L, [ufmerge2_2(H1, T1, Fun, T2, []) | Acc], Fun);
ufmergel([L], [], _Fun) ->
    L;
ufmergel([L], Acc, Fun) ->
    rufmergel([lists:reverse(L, []) | Acc], [], Fun);
ufmergel([], Acc, Fun) ->
    rufmergel(Acc, [], Fun).

rufmergel([[H2 | T2], T1 | L], Acc, Fun) ->
    rufmergel(L, [rufmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun);
rufmergel([L], Acc, Fun) ->
    ufmergel([lists:reverse(L, []) | Acc], [], Fun);
rufmergel([], Acc, Fun) ->
    ufmergel(Acc, [], Fun).

%% umerge/3

%% Elements from the first list are kept and prioritized.
%% HdM before H2.
%% @private
%% @doc Merges two sorted lists based on a user-defined comparison function, while maintaining stability and detecting duplicates.
%%
%% The `ufmerge2_1/6` function merges the head of the first sorted list (`[H1 | T1]`) with the second list (`H2`, `T2`),
%% ensuring that elements are ordered according to the comparison function `Fun`. It also checks for duplicates by comparing
%% elements with a previous head (`HdM`) in the merged list.
%%
%% ### Parameters:
%% - `[H1 | T1]`: The first sorted list to merge.
%% - `H2`: The current head of the second sorted list.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the ordering.
%% - `T2`: The remaining elements of the second sorted list.
%% - `M`: An accumulator for the merged result.
%% - `HdM`: The most recently added element in the merged list, used for duplicate detection.
%%
%% ### Behavior:
%% 1. **Comparison and Stability**:
%%    - Compares the current heads (`H1` and `H2`) of the two lists using `Fun`.
%%    - Adds the smaller element to the result (`M`) while maintaining the order defined by `Fun`.
%% 2. **Duplicate Detection**:
%%    - Detects duplicates by comparing `H2` to `HdM`. If they are equal, `H2` is not added to the merged result.
%% 3. **Base Case**:
%%    - When the first list (`T1`) is empty, appends the remaining elements of the second list (`T2`) to the result,
%%      while checking for duplicates.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A < B end,
%%     ufmerge2_1([1, 3, 5], 2, Fun, [4, 6], [], 1).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%%     Fun = fun(A, B) -> A < B end,
%%     ufmerge2_1([1, 2, 5], 2, Fun, [3, 4, 6], [], 1).
%%     % Result: [1, 2, 3, 4, 5, 6]
%%
%% ### Result:
%% - Returns a merged and sorted list based on the comparison function `Fun`, while maintaining stability.
%%
%% @spec ufmerge2_1(List1, H2, Fun, List2, Acc, LastMerged) -> MergedList
%%       when List1 :: [term()],
%%            H2 :: term(),
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List2 :: [term()],
%%            Acc :: [term()],
%%            LastMerged :: term(),
%%            MergedList :: [term()].
%%
%% @complexity O(N), where `N` is the total number of elements in both lists.
ufmerge2_1([H1 | T1], H2, Fun, T2, M, HdM) ->
    case Fun(H1, H2) of
        true ->
            ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
        false ->
            case Fun(H2, HdM) of
                true -> % HdM equal to H2
                    ufmerge2_2(H1, T1, Fun, T2, M);
                false ->
                    ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
            end
    end;
ufmerge2_1([], H2, Fun, T2, M, HdM) ->
    case Fun(H2, HdM) of
        true -> % HdM equal to H2
            lists:reverse(T2, M);
        false ->
            lists:reverse(T2, [H2 | M])
    end.

%% @private
%% @doc Merges two sorted lists based on a user-defined comparison function, starting with the head of the second list.
%%
%% The `ufmerge2_2/5` function merges the first list (`H1`, `T1`) with the second list (`[H2 | T2]`),
%% ensuring that elements are ordered according to the user-defined comparison function `Fun`.
%%
%% ### Parameters:
%% - `H1`: The current head of the first sorted list.
%% - `T1`: The tail of the first sorted list.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the ordering.
%% - `[H2 | T2]`: The second sorted list to merge.
%% - `M`: An accumulator for the merged result.
%%
%% ### Behavior:
%% 1. **Comparison and Stability**:
%%    - Compares the current heads (`H1` and `H2`) of the two lists using `Fun`.
%%    - Adds the smaller element to the result (`M`) while maintaining the order defined by `Fun`.
%% 2. **Recursive Delegation**:
%%    - If the head of the first list is smaller or equal (`H1 <= H2`), delegates to `ufmerge2_1/6`.
%%    - Otherwise, it continues merging the remaining elements of the second list (`T2`) with the first list (`H1`, `T1`).
%% 3. **Base Case**:
%%    - When the second list is empty (`T2`), appends the remaining elements of the first list (`H1`, `T1`) to the result.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A < B end,
%%     ufmerge2_2(2, [3, 5], Fun, [4, 6], []).
%%     % Result: [2, 3, 4, 5, 6]
%%
%%     Fun = fun(A, B) -> A < B end,
%%     ufmerge2_2(2, [3, 5], Fun, [], []).
%%     % Result: [2, 3, 5]
%%
%% ### Result:
%% - Returns a merged and sorted list based on the comparison function `Fun`.
%%
%% @spec ufmerge2_2(H1, List1, Fun, List2, Acc) -> MergedList
%%       when H1 :: term(),
%%            List1 :: [term()],
%%            Fun :: fun((term(), term()) -> boolean()),
%%            List2 :: [term()],
%%            Acc :: [term()],
%%            MergedList :: [term()].
%%
%% @complexity O(N), where `N` is the total number of elements in both lists.
ufmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
        true ->
            ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
        false ->
            ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
ufmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rumerge/3

%% @private
%% @doc Merges two sorted lists in reverse order based on a user-defined comparison function, prioritizing the head of the second list.
%%
%% The `rufmerge2_1/5` function merges the first sorted list (`[H1 | T1]`) with the second list (`[H2 | T2]`),
%% ensuring that the resulting list is sorted in reverse order according to the user-defined comparison function `Fun`.
%%
%% ### Parameters:
%% - `H1`: The current head of the first sorted list.
%% - `T1`: The tail of the first sorted list.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the reverse ordering.
%% - `H2`: The current head of the second sorted list.
%% - `T2`: The tail of the second sorted list.
%% - `M`: An accumulator for the merged result.
%%
%% ### Behavior:
%% 1. **Comparison and Stability**:
%%    - Compares the current heads (`H1` and `H2`) using `Fun`.
%%    - If `Fun(H1, H2)` returns `true`, `H1` is added to the accumulator `M`.
%%    - Otherwise, `H2` is retained, and merging proceeds with the remaining elements of the first list.
%% 2. **Recursive Delegation**:
%%    - Recursively merges the remaining elements of the two lists until one of the lists is exhausted.
%% 3. **Base Case**:
%%    - When the first list (`[H1 | T1]`) is empty, appends all remaining elements of the second list (`H2`, `T2`) to the result in reverse order.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A > B end,
%%     rufmerge2_1([5, 3], 4, Fun, [2, 1], []).
%%     % Result: [5, 4, 3, 2, 1]
%%
%%     Fun = fun(A, B) -> A > B end,
%%     rufmerge2_1([], 4, Fun, [2, 1], []).
%%     % Result: [4, 2, 1]
%%
%% ### Result:
%% - Returns a merged and sorted list in reverse order based on the comparison function `Fun`.
%%
%% @spec rufmerge2_1([T], T, fun((T, T) -> boolean()), [T], [T]) -> [T]
%%       when T :: term().
%%
%% @complexity O(N), where `N` is the total number of elements in both lists.
rufmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
        true ->
            rufmerge2_2(H1, T1, Fun, T2, M, H2);
        false ->
            rufmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rufmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

%% H1 before H2M
%% @private
%% @doc Merges two sorted lists in reverse order using a user-defined comparison function, prioritizing the head of the first list.
%%
%% The `rufmerge2_2/6` function merges the head and tail of the first list (`H1`, `T1`) with the second list (`H2`, `T2`),
%% ensuring that the resulting list is sorted in reverse order according to the user-defined comparison function `Fun`.
%%
%% ### Parameters:
%% - `H1`: The current head of the first sorted list.
%% - `T1`: The tail of the first sorted list.
%% - `Fun`: A comparison function of the form `fun(A, B) -> boolean()` that defines the reverse ordering.
%% - `H2`: The current head of the second sorted list.
%% - `T2`: The tail of the second sorted list.
%% - `M`: An accumulator for the merged result.
%% - `H2M`: A reference to track the head of the second list for equality checks with `H1`.
%%
%% ### Behavior:
%% 1. **Comparison and Stability**:
%%    - Compares `H1` (from the first list) with `H2` (from the second list) using `Fun`.
%%    - If `Fun(H1, H2)` is `true`, `H2` is added to the accumulator (`M`), and the function continues with the next element of the second list (`T2`).
%%    - Otherwise, performs an additional check to compare `H2M` with `H1`.
%%      - If `Fun(H2M, H1)` is `true`, treats `H1` as equal to `H2M` and delegates merging to `rufmerge2_1/5`.
%%      - Otherwise, considers `H2M` to be smaller and adds both `H1` and `H2M` to the accumulator.
%% 2. **Base Case**:
%%    - When the second list (`T2`) is empty, compares `H2M` with `H1` to decide how to finalize the merged result.
%%
%% ### Examples:
%%     Fun = fun(A, B) -> A > B end,
%%     rufmerge2_2(5, [4, 3], Fun, [2, 1], [], 6).
%%     % Result: [6, 5, 4, 3, 2, 1]
%%
%%     Fun = fun(A, B) -> A > B end,
%%     rufmerge2_2(3, [2], Fun, [4, 1], [], 5).
%%     % Result: [5, 4, 3, 2, 1]
%%
%% ### Result:
%% - Returns a merged and sorted list in reverse order based on the comparison function `Fun`.
%%
%% @spec rufmerge2_2(T, [T], fun((T, T) -> boolean()), [T], [T], T) -> [T]
%%       when T :: term().
%%
%% @complexity O(N), where `N` is the total number of elements in both lists.
rufmerge2_2(H1, T1, Fun, [H2 | T2], M, H2M) ->
    case Fun(H1, H2) of
        true ->
            rufmerge2_2(H1, T1, Fun, T2, [H2M | M], H2);
        false ->
            case Fun(H2M, H1) of
                true -> % H2M equal to H1
                    rufmerge2_1(T1, H2, Fun, T2, [H1 | M]);
                false ->
                    rufmerge2_1(T1, H2, Fun, T2, [H1, H2M | M])
            end
    end;
rufmerge2_2(H1, T1, Fun, [], M, H2M) ->
    case Fun(H2M, H1) of
        true -> 
            lists:reverse(T1, [H1 | M]);
        false ->
            lists:reverse(T1, [H1, H2M | M])
    end.

%% uniq/1: return a new list with the unique elements of the given list

-doc """
Returns a list containing the elements of `List1` with duplicated elements
removed (preserving the order of the elements). The first occurrence of each
element is kept.

_Examples:_

```erlang
> lists:uniq([3,3,1,2,1,2,3]).
[3,1,2]
> lists:uniq([a, a, 1, b, 2, a, 3]).
[a, 1, b, 2, 3]
```
""".
%% @doc Removes duplicate elements from a list while preserving the order of the first occurrences.
%%
%% The `uniq/1` function is a wrapper around `uniq_1/2`, designed to simplify the removal of duplicates in a list. 
%% It initializes the map used for tracking seen elements and returns the resulting list with unique elements.
%%
%% ### Parameters:
%% - `L`: The input list containing elements that may have duplicates.
%%
%% ### Behavior:
%% - Calls `uniq_1/2` with the input list `L` and an empty map (`#{}`) to keep track of seen elements.
%% - Removes duplicates while preserving the order of first occurrences.
%%
%% ### Examples:
%%     uniq([1, 2, 3, 2, 4, 1]).
%%     % Result: [1, 2, 3, 4]
%%
%%     uniq(["a", "b", "a", "c"]).
%%     % Result: ["a", "b", "c"]
%%
%%     uniq([]).
%%     % Result: []
%%
%% ### Result:
%% - Returns a list containing unique elements from the input list, preserving their order of first appearance.
%%
%% @since OTP 25.0
%% @spec uniq(List1) -> List2 when
%%       List1 :: [T],
%%       List2 :: [T],
%%       T :: term().
%%
%% @complexity O(N), where `N` is the number of elements in the input list.
-doc(#{since => <<"OTP 25.0">>}).
-spec uniq(List1) -> List2 when
      List1 :: [T],
      List2 :: [T],
      T :: term().

uniq(L) ->
    uniq_1(L, #{}).

%% @private
%% @doc Removes duplicate elements from a list while preserving the order of the first occurrences.
%%
%% The `uniq_1/2` function processes a list (`[X | Xs]`) and uses a map (`M`) to track elements that have already been seen. 
%% It ensures that each element appears only once in the resulting list while maintaining the order of their first occurrence.
%%
%% ### Parameters:
%% - `[X | Xs]`: The input list to be processed.
%% - `M`: A map used to store already-seen elements. The keys of the map are the elements from the list, and their values are set to `true`.
%%
%% ### Behavior:
%% 1. For each element `X`:
%%    - If `X` is already present in the map `M`, it is skipped.
%%    - Otherwise, it is added to the output list and marked as seen in the map `M`.
%% 2. Continues until the input list is exhausted.
%%
%% ### Examples:
%%     uniq_1([1, 2, 3, 2, 4, 1], #{}) -> [1, 2, 3, 4].
%%     uniq_1(["a", "b", "a", "c"], #{}) -> ["a", "b", "c"].
%%     uniq_1([], #{}) -> [].
%%
%% ### Result:
%% - Returns a list containing unique elements from the input list, preserving their order of first appearance.
%%
%% @spec uniq_1([T], map()) -> [T]
%%       when T :: term().
%%
%% @complexity O(N), where `N` is the number of elements in the input list.
uniq_1([X | Xs], M) ->
    case is_map_key(X, M) of
        true ->
            uniq_1(Xs, M);
        false ->
            [X | uniq_1(Xs, M#{X => true})]
    end;
uniq_1([], _) ->
    [].

%% uniq/2: return a new list with the unique elements of the given list using a function key

-doc """
Returns a list containing the elements of `List1` without the elements for which
`Fun` returned duplicate values (preserving the order of the elements). The
first occurrence of each element is kept.

_Examples:_

```erlang
> lists:uniq(fun({X, _}) -> X end, [{b, 2}, {a, 1}, {c, 3}, {a, 2}]).
[{b, 2}, {a, 1}, {c, 3}]
```
""".
%% @doc Removes duplicate elements from a list based on keys extracted by a user-defined function, preserving the order of the first occurrences.
%%
%% The `uniq/2` function uses a key-extraction function to determine the uniqueness of elements in the input list.
%% It traverses the list, keeping only the first occurrence of elements that produce a unique key.
%%
%% ### Parameters:
%% - `F`: A key-extraction function of arity 1. This function takes an element of the list and returns a value that determines its uniqueness.
%% - `L`: A list of elements that may contain duplicates.
%%
%% ### Behavior:
%% - Calls the helper function `uniq_2/3` with the input list `L`, the key-extraction function `F`, and an empty map (`#{}`) for tracking seen keys.
%% - Removes duplicates by ensuring that no two elements yield the same key when passed to `F`.
%%
%% ### Examples:
%%     uniq(fun(X) -> X end, [1, 2, 3, 2, 4, 1]).
%%     % Result: [1, 2, 3, 4]
%%
%%     uniq(fun({K, _}) -> K end, [{"a", 1}, {"b", 2}, {"a", 3}]).
%%     % Result: [{"a", 1}, {"b", 2}]
%%
%%     uniq(fun(X) -> string:substr(X, 1, 1) end, ["apple", "banana", "apricot"]).
%%     % Result: ["apple", "banana"]
%%
%% ### Result:
%% - Returns a list containing unique elements from the input list, determined by the key returned by `F`.
%%
%% ### Notes:
%% - The key-extraction function `F` must be a valid function of arity 1.
%%
%% @since OTP 25.0
%% @spec uniq(Fun, List1) -> List2 when
%%       Fun :: fun((T) -> any()),
%%       List1 :: [T],
%%       List2 :: [T],
%%       T :: term().
%%
%% @complexity O(N), where `N` is the number of elements in the input list.
-doc(#{since => <<"OTP 25.0">>}).
-spec uniq(Fun, List1) -> List2 when
      Fun :: fun((T) -> any()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

uniq(F, L) when is_function(F, 1) ->
    uniq_2(L, F, #{}).

%% @doc Removes duplicate elements from a list based on a key extracted by the given function, while preserving the order of first occurrences.
%%
%% The `uniq/2` function provides enhanced functionality for deduplication by allowing a custom function to determine the uniqueness of elements.
%% It processes the input list and returns a list where duplicate elements (based on the extracted key) are removed.
%%
%% ### Parameters:
%% - `L`: The input list containing elements that may have duplicates.
%% - `F`: A function that takes an element of the list and returns a key to determine uniqueness.
%%
%% ### Behavior:
%% - Calls `uniq_2/3` with the input list `L`, the key-extraction function `F`, and an empty map (`#{}`) for tracking seen keys.
%% - Removes duplicates by ensuring that no two elements produce the same key when passed to the function `F`.
%%
%% ### Examples:
%%     uniq([1, 2, 3, 2, 4, 1], fun(X) -> X end).
%%     % Result: [1, 2, 3, 4]
%%
%%     uniq([{"a", 1}, {"b", 2}, {"a", 3}], fun({Key, _}) -> Key end).
%%     % Result: [{"a", 1}, {"b", 2}]
%%
%%     uniq(["apple", "banana", "apricot"], fun(X) -> string:substr(X, 1, 1) end).
%%     % Result: ["apple", "banana"]
%%
%% ### Result:
%% - Returns a list containing unique elements from the input list, determined by the key extracted by the function `F`.
%%
%% ### Notes:
%% - The key-extraction function `F` must be a valid function of arity 1.
%%
%% @since OTP 25.0
%% @spec uniq(List1, Fun) -> List2 when
%%       List1 :: [T],
%%       List2 :: [T],
%%       Fun :: fun((T) -> K),
%%       T :: term(),
%%       K :: term().
%%
%% @complexity O(N), where `N` is the number of elements in the input list.
uniq_2([X | Xs], F, M) ->
    Key = F(X),
    case is_map_key(Key, M) of
        true ->
            uniq_2(Xs, F, M);
        false ->
            [X | uniq_2(Xs, F, M#{Key => true})]
    end;
uniq_2([], _, _) ->
    [].
