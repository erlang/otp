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
# Tables and Databases

## Ets, Dets, and Mnesia

Every example using Ets has a corresponding example in Mnesia. In general, all
Ets examples also apply to Dets tables.

### Select/Match Operations

Select/match operations on Ets and Mnesia tables can become very expensive
operations. They usually need to scan the complete table. Try to structure the
data to minimize the need for select/match operations. However, if you require a
select/match operation, it is still more efficient than using `tab2list`.
Examples of this and of how to avoid select/match are provided in the following
sections. The functions `ets:select/2` and `mnesia:select/3` are to be preferred
over `ets:match/2`, `ets:match_object/2`, and `mnesia:match_object/3`.

In some circumstances, the select/match operations do not need to scan the
complete table. For example, if part of the key is bound when searching an
`ordered_set` table, or if it is a Mnesia table and there is a secondary index
on the field that is selected/matched. If the key is fully bound, there is no
point in doing a select/match, unless you have a bag table and are only
interested in a subset of the elements with the specific key.

When creating a record to be used in a select/match operation, you want most of
the fields to have the value "\_". The easiest and fastest way to do that is as
follows:

```text
#person{age = 42, _ = '_'}.
```

### Deleting an Element

The `delete` operation is considered successful if the element was not present
in the table. Hence all attempts to check that the element is present in the
Ets/Mnesia table before deletion are unnecessary. Here follows an example for
Ets tables:

_DO_

```text
...
ets:delete(Tab, Key),
...
```

_DO NOT_

```erlang
...
case ets:lookup(Tab, Key) of
    [] ->
        ok;
    [_|_] ->
        ets:delete(Tab, Key)
end,
...
```

### Fetching Data

Do not fetch data that you already have.

Consider that you have a module that handles the abstract data type `Person`.
You export the interface function `print_person/1`, which uses the internal
functions `print_name/1`, `print_age/1`, and `print_occupation/1`.

> #### Note {: .info }
>
> If the function `print_name/1`, and so on, had been interface functions, the
> situation would have been different, as you do not want the user of the
> interface to know about the internal data representation.

_DO_

```erlang
%%% Interface function
print_person(PersonId) ->
    %% Look up the person in the named table person,
    case ets:lookup(person, PersonId) of
        [Person] ->
            print_name(Person),
            print_age(Person),
            print_occupation(Person);
        [] ->
            io:format("No person with ID = ~p~n", [PersonID])
    end.

%%% Internal functions
print_name(Person) ->
    io:format("No person ~p~n", [Person#person.name]).

print_age(Person) ->
    io:format("No person ~p~n", [Person#person.age]).

print_occupation(Person) ->
    io:format("No person ~p~n", [Person#person.occupation]).
```

_DO NOT_

```erlang
%%% Interface function
print_person(PersonId) ->
    %% Look up the person in the named table person,
    case ets:lookup(person, PersonId) of
        [Person] ->
            print_name(PersonID),
            print_age(PersonID),
            print_occupation(PersonID);
        [] ->
            io:format("No person with ID = ~p~n", [PersonID])
    end.

%%% Internal functions
print_name(PersonID) ->
    [Person] = ets:lookup(person, PersonId),
    io:format("No person ~p~n", [Person#person.name]).

print_age(PersonID) ->
    [Person] = ets:lookup(person, PersonId),
    io:format("No person ~p~n", [Person#person.age]).

print_occupation(PersonID) ->
    [Person] = ets:lookup(person, PersonId),
    io:format("No person ~p~n", [Person#person.occupation]).
```

### Non-Persistent Database Storage

For non-persistent database storage, prefer Ets tables over Mnesia
`local_content` tables. Even the Mnesia `dirty_write` operations carry a fixed
overhead compared to Ets writes. Mnesia must check if the table is replicated or
has indices, this involves at least one Ets lookup for each `dirty_write`. Thus,
Ets writes is always faster than Mnesia writes.

### tab2list

Assuming an Ets table that uses `idno` as key and contains the following:

```text
[#person{idno = 1, name = "Adam",  age = 31, occupation = "mailman"},
 #person{idno = 2, name = "Bryan", age = 31, occupation = "cashier"},
 #person{idno = 3, name = "Bryan", age = 35, occupation = "banker"},
 #person{idno = 4, name = "Carl",  age = 25, occupation = "mailman"}]
```

If you _must_ return all data stored in the Ets table, you can use
`ets:tab2list/1`. However, usually you are only interested in a subset of the
information in which case `ets:tab2list/1` is expensive. If you only want to
extract one field from each record, for example, the age of every person, then:

_DO_

```erlang
...
ets:select(Tab,[{ #person{idno='_',
                          name='_',
                          age='$1',
                          occupation = '_'},
                [],
                ['$1']}]),
...
```

_DO NOT_

```erlang
...
TabList = ets:tab2list(Tab),
lists:map(fun(X) -> X#person.age end, TabList),
...
```

If you are only interested in the age of all persons named "Bryan", then:

_DO_

```erlang
...
ets:select(Tab,[{ #person{idno='_',
                          name="Bryan",
                          age='$1',
                          occupation = '_'},
                [],
                ['$1']}]),
...
```

_DO NOT_

```erlang
...
TabList = ets:tab2list(Tab),
lists:foldl(fun(X, Acc) -> case X#person.name of
                                "Bryan" ->
                                    [X#person.age|Acc];
                                 _ ->
                                     Acc
                           end
             end, [], TabList),
...
```

_REALLY DO NOT_

```erlang
...
TabList = ets:tab2list(Tab),
BryanList = lists:filter(fun(X) -> X#person.name == "Bryan" end,
                         TabList),
lists:map(fun(X) -> X#person.age end, BryanList),
...
```

If you need all information stored in the Ets table about persons named "Bryan",
then:

_DO_

```erlang
...
ets:select(Tab, [{#person{idno='_',
                          name="Bryan",
                          age='_',
                          occupation = '_'}, [], ['$_']}]),
...
```

_DO NOT_

```erlang
...
TabList = ets:tab2list(Tab),
lists:filter(fun(X) -> X#person.name == "Bryan" end, TabList),
...
```

### Ordered_set Tables

If the data in the table is to be accessed so that the order of the keys in the
table is significant, the table type `ordered_set` can be used instead of the
more usual `set` table type. An `ordered_set` is always traversed in Erlang term
order regarding the key field so that the return values from functions such as
`select`, `match_object`, and `foldl` are ordered by the key values. Traversing
an `ordered_set` with the `first` and `next` operations also returns the keys
ordered.

> #### Note {: .info }
>
> An `ordered_set` only guarantees that objects are processed in _key_ order.
> Results from functions such as `ets:select/2` appear in _key_ order even if
> the key is not included in the result.

## Ets-Specific

### Using Keys of Ets Table

An Ets table is a single-key table (either a hash table or a tree ordered by the
key) and is to be used as one. In other words, use the key to look up things
whenever possible. A lookup by a known key in a `set` Ets table is constant and
for an `ordered_set` Ets table it is O(logN). A key lookup is always preferable
to a call where the whole table has to be scanned. In the previous examples, the
field `idno` is the key of the table and all lookups where only the name is
known result in a complete scan of the (possibly large) table for a matching
result.

A simple solution would be to use the `name` field as the key instead of the
`idno` field, but that would cause problems if the names were not unique. A more
general solution would be to create a second table with `name` as key and `idno`
as data, that is, to index (invert) the table regarding the `name` field.
Clearly, the second table would have to be kept consistent with the master
table. Mnesia can do this for you, but a home brew index table can be very
efficient compared to the overhead involved in using Mnesia.

An index table for the table in the previous examples would have to be a bag (as
keys would appear more than once) and can have the following contents:

```text
[#index_entry{name="Adam", idno=1},
 #index_entry{name="Bryan", idno=2},
 #index_entry{name="Bryan", idno=3},
 #index_entry{name="Carl", idno=4}]
```

Given this index table, a lookup of the `age` fields for all persons named
"Bryan" can be done as follows:

```erlang
...
MatchingIDs = ets:lookup(IndexTable,"Bryan"),
lists:map(fun(#index_entry{idno = ID}) ->
                 [#person{age = Age}] = ets:lookup(PersonTable, ID),
                 Age
          end,
          MatchingIDs),
...
```

Notice that this code never uses `ets:match/2` but instead uses the
`ets:lookup/2` call. The `lists:map/2` call is only used to traverse the `idno`s
matching the name "Bryan" in the table; thus the number of lookups in the master
table is minimized.

Keeping an index table introduces some overhead when inserting records in the
table. The number of operations gained from the table must therefore be compared
against the number of operations inserting objects in the table. However, notice
that the gain is significant when the key can be used to lookup elements.

## Mnesia-Specific

### Secondary Index

If you frequently do a lookup on a field that is not the key of the table, you
lose performance using "mnesia:select/match_object" as this function traverses
the whole table. You can create a secondary index instead and use
"mnesia:index_read" to get faster access, however this requires more memory.

_Example_

```erlang
-record(person, {idno, name, age, occupation}).
        ...
{atomic, ok} =
mnesia:create_table(person, [{index,[#person.age]},
                              {attributes,
                                    record_info(fields, person)}]),
{atomic, ok} = mnesia:add_table_index(person, age),
...

PersonsAge42 =
     mnesia:dirty_index_read(person, 42, #person.age),
...
```

### Transactions

Using transactions is a way to guarantee that the distributed Mnesia database
remains consistent, even when many different processes update it in parallel.
However, if you have real-time requirements it is recommended to use `dirty`
operations instead of transactions. When using `dirty` operations, you lose the
consistency guarantee; this is usually solved by only letting one process update
the table. Other processes must send update requests to that process.

_Example_

```erlang
...
% Using transaction

Fun = fun() ->
          [mnesia:read({Table, Key}),
           mnesia:read({Table2, Key2})]
      end,

{atomic, [Result1, Result2]}  = mnesia:transaction(Fun),
...

% Same thing using dirty operations
...

Result1 = mnesia:dirty_read({Table, Key}),
Result2 = mnesia:dirty_read({Table2, Key2}),
...
```
