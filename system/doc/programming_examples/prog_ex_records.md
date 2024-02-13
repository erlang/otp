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
# Records

## Records and Tuples

The main advantage of using records rather than tuples is that fields in a
record are accessed by name, whereas fields in a tuple are accessed by position.
To illustrate these differences, suppose that you want to represent a person
with the tuple `{Name, Address, Phone}`.

To write functions that manipulate this data, remember the following:

- The `Name` field is the first element of the tuple.
- The `Address` field is the second element.
- The `Phone` field is the third element.

For example, to extract data from a variable `P` that contains such a tuple, you
can write the following code and then use pattern matching to extract the
relevant fields:

```erlang
Name = element(1, P),
Address = element(2, P),
...
```

Such code is difficult to read and understand, and errors occur if the numbering
of the elements in the tuple is wrong. If the data representation of the fields
is changed, by re-ordering, adding, or removing fields, all references to the
person tuple must be checked and possibly modified.

Records allow references to the fields by name, instead of by position. In the
following example, a record instead of a tuple is used to store the data:

```erlang
-record(person, {name, phone, address}).
```

This enables references to the fields of the record by name. For example, if `P`
is a variable whose value is a `person` record, the following code access the
name and address fields of the records:

```text
Name = P#person.name,
Address = P#person.address,
...
```

Internally, records are represented using tagged tuples:

```text
{person, Name, Phone, Address}
```

## Defining a Record

This following definition of a `person` is used in several examples in this
section. Three fields are included, `name`, `phone`, and `address`. The default
values for `name` and `phone` is "" and [], respectively. The default value for
`address` is the atom `undefined`, since no default value is supplied for this
field:

```erlang
-record(person, {name = "", phone = [], address}).
```

The record must be defined in the shell to enable use of the record syntax in
the examples:

```erlang
> rd(person, {name = "", phone = [], address}).
person
```

This is because record definitions are only available at compile time, not at
runtime. For details on records in the shell, see the `m:shell` manual page in
STDLIB.

## Creating a Record

A new `person` record is created as follows:

```erlang
> #person{phone=[0,8,2,3,4,3,1,2], name="Robert"}.
#person{name = "Robert",phone = [0,8,2,3,4,3,1,2],address = undefined}
```

As the `address` field was omitted, its default value is used.

From Erlang 5.1/OTP R8B, a value to all fields in a record can be set with the
special field `_`. `_` means "all fields not explicitly specified".

_Example:_

```text
> #person{name = "Jakob", _ = '_'}.
#person{name = "Jakob",phone = '_',address = '_'}
```

It is primarily intended to be used in `ets:match/2` and
`mnesia:match_object/3`, to set record fields to the atom `'_'`. (This is a
wildcard in `ets:match/2`.)

## Accessing a Record Field

The following example shows how to access a record field:

```erlang
> P = #person{name = "Joe", phone = [0,8,2,3,4,3,1,2]}.
#person{name = "Joe",phone = [0,8,2,3,4,3,1,2],address = undefined}
> P#person.name.
"Joe"
```

## Updating a Record

The following example shows how to update a record:

```text
> P1 = #person{name="Joe", phone=[1,2,3], address="A street"}.
#person{name = "Joe",phone = [1,2,3],address = "A street"}
> P2 = P1#person{name="Robert"}.
#person{name = "Robert",phone = [1,2,3],address = "A street"}
```

## Type Testing

The following example shows that the guard succeeds if `P` is record of type
`person`:

```text
foo(P) when is_record(P, person) -> a_person;
foo(_) -> not_a_person.
```

## Pattern Matching

Matching can be used in combination with records, as shown in the following
example:

```text
> P3 = #person{name="Joe", phone=[0,0,7], address="A street"}.
#person{name = "Joe",phone = [0,0,7],address = "A street"}
> #person{name = Name} = P3, Name.
"Joe"
```

The following function takes a list of `person` records and searches for the
phone number of a person with a particular name:

```erlang
find_phone([#person{name=Name, phone=Phone} | _], Name) ->
    {found,  Phone};
find_phone([_| T], Name) ->
    find_phone(T, Name);
find_phone([], Name) ->
    not_found.
```

The fields referred to in the pattern can be given in any order.

## Nested Records

The value of a field in a record can be an instance of a record. Retrieval of
nested data can be done stepwise, or in a single step, as shown in the following
example:

```erlang
-record(name, {first = "Robert", last = "Ericsson"}).
-record(person, {name = #name{}, phone}).

demo() ->
  P = #person{name= #name{first="Robert",last="Virding"}, phone=123},
  First = (P#person.name)#name.first.
```

Here, `demo()` evaluates to `"Robert"`.

## A Longer Example

Comments are embedded in the following example:

```erlang
%% File: person.hrl

%%-----------------------------------------------------------
%% Data Type: person
%% where:
%%    name:  A string (default is undefined).
%%    age:   An integer (default is undefined).
%%    phone: A list of integers (default is []).
%%    dict:  A dictionary containing various information
%%           about the person.
%%           A {Key, Value} list (default is the empty list).
%%------------------------------------------------------------
-record(person, {name, age, phone = [], dict = []}).
```

```erlang
-module(person).
-include("person.hrl").
-compile(export_all). % For test purposes only.

%% This creates an instance of a person.
%%   Note: The phone number is not supplied so the
%%         default value [] will be used.

make_hacker_without_phone(Name, Age) ->
   #person{name = Name, age = Age,
           dict = [{computer_knowledge, excellent},
                   {drinks, coke}]}.

%% This demonstrates matching in arguments

print(#person{name = Name, age = Age,
              phone = Phone, dict = Dict}) ->
  io:format("Name: ~s, Age: ~w, Phone: ~w ~n"
            "Dictionary: ~w.~n", [Name, Age, Phone, Dict]).

%% Demonstrates type testing, selector, updating.

birthday(P) when is_record(P, person) ->
   P#person{age = P#person.age + 1}.

register_two_hackers() ->
   Hacker1 = make_hacker_without_phone("Joe", 29),
   OldHacker = birthday(Hacker1),
   % The central_register_server should have
   % an interface function for this.
   central_register_server ! {register_person, Hacker1},
   central_register_server ! {register_person,
             OldHacker#person{name = "Robert",
                              phone = [0,8,3,2,4,5,3,1]}}.
```
