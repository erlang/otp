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

A record is a data structure for storing a fixed number of elements. It has
named fields and is similar to a struct in C. Record expressions are translated
to tuple expressions during compilation. Therefore, record expressions are not
understood by the shell unless special actions are taken. For details, see the
`m:shell` manual page in STDLIB.

More examples are provided in
[Programming Examples](`e:system:prog_ex_records.md`).

## Defining Records

A record definition consists of the name of the record, followed by the field
names of the record. Record and field names must be atoms. Each field can be
given an optional default value. If no default value is supplied, `undefined` is
used.

```erlang
-record(Name, {Field1 [= Value1],
               ...
               FieldN [= ValueN]}).
```

A record definition can be placed anywhere among the attributes and function
declarations of a module, but the definition must come before any usage of the
record.

If a record is used in several modules, it is recommended that the record
definition is placed in an include file.

## Creating Records

The following expression creates a new `Name` record where the value of each
field `FieldI` is the value of evaluating the corresponding expression `ExprI`:

```text
#Name{Field1=Expr1,...,FieldK=ExprK}
```

The fields can be in any order, not necessarily the same order as in the record
definition, and fields can be omitted. Omitted fields get their respective
default value instead.

If several fields are to be assigned the same value, the following construction
can be used:

```text
#Name{Field1=Expr1,...,FieldK=ExprK, _=ExprL}
```

Omitted fields then get the value of evaluating `ExprL` instead of their default
values. This feature is primarily intended to be used to create patterns for ETS
and Mnesia match functions.

_Example:_

```erlang
-record(person, {name, phone, address}).

...

lookup(Name, Tab) ->
    ets:match_object(Tab, #person{name=Name, _='_'}).
```

## Accessing Record Fields

```text
Expr#Name.Field
```

Returns the value of the specified field. `Expr` is to evaluate to a `Name`
record.

The following expression returns the position of the specified field in the
tuple representation of the record:

```text
#Name.Field
```

_Example:_

```erlang
-record(person, {name, phone, address}).

...

lookup(Name, List) ->
    lists:keysearch(Name, #person.name, List).
```

## Updating Records

```text
Expr#Name{Field1=Expr1,...,FieldK=ExprK}
```

`Expr` is to evaluate to a `Name` record. A copy of this record is returned,
with the value of each specified field `FieldI` changed to the value of
evaluating the corresponding expression `ExprI`. All other fields retain their
old values.

## Records in Guards

Since record expressions are expanded to tuple expressions, creating records and
accessing record fields are allowed in guards. However all subexpressions, for
example, for field initiations, must be valid guard expressions as well.

_Examples:_

```erlang
handle(Msg, State) when Msg==#msg{to=void, no=3} ->
    ...

handle(Msg, State) when State#state.running==true ->
    ...
```

There is also a type test BIF [`is_record(Term, RecordTag)`](`is_record/2`).

_Example:_

```erlang
is_person(P) when is_record(P, person) ->
    true;
is_person(_P) ->
    false.
```

## Records in Patterns

A pattern that matches a certain record is created in the same way as a record
is created:

```text
#Name{Field1=Expr1,...,FieldK=ExprK}
```

In this case, one or more of `Expr1`...`ExprK` can be unbound variables.

## Nested Records

Assume the following record definitions:

```erlang
-record(nrec0, {name = "nested0"}).
-record(nrec1, {name = "nested1", nrec0=#nrec0{}}).
-record(nrec2, {name = "nested2", nrec1=#nrec1{}}).

N2 = #nrec2{},
```

Accessing or updating nested records can be written without parentheses:

```text
"nested0" = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
    N0n = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = "nested0a"},
```

which is equivalent to:

```text
"nested0" = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name,
N0n = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0{name = "nested0a"},
```

> #### Change {: .info }
>
> Before Erlang/OTP R14, parentheses were necessary when accessing or updating
> nested records.

## Internal Representation of Records

Record expressions are translated to tuple expressions during compilation. A
record defined as:

```erlang
-record(Name, {Field1,...,FieldN}).
```

is internally represented by the tuple:

```text
{Name,Value1,...,ValueN}
```

Here each `ValueI` is the default value for `FieldI`.

To each module using records, a pseudo function is added during compilation to
obtain information about records:

```erlang
record_info(fields, Record) -> [Field]
record_info(size, Record) -> Size
```

`Size` is the size of the tuple representation, that is, one more than the
number of fields.

In addition, `#Record.Name` returns the index in the tuple representation of
`Name` of the record `Record`.

`Name` must be an atom.
