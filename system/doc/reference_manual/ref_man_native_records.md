<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2026. All Rights Reserved.

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
# Native Records

A native record is a data structure for storing a fixed number of
elements in named fields. Unlike traditional tuple-based records
described in the [previous section](ref_man_records.md), a native
record is a distinct type.

> #### Warning {: .warning }
>
> Native records are considered experimental in Erlang/OTP 29;
> that is, if necessary, there may be incompatible changes in
> Erlang/OTP 30.

> #### Change {: .info }
>
> Native records were introduced in Erlang/OTP 29.

## Defining Native Records

A native-record definition consists of the `#` character followed by
the record name and a set of named fields. Field names must be
atoms. Each field can have an optional default value and type
annotation.

```text
-record #Name{Field1 [= Expr1],
              ...
              FieldN [= ExprN]}.
```

The `#Name` syntax denotes a native record definition. `Name` is an
atom. As opposed to tuple-based records, it is not necessary to
quote atoms that look like variable names or keywords.

_Examples:_


```erlang
-record #div{class}.
-record #State{}.
-record #'42'{}.
```

A default value must be a literal or a simple expression evaluable at
compile time. The expression must not contain variables, function
calls, or record constructions.

A native record definition can be placed anywhere among the attributes
and function declarations of a module, but the definition must appear
before any usage of the record.

By default, the fields of a native record are only accessible within
the defining module. To make them accessible to other modules, the
record must be exported.

A native record should never be defined in a header file.

## Exporting Records

To allow the fields of a record to be used outside its defining
module, the record needs to be exported.

```text
-export_record([Name1, Name2, ..., NameN]).
```

`Name1`, `Name2`, and so on are record names (atoms) defined in the
module that are to be accessible from other modules.

_Example:_

```erlang
-module(vector_lib).
-record #vector{x = 0.0, y = 0.0}.
-record #position{x = 0.0, y = 0.0}.
-export_record([vector, position]).
```

## Importing Records

To use a native record defined in another module without fully
qualifying its name every time, use `-import_record`:

```text
-import_record(Module, [Name1, Name2, ..., NameN]).
```

`Module`, an atom, specifies which module to import records from.
`Name1`, `Name2`, and so on are record names (atoms) to be imported.

_Example:_

```erlang
-module(example).
-import_record(vector_lib, [vector, position]).
```

## Constructing Native Records

The following expression constructs a new `Name` record where the
value of each field `FieldI` is the result of evaluating the
corresponding expression `ExprI`:

```text
#Name{Field1=Expr1, ..., FieldK=ExprK}
#Module:Name{Field1=Expr1, ..., FieldK=ExprK}
```

The fields can be given in any order, not necessarily the same order
as in the record definition, and fields can be omitted. Omitted fields
are assigned their default values.

It is an error if not all fields are given values either explicitly
or through default values. How the error is manifested depends on
whether the record construction is *local* or *external*.

### Local Record Construction

Record construction is *local* if the first form of the syntax is used
(without a module name) **and** a native-record definition for `Name`
appears earlier in the module than the construction expression.

_Example:_

```erlang
-module(example).
-export([make_pair/2]).

-record #pair{a, b}.

make_pair(A, B) ->
    #pair{a=A, b=B}.
```

```erlang
1> example:make_pair(1, 2).
#example:pair{a = 1,b = 2}
```

Not giving values to all fields (explicitly or through default values)
for a local record results in a compilation error.

_Example:_

```text
-module(example).
-export([make_empty_pair/0]).

-record #pair{a, b}.

make_empty_pair() ->
    #pair{}.
```

Attempting to compile this module results in the following errors:

```
$ erlc example.erl
example.erl:7:5: field a is not initialized in native record pair
%    7|     #pair{}.
%     |     ^

example.erl:7:5: field b is not initialized in native record pair
%    7|     #pair{}.
%     |     ^
```

### External Record Construction

Record construction is *external* if either of the following conditions
is true:

* The `#Module:Name{...}` syntax is used.
* The `#Name{...}` syntax is used **and** `Name` has been imported.

_Examples:_

```erlang
-module(pair_library).
-record #pair{a, b}.
-export_record([pair]).
```

Given the previous record definition, a record value can be constructed
from the `example` module by prefixing the record name `pair` with
the module name `pair_library`:

```erlang
-module(example).
-export([make_pair/2]).

make_pair(A, B) ->
    #pair_library:pair{a=A, b=B}.
```

Alternatively, the `pair` record can be imported from `pair_library`:

```erlang
-module(example).
-export([make_pair/2]).

-import_record(pair_library, [pair]).

make_pair(A, B) ->
    #pair{a=A, b=B}.
```

External record construction will fail with an exception if one of the
following conditions is true:

* The referenced module is not loaded.

* The referenced record is not exported.

* Giving a value to a field not present in the record definition.

* Not giving values to all fields.

_Example:_

```erlang
-module(example).
-export([make_empty_pair/0]).

make_empty_pair() ->
    #pair_library:pair{}.
```

Record construction fails with an exception at runtime because no
values are given to fields `a` and `b`:

```text
1> example:make_empty_pair().
** exception error: no value provided for field a in #pair_library:pair{}
```

## Capturing of the Record Definition

When constructing a native record, the record definition is "captured";
that is, included in the created record value. The export status
(whether it was exported) of the record at the time of construction is
also captured.

Subsequent operations on the record (updating, matching, and accessing
individual fields) use the captured record definition, not the
definition in the code. These operations will succeed even if the
module is unloaded, modified and then reloaded, or executed on another
Erlang node where the module containing the record definition has
never been loaded.

In other words, the record definition in the module is used **only**
when creating a record. All other record operations use the record
definition captured at the time the record was constructed.

## Record Field Access

```text
Expr#Name.Field
Expr#Module:Name.Field
```

`Expr` is expected to evaluate to a record value.

If the operation is local (no module name given), the record value
must have the name `Name` and its module must be the same as the name
of the currently executing module.

If the access operation is external (module name given explicitly or
imported), the record value must be named `Name` and be defined in
module `Module`. Furthermore, at the time the record was constructed,
the record must have been exported.

In either case, the `Field` must have existed at the time the record
was constructed.

If the conditions just described are fulfilled, the value of the
specified field is returned. Otherwise, the operation fails. If the
access operation is in a guard, the guard fails; if it is in a
function body, an exception is raised.

_Example_:

```erlang
-record #person{name, phone, address}.

get_person_name(Person) ->
    Person#person.name.

get_vec_x(Vec) ->
    Vec#geom_2d:vec.x.
```

## Anonymous Record Field Access

```text
Expr#_.Field
```

Returns the value of the specified field. `Expr` is expected to
evaluate to a native record.

The operation will fail if the field does not exist in the captured
record definition.

If the module name is not the same as the currently executing module,
the operation will only succeed if the record was exported at the time
the record was constructed.

## Updating Native Records

Updating is *not* construction, so the captured definition will be
used.

```text
Expr#Name{Field1=Expr1, ..., FieldK=ExprK}
Expr#Module:Name{Field1=Expr1, ..., FieldK=ExprK}
```

`Expr` is expected to evaluate to a record value.

If the operation is local (no module name given), the record value
must have the name `Name` and its module must be the same as the name
of the currently executing module.

If the update operation is external (module name given explicitly or
imported), the record value must have the name `Name` and be defined
in the module `Module`.

A copy of this record is returned, with the value of each specified
field `FieldI` changed to the value of evaluating the corresponding
expression `ExprI`. All other fields retain their old values.

The operation will fail with an exception if any of the named fields
do not exist in the record definition captured when the record was
constructed.

An external record update operation will fail with an exception if
the record was not exported from its owning module when the record
was constructed.

## Anonymous Update

```text
Expr#_{Field1=Expr1, ..., FieldK=ExprK}
```

`Expr` is expected to evaluate to a native record.

A copy of this record is returned, with the value of each specified
field `FieldI` changed to the value of evaluating the corresponding
expression `ExprI`. All other fields retain their old values.

The update operation will fail if any of the fields do not exist
in the record definition captured when the record was constructed.

If the module name is not the same as the currently executing module,
the operation will only succeed if the record was exported at the time
of construction.

## The Guard BIF is_record/1

The guard BIF `is_record(Term)` tests whether `Term` is a native
record.

If `Term` is a tuple-based record, `is_record/1` returns `false`.

## The Guard BIF is_record/2

The guard BIF `is_record(Term, Name)`, where `Name` is an atom, tests
whether `Term` is either a native or a tuple-based record with the name
`Name`, defined in the current module or (in the case of a native record)
imported from another module.

_Example:_

```erlang
-record #vec{x, y}.

increment(Vec) when is_record(Vec, vec) ->
    ... .
```

It is often more convenient to use matching instead of using
`is_record/2`:

```erlang
-record #vec{x, y}.

increment(#vec{}=Vec) ->
    ... .
```

Matching only the record name succeeds regardless of whether the
record was exported at the time of construction.

## The Guard BIF is_record/3

The guard BIF `is_record(Term, Module, Name)`, where `Module` and
`Name` are atoms, tests whether `Term` is a native record with the
name `Name` constructed from a definition in module `Module`. This BIF
checks the module name and record name captured in the record at the
time it was constructed.

_Example:_

```erlang
increment(Vec) when is_record(Vec, geom_2d, vec) ->
    ... ;
increment(Vec) when is_record(Vec, geom_3d, vec) ->
    ... .
```

It is often more convenient to use matching instead of `is_record/3`:

```
increment(#geom_2d:vec{}=Vec) ->
    ... ;
increment(#geom_3d:vec{}=Vec) ->
    ... .
```

## Native Records in Guards

Field access is the only native-record operation allowed in guards.

_Example:_

```erlang
handle(Msg, State) when State#state.running =:= true ->
    ...
```

## Records in Patterns

Pattern matching uses the same syntax as construction:

```text
#Name{Field1=Expr1, ..., FieldN=ExprN}
#Module:Name{Field1=Expr1, ..., FieldN=ExprN}
```

In this case, one or more of `Expr1` ... `ExprN` can contain unbound
variables.

Matching will fail if one or more of the field names are not present
in the record definition captured at the time the record was
constructed.

_Examples:_

```erlang
-record #vec{}.

len(#vec{x=X, y=Y}) ->
    math:sqrt(X * X + Y * Y).
```

For an external matching operation, matching can also fail if at least
one field is being matched and, at the time of construction, the record
was not exported. If the list of fields to match is empty, the match
succeeds as long as the module and record names match.

In the following example, the first clause will match if the module
name `geom_2d` and the record name `vec` match, regardless of whether
the record was exported at construction time or not:

```erlang
is_vec(#geom_2d:vec{}) -> true;
is_vec(_) -> false.
```

## Anonymous Pattern Matching

```text
#_{Field1=Pattern1, ..., FieldN=PatternN}
```

If the module name of the native record being matched is the same as
that of the currently executing module, it does not matter whether the
record was exported at the time the record was constructed. As long as all
fields exist and all patterns match, the match succeeds.

If the module name of the native record being matched is not the same
as that of the currently executing module, if at least one field is
being matched, in order for the match operation to succeed, the record
must have been exported at construction time.

## Reflection: The records Module

The `m:records` module contains functions for constructing and
inspecting native records. The main purpose of the functions in the
`records` module is for debugging, implementing library functions
(such as printing of native records), and implementing tools (such as
the Debugger). Use with care in production code.

## Native Records in the Erlang Shell

Native records can be defined and constructed in the Erlang shell
using the same syntax as in module code. All records constructed
in the shell belongs to the `shell_default` module.

_Examples:_

```text
1> -record #pair{a=1, b=1}.
ok
2> #pair{a=42}.
#shell_default:pair{a = 42,b = 1}
```

The shell does not enforce the privacy of non-exported records.
That is, the shell will print non-exported records and it also
allows constructing non-exported records.

For example, consider the following module that defines the
non-exported record `vec`:

```erlang
-module(geometry).
-export([make_vec/2, origin/0, add_vec/2]).
-record #vec{x, y}.

make_vec(X, Y) ->
    #vec{x=X, y=Y}.

origin() ->
    #vec{x=0.0, y=0.0}.

add_vec(#vec{x=X1, y=Y1}, #vec{x=X2,y=Y2}) ->
    #vec{x=X1+X2, y=Y1+Y2}.
```

From the shell, it is possible to both show and construct `vec`
records:

```erlang
1> geometry:make_vec(1.0, 7.0).
#geometry:vec{x = 1.0,y = 7.0}
2> #geometry:vec{x = 100.0, y = 99.0}.
#geometry:vec{x = 100.0,y = 99.0}
```

## Nested Native Records

Assume the following native record definitions:

```erlang
-record #nrec0{name = "nested0"}.
-record #nrec1{name = "nested1", nrec0}.
-record #nrec2{name = "nested2", nrec1}.

N2 = #nrec2{nrec1 = #nrec1{nrec0 = #nrec0{}}},
```

Accessing or updating nested records can be written without parentheses:

```erlang
"nested0" = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
N0n = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = "nested0a"},
```

which is equivalent to:

```erlang
"nested0" = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name,
N0n = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0{name = "nested0a"},
```

## Advanced: Hot Code Updating

When doing hot code updating, it can be necessary to do term
conversion. For example, one might want to add or remove fields in a
native record.

### How to add a field to a native record

Assume that we start out with the following record definition:

```erlang
-module(add_one_field).
-record #rec{a, b, c, d}.
-export([make_rec/4]).

make_rec(A, B, C, D) ->
    #rec{a=A, b=B, c=C, d=D}.
```

Our task is to keep all the existing fields and add the `new` field.
Here is one way to implement an `update/1` function that will take an
old version of the record and return its content in a new version of
the record:

```erlang
-module(add_one_field).
-record #rec{a, b, c, d, new}.
-export([update/1]).

update(#rec{new=_}=AlreadyUpdated) ->
    AlreadyUpdated;
update(#rec{a=A, b=B, c=C, d=D}) ->
    #rec{a=A, b=B, c=C, d=D, new=new_value}.
```

Remember that the captured definition is used when matching a record.
That makes it possible to access all fields in a record constructed by
a previous version of a module.

Let us look at this code in action. First compile the original version
of the module and construct a `rec` record:

```text
1> c(add_one_field).
{ok,add_one_field}.
2> R = make_rec(1, 2, 3, 4).
#add_one_field:rec{a = 1,b = 2,c = 3,d = 4}
```

Next compile the modified version of the module and call
`add_one_field:update(R)` to update the record:

```text
3> c(add_one_field).
{ok,add_one_field}.
4> NewR = add_one_field:update(R).
#add_one_field:rec{a = 1,b = 2,c = 3,d = 4,new = new_value}
```

If the `update/1` function is called on the already updated record, it
will just return its argument:

```text
5> NewR = add_one_field:update(NewR).
#add_one_field:rec{a = 1,b = 2,c = 3,d = 4,new = new_value}
```

### How to delete one field from a native record

Removing a field from a record is not really any more difficult than
adding a field, except that the compiler by default will emit a
warning.

Assume that we start out with the following record definition:

```erlang
-module(delete_one_field).
-record #rec{a, b, c, d, opts=[]}.
-export([make_rec/4]).

make_rec(A, B, C, D) ->
    #rec{a=A, b=B, c=C, d=D}.

```

Our task is to remove the `d` field and incorporate its content into
`opts`. Here is one way to implement an `update/1` function that will
take an old version of the record and return its content in a new
version of the record:

```text
-module(delete_one_field).
-record #rec{a, b, c, opts=[]}.
-export([update/1]).

update(#rec{a=A, b=B, c=C, d=D, opts=Opts}) ->
    #rec{a=A, b=B, c=C, opts=[{d,D} | Opts]};
update(#rec{}=AlreadyUpdated) ->
    AlreadyUpdated.
```

The first clause refers to the `d` field, despite it not being present
in the record definition. That will cause a compilation warning, but if
the field exists in the captured record definition matching will succeed.

```text
$ erlc delete_one_field.erl
delete_one_field.erl:5:28: Warning: field d undefined in record rec
%    6| update(#rec{a=A, b=B, c=C, d=D, opts=Opts}) ->
%     |                            ^
```

The warning can be disabled using the `nowarn_undefined_field` option:

```erlang
-module(delete_one_field).
-compile(nowarn_undefined_field).
-record #rec{a, b, c, opts=[]}.
-export([update/1]).

update(#rec{a=A, b=B, c=C, d=D, opts=Opts}) ->
    #rec{a=A, b=B, c=C, opts=[{d,D} | Opts]};
update(#rec{}=AlreadyUpdated) ->
    AlreadyUpdated.
```

Let us look at this code in action. First compile the original
version of the module and construct a `rec` record:

```text
1> c(delete_one_field).
{ok,delete_one_field}.
2> R = make_rec(1, 2, 3, 4).
#delete_one_field:rec{a = 1,b = 2,c = 3,d = 4,opts = []}
```

Next compile the modified version of the module and call
`delete_one_field:update(R)` to update the record:

```text
3> c(delete_one_field).
{ok,delete_one_field}.
4> NewR = delete_one_field:update(R).
#delete_one_field:rec{a = 1,b = 2,c = 3,opts = [{d,4}]}
```

If the `update/1` function is called on the already updated record, it
will just return its argument:

```text
5> NewR = delete_one_field:update(NewR).
#delete_one_field:rec{a = 1,b = 2,c = 3,opts = [{d,4}]}
```
