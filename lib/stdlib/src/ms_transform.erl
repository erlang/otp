%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2024. All Rights Reserved.
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
-module(ms_transform).
-moduledoc """
A parse transformation that translates fun syntax into match specifications.

[](){: #top }

This module provides the parse transformation that makes calls to `m:ets` and
`dbg:fun2ms/1` translate into literal match specifications. It also provides the
back end for the same functions when called from the Erlang shell.

The translation from funs to match specifications is accessed through the two
"pseudo functions" `ets:fun2ms/1` and `dbg:fun2ms/1`.

As everyone trying to use [`ets:select/2`](`ets:select/1`) or `m:dbg` seems to
end up reading this manual page, this description is an introduction to the
concept of match specifications.

Read the whole manual page if it is the first time you are using the
transformations.

Match specifications are used more or less as filters. They resemble usual
Erlang matching in a list comprehension or in a fun used with `lists:foldl/3`,
and so on. However, the syntax of pure match specifications is awkward, as they
are made up purely by Erlang terms, and the language has no syntax to make the
match specifications more readable.

As the execution and structure of the match specifications are like that of a
fun, it is more straightforward to write it using the familiar fun syntax and to
have that translated into a match specification automatically. A real fun is
clearly more powerful than the match specifications allow, but bearing the match
specifications in mind, and what they can do, it is still more convenient to
write it all as a fun. This module contains the code that translates the fun
syntax into match specification terms.

## Example 1

Using `ets:select/2` and a match specification, one can filter out rows of a
table and construct a list of tuples containing relevant parts of the data in
these rows. One can use `ets:foldl/3` instead, but the `ets:select/2` call is
far more efficient. Without the translation provided by `ms_transform`, one must
struggle with writing match specifications terms to accommodate this.

Consider a simple table of employees:

```erlang
-record(emp, {empno,     %Employee number as a string, the key
              surname,   %Surname of the employee
              givenname, %Given name of employee
              dept,      %Department, one of {dev,sales,prod,adm}
              empyear}). %Year the employee was employed
```

We create the table using:

```text
ets:new(emp_tab, [{keypos,#emp.empno},named_table,ordered_set]).
```

We fill the table with randomly chosen data:

```erlang
[{emp,"011103","Black","Alfred",sales,2000},
 {emp,"041231","Doe","John",prod,2001},
 {emp,"052341","Smith","John",dev,1997},
 {emp,"076324","Smith","Ella",sales,1995},
 {emp,"122334","Weston","Anna",prod,2002},
 {emp,"535216","Chalker","Samuel",adm,1998},
 {emp,"789789","Harrysson","Joe",adm,1996},
 {emp,"963721","Scott","Juliana",dev,2003},
 {emp,"989891","Brown","Gabriel",prod,1999}]
```

Assuming that we want the employee numbers of everyone in the sales department,
there are several ways.

`ets:match/2` can be used:

```erlang
1> ets:match(emp_tab, {'_', '$1', '_', '_', sales, '_'}).
[["011103"],["076324"]]
```

`ets:match/2` uses a simpler type of match specification, but it is still
unreadable, and one has little control over the returned result. It is always a
list of lists.

`ets:foldl/3` or `ets:foldr/3` can be used to avoid the nested lists:

```erlang
ets:foldr(fun(#emp{empno = E, dept = sales},Acc) -> [E | Acc];
             (_,Acc) -> Acc
          end,
          [],
          emp_tab).
```

The result is `["011103","076324"]`. The fun is straightforward, so the only
problem is that all the data from the table must be transferred from the table
to the calling process for filtering. That is inefficient compared to the
`ets:match/2` call where the filtering can be done "inside" the emulator and
only the result is transferred to the process.

Consider a "pure" `ets:select/2` call that does what `ets:foldr` does:

```erlang
ets:select(emp_tab, [{#emp{empno = '$1', dept = sales, _='_'},[],['$1']}]).
```

Although the record syntax is used, it is still hard to read and even harder to
write. The first element of the tuple,
`#emp{empno = '$1', dept = sales, _='_'}`, tells what to match. Elements not
matching this are not returned, as in the `ets:match/2` example. The second
element, the empty list, is a list of guard expressions, which we do not need.
The third element is the list of expressions constructing the return value (in
ETS this is almost always a list containing one single term). In our case `'$1'`
is bound to the employee number in the head (first element of the tuple), and
hence the employee number is returned. The result is `["011103","076324"]`, as
in the `ets:foldr/3` example, but the result is retrieved much more efficiently
in terms of execution speed and memory consumption.

Using `ets:fun2ms/1`, we can combine the ease of use of the `ets:foldr/3` and
the efficiency of the pure `ets:select/2` example:

```erlang
-include_lib("stdlib/include/ms_transform.hrl").

ets:select(emp_tab, ets:fun2ms(
                      fun(#emp{empno = E, dept = sales}) ->
                              E
                      end)).
```

This example requires no special knowledge of match specifications to
understand. The head of the fun matches what you want to filter out and the body
returns what you want returned. As long as the fun can be kept within the limits
of the match specifications, there is no need to transfer all table data to the
process for filtering as in the `ets:foldr/3` example. It is easier to read than
the `ets:foldr/3` example, as the select call in itself discards anything that
does not match, while the fun of the `ets:foldr/3` call needs to handle both the
elements matching and the ones not matching.

In the `ets:fun2ms/1` example above, it is needed to include `ms_transform.hrl`
in the source code, as this is what triggers the parse transformation of the
`ets:fun2ms/1` call to a valid match specification. This also implies that the
transformation is done at compile time (except when called from the shell) and
therefore takes no resources in runtime. That is, although you use the more
intuitive fun syntax, it gets as efficient in runtime as writing match
specifications by hand.

## Example 2

Assume that we want to get all the employee numbers of employees hired before
year 2000. Using `ets:match/2` is not an alternative here, as relational
operators cannot be expressed there. Once again, `ets:foldr/3` can do it
(slowly, but correct):

```erlang
ets:foldr(fun(#emp{empno = E, empyear = Y},Acc) when Y < 2000 -> [E | Acc];
                  (_,Acc) -> Acc
          end,
          [],
          emp_tab).
```

The result is `["052341","076324","535216","789789","989891"]`, as expected. The
equivalent expression using a handwritten match specification would look like
this:

```erlang
ets:select(emp_tab, [{#emp{empno = '$1', empyear = '$2', _='_'},
                     [{'<', '$2', 2000}],
                     ['$1']}]).
```

This gives the same result. `[{'<', '$2', 2000}]` is in the guard part and
therefore discards anything that does not have an `empyear` (bound to `'$2'` in
the head) less than 2000, as the guard in the `foldr/3` example.

We write it using `ets:fun2ms/1`:

```erlang
-include_lib("stdlib/include/ms_transform.hrl").

ets:select(emp_tab, ets:fun2ms(
                      fun(#emp{empno = E, empyear = Y}) when Y < 2000 ->
                           E
                      end)).
```

## Example 3

Assume that we want the whole object matching instead of only one element. One
alternative is to assign a variable to every part of the record and build it up
once again in the body of the fun, but the following is easier:

```erlang
ets:select(emp_tab, ets:fun2ms(
                      fun(Obj = #emp{empno = E, empyear = Y})
                         when Y < 2000 ->
                              Obj
                      end)).
```

As in ordinary Erlang matching, you can bind a variable to the whole matched
object using a "match inside the match", that is, a `=`. Unfortunately in funs
translated to match specifications, it is allowed only at the "top-level", that
is, matching the _whole_ object arriving to be matched into a separate variable.
If you are used to writing match specifications by hand, we mention that
variable A is simply translated into '$\_'. Alternatively, pseudo function
`object/0` also returns the whole matched object, see section
[Warnings and Restrictions](`m:ms_transform#warnings_and_restrictions`).

## Example 4

This example concerns the body of the fun. Assume that all employee numbers
beginning with zero (`0`) must be changed to begin with one (`1`) instead, and
that we want to create the list `[{<Old empno>,<New empno>}]`:

```erlang
ets:select(emp_tab, ets:fun2ms(
                      fun(#emp{empno = [$0 | Rest] }) ->
                              {[$0|Rest],[$1|Rest]}
                      end)).
```

This query hits the feature of partially bound keys in table type `ordered_set`,
so that not the whole table needs to be searched, only the part containing keys
beginning with `0` is looked into.

## Example 5

The fun can have many clauses. Assume that we want to do the following:

- If an employee started before 1997, return the tuple
  `{inventory, <employee number>}`.
- If an employee started 1997 or later, but before 2001, return
  `{rookie, <employee number>}`.
- For all other employees, return `{newbie, <employee number>}`, except for
  those named `Smith` as they would be affronted by anything other than the tag
  `guru` and that is also what is returned for their numbers:
  `{guru, <employee number>}`.

This is accomplished as follows:

```erlang
ets:select(emp_tab, ets:fun2ms(
                      fun(#emp{empno = E, surname = "Smith" }) ->
                              {guru,E};
                         (#emp{empno = E, empyear = Y}) when Y < 1997  ->
                              {inventory, E};
                         (#emp{empno = E, empyear = Y}) when Y > 2001  ->
                              {newbie, E};
                         (#emp{empno = E, empyear = Y}) -> % 1997 -- 2001
                              {rookie, E}
                      end)).
```

The result is as follows:

```erlang
[{rookie,"011103"},
 {rookie,"041231"},
 {guru,"052341"},
 {guru,"076324"},
 {newbie,"122334"},
 {rookie,"535216"},
 {inventory,"789789"},
 {newbie,"963721"},
 {rookie,"989891"}]
```

## Useful BIFs

What more can you do? A simple answer is: see the documentation of
[match specifications](`e:erts:match_spec.md`) in ERTS User's Guide. However,
the following is a brief overview of the most useful "built-in functions" that
you can use when the fun is to be translated into a match specification by
`ets:fun2ms/1`. It is not possible to call other functions than those allowed in
match specifications. No "usual" Erlang code can be executed by the fun that is
translated by `ets:fun2ms/1`. The fun is limited exactly to the power of the
match specifications, which is unfortunate, but the price one must pay for the
execution speed of `ets:select/2` compared to `ets:foldl/foldr`.

The head of the fun is a head matching (or mismatching) _one_ parameter, one
object of the table we select from. The object is always a single variable (can
be `_`) or a tuple, as ETS, Dets, and Mnesia tables include that. The match
specification returned by `ets:fun2ms/1` can be used with `dets:select/2` and
`mnesia:select/2`, and with `ets:select/2`. The use of `=` in the head is
allowed (and encouraged) at the top-level.

The guard section can contain any guard expression of Erlang. The following is a
list of BIFs and expressions:

- Type tests: `is_atom`, `is_float`, `is_integer`, `is_list`, `is_number`,
  `is_pid`, `is_port`, `is_reference`, `is_tuple`, `is_binary`, `is_function`,
  `is_record`
- Boolean operators: `not`, `and`, `or`, `andalso`, `orelse`
- Relational operators: >, >=, <, =<, =:=, ==, =/=, /=
- Arithmetics: `+`, `-`, `*`, `div`, `rem`
- Bitwise operators: `band`, `bor`, `bxor`, `bnot`, `bsl`, `bsr`
- The guard BIFs: `abs`, `element`, `hd`, `length`, `node`, `round`, `size`,
  `byte_size`, `tl`, `trunc`, `binary_part`, `self`

Contrary to the fact with "handwritten" match specifications, the `is_record`
guard works as in ordinary Erlang code.

Semicolons (`;`) in guards are allowed, the result is (as expected) one "match
specification clause" for each semicolon-separated part of the guard. The
semantics is identical to the Erlang semantics.

The body of the fun is used to construct the resulting value. When selecting
from tables, one usually construct a suiting term here, using ordinary Erlang
term construction, like tuple parentheses, list brackets, and variables matched
out in the head, possibly with the occasional constant. Whatever expressions are
allowed in guards are also allowed here, but no special functions exist except
`object` and `bindings` (see further down), which returns the whole matched
object and all known variable bindings, respectively.

The `dbg` variants of match specifications have an imperative approach to the
match specification body, the ETS dialect has not. The fun body for
`ets:fun2ms/1` returns the result without side effects. As matching (`=`) in the
body of the match specifications is not allowed (for performance reasons) the
only thing left, more or less, is term construction.

## Example with dbg

This section describes the slightly different match specifications translated by
`dbg:fun2ms/1`.

The same reasons for using the parse transformation apply to `dbg`, maybe even
more, as filtering using Erlang code is not a good idea when tracing (except
afterwards, if you trace to file). The concept is similar to that of
`ets:fun2ms/1` except that you usually use it directly from the shell (which can
also be done with `ets:fun2ms/1`).

The following is an example module to trace on:

```erlang
-module(toy).

-export([start/1, store/2, retrieve/1]).

start(Args) ->
    toy_table = ets:new(toy_table, Args).

store(Key, Value) ->
    ets:insert(toy_table, {Key,Value}).

retrieve(Key) ->
    [{Key, Value}] = ets:lookup(toy_table, Key),
    Value.
```

During model testing, the first test results in `{badmatch,16}` in
`{toy,start,1}`, why?

We suspect the `ets:new/2` call, as we match hard on the return value, but want
only the particular `new/2` call with `toy_table` as first parameter. So we
start a default tracer on the node:

```text
1> dbg:tracer().
{ok,<0.88.0>}
```

We turn on call tracing for all processes, we want to make a pretty restrictive
trace pattern, so there is no need to call trace only a few processes (usually
it is not):

```text
2> dbg:p(all,call).
{ok,[{matched,nonode@nohost,25}]}
```

We specify the filter, we want to view calls that resemble
`ets:new(toy_table, <something>)`:

```erlang
3> dbg:tp(ets,new,dbg:fun2ms(fun([toy_table,_]) -> true end)).
{ok,[{matched,nonode@nohost,1},{saved,1}]}
```

As can be seen, the fun used with `dbg:fun2ms/1` takes a single list as
parameter instead of a single tuple. The list matches a list of the parameters
to the traced function. A single variable can also be used. The body of the fun
expresses, in a more imperative way, actions to be taken if the fun head (and
the guards) matches. `true` is returned here, only because the body of a fun
cannot be empty. The return value is discarded.

The following trace output is received during test:

```erlang
(<0.86.0>) call ets:new(toy_table, [ordered_set])
```

Assume that we have not found the problem yet, and want to see what `ets:new/2`
returns. We use a slightly different trace pattern:

```erlang
4> dbg:tp(ets,new,dbg:fun2ms(fun([toy_table,_]) -> return_trace() end)).
```

The following trace output is received during test:

```erlang
(<0.86.0>) call ets:new(toy_table,[ordered_set])
(<0.86.0>) returned from ets:new/2 -> 24
```

The call to `return_trace` results in a trace message when the function returns.
It applies only to the specific function call triggering the match specification
(and matching the head/guards of the match specification). This is by far the
most common call in the body of a `dbg` match specification.

The test now fails with `{badmatch,24}` because the atom `toy_table` does not
match the number returned for an unnamed table. So, the problem is found, the
table is to be named, and the arguments supplied by the test program do not
include `named_table`. We rewrite the start function:

```erlang
start(Args) ->
    toy_table = ets:new(toy_table, [named_table|Args]).
```

With the same tracing turned on, the following trace output is received:

```erlang
(<0.86.0>) call ets:new(toy_table,[named_table,ordered_set])
(<0.86.0>) returned from ets:new/2 -> toy_table
```

Assume that the module now passes all testing and goes into the system. After a
while, it is found that table `toy_table` grows while the system is running and
that there are many elements with atoms as keys. We expected only integer keys
and so does the rest of the system, but clearly not the entire system. We turn
on call tracing and try to see calls to the module with an atom as the key:

```erlang
1> dbg:tracer().
{ok,<0.88.0>}
2> dbg:p(all,call).
{ok,[{matched,nonode@nohost,25}]}
3> dbg:tpl(toy,store,dbg:fun2ms(fun([A,_]) when is_atom(A) -> true end)).
{ok,[{matched,nonode@nohost,1},{saved,1}]}
```

We use `dbg:tpl/3` to ensure to catch local calls (assume that the module has
grown since the smaller version and we are unsure if this inserting of atoms is
not done locally). When in doubt, always use local call tracing.

Assume that nothing happens when tracing in this way. The function is never
called with these parameters. We conclude that someone else (some other module)
is doing it and realize that we must trace on `ets:insert/2` and want to see the
calling function. The calling function can be retrieved using the match
specification function `caller`. To get it into the trace message, the match
specification function `message` must be used. The filter call looks like this
(looking for calls to `ets:insert/2`):

```erlang
4> dbg:tpl(ets,insert,dbg:fun2ms(fun([toy_table,{A,_}]) when is_atom(A) ->
                                    message(caller())
                                  end)).
{ok,[{matched,nonode@nohost,1},{saved,2}]}
```

The caller is now displayed in the "additional message" part of the trace
output, and the following is displayed after a while:

```erlang
(<0.86.0>) call ets:insert(toy_table,{garbage,can}) ({evil_mod,evil_fun,2})
```

You have realized that function `evil_fun` of the `evil_mod` module, with arity
`2`, is causing all this trouble.

This example illustrates the most used calls in match specifications for `dbg`.
The other, more esoteric, calls are listed and explained in
[Match specifications in Erlang](`e:erts:match_spec.md`) in ERTS User's Guide,
as they are beyond the scope of this description.

## Warnings and Restrictions

[](){: #warnings_and_restrictions }

The following warnings and restrictions apply to the funs used in with
`ets:fun2ms/1` and `dbg:fun2ms/1`.

> #### Warning {: .warning }
>
> To use the pseudo functions triggering the translation, ensure to include the
> header file `ms_transform.hrl` in the source code. Failure to do so possibly
> results in runtime errors rather than compile time, as the expression can be
> valid as a plain Erlang program without translation.

> #### Warning {: .warning }
>
> The fun must be literally constructed inside the parameter list to the pseudo
> functions. The fun cannot be bound to a variable first and then passed to
> `ets:fun2ms/1` or `dbg:fun2ms/1`. For example, `ets:fun2ms(fun(A) -> A end)`
> works, but not `F = fun(A) -> A end, ets:fun2ms(F)`. The latter results in a
> compile-time error if the header is included, otherwise a runtime error.

Many restrictions apply to the fun that is translated into a match
specification. To put it simple: you cannot use anything in the fun that you
cannot use in a match specification. This means that, among others, the
following restrictions apply to the fun itself:

- Functions written in Erlang cannot be called, neither can local functions,
  global functions, or real funs.
- Everything that is written as a function call is translated into a match
  specification call to a built-in function, so that the call
  [`is_list(X)`](`is_list/1`) is translated to `{'is_list', '$1'}` (`'$1'` is
  only an example, the numbering can vary). If one tries to call a function that
  is not a match specification built-in, it causes an error.
- Variables occurring in the head of the fun are replaced by match specification
  variables in the order of occurrence, so that fragment `fun({A,B,C})` is
  replaced by `{'$1', '$2', '$3'}`, and so on. Every occurrence of such a
  variable in the match specification is replaced by a match specification
  variable in the same way, so that the fun
  `fun({A,B}) when is_atom(A) -> B end` is translated into
  `[{{'$1','$2'},[{is_atom,'$1'}],['$2']}]`.
- Variables that are not included in the head are imported from the environment
  and made into match specification `const` expressions. Example from the shell:

  ```erlang
  1> X = 25.
  25
  2> ets:fun2ms(fun({A,B}) when A > X -> B end).
  [{{'$1','$2'},[{'>','$1',{const,25}}],['$2']}]
  ```

- Matching with `=` cannot be used in the body. It can only be used on the
  top-level in the head of the fun. Example from the shell again:

  ```erlang
  1> ets:fun2ms(fun({A,[B|C]} = D) when A > B -> D end).
  [{{'$1',['$2'|'$3']},[{'>','$1','$2'}],['$_']}]
  2> ets:fun2ms(fun({A,[B|C]=D}) when A > B -> D end).
  Error: fun with head matching ('=' in head) cannot be translated into
  match_spec
  {error,transform_error}
  3> ets:fun2ms(fun({A,[B|C]}) when A > B -> D = [B|C], D end).
  Error: fun with body matching ('=' in body) is illegal as match_spec
  {error,transform_error}
  ```

  All variables are bound in the head of a match specification, so the
  translator cannot allow multiple bindings. The special case when matching is
  done on the top-level makes the variable bind to `'$_'` in the resulting match
  specification. It is to allow a more natural access to the whole matched
  object. Pseudo function `object()` can be used instead, see below.

  The following expressions are translated equally:

  ```erlang
  ets:fun2ms(fun({a,_} = A) -> A end).
  ets:fun2ms(fun({a,_}) -> object() end).
  ```

- The special match specification variables `'$_'` and `'$*'` can be accessed
  through the pseudo functions `object()` (for `'$_'`) and `bindings()` (for
  `'$*'`). As an example, one can translate the following `ets:match_object/2`
  call to a `ets:select/2` call:

  ```erlang
  ets:match_object(Table, {'$1',test,'$2'}).
  ```

  This is the same as:

  ```erlang
  ets:select(Table, ets:fun2ms(fun({A,test,B}) -> object() end)).
  ```

  In this simple case, the former expression is probably preferable in terms of
  readability.

  The `ets:select/2` call conceptually looks like this in the resulting code:

  ```erlang
  ets:select(Table, [{{'$1',test,'$2'},[],['$_']}]).
  ```

  Matching on the top-level of the fun head can be a more natural way to access
  `'$_'`, see above.

- Term constructions/literals are translated as much as is needed to get them
  into valid match specification. This way tuples are made into match
  specification tuple constructions (a one element tuple containing the tuple)
  and constant expressions are used when importing variables from the
  environment. Records are also translated into plain tuple constructions, calls
  to element, and so on. The guard test [`is_record/2`](`is_record/2`) is
  translated into match specification code using the three parameter version
  that is built into match specification, so that
  [`is_record(A,t)`](`is_record/2`) is translated into `{is_record,'$1',t,5}` if
  the record size of record type `t` is 5.
- Language constructions such as `case`, `if`, and `catch` that are not present
  in match specifications are not allowed.
- If header file `ms_transform.hrl` is not included, the fun is not translated,
  which can result in a _runtime error_ (depending on whether the fun is valid
  in a pure Erlang context).

  Ensure that the header is included when using `ets` and `dbg:fun2ms/1` in
  compiled code.

- If pseudo function triggering the translation is `ets:fun2ms/1`, the head of
  the fun must contain a single variable or a single tuple. If the pseudo
  function is `dbg:fun2ms/1`, the head of the fun must contain a single variable
  or a single list.

The translation from funs to match specifications is done at compile time, so
runtime performance is not affected by using these pseudo functions.

For more information about match specifications, see the
[Match specifications in Erlang](`e:erts:match_spec.md`) in ERTS User's Guide.
""".

-export([format_error/1,transform_from_shell/3,
         parse_transform/2,parse_transform_info/0]).

%% Error codes.
-define(ERROR_BASE_GUARD,0).
-define(ERROR_BASE_BODY,100).
-define(ERR_NOFUN,1).
-define(ERR_ETS_HEAD,2).
-define(ERR_DBG_HEAD,3).
-define(ERR_HEADMATCH,4).
-define(ERR_SEMI_GUARD,5).
-define(ERR_UNBOUND_VARIABLE,6).
-define(ERR_HEADBADREC,7).
-define(ERR_HEADBADFIELD,8).
-define(ERR_HEADMULTIFIELD,9).
-define(ERR_HEADDOLLARATOM,10).
-define(ERR_HEADBINMATCH,11).
-define(ERR_GENMATCH,16).
-define(ERR_GENLOCALCALL,17).
-define(ERR_GENELEMENT,18).
-define(ERR_GENBADFIELD,19).
-define(ERR_GENBADREC,20).
-define(ERR_GENMULTIFIELD,21).
-define(ERR_GENREMOTECALL,22).
-define(ERR_GENBINCONSTRUCT,23).
-define(ERR_GENDISALLOWEDOP,24).
-define(WARN_SHADOW_VAR,50).
-define(ERR_GUARDMATCH,?ERR_GENMATCH+?ERROR_BASE_GUARD).
-define(ERR_BODYMATCH,?ERR_GENMATCH+?ERROR_BASE_BODY).
-define(ERR_GUARDLOCALCALL,?ERR_GENLOCALCALL+?ERROR_BASE_GUARD).
-define(ERR_BODYLOCALCALL,?ERR_GENLOCALCALL+?ERROR_BASE_BODY).
-define(ERR_GUARDELEMENT,?ERR_GENELEMENT+?ERROR_BASE_GUARD).
-define(ERR_BODYELEMENT,?ERR_GENELEMENT+?ERROR_BASE_BODY).
-define(ERR_GUARDBADFIELD,?ERR_GENBADFIELD+?ERROR_BASE_GUARD).
-define(ERR_BODYBADFIELD,?ERR_GENBADFIELD+?ERROR_BASE_BODY).
-define(ERR_GUARDBADREC,?ERR_GENBADREC+?ERROR_BASE_GUARD).
-define(ERR_BODYBADREC,?ERR_GENBADREC+?ERROR_BASE_BODY).
-define(ERR_GUARDMULTIFIELD,?ERR_GENMULTIFIELD+?ERROR_BASE_GUARD).
-define(ERR_BODYMULTIFIELD,?ERR_GENMULTIFIELD+?ERROR_BASE_BODY).
-define(ERR_GUARDREMOTECALL,?ERR_GENREMOTECALL+?ERROR_BASE_GUARD).
-define(ERR_BODYREMOTECALL,?ERR_GENREMOTECALL+?ERROR_BASE_BODY).
-define(ERR_GUARDBINCONSTRUCT,?ERR_GENBINCONSTRUCT+?ERROR_BASE_GUARD).
-define(ERR_BODYBINCONSTRUCT,?ERR_GENBINCONSTRUCT+?ERROR_BASE_BODY).
-define(ERR_GUARDDISALLOWEDOP,?ERR_GENDISALLOWEDOP+?ERROR_BASE_GUARD).
-define(ERR_BODYDISALLOWEDOP,?ERR_GENDISALLOWEDOP+?ERROR_BASE_BODY).

%%
%% Called by compiler or ets/dbg:fun2ms when errors/warnings occur
%%

-doc """
Takes an error code returned by one of the other functions in the module and
creates a textual description of the error.
""".
-spec(format_error(Error) -> Chars when
      Error :: {error, module(), term()},
      Chars :: io_lib:chars()).

format_error({?WARN_SHADOW_VAR,Name}) ->
    lists:flatten(
      io_lib:format("variable ~p shadowed in ms_transform fun head",
		    [Name]));

format_error(?ERR_NOFUN) ->	    
    "Parameter of ets/dbg:fun2ms/1 is not a literal fun";
format_error(?ERR_ETS_HEAD) ->	    
    "ets:fun2ms requires fun with single variable or tuple parameter";
format_error(?ERR_DBG_HEAD) ->	    
    "dbg:fun2ms requires fun with single variable or list parameter";
format_error(?ERR_HEADMATCH) ->	    
    "in fun head, only matching (=) on toplevel can be translated into match_spec";
format_error(?ERR_SEMI_GUARD) ->	    
    "fun with semicolon (;) in guard cannot be translated into match_spec";
format_error(?ERR_GUARDMATCH) ->	    
    "fun with guard matching ('=' in guard) is illegal as match_spec as well";
format_error({?ERR_GUARDLOCALCALL, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the local function call "
				"'~tw/~w' (called in guard) "
				"cannot be translated into match_spec",
				[Name, Arithy]));
format_error({?ERR_GUARDREMOTECALL, Module, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the remote function call "
				"'~w:~tw/~w' (called in guard) "
				"cannot be translated into match_spec",
				[Module,Name,Arithy]));
format_error({?ERR_GUARDELEMENT, Str}) ->
    lists:flatten(
      io_lib:format("the language element ~ts (in guard) cannot be translated "
		    "into match_spec", [Str]));
format_error({?ERR_GUARDBINCONSTRUCT, Var}) ->
    lists:flatten(
      io_lib:format("bit syntax construction with variable ~w (in guard) "
		    "cannot be translated "
		    "into match_spec", [Var]));
format_error({?ERR_GUARDDISALLOWEDOP, Operator}) ->
    %% There is presently no operators that are allowed in bodies but
    %% not in guards.
    lists:flatten(
      io_lib:format("the operator ~w is not allowed in guards", [Operator]));
format_error(?ERR_BODYMATCH) ->	    
    "fun with body matching ('=' in body) is illegal as match_spec";
format_error({?ERR_BODYLOCALCALL, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the local function "
				"call '~tw/~w' (called in body) "
				"cannot be translated into match_spec",
				[Name,Arithy]));
format_error({?ERR_BODYREMOTECALL, Module, Name, Arithy}) ->	    
    lists:flatten(io_lib:format("fun containing the remote function call "
				"'~w:~tw/~w' (called in body) "
				"cannot be translated into match_spec",
				[Module,Name,Arithy]));
format_error({?ERR_BODYELEMENT, Str}) ->
    lists:flatten(
      io_lib:format("the language element ~ts (in body) cannot be translated "
		    "into match_spec", [Str]));
format_error({?ERR_BODYBINCONSTRUCT, Var}) ->
    lists:flatten(
      io_lib:format("bit syntax construction with variable ~w (in body) "
		    "cannot be translated "
		    "into match_spec", [Var]));
format_error({?ERR_BODYDISALLOWEDOP, Operator}) -> 
    %% This will probably never happen, Are there op's that are allowed in 
    %% guards but not in bodies? Not at time of writing anyway...
    lists:flatten(
      io_lib:format("the operator ~w is not allowed in function bodies", 
		    [Operator]));

format_error({?ERR_UNBOUND_VARIABLE, Str}) ->
    lists:flatten(
      io_lib:format("the variable ~s is unbound, cannot translate "
		    "into match_spec", [Str]));
format_error({?ERR_HEADBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun head contains unknown record type ~tw",[Name]));
format_error({?ERR_HEADBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun head contains reference to unknown field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_HEADMULTIFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun head contains already defined field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_HEADDOLLARATOM,Atom}) ->	    
    lists:flatten(
      io_lib:format("fun head contains atom ~w, which conflics with reserved "
		    "atoms in match_spec heads",[Atom]));
format_error({?ERR_HEADBINMATCH,Atom}) ->	    
    lists:flatten(
      io_lib:format("fun head contains bit syntax matching of variable ~w, "
		    "which cannot be translated into match_spec", [Atom]));
format_error({?ERR_GUARDBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains unknown record type ~tw",[Name]));
format_error({?ERR_GUARDBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains reference to unknown field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_GUARDMULTIFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains already defined field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_BODYBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun body contains unknown record type ~tw",[Name]));
format_error({?ERR_BODYBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun body contains reference to unknown field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error({?ERR_BODYMULTIFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun body contains already defined field ~tw in "
		    "record type ~tw",[FName, RName]));
format_error(Else) ->
    lists:flatten(io_lib:format("Unknown error code ~tw",[Else])).

-doc false.
-spec parse_transform_info() -> #{'error_location' => 'column'}.

parse_transform_info() ->
    #{error_location => column}.

%%
%% Called when translating in shell
%%

-doc """
Implements the transformation when the `fun2ms/1` functions are called from the
shell. In this case, the abstract form is for one single fun (parsed by the
Erlang shell). All imported variables are to be in the key-value list passed as
`BoundEnvironment`. The result is a term, normalized, that is, not in abstract
format.
""".
-spec transform_from_shell(Dialect, Clauses, BoundEnvironment) -> term() when
      Dialect :: ets | dbg,
      Clauses :: [erl_parse:abstract_clause()],
      BoundEnvironment :: erl_eval:binding_struct().

transform_from_shell(Dialect, Clauses, BoundEnvironment) ->
    SaveFilename = setup_filename(),
    case catch ms_clause_list(1,Clauses,Dialect,gb_sets:new()) of
	{'EXIT',Reason} ->
	    cleanup_filename(SaveFilename),
	    exit(Reason);
	{error,AnnoOrUnknown,R} ->
	    {error, [{cleanup_filename(SaveFilename),
		      [{location(AnnoOrUnknown), ?MODULE, R}]}], []};
	Else ->
            case (catch fixup_environment(Else,BoundEnvironment)) of
                {error,AnnoOrUnknown1,R1} ->
                    {error, [{cleanup_filename(SaveFilename),
                             [{location(AnnoOrUnknown1), ?MODULE, R1}]}], []};
                Else1 ->
		    Ret = normalise(Else1),
                    cleanup_filename(SaveFilename),
		    Ret
            end
    end.
    

%%
%% Called when translating during compiling
%%

-doc """
Implements the transformation at compile time. This function is called by the
compiler to do the source code transformation if and when header file
`ms_transform.hrl` is included in the source code.

For information about how to use this parse transformation, see `m:ets` and
`dbg:fun2ms/1`.

For a description of match specifications, see section
[Match Specification in Erlang](`e:erts:match_spec.md`) in ERTS User's Guide.
""".
-spec parse_transform(Forms, Options) -> Forms2 | Errors | Warnings when
      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      Forms2 :: [erl_parse:abstract_form() | erl_parse:form_info()],
      Options :: term(),
      Errors :: {error, ErrInfo :: [tuple()], WarnInfo :: []},
      Warnings :: {warning, Forms2, WarnInfo :: [tuple()]}.

parse_transform(Forms, _Options) ->
    SaveFilename = setup_filename(),
    %io:format("Forms: ~p~n",[Forms]),
    case catch forms(Forms) of
	{'EXIT',Reason} ->
	    cleanup_filename(SaveFilename),
	    exit(Reason);
	{error,AnnoOrUnknown,R} ->
	    {error, [{cleanup_filename(SaveFilename),
		      [{location(AnnoOrUnknown), ?MODULE, R}]}], []};
	Else ->
	    %io:format("Transformed into: ~p~n",[Else]),
	    case get_warnings() of
		[] ->
		    cleanup_filename(SaveFilename),
		    Else;
		WL ->
		    FName = cleanup_filename(SaveFilename) ,
		    WList = [ {FName, [{L, ?MODULE, R}]} || {L,R} <- WL ],
		    {warning, Else, WList}
	    end
    end.

location(unknown) ->
    none;
location(Anno) ->
    erl_anno:location(Anno).

get_warnings() ->
    case get(warnings) of
	undefined ->
	    [];
	Else ->
	    Else
    end.

add_warning(Location,R) ->
    put(warnings,[{Location,R}| get_warnings()]).

setup_filename() ->
    {erase(filename),erase(records),erase(warnings)}.

put_filename(Name) ->
    put(filename,Name).

put_records(R) ->
    put(records,R),
    ok.

get_records() ->
    case get(records) of
	undefined ->
	    [];
	Else ->
	    Else
    end.

get_record(RName) ->
    case lists:keyfind(RName, 1, get_records()) of
        {RName, FieldList} ->
            put(records_replaced_by_tuples,
                [RName|get(records_replaced_by_tuples)]),
            FieldList;
        false ->
            not_found
    end.

cleanup_filename({Old,OldRec,OldWarnings}) ->
    Ret = case erase(filename) of
	      undefined ->
		  "TOP_LEVEL";
	      X ->
		  X
	  end,
    case OldRec of
	undefined ->
	    erase(records);
	Rec ->
	    put(records,Rec)
    end,
    case OldWarnings of
	undefined ->
	    erase(warnings);
	Warn ->
	    put(warnings,Warn)
    end,
    case Old of
	undefined ->
	    Ret;
	Y ->
	    put(filename,Y),
	    Ret
    end.

add_record_definition({Name,FieldList}) ->
    {KeyList,_} = lists:foldl(
                    fun(F, {L,C}) -> {[record_field(F, C)|L],C+1} end,
		    {[],2},
		    FieldList),
    put_records([{Name,KeyList}|get_records()]).

record_field({record_field,_,{atom,Anno0,FieldName}}, C) ->
    {FieldName,C,{atom,Anno0,undefined}};
record_field({record_field,_,{atom,_,FieldName},Def}, C) ->
    {FieldName,C,Def};
record_field({typed_record_field,Field,_Type}, C) ->
    record_field(Field, C).

forms(Forms0) ->
    put(records_replaced_by_tuples, []),
    try
        Forms = [form(F) || F <- Forms0],
        %% Add `-compile({nowarn_unused_record, RecordNames}).', where
        %% RecordNames is the names of all records replaced by tuples,
        %% in order to silence the code linter's warnings about unused
        %% records.
        case get(records_replaced_by_tuples) of
            [] ->
                Forms;
            RNames ->
                NoWarn = {nowarn_unused_record,[lists:usort(RNames)]},
                [{attribute,erl_anno:new(0),compile,NoWarn}] ++ Forms
        end
    after
        erase(records_replaced_by_tuples)
    end.

form({attribute,_,file,{Filename,_}}=Form) ->
    put_filename(Filename),
    Form;
form({attribute,_,record,Definition}=Form) -> 
    add_record_definition(Definition),
    Form;
form({function,Anno,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Anno,Name,Arity,Clauses};
form(AnyOther) ->
    AnyOther.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name,Arity,Clauses1}.

clauses([C0|Cs]) ->
    C1 = clause(C0,gb_sets:new()),
    C2 = clauses(Cs),
    [C1|C2];
clauses([]) -> [].

clause({clause,Anno,H0,G0,B0},Bound) ->
    {H1,Bound1} = copy(H0,Bound),
    {B1,_Bound2} = copy(B0,Bound1),
    {clause,Anno,H1,G0,B1}.

copy({call,Anno,{remote,_Anno2,{atom,_Anno3,ets},{atom,_Anno4,fun2ms}},
      As0},Bound) ->
    {transform_call(ets,Anno,As0,Bound),Bound};
copy({call,Anno,{remote,_Anno2,{atom,_Anno3,dbg},{atom,_Anno4,fun2ms}},
      As0},Bound) ->
    {transform_call(dbg,Anno,As0,Bound),Bound};
copy({match,Anno,A,B},Bound) ->
    {B1,Bound1} = copy(B,Bound),
    {A1,Bound2} = copy(A,Bound),
    {{match,Anno,A1,B1},gb_sets:union(Bound1,Bound2)};
copy({var,_Anno,'_'} = VarDef,Bound) ->
    {VarDef,Bound};
copy({var,_Anno,Name} = VarDef,Bound) ->
    Bound1 = gb_sets:add(Name,Bound),
    {VarDef,Bound1};
copy({'fun',Anno,{clauses,Clauses}},Bound) -> % Dont export bindings from funs
    {NewClauses,_IgnoredBindings} = copy_list(Clauses,Bound),
    {{'fun',Anno,{clauses,NewClauses}},Bound};
copy({named_fun,Anno,Name,Clauses},Bound) -> % Dont export bindings from funs
    Bound1 = case Name of
                 '_' -> Bound;
                 Name -> gb_sets:add(Name,Bound)
             end,
    {NewClauses,_IgnoredBindings} = copy_list(Clauses,Bound1),
    {{named_fun,Anno,Name,NewClauses},Bound};
copy({'case',Anno,Of,ClausesList},Bound) -> % Dont export bindings from funs
    {NewOf,NewBind0} = copy(Of,Bound),
    {NewClausesList,NewBindings} = copy_case_clauses(ClausesList,NewBind0,[]),
    {{'case',Anno,NewOf,NewClausesList},NewBindings};
copy(T,Bound) when is_tuple(T) ->
    {L,Bound1} = copy_list(tuple_to_list(T),Bound),
    {list_to_tuple(L),Bound1};
copy(L,Bound) when is_list(L) ->
    copy_list(L,Bound);
copy(AnyOther,Bound) ->
    {AnyOther,Bound}.

copy_case_clauses([],Bound,AddSets) ->
    ReallyAdded = gb_sets:intersection(AddSets),
    {[],gb_sets:union(Bound,ReallyAdded)};
copy_case_clauses([{clause,Anno,Match,Guard,Clauses}|T],Bound,AddSets) ->
    {NewMatch,MatchBinds} = copy(Match,Bound),
    {NewGuard,GuardBinds} = copy(Guard,MatchBinds), %% Really no new binds
    {NewClauses,AllBinds} = copy(Clauses,GuardBinds),
    %% To limit the setsizes, I subtract what I had before the case clause
    %% and add it in the end
    AddedBinds = gb_sets:subtract(AllBinds,Bound),
    {NewTail,ExportedBindings} =
	copy_case_clauses(T,Bound,[AddedBinds | AddSets]),
    {[{clause,Anno,NewMatch,NewGuard,NewClauses}|NewTail],ExportedBindings}.

copy_list([H|T],Bound) ->
    {C1,Bound1} = copy(H,Bound),
    {C2,Bound2} = copy_list(T,Bound1),
    {[C1|C2],Bound2};
copy_list([],Bound) ->
    {[],Bound}.

transform_call(Type,_Anno,[{'fun',Anno2,{clauses, ClauseList}}],Bound) ->
    ms_clause_list(Anno2, ClauseList,Type,Bound);
transform_call(_Type,Anno,_NoAbstractFun,_) ->
    throw({error,Anno,?ERR_NOFUN}).

% Fixup semicolons in guards
ms_clause_expand({clause, Anno, Parameters, Guard = [_,_|_], Body}) ->
    [ {clause, Anno, Parameters, [X], Body} || X <- Guard ];
ms_clause_expand(_Other) ->
    false.

ms_clause_list(Anno,[H|T],Type,Bound) ->
    case ms_clause_expand(H) of
	NewHead when is_list(NewHead) ->
	    ms_clause_list(Anno,NewHead ++ T, Type, Bound);
	false ->
	    {cons, Anno, ms_clause(H, Type, Bound),
	     ms_clause_list(Anno, T, Type, Bound)}
    end;
ms_clause_list(Anno,[],_,_) ->
    {nil,Anno}.
ms_clause({clause, Anno, Parameters, Guards, Body},Type,Bound) ->
    check_type(Anno,Parameters,Type),
    {MSHead,Bindings} = transform_head(Parameters,Bound),
    MSGuards = transform_guards(Anno, Guards, Bindings),
    MSBody = transform_body(Anno,Body,Bindings),
    {tuple, Anno, [MSHead,MSGuards,MSBody]}.


check_type(_,[{var,_,_}],_) ->
    ok;
check_type(_,[{tuple,_,_}],ets) ->
    ok;
check_type(_,[{record,_,_,_}],ets) ->
    ok;
check_type(_,[{cons,_,_,_}],dbg) ->
    ok;
check_type(_,[{nil,_}],dbg) ->
    ok;
check_type(Anno0,[{match,_,{var,_,_},X}],Any) ->
    check_type(Anno0,[X],Any);
check_type(Anno0,[{match,_,X,{var,_,_}}],Any) ->
    check_type(Anno0,[X],Any);
check_type(Anno,_Type,ets) ->
    throw({error,Anno,?ERR_ETS_HEAD});
check_type(Anno,_,dbg) ->
    throw({error,Anno,?ERR_DBG_HEAD}).

-record(tgd,{ b, %Bindings 
	      p, %Part of spec
	      eb %Error code base, 0 for guards, 100 for bodies
	     }).

transform_guards(Anno,[],_Bindings) ->
    {nil,Anno};
transform_guards(Anno,[G],Bindings) ->
    B = #tgd{b = Bindings, p = guard, eb = ?ERROR_BASE_GUARD},
    tg0(Anno,G,B);
transform_guards(Anno,_,_) ->
    throw({error,Anno,?ERR_SEMI_GUARD}).
    
transform_body(Anno,Body,Bindings) ->
    B = #tgd{b = Bindings, p = body, eb = ?ERROR_BASE_BODY},
    tg0(Anno,Body,B).
    

guard_top_trans({call,Anno0,{atom,Anno1,OldTest},Params}) ->
    case old_bool_test(OldTest,length(Params)) of
	undefined ->
	    {call,Anno0,{atom,Anno1,OldTest},Params};
	Trans ->
	    {call,Anno0,{atom,Anno1,Trans},Params}
    end;
guard_top_trans(Else) ->
    Else.

tg0(Anno,[],_) ->
    {nil,Anno};
tg0(Anno,[H0|T],B) when B#tgd.p =:= guard ->
    H = guard_top_trans(H0),
    {cons,Anno, tg(H,B), tg0(Anno,T,B)};
tg0(Anno,[H|T],B) ->
    {cons,Anno, tg(H,B), tg0(Anno,T,B)}.
    

tg({match,Anno,_,_},B) ->
    throw({error,Anno,?ERR_GENMATCH+B#tgd.eb});
tg({op, Anno, Operator, O1, O2}=Expr, B) ->
    case erl_eval:partial_eval(Expr) of
        Expr ->
            {tuple, Anno, [{atom, Anno, Operator}, tg(O1, B), tg(O2, B)]};
        Value ->
            Value
    end;
tg({op, Anno, Operator, O1}=Expr, B) ->
    case erl_eval:partial_eval(Expr) of
        Expr ->
            {tuple, Anno, [{atom, Anno, Operator}, tg(O1, B)]};
        Value ->
            Value
    end;
tg({call, _Anno, {atom, Anno2, bindings},[]},_B) ->
    {atom, Anno2, '$*'};
tg({call, _Anno, {atom, Anno2, object},[]},_B) ->
    {atom, Anno2, '$_'};
tg({call, Anno, {atom, _, is_record}=Call,[Object, {atom,Anno3,RName}=R]},B) ->
    MSObject = tg(Object,B),
    case get_record(RName) of
	FieldList when is_list(FieldList) ->
	    RSize = length(FieldList)+1,
	    {tuple, Anno, [Call, MSObject, R, {integer, Anno3, RSize}]};
	not_found ->
	    throw({error,Anno3,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;
tg({call, Anno, {atom, Anno2, FunName},ParaList},B) ->
    case is_ms_function(FunName,length(ParaList), B#tgd.p) of
	true ->
	    {tuple, Anno, [{atom, Anno2, FunName} |
			   lists:map(fun(X) -> tg(X,B) end, ParaList)]};
	_ ->
	    throw({error,Anno,{?ERR_GENLOCALCALL+B#tgd.eb,
			       FunName,length(ParaList)}}) 
    end;
tg({call, Anno, {remote,_,{atom,_,erlang},{atom, Anno2, FunName}},ParaList},
   B) ->
    L = length(ParaList),
    case is_imported_from_erlang(FunName,L,B#tgd.p) of
	true ->
	    case is_operator(FunName,L,B#tgd.p) of
		false ->
		    tg({call, Anno, {atom, Anno2, FunName},ParaList},B);
		true ->
		    tg(list_to_tuple([op,Anno2,FunName | ParaList]),B)
		end;
	_ ->
	    throw({error,Anno,{?ERR_GENREMOTECALL+B#tgd.eb,erlang,
			       FunName,length(ParaList)}}) 
    end;
tg({call, Anno, {remote,_,{atom,_,ModuleName},
		 {atom, _, FunName}},ParaList},B) ->
    throw({error,Anno,{?ERR_GENREMOTECALL+B#tgd.eb,ModuleName,FunName,length(ParaList)}});
tg({cons,Anno, H, T},B) ->
    {cons, Anno, tg(H,B), tg(T,B)};
tg({nil, Anno},_B) ->
    {nil, Anno};
tg({tuple,Anno,L},B) ->
    {tuple,Anno,[{tuple,Anno,lists:map(fun(X) -> tg(X,B) end, L)}]};
tg({integer,Anno,I},_) ->
    {integer,Anno,I};
tg({char,Anno,C},_) ->
    {char,Anno,C};
tg({float, Anno,F},_) ->
    {float,Anno,F};
tg({atom,Anno,A},_) ->
    case atom_to_list(A) of
	[$$|_] ->
	   {tuple, Anno,[{atom, Anno, 'const'},{atom,Anno,A}]};
	_ ->
	    {atom,Anno,A}
    end;
tg({string,Anno,S},_) ->
    {string,Anno,S};
tg({var,Anno,VarName},B) ->
    case lkup_bind(VarName, B#tgd.b) of
	undefined ->
	    {tuple, Anno,[{atom, Anno, 'const'},{var,Anno,VarName}]};
	AtomName ->
	    {atom, Anno, AtomName}
    end;
tg({record_field,Anno,Object,RName,{atom,_Anno1,KeyName}},B) ->
    case get_record(RName) of
	FieldList when is_list(FieldList) ->
	    case lists:keysearch(KeyName,1, FieldList) of
		{value, {KeyName,Position,_}} ->
		    NewObject = tg(Object,B),
		    {tuple, Anno, [{atom, Anno, 'element'},
				   {integer, Anno, Position}, NewObject]};
		_ ->
		    throw({error,Anno,{?ERR_GENBADFIELD+B#tgd.eb, RName,
				       KeyName}})
	    end;
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record,Anno,RName,RFields},B) ->
    KeyList0 = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{Key,NV}|L];
				    ({record_field,_,{var,_,'_'},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{{default},NV}|L];
				    (_,_) ->
					 throw({error,Anno,
						{?ERR_GENBADREC+B#tgd.eb,
						 RName}})
				 end,
				 [],
				 RFields),
    DefValue = case lists:keysearch({default},1,KeyList0) of
		   {value,{{default},OverriddenDefValue}} ->
		       {true,OverriddenDefValue};
		   _ ->
		       false
	       end,
    KeyList = lists:keydelete({default},1,KeyList0),
    case lists:keysearch({default},1,KeyList) of
	{value,{{default},_}} ->
	    throw({error,Anno,{?ERR_GENMULTIFIELD+B#tgd.eb,RName,'_'}});
	_ ->
	    ok
    end,
    case get_record(RName) of
	FieldList0 when is_list(FieldList0) ->
	    FieldList1 = lists:foldl(
			   fun({FN,_,Def},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						case DefValue of 
						    {true,Overridden} ->
							Overridden;
						    false ->
							Def
						end
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_multi_field(RName,Anno,KeyList,
				 ?ERR_GENMULTIFIELD+B#tgd.eb),
	    check_undef_field(RName,Anno,KeyList,FieldList0,
			      ?ERR_GENBADFIELD+B#tgd.eb),
	    {tuple,Anno,[{tuple,Anno,[{atom,Anno,RName}|FieldList1]}]};
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record_index,Anno,RName,{atom,Anno2,KeyName}},B) ->
    case get_record(RName) of
	FieldList when is_list(FieldList) ->
	    case lists:keysearch(KeyName,1, FieldList) of
		{value, {KeyName,Position,_}} ->
		    {integer, Anno2, Position};
		_ ->
		    throw({error,Anno2,{?ERR_GENBADFIELD+B#tgd.eb, RName,
				       KeyName}})
	    end;
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record,Anno,{var,Anno2,_VName}=AVName, RName,RFields},B) ->
    MSVName = tg(AVName,B),
    KeyList = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{Key,NV}|L];
				    (_,_) ->
					 throw({error,Anno,?ERR_HEADBADREC})
				 end,
				 [],
				 RFields),
    case get_record(RName) of
	FieldList0 when is_list(FieldList0) ->
	    FieldList1 = lists:foldl(
			   fun({FN,Pos,_},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						{tuple, Anno2,
						 [{atom, Anno2, element},
						  {integer, Anno2, Pos},
						  MSVName]}
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_multi_field(RName,Anno,KeyList,
				 ?ERR_GENMULTIFIELD+B#tgd.eb),
	    check_undef_field(RName,Anno,KeyList,FieldList0,
			      ?ERR_GENBADFIELD+B#tgd.eb),
	    {tuple,Anno,[{tuple,Anno,[{atom,Anno,RName}|FieldList1]}]};
	not_found ->
	    throw({error,Anno,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({bin_element,_Anno0,{var, Anno, A},_,_} = Whole,B) ->
    case lkup_bind(A, B#tgd.b) of
	undefined ->
	    Whole; % exists in environment hopefully
	_AtomName ->
	    throw({error,Anno,{?ERR_GENBINCONSTRUCT+B#tgd.eb,A}})
    end;    
tg(default,_B) ->
    default;
tg({bin_element,Anno,X,Y,Z},B) ->
    {bin_element, Anno, tg(X,B), tg(Y,B), Z};

tg({bin,Anno,List},B) ->
    {bin,Anno,[tg(X,B) || X <- List]};

tg({map_field_assoc, Anno, Field, Value}, B) ->
    {map_field_assoc, Anno, tg(Field, B), tg(Value, B)};
tg({map, Anno, List}, B) ->
    {map, Anno, [tg(X, B) || X <- List]};

tg(T,B) when is_tuple(T), tuple_size(T) >= 2 ->
    Element = element(1,T),
    Anno = element(2,T),
    throw({error,Anno,{?ERR_GENELEMENT+B#tgd.eb,
		       translate_language_element(Element)}}); 
tg(Other,B) ->
    Element = io_lib:format("unknown element ~tw", [Other]),
    throw({error,erl_anno:new(0),{?ERR_GENELEMENT+B#tgd.eb,Element}}).

transform_head([V],OuterBound) ->
    Bind = cre_bind(),
    {NewV,NewBind} = toplevel_head_match(V,Bind,OuterBound),
    th(NewV,NewBind,OuterBound).


toplevel_head_match({match,_,{var,Anno,VName},Expr},B,OB) ->
    warn_var_clash(Anno,VName,OB),
    {Expr,new_bind({VName,'$_'},B)};
toplevel_head_match({match,_,Expr,{var,Anno,VName}},B,OB) ->
    warn_var_clash(Anno,VName,OB),
    {Expr,new_bind({VName,'$_'},B)};
toplevel_head_match(Other,B,_OB) ->
    {Other,B}.

th({record,Anno,RName,RFields},B,OB) ->
    % youch...
    {KeyList0,NewB} = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     {L,B0}) ->
					 {NV,B1} = th(Value,B0,OB),
					 {[{Key,NV}|L],B1};
				    ({record_field,_,{var,_,'_'},Value},
				     {L,B0}) ->
					 {NV,B1} = th(Value,B0,OB),
					 {[{{default},NV}|L],B1};
				    (_,_) ->
					 throw({error,Anno,{?ERR_HEADBADREC,
							    RName}})
				 end,
				 {[],B},
				 RFields),
    DefValue = case lists:keysearch({default},1,KeyList0) of
		   {value,{{default},OverriddenDefValue}} ->
		       OverriddenDefValue;
		   _ ->
		       {atom,Anno,'_'}
	       end,
    KeyList = lists:keydelete({default},1,KeyList0),
    case lists:keysearch({default},1,KeyList) of
	{value,{{default},_}} ->
	    throw({error,Anno,{?ERR_HEADMULTIFIELD,RName,'_'}});
	_ ->
	    ok
    end,
    case get_record(RName) of
	FieldList0 when is_list(FieldList0) ->
	    FieldList1 = lists:foldl(
			   fun({FN,_,_},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						DefValue
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_multi_field(RName,Anno,KeyList,
				 ?ERR_HEADMULTIFIELD),
	    check_undef_field(RName,Anno,KeyList,FieldList0,
			      ?ERR_HEADBADFIELD),
	    {{tuple,Anno,[{atom,Anno,RName}|FieldList1]},NewB};
	not_found ->
	    throw({error,Anno,{?ERR_HEADBADREC,RName}})
    end;
th({match,Anno,_,_},_,_) ->
    throw({error,Anno,?ERR_HEADMATCH});
th({atom,Anno,A},B,_OB) ->
    case atom_to_list(A) of
	[$$|NL] ->
	    case (catch list_to_integer(NL)) of
		N when is_integer(N) ->
		    throw({error,Anno,{?ERR_HEADDOLLARATOM,A}});
		_ ->
		    {{atom,Anno,A},B}
	    end;
	_ ->
	    {{atom,Anno,A},B}
    end;
th({bin_element,_Anno0,{var, Anno, A},_,_},_,_) ->
    throw({error,Anno,{?ERR_HEADBINMATCH,A}});

th({var,Anno,Name},B,OB) ->
    warn_var_clash(Anno,Name,OB),
    case lkup_bind(Name,B) of
	undefined ->
	    NewB = new_bind(Name,B),
	    {{atom,Anno,lkup_bind(Name,NewB)},NewB};
	Trans ->
	    {{atom,Anno,Trans},B}
    end;
th({map_field_exact,Anno,Field,Value},B,OB) ->
    {[NField, NValue], NB} = th([Field, Value], B, OB),
    {{map_field_assoc,Anno,NField,NValue}, NB};
th([H|T],B,OB) ->
    {NH,NB} = th(H,B,OB),
    {NT,NNB} = th(T,NB,OB),
    {[NH|NT],NNB};
th(T,B,OB) when is_tuple(T) ->
    {L,NB} = th(tuple_to_list(T),B,OB),
    {list_to_tuple(L),NB};
th(Nonstruct,B,_OB) ->
    {Nonstruct,B}.

warn_var_clash(Anno,Name,OuterBound) ->
    case gb_sets:is_member(Name,OuterBound) of
	true ->
            Location = erl_anno:location(Anno),
	    add_warning(Location,{?WARN_SHADOW_VAR,Name});
	_ ->
	    ok
    end.

%% Could be more efficient...
check_multi_field(_, _, [], _) ->
    ok;
check_multi_field(RName, Anno, [{Key,_}|T], ErrCode) ->
    case lists:keymember(Key,1,T) of
	true ->
	    throw({error,Anno,{ErrCode,RName,Key}});
	false ->
	    check_multi_field(RName, Anno, T, ErrCode)
    end.
check_undef_field(_, _, [], _, _) ->
    ok;
check_undef_field(RName, Anno, [{Key,_}|T], FieldList, ErrCode) ->
    case lists:keymember(Key, 1, FieldList) of
	true ->
	    check_undef_field(RName, Anno, T, FieldList, ErrCode);
	false ->
	    throw({error,Anno,{ErrCode,RName,Key}})
    end.

cre_bind() ->
    {1,[{'_','_'}]}.

lkup_bind(Name,{_,List}) ->
    case lists:keysearch(Name,1,List) of
	{value, {Name, Trans}} ->
	    Trans;
	_ ->
	    undefined
    end.

new_bind({Name,Trans},{Next,L}) ->
    {Next,[{Name,Trans}|L]};
new_bind(Name,{Next,L}) ->
    Trans = list_to_atom([$$|integer_to_list(Next)]),
    {Next+1,[{Name,Trans}|L]}.

translate_language_element(Atom) ->
    Transtab = [
		{lc,"list comprehension"},
		{bc,"binary comprehension"},
		{block, "begin/end block"},
		{'if', "if"},
		{'case', "case"},
		{'receive', "receive"},
		{'try', "try"},
		{'catch', "catch"},
		{'match', "match (=)"},
		{remote, "external function call"}
	       ],
    case lists:keysearch(Atom,1,Transtab) of
	{value,{Atom, String}} ->
	    String;
	_ ->
	    atom_to_list(Atom)
    end.

old_bool_test(atom,1) -> is_atom;
old_bool_test(float,1) -> is_float;
old_bool_test(integer,1) -> is_integer;
old_bool_test(list,1) -> is_list;
old_bool_test(number,1) -> is_number;
old_bool_test(pid,1) -> is_pid;
old_bool_test(port,1) -> is_port;
old_bool_test(reference,1) -> is_reference;
old_bool_test(tuple,1) -> is_tuple;
old_bool_test(binary,1) -> is_binary;
old_bool_test(function,1) -> is_function;
old_bool_test(record,2) -> is_record;
old_bool_test(_,_) -> undefined.

bool_test(is_atom,1) -> true;
bool_test(is_float,1) -> true;
bool_test(is_integer,1) -> true;
bool_test(is_list,1) -> true;
bool_test(is_number,1) -> true;
bool_test(is_pid,1) -> true;
bool_test(is_port,1) -> true;
bool_test(is_reference,1) -> true;
bool_test(is_tuple,1) -> true;
bool_test(is_map,1) -> true;
bool_test(is_map_key, 2) -> true;
bool_test(is_binary,1) -> true;
bool_test(is_function,1) -> true;
bool_test(is_record,2) -> true;
bool_test(is_seq_trace,0) -> true;
bool_test(_,_) -> false.

real_guard_function(abs,1) -> true;
real_guard_function(element,2) -> true;
real_guard_function(hd,1) -> true;
real_guard_function(length,1) -> true;
real_guard_function(max,2) -> true;
real_guard_function(min,2) -> true;
real_guard_function(node,0) -> true;
real_guard_function(node,1) -> true;
real_guard_function(round,1) -> true;
real_guard_function(size,1) -> true;
real_guard_function(bit_size,1) -> true;
real_guard_function(byte_size,1) -> true;
real_guard_function(map_size,1) -> true;
real_guard_function(map_get,2) -> true;
real_guard_function(binary_part,2) -> true;
real_guard_function(binary_part,3) -> true;
real_guard_function(tl,1) -> true;
real_guard_function(trunc,1) -> true;
real_guard_function(self,0) -> true;
real_guard_function(float,1) -> true;
real_guard_function(_,_) -> false.

pseudo_guard_function(get_tcw,0) -> true;
pseudo_guard_function(_,_) -> false.

guard_function(X,A) ->
    real_guard_function(X,A) or pseudo_guard_function(X,A).

action_function(set_seq_token,2) -> true;
action_function(get_seq_token,0) -> true;
action_function(message,1) -> true;
action_function(return_trace,0) -> true;
action_function(exception_trace,0) -> true;
action_function(process_dump,0) -> true;
action_function(enable_trace,1) -> true;
action_function(enable_trace,2) -> true;
action_function(disable_trace,1) -> true;
action_function(disable_trace,2) -> true;
action_function(display,1) -> true;
action_function(caller,0) -> true;
action_function(set_tcw,1) -> true;
action_function(silent,1) -> true;
action_function(trace,2) -> true;
action_function(trace,3) -> true;
action_function(caller_line,0) -> true;
action_function(current_stacktrace,0) -> true;
action_function(current_stacktrace,1) -> true;
action_function(_,_) -> false.

bool_operator('and',2) ->
    true;
bool_operator('or',2) ->
    true;
bool_operator('xor',2) ->
    true;
bool_operator('not',1) ->
    true;
bool_operator('andalso',2) ->
    true;
bool_operator('orelse',2) ->
    true;
bool_operator(_,_) ->
    false.

arith_operator('+',1) ->
    true;
arith_operator('+',2) ->
    true;
arith_operator('-',1) ->
    true;
arith_operator('-',2) ->
    true;
arith_operator('*',2) ->
    true;
arith_operator('/',2) ->
    true;
arith_operator('div',2) ->
    true;
arith_operator('rem',2) ->
    true;
arith_operator('band',2) ->
    true;
arith_operator('bor',2) ->
    true;
arith_operator('bxor',2) ->
    true;
arith_operator('bnot',1) ->
    true;
arith_operator('bsl',2) ->
    true;
arith_operator('bsr',2) ->
    true;
arith_operator(_,_) ->
    false.

cmp_operator('>',2) ->
    true;
cmp_operator('>=',2) ->
    true;
cmp_operator('<',2) ->
    true;
cmp_operator('=<',2) ->
    true;
cmp_operator('==',2) ->
    true;
cmp_operator('=:=',2) ->
    true;
cmp_operator('/=',2) -> 
    true;
cmp_operator('=/=',2) ->
    true;
cmp_operator(_,_) ->
    false.

is_operator(X,A,_) ->
    bool_operator(X,A) or arith_operator(X,A) or cmp_operator(X,A).

is_imported_from_erlang(X,A,_) ->
    real_guard_function(X,A) or bool_test(X,A) or bool_operator(X,A) or
    arith_operator(X,A) or cmp_operator(X,A).

is_ms_function(X,A,body) ->
    action_function(X,A) or guard_function(X,A) or bool_test(X,A);

is_ms_function(X,A,guard) ->
    guard_function(X,A) or bool_test(X,A).

fixup_environment(L,B) when is_list(L) ->    
    lists:map(fun(X) ->
		      fixup_environment(X,B) 
	      end,
	      L);
fixup_environment({var,Anno,Name},B) ->
    case lists:keysearch(Name,1,B) of
	{value,{Name,Value}} -> 
	    freeze(Anno,Value);
	_ ->
	    throw({error,Anno,{?ERR_UNBOUND_VARIABLE,atom_to_list(Name)}})
    end;
fixup_environment(T,B) when is_tuple(T) ->
    list_to_tuple(
      lists:map(fun(X) ->
			fixup_environment(X,B) 
		end,
		tuple_to_list(T)));
fixup_environment(Other,_B) ->
    Other.
    
freeze(Anno,Term) ->
    {frozen,Anno,Term}.

%% Most of this is bluntly stolen from erl_parse.

normalise({frozen,_,Term}) ->
    Term;
normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({op,_,'++',A,B}) ->
    normalise(A) ++ normalise(B);
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
normalise({map,_,Pairs}) ->
    maps:from_list(lists:map(fun
		%% only allow '=>'
		({map_field_assoc,_,K,V}) -> {normalise(K),normalise(V)}
	    end, Pairs));
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		% Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F.

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].
