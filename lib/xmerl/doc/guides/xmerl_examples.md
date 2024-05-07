# Customization functions

### 1 Description

The XML processor offers a number of hooks for customization. These hooks are
defined as function objects, and can be provided by the caller.

The following customization functions are available. If they also have access to
their own state variable, the access function for this state is identified
within parentheses:

-   event function ( `xmerl_scan:event_state/[1,2]` )
-   hook function ( `xmerl_scan:hook_state/[1,2]` )
-   fetch function ( `xmerl_scan:fetch_state/[1,2]` )
-   continuation function ( `xmerl_scan:cont_state/[1,2]` )
-   rules function ( `xmerl_scan:rules_state/[1,2]` )
-   accumulator function
-   close function

For all of the above state access functions, the function with one argument
(e.g. `event_state(GlobalState)`) will read the state variable, while the
function with two arguments (e.g.: `event_state(NewEventState, GlobalState)`)
will modify it.

For each function, the description starts with the syntax for specifying the
function in the [`Option_list`](`t:xmerl_scan:option_list/0`). The general
forms are `{Tag, Fun}`, or `{Tag, Fun, LocalState}`. The second form can be used
to initialize the state variable in question.

#### 1.1 User State

All customization functions are free to access a "User state" variable. Care
must of course be taken to coordinate the use of this state. It is recommended
that functions, which do not really have anything to contribute to the "global"
user state, use their own state variable instead. Another option (used in e.g.
`xmerl_eventp.erl`) is for customization functions to share one of the local
states (in `xmerl_eventp.erl`, the continuation function and the fetch function
both access the cont_state.)

Functions to access user state:

-   `xmerl_scan:user_state(GlobalState)`
-   `xmerl_scan:user_state(UserState, GlobalState)`

#### 1.2 Event Function

``` erlang
{event_fun, fun()} | {event_fun, fun(), EventState}
```

The event function is called at the beginning and at the end of a parsed entity.
It has the following format and semantics:

``` erlang
fun(Event, GlobalState) ->
   EventState = xmerl_scan:event_state(GlobalState),
   EventState2 = foo(Event, EventState),
   GlobalState2 = xmerl_scan:event_state(EventState2, GlobalState)
end.
```

#### 1.3 Hook Function

``` erlang
{hook_fun, fun()} | {hook_fun, fun(), HookState}
```

The hook function is called when the processor has parsed a complete entity.
Format and semantics:

``` erlang
fun(Entity, GlobalState) ->
   HookState = xmerl_scan:hook_state(GlobalState),
   {TransformedEntity, HookState2} = foo(Entity, HookState),
   GlobalState2 = xmerl_scan:hook_state(HookState2, GlobalState),
   {TransformedEntity, GlobalState2}
end.
```

The relationship between the event function, the hook function and the
accumulator function is as follows:

1.  The event function is first called with an 'ended' event for the parsed
    entity.
2.  The hook function is called, possibly re-formatting the entity.
3.  The acc function is called in order to (optionally) add the re-formatted
    entity to the contents of its parent element.

#### 1.4 Fetch Function

``` erlang
{fetch_fun, fun()} | {fetch_fun, fun(), FetchState}
```

The fetch function is called in order to fetch an external resource (e.g. a
DTD).

The fetch function can respond with three different return values:

``` erlang
Result ::=
   {ok, {file, Filename}, NewGlobalState} |
   {ok, {string, String}, NewGlobalState} |
   {ok, not_fetched, NewGlobalState}
```

Format and semantics:

``` erlang
fun(URI, GlobalState) ->
   FetchState = xmerl_scan:fetch_state(GlobalState),
   Result = foo(URI, FetchState).  % Result being one of the above
end.
```

#### 1.5 Continuation Function

``` erlang
{continuation_fun, fun()} | {continuation_fun, fun(), ContinuationState}
```

The continuation function is called when the parser encounters the end of the
byte stream. Format and semantics:

``` erlang
fun(Continue, Exception, GlobalState) ->
   ContState = xmerl_scan:cont_state(GlobalState),
   {Result, ContState2} = get_more_bytes(ContState),
   case Result of
      [] ->
         GlobalState2 = xmerl_scan:cont_state(ContState2, GlobalState),
         Exception(GlobalState2);
      MoreBytes ->
         {MoreBytes2, Rest} = end_on_whitespace_char(MoreBytes),
         ContState3 = update_cont_state(Rest, ContState2),
         GlobalState3 = xmerl_scan:cont_state(ContState3, GlobalState),
         Continue(MoreBytes2, GlobalState3)
   end
end.
```

#### 1.6 Rules Functions

``` erlang
{rules, ReadFun : fun(), WriteFun : fun(), RulesState} |
{rules, Table : ets()}
```

The rules functions take care of storing scanner information in a rules
database. User-provided rules functions may opt to store the information in
mnesia, or perhaps in the user_state(RulesState).

The following modes exist:

-   If the user doesn't specify an option, the scanner creates an ets table, and
    uses built-in functions to read and write data to it. When the scanner is
    done, the ets table is deleted.
-   If the user specifies an ets table via the `{rules, Table}` option, the
    scanner uses this table. When the scanner is done, it does _not_ delete the
    table.
-   If the user specifies read and write functions, the scanner will use them
    instead.

The format for the read and write functions are as follows:

     WriteFun(Context, Name, Definition, ScannerState) -> NewScannerState.
     ReadFun(Context, Name, ScannerState) -> Definition | undefined.

Here is a summary of the data objects currently being written by the scanner:

| Context          | Key Value    | Definition                           |
| ---------------- | ------------ | ------------------------------------ |
| notation         | NotationName | `{system, SL} \| {public, PIDL, SL}` |
| elem_def         | ElementName  | `#xmlElement{content = ContentSpec}` |
| parameter_entity | PEName       | `PEDef`                              |
| entity           | EntityName   | `EntityDef`                          |
** Table 1:** Scanner data objects

where

``` erlang
ContentSpec ::= empty | any | ElemContent
ElemContent ::= {Mode, Elems}
Mode        ::= seq | choice
Elems       ::= [Elem]
Elem        ::= '#PCDATA' | Name | ElemContent | {Occurrence, Elems}
Occurrence  ::= '*' | '?' | '+'
```

NOTE: _When <Elem> is not wrapped with <Occurrence>, (Occurrence = once) is
implied._

#### 1.7 Accumulator Function

``` erlang
{acc_fun, fun()}
```

The accumulator function is called to accumulate the contents of an entity.When
parsing very large files, it may not be desirable to do so.In this case, an acc
function can be provided that simply doesn't accumulate.

Note that it is possible to even modify the parsed entity before accumulating
it, but this must be done with care. xmerl_scan performs post-processing of the
element for namespace management. Thus, the element must keep its original
structure for this to work.

The acc function has the following format and semantics:

``` erlang
%% default accumulating acc fun
fun(ParsedEntity, Acc, GlobalState) ->
   {[ParsedEntity|Acc], GlobalState}.

%% non-accumulating acc fun
fun(ParsedEntity, Acc, GlobalState) ->
   {Acc, GlobalState}.
```

#### 1.8 Close Function

The close function is called when a document (either the main document or an
external DTD) has been completely parsed. When xmerl_scan was started using
`xmerl_scan:file/[1,2]`, the file will be read in full, and closed immediately,
before the parsing starts, so when the close function is called, it will not
need to actually close the file. In this case, the close function will be a good
place to modify the state variables.

Format and semantics:

``` erlang
fun(GlobalState) ->
   GlobalState1 = ....  % state variables may be altered
```

### 2 Examples

See `xmerl_test.erl` for more examples.

#### 2.1 Handling spaces

The following sample program illustrates three ways of scanning a document:

1.  the default scan, which leaves whitespace untouched
2.  normalizing spaces
3.  normalizing spaces, then removing text elements that only contain one space.

``` erlang
-module(tmp).

-include("xmerl.hrl").

-export([file1/1, file2/1, file3/1]).

file1(F) -> xmerl_scan:file(F).

file2(F) -> xmerl_scan:file(F, [{space,normalize}]).

file3(F) -> Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) -> {Acc, P,
S}; % new return format (X, Acc, S) -> {[X|Acc], S} end, xmerl_scan:file(F,
[{space,normalize}, {acc_fun, Acc}]).
```
