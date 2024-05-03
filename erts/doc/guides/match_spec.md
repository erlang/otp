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
# Match Specifications in Erlang

A "match specification" (`match_spec`) is an Erlang term describing a small
"program" that tries to match something. It can be used to either control
tracing with `erlang:trace_pattern/3` or to search for objects in an ETS table
with for example `ets:select/2`. The match specification in many ways works like
a small function in Erlang, but is interpreted/compiled by the Erlang runtime
system to something much more efficient than calling an Erlang function. The
match specification is also very limited compared to the expressiveness of real
Erlang functions.

The most notable difference between a match specification and an Erlang fun is
the syntax. Match specifications are Erlang terms, not Erlang code. Also, a
match specification has a strange concept of exceptions:

- An exception (such as `badarg`) in the `MatchCondition` part, which resembles
  an Erlang guard, generates immediate failure.
- An exception in the `MatchBody` part, which resembles the body of an Erlang
  function, is implicitly caught and results in the single atom `'EXIT'`.

## Grammar

A match specification used in tracing can be described in the following
_informal_ grammar:

- MatchExpression ::= [ MatchFunction, ... ]
- MatchFunction ::= { MatchHead, MatchConditions, MatchBody }
- MatchHead ::= MatchVariable | `'_'` | [ MatchHeadPart, ... ]
- MatchHeadPart ::= [term()](`t:term/0`) | MatchVariable | `'_'`
- MatchVariable ::= '$<number>'
- MatchConditions ::= [ MatchCondition, ...] | `[]`
- MatchCondition ::= { GuardFunction } | { GuardFunction,
  ConditionExpression, ... }
- BoolFunction ::= `is_atom` | `is_float` | `is_integer` | `is_list` |
  `is_number` | `is_pid` | `is_port` | `is_reference` | `is_tuple` | `is_map` |
  `is_map_key` | `is_binary` | `is_bitstring` | `is_boolean` | `is_function` |
  `is_record` | `is_seq_trace` | `'and'` | `'or'` | `'not'` | `'xor'` |
  `'andalso'` | `'orelse'`
- ConditionExpression ::= ExprMatchVariable | { GuardFunction } | {
  GuardFunction, ConditionExpression, ... } | TermConstruct
- ExprMatchVariable ::= MatchVariable (bound in the MatchHead) | `'$_'` | `'$$'`
- TermConstruct = {{}} | {{ ConditionExpression, ... }} | `[]` |
  [ConditionExpression, ...] | `#{}` | #{[term()](`t:term/0`) => ConditionExpression, ...}
  | NonCompositeTerm | Constant
- NonCompositeTerm ::= [term()](`t:term/0`) (not list or tuple or map)
- Constant ::= {`const`, [term()](`t:term/0`)}
- GuardFunction ::= BoolFunction | `abs` | `element` | `hd` | `length` |
  `map_get` | `map_size` | `max` | `min` | `node` | `float` | `round` | `floor`
  | `ceil` | `size` | `bit_size` | `byte_size` | `tuple_size` | `tl` | `trunc` |
  `binary_part` | `'+'` | `'-'` | `'*'` | `'div'` | `'rem'` | `'band'` | `'bor'`
  | `'bxor'` | `'bnot'` | `'bsl'` | `'bsr'` | `'>'` | `'>='` | `'<'` | `'=<'` |
  `'=:='` | `'=='` | `'=/='` | `'/='` | `self` | `get_tcw`
- MatchBody ::= [ ActionTerm ]
- ActionTerm ::= ConditionExpression | ActionCall
- ActionCall ::= {ActionFunction} | {ActionFunction, ActionTerm, ...}
- ActionFunction ::= `set_seq_token` | `get_seq_token` | `message` |
  `return_trace` | `exception_trace` | `process_dump` | `enable_trace` |
  `disable_trace` | `trace` | `display` | `caller` | `caller_line` |
  `current_stacktrace` | `set_tcw` | `silent`

A match specification used in `m:ets` can be described in the following
_informal_ grammar:

- MatchExpression ::= [ MatchFunction, ... ]
- MatchFunction ::= { MatchHead, MatchConditions, MatchBody }
- MatchHead ::= MatchVariable | `'_'` | { MatchHeadPart, ... }
- MatchHeadPart ::= [term()](`t:term/0`) | MatchVariable | `'_'`
- MatchVariable ::= '$<number>'
- MatchConditions ::= [ MatchCondition, ...] | `[]`
- MatchCondition ::= { GuardFunction } | { GuardFunction,
  ConditionExpression, ... }
- BoolFunction ::= `is_atom` | `is_float` | `is_integer` | `is_list` |
  `is_number` | `is_pid` | `is_port` | `is_reference` | `is_tuple` | `is_map` |
  `is_map_key` | `is_binary` | `is_bitstring` | `is_boolean` | `is_function` |
  `is_record` | `'and'` | `'or'` | `'not'` | `'xor'` | `'andalso'` | `'orelse'`
- ConditionExpression ::= ExprMatchVariable | { GuardFunction } | {
  GuardFunction, ConditionExpression, ... } | TermConstruct
- ExprMatchVariable ::= MatchVariable (bound in the MatchHead) | `'$_'` | `'$$'`
- TermConstruct = {{}} | {{ ConditionExpression, ... }} | `[]` |
  [ConditionExpression, ...] | #{} | #{[term()](`t:term/0`) => ConditionExpression, ...}
  | NonCompositeTerm | Constant
- NonCompositeTerm ::= [term()](`t:term/0`) (not list or tuple or map)
- Constant ::= {`const`, [term()](`t:term/0`)}
- GuardFunction ::= BoolFunction | `abs` | `element` | `hd` | `length` |
  `map_get` | `map_size` | `max` | `min` | `node` | `float` | `round` | `floor`
  | `ceil` | `size` | `bit_size` | `byte_size` | `tuple_size` | `tl` | `trunc` |
  `binary_part` | `'+'` | `'-'` | `'*'` | `'div'` | `'rem'` | `'band'` | `'bor'`
  | `'bxor'` | `'bnot'` | `'bsl'` | `'bsr'` | `'>'` | `'>='` | `'<'` | `'=<'` |
  `'=:='` | `'=='` | `'=/='` | `'/='` | `self`
- MatchBody ::= [ ConditionExpression, ... ]

## Function Descriptions

### Functions Allowed in All Types of Match Specifications

The functions allowed in `match_spec` work as follows:

- **`is_atom`, `is_boolean`, `is_float`, `is_integer`, `is_list`, `is_number`,
  `is_pid`, `is_port`, `is_reference`, `is_tuple`, `is_map`, `is_binary`,
  `is_bitstring`, `is_function`** - Same as the corresponding guard tests in
  Erlang, return `true` or `false`.

- **`is_record`** - Takes an additional parameter, which _must_ be the result of
  `record_info(size, <record_type>)`, like in
  `{is_record, '$1', rectype, record_info(size, rectype)}`.

- **`'not'`** - Negates its single argument (anything other than `false` gives
  `false`).

- **`'and'`** - Returns `true` if all its arguments (variable length argument
  list) evaluate to `true`, otherwise `false`. Evaluation order is undefined.

- **`'or'`** - Returns `true` if any of its arguments evaluates to `true`.
  Variable length argument list. Evaluation order is undefined.

- **`'andalso'`** - Works as `'and'`, but quits evaluating its arguments when
  one argument evaluates to something else than `true`. Arguments are evaluated
  left to right.

- **`'orelse'`** - Works as `'or'`, but quits evaluating as soon as one of its
  arguments evaluates to `true`. Arguments are evaluated left to right.

- **`'xor'`** - Only two arguments, of which one must be `true` and the other
  `false` to return `true`; otherwise `'xor'` returns false.

- **`abs`, `element`, `hd`, `length`, `map_get`, `map_size`, `max`, `min`,
  `node`, `round`, `ceil`, `floor`, `float`, `size`, `bit_size`, `byte_size`,
  `tuple_size`, `tl`, `trunc`, `binary_part`, `'+'`, `'-'`, `'*'`, `'div'`,
  `'rem'`, `'band'`, `'bor'`, `'bxor'`, `'bnot'`, `'bsl'`, `'bsr'`, `'>'`,
  `'>='`, `'<'`, `'=<'`, `'=:='`, `'=='`, `'=/='`, `'/='`, `self`** - Same as
  the corresponding Erlang BIFs (or operators). In case of bad arguments, the
  result depends on the context. In the `MatchConditions` part of the
  expression, the test fails immediately (like in an Erlang guard). In the
  `MatchBody` part, exceptions are implicitly caught and the call results in the
  atom `'EXIT'`.

### Functions Allowed Only for Tracing

The functions allowed only for tracing work as follows:

- **`is_seq_trace`** - Returns `true` if a sequential trace token is set for the
  current process, otherwise `false`.

- **`set_seq_token`** - Works as `seq_trace:set_token/2`, but returns `true` on
  success, and `'EXIT'` on error or bad argument. Only allowed in the
  `MatchBody` part and only allowed when tracing.

- **`get_seq_token`** - Same as `seq_trace:get_token/0` and only allowed in the
  `MatchBody` part when tracing.

- **`message`** - Sets an additional message appended to the trace message sent.
  One can only set one additional message in the body. Later calls replace the
  appended message.

  As a special case, `{message, false}` disables sending of trace messages
  ('call' and 'return_to') for this function call, just like if the match
  specification had not matched. This can be useful if only the side effects of
  the `MatchBody` part are desired.

  Another special case is `{message, true}`, which sets the default behavior, as
  if the function had no match specification; trace message is sent with no
  extra information (if no other calls to `message` are placed before
  `{message, true}`, it is in fact a "noop").

  Takes one argument: the message. Returns `true` and can only be used in the
  `MatchBody` part and when tracing.

- **`return_trace`** - Causes a `return_from` trace message to be sent upon
  return from the current function. Takes no arguments, returns `true` and can
  only be used in the `MatchBody` part when tracing. If the process trace flag
  `silent` is active, the `return_from` trace message is inhibited.

  _Warning:_ If the traced function is tail-recursive, this match specification
  function destroys that property. Hence, if a match specification executing
  this function is used on a perpetual server process, it can only be active for
  a limited period of time, or the emulator will eventually use all memory in
  the host machine and crash. If this match specification function is inhibited
  using process trace flag `silent`, tail-recursiveness still remains.

- **`exception_trace`** - Works as `return_trace` plus; if the traced function
  exits because of an exception, an `exception_from` trace message is generated,
  regardless of the exception is caught or not.

- **`process_dump`** - Returns some textual information about the current
  process as a binary. Takes no arguments and is only allowed in the `MatchBody`
  part when tracing.

- **`enable_trace`** - With one parameter this function turns on tracing like
  the Erlang call `erlang:trace(self(), true, [P2])`, where `P2` is the
  parameter to `enable_trace`.

  With two parameters, the first parameter is to be either a process identifier
  or the registered name of a process. In this case tracing is turned on for the
  designated process in the same way as in the Erlang call
  `erlang:trace(P1, true, [P2])`, where `P1` is the first and `P2` is the second
  argument. The process `P1` gets its trace messages sent to the same tracer as
  the process executing the statement uses. `P1` _cannot_ be one of the atoms
  `all`, `new` or `existing` (unless they are registered names). `P2` _cannot_
  be `cpu_timestamp` or `tracer`.

  Returns `true` and can only be used in the `MatchBody` part when tracing.

- **`disable_trace`** - With one parameter this function disables tracing like
  the Erlang call `erlang:trace(self(), false, [P2])`, where `P2` is the
  parameter to `disable_trace`.

  With two parameters this function works as the Erlang call
  `erlang:trace(P1, false, [P2])`, where `P1` can be either a process identifier
  or a registered name and is specified as the first argument to the match
  specification function. `P2` _cannot_ be `cpu_timestamp` or `tracer`.

  Returns `true` and can only be used in the `MatchBody` part when tracing.

- **`trace`** - With two parameters this function takes a list of trace flags to
  disable as first parameter and a list of trace flags to enable as second
  parameter. Logically, the disable list is applied first, but effectively all
  changes are applied atomically. The trace flags are the same as for
  `erlang:trace/3`, not including `cpu_timestamp`, but including `tracer`.

  If a tracer is specified in both lists, the tracer in the enable list takes
  precedence. If no tracer is specified, the same tracer as the process
  executing the match specification is used (not the meta tracer). If that
  process doesn't have tracer either, then trace flags are ignored.

  When using a [tracer module](`m:erl_tracer`), the module must be loaded before
  the match specification is executed. If it is not loaded, the match fails.

  With three parameters to this function, the first is either a process
  identifier or the registered name of a process to set trace flags on, the
  second is the disable list, and the third is the enable list.

  Returns `true` if any trace property was changed for the trace target process,
  otherwise `false`. Can only be used in the `MatchBody` part when tracing.

- **`caller`** - Returns the calling function as a tuple
  `{Module, Function, Arity}` or the atom `undefined` if the calling function
  cannot be determined. Can only be used in the `MatchBody` part when tracing.

  Notice that if a "technically built in function" (that is, a function not
  written in Erlang) is traced, the `caller` function sometimes returns the atom
  `undefined`. The calling Erlang function is not available during such calls.

- **`caller_line`** - Similar to `caller` but returns additional information
  about the source code location of the function call-site within the caller
  function. Returns the calling function as a tuple
  `{Module, Function, Arity, {File, Line}}`. `File` is the
  [string](`e:system:data_types.md#string`) file name while `Line` is source
  line number. If the `File` and `Line` cannot be determined,
  `{Module, Function, Arity, undefined}` is returned. If the calling function
  cannot be determined, the atom `undefined` is returned. Can only be used in
  the `MatchBody` part when tracing.

  Notice that if a "technically built in function" (that is, a function not
  written in Erlang) is traced, the `caller_line` function sometimes returns the
  atom `undefined`. The calling Erlang function is not available during such
  calls.

- **`current_stacktrace`** - Returns the current call stack back-trace
  ([stacktrace](`t:erlang:stacktrace/0`)) of the calling function. The stack has
  the same format as in the `catch` part of a `try`. See
  [The call-stack back trace (stacktrace)](`e:system:errors.md#stacktrace`). The
  depth of the stacktrace is truncated according to the `backtrace_depth` system
  flag setting.

  Accepts a depth parameter. The depth value will be `backtrace_depth` if the
  argument is greater.

- **`display`** - For debugging purposes only. Displays the single argument as
  an Erlang term on `stdout`, which is seldom what is wanted. Returns `true` and
  can only be used in the `MatchBody` part when tracing.

- **`get_tcw`{: #get_tcw }** - Takes no argument and returns the value of the
  node's trace control word. The same is done by
  `erlang:system_info(trace_control_word)`.

  The trace control word is a 32-bit unsigned integer intended for generic trace
  control. The trace control word can be tested and set both from within trace
  match specifications and with BIFs. This call is only allowed when tracing.

- **`set_tcw`{: #set_tcw }** - Takes one unsigned integer argument, sets the
  value of the node's trace control word to the value of the argument, and
  returns the previous value. The same is done by
  `erlang:system_flag(trace_control_word, Value)`. It is only allowed to use
  `set_tcw` in the `MatchBody` part when tracing.

- **`silent`** - Takes one argument. If the argument is `true`, the call trace
  message mode for the current process is set to silent for this call and all
  later calls, that is, call trace messages are inhibited even if
  `{message, true}` is called in the `MatchBody` part for a traced function.

  This mode can also be activated with flag `silent` to `erlang:trace/3`.

  If the argument is `false`, the call trace message mode for the current
  process is set to normal (non-silent) for this call and all later calls.

  If the argument is not `true` or `false`, the call trace message mode is
  unaffected.

> #### Note {: .info }
>
> All "function calls" must be tuples, even if they take no arguments. The value
> of `self` is the atom() `self`, but the value of `{self}` is the pid() of the
> current process.

[](){: #match_target }

## Match target

Each execution of a match specification is done against a match target term. The
format and content of the target term depends on the context in which the match
is done. The match target for ETS is always a full table tuple. The match target
for call trace is always a list of all function arguments. The match target for
event trace depends on the event type, see table below.

| Context | Type      | Match target                 | Description                                 |
| ------- | --------- | ---------------------------- | ------------------------------------------- |
| ETS     |           | {Key, Value1, Value2, ...}   | A table object                              |
| Trace   | call      | [Arg1, Arg2, ...]            | Function arguments                          |
| Trace   | send      | [Receiver, Message]          | Receiving process/port and message term     |
| Trace   | 'receive' | [Node, Sender, Message]      | Sending node, process/port and message term |

_Table: Match target depending on context_

## Variables and Literals

Variables take the form `'$<number>'`, where `<number>` is an integer between 0
and 100,000,000 (1e+8). The behavior if the number is outside these limits is
_undefined_. In the `MatchHead` part, the special variable `'_'` matches
anything, and never gets bound (like `_` in Erlang).

- In the `MatchCondition/MatchBody` parts, no unbound variables are allowed, so
  `'_'` is interpreted as itself (an atom). Variables can only be bound in the
  `MatchHead` part.
- In the `MatchBody` and `MatchCondition` parts, only variables bound previously
  can be used.
- As a special case, the following apply in the `MatchCondition/MatchBody`
  parts:

  - The variable `'$_'` expands to the whole
    [match target](match_spec.md#match_target) term.
  - The variable `'$$'` expands to a list of the values of all bound variables
    in order (that is, `['$1','$2', ...]`).

In the `MatchHead` part, all literals (except the variables above) are
interpreted "as is".

In the `MatchCondition/MatchBody` parts, the interpretation is in some ways
different. Literals in these parts can either be written "as is", which works
for all literals except tuples, or by using the special form `{const, T}`, where
`T` is any Erlang term.

For tuple literals in the match specification, double tuple parentheses can also
be used, that is, construct them as a tuple of arity one containing a single
tuple, which is the one to be constructed. The "double tuple parenthesis" syntax
is useful to construct tuples from already bound variables, like in
`{{'$1', [a,b,'$2']}}`. Examples:

| Expression                | Variable Bindings   | Result                                   |
| ------------------------- | ------------------- | ---------------------------------------- |
| `{{'$1','$2'}}`           | '$1' = a, '$2' = b  | `{a,b}`                                  |
| `{const, {'$1', '$2'}}`   | Irrelevant          | `{'$1', '$2'}`                           |
| `a`                       | Irrelevant          | `a`                                      |
| `'$1'`                    | '$1' = []           | `[]`                                     |
| `[{{a}}]`                 | Irrelevant          | `[{a}]`                                  |
| `['$1']`                  | '$1' = []           | `[[]]`                                   |
| `42`                      | Irrelevant          | `42`                                     |
| `"hello"`                 | Irrelevant          | `"hello"`                                |
| `$1`                      | Irrelevant          | `49` (the ASCII value for character '1') |

_Table: Literals in MatchCondition/MatchBody Parts of a Match Specification_

## Execution of the Match

The execution of the match expression, when the runtime system decides whether a
trace message is to be sent, is as follows:

For each tuple in the `MatchExpression` list and while no match has succeeded:

1. Match the `MatchHead` part against the match target term, binding the
   `'$<number>'` variables (much like in `ets:match/2`). If the `MatchHead` part
   cannot match the arguments, the match fails.
1. Evaluate each `MatchCondition` (where only `'$<number>'` variables previously
   bound in the `MatchHead` part can occur) and expect it to return the atom
   `true`. When a condition does not evaluate to `true`, the match fails. If any
   BIF call generates an exception, the match also fails.
1. Two cases can occur:

- If the match specification is executing when tracing:

  Evaluate each `ActionTerm` in the same way as the `MatchConditions`, but
  ignore the return values. Regardless of what happens in this part, the match
  has succeeded.

- If the match specification is executed when selecting objects from an ETS
  table:

  Evaluate the expressions in order and return the value of the last expression
  (typically there is only one expression in this context).

[](){: #differences_ets_tracing }

## Differences between Match Specifications in ETS and Tracing

ETS match specifications produce a return value. Usually the `MatchBody`
contains one single `ConditionExpression` that defines the return value without
any side effects. Calls with side effects are not allowed in the ETS context.

When tracing there is no return value to produce, the match specification either
matches or does not. The effect when the expression matches is a trace message
rather than a returned term. The `ActionTerm`s are executed as in an imperative
language, that is, for their side effects. Functions with side effects are also
allowed when tracing.

## Tracing Examples

Match an argument list of three, where the first and third arguments are equal:

```erlang
[{['$1', '_', '$1'],
  [],
  []}]
```

Match an argument list of three, where the second argument is a number > 3:

```erlang
[{['_', '$1', '_'],
  [{ '>', '$1', 3}],
  []}]
```

Match an argument list of three, where the third argument is either a tuple
containing argument one and two, _or_ a list beginning with argument one and two
(that is, `[a,b,[a,b,c]]` or `[a,b,{a,b}]`):

```erlang
[{['$1', '$2', '$3'],
  [{'orelse',
      {'=:=', '$3', {{'$1','$2'}}},
      {'and',
        {'=:=', '$1', {hd, '$3'}},
        {'=:=', '$2', {hd, {tl, '$3'}}}}}],
  []}]
```

The above problem can also be solved as follows:

```erlang
[{['$1', '$2', {'$1', '$2}], [], []},
 {['$1', '$2', ['$1', '$2' | '_']], [], []}]
```

Match two arguments, where the first is a tuple beginning with a list that in
turn begins with the second argument times two (that is, `[{[4,x],y},2]` or
`[{[8], y, z},4])`:

```erlang
[{['$1', '$2'],[{'=:=', {'*', 2, '$2'}, {hd, {element, 1, '$1'}}}],
  []}]
```

Match three arguments. When all three are equal and are numbers, append the
process dump to the trace message, otherwise let the trace message be "as is",
but set the sequential trace token label to 4711:

```erlang
[{['$1', '$1', '$1'],
  [{is_number, '$1'}],
  [{message, {process_dump}}]},
 {'_', [], [{set_seq_token, label, 4711}]}]
```

As can be noted above, the parameter list can be matched against a single
`MatchVariable` or an `'_'`. To replace the whole parameter list with a single
variable is a special case. In all other cases the `MatchHead` must be a
_proper_ list.

Generate a trace message only if the trace control word is set to 1:

```erlang
[{'_',
  [{'==',{get_tcw},{const, 1}}],
  []}]
```

Generate a trace message only if there is a `seq_trace` token:

```erlang
[{'_',
  [{'==',{is_seq_trace},{const, 1}}],
  []}]
```

Remove the `'silent'` trace flag when the first argument is `'verbose'`, and add
it when it is `'silent':`

```erlang
[{'$1',
  [{'==',{hd, '$1'},verbose}],
  [{trace, [silent],[]}]},
 {'$1',
  [{'==',{hd, '$1'},silent}],
  [{trace, [],[silent]}]}]
```

Add a `return_trace` message if the function is of arity 3:

```erlang
[{'$1',
  [{'==',{length, '$1'},3}],
  [{return_trace}]},
 {'_',[],[]}]
```

Generate a trace message only if the function is of arity 3 and the first
argument is `'trace'`:

```erlang
[{['trace','$2','$3'],
  [],
  []},
 {'_',[],[]}]
```

## ETS Examples

Match all objects in an ETS table, where the first element is the atom
`'strider'` and the tuple arity is 3, and return the whole object:

```erlang
[{{strider,'_','_'},
  [],
  ['$_']}]
```

Match all objects in an ETS table with arity > 1 and the first element is
'gandalf', and return element 2:

```erlang
[{'$1',
  [{'==', gandalf, {element, 1, '$1'}},{'>=',{size, '$1'},2}],
  [{element,2,'$1'}]}]
```

In this example, if the first element had been the key, it is much more
efficient to match that key in the `MatchHead` part than in the
`MatchConditions` part. The search space of the tables is restricted with
regards to the `MatchHead` so that only objects with the matching key are
searched.

Match tuples of three elements, where the second element is either `'merry'` or
`'pippin'`, and return the whole objects:

```erlang
[{{'_',merry,'_'},
  [],
  ['$_']},
 {{'_',pippin,'_'},
  [],
  ['$_']}]
```

Function `ets:test_ms/2` can be useful for testing complicated ETS matches.
