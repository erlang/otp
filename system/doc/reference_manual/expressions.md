<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# Expressions

In this section, all valid Erlang expressions are listed. When writing Erlang
programs, it is also allowed to use macro and record expressions. However,
these expressions are expanded during compilation and are in that sense not true
Erlang expressions. Macro and record expressions are covered in separate
sections:

- [Preprocessor](macros.md)
- [Records](ref_man_records.md)

## Expression Evaluation

All subexpressions are evaluated before an expression itself is evaluated,
unless explicitly stated otherwise. For example, consider the expression:

```
Expr1 + Expr2
```

`Expr1` and `Expr2`, which are also expressions, are evaluated first — in any
order — before the addition is performed.

Many of the operators can only be applied to arguments of a certain type. For
example, arithmetic operators can only be applied to numbers. An argument of the
wrong type causes a `badarg` runtime error.

## Terms

The simplest form of expression is a term, that is one of
`t:integer/0`, `t:float/0`, `t:atom/0`, `t:string/0`, `t:list/0`,
`t:map/0`, or `t:tuple/0`. The return value is the term itself.

## Variables

A variable is an expression. If a variable is bound to a value, the return value
is this value. Unbound variables are only allowed in patterns.

Variables start with an uppercase letter or underscore (`_`). Variables can
contain alphanumeric characters, underscore, and `@`.

_Examples:_

```
X
Name1
PhoneNumber
Phone_number
_
_Height
name@node
```

Variables are bound to values using [pattern matching](patterns.md). Erlang uses
_single assignment_, that is, a variable can only be bound once.

The _anonymous variable_ is denoted by underscore (\_) and can be used when a
variable is required but its value can be ignored.

_Example:_

```text
[H|_] = [1,2,3]
```

Variables starting with underscore (`_`), for example, `_Height`, are normal
variables, not anonymous. However, they are ignored by the compiler in the sense
that they do not generate warnings.

_Example:_

The following code:

```
member(_, []) ->
    [].
```

can be rewritten to be more readable:

```
member(Elem, []) ->
    [].
```

This causes a warning for an unused variable, `Elem`. To avoid the warning,
the code can be rewritten to:

```
member(_Elem, []) ->
    [].
```

Notice that since variables starting with an underscore are not anonymous, the
following example matches:

```
{_,_} = {1,2}
```

But this example fails:

```
{_N,_N} = {1,2}
```

The scope for a variable is its function clause. Variables bound in a branch of
an `if`, `case`, or `receive` expression must be bound in all branches to have a
value outside the expression. Otherwise they are regarded as unsafe outside
the expression.

For the `try` expression variable scoping is limited so that variables bound in
the expression are always unsafe outside the expression.

## Patterns

A pattern has the same structure as a term but can contain unbound variables.

_Example:_

```
Name1
[H|T]
{error,Reason}
```

Patterns are allowed in clause heads, [case expressions](expressions.md#case),
[receive expressions](expressions.md#receive), and
[match expressions](expressions.md#the-match-operator).

### The Compound Pattern Operator

If `Pattern1` and `Pattern2` are valid patterns, the following is also a valid
pattern:

```
Pattern1 = Pattern2
```

When matched against a term, both `Pattern1` and `Pattern2` are matched against
the term. The idea behind this feature is to avoid reconstruction of terms.

_Example:_

```
f({connect,From,To,Number,Options}, To) ->
    Signal = {connect,From,To,Number,Options},
    ...;
f(Signal, To) ->
    ignore.
```

can instead be written as

```
f({connect,_,To,_,_} = Signal, To) ->
    ...;
f(Signal, To) ->
    ignore.
```

The compound pattern operator does not imply that its operands are matched in
any particular order. That means that it is not legal to bind a variable in
`Pattern1` and use it in `Pattern2`, or vice versa.

### String Prefix in Patterns

When matching strings, the following is a valid pattern:

```
f("prefix" ++ Str) -> ...
```

This is syntactic sugar for the equivalent, but harder to read:

```
f([$p,$r,$e,$f,$i,$x | Str]) -> ...
```

### Expressions in Patterns

An arithmetic expression can be used within a pattern if it meets both of the
following two conditions:

- It uses only numeric or bitwise operators.
- Its value can be evaluated to a constant when complied.

_Example:_

```
case {Value, Result} of
    {?THRESHOLD+1, ok} -> ...
```

## The Match Operator

The following matches `Pattern` against `Expr`:

```
Pattern = Expr
```

If the matching succeeds, any unbound variable in the pattern becomes bound and
the value of `Expr` is returned.

If multiple match operators are applied in sequence, they will be evaluated from
right to left.

If the matching fails, a `badmatch` run-time error occurs.

_Examples:_

```
1> {A, B} = T = {answer, 42}.
{answer,42}
2> A.
answer
3> B.
42
4> T.
{answer,42}
5> {C, D} = [1, 2].
** exception error: no match of right-hand side value [1,2]
```

Because multiple match operators are evaluated from right to left, it means
that:

```
Pattern1 = Pattern2 = . . . = PatternN = Expression
```

is equivalent to:

```
Temporary = Expression,
PatternN = Temporary,
   .
   .
   .,
Pattern2 = Temporary,
Pattern = Temporary
```

## The Match Operator and the Compound Pattern Operator

> #### Note {: .info }
>
> This is an advanced section, which references to topics not yet introduced. It
> can safely be skipped on a first reading.

The `=` character is used to denote two similar but distinct operators: the
match operator and the compound pattern operator. Which one is meant is
determined by context.

The _compound pattern operator_ is used to construct a compound pattern from two
patterns. Compound patterns are accepted everywhere a pattern is accepted. A
compound pattern matches if all of its constituent patterns match. It is not
legal for a pattern that is part of a compound pattern to use variables (as keys
in map patterns or sizes in binary patterns) bound in other sub patterns of the
same compound pattern.

_Examples:_

```
1> fun(#{Key := Value} = #{key := Key}) -> Value end.
* 1:7: variable 'Key' is unbound
2> F = fun({A, B} = E) -> {E, A + B} end, F({1,2}).
{{1,2},3}
3> G = fun(<<A:8,B:8>> = <<C:16>>) -> {A, B, C} end, G(<<42,43>>).
{42,43,10795}
```

The _match operator_ is allowed everywhere an expression is allowed. It is used
to match the value of an expression to a pattern. If multiple match operators
are applied in sequence, they will be evaluated from right to left.

_Examples:_

```
1> M = #{key => key2, key2 => value}.
#{key => key2,key2 => value}
2> f(Key), #{Key := Value} = #{key := Key} = M, Value.
value
3> f(Key), #{Key := Value} = (#{key := Key} = M), Value.
value
4> f(Key), (#{Key := Value} = #{key := Key}) = M, Value.
* 1:12: variable 'Key' is unbound
5> <<X:Y>> = begin Y = 8, <<42:8>> end, X.
42
```

The expression at prompt `2>` first matches the value of variable `M` against
pattern `#{key := Key}`, binding variable `Key`. It then matches the value of
`M` against pattern `#{Key := Value}` using variable `Key` as the key, binding
variable `Value`.

The expression at prompt `3>` matches expression `(#{key := Key} = M)` against
pattern `#{Key := Value}`. The expression inside the parentheses is evaluated
first. That is, `M` is matched against `#{key := Key}`, and then the value of
`M` is matched against pattern `#{Key := Value}`. That is the same evaluation
order as in _2_; therefore, the parentheses are redundant.

In the expression at prompt `4>` the expression `M` is matched against a pattern
inside parentheses. Since the construct inside the parentheses is a pattern, the
`=` that separates the two patterns is the compound pattern operator (_not_ the
match operator). The match fails because the two sub patterns are matched at the
same time, and the variable `Key` is therefore not bound when matching against
pattern `#{Key := Value}`.

In the expression at prompt `5>` the expressions inside the
[block expression](expressions.md#block-expressions) are evaluated first,
binding variable `Y` and creating a binary. The binary is then matched against
pattern `<<X:Y>>` using the value of `Y` as the size of the segment.

## Function Calls

```
ExprF(Expr1,...,ExprN)
ExprM:ExprF(Expr1,...,ExprN)
```

In the first form of function calls, `ExprM:ExprF(Expr1,...,ExprN)`, each of
`ExprM` and `ExprF` must be an atom or an expression that evaluates to an atom.
The function is said to be called by using the _fully qualified function name_.
This is often referred to as a _remote_ or _external function call_.

_Example:_

```
lists:keyfind(Name, 1, List)
```

In the second form of function calls, `ExprF(Expr1,...,ExprN)`, `ExprF` must be
an atom or evaluate to a fun.

If `ExprF` is an atom, the function is said to be called by using the
_implicitly qualified function name_. If the function `ExprF` is locally
defined, it is called. Alternatively, if `ExprF` is explicitly imported from the
`M` module, `M:ExprF(Expr1,...,ExprN)` is called. If `ExprF` is neither declared
locally nor explicitly imported, `ExprF` must be the name of an automatically
imported BIF.

_Examples:_

```
handle(Msg, State)
spawn(m, init, [])
```

_Examples_ where `ExprF` is a fun:

```
1> Fun1 = fun(X) -> X+1 end,
Fun1(3).
4
2> fun lists:append/2([1,2], [3,4]).
[1,2,3,4]
3>
```

Notice that when calling a local function, there is a difference between using
the implicitly or fully qualified function name. The latter always refers to the
latest version of the module. See
[Compilation and Code Loading ](code_loading.md)and
[Function Evaluation](ref_man_functions.md#eval).

### Local Function Names Clashing With Auto-Imported BIFs

If a local function has the same name as an auto-imported BIF, the semantics is
that implicitly qualified function calls are directed to the locally defined
function, not to the BIF. To avoid confusion, there is a compiler directive
available, `-compile({no_auto_import,[F/A]})`, that makes a BIF not being
auto-imported. In certain situations, such a compile-directive is mandatory.

> #### Change {: .info }
>
> Before Erlang/OTP R14A (ERTS version 5.8), an implicitly qualified function call to a
> function having the same name as an auto-imported BIF always resulted in the
> BIF being called. In newer versions of the compiler, the local function is
> called instead. This is to avoid that future additions to the set of
> auto-imported BIFs do not silently change the behavior of old code.

However, to avoid that old (pre R14) code changed its behavior when compiled
with Erlang/OTP version R14A or later, the following restriction applies: If you
override the name of a BIF that was auto-imported in OTP versions prior to R14A
(ERTS version 5.8) and have an implicitly qualified call to that function in
your code, you either need to explicitly remove the auto-import using a compiler
directive, or replace the call with a fully qualified function call. Otherwise
you get a compilation error. See the following example:

```
-export([length/1,f/1]).

-compile({no_auto_import,[length/1]}). % erlang:length/1 no longer autoimported

length([]) ->
    0;
length([H|T]) ->
    1 + length(T). %% Calls the local function length/1

f(X) when erlang:length(X) > 3 -> %% Calls erlang:length/1,
                                  %% which is allowed in guards
    long.
```

The same logic applies to explicitly imported functions from other modules, as
to locally defined functions. It is not allowed to both import a function from
another module and have the function declared in the module at the same time:

```
-export([f/1]).

-compile({no_auto_import,[length/1]}). % erlang:length/1 no longer autoimported

-import(mod,[length/1]).

f(X) when erlang:length(X) > 33 -> %% Calls erlang:length/1,
                                   %% which is allowed in guards

    erlang:length(X);              %% Explicit call to erlang:length in body

f(X) ->
    length(X).                     %% mod:length/1 is called
```

For auto-imported BIFs added in Erlang/OTP R14A and thereafter, overriding the
name with a local function or explicit import is always allowed. However, if the
`-compile({no_auto_import,[F/A])` directive is not used, the compiler issues a
warning whenever the function is called in the module using the implicitly
qualified function name.

## If

```
if
    GuardSeq1 ->
        Body1;
    ...;
    GuardSeqN ->
        BodyN
end
```

The branches of an `if`\-expression are scanned sequentially until a guard
sequence `GuardSeq` that evaluates to true is found. Then the corresponding
`Body` (a sequence of expressions separated by `,`) is evaluated.

The return value of `Body` is the return value of the `if` expression.

If no guard sequence is evaluated as true, an `if_clause` run-time error occurs.
If necessary, the guard expression `true` can be used in the last branch, as
that guard sequence is always true.

_Example:_

```
is_greater_than(X, Y) ->
    if
        X > Y ->
            true;
        true -> % works as an 'else' branch
            false
    end
```

## Case

```
case Expr of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

The expression `Expr` is evaluated and the patterns `Pattern` are sequentially
matched against the result. If a match succeeds and the optional guard sequence
`GuardSeq` is true, the corresponding `Body` is evaluated.

The return value of `Body` is the return value of the `case` expression.

If there is no matching pattern with a true guard sequence, a `case_clause`
run-time error occurs.

_Example:_

```
is_valid_signal(Signal) ->
    case Signal of
        {signal, _What, _From, _To} ->
            true;
        {signal, _What, _To} ->
            true;
        _Else ->
            false
    end.
```

## Maybe

> #### Change {: .info }
>
> The `maybe` [feature](`e:system:features.md#features`) was introduced
> in Erlang/OTP 25. Starting from Erlang/OTP 27 is is enabled by default.

```
maybe
    Expr1,
    ...,
    ExprN
end
```

The expressions in a `maybe` block are evaluated sequentially. If all
expressions are evaluated successfully, the return value of the `maybe` block is
`ExprN`. However, execution can be short-circuited by a conditional match
expression:

```
Expr1 ?= Expr2
```

`?=` is called the conditional match operator. It is only allowed to be used at
the top-level of a `maybe` block. It matches the pattern `Expr1` against
`Expr2`. If the matching succeeds, any unbound variable in the pattern becomes
bound. If the expression is the last expression in the `maybe` block, it also
returns the value of `Expr2`. If the matching is unsuccessful, the rest of the
expressions in the `maybe` block are skipped and the return value of the `maybe`
block is `Expr2`.

None of the variables bound in a `maybe` block must be used in the code that
follows the block.

Here is an example:

```
maybe
    {ok, A} ?= a(),
    true = A >= 0,
    {ok, B} ?= b(),
    A + B
end
```

Let us first assume that `a()` returns `{ok,42}` and `b()` returns `{ok,58}`.
With those return values, all of the match operators will succeed, and the
return value of the `maybe` block is `A + B`, which is equal to `42 + 58 = 100`.

Now let us assume that `a()` returns `error`. The conditional match operator in
`{ok, A} ?= a()` fails to match, and the return value of the `maybe` block is
the value of the expression that failed to match, namely `error`. Similarly, if
`b()` returns `wrong`, the return value of the `maybe` block is `wrong`.

Finally, let us assume that `a()` returns `{ok,-1}`. Because `true = A >= 0` uses
the match operator `=`, a `{badmatch,false}` run-time error occurs when the
expression fails to match the pattern.

The example can be written in a less succinct way using nested case expressions:

```
case a() of
    {ok, A} ->
        true = A >= 0,
        case b() of
            {ok, B} ->
                A + B;
            Other1 ->
                Other1
        end;
    Other2 ->
        Other2
end
```

The `maybe` block can be augmented with `else` clauses:

```
maybe
    Expr1,
    ...,
    ExprN
else
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

If a conditional match operator fails, the failed expression is matched against
the patterns in all clauses between the `else` and `end` keywords. If a match
succeeds and the optional guard sequence `GuardSeq` is true, the corresponding
`Body` is evaluated. The value returned from the body is the return value of the
`maybe` block.

If there is no matching pattern with a true guard sequence, an `else_clause`
run-time error occurs.

None of the variables bound in a `maybe` block must be used in the `else`
clauses. None of the variables bound in the `else` clauses must be used in the
code that follows the `maybe` block.

Here is the previous example augmented with `else` clauses:

```
maybe
    {ok, A} ?= a(),
    true = A >= 0,
    {ok, B} ?= b(),
    A + B
else
    error -> error;
    wrong -> error
end
```

The `else` clauses translate the failing value from the conditional match
operators to the value `error`. If the failing value is not one of the
recognized values, a `else_clause` run-time error occurs.

## Send

```
Expr1 ! Expr2
```

Sends the value of `Expr2` as a message to the process specified by `Expr1`. The
value of `Expr2` is also the return value of the expression.

`Expr1` must evaluate to a pid, an alias (reference), a port, a registered name
(atom), or a tuple `{Name,Node}`. `Name` is an atom and `Node` is a node name,
also an atom.

- If `Expr1` evaluates to a name, but this name is not registered, a `badarg`
  run-time error occurs.
- Sending a message to a reference never fails, even if the reference is no
  longer (or never was) an alias.
- Sending a message to a pid never fails, even if the pid identifies a
  non-existing process.
- Distributed message sending, that is, if `Expr1` evaluates to a tuple
  `{Name,Node}` (or a pid located at another node), also never fails.

## Receive

```
receive
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

The `receive` expression searches for a message in the message queue that match
one of the patterns in the clauses of the `receive` expression. The patterns in
the clauses is matched against a message from top to bottom. The first message,
from the start of the message queue, that matches will be selected. Messages are
normally
[enqueued in the message queue](ref_man_processes.md#message-queue-order) in
order they were received. However,
if [reception of priority messages](ref_man_processes.md#enable-prio-msg-recv)
has been enabled by the receiving process, this is not always the case. When a
match succeeds and the optional guard sequence `GuardSeq` is true, the matching
message is fetched from the message queue and the corresponding `Body` is
evaluated. All other messages in the message queue remain unchanged.

The return value of `Body` is the return value of the `receive` expression.

`receive` never fails. The execution is suspended, possibly indefinitely, until
a message arrives that matches one of the patterns and with a true guard
sequence.

[](){: #selective-receive-warning }
> #### Warning {: .warning }
> The time complexity of a `receive` expression is `O(N)` where `N` corresponds
> to the amount of messages preceeding the matching message in the message queue.
> That is, when the combination of patterns of a `receive` expression only match
> specific messages and the message queue is huge, executing such a `receive`
> expression might become very expensive.
>
> One type of `receive` expressions matching on only specific patterns can,
> however, be optimized by the compiler and runtime system. This in the scenario
> where you create a [*reference*](`e:system:data_types.md#reference`) and
> match on it in all clauses of a `receive` expression *close* to where the
> reference was created. In this case only the amount of messages received after
> the reference was created needs to be inspected. For more information see the
> [*Fetching Received Messages* section of the *Efficiency Guide*](`e:system:eff_guide_processes.md#fetching-received-messages`).

_Example:_

```
wait_for_onhook() ->
    receive
        onhook ->
            disconnect(),
            idle();
        {connect, B} ->
            B ! {busy, self()},
            wait_for_onhook()
    end.
```

The `receive` expression can be augmented with a timeout:

```
receive
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
after
    ExprT ->
        BodyT
end
```

`receive...after` works exactly as `receive`, except that if no matching message
has arrived within `ExprT` milliseconds, then `BodyT` is evaluated instead. The
return value of `BodyT` then becomes the return value of the `receive...after`
expression. `ExprT` is to evaluate to an integer, or the atom `infinity`. The
allowed integer range is from 0 to 4294967295, that is, the longest possible
timeout is almost 50 days. With a zero value the timeout occurs immediately if
there is no matching message in the message queue.

> #### Warning {: .warning }
> It might seem like a `receive` expression with an `after 0` clause (or
> another short timeout) might be cheap since the timeout is short. This is
> *not* necessarily the case. If the patterns in the clauses of the `receive`
> expression only match specific messages and no such messages exist in the
> message queue, the whole message queue needs to be inspected before the
> timeout can occur. That is, the same apply as in
> [the warning above](#selective-receive-warning).

The atom `infinity` will make the process wait indefinitely for a matching
message. This is the same as not using a timeout. It can be useful for timeout
values that are calculated at runtime.

_Example:_

```
wait_for_onhook() ->
    receive
        onhook ->
            disconnect(),
            idle();
        {connect, B} ->
            B ! {busy, self()},
            wait_for_onhook()
    after
        60000 ->
            disconnect(),
            error()
    end.
```

It is legal to use a `receive...after` expression with no branches:

```
receive
after
    ExprT ->
        BodyT
end
```

This construction does not consume any messages, only suspends execution in the
process for `ExprT` milliseconds. This can be used to implement simple timers.

_Example:_

```
timer() ->
    spawn(m, timer, [self()]).

timer(Pid) ->
    receive
    after
        5000 ->
            Pid ! timeout
    end.
```

For more information on timers in Erlang in general, see the
[*Timers*](`e:erts:time_correction.md#timers`) section of the
[*Time and Time Correction in Erlang*](`e:erts:time_correction.md`)
ERTS User's guide.

## Term Comparisons

```
Expr1 op Expr2
```

| op    | Description              |
| ----- | ------------------------ |
| `==`  | Equal to                 |
| `/=`  | Not equal to             |
| `=<`  | Less than or equal to    |
| `<`   | Less than                |
| `>=`  | Greater than or equal to |
| `>`   | Greater than             |
| `=:=` | Term equivalence         |
| `=/=` | Term non-equivalence     |

_Table: Term Comparison Operators._

The arguments can be of different data types. The following order is defined:

```text
number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
```

`nil` in the previous expression represents the empty list (`[]`), which is
regarded as a separate type from `t:list/0`. That is why `nil < list`.

Lists are compared element by element. Tuples are ordered by size, two tuples
with the same size are compared element by element.

Bit strings are compared bit by bit. If one bit string is a prefix of the other,
the shorter bit string is considered smaller.

Maps are ordered by size, two maps with the same size are compared by keys in
ascending term order and then by values in key order. In maps key order integers
types are considered less than floats types.

Atoms are compared using their string value, codepoint by codepoint.

When comparing an integer to a float, the term with the lesser precision is
converted into the type of the other term, unless the operator is one of `=:=`
or `=/=`. A float is more precise than an integer until all significant figures
of the float are to the left of the decimal point. This happens when the float
is larger/smaller than +/-9007199254740992.0. The conversion strategy is changed
depending on the size of the float because otherwise comparison of large floats
and integers would lose their transitivity.

The term equivalence operators, `=:=` and `=/=`, return whether two terms are
indistinguishable. While the other operators consider the same _numbers_ equal
even when their types differ (`1 == 1.0` is true), the term equivalence
operators return whether or not there exists a way to tell the arguments apart.

For example, while the terms `0` and `0.0` represent the same _number_, we can
tell them apart by using the [`is_integer/1`](`is_integer/1`) function. Hence,
`=:=` and `=/=` consider them different.

Furthermore, the terms `0.0` and `-0.0` also represent the same _number_, but
they yield different results when converted to string form through
[`float_to_list/1`](`float_to_list/1`): when given the former it returns a
string without a sign, and when given the latter it returns a string with a
sign. Therefore, `=:=` and `=/=` consider them different.

The term equivalence operators are useful when reasoning about terms as opaque
values, for example in associative containers or memoized functions where using
the equal-to operator (`==`) risks producing incorrect results as a consequence
of mixing up numbers of different types.

Term comparison operators return the Boolean value of the expression, `true` or
`false`.

_Examples:_

```
1> 1 == 1.0.
true
2> 1 =:= 1.0.
false
3> 0 =:= 0.0.
false
4> 0.0 =:= -0.0.
false
5> 0.0 =:= +0.0.
true
6> 1 > a.
false
7> #{c => 3} > #{a => 1, b => 2}.
false
8> #{a => 1, b => 2} == #{a => 1.0, b => 2.0}.
true
9> <<2:2>> < <<128>>.
true
10> <<3:2>> < <<128>>.
false
```

> #### Note {: .info }
>
> Prior to OTP 27, the term equivalence operators considered `0.0`
> and `-0.0` to be the same term.
>
> This was changed in OTP 27 but legacy code may have expected them to be
> considered the same. To help users catch errors that may arise from an
> upgrade, the compiler raises a warning when `0.0` is pattern-matched or used
> in a term equivalence test.
>
> If you need to match `0.0` specifically, the warning can be silenced by
> writing `+0.0` instead, which produces the same term but makes the compiler
> interpret the match as being done on purpose.

## Arithmetic Expressions

```
op Expr
Expr1 op Expr2
```

| Operator | Description               | Argument Type |
| -------- | ------------------------- | ------------- |
| `+`      | Unary +                   | Number        |
| `-`      | Negation (unary -)        | Number        |
| `+`      | Addition                  | Number        |
| `-`      | Subtraction               | Number        |
| `*`      | Multiplication            | Number        |
| `/`      | Floating point division   | Number        |
| `bnot`   | Unary bitwise NOT         | Integer       |
| `div`    | Integer division          | Integer       |
| `rem`    | Integer remainder of X/Y  | Integer       |
| `band`   | Bitwise AND               | Integer       |
| `bor`    | Bitwise OR                | Integer       |
| `bxor`   | Bitwise XOR               | Integer       |
| `bsl`    | Bitshift left             | Integer       |
| `bsr`    | Arithmetic bitshift right | Integer       |

_Table: Arithmetic Operators._

_Examples:_

```
1> +1.
1
2> -1.
-1
3> 1+1.
2
4> 4/2.
2.0
5> 5 div 2.
2
6> 5 rem 2.
1
7> 2#10 band 2#01.
0
8> 2#10 bor 2#01.
3
9> a + 10.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as a + 10
10> 1 bsl (1 bsl 64).
** exception error: a system limit has been reached
     in operator  bsl/2
        called as 1 bsl 18446744073709551616
```

## Boolean Expressions

```
op Expr
Expr1 op Expr2
```

| Operator | Description       |
| -------- | ----------------- |
| `not`    | Unary logical NOT |
| `and`    | Logical AND       |
| `or`     | Logical OR        |
| `xor`    | Logical XOR       |

_Table: Logical Operators._

_Examples:_

```
1> not true.
false
2> true and false.
false
3> true xor false.
true
4> true or garbage.
** exception error: bad argument
     in operator  or/2
        called as true or garbage
```

## Short-Circuit Expressions

```
Expr1 orelse Expr2
Expr1 andalso Expr2
```

`Expr2` is evaluated only if necessary. That is, `Expr2` is evaluated only if:

- `Expr1` evaluates to `false` in an `orelse` expression.

or

- `Expr1` evaluates to `true` in an `andalso` expression.

Returns either the value of `Expr1` (that is, `true` or `false`) or the value of
`Expr2` (if `Expr2` is evaluated).

_Example 1:_

```
case A >= -1.0 andalso math:sqrt(A+1) > B of
```

This works even if `A` is less than `-1.0`, since in that case, `math:sqrt/1` is
never evaluated.

_Example 2:_

```
OnlyOne = is_atom(L) orelse
         (is_list(L) andalso length(L) == 1),
```

`Expr2` is not required to evaluate to a Boolean value. Because of that,
`andalso` and `orelse` are tail-recursive.

_Example 3 (tail-recursive function):_

```
all(Pred, [Hd|Tail]) ->
    Pred(Hd) andalso all(Pred, Tail);
all(_, []) ->
    true.
```

> #### Change {: .info }
>
> Before Erlang/OTP R13A, `Expr2` was required to evaluate to a Boolean value,
> and as consequence, `andalso` and `orelse` were **not** tail-recursive.

## List Operations

```
Expr1 ++ Expr2
Expr1 -- Expr2
```

The list concatenation operator `++` appends its second argument to its first
and returns the resulting list.

The list subtraction operator `--` produces a list that is a copy of the first
argument. The procedure is as follows: for each element in the second argument,
the first occurrence of this element (if any) is removed.

_Example:_

```
1> [1,2,3] ++ [4,5].
[1,2,3,4,5]
2> [1,2,3,2,1,2] -- [2,1,2].
[3,1,2]
```

## Map Expressions

### Creating Maps

Constructing a new map is done by letting an expression `K` be associated with
another expression `V`:

```
#{K => V}
```

New maps can include multiple associations at construction by listing every
association:

```
#{K1 => V1, ..., Kn => Vn}
```

An empty map is constructed by not associating any terms with each other:

```
#{}
```

All keys and values in the map are terms. Any expression is first evaluated and
then the resulting terms are used as _key_ and _value_ respectively.

Keys and values are separated by the `=>` arrow and associations are separated
by a comma (`,`).

_Examples:_

```
M0 = #{},                 % empty map
M1 = #{a => <<"hello">>}, % single association with literals
M2 = #{1 => 2, b => b},   % multiple associations with literals
M3 = #{k => {A,B}},       % single association with variables
M4 = #{{"w", 1} => f()}.  % compound key associated with an evaluated expression
```

Here, `A` and `B` are any expressions and `M0` through `M4` are the resulting
map terms.

If two matching keys are declared, the latter key takes precedence.

_Example:_

```
1> #{1 => a, 1 => b}.
#{1 => b }
2> #{1.0 => a, 1 => b}.
#{1 => b, 1.0 => a}
```

The order in which the expressions constructing the keys (and their associated
values) are evaluated is not defined. The syntactic order of the key-value pairs
in the construction is of no relevance, except in the recently mentioned case of
two matching keys.

### Updating Maps

Updating a map has a similar syntax as constructing it.

An expression defining the map to be updated is put in front of the expression
defining the keys to be updated and their respective values:

```
M#{K => V}
```

Here `M` is a term of type map and `K` and `V` are any expression.

If key `K` does not match any existing key in the map, a new association is
created from key `K` to value `V`.

If key `K` matches an existing key in map `M`, its associated value is replaced
by the new value `V`. In both cases, the evaluated map expression returns a new
map.

If `M` is not of type map, an exception of type `badmap` is raised.

To only update an existing value, the following syntax is used:

```
M#{K := V}
```

Here `M` is a term of type map, `V` is an expression and `K` is an expression
that evaluates to an existing key in `M`.

If key `K` does not match any existing keys in map `M`, an exception of type
`badkey` is raised at runtime. If a matching key `K` is present in map `M`,
its associated value is replaced by the new value `V`, and the evaluated map
expression returns a new map.

If `M` is not of type map, an exception of type `badmap` is raised.

_Examples:_

```
M0 = #{},
M1 = M0#{a => 0},
M2 = M1#{a => 1, b => 2},
M3 = M2#{"function" => fun() -> f() end},
M4 = M3#{a := 2, b := 3}.  % 'a' and 'b' was added in `M1` and `M2`.
```

Here `M0` is any map. It follows that `M1` through `M4` are maps as well.

_More examples:_

```
1> M = #{1 => a}.
#{1 => a }
2> M#{1.0 => b}.
#{1 => a, 1.0 => b}.
3> M#{1 := b}.
#{1 => b}
4> M#{1.0 := b}.
** exception error: bad argument
```

As in construction, the order in which the key and value expressions are
evaluated is not defined. The syntactic order of the key-value pairs in the
update is of no relevance, except in the case where two keys match. In that
case, the latter value is used.

### Maps in Patterns

Matching of key-value associations from maps is done as follows:

```
#{K := V} = M
```

Here `M` is any map. The key `K` must be a
[guard expression](expressions.md#guard-expressions), with all variables already
bound. `V` can be any pattern with either bound or unbound variables.

If the variable `V` is unbound, it becomes bound to the value associated with
the key `K`, which must exist in the map `M`. If the variable `V` is bound, it
must match the value associated with `K` in `M`.

> #### Change {: .info }
>
> Before Erlang/OTP 23, the expression defining the key `K` was restricted to be
> either a single variable or a literal.

_Example:_

```
1> M = #{"tuple" => {1,2}}.
#{"tuple" => {1,2}}
2> #{"tuple" := {1,B}} = M.
#{"tuple" => {1,2}}
3> B.
2.
```

This binds variable `B` to integer `2`.

Similarly, multiple values from the map can be matched:

```
#{K1 := V1, ..., Kn := Vn} = M
```

Here keys `K1` through `Kn` are any expressions with literals or bound
variables. If all key expressions evaluate successfully and all keys
exist in map `M`, all variables in `V1 .. Vn` is matched to the
associated values of their respective keys.

If the matching conditions are not met the match fails.

Note that when matching a map, only the `:=` operator (not the `=>`) is allowed
as a delimiter for the associations.

The order in which keys are declared in matching has no relevance.

Duplicate keys are allowed in matching and match each pattern associated to the
keys:

```
#{K := V1, K := V2} = M
```

The empty map literal (`#{}`) matches any map when used as a pattern:

```
#{} = Expr
```

This expression matches if the expression `Expr` is of type map, otherwise it
fails with an exception `badmatch`.

Here the key to be retrieved is constructed from an expression:

```
#{{tag,length(List)} := V} = Map
```

`List` must be an already bound variable.

#### Matching Syntax

Matching of literals as keys are allowed in function heads:

```
%% only start if not_started
handle_call(start, From, #{state := not_started} = S) ->
...
    {reply, ok, S#{state := start}};

%% only change if started
handle_call(change, From, #{state := start} = S) ->
...
    {reply, ok, S#{state := changed}};
```

### Maps in Guards

Maps are allowed in guards as long as all subexpressions are valid guard
expressions.

The following guard BIFs handle maps:

- [is_map/1](`erlang:is_map/1`) in the `erlang` module
- [is_map_key/2](`erlang:is_map_key/2`) in the `erlang` module
- [map_get/2](`erlang:map_get/2`) in the `erlang` module
- [map_size/1](`erlang:map_size/1`) in the `erlang` module

## Bit Syntax Expressions

The bit syntax operates on _bit strings_. A bit string is a sequence of bits
ordered from the most significant bit to the least significant bit.

```
<<>>  % The empty bit string, zero length
<<E1>>
<<E1,...,En>>
```

Each element `Ei` specifies a _segment_ of the bit string. The segments are
ordered left to right from the most significant bit to the least significant bit
of the bit string.

Each segment specification `Ei` is a value, whose default type is `integer`,
followed by an optional _size expression_ and an optional _type specifier list_.

```
Ei = Value |
     Value:Size |
     Value/TypeSpecifierList |
     Value:Size/TypeSpecifierList
```

When used in a bit string construction, `Value` is an expression that is to
evaluate to an integer, float, or bit string. If the expression is not a single
literal or variable, it is to be enclosed in parentheses.

When used in a bit string matching, `Value` must be a variable, or an integer,
float, or string.

Notice that, for example, using a string literal as in `<<"abc">>` is syntactic
sugar for `<<$a,$b,$c>>`.

When used in a bit string construction, `Size` is an expression that is to
evaluate to an integer.

When used in a bit string matching, `Size` must be a
[guard expression](expressions.md#guard-expressions) that evaluates to an
integer. All variables in the guard expression must be already bound.

> #### Change {: .info }
>
> Before Erlang/OTP 23, `Size` was restricted to be an integer or a variable
> bound to an integer.

The value of `Size` specifies the size of the segment in units (see below). The
default value depends on the type (see below):

- For `integer` it is 8.
- For `float` it is 64.
- For `binary` and `bitstring` it is the whole binary or bit string.

In matching, the default value for a binary or bit string segment is only valid
for the last element. All other bit string or binary elements in the matching
must have a size specification.

[](){: #binaries }

**Binaries**

A bit string with a length that is a multiple of 8 bits is known as a _binary_,
which is the most common and useful type of bit string.

A binary has a canonical representation in memory. Here follows a sequence of
bytes where each byte's value is its sequence number:

```
<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
```

Bit strings are a later generalization of binaries, so many texts and much
information about binaries apply just as well for bit strings.

**Example:**

```
1> <<A/binary, B/binary>> = <<"abcde">>.
* 1:3: a binary field without size is only allowed at the end of a binary pattern
2> <<A:3/binary, B/binary>> = <<"abcde">>.
<<"abcde">>
3> A.
<<"abc">>
4> B.
<<"de">>
```

For the `utf8`, `utf16`, and `utf32` types, `Size` must not be given. The size
of the segment is implicitly determined by the type and value itself.

`TypeSpecifierList` is a list of type specifiers, in any order, separated by
hyphens (-). Default values are used for any omitted type specifiers.

- **`Type`= `integer` | `float` | `binary` | `bytes` | `bitstring` | `bits` |
  `utf8` | `utf16` | `utf32`** - The default is `integer`. `bytes` is a
  shorthand for `binary` and `bits` is a shorthand for `bitstring`. See below
  for more information about the `utf` types.

- **`Signedness`= `signed` | `unsigned`** - Only matters for matching and when
  the type is `integer`. The default is `unsigned`.

- **`Endianness`= `big` | `little` | `native`** - Specifies byte level (octet
  level) endianness (byte order). Native-endian means that the endianness is
  resolved at load time to be either big-endian or little-endian, depending on
  what is native for the CPU that the Erlang machine is run on. Endianness only
  matters when the **Type** is either `integer`, `utf16`, `utf32`, or `float`. The
  default is `big`.

  ```
  <<16#1234:16/little>> = <<16#3412:16>> = <<16#34:8, 16#12:8>>
  ```

- **`Unit`= `unit:IntegerLiteral`** - The allowed range is 1 through 256.
  Defaults to 1 for `integer`, `float`, and `bitstring`, and to 8 for `binary`.
  For types `bitstring`, `bits`, and `bytes`, it is not allowed to specify a
  unit value different from the default value. No unit specifier must be given
  for the types `utf8`, `utf16`, and `utf32`.

### Integer segments

The value of `Size` multiplied with the unit gives the size of the segment in
bits.

When constructing bit strings, if the size `N` of an integer segment is too
small to contain the given integer, the most significant bits of the integer are
silently discarded and only the `N` least significant bits are put into the bit
string. For example, `<<16#ff:4>>` will result in the bit string `<<15:4>>`.

### Float segments

The value of `Size` multiplied with the unit gives the size of the segment in
bits. The size of a float segment in bits must be one of 16, 32, or 64.

When constructing bit strings, if the size of a float segment is too small to
contain the representation of the given float value, an exception is raised.

When matching bit strings, matching of float segments fails if the bits of the
segment does not contain the representation of a finite floating point value.

### Binary segments

In this section, the phrase "binary segment" refers to any one of the segment
types `binary`, `bitstring`, `bytes`, and `bits`.

See also the paragraphs about [Binaries](expressions.md#binaries).

When constructing binaries and no size is specified for a binary segment, the
entire binary value is interpolated into the binary being constructed. However,
the size in bits of the binary being interpolated must be evenly divisible by
the unit value for the segment; otherwise an exception is raised.

For example, the following examples all succeed:

```
1> <<(<<"abc">>)/bitstring>>.
<<"abc">>
2> <<(<<"abc">>)/binary-unit:1>>.
<<"abc">>
3> <<(<<"abc">>)/binary>>.
<<"abc">>
```

The first two examples have a unit value of 1 for the segment, while the third
segment has a unit value of 8.

Attempting to interpolate a bit string of size 1 into a binary segment with unit
8 (the default unit for `binary`) fails as shown in this example:

```
1> <<(<<1:1>>)/binary>>.
** exception error: bad argument
```

For the construction to succeed, the unit value of the segment must be 1:

```
2> <<(<<1:1>>)/bitstring>>.
<<1:1>>
3> <<(<<1:1>>)/binary-unit:1>>.
<<1:1>>
```

Similarly, when matching a binary segment with no size specified, the match
succeeds if and only if the size in bits of the rest of the binary is evenly
divisible by the unit value:

```
1> <<_/binary-unit:16>> = <<"">>.
<<>>
2> <<_/binary-unit:16>> = <<"a">>.
** exception error: no match of right hand side value <<"a">>
3> <<_/binary-unit:16>> = <<"ab">>.
<<"ab">>
4> <<_/binary-unit:16>> = <<"abc">>.
** exception error: no match of right hand side value <<"abc">>
5> <<_/binary-unit:16>> = <<"abcd">>.
<<"abcd">>
```

When a size is explicitly specified for a binary segment, the segment size in
bits is the value of `Size` multiplied by the default or explicit unit value.

When constructing binaries, the size of the binary being interpolated into the
constructed binary must be at least as large as the size of the binary segment.

**Examples:**

```
1> <<(<<"abc">>):2/binary>>.
<<"ab">>
2> <<(<<"a">>):2/binary>>.
** exception error: construction of binary failed
        *** segment 1 of type 'binary': the value <<"a">> is shorter than the size of the segment
```

### Unicode segments

The types `utf8`, `utf16`, and `utf32` specifies encoding/decoding of the
*Unicode Transformation Format*s [UTF-8](https://en.wikipedia.org/wiki/UTF-8),
[UTF-16](https://en.wikipedia.org/wiki/UTF-16), and
[UTF-32](https://en.wikipedia.org/wiki/UTF-32), respectively.

When constructing a segment of a `utf` type, `Value` must be an integer in the
range `0` through `16#D7FF` or `16#E000` through `16#10FFFF`. Construction fails with a
`badarg` exception if `Value` is outside the allowed ranges. The sizes of the
encoded values are as follows:

- For `utf8`, `Value` is encoded in 1-4 bytes.
- For `utf16`, `Value` is encoded in 2 or 4 bytes.
- For `utf32`, `Value` is encoded in 4 bytes.

When constructing, a literal string can be given followed by one of the UTF
types, for example: `<<"abc"/utf8>>` which is syntactic sugar for
`<<$a/utf8,$b/utf8,$c/utf8>>`.

A successful match of a segment of a `utf` type, results in an integer in the
range `0` through `16#D7FF` or `16#E000` through `16#10FFFF`. The match fails if the
returned value falls outside those ranges.

A segment of type `utf8` matches 1-4 bytes in the bit string, if the bit string
at the match position contains a valid UTF-8 sequence. (See RFC-3629 or the
Unicode standard.)

A segment of type `utf16` can match 2 or 4 bytes in the bit string. The match
fails if the bit string at the match position does not contain a legal UTF-16
encoding of a Unicode code point. (See RFC-2781 or the Unicode standard.)

A segment of type `utf32` can match 4 bytes in the bit string in the same way as
an `integer` segment matches 32 bits. The match fails if the resulting integer
is outside the legal ranges previously mentioned.

_Examples:_

```
1> Bin1 = <<1,17,42>>.
<<1,17,42>>
2> Bin2 = <<"abc">>.
<<97,98,99>>

3> Bin3 = <<1,17,42:16>>.
<<1,17,0,42>>
4> <<A,B,C:16>> = <<1,17,42:16>>.
<<1,17,0,42>>
5> C.
42
6> <<D:16,E,F>> = <<1,17,42:16>>.
<<1,17,0,42>>
7> D.
273
8> F.
42
9> <<G,H/binary>> = <<1,17,42:16>>.
<<1,17,0,42>>
10> H.
<<17,0,42>>
11> <<G,J/bitstring>> = <<1,17,42:12>>.
<<1,17,2,10:4>>
12> J.
<<17,2,10:4>>

13> <<1024/utf8>>.
<<208,128>>

14> <<1:1,0:7>>.
<<128>>
15> <<16#123:12/little>> = <<16#231:12>> = <<2:4, 3:4, 1:4>>.
<<35,1:4>>
```

Notice that bit string patterns cannot be nested.

Notice also that "`B=<<1>>`" is interpreted as "`B =< <1>>`" which is a syntax
error. The correct way is to write a space after `=`: "`B = <<1>>`.

More examples are provided in [Programming Examples](`e:system:bit_syntax.md`).

## Fun Expressions

```
fun
    [Name](Pattern11,...,Pattern1N) [when GuardSeq1] ->
              Body1;
    ...;
    [Name](PatternK1,...,PatternKN) [when GuardSeqK] ->
              BodyK
end
```

A fun expression begins with the keyword `fun` and ends with the keyword `end`.
Between them is to be a function declaration, similar to a
[regular function declaration](ref_man_functions.md#function-declaration-syntax),
except that the function name is optional and is to be a variable, if any.

Variables in a fun head shadow the function name and both shadow variables in
the function clause surrounding the fun expression. Variables bound in a fun
body are local to the fun body.

The return value of the expression is the resulting fun.

_Examples:_

```
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.39074546>
2> Fun1(2).
3
3> Fun2 = fun (X) when X>=5 -> gt; (X) -> lt end.
#Fun<erl_eval.6.39074546>
4> Fun2(7).
gt
5> Fun3 = fun Fact(1) -> 1; Fact(X) when X > 1 -> X * Fact(X - 1) end.
#Fun<erl_eval.6.39074546>
6> Fun3(4).
24
```

The following fun expressions are also allowed:

```
fun Name/Arity
fun Module:Name/Arity
```

In `Name/Arity`, `Name` is an atom and `Arity` is an integer. `Name/Arity` must
specify an existing local function. The expression is syntactic sugar for:

```
fun (Arg1,...,ArgN) -> Name(Arg1,...,ArgN) end
```

In `Module:Name/Arity`, `Module`, and `Name` are atoms and `Arity` is an
integer. `Module`, `Name`, and `Arity` can also be variables. A fun defined in
this way refers to the function `Name` with arity `Arity` in the _latest_
version of module `Module`. A fun defined in this way is not dependent on the
code for the module in which it is defined.

> #### Change {: .info }
>
> Before Erlang/OTP R15, `Module`, `Name`, and `Arity` were not allowed to be
> variables.

More examples are provided in [Programming Examples](`e:system:funs.md`).

## Catch and Throw

```
catch Expr
```

Returns the value of `Expr` unless an exception is raised during the evaluation. In
that case, the exception is caught. The return value depends on the class of the
exception:

- **`error`** (a run-time error or the code called [`error(Term)`](`error/1`)) -
  `{'EXIT',{Reason,Stack}}` is returned.

- **`exit`** (the code called [`exit(Term)`](`exit/1`)) - `{'EXIT',Term}` is returned.

- **`throw`** (the code called [`throw(Term)`](`throw/1`)): `Term` is returned.

`Reason` depends on the type of error that occurred, and `Stack` is the stack of
recent function calls, see [Exit Reasons](errors.md#exit_reasons).

_Examples:_

```
1> catch 1+2.
3
2> catch 1+a.
{'EXIT',{badarith,[...]}}
```

The BIF [`throw(Any)`](`throw/1`) can be used for non-local return from a
function. It must be evaluated within a `catch`, which returns the value `Any`.

_Example:_

```
3> catch throw(hello).
hello
```

If [`throw/1`](`throw/1`) is not evaluated within a catch, a `nocatch` run-time
error occurs.

> #### Change {: .info }
>
> Before Erlang/OTP 24, the `catch` operator had the lowest precedence, making
> it necessary to add parentheses when combining it with the `match` operator:
>
> ```
> 1> A = (catch 42).
> 42
> 2> A.
> 42
> ```
>
> Starting from Erlang/OTP 24, the parentheses can be omitted:
>
> ```
> 1> A = catch 42.
> 42
> 2> A.
> 42
> ```

## Try

```
try Exprs
catch
    Class1:ExceptionPattern1[:Stacktrace] [when ExceptionGuardSeq1] ->
        ExceptionBody1;
    ClassN:ExceptionPatternN[:Stacktrace] [when ExceptionGuardSeqN] ->
        ExceptionBodyN
end
```

This is an enhancement of [catch](expressions.md#catch-and-throw). It gives the
possibility to:

- Distinguish between different exception classes.
- Choose to handle only the desired ones.
- Passing the others on to an enclosing `try` or `catch`, or to default error
  handling.

Notice that although the keyword `catch` is used in the `try` expression, there
is not a `catch` expression within the `try` expression.

It returns the value of `Exprs` (a sequence of expressions `Expr1, ..., ExprN`)
unless an exception occurs during the evaluation. In that case the exception is
caught and the patterns `ExceptionPattern` with the right exception class
`Class` are sequentially matched against the caught exception. If a match
succeeds and the optional guard sequence `ExceptionGuardSeq` is true, the
corresponding `ExceptionBody` is evaluated to become the return value.

`Stacktrace`, if specified, must be the name of a variable (not a pattern). The
stack trace is bound to the variable when the corresponding `ExceptionPattern`
matches.

If an exception occurs during evaluation of `Exprs` but there is no matching
`ExceptionPattern` of the right `Class` with a true guard sequence, the
exception is passed on as if `Exprs` had not been enclosed in a `try`
expression.

If an exception occurs during evaluation of `ExceptionBody`, it is not caught.

It is allowed to omit `Class` and `Stacktrace`. An omitted `Class` is shorthand
for `throw`:

```
try Exprs
catch
    ExceptionPattern1 [when ExceptionGuardSeq1] ->
        ExceptionBody1;
    ExceptionPatternN [when ExceptionGuardSeqN] ->
        ExceptionBodyN
end
```

The `try` expression can have an `of` section:

```
try Exprs of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
catch
    Class1:ExceptionPattern1[:Stacktrace] [when ExceptionGuardSeq1] ->
        ExceptionBody1;
    ...;
    ClassN:ExceptionPatternN[:Stacktrace] [when ExceptionGuardSeqN] ->
        ExceptionBodyN
end
```

If the evaluation of `Exprs` succeeds without an exception, the patterns
`Pattern` are sequentially matched against the result in the same way as for a
[case](expressions.md#case) expression, except that if the matching fails, a
`try_clause` run-time error occurs instead of a `case_clause`.

Only exceptions occurring during the evaluation of `Exprs` can be caught by the
`catch` section. Exceptions occurring in a `Body` or due to a failed match are
not caught.

The `try` expression can also be augmented with an `after` section, intended to
be used for cleanup with side effects:

```
try Exprs of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
catch
    Class1:ExceptionPattern1[:Stacktrace] [when ExceptionGuardSeq1] ->
        ExceptionBody1;
    ...;
    ClassN:ExceptionPatternN[:Stacktrace] [when ExceptionGuardSeqN] ->
        ExceptionBodyN
after
    AfterBody
end
```

`AfterBody` is evaluated after either `Body` or `ExceptionBody`, no matter which
one. The evaluated value of `AfterBody` is lost; the return value of the `try`
expression is the same with an `after` section as without.

Even if an exception occurs during evaluation of `Body` or `ExceptionBody`,
`AfterBody` is evaluated. In this case the exception is passed on after
`AfterBody` has been evaluated, so the exception from the `try` expression is
the same with an `after` section as without.

If an exception occurs during evaluation of `AfterBody` itself, it is not
caught. So if `AfterBody` is evaluated after an exception in `Exprs`, `Body`, or
`ExceptionBody`, that exception is lost and masked by the exception in
`AfterBody`.

The `of`, `catch`, and `after` sections are all optional, as long as there is at
least a `catch` or an `after` section. So the following are valid `try`
expressions:

```
try Exprs of
    Pattern when GuardSeq ->
        Body
after
    AfterBody
end

try Exprs
catch
    ExpressionPattern ->
        ExpressionBody
after
    AfterBody
end

try Exprs after AfterBody end
```

Next is an example of using `after`. This closes the file, even in the event of
exceptions in `file:read/2` or in [`binary_to_term/1`](`binary_to_term/1`). The
exceptions are the same as without the `try`...`after`...`end` expression:

```
termize_file(Name) ->
    {ok,F} = file:open(Name, [read,binary]),
    try
        {ok,Bin} = file:read(F, 1024*1024),
        binary_to_term(Bin)
    after
        file:close(F)
    end.
```

Next is an example of using `try` to emulate `catch Expr`:

```
try Expr
catch
    throw:Term -> Term;
    exit:Reason -> {'EXIT',Reason};
    error:Reason:Stk -> {'EXIT',{Reason,Stk}}
end
```

Variables bound in the various parts of these expressions have different scopes.
Variables bound just after the `try` keyword are:

- bound in the `of` section
- unsafe in both the `catch` and `after` sections, as well as after the whole
  construct

Variables bound in `of` section are:

- unbound in the `catch` section
- unsafe in both the `after` section, as well as after the whole construct

Variables bound in the `catch` section are unsafe in the `after` section, as
well as after the whole construct.

Variables bound in the `after` section are unsafe after the whole construct.

## Parenthesized Expressions

```
(Expr)
```

Parenthesized expressions are useful to override
[operator precedences](expressions.md#prec), for example, in arithmetic
expressions:

```
1> 1 + 2 * 3.
7
2> (1 + 2) * 3.
9
```

## Block Expressions

```
begin
   Expr1,
   ...,
   ExprN
end
```

Block expressions provide a way to group a sequence of expressions, similar to a
clause body. The return value is the value of the last expression `ExprN`.

[](){: #lcs }

## Comprehensions

Comprehensions provide a succinct notation for iterating over one or more terms
and constructing a new term. Comprehensions come in three different flavors,
depending on the type of term they build.

List comprehensions construct lists. They have the following syntax:

```
[Expr || Qualifier1, . . ., QualifierN]
```

Here, `Expr` is an arbitrary expression, and each `Qualifier` is either a
**generator** or a **filter**.

Bit string comprehensions construct bit strings or binaries. They have the
following syntax:

```
<< BitStringExpr || Qualifier1, . . ., QualifierN >>
```

`BitStringExpr` is an expression that evaluates to a bit string. If
`BitStringExpr` is a function call, it must be enclosed in parentheses. Each
`Qualifier` is either a **generator** or a **filter**.

Map comprehensions construct maps. They have the following syntax:

```
#{KeyExpr => ValueExpr || Qualifier1, . . ., QualifierN}
```

Here, `KeyExpr` and `ValueExpr` are arbitrary expressions, and each `Qualifier`
is either a **generator** or a **filter**.

> #### Change {: .info }
>
> Map comprehensions and map generators were introduced in Erlang/OTP 26.

There are four kinds of generators. Three of them have a relaxed and a strict
variant. The fourth kind of generator, zip generator, is composed by two or
more non-zip generators.

> #### Change {: .info }
>
> Strict generators and zip generators were introduced in Erlang/OTP 28.
> Using strict generators is a better practice when either strict or relaxed
> generators work. More details are in
> [Programming Examples.](`e:system:list_comprehensions.md`)


A _list generator_ has the following syntax for relaxed:

```
Pattern <- ListExpr
```

and strict variant:

```
Pattern <:- ListExpr
```

where `ListExpr` is an expression that evaluates to a list of terms.

A _bit string generator_ has the following syntax for relaxed:

```
BitstringPattern <= BitStringExpr
```

and strict variant:

```
BitstringPattern <:= BitStringExpr
```

where `BitStringExpr` is an expression that evaluates to a bit string.

A _map generator_ has the following syntax for relaxed:

```
KeyPattern := ValuePattern <- MapExpression
```

and strict variant:

```
KeyPattern := ValuePattern <:- MapExpression
```

where `MapExpr` is an expression that evaluates to a map, or a map iterator
obtained by calling `maps:iterator/1` or `maps:iterator/2`.

A _zip generator_ has the following syntax:

```
Generator_1 && ... && Generator_n
```

where every `Generator_i` is a non-zip generator. Generators within a zip
generator are treated as one generator and evaluated in parallel.

A _filter_ is an expression that evaluates to `true` or `false`.

The variables in the generator patterns shadow previously bound variables,
including variables bound in a previous generator pattern.

Variables bound in a generator expression are not visible outside the
expression:

```
1> [{E,L} || E <- L=[1,2,3]].
* 1:5: variable 'L' is unbound
```

A **list comprehension** returns a list, where the list elements are the result
of evaluating `Expr` for each combination of generator elements for which all
filters are true.

A **bit string comprehension** returns a bit string, which is created by
concatenating the results of evaluating `BitStringExpr` for each combination of
bit string generator elements for which all filters are true.

A **map comprehension** returns a map, where the map elements are the result of
evaluating `KeyExpr` and `ValueExpr` for each combination of generator elements
for which all filters are true. If the key expressions are not unique, the last
occurrence is stored in the map.

**Examples:**

Multiplying each element in a list by two:

```
1> [X*2 || X <:- [1,2,3]].
[2,4,6]
```

Multiplying each byte in a binary by two, returning a list:

```
1> [X*2 || <<X>> <:= <<1,2,3>>].
[2,4,6]
```

Multiplying each byte in a binary by two:

```
1> << <<(X*2)>> || <<X>> <:= <<1,2,3>> >>.
<<2,4,6>>
```

Multiplying each element in a list by two, returning a binary:

```
1> << <<(X*2)>> || X <:- [1,2,3] >>.
<<2,4,6>>
```

Creating a mapping from an integer to its square:

```
1> #{X => X*X || X <:- [1,2,3]}.
#{1 => 1,2 => 4,3 => 9}
```

Multiplying the value of each element in a map by two:

```
1> #{K => 2*V || K := V <:- #{a => 1,b => 2,c => 3}}.
#{a => 2,b => 4,c => 6}
```

Filtering a list, keeping odd numbers:

```
1> [X || X <:- [1,2,3,4,5], X rem 2 =:= 1].
[1,3,5]
```

Filtering a list, keeping only elements that match:

```
1> [X || {_,_}=X <- [{a,b}, [a], {x,y,z}, {1,2}]].
[{a,b},{1,2}]
```

Filtering a list, crashing when the element is not a 2-tuple:

```
1> [X || {_,_}=X <:- [{a,b}, [a], {x,y,z}, {1,2}]].
** exception error: no match of right hand side value [a]
```

Combining elements from two list generators:

```
1> [{P,Q} || P <:- [a,b,c], Q <:- [1,2]].
[{a,1},{a,2},{b,1},{b,2},{c,1},{c,2}]
```

Combining elements from two list generators, using a zip generator:

```
1> [{P,Q} || P <:- [a,b,c] && Q <:- [1,2,3]].
[{a,1},{b,2},{c,3}]
```

Combining elements from two list generators using a zip generator, filtering
out odd numbers:

```
1> [{P,Q} || P <:- [a,b,c] && Q <:- [1,2,3], Q rem 2 =:= 0].
[{a,1},{b,2},{c,3}]
```

Filtering out non-matching elements from two lists.

```
1> [X || X <- [1,2,3,5] && X <- [1,4,3,6]].
[1,3]
```

More examples are provided in
[Programming Examples.](`e:system:list_comprehensions.md`)

When there are no generators, a comprehension returns either a term constructed
from a single element (the result of evaluating `Expr`) if all filters are true,
or a term constructed from no elements (that is, `[]` for list comprehension,
`<<>>` for a bit string comprehension, and `#{}` for a map comprehension).

_Example:_

```
1> [2 || is_integer(2)].
[2]
2> [x || is_integer(x)].
[]
```

What happens when the filter expression does not evaluate to a boolean value
depends on the expression:

- If the expression is a [guard expression](expressions.md#guard-expressions),
  failure to evaluate or evaluating to a non-boolean value is equivalent to
  evaluating to `false`.
- If the expression is not a guard expression and evaluates to a non-Boolean
  value `Val`, an exception `{bad_filter, Val}` is triggered at runtime. If the
  evaluation of the expression raises an exception, it is not caught by the
  comprehension.

**Examples** (using a guard expression as filter):

```
1> List = [1,2,a,b,c,3,4].
[1,2,a,b,c,3,4]
2> [E || E <:- List, E rem 2].
[]
3> [E || E <:- List, E rem 2 =:= 0].
[2,4]
```

**Examples** (using a non-guard expression as filter):

```
1> List = [1,2,a,b,c,3,4].
[1,2,a,b,c,3,4]
2> FaultyIsEven = fun(E) -> E rem 2 end.
#Fun<erl_eval.42.17316486>
3> [E || E <:- List, FaultyIsEven(E)].
** exception error: bad filter 1
4> IsEven = fun(E) -> E rem 2 =:= 0 end.
#Fun<erl_eval.42.17316486>
5> [E || E <:- List, IsEven(E)].
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  rem/2
        called as a rem 2
6> [E || E <:- List, is_integer(E), IsEven(E)].
[2,4]
```

[](){: #guards }

## Guard Sequences

A _guard sequence_ is a sequence of guards, separated by semicolon (`;`). The
guard sequence is true if at least one of the guards is true. (The remaining
guards, if any, are not evaluated.)

`Guard1; ...; GuardK`

A _guard_ is a sequence of guard expressions, separated by comma (`,`). The guard
is true if all guard expressions evaluate to `true`.

`GuardExpr1, ..., GuardExprN`

## Guard Expressions

The set of valid _guard expressions_ is a subset of the set of valid Erlang
expressions. The reason for restricting the set of valid expressions is that
evaluation of a guard expression must be guaranteed to be free of side effects.
Valid guard expressions are the following:

- Variables
- Constants (atoms, integer, floats, lists, tuples, records, binaries, and maps)
- Expressions that construct atoms, integer, floats, lists, tuples, records,
  binaries, and maps
- Expressions that update a map
- The record expressions `Expr#Name.Field` and `#Name.Field`
- Calls to the BIFs specified in tables _Type Test BIFs_ and _Other BIFs Allowed
  in Guard Expressions_
- Term comparisons
- Arithmetic expressions
- Boolean expressions
- Short-circuit expressions (`andalso`/`orelse`)

| BIF                                  |
| ------------------------------------ |
| [`is_atom/1`](`is_atom/1`)           |
| [`is_binary/1`](`is_binary/1`)       |
| [`is_bitstring/1`](`is_bitstring/1`) |
| [`is_boolean/1`](`is_boolean/1`)     |
| [`is_float/1`](`is_float/1`)         |
| [`is_function/1`](`is_function/1`)   |
| [`is_function/2`](`is_function/2`)   |
| [`is_integer/1`](`is_integer/1`)     |
| [`is_list/1`](`is_list/1`)           |
| [`is_map/1`](`is_map/1`)             |
| [`is_number/1`](`is_number/1`)       |
| [`is_pid/1`](`is_pid/1`)             |
| [`is_port/1`](`is_port/1`)           |
| [`is_record/2`](`is_record/2`)       |
| [`is_record/3`](`is_record/3`)       |
| [`is_reference/1`](`is_reference/1`) |
| [`is_tuple/1`](`is_tuple/1`)         |

_Table: Type Test BIFs_

Notice that most type test BIFs have older equivalents, without the
`is_` prefix. These old BIFs are retained only for backwards
compatibility and are not to be used in new code. They are also only
allowed at top level. For example, they are not allowed in Boolean
expressions in guards.

| BIF                                      |
| ---------------------------------------- |
| [`abs(Number)`](`abs/1`)                 |
| [`bit_size(Bitstring)`](`bit_size/1`)    |
| [`byte_size(Bitstring)`](`byte_size/1`)  |
| [`element(N, Tuple)`](`element/2`)       |
| [`float(Term)`](`float/1`)               |
| [`hd(List)`](`hd/1`)                     |
| [`is_map_key(Key, Map)`](`is_map_key/2`) |
| [`length(List)`](`length/1`)             |
| [`map_get(Key, Map)`](`map_get/2`)       |
| [`map_size(Map)`](`map_size/1`)          |
| [`max(A, B)`](`max/2`)                   |
| [`min(A, B)`](`min/2`)                   |
| `node/0`                                 |
| `node(Pid` \| `Ref` \| `Port)`           |
| [`round(Number)`](`round/1`)             |
| `self/0`                                 |
| `size(Tuple` \| `Bitstring)`             |
| [`tl(List)`](`tl/1`)                     |
| [`trunc(Number)`](`trunc/1`)             |
| [`tuple_size(Tuple)`](`tuple_size/1`)    |

_Table: Other BIFs Allowed in Guard Expressions_

> #### Change {: .info }
>
> The [`min/2`](`min/2`) and [`max/2`](`max/2`) BIFs are allowed to be used in
> guards from Erlang/OTP 26.

If an arithmetic expression, a Boolean expression, a short-circuit expression,
or a call to a guard BIF fails (because of invalid arguments), the entire guard
fails. If the guard was part of a guard sequence, the next guard in the sequence
(that is, the guard following the next semicolon) is evaluated.

[](){: #prec }

## Operator Precedence

Operator precedence in descending order:

| Operator                                    | Association       |
| ------------------------------------------- | ----------------- |
| `#`                                         |                   |
| Unary `+` `-` `bnot` `not`                  |                   |
| `/` `*` `div` `rem` `band` `and`            | Left-associative  |
| `+` `-` `bor` `bxor` `bsl` `bsr` `or` `xor` | Left-associative  |
| `++` `--`                                   | Right-associative |
| `==` `/=` `=<` `<` `>=` `>` `=:=` `=/=`     | Non-associative   |
| `andalso`                                   | Left-associative  |
| `orelse`                                    | Left-associative  |
| `catch`                                     |                   |
| `=` `!`                                     | Right-associative |
| `?=`                                        | Non-associative   |

_Table: Operator Precedence_

> #### Change {: .info }
>
> Before Erlang/OTP 24, the `catch` operator had the lowest precedence.

> #### Note {: .info }
>
> The `=` operator in the table is the
> [match operator](expressions.md#the-match-operator). The character `=` can also
> denote the
> [compound pattern operator](expressions.md#the-compound-pattern-operator), which
> can only be used in patterns.
>
> `?=` is restricted in that it can only be used at the top-level inside a
> `maybe` block.

When evaluating an expression, the operator with the highest precedence is
evaluated first. Operators with the same precedence are evaluated according to
their associativity. Non-associative operators cannot be combined with operators
of the same precedence.

_Examples:_

The left-associative arithmetic operators are evaluated left to right:

```
6 + 5 * 4 - 3 / 2 evaluates to
6 + 20 - 1.5 evaluates to
26 - 1.5 evaluates to
24.5
```

The non-associative operators cannot be combined:

```
1> 1 < X < 10.
* 1:7: syntax error before: '<'
```
