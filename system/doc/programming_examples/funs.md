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
# Funs

## map

The following function, `double`, doubles every element in a list:

```erlang
double([H|T]) -> [2*H|double(T)];
double([])    -> [].
```

Hence, the argument entered as input is doubled as follows:

```erlang
> double([1,2,3,4]).
[2,4,6,8]
```

The following function, `add_one`, adds one to every element in a list:

```erlang
add_one([H|T]) -> [H+1|add_one(T)];
add_one([])    -> [].
```

The functions `double` and `add_one` have a similar structure. This can be used
by writing a function `map` that expresses this similarity:

```erlang
map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].
```

The functions `double` and `add_one` can now be expressed in terms of `map` as
follows:

```erlang
double(L)  -> map(fun(X) -> 2*X end, L).
add_one(L) -> map(fun(X) -> 1 + X end, L).
```

`map(F, List)` is a function that takes a function `F` and a list `L` as
arguments and returns a new list, obtained by applying `F` to each of the
elements in `L`.

The process of abstracting out the common features of a number of different
programs is called _procedural abstraction_. Procedural abstraction can be used
to write several different functions that have a similar structure, but differ
in some minor detail. This is done as follows:

1. _Step 1._ Write one function that represents the common features of these
   functions.
1. _Step 2._ Parameterize the difference in terms of functions that are passed
   as arguments to the common function.

## foreach

This section illustrates procedural abstraction. Initially, the following two
examples are written as conventional functions.

This function prints all elements of a list onto a stream:

```erlang
print_list(Stream, [H|T]) ->
    io:format(Stream, "~p~n", [H]),
    print_list(Stream, T);
print_list(Stream, []) ->
    true.
```

This function broadcasts a message to a list of processes:

```erlang
broadcast(Msg, [Pid|Pids]) ->
    Pid ! Msg,
    broadcast(Msg, Pids);
broadcast(_, []) ->
    true.
```

These two functions have a similar structure. They both iterate over a list and
do something to each element in the list. The "something" is passed on as an
extra argument to the function that does this.

The function `foreach` expresses this similarity:

```erlang
foreach(F, [H|T]) ->
    F(H),
    foreach(F, T);
foreach(F, []) ->
    ok.
```

Using the function `foreach`, the function `print_list` becomes:

```erlang
foreach(fun(H) -> io:format(S, "~p~n",[H]) end, L)
```

Using the function `foreach`, the function `broadcast` becomes:

```erlang
foreach(fun(Pid) -> Pid ! M end, L)
```

`foreach` is evaluated for its side-effect and not its value. `foreach(Fun ,L)`
calls `Fun(X)` for each element `X` in `L` and the processing occurs in the
order that the elements were defined in `L`. `map` does not define the order in
which its elements are processed.

## Syntax of Funs

Funs are written with the following syntax (see
[Fun Expressions ](`e:system:expressions.md#fun-expressions`)for full description):

```erlang
F = fun (Arg1, Arg2, ... ArgN) ->
        ...
    end
```

This creates an anonymous function of `N` arguments and binds it to the variable
`F`.

Another function, `FunctionName`, written in the same module, can be passed as
an argument, using the following syntax:

```erlang
F = fun FunctionName/Arity
```

With this form of function reference, the function that is referred to does not
need to be exported from the module.

It is also possible to refer to a function defined in a different module, with
the following syntax:

```erlang
F = fun Module:FunctionName/Arity
```

In this case, the function must be exported from the module in question.

The following program illustrates the different ways of creating funs:

```erlang
-module(fun_test).
-export([t1/0, t2/0]).
-import(lists, [map/2]).

t1() -> map(fun(X) -> 2 * X end, [1,2,3,4,5]).

t2() -> map(fun double/1, [1,2,3,4,5]).

double(X) -> X * 2.
```

The fun `F` can be evaluated with the following syntax:

```erlang
F(Arg1, Arg2, ..., Argn)
```

To check whether a term is a fun, use the test
[`is_function/1`](`is_function/1`) in a guard.

_Example:_

```erlang
f(F, Args) when is_function(F) ->
   apply(F, Args);
f(N, _) when is_integer(N) ->
   N.
```

Funs are a distinct type. The BIFs `erlang:fun_info/1,2` can be used to retrieve
information about a fun, and the BIF `erlang:fun_to_list/1` returns a textual
representation of a fun. The [`check_process_code/2`](`check_process_code/2`)
BIF returns `true` if the process contains funs that depend on the old version
of a module.

## Variable Bindings Within a Fun

The scope rules for variables that occur in funs are as follows:

- All variables that occur in the head of a fun are assumed to be "fresh"
  variables.
- Variables that are defined before the fun, and that occur in function calls or
  guard tests within the fun, have the values they had outside the fun.
- Variables cannot be exported from a fun.

The following examples illustrate these rules:

```erlang
print_list(File, List) ->
    {ok, Stream} = file:open(File, write),
    foreach(fun(X) -> io:format(Stream,"~p~n",[X]) end, List),
    file:close(Stream).
```

Here, the variable `X`, defined in the head of the fun, is a new variable. The
variable `Stream`, which is used within the fun, gets its value from the
`file:open` line.

As any variable that occurs in the head of a fun is considered a new variable,
it is equally valid to write as follows:

```erlang
print_list(File, List) ->
    {ok, Stream} = file:open(File, write),
    foreach(fun(File) ->
                io:format(Stream,"~p~n",[File])
            end, List),
    file:close(Stream).
```

Here, `File` is used as the new variable instead of `X`. This is not so wise
because code in the fun body cannot refer to the variable `File`, which is
defined outside of the fun. Compiling this example gives the following
diagnostic:

```text
./FileName.erl:Line: Warning: variable 'File'
      shadowed in 'fun'
```

This indicates that the variable `File`, which is defined inside the fun,
collides with the variable `File`, which is defined outside the fun.

The rules for importing variables into a fun has the consequence that certain
pattern matching operations must be moved into guard expressions and cannot be
written in the head of the fun. For example, you might write the following code
if you intend the first clause of `F` to be evaluated when the value of its
argument is `Y`:

```erlang
f(...) ->
    Y = ...
    map(fun(X) when X == Y ->
             ;
           (_) ->
             ...
        end, ...)
    ...
```

instead of writing the following code:

```erlang
f(...) ->
    Y = ...
    map(fun(Y) ->
             ;
           (_) ->
             ...
        end, ...)
    ...
```

## Funs and Module Lists

The following examples show a dialogue with the Erlang shell. All the higher
order functions discussed are exported from the module `m:lists`.

### map

`lists:map/2` takes a function of one argument and a list of terms:

```erlang
map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].
```

It returns the list obtained by applying the function to every argument in the
list.

When a new fun is defined in the shell, the value of the fun is printed as
`Fun#<erl_eval>`:

```erlang
> Double = fun(X) -> 2 * X end.
#Fun<erl_eval.6.72228031>
> lists:map(Double, [1,2,3,4,5]).
[2,4,6,8,10]
```

### any

`lists:any/2` takes a predicate `P` of one argument and a list of terms:

```erlang
any(Pred, [H|T]) ->
    case Pred(H) of
        true  ->  true;
        false ->  any(Pred, T)
    end;
any(Pred, []) ->
    false.
```

A predicate is a function that returns `true` or `false`. `any` is `true` if
there is a term `X` in the list such that `P(X)` is `true`.

A predicate `Big(X)` is defined, which is `true` if its argument is greater that
10:

```erlang
> Big =  fun(X) -> if X > 10 -> true; true -> false end end.
#Fun<erl_eval.6.72228031>
> lists:any(Big, [1,2,3,4]).
false
> lists:any(Big, [1,2,3,12,5]).
true
```

### all

`lists:all/2` has the same arguments as `any`:

```erlang
all(Pred, [H|T]) ->
    case Pred(H) of
        true  ->  all(Pred, T);
        false ->  false
    end;
all(Pred, []) ->
    true.
```

It is `true` if the predicate applied to all elements in the list is `true`.

```erlang
> lists:all(Big, [1,2,3,4,12,6]).
false
> lists:all(Big, [12,13,14,15]).
true
```

### foreach

`lists:foreach/2` takes a function of one argument and a list of terms:

```erlang
foreach(F, [H|T]) ->
    F(H),
    foreach(F, T);
foreach(F, []) ->
    ok.
```

The function is applied to each argument in the list. `foreach` returns `ok`. It
is only used for its side-effect:

```erlang
> lists:foreach(fun(X) -> io:format("~w~n",[X]) end, [1,2,3,4]).
1
2
3
4
ok
```

### foldl

`lists:foldl/3` takes a function of two arguments, an accumulator and a list:

```erlang
foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) -> Accu.
```

The function is called with two arguments. The first argument is the successive
elements in the list. The second argument is the accumulator. The function must
return a new accumulator, which is used the next time the function is called.

If you have a list of lists `L = ["I","like","Erlang"]`, then you can sum the
lengths of all the strings in `L` as follows:

```erlang
> L = ["I","like","Erlang"].
["I","like","Erlang"]
10> lists:foldl(fun(X, Sum) -> length(X) + Sum end, 0, L).
11
```

`lists:foldl/3` works like a `while` loop in an imperative language:

```erlang
L =  ["I","like","Erlang"],
Sum = 0,
while( L != []){
    Sum += length(head(L)),
    L = tail(L)
end
```

### mapfoldl

`lists:mapfoldl/3` simultaneously maps and folds over a list:

```erlang
mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs], Accu2};
mapfoldl(F, Accu, []) -> {[], Accu}.
```

The following example shows how to change all letters in `L` to upper case and
then count them.

First the change to upper case:

```erlang
> Upcase =  fun(X) when $a =< X,  X =< $z -> X + $A - $a;
(X) -> X
end.
#Fun<erl_eval.6.72228031>
> Upcase_word =
fun(X) ->
lists:map(Upcase, X)
end.
#Fun<erl_eval.6.72228031>
> Upcase_word("Erlang").
"ERLANG"
> lists:map(Upcase_word, L).
["I","LIKE","ERLANG"]
```

Now, the fold and the map can be done at the same time:

```erlang
> lists:mapfoldl(fun(Word, Sum) ->
{Upcase_word(Word), Sum + length(Word)}
end, 0, L).
{["I","LIKE","ERLANG"],11}
```

### filter

`lists:filter/2` takes a predicate of one argument and a list and returns all elements
in the list that satisfy the predicate:

```erlang
filter(F, [H|T]) ->
    case F(H) of
        true  -> [H|filter(F, T)];
        false -> filter(F, T)
    end;
filter(F, []) -> [].
```

```erlang
> lists:filter(Big, [500,12,2,45,6,7]).
[500,12,45]
```

Combining maps and filters enables writing of very succinct code. For example,
to define a set difference function `diff(L1, L2)` to be the difference between
the lists `L1` and `L2`, the code can be written as follows:

```erlang
diff(L1, L2) ->
    filter(fun(X) -> not member(X, L2) end, L1).
```

This gives the list of all elements in L1 that are not contained in L2.

The AND intersection of the list `L1` and `L2` is also easily defined:

```erlang
intersection(L1,L2) -> filter(fun(X) -> member(X,L1) end, L2).
```

### takewhile

`lists:takewhile/2` takes elements `X` from a list `L` as long as the predicate
`P(X)` is true:

```erlang
takewhile(Pred, [H|T]) ->
    case Pred(H) of
        true  -> [H|takewhile(Pred, T)];
        false -> []
    end;
takewhile(Pred, []) ->
    [].
```

```erlang
> lists:takewhile(Big, [200,500,45,5,3,45,6]).
[200,500,45]
```

### dropwhile

`lists:dropwhile/2` is the complement of `takewhile`:

```erlang
dropwhile(Pred, [H|T]) ->
    case Pred(H) of
        true  -> dropwhile(Pred, T);
        false -> [H|T]
    end;
dropwhile(Pred, []) ->
    [].
```

```erlang
> lists:dropwhile(Big, [200,500,45,5,3,45,6]).
[5,3,45,6]
```

### splitwith

`lists:splitwith/2` splits the list `L` into the two sublists `{L1, L2}`, where
`L = takewhile(P, L)` and `L2 = dropwhile(P, L)`:

```erlang
splitwith(Pred, L) ->
    splitwith(Pred, L, []).

splitwith(Pred, [H|T], L) ->
    case Pred(H) of
        true  -> splitwith(Pred, T, [H|L]);
        false -> {reverse(L), [H|T]}
    end;
splitwith(Pred, [], L) ->
    {reverse(L), []}.
```

```erlang
> lists:splitwith(Big, [200,500,45,5,3,45,6]).
{[200,500,45],[5,3,45,6]}
```

## Funs Returning Funs

So far, only functions that take funs as arguments have been described. More
powerful functions, that themselves return funs, can also be written. The
following examples illustrate these type of functions.

### Simple Higher Order Functions

`Adder(X)` is a function that given `X`, returns a new function `G` such that
`G(K)` returns `K + X`:

```erlang
> Adder = fun(X) -> fun(Y) -> X + Y end end.
#Fun<erl_eval.6.72228031>
> Add6 = Adder(6).
#Fun<erl_eval.6.72228031>
> Add6(10).
16
```

### Infinite Lists

The idea is to write something like:

```erlang
-module(lazy).
-export([ints_from/1]).
ints_from(N) ->
    fun() ->
            [N|ints_from(N+1)]
    end.
```

Then proceed as follows:

```erlang
> XX = lazy:ints_from(1).
#Fun<lazy.0.29874839>
> XX().
[1|#Fun<lazy.0.29874839>]
> hd(XX()).
1
> Y = tl(XX()).
#Fun<lazy.0.29874839>
> hd(Y()).
2
```

And so on. This is an example of "lazy embedding".

### Parsing

The following examples show parsers of the following type:

```erlang
Parser(Toks) -> {ok, Tree, Toks1} | fail
```

`Toks` is the list of tokens to be parsed. A successful parse returns
`{ok, Tree, Toks1}`.

- `Tree` is a parse tree.
- `Toks1` is a tail of `Tree` that contains symbols encountered after the
  structure that was correctly parsed.

An unsuccessful parse returns `fail`.

The following example illustrates a simple, functional parser that parses the
grammar:

```text
(a | b) & (c | d)
```

The following code defines a function `pconst(X)` in the module `funparse`,
which returns a fun that parses a list of tokens:

```erlang
pconst(X) ->
    fun (T) ->
       case T of
           [X|T1] -> {ok, {const, X}, T1};
           _      -> fail
       end
    end.
```

This function can be used as follows:

```erlang
> P1 = funparse:pconst(a).
#Fun<funparse.0.22674075>
> P1([a,b,c]).
{ok,{const,a},[b,c]}
> P1([x,y,z]).
fail
```

Next, the two higher order functions `pand` and `por` are defined. They combine
primitive parsers to produce more complex parsers.

First `pand`:

```erlang
pand(P1, P2) ->
    fun (T) ->
        case P1(T) of
            {ok, R1, T1} ->
                case P2(T1) of
                    {ok, R2, T2} ->
                        {ok, {'and', R1, R2}};
                    fail ->
                        fail
                end;
            fail ->
                fail
        end
    end.
```

Given a parser `P1` for grammar `G1`, and a parser `P2` for grammar `G2`,
`pand(P1, P2)` returns a parser for the grammar, which consists of sequences of
tokens that satisfy `G1`, followed by sequences of tokens that satisfy `G2`.

`por(P1, P2)` returns a parser for the language described by the grammar `G1` or
`G2`:

```erlang
por(P1, P2) ->
    fun (T) ->
        case P1(T) of
            {ok, R, T1} ->
                {ok, {'or',1,R}, T1};
            fail ->
                case P2(T) of
                    {ok, R1, T1} ->
                        {ok, {'or',2,R1}, T1};
                    fail ->
                        fail
                end
        end
    end.
```

The original problem was to parse the grammar `(a | b) & (c | d)`. The following
code addresses this problem:

```erlang
grammar() ->
    pand(
         por(pconst(a), pconst(b)),
         por(pconst(c), pconst(d))).
```

The following code adds a parser interface to the grammar:

```erlang
parse(List) ->
    (grammar())(List).
```

The parser can be tested as follows:

```erlang
> funparse:parse([a,c]).
{ok,{'and',{'or',1,{const,a}},{'or',1,{const,c}}}}
> funparse:parse([a,d]).
{ok,{'and',{'or',1,{const,a}},{'or',2,{const,d}}}}
> funparse:parse([b,c]).
{ok,{'and',{'or',2,{const,b}},{'or',1,{const,c}}}}
> funparse:parse([b,d]).
{ok,{'and',{'or',2,{const,b}},{'or',2,{const,d}}}}
> funparse:parse([a,b]).
fail
```
