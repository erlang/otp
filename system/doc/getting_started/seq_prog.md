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
# Sequential Programming

## The Erlang Shell

Most operating systems have a command interpreter or shell, UNIX and Linux have
many, Windows has the command prompt, powershell and more. Erlang has its own shell
where bits of Erlang code can be written directly, and be evaluated to see what happens (see
the `m:shell` manual page in STDLIB).

Start the Erlang shell (in Linux or UNIX) by starting a shell or command
interpreter in your operating system and typing `erl`. You will see something
like this.

```text
$ erl
Erlang R15B (erts-5.9.1) [source] [smp:8:8] [rq:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
1>
```

Type `2 + 5.` in the shell and then press Enter (carriage return). Notice that
you tell the shell you are done entering code by finishing with a full stop `.`
and a carriage return.

```erlang
1> 2 + 5.
7
2> 
```

As shown, the Erlang shell numbers the lines that can be entered, (as 1> 2>) and
that it correctly says that 2 + 5 is 7. If you make writing mistakes in the
shell, you can delete with the backspace key, as in most shells. There are many
more editing commands in the shell (see
[tty - A command line interface](`e:erts:tty.md`) in ERTS User's Guide).

(Notice that many line numbers given by the shell in the following examples are
out of sequence. This is because this tutorial was written and code-tested in
separate sessions).

Here is a bit more complex calculation:

```erlang
2> (42 + 77) * 66 / 3.
2618.0
```

Notice the use of brackets, the multiplication operator `*`, and the division
operator `/`, as in normal arithmetic (see
[Expressions](`e:system:expressions.md`)).

Press Control-C to shut down the Erlang system and the Erlang shell.

The following output is shown:

```text
BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a
$
```

Type `a` to leave the Erlang system.

Another way to shut down the Erlang system is by entering `halt/0`:

```erlang
3> halt().
$
```

## Modules and Functions

A programming language is not much use if you only can run code from the shell.
So here is a small Erlang program. Enter it into a file named `tut.erl` using a
suitable text editor. The file name `tut.erl` is important, and also that it is
in the same directory as the one where you started `erl`). If you are lucky your
editor has an Erlang mode that makes it easier for you to enter and format your
code nicely (see [The Erlang mode for Emacs](`e:tools:erlang_mode_chapter.md`)
in Tools User's Guide), but you can manage perfectly well without. Here is the
code to enter:

```erlang
-module(tut).
-export([double/1]).

double(X) ->
    2 * X.
```

It is not hard to guess that this program doubles the value of numbers. The
first two lines of the code are described later. Let us compile the program.
This can be done in an Erlang shell as follows, where `c` means compile:

```erlang
3> c(tut).
{ok,tut}
```

The `{ok,tut}` means that the compilation is OK. If it says `error` it means
that there is some mistake in the text that you entered. Additional error
messages gives an idea to what is wrong so you can modify the text and then try
to compile the program again.

Now run the program:

```erlang
4> tut:double(10).
20
```

As expected, double of 10 is 20.

Now let us get back to the first two lines of the code. Erlang programs are
written in files. Each file contains an Erlang _module_. The first line of code
in the module is the module name (see [Modules](`e:system:modules.md`)):

```erlang
-module(tut).
```

Thus, the module is called _tut_. Notice the full stop `.` at the end of the
line. The files which are used to store the module must have the same name as
the module but with the extension `.erl`. In this case the file name is
`tut.erl`. When using a function in another module, the syntax
`module_name:function_name(arguments)` is used. So the following means call
function `double` in module `tut` with argument `10`.

```erlang
4> tut:double(10).
```

The second line says that the module `tut` contains a function called `double`,
which takes one argument (`X` in our example):

```erlang
-export([double/1]).
```

The second line also says that this function can be called from outside the
module `tut`. More about this later. Again, notice the `.` at the end of the
line.

Now for a more complicated example, the factorial of a number. For example, the
factorial of 4 is 4 _ 3 _ 2 * 1, which equals 24.

Enter the following code in a file named `tut1.erl`:

```erlang
-module(tut1).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).
```

So this is a module, called `tut1` that contains a function called `fac>`, which
takes one argument, `N`.

The first part says that the factorial of 1 is 1.:

```erlang
fac(1) ->
    1;
```

Notice that this part ends with a semicolon `;` that indicates that there is
more of the function `fac>` to come.

The second part says that the factorial of N is N multiplied by the factorial of
N - 1:

```erlang
fac(N) ->
    N * fac(N - 1).
```

Notice that this part ends with a `.` saying that there are no more parts of
this function.

Compile the file:

```erlang
5> c(tut1).
{ok,tut1}
```

And now calculate the factorial of 4.

```erlang
6> tut1:fac(4).
24
```

Here the function `fac>` in module `tut1` is called with argument `4`.

A function can have many arguments. Let us expand the module `tut1` with the
function to multiply two numbers:

```erlang
-module(tut1).
-export([fac/1, mult/2]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).

mult(X, Y) ->
    X * Y.
```

Notice that it is also required to expand the `-export` line with the
information that there is another function `mult` with two arguments.

Compile:

```erlang
7> c(tut1).
{ok,tut1}
```

Try out the new function `mult`:

```erlang
8> tut1:mult(3,4).
12
```

In this example the numbers are integers and the arguments in the functions in
the code `N`, `X`, and `Y` are called variables. Variables must start with a
capital letter (see [Variables](`e:system:expressions.md`)). Examples of
variables are `Number`, `ShoeSize`, and `Age`.

## Atoms

Atom is another data type in Erlang. Atoms start with a small letter (see
[Atom](`e:system:data_types.md`)), for example, `charles`, `centimeter`, and
`inch`. Atoms are simply names, nothing else. They are not like variables, which
can have a value.

Enter the next program in a file named `tut2.erl`). It can be useful for
converting from inches to centimeters and conversely:

```erlang
-module(tut2).
-export([convert/2]).

convert(M, inch) ->
    M / 2.54;

convert(N, centimeter) ->
    N * 2.54.
```

Compile:

```erlang
9> c(tut2).
{ok,tut2}
```

Test:

```erlang
10> tut2:convert(3, inch).
1.1811023622047243
11> tut2:convert(7, centimeter).
17.78
```

Notice the introduction of decimals (floating point numbers) without any
explanation. Hopefully you can cope with that.

Let us see what happens if something other than `centimeter` or `inch` is
entered in the `convert` function:

```erlang
12> tut2:convert(3, miles).
** exception error: no function clause matching tut2:convert(3,miles) (tut2.erl, line 4)
```

The two parts of the `convert` function are called its clauses. As shown,
`miles` is not part of either of the clauses. The Erlang system cannot _match_
either of the clauses so an error message `function_clause` is returned. The
shell formats the error message nicely, but the error tuple is saved in the
shell's history list and can be output by the shell command `v/1`:

```erlang
13> v(12).
{'EXIT',{function_clause,[{tut2,convert,
                                [3,miles],
                                [{file,"tut2.erl"},{line,4}]},
                          {erl_eval,do_apply,6,
                                    [{file,"erl_eval.erl"},{line,677}]},
                          {shell,exprs,7,[{file,"shell.erl"},{line,687}]},
                          {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
                          {shell,eval_loop,3,
                                 [{file,"shell.erl"},{line,627}]}]}}
```

## Tuples

Now the `tut2` program is hardly good programming style. Consider:

```erlang
tut2:convert(3, inch).
```

Does this mean that 3 is in inches? Or does it mean that 3 is in centimeters and
is to be converted to inches? Erlang has a way to group things together to make
things more understandable. These are called _tuples_ and are surrounded by
curly brackets, `{` and `}`.

So, `{inch,3}` denotes 3 inches and `{centimeter,5}` denotes 5 centimeters. Now
let us write a new program that converts centimeters to inches and conversely.
Enter the following code in a file called `tut3.erl`):

```erlang
-module(tut3).
-export([convert_length/1]).

convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
```

Compile and test:

```erlang
14> c(tut3).
{ok,tut3}
15> tut3:convert_length({inch, 5}).
{centimeter,12.7}
16> tut3:convert_length(tut3:convert_length({inch, 5})).
{inch,5.0}
```

Notice on line 16 that 5 inches is converted to centimeters and back again and
reassuringly get back to the original value. That is, the argument to a function
can be the result of another function. Consider how line 16 (above) works. The
argument given to the function `{inch,5}` is first matched against the first
head clause of `convert_length`, that is, `convert_length({centimeter,X})`. It
can be seen that `{centimeter,X}` does not match `{inch,5}` (the head is the bit
before the `->`). This having failed, let us try the head of the next clause
that is, `convert_length({inch,Y})`. This matches, and `Y` gets the value 5.

Tuples can have more than two parts, in fact as many parts as you want, and
contain any valid Erlang _term_. For example, to represent the temperature of
various cities of the world:

```erlang
{moscow, {c, -10}}
{cape_town, {f, 70}}
{paris, {f, 28}}
```

Tuples have a fixed number of items in them. Each item in a tuple is called an
_element_. In the tuple `{moscow,{c,-10}}`, element 1 is `moscow` and element 2
is `{c,-10}`. Here `c` represents Celsius and `f` Fahrenheit.

## Lists

Whereas tuples group things together, it is also needed to represent lists of
things. Lists in Erlang are surrounded by square brackets, `[` and `]`. For
example, a list of the temperatures of various cities in the world can be:

```erlang
[{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}},
 {paris, {f, 28}}, {london, {f, 36}}]
```

Notice that this list was so long that it did not fit on one line. This does not
matter, Erlang allows line breaks at all "sensible places" but not, for example,
in the middle of atoms, integers, and others.

A useful way of looking at parts of lists, is by using `|`. This is best
explained by an example using the shell:

```erlang
17> [First |TheRest] = [1,2,3,4,5].
[1,2,3,4,5]
18> First.
1
19> TheRest.
[2,3,4,5]
```

To separate the first elements of the list from the rest of the list, `|` is
used. `First` has got value `1` and `TheRest` has got the value `[2,3,4,5]`.

Another example:

```erlang
20> [E1, E2 | R] = [1,2,3,4,5,6,7].
[1,2,3,4,5,6,7]
21> E1.
1
22> E2.
2
23> R.
[3,4,5,6,7]
```

Here you see the use of `|` to get the first two elements from the list. If you
try to get more elements from the list than there are elements in the list, an
error is returned. Notice also the special case of the list with no elements,
`[]`:

```erlang
24> [A, B | C] = [1, 2].
[1,2]
25> A.
1
26> B.
2
27> C.
[]
```

In the previous examples, new variable names are used, instead of reusing the
old ones: `First`, `TheRest`, `E1`, `E2`, `R`, `A`, `B`, and `C`. The reason for
this is that a variable can only be given a value once in its context (scope).
More about this later.

The following example shows how to find the length of a list. Enter the
following code in a file named `tut4.erl`:

```erlang
-module(tut4).

-export([list_length/1]).

list_length([]) ->
    0;
list_length([First | Rest]) ->
    1 + list_length(Rest).
```

Compile and test:

```erlang
28> c(tut4).
{ok,tut4}
29> tut4:list_length([1,2,3,4,5,6,7]).
7
```

Explanation:

```erlang
list_length([]) ->
    0;
```

The length of an empty list is obviously 0.

```erlang
list_length([First | Rest]) ->
    1 + list_length(Rest).
```

The length of a list with the first element `First` and the remaining elements
`Rest` is 1 + the length of `Rest`.

(Advanced readers only: This is not tail recursive, there is a better way to
write this function.)

In general, tuples are used where "records" or "structs" are used in other
languages. Also, lists are used when representing things with varying sizes,
that is, where linked lists are used in other languages.

Erlang does not have a string data type. Instead, strings can be represented by
lists of Unicode characters. This implies for example that the list `[97,98,99]`
is equivalent to `"abc"`. The Erlang shell is "clever" and guesses what list you
mean and outputs it in what it thinks is the most appropriate form, for example:

```erlang
30> [97,98,99].
"abc"
```

## Maps

Maps are a set of key to value associations. These associations are encapsulated
with `#{` and `}`. To create an association from `"key"` to value `42`:

```erlang
> #{ "key" => 42 }.
#{"key" => 42}
```

Let us jump straight into the deep end with an example using some interesting
features.

The following example shows how to calculate alpha blending using maps to
reference color and alpha channels. Enter the code in a file named `color.erl`):

```erlang
-module(color).

-export([new/4, blend/2]).

-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.

blend(Src,Dst) ->
    blend(Src,Dst,alpha(Src,Dst)).

blend(Src,Dst,Alpha) when Alpha > 0.0 ->
    Dst#{
        red   := red(Src,Dst) / Alpha,
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha
    };
blend(_,Dst,_) ->
    Dst#{
        red   := 0.0,
        green := 0.0,
        blue  := 0.0,
        alpha := 0.0
    }.

alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
```

Compile and test:

```erlang
> c(color).
{ok,color}
> C1 = color:new(0.3,0.4,0.5,1.0).
#{alpha => 1.0,blue => 0.5,green => 0.4,red => 0.3}
> C2 = color:new(1.0,0.8,0.1,0.3).
#{alpha => 0.3,blue => 0.1,green => 0.8,red => 1.0}
> color:blend(C1,C2).
#{alpha => 1.0,blue => 0.5,green => 0.4,red => 0.3}
> color:blend(C2,C1).
#{alpha => 1.0,blue => 0.38,green => 0.52,red => 0.51}
```

This example warrants some explanation:

```erlang
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).
```

First a macro `is_channel` is defined to help with the guard tests. This is only
here for convenience and to reduce syntax cluttering. For more information about
macros, see [The Preprocessor](`e:system:macros.md`).

```erlang
new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.
```

The function `new/4` creates a new map term and lets the keys `red`, `green`,
`blue`, and `alpha` be associated with an initial value. In this case, only
float values between and including 0.0 and 1.0 are allowed, as ensured by the
`?is_channel/1` macro for each argument. Only the `=>` operator is allowed when
creating a new map.

By calling `blend/2` on any color term created by `new/4`, the resulting color
can be calculated as determined by the two map terms.

The first thing `blend/2` does is to calculate the resulting alpha channel:

```erlang
alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).
```

The value associated with key `alpha` is fetched for both arguments using the
`:=` operator. The other keys in the map are ignored, only the key `alpha` is
required and checked for.

This is also the case for functions `red/2`, `blue/2`, and `green/2`.

```erlang
red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
```

The difference here is that a check is made for two keys in each map argument.
The other keys are ignored.

Finally, let us return the resulting color in `blend/3`:

```erlang
blend(Src,Dst,Alpha) when Alpha > 0.0 ->
    Dst#{
        red   := red(Src,Dst) / Alpha,
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha
    };
```

The `Dst` map is updated with new channel values. The syntax for updating an
existing key with a new value is with the `:=` operator.

## Standard Modules and Manual Pages

Erlang has many standard modules to help you do things. For example, the module
`m:io` contains many functions that help in doing formatted input/output. To look
up information about standard modules, the command `h(..)` can be used at the
erlang shell. Try the erlang shell command:

```text
1> h(io).

	io

    Standard I/O server interface functions.
    
    This module provides an interface to standard Erlang I/O servers. The output
    functions all return `ok` if they are successful, or exit if they are not.
     ...
```

If this does not work on your system, the documentation is included as HTML in
the Erlang/OTP release. You can also read the documentation as HTML or download
it as epub from <www.erlang.org/doc>.

## Writing Output to a Terminal

It is nice to be able to do formatted output in examples, so the next example
shows a simple way to use the `io:format/2` function. Like all other exported
functions, you can test the `io:format/2` function in the shell:

```erlang
31> io:format("hello world~n", []).
hello world
ok
32> io:format("this outputs one Erlang term: ~w~n", [hello]).
this outputs one Erlang term: hello
ok
33> io:format("this outputs two Erlang terms: ~w~w~n", [hello, world]).
this outputs two Erlang terms: helloworld
ok
34> io:format("this outputs two Erlang terms: ~w ~w~n", [hello, world]).
this outputs two Erlang terms: hello world
ok
```

The function `io:format/2` (that is, `format` with two arguments) takes two lists.
The first one is nearly always a list written between `" "`. This list is printed
out as it is, except that each `~w` is replaced by a term taken in order from the
second list. Each ~n is replaced by a new line. The `io:format/2` function
itself returns the atom `ok` if everything goes as planned. Like other functions
in Erlang, it crashes if an error occurs. This is not a fault in Erlang, it is a
deliberate policy. Erlang has sophisticated mechanisms to handle errors which
are shown later. As an exercise, try to make `io:format/2` crash, it should not be
difficult. But notice that although `io:format/2` crashes, the Erlang shell itself
does not crash.

## A Larger Example

Now for a larger example to consolidate what you have learnt so far. Assume that
you have a list of temperature readings from a number of cities in the world.
Some of them are in Celsius and some in Fahrenheit (as in the previous list).
First let us convert them all to Celsius, then let us print the data neatly.

```erlang
%% This module is in file tut5.erl

-module(tut5).
-export([format_temps/1]).

%% Only this function is exported
format_temps([])->                        % No output for an empty list
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->  % No conversion needed
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->  % Do the conversion
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).
```

```erlang
35> c(tut5).
{ok,tut5}
36> tut5:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          -10 c
cape_town       21.11111111111111 c
stockholm       -4 c
paris           -2.2222222222222223 c
london          2.2222222222222223 c
ok
```

Before looking at how this program works, notice that a few comments are added
to the code. A comment starts with a %-character and goes on to the end of the
line. Notice also that the `-export([format_temps/1]).` line only includes the
function `format_temps/1`. The other functions are _local_ functions, that is,
they are not visible from outside the module `tut5`.

Notice also that when testing the program from the shell, the input is spread
over two lines as the line was too long.

When `format_temps` is called the first time, `City` gets the value
`{moscow,{c,-10}}` and `Rest` is the rest of the list. So the function
`print_temp(convert_to_celsius({moscow,{c,-10}}))` is called.

Here is a function call as `convert_to_celsius({moscow,{c,-10}})` as the
argument to the function `print_temp`. When function calls are _nested_ like
this, they execute (evaluate) from the inside out. That is, first
`convert_to_celsius({moscow,{c,-10}})` is evaluated, which gives the value
`{moscow,{c,-10}}` as the temperature is already in Celsius. Then
`print_temp({moscow,{c,-10}})` is evaluated. The function `convert_to_celsius`
works in a similar way to the `convert_length` function in the previous example.

`print_temp` simply calls `io:format` in a similar way to what has been
described above. Notice that `~-15w` says to print the "term" with a field length
(width) of 15 and left justify it. (see `io:fwrite/1` manual page in STDLIB).

Now `format_temps(Rest)` is called with the rest of the list as an argument.
This way of doing things is similar to the loop constructs in other languages.
(Yes, this is recursion, but do not let that worry you.) So the same
`format_temps` function is called again, this time `City` gets the value
`{cape_town,{f,70}}` and the same procedure is repeated as before. This is done
until the list becomes empty, that is [], which causes the first clause
`format_temps([])` to match. This simply returns (results in) the atom `ok`, so
the program ends.

## Matching, Guards, and Scope of Variables

It can be useful to find the maximum and minimum temperature in lists like this.
Before extending the program to do this, let us look at functions for finding
the maximum value of the elements in a list:

```erlang
-module(tut6).
-export([list_max/1]).

list_max([Head|Rest]) ->
   list_max(Rest, Head).

list_max([], Res) ->
    Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);
list_max([Head|Rest], Result_so_far)  ->
    list_max(Rest, Result_so_far).
```

```erlang
37> c(tut6).
{ok,tut6}
38> tut6:list_max([1,2,3,4,5,7,4,3,2,1]).
7
```

First notice that two functions have the same name, `list_max`. However, each of
these takes a different number of arguments (parameters). In Erlang these are
regarded as completely different functions. Where you need to distinguish
between these functions, you write Name/Arity, where Name is the function name
and Arity is the number of arguments, in this case `list_max/1` and
`list_max/2`.

In this example you walk through a list "carrying" a value, in this case
`Result_so_far`. `list_max/1` simply assumes that the max value of the list is
the head of the list and calls `list_max/2` with the rest of the list and the
value of the head of the list. In the above this would be
`list_max([2,3,4,5,7,4,3,2,1],1)`. If you tried to use `list_max/1` with an
empty list or tried to use it with something that is not a list at all, you
would cause an error. Notice that the Erlang philosophy is not to handle errors
of this type in the function they occur, but to do so elsewhere. More about this
later.

In `list_max/2`, you walk down the list and use `Head` instead of
`Result_so_far` when `Head` > `Result_so_far`. `when` is a special word used
before the -> in the function to say that you only use this part of the function
if the test that follows is true. A test of this type is called _guard_. If the
guard is false (that is, the guard fails), the next part of the function is
tried. In this case, if `Head` is not greater than `Result_so_far`, then it must
be smaller or equal to it. This means that a guard on the next part of the
function is not needed.

Some useful operators in guards are:

- `<` less than
- `>` greater than
- `==` equal
- `>=` greater or equal
- `=<` less or equal
- `/=` not equal

(see [Guard Sequences](`e:system:expressions.md`)).

To change the above program to one that works out the minimum value of the
element in a list, you only need to write < instead of >. (But it would be wise
to change the name of the function to `list_min`.)

Earlier it was mentioned that a variable can only be given a value once in its
scope. In the above you see that `Result_so_far` is given several values. This
is OK since every time you call `list_max/2` you create a new scope and one can
regard `Result_so_far` as a different variable in each scope.

Another way of creating and giving a variable a value is by using the match
operator = . So if you write `M = 5`, a variable called `M` is created with the
value 5. If, in the same scope, you then write `M = 6`, an error is returned.
Try this out in the shell:

```erlang
39> M = 5.
5
40> M = 6.
** exception error: no match of right hand side value 6
41> M = M + 1.
** exception error: no match of right hand side value 6
42> N = M + 1.
6
```

The use of the match operator is particularly useful for pulling apart Erlang
terms and creating new ones.

```erlang
43> {X, Y} = {paris, {f, 28}}.
{paris,{f,28}}
44> X.
paris
45> Y.
{f,28}
```

Here `X` gets the value `paris` and `Y` the value `{f,28}`.

If you try to do the same again with another city, an error is returned:

```erlang
46> {X, Y} = {london, {f, 36}}.
** exception error: no match of right hand side value {london,{f,36}}
```

Variables can also be used to improve the readability of programs. For example,
in function `list_max/2` above, you can write:

```erlang
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    New_result_far = Head,
    list_max(Rest, New_result_far);
```

This is possibly a little clearer.

## More About Lists

Remember that the `|` operator can be used to get the head of a list:

```erlang
47> [M1|T1] = [paris, london, rome].
[paris,london,rome]
48> M1.
paris
49> T1.
[london,rome]
```

The `|` operator can also be used to add a head to a list:

```erlang
50> L1 = [madrid | T1].
[madrid,london,rome]
51> L1.
[madrid,london,rome]
```

Now an example of this when working with lists - reversing the order of a list:

```erlang
-module(tut8).

-export([reverse/1]).

reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Reversed_List) ->
    reverse(Rest, [Head | Reversed_List]);
reverse([], Reversed_List) ->
    Reversed_List.
```

```erlang
52> c(tut8).
{ok,tut8}
53> tut8:reverse([1,2,3]).
[3,2,1]
```

Consider how `Reversed_List` is built. It starts as [], then successively the
heads are taken off of the list to be reversed and added to the the
`Reversed_List`, as shown in the following:

```erlang
reverse([1|2,3], []) =>
    reverse([2,3], [1|[]])

reverse([2|3], [1]) =>
    reverse([3], [2|[1])

reverse([3|[]], [2,1]) =>
    reverse([], [3|[2,1]])

reverse([], [3,2,1]) =>
    [3,2,1]
```

The module `lists` contains many functions for manipulating lists, for example,
for reversing them. So before writing a list-manipulating function it is a good
idea to check if one not already is written for you (see the `m:lists` manual
page in STDLIB).

Now let us get back to the cities and temperatures, but take a more structured
approach this time. First let us convert the whole list to Celsius as follows:

```erlang
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    convert_list_to_c(List_of_cities).

convert_list_to_c([{Name, {f, F}} | Rest]) ->
    Converted_City = {Name, {c, (F -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
    [].
```

Test the function:

```erlang
54> c(tut7).
{ok, tut7}.
55> tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
[{moscow,{c,-10}},
 {cape_town,{c,21.11111111111111}},
 {stockholm,{c,-4}},
 {paris,{c,-2.2222222222222223}},
 {london,{c,2.2222222222222223}}]
```

Explanation:

```erlang
format_temps(List_of_cities) ->
    convert_list_to_c(List_of_cities).
```

Here `format_temps/1` calls `convert_list_to_c/1`. `convert_list_to_c/1` takes
off the head of the `List_of_cities`, converts it to Celsius if needed. The `|`
operator is used to add the (maybe) converted to the converted rest of the list:

```erlang
[Converted_City | convert_list_to_c(Rest)];
```

or:

```erlang
[City | convert_list_to_c(Rest)];
```

This is done until the end of the list is reached, that is, the list is empty:

```erlang
convert_list_to_c([]) ->
    [].
```

Now when the list is converted, a function to print it is added:

```erlang
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    Converted_List = convert_list_to_c(List_of_cities),
    print_temp(Converted_List).

convert_list_to_c([{Name, {f, F}} | Rest]) ->
    Converted_City = {Name, {c, (F -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
    [].

print_temp([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Rest);
print_temp([]) ->
    ok.
```

```erlang
56> c(tut7).
{ok,tut7}
57> tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          -10 c
cape_town       21.11111111111111 c
stockholm       -4 c
paris           -2.2222222222222223 c
london          2.2222222222222223 c
ok
```

Now a function has to be added to find the cities with the maximum and minimum
temperatures. The following program is not the most efficient way of doing this
as you walk through the list of cities four times. But it is better to first
strive for clarity and correctness and to make programs efficient only if
needed.

```erlang
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    Converted_List = convert_list_to_c(List_of_cities),
    print_temp(Converted_List),
    {Max_city, Min_city} = find_max_and_min(Converted_List),
    print_max_and_min(Max_city, Min_city).

convert_list_to_c([{Name, {f, Temp}} | Rest]) ->
    Converted_City = {Name, {c, (Temp -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->
    [].

print_temp([{Name, {c, Temp}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Rest);
print_temp([]) ->
    ok.

find_max_and_min([City | Rest]) ->
    find_max_and_min(Rest, City, City).

find_max_and_min([{Name, {c, Temp}} | Rest],
         {Max_Name, {c, Max_Temp}},
         {Min_Name, {c, Min_Temp}}) ->
    if
        Temp > Max_Temp ->
            Max_City = {Name, {c, Temp}};           % Change
        true ->
            Max_City = {Max_Name, {c, Max_Temp}} % Unchanged
    end,
    if
         Temp < Min_Temp ->
            Min_City = {Name, {c, Temp}};           % Change
        true ->
            Min_City = {Min_Name, {c, Min_Temp}} % Unchanged
    end,
    find_max_and_min(Rest, Max_City, Min_City);

find_max_and_min([], Max_City, Min_City) ->
    {Max_City, Min_City}.

print_max_and_min({Max_name, {c, Max_temp}}, {Min_name, {c, Min_temp}}) ->
    io:format("Max temperature was ~w c in ~w~n", [Max_temp, Max_name]),
    io:format("Min temperature was ~w c in ~w~n", [Min_temp, Min_name]).
```

```erlang
58> c(tut7).
{ok, tut7}
59> tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          -10 c
cape_town       21.11111111111111 c
stockholm       -4 c
paris           -2.2222222222222223 c
london          2.2222222222222223 c
Max temperature was 21.11111111111111 c in cape_town
Min temperature was -10 c in moscow
ok
```

## If and Case

The function `find_max_and_min` works out the maximum and minimum temperature. A
new construct, `if`, is introduced here. If works as follows:

```c
if
    Condition 1 ->
        Action 1;
    Condition 2 ->
        Action 2;
    Condition 3 ->
        Action 3;
    Condition 4 ->
        Action 4
end
```

Notice that there is no `;` before `end`. Conditions do the same as guards, that
is, tests that succeed or fail. Erlang starts at the top and tests until it
finds a condition that succeeds. Then it evaluates (performs) the action
following the condition and ignores all other conditions and actions before the
`end`. If no condition matches, a run-time failure occurs. A condition that
always succeeds is the atom `true`. This is often used last in an `if`, meaning,
do the action following the `true` if all other conditions have failed.

The following is a short program to show the workings of `if`.

```erlang
-module(tut9).
-export([test_if/2]).

test_if(A, B) ->
    if
        A == 5 ->
            io:format("A == 5~n", []),
            a_equals_5;
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7
    end.
```

Testing this program gives:

```erlang
60> c(tut9).
{ok,tut9}
61> tut9:test_if(5,33).
A == 5
a_equals_5
62> tut9:test_if(33,6).
B == 6
b_equals_6
63> tut9:test_if(2, 3).
A == 2, B == 3
a_equals_2_b_equals_3
64> tut9:test_if(1, 33).
A == 1 ; B == 7
a_equals_1_or_b_equals_7
65> tut9:test_if(33, 7).
A == 1 ; B == 7
a_equals_1_or_b_equals_7
66> tut9:test_if(33, 33).
** exception error: no true branch found when evaluating an if expression
     in function  tut9:test_if/2 (tut9.erl, line 5)
```

Notice that `tut9:test_if(33,33)` does not cause any condition to succeed. This
leads to the run time error `if_clause`, here nicely formatted by the shell. See
[Guard Sequences](`e:system:expressions.md`) for details of the many guard tests
available.

`case` is another construct in Erlang. Recall that the `convert_length` function
was written as:

```erlang
convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
```

The same program can also be written as:

```erlang
-module(tut10).
-export([convert_length/1]).

convert_length(Length) ->
    case Length of
        {centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    end.
```

```erlang
67> c(tut10).
{ok,tut10}
68> tut10:convert_length({inch, 6}).
{centimeter,15.24}
69> tut10:convert_length({centimeter, 2.5}).
{inch,0.984251968503937}
```

Both `case` and `if` have _return values_, that is, in the above example `case`
returned either `{inch,X/2.54}` or `{centimeter,Y*2.54}`. The behaviour of
`case` can also be modified by using guards. The following example clarifies
this. It tells us the length of a month, given the year. The year must be known,
since February has 29 days in a leap year.

```erlang
-module(tut11).
-export([month_length/2]).

month_length(Year, Month) ->
    %% All years divisible by 400 are leap
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above)
    Leap = if
        trunc(Year / 400) * 400 == Year ->
            leap;
        trunc(Year / 100) * 100 == Year ->
            not_leap;
        trunc(Year / 4) * 4 == Year ->
            leap;
        true ->
            not_leap
    end,
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.
```

```erlang
70> c(tut11).
{ok,tut11}
71> tut11:month_length(2004, feb).
29
72> tut11:month_length(2003, feb).
28
73> tut11:month_length(1947, aug).
31
```

## Built-In Functions (BIFs)

BIFs are functions that for some reason are built-in to the Erlang virtual
machine. BIFs often implement functionality that is impossible or is too
inefficient to implement in Erlang. Some BIFs can be called using the function
name only but they are by default belonging to the `erlang` module. For example,
the call to the BIF `trunc` below is equivalent to a call to `erlang:trunc`.

As shown, first it is checked if a year is leap. If a year is divisible by 400,
it is a leap year. To determine this, first divide the year by 400 and use the
BIF `trunc` (more about this later) to cut off any decimals. Then multiply by
400 again and see if the same value is returned again. For example, year 2004:

```erlang
2004 / 400 = 5.01
trunc(5.01) = 5
5 * 400 = 2000
```

2000 is not the same as 2004, so 2004 is not divisible by 400. Year 2000:

```erlang
2000 / 400 = 5.0
trunc(5.0) = 5
5 * 400 = 2000
```

That is, a leap year. The next two `trunc`\-tests evaluate if the year is
divisible by 100 or 4 in the same way. The first `if` returns `leap` or
`not_leap`, which lands up in the variable `Leap`. This variable is used in the
guard for `feb` in the following `case` that tells us how long the month is.

This example showed the use of `trunc`. It is easier to use the Erlang operator
`rem` that gives the remainder after division, for example:

```erlang
74> 2004 rem 400.
4
```

So instead of writing:

```erlang
trunc(Year / 400) * 400 == Year ->
    leap;
```

it can be written:

```erlang
Year rem 400 == 0 ->
    leap;
```

There are many other BIFs such as `trunc`. Only a few BIFs can be used in
guards, and you cannot use functions you have defined yourself in guards. (see
[Guard Sequences](`e:system:expressions.md`)) (For advanced readers: This is to
ensure that guards do not have side effects.) Let us play with a few of these
functions in the shell:

```erlang
75> trunc(5.6).
5
76> round(5.6).
6
77> length([a,b,c,d]).
4
78> float(5).
5.0
79> is_atom(hello).
true
80> is_atom("hello").
false
81> is_tuple({paris, {c, 30}}).
true
82> is_tuple([paris, {c, 30}]).
false
```

All of these can be used in guards. Now for some BIFs that cannot be used in
guards:

```erlang
83> atom_to_list(hello).
"hello"
84> list_to_atom("goodbye").
goodbye
85> integer_to_list(22).
"22"
```

These three BIFs do conversions that would be difficult (or impossible) to do in
Erlang.

## Higher-Order Functions (Funs)

Erlang, like most modern functional programming languages, has higher-order
functions. Here is an example using the shell:

```erlang
86> Xf = fun(X) -> X * 2 end.
#Fun<erl_eval.5.123085357>
87> Xf(5).
10
```

Here is defined a function that doubles the value of a number and assigned this
function to a variable. Thus `Xf(5)` returns value 10. Two useful functions when
working with lists are `foreach` and `map`, which are defined as follows:

```erlang
foreach(Fun, [First|Rest]) ->
    Fun(First),
    foreach(Fun, Rest);
foreach(Fun, []) ->
    ok.

map(Fun, [First|Rest]) ->
    [Fun(First)|map(Fun,Rest)];
map(Fun, []) ->
    [].
```

These two functions are provided in the standard module `lists`. `foreach` takes
a list and applies a fun to every element in the list. `map` creates a new list
by applying a fun to every element in a list. Going back to the shell, `map` is
used and a fun to add 3 to every element of a list:

```erlang
88> Add_3 = fun(X) -> X + 3 end.
#Fun<erl_eval.5.123085357>
89> lists:map(Add_3, [1,2,3]).
[4,5,6]
```

Let us (again) print the temperatures in a list of cities:

```erlang
90> Print_City = fun({City, {X, Temp}}) -> io:format("~-15w ~w ~w~n",
[City, X, Temp]) end.
#Fun<erl_eval.5.123085357>
91> lists:foreach(Print_City, [{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
moscow          c -10
cape_town       f 70
stockholm       c -4
paris           f 28
london          f 36
ok
```

Let us now define a fun that can be used to go through a list of cities and
temperatures and transform them all to Celsius.

```erlang
-module(tut13).

-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    lists:map(fun convert_to_c/1, List).
```

```erlang
92> tut13:convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
[{moscow,{c,-10}},
 {cape_town,{c,21}},
 {stockholm,{c,-4}},
 {paris,{c,-2}},
 {london,{c,2}}]
```

The `convert_to_c` function is the same as before, but here it is used as a fun:

```erlang
lists:map(fun convert_to_c/1, List)
```

When a function defined elsewhere is used as a fun, it can be referred to as
`Function/Arity` (remember that `Arity` = number of arguments). So in the
`map`\-call `lists:map(fun convert_to_c/1, List)` is written. As shown,
`convert_list_to_c` becomes much shorter and easier to understand.

The standard module `lists` also contains a function `sort(Fun, List)` where
`Fun` is a fun with two arguments. This fun returns `true` if the first argument
is less than the second argument, or else `false`. Sorting is added to the
`convert_list_to_c`:

```erlang
-module(tut13).

-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    New_list = lists:map(fun convert_to_c/1, List),
    lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) ->
                       Temp1 < Temp2 end, New_list).
```

```erlang
93> c(tut13).
{ok,tut13}
94> tut13:convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}},
{stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
[{moscow,{c,-10}},
 {stockholm,{c,-4}},
 {paris,{c,-2}},
 {london,{c,2}},
 {cape_town,{c,21}}]
```

In `sort` the fun is used:

```erlang
fun({_, {c, Temp1}}, {_, {c, Temp2}}) -> Temp1 < Temp2 end,
```

Here the concept of an _anonymous variable_ `_` is introduced. This is simply
shorthand for a variable that gets a value, but the value is ignored. This can
be used anywhere suitable, not just in funs. `Temp1 < Temp2` returns `true` if
`Temp1` is less than `Temp2`.
