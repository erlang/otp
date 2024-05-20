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
# Data Types

Erlang provides a number of data types, which are listed in this section.

[](){: #no_user_types }

Note that Erlang has no user defined types, only composite types (data
structures) made of Erlang terms. This means that any function testing for a
composite type, typically named `is_type/1`, might return `true` for a term that
coincides with the chosen representation. The corresponding functions for built
in types do not suffer from this.

## Terms

A piece of data of any data type is called a _term_.

## Number

There are two types of numeric literals, _integers_ and _floats_. Besides the
conventional notation, there are two Erlang-specific notations:

- `$`_`char`_  
  ASCII value or unicode code-point of the character _`char`_.
- _`base`_`#`_`value`_  
  Integer with the base _`base`_, which must be an integer in the range 2
  through 36.

Leading zeroes are ignored. Single underscore characters (`_`) can be
inserted between digits as a visual separator.

_Examples:_

```text
1> 42.
42
2> -1_234_567_890.
-1234567890
3> $A.
65
4> $\n.
10
5> 2#101.
5
6> 16#1f.
31
7> 16#4865_316F_774F_6C64.
5216630098191412324
8> 2.3.
2.3
9> 2.3e3.
2.3e3
10> 2.3e-3.
0.0023
11> 1_234.333_333
1234.333333
12> 36#helloworld.
1767707668033969
```

[](){: #numeric_comparisons }

### Comparisons

Both integers and floats share the same linear order. That is, `1` compares less
than `2.4`, `3` compares greater than `2.99999`, and `5` is equal to `5.0`.

When wanting to compare an integer with another integer or a float with another
float, it may be tempting to use the term equivalence operators (`=:=`, `=/=`)
or pattern matching. This works for integers which has a distinct representation
for every number, but there's a surprising edge case for floating-point as the
latter has two representations for zero which are considered different by the
term equivalence operators and pattern matching.

If you wish to compare floating-point numbers _numerically_, use the regular
comparison operators (such as `==`) and add guards that require both the
arguments to be floating-point.

> #### Note {: .info }
>
> Prior to OTP 27, the term equivalence operators had a bug where they
> considered `0.0` and `-0.0` to be the same term. Legacy code that makes
> equality comparisons on floating-point zero should migrate to using the
> equal-to (`==`) operator with [`is_float/1`](`is_float/1`) guards, and
> compiler warnings have been added to that effect. These can be silenced by
> writing `+0.0` instead, which is the same as `0.0` but makes the compiler
> interpret the comparison as being purposely made against `0.0`.
>
> Note that this does _not_ break compatibility with IEEE 754 which mandates
> that `0.0` and `-0.0` should compare equal: they are equal when interpreted as
> numbers (`==`), and unequal when interpreted as opaque terms (`=:=`).

[](){: #float_representation_problem }

_Examples_:

```erlang
1> 0.0 =:= +0.0.
true
2> 0.0 =:= -0.0.
false
3> +0.0 =:= -0.0.
false
4> +0.0 == -0.0.
true
```

### Representation of Floating Point Numbers

When working with floats you may not see what you expect when printing or doing
arithmetic operations. This is because floats are represented by a fixed number
of bits in a base-2 system while printed floats are represented with a base-10
system. Erlang uses 64-bit floats. Here are examples of this phenomenon:

```erlang
1> 0.1+0.2.
0.30000000000000004
```

The real numbers `0.1` and `0.2` cannot be represented exactly as floats.

```erlang
1> {36028797018963968.0, 36028797018963968 == 36028797018963968.0,
  36028797018963970.0, 36028797018963970 == 36028797018963970.0}.
{3.602879701896397e16, true,
 3.602879701896397e16, false}.
```

The value `36028797018963968` can be represented exactly as a float value but
Erlang's pretty printer rounds `36028797018963968.0` to `3.602879701896397e16`
(`=36028797018963970.0`) as all values in the range
`[36028797018963966.0, 36028797018963972.0]` are represented by
`36028797018963968.0`.

For more information about floats and issues with them see:

- [What Every Programmer Should Know About Floating-Point Arithmetic](https://floating-point-gui.de/)
- [0\.30000000000000004.com/](https://0.30000000000000004.com/)
- [Floating Point Arithmetic: Issues and Limitations](https://docs.python.org/3/tutorial/floatingpoint.html)

If you need to work with exact decimal fractions, for instance to represent
money, it is recommended to use a library that handles that, or work in
cents instead of dollars or euros so that decimal fractions are not needed.

Also note that Erlang's floats do not exactly match IEEE 754 floats,
in that neither _Inf_ nor _NaN_ are supported in Erlang. Any
operation that would result in _NaN_, _+Inf_, or _-Inf_, will instead raise
a `badarith` exception.

_Examples_:

```erlang
1> 1.0 / 0.0.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as 1.0 / 0.0
2> 0.0 / 0.0.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as 0.0 / 0.0
```

## Atom

An atom is a literal, a constant with name. An atom is to be enclosed in single
quotes (`'`) if it does not begin with a lower-case letter or if it contains other
characters than alphanumeric characters, underscore (`_`), or `@`.

_Examples:_

```text
hello
phone_number
name@node
'Monday'
'phone number'
```

## Bit Strings and Binaries

A bit string is used to store an area of untyped memory.

Bit strings are expressed using the [bit syntax](expressions.md#bit-syntax-expressions).

Bit strings that consist of a number of bits that are evenly divisible
by eight are called _binaries_.

_Examples:_

```text
1> <<10,20>>.
<<10,20>>
2> <<"ABC">>.
<<"ABC">>
3> <<1:1,0:1>>.
<<2:2>>
```

The [`is_bitstring/1`](`erlang:is_bitstring/1`) BIF tests whether a
term is a bit string, and the [`is_binary/1`](`erlang:is_binary/1`)
BIF tests whether a term is a binary.

_Examples:_

```erlang
1> is_bitstring(<<1:1>>).
true
2> is_binary(<<1:1>>).
false
3> is_binary(<<42>>).
true

```

For more examples, see [Programming Examples](`e:system:bit_syntax.md`).

## Reference

A term that is [unique](`e:system:system_limits.md#unique_references`)
among connected nodes. A reference is created by calling the
[`make_ref/0`](`erlang:make_ref/0`) BIF. The
[`is_reference/1`](`erlang:is_reference/1`) BIF tests whether a term
is a reference.

_Examples:_

```erlang
1> Ref = make_ref().
#Ref<0.76482849.3801088007.198204>
2> is_reference(Ref).
true
```

## Fun

A fun is a functional object. Funs make it possible to create an anonymous
function and pass the function itself — not its name — as argument to other
functions.

_Examples:_

```erlang
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.39074546>
2> Fun1(2).
3
```

The [`is_function/1`](`erlang:is_function/1`) and [`is_function/2`](`erlang:is_function/2`)
BIFs tests whether a term is a fun.

_Examples_:

```erlang
1> F = fun() -> ok end.
#Fun<erl_eval.43.105768164>
2> is_function(F).
true
3> is_function(F, 0).
true
4> is_function(F, 1).
false
```

Read more about funs in [Fun Expressions](expressions.md#fun-expressions). For more
examples, see [Programming Examples](`e:system:funs.md`).

## Port Identifier

A port identifier identifies an Erlang port.

[`open_port/2`](`open_port/2`) returns a port identifier. The
[`is_port/1`](`erlang:is_port/1`) BIF tests whether a term is a port
identifier.

Read more about ports in [Ports and Port Drivers](ports.md).

## Pid

Pid is an abbreviation for process identifier. Each process has a Pid which
identifies the process. Pids are unique among processes that are alive on
connected nodes. However, a Pid of a terminated process may be reused as a Pid
for a new process after a while.

The BIF [`self/0`](`erlang:self/0`) returns the Pid of the calling process. When
[creating a new process](ref_man_processes.md#process-creation), the parent
process will be able to get the Pid of the child process either via the return
value, as is the case when calling the [`spawn/3`](`erlang:spawn/3`) BIF, or via
a message, which is the case when calling the
[`spawn_request/5`](`erlang:spawn_request/5`) BIF. A Pid is typically used when
when sending a process a [signal](ref_man_processes.md#signals). The
[`is_pid/1`](`erlang:is_pid/1`) BIF tests whether a term is a Pid.

_Example:_

```erlang
-module(m).
-export([loop/0]).

loop() ->
    receive
        who_are_you ->
            io:format("I am ~p~n", [self()]),
            loop()
    end.

1> P = spawn(m, loop, []).
<0.58.0>
2> P ! who_are_you.
I am <0.58.0>
who_are_you
```

Read more about processes in [Processes](ref_man_processes.md).

## Tuple

A tuple is a compound data type with a fixed number of terms:

```text
{Term1,...,TermN}
```

Each term `Term` in the tuple is called an _element_. The number of elements is
said to be the _size_ of the tuple.

There exists a number of BIFs to manipulate tuples.

_Examples:_

```erlang
1> P = {adam,24,{july,29}}.
{adam,24,{july,29}}
2> element(1,P).
adam
3> element(3,P).
{july,29}
4> P2 = setelement(2,P,25).
{adam,25,{july,29}}
5> tuple_size(P).
3
6> tuple_size({}).
0
7> is_tuple({a,b,c}).
true
```

## Map

A map is a compound data type with a variable number of key-value associations:

```text
#{Key1 => Value1, ..., KeyN => ValueN}
```

Each key-value association in the map is called an _association pair_. The key
and value parts of the pair are called _elements_. The number of association
pairs is said to be the _size_ of the map.

There exists a number of BIFs to manipulate maps.

_Examples:_

```erlang
1> M1 = #{name => adam, age => 24, date => {july,29}}.
#{age => 24,date => {july,29},name => adam}
2> maps:get(name, M1).
adam
3> maps:get(date, M1).
{july,29}
4> M2 = maps:update(age, 25, M1).
#{age => 25,date => {july,29},name => adam}
5> map_size(M).
3
6> map_size(#{}).
0
```

A collection of maps processing functions are found in module `m:maps`
in STDLIB.

Read more about maps in [Map Expressions](expressions.md#map-expressions).

> #### Change {: .info }
>
> Maps were introduced as an experimental feature in Erlang/OTP R17. Their
> functionality was extended and became fully supported in Erlang/OTP 18.

## List

A list is a compound data type with a variable number of terms.

```text
[Term1,...,TermN]
```

Each term `Term` in the list is called an _element_. The number of elements is
said to be the _length_ of the list.

Formally, a list is either the empty list `[]` or consists of a _head_ (first
element) and a _tail_ (remainder of the list). The _tail_ is also a list. The
latter can be expressed as `[H|T]`. The notation `[Term1,...,TermN]` above is
equivalent with the list `[Term1|[...|[TermN|[]]]]`.

_Example:_

`[]` is a list, thus  
`[c|[]]` is a list, thus  
`[b|[c|[]]]` is a list, thus  
`[a|[b|[c|[]]]]` is a list, or in short `[a,b,c]`

A list where the tail is a list is sometimes called a _proper list_. It is
allowed to have a list where the tail is not a list, for example, `[a|b]`.
However, this type of list is of little practical use.

_Examples:_

```erlang
1> L1 = [a,2,{c,4}].
[a,2,{c,4}]
2> [H|T] = L1.
[a,2,{c,4}]
3> H.
a
4> T.
[2,{c,4}]
5> L2 = [d|T].
[d,2,{c,4}]
6> length(L1).
3
7> length([]).
0
```

A collection of list processing functions are found in module
`m:lists` in STDLIB.

## String

Strings are enclosed in double quotes ("), but is not a data type in Erlang.
Instead, a string `"hello"` is shorthand for the list `[$h,$e,$l,$l,$o]`, that
is, `[104,101,108,108,111]`.

Two adjacent string literals are concatenated into one. This is done in the
compilation.

_Example:_

```text
"string" "42"
```

is equivalent to

```text
"string42"
```

> #### Change {: .info }
>
> Starting with Erlang/OTP 27 two adjacent string literals have to be separated
> by white space, or otherwise it is a syntax error. This avoids possible confusion
> with _triple-quoted strings_.

[](){: #tqstring } Strings can also be written as _triple-quoted strings_, which
can be _indented_ over multiple lines to follow the indentation of the
surrounding code. They are also _verbatim_, that is, they do not allow escape
sequences, and thereby do not need double quote characters to be escaped.

> #### Change {: .info }
>
> Triple-quoted strings were added in Erlang/OTP 27. Before that 3 consecutive
> double quote characters had a different meaning. There were absolutely no good
> reason to write such a character sequence before triple-quoted strings
> existed, but there _are_ some gotchas; see the
> [Warning ](data_types.md#triple-quoted-strings-warning) at the end of this
> description of triple-quoted strings.

Example, with verbatim double quote characters:

```text
"""
  Line "1"
  Line "2"
  """
```

That is equivalent to the normal single quoted string (which also allows
newlines):

```text
"Line \"1\"
Line \"2\""
```

The opening and the closing line has got the delimiters: the `"""` characters.
The lines between them are the content lines. The newline on the opening line is
not regarded as string content, nor is the newline on the last content line.

The indentation is defined by the white space character sequence preceding the
delimiter on the closing line. That character sequence is stripped from all
content lines. There can only be white space before the delimiter on the closing
line, or else it is regarded as a content line.

The opening line is not allowed to have any characters other than white space
after the delimiter, and all content lines must start with the defined
indentation character sequence, otherwise the string has a syntax error.

Here is a larger example:

```text
X = """
      First line starting with two spaces
    Not escaped: "\t \r \xFF" and """

    """
```

That corresponds to the normal string:

```text
X = "  First line starting with two spaces
Not escaped: \"\\t \\r \\xFF\" and \"\"\"
"
```

It is possible to write consecutive double quote characters on the
beginning of a content line by using more double quote characters as
delimiters. This is a string that contains exactly four double quote
characters, using a delimiter with five double quote characters:

```text
"""""
""""
"""""
```

These strings are all the empty string:

```text
""
```

```text
"""
"""
```

```text
"""

  """
```

[](){: #triple-quoted-strings-warning }

> #### Warning {: .warning }
>
> Before Erlang/OTP 27, when triple-quoted strings were added, the character
> sequence `"""` was interpreted as `"" "`, which means concatenating the empty
> string to the string that follows. All sequences of an odd number of double
> quote characters had this meaning.
>
> Any even number of double quote characters was interpreted as a sequence of
> empty strings, that were concatenated (to the empty string).
>
> There was no reason to write such character sequences. But should that have
> happened, the meaning has probably changed with the introduction of triple-quoted
> strings.
>
> The compiler preprocessor was patched in Erlang/OTP 26.1 to warn about 3 or
> more sequential double quote characters. In Erlang/OTP 26.2 this was improved
> to warn about adjacent string literals without intervening white space, which
> also covers the same problem at a string end.
>
> If the compiler should emit such a warning, please change such double quote
> character sequences to have a whitespace after every second quote character,
> remove redundant empty strings, or write them as one string. This makes the
> code more readable, and means the same thing in all releases.

## Sigil

A _sigil_ is a prefix to a string literal. It is not a data type in Erlang, but
a shorthand notation that indicates how to interpret the string literal. Sigils
offer mainly two things: a compact way to create UTF-8 encoded binary strings,
and a way to write verbatim strings (not having to escape `\` characters),
useful for regular expressions, for example.

A sigil starts with the Tilde character (`~`) followed by a name defining the
sigil type.

Immediately after follows the sigil content; a character sequence between
content delimiters. The allowed delimiters are these start-end delimiter pairs:
`() [] {} <>`, or these characters that are both start and end delimiters:
``/ | ' " ` #``. [Triple-quote](data_types.md#tqstring) string delimiters may
also be used.

The [character escaping rules ](data_types.md#escape-sequences)for the sigil
content depends on the sigil type. When the sigil content is _verbatim_, there
is no escape character. The sigil content simply ends when the end delimiter is
found, so it is impossible to have the end delimiter character in the string
content. The set of delimiters is fairly generous, and in most cases it is
possible to choose an end delimiter that's not in the literal string content.

[Triple-quote](data_types.md#tqstring) string delimiters allow choosing a larger
number of quote characters in the end delimiter, than whatever is in the string
content, which thereby facilitates any content also with a sequence of `"`
characters at the start of a line even for a _verbatim_ string.

The Sigils are:

- **`~`** - The Vanilla (default) Sigil. Shorthand for a UTF-8 encoded
  `t:binary/0`. This sigil does not affect the character escaping rules, so with
  triple-quoted string delimiters they are the same as for `~B`, and for other
  string delimiters they are the same as for `~b`.

- **`~b`** - The Binary Sigil. Shorthand for a
  [UTF-8 encoded `binary()`](`t:unicode:unicode_binary/0`), as if calling
  [`unicode:characters_to_binary/1` ](`unicode:characters_to_binary/1`)on the
  sigil content. Character escaping rules are the same as for `~s`.

- **`~B`** - The Verbatim Binary Sigil. As `~b`, but the sigil content is
  verbatim.

- **`~s`** - The String Sigil. Shorthand for a
  [`string()`](`t:erlang:string/0`), that is, a `[char()]` which is a list of
  Unicode codepoints.
  [Character escaping rules ](data_types.md#escape-sequences)are the same as for
  a normal `t:string/0`. Using this sigil on a regular string does effectively
  nothing.

- **`~S`** - The Verbatim String Sigil. As `~s`, but the sigil content is
  verbatim. Using this sigil on a triple-quoted string does effectively nothing.

Examples

```text
<<"\"\\µA\""/utf8>> = <<$",$\\,194,181,$A,$">> =
    ~b"""
        "\\µA"
        """ = ~b'"\\µA"' =
    ~B"""
        "\µA"
        """ = ~B<"\µA"> =
    ~"""
        "\µA"
        """ = ~"\"\\µA\"" = ~/"\\µA"/
```

```text
[$",$\\,$µ,$A,$"] =
    ~s"""
        "\\µA"
        """ = ~s"\"\\µA\"" = ~s["\\µA"] =
    ~S"""
        "\µA"
        """ = ~S("\µA") =
    """
        "\µA"
        """ = "\"\\µA\""
```

Adjacent strings are concatenated in the compilation, but that is not possible
with sigils, since they are transformed into terms that in general may not be
concatenated. So, `"a" "b"` is equivalent to `"ab"`, but `~s"a" "b"` or
`~s"a" ~s"b"` is a syntax error. `~s"a" ++ "b"`, however, evaluates to `"ab"`
since both operands to the `++` operator are strings.

> #### Change {: .info }
>
> Sigils were introduced in Erlang/OTP 27

## Record

A record is a data structure for storing a fixed number of elements. It has
named fields and is similar to a struct in C. However, a record is not a true
data type. Instead, record expressions are translated to tuple expressions
during compilation. Therefore, record expressions are not understood by the
shell unless special actions are taken. For details, see module `m:shell`
in STDLIB.

_Examples:_

```erlang
-module(person).
-export([new/2]).

-record(person, {name, age}).

new(Name, Age) ->
    #person{name=Name, age=Age}.

1> person:new(ernie, 44).
{person,ernie,44}
```

Read more about records in [Records](ref_man_records.md). More examples are
found in [Programming Examples](`e:system:prog_ex_records.md`).

## Boolean

There is no Boolean data type in Erlang. Instead the atoms `true` and `false`
are used to denote Boolean values. The [`is_boolean/1`](`erlang:is_boolean/1`)
BIF tests whether a term is a boolean.

_Examples:_

```erlang
1> 2 =< 3.
true
2> true or false.
true
3> is_boolean(true).
true
4> is_boolean(false).
true
5> is_boolean(ok).
false
```

## Escape Sequences

Within strings (`"`\-delimited), quoted atoms, and the content of
[`~b` and `~s` sigils](data_types.md#sigil), the following escape sequences are
recognized:

| _Sequence_                  | _Description_                                                                         |
| --------------------------- | ------------------------------------------------------------------------------------- |
| `\b`                        | Backspace (ASCII code 8)                                                              |
| `\d`                        | Delete (ASCII code 127)                                                               |
| `\e`                        | Escape (ASCII code 27)                                                                |
| `\f`                        | Form Feed (ASCII code 12)                                                             |
| `\n`                        | Line Feed/Newline (ASCII code 10)                                                     |
| `\r`                        | Carriage Return (ASCII code 13)                                                       |
| `\s`                        | Space (ASCII code 32)                                                                 |
| `\t`                        | (Horizontal) Tab (ASCII code 9)                                                       |
| `\v`                        | Vertical Tab (ASCII code 11)                                                          |
| `\`XYZ, `\`YZ, `\`Z         | Character with octal representation XYZ, YZ or Z                                      |
| `\xXY`                      | Character with hexadecimal representation XY                                          |
| `\x{`X...`}`                | Character with hexadecimal representation; X... is one or more hexadecimal characters |
| `\^a`...`\^z` `\^A`...`\^Z` | Control A to control Z                                                                |
| `\^@`                       | NUL (ASCII code 0)                                                                    |
| `\^[`                       | Escape (ASCII code 27)                                                                |
| `\^\`                       | File Separator (ASCII code 28)                                                        |
| `\^]`                       | Group Separator (ASCII code 29)                                                       |
| `\^^`                       | Record Separator (ASCII code 30)                                                      |
| `\^_`                       | Unit Separator (ASCII code 31)                                                        |
| `\^?`                       | Delete (ASCII code 127)                                                               |
| `\'`                        | Single quote                                                                          |
| `\"`                        | Double quote                                                                          |
| `\\`                        | Backslash                                                                             |

_Table: Recognized Escape Sequences_

> #### Change {: .info }
>
> As of Erlang/OTP 26, the value of `$\^?` has been changed to be 127 (Delete),
> instead of 31. Previous releases would allow any character following `$\^`; as
> of Erlang/OTP 26, only the documented characters are allowed.

Within [triple-quoted strings](data_types.md#tqstring), escape sequences are not
recognized. The only text that cannot be written in a triple-quoted string is
three consecutive double quote characters at the beginning of a line (preceded
only by whitespace). This limitation can be worked around by using more double
quote characters for the string delimiters than in the string. Any number three
or above is allowed for the start delimiter and the end delimiter is the same as
the start delimiter.

When triple-quote string delimiters are used with the
[`~`, `~B` or `~S` sigils ](data_types.md#sigil)the same applies, but for the
[`~b` or `~s` sigils ](data_types.md#sigil)the escape sequences for normal
strings, above, are used.

> #### Change {: .info }
>
> Triple-quoted strings and sigils were introduced in Erlang/OTP 27.

## Type Conversions

There are a number of BIFs for type conversions.

_Examples:_

```erlang
1> atom_to_list(hello).
"hello"
2> list_to_atom("hello").
hello
3> binary_to_list(<<"hello">>).
"hello"
4> binary_to_list(<<104,101,108,108,111>>).
"hello"
5> list_to_binary("hello").
<<104,101,108,108,111>>
6> float_to_list(7.0).
"7.00000000000000000000e+00"
7> list_to_float("7.000e+00").
7.0
8> integer_to_list(77).
"77"
9> list_to_integer("77").
77
10> tuple_to_list({a,b,c}).
[a,b,c]
11> list_to_tuple([a,b,c]).
{a,b,c}
12> term_to_binary({a,b,c}).
<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>
13> binary_to_term(<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>).
{a,b,c}
14> binary_to_integer(<<"77">>).
77
15> integer_to_binary(77).
<<"77">>
16> float_to_binary(7.0).
<<"7.00000000000000000000e+00">>
17> binary_to_float(<<"7.000e+00">>).
7.0
```
