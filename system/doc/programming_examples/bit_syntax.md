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
# Bit Syntax

## Introduction

The complete specification for the bit syntax appears in the
[Reference Manual](`e:system:expressions.md#bit-syntax-expressions`).

In Erlang, a Bin is used for constructing binaries and matching binary patterns.
A Bin is written with the following syntax:

```erlang
<<E1, E2, ... En>>
```

A Bin is a low-level sequence of bits or bytes. The purpose of a Bin is to
enable construction of binaries:

```erlang
Bin = <<E1, E2, ... En>>
```

All elements must be bound. Or match a binary:

```erlang
<<E1, E2, ... En>> = Bin
```

Here, `Bin` is bound and the elements are bound or unbound, as in any match.

A Bin does not need to consist of a whole number of bytes.

A _bitstring_ is a sequence of zero or more bits, where the number of bits does
not need to be divisible by 8. If the number of bits is divisible by 8, the
bitstring is also a binary.

Each element specifies a certain _segment_ of the bitstring. A segment is a set
of contiguous bits of the binary (not necessarily on a byte boundary). The first
element specifies the initial segment, the second element specifies the
following segment, and so on.

The following examples illustrate how binaries are constructed, or matched, and
how elements and tails are specified.

### Examples

_Example 1:_ A binary can be constructed from a set of constants or a string
literal:

```erlang
Bin11 = <<1, 17, 42>>,
Bin12 = <<"abc">>
```

This gives two binaries of size 3, with the following evaluations:

- [`binary_to_list(Bin11)`](`binary_to_list/1`) evaluates to `[1, 17, 42]`.
- [`binary_to_list(Bin12)`](`binary_to_list/1`) evaluates to `[97, 98, 99]`.

*Example 2:*Similarly, a binary can be constructed from a set of bound
variables:

```erlang
A = 1, B = 17, C = 42,
Bin2 = <<A, B, C:16>>
```

This gives a binary of size 4. Here, a _size expression_ is used for the
variable `C` to specify a 16-bits segment of `Bin2`.

[`binary_to_list(Bin2)`](`binary_to_list/1`) evaluates to `[1, 17, 00, 42]`.

_Example 3:_ A Bin can also be used for matching. `D`, `E`, and `F` are unbound
variables, and `Bin2` is bound, as in Example 2:

```erlang
<<D:16, E, F/binary>> = Bin2
```

This gives `D = 273`, `E = 00`, and F binds to a binary of size 1:
`binary_to_list(F) = [42]`.

_Example 4:_ The following is a more elaborate example of matching. Here,
`Dgram` is bound to the consecutive bytes of an IP datagram of IP protocol
version 4. The ambition is to extract the header and the data of the datagram:

```erlang
-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).

DgramSize = byte_size(Dgram),
case Dgram of
    <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
      ID:16, Flgs:3, FragOff:13,
      TTL:8, Proto:8, HdrChkSum:16,
      SrcIP:32,
      DestIP:32, RestDgram/binary>> when HLen>=5, 4*HLen=<DgramSize ->
        OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
        <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
    ...
end.
```

Here, the segment corresponding to the `Opts` variable has a _type modifier_,
specifying that `Opts` is to bind to a binary. All other variables have the
default type equal to unsigned integer.

An IP datagram header is of variable length. This length is measured in the
number of 32-bit words and is given in the segment corresponding to `HLen`. The
minimum value of `HLen` is 5. It is the segment corresponding to `Opts` that is
variable, so if `HLen` is equal to 5, `Opts` becomes an empty binary.

The tail variables `RestDgram` and `Data` bind to binaries, as all tail
variables do. Both can bind to empty binaries.

The match of `Dgram` fails if one of the following occurs:

- The first 4-bits segment of `Dgram` is not equal to 4.
- `HLen` is less than 5.
- The size of `Dgram` is less than `4*HLen`.

## Lexical Note

Notice that "`B=<<1>>`" will be interpreted as "`B =< <1>>`", which is a syntax
error. The correct way to write the expression is: `B = <<1>>`.

## Segments

Each segment has the following general syntax:

`Value:Size/TypeSpecifierList`

The `Size` or the `TypeSpecifier`, or both, can be omitted. Thus, the following
variants are allowed:

- `Value`
- `Value:Size`
- `Value/TypeSpecifierList`

Default values are used when specifications are missing. The default values are
described in [Defaults](#defaults).

The `Value` part is any expression, when used in binary construction. Used in
binary matching, the `Value` part must be a literal or a variable. For more
information about the `Value` part, see
[Constructing Binaries and Bitstrings](#constructing-binaries-and-bitstrings)
and [Matching Binaries](#matching-binaries).

The `Size` part of the segment multiplied by the unit in `TypeSpecifierList`
(described later) gives the number of bits for the segment. In construction,
`Size` is any expression that evaluates to an integer. In matching, `Size` must
be a constant expression or a variable.

The `TypeSpecifierList` is a list of type specifiers separated by hyphens.

- **Type** - The most commonly used types are `integer`, `float`, and `binary`.
  See
  [Bit Syntax Expressions in the Reference Manual](`e:system:expressions.md#bit-syntax-expressions`)
  for a complete description.

- **Signedness** - The signedness specification can be either `signed` or
  `unsigned`. Notice that signedness only matters for matching.

- **Endianness** - The endianness specification can be either `big`, `little`,
  or `native`. Native-endian means that the endian is resolved at load time, to
  be either big-endian or little-endian, depending on what is "native" for the
  CPU that the Erlang machine is run on.

- **Unit** - The unit size is given as `unit:IntegerLiteral`. The allowed range
  is 1-256. It is multiplied by the `Size` specifier to give the effective size
  of the segment. The unit size specifies the alignment for binary segments
  without size.

_Example:_

```text
X:4/little-signed-integer-unit:8
```

This element has a total size of 4\*8 = 32 bits, and it contains a signed
integer in little-endian order.

## Defaults

The default type for a segment is integer. The default type
does not depend on the value, even if the value is a literal. For example, the
default type in `<<3.14>>` is integer, not float.

The default `Size` depends on the type. For integer it is 8. For float it is 64.
For binary it is all of the binary. In matching, this default value is only
valid for the last element. All other binary elements in matching must have a
size specification.

The default unit depends on the type. For `integer`, `float`, and `bitstring` it
is 1. For binary it is 8.

The default signedness is `unsigned`.

The default endianness is `big`.

## Constructing Binaries and Bitstrings

This section describes the rules for constructing binaries using the bit syntax.
Unlike when constructing lists or tuples, the construction of a binary can fail
with a `badarg` exception.

There can be zero or more segments in a binary to be constructed. The expression
`<<>>` constructs a zero length binary.

Each segment in a binary can consist of zero or more bits. There are no
alignment rules for individual segments of type `integer` and `float`. For
binaries and bitstrings without size, the unit specifies the alignment. Since
the default alignment for the `binary` type is 8, the size of a binary segment
must be a multiple of 8 bits, that is, only whole bytes.

_Example:_

```erlang
<<Bin/binary,Bitstring/bitstring>>
```

The variable `Bin` must contain a whole number of bytes, because the `binary`
type defaults to `unit:8`. A `badarg` exception is generated if `Bin` consist
of, for example, 17 bits.

The `Bitstring` variable can consist of any number of bits, for example, 0, 1,
8, 11, 17, 42, and so on. This is because the default `unit` for bitstrings
is 1.

For clarity, it is recommended not to change the unit size for binaries.
Instead, use `binary` when you need byte alignment and `bitstring` when you need
bit alignment.

The following example successfully constructs a bitstring of 7 bits, provided
that all of X and Y are integers:

```erlang
<<X:1,Y:6>>
```

As mentioned earlier, segments have the following general syntax:

`Value:Size/TypeSpecifierList`

When constructing binaries, `Value` and `Size` can be any Erlang expression.
However, for syntactical reasons, both `Value` and `Size` must be enclosed in
parenthesis if the expression consists of anything more than a single literal or
a variable. The following gives a compiler syntax error:

```erlang
<<X+1:8>>
```

This expression must be rewritten into the following, to be accepted by the
compiler:

```erlang
<<(X+1):8>>
```

### Including Literal Strings

A literal string can be written instead of an element:

```erlang
<<"hello">>
```

This is syntactic sugar for the following:

```erlang
<<$h,$e,$l,$l,$o>>
```

## Matching Binaries

This section describes the rules for matching binaries, using the bit syntax.

There can be zero or more segments in a binary pattern. A binary pattern can
occur wherever patterns are allowed, including inside other patterns. Binary
patterns cannot be nested. The pattern `<<>>` matches a zero length binary.

Each segment in a binary can consist of zero or more bits. A segment of type
`binary` must have a size evenly divisible by 8 (or divisible by the unit size,
if the unit size has been changed). A segment of type `bitstring` has no
restrictions on the size. A segment of type `float` must have size 64 or 32.

As mentioned earlier, segments have the following general syntax:

`Value:Size/TypeSpecifierList`

When matching `Value`, value must be either a variable or an integer, or a
floating point literal. Expressions are not allowed.

`Size` must be a
[guard expression](`e:system:expressions.md#guard-expressions`), which can use
literals and previously bound variables. The following is not allowed:

```erlang
foo(N, <<X:N,T/binary>>) ->
   {X,T}.
```

The two occurrences of `N` are not related. The compiler will complain that the
`N` in the size field is unbound.

The correct way to write this example is as follows:

```erlang
foo(N, Bin) ->
   <<X:N,T/binary>> = Bin,
   {X,T}.
```

> #### Note {: .info }
>
> Before OTP 23, `Size` was restricted to be an integer or a variable bound to
> an integer.

### Binding and Using a Size Variable

There is one exception to the rule that a variable that is used as size must be
previously bound. It is possible to match and bind a variable, and use it as a
size within the same binary pattern. For example:

```erlang
bar(<<Sz:8,Payload:Sz/binary-unit:8,Rest/binary>>) ->
   {Payload,Rest}.
```

Here `Sz` is bound to the value in the first byte of the binary. `Sz` is then
used at the number of bytes to match out as a binary.

Starting in OTP 23, the size can be a guard expression:

```erlang
bar(<<Sz:8,Payload:((Sz-1)*8)/binary,Rest/binary>>) ->
   {Payload,Rest}.
```

Here `Sz` is the combined size of the header and the payload, so we will need to
subtract one byte to get the size of the payload.

### Getting the Rest of the Binary or Bitstring

To match out the rest of a binary, specify a binary field without size:

```erlang
foo(<<A:8,Rest/binary>>) ->
```

The size of the tail must be evenly divisible by 8.

To match out the rest of a bitstring, specify a field without size:

```erlang
foo(<<A:8,Rest/bitstring>>) ->
```

There are no restrictions on the number of bits in the tail.

## Appending to a Binary

Appending to a binary in an efficient way can be done as follows:

```erlang
triples_to_bin(T) ->
    triples_to_bin(T, <<>>).

triples_to_bin([{X,Y,Z} | T], Acc) ->
    triples_to_bin(T, <<Acc/binary,X:32,Y:32,Z:32>>);
triples_to_bin([], Acc) ->
    Acc.
```
