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
# Getting Started

## Example

The following example demonstrates the basic functionality used to run the
Erlang ASN.1 compiler.

Create a file named `People.asn` containing the following:

```text
People DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
  Person ::= SEQUENCE {
    name PrintableString,
    location INTEGER {home(0),field(1),roving(2)},
    age INTEGER OPTIONAL
  }
END
```

This file must be compiled before it can be used. The ASN.1 compiler checks that
the syntax is correct and that the text represents proper ASN.1 code before
generating an abstract syntax tree. The code generator then uses the abstract
syntax tree to generate code.

The generated Erlang files are placed in the current directory or in the
directory specified with option `{outdir,Dir}`.

The compiler can be called from the Erlang shell like this:

```erlang
1> asn1ct:compile("People", [ber]).
ok
```

Option `verbose` can be added to get information about the generated files:

```erlang
2> asn1ct:compile("People", [ber,verbose]).
Erlang ASN.1 compiling "People.asn"
--{generated,"People.asn1db"}--
--{generated,"People.hrl"}--
--{generated,"People.erl"}--
ok
```

ASN.1 module `People` is now accepted and the abstract syntax tree is saved in
file `People.asn1db`. The generated Erlang code is compiled using the Erlang
compiler and loaded into the Erlang runtime system. There is now an API for
`encode/2` and `decode/2` in module `People`, which is called like this:

```
'People':encode(<Type name>, <Value>)
```

or:

```
'People':decode(<Type name>, <Value>)
```

Assume that there is a network application that receives instances of the ASN.1
defined type `Person`, modifies, and sends them back again:

```erlang
receive
   {Port,{data,Bytes}} ->
       case 'People':decode('Person',Bytes) of
           {ok,P} ->
               {ok,Answer} = 'People':encode('Person',mk_answer(P)),
               Port ! {self(),{command,Answer}};
           {error,Reason} ->
               exit({error,Reason})
       end
    end,
```

In this example, a series of bytes is received from an external source and the
bytes are then decoded into a valid Erlang term. This was achieved with the call
`'People':decode('Person',Bytes)`, which returned an Erlang value of the ASN.1
type `Person`. Then an answer was constructed and encoded using
`'People':encode('Person',Answer)`, which takes an instance of a defined ASN.1
type and transforms it to a binary according to the BER or PER encoding rules.

The encoder and decoder can also be run from the shell:

```erlang
2> Rockstar = {'Person',"Some Name",roving,50}.
{'Person',"Some Name",roving,50}
3> {ok,Bin} = 'People':encode('Person',Rockstar).
{ok,<<243,17,19,9,83,111,109,101,32,78,97,109,101,2,1,2,
      2,1,50>>}
4> {ok,Person} = 'People':decode('Person',Bin).
{ok,{'Person',"Some Name",roving,50}}
```

### Module Dependencies

It is common that ASN.1 modules import defined types, values, and other entities
from another ASN.1 module.

Earlier versions of the ASN.1 compiler required that modules that were imported
from had to be compiled before the module that imported. This caused problems
when ASN.1 modules had circular dependencies.

Referenced modules are now parsed when the compiler finds an entity that is
imported. No code is generated for the referenced module. However, the compiled
modules rely on that the referenced modules are also compiled.

## ASN.1 Application User Interface

The `ASN.1` application provides the following two separate user interfaces:

- The module `asn1ct`, which provides the compile-time functions (including the
  compiler)
- The module `asn1rt_nif`, which provides the runtime functions for the ASN.1
  decoder for the BER back end

The reason for this division of the interfaces into compile-time and runtime is
that only runtime modules (`asn1rt_nif`) need to be loaded in an embedded system.

### Compile-Time Functions

The ASN.1 compiler can be started directly from the command line by the `erlc`
program. This is convenient when compiling many ASN.1 files from the command
line or when using Makefiles. Here some examples showing of how `erlc` can
compile ASN.1 modules:

```text
erlc Person.asn
erlc -bper Person.asn
erlc -bber ../Example.asn
erlc -o ../asnfiles -I ../asnfiles -I /usr/local/standards/asn1 Person.asn
```

Useful options for the ASN.1 compiler:

- **`-b[ber | per | uper | jer]`** - Choice of encoding rules. If omitted, `ber`
  is the default.

- **`-o OutDirectory`** - Where to put the generated files. Default is the
  current directory.

- **`-I IncludeDir`** - Where to search for `.asn1db` files and ASN.1 source
  specs to resolve references to other modules. This option can be repeated many
  times if there are several places to search in. The compiler searches the
  current directory first.

- **`+der`** - DER encoding rule. Only when using option `-bber`.

- **`+jer`** - Functions `jer_encode/2` and `jer_decode/2` for JSON encoding
  rules are generated together with functions for `ber` or `per`. Only to be
  used when the main encoding option is `-bber`, `-bper` or `-buper`.

- **`+maps`** - Use maps instead of records to represent the `SEQUENCE` and
  `SET` types. No `.hrl` files will be generated. See the section
  [Map representation for SEQUENCE and SET](asn1_getting_started.md#MAP_SEQ_SET)
  for more information.

- **`+asn1config`** - This functionality works together with option `ber`. It
  enables the specialized decodes, see section
  [Specialized Decode](asn1_spec.md).

- **`+undec_rest`** - A buffer that holds a message being decoded can also have
  trailing bytes. If those trailing bytes are important, they can be returned
  along with the decoded value by compiling the ASN.1 specification with option
  `+undec_rest`. The return value from the decoder is `{ok,Value,Rest}` where
  `Rest` is a binary containing the trailing bytes.

- **`+'Any Erlc Option'`** - Any option can be added to the Erlang compiler when
  compiling the generated Erlang files. Any option unrecognized by the ASN.1
  compiler is passed to the Erlang compiler.

For a complete description of `erlc`, see ERTS Reference Manual.

The compiler and other compile-time functions can also be started from the
Erlang shell. Here follows a brief description of the primary functions. For a
complete description of each function, see module `asn1ct` in the
[ASN.1 Reference Manual](`m:asn1ct`).

The compiler is started by `asn1ct:compile/1` with default options, or
`asn1ct:compile/2` if explicit options are given.

Example:

```text
asn1ct:compile("H323-MESSAGES").
```

This is equivalent to:

```text
asn1ct:compile("H323-MESSAGES", [ber]).
```

If PER encoding is wanted:

```text
asn1ct:compile("H323-MESSAGES", [per]).
```

The generic encode and decode functions can be called as follows:

```text
'H323-MESSAGES':encode('SomeChoiceType', {call,<<"octetstring">>}).
'H323-MESSAGES':decode('SomeChoiceType', Bytes).
```

### Runtime Functions

When an ASN.1 specification is compiled with option `ber`, the `asn1rt_nif`
module and the NIF library in `asn1/priv_dir` are needed at runtime.

By calling function `info/0` in a generated module, you get information about
which compiler options were used.

### Special Decode Functionality for JSON Encoding Rules (JER)

When using the JSON encoding rules, it is possible to call the
`decode/2` function in the following way with data that has already
been decoded by `json:decode/1`:

```erlang
SomeModule:decode(Type, {json_decoded, Decoded}).
```

Example:

```erlang
1> asn1ct:compile("People", [jer]).
ok
2> Rockstar = {'Person',"Vince Eclipse",roving,50}.
{'Person',"Vince Eclipse",roving,50}
3> {ok,Bin} = 'People':encode('Person', Rockstar).
{ok,<<"{\"name\":\"Vince Eclipse\",\"location\":2,\"age\":50}">>}
4> 'People':decode('Person', Bin).
{ok,{'Person',"Vince Eclipse",roving,50}}
5> 'People':decode('Person', {json_decoded,json:decode(Bin)}).
{ok,{'Person',"Vince Eclipse",roving,50}}

```

### Errors

Errors detected at compile-time are displayed on the screen together with line
numbers indicating where in the source file the respective error was detected.
If no errors are found, an Erlang ASN.1 module is created.

The runtime encoders and decoders execute within a `catch` and return `{ok, Data}`
or `{error, {asn1, Description}}` where `Description` is an Erlang term
describing the error.

Currently, `Description` looks like this: `{ErrorDescription, StackTrace}`.
Applications should not depend on the exact contents of `Description` as it
could change in the future.

## Multi-File Compilation

There are various reasons for using multi-file compilation:

- To choose the name for the generated module, for example, because you need to
  compile the same specs for different encoding rules.
- You want only one resulting module.

Specify which ASN.1 specs to compile in a module with extension `.set.asn`.
Choose a module name and provide the names of the ASN.1 specs. For example, if
you have the specs `File1.asn`, `File2.asn`, and `File3.asn`, your module
`MyModule.set.asn` looks as follows:

```text
File1.asn
File2.asn
File3.asn
```

If you compile with the following, the result is one merged module
`MyModule.erl` with the generated code from the three ASN.1 specs:

```text
% erlc MyModule.set.asn
```

## Note about tags

Tags used to be important for all users of ASN.1, because it was necessary to
to manually add tags to certain constructs in order for the ASN.1 specification to
be valid. Example of an old-style specification:

```erlang
Tags DEFINITIONS ::=
BEGIN
  Afters ::= CHOICE { cheese [0] IA5String,
                      dessert [1] IA5String }
END
```

Without the tags (the numbers in square brackets) the ASN.1 compiler refused to
compile the file.

In 1994 the global tagging mode `AUTOMATIC TAGS` was introduced. By putting
`AUTOMATIC TAGS` in the module header, the ASN.1 compiler automatically adds
tags when needed. The following is the same specification in `AUTOMATIC TAGS`
mode:

```erlang
Tags DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
  Afters ::= CHOICE { cheese IA5String,
                      dessert IA5String }
END
```

[](){: #ASN1Types }

## ASN.1 Types

This section describes the ASN.1 types including their functionality, purpose,
and how values are assigned in Erlang.

ASN.1 has both primitive and constructed types:

| Primitive Types                                                | Constructed Types                                            |
| -------------------------------------------------------------- | ------------------------------------------------------------ |
| [BOOLEAN](asn1_getting_started.md#boolean)                     | [SEQUENCE](asn1_getting_started.md#sequence)                 |
| [INTEGER](asn1_getting_started.md#integer)                     | [SET](asn1_getting_started.md#set)                           |
| [REAL](asn1_getting_started.md#real)                           | [CHOICE](asn1_getting_started.md#choice)                     |
| [NULL](asn1_getting_started.md#null)                           | [SET OF and SEQUENCE OF](asn1_getting_started.md#SOF)        |
| [ENUMERATED](asn1_getting_started.md#enumerated)               | [ANY](asn1_getting_started.md#ANY)                           |
| [BIT STRING](asn1_getting_started.md#bit-string)               | [ANY DEFINED BY](asn1_getting_started.md#ANY)                |
| [OCTET STRING](asn1_getting_started.md#octet-string)           | [EXTERNAL](asn1_getting_started.md#NegotiationTypes)         |
| [Character Strings](asn1_getting_started.md#character-strings) | [EMBEDDED PDV](asn1_getting_started.md#NegotiationTypes)     |
| [OBJECT IDENTIFIER](asn1_getting_started.md#object-identifier) | [CHARACTER STRING](asn1_getting_started.md#NegotiationTypes) |
| [Object Descriptor](asn1_getting_started.md#object-descriptor) |                                                              |
| [TIME Types](asn1_getting_started.md#The-TIME-types)           |                                                              |

_Table: Supported ASN.1 Types_

[](){: #TypeNameValue }

> #### Note {: .info }
>
> The values of each ASN.1 type have their own representation in Erlang, as
> described in the following sections. Users must provide these values for
> encoding according to the representation, as shown in the following example:

```text
Operational ::= BOOLEAN --ASN.1 definition
```

In Erlang code it can look as follows:

```erlang
Val = true,
{ok,Bytes} = MyModule:encode('Operational', Val),
```

### BOOLEAN

Booleans in ASN.1 express values that can be either `TRUE` or `FALSE`. The
meanings assigned to `TRUE` and `FALSE` are outside the scope of this text.

In ASN.1 it is possible to have:

```text
Operational ::= BOOLEAN
```

Assigning a value to type `Operational` in Erlang is possible by using the
following Erlang code:

```erlang
Myvar1 = true,
```

Thus, in Erlang the atoms `true` and `false` are used to encode a boolean value.

### INTEGER

An ASN.1 INTEGER is represented by an integer in Erlang.

The concept of subtyping can be applied to integers and to other ASN.1 types.
The details of subtyping are not explained here; for more information, see
X.680. Various syntaxes are allowed when defining a type as an integer:

```erlang
T1 ::= INTEGER
T2 ::= INTEGER (-2..7)
T3 ::= INTEGER (0..MAX)
T4 ::= INTEGER (0<..MAX)
T5 ::= INTEGER (MIN<..-99)
T6 ::= INTEGER {red(0),blue(1),white(2)}
```

The Erlang representation of an ASN.1 `INTEGER` is an integer or an atom if a
`Named Number List` (see `T6` in the previous list) is specified.

The following is an example of Erlang code that assigns values for the types in
the previous list:

```erlang
T1value = 0,
T2value = 6,
T6value1 = blue,
T6value2 = 0,
T6value3 = white
```

These Erlang variables are now bound to valid instances of ASN.1 defined types.
This style of value can be passed directly to the encoder for transformation
into a series of bytes.

The decoder returns an atom if the value corresponds to a symbol in the
`Named Number List`.

### REAL

The following ASN.1 type is used for real numbers:

```text
R1 ::= REAL
```

It is assigned a value in Erlang as follows:

```text
R1value1 = "2.14",
R1value2 = {256,10,-2}
```

In the last line, notice that the tuple \{256,10,-2\} is the real number 2.56 in
a special notation, which encodes faster than simply stating the number as
`"2.56"`. The arity three tuple is `{Mantissa,Base,Exponent}`, that is,
`Mantissa * Base^Exponent`.

The following special values are also recognized:

```text
R1value3 = 0,
R1value4 = 'PLUS-INFINITY',
R1value5 = 'MINUS-INFINITY'
```

### NULL

The type `NULL` is suitable where supply and recognition of a value is important
but the actual value is not.

```text
Notype ::= NULL
```

This type is assigned in Erlang as follows:

```text
N1 = 'NULL',
```

The actual value is the quoted atom `'NULL'`.

### ENUMERATED

The type `ENUMERATED` can be used when the value you want to describe can only
take one of a set of predefined values. Example:

```text
DaysOfTheWeek ::= ENUMERATED {
    sunday(1),monday(2),tuesday(3),
    wednesday(4),thursday(5),friday(6),saturday(7) }
```

For example, to assign a weekday value in Erlang, use the same atom as in the
`Enumerations` of the type definition:

```text
Day1 = saturday,
```

The enumerated type is similar to an integer type, when defined with a set of
predefined values. The difference is that an enumerated type can only have
specified values, whereas an integer can have any value.

### BIT STRING

The type `BIT STRING` can be used to model information that is made up of
arbitrary length series of bits. It is intended to be used for selection of
flags, not for binary files.

In ASN.1, `BIT STRING` definitions can look as follows:

```erlang
Bits1 ::= BIT STRING
Bits2 ::= BIT STRING {foo(0),bar(1),gnu(2),gnome(3),punk(14)}
```

The following two notations are available for representation of `BIT STRING`
values in Erlang and as input to the encode functions:

1. A bitstring. By default, a `BIT STRING` with no symbolic names is decoded to
   an Erlang bitstring.
1. A list of atoms corresponding to atoms in the `NamedBitList` in the
   `BIT STRING` definition. A `BIT STRING` with symbolic names is always decoded
   to the format shown in the following example:

```text
Bits1Val1 = <<0:1,1:1,0:1,1:1,1:1>>,
Bits2Val1 = [gnu,punk],
Bits2Val2 = <<2#1110:4>>,
Bits2Val3 = [bar,gnu,gnome],
```

`Bits2Val2` and `Bits2Val3` denote the same value.

`Bits2Val1` is assigned symbolic values. The assignment means that the bits
corresponding to `gnu` and `punk`, that is, bits 2 and 14 are set to 1, and the
rest are set to 0. The symbolic values are shown as a list of values. If a named
value, which is not specified in the type definition, is shown, a runtime error
occurs.

`BIT STRING`s can also be subtyped with, for example, a `SIZE` specification:

```text
Bits3 ::= BIT STRING (SIZE(0..31))
```

This means that no bit higher than 31 can be set.

#### Deprecated Representations for BIT STRING

In addition to the representations described earlier, the following deprecated
representations are available if the specification has been compiled with option
`legacy_erlang_types`:

1. Aa a list of binary digits (0 or 1). This format is accepted as input to the
   encode functions, and a `BIT STRING` is decoded to this format if option
   `legacy_bit_string` is given.
1. As `{Unused,Binary}` where `Unused` denotes how many trailing zero-bits 0-7
   that are unused in the least significant byte in `Binary`. This format is
   accepted as input to the encode functions, and a `BIT STRING` is decoded to
   this format if `compact_bit_string` has been given.
1. As a hexadecimal number (or an integer). Avoid this as it is easy to
   misinterpret a `BIT STRING` value in this format.

### OCTET STRING

`OCTET STRING` is the simplest of all ASN.1 types. `OCTET STRING` only moves or
transfers, for example, binary files or other unstructured information complying
with two rules: the bytes consist of octets and encoding is not required.

It is possible to have the following ASN.1 type definitions:

```erlang
O1 ::= OCTET STRING
O2 ::= OCTET STRING (SIZE(28))
```

With the following example assignments in Erlang:

```text
O1Val = <<17,13,19,20,0,0,255,254>>,
O2Val = <<"must be exactly 28 chars....">>,
```

By default, an `OCTET STRING` is always represented as an Erlang binary. If the
specification has been compiled with option `legacy_erlang_types`, the encode
functions accept both lists and binaries, and the decode functions decode an
`OCTET STRING` to a list.

### Character Strings

ASN.1 supports a wide variety of character sets. The main difference between an
`OCTET STRING` and a character string is that the `OCTET STRING` has no imposed
semantics on the bytes delivered.

However, when using, for example, `IA5String` (which closely resembles ASCII),
byte 65 (in decimal notation) _means_ character 'A'.

For example, if a defined type is to be a VideotexString and an octet is
received with the unsigned integer value `X`, the octet is to be interpreted as
specified in standard ITU-T T.100, T.101.

The ASN.1 compiler does not determine the correct interpretation of
each BER string octet value with different character strings. The application is
responsible for interpretation of octets. Therefore, from the BER string point
of view, octets are very similar to character strings and are compiled in the
same way.

When PER is used, there is a significant difference in the encoding scheme
for `OCTET STRING`s and other strings. The constraints specified for a type
are especially important for PER, because they affect the encoding.

Examples:

```erlang
Digs ::= NumericString (SIZE(1..3))
TextFile ::= IA5String (SIZE(0..64000))
```

The corresponding Erlang assignments:

```c
DigsVal1 = "456",
DigsVal2 = "123",
TextFileVal1 = "abc...xyz...",
TextFileVal2 = [88,76,55,44,99,121 .......... a lot of characters here ....]
```

The Erlang representation for `BMPString` and `UniversalString` is either a list
of ASCII values or a list of quadruples. The quadruple representation associates
to the Unicode standard representation of characters. The ASCII characters are
all represented by quadruples beginning with three zeros like `{0,0,0,65}` for
character 'A'. When decoding a value for these strings, the result is a list of
quadruples, or integers when the value is an ASCII character.

The following example shows how it works. Assume the following specification is
in file `PrimStrings.asn1`:

```text
PrimStrings DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
   BMP ::= BMPString
END
```

Encoding and decoding some strings:

```erlang
1> asn1ct:compile('PrimStrings', [ber]).
ok
2> {ok,Bytes1} = 'PrimStrings':encode('BMP', [{0,0,53,53},{0,0,45,56}]).
{ok,<<30,4,53,54,45,56>>}
3> 'PrimStrings':decode('BMP', Bytes1).
{ok,[{0,0,53,53},{0,0,45,56}]}
4> {ok,Bytes2} = 'PrimStrings':encode('BMP', [{0,0,53,53},{0,0,0,65}]).
{ok,<<30,4,53,53,0,65>>}
5> 'PrimStrings':decode('BMP', Bytes2).
{ok,[{0,0,53,53},65]}
6> {ok,Bytes3} = 'PrimStrings':encode('BMP', "BMP string").
{ok,<<30,20,0,66,0,77,0,80,0,32,0,115,0,116,0,114,0,105,0,110,0,103>>}
7> 'PrimStrings':decode('BMP', Bytes3).
{ok,"BMP string"}
```

Type `UTF8String` is represented as a UTF-8 encoded binary in Erlang. Such
binaries can be created directly using the binary syntax or by converting from a
list of Unicode code points using function `unicode:characters_to_binary/1`.

The following shows examples of how UTF-8 encoded binaries can be created and
manipulated:

```erlang
1> Gs = "Мой маленький Гном".
[1052,1086,1081,32,1084,1072,1083,1077,1085,1100,1082,1080,
 1081,32,1043,1085,1086,1084]
2> Gbin = unicode:characters_to_binary(Gs).
<<208,156,208,190,208,185,32,208,188,208,176,208,187,208,
  181,208,189,209,140,208,186,208,184,208,185,32,208,147,
  208,...>>
3> Gbin = <<"Мой маленький Гном"/utf8>>.
<<208,156,208,190,208,185,32,208,188,208,176,208,187,208,
  181,208,189,209,140,208,186,208,184,208,185,32,208,147,
  208,...>>
4> Gs = unicode:characters_to_list(Gbin).
[1052,1086,1081,32,1084,1072,1083,1077,1085,1100,1082,1080,
 1081,32,1043,1085,1086,1084]
```

For details, see the `m:unicode` module in STDLIB.

In the following example, this ASN.1 specification is used:

```text
UTF DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
   UTF ::= UTF8String
END
```

Encoding and decoding a string with Unicode characters:

```erlang
5> asn1ct:compile('UTF', [ber]).
ok
6> {ok,Bytes1} = 'UTF':encode('UTF', <<"Гном"/utf8>>).
{ok,<<12,8,208,147,208,189,208,190,208,188>>}
7> {ok,Bin1} = 'UTF':decode('UTF', Bytes1).
{ok,<<208,147,208,189,208,190,208,188>>}
8> io:format("~ts\n", [Bin1]).
Гном
ok
9> unicode:characters_to_list(Bin1).
[1043,1085,1086,1084]
```

### OBJECT IDENTIFIER

The type `OBJECT IDENTIFIER` is used whenever a unique identity is required. An
ASN.1 module, a transfer syntax, and so on, is identified with an
`OBJECT IDENTIFIER`. Assume the following example:

```erlang
Oid ::= OBJECT IDENTIFIER
```

Therefore, the following example is a valid Erlang instance of type `Oid`:

```text
OidVal1 = {1,2,55},
```

The `OBJECT IDENTIFIER` value is a tuple with the consecutive integer values.

The first value is limited to the values 0, 1, or 2. The second value must be in
the range 0 through 39 when the first value is 0 or 1.

The `OBJECT IDENTIFIER` is an important type and it is widely used within
different standards to identify various objects uniquely. Dubuisson: _ASN.1 -
Communication Between Heterogeneous Systems_ includes an easy-to-understand
description of the use of `OBJECT IDENTIFIER`.

### Object Descriptor

Values of this type can be assigned a value as an ordinary string as follows:

```text
"This is the value of an Object descriptor"
```

[](){: #The-TIME-types }

### TIME Types

Two time types are defined within ASN.1: Generalized Time and Universal Time
Coordinated (UTC). Both are assigned a value as an ordinary string within double
quotes, for example, `"19820102070533.8"`.

For DER encoding, the compiler does not check the validity of the time values.
The DER requirements upon those strings are regarded as a matter for the
application to fulfill.

### SEQUENCE

The structured types of ASN.1 are constructed from other types in a manner
similar to the concepts of arrays and structs in C.

A `SEQUENCE` in ASN.1 is comparable with a struct in C and a record in Erlang. A
`SEQUENCE` can be defined as follows:

```erlang
Pdu ::= SEQUENCE {
   a INTEGER,
   b REAL,
   c OBJECT IDENTIFIER,
   d NULL }
```

This is a 4-component structure called `Pdu`. By default, a `SEQUENCE` is
represented by a record in Erlang. It can also be represented as a map; see
[Map representation for SEQUENCE and SET](asn1_getting_started.md#MAP_SEQ_SET).
For each `SEQUENCE` and `SET` in an ASN.1 module an Erlang record declaration is
generated. For `Pdu`, a record like the following is defined:

```erlang
-record('Pdu', {a, b, c, d}).
```

The record declarations for a module `M` are placed in a separate `M.hrl` file.

Values can be assigned in Erlang as follows:

```erlang
MyPdu = #'Pdu'{a=22,b=77.99,c={0,1,2,3,4},d='NULL'}.
```

The decode functions return a record as result when decoding a `SEQUENCE` or a
`SET`.

A `SEQUENCE` and a `SET` can contain a component with a `DEFAULT` keyword
followed by the actual value, which is the default value. The `DEFAULT` keyword
means that the application doing the encoding can omit encoding of the value,
which results in fewer bytes to send to the receiving application.

An application can use the atom `asn1_DEFAULT` to indicate that the encoding is
to be omitted for that position in the `SEQUENCE`.

Depending on the encoding rules, the encoder can also compare the given value to
the default value and automatically omit the encoding if the values are equal.
How much effort the encoder makes to compare the values depends on the encoding
rules. The DER encoding rules forbid encoding a value equal to the default
value, so it has a more thorough and time-consuming comparison than the encoders
for the other encoding rules.

In the following example, this ASN.1 specification is used:

```text
File DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
Seq1 ::= SEQUENCE {
    a INTEGER DEFAULT 1,
    b Seq2 DEFAULT {aa TRUE, bb 15}
}

Seq2 ::= SEQUENCE {
    aa BOOLEAN,
    bb INTEGER
}

Seq3 ::= SEQUENCE {
    bs BIT STRING {a(0), b(1), c(2)} DEFAULT {a, c}
}
END
```

Example where the BER encoder is able to omit encoding of the default values:

```erlang
1> asn1ct:compile('File', [ber]).
ok
2> 'File':encode('Seq1', {'Seq1',asn1_DEFAULT,asn1_DEFAULT}).
{ok,<<48,0>>}
3> 'File':encode('Seq1', {'Seq1',1,{'Seq2',true,15}}).
{ok,<<48,0>>}
```

Example with a named `BIT STRING` where the BER encoder does not omit the
encoding:

```erlang
4> 'File':encode('Seq3', {'Seq3',asn1_DEFAULT).
{ok,<<48,0>>}
5> 'File':encode('Seq3', {'Seq3',<<16#101:3>>).
{ok,<<48,4,128,2,5,160>>}
```

The DER encoder omits the encoding for the same `BIT STRING`:

```erlang
6> asn1ct:compile('File', [ber,der]).
ok
7> 'File':encode('Seq3', {'Seq3',asn1_DEFAULT).
{ok,<<48,0>>}
8> 'File':encode('Seq3', {'Seq3',<<16#101:3>>).
{ok,<<48,0>>}
```

### SET

In Erlang, the `SET` type is used exactly as `SEQUENCE`. Notice that if BER or
DER encoding rules are used, decoding a `SET` is slower than decoding a
`SEQUENCE` because the components must be sorted.

### Extensibility for SEQUENCE and SET

When a `SEQUENCE` or `SET` contains an extension marker and extension components
as the following, the type can get more components in newer versions of the
ASN.1 spec:

```text
SExt ::= SEQUENCE {
           a INTEGER,
           ...,
           b BOOLEAN }
```

In this case it has got a new component `b`. Thus, incoming messages that are
decoded can have more or fever components than this one.

The component `b` is treated as an original component when encoding a message.
In this case, as it is not an optional element, it must be encoded.

During decoding, the `b` field of the record gets the decoded value of the `b`
component, if present, otherwise the value `asn1_NOVALUE`.

[](){: #MAP_SEQ_SET }

### Map representation for SEQUENCE and SET

If the ASN.1 module has been compiled with option `maps`, the types `SEQUENCE`
and `SET` are represented as maps.

In the following example, this ASN.1 specification is used:

```text
File DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
Seq1 ::= SEQUENCE {
    a INTEGER DEFAULT 42,
    b BOOLEAN OPTIONAL,
    c IA5String
}
END
```

Optional fields are to be omitted from the map if they have no value:

```erlang
1> asn1ct:compile('File', [per,maps]).
ok
2> {ok,E} = 'File':encode('Seq1', #{a=>0,c=>"string"}).
{ok,<<128,1,0,6,115,116,114,105,110,103>>}
```

When decoding, optional fields will be omitted from the map:

```erlang
3> 'File':decode('Seq1', E).
{ok,#{a => 0,c => "string"}}
```

Default values can be omitted from the map:

```erlang
4> {ok,E2} = 'File':encode('Seq1', #{c=>"string"}).
{ok,<<0,6,115,116,114,105,110,103>>}
5> 'File':decode('Seq1', E2).
{ok,#{a => 42,c => "string"}}
```

> #### Note {: .info }
>
> It is not allowed to use the atoms `asn1_VALUE` and `asn1_DEFAULT` with maps.

### CHOICE

The type `CHOICE` is a space saver and is similar to the concept of union in
C.

Assume the following:

```text
SomeModuleName DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
T ::= CHOICE {
        x REAL,
        y INTEGER,
        z OBJECT IDENTIFIER }
END
```

It is then possible to assign values as follows:

```erlang
TVal1 = {y,17},
TVal2 = {z,{0,1,2}},
```

A `CHOICE` value is always represented as the tuple `{ChoiceAlternative, Val}`
where `ChoiceAlternative` is an atom denoting the selected choice alternative.

#### Extensible CHOICE

When a `CHOICE` contains an extension marker and the decoder detects an unknown
alternative of the `CHOICE`, the value is represented as follows:

```text
{asn1_ExtAlt, BytesForOpenType}
```

Here `BytesForOpenType` is a list of bytes constituting the encoding of the
"unknown" `CHOICE` alternative.

[](){: #SOF }

### SET OF and SEQUENCE OF

The types `SET OF` and `SEQUENCE OF` correspond to the concept of an array in
several programming languages. The Erlang syntax for both types is
straightforward, for example:

```text
Arr1 ::= SET SIZE (5) OF INTEGER (4..9)
Arr2 ::= SEQUENCE OF OCTET STRING
```

In Erlang the following can apply:

```text
Arr1Val = [4,5,6,7,8],
Arr2Val = ["abc",[14,34,54],"Octets"],
```

Notice that the definition of type `SET OF` implies that the order of the
components is undefined, but in practice there is no difference between `SET OF`
and `SEQUENCE OF`. The ASN.1 compiler for Erlang does not randomize the order of
the `SET OF` components before encoding.

However, for a value of type `SET OF`, the DER encoding format requires the
elements to be sent in ascending order of their encoding, which implies an
expensive sorting procedure in runtime. Therefore it is recommended to use
`SEQUENCE OF` instead of `SET OF` if possible.

[](){: #ANY }

### ANY and ANY DEFINED BY

The types `ANY` and `ANY DEFINED BY` have been removed from the standard
since 1994. It is recommended not to use these types any more. They can,
however, exist in some old ASN.1 modules. The idea with this type was to leave a
"hole" in a definition where it was possible to put unspecified data of any
kind, even non-ASN.1 data.

A value of this type is encoded as an `open type`.

Instead of `ANY` and `ANY DEFINED BY`, it is recommended to use
information object classes, table constraints, and parameterization. In
particular the construct `TYPE-IDENTIFIER.@Type` accomplishes the same as the
deprecated `ANY`.

Also see [Information objects](asn1_getting_started.md#Information-Object).

[](){: #NegotiationTypes }

### EXTERNAL, EMBEDDED PDV, and CHARACTER STRING

The types `EXTERNAL`, `EMBEDDED PDV`, and `CHARACTER STRING` are used in
presentation layer negotiation. They are encoded according to their associated
type, see X.680.

The type `EXTERNAL` had a slightly different associated type before 1994. X.691
states that encoding must follow the older associated type. So, generated
encode/decode functions convert values of the newer format to the older format
before encoding. This implies that it is allowed to use `EXTERNAL` type values
of either format for encoding. Decoded values are always returned in the newer
format.

### Embedded Named Types

The structured types previously described can have other named types as their
components. The general syntax to assign a value to component `C` of a named
ASN.1 type `T` in Erlang is the record syntax `#'T'{'C'=Value}`. Here `Value`
can be a value of yet another type `T2`, for example:

```text
EmbeddedExample DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
B ::= SEQUENCE {
        a Arr1,
        b T }

Arr1 ::= SET SIZE (5) OF INTEGER (4..9)

T ::= CHOICE {
        x REAL,
        y INTEGER,
        z OBJECT IDENTIFIER }
        END
```

`SEQUENCE` `b` can be encoded as follows in Erlang:

```erlang
1> 'EmbeddedExample':encode('B', {'B',[4,5,6,7,8],{x,"7.77"}}).
{ok,<<5,56,0,8,3,55,55,55,46,69,45,50>>}
```

## Naming of Records in .hrl Files

When the option `maps` is given, no `.hrl` files will be generated. The rest of
this section describes the behavior of the compiler when `maps` is not used.

When an ASN.1 specification is compiled, all defined types of type `SET` or
`SEQUENCE` result in a corresponding record in the generated `.hrl` file. This
is because the values for `SET` and `SEQUENCE` are represented as records by
default.

Some special cases of this functionality are presented in the next section.

### Embedded Structured Types

In ASN.1 it is also possible to have components that are themselves structured
types. For example, it is possible to have the following:

```text
Emb ::= SEQUENCE {
    a SEQUENCE OF OCTET STRING,
    b SET {
       a INTEGER,
       b INTEGER DEFAULT 66},
    c CHOICE {
       a INTEGER,
       b FooType } }

FooType ::= [3] VisibleString
```

The following records are generated because of type `Emb`:

```erlang
-record('Emb,{a, b, c}).
-record('Emb_b',{a, b = asn1_DEFAULT}). % the embedded SET type
```

Values of type `Emb` can be assigned as follows:

```erlang
V = #'Emb'{a=["qqqq",[1,2,255]],
           b = #'Emb_b'{a=99},
           c ={b,"Can you see this"}}.
```

For an embedded type of type `SEQUENCE`/`SET` in a `SEQUENCE`/`SET`, the record
name is extended with an underscore and the component name. If the embedded
structure is deeper with the `SEQUENCE`, `SET`, or `CHOICE` types in the line,
each component name/alternative name is added to the record name.

Example:

```text
Seq ::= SEQUENCE{
    a CHOICE{
        b SEQUENCE {
           c  INTEGER
        }
    }
}
```

This results in the following record:

```erlang
-record('Seq_a_b',{c}).
```

If the structured type has a component with an embedded `SEQUENCE OF`/`SET OF`
which embedded type in turn is a `SEQUENCE`/`SET`, it gives a record with the
`SEQUENCE OF`/`SET OF` addition as in the following example:

```text
Seq ::= SEQUENCE {
    a SEQUENCE OF SEQUENCE {
           b
               }
    c SET OF SEQUENCE {
           d
               }
}
```

This results in the following records:

```erlang
-record('Seq_a_SEQOF'{b}).
-record('Seq_c_SETOF'{d}).
```

A parameterized type is to be considered as an embedded type. Each time such a
type is referenced, an instance of it is defined. Thus, in the following example
a record with name `'Seq_b'` is generated in the `.hrl` file and is used to hold
values:

```text
Seq ::= SEQUENCE {
    b PType{INTEGER}
}

PType{T} ::= SEQUENCE{
    id T
}
```

### Recursive Types

Types that refer to themselves are called recursive types. Example:

```erlang
Rec ::= CHOICE {
     nothing NULL,
     something SEQUENCE {
          a INTEGER,
          b OCTET STRING,
          c Rec }}
```

This is allowed in ASN.1 and the ASN.1-to-Erlang compiler supports this
recursive type. A value for this type is assigned in Erlang as follows:

```erlang
V = {something,#'Rec_something'{a = 77,
                                b = "some octets here",
                                c = {nothing,'NULL'}}}.
```

## ASN.1 Values

Values can be assigned to an ASN.1 type within the ASN.1 code itself, as opposed
to the actions in the previous section where a value was assigned to an ASN.1
type in Erlang. The full value syntax of ASN.1 is supported and X.680 describes
in detail how to assign values in ASN.1. A short example:

```erlang
TT ::= SEQUENCE {
   a INTEGER,
   b SET OF OCTET STRING }

tt TT ::= {a 77,b {"kalle","kula"}}
```

The value defined here can be used in several ways. It can, for example, be used
as the value in some `DEFAULT` component:

```text
SS ::= SET {
    s OBJECT IDENTIFIER,
    val TT DEFAULT tt }
```

It can also be used from inside an Erlang program. If this ASN.1 code is defined
in ASN.1 module `Values`, the ASN.1 value `tt` can be reached from Erlang as a
function call to `'Values':tt()` as in the following example:

```erlang
1> Val = 'Values':tt().
{'TT',77,["kalle","kula"]}
2> {ok,Bytes} = 'Values':encode('TT',Val).
{ok,<<48,18,128,1,77,161,13,4,5,107,97,108,108,101,4,4,
      107,117,108,97>>}
4> 'Values':decode('TT',Bytes).
{ok,{'TT',77,["kalle","kula"]}}
5>
```

This example shows that a function is generated by the compiler that returns a
valid Erlang representation of the value, although the value is of a complex
type.

Furthermore, if the option `maps` is not used, a macro is generated for each
value in the `.hrl` file. So, the defined value `tt` can also be extracted by
`?tt` in application code.

## Macros

The type `MACRO` is not supported. It is no longer part of the ASN.1 standard.

[](){: #Information-Object }

## ASN.1 Information Objects (X.681)

Information Object Classes, Information Objects, and Information Object Sets (in
the following called classes, objects, and object sets, respectively) are
defined in the standard definition X.681. Only a brief explanation is given
here.

These constructs makes it possible to define open types, that is, values of that
type can be of any ASN.1 type. Also, relationships can be defined between
different types and values, as classes can hold types, values, objects, object
sets, and other classes in their fields. A class can be defined in ASN.1 as
follows:

```text
GENERAL-PROCEDURE ::= CLASS {
      &Message,
      &Reply               OPTIONAL,
      &Error               OPTIONAL,
      &id          PrintableString UNIQUE
}
WITH SYNTAX {
      NEW MESSAGE     &Message
      [REPLY           &Reply]
      [ERROR           &Error]
      ADDRESS          &id
}
```

An object is an instance of a class. An object set is a set containing objects
of a specified class. A definition can look as follows:

```text
object1 GENERAL-PROCEDURE ::= {
    NEW MESSAGE      PrintableString
    ADDRESS          "home"
}

object2 GENERAL-PROCEDURE ::= {
    NEW MESSAGE INTEGER
    ERROR INTEGER
    ADDRESS "remote"
}
```

The object `object1` is an instance of the class `GENERAL-PROCEDURE` and has one
type field and one fixed type value field. The object `object2` has also an
optional field `ERROR`, which is a type field. The field `ADDRESS` is a `UNIQUE`
field. Objects in an object set must have unique values in their `UNIQUE` field,
as in `GENERAL-PROCEDURES`:

```text
GENERAL-PROCEDURES GENERAL-PROCEDURE ::= {
    object1 | object2}
```

You cannot encode a class, object, or object set, only refer to it when defining
other ASN.1 entities. Typically you refer to a class as well as to object sets
by table constraints and component relation constraints (X.682) in ASN.1 types,
as in the following:

```erlang
StartMessage  ::= SEQUENCE {
    msgId  GENERAL-PROCEDURE.&id  ({GENERAL-PROCEDURES}),
    content GENERAL-PROCEDURE.&Message ({GENERAL-PROCEDURES}{@msgId}),
    }
```

In type `StartMessage`, the constraint following field `content` tells that in a
value of type `StartMessage` the value in field `content` must come from the
same object that is chosen by field `msgId`.

So, the value `#'StartMessage'{msgId="home",content="Any Printable String"}` is
legal to encode as a `StartMessage` value. However, the value
`#'StartMessage'{msgId="remote", content="Some String"}` is illegal as the
constraint in `StartMessage` tells that when you have chosen a value from a
specific object in object set `GENERAL-PROCEDURES` in field `msgId`, you must
choose a value from that same object in the content field too. In this second
case, it is to be any `INTEGER` value.

`StartMessage` can in field `content` be encoded with a value of any type that
an object in object set `GENERAL-PROCEDURES` has in its `NEW MESSAGE` field.
This field refers to a type field `&Message` in the class. Field `msgId` is
always encoded as a `PrintableString`, as the field refers to a fixed type in
the class.

In practice, object sets are usually declared to be extensible so that more
objects can be added to the set later. Extensibility is indicated as follows:

```text
GENERAL-PROCEDURES GENERAL-PROCEDURE ::= {
    object1 | object2, ...}
```

When decoding a type that uses an extensible set constraint, it is always
possible that the value in field `UNIQUE` is unknown (that is, the type has been
encoded with a later version of the ASN.1 specification). The unencoded data is
then returned wrapped in a tuple as follows:

```text
{asn1_OPENTYPE,Binary}
```

Here `Binary` is an Erlang binary that contains the encoded data. (If option
`legacy_erlang_types` has been given, only the binary is returned.)

## Parameterization (X.683)

Parameterization, which is defined in X.683, can be used when defining types,
values, value sets, classes, objects, or object sets. A part of a definition can
be supplied as a parameter. For example, if a `Type` is used in a definition
with a certain purpose, you want the type name to express the intention. This
can be done with parameterization.

When many types (or another ASN.1 entity) only differ in some minor cases, but
the structure of the types is similar, only one general type can be defined and
the differences can be supplied through parameters.

Example of use of parameterization:

```text
General{Type} ::= SEQUENCE
{
     number     INTEGER,
     string     Type
}

T1 ::= General{PrintableString}

T2 ::= General{BIT STRING}
```

An example of a value that can be encoded as type `T1` is `{12,"hello"}`.

Notice that the compiler does not generate encode/decode functions for
parameterized types, only for the instances of the parameterized types.
Therefore, if a file contains the types `General{}`, `T1`, and `T2` as in the
previous example, encode/decode functions are only generated for `T1` and `T2`.
