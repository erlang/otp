<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Using Unicode in Erlang

## Unicode Implementation

Implementing support for Unicode character sets is an ongoing process. The
Erlang Enhancement Proposal (EEP) 10 outlined the basics of Unicode support and
specified a default encoding in binaries that all Unicode-aware modules are to
handle in the future.

Here is an overview what has been done so far:

- The functionality described in EEP10 was implemented in Erlang/OTP R13A.
- Erlang/OTP R14B01 added support for Unicode filenames, but it was not complete
  and was by default disabled on platforms where no guarantee was given for the
  filename encoding.
- With Erlang/OTP R16A came support for UTF-8 encoded source code, with
  enhancements to many of the applications to support both Unicode encoded
  filenames and support for UTF-8 encoded files in many circumstances. Most
  notable is the support for UTF-8 in files read by `file:consult/1`, release
  handler support for UTF-8, and more support for Unicode character sets in the
  I/O system.
- In Erlang/OTP 17.0, the encoding default for Erlang source files was switched
  to UTF-8.
- In Erlang/OTP 20.0, atoms and function can contain Unicode characters. Module
  names, application names, and node names are still restricted to the ISO
  Latin-1 range.

  Support was added for normalizations forms in `unicode` and the `string`
  module now handles utf8-encoded binaries.

This section outlines the current Unicode support and gives some recipes for
working with Unicode data.

## Understanding Unicode

Experience with the Unicode support in Erlang has made it clear that
understanding Unicode characters and encodings is not as easy as one would
expect. The complexity of the field and the implications of the standard require
thorough understanding of concepts rarely before thought of.

Also, the Erlang implementation requires understanding of concepts that were
never an issue for many (Erlang) programmers. To understand and use Unicode
characters requires that you study the subject thoroughly, even if you are an
experienced programmer.

As an example, contemplate the issue of converting between upper and lower case
letters. Reading the standard makes you realize that there is not a simple one
to one mapping in all scripts, for example:

- In German, the letter "ß" (sharp s) is in lower case, but the uppercase
  equivalent is "SS".
- In Greek, the letter "Σ" has two different lowercase forms, "ς" in word-final
  position and "σ" elsewhere.
- In Turkish, both dotted and dotless "i" exist in lower case and upper case
  forms.
- Cyrillic "I" has usually no lowercase form.
- Languages with no concept of upper case (or lower case).

So, a conversion function must know not only one character at a time, but
possibly the whole sentence, the natural language to translate to, the
differences in input and output string length, and so on. Erlang/OTP has
currently no Unicode `uppercase`/`lowercase` functionality with language
specific handling, but publicly available libraries address these issues.

Another example is the accented characters, where the same glyph has two
different representations. The Swedish letter "ö" is one example. The Unicode
standard has a code point for it, but you can also write it as "o" followed by
"U+0308" (Combining Diaeresis, with the simplified meaning that the last letter
is to have "¨" above). They have the same glyph, user perceived character. They
are for most purposes the same, but have different representations. For example,
MacOS X converts all filenames to use Combining Diaeresis, while most other
programs (including Erlang) try to hide that by doing the opposite when, for
example, listing directories. However it is done, it is usually important to
normalize such characters to avoid confusion.

The list of examples can be made long. One need a kind of knowledge that was not
needed when programs only considered one or two languages. The complexity of
human languages and scripts has certainly made this a challenge when
constructing a universal standard. Supporting Unicode properly in your program
will require effort.

## What Unicode Is

Unicode is a standard defining code points (numbers) for all known, living or
dead, scripts. In principle, every symbol used in any language has a Unicode
code point. Unicode code points are defined and published by the Unicode
Consortium, which is a non-profit organization.

Support for Unicode is increasing throughout the world of computing, as the
benefits of one common character set are overwhelming when programs are used in
a global environment. Along with the base of the standard, the code points for
all the scripts, some _encoding standards_ are available.

It is vital to understand the difference between encodings and Unicode
characters. Unicode characters are code points according to the Unicode
standard, while the encodings are ways to represent such code points. An
encoding is only a standard for representation. UTF-8 can, for example, be used
to represent a very limited part of the Unicode character set (for example
ISO-Latin-1) or the full Unicode range. It is only an encoding format.

As long as all character sets were limited to 256 characters, each character
could be stored in one single byte, so there was more or less only one practical
encoding for the characters. Encoding each character in one byte was so common
that the encoding was not even named. With the Unicode system there are much
more than 256 characters, so a common way is needed to represent these. The
common ways of representing the code points are the encodings. This means a
whole new concept to the programmer, the concept of character representation,
which was a non-issue earlier.

Different operating systems and tools support different encodings. For example,
Linux and MacOS X have chosen the UTF-8 encoding, which is backward compatible
with 7-bit ASCII and therefore affects programs written in plain English the
least. Windows supports a limited version of UTF-16, namely all the code planes
where the characters can be stored in one single 16-bit entity, which includes
most living languages.

The following are the most widely spread encodings:

- **Bytewise representation** - This is not a proper Unicode representation, but
  the representation used for characters before the Unicode standard. It can
  still be used to represent character code points in the Unicode standard with
  numbers < 256, which exactly corresponds to the ISO Latin-1 character set. In
  Erlang, this is commonly denoted `latin1` encoding, which is slightly
  misleading as ISO Latin-1 is a character code range, not an encoding.

- **UTF-8** - Each character is stored in one to four bytes depending on code
  point. The encoding is backward compatible with bytewise representation of
  7-bit ASCII, as all 7-bit characters are stored in one single byte in UTF-8.
  The characters beyond code point 127 are stored in more bytes, letting the
  most significant bit in the first character indicate a multi-byte character.
  For details on the encoding, the RFC is publicly available.

  Notice that UTF-8 is _not_ compatible with bytewise representation for code
  points from 128 through 255, so an ISO Latin-1 bytewise representation is
  generally incompatible with UTF-8.

- **UTF-16** - This encoding has many similarities to UTF-8, but the basic unit
  is a 16-bit number. This means that all characters occupy at least two bytes,
  and some high numbers four bytes. Some programs, libraries, and operating
  systems claiming to use UTF-16 only allow for characters that can be stored in
  one 16-bit entity, which is usually sufficient to handle living languages. As
  the basic unit is more than one byte, byte-order issues occur, which is why
  UTF-16 exists in both a big-endian and a little-endian variant.

  In Erlang, the full UTF-16 range is supported when applicable, like in the
  `m:unicode` module and in the bit syntax.

- **UTF-32** - The most straightforward representation. Each character is stored
  in one single 32-bit number. There is no need for escapes or any variable
  number of entities for one character. All Unicode code points can be stored in
  one single 32-bit entity. As with UTF-16, there are byte-order issues. UTF-32
  can be both big-endian and little-endian.

- **UCS-4** - Basically the same as UTF-32, but without some Unicode semantics,
  defined by IEEE, and has little use as a separate encoding standard. For all
  normal (and possibly abnormal) use, UTF-32 and UCS-4 are interchangeable.

Certain number ranges are unused in the Unicode standard and certain ranges are
even deemed invalid. The most notable invalid range is 16#D800-16#DFFF, as the
UTF-16 encoding does not allow for encoding of these numbers. This is possibly
because the UTF-16 encoding standard, from the beginning, was expected to be
able to hold all Unicode characters in one 16-bit entity, but was then extended,
leaving a hole in the Unicode range to handle backward compatibility.

Code point 16#FEFF is used for Byte Order Marks (BOMs) and use of that character
is not encouraged in other contexts. It is valid though, as the character
"ZWNBS" (Zero Width Non Breaking Space). BOMs are used to identify encodings and
byte order for programs where such parameters are not known in advance. BOMs are
more seldom used than expected, but can become more widely spread as they
provide the means for programs to make educated guesses about the Unicode format
of a certain file.

## Areas of Unicode Support

To support Unicode in Erlang, problems in various areas have been addressed.
This section describes each area briefly and more thoroughly later in this
User's Guide.

- **Representation** - To handle Unicode characters in Erlang, a common
  representation in both lists and binaries is needed. EEP (10) and the
  subsequent initial implementation in Erlang/OTP R13A settled a standard
  representation of Unicode characters in Erlang.

- **Manipulation** - The Unicode characters need to be processed by the Erlang
  program, which is why library functions must be able to handle them. In some
  cases functionality has been added to already existing interfaces (as the
  `m:string` module now can handle strings with any code points). In some cases
  new functionality or options have been added (as in the `m:io` module, the
  file handling, the `m:unicode` module, and the bit syntax). Today most modules
  in Kernel and STDLIB, as well as the VM are Unicode-aware.

- **File I/O** - I/O is by far the most problematic area for Unicode. A file is
  an entity where bytes are stored, and the lore of programming has been to
  treat characters and bytes as interchangeable. With Unicode characters, you
  must decide on an encoding when you want to store the data in a file. In
  Erlang, you can open a text file with an encoding option, so that you can read
  characters from it rather than bytes, but you can also open a file for
  bytewise I/O.

  The Erlang I/O-system has been designed (or at least used) in a way where you
  expect any I/O server to handle any string data. That is, however, no longer
  the case when working with Unicode characters. The Erlang programmer must now
  know the capabilities of the device where the data ends up. Also, ports in
  Erlang are byte-oriented, so an arbitrary string of (Unicode) characters
  cannot be sent to a port without first converting it to an encoding of choice.

- **Terminal I/O** - Terminal I/O is slightly easier than file I/O. The output
  is meant for human reading and is usually Erlang syntax (for example, in the
  shell). There exists syntactic representation of any Unicode character without
  displaying the glyph (instead written as `\x`\{`HHH`\}). Unicode data can
  therefore usually be displayed even if the terminal as such does not support
  the whole Unicode range.

- **Filenames** - Filenames can be stored as Unicode strings in different ways
  depending on the underlying operating system and file system. This can be
  handled fairly easy by a program. The problems arise when the file system is
  inconsistent in its encodings. For example, Linux allows files to be named
  with any sequence of bytes, leaving to each program to interpret those bytes.
  On systems where these "transparent" filenames are used, Erlang must be
  informed about the filename encoding by a startup flag. The default is
  bytewise interpretation, which is usually wrong, but allows for interpretation
  of _all_ filenames.

  The concept of "raw filenames" can be used to handle wrongly encoded filenames
  if one enables Unicode filename translation (`+fnu`) on platforms where this
  is not the default.

- **Source code encoding** - The Erlang source code has support for the UTF-8
  encoding and bytewise encoding. The default in Erlang/OTP R16B was bytewise
  (`latin1`) encoding. It was changed to UTF-8 in Erlang/OTP 17.0. You can
  control the encoding by a comment like the following in the beginning of the
  file:

  ```erlang
  %% -*- coding: utf-8 -*-
  ```

  This of course requires your editor to support UTF-8 as well. The same comment
  is also interpreted by functions like `file:consult/1`, the release handler,
  and so on, so that you can have all text files in your source directories in
  UTF-8 encoding.

- **The language** - Having the source code in UTF-8 also allows you to write
  string literals, function names, and atoms containing Unicode characters with
  code points > 255. Module names, application names, and node names are still
  restricted to the ISO Latin-1 range. Binary literals, where you use type
  `/utf8`, can also be expressed using Unicode characters > 255. Having module
  names or application names using characters other than 7-bit ASCII can cause
  trouble on operating systems with inconsistent file naming schemes, and can
  hurt portability, so it is not recommended.

  EEP 40 suggests that the language is also to allow for Unicode characters >
  255 in variable names. Whether to implement that EEP is yet to be decided.

## Standard Unicode Representation

In Erlang, strings are lists of integers. A string was until Erlang/OTP R13
defined to be encoded in the ISO Latin-1 (ISO 8859-1) character set, which is,
code point by code point, a subrange of the Unicode character set.

The standard list encoding for strings was therefore easily extended to handle
the whole Unicode range. A Unicode string in Erlang is a list containing
integers, where each integer is a valid Unicode code point and represents one
character in the Unicode character set.

Erlang strings in ISO Latin-1 are a subset of Unicode strings.

Only if a string contains code points < 256, can it be directly converted to a
binary by using, for example, `erlang:iolist_to_binary/1` or can be sent
directly to a port. If the string contains Unicode characters > 255, an encoding
must be decided upon and the string is to be converted to a binary in the
preferred encoding using
[`unicode:characters_to_binary/1,2,3`](`unicode:characters_to_binary/1`).
Strings are not generally lists of bytes, as they were before Erlang/OTP R13,
they are lists of characters. Characters are not generally bytes, they are
Unicode code points.

Binaries are more troublesome. For performance reasons, programs often store
textual data in binaries instead of lists, mainly because they are more compact
(one byte per character instead of two words per character, as is the case with
lists). Using `erlang:list_to_binary/1`, an ISO Latin-1 Erlang string can be
converted into a binary, effectively using bytewise encoding: one byte per
character. This was convenient for those limited Erlang strings, but cannot be
done for arbitrary Unicode lists.

As the UTF-8 encoding is widely spread and provides some backward compatibility
in the 7-bit ASCII range, it is selected as the standard encoding for Unicode
characters in binaries for Erlang.

The standard binary encoding is used whenever a library function in Erlang is to
handle Unicode data in binaries, but is of course not enforced when
communicating externally. Functions and bit syntax exist to encode and decode
both UTF-8, UTF-16, and UTF-32 in binaries. However, library functions dealing
with binaries and Unicode in general only deal with the default encoding.

Character data can be combined from many sources, sometimes available in a mix
of strings and binaries. Erlang has for long had the concept of `iodata` or
`iolist`s, where binaries and lists can be combined to represent a sequence of
bytes. In the same way, the Unicode-aware modules often allow for combinations
of binaries and lists, where the binaries have characters encoded in UTF-8 and
the lists contain such binaries or numbers representing Unicode code points:

```erlang
unicode_binary() = binary() with characters encoded in UTF-8 coding standard

chardata() = charlist() | unicode_binary()

charlist() = maybe_improper_list(char() | unicode_binary() | charlist(),
  unicode_binary() | nil())
```

The module `m:unicode` even supports similar mixes with binaries containing
other encodings than UTF-8, but that is a special case to allow for conversions
to and from external data:

```erlang
external_unicode_binary() = binary() with characters coded in a user-specified
  Unicode encoding other than UTF-8 (UTF-16 or UTF-32)

external_chardata() = external_charlist() | external_unicode_binary()

external_charlist() = maybe_improper_list(char() | external_unicode_binary() |
  external_charlist(), external_unicode_binary() | nil())
```

## Basic Language Support

[](){: #unicode_in_erlang } As from Erlang/OTP R16, Erlang source files can be
written in UTF-8 or bytewise (`latin1`) encoding. For information about how to
state the encoding of an Erlang source file, see the [`epp`](`m:epp#encoding`)
module. As from Erlang/OTP R16, strings and comments can be written using
Unicode. As from Erlang/OTP 20, also atoms and functions can be written using
Unicode. Modules, applications, and nodes must still be named using characters
from the ISO Latin-1 character set. (These restrictions in the language are
independent of the encoding of the source file.)

### Bit Syntax

The bit syntax contains types for handling binary data in the three main
encodings. The types are named `utf8`, `utf16`, and `utf32`. The `utf16` and
`utf32` types can be in a big-endian or a little-endian variant:

```text
<<Ch/utf8,_/binary>> = Bin1,
<<Ch/utf16-little,_/binary>> = Bin2,
Bin3 = <<$H/utf32-little, $e/utf32-little, $l/utf32-little, $l/utf32-little,
$o/utf32-little>>,
```

For convenience, literal strings can be encoded with a Unicode encoding in
binaries using the following (or similar) syntax:

```text
Bin4 = <<"Hello"/utf16>>,
```

### String and Character Literals

For source code, there is an extension to syntax `\`OOO (backslash followed by
three octal numbers) and `\x`HH (backslash followed by `x`, followed by two
hexadecimal characters), namely `\x{`H ...`}` (backslash followed by `x`,
followed by left curly bracket, any number of hexadecimal digits, and a
terminating right curly bracket). This allows for entering characters of any
code point literally in a string even when the encoding of the source file is
bytewise (`latin1`).

In the shell, if using a Unicode input device, or in source code stored in
UTF-8, `$` can be followed directly by a Unicode character producing an integer.
In the following example, the code point of a Cyrillic `с` is output:

```text
7> $с.
1089
```

### Heuristic String Detection

In certain output functions and in the output of return values in the shell,
Erlang tries to detect string data in lists and binaries heuristically.
Typically you will see heuristic detection in a situation like this:

```erlang
1> [97,98,99].
"abc"
2> <<97,98,99>>.
<<"abc">>
3> <<195,165,195,164,195,182>>.
<<"åäö"/utf8>>
```

Here the shell detects lists containing printable characters or binaries
containing printable characters in bytewise or UTF-8 encoding. But what is a
printable character? One view is that anything the Unicode standard thinks is
printable, is also printable according to the heuristic detection. The result is
then that almost any list of integers are deemed a string, and all sorts of
characters are printed, maybe also characters that your terminal lacks in its
font set (resulting in some unappreciated generic output). Another way is to
keep it backward compatible so that only the ISO Latin-1 character set is used
to detect a string. A third way is to let the user decide exactly what Unicode
ranges that are to be viewed as characters.

As from Erlang/OTP R16B you can select the ISO Latin-1 range or the whole
Unicode range by supplying startup flag `+pc latin1` or `+pc unicode`,
respectively. For backward compatibility, `latin1` is default. This only
controls how heuristic string detection is done. More ranges are expected to be
added in the future, enabling tailoring of the heuristics to the language and
region relevant to the user.

The following examples show the two startup options:

```erlang
$ erl +pc latin1
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> [1024].
[1024]
2> [1070,1085,1080,1082,1086,1076].
[1070,1085,1080,1082,1086,1076]
3> [229,228,246].
"åäö"
4> <<208,174,208,189,208,184,208,186,208,190,208,180>>.
<<208,174,208,189,208,184,208,186,208,190,208,180>>
5> <<229/utf8,228/utf8,246/utf8>>.
<<"åäö"/utf8>>
```

```erlang
$ erl +pc unicode
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> [1024].
"Ѐ"
2> [1070,1085,1080,1082,1086,1076].
"Юникод"
3> [229,228,246].
"åäö"
4> <<208,174,208,189,208,184,208,186,208,190,208,180>>.
<<"Юникод"/utf8>>
5> <<229/utf8,228/utf8,246/utf8>>.
<<"åäö"/utf8>>
```

In the examples, you can see that the default Erlang shell interprets only
characters from the ISO Latin1 range as printable and only detects lists or
binaries with those "printable" characters as containing string data. The valid
UTF-8 binary containing the Russian word "Юникод", is not printed as a string.
When started with all Unicode characters printable (`+pc unicode`), the shell
outputs anything containing printable Unicode data (in binaries, either UTF-8 or
bytewise encoded) as string data.

These heuristics are also used by `io:format/2`, `io_lib:format/2`, and friends
when modifier `t` is used with `~p` or `~P`:

```erlang
$ erl +pc latin1
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> io:format("~tp~n",[{<<"åäö">>, <<"åäö"/utf8>>, <<208,174,208,189,208,184,208,186,208,190,208,180>>}]).
{<<"åäö">>,<<"åäö"/utf8>>,<<208,174,208,189,208,184,208,186,208,190,208,180>>}
ok
```

```erlang
$ erl +pc unicode
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> io:format("~tp~n",[{<<"åäö">>, <<"åäö"/utf8>>, <<208,174,208,189,208,184,208,186,208,190,208,180>>}]).
{<<"åäö">>,<<"åäö"/utf8>>,<<"Юникод"/utf8>>}
ok
```

Notice that this only affects _heuristic_ interpretation of lists and binaries
on output. For example, the `~ts` format sequence always outputs a valid list of
characters, regardless of the `+pc` setting, as the programmer has explicitly
requested string output.

## The Interactive Shell

The interactive Erlang shell can support Unicode input and output.

On Windows, proper operation requires that a suitable font is installed and
selected for the Erlang application to use. If no suitable font is available on
your system, try installing the [DejaVu fonts](http://dejavu-fonts.org), which
are freely available, and then select that font in the Erlang shell application.

On Unix-like operating systems, the terminal is to be able to handle UTF-8 on
input and output (this is done by, for example, modern versions of XTerm, KDE
Konsole, and the Gnome terminal) and your locale settings must be proper. As an
example, a `LANG` environment variable can be set as follows:

```text
$ echo $LANG
en_US.UTF-8
```

Most systems handle variable `LC_CTYPE` before `LANG`, so if that is set, it
must be set to `UTF-8`:

```text
$ echo $LC_CTYPE
en_US.UTF-8
```

The `LANG` or `LC_CTYPE` setting are to be consistent with what the terminal is
capable of. There is no portable way for Erlang to ask the terminal about its
UTF-8 capacity, we have to rely on the language and character type settings.

To investigate what Erlang thinks about the terminal, the call
[`io:getopts()`](`io:getopts/1`) can be used when the shell is started:

```erlang
$ LC_CTYPE=en_US.ISO-8859-1 erl
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> lists:keyfind(encoding, 1, io:getopts()).
{encoding,latin1}
2> q().
ok
$ LC_CTYPE=en_US.UTF-8 erl
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> lists:keyfind(encoding, 1, io:getopts()).
{encoding,unicode}
2>
```

When (finally?) everything is in order with the locale settings, fonts. and the
terminal emulator, you have probably found a way to input characters in the
script you desire. For testing, the simplest way is to add some keyboard
mappings for other languages, usually done with some applet in your desktop
environment.

In a KDE environment, select _KDE Control Center (Personal Settings)_ >
_Regional and Accessibility_ > _Keyboard Layout_.

On Windows XP, select _Control Panel_ > _Regional and Language Options_, select
tab _Language_, and click button _Details..._ in the square named _Text Services
and Input Languages_.

Your environment probably provides similar means of changing the keyboard
layout. Ensure that you have a way to switch back and forth between keyboards
easily if you are not used to this. For example, entering commands using a
Cyrillic character set is not easily done in the Erlang shell.

Now you are set up for some Unicode input and output. The simplest thing to do
is to enter a string in the shell:

```erlang
$ erl
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> lists:keyfind(encoding, 1, io:getopts()).
{encoding,unicode}
2> "Юникод".
"Юникод"
3> io:format("~ts~n", [v(2)]).
Юникод
ok
4>
```

While strings can be input as Unicode characters, the language elements are
still limited to the ISO Latin-1 character set. Only character constants and
strings are allowed to be beyond that range:

```text
$ erl
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> $ξ.
958
2> Юникод.
* 1: illegal character
2>
```

## Escripts and non-interactive I/O

When Erlang is started without an interactive shell (`-noshell`, `-noinput` or
as an escript) the unicode support is identified using environment variables
just as for [interactive shells](unicode_usage.md#the-interactive-shell).
Working with unicode in non-interactive sessions works just the same as for
interactive sessions.

In some situations you may need to be able to read and write raw bytes from
[`standard_io`](`t:io:standard_io/0`). If that is the case, then you want to set
the [standard_io_encoding](`e:kernel:kernel_app.md#standard_io_encoding`)
configuration parameter to `latin1` and use the `m:file` API to read and write
data (as explained in
[Unicode Data in Files](unicode_usage.md#unicode-data-in-files)).

In the example below we first read the character `ξ` from
[`standard_io`](`t:io:standard_io/0`) and then print the
[charlist()](`t:unicode:charlist/0`) represented by it.

```erlang
#!/usr/bin/env escript
%%! -kernel standard_io_encoding latin1

main(_) ->
  {ok, Char} = file:read_line(standard_io),
  ok = file:write(standard_io, string:trim(Char)),
  ok = file:write(standard_io, io_lib:format(": ~w~n",[string:trim(Char)])),
  ok.
```

```text
$ escript test.es
ξ
ξ: [206,190]
```

`ξ` would normally be represented as the integer 958, but since we are using
bytewise encoding (`latin1`), it is represented by 206 and 190, which is the
utf-8 bytes representing `ξ`. When we echo those bytes back to
[`standard_io`](`t:io:standard_io/0`), the terminal will see the bytes as utf-8
and show the correct value even though in Erlang we never knew that it was
indeed a unicode string.

[](){: #unicode_file_names }

## Unicode Filenames

Most modern operating systems support Unicode filenames in some way. There are
many different ways to do this and Erlang by default treats the different
approaches differently:

- **Mandatory Unicode file naming** - Windows, Android and, for most cases,
  MacOS X enforce Unicode support for filenames. All files created in the file
  system have names that can consistently be interpreted. In MacOS X and
  Android, all filenames are retrieved in UTF-8 encoding. In Windows, each
  system call handling filenames has a special Unicode-aware variant, giving
  much the same effect. There are no filenames on these systems that are not
  Unicode filenames. So, the default behavior of the Erlang VM is to work in
  "Unicode filename translation mode". This means that a filename can be
  specified as a Unicode list, which is automatically translated to the proper
  name encoding for the underlying operating system and file system.

  Doing, for example, a `file:list_dir/1` on one of these systems can return
  Unicode lists with code points > 255, depending on the content of the file
  system.

- **Transparent file naming** - Most Unix operating systems have adopted a
  simpler approach, namely that Unicode file naming is not enforced, but by
  convention. Those systems usually use UTF-8 encoding for Unicode filenames,
  but do not enforce it. On such a system, a filename containing characters with
  code points from 128 through 255 can be named as plain ISO Latin-1 or use
  UTF-8 encoding. As no consistency is enforced, the Erlang VM cannot do
  consistent translation of all filenames.

  By default on such systems, Erlang starts in `utf8` filename mode if the
  terminal supports UTF-8, otherwise in `latin1` mode.

  In `latin1` mode, filenames are bytewise encoded. This allows for list
  representation of all filenames in the system. However, a a file named
  "Östersund.txt", appears in `file:list_dir/1` either as "Östersund.txt" (if
  the filename was encoded in bytewise ISO Latin-1 by the program creating the
  file) or more probably as `[195,150,115,116,101,114,115,117,110,100]`, which
  is a list containing UTF-8 bytes (not what you want). If you use Unicode
  filename translation on such a system, non-UTF-8 filenames are ignored by
  functions like `file:list_dir/1`. They can be retrieved with function
  `file:list_dir_all/1`, but wrongly encoded filenames appear as "raw
  filenames".

The Unicode file naming support was introduced in Erlang/OTP R14B01. A VM
operating in Unicode filename translation mode can work with files having names
in any language or character set (as long as it is supported by the underlying
operating system and file system). The Unicode character list is used to denote
filenames or directory names. If the file system content is listed, you also get
Unicode lists as return value. The support lies in the Kernel and STDLIB
modules, which is why most applications (that do not explicitly require the
filenames to be in the ISO Latin-1 range) benefit from the Unicode support
without change.

On operating systems with mandatory Unicode filenames, this means that you more
easily conform to the filenames of other (non-Erlang) applications. You can also
process filenames that, at least on Windows, were inaccessible (because of
having names that could not be represented in ISO Latin-1). Also, you avoid
creating incomprehensible filenames on MacOS X, as the `vfs` layer of the
operating system accepts all your filenames as UTF-8 does not rewrite them.

For most systems, turning on Unicode filename translation is no problem even if
it uses transparent file naming. Very few systems have mixed filename encodings.
A consistent UTF-8 named system works perfectly in Unicode filename mode. It was
still, however, considered experimental in Erlang/OTP R14B01 and is still not
the default on such systems.

Unicode filename translation is turned on with switch `+fnu`. On Linux, a VM
started without explicitly stating the filename translation mode defaults to
`latin1` as the native filename encoding. On Windows, MacOS X and Android, the
default behavior is that of Unicode filename translation. Therefore
`file:native_name_encoding/0` by default returns `utf8` on those systems
(Windows does not use UTF-8 on the file system level, but this can safely be
ignored by the Erlang programmer). The default behavior can, as stated earlier,
be changed using option `+fnu` or `+fnl` to the VM, see the
[`erl`](`e:erts:erl_cmd.md`) program. If the VM is started in Unicode filename
translation mode, `file:native_name_encoding/0` returns atom `utf8`. Switch
`+fnu` can be followed by `w`, `i`, or `e` to control how wrongly encoded
filenames are to be reported.

- `w` means that a warning is sent to the `error_logger` whenever a wrongly
  encoded filename is "skipped" in directory listings. `w` is the default.
- `i` means that wrongly encoded filenames are silently ignored.
- `e` means that the API function returns an error whenever a wrongly encoded
  filename (or directory name) is encountered.

Notice that `file:read_link/1` always returns an error if the link points to an
invalid filename.

In Unicode filename mode, filenames given to BIF [`open_port/2`](`open_port/2`)
with option `{spawn_executable,...}` are also interpreted as Unicode. So is the
parameter list specified in option `args` available when using
`spawn_executable`. The UTF-8 translation of arguments can be avoided using
binaries, see section
[Notes About Raw Filenames](unicode_usage.md#notes-about-raw-filenames).

Notice that the file encoding options specified when opening a file has nothing
to do with the filename encoding convention. You can very well open files
containing data encoded in UTF-8, but having filenames in bytewise (`latin1`)
encoding or conversely.

> #### Note {: .info }
>
> Erlang drivers and NIF-shared objects still cannot be named with names
> containing code points > 127. This limitation will be removed in a future
> release. However, Erlang modules can, but it is definitely not a good idea and
> is still considered experimental.

### Notes About Raw Filenames

> #### Note {: .info }
>
> Note that raw filenames _not_ necessarily are encoded the same way as on the
> OS level.

Raw filenames were introduced together with Unicode filename support in ERTS
5.8.2 (Erlang/OTP R14B01). The reason "raw filenames" were introduced in the
system was to be able to represent filenames, specified in different encodings
on the same system, consistently. It can seem practical to have the VM
automatically translate a filename that is not in UTF-8 to a list of Unicode
characters, but this would open up for both duplicate filenames and other
inconsistent behavior.

Consider a directory containing a file named "björn" in ISO Latin-1, while the
Erlang VM is operating in Unicode filename mode (and therefore expects UTF-8
file naming). The ISO Latin-1 name is not valid UTF-8 and one can be tempted to
think that automatic conversion in, for example, `file:list_dir/1` is a good
idea. But what would happen if we later tried to open the file and have the name
as a Unicode list (magically converted from the ISO Latin-1 filename)? The VM
converts the filename to UTF-8, as this is the encoding expected. Effectively
this means trying to open the file named <<"björn"/utf8>>. This file does not
exist, and even if it existed it would not be the same file as the one that was
listed. We could even create two files named "björn", one named in UTF-8
encoding and one not. If `file:list_dir/1` would automatically convert the ISO
Latin-1 filename to a list, we would get two identical filenames as the result.
To avoid this, we must differentiate between filenames that are properly encoded
according to the Unicode file naming convention (that is, UTF-8) and filenames
that are invalid under the encoding. By the common function `file:list_dir/1`,
the wrongly encoded filenames are ignored in Unicode filename translation mode,
but by function `file:list_dir_all/1` the filenames with invalid encoding are
returned as "raw" filenames, that is, as binaries.

The `file` module accepts raw filenames as input.
`open_port({spawn_executable, ...} ...)` also accepts them. As mentioned
earlier, the arguments specified in the option list to
`open_port({spawn_executable, ...} ...)` undergo the same conversion as the
filenames, meaning that the executable is provided with arguments in UTF-8 as
well. This translation is avoided consistently with how the filenames are
treated, by giving the argument as a binary.

To force Unicode filename translation mode on systems where this is not the
default was considered experimental in Erlang/OTP R14B01. This was because the
initial implementation did not ignore wrongly encoded filenames, so that raw
filenames could spread unexpectedly throughout the system. As from Erlang/OTP
R16B, the wrongly encoded filenames are only retrieved by special functions
(such as `file:list_dir_all/1`). Since the impact on existing code is therefore
much lower it is now supported. Unicode filename translation is expected to be
default in future releases.

Even if you are operating without Unicode file naming translation automatically
done by the VM, you can access and create files with names in UTF-8 encoding by
using raw filenames encoded as UTF-8. Enforcing the UTF-8 encoding regardless of
the mode the Erlang VM is started in can in some circumstances be a good idea,
as the convention of using UTF-8 filenames is spreading.

### Notes About MacOS X

The `vfs` layer of MacOS X enforces UTF-8 filenames in an aggressive way. Older
versions did this by refusing to create non-UTF-8 conforming filenames, while
newer versions replace offending bytes with the sequence "%HH", where HH is the
original character in hexadecimal notation. As Unicode translation is enabled by
default on MacOS X, the only way to come up against this is to either start the
VM with flag `+fnl` or to use a raw filename in bytewise (`latin1`) encoding. If
using a raw filename, with a bytewise encoding containing characters from 127
through 255, to create a file, the file cannot be opened using the same name as
the one used to create it. There is no remedy for this behavior, except keeping
the filenames in the correct encoding.

MacOS X reorganizes the filenames so that the representation of accents, and so
on, uses the "combining characters". For example, character `ö` is represented
as code points `[111,776]`, where `111` is character `o` and `776` is the
special accent character "Combining Diaeresis". This way of normalizing Unicode
is otherwise very seldom used. Erlang normalizes those filenames in the opposite
way upon retrieval, so that filenames using combining accents are not passed up
to the Erlang application. In Erlang, filename "björn" is retrieved as
`[98,106,246,114,110]`, not as `[98,106,117,776,114,110]`, although the file
system can think differently. The normalization into combining accents is redone
when accessing files, so this can usually be ignored by the Erlang programmer.

## Unicode in Environment and Parameters

[](){: #unicode_in_environment_and_parameters }

Environment variables and their interpretation are handled much in the same way
as filenames. If Unicode filenames are enabled, environment variables as well as
parameters to the Erlang VM are expected to be in Unicode.

If Unicode filenames are enabled, the calls to [`os:getenv/0,1`](`os:getenv/0`),
`os:putenv/2`, and `os:unsetenv/1` handle Unicode strings. On Unix-like
platforms, the built-in functions translate environment variables in UTF-8
to/from Unicode strings, possibly with code points > 255. On Windows, the
Unicode versions of the environment system API are used, and code points > 255
are allowed.

On Unix-like operating systems, parameters are expected to be UTF-8 without
translation if Unicode filenames are enabled.

## Unicode-Aware Modules

Most of the modules in Erlang/OTP are Unicode-unaware in the sense that they
have no notion of Unicode and should not have. Typically they handle non-textual
or byte-oriented data (such as `gen_tcp`).

Modules handling textual data (such as `m:io_lib` and `m:string` are sometimes
subject to conversion or extension to be able to handle Unicode characters.

Fortunately, most textual data has been stored in lists and range checking has
been sparse, so modules like `string` work well for Unicode strings with little
need for conversion or extension.

Some modules are, however, changed to be explicitly Unicode-aware. These modules
include:

- **`unicode`** - The `m:unicode` module is clearly Unicode-aware. It contains
  functions for conversion between different Unicode formats and some utilities
  for identifying byte order marks. Few programs handling Unicode data survive
  without this module.

- **`io`** - The `m:io` module has been extended along with the actual I/O
  protocol to handle Unicode data. This means that many functions require
  binaries to be in UTF-8, and there are modifiers to format control sequences
  to allow for output of Unicode strings.

- **`file`, `group`, `user`** - I/O-servers throughout the system can handle
  Unicode data and have options for converting data upon output or input to/from
  the device. As shown earlier, the `m:shell` module has support for Unicode
  terminals and the `m:file` module allows for translation to and from various
  Unicode formats on disk.

  Reading and writing of files with Unicode data is, however, not best done with
  the `file` module, as its interface is byte-oriented. A file opened with a
  Unicode encoding (like UTF-8) is best read or written using the `m:io` module.

- **`re`** - The `m:re` module allows for matching Unicode strings as a special
  option. As the library is centered on matching in binaries, the Unicode
  support is UTF-8-centered.

- **`wx`** - The graphical library `m:wx` has extensive support for Unicode
  text.

The `m:string` module works perfectly for Unicode strings and ISO Latin-1
strings, except the language-dependent functions `string:uppercase/1` and
`string:lowercase/1`. These two functions can never function correctly for
Unicode characters in their current form, as there are language and locale
issues to consider when converting text between cases. Converting case in an
international environment is a large subject not yet addressed in OTP.

## Unicode Data in Files

Although Erlang can handle Unicode data in many forms does not automatically
mean that the content of any file can be Unicode text. The external entities,
such as ports and I/O servers, are not generally Unicode capable.

Ports are always byte-oriented, so before sending data that you are not sure is
bytewise-encoded to a port, ensure to encode it in a proper Unicode encoding.
Sometimes this means that only part of the data must be encoded as, for example,
UTF-8. Some parts can be binary data (like a length indicator) or something else
that must not undergo character encoding, so no automatic translation is
present.

I/O servers behave a little differently. The I/O servers connected to terminals
(or `stdout`) can usually cope with Unicode data regardless of the encoding
option. This is convenient when one expects a modern environment but do not want
to crash when writing to an archaic terminal or pipe.

A file can have an encoding option that makes it generally usable by the `m:io`
module (for example `{encoding,utf8}`), but is by default opened as a
byte-oriented file. The `m:file` module is byte-oriented, so only ISO Latin-1
characters can be written using that module. Use the `io` module if Unicode data
is to be output to a file with other `encoding` than `latin1` (bytewise
encoding). It is slightly confusing that a file opened with, for example,
`file:open(Name,[read,{encoding,utf8}])` cannot be properly read using
`file:read(File,N)`, but using the `io` module to retrieve the Unicode data from
it. The reason is that `file:read` and `file:write` (and friends) are purely
byte-oriented, and should be, as that is the way to access files other than text
files, byte by byte. As with ports, you can write encoded data into a file by
"manually" converting the data to the encoding of choice (using the `m:unicode`
module or the bit syntax) and then output it on a bytewise (`latin1`) encoded
file.

Recommendations:

- Use the `m:file` module for files opened for bytewise access
  (`{encoding,latin1}`).
- Use the `m:io` module when accessing files with any other encoding (for
  example `{encoding,utf8}`).

Functions reading Erlang syntax from files recognize the `coding:` comment and
can therefore handle Unicode data on input. When writing Erlang terms to a file,
you are advised to insert such comments when applicable:

```erlang
$ erl +fna +pc unicode
Erlang R16B (erts-5.10.1) [source]  [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> file:write_file("test.term",<<"%% coding: utf-8\n[{\"Юникод\",4711}].\n"/utf8>>).
ok
2> file:consult("test.term").
{ok,[[{"Юникод",4711}]]}
```

## Summary of Options

[](){: #unicode_options_summary }

The Unicode support is controlled by both command-line switches, some standard
environment variables, and the OTP version you are using. Most options affect
mainly how Unicode data is displayed, not the functionality of the APIs in the
standard libraries. This means that Erlang programs usually do not need to
concern themselves with these options, they are more for the development
environment. An Erlang program can be written so that it works well regardless
of the type of system or the Unicode options that are in effect.

Here follows a summary of the settings affecting Unicode:

- **The `LANG` and `LC_CTYPE` environment variables** - The language setting in
  the operating system mainly affects the shell. The terminal (that is, the
  group leader) operates with `{encoding, unicode}` only if the environment
  tells it that UTF-8 is allowed. This setting is to correspond to the terminal
  you are using.

  The environment can also affect filename interpretation, if Erlang is started
  with flag `+fna` (which is default from Erlang/OTP 17.0).

  You can check the setting of this by calling [`io:getopts()`](`io:getopts/1`),
  which gives you an option list containing `{encoding,unicode}` or
  `{encoding,latin1}`.

- **The `+pc` \{`unicode`|`latin1`\} flag to [`erl(1)`](`e:erts:erl_cmd.md`)** -
  This flag affects what is interpreted as string data when doing heuristic
  string detection in the shell and in `m:io`/
  [`io_lib:format`](`io_lib:format/2`) with the `"~tp"` and `~tP` formatting
  instructions, as described earlier.

  You can check this option by calling `io:printable_range/0`, which returns
  `unicode` or `latin1`. To be compatible with future (expected) extensions to
  the settings, rather use `io_lib:printable_list/1` to check if a list is
  printable according to the setting. That function takes into account new
  possible settings returned from `io:printable_range/0`.

- **The `+fn`\{`l`|`u`|`a`\} [\{`w`|`i`|`e`\}] flag to
  [`erl(1)`](`e:erts:erl_cmd.md`)** - This flag affects how the filenames are to
  be interpreted. On operating systems with transparent file naming, this must
  be specified to allow for file naming in Unicode characters (and for correct
  interpretation of filenames containing characters > 255).

  - `+fnl` means bytewise interpretation of filenames, which was the usual way
    to represent ISO Latin-1 filenames before UTF-8 file naming got widespread.
  - `+fnu` means that filenames are encoded in UTF-8, which is nowadays the
    common scheme (although not enforced).
  - `+fna` means that you automatically select between `+fnl` and `+fnu`, based
    on environment variables `LANG` and `LC_CTYPE`. This is optimistic
    heuristics indeed, nothing enforces a user to have a terminal with the same
    encoding as the file system, but this is usually the case. This is the
    default on all Unix-like operating systems, except MacOS X.

  The filename translation mode can be read with function
  `file:native_name_encoding/0`, which returns `latin1` (bytewise encoding) or
  `utf8`.

- **`epp:default_encoding/0`** - This function returns the default encoding for
  Erlang source files (if no encoding comment is present) in the currently
  running release. In Erlang/OTP R16B, `latin1` (bytewise encoding) was
  returned. As from Erlang/OTP 17.0, `utf8` is returned.

  The encoding of each file can be specified using comments as described in the
  [`epp`](`m:epp#encoding`) module.

- **[`io:setopts/1,2`](`io:setopts/1`) and
  [`standard_io_encoding`](`e:kernel:kernel_app.md#standard_io_encoding`)** -
  When Erlang is started the encoding for [`standard_io`](`t:io:standard_io/0`)
  is by default set to what the
  [locale settings indicate](unicode_usage.md#the-interactive-shell). You can
  override the default by setting the kernel configuration parameter
  [`standard_io_encoding`](`e:kernel:kernel_app.md#standard_io_encoding`) to the
  desired encoding.

  You can set the encoding of a file or other I/O server with function
  [`io:setopts/2`](`io:setopts/1`). This can also be set when opening a file.
  Setting the terminal (or other [`standard_io`](`t:io:standard_io/0`) server)
  unconditionally to option `{encoding,utf8}` implies that UTF-8 encoded
  characters are written to the device, regardless of how Erlang was started or
  the user's environment.

  > #### Note {: .info }
  >
  > If you use [`io:setopts/2`](`io:setopts/1`) to change the encoding of
  > [`standard_io`](`t:io:standard_io/0`) the I/O server may already have read
  > some data using the default encoding. To avoid this you should set the
  > encoding using
  > [`standard_io_encoding`](`e:kernel:kernel_app.md#standard_io_encoding`).

  Opening files with option `encoding` is convenient when writing or reading
  text files in a known encoding.

  You can retrieve the `encoding` setting for an I/O server with function
  [`io:getopts()`](`io:getopts/1`).

## Recipes

When starting with Unicode, one often stumbles over some common issues. This
section describes some methods of dealing with Unicode data.

### Byte Order Marks

A common method of identifying encoding in text files is to put a Byte Order
Mark (BOM) first in the file. The BOM is the code point 16#FEFF encoded in the
same way as the remaining file. If such a file is to be read, the first few
bytes (depending on encoding) are not part of the text. This code outlines how
to open a file that is believed to have a BOM, and sets the files encoding and
position for further sequential reading (preferably using the `m:io` module).

Notice that error handling is omitted from the code:

```erlang
open_bom_file_for_reading(File) ->
    {ok,F} = file:open(File,[read,binary]),
    {ok,Bin} = file:read(F,4),
    {Type,Bytes} = unicode:bom_to_encoding(Bin),
    file:position(F,Bytes),
    io:setopts(F,[{encoding,Type}]),
    {ok,F}.
```

Function `unicode:bom_to_encoding/1` identifies the encoding from a binary of at
least four bytes. It returns, along with a term suitable for setting the
encoding of the file, the byte length of the BOM, so that the file position can
be set accordingly. Notice that function `file:position/2` always works on
byte-offsets, so that the byte length of the BOM is needed.

To open a file for writing and place the BOM first is even simpler:

```erlang
open_bom_file_for_writing(File,Encoding) ->
    {ok,F} = file:open(File,[write,binary]),
    ok = file:write(File,unicode:encoding_to_bom(Encoding)),
    io:setopts(F,[{encoding,Encoding}]),
    {ok,F}.
```

The file is in both these cases then best processed using the `m:io` module, as
the functions in that module can handle code points beyond the ISO Latin-1
range.

### Formatted I/O

When reading and writing to Unicode-aware entities, like a file opened for
Unicode translation, you probably want to format text strings using the
functions in the `m:io` module or the `m:io_lib` module. For backward
compatibility reasons, these functions do not accept any list as a string, but
require a special _translation modifier_ when working with Unicode texts. The
modifier is `t`. When applied to control character `s` in a formatting string,
it accepts all Unicode code points and expects binaries to be in UTF-8:

```erlang
1> io:format("~ts~n",[<<"åäö"/utf8>>]).
åäö
ok
2> io:format("~s~n",[<<"åäö"/utf8>>]).
Ã¥Ã¤Ã¶
ok
```

Clearly, the second `io:format/2` gives undesired output, as the UTF-8 binary is
not in `latin1`. For backward compatibility, the non-prefixed control character
`s` expects bytewise-encoded ISO Latin-1 characters in binaries and lists
containing only code points < 256.

As long as the data is always lists, modifier `t` can be used for any string,
but when binary data is involved, care must be taken to make the correct choice
of formatting characters. A bytewise-encoded binary is also interpreted as a
string, and printed even when using `~ts`, but it can be mistaken for a valid
UTF-8 string. Avoid therefore using the `~ts` control if the binary contains
bytewise-encoded characters and not UTF-8.

Function `io_lib:format/2` behaves similarly. It is defined to return a deep
list of characters and the output can easily be converted to binary data for
outputting on any device by a simple `erlang:list_to_binary/1`. When the
translation modifier is used, the list can, however, contain characters that
cannot be stored in one byte. The call to `erlang:list_to_binary/1` then fails.
However, if the I/O server you want to communicate with is Unicode-aware, the
returned list can still be used directly:

```erlang
$ erl +pc unicode
Erlang R16B (erts-5.10.1) [source] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.10.1 (abort with ^G)
1> io_lib:format("~ts~n", ["Γιούνικοντ"]).
["Γιούνικοντ","\n"]
2> io:put_chars(io_lib:format("~ts~n", ["Γιούνικοντ"])).
Γιούνικοντ
ok
```

The Unicode string is returned as a Unicode list, which is recognized as such,
as the Erlang shell uses the Unicode encoding (and is started with all Unicode
characters considered printable). The Unicode list is valid input to function
`io:put_chars/2`, so data can be output on any Unicode-capable device. If the
device is a terminal, characters are output in format `\x{`H...`}` if encoding
is `latin1`. Otherwise in UTF-8 (for the non-interactive terminal: "oldshell" or
"noshell") or whatever is suitable to show the character properly (for an
interactive terminal: the regular shell).

So, you can always send Unicode data to the
[`standard_io`](`t:io:standard_io/0`) device. Files, however, accept only
Unicode code points beyond ISO Latin-1 if `encoding` is set to something else
than `latin1`.

### Heuristic Identification of UTF-8

While it is strongly encouraged that the encoding of characters in binary data
is known before processing, that is not always possible. On a typical Linux
system, there is a mix of UTF-8 and ISO Latin-1 text files, and there are seldom
any BOMs in the files to identify them.

UTF-8 is designed so that ISO Latin-1 characters with numbers beyond the 7-bit
ASCII range are seldom considered valid when decoded as UTF-8. Therefore one can
usually use heuristics to determine if a file is in UTF-8 or if it is encoded in
ISO Latin-1 (one byte per character). The `m:unicode` module can be used to
determine if data can be interpreted as UTF-8:

```erlang
heuristic_encoding_bin(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
	Bin ->
	    utf8;
	_ ->
	    latin1
    end.
```

If you do not have a complete binary of the file content, you can instead chunk
through the file and check part by part. The return-tuple
`{incomplete,Decoded,Rest}` from function
[`unicode:characters_to_binary/1,2,3`](`unicode:characters_to_binary/1`) comes
in handy. The incomplete rest from one chunk of data read from the file is
prepended to the next chunk and we therefore avoid the problem of character
boundaries when reading chunks of bytes in UTF-8 encoding:

```erlang
heuristic_encoding_file(FileName) ->
    {ok,F} = file:open(FileName,[read,binary]),
    loop_through_file(F,<<>>,file:read(F,1024)).

loop_through_file(_,<<>>,eof) ->
    utf8;
loop_through_file(_,_,eof) ->
    latin1;
loop_through_file(F,Acc,{ok,Bin}) when is_binary(Bin) ->
    case unicode:characters_to_binary([Acc,Bin]) of
	{error,_,_} ->
	    latin1;
	{incomplete,_,Rest} ->
	    loop_through_file(F,Rest,file:read(F,1024));
	Res when is_binary(Res) ->
	    loop_through_file(F,<<>>,file:read(F,1024))
    end.
```

Another option is to try to read the whole file in UTF-8 encoding and see if it
fails. Here we need to read the file using function `io:get_chars/3`, as we have
to read characters with a code point > 255:

```erlang
heuristic_encoding_file2(FileName) ->
    {ok,F} = file:open(FileName,[read,binary,{encoding,utf8}]),
    loop_through_file2(F,io:get_chars(F,'',1024)).

loop_through_file2(_,eof) ->
    utf8;
loop_through_file2(_,{error,_Err}) ->
    latin1;
loop_through_file2(F,Bin) when is_binary(Bin) ->
    loop_through_file2(F,io:get_chars(F,'',1024)).
```

### Lists of UTF-8 Bytes

For various reasons, you can sometimes have a list of UTF-8 bytes. This is not a
regular string of Unicode characters, as each list element does not contain one
character. Instead you get the "raw" UTF-8 encoding that you have in binaries.
This is easily converted to a proper Unicode string by first converting byte per
byte into a binary, and then converting the binary of UTF-8 encoded characters
back to a Unicode string:

```erlang
utf8_list_to_string(StrangeList) ->
  unicode:characters_to_list(list_to_binary(StrangeList)).
```

### Double UTF-8 Encoding

When working with binaries, you can get the horrible "double UTF-8 encoding",
where strange characters are encoded in your binaries or files. In other words,
you can get a UTF-8 encoded binary that for the second time is encoded as UTF-8.
A common situation is where you read a file, byte by byte, but the content is
already UTF-8. If you then convert the bytes to UTF-8, using, for example, the
`m:unicode` module, or by writing to a file opened with option
`{encoding,utf8}`, you have each _byte_ in the input file encoded as UTF-8, not
each character of the original text (one character can have been encoded in many
bytes). There is no real remedy for this other than to be sure of which data is
encoded in which format, and never convert UTF-8 data (possibly read byte by
byte from a file) into UTF-8 again.

By far the most common situation where this occurs, is when you get lists of
UTF-8 instead of proper Unicode strings, and then convert them to UTF-8 in a
binary or on a file:

```erlang
wrong_thing_to_do() ->
  {ok,Bin} = file:read_file("an_utf8_encoded_file.txt"),
  MyList = binary_to_list(Bin), %% Wrong! It is an utf8 binary!
  {ok,C} = file:open("catastrophe.txt",[write,{encoding,utf8}]),
  io:put_chars(C,MyList), %% Expects a Unicode string, but get UTF-8
                          %% bytes in a list!
  file:close(C). %% The file catastrophe.txt contains more or less unreadable
                 %% garbage!
```

Ensure you know what a binary contains before converting it to a string. If no
other option exists, try heuristics:

```erlang
if_you_can_not_know() ->
  {ok,Bin} = file:read_file("maybe_utf8_encoded_file.txt"),
  MyList = case unicode:characters_to_list(Bin) of
    L when is_list(L) ->
      L;
    _ ->
      binary_to_list(Bin) %% The file was bytewise encoded
  end,
  %% Now we know that the list is a Unicode string, not a list of UTF-8 bytes
  {ok,G} = file:open("greatness.txt",[write,{encoding,utf8}]),
  io:put_chars(G,MyList), %% Expects a Unicode string, which is what it gets!
  file:close(G). %% The file contains valid UTF-8 encoded Unicode characters!
```
