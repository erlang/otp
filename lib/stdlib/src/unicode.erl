%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
-module(unicode).
-moduledoc """
Functions for converting Unicode characters.

This module contains functions for converting between different character
representations. It converts between ISO Latin-1 characters and Unicode
characters, but it can also convert between different Unicode encodings (like
UTF-8, UTF-16, and UTF-32).

The default Unicode encoding in Erlang binaries is UTF-8, which is also the
format in which built-in functions and libraries in OTP expect to find binary
Unicode data. In lists, Unicode data is encoded as integers, each integer
representing one character and encoded simply as the Unicode code point for the
character.

Other Unicode encodings than integers representing code points or UTF-8 in
binaries are referred to as "external encodings". The ISO Latin-1 encoding is in
binaries and lists referred to as latin1-encoding.

It is recommended to only use external encodings for communication with external
entities where this is required. When working inside the Erlang/OTP environment,
it is recommended to keep binaries in UTF-8 when representing Unicode
characters. ISO Latin-1 encoding is supported both for backward compatibility
and for communication with external entities not supporting Unicode character
sets.

Programs should always operate on a normalized form and compare
canonical-equivalent Unicode characters as equal. All characters should thus be
normalized to one form once on the system borders. One of the following
functions can convert characters to their normalized forms
`characters_to_nfc_list/1`, `characters_to_nfc_binary/1`,
`characters_to_nfd_list/1` or `characters_to_nfd_binary/1`. For general text
`characters_to_nfc_list/1` or `characters_to_nfc_binary/1` is preferred, and for
identifiers one of the compatibility normalization functions, such as
`characters_to_nfkc_list/1`, is preferred for security reasons. The
normalization functions where introduced in OTP 20. Additional information on
normalization can be found in the
[Unicode FAQ](http://unicode.org/faq/normalization.html).
""".

-compile(nowarn_deprecated_catch).

-export([characters_to_list/1, characters_to_list_int/2,
	 characters_to_binary/1, characters_to_binary_int/2,
	 characters_to_binary/3,
	 bom_to_encoding/1, encoding_to_bom/1,
         characters_to_nfd_list/1, characters_to_nfd_binary/1,
         characters_to_nfc_list/1, characters_to_nfc_binary/1,
         characters_to_nfkd_list/1, characters_to_nfkd_binary/1,
         characters_to_nfkc_list/1, characters_to_nfkc_binary/1
        ]).

-export_type([chardata/0, charlist/0, encoding/0, external_chardata/0,
              external_charlist/0, latin1_char/0, latin1_chardata/0,
              latin1_charlist/0, latin1_binary/0, unicode_binary/0]).

-type encoding()  :: 'latin1' | 'unicode' | 'utf8'
                   | 'utf16' | {'utf16', endian()}
                   | 'utf32' | {'utf32', endian()}.
-type endian()    :: 'big' | 'little'.
-doc "A `t:binary/0` with characters encoded in the UTF-8 coding standard.".
-type unicode_binary() :: binary().
-type charlist() ::
        maybe_improper_list(char() | unicode_binary() | charlist(),
                            unicode_binary() | nil()).
-type chardata() :: charlist() | unicode_binary().
-doc """
A `t:binary/0` with characters coded in a user-specified Unicode encoding other
than UTF-8 (that is, UTF-16 or UTF-32).
""".
-type external_unicode_binary() :: binary().
-type external_chardata() :: external_charlist() | external_unicode_binary().
-type external_charlist() ::
        maybe_improper_list(char() |
                              external_unicode_binary() |
                              external_charlist(),
                            external_unicode_binary() | nil()).
-doc "A `t:binary/0` with characters coded in ISO Latin-1.".
-type latin1_binary() :: binary().
-doc "An `t:integer/0` representing a valid ISO Latin-1 character (0-255).".
-type latin1_char() :: byte().
-doc "Equivalent to `t:iodata/0`.".
-type latin1_chardata() :: latin1_charlist() | latin1_binary().
-doc "Equivalent to `t:iolist/0`.".
-type latin1_charlist() ::
        maybe_improper_list(latin1_char() |
                              latin1_binary() |
                              latin1_charlist(),
                            latin1_binary() | nil()).

%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [badarg_with_info/1, error_with_info/2]}).

%%% BIFs
%%%
%%% characters_to_binary/2 (will trap to characters_to_binary_int/2
%%%                         if InEncoding is not {latin1 | unicode | utf8})
%%% characters_to_list/2   (will trap to characters_to_list_int/2 if
%%%                         InEncoding is not {latin1 | unicode | utf8})

-export([bin_is_7bit/1, characters_to_binary/2, characters_to_list/2]).

-doc false.
-spec bin_is_7bit(Binary) -> boolean() when
      Binary :: binary().

bin_is_7bit(_) ->
    erlang:nif_error(undef).

-doc #{ equiv => characters_to_binary(Data, InEncoding, unicode) }.
-spec characters_to_binary(Data, InEncoding) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      InEncoding :: encoding(),
      Result :: binary()
              | {error, binary(), RestData}
              | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_binary(_, _) ->
    erlang:nif_error(undef).

-doc """
Converts a possibly deep list of integers and binaries into a list of integers
representing Unicode characters. The binaries in the input can have characters
encoded as one of the following:

- ISO Latin-1 (0-255, one character per byte). Here, case parameter `InEncoding`
  is to be specified as `latin1`.
- One of the UTF-encodings, which is specified as parameter `InEncoding`.

Note that integers in the list always represent code points regardless of
`InEncoding` passed. If `InEncoding latin1` is passed, only code points < 256
are allowed; otherwise, all valid unicode code points are allowed.

If `InEncoding` is `latin1`, parameter `Data` corresponds to the `t:iodata/0`
type, but for `unicode`, parameter `Data` can contain integers > 255 (Unicode
characters beyond the ISO Latin-1 range), which makes it invalid as
`t:iodata/0`.

The purpose of the function is mainly to convert combinations of Unicode
characters into a pure Unicode string in list representation for further
processing. For writing the data to an external entity, the reverse function
`characters_to_binary/3` comes in handy.

Option `unicode` is an alias for `utf8`, as this is the preferred encoding for
Unicode characters in binaries. `utf16` is an alias for `{utf16,big}` and
`utf32` is an alias for `{utf32,big}`. The atoms `big` and `little` denote big-
or little-endian encoding.

If the data cannot be converted, either because of illegal Unicode/ISO Latin-1
characters in the list, or because of invalid UTF encoding in any binaries, an
error tuple is returned. The error tuple contains the tag `error`, a list
representing the characters that could be converted before the error occurred
and a representation of the characters including and after the offending
integer/bytes. The last part is mostly for debugging, as it still constitutes a
possibly deep or mixed list, or both, not necessarily of the same depth as the
original data. The error occurs when traversing the list and whatever is left to
decode is returned "as is".

However, if the input `Data` is a pure binary, the third part of the error tuple
is guaranteed to be a binary as well.

Errors occur for the following reasons:

- Integers out of range.

  If `InEncoding` is `latin1`, an error occurs whenever an integer > 255 is
  found in the lists.

  If `InEncoding` is of a Unicode type, an error occurs whenever either of the
  following is found:

  - An integer > 16#10FFFF (the maximum Unicode character)
  - An integer in the range 16#D800 to 16#DFFF (invalid range reserved for
    UTF-16 surrogate pairs)

- Incorrect UTF encoding.

  If `InEncoding` is one of the UTF types, the bytes in any binaries must be
  valid in that encoding.

  Errors can occur for various reasons, including the following:

  - "Pure" decoding errors (like the upper bits of the bytes being wrong).
  - The bytes are decoded to a too large number.
  - The bytes are decoded to a code point in the invalid Unicode range.
  - Encoding is "overlong", meaning that a number should have been encoded in
    fewer bytes.

  The case of a truncated UTF is handled specially, see the paragraph about
  incomplete binaries below.

  If `InEncoding` is `latin1`, binaries are always valid as long as they contain
  whole bytes, as each byte falls into the valid ISO Latin-1 range.

A special type of error is when no actual invalid integers or bytes are found,
but a trailing `t:binary/0` consists of too few bytes to decode the last
character. This error can occur if bytes are read from a file in chunks or if
binaries in other ways are split on non-UTF character boundaries. An
`incomplete` tuple is then returned instead of the `error` tuple. It consists of
the same parts as the `error` tuple, but the tag is `incomplete` instead of
`error` and the last element is always guaranteed to be a binary consisting of
the first part of a (so far) valid UTF character.

If one UTF character is split over two consecutive binaries in the `Data`, the
conversion succeeds. This means that a character can be decoded from a range of
binaries as long as the whole range is specified as input without errors
occurring.

_Example:_

```erlang
decode_data(Data) ->
   case unicode:characters_to_list(Data,unicode) of
      {incomplete,Encoded, Rest} ->
            More = get_some_more_data(),
            Encoded ++ decode_data([Rest, More]);
      {error,Encoded,Rest} ->
            handle_error(Encoded,Rest);
      List ->
            List
   end.
```

However, bit strings that are not whole bytes are not allowed, so a UTF
character must be split along 8-bit boundaries to ever be decoded.

A `badarg` exception is thrown for the following cases:

- Any parameters are of the wrong type.
- The list structure is invalid (a number as tail).
- The binaries do not contain whole bytes (bit strings).
""".
-spec characters_to_list(Data,  InEncoding) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      InEncoding :: encoding(),
      Result ::string()
              | {error, string(), RestData}
              | {incomplete, string(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_list(_, _) ->
    erlang:nif_error(undef).

%%% End of BIFs

-doc #{ equiv => characters_to_list(Data, unicode) }.
-spec characters_to_list(Data) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      Result :: string()
              | {error, string(), RestData}
              | {incomplete, string(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_list(ML) ->
    try
        unicode:characters_to_list(ML, unicode)
    catch
        error:Reason ->
            error_with_info(Reason, [ML])
    end.

-doc #{ equiv => characters_to_binary(Data, unicode, unicode) }.
-spec characters_to_binary(Data) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      Result :: binary()
              | {error, binary(), RestData}
              | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_binary(ML) ->
    try
	unicode:characters_to_binary(ML,unicode)
    catch
	error:Reason ->
            error_with_info(error_type(Reason), [ML])
    end.

-doc """
Behaves as `characters_to_list/2`, but produces a binary instead of a Unicode
list.

`InEncoding` defines how input is to be interpreted if binaries are present in
`Data`

`OutEncoding` defines in what format output is to be generated.

Options:

- **`unicode`** - An alias for `utf8`, as this is the preferred encoding for
  Unicode characters in binaries.

- **`utf16`** - An alias for `{utf16,big}`.

- **`utf32`** - An alias for `{utf32,big}`.

The atoms `big` and `little` denote big- or little-endian encoding.

Errors and exceptions occur as in `characters_to_list/2`, but the second element
in tuple `error` or `incomplete` is a `t:binary/0` and not a `t:list/0`.
""".
-spec characters_to_binary(Data, InEncoding, OutEncoding) -> Result when
      Data :: latin1_chardata() | chardata() | external_chardata(),
      InEncoding :: encoding(),
      OutEncoding :: encoding(),
      Result :: binary()
              | {error, binary(), RestData}
              | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().

characters_to_binary(ML, InEncoding, OutEncoding) ->
    case no_conversion_needed(ML, InEncoding, OutEncoding) of
        true ->
            ML;
        false ->
            try
                characters_to_binary_int(ML, InEncoding, OutEncoding)
            catch
                error:Reason ->
                    error_with_info(error_type(Reason),
                                    [ML, InEncoding, OutEncoding])
            end
    end.

no_conversion_needed(ML, latin1, latin1) ->
    is_binary(ML);
no_conversion_needed(ML, In, Out) ->
    case {In,Out} of
        {latin1,utf8} -> true;
        {latin1,unicode} -> true;
        {utf8,latin1} -> true;
        {unicode,latin1} -> true;
        {_,_} -> false
    end andalso unicode:bin_is_7bit(ML).

-doc """
Checks for a UTF Byte Order Mark (BOM) in the beginning of a binary.

If the supplied binary `Bin` begins with a valid BOM for either UTF-8, UTF-16, or
UTF-32, the function returns the encoding identified along with the BOM length
in bytes.

If no BOM is found, the function returns `{latin1,0}`.
""".
-spec bom_to_encoding(Bin) -> {Encoding, Length} when
      Bin :: binary(),
      Encoding ::  'latin1' | 'utf8'
                 | {'utf16', endian()}
                 | {'utf32', endian()},
      Length :: non_neg_integer().

bom_to_encoding(<<239,187,191,_/binary>>) ->
    {utf8,3};
bom_to_encoding(<<0,0,254,255,_/binary>>) ->
    {{utf32,big},4};
bom_to_encoding(<<255,254,0,0,_/binary>>) ->
    {{utf32,little},4};
bom_to_encoding(<<254,255,_/binary>>) ->
    {{utf16,big},2};
bom_to_encoding(<<255,254,_/binary>>) ->
    {{utf16,little},2};
bom_to_encoding(Bin) when is_binary(Bin) ->
    {latin1,0}.

-doc """
Creates a UTF Byte Order Mark (BOM) as a binary from the supplied `InEncoding`.

The BOM is, if supported at all, expected to be placed first in UTF encoded
files or messages.

The function returns `<<>>` for `latin1` encoding, as there is no BOM for ISO
Latin-1.

Notice that the BOM for UTF-8 is seldom used, and it is really not a _byte
order_ mark. There are obviously no byte order issues with UTF-8, so the BOM is
only there to differentiate UTF-8 encoding from other UTF formats.
""".
-spec encoding_to_bom(InEncoding) -> Bin when
      Bin :: binary(),
      InEncoding :: encoding().

encoding_to_bom(unicode) ->
    <<239,187,191>>;
encoding_to_bom(utf8) ->
    <<239,187,191>>;
encoding_to_bom(utf16) ->
    <<254,255>>;
encoding_to_bom({utf16,big}) ->
    <<254,255>>;
encoding_to_bom({utf16,little}) ->
    <<255,254>>;
encoding_to_bom(utf32) ->
    <<0,0,254,255>>;
encoding_to_bom({utf32,big}) ->
    <<0,0,254,255>>;
encoding_to_bom({utf32,little}) ->
    <<255,254,0,0>>;
encoding_to_bom(latin1) ->
    <<>>.

-define(GC_N, 200). %% arbitrary number

%% Canonical decompose string to list of chars
-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Decomposed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
1> unicode:characters_to_nfd_list("abc..åäö").
[97,98,99,46,46,97,778,97,776,111,776]
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfd_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfd_list(CD) ->
    try
        characters_to_nfd_list(CD, [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfd_list(CD, Acc) ->
    case unicode_util:nfd(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfd_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfd_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Decomposed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
2> unicode:characters_to_nfd_binary("abc..åäö").
<<97,98,99,46,46,97,204,138,97,204,136,111,204,136>>
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfd_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfd_binary(CD) ->
    try
        characters_to_nfd_binary(CD, ?GC_N, [], [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfd_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfd(CD) of
        [GC|Str] -> characters_to_nfd_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfd_binary(CD, _, Row, Acc) ->
    characters_to_nfd_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

%% Compability Canonical decompose string to list of chars.
-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Decomposed characters according to the Unicode
standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
1> unicode:characters_to_nfkd_list(["abc..åäö",[65299,65298]]).
[97,98,99,46,46,97,778,97,776,111,776,51,50]
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfkd_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfkd_list(CD) ->
    try
        characters_to_nfkd_list(CD, [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfkd_list(CD, Acc) ->
    case unicode_util:nfkd(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfkd_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfkd_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Decomposed characters according to the Unicode
standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
2> unicode:characters_to_nfkd_binary(["abc..åäö",[65299,65298]]).
<<97,98,99,46,46,97,204,138,97,204,136,111,204,136,51,50>>
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfkd_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfkd_binary(CD) ->
    try
        characters_to_nfkd_binary(CD, ?GC_N, [], [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfkd_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfkd(CD) of
        [GC|Str] -> characters_to_nfkd_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfkd_binary(CD, _, Row, Acc) ->
    characters_to_nfkd_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).


%% Canonical compose string to list of chars
-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
3> unicode:characters_to_nfc_list([<<"abc..a">>,[778],$a,[776],$o,[776]]).
"abc..åäö"
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfc_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfc_list(CD) ->
    try
        characters_to_nfc_list(CD, [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfc_list(CD, Acc) ->
    case unicode_util:nfc(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfc_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfc_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
4> unicode:characters_to_nfc_binary([<<"abc..a">>,[778],$a,[776],$o,[776]]).
<<"abc..åäö"/utf8>>
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfc_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfc_binary(CD) ->
    try
        characters_to_nfc_binary(CD, ?GC_N, [], [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfc_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfc(CD) of
        [GC|Str] -> characters_to_nfc_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfc_binary(CD, _, Row, Acc) ->
    characters_to_nfc_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

%% Compability Canonical compose string to list of chars
-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
3> unicode:characters_to_nfkc_list([<<"abc..a">>,[778],$a,[776],$o,[776],[65299,65298]]).
"abc..åäö32"
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfkc_list(chardata()) -> [char()] | {error, [char()], chardata()}.
characters_to_nfkc_list(CD) ->
    try
        characters_to_nfkc_list(CD, [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfkc_list(CD, Acc) ->
    case unicode_util:nfkc(CD) of
        [GC|Str] when is_list(GC) -> characters_to_nfkc_list(Str, lists:reverse(GC, Acc));
        [CP|Str] -> characters_to_nfkc_list(Str, [CP | Acc]);
        [] -> lists:reverse(Acc);
        {error,Error} -> {error, lists:reverse(Acc), Error}
    end.

-doc """
Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
4> unicode:characters_to_nfkc_binary([<<"abc..a">>,[778],$a,[776],$o,[776],[65299,65298]]).
<<"abc..åäö32"/utf8>>
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec characters_to_nfkc_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
characters_to_nfkc_binary(CD) ->
    try
        characters_to_nfkc_binary(CD, ?GC_N, [], [])
    catch
        error:_ ->
            badarg_with_info([CD])
    end.

characters_to_nfkc_binary(CD, N, Row, Acc) when N > 0 ->
    case unicode_util:nfkc(CD) of
        [GC|Str] -> characters_to_nfkc_binary(Str, N-1, [GC|Row], Acc);
        [] -> acc_to_binary(prepend_row_to_acc(Row, Acc));
        {error, Error} -> {error, acc_to_binary(prepend_row_to_acc(Row, Acc)), Error}
    end;
characters_to_nfkc_binary(CD, _, Row, Acc) ->
    characters_to_nfkc_binary(CD, ?GC_N, [], prepend_row_to_acc(Row, Acc)).

acc_to_binary(Acc) ->
    list_to_binary(lists:reverse(Acc)).
prepend_row_to_acc(Row, Acc) ->
    [characters_to_binary(lists:reverse(Row))|Acc].

%% internals

-doc false.
characters_to_list_int(ML, Encoding) ->
    try
	do_characters_to_list(ML, Encoding)
    catch
	error:Reason ->
            fake_stacktrace(Reason, characters_to_list, [ML, Encoding])
    end.

                                                % XXX: Optimize me!
do_characters_to_list(ML, Encoding) ->
    case unicode:characters_to_binary(ML,Encoding) of
	Bin when is_binary(Bin) ->
	    unicode:characters_to_list(Bin,utf8);
	{error,Encoded,Rest} ->
	    {error,unicode:characters_to_list(Encoded,utf8),Rest};
	{incomplete, Encoded2, Rest2} ->
	    {incomplete,unicode:characters_to_list(Encoded2,utf8),Rest2}
    end.


-doc false.
characters_to_binary_int(ML, InEncoding) ->
    try
	characters_to_binary_int(ML, InEncoding, unicode)
    catch
	error:Reason ->
            fake_stacktrace(Reason, characters_to_binary, [ML, InEncoding])
    end.

-spec fake_stacktrace(term(), atom(), [term()]) -> no_return().

fake_stacktrace(Reason, Name, Args) ->
    try
        error(new_stacktrace, Args)
    catch
        error:_:Stk0 ->
            [{Mod,_,Args,_}|Stk1] = Stk0,
            Info = [{error_info,#{module => erl_stdlib_errors}}],
            Stk = [{Mod,Name,Args,Info}|Stk1],
            erlang:raise(error, error_type(Reason), Stk)
    end.

characters_to_binary_int(ML, InEncoding, OutEncoding) when
      InEncoding =:= latin1, OutEncoding =:= unicode;
      InEncoding =:= latin1, OutEncoding =:= utf8;
      InEncoding =:= unicode, OutEncoding =:= unicode;
      InEncoding =:= unicode, OutEncoding =:= utf8;
      InEncoding =:= utf8, OutEncoding =:= unicode;
      InEncoding =:= utf8, OutEncoding =:= utf8 ->
    unicode:characters_to_binary(ML,InEncoding);

characters_to_binary_int(ML, InEncoding, OutEncoding) ->
    {InTrans,Limit} = case OutEncoding of
                          latin1 -> {i_trans_chk(InEncoding),255};
                          _ -> {i_trans(InEncoding),case InEncoding of latin1 -> 255; _ -> 16#10FFFF end}
                      end,
    OutTrans = o_trans(OutEncoding),
    Res =
	ml_map(ML,
	       fun(Part,Accum) when is_binary(Part) ->
		       case InTrans(Part) of
			   List when is_list(List) ->
			       Tail = OutTrans(List),
			       <<Accum/binary, Tail/binary>>;
			   {error, Translated, Rest} ->
			       Tail = OutTrans(Translated),
			       {error, <<Accum/binary,Tail/binary>>, Rest};
			   {incomplete, Translated, Rest, Missing}  ->
			       Tail = OutTrans(Translated),
			       {incomplete, <<Accum/binary,Tail/binary>>, Rest,
				Missing}
		       end;
		  (Part, Accum) when is_integer(Part), Part =< Limit ->
		       case OutTrans([Part]) of
			   Binary when is_binary(Binary) ->
			       <<Accum/binary, Binary/binary>>;
			   {error, _, [Part]} ->
			       {error,Accum,[Part]}
		       end;
		  (Part, Accum) ->
		       {error, Accum, [Part]}
	       end,<<>>),
    case Res of
        Bin when is_binary(Bin) ->
            Bin;
	{incomplete,A,B,_} ->
	    {incomplete,A,B};
        {error, _Converted, _Rest} = Error ->
            Error
    end.


cbv(utf8,<<1:1,1:1,0:1,_:5>>) ->
    1;
cbv(utf8,<<1:1,1:1,1:1,0:1,_:4,R/binary>>) ->
    case R of
	<<>> ->
	    2;
	<<1:1,0:1,_:6>> ->
	    1;
	_ ->
	    false
    end;
cbv(utf8,<<1:1,1:1,1:1,1:1,0:1,_:3,R/binary>>) ->
    case R of
	<<>> ->
	    3;
	<<1:1,0:1,_:6>> ->
	    2;
	<<1:1,0:1,_:6,1:1,0:1,_:6>> ->
	    1;
	_ ->
	    false
    end;
cbv(utf8,_) ->
    false;

cbv({utf16,big},<<A:8>>) when A =< 215; A >= 224 ->
    1;
cbv({utf16,big},<<54:6,_:2>>) ->
    3;
cbv({utf16,big},<<54:6,_:10>>) ->
    2;
cbv({utf16,big},<<54:6,_:10,55:6,_:2>>) ->
    1;
cbv({utf16,big},_) ->
    false;
cbv({utf16,little},<<_:8>>) ->
    1; % or 3, we'll see
cbv({utf16,little},<<_:8,54:6,_:2>>) ->
    2;
cbv({utf16,little},<<_:8,54:6,_:2,_:8>>) ->
    1;
cbv({utf16,little},_) ->
    false;


cbv({utf32,big}, <<0:8>>) ->
    3;
cbv({utf32,big}, <<0:8,X:8>>) when X =< 16 ->
    2;
cbv({utf32,big}, <<0:8,X:8,Y:8>>)
  when X =< 16, ((X > 0) or ((Y =< 215) or (Y >= 224))) ->
    1;
cbv({utf32,big},_) ->
    false;
cbv({utf32,little},<<_:8>>) ->
    3;
cbv({utf32,little},<<_:8,_:8>>) ->
    2;
cbv({utf32,little},<<X:8,255:8,0:8>>) when X =:= 254; X =:= 255 ->
    false;
cbv({utf32,little},<<_:8,Y:8,X:8>>)
  when X =< 16, ((X > 0) or ((Y =< 215) or (Y >= 224))) ->
    1;
cbv({utf32,little},_) ->
    false.


ml_map([],_,{{Inc,X},Accum}) ->
    {incomplete, Accum, Inc, X};
ml_map([],_Fun,Accum) ->
    Accum;
ml_map([Part|_] = Whole,_,{{Incomplete, _}, Accum}) when is_integer(Part) ->
    {error, Accum, [Incomplete | Whole]};
ml_map([Part|T],Fun,Accum) when is_integer(Part) ->
    case Fun(Part,Accum) of
	Bin when is_binary(Bin) ->
	    ml_map(T,Fun,Bin);
	% Can not be incomplete - it's an integer
	{error, Converted, Rest} ->
	    {error, Converted, [Rest|T]}
    end;
ml_map([Part|T],Fun,{{Incomplete,Missing}, Accum}) when is_binary(Part) ->
    % Ok, how much is needed to fill in the incomplete part?
    case byte_size(Part) of
	N when N >= Missing ->
	    <<FillIn:Missing/binary,Trailing/binary>> = Part,
	    NewPart = <<Incomplete/binary,FillIn/binary>>,
	    ml_map([NewPart,Trailing|T], Fun, Accum);
	M ->
	    NewIncomplete = <<Incomplete/binary, Part/binary>>,
	    NewMissing = Missing - M,
	    ml_map(T,Fun,{{NewIncomplete, NewMissing}, Accum})
    end;
ml_map([Part|T],Fun,Accum) when is_binary(Part), byte_size(Part) > 8192 ->
    <<Part1:8192/binary,Part2/binary>> = Part,
    ml_map([Part1,Part2|T],Fun,Accum);
ml_map([Part|T],Fun,Accum) when is_binary(Part) ->
    case Fun(Part,Accum) of
	Bin when is_binary(Bin) ->
	    ml_map(T,Fun,Bin);
	{incomplete, Converted, Rest, Missing} ->
	    ml_map(T,Fun,{{Rest, Missing},Converted});
	{error, Converted, Rest} ->
	    {error, Converted, [Rest|T]}
    end;
ml_map([List|T],Fun,Accum) when is_list(List) ->
    case ml_map(List,Fun,Accum) of
	Bin when is_binary(Bin) ->
	    ml_map(T,Fun,Bin);
	{error, Converted,Rest} ->
	    {error, Converted, [Rest | T]};
	{incomplete, Converted,Rest,N} ->
	    ml_map(T,Fun,{{Rest,N},Converted})
    end;
ml_map(Bin,Fun,{{Incomplete,Missing},Accum}) when is_binary(Bin) ->
    case byte_size(Bin) of
	N when N >= Missing ->
	    ml_map([Incomplete,Bin],Fun,Accum);
	M ->
	    {incomplete, Accum, <<Incomplete/binary, Bin/binary>>, Missing - M}
    end;
ml_map(Part,Fun,Accum) when is_binary(Part), byte_size(Part) > 8192 ->
     <<Part1:8192/binary,Part2/binary>> = Part,
    ml_map([Part1,Part2],Fun,Accum);
ml_map(Bin,Fun,Accum) when is_binary(Bin) ->
    Fun(Bin,Accum).





i_trans(latin1) ->
    fun(Bin) -> binary_to_list(Bin) end;
i_trans(unicode) ->
    i_trans(utf8);
i_trans(utf8) ->
    fun do_i_utf8/1;
i_trans(utf16) ->
    fun do_i_utf16_big/1;
i_trans({utf16,big}) ->
    fun do_i_utf16_big/1;
i_trans({utf16,little}) ->
    fun do_i_utf16_little/1;
i_trans(utf32) ->
    fun do_i_utf32_big/1;
i_trans({utf32,big}) ->
    fun do_i_utf32_big/1;
i_trans({utf32,little}) ->
    fun do_i_utf32_little/1.

i_trans_chk(latin1) ->
    fun(Bin) -> binary_to_list(Bin) end;
i_trans_chk(unicode) ->
    i_trans_chk(utf8);
i_trans_chk(utf8) ->
    fun do_i_utf8_chk/1;
i_trans_chk(utf16) ->
    fun do_i_utf16_big_chk/1;
i_trans_chk({utf16,big}) ->
    fun do_i_utf16_big_chk/1;
i_trans_chk({utf16,little}) ->
    fun do_i_utf16_little_chk/1;
i_trans_chk(utf32) ->
    fun do_i_utf32_big_chk/1;
i_trans_chk({utf32,big}) ->
    fun do_i_utf32_big_chk/1;
i_trans_chk({utf32,little}) ->
    fun do_i_utf32_little_chk/1.

o_trans(latin1) ->
    fun(L) -> list_to_binary(L) end;
o_trans(unicode) ->
    o_trans(utf8);
o_trans(utf8) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf8>>
			end, L)
    end;

o_trans(utf16) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf16>>
			end, L)
    end;
o_trans({utf16,big}) ->
    o_trans(utf16);
o_trans({utf16,little}) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf16-little>>
			end, L)
    end;
o_trans(utf32) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf32>>
			end, L)
    end;
o_trans({utf32,big}) ->
    o_trans(utf32);
o_trans({utf32,little}) ->
    fun(L) ->
	    do_o_binary(fun(One) ->
				<<One/utf32-little>>
			end, L)
    end.

do_o_binary(F,L) ->
    case do_o_binary2(F,L) of
	{Tag,List,R} ->
	    {Tag,erlang:iolist_to_binary(List),R};
	List ->
	    erlang:iolist_to_binary(List)
    end.

-dialyzer({no_improper_lists, do_o_binary2/2}).

do_o_binary2(_F,[]) ->
    <<>>;
do_o_binary2(F,[H|T]) ->
    case (catch F(H)) of
	{'EXIT',_} ->
	    {error,<<>>,[H|T]};
	Bin when is_binary(Bin) ->
	    case do_o_binary2(F,T) of
		{error,Bin2,Rest} ->
		    {error,[Bin|Bin2],Rest};
		Bin3 ->
		    [Bin|Bin3]
	    end
    end.

%% Specific functions only allowing codepoints in latin1 range

do_i_utf8_chk(<<>>) ->
    [];
do_i_utf8_chk(<<U/utf8,R/binary>>) when U =< 255 ->
    case do_i_utf8_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf8_chk(<<_/utf8,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf8_chk(Bin) when is_binary(Bin) ->
    case cbv(utf8,Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin,N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf16_big_chk(<<>>) ->
    [];
do_i_utf16_big_chk(<<U/utf16,R/binary>>) when U =< 255 ->
    case do_i_utf16_big_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_big_chk(<<_/utf16,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf16_big_chk(Bin) when is_binary(Bin) ->
    case cbv({utf16,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf16_little_chk(<<>>) ->
    [];
do_i_utf16_little_chk(<<U/utf16-little,R/binary>>) when U =< 255 ->
    case do_i_utf16_little_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_little_chk(<<_/utf16-little,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf16_little_chk(Bin) when is_binary(Bin) ->
    case cbv({utf16,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.


do_i_utf32_big_chk(<<>>) ->
    [];
do_i_utf32_big_chk(<<U/utf32,R/binary>>) when U =< 255 ->
    case do_i_utf32_big_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_big_chk(<<_/utf32,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf32_big_chk(Bin) when is_binary(Bin) ->
    case cbv({utf32,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf32_little_chk(<<>>) ->
    [];
do_i_utf32_little_chk(<<U/utf32-little,R/binary>>) when U =< 255 ->
    case do_i_utf32_little_chk(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_little_chk(<<_/utf32-little,_/binary>> = Bin) ->
    {error, [], Bin};
do_i_utf32_little_chk(Bin) when is_binary(Bin) ->
    case cbv({utf32,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.


%% General versions

do_i_utf8(<<>>) ->
    [];
do_i_utf8(<<U/utf8,R/binary>>) ->
    case do_i_utf8(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf8(Bin) when is_binary(Bin) ->
    case cbv(utf8,Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin,N};
	false ->
	    {error, [], Bin}
    end.

do_i_utf16_big(<<>>) ->
    [];
do_i_utf16_big(<<U/utf16,R/binary>>) ->
    case do_i_utf16_big(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_big(Bin) when is_binary(Bin) ->
    case cbv({utf16,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf16_little(<<>>) ->
    [];
do_i_utf16_little(<<U/utf16-little,R/binary>>) ->
    case do_i_utf16_little(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf16_little(Bin) when is_binary(Bin) ->
    case cbv({utf16,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.


do_i_utf32_big(<<>>) ->
    [];
do_i_utf32_big(<<U/utf32,R/binary>>) ->
    case do_i_utf32_big(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_big(Bin) when is_binary(Bin) ->
    case cbv({utf32,big},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.
do_i_utf32_little(<<>>) ->
    [];
do_i_utf32_little(<<U/utf32-little,R/binary>>) ->
    case do_i_utf32_little(R) of
	{error,Trans,Rest} ->
	    {error, [U|Trans], Rest};
	{incomplete,Trans,Rest,N} ->
	    {incomplete, [U|Trans], Rest, N};
	L when is_list(L) ->
	    [U|L]
    end;
do_i_utf32_little(Bin) when is_binary(Bin) ->
    case cbv({utf32,little},Bin) of
	N when is_integer(N) ->
	    {incomplete, [], Bin, N};
	false ->
	    {error, [], Bin}
    end.

error_type(system_limit=SL) -> SL;
error_type(_) -> badarg.

badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_stdlib_errors}}]).
