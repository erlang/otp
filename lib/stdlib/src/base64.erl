%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
%% Description: Implements base 64 encode and decode. See RFC4648.

-module(base64).
-moduledoc """
Provides base64 encode and decode, see RFC 2045.

Provides base64 encode and decode, see
[RFC 2045](https://www.ietf.org/rfc/rfc2045.txt).
""".

-export([encode/1, encode/2,
	 decode/1, decode/2,
	 mime_decode/1, mime_decode/2,
	 encode_to_string/1, encode_to_string/2,
	 decode_to_string/1, decode_to_string/2,
	 mime_decode_to_string/1, mime_decode_to_string/2,
         format_error/2]).

%% RFC 4648: Base 64 Encoding alphabet
-doc """
Base 64 Encoding alphabet, see
[RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648).
""".
-type base64_alphabet() :: $A..$Z | $a..$z | $0..$9 | $+ | $/ | $- | $_ | $=.

%% Selector for the Base 64 alphabet, `standard'  for RFC 4648
%% Section 4, `urlsafe'  for RFC 4648 Section 5.
-doc """
Selector for the Base 64 Encoding alphabet used for [encoding](`encode/2`) and
[decoding](`decode/2`). See
[RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648) Sections
[4](https://datatracker.ietf.org/doc/html/rfc4648#section-4) and
[5](https://datatracker.ietf.org/doc/html/rfc4648#section-5).
""".
-type base64_mode() :: 'standard' | 'urlsafe'.

-doc """
Customises the behaviour of the encode and decode functions. Default value if
omitted entirely or partially is `#{mode => standard, padding => true}`.
""".
-type options() :: #{padding => boolean(), mode => base64_mode()}.

%% The following type is a subtype of string() for return values
%% of encoding functions.
-doc "Base 64 encoded string.".
-type base64_string() :: [base64_alphabet()].
-doc "Base 64 encoded binary.".
-type base64_binary() :: binary().

%% Decoded sequence of octets
-doc "Arbitrary sequences of octets.".
-type byte_string() :: [byte()].

-doc """
Encodes a plain ASCII string into base64 using the standard alphabet according
to
[RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4).
The result is 33% larger than the data.

Always appends correct number of `=` padding characters to the encoded string.
""".
-spec encode_to_string(Data) -> Base64String when
      Data :: byte_string() | binary(),
      Base64String :: base64_string().

encode_to_string(Data) ->
    encode_to_string(Data, #{}).

-doc """
Encodes a plain ASCII string into base64 using the alphabet indicated by the
`mode` option. The result is 33% larger than the data.

The `mode` option can be one of the following:

- **`standard`** - Default. Encode the given string using the standard base64
  alphabet according to
  [RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4).

- **`urlsafe`** - Encode the given string using the alternative "URL and
  Filename safe" base64 alphabet according to
  [RFC 4648 Section 5](https://datatracker.ietf.org/doc/html/rfc4648#section-5).

The `padding` option can be one of the following:

- **`true`** - Default. Appends correct number of `=` padding characters to the
  encoded string.

- **`false`** - Skips appending `=` padding characters to the encoded string.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec encode_to_string(Data, Options) -> Base64String when
      Data :: byte_string() | binary(),
      Options :: options(),
      Base64String :: base64_string().

encode_to_string(Bin, Options) when is_binary(Bin), is_map(Options) ->
    encode_to_string(binary_to_list(Bin), Options);
encode_to_string(List, Options) when is_list(List), is_map(Options) ->
    encode_list_to_string(get_encoding_offset(Options), get_padding(Options), List).

-doc(#{equiv => encode_to_string/1}).
-spec encode(Data) -> Base64 when
      Data :: byte_string() | binary(),
      Base64 :: base64_binary().

encode(Data) ->
    encode(Data, #{}).

-doc(#{equiv => encode_to_string/2}).
-doc(#{since => <<"OTP 26.0">>}).
-spec encode(Data, Options) -> Base64 when
      Data :: byte_string() | binary(),
      Options :: options(),
      Base64 :: base64_binary().

encode(Bin, Options) when is_binary(Bin), is_map(Options) ->
    encode_binary(get_encoding_offset(Options), get_padding(Options), Bin, <<>>);
encode(List, Options) when is_list(List) ->
    encode_list(get_encoding_offset(Options), get_padding(Options), List, <<>>).

encode_list_to_string(_ModeOffset, _Padding, []) ->
    [];
encode_list_to_string(ModeOffset, Padding, [B1]) ->
    [b64e(B1 bsr 2, ModeOffset),
     b64e((B1 band 3) bsl 4, ModeOffset) |
     case Padding of
         true -> "==";
         false -> ""
     end];
encode_list_to_string(ModeOffset, Padding, [B1,B2]) ->
    [b64e(B1 bsr 2, ModeOffset),
     b64e(((B1 band 3) bsl 4) bor (B2 bsr 4), ModeOffset),
     b64e((B2 band 15) bsl 2, ModeOffset) |
     case Padding of
         true -> "=";
         false -> ""
     end];
encode_list_to_string(ModeOffset, Padding, [B1,B2,B3|Ls]) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    [b64e(BB bsr 18, ModeOffset),
     b64e((BB bsr 12) band 63, ModeOffset),
     b64e((BB bsr 6) band 63, ModeOffset),
     b64e(BB band 63, ModeOffset) | encode_list_to_string(ModeOffset, Padding, Ls)].

encode_binary(ModeOffset, Padding, <<B1:6, B2:6, B3:6, B4:6, B5:6, B6:6, B7:6, B8:6, Ls/bits>>, A) ->
    encode_binary(ModeOffset,
                  Padding,
                  Ls,
                  <<A/bits,
                    (b64e(B1, ModeOffset)):8,
                    (b64e(B2, ModeOffset)):8,
                    (b64e(B3, ModeOffset)):8,
                    (b64e(B4, ModeOffset)):8,
                    (b64e(B5, ModeOffset)):8,
                    (b64e(B6, ModeOffset)):8,
                    (b64e(B7, ModeOffset)):8,
                    (b64e(B8, ModeOffset)):8>>);
encode_binary(_ModeOffset, _Padding, <<>>, A) ->
    A;
encode_binary(ModeOffset, Padding, <<B1:6, B2:6, B3:6, B4:6, Ls/bits>>, A) ->
    encode_binary(ModeOffset,
                  Padding,
                  Ls,
                  <<A/bits,
                    (b64e(B1, ModeOffset)):8,
                    (b64e(B2, ModeOffset)):8,
                    (b64e(B3, ModeOffset)):8,
                    (b64e(B4, ModeOffset)):8>>);
encode_binary(ModeOffset, Padding, <<B1:6, B2:2>>, A) ->
    E1 = b64e(B1, ModeOffset),
    E2 = b64e(B2 bsl 4, ModeOffset),
    case Padding of
        true -> <<A/bits,E1,E2,$=,$=>>;
        _ -> <<A/bits,E1,E2>>
    end;
encode_binary(ModeOffset, Padding, <<B1:6, B2:6, B3:4>>, A) ->
    E1 = b64e(B1, ModeOffset),
    E2 = b64e(B2, ModeOffset),
    E3 = b64e(B3 bsl 2, ModeOffset),
    case Padding of
        true -> <<A/bits,E1,E2,E3,$=>>;
        _ -> <<A/bits,E1,E2,E3>>
    end.

encode_list(_ModeOffset, _Padding, [], A) ->
    A;
encode_list(ModeOffset, Padding, [B1], A) ->
    E1 = b64e(B1 bsr 2, ModeOffset),
    E2 = b64e((B1 band 3) bsl 4, ModeOffset),
    case Padding of
        true -> <<A/bits,E1,E2,$=,$=>>;
        false -> <<A/bits,E1,E2>>
    end;
encode_list(ModeOffset, Padding, [B1,B2], A) ->
    E1 = b64e(B1 bsr 2, ModeOffset),
    E2 = b64e(((B1 band 3) bsl 4) bor (B2 bsr 4), ModeOffset),
    E3 = b64e((B2 band 15) bsl 2, ModeOffset),
    case Padding of
        true -> <<A/bits,E1,E2,E3,$=>>;
        false -> <<A/bits,E1,E2,E3>>
    end;
encode_list(ModeOffset, Padding, [B1,B2,B3|Ls], A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    encode_list(ModeOffset,
                Padding,
                Ls,
                <<A/bits,(b64e(BB bsr 18, ModeOffset)):8,
                  (b64e((BB bsr 12) band 63, ModeOffset)):8,
                  (b64e((BB bsr 6) band 63, ModeOffset)):8,
                  (b64e(BB band 63, ModeOffset)):8>>).

%% mime_decode strips away all characters not Base64 before
%% converting, whereas decode crashes if an illegal character is found

-doc(#{equiv => mime_decode_to_string/1}).
-spec decode(Base64) -> Data when
      Base64 :: base64_string() | base64_binary(),
      Data :: binary().

decode(Base64) ->
    decode(Base64, #{}).

-doc(#{equiv => mime_decode_to_string/2}).
-doc(#{since => <<"OTP 26.0">>}).
-spec decode(Base64, Options) -> Data when
      Base64 :: base64_string() | base64_binary(),
      Options :: options(),
      Data :: binary().

decode(Bin, Options) when is_binary(Bin) ->
    decode_binary(get_decoding_offset(Options), get_padding(Options), Bin, <<>>);
decode(List, Options) when is_list(List) ->
    decode_list(get_decoding_offset(Options), get_padding(Options), List, <<>>).

-doc(#{equiv => mime_decode_to_string/1}).
-spec mime_decode(Base64) -> Data when
      Base64 :: base64_string() | base64_binary(),
      Data :: binary().

mime_decode(Base64) ->
    mime_decode(Base64, #{}).

-doc(#{equiv => mime_decode_to_string/2}).
-doc(#{since => <<"OTP 26.0">>}).
-spec mime_decode(Base64, Options) -> Data when
      Base64 :: base64_string() | base64_binary(),
      Options :: options(),
      Data :: binary().

mime_decode(Bin, Options) when is_binary(Bin) ->
    mime_decode_binary(get_decoding_offset(Options), get_padding(Options), Bin, <<>>);
mime_decode(List, Options) when is_list(List) ->
    mime_decode_list(get_decoding_offset(Options), get_padding(Options), List, <<>>).

%% mime_decode_to_string strips away all characters not Base64 before
%% converting, whereas decode_to_string crashes if an illegal
%% character is found

-doc(#{equiv => mime_decode_to_string/1}).
-spec decode_to_string(Base64) -> DataString when
      Base64 :: base64_string() | base64_binary(),
      DataString :: byte_string().

decode_to_string(Base64) ->
    decode_to_string(Base64, #{}).

-doc(#{equiv => mime_decode_to_string/2}).
-doc(#{since => <<"OTP 26.0">>}).
-spec decode_to_string(Base64, Options) -> DataString when
      Base64 :: base64_string() | base64_binary(),
      Options :: options(),
      DataString :: byte_string().

decode_to_string(Bin, Options) when is_binary(Bin) ->
    decode_to_string(binary_to_list(Bin), Options);
decode_to_string(List, Options) when is_list(List) ->
    decode_list_to_string(get_decoding_offset(Options), get_padding(Options), List).

-doc """
Decodes a base64 string encoded using the standard alphabet according to
[RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4) to
plain ASCII.

[`mime_decode/1`](`mime_decode/1`) and
[`mime_decode_to_string/1`](`mime_decode_to_string/1`) strip away illegal
characters, while [`decode/1`](`decode/1`) and
[`decode_to_string/1`](`decode_to_string/1`) only strip away whitespace
characters.

Checks the correct number of `=` padding characters at the end of the encoded
string.
""".
-spec mime_decode_to_string(Base64) -> DataString when
      Base64 :: base64_string() | base64_binary(),
      DataString :: byte_string().

mime_decode_to_string(Base64) ->
    mime_decode_to_string(Base64, #{}).

-doc """
Decodes a base64 string encoded using the alphabet indicated by the `mode`
option to plain ASCII.

[`mime_decode/2`](`mime_decode/2`) and
[`mime_decode_to_string/2`](`mime_decode_to_string/2`) strip away illegal
characters, while [`decode/2`](`decode/2`) and
[`decode_to_string/2`](`decode_to_string/2`) only strip away whitespace
characters.

The `mode` option can be one of the following:

- **`standard`** - Default. Decode the given string using the standard base64
  alphabet according to
  [RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4),
  that is `"+"` and `"/"` are representing bytes `62` and `63` respectively,
  while `"-"` and `"_"` are illegal characters.

- **`urlsafe`** - Decode the given string using the alternative "URL and
  Filename safe" base64 alphabet according to
  [RFC 4648 Section 5](https://datatracker.ietf.org/doc/html/rfc4648#section-5),
  that is `"-"` and `"_"` are representing bytes `62` and `63` respectively,
  while `"+"` and `"/"` are illegal characters.

The `padding` option can be one of the following:

- **`true`** - Default. Checks the correct number of `=` padding characters at
  the end of the encoded string.

- **`false`** - Accepts an encoded string with missing `=` padding characters at
  the end.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec mime_decode_to_string(Base64, Options) -> DataString when
      Base64 :: base64_string() | base64_binary(),
      Options :: options(),
      DataString :: byte_string().

mime_decode_to_string(Bin, Options) when is_binary(Bin) ->
    mime_decode_to_string(binary_to_list(Bin), Options);
mime_decode_to_string(List, Options) when is_list(List) ->
    mime_decode_list_to_string(get_decoding_offset(Options), get_padding(Options), List).

%% Skipping pad character if not at end of string. Also liberal about
%% excess padding and skipping of other illegal (non-base64 alphabet)
%% characters. See section 3.3 of RFC4648
mime_decode_list(ModeOffset, Padding, [C1 | Cs], A) ->
    case b64d(C1, ModeOffset) of
        B1 when is_integer(B1) -> mime_decode_list(ModeOffset, Padding, Cs, A, B1);
        _ -> mime_decode_list(ModeOffset, Padding, Cs, A)  % eq is padding
    end;
mime_decode_list(_ModeOffset, _Padding, [], A) ->
    A.

mime_decode_list(ModeOffset, Padding, [C2 | Cs], A, B1) ->
    case b64d(C2, ModeOffset) of
        B2 when is_integer(B2) ->
            mime_decode_list(ModeOffset, Padding, Cs, A, B1, B2);
        _ -> mime_decode_list(ModeOffset, Padding, Cs, A, B1) % eq is padding
    end.

mime_decode_list(ModeOffset, Padding, [C3 | Cs], A, B1, B2) ->
    case b64d(C3, ModeOffset) of
        B3 when is_integer(B3) ->
            mime_decode_list(ModeOffset, Padding, Cs, A, B1, B2, B3);
        eq=B3 ->
            mime_decode_list_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3);
        _ -> mime_decode_list(ModeOffset, Padding, Cs, A, B1, B2)
    end;
mime_decode_list(ModeOffset, Padding, [], A, B1, B2) ->
    case Padding of
        true -> missing_padding_error();
        false -> mime_decode_list_after_eq(ModeOffset, Padding, [], A, B1, B2, eq)
    end.

mime_decode_list(ModeOffset, Padding, [C4 | Cs], A, B1, B2, B3) ->
    case b64d(C4, ModeOffset) of
        B4 when is_integer(B4) ->
            mime_decode_list(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>);
        eq ->
            mime_decode_list_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3);
        _ -> mime_decode_list(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end;
mime_decode_list(ModeOffset, Padding, [], A, B1, B2, B3) ->
    case Padding of
        true -> missing_padding_error();
        false -> mime_decode_list_after_eq(ModeOffset, Padding, [], A, B1, B2, B3)
    end.

mime_decode_list_after_eq(ModeOffset, Padding, [C | Cs], A, B1, B2, B3) ->
    case b64d(C, ModeOffset) of
        B when is_integer(B) ->
            %% More valid data, skip the eq as invalid
            case B3 of
                eq -> mime_decode_list(ModeOffset, Padding, Cs, A, B1, B2, B);
                _ -> mime_decode_list(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B:6>>)
            end;
        _ -> mime_decode_list_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end;
mime_decode_list_after_eq(_ModeOffset, _Padding, [], A, B1, B2, eq) ->
    <<A/bits,B1:6,(B2 bsr 4):2>>;
mime_decode_list_after_eq(_ModeOffset, _Padding, [], A, B1, B2, B3) ->
    <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>.

mime_decode_binary(ModeOffset, Padding, <<C1:8, Cs/bits>>, A) ->
    case b64d(C1, ModeOffset) of
        B1 when is_integer(B1) -> mime_decode_binary(ModeOffset, Padding, Cs, A, B1);
        _ -> mime_decode_binary(ModeOffset, Padding, Cs, A)  % eq is padding
    end;
mime_decode_binary(_ModeOffset, _Padding, <<>>, A) ->
    A.

mime_decode_binary(ModeOffset, Padding, <<C2:8, Cs/bits>>, A, B1) ->
    case b64d(C2, ModeOffset) of
        B2 when is_integer(B2) ->
            mime_decode_binary(ModeOffset, Padding, Cs, A, B1, B2);
        _ -> mime_decode_binary(ModeOffset, Padding, Cs, A, B1) % eq is padding
    end.

mime_decode_binary(ModeOffset, Padding, <<C3:8, Cs/bits>>, A, B1, B2) ->
    case b64d(C3, ModeOffset) of
        B3 when is_integer(B3) ->
            mime_decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B3);
        eq=B3 ->
            mime_decode_binary_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3);
        _ -> mime_decode_binary(ModeOffset, Padding, Cs, A, B1, B2)
    end;
mime_decode_binary(ModeOffset, Padding, <<Cs/bits>>, A, B1, B2) ->
    case Padding of
        true -> missing_padding_error();
        false -> mime_decode_binary_after_eq(ModeOffset, Padding, Cs, A, B1, B2, eq)
    end.

mime_decode_binary(ModeOffset, Padding, <<C4:8, Cs/bits>>, A, B1, B2, B3) ->
    case b64d(C4, ModeOffset) of
        B4 when is_integer(B4) ->
            mime_decode_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>);
        eq ->
            mime_decode_binary_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3);
        _ -> mime_decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end;
mime_decode_binary(ModeOffset, Padding, <<Cs/bits>>, A, B1, B2, B3) ->
    case Padding of
        true -> missing_padding_error();
        false -> mime_decode_binary_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end.

mime_decode_binary_after_eq(ModeOffset, Padding, <<C:8, Cs/bits>>, A, B1, B2, B3) ->
    case b64d(C, ModeOffset) of
        B when is_integer(B) ->
            %% More valid data, skip the eq as invalid
            case B3 of
                eq -> mime_decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B);
                _ -> mime_decode_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B:6>>)
            end;
        _ -> mime_decode_binary_after_eq(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end;
mime_decode_binary_after_eq(_ModeOffset, _Padding, <<>>, A, B1, B2, eq) ->
    <<A/bits,B1:6,(B2 bsr 4):2>>;
mime_decode_binary_after_eq(_ModeOffset, _Padding, <<>>, A, B1, B2, B3) ->
    <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>.

mime_decode_list_to_string(ModeOffset, Padding, [C1 | Cs]) ->
    case b64d(C1, ModeOffset) of
        B1 when is_integer(B1) -> mime_decode_list_to_string(ModeOffset, Padding, Cs, B1);
        _ -> mime_decode_list_to_string(ModeOffset, Padding, Cs) % eq is padding
    end;
mime_decode_list_to_string(_ModeOffset, _Padding, []) ->
    [].

mime_decode_list_to_string(ModeOffset, Padding, [C2 | Cs], B1) ->
    case b64d(C2, ModeOffset) of
        B2 when is_integer(B2) ->
            mime_decode_list_to_string(ModeOffset, Padding, Cs, B1, B2);
        _ -> mime_decode_list_to_string(ModeOffset, Padding, Cs, B1) % eq is padding
    end.

mime_decode_list_to_string(ModeOffset, Padding, [C3 | Cs], B1, B2) ->
    case b64d(C3, ModeOffset) of
        B3 when is_integer(B3) ->
            mime_decode_list_to_string(ModeOffset, Padding, Cs, B1, B2, B3);
        eq=B3 -> mime_decode_list_to_string_after_eq(ModeOffset, Padding, Cs, B1, B2, B3);
        _ -> mime_decode_list_to_string(ModeOffset, Padding, Cs, B1, B2)
    end;
mime_decode_list_to_string(ModeOffset, Padding, [], B1, B2) ->
    case Padding of
        true -> missing_padding_error();
        false -> mime_decode_list_to_string_after_eq(ModeOffset, Padding, [], B1, B2, eq)
    end.

mime_decode_list_to_string(ModeOffset, Padding, [C4 | Cs], B1, B2, B3) ->
    case b64d(C4, ModeOffset) of
        B4 when is_integer(B4) ->
            Bits4x6 = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
            Octet1 = Bits4x6 bsr 16,
            Octet2 = (Bits4x6 bsr 8) band 16#ff,
            Octet3 = Bits4x6 band 16#ff,
            [Octet1, Octet2, Octet3 | mime_decode_list_to_string(ModeOffset, Padding, Cs)];
        eq ->
            mime_decode_list_to_string_after_eq(ModeOffset, Padding, Cs, B1, B2, B3);
        _ -> mime_decode_list_to_string(ModeOffset, Padding, Cs, B1, B2, B3)
    end;
mime_decode_list_to_string(ModeOffset, Padding, [], B1, B2, B3) ->
    case Padding of
        true -> missing_padding_error();
        false -> mime_decode_list_to_string_after_eq(ModeOffset, Padding, [], B1, B2, B3)
    end.

mime_decode_list_to_string_after_eq(ModeOffset, Padding, [C | Cs], B1, B2, B3) ->
    case b64d(C, ModeOffset) of
        B when is_integer(B) ->
            %% More valid data, skip the eq as invalid
            case B3 of
                eq -> mime_decode_list_to_string(ModeOffset, Padding, Cs, B1, B2, B);
                _ ->
                    Bits4x6 = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B,
                    Octet1 = Bits4x6 bsr 16,
                    Octet2 = (Bits4x6 bsr 8) band 16#ff,
                    Octet3 = Bits4x6 band 16#ff,
                    [Octet1, Octet2, Octet3 | mime_decode_list_to_string(ModeOffset, Padding, Cs)]
            end;
        _ -> mime_decode_list_to_string_after_eq(ModeOffset, Padding, Cs, B1, B2, B3)
    end;
mime_decode_list_to_string_after_eq(_ModeOffset, _Padding, [], B1, B2, eq) ->
    binary_to_list(<<B1:6,(B2 bsr 4):2>>);
mime_decode_list_to_string_after_eq(_ModeOffset, _Padding, [], B1, B2, B3) ->
    binary_to_list(<<B1:6,B2:6,(B3 bsr 2):4>>).

decode_list(ModeOffset, Padding, [C1 | Cs], A) ->
    case b64d(C1, ModeOffset) of
        ws -> decode_list(ModeOffset, Padding, Cs, A);
        B1 -> decode_list(ModeOffset, Padding, Cs, A, B1)
    end;
decode_list(_ModeOffset, _Padding, [], A) ->
    A.

decode_list(ModeOffset, Padding, [C2 | Cs], A, B1) ->
    case b64d(C2, ModeOffset) of
        ws -> decode_list(ModeOffset, Padding, Cs, A, B1);
        B2 -> decode_list(ModeOffset, Padding, Cs, A, B1, B2)
    end.

decode_list(ModeOffset, Padding, [C3 | Cs], A, B1, B2) ->
    case b64d(C3, ModeOffset) of
        ws -> decode_list(ModeOffset, Padding, Cs, A, B1, B2);
        B3 -> decode_list(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end;
decode_list(ModeOffset, Padding, [], A, B1, B2) ->
    case Padding of
        true -> missing_padding_error();
        false -> decode_list(ModeOffset, Padding, [], A, B1, B2, eq)
    end.

decode_list(ModeOffset, Padding, [C4 | Cs], A, B1, B2, B3) ->
    case b64d(C4, ModeOffset) of
        ws                -> decode_list(ModeOffset, Padding, Cs, A, B1, B2, B3);
        eq when B3 =:= eq -> only_ws(ModeOffset, Padding, Cs, <<A/bits,B1:6,(B2 bsr 4):2>>);
        eq                -> only_ws(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>);
        B4                -> decode_list(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>)
    end;
decode_list(_ModeOffset, Padding, [], A, B1, B2, B3) ->
    case Padding of
        true -> missing_padding_error();
        false when B3 == eq -> <<A/bits,B1:6,(B2 bsr 4):2>>;
        false -> <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>
    end.

decode_binary(ModeOffset, Padding, <<C1:8, C2:8, C3:8, C4:8, Cs/bits>>, A) ->
    case {b64d(C1, ModeOffset), b64d(C2, ModeOffset), b64d(C3, ModeOffset), b64d(C4, ModeOffset)} of
        {B1, B2, B3, B4} when is_integer(B1), is_integer(B2),
                              is_integer(B3), is_integer(B4) ->
            decode_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>);
        {B1, B2, B3, B4} ->
            dec_bin(ModeOffset, Padding, Cs, B1, B2, B3, B4, A)
    end;
decode_binary(_ModeOffset, _Padding, <<>>, A) ->
    A;
decode_binary(ModeOffset, Padding, <<C1:8, Cs/bits>>, A) ->
    case b64d(C1, ModeOffset) of
        ws -> decode_binary(ModeOffset, Padding, Cs, A);
        B1 -> decode_binary(ModeOffset, Padding, Cs, A, B1)
    end.

dec_bin(ModeOffset, Padding, Cs, ws, B2, B3, B4, A) ->
    dec_bin(ModeOffset, Padding, Cs, B2, B3, B4, A);
dec_bin(ModeOffset, Padding, Cs, B1, ws, B3, B4, A) ->
    dec_bin(ModeOffset, Padding, Cs, B1, B3, B4, A);
dec_bin(ModeOffset, Padding, Cs, B1, B2, ws, B4, A) ->
    dec_bin(ModeOffset, Padding, Cs, B1, B2, B4, A);
dec_bin(ModeOffset, Padding, Cs, B1, B2, B3, B4, A) ->
    case B4 of
        ws                -> decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B3);
        eq when B3 =:= eq -> only_ws_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,(B2 bsr 4):2>>);
        eq                -> only_ws_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>);
        B4                -> decode_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>)
    end.

dec_bin(ModeOffset, Padding, Cs, ws, B2, B3, A) ->
    dec_bin(ModeOffset, Padding, Cs, B2, B3, A);
dec_bin(ModeOffset, Padding, Cs, B1, ws, B3, A) ->
    dec_bin(ModeOffset, Padding, Cs, B1, B3, A);
dec_bin(ModeOffset, Padding, Cs, B1, B2, ws, A) ->
    dec_bin(ModeOffset, Padding, Cs, B1, B2, A);
dec_bin(ModeOffset, Padding, Cs, B1, B2, B3, A) ->
    decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B3).

dec_bin(ModeOffset, Padding, Cs, ws, B2, A) ->
    dec_bin(ModeOffset, Padding, Cs, B2, A);
dec_bin(ModeOffset, Padding, Cs, B1, ws, A) ->
    dec_bin(ModeOffset, Padding, Cs, B1, A);
dec_bin(ModeOffset, Padding, Cs, B1, B2, A) ->
    decode_binary(ModeOffset, Padding, Cs, A, B1, B2).

dec_bin(ModeOffset, Padding, Cs, ws, A) ->
    decode_binary(ModeOffset, Padding, Cs, A);
dec_bin(ModeOffset, Padding, Cs, B1, A) ->
    decode_binary(ModeOffset, Padding,Cs, A, B1).

decode_binary(ModeOffset, Padding, <<C2:8, Cs/bits>>, A, B1) ->
    case b64d(C2, ModeOffset) of
        ws -> decode_binary(ModeOffset, Padding, Cs, A, B1);
        B2 -> decode_binary(ModeOffset, Padding, Cs, A, B1, B2)
    end.

decode_binary(ModeOffset, Padding, <<C3:8, Cs/bits>>, A, B1, B2) ->
    case b64d(C3, ModeOffset) of
        ws -> decode_binary(ModeOffset, Padding, Cs, A, B1, B2);
        B3 -> decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B3)
    end;
decode_binary(ModeOffset, Padding, <<Cs/bits>>, A, B1, B2) ->
    case Padding of
        true -> missing_padding_error();
        false -> decode_binary(ModeOffset, Padding, Cs, A, B1, B2, eq)
    end.

decode_binary(ModeOffset, Padding, <<C4:8, Cs/bits>>, A, B1, B2, B3) ->
    case b64d(C4, ModeOffset) of
        ws                -> decode_binary(ModeOffset, Padding, Cs, A, B1, B2, B3);
        eq when B3 =:= eq -> only_ws_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,(B2 bsr 4):2>>);
        eq                -> only_ws_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>);
        B4                -> decode_binary(ModeOffset, Padding, Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>)
    end;
decode_binary(_ModeOffset, Padding, <<>>, A, B1, B2, B3) ->
    case Padding of
        true -> missing_padding_error();
        false when B3 =:= eq -> <<A/bits,B1:6,(B2 bsr 4):2>>;
        false -> <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>
    end.

only_ws_binary(_ModeOffset, _Padding, <<>>, A) ->
    A;
only_ws_binary(ModeOffset, Padding, <<C:8, Cs/bits>>, A) ->
    case b64d(C, ModeOffset) of
        ws -> only_ws_binary(ModeOffset, Padding, Cs, A)
    end.

decode_list_to_string(ModeOffset, Padding, [C1 | Cs]) ->
    case b64d(C1, ModeOffset) of
        ws -> decode_list_to_string(ModeOffset, Padding, Cs);
        B1 -> decode_list_to_string(ModeOffset, Padding, Cs, B1)
    end;
decode_list_to_string(_ModeOffset, _Padding, []) ->
    [].

decode_list_to_string(ModeOffset, Padding, [C2 | Cs], B1) ->
    case b64d(C2, ModeOffset) of
        ws -> decode_list_to_string(ModeOffset, Padding, Cs, B1);
        B2 -> decode_list_to_string(ModeOffset, Padding, Cs, B1, B2)
    end.

decode_list_to_string(ModeOffset, Padding, [C3 | Cs], B1, B2) ->
    case b64d(C3, ModeOffset) of
        ws -> decode_list_to_string(ModeOffset, Padding, Cs, B1, B2);
        B3 -> decode_list_to_string(ModeOffset, Padding, Cs, B1, B2, B3)
    end;
decode_list_to_string(ModeOffset, Padding, [], B1, B2) ->
    case Padding of
        true -> missing_padding_error();
        false -> decode_list_to_string(ModeOffset, Padding, [], B1, B2, eq)
    end.

decode_list_to_string(ModeOffset, Padding, [C4 | Cs], B1, B2, B3) ->
    case b64d(C4, ModeOffset) of
        ws ->
            decode_list_to_string(ModeOffset, Padding, Cs, B1, B2, B3);
        eq when B3 =:= eq ->
            only_ws(ModeOffset, Padding, Cs, binary_to_list(<<B1:6,(B2 bsr 4):2>>));
        eq ->
            only_ws(ModeOffset, Padding, Cs, binary_to_list(<<B1:6,B2:6,(B3 bsr 2):4>>));
        B4 ->
            Bits4x6 = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
            Octet1 = Bits4x6 bsr 16,
            Octet2 = (Bits4x6 bsr 8) band 16#ff,
            Octet3 = Bits4x6 band 16#ff,
            [Octet1, Octet2, Octet3 | decode_list_to_string(ModeOffset, Padding, Cs)]
    end;
decode_list_to_string(_ModeOffset, Padding, [], B1, B2, B3) ->
    case Padding of
        true -> missing_padding_error();
        false when B3 =:= eq -> binary_to_list(<<B1:6,(B2 bsr 4):2>>);
        false -> binary_to_list(<<B1:6,B2:6,(B3 bsr 2):4>>)
    end.

only_ws(_ModeOffset, _Padding, [], A) ->
    A;
only_ws(ModeOffset, Padding, [C | Cs], A) ->
    case b64d(C, ModeOffset) of
        ws -> only_ws(ModeOffset, Padding, Cs, A)
    end.

%%%========================================================================
%%% Error handling functions
%%%========================================================================

% always inlined for useful stacktraces when called in tail position
-compile({inline, missing_padding_error/0}).
missing_padding_error() ->
    error(missing_padding, none, [{error_info, #{}}]).

-doc false.
format_error(missing_padding, _) ->
    #{general => "data to decode is missing final = padding characters, if this is intended, use the `padding => false` option"};
format_error(_, _) ->
    #{}.

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% accessors

get_padding(#{padding := Bool}) when is_boolean(Bool) -> Bool;
get_padding(#{}) -> true.

get_decoding_offset(#{mode := standard}) -> 1;
get_decoding_offset(#{mode := urlsafe}) -> 257;
get_decoding_offset(#{}) -> 1.

-compile({inline, [{b64d, 2}]}).
b64d(X, Off) ->
    element(X + Off,
            {
	     %% standard base64 alphabet (RFC 4648 Section 4)
	     bad,bad,bad,bad,bad,bad,bad,bad,bad,ws,ws,bad,bad,ws,bad,bad, %0-15
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %16-31
             ws,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,62,bad,bad,bad,63, %32-47
             52,53,54,55,56,57,58,59,60,61,bad,bad,bad,eq,bad,bad, %48-61
             bad,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,
             15,16,17,18,19,20,21,22,23,24,25,bad,bad,bad,bad,bad,
             bad,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
             41,42,43,44,45,46,47,48,49,50,51,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,

	     %% alternative base64url alphabet (RFC 4648 Section 5)
             bad,bad,bad,bad,bad,bad,bad,bad,bad,ws,ws,bad,bad,ws,bad,bad, %0-15
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %16-31
             ws,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,62,bad,bad, %32-47
             52,53,54,55,56,57,58,59,60,61,bad,bad,bad,eq,bad,bad, %48-61
             bad,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,
             15,16,17,18,19,20,21,22,23,24,25,bad,bad,bad,bad,63,
             bad,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
             41,42,43,44,45,46,47,48,49,50,51,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad}).

get_encoding_offset(#{mode := standard}) -> 1;
get_encoding_offset(#{mode := urlsafe}) -> 65;
get_encoding_offset(#{}) -> 1.

-compile({inline, [{b64e, 2}]}).
b64e(X, Off) ->
    element(X + Off,
	    {
	     %% standard base64 alphabet (RFC 4648 Section 4)
	     $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
	     $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
	     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
	     $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
	     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $+, $/,

	     %% alternative base64url alphabet (RFC 4648 Section 5)
	     $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
	     $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
	     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
	     $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
	     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $-, $_}).
