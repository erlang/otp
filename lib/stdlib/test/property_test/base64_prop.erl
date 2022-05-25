%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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
-module(base64_prop).

-compile([export_all, nowarn_export_all]).

-proptest(eqc).
-proptest([triq, proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC, true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_encode() ->
    ?FORALL(
        Str,
        oneof([list(byte()), binary()]),
        begin
            Enc = base64:encode(Str),
            Dec = base64:decode(Enc),
            is_b64_binary(Enc) andalso str_equals(Str, Dec)
        end
    ).

prop_encode_to_string() ->
    ?FORALL(
        Str,
        oneof([list(byte()), binary()]),
        begin
            Enc = base64:encode_to_string(Str),
            Dec = base64:decode_to_string(Enc),
            is_b64_string(Enc) andalso str_equals(Str, Dec)
        end
    ).

prop_decode() ->
    ?FORALL(
        {NormalizedB64, WspedB64},
        wsped_b64(),
        begin
            Dec = base64:decode(WspedB64),
            Enc = base64:encode(Dec),
            is_binary(Dec) andalso b64_equals(NormalizedB64, Enc)
        end
    ).

prop_decode_malformed() ->
    common_decode_malformed(wsped_b64(), fun base64:decode/1).

prop_decode_noisy() ->
    common_decode_noisy(fun base64:decode/1).

prop_decode_to_string() ->
    ?FORALL(
        {NormalizedB64, WspedB64},
        wsped_b64(),
        begin
            Dec = base64:decode_to_string(WspedB64),
            Enc = base64:encode(Dec),
            is_bytelist(Dec) andalso b64_equals(NormalizedB64, Enc)
        end
    ).

prop_decode_to_string_malformed() ->
    common_decode_malformed(wsped_b64(), fun base64:decode_to_string/1).

prop_decode_to_string_noisy() ->
    common_decode_noisy(fun base64:decode_to_string/1).

prop_mime_decode() ->
    ?FORALL(
        {NormalizedB64, NoisyB64},
        noisy_b64(),
        begin
            Dec = base64:mime_decode(NoisyB64),
            Enc = base64:encode(Dec),
            is_binary(Dec) andalso b64_equals(NormalizedB64, Enc)
        end
    ).

prop_mime_decode_malformed() ->
    common_decode_malformed(noisy_b64(), fun base64:mime_decode/1).

prop_mime_decode_to_string() ->
    ?FORALL(
        {NormalizedB64, NoisyB64},
        noisy_b64(),
        begin
            Dec = base64:mime_decode_to_string(NoisyB64),
            Enc = base64:encode(Dec),
            is_bytelist(Dec) andalso b64_equals(NormalizedB64, Enc)
        end
    ).

prop_mime_decode_to_string_malformed() ->
    common_decode_malformed(noisy_b64(), fun base64:mime_decode_to_string/1).

common_decode_noisy(Fn) ->
    ?FORALL(
        {_, NoisyB64},
        ?SUCHTHAT({NormalizedB64, NoisyB64}, noisy_b64(), NormalizedB64 =/= NoisyB64),
        try
            Fn(NoisyB64)
        of
            _ ->
                false
        catch
            error:_ ->
                true
        end
    ).

common_decode_malformed(Gen, Fn) ->
    ?FORALL(
        MalformedB64,
        ?LET(
            {{NormalizedB64, NoisyB64}, Malformings},
            {
                Gen,
                oneof(
                    [
                        [b64_char()],
                        [b64_char(), b64_char()],
                        [b64_char(), b64_char(), b64_char()]
                    ]
                )
            },
            {NormalizedB64, insert_noise(NoisyB64, Malformings)}
        ),
        try
            Fn(MalformedB64)
        of
            _ ->
                false
        catch
            error:_ ->
                true
        end
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Generate a single character from the base64 alphabet.
b64_char() ->
    oneof(b64_chars()).

%% Generate a string of characters from the base64 alphabet,
%% including padding if needed.
b64_string() ->
    ?LET(
        {L, Filler},
        {list(b64_char()), b64_char()},
        case length(L) rem 4 of
            0 -> L;
            1 -> L ++ [Filler, $=, $=];
            2 -> L ++ [$=, $=];
            3 -> L ++ [$=]
        end
    ).

%% Generate a binary of characters from the base64 alphabet,
%% including padding if needed.
b64_binary() ->
    ?LET(
        L,
        b64_string(),
        list_to_binary(L)
    ).

%% Generate a string or binary of characters from the
%% base64 alphabet, including padding if needed.
b64() ->
    oneof([b64_string(), b64_binary()]).

%% Generate a string or binary of characters from the
%% base64 alphabet, including padding if needed, with
%% whitespaces inserted at random indexes.
wsped_b64() ->
    ?LET(
        {B64, Wsps},
        {b64(), list(oneof([$\t, $\r, $\n, $\s]))},
        {B64, insert_noise(B64, Wsps)}
    ).

%% Generate a single character outside of the base64 alphabet.
%% As whitespaces are allowed but ignored in base64, this generator
%% will produce no whitespaces, either.
non_b64_char() ->
    oneof(lists:seq(16#00, 16#FF) -- b64_allowed_chars()).

%% Generate a string or binary of characters from the
%% base64 alphabet, including padding if needed, with
%% whitespaces and non-base64 ("invalid") characters
%% inserted at random indexes.
noisy_b64() ->
    ?LET(
        {{B64, WspedB64}, Noise},
        {wsped_b64(), non_empty(list(non_b64_char()))},
        {B64, insert_noise(WspedB64, Noise)}
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% The characters of the base64 alphabet.
%% "=" is not included, as it is special in that it
%% may only appear at the end of a base64 encoded string
%% for padding.
b64_chars() ->
    lists:seq($0, $9) ++
    lists:seq($a, $z) ++
    lists:seq($A, $Z) ++
    [$+, $/].

%% In addition to the above, the whitespace characters
%% HTAB, CR, LF and SP are allowed to appear in a base64
%% encoded string and should be ignored.
b64_allowed_chars() ->
    [$\t, $\r, $\n, $\s | b64_chars()].

%% Insert the given list of noise characters at random
%% places into the given base64 string.
insert_noise(B64, []) ->
    B64;
insert_noise([], Noise) ->
    Noise;
insert_noise(<<>>, Noise) ->
    list_to_binary(Noise);
insert_noise([B|Bs] = B64, [N|Ns] = Noise) ->
    case rand:uniform(2) of
        1 ->
            [B|insert_noise(Bs, Noise)];
        2 ->
            [N|insert_noise(B64, Ns)]
    end;
insert_noise(<<B, Bs/binary>> = B64, [N|Ns] = Noise) ->
    case rand:uniform(2) of
        1 ->
            <<B, (insert_noise(Bs, Noise))/binary>>;
        2 ->
            <<N, (insert_noise(B64, Ns))/binary>>
    end.

%% Check if the given character is in the base64 alphabet.
%% This does not include the padding character "=".
is_b64_char($+) ->
    true;
is_b64_char($/) ->
    true;
is_b64_char(C) when C >= $0, C =< $9 ->
    true;
is_b64_char(C) when C >= $A, C =< $Z ->
    true;
is_b64_char(C) when C >= $a, C =< $z ->
    true;
is_b64_char(_) ->
    false.

%% Check if the given argument is a base64 binary,
%% ie that it consists of quadruplets of characters
%% from the base64 alphabet, whereas the last quadruplet
%% may be padded with one or two "="s
is_b64_binary(B) ->
    is_b64_binary(B, 0).

is_b64_binary(<<>>, N) ->
    N rem 4 =:= 0;
is_b64_binary(<<$=>>, N) ->
    N rem 4 =:= 3;
is_b64_binary(<<$=, $=>>, N) ->
    N rem 4 =:= 2;
is_b64_binary(<<C, More/binary>>, N) ->
    case is_b64_char(C) of
        true ->
            is_b64_binary(More, N + 1);
        false ->
            false
    end.

%% Check if the given argument is a base64 string
%% (see is_b64_binary/1)
is_b64_string(S) ->
    is_b64_binary(list_to_binary(S)).

%% Check if the argument is a list of bytes.
is_bytelist(L) ->
    lists:all(
        fun (B) ->
            is_integer(B) andalso B >= 16#00 andalso B =< 16#FF
        end,
        L
    ).

%% Check two byte-lists or binaries for equality.
str_equals(Str1, Str2) when is_list(Str1) ->
    str_equals(list_to_binary(Str1), Str2);
str_equals(Str1, Str2) when is_list(Str2) ->
    str_equals(Str1, list_to_binary(Str2));
str_equals(Str1, Str2) when is_binary(Str1), is_binary(Str2) ->
    Str1 =:= Str2.

%% Check two base64-encoded byte-lists or binaries for equality.
%% Assumes that the given arguments are in a normalized form,
%% ie that they consist only of characters from the base64
%% alphabet and possible padding ("=").
b64_equals(L, B) when is_list(L) ->
    b64_equals(list_to_binary(L), B);
b64_equals(B, L) when is_list(L) ->
    b64_equals(B, list_to_binary(L));
b64_equals(B1, B2) when is_binary(B1), is_binary(B2) ->
    b64_equals1(B1, B2).

b64_equals1(<<Eq:4/bytes>>, <<Eq:4/bytes>>) ->
    is_b64_binary(Eq);
b64_equals1(<<Eq:4/bytes, More1/binary>>, <<Eq:4/bytes, More2/binary>>) ->
    case lists:all(fun is_b64_char/1, binary_to_list(Eq)) of
        true ->
            b64_equals1(More1, More2);
        false ->
            false
    end;
b64_equals1(<<Eq, B1, $=, $=>>, <<Eq, B2, $=, $=>>) ->
    %% If the encoded string ends with "==", there exist multiple
    %% possibilities for the character preceding the "==" as only the
    %% 3rd and 4th bits of the encoded byte represented by that
    %% character are significant.
    %%
    %% For example, all of the encoded strings "QQ==", "QR==", ..., "QZ=="
    %% decode to the string "A", since all the bytes represented by Q to Z
    %% are the same in the significant 3rd and 4th bit.
    case is_b64_char(Eq) of
        true ->
            Normalize = fun
                (C) when C >= $A, C =< $P -> $A;
                (C) when C >= $Q, C =< $Z -> $Q;
                (C) when C >= $a, C =< $f -> $Q;
                (C) when C >= $g, C =< $v -> $g;
                (C) when C >= $w, C =< $z -> $w;
                (C) when C >= $0, C =< $9 -> $w;
                ($+) -> $w;
                ($/) -> $w
            end,
            Normalize(B1) =:= Normalize(B2);
        false ->
            false
    end;
b64_equals1(<<Eq:2/bytes, B1, $=>>, <<Eq:2/bytes, B2, $=>>) ->
    %% Similar to the above, but with the encoded string ending with a
    %% single "=" the 3rd to 6th bits of the encoded byte are significant,
    %% such that, for example, all the encoded strings "QUE=" to "QUH="
    %% decode to the same string "AA".
    <<Eq1, Eq2>> = Eq,
    case is_b64_char(Eq1) andalso is_b64_char(Eq2) of
        true ->
            Normalize = fun
                (C) when C >= $A, C =< $D -> $A;
                (C) when C >= $E, C =< $H -> $E;
                (C) when C >= $I, C =< $L -> $I;
                (C) when C >= $M, C =< $P -> $M;
                (C) when C >= $Q, C =< $T -> $Q;
                (C) when C >= $U, C =< $X -> $U;
                (C) when C >= $Y, C =< $Z -> $Y;
                (C) when C >= $a, C =< $b -> $Y;
                (C) when C >= $c, C =< $f -> $c;
                (C) when C >= $g, C =< $j -> $g;
                (C) when C >= $k, C =< $n -> $k;
                (C) when C >= $o, C =< $r -> $o;
                (C) when C >= $s, C =< $v -> $s;
                (C) when C >= $w, C =< $z -> $w;
                (C) when C >= $0, C =< $3 -> $0;
                (C) when C >= $4, C =< $7 -> $4;
                (C) when C >= $8, C =< $9 -> $8;
                ($+) -> $8;
                ($/) -> $8
            end,
            Normalize(B1) =:= Normalize(B2);
        false ->
            false
    end;
b64_equals1(<<>>, <<>>) ->
    true;
b64_equals1(_, _) ->
    false.
