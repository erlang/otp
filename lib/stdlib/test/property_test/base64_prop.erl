%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2023. All Rights Reserved.
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

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_encode_1() ->
    ?FORALL(
        Str,
        oneof([list(byte()), binary()]),
        begin
            Enc = base64:encode(Str),
            Dec = base64:decode(Enc),
            is_b64_binary(standard, Enc) andalso str_equals(Str, Dec)
        end
    ).

prop_encode_2() ->
    ?FORALL(
        {Str, Mode},
        {oneof([list(byte()), binary()]), mode()},
        begin
            Enc = base64:encode(Str, #{mode => Mode}),
            Dec = base64:decode(Enc, #{mode => Mode}),
            is_b64_binary(Mode, Enc) andalso str_equals(Str, Dec)
        end
    ).

prop_encode_to_string_1() ->
    ?FORALL(
        Str,
        oneof([list(byte()), binary()]),
        begin
            Enc = base64:encode_to_string(Str),
            Dec = base64:decode_to_string(Enc),
            is_b64_string(standard, Enc) andalso str_equals(Str, Dec)
        end
    ).

prop_encode_to_string_2() ->
    ?FORALL(
        {Str, Mode},
        {oneof([list(byte()), binary()]), mode()},
        begin
            Enc = base64:encode_to_string(Str, #{mode => Mode}),
            Dec = base64:decode_to_string(Enc, #{mode => Mode}),
            is_b64_string(Mode, Enc) andalso str_equals(Str, Dec)
        end
    ).

prop_decode_1() ->
    ?FORALL(
        {NormalizedB64, WspedB64},
        wsped_b64(standard),
        begin
            Dec = base64:decode(WspedB64),
            Enc = base64:encode(Dec),
            is_binary(Dec) andalso b64_equals(standard, NormalizedB64, Enc)
        end
    ).

prop_decode_2() ->
    ?FORALL(
        {{NormalizedB64, WspedB64}, Mode},
        ?LET(
            Mode,
            mode(),
            {wsped_b64(Mode), Mode}
        ),
        begin
            Dec = base64:decode(WspedB64, #{mode => Mode}),
            Enc = base64:encode(Dec, #{mode => Mode}),
            is_binary(Dec) andalso b64_equals(Mode, NormalizedB64, Enc)
        end
    ).

prop_decode_1_malformed() ->
    common_decode_malformed(fun wsped_b64/1, standard, fun(Data, _) -> base64:decode(Data) end).

prop_decode_2_malformed() ->
    common_decode_malformed(fun wsped_b64/1, mode(), fun base64:decode/2).

prop_decode_1_noisy() ->
    common_decode_noisy(standard, fun(Data, _) -> base64:decode(Data) end).

prop_decode_2_noisy() ->
    common_decode_noisy(mode(), fun base64:decode/2).

prop_decode_to_string_1() ->
    ?FORALL(
        {NormalizedB64, WspedB64},
        wsped_b64(standard),
        begin
            Dec = base64:decode_to_string(WspedB64),
            Enc = base64:encode(Dec),
            is_bytelist(Dec) andalso b64_equals(standard, NormalizedB64, Enc)
        end
    ).

prop_decode_to_string_2() ->
    ?FORALL(
        {{NormalizedB64, WspedB64}, Mode},
        ?LET(
            Mode,
            mode(),
            {wsped_b64(Mode), Mode}
        ),
        begin
            Dec = base64:decode_to_string(WspedB64, #{mode => Mode}),
            Enc = base64:encode(Dec, #{mode => Mode}),
            is_bytelist(Dec) andalso b64_equals(Mode, NormalizedB64, Enc)
        end
    ).

prop_decode_to_string_1_malformed() ->
    common_decode_malformed(fun wsped_b64/1, standard, fun(Data, _) -> base64:decode_to_string(Data) end).

prop_decode_to_string_2_malformed() ->
    common_decode_malformed(fun wsped_b64/1, mode(), fun base64:decode_to_string/2).

prop_decode_to_string_1_noisy() ->
    common_decode_noisy(standard, fun(Data, _) -> base64:decode_to_string(Data) end).

prop_decode_to_string_2_noisy() ->
    common_decode_noisy(mode(), fun base64:decode_to_string/2).

prop_mime_decode_1() ->
    ?FORALL(
        {NormalizedB64, NoisyB64},
        noisy_b64(standard),
        begin
            Dec = base64:mime_decode(NoisyB64),
            Enc = base64:encode(Dec),
            is_binary(Dec) andalso b64_equals(standard, NormalizedB64, Enc)
        end
    ).

prop_mime_decode_2() ->
    ?FORALL(
        {{NormalizedB64, NoisyB64}, Mode},
        ?LET(
            Mode,
            mode(),
            {wsped_b64(Mode), Mode}
        ),
        begin
            Dec = base64:mime_decode(NoisyB64, #{mode => Mode}),
            Enc = base64:encode(Dec, #{mode => Mode}),
            is_binary(Dec) andalso b64_equals(Mode, NormalizedB64, Enc)
        end
    ).

prop_mime_decode_1_malformed() ->
    common_decode_malformed(fun noisy_b64/1, standard, fun(Data, _) -> base64:mime_decode(Data) end).

prop_mime_decode_2_malformed() ->
    common_decode_malformed(fun noisy_b64/1, mode(), fun base64:mime_decode/2).

prop_mime_decode_to_string_1() ->
    ?FORALL(
        {NormalizedB64, NoisyB64},
        noisy_b64(standard),
        begin
            Dec = base64:mime_decode_to_string(NoisyB64),
            Enc = base64:encode(Dec),
            is_bytelist(Dec) andalso b64_equals(standard, NormalizedB64, Enc)
        end
    ).

prop_mime_decode_to_string_2() ->
    ?FORALL(
        {{NormalizedB64, NoisyB64}, Mode},
        ?LET(
            Mode,
            mode(),
            {wsped_b64(Mode), Mode}
        ),
        begin
            Dec = base64:mime_decode_to_string(NoisyB64, #{mode => Mode}),
            Enc = base64:encode(Dec, #{mode => Mode}),
            is_bytelist(Dec) andalso b64_equals(Mode, NormalizedB64, Enc)
        end
    ).

prop_mime_decode_to_string_1_malformed() ->
    common_decode_malformed(fun noisy_b64/1, standard, fun(Data, _) -> base64:mime_decode_to_string(Data) end).

prop_mime_decode_to_string_2_malformed() ->
    common_decode_malformed(fun noisy_b64/1, mode(), fun base64:mime_decode_to_string/2).

common_decode_noisy(ModeGen, Fn) ->
    ?FORALL(
        {{_, NoisyB64}, Mode},
        ?LET(
            Mode,
            ModeGen,
            {?SUCHTHAT({NormalizedB64, NoisyB64}, noisy_b64(Mode), NormalizedB64 =/= NoisyB64), Mode}
        ),
        try
            Fn(NoisyB64, #{mode => Mode})
        of
            _ ->
                false
        catch
            error:_ ->
                true
        end
    ).

common_decode_malformed(DataGen, ModeGen, Fn) ->
    ?FORALL(
        {MalformedB64, Mode},
        ?LET(
            Mode,
            ModeGen,
            ?LET(
                {{NormalizedB64, NoisyB64}, Malformings, InsertFn},
                {
                    DataGen(Mode),
                    oneof(
                        [
                            [b64_char(Mode)],
                            [b64_char(Mode), b64_char(Mode)],
                            [b64_char(Mode), b64_char(Mode), b64_char(Mode)]
                        ]
                    ),
                    function1(boolean())
                },
                {{NormalizedB64, insert_noise(NoisyB64, Malformings, InsertFn)}, Mode}
            )
        ),
        try
            Fn(MalformedB64, #{mode => Mode})
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

%% Generate base64 encoding mode.
mode() ->
    oneof([standard, urlsafe]).

%% Generate a single character from the base64 alphabet.
b64_char(Mode) ->
    oneof(b64_chars(Mode)).

%% Generate a string of characters from the base64 alphabet,
%% including padding if needed.
b64_string(Mode) ->
    ?LET(
        {L, Filler},
        {list(b64_char(Mode)), b64_char(Mode)},
        case length(L) rem 4 of
            0 -> L;
            1 -> L ++ [Filler, $=, $=];
            2 -> L ++ [$=, $=];
            3 -> L ++ [$=]
        end
    ).

%% Generate a binary of characters from the base64 alphabet,
%% including padding if needed.
b64_binary(Mode) ->
    ?LET(
        L,
        b64_string(Mode),
        list_to_binary(L)
    ).

%% Generate a string or binary of characters from the
%% base64 alphabet, including padding if needed.
b64(Mode) ->
    oneof([b64_string(Mode), b64_binary(Mode)]).

%% Generate a string or binary of characters from the
%% base64 alphabet, including padding if needed, with
%% whitespaces inserted at random indexes.
wsped_b64(Mode) ->
    ?LET(
        {B64, Wsps, InsertFn},
        {b64(Mode), list(oneof([$\t, $\r, $\n, $\s])), function1(boolean())},
        {B64, insert_noise(B64, Wsps, InsertFn)}
    ).

%% Generate a single character outside of the base64 alphabet.
%% As whitespaces are allowed but ignored in base64, this generator
%% will produce no whitespaces, either.
non_b64_char(Mode) ->
    oneof(lists:seq(16#00, 16#FF) -- b64_allowed_chars(Mode)).

%% Generate a string or binary of characters from the
%% base64 alphabet, including padding if needed, with
%% whitespaces and non-base64 ("invalid") characters
%% inserted at random indexes.
noisy_b64(Mode) ->
    ?LET(
        {{B64, WspedB64}, Noise, InsertFn},
        {wsped_b64(Mode), non_empty(list(non_b64_char(Mode))), function1(boolean())},
        {B64, insert_noise(WspedB64, Noise, InsertFn)}
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% The characters of the base64 alphabet.
%% "=" is not included, as it is special in that it
%% may only appear at the end of a base64 encoded string
%% for padding.
b64_chars_common() ->
    lists:seq($0, $9) ++
    lists:seq($a, $z) ++
    lists:seq($A, $Z).

b64_chars(standard) ->
    b64_chars_common() ++ [$+, $/];
b64_chars(urlsafe) ->
    b64_chars_common() ++ [$-, $_].

%% In addition to the above, the whitespace characters
%% HTAB, CR, LF and SP are allowed to appear in a base64
%% encoded string and should be ignored.
b64_allowed_chars(Mode) ->
    [$\t, $\r, $\n, $\s | b64_chars(Mode)].

%% Insert the given list of noise characters at random
%% places into the given base64 string.
insert_noise(B64, Noise, InsertFn) ->
    insert_noise(B64, Noise, InsertFn, 0).

insert_noise(B64, [], _, _) ->
    B64;
insert_noise([], Noise, _, _) ->
    Noise;
insert_noise(<<>>, Noise, _, _) ->
    list_to_binary(Noise);
insert_noise([B|Bs] = B64, [N|Ns] = Noise, InsertFn, Idx) ->
    case InsertFn(Idx) of
        true ->
            [B|insert_noise(Bs, Noise, InsertFn, Idx + 1)];
        false ->
            [N|insert_noise(B64, Ns, InsertFn, Idx + 1)]
    end;
insert_noise(<<B, Bs/binary>> = B64, [N|Ns] = Noise, InsertFn, Idx) ->
    case InsertFn(Idx) of
        true ->
            <<B, (insert_noise(Bs, Noise, InsertFn, Idx + 1))/binary>>;
        false ->
            <<N, (insert_noise(B64, Ns, InsertFn, Idx + 1))/binary>>
    end.

%% Check if the given character is in the base64 alphabet.
%% This does not include the padding character "=".
is_b64_char(standard, $+) ->
    true;
is_b64_char(standard, $/) ->
    true;
is_b64_char(urlsafe, $-) ->
    true;
is_b64_char(urlsafe, $_) ->
    true;
is_b64_char(_, C) when C >= $0, C =< $9 ->
    true;
is_b64_char(_, C) when C >= $A, C =< $Z ->
    true;
is_b64_char(_, C) when C >= $a, C =< $z ->
    true;
is_b64_char(_, _) ->
    false.

%% Check if the given argument is a base64 binary,
%% ie that it consists of quadruplets of characters
%% from the base64 alphabet, whereas the last quadruplet
%% may be padded with one or two "="s
is_b64_binary(Mode, B) ->
    is_b64_binary(Mode, B, 0).

is_b64_binary(_, <<>>, N) ->
    N rem 4 =:= 0;
is_b64_binary(_, <<$=>>, N) ->
    N rem 4 =:= 3;
is_b64_binary(_, <<$=, $=>>, N) ->
    N rem 4 =:= 2;
is_b64_binary(Mode, <<C, More/binary>>, N) ->
    case is_b64_char(Mode, C) of
        true ->
            is_b64_binary(Mode, More, N + 1);
        false ->
            false
    end.

%% Check if the given argument is a base64 string
%% (see is_b64_binary/1)
is_b64_string(Mode, S) ->
    is_b64_binary(Mode, list_to_binary(S)).

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
b64_equals(Mode, L, B) when is_list(L) ->
    b64_equals(Mode, list_to_binary(L), B);
b64_equals(Mode, B, L) when is_list(L) ->
    b64_equals(Mode, B, list_to_binary(L));
b64_equals(Mode, B1, B2) when is_binary(B1), is_binary(B2) ->
    b64_equals1(Mode, B1, B2).

b64_equals1(Mode, <<Eq:4/bytes>>, <<Eq:4/bytes>>) ->
    is_b64_binary(Mode, Eq);
b64_equals1(Mode, <<Eq:4/bytes, More1/binary>>, <<Eq:4/bytes, More2/binary>>) ->
    case lists:all(fun(C) -> is_b64_char(Mode, C) end, binary_to_list(Eq)) of
        true ->
            b64_equals1(Mode, More1, More2);
        false ->
            false
    end;
b64_equals1(Mode, <<Eq, B1, $=, $=>>, <<Eq, B2, $=, $=>>) ->
    %% If the encoded string ends with "==", there exist multiple
    %% possibilities for the character preceding the "==" as only the
    %% 3rd and 4th bits of the encoded byte represented by that
    %% character are significant.
    %%
    %% For example, all of the encoded strings "QQ==", "QR==", ..., "QZ=="
    %% decode to the string "A", since all the bytes represented by Q to Z
    %% are the same in the significant 3rd and 4th bit.
    case is_b64_char(Mode, Eq) of
        true ->
            Normalize = fun
                (C) when C >= $A, C =< $P -> $A;
                (C) when C >= $Q, C =< $Z -> $Q;
                (C) when C >= $a, C =< $f -> $Q;
                (C) when C >= $g, C =< $v -> $g;
                (C) when C >= $w, C =< $z -> $w;
                (C) when C >= $0, C =< $9 -> $w;
                ($+) when Mode =:= standard -> $w;
                ($-) when Mode =:= urlsafe -> $w;
                ($/) when Mode =:= standard -> $w;
                ($_) when Mode =:= urlsafe -> $w
            end,
            Normalize(B1) =:= Normalize(B2);
        false ->
            false
    end;
b64_equals1(Mode, <<Eq1, Eq2, B1, $=>>, <<Eq1, Eq2, B2, $=>>) ->
    %% Similar to the above, but with the encoded string ending with a
    %% single "=" the 3rd to 6th bits of the encoded byte are significant,
    %% such that, for example, all the encoded strings "QUE=" to "QUH="
    %% decode to the same string "AA".
    case is_b64_char(Mode, Eq1) andalso is_b64_char(Mode, Eq2) of
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
                ($+) when Mode =:= standard -> $8;
                ($-) when Mode =:= urlsafe -> $8;
                ($/) when Mode =:= standard -> $8;
                ($_) when Mode =:= urlsafe -> $8
            end,
            Normalize(B1) =:= Normalize(B2);
        false ->
            false
    end;
b64_equals1(_, <<>>, <<>>) ->
    true;
b64_equals1(_, _, _) ->
    false.
