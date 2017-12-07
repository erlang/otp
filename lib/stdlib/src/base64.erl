%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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

-export([encode/1, decode/1, mime_decode/1,
	 encode_to_string/1, decode_to_string/1, mime_decode_to_string/1]).

%% The following type is a subtype of string() for return values
%% of (some) functions of this module.
-type ascii_string() :: [1..255].
-type ascii_binary() :: binary().

-spec encode_to_string(Data) -> Base64String when
      Data :: ascii_string() | ascii_binary(),
      Base64String :: ascii_string().

encode_to_string(Bin) when is_binary(Bin) ->
    encode_to_string(binary_to_list(Bin));
encode_to_string(List) when is_list(List) ->
    encode_list_to_string(List).

-spec encode(Data) -> Base64 when
      Data :: ascii_string() | ascii_binary(),
      Base64 :: ascii_binary().

encode(Bin) when is_binary(Bin) ->
    encode_binary(Bin, <<>>);
encode(List) when is_list(List) ->
    encode_list(List, <<>>).

encode_list_to_string([]) ->
    [];
encode_list_to_string([B1]) ->
    [b64e(B1 bsr 2),
     b64e((B1 band 3) bsl 4), $=, $=];
encode_list_to_string([B1,B2]) ->
    [b64e(B1 bsr 2),
     b64e(((B1 band 3) bsl 4) bor (B2 bsr 4)),
     b64e((B2 band 15) bsl 2), $=];
encode_list_to_string([B1,B2,B3|Ls]) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    [b64e(BB bsr 18),
     b64e((BB bsr 12) band 63), 
     b64e((BB bsr 6) band 63),
     b64e(BB band 63) | encode_list_to_string(Ls)].

encode_binary(<<>>, A) ->
    A;
encode_binary(<<B1:8>>, A) ->
    <<A/bits,(b64e(B1 bsr 2)):8,(b64e((B1 band 3) bsl 4)):8,$=:8,$=:8>>;
encode_binary(<<B1:8, B2:8>>, A) ->
    <<A/bits,(b64e(B1 bsr 2)):8,
      (b64e(((B1 band 3) bsl 4) bor (B2 bsr 4))):8,
      (b64e((B2 band 15) bsl 2)):8, $=:8>>;
encode_binary(<<B1:8, B2:8, B3:8, Ls/bits>>, A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    encode_binary(Ls,
                  <<A/bits,(b64e(BB bsr 18)):8,
                    (b64e((BB bsr 12) band 63)):8,
                    (b64e((BB bsr 6) band 63)):8,
                    (b64e(BB band 63)):8>>).

encode_list([], A) ->
    A;
encode_list([B1], A) ->
    <<A/bits,(b64e(B1 bsr 2)):8,(b64e((B1 band 3) bsl 4)):8,$=:8,$=:8>>;
encode_list([B1,B2], A) ->
    <<A/bits,(b64e(B1 bsr 2)):8,
      (b64e(((B1 band 3) bsl 4) bor (B2 bsr 4))):8,
      (b64e((B2 band 15) bsl 2)):8, $=:8>>;
encode_list([B1,B2,B3|Ls], A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    encode_list(Ls,
                <<A/bits,(b64e(BB bsr 18)):8,
                  (b64e((BB bsr 12) band 63)):8,
                  (b64e((BB bsr 6) band 63)):8,
                  (b64e(BB band 63)):8>>).

%% mime_decode strips away all characters not Base64 before
%% converting, whereas decode crashes if an illegal character is found

-spec decode(Base64) -> Data when
      Base64 :: ascii_string() | ascii_binary(),
      Data :: ascii_binary().

decode(Bin) when is_binary(Bin) ->
    decode_binary(Bin, <<>>);
decode(List) when is_list(List) ->
    decode_list(List, <<>>).

-spec mime_decode(Base64) -> Data when
      Base64 :: ascii_string() | ascii_binary(),
      Data :: ascii_binary().

mime_decode(Bin) when is_binary(Bin) ->
    mime_decode_binary(Bin, <<>>);
mime_decode(List) when is_list(List) ->
    mime_decode_list(List, <<>>).

%% mime_decode_to_string strips away all characters not Base64 before
%% converting, whereas decode_to_string crashes if an illegal
%% character is found

-spec decode_to_string(Base64) -> DataString when
      Base64 :: ascii_string() | ascii_binary(),
      DataString :: ascii_string().

decode_to_string(Bin) when is_binary(Bin) ->
    decode_to_string(binary_to_list(Bin));
decode_to_string(List) when is_list(List) ->
    decode_list_to_string(List).

-spec mime_decode_to_string(Base64) -> DataString when
      Base64 :: ascii_string() | ascii_binary(),
      DataString :: ascii_string().

mime_decode_to_string(Bin) when is_binary(Bin) ->
    mime_decode_to_string(binary_to_list(Bin));
mime_decode_to_string(List) when is_list(List) ->
    mime_decode_list_to_string(List).

%% Skipping pad character if not at end of string. Also liberal about
%% excess padding and skipping of other illegal (non-base64 alphabet)
%% characters. See section 3.3 of RFC4648
mime_decode_list([0 | Cs], A) ->
    mime_decode_list(Cs, A);
mime_decode_list([C1 | Cs], A) ->
    case b64d(C1) of
        B1 when is_integer(B1) -> mime_decode_list(Cs, A, B1);
        _ -> mime_decode_list(Cs, A)  % eq is padding
    end;
mime_decode_list([], A) ->
    A.

mime_decode_list([0 | Cs], A, B1) ->
    mime_decode_list(Cs, A, B1);
mime_decode_list([C2 | Cs], A, B1) ->
    case b64d(C2) of
        B2 when is_integer(B2) ->
            mime_decode_list(Cs, A, B1, B2);
        _ -> mime_decode_list(Cs, A, B1) % eq is padding
    end.

mime_decode_list([0 | Cs], A, B1, B2) ->
    mime_decode_list(Cs, A, B1, B2);
mime_decode_list([C3 | Cs], A, B1, B2) ->
    case b64d(C3) of
        B3 when is_integer(B3) ->
            mime_decode_list(Cs, A, B1, B2, B3);
        eq=B3 ->
            mime_decode_list_after_eq(Cs, A, B1, B2, B3);
        _ -> mime_decode_list(Cs, A, B1, B2)
    end.

mime_decode_list([0 | Cs], A, B1, B2, B3) ->
    mime_decode_list(Cs, A, B1, B2, B3);
mime_decode_list([C4 | Cs], A, B1, B2, B3) ->
    case b64d(C4) of
        B4 when is_integer(B4) ->
            mime_decode_list(Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>);
        eq ->
            mime_decode_list_after_eq(Cs, A, B1, B2, B3);
        _ -> mime_decode_list(Cs, A, B1, B2, B3)
    end.

mime_decode_list_after_eq([0 | Cs], A, B1, B2, B3) ->
    mime_decode_list_after_eq(Cs, A, B1, B2, B3);
mime_decode_list_after_eq([C | Cs], A, B1, B2, B3) ->
    case b64d(C) of
        B when is_integer(B) ->
            %% More valid data, skip the eq as invalid
            case B3 of
                eq -> mime_decode_list(Cs, A, B1, B2, B);
                _ -> mime_decode_list(Cs, <<A/bits,B1:6,B2:6,B3:6,B:6>>)
            end;
        _ -> mime_decode_list_after_eq(Cs, A, B1, B2, B3)
    end;
mime_decode_list_after_eq([], A, B1, B2, eq) ->
    <<A/bits,B1:6,(B2 bsr 4):2>>;
mime_decode_list_after_eq([], A, B1, B2, B3) ->
    <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>.

mime_decode_binary(<<0:8, Cs/bits>>, A) ->
    mime_decode_binary(Cs, A);
mime_decode_binary(<<C1:8, Cs/bits>>, A) ->
    case b64d(C1) of
        B1 when is_integer(B1) -> mime_decode_binary(Cs, A, B1);
        _ -> mime_decode_binary(Cs, A)  % eq is padding
    end;
mime_decode_binary(<<>>, A) ->
    A.

mime_decode_binary(<<0:8, Cs/bits>>, A, B1) ->
    mime_decode_binary(Cs, A, B1);
mime_decode_binary(<<C2:8, Cs/bits>>, A, B1) ->
    case b64d(C2) of
        B2 when is_integer(B2) ->
            mime_decode_binary(Cs, A, B1, B2);
        _ -> mime_decode_binary(Cs, A, B1) % eq is padding
    end.

mime_decode_binary(<<0:8, Cs/bits>>, A, B1, B2) ->
    mime_decode_binary(Cs, A, B1, B2);
mime_decode_binary(<<C3:8, Cs/bits>>, A, B1, B2) ->
    case b64d(C3) of
        B3 when is_integer(B3) ->
            mime_decode_binary(Cs, A, B1, B2, B3);
        eq=B3 ->
            mime_decode_binary_after_eq(Cs, A, B1, B2, B3);
        _ -> mime_decode_binary(Cs, A, B1, B2)
    end.

mime_decode_binary(<<0:8, Cs/bits>>, A, B1, B2, B3) ->
    mime_decode_binary(Cs, A, B1, B2, B3);
mime_decode_binary(<<C4:8, Cs/bits>>, A, B1, B2, B3) ->
    case b64d(C4) of
        B4 when is_integer(B4) ->
            mime_decode_binary(Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>);
        eq ->
            mime_decode_binary_after_eq(Cs, A, B1, B2, B3);
        _ -> mime_decode_binary(Cs, A, B1, B2, B3)
    end.

mime_decode_binary_after_eq(<<0:8, Cs/bits>>, A, B1, B2, B3) ->
    mime_decode_binary_after_eq(Cs, A, B1, B2, B3);
mime_decode_binary_after_eq(<<C:8, Cs/bits>>, A, B1, B2, B3) ->
    case b64d(C) of
        B when is_integer(B) ->
            %% More valid data, skip the eq as invalid
            case B3 of
                eq -> mime_decode_binary(Cs, A, B1, B2, B);
                _ -> mime_decode_binary(Cs, <<A/bits,B1:6,B2:6,B3:6,B:6>>)
            end;
        _ -> mime_decode_binary_after_eq(Cs, A, B1, B2, B3)
    end;
mime_decode_binary_after_eq(<<>>, A, B1, B2, eq) ->
    <<A/bits,B1:6,(B2 bsr 4):2>>;
mime_decode_binary_after_eq(<<>>, A, B1, B2, B3) ->
    <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>.

mime_decode_list_to_string([0 | Cs]) ->
    mime_decode_list_to_string(Cs);
mime_decode_list_to_string([C1 | Cs]) ->
    case b64d(C1) of
        B1 when is_integer(B1) -> mime_decode_list_to_string(Cs, B1);
        _ -> mime_decode_list_to_string(Cs) % eq is padding
    end;
mime_decode_list_to_string([]) ->
    [].

mime_decode_list_to_string([0 | Cs], B1) ->
    mime_decode_list_to_string(Cs, B1);
mime_decode_list_to_string([C2 | Cs], B1) ->
    case b64d(C2) of
        B2 when is_integer(B2) ->
            mime_decode_list_to_string(Cs, B1, B2);
        _ -> mime_decode_list_to_string(Cs, B1) % eq is padding
    end.

mime_decode_list_to_string([0 | Cs], B1, B2) ->
    mime_decode_list_to_string(Cs, B1, B2);
mime_decode_list_to_string([C3 | Cs], B1, B2) ->
    case b64d(C3) of
        B3 when is_integer(B3) ->
            mime_decode_list_to_string(Cs, B1, B2, B3);
        eq=B3 -> mime_decode_list_to_string_after_eq(Cs, B1, B2, B3);
        _ -> mime_decode_list_to_string(Cs, B1, B2)
    end.

mime_decode_list_to_string([0 | Cs], B1, B2, B3) ->
    mime_decode_list_to_string(Cs, B1, B2, B3);
mime_decode_list_to_string([C4 | Cs], B1, B2, B3) ->
    case b64d(C4) of
        B4 when is_integer(B4) ->
            Bits4x6 = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
            Octet1 = Bits4x6 bsr 16,
            Octet2 = (Bits4x6 bsr 8) band 16#ff,
            Octet3 = Bits4x6 band 16#ff,
            [Octet1, Octet2, Octet3 | mime_decode_list_to_string(Cs)];
        eq ->
            mime_decode_list_to_string_after_eq(Cs, B1, B2, B3);
        _ -> mime_decode_list_to_string(Cs, B1, B2, B3)
    end.

mime_decode_list_to_string_after_eq([0 | Cs], B1, B2, B3) ->
    mime_decode_list_to_string_after_eq(Cs, B1, B2, B3);
mime_decode_list_to_string_after_eq([C | Cs], B1, B2, B3) ->
    case b64d(C) of
        B when is_integer(B) ->
            %% More valid data, skip the eq as invalid
            case B3 of
                eq -> mime_decode_list_to_string(Cs, B1, B2, B);
                _ ->
                    Bits4x6 = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B,
                    Octet1 = Bits4x6 bsr 16,
                    Octet2 = (Bits4x6 bsr 8) band 16#ff,
                    Octet3 = Bits4x6 band 16#ff,
                    [Octet1, Octet2, Octet3 | mime_decode_list_to_string(Cs)]
            end;
        _ -> mime_decode_list_to_string_after_eq(Cs, B1, B2, B3)
    end;
mime_decode_list_to_string_after_eq([], B1, B2, eq) ->
    binary_to_list(<<B1:6,(B2 bsr 4):2>>);
mime_decode_list_to_string_after_eq([], B1, B2, B3) ->
    binary_to_list(<<B1:6,B2:6,(B3 bsr 2):4>>).

decode_list([C1 | Cs], A) ->
    case b64d(C1) of
        ws -> decode_list(Cs, A);
        B1 -> decode_list(Cs, A, B1)
    end;
decode_list([], A) ->
    A.

decode_list([C2 | Cs], A, B1) ->
    case b64d(C2) of
        ws -> decode_list(Cs, A, B1);
        B2 -> decode_list(Cs, A, B1, B2)
    end.

decode_list([C3 | Cs], A, B1, B2) ->
    case b64d(C3) of
        ws -> decode_list(Cs, A, B1, B2);
        B3 -> decode_list(Cs, A, B1, B2, B3)
    end.

decode_list([C4 | Cs], A, B1, B2, B3) ->
    case b64d(C4) of
        ws                -> decode_list(Cs, A, B1, B2, B3);
        eq when B3 =:= eq -> only_ws(Cs, <<A/bits,B1:6,(B2 bsr 4):2>>);
        eq                -> only_ws(Cs, <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>);
        B4                -> decode_list(Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>)
    end.

decode_binary(<<C1:8, Cs/bits>>, A) ->
    case b64d(C1) of
        ws -> decode_binary(Cs, A);
        B1 -> decode_binary(Cs, A, B1)
    end;
decode_binary(<<>>, A) ->
    A.

decode_binary(<<C2:8, Cs/bits>>, A, B1) ->
    case b64d(C2) of
        ws -> decode_binary(Cs, A, B1);
        B2 -> decode_binary(Cs, A, B1, B2)
    end.

decode_binary(<<C3:8, Cs/bits>>, A, B1, B2) ->
    case b64d(C3) of
        ws -> decode_binary(Cs, A, B1, B2);
        B3 -> decode_binary(Cs, A, B1, B2, B3)
    end.

decode_binary(<<C4:8, Cs/bits>>, A, B1, B2, B3) ->
    case b64d(C4) of
        ws                -> decode_binary(Cs, A, B1, B2, B3);
        eq when B3 =:= eq -> only_ws_binary(Cs, <<A/bits,B1:6,(B2 bsr 4):2>>);
        eq                -> only_ws_binary(Cs, <<A/bits,B1:6,B2:6,(B3 bsr 2):4>>);
        B4                -> decode_binary(Cs, <<A/bits,B1:6,B2:6,B3:6,B4:6>>)
    end.

only_ws_binary(<<>>, A) ->
    A;
only_ws_binary(<<C:8, Cs/bits>>, A) ->
    case b64d(C) of
        ws -> only_ws_binary(Cs, A)
    end.

decode_list_to_string([C1 | Cs]) ->
    case b64d(C1) of
        ws -> decode_list_to_string(Cs);
        B1 -> decode_list_to_string(Cs, B1)
    end;
decode_list_to_string([]) ->
    [].

decode_list_to_string([C2 | Cs], B1) ->
    case b64d(C2) of
        ws -> decode_list_to_string(Cs, B1);
        B2 -> decode_list_to_string(Cs, B1, B2)
    end.

decode_list_to_string([C3 | Cs], B1, B2) ->
    case b64d(C3) of
        ws -> decode_list_to_string(Cs, B1, B2);
        B3 -> decode_list_to_string(Cs, B1, B2, B3)
    end.

decode_list_to_string([C4 | Cs], B1, B2, B3) ->
    case b64d(C4) of
        ws ->
            decode_list_to_string(Cs, B1, B2, B3);
        eq when B3 =:= eq ->
            only_ws(Cs, binary_to_list(<<B1:6,(B2 bsr 4):2>>));
        eq ->
            only_ws(Cs, binary_to_list(<<B1:6,B2:6,(B3 bsr 2):4>>));
        B4 ->
            Bits4x6 = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
            Octet1 = Bits4x6 bsr 16,
            Octet2 = (Bits4x6 bsr 8) band 16#ff,
            Octet3 = Bits4x6 band 16#ff,
            [Octet1, Octet2, Octet3 | decode_list_to_string(Cs)]
    end.

only_ws([], A) ->
    A;
only_ws([C | Cs], A) ->
    case b64d(C) of
        ws -> only_ws(Cs, A)
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% accessors 
-compile({inline, [{b64d, 1}]}).
%% One-based decode map.
b64d(X) ->
    element(X,
            {bad,bad,bad,bad,bad,bad,bad,bad,ws,ws,bad,bad,ws,bad,bad, %1-15
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad, %16-31
             ws,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,62,bad,bad,bad,63, %32-47
             52,53,54,55,56,57,58,59,60,61,bad,bad,bad,eq,bad,bad, %48-63
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
             bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad}).

-compile({inline, [{b64e, 1}]}).
b64e(X) ->
    element(X+1,
	    {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
	     $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
	     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
	     $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
	     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $+, $/}).
