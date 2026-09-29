%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2026. All Rights Reserved.
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
-module(inet_parse_prop).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% ipv4_address/1, address/1
prop_ipv4_address() ->
    ?FORALL(
        {Expected, Str},
        gen_ipv4_relaxed_address(),
        all_match({ok, Expected},
                  [F(X) || F <- [fun inet_parse:ipv4_address/1,
                                 fun inet_parse:address/1],
                           X <- [Str,
                                 list_to_binary(Str)]])
    ).

%% ipv4strict_address/1, strict_address/1, ipv4_address/1, address/1
prop_ipv4strict_address() ->
    ?FORALL(
        {Expected, Str},
        gen_ipv4_strict_address(),
        all_match({ok, Expected},
                  [F(X) || F <- [fun inet_parse:ipv4strict_address/1,
                                 fun inet_parse:strict_address/1,
                                 fun inet_parse:ipv4_address/1,
                                 fun inet_parse:address/1],
                           X <- [Str,
                                 list_to_binary(Str)]])
    ).

%% ipv6strict_address/1, strict_address/1, ipv6_address/1, address/1
prop_ipv6strict_address() ->
    ?FORALL(
        {Expected, Str},
        gen_ipv6_strict_address(),
        all_match({ok, Expected},
                  [F(X) || F <- [fun inet_parse:ipv6strict_address/1,
                                 fun inet_parse:strict_address/1,
                                 fun inet_parse:ipv6_address/1,
                                 fun inet_parse:address/1],
                           X <- [Str,
                                 list_to_binary(Str)]])
    ).

%% ipv6_address/1 with IPv4 addresses, parsed as IPv4-mapped IPv6 addresses
prop_ipv6mapped_ipv4_address() ->
    ?FORALL(
        {Expected, Str},
        gen_ipv6mapped_ipv4_address(),
        all_match({ok, Expected},
                  [F(X) || F <- [fun inet_parse:ipv6_address/1],
                           X <- [Str,
                                 list_to_binary(Str)]])
    ).

%% ntoa/1, address/1
prop_ntoa() ->
    ?FORALL(
       {Addr, _},
       oneof([gen_ipv4_relaxed_address(),
              gen_ipv6_strict_address(),
              gen_ipv6mapped_ipv4_address()]),
       {ok, Addr} =:= inet_parse:address(inet_parse:ntoa(Addr))
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Generator for {Addr, Str} with Str in strict dotted-decimal IPv4 notation.
gen_ipv4_strict_address() ->
    ?LET(
        {N1, N2, N3, N4} = Addr,
        {?CT_BYTE(), ?CT_BYTE(), ?CT_BYTE(), ?CT_BYTE()},
        ?LET(
            Str,
            [gen_ipv4_field(N1, dec), $., gen_ipv4_field(N2, dec), $., gen_ipv4_field(N3, dec), $., gen_ipv4_field(N4, dec)],
            {Addr, lists:flatten(Str)}
        )
    ).

%% Generator for {Addr, Str} with Str in relaxed IPv4 notation: 1 to 4
%% fields, each in decimal, octal or hexadecimal.
gen_ipv4_relaxed_address() ->
    ?LET(
        {N1, N2, N3, N4} = Addr,
        {?CT_BYTE(), ?CT_BYTE(), ?CT_BYTE(), ?CT_BYTE()},
        ?LET(
            Str,
            oneof([
                       [gen_ipv4_field((N1 bsl 24) bor (N2 bsl 16) bor (N3 bsl 8) bor N4)],
                       [gen_ipv4_field(N1), $., gen_ipv4_field((N2 bsl 16) bor (N3 bsl 8) bor N4)],
                       [gen_ipv4_field(N1), $., gen_ipv4_field(N2), $., gen_ipv4_field((N3 bsl 8) bor N4)],
                       [gen_ipv4_field(N1), $., gen_ipv4_field(N2), $., gen_ipv4_field(N3), $., gen_ipv4_field(N4)]
                  ]),
           {Addr, lists:flatten(Str)}
        )
    ).

%% Generator for the textual representation of IPv4 field value N, in
%% random or given (hex, dec, oct) notation, with random leading zeros
%% for hex and oct.
gen_ipv4_field(N) ->
    ?LET(
        F,
        oneof([hex, dec, oct]),
        gen_ipv4_field(N, F)
    ).

gen_ipv4_field(N, hex) ->
    ?LET(
       {W, X, B},
       {?CT_RANGE(1, 8), oneof([$x, $X]), oneof([$b, $B])},
       begin
           Str = lists:flatten(io_lib:format("~.16" ++ [B], [N])),
           [$0, X] ++ lists:duplicate(max(0, W - length(Str)), $0) ++ Str
       end
    );
gen_ipv4_field(N, dec) ->
    lists:flatten(io_lib:format("~.10b", [N]));
gen_ipv4_field(N, oct) ->
    ?LET(
        W,
        ?CT_RANGE(0, 11),
        begin
            Str = lists:flatten(io_lib:format("~.8b", [N])),
            [$0] ++ lists:duplicate(max(0, W - length(Str)), $0) ++ Str
        end
    ).

%% Generator for {Addr, Str} with Str in relaxed IPv4 notation and Addr
%% being the corresponding IPv4-mapped IPv6 address.
gen_ipv6mapped_ipv4_address() ->
    ?LET(
        {{N1, N2, N3, N4}, Str},
        gen_ipv4_relaxed_address(),
        {{0, 0, 0, 0, 0, 16#ffff, (N1 bsl 8) bor N2, (N3 bsl 8) bor N4}, Str}
    ).

%% Generator for {Addr, Str} with Str in IPv6 notation, with or without
%% an embedded strict IPv4 address in place of the last two fields, and
%% with an optional zone id. Numeric zone ids are only generated for
%% fe80:: and ff02:: addresses, where they end up in the second field.
gen_ipv6_strict_address() ->
    oneof([
        %% IPv6 address with an optional alphanumeric zone id
        ?LET(
            {{Ns, Str}, ZoneIdStr},
            {gen_ipv6_fields(8), gen_zone_id_alnum()},
            {
                Ns,
                Str ++ ZoneIdStr
            }
        ),
        %% link-local (fe80::, ff02::) IPv6 address with an optional numeric zone id
        ?LET(
            {N1, {{N3, N4, N5, N6, N7, N8}, Str}, {ZoneId, ZoneIdStr}},
            {oneof([16#fe80, 16#ff02]), gen_ipv6_fields(6), gen_zone_id_numeric()},
            ?LET(
                {N1Str, N2Str},
                {gen_ipv6_field(N1), gen_ipv6_field(0)},
                {
                    {N1, ZoneId, N3, N4, N5, N6, N7, N8},
                    case lists:prefix("::", Str) of
                        true -> N1Str ++ Str ++ ZoneIdStr;
                        false -> N1Str ++ ":" ++ N2Str ++ ":" ++ Str ++ ZoneIdStr
                    end
                }
            )
        ),
        %% IPv6 address with IPv4 embedding and an optional alphanumeric zone id
        ?LET(
            {{{N6_1, N6_2, N6_3, N6_4, N6_5, N6_6}, Str6}, {{N4_1, N4_2, N4_3, N4_4}, Str4}, ZoneIdStr},
            {gen_ipv6_fields(6), gen_ipv4_strict_address(), gen_zone_id_alnum()},
            {
                {N6_1, N6_2, N6_3, N6_4, N6_5, N6_6, (N4_1 bsl 8) bor N4_2, (N4_3 bsl 8) bor N4_4},
                case lists:suffix("::", Str6) of
                    true -> Str6 ++ Str4 ++ ZoneIdStr;
                    false -> Str6 ++ ":" ++ Str4 ++ ZoneIdStr
                end
            }
        ),
        %% link-local (fe80::, ff02::) IPv6 address with embedded IPv4 and an optional numeric zone id
        ?LET(
            {N6_1, {{N6_3, N6_4, N6_5, N6_6}, Str6}, {{N4_1, N4_2, N4_3, N4_4}, Str4}, {ZoneId, ZoneIdStr}},
            {oneof([16#fe80, 16#ff02]), gen_ipv6_fields(4), gen_ipv4_strict_address(), gen_zone_id_numeric()},
            ?LET(
                {N6_1Str, N6_2Str},
                {gen_ipv6_field(N6_1), gen_ipv6_field(0)},
                {
                    {N6_1, ZoneId, N6_3, N6_4, N6_5, N6_6, (N4_1 bsl 8) bor N4_2, (N4_3 bsl 8) bor N4_4},
                    case {lists:prefix("::", Str6), lists:suffix("::", Str6)} of
                        {true, true} -> N6_1Str ++ Str6 ++ Str4 ++ ZoneIdStr;
                        {true, false} -> N6_1Str ++ Str6 ++ ":" ++ Str4 ++ ZoneIdStr;
                        {false, true} -> N6_1Str ++ ":" ++ N6_2Str ++ ":" ++ Str6 ++ Str4 ++ ZoneIdStr;
                        {false, false} -> N6_1Str ++ ":" ++ N6_2Str ++ ":" ++ Str6 ++ ":" ++ Str4 ++ ZoneIdStr
                    end
                }
            )
        )
    ]).

%% Generator for {Addr, Str} with Str being Total IPv6 fields, with a
%% random run of zero fields compressed to "::".
gen_ipv6_fields(Total) ->
    ?LET(
        {FieldsL, FieldsR},
        {?CT_RANGE(0, Total), ?CT_RANGE(0, Total)},
        ?LET(
            {NsL, NsM, NsR},
            {vector(FieldsL, ?CT_RANGE(0, 16#FFFF)), lists:duplicate(max(0, Total - FieldsL - FieldsR), 0), vector(min(FieldsR, Total - FieldsL), ?CT_RANGE(0, 16#FFFF))},
            ?LET(
                {StrsL, StrsR},
                {lists:map(fun gen_ipv6_field/1, NsL), lists:map(fun gen_ipv6_field/1, NsR)},
                {
                    list_to_tuple(NsL ++ NsM ++ NsR),
                    lists:flatten(
                        if
                            NsM =:= [] -> lists:join($:, StrsL ++ StrsR);
                            true -> [lists:join($:, StrsL), "::", lists:join($:, StrsR)]
                        end
                    )
                }
            )
        )
    ).

%% Generator for the textual representation of IPv6 field value N, in
%% random case and with random leading zeros.
gen_ipv6_field(N) ->
    ?LET(
        {W, B},
        {?CT_RANGE(1, 4), oneof([$b, $B])},
        begin
            Str = lists:flatten(io_lib:format("~.16" ++ [B], [N])),
            lists:duplicate(max(0, W - length(Str)), $0) ++ Str
        end
    ).

%% Generator for {ZoneId, Str} with Str being either empty (ZoneId 0) or
%% "%" followed by the decimal ZoneId, with random leading zeros.
gen_zone_id_numeric() ->
    oneof([{0, ""},
           ?LET(
               {ZoneId, Pad},
               {?CT_RANGE(0, 16#ffff), ?CT_RANGE(0, 10)},
               {ZoneId, [$% | lists:duplicate(Pad, $0) ++ integer_to_list(ZoneId)]}
           )]).

%% Generator for a zone id string that is either empty or "%" followed by
%% an alphanumeric string containing at least one letter, which makes it
%% non-numeric and thus ignored by the parser.
gen_zone_id_alnum() ->
    ?LET(
        {L, R},
        oneof([{"", ""},
               {non_empty(list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z)))), list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9)))},
               {list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9))), non_empty(list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z))))}]),
        if
            L =:= "", R =:= "" -> "";
            true -> [$%|L ++ R]
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% Check that all values in Vals are equal to Expected.
all_match(Expected, Vals) ->
    lists:all(fun(Val) -> Expected =:= Val end, Vals).
