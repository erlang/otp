%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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
-module(small_SUITE).

-export([all/0, suite/0]).
-export([edge_cases/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [edge_cases].

edge_cases(Config) when is_list(Config) ->
    {MinSmall, MaxSmall} = Limits = determine_small_limits(0),
    ct:pal("Limits = ~p", [Limits]),

    true = (MaxSmall + 1) =:= MaxSmall + id(1),
    true = (MinSmall - 1) =:= MinSmall - id(1),
    true = (MaxSmall + 1) > id(MaxSmall),
    true = (MinSmall - 1) < id(MinSmall),
    -1 = MinSmall + id(MaxSmall),
    -1 = MaxSmall + id(MinSmall),

    false = is_small(MinSmall * -1),
    false = is_small(MinSmall - id(1)),
    false = is_small(MinSmall - 1),
    false = is_small(MaxSmall + id(1)),

    Lower = lists:seq(MinSmall, MinSmall + 128),
    Upper = lists:seq(MaxSmall, MaxSmall - 128, -1),
    Pow2 = seq_pow2(MinSmall, MaxSmall),
    NearZero = lists:seq(-128, 128),

    ok = test_combinations([Lower, Upper, Pow2, NearZero], MinSmall, MaxSmall),

    ok.

test_combinations([As | Rest]=TestVectors, MinS, MaxS) ->
    [begin
        _ = [arith_test(A, B, MinS, MaxS) || B <- Bs]
     end || A <- As, Bs <- TestVectors],
    test_combinations(Rest, MinS, MaxS);
test_combinations([], _MinS, _MaxS) ->
    ok.

%% Builds a sequence of all powers of 2 between MinSmall and MaxSmall
seq_pow2(MinSmall, MaxSmall) ->
    sp2_1(MinSmall, MinSmall, MaxSmall).

sp2_1(N, _MinS, MaxS) when N >= MaxS ->
    [];
sp2_1(-1, MinS, MaxS) ->
    [-1 | sp2_1(1, MinS, MaxS)];
sp2_1(N, MinS, MaxS) when N < 0 ->
    [N | sp2_1(N bsr 1, MinS, MaxS)];
sp2_1(N, MinS, MaxS) when N > 0 ->
    [N | sp2_1(N bsl 1, MinS, MaxS)].

arith_test(A, B, MinS, MaxS) ->
    verify_kind(A + B, MinS, MaxS),
    verify_kind(B + A, MinS, MaxS),
    verify_kind(A - B, MinS, MaxS),
    verify_kind(B - A, MinS, MaxS),
    verify_kind(A * B, MinS, MaxS),
    verify_kind(B * A, MinS, MaxS),

    true = A + B =:= apply(erlang, id('+'), [A, B]),
    true = A - B =:= apply(erlang, id('-'), [A, B]),
    true = A * B =:= apply(erlang, id('*'), [A, B]),

    true = A + B =:= B + id(A),
    true = A - B =:= A + id(-B),
    true = B - A =:= B + id(-A),
    true = A * B =:= B * id(A),

    true = B =:= 0 orelse ((A * B) div id(B) =:= A),
    true = A =:= 0 orelse ((B * A) div id(A) =:= B),

    ok.

%% Verifies that N is a small when it should be
verify_kind(N, MinS, MaxS) ->
    true = is_small(N) =:= (N >= MinS andalso N =< MaxS).

is_small(N) when is_integer(N) ->
    0 =:= erts_debug:flat_size(N).

determine_small_limits(N) ->
    case is_small(-1 bsl N) of
        true -> determine_small_limits(N + 1);
        false -> {-1 bsl (N - 1), (1 bsl (N - 1)) - 1}
    end.

id(I) -> I.
