%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022. All Rights Reserved.
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

-module(beam_bounds_SUITE).
-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         band_bounds/1, bor_bounds/1, bxor_bounds/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [band_bounds,
       bor_bounds,
       bxor_bounds]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

band_bounds(_Config) ->
    test_commutative('band'),

    %% Coverage.
    {0,17} = beam_bounds:'band'(any, {7,17}),
    {0,42} = beam_bounds:'band'({0,42}, any),
    any = beam_bounds:'band'({-1,1}, any),
    any = beam_bounds:'band'(any, {-10,0}),

    ok.

bor_bounds(_Config) ->
    test_commutative('bor').

bxor_bounds(_Config) ->
    test_commutative('bxor').

%%% Utilities

-define(MAX_RANGE, 32).

test_commutative(Op) ->
    Seq = lists:seq(0, ?MAX_RANGE),
    _ = [test_commutative_1(Op, {A,B}, {C,D}) ||
            A <- Seq,
            B <- lists:nthtail(A, Seq),
            C <- lists:nthtail(A, Seq),
            D <- lists:nthtail(C, Seq),
            {A,B} =< {C,D}],

    any = beam_bounds:Op({-10,0},{-1,10}),
    any = beam_bounds:Op({-20,-10},{-1,10}),

    ok.

test_commutative_1(Op, R1, R2) ->
    {HighestMin,LowestMax} = max_op(Op, R1, R2),
    {Min,Max} = beam_bounds:Op(R1, R2),
    {Min,Max} = beam_bounds:Op(R2, R1),
    if
        Min =< HighestMin, LowestMax =< Max ->
            ok;
        true ->
            io:format("~p(~p, ~p) evaluates to ~p; should be ~p\n",
                      [Op,R1,R2,{Min,Max},{HighestMin,LowestMax}]),
            ct:fail(bad_min_or_max)
        end.

max_op(Op, {A,B}, {C,D}) ->
    max_op_1(Op, A, B, C, D, {infinity,0}).

max_op_1(Op, A, B, C, D, MinMax0) when A =< B ->
    MinMax = max_op_2(Op, A, C, D, MinMax0),
    max_op_1(Op, A + 1, B, C, D, MinMax);
max_op_1(_Op, _, _, _, _, MinMax) ->
    MinMax.

max_op_2(Op, A, C, D, MinMax) when C =< D ->
    Val = erlang:Op(A, C),
    case MinMax of
        {Min,Max} when Min =< Val, Val =< Max ->
            max_op_2(Op, A, C + 1, D, {Min,Max});
        {Min,Max} ->
            max_op_2(Op, A, C + 1, D, {min(Min, Val),max(Max, Val)})
    end;
max_op_2(_Op, _, _, _, MinMax) ->
    MinMax.
