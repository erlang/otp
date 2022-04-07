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
         addition_bounds/1, subtraction_bounds/1,
         multiplication_bounds/1, division_bounds/1, rem_bounds/1,
         band_bounds/1, bor_bounds/1, bxor_bounds/1,
         bsr_bounds/1, bsl_bounds/1,
         lt_bounds/1, le_bounds/1, gt_bounds/1, ge_bounds/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [addition_bounds,
       subtraction_bounds,
       multiplication_bounds,
       division_bounds,
       rem_bounds,
       band_bounds,
       bor_bounds,
       bxor_bounds,
       bsr_bounds,
       bsl_bounds,
       lt_bounds,
       le_bounds,
       gt_bounds,
       ge_bounds]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

addition_bounds(_Config) ->
    test_commutative('+', {-12,12}).

subtraction_bounds(_Config) ->
    test_noncommutative('-', {-12,12}).

multiplication_bounds(_Config) ->
    test_commutative('*', {-12,12}).

division_bounds(_Config) ->
    test_noncommutative('div', {-12,12}).

rem_bounds(_Config) ->
    test_noncommutative('rem', {-12,12}),

    {-7,7} = beam_bounds:'rem'(any, {1,8}),
    {-11,11} = beam_bounds:'rem'(any, {-12,8}),

    ok.

band_bounds(_Config) ->
    test_commutative('band'),

    %% Coverage.
    {0,17} = beam_bounds:'band'(any, {7,17}),
    {0,42} = beam_bounds:'band'({0,42}, any),
    any = beam_bounds:'band'({-1,1}, any),
    any = beam_bounds:'band'(any, {-10,0}),
    any = beam_bounds:'band'({-10,0},{-1,10}),
    any = beam_bounds:'band'({-20,-10},{-1,10}),

    ok.

bor_bounds(_Config) ->
    test_commutative('bor'),

    any = beam_bounds:'bor'({-10,0},{-1,10}),
    any = beam_bounds:'bor'({-20,-10},{-1,10}),

    ok.

bxor_bounds(_Config) ->
    test_commutative('bxor'),

    any = beam_bounds:'bxor'({-10,0},{-1,10}),
    any = beam_bounds:'bxor'({-20,-10},{-1,10}),

    ok.

bsr_bounds(_Config) ->
    test_noncommutative('bsr', {-12,12}, {0,7}).

bsl_bounds(_Config) ->
    test_noncommutative('bsl', {-12,12}, {0,7}).

lt_bounds(_Config) ->
    test_relop('<').

le_bounds(_Config) ->
    test_relop('=<').

gt_bounds(_Config) ->
    test_relop('>').

ge_bounds(_Config) ->
    test_relop('>=').

%%% Utilities

test_commutative(Op) ->
    test_commutative(Op, {0,32}).

test_commutative(Op, {Min,Max}) ->
    Seq = lists:seq(Min, Max),
    _ = [test_commutative_1(Op, {A,B}, {C,D}) ||
            A <- Seq,
            B <- lists:nthtail(A-Min, Seq),
            C <- lists:nthtail(A-Min, Seq),
            D <- lists:nthtail(C-Min, Seq),
            {A,B} =< {C,D}],
    ok.

test_commutative_1(Op, R1, R2) ->
    {HighestMin,LowestMax} = min_max_op(Op, R1, R2),
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
test_noncommutative(Op, Range) ->
    test_noncommutative(Op, Range, Range).

test_noncommutative(Op, {Min1,Max1}, {Min2,Max2}) ->
    Seq1 = lists:seq(Min1, Max1),
    Seq2 = lists:seq(Min2, Max2),
    _ = [test_noncommutative_1(Op, {A,B}, {C,D}) ||
            A <- Seq1,
            B <- lists:nthtail(A-Min1, Seq1),
            C <- Seq2,
            D <- lists:nthtail(C-Min2, Seq2)],
    ok.

test_noncommutative_1(Op, R1, R2) ->
    {HighestMin,LowestMax} = min_max_op(Op, R1, R2),
    case beam_bounds:Op(R1, R2) of
        any ->
            case {Op,R2} of
                {'rem',{0,0}} -> ok
            end;
        {Min,Max} when Min =< HighestMin, LowestMax =< Max ->
            ok;
        {Min,Max} ->
            io:format("~p(~p, ~p) evaluates to ~p; should be ~p\n",
                      [Op,R1,R2,{Min,Max},{HighestMin,LowestMax}]),
            ct:fail(bad_min_or_max)
        end.

min_max_op(Op, {A,B}, {C,D}) ->
    min_max_op_1(Op, A, B, C, D, {infinity,-(1 bsl 24)}).

min_max_op_1(Op, A, B, C, D, MinMax0) when A =< B ->
    MinMax = min_max_op_2(Op, A, C, D, MinMax0),
    min_max_op_1(Op, A + 1, B, C, D, MinMax);
min_max_op_1(_Op, _, _, _, _, MinMax) ->
    MinMax.

min_max_op_2(Op, A, 0, D, MinMax) when Op =:= 'div'; Op =:= 'rem' ->
    min_max_op_2(Op, A, 1, D, MinMax);
min_max_op_2(Op, A, C, D, MinMax) when C =< D ->
    Val = erlang:Op(A, C),
    case MinMax of
        {Min,Max} when Min =< Val, Val =< Max ->
            min_max_op_2(Op, A, C + 1, D, {Min,Max});
        {Min,Max} ->
            min_max_op_2(Op, A, C + 1, D, {min(Min, Val),max(Max, Val)})
    end;
min_max_op_2(_Op, _, _, _, MinMax) ->
    MinMax.

test_relop(Op) ->
    Max = 15,
    Seq = lists:seq(0, Max),
    _ = [test_relop_1(Op, {A,B}, {C,D}) ||
            A <- Seq,
            B <- lists:nthtail(A, Seq),
            C <- Seq,
            D <- lists:nthtail(C, Seq)],
    ok.

test_relop_1(Op, R1, R2) ->
    Bool = rel_op(Op, R1, R2),
    case beam_bounds:relop(Op, R1, R2) of
        Bool ->
            ok;
        Wrong ->
            io:format("~p(~p, ~p) evaluates to ~p; should be ~p\n",
                      [Op,R1,R2,Wrong,Bool]),
            ct:fail(bad_bool_result)
    end.

rel_op(Op, {A,B}, {C,D}) ->
    rel_op_1(Op, A, B, C, D, none).

rel_op_1(Op, A, B, C, D, BoolResult0) when A =< B ->
    BoolResult = rel_op_2(Op, A, C, D, BoolResult0),
    rel_op_1(Op, A + 1, B, C, D, BoolResult);
rel_op_1(_Op, _, _, _, _, BoolResult) ->
    BoolResult.

rel_op_2(Op, A, C, D, BoolResult0) when C =< D ->
    Val = erlang:Op(A, C),
    BoolResult = case BoolResult0 of
                     none -> Val;
                     Val -> BoolResult0;
                     _ -> 'maybe'
                 end,
    rel_op_2(Op, A, C + 1, D, BoolResult);
rel_op_2(_Op, _, _, _, BoolResult) ->
    BoolResult.
