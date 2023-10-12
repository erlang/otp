%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2022-2023. All Rights Reserved.
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
         bnot_bounds/1,
         lt_bounds/1, le_bounds/1, gt_bounds/1, ge_bounds/1,
         min_bounds/1, max_bounds/1,
         abs_bounds/1,
         infer_lt_gt_bounds/1,
         redundant_masking/1]).

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
       bnot_bounds,
       bsr_bounds,
       bsl_bounds,
       lt_bounds,
       le_bounds,
       gt_bounds,
       ge_bounds,
       min_bounds,
       max_bounds,
       abs_bounds,
       infer_lt_gt_bounds,
       redundant_masking]}].

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
    test_commutative('+', {-12,12}),

    {'-inf',-15} = beam_bounds:bounds('+', {'-inf',-20}, {2,5}),
    {'-inf',55} = beam_bounds:bounds('+', {'-inf',50}, {'-inf',5}),
    {'-inf',110} = beam_bounds:bounds('+', {1,10}, {'-inf',100}),
    any = beam_bounds:bounds('+', {1,'+inf'}, {'-inf',100}),

    {-8,'+inf'} = beam_bounds:bounds('+', {2,'+inf'}, {-10,20}),
    {6,'+inf'} = beam_bounds:bounds('+', {1,10}, {5,'+inf'}),
    {9,'+inf'} = beam_bounds:bounds('+', {2,'+inf'}, {7,'+inf'}),

    ok.

subtraction_bounds(_Config) ->
    test_noncommutative('-', {-12,12}),

    {'-inf',18} = beam_bounds:bounds('-', {'-inf',20}, {2,9}),
    any = beam_bounds:bounds('-', {'-inf',20}, {'-inf',17}),
    {-99,'+inf'} = beam_bounds:bounds('-', {1,10}, {'-inf',100}),
    {-93,'+inf'} = beam_bounds:bounds('-', {7,'+inf'}, {'-inf',100}),

    {-18,'+inf'} = beam_bounds:bounds('-', {2,'+inf'}, {-10,20}),
    {'-inf',6} = beam_bounds:bounds('-', {1,11}, {5,'+inf'}),
    any = beam_bounds:bounds('-', {2,'+inf'}, {7,'+inf'}),

    ok.

multiplication_bounds(_Config) ->
    test_commutative('*', {-12,12}),

    {'-inf',-40} = beam_bounds:bounds('*', {'-inf',-20}, {2,5}),
    {'-inf',1000} = beam_bounds:bounds('*', {'-inf',100}, {1,10}),
    any = beam_bounds:bounds('*', {'-inf',100}, {-10,10}),

    {-100,'+inf'} = beam_bounds:bounds('*', {-10,'+inf'}, {1,10}),
    {7,'+inf'} = beam_bounds:bounds('*', {7,'+inf'}, {1,10}),
    any = beam_bounds:bounds('*', {-10,'+inf'}, {-5,5}),

    {'-inf',1000} = beam_bounds:bounds('*', {1,10}, {'-inf',100}),
    {-100,'+inf'} = beam_bounds:bounds('*', {1,10}, {-10,'+inf'}),

    ok.

division_bounds(_Config) ->
    test_noncommutative('div', {-12,12}),

    {'-inf',-5} = beam_bounds:bounds('div', {'-inf',-20}, {2,4}),
    {'-inf',50} = beam_bounds:bounds('div', {'-inf',100}, {2,4}),

    {-5,'+inf'} = beam_bounds:bounds('div', {-10,'+inf'}, {2,4}),
    {2,'+inf'} = beam_bounds:bounds('div', {10,'+inf'}, {2,4}),

    any = beam_bounds:bounds('div', {10,'+inf'}, {0,0}),
    {'EXIT', {badarith, _}} = catch division_bounds_1([], ok),

    {-10,10} = beam_bounds:bounds('div', {0,10}, any),
    {-50,50} = beam_bounds:bounds('div', {-50,-15}, {-10,'+inf'}),
    {-20,20} = beam_bounds:bounds('div', {-20,10}, any),
    {-7,7} = beam_bounds:bounds('div', {-5,7}, {'-inf',-1}),
    {-42,42} = beam_bounds:bounds('div', {42,42}, any),
    {-42,42} = beam_bounds:bounds('div', {-42,-42}, any),

    any = beam_bounds:bounds('div', {'-inf',10}, any),
    any = beam_bounds:bounds('div', {0,'+inf'}, any),

    ok.

%% GH-6604: Division by zero could cause type analysis to hang forever as
%% beam_bounds would return a bogus result.
division_bounds_1([], X) ->
    -1 div division_bounds_1(X, ok);
division_bounds_1(_, _) ->
    0.

rem_bounds(_Config) ->
    test_noncommutative('rem', {-12,12}),

    {-7,7} = beam_bounds:bounds('rem', any, {1,8}),
    {-11,11} = beam_bounds:bounds('rem', any, {-12,8}),

    {-7,7} = beam_bounds:bounds('rem', {'-inf',10}, {1,8}),
    {0,7} = beam_bounds:bounds('rem', {10,'+inf'}, {1,8}),
    {0,'+inf'} = beam_bounds:bounds('rem', {17,'+inf'}, any),

    {0,10} = beam_bounds:bounds('rem', {1,10}, {'-inf',10}),
    {0,'+inf'} = beam_bounds:bounds('rem', {20,'+inf'}, {10,'+inf'}),
    {'-inf',10} = beam_bounds:bounds('rem', {'-inf',10}, any),

    {-11,10} = beam_bounds:bounds('rem', {-11,10}, {'-inf',89}),
    {-11,10} = beam_bounds:bounds('rem', {-11,10}, {7,'+inf'}),
    {-11,10} = beam_bounds:bounds('rem', {-11,10}, {'-inf',113}),
    {-11,10} = beam_bounds:bounds('rem', {-11,10}, {55,'+inf'}),
    {-11,10} = beam_bounds:bounds('rem', {-11,10}, any),

    {0,0} = beam_bounds:bounds('rem', {0,0}, any),
    {0,1} = beam_bounds:bounds('rem', {1,1}, any),
    {0,2} = beam_bounds:bounds('rem', {2,2}, any),
    {0,3} = beam_bounds:bounds('rem', {2,3}, any),

    {-1,0} = beam_bounds:bounds('rem', {-1,-1}, any),
    {-7,0} = beam_bounds:bounds('rem', {-7,-7}, any),
    {-6,0} = beam_bounds:bounds('rem', {-6,-4}, any),

    ok.

band_bounds(_Config) ->
    test_commutative('band'),

    %% Coverage.
    {0,17} = beam_bounds:bounds('band', any, {7,17}),
    {0,42} = beam_bounds:bounds('band', {0,42}, any),
    any = beam_bounds:bounds('band', {-1,1}, any),
    any = beam_bounds:bounds('band', any, {-10,0}),
    any = beam_bounds:bounds('band', {-10,0}, {-1,10}),
    any = beam_bounds:bounds('band', {-20,-10}, {-1,10}),

    ok.

bor_bounds(_Config) ->
    test_commutative('bor'),

    {'-inf',15} = beam_bounds:bounds('bor', {-10,7},{3,10}),
    {'-inf',11} = beam_bounds:bounds('bor', {-10,1},{-1,10}),
    {'-inf',-1} = beam_bounds:bounds('bor', {-20,-10}, {-2,10}),

    {'-inf',15} = beam_bounds:bounds('bor', {'-inf',10}, {3,5}),
    {'-inf',-1} = beam_bounds:bounds('bor', {-20,-10}, {-100,-50}),

    any = beam_bounds:bounds('bor', {-20,-10}, {-2,'+inf'}),
    any = beam_bounds:bounds('bor', {-20,'+inf'}, {-7,-3}),

    {16,'+inf'} = beam_bounds:bounds('bor', {0,8}, {16,'+inf'}),
    {16,'+inf'} = beam_bounds:bounds('bor', {3,'+inf'}, {16,'+inf'}),

    ok.

bxor_bounds(_Config) ->
    test_commutative('bxor'),

    any = beam_bounds:bounds('bxor', {-10,0}, {-1,10}),
    any = beam_bounds:bounds('bxor', {-20,-10}, {-1,10}),

    ok.

bnot_bounds(_Config) ->
    Min = -7,
    Max = 7,
    Seq = lists:seq(Min, Max),
    _ = [bnot_bounds_1({A,B}) ||
            A <- Seq,
            B <- lists:nthtail(A-Min, Seq)],

    {-43,'+inf'} = beam_bounds:bounds('bnot', {'-inf',42}),
    {99,'+inf'} = beam_bounds:bounds('bnot', {'-inf',-100}),
    {'-inf',-8} = beam_bounds:bounds('bnot', {7,'+inf'}),
    {'-inf',9} = beam_bounds:bounds('bnot', {-10,'+inf'}),
    {-1114111,'+inf'} = beam_bounds:bounds('bnot', {'-inf', 1114110}),

    -1 = bnot_bounds_2(0),
    -43 = bnot_bounds_2_coverage(id(42)),
    {'EXIT',{badarith,_}} = catch bnot_bounds_2_coverage(id(bad)),

    {'EXIT',{_,_}} = catch bnot_bounds_3(id(true)),
    {'EXIT',{_,_}} = catch bnot_bounds_3(id(false)),
    {'EXIT',{_,_}} = catch bnot_bounds_3(id(0)),

    {'EXIT',{{bad_generator,-3},_}} = catch bnot_bounds_4(),

    ok.

bnot_bounds_1(R) ->
    {HighestMin,LowestMax} = min_max_unary_op('bnot', R),
    {Min,Max} = beam_bounds:bounds('bnot', R),
    if
        Min =< HighestMin, LowestMax =< Max ->
            ok;
        true ->
            io:format("bnot(~p) evaluates to ~p; should be ~p\n",
                      [R,{Min,Max},{HighestMin,LowestMax}]),
            ct:fail(bad_min_or_max)
        end.

%% GH-7145: 'bnot' converged too slowly, effectively hanging the compiler.
bnot_bounds_2(0) -> -1;
bnot_bounds_2(N) -> abs(bnot bnot_bounds_2(N)).

bnot_bounds_2_coverage(N) -> bnot N.

%% GH-7468. Would result in a bad_typed_register failure in beam_validator.
bnot_bounds_3(A) ->
    (bnot round(((A xor false) andalso 1) + 2)) bsr ok.

%% GH-7468. Would result in a bad_arg_type failure in beam_validator.
bnot_bounds_4() ->
    << 0 || A <- [1,2], _ <- bnot round(A + trunc(A))>>.


bsr_bounds(_Config) ->
    test_noncommutative('bsr', {-12,12}, {0,7}),

    {0,10} = beam_bounds:bounds('bsr', {0,10}, {0,'+inf'}),
    {0,2} = beam_bounds:bounds('bsr', {0,10}, {2,'+inf'}),

    {-1,10} = beam_bounds:bounds('bsr', {-1,10}, {0,'+inf'}),
    {-100,900} = beam_bounds:bounds('bsr', {-100,900}, {0,'+inf'}),
    {-50,450} = beam_bounds:bounds('bsr', {-100,900}, {1,'+inf'}),

    {'-inf',16} = beam_bounds:bounds('bsr', {'-inf',32}, {1,10}),
    {-5,'+inf'} = beam_bounds:bounds('bsr', {-10,'+inf'}, {1,10}),

    ok.

bsl_bounds(_Config) ->
    test_noncommutative('bsl', {-12,12}, {-7,7}),

    {2,'+inf'} = beam_bounds:bounds('bsl', {1,10}, {1,10_000}),
    {0,'+inf'} = beam_bounds:bounds('bsl', {1,10}, {-10,10_000}),
    {'-inf',-20} = beam_bounds:bounds('bsl', {-30,-10}, {1,10_000}),
    {'-inf',-2} = beam_bounds:bounds('bsl', {-9,-1}, {1,10_000}),
    any = beam_bounds:bounds('bsl', {-7,10}, {1,10_000}),

    {0,'+inf'} = beam_bounds:bounds('bsl', {0,'+inf'}, {0,'+inf'}),
    {20,'+inf'} = beam_bounds:bounds('bsl', {20,30}, {0,'+inf'}),

    any = beam_bounds:bounds('bsl', {-10,100}, {0,'+inf'}),
    any = beam_bounds:bounds('bsl', {-10,100}, {1,'+inf'}),
    any = beam_bounds:bounds('bsl', {-10,100}, {-1,'+inf'}),

    {0,10} = beam_bounds:bounds('bsl', {1,10}, {'-inf',0}),
    {0,20} = beam_bounds:bounds('bsl', {1,10}, {'-inf',1}),
    {-7,10} = beam_bounds:bounds('bsl', {-7,10}, {'-inf',0}),
    {-28,40} = beam_bounds:bounds('bsl', {-7,10}, {'-inf',2}),

    {'-inf',-1} = beam_bounds:bounds('bsl', {-10,-1}, {500,1024}),
    {0,'+inf'} = beam_bounds:bounds('bsl', {1,10}, {500,1024}),

    {'-inf',-40} = beam_bounds:bounds('bsl', {'-inf',-10}, {2,64}),
    {'-inf',224} = beam_bounds:bounds('bsl', {'-inf',7}, {3,5}),

    any = beam_bounds:bounds('bsl', {'-inf',7}, {3,'+inf'}),

    ok.

lt_bounds(_Config) ->
    test_relop('<').

le_bounds(_Config) ->
    test_relop('=<').

gt_bounds(_Config) ->
    test_relop('>').

ge_bounds(_Config) ->
    test_relop('>=').

min_bounds(_Config) ->
    test_commutative(min, {-12,12}),

    {'-inf',-10} = min_bounds({'-inf',-10}, {1,100}),
    {'-inf',1} = min_bounds({'-inf',1}, {1,100}),
    {'-inf',50} = min_bounds({'-inf',50}, {1,100}),
    {'-inf',100} = min_bounds({'-inf',500}, {1,100}),

    {'-inf',-10} = min_bounds({'-inf',-10}, {1,'+inf'}),
    {'-inf',1} = min_bounds({'-inf',1}, {1,'+inf'}),
    {'-inf',700} = min_bounds({'-inf',700}, {1,'+inf'}),

    {1,99} = min_bounds({1,99}, {100,'+inf'}),
    {1,100} = min_bounds({1,100}, {100,'+inf'}),
    {100,200} = min_bounds({150,200}, {100,'+inf'}),

    {'-inf',10} = min_bounds({1,10}, any),
    any = min_bounds({1,'+inf'}, any),
    {'-inf',777} = min_bounds(any, {'-inf',777}),

    ok.

min_bounds(R1, R2) ->
    Result = beam_bounds:bounds(min, R1, R2),
    Result = beam_bounds:bounds(min, R2, R1).

max_bounds(_Config) ->
    test_commutative(max, {-12,12}),

    {1,100} = max_bounds({'-inf',-10}, {1,100}),
    {1,100} = max_bounds({'-inf',1}, {1,100}),
    {1,100} = max_bounds({'-inf',50}, {1,100}),
    {1,500} = max_bounds({'-inf',500}, {1,100}),

    {1,'+inf'}  = max_bounds({'-inf',-10}, {1,'+inf'}),
    {1,'+inf'} = max_bounds({'-inf',1}, {1,'+inf'}),
    {1,'+inf'} = max_bounds({'-inf',700}, {1,'+inf'}),

    {100,'+inf'} = max_bounds({1,99}, {100,'+inf'}),
    {100,'+inf'} = max_bounds({1,100}, {100,'+inf'}),
    {150,'+inf'} = max_bounds({150,200}, {100,'+inf'}),

    {1,'+inf'} = max_bounds({1,99}, any),
    {10,'+inf'} = max_bounds({10,'+inf'}, any),
    any = max_bounds({'-inf',70}, any),

    ok.

max_bounds(R1, R2) ->
    Result = beam_bounds:bounds(max, R1, R2),
    Result = beam_bounds:bounds(max, R2, R1).

abs_bounds(_Config) ->
    Min = -7,
    Max = 7,
    Seq = lists:seq(Min, Max),
    _ = [abs_bounds_1({A,B}) ||
            A <- Seq,
            B <- lists:nthtail(A-Min, Seq)],
    ok.

abs_bounds_1(R) ->
    {HighestMin,LowestMax} = min_max_unary_op('abs', R),
    {Min,Max} = beam_bounds:bounds(abs, R),
    if
        Min =< HighestMin, LowestMax =< Max ->
            ok;
        true ->
            io:format("~p(~p) evaluates to ~p; should be ~p\n",
                      [bif_abs,R,{Min,Max},{HighestMin,LowestMax}]),
            ct:fail(bad_min_or_max)
        end.

infer_lt_gt_bounds(_Config) ->
    {{'-inf',-1}, {'-inf',0}} = infer_lt_gt({'-inf',0}, {'-inf',0}),
    {{'-inf',1}, {'-inf',2}} = infer_lt_gt({'-inf',1}, {'-inf',2}),
    {{'-inf',-2}, {'-inf',-1}} = infer_lt_gt({'-inf',1}, {'-inf',-1}),
    {{'-inf',2}, {1,3}} = infer_lt_gt({'-inf',2}, {1,3}),

    any = infer_lt_gt({'-inf',2}, {3,10}),
    any = infer_lt_gt({'-inf',2}, {3,'+inf'}),

    {{0,10}, {1,84}}  = infer_lt_gt({0,10}, {'-inf',84}),
    {{0,83}, {1,84}}  = infer_lt_gt({0,'+inf'}, {'-inf',84}),

    {{0,'+inf'}, {42, '+inf'}}  = infer_lt_gt({0,'+inf'}, {42, '+inf'}),
    {{100,'+inf'}, {101, '+inf'}}  = infer_lt_gt({100,'+inf'}, {42, '+inf'}),

    ok.

%%% Utilities

infer_lt_gt(R1, R2) ->
    case beam_bounds:infer_relop_types('>', R2, R1) of
        {Rb,Ra} ->
            {Ra,Rb} = beam_bounds:infer_relop_types('<', R1, R2);
        any ->
            any = beam_bounds:infer_relop_types('<', R1, R2)
    end.

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
    {Min,Max} = beam_bounds:bounds(Op, R1, R2),
    {Min,Max} = beam_bounds:bounds(Op, R2, R1),
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
    case beam_bounds:bounds(Op, R1, R2) of
        any ->
            case {Op,R2} of
                {'div',{0,0}} -> ok;
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

min_max_unary_op(Op, {A,B}) ->
    min_max_unary_op_1(Op, A, B, {infinity,-(1 bsl 24)}).

min_max_unary_op_1(Op, A, B, {Min,Max}) when A =< B ->
    Val = erlang:Op(A),
    if
        Min =< Val, Val =< Max ->
            min_max_unary_op_1(Op, A + 1, B, {Min,Max});
        true ->
            min_max_unary_op_1(Op, A + 1, B, {min(Min, Val),max(Max, Val)})
    end;
min_max_unary_op_1(_Op, _, _, MinMax) ->
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
            test_infer_relop(Bool, Op, R1, R2);
        Wrong ->
            io:format("~p(~p, ~p) evaluates to ~p; should be ~p\n",
                      [Op,R1,R2,Wrong,Bool]),
            ct:fail(bad_bool_result)
    end.

test_infer_relop(true, Op, R1, R2) ->
    any = beam_bounds:infer_relop_types(Op, R1, R2);
test_infer_relop(false, Op, R1, R2) ->
    none = beam_bounds:infer_relop_types(Op, R1, R2);
test_infer_relop('maybe', Op, {A0,B0}=R1, {C0,D0}=R2) ->
    {{A,B},{C,D}} = beam_bounds:infer_relop_types(Op, R1, R2),
    if
        A =< B, C =< D, A0 =< A, B0 >= B, C0 =< C, D0 >= D ->
            ok;
        true ->
            io:format("~p ~p infers as ~p ~p\n",
                      [R1,R2,{A,B},{C,D}]),
            ct:fail(ranges_grew)
    end,
    _ = [begin
             case in_range(X, {A,B}) andalso in_range(Y, {C,D}) of
                 true ->
                     ok;
                 false ->
                     io:format("X = ~p; Y = ~p\n", [X,Y]),
                     io:format("~p ~p infers as ~p ~p\n",
                               [R1,R2,{A,B},{C,D}]),
                     ct:fail(bad_inference)
             end
         end || X <- lists:seq(A0, B0),
                Y <- lists:seq(C0, D0),
                erlang:Op(X, Y)],
    ok.

in_range(Int, {A,B}) ->
    A =< Int andalso Int =< B.

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

redundant_masking(_Config) ->
    Min = -7,
    Max = 15,
    Seq = lists:seq(Min, Max),
    _ = [test_redundant_masking({A,B}, M) ||
            A <- Seq,
            B <- lists:nthtail(A-Min, Seq),
            M <- Seq],

    false = beam_bounds:is_masking_redundant({'-inf',10}, 16#ff),
    false = beam_bounds:is_masking_redundant({0,'+inf'}, 16#ff),
    ok.

test_redundant_masking({A,B}=R, M) ->
    ShouldBe = test_redundant_masking(A, B, M),
    case beam_bounds:is_masking_redundant(R, M) of
        ShouldBe ->
            ok;
        false when M band (M + 1) =/= 0 ->
            %% M + 1 is not a power of two.
            ok;
        false when A =:= B ->
            ok;
        Unexpected ->
            io:format("beam_bounds:is_masking_redundant(~p, ~p) "
                      "evaluates to ~p; should be ~p\n",
                      [R,M,Unexpected,ShouldBe]),
            ct:fail(bad_boolean)
    end.

test_redundant_masking(A, B, M) when A =< B ->
    A band M =:= A andalso test_redundant_masking(A + 1, B, M);
test_redundant_masking(_, _, _) -> true.

%%%
%%% Common utilities.
%%%

id(I) ->
    I.
