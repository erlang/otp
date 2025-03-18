%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

-include_lib("syntax_tools/include/merl.hrl").

-export([all/0, suite/0, groups/0]).
-export([edge_cases/1,
         addition/1, subtraction/1, negation/1,
         multiplication/1, mul_add/1, division/1,
         test_bitwise/1, test_bsl/1, test_bsr/1,
         element/1,
         range_optimization/1]).
-export([mul_add/0, division/0]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [{group, p}].

groups() ->
    [{p, [parallel],
      [edge_cases,
       addition, subtraction, negation, multiplication, mul_add, division,
       test_bitwise, test_bsl, test_bsr,
       element,
       range_optimization]}].

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
    try arith_test_1(A, B, MinS, MaxS) of
        ok -> ok
    catch
        error:Reason:Stk ->
            ct:fail("arith_test failed with ~p~n\tA = ~p~n\tB = ~p\n\t~p",
                    [Reason, A, B, Stk])
    end.

arith_test_1(A, B, MinS, MaxS) ->
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

    true = B =:= 0 orelse (((A div id(B)) * id(B) + A rem id(B)) =:= A),
    true = A =:= 0 orelse (((B div id(A)) * id(A) + B rem id(A)) =:= B),

    ok.

%% Test that the JIT only omits the overflow check when it's safe.
addition(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = add_gen_pairs(),
    %% io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_add_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_addition(Fs0, Mod),
    unload(Mod),
    ok.

add_gen_pairs() ->
    {MinSmall, MaxSmall} = determine_small_limits(0),

    %% Generate random pairs of smalls.
    N = 1000,
    M = MaxSmall + N div 2,
    Pairs0 = [{M - rand:uniform(N), rand:uniform(N)} ||
                 _ <- lists:seq(1, 75)],

    Seq = lists:seq(MinSmall-3, MinSmall+2) ++
        lists:seq(-5, 5),
        lists:seq(MaxSmall-2, MaxSmall+2),
    [{N1, N2} || N1 <- Seq, N2 <- Seq] ++ Pairs0.

gen_add_function({Name,{A,B}}) ->
    APlusOne = abs(A) + 1,
    BPlusOne = abs(B) + 1,
    ?Q("'@Name@'(integer, X0, Y0) when is_integer(X0), is_integer(Y0)->
           X1 = X0 rem _@APlusOne@,
           Y1 = Y0 rem _@BPlusOne@,
           Res = X0 + Y0,
           Res = X1 + Y1,
           Res = Y1 + X1,
           Res = X0 + Y1,
           Res = X1 + Y0;
        '@Name@'(number0, X, Y) when is_number(X), is_number(Y),
             X < _@APlusOne@, 0 =< Y, Y < _@BPlusOne@ ->
           Res = X + Y,
           Res = Y + X;
        '@Name@'(number0, X, Y) when is_number(X), is_number(Y),
             X < _@APlusOne@, -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           Res = X + Y,
           Res = Y + X;
        '@Name@'(number1, X, Y) when is_number(X), is_number(Y),
             X > -_@APlusOne@, 0 =< Y, Y < _@BPlusOne@ ->
           Res = X + Y,
           Res = Y + X;
        '@Name@'(number1, X, Y) when is_number(X), is_number(Y),
             X > -_@APlusOne@, -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           Res = X + Y,
           Res = Y + X;
        '@Name@'(number1, X, Y) when X > -_@APlusOne@, Y > -_@BPlusOne@ ->
           Res = X + Y,
           Res = Y + X. ").

test_addition([{Name,{A,B}}|T], Mod) ->
    F = fun Mod:Name/3,
    try
        Res0 = A + B,
        Res0 = F(integer, A, B),
        Res0 = F(number0, A, B),
        Res0 = F(number1, A, B),

        Res1 = -A + B,
        Res1 = F(integer, -A, B),
        Res1 = F(number0, -A, B),
        Res1 = F(number1, -A, B),

        Res2 = A + (-B),
        Res2 = F(integer, A, -B),
        Res2 = F(number0, A, -B),
        Res2 = F(number1, A, -B),

        Res3 = -A + (-B),
        Res3 = F(integer, -A, -B),
        Res3 = F(number0, -A, -B),
        Res3 = F(number1, -A, -B),

        AbsB = abs(B),
        Res4 = A + AbsB,
        Res4 = F(number0, A, AbsB),
        Res4 = F(number1, A, AbsB)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    bad_arith(F, [a], B),
    bad_arith(F, aa, B),
    bad_arith(F, A, [b]),
    bad_arith(F, A, bb),
    bad_arith(F, {a,b}, {c,d}),

    test_addition(T, Mod);
test_addition([], _) ->
    ok.

bad_arith(F, A, B) ->
    {'EXIT',{badarith,_}} = catch F(number1, A, B),
    ok.

%% Test that the JIT only omits the overflow check when it's safe.
subtraction(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = sub_gen_pairs(),
    %% io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_sub_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_subtraction(Fs0, Mod),
    unload(Mod),
    ok.

sub_gen_pairs() ->
    {MinSmall, MaxSmall} = determine_small_limits(0),

    %% Generate random pairs of smalls.
    N = 1000,
    M = MaxSmall + N div 2,
    Pairs0 = [{M - rand:uniform(N), M - rand:uniform(N)} ||
                 _ <- lists:seq(1, 75)],

    [{N1, N2} ||
        N1 <- lists:seq(MinSmall-2, MinSmall+2),
        N2 <- lists:seq(MaxSmall-2, MaxSmall+2)] ++ Pairs0.

gen_sub_function({Name,{A,B}}) ->
    APlusOne = abs(A) + 1,
    BPlusOne = abs(B) + 1,
    ?Q("'@Name@'(integer, X0, Y0) when is_integer(X0), is_integer(Y0)->
           X1 = X0 rem _@APlusOne@,
           Y1 = Y0 rem _@BPlusOne@,
           Res = X0 - Y0,
           Res = X1 - Y1,
           Res = X0 - Y1,
           Res = X1 - Y0;
        '@Name@'(number0, X, Y) when is_number(X), is_number(Y),
             X > -_@APlusOne@, 0 =< Y, Y < _@BPlusOne@ ->
           X - Y;
        '@Name@'(number0, X, Y) when is_number(X), is_number(Y),
             X > -_@APlusOne@, -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           X - Y;
        '@Name@'(number1, X, Y) when is_number(X), is_number(Y),
             X > -_@APlusOne@, 0 =< Y, Y < _@BPlusOne@ ->
           X - Y;
        '@Name@'(number1, X, Y) when is_number(X), is_number(Y),
             X > -_@APlusOne@, -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           X - Y;
        '@Name@'(number1, X, Y) when X > -_@APlusOne@, Y > -_@BPlusOne@ ->
           X - Y. ").

test_subtraction([{Name,{A,B}}|T], Mod) ->
    F = fun Mod:Name/3,
    try
        Res0 = A - B,
        Res0 = F(integer, A, B),
        Res0 = F(number0, A, B),

        Res1 = -A - B,
        Res1 = F(integer, -A, B),
        Res1 = F(number0, -A, B),

        Res2 = A - (-B),
        Res2 = F(integer, A, -B),
        Res2 = F(number0, A, -B),

        Res3 = -A - (-B),
        Res3 = F(integer, -A, -B),
        Res3 = F(number0, -A, -B),

        AbsB = abs(B),
        Res4 = A - AbsB,
        Res4 = F(integer, A, AbsB),
        Res4 = F(number0, A, AbsB)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    bad_arith(F, [a], B),
    bad_arith(F, aa, B),
    bad_arith(F, A, [b]),
    bad_arith(F, A, bb),
    bad_arith(F, {a,b}, {c,d}),

    test_subtraction(T, Mod);
test_subtraction([], _) ->
    ok.

%% Test that the JIT only omits the overflow check when it's safe.
negation(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Integers = neg_gen_integers(),
    %% io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Integers, 0),
    Fs = [gen_neg_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_negation(Fs0, Mod),
    unload(Mod),
    ok.

neg_gen_integers() ->
    {MinSmall, MaxSmall} = determine_small_limits(0),

    N = 1000,
    M = MaxSmall + N div 2,
    Ns = [M - rand:uniform(N) || _ <- lists:seq(1, 75)],

    lists:seq(MinSmall-2, MinSmall+2) ++ lists:seq(MaxSmall-2, MaxSmall+2) ++ Ns.

gen_neg_function({Name,A}) ->
    APlusOne = abs(A) + 1,
    ?Q("'@Name@'(integer0, X0) when is_integer(X0) ->
           X1 = X0 rem _@APlusOne@,
           Res = -X0,
           Res = -X1;
         '@Name@'(integer1, X) when is_integer(X), X > -_@APlusOne@ ->
           -X;
         '@Name@'(integer2, X) when is_integer(X), X > -_@APlusOne@ ->
           -X;
         '@Name@'(number, X) when is_number(X), X > -_@APlusOne@ ->
           -X;
         '@Name@'(number, X) when is_number(X), X > -_@APlusOne@ ->
           -X;
         '@Name@'(number, X) when is_number(X) ->
           -X. ").

test_negation([{Name,A}|T], Mod) ->
    F = fun Mod:Name/2,
    try
        Res0 = -A,
        Res0 = F(integer0, A),
        Res0 = F(integer1, A),
        Res0 = F(integer2, A),
        Res0 = F(number, A),

        Res1 = A,
        Res1 = F(integer0, -A),
        Res1 = F(integer1, -A),
        Res1 = F(integer2, -A),
        Res1 = F(number, -A)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p\n", [Name,A]),
            erlang:raise(C, R, Stk)
    end,

    test_negation(T, Mod);
test_negation([], _) ->
    ok.

%% Test that the JIT only omits the overflow check when it's safe.
multiplication(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = mul_gen_pairs(),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_mul_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_multiplication(Fs0, Mod),
    unload(Mod),
    ok.

mul_gen_pairs() ->
    {_, MaxSmall} = determine_small_limits(0),
    NumBitsMaxSmall = num_bits(MaxSmall),

    %% Generate random pairs of smalls.
    Pairs0 = [{rand:uniform(MaxSmall),rand:uniform(MaxSmall)} ||
                 _ <- lists:seq(1, 75)],

    %% Generate pairs of numbers whose product is small.
    SmallPairs = [{N, MaxSmall div N} ||
                     N <- [1,2,3,4,5,6,7,8,9,16,17,32,63,64,1111,22222]],
    Pairs1 = [{N,M-1} || {N,M} <- SmallPairs] ++ SmallPairs ++ Pairs0,

    %% Add prime factors of 2^59 - 1 (MAX_SMALL for 64-bit architecture
    %% at the time of writing).
    Pairs2 = [{179951,3203431780337}|Pairs1],

    %% Generate pairs of numbers whose product are bignums.
    LeastBig = MaxSmall + 1,
    Divisors = [(1 bsl Pow) + Offset ||
                   Pow <- lists:seq(NumBitsMaxSmall - 4, NumBitsMaxSmall - 1),
                   Offset <- [0,1,17,20333]],
    [{Div,ceil(LeastBig / Div)} || Div <- Divisors] ++ Pairs2.

gen_mul_function({Name,{A,B}}) ->
    APlusOne = A + 1,
    BPlusOne = B + 1,
    NumBitsA = num_bits(A),
    NumBitsB = num_bits(B),
    ?Q("'@Name@'(X0, Y0, More) when is_integer(X0), is_integer(Y0), is_boolean(More) ->
           X1 = X0 rem _@APlusOne@,
           Y1 = Y0 rem _@BPlusOne@,
           Res = X0 * Y0,
           Res = X1 * Y1,
           Res = Y1 * X1,
           if More ->
                Res = X1 * _@B@,
                Res = _@A@ * Y1,
                <<X2:_@NumBitsA@>> = <<X0:_@NumBitsA@>>,
                <<Y2:_@NumBitsB@>> = <<Y0:_@NumBitsB@>>,
                Res = X2 * Y2,
                Res = X1 * Y2,
                Res = X2 * Y1;
               true ->
                Res
           end;
        '@Name@'(X, Y, number) when -_@APlusOne@ < X, X < _@APlusOne@, -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           Res = X * Y,
           Res = Y * X;
        '@Name@'(X, fixed, number) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           X * _@B@;
        '@Name@'(X, fixed, any) ->
           X * _@B@;
        '@Name@'(fixed, Y, number) when -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           _@A@ * Y;
        '@Name@'(fixed, Y, any) ->
           _@A@ * Y. ").

test_multiplication([{Name,{A,B}}|T], Mod) ->
    F = fun Mod:Name/3,
    try
        Res0 = A * B,
        %% io:format("~p * ~p = ~p; size = ~p\n",
        %%           [A,B,Res0,erts_debug:flat_size(Res0)]),

        Res0 = F(A, B, true),
        Res0 = F(-A, -B, false),
        Res0 = F(A, B, number),
        Res0 = F(fixed, B, number),
        Res0 = F(fixed, B, any),
        Res0 = F(A, fixed, number),
        Res0 = F(A, fixed, any),
        Res0 = F(-A, -B, number),

        Res1 = -(A * B),
        Res1 = F(-A, B, false),
        Res1 = F(A, -B, false),
        Res1 = F(-A, B, number),
        Res1 = F(A, -B, number),
        Res1 = F(-A, fixed, number),
        Res1 = F(-A, fixed, any),
        Res1 = F(fixed, -B, number),
        Res1 = F(fixed, -B, any)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    test_multiplication(T, Mod);
test_multiplication([], _) ->
    ok.

mul_add() ->
    [{timetrap, {minutes, 5}}].
mul_add(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Triples = mul_add_triples(),
    Fs0 = gen_func_names(Triples, 0),
    Fs = [gen_mul_add_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all]).",
               "id(I) -> I."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_mul_add(Fs0, Mod),
    unload(Mod),

    test_mul_add_float(),
    test_mul_add_exceptions(),

    ok.

mul_add_triples() ->
    {_, MaxSmall} = determine_small_limits(0),
    SqrtMaxSmall = floor(math:sqrt(MaxSmall)),

    Numbers0 = [1,2,3,4,5,8,9,
                (MaxSmall div 2) band -2,
                MaxSmall band -2,
                MaxSmall * 2],
    Numbers = [rand:uniform(SqrtMaxSmall) || _ <- lists:seq(1, 5)] ++ Numbers0,

    %% Generate pairs of numbers whose product is small.
    SmallPairs = [{MaxSmall div M,M} || M <- Numbers],
    Pairs = [{N+M,M} || {N,M} <- SmallPairs] ++ SmallPairs,

    Triples0 = [{A,B,rand:uniform(MaxSmall)} || {A,B} <- Pairs],
    Triples1a = [{A,B,abs(MaxSmall - A * B)} || {A,B} <- Pairs],
    Triples1 = [{A,B,C+Offset} ||
                   {A,B,C} <- Triples1a,
                   Offset <- [-2,-1,0,1,2],
                   C + Offset >= 0],
    Triples2 = [{A,B,MaxSmall+1} || {A,B} <- Pairs],
    [{3,4,5},
     {MaxSmall div 2,2,42},                     %Result is not small.
     {MaxSmall,MaxSmall,MaxSmall}|Triples0 ++ Triples1 ++ Triples2].

gen_mul_add_function({Name,{A,B,C}}) ->
    APlusOne = A + 1,
    BPlusOne = B + 1,
    CPlusOne = C + 1,
    ?Q("'@Name@'(int_vvv_plus_z, X, Y, Z)
          when is_integer(X), is_integer(Y), is_integer(Z),
               -_@APlusOne@ < X, X < _@APlusOne@,
               -_@BPlusOne@ < Y, Y < _@BPlusOne@,
               -_@CPlusOne@ < Z, Z < _@CPlusOne@ ->
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res;
        '@Name@'(int_vvv_minus_z, X, Y, Z)
           when is_integer(X), is_integer(Y), is_integer(Z),
               -_@APlusOne@ < X, X < _@APlusOne@,
               -_@BPlusOne@ < Y, Y < _@BPlusOne@,
               -_@CPlusOne@ < Z, Z < _@CPlusOne@ ->
           Res = id(X * Y - Z),
           Res = id(Y * X - Z),
           Res;
        '@Name@'(pos_int_vvv_plus_z, X, Y, Z)
          when is_integer(X), is_integer(Y), is_integer(Z),
               0 =< X, X < _@APlusOne@,
               0 =< Y, Y < _@BPlusOne@,
               0 =< Z, Z < _@CPlusOne@ ->
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res;
        '@Name@'(neg_int_vvv_plus_z, X, Y, Z)
          when is_integer(X), is_integer(Y), is_integer(Z),
               -_@APlusOne@ < X, X < 0,
               -_@BPlusOne@ < Y, Y < 0,
               -_@CPlusOne@ < Z, Z < 0 ->
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res;
        '@Name@'(int_vii_plus_z, X, fixed, fixed)
          when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Z = _@C@,
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res = '@Name@'(any_vvi_plus_z, X, id(Y), fixed),
           Res = '@Name@'(any_vvv_minus_z, X, id(Y), id(-Z)),
           Res;
        '@Name@'(any_vvv_plus_z, X, Y, Z) ->
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res = '@Name@'(int_vvv_plus_z, id(X), id(Y), id(Z)),
           Res;
        '@Name@'(any_vvv_minus_z, X, Y, Z) ->
           Res = id(X * Y - Z),
           Res = id(Y * X - Z),
           Res = '@Name@'(int_vvv_minus_z, id(X), id(Y), id(Z)),
           Res;
        '@Name@'(any_vvi_plus_z, X, Y, _Z) ->
           Z = _@C@,
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res = '@Name@'(any_vvv_plus_z, X, Y, id(Z)),
           Res = '@Name@'(any_vvv_minus_z, X, Y, id(-Z)),
           Res;
        '@Name@'(any_vvi_minus_z, X, Y, _Z) ->
           Z = _@C@,
           Res = id(X * Y - Z),
           Res = id(Y * X - Z),
           Res = id(-Z + X * Y),
           Res = id(-Z + Y * X),
           Res = '@Name@'(any_vvv_plus_z, X, Y, id(-Z)),
           Res = '@Name@'(any_vvv_minus_z, X, Y, id(Z)),
           Res;
        '@Name@'(any_vii_plus_z, X, fixed, fixed) ->
           Y = _@B@,
           Z = _@C@,
           Res = id(X * Y + Z),
           Res = id(Y * X + Z),
           Res = id(Z + X * Y),
           Res = id(Z + Y * X),
           Res = '@Name@'(any_vvi_plus_z, X, id(Y), fixed),
           Res = '@Name@'(any_vvv_minus_z, X, id(Y), id(-Z)),
           Res;
        '@Name@'(any_vii_minus_z, X, fixed, fixed) ->
           Y = _@B@,
           Z = _@C@,
           Res = id(X * Y - Z),
           Res = id(Y * X - Z),
           Res = id(-Z + X * Y),
           Res = id(-Z + Y * X),
           Res = '@Name@'(any_vvi_minus_z, X, id(Y), fixed),
           Res = '@Name@'(any_vvv_plus_z, X, Y, id(-Z)),
           Res;
        '@Name@'({guard_plus_z,Res}, X, Y, Z) when X * Y + Z =:= Res ->
           ok;
        '@Name@'({guard_minus_z,Res}, X, Y, Z) when X * Y - Z =:= Res ->
           ok. ").

test_mul_add([{Name,{A,B,C}}|T], Mod) ->
    F = fun Mod:Name/4,
    try
        Res0 = A * B + C,
        Res0 = F(any_vii_plus_z, A, fixed, fixed),
        Res0 = F(pos_int_vvv_plus_z, A, B, C),
        Res0 = F(int_vii_plus_z, A, fixed, fixed),
        ok = F({guard_plus_z,Res0}, A, B, C),
        ok = F({guard_plus_z,Res0}, -A, -B, C),

        Res1 = A * B - C,
        Res1 = F(any_vii_minus_z, A, fixed, fixed),
        Res1 = if
                   A > 0, B > 0, C > 0 ->
                       F(neg_int_vvv_plus_z, -A, -B, -C);
                   true ->
                       Res1
              end,
        ok = F({guard_minus_z,Res1}, A, B, C),
        ok = F({guard_minus_z,Res1}, -A, -B, C),

        Res2 = -A * B + C,
        Res2 = A * -B + C,
        Res2 = F(any_vii_plus_z, -A, fixed, fixed),
        ok = F({guard_plus_z,Res2}, -A, B, C),

        Res3 = -A * B - C,
        Res3 = A * -B - C,
        Res3 = F(any_vii_minus_z, -A, fixed, fixed),
        ok = F({guard_minus_z,Res3}, -A, B, C)
    catch
        Class:R:Stk ->
            io:format("~p failed. numbers: ~p ~p ~p\n", [Name,A,B,C]),
            erlang:raise(Class, R, Stk)
    end,
    test_mul_add(T, Mod);
test_mul_add([], _) ->
    ok.

test_mul_add_float() ->
    Res = madd(id(2.0), id(3.0), id(7.0)),
    Res = madd(id(2.0), id(3.0), id(7)),
    ok = madd(id(2.0), id(3.0), id(7), id(Res)).

test_mul_add_exceptions() ->
    error = madd(id(a), id(2), id(3), id(whatever)),
    error = madd(id(7), id(b), id(3), id(whatever)),
    error = madd(id(7), id(15), id(c), id(whatever)),

    {'EXIT',{badarith,[{erlang,'*',[a,2],_}|_]}} = catch madd(id(a), id(2), id(0)),
    {'EXIT',{badarith,[{erlang,'*',[a,2],_}|_]}} = catch madd(id(a), id(2), id(42)),
    {'EXIT',{badarith,[{erlang,'*',[a,2],_}|_]}} = catch madd(id(a), id(2), id(c)),
    {'EXIT',{badarith,[{erlang,'*',[3,b],_}|_]}} = catch madd(id(3), id(b), id(c)),
    {'EXIT',{badarith,[{erlang,'+',[6,c],_}|_]}} = catch madd(id(2), id(3), id(c)),

    ok.

madd(A, B, C) -> A * B + C.

madd(A, B, C, Res) when Res =:= A * B + C -> ok;
madd(_, _, _, _) -> error.


%% Test that the JIT only omits the overflow check when it's safe.
division() ->
    [{timetrap, {minutes, 5}}].
division(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = div_gen_pairs(),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_div_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_division(Fs0, Mod),

    3 = ignore_rem(ignore, 10, 3),
    1 = ignore_div(ignore, 16, 5),

    unload(Mod),

    ok.

ignore_rem(_, X, Y) ->
    _ = X rem Y,                                %Result in x0.
    X div Y.                                    %Reuse x0 for result.

ignore_div(_, X, Y) ->
    _ = X div Y,                                %Result in x0.
    X rem Y.                                    %Reuse x0 for result.

div_gen_pairs() ->
    {_, MaxSmall} = determine_small_limits(0),
    NumBitsMaxSmall = num_bits(MaxSmall),

    Divisors = [-8,-2,-1,1,2,3,4,5,8,16,17,64,22222333] ++
        [1 bsl P || P <- lists:seq(8, 12) ++ lists:seq(26, 36)],

    %% Generate random pairs of smalls.
    Pairs0 = [{rand:uniform(MaxSmall),
               rand:uniform(MaxSmall) * rand_sign()} ||
                 _ <- lists:seq(1, 50)],
    Pairs1 = [{rand:uniform(MaxSmall), N} || N <- Divisors] ++ Pairs0,
    Pairs2 = [{N, M} || N <- lists:seq(0, 7), M <- [-2,-1,1,2,3,4]] ++ Pairs1,
    Pairs3 = [{abs(M) * (rand:uniform(10)+1) + rand:uniform(1000), M} ||
                 M <- Divisors] ++ Pairs2,

    %% Generate pairs of numbers whose product are bignums.
    [{rand:uniform(MaxSmall),1 bsl Pow} ||
        Pow <- lists:seq(NumBitsMaxSmall - 4, NumBitsMaxSmall - 1)] ++ Pairs3.

rand_sign() ->
    case rand:uniform() < 0.2 of
        true -> -1;
        false -> 1
    end.

gen_div_function({Name,{A,B}}) ->
    APlusOne = abs(A) + 1,
    BPlusOne = abs(B) + 1,
    NumBitsA = num_bits(abs(A)+1),
    NumBitsB = num_bits(abs(B)+1),
    ?Q("'@Name@'(integer0, X0, Y0) ->
           Q = X0 div Y0,
           R = X0 rem Y0,
           if X0 > 0, Y0 > 0 ->
             <<X:_@NumBitsA@>> = <<X0:_@NumBitsA@>>,
             <<Y:_@NumBitsB@>> = <<Y0:_@NumBitsB@>>,
             Q = X div Y,
             R = X rem Y,
             {Q, R};
           true ->
             {Q, R}
           end;
        '@Name@'(integer1, X, fixed) when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(integer2, X, fixed) when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(integer3, X, fixed) when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(pos_integer1, X, fixed) when is_integer(X), 0 =< X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(pos_integer2, X, fixed) when is_integer(X), 0 =< X, X < _@APlusOne@ ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(pos_integer3, X, fixed) when is_integer(X), 0 =< X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(number0, X, Y) when -_@APlusOne@ < X, X < _@APlusOne@,
                                    -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(number1, X, Y) when -_@APlusOne@ < X, X < _@APlusOne@,
                                    -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(number2, X, fixed) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(number3, X, fixed) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(number4, X, fixed) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(any0, X, fixed) ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(any1, X, fixed) ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(any2, X, fixed) ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(any0, fixed, Y) ->
           X = _@A@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(any1, fixed, Y) ->
           X = _@A@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(any2, fixed, Y) ->
           X = _@A@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(X0, Y0, integer0) ->
           Q = X0 div Y0,
           R = X0 rem Y0,
           if X0 > 0, Y0 > 0 ->
             <<X:_@NumBitsA@>> = <<X0:_@NumBitsA@>>,
             <<Y:_@NumBitsB@>> = <<Y0:_@NumBitsB@>>,
             Q = X div Y,
             R = X rem Y,
             {Q, R};
           true ->
             {Q, R}
           end;
        '@Name@'(X, fixed, integer1) when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(X, fixed, integer2) when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(X, fixed, integer3) when is_integer(X), -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(X, fixed, pos_integer1) when is_integer(X), 0 =< X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(X, fixed, pos_integer2) when is_integer(X), 0 =< X, X < _@APlusOne@ ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(X, fixed, pos_integer3) when is_integer(X), 0 =< X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(X, Y, number0) when -_@APlusOne@ < X, X < _@APlusOne@,
                                    -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(X, Y, number1) when -_@APlusOne@ < X, X < _@APlusOne@,
                                    -_@BPlusOne@ < Y, Y < _@BPlusOne@ ->
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(X, fixed, number2) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(X, fixed, number3) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(X, fixed, number4) when -_@APlusOne@ < X, X < _@APlusOne@ ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(X, fixed, any0) ->
           Y = _@B@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(X, fixed, any1) ->
           Y = _@B@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(X, fixed, any2) ->
           Y = _@B@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R};
        '@Name@'(fixed, Y, any0) ->
           X = _@A@,
           Q = X div Y,
           R = X rem Y,
           {Q, R};
        '@Name@'(fixed, Y, any1) ->
           X = _@A@,
           R = X rem Y,
           Q = X div Y,
           {Q, R};
        '@Name@'(fixed, Y, any2) ->
           X = _@A@,
           Q = X div Y,
           put(prevent_div_rem_fusion, Q),
           R = X rem Y,
           {Q, R}. ").


test_division([{Name,{A,B}}|T], Mod) ->
    F = fun Mod:Name/3,
    try
        PosRes = {A div B, A rem B},
        NegRes = {-A div B, -A rem B},

        PosRes = F(integer0, A, B),
        PosRes = F(integer1, A, fixed),
        PosRes = F(integer2, A, fixed),
        PosRes = F(integer3, A, fixed),
        PosRes = F(pos_integer1, A, fixed),
        PosRes = F(pos_integer2, A, fixed),
        PosRes = F(pos_integer3, A, fixed),
        PosRes = F(number0, A, B),
        PosRes = F(number1, A, B),
        PosRes = F(number2, A, fixed),
        PosRes = F(number3, A, fixed),
        PosRes = F(number4, A, fixed),
        PosRes = F(any0, A, fixed),
        PosRes = F(any1, A, fixed),
        PosRes = F(any2, A, fixed),
        PosRes = F(any0, fixed, B),
        PosRes = F(any1, fixed, B),
        PosRes = F(any2, fixed, B),

        PosRes = F(A, B, integer0),
        PosRes = F(A, fixed, integer1),
        PosRes = F(A, fixed, integer2),
        PosRes = F(A, fixed, integer3),
        PosRes = F(A, fixed, pos_integer1),
        PosRes = F(A, fixed, pos_integer2),
        PosRes = F(A, fixed, pos_integer3),
        PosRes = F(A, B, number0),
        PosRes = F(A, B, number1),
        PosRes = F(A, fixed, number2),
        PosRes = F(A, fixed, number3),
        PosRes = F(A, fixed, number4),
        PosRes = F(A, fixed, any0),
        PosRes = F(A, fixed, any1),
        PosRes = F(A, fixed, any2),
        PosRes = F(fixed, B, any0),
        PosRes = F(fixed, B, any1),
        PosRes = F(fixed, B, any2),

        NegRes = F(integer0, -A, B),
        NegRes = F(integer1, -A, fixed),
        NegRes = F(integer2, -A, fixed),
        NegRes = F(integer3, -A, fixed),
        NegRes = F(number0, -A, B),
        NegRes = F(number1, -A, B),
        NegRes = F(number2, -A, fixed),
        NegRes = F(number3, -A, fixed),
        NegRes = F(number4, -A, fixed),

        NegRes = F(-A, B, integer0),
        NegRes = F(-A, fixed, integer1),
        NegRes = F(-A, fixed, integer2),
        NegRes = F(-A, fixed, integer3),
        NegRes = F(-A, B, number0),
        NegRes = F(-A, B, number1),
        NegRes = F(-A, fixed, number2),
        NegRes = F(-A, fixed, number3),
        NegRes = F(-A, fixed, number4)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    test_division(T, Mod);
test_division([], _) ->
    ok.

%% Test that the JIT only omits the overflow check when it's safe.
test_bitwise(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = bitwise_gen_pairs(),
    %% io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_bitwise_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_bitwise(Fs0, Mod),
    unload(Mod),

    %% Test invalid operands.
    expect_badarith(fun(X) -> 42 band X end),
    expect_badarith(fun(X) -> 42 bor X end),
    expect_badarith(fun(X) -> 42 bxor X end),
    expect_badarith(fun(X) -> X band 42 end),
    expect_badarith(fun(X) -> X bor 42 end),
    expect_badarith(fun(X) -> X bxor 42 end),
    expect_fc(fun(X) when is_integer(42 band X) -> ok end),
    expect_fc(fun(X) when is_integer(42 bor X) -> ok end),
    expect_fc(fun(X) when is_integer(42 bxor X) -> ok end),
    expect_fc(fun(X) when is_integer(X band 42) -> ok end),
    expect_fc(fun(X) when is_integer(X bor 42) -> ok end),
    expect_fc(fun(X) when is_integer(X bxor 42) -> ok end),

    ok.

expect_fc(Fun) ->
    {'EXIT',{function_clause,_}} = catch Fun(id(bad)),
    ok.

expect_badarith(Fun) ->
    {'EXIT',{badarith,_}} = catch Fun(id(bad)),
    ok.

bitwise_gen_pairs() ->
    {MinSmall, MaxSmall} = determine_small_limits(0),

    %% Generate random pairs of smalls.
    N = 1000,
    M = MaxSmall + N div 2,
    Pairs0 = [{M - rand:uniform(N), rand:uniform(N)} ||
                 _ <- lists:seq(1, 75)],

    Seq = lists:seq(MinSmall-3, MinSmall+2) ++
        lists:seq(-5, 5),
        lists:seq(MaxSmall-2, MaxSmall+2),
    [{N1, N2} || N1 <- Seq, N2 <- Seq] ++ Pairs0.

gen_bitwise_function({Name,{A,B}}) ->
    APlusOne = abs(A) + 1,
    BPlusOne = abs(B) + 1,
    ?Q("'@Name@'(X0, Y0) when is_integer(X0), is_integer(Y0)->
           X1 = X0 rem _@APlusOne@,
           Y1 = Y0 rem _@BPlusOne@,

           AndRes = X0 band Y0,
           AndRes = Y0 band X0,
           AndRes = X1 band Y1,
           AndRes = Y1 band X1,
           AndRes = X0 band Y1,
           AndRes = X1 band Y0,

           OrRes = X0 bor Y0,
           OrRes = Y0 bor X0,
           OrRes = X1 bor Y1,
           OrRes = Y1 bor X1,
           OrRes = X0 bor Y1,
           OrRes = X1 bor Y0,

           XorRes = X0 bxor Y0,
           XorRes = Y0 bxor X0,
           XorRes = X1 bxor Y1,
           XorRes = Y1 bxor X1,
           XorRes = X0 bxor Y1,
           XorRes = X1 bxor Y0,

           {AndRes, OrRes, XorRes};
        '@Name@'(X0, fixed) when is_integer(X0) ->
           X1 = X0 rem _@APlusOne@,

           AndRes = X0 band _@B@,
           AndRes = _@B@ band X0,
           AndRes = X1 band _@B@,
           AndRes = _@B@ band X1,

           OrRes = X0 bor _@B@,
           OrRes = _@B@ bor X0,
           OrRes = X1 bor _@B@,
           OrRes = _@B@ bor X1,

           XorRes = X0 bxor _@B@,
           XorRes = _@B@ bxor X0,
           XorRes = X1 bxor _@B@,
           XorRes = _@B@ bxor X1,

           {AndRes, OrRes, XorRes}.
 ").

test_bitwise([{Name,{A,B}}|T], Mod) ->
    try
        test_bitwise_1(A, B, Mod, Name),
        test_bitwise_1(-A, B, Mod, Name),
        test_bitwise_1(A, -B, Mod, Name),
        test_bitwise_1(-A, -B, Mod, Name),

        AndRes = A band B,
        OrRes = A bor B,
        XorRes = A bxor B,
        {AndRes, OrRes, XorRes} = Mod:Name(A, fixed)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,
    test_bitwise(T, Mod);
test_bitwise([], _) ->
    ok.

test_bitwise_1(A, B, Mod, Name) ->
    AndRes = A band B,
    OrRes = A bor B,
    XorRes = A bxor B,
    {AndRes, OrRes, XorRes} = Mod:Name(A, B),
    ok.

%% Test that the JIT only omits the overflow check when it's safe.
test_bsl(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = bsl_gen_pairs(),
    %% io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_bsl_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_bsl(Fs0, Mod),
    unload(Mod),
    ok.

bsl_gen_pairs() ->
    {_MinSmall, MaxSmall} = determine_small_limits(0),
    SmallBits = num_bits(MaxSmall),

    [{N,S} ||
        P <- lists:seq(20, SmallBits),
        N <- [(1 bsl P)-rand:uniform(1000), (1 bsl P)-1, 1 bsl P],
        S <- lists:seq(SmallBits-P-4, SmallBits - P + 3)].

gen_bsl_function({Name,{N,S}}) ->
    Mask = (1 bsl num_bits(N)) - 1,
    ?Q("'@Name@'(N0, fixed) ->
           Res = N0 bsl _@S@,
           N = N0 band _@Mask@,
           Res = N0 bsl _@S@,
           Res = N bsl _@S@;
        '@Name@'(N0, S) when is_integer(S), 0 =< S, S =< _@S@ ->
           Res = N0 bsl S,
           N = N0 band _@Mask@,
           Res = N0 bsl S,
           Res = N bsl S. ").

test_bsl([{Name,{N,S}}|T], Mod) ->
    try
        Res0 = N bsl S,
        Res0 = Mod:Name(N, fixed),

        if
            S >= 0 ->
                Res1 = N bsl S,
                Res1 = Mod:Name(N, S),

                N = Mod:Name(N, 0),

                if
                    S >= 2 ->
                        Res2 = N bsl (S - 1),
                        Res2 = Mod:Name(N, S - 1),

                        Res3 = N bsl (S - 2),
                        Res3 = Mod:Name(N, S - 2);
                    true ->
                        ok
                end;
            S < 0 ->
                {'EXIT', {function_clause,_}} = catch Mod:Name(N, S)
        end
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,N,S]),
            erlang:raise(C, R, Stk)
    end,
    test_bsl(T, Mod);
test_bsl([], _) ->
    ok.

test_bsr(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = bsr_gen_pairs(),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_bsr_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all]).",
               "id(I) -> I."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_bsr(Fs0, Mod),
    unload(Mod),
    ok.

bsr_gen_pairs() ->
    {_MinSmall, MaxSmall} = determine_small_limits(0),
    SmallBits = num_bits(MaxSmall),

    {Powers,Shifts} =
        if
            SmallBits < 32 ->
                {lists:seq(15, SmallBits+2),
                 lists:seq(0, 7) ++ lists:seq(24, 36)};
            true ->
                {lists:seq(30, SmallBits+2),
                 lists:seq(0, 7) ++ lists:seq(56, 72)}
        end,

    [{N,S} ||
        P <- Powers,
        N <- [rand:uniform(1 bsl P), (1 bsl P)-1],
        S <- Shifts].

gen_bsr_function({Name,{N,S}}) ->
    Mask = (1 bsl num_bits(N)) - 1,
    ?Q("'@Name@'(N0, fixed, More) ->
           Res = N0 bsr _@S@,
           if
               More ->
                   N = N0 band _@Mask@,
                   Res = N0 bsr _@S@,
                   Res = N bsr _@S@;
               true ->
                   Res
           end;
        '@Name@'(N0, S, More) ->
           Res = id(N0 bsr S),
           if
               More ->
                   N = N0 band _@Mask@,
                   Res = id(N0 bsr S),
                   Res = id(N bsr S),
                   if
                      S >= 0 ->
                          Res = id(N bsr S);
                      true ->
                           Res
                   end;
               true ->
                   Res
           end. ").

test_bsr([{Name,{N,S}}|T], Mod) ->
    try
        Res = N bsr S,
        Res = Mod:Name(N, fixed, true),
        Res = Mod:Name(N, S, true),

        NegRes = -N bsr S,
        NegRes = Mod:Name(-N, fixed, false),

        NegRes = -N bsr S,
        NegRes = Mod:Name(-N, S, false),

        BslRes = N bsr -S,
        BslRes = Mod:Name(N, -S, false)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,N,S]),
            erlang:raise(C, R, Stk)
    end,
    test_bsr(T, Mod);
test_bsr([], _) ->
    ok.

element(_Config) ->
    %% Test element_1: Can't fail for integer arguments.
    zero = element_1(0),
    one = element_1(1),
    two = element_1(2),
    three = element_1(3),

    three = element_1(3-4),
    two = element_1(2-4),
    one = element_1(1-4),
    zero = element_1(0-4),

    zero = element_1(0+4),
    one = element_1(1+4),

    {'EXIT',{badarith,_}} = catch element_1(id(a)),

    %% Test element_2: Test that it fails for 0.
    one = element_2(1),
    two = element_2(2),
    three = element_2(3),

    one = element_2(1+4),
    two = element_2(2+4),
    three = element_2(3+4),

    {'EXIT',{badarg,[{erlang,element,[0,{one,two,three}],_}|_]}} =
        catch element_2(id(0)),
    {'EXIT',{badarith,_}} = catch element_2(id(b)),

    %% Test element_3: Test that if fails for integers less than 1.
    one = element_3(1),
    two = element_3(2),
    three = element_3(3),

    one = element_3(1+4),
    two = element_3(2+4),
    three = element_3(3+4),

    {'EXIT',{badarg,[{erlang,element,[0,{one,two,three}],_}|_]}} =
        catch element_3(id(0)),
    {'EXIT',{badarg,_}} = catch element_3(id(-1)),
    {'EXIT',{badarg,_}} = catch element_3(id(-999)),
    {'EXIT',{badarith,_}} = catch element_3(id(c)),

    %% Test element_4: Test that it fails for integers outside of the range 1..3.
    one = element_4(1),
    two = element_4(2),
    three = element_4(3),

    one = element_4(1+8),
    two = element_4(2+8),
    three = element_4(3+8),

    {'EXIT',{badarg,[{erlang,element,[0,{one,two,three}],_}|_]}} =
        catch element_4(id(0)),
    {'EXIT',{badarg,[{erlang,element,[5,{one,two,three}],_}|_]}} =
        catch element_4(id(5)),
    {'EXIT',{badarg,_}} = catch element_4(id(-1)),
    {'EXIT',{badarg,[{erlang,element,[-7,{one,two,three}],_}|_]}} =
        catch element_4(id(-999)),
    {'EXIT',{badarith,_}} = catch element_4(id(d)),

    %% Test element_5: Test that it fails for integers outside of the
    %% range 0..3.
    zero = element_5(0),
    one = element_5(1),
    two = element_5(2),
    three = element_5(3),

    zero = element_5(0+8),
    one = element_5(1+8),
    two = element_5(2+8),
    three = element_5(3+8),

    {'EXIT',{badarg,[{erlang,element,[5,{zero,one,two,three}],_}|_]}} =
        catch element_5(id(4)),
    {'EXIT',{badarg,[{erlang,element,[0,{zero,one,two,three}],_}|_]}} =
        catch element_5(id(-1)),
    {'EXIT',{badarith,_}} = catch element_5(id(e)),

    %% element_6: Test that it fails for values outside of 0..3.
    zero = element_6(0),
    one = element_6(1),
    two = element_6(2),
    three = element_6(3),

    {'EXIT',{badarg,[{erlang,element,[5,{zero,one,two,three}],_}|_]}} =
        catch element_6(id(4)),
    {'EXIT',{badarg,[{erlang,element,[0,{zero,one,two,three}],_}|_]}} =
        catch element_6(id(-1)),

    %% Test element_7: Test that it fails for values outside of 1..3.
    one = element_7(1),
    two = element_7(2),
    three = element_7(3),

    one = element_7(1+5),
    two = element_7(2+5),
    three = element_7(3+5),

    {'EXIT',{badarg,[{erlang,element,[0,{one,two,three}],_}|_]}} =
        catch element_7(id(0)),
    {'EXIT',{badarg,[{erlang,element,[4,{one,two,three}],_}|_]}} =
        catch element_7(id(4)),
    {'EXIT',{badarith,_}} = catch element_7(id(f)),

    %% element_8: Test that it works in a guard.
    ok = element_8(id(1), id(a)),
    error = element_8(id(1), id(b)),
    error = element_8(id(-1), whatever),
    error = element_8(id(0), whatever),
    error = element_8(id(5), whatever),

    ok.

element_1(N0) ->
    N = N0 band 3,
    element(N + 1, {zero,one,two,three}).

element_2(N0) ->
    N = N0 band 3,
    element(N, {one,two,three}).

element_3(N0) ->
    N = N0 rem 4,
    element(N, {one,two,three}).

element_4(N0) ->
    N = N0 rem 8,
    element(N, {one,two,three}).

element_5(N0) ->
    N = N0 rem 8,
    element(N + 1, {zero,one,two,three}).

element_6(N) when is_integer(N) ->
    element(N + 1, {zero,one,two,three}).

element_7(N0) ->
    N = N0 rem 5,
    %% Max N is one more than the size of the tuple.
    element(N, {one,two,three}).

element_8(N0, E) ->
    N = N0 rem 8,
    if
        element(N, {a,b,c,d}) =:= E ->
            ok;
        true ->
            error
    end.

%% Test basic range optimization of arguments.
range_optimization(_Config) ->
    immed_reg_confusion(),

    ok.

%% The JIT confused x15/y15 with smalls when checking whether an argument fell
%% within the range of a small, because `is_small(arg.getValue())` happened to
%% be true.
immed_reg_confusion() ->
    M = any_integer(1),
    N = any_integer(1 bsl 128),
    Res = any_integer(M bor N),

    Res = bor_x0_x15(M, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, N),

    ok.

bor_x0_x15(_x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9,
           _x10, _x11, _x12, _x13, _x14, _x15) ->
    _x0 bor _x15.

any_integer(I) ->
    case id(I) of
        N when is_integer(N) -> N
    end.

%%%
%%% Helpers.
%%%

gen_func_names([E|Es], I) ->
    Name = list_to_atom("f" ++ integer_to_list(I)),
    [{Name,E}|gen_func_names(Es, I+1)];
gen_func_names([], _) -> [].

num_bits(Int) when Int >= 0 ->
    num_bits(Int, 0).

num_bits(0, N) -> N;
num_bits(Int, N) -> num_bits(Int bsr 1, N + 1).

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

unload(Mod) ->
    _ = code:delete(Mod),
    code:purge(Mod).

id(I) -> I.
