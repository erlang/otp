%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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
-export([edge_cases/1, addition/1, subtraction/1, multiplication/1,
         element/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [{group, p}].

groups() ->
    [{p, [parallel],
      [edge_cases, addition, subtraction, multiplication,
       element]}].

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
    io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_add_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_addition(Fs0, Mod),
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
    ?Q("'@Name@'(X0, Y0) when is_integer(X0), is_integer(Y0)->
           X1 = X0 rem _@APlusOne@,
           Y1 = Y0 rem _@BPlusOne@,
           Res = X0 + Y0,
           Res = X1 + Y1,
           Res = Y1 + X1,
           Res = X0 + Y1,
           Res = X1 + Y0. ").

test_addition([{Name,{A,B}}|T], Mod) ->
    try
        Res0 = A + B,
        Res0 = Mod:Name(A, B),

        Res1 = -A + B,
        Res1 = Mod:Name(-A, B),

        Res2 = A + (-B),
        Res2 = Mod:Name(A, -B),

        Res3 = -A + (-B),
        Res3 = Mod:Name(-A, -B)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    test_addition(T, Mod);
test_addition([], _) ->
    ok.

%% Test that the JIT only omits the overflow check when it's safe.
subtraction(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),
    Mod = list_to_atom(lists:concat([?MODULE,"_",?FUNCTION_NAME])),
    Pairs = sub_gen_pairs(),
    io:format("~p\n", [Pairs]),
    Fs0 = gen_func_names(Pairs, 0),
    Fs = [gen_sub_function(F) || F <- Fs0],
    Tree = ?Q(["-module('@Mod@').",
               "-compile([export_all,nowarn_export_all])."]) ++ Fs,
    %% merl:print(Tree),
    {ok,_Bin} = merl:compile_and_load(Tree, []),
    test_subtraction(Fs0, Mod),
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
    ?Q("'@Name@'(X0, Y0) when is_integer(X0), is_integer(Y0)->
           X1 = X0 rem _@APlusOne@,
           Y1 = Y0 rem _@BPlusOne@,
           Res = X0 - Y0,
           Res = X1 - Y1,
           Res = X0 - Y1,
           Res = X1 - Y0. ").

test_subtraction([{Name,{A,B}}|T], Mod) ->
    try
        Res0 = A - B,
        Res0 = Mod:Name(A, B),

        Res1 = -A - B,
        Res1 = Mod:Name(-A, B),

        Res2 = A - (-B),
        Res2 = Mod:Name(A, -B),

        Res3 = -A - (-B),
        Res3 = Mod:Name(-A, -B)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    test_subtraction(T, Mod);
test_subtraction([], _) ->
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
    ok.

mul_gen_pairs() ->
    {_, MaxSmall} = determine_small_limits(0),
    NumBitsMaxSmall = num_bits(MaxSmall),

    %% Generate random pairs of smalls.
    Pairs0 = [{rand:uniform(MaxSmall),rand:uniform(MaxSmall)} ||
                 _ <- lists:seq(1, 75)],

    %% Generate pairs of numbers whose product is small.
    Pairs1 = [{N, MaxSmall div N} || N <- [1,2,3,5,17,63,64,1111,22222]] ++ Pairs0,

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
    ?Q("'@Name@'(X0, Y0, More) when is_integer(X0), is_integer(Y0)->
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
           end. ").

test_multiplication([{Name,{A,B}}|T], Mod) ->
    try
        Res0 = A * B,
        %% io:format("~p * ~p = ~p; size = ~p\n",
        %%           [A,B,Res0,erts_debug:flat_size(Res0)]),

        Res0 = Mod:Name(A, B, true),
        Res0 = Mod:Name(-A, -B, false),

        Res1 = -(A * B),
        Res1 = Mod:Name(-A, B, false),
        Res1 = Mod:Name(A, -B, false)
    catch
        C:R:Stk ->
            io:format("~p failed. numbers: ~p ~p\n", [Name,A,B]),
            erlang:raise(C, R, Stk)
    end,

    test_multiplication(T, Mod);
test_multiplication([], _) ->
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

id(I) -> I.
