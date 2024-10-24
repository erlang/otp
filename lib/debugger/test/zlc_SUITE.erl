%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2024. All Rights Reserved.
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
-module(zlc_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2,
         init_per_testcase/2,end_per_testcase/2,
         basic/1,mixed_zlc/1,zmc/1,filter_guard/1,
         filter_pattern/1,cartesian/1,nomatch/1,bad_generators/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [basic,
     mixed_zlc,
     zmc,
     filter_guard,
     filter_pattern,
     cartesian,
     nomatch,
     bad_generators].

groups() ->
    [].

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Config.

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    ok.

basic(Config) when is_list(Config) ->
    [6, 7, 8] = [X + Y + Z || X <- [1, 2, 3] && Y <- [2, 2, 2] && Z <- [3,3,3]],
    [{1, 2, 3}, {2, 2, 3}, {3, 2, 3}] =
        [{X, Y, Z} || X <- [1, 2, 3] && Y <- [2, 2, 2] && Z <- [3,3,3]],
    [6, 24] = zipwith4(fun(A, B, C, D) -> (A + B + C) * D end,
                       [1, 5], [2, 2], [0, 1], [2, 3]),
    96 = dot([1, 2, 3, 4], [24, 12, 8, 6]),

    [1, 4, 5] = ifelse([true, false, true], [1, 3, 5], [2, 4, 6]),

    [8, 14, 20] = [X + Y + Z || <<X>> <= <<5, 10, 15>> && <<Y>> <= <<1, 2, 3>>
                                    && <<Z>> <= <<2, 2, 2>>].


zipwith4(F, As, Bs, Cs, Ds) ->
    [F(A,B,C,D) || A <- As && B <- Bs && C <- Cs && D <- Ds].

dot(Xs, Ys) ->
    lists:sum([X*Y || X <- Xs && Y <- Ys]).

ifelse(Tests, Xs, Ys) -> % Simulate R's ifelse(,,)
    [  case T of true -> X ; false -> Y end
       || T <- Tests && X <- Xs && Y <- Ys
    ].

mixed_zlc(Config) when is_list(Config) ->
    [{a, 2}, {b, 4}, {c, 6}] = [{X,Y} || X <- [a,b,c] && <<Y>> <= <<2,4,6>>],
    [{a, 2}, {b, 4}, {c, 6}] = [{X,Y} || <<Y>> <= <<2,4,6>> && X <- [a,b,c]],
    [{a,c,1,3}, {b,d,2,4}] = [{K1,K2,V1,V2}||
                                 K1 := V1 <- maps:iterator(#{a=>1, b=>2}, ordered) &&
                                     K2 := V2 <- maps:iterator(#{c=>3, d=>4}, ordered)],
    [{a,1,2}, {b,2,4}] =
        [{K1,V1,Y} || K1 := V1 <- maps:iterator(#{a=>1, b=>2}, ordered) &&
                          <<Y>> <= <<2,4>>],
    [{a,1,2}, {b,2,4}] = [{K1,V1,Y} ||
                             K1 := V1 <- maps:iterator(#{a=>1, b=>2}, ordered) &&
                                 <<Y>> <= <<2,4>>],
    <<3,4,5>> = << <<(X+Y)/integer>> || X <- [1,2,3] && Y <- [2,2,2]>>,
    <<3,4,5>> = << <<(X+V1)/integer>> ||
                    X <- [1,2,3] &&
                        _K1 := V1 <- maps:iterator(#{a=>2, b=>2, c=>2}, ordered)>>,
    <<3,4,5>> = << <<(X+V1)/integer>> ||
                    <<X>> <= <<1,2,3>> &&
                        _K1 := V1 <- maps:iterator(#{a=>2, b=>2, c=>2}, ordered)>>,
    <<3,4,5>> = << <<(V1+V2)/integer>> ||
                    _K1 := V1 <- maps:iterator(#{a=>1, b=>2, c=>3}, ordered) &&
                        _K2 := V2 <- maps:iterator(#{a=>2, b=>2, c=>2}, ordered)>>,
    #{c := 3,b := 2,a := 1} = #{X => Y || X <- [a,b,c] && Y <- [1,2,3]},
    #{c := 3,b := 2,a := 1} = #{X => Y || X <- [a,b,c] && <<Y>> <= <<1,2,3>>},
    ok.

zmc(Config) when is_list(Config) ->
    [{a,b,1,3}] = [{K1, K2, V1, V2} || K1 := V1 <- #{a=>1} && K2 := V2 <- #{b=>3}],
    Seq = lists:seq(1, 50),
    M1 = maps:iterator(#{X=>X || X <- Seq}, ordered),
    M2 = maps:iterator(#{X=>X || X <- lists:seq(1,50)}, ordered),
    true = [A * 4 || A <- Seq] =:=
        [X+Y+Z+W || X := Y <- M1 && Z := W <- M2],
    true = << <<(A * 4):64>> || A <- Seq>> =:=
        << <<(X+Y+Z+W):64>> || X := Y <- M1 && Z := W <- M2>>,

    M3 = maps:iterator(#{X=>X*3 || X <- Seq}, ordered),
    M4 = maps:iterator(#{X*2=>X*4 || X <- Seq}, ordered),
    true = [{A, A*3, A*2, A*4} || A <- Seq] =:=
        [{X, Y, Z, W} || X := Y <- M3 && Z := W <- M4],
    true = [A * 3 || A <- Seq] =:= [X+Y+Z || X := Y <- M1 && Z <- Seq],
    true = << <<A:64, (A*3):64, (A*2):64, (A*4):64>> || A <- Seq>> =:=
        << <<X:64, Y:64, Z:64, W:64>> || X := Y <- M3 && Z := W <- M4>>,
    true = << <<(A*3):64>> || A <- Seq>> =:=
        << <<(X+Y+Z):64>> || X := Y <- M1 && Z <- Seq>>,

    M5 = maps:iterator(#{X =>
                             case X rem 2 of
                                 0 -> {ok,X};
                                 1 -> {error,X}
                             end || X <- Seq}, ordered),
    M6 = maps:iterator(#{X*2 => X*4 || X <- Seq}, ordered),
    [] = [X || {{X,{ok,X}}, {_,X}} <- lists:zip(maps:to_list(M5), maps:to_list(M6))],
    [] = [X || X := {ok,X} <- M5 && _ := X <- M6],
    [] = [X || X := {e,X} <- M5 && X := {ok,X} <- M5],

    ok.

filter_guard(Config) when is_list(Config) ->
    [[1,2,1]] = [X++Y || X <- [[1,2], [2,-3]] && Y <- [[1], [2]], lists:sum(X)>0],
    [{a,2}, {b,4}, {c,6}] = [{X,Y} || X <- [a,b,c] && <<Y>> <= <<2,4,6>>,
                                      Y rem 2 == 0],
    [{b,4}, {c,6}] = [{X, Y} || X <- [a,b,c] && <<Y>> <= <<2,4,6>>, Y =/= 2],
    [] = [{X,Y} || X <- [a, b, c] && <<Y>> <= <<2,4,6>>, Y rem 2 == 1],
    [{b,4}] = [{X,Y} || <<Y>> <= <<2,4,6>> && X <- [a,b,c], X>a, X<c],
    [{b,d,2}] = [{K1,K2,V1} || K1 := V1 <- maps:iterator(#{a=>1, b=>2}, ordered) &&
                                   K2 := V2 <- maps:iterator(#{c=>3, d=>4}, ordered),
                               V2 rem 2 == 0],
    <<5>> = << <<(X+Y)/integer>> || X <- [1,2,3] && Y <- [2,2,2], X rem 2 == 1, X+Y>4>>,
    #{c := 3,a := 1} = #{X => Y || X <- [a,b,c] && Y <- [1,2,3], Y rem 2 == 1},
    #{c := 3} = #{X => Y || X <- [a,b,c] && Y <- [1,2,3], Y rem 2 == 1, Y > 1},
    #{c := 3,a := 1} = #{X => Y || X <- [a,b,c] && <<Y>> <= <<1,2,3>>, Y rem 2 == 1}.

filter_pattern(Config) when is_list(Config) ->
    [] = do_filter_pat_1([], []),
    [] = do_filter_pat_1([a], [a]),
    [] = do_filter_pat_1([{ok,a}], [{error,e}]),
    [] = do_filter_pat_2([], []),
    [] = do_filter_pat_2([a], [b]),
    [] = do_filter_pat_2([{a,1}], [{b,1}]),
    [{1,7}] = do_filter_pat_2([{a,1}], [{a,7}]),
    [{1,7},{10,20}] = do_filter_pat_2([{a,1},{b,9},{x,10}],
                                      [{a,7},{wrong,8},{x,20}]),
    ok.
do_filter_pat_1(L1, L2) ->
    Res = [{A,B} || {ok,A} <- L1 && {ok,B} <- L2],
    Res = [{A,B} || {{ok,A},{ok,B}} <- lists:zip(L1,L2)],
    Res.
do_filter_pat_2(L1, L2) ->
    Res = [{A,B} || {Same,A} <- L1 && {Same,B} <- L2],
    Res = [{A,B} || {{Same,A},{Same,B}} <- lists:zip(L1,L2)],
    Res.

cartesian(Config) when is_list(Config) ->
    [{a,3}, {b,5}, {c,7}, {a,4}, {b,6}, {c,8}] =
        [{X, W+Y} || W <- [1,2],
                     X <- [a,b,c] && <<Y>> <= <<2,4,6>>],
    [{a,3}, {a,4}, {b,5}, {b,6}, {c,7}, {c,8}] =
        [{X, W+Y} || X <- [a,b,c] &&
                         <<Y>> <= <<2,4,6>>, W <- [1,2]],
    [{a,4}, {b,6}, {c,8}] =
        [{X, W+Y} || X <- [a,b,c] &&
                         <<Y>> <= <<2,4,6>>, W <- [1,2], (W + Y) rem 2 == 0],
    <<4,2,5,3,6,4>> = << <<(X+V1+Y)/integer>> ||
                          X <- [1,2,3] &&
                              _K1 := V1 <- maps:iterator(#{a=>2, b=>2, c=>2}, ordered),
                          <<Y>> <= <<1,-1>> >>,
    ok.

nomatch(Config) when is_list(Config) ->
    [] = do_nomatch_1([], []),
    [] = do_nomatch_1([1], [a]),
    [] = do_nomatch_1([1,2], [a,b]),
    {'EXIT',{{bad_generators,{[1,2,3],[]}},_}} = do_nomatch_1([1,2,3], []),
    {'EXIT',{{bad_generators,{[3],[]}},_}} = do_nomatch_1([1,2,3], [a,b]),

    <<>> = do_nomatch_2([], <<>>),
    <<>> = do_nomatch_2([a], <<1>>),
    {'EXIT',{{bad_generators,{[2],<<>>}},_}} = do_nomatch_2([1,2], <<3>>),
    ok.

do_nomatch_1(L1, L2) ->
    catch [{X, Y} || a=b=X <- L1 && Y <- L2].

do_nomatch_2(L, Bin) ->
    catch << <<(X+Y)/integer>> || a=b=X <- L && <<Y>> <= Bin >>.

bad_generators(Config) when is_list(Config) ->
    {'EXIT',{{bad_generators,{x,[1,2]}},_}} =
        catch [{X,Y} || X <- x && Y <- [1,2]],
    {'EXIT',{{bad_generators,{[],[4]}},_}} =
        catch [{X,Y} || X <- [1,2,3] && Y <- [1,2,3,4]],
    {'EXIT',{{bad_generators,{[3,4],[]}},_}} =
        catch [{X,Y} || X <- [1,2,3,4] && Y <- [1,2], X < 3],
    {'EXIT',{{bad_generators,{[3,4],[]}},_}} =
        catch << <<(X+Y)/integer>> || X <- [1,2,3,4] && Y <- [1,2], X < 3>>,
    {'EXIT',{{bad_generators,{<<>>,<<4>>}},_}} =
        catch << <<X:16>> || <<X:16>> <= <<1:8,2:8>> && <<X:8>> <= <<3:8,4:8>>>>,
    {'EXIT',{{bad_generators,{<<1,2>>,a}},_}} =
        catch << <<X:16>> || <<X:16>> <= <<1:8,2:8>> && <<X:8>> <= a>>,
    {'EXIT',{{bad_generator,a},_}} = catch [X || X := X <- a && _Y <- [1]],
    {'EXIT',{{bad_generators,{[d],[]}},_}} =
        catch #{X => Y || X <- [a,b,c,d] && Y <- [1,2,3], Y > 1},

    %% Make sure that line numbers point out the generator.

    {'EXIT',{{bad_generators,{[],[4]}},
             [{?MODULE,_,_,_}|_]}} =
        catch bad_generators([1,2,3],[1,2,3,4]),

    {'EXIT',{{bad_generators,{a,[2,3]}},
             [{?MODULE,_,_,_}|_]}} =
        catch bad_generators_bc(a,[2,3]),

    {'EXIT',{{bad_generators,{[2],[]}},
             [{?MODULE,_,_,_}|_]}} =
        catch bad_generators_mc([1,2],[1]),

    %% List comprehensions with improper lists.
    {'EXIT',{{bad_generators,{d,[d]}},
             [{?MODULE,_,_,_}|_]}} =
        catch bad_generators([a,b,c|d],[a,b,c,d]),

    ok.

-file("bad_zlc.erl", 1).
bad_generators(L1,L2) ->                        %Line 2
    [{I1, I2} ||                                %Line 3
        I1 <- L1 && I2 <- L2].                  %Line 4
bad_generators_bc(L1,L2) ->                     %Line 5
    << <<I1:4,I2:4>> ||                         %Line 6
        I1 <- L1 && I2 <- L2>>.                 %Line 7
bad_generators_mc(L1,L2) ->                     %Line 8
    #{I1 => I2 ||                               %Line 9
        I1 <- L1 && I2 <- L2}.                  %Line 10
