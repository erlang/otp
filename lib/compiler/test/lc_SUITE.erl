%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(lc_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 basic/1,deeply_nested/1,no_generator/1,
	 empty_generator/1,no_export/1,shadow/1,
	 effect/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [basic,
       deeply_nested,
       no_generator,
       empty_generator,
       no_export,
       shadow,
       effect
      ]}].

init_per_suite(Config) ->
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
    L0 = lists:seq(1, 10),
    L1 = my_map(fun(X) -> {x,X} end, L0),
    L1 = [{x,X} || X <- L0],
    L0 = my_map(fun({x,X}) -> X end, L1),
    [1,2,3,4,5] = [X || X <- L0, X < 6],
    [4,5,6] = [X || X <- L0, X > 3, X < 7],
    [] = [X || X <- L0, X > 32, X < 7],
    [1,3,5,7,9] = [X || X <- L0, odd(X)],
    [2,4,6,8,10] = [X || X <- L0, not odd(X)],
    [1,3,5,9] = [X || X <- L0, odd(X), X =/= 7],
    [2,4,8,10] = [X || X <- L0, not odd(X), X =/= 6],

    %% Append is specially handled.
    [1,3,5,9,2,4,8,10] = [X || X <- L0, odd(X), X =/= 7] ++
	[X || X <- L0, not odd(X), X =/= 6],

    %% Guards BIFs are evaluated in guard context. Weird, but true.
    [{a,b,true},{x,y,true,true}] = [X || X <- tuple_list(), element(3, X)],

    %% Filter expressions with andalso/orelse.
    "abc123" = alphanum("?abc123.;"),

    %% Aliased patterns.
    [] = [t || {C=D}={_,_} <- []],
    [] = [X || {X,{Y}={X,X}} <- []],
    [t] = [t || "a"++"b" = "ab" <- ["ab"]],

    %% Strange filter block.
    [] = [{X,Y} || {X} <- [], begin Y = X, Y =:= X end],
    [{a,a}] = [{X,Y} || {X} <- [{a}], begin Y = X, Y =:= X end],

    %% Not matching.
    [] = [3 || {3=4} <- []],

    %% Error cases.
    [] = [{xx,X} || X <- L0, element(2, X) == no_no_no],
    {'EXIT',_} = (catch [X || X <- L1, list_to_atom(X) == dum]),
    [] = [X || X <- L1, X+1 < 2],
    {'EXIT',_} = (catch [X || X <- L1, odd(X)]),
    fc([x], catch [E || E <- id(x)]),
    ok.

tuple_list() ->
    [{a,b,true},[a,b,c],glurf,{a,b,false,xx},{a,b},{x,y,true,true},{a,b,d,ddd}].

my_map(F, L) ->
    [F(X) || X <- L].

odd(X) ->
    X rem 2 == 1.

alphanum(Str) ->
    [C || C <- Str, ((C >= $0) andalso (C =< $9))
	      orelse ((C >= $a) andalso (C =< $z))
	      orelse ((C >= $A) andalso (C =< $Z))].

deeply_nested(Config) when is_list(Config) ->
    [[99,98,97,96,42,17,1764,12,11,10,9,8,7,6,5,4,3,7,2,1]] =  deeply_nested_1(),
    ok.

deeply_nested_1() ->
    %% This used to compile really, really SLOW before R11B-1...
    [[X1,X2,X3,X4,X5,X6,X7(),X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18(),X19,X20] ||
        X1 <- [99],X2 <- [98],X3 <- [97],X4 <- [96],X5 <- [42],X6 <- [17],
	X7 <- [fun() -> X5*X5 end],X8 <- [12],X9 <- [11],X10 <- [10],
        X11 <- [9],X12 <- [8],X13 <- [7],X14 <- [6],X15 <- [5],
	X16 <- [4],X17 <- [3],X18 <- [fun() -> X16+X17 end],X19 <- [2],X20 <- [1]].

no_generator(Config) when is_list(Config) ->
    Seq = lists:seq(-10, 17),
    [no_gen_verify(no_gen(A, B), A, B) || A <- Seq, B <- Seq],

    %% Literal expression, for coverage.
    [a] = [a || true],
    [a,b,c] = [a || true] ++ [b,c],
    ok.

no_gen(A, B) ->    
    [{A,B} || A+B =:= 0] ++
	[{A,B} || A*B =:= 0] ++
	[{A,B} || A rem B =:= 3] ++
	[{A,B} || A =:= B] ++
	[{one_more,A,B} || no_gen_one_more(A, B)] ++
	[A || A =:= 1] ++
	[A || A =:= 2] ++
	[A || A =:= 3] ++
	[A || A =:= 4] ++
	[A || A =:= 5] ++
	[A || A =:= 6] ++
	[A || A =:= 7] ++
	[A || A =:= 8] ++
	[A || A =:= 9] ++
	[B || B =:= 1] ++
	[B || B =:= 2] ++
	[B || B =:= 3] ++
	[B || B =:= 4] ++
	[B || B =:= 5] ++
	[B || B =:= 6] ++
	[B || B =:= 7] ++
	[B || B =:= 8] ++
	[B || B =:= 9].

no_gen_verify(Res, A, B) ->
    Pair = {A,B},
    ShouldBe = no_gen_eval(fun() -> A+B =:= 0 end, Pair) ++
	no_gen_eval(fun() -> A*B =:= 0 end, Pair) ++
	no_gen_eval(fun() -> B =/= 0 andalso A rem B =:= 3 end, Pair) ++
	no_gen_eval(fun() -> A =:= B end, Pair) ++
	no_gen_eval(fun() -> A + 1 =:= B end, {one_more,A,B}) ++
	no_gen_eval(fun() -> 1 =< A andalso A =< 9 end, A) ++
	no_gen_eval(fun() -> 1 =< B andalso B =< 9 end, B),
    case Res of
	ShouldBe -> ok;
	_ ->
	    io:format("A = ~p; B = ~p; Expected = ~p, actual = ~p", [A,B,ShouldBe,Res]),
	    ct:fail(failed)
    end.

no_gen_eval(Fun, Res) ->
    case Fun() of
	true -> [Res];
	false -> []
    end.

no_gen_one_more(A, B) -> A + 1 =:= B.

empty_generator(Config) when is_list(Config) ->
    [] = [X || {X} <- [], (false or (X/0 > 3))],
    ok.

no_export(Config) when is_list(Config) ->
    [] = [ _X = a || false ] ++ [ _X = a || false ],
    ok.

%% Test that variables in list comprehensions are
%% correctly shadowed.

shadow(Config) when is_list(Config) ->
    Shadowed = nomatch,
    _ = id(Shadowed),				%Eliminate warning.
    L = [{Shadowed,Shadowed+1} || Shadowed <- lists:seq(7, 9)],
    [{7,8},{8,9},{9,10}] = id(L),
    [8,9] = id([Shadowed || {_,Shadowed} <- id(L),
			    Shadowed < 10]),
    ok.

effect(Config) when is_list(Config) ->
    [{42,{a,b,c}}] =
	do_effect(fun(F, L) ->
			  [F({V1,V2}) ||
			      #{<<1:500>>:=V1,<<2:301>>:=V2} <- L],
			  ok
		  end, id([#{},x,#{<<1:500>>=>42,<<2:301>>=>{a,b,c}}])),

    %% Will trigger the time-trap timeout if not tail-recursive.
    case ?MODULE of
	lc_SUITE ->
	    _ = [{'EXIT',{badarg,_}} =
		     (catch binary_to_atom(<<C/utf8>>, utf8)) ||
		    C <- lists:seq(16#10000, 16#FFFFF)];
	_ ->
	    ok
    end,

    ok.

do_effect(Lc, L) ->
    put(?MODULE, []),
    F = fun(V) -> put(?MODULE, [V|get(?MODULE)]) end,
    ok = Lc(F, L),
    lists:reverse(erase(?MODULE)).

id(I) -> I.
    
fc(Args, {'EXIT',{function_clause,[{?MODULE,_,Args,_}|_]}}) -> ok;
fc(Args, {'EXIT',{function_clause,[{?MODULE,_,Arity,_}|_]}})
  when length(Args) =:= Arity ->
    true = test_server:is_native(?MODULE);
fc(Args, {'EXIT',{{case_clause,ActualArgs},_}})
  when ?MODULE =:= lc_inline_SUITE ->
    Args = tuple_to_list(ActualArgs).
