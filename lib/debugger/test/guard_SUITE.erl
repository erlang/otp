%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

%%
-module(guard_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 bad_arith/1,bad_tuple/1,test_heap_guards/1,guard_bifs/1,
	 type_tests/1,const_guard/1,
	 const_cond/1,basic_not/1,complex_not/1,
	 semicolon/1,complex_semicolon/1,comma/1,
	 or_guard/1,more_or_guards/1,
	 complex_or_guards/1,and_guard/1,
	 xor_guard/1,more_xor_guards/1,
	 old_guard_tests/1,
	 build_in_guard/1,gbif/1,
	 t_is_boolean/1,is_function_2/1,
	 tricky/1,rel_ops/1,
	 basic_andalso_orelse/1,traverse_dcd/1,
	 check_qlc_hrl/1,andalso_semi/1,t_tuple_size/1,binary_part/1,
	 bad_constants/1]).

-include_lib("common_test/include/ct.hrl").

-export([init/4]).
-import(lists, [member/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [bad_arith, bad_tuple, test_heap_guards, guard_bifs,
     type_tests, const_guard, const_cond, basic_not,
     complex_not, semicolon, complex_semicolon, comma,
     or_guard, more_or_guards, complex_or_guards, and_guard,
     xor_guard, more_xor_guards, build_in_guard,
     old_guard_tests, gbif, t_is_boolean, is_function_2,
     tricky, rel_ops, basic_andalso_orelse, traverse_dcd,
     check_qlc_hrl, andalso_semi, t_tuple_size, binary_part,
     bad_constants].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

%% Test that a bad arithmetic operation in a guard works correctly.
bad_arith(Config) when list(Config) ->
    5 = bad_arith1(2, 3),
    10 = bad_arith1(1, infinity),
    10 = bad_arith1(infinity, 1),
    42 = bad_div(24, 0),
    ok.

bad_arith1(T1, T2) when T1+T2 < 10 ->
    T1+T2;
bad_arith1(_, _) ->
    10.

bad_div(A, B) when A/B > 0 ->
    A/B;
bad_div(A, B) when A div B > 0 ->
    A div B;
bad_div(_A, _B) ->
    42.

%% Test that bad arguments to element/2 are handled correctly.
bad_tuple(Config) when list(Config) ->
    error = bad_tuple1(a),
    error = bad_tuple1({a, b}),
    x = bad_tuple1({x, b}),
    y = bad_tuple1({a, b, y}),
    ok.

bad_tuple1(T) when element(1, T) == x -> x;
bad_tuple1(T) when element(3, T) == y -> y;
bad_tuple1(_) -> error.

%% .
test_heap_guards(Config) when list(Config) ->
    process_flag(trap_exit, true),
    Tuple = {a, tuple, is, built, here, xxx},
    List = [a, list, is, built, here],

    try_fun(fun a_case/1, [Tuple], [Tuple]),
    try_fun(fun a_case/1, [List], [List, List]),
    try_fun(fun a_case/1, [a], [a]),

    try_fun(fun an_if/1, [Tuple], [Tuple]),
    try_fun(fun an_if/1, [List], [List, List]),
    try_fun(fun an_if/1, [a], [a]),

    try_fun(fun receive_test/1, [Tuple], [Tuple]),
    try_fun(fun receive_test/1, [List], [List, List]),
    try_fun(fun receive_test/1, [a], [a]),
    ok.

a_case(V) ->
    case V of
	T when T == {a, tuple, is, built, here, xxx} ->
	    [T];
	L when L == [a, list, is, built, here] ->
	    [L, L];
	a ->
	    [a]
    end.

an_if(V) ->
    if
	V == {a, tuple, is, built, here, xxx} ->
	    [V];
	V == [a, list, is, built, here] ->
	    [V, V];
	V == a ->
	    [a]
    end.

receive_test(V) ->
    self() ! V,
    a_receive().

a_receive() ->
    receive
	T when T == {a, tuple, is, built, here, xxx} ->
	    [T];
	L when L == [a, list, is, built, here] ->
	    [L, L];
	a ->
	    [a]
    end.

try_fun(Fun, Args, Result) ->
    try_fun(16, Fun, Args, Result, []).

try_fun(0, _, _, _, _) ->
    ok;
try_fun(Iter, Fun, Args, Result, Filler) ->
    Pid = spawn_link(?MODULE, init, [self(),Fun,Args,list_to_tuple(Filler)]),
    receive
	{'EXIT',Pid,{result,Result}} ->
	    try_fun(Iter-1, Fun, Args, Result, [0|Filler]);
	{'EXIT',Pid,{result,Other}} ->
	    io:format("Expected ~p; got ~p~n", [Result,Other]),
	    ct:fail(failed);
	Other ->
	    ct:fail({unexpected_message,Other})
    end.

init(_ReplyTo, Fun, Args, Filler) ->
    Result = {result, apply(Fun, Args)},
    dummy(Filler),
    io:format("~p: result = ~p\n", [?LINE,Result]),
    exit(Result).

dummy(_) ->
    ok.

%% Test all guard bifs with nasty (but legal arguments).
guard_bifs(Config) when list(Config) ->
    Big = -237849247829874297658726487367328971246284736473821617265433,
    Float = 387924.874,

    %% Succeding use of guard bifs.

    try_gbif('abs/1', Big, -Big),
    try_gbif('float/1', Big, float(Big)),
    try_gbif('trunc/1', Float, 387924.0),
    try_gbif('round/1', Float, 387925.0),
    try_gbif('length/1', [], 0),

    try_gbif('length/1', [a], 1),
    try_gbif('length/1', [a, b], 2),
    try_gbif('length/1', lists:seq(0, 31), 32),

    try_gbif('hd/1', [a], a),
    try_gbif('hd/1', [a, b], a),

    try_gbif('tl/1', [a], []),
    try_gbif('tl/1', [a, b], [b]),
    try_gbif('tl/1', [a, b, c], [b, c]),

    try_gbif('size/1', {}, 0),
    try_gbif('size/1', {a}, 1),
    try_gbif('size/1', {a, b}, 2),
    try_gbif('size/1', {a, b, c}, 3),
    try_gbif('size/1', list_to_binary([]), 0),
    try_gbif('size/1', list_to_binary([1]), 1),
    try_gbif('size/1', list_to_binary([1, 2]), 2),
    try_gbif('size/1', list_to_binary([1, 2, 3]), 3),

    try_gbif('element/2', {x}, {1, x}),
    try_gbif('element/2', {x, y}, {1, x}),
    try_gbif('element/2', {x, y}, {2, y}),

    try_gbif('self/0', 0, self()),
    try_gbif('node/0', 0, node()),
    try_gbif('node/1', self(), node()),

    %% Failing use of guard bifs.

    try_fail_gbif('abs/1', Big, 1),
    try_fail_gbif('abs/1', [], 1),

    try_fail_gbif('float/1', Big, 42),
    try_fail_gbif('float/1', [], 42),

    try_fail_gbif('trunc/1', Float, 0.0),
    try_fail_gbif('trunc/1', [], 0.0),

    try_fail_gbif('round/1', Float, 1.0),
    try_fail_gbif('round/1', [], a),

    try_fail_gbif('length/1', [], 1),
    try_fail_gbif('length/1', [a], 0),
    try_fail_gbif('length/1', a, 0),
    try_fail_gbif('length/1', {a}, 0),

    try_fail_gbif('hd/1', [], 0),
    try_fail_gbif('hd/1', [a], x),
    try_fail_gbif('hd/1', x, x),

    try_fail_gbif('tl/1', [], 0),
    try_fail_gbif('tl/1', [a], x),
    try_fail_gbif('tl/1', x, x),

    try_fail_gbif('size/1', {}, 1),
    try_fail_gbif('size/1', [], 0),
    try_fail_gbif('size/1', [a], 1),

    try_fail_gbif('element/2', {}, {1, x}),
    try_fail_gbif('element/2', {x}, {1, y}),
    try_fail_gbif('element/2', [], {1, z}),

    try_fail_gbif('self/0', 0, list_to_pid("<0.0.0>")),
    try_fail_gbif('node/0', 0, xxxx),
    try_fail_gbif('node/1', self(), xxx),
    try_fail_gbif('node/1', yyy, xxx),
    ok.

try_gbif(Id, X, Y) ->
    case guard_bif(Id, X, Y) of
	{Id, X, Y} ->
	    io:format("guard_bif(~p, ~p, ~p) -- ok", [Id, X, Y]);
	Other ->
	    ok = io:format("guard_bif(~p, ~p, ~p) -- bad result: ~p\n",
			   [Id, X, Y, Other]),
	    ct:fail(failed)
    end.

try_fail_gbif(Id, X, Y) ->
    case catch guard_bif(Id, X, Y) of
	{'EXIT', {function_clause,[{?MODULE,guard_bif,[Id,X,Y],_}|_]}} ->
	    io:format("guard_bif(~p, ~p, ~p) -- ok", [Id,X,Y]);
	Other ->
	    ok = io:format("guard_bif(~p, ~p, ~p) -- bad result: ~p\n",
			   [Id, X, Y, Other]),
	    ct:fail(failed)
    end.

guard_bif('abs/1', X, Y) when abs(X) == Y ->
    {'abs/1', X, Y};
guard_bif('float/1', X, Y) when float(X) == Y ->
    {'float/1', X, Y};
guard_bif('trunc/1', X, Y) when trunc(X) == Y ->
    {'trunc/1', X, Y};
guard_bif('round/1', X, Y) when round(X) == Y ->
    {'round/1', X, Y};
guard_bif('length/1', X, Y) when length(X) == Y ->
    {'length/1', X, Y};
guard_bif('hd/1', X, Y) when hd(X) == Y ->
    {'hd/1', X, Y};
guard_bif('tl/1', X, Y) when tl(X) == Y ->
    {'tl/1', X, Y};
guard_bif('size/1', X, Y) when size(X) == Y ->
    {'size/1', X, Y};
guard_bif('element/2', X, {Pos, Expected}) when element(Pos, X) == Expected ->
    {'element/2', X, {Pos, Expected}};
guard_bif('self/0', X, Y) when self() == Y ->
    {'self/0', X, Y};
guard_bif('node/0', X, Y) when node() == Y ->
    {'node/0', X, Y};
guard_bif('node/1', X, Y) when node(X) == Y ->
    {'node/1', X, Y}.

%% Test the type tests.
type_tests(Config) when list(Config) ->
    Types = all_types(),
    Tests = type_test_desc(),
    put(errors, 0),
    put(violations, 0),
    type_tests(Tests, Types),
    case {get(errors), get(violations)} of
	{0, 0} ->
	    ok;
	{0, N} ->
	    {comment, integer_to_list(N) ++ " standard violation(s)"};
	{Errors, Violations} ->
	    io:format("~p sub test(s) failed, ~p violation(s)",
		      [Errors, Violations]),
	    ct:fail(failed)
    end.

type_tests([{Test, AllowedTypes}| T], AllTypes) ->
    type_tests(Test, AllTypes, AllowedTypes),
    type_tests(T, AllTypes);
type_tests([], _) ->
    ok.

type_tests(Test, [Type|T], Allowed) ->
    {TypeTag, Value} = Type,
    case member(TypeTag, Allowed) of
	true ->
	    case catch type_test(Test, Value) of
		Test ->
		    ok;
		_Other ->
		    io:format("Test ~p(~p) failed", [Test, Value]),
		    put(errors, get(errors) + 1)
	    end;
	false ->
	    case catch type_test(Test, Value) of
		{'EXIT',{function_clause,
			 [{?MODULE,type_test,[Test,Value],_}|_]}} ->
		    ok;
		{'EXIT',Other} ->
		    ct:fail({unexpected_error_reason,Other});
		tuple when function(Value) ->
		    io:format("Standard violation: Test ~p(~p) should fail",
			      [Test, Value]),
		    put(violations, get(violations) + 1);
		_Other ->
		    io:format("Test ~p(~p) succeeded (should fail)", [Test, Value]),
		    put(errors, get(errors) + 1)
	    end
    end,
    type_tests(Test, T, Allowed);
type_tests(_, [], _) ->
    ok.

all_types() ->
    [{small, 42},
     {big, 392742928742947293873938792874019287447829874290742},
     {float, 3.14156},
     {nil, []},
     {cons, [a]},
     {tuple, {a, b}},
     {atom, xxxx},
     {ref, make_ref()},
     {pid, self()},
     {port, make_port()},
     {function, fun(X) -> X+1, "" end},
     {binary, list_to_binary([])}].

type_test_desc() ->
    [{integer, [small, big]},
     {float, [float]},
     {number, [small, big, float]},
     {atom, [atom]},
     {list, [cons, nil]},
     {nonempty_list, [cons]},
     {nil, [nil]},
     {tuple, [tuple]},
     {pid, [pid]},
     {port, [port]},
     {reference, [ref]},
     {function, [function]}].

type_test(integer, X) when integer(X) ->
    integer;
type_test(float, X) when float(X) ->
    float;
type_test(number, X) when number(X) ->
    number;
type_test(atom, X) when atom(X) ->
    atom;
type_test(list, X) when list(X) ->
    list;
type_test(nonempty_list, [_]) ->
    nonempty_list;
type_test(nil, []) ->
    nil;
type_test(tuple, X) when tuple(X) ->
    tuple;
type_test(pid, X) when pid(X) ->
    pid;
type_test(reference, X) when reference(X) ->
    reference;
type_test(port, X) when port(X) ->
    port;
type_test(binary, X) when binary(X) ->
    binary;
type_test(function, X) when function(X) ->
    function.

make_port() ->
    hd(erlang:ports()).

const_guard(Config) when is_list(Config) ->
    if
	(0 == 0) and ((0 == 0) or (0 == 0)) ->
	    ok
    end.


const_cond(Config) when is_list(Config) ->
    ok = const_cond({}, 0),
    ok = const_cond({a}, 1),
    error = const_cond({a,b}, 3),
    error = const_cond({a}, 0),
    error = const_cond({a,b}, 1),
    ok.

const_cond(T, Sz) ->
    case T of
	_X when false -> never;
	_X when tuple(T), eq == eq, size(T) == Sz -> ok;
	_X when tuple(T), eq == leq, size(T) =< Sz -> ok;
	_X -> error
    end.

basic_not(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    Glurf = id(glurf),
    A = id(5),
    B = id(37.5),
    C = id(-1),
    D = id(5),
    ATuple = {False,True,Glurf},

    check(fun() -> if not false -> ok; true -> error end end, ok),
    check(fun() -> if not true -> ok; true -> error end end, error),
    check(fun() -> if not False -> ok; true -> error end end, ok),
    check(fun() -> if not True -> ok; true -> error end end, error),

    check(fun() -> if A > B -> gt; A < B -> lt; A == B -> eq end end, lt),
    check(fun() -> if A > C -> gt; A < C -> lt; A == C -> eq end end, gt),
    check(fun() -> if A > D -> gt; A < D -> lt; A == D -> eq end end, eq),

    check(fun() -> if not (7 > 453) -> le; not (7 < 453) -> ge;
		      not (7 == 453) -> ne; true -> eq end end, le),
    check(fun() -> if not (7 > -8) -> le; not (7 < -8) -> ge;
		      not (7 == -8) -> ne; true -> eq end end, ge),
    check(fun() -> if not (7 > 7) -> le; not (7 < 7) -> ge;
		      not (7 == 7) -> ne; true -> eq end end, le),

    check(fun() -> if not (A > B) -> le; not (A < B) -> ge;
		      not (A == B) -> ne; true -> eq end end, le),
    check(fun() -> if not (A > C) -> le; not (A < C) -> ge;
		      not (A == C) -> ne; true -> eq end end, ge),
    check(fun() -> if not (A > D) -> le; not (A < D) -> ge;
		      not (A == D) -> ne; true -> eq end end, le),

    check(fun() -> if not element(1, ATuple) -> ok; true -> error end end, ok),
    check(fun() -> if not element(2, ATuple) -> ok; true -> error end end, error),
    check(fun() -> if not element(3, ATuple) -> ok; true -> error end end, error),

    check(fun() -> if not glurf -> ok; true -> error end end, error),
    check(fun() -> if not Glurf -> ok; true -> error end end, error),

    ok.

complex_not(Config) when is_list(Config) ->
    ATuple = id({false,true,gurka}),
    check(fun() -> if not(element(1, ATuple)) -> ok; true -> error end end, ok),
    check(fun() -> if not(element(2, ATuple)) -> ok; true -> error end end, error),

    check(fun() -> if not(element(3, ATuple) == gurka) -> ok;
		      true -> error end end, error),
    check(fun() -> if not(element(3, ATuple) =/= gurka) -> ok;
		      true -> error end end, ok),

    check(fun() -> if {a,not(element(2, ATuple))} == {a,false} -> ok;
		      true -> error end end, ok),
    check(fun() -> if {a,not(element(1, ATuple))} == {a,false} -> ok;
		      true -> error end end, error),

    check(fun() -> if not(element(1, ATuple) or element(3, ATuple)) -> ok;
		      true -> error end end, error),

    %% orelse
    check(fun() -> if not(element(1, ATuple) orelse element(3, ATuple)) -> ok;
		      true -> error end end, error),

    ok.

semicolon(Config) when is_list(Config) ->

    %% True/false combined using ';' (literal atoms).

    check(fun() -> if true; false -> ok end end, ok),
    check(fun() -> if false; true -> ok end end, ok),
    check(fun() -> if true; true -> ok end end, ok),
    check(fun() -> if false; false -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if false; false -> ok end),
		  exit
	  end, exit),

    %% True/false combined used ';'.

    True = id(true),
    False = id(false),

    check(fun() -> if True; False -> ok end end, ok),
    check(fun() -> if False; True -> ok end end, ok),
    check(fun() -> if True; True -> ok end end, ok),
    check(fun() -> if False; False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if False; False -> ok end),
		  exit
	  end, exit),

    %% Combine true/false with a non-boolean value.
    Glurf = id(glurf),


    check(fun() -> if True; Glurf -> ok end end, ok),
    check(fun() -> if Glurf; True -> ok end end, ok),
    check(fun() -> if Glurf; Glurf -> ok; true -> error end end, error),
    check(fun() -> if False; Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf; False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if Glurf; Glurf -> ok end),
		  exit
	  end, exit),

    %% Combine true/false with errors.

    ATuple = id({false,true,gurka}),

    check(fun() -> if True; element(42, ATuple) -> ok end end, ok),
    check(fun() -> if element(42, ATuple); True -> ok end end, ok),
    check(fun() -> if element(42, ATuple); element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if False; element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if element(42, ATuple);
		      False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if element(42, ATuple);
				element(42, ATuple) -> ok end),
		  exit
	  end, exit),

    ok.

complex_semicolon(Config) when is_list(Config) ->
    ok = csemi1(int, {blurf}),
    ok = csemi1(string, {blurf}),
    ok = csemi1(float, [a]),
    error = csemi1(35, 42),

    %% 2
    ok = csemi2({}, {a,b,c}),
    ok = csemi2({1,3.5}, {a,b,c}),
    ok = csemi2(dum, {a,b,c}),

    ok = csemi2({45,-19.3}, {}),
    ok = csemi2({45,-19.3}, {dum}),
    ok = csemi2({45,-19.3}, {dum,dum}),

    error = csemi2({45}, {dum}),
    error = csemi2([], {dum}),
    error = csemi2({dum}, []),
    error = csemi2([], []),

    %% 3
    csemi3(fun csemi3a/4),
    csemi3(fun csemi3b/4),
    csemi3(fun csemi3c/4),

    %% 4
    csemi4(fun csemi4a/4),
    csemi4(fun csemi4b/4),
    csemi4(fun csemi4c/4),
    csemi4(fun csemi4d/4),

    %% 4, 'orelse' instead of 'or'
    csemi4_orelse(fun csemi4_orelse_a/4),
    csemi4_orelse(fun csemi4_orelse_b/4),
    csemi4_orelse(fun csemi4_orelse_c/4),
    csemi4_orelse(fun csemi4_orelse_d/4),

    ok.

csemi1(Type, Val) when is_list(Val), Type == float;
		       Type == int; Type == string -> ok;
csemi1(_, _) -> error.

csemi2(A, B) when size(A) > 1; size(B) > 2 -> ok;
csemi2(_, _) -> error.

csemi3(Csemi3) ->
    ok = Csemi3({}, {a,b,c}, [0], [0]),
    ok = Csemi3({1,3.5}, {a,b,c}, -1, -1),
    ok = Csemi3(dum, {a,b,c}, 0.0, 0.0),
    ok = Csemi3(dum, {c}, b, a),
    ok = Csemi3(dum, <<1,2,3>>, 0.0, 0.0),
    ok = Csemi3(<<3.5/float>>, {a,b,c}, -1, -1),

    ok = Csemi3({45,-19.3}, {}, [], []),
    ok = Csemi3({45,-19.3}, {dum}, 42, 42),
    ok = Csemi3({45,-19.3}, {dum,dum}, 33, 33),

    ok = Csemi3({45}, {dum}, 1.0, 0),
    ok = Csemi3([a], {dum}, 1.0, 0),
    ok = Csemi3({dum}, [], 1.0, 0),
    ok = Csemi3([], [], 1.0, 0),
    ok = Csemi3(blurf, {dum}, 1.0, 0),
    ok = Csemi3({a}, blurf, 1.0, 0),
    ok = Csemi3([a], [dum], 1.0, 0),
    ok = Csemi3({dum}, [], 1.0, 0),
    ok = Csemi3([], [], 1.0, 0),

    error = Csemi3({45}, {dum}, 0, 0),
    error = Csemi3([a], {dum}, 0, 0),
    error = Csemi3({dum}, [], 0, 0),
    error = Csemi3([], [], 0, 0),
    ok.

csemi3a(A, B, X, Y) when X > Y; size(A) > 1; size(B) > 2 -> ok;
csemi3a(_, _, _, _) -> error.

csemi3b(A, B, X, Y) when size(A) > 1; X > Y; size(B) > 2 -> ok;
csemi3b(_, _, _, _) -> error.

csemi3c(A, B, X, Y) when size(A) > 1; size(B) > 2; X > Y -> ok;
csemi3c(_, _, _, _) -> error.


csemi4(Test) ->
    ok = Test({a,b}, 2, {c,d}, 2),
    ok = Test({1,2,3}, 0, [], 0),
    ok = Test({}, 2, blurf, 0),
    ok = Test({}, 2, {1}, 2),

    error = Test([], 4, {}, 0),
    error = Test({}, 0, [a,b], 4),
    error = Test({}, 0, [a,b], 0),
    error = Test([], 0, {}, 0),
    error = Test({}, 0, {}, 0),

    ok.

csemi4a(A, X, B, Y) when (size(A) > 1) or (X > 1);
			 (size(B) > 1) or (Y > 1) -> ok;
csemi4a(_, _, _, _) -> error.

csemi4b(A, X, B, Y) when (X > 1) or (size(A) > 1);
			 (size(B) > 1) or (Y > 1) -> ok;
csemi4b(_, _, _, _) -> error.

csemi4c(A, X, B, Y) when (size(A) > 1) or (X > 1);
			 (Y > 1) or (size(B) > 1) -> ok;
csemi4c(_, _, _, _) -> error.

csemi4d(A, X, B, Y) when (X > 1) or (size(A) > 1);
			 (Y > 1) or (size(B) > 1) -> ok;
csemi4d(_, _, _, _) -> error.


csemi4_orelse(Test) ->
    ok = Test({a,b}, 2, {c,d}, 2),
    ok = Test({1,2,3}, 0, [], 0),
    ok = Test({}, 2, blurf, 0),
    ok = Test({}, 2, {1}, 2),

    error = Test([], 1, {}, 0),

    ok.

csemi4_orelse_a(A, X, B, Y) when (size(A) > 1) orelse (X > 1);
				 (size(B) > 1) orelse (Y > 1) -> ok;
csemi4_orelse_a(_, _, _, _) -> error.

csemi4_orelse_b(A, X, B, Y) when (X > 1) orelse (size(A) > 1);
				 (size(B) > 1) orelse (Y > 1) -> ok;
csemi4_orelse_b(_, _, _, _) -> error.

csemi4_orelse_c(A, X, B, Y) when (size(A) > 1) orelse (X > 1);
				 (Y > 1) orelse (size(B) > 1) -> ok;
csemi4_orelse_c(_, _, _, _) -> error.

csemi4_orelse_d(A, X, B, Y) when (X > 1) or (size(A) > 1);
				 (Y > 1) or (size(B) > 1) -> ok;
csemi4_orelse_d(_, _, _, _) -> error.


comma(Config) when is_list(Config) ->

    %% ',' combinations of literal true/false.

    check(fun() -> if true, false -> ok; true -> error end end, error),
    check(fun() -> if false, true -> ok; true -> error end end, error),
    check(fun() -> if true, true -> ok end end, ok),
    check(fun() -> if false, false -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if true, false -> ok;
				false, true -> ok;
				false, false -> ok
			     end),
		  exit
	  end, exit),

    %% ',' combinations of true/false in variables.

    True = id(true),
    False = id(false),

    check(fun() -> if True, False -> ok; true -> error end end, error),
    check(fun() -> if False, True -> ok; true -> error end end, error),
    check(fun() -> if True, True -> ok end end, ok),
    check(fun() -> if False, False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True, False -> ok;
				False, True -> ok;
				False, False -> ok
			     end),
		  exit
	  end, exit),

    %% ',' combinations of true/false, and non-boolean in variables.

    Glurf = id(glurf),

    check(fun() -> if True, Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf, True -> ok; true -> error end end, error),
    check(fun() -> if True, True -> ok end end, ok),
    check(fun() -> if Glurf, Glurf -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True, Glurf -> ok;
				Glurf, True -> ok;
				Glurf, Glurf -> ok
			     end),
		  exit
	  end, exit),

    %% ',' combinations of true/false with errors.
    ATuple = id({a,b,c}),

    check(fun() -> if True, element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if element(42, ATuple), True -> ok;
		      true -> error end end, error),
    check(fun() -> if True, True -> ok end end, ok),
    check(fun() -> if element(42, ATuple), element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True, element(42, ATuple) -> ok;
				element(42, ATuple), True -> ok;
				element(42, ATuple), element(42, ATuple) -> ok
			     end),
		  exit
	  end, exit),

    ok.

or_guard(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    Glurf = id(glurf),

    %% 'or' combinations of literal true/false.
    check(fun() -> if true or false -> ok end end, ok),
    check(fun() -> if false or true -> ok end end, ok),
    check(fun() -> if true or true -> ok end end, ok),
    check(fun() -> if false or false -> ok; true -> error end end, error),

    check(fun() -> if glurf or true -> ok; true -> error end end, error),
    check(fun() -> if true or glurf -> ok; true -> error end end, error),
    check(fun() -> if glurf or glurf -> ok; true -> error end end, error),

    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if false or false -> ok end),
		  exit
	  end, exit),


    %% 'or' combinations using variables containing true/false.
    check(fun() -> if True or False -> ok end end, ok),
    check(fun() -> if False or True -> ok end end, ok),
    check(fun() -> if True or True -> ok end end, ok),
    check(fun() -> if False or False -> ok; true -> error end end, error),

    check(fun() -> if True or Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf or True -> ok; true -> error end end, error),
    check(fun() -> if Glurf or Glurf -> ok; true -> error end end, error),

    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if False or False -> ok end),
		  exit
	  end, exit),

    ok.

more_or_guards(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    ATuple = id({false,true,gurka}),

    check(fun() ->
		  if element(42, ATuple) or False -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if False or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(18, ATuple) or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if True or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(42, ATuple) or True -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(1, ATuple) or element(42, ATuple) or True -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(1, ATuple) or True or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if
		      (<<False:8>> == <<0>>) or element(2, ATuple) -> ok;
		      true -> error end
	  end, error),

    check(fun() ->
		  if
		      element(2, ATuple) or (<<True:8>> == <<1>>) -> ok;
		      true -> error end
	  end, error),

    check(fun() ->
		  if element(2, ATuple) or element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if
		      element(1, ATuple) or
		      element(2, ATuple) or
		      element(19, ATuple) -> ok;
		      true -> error end
	  end, error),
    ok.

complex_or_guards(Config) when is_list(Config) ->
    %% complex_or_1/2
    ok = complex_or_1({a,b,c,d}, {1,2,3}),
    ok = complex_or_1({a,b,c,d}, {1}),
    ok = complex_or_1({a}, {1,2,3}),
    error = complex_or_1({a}, {1}),

    error = complex_or_1(1, 2),
    error = complex_or_1([], {a,b,c,d}),
    error = complex_or_1({a,b,c,d}, []),


    %% complex_or_2/1
    ok = complex_or_2({true,{}}),
    ok = complex_or_2({false,{a}}),
    ok = complex_or_2({false,{a,b,c}}),
    ok = complex_or_2({true,{a,b,c,d}}),

    error = complex_or_2({blurf,{a,b,c}}),

    error = complex_or_2({true}),
    error = complex_or_2({true,no_tuple}),
    error = complex_or_2({true,[]}),

    %% complex_or_3/2
    ok = complex_or_3({true}, {}),
    ok = complex_or_3({false}, {a}),
    ok = complex_or_3({false}, {a,b,c}),
    ok = complex_or_3({true}, {a,b,c,d}),
    ok = complex_or_3({false}, <<1,2,3>>),
    ok = complex_or_3({true}, <<1,2,3,4>>),

    error = complex_or_3(blurf, {a,b,c}),

    error = complex_or_3({false}, <<1,2,3,4>>),
    error = complex_or_3([], <<1,2>>),
    error = complex_or_3({true}, 45),
    error = complex_or_3(<<>>, <<>>),

    %% complex_or_4/2
    ok = complex_or_4(<<1,2,3>>, {true}),
    ok = complex_or_4(<<1,2,3>>, {false}),
    ok = complex_or_4(<<1,2,3>>, {true}),
    ok = complex_or_4({1,2,3}, {true}),
    error = complex_or_4({1,2,3,4}, {false}),

    error = complex_or_4(<<1,2,3,4>>, []),
    error = complex_or_4([], {true}),

    %% complex_or_5/2
    ok = complex_or_5(<<1>>, {false}),
    ok = complex_or_5(<<1,2,3>>, {true}),
    ok = complex_or_5(<<1,2,3,4>>, {false}),
    ok = complex_or_5({1,2,3}, {false}),
    ok = complex_or_5({1,2,3,4}, {false}),

    error = complex_or_5(blurf, {false}),
    error = complex_or_5(<<1>>, klarf),
    error = complex_or_5(blurf, klarf),

    %% complex_or_6/2
    ok = complex_or_6({true,true}, {1,2,3,4}),
    ok = complex_or_6({true,true}, <<1,2,3,4>>),
    ok = complex_or_6({false,false}, <<1,2,3,4>>),
    ok = complex_or_6({false,true}, <<1>>),
    ok = complex_or_6({true,false}, {1}),
    ok = complex_or_6({true,true}, {1}),

    error = complex_or_6({false,false}, {1}),

    error = complex_or_6({true}, {1,2,3,4}),
    error = complex_or_6({}, {1,2,3,4}),
    error = complex_or_6([], {1,2,3,4}),
    error = complex_or_6([], {1,2,3,4}),
    error = complex_or_6({true,false}, klurf),

    ok.

complex_or_1(A, B) ->
    if
	((3 < size(A)) and (size(A) < 9)) or
	((2 < size(B)) and (size(B) < 7)) -> ok;
	true -> error
    end.

complex_or_2(Tuple) ->
    if
	element(1, Tuple) or not (size(element(2, Tuple)) > 3) -> ok;
	true -> error
    end.

complex_or_3(A, B) ->
    if
	not (size(B) > 3) or element(1, A) -> ok;
	true -> error
    end.

complex_or_4(A, B) ->
    if
	not (is_tuple(A) and (size(A) > 3)) or element(1, B) -> ok;
	true -> error
    end.

complex_or_5(A, B) ->
    if
	not (is_tuple(A) or (size(A) > 3)) or not element(1, B) -> ok;
	true -> error
    end.

complex_or_6(A, B) ->
    if
	not (not element(1, A) and not element(2, A)) or
	not (not (size(B) > 3)) -> ok;
	true -> error
    end.

and_guard(Config) when is_list(Config) ->

    %% 'and' combinations of literal true/false.

    check(fun() -> if true and false -> ok; true -> error end end, error),
    check(fun() -> if false and true -> ok; true -> error end end, error),
    check(fun() -> if true and true -> ok end end, ok),
    check(fun() -> if false and false -> ok; true -> error end end, error),

    check(fun() -> if glurf and true -> ok; true -> error end end, error),
    check(fun() -> if true and glurf -> ok; true -> error end end, error),
    check(fun() -> if glurf and glurf -> ok; true -> error end end, error),

    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if true and false -> ok;
				false and true -> ok;
				false and false -> ok
			     end),
		  exit
	  end, exit),

    %% 'and' combinations of true/false in variables.

    True = id(true),
    False = id(false),

    check(fun() -> if True and False -> ok; true -> error end end, error),
    check(fun() -> if False and True -> ok; true -> error end end, error),
    check(fun() -> if True and True -> ok end end, ok),
    check(fun() -> if False and False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True and False -> ok;
				False and True -> ok;
				False and False -> ok
			     end),
		  exit
	  end, exit),

    %% 'and' combinations of true/false and a non-boolean in variables.

    Glurf = id(glurf),

    check(fun() -> if True and Glurf -> ok; true -> error end end, error),
    check(fun() -> if Glurf and True -> ok; true -> error end end, error),
    check(fun() -> if True and True -> ok end end, ok),
    check(fun() -> if Glurf and Glurf -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True and Glurf -> ok;
				Glurf and True -> ok;
				Glurf and Glurf -> ok
			     end),
		  exit
	  end, exit),

    %% 'and' combinations of true/false with errors.
    ATuple = id({a,b,c}),

    check(fun() -> if True and element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() -> if element(42, ATuple) and True -> ok;
		      true -> error end end, error),
    check(fun() -> if True and True -> ok end end, ok),
    check(fun() -> if element(42, ATuple) and element(42, ATuple) -> ok;
		      true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} =
		      (catch if True and element(42, ATuple) -> ok;
				element(42, ATuple) and True -> ok;
				element(42, ATuple) and element(42, ATuple) -> ok
			     end),
		  exit
	  end, exit),

    ok = relprod({'Set',a,b}, {'Set',a,b}),

    ok.

relprod(R1, R2) when (erlang:size(R1) =:= 3) and (erlang:element(1,R1) =:= 'Set'), (erlang:size(R2) =:= 3) and (erlang:element(1,R2) =:= 'Set') ->
    ok.


xor_guard(Config) when is_list(Config) ->

    %% 'xor' combinations of literal true/false.
    check(fun() -> if true xor false -> ok end end, ok),
    check(fun() -> if false xor true -> ok end end, ok),
    check(fun() -> if true xor true -> ok; true -> error end end, error),
    check(fun() -> if false xor false -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if false xor false -> ok end),
		  exit
	  end, exit),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if true xor true -> ok end),
		  exit
	  end, exit),


    %% 'xor' combinations using variables containing true/false.

    True = id(true),
    False = id(false),

    check(fun() -> if True xor False -> ok end end, ok),
    check(fun() -> if False xor True -> ok end end, ok),
    check(fun() -> if True xor True -> ok; true -> error end end, error),
    check(fun() -> if False xor False -> ok; true -> error end end, error),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if False xor False -> ok end),
		  exit
	  end, exit),
    check(fun() ->
		  {'EXIT',{if_clause,_}} = (catch if True xor True -> ok end),
		  exit
	  end, exit),

    ok.

more_xor_guards(Config) when is_list(Config) ->
    True = id(true),
    False = id(false),
    ATuple = id({false,true,gurka}),

    check(fun() ->
		  if element(42, ATuple) xor False -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if False xor element(42, ATuple) xor False -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(18, ATuple) xor element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if True xor element(42, ATuple) -> ok;
		     true -> error end
	  end, error),

    check(fun() ->
		  if element(42, ATuple) xor True -> ok;
		     true -> error end
	  end, error),
    ok.

build_in_guard(Config) when is_list(Config) ->
    SubBin = <<5.0/float>>,
    B = <<1,SubBin/binary,3.5/float>>,
    if
	B =:= <<1,SubBin/binary,3.5/float>> -> ok
    end.

old_guard_tests(Config) when list(Config) ->
    %% Check that all the old guard tests are still recognized.
    list = og(Config),
    atom = og(an_atom),
    binary = og(<<1,2>>),
    float = og(3.14),
    integer = og(43),
    a_function = og(fun() -> ok end),
    pid = og(self()),
    reference = og(make_ref()),
    tuple = og({}),

    number = on(45.333),
    number = on(-19),
    ok.

og(V) when atom(V) -> atom;
og(V) when binary(V) -> binary;
og(V) when float(V) -> float;
og(V) when integer(V) -> integer;
og(V) when function(V) -> a_function;
og(V) when list(V) -> list;
og(V) when pid(V) -> pid;
og(V) when port(V) -> port;
og(V) when reference(V) -> reference;
og(V) when tuple(V) -> tuple;
og(_) -> what.

on(V) when number(V) -> number;
on(_) -> not_number.

gbif(Config) when is_list(Config) ->
    error = gbif_1(1, {false,true}),
    ok = gbif_1(2, {false,true}),
    ok.

gbif_1(P, T) when element(P, T) -> ok;
gbif_1(_, _) -> error.


t_is_boolean(Config) when is_list(Config) ->
    true = is_boolean(true),
    true = is_boolean(false),
    true = is_boolean(id(true)),
    true = is_boolean(id(false)),

    false = is_boolean(glurf),
    false = is_boolean(id(glurf)),

    false = is_boolean([]),
    false = is_boolean(id([])),
    false = is_boolean(42),
    false = is_boolean(id(-42)),

    false = is_boolean(math:pi()),
    false = is_boolean(384793478934378924978439789873478934897),

    false = is_boolean(id(self())),
    false = is_boolean(id({x,y,z})),
    false = is_boolean(id([a,b,c])),
    false = is_boolean(id(make_ref())),
    false = is_boolean(id(<<1,2,3>>)),

    ok = bool(true),
    ok = bool(false),
    ok = bool(id(true)),
    ok = bool(id(false)),

    error = bool(glurf),
    error = bool(id(glurf)),

    error = bool([]),
    error = bool(id([])),
    error = bool(42),
    error = bool(id(-42)),

    error = bool(math:pi()),
    error = bool(384793478934378924978439789873478934897),

    error = bool(id(self())),
    error = bool(id({x,y,z})),
    error = bool(id([a,b,c])),
    error = bool(id(make_ref())),
    error = bool(id(<<1,2,3>>)),

    ok.

bool(X) when is_boolean(X) -> ok;
bool(_) -> error.


is_function_2(Config) when is_list(Config) ->
    true = is_function(id(fun ?MODULE:all/1), 1),
    true = is_function(id(fun() -> ok end), 0),
    false = is_function(id(fun ?MODULE:all/1), 0),
    false = is_function(id(fun() -> ok end), 1),

    F = fun(_) -> ok end,
    if
	is_function(F, 1) -> ok
    end.

tricky(Config) when is_list(Config) ->
    not_ok = tricky_1(1, 2),
    not_ok = tricky_1(1, blurf),
    not_ok = tricky_1(foo, 2),
    not_ok = tricky_1(a, b),

    false = rb(100000, [1], 42),
    true = rb(100000, [], 42),
    true = rb(555, [a,b,c], 19),
    ok.

tricky_1(X, Y) when abs((X == 1) or (Y == 2)) -> ok;
tricky_1(_, _) -> not_ok.

%% From dets_v9:read_buckets/11, simplified.

rb(Size, ToRead, SoFar) when SoFar + Size < 81920; ToRead == [] -> true;
rb(_, _, _) -> false.


-define(T(Op,A,B),
	ok = if A Op B -> ok; true -> error end,
	ok = if not (A Op B) -> error; true -> ok end,
	(fun(X, Y, True, False) ->
		 ok = if X Op Y -> ok; true -> error end,
		 ok = if False; X Op Y; False -> ok; true -> error end,
		 ok = if X Op Y, True -> ok; true -> error end,
		 ok = if not (X Op Y) -> error; true -> ok end,
		 ok = if False; not (X Op Y); False -> error; true -> ok end
	 end)(id(A), id(B), id(true), id(false))).

-define(F(Op,A,B),
	ok = if A Op B -> error; true -> ok end,
	ok = if not (A Op B) -> ok; true -> error end,
	(fun(X, Y, True, False) ->
		 ok = if X Op Y -> error; true -> ok end,
		 ok = if False; X Op Y; False -> error; true -> ok end,
		 ok = if not (X Op Y); False -> ok; true -> error end,
		 ok = if not (X Op Y), True -> ok; true -> error end
	 end)(id(A), id(B), id(true), id(false))).


rel_ops(Config) when is_list(Config) ->
    ?T(=/=, 1, 1.0),
    ?F(=/=, 2, 2),
    ?F(=/=, {a}, {a}),

    ?F(/=, a, a),
    ?F(/=, 0, 0.0),
    ?T(/=, 0, 1),
    ?F(/=, {a}, {a}),

    ?T(==, 1, 1.0),
    ?F(==, a, {}),

    ?F(=:=, 1, 1.0),
    ?T(=:=, 42.0, 42.0),

    ?F(>, a, b),
    ?T(>, 42, 1.0),
    ?F(>, 42, 42.0),

    ?T(<, a, b),
    ?F(<, 42, 1.0),
    ?F(<, 42, 42.0),

    ?T(=<, 1.5, 5),
    ?F(=<, -9, -100.344),
    ?T(=<, 42, 42.0),

    ?T(>=, 42, 42.0),
    ?F(>=, a, b),
    ?T(>=, 1.0, 0),

    ok.

-undef(TestOp).

basic_andalso_orelse(Config) when is_list(Config) ->
    T = id({type,integers,23,42}),
    65 = if
	     ((element(1, T) =:= type) andalso (size(T) =:= 4) andalso
	      element(2, T) == integers) ->
		 element(3, T) + element(4, T);
	     true -> error
	 end,
    65 = case [] of
	     [] when ((element(1, T) =:= type) andalso (size(T) =:= 4) andalso
		      element(2, T) == integers) ->
		 element(3, T) + element(4, T)
	 end,

    42 = basic_rt({type,integers,40,2}),
    5.0 = basic_rt({vector,{3.0,4.0}}),
    20 = basic_rt(['+',3,7]),
    {'Set',a,b} = basic_rt({{'Set',a,b},{'Set',a,b}}),
    12 = basic_rt({klurf,4}),

    error = basic_rt({type,integers,40,2,3}),
    error = basic_rt({kalle,integers,40,2}),
    error = basic_rt({kalle,integers,40,2}),
    error = basic_rt({1,2}),
    error = basic_rt([]),

    RelProdBody =
	fun(R1, R2) ->
		if
		    (erlang:size(R1) =:= 3) andalso (erlang:element(1,R1) =:= 'Set'),
		    (erlang:size(R2) =:= 3) andalso (erlang:element(1,R2) =:= 'Set') ->
			ok
		end
	end,

    ok = RelProdBody({'Set',a,b}, {'Set',a,b}),
    ok.

basic_rt(T) when is_tuple(T) andalso size(T) =:= 4 andalso element(1, T) =:= type andalso
		 element(2, T) == integers ->
    element(3, T) + element(4, T);
basic_rt(T) when is_tuple(T) andalso size(T) =:= 2 andalso element(1, T) =:= vector ->
    {X,Y} = element(2, T),
    if
	is_float(X), is_float(Y) ->
	    math:sqrt(X*X+Y*Y)
    end;
basic_rt(['+',A,B]) ->
    2*id(A+B);
basic_rt({R1,R2}) when erlang:size(R1) =:= 3 andalso erlang:element(1,R1) =:= 'Set',
		       erlang:size(R2) =:= 3 andalso erlang:element(1,R2) =:= 'Set' ->
    R1 = id(R1),
    R2 = id(R2),
    R1;
basic_rt(T) when is_tuple(T) andalso size(T) =:= 2 andalso element(1, T) =:= klurf ->
    3*id(element(2, T));
basic_rt(_) ->
    error.

traverse_dcd(Config) when is_list(Config) ->
    L0 = [{log_header,dcd_log,"1.0",a,b,c},{log_header,dcd_log,"2.0",a,b,c},
	  {log_header,dcd_log,"0.0",a,b,c},blurf],
    {cont,[{log_header,dcd_log,"0.0",a,b,c},blurf],log,funny} =
	traverse_dcd({cont,L0}, log, funny),
    L1 = [{log_header,dcd_log,"1.0"}],
    {cont,L1,log,funny} = traverse_dcd({cont,L1}, log, funny),
    L2 = [{a,tuple}],
    {cont,L2,log,funny} = traverse_dcd({cont,L2}, log, funny),
    ok.

%% The function starts out with 3 arguments in {x,0}, {x,1}, {x,2}.
%% The outer match of a two tuple will places the first element in {x,3} and
%% second in {x,4}. The guard for the first clause must make ensure that all of those
%% registers are restored befor entering the second clause.
%%
%% (From mnesia_checkpoint.erl, modified.)

traverse_dcd({Cont,[LogH|Rest]},Log,Fun)
  when is_tuple(LogH) andalso size(LogH) =:= 6 andalso element(1, LogH) =:= log_header
       andalso erlang:element(2,LogH) == dcd_log,
       is_tuple(LogH) andalso size(LogH) =:= 6 andalso element(1, LogH) =:= log_header
       andalso erlang:element(3,LogH) >= "1.0" ->
    traverse_dcd({Cont,Rest},Log,Fun);
traverse_dcd({Cont,Recs},Log,Fun) ->
    {Cont,Recs,Log,Fun}.


check_qlc_hrl(Config) when is_list(Config) ->
    St = {r1,false,dum},
    foo = cqlc(qlc, q, [{lc,1,2,3}], St),
    foo = cqlc(qlc, q, [{lc,1,2,3},b], St),
    St = cqlc(qlc, q, [], St),
    St = cqlc(qlc, blurf, [{lc,1,2,3},b], St),
    St = cqlc(q, q, [{lc,1,2,3},b], St),
    St = cqlc(qlc, q, [{lc,1,2,3},b,c], St),
    St = cqlc(qlc, q, [a,b], St),
    {r1,true,kalle} = cqlc(qlc, q, [{lc,1,2,3},b], {r1,true,kalle}),
    ok.

%% From erl_lint.erl; original name was check_qlc_hrl/4.
cqlc(M, F, As, St) ->
    Arity = length(As),
    case As of
        [{lc,_L,_E,_Qs}|_] when M =:= qlc, F =:= q,
                                Arity < 3,
                                not (((element(1, St) =:= r1) orelse fail) and (size(St) =:= 3) and element(2, St)) ->
            foo;
        _ ->
            St
    end.

%% OTP-7679: Thanks to Hunter Morris.
andalso_semi(Config) when is_list(Config) ->
    ok = andalso_semi_foo(0),
    ok = andalso_semi_foo(1),
    fc(catch andalso_semi_foo(2)),

    ok = andalso_semi_bar([a,b,c]),
    ok = andalso_semi_bar(1),
    fc(catch andalso_semi_bar([a,b])),
    ok.

andalso_semi_foo(Bar) when is_integer(Bar) andalso Bar =:= 0; Bar =:= 1 ->
    ok.

andalso_semi_bar(Bar) when is_list(Bar) andalso length(Bar) =:= 3; Bar =:= 1 ->
    ok.


t_tuple_size(Config) when is_list(Config) ->
    10 = do_tuple_size({1,2,3,4}),
    fc(catch do_tuple_size({1,2,3})),
    fc(catch do_tuple_size(42)),

    error = ludicrous_tuple_size({a,b,c}),
    error = ludicrous_tuple_size([a,b,c]),

    ok.

do_tuple_size(T) when tuple_size(T) =:= 4 ->
    {A,B,C,D} = T,
    A+B+C+D.

ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#7777777777777777777777777777777777 -> ok;
ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#10000000000000000 -> ok;
ludicrous_tuple_size(T)
  when tuple_size(T) =:= (1 bsl 64) - 1 -> ok;
ludicrous_tuple_size(T)
  when tuple_size(T) =:= 16#FFFFFFFFFFFFFFFF -> ok;
ludicrous_tuple_size(_) -> error.

%%
%% The binary_part/2,3 guard BIFs
%%
-define(MASK_ERROR(EXPR),mask_error((catch (EXPR)))).
mask_error({'EXIT',{Err,_}}) ->
    Err;
mask_error(Else) ->
    Else.

%% Tests the binary_part/2,3 guard (GC) bif's.
binary_part(Config) when is_list(Config) ->
    %% This is more or less a copy of what the guard_SUITE in emulator
    %% does to cover the guard bif's
    1 = bptest(<<1,2,3>>),
    2 = bptest(<<2,1,3>>),
    error = bptest(<<1>>),
    error = bptest(<<>>),
    error = bptest(apa),
    3 = bptest(<<2,3,3>>),
    %% With one variable (pos)
    1 = bptest(<<1,2,3>>,1),
    2 = bptest(<<2,1,3>>,1),
    error = bptest(<<1>>,1),
    error = bptest(<<>>,1),
    error = bptest(apa,1),
    3 = bptest(<<2,3,3>>,1),
    %% With one variable (length)
    1 = bptesty(<<1,2,3>>,1),
    2 = bptesty(<<2,1,3>>,1),
    error = bptesty(<<1>>,1),
    error = bptesty(<<>>,1),
    error = bptesty(apa,1),
    3 = bptesty(<<2,3,3>>,2),
    %% With one variable (whole tuple)
    1 = bptestx(<<1,2,3>>,{1,1}),
    2 = bptestx(<<2,1,3>>,{1,1}),
    error = bptestx(<<1>>,{1,1}),
    error = bptestx(<<>>,{1,1}),
    error = bptestx(apa,{1,1}),
    3 = bptestx(<<2,3,3>>,{1,2}),
    %% With two variables
    1 = bptest(<<1,2,3>>,1,1),
    2 = bptest(<<2,1,3>>,1,1),
    error = bptest(<<1>>,1,1),
    error = bptest(<<>>,1,1),
    error = bptest(apa,1,1),
    3 = bptest(<<2,3,3>>,1,2),
    %% Direct (autoimported) call, these will be evaluated by the compiler...
    <<2>> = binary_part(<<1,2,3>>,1,1),
    <<1>> = binary_part(<<2,1,3>>,1,1),
    %% Compiler warnings due to constant evaluation expected (3)
    badarg = ?MASK_ERROR(binary_part(<<1>>,1,1)),
    badarg = ?MASK_ERROR(binary_part(<<>>,1,1)),
    badarg = ?MASK_ERROR(binary_part(apa,1,1)),
    <<3,3>> = binary_part(<<2,3,3>>,1,2),
    %% Direct call through apply
    <<2>> = apply(erlang,binary_part,[<<1,2,3>>,1,1]),
    <<1>> = apply(erlang,binary_part,[<<2,1,3>>,1,1]),
    %% Compiler warnings due to constant evaluation expected (3)
    badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<1>>,1,1])),
    badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<>>,1,1])),
    badarg = ?MASK_ERROR(apply(erlang,binary_part,[apa,1,1])),
    <<3,3>> = apply(erlang,binary_part,[<<2,3,3>>,1,2]),
    %% Constant propagation
    Bin = <<1,2,3>>,
    ok = if
	     binary_part(Bin,1,1) =:= <<2>> ->
		 ok;
	     %% Compiler warning, clause cannot match (expected)
	     true ->
		 error
	 end,
    ok = if
	     binary_part(Bin,{1,1}) =:= <<2>> ->
		 ok;
	     %% Compiler warning, clause cannot match (expected)
	     true ->
		 error
	 end,
    ok.


bptest(B) when length(B) =:= 1337 ->
    1;
bptest(B) when binary_part(B,{1,1}) =:= <<2>> ->
    1;
bptest(B) when erlang:binary_part(B,1,1) =:= <<1>> ->
    2;
bptest(B)  when erlang:binary_part(B,{1,2}) =:= <<3,3>> ->
    3;
bptest(_) ->
    error.

bptest(B,A) when length(B) =:= A ->
    1;
bptest(B,A) when binary_part(B,{A,1}) =:= <<2>> ->
    1;
bptest(B,A) when erlang:binary_part(B,A,1) =:= <<1>> ->
    2;
bptest(B,A)  when erlang:binary_part(B,{A,2}) =:= <<3,3>> ->
    3;
bptest(_,_) ->
    error.

bptestx(B,A) when length(B) =:= A ->
    1;
bptestx(B,A) when binary_part(B,A) =:= <<2>> ->
    1;
bptestx(B,A) when erlang:binary_part(B,A) =:= <<1>> ->
    2;
bptestx(B,A)  when erlang:binary_part(B,A) =:= <<3,3>> ->
    3;
bptestx(_,_) ->
    error.

bptesty(B,A) when length(B) =:= A ->
    1;
bptesty(B,A) when binary_part(B,{1,A}) =:= <<2>> ->
    1;
bptesty(B,A) when erlang:binary_part(B,1,A) =:= <<1>> ->
    2;
bptesty(B,A)  when erlang:binary_part(B,{1,A}) =:= <<3,3>> ->
    3;
bptesty(_,_) ->
    error.

bptest(B,A,_C) when length(B) =:= A ->
    1;
bptest(B,A,C) when binary_part(B,{A,C}) =:= <<2>> ->
    1;
bptest(B,A,C) when erlang:binary_part(B,A,C) =:= <<1>> ->
    2;
bptest(B,A,C)  when erlang:binary_part(B,{A,C}) =:= <<3,3>> ->
    3;
bptest(_,_,_) ->
    error.

-define(FAILING(C),
	if
	    C -> ct:fail(should_fail);
	    true -> ok
	end,
	if
	    true, C -> ct:fail(should_fail);
	    true -> ok
	end).

bad_constants(Config) when is_list(Config) ->
    ?FAILING(false),
    ?FAILING([]),
    ?FAILING([a]),
    ?FAILING([Config]),
    ?FAILING({a,b}),
    ?FAILING({a,Config}),
    ?FAILING(<<1>>),
    ?FAILING(42),
    ?FAILING(3.14),
    ok.

%% Call this function to turn off constant propagation.
id(I) -> I.

check(F, Result) ->
    case F() of
	Result -> ok;
	Other ->
	    io:format("Expected: ~p\n", [Result]),
	    io:format("     Got: ~p\n", [Other]),
	    ct:fail(failed)
    end.

fc({'EXIT',{function_clause,_}}) -> ok.
