%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-module(guard_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, bad_arith/1, bad_tuple/1, 
	 test_heap_guards/1, guard_bifs/1,
	 type_tests/1,guard_bif_binary_part/1]).

-include_lib("test_server/include/test_server.hrl").

-export([init/3]).
-import(lists, [member/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [bad_arith, bad_tuple, test_heap_guards, guard_bifs,
     type_tests, guard_bif_binary_part].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


bad_arith(doc) -> "Test that a bad arithmetic operation in a guard works correctly.";
bad_arith(Config) when is_list(Config) ->
    ?line 5 = bad_arith1(2, 3),
    ?line 10 = bad_arith1(1, infinity),
    ?line 10 = bad_arith1(infinity, 1),
    ok.

bad_arith1(T1, T2) when T1+T2 < 10 ->
    T1+T2;
bad_arith1(_, _) ->
    10.

bad_tuple(doc) -> "Test that bad arguments to element/2 are handled correctly.";
bad_tuple(Config) when is_list(Config) ->
    ?line error = bad_tuple1(a),
    ?line error = bad_tuple1({a, b}),
    ?line x = bad_tuple1({x, b}),
    ?line y = bad_tuple1({a, b, y}),
    ok.

bad_tuple1(T) when element(1, T) == x ->
    x;
bad_tuple1(T) when element(3, T) == y ->
    y;
bad_tuple1(_) ->
    error.

test_heap_guards(doc) -> "";
test_heap_guards(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),
    
    ?line process_flag(trap_exit, true),
    ?line Tuple = {a, tuple, is, built, here, xxx},
    ?line List = [a, list, is, built, here],

    ?line 'try'(fun a_case/1, [Tuple], [Tuple]),
    ?line 'try'(fun a_case/1, [List], [List, List]),
    ?line 'try'(fun a_case/1, [a], [a]),

    ?line 'try'(fun an_if/1, [Tuple], [Tuple]),
    ?line 'try'(fun an_if/1, [List], [List, List]),
    ?line 'try'(fun an_if/1, [a], [a]),

    ?line 'try'(fun receive_test/1, [Tuple], [Tuple]),
    ?line 'try'(fun receive_test/1, [List], [List, List]),
    ?line 'try'(fun receive_test/1, [a], [a]),
    ?line test_server:timetrap_cancel(Dog).

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

'try'(Fun, Args, Result) ->
    'try'(512, Fun, Args, Result, []).

'try'(0, _, _, _, _) ->
    ok;
'try'(Iter, Fun, Args, Result, Filler) ->
    Pid = spawn_link(?MODULE, init, [Fun,Args,list_to_tuple(Filler)]),
    receive
	{'EXIT', Pid, {result, Result}} ->
	    ?line 'try'(Iter-1, Fun, Args, Result, [0|Filler]);
	{result, Other} ->
	    ?line io:format("Expected ~p; got ~p~n", [Result, Other]),
	    ?line test_server:fail();
	Other ->
	    ?line test_server:fail({unexpected_message, Other})
    end.

init(Fun, Args, Filler) ->
    Result = {result,apply(Fun, Args)},
    dummy(Filler),
    exit(Result).

dummy(_) ->
    ok.

-define(MASK_ERROR(EXPR),mask_error((catch (EXPR)))).
mask_error({'EXIT',{Err,_}}) ->
    Err;
mask_error(Else) ->
    Else.

guard_bif_binary_part(doc) ->
    ["Test the binary_part/2,3 guard BIF's extensively"];
guard_bif_binary_part(Config) when is_list(Config) ->
    %% Overflow tests that need to be unoptimized
    ?line badarg =
	?MASK_ERROR(
	   binary_part(<<1,2,3>>,{16#FFFFFFFFFFFFFFFF,
				 -16#7FFFFFFFFFFFFFFF-1})),
    ?line badarg =
	?MASK_ERROR(
	   binary_part(<<1,2,3>>,{16#FFFFFFFFFFFFFFFF,
				 16#7FFFFFFFFFFFFFFF})),
    F = fun(X) ->
		Master = self(),
		{Pid,Ref} = spawn_monitor( fun() ->
					     A = lists:duplicate(X,a),
					     B = [do_binary_part_guard() | A],
					     Master ! {self(),hd(B)},
					     ok
				     end),
		receive
		    {Pid,ok} ->
			erlang:demonitor(Ref,[flush]),
			ok;
		    Error ->
			Error
		end
	end,
    [ ok = F(N) || N <- lists:seq(1,10000) ],
    ok.


do_binary_part_guard() ->
    ?line 1 = bptest(<<1,2,3>>),
    ?line 2 = bptest(<<2,1,3>>),
    ?line error = bptest(<<1>>),
    ?line error = bptest(<<>>),
    ?line error = bptest(apa),
    ?line 3 = bptest(<<2,3,3>>),
    % With one variable (pos)
    ?line 1 = bptest(<<1,2,3>>,1),
    ?line 2 = bptest(<<2,1,3>>,1),
    ?line error = bptest(<<1>>,1),
    ?line error = bptest(<<>>,1),
    ?line error = bptest(apa,1),
    ?line 3 = bptest(<<2,3,3>>,1),
    % With one variable (length)
    ?line 1 = bptesty(<<1,2,3>>,1),
    ?line 2 = bptesty(<<2,1,3>>,1),
    ?line error = bptesty(<<1>>,1),
    ?line error = bptesty(<<>>,1),
    ?line error = bptesty(apa,1),
    ?line 3 = bptesty(<<2,3,3>>,2),
    % With one variable (whole tuple)
    ?line 1 = bptestx(<<1,2,3>>,{1,1}),
    ?line 2 = bptestx(<<2,1,3>>,{1,1}),
    ?line error = bptestx(<<1>>,{1,1}),
    ?line error = bptestx(<<>>,{1,1}),
    ?line error = bptestx(apa,{1,1}),
    ?line 3 = bptestx(<<2,3,3>>,{1,2}),
    % With two variables
    ?line 1 = bptest(<<1,2,3>>,1,1),
    ?line 2 = bptest(<<2,1,3>>,1,1),
    ?line error = bptest(<<1>>,1,1),
    ?line error = bptest(<<>>,1,1),
    ?line error = bptest(apa,1,1),
    ?line 3 = bptest(<<2,3,3>>,1,2),
    % Direct (autoimported) call, these will be evaluated by the compiler...
    ?line <<2>> = binary_part(<<1,2,3>>,1,1),
    ?line <<1>> = binary_part(<<2,1,3>>,1,1),
    % Compiler warnings due to constant evaluation expected (3)
    ?line badarg = ?MASK_ERROR(binary_part(<<1>>,1,1)),
    ?line badarg = ?MASK_ERROR(binary_part(<<>>,1,1)),
    ?line badarg = ?MASK_ERROR(binary_part(apa,1,1)),
    ?line <<3,3>> = binary_part(<<2,3,3>>,1,2),
    % Direct call through apply
    ?line <<2>> = apply(erlang,binary_part,[<<1,2,3>>,1,1]),
    ?line <<1>> = apply(erlang,binary_part,[<<2,1,3>>,1,1]),
    % Compiler warnings due to constant evaluation expected (3)
    ?line badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<1>>,1,1])),
    ?line badarg = ?MASK_ERROR(apply(erlang,binary_part,[<<>>,1,1])),
    ?line badarg = ?MASK_ERROR(apply(erlang,binary_part,[apa,1,1])),
    ?line <<3,3>> = apply(erlang,binary_part,[<<2,3,3>>,1,2]),
    % Constant propagation
    ?line  Bin = <<1,2,3>>,
    ?line  ok = if
		    binary_part(Bin,1,1) =:= <<2>> ->
			ok;
		    %% Compiler warning, clause cannot match (expected)
		    true ->
			error
		end,
    ?line  ok = if
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


guard_bifs(doc) -> "Test all guard bifs with nasty (but legal arguments).";
guard_bifs(Config) when is_list(Config) ->
    ?line Big = -237849247829874297658726487367328971246284736473821617265433,
    ?line Float = 387924.874,

    %% Succeding use of guard bifs.

    ?line try_gbif('abs/1', Big, -Big),
    ?line try_gbif('float/1', Big, float(Big)),
    ?line try_gbif('float/1', Big, float(id(Big))),
    ?line try_gbif('trunc/1', Float, 387924.0),
    ?line try_gbif('round/1', Float, 387925.0),
    ?line try_gbif('length/1', [], 0),

    ?line try_gbif('length/1', [a], 1),
    ?line try_gbif('length/1', [a, b], 2),
    ?line try_gbif('length/1', lists:seq(0, 31), 32),

    ?line try_gbif('hd/1', [a], a),
    ?line try_gbif('hd/1', [a, b], a),

    ?line try_gbif('tl/1', [a], []),
    ?line try_gbif('tl/1', [a, b], [b]),
    ?line try_gbif('tl/1', [a, b, c], [b, c]),

    ?line try_gbif('size/1', {}, 0),
    ?line try_gbif('size/1', {a}, 1),
    ?line try_gbif('size/1', {a, b}, 2),
    ?line try_gbif('size/1', {a, b, c}, 3),
    ?line try_gbif('size/1', list_to_binary([]), 0),
    ?line try_gbif('size/1', list_to_binary([1]), 1),
    ?line try_gbif('size/1', list_to_binary([1, 2]), 2),
    ?line try_gbif('size/1', list_to_binary([1, 2, 3]), 3),

    ?line try_gbif('bit_size/1', <<0:7>>, 7),

    ?line try_gbif('element/2', {x}, {1, x}),
    ?line try_gbif('element/2', {x, y}, {1, x}),
    ?line try_gbif('element/2', {x, y}, {2, y}),

    ?line try_gbif('self/0', 0, self()),
    ?line try_gbif('node/0', 0, node()),
    ?line try_gbif('node/1', self(), node()),

    %% Failing use of guard bifs.

    ?line try_fail_gbif('abs/1', Big, 1),
    ?line try_fail_gbif('abs/1', [], 1),

    ?line try_fail_gbif('float/1', Big, 42),
    ?line try_fail_gbif('float/1', [], 42),

    ?line try_fail_gbif('trunc/1', Float, 0.0),
    ?line try_fail_gbif('trunc/1', [], 0.0),

    ?line try_fail_gbif('round/1', Float, 1.0),
    ?line try_fail_gbif('round/1', [], a),

    ?line try_fail_gbif('length/1', [], 1),
    ?line try_fail_gbif('length/1', [a], 0),
    ?line try_fail_gbif('length/1', a, 0),
    ?line try_fail_gbif('length/1', {a}, 0),

    ?line try_fail_gbif('hd/1', [], 0),
    ?line try_fail_gbif('hd/1', [a], x),
    ?line try_fail_gbif('hd/1', x, x),

    ?line try_fail_gbif('tl/1', [], 0),
    ?line try_fail_gbif('tl/1', [a], x),
    ?line try_fail_gbif('tl/1', x, x),

    ?line try_fail_gbif('size/1', {}, 1),
    ?line try_fail_gbif('size/1', [], 0),
    ?line try_fail_gbif('size/1', [a], 1),
    ?line try_fail_gbif('size/1', fun() -> 1 end, 0),
    ?line try_fail_gbif('size/1', fun() -> 1 end, 1),

    ?line try_fail_gbif('element/2', {}, {1, x}),
    ?line try_fail_gbif('element/2', {x}, {1, y}),
    ?line try_fail_gbif('element/2', [], {1, z}),

    ?line try_fail_gbif('self/0', 0, list_to_pid("<0.0.0>")),
    ?line try_fail_gbif('node/0', 0, xxxx),
    ?line try_fail_gbif('node/1', self(), xxx),
    ?line try_fail_gbif('node/1', yyy, xxx),
    ok.

try_gbif(Id, X, Y) ->
    case guard_bif(Id, X, Y) of
	{Id, X, Y} ->
	    io:format("guard_bif(~p, ~p, ~p) -- ok", [Id, X, Y]);
	Other ->
	    ?line ok = io:format("guard_bif(~p, ~p, ~p) -- bad result: ~p\n",
				 [Id, X, Y, Other]),
	    ?line test_server:fail()
    end.

try_fail_gbif(Id, X, Y) ->
    case catch guard_bif(Id, X, Y) of
	{'EXIT',{function_clause,[{?MODULE,guard_bif,[Id,X,Y],_}|_]}} ->
	    io:format("guard_bif(~p, ~p, ~p) -- ok", [Id,X,Y]);
	Other ->
	    ?line ok = io:format("guard_bif(~p, ~p, ~p) -- bad result: ~p\n",
				 [Id, X, Y, Other]),
	    ?line test_server:fail()
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
guard_bif('bit_size/1', X, Y) when bit_size(X) == Y ->
    {'bit_size/1', X, Y};
guard_bif('element/2', X, {Pos, Expected}) when element(Pos, X) == Expected ->
    {'element/2', X, {Pos, Expected}};
guard_bif('self/0', X, Y) when self() == Y ->
    {'self/0', X, Y};
guard_bif('node/0', X, Y) when node() == Y ->
    {'node/0', X, Y};
guard_bif('node/1', X, Y) when node(X) == Y ->
    {'node/1', X, Y}.

type_tests(doc) -> "Test the type tests.";
type_tests(Config) when is_list(Config) ->
    ?line Types = all_types(),
    ?line Tests = type_test_desc(),
    ?line put(errors, 0),
    ?line put(violations, 0),
    ?line type_tests(Tests, Types),
    ?line case {get(errors), get(violations)} of
	      {0, 0} ->
		  ok;
	      {0, N} ->
		  {comment, integer_to_list(N) ++ " standard violation(s)"};
	      {Errors, Violations} ->
		  io:format("~p sub test(s) failed, ~p violation(s)",
			    [Errors, Violations]),
		  ?line test_server:fail()
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
			 [{?MODULE,type_test,[Test,Value],Loc}|_]}}
		when is_list(Loc) ->
		    ok;
		{'EXIT',Other} ->
		    ?line test_server:fail({unexpected_error_reason,Other});
		tuple when is_function(Value) ->
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
     {port, open_port({spawn, efile}, [])},
     {function, fun(_) -> "" end},
     {function, fun erlang:abs/1},
     {binary, list_to_binary([])},
     {bitstring, <<0:7>>}].

type_test_desc() ->
    [{binary, [binary]},
     {bitstring, [binary, bitstring]},
     {integer, [small, big]},
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

type_test(integer, X) when is_integer(X) ->
    integer;
type_test(float, X) when is_float(X) ->
    float;
type_test(number, X) when is_number(X) ->
    number;
type_test(atom, X) when is_atom(X) ->
    atom;
type_test(list, X) when is_list(X) ->
    list;
type_test(nonempty_list, [_]) ->
    nonempty_list;
type_test(nil, []) ->
    nil;
type_test(tuple, X) when is_tuple(X) ->
    tuple;
type_test(pid, X) when is_pid(X) ->
    pid;
type_test(reference, X) when is_reference(X) ->
    reference;
type_test(port, X) when is_port(X) ->
    port;
type_test(binary, X) when is_binary(X) ->
    binary;
type_test(bitstring, X) when is_bitstring(X) ->
    bitstring;
type_test(function, X) when is_function(X) ->
    function.

id(I) -> I.
