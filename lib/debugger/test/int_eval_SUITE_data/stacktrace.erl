-module(stacktrace).
-export([?MODULE/0]).

?MODULE() ->
    OldDepth = erlang:system_flag(backtrace_depth, 32),
    done = (catch do_try()),
    Stk = trim(erlang:get_stacktrace()),
    erlang:system_flag(backtrace_depth, OldDepth),
    {done,Stk}.

trim([{int_eval_SUITE,_,_,_}|_]) ->
    [];
trim([H|T]) ->
    [H|trim(T)];
trim([]) -> [].

do_try() ->
    try
	0 = id(42)
    catch
	error:{badmatch,42} ->
	    do_try2()				%Tail-recursive
    end.

do_try2() ->
    try
	0 = id(42)
    catch
	error:{badmatch,42} ->
	    do_try3()				%Not tail-recursive
    end,
    ?LINE.

do_try3() ->
    try id(42) of
	42 -> do_try4()				%Tail-recursive
    catch
	error:ignore ->				%Should never catch
	    ?LINE
    end.

do_try4() ->
    try
	do_recv()				%Not tail-recursive
    catch
	error:ignore ->				%Should never catch
	    ?LINE
    end.

do_recv() ->
    self() ! x,
    receive
	x -> do_recv2()				%Not tail-recursive
    end,
    ?LINE.

do_recv2() ->
    self() ! y,
    receive
	y -> do_recv3()				%Tail-recursive
    end.

do_recv3() ->
    receive
	after 0 -> do_recv4()			%Tail-recursive
    end.

do_recv4() ->
    receive
	after 0 -> do_if(true)			%Not tail-recursive
    end,
    ?LINE.

do_if(Bool) ->
    if
	Bool -> do_if2(Bool)			%Tail-recursive
    end.

do_if2(Bool) ->
    if
	Bool -> do_case(Bool)			%Not tail-recursive
    end,
    ?LINE.


do_case(Bool) ->
    case Bool of
	true -> do_case2(Bool)			%Tail-recursive
    end.

do_case2(Bool) ->
    case Bool of
	true -> do_fun(Bool)			%Not tail-recursive
    end,
    ?LINE.

do_fun(Bool) ->
    F = fun(true) ->
		do_fun2(Bool)			%Tail-recursive
	end,
    F(Bool).					%Tail-recursive

do_fun2(Bool) ->
    F = fun(true) ->
		cons(Bool)			%Tail-recursive
	end,
    F(Bool),					%Not tail-recursive
    ?LINE.

cons(Bool) ->
    [Bool|tuple()].

tuple() ->
    {ok,op()}.

op() ->
    1 + lc().

lc() ->
    [done() || true].

done() ->
    tail(100),
    throw(done).

tail(0) -> ok;
tail(N) -> tail(N-1).

id(I) ->
    I.
