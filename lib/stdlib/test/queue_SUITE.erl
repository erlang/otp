%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(queue_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).

-export([do/1, to_list/1, io_test/1, op_test/1, error/1, oops/1]).


-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [do, to_list, io_test, op_test, error, oops].

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


do(Config) when is_list(Config) ->
    L = [{in, 1},
	 {in, 2},
	 {out, {value, 1}},
	 {in, 3},
	 {out, {value, 2}},
	 {out, {value, 3}},
	 {out, empty}
	],

    E = queue:new(),
    [] = queue:to_list(E),
    Q = do_queue(E, L),
    true = queue:is_empty(Q),
    0 = queue:len(Q),
    ok.

%% OTP-2701
to_list(Config) when is_list(Config) ->
    E = queue:new(),
    Q = do_queue(E, [{in, 1},
		     {in, 2},
		     {in, 3},
		     {out, {value, 1}},
		     {in, 4},
		     {in, 5}]),
    true = queue:is_queue(Q),
    4 = queue:len(Q),
    case queue:to_list(Q) of
	[2,3,4,5] ->
	    ok;
	Other1 ->
	    ct:fail(Other1)
    end,
    ok.

do_queue(Q, []) ->
    Q;
do_queue(Q, [E | Rest]) ->
    do_queue(do_queue_1(E, Q), Rest).

do_queue_1({in, E}, Q) ->
    queue:in(E, Q);
do_queue_1({out, E}, Q) ->
    case queue:out(Q) of
	{E, Q1} ->
	    Q1;
	Other ->
	    ct:fail({"out failed", E, Q, Other})
    end.


%% Test input and output.
io_test(Config) when is_list(Config) ->
    E = queue:new(),
    do_io_test(E),
    ok.

do_io_test(E) ->
    [4,3,5] =
	io([snoc,snoc,head,head,head,cons,cons,snoc], E, 1),
    [5,3,4] =
	io([cons,cons,daeh,daeh,daeh,snoc,snoc,cons], E, 1),
    [4,3,5] =
	io([in,in,out,out,out,in_r,in_r,in], E, 1),
    [5,3,4] =
	io([in_r,in_r,out_r,out_r,out_r,in,in,in_r], E, 1),
    %%
    [] =
	io([snoc,snoc,head,snoc,snoc,head,head,snoc,head,head], E, 1),
    [] =
	io([cons,cons,daeh,cons,cons,daeh,daeh,cons,daeh,daeh], E, 1),
    [] =
	io([in,in,out,in,in,out,out,in,out,out], E, 1),
    [] =
	io([in_r,in_r,out_r,in_r,in_r,out_r,out_r,in_r,out_r,out_r], 
	   E, 1),
    %%
    [5,6] =
	io([snoc,snoc,snoc,head,head,snoc,snoc,snoc,head,head], E, 1),
    [6,5] =
	io([cons,cons,cons,daeh,daeh,cons,cons,cons,daeh,daeh], E, 1),
    [5,6] =
	io([in,in,in,out,out,in,in,in,out,out], E, 1),
    [6,5] =
	io([in_r,in_r,in_r,out_r,out_r,in_r,in_r,in_r,out_r,out_r], 
	   E, 1),
    %%
    [5] =
	io([snoc,head,head,snoc,head,snoc,head,snoc,head,snoc], E, 1),
    [5] =
	io([cons,daeh,daeh,cons,daeh,cons,daeh,cons,daeh,cons], E, 1),
    [5] =
	io([in,out,out,in,out,in,out,in,out,in], E, 1),
    [5] =
	io([in_r,out_r,out_r,in_r,out_r,in_r,out_r,in_r,out_r,in_r], 
	   E, 1),
    %%
    [] =
	io([snoc,head,snoc,snoc,head,head,snoc,snoc,snoc,head,head,head], 
	   E, 1),
    [] =
	io([cons,daeh,cons,cons,daeh,daeh,cons,cons,cons,daeh,daeh,daeh],
	   E, 1),
    [] =
	io([in,out,in,in,out,out,in,in,in,out,out,out], 
	   E, 1),
    [] =
	io([in_r,out_r,in_r,in_r,out_r,out_r,in_r,in_r,in_r,out_r,out_r,out_r],
	   E, 1),
    %%
    [3] =	io([cons,cons,cons,snoc,daeh,daeh,daeh], E, 1),
    [3] =	io([snoc,snoc,snoc,cons,head,head,head], E, 1),
    [3] =	io([in,in,in,in_r,out,out,out], E, 1),
    [3] =	io([in_r,in_r,in_r,in,out_r,out_r,out_r], E, 1),
    %%
    Q2 = queue:join(queue:cons(1, E),queue:cons(2, E)),
    Q1 = queue:reverse(Q2),
    [1] = io([head],  Q1, 3),
    [1] = io([out],   Q1, 3),
    [1] = io([daeh],  Q2, 3),
    [1] = io([out_r], Q2, 3),
    %%
    [2] =
	io([in,peek,peek_r,drop,in_r,peek,peek_r,in,peek,peek_r,drop_r], E, 1),
    %% Malformed queues UGLY-GUTS-ALL-OVER-THE-PLACE
    [2,1] = io([peek], {[1,2],[]}, 1),
    [1,2] = io([peek_r], {[],[1,2]}, 1),
    %%
    ok.

%% Perform a list of operations to a queue.
%% Keep a reference queue on the side; just a list.
%% Compare the read values between the queues.
%% Return the resulting queue as a list.
%% Inserted values are increments of the previously inserted.
io(Ops, Q, X) ->
    io(Ops, Q, queue:to_list(Q), X).

io([head | Tail], Q, [], X) ->
    true = queue:is_empty(Q),
    {'EXIT',{empty,_}} = (catch {ok,queue:head(Q)}),
    {'EXIT',{empty,_}} = (catch {ok,queue:tail(Q)}),
    io(Tail, Q, [], X);
io([head | Tail], Q, [H | T], X) ->
    H = queue:head(Q),
    false = queue:is_empty(Q),
    io(Tail, queue:tail(Q), T, X);
io([daeh | Tail], Q, [], X) ->
    true = queue:is_empty(Q),
    {'EXIT',{empty,_}} = (catch {ok,queue:daeh(Q)}),
    {'EXIT',{empty,_}} = (catch {ok,queue:liat(Q)}),
    {'EXIT',{empty,_}} = (catch {ok,queue:lait(Q)}),
    io(Tail, Q, [], X);
io([daeh | Tail], Q, QQ, X) ->
    H = queue:daeh(Q),
    false = queue:is_empty(Q),
    [H | T] = lists:reverse(QQ),
    io(Tail, queue:liat(Q), lists:reverse(T), X);
io([out | Tail], Q, [], X) ->
    {empty, Q1} = queue:out(Q),
    io(Tail, Q1, [], X);
io([out | Tail], Q, [H | T], X) ->
    {{value,H}, Q1} = queue:out(Q),
    io(Tail, Q1, T, X);
io([out_r | Tail], Q, [], X) ->
    {empty, Q1} = queue:out_r(Q),
    io(Tail, Q1, [], X);
io([out_r | Tail], Q, QQ, X) ->
    {{value,H}, Q1} = queue:out_r(Q),
    [H | T] = lists:reverse(QQ),
    io(Tail, Q1, lists:reverse(T), X);
io([cons | Tail], Q, QQ, X) ->
    io(Tail, queue:cons(X,Q), [X|QQ], X+1);
io([snoc | Tail], Q, QQ, X) ->
    io(Tail, queue:snoc(Q,X), QQ++[X], X+1);
io([in_r | Tail], Q, QQ, X) ->
    io(Tail, queue:in_r(X,Q), [X|QQ], X+1);
io([in | Tail], Q, QQ, X) ->
    io(Tail, queue:in(X,Q), QQ++[X], X+1);
io([peek | Tail], Q, [], X) ->
    empty = queue:peek(Q),
    io(Tail, Q, [], X);
io([peek | Tail], Q, [H|_]=Q0, X) ->
    {value,H} = queue:peek(Q),
    io(Tail, Q, Q0, X);
io([peek_r | Tail], Q, [], X) ->
    empty = queue:peek_r(Q),
    io(Tail, Q, [], X);
io([peek_r | Tail], Q, Q0, X) ->
    E = lists:last(Q0),
    {value,E} = queue:peek_r(Q),
    io(Tail, Q, Q0, X);
io([drop | Tail], Q, [], X) ->
    try queue:drop(Q) of
	V ->
	    ct:fail({?MODULE,?LINE,V})
    catch
	error:empty ->
	    io(Tail, Q, [], X)
    end;
io([drop | Tail], Q, [_ | T], X) ->
    Q1 = queue:drop(Q),
    io(Tail, Q1, T, X);
io([drop_r | Tail], Q, [], X) ->
    try queue:drop_r(Q) of
	V ->
	    ct:fail({?MODULE,?LINE,V})
    catch
	error:empty ->
	    io(Tail, Q, [], X)
    end;
io([drop_r | Tail], Q, L, X) ->
    io:format("~p~n", [{drop_r,Tail,Q,L,X}]),
    Q1 = queue:drop_r(Q),
    [_ | T] = lists:reverse(L),
    io:format("~p~n", [{drop_r,Q1,T}]),
    io(Tail, Q1, lists:reverse(T), X);
io([], Q, QQ, _X) ->
    QQ = queue:to_list(Q),
    Length = length(QQ),
    Length = queue:len(Q),
    QQ.


%% Test operations on whole queues.
op_test(Config) when is_list(Config) ->
    do_op_test(fun id/1),
    ok.

do_op_test(F) ->
    Len = 50,
    Len2 = 2*Len,
    L1 = lists:seq(1, Len),
    L1r = lists:reverse(L1),
    L2 = lists:seq(Len+1, Len2),
    L2r = lists:reverse(L2),
    L3 = L1++L2,
    L3r = L2r++L1r,
    Q0 = F(queue:new()),
    [] = queue:to_list(Q0),
    Q0 = F(queue:from_list([])),
    Q1 = F(queue:from_list(L1)),
    Q2 = F(queue:from_list(L2)),
    Q3 = F(queue:from_list(L3)),
    Len = queue:len(Q1),
    Len = queue:len(Q2),
    Len2 = queue:len(Q3),
    L1 = queue:to_list(Q1),
    L2 = queue:to_list(Q2),
    L3 = queue:to_list(Q3),
    Q3b = queue:join(Q0, queue:join(queue:join(Q1, Q2), Q0)),
    L3 = queue:to_list(Q3b),
    {Q0, Q3New1} = queue:split(0, Q3),
    L3 = queue:to_list(Q3New1),
    {Q3New2, Q0} = queue:split(Len2, Q3),
    L3 = queue:to_list(Q3New2),
    {Q1a, Q2a} = queue:split(Len, Q3),
    L1 = queue:to_list(Q1a),
    L2 = queue:to_list(Q2a),
    {Q3c, Q3d} = queue:split(2, Q3),
    L3 = queue:to_list(Q3c) ++ queue:to_list(Q3d),
    {Q1b, Q2b} = queue:split(Len, Q3b),
    L1 = queue:to_list(Q1b),
    L2 = queue:to_list(Q2b),
    Len = queue:len(Q1b),
    Len = queue:len(Q2b),
    Len2 = queue:len(Q3b),
    Q1r = queue:reverse(Q1),
    Q2r = queue:reverse(Q2),
    Q1ar = queue:reverse(Q1a),
    Q2ar = queue:reverse(Q2a),
    Q1br = queue:reverse(Q1b),
    Q2br = queue:reverse(Q2b),
    Q3br = queue:reverse(Q3b),
    L1r = queue:to_list(Q1r),
    L1r = queue:to_list(Q1ar),
    L1r = queue:to_list(Q1br),
    L2r = queue:to_list(Q2r),
    L2r = queue:to_list(Q2ar),
    L2r = queue:to_list(Q2br),
    L3r = queue:to_list(Q3br),
    Len = queue:len(Q1br),
    Len = queue:len(Q2br),
    Len2 = queue:len(Q3br),
    false = queue:member([], Q0),
    false = queue:member(0, Q0),
    false = queue:member(0, Q1),
    false = queue:member([], Q1),
    true = queue:member(1, Q1),
    false = queue:member(1.0, Q1),
    true = queue:member(Len, Q1),
    %%
    %% Additional coverage.
    {MyL1r,MyL2r} = lists:split(Len-2, L1r),
    MyQ0r = queue:reverse(F(queue:from_list(L1))),
    {MyQ1r,MyQ2r} = queue:split(Len-2, MyQ0r),
    MyL1r = queue:to_list(MyQ1r),
    MyL2r = queue:to_list(MyQ2r),
    MyQ3r = queue:filter(
	      fun (X) when X rem 4 >= 2 -> false;
		  (X) when X rem 8 == 0 -> [float(X),{X}];
		  (X) when X rem 2 >= 1 -> [{X}];
		  (_)                   -> true
	      end, MyQ1r),
    MyL3r = lists:flatten(
	      [if X rem 8 == 0 -> [float(X),{X}];
		  X rem 2 >= 1 -> {X};
		  true         -> X
	       end || X <- MyL1r,
		      X rem 4 < 2]),
    MyL3r = queue:to_list(MyQ3r),
    MyQ4 = F(queue:from_list([11,22,33,44])),
    [11,22] = queue:to_list(queue:filter(fun(X) when X < 27 -> true;
					    (_) -> [] end, MyQ4)),
    [33,44] = queue:to_list(queue:filter(fun(X) when X < 27 -> false;
					    (X) -> [X] end, MyQ4)),
    %%
    ok.

%% Test queue errors.
error(Config) when is_list(Config) ->
    do_error(fun id/1, illegal_queue),
    do_error(fun id/1, {[],illegal_queue}),
    do_error(fun id/1, {illegal_queue,[17]}),
    ok.

trycatch(F, Args) ->
    trycatch(queue, F, Args).

trycatch(M, F, Args) ->
    try apply(M, F, Args) of
	V -> {value,V}
    catch
	C:R -> {C,R}
    end.

do_error(F, IQ) ->
    io:format("Illegal Queue: ~p~n", [IQ]),
    %%
    {error,badarg} = trycatch(in, [1, IQ]),
    {error,badarg} = trycatch(out, [IQ]),
    {error,badarg} = trycatch(in_r ,[1, IQ]),
    {error,badarg} = trycatch(out_r ,[IQ]),
    {error,badarg} = trycatch(to_list ,[IQ]),
    %%
    {error,badarg} = trycatch(from_list, [no_list]),
    {error,badarg} = trycatch(is_empty, [IQ]),
    {error,badarg} = trycatch(len, [IQ]),
    %%
    {error,badarg} = trycatch(cons, [1, IQ]),
    {error,badarg} = trycatch(head, [IQ]),
    {error,badarg} = trycatch(tail, [IQ]),
    %%
    {error,badarg} = trycatch(snoc, [IQ, 1]),
    {error,badarg} = trycatch(last, [IQ]),
    {error,badarg} = trycatch(daeh, [IQ]),
    {error,badarg} = trycatch(liat, [IQ]),
    {error,badarg} = trycatch(lait, [IQ]),
    {error,badarg} = trycatch(init, [IQ]),
    %%
    {error,badarg} = trycatch(reverse, [IQ]),
    {error,badarg} = trycatch(join, [F(queue:new()), IQ]),
    {error,badarg} = trycatch(join, [IQ, F(queue:new())]),
    {error,badarg} = trycatch(split, [17, IQ]),
    {error,badarg} = trycatch(head, [IQ]),
    %%
    Q0 = F(queue:new()),
    {error,badarg} = trycatch(split, [1, Q0]),
    {error,badarg} = trycatch(split, [2, queue:snoc(Q0, 1)]),
    %%
    {value,false}  = trycatch(is_queue, [IQ]),
    {error,badarg} = trycatch(get, [IQ]),
    {error,badarg} = trycatch(peek, [IQ]),
    {error,badarg} = trycatch(peek_r, [IQ]),
    {error,badarg} = trycatch(filter, [fun id/1, IQ]),
    {error,badarg} = trycatch(filter, [no_fun, Q0]),
    %%
    {error,badarg} = trycatch(member, [1, IQ]),
    ok.

id(X) ->
    X.

%% Test queue errors.
oops(Config) when is_list(Config) ->
    N = 3142,
    Optab = optab(),
    Seed0 = rand:seed(exsplus, {1,2,4}),
    {Is,Seed} = random_list(N, tuple_size(Optab), Seed0, []),
    io:format("~p ", [Is]),
    QA = queue:new(),
    QB = {[]},
    emul([QA], [QB], Seed, [element(I, Optab) || I <- Is]).

optab() ->
    {{new,[],        q,     fun ()     -> {[]} end},
     {is_queue,[q],  v,     fun (_)    -> true end},
     {is_empty,[q],  v,     fun (Q) -> 
				    case Q of 
					{[]} -> true;
					_    -> false
				    end end},
     {len,[q],       v,     fun ({L})   -> length(L) end},
     {to_list,[q],   v,     fun ({L})   -> L end},
     {from_list,[l], q,     fun (L)     -> {L} end},
     {in,[t,q],      q,     fun (X,{L}) -> {L++[X]} end},
     {in_r,[t,q],    q,     fun (X,{L}) -> {[X|L]} end},
     {out,[q],       {v,q}, fun ({L}=Q) ->
				    case L of
					[]    -> {empty,Q};
					[X|T] -> {{value,X},{T}}
				    end
			    end},
     {out_r,[q],     {v,q}, fun ({L}=Q) ->
				    case L of
					[]    -> {empty,Q};
					_ -> 
					    [X|R] = lists:reverse(L),
					    T = lists:reverse(R),
					    {{value,X},{T}}
				    end
			    end},
     {get,[q],       v,     fun ({[]})    -> erlang:error(empty);
				({[H|_]}) -> H
			    end},
     {get_r,[q],     v,     fun ({[]})    -> erlang:error(empty);
				({L})     -> lists:last(L)
			    end},
     {peek,[q],      v,     fun ({[]})    -> empty;
				({[H|_]}) -> {value,H}
			    end},
     {peek_r,[q],    v,     fun ({[]})    -> empty;
				({L})     -> {value,lists:last(L)}
			    end},
     {drop,[q],      q,     fun ({[]})    -> erlang:error(empty);
				({[_|T]}) -> {T}
			    end},
     {drop_r,[q],    q,     fun ({[]})    -> erlang:error(empty);
				({L})     -> [_|R] = lists:reverse(L),
					     {lists:reverse(R)}
			    end},
     {reverse,[q],   q,     fun ({L})     -> {lists:reverse(L)} end},
     {join,[q,q],    q,     fun ({L1}, {L2}) -> {L1++L2} end},
     {split,[n,q],   {q,q}, fun (N, {L})  -> {L1,L2} = lists:split(N, L),
					     {{L1},{L2}} end},
     {member,[t,q],  v,     fun (X, {L})  -> lists:member(X, L) end}
    }.

emul(_, _, _, []) ->
    ok;
emul(QsA0, QsB0, Seed0, [{Op,Ts,S,Fun}|Ops]) ->
    {AsA,Seed} = args(Ts, QsA0, Seed0, []),
    {AsB,Seed} = args(Ts, QsB0, Seed0, []),
    io:format("~n% ~w % ~p ", [Op,AsA]),
    io:format("% ~p :", [AsB]),
    XX = call({queue,Op}, AsA),
    YY = call(Fun, AsB),
    case {XX,YY} of
	{{value,X},{value,Y}} ->
	    {[Qa|_]=QsA,[{Lb}|_]=QsB} = chk(QsA0, QsB0, S, X, Y),
	    case queue:to_list(Qa) of
		Lb ->
		    io:format("|~p| ", [Lb]),
		    emul(QsA, QsB, Seed, Ops);
		La ->
		    throw({to_list,[XX,YY,Op,AsA,AsB,La,Lb]})
	    end;
	{Exception,Exception} ->
	    io:format("!~p! ", [Exception]),
	    emul(QsA0, QsB0, Seed, Ops);
	_ ->
	    throw({diff,[XX,YY,Op,AsA,AsB]})
    end.

args([], _, Seed, R) ->
    {lists:reverse(R),Seed};
args([q|Ts], [Q|Qs]=Qss, Seed, R) ->
    args(Ts, if Qs =:= [] -> Qss; true -> Qs end, Seed, [Q|R]);
args([l|Ts], Qs, Seed0, R) ->
    {N,Seed1} = rand:uniform_s(17, Seed0),
    {L,Seed} = random_list(N, 4711, Seed1, []),
    args(Ts, Qs, Seed, [L|R]);
args([t|Ts], Qs, Seed0, R) ->
    {T,Seed} = rand:uniform_s(4711, Seed0),
    args(Ts, Qs, Seed, [T|R]);
args([n|Ts], Qs, Seed0, R) ->
    {N,Seed} = rand:uniform_s(17, Seed0),
    args(Ts, Qs, Seed, [N|R]).

random_list(0, _, Seed, R) ->
    {R,Seed};
random_list(N, M, Seed0, R) ->
    {X,Seed} = rand:uniform_s(M, Seed0),
    random_list(N-1, M, Seed, [X|R]).

call(Func, As) ->
    try case Func of
	    {M,F} -> apply(M, F, As);
	    _     -> apply(Func, As)
	end of
	V ->
	    {value,V}
    catch
	Class:Reason ->
	    {Class,Reason}
    end.

chk(QsA, QsB, v, X, X) ->
    io:format("<~p> ", [X]),
    {QsA,QsB};
chk(_, _, v, X, Y) ->
    throw({diff,v,[X,Y]});
chk(QsA, QsB, q, Qa, {Lb}=Qb) ->
    case queue:to_list(Qa) of
	Lb -> 
	    io:format("|~p| ", [Lb]),
	    {[Qa|QsA],[Qb|QsB]};
	La ->
	    throw({diff,q,[Qa,La,Lb]})
    end;
chk(QsA, QsB, T, X, Y)
  when tuple_size(T) =:= tuple_size(X), tuple_size(T) =:= tuple_size(Y) ->
    io:format("{"),
    try
	chk_tuple(QsA, QsB, T, X, Y, 1)
    after
	io:format("}")
    end;
chk(_, _, T, X, Y)
  when is_tuple(T), is_tuple(X), is_tuple(Y) ->
    throw({diff,T,[X,Y]}).

chk_tuple(QsA, QsB, T, _, _, N) when N > tuple_size(T) ->
    {QsA,QsB};
chk_tuple(QsA0, QsB0, T, X, Y, N) ->
    {QsA,QsB} = chk(QsA0, QsB0, element(N, T), element(N, X), element(N, Y)),
    chk_tuple(QsA, QsB, T, X, Y, N+1).
