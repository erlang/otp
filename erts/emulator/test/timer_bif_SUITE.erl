%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(timer_bif_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([start_timer_1/1, send_after_1/1, send_after_2/1, send_after_3/1,
	 cancel_timer_1/1,
	 start_timer_big/1, send_after_big/1,
	 start_timer_e/1, send_after_e/1, cancel_timer_e/1,
	 read_timer_trivial/1, read_timer/1,
	 cleanup/1, evil_timers/1, registered_process/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(test_server:seconds(30)),
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> ok;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_timer_1, send_after_1, send_after_2,
     cancel_timer_1, start_timer_e, send_after_e,
     cancel_timer_e, start_timer_big, send_after_big,
     read_timer_trivial, read_timer, cleanup, evil_timers,
     registered_process].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


start_timer_1(doc) -> ["Basic start_timer/3 functionality"];
start_timer_1(Config) when is_list(Config) ->
    Ref1 = erlang:start_timer(1000, self(), plopp),
    ok   = get(1100, {timeout, Ref1, plopp}),

    false = erlang:read_timer(Ref1),
    false = erlang:cancel_timer(Ref1),
    false = erlang:read_timer(Ref1),

    Ref2  = erlang:start_timer(1000, self(), plapp),
    Left2 = erlang:cancel_timer(Ref2),
    UpperLimit = 1000,
    true = (Left2 > 900) and (Left2 =< UpperLimit),
    empty = get_msg(),
    false = erlang:cancel_timer(Ref2),

    Ref3 = erlang:start_timer(1000, self(), plopp),
    no_message = get(900, {timeout, Ref3, plopp}),
    ok.

send_after_1(doc) -> ["Basic send_after/3 functionality"];
send_after_1(Config) when is_list(Config) ->
    ?line Ref3 = erlang:send_after(1000, self(), plipp),
    ?line ok = get(1500, plipp),
    ?line false = erlang:read_timer(Ref3),
    ok.

start_timer_big(doc) -> ["Big timeouts for start_timer/3"];
start_timer_big(Config) when is_list(Config) ->
    ?line Big = 1 bsl 31,
    ?line R = erlang:start_timer(Big, self(), hej),
    ?line timer:sleep(200),
    ?line Left = erlang:cancel_timer(R),
    ?line case Big - Left of
	      Diff when Diff >= 200, Diff < 10000 ->
		  ok;
	      _Diff ->
		  test_server:fail({big, Big, Left})
	  end,
    ok.

send_after_big(doc) -> ["Big timeouts for send_after/3"];
send_after_big(Config) when is_list(Config) ->
    ?line Big = 1 bsl 31,
    ?line R = erlang:send_after(Big, self(), hej),
    ?line timer:sleep(200),
    ?line Left = erlang:cancel_timer(R),
    ?line case Big - Left of
	      Diff when Diff >= 200, Diff < 10000 ->
		  ok;
	      _Diff ->
		  test_server:fail({big, Big, Left})
	  end,
    ok.

send_after_2(doc) -> ["send_after/3: messages in the right order, kind version"];
send_after_2(Config) when is_list(Config) ->
    ?line _ = erlang:send_after(5000, self(), last),
    ?line _ = erlang:send_after(0, self(), a0),
    ?line _ = erlang:send_after(200, self(), a2),
    ?line _ = erlang:send_after(100, self(), a1),
    ?line _ = erlang:send_after(500, self(), a5),
    ?line _ = erlang:send_after(300, self(), a3),
    ?line _ = erlang:send_after(400, self(), a4),
    ?line [a0,a1,a2,a3,a4,a5,last] = collect(last),
    ok.

send_after_3(doc) -> ["send_after/3: messages in the right order, worse than send_after_2"];
send_after_3(Config) when is_list(Config) ->
    _ = erlang:send_after(100, self(), b1),
    _ = erlang:send_after(101, self(), b2),
    _ = erlang:send_after(102, self(), b3),
    _ = erlang:send_after(103, self(), last),
    [b1, b2, b3, last] = collect(last),

% This behaviour is not guaranteed:
%    ?line _ = erlang:send_after(100, self(), c1),
%    ?line _ = erlang:send_after(100, self(), c2),
%    ?line _ = erlang:send_after(100, self(), c3),
%    ?line _ = erlang:send_after(100, self(), last),
%    ?line [c1, c2, c3, last] = collect(last),

    ok.

cancel_timer_1(doc) -> ["Check trivial cancel_timer/1 behaviour"];
cancel_timer_1(Config) when is_list(Config) ->
    ?line false = erlang:cancel_timer(make_ref()),

    ok.

start_timer_e(doc) -> ["Error cases for start_timer/3"];
start_timer_e(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch erlang:start_timer(-4, self(), hej)),
    ?line {'EXIT', _} = (catch erlang:start_timer(4728472847827482,
						  self(), hej)),

    ?line {'EXIT', _} = (catch erlang:start_timer(4.5, self(), hej)),
    ?line {'EXIT', _} = (catch erlang:start_timer(a, self(), hej)),

    ?line Node = start_slave(),
    ?line Pid = spawn(Node, timer, sleep, [10000]),
    ?line {'EXIT', _} = (catch erlang:start_timer(1000, Pid, hej)),
    ?line stop_slave(Node),


    ok.

send_after_e(doc) -> ["Error cases for send_after/3"];
send_after_e(suite) -> [];
send_after_e(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch erlang:send_after(-4, self(), hej)),
    ?line {'EXIT', _} = (catch erlang:send_after(4728472847827482,
						 self(), hej)),

    ?line {'EXIT', _} = (catch erlang:send_after(4.5, self(), hej)),
    ?line {'EXIT', _} = (catch erlang:send_after(a, self(), hej)),

    ?line Node = start_slave(),
    ?line Pid = spawn(Node, timer, sleep, [10000]),
    ?line {'EXIT', _} = (catch erlang:send_after(1000, Pid, hej)),
    ?line stop_slave(Node),
    ok.

cancel_timer_e(doc) -> ["Error cases for cancel_timer/1"];
cancel_timer_e(suite) -> [];
cancel_timer_e(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch erlang:cancel_timer(1)),
    ?line {'EXIT', _} = (catch erlang:cancel_timer(self())),
    ?line {'EXIT', _} = (catch erlang:cancel_timer(a)),
    ok.

read_timer_trivial(doc) -> ["Trivial and error test cases for read_timer/1."];
read_timer_trivial(suite) -> [];
read_timer_trivial(Config) when is_list(Config) ->
    ?line false = erlang:read_timer(make_ref()),
    ?line {'EXIT', _} = (catch erlang:read_timer(42)),
    ?line {'EXIT', _} = (catch erlang:read_timer(423497834744444444457667444444)),
    ?line {'EXIT', _} = (catch erlang:read_timer(self())),
    ?line {'EXIT', _} = (catch erlang:read_timer(ab)),
    ok.

read_timer(doc) -> ["Test that read_timer/1 seems to return the correct values."];
read_timer(suite) -> [];
read_timer(Config) when is_list(Config) ->
    ?line Big = 1 bsl 31,
    ?line R = erlang:send_after(Big, self(), hej_hopp),

    ?line receive after 200 -> ok end,		% Delay and clear reductions.
    ?line Left = erlang:read_timer(R),
    ?line Left = erlang:cancel_timer(R),
    ?line false = erlang:read_timer(R),

    ?line case Big - Left of
	      Diff when Diff >= 200, Diff < 10000 ->
		  ok;
	      _Diff ->
		  test_server:fail({big, Big, Left})
	  end,
    ok.

cleanup(doc) -> [];
cleanup(suite) -> [];
cleanup(Config) when is_list(Config) ->
    ?line Mem = mem(),
    %% Timer on dead process
    ?line P1 = spawn(fun () -> ok end),
    ?line wait_until(fun () -> process_is_cleaned_up(P1) end),
    ?line T1 = erlang:start_timer(10000, P1, "hej"),
    ?line T2 = erlang:send_after(10000, P1, "hej"),
    ?line Mem = mem(),
    ?line false = erlang:read_timer(T1),
    ?line false = erlang:read_timer(T2),
    ?line Mem = mem(),
    %% Process dies before timeout
    ?line P2 = spawn(fun () -> receive after 500 -> ok end end),
    ?line T3 = erlang:start_timer(10000, P2, "hej"),
    ?line T4 = erlang:send_after(10000, P2, "hej"),
    ?line true = Mem < mem(),
    ?line true = is_integer(erlang:read_timer(T3)),
    ?line true = is_integer(erlang:read_timer(T4)),
    ?line wait_until(fun () -> process_is_cleaned_up(P2) end),
    ?line false = erlang:read_timer(T3),
    ?line false = erlang:read_timer(T4),
    ?line Mem = mem(),
    %% Cancel timer
    ?line P3 = spawn(fun () -> receive after 20000 -> ok end end),
    ?line T5 = erlang:start_timer(10000, P3, "hej"),
    ?line T6 = erlang:send_after(10000, P3, "hej"),
    ?line true = Mem < mem(),
    ?line true = is_integer(erlang:cancel_timer(T5)),
    ?line true = is_integer(erlang:cancel_timer(T6)),
    ?line false = erlang:read_timer(T5),
    ?line false = erlang:read_timer(T6),
    ?line exit(P3, kill),
    ?line Mem = mem(),
    %% Timeout
    ?line Ref = make_ref(),
    ?line T7 = erlang:start_timer(500, self(), Ref),
    ?line T8 = erlang:send_after(500, self(), Ref),
    ?line true = Mem < mem(),
    ?line true = is_integer(erlang:read_timer(T7)),
    ?line true = is_integer(erlang:read_timer(T8)),
    ?line receive {timeout, T7, Ref} -> ok end,
    ?line receive Ref -> ok end,
    ?line Mem = mem(),
    ?line ok.


evil_timers(doc) -> [];
evil_timers(suite) -> [];
evil_timers(Config) when is_list(Config) ->
    %% Create a composite term consisting of at least:
    %% * externals (remote pids, ports, and refs)
    %% * large (off heap) binaries
    %% * small (heap) binaries
    %% * funs
    %% * bignums
    %% * tuples
    %% * lists
    %% since data of these types have to be adjusted if moved
    %% in memory
    ?line Self = self(),
    ?line R1 = make_ref(),
    ?line Node = start_slave(),
    ?line spawn_link(Node,
		     fun () ->
			     Self ! {R1,
				     [lists:sublist(erlang:ports(), 3),
				      [make_ref(), make_ref(), make_ref()],
				      lists:sublist(processes(), 3),
				      [fun () -> gurka end,
				       fun (A) -> A + 1 end,
				       fun (A, B) -> A + B end]]}
		     end),
    ?line ExtList = receive {R1, L} -> L end,
    ?line stop_slave(Node),
    ?line BinList = [<<"bla">>,
		     <<"blipp">>,
		     <<"blupp">>,
		     list_to_binary(lists:duplicate(1000000,$a)),
		     list_to_binary(lists:duplicate(1000000,$b)),
		     list_to_binary(lists:duplicate(1000000,$c))],
    ?line FunList = [fun () -> gurka end,
		     fun (A) -> A + 1 end,
		     fun (A, B) -> A + B end],
    ?line PidList = lists:sublist(processes(), 3),
    ?line PortList = lists:sublist(erlang:ports(), 3),
    ?line RefList = [make_ref(), make_ref(), make_ref()],
    ?line BigList = [111111111111, 22222222222222, 333333333333333333],
    ?line Msg = {BinList,[FunList,{RefList,ExtList,PidList,PortList,BigList}]},
    %% ?line ?t:format("Msg=~p~n",[Msg]),

    ?line Prio = process_flag(priority, max),
    %%
    %% In the smp case there are four major cases we want to test:
    %%
    %% 1. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is a composite term, expires, and the receivers main
    %%    lock *can not* be acquired immediately (typically when the
    %%    receiver *is* running).
    %%
    %%    The wrap tuple ({timeout, TRef, Msg}) will in this case
    %%    be allocated in the previously allocated message buffer along
    %%    with Msg, i.e. the previously allocated message buffer will be
    %%    reallocated and potentially moved.
    ?line TimeOutMsgs0 = evil_setup_timers(200, Self, Msg),
    ?line RecvTimeOutMsgs0 = evil_recv_timeouts(200),
    %% 2. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is an immediate term, expires, and the receivers main
    %%    lock *can not* be acquired immediately (typically when the
    %%    receiver *is* running).
    %%
    %%    The wrap tuple will in this case be allocated in a new
    %%    message buffer.
    ?line TimeOutMsgs1 = evil_setup_timers(200, Self, immediate),
    ?line RecvTimeOutMsgs1 = evil_recv_timeouts(200),
    %% 3. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is a composite term, expires, and the receivers main
    %%    lock *can* be acquired immediately (typically when the receiver
    %%    *is not* running).
    %%
    %%    The wrap tuple will in this case be allocated on the receivers
    %%    heap, and Msg is passed in the previously allocated message
    %%    buffer.
    ?line R2 = make_ref(),
    ?line spawn_link(fun () ->
			     Self ! {R2, evil_setup_timers(200, Self, Msg)}
		     end),
    ?line receive after 1000 -> ok end,
    ?line TimeOutMsgs2 = receive {R2, TOM2} -> TOM2 end,
    ?line RecvTimeOutMsgs2 = evil_recv_timeouts(200),
    %% 4. A timer started with erlang:start_timer(Time, Receiver, Msg),
    %%    where Msg is an immediate term, expires, and the Receivers main
    %%    lock *can* be acquired immediately (typically when the receiver
    %%    *is not* running).
    %%
    %%    The wrap tuple will in this case be allocated on the receivers
    %%    heap.
    ?line R3 = make_ref(),
    ?line spawn_link(fun () ->
			     Self ! {R3, evil_setup_timers(200,Self,immediate)}
		     end),
    ?line receive after 1000 -> ok end,
    ?line TimeOutMsgs3 = receive {R3, TOM3} -> TOM3 end,
    ?line RecvTimeOutMsgs3 = evil_recv_timeouts(200),

    %% Garge collection will hopefully crash the emulator if something
    %% is wrong...
    ?line garbage_collect(),
    ?line garbage_collect(),
    ?line garbage_collect(),

    %% Make sure we got the timeouts we expected
    %%
    %% Note timeouts are *not* guaranteed to be delivered in order
    ?line ok = match(lists:sort(RecvTimeOutMsgs0), lists:sort(TimeOutMsgs0)),
    ?line ok = match(lists:sort(RecvTimeOutMsgs1), lists:sort(TimeOutMsgs1)),
    ?line ok = match(lists:sort(RecvTimeOutMsgs2), lists:sort(TimeOutMsgs2)),
    ?line ok = match(lists:sort(RecvTimeOutMsgs3), lists:sort(TimeOutMsgs3)),

    ?line process_flag(priority, Prio),
    ?line ok.

evil_setup_timers(N, Receiver, Msg) ->
    ?line evil_setup_timers(0, N, Receiver, Msg, []).

evil_setup_timers(N, N, _Receiver, _Msg, TOs) ->
    ?line TOs;
evil_setup_timers(N, Max, Receiver, Msg, TOs) ->
    ?line TRef = erlang:start_timer(N, Receiver, Msg),
    ?line evil_setup_timers(N+1, Max, Receiver, Msg, [{timeout,TRef,Msg}|TOs]).


evil_recv_timeouts(M) ->
    ?line evil_recv_timeouts([], 0, M).

evil_recv_timeouts(TOs, N, N) ->
    ?line TOs;
evil_recv_timeouts(TOs, N, M) ->
    ?line receive
	      {timeout, _, _} = TO ->
		  ?line evil_recv_timeouts([TO|TOs], N+1, M)
	  after 0 ->
		  ?line evil_recv_timeouts(TOs, N, M)
	  end.

registered_process(doc) -> [];
registered_process(suite) -> [];
registered_process(Config) when is_list(Config) ->
    ?line Mem = mem(),
    %% Cancel
    ?line T1 = erlang:start_timer(500, ?MODULE, "hej"),
    ?line T2 = erlang:send_after(500, ?MODULE, "hej"),
    ?line undefined = whereis(?MODULE),
    ?line true = Mem < mem(),
    ?line true = is_integer(erlang:cancel_timer(T1)),
    ?line true = is_integer(erlang:cancel_timer(T2)),
    ?line false = erlang:read_timer(T1),
    ?line false = erlang:read_timer(T2),
    ?line Mem = mem(),
    %% Timeout register after start
    ?line Ref1 = make_ref(),
    ?line T3 = erlang:start_timer(500, ?MODULE, Ref1),
    ?line T4 = erlang:send_after(500, ?MODULE, Ref1),
    ?line undefined = whereis(?MODULE),
    ?line true = Mem < mem(),
    ?line true = is_integer(erlang:read_timer(T3)),
    ?line true = is_integer(erlang:read_timer(T4)),
    ?line true = register(?MODULE, self()),
    ?line receive {timeout, T3, Ref1} -> ok end,
    ?line receive Ref1 -> ok end,
    ?line Mem = mem(),
    %% Timeout register before start
    ?line Ref2 = make_ref(),
    ?line T5 = erlang:start_timer(500, ?MODULE, Ref2),
    ?line T6 = erlang:send_after(500, ?MODULE, Ref2),
    ?line true = Mem < mem(),
    ?line true = is_integer(erlang:read_timer(T5)),
    ?line true = is_integer(erlang:read_timer(T6)),
    ?line receive {timeout, T5, Ref2} -> ok end,
    ?line receive Ref2 -> ok end,
    ?line Mem = mem(),
    ?line true = unregister(?MODULE),
    ?line ok.

mem() ->
    AA = erlang:system_info(allocated_areas),
    {value,{bif_timer,Mem}} = lists:keysearch(bif_timer, 1, AA),
    Mem.

process_is_cleaned_up(P) when is_pid(P) ->
    undefined == erts_debug:get_internal_state({process_status, P}).

wait_until(Pred) when is_function(Pred) ->
    case catch Pred() of
	true -> ok;
	_ -> receive after 50 -> ok end, wait_until(Pred)
    end.

get(Time, Msg) ->
    receive 
	Msg ->
	    ok
    after Time
	  ->
	    no_message
    end.

get_msg() ->
    receive
	Msg ->
	    {ok, Msg}
    after 0 ->
	    empty
    end.

start_slave() ->
    ?line {A, B, C} = now(),
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line Name = atom_to_list(?MODULE) ++ "-" ++ integer_to_list(A+B+C),
    {ok, Node} = ?t:start_node(Name, slave, [{args, "-pa " ++ Pa}]),
    Node.

stop_slave(Node) ->
    test_server:stop_node(Node).

collect(Last) ->
    collect(Last, []).

receive_one() ->
    receive
	Msg ->
	    Msg
    end.

collect(Last, Msgs0) ->
    Msg = receive_one(),
    Msgs = Msgs0 ++ [Msg],
    case Msg of
	Last ->
	    Msgs;
	_ ->
	    collect(Last, Msgs)
    end.

match(X, X) ->
    %erlang:display({match, X}),
    ok;
match(X, Y) ->
    %erlang:display({mismatch, X, Y}),
    match_aux(X, Y).

match_aux(X, X) ->
    unexpected_error;
match_aux(X, Y) when is_list(X), is_list(Y), length(X) =/= length(Y) ->
    %% erlang:display({mismatch, X, Y}),
    {list_length_mismatch, length(X), length(Y)};
match_aux([X|Xs], [X|Ys]) ->
    match_aux(Xs, Ys);
match_aux([X|_], [Y|_]) ->
    match_aux(X, Y);
match_aux(X, Y) when is_tuple(X), is_tuple(Y), size(X) =/= size(Y) ->
    %% erlang:display({mismatch, X, Y}),
    {tuple_size_mismatch, size(X), size(Y)};
match_aux(X, Y) when is_tuple(X), is_tuple(Y) ->
    match_aux(tuple_to_list(X), tuple_to_list(Y));
match_aux(X, Y) ->
    %% erlang:display({mismatch, X, Y}),
    {mismatch, type(X), type(Y)}.

type(X) when is_list(X) -> list;
type(X) when is_tuple(X) -> tuple;
type(X) when is_float(X) -> float;
type(X) when is_integer(X) -> integer;
type(X) when is_pid(X) -> {pid, node(X)};
type(X) when is_reference(X) -> {reference, node(X)};
type(X) when is_port(X) -> {port, node(X)};
type(X) when is_binary(X) -> binary;
type(X) when is_atom(X) -> atom;
type(_) -> unknown.
    

