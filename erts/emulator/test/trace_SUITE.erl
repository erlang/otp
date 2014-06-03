%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(trace_SUITE).

%%%
%%% Tests the trace BIF.
%%%

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, receive_trace/1, self_send/1,
	 timeout_trace/1, send_trace/1,
	 procs_trace/1, dist_procs_trace/1,
	 suspend/1, mutual_suspend/1, suspend_exit/1, suspender_exit/1,
	 suspend_system_limit/1, suspend_opts/1, suspend_waiting/1,
	 new_clear/1, existing_clear/1,
	 set_on_spawn/1, set_on_first_spawn/1, cpu_timestamp/1,
	 system_monitor_args/1, more_system_monitor_args/1,
	 system_monitor_long_gc_1/1, system_monitor_long_gc_2/1, 
	 system_monitor_large_heap_1/1, system_monitor_large_heap_2/1,
	 system_monitor_long_schedule/1,
	 bad_flag/1, trace_delivered/1]).

-include_lib("test_server/include/test_server.hrl").

%%% Internal exports
-export([process/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [cpu_timestamp, receive_trace, self_send, timeout_trace,
     send_trace, procs_trace, dist_procs_trace, suspend,
     mutual_suspend, suspend_exit, suspender_exit,
     suspend_system_limit, suspend_opts, suspend_waiting,
     new_clear, existing_clear, set_on_spawn,
     set_on_first_spawn, system_monitor_args,
     more_system_monitor_args, system_monitor_long_gc_1,
     system_monitor_long_gc_2, system_monitor_large_heap_1,
      system_monitor_long_schedule,
     system_monitor_large_heap_2, bad_flag, trace_delivered].

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



%% No longer testing anything, just reporting whether cpu_timestamp
%% is enabled or not.
cpu_timestamp(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),

    %% Test whether cpu_timestamp is implemented on this platform.
    ?line Works = try erlang:trace(all, true, [cpu_timestamp]) of
		      _ ->
			  ?line erlang:trace(all, false, [cpu_timestamp]),
			  true
		  catch
		      error:badarg -> false
		  end,

    ?line test_server:timetrap_cancel(Dog),
    {comment,case Works of
		 false -> "cpu_timestamp is NOT implemented/does not work";
		 true -> "cpu_timestamp works"
	     end}.


%% Tests that trace(Pid, How, ['receive']) works.

receive_trace(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line Receiver = fun_spawn(fun receiver/0),
    ?line process_flag(trap_exit, true),

    %% Trace the process; make sure that we receive the trace messages.
    ?line 1 = erlang:trace(Receiver, true, ['receive']),
    ?line Hello = {hello, world},
    ?line Receiver ! Hello,
    ?line {trace, Receiver, 'receive', Hello} = receive_first(),
    ?line Hello2 = {hello, again, world},
    ?line Receiver ! Hello2,
    ?line {trace, Receiver, 'receive', Hello2} = receive_first(),
    ?line receive_nothing(),

    %% Another process should not be able to trace Receiver.
    ?line Intruder = fun_spawn(fun() -> erlang:trace(Receiver, true, ['receive']) end),
    ?line {'EXIT', Intruder, {badarg, _}} = receive_first(),

    %% Untrace the process; we should not receive anything.
    ?line 1 = erlang:trace(Receiver, false, ['receive']),
    ?line Receiver ! {hello, there},
    ?line Receiver ! any_garbage,
    ?line receive_nothing(),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.

self_send(doc) -> ["Test that traces are generated for messages sent ",
		    "and received to/from self()."];
self_send(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line Fun =
	fun(Self, Parent) -> receive
			   go_ahead ->
				     self() ! from_myself,
				     Self(Self, Parent);
			   from_myself ->
				     Parent ! done
		       end
	end,
    ?line Self = self(),
    ?line SelfSender = fun_spawn(Fun, [Fun, Self]),
    ?line erlang:trace(SelfSender, true, ['receive', 'send']),
    ?line SelfSender ! go_ahead,
    ?line receive {trace, SelfSender, 'receive', go_ahead} -> ok end,
    ?line receive {trace, SelfSender, 'receive', from_myself} -> ok end,
    ?line receive
	      {trace,SelfSender,send,from_myself,SelfSender} -> ok
	  end,
    ?line receive {trace,SelfSender,send,done,Self} -> ok end,
    ?line receive done -> ok end,

    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Test that we can receive timeout traces.
timeout_trace(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),

    ?line Process = fun_spawn(fun process/0),
    ?line 1 = erlang:trace(Process, true, ['receive']),
    ?line Process ! timeout_please,
    ?line {trace, Process, 'receive', timeout_please} = receive_first(),
    ?line {trace, Process, 'receive', timeout} = receive_first(),
    ?line receive_nothing(),

    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests that trace(Pid, How, [send]) works.

send_trace(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line process_flag(trap_exit, true),
    ?line Sender = fun_spawn(fun sender/0),
    ?line Receiver = fun_spawn(fun receiver/0),

    %% Check that a message sent to another process is traced.
    ?line 1 = erlang:trace(Sender, true, [send]),
    ?line Sender ! {send_please, Receiver, to_receiver},
    ?line {trace, Sender, send, to_receiver, Receiver} = receive_first(),
    ?line receive_nothing(),

    %% Check that a message sent to another registered process is traced.
    register(?MODULE,Receiver),
    Sender ! {send_please, ?MODULE, to_receiver},
    {trace, Sender, send, to_receiver, ?MODULE} = receive_first(),
    receive_nothing(),
    unregister(?MODULE),

    %% Check that a message sent to this process is traced.
    ?line Sender ! {send_please, self(), to_myself},
    ?line receive to_myself -> ok end,
    ?line Self = self(),
    ?line {trace, Sender, send, to_myself, Self} = receive_first(),
    ?line receive_nothing(),

    %% Check that a message sent to dead process is traced.
    {Pid,Ref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN',Ref,_,_,_} -> ok end,
    Sender ! {send_please, Pid, to_dead},
    {trace, Sender, send_to_non_existing_process, to_dead, Pid} = receive_first(),
    receive_nothing(),

    %% Check that a message sent to unknown registrated process is traced.
    BadargSender = fun_spawn(fun sender/0),
    1 = erlang:trace(BadargSender, true, [send]),
    unlink(BadargSender),
    BadargSender ! {send_please, not_registered, to_unknown},
    {trace, BadargSender, send, to_unknown, not_registered} = receive_first(),
    receive_nothing(),

    %% Another process should not be able to trace Sender.
    ?line Intruder = fun_spawn(fun() -> erlang:trace(Sender, true, [send]) end),
    ?line {'EXIT', Intruder, {badarg, _}} = receive_first(),

    %% Untrace the sender process and make sure that we receive no more
    %% trace messages.
    ?line 1 = erlang:trace(Sender, false, [send]),
    ?line Sender ! {send_please, Receiver, to_receiver},
    ?line Sender ! {send_please, self(), to_myself_again},
    ?line receive to_myself_again -> ok end,
    ?line receive_nothing(),
    
    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Test trace(Pid, How, [procs]).
procs_trace(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line Name = list_to_atom(atom_to_list(?MODULE)++"_procs_trace"),
    ?line Self = self(),
    ?line process_flag(trap_exit, true),
    %%
    ?line Proc1 = spawn_link(?MODULE, process, [Self]),
    ?line io:format("Proc1 = ~p ~n", [Proc1]),
    ?line Proc2 = spawn(?MODULE, process, [Self]),
    ?line io:format("Proc2 = ~p ~n", [Proc2]),
    %%
    ?line 1 = erlang:trace(Proc1, true, [procs]),
    ?line MFA = {?MODULE, process, [Self]},
    %%
    %% spawn, link
    ?line Proc1 ! {spawn_link_please, Self, MFA},
    ?line Proc3 = receive {spawned, Proc1, P3} -> P3 end,
    ?line {trace, Proc1, spawn, Proc3, MFA} = receive_first(),
    ?line io:format("Proc3 = ~p ~n", [Proc3]),
    ?line {trace, Proc1, link, Proc3} = receive_first(),
    ?line receive_nothing(),
    %%
    %% getting_unlinked by exit()
    ?line Proc1 ! {trap_exit_please, true},
    ?line Reason3 = make_ref(),
    ?line Proc1 ! {send_please, Proc3, {exit_please, Reason3}},
    ?line receive {Proc1, {'EXIT', Proc3, Reason3}} -> ok end,
    ?line {trace, Proc1, getting_unlinked, Proc3} = receive_first(),
    ?line Proc1 ! {trap_exit_please, false},
    ?line receive_nothing(),
    %%
    %% link
    ?line Proc1 ! {link_please, Proc2},
    ?line {trace, Proc1, link, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% unlink
    ?line Proc1 ! {unlink_please, Proc2},
    ?line {trace, Proc1, unlink, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% getting_linked
    ?line Proc2 ! {link_please, Proc1},
    ?line {trace, Proc1, getting_linked, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% getting_unlinked
    ?line Proc2 ! {unlink_please, Proc1},
    ?line {trace, Proc1, getting_unlinked, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% register
    ?line true = register(Name, Proc1),
    ?line {trace, Proc1, register, Name} = receive_first(),
    ?line receive_nothing(),
    %%
    %% unregister
    ?line true = unregister(Name),
    ?line {trace, Proc1, unregister, Name} = receive_first(),
    ?line receive_nothing(),
    %%
    %% exit (with registered name, due to link)
    ?line Reason4 = make_ref(),
    ?line Proc1 ! {spawn_link_please, Self, MFA},
    ?line Proc4 = receive {spawned, Proc1, P4} -> P4 end,
    ?line {trace, Proc1, spawn, Proc4, MFA} = receive_first(),
    ?line io:format("Proc4 = ~p ~n", [Proc4]),
    ?line {trace, Proc1, link, Proc4} = receive_first(),
    ?line Proc1 ! {register_please, Name, Proc1},
    ?line {trace, Proc1, register, Name} = receive_first(),
    ?line Proc4 ! {exit_please, Reason4},
    ?line receive {'EXIT', Proc1, Reason4} -> ok end,
    ?line {trace, Proc1, exit, Reason4} = receive_first(),
    ?line {trace, Proc1, unregister, Name} = receive_first(),
    ?line receive_nothing(),
    %%
    %% exit (not linked to tracing process)
    ?line 1 = erlang:trace(Proc2, true, [procs]),
    ?line Reason2 = make_ref(),
    ?line Proc2 ! {exit_please, Reason2},
    ?line {trace, Proc2, exit, Reason2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.


dist_procs_trace(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(15)),
    ?line OtherName = atom_to_list(?MODULE)++"_dist_procs_trace",
    ?line {ok, OtherNode} = start_node(OtherName),
    ?line Self = self(),
    ?line process_flag(trap_exit, true),
    %%
    ?line Proc1 = spawn_link(?MODULE, process, [Self]),
    ?line io:format("Proc1 = ~p ~n", [Proc1]),
    ?line Proc2 = spawn(OtherNode, ?MODULE, process, [Self]),
    ?line io:format("Proc2 = ~p ~n", [Proc2]),
    %%
    ?line 1 = erlang:trace(Proc1, true, [procs]),
    ?line MFA = {?MODULE, process, [Self]},
    %%
    %% getting_unlinked by exit()
    ?line Proc1 ! {spawn_link_please, Self, OtherNode, MFA},
    ?line Proc1 ! {trap_exit_please, true},
    ?line Proc3 = receive {spawned, Proc1, P3} -> P3 end,
    ?line io:format("Proc3 = ~p ~n", [Proc3]),
    ?line {trace, Proc1, getting_linked, Proc3} = receive_first(),
    ?line Reason3 = make_ref(),
    ?line Proc1 ! {send_please, Proc3, {exit_please, Reason3}},
    ?line receive {Proc1, {'EXIT', Proc3, Reason3}} -> ok end,
    ?line {trace, Proc1, getting_unlinked, Proc3} = receive_first(),
    ?line Proc1 ! {trap_exit_please, false},
    ?line receive_nothing(),
    %%
    %% link
    ?line Proc1 ! {link_please, Proc2},
    ?line {trace, Proc1, link, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% unlink
    ?line Proc1 ! {unlink_please, Proc2},
    ?line {trace, Proc1, unlink, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% getting_linked
    ?line Proc2 ! {link_please, Proc1},
    ?line {trace, Proc1, getting_linked, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% getting_unlinked
    ?line Proc2 ! {unlink_please, Proc1},
    ?line {trace, Proc1, getting_unlinked, Proc2} = receive_first(),
    ?line receive_nothing(),
    %%
    %% exit (with registered name, due to link)
    ?line Name = list_to_atom(OtherName),
    ?line Reason2 = make_ref(),
    ?line Proc1 ! {link_please, Proc2},
    ?line {trace, Proc1, link, Proc2} = receive_first(),
    ?line Proc1 ! {register_please, Name, Proc1},
    ?line {trace, Proc1, register, Name} = receive_first(),
    ?line Proc2 ! {exit_please, Reason2},
    ?line receive {'EXIT', Proc1, Reason2} -> ok end,
    ?line {trace, Proc1, exit, Reason2} = receive_first(),
    ?line {trace, Proc1, unregister, Name} = receive_first(),
    ?line receive_nothing(),
    %%
    %% Done.
    ?line true = stop_node(OtherNode),
    ?line test_server:timetrap_cancel(Dog),
    ok.




%% Tests trace(Pid, How, [set_on_spawn]).

set_on_spawn(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line Listener = fun_spawn(fun process/0),

    %% Create and trace a process with the set_on_spawn flag.
    %% Make sure it is traced.
    ?line Father_SOS = fun_spawn(fun process/0),
    ?line 1 = erlang:trace(Father_SOS, true, [send, set_on_spawn]),
    ?line true = is_send_traced(Father_SOS, Listener, sos_father),

    %% Have the process spawn of two children and test that they
    %% are traced.
    ?line [Child1, Child2] = spawn_children(Father_SOS, 2),
    ?line true = is_send_traced(Child1, Listener, child1),
    ?line true = is_send_traced(Child2, Listener, child2),

    %% Second generation.
    [Child11, Child12] = spawn_children(Child1, 2),
    ?line true = is_send_traced(Child11, Listener, child11),
    ?line true = is_send_traced(Child12, Listener, child12),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests trace(Pid, How, [set_on_first_spawn]).

set_on_first_spawn(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Listener = fun_spawn(fun process/0),

    %% Create and trace a process with the set_on_first_spawn flag.
    %% Make sure it is traced.
    ?line Parent = fun_spawn(fun process/0),
    ?line 1 = erlang:trace(Parent, true, [send, set_on_first_spawn]),
    ?line is_send_traced(Parent, Listener, sos_father),

    %% Have the process spawn off three children and test that the
    %% first is traced.
    ?line [Child1, Child2, Child3] = spawn_children(Parent, 3),
    ?line true = is_send_traced(Child1, Listener, child1),
    ?line false = is_send_traced(Child2, Listener, child2),
    ?line false = is_send_traced(Child3, Listener, child3),
    ?line receive_nothing(),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.


system_monitor_args(doc) ->
    ["Tests arguments to erlang:system_monitor/0-2)"];
system_monitor_args(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line Self = self(),
    %%
    ?line OldMonitor = erlang:system_monitor(undefined),
    ?line undefined = erlang:system_monitor(Self, [{long_gc,0}]),
    ?line MinT = case erlang:system_monitor() of
		     {Self,[{long_gc,T}]} when is_integer(T), T > 0 -> T;
		     Other1 -> test_server:fault(Other1)
		 end,
    ?line {Self,[{long_gc,MinT}]} = erlang:system_monitor(),
    ?line {Self,[{long_gc,MinT}]} = 
	erlang:system_monitor({Self,[{large_heap,0}]}),
    ?line MinN = case erlang:system_monitor() of
		  {Self,[{large_heap,N}]} when is_integer(N), N > 0 -> N;
		  Other2 -> test_server:fault(Other2)
	      end,
    ?line {Self,[{large_heap,MinN}]} = erlang:system_monitor(),
    ?line {Self,[{large_heap,MinN}]} = 
	erlang:system_monitor(Self, [busy_port]),
    ?line {Self,[busy_port]} = erlang:system_monitor(),
    ?line {Self,[busy_port]} = 
	erlang:system_monitor({Self,[busy_dist_port]}),
    ?line {Self,[busy_dist_port]} = erlang:system_monitor(),
    ?line All = lists:sort([busy_port,busy_dist_port,
			    {long_gc,1},{large_heap,65535}]),
    ?line {Self,[busy_dist_port]} = erlang:system_monitor(Self, All),
    ?line {Self,A1} = erlang:system_monitor(),
    ?line All = lists:sort(A1),
    ?line {Self,A1} = erlang:system_monitor(Self, []),
    ?line Pid = spawn(fun () -> receive {Self,die} -> exit(die) end end),
    ?line Mref = erlang:monitor(process, Pid),
    ?line undefined = erlang:system_monitor(Pid, All),
    ?line {Pid,A2} = erlang:system_monitor(),
    ?line All = lists:sort(A2),
    ?line Pid ! {Self,die},
    ?line receive {'DOWN',Mref,_,_,_} -> ok end,
    ?line undefined = erlang:system_monitor(OldMonitor),
    ?line erlang:yield(),
    ?line OldMonitor = erlang:system_monitor(),
    %%
    ?line {'EXIT',{badarg,_}} = (catch erlang:system_monitor(atom)),
    ?line {'EXIT',{badarg,_}} = (catch erlang:system_monitor({})),
    ?line {'EXIT',{badarg,_}} = (catch erlang:system_monitor({1})),
    ?line {'EXIT',{badarg,_}} = (catch erlang:system_monitor({1,2,3})),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor({Self,atom})),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor(atom, atom)),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor({Self,[busy_port|busy_dist_port]})),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor(Self, [{long_gc,-1}])),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor({Self,[{long_gc,atom}]})),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor(Self,[{large_heap,-1}])),
    ?line {'EXIT',{badarg,_}} = 
	(catch erlang:system_monitor({Self,[{large_heap,atom}]})),
    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.


more_system_monitor_args(doc) ->
    ["Tests arguments to erlang:system_monitor/0-2)"];
more_system_monitor_args(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    
    ?line try_l(64000),
    ?line try_l(16#7ffffff),
    ?line try_l(16#3fffffff),
    ?line try_l(16#7fffffff),
    ?line try_l(16#ffffffff),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.

try_l(Val) ->
    Self = self(),
    Arbitrary1 = 77777,
    Arbitrary2 = 88888,

    ?line erlang:system_monitor(undefined),

    ?line undefined = erlang:system_monitor(Self, [{long_gc,Val},{large_heap,Arbitrary1}]),

    ?line {Self,Comb0} = erlang:system_monitor(Self, [{long_gc,Arbitrary2},{large_heap,Val}]),
    ?line [{large_heap,Arbitrary1},{long_gc,Val}] = lists:sort(Comb0),

    ?line {Self,Comb1} = erlang:system_monitor(undefined),
    ?line [{large_heap,Val},{long_gc,Arbitrary2}] = lists:sort(Comb1).

monitor_sys(Parent) ->
    receive 
	{monitor,Pid,long_schedule,Data} when is_pid(Pid) -> 
	    io:format("Long schedule of ~w: ~w~n",[Pid,Data]),
	    Parent ! {Pid,Data},
	    monitor_sys(Parent);
	{monitor,Port,long_schedule,Data} when is_port(Port) -> 
	    {name,Name} = erlang:port_info(Port,name),
	    io:format("Long schedule of ~w (~p): ~w~n",[Port,Name,Data]),
	    Parent ! {Port,Data},
	    monitor_sys(Parent);
	Other ->
	    erlang:display(Other)
    end.

start_monitor() ->
    Parent = self(),
    Mpid = spawn_link(fun() -> monitor_sys(Parent) end),
    erlang:system_monitor(Mpid,[{long_schedule,100}]),
    erlang:yield(), % Need to be rescheduled for the trace to take
    ok.

system_monitor_long_schedule(suite) ->
    [];
system_monitor_long_schedule(doc) ->
    ["Tests erlang:system_monitor(Pid, [{long_schedule,Time}])"];
system_monitor_long_schedule(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    erl_ddll:start(),
    case (catch load_driver(Path, slow_drv)) of
	ok ->
	    do_system_monitor_long_schedule();
	_Error ->
	    {skip, "Unable to load slow_drv (windows or no usleep()?)"}
    end.
do_system_monitor_long_schedule() ->
    start_monitor(),
    Port = open_port({spawn_driver,slow_drv}, []),
    "ok" = erlang:port_control(Port,0,[]),
    Self = self(),
    receive
	{Self,L} when is_list(L) ->
	    ok
    after 1000 ->
	    ?t:fail(no_trace_of_pid)
    end,
    "ok" = erlang:port_control(Port,1,[]),
    "ok" = erlang:port_control(Port,2,[]),
    receive
	{Port,LL} when is_list(LL) ->
	    ok
    after 1000 ->
	    ?t:fail(no_trace_of_port)
    end,
    port_close(Port),
    erlang:system_monitor(undefined),
    ok.


-define(LONG_GC_SLEEP, 670).

system_monitor_long_gc_1(suite) ->
    [];
system_monitor_long_gc_1(doc) ->
    ["Tests erlang:system_monitor(Pid, [{long_gc,Time}])"];
system_monitor_long_gc_1(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    try 
	case erts_debug:get_internal_state(force_heap_frags) of
	    true ->
		{skip,"emulator with FORCE_HEAP_FRAGS defined"};
	    false ->
		%% Add ?LONG_GC_SLEEP ms to all gc
		?line erts_debug:set_internal_state(test_long_gc_sleep,
						    ?LONG_GC_SLEEP),
		?line LoadFun = fun () -> 
					garbage_collect(),
					self() 
				end,
		?line long_gc(LoadFun, false)
	end
    after
	erts_debug:set_internal_state(test_long_gc_sleep, 0),
	erts_debug:set_internal_state(available_internal_state, false)	
    end.

system_monitor_long_gc_2(suite) ->
    [];
system_monitor_long_gc_2(doc) ->
    ["Tests erlang:system_monitor(Pid, [{long_gc,Time}])"];
system_monitor_long_gc_2(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    try
	case erts_debug:get_internal_state(force_heap_frags) of
	    true ->
		{skip,"emulator with FORCE_HEAP_FRAGS defined"};
	    false ->
		%% Add ?LONG_GC_SLEEP ms to all gc
		?line erts_debug:set_internal_state(test_long_gc_sleep,
						    ?LONG_GC_SLEEP),
		?line Parent = self(),
		?line LoadFun =
		    fun () ->
			    Ref = make_ref(),
			    Pid = 
				spawn_link(
				  fun () ->
					  garbage_collect(),
					  Parent ! {Ref, self()}
				  end),
			    receive {Ref, Pid} -> Pid end
		    end,
		?line long_gc(LoadFun, true),
		?line long_gc(LoadFun, true),
		?line long_gc(LoadFun, true)
	end
    after
	erts_debug:set_internal_state(test_long_gc_sleep, 0),
	erts_debug:set_internal_state(available_internal_state, false)
    end.

long_gc(LoadFun, ExpectMonMsg) ->
    ?line Self = self(),
    ?line Time = 1,
    ?line OldMonitor = erlang:system_monitor(Self, [{long_gc,Time}]),
    ?line Pid = LoadFun(),
    ?line Ref = erlang:trace_delivered(Pid),
    ?line receive {trace_delivered, Pid, Ref} -> ok end,
    ?line {Self,[{long_gc,Time}]} = erlang:system_monitor(OldMonitor),
    ?line case {long_gc_check(Pid, Time, undefined), ExpectMonMsg} of
	      {ok, true} when Pid =/= Self ->
		  ok;
	      {ok, false} ->
		  ?line ?t:fail(unexpected_system_monitor_message_received);
	      {undefined, false} ->
		  ok;
	      {undefined, true} ->
		  ?line ?t:fail(no_system_monitor_message_received)
	  end.

long_gc_check(Pid, Time, Result) ->
    receive
	{monitor,Pid,long_gc,L} = Monitor ->
	    case lists:foldl(
		   fun (_, error) ->
			   error;
		       ({timeout,T}, N) when is_integer(T),
					     Time =< T, T =< 10*?LONG_GC_SLEEP ->
			   %% OTP-7622. The time T must be within reasonable limits
			   %% for the test to pass.
			   N-1;
		       ({heap_size,_}, N) ->
			   N-1;
		       ({old_heap_size,_}, N) ->
			   N-1;
		       ({stack_size,_}, N) ->
			   N-1;
		       ({mbuf_size,_}, N) ->
			   N-1;
		       ({heap_block_size,_}, N) ->
			   N-1;
		       ({old_heap_block_size,_}, N) ->
			   N-1;
		       (_, _) ->
			   error
		   end, 7, L) of
		0 ->
		    long_gc_check(Pid, Time, ok);
		error ->
		    {error,Monitor}
	    end;
	{monitor,_,long_gc,_} ->
	    long_gc_check(Pid, Time, Result);
	Other ->
	    {error,Other}
    after 0 ->
	    Result
    end.

system_monitor_large_heap_1(suite) ->
    [];
system_monitor_large_heap_1(doc) ->
    ["Tests erlang:system_monitor(Pid, [{large_heap,Size}])"];
system_monitor_large_heap_1(Config) when is_list(Config) ->
    ?line LoadFun =
	fun (Size) -> 
		List = seq(1,2*Size),
		garbage_collect(),
		true = lists:prefix([1], List),
		self() 
	end,
    ?line large_heap(LoadFun, false).

system_monitor_large_heap_2(suite) ->
    [];
system_monitor_large_heap_2(doc) ->
    ["Tests erlang:system_monitor(Pid, [{large_heap,Size}])"];
system_monitor_large_heap_2(Config) when is_list(Config) ->
    ?line Parent = self(),
    ?line LoadFun =
	fun (Size) ->
		Ref = make_ref(),
		Pid = 
		    spawn_opt(fun () ->
				      garbage_collect(),
				      Parent ! {Ref, self()}
			      end,
			      [link, {min_heap_size, 2*Size}]),
		receive {Ref, Pid} -> Pid end
	end,
    ?line large_heap(LoadFun, true).

large_heap(LoadFun, ExpectMonMsg) ->
    ?line Dog = test_server:timetrap(test_server:seconds(20)),
    %%
    ?line Size = 65535,
    ?line Self = self(),
    ?line NewMonitor = {Self,[{large_heap,Size}]},
    ?line OldMonitor = erlang:system_monitor(NewMonitor),
    ?line Pid = LoadFun(Size),
    ?line Ref = erlang:trace_delivered(Pid),
    ?line receive {trace_delivered, Pid, Ref} -> ok end,
    ?line {Self,[{large_heap,Size}]} = erlang:system_monitor(OldMonitor),
    ?line case {large_heap_check(Pid, Size, undefined), ExpectMonMsg} of
	      {ok, true} when Pid =/= Self ->
		  ?line ok;
	      {ok, false} ->
		  ?line ?t:fail(unexpected_system_monitor_message_received);
	      {undefined, false} ->
		  ?line ok;
	      {undefined, true} ->
		  ?line ?t:fail(no_system_monitor_message_received)
	  end,
    %%
    ?line test_server:timetrap_cancel(Dog),
    ok.

large_heap_check(Pid, Size, Result) ->
    receive
	{monitor,Pid,large_heap,L} = Monitor ->
	    case lists:foldl(
		   fun (_, error) ->
			   error;
		       ({heap_size,_}, N) ->
			   N-1;
		       ({old_heap_size,_}, N) ->
			   N-1;
		       ({stack_size,_}, N) ->
			   N-1;
		       ({mbuf_size,_}, N) ->
			   N-1;
		       ({heap_block_size,_}, N) ->
			   N-1;
		       ({old_heap_block_size,_}, N) ->
			   N-1;
		       (_, _) ->
			   error
		   end, 6, L) of
		0 ->
		    large_heap_check(Pid, Size, ok);
		error ->
		    {error,Monitor}
	    end;
	{monitor,_,large_heap,_} ->
	    large_heap_check(Pid, Size, Result);
	Other ->
	    {error,Other}
    after 0 ->
	    Result
    end.

seq(N, M) ->
    seq(N, M, []).

seq(M, M, R) ->
    lists:reverse(R);
seq(N, M, R) ->
    seq(N+1, M, [N|R]).


is_send_traced(Pid, Listener, Msg) ->
    Pid ! {send_please, Listener, Msg},
    receive
	Any ->
	    {trace, Pid, send, Msg, Listener} = Any,
	    true
    after 1000 ->
	    false
    end.

%% This procedure assumes that the Parent process is send traced.

spawn_children(Parent, Number) ->
    spawn_children(Parent, Number, []).

spawn_children(_Parent, 0, Result) ->
    lists:reverse(Result);
spawn_children(Parent, Number, Result) ->
    Self = self(),
    Parent ! {spawn_please, Self, fun process/0},
    Child = 
	receive
	    {trace, Parent, send, {spawned, Pid}, Self} -> Pid
	end,
    receive
	{spawned, Child} ->
	    spawn_children(Parent, Number-1, [Child|Result])
    end.

suspend(doc) -> "Test erlang:suspend/1 and erlang:resume/1.";
suspend(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),

    ?line Worker = fun_spawn(fun worker/0),
    %% Suspend a process and test that it is suspended.
    ?line ok = do_suspend(Worker, 10000),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.

do_suspend(_Pid, 0) ->
    ?line ok;
do_suspend(Pid, N) ->
    %% Suspend a process and test that it is suspended.
    ?line true = erlang:suspend_process(Pid),
    ?line {status, suspended} = process_info(Pid, status),

    %% Unsuspend the process and make sure it starts working.
    ?line true = erlang:resume_process(Pid),
    ?line case process_info(Pid, status) of
	      {status, runnable} -> ?line ok;
	      {status, running} -> ?line ok;
	      {status, garbage_collecting} -> ?line ok;
	      ST -> ?line ?t:fail(ST)
	  end,
    ?line erlang:yield(),
    ?line do_suspend(Pid, N-1).



mutual_suspend(doc) ->
    [];
mutual_suspend(suite) ->
    [];
mutual_suspend(Config) when is_list(Config) ->
    ?line TimeoutSecs = 5*60,
    ?line Dog = test_server:timetrap(test_server:minutes(TimeoutSecs)),
    ?line Parent = self(),
    ?line Fun = fun () ->
			receive
			    {go, Pid} ->
				do_mutual_suspend(Pid, 100000)
			end,
			Parent ! {done, self()},
			receive after infinity -> ok end
		end,
    ?line P1 = spawn_link(Fun),
    ?line P2 = spawn_link(Fun),
    ?line T1 = erlang:start_timer((TimeoutSecs - 5)*1000, self(), oops),
    ?line T2 = erlang:start_timer((TimeoutSecs - 5)*1000, self(), oops),
    ?line P1 ! {go, P2},
    ?line P2 ! {go, P1},
    ?line Res1 = receive
		    {done, P1} -> done;
		    {timeout,T1,_} -> timeout
		end,
    ?line Res2 = receive
		     {done, P2} -> done;
		     {timeout,T2,_} -> timeout
		 end,
    ?line P1S = process_info(P1, status),
    ?line P2S = process_info(P2, status),
    ?line ?t:format("P1S=~p P2S=~p", [P1S, P2S]),
    ?line false = {status, suspended} == P1S,
    ?line false = {status, suspended} == P2S,
    ?line unlink(P1), exit(P1, bang),
    ?line unlink(P2), exit(P2, bang),
    ?line done = Res1,
    ?line done = Res2,
    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ?line ok.
    
do_mutual_suspend(_Pid, 0) ->
    ?line ok;
do_mutual_suspend(Pid, N) ->
    %% Suspend a process and test that it is suspended.
    ?line true = erlang:suspend_process(Pid),
    ?line {status, suspended} = process_info(Pid, status),
    %% Unsuspend the process.
    ?line true = erlang:resume_process(Pid),
    ?line do_mutual_suspend(Pid, N-1).		

suspend_exit(doc) ->
    [];
suspend_exit(suite) ->
    [];
suspend_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),
    ?line random:seed(4711,17,4711),
    ?line do_suspend_exit(5000),
    ?line test_server:timetrap_cancel(Dog),
    ?line ok.

do_suspend_exit(0) ->
    ?line ok;
do_suspend_exit(N) ->
    ?line Work = random:uniform(50),
    ?line Parent = self(),
    ?line {Suspendee, Mon2}
	= spawn_monitor(fun () ->
				suspend_exit_work(Work),
				exit(normal)
			end),
    ?line {Suspender, Mon1}
	= spawn_monitor(
	    fun () ->
		    suspend_exit_work(Work div 2),
		    Parent ! {doing_suspend, self()},
		    case catch erlang:suspend_process(Suspendee) of
			{'EXIT', _} ->
			    ok;
			true ->
			    ?line erlang:resume_process(Suspendee)
		    end
	    end),
    ?line receive
	      {doing_suspend, Suspender} ->
		  case N rem 2 of
		      0 -> exit(Suspender, bang);
		      1 -> ok
		  end
	  end,
    ?line receive {'DOWN', Mon1, process, Suspender, _} -> ok end,
    ?line receive {'DOWN', Mon2, process, Suspendee, _} -> ok end,
    ?line do_suspend_exit(N-1).
				 


 
suspend_exit_work(0) ->
    ok;
suspend_exit_work(N) ->
    process_info(self()),
    suspend_exit_work(N-1).

-define(CHK_SUSPENDED(P,B), chk_suspended(P, B, ?LINE)).

chk_suspended(P, Bool, Line) ->
    {Bool, Line} = {({status, suspended} == process_info(P, status)), Line}.

suspender_exit(doc) ->
    [];
suspender_exit(suite) ->
    [];
suspender_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(3)),
    ?line P1 = spawn_link(fun () -> receive after infinity -> ok end end),
    ?line {'EXIT', _} = (catch erlang:resume_process(P1)),
    ?line {P2, M2} = spawn_monitor(
		       fun () ->
			       ?CHK_SUSPENDED(P1, false),
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       erlang:suspend_process(P1),
			       erlang:suspend_process(P1),
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       erlang:resume_process(P1),
			       erlang:resume_process(P1),
			       erlang:resume_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       erlang:resume_process(P1),
			       ?CHK_SUSPENDED(P1, false),
			       erlang:suspend_process(P1),
			       erlang:suspend_process(P1),
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       exit(bang)
		 end),
    ?line receive
	      {'DOWN', M2,process,P2,R2} ->
		  ?line bang = R2,
		  ?line ?CHK_SUSPENDED(P1, false)
	  end,
    ?line Parent = self(),
    ?line {P3, M3} = spawn_monitor(
		       fun () ->
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       Parent ! self(),
			       receive after infinity -> ok end
		       end),
    ?line {P4, M4} = spawn_monitor(
		       fun () ->
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       Parent ! self(),
			       receive after infinity -> ok end
		       end),
    ?line {P5, M5} = spawn_monitor(
		       fun () ->
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       Parent ! self(),
			       receive after infinity -> ok end
		       end),
    ?line {P6, M6} = spawn_monitor(
		       fun () ->
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       Parent ! self(),
			       receive after infinity -> ok end
		       end),
    ?line {P7, M7} = spawn_monitor(
		       fun () ->
			       erlang:suspend_process(P1),
			       ?CHK_SUSPENDED(P1, true),
			       Parent ! self(),
			       receive after infinity -> ok end
		       end),
    ?line receive P3 -> ok end,
    ?line receive P4 -> ok end,
    ?line receive P5 -> ok end,
    ?line receive P6 -> ok end,
    ?line receive P7 -> ok end,
    ?line ?CHK_SUSPENDED(P1, true),
    ?line exit(P3, bang),
    ?line receive
	      {'DOWN',M3,process,P3,R3} ->
		  ?line bang = R3,
		  ?line ?CHK_SUSPENDED(P1, true)
	  end,
    ?line exit(P4, bang),
    ?line receive
	      {'DOWN',M4,process,P4,R4} ->
		  ?line bang = R4,
		  ?line ?CHK_SUSPENDED(P1, true)
	  end,
    ?line exit(P5, bang),
    ?line receive
	      {'DOWN',M5,process,P5,R5} ->
		  ?line bang = R5,
		  ?line ?CHK_SUSPENDED(P1, true)
	  end,
    ?line exit(P6, bang),
    ?line receive
	      {'DOWN',M6,process,P6,R6} ->
		  ?line bang = R6,
		  ?line ?CHK_SUSPENDED(P1, true)
	  end,
    ?line exit(P7, bang),
    ?line receive
	      {'DOWN',M7,process,P7,R7} ->
		  ?line bang = R7,
		  ?line ?CHK_SUSPENDED(P1, false)
	  end,
    ?line unlink(P1),
    ?line exit(P1, bong),
    ?line test_server:timetrap_cancel(Dog),
    ?line ok.
			 
suspend_system_limit(doc) ->			 
    [];
suspend_system_limit(suite) ->
    [];
suspend_system_limit(Config) when is_list(Config) ->
    case os:getenv("ERL_EXTREME_TESTING") of
	"true" ->
	    ?line Dog = test_server:timetrap(test_server:minutes(3*60)),
	    ?line P = spawn_link(fun () -> receive after infinity -> ok end end),
	    ?line suspend_until_system_limit(P),
	    ?line unlink(P),
	    ?line exit(P, bye),
	    ?line test_server:timetrap_cancel(Dog),
	    ?line ok;
	_ ->
	    {skip, "Takes too long time for normal testing"}
    end.

suspend_until_system_limit(P) ->
    ?line suspend_until_system_limit(P, 0, 0).

suspend_until_system_limit(P, N, M) ->
    NewM = case M of
	       1 ->
		   ?line ?CHK_SUSPENDED(P, true), 2;
	       1000000 ->
		   erlang:display(N), 1;
	       _ ->
		   M+1
	   end,
    ?line case catch erlang:suspend_process(P) of
	      true ->
		  suspend_until_system_limit(P, N+1, NewM);
	      {'EXIT', R} when R == system_limit;
			       element(1, R) == system_limit ->
		  ?line ?t:format("system limit at ~p~n", [N]),
		  ?line resume_from_system_limit(P, N, 0);
	      Error ->
		  ?line ?t:fail(Error)
	  end.

resume_from_system_limit(P, 0, _) ->
    ?line ?CHK_SUSPENDED(P, false),
    ?line {'EXIT', _} = (catch erlang:resume_process(P)),
    ?line ok;
resume_from_system_limit(P, N, M) ->
    ?line NewM = case M of
		     1 ->
			 ?line ?CHK_SUSPENDED(P, true), 2;
		     1000000 ->
			 erlang:display(N), 1;
		     _ ->
			 M+1
		 end,
    ?line erlang:resume_process(P),
    ?line resume_from_system_limit(P, N-1, NewM).

-record(susp_info, {async = 0,
		    dbl_async = 0,
		    synced = 0,
		    async_once = 0}).

suspend_opts(doc) ->
    [];
suspend_opts(suite) ->
    [];
suspend_opts(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(3)),
    ?line Self = self(),
    ?line wait_for_empty_runq(10),
    ?line Tok = spawn_link(fun () ->
				   Self ! self(),
				   tok_trace_loop(Self, 0, 1000000000)
			   end),
    ?line TC = 1000,
    ?line receive Tok -> ok end,
    ?line SF = fun (N, #susp_info {async = A,
				   dbl_async = AA,
				   synced = S,
				   async_once = AO} = Acc) ->
		       ?line erlang:suspend_process(Tok, [asynchronous]),
		       ?line Res = case {suspend_count(Tok), N rem 4} of
				       {0, 2} ->
					   ?line erlang:suspend_process(Tok,
									[asynchronous]),
					   case suspend_count(Tok) of
					       2 ->
						   ?line erlang:resume_process(Tok),
						   ?line Acc#susp_info{async = A+1};
					       0 ->
						   ?line erlang:resume_process(Tok),
						   ?line Acc#susp_info{async = A+1,
								       dbl_async = AA+1}
					   end;
				       {0, 1} ->
					   ?line erlang:suspend_process(Tok,
									[asynchronous,
									 unless_suspending]),
					   case suspend_count(Tok) of
					       1 ->
						   ?line Acc#susp_info{async = A+1};
					       0 ->
						   ?line Acc#susp_info{async = A+1,
								       async_once = AO+1}
					   end;
				       {0, 0} ->
					   ?line erlang:suspend_process(Tok,
									[unless_suspending]),
					   ?line 1 = suspend_count(Tok),
					   ?line Acc#susp_info{async = A+1,
							       synced = S+1};
				       {0, _} ->
					   ?line Acc#susp_info{async = A+1};
				       _ ->
					   Acc
				   end,
		       ?line erlang:resume_process(Tok),
		       ?line erlang:yield(),
		       ?line Res
	       end,
    ?line SI = repeat_acc(SF, TC, #susp_info{}),
    ?line erlang:suspend_process(Tok, [asynchronous]),
    %% Verify that it eventually suspends
    ?line WaitTime0 = 10,
    ?line WaitTime1 = case {erlang:system_info(debug_compiled),
			    erlang:system_info(lock_checking)} of
			  {false, false} ->
			      WaitTime0;
			  {false, true} ->
			      WaitTime0*5;
			  _ ->
			      WaitTime0*10
		      end,
    ?line WaitTime = case {erlang:system_info(schedulers_online),
			   erlang:system_info(logical_processors)} of
			 {Schdlrs, CPUs} when is_integer(CPUs),
					      Schdlrs =< CPUs ->
			     WaitTime1;
			 _ ->
			     WaitTime1*10
		     end,
    ?line receive after WaitTime -> ok end,
    ?line 1 = suspend_count(Tok),
    ?line erlang:suspend_process(Tok, [asynchronous]),
    ?line 2 = suspend_count(Tok),
    ?line erlang:suspend_process(Tok, [asynchronous]),
    ?line 3 = suspend_count(Tok),
    ?line erlang:suspend_process(Tok),
    ?line 4 = suspend_count(Tok),
    ?line erlang:suspend_process(Tok),
    ?line 5 = suspend_count(Tok),
    ?line erlang:suspend_process(Tok, [unless_suspending]),
    ?line 5 = suspend_count(Tok),
    ?line erlang:suspend_process(Tok, [unless_suspending,
				       asynchronous]),
    ?line 5 = suspend_count(Tok),
    ?line erlang:resume_process(Tok),
    ?line erlang:resume_process(Tok),
    ?line erlang:resume_process(Tok),
    ?line erlang:resume_process(Tok),
    ?line 1 = suspend_count(Tok),
    ?line ?t:format("Main suspends: ~p~n"
		    "Main async: ~p~n"
		    "Double async: ~p~n"
		    "Async once: ~p~n"
		    "Synced: ~p~n",
		    [TC,
		     SI#susp_info.async,
		     SI#susp_info.dbl_async,
		     SI#susp_info.async_once,
		     SI#susp_info.synced]),
    ?line case erlang:system_info(schedulers_online) of
	      1 ->
		  ?line ok;
	      _ ->
		  ?line true = SI#susp_info.async =/= 0
	  end,
    ?line unlink(Tok),
    ?line exit(Tok, bang),
    ?line test_server:timetrap_cancel(Dog),
    ?line ok.

suspend_count(Suspendee) ->
    suspend_count(self(), Suspendee).

suspend_count(Suspender, Suspendee) ->
    {suspending, SList} = process_info(Suspender, suspending),
    
    case lists:keysearch(Suspendee, 1, SList) of
	{value, {_Suspendee, 0, 0}} ->
	    ?line ?t:fail({bad_suspendee_list, SList});
	{value, {Suspendee, Count, 0}} when is_integer(Count), Count > 0 ->
	    {status, suspended} = process_info(Suspendee, status),
	    Count;
	{value, {Suspendee, 0, Outstanding}} when is_integer(Outstanding),
	                                          Outstanding > 0 ->
	    0;
	false ->
	    0;
	Error ->
	    ?line ?t:fail({bad_suspendee_list, Error, SList})
    end.
    
repeat_acc(Fun, N, Acc) ->
    repeat_acc(Fun, 0, N, Acc).

repeat_acc(_Fun, N, N, Acc) ->
    Acc;
repeat_acc(Fun, N, M, Acc) ->
    repeat_acc(Fun, N+1, M, Fun(N, Acc)).
		   
%% Tests that waiting process can be suspended
%% (bug in R2D and earlier; see OTP-1488).

suspend_waiting(doc) -> "Test that a waiting process can be suspended.";
suspend_waiting(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),

    ?line Process = fun_spawn(fun process/0),
    ?line receive after 1 -> ok end,
    ?line true = erlang:suspend_process(Process),
    ?line {status, suspended} = process_info(Process, status),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.



new_clear(doc) ->
    "Test that erlang:trace(new, true, ...) is cleared when tracer dies.";
new_clear(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),

    ?line Tracer = spawn(fun receiver/0),
    ?line 0 = erlang:trace(new, true, [send, {tracer, Tracer}]),
    ?line {flags, [send]} = erlang:trace_info(new, flags),
    ?line {tracer, Tracer} = erlang:trace_info(new, tracer),
    ?line Mref = erlang:monitor(process, Tracer),
    ?line true = exit(Tracer, done),
    receive
	{'DOWN',Mref,_,_,_} -> ok
    end,
    ?line {flags, []} = erlang:trace_info(new, flags),
    ?line {tracer, []} = erlang:trace_info(new, tracer),

    %% Done.
    ?line test_server:timetrap_cancel(Dog),

    ok.



existing_clear(doc) ->
    "Test that erlang:trace(all, false, ...) works without tracer.";
existing_clear(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(5)),
    ?line Self = self(),

    ?line Tracer = fun_spawn(fun receiver/0),
    ?line N = erlang:trace(existing, true, [send, {tracer, Tracer}]),
    ?line {flags, [send]} = erlang:trace_info(Self, flags),
    ?line {tracer, Tracer} = erlang:trace_info(Self, tracer),
    ?line M = erlang:trace(all, false, [all]),
    ?line io:format("Started trace on ~p processes and stopped on ~p~n", 
		    [N, M]),
    ?line {flags, []} = erlang:trace_info(Self, flags),
    ?line {tracer, []} = erlang:trace_info(Self, tracer),
    ?line M = N + 1, % Since trace could not be enabled on the tracer.

    %% Done.
    ?line test_server:timetrap_cancel(Dog),
    ok.

bad_flag(doc) -> "Test that an invalid flag cause badarg";
bad_flag(suite) -> [];
bad_flag(Config) when is_list(Config) ->
    %% A bad flag could deadlock the SMP emulator in erts-5.5
    ?line {'EXIT', {badarg, _}} = (catch erlang:trace(new,
						      true,
						      [not_a_valid_flag])),
    ?line ok.

trace_delivered(doc) -> "Test erlang:trace_delivered/1";
trace_delivered(suite) -> [];
trace_delivered(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line TokLoops = 10000,
    ?line Go = make_ref(),
    ?line Parent = self(),
    ?line Tok = spawn(fun () ->
			      receive Go -> gone end,
			      tok_trace_loop(Parent, 0, TokLoops)
		      end),
    ?line 1 = erlang:trace(Tok, true, [procs]),
    ?line Mon = erlang:monitor(process, Tok),
    ?line NoOfTraceMessages = 4*TokLoops + 1,
    ?line io:format("Expect a total of ~p trace messages~n",
		    [NoOfTraceMessages]),
    ?line Tok ! Go,
    ?line NoOfTraceMessages = drop_trace_until_down(Tok, Mon),
    ?line receive
	      Msg ->
		  ?line ?t:fail({unexpected_message, Msg})
	  after 1000 ->
		  ?line test_server:timetrap_cancel(Dog),
		  ?line ok
	  end.

drop_trace_until_down(Proc, Mon) ->
    drop_trace_until_down(Proc, Mon, false, 0, 0).

drop_trace_until_down(Proc, Mon, TDRef, N, D) ->
    case receive Msg -> Msg end of
	{trace_delivered, Proc, TDRef} ->
	    io:format("~p trace messages on 'DOWN'~n", [D]),
	    io:format("Got a total of ~p trace messages~n", [N]),
	    N;
	{'DOWN', Mon, process, Proc, _} ->
	    Ref = erlang:trace_delivered(Proc),
	    drop_trace_until_down(Proc, Mon, Ref, N, N);
	Trace when is_tuple(Trace),
		   element(1, Trace) == trace,
		   element(2, Trace) == Proc ->
	    drop_trace_until_down(Proc, Mon, TDRef, N+1, D)
    end.

tok_trace_loop(_, N, N) ->
    ok;
tok_trace_loop(Parent, N, M) ->
    Name = 'A really stupid name which I will unregister at once',
    link(Parent),
    register(Name, self()),
    unregister(Name),
    unlink(Parent),
    tok_trace_loop(Parent, N+1, M).

%% Waits for and returns the first message in the message queue.

receive_first() ->
    receive
	Any -> Any
    end.

%% Ensures that there is no message in the message queue.

receive_nothing() ->
    receive
	Any ->
	    test_server:fail({unexpected_message, Any})
    after 200 ->
	    ok
    end.


%%% Models for various kinds of processes.

process(Dest) ->
    receive
	{send_please, To, What} ->
	    To ! What,
	    process(Dest);
	{spawn_link_please, ReplyTo, {M, F, A}} ->
	    Pid = spawn_link(M, F, A),
	    ReplyTo ! {spawned, self(), Pid},
	    process(Dest);
	{spawn_link_please, ReplyTo, Node, {M, F, A}} ->
	    Pid = spawn_link(Node, M, F, A),
	    ReplyTo ! {spawned, self(), Pid},
	    process(Dest);
	{link_please, Pid} ->
	    link(Pid),
	    process(Dest);
	{unlink_please, Pid} ->
	    unlink(Pid),
	    process(Dest);
	{register_please, Name, Pid} ->
	    register(Name, Pid),
	    process(Dest);
	{unregister_please, Name} ->
	    unregister(Name),
	    process(Dest);
	{exit_please, Reason} ->
	    exit(Reason);
	{trap_exit_please, State} ->
	    process_flag(trap_exit, State),
	    process(Dest);
	Other ->
	    Dest ! {self(), Other},
	    process(Dest)
    after 3000 ->
	    exit(timeout)
    end.


%% A smart process template.

process() ->
    receive
	{spawn_please, ReplyTo, Fun} ->
	    Pid = fun_spawn(Fun),
	    ReplyTo ! {spawned, Pid},
	    process();
	{send_please, To, What} ->
	    To ! What,
	    process();
	timeout_please ->
	    receive after 1 -> process() end;
	_Other ->
	    process()
    end.


%% Sends messages when ordered to.

sender() ->
    receive
	{send_please, To, What} ->
	    To ! What,
	    sender()
    end.


%% Just consumes messages from its message queue.

receiver() ->
    receive
	_Any -> receiver()
    end.

%% Works as long as it receives CPU time.  Will always be RUNNABLE.

worker() ->
    worker(0).

worker(Number) ->
    worker(Number+1).

fun_spawn(Fun) ->
    spawn_link(erlang, apply, [Fun, []]).

fun_spawn(Fun, Args) ->
    spawn_link(erlang, apply, [Fun, Args]).


start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = atom_to_list(erlang:get_cookie()),
    test_server:start_node(Name, slave, 
			   [{args, "-setcookie " ++ Cookie ++" -pa " ++ Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).


wait_for_empty_runq(DeadLine) ->
    case statistics(run_queue) of
	0 -> true;
	RQLen ->
	    erlang:display("Waiting for empty run queue"),
	    MSDL = DeadLine*1000,
	    wait_for_empty_runq(MSDL, MSDL, RQLen)
    end.

wait_for_empty_runq(DeadLine, Left, RQLen) when Left =< 0 ->
    issue_non_empty_runq_warning(DeadLine, RQLen),
    false;
wait_for_empty_runq(DeadLine, Left, _RQLen) ->
    Wait = 10,
    UntilDeadLine = Left - Wait,
    receive after Wait -> ok end,
    case statistics(run_queue) of
	0 ->
	    erlang:display("Waited for "
			   ++ integer_to_list(DeadLine
					      - UntilDeadLine)
			   ++ " ms for empty run queue."),
	    true;
	NewRQLen ->
	    wait_for_empty_runq(DeadLine,
				UntilDeadLine,
				NewRQLen)
    end.

issue_non_empty_runq_warning(DeadLine, RQLen) ->
    PIs = lists:foldl(
	    fun (P, Acc) ->
		    case process_info(P,
				      [status,
				       initial_call,
				       current_function,
				       registered_name,
				       reductions,
				       message_queue_len]) of
			[{status, Runnable} | _] = PI when Runnable /= waiting,
							   Runnable /= suspended ->
			    [[{pid, P} | PI] | Acc];
			_ ->
			    Acc
		    end
	    end,
	    [],
	    processes()),
    ?t:format("WARNING: Unexpected runnable processes in system (waited ~p sec).~n"
	      "         Run queue length: ~p~n"
	      "         Self: ~p~n"
	      "         Processes info: ~p~n",
	      [DeadLine div 1000, RQLen, self(), PIs]),
    receive after 1000 -> ok end.

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.
