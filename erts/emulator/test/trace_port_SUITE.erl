%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2012. All Rights Reserved.
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


-module(trace_port_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 call_trace/1,
	 return_trace/1,
	 send/1,
	 receive_trace/1,
	 process_events/1,
	 schedule/1,
	 fake_schedule/1,
	 fake_schedule_after_register/1,
	 fake_schedule_after_getting_linked/1,
	 fake_schedule_after_getting_unlinked/1,
	 gc/1,
	 default_tracer/1,
	 tracer_port_crash/1]).

-include_lib("test_server/include/test_server.hrl").

test_cases() -> 
    [call_trace, return_trace, send, receive_trace,
     process_events, schedule, fake_schedule,
     fake_schedule_after_register,
     fake_schedule_after_getting_linked,
     fake_schedule_after_getting_unlinked, gc,
     default_tracer, tracer_port_crash].

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_cases().

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:seconds(30)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

call_trace(doc) -> "Test sending call trace messages to a port.";
call_trace(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) orelse
	test_server:is_native(lists) of
	true -> 
	    {skip,"Native code"};
	false ->
	    ?line start_tracer(Config),
	    Self = self(),
	    ?line trace_func({lists,reverse,1}, []),
	    ?line trace_pid(Self, true, [call]),
	    ?line trace_info(Self, flags),
	    ?line trace_info(Self, tracer),
	    ?line [b,a] = lists:reverse([a,b]),
	    ?line expect({trace,Self,call,{lists,reverse,[[a,b]]}}),
	    
	    ?line trace_pid(Self, true, [timestamp]),
	    ?line trace_info(Self, flags),
	    ?line Huge = huge_data(),
	    ?line lists:reverse(Huge),
	    ?line expect({trace_ts,Self,call,{lists,reverse,[Huge]},ts}),
	    
	    ?line trace_pid(Self, true, [arity]),
	    ?line trace_info(Self, flags),
	    ?line [y,x] = lists:reverse([x,y]),
	    ?line expect({trace_ts,Self,call,{lists,reverse,1},ts}),
	    
	    ?line trace_pid(Self, false, [timestamp]),
	    ?line trace_info(Self, flags),
	    ?line [z,y,x] = lists:reverse([x,y,z]),
	    ?line expect({trace,Self,call,{lists,reverse,1}}),

	    %% OTP-7399. Delayed sub-binary creation optimization.
	    ?line trace_pid(Self, false, [arity]),
	    ?line trace_info(Self, flags),
	    ?line trace_func({?MODULE,bs_sum_c,2}, [], [local]),
	    ?line 26 = bs_sum_c(<<3:4,5:4,7:4,11:4>>, 0),
	    ?line trace_func({?MODULE,bs_sum_c,2}, false, [local]),
	    ?line expect({trace,Self,call,{?MODULE,bs_sum_c,[<<3:4,5:4,7:4,11:4>>,0]}}),
	    ?line expect({trace,Self,call,{?MODULE,bs_sum_c,[<<5:4,7:4,11:4>>,3]}}),
	    ?line expect({trace,Self,call,{?MODULE,bs_sum_c,[<<7:4,11:4>>,8]}}),
	    ?line expect({trace,Self,call,{?MODULE,bs_sum_c,[<<11:4>>,15]}}),
	    ?line expect({trace,Self,call,{?MODULE,bs_sum_c,[<<>>,26]}}),
	    
	    ?line trace_func({lists,reverse,1}, false),
	    ok
    end.

bs_sum_c(<<H:4,T/bits>>, Acc) -> bs_sum_c(T, H+Acc);
bs_sum_c(<<>>, Acc) -> Acc.


return_trace(doc) -> "Test the new return trace.";
return_trace(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) orelse
	test_server:is_native(lists) of
	true -> 
	    {skip,"Native code"};
	false ->
	    ?line start_tracer(Config),
	    Self = self(),
	    MFA = {lists,reverse,1},
	    
	    %% Plain (no timestamp, small data).
	    
	    ?line trace_func(MFA, [{['$1'],[],[{return_trace},
					       {message,false}]}]),
	    ?line trace_pid(Self, true, [call]),
	    ?line trace_info(Self, flags),
	    ?line trace_info(Self, tracer),
	    ?line trace_info(MFA, match_spec),
	    ?line {traced,global} = trace_info(MFA, traced),
	    ?line [b,a] = lists:reverse([a,b]),
	    ?line expect({trace,Self,return_from,MFA,[b,a]}),
	    
	    %% Timestamp, huge data.
	    ?line trace_pid(Self, true, [timestamp]),
	    ?line Result = lists:reverse(huge_data()),
	    ?line expect({trace_ts,Self,return_from,MFA,Result,ts}),
	    
	    %% Turn off trace.
	    ?line trace_func(MFA, false),
	    ?line trace_info(MFA, match_spec),
	    ?line {traced,false} = trace_info(MFA, traced),
	    ok
    end.

send(doc) -> "Test sending send trace messages to a port.";
send(Config) when is_list(Config) ->
    ?line Tracer = start_tracer(Config),
    Self = self(),
    ?line Sender = fun_spawn(fun sender/0),
    ?line trac(Sender, true, [send]),

    %% Simple message, no timestamp.

    ?line Bin = list_to_binary(lists:seq(1, 10)),
    ?line Msg = {some_data,Bin},
    Sender ! {send_please,self(),Msg},
    receive Msg -> ok end,
    ?line expect({trace,Sender,send,Msg,Self}),

    %% Timestamp.

    BiggerMsg = {even_bigger,Msg},
    ?line trac(Sender, true, [send,timestamp]),
    Sender ! {send_please,self(),BiggerMsg},
    receive BiggerMsg -> ok end,
    ?line expect({trace_ts,Sender,send,BiggerMsg,Self,ts}),

    %% Huge message.

    ?line HugeMsg = huge_data(),
    Sender ! {send_please,self(),HugeMsg},
    receive HugeMsg -> ok end,
    ?line expect({trace_ts,Sender,send,HugeMsg,Self,ts}),

    %% Kill trace port and force a trace.  The emulator should not crasch.

    ?line unlink(Tracer),
    ?line exit(Tracer, kill),
    erlang:yield(),				% Make sure that port gets killed.
    Sender ! {send_please,Self,good_bye},
    receive good_bye -> ok end,
    ok.

receive_trace(doc) -> "Test sending receive traces to a port.";
receive_trace(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    ?line Receiver = fun_spawn(fun receiver/0),
    ?line trac(Receiver, true, ['receive']),

    Receiver ! {hello,world},
    ?line expect({trace,Receiver,'receive',{hello,world}}),

    ?line trac(Receiver, true, ['receive',timestamp]),
    Huge = {hello,huge_data()},
    Receiver ! {hello,huge_data()},
    ?line expect({trace_ts,Receiver,'receive',Huge,ts}),
    ok.

process_events(doc) -> "Tests a few process events (like getting linked).";
process_events(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    Self = self(),
    ?line Receiver = fun_spawn(fun receiver/0),
    ?line trac(Receiver, true, [procs]),

    unlink(Receiver),				%It is already linked.
    ?line expect({trace,Receiver,getting_unlinked,Self}),
    link(Receiver),
    ?line expect({trace,Receiver,getting_linked,Self}),
    ?line trac(Receiver, true, [procs,timestamp]),
    unlink(Receiver),
    ?line expect({trace_ts,Receiver,getting_unlinked,Self,ts}),
    link(Receiver),
    ?line expect({trace_ts,Receiver,getting_linked,Self,ts}),

    unlink(Receiver),
    ?line expect({trace_ts,Receiver,getting_unlinked,Self,ts}),
    Huge = huge_data(),
    exit(Receiver, Huge),
    ?line expect({trace_ts,Receiver,exit,Huge,ts}),

    ok.

schedule(doc) -> "Test sending scheduling events to a port.";
schedule(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    ?line Receiver = fun_spawn(fun receiver/0),
    ?line trac(Receiver, true, [running]),

    Receiver ! hi,
    expect({trace,Receiver,in,{?MODULE,receiver,0}}),
    expect({trace,Receiver,out,{?MODULE,receiver,0}}),

    ?line trac(Receiver, true, [running,timestamp]),

    Receiver ! hi_again,
    expect({trace_ts,Receiver,in,{?MODULE,receiver,0},ts}),
    expect({trace_ts,Receiver,out,{?MODULE,receiver,0},ts}),

    ok.

run_fake_sched_test(Fun, Config) when is_function(Fun), is_list(Config) ->
    ?line case catch erlang:system_info(smp_support) of
	      true ->
		  ?line {skipped,
			 "No need for faked schedule out/in trace messages "
			 "when smp support is enabled"};
	      _ ->
		  ?line Fun(Config)
	  end.

fake_schedule(doc) -> "Tests time compensating fake out/in scheduling.";
fake_schedule(Config) when is_list(Config) ->
    ?line run_fake_sched_test(fun fake_schedule_test/1, Config).

fake_schedule_test(Config) when is_list(Config) ->
    ?line Tracer = start_tracer(Config),
    ?line Port = get(tracer_port),
    ?line General = fun_spawn(fun general/0),
    %%
    ?line trac(General, true, [send, running]),
    %%
    %% Test that fake out/in scheduling is not generated unless
    %% both 'running' and 'timestamp' is active.
    ?line [] = erlang:port_control(Port, $h, []),
    ?line General ! nop,
    ?line expect({trace, General, in, {?MODULE, general, 0}}),
    ?line expect({trace, General, out, {?MODULE, general, 0}}),
    ?line expect(),
    %%
    ?line trac(General, false, [running]),
    ?line trac(General, true, [timestamp]),
    %%
    ?line Ref1 = make_ref(),
    ?line Msg1 = {Port, {data, term_to_binary(Ref1)}},
    ?line [] = erlang:port_control(Port, $h, []),
    ?line General ! {send, Tracer, Msg1},
    ?line expect({trace_ts, General, send, Msg1, Tracer, ts}),
    ?line expect(Ref1),
    ?line expect(),
    %%
    ?line trac(General, true, [running]),
    %%
    %% Test that fake out/in scheduling can be generated by the driver
    ?line Ref2 = make_ref(),
    ?line Msg2 = {Port, {data, term_to_binary(Ref2)}},
    ?line [] = erlang:port_control(Port, $h, []),
    ?line General ! {send, Tracer, Msg2},
    ?line {_,_,_,_,Ts} = 
	expect({trace_ts, General, in, {?MODULE, general, 0}, ts}),
    ?line expect({trace_ts, General, out, 0, Ts}),
    ?line expect({trace_ts, General, in, 0, ts}),
    ?line expect({trace_ts, General, send, Msg2, Tracer, ts}),
    ?line expect(Ref2),
    ?line expect({trace_ts, General, out, {?MODULE, general, 0}, ts}),
    ?line expect(),
    %%
    %% Test that fake out/in scheduling is not generated after an
    %% 'out' scheduling event
    ?line Ref3 = make_ref(),
    ?line Msg3 = {Port, {data, term_to_binary(Ref3)}},
    ?line General ! {apply, {erlang, port_control, [Port, $h, []]}},
    ?line expect({trace_ts, General, in, {?MODULE, general, 0}, ts}),
    ?line expect({trace_ts, General, out, {?MODULE, general, 0}, ts}),
    ?line General ! {send, Tracer, Msg3},
    ?line expect({trace_ts, General, in, {?MODULE, general, 0}, ts}),
    ?line expect({trace_ts, General, send, Msg3, Tracer, ts}),
    ?line expect(Ref3),
    ?line expect({trace_ts, General, out, {?MODULE, general, 0}, ts}),
    ?line expect(),
    %%
    ok.

fake_schedule_after_register(doc) -> 
    "Tests fake out/in scheduling contents.";
fake_schedule_after_register(Config) when is_list(Config) ->
    ?line run_fake_sched_test(fun fake_schedule_after_register_test/1, Config).

fake_schedule_after_register_test(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    ?line Port = get(tracer_port),
    ?line G1 = fun_spawn(fun general/0),
    ?line G2 = fun_spawn(fun general/0),
    %%
    ?line trac(G1, true, [running, timestamp, procs]),
    ?line trac(G2, true, [running, timestamp]),
    %%
    %% Test fake out/in scheduling after certain messages
    ?line erlang:yield(),
    ?line G2 ! {apply, {erlang, port_control, [Port, $h, []]}},
    ?line G2 ! {apply, {erlang, register, [fake_schedule_after_register, G1]}},
    ?line expect({trace_ts, G2, in, {?MODULE, general, 0}, ts}),
    ?line {_,_,_,_,Ts} = 
	expect({trace_ts, G1, register, fake_schedule_after_register, ts}),
    ?line expect({trace_ts, G2, out, 0, Ts}),
    ?line expect({trace_ts, G2, in, 0, ts}),
    ?line expect({trace_ts, G2, out, {?MODULE, general, 0}, ts}),
    ?line expect(),
    %%
    ok.

fake_schedule_after_getting_linked(doc) -> 
    "Tests fake out/in scheduling contents.";
fake_schedule_after_getting_linked(Config) when is_list(Config) ->
    ?line run_fake_sched_test(fun fake_schedule_after_getting_linked_test/1,
			      Config).

fake_schedule_after_getting_linked_test(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    ?line Port = get(tracer_port),
    ?line G1 = fun_spawn(fun general/0),
    ?line G2 = fun_spawn(fun general/0),
    %%
    ?line trac(G1, true, [running, timestamp, procs]),
    ?line trac(G2, true, [running, timestamp]),
    %%
    %% Test fake out/in scheduling after certain messages
    ?line erlang:yield(),
    ?line G2 ! {apply, {erlang, port_control, [Port, $h, []]}},
    ?line G2 ! {apply, {erlang, link, [G1]}},
    ?line expect({trace_ts, G2, in, {?MODULE, general, 0}, ts}),
    ?line {_,_,_,_,Ts} = 
	expect({trace_ts, G1, getting_linked, G2, ts}),
    ?line expect({trace_ts, G2, out, 0, Ts}),
    ?line expect({trace_ts, G2, in, 0, ts}),
    ?line expect({trace_ts, G2, out, {?MODULE, general, 0}, ts}),
    ?line expect(),
    %%
    ok.

fake_schedule_after_getting_unlinked(doc) -> 
    "Tests fake out/in scheduling contents.";
fake_schedule_after_getting_unlinked(Config) when is_list(Config) ->
    ?line run_fake_sched_test(fun fake_schedule_after_getting_unlinked_test/1,
			      Config).

fake_schedule_after_getting_unlinked_test(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    ?line Port = get(tracer_port),
    ?line G1 = fun_spawn(fun general/0),
    ?line G2 = fun_spawn(fun general/0),
    %%
    ?line trac(G1, true, [running, procs]),
    ?line trac(G2, true, [running, timestamp]),
    %%
    %% Test fake out/in scheduling after certain messages
    ?line erlang:yield(),
    ?line G2 ! {apply, {erlang, link, [G1]}},
    ?line G2 ! {apply, {erlang, port_control, [Port, $h, []]}},
    ?line G2 ! {apply, {erlang, unlink, [G1]}},
    ?line expect({trace_ts, G2, in, {?MODULE, general, 0}, ts}),
    ?line expect({trace, G1, getting_linked, G2}),
    ?line expect({trace, G1, getting_unlinked, G2}),
    ?line expect({trace_ts, G2, out, 0, ts}),
    ?line expect({trace_ts, G2, in, 0, ts}),
    ?line expect({trace_ts, G2, out, {?MODULE, general, 0}, ts}),
    ?line expect(),
    %%
    ok.

gc(doc) -> "Test sending garbage collection events to a port.";
gc(Config) when is_list(Config) ->
    ?line start_tracer(Config),
    ?line Garber = fun_spawn(fun garber/0, [{min_heap_size, 5000}]),
    ?line trac(Garber, true, [garbage_collection]),
    ?line trace_info(Garber, flags),

    ?line trace_info(Garber, flags),
    Garber ! hi,
    expect({trace,Garber,gc_start,info}),
    expect({trace,Garber,gc_end,info}),

    ?line trac(Garber, true, [garbage_collection,timestamp]),
    Garber ! hi,
    expect({trace_ts,Garber,gc_start,info,ts}),
    expect({trace_ts,Garber,gc_end,info,ts}),

    ok.

default_tracer(doc) ->
    "Test a port as default tracer.";
default_tracer(Config) when is_list(Config) ->
    ?line Tracer = start_tracer(Config),
    ?line TracerMonitor = erlang:monitor(process, Tracer),
    ?line Port = get(tracer_port),
    %%
    ?line N = erlang:trace(all, true, [send, {tracer, Port}]),
    ?line {flags, [send]} = erlang:trace_info(self(), flags),
    ?line {tracer, Port} = erlang:trace_info(self(), tracer),
    ?line {flags, [send]} = erlang:trace_info(new, flags),
    ?line {tracer, Port} = erlang:trace_info(new, tracer),
    ?line G1 = fun_spawn(fun general/0),
    ?line {flags, [send]} = erlang:trace_info(G1, flags),
    ?line {tracer, Port} = erlang:trace_info(G1, tracer),
    ?line unlink(Tracer),
    ?line exit(Port, done),
    ?line receive
	      {'DOWN', TracerMonitor, process, Tracer, TracerExitReason} ->
		  ?line done = TracerExitReason
	  end,
    ?line {flags, []} = erlang:trace_info(self(), flags),
    ?line {tracer, []} = erlang:trace_info(self(), tracer),
    ?line {flags, []} = erlang:trace_info(new, flags),
    ?line {tracer, []} = erlang:trace_info(new, tracer),
    ?line M = erlang:trace(all, false, [all]),
    ?line {flags, []} = erlang:trace_info(self(), flags),
    ?line {tracer, []} = erlang:trace_info(self(), tracer),
    ?line {flags, []} = erlang:trace_info(G1, flags),
    ?line {tracer, []} = erlang:trace_info(G1, tracer),
    ?line G1 ! {apply,{erlang,exit,[normal]}},
    ?line io:format("~p = ~p.~n", [M, N]),
    ?line M = N,
    ok.

tracer_port_crash(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) orelse
	test_server:is_native(lists) of
	true -> 
	    {skip,"Native code"};
	false ->
	    Tr = start_tracer(Config),
	    Port = get(tracer_port),
	    Tracee = spawn(fun () ->
				   register(trace_port_linker, self()),
				   link(Port),
				   receive go -> ok end,
				   lists:reverse([1,b,c]),
				   receive die -> ok end
			   end),
	    Tr ! {unlink_tracer_port, self()},
	    receive {unlinked_tracer_port, Tr} -> ok end,
	    port_control(Port, $c, []), %% Make port commands crash tracer port...
	    trace_func({lists,reverse,1}, []),
	    trace_pid(Tracee, true, [call]),
	    trace_info(Tracee, flags),
	    trace_info(self(), tracer),
	    Tracee ! go,
	    receive after 1000 -> ok end,
	    case whereis(trace_port_linker) of
		undefined ->
		    ok;
		Id ->
%		    erts_debug:set_internal_state(available_internal_state, true),
%		    erts_debug:set_internal_state(abort, {trace_port_linker, Id})
		    ?t:fail({trace_port_linker, Id})
	    end,
	    undefined = process_info(Tracee),
	    ok
    end.

%%% Help functions.

huge_data() -> huge_data(16384).
huge_data(0) -> [];
huge_data(N) when N rem 2 == 0 ->
    P = huge_data(N div 2),
    [P|P];
huge_data(N) ->
    P = huge_data(N div 2),
    [16#1234566,P|P].

expect() ->
    receive
	Other ->
	    ok = io:format("Unexpected; got ~p", [Other]),
	    test_server:fail({unexpected, Other})
    after 200 ->
	    ok
    end.

expect({trace_ts,E1,E2,info,ts}=Message) ->
    receive
	{trace_ts,E1,E2,_Info,_Ts}=MessageTs ->
	    ok = io:format("Expected and got ~p", [MessageTs]),
	    MessageTs;
	Other ->
	    io:format("Expected ~p; got ~p", [Message,Other]),
	    test_server:fail({unexpected,Other})
    after 5000 ->
	    io:format("Expected ~p; got nothing", [Message]),
	    test_server:fail(no_trace_message)
    end;
expect({trace,E1,E2,info}=Message) ->
    receive
	{trace,E1,E2,_Info}=MessageTs ->
	    ok = io:format("Expected and got ~p", [MessageTs]),
	    MessageTs;
	Other ->
	    io:format("Expected ~p; got ~p", [Message,Other]),
	    test_server:fail({unexpected,Other})
    after 5000 ->
	    io:format("Expected ~p; got nothing", [Message]),
	    test_server:fail(no_trace_message)
    end;
expect({trace_ts,E1,E2,E3,ts}=Message) ->
    receive
	{trace_ts,E1,E2,E3,_Ts}=MessageTs ->
	    ok = io:format("Expected and got ~p", [MessageTs]),
	    MessageTs;
	Other ->
	    io:format("Expected ~p; got ~p", [Message,Other]),
	    test_server:fail({unexpected,Other})
    after 5000 ->
	    io:format("Expected ~p; got nothing", [Message]),
	    test_server:fail(no_trace_message)
    end;
expect({trace_ts,E1,E2,E3,E4,ts}=Message) ->
    receive
	{trace_ts,E1,E2,E3,E4,_Ts}=MessageTs ->
	    ok = io:format("Expected and got ~p", [MessageTs]),
	    MessageTs;
	Other ->
	    io:format("Expected ~p; got ~p", [Message,Other]),
	    test_server:fail({unexpected,Other})
    after 5000 ->
	    io:format("Expected ~p; got nothing", [Message]),
	    test_server:fail(no_trace_message)
    end;
expect(Message) ->
    receive
	Message ->
	    ok = io:format("Expected and got ~p", [Message]),
	    Message;
	Other ->
	    io:format("Expected ~p; got ~p", [Message,Other]),
	    test_server:fail({unexpected,Other})
    after 5000 ->
	    io:format("Expected ~p; got nothing", [Message]),
	    test_server:fail(no_trace_message)
    end.

trac(What, On, Flags0) ->
    Flags = [{tracer,get(tracer_port)}|Flags0],
    get(tracer) ! {apply,self(),{erlang,trace,[What,On,Flags]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("erlang:trace(~p, ~p, ~p) -> ~p",
		   [What,On,Flags,Res]),
    Res.
    
trace_info(What, Key) ->
    get(tracer) ! {apply,self(),{erlang,trace_info,[What,Key]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("erlang:trace_info(~p, ~p) -> ~p",
		   [What,Key,Res]),
    Res.
    
trace_func(MFA, MatchProg) ->
    get(tracer) ! {apply,self(),{erlang,trace_pattern,[MFA,MatchProg]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("erlang:trace_pattern(~p, ~p) -> ~p", [MFA,MatchProg,Res]),
    Res.

trace_func(MFA, MatchProg, Flags) ->
    get(tracer) ! {apply,self(),{erlang,trace_pattern,[MFA,MatchProg,Flags]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("erlang:trace_pattern(~p, ~p) -> ~p", [MFA,MatchProg,Res]),
    Res.

trace_pid(Pid, On, Flags0) ->
    Flags = [{tracer,get(tracer_port)}|Flags0],
    get(tracer) ! {apply,self(),{erlang,trace,[Pid,On,Flags]}},
    Res = receive
	      {apply_result,Result} -> Result
	  end,
    ok = io:format("erlang:trace(~p, ~p, ~p) -> ~p",
		   [Pid,On,Flags,Res]),
    Res.

start_tracer(Config) ->
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, echo_drv),
    Self = self(),
    put(tracer, fun_spawn(fun() -> tracer(Self) end)),
    receive
	{started,Port} ->
	    put(tracer_port, Port)
    end,
    get(tracer).

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.

tracer(RelayTo) ->
    Port = open_port({spawn,echo_drv}, [eof,binary]),
    RelayTo ! {started,Port},
    tracer_loop(RelayTo, Port).

tracer_loop(RelayTo, Port) ->
    receive
	{apply,From,{M,F,A}} ->
	    From ! {apply_result,apply(M, F, A)},
	    tracer_loop(RelayTo, Port);
	{Port,{data,Msg}} ->
	    RelayTo ! binary_to_term(Msg),
	    tracer_loop(RelayTo, Port);
	{unlink_tracer_port, From} ->
	    unlink(Port),
	    From ! {unlinked_tracer_port, self()},
	    tracer_loop(RelayTo, Port);
	Other ->
	    exit({bad_message,Other})
    end.

fun_spawn(Fun) ->
    spawn_link(erlang, apply, [Fun,[]]).

fun_spawn(Fun, Opts) ->
    spawn_opt(erlang, apply, [Fun,[]], [link | Opts]).

% flush() ->
%     receive
% 	X ->
% 	    [X | flush()]
%     after 2000 ->
% 	    []
%     end.


%%% Models for various kinds of processes.

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

%% Does a garbage collection when it receives a message.

garber() ->
    receive
	_Any ->
	    lists:seq(1, 100),
	    erlang:garbage_collect(),
	    garber()
    end.

%% All-purpose process

general() ->
    receive
	{apply, {M, F, Args}} ->
	    erlang:apply(M, F, Args),
	    general();
	{send, Dest, Msg} ->
	    Dest ! Msg,
	    general();
	{call_f_1, Arg} ->
	    f(Arg),
	    general();
	nop ->
	    general()
    end.

f(Arg) ->
    Arg.
