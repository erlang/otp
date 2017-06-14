%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

-export([all/0, suite/0,
	 call_trace/1,
	 return_trace/1,
	 send/1,
	 receive_trace/1,
	 receive_trace_non_scheduler/1,
	 process_events/1,
	 schedule/1,
	 gc/1,
	 default_tracer/1,
	 tracer_port_crash/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [call_trace, return_trace, send, receive_trace,
     receive_trace_non_scheduler,
     process_events, schedule, gc,
     default_tracer, tracer_port_crash].

%% Test sending call trace messages to a port.
call_trace(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) orelse
         test_server:is_native(lists) of
        true -> 
            {skip,"Native code"};
        false ->
            start_tracer(Config),
            Self = self(),
            trace_func({lists,reverse,1}, []),
            trace_pid(Self, true, [call]),
            trace_info(Self, flags),
            trace_info(Self, tracer),
            [b,a] = lists:reverse([a,b]),
            expect({trace,Self,call,{lists,reverse,[[a,b]]}}),

            trace_pid(Self, true, [timestamp]),
            trace_info(Self, flags),
            Huge = huge_data(),
            lists:reverse(Huge),
            expect({trace_ts,Self,call,{lists,reverse,[Huge]},ts}),

            trace_pid(Self, true, [arity]),
            trace_info(Self, flags),
            [y,x] = lists:reverse([x,y]),
            expect({trace_ts,Self,call,{lists,reverse,1},ts}),

            trace_pid(Self, false, [timestamp]),
            trace_info(Self, flags),
            [z,y,x] = lists:reverse([x,y,z]),
            expect({trace,Self,call,{lists,reverse,1}}),

            %% OTP-7399. Delayed sub-binary creation optimization.
            trace_pid(Self, false, [arity]),
            trace_info(Self, flags),
            trace_func({?MODULE,bs_sum_c,2}, [], [local]),
            26 = bs_sum_c(<<3:4,5:4,7:4,11:4>>, 0),
            trace_func({?MODULE,bs_sum_c,2}, false, [local]),
            expect({trace,Self,call,{?MODULE,bs_sum_c,[<<3:4,5:4,7:4,11:4>>,0]}}),
            expect({trace,Self,call,{?MODULE,bs_sum_c,[<<5:4,7:4,11:4>>,3]}}),
            expect({trace,Self,call,{?MODULE,bs_sum_c,[<<7:4,11:4>>,8]}}),
            expect({trace,Self,call,{?MODULE,bs_sum_c,[<<11:4>>,15]}}),
            expect({trace,Self,call,{?MODULE,bs_sum_c,[<<>>,26]}}),

            trace_func({lists,reverse,1}, false),
            ok
    end.

bs_sum_c(<<H:4,T/bits>>, Acc) -> bs_sum_c(T, H+Acc);
bs_sum_c(<<>>, Acc) -> Acc.


%% Test the new return trace.
return_trace(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) orelse
         test_server:is_native(lists) of
        true -> 
            {skip,"Native code"};
        false ->
            start_tracer(Config),
            Self = self(),
            MFA = {lists,reverse,1},

            %% Plain (no timestamp, small data).

            trace_func(MFA, [{['$1'],[],[{return_trace},
                                         {message,false}]}]),
            trace_pid(Self, true, [call]),
            trace_info(Self, flags),
            trace_info(Self, tracer),
            trace_info(MFA, match_spec),
            {traced,global} = trace_info(MFA, traced),
            [b,a] = lists:reverse([a,b]),
            expect({trace,Self,return_from,MFA,[b,a]}),

            %% Timestamp, huge data.
            trace_pid(Self, true, [timestamp]),
            Result = lists:reverse(huge_data()),
            expect({trace_ts,Self,return_from,MFA,Result,ts}),

            %% Turn off trace.
            trace_func(MFA, false),
            trace_info(MFA, match_spec),
            {traced,false} = trace_info(MFA, traced),
            ok
    end.

%% Test sending send trace messages to a port.
send(Config) when is_list(Config) ->
    Tracer = start_tracer(Config),
    Self = self(),
    Sender = fun_spawn(fun sender/0),
    trac(Sender, true, [send]),

    %% Simple message, no timestamp.

    Bin = list_to_binary(lists:seq(1, 10)),
    Msg = {some_data,Bin},
    Sender ! {send_please,self(),Msg},
    receive Msg -> ok end,
    expect({trace,Sender,send,Msg,Self}),

    %% Timestamp.

    BiggerMsg = {even_bigger,Msg},
    trac(Sender, true, [send,timestamp]),
    Sender ! {send_please,self(),BiggerMsg},
    receive BiggerMsg -> ok end,
    expect({trace_ts,Sender,send,BiggerMsg,Self,ts}),

    %% Huge message.

    HugeMsg = huge_data(),
    Sender ! {send_please,self(),HugeMsg},
    receive HugeMsg -> ok end,
    expect({trace_ts,Sender,send,HugeMsg,Self,ts}),

    %% Kill trace port and force a trace.  The emulator should not crasch.

    unlink(Tracer),
    exit(Tracer, kill),
    erlang:yield(),				% Make sure that port gets killed.
    Sender ! {send_please,Self,good_bye},
    receive good_bye -> ok end,
    ok.

%% Test sending receive traces to a port.
receive_trace(Config) when is_list(Config) ->
    start_tracer(Config),
    Receiver = fun_spawn(fun receiver/0),
    trac(Receiver, true, ['receive']),

    Receiver ! {hello,world},
    expect({trace,Receiver,'receive',{hello,world}}),

    trac(Receiver, true, ['receive',timestamp]),
    Huge = {hello,huge_data()},
    Receiver ! {hello,huge_data()},
    expect({trace_ts,Receiver,'receive',Huge,ts}),
    ok.

%% Test sending receive traces to a port.
receive_trace_non_scheduler(Config) when is_list(Config) ->
    start_tracer(Config),
    S = self(),
    Receiver = spawn_link(
                 fun() ->
                         receive
                             go ->
                                 Ref = S ! erlang:trace_delivered(all),
                                 receive {trace_delivered, Ref, all} -> ok end
                         end
                 end),
    trac(Receiver, true, ['receive']),
    Receiver ! go,
    Ref = receive R -> R end,
    expect({trace,Receiver,'receive',go}),
    expect({trace,Receiver,'receive',{trace_delivered, all, Ref}}),

    ok.

%% Tests a few process events (like getting linked).
process_events(Config) when is_list(Config) ->
    start_tracer(Config),
    Self = self(),
    Receiver = fun_spawn(fun receiver/0),
    trac(Receiver, true, [procs]),

    unlink(Receiver),				%It is already linked.
    expect({trace,Receiver,getting_unlinked,Self}),
    link(Receiver),
    expect({trace,Receiver,getting_linked,Self}),
    trac(Receiver, true, [procs,timestamp]),
    unlink(Receiver),
    expect({trace_ts,Receiver,getting_unlinked,Self,ts}),
    link(Receiver),
    expect({trace_ts,Receiver,getting_linked,Self,ts}),

    unlink(Receiver),
    expect({trace_ts,Receiver,getting_unlinked,Self,ts}),
    Huge = huge_data(),
    exit(Receiver, Huge),
    expect({trace_ts,Receiver,exit,Huge,ts}),

    ok.

%% Test sending scheduling events to a port.
schedule(Config) when is_list(Config) ->
    start_tracer(Config),
    Receiver = fun_spawn(fun receiver/0),
    trac(Receiver, true, [running]),

    Receiver ! hi,
    expect({trace,Receiver,in,{?MODULE,receiver,0}}),
    expect({trace,Receiver,out,{?MODULE,receiver,0}}),

    trac(Receiver, true, [running,timestamp]),

    Receiver ! hi_again,
    expect({trace_ts,Receiver,in,{?MODULE,receiver,0},ts}),
    expect({trace_ts,Receiver,out,{?MODULE,receiver,0},ts}),

    ok.

%% Test sending garbage collection events to a port.
gc(Config) when is_list(Config) ->
    start_tracer(Config),
    Garber = fun_spawn(fun garber/0, [{min_heap_size, 5000}]),
    trac(Garber, true, [garbage_collection]),
    trace_info(Garber, flags),

    trace_info(Garber, flags),
    Garber ! hi,
    expect({trace,Garber,gc_major_start,info}),
    expect({trace,Garber,gc_major_end,info}),

    trac(Garber, true, [garbage_collection,timestamp]),
    Garber ! hi,
    expect({trace_ts,Garber,gc_major_start,info,ts}),
    expect({trace_ts,Garber,gc_major_end,info,ts}),

    ok.

%% Test a port as default tracer.
default_tracer(Config) when is_list(Config) ->
    Tracer = start_tracer(Config),
    TracerMonitor = erlang:monitor(process, Tracer),
    Port = get(tracer_port),
    %%
    N = erlang:trace(all, true, [send, {tracer, Port}]),
    {flags, [send]} = erlang:trace_info(self(), flags),
    {tracer, Port} = erlang:trace_info(self(), tracer),
    {flags, [send]} = erlang:trace_info(new, flags),
    {tracer, Port} = erlang:trace_info(new, tracer),
    G1 = fun_spawn(fun general/0),
    {flags, [send]} = erlang:trace_info(G1, flags),
    {tracer, Port} = erlang:trace_info(G1, tracer),
    unlink(Tracer),
    exit(Port, done),
    receive
        {'DOWN', TracerMonitor, process, Tracer, TracerExitReason} ->
            done = TracerExitReason
    end,
    {flags, []} = erlang:trace_info(self(), flags),
    {tracer, []} = erlang:trace_info(self(), tracer),
    {flags, []} = erlang:trace_info(new, flags),
    {tracer, []} = erlang:trace_info(new, tracer),
    M = erlang:trace(all, false, [all]),
    {flags, []} = erlang:trace_info(self(), flags),
    {tracer, []} = erlang:trace_info(self(), tracer),
    {flags, []} = erlang:trace_info(G1, flags),
    {tracer, []} = erlang:trace_info(G1, tracer),
    G1 ! {apply,{erlang,exit,[normal]}},
    io:format("~p = ~p.~n", [M, N]),
    M = N - 1, % G1 has been started, but Tracer and Port have died
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
                    ct:fail({trace_port_linker, Id})
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

expect({trace_ts,E1,E2,info,ts}=Message) ->
    receive
        {trace_ts,E1,E2,_Info,_Ts}=MessageTs ->
            ok = io:format("Expected and got ~p", [MessageTs]),
            MessageTs;
        Other ->
            io:format("Expected ~p; got ~p", [Message,Other]),
            ct:fail({unexpected,Other})
    after 5000 ->
              io:format("Expected ~p; got nothing", [Message]),
              ct:fail(no_trace_message)
    end;
expect({trace,E1,E2,info}=Message) ->
    receive
        {trace,E1,E2,_Info}=MessageTs ->
            ok = io:format("Expected and got ~p", [MessageTs]),
            MessageTs;
        Other ->
            io:format("Expected ~p; got ~p", [Message,Other]),
            ct:fail({unexpected,Other})
    after 5000 ->
              io:format("Expected ~p; got nothing", [Message]),
              ct:fail(no_trace_message)
    end;
expect({trace_ts,E1,E2,E3,ts}=Message) ->
    receive
        {trace_ts,E1,E2,E3,_Ts}=MessageTs ->
            ok = io:format("Expected and got ~p", [MessageTs]),
            MessageTs;
        Other ->
            io:format("Expected ~p; got ~p", [Message,Other]),
            ct:fail({unexpected,Other})
    after 5000 ->
              io:format("Expected ~p; got nothing", [Message]),
              ct:fail(no_trace_message)
    end;
expect({trace_ts,E1,E2,E3,E4,ts}=Message) ->
    receive
        {trace_ts,E1,E2,E3,E4,_Ts}=MessageTs ->
            ok = io:format("Expected and got ~p", [MessageTs]),
            MessageTs;
        Other ->
            io:format("Expected ~p; got ~p", [Message,Other]),
            ct:fail({unexpected,Other})
    after 5000 ->
              io:format("Expected ~p; got nothing", [Message]),
              ct:fail(no_trace_message)
    end;
expect(Message) ->
    receive
        Message ->
            ok = io:format("Expected and got ~p", [Message]),
            Message;
        Other ->
            io:format("Expected ~p; got ~p", [Message,Other]),
            ct:fail({unexpected,Other})
    after 5000 ->
              io:format("Expected ~p; got nothing", [Message]),
              ct:fail(no_trace_message)
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
    Path = proplists:get_value(data_dir, Config),
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
