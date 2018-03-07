%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

-module(trace_SUITE).

%%%
%%% Tests the trace BIF.
%%%

-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2,
         link_receive_call_correlation/0,
         receive_trace/1, link_receive_call_correlation/1, self_send/1,
	 timeout_trace/1, send_trace/1,
	 procs_trace/1, dist_procs_trace/1, procs_new_trace/1,
	 suspend/1, mutual_suspend/1, suspend_exit/1, suspender_exit/1,
	 suspend_system_limit/1, suspend_opts/1, suspend_waiting/1,
	 new_clear/1, existing_clear/1, tracer_die/1,
	 set_on_spawn/1, set_on_first_spawn/1, cpu_timestamp/1,
	 set_on_link/1, set_on_first_link/1,
	 system_monitor_args/1, more_system_monitor_args/1,
	 system_monitor_long_gc_1/1, system_monitor_long_gc_2/1, 
	 system_monitor_large_heap_1/1, system_monitor_large_heap_2/1,
	 system_monitor_long_schedule/1,
	 bad_flag/1, trace_delivered/1, trap_exit_self_receive/1]).

-include_lib("common_test/include/ct.hrl").

%%% Internal exports
-export([process/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [cpu_timestamp, receive_trace, link_receive_call_correlation,
     self_send, timeout_trace,
     send_trace, procs_trace, dist_procs_trace, suspend,
     mutual_suspend, suspend_exit, suspender_exit,
     suspend_system_limit, suspend_opts, suspend_waiting,
     new_clear, existing_clear, tracer_die, set_on_spawn,
     set_on_first_spawn, set_on_link, set_on_first_link,
     system_monitor_args,
     more_system_monitor_args, system_monitor_long_gc_1,
     system_monitor_long_gc_2, system_monitor_large_heap_1,
     system_monitor_long_schedule,
     system_monitor_large_heap_2, bad_flag, trace_delivered,
     trap_exit_self_receive].

init_per_testcase(_Case, Config) ->
    [{receiver,spawn(fun receiver/0)}|Config].

end_per_testcase(_Case, Config) ->
    Receiver = proplists:get_value(receiver, Config),
    unlink(Receiver),
    exit(Receiver, die),
    ok.

%% No longer testing anything, just reporting whether cpu_timestamp
%% is enabled or not.
cpu_timestamp(Config) when is_list(Config) ->
    %% Test whether cpu_timestamp is implemented on this platform.
    Works = try erlang:trace(all, true, [cpu_timestamp]) of
                _ ->
                    erlang:trace(all, false, [cpu_timestamp]),
                    true
            catch
                error:badarg -> false
            end,
    {comment,case Works of
                 false -> "cpu_timestamp is NOT implemented/does not work";
                 true -> "cpu_timestamp works"
             end}.


%% Tests that trace(Pid, How, ['receive']) works.

receive_trace(Config) when is_list(Config) ->
    Receiver = proplists:get_value(receiver, Config),

    %% Trace the process; make sure that we receive the trace messages.
    1 = erlang:trace(Receiver, true, ['receive']),
    Hello = {hello, world},
    Receiver ! Hello,
    {trace, Receiver, 'receive', Hello} = receive_first_trace(),
    Hello2 = {hello, again, world},
    Receiver ! Hello2,
    {trace, Receiver, 'receive', Hello2} = receive_first_trace(),
    receive_nothing(),

    %% Test 'receive' with matchspec
    F1 = fun ({Pat, IsMatching}) ->
		 set_trace_pattern('receive', Pat, []),
		 Receiver ! Hello,
		 case IsMatching of
		     true ->
			 {trace, Receiver, 'receive', Hello} = receive_first_trace();
		     false ->
			 ok
		 end,
		 receive_nothing()
	 end,
    From = self(),
    Node = node(),
    lists:foreach(F1, [{no, true},
		       {[{[Node, undefined,"Unexpected"],[],[]}], false},
		       {[{[Node, From,'_'],[],[]}], true},
		       {[{[Node, '$1','_'],[{'=/=','$1',From}],[]}], false},
		       {[{['$1', '_','_'],[{'=:=','$1',Node}],[]}], true},
		       {false, false},
		       {true, true}]),

    %% Remote messages
    OtherName = atom_to_list(?MODULE)++"_receive_trace",
    {ok, OtherNode} = start_node(OtherName),
    RemoteProc = spawn_link(OtherNode, ?MODULE, process, [self()]),
    io:format("RemoteProc = ~p ~n", [RemoteProc]),

    RemoteProc ! {send_please, Receiver, Hello},
    {trace, Receiver, 'receive', Hello} = receive_first_trace(),
    RemoteProc ! {send_please, Receiver, 99},
    {trace, Receiver, 'receive', 99} = receive_first_trace(),

    %% Remote with matchspec
    F2 = fun (To, {Pat, IsMatching}) ->
		 set_trace_pattern('receive', Pat, []),
		 RemoteProc ! {send_please, To, Hello},
		 case IsMatching of
		     true ->
			 {trace, Receiver, 'receive', Hello} = receive_first_trace();
		     false ->
			 ok
		 end,
		 receive_nothing()
	 end,
    F2(Receiver, {no, true}),
    F2(Receiver, {[{[OtherNode, undefined,"Unexpected"],[],[]}], false}),
    F2(Receiver, {[{[OtherNode, RemoteProc,'_'],[],[]},
		   {[OtherNode, undefined,'_'],[],[]}], true}),
    F2(Receiver, {[{[OtherNode, '$1','_'],
		    [{'orelse',{'=:=','$1',undefined},{'=/=',{node,'$1'},{node}}}],
		    []}], true}),
    F2(Receiver, {[{['$1', '_','_'], [{'=:=','$1',OtherNode}], []}], true}),
    F2(Receiver, {false, false}),
    F2(Receiver, {true, true}),

    %% Remote to named with matchspec
    Name = trace_SUITE_receiver,
    register(Name, Receiver),
    NN = {Name, node()},
    F2(NN, {no, true}),
    F2(NN, {[{[OtherNode, undefined,"Unexpected"],[],[]}], false}),
    F2(NN, {[{[OtherNode, RemoteProc,'_'],[],[]},
	     {[OtherNode, undefined,'_'],[],[]}], true}),
    F2(NN, {[{[OtherNode, '$1','_'],
	      [{'orelse',{'=:=','$1',undefined},{'=/=',{node,'$1'},{node}}}],
	      []}], true}),
    F2(NN, {[{['$1', '_','_'], [{'==','$1',OtherNode}], []}], true}),
    F2(NN, {false, false}),
    F2(NN, {true, true}),

    unlink(RemoteProc),
    true = stop_node(OtherNode),

    %% Timeout
    Receiver ! {set_timeout, 10},
    {trace, Receiver, 'receive', {set_timeout, 10}} = receive_first_trace(),
    {trace, Receiver, 'receive', timeout} = receive_first_trace(),
    erlang:trace_pattern('receive', [{[clock_service,undefined,timeout], [], []}], []),
    Receiver ! {set_timeout, 7},
    {trace, Receiver, 'receive', timeout} = receive_first_trace(),
    erlang:trace_pattern('receive', true, []),

    %% Another process should not be able to trace Receiver.
    process_flag(trap_exit, true),
    Intruder = fun_spawn(fun() -> erlang:trace(Receiver, true, ['receive']) end),
    {'EXIT', Intruder, {badarg, _}} = receive_first(),

    %% Untrace the process; we should not receive anything.
    1 = erlang:trace(Receiver, false, ['receive']),
    Receiver ! {hello, there},
    Receiver ! any_garbage,
    receive_nothing(),

    %% Verify restrictions in matchspec for 'receive'
    F3 = fun (Pat) -> {'EXIT', {badarg,_}} = (catch erlang:trace_pattern('receive', Pat, [])) end,
    WC = ['_','_','_'],
    F3([{WC,[],[{message, {process_dump}}]}]),
    F3([{WC,[{is_seq_trace}],[]}]),
    F3([{WC,[],[{set_seq_token,label,4711}]}]),
    F3([{WC,[],[{get_seq_token}]}]),
    F3([{WC,[],[{enable_trace,call}]}]),
    F3([{WC,[],[{enable_trace,self(),call}]}]),
    F3([{WC,[],[{disable_trace,call}]}]),
    F3([{WC,[],[{disable_trace,self(),call}]}]),
    F3([{WC,[],[{trace,[call],[]}]}]),
    F3([{WC,[],[{trace,self(),[],[call]}]}]),
    F3([{WC,[],[{caller}]}]),
    F3([{WC,[],[{silent,true}]}]),

    ok.

%% Tests that receive of a message always happens before a call with
%% that message and that links/unlinks are ordered together with the
%% 'receive'.
link_receive_call_correlation() ->
    [{timetrap, {minutes, 5}}].
link_receive_call_correlation(Config) when is_list(Config) ->
    Receiver = fun_spawn(fun F() ->
                                 receive
                                     stop -> ok;
                                     M -> receive_msg(M), F()
                                 end
                         end),
    process_flag(trap_exit, true),

    %% Trace the process; make sure that we receive the trace messages.
    1 = erlang:trace(Receiver, true, ['receive', procs, call, timestamp, scheduler_id]),
    1 = erlang:trace_pattern({?MODULE, receive_msg, '_'}, [], [local]),

    Num = 100,

    (fun F(0) -> [];
         F(N) ->
             if N rem 2 == 0 ->
                     link(Receiver);
                true ->
                     unlink(Receiver)
             end,
             [Receiver ! N | F(N-1)]
     end)(Num),

    Receiver ! stop,
    MonRef = erlang:monitor(process, Receiver),
    receive {'DOWN', MonRef, _, _, _} -> ok end,
    Ref = erlang:trace_delivered(Receiver),
    receive {trace_delivered, _, Ref} -> ok end,

    Msgs = (fun F() -> receive M -> [M | F()] after 1 -> [] end end)(),

    case check_consistent(Receiver, Num, Num, Num, Msgs, false, undefined) of
        ok ->
            ok;
        {error, Reason} ->
            ct:log("~p", [Msgs]),
            ct:fail({error, Reason})
    end.

-define(schedid, , _).

check_consistent(_Pid, Recv, Call, _LU, [Msg | _], _Received, _LinkedN) when Recv > Call ->
    {error, Msg};
check_consistent(Pid, Recv, Call, LU, [Msg | Msgs], false, undefined) ->

    case Msg of
        {trace, Pid, 'receive', Recv ?schedid} ->
            check_consistent(Pid,Recv - 1, Call, LU, Msgs, true, undefined);
        {trace_ts, Pid, 'receive', Recv ?schedid, _} ->
            check_consistent(Pid,Recv - 1, Call, LU, Msgs, true, undefined);

        {trace, Pid, call, {?MODULE, receive_msg, [Call]} ?schedid} ->
            check_consistent(Pid,Recv, Call - 1, LU, Msgs, false, undefined);
        {trace_ts, Pid, call, {?MODULE, receive_msg, [Call]} ?schedid, _} ->
            check_consistent(Pid,Recv, Call - 1, LU, Msgs, false, undefined);

        {trace, Pid, _, _Self ?schedid} ->
            check_consistent(Pid, Recv, Call, LU, Msgs, false, undefined);
        {trace_ts, Pid, _, _Self ?schedid, _} ->
            check_consistent(Pid, Recv, Call, LU, Msgs, false, undefined);

        Msg ->
            {error, Msg}
    end;
check_consistent(Pid, Recv, Call, LU, [Msg | Msgs], true, undefined) ->

    case Msg of
        {trace, Pid, call, {?MODULE, receive_msg, [Call]} ?schedid} ->
            check_consistent(Pid,Recv, Call - 1, LU, Msgs, true, undefined);
        {trace_ts, Pid, call, {?MODULE, receive_msg, [Call]} ?schedid, _} ->
            check_consistent(Pid,Recv, Call - 1, LU, Msgs, true, undefined);

        {trace, Pid, getting_linked, _Self ?schedid} ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, Recv rem 2);
        {trace_ts, Pid, getting_linked, _Self ?schedid, _} ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, Recv rem 2);

        {trace, Pid, getting_unlinked, _Self ?schedid} ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, (Recv+1) rem 2);
        {trace_ts, Pid, getting_unlinked, _Self ?schedid, _} ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, (Recv+1) rem 2);

        Msg ->
            {error, Msg}
    end;
check_consistent(Pid, Recv, Call, LU, [Msg | Msgs], true, LinkedN) ->
    UnlinkedN = (LinkedN + 1) rem 2,

    case Msg of
        {trace, Pid, 'receive', Recv ?schedid} when Recv == LU ->
            check_consistent(Pid,Recv - 1, Call, LU, Msgs, true, LinkedN);
        {trace_ts, Pid, 'receive', Recv ?schedid, _} when Recv == LU ->
            check_consistent(Pid,Recv - 1, Call, LU, Msgs, true, LinkedN);

        {trace, Pid, call, {?MODULE, receive_msg, [Call]} ?schedid} ->
            check_consistent(Pid,Recv, Call - 1, LU, Msgs, true, LinkedN);
        {trace_ts, Pid, call, {?MODULE, receive_msg, [Call]} ?schedid, _} ->
            check_consistent(Pid,Recv, Call - 1, LU, Msgs, true, LinkedN);

        %% We check that for each receive we have gotten a
        %% getting_linked or getting_unlinked message. Also
        %% if we receive a getting_linked, then the next
        %% message we expect to receive is an even number
        %% and odd number for getting_unlinked.
        {trace, Pid, getting_linked, _Self ?schedid}
          when Recv rem 2 == LinkedN ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, LinkedN);
        {trace_ts, Pid, getting_linked, _Self ?schedid, _}
          when Recv rem 2 == LinkedN ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, LinkedN);

        {trace, Pid, getting_unlinked, _Self ?schedid}
          when Recv rem 2 == UnlinkedN ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, LinkedN);
        {trace_ts, Pid, getting_unlinked, _Self ?schedid, _}
          when Recv rem 2 == UnlinkedN ->
            check_consistent(Pid, Recv, Call, LU - 1, Msgs, true, LinkedN);

        {trace,Pid,'receive',Ignore ?schedid}
          when Ignore == stop; Ignore == timeout ->
            check_consistent(Pid, Recv, Call, LU, Msgs, true, LinkedN);
        {trace_ts,Pid,'receive',Ignore ?schedid,_}
          when Ignore == stop; Ignore == timeout ->
            check_consistent(Pid, Recv, Call, LU, Msgs, true, LinkedN);

        {trace, Pid, exit, normal ?schedid} ->
            check_consistent(Pid, Recv, Call, LU, Msgs, true, LinkedN);
        {trace_ts, Pid, exit, normal  ?schedid, _} ->
            check_consistent(Pid, Recv, Call, LU, Msgs, true, LinkedN);
        {'EXIT', Pid, normal} ->
            check_consistent(Pid, Recv, Call, LU, Msgs, true, LinkedN);
        Msg ->
            {error, Msg}
    end;
check_consistent(_, 0, 0, 1, [], true, _) ->
    ok;
check_consistent(_, Recv, Call, LU, [], _, _) ->
    {error,{Recv, Call, LU}}.

receive_msg(M) ->
    M.

%% Test that traces are generated for messages sent
%% and received to/from self().
self_send(Config) when is_list(Config) ->
    Fun =
    fun(Self, Parent) -> receive
                             go_ahead ->
                                 self() ! from_myself,
                                 Self(Self, Parent);
                             from_myself ->
                                 Parent ! done
                         end
    end,
    Self = self(),
    SelfSender = fun_spawn(Fun, [Fun, Self]),
    erlang:trace(SelfSender, true, ['receive', 'send']),
    SelfSender ! go_ahead,
    receive {trace, SelfSender, 'receive', go_ahead} -> ok end,
    receive {trace, SelfSender, 'receive', from_myself} -> ok end,
    receive
        {trace,SelfSender,send,from_myself,SelfSender} -> ok
    end,
    receive {trace,SelfSender,send,done,Self} -> ok end,
    receive done -> ok end,
    ok.

%% Test that we can receive timeout traces.
timeout_trace(Config) when is_list(Config) ->
    Process = fun_spawn(fun process/0),
    1 = erlang:trace(Process, true, ['receive']),
    Process ! timeout_please,
    {trace, Process, 'receive', timeout_please} = receive_first_trace(),
    {trace, Process, 'receive', timeout} = receive_first_trace(),
    receive_nothing(),
    ok.

%% Tests that trace(Pid, How, [send]) works.

send_trace(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Sender = fun_spawn(fun sender/0),
    Receiver = proplists:get_value(receiver, Config),

    %% Check that a message sent to another process is traced.
    1 = erlang:trace(Sender, true, [send]),
    F1 = fun (Pat) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, Receiver, to_receiver},
		 {trace, Sender, send, to_receiver, Receiver} = receive_first_trace(),
		 receive_nothing()
	 end,
    lists:foreach(F1, [no,
		       [{[Receiver,to_receiver],[],[]}],
		       [{['_','_'],[],[]}],
		       [{['$1','_'],[{is_pid,'$1'}],[]}],
		       [{['_','$1'],[{is_atom,'$1'}],[]}],
		       true]),

    %% Test {message, Msg}
    F1m = fun ({Pat, Msg}) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, Receiver, to_receiver},
		 {trace, Sender, send, to_receiver, Receiver, Msg} = receive_first_trace(),
		 receive_nothing()
	 end,
    lists:foreach(F1m, [{[{['_','_'],[],[{message, 4711}]}], 4711},
			{[{['_','_'],[],[{message, "4711"}]}], "4711"}
		       ]),

    %% Test {message, {process_dump}}
    set_trace_pattern(send, [{['_','_'],[],[{message, {process_dump}}]}], []),
    Sender ! {send_please, Receiver, to_receiver},
    {trace, Sender, send, to_receiver, Receiver, ProcDump} = receive_first_trace(),
    true = is_binary(ProcDump),
    receive_nothing(),

    %% Same test with false match spec
    F2 = fun (Pat) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, Receiver, to_receiver},
		 receive_nothing()
	 end,
    lists:foreach(F2, [[{[Sender,to_receiver],[],[]}],
		       [{[Receiver,nomatch],[],[]}],
		       [{['$1','_'],[{is_atom,'$1'}],[]}],
		       [{['_','$1'],[{is_pid,'$1'}],[]}],
		       false,
		       [{['_','_'],[],[{message,false}]}],
		       [{['_','_'],[],[{silent,true}]}]]),
    erlang:trace_pattern(send, true, []),
    erlang:trace(Sender, false, [silent]),

    %% Check that a message sent to another registered process is traced.
    register(?MODULE,Receiver),
    F3 = fun (Pat) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, ?MODULE, to_receiver},
		 {trace, Sender, send, to_receiver, ?MODULE} = receive_first_trace(),
		 receive_nothing()
	 end,
    lists:foreach(F3, [no,
		       [{[?MODULE,to_receiver],[],[]}],
		       [{['_','_'],[],[]}],
		       [{['$1','_'],[{is_atom,'$1'}],[]}],
		       [{['_','$1'],[{is_atom,'$1'}],[]}],
		       true]),
    %% Again with false match spec
    F4 = fun (Pat) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, ?MODULE, to_receiver},
		 receive_nothing()
	 end,
    lists:foreach(F4, [[{[nomatch,to_receiver],[],[]}],
		       [{[?MODULE,nomatch],[],[]}],
		       [{['$1','_'],[{is_pid,'$1'}],[]}],
		       [{['_','$1'],[{is_pid,'$1'}],[]}],
		       [{['_','_'],[],[{message,false}]}],
		       [{['_','_'],[],[{silent,true}]}]
		      ]),
    unregister(?MODULE),
    erlang:trace_pattern(send, true, []),
    erlang:trace(Sender, false, [silent]),

    %% Check that a message sent to this process is traced.
    F5 = fun (Pat) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, self(), to_myself},
		 receive to_myself -> ok end,
		 Self = self(),
		 {trace, Sender, send, to_myself, Self} = receive_first_trace(),
		 receive_nothing()
	 end,
    lists:foreach(F5, [no,
		       [{[self(),to_myself],[],[]}],
		       [{['_','_'],[],[]}],
		       true]),

    %% Check that a message sent to dead process is traced.
    {Pid,Ref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN',Ref,_,_,_} -> ok end,
    F6 = fun (Pat) ->
		 set_trace_pattern(send, Pat, []),
		 Sender ! {send_please, Pid, to_dead},
		 {trace, Sender, send_to_non_existing_process, to_dead, Pid} = receive_first_trace(),
		 receive_nothing()
	 end,
    lists:foreach(F6, [no,
		       [{[Pid,to_dead],[],[]}],
		       [{['_','_'],[],[]}],
		       true]),

    %% Check that a message sent to unknown registrated process is traced.
    BadargSender = fun_spawn(fun sender/0),
    1 = erlang:trace(BadargSender, true, [send]),
    unlink(BadargSender),
    BadargSender ! {send_please, not_registered, to_unknown},
    {trace, BadargSender, send, to_unknown, not_registered} = receive_first_trace(),
    receive_nothing(),

    %% Another process should not be able to trace Sender.
    Intruder = fun_spawn(fun() -> erlang:trace(Sender, true, [send]) end),
    {'EXIT', Intruder, {badarg, _}} = receive_first(),

    %% Untrace the sender process and make sure that we receive no more
    %% trace messages.
    1 = erlang:trace(Sender, false, [send]),
    Sender ! {send_please, Receiver, to_receiver},
    Sender ! {send_please, self(), to_myself_again},
    receive to_myself_again -> ok end,
    receive_nothing(),
    
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, true, [global])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, true, [local])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, true, [meta])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, true, [{meta,self()}])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, true, [call_count])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, true, [call_time])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, restart, [])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, pause, [])),
    {'EXIT',{badarg,_}} = (catch erlang:trace_pattern(send, [{['_','_'],[],[{caller}]}], [])),

    %% Done.
    ok.

set_trace_pattern(_, no, _) -> 0;
set_trace_pattern(MFA, Pat, Flg) ->
    R = erlang:trace_pattern(MFA, Pat, Flg),
    {match_spec, Pat} = erlang:trace_info(MFA, match_spec),
    R.

%% Test trace(Pid, How, [procs]).
procs_trace(Config) when is_list(Config) ->
    Name = list_to_atom(atom_to_list(?MODULE)++"_procs_trace"),
    Self = self(),
    process_flag(trap_exit, true),
    %%
    Proc1 = spawn_link(?MODULE, process, [Self]),
    io:format("Proc1 = ~p ~n", [Proc1]),
    Proc2 = spawn(?MODULE, process, [Self]),
    io:format("Proc2 = ~p ~n", [Proc2]),
    %%
    1 = erlang:trace(Proc1, true, [procs, set_on_first_spawn]),
    MFA = {?MODULE, process, [Self]},
    %%
    %% spawn, link
    Proc1 ! {spawn_link_please, Self, MFA},
    Proc3 = receive {spawned, Proc1, P3} -> P3 end,
    receive {trace, Proc3, spawned, Proc1, MFA} -> ok end,
    receive {trace, Proc3, getting_linked, Proc1} -> ok end,
    {trace, Proc1, spawn, Proc3, MFA} = receive_first_trace(),
    io:format("Proc3 = ~p ~n", [Proc3]),
    {trace, Proc1, link, Proc3} = receive_first_trace(),
    receive_nothing(),
    %%
    %% getting_unlinked by exit()
    Proc1 ! {trap_exit_please, true},
    Reason3 = make_ref(),
    Proc1 ! {send_please, Proc3, {exit_please, Reason3}},
    receive {Proc1, {'EXIT', Proc3, Reason3}} -> ok end,
    receive {trace, Proc3, exit, Reason3} -> ok end,
    {trace, Proc1, getting_unlinked, Proc3} = receive_first_trace(),
    Proc1 ! {trap_exit_please, false},
    receive_nothing(),
    %%
    %% link
    Proc1 ! {link_please, Proc2},
    {trace, Proc1, link, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% unlink
    Proc1 ! {unlink_please, Proc2},
    {trace, Proc1, unlink, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% getting_linked
    Proc2 ! {link_please, Proc1},
    {trace, Proc1, getting_linked, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% getting_unlinked
    Proc2 ! {unlink_please, Proc1},
    {trace, Proc1, getting_unlinked, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% register
    true = register(Name, Proc1),
    {trace, Proc1, register, Name} = receive_first_trace(),
    receive_nothing(),
    %%
    %% unregister
    true = unregister(Name),
    {trace, Proc1, unregister, Name} = receive_first_trace(),
    receive_nothing(),
    %%
    %% exit (with registered name, due to link)
    Reason4 = make_ref(),
    Proc1 ! {spawn_link_please, Self, MFA},
    Proc4 = receive {spawned, Proc1, P4} -> P4 end,
    {trace, Proc1, spawn, Proc4, MFA} = receive_first_trace(),
    io:format("Proc4 = ~p ~n", [Proc4]),
    {trace, Proc1, link, Proc4} = receive_first_trace(),
    Proc1 ! {register_please, Name, Proc1},
    {trace, Proc1, register, Name} = receive_first_trace(),
    Proc4 ! {exit_please, Reason4},
    receive {'EXIT', Proc1, Reason4} -> ok end,
    {trace, Proc1, exit, Reason4} = receive_first_trace(),
    {trace, Proc1, unregister, Name} = receive_first_trace(),
    receive_nothing(),
    %%
    %% exit (not linked to tracing process)
    1 = erlang:trace(Proc2, true, [procs]),
    Reason2 = make_ref(),
    Proc2 ! {exit_please, Reason2},
    {trace, Proc2, exit, Reason2} = receive_first_trace(),
    receive_nothing(),
    ok.


dist_procs_trace(Config) when is_list(Config) ->
    ct:timetrap({seconds, 15}),
    OtherName = atom_to_list(?MODULE)++"_dist_procs_trace",
    {ok, OtherNode} = start_node(OtherName),
    Self = self(),
    process_flag(trap_exit, true),
    %%
    Proc1 = spawn_link(?MODULE, process, [Self]),
    io:format("Proc1 = ~p ~n", [Proc1]),
    Proc2 = spawn(OtherNode, ?MODULE, process, [Self]),
    io:format("Proc2 = ~p ~n", [Proc2]),
    %%
    1 = erlang:trace(Proc1, true, [procs]),
    MFA = {?MODULE, process, [Self]},
    %%
    %% getting_unlinked by exit()
    Proc1 ! {spawn_link_please, Self, OtherNode, MFA},
    Proc1 ! {trap_exit_please, true},
    Proc3 = receive {spawned, Proc1, P3} -> P3 end,
    io:format("Proc3 = ~p ~n", [Proc3]),
    {trace, Proc1, getting_linked, Proc3} = receive_first_trace(),
    Reason3 = make_ref(),
    Proc1 ! {send_please, Proc3, {exit_please, Reason3}},
    receive {Proc1, {'EXIT', Proc3, Reason3}} -> ok end,
    {trace, Proc1, getting_unlinked, Proc3} = receive_first_trace(),
    Proc1 ! {trap_exit_please, false},
    receive_nothing(),
    %%
    %% link
    Proc1 ! {link_please, Proc2},
    {trace, Proc1, link, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% unlink
    Proc1 ! {unlink_please, Proc2},
    {trace, Proc1, unlink, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% getting_linked
    Proc2 ! {link_please, Proc1},
    {trace, Proc1, getting_linked, Proc2} = receive_first_trace(),
    receive_nothing(),
    %%
    %% getting_unlinked
    Proc2 ! {unlink_please, Proc1},
    {trace, Proc1, getting_unlinked, Proc2} = receive_first_trace(),
    receive_nothing(),

    %%
    %% exit (with registered name, due to link)
    Name = list_to_atom(OtherName),
    Reason2 = make_ref(),
    Proc1 ! {link_please, Proc2},
    {trace, Proc1, link, Proc2} = receive_first_trace(),
    Proc1 ! {register_please, Name, Proc1},
    {trace, Proc1, register, Name} = receive_first_trace(),
    Proc2 ! {exit_please, Reason2},
    receive {'EXIT', Proc1, Reason2} -> ok end,
    {trace, Proc1, exit, Reason2} = receive_first_trace(),
    {trace, Proc1, unregister, Name} = receive_first_trace(),
    receive_nothing(),
    %%
    %% Done.
    true = stop_node(OtherNode),
    ok.

%% Test trace(new, How, [procs]).
procs_new_trace(Config) when is_list(Config) ->
    Self = self(),
    process_flag(trap_exit, true),
    %%
    Proc1 = spawn_link(?MODULE, process, [Self]),
    io:format("Proc1 = ~p ~n", [Proc1]),
    %%
    0 = erlang:trace(new, true, [procs]),

    MFA = {?MODULE, process, [Self]},
    %%
    %% spawn, link
    Proc1 ! {spawn_link_please, Self, MFA},
    Proc3 = receive {spawned, Proc1, P3} -> P3 end,
    receive {trace, Proc3, spawned, Proc1, MFA} -> ok end,
    receive {trace, Proc3, getting_linked, Proc1} -> ok end,
    io:format("Proc3 = ~p ~n", [Proc3]),
    receive_nothing(),
    %%
    %%
    %% exit (not linked to tracing process)
    Reason1 = make_ref(),
    Proc1 ! {exit_please, Reason1},
    receive {'EXIT', Proc1, Reason1} -> ok end,
    {trace, Proc3, exit, Reason1} = receive_first_trace(),
    receive_nothing(),
    ok.



%% Tests trace(Pid, How, [set_on_spawn]).

set_on_spawn(Config) when is_list(Config) ->
    Listener = fun_spawn(fun process/0),

    %% Create and trace a process with the set_on_spawn flag.
    %% Make sure it is traced.
    Father_SOS = fun_spawn(fun process/0),
    1 = erlang:trace(Father_SOS, true, [send, set_on_spawn]),
    true = is_send_traced(Father_SOS, Listener, sos_father),

    %% Have the process spawn of two children and test that they
    %% are traced.
    [Child1, Child2] = spawn_children(Father_SOS, 2),
    true = is_send_traced(Child1, Listener, child1),
    true = is_send_traced(Child2, Listener, child2),

    %% Second generation.
    [Child11, Child12] = spawn_children(Child1, 2),
    true = is_send_traced(Child11, Listener, child11),
    true = is_send_traced(Child12, Listener, child12),
    ok.

%% Tests trace(Pid, How, [set_on_first_spawn]).

set_on_first_spawn(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),
    Listener = fun_spawn(fun process/0),

    %% Create and trace a process with the set_on_first_spawn flag.
    %% Make sure it is traced.
    Parent = fun_spawn(fun process/0),
    1 = erlang:trace(Parent, true, [send, set_on_first_spawn]),
    is_send_traced(Parent, Listener, sos_father),

    %% Have the process spawn off three children and test that the
    %% first is traced.
    [Child1, Child2, Child3] = spawn_children(Parent, 3),
    true = is_send_traced(Child1, Listener, child1),
    false = is_send_traced(Child2, Listener, child2),
    false = is_send_traced(Child3, Listener, child3),
    receive_nothing(),
    ok.

%% Tests trace(Pid, How, [set_on_link]).

set_on_link(_Config) ->
    Listener = fun_spawn(fun process/0),

    %% Create and trace a process with the set_on_link flag.
    %% Make sure it is traced.
    Father_SOL = fun_spawn(fun process/0),
    1 = erlang:trace(Father_SOL, true, [send, set_on_link]),
    true = is_send_traced(Father_SOL, Listener, sol_father),

    %% Have the process spawn of two children and test that they
    %% are traced.
    [Child1, Child2] = spawn_children(Father_SOL, 2),
    true = is_send_traced(Child1, Listener, child1),
    true = is_send_traced(Child2, Listener, child2),

    %% Second generation.
    [Child11, Child12] = spawn_children(Child1, 2),
    true = is_send_traced(Child11, Listener, child11),
    true = is_send_traced(Child12, Listener, child12),
    ok.

%% Tests trace(Pid, How, [set_on_first_spawn]).

set_on_first_link(_Config) ->
    ct:timetrap({seconds, 10}),
    Listener = fun_spawn(fun process/0),

    %% Create and trace a process with the set_on_first_spawn flag.
    %% Make sure it is traced.
    Parent = fun_spawn(fun process/0),
    1 = erlang:trace(Parent, true, [send, set_on_first_link]),
    is_send_traced(Parent, Listener, sol_father),

    %% Have the process spawn off three children and test that the
    %% first is traced.
    [Child1, Child2, Child3] = spawn_children(Parent, 3),
    true = is_send_traced(Child1, Listener, child1),
    false = is_send_traced(Child2, Listener, child2),
    false = is_send_traced(Child3, Listener, child3),
    receive_nothing(),
    ok.



%% Tests arguments to erlang:system_monitor/0,1,2
system_monitor_args(Config) when is_list(Config) ->
    Self = self(),
    %%
    OldMonitor = erlang:system_monitor(undefined),
    undefined = erlang:system_monitor(Self, [{long_gc,0}]),
    MinT = case erlang:system_monitor() of
               {Self,[{long_gc,T}]} when is_integer(T), T > 0 -> T;
               Other1 -> test_server:fault(Other1)
           end,
    {Self,[{long_gc,MinT}]} = erlang:system_monitor(),
    {Self,[{long_gc,MinT}]} = 
    erlang:system_monitor({Self,[{large_heap,0}]}),
    MinN = case erlang:system_monitor() of
               {Self,[{large_heap,N}]} when is_integer(N), N > 0 -> N;
               Other2 -> test_server:fault(Other2)
           end,
    {Self,[{large_heap,MinN}]} = erlang:system_monitor(),
    {Self,[{large_heap,MinN}]} = 
    erlang:system_monitor(Self, [busy_port]),
    {Self,[busy_port]} = erlang:system_monitor(),
    {Self,[busy_port]} = 
    erlang:system_monitor({Self,[busy_dist_port]}),
    {Self,[busy_dist_port]} = erlang:system_monitor(),
    All = lists:sort([busy_port,busy_dist_port,
                      {long_gc,1},{large_heap,65535}]),
    {Self,[busy_dist_port]} = erlang:system_monitor(Self, All),
    {Self,A1} = erlang:system_monitor(),
    All = lists:sort(A1),
    {Self,A1} = erlang:system_monitor(Self, []),
    Pid = spawn(fun () -> receive {Self,die} -> exit(die) end end),
    Mref = erlang:monitor(process, Pid),
    undefined = erlang:system_monitor(Pid, All),
    {Pid,A2} = erlang:system_monitor(),
    All = lists:sort(A2),
    Pid ! {Self,die},
    receive {'DOWN',Mref,_,_,_} -> ok end,
    undefined = erlang:system_monitor(OldMonitor),
    erlang:yield(),
    OldMonitor = erlang:system_monitor(),
    %%
    {'EXIT',{badarg,_}} = (catch erlang:system_monitor(atom)),
    {'EXIT',{badarg,_}} = (catch erlang:system_monitor({})),
    {'EXIT',{badarg,_}} = (catch erlang:system_monitor({1})),
    {'EXIT',{badarg,_}} = (catch erlang:system_monitor({1,2,3})),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor({Self,atom})),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor(atom, atom)),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor({Self,[busy_port|busy_dist_port]})),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor(Self, [{long_gc,-1}])),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor({Self,[{long_gc,atom}]})),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor(Self,[{large_heap,-1}])),
    {'EXIT',{badarg,_}} = 
    (catch erlang:system_monitor({Self,[{large_heap,atom}]})),
    ok.


%% Tests arguments to erlang:system_monitor/0,1,2
more_system_monitor_args(Config) when is_list(Config) ->
    try_l(64000),
    try_l(16#7ffffff),
    try_l(16#3fffffff),
    try_l(16#7fffffff),
    try_l(16#ffffffff),
    ok.

try_l(Val) ->
    Self = self(),
    Arbitrary1 = 77777,
    Arbitrary2 = 88888,

    erlang:system_monitor(undefined),

    undefined = erlang:system_monitor(Self, [{long_gc,Val},{large_heap,Arbitrary1}]),

    {Self,Comb0} = erlang:system_monitor(Self, [{long_gc,Arbitrary2},{large_heap,Val}]),
    [{large_heap,Arbitrary1},{long_gc,Val}] = lists:sort(Comb0),

    {Self,Comb1} = erlang:system_monitor(undefined),
    [{large_heap,Val},{long_gc,Arbitrary2}] = lists:sort(Comb1).

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

%% Tests erlang:system_monitor(Pid, [{long_schedule,Time}])
system_monitor_long_schedule(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
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
              ct:fail(no_trace_of_pid)
    end,
    "ok" = erlang:port_control(Port,1,[]),
    "ok" = erlang:port_control(Port,2,[]),
    receive
        {Port,LL} when is_list(LL) ->
            ok
    after 1000 ->
              ct:fail(no_trace_of_port)
    end,
    port_close(Port),
    erlang:system_monitor(undefined),
    ok.


-define(LONG_GC_SLEEP, 670).

%% Tests erlang:system_monitor(Pid, [{long_gc,Time}])
system_monitor_long_gc_1(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    try 
        case erts_debug:get_internal_state(force_heap_frags) of
            true ->
                {skip,"emulator with FORCE_HEAP_FRAGS defined"};
            false ->
                %% Add ?LONG_GC_SLEEP ms to all gc
                erts_debug:set_internal_state(test_long_gc_sleep,
                                              ?LONG_GC_SLEEP),
                LoadFun = fun () -> 
                                  garbage_collect(),
                                  self() 
                          end,
                long_gc(LoadFun, false)
        end
    after
        erts_debug:set_internal_state(test_long_gc_sleep, 0),
        erts_debug:set_internal_state(available_internal_state, false)	
    end.

%% Tests erlang:system_monitor(Pid, [{long_gc,Time}])
system_monitor_long_gc_2(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    try
        case erts_debug:get_internal_state(force_heap_frags) of
            true ->
                {skip,"emulator with FORCE_HEAP_FRAGS defined"};
            false ->
                %% Add ?LONG_GC_SLEEP ms to all gc
                erts_debug:set_internal_state(test_long_gc_sleep,
                                              ?LONG_GC_SLEEP),
                Parent = self(),
                LoadFun =
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
                long_gc(LoadFun, true),
                long_gc(LoadFun, true),
                long_gc(LoadFun, true)
        end
    after
        erts_debug:set_internal_state(test_long_gc_sleep, 0),
        erts_debug:set_internal_state(available_internal_state, false)
    end.

long_gc(LoadFun, ExpectMonMsg) ->
    Self = self(),
    Time = 1,
    OldMonitor = erlang:system_monitor(Self, [{long_gc,Time}]),
    Pid = LoadFun(),
    Ref = erlang:trace_delivered(Pid),
    receive {trace_delivered, Pid, Ref} -> ok end,
    {Self,[{long_gc,Time}]} = erlang:system_monitor(OldMonitor),
    case {long_gc_check(Pid, Time, undefined), ExpectMonMsg} of
        {ok, true} when Pid =/= Self ->
            ok;
        {ok, false} ->
            ct:fail(unexpected_system_monitor_message_received);
        {undefined, false} ->
            ok;
        {undefined, true} ->
            ct:fail(no_system_monitor_message_received)
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

%% Tests erlang:system_monitor(Pid, [{large_heap,Size}])
system_monitor_large_heap_1(Config) when is_list(Config) ->
    LoadFun =
    fun (Size) -> 
            List = seq(1,2*Size),
            garbage_collect(),
            true = lists:prefix([1], List),
            self() 
    end,
    large_heap(LoadFun, false).

%% Tests erlang:system_monitor(Pid, [{large_heap,Size}])
system_monitor_large_heap_2(Config) when is_list(Config) ->
    Parent = self(),
    LoadFun =
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
    large_heap(LoadFun, true).

large_heap(LoadFun, ExpectMonMsg) ->
    ct:timetrap({seconds, 20}),
    %%
    Size = 65535,
    Self = self(),
    NewMonitor = {Self,[{large_heap,Size}]},
    OldMonitor = erlang:system_monitor(NewMonitor),
    Pid = LoadFun(Size),
    Ref = erlang:trace_delivered(Pid),
    receive {trace_delivered, Pid, Ref} -> ok end,
    {Self,[{large_heap,Size}]} = erlang:system_monitor(OldMonitor),
    case {large_heap_check(Pid, Size, undefined), ExpectMonMsg} of
        {ok, true} when Pid =/= Self ->
            ok;
        {ok, false} ->
            ct:fail(unexpected_system_monitor_message_received);
        {undefined, false} ->
            ok;
        {undefined, true} ->
            ct:fail(no_system_monitor_message_received)
    end,
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

%% Test erlang:suspend/1 and erlang:resume/1.
suspend(Config) when is_list(Config) ->
    ct:timetrap({minutes,2}),
    Worker = fun_spawn(fun worker/0),
    %% Suspend a process and test that it is suspended.
    ok = do_suspend(Worker, 10000),
    ok.

do_suspend(_Pid, 0) ->
    ok;
do_suspend(Pid, N) ->
    %% Suspend a process and test that it is suspended.
    true = erlang:suspend_process(Pid),
    {status, suspended} = process_info(Pid, status),

    %% Unsuspend the process and make sure it starts working.
    true = erlang:resume_process(Pid),
    case process_info(Pid, status) of
        {status, runnable} -> ok;
        {status, running} -> ok;
        {status, garbage_collecting} -> ok;
        ST -> ct:fail(ST)
    end,
    erlang:yield(),
    do_suspend(Pid, N-1).



mutual_suspend(Config) when is_list(Config) ->
    TimeoutSecs = 5*60,
    ct:timetrap({seconds, TimeoutSecs}),
    Parent = self(),
    Fun = fun () ->
                  receive
                      {go, Pid} ->
                          do_mutual_suspend(Pid, 100000)
                  end,
                  Parent ! {done, self()},
                  receive after infinity -> ok end
          end,
    P1 = spawn_link(Fun),
    P2 = spawn_link(Fun),
    T1 = erlang:start_timer((TimeoutSecs - 5)*1000, self(), oops),
    T2 = erlang:start_timer((TimeoutSecs - 5)*1000, self(), oops),
    P1 ! {go, P2},
    P2 ! {go, P1},
    Res1 = receive
               {done, P1} -> done;
               {timeout,T1,_} -> timeout
           end,
    Res2 = receive
               {done, P2} -> done;
               {timeout,T2,_} -> timeout
           end,
    P1S = process_info(P1, status),
    P2S = process_info(P2, status),
    io:format("P1S=~p P2S=~p", [P1S, P2S]),
    false = {status, suspended} == P1S,
    false = {status, suspended} == P2S,
    unlink(P1), exit(P1, bang),
    unlink(P2), exit(P2, bang),
    done = Res1,
    done = Res2,
    ok.

do_mutual_suspend(_Pid, 0) ->
    ok;
do_mutual_suspend(Pid, N) ->
    %% Suspend a process and test that it is suspended.
    true = erlang:suspend_process(Pid),
    {status, suspended} = process_info(Pid, status),
    %% Unsuspend the process.
    true = erlang:resume_process(Pid),
    do_mutual_suspend(Pid, N-1).		

suspend_exit(Config) when is_list(Config) ->
    ct:timetrap({minutes, 2}),
    rand:seed(exsplus, {4711,17,4711}),
    do_suspend_exit(5000),
    ok.

do_suspend_exit(0) ->
    ok;
do_suspend_exit(N) ->
    Work = rand:uniform(50),
    Parent = self(),
    {Suspendee, Mon2}
    = spawn_monitor(fun () ->
                            suspend_exit_work(Work),
                            exit(normal)
                    end),
    {Suspender, Mon1}
    = spawn_monitor(
        fun () ->
                suspend_exit_work(Work div 2),
                Parent ! {doing_suspend, self()},
                case catch erlang:suspend_process(Suspendee) of
                    {'EXIT', _} ->
                        ok;
                    true ->
                        erlang:resume_process(Suspendee)
                end
        end),
    receive
        {doing_suspend, Suspender} ->
            case N rem 2 of
                0 -> exit(Suspender, bang);
                1 -> ok
            end
    end,
    receive {'DOWN', Mon1, process, Suspender, _} -> ok end,
    receive {'DOWN', Mon2, process, Suspendee, _} -> ok end,
    do_suspend_exit(N-1).




suspend_exit_work(0) ->
    ok;
suspend_exit_work(N) ->
    process_info(self()),
    suspend_exit_work(N-1).

-define(CHK_SUSPENDED(P,B), chk_suspended(P, B, ?LINE)).

chk_suspended(P, Bool, Line) ->
    {Bool, Line} = {({status, suspended} == process_info(P, status)), Line}.

suspender_exit(Config) when is_list(Config) ->
    ct:timetrap({minutes, 3}),
    P1 = spawn_link(fun () -> receive after infinity -> ok end end),
    {'EXIT', _} = (catch erlang:resume_process(P1)),
    {P2, M2} = spawn_monitor(
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
    receive
        {'DOWN', M2,process,P2,R2} ->
            bang = R2,
            ?CHK_SUSPENDED(P1, false)
    end,
    Parent = self(),
    {P3, M3} = spawn_monitor(
                 fun () ->
                         erlang:suspend_process(P1),
                         ?CHK_SUSPENDED(P1, true),
                         Parent ! self(),
                         receive after infinity -> ok end
                 end),
    {P4, M4} = spawn_monitor(
                 fun () ->
                         erlang:suspend_process(P1),
                         ?CHK_SUSPENDED(P1, true),
                         Parent ! self(),
                         receive after infinity -> ok end
                 end),
    {P5, M5} = spawn_monitor(
                 fun () ->
                         erlang:suspend_process(P1),
                         ?CHK_SUSPENDED(P1, true),
                         Parent ! self(),
                         receive after infinity -> ok end
                 end),
    {P6, M6} = spawn_monitor(
                 fun () ->
                         erlang:suspend_process(P1),
                         ?CHK_SUSPENDED(P1, true),
                         Parent ! self(),
                         receive after infinity -> ok end
                 end),
    {P7, M7} = spawn_monitor(
                 fun () ->
                         erlang:suspend_process(P1),
                         ?CHK_SUSPENDED(P1, true),
                         Parent ! self(),
                         receive after infinity -> ok end
                 end),
    receive P3 -> ok end,
    receive P4 -> ok end,
    receive P5 -> ok end,
    receive P6 -> ok end,
    receive P7 -> ok end,
    ?CHK_SUSPENDED(P1, true),
    exit(P3, bang),
    receive
        {'DOWN',M3,process,P3,R3} ->
            bang = R3,
            ?CHK_SUSPENDED(P1, true)
    end,
    exit(P4, bang),
    receive
        {'DOWN',M4,process,P4,R4} ->
            bang = R4,
            ?CHK_SUSPENDED(P1, true)
    end,
    exit(P5, bang),
    receive
        {'DOWN',M5,process,P5,R5} ->
            bang = R5,
            ?CHK_SUSPENDED(P1, true)
    end,
    exit(P6, bang),
    receive
        {'DOWN',M6,process,P6,R6} ->
            bang = R6,
            ?CHK_SUSPENDED(P1, true)
    end,
    exit(P7, bang),
    receive
        {'DOWN',M7,process,P7,R7} ->
            bang = R7,
            ?CHK_SUSPENDED(P1, false)
    end,
    unlink(P1),
    exit(P1, bong),
    ok.

suspend_system_limit(Config) when is_list(Config) ->
    case os:getenv("ERL_EXTREME_TESTING") of
        "true" ->
            ct:timetrap({minutes, 3*60}),
            P = spawn_link(fun () -> receive after infinity -> ok end end),
            suspend_until_system_limit(P),
            unlink(P),
            exit(P, bye),
            ok;
        _ ->
            {skip, "Takes too long time for normal testing"}
    end.

suspend_until_system_limit(P) ->
    suspend_until_system_limit(P, 0, 0).

suspend_until_system_limit(P, N, M) ->
    NewM = case M of
               1 ->
                   ?CHK_SUSPENDED(P, true), 2;
               1000000 ->
                   erlang:display(N), 1;
               _ ->
                   M+1
           end,
    case catch erlang:suspend_process(P) of
        true ->
            suspend_until_system_limit(P, N+1, NewM);
        {'EXIT', R} when R == system_limit;
                         element(1, R) == system_limit ->
            io:format("system limit at ~p~n", [N]),
            resume_from_system_limit(P, N, 0);
        Error ->
            ct:fail(Error)
    end.

resume_from_system_limit(P, 0, _) ->
    ?CHK_SUSPENDED(P, false),
    {'EXIT', _} = (catch erlang:resume_process(P)),
    ok;
resume_from_system_limit(P, N, M) ->
    NewM = case M of
               1 ->
                   ?CHK_SUSPENDED(P, true), 2;
               1000000 ->
                   erlang:display(N), 1;
               _ ->
                   M+1
           end,
    erlang:resume_process(P),
    resume_from_system_limit(P, N-1, NewM).

-record(susp_info, {async = 0,
                    dbl_async = 0,
                    synced = 0,
                    async_once = 0}).

suspend_opts(Config) when is_list(Config) ->
    ct:timetrap({minutes, 3}),
    Self = self(),
    wait_for_empty_runq(10),
    Tok = spawn_link(fun () ->
                             Self ! self(),
                             tok_trace_loop(Self, 0, 1000000000)
                     end),
    TC = 1000,
    receive Tok -> ok end,
    SF = fun (N, #susp_info {async = A,
                             dbl_async = AA,
                             synced = S,
                             async_once = AO} = Acc) ->
                 erlang:suspend_process(Tok, [asynchronous]),
                 Res = case {suspend_count(Tok), N rem 4} of
                           {0, 2} ->
                               erlang:suspend_process(Tok,
                                                      [asynchronous]),
                               case suspend_count(Tok) of
                                   2 ->
                                       erlang:resume_process(Tok),
                                       Acc#susp_info{async = A+1};
                                   0 ->
                                       erlang:resume_process(Tok),
                                       Acc#susp_info{async = A+1,
                                                     dbl_async = AA+1}
                               end;
                           {0, 1} ->
                               erlang:suspend_process(Tok,
                                                      [asynchronous,
                                                       unless_suspending]),
                               case suspend_count(Tok) of
                                   1 ->
                                       Acc#susp_info{async = A+1};
                                   0 ->
                                       Acc#susp_info{async = A+1,
                                                     async_once = AO+1}
                               end;
                           {0, 0} ->
                               erlang:suspend_process(Tok,
                                                      [unless_suspending]),
                               1 = suspend_count(Tok),
                               Acc#susp_info{async = A+1,
                                             synced = S+1};
                           {0, _} ->
                               Acc#susp_info{async = A+1};
                           _ ->
                               Acc
                       end,
                 erlang:resume_process(Tok),
                 erlang:yield(),
                 Res
         end,
    SI = repeat_acc(SF, TC, #susp_info{}),
    erlang:suspend_process(Tok, [asynchronous]),
    %% Verify that it eventually suspends
    WaitTime0 = 10,
    WaitTime1 = case {erlang:system_info(debug_compiled),
                      erlang:system_info(lock_checking)} of
                    {false, false} ->
                        WaitTime0;
                    {false, true} ->
                        WaitTime0*5;
                    _ ->
                        WaitTime0*10
                end,
    WaitTime = case {erlang:system_info(schedulers_online),
                     erlang:system_info(logical_processors)} of
                   {Schdlrs, CPUs} when is_integer(CPUs),
                                        Schdlrs =< CPUs ->
                       WaitTime1;
                   _ ->
                       WaitTime1*10
               end,
    receive after WaitTime -> ok end,
    1 = suspend_count(Tok),
    erlang:suspend_process(Tok, [asynchronous]),
    2 = suspend_count(Tok),
    erlang:suspend_process(Tok, [asynchronous]),
    3 = suspend_count(Tok),
    erlang:suspend_process(Tok),
    4 = suspend_count(Tok),
    erlang:suspend_process(Tok),
    5 = suspend_count(Tok),
    erlang:suspend_process(Tok, [unless_suspending]),
    5 = suspend_count(Tok),
    erlang:suspend_process(Tok, [unless_suspending,
                                 asynchronous]),
    5 = suspend_count(Tok),
    erlang:resume_process(Tok),
    erlang:resume_process(Tok),
    erlang:resume_process(Tok),
    erlang:resume_process(Tok),
    1 = suspend_count(Tok),
    io:format("Main suspends: ~p~n"
              "Main async: ~p~n"
              "Double async: ~p~n"
              "Async once: ~p~n"
              "Synced: ~p~n",
              [TC,
               SI#susp_info.async,
               SI#susp_info.dbl_async,
               SI#susp_info.async_once,
               SI#susp_info.synced]),
    case erlang:system_info(schedulers_online) of
        1 ->
            ok;
        _ ->
            true = SI#susp_info.async =/= 0
    end,
    unlink(Tok),
    exit(Tok, bang),
    ok.

suspend_count(Suspendee) ->
    suspend_count(self(), Suspendee).

suspend_count(Suspender, Suspendee) ->
    {suspending, SList} = process_info(Suspender, suspending),

    case lists:keysearch(Suspendee, 1, SList) of
        {value, {_Suspendee, 0, 0}} ->
            ct:fail({bad_suspendee_list, SList});
        {value, {Suspendee, Count, 0}} when is_integer(Count), Count > 0 ->
            {status, suspended} = process_info(Suspendee, status),
            Count;
        {value, {Suspendee, 0, Outstanding}} when is_integer(Outstanding),
                                                  Outstanding > 0 ->
            0;
        false ->
            0;
        Error ->
            ct:fail({bad_suspendee_list, Error, SList})
    end.

repeat_acc(Fun, N, Acc) ->
    repeat_acc(Fun, 0, N, Acc).

repeat_acc(_Fun, N, N, Acc) ->
    Acc;
repeat_acc(Fun, N, M, Acc) ->
    repeat_acc(Fun, N+1, M, Fun(N, Acc)).

%% Tests that waiting process can be suspended
%% (bug in R2D and earlier; see OTP-1488).

%% Test that a waiting process can be suspended.
suspend_waiting(Config) when is_list(Config) ->
    Process = fun_spawn(fun process/0),
    receive after 1 -> ok end,
    true = erlang:suspend_process(Process),
    {status, suspended} = process_info(Process, status),
    ok.


%% Test that erlang:trace(new, true, ...) is cleared when tracer dies.
new_clear(Config) when is_list(Config) ->
    Tracer = proplists:get_value(receiver, Config),

    0 = erlang:trace(new, true, [send, {tracer, Tracer}]),
    {flags, [send]} = erlang:trace_info(new, flags),
    {tracer, Tracer} = erlang:trace_info(new, tracer),
    Mref = erlang:monitor(process, Tracer),
    true = exit(Tracer, done),
    receive
        {'DOWN',Mref,_,_,_} -> ok
    end,
    {flags, []} = erlang:trace_info(new, flags),
    {tracer, []} = erlang:trace_info(new, tracer),
    ok.



%% Test that erlang:trace(all, false, ...) works without tracer.
existing_clear(Config) when is_list(Config) ->
    Self = self(),

    Tracer = proplists:get_value(receiver, Config),
    N = erlang:trace(existing, true, [send, {tracer, Tracer}]),
    {flags, [send]} = erlang:trace_info(Self, flags),
    {tracer, Tracer} = erlang:trace_info(Self, tracer),
    M = erlang:trace(all, false, [all]),
    io:format("Started trace on ~p processes and stopped on ~p~n", 
              [N, M]),
    {flags, []} = erlang:trace_info(Self, flags),
    {tracer, []} = erlang:trace_info(Self, tracer),
    M = N, % Used to be N + 1, but from 19.0 the tracer is also traced

    ok.

%% Test that erlang:trace/3 can be called on processes where the
%% tracer has died. OTP-13928
tracer_die(Config) when is_list(Config) ->
    Proc = spawn_link(fun receiver/0),

    Tracer = spawn_link(fun receiver/0),
    timer:sleep(1),
    N = erlang:trace(existing, true, [send, {tracer, Tracer}]),
    {flags, [send]} = erlang:trace_info(Proc, flags),
    {tracer, Tracer} = erlang:trace_info(Proc, tracer),
    unlink(Tracer),
    exit(Tracer, die),

    Tracer2 = spawn_link(fun receiver/0),
    timer:sleep(1),
    N = erlang:trace(existing, true, [send, {tracer, Tracer2}]),
    {flags, [send]} = erlang:trace_info(Proc, flags),
    {tracer, Tracer2} = erlang:trace_info(Proc, tracer),
    unlink(Tracer2),
    exit(Tracer2, die),

    Tracer3 = spawn_link(fun receiver/0),
    timer:sleep(1),
    1 = erlang:trace(Proc, true, [send, {tracer, Tracer3}]),
    {flags, [send]} = erlang:trace_info(Proc, flags),
    {tracer, Tracer3} = erlang:trace_info(Proc, tracer),
    unlink(Tracer3),
    exit(Tracer3, die),

    ok.

%% Test that an invalid flag cause badarg
bad_flag(Config) when is_list(Config) ->
    %% A bad flag could deadlock the SMP emulator in erts-5.5
    {'EXIT', {badarg, _}} = (catch erlang:trace(new,
                                                true,
                                                [not_a_valid_flag])),
    ok.

%% Test erlang:trace_delivered/1
trace_delivered(Config) when is_list(Config) ->
    ct:timetrap({minutes, 1}),
    TokLoops = 10000,
    Go = make_ref(),
    Parent = self(),
    Tok = spawn(fun () ->
                        receive Go -> gone end,
                        tok_trace_loop(Parent, 0, TokLoops)
                end),
    1 = erlang:trace(Tok, true, [procs]),
    Mon = erlang:monitor(process, Tok),
    NoOfTraceMessages = 4*TokLoops + 1,
    io:format("Expect a total of ~p trace messages~n",
              [NoOfTraceMessages]),
    Tok ! Go,
    NoOfTraceMessages = drop_trace_until_down(Tok, Mon),
    receive
        Msg ->
            ct:fail({unexpected_message, Msg})
    after 1000 ->
              ok
    end.

%% This testcase checks that receive trace works on exit signal messages
%% when the sender of the exit signal is the process itself.
trap_exit_self_receive(Config) ->
    Parent = self(),
    Proc = spawn_link(fun() -> process(Parent) end),

    1 = erlang:trace(Proc, true, ['receive']),
    Proc ! {trap_exit_please, true},
    {trace, Proc, 'receive', {trap_exit_please, true}} = receive_first_trace(),

    %% Make the process call exit(self(), signal)
    Reason1 = make_ref(),
    Proc ! {exit_signal_please, Reason1},
    {trace, Proc, 'receive', {exit_signal_please, Reason1}} = receive_first_trace(),
    {trace, Proc, 'receive', {'EXIT', Proc, Reason1}} = receive_first_trace(),
    receive {Proc, {'EXIT', Proc, Reason1}} -> ok end,
    receive_nothing(),

    unlink(Proc),
    Reason2 = make_ref(),
    Proc ! {exit_please, Reason2},
    {trace, Proc, 'receive', {exit_please, Reason2}} = receive_first_trace(),
    receive_nothing(),
    ok.

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

%% Waits for and returns the first message in the message queue.

receive_first_trace() ->
    receive
	Any when element(1,Any) =:= trace; element(1,Any) =:= trace_ts -> Any
    end.

%% Ensures that there is no message in the message queue.

receive_nothing() ->
    receive
        Any ->
            ct:fail({unexpected_message, Any})
    after 100 ->
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
        {exit_signal_please, Reason} ->
            exit(self(), Reason),
            process(Dest);
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
    receiver(infinity).

receiver(Timeout) ->
    receiver(receive
		 {set_timeout, NewTimeout} -> NewTimeout;
		 _Any -> Timeout
	     after Timeout -> infinity  %% reset
	     end).

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
    io:format("WARNING: Unexpected runnable processes in system (waited ~p sec).~n"
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
