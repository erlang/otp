%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2014-2022. All Rights Reserved.
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

-module(message_queue_data_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([basic/1, process_info_messages/1, total_heap_size/1,
	 change_to_off_heap/1, change_to_off_heap_gc/1]).

-export([basic_test/1, id/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    erts_debug:set_internal_state(available_internal_state, false),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

all() ->
    [basic, process_info_messages, total_heap_size, change_to_off_heap,
     change_to_off_heap_gc].

%%
%%
%% Test cases
%%
%%

basic(Config) when is_list(Config) ->

    basic_test(erlang:system_info(message_queue_data)),

    {ok, Peer1, Node1} = ?CT_PEER(["+hmqd", "off_heap"]),
    ok = rpc:call(Node1, ?MODULE, basic_test, [off_heap]),
    peer:stop(Peer1),

    {ok, Peer2, Node2} = ?CT_PEER(["+hmqd", "on_heap"]),
    ok = rpc:call(Node2, ?MODULE, basic_test, [on_heap]),
    peer:stop(Peer2),

    ok.

is_valid_mqd_value(off_heap) ->
    true;
is_valid_mqd_value(on_heap) ->
    true;
is_valid_mqd_value(_) ->
    false.


basic_test(Default) ->

    Default = erlang:system_info(message_queue_data),
    true = is_valid_mqd_value(Default),

    {message_queue_data, Default} = process_info(self(), message_queue_data),
    Default = process_flag(message_queue_data, off_heap),
    {message_queue_data, off_heap} = process_info(self(), message_queue_data),
    off_heap = process_flag(message_queue_data, on_heap),
    {message_queue_data, on_heap} = process_info(self(), message_queue_data),
    {'EXIT', _} = (catch process_flag(message_queue_data, blupp)),

    P1 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link]),
    {message_queue_data, Default} = process_info(P1, message_queue_data),
    unlink(P1),
    exit(P1, bye),

    P2 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {message_queue_data, off_heap}]),
    {message_queue_data, off_heap} = process_info(P2, message_queue_data),
    unlink(P2),
    exit(P2, bye),

    P3 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {message_queue_data, on_heap}]),
    {message_queue_data, on_heap} = process_info(P3, message_queue_data),
    unlink(P3),
    exit(P3, bye),

    {'EXIT', _} = (catch spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {message_queue_data, blapp}])),

    ok.

process_info_messages(Config) when is_list(Config) ->
    Tester = self(),
    P1 = spawn_opt(fun () ->
			   receive after 500 -> ok end,
			   on_heap = process_flag(message_queue_data, off_heap),
			   Tester ! first,
			   receive after 500 -> ok end,
			   off_heap = process_flag(message_queue_data, on_heap),
			   Tester ! second,
			   receive after 500 -> ok end,
			   on_heap = process_flag(message_queue_data, off_heap),
			   Tester ! third,
			    
			   receive after infinity -> ok end
		   end,
		   [link, {message_queue_data, on_heap}]),
    
    P1 ! "A",
    receive first -> ok end,
    P1 ! "B",
    receive second -> ok end,
    P1 ! "C",
    receive third -> ok end,
    P1 ! "D",

    {messages, ["A", "B", "C", "D"]} = process_info(P1, messages),

    P2 = spawn_opt(fun () ->
			   receive after 500 -> ok end,
			   on_heap = process_flag(message_queue_data, off_heap),
			   Tester ! first,
			   receive after 500 -> ok end,
			   off_heap = process_flag(message_queue_data, on_heap),
			   Tester ! second,
			   receive after 500 -> ok end,
			   on_heap = process_flag(message_queue_data, off_heap),
			   Tester ! third,
			   receive after 500 -> ok end,

			   Tester ! process_info(self(), messages),

			   receive M1 -> M1 = "A" end,
			   receive M2 -> M2 = "B" end,
			   receive M3 -> M3 = "C" end,
			   receive M4 -> M4 = "D" end,

			   Tester ! self()
		   end,
		   [link, {message_queue_data, on_heap}]),

    P2 ! "A",
    receive first -> ok end,
    P2 ! "B",
    receive second -> ok end,
    P2 ! "C",
    receive third -> ok end,
    P2 ! "D",

    receive
	Msg ->
	    {messages, ["A", "B", "C", "D"]} = Msg
    end,
    
    receive P2 -> ok end,

    ok.

total_heap_size(_Config) ->

    Fun = fun F() -> receive Pid when is_pid(Pid) -> Pid ! ok,F() end end,

    %% Test that on_heap messages grow the heap even if they are not received
    OnPid = spawn_opt(Fun, [{message_queue_data, on_heap},link]),
    {total_heap_size, OnSize} = erlang:process_info(OnPid, total_heap_size),
    [OnPid ! lists:duplicate(N,N) || N <- lists:seq(1,100)],
    OnPid ! self(), receive ok -> ok end,
    {total_heap_size, OnSizeAfter} = erlang:process_info(OnPid, total_heap_size),
    ct:log("OnSize = ~p, OnSizeAfter = ~p",[OnSize, OnSizeAfter]),
    true = OnSize < OnSizeAfter,

    %% Test that off_heap messages do not grow the heap if they are not received
    OffPid = spawn_opt(Fun, [{message_queue_data, off_heap},link]),
    {total_heap_size, OffSize} = erlang:process_info(OffPid, total_heap_size),
    [OffPid ! lists:duplicate(N,N) || N <- lists:seq(1,100)],
    OffPid ! self(), receive ok -> ok end,
    {total_heap_size, OffSizeAfter} = erlang:process_info(OffPid, total_heap_size),
    ct:log("OffSize = ~p, OffSizeAfter = ~p",[OffSize, OffSizeAfter]),
    true = OffSize == OffSizeAfter.

change_to_off_heap(Config) when is_list(Config) ->
    %% Without seq-trace tokens on messages...
    change_to_off_heap_test(),
    %% With seq-trace tokens on messages...
    Tracer = start_seq_tracer(),
    try
	change_to_off_heap_test()
    after
	stop_seq_tracer(Tracer)
    end,
    ok.

change_to_off_heap_test() ->
    process_flag(message_queue_data, on_heap),
    process_flag(min_heap_size, 1000000),
    garbage_collect(),
    persistent_term:put(mqd_test_ref, make_ref()),
    try
	Alias = alias(),
	%% A lot of on-heap messages to convert in the change...
	Msgs = make_misc_messages(Alias, persistent_term:get(mqd_test_ref)),
	%% io:format("Msgs: ~p~n",[Msgs]),
	process_flag(message_queue_data, off_heap),
	wait_change_off_heap(),
	%% All messages in message queue have now been moved off-heap...
	RecvMsgs = recv_msgs(),
	unalias(Alias),
	%% io:format("RecvMsgs: ~p~n",[RecvMsgs]),
	true = Msgs =:= RecvMsgs,
	garbage_collect(),
	persistent_term:erase(mqd_test_ref),
	receive after 1000 -> ok end,
	garbage_collect(),
	true = Msgs =:= RecvMsgs
    after
 	persistent_term:erase(mqd_test_ref)
    end.

start_seq_tracer() ->
    Tester = self(),
    Go = make_ref(),
    Pid = spawn_link(fun () ->
			     seq_trace:set_system_tracer(self()),
			     Tester ! Go,
			     seq_trace_loop([])
		     end),
    receive
	Go ->
	    seq_trace:set_token(label,666),
	    seq_trace:set_token('receive',true),
	    Pid
    end.

stop_seq_tracer(Tracer) ->
    Ref = make_ref(),
    Tracer ! {stop, Ref, self()},
    receive
	{Ref, SeqTrace} ->
	    io:format("SeqTrace: ~p~n", [SeqTrace])
    end.

seq_trace_loop(SeqTrace) ->
    receive
	{seq_trace,_Label,_Info,_Ts} = ST->
	    seq_trace_loop([ST|SeqTrace]);
	{seq_trace,_Label,_Info} = ST ->
	    seq_trace_loop([ST|SeqTrace]);
	{stop,Ref,From} ->
	    From ! {Ref,lists:reverse(SeqTrace)}
    end.

make_misc_messages(Alias, Lit) ->
    process_flag(trap_exit, true),
    make_misc_messages(Alias, Lit, [], 100).

make_misc_messages(_Alias, _Lit, Msgs, N) when N =< 0 ->
    lists:flatten(lists:reverse(Msgs));
make_misc_messages(Alias, Lit, Msgs, N) ->
    M1 = Lit,
    M2 = {Lit, N},
    M3 = immediate,
    M4 = {not_immediate, N},
    exit(self(), tjena), %% Will become off-heap msg...
    %% Rest will become on-heap msgs...
    self() ! M1,
    self() ! M2,
    self() ! M3,
    self() ! M4,
    Alias ! M1,
    Alias ! M2,
    Alias ! M3,
    Alias ! M4,
    NewMsgs = [{'EXIT', self(), tjena},
	       M1, M2, M3, M4, M1, M2, M3, M4],
    make_misc_messages(Alias, Lit, [NewMsgs | Msgs], N-9).

%% Test that setting message queue to off_heap works if a GC is triggered
%% as the message queue if moved off heap. See GH-5933 for more details.
%% This testcase will most likely only fail in debug build.
change_to_off_heap_gc(_Config) ->
    Msg = {ok, lists:duplicate(20,20)},

    %% We test that this process can receive a message and when it is still
    %% in its external message queue we change the message queue data to
    %% off_heap and then GC.
    {Pid, Ref} = spawn_monitor(
            fun() ->
                    spinner(1, 10000),
                    process_flag(message_queue_data, off_heap),
                    garbage_collect(),
                    receive {ok, _M} -> ok end
            end),
    Pid ! Msg,
    receive
        {'DOWN',Ref,_,_,_} ->
            ok
    end.

%%
%%
%% helpers
%%
%%

wait_change_off_heap() ->
    %%
    %% Will wait until a change to off_heap message_queue_data
    %% has been made on current process if (and only if) it
    %% was previously changed on this process...
    %%
    %% Work with *current* implementation! This may change...
    %%
    erts_debug:set_internal_state(wait, thread_progress),
    %% We have now flushed later ops including later op that
    %% sent 'adjust msgq' signal to us. Now pass a message
    %% through our message queue. When received we know that
    %% that the 'adjust message queue' signal has been
    %% handled...
    %%
    Ref = make_ref(),
    self() ! Ref,
    receive Ref -> ok end.
    
recv_msgs() ->    
    recv_msgs([]).

recv_msgs(Msgs) ->
    receive
	Msg ->
	    recv_msgs([Msg|Msgs])
    after 0 ->
	    lists:reverse(Msgs)
    end.

%% This spinner needs to make sure that it does not allocate any memory
%% as a GC in here will break the test
spinner(_N, 0) -> ok;
spinner(N, M) -> spinner(?MODULE:id(N) div 1, M - 1).

id(N) -> N.
