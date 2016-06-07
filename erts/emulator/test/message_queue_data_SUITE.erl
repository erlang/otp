%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

-export([all/0, suite/0]).
-export([basic/1, process_info_messages/1, total_heap_size/1]).

-export([basic_test/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [basic, process_info_messages, total_heap_size].

%%
%%
%% Test cases
%%
%%

basic(Config) when is_list(Config) ->

    basic_test(erlang:system_info(message_queue_data)),

    {ok, Node1} = start_node(Config, "+hmqd off_heap"),
    ok = rpc:call(Node1, ?MODULE, basic_test, [off_heap]),
    stop_node(Node1),

    {ok, Node2} = start_node(Config, "+hmqd on_heap"),
    ok = rpc:call(Node2, ?MODULE, basic_test, [on_heap]),
    stop_node(Node2),

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
    OnPid = spawn_opt(Fun, [{message_queue_data, on_heap}]),
    {total_heap_size, OnSize} = erlang:process_info(OnPid, total_heap_size),
    [OnPid ! lists:duplicate(N,N) || N <- lists:seq(1,100)],
    OnPid ! self(), receive ok -> ok end,
    {total_heap_size, OnSizeAfter} = erlang:process_info(OnPid, total_heap_size),
    ct:log("OnSize = ~p, OnSizeAfter = ~p",[OnSize, OnSizeAfter]),
    true = OnSize < OnSizeAfter,

    %% Test that off_heap messages do not grow the heap if they are not received
    OffPid = spawn_opt(Fun, [{message_queue_data, off_heap}]),
    {total_heap_size, OffSize} = erlang:process_info(OffPid, total_heap_size),
    [OffPid ! lists:duplicate(N,N) || N <- lists:seq(1,100)],
    OffPid ! self(), receive ok -> ok end,
    {total_heap_size, OffSizeAfter} = erlang:process_info(OffPid, total_heap_size),
    ct:log("OffSize = ~p, OffSizeAfter = ~p",[OffSize, OffSizeAfter]),
    true = OffSize == OffSizeAfter.

%%
%%
%% helpers
%%
%%

start_node(Config) ->
    start_node(Config, []).
start_node(Config, Opts) when is_list(Config), is_list(Opts) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, Opts++" -pa "++Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).
