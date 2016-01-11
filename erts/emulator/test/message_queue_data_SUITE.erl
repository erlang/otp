%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2014. All Rights Reserved.
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

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([basic/1, process_info_messages/1]).

-export([basic_test/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(Case, Config) ->
    ?line Dog=test_server:timetrap(test_server:minutes(2)),
    [{watchdog, Dog}, {testcase, Case}|Config].

end_per_testcase(_, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, process_info_messages].

groups() -> 
    [].

init_per_suite(Config) ->
%%    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
%%    erts_debug:set_internal_state(available_internal_state, false),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%
%%
%% Test cases
%%
%%

basic(Config) when is_list(Config) ->

    basic_test(erlang:system_info(message_queue_data)),

    {ok, Node1} = start_node(Config, "+xmqd off_heap"),
    ok = rpc:call(Node1, ?MODULE, basic_test, [off_heap]),
    stop_node(Node1),

    {ok, Node2} = start_node(Config, "+xmqd on_heap"),
    ok = rpc:call(Node2, ?MODULE, basic_test, [on_heap]),
    stop_node(Node2),

    {ok, Node3} = start_node(Config, "+xmqd mixed"),
    ok = rpc:call(Node3, ?MODULE, basic_test, [mixed]),
    stop_node(Node3),

    ok.

is_valid_mqd_value(off_heap) ->
    true;
is_valid_mqd_value(on_heap) ->
    true;
is_valid_mqd_value(mixed) ->
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
    on_heap = process_flag(message_queue_data, mixed),
    {message_queue_data, mixed} = process_info(self(), message_queue_data),
    mixed = process_flag(message_queue_data, Default),
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

    P4 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {message_queue_data, mixed}]),
    {message_queue_data, mixed} = process_info(P4, message_queue_data),
    unlink(P4),
    exit(P4, bye),

    {'EXIT', _} = (catch spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {message_queue_data, blapp}])),

    ok.

process_info_messages(Config) when is_list(Config) ->
    Tester = self(),
    P1 = spawn_opt(fun () ->
			   receive after 500 -> ok end,
			   mixed = process_flag(message_queue_data, off_heap),
			   Tester ! first,
			   receive after 500 -> ok end,
			   off_heap = process_flag(message_queue_data, on_heap),
			   Tester ! second,
			   receive after 500 -> ok end,
			   on_heap = process_flag(message_queue_data, mixed),
			   Tester ! third,
			   receive after 500 -> ok end,
			   mixed = process_flag(message_queue_data, off_heap),
			   Tester ! fourth,
			    
			   receive after infinity -> ok end
		   end,
		   [link, {message_queue_data, mixed}]),
    
    P1 ! "A",
    receive first -> ok end,
    P1 ! "B",
    receive second -> ok end,
    P1 ! "C",
    receive third -> ok end,
    P1 ! "D",
    receive fourth -> ok end,
    P1 ! "E",

    {messages, ["A", "B", "C", "D", "E"]} = process_info(P1, messages),

    P2 = spawn_opt(fun () ->
			   receive after 500 -> ok end,
			   mixed = process_flag(message_queue_data, off_heap),
			   Tester ! first,
			   receive after 500 -> ok end,
			   off_heap = process_flag(message_queue_data, on_heap),
			   Tester ! second,
			   receive after 500 -> ok end,
			   on_heap = process_flag(message_queue_data, mixed),
			   Tester ! third,
			   receive after 500 -> ok end,
			   mixed = process_flag(message_queue_data, off_heap),
			   Tester ! fourth,
			   receive after 500 -> ok end,

			   Tester ! process_info(self(), messages),

			   receive M1 -> M1 = "A" end,
			   receive M2 -> M2 = "B" end,
			   receive M3 -> M3 = "C" end,
			   receive M4 -> M4 = "D" end,
			   receive M5 -> M5 = "E" end,

			   Tester ! self()
		   end,
		   [link, {message_queue_data, mixed}]),

    P2 ! "A",
    receive first -> ok end,
    P2 ! "B",
    receive second -> ok end,
    P2 ! "C",
    receive third -> ok end,
    P2 ! "D",
    receive fourth -> ok end,
    P2 ! "E",

    receive
	Msg ->
	    {messages, ["A", "B", "C", "D", "E"]} = Msg
    end,
    
    receive P2 -> ok end,

    ok.

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
			++ atom_to_list(?config(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    ?t:start_node(Name, slave, [{args, Opts++" -pa "++Pa}]).

stop_node(Node) ->
    ?t:stop_node(Node).
