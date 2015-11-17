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

-module(off_heap_message_queue_SUITE).

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

    basic_test(erlang:system_info(off_heap_message_queue)),

    {ok, Node1} = start_node(Config, "+xohmq true"),
    ok = rpc:call(Node1, ?MODULE, basic_test, [true]),
    stop_node(Node1),

    {ok, Node2} = start_node(Config, "+xohmq false"),
    ok = rpc:call(Node2, ?MODULE, basic_test, [false]),
    stop_node(Node2),
    ok.

basic_test(Default) ->

    Default = erlang:system_info(off_heap_message_queue),
    true = (Default == true) orelse (Default == false),

    {off_heap_message_queue, Default} = process_info(self(), off_heap_message_queue),
    Default = process_flag(off_heap_message_queue, true),
    {off_heap_message_queue, true} = process_info(self(), off_heap_message_queue),
    true = process_flag(off_heap_message_queue, false),
    {off_heap_message_queue, false} = process_info(self(), off_heap_message_queue),
    false = process_flag(off_heap_message_queue, Default),
    {'EXIT', _} = (catch process_flag(off_heap_message_queue, blupp)),

    P1 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link]),
    {off_heap_message_queue, Default} = process_info(P1, off_heap_message_queue),
    unlink(P1),
    exit(P1, bye),

    P2 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {off_heap_message_queue, false}]),
    {off_heap_message_queue, false} = process_info(P2, off_heap_message_queue),
    unlink(P2),
    exit(P2, bye),

    P3 = spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {off_heap_message_queue, true}]),
    {off_heap_message_queue, true} = process_info(P3, off_heap_message_queue),
    unlink(P3),
    exit(P3, bye),

    {'EXIT', _} = (catch spawn_opt(fun () -> receive after infinity -> ok end end,
		   [link, {off_heap_message_queue, blapp}])),

    ok.

process_info_messages(Config) when is_list(Config) ->
    Tester = self(),
    P1 = spawn_opt(fun () ->
			   receive after 500 -> ok end,
			   false = process_flag(off_heap_message_queue, true),
			   Tester ! first,
			   receive after 500 -> ok end,
			   true = process_flag(off_heap_message_queue, false),
			   Tester ! second,
			   receive after 500 -> ok end,
			   false = process_flag(off_heap_message_queue, true),
			   Tester ! third,
			   receive after 500 -> ok end,
			   true = process_flag(off_heap_message_queue, false),
			   Tester ! fourth,
			    
			   receive after infinity -> ok end
		   end,
		   [link, {off_heap_message_queue, false}]),
    
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
			   false = process_flag(off_heap_message_queue, true),
			   Tester ! first,
			   receive after 500 -> ok end,
			   true = process_flag(off_heap_message_queue, false),
			   Tester ! second,
			   receive after 500 -> ok end,
			   false = process_flag(off_heap_message_queue, true),
			   Tester ! third,
			   receive after 500 -> ok end,
			   true = process_flag(off_heap_message_queue, false),
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
		   [link, {off_heap_message_queue, false}]),

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
