%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%

%%----------------------------------------------------------------------
%% Purpose : Test suite for erl-client/c-server
%%----------------------------------------------------------------------


-module(erl_client_c_server_proto_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_testcase/2, end_per_testcase/2,all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, init_per_group/2,end_per_group/2, void_test/1,
	 long_test/1, longlong_test/1, ushort_test/1, ulong_test/1,
	 ulonglong_test/1, double_test/1, char_test/1, wchar_test/1,
	 octet_test/1, bool_test/1, struct_test/1, struct2_test/1,
	 seq1_test/1, seq2_test/1, seq3_test/1, seq4_test/1,
	 seq5_test/1, array1_test/1, array2_test/1, enum_test/1,
	 string1_test/1, string2_test/1, string3_test/1,
	 string4_test/1, pid_test/1, port_test/1, ref_test/1,
	 term_test/1, typedef_test/1, inline_sequence_test/1,
	 term_sequence_test/1, term_struct_test/1, wstring1_test/1]).

-define(DEFAULT_TIMEOUT, 20000).
-define(PORT_TIMEOUT, 15000).
-define(CALL_TIMEOUT, 5000).

-define(C_SERVER_NODE_NAME, idl_c_server_test).
  
%% Add/remove code path and watchdog before/after each test case.
%%
init_per_testcase(_Case, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    code:add_patha(DataDir),

    %% Since other test suites use the module m_i, we have
    %% to make sure we are using the right m_i module.
    code:purge(m_i),
    code:load_file(m_i),

    WatchDog = test_server:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, WatchDog}| Config].

end_per_testcase(_Case, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    code:del_path(DataDir),
    WatchDog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
[void_test, long_test, longlong_test, ushort_test,
 ulong_test, ulonglong_test, double_test, char_test,
 wchar_test, octet_test, bool_test, struct_test,
 struct2_test, seq1_test, seq2_test, seq3_test,
 seq4_test, seq5_test, array1_test, array2_test,
 enum_test, string1_test, string2_test, string3_test,
 string4_test, pid_test, port_test, ref_test, term_test,
 typedef_test, inline_sequence_test, term_sequence_test,
 term_struct_test, wstring1_test].

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


array1_test(Config) ->
    do_test(array1_test, Config). 

array2_test(Config) ->
    do_test(array2_test, Config).

bool_test(Config) ->
    do_test(bool_test, Config).

char_test(Config) ->
    do_test(char_test, Config).

double_test(Config) ->
    do_test(double_test, Config).

enum_test(Config) ->
    do_test(enum_test, Config).

inline_sequence_test(Config) ->
    do_test(inline_sequence_test, Config).

longlong_test(Config) ->
    do_test(longlong_test, Config).

long_test(Config) ->
    do_test(long_test, Config).

octet_test(Config) ->
    do_test(octet_test, Config).

pid_test(Config) ->
    do_test(pid_test, Config).

port_test(Config) ->
    do_test(port_test, Config).

ref_test(Config) ->
    do_test(ref_test, Config).

seq1_test(Config) ->
    do_test(seq1_test, Config).

seq2_test(Config) ->
    do_test(seq2_test, Config).

seq3_test(Config) ->
    do_test(seq3_test, Config).

seq4_test(Config) ->
    do_test(seq4_test, Config).

seq5_test(Config) ->
    do_test(seq5_test, Config).

string1_test(Config) ->
    do_test(string1_test, Config).

string2_test(Config) ->
    do_test(string2_test, Config).

string3_test(Config) ->
    do_test(string3_test, Config).

string4_test(Config) ->
    do_test(string4_test, Config).

struct2_test(Config) ->
    do_test(struct2_test, Config).

struct_test(Config) ->
    do_test(struct_test, Config).

term_sequence_test(Config) ->
    do_test(term_sequence_test, Config).

term_struct_test(Config) ->
    do_test(term_struct_test, Config).

term_test(Config) ->
    do_test(term_test, Config).

typedef_test(Config) ->
    do_test(typedef_test, Config).

ulonglong_test(Config) ->
    do_test(ulonglong_test, Config).

ulong_test(Config) ->
    do_test(ulong_test, Config).

ushort_test(Config) ->
    do_test(ushort_test, Config).

void_test(Config) ->
    do_test(void_test, Config).

wchar_test(Config) ->
    do_test(wchar_test, Config).

wstring1_test(Config) ->
    do_test(wstring1_test, Config).


do_test(Case, Config) ->
    %% Trap exits
    process_flag(trap_exit, true),
    Node = atom_to_list(node()),
    [_NodeName, HostName] = string:tokens(Node, "@"),
    DataDir = proplists:get_value(data_dir, Config),
    %% io:format("~p: data directory: ~p~n", [?MODULE, DataDir]),
    Cookie = atom_to_list(erlang:get_cookie()),
    ServerNodeName = atom_to_list(?C_SERVER_NODE_NAME), 
    %% Start C-server node as a port program. We wait for the node
    %% to connect to us.
    Cmd = filename:join([DataDir, "c_server"]) ++
	" -this-node-name " ++ ServerNodeName ++ 
	" -peer-node " ++ Node ++
	" -cookie " ++ Cookie, 
    Port = open_port({spawn, Cmd}, [exit_status, eof, stderr_to_stdout]),
    ServerNode = list_to_atom(ServerNodeName ++ "@" ++ HostName),
    Res = case wait_for_hidden_node(ServerNode) of
	      ok ->
		  %% Need a port for port_test and typedef_test
		  put(port_test_port, Port),
		  R = (catch erl_client:Case(ServerNode, ?CALL_TIMEOUT)),
		  case wait_for_completion(Port) of
		      {error, timeout} ->
			  kill_off_node(ServerNode);
		      _ ->
			  ok
		  end,
		  R;
	      {error, timeout} ->
		  case wait_for_completion(Port) of
		      {error, timeout} ->
			  kill_off_node(ServerNode);
		      _ ->
			  ok
		  end,
		  {error, timeout}
	  end,
    process_flag(trap_exit, false),
    true = Res.


%% Wait for eof *and* exit status, but return if exit status indicates
%% an error, or we have been waiting more than PORT_TIMEOUT seconds.
%%
wait_for_completion(Port) ->
    wait_for_completion(Port, 0).

wait_for_completion(Port, N) when N < 2 ->
    receive
	{Port, {data, Bytes}} ->
	    %% Relay output
	    io:format("~s", [Bytes]),
	    wait_for_completion(Port, N);
	{Port, {exit_status, 0}} ->
	    wait_for_completion(Port, N + 1);
	{Port, {exit_status, Status}} ->
	    {error, Status};
	{Port, eof} ->
	    wait_for_completion(Port, N + 1);
	{'EXIT', Port, Reason} ->
	    io:format("Port exited with reason: ~w~n", [Reason]),
	    wait_for_completion(Port, N);
	{'EXIT', From, Reason} ->
	    io:format("Got unexpected exit: ~p~n", [{'EXIT', From, Reason}]),
	    wait_for_completion(Port, N)
    after ?PORT_TIMEOUT ->
	    {error, timeout}
    end;
wait_for_completion(_, _) ->
    ok.
	    
wait_for_hidden_node(Node) ->
    Times = ?DEFAULT_TIMEOUT div 100,
    wait_for_hidden_node(Node, Times, 100).

wait_for_hidden_node(Node, Times, WaitTime) when Times > 0 ->
    io:format("Waiting for hidden node: ~p~n", [Node]), 
    case lists:member(Node, erlang:nodes(hidden)) of
	true ->
	    ok;
	false ->
	    delay(WaitTime),
	    wait_for_hidden_node(Node, Times - 1, WaitTime)
    end;
wait_for_hidden_node(_Node, _, _WaitTime) ->
    {error, timeout}.

kill_off_node(Node) ->
    catch rpc:cast(Node, erlang, halt, [1]).

delay(Time) ->
    receive
	after Time ->
		ok
	end.
    

	    

