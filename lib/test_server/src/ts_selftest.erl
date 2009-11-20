%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(ts_selftest).
-export([selftest/0]).

selftest() ->
    case node() of
	nonode@nohost ->
	    io:format("Sorry, you have to start this node distributed.~n"),
	    exit({error, node_not_distributed});
	_ ->
	    ok
    end,
    case catch ts:tests(test_server) of
	{'EXIT', _} ->
	    io:format("Test Server self test not availiable.");
	Other ->
	    selftest1()
    end.

selftest1() ->
    % Batch starts
    io:format("Selftest #1: Whole spec, batch mode:~n"),
    io:format("------------------------------------~n"),
    ts:run(test_server, [batch]),
    ok=check_result(1, "test_server.logs", 2),

    io:format("Selftest #2: One module, batch mode:~n"),
    io:format("------------------------------------~n"),
    ts:run(test_server, test_server_SUITE, [batch]),
    ok=check_result(2, "test_server_SUITE.logs", 2),

    io:format("Selftest #3: One testcase, batch mode:~n"),
    io:format("--------------------------------------~n"),
    ts:run(test_server, test_server_SUITE, msgs, [batch]),
    ok=check_result(3, "test_server_SUITE.logs", 0),

    % Interactive starts
    io:format("Selftest #4: Whole spec, interactive mode:~n"),
    io:format("------------------------------------------~n"),
    ts:run(test_server),
    kill_test_server(),
    ok=check_result(4, "test_server.logs", 2),

    io:format("Selftest #5: One module, interactive mode:~n"),
    io:format("------------------------------------------~n"),
    ts:run(test_server, test_server_SUITE),
    kill_test_server(),
    ok=check_result(5, "test_server_SUITE.logs", 2),

    io:format("Selftest #6: One testcase, interactive mode:~n"),
    io:format("--------------------------------------------~n"),
    ts:run(test_server, test_server_SUITE, msgs),
    kill_test_server(),
    ok=check_result(6, "test_server_SUITE.logs", 0),
    
    ok.

check_result(Test, TDir, ExpSkip) ->
    Dir=ts_lib:last_test(TDir),
    {Total, Failed, Skipped}=ts_reports:count_cases(Dir),
		io:format("Selftest #~p:",[Test]),
    case {Total, Failed, Skipped} of
	{_, 0, ExpSkip} ->        % 2 test cases should be skipped.
	    io:format("All ok.~n~n"),
	    ok;
	{_, _, _} ->
	    io:format("Not completely successful.~n~n"),
	    error
    end.


%% Wait for test server to get started.
kill_test_server() ->
    Node=list_to_atom("test_server@"++atom_to_list(hostname())),
    net_adm:ping(Node),
    case whereis(test_server_ctrl) of
	undefined ->
	    kill_test_server();
	Pid ->
	    kill_test_server(0, Pid)
    end.

%% Wait for test server to finish.
kill_test_server(30, Pid) ->
    exit(self(), test_server_is_dead);
kill_test_server(Num, Pid) ->
    case whereis(test_server_ctrl) of
	undefined ->
	    slave:stop(node(Pid));
	Pid ->
	    receive
	    after
		1000 ->
		    kill_test_server(Num+1, Pid)
	    end
    end.


hostname() ->
    list_to_atom(from($@, atom_to_list(node()))).
from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(H, []) -> [].
