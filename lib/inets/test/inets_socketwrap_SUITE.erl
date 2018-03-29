%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(inets_socketwrap_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

suite() -> 
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_httpd_fd].

init_per_suite(Config) ->
    case os:type() of
	{unix, linux} ->
	    Config;
	_ ->
	    {skip, linux_feature}
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) ->
    end_per_testcase(Case, Config),
    Config.

end_per_testcase(_, Config) ->
    inets:stop(),
    Config.

%%-------------------------------------------------------------------------
start_httpd_fd() ->
    [{doc, "Start/stop of httpd service with socket wrapper"}].
start_httpd_fd(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    HttpdConf = [{port, 80}, {ipfamily, inet},
		 {server_name, "httpd_fd_test"}, {server_root, PrivDir},
		 {document_root, PrivDir}, {bind_address, any}],
    case setup_node_info(node()) of
	{skip, _}  = Skip ->
	    Skip;
	{Node, NodeArg} ->
	    InetPort = inets_test_lib:inet_port(node()),
	    ct:pal("Node: ~p  Port ~p~n", [Node, InetPort]),
      	    Wrapper = filename:join(DataDir, "setuid_socket_wrap"),
	    Cmd = Wrapper ++  
		" -s -httpd_80,0:" ++ integer_to_list(InetPort) 
		++ " -p " ++ os:find_executable("erl") ++
		" -- " ++ NodeArg, 
	    ct:pal("cmd: ~p~n", [Cmd]),
	    case open_port({spawn, Cmd}, [stderr_to_stdout]) of 
	    	Port when is_port(Port) ->
		    wait_node_up(Node, 10),
		    ct:pal("~p", [rpc:call(Node, init, get_argument, [httpd_80])]),
		    ok  = rpc:call(Node, inets, start, []),
		    {ok, Pid} = rpc:call(Node, inets, start, [httpd, HttpdConf]),
		    [{port, InetPort}] = rpc:call(Node, httpd, info, [Pid, [port]]),
		    rpc:call(Node, erlang, halt, []);
		_  ->
		    ct:fail(open_port_failed)
	    end
    end.

%%-------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------
setup_node_info(nonode@nohost) ->
    {skip, needs_distributed_node};
setup_node_info(Node) ->
    Static = "-detached -noinput",
    Name = "inets_fd_test",
    NameSw = case net_kernel:longnames() of
		 false -> "-sname ";
		 _ -> "-name "
		     end,
    StrNode = 
	Static ++ " "
	++ NameSw ++ " " ++ Name ++ " "
	++ "-setcookie " ++ atom_to_list(erlang:get_cookie()),
	    [_, Location] = string:tokens(atom_to_list(Node), "$@"),
    TestNode =  Name ++ "@" ++ Location, 
    {list_to_atom(TestNode), StrNode}.

wait_node_up(Node, 0) ->
    ct:fail({failed_to_start_node, Node});
wait_node_up(Node, N) ->
    ct:pal("(Node ~p: net_adm:ping(~p)~n", [node(), Node]),
    case net_adm:ping(Node) of
	pong ->
	    ok;
	pang ->
	    ct:sleep(5000),
	    wait_node_up(Node, N-1)
    end.
