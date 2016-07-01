%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(ssl_dist_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(DEFAULT_TIMETRAP_SECS, 240).

-define(AWAIT_SSL_NODE_UP_TIMEOUT, 30000).

-record(node_handle,
	{connection_handler,
	 socket,
	 name,
	 nodename}
       ).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [basic, payload, plain_options, plain_verify_options, nodelay_option, 
     listen_port_options, listen_options, connect_options, use_interface,
     verify_fun_fail, verify_fun_pass, crl_check_pass, crl_check_fail,
     crl_check_best_effort, crl_cache_check_pass, crl_cache_check_fail].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config0) ->
    try crypto:start() of
	ok ->
	    %% Currently no ct function avilable for is_cover!
	    case test_server:is_cover() of
		false ->
		    Config = add_ssl_opts_config(Config0),
		    setup_certs(Config),
		    Config;
		true ->
		    {skip, "Can not be covered"}
	    end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(Config) ->
    application:stop(crypto),
    Config.

init_per_testcase(plain_verify_options = Case, Config) when is_list(Config) ->
    SslFlags = setup_dist_opts([{many_verify_opts, true} | Config]),
    Flags = case os:getenv("ERL_FLAGS") of
		false ->
		    os:putenv("ERL_FLAGS", SslFlags),
		    "";
		OldFlags ->
		    os:putenv("ERL_FLAGS", OldFlags ++ "" ++ SslFlags),
		    OldFlags
    end,
    common_init(Case, [{old_flags, Flags} | Config]);

init_per_testcase(Case, Config) when is_list(Config) ->
    common_init(Case, Config).

common_init(Case, Config) ->
    ct:timetrap({seconds, ?DEFAULT_TIMETRAP_SECS}),
    [{testcase, Case}|Config].

end_per_testcase(Case, Config) when is_list(Config) ->
    Flags = proplists:get_value(old_flags, Config),
    catch os:putenv("ERL_FLAGS", Flags),
    common_end(Case, Config).

common_end(_, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

basic() ->
    [{doc,"Test that two nodes can connect via ssl distribution"}].
basic(Config) when is_list(Config) ->
    gen_dist_test(basic_test, Config).

basic_test(NH1, NH2, _) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,
    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end),

    %% The test_server node has the same cookie as the ssl nodes
    %% but it should not be able to communicate with the ssl nodes
    %% via the erlang distribution.
    pang = net_adm:ping(Node1),
    pang = net_adm:ping(Node2),

    %% SSL nodes should not be able to communicate with the test_server node
    %% either (and ping should return eventually).
    TestServer = node(),
    pang = apply_on_ssl_node(NH1, fun () -> net_adm:ping(TestServer) end),
    pang = apply_on_ssl_node(NH2, fun () -> net_adm:ping(TestServer) end),

    %%
    %% Check that we are able to communicate over the erlang
    %% distribution between the ssl nodes.
    %%
    Ref = make_ref(),
    spawn(fun () ->
		  apply_on_ssl_node(
		    NH1,
		    fun () ->
			    tstsrvr_format("Hi from ~p!~n", [node()]),
			    send_to_tstcntrl({Ref, self()}),
			    receive
				{From, ping} ->
				    tstsrvr_format("Received ping ~p!~n", [node()]),
				    From ! {self(), pong}
			    end
		    end)
	  end),
     receive
	 {Ref, SslPid} ->
	     ok = apply_on_ssl_node(
		    NH2,
		    fun () ->
			    tstsrvr_format("Hi from ~p!~n", [node()]),
			    SslPid ! {self(), ping},
			    receive
				{SslPid, pong} ->
				    ok
			    end
		    end)
     end.

%%--------------------------------------------------------------------
payload() ->
    [{doc,"Test that send a lot of data between the ssl distributed noes"}].
payload(Config) when is_list(Config) ->
    gen_dist_test(payload_test, Config).

payload_test(NH1, NH2, _) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,

    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end),

    Ref = make_ref(),
    spawn(fun () ->
		  apply_on_ssl_node(
		    NH1,
		    fun () ->
			    send_to_tstcntrl({Ref, self()}),
			    receive
				{From, Msg} ->
				    From ! {self(), Msg}
			    end
		    end)
	  end),
     receive
	 {Ref, SslPid} ->
	     ok = apply_on_ssl_node(
		    NH2,
		    fun () ->
			    Msg = crypto:strong_rand_bytes(100000),
			    SslPid ! {self(), Msg},
			    receive
				{SslPid, Msg} ->
				    ok
			    end
		    end)
     end.

%%--------------------------------------------------------------------
plain_options() ->
    [{doc,"Test specifying additional options"}].
plain_options(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt server_secure_renegotiate true "
	"client_secure_renegotiate true "
	"server_reuse_sessions true client_reuse_sessions true  "
	"client_verify verify_none server_verify verify_none "
	"server_depth 1 client_depth 1 "
	"server_hibernate_after 500 client_hibernate_after 500",
    gen_dist_test(plain_options_test, [{additional_dist_opts, DistOpts} | Config]).

plain_options_test(NH1, NH2, _) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,

    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end).

%%--------------------------------------------------------------------
plain_verify_options() ->
    [{doc,"Test specifying additional options"}].
plain_verify_options(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt server_secure_renegotiate true "
	"client_secure_renegotiate true "
	"server_reuse_sessions true client_reuse_sessions true  "
	"server_hibernate_after 500 client_hibernate_after 500",
    gen_dist_test(plain_verify_options_test, [{additional_dist_opts, DistOpts} | Config]).

plain_verify_options_test(NH1, NH2, _) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,
    
    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),
    
    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end).


%%--------------------------------------------------------------------
nodelay_option() ->
    [{doc,"Test specifying dist_nodelay option"}].
nodelay_option(Config) ->
    try
	%% The default is 'true', so try setting it to 'false'.
	application:set_env(kernel, dist_nodelay, false),
	basic(Config)
    after
	application:unset_env(kernel, dist_nodelay)
    end.
%%--------------------------------------------------------------------

listen_port_options() ->
    [{doc, "Test specifying listening ports"}].
listen_port_options(Config) when is_list(Config) ->
    %% Start a node, and get the port number it's listening on.
    NH1 = start_ssl_node(Config),
    Node1 = NH1#node_handle.nodename,
    Name1 = lists:takewhile(fun(C) -> C =/= $@ end, atom_to_list(Node1)),
    {ok, NodesPorts} = apply_on_ssl_node(NH1, fun net_adm:names/0),
    {Name1, Port1} = lists:keyfind(Name1, 1, NodesPorts),
    
    %% Now start a second node, configuring it to use the same port
    %% number.
    PortOpt1 = "-kernel inet_dist_listen_min " ++ integer_to_list(Port1) ++
        " inet_dist_listen_max " ++ integer_to_list(Port1),
    
    try start_ssl_node([{additional_dist_opts, PortOpt1} | Config]) of
	#node_handle{} ->
	    %% If the node was able to start, it didn't take the port
	    %% option into account.
	    stop_ssl_node(NH1),
	    exit(unexpected_success)
    catch
	exit:{accept_failed, timeout} ->
	    %% The node failed to start, as expected.
	    ok
    end,
    
    %% Try again, now specifying a high max port.
    PortOpt2 = "-kernel inet_dist_listen_min " ++ integer_to_list(Port1) ++
	" inet_dist_listen_max 65535",
    NH2 = start_ssl_node([{additional_dist_opts, PortOpt2} | Config]),
    
    try 
	Node2 = NH2#node_handle.nodename,
	Name2 = lists:takewhile(fun(C) -> C =/= $@ end, atom_to_list(Node2)),
	{ok, NodesPorts2} = apply_on_ssl_node(NH2, fun net_adm:names/0),
	{Name2, Port2} = lists:keyfind(Name2, 1, NodesPorts2),
	
	%% The new port should be higher:
	if Port2 > Port1 ->
		ok;
	   true ->
		error({port, Port2, not_higher_than, Port1})
	end
    catch
	_:Reason ->
	    stop_ssl_node(NH2),
	    ct:fail(Reason)
    end,
    stop_ssl_node(NH2),
    success(Config).

%%--------------------------------------------------------------------
listen_options() ->
    [{doc, "Test inet_dist_listen_options"}].
listen_options(Config) when is_list(Config) ->
    try_setting_priority(fun do_listen_options/2, Config).

do_listen_options(Prio, Config) ->
    PriorityString0 = "[{priority,"++integer_to_list(Prio)++"}]",
    PriorityString =
	case os:cmd("echo [{a,1}]") of
	    "[{a,1}]"++_ ->
		PriorityString0;
	    _ ->
		%% Some shells need quoting of [{}]
		"'"++PriorityString0++"'"
	end,

    Options = "-kernel inet_dist_listen_options " ++ PriorityString,
    gen_dist_test(listen_options_test, [{prio, Prio}, {additional_dist_opts, Options} | Config]).
	
listen_options_test(NH1, NH2, Config) ->
    Prio = proplists:get_value(prio, Config),
    Node2 = NH2#node_handle.nodename,    
    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    PrioritiesNode1 =
	apply_on_ssl_node(NH1, fun get_socket_priorities/0),
    PrioritiesNode2 =
	apply_on_ssl_node(NH2, fun get_socket_priorities/0),
    
    Elevated1 = [P || P <- PrioritiesNode1, P =:= Prio],
    ct:pal("Elevated1: ~p~n", [Elevated1]),
    Elevated2 = [P || P <- PrioritiesNode2, P =:= Prio],
    ct:pal("Elevated2: ~p~n", [Elevated2]),
    [_|_] = Elevated1,
    [_|_] = Elevated2.

%%--------------------------------------------------------------------
connect_options() ->
    [{doc, "Test inet_dist_connect_options"}].
connect_options(Config) when is_list(Config) ->
    try_setting_priority(fun do_connect_options/2, Config).

do_connect_options(Prio, Config) ->
    PriorityString0 = "[{priority,"++integer_to_list(Prio)++"}]",
    PriorityString =
	case os:cmd("echo [{a,1}]") of
	    "[{a,1}]"++_ ->
		PriorityString0;
	    _ ->
		%% Some shells need quoting of [{}]
		"'"++PriorityString0++"'"
	end,

    Options = "-kernel inet_dist_connect_options " ++ PriorityString,
    gen_dist_test(connect_options_test, 
		  [{prio, Prio}, {additional_dist_opts, Options} | Config]).

connect_options_test(NH1, NH2, Config) ->
    Prio = proplists:get_value(prio, Config),
    Node2 = NH2#node_handle.nodename,
    
    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    PrioritiesNode1 =
	apply_on_ssl_node(NH1, fun get_socket_priorities/0),
    PrioritiesNode2 =
	apply_on_ssl_node(NH2, fun get_socket_priorities/0),

    Elevated1 = [P || P <- PrioritiesNode1, P =:= Prio],
    ct:pal("Elevated1: ~p~n", [Elevated1]),
    Elevated2 = [P || P <- PrioritiesNode2, P =:= Prio],
    ct:pal("Elevated2: ~p~n", [Elevated2]),
    %% Node 1 will have a socket with elevated priority.
    [_|_] = Elevated1,
    %% Node 2 will not, since it only applies to outbound connections.
    [] = Elevated2.

%%--------------------------------------------------------------------
use_interface() ->
    [{doc, "Test inet_dist_use_interface"}].
use_interface(Config) when is_list(Config) ->
    %% Force the node to listen only on the loopback interface.
    IpString = "'{127,0,0,1}'",
    Options = "-kernel inet_dist_use_interface " ++ IpString,

    %% Start a node, and get the port number it's listening on.
    NH1 = start_ssl_node([{additional_dist_opts, Options} | Config]),
  
    try
	Node1 = NH1#node_handle.nodename,
	Name = lists:takewhile(fun(C) -> C =/= $@ end, atom_to_list(Node1)),
	{ok, NodesPorts} = apply_on_ssl_node(NH1, fun net_adm:names/0),
	{Name, Port} = lists:keyfind(Name, 1, NodesPorts),
	
	%% Now find the socket listening on that port, and check its sockname.
	Sockets = apply_on_ssl_node(
		    NH1,
		    fun() ->
			    [inet:sockname(P) ||
				P <- inet_ports(),
				{ok, Port} =:= (catch inet:port(P))]
		    end),
	%% And check that it's actually listening on localhost.
	[{ok,{{127,0,0,1},Port}}] = Sockets
    catch 
	_:Reason ->
	    stop_ssl_node(NH1),
	    ct:fail(Reason)
    end,
    stop_ssl_node(NH1),
    success(Config).
%%--------------------------------------------------------------------
verify_fun_fail() ->
    [{doc,"Test specifying verify_fun with a function that always fails"}].
verify_fun_fail(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt "
        "server_verify verify_peer server_verify_fun "
	"\"{ssl_dist_SUITE,verify_fail_always,{}}\" "
        "client_verify verify_peer client_verify_fun "
	"\"{ssl_dist_SUITE,verify_fail_always,{}}\" ",
    gen_dist_test(verify_fun_fail_test, [{additional_dist_opts, DistOpts} | Config]).

verify_fun_fail_test(NH1, NH2, _) ->
    Node2 = NH2#node_handle.nodename,
    
    pang = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [] = apply_on_ssl_node(NH2, fun () -> nodes() end),

    %% Check that the function ran on the client node.
    [{verify_fail_always_ran, true}] =
        apply_on_ssl_node(NH1, fun () -> ets:tab2list(verify_fun_ran) end),
    %% On the server node, it wouldn't run, because the server didn't
    %% request a certificate from the client.
    undefined =
        apply_on_ssl_node(NH2, fun () -> ets:info(verify_fun_ran) end).



%%--------------------------------------------------------------------
verify_fun_pass() ->
    [{doc,"Test specifying verify_fun with a function that always succeeds"}].
verify_fun_pass(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt "
        "server_verify verify_peer server_verify_fun "
	"\"{ssl_dist_SUITE,verify_pass_always,{}}\" "
        "server_fail_if_no_peer_cert true "
        "client_verify verify_peer client_verify_fun "
	"\"{ssl_dist_SUITE,verify_pass_always,{}}\" ",
    gen_dist_test(verify_fun_pass_test, [{additional_dist_opts, DistOpts} | Config]).

verify_fun_pass_test(NH1, NH2, _) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,

    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end),

    %% Check that the function ran on the client node.
    [{verify_pass_always_ran, true}] =
        apply_on_ssl_node(NH1, fun () -> ets:tab2list(verify_fun_ran) end),
    %% Check that it ran on the server node as well.  The server
    %% requested and verified the client's certificate because we
    %% passed fail_if_no_peer_cert.
    [{verify_pass_always_ran, true}] =
        apply_on_ssl_node(NH2, fun () -> ets:tab2list(verify_fun_ran) end).

%%--------------------------------------------------------------------
crl_check_pass() ->
    [{doc,"Test crl_check with non-revoked certificate"}].
crl_check_pass(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt client_crl_check true",
    NewConfig =
        [{many_verify_opts, true}, {additional_dist_opts, DistOpts}] ++ Config,
    gen_dist_test(crl_check_pass_test, NewConfig).

crl_check_pass_test(NH1, NH2, Config) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,

    PrivDir = ?config(priv_dir, Config),
    cache_crls_on_ssl_nodes(PrivDir, ["erlangCA", "otpCA"], [NH1, NH2]),

    %% The server's certificate is not revoked, so connection succeeds.
    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end).

%%--------------------------------------------------------------------
crl_check_fail() ->
    [{doc,"Test crl_check with revoked certificate"}].
crl_check_fail(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt client_crl_check true",
    NewConfig =
        [{many_verify_opts, true},
         %% The server uses a revoked certificate.
         {server_cert_dir, "revoked"},
         {additional_dist_opts, DistOpts}] ++ Config,
    gen_dist_test(crl_check_fail_test, NewConfig).

crl_check_fail_test(NH1, NH2, Config) ->
    Node2 = NH2#node_handle.nodename,

    PrivDir = ?config(priv_dir, Config),
    cache_crls_on_ssl_nodes(PrivDir, ["erlangCA", "otpCA"], [NH1, NH2]),

    %% The server's certificate is revoked, so connection fails.
    pang = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [] = apply_on_ssl_node(NH2, fun () -> nodes() end).

%%--------------------------------------------------------------------
crl_check_best_effort() ->
    [{doc,"Test specifying crl_check as best_effort"}].
crl_check_best_effort(Config) when is_list(Config) ->
    DistOpts = "-ssl_dist_opt "
        "server_verify verify_peer server_crl_check best_effort",
    NewConfig =
        [{many_verify_opts, true}, {additional_dist_opts, DistOpts}] ++ Config,
   gen_dist_test(crl_check_best_effort_test, NewConfig).

crl_check_best_effort_test(NH1, NH2, _Config) ->
    %% We don't have the correct CRL at hand, but since crl_check is
    %% best_effort, we accept it anyway.
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,

    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end).

%%--------------------------------------------------------------------
crl_cache_check_pass() ->
    [{doc,"Test specifying crl_check with custom crl_cache module"}].
crl_cache_check_pass(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    NodeDir = filename:join([PrivDir, "Certs"]),
    DistOpts = "-ssl_dist_opt "
        "client_crl_check true "
        "client_crl_cache "
	"\"{ssl_dist_SUITE,{\\\"" ++ NodeDir ++ "\\\",[]}}\"",
    NewConfig =
        [{many_verify_opts, true}, {additional_dist_opts, DistOpts}] ++ Config,
    gen_dist_test(crl_cache_check_pass_test, NewConfig).

crl_cache_check_pass_test(NH1, NH2, _) ->
    Node1 = NH1#node_handle.nodename,
    Node2 = NH2#node_handle.nodename,

    pong = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [Node2] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [Node1] = apply_on_ssl_node(NH2, fun () -> nodes() end).

%%--------------------------------------------------------------------
crl_cache_check_fail() ->
    [{doc,"Test custom crl_cache module with revoked certificate"}].
crl_cache_check_fail(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    NodeDir = filename:join([PrivDir, "Certs"]),
    DistOpts = "-ssl_dist_opt "
        "client_crl_check true "
        "client_crl_cache "
	"\"{ssl_dist_SUITE,{\\\"" ++ NodeDir ++ "\\\",[]}}\"",
    NewConfig =
        [{many_verify_opts, true},
         %% The server uses a revoked certificate.
         {server_cert_dir, "revoked"},
         {additional_dist_opts, DistOpts}] ++ Config,

    gen_dist_test(crl_cache_check_fail_test, NewConfig).

crl_cache_check_fail_test(NH1, NH2, _) ->
    Node2 = NH2#node_handle.nodename,
    pang = apply_on_ssl_node(NH1, fun () -> net_adm:ping(Node2) end),

    [] = apply_on_ssl_node(NH1, fun () -> nodes() end),
    [] = apply_on_ssl_node(NH2, fun () -> nodes() end).
%%--------------------------------------------------------------------
%%% Internal functions -----------------------------------------------
%%--------------------------------------------------------------------
gen_dist_test(Test, Config) ->
    NH1 = start_ssl_node(Config),
    NH2 = start_ssl_node(Config),
    try 
	?MODULE:Test(NH1, NH2, Config)
    catch
	_:Reason ->
	    stop_ssl_node(NH1),
	    stop_ssl_node(NH2),
	    ct:fail(Reason)
    end,
    stop_ssl_node(NH1),
    stop_ssl_node(NH2),	
    success(Config).

%% ssl_node side api
%%

tstsrvr_format(Fmt, ArgList) ->
    send_to_tstsrvr({format, Fmt, ArgList}).

send_to_tstcntrl(Message) ->
    send_to_tstsrvr({message, Message}).

try_setting_priority(TestFun, Config) ->
    Prio = 1,
    case gen_udp:open(0, [{priority,Prio}]) of
	{ok,Socket} ->
	    case inet:getopts(Socket, [priority]) of
		{ok,[{priority,Prio}]} ->
		    ok = gen_udp:close(Socket),
		    TestFun(Prio, Config);
		_ ->
		    ok = gen_udp:close(Socket),
		    {skip,
		     "Can not set priority "++integer_to_list(Prio)++
			 " on socket"}
	    end;
	{error,_} ->
	    {skip, "Can not set priority on socket"}
    end.

get_socket_priorities() ->
    [Priority ||
	{ok,[{priority,Priority}]} <-
	    [inet:getopts(Port, [priority]) || Port <- inet_ports()]].

inet_ports() ->
     [Port || Port <- erlang:ports(),
              element(2, erlang:port_info(Port, name)) =:= "tcp_inet"].

%%
%% test_server side api
%%

apply_on_ssl_node(Node, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Ref = make_ref(),
    send_to_ssl_node(Node, {apply, self(), Ref, M, F, A}),
    receive
	{Ref, Result} ->
	    Result
    end.

apply_on_ssl_node(Node, Fun) when is_function(Fun, 0) ->
    Ref = make_ref(),
    send_to_ssl_node(Node, {apply, self(), Ref, Fun}),
    receive
	{Ref, Result} ->
	    Result
    end.

stop_ssl_node(#node_handle{connection_handler = Handler,
			   socket = Socket,
			   name = Name}) ->
    ?t:format("Trying to stop ssl node ~s.~n", [Name]),
    Mon = erlang:monitor(process, Handler),
    unlink(Handler),
    case gen_tcp:send(Socket, term_to_binary(stop)) of
	ok ->
	    receive
		{'DOWN', Mon, process, Handler, Reason} ->
		    case Reason of
			normal ->
			    ok;
			_ -> 
			    ct:pal("Down  ~p ~n", [Reason])
		    end
	    end;
	Error ->
	    erlang:demonitor(Mon, [flush]),
	    ct:pal("Warning  ~p ~n", [Error])
    end.

start_ssl_node(Config) ->
    start_ssl_node(Config, "").

start_ssl_node(Config, XArgs) ->
    Name = mk_node_name(Config),
    SSL = proplists:get_value(ssl_opts, Config),
    SSLDistOpts = setup_dist_opts(Config),
    start_ssl_node_raw(Name, SSL ++ " " ++ SSLDistOpts ++ XArgs).

start_ssl_node_raw(Name, Args) ->
    {ok, LSock} = gen_tcp:listen(0,
				 [binary, {packet, 4}, {active, false}]),
    {ok, ListenPort} = inet:port(LSock),
    CmdLine = mk_node_cmdline(ListenPort, Name, Args),
    ?t:format("Attempting to start ssl node ~ts: ~ts~n", [Name, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
	Port when is_port(Port) ->
	    unlink(Port),
	    erlang:port_close(Port),
	    case await_ssl_node_up(Name, LSock) of
		#node_handle{} = NodeHandle ->
		    ?t:format("Ssl node ~s started.~n", [Name]),
		    NodeName = list_to_atom(Name ++ "@" ++ host_name()),
		    NodeHandle#node_handle{nodename = NodeName};
		Error ->
		    exit({failed_to_start_node, Name, Error})
	    end;
	Error ->
	    exit({failed_to_start_node, Name, Error})
    end.

cache_crls_on_ssl_nodes(PrivDir, CANames, NHs) ->
    [begin
         File = filename:join([PrivDir, "Certs", CAName, "crl.pem"]),
         {ok, PemBin} = file:read_file(File),
         PemEntries = public_key:pem_decode(PemBin),
         CRLs = [ CRL || {'CertificateList', CRL, not_encrypted} 
                             <- PemEntries],
         ok = apply_on_ssl_node(NH, ssl_manager, insert_crls,
                                ["no_distribution_point", CRLs, dist])
     end
     || NH <- NHs, CAName <- CANames],
    ok.

%%
%% command line creation
%%

host_name() ->
    [$@ | Host] = lists:dropwhile(fun ($@) -> false; (_) -> true end,
				  atom_to_list(node())),
    Host.

mk_node_name(Config) ->
    N = erlang:unique_integer([positive]),
    Case = proplists:get_value(testcase, Config),
    atom_to_list(?MODULE)
	++ "_"
	++ atom_to_list(Case)
	++ "_"
	++ integer_to_list(N).

mk_node_cmdline(ListenPort, Name, Args) ->
    Static = "-detached -noinput",
    Pa = filename:dirname(code:which(?MODULE)),
    Prog = case catch init:get_argument(progname) of
	       {ok,[[P]]} -> P;
	       _ -> exit(no_progname_argument_found)
	   end,
    NameSw = case net_kernel:longnames() of
		 false -> "-sname ";
		 _ -> "-name "
	     end,
    {ok, Pwd} = file:get_cwd(),
    "\"" ++ Prog ++ "\" "
	++ Static ++ " "
	++ NameSw ++ " " ++ Name ++ " "
	++ "-pa " ++ Pa ++ " "
	++ "-run application start crypto -run application start public_key "
	++ "-eval 'net_kernel:verbose(1)' "
	++ "-run " ++ atom_to_list(?MODULE) ++ " cnct2tstsrvr "
	++ host_name() ++ " "
	++ integer_to_list(ListenPort) ++ " "
	++ Args ++ " "
	++ "-env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ Name ++ " "
	++ "-kernel error_logger \"{file,\\\"" ++ Pwd ++ "/error_log." ++ Name ++ "\\\"}\" "
	++ "-setcookie " ++ atom_to_list(erlang:get_cookie()).

%%
%% Connection handler test_server side
%%

await_ssl_node_up(Name, LSock) ->
    case gen_tcp:accept(LSock, ?AWAIT_SSL_NODE_UP_TIMEOUT) of
	timeout ->
	    gen_tcp:close(LSock),
	    ?t:format("Timeout waiting for ssl node ~s to come up~n",
		      [Name]),
	    timeout;
	{ok, Socket} ->
	    gen_tcp:close(LSock),
	    case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
		    check_ssl_node_up(Socket, Name, Bin);
		{error, closed} ->
		    gen_tcp:close(Socket),
		    exit({lost_connection_with_ssl_node_before_up, Name})
	    end;
	{error, Error} ->
	    gen_tcp:close(LSock),
	    exit({accept_failed, Error})
    end.

check_ssl_node_up(Socket, Name, Bin) ->
    case catch binary_to_term(Bin) of
	{'EXIT', _} ->
	    gen_tcp:close(Socket),
	    exit({bad_data_received_from_ssl_node, Name, Bin});
	{ssl_node_up, NodeName} ->
	    case list_to_atom(Name++"@"++host_name()) of
		NodeName ->
		    Parent = self(),
		    Go = make_ref(),
		    %% Spawn connection handler on test server side
		    Pid = spawn_link(
			    fun () ->
				    receive Go -> ok end,
				    tstsrvr_con_loop(Name, Socket, Parent)
			    end),
		    ok = gen_tcp:controlling_process(Socket, Pid),
		    Pid ! Go,
		    #node_handle{connection_handler = Pid,
				 socket = Socket,
				 name = Name};
		_ ->
		    exit({unexpected_ssl_node_connected, NodeName})
	    end;
	Msg ->
	    exit({unexpected_msg_instead_of_ssl_node_up, Name, Msg})
    end.

send_to_ssl_node(#node_handle{connection_handler = Hndlr}, Term) ->
    Hndlr ! {relay_to_ssl_node, term_to_binary(Term)},
    ok.

tstsrvr_con_loop(Name, Socket, Parent) ->
    inet:setopts(Socket,[{active,once}]),
    receive
	{relay_to_ssl_node, Data} when is_binary(Data) ->
	    case gen_tcp:send(Socket, Data) of
		ok ->
		    ok;
		_Error ->
		    gen_tcp:close(Socket),
		    exit({failed_to_relay_data_to_ssl_node, Name, Data})
	    end;
	{tcp, Socket, Bin} ->
	    case catch binary_to_term(Bin) of
		{'EXIT', _} ->
		    gen_tcp:close(Socket),
		    exit({bad_data_received_from_ssl_node, Name, Bin});
		{format, FmtStr, ArgList} ->
		    ?t:format(FmtStr, ArgList);
		{message, Msg} ->
		    ?t:format("Got message ~p", [Msg]),
		    Parent ! Msg;
		{apply_res, To, Ref, Res} ->
		    To ! {Ref, Res};
		bye ->
		    ?t:format("Ssl node ~s stopped.~n", [Name]),
		    gen_tcp:close(Socket),
		    exit(normal);
		Unknown ->
		    exit({unexpected_message_from_ssl_node, Name, Unknown})
	    end;
	{tcp_closed, Socket} ->
	    gen_tcp:close(Socket),
	    exit({lost_connection_with_ssl_node, Name})
    end,
    tstsrvr_con_loop(Name, Socket, Parent).

%%
%% Connection handler ssl_node side
%%

% cnct2tstsrvr() is called via command line arg -run ...
cnct2tstsrvr([Host, Port]) when is_list(Host), is_list(Port) ->
    %% Spawn connection handler on ssl node side
    ConnHandler
	= spawn(fun () ->
			case catch gen_tcp:connect(Host,
						   list_to_integer(Port),
						   [binary,
						    {packet, 4},
						    {active, false}]) of
			    {ok, Socket} ->
				notify_ssl_node_up(Socket),
				ets:new(test_server_info,
					[set,
					 public,
					 named_table,
					 {keypos, 1}]),
				ets:insert(test_server_info,
					   {test_server_handler, self()}),
				ssl_node_con_loop(Socket);
			    Error ->
				halt("Failed to connect to test server " ++
					 lists:flatten(io_lib:format("Host:~p ~n Port:~p~n Error:~p~n",
								     [Host, Port, Error])))
			end
		end),
    spawn(fun () ->
		  Mon = erlang:monitor(process, ConnHandler),
		  receive
		      {'DOWN', Mon, process, ConnHandler, Reason} ->
			  receive after 1000 -> ok end,
			  halt("test server connection handler terminated: " ++
				   lists:flatten(io_lib:format("~p", [Reason])))
		  end
	  end).

notify_ssl_node_up(Socket) ->
    case catch gen_tcp:send(Socket,
			    term_to_binary({ssl_node_up, node()})) of
	ok -> ok;
	_ -> halt("Failed to notify test server that I'm up")
    end.

send_to_tstsrvr(Term) ->
    case catch ets:lookup_element(test_server_info, test_server_handler, 2) of
	Hndlr when is_pid(Hndlr) ->
	    Hndlr ! {relay_to_test_server, term_to_binary(Term)}, ok;
	_ ->
	    receive after 200 -> ok end,
	    send_to_tstsrvr(Term)
    end.

ssl_node_con_loop(Socket) ->
    inet:setopts(Socket,[{active,once}]),
    receive
	{relay_to_test_server, Data} when is_binary(Data) ->
	    case gen_tcp:send(Socket, Data) of
		ok ->
		    ok;
		_Error ->
		    gen_tcp:close(Socket),
		    halt("Failed to relay data to test server")
	    end;
	{tcp, Socket, Bin} ->
	    case catch binary_to_term(Bin) of
		{'EXIT', _} ->
		    gen_tcp:close(Socket),
		    halt("test server sent me bad data");
		{apply, From, Ref, M, F, A} ->
		    spawn_link(
		      fun () ->
			      send_to_tstsrvr({apply_res,
					       From,
					       Ref,
					       (catch apply(M, F, A))})
			  end);
		{apply, From, Ref, Fun} ->
		    spawn_link(fun () ->
				       send_to_tstsrvr({apply_res,
							From,
							Ref,
							(catch Fun())})
			       end);
		stop ->
		    gen_tcp:send(Socket, term_to_binary(bye)),
		    gen_tcp:close(Socket),
		    init:stop(),
		    receive after infinity -> ok end;
		_Unknown ->
		    halt("test server sent me an unexpected message")
	    end;
	{tcp_closed, Socket} ->
	    halt("Lost connection to test server")
    end,
    ssl_node_con_loop(Socket).

%%
%% Setup ssl dist info
%%

rand_bin(N) ->
    rand_bin(N, []).

rand_bin(0, Acc) ->
    Acc;
rand_bin(N, Acc) ->
    rand_bin(N-1, [rand:uniform(256)-1|Acc]).

make_randfile(Dir) ->
    {ok, IoDev} = file:open(filename:join([Dir, "RAND"]), [write]),
    ok = file:write(IoDev, rand_bin(1024)),
    file:close(IoDev).

append_files(FileNames, ResultFileName) ->
    {ok, ResultFile} = file:open(ResultFileName, [write]),
    do_append_files(FileNames, ResultFile).

do_append_files([], RF) ->
    ok = file:close(RF);
do_append_files([F|Fs], RF) ->
    {ok, Data} = file:read_file(F),
    ok = file:write(RF, Data),
    do_append_files(Fs, RF).

setup_certs(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    NodeDir = filename:join([PrivDir, "Certs"]),
    RGenDir = filename:join([NodeDir, "rand_gen"]),
    ok = file:make_dir(NodeDir),
    ok = file:make_dir(RGenDir),
    make_randfile(RGenDir),
    {ok, _} = make_certs:all(RGenDir, NodeDir),
    SDir = filename:join([NodeDir, "server"]),
    SC = filename:join([SDir, "cert.pem"]),
    SK = filename:join([SDir, "key.pem"]),
    SKC = filename:join([SDir, "keycert.pem"]),
    append_files([SK, SC], SKC),
    CDir = filename:join([NodeDir, "client"]),
    CC = filename:join([CDir, "cert.pem"]),
    CK = filename:join([CDir, "key.pem"]),
    CKC = filename:join([CDir, "keycert.pem"]),
    append_files([CK, CC], CKC).

setup_dist_opts(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    Dhfile = filename:join([DataDir, "dHParam.pem"]),
    NodeDir = filename:join([PrivDir, "Certs"]),
    SDir = filename:join([NodeDir, proplists:get_value(server_cert_dir, Config, "server")]),
    CDir = filename:join([NodeDir, proplists:get_value(client_cert_dir, Config, "client")]),
    SC = filename:join([SDir, "cert.pem"]),
    SK = filename:join([SDir, "key.pem"]),
    SKC = filename:join([SDir, "keycert.pem"]),
    SCA = filename:join([CDir, "cacerts.pem"]),
    CC = filename:join([CDir, "cert.pem"]),
    CK = filename:join([CDir, "key.pem"]),
    CKC = filename:join([CDir, "keycert.pem"]),
    CCA = filename:join([SDir, "cacerts.pem"]),

    DistOpts = case  proplists:get_value(many_verify_opts, Config, false) of
		   false ->
		       "-proto_dist inet_tls "
			   ++ "-ssl_dist_opt server_certfile " ++ SKC ++ " "
			   ++ "-ssl_dist_opt client_certfile " ++ CKC ++ " ";
		   true ->
		       case os:type() of
			   {win32, _} ->
			       "-proto_dist inet_tls "
				   ++ "-ssl_dist_opt server_certfile " ++ SKC ++ " "
				   ++ "-ssl_dist_opt server_cacertfile " ++ SCA ++ " "
				   ++ "-ssl_dist_opt server_verify verify_peer "
				   ++ "-ssl_dist_opt server_fail_if_no_peer_cert true "
				   ++ "-ssl_dist_opt server_ciphers DHE-RSA-AES256-SHA\:DHE-RSA-AES128-SHA "
				   ++ "-ssl_dist_opt server_dhfile " ++ Dhfile ++ " "
				   ++ "-ssl_dist_opt client_certfile " ++ CKC ++ " "
				   ++ "-ssl_dist_opt client_cacertfile " ++ CCA ++ " "
				   ++ "-ssl_dist_opt client_verify verify_peer "
				   ++ "-ssl_dist_opt client_ciphers DHE-RSA-AES256-SHA\:DHE-RSA-AES128-SHA ";
			   _ ->
			       "-proto_dist inet_tls "
				   ++ "-ssl_dist_opt server_certfile " ++ SC ++ " "
				   ++ "-ssl_dist_opt server_keyfile " ++ SK ++ " "
				   ++ "-ssl_dist_opt server_cacertfile " ++ SCA ++ " "
				   ++ "-ssl_dist_opt server_verify verify_peer "
				   ++ "-ssl_dist_opt server_fail_if_no_peer_cert true "
				   ++ "-ssl_dist_opt server_ciphers DHE-RSA-AES256-SHA\:DHE-RSA-AES128-SHA "
				   ++ "-ssl_dist_opt server_dhfile " ++ Dhfile ++ " "
				   ++ "-ssl_dist_opt client_certfile " ++ CC ++ " "
				   ++ "-ssl_dist_opt client_keyfile " ++ CK ++ " "
				   ++ "-ssl_dist_opt client_cacertfile " ++ CCA ++ " "
				   ++ "-ssl_dist_opt client_verify verify_peer "
				   ++ "-ssl_dist_opt client_ciphers DHE-RSA-AES256-SHA\:DHE-RSA-AES128-SHA "
		       end
	       end,
    MoreOpts = proplists:get_value(additional_dist_opts, Config, []),
    DistOpts ++ MoreOpts.

%%
%% Start scripts etc...
%%

add_ssl_opts_config(Config) ->
    %%
    %% Start with boot scripts if on an installed system; otherwise,
    %% just point out ssl ebin with -pa.
    %%
    try
	Dir = proplists:get_value(priv_dir, Config),
	LibDir = code:lib_dir(),
	Apps = application:which_applications(),
	{value, {stdlib, _, STDL_VSN}} = lists:keysearch(stdlib, 1, Apps),
	{value, {kernel, _, KRNL_VSN}} = lists:keysearch(kernel, 1, Apps),
	StdlDir = filename:join([LibDir, "stdlib-" ++ STDL_VSN]),
	KrnlDir = filename:join([LibDir, "kernel-" ++ KRNL_VSN]),
	{ok, _} = file:read_file_info(StdlDir),
	{ok, _} = file:read_file_info(KrnlDir),
	SSL_VSN = vsn(ssl),
	VSN_CRYPTO = vsn(crypto),
	VSN_PKEY = vsn(public_key),

	SslDir = filename:join([LibDir, "ssl-" ++ SSL_VSN]),
	{ok, _} = file:read_file_info(SslDir),
	%% We are using an installed otp system, create the boot script.
	Script = filename:join(Dir, atom_to_list(?MODULE)),
	{ok, RelFile} = file:open(Script ++ ".rel", [write]),
        io:format(RelFile,
		  "{release, ~n"
		  " {\"SSL distribution test release\", \"~s\"},~n"
		  " {erts, \"~s\"},~n"
		  " [{kernel, \"~s\"},~n"
		  "  {stdlib, \"~s\"},~n"
		  "  {crypto, \"~s\"},~n"
		  "  {public_key, \"~s\"},~n"
		  "  {ssl, \"~s\"}]}.~n",
		  [case catch erlang:system_info(otp_release) of
		       {'EXIT', _} -> "R11B";
		       Rel -> Rel
		   end,
		   erlang:system_info(version),
		   KRNL_VSN,
		   STDL_VSN,
		   VSN_CRYPTO,
		   VSN_PKEY,
		   SSL_VSN]),
	ok = file:close(RelFile),
	ok = systools:make_script(Script, []),
	[{ssl_opts, "-boot " ++ Script} | Config]
    catch
	_:_ ->
	    [{ssl_opts, "-pa \"" ++ filename:dirname(code:which(ssl))++"\""}
	     | add_comment_config(
		 "Bootscript wasn't used since the test wasn't run on an "
		 "installed OTP system.",
		 Config)]
    end.

%%
%% Add common comments to config
%%

add_comment_config(Comment, []) ->
    [{comment, Comment}];
add_comment_config(Comment, [{comment, OldComment} | Cs]) ->
    [{comment, Comment ++ " " ++ OldComment} | Cs];
add_comment_config(Comment, [C|Cs]) ->
    [C|add_comment_config(Comment, Cs)].

%%
%% Call when test case success
%%

success(Config) ->
    case lists:keysearch(comment, 1, Config) of
	{value, {comment, _} = Res} -> Res;
	_ -> ok
    end.

vsn(App) ->
    application:start(App),
    try
	{value,
	 {ssl,
	  _,
	  VSN}} = lists:keysearch(App,
				  1,
				  application:which_applications()),
	VSN
     after
	 application:stop(ssl)
     end.

verify_fail_always(_Certificate, _Event, _State) ->
    %% Create an ETS table, to record the fact that the verify function ran.
    %% Spawn a new process, to avoid the ETS table disappearing.
    Parent = self(),
    spawn(
      fun() ->
	      ets:new(verify_fun_ran, [public, named_table]),
	      ets:insert(verify_fun_ran, {verify_fail_always_ran, true}),
	      Parent ! go_ahead,
	      timer:sleep(infinity)
      end),
    receive go_ahead -> ok end,
    {fail, bad_certificate}.

verify_pass_always(_Certificate, _Event, State) ->
    %% Create an ETS table, to record the fact that the verify function ran.
    %% Spawn a new process, to avoid the ETS table disappearing.
    Parent = self(),
    spawn(
      fun() ->
	      ets:new(verify_fun_ran, [public, named_table]),
	      ets:insert(verify_fun_ran, {verify_pass_always_ran, true}),
	      Parent ! go_ahead,
	      timer:sleep(infinity)
      end),
    receive go_ahead -> ok end,
    {valid, State}.

%% ssl_crl_cache_api callbacks
lookup(_DistributionPoint, _DbHandle) ->
    not_available.

select({rdnSequence, NameParts}, {NodeDir, _}) ->
    %% Extract the CN from the issuer name...
    [CN] = [CN ||
               [#'AttributeTypeAndValue'{
                   type = ?'id-at-commonName',
                   value = <<_, _, CN/binary>>}] <- NameParts],
    %% ...and use that as the directory name to find the CRL.
    error_logger:info_report([{found_cn, CN}]),
    CRLFile = filename:join([NodeDir, CN, "crl.pem"]),
    {ok, PemBin} = file:read_file(CRLFile),
    PemEntries = public_key:pem_decode(PemBin),
    CRLs = [ CRL || {'CertificateList', CRL, not_encrypted} 
                        <- PemEntries],
    CRLs.

fresh_crl(_DistributionPoint, CRL) ->
    CRL.
