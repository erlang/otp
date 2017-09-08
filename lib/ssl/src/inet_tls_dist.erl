%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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
-module(inet_tls_dist).

-export([childspecs/0]).
-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% Generalized dist API
-export([gen_listen/2, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_close/2, gen_select/2]).

-export([split_node/1, nodelay/0]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

%% -undef(trace).
%% -define(trace(Fmt,Args),
%%         erlang:display(
%%           [erlang:convert_time_unit(
%%              erlang:monotonic_time()
%%              - erlang:system_info(start_time), native, microsecond),
%%            node(),
%%            lists:flatten(io_lib:format(Fmt, Args))])).

%% -------------------------------------------------------------------------

childspecs() ->
    {ok, [{ssl_dist_sup,{ssl_dist_sup, start_link, []},
	   permanent, infinity, supervisor, [ssl_dist_sup]}]}.

select(Node) ->
    gen_select(inet_tcp, Node).

gen_select(Driver, Node) ->
    case split_node(Node) of
        false ->
            false;
        Host ->
	    case Driver:getaddr(Host) of
		{ok, _} -> true;
		_ -> false
	    end
    end.

%% -------------------------------------------------------------------------

is_node_name(Node) ->
    case split_node(Node) of
        false ->
            false;
        _Host ->
            true
    end.

%% -------------------------------------------------------------------------

listen(Name) ->
    gen_listen(inet_tcp, Name).

gen_listen(Driver, Name) ->
    case inet_tcp_dist:gen_listen(Driver, Name) of
        {ok, {Socket, Address, Creation}} ->
            inet:setopts(Socket, [{packet, 4}]),
            {ok, {Socket, Address#net_address{protocol=tls}, Creation}};
        Other ->
            Other
    end.

%% -------------------------------------------------------------------------

accept(Listen) ->
    gen_accept(inet_tcp, Listen).

gen_accept(Driver, Listen) ->
    Kernel = self(),
    spawn_opt(
      fun () ->
              accept_loop(Driver, Listen, Kernel)
      end,
      [link, {priority, max}]).

accept_loop(Driver, Listen, Kernel) ->
    ?trace("~p~n",[{?MODULE, accept_loop, self()}]),
    case Driver:accept(Listen) of
        {ok, Socket} ->
            Opts = get_ssl_options(server),
            wait_for_code_server(),
            case ssl:ssl_accept(
                   Socket, [{active, false}, {packet, 4}] ++ Opts) of
                {ok, SslSocket} ->
                    DistCtrl = ssl_tls_dist_ctrl:start(SslSocket),
                    ?trace("~p~n",
                           [{?MODULE, accept_loop, accepted,
                             SslSocket, DistCtrl, self()}]),
                    ok = ssl:controlling_process(SslSocket, DistCtrl),
                    Kernel !
                        {accept, self(), DistCtrl, Driver:family(), tls},
                    receive
                        {Kernel, controller, Pid} ->
                            ?trace("~p~n",
                                   [{?MODULE, accept_loop,
                                     controller, self()}]),
                            ssl_tls_dist_ctrl:set_supervisor(DistCtrl, Pid),
                            Pid ! {self(), controller};
                        {Kernel, unsupported_protocol} ->
                            ?trace("~p~n",
                                   [{?MODULE, accept_loop,
                                     unsupported_protocol, self()}]),
                            exit(unsupported_protocol)
                    end,
                    accept_loop(Driver, Listen, Kernel);
		{error, {options, _}} = Error ->
		    %% Bad options: that's probably our fault.
                    %% Let's log that.
		    error_logger:error_msg(
                      "Cannot accept TLS distribution connection: ~s~n",
                      [ssl:format_error(Error)]),
		    gen_tcp:close(Socket);
		_ ->
		    gen_tcp:close(Socket)
	    end;
	Error ->
	    exit(Error)
    end,
    accept_loop(Driver, Listen, Kernel).

wait_for_code_server() ->
    %% This is an ugly hack.  Upgrading a socket to TLS requires the
    %% crypto module to be loaded.  Loading the crypto module triggers
    %% its on_load function, which calls code:priv_dir/1 to find the
    %% directory where its NIF library is.  However, distribution is
    %% started earlier than the code server, so the code server is not
    %% necessarily started yet, and code:priv_dir/1 might fail because
    %% of that, if we receive an incoming connection on the
    %% distribution port early enough.
    %%
    %% If the on_load function of a module fails, the module is
    %% unloaded, and the function call that triggered loading it fails
    %% with 'undef', which is rather confusing.
    %%
    %% Thus, the ssl_tls_dist_proxy process will terminate, and be
    %% restarted by ssl_dist_sup.  However, it won't have any memory
    %% of being asked by net_kernel to listen for incoming
    %% connections.  Hence, the node will believe that it's open for
    %% distribution, but it actually isn't.
    %%
    %% So let's avoid that by waiting for the code server to start.
    case whereis(code_server) of
	undefined ->
	    timer:sleep(10),
	    wait_for_code_server();
	Pid when is_pid(Pid) ->
	    ok
    end.

%% -------------------------------------------------------------------------

accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    gen_accept_connection(
      inet_tcp, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime).

gen_accept_connection(
  Driver, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    Kernel = self(),
    spawn_opt(
      fun() ->
              do_accept(
                Driver, Kernel, AcceptPid, DistCtrl,
                MyNode, Allowed, SetupTime)
      end,
      [link, {priority, max}]).

do_accept(Driver, Kernel, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(Driver, DistCtrl) of
		true ->
                    HSData0 = ssl_tls_dist_ctrl:hs_data_common(DistCtrl),
                    HSData =
                        HSData0#hs_data{
                          kernel_pid = Kernel,
                          this_node = MyNode,
                          socket = DistCtrl,
                          timer = Timer,
                          this_flags = 0,
                          allowed = Allowed},
		    dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_logger:error_msg(
                      "** Connection attempt from "
                      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.



setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    gen_setup(inet_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    spawn_opt(
      fun() ->
              do_setup(
                Driver, Kernel, Node, Type,
                MyNode, LongOrShortNames, SetupTime)
      end,
      [link, {priority, max}]).

do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    [Name, Address] = splitnode(Driver, Node, LongOrShortNames),
    case Driver:getaddr(Address) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    ErlEpmd = net_kernel:epmd_module(),
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
                    Opts = connect_options(get_ssl_options(client)),
		    dist_util:reset_timer(Timer),
                    case ssl:connect(
                           Ip, TcpPort,
                           [binary, {active, false}, {packet, 4},
                            Driver:family(), nodelay()] ++ Opts) of
			{ok, SslSocket} ->
                            ?trace("~p~n",
                                   [{?MODULE, do_setup,
                                     ssl_socket, SslSocket}]),
                            DistCtrl = ssl_tls_dist_ctrl:start(SslSocket),
                            ssl_tls_dist_ctrl:set_supervisor(
                              DistCtrl, self()),
                            ok =
                                ssl:controlling_process(
                                  SslSocket, DistCtrl),
                            HSData0 =
                                ssl_tls_dist_ctrl:hs_data_common(DistCtrl),
			    HSData =
                                HSData0#hs_data{
                                  kernel_pid = Kernel,
                                  other_node = Node,
                                  this_node = MyNode,
                                  socket = DistCtrl,
                                  timer = Timer,
                                  this_flags = 0,
                                  other_version = Version,
                                  request_type = Type},
                            ?trace("~p~n",
                                   [{?MODULE, do_setup,
                                     handshake_we_started, HSData}]),
			    dist_util:handshake_we_started(HSData);
			Other ->
			    %% Other Node may have closed since 
			    %% port_please !
			    ?trace("other node (~p) "
				   "closed since port_please.~n", 
				   [Node]),
			    ?shutdown2(Node,
                                       {shutdown, {connect_failed, Other}})
		    end;
		Other ->
		    ?trace("port_please (~p) "
			   "failed.~n", [Node]),
		    ?shutdown2(Node, {shutdown, {port_please_failed, Other}})
	    end;
	Other ->
	    ?trace("~w:getaddr(~p) "
		   "failed (~p).~n", [Driver, Address, Other]),
	    ?shutdown2(Node, {shutdown, {getaddr_failed, Other}})
    end.

close(Socket) ->
    gen_close(inet, Socket).

gen_close(Driver, Socket) ->
    Driver:close(Socket).

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Driver, DistCtrl) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(DistCtrl) of
		{ok, IFs, IP} ->
		    check_ip(Driver, IFs, IP);
		_ ->
		    ?shutdown(no_node)
	    end;
	_ ->
	    true
    end.

check_ip(Driver, [{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {Driver:mask(Netmask, PeerIP), Driver:mask(Netmask, OwnIP)} of
	{M, M} -> true;
	_      -> check_ip(IFs, PeerIP)
    end;
check_ip(_Driver, [], PeerIP) ->
    {false, PeerIP}.

get_ifs(DistCtrl) ->
    Socket = ssl_tls_dist_ctrl:get_socket(DistCtrl),
    case inet:peername(Socket) of
	{ok, {IP, _}} ->
            %% XXX this is seriously broken for IPv6
	    case inet:getif(Socket) of
		{ok, IFs} -> {ok, IFs, IP};
		Error     -> Error
	    end;
	Error ->
	    Error
    end.


%% If Node is illegal terminate the connection setup!!
splitnode(Driver, Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    check_node(Driver, Name, Node, Host, LongOrShortNames);
	[_] ->
	    error_logger:error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_logger:error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

check_node(Driver, Name, Node, Host, LongOrShortNames) ->
    case split_node(Host, $., []) of
	[_] when LongOrShortNames == longnames ->
	    case Driver:parse_address(Host) of
		{ok, _} ->
		    [Name, Host];
		_ ->
		    error_logger:error_msg(
                      "** System running to use "
                      "fully qualified hostnames **~n"
                      "** Hostname ~s is illegal **~n",
                      [Host]),
		    ?shutdown(Node)
	    end;
	[_, _ | _] when LongOrShortNames == shortnames ->
	    error_logger:error_msg(
              "** System NOT running to use "
              "fully qualified hostnames **~n"
              "** Hostname ~s is illegal **~n",
              [Host]),
	    ?shutdown(Node);
	_ ->
	    [Name, Host]
    end.

split_node(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
        [_, Host] ->
            Host;
        _ ->
            false
    end;
split_node(_) ->
    false.
%%
split_node([Chr|T], Chr, Ack) -> 
    [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack) -> 
    split_node(T, Chr, [H|Ack]);
split_node([], _, Ack) -> 
    [lists:reverse(Ack)].

%% -------------------------------------------------------------------------

connect_options(Opts) ->
    case application:get_env(kernel, inet_dist_connect_options) of
	{ok,ConnectOpts} ->
	    lists:ukeysort(1, ConnectOpts ++ Opts);
	_ ->
	    Opts
    end.

%% we may not always want the nodelay behaviour
%% for performance reasons
nodelay() ->
    case application:get_env(kernel, dist_nodelay) of
	undefined ->
	    {nodelay, true};
	{ok, true} ->
	    {nodelay, true};
	{ok, false} ->
	    {nodelay, false};
	_ ->
	    {nodelay, true}
    end.


get_ssl_options(Type) ->
    case init:get_argument(ssl_dist_opt) of
	{ok, Args} ->
	    [{erl_dist, true} | ssl_options(Type, lists:append(Args))];
	_ ->
	    [{erl_dist, true}]
    end.

ssl_options(_,[]) ->
    [];
ssl_options(server, ["client_" ++ _, _Value |T]) ->
    ssl_options(server,T);
ssl_options(client, ["server_" ++ _, _Value|T]) ->
    ssl_options(client,T);
ssl_options(server, ["server_certfile", Value|T]) ->
    [{certfile, Value} | ssl_options(server,T)];
ssl_options(client, ["client_certfile", Value | T]) ->
    [{certfile, Value} | ssl_options(client,T)];
ssl_options(server, ["server_cacertfile", Value|T]) ->
    [{cacertfile, Value} | ssl_options(server,T)];
ssl_options(client, ["client_cacertfile", Value|T]) ->
    [{cacertfile, Value} | ssl_options(client,T)];
ssl_options(server, ["server_keyfile", Value|T]) ->
    [{keyfile, Value} | ssl_options(server,T)];
ssl_options(client, ["client_keyfile", Value|T]) ->
    [{keyfile, Value} | ssl_options(client,T)];
ssl_options(server, ["server_password", Value|T]) ->
    [{password, Value} | ssl_options(server,T)];
ssl_options(client, ["client_password", Value|T]) ->
    [{password, Value} | ssl_options(client,T)];
ssl_options(server, ["server_verify", Value|T]) ->
    [{verify, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_verify", Value|T]) ->
    [{verify, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_verify_fun", Value|T]) ->
    [{verify_fun, verify_fun(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_verify_fun", Value|T]) ->
    [{verify_fun, verify_fun(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_crl_check", Value|T]) ->
    [{crl_check, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_crl_check", Value|T]) ->
    [{crl_check, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_crl_cache", Value|T]) ->
    [{crl_cache, termify(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_crl_cache", Value|T]) ->
    [{crl_cache, termify(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_reuse_sessions", Value|T]) ->
    [{reuse_sessions, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_reuse_sessions", Value|T]) ->
    [{reuse_sessions, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_secure_renegotiate", Value|T]) ->
    [{secure_renegotiate, atomize(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_secure_renegotiate", Value|T]) ->
    [{secure_renegotiate, atomize(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_depth", Value|T]) ->
    [{depth, list_to_integer(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_depth", Value|T]) ->
    [{depth, list_to_integer(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_hibernate_after", Value|T]) ->
    [{hibernate_after, list_to_integer(Value)} | ssl_options(server,T)];
ssl_options(client, ["client_hibernate_after", Value|T]) ->
    [{hibernate_after, list_to_integer(Value)} | ssl_options(client,T)];
ssl_options(server, ["server_ciphers", Value|T]) ->
    [{ciphers, Value} | ssl_options(server,T)];
ssl_options(client, ["client_ciphers", Value|T]) ->
    [{ciphers, Value} | ssl_options(client,T)];
ssl_options(server, ["server_dhfile", Value|T]) ->
    [{dhfile, Value} | ssl_options(server,T)];
ssl_options(server, ["server_fail_if_no_peer_cert", Value|T]) ->
    [{fail_if_no_peer_cert, atomize(Value)} | ssl_options(server,T)];
ssl_options(Type, Opts) ->
    error(malformed_ssl_dist_opt, [Type, Opts]).

atomize(List) when is_list(List) ->
    list_to_atom(List);
atomize(Atom) when is_atom(Atom) ->
    Atom.

termify(String) when is_list(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

verify_fun(Value) ->
    case termify(Value) of
	{Mod, Func, State} when is_atom(Mod), is_atom(Func) ->
	    Fun = fun Mod:Func/3,
	    {Fun, State};
	_ ->
	    error(malformed_ssl_dist_opt, [Value])
    end.
