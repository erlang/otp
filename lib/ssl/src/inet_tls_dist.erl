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

-export([dbg/0]). % Debug

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-include("ssl_api.hrl").

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

hs_data_common(#sslsocket{pid = DistCtrl} = SslSocket) ->
    #hs_data{
       f_send =
           fun (Ctrl, Packet) when Ctrl == DistCtrl ->
                   f_send(SslSocket, Packet)
           end,
       f_recv =
           fun (Ctrl, Length, Timeout) when Ctrl == DistCtrl ->
                   f_recv(SslSocket, Length, Timeout)
           end,
       f_setopts_pre_nodeup =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   f_setopts_pre_nodeup(SslSocket)
           end,
       f_setopts_post_nodeup =
           fun (Ctrl) when Ctrl == DistCtrl ->
%%%                   sys:trace(Ctrl, true),
                   f_setopts_post_nodeup(SslSocket)
           end,
       f_getll =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   f_getll(DistCtrl)
           end,
       f_address =
           fun (Ctrl, Node) when Ctrl == DistCtrl ->
                   f_address(SslSocket, Node)
           end,
       mf_tick =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   mf_tick(DistCtrl)
           end,
       mf_getstat =
           fun (Ctrl) when Ctrl == DistCtrl ->
                   mf_getstat(SslSocket)
           end,
       mf_setopts =
           fun (Ctrl, Opts) when Ctrl == DistCtrl ->
                   mf_setopts(SslSocket, Opts)
           end,
       mf_getopts =
           fun (Ctrl, Opts) when Ctrl == DistCtrl ->
                   mf_getopts(SslSocket, Opts)
           end,
       f_handshake_complete =
           fun (Ctrl, Node, DHandle) when Ctrl == DistCtrl ->
                   f_handshake_complete(DistCtrl, Node, DHandle)
           end}.

f_send(SslSocket, Packet) ->
    ssl:send(SslSocket, Packet).

f_recv(SslSocket, Length, Timeout) ->
    case ssl:recv(SslSocket, Length, Timeout) of
        {ok, Bin} when is_binary(Bin) ->
            {ok, binary_to_list(Bin)};
        Other ->
            Other
    end.

f_setopts_pre_nodeup(_SslSocket) ->
    ok.

f_setopts_post_nodeup(_SslSocket) ->
    ok.

f_getll(DistCtrl) ->
    {ok, DistCtrl}.

f_address(SslSocket, Node) ->
    case ssl:peername(SslSocket) of
        {ok, Address} ->
            case split_node(Node) of
                false ->
                    {error, no_node};
                Host ->
                    #net_address{
                       address=Address, host=Host,
                       protocol=tls, family=inet}
            end
    end.

mf_tick(DistCtrl) ->
    DistCtrl ! tick,
    ok.

mf_getstat(SslSocket) ->
    case ssl:getstat(
           SslSocket, [recv_cnt, send_cnt, send_pend]) of
        {ok, Stat} ->
            split_stat(Stat,0,0,0);
        Error ->
            Error
    end.

mf_setopts(SslSocket, Opts) ->
    case setopts_filter(Opts) of
        [] ->
            ssl:setopts(SslSocket, Opts);
        Opts1 ->
            {error, {badopts,Opts1}}
    end.

mf_getopts(SslSocket, Opts) ->
    ssl:getopts(SslSocket, Opts).

f_handshake_complete(DistCtrl, Node, DHandle) ->
    ssl_connection:handshake_complete(DistCtrl, Node, DHandle).


setopts_filter(Opts) ->
    [Opt || {K,_} = Opt <- Opts,
            K =:= active orelse K =:= deliver orelse K =:= packet].

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

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
    monitor_pid(
      spawn_opt(
        fun () ->
                accept_loop(Driver, Listen, Kernel)
        end,
        [link, {priority, max}])).

accept_loop(Driver, Listen, Kernel) ->
    case Driver:accept(Listen) of
        {ok, Socket} ->
            Opts = get_ssl_options(server),
            wait_for_code_server(),
            case ssl:ssl_accept(
                   Socket, [{active, false}, {packet, 4}] ++ Opts,
                   net_kernel:connecttime()) of
                {ok, #sslsocket{pid = DistCtrl} = SslSocket} ->
                    monitor_pid(DistCtrl),
                    trace(
                      Kernel !
                          {accept, self(), DistCtrl,
                           Driver:family(), tls}),
                    receive
                        {Kernel, controller, Pid} ->
                            ok = ssl:controlling_process(SslSocket, Pid),
                            trace(
                              Pid ! {self(), controller});
                        {Kernel, unsupported_protocol} ->
                            exit(trace(unsupported_protocol))
                    end,
                    accept_loop(Driver, Listen, Kernel);
		{error, {options, _}} = Error ->
		    %% Bad options: that's probably our fault.
                    %% Let's log that.
		    error_logger:error_msg(
                      "Cannot accept TLS distribution connection: ~s~n",
                      [ssl:format_error(Error)]),
                    _ = trace(Error),
                    gen_tcp:close(Socket);
		Other ->
                    _ = trace(Other),
                    gen_tcp:close(Socket)
	    end;
	Error ->
	    exit(trace(Error))
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
    %% Thus, the accept process will terminate, and be
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
    monitor_pid(
      spawn_opt(
        fun() ->
                do_accept(
                  Driver, Kernel, AcceptPid, DistCtrl,
                  MyNode, Allowed, SetupTime)
        end,
        [link, {priority, max}])).

do_accept(Driver, Kernel, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    SslSocket = ssl_connection:get_sslsocket(DistCtrl),
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(Driver, SslSocket) of
		true ->
                    HSData0 = hs_data_common(SslSocket),
                    HSData =
                        HSData0#hs_data{
                          kernel_pid = Kernel,
                          this_node = MyNode,
                          socket = DistCtrl,
                          timer = Timer,
                          this_flags = 0,
                          allowed = Allowed},
                    link(DistCtrl),
		    dist_util:handshake_other_started(trace(HSData));
		{false,IP} ->
		    error_logger:error_msg(
                      "** Connection attempt from "
                      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown2(no_node, trace({disallowed, IP}))
	    end
    end.



setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    gen_setup(inet_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    monitor_pid(
      spawn_opt(
        fun() ->
                do_setup(
                  Driver, Kernel, Node, Type,
                  MyNode, LongOrShortNames, SetupTime)
        end,
        [link, {priority, max}])).

do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    [Name, Address] = splitnode(Driver, Node, LongOrShortNames),
    case Driver:getaddr(Address) of
	{ok, Ip} ->
            Timer = trace(dist_util:start_timer(SetupTime)),
	    ErlEpmd = net_kernel:epmd_module(),
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
                    Opts =
                        trace(
                          connect_options(
                            [{server_name_indication, atom_to_list(Node)}
                             |get_ssl_options(client)])),
		    dist_util:reset_timer(Timer),
                    case ssl:connect(
                           Address, TcpPort,
                           [binary, {active, false}, {packet, 4},
                            Driver:family(), nodelay()] ++ Opts,
                           net_kernel:connecttime()) of
			{ok, #sslsocket{pid = DistCtrl} = SslSocket} ->
                            monitor_pid(DistCtrl),
                            ok = ssl:controlling_process(SslSocket, self()),
                            HSData0 = hs_data_common(SslSocket),
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
                            link(DistCtrl),
			    dist_util:handshake_we_started(trace(HSData));
			Other ->
			    %% Other Node may have closed since 
			    %% port_please !
			    ?shutdown2(
                               Node,
                               trace(
                                 {ssl_connect_failed, Ip, TcpPort, Other}))
		    end;
		Other ->
		    ?shutdown2(
                       Node,
                       trace(
                         {port_please_failed, ErlEpmd, Name, Ip, Other}))
	    end;
	Other ->
	    ?shutdown2(
               Node,
               trace({getaddr_failed, Driver, Address, Other}))
    end.

close(Socket) ->
    gen_close(inet, Socket).

gen_close(Driver, Socket) ->
    trace(Driver:close(Socket)).

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Driver, SslSocket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(SslSocket) of
		{ok, IFs, IP} ->
		    check_ip(Driver, IFs, IP);
		Other ->
		    ?shutdown2(
                       no_node, trace({check_ip_failed, SslSocket, Other}))
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

get_ifs(#sslsocket{fd = {gen_tcp, Socket, _}}) ->
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
    case string:split(atom_to_list(Node), "@") of
	[Name, Host] when Host =/= [] ->
	    check_node(Driver, Name, Node, Host, LongOrShortNames);
	[_] ->
	    error_logger:error_msg(
              "** Nodename ~p illegal, no '@' character **~n",
              [Node]),
	    ?shutdown2(Node, trace({illegal_node_n@me, Node}));
	_ ->
	    error_logger:error_msg(
              "** Nodename ~p illegal **~n", [Node]),
	    ?shutdown2(Node, trace({illegal_node_name, Node}))
    end.

check_node(Driver, Name, Node, Host, LongOrShortNames) ->
    case string:split(Host, ".") of
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
		    ?shutdown2(Node, trace({not_longnames, Host}))
	    end;
	[_, _] when LongOrShortNames == shortnames ->
	    error_logger:error_msg(
              "** System NOT running to use "
              "fully qualified hostnames **~n"
              "** Hostname ~s is illegal **~n",
              [Host]),
	    ?shutdown2(Node, trace({not_shortnames, Host}));
	_ ->
	    [Name, Host]
    end.

split_node(Node) when is_atom(Node) ->
    case string:split(atom_to_list(Node), "@") of
        [Name, Host] when Name =/= [], Host =/= [] ->
            Host;
        _ ->
            false
    end;
split_node(_) ->
    false.

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
    try ets:lookup(ssl_dist_opts, Type) of
        [{Type, Opts}] ->
            [{erl_dist, true} | Opts];
        _ ->
            get_ssl_dist_arguments(Type)
    catch
        error:badarg ->
            get_ssl_dist_arguments(Type)
    end.

get_ssl_dist_arguments(Type) ->
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

%% -------------------------------------------------------------------------

%% Trace point
trace(Term) -> Term.

%% Keep an eye on distribution Pid:s we know of
monitor_pid(Pid) ->
    %%spawn(
    %%  fun () ->
    %%          MRef = erlang:monitor(process, Pid),
    %%          receive
    %%              {'DOWN', MRef, _, _, normal} ->
    %%                  error_logger:error_report(
    %%                    [dist_proc_died,
    %%                     {reason, normal},
    %%                     {pid, Pid}]);
    %%              {'DOWN', MRef, _, _, Reason} ->
    %%                  error_logger:info_report(
    %%                    [dist_proc_died,
    %%                     {reason, Reason},
    %%                     {pid, Pid}])
    %%          end
    %%  end),
    Pid.

dbg() ->
    dbg:stop(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(?MODULE, cx),
    dbg:tpl(erlang, dist_ctrl_get_data_notification, cx),
    dbg:tpl(erlang, dist_ctrl_get_data, cx),
    dbg:tpl(erlang, dist_ctrl_put_data, cx),
    ok.
