%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2019. All Rights Reserved.
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

-export([nodelay/0]).

-export([verify_client/3, cert_nodes/1]).

-export([dbg/0]). % Debug

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").
-include_lib("public_key/include/public_key.hrl").

-include("ssl_api.hrl").
-include_lib("kernel/include/logger.hrl").

%% -------------------------------------------------------------------------

childspecs() ->
    {ok, [{ssl_dist_sup,{ssl_dist_sup, start_link, []},
	   permanent, infinity, supervisor, [ssl_dist_sup]}]}.

select(Node) ->
    gen_select(inet_tcp, Node).

gen_select(Driver, Node) ->
    case dist_util:split_node(Node) of
        {node,_,Host} ->
	    case Driver:getaddr(Host) of
		{ok, _} -> true;
		_ -> false
	    end;
        _ ->
            false
    end.

%% -------------------------------------------------------------------------

is_node_name(Node) ->
    dist_util:is_node_name(Node).

%% -------------------------------------------------------------------------

hs_data_common(#sslsocket{pid = [_, DistCtrl|_]} = SslSocket) ->
    #hs_data{
       f_send =
           fun (_Ctrl, Packet) ->
                   f_send(SslSocket, Packet)
           end,
       f_recv =
           fun (_, Length, Timeout) ->
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

f_setopts_post_nodeup(SslSocket) ->
    ssl:setopts(SslSocket, [nodelay()]).

f_getll(DistCtrl) ->
    {ok, DistCtrl}.

f_address(SslSocket, Node) ->
    case ssl:peername(SslSocket) of
        {ok, Address} ->
            case dist_util:split_node(Node) of
                {node,_,Host} ->
                    #net_address{
                       address=Address, host=Host,
                       protocol=tls, family=inet};
                _ ->
                    {error, no_node}
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
    tls_sender:dist_handshake_complete(DistCtrl, Node, DHandle).

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
            inet:setopts(Socket, [{packet, 4}, {nodelay, true}]),
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
	    case check_ip(Driver, Socket) of
                true ->
                    accept_loop(Driver, Listen, Kernel, Socket);
                {false,IP} ->
		    ?LOG_ERROR(
                      "** Connection attempt from "
                      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown2(no_node, trace({disallowed, IP}))
	    end;
	Error ->
	    exit(trace(Error))
    end.

accept_loop(Driver, Listen, Kernel, Socket) ->
    Opts = setup_verify_client(Socket, get_ssl_options(server)),
    wait_for_code_server(),
    case
        ssl:handshake(
          Socket,
          trace([{active, false},{packet, 4}|Opts]),
          net_kernel:connecttime())
    of
        {ok, #sslsocket{pid = [_, DistCtrl| _]} = SslSocket} ->
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
            ?LOG_ERROR(
              "Cannot accept TLS distribution connection: ~s~n",
              [ssl:format_error(Error)]),
            gen_tcp:close(Socket),
            exit(trace(Error));
        Other ->
            gen_tcp:close(Socket),
            exit(trace(Other))
    end.


%% {verify_fun,{fun ?MODULE:verify_client/3,_}} is used
%% as a configuration marker that verify_client/3 shall be used.
%%
%% Replace the State in the first occurence of
%% {verify_fun,{fun ?MODULE:verify_client/3,State}}
%% and remove the rest.
%% The inserted state is not accesible from a configuration file
%% since it is dynamic and connection dependent.
%%
setup_verify_client(Socket, Opts) ->
    setup_verify_client(Socket, Opts, true, []).
%%
setup_verify_client(_Socket, [], _, OptsR) ->
    lists:reverse(OptsR);
setup_verify_client(Socket, [Opt|Opts], First, OptsR) ->
    case Opt of
        {verify_fun,{Fun,_}} ->
            case Fun =:= fun ?MODULE:verify_client/3 of
                true ->
                    if
                        First ->
                            case inet:peername(Socket) of
                                {ok,{PeerIP,_Port}} ->
                                    {ok,Allowed} = net_kernel:allowed(),
                                    AllowedHosts = allowed_hosts(Allowed),
                                    setup_verify_client(
                                      Socket, Opts, false,
                                      [{verify_fun,
                                        {Fun, {AllowedHosts,PeerIP}}}
                                       |OptsR]);
                                {error,Reason} ->
                                    exit(trace({no_peername,Reason}))
                            end;
                        true ->
                            setup_verify_client(
                              Socket, Opts, First, OptsR)
                    end;
                false ->
                    setup_verify_client(
                      Socket, Opts, First, [Opt|OptsR])
            end;
        _ ->
            setup_verify_client(Socket, Opts, First, [Opt|OptsR])
    end.

allowed_hosts(Allowed) ->
    lists:usort(allowed_node_hosts(Allowed)).
%%
allowed_node_hosts([]) -> [];
allowed_node_hosts([Node|Allowed]) ->
    case dist_util:split_node(Node) of
        {node,_,Host} ->
            [Host|allowed_node_hosts(Allowed)];
        {host,Host} ->
            [Host|allowed_node_hosts(Allowed)];
        _ ->
            allowed_node_hosts(Allowed)
    end.

%% Same as verify_peer but check cert host names for
%% peer IP address
verify_client(_, {bad_cert,_} = Reason, _) ->
    {fail,Reason};
verify_client(_, {extension,_}, S) ->
    {unknown,S};
verify_client(_, valid, S) ->
    {valid,S};
verify_client(_, valid_peer, {[],_} = S) ->
    %% Allow all hosts
    {valid,S};
verify_client(PeerCert, valid_peer, {AllowedHosts,PeerIP} = S) ->
    case
        public_key:pkix_verify_hostname(
          PeerCert,
          [{ip,PeerIP}|[{dns_id,Host} || Host <- AllowedHosts]])
    of
        true ->
            {valid,S};
        false ->
            {fail,cert_no_hostname_nor_ip_match}
    end.


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
                  Driver, AcceptPid, DistCtrl,
                  MyNode, Allowed, SetupTime, Kernel)
        end,
        [link, {priority, max}])).

do_accept(
  _Driver, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime, Kernel) ->
    {ok, SslSocket} = tls_sender:dist_tls_socket(DistCtrl),
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
            NewAllowed = allowed_nodes(SslSocket, Allowed),
            HSData0 = hs_data_common(SslSocket),
            HSData =
                HSData0#hs_data{
                  kernel_pid = Kernel,
                  this_node = MyNode,
                  socket = DistCtrl,
                  timer = Timer,
                  this_flags = 0,
                  allowed = NewAllowed},
            link(DistCtrl),
            dist_util:handshake_other_started(trace(HSData))
    end.

allowed_nodes(_SslSocket, []) ->
    %% Allow all
    [];
allowed_nodes(SslSocket, Allowed) ->
    case ssl:peercert(SslSocket) of
        {ok,PeerCertDER} ->
            case ssl:peername(SslSocket) of
                {ok,{PeerIP,_Port}} ->
                    PeerCert =
                        public_key:pkix_decode_cert(PeerCertDER, otp),
                    case
                        allowed_nodes(
                          PeerCert, allowed_hosts(Allowed), PeerIP)
                    of
                        [] ->
                            ?LOG_ERROR(
                              "** Connection attempt from "
                              "disallowed node(s) ~p ** ~n", [PeerIP]),
                            ?shutdown2(
                               PeerIP, trace({is_allowed, not_allowed}));
                        AllowedNodes ->
                            AllowedNodes
                    end;
                Error1 ->
                    ?shutdown2(no_peer_ip, trace(Error1))
            end;
        {error,no_peercert} ->
            Allowed;
        Error2 ->
            ?shutdown2(no_peer_cert, trace(Error2))
    end.

allowed_nodes(PeerCert, [], PeerIP) ->
    case public_key:pkix_verify_hostname(PeerCert, [{ip,PeerIP}]) of
        true ->
            Host = inet:ntoa(PeerIP),
            true = is_list(Host),
            [Host];
        false ->
            []
    end;
allowed_nodes(PeerCert, [Node|Allowed], PeerIP) ->
    case dist_util:split_node(Node) of
        {node,_,Host} ->
            allowed_nodes(PeerCert, Allowed, PeerIP, Node, Host);
        {host,Host} ->
            allowed_nodes(PeerCert, Allowed, PeerIP, Node, Host);
        _ ->
            allowed_nodes(PeerCert, Allowed, PeerIP)
    end.

allowed_nodes(PeerCert, Allowed, PeerIP, Node, Host) ->
    case public_key:pkix_verify_hostname(PeerCert, [{dns_id,Host}]) of
        true ->
            [Node|allowed_nodes(PeerCert, Allowed, PeerIP)];
        false ->
            allowed_nodes(PeerCert, Allowed, PeerIP)
    end.

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    gen_setup(inet_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    monitor_pid(
      spawn_opt(setup_fun(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime),
                [link, {priority, max}])).

-spec setup_fun(_,_,_,_,_,_,_) -> fun(() -> no_return()).
setup_fun(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    fun() ->
            do_setup(
              Driver, Kernel, Node, Type,
              MyNode, LongOrShortNames, SetupTime)
    end.


-spec do_setup(_,_,_,_,_,_,_) -> no_return().
do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    {Name, Address} = split_node(Driver, Node, LongOrShortNames),
    ErlEpmd = net_kernel:epmd_module(),
    {ARMod, ARFun} = get_address_resolver(ErlEpmd, Driver),
    Timer = trace(dist_util:start_timer(SetupTime)),
    case ARMod:ARFun(Name,Address,Driver:family()) of
    {ok, Ip, TcpPort, Version} ->
        do_setup_connect(Driver, Kernel, Node, Address, Ip, TcpPort, Version, Type, MyNode, Timer);
	{ok, Ip} ->
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
                do_setup_connect(Driver, Kernel, Node, Address, Ip, TcpPort, Version, Type, MyNode, Timer);
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

-spec do_setup_connect(_,_,_,_,_,_,_,_,_,_) -> no_return().

do_setup_connect(Driver, Kernel, Node, Address, Ip, TcpPort, Version, Type, MyNode, Timer) ->
    Opts =  trace(connect_options(get_ssl_options(client))),
    dist_util:reset_timer(Timer),
    case ssl:connect(
        Address, TcpPort,
        [binary, {active, false}, {packet, 4},
            Driver:family(), {nodelay, true}] ++ Opts,
        net_kernel:connecttime()) of
    {ok, #sslsocket{pid = [_, DistCtrl| _]} = SslSocket} ->
            _ = monitor_pid(DistCtrl),
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
    end.

close(Socket) ->
    gen_close(inet, Socket).

gen_close(Driver, Socket) ->
    trace(Driver:close(Socket)).


%% ------------------------------------------------------------
%% Determine if EPMD module supports address resolving. Default
%% is to use inet_tcp:getaddr/2.
%% ------------------------------------------------------------
get_address_resolver(EpmdModule, _Driver) ->
    case erlang:function_exported(EpmdModule, address_please, 3) of
        true -> {EpmdModule, address_please};
        _    -> {erl_epmd, address_please}
    end.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Driver, Socket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(Socket) of
		{ok, IFs, IP} ->
		    check_ip(Driver, IFs, IP);
		Other ->
		    ?shutdown2(
                       no_node, trace({check_ip_failed, Socket, Other}))
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

get_ifs(Socket) ->
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


%% Look in Extensions, in all subjectAltName:s
%% to find node names in this certificate.
%% Host names are picked up as a subjectAltName containing
%% a dNSName, and the first subjectAltName containing
%% a commonName is the node name.
%%
cert_nodes(
  #'OTPCertificate'{
     tbsCertificate = #'OTPTBSCertificate'{extensions = Extensions}}) ->
    parse_extensions(Extensions).


parse_extensions(Extensions) when is_list(Extensions) ->
    parse_extensions(Extensions, [], []);
parse_extensions(asn1_NOVALUE) ->
    undefined. % Allow all nodes
%%
parse_extensions([], [], []) ->
    undefined; % Allow all nodes
parse_extensions([], Hosts, []) ->
    lists:reverse(Hosts);
parse_extensions([], [], Names) ->
    [Name ++ "@" || Name <- lists:reverse(Names)];
parse_extensions([], Hosts, Names) ->
    [Name ++ "@" ++ Host ||
        Host <- lists:reverse(Hosts),
        Name <- lists:reverse(Names)];
parse_extensions(
  [#'Extension'{
      extnID = ?'id-ce-subjectAltName',
      extnValue = AltNames}
   |Extensions],
  Hosts, Names) ->
    case parse_subject_altname(AltNames) of
        none ->
            parse_extensions(Extensions, Hosts, Names);
        {host,Host} ->
            parse_extensions(Extensions, [Host|Hosts], Names);
        {name,Name} ->
            parse_extensions(Extensions, Hosts, [Name|Names])
    end;
parse_extensions([_|Extensions], Hosts, Names) ->
    parse_extensions(Extensions, Hosts, Names).

parse_subject_altname([]) ->
    none;
parse_subject_altname([{dNSName,Host}|_AltNames]) ->
    {host,Host};
parse_subject_altname(
  [{directoryName,{rdnSequence,[Rdn|_]}}|AltNames]) ->
    %%
    %% XXX Why is rdnSequence a sequence?
    %% Should we parse all members?
    %%
    case parse_rdn(Rdn) of
        none ->
            parse_subject_altname(AltNames);
        Name ->
            {name,Name}
    end;
parse_subject_altname([_|AltNames]) ->
    parse_subject_altname(AltNames).


parse_rdn([]) ->
    none;
parse_rdn(
  [#'AttributeTypeAndValue'{
     type = ?'id-at-commonName',
     value = {utf8String,CommonName}}|_]) ->
    unicode:characters_to_list(CommonName);
parse_rdn([_|Rdn]) ->
    parse_rdn(Rdn).


%% If Node is illegal terminate the connection setup!!
split_node(Driver, Node, LongOrShortNames) ->
    case dist_util:split_node(Node) of
        {node, Name, Host} ->
	    check_node(Driver, Node, Name, Host, LongOrShortNames);
	{host, _} ->
	    ?LOG_ERROR(
              "** Nodename ~p illegal, no '@' character **~n",
              [Node]),
	    ?shutdown2(Node, trace({illegal_node_n@me, Node}));
	_ ->
	    ?LOG_ERROR(
              "** Nodename ~p illegal **~n", [Node]),
	    ?shutdown2(Node, trace({illegal_node_name, Node}))
    end.

check_node(Driver, Node, Name, Host, LongOrShortNames) ->
    case string:split(Host, ".", all) of
	[_] when LongOrShortNames =:= longnames ->
	    case Driver:parse_address(Host) of
		{ok, _} ->
		    {Name, Host};
		_ ->
		    ?LOG_ERROR(
                      "** System running to use "
                      "fully qualified hostnames **~n"
                      "** Hostname ~s is illegal **~n",
                      [Host]),
		    ?shutdown2(Node, trace({not_longnames, Host}))
	    end;
	[_,_|_] when LongOrShortNames =:= shortnames ->
	    ?LOG_ERROR(
              "** System NOT running to use "
              "fully qualified hostnames **~n"
              "** Hostname ~s is illegal **~n",
              [Host]),
	    ?shutdown2(Node, trace({not_shortnames, Host}));
	_ ->
	    {Name, Host}
    end.

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


ssl_options(_Type, []) ->
    [];
ssl_options(client, ["client_" ++ Opt, Value | T] = Opts) ->
    ssl_options(client, T, Opts, Opt, Value);
ssl_options(server, ["server_" ++ Opt, Value | T] = Opts) ->
    ssl_options(server, T, Opts, Opt, Value);
ssl_options(Type, [_Opt, _Value | T]) ->
    ssl_options(Type, T).
%%
ssl_options(Type, T, Opts, Opt, Value) ->
    case ssl_option(Type, Opt) of
        error ->
            error(malformed_ssl_dist_opt, [Type, Opts]);
        Fun ->
            [{list_to_atom(Opt), Fun(Value)}|ssl_options(Type, T)]
    end.

ssl_option(server, Opt) ->
    case Opt of
        "dhfile" -> fun listify/1;
        "fail_if_no_peer_cert" -> fun atomize/1;
        _ -> ssl_option(client, Opt)
    end;
ssl_option(client, Opt) ->
    case Opt of
        "certfile" -> fun listify/1;
        "cacertfile" -> fun listify/1;
        "keyfile" -> fun listify/1;
        "password" -> fun listify/1;
        "verify" -> fun atomize/1;
        "verify_fun" -> fun verify_fun/1;
        "crl_check" -> fun atomize/1;
        "crl_cache" -> fun termify/1;
        "reuse_sessions" -> fun atomize/1;
        "secure_renegotiate" -> fun atomize/1;
        "depth" -> fun erlang:list_to_integer/1;
        "hibernate_after" -> fun erlang:list_to_integer/1;
        "ciphers" -> fun listify/1;
        _ -> error
    end.

listify(List) when is_list(List) ->
    List.

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
    %%                  ?LOG_ERROR(
    %%                    [{slogan, dist_proc_died},
    %%                     {reason, normal},
    %%                     {pid, Pid}]);
    %%              {'DOWN', MRef, _, _, Reason} ->
    %%                  ?LOG_NOTICE(
    %%                    [{slogan, dist_proc_died},
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
