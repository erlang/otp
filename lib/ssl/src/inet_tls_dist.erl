%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2024. All Rights Reserved.
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
-moduledoc false.

-export([childspecs/0]).
-export([select/1, address/0, is_node_name/1,
         listen/2, accept/1, accept_connection/5,
	 setup/5, close/1]).

%% Generalized dist API
-export([fam_select/2, fam_address/1, fam_listen/3, fam_accept/2,
         fam_accept_connection/6, fam_setup/6]).

-export([verify_client/3, cert_nodes/1]).

%% kTLS helpers
-export([inet_ktls_setopt/3, inet_ktls_getopt/3,
         set_ktls/1, set_ktls_ulp/2, set_ktls_cipher/5,
         ktls_os/0, ktls_opt_ulp/1, ktls_opt_cipher/6]).

-export([dbg/0]). % Debug

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").
-include_lib("public_key/include/public_key.hrl").

-include("ssl_api.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include_lib("kernel/include/logger.hrl").

-define(FAMILY, inet).
-define(DRIVER, inet_tcp). % Implies ?FAMILY = inet through inet_drv.c
-define(PROTOCOL, tls).

%% -------------------------------------------------------------------------

childspecs() ->
    {ok, [{ssl_dist_sup,{ssl_dist_sup, start_link, []},
	   permanent, infinity, supervisor, [ssl_dist_sup]}]}.

%% -------------------------------------------------------------------------
%% Select this protocol based on node name
select(Node) ->
    fam_select(?FAMILY, Node).
fam_select(Family, Node) ->
    inet_tcp_dist:fam_select(Family, Node).
%% -------------------------------------------------------------------------
%% Get the #net_address this distribution uses
address() ->
    fam_address(?FAMILY).
fam_address(Family) ->
    NetAddress = inet_tcp_dist:fam_address(Family),
    NetAddress#net_address{ protocol = ?PROTOCOL }.
%% -------------------------------------------------------------------------
%% Is this one really needed??
is_node_name(Node) ->
    dist_util:is_node_name(Node).
%% -------------------------------------------------------------------------

hs_data_inet_tcp(Driver, Socket) ->
    Family = Driver:family(),
    {ok, Peername} =
        maybe
            {error, einval} ?= inet:peername(Socket),
            ?shutdown({Driver, closed})
        end,
    (inet_tcp_dist:gen_hs_data(Driver, Socket))
    #hs_data{
      f_address =
          fun(_, Node) ->
                  {node, _, Host} = dist_util:split_node(Node),
                  #net_address{
                     address  = Peername,
                     host     = Host,
                     protocol = ?PROTOCOL,
                     family   = Family
                    }
          end}.

hs_data_ssl(Family, #sslsocket{pid = [_, DistCtrl|_]} = SslSocket) ->
    {ok, Address} =
        maybe
            {error, einval} ?= ssl:peername(SslSocket),
            ?shutdown({sslsocket, closed})
        end,
    #hs_data{
       socket = DistCtrl,
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
                   f_address(Family, Address, Node)
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
    ssl:setopts(SslSocket, [inet_tcp_dist:nodelay()]).

f_getll(DistCtrl) ->
    {ok, DistCtrl}.

f_address(Family, Address, Node) ->
    case dist_util:split_node(Node) of
        {node,_,Host} ->
            #net_address{
               address = Address,
               host = Host,
               protocol = ?PROTOCOL,
               family = Family};
        _ ->
            {error, no_node}
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

listen(Name, Host) ->
    fam_listen(?FAMILY, Name, Host).

fam_listen(Family, Name, Host) ->
    ForcedOptions =
        [Family, {active, false}, {packet, 4},
         {read_ahead, false}, {nodelay, true}],
    ListenFun =
        fun (First, Last, ListenOptions) ->
                listen_loop(
                  First, Last,
                  inet_tcp_dist:merge_options(ListenOptions, ForcedOptions))
        end,
    maybe
        %%
        {ok, {ListenSocket, Address, Creation}} ?=
            inet_tcp_dist:fam_listen(Family, Name, Host, ListenFun),
        NetAddress =
            #net_address{
               host = Host,
               protocol = ?PROTOCOL,
               family = Family,
               address = Address},
        {ok, {ListenSocket, NetAddress, Creation}}
    end.

listen_loop(First, Last, ListenOptions) when First =< Last ->
    case gen_tcp:listen(First, ListenOptions) of
        {error, eaddrinuse} ->
            listen_loop(First + 1, Last, ListenOptions);
        Result ->
            Result
    end;
listen_loop(_, _, _) ->
    {error, eaddrinuse}.

%% -------------------------------------------------------------------------

accept(ListenSocket) ->
    fam_accept(?FAMILY, ListenSocket).

fam_accept(Family, ListenSocket) ->
    NetKernel = self(),
    monitor_pid(
      spawn_opt(
        fun () ->
                process_flag(trap_exit, true),
                MaxPending = erlang:system_info(schedulers_online),
                Continue = make_ref(),
                FLNC = {Family, ListenSocket, NetKernel, Continue},
                Pending = #{},
                accept_loop(
                  FLNC, Continue, spawn_accept(FLNC), MaxPending,
                  Pending)
        end,
        dist_util:net_ticker_spawn_options())).

%% Concurrent accept loop will spawn a new HandshakePid when
%%  there is no HandshakePid already running, and Pending map is
%%  smaller than MaxPending
accept_loop(FLNC, Continue, undefined, MaxPending, Pending)
  when map_size(Pending) < MaxPending ->
    accept_loop(FLNC, Continue, spawn_accept(FLNC), MaxPending, Pending);
accept_loop({_, _, NetKernelPid, _} = FLNC, Continue, HandshakePid, MaxPending, Pending) ->
    receive
        {Continue, HandshakePid} when is_pid(HandshakePid) ->
            accept_loop(
              FLNC, Continue, undefined, MaxPending,
              Pending#{HandshakePid => true});
        {'EXIT', Pid, Reason} when is_map_key(Pid, Pending) ->
            Reason =/= normal andalso
                ?LOG_ERROR("TLS distribution handshake failed: ~p~n", [Reason]),
            accept_loop(
              FLNC, Continue, HandshakePid, MaxPending,
              maps:remove(Pid, Pending));
        {'EXIT', HandshakePid, Reason} when is_pid(HandshakePid) ->
            %% HandshakePid crashed before turning into Pending, which means
            %%  error happened in accept. Need to restart the listener.
            exit(Reason);
        {'EXIT', NetKernelPid, Reason} ->
            %% Since we're trapping exits, need to manually propagate this signal
            exit(Reason);
        Unexpected ->
            ?LOG_WARNING(
               "TLS distribution: unexpected message: ~p~n", [Unexpected]),
            accept_loop(FLNC, Continue, HandshakePid, MaxPending, Pending)
    end.

spawn_accept({Family, ListenSocket, NetKernel, Continue}) ->
    AcceptLoop = self(),
    spawn_link(
        fun () ->
            case gen_tcp:accept(ListenSocket) of
                {ok, Socket} ->
                    AcceptLoop ! {Continue, self()},
                    case check_ip(Socket) of
                        true ->
                            accept_one(Family, Socket, NetKernel);
                        {false,IP} ->
                            ?LOG_ERROR(
                                "** Connection attempt from "
                                "disallowed IP ~w ** ~n", [IP]),
                            trace({disallowed, IP})
                    end;
                Error ->
                    exit(Error)
            end
        end).

accept_one(Family, Socket, NetKernel) ->
    Opts = setup_verify_client(Socket, get_ssl_options(server)),
    KTLS = proplists:get_value(ktls, Opts, false),
    case
        ssl:handshake(
          Socket,
          trace([{active, false},{packet, 4}|Opts]),
          net_kernel:connecttime())
    of
        {ok, SslSocket} ->
            Receiver = hd(SslSocket#sslsocket.pid),
            case KTLS of
                true ->
                    {ok, KtlsInfo} = ssl_gen_statem:ktls_handover(Receiver),
                    case inet_set_ktls(KtlsInfo) of
                        ok ->
                            accept_one(
                              Family, maps:get(socket, KtlsInfo), NetKernel,
                              fun gen_tcp:controlling_process/2);
                        {error, KtlsReason} ->
                            ?LOG_ERROR(
                               [{slogan, set_ktls_failed},
                                {reason, KtlsReason},
                                {pid, self()}]),
                            close(Socket),
                            trace({ktls_error, KtlsReason})
                    end;
                false ->
                    accept_one(
                      Family, SslSocket, NetKernel,
                      fun ssl:controlling_process/2)
            end;
        {error, {options, _}} = Error ->
            %% Bad options: that's probably our fault.
            %% Let's log that.
            ?LOG_ERROR(
              "Cannot accept TLS distribution connection: ~s~n",
              [ssl:format_error(Error)]),
            close(Socket),
            trace(Error);
        Other ->
            close(Socket),
            trace(Other)
    end.
%%
accept_one(
  Family, DistSocket, NetKernel, ControllingProcessFun) ->
    trace(NetKernel ! {accept, self(), DistSocket, Family, ?PROTOCOL}),
    receive
        {NetKernel, controller, Pid} ->
            case ControllingProcessFun(DistSocket, Pid) of
                ok ->
                    trace(Pid ! {self(), controller});
                {error, Reason} ->
                    trace(Pid ! {self(), exit}),
                    ?LOG_ERROR(
                       [{slogan, controlling_process_failed},
                        {reason, Reason},
                        {pid, self()}])
            end;
        {NetKernel, unsupported_protocol} ->
            trace(unsupported_protocol)
    end.


%% {verify_fun,{fun ?MODULE:verify_client/3,_}} is used
%% as a configuration marker that verify_client/3 shall be used.
%%
%% Replace the State in the first occurrence of
%% {verify_fun,{fun ?MODULE:verify_client/3,State}}
%% and remove the rest.
%% The inserted state is not accessible from a configuration file
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


%% -------------------------------------------------------------------------

accept_connection(AcceptPid, DistSocket, MyNode, Allowed, SetupTime) ->
    fam_accept_connection(
      ?FAMILY, AcceptPid, DistSocket, MyNode, Allowed, SetupTime).

fam_accept_connection(
  Family, AcceptPid, DistSocket, MyNode, Allowed, SetupTime) ->
    Kernel = self(),
    monitor_pid(
      spawn_opt(
        fun() ->
                do_accept(
                  Family, AcceptPid, DistSocket,
                  MyNode, Allowed, SetupTime, Kernel)
        end,
        dist_util:net_ticker_spawn_options())).

do_accept(
  Family, AcceptPid, DistSocket, MyNode, Allowed, SetupTime, Kernel) ->
    MRef = erlang:monitor(process, AcceptPid),
    receive
	{AcceptPid, controller} ->
            erlang:demonitor(MRef, [flush]),
            Timer = dist_util:start_timer(SetupTime),
            {HSData0, NewAllowed} =
                case DistSocket of
                    SslSocket = #sslsocket{pid = [_Receiver, Sender| _]} ->
                        link(Sender),
                        {hs_data_ssl(Family, SslSocket),
                         allowed_nodes(SslSocket, Allowed)};
                    PortSocket when is_port(DistSocket) ->
                        %%% XXX Breaking abstraction barrier
                        Driver = erlang:port_get_data(PortSocket),
                        {hs_data_inet_tcp(Driver, PortSocket),
                         Allowed}
                end,
            HSData =
                HSData0#hs_data{
                  kernel_pid = Kernel,
                  this_node = MyNode,
                  timer = Timer,
                  this_flags = 0,
                  allowed = NewAllowed},
            dist_util:handshake_other_started(trace(HSData));
        {AcceptPid, exit} ->
            %% this can happen when connection was initiated, but dropped
            %%  between TLS handshake completion and dist handshake start
            ?shutdown2(MyNode, connection_setup_failed);
        {'DOWN', MRef, _, _, _Reason} ->
            %% this may happen when connection was initiated, but dropped
            %% due to crash propagated from other handshake process which
            %% failed on inet_tcp:accept (see GH-5332)
            ?shutdown2(MyNode, connection_setup_failed)
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


%% -------------------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    fam_setup(?FAMILY, Node, Type, MyNode, LongOrShortNames, SetupTime).

fam_setup(Family, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    NetKernel = self(),
    monitor_pid(
      spawn_opt(
        setup_fun(
          Family, Node, Type, MyNode, LongOrShortNames, SetupTime,
          NetKernel),
        dist_util:net_ticker_spawn_options())).

-spec setup_fun(_,_,_,_,_,_,_) -> fun(() -> no_return()).
setup_fun(
  Family, Node, Type, MyNode, LongOrShortNames, SetupTime, NetKernel) ->
    fun() ->
            do_setup(
              Family, Node, Type, MyNode, LongOrShortNames, SetupTime,
              NetKernel)
    end.

-spec do_setup(_,_,_,_,_,_,_) -> no_return().
do_setup(
  Family, Node, Type, MyNode, LongOrShortNames, SetupTime, NetKernel) ->
    Timer = trace(dist_util:start_timer(SetupTime)),
    ParseAddress = fun (A) -> inet:parse_strict_address(A, Family) end,
    {#net_address{
        host = Host,
        address = {Ip, PortNum}},
     ConnectOptions,
     Version} =
        trace(inet_tcp_dist:fam_setup(
                Family, Node, LongOrShortNames, ParseAddress)),
    Opts =
        inet_tcp_dist:merge_options(
          inet_tcp_dist:merge_options(
            ConnectOptions,
            get_ssl_options(client)),
          [Family, binary, {active, false}, {packet, 4},
           {read_ahead, false}, {nodelay, true}],
          [{server_name_indication, Host}]),
    KTLS = proplists:get_value(ktls, Opts, false),
    dist_util:reset_timer(Timer),
    maybe
        {ok, #sslsocket{pid = [Receiver, Sender| _]} = SslSocket} ?=
            ssl:connect(Ip, PortNum, Opts, net_kernel:connecttime()),
        HSData =
            case KTLS of
                true ->
                    {ok, KtlsInfo} =
                        ssl_gen_statem:ktls_handover(Receiver),
                    Socket = maps:get(socket, KtlsInfo),
                    case inet_set_ktls(KtlsInfo) of
                        ok when is_port(Socket) ->
                            %% XXX Breaking abstraction barrier
                            Driver = erlang:port_get_data(Socket),
                            hs_data_inet_tcp(Driver, Socket);
                        {error, KtlsReason} ->
                            ?shutdown2(
                               Node,
                               trace({set_ktls_failed, KtlsReason}))
                    end;
                false ->
                    _ = monitor_pid(Sender),
                    ok = ssl:controlling_process(SslSocket, self()),
                    link(Sender),
                    hs_data_ssl(Family, SslSocket)
            end
            #hs_data{
              kernel_pid = NetKernel,
              other_node = Node,
              this_node = MyNode,
              timer = Timer,
              this_flags = 0,
              other_version = Version,
              request_type = Type},
        dist_util:handshake_we_started(trace(HSData))
    else
        Other ->
            %% Other Node may have closed since
            %% port_please !
            ?shutdown2(
               Node,
               trace({ssl_connect_failed, Ip, PortNum, Other}))
    end.


close(Socket) ->
    gen_tcp:close(Socket).


%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Socket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
            maybe
                {ok, {IP, _}} ?= inet:sockname(Socket),
                ok ?= if is_tuple(IP) -> ok;
                         true -> {error, {no_ip_address, IP}}
                      end,
                {ok, Ifaddrs} ?= inet:getifaddrs(),
                {ok, Netmask} ?= find_netmask(IP, Ifaddrs),
                {ok, {PeerIP, _}} ?= inet:sockname(Socket),
                ok ?= if is_tuple(PeerIP) -> ok;
                         true -> {error, {no_ip_address, PeerIP}}
                      end,
                mask(IP, Netmask) =:= mask(PeerIP, Netmask)
                    orelse {false, PeerIP}
            else
                Other ->
                    exit({check_ip, Other})
            end;
	_ ->
	    true
    end.

find_netmask(IP, [{_Name,Items} | Ifaddrs]) ->
    find_netmask(IP, Ifaddrs, Items);
find_netmask(_, []) ->
    {error, no_netmask}.
%%
find_netmask(IP, _Ifaddrs, [{addr, IP}, {netmask, Netmask} | _]) ->
    {ok, Netmask};
find_netmask(IP, Ifaddrs, [_ | Items]) ->
    find_netmask(IP, Ifaddrs, Items);
find_netmask(IP, Ifaddrs, []) ->
    find_netmask(IP, Ifaddrs).

mask(Addr, Mask) ->
    list_to_tuple(mask(Addr, Mask, 1)).
%%
mask(Addr, Mask, N) when N =< tuple_size(Addr) ->
    [element(N, Addr) band element(N, Mask) | mask(Addr, Mask, N + 1)];
mask(_, _, _) ->
    [].



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


%% -------------------------------------------------------------------------
get_ssl_options(Type) ->
    [{erl_dist, true} |
     case
         case init:get_argument(ssl_dist_opt) of
             {ok, Args} ->
                 ssl_options(Type, lists:append(Args));
             _ ->
                 []
         end
         ++
         try ets:lookup(ssl_dist_opts, Type) of
             [{Type, Opts0}] ->
                 Opts0;
             _ ->
                 []
         catch
             error:badarg ->
                 []
         end
     of
         [] ->
             [];
         Opts1 ->
             dist_defaults(Opts1)
     end].

dist_defaults(Opts) ->
    case proplists:get_value(versions, Opts, undefined) of
        undefined ->
            [{versions, ['tlsv1.2']} | Opts];
        _ ->
            Opts
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
        "ciphers" ->
            %% Allows just one cipher, for now (could be , separated)
            fun (Val) -> [listify(Val)] end;
        "versions" ->
            %% Allows just one version, for now (could be , separated)
            fun (Val) -> [atomize(Val)] end;
        "ktls" -> fun atomize/1;
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


inet_set_ktls(
  #{ socket := Socket, socket_options := SocketOptions } = KtlsInfo) ->
    %%
    maybe
        ok ?=
            set_ktls(
              KtlsInfo
              #{ setopt_fun => fun ?MODULE:inet_ktls_setopt/3,
                 getopt_fun => fun ?MODULE:inet_ktls_getopt/3 }),
        %%
        #socket_options{
           mode = _Mode,
           packet = Packet,
           packet_size = PacketSize,
           header = Header,
           active = Active
          } = SocketOptions,
        case
            inet:setopts(
              Socket,
              [list, {packet, Packet}, {packet_size, PacketSize},
               {header, Header}, {read_ahead, true}, {active, Active}])
        of
            ok ->
                ok;
            {error, SetoptError} ->
                {error, {ktls_setopt_failed, SetoptError}}
        end
    end.

inet_ktls_setopt(Socket, {Level, Opt}, Value)
  when is_integer(Level), is_integer(Opt), is_binary(Value) ->
    inet:setopts(Socket, [{raw, Level, Opt, Value}]).

inet_ktls_getopt(Socket, {Level, Opt}, Size)
  when is_integer(Level), is_integer(Opt), is_integer(Size) ->
    case inet:getopts(Socket, [{raw, Level, Opt, Size}]) of
        {ok, [{raw, Level, Opt, Value}]} ->
            {ok, Value};
        {ok, _} = Error ->
            {error, Error};
        {error, _} = Error ->
            Error
    end.


set_ktls(KtlsInfo) ->
    maybe
        {ok, OS} ?= ktls_os(),
        ok ?= set_ktls_ulp(KtlsInfo, OS),
        #{ write_state := WriteState,
           write_seq := WriteSeq,
           read_state := ReadState,
           read_seq := ReadSeq } = KtlsInfo,
        ok ?= set_ktls_cipher(KtlsInfo, OS, WriteState, WriteSeq, tx),
        set_ktls_cipher(KtlsInfo, OS, ReadState, ReadSeq, rx)
    end.

set_ktls_ulp(
  #{ socket := Socket,
     setopt_fun := SetoptFun,
     getopt_fun := GetoptFun },
  OS) ->
    %%
    {Option, Value} = ktls_opt_ulp(OS),
    Size = byte_size(Value),
    _ = SetoptFun(Socket, Option, Value),
    %%
    %% Check if kernel module loaded,
    %% i.e if getopts Level, Opt returns Value
    %%
    case GetoptFun(Socket, Option, Size + 1) of
        {ok, <<Value:Size/binary, 0>>} ->
            ok;
        Other ->
            {error, {ktls_set_ulp_failed, Option, Value, Other}}
    end.

%% Set kTLS cipher
%%
set_ktls_cipher(
  _KtlsInfo =
      #{ tls_version := TLS_version,
         cipher_suite := CipherSuite,
         %%
         socket := Socket,
         setopt_fun := SetoptFun,
         getopt_fun := GetoptFun },
  OS, CipherState, CipherSeq, TxRx) ->
    maybe
        {ok, {Option, Value}} ?=
            ktls_opt_cipher(
              OS, TLS_version, CipherSuite, CipherState, CipherSeq, TxRx),
        _ = SetoptFun(Socket, Option, Value),
        case TxRx of
            tx ->
                Size = byte_size(Value),
                case GetoptFun(Socket, Option, Size) of
                    {ok, Value} ->
                        ok;
                    Other ->
                        {error, {ktls_set_cipher_failed, Other}}
                end;
            rx ->
                ok
        end
    end.

ktls_os() ->
    OS = {os:type(), os:version()},
    case OS of
        {{unix,linux}, OsVersion} when {5,2,0} =< OsVersion ->
            {ok, OS};
        _  ->
            {error, {ktls_notsup, {os,OS}}}
    end.

ktls_opt_ulp(_OS) ->
    %%
    %% See https://www.kernel.org/doc/html/latest/networking/tls.html
    %% and include/netinet/tcp.h
    %%
    SOL_TCP = 6, TCP_ULP = 31,
    KtlsMod = <<"tls">>,
    {{SOL_TCP,TCP_ULP}, KtlsMod}.

ktls_opt_cipher(
  _OS,
  _TLS_version = ?TLS_1_3, % 'tlsv1.3'
  _CipherSpec = ?TLS_AES_256_GCM_SHA384,
  #cipher_state{
     key = <<Key:32/bytes>>,
     iv = <<Salt:4/bytes, IV:8/bytes>> },
  CipherSeq,
  TxRx) when is_integer(CipherSeq) ->
    %%
    %% See include/linux/tls.h
    %%
    TLS_1_3_VERSION_MAJOR = 3,
    TLS_1_3_VERSION_MINOR = 4,
    TLS_1_3_VERSION =
        (TLS_1_3_VERSION_MAJOR bsl 8) bor TLS_1_3_VERSION_MINOR,
    TLS_CIPHER_AES_GCM_256 = 52,
    SOL_TLS = 282,
    TLS_TX = 1,
    TLS_RX = 2,
    Value =
        <<TLS_1_3_VERSION:16/native,
          TLS_CIPHER_AES_GCM_256:16/native,
          IV/bytes, Key/bytes,
          Salt/bytes, CipherSeq:64/native>>,
    %%
    SOL_TLS = 282,
    TLS_TX = 1,
    TLS_RX = 2,
    TLS_TxRx =
        case TxRx of
            tx -> TLS_TX;
            rx -> TLS_RX
        end,
    {ok, {{SOL_TLS,TLS_TxRx}, Value}};
ktls_opt_cipher(
  _OS, TLS_version, CipherSpec, _CipherState, _CipherSeq, _TxRx) ->
    {error,
     {ktls_notsup, {cipher, TLS_version, CipherSpec, _CipherState}}}.


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
