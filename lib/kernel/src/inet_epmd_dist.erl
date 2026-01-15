%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

-module(inet_epmd_dist).
-moduledoc false.

%% DistMod API - own inet_tcp_dist equivalence implementation
-export([net_address/0, listen_open/2, listen_port/3, listen_close/1,
         accept_open/2, accept_controller/3, accepted/3,
         connect/3]).
%% DistMod helper API
-export([check_ip/2,
         hs_data/2, f_address/2, tick/1, getstat/1, setopts/2, getopts/2,
         nodelay/0, merge_options/3]).

%% net_kernel and dist_util distribution Module API
-export([childspecs/0,
         address/1, listen/2,
         accept/1, accept_connection/5,
         select/1, setup/5,
         close/1]).

-include("net_address.hrl").
-include("dist.hrl").
-include("dist_util.hrl").

-define(DISTNAME, inet_epmd).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DistMod API
%%
%% Called by net_kernel:
%%
%%     net_address() ->
%%         #net_address{ protocol = Protocol, family = Family }.
%%
%%     listen_open(NetAddress, ListenOptions) ->
%%         {ok, StateO} |
%%         {error, _}.
%%
%%     listen_port(NetAddress, Port, StateO) ->
%%         {ok, {StateL, {Ip, Port}}} |
%%         {error, _}.
%%
%%     listen_close(StateL) ->
%%         ok.
%%
%%
%% Called by Acceptor:
%%
%%     %% Wait for incoming connection,
%%     %% return new socket and endpoint addresses
%%     accept_open(NetAddress, StateL) ->
%%         {StateA, PeerAddress}.
%%
%%     %% Notification about the process id of the Controller
%%     %% - transfer socket ownership to it
%%     accept_controller(NetAddress, Controller, StateA) ->
%%         StateC.
%%
%%
%% Called by Accept Controller:
%%
%%     accepted(NetAddress, Timer, StateC) ->
%%         #hs_data{} | {error, Reason}
%%
%%
%% Called by Connect Controller:
%%     connect(NetAddress, Timer, ConnectOptions) ->
%%         #hs_data{} | {error, Reason}
%%
%% ------------------------------------------------------------
%% Own DistMod API implementation -
%% when used by this module the combination
%% is equivalent to inet_tcp_dist

-define(DRIVER, inet_tcp).
-define(PROTOCOL, tcp).

%% ------------------------------------------------------------
net_address() ->
    Family = ?DRIVER:family(),
    #net_address{
       protocol = ?PROTOCOL,
       family = Family }.

%% ------------------------------------------------------------
listen_open(_NetAddress, Options) ->
    {ok, merge_options(Options, [{active, false}, {packet, 2}], [])}.

%% ------------------------------------------------------------
listen_port(_NetAddress, Port, ListenOptions) ->
    maybe
        {ok, ListenSocket} ?=
            ?DRIVER:listen(Port, ListenOptions),
        {ok, Address} ?=
            inet:sockname(ListenSocket),
        {ok, {ListenSocket, Address}}
    end.

%% ------------------------------------------------------------
listen_close(ListenSocket) ->
    ?DRIVER:close(ListenSocket).

%% ------------------------------------------------------------
accept_open(_NetAddress, ListenSocket) ->
    maybe
        {ok, Socket} ?=
            ?DRIVER:accept(ListenSocket),
        {ok, {Ip, _}} ?=
            inet:sockname(Socket),
        {ok, {PeerIp, _} = PeerAddress} ?=
            inet:peername(Socket),
        check_ip(Ip, PeerIp),
        {Socket, PeerAddress}
    else
        {error, Reason} ->
            exit({accept, Reason})
    end.

%% ------------------------------------------------------------
accept_controller(_NetAddress, Controller, Socket) ->
    ?DRIVER:controlling_process(Socket, Controller),
%%%     flush_to(Socket, Controller),
    Socket.

-ifdef(undefined).
flush_to(Socket, Pid) ->
    receive
	{tcp, Socket, Data} ->
	    Pid ! {tcp, Socket, Data},
	    flush_to(Socket, Pid);
	{tcp_closed, Socket} ->
	    Pid ! {tcp_closed, Socket},
	    flush_to(Socket, Pid)
    after 0 ->
	    ok
    end.
-endif.

%% ------------------------------------------------------------
accepted(NetAddress, _Timer, Socket) ->
    hs_data(NetAddress, Socket).

%% ------------------------------------------------------------
connect(NetAddress, _Timer, Options) ->
    ConnectOptions =
        merge_options(Options, [{active, false}, {packet, 2}], []),
    #net_address{ address = {Ip, Port} } = NetAddress,
    maybe
        {ok, Socket} ?=
            ?DRIVER:connect(Ip, Port, ConnectOptions),
        hs_data(NetAddress, Socket)
    else
        {error, _} = Error ->
            Error
    end.

%% -------
hs_data(NetAddress, Socket) ->
    Nodelay = nodelay(),
    #hs_data{
       socket = Socket,
       f_send = fun ?DRIVER:send/2,
       f_recv = fun ?DRIVER:recv/3,
       f_setopts_pre_nodeup =
           fun (S) when S =:= Socket ->
                   f_setopts_pre_nodeup(S, Nodelay)
           end,
       f_setopts_post_nodeup =
           fun (S) when S =:= Socket ->
                   f_setopts_post_nodeup(S, Nodelay)
           end,
       f_address =
           fun (S, Node) when S =:= Socket ->
                   f_address(NetAddress, Node)
           end,
       f_getll    = fun inet:getll/1,
       mf_tick    = fun ?MODULE:tick/1,
       mf_getstat = fun ?MODULE:getstat/1,
       mf_setopts = fun ?MODULE:setopts/2,
       mf_getopts = fun ?MODULE:getopts/2 }.

f_setopts_pre_nodeup(Socket, Nodelay) ->
    inet:setopts(Socket, [{active, false}, {packet, 4}, Nodelay]).

f_setopts_post_nodeup(Socket, Nodelay) ->
    inet:setopts(
      Socket,
      [{active, true}, {packet,4}, {deliver, port}, binary, Nodelay]).

f_address(NetAddress, Node) ->
    case dist_util:split_node(Node) of
        {node, _Name, Host} ->
            NetAddress#net_address{
              host = Host };
        Other ->
            ?shutdown2(Node, {split_node, Other})
    end.

tick(Socket) when is_port(Socket) ->
    Result = ?DRIVER:send(Socket, [], [force]),
    _ = (Result =:= {error, closed}) andalso
        (self() ! {tcp_closed, Socket}),
    Result.

getstat(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
	{ok, Stat} ->
	    split_stat(Stat,0,0,0);
	Error ->
	    Error
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

setopts(S, Opts) ->
    case [Opt || {K,_}=Opt <- Opts,
		 K =:= active orelse K =:= deliver orelse K =:= packet] of
	[] -> inet:setopts(S,Opts);
	Opts1 -> {error, {badopts,Opts1}}
    end.

getopts(S, OptNames) ->
    inet:getopts(S, OptNames).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% net_kernel distribution module API

childspecs() ->
    case pt_init_dist_mod() of
        ?MODULE -> {error, no_childspecs};
        DistMod -> DistMod:childspecs()
    end.

%% ------------------------------------------------------------
%% Return which #net_address{} we handle
%% ------------------------------------------------------------

address(Host) ->
    pt_init_net_address(Host).

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name, Host) ->
    try
        maybe
            DistMod = pt_init_dist_mod(),
            NetAddress = pt_init_net_address(Host),
            EpmdMod = net_kernel:epmd_module(),
            %%
            {ok, ListenOptions} ?=
                %% *******
                DistMod:listen_open(NetAddress, listen_options()),
            {First, Last} =
                case
                    call_epmd_function(
                  EpmdMod, listen_port_please, [Name, Host])
                of
                    {ok, 0} ->
                        get_port_range();
                    {ok, PortNum} ->
                        {PortNum, PortNum}
                end,
            #net_address{ family = Family } = NetAddress = pt_get(net_address),
            %%
            {ok, Result} ?=
                listen_loop(First, NetAddress, ListenOptions, Last, DistMod),
            {StateL, {_Ip, Port} = Address} = Result,
            %%
            {ok, Creation} ?=
                EpmdMod:register_node(Name, Port, Family),
            NetAddress_1 = NetAddress#net_address{ address = Address },
            {ok, {{NetAddress_1, StateL}, NetAddress_1, Creation}}
        else
            {error, _} = Error ->
                Error
        end
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

get_port_range() ->
    case application:get_env(kernel, inet_dist_listen_min) of
        {ok,N} when is_integer(N) ->
            case application:get_env(kernel, inet_dist_listen_max) of
                {ok,M} when is_integer(M) ->
                    {N,M};
                _ ->
                    {N,N}
            end;
        _ ->
            {0,0}
    end.

listen_loop(Port, NetAddress, ListenOptions, LastPort, DistMod)
  when Port =< LastPort ->
    case
        %% *******
        DistMod:listen_port(NetAddress, Port, ListenOptions)
    of
        {error, eaddrinuse} ->
            listen_loop(
              Port + 1, NetAddress, ListenOptions, LastPort, DistMod);
        Result ->
            Result
    end;
listen_loop(_, _, _, _, _) ->
    {error, eaddrinuse}.

%% ------------------------------------------------------------
%% Close the listen socket
%% ------------------------------------------------------------
close(ListenSocket) ->
    (pt_get(dist_mod)):listen_close(ListenSocket).

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% Loops accept on the listen socket and notifies net_kernel.
%% ------------------------------------------------------------

accept({NetAddress, StateL}) ->
    try
        NetKernel = self(),
        DistMod = pt_get(dist_mod),
        AcceptLoop =
            spawn_link(
              fun () ->
                      _ = process_flag(trap_exit, true),
                      accept_loop(
                        StateL, NetAddress, NetKernel, DistMod,
                        erlang:system_info(schedulers_online), #{})
              end),
        AcceptLoop
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

%% Open parallel acceptors on the listen socket
%%
accept_loop(
  StateL, NetAddress, NetKernel, DistMod, MaxPending, Acceptors)
  when map_size(Acceptors) =< MaxPending ->
    AcceptRef = make_ref(),
    Acceptor =
        spawn_link(
          acceptor_fun(StateL, NetAddress, NetKernel, DistMod, AcceptRef)),
    accept_loop(
      StateL, NetAddress, NetKernel, DistMod, MaxPending,
      Acceptors#{ Acceptor => AcceptRef });
accept_loop(
  StateL, NetAddress, NetKernel, DistMod, MaxPending, Acceptors) ->
    receive Msg ->
            case Msg of
                {'EXIT', Acceptor, Reason}
                  when is_map_key(Acceptor, Acceptors) ->
                    AcceptRef = maps:get(Acceptor, Acceptors),
                    case Reason of
                        AcceptRef -> % Done ok
                            accept_loop(
                              StateL, NetAddress, NetKernel, DistMod,
                              MaxPending, maps:remove(Acceptor, Acceptors));
                        {accept, _} ->
                            %% Should mean that accept failed
                            %% so we need to restart the listener
                            exit(Reason);
                        _ ->
                            error_logger:warning_msg(
                              "~w:~w acceptor ~w failed: ~p",
                              [?MODULE, ?FUNCTION_NAME, Acceptor, Reason]),
                            accept_loop(
                              StateL, NetAddress, NetKernel, DistMod,
                              MaxPending, maps:remove(Acceptor, Acceptors))
                    end;
                {'EXIT', NetKernel, Reason} ->
                    exit(Reason);
                _ ->
                    error_logger:warning_msg(
                      "~w:~w unknown message: ~p",
                      [?MODULE, ?FUNCTION_NAME, Msg]),
                    accept_loop(
                      StateL, NetAddress, NetKernel, DistMod,
                      MaxPending, Acceptors)
            end
    end.

%% Return value with exit/1
-spec acceptor_fun(_, _, _, _, _) -> fun(() -> no_return()).
acceptor_fun(
  StateL,
  #net_address{ family = Family, protocol = Protocol } = NetAddress,
  NetKernel, DistMod, AcceptRef) ->
    fun () ->
            {StateA, PeerAddress} =
                %% *******
                DistMod:accept_open(NetAddress, StateL),
            %%
            NetAddress_1 = NetAddress#net_address{ address = PeerAddress },
            Acceptor = self(),
            NetKernel ! {accept, Acceptor, NetAddress_1, Family, Protocol},
            receive
                {NetKernel, controller, Controller} ->
                    StateD =
                        %% *******
                        DistMod:accept_controller(
                          NetAddress_1, Controller, StateA),
                    Controller !
                        {Acceptor, controller, DistMod, StateD},
                    exit(AcceptRef);
                {NetKernel, unsupported_protocol = Reason} ->
                    exit(Reason)
            end
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(Acceptor, NetAddress, MyNode, Allowed, SetupTime) ->
    try
        NetKernel = self(),
        Controller =
            spawn_opt(
              fun () ->
                      accept_controller(
                        Acceptor, NetAddress, MyNode, Allowed, SetupTime,
                        NetKernel)
              end, dist_util:net_ticker_spawn_options()),
        Controller
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

accept_controller(
  Acceptor, NetAddress, MyNode, Allowed, SetupTime,
  NetKernel) ->
    receive
        {Acceptor, controller, DistMod, StateD} ->
            Timer = dist_util:start_timer(SetupTime),
            case
                %% *******
                DistMod:accepted(NetAddress, Timer, StateD)
            of
                #hs_data{} = HsData ->
                    dist_util:handshake_other_started(
                      HsData
                      #hs_data{
                        kernel_pid = NetKernel,
                        this_node = MyNode,
                        timer = Timer,
                        allowed = Allowed });
                {error, Reason} ->
                    ?shutdown({{DistMod, accepted}, Reason})
            end
    end.

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    try
        case dist_util:split_node(Node) of
            {node, Name, Host} ->
                #net_address{ family = Family } = pt_get(net_address),
                EpmdMod = net_kernel:epmd_module(),
                case
                    call_epmd_function(
                      EpmdMod, address_please, [Name, Host, Family])
                of
                    {ok, _Addr} -> true;
                    {ok, _Addr, _Port, _Creation} -> true;
                    _ -> false
                end;
            _ -> false
        end
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    try
        NetKernel = self(),
        Controller =
            spawn_opt(
              fun () ->
                      setup(
                        Node, Type, MyNode, LongOrShortNames, SetupTime,
                        NetKernel)
              end, dist_util:net_ticker_spawn_options()),
        Controller
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

setup(Node, Type, MyNode, LongOrShortNames, SetupTime, NetKernel) ->
    Timer = dist_util:start_timer(SetupTime),
    DistMod = pt_get(dist_mod),
    #net_address{ family = Family } = NetAddress = pt_get(net_address),
    {Name, Host} = split_node(Node, LongOrShortNames, Family),
    ErlEpmd = net_kernel:epmd_module(),
    {Address, Version} =
        case
            call_epmd_function(
              ErlEpmd, address_please, [Name, Host, Family])
        of
            {ok, Ip, Port, Ver} ->
                {{Ip, Port}, Ver};
            {ok, Ip} ->
                case ErlEpmd:port_please(Name, Ip) of
                    {port, Port, Ver} ->
                        {{Ip, Port}, Ver};
                    Other ->
                        ?shutdown2(Node, {port_please, Other})
                end;
            Other ->
                ?shutdown2(Node, {address_please, Other})
        end,
    NetAddress_1 =
        NetAddress#net_address{
          host = Host,
          address = Address },
    dist_util:reset_timer(Timer),
    case
        %% *******
        DistMod:connect(NetAddress_1, Timer, connect_options())
    of
        #hs_data{} = HsData ->
            dist_util:handshake_we_started(
              HsData
              #hs_data{
                kernel_pid = NetKernel,
                other_node = Node,
                this_node = MyNode,
                timer = Timer,
                other_version = Version,
                request_type = Type });
        {error, Reason} ->
            ?shutdown2(Node, {{DistMod, connect}, Reason})
    end.

%% ------------------------------------------------------------
%% Check that accepted address is on our subnet
%%
%% Only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Ip, PeerIp) ->
    try
        case application:get_env(kernel, check_ip) of
            {ok, true} ->
                maybe
                    {ok, Ifaddrs} ?= inet:getifaddrs(),
                    {ok, Netmask} ?= find_netmask(Ip, Ifaddrs),
                    mask(Ip, Netmask) =:= mask(PeerIp, Netmask) orelse
                        begin
                            error_logger:error_msg(
                              "** Connection attempt from "
                              "disallowed IP ~w ** ~n",
                              [PeerIp]),
                            ?shutdown(no_node)
                        end,
                    ok
                else
                    Other ->
                        exit({check_ip, Other})
                end;
            _ ->
                ok
        end
    catch error : Reason : Stacktrace ->
            error_logger:error_msg(
              "error : ~p in ~n    ~p~n", [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.

find_netmask(Ip, [{_Name,Items} | Ifaddrs]) ->
    find_netmask(Ip, Ifaddrs, Items);
find_netmask(_, []) ->
    {error, no_netmask}.
%%
find_netmask(Ip, _Ifaddrs, [{addr, Ip}, {netmask, Netmask} | _]) ->
    {ok, Netmask};
find_netmask(Ip, Ifaddrs, [_ | Items]) ->
    find_netmask(Ip, Ifaddrs, Items);
find_netmask(Ip, Ifaddrs, []) ->
    find_netmask(Ip, Ifaddrs).

mask(Addr, Mask) ->
    mask(Addr, Mask, 1).
%%
mask(Addr, Mask, N) when N =< tuple_size(Addr) ->
    [element(N, Addr) band element(N, Mask) | mask(Addr, Mask, N + 1)];
mask(_, _, _) ->
    [].


%% ------------------------------------------------------------
%% Split and validate node name
%% ------------------------------------------------------------

split_node(Node, LongOrShortNames, Family) ->
    case dist_util:split_node(Node) of
        {node, Name, Host} ->
            Dots = members($., Host),
            if
                LongOrShortNames =:= longnames, 0 < Dots ->
                    {Name, Host};
                LongOrShortNames =:= longnames ->
                    case inet:parse_strict_address(Host, Family) of
                        {ok, _} ->
                            %% We count an IP address as a long name
                            %% since it is not relative to the current
                            %% domain.
                            %%
                            %% This clause is for for IPv6 addresses
                            %% that are : separated.
                            {Name, Host};
                        {error, Reason} ->
                            error_logger:error_msg(
                              "** System running to use fully qualified "
                              "hostnames **~n"
                              "** Hostname ~ts is illegal **~n",
                              [Host]),
                            ?shutdown2(Node, {parse_address, Reason})
                    end;
                LongOrShortNames =:= shortnames, 0 < Dots ->
		    error_logger:error_msg(
                      "** System NOT running to use fully qualified "
                      "hostnames **~n"
                      "** Hostname ~ts is illegal **~n",
                      [Host]),
		    ?shutdown(Node);
                LongOrShortNames =:= shortnames ->
                    {Name, Host}
            end;
        Other ->
	    error_logger:error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown2(Node, {split_node, Other})
    end.

%% Count list members
members(X, [X | T]) -> members(X, T) + 1;
members(X, [_ | T]) -> members(X, T);
members(_, [])      -> 0.

%% ------------------------------------------------------------
%% Determine if EPMD module supports the called functions.
%% If not call the builtin erl_epmd
%% ------------------------------------------------------------
call_epmd_function(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true -> apply(Mod,Fun,Args);
        _    -> apply(erl_epmd, Fun, Args)
    end.


%% ------------------------------------------------------------

listen_options() ->
    DefaultOpts = [{reuseaddr, true}, {backlog, 128}],
    ForcedOpts =
        case application:get_env(kernel, inet_dist_use_interface) of
            {ok, Ip}  -> [{ip, Ip}];
            undefined -> []
        end,
    InetDistListenOpts =
        case application:get_env(kernel, inet_dist_listen_options) of
            {ok, Opts} -> Opts;
            undefined  -> []
        end,
    merge_options(InetDistListenOpts, ForcedOpts, DefaultOpts).

connect_options() ->
    case application:get_env(kernel, inet_dist_connect_options) of
        {ok, ConnectOpts} ->
            ConnectOpts;
        _ ->
            []
    end.

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


merge_options(Opts, ForcedOpts, DefaultOpts) ->
    Forced = merge_options(ForcedOpts),
    Default = merge_options(DefaultOpts),
    ForcedOpts ++ merge_options(Opts, Forced, DefaultOpts, Default).

%% Collect expanded 2-tuple options in a map
merge_options(Opts) ->
    lists:foldr(
      fun (Opt, Acc) ->
              case expand_option(Opt) of
                  {OptName, OptVal} ->
                      maps:put(OptName, OptVal, Acc);
                  _ ->
                      Acc
              end
      end, #{}, Opts).

%% Pass through all options that are not forced,
%% which we already have prepended,
%% and remove options that we see from the Default map
%%
merge_options([Opt | Opts], Forced, DefaultOpts, Default) ->
    case expand_option(Opt) of
        {OptName, _} ->
            %% Remove from the Default map
            Default_1 = maps:remove(OptName, Default),
            if
                is_map_key(OptName, Forced) ->
                    %% Forced option - do not pass through
                    merge_options(Opts, Forced, DefaultOpts, Default_1);
                true ->
                    %% Pass through
                    [Opt |
                     merge_options(Opts, Forced, DefaultOpts, Default_1)]
            end;
        _ ->
            %% Unhandled options e.g {raw, ...} - pass through
            [Opt | merge_options(Opts, Forced, DefaultOpts, Default)]
    end;
merge_options([], _Forced, DefaultOpts, Default) ->
    %% Append the needed default options (that we have not seen)
    [Opt ||
        Opt <- DefaultOpts,
        is_map_key(element(1, expand_option(Opt)), Default)].

%% Expand an atom option into its tuple equivalence,
%% pass through others
expand_option(Opt) ->
    if
        Opt =:= list; Opt =:= binary ->
            {mode, Opt};
        Opt =:= inet; Opt =:= inet6; Opt =:= local ->
            %% 'family' is not quite an option name, but could/should be
            {family, Opt};
        true ->
            Opt
    end.

%% ------------------------------------------------------------
%% Cache distribution module parameters
%% ------------------------------------------------------------

pt_get(Key)
  when Key =:= dist_mod;
       Key =:= net_address ->
    persistent_term:get({?MODULE, Key}).

pt_init_dist_mod() ->
    maybe
        {ok, [[DistModStr]]} ?= init:get_argument(?DISTNAME),
        DistMod = list_to_atom(atom_to_list(?DISTNAME) ++ "_" ++ DistModStr),
        persistent_term:put({?MODULE, dist_mod}, DistMod),
        DistMod
    else
        Other ->
            error_logger:error_msg(
              "init:get_arguments(~w) -> ~p~n", [?DISTNAME, Other]),
            exit({{init,get_argument,[?DISTNAME]},Other})
    end.

pt_init_net_address(Host) ->
    DistMod = pt_get(dist_mod),
    NetAddress =
        %% *******
        (DistMod:net_address())
        #net_address{ host = Host },
    persistent_term:put({?MODULE, net_address}, NetAddress),
    NetAddress.
