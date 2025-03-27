%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

-module(gen_tcp_socket).
-moduledoc false.
-behaviour(gen_statem).

-compile({no_auto_import, [monitor/1]}).

%% gen_tcp
-export([connect/3, connect/4,
         listen/2, accept/2,
         send/2, recv/3,
         sendfile/4,
         shutdown/2, close/1, controlling_process/2]).
%% inet
-export([
         monitor/1, cancel_monitor/1,
         setopts/2, getopts/2,
         sockname/1, peername/1,
         socknames/1,
         getstat/2
        ]).

%% Utility
-export([info/1, which_sockets/0, which_packet_type/1, socket_to_list/1]).

%% Undocumented or unsupported
-export([unrecv/2]).
-export([fdopen/2]).
-export([socket_setopts/2]).


%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).
-export([handle_event/4]).

-include("inet_int.hrl").
-include("socket_int.hrl").

-define(RECV_BUFFER_SIZE_DEFAULT, 8192).

%% -define(DBG(T),
%% 	erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

%% -define(P(F),    ?P(F, [])).
%% -define(P(F, A), d("~w:~w(~w) -> " ++ F, [?MODULE, ?FUNCTION_NAME, ?LINE | A])).


%% -------------------------------------------------------------------------

%% Construct a "socket" as in this module's API
-define(MODULE_socket(Server, Socket),
        {'$inet', ?MODULE, {Server, Socket}}).

%% Standard length before data header for packet,1|2|4
-define(header(Packet, Size),
        (Size):(Packet)/unit:8-integer-big-unsigned).

-define(badarg_exit(Error),
        case begin Error end of
            {error, badarg} -> exit(badarg);
            OTHER__ -> OTHER__
        end).

-define(badarg_einval_exit(Error),
        case begin Error end of
            {error, badarg} -> exit(badarg);
            {error, einval} -> exit(badarg);
            OTHER__ -> OTHER__
        end).

-define(socket_abort(Socket, SelectHandle, Reason),
        ?ESOCK_ABORT_MSG(Socket, SelectHandle, Reason)).
-define(socket_select(Socket, SelectHandle),
        ?ESOCK_SELECT_MSG(Socket, SelectHandle)).
-define(socket_completion(Socket, CH, CS),
        ?ESOCK_COMPLETION_MSG(Socket, CH, CS)).

-define(socket_counter_wrap(Socket, Counter),
        ?ESOCK_SOCKET_MSG(Socket, counter_wrap, Counter)).

-define(select_info(SelectHandle),
        ?ESOCK_SELECT_INFO(SelectHandle)).
-define(completion_info(CompletionHandle),
        ?ESOCK_COMPLETION_INFO(CompletionHandle)).

-define(CLOSED_SOCKET, #{rstates => [closed], wstates => [closed]}).

%% Options that are inherited by accept/2
-compile({inline, [socket_inherit_opts/0]}).
socket_inherit_opts() ->
    [priority].


%%% ========================================================================
%%% API
%%%

connect(SockAddr, Opts, Timeout) ->
    Timer = inet:start_timer(Timeout),
    try
        connect_lookup(SockAddr, Opts, Timer)
    after
        _ = inet:stop_timer(Timer)
    end.

connect(Address, Port, Opts, Timeout) ->
    %% ?DBG([{address, Address}, {port, Port}, {opts, Opts}, {timeout, Timeout}]),
    Timer = inet:start_timer(Timeout),
    try
        connect_lookup(Address, Port, Opts, Timer)
    after
        _ = inet:stop_timer(Timer)
    end.


%% Helpers -------

connect_lookup(#{family := Domain,
                 addr   := Address,
                 port   := Port} = _SockAddr, Opts0, Timer) ->
    %% ?DBG([{domain, Domain}, {addr, Address}, {port, Port},
    %%       {opts0, Opts0}, {timer, Timer}]),
    Opts1        = internalize_setopts(Opts0),
    {Mod, Opts2} = inet:tcp_module(Opts1, Address),
    connect_lookup(Domain, Address, Port, Mod, Opts2, Timer).


connect_lookup(Address, Port, Opts0, Timer) ->
    %% ?DBG([{addr, Address}, {port, Port},
    %%       {opts0, Opts0}, {timer, Timer}]),
    Opts1        = internalize_setopts(Opts0),
    {Mod, Opts2} = inet:tcp_module(Opts1, Address),
    Domain       = domain(Mod),
    connect_lookup(Domain, Address, Port, Mod, Opts2, Timer).

connect_lookup(Domain, Address, Port, Mod, Opts0, Timer) ->
    %% ?DBG([{domain, Domain}, {addr, Address}, {port, Port},
    %%       {mod, Mod}, {opts0, Opts0}, {timer, Timer}]),
    {StartOpts, Opts1} = split_start_opts(Opts0),
    {OpenOpts0, Opts2} = split_open_opts(Opts1),
    ErrRef = make_ref(),
    try
	%% ?DBG(['try getaddrs']),
        IPs = val(ErrRef, Mod:getaddrs(Address, Timer)),
	%% ?DBG(['try getserv']),
        TP  = val(ErrRef, Mod:getserv(Port)),
	%% ?DBG(['process connect options']),
        CO  = val(ErrRef, inet:connect_options(Opts2, Mod)),
	%% ?DBG(['process sockaddrs']),
	SAs = sockaddrs(IPs, TP, Domain),
	%% ?DBG([{sas, SAs}, {co, CO}]),
        {SAs, CO}
    of
        {Addrs,
         #connect_opts{fd     = Fd,
                       ifaddr = BindAddr,
                       port   = BindPort,
                       opts   = ConnectOpts}} ->
            %%
            %% ?DBG([{domain, Domain}, {bind_ip, BindAddr}]),
            BindSockaddr = bind_addr(Domain, BindAddr, BindPort),
            OpenOpts = open_opts(OpenOpts0, open_opts(Fd)),
            connect_open(
              Addrs, Domain, ConnectOpts, StartOpts, OpenOpts,
              Timer, BindSockaddr)
    catch
        throw : {ErrRef, Reason} ->
            ?badarg_exit({error, Reason})
    end.

connect_open(Addrs, Domain,
             ConnectOpts, StartOpts0, OpenOpts,
             Timer, BindAddr) ->
    %%
    %% The {netns, File} option is passed in Fd by inet:connect_options/2,
    %% and then over to ExtraOpts.
    %%
    StartOpts = [{timeout, inet:timeout(Timer)} | StartOpts0],
    case start_server(Domain, StartOpts, OpenOpts) of
        {ok, Server} ->
	    %% ?DBG(['server started', {server, Server}]),
            ErrRef = make_ref(),
            try
                try_setopts(ErrRef, Server, StartOpts, ConnectOpts),
                try_bind(ErrRef, Server, Domain, BindAddr, OpenOpts),
                Socket = try_connect(ErrRef, Server, Addrs, Timer),
                MSock  = ?MODULE_socket(Server, Socket),
                %% ?DBG(['done', {msock, MSock}]),
                {ok, MSock}

            catch
                throw : {ErrRef, Reason} ->
		    %% ?DBG([{reason, Reason}]),
                    close_server(Server),
                    ?badarg_exit({error, Reason})
            end;
        {error, _Reason} = Error ->
	    %% ?DBG(['server start failed', {reason, _Reason}]),
            ?badarg_exit(Error)
    end.

try_connect(ErrRef, Server, Addrs, Timer) ->
    DefaultError = {error, einval},
    val(ErrRef, connect_loop(Addrs, Server, DefaultError, Timer)).

connect_loop([], _Server, Error, _Timer) ->
    %% ?DBG(['done', {error, Error}]),
    Error;
connect_loop([Addr | Addrs], Server, _Error, Timer) ->
    Result = call(Server, {connect, Addr, inet:timeout(Timer)}),
    case Result of
        {ok, _Socket}   -> Result;
        {error, Reason} ->
            case Reason of
                badarg  -> Result;
                einval  -> Result;
                timeout -> Result;
                closed  -> Result;
                _ ->
                    %% ?DBG([{addr, Addr}, {result, Result}]),
                    connect_loop(Addrs, Server, Result, Timer)
            end
    end.


open_opts(Fd) when is_integer(Fd) ->
    if
        Fd < 0 ->
            #{};
        true ->
            #{fd => Fd}
    end;
open_opts(OpenOpts) when is_list(OpenOpts) ->
    %% This is an **ugly** hack.
    %% inet:{connect,listen,udp,sctp}_options/2 has the bad taste
    %% to use this for [{netns,BinNS}] if that option is used...
   maps:from_list(OpenOpts).

%% Should we verify the options or just accept them?
open_opts([], OpenOpts)
  when is_map(OpenOpts) ->
    OpenOpts;
open_opts([{Opt, Val}|Opts], OpenOpts)
  when is_list(Opts) andalso is_map(OpenOpts) ->
    open_opts(Opts, OpenOpts#{Opt => Val}).


default_any(_Domain, undefined, #{fd := _}) ->
    undefined;
default_any(Domain, undefined, _Opts) ->
    if
        Domain =:= inet;
        Domain =:= inet6 ->
            #{family => Domain,
              addr   => any,
              port   => 0};
        true ->
            undefined
    end;
default_any(_Domain, BindAddr, _Opts) ->
    BindAddr.

bind_addr(Domain, #{family := Domain} = BindSockaddr, _BindPort) ->
    BindSockaddr;
bind_addr(Domain, BindIP, BindPort)
  when ((BindIP =:= undefined) andalso (BindPort =:= 0)) ->
    %% *Maybe* Do not bind! On Windows we actually need to bind
    %% ?DBG([{bind_ip, BindIP}, {bind_port, BindPort}, {fd, Fd}]),
    case os:type() of
        {win32, nt} ->
            Addr = which_bind_address(Domain, BindIP),
            #{family => Domain,
              addr   => Addr,
              port   => BindPort};
        _ ->
            undefined
    end;
bind_addr(local = Domain, BindIP, _BindPort) ->
    case BindIP of
	any ->
	    undefined;
	{local, Path} ->
	    #{family => Domain,
	      path   => Path}
    end;
bind_addr(Domain, BindIP, BindPort)
  when (Domain =:= inet) orelse (Domain =:= inet6) ->
    %% ?DBG([{domain, Domain}, {bind_ip, BindIP}, {bind_port, BindPort}]),
    Addr = which_bind_address(Domain, BindIP),
    #{family => Domain,
      addr   => Addr,
      port   => BindPort}.

which_bind_address(Domain, BindIP) when (BindIP =:= undefined) ->
    which_default_bind_address(Domain);
which_bind_address(_Domain, BindIP) ->
    %% We should really check if its any here,
    %% since that will not work on Windows...
    BindIP.

which_default_bind_address(Domain) ->
    case os:type() of
        {win32, nt} ->
            %% Binding to 'any' causes "issues" on Windows:
            %% The socket is actually auto-bound when first *sending*,
            %% so since the server process start *reading* directly,
            %% that (reading) fails.
            %% Therefor pick a "proper" address...
            which_default_bind_address2(Domain);
        _ ->
            any
    end.

which_default_bind_address2(Domain) ->
    %% ?DBG([{domain, Domain}]),
    case net_getifaddrs(Domain) of
        {ok, Addrs} ->
            %% ?DBG([{addrs, Addrs}]),
            %% Pick first *non-loopback* interface that is 'up'
            UpNonLoopbackAddrs =
                [Addr ||
                    #{flags := Flags} = Addr <-
                        Addrs,
                    (not lists:member(loopback, Flags)) andalso
                        lists:member(up, Flags)],
            %% ?DBG([{up_non_loopback_addrs, UpNonLoopbackAddrs}]),
            case UpNonLoopbackAddrs of
                [#{addr := #{addr := Addr}} | _] ->
                    Addr;
                _ ->
                    any % better than nothing
            end;
        {error, _} ->
            any % better than nothing
    end.

net_getifaddrs(local = _Domain) ->
    net:getifaddrs(#{family => local, flags => any});
net_getifaddrs(Domain) ->
    net:getifaddrs(Domain).

call_bind(_Server, undefined) ->
    ok;
call_bind(Server, BindAddr) ->
    %% ?DBG([{bind_addr, BindAddr}]),
    call(Server, {bind, BindAddr}).


default_active_true(Opts) ->
    case lists:keyfind(active, 1, Opts) of
        {active,_} ->
            Opts;
        _ ->
            [{active,true} | Opts]
    end.


%% -------------------------------------------------------------------------

listen(Port, Opts0) ->
    %% ?DBG([{port, Port}, {opts0, Opts0}]),
    Opts1              = internalize_setopts(Opts0),
    %% ?DBG([{opts1, Opts1}]), 
    {Mod, Opts2}       = inet:tcp_module(Opts1),
    %% ?DBG([{mod, Mod}, {opts2, Opts2}]),
    {StartOpts, Opts3} = split_start_opts(Opts2),
    {OpenOpts0, Opts4} = split_open_opts(Opts3),
    %% ?DBG([{start_opts, StartOpts},
    %%       {open_opts0, OpenOpts0},
    %%       {opts4,      Opts4}]),
    case Mod:getserv(Port) of
        {ok, TP} ->
            %% ?DBG([{tp, TP}]),
            case inet:listen_options([{port, TP} | Opts4], Mod) of
                {error, badarg} ->
                    exit(badarg);
                {ok,
                 #listen_opts{fd      = Fd,
                              ifaddr  = BindAddr,
                              port    = BindPort,
                              opts    = ListenOpts,
                              backlog = Backlog}} ->
                    %%
                    Domain    = domain(Mod),
                    %% ?DBG([{domain, Domain}, {bind_ip, BindAddr},
                    %%       {listen_opts, ListenOpts}, {backlog, Backlog}]),
                    BindSockaddr  = bind_addr(Domain, BindAddr, BindPort),
                    %% ?DBG([{bind_sock_addr, BindSockaddr}]),
                    OpenOpts = open_opts(OpenOpts0, open_opts(Fd)),
                    %% ?DBG([{open_opts, OpenOpts}]),
                    listen_open(
                      Domain, ListenOpts, StartOpts, OpenOpts,
                      Backlog, BindSockaddr)
            end;
        {error, _} = Error ->
            ?badarg_exit(Error)
    end.


%% Helpers -------

listen_open(Domain, ListenOpts, StartOpts0, OpenOpts, BackLog, BindAddr) ->
    %% ?DBG(['start server',
    %%       {listen_opts,  ListenOpts},
    %%       {start_opts0,  StartOpts0},
    %%       {open_opts,    OpenOpts}]),
    StartOpts = [{timeout, infinity} | StartOpts0],
    case start_server(Domain, StartOpts, OpenOpts) of
        {ok, Server} ->
            %% ?DBG([{server, Server}]),
            ErrRef = make_ref(),
            try
                case os:type() of
                    {win32, nt} ->
                        %% On *Windows*
                        %% we need to bind before everything else...
                        try_bind(ErrRef, Server, Domain, BindAddr, OpenOpts),
                        try_setopts(ErrRef, Server, StartOpts, ListenOpts),
                        Socket = try_listen(ErrRef, Server, BackLog),
                        MSock  = ?MODULE_socket(Server, Socket),
                        %% ?DBG(['done', {msock, MSock}]),
                        {ok, MSock};

                    _ ->
                        try_setopts(ErrRef, Server, StartOpts, ListenOpts),
                        try_bind(ErrRef, Server, Domain, BindAddr, OpenOpts),
                        Socket = try_listen(ErrRef, Server, BackLog),
                        MSock  = ?MODULE_socket(Server, Socket),
                        %% ?DBG(['done', {msock, MSock}]),
                        {ok, MSock}
                end
            catch
                throw : {ErrRef, Reason} ->
                    %% ?DBG(['failure', {reason, Reason}]),
                    close_server(Server),
                    ?badarg_exit({error, Reason})
            end;
        {error, {shutdown, Reason}} ->
            %% ?DBG(['shutdown', {reason, Reason}]),
            ?badarg_exit({error, Reason});
        {error, _} = Error ->
            %% ?DBG(['other error', {error, Error}]),
            ?badarg_exit(Error)
    end.


try_bind(ErrRef, Server, Domain, BindAddr0, OpenOpts) ->
    %% ?DBG(['process bind-address',
    %%       {domain,     Domain},
    %%       {bind_addr0, BindAddr0},
    %%       {open_opts,  OpenOpts}]),
    BindAddr1 = default_any(Domain, BindAddr0, OpenOpts),
    %% ?DBG(['try bind', {bind_addr1, BindAddr1}]),
    ok(ErrRef, call_bind(Server, BindAddr1)).

try_setopts(ErrRef, Server, StartOpts, OperationOpts) ->
    %% ?DBG(['process options',
    %%       {start_opts,     StartOpts},
    %%       {operation_opts, OperationOpts}]),
    SetOpts = default_active_true([{start_opts, StartOpts} |
                                   setopts_opts(ErrRef, OperationOpts)]),
    %% ?DBG(['try setopts', {set_opts, SetOpts}]),
    ok(ErrRef, call(Server, {setopts, SetOpts})).

try_listen(ErrRef, Server, BackLog) ->
    %% ?DBG(['try listen', {backlog, BackLog}]),
    val(ErrRef, call(Server, {listen, BackLog})).


%% -------------------------------------------------------------------------

accept(?MODULE_socket(ListenServer, ListenSocket), Timeout) ->
    %%
    Timer = inet:start_timer(Timeout),
    ErrRef = make_ref(),
    try
        #{start_opts := StartOpts} = ServerData =
            val(ErrRef, call(ListenServer, get_server_opts)),
        Server =
            val(ErrRef,
                start_server(
                 ServerData, [{timeout, inet:timeout(Timer)} | StartOpts])),
        Socket =
            val({ErrRef, Server},
                call(Server, {accept, ListenSocket, inet:timeout(Timer)})),
        {ok, ?MODULE_socket(Server, Socket)}
    catch
        throw : {{ErrRef, Srv}, Reason} ->
            stop_server(Srv),
            ?badarg_exit({error, Reason});
        throw : {ErrRef, Reason} ->
            ?badarg_exit({error, Reason})
    after
        _ = inet:stop_timer(Timer)
    end.

%% -------------------------------------------------------------------------

send(?MODULE_socket(Server, Socket), Data) ->
    case socket:getopt(Socket, {otp,meta}) of
        {ok,
         #{packet       := Packet,
           send_timeout := SendTimeout} = Meta} ->
            if
                Packet =:= 1;
                Packet =:= 2;
                Packet =:= 4 ->
                    Data2       = iolist_to_binary(Data),
                    Size        = byte_size(Data2),
		    %% ?DBG([{packet, Packet}, {data_size, Size}]),
                    Header      = <<?header(Packet, Size)>>,
                    Header_Data = [Header, Data2],
                    Result      = socket_sendv(Socket,
                                               Header_Data, SendTimeout),
                    send_result(Server, Header_Data, Meta, Result);
                true ->
                    Result = socket_send(Socket, Data, SendTimeout),
                    send_result(Server, Data, Meta, Result)
            end;
        {ok, _BadMeta} ->
            exit(badarg);
        {error, _} = Error ->
            Error
    end.
%%
send_result(Server, Data, Meta, Result) ->
    %% ?DBG([{meta, Meta}, {send_result, Result}]),
    case Result of
        {error, {timeout, RestData}} when is_binary(RestData) orelse
                                          is_list(RestData) ->
            send_timeout(Server, {false, RestData}, Meta);
        {error, timeout} ->
            send_timeout(Server, {true, Data}, Meta);
        {error, {Reason, RestData}} when is_binary(RestData) orelse
                                         is_list(RestData) ->
            call(Server, {send_error, Reason});
        {error, Reason} ->
            call(Server, {send_error, Reason});
        ok ->
            ok
    end.

send_timeout(Server, {ToBin, Data}, Meta) ->
    case maps:get(send_timeout_close, Meta) of
        true ->
            %% To handle RestData we would have to pass
            %% all writes through a single process that buffers
            %% the write data, which would be a bottleneck.
            %%
            %% We have to waste the lingering data...
            %%
            close_server(Server),
            {error, timeout};
        false when (ToBin =:= true) ->
            %% Leave the lingering data to the caller to handle
            {error, {timeout, iolist_to_binary(Data)}};
        false ->
            %% Leave the lingering data to the caller to handle
            {error, {timeout, Data}}
    end.

%% -------------------------------------------------------------------------
%% Handler called by file:sendfile/5 to handle ?MODULE_socket()s
%% as a sibling of prim_file:sendfile/8

sendfile(
  ?MODULE_socket(_Server, Socket),
  FileHandle, Offset, Count) ->
    %%
    case socket:getopt(Socket, {otp,meta}) of
        {ok, #{packet := _}} ->
            try
                %% XXX should we do cork/uncork here, like in prim_inet?
                %%     And, maybe file:advise too, like prim_file
                socket:sendfile(Socket, FileHandle, Offset, Count, infinity)
            catch
                Class : Reason : Stacktrace
                  when Class =:= error, Reason =:= badarg ->
                    %% Convert badarg exception into return value
                    %% to look like file:sendfile
                    case Stacktrace of
                        [{socket, sendfile, Args, _} | _]
                          when Args =:= 5;                        % Arity 5
                               tl(tl(tl(tl(tl(Args))))) =:= [] -> % Arity 5
                            {Class, Reason};
                        _ ->
                            erlang:raise(Class, Reason, Stacktrace)
                    end;
                Class : notsup when Class =:= error ->
                    {Class, enotsup}
            end;
        {ok, _BadMeta} ->
            {error, badarg};
        {error, _} = Error ->
            Error
    end.

%% -------------------------------------------------------------------------

recv(?MODULE_socket(Server, _Socket), Length, Timeout)
  when is_integer(Length), 0 =< Length, is_integer(Timeout), 0 =< Timeout;
       is_integer(Length), 0 =< Length, Timeout =:= infinity ->
    ?badarg_exit(call(Server, {recv, Length, Timeout})).

%% -------------------------------------------------------------------------

shutdown(?MODULE_socket(_Server, Socket), How) ->
    socket:shutdown(Socket, How).

%% -------------------------------------------------------------------------

close(?MODULE_socket(Server, _Socket)) ->
    close_server(Server).

%% Helpers -------

close_server(Server) ->
    case call(Server, {close, self()}) of
        {error, einval} -> ok;
        {error, closed} -> ok;
        ok              -> ok
    end,
    stop_server(Server),
    ok.

%% -------------------------------------------------------------------------

controlling_process(?MODULE_socket(Server, _Socket) = S, NewOwner)
  when is_pid(NewOwner) ->
    case call(Server, {controlling_process, NewOwner}) of
        ok -> ok;
        transfer -> controlling_process(S, NewOwner, Server);
        {error, _} = Error -> Error
    end.
%%
%% Helpers -------
%%
%% Transfer all queued socket messages to new owner
controlling_process(S, NewOwner, Server) ->
    receive
        {tcp, S, _Data} = Msg ->
            controlling_process(S, NewOwner, Server, Msg);
        {tcp_closed, S} = Msg ->
            controlling_process(S, NewOwner, Server, Msg);
        {S, {data, _Data}} = Msg ->
            controlling_process(S, NewOwner, Server, Msg)
    after 0 ->
            call(Server, controlling_process)
    end.
%% Loop
controlling_process(S, NewOwner, Server, Msg) ->
    NewOwner ! Msg,
    controlling_process(S, NewOwner, Server).


%% -------------------------------------------------------------------------
%% Module inet backends
%% -------------------------------------------------------------------------

monitor(?MODULE_socket(_Server, ESock) = Socket) ->
    %% The socket that is part of the down message:
    case socket_registry:monitor(ESock, #{msocket => Socket}) of
	{error, Reason} ->
	    erlang:error({invalid, Reason});
	MRef when is_reference(MRef) ->
	    MRef
    end;
monitor(Socket) ->
    erlang:error(badarg, [Socket]).

cancel_monitor(MRef) when is_reference(MRef) ->
    socket:cancel_monitor(MRef);
cancel_monitor(MRef) ->
    erlang:error(badarg, [MRef]).


%% -------------------------------------------------------------------------

%% Undocumented option for debugging the state machine
%% that mustn't be counted on
setopts(?MODULE_socket(Server, _Socket), [{sys_trace,T}]) ->
    sys:trace(Server, T);
%%
setopts(?MODULE_socket(Server, _Socket), [{active,Active}]) ->
    %% Optimized implementation
    call(Server, {setopt_active,Active});
setopts(?MODULE_socket(Server, _Socket), Opts) when is_list(Opts) ->
    try internalize_setopts(Opts) of
        Opts_I ->
            call(Server, {setopts, Opts_I})
    catch
        exit:badarg ->
            {error, einval}
    end.


%% -------------------------------------------------------------------------

getopts(?MODULE_socket(Server, _Socket), Opts) when is_list(Opts) ->
    try internalize_getopts(Opts) of
        Opts_I ->
            call(Server, {getopts, Opts_I})
    catch
        exit:badarg ->
            {error, einval}
    end.


%% -------------------------------------------------------------------------

sockname(?MODULE_socket(_Server, Socket)) ->
    case socket:sockname(Socket) of
        {ok, SockAddr} -> {ok, address(SockAddr)};
        {error, _} = Error -> Error
    end.


%% -------------------------------------------------------------------------

socknames(Socket) ->
    case sockname(Socket) of
        {ok, Addr} -> {ok, [Addr]};
        {error, _} = Error -> Error
    end.


%% -------------------------------------------------------------------------

peername(?MODULE_socket(_Server, Socket)) ->
    case socket:peername(Socket) of
        {ok, SockAddr} -> {ok, address(SockAddr)};
        {error, _} = Error -> Error
    end.

%% -------------------------------------------------------------------------

getstat(?MODULE_socket(Server, _Socket), What) when is_list(What) ->
    call(Server, {getstat, What}).


%% -------------------------------------------------------------------------

info(?MODULE_socket(Server, _Socket)) ->
    case call(Server, {info, inet:stats()}) of
	{error, closed} ->
	    ?CLOSED_SOCKET;
	{error, einval} ->
	    ?CLOSED_SOCKET;
	Other ->
	    Other
    end.


%% -------------------------------------------------------------------------

socket_to_list(?MODULE_socket(_Server, Socket)) ->
    "#Socket" ++ Id = socket:to_list(Socket),
    "#InetSocket" ++ Id;
socket_to_list(Socket) ->
    erlang:error(badarg, [Socket]).


which_sockets() ->
    which_sockets(socket:which_sockets(tcp)).

which_sockets(Socks) ->
    which_sockets(Socks, []).

which_sockets([], Acc) ->
    Acc;
which_sockets([Sock|Socks], Acc) ->
    case socket:getopt(Sock, {otp, meta}) of
	{ok, undefined} ->
	    which_sockets(Socks, Acc);
	{ok, _Meta} ->
	    %% One of ours - try to recreate the compat socket
	    %% Currently we don't have the 'owner' in meta, so we need to look
	    %% it up...
	    #{owner := Owner} = socket:info(Sock),
	    MSock = ?MODULE_socket(Owner, Sock),
	    which_sockets(Socks, [MSock|Acc]);
	_ ->
	    which_sockets(Socks, Acc)
    end.


%% -------------------------------------------------------------------------

which_packet_type(?MODULE_socket(_Server, Socket)) ->
    %% quick and dirty...
    case socket:getopt(Socket, {otp, meta}) of
	{ok, #{packet := Type}} ->
	    {ok, Type};
	_ ->
	    error
    end.


%% -------------------------------------------------------------------------
%% Undocumented or unsupported
%% -------------------------------------------------------------------------

unrecv(?MODULE_socket(_Server, _Socket), _Data) ->
    {error, enotsup}.

fdopen(Fd, Opts0) when is_integer(Fd), 0 =< Fd, is_list(Opts0) ->
    Opts1        = internalize_setopts(Opts0),
    {Mod, Opts2} = inet:tcp_module(Opts1),
    Domain = domain(Mod),
    {StartOpts0, Opts3} = split_start_opts(Opts2),
    {Opts4, OpenOpts0}  = split_open_opts(Opts3),
    OpenOpts            = open_opts(OpenOpts0, open_opts(Fd)),
    StartOpts           = [{timeout, infinity} | StartOpts0],
    case start_server(Domain, StartOpts, OpenOpts) of
        {ok, Server} ->
            ErrRef = make_ref(),
            try
                Setopts =
                    [{start_opts, StartOpts} | setopts_opts(ErrRef, Opts4)],
                ok(ErrRef, call(Server, {setopts, Setopts})),
                Socket = val(ErrRef, call(Server, fdopen)),
                {ok, ?MODULE_socket(Server, Socket)}
            catch
                throw : {ErrRef, Reason} ->
                    close_server(Server),
                    ?badarg_exit({error, Reason})
            end;
        {error, {shutdown, Reason}} ->
            ?badarg_exit({error, Reason});
        {error, _} = Error ->
            ?badarg_exit(Error)
    end.


%%% ========================================================================
%%% Socket glue code
%%%

-compile({inline, [socket_send/3]}).
socket_send(Socket, Data, Timeout) ->
    Result = socket:send(Socket, Data, [], Timeout),
    case Result of
        {error, {Reason, RestData}} when is_binary(RestData) ->
            {error, NewReason} = socket_send_error({error, Reason}),
            {error, {NewReason, RestData}};
        {error, _} ->
            socket_send_error(Result);
        _ ->
            Result
    end.

-compile({inline, [socket_sendv/3]}).
socket_sendv(Socket, Data, Timeout) ->
    Result = socket:sendv(Socket, Data, Timeout),
    case Result of
        {error, {Reason, RestData}} when is_list(RestData) ->
        {error, NewReason} = socket_send_error({error, Reason}),
        {error, {NewReason, RestData}};
    {error, _} ->
        socket_send_error(Result);
    _ ->
        Result
end.

-compile({inline, [socket_send_error/1]}).
socket_send_error({error, Reason}) ->
    {error, socket_send_reason(Reason)}.

-compile({inline, [socket_send_reason/1]}).
socket_send_reason({completion_status, CS}) ->
    socket_send_reason(CS);
socket_send_reason(#{info := Info}) ->
    socket_send_reason(Info);
socket_send_reason(epipe) ->
    econnreset;
socket_send_reason(netname_deleted) ->
    econnreset;
socket_send_reason(too_many_cmds) ->
    closed;
socket_send_reason(Reason) ->
    Reason.



-compile({inline, [socket_recv/2]}).
socket_recv(Socket, Length) ->
    Result = socket:recv(Socket, Length, [], nowait),
    %% ?DBG({Socket, Length, Result}),
    Result.

-compile({inline, [socket_close/1]}).
socket_close(Socket) ->
    %% XXX Should we set the meta option to closed here,
    %% for the send operation to detect closed without calling
    %% the NIF???
    case socket:close(Socket) of
        ok -> ok;
        {error, closed} -> ok
    end.

-compile({inline, [socket_cancel/2]}).
socket_cancel(Socket, SelectInfo) ->
    case socket:cancel(Socket, SelectInfo) of
        ok          -> ok;
        {error, _}  -> ok
    end.

%%% ========================================================================
%%% API Helpers
%%%

%% Deep return helpers

ok(_ErrRef, ok) -> ok;
ok(ErrRef, {error, Reason}) -> throw({ErrRef, Reason}).

val(_ErrRef, {ok, Val}) -> Val;
val(ErrRef, {error, Reason}) -> throw({ErrRef, Reason}).


address(SockAddr) ->
    case SockAddr of
        #{family := Family, addr := IP, port := Port}
          when Family =:= inet;
               Family =:= inet6 ->
            {IP, Port};
        #{family := local, path := Path} ->
            {local, Path}
    end.

-ifdef(undefined).
chain([F | Fs], Fail) ->
    chain(Fs, Fail, [], F()).
%%
chain([F | Fs], Fail, Values) ->
    chain(Fs, Fail, Values, F(Values)).
%%
chain([], _Fail, _Values, Ret) -> Ret;
chain(Fs, Fail, Values, Ret) ->
    case Ret of
        {error, _} -> Fail(Ret);
        ok -> chain(Fs, Fail, Values);
        {ok, Value} -> chain(Fs, Fail, [Value | Values])
    end.
-endif. % -ifdef(undefined).

%% -------------------------------------------------------------------------

-compile({inline, [domain/1]}).
domain(Mod) ->
    case Mod of
        inet_tcp  -> inet;
        inet6_tcp -> inet6;
        local_tcp -> local
    end.

%% -------------------------------------------------------------------------

sockaddrs([], _TP, _Domain) -> [];
sockaddrs([{local, Path} | IPs], TP, Domain) when (Domain =:= local) ->
    [#{family => Domain, path => Path}
     | sockaddrs(IPs, TP, Domain)];
sockaddrs([IP | IPs], TP, Domain) ->
    [#{family => Domain, addr => IP, port => TP}
     | sockaddrs(IPs, TP, Domain)].

%% -------------------------------------------------------------------------
%% Make all options 2-tuple options.
%% Convert special options i.e {raw, Level, Key, Value}.
%% Pass through 2-tuple options with atom tag.
%% Reject all other terms by exit(badarg).
%%

internalize_setopts(Opts) ->
    [case Opt of
         binary                     -> {mode, binary};
         list                       -> {mode, list};
         inet                       -> {tcp_module, inet_tcp};
         inet6                      -> {tcp_module, inet6_tcp};
         local                      -> {tcp_module, local_tcp};
         {Tag, _} when is_atom(Tag) -> Opt;
         {raw, Level, Key, Value}   -> {raw, {Level, Key, Value}};
         _ ->
             %% ?DBG([{opt, Opt}]),
             exit(badarg)
     end || Opt <- Opts].

internalize_getopts(Opts) ->
    [case Opt of
         Tag when is_atom(Tag)        -> Opt;
         {raw, _}                     -> Opt;
         {raw, Level, Key, ValueSpec} -> {raw, {Level, Key, ValueSpec}};
         _                            -> %% ?DBG([{opt, Opt}]),
                                         exit(badarg)
     end || Opt <- Opts].

externalize_getopts(Opts) ->
    [case Opt of
         {raw, {Level, Key, Value}} -> {raw, Level, Key, Value};
         {Tag, _} when is_atom(Tag) -> Opt;
         _                          -> %% ?DBG([{opt, Opt}]),
                                       exit(badarg)
     end || Opt <- Opts].
 
%%
%% -------
%% Split options into server start options and other options.
%% Convert our {sys_debug, _} option into {debug, _} (the
%% sys_debug option is how to pass a debug option to
%% gen_statem:start/3).  A {debug,Val} option is
%% on the other hand a socket option and is later,
%% through socket_opts(),  transformed into the module
%% 'socket' option {{otp,debug}, Val}.
%%

split_start_opts(Opts) ->
    {StartOpts,
     NonStartOpts} =
        lists:partition(
          fun ({sys_debug, _}) -> true;
              (_)              -> false
          end, Opts),
    {[case Opt of
          {sys_debug, Val} -> {debug, Val};
          _                -> Opt
      end || Opt <- StartOpts],
     NonStartOpts}.


%% No need to (at this point) do something fancy here,
%% since we really only got one option we need to pick out; the debug
%% option.
split_open_opts(Opts) ->
    split_open_opts(Opts, [], []).

split_open_opts([], OpenOpts, OtherOpts) ->
    {lists:reverse(OpenOpts), lists:reverse(OtherOpts)};
split_open_opts([{debug, _} = Opt|Opts], OpenOpts, OtherOpts) ->
    split_open_opts(Opts, [Opt|OpenOpts], OtherOpts);
split_open_opts([Opt|Opts], OpenOpts, OtherOpts) ->
    split_open_opts(Opts, OpenOpts, [Opt|OtherOpts]).


%%
%% -------
%% Verify that all options can be set with setopts/2 after
%% opening the socket.  They should be known socket options,
%% options handled by the server, or options we should ignore.
%% filter out the ignored options and fail for unknown options
%% by throwing {ErrRef, badarg}.
%%
setopts_opts(ErrRef, Opts) ->
    %% ?DBG([{opts, Opts}]),
    SocketOpts = socket_opts(),
    %% ?DBG([{socket_opts, SocketOpts}]),
    ServerOpts = server_opts(),
    %% ?DBG([{server_opts, ServerOpts}]),
    [Opt ||
        {Tag,_} = Opt <- Opts,
        if
            is_map_key(Tag, SocketOpts) -> true;
            is_map_key(Tag, ServerOpts) -> true;
            true ->
                %% ?DBG(['check ignore', {tag, Tag}]),
                case ignore_optname(Tag) of
                    true  -> false; % ignore -> filter out
                    false ->
                        throw({ErrRef, badarg})
                end
        end].



%% Socket options

socket_setopt(Socket, raw, Value) ->
    %% ?DBG([raw, {value, Value}]),
    case Value of
        {Level, Key, Val} ->
            try socket:setopt_native(Socket, {Level,Key}, Val) of
                Res ->
                    %% ?DBG([{res, Res}]),
                    Res
            catch
                throw:{invalid, _} ->
                    {error, einval}
            end;
        _ ->
            {error, einval}
    end;
socket_setopt(Socket, {Domain, _} = Opt, Value) when is_atom(Domain) ->
    %% ?DBG([{opt, Opt}, {value, Value}]),
    %% socket:setopt(Socket, otp, debug, true),
    Res = socket:setopt(Socket, Opt, socket_setopt_value(Opt, Value)),
    %% socket:setopt(Socket, otp, debug, false),
    Res;
socket_setopt(Socket, DomainProps, Value) when is_list(DomainProps) ->
    %% ?DBG([{domain_props, DomainProps}, {value, Value}]),
    %% We need to lookup the domain of the socket,
    %% so we can select which one to use.
    %% ?DBG(Opt0),
    case socket:getopt(Socket, otp, domain) of
        {ok, Domain} ->
            case lists:keysearch(Domain, 1, DomainProps) of
                {value, {Domain, Opt}} ->
                    %% _ = socket:setopt(Socket, otp, debug, true),
                    Res =
                        socket:setopt(
                          Socket, Opt,
                          socket_setopt_value(Opt, Value)),
                    %% _ = socket:setopt(Socket, otp, debug, false),
                    Res;
                false ->
                    {error, einval}
            end;
        {error, _} ->
            {error, einval}
    end.

socket_setopt_value({socket,linger}, {OnOff, Linger}) ->
    #{onoff => OnOff, linger => Linger};
socket_setopt_value({socket,bindtodevice}, DeviceBin)
  when is_binary(DeviceBin) ->
    %% Currently: 
    %% prim_inet: Require that device is a binary()
    %% socket:    Require that device is a string()
    binary_to_list(DeviceBin);
socket_setopt_value(_Opt, Value) -> Value.


socket_getopt(Socket, raw, Val) ->
    %% ?DBG([raw, {val, Val}]),
    case Val of
        {Level, Key, ValueSpec} ->
            case socket:getopt_native(Socket, {Level,Key}, ValueSpec) of
                {ok, Value} ->
                    {ok, {Level, Key, Value}};
                {error, {invalid, _} = _Reason} ->
                    %% ?DBG([{reason, _Reason}]),
                    {error, einval};
                {error, _Reason} = ERROR ->
                    %% ?DBG([{reason, _Reason}]),
                    ERROR
            end;
        _ ->
            %% ?DBG(bad_raw_value),
            {error, einval}
    end;
socket_getopt(Socket, {Domain, _} = Opt, _) when is_atom(Domain) ->
    %% ?DBG([{opt, Opt}]),
    %% _ = socket:setopt(Socket, otp, debug, true),
    Res = socket:getopt(Socket, Opt),
    %% ?DBG([{res, Res}]),
    %% _ = socket:setopt(Socket, otp, debug, false),
    socket_getopt_value(Opt, Res);
socket_getopt(Socket, DomainProps, _) when is_list(DomainProps) ->
    %% ?DBG([{domain_props, DomainProps}]),
    %% We need to lookup the domain of the socket,
    %% so we can select which one to use.
    case socket:getopt(Socket, otp, domain) of
        {ok, Domain} ->
            %% ?DBG({'socket_getopt - domain', Tag, Domain}),
            case lists:keysearch(Domain, 1, DomainProps) of
                {value, {Domain, Opt}} ->
                    %% ?DBG([{domain, Domain}, {opt, Opt}]),
                    %% _ = socket:setopt(Socket, otp, debug, true),
                    Res = socket:getopt(Socket, Opt),
                    %% _ = socket:setopt(Socket, otp, debug, false),
                    %% ?DBG([{result, Res}]),
                    socket_getopt_value(Opt, Res);
                false ->
                    %% ?DBG(no_domain),
                    {error, einval}
            end;
        {error, _DReason} ->
            %% ?DBG(no_domain),
            {error, einval}
    end.

socket_getopt_value(
  {socket,linger}, {ok, #{onoff := OnOff, linger := Linger}}) ->
    {ok, {OnOff, Linger}};
socket_getopt_value({Level,pktoptions}, {ok, PktOpts})
  when Level =:= ip,   is_list(PktOpts);
       Level =:= ipv6, is_list(PktOpts) ->
    {ok, [{Type, Value} || #{type := Type, value := Value} <- PktOpts]};
socket_getopt_value(_Tag, {ok, _Value} = Ok) -> Ok;
socket_getopt_value(_Tag, {error, _} = Error) -> Error.


socket_copy_opt(Socket, Tag, TargetSocket) when is_atom(Tag) ->
    case socket_opts() of
        #{Tag := {_Level,_Key} = Opt} ->
	    case socket:is_supported(options, Opt) of
		true ->
		    case socket:getopt(Socket, Opt) of
			{ok, Value} ->
			    socket:setopt(TargetSocket, Opt, Value);
			{error, _Reason} = Error ->
			    Error
		    end;
		false ->
		    ok
	    end;
        #{} = _X ->
	    {error, einval}
    end.


-compile({inline, [ignore_optname/1]}).
ignore_optname(Tag) ->
    case Tag of
        %% Handled by inet:tcp_module/2
        tcp_module -> true;
        %% Handled by inet:connect_options/2 and inet:listen_options/2
        ip      -> true;
        backlog -> true;
        %% XXX Some of these must probably be handled one day...
        high_msgq_watermark -> true;
        high_watermark      -> true;
        low_msgq_watermark  -> true;
        low_watermark       -> true;
        nopush              ->
            case nopush_or_cork() of
                undefined ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% 'socket' options; translation to 'level' and 'opt'
%%
-compile({inline, [socket_opts/0]}).
socket_opts() ->
    Opts =
        #{
          %% Level: otp
          buffer => {otp, rcvbuf},
          debug  => {otp, debug},
          fd     => {otp, fd},

          %%
          %% Level: socket
          bind_to_device   => {socket, bindtodevice},
          dontroute        => {socket, dontroute},
          exclusiveaddruse => {socket, exclusiveaddruse},
          keepalive        => {socket, keepalive},
          linger           => {socket, linger},
          priority         => {socket, priority},
          recbuf           => {socket, rcvbuf},
          reuseaddr        => {socket, reuseaddr},
          sndbuf           => {socket, sndbuf},

          %%
          %% Level: tcp
          nodelay => {tcp, nodelay},

          %%
          %% Level: ip
          recvtos => {ip, recvtos},
          recvttl => {ip, recvttl},
          tos     => {ip, tos},
          ttl     => {ip, ttl},

          %%
          %% Level: ipv6
          recvtclass  => {ipv6, recvtclass},
          ipv6_v6only => {ipv6, v6only},
          tclass      => {ipv6, tclass},

          %%
          %% Raw
          raw => raw,

          %%
          %% Special cases
          %% These are options that cannot be mapped as above,
          %% as they, for instance, "belong to" several domains.
          %% So, we select which level to use based on the domain
          %% of the socket.

          %% This is a special case.
          %% Only supported on Linux and then only actually for IPv6,
          %% but unofficially also for ip...barf...
          %% In both cases this is *no longer valid* as the RFC which 
          %% introduced this, RFC 2292, is *obsoleted* by RFC 3542, where
          %% this "feature" *does not exist*...
          pktoptions  =>
              [{inet, {ip, pktoptions}}, {inet6, {ipv6, pktoptions}}]
         },
    case nopush_or_cork() of
        undefined ->
            %% Neither
            Opts;
        NopushOpt ->
            maps:put(nopush, {tcp, NopushOpt}, Opts)
    end.

-compile({inline, [nopush_or_cork/0]}).
nopush_or_cork() ->
    case os:type() of
        {unix, darwin} ->
            %% This option exist (on Darwin), but does something else!
            undefined;
        _ ->
            OptsSup = socket:supports(options),
            NoPushKey = {tcp, nopush},
            case lists:keysearch(NoPushKey, 1, OptsSup) of
                {value, {NoPushKey, true}} ->
                    nopush;
                _ ->
                    CorkKey = {tcp, cork},
                    case lists:keysearch(CorkKey, 1, OptsSup) of
                        {value, {CorkKey, true}} ->
                            cork;
                        _ ->
                            undefined
                    end
            end
    end.

%% -type packet_option_value() ::
%%         0 | 1 | 2 | 4 | raw | sunrm |  asn1 |
%%         cdr | fcgi | line | tpkt | http | httph | http_bin | httph_bin.

-compile({inline, [is_packet_option_value/1]}).
is_packet_option_value(Value) ->
    case Value of
        0 -> true; 1 -> true; 2 -> true; 4 -> true;
        raw -> true;
        sunrm -> true;
        asn1 -> true;
        cdr -> true;
        fcgi -> true;
        line -> true;
        tpkt -> true;
        http -> true;
        httph -> true;
        http_bin -> true;
        httph_bin -> true;
        _ -> false
    end.

-compile({inline, [server_read_write_opts/0]}).
server_read_write_opts() ->
    %% Common for read and write side
    #{packet          => raw,
      packet_size     => 16#4000000, % 64 MByte
      show_econnreset => false,
      exit_on_close   => true}.
-compile({inline, [server_read_opts/0]}).
server_read_opts() ->
    %% Read side only opts
    maps:merge(
      #{active => false, % inet_drv also has this default
        mode => list,
        header => 0,
        deliver => term,
        start_opts => [], % Just to make it settable
        line_delimiter => $\n,
        read_ahead => false},
      server_read_write_opts()).
-compile({inline, [server_write_opts/0]}).
server_write_opts() ->
    %% Write side only opts
    maps:merge(
      #{send_timeout => infinity,
        send_timeout_close => false,
        %% XXX not implemented yet
        delay_send => false},
      server_read_write_opts()).
%% Category 'server'
%%
%% Default values
-compile({inline, [server_opts/0]}).
server_opts() ->
    maps:merge(server_read_opts(), server_write_opts()).

-compile({inline, [meta/1]}).
meta(D) -> maps:with(maps:keys(server_write_opts()), D).

-compile({inline, [meta_opts/0]}).
meta_opts() -> maps:keys(server_write_opts()).


%%% ========================================================================
%%% State Machine
%%%

%% State Machine Engine Call Interface

%% Start for connect or listen - create a socket
%%    StartOpts - Options for gen_statem (start the gen_statem server)
%%    OpenOpts  - Options for socket:open
start_server(Domain, StartOpts, OpenOpts) ->
    %% ?DBG([{domain,     Domain},
    %%       {start_opts, StartOpts},
    %%       {open_opts,  OpenOpts}]),
    Owner = self(),
    Arg   = {open, Domain, OpenOpts, Owner},
    case gen_statem:start(?MODULE, Arg, StartOpts) of
        {ok, Server} ->
	    %% ?DBG([{server, Server}]),
	    {ok, Server};
        {error, _} = Error ->
	    %% ?DBG([{error, Error}]),
	    Error
    end.

%% Start for accept - have no socket yet
start_server(ServerData, StartOpts) ->
    %% ?DBG([{server_data, ServerData}, {start_opts, StartOpts}]),
    Owner = self(),
    Arg = {prepare, ServerData, Owner},
    case gen_statem:start(?MODULE, Arg, StartOpts) of
        {ok, Server} ->
	    %% ?DBG([{server, Server}]),
	    {ok, Server};
        {error, _} = Error ->
	    %% ?DBG([{error, Error}]),
	    Error
    end.

call(Server, Msg) ->
    try
        gen_statem:call(Server, Msg)
    catch
        exit:{noproc, {gen_statem, call, _Args}} ->
            {error, einval};
        exit:{{shutdown, _}, _} ->
            {error, closed};
        C:E:S ->
            error_msg("~w call failed: "
                      "~n      Msg:   ~p"
                      "~n      Class: ~p"
                      "~n      Error: ~p"
                      "~n      Stack: ~p",
                      [?MODULE, Msg, C, E, S]),
            erlang:raise(C, E, S)
    end.

stop_server(Server) ->
    try gen_statem:stop(Server, {shutdown, closed}, infinity) of
        _ -> ok
    catch
        _:_ -> ok
    end.

%% reply(From, Reply) ->
%%     gen_statem:reply(From, Reply).

%% -------------------------------------------------------------------------
%% Statem Machine Engine Callbacks

callback_mode() -> handle_event_function.



%% States:
%%

-record(controlling_process,
        {owner :: pid(),
         state :: term()}).
%% A super state that encapsulates any other state
%% and postpones all events but get_server_opts/0
%% and Owner 'DOWN'

%% 'accept'

-record(accept,
        {info :: socket:select_info() | socket:completion_info(),
         from :: gen_statem:from(),
         listen_socket :: socket:socket()}).
%% Socket is not created

-record(wrap_counters,
        {ref      :: reference(),
         call     :: {info | getstat, [inet:stat_option()]},
         state    :: term()}).

%% 'connect' % A listen socket stays here

-record(connect,
        {info :: socket:select_info() | socket:completion_info(),
         from :: gen_statem:from(),
         addr :: socket:sockaddr()}).

%% 'connected'

-record(recv,
        {info :: socket:select_info() | socket:completion_info()}).

%% 'closed'
%% Socket is closed; only happens for {call,_},close
%% right before terminate

%% 'closed_read'
%% Got closed from read on socket, no point in trying to read again



-record(params,
        {socket     :: undefined | socket:socket(),
         owner      :: pid(),
         owner_mon  :: reference()}).

server_vars() ->
    #{counters := #{num_cnt_bits := NumCntBits}} = socket:info(),
    %%
    #{type          => undefined,
      buffer        => <<>>,
      num_cnt_bits  => NumCntBits}.

init({open, Domain, ExtraOpts, Owner}) ->
    %% Listen or Connect
    %%
    process_flag(trap_exit, true),
    OwnerMon  = monitor(process, Owner),
    Extra = #{}, % #{debug => true},
    case socket_open(Domain, ExtraOpts, Extra) of
        {ok, Socket} ->
	    %% ?DBG(['open success', {socket, Socket}]),
            D  = maps:merge(server_opts(), server_vars()),
            ok = socket:setopt(Socket, {otp,iow}, true),
            %%
            %% meta(server_opts()) is an expensive way to write
            %% server_write_opts(), so, meta(D) is redundant code
            %% until someone decides to change D
            ok = socket:setopt(Socket, {otp,meta}, meta(D)),
            P = #params{
                   socket    = Socket,
                   owner     = Owner,
                   owner_mon = OwnerMon},
            {ok, connect, {P, D}};
        {error, Reason} ->
	    %% ?DBG(['open failed', {reason, Reason}]),
	    {stop, {shutdown, Reason}}
    end;
init({prepare, D, Owner}) ->
    %% Accept
    %%
    %% ?DBG([{init, prepare}, {d, D}, {owner, Owner}]),
    process_flag(trap_exit, true),
    OwnerMon = monitor(process, Owner),
    P = #params{
           owner     = Owner,
           owner_mon = OwnerMon},
    {ok, accept, {P, maps:merge(D, server_vars())}};
init(Arg) ->
    error_report([{badarg, {?MODULE, init, [Arg]}}]),
    error(badarg, [Arg]).

socket_open(Domain, #{fd := FD} = ExtraOpts, Extra) ->
    Opts =
        (maps:merge(Extra, maps:remove(fd, ExtraOpts)))
        #{dup      => false,
          domain   => Domain,
          type     => stream,
          protocol => proto(Domain)},
    %% ?DBG([{fd, FD}, {opts, Opts}]),
    socket:open(FD, Opts);
socket_open(Domain, ExtraOpts, Extra) ->
    Opts = maps:merge(Extra, ExtraOpts),
    %% ?DBG([{domain, Domain}, {extra_opts, ExtraOpts}, {extra, Extra}]),
    socket:open(Domain, stream, proto(Domain), Opts).

proto(Domain) ->
    case Domain of
        inet  -> tcp;
        inet6 -> tcp;
        _     -> default
    end.


terminate(_Reason, State, P_D) ->
    case State of
        #controlling_process{state = OldState} ->
            terminate(OldState, P_D);
        #wrap_counters{state = OldState} ->
            terminate(OldState, P_D);
        _ ->
            terminate(State, P_D)
    end.
%%
terminate(State, {P, D}) ->
    {next_state, 'closed', _P_D, Actions} =
        handle_close(P, D, State, undefined, []),
    [gen_statem:reply(Reply) || {reply, _From, _Msg} = Reply <- Actions],
    void.

%% -------------------------------------------------------------------------
%% Helpers

%% Construct a "socket" as in this module's API
module_socket(#params{socket = Socket}) ->
    ?MODULE_socket(self(), Socket).

%% -------------------------------------------------------------------------
%% Event Handler (callback)

handle_event(
  info, {'DOWN', OwnerMon, _, _, Reason}, _State,
  {#params{owner_mon = OwnerMon} = _P, _D} = P_D) ->
    %%
    {stop, {shutdown, Reason}, P_D};

%% -------
%% State: 'accept' | #accept{}

handle_event(
  {call, From}, {accept, ListenSocket, Timeout},
  'accept' = _State, {P, D}) ->
    handle_accept(P, D, From, ListenSocket, Timeout, accept);
handle_event(Type, Content, 'accept' = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);

handle_event(
  info, ?socket_select(ListenSocket, SelectRef),
  #accept{
     info = ?select_info(SelectRef), from = From,
     listen_socket = ListenSocket},
  {P, D}) ->
    handle_accept(P, D, From, ListenSocket, update, select);
handle_event(
  info, ?socket_completion(ListenSocket, CompletionRef, CompletionStatus),
  #accept{
     info = ?completion_info(CompletionRef), from = From,
     listen_socket = ListenSocket},
  {P, D}) ->
    handle_accept(P, D, From, ListenSocket, update, CompletionStatus);
handle_event(
  info, ?socket_abort(ListenSocket, SelectRef, Reason),
  #accept{
     info = ?select_info(SelectRef), from = From,
     listen_socket = ListenSocket},
  P_D) ->
    {next_state,'accept', P_D,
     [{{timeout, accept}, cancel},
      {reply, From, {error, Reason}}]};
handle_event(
  info, ?socket_abort(ListenSocket, CompletionRef, Reason),
  #accept{
     info = ?completion_info(CompletionRef), from = From,
     listen_socket = ListenSocket},
  P_D) ->
    {next_state, 'accept', P_D,
     [{{timeout, accept}, cancel},
      {reply, From, {error, Reason}}]};
handle_event(
  {timeout, accept}, Info,
  #accept{
     info = Info, from = From,
     listen_socket = ListenSocket},
  P_D) ->
    socket_cancel(ListenSocket, Info),
    {next_state, 'accept', P_D,
     [{reply, From, {error, timeout}}]};
handle_event(Type, Content, #accept{} = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);

%% State: 'accept' | #accept{}
%% -------
%% When an accept socket has cleared 'accept' | #accept{}
%% #params.socket is a socket:socket(), not undefined.
%%
%% For listen and connect sockets it is so from state machine start.
%% ------- #params.socket is defined from here on ---------------------------


handle_event({call, From}, get_server_opts, _State, {_P, D}) ->
    ServerData = maps:with(maps:keys(server_opts()), D),
    {keep_state_and_data,
     [{reply, From, {ok, ServerData}}]};


%% How to handle counter wrap messages,
%% i.e how to keep counter wraps in sync with counter values
%% a.k.a how to get consistent counter values...
%%
%% Wrap messages are sent from NIF calls, a call made by
%% this server, or by another process which would be a send operation.
%%
%% A process calls info(Socket), we get the message, there may
%% be a wrap message induced by a NIF call we just made that still
%% is in transit.  So we read the counters, send them plus the current
%% wrap value in a special message to ourselves and enter
%% an intermediate state where we postpone all (almost) events
%% except wrap message events.  When we receive the special
%% message, the wrap counters we have have been updated.
%% But after we did read the counters a counter may wrap (e.g due to some
%% other process calling a NIF send operation).  This wrap message may
%% arrive before *or* after we get the special message when we read
%% the counters a second time.
%%
%% So: when we read the counters the second time we almost trust them
%% together with the current wrap value.  But if the wrap value
%% hasn't changed since we sent the special message we need to check
%% if the counter value has wrapped (jumped down); then there is
%% a wrap message in transit from after our first counter read,
%% and we add 1 to the current wrap count when calculating
%% the counter value.  The actual wrap counter will be updated when
%% the wrap message arrives.
%%
%% That all assumes that we are fast enough to not have to handle
%% two wraps of the same counter in this procedure...
handle_event(
  info, ?socket_counter_wrap(Socket, CounterName), _State,
  {#params{socket = Socket} = P, D}) ->
    {keep_state, {P, count_wrap(CounterName, D)}};
%%
handle_event({call, From}, {Tag, What} = Call, State, {P, D} = P_D)
  when info    =:= Tag;
       getstat =:= Tag ->
    case State of
        'closed' ->
            {keep_state_and_data,
             [{reply, From, ?CLOSED_SOCKET}]};
        _ ->
            Socket      = P#params.socket,
            #{counters := Counters_0} = socket:info(Socket),
            CounterKeys = counter_keys(What),
            Counters    = maps:with(CounterKeys, Counters_0),
            Wraps       = maps:with(CounterKeys, D),
            Ref         = make_ref(),
            %% We could send D as Wraps, but we filter out
            %% the wrap counter keys to keep the message size down
            self() ! {wrap_counters, Ref, From, Counters, Wraps},
            {next_state,
             #wrap_counters{ref = Ref, call = Call, state = State},
             P_D}
    end;
%% -------
%% State: #wrap_counters{}
handle_event(
  info, {wrap_counters, Ref, From, Counters1, Wraps1},
  #wrap_counters{ref = Ref, call = {Tag, What}, state = NewState},
  {#params{socket = Socket} = P, D} = P_D) ->
    Info            = #{counters := Counters2} = socket:info(Socket),
    Counters        = wrap_counters(Counters1, Counters2, Wraps1, D),
    GetstatCounters = getstat_what(What, Counters),
    Reply =
        case Tag of
            info ->
                Owner           = P#params.owner,
                Active          = maps:get(active, D),
                Info
                    #{counters := maps:from_list(GetstatCounters),
                      owner => Owner,
                      active => Active};
            getstat ->
                {ok, GetstatCounters}
        end,
    {next_state, NewState, P_D,
     [{reply, From, Reply}]};
handle_event(_Type, _Content, #wrap_counters{}, _P_D) ->
    {keep_state_and_data, [postpone]};
%% State: #info{}
%% -------

handle_event(
  {call, {Caller, _} = From}, {controlling_process, NewOwner},
  State, {P, _D} = P_D) ->
    %%
    case P of
        #params{owner = NewOwner} ->
            {keep_state_and_data,
             [{reply, From, ok}]};
        #params{owner = Caller} ->
            {next_state,
             #controlling_process{owner = NewOwner, state = State},
             P_D,
             [{reply, From, transfer}]};
        #params{} ->
            {keep_state_and_data,
             [{reply, From, {error, not_owner}}]}
    end;
%% -------
%% State: #controlling_process{}
handle_event(
  {call, {Owner, _} = From}, controlling_process,
  #controlling_process{owner = NewOwner, state = State},
  {#params{owner = Owner, owner_mon = OwnerMon} = P, D}) ->
    %%
    NewOwnerMon = erlang:monitor(process, NewOwner),
    true = erlang:demonitor(OwnerMon, [flush]),
    {next_state, State,
     {P#params{owner = NewOwner, owner_mon = NewOwnerMon}, D},
     [{reply, From, ok}]};
%%
%% Postpone all events but the ones above controlling_process/1
%% until the controlling process has been changed
handle_event(
  _Type, _Content,
  #controlling_process{},
  _StateData) ->
    %%
    {keep_state_and_data, [postpone]};
%% State: #controlling_process{}
%% -------

handle_event({call, From}, {getopts, Opts}, State, {P, D}) ->
    %% ?DBG([{opts, Opts}, {state, State}, {d, D}]),
    Result = case call_getopts(P, D, State, Opts) of
                 {ok, OptVals} ->
                     %% ?DBG([{opt_vals, OptVals}]),
                     {ok, externalize_getopts(OptVals)};
                 {error, _} = ERROR ->
                     ERROR
             end,
    %% ?DBG([{result, Result}]),
    {keep_state_and_data,
     [{reply, From, Result}]};

handle_event({call, From}, {setopts, Opts}, State, {P, D_0}) ->
    %% To produce less garbage - work on a diminished D map
    %% and last merge all changes, also to see when there are
    %% metadata changes and only update metadata if so
    %%
    %% The map keys MinKeys are the ones for which the current value
    %% affects the option handling
    %%
    MinKeys = [active, recv_httph],
    {Result, D_1, ActionsR} =
        call_setopts(P, maps:with(MinKeys, D_0), State, Opts),
    %% Merge the changes
    D = maps:merge(maps:without(MinKeys, D_0), D_1),
    is_map_keys(meta_opts(), D_1) andalso
        begin % Metadata option change - update by overwrite
            ok = socket:setopt(P#params.socket, {otp,meta}, meta(D))
        end,
    handle_active(P, D, State, reverse(ActionsR, [{reply, From, Result}]));

handle_event({call, From}, {setopt_active, Active}, State, {P, D_0}) ->
    %% The Active option doesn't affect any metadata option,
    %% and will only cause one D map update, so there is no need
    %% for the diminished D map dance above
    %%
    {Result, D, ActionsR} = call_setopts_active(P, D_0, State, [], Active),
    handle_active(P, D, State, reverse(ActionsR, [{reply, From, Result}]));

handle_event({call, From}, {close, Caller}, State, {P, D}) ->
    handle_close(P, D, State, Caller, [{reply, From, ok}]);

%% -------
%% State: 'closed'
handle_event(internal, exit, 'closed', _P_D) ->
    %% Sent from handle_recv_error/4,
    %% corresponds to driver_exit() in inet_drv
    {stop, {shutdown, closed}};
handle_event(Type, Content, 'closed' = State, P_D) ->
    handle_closed(Type, Content, State, P_D);
%% State: 'closed'
%% -------

handle_event({call, From}, {send_error, Reason}, State, {P, D}) ->
    handle_send_error(P, D, State, From, Reason);

%% -------
%% State: 'closed_read'
handle_event(Type, Content, 'closed_read' = State, P_D) ->
    handle_closed(Type, Content, State, P_D);
%% State: 'closed_read'
%% -------

handle_event({call, From}, {bind, BindAddr} = _BIND, _State, {P, _D}) ->
    %% ?DBG([_BIND, {state, _State}, {p, P}]),
    %% _ = socket:setopt(P#params.socket, otp, debug, true),
    Result = socket:bind(P#params.socket, BindAddr),
    %% _ = socket:setopt(P#params.socket, otp, debug, false),
    %% ?DBG([{bind_result, Result}]),
    {keep_state_and_data,
     [{reply, From, Result}]};

%% It is a bit arbitrary that {listen, _} returns {ok, Socket},
%% since Socket is known since start_server, but has not been returned
%% to listen/1 yet.  It could be returned from {bind, _},
%% or from a separate get_socket call, but piggy-backing it
%% on {listen, _} is convenient.
%% It also reflects the API behaviour (gen_tcp:listen(...) -> {ok, Socket})

handle_event(
  {call, From}, {listen, Backlog} = _LISTEN,
  _State, {#params{socket = Socket} = P, D}) ->
    %% ?DBG({handle_event, call, _LISTEN, _State}),
    Result =
        case socket:listen(Socket, Backlog) of
            ok -> {ok, Socket};
            {error, _} = Error -> Error
        end,
    %% ?DBG({listen_result, Result}),
    {keep_state, {P, D#{type => listen}},
     [{reply, From, Result}]};

%% -------
%% State: 'connect'

handle_event(
  {call, From}, {connect, Addr, Timeout}, 'connect' = _State, {P, D}) ->
    handle_connect(P, D, From, Addr, Timeout, connect);

handle_event(
  {call, From}, {recv, _Length, _Timeout}, 'connect' = _State, _P_D) ->
    {keep_state_and_data,
     [{reply, From, {error, enotconn}}]};

handle_event(
  {call, From}, fdopen, 'connect' = _State,
  {#params{socket = Socket} = P, D}) ->
    handle_connected(
      P, D#{type => fdopen},
      [{reply, From, {ok, Socket}}]);

handle_event(Type, Content, 'connect' = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);

%% State: 'connect'
%% -------
%% State: #connect{}

handle_event(
  info, ?socket_select(Socket, SelectRef),
  #connect{info = ?select_info(SelectRef), from = From, addr = Addr} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['select message', {ref, SelectRef}]),
    handle_connect(P, D, From, Addr, update, select);

handle_event(
  info, ?socket_abort(Socket, SelectRef, Reason),
  #connect{info = ?select_info(SelectRef), from = From} = _State,
  {#params{socket = Socket}, _D} = P_D) ->
    {next_state, 'connect', P_D,
     [{{timeout, connect}, cancel}, {reply, From, {error, Reason}}]};

handle_event(
  info, ?socket_completion(Socket, CompletionRef, CompletionStatus),
  #connect{info = ?completion_info(CompletionRef),
           from = From,
           addr = Addr} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['completion message',
    %% 	  {ref, CompletionRef}, {status, CompletionStatus}]),
    handle_connect(P, D, From, Addr, update, CompletionStatus);

handle_event(
  info, ?socket_abort(Socket, CompletionRef, Reason),
  #connect{info = ?completion_info(CompletionRef), from = From} = _State,
  {#params{socket = Socket}, _D} = P_D) ->
  {next_state, 'connect', P_D,
   [{{timeout, connect}, cancel},
    {reply, From, {error, completion_status_reason(Reason)}}]};

handle_event(
  {timeout, connect}, Info,
  #connect{info = Info, from = From},
  {#params{socket = Socket}, _D} = P_D) ->
    socket_cancel(Socket, Info),
    {next_state, 'connect', P_D,
     [{reply, From, {error, timeout}}]};

handle_event(
  {call, From}, {recv, _Length, _Timeout}, #connect{} = _State, _P_D) ->
    {keep_state_and_data,
     [{reply, From, {error, enotconn}}]};
handle_event(Type, Content, #connect{} = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);

%% State: #connect{}
%% -------
%% State: 'connected' | #recv{}

handle_event(
  {call, From}, {recv, Length, Timeout}, State, {P, D}) ->
    %% ?DBG([recv, {length, Length}, {timeout, Timeout}, {state, State}]),
    case State of
        'connected' ->
            Packet = maps:get(packet, D),
            if
                Packet =/= raw, Packet =/= 0, 0 < Length ->
                    %% Nonzero Length not allowed in packet mode (non-raw)
                    {keep_state_and_data, [{reply, From, {error, einval}}]};
                true ->
                    handle_recv(
                      P, D#{recv_length => Length, recv_from => From},
                      [{{timeout, recv}, Timeout, recv}])
            end;
        #recv{} ->
            case maps:get(active, D) of
                false ->
                    %% Receive in progress
                    {keep_state_and_data,
                     [postpone]};
                _ ->
                    {keep_state_and_data,
                     [{reply, From, {error, einval}}]}
            end
    end;

%% State: 'connected' | #recv{}
%% -------
%% State: #recv{}

handle_event(
  info, ?socket_select(Socket, SelectRef),
  #recv{info = ?select_info(SelectRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG([info, {socket, Socket}, {ref, SelectRef}]),
    handle_recv(P, D, []);

handle_event(
  info, ?socket_abort(Socket, SelectRef, Reason),
  #recv{info = ?select_info(SelectRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG({abort, Reason}),
    handle_recv_error(P, D, [], Reason);

handle_event(
  info, ?socket_completion(Socket, CompletionRef, CompletionStatus),
  #recv{info = ?completion_info(CompletionRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['completion msg', {socket, Socket}, {ref, CompletionRef}]),
    handle_recv(P, D, [], CompletionStatus);

handle_event(
  info, ?socket_abort(Socket, CompletionRef, Reason),
  #recv{info = ?completion_info(CompletionRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['abort msg', {reason, Reason}]),
    handle_recv_error(P, D, [], Reason);

handle_event(
  {timeout, recv}, recv, #recv{info = Info},
  {#params{socket = Socket} = P, D}) ->
    socket_cancel(Socket, Info),
    handle_recv_error(P, D, [], timeout);

%% State: #recv{}
%% -------

%% Catch-all
handle_event(Type, Content, State, P_D) ->
    handle_unexpected(Type, Content, State, P_D).

%% End of event handler
%% -------------------------------------------------------------------------
%% Event handler helpers

completion_status_reason(Reason) ->
    case Reason of
        {completion_status, #{info := netname_deleted}} -> closed;
        {completion_status, netname_deleted}            -> closed;
        {completion_status, #{info := Info}}            -> Info;
        {completion_status, Info}                       -> Info;
        _                                               -> Reason
    end.

handle_closed(
  {call, From}, {recv, _Length, _Timeout}, _State,
  {P, #{delayed_reason := Reason} = D}) ->
    %%
    {keep_state, {P, maps:remove(delayed_reason, D)},
     [{reply, From, {error, Reason}}]};
handle_closed(Type, Content, State, {P, _D}) ->
    case Type of
        {call, From} ->
            {keep_state_and_data,
             [{reply, From, {error, closed}}]};
        _ ->
            warning_msg("Received unexpected event:"
                        "~n   Socket:     ~p"
                        "~n   State:      ~p"
                        "~n   Event Type: ~p"
                        "~n   Content:    ~p",
                        [P#params.socket, State, Type, Content]),
            keep_state_and_data
    end.

handle_unexpected(Type, Content, State, {P, _D}) ->
    warning_msg("Received unexpected event:"
                "~n   Socket:     ~p"
                "~n   State:      ~p"
                "~n   Event Type: ~p"
                "~n   Content:    ~p",
                [P#params.socket, State, Type, Content]),
    case Type of
        {call, From} ->
            {keep_state_and_data,
             [{reply, From, {error, einval}}]};
        _ ->
            keep_state_and_data
    end.

%% State transition helpers -------

handle_close(#params{socket = Socket} = P, D, State, Caller, ActionsR) ->
    {P_D, Actions} =
        case State of
            %% State #controlling_process{} postpones the close/0 call,
            %% and terminate/3 hides that state,
            %% therefore it cannot appear here
            #accept{
               info = SelectInfo, from = From, listen_socket = ListenSocket} ->
                socket_cancel(ListenSocket, SelectInfo),
                {{P, D},
                 reverse(ActionsR,
                         [{{timeout, accept}, cancel},
                          {reply, From, {error, closed}}])};
            #connect{info = Info, from = From} ->
                socket_cancel(Socket, Info),
                socket_close(Socket),
                {{P, D},
                 reverse(ActionsR,
                         [{{timeout, connect}, cancel},
                          {reply, From, {error, closed}}])};
            #recv{info = Info} ->
                socket_cancel(P#params.socket, Info),
                socket_close(Socket),
                {next_state, _, P_D_1, Actions_1} =
                    handle_recv_error(P, D, ActionsR, closed, Caller),
                {P_D_1, Actions_1};
            _ when State =:= 'closed_read';
                   State =:= 'connect';
                   State =:= 'connected' ->
                socket_close(Socket),
                {{P, D}, reverse(ActionsR)};
            _ when State =:= 'accept';
                   State =:= 'closed' ->
                {{P, D}, reverse(ActionsR)}
        end,
    {next_state, 'closed', P_D, Actions}.

handle_connect(
  #params{socket = Socket} = P, D, From, Addr, Timeout, connect) ->
    %%
    %% ?DBG([{d, D}, {addr, Addr}]),
    %% _ = socket:setopt(Socket, otp, debug, true),
    case socket:connect(Socket, Addr, nowait) of
        ok ->
	    %% _ = socket:setopt(Socket, otp, debug, false),
            handle_connected(
              P, D#{type => connect},
              [{{timeout, connect}, cancel},
               {reply, From, {ok, Socket}}]);

        {select, ?select_info(_) = Info} ->
	    %% _ = socket:setopt(Socket, otp, debug, false),
	    %% ?DBG(['select info']),
            {next_state,
             #connect{info = Info, from = From, addr = Addr},
             {P, D#{type => connect}},
             [{{timeout, connect}, Timeout, Info}]};

        {completion, ?completion_info(_) = Info} ->
	    %% _ = socket:setopt(Socket, otp, debug, false),
	    %% ?DBG(['completion info']),
            {next_state,
             #connect{info = Info, from = From, addr = Addr},
             {P, D#{type => connect}},
             [{{timeout, connect}, Timeout, Info}]};

        {error, _} = Error ->
	    %% _ = socket:setopt(Socket, otp, debug, false),
	    %% ?DBG(['connect failed', {error, Error}]),
            {next_state,
             'connect', {P, D},
             [{{timeout, connect}, cancel},
              {reply, From, Error}]}
    end;
handle_connect(
  #params{socket = Socket} = P, D, From, Addr, Timeout, select) ->
    %%
    %% ?DBG([{d, D}, {addr, Addr}]),
    case socket:connect(Socket, Addr, nowait) of
        ok ->
            handle_connected(
              P, D#{type => connect},
              [{{timeout, connect}, cancel},
               {reply, From, {ok, Socket}}]);

        {select, ?select_info(_) = Info} ->
            {next_state,
             #connect{info = Info, from = From, addr = Addr},
             {P, D#{type => connect}},
             [{{timeout, connect}, Timeout, Info}]};

        {error, _} = Error ->
            {next_state,
             'connect', {P, D},
             [{{timeout, connect}, cancel},
              {reply, From, Error}]}
    end;
handle_connect(#params{socket = Socket} = P, D, From, _Addr, _Timeout, ok) ->
    handle_connected(
      P,
      D#{type => connect},
      [{{timeout, connect}, cancel}, {reply, From, {ok, Socket}}]);
handle_connect(#params{} = P, D, From, _Addr, _Timeout,
	       {error, _Reason} = Error) ->
    %% ?DBG(['connect failed', {readon, _Reason}]),
    {next_state, 'connect', {P, D},
     [{{timeout, connect}, cancel}, {reply, From, Error}]}.


handle_accept(P, D, From, ListenSocket, Timeout, Status)
  when Status =:= select;
       Status =:= accept ->
    %% ?DBG({try_accept, D}),
    case socket:accept(ListenSocket, nowait) of
        {ok, Socket} ->
            handle_accept_success(P, D, From, ListenSocket, Socket);

        {select, ?select_info(_) = SelectInfo} ->
            %% ?DBG({accept_select, SelectInfo}),
            {next_state,
             #accept{
                info = SelectInfo, from = From,
                listen_socket = ListenSocket},
             {P, D#{type => accept}},
             [{{timeout, accept}, Timeout, SelectInfo}]};

        {completion, ?completion_info(_) = CompletionInfo} ->
            %% ?DBG({accept_completion, CompletionInfo}),
            {next_state,
             #accept{
                info = CompletionInfo, from = From,
                listen_socket = ListenSocket},
             {P, D#{type => accept}},
             [{{timeout, accept}, Timeout, CompletionInfo}]};

        {error, _Reason} = Error ->
            handle_accept_failure(P, D, From, Error)
    end;
handle_accept(P, D, From, ListenSocket, _Timeout, {ok, Socket}) ->
    handle_accept_success(P, D, From, ListenSocket, Socket);
handle_accept(P, D, From, _ListenSocket, _Timeout, {error, _Reason} = Error) ->
    handle_accept_failure(P, D, From, Error).

handle_accept_success(P, D, From, ListenSocket, AccSocket) ->
    %% ?DBG([{acc_socket, AccSocket}]),
    ok = socket:setopt(AccSocket, {otp,iow}, true),
    ok = socket:setopt(AccSocket, {otp,meta}, meta(D)),
    [ok = socket_copy_opt(ListenSocket, Opt, AccSocket)
     || Opt <- socket_inherit_opts()],
    handle_connected(
      P#params{socket = AccSocket}, D#{type => accept},
      [{{timeout, accept}, cancel},
       {reply, From, {ok, AccSocket}}]).

handle_accept_failure(P, D, From, Error) ->
    %% ?DBG([{error, Error}]),
    {next_state,
     'accept', {P, D},
     [{{timeout, accept}, cancel},
      {reply, From, Error}]}.


handle_connected(P, {D, ActionsR}) ->
    handle_connected(P, D, ActionsR).
%%
handle_connected(P, D, ActionsR) ->
    %% ?DBG([{p, P}, {d, D}, {actions_r, ActionsR}]),
    case D of
        #{active := false} ->
            {next_state, 'connected',
             {P, D},
             reverse(ActionsR)};
        #{active := _} ->
            handle_recv(P, recv_start(D), ActionsR)
    end.


handle_recv(P, D, ActionsR) ->
    handle_recv(P, D, ActionsR, recv).

%% CS is:
%%     recv             -> no async recv in progress - either a new
%%                         socket passive mode recv request, an
%%                         active mode socket reached 'connected',
%%                         or a select message was received
%%                         so it is time for a retry
%%     {recv, Reason}   -> a socket operation has failed, but we will handle
%%                         the buffer content before returning the error
%%     CompletionStatus -> the result of an async recv operation
%%
handle_recv(P, #{buffer := Buffer} = D, ActionsR, CS) ->
    BufferSize = iolist_size(Buffer),
    %% ?DBG(CS),
    case CS of
        recv ->
            handle_recv(P, D, ActionsR, Buffer, BufferSize, CS);
        {recv, _} ->
            handle_recv(P, D, ActionsR, Buffer, BufferSize, CS);

        %% CompletionStatus
        {ok, <<Data/binary>>} ->
            handle_recv(
              P, D, ActionsR, buffer(Data, Buffer),
              BufferSize + byte_size(Data), recv);
        {more, <<Data/binary>>} ->
            handle_recv(
              P, D, ActionsR, buffer(Data, Buffer),
              BufferSize + byte_size(Data), recv);
        {error, {Reason, <<Data/binary>>}} ->
            handle_recv(
              P, D, ActionsR, buffer(Data, Buffer),
              BufferSize + byte_size(Data), {recv, Reason});
        {error, Reason} ->
            handle_recv_error(P, D, ActionsR, Reason)
    end.

handle_recv(
  P, #{packet := PacketType, recv_length := Length} = D,
  ActionsR, Buffer, BufferSize, CS) ->
    %% ?DBG({D, Buffer, BufferSize, CS}),
    if
        BufferSize < Length;
        Length == 0, BufferSize == 0 ->
            case CS of
                recv ->
                    handle_recv_more(
                      P, D, ActionsR, Buffer, BufferSize, Length);
                {recv, Reason} ->
                    handle_recv_error(P, D, ActionsR, Reason)
            end;

        PacketType =:= raw;
        PacketType =:= 0 ->
            Data = condense_buffer(Buffer),
            {Data_1, Buffer_1} =
                if
                    0 < Length -> %% Length =< BufferSize
                        split_binary(Data, Length);
                    true -> %% Length == 0, 0 < BufferSize
                        {Data, <<>>}
                end,
            D_1 = D#{buffer := Buffer_1},
            case CS of
                recv ->
                    handle_connected(
                      P, recv_data_deliver(P, D_1, ActionsR, Data_1));
                {recv, Reason} ->
                    handle_recv_error(
                      P, recv_data_deliver(P, D_1, ActionsR, Data_1), Reason)
            end;

        true ->
            MinHdrLen = packet_header_length(PacketType),
            if
                BufferSize < MinHdrLen ->
                    handle_recv_more(
                      P, D, ActionsR, Buffer, BufferSize, MinHdrLen);
                true ->
                    handle_recv_packet(
                      P, D, ActionsR, Buffer, BufferSize, CS)
            end
    end.

handle_recv_packet(P, D, ActionsR, Buffer, BufferSize, recv = _CS) ->
    %% ?DBG({Buffer, BufferSize, _CS}),
    Data = condense_buffer(Buffer),
    case decode_packet(D, Data) of
        {ok, Decoded, Rest} ->
            D_1 = D#{buffer := Rest},
            handle_connected(
              P, recv_data_deliver(P, D_1, ActionsR, Decoded));
        {more, undefined} ->
            %% Odd bad case - try one byte more
            %%
            %% Can be very inefficient, but the only way to not read ahead
            %% for example for packet=line, so to combine such packet types
            %% with read_ahead=false would be considered as misuse
            handle_recv_more(
              P, D, ActionsR, Data, BufferSize, BufferSize + 1);
        {more, Length} ->
            handle_recv_more(
              P, D, ActionsR, Data, BufferSize, Length);
        {error, invalid} ->
            handle_recv_error(P, D, ActionsR, emsgsize);
        {error, Reason} ->
            handle_recv_error(P, D, ActionsR, Reason)
    end;
handle_recv_packet(
  P, D, ActionsR, Buffer, _BufferSize, {recv, Reason} = _CS) ->
    %% ?DBG({Buffer, _BufferSize, _CS}),
    Data = condense_buffer(Buffer),
    case decode_packet(D, Data) of
        {ok, Decoded, Rest} ->
            D_1 = D#{buffer := Rest},
            handle_recv_error(
              P, recv_data_deliver(P, D_1, ActionsR, Decoded),
              Reason);
        {more, _} ->
            handle_recv_error(P, D, ActionsR, Reason);
        {error, _} ->
            handle_recv_error(P, D, ActionsR, Reason)
    end.

handle_recv_more(
  P, D, ActionsR, Buffer, BufferSize, Length) ->
    %% ?DBG({Buffer, BufferSize, Length}),
    %% Less buffered than requested
    %% or nothing buffered and "what's available"|unknown requested
    %%
    %% I.e 0 < BufferSize, BufferSize < Length;
    %%         BufferSize == 0, Length == 0 ->
    %% In both cases this works:
    N = Length - BufferSize, % How much to recv
    case socket_recv(P#params.socket, read_size(D, N)) of
        {ok, <<Data/binary>>} ->
            handle_recv(
              P, D, ActionsR, buffer(Data, Buffer),
              BufferSize + byte_size(Data), recv);

        {select, {?select_info(_) = SelectInfo, <<Data/binary>>}} ->
            Buffer_1 = buffer(Data, Buffer),
            BufferSize_1 = BufferSize + byte_size(Data),
            if
                Length =< BufferSize_1 ->
                    %% Enough data; cancel the async recv
                    %% and use what we have
                    Socket = P#params.socket,
                    case socket:cancel(Socket, SelectInfo) of
                        ok ->
                            handle_recv(
                              P, D, ActionsR, Buffer_1, BufferSize_1, recv);
                        {error, Reason} ->
                            handle_recv(
                              P, D, ActionsR, Buffer_1, BufferSize_1,
                              {recv, Reason})
                    end;
                true ->
                    %% Need to wait for the rest of the data
                    {next_state,
                     #recv{info = SelectInfo},
                     {P,D#{buffer := Buffer_1}},
                     reverse(ActionsR)}
            end;

        {select, ?select_info(_) = SelectInfo} ->
            %% ?DBG(['recv select']),
            {next_state,
             #recv{info = SelectInfo},
             {P, D#{buffer := Buffer}},
             reverse(ActionsR)};

        {completion, ?completion_info(_) = CompletionInfo} ->
            %% ?DBG(['recv completion']),
            {next_state,
             #recv{info = CompletionInfo},
             {P, D#{buffer := Buffer}},
             reverse(ActionsR)};

        {error, {Reason, <<Data/binary>>}} ->
            %% ?DBG({'recv error', Reason, byte_size(Data)}),
            handle_recv(
              P, D, ActionsR, buffer(Data, Buffer),
              BufferSize + byte_size(Data), {recv, Reason});

        {error, Reason} ->
            %% ?DBG({'recv error', Reason}),
            handle_recv_error(P, D, ActionsR, Reason)
    end.


decode_packet(
  #{packet         := (PacketType = line),
    line_delimiter := LineDelimiter,
    packet_size    := PacketSize},
  Data) ->
    %%
    erlang:decode_packet(
      PacketType, Data,
      [{packet_size,    PacketSize},
       {line_delimiter, LineDelimiter},
       {line_length,    PacketSize}]);
decode_packet(
  #{packet         := http,
    recv_httph     := true,
    packet_size    := PacketSize},
  Data) ->
    %%
    erlang:decode_packet(httph, Data, [{packet_size, PacketSize}]);
decode_packet(
  #{packet         := http_bin,
    recv_httph     := true,
    packet_size    := PacketSize},
  Data) ->
    %%
    erlang:decode_packet(httph_bin, Data, [{packet_size, PacketSize}]);
decode_packet(
  #{packet         := PacketType,
    packet_size    := PacketSize},
  Data) ->
    %%
    erlang:decode_packet(PacketType, Data, [{packet_size, PacketSize}]).

-ifdef(undefined).
decode_packet(D, Data, PacketType, Options) ->
    case erlang:decode_packet(PacketType, Data, Options) of
        {ok, Decoded, Rest} ->
            %% ?DBG({ok, PacketType, byte_size(Decoded)}),
            {D#{buffer := Rest}, ok, Decoded};
        {more, undefined} ->
            Length = packet_header_length(PacketType, Data),
            {D, more, Length};
        {more, Length} ->
            {D#{recv_length := Length}, more, Length};
        {error, Reason} ->
            {D, error, Reason}
    end.
-endif.

-compile({inline, [packet_header_length/1]}).
packet_header_length(PacketType) ->
    case PacketType of
        raw     -> error(badarg, [PacketType]);
        0       -> error(badarg, [PacketType]);
        1       -> 1;
        2       -> 2;
        4       -> 4;
        cdr     -> 12;
        sunrm   -> 4;
        fcgi    -> 8;
        tpkt    -> 4;
        ssl     -> 5;
        ssl_tls -> 5;
        asn1    -> 2;
        _       -> 1
    end.


%% How much to read given read_ahead, {otp,rcvbuf} and request size N
read_size(D, N) ->
    case
        N == 0 % "What's available" requested
        orelse
        (maps:get(read_ahead, D) % Read ahead configured
         andalso
         %% Request smaller than rcvbuf
         N < maps:get({otp,rcvbuf}, D, ?RECV_BUFFER_SIZE_DEFAULT))
    of
        true  -> 0;
        false -> N
    end.


handle_recv_error(P, {D, ActionsR}, Reason) ->
    handle_recv_error(P, D, ActionsR, Reason).

handle_recv_error(P, D, ActionsR, Reason) ->
    handle_recv_error(P, D, ActionsR, Reason, undefined).
%%
handle_recv_error(
  P, #{active := Active} = D_0, ActionsR_0, Reason, Caller) ->
    %%
    %% Send active socket messages
    %%
    if
        Reason =:= timeout;
        Reason =:= emsgsize ->
            {D_1, ActionsR_1} =
                case Active of
                    false ->
                        recv_error_reply(D_0, ActionsR_0, Reason);
                    _ ->
                        ModuleSocket = module_socket(P),
                        Owner = P#params.owner,
                        Owner ! {tcp_error, ModuleSocket, Reason},
                        {D_0#{active := false}, ActionsR_0}
                end,
            {next_state, 'connected', {P, D_1}, reverse(ActionsR_1)};
        true ->
            ShowReason = curated_error_reason(D_0, Reason),
            case Active of
                false ->
                    {D_1, ActionsR_1} =
                        recv_error_reply(D_0, ActionsR_0, ShowReason),
                    handle_recv_error_exit_on_close(P, D_1, ActionsR_1);
                _ ->
                    ModuleSocket = module_socket(P),
                    Owner = P#params.owner,
                    ShowReason =/= closed andalso
                        begin
                            Owner ! {tcp_error, ModuleSocket, ShowReason}
                        end,
                    Caller =/= Owner andalso
                        begin
                            Owner ! {tcp_closed, ModuleSocket}
                        end,
                    handle_recv_error_exit_on_close(
                      P, D_0#{active := false}, ActionsR_0)
            end
    end.

handle_recv_error_exit_on_close(P, D, ActionsR_0) ->
    {ExitOnClose, ActionsR_1} = exit_on_close(D, ActionsR_0),
    case ExitOnClose of
        false ->
            {next_state, 'closed_read', {P, D}, reverse(ActionsR_1)};
        true ->
            socket_close(P#params.socket),
            {next_state, 'closed', {P, D}, reverse(ActionsR_1)}
    end.

%% -> {D, ActionsR}
recv_error_reply(D, ActionsR, Reason) ->
    case D of
        #{recv_from := From} ->
            {recv_stop(D),
             [{{timeout, recv}, cancel},
              {reply, From, {error, Reason}} | ActionsR]};
        #{} ->
            {D, ActionsR}
    end.

%% -> {ExitOnClose, ActionsR}
exit_on_close(D, ActionsR) ->
    ExitOnClose = maps:get(exit_on_close, D),
    {ExitOnClose,
     case ExitOnClose of
         true ->
             [{next_event, internal, exit} | ActionsR];
         false ->
            ActionsR
     end}.

handle_send_error(#params{socket = Socket} = P, D_0, State, From, Reason) ->
    %%
    ReplyReason = curated_error_reason(D_0, Reason),
    Reply = {reply, From, {error, ReplyReason}},
    D_1 = D_0#{delayed_reason => ReplyReason},
    case State of
        #recv{info = Info} ->
            socket_cancel(Socket, Info),
            socket_close(Socket),
            case maps:get(active, D_1) of
                false ->
                    {D_2, ActionsR_2} =
                        recv_error_reply(D_1, [Reply], ReplyReason),
                    {_, ActionsR_2} = exit_on_close(D_2, ActionsR_2),
                    {next_state, 'closed', {P, D_2}, ActionsR_2};
                _ ->
                    ModuleSocket = module_socket(P),
                    Owner = P#params.owner,
                    ReplyReason =/= closed andalso
                        begin
                            Owner ! {tcp_error, ModuleSocket, ReplyReason}
                        end,
                    Owner ! {tcp_closed, ModuleSocket},
                    {_, ActionsR} = exit_on_close(D_1, [Reply]),
                    {next_state, 'closed', {P, D_1}, ActionsR}
            end;
        _ when State =:= 'connected';
               State =:= 'closed_read';
               State =:= 'connect' ->
            socket_close(Socket),
            {next_state, 'closed', {P, D_1}, [Reply]}
    end.

%% -> CuratedReason
curated_error_reason(D, {completion_status, CS}) ->
    curated_error_reason(D, CS);
curated_error_reason(D, #{info := Info}) ->
    curated_error_reason(D, Info);
curated_error_reason(D, Reason) ->
    if
        Reason =:= econnreset;
        Reason =:= econnaborted;
	Reason =:= netname_deleted;
	Reason =:= epipe ->
            case maps:get(show_econnreset, D) of
                true  -> econnreset;
                false -> closed
            end;
	Reason =:= too_many_commands ->
	    closed;
        true ->
            Reason
    end.


handle_active(P, D, State, ActionsR) ->
    case State of
        'connected' ->
            handle_connected(P, D, reverse(ActionsR));
        #recv{info = Info} ->
            case D of
                #{active := false} ->
                    %% Cancel recv in progress
                    _ = socket_cancel(P#params.socket, Info),
                    {next_state, 'connected',
                     {P, D}, reverse(ActionsR)};
                #{active := _} ->
                    {keep_state, {P, D}, reverse(ActionsR)}
            end;
        _ ->
            {keep_state, {P, D}, reverse(ActionsR)}
    end.

%% -------------------------------------------------------------------------
%% Callback Helpers

%% Initialize packet recv state
recv_start(D) ->
    D#{recv_length => 0}.

recv_stop(D) ->
    maps:without([recv_from, recv_length], D).

%% Deliver data and update the active state
%% -> {NewD, NewActionsR}
recv_data_deliver(
  #params{owner = Owner} = P,
  #{mode := Mode, header := Header, deliver := Deliver,
    packet := Packet} = D,
  ActionsR, Data) ->
    %% ?DBG(Data),
    %%
    %% ?DBG([{owner, Owner},
    %% 	  {mode, Mode},
    %% 	  {header, Header}, {deliver, Deliver}, {packet, Packet}]),
    DeliverData = deliver_data(Data, Mode, Header, Packet),
    case D of
        #{recv_from := From} -> % Explicit recv/2 call
            {recv_stop(next_packet(D, Packet, Data)),
             [{reply, From, {ok, DeliverData}},
              {{timeout, recv}, cancel}
              | ActionsR]};
        #{active := false} -> % Cannot deliver - buffer instead
            D_1 = D#{buffer := buffer(Data, maps:get(buffer, D))},
            {recv_stop(next_packet(D_1, Packet, Data)),
             ActionsR};
        #{active := Active} ->
            ModuleSocket = module_socket(P),
            Owner !
                case Deliver of
                    term ->
                        {tag(Packet), ModuleSocket, DeliverData};
                    port ->
                        {ModuleSocket, {data, DeliverData}}
                end,
            %% ?DBG('package delivered'),
            case Active of
                true ->
                    {recv_start(next_packet(D, Packet, Data)),
                     ActionsR};
                once ->
                    {recv_stop(next_packet(D, Packet, Data, false)),
                     ActionsR};
                1 ->
                    Owner ! {tcp_passive, ModuleSocket},
                    {recv_stop(next_packet(D, Packet, Data, false)),
                     ActionsR};
                N when is_integer(N) ->
                    {recv_start(next_packet(D, Packet, Data, Active - 1)),
                     ActionsR}
            end
    end.

%% Next packet type
next_packet(D, Packet, Data) ->
    if
        Packet =:= http;
        Packet =:= http_bin ->
            case Data of
                {http_request, _HttpMethod, _HttpUri, _HttpVersion} ->
                    D#{recv_httph => true};
                {http_response, _HttpVersion, _Integer, _HttpString} ->
                    D#{recv_httph => true};
                {http_header, _Integer, _HttpField, _Reserver, _Value} -> D;
                http_eoh ->
                    D#{recv_httph => false};
                {http_error, _HttpString} -> D
            end;
        true -> D
    end.

%% Duplicate of the above, and also sets active
next_packet(D, Packet, Data, Active) ->
    if
        Packet =:= http;
        Packet =:= http_bin ->
            case Data of
                {http_request, _HttpMethod, _HttpUri, _HttpVersion} ->
                    D#{recv_httph => true, active => Active};
                {http_response, _HttpVersion, _Integer, _HttpString} ->
                    D#{recv_httph => true, active => Active};
                {http_header, _Integer, _HttpField, _Reserver, _Value} ->
                    D#{active => Active};
                http_eoh ->
                    D#{recv_httph => false, active => Active};
                {http_error, _HttpString} ->
                    D#{active => Active}
            end;
        true ->
            D#{active => Active}
    end.

%% Buffer Data in Buffer
-compile({inline, [buffer/2]}).
buffer(Data, <<>>) ->
    Data;
buffer(Data, Buffer) ->
    if
        is_binary(Buffer) ->
            [Data, Buffer];
        is_list(Buffer) ->
            [Data | Buffer]
    end.

%% Condense buffer into a Binary
-compile({inline, [condense_buffer/1]}).
condense_buffer(Bin) when is_binary(Bin) -> Bin;
condense_buffer([Bin]) when is_binary(Bin) -> Bin;
condense_buffer(Buffer) ->
    iolist_to_binary(reverse(Buffer)).

-ifdef(undefined).
condense_buffer(Data, Buffer) ->
    condense_buffer(buffer(Data, Buffer)).
-endif.

deliver_data(Data, Mode, Header, Packet) ->
    if
%%%         Packet =:= 1;
%%%         Packet =:= 2;
%%%         Packet =:= 4 ->
%%%             %% Strip {packet,N} header
%%%             <<?header(Packet, _Size), Payload/binary>> = Data,
%%%             deliver_data(Payload, Mode, Header);
        Packet =:= http;
        Packet =:= http_bin;
        Packet =:= httph;
        Packet =:= httph_bin ->
            %% These haven't got mixed mode header delivery
            Data;
        true ->
            deliver_data(Data, Mode, Header)
    end.

%% Binary -> the data format inet_drv delivers depending on delivery mode
deliver_data(Data, list, _N) -> binary_to_list(Data);
deliver_data(Data, binary, 0) -> Data;
deliver_data(Data, binary, N) ->
    %% Mixed mode with header bytes as list and payload as binary in tail
    case Data of
        <<_:N/binary>> ->
            binary_to_list(Data);
        <<Header:N/binary, Payload/binary>> ->
            binary_to_list(Header) ++ Payload
    end.

%% Packet type -> tuple tag in active mode delivery message
tag(Packet) ->
    if
        Packet =:= http;
        Packet =:= http_bin;
        Packet =:= httph;
        Packet =:= httph_bin ->
            http;
        true ->
            tcp
    end.

%% -------
%% {call, From}, {setopts, Opts}
%%
%% -> {Result, D, ActionsR}
%%

call_setopts(_P, D, _State, []) ->
    call_setopts_result(ok, D);
call_setopts(P, D, State, [{Tag, Val} | Opts]) ->
    case socket_opts() of
        #{Tag := SocketOpt} ->
            call_setopts_socket(P, D, State, Opts, SocketOpt, Val);
        #{} ->
            case maps:is_key(Tag, server_write_opts()) of
                %% server options for socket send hence
                %% duplicated in {opt,meta}
                %%
                true when State =:= 'closed' ->
                    %% ?DBG('server write when state closed'),
                    call_setopts_result({error, einval}, D);
                true ->
                    %% ?DBG('server write side'),
                    call_setopts_server(P, D, State, Opts, Tag, Val);
                false ->
                    case maps:is_key(Tag, server_read_opts()) of
                        %% server options for receive
                        %%
                        true
                          when State =:= 'closed' ->
                            %% ?DBG('server read when state closed*'),
                            call_setopts_result({error, einval}, D);
                        true ->
                            %% ?DBG('server read side'),
                            call_setopts_server(P, D, State, Opts, Tag, Val);
                        false ->
                            %% ignored and invalid options
                            %%
                            case ignore_optname(Tag) of
                                true ->
                                    %% ?DBG(ignore),
                                    call_setopts(P, D, State, Opts);
                                false ->
                                    %% ?DBG({extra, Tag}),
                                    call_setopts_result({error, einval}, D)
                            end
                    end
            end
    end.

%% Options for the 'socket' module
%%
call_setopts_socket(P, D, State, Opts, SocketOpt, Val) ->
    case P#params.socket of
        undefined ->
            call_setopts_result({error, closed}, D);
        Socket ->
            case socket_setopt(Socket, SocketOpt, Val) of
                ok when SocketOpt =:= {otp,rcvbuf} ->
                    Size =
                        case Val of
                            {Count, Sz} -> Count * Sz;
                            Sz when is_integer(Sz) -> Sz
                        end,
                    call_setopts(P, D#{SocketOpt => Size}, State, Opts);
                ok when SocketOpt =:= {socket,rcvbuf} ->
                    %% Mimic inet_drv.c for SOCK_STREAM:
                    %% when setting 'recbuf', if 'buffer' hasn't been set;
                    %% set 'buffer' to the same size
                    %%
                    OtpOpt = {otp,rcvbuf},
                    case D of
                        #{OtpOpt := _} ->
                            case socket_setopt(Socket, OtpOpt, Val) of
                                ok ->
                                    call_setopts(P, D, State, Opts);
                                {error, _} = Error ->
                                    call_setopts_result(Error, D)
                            end;
                        #{} ->
                            call_setopts(P, D, State, Opts)
                    end;
                ok ->
                    call_setopts(P, D, State, Opts);
                {error, _} = Error ->
                    call_setopts_result(Error, D)
            end
    end.

%% Options in the server process D variable
%%
call_setopts_server(P, D, State, Opts, Tag, Val) ->
    case Tag of
        packet ->
            case is_packet_option_value(Val) of
                true ->
                    call_setopts(
                      P, maps:remove(recv_httph, D#{packet => Val}),
                      State, Opts);
                false ->
                    call_setopts_result({error, einval}, D)
            end;
        active ->
            call_setopts_active(P, D, State, Opts, Val);
        _ ->
	    %% ?DBG([{tag, Tag}, {value, Value}]),
            call_setopts(P, D#{Tag => Val}, State, Opts)
    end.

call_setopts_active(P, D, State, Opts, _Active)
  when State =:= 'closed_read' ->
    call_setopts(P, D, State, Opts);
call_setopts_active(P, D_0, State, Opts, Active)
  when State =:= 'closed' ->
    if
        Active =:= false ->
            call_setopts(P, D_0, State, Opts);
        true -> % not false; socket is active
            ModuleSocket = module_socket(P),
            Owner = P#params.owner,
            D_1 =
                case D_0 of
                    #{delayed_reason := Reason} ->
                        Reason =/= closed andalso
                            begin
                                Owner ! {tcp_error, ModuleSocket, Reason}
                            end,
                        maps:remove(delayed_reason, D_0);
                    #{} ->
                        D_0
                end,
            Owner ! {tcp_closed, ModuleSocket},
            socket_close(P#params.socket),
            {ok, D_1, [{next_event, internal, exit}]}
    end;
call_setopts_active(P, D, State, Opts, Active) ->
    %% ?DBG([{active, Active}]),
    if
        Active =:= once;
        Active =:= true ->
            call_setopts(P, D#{active := Active}, State, Opts);
        Active =:= false ->
            OldActive = maps:get(active, D),
            is_integer(OldActive) andalso
                begin
                    P#params.owner ! {tcp_passive, module_socket(P)}
                end,
            call_setopts(P, D#{active := Active}, State, Opts);
        is_integer(Active), -32768 =< Active, Active =< 32767 ->
            OldActive = maps:get(active, D),
            N =
                if
                    is_integer(OldActive) -> OldActive + Active;
                    true                  -> Active
                end,
            if
                32767 < N ->
                    call_setopts_result({error, einval}, D);
                N =< 0 ->
                    P#params.owner ! {tcp_passive, module_socket(P)},
                    call_setopts(P, D#{active := false}, State, Opts);
                true ->
                    call_setopts(P, D#{active := N}, State, Opts)
            end;
        true ->
            call_setopts_result({error, einval}, D)
    end.

call_setopts_result(Result_0, D) ->
    call_setopts_result(Result_0, D, []).
%%
call_setopts_result(Result_0, D, ActionsR) ->
    Result =
        case Result_0 of
            {error, enoprotoopt} ->
                %% If we get this error, the options is not valid for
                %% this (tcp) protocol.
                {error, einval};

            {error, {invalid, _}} ->
                %% If we get this error, the options where crap.
                {error, einval};

            {error, einval} ->
                %% If we get this error, either the options where crap or
                %% the socket is in a "bad state" (maybe it's closed).
                %% So, if that is the case we accept that we may not be
                %% able to update the meta data.
                Result_0;
            {error, _} ->
                %% We should really handle this better. stop_and_reply?
                Result_0;
            ok ->
                ok
        end,
    {Result, D, ActionsR}.

%% -------
%% Exported socket option translation
%%
socket_setopts(Socket, Opts) ->
    try
        begin
            socket_setopts(
              Socket,
              [Opt ||
                  Opt <- internalize_setopts(Opts),
                  element(1, Opt) =/= tcp_module],
              socket_opts())
        end
    catch
        exit:badarg ->
            {error, einval}
    end.
%%
socket_setopts(_Socket, [], _SocketOpts) ->
    ok;
socket_setopts(Socket, [{Tag,Val} | Opts], SocketOpts) ->
    case SocketOpts of
        #{ Tag := Name } ->
            %% Ignore all errors as an approximation for
            %% inet_drv ignoring most errors
            _ = socket_setopt(Socket, Name, Val),
            socket_setopts(Socket, Opts, SocketOpts);
        #{} -> % Ignore
            socket_setopts(Socket, Opts, SocketOpts)
    end.


%%
%% -------
%% getopts in server
%%
%% {call, From}, {getopts, Opts}
%%
%% -> {ok, [Options]} | {error, einval}
%%
call_getopts(P, D, State, Opts) ->
    call_getopts(P, D, State, Opts, []).
call_getopts(_P, _D, _State, [], Acc) ->
    {ok, reverse(Acc)};
call_getopts(P, D, State, [Tag | Tags], Acc) ->
    %% ?DBG([{tag, Tag}]),
    SocketOpts = socket_opts(),
    {Key, Val} =
        case Tag of
            {_, _} -> Tag; % E.g the raw option
            _ when is_atom(Tag) -> {Tag, Tag}
        end,
    case maps:is_key(Key, SocketOpts) of
        true ->
            %% options for the socket module
            %%
            case P#params.socket of
                undefined ->
                    {error, closed};
                Socket ->
                    %% ?DBG({'socket getopt', Tag}),
                    case
                        socket_getopt(
                          Socket, maps:get(Key, SocketOpts), Val)
                    of
                        {ok, Value} ->
                            %% ?DBG({'socket getopt', ok, Value}),
                            call_getopts(
                              P, D, State, Tags, [{Key, Value} | Acc]);
                        {error, einval} = ERROR ->
                            ERROR;
                        {error, _Reason} ->
                            %% ?DBG([{reason, _Reason}]),
                            call_getopts(P, D, State, Tags, Acc)
                    end
              end;
        false ->
            case maps:is_key(Key, server_write_opts()) of
                %% server options for socket send hence
                %% duplicated in {opt,meta}
                %%
                true when State =:= 'closed' ->
                    %% ?DBG('server write when closed'),
                    {error, einval};
                true ->
                    %% ?DBG('server write'),
                    Value = maps:get(Key, D),
                    call_getopts(P, D, State, Tags, [{Key, Value} | Acc]);
                false ->
                    case maps:is_key(Key, server_read_opts()) of
                        %% server options for receive
                        %%
                        true
                          when State =:= 'closed' ->
                            %% ?DBG('server read when closed*'),
                            {error, einval};
                        true ->
                            %% ?DBG('server read'),
                            Value = maps:get(Key, D),
                            call_getopts(
                              P, D, State, Tags, [{Key, Value} | Acc]);
                        false ->
                            %% ignored and invalid options
                            %%
                            case ignore_optname(Key) of
                                true ->
                                    %% ?DBG({ignore, Tag}),
                                    call_getopts(P, D, State, Tags, Acc);
                                false ->
                                    %% ?DBG({extra, Tag}),
                                    {error, einval}
                            end
                    end
            end
    end.

%%
%% -------
%% {call, From}, {info|getstat, What}
%%

counter_keys(What) ->
    lists:usort(counter_keys_1(What)).
%%
counter_keys_1([]) -> [];
counter_keys_1([Tag | Tags]) ->
    counter_key(Tag) ++ counter_keys_1(Tags).

counter_key(Tag) ->
    case Tag of
        recv_oct -> [read_byte];
        recv_cnt -> [read_pkg];
        recv_max -> [read_pkg_max];
        recv_avg -> [read_byte, read_pkg];
        send_oct -> [write_byte];
        send_cnt -> [write_pkg];
        send_max -> [write_pkg_max];
        send_avg -> [write_byte, write_pkg];
        _ -> []
    end.

getstat_what([], _Counters) -> [];
getstat_what([Tag | Tags], Counters) ->
    case counter_key(Tag) of
        [SocketTag] ->
            [{Tag, maps:get(SocketTag, Counters)}
            | getstat_what(Tags, Counters)];
        [NumTag, DenomTag] ->
            [{Tag,
              getstat_div(
                maps:get(NumTag, Counters),
                maps:get(DenomTag, Counters))}
            | getstat_what(Tags, Counters)];
        [] ->
            getstat_what(Tags, Counters)
    end.

getstat_div(N, 0) when is_integer(N), 0 =< N -> N;
getstat_div(N, D)
  when is_integer(N), 0 =< N, is_integer(D), 0 < D ->
    %% Integer division with rounding
    Q = N div D,
    R = N rem D,
    if
        D =< R bsl 1 -> Q + 1;
        true         -> Q
    end.

count_wrap(CounterName, Wraps) ->
    case Wraps of
        #{CounterName := N} ->
            Wraps#{CounterName := N + 1};
        #{} ->
            Wraps#{CounterName => 1}
    end.

wrap_counters(Counters1, Counters2, Wraps1, D) ->
    #{Nm => counter_value(C1, Counters2, Wraps1, D, Nm)
      || Nm := C1 <- Counters1}.

counter_value(C1, Counters2, Wraps1, D, Nm) ->
    W1 = maps:get(Nm, Wraps1, 0),
    W2 = maps:get(Nm, D, 0),
    C2 = maps:get(Nm, Counters2),
    if
        W1 < W2 ->
            counter_value(W2, C2, D);
        true ->
            if
                C2 < C1 ->
                    counter_value(W2 + 1, C2, D);
                true ->
                    counter_value(W2, C2, D)
            end
    end.

-compile({inline, [counter_value/3]}).
counter_value(0, C, _D) -> C;
counter_value(W, C, #{num_cnt_bits := NumCntBits}) when is_integer(W) ->
    (W bsl NumCntBits) + C.


%% -------
%% General helpers
%%

-compile({inline, [reverse/1]}).
reverse([]) -> [];
reverse([_] = L) -> L;
reverse([A, B]) -> [B, A];
reverse(L) -> lists:reverse(L).

-compile({inline, [reverse/2]}).
reverse([], L) -> L;
reverse([A], L) -> [A | L];
reverse([A, B], L) -> [B, A | L];
reverse(L1, L2) -> lists:reverse(L1, L2).

-ifdef(undefined).
%% Reverse but allow improper list
reverse_improper([H | T], Acc) ->
    reverse_improper(T, [H | Acc]);
reverse_improper([], Acc) -> Acc;
reverse_improper(T, Acc) -> [T | Acc].
-endif.


is_map_keys([], #{}) -> false;
is_map_keys([Key|Keys], Map) ->
    is_map_key(Key, Map) orelse is_map_keys(Keys, Map).


%% -------------------------------------------------------------------------
-ifdef(undefined).

%% Better, Leaner, Faster, Smarter, than inet:timeout

end_time(infinity) -> infinity;
end_time(Timeout) when is_integer(Timeout), 0 =< Timeout ->
    erlang:monotonic_time(millisecond) + Timeout.

timeout(infinity) -> infinity;
timeout(EndTime) ->
    Time = erlang:monotonic_time(millisecond),
    if
        Time < EndTime ->
            EndTime - Time;
        true -> 0
    end.

-endif. % -ifdef(undefined).


%% -------------------------------------------------------------------------

error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

warning_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

error_report(Report) ->
    error_logger:error_report(Report).

%% warning_report(Report) ->
%%     error_logger:warning_report([{module, ?MODULE}|Report]).



%% -------------------------------------------------------------------------

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(TS) ->
%%     megaco:format_timestamp(TS).

%% d(F) ->
%%     d(F, []).

%% d(F, A) ->
%%     io:format("*** [~s] ~p ~w " ++ F ++ "~n",
%%               [formated_timestamp(), self(), ?MODULE | A]).
