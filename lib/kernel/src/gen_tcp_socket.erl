%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

%% -define(DBG(T),
%% 	erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).


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
    {StartOpts, Opts} = split_start_opts(Opts0),
    ErrRef = make_ref(),
    try
	%% ?DBG(['try getaddrs']),
        IPs = val(ErrRef, Mod:getaddrs(Address, Timer)),
	%% ?DBG(['try getserv']),
        TP  = val(ErrRef, Mod:getserv(Port)),
	%% ?DBG(['process connect options']),
        CO  = val(ErrRef, inet:connect_options(Opts, Mod)),
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
            ExtraOpts = extra_opts(Fd),
            connect_open(
              Addrs, Domain, ConnectOpts, StartOpts, ExtraOpts,
              Timer, BindSockaddr)
    catch
        throw : {ErrRef, Reason} ->
            ?badarg_exit({error, Reason})
    end.

connect_open(
  Addrs, Domain, ConnectOpts, StartOpts, ExtraOpts, Timer, BindAddr) ->
    %%
    %% The {netns, File} option is passed in Fd by inet:connect_options/2,
    %% and then over to ExtraOpts.
    %%
    case
        start_server(
          Domain,
	  [{timeout, inet:timeout(Timer)} | StartOpts],
	  ExtraOpts)
    of
        {ok, Server} ->
	    %% ?DBG(['server started', {server, Server}]),
            ErrRef = make_ref(),
            try
                try_setopts(ErrRef, Server, StartOpts, ConnectOpts),
                try_bind(ErrRef, Server, Domain, BindAddr, ExtraOpts),
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
        {ok, _Socket}    -> Result;
        {error, badarg}  -> Result;
        {error, einval}  -> Result;
        {error, timeout} -> Result;
        {error, _} ->
	    %% ?DBG([{addr, Addr}, {result, Result}]),
            connect_loop(Addrs, Server, Result, Timer)
    end.


extra_opts(Fd) when is_integer(Fd) ->
    if
        Fd < 0 ->
            #{};
        true ->
            #{fd => Fd}
    end;
extra_opts(OpenOpts) when is_list(OpenOpts) ->
    %% This is an **ugly** hack.
    %% inet:{connect,listen,udp,sctp}_options/2 has the bad taste
    %% to use this for [{netns,BinNS}] if that option is used...
   maps:from_list(OpenOpts).


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

listen(Port, Opts) ->
    %% ?DBG([{port, Port}, {opts, Opts}]),
    Opts_1              = internalize_setopts(Opts),
    %% ?DBG([{opts_1, Opts_1}]), 
   {Mod, Opts_2}       = inet:tcp_module(Opts_1),
    %% ?DBG([{mod, Mod}, {opts_2, Opts_2}]),
    {StartOpts, Opts_3} = split_start_opts(Opts_2),
    %% ?DBG([{start_opts, StartOpts}, {opts_3, Opts_3}]),
    case Mod:getserv(Port) of
        {ok, TP} ->
            %% ?DBG([{tp, TP}]),
            case inet:listen_options([{port, TP} | Opts_3], Mod) of
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
                    ExtraOpts = extra_opts(Fd),
                    %% ?DBG([{extra_opts, ExtraOpts}]),
                    listen_open(
                      Domain, ListenOpts, StartOpts, ExtraOpts,
                      Backlog, BindSockaddr)
            end;
        {error, _} = Error ->
            ?badarg_exit(Error)
    end.


%% Helpers -------

listen_open(Domain, ListenOpts, StartOpts, ExtraOpts, BackLog, BindAddr) ->
    %% ?DBG(['start server',
    %%       {listen_opts, ListenOpts},
    %%       {start_opts,  StartOpts},
    %%       {extra_opts,  ExtraOpts}]),
    case
        start_server(Domain, [{timeout, infinity} | StartOpts], ExtraOpts)
    of
        {ok, Server} ->
            %% ?DBG([{server, Server}]),
            ErrRef = make_ref(),
            try
                case os:type() of
                    {win32, nt} ->
                        %% On *Windows*
                        %% we need to bind before everything else...
                        try_bind(ErrRef, Server, Domain, BindAddr, ExtraOpts),
                        try_setopts(ErrRef, Server, StartOpts, ListenOpts),
                        Socket = try_listen(ErrRef, Server, BackLog),
                        MSock  = ?MODULE_socket(Server, Socket),
                        %% ?DBG(['done', {msock, MSock}]),
                        {ok, MSock};

                    _ ->
                        try_setopts(ErrRef, Server, StartOpts, ListenOpts),
                        try_bind(ErrRef, Server, Domain, BindAddr, ExtraOpts),
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


try_bind(ErrRef, Server, Domain, BindAddr0, ExtraOpts) ->
    %% ?DBG(['process bind-address',
    %%       {domain,     Domain},
    %%       {bind_addr0, BindAddr0},
    %%       {extra_opts, ExtraOpts}]),
    BindAddr1 = default_any(Domain, BindAddr0, ExtraOpts),
    %% ?DBG(['try bind', {bind_addr1, BindAddr1}]),
    ok(ErrRef, call_bind(Server, BindAddr1)).

try_setopts(ErrRef, Server, StartOpts, OperationOpts) ->
    %% ?DBG(['process options',
    %%       {start_opts,     StartOpts},
    %%       {operation_opts, listenOpts}]),
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
         #{packet := Packet,
           send_timeout := SendTimeout} = Meta} ->
            if
                Packet =:= 1;
                Packet =:= 2;
                Packet =:= 4 ->
                    Size = iolist_size(Data),
		    %% ?DBG([{packet, Packet}, {data_size, Size}]),
                    Header = <<?header(Packet, Size)>>,
                    Header_Data = [Header, Data],
                    Result = socket_send(Socket, Header_Data, SendTimeout),
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
        {error, Reason} ->
            %% ?DBG(['send failure', {reason, Reason}]),
            case Reason of
                econnreset ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> Result;
                        false -> {error, closed}
                    end;
                {completion_status, #{info := econnreset = R}} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, R};
                        false -> {error, closed}
                    end;
		{completion_status, econnreset = R} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, R};
                        false -> {error, closed}
                    end;
                #{info := econnreset = R} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, R};
                        false -> {error, closed}
                    end;

		%% Shall we really use (abuse) the show_econnreset option?
                {completion_status, #{info := econnaborted}} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;
		{completion_status, econnaborted} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;
                #{info := econnaborted} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;
                econnaborted ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;

                {completion_status, #{info := netname_deleted}} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;
		{completion_status, netname_deleted} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;
                #{info := netname_deleted} ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;
                netname_deleted ->
                    case maps:get(show_econnreset, Meta) of
                        true  -> {error, econnreset};
                        false -> {error, closed}
                    end;

                {completion_status, #{info := too_many_cmds}} ->
		    {error, closed};
		{completion_status, too_many_cmds} ->
		    {error, closed};
                #{info := too_many_cmds} ->
		    {error, closed};
                too_many_cmds ->
		    {error, closed};

                {timeout = R, RestData} when is_binary(RestData) ->
                    %% To handle RestData we would have to pass
                    %% all writes through a single process that buffers
                    %% the write data, which would be a bottleneck.
                    %%
                    %% For send_timeout_close we have to waste RestData.
                    %%
		    %% ?DBG(['timeout with restdata',
		    %% 	  {restdata_size, byte_size(RestData)}]),
                    case maps:get(send_timeout_close, Meta) of
                        true ->
                            close_server(Server),
                            {error, R};
                        false ->
                            Result
                    end;
                timeout ->
                    %% No data was sent.
                    %%
                    %% Return all data to the user as RestData.
                    %% For packet modes (inserted header);
                    %% the user will have to switch to raw packet
                    %% mode to retransmit RestData since at least
                    %% part of the packet header has been transmitted
                    %% and inserting a new packet header into the
                    %% stream would be dead wrong.
                    %%
		    %% ?DBG(['timeout']),
                    case maps:get(send_timeout_close, Meta) of
                        true ->
                            close_server(Server),
                            Result;
                        false ->
                            {error, {Reason, iolist_to_binary(Data)}}
                    end;

                _ ->
                    ?badarg_exit(Result)
            end;
        ok ->
            ok
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

recv(?MODULE_socket(Server, _Socket), Length, Timeout) ->
    ?badarg_exit(call(Server, {recv, Length, Timeout})).

%% -------------------------------------------------------------------------

shutdown(?MODULE_socket(Server, _Socket), How) ->
    %% ?DBG({shutdown, How}),
    Result = call(Server, {shutdown, How}),
    %% ?DBG({shutdown_result, Result}),
    ?badarg_exit(Result).

%% -------------------------------------------------------------------------

close(?MODULE_socket(Server, _Socket)) ->
    ?badarg_exit(close_server(Server)).

%% Helpers -------

close_server(Server) ->
    Result = call(Server, close),
    stop_server(Server),
    Result.

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

setopts(?MODULE_socket(Server, _Socket), Opts) when is_list(Opts) ->
    try
        begin
            call(Server, {setopts, internalize_setopts(Opts)})
        end
    catch
        exit:badarg ->
            {error, einval}
    end.


%% -------------------------------------------------------------------------

getopts(?MODULE_socket(Server, _Socket), Opts) when is_list(Opts) ->
    try
        begin
            call(Server, {getopts, internalize_getopts(Opts)})
        end
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
    case call(Server, info) of
	{error, closed} ->
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

fdopen(Fd, Opts) when is_integer(Fd), 0 =< Fd, is_list(Opts) ->
    Opts_1 = internalize_setopts(Opts),
    {Mod, Opts_2} = inet:tcp_module(Opts_1),
    Domain = domain(Mod),
    {StartOpts, Opts_3} = split_start_opts(Opts_2),
    ExtraOpts = extra_opts(Fd),
    case
        start_server(Domain, [{timeout, infinity} | StartOpts], ExtraOpts)
    of
        {ok, Server} ->
            ErrRef = make_ref(),
            try
                Setopts =
                    [{start_opts, StartOpts} | setopts_opts(ErrRef, Opts_3)],
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
    Result = socket:send(Socket, Data, Timeout),
    case Result of
        {error, {timeout = _Reason, RestData}} = E when is_binary(RestData) ->
	    %% This is better then closing the socket for every timeout
	    %% We need to do something about this!
	    %% ?DBG({timeout, byte_size(RestData)}),
	    %% {error, Reason};
	    E;
        {error, {_Reason, RestData}} when is_binary(RestData) ->
            %% To properly handle RestData we would have to pass
            %% all writes through a single process that buffers
            %% the write data, which would be a bottleneck
            %%
            %% Since send data may have been lost, and there is no room
            %% in this API to inform the caller, we at least close
            %% the socket in the write direction
	    %% ?DBG({_Reason, byte_size(RestData)}),
            {error, econnreset};
        {error, Reason} ->
	    %% ?DBG(Reason),
            {error,
             case Reason of
                 epipe -> econnreset;
                 _     -> Reason
             end};

        {ok, RestData} when is_binary(RestData) ->
            %% Can not happen for stream socket, but that
            %% does not show in the type spec
            %% - make believe a fatal connection error
	    %% ?DBG({ok, byte_size(RestData)}),
            {error, econnreset};

        ok ->
            ok
    end.

-compile({inline, [socket_recv_peek/2]}).
socket_recv_peek(Socket, Length) ->
    Options = [peek],
    Result = socket:recv(Socket, Length, Options, nowait),
    %% ?DBG({Socket, Length, Options, Result}),
    Result.
-compile({inline, [socket_recv/2]}).
socket_recv(Socket, Length) ->
    Result = socket:recv(Socket, Length, nowait),
    %% ?DBG({Socket, Length, Result}),
    Result.

-compile({inline, [socket_close/1]}).
socket_close(Socket) ->
    %% XXX Should we set the meta option to closed here,
    %% for the send operation to detect without calling
    %% the NIF???
    case socket:close(Socket) of
        ok -> ok;
        {error, closed} -> ok
    end.

-compile({inline, [socket_cancel/2]}).
socket_cancel(Socket, SelectInfo) ->
    case socket:cancel(Socket, SelectInfo) of
        ok                 -> ok;
        {error, closed}    -> ok;
        {error, _} = ERROR -> ERROR

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

-compile({inline, [server_read_write_opts/0]}).
server_read_write_opts() ->
    %% Common for read and write side
    #{packet          => raw,
      packet_size     => 16#4000000, % 64 MByte
      show_econnreset => false}.
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
        %% XXX not implemented yet
        exit_on_close => true},
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


%%% ========================================================================
%%% State Machine
%%%

%% State Machine Engine Call Interface

%% Start for connect or listen - create a socket
start_server(Domain, StartOpts, ExtraOpts) ->
    %% ?DBG([{domain, Domain}, {start_opts, StartOpts}, {extra_opts, ExtraOpts}]),
    Owner = self(),
    Arg   = {open, Domain, ExtraOpts, Owner},
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

call(Server, Call) ->
    try gen_statem:call(Server, Call)
    catch
        exit:{noproc, {gen_statem, call, _Args}} -> {error, closed};
        exit:{{shutdown, _}, _}                  -> {error, closed};
        C:E:S ->
            error_msg("~w call failed: "
                      "~n      Call:  ~p"
                      "~n      Class: ~p"
                      "~n      Error: ~p"
                      "~n      Stack: ~p", [?MODULE, Call, C, E, S]),
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

%% 'connect' % A listen socket stays here
-record(connect,
        {info :: socket:select_info() | socket:completion_info(),
         from :: gen_statem:from(),
         addr :: socket:sockaddr()}).

%% 'connected'
-record(recv,
        {info :: socket:select_info() | socket:completion_info()}).

%% 'closed_read' | 'closed_read_write'
%% 'closed' % Socket is closed or not created


-record(params,
        {socket    :: undefined | socket:socket(),
         owner     :: pid(),
         owner_mon :: reference()}).

init({open, Domain, ExtraOpts, Owner}) ->
    %% Listen or Connect
    %%

    %% ?DBG([{init, open},
    %%   	  {domain, Domain}, {extraopts, ExtraOpts}, {owner, Owner}]),

    process_flag(trap_exit, true),
    OwnerMon  = monitor(process, Owner),
    Extra = #{}, % #{debug => true},
    case socket_open(Domain, ExtraOpts, Extra) of
        {ok, Socket} ->
	    %% ?DBG(['open success', {socket, Socket}]),
            D  = server_opts(),
            ok = socket:setopt(Socket, {otp,iow}, true),
            %%
            %% meta(server_opts()) is an expensive way to write
            %% server_write_opts(), so, meta(D) is redundant code
            %% until someone decides to change D
            ok = socket:setopt(Socket, {otp,meta}, meta(D)),
            P  =
                #params{
                   socket    = Socket,
                   owner     = Owner,
                   owner_mon = OwnerMon},
            {ok, connect, {P, D#{type => undefined, buffer => <<>>}}};
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
    P        = #params{owner     = Owner,
                       owner_mon = OwnerMon},
    {ok, accept, {P, D#{type => undefined, buffer => <<>>}}};
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


terminate(_Reason, State, {_P, _} = P_D) ->
    %% ?DBG({_P#params.socket, State, _Reason}),
    case State of
        #controlling_process{state = OldState} ->
            terminate(OldState, P_D);
        _ ->
            terminate(State, P_D)
    end.
%%
terminate(State, {#params{socket = Socket} = P, D}) ->
    %% ?DBG({Socket, State}),
    case State of
        'closed' -> ok;
        'closed_read' ->
            _ = socket_close(Socket),
            ok;
        'closed_read_write' ->
            _ = socket_close(Socket),
            ok;
        _ ->
            case State of
                'accept' -> ok;
                #accept{} -> ok;
                _ ->
                    _ = socket_close(Socket),
                    ok
            end,
            {_D_1, ActionsR} =
                case State of
                    #controlling_process{state = OldState} ->
                        cleanup_close_read(P, D, OldState, closed);
                    _ ->
                        cleanup_close_read(P, D, State, closed)
                end,
            [gen_statem:reply(Reply)
             || {reply, _From, _Msg} = Reply <- reverse(ActionsR)],
            ok
    end,
    void.

%% -------------------------------------------------------------------------
%% Helpers

%% Construct a "socket" as in this module's API
module_socket(#params{socket = Socket}) ->
    ?MODULE_socket(self(), Socket).

%% -------------------------------------------------------------------------
%% Event Handler (callback)

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

%% Any state:

%% Call: get_server_opts/0
handle_event({call, From}, get_server_opts, _State, {_P, D}) ->
    ServerData = maps:with(maps:keys(server_opts()), D),
    {keep_state_and_data,
     [{reply, From, {ok, ServerData}}]};

%% Event: Owner 'DOWN'
handle_event(
  info, {'DOWN', OwnerMon, _, _, Reason}, _State,
  {#params{owner_mon = OwnerMon} = _P, _D} = P_D) ->
    %%
    {stop, {shutdown, Reason}, P_D};

%% Event: ?socket_counter_wrap/2
handle_event(
  info, ?socket_counter_wrap(Socket, Counter),
  'connected' = _State, {#params{socket = Socket} = P, D}) ->
    %% ?DBG([{state, _State}, {counter, Counter}]),
    {keep_state, {P, wrap_counter(Counter, D)}};
handle_event(
  info, ?socket_counter_wrap(Socket, Counter),
  #recv{} = _State, {#params{socket = Socket} = P, D}) ->
    %% ?DBG([{state, _State}, {counter, Counter}]),
    {keep_state, {P, wrap_counter(Counter, D)}};
handle_event(
  info, ?socket_counter_wrap(_Socket, _Counter), _State, _P_D) ->
    %% ?DBG([{state, _State}, {counter, _Counter}]),
    {keep_state_and_data,
     [postpone]};

%% Call: controlling_process/1
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
%%
%% State: #controlling_process{}
%%
%% Call: controlling_process/0
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
%% Handled state: #controlling_process{}

%% Call: close/0
handle_event({call, From}, close, State, {P, D} = P_D) ->
    %% ?DBG({P#params.socket, State}),
    case State of
        'closed_read' ->
            {next_state, 'closed', P_D,
             [{reply, From, socket_close(P#params.socket)}]};
        'closed_read_write' ->
            {next_state, 'closed', P_D,
             [{reply, From, socket_close(P#params.socket)}]};
        'closed' ->
            {keep_state_and_data,
             [{reply, From, ok}]};
        _ ->
            next_state(
              P, cleanup_close_read(P, D#{active := false}, State, closed),
              'closed',
              [{reply, From, socket_close(P#params.socket)}])
    end;

%% Call: getopts/1
handle_event({call, From}, {getopts, Opts}, State, {P, D}) ->
    %% ?DBG([{opts, Opts}, {state, State}, {d, D}]),
    Result = case state_getopts(P, D, State, Opts) of
                 {ok, OptVals} ->
                     %% ?DBG([{opt_vals, OptVals}]),
                     {ok, externalize_getopts(OptVals)};
                 {error, _} = ERROR ->
                     ERROR
             end,
    %% ?DBG([{result, Result}]),
    {keep_state_and_data,
     [{reply, From, Result}]};

%% Call: setopts/1
handle_event({call, From}, {setopts, Opts}, State, {P, D}) ->
    %% ?DBG([{setopts, Opts}, {state, State}, {d, D}]),
    {Result_1, D_1} = state_setopts(P, D, State, Opts),
    %% ?DBG([{result, Result_1}, {d1, D_1}]),
    Result =
        case Result_1 of
            {error, enoprotoopt} ->
                %% If we get this error, the options is not valid for
                %% this (tcp) protocol.
                _ = socket:setopt(P#params.socket, {otp,meta}, meta(D_1)),
                {error, einval};

            {error, {invalid, _}} ->
                %% If we get this error, the options where crap.
                _ = socket:setopt(P#params.socket, {otp,meta}, meta(D_1)),
                {error, einval};

            {error, einval} ->
                %% If we get this error, either the options where crap or
                %% the socket is in a "bad state" (maybe its closed).
                %% So, if that is the case we accept that we may not be
                %% able to update the meta data.
                _ = socket:setopt(P#params.socket, {otp,meta}, meta(D_1)),
                Result_1;
            _ ->
                %% We should really handle this better. stop_and_reply?
                ok = socket:setopt(P#params.socket, {otp,meta}, meta(D_1)),
                Result_1
        end,
    Reply = {reply, From, Result},

    %% If the socket is deactivated; active: once | true | N > 0 -> false
    %% we do not cancel any select! Data that arrive during the phase when
    %% we are in state 'recv' but are inactive is simply stored in the buffer.
    %% If activated: active: false -> once | true | N > 0
    %% We need to check if there is something in our buffers, and maybe deliver
    %% it to its owner. This is what we do here. This should only occur
    %% if we are in state connected (state 'recv' and in-active when data
    %% arrives => put data in buffer and then enter state 'connected', since
    %% we are in-active).
    case State of
        'connected' ->
            handle_connected(P, handle_buffered(P, D_1), [Reply]);
        _ ->
            {keep_state, {P, D_1}, [Reply]}
    end;

%% Call: getstat/2
handle_event({call, From}, {getstat, What}, State, {P, D}) ->
    case State of
        'closed' ->
            {keep_state_and_data,
             [{reply, From, {error, closed}}]};
        _ ->
            {D_1, Result} = getstat(P#params.socket, D, What),
            {keep_state, {P, D_1},
             [{reply, From, {ok, Result}}]}
    end;

%% Call: info/1
handle_event({call, From}, info, State, {P, D}) ->
    case State of
        'closed' ->
            {keep_state_and_data,
             [{reply, From, ?CLOSED_SOCKET}]};
        _ ->
            {D_1, Result} = handle_info(P#params.socket, P#params.owner, D),
            {keep_state, {P, D_1},
             [{reply, From, Result}]}
    end;

%% State: 'closed' - what is not handled above
handle_event(Type, Content, 'closed' = State, P_D) ->
    handle_closed(Type, Content, State, P_D);
%% Handled state: 'closed'

%% Call: shutdown/1
handle_event({call, From}, {shutdown, How} = _SHUTDOWN, State, {P, D}) ->
    %% ?DBG({P#params.socket, _SHUTDOWN, State}),
    case State of
        'closed_read' when (How =:= read) ->
            %% ?DBG('already closed-read'),
            {keep_state_and_data,
             [{reply, From, ok}]};
        'closed_read_write' when (How =:= read_write) ->
            %% ?DBG('already closed-read-write'),
            {keep_state_and_data,
             [{reply, From, ok}]};
        _ ->
            %% ?DBG({'handle shutdown', How, State}),
            case handle_shutdown(P, State, How) of
                {keep, SRes} ->
                    %% ?DBG({'shutdown result', SRes, keep}),
                    {keep_state_and_data,
                     [{reply, From, SRes}]};
                {NextState, SRes} ->
                    %% ?DBG({P#params.socket, 'shutdown result', SRes, NextState}),
                    next_state(
                      P,
                      cleanup_close_read(P, D#{active := false}, State, closed),
                      NextState,
                      [{reply, From, SRes}])
            end
    end;
%% State: 'closed_read' | 'closed_read_write' - what is not handled in
%%        close/0 and shutdown/1 above
handle_event(Type, Content, State, P_D)
  when (State =:= 'closed_read') orelse (State =:= 'closed_read_write') ->
    handle_closed(Type, Content, State, P_D);


%% State: 'accept'
handle_event(
  {call, From}, {accept, ListenSocket, Timeout},
  'accept' = _State, {P, D}) ->
    handle_accept(P, D, From, ListenSocket, Timeout, accept);
handle_event(Type, Content, 'accept' = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);
%%
%% State: #accept{}
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
  {P, D}) ->
    {next_state, 'closed', {P, D},
     [{reply, From, {error, Reason}}]};
handle_event(
  info, ?socket_abort(ListenSocket, CompletionRef, Reason),
  #accept{
     info = ?completion_info(CompletionRef), from = From,
     listen_socket = ListenSocket},
  {P, D}) ->
    {next_state, 'closed', {P, D},
     [{reply, From, {error, Reason}}]};
handle_event(
  {timeout, accept}, accept,
  #accept{
     info = SelectInfo, from = From,
     listen_socket = ListenSocket},
  {P, D}) ->
    _ = socket_cancel(ListenSocket, SelectInfo),
    {next_state, 'closed', {P, D},
     [{reply, From, {error, timeout}}]};
handle_event(Type, Content, #accept{} = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);
%% Handled states: 'accept' | #accept{}

%% ------- Socket is defined from here on -----------------------------------

%% Call: bind/1
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

%% Call: listen/1
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

%% Call: recv/2 - active socket
handle_event(
  {call, From}, {recv, _Length, _Timeout},
  _State, {_P, #{active := Active} = _D})
  when Active =/= false ->
    {keep_state_and_data,
     [{reply, From, {error, einval}}]};

%% State: 'connect'
%%
%% Call: connect/2
handle_event(
  {call, From}, {connect, Addr, Timeout}, 'connect' = _State, {P, D}) ->
    handle_connect(P, D, From, Addr, Timeout, connect);
%%
%% Call: recv/2 - not connected
handle_event(
  {call, From}, {recv, _Length, _Timeout}, 'connect' = _State, _P_D) ->
    {keep_state_and_data,
     [{reply, From, {error, enotconn}}]};
%% Call: fdopen/2
handle_event(
  {call, From}, fdopen, 'connect' = _State,
  {#params{socket = Socket} = P, D}) ->
    handle_connected(
      P, D#{type => fdopen},
      [{reply, From, {ok, Socket}}]);
handle_event(Type, Content, 'connect' = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);

%%
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
  {#params{socket = Socket} = _P, _D} = P_D) ->
    %% ?DBG(['abort message',
    %% 	  {ref, SelectRef}, {reason, Reason}]),
    _ = socket_close(Socket),
    {next_state, 'closed', P_D,
     [{reply, From, {error, Reason}}]};

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
  {#params{socket = Socket} = _P, _D} = P_D) ->
    %% ?DBG(['abort message',
    %% 	  {ref, CompletionRef}, {reason, Reason}]),
    _ = socket_close(Socket),
    NewReason = case Reason of
                    {completion_status, #{info := netname_deleted}} ->
                        closed;
                    {completion_status, netname_deleted} ->
                        closed;
                    {completion_status, #{info := INFO}} ->
                        INFO;
                    {completion_status, INFO} ->
                        INFO;
                    _ ->
                        Reason
                end,
    {next_state, 'closed', P_D,
     [{reply, From, {error, NewReason}}]};

handle_event(
  {timeout, connect}, connect,
  #connect{info = SelectInfo, from = From},
  {#params{socket = Socket} = _P, _D} = P_D) ->
    _ = socket_cancel(Socket, SelectInfo),
    _ = socket_close(Socket),
    {next_state, 'closed', P_D,
     [{reply, From, {error, timeout}}]};
%%
%% Call: recv/2 - not connected
handle_event(
  {call, From}, {recv, _Length, _Timeout}, #connect{} = _State, _P_D) ->
    {keep_state_and_data,
     [{reply, From, {error, enotconn}}]};
handle_event(Type, Content, #connect{} = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);
%% Handled states: 'connect' | #connect{}



%% Remaining states: 'connected' | #recv{}

%% Call: recv/2 - last part
handle_event(
  {call, From}, {recv, Length, Timeout}, State, {P, D}) ->
    %% ?DBG([recv, {length, Length}, {timeout, Timeout}, {state, State}]),
    case State of
        'connected' ->
            handle_recv_start(P, D, From, Length, Timeout);
        #recv{} ->
            %% Receive in progress
            {keep_state_and_data,
             [postpone]}
    end;

%% State: #recv{}
%%
%% Handle select done - try recv again
handle_event(
  info, ?socket_select(Socket, SelectRef),
  #recv{info = ?select_info(SelectRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG([info, {socket, Socket}, {ref, SelectRef}]),
    handle_recv(P, D, [], recv);
%%
handle_event(
  info, ?socket_abort(Socket, SelectRef, Reason),
  #recv{info = ?select_info(SelectRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG({abort, Reason}),
    handle_connected(P, cleanup_recv_reply(P, D, [], Reason));

%%
%% Handle completion done
handle_event(
  info, ?socket_completion(Socket, CompletionRef, CompletionStatus),
  #recv{info = ?completion_info(CompletionRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['completion msg', {socket, Socket}, {ref, CompletionRef}]),
    handle_recv(P, D, [], CompletionStatus);
%%
handle_event(
  info, ?socket_abort(Socket, CompletionRef, Reason),
  #recv{info = ?completion_info(CompletionRef)} = _State,
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['abort msg', {reason, Reason}]),
    NewReason = case Reason of
                    {completion_status, #{info := netname_deleted}} ->
                        closed;
                    {completion_status, netname_deleted} ->
                        closed;
                    {completion_status, #{info := INFO}} ->
                        INFO;
                    {completion_status, INFO} ->
                        INFO;
                    _ ->
                        Reason
                end,
    handle_connected(P, cleanup_recv_reply(P, D, [], NewReason));

%%
%% Timeout on recv in non-active mode
handle_event(
  {timeout, recv}, recv, #recv{} = State, {P, D}) ->
    %%
    %% ?DBG({timeout, recv}),
    handle_connected(P, cleanup_recv(P, D, State, timeout));

%% Catch-all
handle_event(Type, Content, State, P_D) ->
    handle_unexpected(Type, Content, State, P_D).

%% End of event handler
%% -------------------------------------------------------------------------
%% Event handler helpers


%% We only accept/perform shutdown when socket is 'connected'
%% We only accept/perform shutdown when socket is 'connected'
%% (or closed_read | closed_write).
%% This is done to be "compatible" with the inet-driver!

handle_shutdown(#params{socket = Socket},
                closed_write = _State,
                read = How) ->
    handle_shutdown2(Socket, closed_read_write, How);
handle_shutdown(#params{socket = Socket},
                closed_read = _State,
                write = How) ->
    handle_shutdown2(Socket, closed_read_write, How);
handle_shutdown(#params{socket = Socket},
                connected = _State,
                write = How) ->
    {keep, socket:shutdown(Socket, How)};
handle_shutdown(#params{socket = Socket},
                #recv{} = _State,
                write = How) ->
    {keep, socket:shutdown(Socket, How)};
handle_shutdown(#params{socket = Socket},
                connected = _State,
                read = How) ->
    handle_shutdown2(Socket, closed_read, How);
handle_shutdown(#params{socket = Socket},
                #recv{} = _State,
                read = How) ->
    handle_shutdown2(Socket, closed_read, How);
handle_shutdown(#params{socket = Socket},
                connected = _State,
                read_write = How) ->
    handle_shutdown2(Socket, closed_read_write, How);
handle_shutdown(#params{socket = Socket},
                #recv{} = _State,
                read_write = How) ->
    handle_shutdown2(Socket, closed_read_write, How);
handle_shutdown(_Params, _State, _How) ->
    {keep, {error, enotconn}}.

handle_shutdown2(Socket, NextState, How) ->
    case socket:shutdown(Socket, How) of
        ok ->
            {NextState, ok};
        Error ->
            {keep, Error}
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

handle_closed(Type, Content, State, {P, _D}) ->
    case Type of
        {call, From} ->
            {keep_state_and_data,
             [{reply, From, {error, closed}}]};
        _ ->
            warning_msg("Received unexpected event when closed:"
                        "~n   Socket:     ~p"
                        "~n   State:      ~p"
                        "~n   Event Type: ~p"
                        "~n   Content:    ~p",
                        [P#params.socket, State, Type, Content]),
            keep_state_and_data
    end.

%% State transition helpers -------

handle_connect(
  #params{socket = Socket} = P, D, From, Addr, Timeout, Status)
  when (Status =:= connect) ->
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
             [{{timeout, connect}, Timeout, connect}]};

        {completion, ?completion_info(_) = Info} ->
	    %% _ = socket:setopt(Socket, otp, debug, false),
	    %% ?DBG(['completion info']),
            {next_state,
             #connect{info = Info, from = From, addr = Addr},
             {P, D#{type => connect}},
             [{{timeout, connect}, Timeout, connect}]};

        {error, _} = Error ->
	    %% _ = socket:setopt(Socket, otp, debug, false),
	    %% ?DBG(['connect failed', {error, Error}]),
            {next_state,
             'connect', {P, D},
             [{{timeout, connect}, cancel},
              {reply, From, Error}]}
    end;
handle_connect(
  #params{socket = Socket} = P, D, From, Addr, Timeout, Status)
  when (Status =:= select) ->
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
             [{{timeout, connect}, Timeout, connect}]};

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
  when (Status =:= select) orelse (Status =:= accept) ->
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
             [{{timeout, accept}, Timeout, accept}]};

        {completion, ?completion_info(_) = CompletionInfo} ->
            %% ?DBG({accept_completion, CompletionInfo}),
            {next_state,
             #accept{
                info = CompletionInfo, from = From,
                listen_socket = ListenSocket},
             {P, D#{type => accept}},
             [{{timeout, accept}, Timeout, accept}]};

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
            handle_recv(P, recv_start(D), ActionsR, recv)
    end.

handle_recv_start(
  P, #{packet := Packet, buffer := Buffer} = D, From, Length, Timeout)
  when Packet =:= raw, 0 < Length;
       Packet =:= 0, 0 < Length ->
    Size = iolist_size(Buffer),
    %% ?DBG([{packet, Packet}, {length, Length}, {buf_sz, Size}]),
    if
        Length =< Size ->
            {Data, NewBuffer} =
                split_binary(condense_buffer(Buffer), Length),
            handle_recv_deliver(
              P,
              D#{recv_length => Length, % Redundant
                 recv_from => From,
                 buffer := NewBuffer},
              [], Data);
        true ->
            N = Length - Size,
            handle_recv(
              P, D#{recv_length => N, recv_from => From},
              [{{timeout, recv}, Timeout, recv}],
              recv)
    end;
handle_recv_start(P, D, From, _Length, Timeout) ->
    %% ?DBG([{p, P}, {d, D}]),
    handle_recv(
      P, D#{recv_length => 0, recv_from => From},
      [{{timeout, recv}, Timeout, recv}],
      recv).

handle_recv(P, #{packet := Packet, recv_length := Length} = D, ActionsR, CS) ->
    %% ?DBG([{packet, Packet}, {recv_length, Length}]),
    if
        0 < Length ->
            handle_recv_length(P, D, ActionsR, Length, CS);
        Packet =:= raw;
        Packet =:= 0 ->
            handle_recv_length(P, D, ActionsR, Length, CS);
        Packet =:= 1;
        Packet =:= 2;
        Packet =:= 4 ->
            handle_recv_peek(P, D, ActionsR, Packet, CS);
        true ->
            handle_recv_packet(P, D, ActionsR, CS)
    end.

handle_recv_peek(P, D, ActionsR, Packet, CS) ->
    %% Peek Packet bytes
    %% ?DBG({packet, Packet}),
    case D of
        #{buffer := Buffer} when is_list(Buffer) ->
	    %% ?DBG('buffer is list - condence'),
            Data = condense_buffer(Buffer),
            handle_recv_peek(P, D#{buffer := Data}, ActionsR, Packet, CS);
        #{buffer := <<Data:Packet/binary, _Rest/binary>>} ->
	    %% ?DBG('buffer contains header'),
            handle_recv_peek2(P, D, ActionsR, Packet, Data);
        #{buffer := <<ShortData/binary>>} when (CS =:= recv) ->
            N = Packet - byte_size(ShortData),
	    %% ?DBG(['buffer does not contain complete header',
	    %%  	  {cs, CS},
            %%       {packet, Packet}, {n, N},
            %%       {short_data, byte_size(ShortData)}]),
            case socket_recv_peek(P#params.socket, N) of
                {ok, <<FinalData/binary>>} ->
                    handle_recv_peek2(
                      P, D, ActionsR, Packet,
                      <<ShortData/binary, FinalData/binary>>);

                {select, Select} ->
                    {next_state,
                     #recv{
                        info =
                            case Select of
                                {?select_info(_) = SelectInfo, _Data} ->
                                    SelectInfo;
                                ?select_info(_) = SelectInfo ->
                                    SelectInfo
                            end},
                     {P, D},
                     reverse(ActionsR)};

                {completion, Completion} ->
                    {next_state,
                     #recv{info = Completion},
                     {P, D},
                     reverse(ActionsR)};

                {error, {Reason, <<_Data/binary>>}} ->
                    handle_recv_error(P, D, ActionsR, Reason);
                {error, Reason} ->
                    handle_recv_error(P, D, ActionsR, Reason)
            end;
        #{buffer := <<ShortData/binary>>} ->
	    %% ?DBG(['buffer did not contain complete header',
            %%       {cs, CS},
	    %%  	  {packet, Packet},
            %%       {short_data, byte_size(ShortData)}]),
            case CS of
                {ok, <<FinalData/binary>>} ->
                    handle_recv_peek2(
                      P, D, ActionsR, Packet,
                      <<ShortData/binary, FinalData/binary>>);
                {error, {Reason, <<_Data/binary>>}} ->
                    handle_recv_error(P, D, ActionsR, Reason);
                {error, Reason} ->
                    handle_recv_error(P, D, ActionsR, Reason)
            end
    end.

handle_recv_peek2(P, D, ActionsR, Packet, Data) ->
    <<?header(Packet, N)>> = Data,
    #{packet_size := PacketSize} = D,
    %% ?DBG([{'packet size', Packet, N, PacketSize}]),
    if
        0 < PacketSize, PacketSize < N ->
	    %% ?DBG({emsgsize}),
            handle_recv_error(P, D, ActionsR, emsgsize);
        true ->
	    %% ?DBG({'read a message'}),
            handle_recv_length(P, D, ActionsR, Packet + N, recv)
    end.


handle_buffered(_P, #{recv_from := _From} = D) ->
    D;
handle_buffered(P, #{active := Active} = D) when (Active =/= false) ->
    case D of
        #{buffer := Buffer} when is_list(Buffer) andalso (Buffer =/= []) ->
            Data = condense_buffer(Buffer),
            handle_buffered(P, D, Data);
        #{buffer := Data} when is_binary(Data) andalso (byte_size(Data) > 0) ->
            handle_buffered(P, D, Data);
        _ ->
            D
    end;
handle_buffered(_P, D) ->
    D.

handle_buffered(P,
                #{packet         := line,
                  line_delimiter := LineDelimiter,
                  packet_size    := PacketSize} = D,
                Data) ->
    DecodeOpts = [{line_delimiter, LineDelimiter},
		  {line_length,    PacketSize}],
    handle_buffered(P, D, Data, DecodeOpts);
handle_buffered(P, D, Data) ->
    handle_buffered(P, D, Data, []).

handle_buffered(P, #{packet_size := PacketSize} = D,
                Data, DecocdeOpts0) ->
    DecodeOpts = [{packet_size, PacketSize}|DecocdeOpts0], 
    Type       = decode_packet(D),
    case erlang:decode_packet(Type, Data, DecodeOpts) of
        {ok, Decoded, Rest} ->
            D2 = deliver_buffered_data(P, D, Decoded),
            %% Prepare the rest
            %% is_list(Buffer) -> try to decode first
            %% is_binary(Buffer) -> get more data first
            Buffer =
                case Rest of
                    <<>> -> Rest;
                    <<_/binary>> -> [Rest]
                end,
            D2#{buffer := Buffer};
        {more, _} ->
            D;
        {error, Reason} ->
            %% What do we do here?
            %% Keep the buffer and hope that it will go better with more data?
            %% Or discard it and continue as if nothing happened?
            warning_msg("Failed decoding message"
                        "~n   Socket:          ~p"
                        "~n   Socket server:   ~p"
                        "~n   Packet type:     ~p"
                        "~n   byte_size(Data): ~p"
                        "~n   Reason:          ~p",
                        [P#params.socket, self(),
                         Type, byte_size(Data), Reason]),
            D
    end.

%% If we get this far, we *know* that the socket is 'active'.
deliver_buffered_data(#params{owner = Owner} = P,
                      #{active  := Active,
                        mode    := Mode,
                        header  := Header,
                        deliver := Deliver,
                        packet  := Packet} = D, Data) ->
    DeliverData  = deliver_data(Data, Mode, Header, Packet),
    ModuleSocket = module_socket(P),
    Owner !
        case Deliver of
            term ->
                {tag(Packet), ModuleSocket, DeliverData};
            port ->
                {ModuleSocket, {data, DeliverData}}
        end,
    case Active of
        true ->
            recv_start(next_packet(D, Packet, Data));
        once ->
            recv_stop(next_packet(D, Packet, Data, false));
        1 ->
            Owner ! {tcp_passive, ModuleSocket},
            recv_stop(next_packet(D, Packet, Data, false));
        N when is_integer(N) ->
            recv_start(next_packet(D, Packet, Data, Active - 1))
    end.    


handle_recv_packet(P, D, ActionsR, CS) ->
    case D of
        #{buffer := Buffer} when is_list(Buffer) ->
            Data = condense_buffer(Buffer),
            handle_recv_decode(P, D, ActionsR, Data, CS);
        #{buffer := Data} when is_binary(Data) ->
            handle_recv_more(P, D, ActionsR, Data, CS)
    end.

handle_recv_length(P, #{buffer := Buffer} = D, ActionsR, Length, CS) ->
    handle_recv_length(P, D, ActionsR, Length, Buffer, CS).
%%
%% Here and downwards until handle_recv_deliver() all buffered data
%% is the last argument binary and D#{buffer} is not updated
%%
handle_recv_length(P, D, ActionsR, Length, Buffer, CS)
  when (0 < Length) andalso (CS =:= recv) ->
    %% ?DBG(['try socket recv', {length, Length}, {cs, CS}]),
    case socket_recv(P#params.socket, Length) of
        {ok, <<Data/binary>>} ->
            handle_recv_deliver(
              P, D#{buffer := <<>>}, ActionsR,
              condense_buffer([Data | Buffer]));

        {select, {?select_info(_) = SelectInfo, Data}} ->
            N = Length - byte_size(Data),
            {next_state,
             #recv{info = SelectInfo},
             {P, D#{buffer := [Data | Buffer], recv_length := N}},
             reverse(ActionsR)};
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
            %% Error before all data
            %% ?DBG({'recv error w rest-data', Reason, byte_size(Data)}),
            handle_recv_error(
              P, D#{buffer := [Data | Buffer]}, ActionsR, Reason);
        {error, Reason} ->
            %% ?DBG({'recv error wo rest-data', Reason}),
            handle_recv_error(P, D#{buffer := Buffer}, ActionsR, Reason)
    end;
handle_recv_length(P, D, ActionsR, Length, Buffer, CS)
  when (0 < Length) ->
    %% ?DBG(['socket recv result', {cs_result, element(1, CS)}]),
    case CS of
        {ok, <<Data/binary>>} ->
	    %% ?DBG([{received, byte_size(Data)}]),
            handle_recv_deliver(
              P, D#{buffer := <<>>}, ActionsR,
              condense_buffer([Data | Buffer]));

        {error, {Reason, <<Data/binary>>}} ->
            %% Error before all data
            %% ?DBG({'recv error w rest-data', Reason, byte_size(Data)}),
            handle_recv_error(
              P, D#{buffer := [Data | Buffer]}, ActionsR, Reason);

        {error, Reason} ->
            %% ?DBG({'recv error wo rest-data', Reason}),
            handle_recv_error(P, D#{buffer := Buffer}, ActionsR, Reason)
    end;
handle_recv_length(P, D, ActionsR, _0, Buffer, CS) when (CS =:= recv) ->
    %% ?DBG([{buffer_size, byte_size(Buffer)}, {cs, CS}]),
    case Buffer of
        <<>> ->
            %% We should not need to update the buffer field here
            %% since the only way to get here with empty Buffer
            %% is when Buffer comes from the buffer field
            Socket = P#params.socket,
	    %% ?DBG(['try read some more', {buffer_size, byte_size(Buffer)}]),
            case socket_recv(Socket, 0) of
                {ok, <<Data/binary>>} ->
		    %% ?DBG(['got some data', {data_size, byte_size(Data)}]),
                    handle_recv_deliver(P, D, ActionsR, Data);

                {select, {?select_info(_) = SelectInfo, Data}} ->
		    %% ?DBG({'select with data', byte_size(Data)}),
                    case socket:cancel(Socket, SelectInfo) of
                        ok ->
                            handle_recv_deliver(P, D, ActionsR, Data);
                        {error, Reason} ->
                            handle_recv_error(P, D, ActionsR, Reason, Data)
                    end;
                {select, ?select_info(_) = SelectInfo} ->
		    %% ?DBG({'select', SelectInfo}),
                    {next_state,
                     #recv{info = SelectInfo},
                     {P, D},
                     reverse(ActionsR)};

                {completion, ?completion_info(_) = CompletionInfo} ->
		    %% ?DBG(['completion',
		    %% 	  {completion_info, CompletionInfo}]),
                    {next_state,
                     #recv{info = CompletionInfo},
                     {P, D},
                     reverse(ActionsR)};

                {error, {Reason, <<Data/binary>>}} ->
		    %% ?DBG(['error with data',
		    %% 	  {reason, Reason}, {data_size, byte_size(Data)}]),
                    handle_recv_error(P, D, ActionsR, Reason, Data);
                {error, Reason} ->
		    %% ?DBG(['error', {reason, Reason}]),
                    handle_recv_error(P, D, ActionsR, Reason)
            end;
        <<Data/binary>> ->
            handle_recv_deliver(P, D#{buffer := <<>>}, ActionsR, Data);
        _ when is_list(Buffer) ->
            Data = condense_buffer(Buffer),
            handle_recv_deliver(P, D#{buffer := <<>>}, ActionsR, Data)
    end;
handle_recv_length(P, D, ActionsR, _0, Buffer, CS) ->
    %% ?DBG([{buffer, byte_size(Buffer)}, {cs_result, element(1, CS)}]),
    case Buffer of
        <<>> ->
            %% We should not need to update the buffer field here
            %% since the only way to get here with empty Buffer
            %% is when Buffer comes from the buffer field
            case CS of
                {ok, <<Data/binary>>} ->
		    %% ?DBG({'got some', byte_size(Data)}),
                    handle_recv_deliver(P, D, ActionsR, Data);

                {error, Reason} ->
		    %% ?DBG(['error', {reason, Reason}]),
                    handle_recv_error(P, D, ActionsR, Reason)
            end;
        <<_/binary>> ->
            case CS of
                {ok, <<Data/binary>>} ->
		    %% ?DBG(['got some data', {data_size, byte_size(Data)}]),
                    handle_recv_deliver(P, D#{buffer := <<>>}, ActionsR,
                                        condense_buffer([Data, Buffer]));

                {error, Reason} ->
		    %% ?DBG(['error', {reason, Reason}]),
                    handle_recv_error(P, D, ActionsR, Reason)
            end;                
        _ when is_list(Buffer) ->
            case CS of
                {ok, <<Data/binary>>} ->
		    %% ?DBG(['got some data', {data_size, byte_size(Data)}]),
                    handle_recv_deliver(P, D#{buffer := <<>>}, ActionsR,
                                        condense_buffer([Data | Buffer]));

                {error, Reason} ->
		    %% ?DBG(['error', {reason, Reason}]),
                    handle_recv_error(P, D, ActionsR, Reason)
            end
    end.

handle_recv_decode(P,
		   #{packet         := line,
		     line_delimiter := LineDelimiter,
		     packet_size    := PacketSize} = D,
		   ActionsR, Data, CS) ->
    DecodeOpts = [{line_delimiter, LineDelimiter},
		  {line_length,    PacketSize}],
    handle_recv_decode(P, D,
		       ActionsR, Data, DecodeOpts, CS);
handle_recv_decode(P, D, ActionsR, Data, CS) ->
    handle_recv_decode(P, D, ActionsR, Data, [], CS).

handle_recv_decode(P, #{packet_size := PacketSize} = D,
		   ActionsR, Data, DecocdeOpts0, CS) ->
    %% ?DBG([{packet_sz, PacketSize}, {decode_opts0, DecocdeOpts0}, {cs, CS}]),
    DecodeOpts = [{packet_size, PacketSize}|DecocdeOpts0], 
    case erlang:decode_packet(decode_packet(D), Data, DecodeOpts) of
        {ok, Decoded, Rest} ->
            %% ?DBG(['packet decoded', {decoded, Decoded}, {rest, Rest}]),
            %% is_list(Buffer) -> try to decode first
            %% is_binary(Buffer) -> get more data first
            Buffer =
                case Rest of
                    <<>> -> Rest;
                    <<_/binary>> -> [Rest]
                end,
            handle_recv_deliver(P, D#{buffer := Buffer}, ActionsR, Decoded);
        {more, undefined} ->
            %% ?DBG(['more undef']),
            handle_recv_more(P, D, ActionsR, Data, CS);
        {more, Length} ->
            %% ?DBG(['more', {length, Length}]),
            N = Length - byte_size(Data),
            handle_recv_length(P, D, ActionsR, N, Data, CS);
        {error, Reason} ->
            %% ?DBG(['error', {reason, Reason}]),
            handle_recv_error(
              P, D#{buffer := Data}, ActionsR,
              case Reason of
                  invalid -> emsgsize;
                  _ -> Reason
              end)
    end.

handle_recv_error_decode(
  P, #{packet_size := PacketSize} = D, ActionsR, Reason, Data) ->
    %%
    case
        erlang:decode_packet(
          decode_packet(D), Data,
          [{packet_size, PacketSize},
           {line_length, PacketSize}])
    of
        {ok, Decoded, Rest} ->
            %% is_list(Buffer) -> try to decode first
            %% is_binary(Buffer) -> get more data first
            Buffer =
                case Rest of
                    <<>> -> Rest;
                    <<_/binary>> -> [Rest]
                end,
            handle_recv_error(
              P, D#{buffer := Buffer}, ActionsR, Reason, Decoded);
        {more, _} ->
            handle_recv_error(P, D#{buffer := Data}, ActionsR, Reason);
        {error, Reason} ->
            handle_recv_error(
              P, D#{buffer := Data}, ActionsR,
              case Reason of
                  invalid -> emsgsize;
                  _ -> Reason
              end)
    end.

handle_recv_more(P, D, ActionsR, BufferedData, CS) when (CS =:= recv) ->
    case socket_recv(P#params.socket, 0) of
        {ok, <<MoreData/binary>>} ->
	    %% ?DBG([{more_data_sz, byte_size(MoreData)}]), 
	    Data = catbin(BufferedData, MoreData),
            handle_recv_decode(P, D, ActionsR, Data, recv);

        {select, ?select_info(_) = SelectInfo} ->
	    %% ?DBG([{select_info, SelectInfo}]), 
            {next_state,
             #recv{info = SelectInfo},
             {P, D#{buffer := BufferedData}},
             reverse(ActionsR)};

        {completion, ?completion_info(_) = CompletionInfo} ->
	    %% ?DBG([{completion_info, CompletionInfo}]), 
            {next_state,
             #recv{info = CompletionInfo},
             {P, D#{buffer := BufferedData}},
             reverse(ActionsR)};

        {error, {Reason, <<MoreData/binary>>}} ->
            %% ?DBG({P#params.socket, error, Reason, byte_size(MoreData)}),
            Data = catbin(BufferedData, MoreData),
            handle_recv_error_decode(P, D, ActionsR, Reason, Data);
        {error, Reason} ->
            %% ?DBG({P#params.socket, error, Reason}),
            handle_recv_error(
              P, D#{buffer := BufferedData}, ActionsR, Reason)
    end;
handle_recv_more(P, D, ActionsR, BufferedData, CS) ->
    case CS of
        {ok, <<MoreData/binary>>} ->
	    %% ?DBG([{more_data_sz, byte_size(MoreData)}]), 
	    Data = catbin(BufferedData, MoreData),
            handle_recv_decode(P, D, ActionsR, Data, recv);

        {error, Reason} ->
            %% ?DBG({P#params.socket, error, Reason}),
            handle_recv_error(
              P, D#{buffer := BufferedData}, ActionsR, Reason)
    end.

%% Here D#{buffer} is supposed to be updated again

handle_recv_deliver(P, D, ActionsR, Data) ->
    handle_connected(P, recv_data_deliver(P, D, ActionsR, Data)).

handle_recv_error(P, D, ActionsR, Reason, Data) ->
    %% Deliver, then error
    {D_1, ActionsR_1} = recv_data_deliver(P, D, ActionsR, Data),
    handle_recv_error(P, D_1, ActionsR_1, Reason).
%%
handle_recv_error(P, D, ActionsR, Reason) ->
    %% ?DBG({P#params.socket, Reason}),
    {D_1, ActionsR_1} =
        cleanup_recv_reply(P, D#{buffer := <<>>}, ActionsR, Reason),
    if
        Reason =:= timeout;
        Reason =:= emsgsize ->
            {next_state, 'connected',
             {P, recv_stop(D#{active := false})},
             reverse(ActionsR_1)};
        Reason =:= closed ->
            %% This may be incorrect with respect to inet_drv.c:s
            %% default exit_on_close behaviour...
            {next_state, 'closed_read', {P, D_1}, reverse(ActionsR_1)};
        true ->
            _ = socket_close(P#params.socket),
            {next_state, 'closed', {P, D_1}, reverse(ActionsR_1)}
    end.

%% -------------------------------------------------------------------------
%% Callback Helpers

next_state(P, {D, ActionsR}, State, Actions) ->
    {next_state, State, {P, D}, reverse(ActionsR, Actions)}.

cleanup_close_read(P, D, State, Reason) ->
    %% ?DBG({P#params.socket, State, Reason}),    
    case State of
        #accept{
           info = SelectInfo, from = From, listen_socket = ListenSocket} ->
            _ = socket_cancel(ListenSocket, SelectInfo),
            {D,
             [{reply, From, {error, Reason}}]};
        #connect{info = Info, from = From} ->
            _ = socket_cancel(P#params.socket, Info),
            {D,
             [{reply, From, {error, Reason}}]};
        _ ->
            cleanup_recv(P, D, State, Reason)
    end.

cleanup_recv(P, D, State, Reason) ->
    %% ?DBG({P#params.socket, State, Reason}),    
    case State of
        #recv{info = Info} ->
            _ = socket_cancel(P#params.socket, Info),
            cleanup_recv_reply(P, D, [], Reason);
        _ ->
            cleanup_recv_reply(P, D, [], Reason)
    end.

cleanup_recv_reply(
  P, #{show_econnreset := ShowEconnreset} = D, ActionsR, Reason) ->
    %% ?DBG({ShowEconnreset, Reason}),
    case D of
        #{active := false} -> ok;
        #{active := _} ->
            ModuleSocket = module_socket(P),
            Owner = P#params.owner,
            %% ?DBG({ModuleSocket, {Reason,ShowEconnreset}}),
            if
                Reason =:= timeout;
                Reason =:= emsgsize ->
                    %% ?DBG({P#params.socket, Reason}),
                    Owner ! {tcp_error, ModuleSocket, Reason},
                    ok;
                Reason =:= closed, ShowEconnreset =:= false;
                Reason =:= econnreset, ShowEconnreset =:= false ->
                    %% ?DBG({P#params.socket, {Reason,ShowEconnreset}}),
                    Owner ! {tcp_closed, ModuleSocket},
                    ok;
                Reason =:= closed -> % ShowEconnreset =:= true
                    %% ?DBG({P#params.socket, {Reason,ShowEconnreset}}),
                    %% Try to be bug-compatible with the inet-driver...
                    Owner ! {tcp_error, ModuleSocket, econnreset},
                    Owner ! {tcp_closed, ModuleSocket},
                    ok;
                true ->
                    %% ?DBG({P#params.socket, {Reason,ShowEconnreset}}),
                    Owner ! {tcp_error, ModuleSocket, Reason},
                    Owner ! {tcp_closed, ModuleSocket},
                    ok
            end
    end,
    {recv_stop(D#{active := false}),
     case D of
         #{recv_from := From} ->
             Reason_1 =
                 case Reason of
                     econnreset when ShowEconnreset =:= false -> closed;
                     closed     when ShowEconnreset =:= true  -> econnreset;
                     _ -> Reason
                 end,
             [{reply, From, {error, Reason_1}},
              {{timeout, recv}, cancel}
              | ActionsR];
         #{} ->
             ActionsR
     end}.

%% Initialize packet recv state
recv_start(D) ->
    D#{recv_length => 0}.

recv_stop(D) ->
    maps:without([recv_from, recv_length], D).

decode_packet(#{packet := Packet} = D) ->
    case D of
        #{packet := http, recv_httph := true} -> httph;
        #{packet := http_bin, recv_httph := true} -> httph_bin;
        #{packet := Packet} -> Packet
    end.

%% Deliver data and update the active state
%% -> {NewD, NewActionsR}
recv_data_deliver(
  #params{owner = Owner} = P,
  #{mode := Mode, header := Header, deliver := Deliver,
    packet := Packet} = D,
  ActionsR, Data) ->
    %%
    %% ?DBG([{owner, Owner},
    %% 	  {mode, Mode},
    %% 	  {header, Header}, {deliver, Deliver}, {packet, Packet}]), 
    DeliverData = deliver_data(Data, Mode, Header, Packet),
    case D of
        #{recv_from := From} ->
            {recv_stop(next_packet(D, Packet, Data)),
             [{reply, From, {ok, DeliverData}},
              {{timeout, recv}, cancel}
              | ActionsR]};
        #{active := false} ->
            D_1 = D#{buffer := unrecv_buffer(Data, maps:get(buffer, D))},
            {recv_stop(next_packet(D_1, Packet, Data)),
             ActionsR};
        #{active := Active} ->
            %% ?DBG({active, Active}),
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

catbin(<<>>, Bin) when is_binary(Bin) -> Bin;
catbin(Bin, <<>>) when is_binary(Bin) -> Bin;
catbin(Bin1, Bin2) when is_binary(Bin1), is_binary(Bin2) ->
    <<Bin1/binary, Bin2/binary>>.

unrecv_buffer(Data, Buffer) ->
    case Buffer of
        <<>> ->
            Data;
        _ when is_binary(Buffer) ->
            [Data, Buffer];
        _ ->
            [Data | Buffer]
    end.

condense_buffer([Bin]) when is_binary(Bin) -> Bin;
condense_buffer(Buffer) ->
    iolist_to_binary(reverse_improper(Buffer, [])).

deliver_data(Data, Mode, Header, Packet) ->
    if
        Packet =:= 1;
        Packet =:= 2;
        Packet =:= 4 ->
            <<?header(Packet, _Size), Payload/binary>> = Data,
            deliver_data(Payload, Mode, Header);
        Packet =:= http;
        Packet =:= http_bin;
        Packet =:= httph;
        Packet =:= httph_bin ->
            Data;
        true ->
            deliver_data(Data, Mode, Header)
    end.

deliver_data(Data, list, _N) -> binary_to_list(Data);
deliver_data(Data, binary, 0) -> Data;
deliver_data(Data, binary, N) ->
    case Data of
        <<_:N/binary>> -> binary_to_list(Data);
        <<Header:N/binary, Payload/binary>> ->
            binary_to_list(Header) ++ Payload
    end.

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


%% -------
%% setopts in server
%%

%% -> {ok, NewD} | {{error, Reason}, D}
state_setopts(_P, D, _State, []) ->
    {ok, D};
state_setopts(P, D, State, [{Tag,Val} | Opts]) ->
    %% ?DBG([{state, State}, {opt, {Tag,Val}}]),
    SocketOpts = socket_opts(),
    case maps:is_key(Tag, SocketOpts) of
        true ->
            %% options for the 'socket' module
            %%
            case P#params.socket of
                undefined ->
                    {{error, closed}, D};
                Socket ->
                    case
                        socket_setopt(
                          Socket, maps:get(Tag, SocketOpts), Val)
                    of
                        ok ->
                            state_setopts(P, D, State, Opts);
                        {error, _} = Error ->
                            {Error, D}
                    end
            end;
        false ->
            case maps:is_key(Tag, server_write_opts()) of
                %% server options for socket send hence
                %% duplicated in {opt,meta}
                %%
                true when State =:= 'closed' ->
                    %% ?DBG('server write when state closed'),
                    {{error, einval}, D};
                true ->
                    %% ?DBG('server write'),
                    state_setopts_server(
                      P, D, State, Opts, Tag, Val);
                false ->
                    case maps:is_key(Tag, server_read_opts()) of
                        %% server options for receive
                        %%
                        true
                          when State =:= 'closed';
                               State =:= 'closed_read';
                               State =:= 'closed_read_write' ->
                            %% ?DBG('server read when state closed*'),
                            {{error, einval}, D};
                        true ->
                            %% ?DBG('server read'),
                            state_setopts_server(
                              P, D, State, Opts, Tag, Val);
                        false ->
                            %% ignored and invalid options
                            %%
                            case ignore_optname(Tag) of
                                true ->
                                    %% ?DBG(ignore),
                                    state_setopts(P, D, State, Opts);
                                false ->
                                    %% ?DBG({extra, Tag}),
                                    {{error, einval}, D}
                            end
                    end
            end
    end.

state_setopts_server(P, D, State, Opts, Tag, Value) ->
    case Tag of
        active ->
            state_setopts_active(P, D, State, Opts, Value);
        packet ->
            case is_packet_option_value(Value) of
                true ->
                    case D of
                        #{recv_httph := _} ->
                            state_setopts(
                              P,
                              maps:remove(
                                recv_httph, D#{packet => Value}),
                              State, Opts);
                                #{} ->
                            state_setopts(
                              P, D#{packet => Value}, State, Opts)
                    end;
                false ->
                    {{error, einval}, D}
            end;
        _ ->
	    %% ?DBG([{tag, Tag}, {value, Value}]),
            state_setopts(P, D#{Tag => Value}, State, Opts)
    end.

state_setopts_active(P, D, State, Opts, Active) ->
    %% ?DBG([{active, Active}]),
    if
        Active =:= once;
        Active =:= true ->
            state_setopts(P, D#{active := Active}, State, Opts);
        Active =:= false ->
            case D of
                #{active := OldActive} when is_integer(OldActive) ->
                    P#params.owner ! {tcp_passive, module_socket(P)},
                    ok;
                #{active := _OldActive} -> ok
            end,
            state_setopts(P, D#{active := Active}, State, Opts);
        is_integer(Active), -32768 =< Active, Active =< 32767 ->
            N =
                case D of
                    #{active := OldActive} when is_integer(OldActive) ->
                        OldActive + Active;
                    #{active := _OldActive} ->
                        Active
                end,
            if
                32767 < N ->
                    {{error, einval}, D};
                N =< 0 ->
                    P#params.owner ! {tcp_passive, module_socket(P)},
                    state_setopts(P, D#{active := false}, State, Opts);
                true ->
                    state_setopts(P, D#{active := N}, State, Opts)
            end;
        true ->
            {{error, einval}, D}
    end.

%%
%% -------
%% getopts in server
%%

%% -> {ok, [Options]} | {error, einval}
state_getopts(P, D, State, Opts) ->
    state_getopts(P, D, State, Opts, []).
state_getopts(_P, _D, _State, [], Acc) ->
    {ok, reverse(Acc)};
state_getopts(P, D, State, [Tag | Tags], Acc) ->
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
                            state_getopts(
                              P, D, State, Tags, [{Key, Value} | Acc]);
                        {error, einval} = ERROR ->
                            ERROR;
                        {error, _Reason} ->
                            %% ?DBG([{reason, _Reason}]),
                            state_getopts(P, D, State, Tags, Acc)
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
                    state_getopts(P, D, State, Tags, [{Key, Value} | Acc]);
                false ->
                    case maps:is_key(Key, server_read_opts()) of
                        %% server options for receive
                        %%
                        true
                          when State =:= 'closed';
                               State =:= 'closed_read';
                               State =:= 'closed_read_write' ->
                            %% ?DBG('server read when closed*'),
                            {error, einval};
                        true ->
                            %% ?DBG('server read'),
                            Value = maps:get(Key, D),
                            state_getopts(
                              P, D, State, Tags, [{Key, Value} | Acc]);
                        false ->
                            %% ignored and invalid options
                            %%
                            case ignore_optname(Key) of
                                true ->
                                    %% ?DBG({ignore, Tag}),
                                    state_getopts(P, D, State, Tags, Acc);
                                false ->
                                    %% ?DBG({extra, Tag}),
                                    {error, einval}
                            end
                    end
            end
    end.

%%
%% -------

handle_info(Socket, Owner, #{active := Active} = D) -> 
    %% Read counters
    Counters_1 = socket_info_counters(Socket),
    %% Check for recent wraps
    {D_1, Wrapped} = receive_counter_wrap(Socket, D, []),
    %%
    %% Assumption: a counter that we just now got a wrap message from
    %% will not wrap again before we read the updated value
    %%
    %% Update wrapped counters
    Info = #{counters := Counters_2} = socket:info(Socket),
    Counters_3 = maps:merge(Counters_1, maps:with(Wrapped, Counters_2)),
    %% Go ahead with wrap updated counters
    Counters_4 = maps:from_list(getstat_what(D_1, Counters_3)),
    {D_1, Info#{counters => Counters_4, owner => Owner, active => Active}}.
    
getstat(Socket, D, What) ->
    %% Read counters
    Counters_1 = socket_info_counters(Socket),
    %% Check for recent wraps
    {D_1, Wrapped} = receive_counter_wrap(Socket, D, []),
    %%
    %% Assumption: a counter that we just now got a wrap message from
    %% will not wrap again before we read the updated value
    %%
    %% Update wrapped counters
    Counters_2 = socket_info_counters(Socket),
    Counters_3 = maps:merge(Counters_1, maps:with(Wrapped, Counters_2)),
    %% Go ahead with wrap updated counters
    {D_1, getstat_what(What, D_1, Counters_3)}.

getstat_what(D, C) ->
    getstat_what(inet:stats(), D, C).

getstat_what([], _D, _C) -> [];
getstat_what([Tag | What], D, C) ->
    Val =
        case Tag of
            recv_oct ->
                counter_value(read_byte, D, C);
            recv_cnt ->
                counter_value(read_pkg, D, C);
            recv_max ->
                getstat_avg(read_byte, D, C, read_pkg);
            recv_avg ->
                getstat_avg(read_byte, D, C, read_pkg);
            recv_dvi -> 0;
            %%
            send_oct ->
                counter_value(write_byte, D, C);
            send_cnt ->
                counter_value(write_pkg, D, C);
            send_max ->
                getstat_avg(write_byte, D, C, write_pkg);
            send_avg ->
                getstat_avg(write_byte, D, C, write_pkg);
            send_pend -> 0
        end,
    [{Tag, Val} | getstat_what(What, D, C)].

getstat_avg(SumTag, D, C, CntTag) ->
    Cnt = counter_value(CntTag, D, C),
    if
        Cnt =:= 0 ->
            counter_value(SumTag, D, C);
        true ->
            round(counter_value(SumTag, D, C) / Cnt)
    end.

socket_info_counters(Socket) ->
    #{counters := Counters} = socket:info(Socket),
    Counters.

receive_counter_wrap(Socket, D, Wrapped) ->
    receive
        ?socket_counter_wrap(Socket, Counter) ->
	    %% ?DBG([{counter, Counter}]),
            receive_counter_wrap(
              Socket, wrap_counter(Counter, D), [Counter | Wrapped])
    after 0 ->
            {D, Wrapped}
    end.

wrap_counter(Counter, D) ->
    case D of
        #{Counter := N} ->
            D#{Counter := N + 1};
        #{} ->
            D#{Counter => 1}
    end.

-define(COUNTER_BITS, 32).
counter_value(Counter, D, Counters) ->
    case D of
        #{Counter := Wraps} ->
            (Wraps bsl ?COUNTER_BITS) + maps:get(Counter, Counters);
        #{} ->
            maps:get(Counter, Counters)
    end.



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

%% Reverse but allow improper list
reverse_improper([H | T], Acc) ->
    reverse_improper(T, [H | Acc]);
reverse_improper([], Acc) -> Acc;
reverse_improper(T, Acc) -> [T | Acc].


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
