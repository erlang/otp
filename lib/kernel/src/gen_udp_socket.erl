%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

-module(gen_udp_socket).
-moduledoc false.
-behaviour(gen_statem).

-compile(nowarn_deprecated_catch).

-compile({no_auto_import, [monitor/1]}).

%% gen_udp
-export([
	 open/2,
	 close/1,
	 connect/3,
	 controlling_process/2,
	 recv/2,recv/3,
         send/2, send/3, send/4, send/5
	]).

%% inet
-export([
         monitor/1, cancel_monitor/1,
         setopts/2, getopts/2,
         sockname/1, peername/1,
         socknames/1,
         getstat/2
        ]).

%% Utility
-export([info/1, which_sockets/0, socket_to_list/1]).

-ifdef(undefined).
-export([fdopen/2]).
-endif.

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).
-export([handle_event/4]).

-include("inet_int.hrl").
-include("socket_int.hrl").

%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

-define(RECBUF, 65536).

%% -define(ESOCK_VERBOSE_BADARG, true).
-ifdef(ESOCK_VERBOSE_BADARG).
-define(BADARG(Args), erlang:error(badarg, Args)).
-else.
-define(BADARG(_), exit(badarg)).
-endif.


%% -------------------------------------------------------------------------

%% Construct a "socket" as in this module's API
-define(MODULE_socket(Server, Socket),
        {'$inet', ?MODULE, {Server, Socket}}).

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


%%% ========================================================================
%%% API
%%%

%% -- close ----------------------------------------------------------------

close(?MODULE_socket(Server, _Socket)) ->
    ?badarg_exit(close_server(Server)).


%% Helpers -------

close_server(Server) ->
    Result = call(Server, close),
    stop_server(Server),
    Result.


%% -- connect ----------------------------------------------------------------

connect(?MODULE_socket(_Server, Socket), Address, Port) ->
    %% ?DBG([{address, Address}, {port, Port}]),
    {Mod, _} = inet:udp_module([], Address),
    Domain   = domain(Mod),
    %% ?DBG([{mod, Mod}, {domain, Domain}]),
    try
        begin
            Dest =
                case Mod:getaddr(Address) of
                    {ok, IP} when (Domain =:= local) ->
			%% ?DBG([{ip, IP}]),
                        dest2sockaddr(IP);
                    {ok, IP} ->
			%% ?DBG([{ip, IP}]),
                        dest2sockaddr({IP, Port});
                    {error, _Reason} = ERROR ->
			%% ?DBG(['getaddr', {error, ERROR}]),
                        throw(ERROR)
                end,
            case os:type() of
                {unix,linux} ->
                    case socket:peername(Socket) of
                        {error, enotconn} ->
                            socket:connect(Socket, Dest);
                        {error, closed} = Error ->
                            Error;
                        _X -> % Matches {ok, _} and unknown errors
                            _ = socket:connect(Socket, #{family => unspec}),
                            socket:connect(Socket, Dest)
                    end;
                _ ->
		    %% ?DBG(['try connect']),
                    socket:connect(Socket, Dest)
            end
        end
    catch
        throw:E:_ ->
            E
    end.


%% -- open -----------------------------------------------------------------

open(Service, Opts) ->
    open_lookup(Service, Opts).


%% Helpers -------

open_lookup(Service, Opts0) ->
    %% ?DBG(['open lookup', {service, Service}, {opts, Opts0}]),
    {EinvalOpts, Opts1} = setopts_split(einval, Opts0),
    EinvalOpts =:= [] orelse exit(badarg),
    {Mod, Opts2} = inet:udp_module(Opts1),
    Domain = domain(Mod),
    {StartOpts, Opts3} = setopts_split(start, Opts2),
    ErrRef = make_ref(),
    try
	begin
	    %% IPs    = val(ErrRef, Mod:getaddrs(Address, Domain)),
	    Port   = val(ErrRef, Mod:getserv(Service)),
	    %% Opts_4 = [{port, Port}, {buffer, ?RECBUF} | Opts3],
	    Opts4 = [{port, Port} | Opts3],
	    #udp_opts{fd     = Fd,
		      ifaddr = BindIP,
		      port   = BindPort,
		      opts   = OpenOpts} =
		val(ErrRef, inet:udp_options(Opts4, Mod)),
            %% ?DBG([{fd, Fd}, {bind_ip, BindIP}, {bind_port, BindPort},
            %%       {opts, OpenOpts}]),
            BindAddr  = bind_addr(Domain, BindIP, BindPort, Fd),
            ExtraOpts = extra_opts(Fd),
            do_open(Mod, BindAddr, Domain, OpenOpts, StartOpts, ExtraOpts)
	end
    catch
        throw : {ErrRef, Reason} ->
            ?badarg_exit({error, Reason})
    end.

do_open(Mod, BindAddr, Domain, OpenOpts, Opts, ExtraOpts0) ->

    %% ?DBG([{mod, Mod}, {bind_addr, BindAddr}, {domain, Domain},
    %%       {open_opts, OpenOpts}, {opts, Opts}, {extra_opts0, ExtraOpts0}]),

    %%
    %% The {netns, File} option is passed in Fd by inet:connect_options/2,
    %% and then over to ExtraOpts.
    %% The {debug, Bool} option is passed in Opts (and also into ExtraOpts)
    %% since it is subversively classified as both start and socket option
    %% (and also open_opts).
    %%

    {OOpts, Opts2}           = setopts_split(open_opts, Opts),
    ExtraOpts                = extra_opts(OOpts, ExtraOpts0),
    {SocketOpts, StartOpts0} = setopts_split(socket,    Opts2),
    StartOpts                = start_opts(StartOpts0),
    %% ?DBG(['try start server',
    %%       {start_opts, StartOpts},
    %%       {extra_opts, ExtraOpts}]),
    case start_server(Mod, Domain, StartOpts, ExtraOpts) of
        {ok, Server} ->
            {PreBindSetOpts, OpenOpts2} = setopts_split(pre_bind, OpenOpts),
            %% ?DBG([{pre_bind_open_opts, PreBindSetOpts},
            %%       {open_opts_2, OpenOpts2}]),

            {SetOpts0, _DroppedOpts} =
                setopts_split(#{socket       => [],
                                server_read  => [],
                                server_write => []},
                              OpenOpts2),
            %% ?DBG([{set_opts_0, SetOpts0}, {dropped_opts, _DroppedOpts}]),
            SetOpts =
                default_active_true(
                  [{start_opts, StartOpts}] ++ SocketOpts ++ SetOpts0),
            %% ?DBG([{set_opts, SetOpts}]),

            ErrRef = make_ref(),
            try
                %% Set pre-bind opts
                %% ?DBG(['maybe pre bind setopts']),
                PreBindSetOpts =/= [] andalso
                    ok(ErrRef, call(Server, {setopts, PreBindSetOpts})),

                %% ?DBG(['maybe try bind', {bind_addr, BindAddr}]),
                ok(ErrRef, call_bind(Server,
                                     default_any(Domain, ExtraOpts, BindAddr))),

                %% ?DBG(['try setopts',
                %%       {socket_opts, SocketOpts}, {set_opts, SetOpts}]),
                ok(ErrRef, call(Server, {setopts, SocketOpts ++ SetOpts})),

                %% ?DBG(['try get-socket']),
                Socket = val(ErrRef, call(Server, get_socket)),

                {ok, ?MODULE_socket(Server, Socket)}
            catch
                throw : {ErrRef, Reason} ->
                    close_server(Server),
                    ?badarg_exit({error, Reason})
            end;
        {error, _} = Error ->
            ?badarg_exit(Error)
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

%% Should we verify the options or just accept them?
%% The *opt_categories functions *should* filter, so...
extra_opts([], ExtraOpts)
  when is_map(ExtraOpts) ->
    ExtraOpts;
extra_opts([{Opt, Val}|Opts], ExtraOpts)
  when is_list(Opts) andalso is_map(ExtraOpts) ->
    extra_opts(Opts, ExtraOpts#{Opt => Val}).



default_any(_Domain, #{fd := _}, _BindAddr) ->
    undefined;
default_any(Domain, _ExtraOpts, undefined = Undefined) ->
    if
        ((Domain =:= inet) orelse (Domain =:= inet6)) ->
            #{family => Domain,
              addr   => any,
              port   => 0};
        true ->
            Undefined
    end;
default_any(_Domain, _ExtraOpts, BindAddr) ->
    BindAddr.

bind_addr(Domain, BindIP, BindPort, Fd)
  when ((BindIP =:= undefined) andalso (BindPort =:= 0)) orelse
       (is_integer(Fd) andalso (0 =< Fd)) ->
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
bind_addr(local = Domain, BindIP, _BindPort, _Fd) ->
    %% ?DBG([{bind_ip, BindIP}]),
    case BindIP of
	any ->
	    undefined;
	{local, Path} ->
	    #{family => Domain,
	      path   => Path}
    end;
bind_addr(Domain, BindIP, BindPort, _Fd)
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
                    #{flags := Flags, addr := #{addr := _A}} = Addr <-
                        Addrs,
		    %% (element(1, A) =/= 169) andalso
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
    call(Server, {bind, BindAddr}).


default_active_true(Opts) ->
    case lists:keyfind(active, 1, Opts) of
        {active,_} ->
            Opts;
        _ ->
            [{active, true} | Opts]
    end.


%% -------------------------------------------------------------------------

%% Connected send

send(?MODULE_socket(_Server, Socket), Data) ->
    socket_send(Socket, Data);
send(_Socket, _Data) ->
    ?BADARG([_Socket, _Data]).


-compile({inline, [make_iov/1]}).
make_iov(Bin) when is_binary(Bin) -> [Bin];
make_iov(IOV) when is_list(IOV)   -> IOV.
    
-compile({inline, [dest2sockaddr/1]}).
dest2sockaddr({Addr, Port})
  when is_tuple(Addr) andalso (tuple_size(Addr) =:= 4) andalso
       is_integer(Port) ->
    #{family => inet,
      port   => Port,
      addr   => Addr};
dest2sockaddr({Addr, Port})
  when is_tuple(Addr) andalso (tuple_size(Addr) =:= 8) andalso
       is_integer(Port) ->
    #{family => inet6,
      port   => Port,
      addr   => Addr};
dest2sockaddr({local = Fam, Path})
  when is_binary(Path) orelse is_list(Path) ->
    #{family => Fam,
      path   => Path};
dest2sockaddr({Fam, {Addr, Port}})
  when is_atom(Fam) andalso
       ((Fam =:= inet) orelse (Fam =:= inet6)) andalso
       is_integer(Port) ->
    #{family => Fam,
      port   => Port,
      addr   => Addr};
dest2sockaddr(_Arg) ->
    ?BADARG([_Arg]).

-compile({inline, [make_cmsghdrs/1]}).
make_cmsghdrs(AncData) when is_list(AncData) ->
    [make_cmsghdr(AD) || AD <- AncData];
make_cmsghdrs(_Arg) ->
    ?BADARG([_Arg]).

make_cmsghdr({tos = Type, Byte}) ->
    #{level => ip, type => Type, data => Byte};
make_cmsghdr({tclass = Type, Byte}) ->
    #{level => ipv6, type => Type, data => Byte};
make_cmsghdr({ttl = Type, Byte}) ->
    #{level => ip, type => Type, data => Byte};
make_cmsghdr(_Arg) ->
    ?BADARG([_Arg]).
    

send(Socket, Destination, Data) ->
    do_sendto(Socket, dest2sockaddr(Destination), Data).

send(Socket, {_, _} = Destination, PortZero, Data)
  when (PortZero =:= 0) ->
    send(Socket, Destination, Data);
send(_Socket, {_,_} = _Destination, PortZero, _Data)
  when is_integer(PortZero) ->
    {error, einval};
send(Socket, {_,_} = Destination, AncData, Data) ->
    do_sendmsg(Socket,
	       dest2sockaddr(Destination),
	       make_iov(Data),
	       make_cmsghdrs(AncData));
send(Socket, Addr, Port, Data)
  when is_tuple(Addr) andalso
       ((tuple_size(Addr) =:= 4) orelse (tuple_size(Addr) =:= 8)) andalso
       is_integer(Port) ->
    send(Socket, {Addr, Port}, Data);
send(?MODULE_socket(_, ESock) = Socket,
     Host, Service, Data) when is_list(Host) orelse is_atom(Host) ->
    case socket:getopt(ESock, {otp, domain}) of
	{ok, Domain} ->
	    case inet:getaddr(Host, Domain) of
		{ok, Addr} ->
                    {ok, #{mod := Mod}} = socket:getopt(ESock, {otp, meta}),
		    case Mod:getserv(Service) of
			{ok, Port} ->
			    send(Socket, {Addr, Port}, Data);
			{error, einval} ->
			    exit(badarg);
			{error, _} = ERROR ->
			    ERROR
		    end;
		{error, einval} ->
		    exit(badarg);
		{error, _} = ERROR ->
		    ERROR
	    end;
	ERROR ->
	    ERROR
    end;
send(_Socket, _Arg1, _Arg2, _Arg3) ->
    ?BADARG([_Socket, _Arg1, _Arg2, _Arg3]).


send(Socket, Addr, Port, AncData, Data)
  when is_tuple(Addr) andalso
       ((tuple_size(Addr) =:= 4) orelse (tuple_size(Addr) =:= 8)) andalso
       is_integer(Port) ->
    send(Socket, {Addr, Port}, AncData, Data);
send(?MODULE_socket(_, ESock) = Socket,
     Host, Service, AncData, Data) when is_list(Host) orelse is_atom(Host) ->
    case socket:getopt(ESock, {otp, domain}) of
	{ok, Domain} ->
	    case inet:getaddr(Host, Domain) of
		{ok, Addr} ->
                    {ok, #{mod := Mod}} = socket:getopt(ESock, {otp, meta}),
		    case Mod:getserv(Service) of
			{ok, Port} ->
			    send(Socket, {Addr, Port}, AncData, Data);
			{error, einval} ->
			    exit(badarg);
			{error, _} = ERROR ->
			    ERROR
		    end;
		{error, einval} ->
		    exit(badarg);
		{error, _} = ERROR ->
		    ERROR
	    end;
	ERROR ->
	    ERROR
    end;
send(_Socket, _Arg1, _Arg2, _Arg3, _Arg4) ->
    ?BADARG([_Socket, _Arg1, _Arg2, _Arg3, _Arg4]).



do_sendto(?MODULE_socket(_Server, Socket), Dest, Data) ->
    case socket_sendto(Socket, Dest, Data) of
        {error, {invalid, _}} ->
            exit(badarg);
        Any ->
            Any
    end.

do_sendmsg(?MODULE_socket(_Server, Socket), SockAddr, IOV, Ctrl)
  when is_list(IOV) andalso is_list(Ctrl) ->
    MsgHdr = #{addr => SockAddr,
	       iov  => IOV,
	       ctrl => Ctrl},
    case socket_sendmsg(Socket, MsgHdr) of
        {error, {invalid, _}} ->
            exit(badarg);
        Any ->
            Any
    end.

    
%% -------------------------------------------------------------------------

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% This is a bit weird. If two calls to recv is made with short intervals
%% from different processes, with say timeout 10s and then 5s. The second
%% call will be postponed until the first has been processed, which can
%% take up to 10s. If the first times out (after 10s) the second is 
%% supposed to have been timed out already by the time it gets processed.
%% Can we postpone? And can we make conditional postpone?

recv(?MODULE_socket(Server, _Socket), Length, Timeout) ->
    ?badarg_exit(call(Server, {recv, Length, Timeout})).


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
        {udp, S, _Data} = Msg ->
            controlling_process(S, NewOwner, Server, Msg);
        {udp_closed, S} = Msg ->
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
    call(Server, {setopts, Opts}).


%% -------------------------------------------------------------------------

getopts(?MODULE_socket(Server, _Socket), Opts) when is_list(Opts) ->
    call(Server, {getopts, Opts}).


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
            #{rstates => [closed], wstates => [closed]};
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
    which_sockets(socket:which_sockets(udp)).

which_sockets(Socks) ->
    which_sockets(Socks, []).

which_sockets([], Acc) ->
    Acc;
which_sockets([Sock|Socks], Acc) ->
    case socket:getopt(Sock, {otp, meta}) of
	{ok, undefined} ->
	    which_sockets(Socks, Acc);
	{ok, _Meta} ->
	    %% One of ours - try to recreate the compat socket.
	    %% Currently we don't have the 'owner' in meta,
	    %% so we need to look it up...
	    #{owner := Owner} = socket:info(Sock),
	    MSock = ?MODULE_socket(Owner, Sock),
	    which_sockets(Socks, [MSock|Acc]);
	_ ->
	    which_sockets(Socks, Acc)
    end.


%%% ========================================================================
%%% Socket glue code
%%%

-compile({inline, [socket_recvfrom/2]}).
socket_recvfrom(Socket, Length) ->
    socket:recvfrom(Socket, Length, nowait).

-compile({inline, [socket_recvmsg/2]}).
socket_recvmsg(Socket, Length) ->
    socket:recvmsg(Socket, Length, 0, nowait).


-compile({inline, [socket_send/2]}).
socket_send(Socket, Data) ->
    socket:send(Socket, Data).

-compile({inline, [socket_sendto/3]}).
socket_sendto(Socket, Dest, Data) ->
    %% <OBSERVE>
    %%
    %% Argument swap!
    %%
    %% </OBSERVE>
    %% socket:setopt(Socket, otp, debug, true),
    Res = socket:sendto(Socket, Data, Dest),
    %% socket:setopt(Socket, otp, debug, false),
    Res.

-compile({inline, [socket_sendmsg/2]}).
socket_sendmsg(Socket, MsgHdr) ->
    socket:sendmsg(Socket, MsgHdr).


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
socket_cancel(Socket, Info) ->
    case socket:cancel(Socket, Info) of
        ok              -> ok;
        {error, closed} -> ok;

        %% Race - shall we await the message (to flush) or just ignore?
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


%% -------------------------------------------------------------------------

-compile({inline, [domain/1]}).
domain(Mod) ->
    case Mod of
        inet_udp  -> inet;
        inet6_udp -> inet6;
        local_udp -> local
    end.


%% -------------------------------------------------------------------------

address(SockAddr) ->
    case SockAddr of
        #{family := Family, addr := IP, port := Port}
	when (Family =:= inet) orelse (Family =:= inet6) ->
            {IP, Port};
        #{family := local, path := Path} ->
            {local, Path}
    end.


%% -------------------------------------------------------------------------

setopts_split(FilterTags, Opts) ->
    setopts_split(FilterTags, Opts, [], []).

%%
setopts_split(_FilterTags, [], True, False) ->
    %% ?DBG(['done', {true, True}, {false, False}]),
    {reverse(True), reverse(False)};
setopts_split(FilterTags, [Opt | Opts], True, False) ->
    %% ?DBG([{ftags, FilterTags}, {opt, Opt}]),
    Opt_1 = conv_setopt(Opt),
    case member(FilterTags, setopt_categories(Opt_1)) of
        true ->
            setopts_split(FilterTags, Opts, [Opt_1 | True], False);
        false ->
            setopts_split(FilterTags, Opts, True, [Opt_1 | False])
    end.


%% Set operation on atom sets that are atoms or maps with atom tags.
%% Returns true if sets have at least one common member, false otherwise.
%% X is atom() or map(), Y is map().
member(X, Y) when is_atom(X) andalso is_map(Y) ->
    case Y of
        #{X := _} -> true;
        #{} -> false
    end;
member(X, Y) when is_map(X) andalso is_map(Y) ->
    maps:fold(
      fun (_, _, true) -> true;
          (Key, _, false) -> maps:is_key(Key, Y)
      end, false, X).


conv_setopt(binary) -> {mode, binary};
conv_setopt(list)   -> {mode, list};
conv_setopt(inet)   -> {udp_module, inet_udp};
conv_setopt(inet6)  -> {udp_module, inet6_udp};
conv_setopt(local)  -> {udp_module, local_udp};
conv_setopt(Other)  -> Other.

%% Socket options

socket_setopt(Socket, {raw, Level, Key, Value}) ->
    try socket:setopt_native(Socket, {Level,Key}, Value)
    catch
        throw:{invalid, _} ->
            {error, einval}
    end;
socket_setopt(Socket, {raw, {Level, Key, Value}}) ->
    try socket:setopt_native(Socket, {Level,Key}, Value)
    catch
        throw:{invalid, _} ->
            {error, einval}
    end;
socket_setopt(Socket, {Tag, Value}) ->
    %% ?DBG({Tag, Value}),
    case socket_opt() of
        #{Tag := {Domain, _} = Opt} when is_atom(Domain) ->
            %% ?DBG(Opt),
            socket_setopt_opt(Socket, Opt, Tag, Value);

        #{Tag := Opts} when is_list(Opts) ->
            %% ?DBG(Opts),
            socket_setopt_opts(Opts, Socket, Tag, Value);

        #{} ->
            {error, einval}
    end.

socket_setopt_opt(Socket, Opt, Tag, Value) ->
    %% _ = socket:setopt(Socket, otp, debug, true),
    Val = socket_setopt_value(Tag, Value, Opt),
    %% ?DBG(Val),
    Res = socket:setopt(Socket, Opt, Val),
    %% ?DBG(Res),
    %% _ = socket:setopt(Socket, otp, debug, false),
    Res.
    

socket_setopt_opts([], _Socket, _Tag, _Value) ->
    ok;

%% Its possible for *one* option to be mapped to several 'socket' options.
%% For instance, when *setting* the the option recbuf, we actually set two
%% 'socket' options: {otp, rcvbuf} and {socket, rcvbuf}.
socket_setopt_opts([{_Level, _OptKey} = Opt|Opts], Socket, Tag, Value) ->
    %% _ = socket:setopt(Socket, otp, debug, true),
    Res = socket:setopt(Socket, Opt, socket_setopt_value(Tag, Value, Opt)),
    %% _ = socket:setopt(Socket, otp, debug, false),
    case Res of
        ok ->
            socket_setopt_opts(Opts, Socket, Tag, Value);
        {error, _Reason} ->
            {error, einval}
    end;

%% We need to lookup the domain of the socket,
%% so we can select which one to use.
socket_setopt_opts(Opts, Socket, Tag, Value) ->
    case socket:getopt(Socket, otp, domain) of
        {ok, Domain} ->
            case lists:keysearch(Domain, 1, Opts) of
                {value, {Domain, Level, OptKey}} ->
                    Opt = {Level, OptKey},
                    %% _ = socket:setopt(Socket, otp, debug, true),
                    Res = socket:setopt(Socket, Opt,
                                        socket_setopt_value(Tag, Value, Opt)),
                    %% _ = socket:setopt(Socket, otp, debug, false),
                    Res;
                false ->
                    {error, einval}
            end;
        {error, _} ->
            {error, einval}
    end.



%% This happens when we set the recbuf options.
%% As a side effect, we also set our internal buffer,
%% but with a limiter.
socket_setopt_value(recbuf, Value, {otp, rcvbuf}) 
  when (Value > ?RECBUF) ->
    ?RECBUF;
socket_setopt_value(_Tag, Value, _) ->
    Value.

socket_getopt(Socket, {raw, Level, Key, ValueSpec}) ->
    case socket:getopt_native(Socket, {Level,Key}, ValueSpec) of
        {error, {invalid, _} = _Reason} ->
            %% ?DBG([{reason, _Reason}]),
            {error, einval};
        ELSE ->
            ELSE
    end;
socket_getopt(Socket, {raw, {Level, Key, ValueSpec}}) ->
    case socket:getopt_native(Socket, {Level,Key}, ValueSpec) of
        {error, {invalid, _} = _Reason} ->
            %% ?DBG([{reason, _Reason}]),
            {error, einval};
        ELSE ->
            ELSE
    end;
socket_getopt(Socket, Tag) when is_atom(Tag) ->
    case socket_opt() of
        #{Tag := {Domain, _} = Opt} when is_atom(Domain) ->
            %% ?DBG(Opt),
            socket_getopt_opt(Socket, Opt, Tag);

        #{Tag := Opts} when is_list(Opts) ->
            %% ?DBG(Opts),
            socket_getopt_opts(Opts, Socket, Tag);

        #{} = __X__ ->
            %% ?DBG({'socket_getopt - no match - 2', Tag, __X__}),
            {error, einval}
    end.


socket_getopt_opt(Socket, Opt, Tag) ->
    %% _ = socket:setopt(Socket, otp, debug, true),
    Res = socket:getopt(Socket, Opt),
    %% ?DBG([{'socket_getopt - result', Res}]),
    %% _ = socket:setopt(Socket, otp, debug, false),
    socket_getopt_value(Tag, Res).

%% socket_getopt_opts([], _Socket, _Tag) ->
%%     {error, einval};

%% Its possible for *one* option to be mapped to several 'socket' options.
%% But, its *always* the first element in th elist that is the "real"
%% option. This is the one used when *reading*. The other elements in the list
%% is basically side effect options, which is not used when reading.
socket_getopt_opts([{_Domain, _} = Opt|_], Socket, Tag) ->
    socket_getopt_opt(Socket, Opt, Tag);

socket_getopt_opts(Opts, Socket, Tag) ->
    case socket:getopt(Socket, otp, domain) of
        {ok, Domain} ->
            %% ?DBG([{'domain', Domain}]),
            case lists:keysearch(Domain, 1, Opts) of
                {value, {Domain, Level, OptKey}} ->
                    %% ?DBG(['ok domain', {level, Level}, {optkey, OptKey}]),
                    Opt  = {Level, OptKey},
                    %% _ = socket:setopt(Socket, otp, debug, true),
                    Res = socket:getopt(Socket, Opt),
                    %% _ = socket:setopt(Socket, otp, debug, false),
                    %% ?DBG([{'result', Res}]),
                    socket_getopt_value(Tag, Res);
                false ->
                    %% ?DBG({'invalid domain', Tag, Domain, Opts}),
                    {error, einval}
            end;
        {error, _DReason} ->
            %% ?DBG([{'unknown domain', _DReason}]),
            {error, einval}
    end.

    
    
%% socket_getopt_value(pktoptions, {ok, PktOpts0}) when is_list(PktOpts0) ->
%%     PktOpts = [{Type, Value} || #{type := Type, value := Value} <- PktOpts0],
%%     {ok, PktOpts};
socket_getopt_value(_Tag, {ok, _Value} = Ok) -> Ok;
socket_getopt_value(_Tag, {error, _} = Error) -> Error.

start_opts([{sys_debug, D} | Opts]) ->
    [{debug, D} | start_opts(Opts)];
start_opts([Opt | Opts]) ->
    [Opt | start_opts(Opts)];
start_opts([]) -> [].


%% Categories: socket, ignore, start, server_read, server_write, einval
%% returns a maps set

setopt_categories(Opt) ->
    case Opt of
        {raw, _, _, _}   -> #{socket => []};
        {raw, {_, _, _}} -> #{socket => []};
        {Tag, _}         -> opt_categories(Tag);
        _                -> ignore
    end.

getopt_categories(Opt) ->
    case Opt of
        {raw, _, _, _}   -> #{socket => []};
        {raw, {_, _, _}} -> #{socket => []};
        _                -> opt_categories(Opt)
    end.

%% setopt and getopt category
opt_categories(Tag) when is_atom(Tag) ->
    case Tag of
        sys_debug   -> #{start => []};
        %% open_opts is for the 'Opts' argument of the socket:open call
        debug       -> #{socket => [], start    => [], open_opts => []};
        ipv6_v6only -> #{socket => [], pre_bind => []};

        %% Some options may trigger us to choose recvmsg (instead of recvfrom)
        %% Or trigger us to choose recvfrom *if* was previously selected
        
        _ when (Tag =:= recvtos) orelse
               (Tag =:= recvttl) orelse
               (Tag =:= recvtclass) ->
            #{socket => [], recv_method => []};

        _ ->
            case maps:is_key(Tag, socket_opt()) of
                true -> #{socket => []};
                false ->
                    case maps:is_key(Tag, ignore_opt()) of
                        true ->
                            #{ignore => []};
                        false ->
                            maps:merge(
                              case maps:is_key(Tag, server_read_opts()) of
                                  true ->
                                      #{server_read => []};
                                  false ->
                                      #{}
                              end,
                              case maps:is_key(Tag, server_write_opts()) of
                                  true ->
                                      #{server_write => []};
                                  false ->
                                      #{}
                              end)
                    end
            end
    end.

-compile({inline, [ignore_opt/0]}).
ignore_opt() ->
    #{
      %% Handled by inet:udp_module/2
      udp_module => [],
      %% Handled by inet:udp_options/2
      ip => [],
      %% XXX Some of these must probably be handled one day...
      %% These are options for the 'port', which we don't have, so...
      high_msgq_watermark => [],
      low_msgq_watermark  => []
     }.

%% Category 'socket'
%%
%% Translation to 'level' and 'opt'
%% The map-values are one of the following:
%%    {Level, Key} | [{Level, Key}] | [{Domain, Level, Key}]
%% * The first is the "normal" way
%% * The second is basically a way to have side effects.
%%   For example, recbuf is nominally mapped to {socket, rcvbuf}
%%   but as a side effect our (socket nif) internal buffer is also
%%   set to the same value, but limited to at most 65536.
%% * The third is an even more special case, for the non-standard
%%   option pktoptions, which has different 'level' depending on the
%%   domain.
-compile({inline, [socket_opt/0]}).
socket_opt() ->
    #{
      %% Level: otp
      buffer => {otp, rcvbuf},
      debug  => {otp, debug},
      fd     => {otp, fd},

      %%
      %% Level: socket
      broadcast        => {socket, broadcast},
      bind_to_device   => {socket, bindtodevice},
      dontroute        => {socket, dontroute},
      exclusiveaddruse => {socket, exclusiveaddruse},
      keepalive        => {socket, keepalive},
      %% linger           => {socket, linger},
      %% low_watermark    => {socket, rcvlowat},
      priority         => {socket, priority},
      %% Note that its the *first* that is the actual option!
      %% The second can be seen as a side effect...
      recbuf           => [{socket, rcvbuf}, {otp, rcvbuf}],
      reuseaddr        => {socket, reuseaddr},
      sndbuf           => {socket, sndbuf},

      %%
      %% Level: udp
      %% nodelay => {udp, nodelay},

      %%
      %% Level: ip
      recvtos         => {ip, recvtos},
      recvttl         => {ip, recvttl},
      tos             => {ip, tos},
      ttl             => {ip, ttl},
      add_membership  => {ip, add_membership},
      drop_membership => {ip, drop_membership},
      multicast_if    => {ip, multicast_if},
      multicast_ttl   => {ip, multicast_ttl},
      multicast_loop  => {ip, multicast_loop},

      %%
      %% Level: ipv6
      recvtclass  => {ipv6, recvtclass},
      ipv6_v6only => {ipv6, v6only}, % pre_bind
      tclass      => {ipv6, tclass},

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
      pktoptions  => [{inet, ip, pktoptions}, {inet6, ipv6, pktoptions}]
      }.

%% -compile({inline, [socket_inherit_opts/0]}).
%% socket_inherit_opts() ->
%%     [priority].

-compile({inline, [server_read_write_opts/0]}).
server_read_write_opts() ->
    %% Common for read and write side
    %% mod is unknown at this time, but will be properly "initiated"
    %% before 'meta' is written...
    #{mod => undefined}.
-compile({inline, [server_read_opts/0]}).
server_read_opts() ->
    %% Read side only opts
    maps:merge(
      #{active => false, % inet_drv also has this default
        mode => list,
        header => 0,
        deliver => term,
        %% WHAT DO WE DO ABOUT THIS!!!
        %% This option, read_packets, is *currently* not used,
        %% but accepted for backward compatibillity reasons.
	read_packets => 5,
        start_opts => [], % Just to make it settable
        %% XXX not implemented yet
        exit_on_close => true},
      server_read_write_opts()).
-compile({inline, [server_write_opts/0]}).
server_write_opts() ->
    %% Write side only opts
    maps:merge(#{}, server_read_write_opts()).
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

%% Start for open - create a socket
start_server(Mod, Domain, StartOpts, ExtraOpts) ->
    Owner = self(),
    Arg   = {Mod, Domain, ExtraOpts, Owner},
    case gen_statem:start(?MODULE, Arg, StartOpts) of
        {ok, _} = OK                -> OK;
        {error, {shutdown, Reason}} -> {error, Reason};
        {error, _} = Error          -> Error
    end.


call(Server, Call) ->
    try gen_statem:call(Server, Call)
    catch exit:{noproc, {gen_statem, call, _Args}} -> {error, closed}
    end.

stop_server(Server) ->
    try gen_statem:stop(Server) of
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

%% 'controlling_process'
%% A super state that encapsulates any other state and
%% postpones all events but get_socket, get_server_opts/0
%% and Owner 'DOWN'.
%% Handling of controlling_process is divided up into two
%% parts. One part is handled by the state machine and the
%% other by the function controlling_process/2.
-record(controlling_process,
        {owner :: pid(),
         state :: term()}).


%% 'open'

%% 'reading'
-record(recv,
        {info :: socket:select_info() | socket:completion_info() }).

%% 'closed_read' | 'closed_read_write'
%% 'closed' % Socket is closed or not created

-record(params,
        {socket        :: undefined | socket:socket(),
         owner         :: pid(),
         owner_mon     :: reference(),
         %% This comes from the options recvtos | recvttl | recvtclass
         %% If any of them are set then the method will change to msg
         %%    RecvMethod =:= [] => recvfrom
         %%    RecvMethod =/= [] => recvmsg
         recv_method = []}).


init({Mod, Domain, ExtraOpts, Owner}) ->

    %% ?DBG([{init, open},
    %%       {domain, Domain}, {extraopts, ExtraOpts}, {owner, Owner}]),

    process_flag(trap_exit, true),
    OwnerMon  = erlang:monitor(process, Owner),
    Proto     = if (Domain =:= local) -> default; true -> udp end,
    Extra     = #{}, % #{debug => true},
    case socket_open(Domain, Proto, ExtraOpts, Extra) of
        {ok, Socket} ->
            D0  = server_opts(),
            D   = D0#{mod => Mod},
            ok = socket:setopt(Socket, {otp,iow},  true),
            ok = socket:setopt(Socket, {otp,meta}, meta(D)),
            P  = #params{socket    = Socket,
                         owner     = Owner,
                         owner_mon = OwnerMon},
            State = open,
            Data  = {P, D},
            {ok, State, Data};
        {error, Reason} ->
	    %% ?DBG({open_failed, Reason}),
	    {stop, {shutdown, Reason}}
    end;
init(Arg) ->
    error_logger:error_report([{badarg, {?MODULE, init, [Arg]}}]),
    error(badarg, [Arg]).

                        
socket_open(Domain, Proto, #{fd := FD} = ExtraOpts, Extra) ->
    Opts =
        (maps:merge(Extra, maps:remove(fd, ExtraOpts)))
        #{dup      => false,
          domain   => Domain,
          type     => dgram,
          protocol => Proto},
    %% ?DBG([{fd, FD}, {opts, Opts}]),
    case socket:open(FD, Opts) of
        {ok, Socket} = OK ->
            %% ?DBG(['open ok', {socket, Socket}]),
            case socket:info(Socket) of
                #{ctype    := fromfd,
                  domain   := Domain,
                  type     := dgram,
                  protocol := _SelectedProto} ->
                    OK;
                _INVALID ->
                    %% ?DBG(['info invalid', {invalid, _INVALID}]),
                    (catch socket:close(Socket)),
                    {error, einval}
            end;
        {error, _Reason} = ERROR ->
            %% ?DBG(['open error', {reason, _Reason}]),
            ERROR
    end;
socket_open(Domain, Proto, ExtraOpts, Extra) ->
    %% ?DBG([{domain, Domain}, {proto, Proto},
    %%       {extra_opts, ExtraOpts}, {extra, Extra}]),
    Opts = maps:merge(Extra, ExtraOpts),
    socket:open(Domain, dgram, Proto, Opts).


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
    %% ?DBG(['terminate', {socket, Socket}, {state, State}]),
    case State of
        'closed' ->
	    ok;
        'closed_read' ->
            _ = socket_close(Socket),
            ok;
        'closed_read_write' ->
            _ = socket_close(Socket),
            ok;
        _ ->
	    _ = socket_close(Socket),
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

%% Any state:

%% Call: get_socket/0 - internal
handle_event({call, From}, get_socket, _State,
             {#params{socket = Socket}, _D}) ->
    {keep_state_and_data,
     [{reply, From, {ok, Socket}}]};

%% Call: get_server_opts/0
handle_event({call, From}, get_server_opts, _State, {_P, D}) ->
    ServerOpts = maps:with(maps:keys(server_opts()), D),
    {keep_state_and_data,
     [{reply, From, {ok, ServerOpts}}]};

%% Event: Owner 'DOWN'
handle_event(info,
	     {'DOWN', OwnerMon, _, _, Reason} = _DOWN,
	     _State,
	     {#params{owner_mon = OwnerMon} = _P, _D} = P_D) ->
    %% ?DBG([{event, info}, {down, _DOWN}, {state, _State}]),
    {stop, {shutdown, Reason}, P_D};

%% Event: ?socket_counter_wrap/2
handle_event(info,
	     ?socket_counter_wrap(Socket, Counter),
	     'open' = _State,
	     {#params{socket = Socket} = P, D}) ->
    %% ?DBG([{state, _State}, {counter, Counter}]),
    {keep_state, {P, wrap_counter(Counter, D)}};

handle_event(info,
	     ?socket_counter_wrap(Socket, Counter),
	     #recv{} = _State,
	     {#params{socket = Socket} = P, D}) ->
    %% ?DBG([{state, _State}, {counter, Counter}]),
    {keep_state, {P, wrap_counter(Counter, D)}};

handle_event(info,
	     ?socket_counter_wrap(_Socket, _Counter),
	     _State,
	     _P_D) ->
    %% ?DBG([{state, _State}, {counter, _Counter}]),
    {keep_state_and_data, [postpone]};

%% Call: controlling_process/1
handle_event({call, {Caller, _} = From},
	     {controlling_process, NewOwner},
	     State,
	     {P, _D} = P_D) ->
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
handle_event({call, {Owner, _} = From},
	     controlling_process,
	     #controlling_process{owner = NewOwner, state = State},
	     {#params{owner = Owner, owner_mon = OwnerMon} = P, D}) ->
    NewOwnerMon = erlang:monitor(process, NewOwner),
    true = erlang:demonitor(OwnerMon, [flush]),
    {next_state, State,
     {P#params{owner = NewOwner, owner_mon = NewOwnerMon}, D},
     [{reply, From, ok}]};

%%
%% Postpone all events but the ones above controlling_process/1
%% until the controlling process has been changed
handle_event(_Type,
	     _Content,
	     #controlling_process{},
	     _StateData) ->
    {keep_state_and_data, [postpone]};
%% Handled state: #controlling_process{}

%% Call: close/0
handle_event({call, From},
	     close,
	     State,
	     {P, D} = P_D) ->
    %% ?DBG([{socket, P#params.socket}, {state, State}]),
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
            next_state(P,
		       cleanup_close_read(P,
					  D#{active := false},
					  State,
					  closed),
		       'closed',
		       [{reply, From, socket_close(P#params.socket)}])
    end;

%% Call: getopts/1
handle_event({call, From},
	     {getopts, Opts},
	     State,
	     {P, D}) ->
    %% ?DBG(['call getopts', {opts, Opts}, {state, State}, {d, D}]),
    Result = state_getopts(P, D, State, Opts),
    %% ?DBG(['call getopts result', {result, Result}]),
    {keep_state_and_data,
     [{reply, From, Result}]};

%% Call: setopts/1
handle_event({call, From},
	     {setopts, Opts},
	     State,
	     {P, D}) ->
    %% ?DBG([{setopts, Opts}, {state, State}, {d, D}]),
    {Result_1, {P_1, D_1}} = state_setopts(P, D, State, Opts),
    %% ?DBG([{result, Result_1}, {p1, P_1}, {d1, D_1}]),
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
                _ = socket:setopt(P_1#params.socket, {otp,meta}, meta(D_1)),
                Result_1;
            _ ->
                %% We should really handle this better. stop_and_reply?
                %% D_2 = meta(D_1),
                %% ?DBG([{d2, D_2}]),
                %% socket:setopt(P_1#params.socket, otp, debug, true),
                ok = socket:setopt(P_1#params.socket, {otp,meta}, meta(D_1)),
                %% socket:setopt(P_1#params.socket, otp, debug, false),
                Result_1
        end,
    Reply = {reply, From, Result},
    handle_reading(State, P_1, D_1, [Reply]);
    

%% Call: getstat/2
handle_event({call, From},
	     {getstat, What},
	     State,
	     {P, D}) ->
    case State of
        'closed' ->
            {keep_state_and_data, [{reply, From, {error, closed}}]};
        _ ->
            {D_1, Result} = getstat(P#params.socket, D, What),
            {keep_state, {P, D_1}, [{reply, From, {ok, Result}}]}
    end;

%% Call: info/1
handle_event({call, From},
	     info,
	     State,
	     {P, D}) ->
    case State of
        'closed' ->
            {keep_state_and_data, [{reply, From, #{rstates => [closed],
                                                   wstates => [closed]}}]};
        _ ->
            {D_1, Result} =
                handle_info(State, P#params.socket, P#params.owner, D),
            {keep_state, {P, D_1}, [{reply, From, Result}]}
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
            {keep_state_and_data, [{reply, From, ok}]};
        'closed_read_write' when (How =:= read_write) ->
            %% ?DBG('already closed-read-write'),
            {keep_state_and_data, [{reply, From, ok}]};
        _ ->
            %% ?DBG({'handle shutdown', How, State}),
            case handle_shutdown(P, State, How) of
                {keep, SRes} ->
                    %% ?DBG({'shutdown result', SRes, keep}),
                    {keep_state_and_data, [{reply, From, SRes}]};
                {NextState, SRes} ->
                    next_state(P,
			       cleanup_close_read(P,
						  D#{active := false},
						  State,
						  closed),
			       NextState,
			       [{reply, From, SRes}])
            end
    end;

%% State: 'closed_read' | 'closed_read_write' - what is not handled in
%%        close/0 and shutdown/1 above
handle_event(Type, Content, State, P_D)
  when (State =:= 'closed_read') orelse (State =:= 'closed_read_write') ->
    handle_closed(Type, Content, State, P_D);


%% ------- Socket is defined from here on -----------------------------------

%% Call: bind/1
handle_event({call, From}, {bind, BindAddr} = _BIND, _State, {P, _D}) ->
    %% ?DBG(['try bind',
    %%       {handle_event, call}, {bind_addr, BindAddr}, {state, _State}]),
    Result = socket:bind(P#params.socket, BindAddr),
    %% ?DBG([{bind_result, Result}] ++ 
    %%          case Result of
    %%              ok ->
    %%                  case socket:sockname(P#params.socket) of
    %%                      {ok, SockAddr} ->
    %%                          [{sockaddr, SockAddr}];
    %%                      {error, SAReason} ->
    %%                          [{sockaddr_reason, SAReason}]
    %%                  end;
    %%              {error, BReason} ->
    %%                  [{bind_reason, BReason}]
    %%          end),
    {keep_state_and_data, [{reply, From, Result}]};


%% Call: recv/2,3 - active socket
handle_event({call, From},
	     {recv, _Length, _Timeout},
	     _State,
	     {_P, #{active := Active} = _D})
  when (Active =/= false) ->
    {keep_state_and_data, [{reply, From, {error, einval}}]};


%% 'open' | #recv{}

%% Call: recv/2,3
handle_event(
  {call, From}, {recv, Length, Timeout}, State, {P, D}) ->
    %% ?DBG([recv, {length, Length}, {timeout, Timeout}, {state, State}]),
    case State of
        'open' -> % idle
            handle_recv_start(P, D, From, Length, Timeout);
        #recv{} ->
            %% Receive already in progress
            {keep_state_and_data, [postpone]}
    end;

%% State: #recv{}
%%
%% Handle select done - try recv again
handle_event(
  info, ?socket_select(Socket, SelectRef),
  #recv{info = ?select_info(SelectRef)},
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['info socket select',
    %%       {socket, Socket}, {ref, SelectRef}, {p, P}, {d, D}]),
    handle_recv(P, D, [], recv);
handle_event(
  info, ?socket_completion(Socket, CompletionRef, CompletionStatus),
  #recv{info = ?completion_info(CompletionRef)},
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['info socket select|completion',
    %%       {socket, Socket}, {ref, CompletionRef}, {p, P}, {d, D}]),
    handle_recv(P, D, [], CompletionStatus);

%%
handle_event(
  info, ?socket_abort(Socket, SelectRef, Reason),
  #recv{info = ?select_info(SelectRef)},
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['socket abort', {reason, Reason}, {p, P}, {d, D}]),
    handle_reading(P, cleanup_recv_reply(P, D, [], Reason));
handle_event(
  info, ?socket_abort(Socket, Handle, Reason),
  #recv{info = ?completion_info(Handle)},
  {#params{socket = Socket} = P, D}) ->
    %% ?DBG(['socket abort',
    %%       {timestamp, formated_timestamp()},
    %%       {reason, Reason}, {p, P}, {d, D}]),
    handle_reading(P, cleanup_recv_reply(P, D, [], Reason));

%%
%% Timeout on recv in non-active mode
handle_event(
  {timeout, recv}, recv, #recv{} = State, {P, D}) ->
    %% ?DBG({timeout, recv}),
    handle_reading(P, cleanup_recv(P, D, State, timeout));

%% Catch-all
handle_event(Type, Content, State, P_D) ->
    handle_unexpected(Type, Content, State, P_D).

%% End of event handler
%% -------------------------------------------------------------------------
%% Event handler helpers


%% We only accept/perform shutdown when socket is 'connected'.
%% This is done to be "compatible" with the inet-driver!

handle_shutdown(#params{socket = Socket},
                open = _State,
                write = How) ->
    {keep, socket:shutdown(Socket, How)};
handle_shutdown(#params{socket = Socket},
                #recv{} = _State,
                write = How) ->
    {keep, socket:shutdown(Socket, How)};
handle_shutdown(#params{socket = Socket},
                open = _State,
                read = How) ->
    handle_shutdown2(Socket, closed_read, How);
handle_shutdown(#params{socket = Socket},
                #recv{} = _State,
                read = How) ->
    handle_shutdown2(Socket, closed_read, How);
handle_shutdown(#params{socket = Socket},
                open = _State,
                read_write = How) ->
    handle_shutdown2(Socket, closed_read_write, How);
handle_shutdown(#params{socket = Socket},
                #recv{} = _State,
                read_write = How) ->
    handle_shutdown2(Socket, closed_read_write, How);
handle_shutdown(_Params, State, _How) ->
    {keep, {error, {invalid_state, State}}}.

handle_shutdown2(Socket, NextState, How) ->
    case socket:shutdown(Socket, How) of
        ok ->
            {NextState, ok};
        Error ->
            {keep, Error}
    end.


handle_unexpected(Type, Content, State, {P, _D}) ->
    error_logger:warning_report(
      [{module, ?MODULE}, {socket, P#params.socket},
       {unknown_event, {Type, Content}}, {state, State}]),
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
            error_logger:warning_report(
              [{module, ?MODULE}, {socket, P#params.socket},
               {unknown_event, {Type, Content}}, {state, State}]),
            keep_state_and_data
    end.

%% State transition helpers -------

%% The socket was "activated" => Start reading
handle_reading('open' = _State,
               P,
               #{active := Active} = D,
               ActionsR)
  when (Active =/= false) ->
    %% ?DBG(['open', {p, P}, {d, D}, {actions_r, ActionsR}]),
    handle_recv(P, recv_start(D), ActionsR, recv);

%% The socket was "deactivated" (made passive) => Stop reading
handle_reading(#recv{info = Info} = _State,
               #params{socket = Socket} = P,
               #{active := Active} = D,
               ActionsR)
  when (Active =:= false) ->
    %% ?DBG(['recv', {info, Info},
    %%       {p, P}, {d, D}, {actions_r, ActionsR}]),
    _ = socket_cancel(Socket, Info),
    {D2, ActionsR2} = cleanup_recv_reply(P, D, ActionsR, normal),
    {next_state, 'open',
     {P, recv_stop(D2#{active := false})}, reverse(ActionsR2)};

%% No state change (either still passive, still reading,
%% or was "closed" already)
handle_reading(_State, P, D, ActionsR) ->
    {keep_state, {P, D}, ActionsR}.
    

handle_reading(P, {D, ActionsR}) ->
    handle_reading(P, D, ActionsR).

handle_reading(P, D, ActionsR) ->
    %% ?DBG([{p, P}, {d, D}, {actions_r, ActionsR}]),
    case D of
        #{active := false} ->
            {next_state, 'open', {P, D}, reverse(ActionsR)};
        #{active := _} ->
            %% ?DBG([{p, P}, {d, D}, {actions_r, ActionsR}]),
            handle_recv(P, recv_start(D), ActionsR, recv)
    end.

handle_recv_start(P, D, From, Length, Timeout) ->
    %% ?DBG([{length, Length}, {timeout, Timeout}]),
    handle_recv(P,
                D#{recv_length => Length, recv_from => From},
                [{{timeout, recv}, Timeout, recv}],
               recv).

handle_recv(#params{socket = Socket, recv_method = []} = P,
            #{recv_length := Length} = D,
            ActionsR, CS) when (CS =:= recv) ->
    %% ?DBG(['try recvfrom', {socket, Socket}, {length, Length}, {d, D}]),
    case socket_recvfrom(Socket, Length) of
        {ok, {Source, <<Data/binary>>}} ->
            %% ?DBG(['recvfrom ok',
            %%       {source, Source}, {'data sz', byte_size(Data)}]),
            handle_recv_deliver(P, D, ActionsR, {Source, Data});

        {select, ?select_info(_) = SelectInfo} ->
            %% ?DBG(['recvfrom select', {info, SelectInfo}]),
            {next_state,
             #recv{info = SelectInfo},
             {P, D},
             reverse(ActionsR)};
        {completion, ?completion_info(_) = CompletionInfo} ->
            %% ?DBG(['recvfrom completion', {info, CompletionInfo}]),
            {next_state,
             #recv{info = CompletionInfo},
             {P, D},
             reverse(ActionsR)};

        {error, Reason} ->
            %% ?DBG(['recvfrom error', {reason, Reason}]),
            handle_recv_error(P, D, ActionsR, Reason)
    end;
handle_recv(#params{recv_method = []} = P,
            D,
            ActionsR,
            CS) ->
    %% ?DBG(['recvfrom completion status']),
    case CS of
        {ok, {Source, <<Data/binary>>}} ->
            %% ?DBG(['recvfrom ok', {source, Source},
            %%       {'data sz', byte_size(Data)}]),
            handle_recv_deliver(P, D, ActionsR, {Source, Data});

        {error, Reason0} ->
            Reason =
                case Reason0 of
                    {completion_status, #{info := more_data = _INFO}} ->
                        %% ?DBG(['completion status',
                        %%       {timestamp, formated_timestamp()},
                        %%       {info, INFO},
                        %%       {p, P}, {d, D}]),
                        emsgsize;
                    {completion_status, more_data = _INFO} ->
                        %% ?DBG(['completion status',
                        %%       {timestamp, formated_timestamp()},
                        %%       {info, INFO},
                        %%       {p, P}, {d, D}]),
                        emsgsize;

                    {completion_status, #{info := INFO}} ->
                        %% ?DBG(['completion status',
                        %%       {timestamp, formated_timestamp()},
                        %%       {info, INFO},
                        %%       {p, P}, {d, D}]),
                        INFO;
                    {completion_status, INFO} ->
                        %% ?DBG(['completion status',
                        %%       {timestamp, formated_timestamp()},
                        %%       {info, INFO},
                        %%       {p, P}, {d, D}]),
                        INFO;
                    _ ->
                       Reason0 
                end,
            %% ?DBG(['recvfrom error', {reason, Reason}]),
            handle_recv_error(P, D, ActionsR, Reason)
    end;
handle_recv(#params{socket = Socket} = P,
            #{recv_length := Length} = D, ActionsR, CS) when (CS =:= recv) ->
    %% ?DBG(['try recvmsg', {socket, Socket}, {length, Length}]),
    case socket_recvmsg(Socket, Length) of
        {ok, MsgHdr} ->
            handle_recv_deliver(P, D, ActionsR, MsgHdr);

        {select, ?select_info(_) = SelectInfo} ->
            %% ?DBG(['recvmsg select', {socket_info, SelectInfo}]),
            {next_state,
             #recv{info = SelectInfo},
             {P, D},
             reverse(ActionsR)};
        {completion, ?completion_info(_) = CompletionInfo} ->
            %% ?DBG(['recvmsg select', {cinfo, CompletionInfo}]),
            {next_state,
             #recv{info = CompletionInfo},
             {P, D},
             reverse(ActionsR)};

        {error, Reason} ->
            %% ?DBG(['recvmsg error', {reason, Reason}]),
            handle_recv_error(P, D, ActionsR, Reason)
    end;
handle_recv(P, D, ActionsR, CS) ->
    %% ?DBG(['recvmsg completion status']),
    case CS of
        {ok, MsgHdr} ->
            %% ?DBG(['recvmsg success']),
            handle_recv_deliver(P, D, ActionsR, MsgHdr);

        {error, Reason} ->
            %% ?DBG(['recvmsg error', {reason, Reason}]),
            handle_recv_error(P, D, ActionsR, Reason)
    end.


handle_recv_deliver(P, D, ActionsR, Data) ->
    handle_reading(P, recv_data_deliver(P, D, ActionsR, Data)).

handle_recv_error(P, D, ActionsR, Reason) ->
    %% ?DBG([{p, P}, {d, D}, {socket, P#params.socket}, {reason, Reason}]),
    {D_1, ActionsR_1} = cleanup_recv_reply(P, D, ActionsR, Reason),
    %% ?DBG([{d1, D_1}]),
    case Reason of
        closed ->
            {next_state, 'closed_read', {P, D_1}, reverse(ActionsR_1)};
        emsgsize ->
            {next_state, 'open',
             {P, recv_stop(D#{active := false})}, reverse(ActionsR_1)};
        _ ->
            %% Temporary ... need something better here...maybe
            {next_state, 'open',
             {P, recv_stop(D#{active := false})}, reverse(ActionsR_1)}
    end.

%% -------------------------------------------------------------------------
%% Callback Helpers

next_state(P, {D, ActionsR}, State, Actions) ->
    {next_state, State, {P, D}, reverse(ActionsR, Actions)}.

cleanup_close_read(P, D, State, Reason) ->
    cleanup_recv(P, D, State, Reason).

cleanup_recv(P, D, State, Reason) ->
    %% ?DBG([{socket, P#params.socket}, {state, State}, {reason, Reason}]),    
    case State of
        #recv{info = Info} ->
            _ = socket_cancel(P#params.socket, Info),
            cleanup_recv_reply(P, D, [], Reason);
        _ ->
            cleanup_recv_reply(P, D, [], Reason)
    end.

cleanup_recv_reply(P, D, ActionsR, Reason0) ->
    Reason =
        case D of
            #{active := false} ->
                Reason0;
            #{active := _} ->
                ModuleSocket = module_socket(P),
                Owner        = P#params.owner,
                case Reason0 of
                    timeout ->
                        %% ?DBG(['error - timeout',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {p, P}, {d, D}]),
                        Owner ! {udp_error, ModuleSocket, Reason0},
                        Reason0;
                    closed ->
                        %% ?DBG(['closed',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {p, P}, {d, D}]),
                        Owner ! {udp_closed, ModuleSocket},
                        Reason0;
                    emsgsize ->
                        %% ?DBG(['error - emsgsize',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {p, P}, {d, D}]),
                        Owner ! {udp_error, ModuleSocket, Reason0},
                        Reason0;

                    %% None of these errors (completion_status) should
                    %% be cause to close the socket.
                    {completion_status, #{info := more_data = _INFO}} ->
                        %% ?DBG(['completion status',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {info, INFO}, {p, P}, {d, D},
                        %%       {mq, mq(Owner)}]),
                        R = emsgsize,
                        Owner ! {udp_error, ModuleSocket, R},
                        %% ?DBG(['udp error sent',
                        %%       {timestamp, formated_timestamp()},
                        %%       {mq, mq(Owner)}]),
                        R;
                    {completion_status, more_data = _INFO} ->
                        %% ?DBG(['completion status',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {info, INFO}, {p, P}, {d, D},
                        %%       {mq, mq(Owner)}]),
                        R = emsgsize,
                        Owner ! {udp_error, ModuleSocket, R},
                        %% ?DBG(['udp error sent',
                        %%       {timestamp, formated_timestamp()},
                        %%       {mq, mq(Owner)}]),
                        R;

                    {completion_status, #{info := INFO}} ->
                        %% ?DBG(['completion status',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {info, INFO}, {p, P}, {d, D}]),
                        Owner ! {udp_error, ModuleSocket, INFO},
                        INFO;
                    {completion_status, INFO} ->
                        %% ?DBG(['completion status',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {info, INFO}, {p, P}, {d, D}]),
                        Owner ! {udp_error, ModuleSocket, INFO},
                        INFO;

                    _ ->
                        %% ?DBG(['error and closed',
                        %%       {owner, Owner},
                        %%       {timestamp, formated_timestamp()},
                        %%       {module_socket, ModuleSocket},
                        %%       {reason, Reason0}, {p, P}, {d, D}]),
                        Owner ! {udp_error, ModuleSocket, Reason0},
                        Owner ! {udp_closed, ModuleSocket},
                        Reason0
                end
        end,
    {recv_stop(D#{active := false}),
     case D of
         #{recv_from := From} ->
             [{reply, From, {error, Reason}}, {{timeout, recv}, cancel} |
              ActionsR];
         #{} ->
             ActionsR
     end}.

%% send_udp_error_msg(Dest, Sock, Error) ->
%%     send_udp_msg(Dest, {udp_error, Sock, Error}).

%% send_udp_closed_msg(Dest, Sock) ->
%%     send_udp_msg(Dest, {udp_closed, Sock}).

%% send_udp_msg(Dest, Msg) ->
%%     erlang:send(Dest, Msg).


%% Initialize packet recv state
recv_start(D) ->
    D#{recv_length => 0}.

recv_stop(D) ->
    maps:without([recv_from, recv_length], D).


%% Deliver data and update the active state
%% -> {NewD, NewActionsR}
recv_data_deliver(
  #params{owner = Owner} = P,
  #{mode := Mode, deliver := Deliver} = D,
  ActionsR, Data) ->

    %% ?DBG([{owner, Owner}, {mode, Mode}, {deliver, Deliver}]), 

    {IP, Port, AncData, DeliverData} = deliver_data(Data, Mode),
    case D of
        #{recv_from := From} ->
            {recv_stop(D),
             [{reply, From, {ok, mk_recv_reply(IP, Port, AncData, DeliverData)}},
              {{timeout, recv}, cancel}
              | ActionsR]};
        #{active := false} ->
            {recv_stop(D), ActionsR};
        #{active := Active} ->
            %% ?DBG({active, Active}),
            ModuleSocket = module_socket(P),
            _ = deliver_recv_msg(Owner, Active, Deliver,
                                 ModuleSocket, IP, Port, AncData, DeliverData),
            %% ?DBG('package delivered'),
            case Active of
                true ->
                    {recv_start(D), ActionsR};
                once ->
                    {recv_stop(D#{active => false}), ActionsR};
                1 ->
                    Owner ! {udp_passive, ModuleSocket},
                    {recv_stop(D#{active => false}), ActionsR};
                N when is_integer(N) ->
                    {recv_start(D#{active => Active - 1}), ActionsR}
            end
    end.

mk_recv_reply(IP, Port, undefined = _AncData, Data) ->
    {IP, Port, Data};
mk_recv_reply(IP, Port, AncData, Data) ->
    {IP, Port, AncData, Data}.

deliver_recv_msg(Pid, Active, Deliver, Socket, IP, Port, AncData, Data) ->
    %% ?DBG(['deliver packet', {pid, Pid}]),
    Pid ! mk_recv_msg(Active, Deliver, Socket, IP, Port, AncData, Data).

mk_recv_msg(true = _Active, port = _Deliver,
            Socket, IP, Port, undefined, Data) ->
    {Socket, {data, [IP, Port, Data]}};
mk_recv_msg(true = _Active, port = _Deliver,
            Socket, IP, Port, AncData, Data) ->
    {Socket, {data, [IP, Port, AncData, Data]}};
mk_recv_msg(_Active, _Deliver,
            Socket, IP, Port, undefined, Data) ->
    {udp, Socket, IP, Port, Data};
mk_recv_msg(_Active, _Deliver,
            Socket, IP, Port, AncData, Data) ->
    {udp, Socket, IP, Port, AncData, Data}.


%% {Source :: sockaddr(), Data :: binary()}
%% #{addr := SockAddr :: sockaddr(),
%%   iov  := IOV      :: iovec(),
%%   ctrl := Ctrl     :: list()}
deliver_data({#{family := local = Fam, path := Path}, <<Data/binary>>}, Mode) ->
    {{Fam, Path}, 0, undefined, deliver_data_mode(Data, Mode)};
deliver_data({#{family := Fam,
                addr   := Addr,
                port   := Port}, <<Data/binary>>}, Mode)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) ->
    {Addr, Port, undefined, deliver_data_mode(Data, Mode)};
deliver_data(#{addr := #{family := local = Fam, path := Path},
               iov  := IOV,
               ctrl := Ctrl}, Mode) ->
    Data  = deliver_data_mode(IOV, Mode),
    Ctrl2 = ctrl2ancdata(Ctrl),
    {{Fam, Path}, 0, Ctrl2, Data};
deliver_data(#{addr := #{family := Fam, addr := Addr, port := Port},
               iov  := IOV,
               ctrl := Ctrl}, Mode) 
  when ((Fam =:= inet) orelse (Fam =:= inet6)) ->
    Data  = deliver_data_mode(IOV, Mode),
    Ctrl2 = ctrl2ancdata(Ctrl),
    {Addr, Port, Ctrl2, Data};
deliver_data({#{family := unspec, addr := Addr}, <<Data/binary>>}, Mode)
  when is_binary(Addr) ->
    {{unspec, Addr}, 0, undefined, deliver_data_mode(Data, Mode)};
deliver_data({Unspec, <<Data/binary>>}, Mode) when is_binary(Unspec) ->
    {{unspec, Unspec}, 0, undefined, deliver_data_mode(Data, Mode)}.
%% deliver_data(_Arg1, _Arg2) ->
%%     ?BADARG([_Arg1, _Arg2]).


deliver_data_mode(Data, list)   when is_binary(Data) -> binary_to_list(Data);
deliver_data_mode(Data, binary) when is_binary(Data) -> Data;
deliver_data_mode(IOV,  list)   when is_list(IOV) -> deliver_data_mode_bin(IOV);
deliver_data_mode(IOV,  binary) when is_list(IOV) -> iolist_to_binary(IOV).

%% Make some optimizations
deliver_data_mode_bin([Bin]) ->
    Bin;
deliver_data_mode_bin([Bin1, Bin2]) ->
    <<Bin1/binary, Bin2/binary>>;
deliver_data_mode_bin([Bin1, Bin2, Bin3]) ->
    <<Bin1/binary, Bin2/binary, Bin3/binary>>;
deliver_data_mode_bin([Bin1, Bin2, Bin3, Bin4]) ->
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary>>;
deliver_data_mode_bin(IOV) ->
    deliver_data_mode_bin(IOV, <<>>).

deliver_data_mode_bin([], Acc) ->
    Acc;
deliver_data_mode_bin([Bin|IOV], Acc) ->
    deliver_data_mode_bin(IOV, <<Bin/binary, Acc/binary>>).


ctrl2ancdata(CTRL) ->
    ctrl2ancdata(CTRL, []).

ctrl2ancdata([], AncData) ->
   lists:reverse(AncData);
ctrl2ancdata([#{level := ip,
                type  := TOS,
                value := Value,
                data  := _Data}| CTRL],
             AncData) when (TOS =:= tos) orelse (TOS =:= recvtos) ->
    ctrl2ancdata(CTRL, [{tos, Value}|AncData]);
ctrl2ancdata([#{level := ip,
                type  := TTL,
                value := Value,
                data  := _Data}| CTRL],
             AncData) when (TTL =:= ttl) orelse (TTL =:= recvttl) ->
    ctrl2ancdata(CTRL, [{ttl, Value}|AncData]);
ctrl2ancdata([#{level := ipv6,
                type  := tclass,
                value := TClass,
                data  := _Data}| CTRL],
             AncData) ->
    ctrl2ancdata(CTRL, [{tclass, TClass}|AncData]);
ctrl2ancdata([_|CTRL], AncData) ->
    ctrl2ancdata(CTRL, AncData).


%% -> {ok, NewD} | {{error, Reason}, D}
state_setopts(P, D, _State, []) ->
    {ok, {P, D}};
state_setopts(P, D, State, [Opt | Opts]) ->
    %% ?DBG([{state, State}, {opt, Opt}]),
    Opt_1 = conv_setopt(Opt),
    %% ?DBG([{'option converted', Opt_1}]),
    case setopt_categories(Opt_1) of
        %% This is a special case for options that trigger us to choose 
        %% the recv "method"; either recvfrom or recvmsg.
        %% If the recv_method list is empty => recvfrom otherwise recvmsg.
        #{socket := _, recv_method := _} ->
            RecvMethod = P#params.recv_method,
            RecvMethod2 =
                case Opt_1 of
                    {Tag, true} -> %% Add if not already added
                        M = lists:member(Tag, RecvMethod),
                        if (M) -> RecvMethod; true -> [Tag|RecvMethod] end;
                    {Tag, false} -> %% Remove
                        lists:delete(Tag, RecvMethod)
                end,
            P_1 = P#params{recv_method = RecvMethod2},
            case P_1#params.socket of
                undefined ->
                    {{error, closed}, {P, D, State}};
                Socket ->
                    case socket_setopt(Socket, Opt_1) of
                        ok ->
                            state_setopts(P_1, D, State, Opts);
                        {error, _} = Error ->
                            {Error, {P_1, D}}
                    end
            end;

        #{socket := _} ->
            %% ?DBG(socket),
            case P#params.socket of
                undefined ->
                    {{error, closed}, {P, D}};
                Socket ->
                    case socket_setopt(Socket, Opt_1) of
                        ok ->
                            state_setopts(P, D, State, Opts);
                        {error, _} = Error ->
                            {Error, {P, D}}
                    end
            end;

        %%
        #{server_write := _} when State =:= 'closed' ->
            %% ?DBG('server write when state closed'),
            {{error, einval}, {P, D}};
        #{server_write := _} ->
            %% ?DBG('server write'),
            state_setopts_server(P, D, State, Opts, Opt_1);

        %%
        #{server_read := _} when State =:= 'closed' ->
            %% ?DBG('server read when state closed'),
            {{error, einval}, {P, D}};
        #{server_read := _} when (State =:= 'closed_read') orelse
                                 (State =:= 'closed_read_write') ->
            %% ?DBG('server read when state closed-read or closed-read-write'),
            {{error, einval}, {P, D}};
        #{server_read := _} ->
            %% ?DBG('server read'),
            state_setopts_server(P, D, State, Opts, Opt_1);

        %%
        #{ignore := _} ->
            %% ?DBG(ignore),
            state_setopts(P, D, State, Opts);
        #{} = _EXTRA -> % extra | einval
            %% ?DBG({extra, _EXTRA}),
            {{error, einval}, {P, D}}
    end.

state_setopts_server(P, D, State, Opts, {Tag, Value}) ->
    case Tag of
        active ->
	    %% ?DBG(['active', {value, Value}]),
            state_setopts_active(P, D, State, Opts, Value);
        _ ->
	    %% ?DBG([{tag, Tag}, {value, Value}]),
            state_setopts(P, D#{Tag => Value}, State, Opts)
    end.

state_setopts_active(P, D, State, Opts, Active) ->
    if
        Active =:= once;
        Active =:= true ->
            %% ?DBG(['active', {state, State}, {active, Active}]),
            state_setopts(P, D#{active := Active}, State, Opts);
        Active =:= false ->
            %% ?DBG(['passive', {state, State}]),
            case D of
                #{active := OldActive} when is_integer(OldActive) ->
                    P#params.owner ! {udp_passive, module_socket(P)},
                    ok;
                _ ->
                    ok
                end,
            state_setopts(P, D#{active := Active}, State, Opts);
        is_integer(Active), -32768 =< Active, Active =< 32767 ->
            %% ?DBG(['active', {state, State}, {active, Active}]),
            N =
                case D of
                    #{active := OldActive} when is_integer(OldActive) ->
                        OldActive + Active;
                    #{active := _OldActive} ->
                        Active
                end,
            %% ?DBG(['active', {'N', N}]),
            if
                32767 < N ->
                    {{error, einval}, {P, D}};
                N =< 0 ->
                    P#params.owner ! {udp_passive, module_socket(P)},
                    state_setopts(P, D#{active := false}, State, Opts);
                true ->
                    state_setopts(P, D#{active := N}, State, Opts)
            end;
        true ->
            %% ?DBG(['error', {state, State}, {active, Active}]),
            {{error, einval}, {P, D}}
    end.

%% -> {ok, [Options]} | {error, einval}
state_getopts(P, D, State, Opts) ->
    state_getopts(P, D, State, Opts, []).
%%
state_getopts(_P, _D, _State, [], Acc) ->
    {ok, reverse(Acc)};
state_getopts(P, D, State, [Tag | Tags], Acc) ->
    case getopt_categories(Tag) of
        #{socket := _} ->
            case P#params.socket of
                undefined ->
                    {error, closed};
                Socket ->
                    case socket_getopt(Socket, Tag) of
                        {ok, Value} ->
                            state_getopts(
                              P, D, State, Tags, [{Tag, Value} | Acc]);
                        {error, _Reason} ->
                            state_getopts(P, D, State, Tags, Acc)
                    end
              end;
        #{server_write := _} when State =:= 'closed' ->
            {error, einval};
        #{server_write := _} ->
            Value = maps:get(Tag, D),
            state_getopts(P, D, State, Tags, [{Tag, Value} | Acc]);
        #{server_read := _} when State =:= 'closed' ->
            {error, einval};
        #{server_read := _} when (State =:= 'closed_read') orelse
                                 (State =:= 'closed_read_write') ->
            {error, einval};
        #{server_read := _} ->
            Value = maps:get(Tag, D),
            state_getopts(P, D, State, Tags, [{Tag, Value} | Acc]);
        #{} = _EXTRA -> % extra | einval
            {error, einval}
    end.

handle_info(State, Socket, Owner, #{active := Active} = D) ->
    %% ?DBG([{state, State}, {active, Active}]),
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
    SimpleState = simplify_state(State),
    {D_1, Info#{counters => Counters_4,
                istate   => SimpleState,
                owner    => Owner,
                active   => Active}}.

simplify_state(#recv{}) ->
    recv;
simplify_state(State) ->
    State.

    
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


%% -------------------------------------------------------------------------

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp({_N1, _N2, N3} = TS) ->
%%     {_Date, Time}   = calendar:now_to_local_time(TS),
%%     {Hour, Min, Sec} = Time,
%%     FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w",
%%                              [Hour, Min, Sec, N3 div 1000]),  
%%     lists:flatten(FormatTS).


%% mq() ->
%%     mq(self()).
%% mq(Pid) when is_pid(Pid) ->
%%     pi(Pid, messages).

%% pi(Item) when is_atom(Item) ->
%%     pi(self(), Item);
%% pi(Pid) when is_pid(Pid) ->
%%     erlang:process_info(Pid).

%% pi(Pid, Item) ->
%%     case erlang:process_info(Pid, Item) of
%%         {Item, Value} ->
%%             Value;
%%         _ ->
%%             undefined
%%     end.


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

-endif.
