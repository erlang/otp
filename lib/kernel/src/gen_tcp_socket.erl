%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2020. All Rights Reserved.
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

%% gen_tcp
-export([connect/4, listen/2, accept/2,
         send/2, recv/3,
         shutdown/2, close/1, controlling_process/2]).
%% inet
-export([setopts/2, getopts/2,
         sockname/1, peername/1,
         getstat/2]).

-ifdef(undefined).
-export([unrecv/2]).
-export([fdopen/2]).
-endif.

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).
-export([handle_event/4]).

-include("inet_int.hrl").

-define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

%% -------------------------------------------------------------------------

%% Construct a "socket" as in this module's API
-define(module_socket(Server, Socket),
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

-define(socket_abort(Socket, SelectRef, Reason),
        {'$socket', (Socket), abort, {(SelectRef), (Reason)}}).
-define(socket_select(Socket, SelectRef),
        {'$socket', (Socket), select, (SelectRef)}).
-define(socket_counter_wrap(Socket, Counter),
        {'$socket', (Socket), counter_wrap, (Counter)}).
-define(select_info(SelectRef),
        {select_info, _, (SelectRef)}).

%%% ========================================================================
%%% API
%%%

connect(Address, Port, Opts, Timeout) ->
    Timer = inet:start_timer(Timeout),
    try
        connect_lookup(Address, Port, Opts, Timer)
    after
        _ = inet:stop_timer(Timer)
    end.

%% Helpers -------

connect_lookup(Address, Port, Opts, Timer) ->
    %% ?DBG({Address, Port, Opts, Timer}),
    {EinvalOpts, Opts_1} = setopts_split(einval, Opts),
    EinvalOpts =:= [] orelse exit(badarg),
    {Mod, Opts_2} = inet:tcp_module(Opts_1, Address),
    Domain = domain(Mod),
    {StartOpts, Opts_3} = setopts_split(start, Opts_2),
    ErrRef = make_ref(),
    try
        IPs = val(ErrRef, Mod:getaddrs(Address, Timer)),
        TP = val(ErrRef, Mod:getserv(Port)),
        CO = val(ErrRef, inet:connect_options(Opts_3, Mod)),
        {sockaddrs(IPs, TP, Domain), CO}
    of
        {Addrs,
         #connect_opts{
            fd = Fd,
            ifaddr = BindIP,
            port = BindPort,
            opts = ConnectOpts}} ->
            %%
            %% ?DBG({Domain, BindIP}),
            BindAddr =
                case Domain of
                    local ->
                        {local, Path} = BindIP,
                        #{family => Domain,
                          path   => Path};
                    _ ->
                        #{family => Domain,
                          addr   => BindIP,
                          port   => BindPort}
                end,
            connect_open(
              Addrs, Domain, ConnectOpts, StartOpts, Fd, Timer, BindAddr)
    catch
        throw : {ErrRef, Reason} ->
            ?badarg_exit({error, Reason})
    end.

connect_open(Addrs, Domain, ConnectOpts, Opts, Fd, Timer, BindAddr) ->
    %% ?DBG({Addrs, Domain, ConnectOpts, Opts, Fd, Timer, BindAddr}),
    %%
    %% The {netns, File} option is passed in Fd by inet:connect_options/2.
    %% The {debug, Bool} option is passed in Opts since it is
    %% subversively classified as both start and socket option.
    %%
    ExtraOpts =
        if
            Fd =:= -1      -> [];
            is_integer(Fd) -> [{fd, Fd}];
            %% This is an **ugly** hack.
            %% inet:connect_options/2 has the bad taste to use this
            %% for [{netns,NS}] if that option is used...
            is_list(Fd)    -> Fd
        end,
    {SocketOpts, StartOpts} = setopts_split(socket, Opts),
    case
        start_server(
          Domain, ExtraOpts,
          [{timeout, inet:timeout(Timer)} | start_opts(StartOpts)])
    of
        {ok, Server} ->
            {Setopts, _} =
                setopts_split(
                  #{socket => [], server_read => [], server_write => []},
                  ConnectOpts),
            ErrRef = make_ref(),
            try
                ok(ErrRef, call(Server, {setopts, SocketOpts ++ Setopts})),
                ok(ErrRef, call(Server, {bind, BindAddr})),
                DefaultError = {error, einval},
                Socket =  
                    val(ErrRef,
                        connect_loop(Addrs, Server, DefaultError, Timer)),
                {ok, ?module_socket(Server, Socket)}
            catch
                throw : {ErrRef, Reason} ->
                    close_server(Server),
                    ?badarg_exit({error, Reason})
            end;
        {error, _} = Error ->
            ?badarg_exit(Error)
    end.

connect_loop([], _Server, Error, _Timer) -> Error;
connect_loop([Addr | Addrs], Server, _Error, Timer) ->
    Result = call(Server, {connect, Addr, inet:timeout(Timer)}),
    case Result of
        {ok, _Socket} -> Result;
        {error, badarg} -> Result;
        {error, einval} -> Result;
        {error, timeout} -> Result;
        {error, _} ->
            connect_loop(Addrs, Server, Result, Timer)
    end.

%% -------------------------------------------------------------------------

listen(Port, Opts) ->
    %% ?DBG({Port, Opts}),
    {EinvalOpts, Opts_1} = setopts_split(einval, Opts),
    EinvalOpts =:= [] orelse exit(badarg),
    {Mod, Opts_2} = inet:tcp_module(Opts_1),
    {StartOpts, Opts_3} = setopts_split(start, Opts_2),
    case Mod:getserv(Port) of
        {ok, TP} ->
            case inet:listen_options([{port, TP} | Opts_3], Mod) of
                {error, badarg} ->
                    exit(badarg);
                {ok,
                 #listen_opts{
                    fd = Fd,
                    ifaddr = BindIP,
                    port = BindPort,
                    opts = ListenOpts,
                    backlog = Backlog}} ->
                    %%
                    Domain = domain(Mod),
                    %% ?DBG({Domain, BindIP}),
                    BindAddr =
                        case Domain of
                            local ->
                                {local, Path} = BindIP,
                                #{family => Domain,
                                  path   => Path};
                            _ ->
                                #{family => Domain,
                                  addr   => BindIP,
                                  port   => BindPort}
                        end,
                    listen_open(
                      Domain, ListenOpts, StartOpts, Fd, Backlog, BindAddr)
            end;
        {error, _} = Error ->
            ?badarg_exit(Error)
    end.

%% Helpers -------

listen_open(Domain, ListenOpts, Opts, Fd, Backlog, BindAddr) ->
    %% ?DBG({Domain, ListenOpts, Opts, Fd, Backlog, BindAddr}),
    ExtraOpts =
        if
            Fd =:= -1      -> [];
            is_integer(Fd) -> [{fd, Fd}];
            %% This is an **ugly** hack.
            %% inet:connect_options/2 has the bad taste to use this
            %% for [{netns,NS}] if that option is used...
            is_list(Fd)    -> Fd
        end,
    {SocketOpts, StartOpts} = setopts_split(socket, Opts),
    case
        start_server(
          Domain, ExtraOpts,
          [{timeout, infinity} | start_opts(StartOpts)])
    of
        {ok, Server} ->
            {Setopts, _} =
                setopts_split(
                  #{socket => [], server_read => [], server_write => []},
                  ListenOpts),
            ErrRef = make_ref(),
            try
                ok(ErrRef,
                   call(
                     Server,
                     {setopts,
                      [{start_opts, StartOpts}] ++ SocketOpts ++ Setopts})),
                ok(ErrRef, call(Server, {bind, BindAddr})),
                Socket = val(ErrRef, call(Server, {listen, Backlog})),
                {ok, ?module_socket(Server, Socket)}
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

%% -------------------------------------------------------------------------

accept(?module_socket(ListenServer, ListenSocket), Timeout) ->
    %%
    Timer = inet:start_timer(Timeout),
    ErrRef = make_ref(),
    try
        #{start_opts := StartOpts} = ServerData =
            val(ErrRef, call(ListenServer, get_server_opts)),
        Server =
            val(ErrRef,
                start_server(
                 ServerData,
                 [{timeout, inet:timeout(Timer)} | start_opts(StartOpts)])),
        Socket =
            val({ErrRef, Server},
                call(Server, {accept, ListenSocket, inet:timeout(Timer)})),
        {ok, ?module_socket(Server, Socket)}
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

send(?module_socket(Server, Socket), Data) ->
    case socket:getopt(Socket, otp, meta) of
        {ok,
         #{packet := Packet,
           send_timeout := SendTimeout} = Meta} ->
            if
                Packet =:= 1;
                Packet =:= 2;
                Packet =:= 4 ->
                    Size = iolist_size(Data),
                    Header = <<?header(Packet, Size)>>,
                    Result =
                        socket_send(Socket, [Header, Data], SendTimeout),
                    send_result(Server, Meta, Result);

                true ->
                    Result = socket_send(Socket, Data, SendTimeout),
                    send_result(Server, Meta, Result)
            end;
        {ok, _BadMeta} ->
            exit(badarg);
        {error, _} = Error ->
            Error
    end.
%%
send_result(Server, Meta, Result) ->
    case Result of
        {error, {Reason, _RestData}} ->
            %% To handle RestData we would have to pass
            %% all writes through a single process that buffers
            %% the write data, which would be a bottleneck
            %%
            %% Since send data may have been lost, and there is no room
            %% in this API to inform the caller, we at least close
            %% the socket in the write direction
%%%    ?DBG(Result),
            case Reason of
                econnreset ->
                    case maps:get(show_econnreset, Meta) of
                        true -> {error, econnreset};
                        false -> {error, closed}
                    end;
                timeout ->
                    _ = maps:get(send_timeout_close, Meta)
                        andalso close_server(Server),
                    {error, Reason};
                _ ->
                    ?badarg_exit({error, Reason})
            end;
        ok ->
            ok
    end.
%%%            send_error(Server, Meta, {error, Reason});
%%%        {error, _} = Error ->
%%%            send_error(Server, Meta, Error);
%%%        ok -> ok
%%%    end.

%%%send_error(Server, Meta, Error) ->
%%%    %% Since send data may have been lost, and there is no room
%%%    %% in this API to inform the caller, we at least close
%%%    %% the socket in the write direction
%%%%%%    ?DBG(Error),
%%%    case Error of
%%%        {error, econnreset} ->
%%%            case maps:get(show_econnreset, Meta) of
%%%                true -> ?badarg_exit(Error);
%%%                false -> {error, closed}
%%%            end;
%%%        {error, timeout} ->
%%%            _ = maps:get(send_timeout_close, Meta)
%%%                andalso close_server(Server),
%%%            ?badarg_exit(Error);
%%%        _ ->
%%%            ?badarg_exit(Error)
%%%    end.

%% -------------------------------------------------------------------------

recv(?module_socket(Server, _Socket), Length, Timeout) ->
    ?badarg_exit(call(Server, {recv, Length, Timeout})).

%% -------------------------------------------------------------------------

shutdown(?module_socket(Server, Socket), How) ->
    Result =
        case How of
            write ->
                socket:shutdown(Socket, How);
            read ->
                call(Server, shutdown_read);
            read_write ->
                close_server(Server)
        end,
    ?badarg_exit(Result).

%% -------------------------------------------------------------------------

close(?module_socket(Server, _Socket)) ->
    ?badarg_exit(close_server(Server)).

%% Helpers -------

close_server(Server) ->
    Result = call(Server, close),
    stop_server(Server),
    Result.

%% -------------------------------------------------------------------------

controlling_process(?module_socket(Server, _Socket) = S, NewOwner)
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

setopts(?module_socket(Server, _Socket), Opts) when is_list(Opts) ->
    call(Server, {setopts, Opts}).

%% -------------------------------------------------------------------------

getopts(?module_socket(Server, _Socket), Opts) when is_list(Opts) ->
    call(Server, {getopts, Opts}).

%% -------------------------------------------------------------------------

sockname(?module_socket(_Server, Socket)) ->
    case socket:sockname(Socket) of
        {ok, SockAddr} -> {ok, address(SockAddr)};
        {error, _} = Error -> Error
    end.

%% -------------------------------------------------------------------------

peername(?module_socket(_Server, Socket)) ->
    case socket:peername(Socket) of
        {ok, SockAddr} -> {ok, address(SockAddr)};
        {error, _} = Error -> Error
    end.

%% -------------------------------------------------------------------------

getstat(?module_socket(Server, _Socket), What) when is_list(What) ->
    call(Server, {getstat, What}).

%%% ========================================================================
%%% Socket glue code
%%%

-compile({inline, [socket_send/3]}).
socket_send(Socket, Opts, Timeout) ->
    Result = socket:send(Socket, Opts, Timeout),
    case Result of
        {error, {epipe, Rest}} -> {error, {econnreset, Rest}};
        {error, {_Reason, _Rest}} -> Result;
        {select, _} -> Result;
        {ok, _} -> Result;
        ok -> ok
    end.

-compile({inline, [socket_recv_peek/2]}).
socket_recv_peek(Socket, Length) ->
    Options = [peek],
    Result = socket:recv(Socket, Length, Options, nowait),
%%%    ?DBG({Socket, Length, Options, Result}),
    Result.
-compile({inline, [socket_recv/2]}).
socket_recv(Socket, Length) ->
    Result = socket:recv(Socket, Length, nowait),
%%%    ?DBG({Socket, Length, Result}),
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
        ok -> ok;
        {error, closed} -> ok
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
        #{family := inet, addr := IP, port := Port} ->
            {IP, Port};
        #{family := inet6, addr := IP, port := Port} ->
            {IP, Port};
        #{family := local, path := Path} when is_list(Path) ->
            {local, prim_socket:encode_path(Path)};
        #{family := local, path := Path} when is_binary(Path) ->
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
-endif.

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

setopts_split(FilterTags, Opts) ->
    setopts_split(FilterTags, Opts, [], []).
%%
setopts_split(_FilterTags, [], True, False) ->
    {reverse(True), reverse(False)};
setopts_split(FilterTags, [Opt | Opts], True, False) ->
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
member(X, Y) when is_atom(X), is_map(Y) ->
    case Y of
        #{X := _} -> true;
        #{} -> false
    end;
member(X, Y) when is_map(X), is_map(Y) ->
    maps:fold(
      fun (_, _, true) -> true;
          (Key, _, false) -> maps:is_key(Key, Y)
      end, false, X).


conv_setopt(binary) -> {mode, binary};
conv_setopt(list) -> {mode, list};
conv_setopt(inet) -> {tcp_module, inet_tcp};
conv_setopt(inet6) -> {tcp_module, inet6_tcp};
conv_setopt(local) -> {tcp_module, local_tcp};
conv_setopt(Other) -> Other.

%% Socket options

socket_setopt(Socket, {raw, Level, Key, Value}) ->
    socket:setopt(Socket, Level, Key, Value);
socket_setopt(Socket, {raw, {Level, Key, Value}}) ->
    socket:setopt(Socket, Level, Key, Value);
socket_setopt(Socket, {Tag, Value}) ->
    case socket_opt() of
        #{Tag := {Level, Key}} ->
            socket:setopt(
              Socket, Level, Key,
              socket_setopt_value(Tag, Value));
        #{} -> {error, einval}
    end.

socket_setopt_value(_Tag, Value) -> Value.

socket_getopt(Socket, {raw, Level, Key, _Placeholder}) ->
    socket:getopt(Socket, Level, Key);
socket_getopt(Socket, {raw, {Level, Key, _Placeholder}}) ->
    socket:getopt(Socket, Level, Key);
socket_getopt(Socket, Tag) when is_atom(Tag) ->
    case socket_opt() of
        #{Tag := {Level, Key}} ->
            socket_getopt_value(
              Tag, socket:getopt(Socket, Level, Key));
        #{} -> {error, einval}
    end.

socket_getopt_value(_Tag, {ok, _Value} = Ok) -> Ok;
socket_getopt_value(_Tag, {error, _} = Error) -> Error.

socket_copy_opt(Socket, Tag, TargetSocket) when is_atom(Tag) ->
    case socket_opt() of
        #{Tag := {Level, Key}} ->
	    case socket:is_supported(Level, Key) of
		true ->
		    case socket:getopt(Socket, Level, Key) of
			{ok, Value} ->
			    socket:setopt(TargetSocket, Level, Key, Value);
			{error, _Reason} = Error ->
			    Error
		    end;
		false ->
		    ok
	    end;
        #{} = _X ->
	    {error, einval}
    end.

start_opts([{sys_debug, D} | Opts]) ->
    [{debug, D} | start_opts(Opts)];
start_opts([Opt | Opts]) ->
    [Opt | start_opts(Opts)];
start_opts([]) -> [].


%% Categories: socket, ignore, start, server_read, server_write, einval
%% returns a maps set

setopt_categories(Opt) ->
    case Opt of
        {raw, _, _, _} -> #{socket => []};
        {raw, {_, _, _}} -> #{socket => []};
        {Tag, _} -> opt_categories(Tag);
        _ -> ignore
    end.

getopt_categories(Opt) ->
    case Opt of
        {raw, _, _, _} -> #{socket => []};
        {raw, {_, _, _}} -> #{socket => []};
        _ -> opt_categories(Opt)
    end.

%% setopt and getopt category
opt_categories(Tag) when is_atom(Tag) ->
    case Tag of
        sys_debug -> #{start => []};
        debug -> #{socket => [], start => []};
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
      %% Handled by inet:tcp_module/2
      tcp_module => [],
      %% Handled by inet:connect_options/2 and inet:listen_options/2
      ip => [],
      backlog => [],
      %% XXX Some of these must probably be handled one day...
      high_msgq_watermark => [],
      high_watermark => [],
      low_msgq_watermark => [],
      nopush => []
      }.

%% Category 'socket'
%%
%% Translation to level and type
-compile({inline, [socket_opt/0]}).
socket_opt() ->
    #{%% Level: otp
      buffer => {otp, rcvbuf},
      debug  => {otp, debug},
      fd     => {otp, fd},
      %%
      %% Level: socket
      bind_to_device => {socket, bindtodevice},
      dontroute => {socket, dontroute},
      keepalive => {socket, keepalive},
      linger => {socket, linger},
      low_watermark => {socket, rcvlowat},
      priority => {socket, priority},
      recbuf => {socket, rcvbuf},
      reuseaddr => {socket, reuseaddr},
      sndbuf => {socket, sndbuf},
      %%
      %% Level: tcp
      nodelay => {tcp, nodelay},
      %%
      %% Level: ip
      recvtos => {ip, recvtos},
      recvttl => {ip, recvttl},
      tos => {ip, tos},
      ttl => {ip, ttl},
      %%
      %% Level: ipv6
      recvtclass => {ipv6, recvtclass},
      ipv6_v6only => {ipv6, v6only}
      }.

-compile({inline, [socket_inherit_opts/0]}).
socket_inherit_opts() ->
    [priority].

-compile({inline, [server_read_write_opts/0]}).
server_read_write_opts() ->
    %% Common for read and write side
    #{packet => raw,
      packet_size => 16#4000000, % 64 MByte
      show_econnreset => false}.
-compile({inline, [server_read_opts/0]}).
server_read_opts() ->
    %% Read side only opts
    maps:merge(
      #{active => true,
        mode => list,
        header => 0,
        deliver => term,
        start_opts => [], % Just to make it settable
        %% XXX not implemented yet
        exit_on_close => true,
        line_delimiter => $\n},
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
start_server(Domain, ExtraOpts, StartOpts) ->
    Owner = self(),
    Arg = {open, Domain, ExtraOpts, Owner},
    case gen_statem:start(?MODULE, Arg, StartOpts) of
        {ok, Server} -> {ok, Server};
        {error, _} = Error -> Error
    end.

%% Start for accept - have no socket yet
start_server(ServerData, StartOpts) ->
    Owner = self(),
    Arg = {prepare, ServerData, Owner},
    case gen_statem:start(?MODULE, Arg, StartOpts) of
        {ok, Server} -> {ok, Server};
        {error, _} = Error -> Error
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
%%
-record(controlling_process,
        {owner :: pid(),
         state :: term()}).
%% A super state that encapsulates any other state
%% and postpones all events but get_server_opts/0
%% and Owner 'DOWN'

%% 'accept'
-record(accept,
        {info :: socket:select_info(),
         from :: gen_statem:from(),
         listen_socket :: socket:socket()}).
%% Socket is not created

%% 'connect' % A listen socket stays here
-record(connect,
        {info :: socket:select_info(),
         from :: gen_statem:from(),
         addr :: socket:sockaddr()}).

%% 'connected'
-record(recv,
        {info :: socket:select_info()}).

%% 'closed_read'
%% 'closed' % Socket is closed or not created


-record(params,
        {socket :: undefined | socket:socket(),
         owner :: pid(),
         owner_mon :: reference()}).

init({open, Domain, ExtraOpts, Owner}) ->
    %% Listen or Connect
    %%
    process_flag(trap_exit, true),
    OwnerMon = monitor(process, Owner),
    Extra = maps:from_list(ExtraOpts),
    Proto = if (Domain =:= local) -> default; true -> tcp end,
    case socket:open(Domain, stream, Proto, Extra) of
        {ok, Socket} ->
            D  = server_opts(),
            ok = socket:setopt(Socket, otp, iow, true),
            ok = socket:setopt(Socket, otp, meta, meta(D)),
            P =
                #params{
                   socket = Socket,
                   owner = Owner,
                   owner_mon = OwnerMon},
            {ok, connect, {P, D#{buffer => <<>>}}};
        {error, Reason} -> {stop, {shutdown, Reason}}
    end;
init({prepare, D, Owner}) ->
    %% Accept
    %%
    process_flag(trap_exit, true),
    OwnerMon = monitor(process, Owner),
    P =
        #params{
           owner = Owner,
           owner_mon = OwnerMon},
    {ok, accept, {P, D#{buffer => <<>>}}};
init(Arg) ->
    error_logger:error_report([{badarg, {?MODULE, init, [Arg]}}]),
    error(badarg, [Arg]).

terminate(_Reason, State, P_D) ->
    case State of
        #controlling_process{state = OldState} ->
            terminate(OldState, P_D);
        _ ->
            terminate(State, P_D)
    end.
%%
terminate(State, {#params{socket = Socket} = P, D}) ->
    case State of
        'closed' -> ok;
        'closed_read' ->
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
    ?module_socket(self(), Socket).

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
    {keep_state, {P, wrap_counter(Counter, D)}};
handle_event(
  info, ?socket_counter_wrap(Socket, Counter),
  #recv{} = _State, {#params{socket = Socket} = P, D}) ->
    {keep_state, {P, wrap_counter(Counter, D)}};
handle_event(
  info, ?socket_counter_wrap(_Socket, _Counter), _State, _P_D) ->
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
    NewOwnerMon = monitor(process, NewOwner),
    true = demonitor(OwnerMon, [flush]),
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
    case State of
        'closed_read' ->
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
    Result = state_getopts(P, D, State, Opts),
    {keep_state_and_data,
     [{reply, From, Result}]};

%% Call: setopts/1
handle_event({call, From}, {setopts, Opts}, State, {P, D}) ->
    {Result, D_1} = state_setopts(P, D, State, Opts),
    ok = socket:setopt(P#params.socket, otp, meta, meta(D_1)),
    Reply = {reply, From, Result},
    case State of
        'connected' ->
            handle_connected(
              P, D_1,
              [Reply]);
        _ ->
            {keep_state, {P, D_1},
             [Reply]}
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

%% State: 'closed' - what is not handled above
handle_event(Type, Content, 'closed' = State, P_D) ->
    handle_closed(Type, Content, State, P_D);
%% Handled state: 'closed'

%% Call: shutdown/1
handle_event({call, From}, shutdown_read, State, {P, D}) ->
    case State of
        'closed_read' ->
            {keep_state_and_data,
             [{reply, From, ok}]};
        _ ->
            next_state(
              P,
              cleanup_close_read(P, D#{active := false}, State, closed),
              'closed_read',
              [{reply, From, socket:shutdown(P#params.socket, read)}])
    end;
%% State: 'closed_read' - what is not handled in
%%        close/0 and shutdown/1 above
handle_event(Type, Content, 'closed_read' = State, P_D) ->
    handle_closed(Type, Content, State, P_D);



%% State: 'accept'
handle_event(
  {call, From}, {accept, ListenSocket, Timeout},
  'accept' = _State, {P, D}) ->
    handle_accept(P, D, From, ListenSocket, Timeout);
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
    handle_accept(P, D, From, ListenSocket, update);
handle_event(
  info, ?socket_abort(ListenSocket, SelectRef, Reason),
  #accept{
     info = ?select_info(SelectRef), from = From,
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
    socket_cancel(ListenSocket, SelectInfo),
    {next_state, 'closed', {P, D},
     [{reply, From, {error, timeout}}]};
handle_event(Type, Content, #accept{} = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);
%% Handled states: 'accept' | #accept{}

%% ------- Socket is defined from here on -----------------------------------

%% Call: bind/1
handle_event({call, From}, {bind, BindAddr}, _State, {P, _D}) ->
    Result =
        case socket:bind(P#params.socket, BindAddr) of
            %% XXX Should we store Port with BindAddr as sockname?
            %%     Should bind return port?
            %%     There is no port for domain = unix
            {ok, _Port} -> ok;
            {error, _} = Error -> Error
        end,
    {keep_state_and_data,
     [{reply, From, Result}]};

%% It is a bit arbitrary that {listen, _} returns {ok, Socket},
%% since Socket is known since start_server, but has not been returned
%% to listen/1 yet.  It could be returned from {bind, _},
%% or from a separate get_socket call, but piggy-backing it
%% on {listen, _} is convenient.

%% Call: listen/1
handle_event(
  {call, From}, {listen, Backlog},
  _State, {#params{socket = Socket} = _P, _D}) ->
    Result =
        case socket:listen(Socket, Backlog) of
            ok -> {ok, Socket};
            {error, _} = Error -> Error
        end,
    {keep_state_and_data,
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
    handle_connect(P, D, From, Addr, Timeout);
%%
%% Call: recv/2 - not connected
handle_event(
  {call, From}, {recv, _Length, _Timeout}, 'connect' = _State, _P_D) ->
    {keep_state_and_data,
     [{reply, From, {error, enotconn}}]};
handle_event(Type, Content, 'connect' = State, P_D) ->
    handle_unexpected(Type, Content, State, P_D);
%%
%% State: #connect{}
handle_event(
  info, ?socket_select(Socket, SelectRef),
  #connect{
     info = ?select_info(SelectRef), from = From, addr = Addr} = _State,
  {#params{socket = Socket} = P, D}) ->
    handle_connect(P, D, From, Addr, update);
handle_event(
  info, ?socket_abort(Socket, SelectRef, Reason),
  #connect{info = ?select_info(SelectRef), from = From} = _State,
  {#params{socket = Socket} = _P, _D} = P_D) ->
    _ = socket_close(Socket),
    {next_state, 'closed', P_D,
     [{reply, From, {error, Reason}}]};
handle_event(
  {timeout, connect}, connect,
  #connect{info = SelectInfo, from = From},
  {#params{socket = Socket} = _P, _D} = P_D) ->
    socket_cancel(Socket, SelectInfo),
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
  #recv{info = ?select_info(SelectRef)},
  {#params{socket = Socket} = P, D}) ->
    %%
    handle_recv(P, D, []);
%%
handle_event(
  info, ?socket_abort(Socket, SelectRef, Reason),
  #recv{info = ?select_info(SelectRef)},
  {#params{socket = Socket} = P, D}) ->
    %%
    handle_connected(P, cleanup_recv_reply(P, D, [], Reason));
%%
%% Timeout on recv in non-active mode
handle_event(
  {timeout, recv}, recv, #recv{} = State, {P, D}) ->
    %%
    handle_connected(P, cleanup_recv(P, D, State, timeout));

%% Catch-all
handle_event(Type, Content, State, P_D) ->
    handle_unexpected(Type, Content, State, P_D).

%% End of event handler
%% -------------------------------------------------------------------------
%% Event handler helpers

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

handle_connect(
  #params{socket = Socket} = P, D, From, Addr, Timeout) ->
    %%
    case socket:connect(Socket, Addr, nowait) of
        ok ->
            handle_connected(
              P, D,
              [{{timeout, connect}, cancel},
               {reply, From, {ok, Socket}}]);
        {select, SelectInfo} ->
            {next_state,
             #connect{info = SelectInfo, from = From, addr = Addr},
             {P, D},
             [{{timeout, connect}, Timeout, connect}]};
        {error, _} = Error ->
            {next_state,
             'connect', {P, D},
             [{{timeout, connect}, cancel},
              {reply, From, Error}]}
    end.

handle_accept(P, D, From, ListenSocket, Timeout) ->
    case socket:accept(ListenSocket, nowait) of
        {ok, Socket} ->
            ok = socket:setopt(Socket, otp, iow, true),
            ok = socket:setopt(Socket, otp, meta, meta(D)),
            [ok = socket_copy_opt(ListenSocket, Opt, Socket)
             || Opt <- socket_inherit_opts()],
            handle_connected(
              P#params{socket = Socket}, D,
              [{{timeout, accept}, cancel},
               {reply, From, {ok, Socket}}]);
        {select, SelectInfo} ->
            {next_state,
             #accept{
                info = SelectInfo, from = From,
                listen_socket = ListenSocket},
             {P, D},
             [{{timeout, accept}, Timeout, accept}]};
        {error, _} = Error ->
            {next_state,
             'accept', {P, D},
             [{{timeout, accept}, cancel},
              {reply, From, Error}]}
    end.

handle_connected(P, {D, ActionsR}) ->
    handle_connected(P, D, ActionsR).
%%
handle_connected(P, D, ActionsR) ->
    case D of
        #{active := false} ->
            {next_state, 'connected',
             {P, D},
             reverse(ActionsR)};
        #{active := _} ->
            handle_recv(P, recv_start(D), ActionsR)
    end.

handle_recv_start(
  P, #{packet := Packet, buffer := Buffer} = D, From, Length, Timeout)
  when Packet =:= raw, 0 < Length;
       Packet =:= 0, 0 < Length ->
    Size = iolist_size(Buffer),
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
              [{{timeout, recv}, Timeout, recv}])
    end;
handle_recv_start(P, D, From, _Length, Timeout) ->
    handle_recv(
      P, D#{recv_length => 0, recv_from => From},
      [{{timeout, recv}, Timeout, recv}]).

handle_recv(P, #{packet := Packet, recv_length := Length} = D, ActionsR) ->
    if
        0 < Length ->
            handle_recv_length(P, D, ActionsR, Length);
        Packet =:= raw;
        Packet =:= 0 ->
            handle_recv_length(P, D, ActionsR, Length);
        Packet =:= 1;
        Packet =:= 2;
        Packet =:= 4 ->
            handle_recv_peek(P, D, ActionsR, Packet);
        true ->
            handle_recv_packet(P, D, ActionsR)
    end.

handle_recv_peek(P, D, ActionsR, Packet) ->
    %% Peek Packet bytes
    case D of
        #{buffer := Buffer} when is_list(Buffer) ->
            Data = condense_buffer(Buffer),
            handle_recv_peek(P, D#{buffer := Data}, ActionsR, Packet);
        #{buffer := <<Data:Packet/binary, _Rest/binary>>} ->
            handle_recv_peek(P, D, ActionsR, Packet, Data);
        #{buffer := <<ShortData/binary>>} ->
            N = Packet - byte_size(ShortData),
            case socket_recv_peek(P#params.socket, N) of
                {ok, <<FinalData/binary>>} ->
                    handle_recv_peek(
                      P, D, ActionsR, Packet,
                      <<ShortData/binary, FinalData/binary>>);
                {ok, {_, SelectInfo}} ->
                    {next_state,
                     #recv{info = SelectInfo},
                     {P, D},
                     reverse(ActionsR)};
                {select, SelectInfo} ->
                    {next_state,
                     #recv{info = SelectInfo},
                     {P, D},
                     reverse(ActionsR)};
                {error, {Reason, <<_Data/binary>>}} ->
                    handle_recv_error(P, D, ActionsR, Reason);
                {error, Reason} ->
                    handle_recv_error(P, D, ActionsR, Reason)
            end
    end.

handle_recv_peek(P, D, ActionsR, Packet, Data) ->
    <<?header(Packet, N)>> = Data,
    #{packet_size := PacketSize} = D,
    if
        0 < PacketSize, PacketSize < N ->
            handle_recv_error(P, D, ActionsR, emsgsize);
        true ->
            handle_recv_length(P, D, ActionsR, Packet + N)
    end.

handle_recv_packet(P, D, ActionsR) ->
    case D of
        #{buffer := Buffer} when is_list(Buffer) ->
            Data = condense_buffer(Buffer),
            handle_recv_decode(P, D, ActionsR, Data);
        #{buffer := Data} when is_binary(Data) ->
            handle_recv_more(P, D, ActionsR, Data)
    end.

handle_recv_length(P, #{buffer := Buffer} = D, ActionsR, Length) ->
    handle_recv_length(P, D, ActionsR, Length, Buffer).
%%
%% Here and downwards until handle_recv_deliver() all buffered data
%% is the last argument binary and D#{buffer} is not updated
%%
handle_recv_length(P, D, ActionsR, Length, Buffer) when 0 < Length ->
    case socket_recv(P#params.socket, Length) of
        {ok, <<Data/binary>>} ->
            handle_recv_deliver(
              P, D#{buffer := <<>>}, ActionsR,
              condense_buffer([Data | Buffer]));
        {ok, {Data, SelectInfo}} ->
            N = Length - byte_size(Data),
            {next_state,
             #recv{info = SelectInfo},
             {P, D#{buffer := [Data | Buffer], recv_length := N}},
             reverse(ActionsR)};
        {select, SelectInfo} ->
            {next_state,
             #recv{info = SelectInfo},
             {P, D#{buffer := Buffer}},
             reverse(ActionsR)};
        {error, {Reason, <<Data/binary>>}} ->
            %% Error before all data
            handle_recv_error(
              P, D#{buffer := [Data | Buffer]}, ActionsR, Reason);
        {error, Reason} ->
            handle_recv_error(P, D#{buffer := Buffer}, ActionsR, Reason)
    end;
handle_recv_length(P, D, ActionsR, _0, Buffer) ->
    case Buffer of
        <<>> ->
            %% We should not need to update the buffer field here
            %% since the only way to get here with empty Buffer
            %% is when Buffer comes from the buffer field
            Socket = P#params.socket,
            case socket_recv(Socket, 0) of
                {ok, <<Data/binary>>} ->
                    handle_recv_deliver(P, D, ActionsR, Data);
                {ok, {Data, SelectInfo}} ->
                    case socket:cancel(Socket, SelectInfo) of
                        ok ->
                            handle_recv_deliver(P, D, ActionsR, Data);
                        {error, Reason} ->
                            handle_recv_error(P, D, ActionsR, Reason, Data)
                    end;
                {select, SelectInfo} ->
                    {next_state,
                     #recv{info = SelectInfo},
                     {P, D},
                     reverse(ActionsR)};
                {error, {Reason, <<Data/binary>>}} ->
                    handle_recv_error(P, D, ActionsR, Reason, Data);
                {error, Reason} ->
                    handle_recv_error(P, D, ActionsR, Reason)
            end;
        <<Data/binary>> ->
            handle_recv_deliver(P, D#{buffer := <<>>}, ActionsR, Data);
        _ when is_list(Buffer) ->
            Data = condense_buffer(Buffer),
            handle_recv_deliver(P, D#{buffer := <<>>}, ActionsR, Data)
    end.

handle_recv_decode(P, #{packet_size := PacketSize} = D, ActionsR, Data) ->
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
            handle_recv_deliver(P, D#{buffer := Buffer}, ActionsR, Decoded);
        {more, undefined} ->
            handle_recv_more(P, D, ActionsR, Data);
        {more, Length} ->
            N = Length - byte_size(Data),
            handle_recv_length(P, D, ActionsR, N, Data);
        {error, Reason} ->
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

handle_recv_more(P, D, ActionsR, BufferedData) ->
    case socket_recv(P#params.socket, 0) of
        {ok, <<MoreData/binary>>} ->
            Data = catbin(BufferedData, MoreData),
            handle_recv_decode(P, D, ActionsR, Data);
        {select, SelectInfo} ->
            {next_state,
             #recv{info = SelectInfo},
             {P, D#{buffer := BufferedData}},
             reverse(ActionsR)};
        {error, {Reason, <<MoreData/binary>>}} ->
            Data = catbin(BufferedData, MoreData),
            handle_recv_error_decode(P, D, ActionsR, Reason, Data);
        {error, Reason} ->
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
%%%    ?DBG(Reason),
    {D_1, ActionsR_1} =
        cleanup_recv_reply(P, D#{buffer := <<>>}, ActionsR, Reason),
    case Reason of
        closed ->
            {next_state, 'closed_read', {P, D_1}, reverse(ActionsR_1)};
        econnreset ->
            _ = socket_close(P#params.socket),
            {next_state, 'closed', {P, D_1}, reverse(ActionsR_1)};
        emsgsize ->
            {next_state, 'connected',
             {P, recv_stop(D#{active := false})},
             reverse(ActionsR_1)}
    end.

%% -------------------------------------------------------------------------
%% Callback Helpers

next_state(P, {D, ActionsR}, State, Actions) ->
    {next_state, State, {P, D}, reverse(ActionsR, Actions)}.

cleanup_close_read(P, D, State, Reason) ->
    case State of
        #accept{
           info = SelectInfo, from = From, listen_socket = ListenSocket} ->
            socket_cancel(ListenSocket, SelectInfo),
            {D,
             [{reply, From, {error, Reason}}]};
        #connect{info = SelectInfo, from = From} ->
            socket_cancel(P#params.socket, SelectInfo),
            {D,
             [{reply, From, {error, Reason}}]};
        _ ->
            cleanup_recv(P, D, State, Reason)
    end.

cleanup_recv(P, D, State, Reason) ->
    case State of
        #recv{info = SelectInfo} ->
            socket_cancel(P#params.socket, SelectInfo),
            cleanup_recv_reply(P, D, [], Reason);
        _ ->
            cleanup_recv_reply(P, D, [], Reason)
    end.

cleanup_recv_reply(
  P, #{show_econnreset := ShowEconnreset} = D, ActionsR, Reason) ->
    case D of
        #{active := false} -> ok;
        #{active := _} ->
            ModuleSocket = module_socket(P),
            Owner = P#params.owner,
%%%            ?DBG({ModuleSocket, Reason}),
            case Reason of
                timeout ->
                    Owner ! {tcp_error, ModuleSocket, Reason},
                    ok;
                closed ->
                    Owner ! {tcp_closed, ModuleSocket},
                    ok;
                emsgsize ->
                    Owner ! {tcp_error, ModuleSocket, Reason},
                    ok;
                econnreset when ShowEconnreset =:= false ->
                    Owner ! {tcp_closed, ModuleSocket},
                    ok;
                _ ->
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

%% -> {ok, NewD} | {{error, Reason}, D}
state_setopts(_P, D, _State, []) ->
    {ok, D};
state_setopts(P, D, State, [Opt | Opts]) ->
    Opt_1 = conv_setopt(Opt),
    case setopt_categories(Opt_1) of
        #{socket := _} ->
            case P#params.socket of
                undefined ->
                    {{error, closed}, D};
                Socket ->
                    case socket_setopt(Socket, Opt_1) of
                        ok ->
                            state_setopts(P, D, State, Opts);
                        {error, _} = Error ->
                            {Error, D}
                    end
            end;
        %%
        #{server_write := _} when State =:= 'closed' ->
            {{error, einval}, D};
        #{server_write := _} ->
            state_setopts_server(P, D, State, Opts, Opt_1);
        %%
        #{server_read := _} when State =:= 'closed' ->
            {{error, einval}, D};
        #{server_read := _} when State =:= 'closed_read' ->
            {{error, einval}, D};
        #{server_read := _} ->
            state_setopts_server(P, D, State, Opts, Opt_1);
        %%
        #{ignore := _} ->
            state_setopts(P, D, State, Opts);
        #{} -> % extra | einval
            {{error, einval}, D}
    end.

state_setopts_server(P, D, State, Opts, {Tag, Value}) ->
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
            state_setopts(P, D#{Tag => Value}, State, Opts)
    end.

state_setopts_active(P, D, State, Opts, Active) ->
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
                        {error, _} ->
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
        #{server_read := _} when State =:= 'closed_read' ->
            {error, einval};
        #{server_read := _} ->
            Value = maps:get(Tag, D),
            state_getopts(P, D, State, Tags, [{Tag, Value} | Acc]);
        #{} -> % extra | einval
            {error, einval}
    end.


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
            receive_counter_wrap(
              Socket, wrap_counter(Counter, D) , [Counter | Wrapped])
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

-endif.
