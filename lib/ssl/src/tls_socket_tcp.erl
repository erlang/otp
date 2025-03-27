%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose:  Use socket directly in tls instead of gen_tcp
%%----------------------------------------------------------------------

-module(tls_socket_tcp).
-moduledoc false.

-export([cb_info/0,

         setopts/2,
         getopts/2,
         getstat/2,
         peername/1,
         sockname/1,
         port/1,
         controlling_process/2,

         listen/2,
         accept/2,
         connect/4,
%%         open/2,   dtls only
         close/1,
         shutdown/2,
         send/2,
%%         send/4,
         recv/2, recv/3,

         data_available/4
        ]).

-include("ssl_internal.hrl").

%% -define(DBG_LOG(F,A), ct:log(default, 1, "~w:~w: " ++ F, [?MODULE, ?LINE|A], [esc_chars])).
-define(DBG_LOG(F,A), ok).

cb_info() ->
    %% {gen_tcp, tcp, tcp_closed, tcp_error, tcp_passive}
    {?MODULE, '$socket', '$socket_closed', '$socket', '$socket_passive'}.

setopts(Socket, List) ->
    try
        Opts = check_opts(List),
        [ok = setopt(Socket, Opt, Val) || Opt := Val <- Opts],
        ok
    catch _:Err ->
            Err
    end.

getopts(Socket, Keys) ->
    try
        Get = fun(Key, Acc) ->
                      case maps:get(Key, socket_opts(), not_existing_opt) of
                          not_existing_opt ->
                              throw({error, {not_supported, Key}});
                          inet_option_only ->
                              Acc;
                          SocketKey ->
                              {ok, Val} = socket:getopt(Socket, SocketKey),
                              [{Key, Val}|Acc]
                      end
              end,
        {ok, lists:foldr(Get, [], Keys)}
    catch _:Err ->
            Err
    end.

getstat(Socket, Opts) ->
    #{counters := Counters} = socket:info(Socket),
    {ok, getstat_what(Opts, Counters)}.

peername(Socket) ->
    case socket:peername(Socket) of
        {ok, #{addr := Addr, port := Port}} ->
            {ok, {Addr, Port}};
        Err ->
            Err
    end.

sockname(Socket) ->
    case socket:sockname(Socket) of
        {ok, #{addr := Addr, port := Port}} ->
            {ok, {Addr, Port}};
        Err ->
            Err
    end.

port(Socket) ->
    case socket:sockname(Socket) of
        {ok, #{port := Port}} ->
            {ok, Port};
        Err ->
            Err
    end.

controlling_process(Socket, NewOwner) ->
    socket:setopt(Socket, {otp, controlling_process}, NewOwner).

listen(Port, Opts1) when is_integer(Port) ->
    {Mod, Opts2} = inet:tcp_module(Opts1),
    SetOpts = check_opts(Opts2),
    maybe
        BindAddr = get_addr(Opts2, undefined, Mod),
        Domain = domain(Mod),
        Fd = maps:get(fd, SetOpts, -1),
        {ok, Sock} ?= socket_open(Fd, Domain, stream, tcp),
        SockAddr = bind_addr(Domain, BindAddr, Port),
        ok ?= socket:bind(Sock, SockAddr),
        [ok = socket:setopt(Sock, Opt, Val)
         || {_,_}=Opt := Val <- SetOpts,
            Opt =/= {otp,fd}
        ],
        Backlog = maps:get(backlog, SetOpts, 5),
        ok ?= socket:listen(Sock, Backlog),
        {ok, Sock}
    else
        {error, _} = Error -> Error;
        Reason -> {error, Reason}
    end.

accept(Socket, Timeout) ->
    socket:accept(Socket, Timeout).

-spec connect(Address, Port, Opts, Timeout) ->
                     {ok, Socket} | {error, Reason} when
      Address :: inet:socket_address() | inet:hostname(),
      Port    :: inet:port_number(),
      Opts    :: [inet:inet_backend() | gen_tcp:connect_option()],
      Timeout :: timeout(),
      Socket  :: socket:socket(),
      Reason  :: timeout | inet:posix().

connect(Address, Port, Opts0, Timeout) ->
    Opts1 = check_opts(Opts0),
    case inet:is_ip_address(Address) of
        true ->
            connect_1(Address, Port, Opts1, Timeout, {error,einval});
        false ->
            {Mod, Opts} = inet:tcp_module(maps:to_list(Opts1), Address),
            case Mod:getaddrs(Address, Timeout) of
                {ok, IPs} ->
                    connect_1(IPs, Port, Opts, Timeout, {error,einval});
                Error ->
                    Error
            end
    end.

close(Socket) ->
    socket:close(Socket).

shutdown(Socket, How) ->
    ?DBG_LOG("~w Shutdown ~w ~w~n",[get(tls_role), Socket, How]),
    socket:shutdown(Socket, How).

recv(Socket, Size) ->
    recv(Socket, Size, infinity).
recv(Socket, Size, Timeout) ->
    socket:recv(Socket, Size, [], Timeout).

send(Socket, Data) ->
    ?DBG_LOG("~w send ~w", [get(tls_role), iolist_size(Data)]),
    case socket:sendv(Socket, erlang:iolist_to_iovec(Data)) of
        ok ->
            ok;
        {ok, Cont} ->
            ?DBG_LOG("~w send loop ~w", [get(tls_role), iolist_size(Cont)]),
            send(Socket, Cont);
        Err ->
            ?DBG_LOG("~w send ~w", [get(tls_role), Err]),
            Err
    end.

%% Note: returns the reverse list of packets
data_available(Socket, completion, {Handle, Res}, Activate) ->
    data_available_result(Res, Socket, Handle, Activate, []);
data_available(Socket, select, Handle, Activate) ->
    data_available_recv(Socket, Handle, Activate, []).

data_available_recv(Socket, Handle, Activate, Acc) ->
    data_available_result(socket:recv(Socket, 0, Handle), Socket, Handle, Activate, Acc).

data_available_result(Res, Socket, __Handle, Activate, Acc) ->
    case Res of
        {ok, Data} ->
            case Activate of
                true ->
                    ?DBG_LOG("~w data", [get(tls_role)]),
                    self() ! {'$socket', Socket, select, nowait},
                    [Data|Acc];
                false -> %% Activate is false, fake passive message
                    ?DBG_LOG("~w passive", [get(tls_role)]),
                    ok = socket:setopt(Socket, {otp,select_read}, false),
                    self() ! {'$socket_passive', Socket},
                    [Data|Acc]
            end;
        {select_read, {{select_info, _, _Handle}, Data}} ->
            ?DBG_LOG("~w wait_select ~p", [get(tls_role), _Handle]),
            [Data|Acc];
        {completion, {_, recv, _Handle}} ->
            %% Windows
            ?DBG_LOG("~w wait_select ~p", [get(tls_role), _Handle]),
            Acc;
        {select, {_, recv, _Handle}} ->
            %% First time
            ?DBG_LOG("~w wait_select ~p", [get(tls_role), _Handle]),
            Acc;
        {error, Reason} ->
            ?SSL_LOG(debug, socket_error, [{error, Reason}]),
            ?DBG_LOG("~w error ~p", [get(tls_role), Reason]),
            self() ! {'$socket_closed', Socket},
            Acc
    end.

%% Helpers

setopt(Socket, active, N) ->
    if
        N == true; N == 1 ->
            ?DBG_LOG("Send socket select", [1]),
            self() ! {'$socket', Socket, select, nowait},
            ok;
        N == false ->
            ok = socket:setopt(Socket, {otp,select_read}, false);
        N > 1 ->
            ?DBG_LOG("~p Set active ~w", [self(), N]),
            self() ! {'$socket', Socket, select, nowait},
            ok = socket:setopt(Socket, {otp,select_read}, true);
        true ->
            ?DBG_LOG("ignore active ~w", [N]),
            ok
    end;
setopt(Socket, {_,_}=Opt, Val) ->
    ok = socket:setopt(Socket, Opt, Val);
setopt(_Socket, _Opt, _Val) ->
    ?DBG_LOG("setopt: Ignore: ~p ~p", [_Opt, _Val]),
    ok.

connect_1([IP|IPs], Port, Opts, Timeout, _Err) ->
    Family = which_family(IP),
    SockAddr = #{family => Family, addr => IP, port => Port},
    {ok, Socket} = socket:open(Family, stream, tcp),
    ok = socket:bind(Socket, SockAddr#{port => 0}),
    [ok = socket:setopt(Socket, Opt, Val) || {{_,_}=Opt, Val} <- Opts],
    case socket:connect(Socket, SockAddr, Timeout) of
        ok ->
            {ok, Socket};
        Err ->
            socket:close(Socket),
            connect_1(IPs, Port, Opts, Timeout, Err)
    end;
connect_1([], _Port, _Opts, _Timeout, Err) ->
    Err.

socket_open(Fd, Domain, Type, Protocol) when Fd < 0 ->
    socket:open(Domain, Type, Protocol);
socket_open(Fd, Domain, Type, Protocol) ->
    socket:open(Fd, #{domain => Domain, type => Type, protocol => Protocol}).

get_addr([{ip, Ip}|Rest], _Prev, Mod) ->
    get_addr(Rest, Ip, Mod);
get_addr([{ifaddr, #{addr := Addr}}|Rest], _Prev, Mod) ->
    get_addr(Rest, Addr, Mod);
get_addr([{ifaddr, Addr}|Rest], _Prev, Mod) ->
    get_addr(Rest, Addr, Mod);
get_addr([_|Rest], Prev, Mod) ->
    get_addr(Rest, Prev, Mod);
get_addr([], Addr, Mod) ->
    Mod:translate_ip(Addr).

bind_addr(Domain, undefined, Port) ->
    #{family => Domain,
      addr   => any,
      port   => Port};
bind_addr(Domain, Addr, Port) ->
    #{family => Domain,
      addr   => Addr,
      port   => Port}.

which_family(Addr) when is_tuple(Addr) andalso (tuple_size(Addr) =:= 4) ->
    inet;
which_family(Addr) when is_tuple(Addr) andalso (tuple_size(Addr) =:= 8) ->
    inet6.

domain(Mod) ->
    case Mod of
        inet_tcp  -> inet;
        inet6_tcp -> inet6;
        local_tcp -> local
    end.

check_opts(Opts0) ->
    Def = #{
            tcp_module => inet_tcp
           },
    ?DBG_LOG("Opts: ~p~n", [Opts0]),
    lists:foldr(fun check_opts_1/2, Def, Opts0).


check_opts_1({active, Val}, Opts) ->
    Opts#{active => Val};
check_opts_1(inet, Opts) ->
    Opts#{tcp_module => inet_tcp};
check_opts_1(inet6, Opts) ->
    Opts#{tcp_module => inet6_tcp};
check_opts_1(local, Opts) ->
    Opts#{tcp_module => local_tcp};
check_opts_1({raw, Level, Key, Value}, Opts) ->
    Opts#{raw => {Level, Key, Value}};
check_opts_1({exit_on_close, false}, Opts) ->
    Opts;

check_opts_1({backlog, Val}, Opts) ->
    Opts#{backlog => Val};

check_opts_1({ip, Val}, Opts) ->
    Opts#{ip => Val};
check_opts_1({ifaddr, Val}, Opts) ->
    Opts#{ifaddr => Val};

check_opts_1({Key, Val}, Opts) ->
    case maps:get(Key, socket_opts(), undefined) of
        undefined ->
            logger:log(notice, "Unsupported option ~p ~p", [Key,Val]),
            Opts;
        SocketKeyOpt ->
            Opts#{SocketKeyOpt => Val}
    end;

check_opts_1(Opt, Opts) ->
    logger:log(notice, "Unsupported option ~p ", [Opt]),
    Opts.


socket_opts() ->
    #{
      mode   => inet_option_only,
      active => inet_option_only,
      header => inet_option_only,
      packet => inet_option_only,
      packet_size => inet_option_only,

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
     }.

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
            Denom = max(maps:get(DenomTag, Counters), 1),
            [{Tag, maps:get(NumTag, Counters) div Denom}
            | getstat_what(Tags, Counters)];
        [] ->
            getstat_what(Tags, Counters)
    end.
