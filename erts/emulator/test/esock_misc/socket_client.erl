%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(socket_client).

-export([
         start/1, start/2, start/5, start/6,
         start_tcp/1, start_tcp/2, start_tcp/3,
         start_tcp4/1, start_tcp4/2, start_tcp6/1, start_tcp6/2,
         start_udp/1, start_udp/2, start_udp/3,
         start_udp4/1, start_udp4/2, start_udp6/1, start_udp6/2
        ]).

-define(LIB, socket_lib).

-record(client, {socket, verbose = true, msg = true, type, dest, msg_id = 1}).

start(Port) ->
    start(Port, 1).

start(Port, Num) ->
    start_tcp(Port, Num).

start_tcp(Port) ->
    start_tcp(Port, 1).

start_tcp(Port, Num) ->
    start_tcp4(Port, Num).

start_tcp4(Port) ->
    start_tcp4(Port, 1).

start_tcp4(Port, Num) ->
    start(inet, stream, tcp, Port, Num).

start_tcp6(Port) ->
    start_tcp6(Port, 1).

start_tcp6(Port, Num) ->
    start(inet6, stream, tcp, Port, Num).

start_tcp(Addr, Port, Num) when (size(Addr) =:= 4) andalso 
                                is_integer(Num) andalso 
                                (Num > 0) ->
    start(inet, stream, tcp, Addr, Port, Num);
start_tcp(Addr, Port, Num) when (size(Addr) =:= 8) andalso 
                                is_integer(Num) andalso 
                                (Num > 0) ->
    start(inet6, stream, tcp, Addr, Port, Num).


start_udp(Port) ->
    start_udp(Port, 1).

start_udp(Port, Num) ->
    start_udp4(Port, Num).

start_udp4(Port) ->
    start_udp4(Port, 1).

start_udp4(Port, Num) ->
    start(inet, dgram, udp, Port, Num).

start_udp6(Port) ->
    start_udp6(Port, 1).

start_udp6(Port, Num) ->
    start(inet6, dgram, udp, Port, Num).

start_udp(Addr, Port, Num) when (size(Addr) =:= 4) ->
    start(inet, dgram, udp, Addr, Port, Num);
start_udp(Addr, Port, Num) when (size(Addr) =:= 8) ->
    start(inet6, dgram, udp, Addr, Port, Num).


start(Domain, Type, Proto, Port, Num) 
  when is_integer(Port) andalso is_integer(Num) ->
    start(Domain, Type, Proto, which_addr(Domain), Port, Num);

start(Domain, Type, Proto, Addr, Port) ->
    start(Domain, Type, Proto, Addr, Port, 1).

start(Domain, Type, Proto, Addr, Port, 1 = Num) ->
    start(Domain, Type, Proto, Addr, Port, Num, true);
start(Domain, Type, Proto, Addr, Port, Num) 
  when is_integer(Num) andalso (Num > 1) ->
    start(Domain, Type, Proto, Addr, Port, Num, false).

start(Domain, Type, Proto, Addr, Port, Num, Verbose) ->
    put(sname, "starter"),
    Clients = start_clients(Num, Domain, Type, Proto, Addr, Port, Verbose),
    await_clients(Clients).

start_clients(Num, Domain, Type, Proto, Addr, Port, Verbose) ->
    start_clients(Num, 1, Domain, Type, Proto, Addr, Port, Verbose, []).

start_clients(Num, ID, Domain, Type, Proto, Addr, Port, Verbose, Acc) 
  when (Num > 0) ->
    StartClient = fun() ->
                          start_client(ID, Domain, Type, Proto, Addr, Port, Verbose)
                  end,
    {Pid, _} = spawn_monitor(StartClient),
    ?LIB:sleep(500),
    i("start client ~w", [ID]),
    start_clients(Num-1, ID+1, Domain, Type, Proto, Addr, Port, Verbose, [Pid|Acc]);
start_clients(_, _, _, _, _, _, _, _, Acc) ->
    i("all client(s) started"),
    lists:reverse(Acc).

await_clients([]) ->
    i("all clients done");
await_clients(Clients) ->
    receive
        {'DOWN', _MRef, process, Pid, _Reason} ->
            case lists:delete(Pid, Clients) of
                Clients2 when (Clients2 =/= Clients) ->
                    i("client ~p done", [Pid]),
                    await_clients(Clients2);
                _ ->
                    await_clients(Clients)
            end
    end.


start_client(ID, Domain, Type, Proto, Addr, Port, Verbose) ->
    put(sname, ?LIB:f("client[~w]", [ID])),
    SA = #{family => Domain,
           addr   => Addr,
           port   => Port},
    %% The way we use tos only works because we
    %% send so few messages (a new value for every
    %% message).
    tos_init(),
    do_start(Domain, Type, Proto, SA, Verbose).

do_start(Domain, stream = Type, Proto, SA, Verbose) ->
    try do_init(Domain, Type, Proto) of
        Sock ->
            connect(Sock, SA),
            maybe_print_start_info(Verbose, Sock, Type),
            %% Give the server some time...
            ?LIB:sleep(5000),
            %% ok = socket:close(Sock),
            send_loop(#client{socket  = Sock, 
                              type    = Type,
                              verbose = Verbose})
    catch
        throw:E ->
            e("Failed initiate: "
              "~n   Error: ~p", [E])
    end;
do_start(Domain, dgram = Type, Proto, SA, Verbose) ->
    try do_init(Domain, Type, Proto) of
        Sock ->
            maybe_print_start_info(Verbose, Sock, Type),
            %% Give the server some time...
            ?LIB:sleep(5000),
            %% ok = socket:close(Sock),
            send_loop(#client{socket  = Sock,
                              type    = Type,
                              dest    = SA,
                              verbose = Verbose})
    catch
        throw:E ->
            e("Failed initiate: "
              "~n   Error: ~p", [E])
    end.

maybe_print_start_info(true = _Verbose, Sock, stream = _Type) ->
    {ok, Name}    = socket:sockname(Sock),
    {ok, Peer}    = socket:peername(Sock),
    {ok, Domain}  = socket:getopt(Sock, socket, domain),
    {ok, Type}    = socket:getopt(Sock, socket, type),
    {ok, Proto}   = socket:getopt(Sock, socket, protocol),
    {ok, OOBI}    = socket:getopt(Sock, socket, oobinline),
    {ok, SndBuf}  = socket:getopt(Sock, socket, sndbuf),
    {ok, RcvBuf}  = socket:getopt(Sock, socket, rcvbuf),
    {ok, Linger}  = socket:getopt(Sock, socket, linger),
    {ok, MTU}     = socket:getopt(Sock, ip,     mtu),
    {ok, MTUDisc} = socket:getopt(Sock, ip,     mtu_discover),
    {ok, MALL}    = socket:getopt(Sock, ip,     multicast_all),
    {ok, MIF}     = socket:getopt(Sock, ip,     multicast_if),
    {ok, MLoop}   = socket:getopt(Sock, ip,     multicast_loop),
    {ok, MTTL}    = socket:getopt(Sock, ip,     multicast_ttl),
    {ok, RecvTOS} = socket:getopt(Sock, ip,     recvtos),
    i("connected: "
      "~n   From: ~p"
      "~n   To:   ~p"
      "~nwhen"
      "~n   (socket) Domain:         ~p"
      "~n   (socket) Type:           ~p"
      "~n   (socket) Protocol:       ~p"
      "~n   (socket) OOBInline:      ~p"
      "~n   (socket) SndBuf:         ~p"
      "~n   (socket) RcvBuf:         ~p"
      "~n   (socket) Linger:         ~p"
      "~n   (ip)     MTU:            ~p"
      "~n   (ip)     MTU Discovery:  ~p"
      "~n   (ip)     Multicast ALL:  ~p"
      "~n   (ip)     Multicast IF:   ~p"
      "~n   (ip)     Multicast Loop: ~p"
      "~n   (ip)     Multicast TTL:  ~p"
      "~n   (ip)     RecvTOS:        ~p"
      "~n   => wait some", 
      [Name, Peer,
       Domain, Type, Proto,
       OOBI, SndBuf, RcvBuf, Linger,
       MTU, MTUDisc, MALL, MIF, MLoop, MTTL,
       RecvTOS]);
maybe_print_start_info(true = _Verbose, Sock, dgram = _Type) ->
    {ok, Domain}  = socket:getopt(Sock, socket, domain),
    {ok, Type}    = socket:getopt(Sock, socket, type),
    {ok, Proto}   = socket:getopt(Sock, socket, protocol),
    {ok, OOBI}    = socket:getopt(Sock, socket, oobinline),
    {ok, SndBuf}  = socket:getopt(Sock, socket, sndbuf),
    {ok, RcvBuf}  = socket:getopt(Sock, socket, rcvbuf),
    {ok, Linger}  = socket:getopt(Sock, socket, linger),
    {ok, MALL}    = socket:getopt(Sock, ip,     multicast_all),
    {ok, MIF}     = socket:getopt(Sock, ip,     multicast_if),
    {ok, MLoop}   = socket:getopt(Sock, ip,     multicast_loop),
    {ok, MTTL}    = socket:getopt(Sock, ip,     multicast_ttl),
    {ok, RecvTOS} = socket:getopt(Sock, ip,     recvtos),
    {ok, RecvTTL} = socket:getopt(Sock, ip,     recvttl),
    i("initiated when: "
      "~n   (socket) Domain:         ~p"
      "~n   (socket) Type:           ~p"
      "~n   (socket) Protocol:       ~p"
      "~n   (socket) OOBInline:      ~p"
      "~n   (socket) SndBuf:         ~p"
      "~n   (socket) RcvBuf:         ~p"
      "~n   (socket) Linger:         ~p"
      "~n   (ip)     Multicast ALL:  ~p"
      "~n   (ip)     Multicast IF:   ~p"
      "~n   (ip)     Multicast Loop: ~p"
      "~n   (ip)     Multicast TTL:  ~p"
      "~n   (ip)     RecvTOS:        ~p"
      "~n   (ip)     RecvTTL:        ~p"
      "~n   => wait some",
      [Domain, Type, Proto,
       OOBI, SndBuf, RcvBuf, Linger,
       MALL, MIF, MLoop, MTTL,
       RecvTOS, RecvTTL]);
maybe_print_start_info(_Verbose, _Sock, _Type) ->
    ok.

    
do_init(Domain, stream = Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    i("try (socket) bind"),
    case socket:bind(Sock, any) of
        {ok, _P} ->
            ok = socket:setopt(Sock, socket, timestamp, true),
            ok = socket:setopt(Sock, ip,     tos,       mincost),
            ok = socket:setopt(Sock, ip,     recvtos,   true),
            Sock;
        {error, BReason} ->
            throw({bind, BReason})
    end;
do_init(Domain, dgram = Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    case socket:bind(Sock, any) of
        {ok, _} ->
            ok = socket:setopt(Sock, socket, timestamp, true),
            ok = socket:setopt(Sock, ip,     tos,       mincost),
            ok = socket:setopt(Sock, ip,     recvtos,   true),
            ok = socket:setopt(Sock, ip,     recvttl,   true),
            Sock;
        {error, BReason} ->
            throw({bind, BReason})
    end.


which_addr(Domain) ->
    Iflist = case inet:getifaddrs() of
                 {ok, IFL} ->
                     IFL;
                 {error, Reason} ->
                     throw({inet,getifaddrs,Reason})
             end,
    which_addr(Domain, Iflist).


connect(Sock, SA) ->
    i("try (socket) connect to:"
      "~n   ~p", [SA]),
    case socket:connect(Sock, SA) of
        ok ->
            ok;
        {error, Reason} ->
            e("connect failure: "
              "~n   ~p", [Reason]),
            exit({connect, Reason})
    end.


send_loop(#client{msg_id = N} = C) when (N =< 10) ->
    i("try send request ~w", [N]),
    Req = ?LIB:enc_req_msg(N, "hejsan"),
    case send(C, Req) of
        ok ->
            i("request ~w sent - now try read answer", [N]),
            case recv(C) of
                {ok, {Source, Msg}} ->
                    if
                        (C#client.verbose =:= true) ->
                            i("received ~w bytes of data~s", 
                              [size(Msg),  case Source of
                                               undefined -> "";
                                               _ -> ?LIB:f(" from:~n   ~p", [Source])
                                           end]);
                        true ->
                            i("received ~w bytes", [size(Msg)])
                    end,
                    case ?LIB:dec_msg(Msg) of
                        {reply, N, Reply} ->
                            if
                                (C#client.verbose =:= true) ->
                                    i("received reply ~w: ~p", [N, Reply]);
                                true ->
                                    i("received reply ~w", [N])
                            end,
                            ?LIB:sleep(500), % Just to spread it out a bit
                            send_loop(C#client{msg_id = N+1})
                    end;
                {error, RReason} ->
                    e("Failed recv response for request ~w: "
                      "~n   ~p", [N, RReason]),
                    exit({failed_recv, RReason})
            end;
        {error, SReason} ->
            e("Failed send request ~w: "
              "~n   ~p", [N, SReason]),
            exit({failed_send, SReason})
    end;
send_loop(Client) ->
    sock_close(Client).

sock_close(#client{socket = Sock, verbose = true}) ->
    i("we are done - close the socket when: "
      "~n   ~p", [socket:info()]),
    ok = socket:close(Sock),
    i("we are done - socket closed when: "
      "~n   ~p", [socket:info()]);
sock_close(#client{socket = Sock}) ->
    i("we are done"),
    ok = socket:close(Sock).

    

send(#client{socket = Sock, type = stream}, Msg) ->
    socket:send(Sock, Msg);
send(#client{socket = Sock, type = dgram, dest = Dest}, Msg) ->
    %% i("try send to: "
    %%   "~n   ~p", [Dest]),
    %% ok = socket:setopt(Sock, otp, debug, true),
    TOS = tos_next(),
    ok = socket:setopt(Sock, ip, tos, TOS),
    case socket:sendto(Sock, Msg, Dest) of
        ok = OK ->
            OK;
        {error, _} = ERROR ->
            ERROR
    end.

recv(#client{socket = Sock, type = stream, msg = false}) ->
    case socket:recv(Sock) of
        {ok, Msg} ->
            {ok, {undefined, Msg}};
        {error, _} = ERROR ->
            ERROR
    end;
recv(#client{socket = Sock, verbose = Verbose, type = stream, msg = true}) ->
    case socket:recvmsg(Sock) of
        %% An iov of length 1 is an simplification...
        {ok, #{addr  := undefined = Source,
               iov   := [Msg],
               ctrl  := CMsgHdrs,
               flags := Flags}} ->
            if
                (Verbose =:= true) ->
                    i("received message: "
                      "~n   CMsgHdr: ~p"
                      "~n   Flags:   ~p", [CMsgHdrs, Flags]);
                true ->
                    ok
                        end,
            {ok, {Source, Msg}};
        {error, _} = ERROR ->
            ERROR
    end;
recv(#client{socket = Sock, type = dgram, msg = false}) ->
    socket:recvfrom(Sock);
recv(#client{socket = Sock, verbose = Verbose, type = dgram, msg = true}) ->
    case socket:recvmsg(Sock) of
        {ok, #{addr  := Source,
               iov   := [Msg],
               ctrl  := CMsgHdrs,
               flags := Flags}} ->
            if
                (Verbose =:= true) ->
                    i("received message: "
                      "~n   CMsgHdr: ~p"
                      "~n   Flags:   ~p", [CMsgHdrs, Flags]);
                true ->
                    ok
            end,
            {ok, {Source, Msg}};
        {error, _} = ERROR ->
            ERROR
    end.
        


which_addr(_Domain, []) ->
    throw(no_address);
which_addr(Domain, [{Name, IFO}|_IFL]) when (Name =/= "lo") ->
    which_addr2(Domain, IFO);
which_addr(Domain, [_|IFL]) ->
    which_addr(Domain, IFL).

which_addr2(inet = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 4) ->
    Addr;
which_addr2(inet6 = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 8) ->
    Addr;
which_addr2(Domain, [_|IFO]) ->
    which_addr2(Domain, IFO).


%% ---

%% enc_req_msg(N, Data) ->
%%     enc_msg(?REQ, N, Data).

%% enc_rep_msg(N, Data) ->
%%     enc_msg(?REP, N, Data).

%% enc_msg(Type, N, Data) when is_list(Data) ->
%%     enc_msg(Type, N, list_to_binary(Data));
%% enc_msg(Type, N, Data) 
%%   when is_integer(Type) andalso is_integer(N) andalso is_binary(Data) ->
%%     <<Type:32/integer, N:32/integer, Data/binary>>.
    
%% dec_msg(<<?REQ:32/integer, N:32/integer, Data/binary>>) ->
%%     {request, N, Data};
%% dec_msg(<<?REP:32/integer, N:32/integer, Data/binary>>) ->
%%     {reply, N, Data}.


%% ---

%% sleep(T) ->
%%     receive after T -> ok end.

                     
%% ---

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(Now) ->
%%     N2T = fun(N) -> calendar:now_to_local_time(N) end,
%%     format_timestamp(Now, N2T, true).

%% format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
%%     FormatExtra = ".~.2.0w",
%%     ArgsExtra   = [N3 div 10000],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra);
%% format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
%%     FormatExtra = "",
%%     ArgsExtra   = [],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra).

%% format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
%%     {Date, Time}   = N2T(N),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate =
%%         io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
%%                       [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
%%     lists:flatten(FormatDate).


%% ---

tos_init() ->
    put(tos, 1).

tos_next() ->
    case get(tos) of
        TOS when (TOS < 100) ->
            put(tos, TOS + 1),
            TOS;
        _ ->
            put(tos, 1),
            1
    end.


%% ---

e(F, A) ->
    ?LIB:e(F, A).

i(F) ->
    ?LIB:i(F).

i(F, A) ->
    ?LIB:i(F, A).
    
