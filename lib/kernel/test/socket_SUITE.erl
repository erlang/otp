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

-module(socket_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         %% API Basic
         api_b_open_and_close_udp4/1,
         api_b_open_and_close_tcp4/1,
         api_b_sendto_and_recvfrom_udp4/1,
         api_b_sendmsg_and_recvmsg_udp4/1,
         api_b_send_and_recv_tcp4/1,
         api_b_sendmsg_and_recvmsg_tcp4/1,

         %% API Options
         api_opt_simple_otp_options/1,
         api_opt_simple_otp_controlling_process/1,

         %% API Operation Timeout
         api_to_connect_tcp4/1,
         api_to_connect_tcp6/1,
         api_to_accept_tcp4/1,
         api_to_accept_tcp6/1,
         api_to_send_tcp4/1,
         api_to_send_tcp6/1,
         api_to_sendapi_to_udp4/1,
         api_to_sendapi_to_udp6/1,
         api_to_sendmsg_tcp4/1,
         api_to_sendmsg_tcp6/1,
         api_to_recv_udp4/1,
         api_to_recv_udp6/1,
         api_to_recv_tcp4/1,
         api_to_recv_tcp6/1,
         api_to_recvfrom_udp4/1,
         api_to_recvfrom_udp6/1,
         api_to_recvmsg_udp4/1,
         api_to_recvmsg_udp6/1,
         api_to_recvmsg_tcp4/1,
         api_to_recvmsg_tcp6/1

         %% Tickets
        ]).

%% Internal exports
%% -export([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(BASIC_REQ, <<"hejsan">>).
-define(BASIC_REP, <<"hoppsan">>).

-define(FAIL(R), exit(R)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [
     {group, api}
     %% {group, tickets}
    ].

groups() -> 
    [{api,                 [], api_cases()},
     {api_basic,           [], api_basic_cases()},
     {api_op_with_timeout, [], api_op_with_timeout_cases()},
     {api_options,         [], api_options_cases()}
     %% {tickets,             [], ticket_cases()}
    ].
     
api_cases() ->
    [
     {group, api_basic},
     {group, api_options},
     {group, api_op_with_timeout}
    ].

api_basic_cases() ->
    [
     api_b_open_and_close_udp4,
     api_b_open_and_close_tcp4,
     api_b_sendto_and_recvfrom_udp4,
     api_b_sendmsg_and_recvmsg_udp4,
     api_b_send_and_recv_tcp4,
     api_b_sendmsg_and_recvmsg_tcp4
    ].

api_options_cases() ->
    [
     api_opt_simple_otp_options,
     api_opt_simple_otp_controlling_process
    ].

api_op_with_timeout_cases() ->
    [
     api_to_connect_tcp4,
     api_to_connect_tcp6,
     api_to_accept_tcp4,
     api_to_accept_tcp6,
     api_to_send_tcp4,
     api_to_send_tcp6,
     api_to_sendapi_to_udp4,
     api_to_sendapi_to_udp6,
     api_to_sendmsg_tcp4,
     api_to_sendmsg_tcp6,
     api_to_recv_udp4,
     api_to_recv_udp6,
     api_to_recv_tcp4,
     api_to_recv_tcp6,
     api_to_recvfrom_udp4,
     api_to_recvfrom_udp6,
     api_to_recvmsg_udp4,
     api_to_recvmsg_udp6,
     api_to_recvmsg_tcp4,
     api_to_recvmsg_tcp6
    ].


%% ticket_cases() ->
%%     [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 UDP (dgram) socket.
%% With some extra checks...
api_b_open_and_close_udp4(suite) ->
    [];
api_b_open_and_close_udp4(doc) ->
    [];
api_b_open_and_close_udp4(_Config) when is_list(_Config) ->
    tc_begin(api_b_open_and_close_udp4),
    ok = api_b_open_and_close(inet, dgram, udp),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 TCP (stream) socket.
%% With some extra checks...
api_b_open_and_close_tcp4(suite) ->
    [];
api_b_open_and_close_tcp4(doc) ->
    [];
api_b_open_and_close_tcp4(_Config) when is_list(_Config) ->
    tc_begin(api_b_open_and_close_tcp4),
    ok = api_b_open_and_close(inet, stream, tcp),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_open_and_close(Domain, Type, Proto) ->
    Socket = case socket:open(Domain, Type, Proto) of
                 {ok, S} ->
                     S;
                 {error, Reason} ->
                     ?FAIL({open, Reason})
             end,
    {ok, Domain} = socket:getopt(Socket, socket, domain),
    {ok, Type}   = socket:getopt(Socket, socket, type),
    {ok, Proto}  = socket:getopt(Socket, socket, protocol),
    Self = self(),
    {ok, Self}   = socket:getopt(Socket, otp, controlling_process),
    ok = socket:close(Socket),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket using
%% sendto and recvfrom..
api_b_sendto_and_recvfrom_udp4(suite) ->
    [];
api_b_sendto_and_recvfrom_udp4(doc) ->
    [];
api_b_sendto_and_recvfrom_udp4(_Config) when is_list(_Config) ->
    tc_begin(api_b_sendto_and_recvfrom_udp4),
    Send = fun(Sock, Data, Dest) ->
                   socket:sendto(Sock, Data, Dest)
           end,
    Recv = fun(Sock) ->
                   socket:recvfrom(Sock)
           end,
    ok = api_b_send_and_recv_udp(inet, Send, Recv),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket
%% using sendmsg and recvmsg.
api_b_sendmsg_and_recvmsg_udp4(suite) ->
    [];
api_b_sendmsg_and_recvmsg_udp4(doc) ->
    [];
api_b_sendmsg_and_recvmsg_udp4(_Config) when is_list(_Config) ->
    tc_begin(api_b_sendmsg_and_recvmsg_udp4),
    Send = fun(Sock, Data, Dest) ->
                   %% CMsgHdr  = #{level => ip, type => tos, data => reliability},
                   %% CMsgHdrs = [CMsgHdr],
                   MsgHdr = #{addr => Dest,
                              %% ctrl => CMsgHdrs,
                              iov  => [Data]},
                   socket:sendmsg(Sock, MsgHdr)
           end,
    Recv = fun(Sock) ->
                   case socket:recvmsg(Sock) of
                       {ok, #{addr  := Source,
                              iov   := [Data]}} ->
                           {ok, {Source, Data}};
                       {error, _} = ERROR ->
                           ERROR
                   end
           end,
    ok = api_b_send_and_recv_udp(inet, Send, Recv),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_send_and_recv_udp(Domain, Send, Recv) ->
    SockSrc = sock_open(Domain, dgram, udp),
    LAddr   = which_local_addr(Domain),
    LSA     = #{family => Domain, addr => LAddr}, 
    sock_bind(SockSrc, LSA),
    SockDst = sock_open(Domain, dgram, udp),
    sock_bind(SockDst, LSA),
    Dst     = sock_sockname(SockDst),
    ok      = Send(SockSrc, ?BASIC_REQ, Dst),
    {ok, {Src, ?BASIC_REQ}} = Recv(SockDst),
    ok      = Send(SockDst, ?BASIC_REP, Src),
    {ok, {Dst, ?BASIC_REP}} = Recv(SockSrc),
    socket:close(SockSrc),
    socket:close(SockDst),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the "common" functions (send and recv)
%% on an IPv4 TCP (stream) socket.
api_b_send_and_recv_tcp4(suite) ->
    [];
api_b_send_and_recv_tcp4(doc) ->
    [];
api_b_send_and_recv_tcp4(_Config) when is_list(_Config) ->
    tc_begin(api_b_send_and_recv_tcp4),
    Send = fun(Sock, Data) ->
                   socket:send(Sock, Data)
           end,
    Recv = fun(Sock) ->
                   socket:recv(Sock)
           end,
    ok = api_b_send_and_recv_tcp(inet, Send, Recv),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the msg functions (sendmsg and recvmsg)
%% on an IPv4 TCP (stream) socket.
api_b_sendmsg_and_recvmsg_tcp4(suite) ->
    [];
api_b_sendmsg_and_recvmsg_tcp4(doc) ->
    [];
api_b_sendmsg_and_recvmsg_tcp4(_Config) when is_list(_Config) ->
    tc_begin(api_b_sendmsg_and_recvmsg_tcp4),
    Send = fun(Sock, Data) ->
                   MsgHdr = #{iov => [Data]},
                   socket:sendmsg(Sock, MsgHdr)
           end,
    Recv = fun(Sock) ->
                   case socket:recvmsg(Sock) of
                       {ok, #{addr  := undefined,
                              iov   := [Data]}} ->
                           {ok, Data};
                       {error, _} = ERROR ->
                           ERROR
                   end
           end,
    ok = api_b_send_and_recv_tcp(inet, Send, Recv),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_send_and_recv_tcp(Domain, Send, Recv) ->
    process_flag(trap_exit, true),
    LAddr     = which_local_addr(Domain),
    LSA       = #{family => Domain, addr => LAddr}, 
    Starter   = self(),
    ServerFun = fun() ->
                        put(sname, "server"),
                        %% Create the listen socket
                        ServerLSock = 
                            case socket:open(Domain, stream, tcp) of
                                {ok, S1} ->
                                    S1;
                                {error, ServerOR} ->
                                    ?FAIL({server, open, ServerOR})
                            end,
                        %% And bind it to the local address
                        SP = 
                            case socket:bind(ServerLSock, LSA) of
                                {ok, P} ->
                                    P;
                                {error, ServerBR} ->
                                    ?FAIL({server, bind, ServerBR})
                            end,
                        %% Listen for connecting clients
                        case socket:listen(ServerLSock) of
                            ok ->
                                ok;
                            {error, ServerLR} ->
                                ?FAIL({server, listen, ServerLR})
                        end,
                        %% We are ready
                        Starter ! {self(), {ok, SP}},
                        %% Accept connections
                        ServerSock = 
                            case socket:accept(ServerLSock) of
                                {ok, Sock} ->
                                    Sock;
                                {error, ServerAR} ->
                                    ?FAIL({server, accept, ServerAR})
                            end,
                        %% Wait for a message
                        case Recv(ServerSock) of
                            {ok, ?BASIC_REQ} ->
                                ok;
                            {error, ServerRR} ->
                                ?FAIL({server, recv, ServerRR})
                        end,
                        %% Send the reply
                        case Send(ServerSock, ?BASIC_REP) of
                            ok ->
                                ok;
                            {error, ServerSR} ->
                                ?FAIL({server, send, ServerSR})
                        end,
                        %% Close the sockets
                        socket:close(ServerSock),
                        socket:close(ServerLSock),
                        %% We are done
                        exit(normal)
                end,
    Server = spawn_link(ServerFun),
    ServerPort = 
        receive
            {Server, {ok, P}} ->
                P;
            {'EXIT', Server, ServerStartReason} ->
                ?FAIL({server, start, ServerStartReason})
        end,
    ClientSock = 
        case socket:open(Domain, stream, tcp) of
            {ok, S2} ->
                S2;
            {error, ClientOR} ->
                ?FAIL({client, open, ClientOR})
        end,
    case socket:bind(ClientSock, LSA) of
        {ok, _} ->
            ok;
        {error, ClientBR} ->
            ?FAIL({client, bind, ClientBR})
    end,
    case socket:connect(ClientSock, LSA#{port => ServerPort}) of
        ok ->
            ok;
        {error, ClientCR} ->
            ?FAIL({client, connect, ClientCR})
    end,
    case Send(ClientSock, ?BASIC_REQ) of
        ok ->
            ok;
        {error, ClientSR} ->
            ?FAIL({client, send, ClientSR})
    end,
    case Recv(ClientSock) of
        {ok, ?BASIC_REP} ->
            ok;
        {ok, Msg} ->
            ?FAIL({client, recv, {unexpected, Msg}})
    end,
    receive
        {'EXIT', Server, normal} ->
            ok;
        {'EXIT', Server, ServerStopReason} ->
            ?FAIL({server, stop, ServerStopReason})
    end,
    socket:close(ClientSock),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_options(suite) ->
    [];
api_opt_simple_otp_options(doc) ->
    [];
api_opt_simple_otp_options(_Config) when is_list(_Config) ->
    tc_begin(api_opt_simple_otp_options),

    p("Create sockets"),
    S1 = sock_open(inet, stream, tcp),
    S2 = sock_open(inet, dgram,  udp),

    Get = fun(S, Key) ->
                  socket:getopt(S, otp, Key)
          end,
    Set = fun(S, Key, Val) ->
                  socket:setopt(S, otp, Key, Val)
          end,

    p("Create dummy process"),
    Pid = spawn_link(fun() -> 
                             put(sname, "dummy"),
                             receive
                                 die -> 
                                     exit(normal) 
                             end 
                     end),

    F = fun(Sock) ->
                p("Test IOW"),
                {ok, IOW}    = Get(Sock, iow),
                NotIOW       = not IOW,
                ok           = Set(Sock, iow, NotIOW),
                {ok, NotIOW} = Get(Sock, iow),

                p("Test rcvbuf"),
                {ok, RcvBuf}  = Get(Sock, rcvbuf),
                RcvBuf2       = RcvBuf*2,
                ok            = Set(Sock, rcvbuf, RcvBuf2),
                {ok, RcvBuf2} = Get(Sock, rcvbuf),
                ok            = Set(Sock, rcvbuf, default),
                {ok, RcvBuf}  = Get(Sock, rcvbuf),

                p("Test rcvctrlbuf"),
                {ok, RcvCtrlBuf}  = Get(Sock, rcvctrlbuf),
                RcvCtrlBuf2       = RcvCtrlBuf*2,
                ok                = Set(Sock, rcvctrlbuf, RcvCtrlBuf2),
                {ok, RcvCtrlBuf2} = Get(Sock, rcvctrlbuf),
                ok                = Set(Sock, rcvctrlbuf, default),
                {ok, RcvCtrlBuf}  = Get(Sock, rcvctrlbuf),

                p("Test sndctrlbuf"),
                {ok, SndCtrlBuf}  = Get(Sock, sndctrlbuf),
                SndCtrlBuf2       = SndCtrlBuf*2,
                ok                = Set(Sock, sndctrlbuf, SndCtrlBuf2),
                {ok, SndCtrlBuf2} = Get(Sock, sndctrlbuf),
                ok                = Set(Sock, sndctrlbuf, default),
                {ok, RcvCtrlBuf}  = Get(Sock, sndctrlbuf),

                p("Test controlling-process"),
                Self = self(),
                {ok, Self}   = Get(Sock, controlling_process),
                ok           = Set(Sock, controlling_process, Pid),
                {ok, Pid}    = Get(Sock, controlling_process)

        end,

    p("Test stream/tcp "),
    F(S1),

    p("Test dgram/udp "),
    F(S2),

    p("kill dummy process"),
    %% This will also close its sockets (S1 and S2),
    %% This should really be tested explicitly...
    Pid ! die,

    %% p("close sockets"),
    %% sock_close(S1),
    %% sock_close(S2),

    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_controlling_process(suite) ->
    [];
api_opt_simple_otp_controlling_process(doc) ->
    [];
api_opt_simple_otp_controlling_process(_Config) when is_list(_Config) ->
    tc_begin(api_opt_simple_otp_controlling_process),

    p("Create sockets"),
    S1 = sock_open(inet, stream, tcp),
    S2 = sock_open(inet, dgram,  udp),

    Get = fun(S, Key) ->
                  socket:getopt(S, otp, Key)
          end,
    Set = fun(S, Key, Val) ->
                  socket:setopt(S, otp, Key, Val)
          end,

    AwaitStart =
        fun() ->
                p("await start command"),
                receive
                    {start, P, S} ->
                        {P, S}
                end
        end,
    AwaitContinue =
        fun(Pid) ->
                p("await continue command"),
                receive
                    {continue, Pid} -> 
                        ok
                end
        end,
    AwaitReady =
        fun(Pid) ->
                p("await ready confirmation from ~p", [Pid]),
                receive
                    {ready, Pid} -> 
                        ok
                end
        end,
    AwaitDie =
        fun(Pid) ->
                p("await die command"),
                receive
                    {die, Pid} -> 
                        ok
                end
        end,
    ClientStarter = 
        fun() ->
                put(sname, "client"),
                Self = self(),
                {Parent, Sock} = AwaitStart(),
                p("verify parent ~p controlling", [Parent]),
                {ok, Parent} = Get(Sock, controlling_process),
                p("attempt invalid control transfer (to self)"),
                {error, not_owner} = Set(Sock, controlling_process, self()),
                p("verify parent ~p (still) controlling", [Parent]),
                {ok, Parent} = Get(Sock, controlling_process),
                p("announce ready"),
                Parent ! {ready, self()},

                AwaitContinue(Parent),
                p("verify self controlling"),
                {ok, Self} = Get(Sock, controlling_process),
                p("transfer control to parent ~p", [Parent]),
                ok = Set(Sock, controlling_process, Parent),
                p("attempt invalid control transfer (to self)"),
                {error, not_owner} = Set(Sock, controlling_process, self()),
                p("verify parent ~p controlling", [Parent]),
                {ok, Parent} = Get(Sock, controlling_process),
                p("announce ready"),
                Parent ! {ready, self()},

                AwaitDie(Parent),
                p("done"),
                exit(normal)
        end,

    Tester = 
        fun(Sock, Client) ->
                p("start"),
                Self = self(),
                p("verify self controlling"),
                {ok, Self} = Get(Sock, controlling_process),
                p("announce start"),
                Client ! {start, Self, Sock},
                AwaitReady(Client),

                p("transfer control to client ~p", [Client]),
                ok = Set(Sock, controlling_process, Client),
                p("verify client ~p controlling", [Client]),
                {ok, Client} = Get(Sock, controlling_process),
                p("attempt invalid control transfer (to self)"),
                {error, not_owner} = Set(Sock, controlling_process, self()),
                p("announce continue"),
                Client ! {continue, Self},
                AwaitReady(Client),

                p("verify self controlling"),
                {ok, Self} = Get(Sock, controlling_process),
                p("announce die"),
                Client ! {die, Self},
                p("done"),
                ok
        end,

    p("Create Worker Process(s)"),
    Pid1 = spawn_link(ClientStarter),
    Pid2 = spawn_link(ClientStarter),

    p("Test stream/tcp "),
    Tester(S1, Pid1),

    p("Test dgram/udp "),
    Tester(S2, Pid2),

    p("close sockets"),
    sock_close(S1),
    sock_close(S2),

    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the connect timeout option
%% on an IPv4 TCP (stream) socket.
api_to_connect_tcp4(suite) ->
    [];
api_to_connect_tcp4(doc) ->
    [];
api_to_connect_tcp4(_Config) when is_list(_Config) ->
    tc_begin(api_to_connect_tcp4),
    ok = api_to_connect_tcp(inet),
    tc_end().
    %% not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the connect timeout option
%% on an IPv6 TCP (stream) socket.
api_to_connect_tcp6(suite) ->
    [];
api_to_connect_tcp6(doc) ->
    [];
api_to_connect_tcp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_connect_tcp6),
    %% ok = api_to_connect_tcp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_connect_tcp(Domain) ->
    process_flag(trap_exit, true),
    p("init"),
    Client     = self(),
    LocalAddr  = which_local_addr(Domain),
    LocalSA    = #{family => Domain, addr => LocalAddr}, 
    ServerName = f("~s:server", [get_tc_name()]),
    Server = spawn_link(fun() ->
                                put(sname, ServerName),
                                p("open"),
                                LSock = sock_open(Domain, stream, tcp),
                                p("bind"),
                                ServerLPort = sock_bind(LSock, LocalSA),
                                p("listen on ~w", [ServerLPort]),
                                sock_listen(LSock, 1),
                                p("inform client"),
                                Client ! {self(), ServerLPort},
                                p("await termination command"),
                                receive
                                    die ->
                                        p("terminating"),
                                        exit(normal)
                                end
                        end),
    
    p("await server port"),
    ServerLPort = 
        receive
            {Server, Port} ->
                Port
        end,
    p("open(s)"),
    CSock1       = sock_open(Domain, stream, tcp),
    CSock2       = sock_open(Domain, stream, tcp),
    CSock3       = sock_open(Domain, stream, tcp),
    p("bind(s)"),
    _ClientPort1 = sock_bind(CSock1, LocalSA),
    _ClientPort2 = sock_bind(CSock2, LocalSA),
    _ClientPort3 = sock_bind(CSock3, LocalSA),
    ServerSA = LocalSA#{port => ServerLPort},
    api_to_connect_tcp_await_timeout([CSock1, CSock2, CSock3], ServerSA),
    p("terminate server"),
    Server ! die,
    receive
        {'EXIT', Server, _} ->
            p("server terminated"),
            ok
    end,
    ok.


api_to_connect_tcp_await_timeout(Socks, ServerSA) ->
    api_to_connect_tcp_await_timeout(Socks, ServerSA, 1).

api_to_connect_tcp_await_timeout([], _ServerSA, _ID) ->
    ?FAIL(unexpected_success);
api_to_connect_tcp_await_timeout([Sock|Socks], ServerSA, ID) ->
    p("~w: try connect", [ID]),
    case socket:connect(Sock, ServerSA, 5000) of
        {error, timeout} ->
            p("expected timeout (~w)", [ID]),
            ok;
        {error, Reason} ->
            p("failed connecting: ~p", [Reason]),
            ?FAIL({recv, Reason});
        ok ->
            p("unexpected success (~w) - try next", [ID]),
            api_to_connect_tcp_await_timeout(Socks, ServerSA, ID+1)
    end.
        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv4 TCP (stream) socket.
api_to_accept_tcp4(suite) ->
    [];
api_to_accept_tcp4(doc) ->
    [];
api_to_accept_tcp4(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_accept_tcp4),
    %% ok = api_to_accept_tcp(inet),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv6 TCP (stream) socket.
api_to_accept_tcp6(suite) ->
    [];
api_to_accept_tcp6(doc) ->
    [];
api_to_accept_tcp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_accept_tcp6),
    %% ok = api_to_accept_tcp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the send timeout option
%% on an IPv4 TCP (stream) socket.
api_to_send_tcp4(suite) ->
    [];
api_to_send_tcp4(doc) ->
    [];
api_to_send_tcp4(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_send_tcp4),
    %% ok = api_to_send_tcp(inet),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the send timeout option
%% on an IPv6 TCP (stream) socket.
api_to_send_tcp6(suite) ->
    [];
api_to_send_tcp6(doc) ->
    [];
api_to_send_tcp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_send_tcp6),
    %% ok = api_to_send_tcp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendto timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_sendapi_to_udp4(suite) ->
    [];
api_to_sendapi_to_udp4(doc) ->
    [];
api_to_sendapi_to_udp4(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_sendapi_to_udp4),
    %% ok = api_to_sendapi_to_udp(inet),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendto timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_sendapi_to_udp6(suite) ->
    [];
api_to_sendapi_to_udp6(doc) ->
    [];
api_to_sendapi_to_udp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_sendapi_to_udp6),
    %% ok = api_to_sendapi_to_udp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendmsg timeout option
%% on an IPv4 TCP (stream) socket.
api_to_sendmsg_tcp4(suite) ->
    [];
api_to_sendmsg_tcp4(doc) ->
    [];
api_to_sendmsg_tcp4(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_sendmsg_tcp4),
    %% ok = api_to_sendmsg_tcp(inet),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendmsg timeout option
%% on an IPv6 TCP (stream) socket.
api_to_sendmsg_tcp6(suite) ->
    [];
api_to_sendmsg_tcp6(doc) ->
    [];
api_to_sendmsg_tcp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_sendmsg_tcp6),
    %% ok = api_to_sendmsg_tcp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv4 UDP (dgram) socket. To test this we must connect
%% the socket.
api_to_recv_udp4(suite) ->
    [];
api_to_recv_udp4(doc) ->
    [];
api_to_recv_udp4(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_recv_udp4),
    %% ok = api_to_recv_udp(inet),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv6 UDP (dgram) socket. To test this we must connect
%% the socket.
api_to_recv_udp6(suite) ->
    [];
api_to_recv_udp6(doc) ->
    [];
api_to_recv_udp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_recv_udp6),
    %% ok = api_to_recv_udp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv4 TCP (stream) socket.
api_to_recv_tcp4(suite) ->
    [];
api_to_recv_tcp4(doc) ->
    [];
api_to_recv_tcp4(_Config) when is_list(_Config) ->
    tc_begin(api_to_recv_tcp4),
    ok = api_to_recv_tcp(inet),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv6 TCP (stream) socket.
api_to_recv_tcp6(suite) ->
    [];
api_to_recv_tcp6(doc) ->
    [];
api_to_recv_tcp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_recv_tcp6),
    %% ok = api_to_recv_tcp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_recv_tcp(Domain) ->
    process_flag(trap_exit, true),
    p("server -> open"),
    LSock     = sock_open(Domain, stream, tcp),
    LocalAddr = which_local_addr(Domain),
    LocalSA   = #{family => Domain, addr => LocalAddr}, 
    p("server -> bind"),
    ServerLPort     = sock_bind(LSock, LocalSA),
    p("server(~w) -> listen", [ServerLPort]),
    sock_listen(LSock),
    ClientName = f("~s:client", [get_tc_name()]),
    Client = spawn_link(fun() ->
                                put(sname, ClientName),
                                p("open"),
                                CSock      = sock_open(Domain, stream, tcp),
                                p("bind"),
                                ClientPort = sock_bind(CSock, LocalSA),
                                p("[~w] connect to ~w", 
                                  [ClientPort, ServerLPort]),
                                sock_connect(CSock, LocalSA#{port => ServerLPort}),
                                p("await termination command"),
                                receive
                                    die ->
                                        p("terminating"),
                                        exit(normal)
                                end
                        end),
    p("server -> accept on ~w", [ServerLPort]),
    Sock      = sock_accept(LSock),
    p("server -> recv"),
    %% The zero (0) represents "give me everything you have"
    case socket:recv(Sock, 0, 5000) of
        {error, timeout} ->
            p("server -> expected timeout"),
            ok;
        {ok, _Data} ->
            ?FAIL(unexpected_success);
        {error, Reason} ->
            ?FAIL({recv, Reason})
    end,
    Client ! die,
    receive
        {'EXIT', Client, _} ->
            ok
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvfrom timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_recvfrom_udp4(suite) ->
    [];
api_to_recvfrom_udp4(doc) ->
    [];
api_to_recvfrom_udp4(_Config) when is_list(_Config) ->
    tc_begin(api_to_recvfrom_udp4),
    ok = api_to_recvfrom_udp(inet),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvfrom timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_recvfrom_udp6(suite) ->
    [];
api_to_recvfrom_udp6(doc) ->
    [];
api_to_recvfrom_udp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_recvfrom_udp6),
    %% ok = api_to_recvfrom_udp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_recvfrom_udp(Domain) ->
    process_flag(trap_exit, true),
    p("init"),
    LocalAddr = which_local_addr(Domain),
    LocalSA   = #{family => Domain, addr => LocalAddr}, 
    p("open"),
    Sock      = sock_open(Domain, dgram, udp),
    p("bind"),
    _Port     = sock_bind(Sock, LocalSA),
    p("recv"),
    case socket:recvfrom(Sock, 0, 5000) of
        {error, timeout} ->
            p("expected timeout"),
            ok;
        {ok, _SrcData} ->
            ?FAIL(unexpected_success);
        {error, Reason} ->
            ?FAIL({recv, Reason})
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_recvmsg_udp4(suite) ->
    [];
api_to_recvmsg_udp4(doc) ->
    [];
api_to_recvmsg_udp4(_Config) when is_list(_Config) ->
    %% not_yet_implemented().
    tc_begin(api_to_recvmsg_udp4),
    ok = api_to_recvmsg_udp(inet),
    tc_end().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_recvmsg_udp6(suite) ->
    [];
api_to_recvmsg_udp6(doc) ->
    [];
api_to_recvmsg_udp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_recvmsg_udp6),
    %% ok = api_to_recvmsg_udp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_recvmsg_udp(Domain) ->
    process_flag(trap_exit, true),
    p("init"),
    LocalAddr = which_local_addr(Domain),
    LocalSA   = #{family => Domain, addr => LocalAddr}, 
    p("open"),
    Sock      = sock_open(Domain, dgram, udp),
    p("bind"),
    _Port     = sock_bind(Sock, LocalSA),
    p("recv"),
    case socket:recvmsg(Sock, 5000) of
        {error, timeout} ->
            p("expected timeout"),
            ok;
        {ok, _MsgHdr} ->
            ?FAIL(unexpected_success);
        {error, Reason} ->
            ?FAIL({recv, Reason})
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv4 TCP (stream) socket.
api_to_recvmsg_tcp4(suite) ->
    [];
api_to_recvmsg_tcp4(doc) ->
    [];
api_to_recvmsg_tcp4(_Config) when is_list(_Config) ->
    tc_begin(api_to_recvmsg_tcp4),
    ok = api_to_recvmsg_tcp(inet),
    tc_end().
    %% not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv6 TCP (stream) socket.
api_to_recvmsg_tcp6(suite) ->
    [];
api_to_recvmsg_tcp6(doc) ->
    [];
api_to_recvmsg_tcp6(_Config) when is_list(_Config) ->
    %% tc_begin(api_to_recvmsg_tcp6),
    %% ok = api_to_recvmsg_tcp(inet6),
    %% tc_end().
    not_yet_implemented().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_recvmsg_tcp(Domain) ->
    process_flag(trap_exit, true),
    p("server -> open"),
    LSock     = sock_open(Domain, stream, tcp),
    LocalAddr = which_local_addr(Domain),
    LocalSA   = #{family => Domain, addr => LocalAddr}, 
    p("server -> bind"),
    ServerLPort     = sock_bind(LSock, LocalSA),
    p("server(~w) -> listen", [ServerLPort]),
    sock_listen(LSock),
    ClientName = f("~s:client", [get_tc_name()]),
    Client = spawn_link(fun() ->
                                put(sname, ClientName),
                                p("open"),
                                CSock      = sock_open(Domain, stream, tcp),
                                p("bind"),
                                ClientPort = sock_bind(CSock, LocalSA),
                                p("[~w] connect to ~w", 
                                  [ClientPort, ServerLPort]),
                                sock_connect(CSock, LocalSA#{port => ServerLPort}),
                                p("await termination command"),
                                receive
                                    die ->
                                        p("terminating"),
                                        exit(normal)
                                end
                        end),
    p("server -> accept on ~w", [ServerLPort]),
    Sock      = sock_accept(LSock),
    p("server -> recv"),
    %% The zero (0) represents "give me everything you have"
    case socket:recvmsg(Sock, 5000) of
        {error, timeout} ->
            p("server -> expected timeout"),
            ok;
        {ok, _Data} ->
            ?FAIL(unexpected_success);
        {error, Reason} ->
            ?FAIL({recv, Reason})
    end,
    Client ! die,
    receive
        {'EXIT', Client, _} ->
            ok
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This gets the local address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    case inet:getifaddrs() of
        {ok, IFL} ->
            which_addr(Domain, IFL);
        {error, Reason} ->
            ?FAIL({inet, getifaddrs, Reason})
    end.

which_addr(_Domain, []) ->
    ?FAIL(no_address);
which_addr(Domain, [{Name, IFO}|_IFL]) when (Name =/= "lo") ->
    which_addr2(Domain, IFO);
which_addr(Domain, [_|IFL]) ->
    which_addr(Domain, IFL).

which_addr2(_Domain, []) ->
    ?FAIL(no_address);
which_addr2(inet = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 4) ->
    Addr;
which_addr2(inet6 = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 8) ->
    Addr;
which_addr2(Domain, [_|IFO]) ->
    which_addr2(Domain, IFO).
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sock_open(Domain, Type, Proto) ->
    try socket:open(Domain, Type, Proto) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            ?FAIL({open, Reason})
    catch
        C:E:S ->
            ?FAIL({open, C, E, S})
    end.


sock_bind(Sock, SockAddr) ->
    try socket:bind(Sock, SockAddr) of
        {ok, Port} ->
            Port;
        {error, Reason} ->
            p("sock_bind -> error: ~p", [Reason]),
            ?FAIL({bind, Reason})
    catch
        C:E:S ->
            p("sock_bind -> failed: ~p, ~p, ~p", [C, E, S]),
            ?FAIL({bind, C, E, S})
    end.

sock_connect(Sock, SockAddr) ->
    try socket:connect(Sock, SockAddr) of
        ok ->
            ok;
        {error, Reason} ->
            ?FAIL({connect, Reason})
    catch
        C:E:S ->
            ?FAIL({connect, C, E, S})
    end.
    
sock_sockname(Sock) ->
    try socket:sockname(Sock) of
        {ok, SockAddr} ->
            SockAddr;
        {error, Reason} ->
            ?FAIL({sockname, Reason})
    catch
        C:E:S ->
            ?FAIL({sockname, C, E, S})
    end.
    

sock_listen(Sock) ->
    sock_listen2(fun() -> socket:listen(Sock) end).

sock_listen(Sock, BackLog) ->
    sock_listen2(fun() -> socket:listen(Sock, BackLog) end).

sock_listen2(Listen) ->
    try Listen() of
        ok ->
            ok;
        {error, Reason} ->
            ?FAIL({listen, Reason})
    catch
        C:E:S ->
            ?FAIL({listen, C, E, S})
    end.


sock_accept(LSock) ->
    try socket:accept(LSock) of
        {ok, Sock} ->
            Sock;
        {error, Reason} ->
            p("sock_accept -> error: ~p", [Reason]),
            ?FAIL({accept, Reason})
    catch
        C:E:S ->
            p("sock_accept -> failed: ~p, ~p, ~p", [C, E, S]),
            ?FAIL({accept, C, E, S})
    end.


sock_close(Sock) ->
    try socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            p("sock_close -> error: ~p", [Reason]),
            ?FAIL({close, Reason})
    catch
        C:E:S ->
            p("sock_close -> failed: ~p, ~p, ~p", [C, E, S]),
            ?FAIL({close, C, E, S})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    {skip, "not yet implemented"}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_tc_name(N) when is_atom(N) ->
    set_tc_name(atom_to_list(N));
set_tc_name(N) when is_list(N) ->
    put(tc_name, N).

get_tc_name() ->
    get(tc_name).

tc_begin(TC) ->
    set_tc_name(TC),
    p("begin ***").
    
tc_end() ->
    p("done ***"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

p(F) ->
    p(F, []).

p(F, A) ->
    TcName = 
        case get(tc_name) of
            undefined ->
                case get(sname) of
                    undefined ->
                        "";
                    SName when is_list(SName) ->
                        SName
                end;
            Name when is_list(Name) ->
                Name
        end,
    i("*** ~s[~p] " ++ F, [TcName,self()|A]).


%% i(F) ->
%%     i(F, []).

i(F, A) ->
    io:format(user, F ++ "~n", A).
