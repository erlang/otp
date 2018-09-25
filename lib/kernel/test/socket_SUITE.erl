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

%% Test cases
-export([
         %% Basic
         api_b_open_and_close_udp4/1,
         api_b_open_and_close_tcp4/1,
         api_b_sendto_and_recvfrom_udp4/1,
         api_b_sendmsg_and_recvmsg_udp4/1,
         api_b_send_and_recv_tcp4/1,
         api_b_sendmsg_and_recvmsg_tcp4/1

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
    [{api,            [], api_cases()},
     {api_basic,      [], api_basic_cases()}
     %% {tickets,        [], ticket_cases()}
    ].
     
api_cases() ->
    [
     {group, api_basic}
    ].

api_basic_cases() ->
    [api_b_open_and_close_udp4,
     api_b_open_and_close_tcp4,
     api_b_sendto_and_recvfrom_udp4,
     api_b_sendmsg_and_recvmsg_udp4,
     api_b_send_and_recv_tcp4,
     api_b_sendmsg_and_recvmsg_tcp4
    ].

%% ticket_cases() ->
%%     [].



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


api_b_send_and_recv_udp(Domain, Send, Recv) ->
    SockSrc = case socket:open(Domain, dgram, udp) of
                  {ok, S1} ->
                      S1;
                  {error, OR1} ->
                      ?FAIL({open, src, OR1})
              end,
    LAddr = which_local_addr(Domain),
    LSA   = #{family => Domain, addr => LAddr}, 
    case socket:bind(SockSrc, LSA) of
        {ok, _} ->
            ok;
        {error, BR1} ->
            ?FAIL({bind, src, BR1})
    end,
    SockDst = case socket:open(Domain, dgram, udp) of
                  {ok, S2} ->
                      S2;
                  {error, OR2} ->
                      ?FAIL({open, dst, OR2})
              end,
    case socket:bind(SockDst, LSA) of
        {ok, _} ->
            ok;
        {error, BR2} ->
            ?FAIL({bind, dst, BR2})
    end,
    Dst = case socket:sockname(SockDst) of
              {ok, SA} ->
                  SA;
              {ok, SNR} ->
                  ?FAIL({sockname, dst, SNR})
          end,
    ok = Send(SockSrc, ?BASIC_REQ, Dst),
    {ok, {Src, ?BASIC_REQ}} = Recv(SockDst),
    ok = Send(SockDst, ?BASIC_REP, Src),
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


api_b_send_and_recv_tcp(Domain, Send, Recv) ->
    process_flag(trap_exit, true),
    LAddr     = which_local_addr(Domain),
    LSA       = #{family => Domain, addr => LAddr}, 
    Starter   = self(),
    ServerFun = fun() ->
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
   

tc_begin(TC) ->
    put(tc_name, TC),
    p("begin").
    
tc_end() ->
    ok.

p(F) ->
    p(F, []).

p(F, A) ->
    io:format(user, "*** ~w " ++ F ++ "~n", [get(tc_name)|A]).

