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
         api_to_maccept_tcp4/1,
         api_to_maccept_tcp6/1,
         api_to_send_tcp4/1,
         api_to_send_tcp6/1,
         api_to_sendto_udp4/1,
         api_to_sendto_udp6/1,
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
         api_to_recvmsg_tcp6/1,

         %% Socket Closure
         sc_cpe_socket_cleanup_tcp4/1,
         sc_cpe_socket_cleanup_tcp6/1,
         sc_cpe_socket_cleanup_udp4/1,
         sc_cpe_socket_cleanup_udp6/1,
         sc_recv_response_local_close_tcp4/1,
         sc_recv_response_local_close_tcp6/1,
         sc_recv_response_remote_close_tcp4/1,
         sc_recv_response_remote_close_tcp6/1,
         sc_recvmsg_response_local_close_tcp4/1,
         sc_recvmsg_response_local_close_tcp6/1,
         sc_recvmsg_response_remote_close_tcp4/1,
         sc_recvmsg_response_remote_close_tcp6/1,
         sc_acceptor_response_local_close_tcp4/1,
         sc_acceptor_response_local_close_tcp6/1,
         sc_acceptor_response_remote_close_tcp4/1,
         sc_acceptor_response_remote_close_tcp6/1

         %% Tickets
        ]).

%% Internal exports
%% -export([]).


-type initial_evaluator_state() :: map().
-type evaluator_state() :: term().
-type command_fun() :: 
        fun((State :: evaluator_state()) -> ok) |
        fun((State :: evaluator_state()) -> {ok, evaluator_state()}) |
        fun((State :: evaluator_state()) -> {error, term()}).

-type command() :: #{desc  := string(),
                     cmd   := command_fun()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(BASIC_REQ, <<"hejsan">>).
-define(BASIC_REP, <<"hoppsan">>).

-define(FAIL(R), exit(R)).

-define(SLEEP(T), receive after T -> ok end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [
     {group, api},
     {group, socket_closure}
     %% {group, tickets}
    ].

groups() -> 
    [{api,                 [], api_cases()},
     {api_basic,           [], api_basic_cases()},
     {api_options,         [], api_options_cases()},
     {api_op_with_timeout, [], api_op_with_timeout_cases()},
     {socket_closure,      [], socket_closure_cases()},
     {sc_ctrl_proc_exit,   [], sc_cp_exit_cases()}
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
     api_to_sendto_udp4,
     api_to_sendto_udp6,
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

%% These cases tests what happens when the socket is closed, locally or
%% remotely.
socket_closure_cases() ->
    [
     {group, sc_ctrl_proc_exit},

     sc_recv_response_local_close_tcp4,
     sc_recv_response_local_close_tcp6,
     sc_recv_response_remote_close_tcp4,
     sc_recv_response_remote_close_tcp6,

     sc_recvmsg_response_local_close_tcp4,
     sc_recvmsg_response_local_close_tcp6,
     sc_recvmsg_response_remote_close_tcp4,
     sc_recvmsg_response_remote_close_tcp6,

     sc_acceptor_response_local_close_tcp4,
     sc_acceptor_response_local_close_tcp6,
     sc_acceptor_response_remote_close_tcp4,
     sc_acceptor_response_remote_close_tcp6
    ].

%% These cases are all about socket cleanup after the controlling process
%% exits *without* calling socket:close/1.
sc_cp_exit_cases() ->
    [
     sc_cpe_socket_cleanup_tcp4,
     sc_cpe_socket_cleanup_tcp6,
     sc_cpe_socket_cleanup_udp4,
     sc_cpe_socket_cleanup_udp6
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                           API BASIC                                 %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 UDP (dgram) socket.
%% With some extra checks...
api_b_open_and_close_udp4(suite) ->
    [];
api_b_open_and_close_udp4(doc) ->
    [];
api_b_open_and_close_udp4(_Config) when is_list(_Config) ->
    tc_try(api_b_open_and_close_udp4,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp},
                   ok = api_b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 TCP (stream) socket.
%% With some extra checks...
api_b_open_and_close_tcp4(suite) ->
    [];
api_b_open_and_close_tcp4(doc) ->
    [];
api_b_open_and_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_b_open_and_close_tcp4,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = api_b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_open_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = S) -> 
                           Res = socket:open(Domain, Type, Protocol), 
                           {ok, {S, Res}} 
                   end},
         #{desc => "validate open",
           cmd  => fun({S, {ok, Sock}}) -> 
                           NewS = S#{socket => Sock},
                           {ok, NewS};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get domain (maybe)",
           cmd  => fun(#{socket := Sock} = S) ->
                           Res = socket:getopt(Sock, socket, domain),
                           {ok, {S, Res}}
                   end},
         #{desc => "validate domain (maybe)",
           cmd  => fun({#{domain := Domain} = S, {ok, Domain}}) -> 
                           {ok, S};
                      ({#{domain := ExpDomain}, {ok, Domain}}) ->
                           {error, {unexpected_domain, ExpDomain, Domain}};
                      %% Some platforms do not support this option
                      ({S, {error, einval}}) ->
                           {ok, S};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get type",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, socket, type), 
                           {ok, {State, Res}}
                   end},
         #{desc => "validate type",
           cmd  => fun({#{type := Type} = State, {ok, Type}}) ->
                           {ok, State};
                      ({#{type := ExpType}, {ok, Type}}) ->
                           {error, {unexpected_type, ExpType, Type}};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get protocol",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, socket, protocol),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate protocol",
           cmd  => fun({#{protocol := Protocol} = State, {ok, Protocol}}) ->
                           {ok, State};
                      ({#{protocol := ExpProtocol}, {ok, Protocol}}) ->
                           {error, {unexpected_type, ExpProtocol, Protocol}};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "get controlling-process",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, otp, controlling_process),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate controlling-process",
           cmd  => fun({State, {ok, Pid}}) ->
                           case self() of
                               Pid ->
                                   {ok, State};
                               _ ->
                                   {error, {unexpected_owner, Pid}}
                           end;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},
         #{desc => "close socket",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:close(Sock),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate socket close",
           cmd  => fun({_, ok}) ->
                           {ok, normal};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end}],
    Evaluator = evaluator_start("tester", Seq, InitState),
    ok = await_evaluator_finish([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket using
%% sendto and recvfrom..
api_b_sendto_and_recvfrom_udp4(suite) ->
    [];
api_b_sendto_and_recvfrom_udp4(doc) ->
    [];
api_b_sendto_and_recvfrom_udp4(_Config) when is_list(_Config) ->
    tc_try(api_b_sendto_and_recvfrom_udp4,
           fun() ->
                   Send = fun(Sock, Data, Dest) ->
                                  socket:sendto(Sock, Data, Dest)
                          end,
                   Recv = fun(Sock) ->
                                  socket:recvfrom(Sock)
                          end,
                   InitState = #{domain => inet,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive on an IPv4 UDP (dgram) socket
%% using sendmsg and recvmsg.
api_b_sendmsg_and_recvmsg_udp4(suite) ->
    [];
api_b_sendmsg_and_recvmsg_udp4(doc) ->
    [];
api_b_sendmsg_and_recvmsg_udp4(_Config) when is_list(_Config) ->
    tc_try(api_b_sendmsg_and_recvmsg_udp4,
           fun() ->
                   Send = fun(Sock, Data, Dest) ->
                                  %% CMsgHdr  = #{level => ip,
                                  %%              type  => tos,
                                  %%              data  => reliability},
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
                   InitState = #{domain => inet,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_send_and_recv_udp(InitState) ->
    Seq = 
        [
         #{desc => "local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "open src socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           Sock = sock_open(Domain, dgram, udp),
                           SASrc = sock_sockname(Sock),
                           {ok, State#{sock_src => Sock, sa_src => SASrc}}
                   end},
         #{desc => "bind src",
           cmd  => fun(#{sock_src := Sock, lsa := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "sockname src socket",
           cmd  => fun(#{sock_src := Sock} = State) ->
                           SASrc = sock_sockname(Sock),
                           %% ei("src sockaddr: ~p", [SASrc]),
                           {ok, State#{sa_src => SASrc}}
                   end},
         #{desc => "open dst socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           Sock = sock_open(Domain, dgram, udp),
                           {ok, State#{sock_dst => Sock}}
                   end},
         #{desc => "bind dst",
           cmd  => fun(#{sock_dst := Sock, lsa := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "sockname dst socket",
           cmd  => fun(#{sock_dst := Sock} = State) ->
                           SADst = sock_sockname(Sock),
                           %% ei("dst sockaddr: ~p", [SADst]),
                           {ok, State#{sa_dst => SADst}}
                   end},
         #{desc => "send req (to dst)",
           cmd  => fun(#{sock_src := Sock, sa_dst := Dst, send := Send}) ->
                           ok = Send(Sock, ?BASIC_REQ, Dst)
                   end},
         #{desc => "recv req (from src)",
           cmd  => fun(#{sock_dst := Sock, sa_src := Src, recv := Recv}) ->
                           {ok, {Src, ?BASIC_REQ}} = Recv(Sock),
                           ok
                   end},
         #{desc => "send rep (to src)",
           cmd  => fun(#{sock_dst := Sock, sa_src := Src, send := Send}) ->
                           ok = Send(Sock, ?BASIC_REP, Src)
                   end},
         #{desc => "recv rep (from dst)",
           cmd  => fun(#{sock_src := Sock, sa_dst := Dst, recv := Recv}) ->
                           {ok, {Dst, ?BASIC_REP}} = Recv(Sock),
                           ok
                   end},
         #{desc => "close src socket",
           cmd  => fun(#{sock_src := Sock}) ->
                           ok = socket:close(Sock)
                   end},
         #{desc => "close dst socket",
           cmd  => fun(#{sock_dst := Sock}) ->
                           ok = socket:close(Sock),
                           {ok, normal}
                   end}
        ],
    Evaluator = evaluator_start("tester", Seq, InitState),
    ok = await_evaluator_finish([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the "common" functions (send and recv)
%% on an IPv4 TCP (stream) socket.
api_b_send_and_recv_tcp4(suite) ->
    [];
api_b_send_and_recv_tcp4(doc) ->
    [];
api_b_send_and_recv_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_b_send_and_recv_tcp4,
           fun() ->
                   Send = fun(Sock, Data) ->
                                  socket:send(Sock, Data)
                          end,
                   Recv = fun(Sock) ->
                                  socket:recv(Sock)
                          end,
                   InitState = #{domain => inet,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically send and receive using the msg functions (sendmsg and recvmsg)
%% on an IPv4 TCP (stream) socket.
api_b_sendmsg_and_recvmsg_tcp4(suite) ->
    [];
api_b_sendmsg_and_recvmsg_tcp4(doc) ->
    [];
api_b_sendmsg_and_recvmsg_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_b_sendmsg_and_recvmsg_tcp4,
           fun() ->
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
                   InitState = #{domain => inet,
                                 send   => Send,
                                 recv   => Recv},
                   ok = api_b_send_and_recv_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_b_send_and_recv_tcp(InitState) ->
    process_flag(trap_exit, true),
    ServerSeq = 
        [
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce server port",
           cmd  => fun(#{parent := Parent, lport := Port}) ->
                           ei("announcing port to parent (~p)", [Parent]),
                           Parent ! {server_port, self(), Port},
                           ok
                   end},
         #{desc => "await connection",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ei("accepted: ~n   ~p", [Sock]),
                                   {ok, State#{tsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await request",
           cmd  => fun(#{tsock := Sock, recv := Recv}) ->
                           case Recv(Sock) of
                               {ok, ?BASIC_REQ} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "send reply",
           cmd  => fun(#{tsock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REP)
                   end},
         #{desc => "sleep some",
           cmd  => fun(_) ->
                           ?SLEEP(1000),
                           ok
                   end},
         #{desc => "close traffic socket",
           cmd  => fun(#{tsock := Sock}) ->
                           socket:close(Sock)
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := Sock}) ->
                           socket:close(Sock)
                   end},
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    ClientSeq = 
        [
         #{desc => "which server (local) address",
           cmd  => fun(#{domain := Domain, server_port := Port} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, 
                                     addr   => LAddr},
                           SSA   = LSA#{port => Port},
                           {ok, State#{lsa => LSA, ssa => SSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{port => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, ssa := SSA}) ->
                           socket:connect(Sock, SSA)
                   end},
         #{desc => "send request (to server)",
           cmd  => fun(#{sock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REQ)
                   end},
         #{desc => "recv reply (from server)",
           cmd  => fun(#{sock := Sock, recv := Recv}) ->
                           {ok, ?BASIC_REP} = Recv(Sock),
                           ok
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:close(Sock)
                   end},
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("start server evaluator"),
    Server = evaluator_start("server", ServerSeq, InitState),
    p("await server (~p) port", [Server]),
    SPort = receive
                {server_port, Server, Port} ->
                    Port
            end,
    p("start client evaluator"),
    Client = evaluator_start("client", ClientSeq, InitState#{server_port => SPort}),
    p("await evaluator(s)"),
    ok = await_evaluator_finish([Server, Client]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                           API OPTIONS                               %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_options(suite) ->
    [];
api_opt_simple_otp_options(doc) ->
    [];
api_opt_simple_otp_options(_Config) when is_list(_Config) ->
    tc_try(api_opt_simple_otp_options,
           fun() -> api_opt_simple_otp_options() end).

api_opt_simple_otp_options() ->
    Get = fun(S, Key) ->
                  socket:getopt(S, otp, Key)
          end,
    Set = fun(S, Key, Val) ->
                  socket:setopt(S, otp, Key, Val)
          end,

    Seq = 
        [
         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type,
                         protocol := Protocol} = State) ->
                           Sock = sock_open(Domain, Type, Protocol),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "create dummy process",
           cmd  => fun(State) ->
                           Pid =  spawn_link(fun() -> 
                                                     put(sname, "dummy"),
                                                     receive
                                                         die -> 
                                                             exit(normal) 
                                                     end 
                                             end),
                           {ok, State#{dummy => Pid}}
                   end},

         %% *** Check iow part ***
         #{desc => "get iow",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, iow) of
                               {ok, IOW} when is_boolean(IOW) ->
                                   {ok, State#{iow => IOW}};
                               {ok, InvalidIOW} ->
                                   {error, {invalid, InvalidIOW}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) iow",
           cmd  => fun(#{sock := Sock, iow := OldIOW} = State) ->
                           NewIOW = not OldIOW,
                           case Set(Sock, iow, NewIOW) of
                               ok ->
                                   {ok, State#{iow => NewIOW}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) iow",
           cmd  => fun(#{sock := Sock, iow := IOW}) ->
                           case Get(Sock, iow) of
                               {ok, IOW} ->
                                   ok;
                               {ok, InvalidIOW} ->
                                   {error, {invalid, InvalidIOW}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Check rcvbuf part ***
         #{desc => "get rcvbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, rcvbuf) of
                               {ok, RcvBuf} when is_integer(RcvBuf) ->
                                   {ok, State#{rcvbuf => RcvBuf}};
                               {ok, InvalidRcvBuf} ->
                                   {error, {invalid, InvalidRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := OldRcvBuf} = State) ->
                           NewRcvBuf = 2 * OldRcvBuf,
                           case Set(Sock, rcvbuf, NewRcvBuf) of
                               ok ->
                                   {ok, State#{rcvbuf => NewRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) rcvbuf",
           cmd  => fun(#{sock := Sock, rcvbuf := RcvBuf}) ->
                           case Get(Sock, rcvbuf) of
                               {ok, RcvBuf} ->
                                   ok;
                               {ok, InvalidRcvBuf} ->
                                   {error, {invalid, InvalidRcvBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Check rcvctrlbuf part ***
         #{desc => "get rcvctrlbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} when is_integer(RcvCtrlBuf) ->
                                   {ok, State#{rcvctrlbuf => RcvCtrlBuf}};
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := OldRcvCtrlBuf} = State) ->
                           NewRcvCtrlBuf = 2 * OldRcvCtrlBuf,
                           case Set(Sock, rcvctrlbuf, NewRcvCtrlBuf) of
                               ok ->
                                   {ok, State#{rcvctrlbuf => NewRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := RcvCtrlBuf}) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} ->
                                   ok;
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         %% *** Check rcvctrlbuf part ***
         #{desc => "get rcvctrlbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} when is_integer(RcvCtrlBuf) ->
                                   {ok, State#{rcvctrlbuf => RcvCtrlBuf}};
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := OldRcvCtrlBuf} = State) ->
                           NewRcvCtrlBuf = 2 * OldRcvCtrlBuf,
                           case Set(Sock, rcvctrlbuf, NewRcvCtrlBuf) of
                               ok ->
                                   {ok, State#{rcvctrlbuf => NewRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) rcvctrlbuf",
           cmd  => fun(#{sock := Sock, rcvctrlbuf := RcvCtrlBuf}) ->
                           case Get(Sock, rcvctrlbuf) of
                               {ok, RcvCtrlBuf} ->
                                   ok;
                               {ok, InvalidRcvCtrlBuf} ->
                                   {error, {invalid, InvalidRcvCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** Check sndctrlbuf part ***
         #{desc => "get sndctrlbuf",
           cmd  => fun(#{sock := Sock} = State) ->
                           case Get(Sock, sndctrlbuf) of
                               {ok, SndCtrlBuf} when is_integer(SndCtrlBuf) ->
                                   {ok, State#{sndctrlbuf => SndCtrlBuf}};
                               {ok, InvalidSndCtrlBuf} ->
                                   {error, {invalid, InvalidSndCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set (new) sndctrlbuf",
           cmd  => fun(#{sock := Sock, sndctrlbuf := OldSndCtrlBuf} = State) ->
                           NewSndCtrlBuf = 2 * OldSndCtrlBuf,
                           case Set(Sock, sndctrlbuf, NewSndCtrlBuf) of
                               ok ->
                                   {ok, State#{sndctrlbuf => NewSndCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "get (new) sndctrlbuf",
           cmd  => fun(#{sock := Sock, sndctrlbuf := SndCtrlBuf}) ->
                           case Get(Sock, sndctrlbuf) of
                               {ok, SndCtrlBuf} ->
                                   ok;
                               {ok, InvalidSndCtrlBuf} ->
                                   {error, {invalid, InvalidSndCtrlBuf}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Check controlling-process part ***
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock}) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "set dummy as controlling-process",
           cmd  => fun(#{sock := Sock, dummy := Dummy}) ->
                           Set(Sock, controlling_process, Dummy)
                   end},
         #{desc => "verify dummy as controlling-process",
           cmd  => fun(#{sock := Sock, dummy := Dummy}) ->
                           case Get(Sock, controlling_process) of
                               {ok, Dummy} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("Run test for stream/tcp socket"),
    InitState1 = #{domain => inet, type => stream, protocol => tcp},
    Tester1 = evaluator_start("tcp-tester", Seq, InitState1),
    p("await evaluator 1"),
    ok = await_evaluator_finish([Tester1]),

    p("Run test for dgram/udp socket"),
    InitState2 = #{domain => inet, type => dgram, protocol => udp},
    Tester2 = evaluator_start("udp-tester", Seq, InitState2),
    p("await evaluator 2"),
    ok = await_evaluator_finish([Tester2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_controlling_process(suite) ->
    [];
api_opt_simple_otp_controlling_process(doc) ->
    [];
api_opt_simple_otp_controlling_process(_Config) when is_list(_Config) ->
    tc_try(api_opt_simple_otp_controlling_process,
           fun() -> api_opt_simple_otp_controlling_process() end).

api_opt_simple_otp_controlling_process() ->
    Get = fun(S, Key) ->
                  socket:getopt(S, otp, Key)
          end,
    Set = fun(S, Key, Val) ->
                  socket:setopt(S, otp, Key, Val)
          end,

    ClientSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester, Socket} ->
                                   {ok, State#{tester => Tester,
                                               sock   => Socket}}
                           end
                   end},
         #{desc => "verify tester as controlling-process",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           case Get(Sock, controlling_process) of
                               {ok, Tester} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt invalid controlling-process transfer (to self)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case Set(Sock, controlling_process, self()) of
                               {error, not_owner} ->
                                   ok;
                               ok ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock} = _State) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt controlling-process transfer to tester",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           Set(Sock, controlling_process, Tester)
                   end},
         #{desc => "attempt invalid controlling-process transfer (to self)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case Set(Sock, controlling_process, self()) of
                               {error, not_owner} ->
                                   ok;
                               ok ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await termination",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {terminate, Tester} ->
                                   State1 = maps:remove(tester, State),
                                   State2 = maps:remove(sock, State1),
                                   {ok, State2}
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type,
                         protocol := Protocol} = State) ->
                           Sock = sock_open(Domain, Type, Protocol),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock} = _State) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (client) start",
           cmd  => fun(#{client := Client, sock := Sock} = _State) ->
                           Client ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await (client) ready (1)",
           cmd  => fun(#{client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok
                           end
                   end},
         #{desc => "attempt controlling-process transfer to client",
           cmd  => fun(#{client := Client, sock := Sock} = _State) ->
                           Set(Sock, controlling_process, Client)
                   end},
         #{desc => "verify client as controlling-process",
           cmd  => fun(#{client := Client, sock := Sock} = _State) ->
                           case Get(Sock, controlling_process) of
                               {ok, Client} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt invalid controlling-process transfer (to self)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case Set(Sock, controlling_process, self()) of
                               {error, not_owner} ->
                                   ok;
                               ok ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (client) continue",
           cmd  => fun(#{client := Client} = _State) ->
                           Client ! {continue, self()},
                           ok
                   end},
         #{desc => "await (client) ready (2)",
           cmd  => fun(#{client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok
                           end
                   end},
         #{desc => "verify self as controlling-process",
           cmd  => fun(#{sock := Sock} = _State) ->
                           Self = self(),
                           case Get(Sock, controlling_process) of
                               {ok, Self} ->
                                   ok;
                               {ok, InvalidPid} ->
                                   {error, {invalid, InvalidPid}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = State) ->
                           MRef = erlang:monitor(process, Client),
                           {ok, State#{client_mref => MRef}}
                   end},
         #{desc => "order (client) terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           Client ! {terminate, self()},
                           ok
                   end},
         #{desc => "await (client) down",
           cmd  => fun(#{client := Client} = State) ->
                           receive
                               {'DOWN', _, process, Client, _} ->
                                   {ok, maps:remove(client, State)}
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("Run test for stream/tcp socket"),
    ClientInitState1 = #{},
    Client1          = evaluator_start("tcp-client", ClientSeq, ClientInitState1),
    TesterInitState1 = #{domain   => inet, 
                         type     => stream, 
                         protocol => tcp,
                         client   => Client1},
    Tester1          = evaluator_start("tcp-tester", TesterSeq, TesterInitState1),
    p("await stream/tcp evaluator"),
    ok = await_evaluator_finish([Tester1, Client1]),

    p("Run test for dgram/udp socket"),
    ClientInitState2 = #{},
    Client2          = evaluator_start("udp-client", ClientSeq, ClientInitState2),
    TesterInitState2 = #{domain   => inet, 
                         type     => dgram, 
                         protocol => udp,
                         client   => Client2},
    Tester2          = evaluator_start("udp-tester", TesterSeq, TesterInitState2),
    p("await dgram/udp evaluator"),
    ok = await_evaluator_finish([Tester2, Client2]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                  API OPERATIONS WITH TIMEOUT                        %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the connect timeout option
%% on an IPv4 TCP (stream) socket.
api_to_connect_tcp4(suite) ->
    [];
api_to_connect_tcp4(doc) ->
    [];
api_to_connect_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_connect_tcp4,
           fun() ->
                   InitState = #{domain => inet, timeout => 5000},
                   ok = api_to_connect_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the connect timeout option
%% on an IPv6 TCP (stream) socket.
api_to_connect_tcp6(suite) ->
    [];
api_to_connect_tcp6(doc) ->
    [];
api_to_connect_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_connect_tcp6,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain => inet6, timeout => 5000},
                   ok = api_to_connect_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We use the backlog (listen) argument to test this.
%% Note that the behaviour of the TCP "server side" can vary when 
%% a client connect to a "busy" server (full backlog).
%% For instance, on FreeBSD (11.2) the reponse when the backlog is full
%% is a econreset.

api_to_connect_tcp(InitState) ->
    process_flag(trap_exit, true),

    ServerSeq = 
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} when is_pid(Tester) ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket (with backlog = 1)",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock, 1)
                   end},
         #{desc => "monitor server",
           cmd  => fun(#{tester := Tester} = State) ->
                           MRef = erlang:monitor(process, Tester),
                           {ok, State#{tester_mref => MRef}}
                   end},
         #{desc => "announce ready",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           ei("announcing ready to tester (~p)", [Tester]),
                           Tester ! {ready, self(), Port},
                           ok
                   end},
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create socket 1",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock1 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 2",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock2 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 3",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock3 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket 1 to local address",
           cmd  => fun(#{sock1 := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket 2 to local address",
           cmd  => fun(#{sock2 := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket 3 to local address",
           cmd  => fun(#{sock3 := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Synchronize with the server ***
         #{desc => "order (server) start",
           cmd  => fun(#{server := Server}) ->
                           Server ! {start, self()},
                           ok
                   end},
         #{desc => "await ready (from server)",
           cmd  => fun(#{server := Server, lsa := LSA} = State) ->
                           receive
                               {ready, Server, Port} ->
                                   {ok, State#{ssa => LSA#{port => Port}}}
                           end
                   end},

         %% *** Connect sequence ***
         #{desc => "order (server) start",
           cmd  => fun(#{sock1   := Sock1,
                         sock2   := Sock2,
                         sock3   := Sock3,
                         ssa     := SSA,
                         timeout := To}) ->
                           Socks = [Sock1, Sock2, Sock3],
                           api_to_connect_tcp_await_timeout(Socks, To, SSA)
                   end},

         %% *** Terminate server ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Server} = State) ->
                           MRef = erlang:monitor(process, Server),
                           {ok, State#{server_mref => MRef}}
                   end},
         #{desc => "order (server) terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           Server ! {terminate, self()},
                           ok
                   end},
         #{desc => "await (server) down",
           cmd  => fun(#{server := Server} = State) ->
                           receive
                               {'DOWN', _, process, Server, _} ->
                                   State1 = maps:remove(server, State),
                                   State2 = maps:remove(ssa, State1),
                                   {ok, State2}
                           end
                   end},

         %% *** Close our sockets ***
         #{desc => "close socket 3",
           cmd  => fun(#{sock3 := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock3, State)}

                   end},
         #{desc => "close socket 2",
           cmd  => fun(#{sock2 := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock2, State)}

                   end},
         #{desc => "close socket 1",
           cmd  => fun(#{sock1 := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock1, State)}

                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("create server evaluator"),
    ServerInitState = InitState,
    Server          = evaluator_start("server", ServerSeq, ServerInitState),

    p("create tester evaluator"),
    TesterInitState = InitState#{server => Server},
    Tester          = evaluator_start("tester", TesterSeq, TesterInitState),

    p("await evaluator(s)"),
    ok = await_evaluator_finish([Server, Tester]).


api_to_connect_tcp_await_timeout(Socks, To, ServerSA) ->
    api_to_connect_tcp_await_timeout(Socks, To, ServerSA, 1).

api_to_connect_tcp_await_timeout([], _To, _ServerSA, _ID) ->
    ?FAIL(unexpected_success);
api_to_connect_tcp_await_timeout([Sock|Socks], To, ServerSA, ID) ->
    ei("~w: try connect", [ID]),
    Start = t(),
    case socket:connect(Sock, ServerSA, To) of
        {error, timeout} ->
            ei("expected timeout (~w)", [ID]),
            Stop  = t(),
            TDiff = tdiff(Start, Stop),
            if
                (TDiff >= To) ->
                    ok;
                true ->
                    {error, {unexpected_timeout, TDiff, To}}
            end;
        {error, econnreset = Reason} ->
            ei("failed connecting: ~p - giving up", [Reason]),
            ok;
        {error, Reason} ->
            ee("failed connecting: ~p", [Reason]),
            ?FAIL({connect, Reason});
        ok ->
            ei("unexpected success (~w) - try next", [ID]),
            api_to_connect_tcp_await_timeout(Socks, To, ServerSA, ID+1)
    end.
        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv4 TCP (stream) socket.
api_to_accept_tcp4(suite) ->
    [];
api_to_accept_tcp4(doc) ->
    [];
api_to_accept_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_accept_tcp4,
           fun() ->
                   InitState = #{domain => inet, timeout => 5000},
                   ok = api_to_accept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv6 TCP (stream) socket.
api_to_accept_tcp6(suite) ->
    [];
api_to_accept_tcp6(doc) ->
    [];
api_to_accept_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_accept_tcp4,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain => inet6, timeout => 5000},
                   ok = api_to_accept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_accept_tcp(InitState) ->
    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},

         %% *** The actual test part ***
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = tdiff(Start, Stop),
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},

         %% *** Close (listen) socket ***
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(sock3, State)}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("create tester evaluator"),
    Tester = evaluator_start("tester", TesterSeq, InitState),

    p("await evaluator"),
    ok = await_evaluator_finish([Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the multi accept timeout option
%% on an IPv4 TCP (stream) socket with multiple acceptor processes 
%% (three in this case).
api_to_maccept_tcp4(suite) ->
    [];
api_to_maccept_tcp4(doc) ->
    [];
api_to_maccept_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_maccept_tcp4,
           fun() ->
                   InitState = #{domain => inet, timeout => 5000},
                   ok = api_to_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the accept timeout option
%% on an IPv6 TCP (stream) socket.
api_to_maccept_tcp6(suite) ->
    [];
api_to_maccept_tcp6(doc) ->
    [];
api_to_maccept_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_maccept_tcp4,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain => inet6, timeout => 5000},
                   ok = api_to_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_maccept_tcp(InitState) ->
    PrimAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} ->
                                   MRef = erlang:monitor(process, Tester),
                                   {ok, State#{tester      => Tester,
                                               tester_mref => MRef}}
                           end
                   end},
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},

         #{desc => "announce ready",
           cmd  => fun(#{lsock := LSock, tester := Tester}) ->
                           ei("announcing port to tester (~p)", [Tester]),
                           Tester ! {ready, self(), LSock},
                           ok
                   end},
         #{desc => "await continue",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},

         %% *** The actual test part ***
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = tdiff(Start, Stop),
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready",
           cmd  => fun(#{tester := Tester}) ->
                           ei("announcing port to tester (~p)", [Tester]),
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   ok
                           end
                   end},

         %% *** Close (listen) socket ***
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],


    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester, LSock} ->
                                   MRef = erlang:monitor(process, Tester),
                                   {ok, State#{tester      => Tester,
                                               lsock       => LSock,
                                               tester_mref => MRef}}
                           end
                   end},
         #{desc => "announce ready (1)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester, Reason}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},

         %% *** The actual test part ***
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = tdiff(Start, Stop),
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready (2)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester, Reason}};
                               {terminate, Tester} ->
                                   ok
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],


    TesterSeq =
        [
         %% Init part
         #{desc => "monitor prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},


         %% Start the prim-acceptor
         #{desc => "start prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await prim-acceptor ready (1)",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-acceptor ~p:"
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, prim_acceptor}};
                               {ready, Pid, LSock} ->
                                   {ok, State#{lsock => LSock}}
                           end
                   end},

         %% Start sec-acceptor-1
         #{desc => "start sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid, lsock := LSock} = _State) ->
                           Pid ! {start, self(), LSock},
                           ok
                   end},
         #{desc => "await sec-acceptor 1 ready (1)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acceptor 1 ~p:"
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, sec_acceptor_1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Start sec-acceptor-2
         #{desc => "start sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid, lsock := LSock} = _State) ->
                           Pid ! {start, self(), LSock},
                           ok
                   end},
         #{desc => "await sec-acceptor 2 ready (1)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acceptor 2 ~p:"
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, sec_acceptor_2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Activate the acceptor(s)
         #{desc => "active prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "active sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "active sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},

         %% Await acceptor(s) completions
         #{desc => "await prim-acceptor ready (2)",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-acceptor ~p:"
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, prim_acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await sec-acceptor 1 ready (2)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acceptor 1 ~p:"
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, sec_acceptor_1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await sec-acceptor 2 ready (2)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acceptor 2 ~p:"
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, sec_acceptor_2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},


         %% Terminate the acceptor(s)
         #{desc => "order prim-acceptor to terminate",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ei("send terminate command to prim-acceptor (~p)", [Pid]),
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "order sec-acceptor 1 to terminate",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ei("send terminate command to sec-acceptor-1 (~p)", [Pid]),
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "order sec-acceptor 2 to terminate",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ei("send terminate command to sec-acceptor-2 (~p)", [Pid]),
                           Pid ! {terminate, self()},
                           ok
                   end},

         %% Await acceptor(s) termination
         #{desc => "await prim-acceptor termination",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   State1 = maps:remove(prim_acceptor, State),
                                   {ok, State1}
                           end
                   end},
         #{desc => "await sec-acceptor 1 termination",
           cmd  => fun(#{sec_acceptor1 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   State1 = maps:remove(sec_acceptor1, State),
                                   {ok, State1}
                           end
                   end},
         #{desc => "await sec-acceptor 2 termination",
           cmd  => fun(#{sec_acceptor2 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   State1 = maps:remove(sec_acceptor2, State),
                                   {ok, State1}
                           end
                   end},
         
         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("create prim-acceptor evaluator"),
    PrimAInitState = InitState,
    PrimAcceptor = evaluator_start("prim-acceptor", 
                                   PrimAcceptorSeq, PrimAInitState),

    p("create prim-acceptor 1 evaluator"),
    SecAInitState1 = maps:remove(domain, InitState),
    SecAcceptor1   = evaluator_start("sec-acceptor-1", 
                                     SecAcceptorSeq, SecAInitState1),

    p("create prim-acceptor 2 evaluator"),
    SecAInitState2 = SecAInitState1,
    SecAcceptor2   = evaluator_start("sec-acceptor-2", 
                                     SecAcceptorSeq, SecAInitState2),

    p("create tester evaluator"),
    TesterInitState = #{prim_acceptor => PrimAcceptor,
                        sec_acceptor1 => SecAcceptor1,
                        sec_acceptor2 => SecAcceptor2},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    p("await evaluator(s)"),
    ok = await_evaluator_finish([PrimAcceptor, SecAcceptor1, SecAcceptor2, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the send timeout option
%% on an IPv4 TCP (stream) socket.
api_to_send_tcp4(suite) ->
    [];
api_to_send_tcp4(doc) ->
    [];
api_to_send_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_send_tcp4,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_send_tcp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the send timeout option
%% on an IPv6 TCP (stream) socket.
api_to_send_tcp6(suite) ->
    [];
api_to_send_tcp6(doc) ->
    [];
api_to_send_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_send_tcp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_send_tcp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendto timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_sendto_udp4(suite) ->
    [];
api_to_sendto_udp4(doc) ->
    [];
api_to_sendto_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_sendto_udp4,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendto_to_udp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendto timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_sendto_udp6(suite) ->
    [];
api_to_sendto_udp6(doc) ->
    [];
api_to_sendto_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_sendto_udp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendto_to_udp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendmsg timeout option
%% on an IPv4 TCP (stream) socket.
api_to_sendmsg_tcp4(suite) ->
    [];
api_to_sendmsg_tcp4(doc) ->
    [];
api_to_sendmsg_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_sendmsg_tcp4,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendmsg_tcp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the sendmsg timeout option
%% on an IPv6 TCP (stream) socket.
api_to_sendmsg_tcp6(suite) ->
    [];
api_to_sendmsg_tcp6(doc) ->
    [];
api_to_sendmsg_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_sendmsg_tcp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_sendmsg_tcp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv4 UDP (dgram) socket. To test this we must connect
%% the socket.
api_to_recv_udp4(suite) ->
    [];
api_to_recv_udp4(doc) ->
    [];
api_to_recv_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_udp4,
           fun() ->
                   not_yet_implemented()%%,
                   %%ok = api_to_recv_udp(inet)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv6 UDP (dgram) socket. To test this we must connect
%% the socket.
api_to_recv_udp6(suite) ->
    [];
api_to_recv_udp6(doc) ->
    [];
api_to_recv_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_udp6,
           fun() ->
                   not_yet_implemented()%% ,
                   %% ok = api_to_recv_udp(inet6)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv4 TCP (stream) socket.
api_to_recv_tcp4(suite) ->
    [];
api_to_recv_tcp4(doc) ->
    [];
api_to_recv_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_tcp4,
           fun() ->
                   Recv = fun(Sock, To) -> socket:recv(Sock, 0, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recv timeout option
%% on an IPv6 TCP (stream) socket.
api_to_recv_tcp6(suite) ->
    [];
api_to_recv_tcp6(doc) ->
    [];
api_to_recv_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recv_tcp6,
           fun() ->
                   not_yet_implemented(),
                   case socket:supports(ipv6) of
                       true ->
                           Recv = fun(Sock, To) -> 
                                          socket:recv(Sock, 0, To)
                                  end,
                           InitState = #{domain  => inet6,
                                         recv    => Recv,
                                         timeout => 5000},
                           ok = api_to_receive_tcp(InitState);
                       false ->
                           skip("ipv6 not supported")
                   end
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_receive_tcp(InitState) ->
    process_flag(trap_exit, true),

    ServerSeq = 
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} when is_pid(Tester) ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket (with backlog = 1)",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock, 1)
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = State) ->
                           MRef = erlang:monitor(process, Tester),
                           {ok, State#{tester_mref => MRef}}
                   end},
         #{desc => "announce ready",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           Tester ! {ready, self(), Port},
                           ok
                   end},
         #{desc => "await continue",
           cmd  => fun(#{tester := Tester}) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   {error, {unexpected_exit, tester, Reason}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},

         %% *** The actual test ***
         #{desc => "await accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "attempt to recv (without success)",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := To} = State) ->
                           Start = t(),
                           case Recv(Sock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, _Data} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = tdiff(Start, Stop),
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready (recv timeout success)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   {error, {unexpected_exit, tester, Reason}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         #{desc => "close (traffic) socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},
         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester, Port} when is_pid(Tester) ->
                                   {ok, State#{tester      => Tester,
                                               server_port => Port}}
                           end
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain, server_port := Port} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain,
                                     addr   => LAddr},
                           SSA   = LSA#{port => Port},
                           {ok, State#{lsa => LSA, ssa => SSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, stream, tcp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = State) ->
                           MRef = erlang:monitor(process, Tester),
                           {ok, State#{tester_mref => MRef}}
                   end},
         #{desc => "announce ready",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (with connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   {error, {unexpected_exit, tester, Reason}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "connect",
           cmd  => fun(#{sock := Sock, ssa := SSA}) ->
                           sock_connect(Sock, SSA),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   {error, {unexpected_exit, tester, Reason}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Server} = State) ->
                           MRef = erlang:monitor(process, Server),
                           {ok, State#{server_mref => MRef}}
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = State) ->
                           MRef = erlang:monitor(process, Client),
                           {ok, State#{client_mref => MRef}}
                   end},

         %% *** Activate server ***
         #{desc => "start server",
           cmd  => fun(#{server := Server} = _State) ->
                           Server ! {start, self()},
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server} = State) ->
                           receive
                               {'DOWN', _, process, Server, Reason} ->
                                   {error, {unexpected_exit, server, Reason}};
                               {ready, Server, Port} ->
                                   {ok, State#{server_port => Port}}
                           end
                   end},
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           Server ! {continue, self()},
                           ok
                   end},

         %% *** Activate client ***
         #{desc => "start client",
           cmd  => fun(#{client := Client, server_port := Port} = _State) ->
                           Client ! {start, self(), Port},
                           ok
                   end},
         #{desc => "await client ready",
           cmd  => fun(#{client := Client} = _State) ->
                           receive
                               {'DOWN', _, process, Client, Reason} ->
                                   {error, {unexpected_exit, client, Reason}};
                               {ready, Client} ->
                                   ok
                           end
                   end},

         %% *** The actual test ***
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           Client ! {continue, self()},
                           ok
                   end},
         #{desc => "await server ready (accept/recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           receive
                               {'DOWN', _, process, Server, Reason} ->
                                   {error, {unexpected_exit, server, Reason}};
                               {ready, Server} ->
                                   ok
                           end
                   end},

         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           Client ! {terminate, self()},
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           receive
                               {'DOWN', _, process, Client, _Reason} ->
                                   State1 = maps:remove(client, State),
                                   State2 = maps:remove(client_mref, State1),
                                   {ok, State2}
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           Server ! {terminate, self()},
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           receive
                               {'DOWN', _, process, Server, _Reason} ->
                                   State1 = maps:remove(server, State),
                                   State2 = maps:remove(server_mref, State1),
                                   State3 = maps:remove(server_port, State2),
                                   {ok, State3}
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    
    p("start server evaluator"),
    ServerInitState = InitState,
    Server = evaluator_start("server", ServerSeq, ServerInitState),

    p("start client evaluator"),
    ClientInitState = InitState,
    Client = evaluator_start("client", ClientSeq, ClientInitState),

    p("start tester evaluator"),
    TesterInitState = #{server => Server, client => Client},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    p("await evaluator(s)"),
    ok = await_evaluator_finish([Server, Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvfrom timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_recvfrom_udp4(suite) ->
    [];
api_to_recvfrom_udp4(doc) ->
    [];
api_to_recvfrom_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recvfrom_udp4,
           fun() ->
                   Recv = fun(Sock, To) -> socket:recvfrom(Sock, 0, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvfrom timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_recvfrom_udp6(suite) ->
    [];
api_to_recvfrom_udp6(doc) ->
    [];
api_to_recvfrom_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recvfrom_udp6,
           fun() ->
                   not_yet_implemented(),
                   Recv = fun(Sock, To) -> socket:recvfrom(Sock, 0, To) end,
                   InitState = #{domain  => inet6,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_to_receive_udp(InitState) ->
    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, dgram, udp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _Port} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** The actual test ***
         #{desc => "attempt to read (without success)",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := To} = State) ->
                           Start = t(),
                           case Recv(Sock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, _} ->
                                   {error, unexpected_sucsess};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "validate timeout time",
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = _State) ->
                           TDiff  = tdiff(Start, Stop),
                           if
                               (TDiff >= To) ->
                                   ok;
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         
         %% *** Termination ***
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = _State) ->
                           sock_close(Sock),
                           ok
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("start tester evaluator"),
    Tester = evaluator_start("tester", TesterSeq, InitState),
    
    p("await evaluator"),
    ok = await_evaluator_finish([Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv4 UDP (dgram) socket.
api_to_recvmsg_udp4(suite) ->
    [];
api_to_recvmsg_udp4(doc) ->
    [];
api_to_recvmsg_udp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_udp4,
           fun() ->
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv6 UDP (dgram) socket.
api_to_recvmsg_udp6(suite) ->
    [];
api_to_recvmsg_udp6(doc) ->
    [];
api_to_recvmsg_udp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_udp6,
           fun() ->
                   not_yet_implemented(),
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet6,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv4 TCP (stream) socket.
api_to_recvmsg_tcp4(suite) ->
    [];
api_to_recvmsg_tcp4(doc) ->
    [];
api_to_recvmsg_tcp4(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_tcp4,
           fun() ->
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is intended to test the recvmsg timeout option
%% on an IPv6 TCP (stream) socket.
api_to_recvmsg_tcp6(suite) ->
    [];
api_to_recvmsg_tcp6(doc) ->
    [];
api_to_recvmsg_tcp6(_Config) when is_list(_Config) ->
    tc_try(api_to_recvmsg_tcp6,
           fun() ->
                   not_yet_implemented(),
                   Recv = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain  => inet6,
                                 recv    => Recv,
                                 timeout => 5000},
                   ok = api_to_receive_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                         SOCKET CLOSURE                              %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv4 TCP (stream) socket.

sc_cpe_socket_cleanup_tcp4(suite) ->
    [];
sc_cpe_socket_cleanup_tcp4(doc) ->
    [];
sc_cpe_socket_cleanup_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_tcp4,
           fun() ->
                   %% not_yet_implemented(),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv6 TCP (stream) socket.

sc_cpe_socket_cleanup_tcp6(suite) ->
    [];
sc_cpe_socket_cleanup_tcp6(doc) ->
    [];
sc_cpe_socket_cleanup_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_tcp6,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv4 UDP (dgram) socket.

sc_cpe_socket_cleanup_udp4(suite) ->
    [];
sc_cpe_socket_cleanup_udp4(doc) ->
    [];
sc_cpe_socket_cleanup_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_udp4,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% (removed) when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv6 UDP (dgram) socket.

sc_cpe_socket_cleanup_udp6(suite) ->
    [];
sc_cpe_socket_cleanup_udp6(doc) ->
    [];
sc_cpe_socket_cleanup_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_cpe_socket_cleanup_udp6,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain   => inet6,
                                 type     => dgram,
                                 protocol => udp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_cpe_socket_cleanup(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} when is_pid(Tester) ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},

         %% *** Init part ***
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           Tester ! {ready, self(), Sock},
                           ok
                   end},

         %% *** The actual test ***
         %% We intentially leave the socket "as is", no explicit close
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         #{desc => "enable (otp) debug",
           cmd  => fun(#{sock := Sock} = _State) ->
                           ok = socket:setopt(Sock, otp, debug, true)
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Owner} = State) ->
                           receive
                               {'DOWN', _, process, Owner, Reason} ->
                                   ee("Unexpected DOWN regarding owner ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, owner}};
                               {ready, Owner, Sock} ->
                                   {ok, State#{sock => Sock}}
                           end
                   end},
         #{desc => "verify owner as controlling-process",
           cmd  => fun(#{owner := Owner, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, Owner} ->
                                   ok;
                               {ok, Other} ->
                                   {error, {unexpected_owner, Other}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Owner} = _State) ->
                           receive
                               {'DOWN', _, process, Owner, _} ->
                                   ok
                           end
                   end},
         #{desc => "verify no socket (closed)",
           cmd  => fun(#{owner := Owner, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, Pid} ->
                                   {error, {unexpected_success, Owner, Pid}};
                               {error, closed} ->
                                   ok;
                               {error, Reason} ->
                                   ei("expected failure: ~p", [Reason]),
                                   {error, {unexpected_failure, Reason}}
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("start (socket) owner evaluator"),
    Owner = evaluator_start("owner", OwnerSeq, InitState),

    p("start tester evaluator"),
    TesterInitState = #{owner => Owner},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    p("await evaluator"),
    ok = await_evaluator_finish([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while a process is calling the recv function.
%% Socket is IPv4.
%% 
%% <KOLLA>
%% 
%% We should really have a similar test cases for when the controlling
%% process exits and there are other processes in recv, accept, and 
%% all the other functions.
%% 
%% </KOLLA>

sc_recv_response_local_close_tcp4(suite) ->
    [];
sc_recv_response_local_close_tcp4(doc) ->
    [];
sc_recv_response_local_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_recv_response_local_close_tcp4,
           fun() ->
                   %% not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_local_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_recv_response_local_close_tcp6(suite) ->
    [];
sc_recv_response_local_close_tcp6(doc) ->
    [];
sc_recv_response_local_close_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_recv_response_local_close_tcp6,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_local_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_receive_response_local_close_tcp(InitState) ->
    %% This is the server that accepts connections.
    %% But it is also suppose to close the connection socket, 
    %% and trigger the read failure for the handler process.
    AcceptorSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} when is_pid(Tester) ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{lsock := LSock, lsa := LSA} = State) ->
                           case socket:bind(LSock, LSA) of
                               {ok, Port} ->
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           Tester ! {ready, self(), Port},
                           ok
                   end},
                           
         %% The actual test
         #{desc => "await continue (connection)",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester, Handler} ->
                                   {ok, State#{handler => Handler}}
                           end
                   end},
         #{desc => "await connection",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ei("connection accepted"),
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "transfer new connection to handler",
           cmd  => fun(#{handler := Handler, csock := Sock}) ->
                           ok = socket:setopt(Sock, 
                                              otp, controlling_process, 
                                              Handler),
                           Handler ! {connection, Sock},
                           ok
                   end},
         #{desc => "announce ready (connection)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         %% #{desc => "enable debug",
         %%   cmd  => fun(#{csock := Sock}) ->
         %%                   socket:setopt(Sock, otp, debug, true)
         %%           end},
         #{desc => "close (the connection) socket",
           cmd  => fun(#{csock := Sock}) ->
                           socket:close(Sock)
                   end},

         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         #{desc => "socket cleanup",
           cmd  => fun(#{lsock := Sock} = State) ->
                           ok = socket:close(Sock),
                           State1 = maps:remove(csock, State),
                           State2 = maps:remove(lsock, State1),
                           State3 = maps:remove(lport, State2),
                           {ok, State3}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    %% The point of this is to perform the recv for which we are testing the reponse
    HandlerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} when is_pid(Tester) ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},
         #{desc => "monitor server",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% The actual test
         #{desc => "await connection socket",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {connection, Sock} ->
                                   {ok, State#{sock => Sock}}
                           end
                   end},
         #{desc => "announce ready (connection)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         %% #{desc => "enable debug",
         %%   cmd  => fun(#{sock := Sock}) ->
         %%                   socket:setopt(Sock, otp, debug, true)
         %%           end},
         %% #{desc => "monitored-by",
         %%   cmd  => fun(_) ->
         %%                   {_, Mons} = process_info(self(), monitored_by),
         %%                   ei("Monitored By: ~p", [Mons]),
         %%                   ok
         %%           end},
         #{desc => "attempt recv",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:recv(Sock) of
                               {ok, _Data} ->
                                   ee("Unexpected data received"),
                                   {error, unexpected_data};
                               {error, closed} ->
                                   State1 = maps:remove(sock, State),
                                   {ok, State1};
                               {error, Reason} = ERROR ->
                                   ee("Unexpected read faulure: "
                                      "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         %% #{desc => "monitored-by",
         %%   cmd  => fun(_) ->
         %%                   {_, Mons} = process_info(self(), monitored_by),
         %%                   ei("Monitored By: ~p", [Mons]),
         %%                   ok
         %%           end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "sleep some",
           cmd  => fun(_) ->
                           ?SLEEP(1000),
                           ok
                   end},
         %% #{desc => "monitored-by",
         %%   cmd  => fun(_) ->
         %%                   {_, Mons} = process_info(self(), monitored_by),
         %%                   ei("Monitored By: ~p", [Mons]),
         %%                   ok
         %%           end},
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   ok
                           end
                   end},


         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    %% The point of this is basically just to create the connection.
    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} when is_pid(Tester) ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},

         %% Init
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% The actual test
         #{desc => "await continue",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester, Reason}};
                               {continue, Tester, Port} ->
                                   {ok, State#{lport => Port}}
                           end
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, lsa := LSA, lport := LPort}) ->
                           socket:connect(Sock, LSA#{port => LPort})
                   end},
         #{desc => "announce ready (connection)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% Cleaning up
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor acceptor",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler",
           cmd  => fun(#{handler := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the acceptor
         #{desc => "order acceptor start",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await acceptor ready (init)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding acceptor ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid, Port} ->
                                   {ok, State#{lport => Port}}
                           end
                   end},

         %% Start the handler
         #{desc => "order handler start",
           cmd  => fun(#{handler := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await handler ready (init)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding cient ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% The actual test
         #{desc => "order acceptor to continue",
           cmd  => fun(#{acceptor := Pid, handler := Handler} = _State) ->
                           Pid ! {continue, self(), Handler},
                           ok
                   end},
         #{desc => "order client to continue",
           cmd  => fun(#{client := Pid, lport := Port} = _State) ->
                           Pid ! {continue, self(), Port},
                           ok
                   end},
         #{desc => "await acceptor ready (connection)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding acceptor ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await client ready (connection)",
           cmd  => fun(#{client := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await handler ready (connection)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler ~p: "
                                      "~n   ~p", [Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "sleep some",
           cmd  => fun(_State) ->
                           ?SLEEP(1000),
                           ok
                   end},
         #{desc => "order acceptor to continue (close)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "await handler ready (close)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, acceptor}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Terminations
         #{desc => "order handler to terminate",
           cmd  => fun(#{handler := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await handler termination",
           cmd  => fun(#{handler := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(handler, State)}
                           end
                   end},
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(client, State)}
                           end
                   end},
         #{desc => "order acceptor to terminate",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await acceptor termination",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(acceptor, State)}
                           end
                   end},


         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    p("start acceptor evaluator"),
    AccInitState = InitState,
    Acceptor = evaluator_start("acceptor", AcceptorSeq, AccInitState),

    p("start handler evaluator"),
    HandlerInitState = #{},
    Handler = evaluator_start("handler", HandlerSeq, HandlerInitState),

    p("start client evaluator"),
    ClientInitState = InitState,
    Client = evaluator_start("client", ClientSeq, ClientInitState),

    p("start tester evaluator"),
    TesterInitState = #{acceptor => Acceptor,
                        handler  => Handler,
                        client   => Client},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    p("await evaluator"),
    ok = await_evaluator_finish([Acceptor, Handler, Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv4.

sc_recv_response_remote_close_tcp4(suite) ->
    [];
sc_recv_response_remote_close_tcp4(doc) ->
    [];
sc_recv_response_remote_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_recv_response_remote_close_tcp4,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_remote_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv6.

sc_recv_response_remote_close_tcp6(suite) ->
    [];
sc_recv_response_remote_close_tcp6(doc) ->
    [];
sc_recv_response_remote_close_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_recv_response_remote_close_tcp6,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_remote_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_receive_response_remote_close_tcp(_InitState) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_recvmsg_response_local_close_tcp4(suite) ->
    [];
sc_recvmsg_response_local_close_tcp4(doc) ->
    [];
sc_recvmsg_response_local_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_local_close_tcp4,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_local_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_recvmsg_response_local_close_tcp6(suite) ->
    [];
sc_recvmsg_response_local_close_tcp6(doc) ->
    [];
sc_recvmsg_response_local_close_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_local_close_tcp6,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_local_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_recvmsg_response_remote_close_tcp4(suite) ->
    [];
sc_recvmsg_response_remote_close_tcp4(doc) ->
    [];
sc_recvmsg_response_remote_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_remote_close_tcp4,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_remote_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_recvmsg_response_remote_close_tcp6(suite) ->
    [];
sc_recvmsg_response_remote_close_tcp6(doc) ->
    [];
sc_recvmsg_response_remote_close_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_remote_close_tcp6,
           fun() ->
                   not_yet_implemented(),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_receive_response_remote_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv4.

sc_acceptor_response_local_close_tcp4(suite) ->
    [];
sc_acceptor_response_local_close_tcp4(doc) ->
    [];
sc_acceptor_response_local_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_acceptor_response_local_close_tcp4,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_acceptor_response_local_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv6.

sc_acceptor_response_local_close_tcp6(suite) ->
    [];
sc_acceptor_response_local_close_tcp6(doc) ->
    [];
sc_acceptor_response_local_close_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_acceptor_response_local_close_tcp6,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_acceptor_response_local_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_acceptor_response_local_close_tcp(_InitState) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv4.

sc_acceptor_response_remote_close_tcp4(suite) ->
    [];
sc_acceptor_response_remote_close_tcp4(doc) ->
    [];
sc_acceptor_response_remote_close_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_acceptor_response_remote_close_tcp4,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_acceptor_response_remote_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv6.

sc_acceptor_response_remote_close_tcp6(suite) ->
    [];
sc_acceptor_response_remote_close_tcp6(doc) ->
    [];
sc_acceptor_response_remote_close_tcp6(_Config) when is_list(_Config) ->
    tc_try(acceptor_response_remote_close_tcp6,
           fun() ->
                   not_yet_implemented(),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_acceptor_response_remote_close_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_acceptor_response_remote_close_tcp(_InitState) ->
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

%% An evaluator is a process that executes a command sequence.
%% A test case will consist of atleast one evaluator (one for
%% each actor).
%% The evaluator process *always* run locally. Which means that
%% it will act as a "proxy" for remote nodes in necessary.
%% When the command sequence has been processed, the final state
%% will be used as exit reason.
%% A successful command shall evaluate to ok | {ok, NewState} 

-spec evaluator_start(Name, Seq, Init) -> {Pid, MRef} when
      Name :: string(),
      Seq  :: [command()],
      Init :: initial_evaluator_state(),
      Pid  :: pid(),
      MRef :: reference().
                             
evaluator_start(Name, Seq, Init) 
  when is_list(Name) andalso is_list(Seq) andalso (Seq =/= []) ->
    Init2 = Init#{parent => self()},
    {Pid, _} = erlang:spawn_monitor(fun() -> evaluator_init(Name, Seq, Init2) end),
    Pid.

evaluator_init(Name, Seq, Init) ->
    put(sname, Name),
    evaluator_loop(1, Seq, Init).

evaluator_loop(_ID, [], FinalState) ->
    exit(FinalState);
evaluator_loop(ID, [#{desc := Desc,
                      cmd  := Cmd}|Cmds], State) when is_function(Cmd, 1) ->
    ei("evaluate command ~2w: ~s", [ID, Desc]),
    try Cmd(State) of
        ok ->
            evaluator_loop(ID + 1, Cmds, State);
        {ok, NewState} ->
            evaluator_loop(ID + 1, Cmds, NewState);
        {error, Reason} ->
            ee("command ~w failed: "
               "~n   Reason: ~p", [ID, Reason]),
            exit({command_failed, ID, Reason, State})
    catch
        C:E:S ->
            ee("command ~w crashed: "
               "~n   Class:      ~p"
               "~n   Error:      ~p"
               "~n   Call Stack: ~p", [ID, C, E, S]),
            exit({command_crashed, ID, {C,E,S}, State})
    end.

await_evaluator_finish(Evs) ->
    await_evaluator_finish(Evs, []).

await_evaluator_finish([], []) ->
    ok;
await_evaluator_finish([], Fails) ->
    Fails;
await_evaluator_finish(Evs, Fails) ->
    receive
        {'DOWN', _MRef, process, Pid, normal} ->
            case lists:delete(Pid, Evs) of
                Evs ->
                    p("unknown process ~p died (normal)", [Pid]),
                    await_evaluator_finish(Evs, Fails);
                NewEvs ->
                    p("evaluator ~p success", [Pid]),
                    await_evaluator_finish(NewEvs, Fails)
            end;
        {'DOWN', _MRef, process, Pid, Reason} ->
            case lists:delete(Pid, Evs) of
                Evs ->
                    p("unknown process ~p died: "
                        "~n   ~p", [Pid, Reason]),
                    await_evaluator_finish(Evs, Fails);
                NewEvs ->
                    p("Evaluator ~p failed", [Pid]),
                    await_evaluator_finish(NewEvs, [{Pid, Reason}|Fails])
            end
    end.


ei(F) ->
    ei(F, []).
ei(F, A) ->
    eprint("", F, A).

ee(F) ->
    ee(F, []).
ee(F, A) ->
    eprint("<ERROR> ", F, A).

eprint(Prefix, F, A) ->
    io:format(user, "[~s][~s][~p] ~s" ++ F ++ "~n", 
              [formated_timestamp(), get(sname), self(), Prefix | A]).



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
    

%% sock_listen(Sock) ->
%%     sock_listen2(fun() -> socket:listen(Sock) end).

%% sock_listen(Sock, BackLog) ->
%%     sock_listen2(fun() -> socket:listen(Sock, BackLog) end).

%% sock_listen2(Listen) ->
%%     try Listen() of
%%         ok ->
%%             ok;
%%         {error, Reason} ->
%%             ?FAIL({listen, Reason})
%%     catch
%%         C:E:S ->
%%             ?FAIL({listen, C, E, S})
%%     end.


%% sock_accept(LSock) ->
%%     try socket:accept(LSock) of
%%         {ok, Sock} ->
%%             Sock;
%%         {error, Reason} ->
%%             p("sock_accept -> error: ~p", [Reason]),
%%             ?FAIL({accept, Reason})
%%     catch
%%         C:E:S ->
%%             p("sock_accept -> failed: ~p, ~p, ~p", [C, E, S]),
%%             ?FAIL({accept, C, E, S})
%%     end.


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
    skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t() ->
    os:timestamp().


tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, _N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    %% {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    %% FormatTS = 
    %%     io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~w",
    %%                   [YYYY, MM, DD, Hour, Min, Sec, N3]),  
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec]),  
    lists:flatten(FormatTS).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_tc_name(N) when is_atom(N) ->
    set_tc_name(atom_to_list(N));
set_tc_name(N) when is_list(N) ->
    put(tc_name, N).

%% get_tc_name() ->
%%     get(tc_name).

tc_begin(TC) ->
    set_tc_name(TC),
    p("begin ***").
    
tc_end(Result) when is_list(Result) ->
    p("done: ~s", [Result]),
    ok.


tc_try(Case, Fun) when is_atom(Case) andalso is_function(Fun, 0) ->
    tc_begin(Case),
    try 
        begin
            Fun(),
            tc_end("ok")
        end
    catch
        throw:{skip, _} = SKIP ->
            tc_end("skipping"),
            SKIP;
        Class:Error:Stack ->
            tc_end("failed"),
            erlang:raise(Class, Error, Stack)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% f(F, A) ->
%%     lists:flatten(io_lib:format(F, A)).

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
    i("*** [~s][~s][~p] " ++ F, [formated_timestamp(),TcName,self()|A]).


%% i(F) ->
%%     i(F, []).

i(F, A) ->
    io:format(user, F ++ "~n", A).
