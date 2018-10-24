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
         sc_lc_recv_response_tcp4/1,
         sc_lc_recv_response_tcp6/1,
         sc_lc_recvfrom_response_udp4/1,
         sc_lc_recvfrom_response_udp6/1,
         sc_lc_recvmsg_response_tcp4/1,
         sc_lc_recvmsg_response_tcp6/1,
         sc_lc_recvmsg_response_udp4/1,
         sc_lc_recvmsg_response_udp6/1,
         sc_lc_acceptor_response_tcp4/1,
         sc_lc_acceptor_response_tcp6/1,
         sc_rc_recv_response_tcp4/1,
         sc_rc_recv_response_tcp6/1,
         sc_rc_recvmsg_response_tcp4/1,
         sc_rc_recvmsg_response_tcp6/1

         %% Traffic



         %% Tickets
        ]).

%% Internal exports
%% -export([]).

-record(ev, {name :: string(),
             pid  :: pid(),
             mref :: reference()}).
-type ev() :: #ev{}.
-define(MKEV(N,P,R), #ev{name = N, pid = P, mref = R}).
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

-define(MINS(M), timer:minutes(M)).
-define(SECS(S), timer:seconds(S)).

-define(TT(T),   ct:timetrap(T)).

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
     {sc_ctrl_proc_exit,   [], sc_cp_exit_cases()},
     {sc_local_close,      [], sc_lc_cases()},
     {sc_remote_close,     [], sc_rc_cases()}
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
     api_to_maccept_tcp4,
     api_to_maccept_tcp6,
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
     {group, sc_local_close},
     {group, sc_remote_close}
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

%% These cases tests what happens when the socket is closed locally.
sc_lc_cases() ->
    [
     sc_lc_recv_response_tcp4,
     sc_lc_recv_response_tcp6,

     sc_lc_recvfrom_response_udp4,
     sc_lc_recvfrom_response_udp6,

     sc_lc_recvmsg_response_tcp4,
     sc_lc_recvmsg_response_tcp6,
     sc_lc_recvmsg_response_udp4,
     sc_lc_recvmsg_response_udp6,

     sc_lc_acceptor_response_tcp4,
     sc_lc_acceptor_response_tcp6
    ].

%% These cases tests what happens when the socket is closed remotely.
sc_rc_cases() ->
    [
     sc_rc_recv_response_tcp4,
     sc_rc_recv_response_tcp6,

     sc_rc_recvmsg_response_tcp4,
     sc_rc_recvmsg_response_tcp6
    ].


%% traffic_cases() ->
%%     [].


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
    ?TT(?SECS(5)),
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
    ?TT(?SECS(5)),
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
    ?TT(?SECS(5)),
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
    ?TT(?SECS(5)),
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
    ?TT(?SECS(10)),
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
    ?TT(?SECS(10)),
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
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ev_await_start(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
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
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           ev_ready(Tester, init, Port),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ok = ev_await_continue(Tester, tester, accept)
                   end},
         #{desc => "await connection",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ei("accepted: ~n   ~p", [Sock]),
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ev_ready(Tester, accept),
                           ok
                   end},
         #{desc => "await (recv) request",
           cmd  => fun(#{csock := Sock, recv := Recv}) ->
                           case Recv(Sock) of
                               {ok, ?BASIC_REQ} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv request)",
           cmd  => fun(#{tester := Tester}) ->
                           %% Tester ! {ready, self(), Port},
                           ev_ready(Tester, recv_req),
                           ok
                   end},
         #{desc => "await continue (with send reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ok = ev_await_continue(Tester, tester, send_reply)
                   end},
         #{desc => "send reply",
           cmd  => fun(#{csock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REP)
                   end},
         #{desc => "announce ready (send reply)",
           cmd  => fun(#{tester := Tester}) ->
                           %% Tester ! {ready, self(), Port},
                           ev_ready(Tester, send_reply),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ev_await_terminate(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close connection socket",
           cmd  => fun(#{csock := Sock}) ->
                           socket:close(Sock)
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := Sock}) ->
                           socket:close(Sock)
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    ClientSeq = 
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Port} = ev_await_start(),
                           {ok, State#{tester => Tester, server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** The init part ***
         #{desc => "which server (local) address",
           cmd  => fun(#{domain := Domain, server_port := Port} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, 
                                     addr   => LAddr},
                           SSA   = LSA#{port => Port},
                           {ok, State#{local_sa => LSA, server_sa => SSA}}
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
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _Port} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ev_ready(Tester, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_await_continue(Tester, tester, connect)
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := SSA}) ->
                           socket:connect(Sock, SSA)
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ev_ready(Tester, connect),
                           ok
                   end},
         #{desc => "await continue (send request)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_await_continue(Tester, tester, send_req)
                   end},
         #{desc => "send request (to server)",
           cmd  => fun(#{sock := Sock, send := Send}) ->
                           Send(Sock, ?BASIC_REQ)
                   end},
         #{desc => "announce ready (send request)",
           cmd  => fun(#{tester := Tester}) ->
                           ev_ready(Tester, send_req),
                           ok
                   end},
         #{desc => "await recv reply (from server)",
           cmd  => fun(#{sock := Sock, recv := Recv}) ->
                           {ok, ?BASIC_REP} = Recv(Sock),
                           ok
                   end},
         #{desc => "announce ready (recv reply)",
           cmd  => fun(#{tester := Tester}) ->
                           ev_ready(Tester, recv_reply),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ev_await_terminate(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:close(Sock)
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
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ev_start(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, Port} = ev_await_ready(Pid, server, init),
                           {ok, State#{server_port => Port}}
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid, server_port := Port} = _State) ->
                           ev_start(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ev_await_ready(Pid, client, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_continue(Server, accept),
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_continue(Client, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_await_ready(Client, client, connect)
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_await_ready(Server, server, accept)
                   end},
         #{desc => "order client to continue (with send request)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_continue(Client, send_req),
                           ok
                   end},
         #{desc => "await client ready (with send request)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_await_ready(Client, client, send_req)
                   end},
         #{desc => "await server ready (request recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_await_ready(Server, server, recv_req)
                   end},
         #{desc => "order server to continue (with send reply)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_continue(Server, send_reply),
                           ok
                   end},
         #{desc => "await server ready (with reply sent)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_await_ready(Server, server, send_reply)
                   end},
         #{desc => "await client ready (reply recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_await_ready(Client, client, recv_reply)
                   end},


         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_terminate(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ev_await_termination(Client),
                           State1 = maps:remove(client, State),
                           {ok, State1}
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_terminate(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           ev_await_termination(Server),
                           State1 = maps:remove(server, State),
                           {ok, State1}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    i("start server evaluator"),
    Server = evaluator_start("server", ServerSeq, InitState),

    i("start client evaluator"),
    Client = evaluator_start("client", ClientSeq, InitState),
    i("await evaluator(s)"),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    ok = await_evaluator_finish([Server, Client, Tester]).



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
    ?TT(?SECS(5)),
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

    i("start tcp (stream) evaluator"),
    InitState1 = #{domain => inet, type => stream, protocol => tcp},
    Tester1 = evaluator_start("tcp-tester", Seq, InitState1),
    i("await tcp evaluator"),
    ok = await_evaluator_finish([Tester1]),

    i("start udp (dgram) socket"),
    InitState2 = #{domain => inet, type => dgram, protocol => udp},
    Tester2 = evaluator_start("udp-tester", Seq, InitState2),
    i("await udp evaluator"),
    ok = await_evaluator_finish([Tester2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Perform some simple getopt and setopt with the level = otp options
api_opt_simple_otp_controlling_process(suite) ->
    [];
api_opt_simple_otp_controlling_process(doc) ->
    [];
api_opt_simple_otp_controlling_process(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
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
                           %% receive
                           %%     {start, Tester, Socket} ->
                           %%         {ok, State#{tester => Tester,
                           %%                     sock   => Socket}}
                           %% end
                           {Tester, Sock} = ev_await_start(),
                           {ok, State#{tester => Tester,
                                       sock   => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** The actual test ***
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
         #{desc => "announce ready (not owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_ready(Tester, not_owner),
                           %% Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue (owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           %% receive
                           %%     {continue, Tester} ->
                           %%         ok
                           %% end
                           ev_await_continue(Tester, tester, owner),
                           ok
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
         #{desc => "announce ready (owner)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           %% Tester ! {ready, self()},
                           ev_ready(Tester, owner),
                           ok

                   end},
         
         %% *** Termination ***
         #{desc => "await termination",
           cmd  => fun(#{tester := Tester} = State) ->
                           %% receive
                           %%     {terminate, Tester} ->
                           %%         State1 = maps:remove(tester, State),
                           %%         State2 = maps:remove(sock, State1),
                           %%         {ok, State2}
                           %% end
                           ev_await_terminate(Tester, tester),
                           State1 = maps:remove(tester, State),
                           State2 = maps:remove(sock, State1),
                           {ok, State2}
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
         #{desc => "monitor client",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},

         %% *** The actual test ***
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
                           %% Client ! {start, self(), Sock},
                           ev_start(Client, Sock),
                           ok
                   end},
         #{desc => "await (client) ready (not owner)",
           cmd  => fun(#{client := Client} = _State) ->
                           %% receive
                           %%     {ready, Client} ->
                           %%         ok
                           %% end
                           ev_await_ready(Client, client, not_owner)
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
         #{desc => "order (client) continue (owner)",
           cmd  => fun(#{client := Client} = _State) ->
                           %% Client ! {continue, self()},
                           ev_continue(Client, owner),
                           ok
                   end},
         #{desc => "await (client) ready (2)",
           cmd  => fun(#{client := Client} = _State) ->
                           %% receive
                           %%     {ready, Client} ->
                           %%         ok
                           %% end
                           ev_await_ready(Client, client, owner),
                           ok
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

         %% *** Termination ***
         #{desc => "order (client) terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           %% Client ! {terminate, self()},
                           ev_terminate(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           %% receive
                           %%     {'DOWN', _, process, Client, _} ->
                           %%         {ok, maps:remove(client, State)}
                           %% end
                           ev_await_termination(Client),
                           {ok, maps:remove(client, State)}
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

    i("start tcp (stream) socket"),
    ClientInitState1 = #{},
    Client1 = evaluator_start("tcp-client", ClientSeq, ClientInitState1),
    TesterInitState1 = #{domain   => inet, 
                         type     => stream, 
                         protocol => tcp,
                         client   => Client1#ev.pid},
    Tester1          = evaluator_start("tcp-tester", TesterSeq, TesterInitState1),
    i("await tcp evaluator"),
    ok = await_evaluator_finish([Tester1, Client1]),

    i("start udp (dgram) socket"),
    ClientInitState2 = #{},
    Client2 = evaluator_start("udp-client", ClientSeq, ClientInitState2),
    TesterInitState2 = #{domain   => inet, 
                         type     => dgram, 
                         protocol => udp,
                         client   => Client2#ev.pid},
    Tester2          = evaluator_start("udp-tester", TesterSeq, TesterInitState2),
    i("await udp evaluator"),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(10)),
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
                           Tester = ev_await_start(),
                           {ok, State#{tester => Tester}}
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
                           {ok, State#{local_sa => LSA}}
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
           cmd  => fun(#{lsock := LSock, local_sa := LSA} = State) ->
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
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, lport := Port}) ->
                           ev_ready(Tester, init, Port),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ev_await_terminate(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{lsock := Sock} = State) ->
                           sock_close(Sock),
                           State1 = maps:remove(lport, State),
                           State2 = maps:remove(sock,  State1),
                           {ok, State2}
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
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{local_sa => LSA}}
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
           cmd  => fun(#{sock1 := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket 2 to local address",
           cmd  => fun(#{sock2 := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket 3 to local address",
           cmd  => fun(#{sock3 := Sock, local_sa := LSA} = _State) ->
                           case socket:bind(Sock, LSA) of
                               {ok, _} ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** Synchronize with the server ***
         #{desc => "order server start",
           cmd  => fun(#{server := Server}) ->
                           ev_start(Server),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server, local_sa := LSA} = State) ->
                           {ok, Port} = ev_await_ready(Server, server, init),
                           ServerSA = LSA#{port => Port},
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% *** Connect sequence ***
         #{desc => "order (server) start",
           cmd  => fun(#{sock1     := Sock1,
                         sock2     := Sock2,
                         sock3     := Sock3,
                         server_sa := SSA,
                         timeout   := To}) ->
                           Socks = [Sock1, Sock2, Sock3],
                           api_to_connect_tcp_await_timeout(Socks, To, SSA)
                   end},

         %% *** Terminate server ***
         #{desc => "order (server) terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_terminate(Server),
                           ok
                   end},
         #{desc => "await (server) down",
           cmd  => fun(#{server := Server} = State) ->
                           ev_await_termination(Server),
                           State1 = maps:remove(server,    State),
                           State2 = maps:remove(server_sa, State1),
                           {ok, State2}
                   end},
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

    i("create server evaluator"),
    ServerInitState = InitState,
    Server          = evaluator_start("server", ServerSeq, ServerInitState),

    i("create tester evaluator"),
    TesterInitState = InitState#{server => Server#ev.pid},
    Tester          = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(10)),
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

    i("create tester evaluator"),
    Tester = evaluator_start("tester", TesterSeq, InitState),

    i("await evaluator"),
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
    ?TT(?SECS(20)),
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
    ?TT(?SECS(20)),
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
                           Tester = ev_await_start(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
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
         #{desc => "announce ready (init)",
           cmd  => fun(#{lsock := LSock, tester := Tester}) ->
                           ev_ready(Tester, init, LSock),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_await_continue(Tester, tester, accept),
                           ok
                   end},
         #{desc => "attempt to accept (without success)",
           cmd  => fun(#{lsock := LSock, timeout := To} = State) ->
                           Start = t(),
                           case socket:accept(LSock, To) of
                               {error, timeout} ->
                                   {ok, State#{start => Start, stop => t()}};
                               {ok, Sock} ->
                                   ee("Unexpected accept success: "
                                      "~n   ~p", [Sock]),
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
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ev_ready(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_await_terminate(Tester, tester),
                           ok
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
                           {Tester, LSock} = ev_await_start(),
                           {ok, State#{tester => Tester,
                                       lsock  => LSock}}
                           
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_ready(Tester, init),
                           ok
                   end},

         %% *** The actual test part ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_await_continue(Tester, tester, accept),
                           ok
                   end},
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
           cmd  => fun(#{start := Start, stop := Stop, timeout := To} = State) ->
                           TDiff  = tdiff(Start, Stop),
                           if
                               (TDiff >= To) ->
                                   State1 = maps:remove(start, State),
                                   State2 = maps:remove(stop,  State1),
                                   {ok, State2};
                               true ->
                                   {error, {unexpected_timeout, TDiff, To}}
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_ready(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ev_await_terminate(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
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
                           ev_start(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor ready (init)",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           {ok, Sock} = ev_await_ready(Pid, prim_acceptor, init),
                           {ok, State#{lsock => Sock}}
                   end},

         %% Start sec-acceptor-1
         #{desc => "start sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid, lsock := LSock} = _State) ->
                           ev_start(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 ready (init)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ev_await_ready(Pid, sec_acceptor1, init)
                   end},

         %% Start sec-acceptor-2
         #{desc => "start sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid, lsock := LSock} = _State) ->
                           ev_start(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 ready (init)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ev_await_ready(Pid, sec_acceptor2, init)
                   end},

         %% Activate the acceptor(s)
         #{desc => "active prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ev_continue(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ev_continue(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ev_continue(Pid, accept),
                           ok
                   end},

         %% Await acceptor(s) completions
         #{desc => "await prim-acceptor ready (accept)",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ev_await_ready(Pid, prim_acceptor, accept)
                   end},
         #{desc => "await sec-acceptor 1 ready (accept)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ev_await_ready(Pid, sec_acceptor1, accept)
                   end},
         #{desc => "await sec-acceptor 2 ready (accept)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ev_await_ready(Pid, sec_acceptor2, accept)
                   end},

         %% Terminate
         #{desc => "order prim-acceptor to terminate",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ev_terminate(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor termination",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           ev_await_termination(Pid),
                           State1 = maps:remove(prim_acceptor, State),
                           {ok, State1}
                   end},
         #{desc => "order sec-acceptor 1 to terminate",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ev_terminate(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 termination",
           cmd  => fun(#{sec_acceptor1 := Pid} = State) ->
                           ev_await_termination(Pid),
                           State1 = maps:remove(sec_acceptor1, State),
                           {ok, State1}
                   end},
         #{desc => "order sec-acceptor 2 to terminate",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ev_terminate(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 termination",
           cmd  => fun(#{sec_acceptor2 := Pid} = State) ->
                           ev_await_termination(Pid),
                           State1 = maps:remove(sec_acceptor2, State),
                           {ok, State1}
                   end},
         
         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    i("create prim-acceptor evaluator"),
    PrimAInitState = InitState,
    PrimAcceptor = evaluator_start("prim-acceptor", 
                                   PrimAcceptorSeq, PrimAInitState),

    i("create sec-acceptor 1 evaluator"),
    SecAInitState1 = maps:remove(domain, InitState),
    SecAcceptor1 = evaluator_start("sec-acceptor-1", 
                                   SecAcceptorSeq, SecAInitState1),
    
    i("create sec-acceptor 2 evaluator"),
    SecAInitState2 = SecAInitState1,
    SecAcceptor2 = evaluator_start("sec-acceptor-2", 
                                   SecAcceptorSeq, SecAInitState2),

    i("create tester evaluator"),
    TesterInitState = #{prim_acceptor => PrimAcceptor#ev.pid,
                        sec_acceptor1 => SecAcceptor1#ev.pid,
                        sec_acceptor2 => SecAcceptor2#ev.pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
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
                   ?TT(?SECS(10)),
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
                           ?TT(?SECS(10)),
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
                           Tester = ev_await_start(),
                           {ok, State#{tester => Tester}}
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
                           %% Tester ! {ready, self(), Port},
                           ev_ready(Tester, init, Port),
                           ok
                   end},
         #{desc => "await continue (accept and recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ok = ev_await_continue(Tester, tester, accept_recv)
                   end},

         %% *** The actual test ***
         #{desc => "attempt accept",
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
                           %% Tester ! {ready, self()},
                           %% ok
                           ev_ready(Tester, accept_recv),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ev_await_terminate(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
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
                           {Tester, Port} = ev_await_start(),
                           {ok, State#{tester      => Tester,
                                       server_port => Port}}
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
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ev_ready(Tester, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (with connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ev_await_continue(Tester, tester, connect)
                   end},
         #{desc => "connect",
           cmd  => fun(#{sock := Sock, ssa := SSA}) ->
                           sock_connect(Sock, SSA),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ev_await_terminate(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
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
                           ev_start(Server),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Server} = State) ->
                           {ok, Port} = ev_await_ready(Server, server, init),
                           {ok, State#{server_port => Port}}
                   end},
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_continue(Server, accept_recv),
                           ok
                   end},

         %% *** Activate client ***
         #{desc => "start client",
           cmd  => fun(#{client := Client, server_port := Port} = _State) ->
                           ev_start(Client, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_await_ready(Client, client, init)
                   end},

         %% *** The actual test ***
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_continue(Client, connect),
                           ok
                   end},
         #{desc => "await server ready (accept/recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_await_ready(Server, server, accept_recv)
                   end},

         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ev_terminate(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           ev_await_termination(Client),
                           State1 = maps:remove(client, State),
                           State2 = maps:remove(client_mref, State1),
                           {ok, State2}
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ev_terminate(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           ev_await_termination(Server),
                           State1 = maps:remove(server, State),
                           State2 = maps:remove(server_mref, State1),
                           State3 = maps:remove(server_port, State2),
                           {ok, State3}
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    
    i("start server evaluator"),
    ServerInitState = InitState,
    #ev{pid = SPid} = Server = evaluator_start("server", 
                                               ServerSeq, 
                                               ServerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    #ev{pid = CPid} = Client = evaluator_start("client", 
                                               ClientSeq, 
                                               ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{server => SPid, client => CPid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(10)),
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

    i("start tester evaluator"),
    Tester = evaluator_start("tester", TesterSeq, InitState),
    
    i("await evaluator"),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(10)),
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
                   ?TT(?SECS(5)),
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
                   ?TT(?SECS(5)),
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
                   ?TT(?SECS(5)),
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
                   ?TT(?SECS(5)),
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
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         %% #{desc => "enable (otp) debug",
         %%   cmd  => fun(#{sock := Sock} = _State) ->
         %%                   ok = socket:setopt(Sock, otp, debug, true)
         %%           end},

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
                                      "~n   ~p", [Owner, Reason]),
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

    i("start (socket) owner evaluator"),
    #ev{pid = Pid} = Owner = evaluator_start("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
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

sc_lc_recv_response_tcp4(suite) ->
    [];
sc_lc_recv_response_tcp4(doc) ->
    [];
sc_lc_recv_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recv_response_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_lc_recv_response_tcp6(suite) ->
    [];
sc_lc_recv_response_tcp6(doc) ->
    [];
sc_lc_recv_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recv_response_tcp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_receive_response_tcp(InitState) ->
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
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester, {H1, H2, H3}} ->
                                   {ok, State#{handler1 => H1,
                                               handler2 => H2,
                                               handler3 => H3}}
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
         #{desc => "transfer connection to handler 1",
           cmd  => fun(#{handler1 := Handler, csock := Sock}) ->
                           %% ok = socket:setopt(Sock, 
                           %%                    otp, controlling_process, 
                           %%                    Handler),
                           Handler ! {connection, Sock},
                           ok
                   end},
         #{desc => "transfer connection to handler 2",
           cmd  => fun(#{handler2 := Handler, csock := Sock}) ->
                           %% ok = socket:setopt(Sock, 
                           %%                    otp, controlling_process, 
                           %%                    Handler),
                           Handler ! {connection, Sock},
                           ok
                   end},
         #{desc => "transfer connection to handler 3",
           cmd  => fun(#{handler3 := Handler, csock := Sock}) ->
                           %% ok = socket:setopt(Sock, 
                           %%                    otp, controlling_process, 
                           %%                    Handler),
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
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "close the connection socket",
           cmd  => fun(#{csock := Sock}) ->
                           socket:close(Sock)
                   end},

         #{desc => "await terminate",
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
                                      "~n   ~p", [Tester, Reason]),
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
         #{desc => "attempt recv",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           case Recv(Sock) of
                               {ok, _Data} ->
                                   ee("Unexpected data received"),
                                   {error, unexpected_data};
                               {error, closed} ->
                                   ei("received expected 'closed' result"),
                                   State1 = maps:remove(sock, State),
                                   {ok, State1};
                               {error, Reason} = ERROR ->
                                   ee("Unexpected read faulure: "
                                      "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
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
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
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
         #{desc => "monitor handler 1",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 2",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 3",
           cmd  => fun(#{handler3 := Pid} = _State) ->
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

         %% Start the handler(s)
         #{desc => "order handler 1 start",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await handler 1 ready (init)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 1 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "order handler 2 start",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await handler 2 ready (init)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "order handler 3 start",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await handler 3 ready (init)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 3 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler3}};
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
                                   {error, {unexpected_exit, client}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% The actual test
         #{desc => "order acceptor to continue",
           cmd  => fun(#{acceptor := Pid, 
                         handler1 := H1, 
                         handler2 := H2, 
                         handler3 := H3} = _State) ->
                           Pid ! {continue, self(), {H1, H2, H3}},
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
                                   {error, {unexpected_exit, client}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await handler 1 ready (connection)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 1 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await handler 2 ready (connection)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await handler 3 ready (connection)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 3 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler3}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "sleep",
           cmd  => fun(_State) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order acceptor to continue (close)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "await handler 1 ready (close)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 1 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await handler 2 ready (close)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await handler 3 ready (close)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding handler 2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, handler2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Terminations
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(handler1, State)}
                           end
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(handler2, State)}
                           end
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(handler3, State)}
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

    i("start acceptor evaluator"),
    AccInitState = InitState,
    Acceptor = evaluator_start("acceptor", AcceptorSeq, AccInitState),

    i("start handler 1 evaluator"),
    HandlerInitState = #{recv => maps:get(recv, InitState)},
    Handler1 = evaluator_start("handler-1", HandlerSeq, HandlerInitState),

    i("start handler 2 evaluator"),
    Handler2 = evaluator_start("handler-2", HandlerSeq, HandlerInitState),

    i("start handler 3 evaluator"),
    Handler3 = evaluator_start("handler-3", HandlerSeq, HandlerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = evaluator_start("client", ClientSeq, ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{acceptor => Acceptor#ev.pid,
                        handler1 => Handler1#ev.pid,
                        handler2 => Handler2#ev.pid,
                        handler3 => Handler3#ev.pid,
                        client   => Client#ev.pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = await_evaluator_finish([Acceptor, 
                                 Handler1, Handler2, Handler3, 
                                 Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while a process is calling the recvfrom function.
%% Socket is IPv4.
%% 

sc_lc_recvfrom_response_udp4(suite) ->
    [];
sc_lc_recvfrom_response_udp4(doc) ->
    [];
sc_lc_recvfrom_response_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvfrom_response_udp4,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock, To) -> socket:recvfrom(Sock, [], To) end,
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_lc_recvfrom_response_udp6(suite) ->
    [];
sc_lc_recvfrom_response_udp6(doc) ->
    [];
sc_lc_recvfrom_response_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvfrom_response_udp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock, To) -> socket:recvfrom(Sock, [], To) end,
                   InitState = #{domain => inet6,
                                 recv   => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_receive_response_udp(InitState) ->
    PrimServerSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LAddr = which_local_addr(Domain),
                           LSA   = #{family => Domain, addr => LAddr},
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "open socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           Sock = sock_open(Domain, dgram, udp),
                           SA   = sock_sockname(Sock),
                           {ok, State#{sock => Sock, sa => SA}}
                   end},
         #{desc => "bind socket",
           cmd  => fun(#{sock := Sock, lsa := LSA}) ->
                           sock_bind(Sock, LSA),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock}) ->
                           Tester ! {ready, self(), Sock},
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (receive)",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester, Timeout} ->
                                   {ok, State#{timeout => Timeout}}
                           end
                   end},
         #{desc => "receive with timeout",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := Timeout}) ->
                           case Recv(Sock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (closed)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% Termination
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

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    SecServerSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester, Sock} ->
                                   {ok, State#{tester => Tester, sock => Sock}}
                           end
                   end},
         #{desc => "monitor tester",
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
         #{desc => "await continue (receive)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "receive",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           %% ok = socket:setopt(Sock, otp, debug, true),
                           case Recv(Sock, infinity) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (closed)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% Termination
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

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],


    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor primary server",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 1",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 2",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 3",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the primary server
         #{desc => "order 'primary server' start",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await 'primary server' ready (init)",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-server ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, prim_server}};
                               {ready, Pid, Sock} ->
                                   {ok, State#{sock => Sock}}
                           end
                   end},

         %% Start the secondary server 1
         #{desc => "order 'secondary server 1' start",
           cmd  => fun(#{sec_server1 := Pid, sock := Sock} = _State) ->
                           Pid ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await 'secondary server 1' ready (init)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-server ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_server1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Start the secondary server 2
         #{desc => "order 'secondary server 2' start",
           cmd  => fun(#{sec_server2 := Pid, sock := Sock} = _State) ->
                           Pid ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await 'secondary server 2' ready (init)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-server-2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_server2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Start the secondary server 3
         #{desc => "order 'secondary server 3' start",
           cmd  => fun(#{sec_server3 := Pid, sock := Sock} = _State) ->
                           Pid ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await 'secondary server 3' ready (init)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-server-3 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_server3}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinit recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary server 1' to continue",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order 'secondary server 2' to continue",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order 'secondary server 3' to continue",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order 'primary server' to continue (recvfrom)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           Pid ! {continue, self(), ?SECS(5)},
                           ok
                   end},
         #{desc => "await 'primary server' ready (timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-server ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, prim_server}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "order 'primary server' to continue (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "await 'primary server' ready (closed)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-server ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, prim_server}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await 'secondary server 1' ready (closed)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-server-1 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_server1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await 'secondary server 2' ready (closed)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-server-2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_server2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await 'secondary server 3' ready (closed)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-server-3 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_server3}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         

         %% Terminations
         #{desc => "order 'secondary server 3' to terminate",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'secondary server 3' termination",
           cmd  => fun(#{sec_server3 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(sec_server3, State)}
                           end
                   end},
         #{desc => "order 'secondary server 2' to terminate",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'secondary server 2' termination",
           cmd  => fun(#{sec_server2 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(sec_server2, State)}
                           end
                   end},
         #{desc => "order 'secondary server 1' to terminate",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'secondary server 1' termination",
           cmd  => fun(#{sec_server1 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(sec_server1, State)}
                           end
                   end},
         #{desc => "order 'primary server' to terminate",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'primary server' termination",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(prim_server, State)}
                           end
                   end},


         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],
    

    i("start 'primary server' evaluator"),
    PrimSrvInitState = InitState,
    PrimServer = evaluator_start("prim-server", PrimServerSeq, PrimSrvInitState),

    i("start 'secondary server 1' evaluator"),
    SecSrvInitState = #{recv => maps:get(recv, InitState)},
    SecServer1 = evaluator_start("sec-server-1", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 2' evaluator"),
    SecServer2 = evaluator_start("sec-server-2", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 3' evaluator"),
    SecServer3 = evaluator_start("sec-server-3", SecServerSeq, SecSrvInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_server => PrimServer#ev.pid,
                        sec_server1  => SecServer1#ev.pid,
                        sec_server2  => SecServer2#ev.pid,
                        sec_server3  => SecServer3#ev.pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = await_evaluator_finish([PrimServer, 
                                 SecServer1, SecServer2, SecServer3,
                                 Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_lc_recvmsg_response_tcp4(suite) ->
    [];
sc_lc_recvmsg_response_tcp4(doc) ->
    [];
sc_lc_recvmsg_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvmsg_response_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_lc_recvmsg_response_tcp6(suite) ->
    [];
sc_lc_recvmsg_response_tcp6(doc) ->
    [];
sc_lc_recvmsg_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_tcp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_lc_recvmsg_response_udp4(suite) ->
    [];
sc_lc_recvmsg_response_udp4(doc) ->
    [];
sc_lc_recvmsg_response_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvmsg_response_udp4,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain => inet,
                                 recv   => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_lc_recvmsg_response_udp6(suite) ->
    [];
sc_lc_recvmsg_response_udp6(doc) ->
    [];
sc_lc_recvmsg_response_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_udp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain => inet6,
                                 recv   => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv4.
%%
%% To minimize the chance of "weirdness", we should really have test cases
%% where the two sides of the connection is on different machines. But for
%% now, we will make do with different VMs on the same host.
%%

sc_rc_recv_response_tcp4(suite) ->
    [];
sc_rc_recv_response_tcp4(doc) ->
    [];
sc_rc_recv_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recv_response_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv6.

sc_rc_recv_response_tcp6(suite) ->
    [];
sc_rc_recv_response_tcp6(doc) ->
    [];
sc_rc_recv_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recv_response_tcp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_rc_receive_response_tcp(InitState) ->
    ServerSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester} ->
                                   {ok, State#{tester => Tester}}
                           end
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
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
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, lsa := LSA, lport := Port}) ->
                           SA = LSA#{port => Port},
                           Tester ! {ready, self(), SA},
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {continue, Tester} ->
                                   ok;
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}}
                           end
                   end},
         #{desc => "accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accepted)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "receive",
           cmd  => fun(#{csock := Sock, recv := Recv} = State) ->
                           case Recv(Sock) of
                               {error, closed} ->
                                   {ok, maps:remove(csock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (closed)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         
         %% Termination
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
         #{desc => "close listen socket",
           cmd  => fun(#{lsock := LSock} = _State) ->
                           socket:close(LSock)
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    ClientSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           receive
                               {start, Tester, Node, ServerSA} ->
                                   {ok, State#{tester    => Tester, 
                                               node      => Node, 
                                               server_sa => ServerSA}}
                           end
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "start client process on client node",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = sc_rc_tcp_client_start(Node),
                           ei("client ~p started", [Pid]),
                           {ok, State#{client => Pid}}
                   end},
         #{desc => "monitor client process",
           cmd  => fun(#{client := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order client process to start",
           cmd  => fun(#{client := Client, server_sa := ServerSA}) ->
                           Client ! {start, self(), ServerSA},
                           ok
                   end},
         #{desc => "await client process ready",
           cmd  => fun(#{tester := Tester,
                         client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok;
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester,
                         client := Client} = _State) ->
                           receive
                               {continue, Tester} ->
                                   ok;
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "order client process to continue (connect)",
           cmd  => fun(#{client := Client}) ->
                           Client ! {continue, self()},
                           ok
                   end},
         #{desc => "await client process ready (connected)",
           cmd  => fun(#{tester := Tester,
                         client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok;
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "announce ready (connected)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester,
                         client := Client} = _State) ->
                           receive
                               {continue, Tester} ->
                                   ok;
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "order client process to close",
           cmd  => fun(#{client := Client}) ->
                           Client ! {continue, self()},
                           ok
                   end},
         #{desc => "await client process ready (closed)",
           cmd  => fun(#{tester := Tester,
                         client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok;
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "announce ready (closed)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester, client := Client} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}};
                               {terminate, Tester} ->
                                   {ok, maps:remove(tester, State)}
                           end
                   end},
         #{desc => "kill client process",
           cmd  => fun(#{client := Client}) ->
                           Client ! {terminate, self(), normal},
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           receive
                               {'DOWN', _, process, Client, _} ->
                                   {ok, maps:remove(client, State)}
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
         #{desc => "create client node",
           cmd  => fun(#{host := Host} = State) ->
                           case start_node(Host, client) of
                               {ok, Node} ->
                                   ei("client node ~p started", [Node]),
                                   {ok, State#{client_node => Node}};
                               {error, Reason, _} ->
                                   {error, Reason}
                           end
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{client_node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           receive
                               {ready, Pid, ServerSA} ->
                                   {ok, State#{server_sa => ServerSA}};
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding server ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, server}}
                           end
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client      := Pid, 
                         client_node := Node,
                         server_sa   := ServerSA} = _State) ->
                           Pid ! {start, self(), Node, ServerSA},
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           receive
                               {ready, Pid} ->
                                   ok;
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},

         %% The actual test
         #{desc => "order server accept",
           cmd  => fun(#{server := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order client connect",
           cmd  => fun(#{client := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "await client ready (connected)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok;
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}};
                               {'DOWN', _, process, Server, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Server, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "await server ready (accepted)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           receive
                               {ready, Server} ->
                                   ok;
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}};
                               {'DOWN', _, process, Server, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Server, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order client close",
           cmd  => fun(#{client := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "await client ready (closed)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           receive
                               {ready, Client} ->
                                   ok;
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}};
                               {'DOWN', _, process, Server, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Server, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},
         #{desc => "await server ready (closed)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           receive
                               {ready, Server} ->
                                   ok;
                               {'DOWN', _, process, Client, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Client, Reason]),
                                   {error, {unexpected_exit, client}};
                               {'DOWN', _, process, Server, Reason} ->
                                   ee("Unexpected DOWN regarding client ~p: "
                                      "~n   ~p", [Server, Reason]),
                                   {error, {unexpected_exit, client}}
                           end
                   end},

         %% Terminations
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
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(server, State)}
                           end
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{client_node := Node} = _State) ->
                           stop_node(Node)
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{client_node := Node} = State) ->
                           receive
                               {nodedown, Node} ->
                                   {ok, maps:remove(client_node, State)}
                           end
                   end},

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    i("start server evaluator"),
    ServerInitState = InitState,
    Server = evaluator_start("server", ServerSeq, ServerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = evaluator_start("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{host   => local_host(),
                        server => Server#ev.pid,
                        client => Client#ev.pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = await_evaluator_finish([Server, Client, Tester]).


start_node(Host, NodeName) ->
    Dir   = filename:dirname(code:which(?MODULE)),
    Flags = "-pa " ++ Dir,
    Opts  = [{monitor_master, true}, {erl_flags, Flags}],
    ct_slave:start(Host, NodeName, Opts).

stop_node(Node) ->
    case ct_slave:stop(Node) of
        {ok, _} ->
            ok;
        {error, _} = ERROR ->
            ERROR
    end.

local_host() ->
    try net_adm:localhost() of
        Host when is_list(Host) ->
            list_to_atom(Host)
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    end.

    
sc_rc_tcp_client_start(Node) ->
    Self = self(),
    GL   = group_leader(),
    Fun  = fun() -> sc_rc_tcp_client(Self, GL) end,
    erlang:spawn(Node, Fun).


sc_rc_tcp_client(Parent, GL) ->
    sc_rc_tcp_client_init(Parent, GL),
    ServerSA = sc_rc_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = sc_rc_tcp_client_create(Domain),
    sc_rc_tcp_client_bind(Sock, Domain),
    sc_rc_tcp_client_announce_ready(Parent),
    sc_rc_tcp_client_await_continue(Parent),
    sc_rc_tcp_client_connect(Sock, ServerSA),
    sc_rc_tcp_client_announce_ready(Parent),
    sc_rc_tcp_client_await_continue(Parent),
    sc_rc_tcp_client_close(Sock),
    sc_rc_tcp_client_announce_ready(Parent),
    Reason = sc_rc_tcp_client_await_terminate(Parent),
    exit(Reason).

sc_rc_tcp_client_init(Parent, GL) ->
    i("sc_rc_tcp_client_init -> entry"),
    _MRef = erlang:monitor(process, Parent),
    group_leader(self(), GL),
    ok.

sc_rc_tcp_client_await_start(Parent) ->
    i("sc_rc_tcp_client_await_start -> entry"),
    receive
        {start, Parent, ServerSA} ->
            ServerSA;
        {'DOWN', _, process, Parent, _Reason} ->
            init:stop()
    end.

sc_rc_tcp_client_create(Domain) ->
    i("sc_rc_tcp_client_create -> entry"),
    case socket:open(Domain, stream, tcp) of
        {ok, Sock} ->
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

sc_rc_tcp_client_bind(Sock, Domain) ->
    i("sc_rc_tcp_client_bind -> entry"),
    LAddr = which_local_addr(Domain),
    LSA   = #{family => Domain, 
              addr   => LAddr},
    case socket:bind(Sock, LSA) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            exit({bind, Reason})
    end.

sc_rc_tcp_client_announce_ready(Parent) ->
    Parent ! {ready, self()},
    ok.

sc_rc_tcp_client_await_continue(Parent) ->
    i("sc_rc_tcp_client_await_continue -> entry"),
    receive
        {continue, Parent} ->
            ok;
        {'DOWN', _, process, Parent, _Reason} ->
            init:stop()
    end.

sc_rc_tcp_client_connect(Sock, ServerSA) ->
    i("sc_rc_tcp_client_connect -> entry"),
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

sc_rc_tcp_client_close(Sock) ->
    i("sc_rc_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            exit({close, Reason})
    end.

sc_rc_tcp_client_await_terminate(Parent) ->
    i("sc_rc_tcp_client_await_terminate -> entry"),
    receive
        {terminate, Parent, Reason} ->
            Reason;
        {'DOWN', _, process, Parent, _Reason} ->
            init:stop()
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_rc_recvmsg_response_tcp4(suite) ->
    [];
sc_rc_recvmsg_response_tcp4(doc) ->
    [];
sc_rc_recvmsg_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recvmsg_response_tcp4,
           fun() ->
                   ?TT(?SECS(30)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_rc_recvmsg_response_tcp6(suite) ->
    [];
sc_rc_recvmsg_response_tcp6(doc) ->
    [];
sc_rc_recvmsg_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_rc_recvmsg_response_tcp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv4.

sc_lc_acceptor_response_tcp4(suite) ->
    [];
sc_lc_acceptor_response_tcp4(doc) ->
    [];
sc_lc_acceptor_response_tcp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_acceptor_response_tcp4,
           fun() ->
                   ?TT(?SECS(10)),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv6.

sc_lc_acceptor_response_tcp6(suite) ->
    [];
sc_lc_acceptor_response_tcp6(doc) ->
    [];
sc_lc_acceptor_response_tcp6(_Config) when is_list(_Config) ->
    tc_try(sc_lc_acceptor_response_tcp6,
           fun() ->
                   not_yet_implemented(),
                   ?TT(?SECS(10)),
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_acceptor_response_tcp(InitState) ->
    PrimAcceptorSeq =
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
         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:listen(Sock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           Tester ! {ready, self(), Sock},
                           ok
                   end},
                           
         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester, Timeout} ->
                                   {ok, State#{timeout => Timeout}}
                           end
                   end},
         #{desc => "await connection",
           cmd  => fun(#{sock := Sock, timeout := Timeout} = _State) ->
                           case socket:accept(Sock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, Sock} ->
                                   ee("unexpected success"),
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (closed)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         % Termination
         #{desc => "await terminate",
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
                               {start, Tester, Sock} ->
                                   {ok, State#{tester => Tester, sock => Sock}}
                           end
                   end},
         #{desc => "monitor tester",
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
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           receive
                               {'DOWN', _, process, Tester, Reason} ->
                                   ee("Unexpected DOWN regarding tester ~p: "
                                      "~n   ~p", [Tester, Reason]),
                                   {error, {unexpected_exit, tester}};
                               {continue, Tester} ->
                                   ok
                           end
                   end},
         #{desc => "accept",
           cmd  => fun(#{sock := Sock} = State) ->
                           %% ok = socket:setopt(Sock, otp, debug, true),
                           case socket:accept(Sock) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (closed)",
           cmd  => fun(#{tester := Tester}) ->
                           Tester ! {ready, self()},
                           ok
                   end},

         %% Termination
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

         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'primary acceptor'",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor 'secondary acceptor 1'",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary acceptor 2",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary acceptor 3",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the primary server
         #{desc => "order 'primary acceptor' start",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           Pid ! {start, self()},
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (init)",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-acc ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, prim_acc}};
                               {ready, Pid, Sock} ->
                                   {ok, State#{sock => Sock}}
                           end
                   end},

         %% Start the secondary acceptor 1
         #{desc => "order 'secondary acceptor 1' start",
           cmd  => fun(#{sec_acc1 := Pid, sock := Sock} = _State) ->
                           Pid ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' ready (init)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acc-1 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_acc1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Start the secondary acceptor 2
         #{desc => "order 'secondary acceptor 2' start",
           cmd  => fun(#{sec_acc2 := Pid, sock := Sock} = _State) ->
                           Pid ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' ready (init)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acc-2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_acc2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},

         %% Start the secondary acceptor 3
         #{desc => "order 'secondary acceptor 3' start",
           cmd  => fun(#{sec_acc3 := Pid, sock := Sock} = _State) ->
                           Pid ! {start, self(), Sock},
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' ready (init)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acc-3 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_acc3}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinit recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary acceptor 1' to continue",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order 'secondary acceptor 2' to continue",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order 'secondary acceptor 3' to continue",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "sleep",
           cmd  => fun(_) ->
                           ?SLEEP(?SECS(1)),
                           ok
                   end},
         #{desc => "order 'primary acceptor' to continue",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           Pid ! {continue, self(), ?SECS(5)},
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (timeout)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-acc ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, prim_acc}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "order 'primary acceptor' to continue (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           Pid ! {continue, self()},
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (closed)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding prim-acc ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, prim_acc}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await 'secondary acceptor 1' ready (closed)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acc-1 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_acc1}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await 'secondary acceptor 2' ready (closed)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acc-2 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_acc2}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         #{desc => "await 'secondary acceptor 3' ready (closed)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           receive
                               {'DOWN', _, process, Pid, Reason} ->
                                   ee("Unexpected DOWN regarding sec-acc-3 ~p: "
                                      "~n   ~p", [Pid, Reason]),
                                   {error, {unexpected_exit, sec_acc3}};
                               {ready, Pid} ->
                                   ok
                           end
                   end},
         

         %% Terminations
         #{desc => "order 'secondary acceptor 3' to terminate",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' termination",
           cmd  => fun(#{sec_acc3 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(sec_acc3, State)}
                           end
                   end},
         #{desc => "order 'secondary acceptor 2' to terminate",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' termination",
           cmd  => fun(#{sec_acc2 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(sec_acc2, State)}
                           end
                   end},
         #{desc => "order 'secondary acceptor 1' to terminate",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' termination",
           cmd  => fun(#{sec_acc1 := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(sec_acc1, State)}
                           end
                   end},
         #{desc => "order 'primary acceptor' to terminate",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           Pid ! {terminate, self()},
                           ok
                   end},
         #{desc => "await 'primary acceptor' termination",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           receive
                               {'DOWN', _, process, Pid, _} ->
                                   {ok, maps:remove(prim_acc, State)}
                           end
                   end},


         %% *** We are done ***
         #{desc => "finish",
           cmd  => fun(_) ->
                           {ok, normal}
                   end}
        ],


    i("start 'primary acceptor' evaluator"),
    PrimAccInitState = InitState,
    PrimAcc = evaluator_start("prim-acceptor", PrimAcceptorSeq, PrimAccInitState),

    i("start 'secondary acceptor 1' evaluator"),
    SecAccInitState = #{},
    SecAcc1 = evaluator_start("sec-acceptor-1", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 2' evaluator"),
    SecAcc2 = evaluator_start("sec-acceptor-2", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 3' evaluator"),
    SecAcc3 = evaluator_start("sec-acceptor-3", SecAcceptorSeq, SecAccInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_acc => PrimAcc#ev.pid,
                        sec_acc1 => SecAcc1#ev.pid,
                        sec_acc2 => SecAcc2#ev.pid,
                        sec_acc3 => SecAcc3#ev.pid},
    Tester = evaluator_start("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = await_evaluator_finish([PrimAcc, SecAcc1, SecAcc2, SecAcc3, Tester]).



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

-spec evaluator_start(Name, Seq, Init) -> ev() when
      Name :: string(),
      Seq  :: [command()],
      Init :: initial_evaluator_state().
                             
evaluator_start(Name, Seq, Init) 
  when is_list(Name) andalso is_list(Seq) andalso (Seq =/= []) ->
    Init2 = Init#{parent => self()},
    {Pid, MRef} = erlang:spawn_monitor(
                    fun() -> evaluator_init(Name, Seq, Init2) end),
    ?MKEV(Name, Pid, MRef).

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
            case lists:keysearch(Pid, #ev.pid, Evs) of
                {value, #ev{name = Name}} ->
                    i("evaluator '~s' (~p) success", [Name, Pid]),
                    NewEvs = lists:keydelete(Pid, #ev.pid, Evs),
                    await_evaluator_finish(NewEvs, Fails);
                false ->
                    i("unknown process ~p died (normal)", [Pid]),
                    await_evaluator_finish(Evs, Fails)
                end;
        {'DOWN', _MRef, process, Pid, Reason} ->
            case lists:keysearch(Pid, #ev.pid, Evs) of
                {value, #ev{name = Name}} ->
                    i("evaluator '~s' (~p) failed", [Name, Pid]),
                    NewEvs = lists:keydelete(Pid, #ev.pid, Evs),
                    await_evaluator_finish(NewEvs, [{Pid, Reason}|Fails]);
                false ->
                    i("unknown process ~p died: "
                      "~n   ~p", [Pid, Reason]),
                    await_evaluator_finish(Evs, Fails)
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
    %% The two prints is to get the output both in the shell (for when
    %% "personal" testing is going on) and in the logs.
    FStr = f("[~s][~s][~p] ~s" ++ F, 
             [formated_timestamp(), get(sname), self(), Prefix | A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ev_start(Pid) ->
    ev_announce(Pid, start, undefined).

ev_start(Pid, Extra) ->
    ev_announce(Pid, start, undefined, Extra).

ev_continue(Pid, Slogan) ->
    ev_announce(Pid, continue, Slogan).

%% ev_continue(Pid, Slogan, Extra) ->
%%     ev_announce(Pid, continue, Slogan, Extra).

ev_ready(Pid, Slogan) ->
    ev_announce(Pid, ready, Slogan).

ev_ready(Pid, Slogan, Extra) ->
    ev_announce(Pid, ready, Slogan, Extra).

ev_terminate(Pid) ->
    Pid ! {terminate, self()}.

ev_announce(To, Tag, Slogan) ->
    ev_announce(To, Tag, Slogan, undefined).

ev_announce(To, Tag, Slogan, Extra) ->
    To ! {Tag, self(), Slogan, Extra}.

ev_await_start() ->
    receive
        {start, Pid, _, undefined} ->
            Pid;
        {start, Pid, _, Extra} ->
            {Pid, Extra}
    end.

ev_await_continue(Pid, Name, Slogan) ->
    ev_await_continue(Pid, Name, Slogan, []).
ev_await_continue(Pid, Name, Slogan, Pids) when is_pid(Pid) andalso is_list(Pids) ->
    ev_await(Pid, Name, continue, Slogan, Pids).

ev_await_ready(Pid, Name, Slogan) ->
    ev_await_ready(Pid, Name, Slogan, []).
ev_await_ready(Pid, Name, Slogan, Pids) when is_pid(Pid) andalso is_list(Pids) ->
    ev_await(Pid, Name, ready, Slogan, Pids).

ev_await_terminate(Pid, Name) ->
    ev_await_terminate(Pid, Name, []).
ev_await_terminate(Pid, Name, Pids) ->
    receive
        {terminate, Pid} ->
            ok;
        {'DOWN', _, process, Pid, Reason} ->
            ee("Unexpected DOWN regarding ~w ~p: "
               "~n   ~p", [Name, Pid, Reason]),
            {error, {unexpected_exit, Name}};
        {'DOWN', _, process, DownPid, Reason} ->
            case ev_await_check_down(DownPid, Reason, Pids) of
                ok ->
                    ei("DOWN from unknown process ~p: "
                       "~n   ~p", [DownPid, Reason]),
                    ev_await_terminate(Pid, Name, Pids);
                {error, _} = ERROR ->
                    ERROR
            end
    end.

ev_await_termination(Pid) ->
    receive
        {'DOWN', _, process, Pid, _} ->
            ok
    end.

%% We expect a message from Pid, but we also watch for DOWN from 
%% both Pid and Pids, in which case the test has failed!
ev_await(Pid, Name, Tag, Slogan, Pids) ->
    receive
        {Tag, Pid, Slogan, undefined} ->
            ok;
        {Tag, Pid, Slogan, Extra} ->
            {ok, Extra};
        {'DOWN', _, process, Pid, Reason} ->
            ee("Unexpected DOWN regarding ~w ~p: "
               "~n   ~p", [Name, Pid, Reason]),
            {error, {unexpected_exit, Name}};
        {'DOWN', _, process, DownPid, Reason} ->
            case ev_await_check_down(DownPid, Reason, Pids) of
                ok ->
                    ei("DOWN from unknown process ~p: "
                       "~n   ~p", [DownPid, Reason]),
                    ev_await(Pid, Name, Tag, Slogan, Pids);
                {error, _} = ERROR ->
                    ERROR
            end
    end.

ev_await_check_down(DownPid, DownReason, Pids) ->
    case lists:keymember(DownPid, 1, Pids) of
        {value, {_, Name}} ->
            ee("Unexpected DOWN regarding ~w ~p: "
               "~n   ~p", [Name, DownPid, DownReason]),
            {error, {unexpected_exit, Name}};
        false ->
            ok
    end.


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
            i("sock_bind -> error: ~p", [Reason]),
            ?FAIL({bind, Reason})
    catch
        C:E:S ->
            i("sock_bind -> failed: ~p, ~p, ~p", [C, E, S]),
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
%%             i("sock_accept -> error: ~p", [Reason]),
%%             ?FAIL({accept, Reason})
%%     catch
%%         C:E:S ->
%%             i("sock_accept -> failed: ~p, ~p, ~p", [C, E, S]),
%%             ?FAIL({accept, C, E, S})
%%     end.


sock_close(Sock) ->
    try socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            i("sock_close -> error: ~p", [Reason]),
            ?FAIL({close, Reason})
    catch
        C:E:S ->
            i("sock_close -> failed: ~p, ~p, ~p", [C, E, S]),
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
    tc_print("begin ***",
             "~n----------------------------------------------------~n", "").
    
tc_end(Result) when is_list(Result) ->
    tc_print("done: ~s", [Result], 
             "", "----------------------------------------------------~n~n"),
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


tc_print(F, Before, After) ->
    tc_print(F, [], Before, After).

tc_print(F, A, Before, After) ->
    Name = tc_which_name(),
    FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
             [formated_timestamp(),Name,self()|A]),
    io:format(user, Before ++ FStr ++ After, []).

tc_which_name() ->
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
    end.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

%% p(F) ->
%%     p(F, []).

%% p(F, A) ->
%%     p(F, A, "", "").

%% p(F, A, Before, After) when is_list(Before) andalso is_list(After) ->
%%     TcName = 
%%         case get(tc_name) of
%%             undefined ->
%%                 case get(sname) of
%%                     undefined ->
%%                         "";
%%                     SName when is_list(SName) ->
%%                         SName
%%                 end;
%%             Name when is_list(Name) ->
%%                 Name
%%         end,
%%     FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
%%              [formated_timestamp(),TcName,self()|A]),
%%     i(Before ++ FStr ++ After, []).


i(F) ->
    i(F, []).

i(F, A) ->
    FStr = f("[~s] " ++ F, [formated_timestamp()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

